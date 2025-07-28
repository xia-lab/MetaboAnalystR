################################################################################################
################# First create table of all studies with named metabolites #####################
################################################################################################


my.get.nmdr.data <- function(StudyID){

    check.inx <-  nchar(StudyID) == 8 & grep("ST", StudyID)
  
    if(!check.inx){
        AddErrMsg("Error! Invalid Study ID selected!")
        return(0)
    }
  
    load_httr();
    call <- paste0("https://www.metabolomicsworkbench.org/rest/study/study_id/", StudyID, "/metaboanalyst");
  
    # set timeout to 45 seconds
    query_results <- httr::GET(call, encode = "json", timeout = 45)
  
    if(query_results$status_code!=200){
        AddErrMsg("Error! REST url to Metabolomics Workbench failed!")
        return(0)
    }
  
    # Parse the response into a table
    query_results_text <- content(query_results, "text", encoding = "UTF-8")
    query_study_dataset <- read.delim(text = query_results_text, header = T, check.names = F)
  
    if(nrow(query_study_dataset) < 3){
        AddErrMsg("Error! Selected study does not have enough samples for downstream statistical analyses!")
        return(0)
    }
  
    if(ncol(query_study_dataset) < 3){
        AddErrMsg("Error! Selected study does not have enough features for downstream statistical analyses!")
        return(0)
    }
  
    qs::qsave(query_study_dataset, "nmdr_study.qs");
    return(1);
}

#'Function to retrieve all available datasets from the Metabolomics Workbench.
#'@description This function uses the httr R package to make an API
#'call to the Metabolomics Workbench to retrieve a table of
#'all compatible datasets.
#'@usage ListNMDRStudies(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
ListNMDRStudies <- function(mSetObj=NA){
  
  # The Metabolomics Workbench API url
  call <- "https://www.metabolomicsworkbench.org/rest/study/study_id/ST/named_metabolites"
  
  # Use httr::GET to send the request to the Metabolomics Workbench API
  # The response will be saved in query_results
  query_results <- httr::GET(call, encode = "json")
  
  # Check if response is ok
  # 200 is ok! 401 means an error has occured on the user's end.
  if(query_results$status_code!=200){
    AddErrMsg("REST url to Metabolomics Workbench failed!")
    return(0)
  }
  
  # Parse the response into a table
  query_results_text <- content(query_results, "text", encoding = "UTF-8")
  query_results_json <- rjson::fromJSON(query_results_text, flatten = TRUE)
  query_results_table <- t(rbind.data.frame(query_results_json))
  rownames(query_results_table) <- query_results_table[,1]
  
  # Keep studies with > 10 metabolites
  num_metabolites <- as.numeric(query_results_table[,"num_metabolites"])
  keep.inx <- num_metabolites > 10
  query_results_table_keep <- query_results_table[keep.inx, ]
  query_results_table_keep <- subset(query_results_table_keep, select = - c(analysis_id, analysis_type, 
                                                                            num_metabolites,
                                                                            ms_type, units))
  
  query_results_table_keep <- data.frame(query_results_table_keep)
  
  load_stringr()
  query_results_table_keep$lipid <- stringr::str_detect(query_results_table_keep$study_title, fixed("lipid", ignore_case=TRUE))
  
  mSetObj$dataSet$NMDR_studies <- query_results_table_keep
  
  if(!.on.public.web){
    fast.write.csv(query_results_table_keep, "all_metabolomics_workbench_studies.csv")
  }else{
    qs::qsave(query_results_table_keep, "metabolomics_workbench_studies.qs")
  }
  
  return(.set.mSet(mSetObj));
}