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
