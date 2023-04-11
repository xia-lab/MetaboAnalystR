
CleanLipidNames <- function(qvec){
  
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.clean.lipid")){ # public web on same user dir
      .load.scripts.on.demand("util_lipid.Rc");    
    }
    return(my.clean.lipid(qvec));
  }else{
    return(my.clean.lipid(qvec));
  }
  
}

#' Perform detailed name match
#'@description Given a query, perform compound matching. 
#'@param mSetObj Input name of the created mSet Object.
#'@param q Input the query.
#'@export
#'
PerformDetailMatch <- function(mSetObj=NA, q){

  mSetObj <- .get.mSet(mSetObj);
  
  lipid = mSetObj$lipid.feats

  if(mSetObj$dataSet$q.type == "name"){
    PerformApproxMatch(mSetObj, q, lipid);
  }else{
    PerformMultiMatch(mSetObj, q, lipid);
  }
}

#' Perform multiple name matches
#'@description Given a query, performs compound name matching. 
#'@param mSetObj Input name of the created mSet Object.
#'@param q Input the query.
#'@param lipid lipid, logical
#'@export
#'
PerformMultiMatch <- function(mSetObj=NA, q, lipid){
  
  mSetObj <- .get.mSet(mSetObj);

  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  matched.inx <- which(cmpd.db$kegg %in% q);
  if(length(matched.inx) > 0) {
    # record all the candidates,
    candidates <- cbind(matched.inx, cmpd.db$name[matched.inx]);
    mSetObj$dataSet$candidates <- candidates;
  }else{
    mSetObj$dataSet$candidates <- NULL;
  }
  return(.set.mSet(mSetObj));
}

#'Perform approximate compound matches
#'@description Given a query, perform approximate compound matching 
#'@param mSetObj Input the name of the created mSetObj.
#'@param q Input the q vector.
#'@param lipid lipid, logical
#'@export
#'
PerformApproxMatch <- function(mSetObj=NA, q, lipid){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.approx.match")){ # public web on same user dir
      .load.scripts.on.demand("util_approx.Rc");    
    }
    return(my.approx.match(mSetObj, q, lipid));
  }else{
    return(my.approx.match(mSetObj, q, lipid));
  }
}

#'Set matched name based on user selection from all potential hits
#'@description Note: to change object in the enclosing enviroment, use "<<-"
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param query_nm Input the query name.
#'@param can_nm Input the candidate name.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetCandidate <- function(mSetObj=NA, query_nm, can_nm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  lipid = mSetObj$lipid.feats
  
  query_inx <- which(mSetObj$name.map$query.vec == query_nm);
  
  can_mat <- mSetObj$dataSet$candidates;
  
  if(!is.null(can_mat)){

    if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
      cmpd.db <- .get.my.lib("lipid_compound_db.qs");
    }else if(anal.type == "utils"){
      cmpd.db <- .get.my.lib("master_compound_db.qs");
    }else{
      cmpd.db <- .get.my.lib("compound_db.qs");
    }
    
    can_inx <- which(can_mat[,2] == can_nm);
    
    if(can_inx <= nrow(can_mat)){
      can_inx <- which(cmpd.db$name == can_nm);
      hit <- cmpd.db[can_inx, ,drop=F];
      mSetObj$name.map$hit.inx[query_inx] <- can_inx;
      mSetObj$name.map$hit.values[query_inx] <- hit[,2];
      mSetObj$name.map$match.state[query_inx] <- 1;
      
      # re-generate the CSV file
      csv.res <- mSetObj$dataSet$map.table;
      if(ncol(csv.res) > 7){ # general utilities
        csv.res[query_inx, ]<-c(csv.res[query_inx, 1],
                                mSetObj$name.map$hit.values[query_inx],
                                hit$hmdb_id,
                                hit$pubchem_id,
                                hit$chebi_id,
                                hit$kegg_id,
                                hit$metlin_id,
                                hit$smiles,
                                1);
      }else{ # pathway analysis
        csv.res[query_inx, ]<-c(csv.res[query_inx, 1],
                                mSetObj$name.map$hit.values[query_inx],
                                hit$hmdb_id,
                                hit$pubchem_id,
                                hit$kegg_id,
                                hit$smiles,
                                1);
      }
      fast.write.csv(csv.res, file="name_map.csv", row.names=F);
      mSetObj$dataSet$map.table <- csv.res;
    }else{ #no match
      mSetObj$name.map$hit.inx[query_inx] <- 0;
      mSetObj$name.map$hit.values[query_inx] <- "";
      mSetObj$name.map$match.state[query_inx] <- 0;
      print("No name matches found.")
    }
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(query_inx);
  }else{
    return(.set.mSet(mSetObj));
  }
}

CrossReferencingAPI <- function(mSetObj=NA, inputType){
  
   # make this lazy load
    if(!exists("my.namemap.api")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }

    mSetObj <- .get.mSet(mSetObj);
    toSend <- list(mSet = mSetObj, inputType = inputType, analType = anal.type)
    saveRDS(toSend, "tosend.rds");
    return(my.namemap.api());
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Get all candidate compound names for a given index 
#'@description Returns 3 coloumns - inx, name, score
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param lipid Logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

GetCandidateList <- function(mSetObj=NA, lipid){

  mSetObj <- .get.mSet(mSetObj);
  
  lipid = mSetObj$lipid.feats
  
  can_hits <- mSetObj$dataSet$candidates;
  
  if(is.null(can_hits)){
    can.mat <- matrix("", nrow=1, ncol= 6);
  }else{
    # construct the result table with cells wrapped in html tags
    # the unmatched will be highlighted in different background
    
    can.mat <- matrix("", nrow=nrow(can_hits)+1, ncol= 6);

    if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
      cmpd.db <- .get.my.lib("lipid_compound_db.qs");
    }else if(anal.type == "utils"){
      cmpd.db <- .get.my.lib("master_compound_db.qs");
    }else{
      cmpd.db <- .get.my.lib("compound_db.qs");
    }
    
    if(!lipid){
      # need to exclude lipids, to be consistent with approx matching part so that same index can be used to fetch db entries
      nonLipidInx <- cmpd.db$lipid == 0;
      cmpd.db <-cmpd.db[nonLipidInx,];
    }
    
    for (i in 1:nrow(mSetObj$dataSet$candidates)){
      hit.inx <- mSetObj$dataSet$candidates[i, 1];
      hit.name <- mSetObj$dataSet$candidates[i, 2];
      hit <- cmpd.db[hit.inx, ,drop=F];
      can.mat[i, ] <- c(hit.name,
                        paste(ifelse(hit$hmdb_id=="NA","", paste("<a href=http://www.hmdb.ca/metabolites/", hit$hmdb_id, " target='_blank'>",hit$hmdb_id,"</a>", sep="")), sep=""),
                        paste(ifelse(hit$pubchem_id=="NA", "", paste("<a href=http://pubchem.ncbi.nlm.nih.gov/summary/summary.cgi?cid=", hit$pubchem_id," target='_blank'>", hit$pubchem_id,"</a>", sep="")), sep=""),
                        paste(ifelse(hit$chebi_id=="NA","", paste("<a href=http://www.ebi.ac.uk/chebi/searchId.do?chebiId=", hit$chebi_id, " target='_blank'>",hit$chebi_id,"</a>", sep="")), sep=""),
                        paste(ifelse(hit$kegg_id=="NA","",paste("<a href=http://www.genome.jp/dbget-bin/www_bget?", hit$kegg_id, " target='_blank'>", hit$kegg_id,"</a>", sep="")), sep=""),
                        paste(ifelse(hit$metlin_id=="NA","",paste("<a href=http://metlin.scripps.edu/metabo_info.php?molid=", hit$metlin_id," target='_blank'>",hit$metlin_id,"</a>", sep="")), sep=""));
    }
    # add "none" option
    can.mat[nrow(mSetObj$dataSet$candidates)+1,] <- c("None of the above", "", "", "", "", "");
  }
  # add the hit columns
  return.cols <- c(TRUE, mSetObj$return.cols);
  
  if(.on.public.web){
    return(as.vector(can.mat[,return.cols, drop=F]));
  }
  
  mSetObj$name.map$hits.candidate.list <- can.mat[,mSetObj$return.cols, drop=F]
  
  return(.set.mSet(mSetObj));
}

GetCanListRowNumber <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$candidates)){
    return(1);
  }else{
    return(nrow(mSetObj$dataSet$candidates)+1); # include the "none" row
  }
}

GetQuery <- function(mSetObj=NA, inx){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$cmpd[inx]);
}



#'Get mapping table
#'@description Return results from compound name mapping in a table
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export

GetMapTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  print(xtable::xtable(mSetObj$dataSet$map.table, caption="Result from Compound Name Mapping"),
        tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
}

#'Creates the mapping result table
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
CreateMappingResultTable <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj);
  lipid <- mSetObj$lipid.feats;
 
  if(!is.null(lipid) & anal.type == "msetqea"){
    qvec <- names(mSet$dataSet$url.var.nms);
  }else{
    qvec <- mSetObj$dataSet$cmpd;
  }
  
  if(is.null(qvec)){
    return();
  }
  # style for highlighted background for unmatched names
  pre.style<-NULL;
  post.style<-NULL;
  
  # style for no matches
  if(mSetObj$dataSet$q.type == "name"){
    no.prestyle<-"<strong style=\"background-color:yellow; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }else{
    no.prestyle<-"<strong style=\"background-color:red; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }
  
  hit.inx<-mSetObj$name.map$hit.inx;
  hit.values<-mSetObj$name.map$hit.values;
  match.state<-mSetObj$name.map$match.state;
  
  # construct the result table with cells wrapped in html tags
  # the unmatched will be highlighted in different background
  html.res <- matrix("", nrow=length(qvec), ncol=8);
  csv.res <- matrix("", nrow=length(qvec), ncol=9);
  colnames(csv.res) <- c("Query", "Match", "HMDB", "PubChem", "ChEBI", "KEGG", "METLIN", "SMILES", "Comment");

  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  for (i in 1:length(qvec)){
    if(match.state[i]==1){
      pre.style<-"";
      post.style="";
    }else{ # no matches
      pre.style<-no.prestyle;
      post.style<-no.poststyle;
    }
    hit <-cmpd.db[hit.inx[i], ,drop=F];
    html.res[i, ]<-c(paste(pre.style, qvec[i], post.style, sep=""),
                     paste(ifelse(match.state[i]==0, "", hit.values[i]), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$hmdb_id) || hit$hmdb_id=="" || hit$hmdb_id=="NA","-", paste("<a href=http://www.hmdb.ca/metabolites/", hit$hmdb_id, " target='_blank'>",hit$hmdb_id,"</a>", sep="")),  sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$pubchem_id) || hit$pubchem_id=="" || hit$pubchem_id=="NA", "-", paste("<a href=http://pubchem.ncbi.nlm.nih.gov/summary/summary.cgi?cid=", hit$pubchem_id," target='_blank'>", hit$pubchem_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$chebi_id) || hit$chebi_id==""|| hit$chebi_id=="NA","-", paste("<a href=http://www.ebi.ac.uk/chebi/searchId.do?chebiId=", hit$chebi_id, " target='_blank'>",hit$chebi_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$kegg_id) || hit$kegg_id==""|| hit$kegg_id=="NA","-",paste("<a href=http://www.genome.jp/dbget-bin/www_bget?", hit$kegg_id, " target='_blank'>", hit$kegg_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$metlin_id) || hit$metlin_id==""|| hit$metlin_id=="NA","-",paste("<a href=http://metlin.scripps.edu/metabo_info.php?molid=", hit$metlin_id," target='_blank'>",hit$metlin_id,"</a>", sep="")), sep=""),
                     ifelse(match.state[i]!=1,"View",""));
    csv.res[i, ]<-c(qvec[i],
                    ifelse(match.state[i]==0, "NA", hit.values[i]),
                    ifelse(match.state[i]==0, "NA", hit$hmdb_id),
                    ifelse(match.state[i]==0, "NA", hit$pubchem_id),
                    ifelse(match.state[i]==0, "NA", hit$chebi_id),
                    ifelse(match.state[i]==0, "NA", hit$kegg_id),
                    ifelse(match.state[i]==0, "NA", hit$metlin_id),
                    ifelse(match.state[i]==0, "NA", hit$smiles),
                    match.state[i]);
  }
  # return only columns user selected
  
  # add query and match columns at the the beginning, and 'Detail' at the end
  return.cols <- c(TRUE, TRUE, mSetObj$return.cols, TRUE);
  html.res <- html.res[,return.cols, drop=F];
  csv.res <- csv.res[,return.cols, drop=F];
  
  # store the value for report
  mSetObj$dataSet$map.table <- csv.res;
  fast.write.csv(csv.res, file="name_map.csv", row.names=F);
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(as.vector(html.res));
  }else{
    return(.set.mSet(mSetObj));
  }
}

GetHitsRowNumber<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(length(mSetObj$name.map$hit.inx));
}

GetPathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$path.res[,1]);
}

GetMatchedCompounds<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$path.res[,2]);
}

GetIsLipids <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  is.lipid <- mSetObj$lipid.feats
  
  if(is.lipid){
    is.lipid <- "lipid"
  }else{
    is.lipid <- "met"
  }
  
  return(is.lipid)
}