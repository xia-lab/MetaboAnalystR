#'Various functions for mapping b/w names & database identifiers

#'Given a list of compound names or ids, find matched name or ids from selected databases
#'@description Given a list of compound names or ids
#'find matched name or IDs from selected databases
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param q.type Input the query type, "name" for compound names, "hmdb" for HMDB IDs, "kegg" for KEGG IDs, "pubchem"
#'for PubChem CIDs, "chebi" for ChEBI IDs, "metlin" for METLIN IDs, and "hmdb_kegg" for a both KEGG and HMDB IDs.
#'@param hmdb Logical, T to cross reference to HMDB, F to not.
#'@param pubchem Logical, T to cross reference to PubChem, F to not.
#'@param chebi Logical, T to cross reference to CheBI, F to not.
#'@param kegg Logical, T to cross reference to KEGG, F to not.
#'@param metlin Logical, T to cross reference to MetLin, F to not.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CrossReferencing <- function(mSetObj=NA, q.type, hmdb=T, pubchem=T, chebi=F, kegg=T, metlin=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # record the filter for 8 major databases
  mSetObj$return.cols <- c(hmdb, pubchem, chebi, kegg, metlin);
  
  # record all the data
  if(!exists("name.map", where = mSetObj)){
    mSetObj$name.map <- list();
  }
  
  # distribute job
  mSetObj$dataSet$q.type <- q.type;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    MetaboliteMappingExact(mSetObj, q.type);
    mSetObj <- .get.mSet(mSetObj);
  }else{
    mSetObj <- MetaboliteMappingExact(mSetObj, q.type);
  }
  
  # do some sanity check
  todo.inx <- which(is.na(mSetObj$name.map$hit.inx));
  
  if(length(todo.inx)/length(mSetObj$name.map$hit.inx) > 0.5){
    mSetObj$msgSet$nmcheck.msg <- c(0, "Over half of the compound IDs could not be matched to our database. Please make 
                                    sure that correct compound IDs or common compound names are used.");
  }else if (length(todo.inx) > 15){
    mSetObj$msgSet$nmcheck.msg <- c(2, "There are >15 compounds without matches. You can either proceed or if necessary, update these compound IDs and upload again.");        
  }else{
    mSetObj$msgSet$nmcheck.msg <- c(1, "Name matching OK, please inspect (and manual correct) the results then proceed.");   
  }
  return(.set.mSet(mSetObj));
}

#'Mapping from different metabolite IDs
#'@description For compound names to other ids, can do exact or approximate matches
#'For other IDs, except HMDB ID, all others may return multiple/non-unique hits
#'Multiple hits or non-unique hits will allow users to manually select
#'@param mSetObj Input the name of the created mSetObj.
#'@param q.type Inpute the query-type, "name" for compound names, "hmdb" for HMDB IDs, "kegg" for KEGG IDs, "pubchem"
#'for PubChem CIDs, "chebi" for ChEBI IDs, "metlin" for METLIN IDs, and "hmdb_kegg" for a both KEGG and HMDB IDs.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
MetaboliteMappingExact <- function(mSetObj=NA, q.type){
  
  mSetObj <- .get.mSet(mSetObj);
  qvec <- mSetObj$dataSet$cmpd;
  
  # variables to record results
  hit.inx <- vector(mode='numeric', length=length(qvec)); # record hit index, initial 0
  names(hit.inx) <- qvec;
  match.values <- vector(mode='character', length=length(qvec)); # the best matched values (hit names), initial ""
  match.state <- vector(mode='numeric', length=length(qvec));  # match status - 0, no match; 1, exact match; initial 0 
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  if(q.type == "hmdb"){
    n <- 5 # Number of digits for V3 of HMDB
    hmdb.digits <- as.vector(sapply(cmpd.db$hmdb, function(x) strsplit(x, "HMDB", fixed=TRUE)[[1]][2]))
    hmdb.v3.ids <- paste0("HMDB", substr(hmdb.digits, nchar(hmdb.digits)-n+1, nchar(hmdb.digits)))
    hit.inx.v3 <- match(tolower(qvec), tolower(hmdb.v3.ids));
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx[is.na(hit.inx)] <- hit.inx.v3[is.na(hit.inx)]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "pubchem"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$pubchem));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "chebi"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$chebi));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "metlin"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$metlin));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "kegg"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$kegg));
    #hit.inx2 <- match(tolower(qvec), rev(tolower(cmpd.db$kegg)));
    
    # unique hits
    #nonuniq.hits <- hit.inx + hit.inx2 != nrow(cmpd.db) + 1;
    #hit.inx[nonuniq.hits] <- NA;
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }else if(q.type == "name"){
    # first find exact match to the common compound names
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$name));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
    # then try to find exact match to synonyms for the remaining unmatched query names one by one
    if(anal.type %in% c("msetora", "msetssp", "msetqea")){
      syn.db <- .get.my.lib("class_syn_nms_2020.qs")
    }else{
      syn.db <- .get.my.lib("syn_nms.qs")
    }

    syns.list <-  syn.db$syns.list;
    todo.inx <-which(is.na(hit.inx));

    if(length(todo.inx) > 0){
      for(i in 1:length(syns.list)){
        syns <-  syns.list[[i]];
        hitInx <- match(tolower(qvec[todo.inx]), tolower(syns));
        
        hitPos <- which(!is.na(hitInx));
        if(length(hitPos)>0){
          # record matched ones
          orig.inx<-todo.inx[hitPos];
          hit.inx[orig.inx] <- i;                  
          # match.values[orig.inx] <- syns[hitInx[hitPos]];  # show matched synnames
          match.values[orig.inx] <- cmpd.db$name[i];    # show common name
          match.state[orig.inx] <- 1;
          
          # update unmatched list
          todo.inx<-todo.inx[is.na(hitInx)];
        }
        if(length(todo.inx) == 0) break;
      }
    }
  }else{
    print(paste("Unknown compound ID type:", q.type));
    # guess a mix of kegg and hmdb ids
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx2 <- match(tolower(qvec), tolower(cmpd.db$kegg));
    nohmdbInx <- is.na(hit.inx);
    hit.inx[nohmdbInx]<-hit.inx2[nohmdbInx]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }
  # empty memory
  gc();
  
  mSetObj$name.map$query.vec <- qvec; 
  mSetObj$name.map$hit.inx <- hit.inx;
  mSetObj$name.map$hit.values <- match.values;
  mSetObj$name.map$match.state <- match.state;
  
  return(.set.mSet(mSetObj));
}

#' Perform detailed name match
#'@description Given a query, perform compound matching. 
#'@param mSetObj Input name of the created mSet Object.
#'@param q Input the query.
#'@export
#'
PerformDetailMatch <- function(mSetObj=NA, q){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$q.type == "name"){
    PerformApproxMatch(mSetObj, q);
  }else{
    PerformMultiMatch(mSetObj, q);
  }
}

#' Perform multiple name matches
#'@description Given a query, performs compound name matching. 
#'@param mSetObj Input name of the created mSet Object.
#'@param q Input the query.
#'@export
#'
PerformMultiMatch <- function(mSetObj=NA, q){
  
  mSetObj <- .get.mSet(mSetObj);

  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
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
#'@export
#'
PerformApproxMatch <- function(mSetObj=NA, q){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }

  # only for none lipids
  nonLipidInx <- cmpd.db$lipid == 0;
  com.nms <- cmpd.db$name[nonLipidInx];
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    syn.db <- .get.my.lib("class_syn_nms_2020.qs")
  }else{
    syn.db <- .get.my.lib("syn_nms.qs")
  }

  syns.vec <- syn.db$syns.vec[nonLipidInx];
  syns.list <- syn.db$syns.list[nonLipidInx];
  
  matched.dist <- NULL;
  q.length <- nchar(q);
  s <- c(0, 0.1, 0.2);
  
  # init withno hits, then see if any hits
  mSetObj$dataSet$candidates <- NULL;
  for (j in s) {
    new.q <- q;
    if(q.length > 32){ # note: agrep fail for exact match when length over 32 characters
      new.q<-substr(q, 1, 32);
    }
    matched <- FALSE;
    matched.inx <- agrep(new.q, syns.vec, ignore.case=T, max.distance=j, useBytes=T);
    
    if(length(matched.inx) > 0) {
      # record all the candidates,
      # don't use cbind, since all will be converted to character mode
      # for data.frame specify "stringsAsFactors" to prevent convert value col into factor
      candidates <- data.frame(index=vector(mode = "numeric", length=length(matched.inx)),
                               value=vector(mode = "character", length=length(matched.inx)),
                               score=vector(mode = "numeric", length=length(matched.inx)),
                               stringsAsFactors = FALSE);
      
      for(n in 1:length(matched.inx)){
        nm.vec<-syns.list[[matched.inx[n]]];
        # try approximate match, note: in some cases, split into element will break match using whole string
        hit3.inx <- agrep(q,nm.vec,ignore.case=T, max.distance=j, useBytes=T);
        if(length(hit3.inx)>0){
          hit3.nm <- vector(mode = "character", length=length(hit3.inx));
          hit3.score <- vector(mode = "numeric", length=length(hit3.inx));
          for(k in 1:length(hit3.inx)){
            idx <- hit3.inx[k];
            hit3.nm[k] <- nm.vec[idx];
            hit3.score[k] <- j + abs(nchar(nm.vec[idx])-nchar(q))/(10*nchar(q));
          }
          
          # now get the best match, the rule is that the first two character should matches
          # first check if first two character are digits or characters, otherwise will cause error
          matches2 <- c();
          if(length(grep("^[1-9a-z]{2}", q, ignore.case=T))>0){
            matches2 <- grep(paste("^", substr(q, 1, 2), sep=""), hit3.nm);
          }else if (length(grep("^[1-9a-z]", q, ignore.case=T))>0){
            matches2 <- grep(paste("^", substr(q, 1, 1), sep=""), hit3.nm);
          }
          
          if(length(matches2)>0){
            hit3.score[matches2] <- hit3.score[matches2] - 0.05;
          }
          
          best.inx<-which(hit3.score==min(hit3.score))[1];
          candidates[n,1]<-matched.inx[n];
          #    candidates[n,2]<-hit3.nm[best.inx]; # show matched syn names
          candidates[n,2]<-com.nms[matched.inx[n]] # show common names
          candidates[n,3]<-hit3.score[best.inx];
        }      
      }
      
      rm.inx <- is.na(candidates[,2]) | candidates[,2]=="NA" | candidates[,2]=="";
      mSetObj$dataSet$candidates<-candidates[!rm.inx, ];  
      mSetObj$dataSet$candidates<-candidates[order(candidates[,3], decreasing=F), , drop=F];    
      
      if(nrow(candidates) > 10){
        mSetObj$dataSet$candidates<-candidates[1:10,];
      }
      return(.set.mSet(mSetObj));
    }
  }
  return(.set.mSet(mSetObj));
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
  query_inx <- which(mSetObj$name.map$query.vec == query_nm);
  
  can_mat <- mSetObj$dataSet$candidates;
  
  if(!is.null(can_mat)){

    if(anal.type %in% c("msetora", "msetssp", "msetqea")){
      cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
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

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Get all candidate compound names for a given index 
#'@description Returns 3 coloumns - inx, name, score
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

GetCandidateList <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  can_hits <- mSetObj$dataSet$candidates;
  
  if(is.null(can_hits)){
    can.mat <- matrix("", nrow=1, ncol= 6);
  }else{
    # construct the result table with cells wrapped in html tags
    # the unmatched will be highlighted in different background
    
    can.mat <- matrix("", nrow=nrow(can_hits)+1, ncol= 6);
    
    if(anal.type %in% c("msetora", "msetssp", "msetqea")){
      cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
    }else{
      cmpd.db <- .get.my.lib("compound_db.qs");
    }
    
    # need to exclude lipids, to be consistent with approx matching part so that same index can be used to fetch db entries
    nonLipidInx <- cmpd.db$lipid == 0;
    cmpd.db <-cmpd.db[nonLipidInx,];
    
    for (i in 1:nrow(mSetObj$dataSet$candidates)){
      hit.inx <- mSetObj$dataSet$candidates[i, 1];
      hit.name <- mSetObj$dataSet$candidates[i, 2];
      hit <-cmpd.db[hit.inx, ,drop=F];
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

#'Return the final (after user selection) map as dataframe
#'@description Returns three columns: original name, HMDB name and KEGG ID,
#'for enrichment and pathway analysis, respectively
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetFinalNameMap <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  hit.inx <- mSetObj$name.map$hit.inx;
  hit.values <- mSetObj$name.map$hit.values;
  match.state <- mSetObj$name.map$match.state;
  
  qvec <- mSetObj$dataSet$cmpd;
  nm.mat <- matrix(nrow=length(qvec), ncol=4);
  colnames(nm.mat) <- c("query", "hmdb",  "kegg", "hmdbid");
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  for (i in 1:length(qvec)){
    hit <-cmpd.db[hit.inx[i], ,drop=F];
    if(match.state[i]==0){
      hmdb.hit <- NA;
      hmdb.hit.id <- NA;
      kegg.hit <- NA;
    }else{
      hmdb.hit <- ifelse(nchar(hit.values[i])==0, NA, hit.values[i]);
      hmdb.hit.id <- ifelse(nchar(hit$hmdb_id)==0, NA, hit$hmdb_id);
      kegg.hit <- ifelse(nchar(hit$kegg_id)==0, NA, hit$kegg_id);
    }
    nm.mat[i, ]<-c(qvec[i], hmdb.hit, kegg.hit, hmdb.hit.id);
  }
  return(as.data.frame(nm.mat));
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
CreateMappingResultTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  qvec <- mSetObj$dataSet$cmpd;
  
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
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("class_compound_db_2020.qs");
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
