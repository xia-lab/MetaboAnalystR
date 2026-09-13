###############################
## Metabolite set library
###############################

#'Set the cachexia set used
#'@description Set cachexia set used
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param used Set data to be used
#'@export
#'
SetCachexiaSetUsed <- function(mSetObj=NA, used){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cachexia.set.used <- used;
  return(.set.mSet(mSetObj));
}

#'Set current user selected metset library for search
#'@description if enrichment analysis, also prepare lib by
#'creating a list of metabolite sets
#'@usage SetCurrentMsetLib(mSetObj=NA, libname, excludeNum)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param libname Input user selected name of library, "self", "kegg_pathway",
#'"smpdb_pathway", "blood", "urine", "csf", "snp", "predicted", "location", and "drug".
#'@param excludeNum Users input the mimimum number compounds within selected metabolite sets (metabolitesets < excludeNum)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetCurrentMsetLib <- function(mSetObj=NA, libname, excludeNum=0){

  mSetObj <- .get.mSet(mSetObj);

  if(libname=="self"){
    ms.list <- mSetObj$dataSet$user.mset;
    ms.list <- lapply(ms.list, function(x) unique(unlist(strsplit(x, "; ", fixed=TRUE))));
    current.msetlib <- vector("list", 3)
    names(current.msetlib) <- c("name", "member", "reference")
    mSetObj$analSet$msetlibname <- libname;
  } else {
    if(!.on.public.web & grepl("kegg", libname)){ # api only for KEGG msets
      mSetObj$api$libname <- libname
      mSetObj$api$excludeNum = excludeNum
      mSetObj$analSet$msetlibname <- libname
      return(.set.mSet(mSetObj));
    }
    
    # feature enhancement https://omicsforum.ca/t/error-in-setcurrentmsetlib-function-in-r/2058
    # Also check if current.msetlib has already been processed (has $member)
    need_reload <- !exists("current.msetlib") ||
                   is.null(mSetObj$analSet$msetlibname) ||
                   mSetObj$analSet$msetlibname != libname ||
                   !is.null(current.msetlib$member);  # Already processed, need fresh load

    if(need_reload) {
        destfile <- paste(libname, ".qs", sep = "");
        if(.on.public.web){
            my.qs  <- paste(rpath, "libs/msets/", destfile, sep="");
            current.msetlib <- ov_qs_read(my.qs);
        } else {
            my.qs <- paste("https://www.metaboanalyst.ca/resources/libs/msets/", destfile, sep="");
            if(!file.exists(destfile)){
                download.file(my.qs, destfile, method = "curl");
            }
            current.msetlib <- ov_qs_read(destfile);
        }
        mSetObj$analSet$msetlibname <- libname;
    }
    # create a named list, use the ids for list names
    # https://github.com/xia-lab/MetaboAnalystR/issues/172
    ms.list <- iconv(current.msetlib[, 3], from = 'utf8', to = 'utf8');
    ms.list <- lapply(ms.list, function(x) unique(unlist(strsplit(x, "; ", fixed=TRUE))));
    names(ms.list) <- current.msetlib[,2];
  }

  if(excludeNum > 0){
    cmpd.count <- lapply(ms.list, length);
    sel.inx <- cmpd.count >= excludeNum;
    ms.list <- ms.list[sel.inx];
    
    if(libname!="self"){
      current.msetlib <- current.msetlib[sel.inx,];
    }
  }
  
  # total uniq cmpds in the mset lib
  mSetObj$dataSet$uniq.count <- length(unique(unlist(ms.list, use.names = FALSE)));
  
  # update current.mset and push to global env
  current.msetlib$member <- ms.list;
  
  if(libname=="self"){
    current.msetlib$name <- names(ms.list)
    current.msetlib$reference <- rep("User-uploaded", length(ms.list))
  }

  current.msetlib <<- current.msetlib;
  ov_qs_save(current.msetlib, "current.msetlib.qs");
  return(.set.mSet(mSetObj));
}


#'Read user upload metabolite set library file
#'@description Return two col csv file, first name, second cmpd list
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param filePath Input the path to the user's uploaded metabolite set library
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.UserMsetLibData<-function(mSetObj=NA, filePath){
  
  mSetObj <- .get.mSet(mSetObj);
  
  dat <- .readDataTable(filePath);
  libCheck.msg <- NULL;
  if(class(dat) == "try-error") {
    libCheck.msg <-c(libCheck.msg, "Data format error - fail to read in the data!");
    AddErrMsg(libCheck.msg);
    return(0);
  }
  
  if(is.null(dim(dat)) || dim(dat)[2]!=2){
    libCheck.msg <-c(libCheck.msg, "Data format error - must have two columns!");
    AddErrMsg(libCheck.msg);
    return(0);
  }
  
  # create a named list, use the ids for list names
  mset.list<-strsplit(dat[,2],"; ", fixed=TRUE);
  mset.ids <- paste("USER", sprintf("%04d",1:nrow(dat)), sep="");
  names(mset.list)<-dat[,1];
  names(mset.ids)<-dat[,1];
  
  cmpd.db <- .get.my.lib("compound_db.qs");
  
  # now need to check all metabolites match HMDB names
  # and get the statistics
  unmatched.nms <- NULL;
  unmatched.num <- 0;
  hmdb.nms <- tolower(cmpd.db$name);
  for(i in 1:length(mset.list)){
    mset <- mset.list[[i]];
    hit.inx <- match(tolower(mset), hmdb.nms);
    unmatched.nms <- c(unmatched.nms, mset[is.na(hit.inx)]);
    unmatched.num <- unmatched.num + sum(is.na(hit.inx));
  }
  
  # save the lib data
  mSetObj$dataSet$user.mset <- mset.list;
  mSetObj$dataSet$user.mset.ids <- mset.ids;
  
  if(unmatched.num > 0) {
    mSetObj$dataSet$user.mset.info <- paste("A total of", unmatched.num, "compounds were not matched to HMDB common names.",
                                            "They are:", paste(unmatched.nms, collapse="; "), ". Please correct these names. Otherwise,",
                                            "they will be ignored during the enrichment analysis.");
  }else{
    mSetObj$dataSet$user.mset.info <- paste("A total of", length(mset.list), "were sucessfully added to the library.");
  }
  
  return(.set.mSet(mSetObj));
  
}

#'Get the library check messages
#'@description Get the library check messages
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
#'
GetMsetLibCheckMsg<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (mSetObj$dataSet$user.mset.info);
}

#'Get the concentration reference
#'@description Get the concentration reference
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpd.nm Input the compound name
#'@export
#'
Get.ConcRef<-function(mSetObj=NA, cmpd.nm){
  mSetObj <- .get.mSet(mSetObj);
  
  if(!exists('conc.db')){
    if(.on.public.web){
      conc.db <<-  .readDataTable(paste0(rpath ,"libs/cmpd_conc.csv"));
    }else{
      conc.db <<-  .readDataTable("https://www.metaboanalyst.ca/resources/libs/cmpd_conc.csv");
    }
  }
  matches <- subset(conc.db, name == cmpd.nm & biotype==mSetObj$dataSet$biofluid, select=c(conc, pubmed, references, notes));
  if(nrow(matches)==0){
    return(NA);
  }
  return(list(concs = matches$conc, pmid = matches$pubmed, refs = matches$references, note = matches$notes));
}

#'Search metabolite set libraries
#'@description Search metabolite set libraries
#'@param mSetObj Input name of the created mSet Object
#'@param query Input the query to search
#'@param type Input the data type (name or compound)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SearchMsetLibraries<-function(mSetObj=NA, query, type){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!exists("lib.search", where = mSetObj$dataSet)){
    mSetObj$dataSet$lib.search <<- list();
  }
  
  query <- ClearStrings(query);
  
  if(nchar(query)==0){
    return();
  }
  
  if(type=="name"){
    SearchByName(query);
  }else{
    SearchByCompound(query);
  }
}

#'Search for compound from all member compounds of metabolite set
#'@description Search for compound from all member compounds of metabolite set
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param query Input the query to search
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SearchByCompound <- function(mSetObj=NA, query){
  
  mSetObj <- .get.mSet(mSetObj);

    if(!exists("current.msetlib")){
        current.msetlib <<- ov_qs_read("current.msetlib.qs");
    }


  names.vec <- current.msetlib$member;
  matched.inx <- NULL;
  matched <- FALSE;
  exact = FALSE;
  
  # matching from close match to more dist match (max.dist = 0.5)
  # once a closer match found, stop trying more distant one
  matched.dist <- NULL;
  s <- seq(0, 0.2, .1)
  for (i in s) {
    matched.inx <- agrep(query,names.vec,ignore.case=T, max.distance=i);
    if(length(matched.inx) > 0) {
      matched.dist <- i;
      matched <- TRUE;
      break;
    }
  }
  
  if(matched){
    # now break down the set into each individual metabolites and find out which one gives the best hit
    matched.list<- vector(mode = "list", length=length(matched.inx));
    for(i in 1:length(matched.inx)){
      matched.list[i]<-strsplit(current.msetlib[matched.inx[i], "member"],"; *");
    }
    
    # re-do the matching, and record the matched values & sort
    matched.score <- NULL;
    hit.value <- vector(mode = "character", length=length(matched.inx)); # save the exact hit
    matched.value <- vector(mode = "character", length=length(matched.inx)); # save the whole metset
    for (i in 1:length(matched.inx)) {
      matched.nm <- matched.list[[i]];
      # test if it is exact match
      if((matched.dist == 0.0) & (!is.na(hit.inx <- match(tolower(query), tolower(matched.nm))))){
        matched.score[i] <- -1.0;
        exact <- TRUE;
      }else{ # try approximate match, note: we only record the first match in each set
        hit.inx <- agrep(query,matched.nm,ignore.case=T, max.distance=matched.dist)[1];
        # matched.dist 0.0, 0.1, 0.2, with the matches of the same distance, add fine adjustment
        # based on the length b/w query and matched name
        # use query length for normalization
        matched.score[i] <- matched.dist + abs(nchar(matched.nm[hit.inx])-nchar(query))/(1000*nchar(query));
      }
      
      # wrap up hit metabolite sets in html tags
      html.tag <- "<p>";
      for(m in 1:length(matched.list[[i]])){
        current.cmpd <- matched.list[[i]][m];
        if(m == hit.inx){
          current.cmpd <- paste("<font color=\"red\">", "<b>", current.cmpd, "</b>", "</font>",sep="");
        }
        if(m == 1){
          html.tag <- paste(html.tag, current.cmpd, sep="");
        }else {
          html.tag <- paste(html.tag, "; ", current.cmpd, sep="");
        }
      }
      hit.value[i] <-matched.list[[i]][hit.inx] ;
      matched.value[i] <- paste(html.tag, "</p>");
    }
    
    matched.table <- cbind(current.msetlib$name[matched.inx],
                           matched.value,
                           current.msetlib$reference[matched.inx]);
    if(exact){
      exact.inx <- matched.score == -1;
      mSetObj$dataSet$lib.search$matched.table <- matched.table[exact.inx, ];
      mSetObj$dataSet$lib.search$best.hit <- "NA";
    }else{
      # rank results based on the matched scores
      ord.inx <- order (matched.score, decreasing=F);
      mSetObj$dataSet$lib.search$matched.table <- matched.table[ord.inx, ];
      mSetObj$dataSet$lib.search$best.hit <- hit.value[ord.inx][1];
    }
  }else{
    mSetObj$dataSet$lib.search$best.hit <- "NA";
  }
  if(.on.public.web){
    .set.mSet(mSetObj);
    mSetObj$dataSet$lib.search
  }
  return(.set.mSet(mSetObj));
}

#'Given a metabolite set name, search its index
#'@description Given a metabolite set name, search its index
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param query Input the query to search 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SearchByName <- function(mSetObj=NA, query){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # no need for suggestions for metabolite set name search
  mSetObj$dataSet$lib.search$best.hit <- "NA";
  names.vec <- current.msetlib$name;
  matched <- FALSE;
  
  # matching from exact match (max.dist = 0) to more dist match (max.dist = 0.5)
  # once a closer match found, stop trying more distant one
  matched.inx <- match(tolower(query), tolower(names.vec));
  if(is.na(matched.inx)){ # try approximate match
    s <- seq(0, 0.2, .1)
    for (i in s) {
      matched.inx <- agrep(query,names.vec,ignore.case=T, max.distance=i);
      if(length(matched.inx) > 0) {
        matched = TRUE;
        break;
      }
    }
  }else{
    matched = TRUE;
  }
  
  if(matched){
    # wrap up in html tags
    matched.names <- paste("<p><font color=\"red\"><b>", names.vec[matched.inx], "</b></font></p>",sep="");
    mSetObj$dataSet$lib.search$matched.table <- cbind(matched.names, current.msetlib$member[matched.inx], current.msetlib$reference[matched.inx]);
  }else{
    mSetObj$dataSet$lib.search$matched.table <-"NA";
  }
  if(.on.public.web){
    .set.mSet(mSetObj);
    mSetObj$dataSet$lib.search$matched.table;
  }
  return(.set.mSet(mSetObj));
}

#'Set KEGG pathway library
#'@description note, this process can be long, need to return a value
#'to force Java to wait
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param lib.version Input the KEGG pathway version. "current" for the latest 
#'@param libNm lib name option
#'KEGG pathway library or "v2018" for the KEGG pathway library version prior to November 2019. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetKEGG.PathLib<-function(mSetObj=NA, libNm, lib.version){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$paramSet$lib.nm <- libNm; 
  mSetObj$msgSet$lib.msg <- paste("Your selected pathway library code is \\textbf{", libNm, "}(KEGG organisms abbreviation).");
  
  if(!.on.public.web){
    if(libNm %in% c("spym", "kva", "kpn", "cvr") & lib.version != "current"){
      AddErrMsg("Support for this organism is only available in the current version!");
      return(0);
    }
    mSetObj$api <- list()
    mSetObj$api$libVersion <- lib.version
    mSetObj$api$libNm <- libNm
  }else{
    sub.dir <- "kegg/metpa";
    destfile <- paste0(libNm, ".qs");
    current.kegglib <<- .get.my.lib(destfile, sub.dir);
    ov_qs_save(current.kegglib, "current.kegglib.qs");

    load_igraph();
  }
  
  mSetObj$pathwaylibtype <- "KEGG"
  return(.set.mSet(mSetObj));
}

#'Set SMPDB pathway library
#'@description note, this process can be long, need to return a value
#'to force Java to wait
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param libNm Input library name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetSMPDB.PathLib<-function(mSetObj=NA, libNm){
  
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$paramSet$lib.nm <- libNm; 
    mSetObj$msgSet$lib.msg <- paste("Your selected pathway library code is \\textbf{", libNm, "}(KEGG organisms abbreviation).");

    destfile <- paste0(libNm, ".qs");
    current.kegglib <<- .get.my.lib(destfile, "smpdb");
    ov_qs_save(current.kegglib, "current.kegglib.qs");

    load_igraph();

    mSetObj$pathwaylibtype <- "SMPDB"
    return(.set.mSet(mSetObj));
}

#'Read user uploaded metabolome as a list of KEGG pathway ids
#'@description Read user uploaded metabolome as a list of KEGG pathway ids
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param filePath Input the path to the user's list of KEGG pathway ids
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.KEGGReferenceMetabolome<-function(mSetObj=NA, filePath){
  
  mSetObj <- .get.mSet(mSetObj);
  
  inFile <- file(filePath, "r");
  ref.vec<-try(scan(inFile, 'character', strip.white = T, sep="\n")); # must be single column
  close(inFile);
  libCheck.msg <- NULL;
  
  if(class(ref.vec) == "try-error") {
    libCheck.msg <-c(libCheck.msg, "Data format error - fail to read in the data!");
    # print(libCheck.msg);
    AddErrMsg(libCheck.msg);
    return(0);
  }
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  # now need to check all metabolites match KEGG IDs
  # and get the statistics
  hits <- tolower(ref.vec)%in%tolower(cmpd.db$kegg_id);
  unmatched.num <- sum(!hits);
  unmatched.perc <- unmatched.num/length(hits);
  # test percentage
  if(unmatched.perc > 0.5){
    libCheck.msg <-c(libCheck.msg, "Over half of your uploaded IDs cannot be matched to our database! Please make sure they are KEGG IDs!");
    # print(libCheck.msg);
    AddErrMsg(libCheck.msg);
    return(0);
  }
  
  mSetObj$dataSet$metabo.filter.kegg <- ref.vec[hits];
  if(unmatched.num > 0) {
    unmatched.nms <- ref.vec[!hits];
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", unmatched.num, "compounds were not matched to KEGG compound IDs.",
                                             "They are:", paste(unmatched.nms, collapse="; "), ". Please correct these names. Otherwise,",
                                             "they will be ignored during the enrichment analysis.");
  }else{
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", length(ref.vec), "were successfully added to the library.");
  }
  return(.set.mSet(mSetObj));
}

#'Read user uploaded metabolome as a list of HMDB compound names
#'@description Read user uploaded metabolome as a list of HMDB compound names
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param filePath Input the path to the user's list of HMDB compound names 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.HMDBReferenceMetabolome<-function(mSetObj=NA, filePath){
  
  mSetObj <- .get.mSet(mSetObj);
  
  inFile <- file(filePath, "r");
  ref.vec<-try(scan(inFile, 'character', strip.white = T, sep="\n")); # must be single column
  close(inFile);
  libCheck.msg <- NULL;
  if(class(ref.vec) == "try-error") {
    libCheck.msg <-c(libCheck.msg, "Data format error - fail to read in the data!");
    AddErrMsg(libCheck.msg);
    return(0);
  }
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea")){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  # now need to check all metabolites match HMDB names
  # and get the statistics
  hits <- tolower(ref.vec)%in%tolower(cmpd.db$name);
  unmatched.num <- sum(!hits);

  unmatched.perc <- unmatched.num/length(hits);
  # test percentage
  if(unmatched.perc > 0.5){
    libCheck.msg <-c(libCheck.msg, "Over half of your uploaded IDs cannot be matched to our database! Please make sure they are valid HMDB names!");
    # print(libCheck.msg);
    AddErrMsg(libCheck.msg);
    return(0);
  }

  mSetObj$dataSet$metabo.filter.hmdb <- ref.vec[hits]; 
  if(unmatched.num > 0) {
    unmatched.nms <- ref.vec[!hits];
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", unmatched.num, "compounds were not matched to HMDB compound names.",
                                             "They are:", paste(unmatched.nms, collapse="; "), ". Please correct these names. Otherwise,",
                                             "they will be ignored during the enrichment analysis.");
  }else{
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", length(ref.vec), "were successfully added to the library.");
  }
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
############################################## 

#'Return the selected metset library to java for display
#'@description Return the selected metset library to java for display
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetMsetNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
    if(!exists("current.msetlib")){
        current.msetlib <<- ov_qs_read("current.msetlib.qs");
    }

  return(current.msetlib$name);
}

GetMsetMembers<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
    if(!exists("current.msetlib")){
        current.msetlib <<- ov_qs_read("current.msetlib.qs");
    }

  return(current.msetlib$member);
}

GetMsetReferences<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
    if(!exists("current.msetlib")){
        current.msetlib <<- ov_qs_read("current.msetlib.qs");
    }

  return(current.msetlib$reference);
}

GetRefLibCheckMsg<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$metabo.ref.info);
}

#'Set metabolome filter
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param TorF Input metabolome filter
#'@export
SetMetabolomeFilter<-function(mSetObj=NA, TorF){

  mSetObj <- .get.mSet(mSetObj);

  if(!.on.public.web){
    mSetObj$api$filter <- TorF
  }

  mSetObj$dataSet$use.metabo.filter <- TorF;
  return(.set.mSet(mSetObj));
}

#'Set per-compound rank scores for preranked GSEA-style enrichment
#'@description The complete-ranked-list input has two forms, exactly mirroring
#'the peak-based mummichog/GSEA module's "1 column" vs "3 column" upload formats
#'(PeakUploadView.xhtml): a plain ordered compound list (no score; rank = submitted
#'order), or a compound list carrying one numeric score per compound (fold change,
#'a signed p-value, etc, e.g. a vendor differential report with no raw per-sample
#'values). This function stores the latter -- one numeric score per compound, in
#'the same order as dataSet$cmpd -- so CalculateOraScore/CalculateHyperScore's
#'"gsea_like" branch can run genuine preranked GSEA (real magnitude, two-tailed
#'when scores carry both signs) instead of falling back to submission-order-only
#'ranking. Calling this is entirely optional; when it hasn't been called (or the
#'stored vector doesn't match the current compound list), "gsea_like" transparently
#'falls back to order-based ranking as before.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param scoreType "fc" (default), "p" or "stat" -- what the score column is; see the body.
#'@param scoreVec Numeric vector, same length and order as dataSet$cmpd
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.CmpdRankScore <- function(mSetObj=NA, scoreVec, scoreType="fc"){
  mSetObj <- .get.mSet(mSetObj);

  scoreVec <- suppressWarnings(as.numeric(scoreVec));
  if(length(scoreVec) != length(mSetObj$dataSet$cmpd)){
    AddErrMsg("Rank score vector length does not match the number of compounds!");
    return(0);
  }
  # What the column IS decides how it ranks -- the same three metrics Setup.CmpdRankData
  # accepts, so a pasted score and an uploaded table rank identically:
  #   "fc"   signed (log) fold change: used as is, direction = sign, magnitude = strength;
  #   "p"    p-value / FDR: smaller = more significant, so it becomes -log10(p) (a p of
  #          exactly 0 is clamped to the smallest other p, as in Setup.CmpdRankData);
  #   "stat" a signed statistic (t, sign(FC) * -log10 p): used as is.
  # After this, "larger |score| = more significant" holds for every metric, which is what
  # the permutation top fraction (|score|) and fgsea (scoreType std / pos) rely on.
  if(!scoreType %in% c("fc", "p", "stat")){
    AddErrMsg("scoreType must be one of: fc, p, stat");
    return(0);
  }
  if(scoreType == "p"){
    if(any(scoreVec < 0 | scoreVec > 1, na.rm=TRUE)){
      AddErrMsg("The score column was declared a p-value, but it has values outside [0, 1]!");
      return(0);
    }
    if(any(scoreVec == 0, na.rm=TRUE)){
      finite.p <- scoreVec[!is.na(scoreVec) & scoreVec > 0];
      floor.p <- if(length(finite.p)) min(finite.p) else .Machine$double.eps;
      scoreVec[!is.na(scoreVec) & scoreVec == 0] <- floor.p;
    }
    scoreVec <- -log10(scoreVec);
  }

  mSetObj$dataSet$cmpd.rank.score <- scoreVec;
  mSetObj$dataSet$cmpd.rank.score.type <- scoreType;
  return(.set.mSet(mSetObj));
}

#'Read a ranked compound list (name + fold change + p-value/FDR) for preranked GSEA
#'@description The data-loading entry point for the terminal/API mode counterpart
#'to pasting a one-column ranked list in the web upload page: reads a table with
#'one row per compound and a compound-name column plus a fold-change and/or
#'p-value column -- e.g. a vendor differential-abundance report (Metabolon, etc.)
#'covering EVERY compound, not just the significant ones. Auto-detects the
#'compound-name column (a header among "compound"/"name"/"metabolite"/"cmpd"/
#'"biochemical", else the first non-numeric column) and the fold-change/p-value
#'columns by common header aliases. Initializes a fresh mSetObj (data.type="list",
#'anal.type="msetora"), populates dataSet$cmpd, runs CrossReferencing(NA,"name")
#'to map names to HMDB, and stores the requested rank score via
#'Setup.CmpdRankScore. Call SetCurrentMsetLib()/SetCurrentPathLib() afterwards to
#'pick the library, then CalculateHyperScore("gsea_like")/CalculateOraScore(...,
#'"gsea_like").
#'@param mSetObj Ignored (kept for signature symmetry with other Setup.* functions);
#'this always starts a FRESH session via InitDataObjects, matching how ORA/QEA's
#'own compound-list entry points behave.
#'@param filePath Path to the ranked-compound table (csv/tsv/txt)
#'@param rankMetric "signed_p" (default; sign(log2FC) * -log10(p-value), the
#'standard preranked-GSEA score), "fc" (log2 fold change alone), or "p"
#'(-log10(p-value), unsigned -- for a list with no fold-change column).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.CmpdRankData <- function(mSetObj=NA, filePath, rankMetric="signed_p"){

  if(!rankMetric %in% c("signed_p", "fc", "p")){
    AddErrMsg("rankMetric must be one of: signed_p, fc, p");
    return(0);
  }

  dat <- .readDataTable(filePath);
  if(inherits(dat, "try-error") || is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0){
    AddErrMsg("Data format error - failed to read the ranked compound list!");
    return(0);
  }

  hdr <- tolower(trimws(colnames(dat)));
  find.col <- function(aliases){
    hit <- which(hdr %in% aliases);
    if(length(hit)) hit[1] else NA_integer_;
  }

  cmpd.col <- find.col(c("compound", "compound name", "name", "metabolite", "cmpd", "cmpd.name", "biochemical"));
  if(is.na(cmpd.col)){
    # Fall back to the first column that is not entirely numeric.
    is.num.col <- vapply(dat, function(x) suppressWarnings(all(!is.na(as.numeric(as.character(x))))), logical(1));
    non.num <- which(!is.num.col);
    if(!length(non.num)){
      AddErrMsg("Could not find a compound-name column in the uploaded file (expected a header like Compound/Name/Metabolite)!");
      return(0);
    }
    cmpd.col <- non.num[1];
  }

  fc.col  <- find.col(c("fc", "fold change", "foldchange", "log2fc", "log2foldchange", "log2(fc)"));
  p.col   <- find.col(c("p", "p.value", "pvalue", "p value", "raw p", "pval"));

  if(rankMetric %in% c("signed_p", "p") && is.na(p.col)){
    AddErrMsg(paste0("rankMetric=\"", rankMetric, "\" requires a p-value column, but none was found (expected a header like p.value/pvalue/raw p)!"));
    return(0);
  }
  if(rankMetric %in% c("signed_p", "fc") && is.na(fc.col)){
    AddErrMsg(paste0("rankMetric=\"", rankMetric, "\" requires a fold-change column, but none was found (expected a header like FC/fold change/log2FC)!"));
    return(0);
  }

  cmpd.nms <- trimws(as.character(dat[[cmpd.col]]));
  fc.vec <- if(!is.na(fc.col)) suppressWarnings(as.numeric(as.character(dat[[fc.col]]))) else rep(NA_real_, nrow(dat));
  p.vec  <- if(!is.na(p.col))  suppressWarnings(as.numeric(as.character(dat[[p.col]])))  else rep(NA_real_, nrow(dat));

  # A p-value of exactly 0 (common after rounding in a vendor export) scores as
  # Inf on the -log10 scale, dwarfing every real compound and making it the sole
  # "leading edge" hit of every pathway it belongs to. Clamp it to the smallest
  # OTHER nonzero p-value in the list rather than let one rounded value dominate.
  if(any(p.vec == 0, na.rm=TRUE)){
    finite.p <- p.vec[!is.na(p.vec) & p.vec > 0];
    floor.p <- if(length(finite.p)) min(finite.p) else .Machine$double.eps;
    p.vec[!is.na(p.vec) & p.vec == 0] <- floor.p;
  }

  score <- switch(rankMetric,
    fc       = fc.vec,
    p        = -log10(p.vec),
    signed_p = sign(fc.vec) * -log10(p.vec)
  );

  valid <- !is.na(cmpd.nms) & nzchar(cmpd.nms) & is.finite(score);
  dup <- duplicated(cmpd.nms) & valid;
  keep <- valid & !dup;
  if(sum(keep) < 10){
    AddErrMsg("Too few rows with both a compound name and a finite rank score (need at least 10)!");
    return(0);
  }
  cmpd.nms <- cmpd.nms[keep];
  score    <- score[keep];

  # InitDataObjects/CrossReferencing return a literal 1/0 status code ONLY under
  # .on.public.web=TRUE (the live app's calling convention); when this package is
  # loaded plainly (library(MetaboAnalystR) -- testthat, or any non-web embedding),
  # .on.public.web defaults FALSE and BOTH functions instead return the mSetObj
  # object directly (their own final `return(.set.mSet(mSetObj))` resolves that way
  # in FALSE mode). Branch exactly like CrossReferencing itself already does
  # internally, so this works correctly under either convention rather than only
  # the live app's.
  mSetObj0 <- InitDataObjects("list", "msetora", FALSE);
  if(.on.public.web){
    if(!isTRUE(mSetObj0 == 1)){
      AddErrMsg("Failed to initialize the analysis session!");
      return(0);
    }
    mSetObj <- .get.mSet(NA);
  }else{
    mSetObj <- mSetObj0;
  }

  mSetObj$dataSet$cmpd <- cmpd.nms;
  .set.mSet(mSetObj);   # TRUE mode: publishes to the session global for CrossReferencing
                        # to read next; FALSE mode: no-op, mSetObj is already current.

  rc2 <- CrossReferencing(mSetObj, "name");
  if(.on.public.web){
    if(!isTRUE(rc2 == 1)){
      AddErrMsg("Compound name matching failed - see the name-check message for details!");
      return(0);
    }
    mSetObj <- .get.mSet(NA);
  }else{
    mSetObj <- rc2;
  }

  return(Setup.CmpdRankScore(mSetObj, score));
}




