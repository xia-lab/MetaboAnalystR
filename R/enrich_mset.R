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
#'@usage SetCurrentMsetLib(mSetObj=NA, lib.type, excludeNum)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param lib.type Input user selected name of library, "self", "kegg_pathway",
#'"smpdb_pathway", "blood", "urine", "csf", "snp", "predicted", "location", and "drug".
#'@param excludeNum Users input the mimimum number compounds within selected metabolite sets (metabolitesets < excludeNum)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
SetCurrentMsetLib <- function(mSetObj=NA, libname, excludeNum=0){

  mSetObj <- .get.mSet(mSetObj);
  
  if(libname=="self"){
    ms.list <- mSetObj$dataSet$user.mset;
    ms.list <- lapply(ms.list, function(x) unlist(strsplit(x, "; ", fixed=TRUE)))
    current.msetlib <- vector("list", 3)
    names(current.msetlib) <- c("name", "member", "reference")
  }else{
    
    if(!.on.public.web & grepl("kegg", libname)){ # api only for KEGG msets
      mSetObj$api$libname <- libname
      mSetObj$analSet$msetlibname <- libname;
      mSetObj$api$excludeNum = excludeNum
      return(.set.mSet(mSetObj));
    }
    
    if(!exists("current.msetlib") || mSetObj$analSet$msetlibname != libname) {
        destfile <- paste(libname, ".qs", sep = "");
        if(.on.public.web){
            my.qs  <- paste("../../libs/msets/", destfile, sep="");
            current.msetlib <- qs::qread(my.qs);
        }else{
            my.qs <- paste("https://www.metaboanalyst.ca/resources/libs/msets/", destfile, sep="");
            if(!file.exists(destfile)){
                download.file(my.qs, destfile);
            }
            current.msetlib <- qs::qread(destfile);
        }
        mSetObj$analSet$msetlibname <- libname;
    }
    # create a named list, use the ids for list names
    ms.list <- strsplit(current.msetlib[,3],"; ", fixed=TRUE);
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
      conc.db <<-  .readDataTable("../../libs/cmpd_conc.csv");
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
#'@param kegg.rda Input the name of the KEGG library
#'@param lib.version Input the KEGG pathway version. "current" for the latest 
#'KEGG pathway library or "v2018" for the KEGG pathway library version prior to November 2019. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetKEGG.PathLib<-function(mSetObj=NA, libNm, lib.version){
  
  mSetObj <- .get.mSet(mSetObj);
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
    load_igraph();
  }
  
  mSetObj$pathwaylibtype <- "KEGG"
  return(.set.mSet(mSetObj));
}

#'Set SMPDB pathway library
#'@description note, this process can be long, need to return a value
#'to force Java to wait
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param smpdb.rda Input the name of the SMPDB library (e.g. hsa or mmu)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetSMPDB.PathLib<-function(mSetObj=NA, libNm){
  
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$msgSet$lib.msg <- paste("Your selected pathway library code is \\textbf{", libNm, "}(KEGG organisms abbreviation).");

    destfile <- paste0(libNm, ".qs");
    current.kegglib <<- .get.my.lib(destfile, "smpdb");
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
    print(libCheck.msg);
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
  hits <- tolower(ref.vec)%in%tolower(cmpd.db$kegg_id);
  mSetObj$dataSet$metabo.filter.kegg <- ref.vec[hits];
  unmatched.num <- sum(!hits);
  
  if(unmatched.num > 0) {
    unmatched.nms <- ref.vec[!hits];
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", unmatched.num, "compounds were not matched to KEGG compound IDs.",
                                             "They are:", paste(unmatched.nms, collapse="; "), ". Please correct these names. Otherwise,",
                                             "they will be ignored during the enrichment analysis.");
  }else{
    mSetObj$dataSet$metabo.ref.info <- paste("A total of", length(ref.vec), "were sucessfully added to the library.");
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
  mSetObj$dataSet$metabo.filter.hmdb <- ref.vec[hits];
  unmatched.num <- sum(!hits);
  
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
  return(current.msetlib$name);
}

GetMsetMembers<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(current.msetlib$member);
}

GetMsetReferences<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
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

getBestHit<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$lib.search$best.hit);
}


#'Return metset search results
#'@description since String[][] is not supported, have to return as 1D vector, 
#'matrix can be directly convert to vector, note default will be column first
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetMsetLibSearchResult<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(as.vector(mSetObj$dataSet$lib.search$matched.table));
}

GetSMPDBimg<-function(mSetObj=NA, msetInx){
  mSetObj <- .get.mSet(mSetObj);
  msetNm <- GetMetSetName(msetInx);
  inx <- which(current.msetlib$name == msetNm);
  return(current.msetlib$image[inx]);
}