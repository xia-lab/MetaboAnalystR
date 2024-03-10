##################################################
## R script for mGWAS
## Description: Data/resource management functions
## Author: Le, le.chang@mail.mcgill.ca
###################################################


SetupListData <- function(mSetObj=NA, mirs, idType, tissue, population){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$idType <- idType;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;

  mir.mat <- mir.mat[,-1, drop=F];
  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

SetupIndListData <- function(mSetObj=NA, mirs, inputType, idType, tissue, population){

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;
  
  mir.mat <- mir.mat[,-1, drop=F];
  if(is.null(mSetObj$dataSet$data)){
    mSetObj$dataSet$data <- mSetObj$dataSet$id.types <- list();
  }
  mSetObj$dataSet$data[[inputType]] <- mir.mat;
  mSetObj$dataSet$id.types[[inputType]] <- idType;

  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

# "ID", "Accession","Gene", "PMID"
GetResCol <- function(netType, colInx){
   #netType<<-netType;
   #colInx<<-colInx;
   #save.image("GetMirResCol.RData");
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;
  dataSet <- mSetObj$dataSet;
  if(netType == "manhattan" || analSet == "studyview" || analSet == "search"){
    res <- mSetObj$dataSet$snp2met[, colInx];
  } else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
    # mr_results_merge
    res <- dataSet[netType][[1]][, colInx];
  } else{
    res <- dataSet$mir.res[, colInx];
  }
    hit.inx <- is.na(res) | res == ""; # note, must use | for element-wise operation
    res[hit.inx] <- "N/A";
    return(res);
}

# "ID", "Accession","Gene", "PMID"
GetResColByName <- function(netType, name){
   #netType<<-netType;
   #colInx<<-colInx;
   #save.image("GetMirResCol.RData");
   mSetObj <- .get.mSet(mSetObj);
   analSet <- mSetObj$analSet$type;
   dataSet <- mSetObj$dataSet;
  if(netType == "manhattan" || analSet == "studyview" || analSet == "search"){
    df <-mSetObj$dataSet$snp2met;
  } else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
    # mr_results_merge
    df <-dataSet[netType][[1]];
  } else{
    df <- dataSet$mir.res;
  }
    colInx <- which(colnames(df) == name);
    res <- df[, colInx];

    hit.inx <- is.na(res) | res == ""; # note, must use | for element-wise operation
    res[hit.inx] <- "N/A";
    return(res);
}

RemoveEntryExposure <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$exposure) == mir.id);
  if(length(inx) > 0){
    if(is.null(mSetObj$dataSet$exposure.orig)){
        mSetObj$dataSet$exposure.orig <- mSetObj$dataSet$exposure;
    }
    mSetObj$dataSet$exposure <- dataSet$exposure[-inx,];
    if(!is.null(mSetObj$dataSet$harmonized.dat)){
        inx <- which(rownames(mSetObj$dataSet$harmonized.dat) %in% rownames(mSetObj$dataSet$exposure));
        mSetObj$dataSet$harmonized.dat <- mSetObj$dataSet$harmonized.dat[inx,];
    }
  }

  return(.set.mSet(mSetObj));
}

AddEntryExposure <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$exposure.orig) == mir.id);
  if(length(inx) > 0){
    mSetObj$dataSet$exposure <- rbind(dataSet$exposure, dataSet$exposure.orig[inx,]);
  }
  return(.set.mSet(mSetObj));
}


GetResRowNames <- function(netType){
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;

  if(netType == "manhattan" || analSet == "studyview"){
    resTable <- mSetObj$dataSet$snp2met_study; 
  }else if (analSet == "search"){
    resTable <- mSetObj$dataSet$snp2met_single;
  }else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
   resTable <- mSetObj$dataSet[netType][[1]]
  }  else{
    resTable <- mSetObj$dataSet$mir.res;
  }

  if(nrow(resTable) > 1000 & netType != "phe_mr_sig"){
    resTable <- resTable[1:1000, ];
    current.msg <<- "Due to computational constraints, only the top 1000 rows will be displayed.";
  }
  rownames(resTable);
}


# Load RSQLite, necessary for network analysis
load_rsqlite <- function(){
  suppressMessages(library(RSQLite))
}

.get.sqlite.con <- function(sqlite.path){
  load_rsqlite();
  return(dbConnect(SQLite(), sqlite.path)); 
}

# private method for all sqlite queries
.query.sqlite <- function(db.con, statement, offline=TRUE){
  rs <- dbSendQuery(db.con, statement);
  res <- fetch(rs, n=-1); # get all records
  dbClearResult(rs);
  if(offline){
    dbDisconnect(db.con);
  }
  cleanMem();
  return(res);
}

doGeneIDMapping <- function(q.vec, org, type){
  #save.image("doGeneIDMapping.RData")
  
  sqlite.path <- paste0(url.pre, org, "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  
  if(type == "symbol"){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "symbol"]);
  }else if(type == "entrez" || type == "entrez2sbl" ){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
  }else{
    if(type == "genbank"){
      db.map = dbReadTable(con, "entrez_gb");
    }else if(type == "embl"){
      db.map = dbReadTable(con, "entrez_embl_gene");
    }else if(type == "refseq"){
      db.map = dbReadTable(con, "entrez_refseq");
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
    }else if(type == "kos"){
      db.map = dbReadTable(con, "entrez_ortholog");
    }
    hit.inx <- match(q.vec, db.map[, "accession"]);
  }
  
  if(type=="entrez2sbl"){
    entrezs=db.map[hit.inx, "symbol"];
  }else{
    entrezs=db.map[hit.inx, "gene_id"];
  }
  
  rm(db.map, q.vec); gc();
  dbDisconnect(con);
  return(entrezs);
}

addEntrezID <- function(data){
  #save.image("addEntrezID.RData")
  
  sqlite.path <- paste0(url.pre, "hsa", "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  embl <- data;
  
  db.map = dbReadTable(con, "entrez_embl_gene");
  all.hits <- db.map$accession %in% embl$ensembl;
  my.map <- db.map[all.hits, ];
  dat <- merge(my.map, data, by.x="accession", by.y="ensembl");

    rm(db.map, embl); gc();
  dbDisconnect(con);
  return(dat);
}


PerformCmpdMapping2 <- function(mSetObj=NA, cmpdIDs, idType, tissueType, population){
  
  mSetObj <- .get.mSet(mSetObj);
  org <- "hsa";
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
  mSetObj$dataSet$tissue <- tissueType;
  mSetObj$dataSet$population <- population;
  mSetObj$dataSet$idType <- idType;
  current.msg <<- NULL;  
  if(idType == "name"){
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "tab");
  }else{ # all other id should not contains space
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "space");
  }
  mSetObj$dataSet$cmpd <- rownames(cmpd.mat); # this is for compatibility with name_match function
  mSetObj$dataSet$cmpd.mat <- cmpd.mat;
  mSetObj$dataSet$mir.orig <- cmpd.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(cmpd.mat));
  }else{
    return (paste("A total of",  nrow(cmpd.mat), "unique items were entered."))
  }
}

SetMappingType <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mapType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Mapping options were entered!"))
  }
}

SetDbOpt <- function(){
  #save.image("SetDbOpt.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$dbOpt <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Database options were entered!"))
  }
}


SetMRMethod <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$methodType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("MR method options were entered!"))
  }
}


