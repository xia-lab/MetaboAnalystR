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

# batch remove based on
UpdateEntries <- function(mSetObj=NA, col.id, method, value, action) {
  mSetObj <- .get.mSet(mSetObj);
  
  if(col.id == "name"){
    col <- mSetObj$dataSet$snp.res$Metabolite;
  }else if(col.id == "rsid"){
    col <- mSetObj$dataSet$snp.res$rsID;
  }else if(col.id == "chr"){
    col <- mSetObj$dataSet$snp.res$Chr;
  }else if(col.id == "p_value"){
    col <- mSetObj$dataSet$snp.res$`P-value`
  }else if(col.id == "consequence"){
    col <- mSetObj$dataSet$snp.res$Consequence;
  }else if(col.id == "symbol"){
    col <- mSetObj$dataSet$snp.res$Gene;
  }else if(col.id == "pmid"){
    col <- mSetObj$dataSet$snp.res$PMID;
  } else {
    print(paste("unknown column:", col.id));
  }
  
  if(method == "contain"){
    hits <- grepl(value, col, ignore.case = TRUE);
  }else if(method == "match"){
    hits <- tolower(col) %in% tolower(value);
  }else{ # at least
    if( col.id == "p_value"){
      col.val <- as.numeric(col);
      na.inx <- is.na(col.val);
      col.val[na.inx] <- max(col.val[!na.inx]);
      hits <- col.val > as.numeric(value);
    } else {
      print("This is only for P-value at this moment.");
      return("NA");
    }
  }
  
  if(action == "keep"){
    hits = !hits;
  }
  
  if(sum(hits) > 0){
    row.ids <- rownames(mSetObj$dataSet$snp.res)[hits];
    mSetObj$dataSet$snp.res <- mSetObj$dataSet$snp.res[!hits,];
    fast.write.csv(mSetObj$dataSet$snp.res, file="mgwas_snp_met.csv", row.names=FALSE);
    #.prepareIdeogramJSON(mSetObj$dataSet$snp.res);
    .set.mSet(mSetObj);
    return(row.ids);
  }else{
    return("NA");
  }
}

GetResRowNames <- function(netType){
  mSetObj <- .get.mSet(mSetObj);
  save.image("rownames.RData");
  analSet <- mSetObj$analSet$type;
   #netType<<-netType
   #save.image("GetResRowNames.RData")
  if(netType == "manhattan" || analSet == "studyview"){
    resTable <- mSetObj$dataSet$snp2met_study; 
  }else if (analSet == "search"){
    resTable <- mSetObj$dataSet$snp2met_single;
  }else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
   resTable <- mSetObj$dataSet[netType][[1]]
  }  else{
    resTable <- dataSet$mir.res;

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


PerformCmpdMapping <- function(mSetObj=NA, cmpdIDs, idType, tissueType, population){
  
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

UpdateMREntries <- function(mSetObj=NA, col.id, method, value, action) {
  #col.id<<-col.id;
  #method<<-method;
  #value<<-value;
  #action<<-action;
  #save.image("UpdateMREntries.RData")
  
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(col.id == "name"){
    col <- mSetObj$dataSet$exposure$Metabolite;
  }else if(col.id == "rsid"){
    col <- mSetObj$dataSet$exposure$SNP;
  }else if(col.id == "chr"){
    col <- mSetObj$dataSet$exposure$Chr;
  }else if(col.id == "beta"){
    col <- mSetObj$dataSet$exposure$Beta
  }else if(col.id == "se"){
    col <- mSetObj$dataSet$exposure$SE;
  }else if(col.id == "pval"){
    col <- mSetObj$dataSet$exposure$`P-value`;
  }else if(col.id == "pmid"){
    col <- mSetObj$dataSet$exposure$PMID;
  } else {
    print(paste("unknown column:", col.id));
  }
  
  if(method == "contain"){
    hits <- grepl(value, col, ignore.case = TRUE);
  }else if(method == "match"){
    hits <- tolower(col) %in% tolower(value);
  }else{ # at least
    if( col.id == "pval"){
      col.val <- as.numeric(col);
      na.inx <- is.na(col.val);
      col.val[na.inx] <- max(col.val[!na.inx]);
      hits <- col.val > as.numeric(value);
    } else {
      print("This is only for P-value at this moment.");
      return("NA");
    }
  }
  
  if(action == "keep"){
    hits = !hits;
  }
  
  if(sum(hits) > 0){
    row.ids <- rownames(mSetObj$dataSet$exposure)[hits];
    mSetObj$dataSet$exposure <- mSetObj$dataSet$exposure[!hits,];
    fast.write.csv(mSetObj$dataSet$exposure, file="mr_exposure_data.csv", row.names=FALSE);
    .set.mSet(mSetObj);
    return(row.ids);
  }else{
    return("NA");
  }
}

