##################################################
## R script for ExpressAnalyst
## Description: functions for id annotation onedata and metadata
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

# read in the data and perform
# gene ID mapping using built in libraries
# matchMin is minimal matched probe (%)
# idType: INVEX supported ID types
# lvlOpt: "NA" to keep original, other values will merge original ID to entrez gene IDs

# return the total matched gene number
# note: unmapped IDs will be retained as 
# original label (i.e. intergenic regions) in further analysis
#'Perform data annotation
#'@description Read data and perform gene ID mapping using built in databases
#'@param dataSetObj Input the name of the created datasetObj (see Init.Data).
#'@param org three letters annotation of organism (i.e hsa, mmu)
#'@param dataType Either "array" (microarray) or "count" (rna-seq)
#'@param idType original id type of genes
#'@param lvlOpt merging original ID to entrez gene IDs. "NA" to keep original IDs without merging
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PerformDataAnnot <- function(dataName="", org="hsa", dataType="array", idType="entrez", lvlOpt="mean"){
  #dataName <<- dataName;
  #org <<- org;
  #dataType <<- dataType;
  #idType <<- idType;
  #lvlOpt <<- lvlOpt;
  #save.image("dataannot.RData");
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  current.msg <- "";
  
  if(org == "custom"){
    idType <- "custom";
  }
  paramSet$data.org <- org;
  paramSet$data.idType <- idType;
  
  dataSet$type <- dataType;
  dataSet$id.orig <- dataSet$id.current <- idType;
  dataSet$annotated <- F;
  # should not contain duplicates, however sanity check
  if(dataType=="prot"){
    data.proc <- qs::qread("int.mat.qs");
  }else{
    data.proc <- qs::qread("data.raw.qs");
  }
  dataSet$data.anot <- data.proc;
  
  if (org != 'NA' & idType != 'NA'){
    feature.vec <- rownames(data.proc);
    
    anot.id <- .doAnnotation(feature.vec, idType, paramSet);
    anot.id <- unname(anot.id);
    if(idType %in% c("s2f", "generic", "ko")){
      symbol.map <- .doGeneIDMapping(anot.id, idType, paramSet, "matrix");
    }else{
      symbol.map <- .doGeneIDMapping(anot.id, "entrez", paramSet, "matrix");
    }
    symbol.map <- symbol.map[which(symbol.map$gene_id %in% anot.id),];
    
    saveDataQs(symbol.map, "symbol.map.qs", paramSet$anal.type, dataName);
    
    qs::qsave(anot.id, "annotation.qs");
    
    hit.inx <- !is.na(anot.id);
    matched.len <- sum(hit.inx);
    perct <- round(matched.len/length(feature.vec),3)*100;
    thresh <- 0.1 # previous value of 0.25 is causing challenges 
    #for datasets like Ppromelas with low annotation quality
    if (matched.len < length(feature.vec)*thresh){
      current.msg <- paste('Only ', perct, '% ID were matched. You may want to choose another ID type or use default.', sep=""); 
    } else {
      current.msg <- paste("ID annotation: ", "Total [", length(anot.id), 
                           "] Matched [", matched.len, "] Unmatched [", sum(!hit.inx),"]", collapse="\n");    
      
      if (lvlOpt != 'NA' | idType == "entrez"){
        # do actual summarization to gene level
        matched.entrez <- anot.id[hit.inx];
        data.anot <- data.proc[hit.inx,];
        rownames(data.anot) <- matched.entrez;
        current.msg <- paste(current.msg, "Data is now transformed to gene-level (Entrez) expression.");
        paramSet$lvl.opt <- lvlOpt;
        res <- RemoveDuplicates(data.anot, lvlOpt, quiet=F, paramSet, msgSet);
        dataSet$data.anot <- res[[1]];
        msgSet <- res[[2]];
        dataSet$id.current <- "entrez";
        dataSet$annotated <- T; 
      } else {
        current.msg <- paste(current.msg, "No gene level summarization was performed.");
      }
    }
  } else { # no conversion will be performed
    feature.vec <- rownames(data.proc);
    anot.id <- feature.vec
    perct <- 100;
    hit.inx <- !is.na(anot.id);
    matched.len <- length(feature.vec); # dummies
    minLvl <- 1;
    current.msg <- paste("No annotation was performed. Make sure organism and gene ID are specified correctly!"); 
  }
  # need to save the ids (mixed gene annotation and original id) 
  # in case, users needs to keep unannotated features
  # this need to be updated to gether with data from now on
  dataSet$data.norm <- dataSet$data.anot;
  
  qs::qsave(dataSet$data.anot, file="orig.data.anot.qs"); # keep original copy, not in mem
  
  totalCount <-  sum(colSums(dataSet$data.anot));
  avgCount <- sum(colSums(dataSet$data.anot))/ ncol(dataSet$data.anot);
  minCount <- min(colSums(dataSet$data.anot))
  maxCount <- max(colSums(dataSet$data.anot))
  lvls = ""
  if(any(dataSet$disc.inx.orig)){
    disc = paste(names(dataSet$disc.inx.orig)[which(dataSet$disc.inx.orig)],collapse = ", ")
    lvls = paste0(lvls,length(which(dataSet$disc.inx.orig))," discrete factors: ",disc,"; ")
  }
  if(any(dataSet$cont.inx.orig)){
    cont = paste(names(dataSet$cont.inx.orig)[which(dataSet$cont.inx.orig)],collapse = ", ")
    lvls = paste0(lvls,length(which(dataSet$cont.inx.orig))," continuous factors: ",cont,".")
  }
  missNum = which(is.na(dataSet$data.anot)|dataSet$data.anot=="NA"|dataSet$data.anot=="")
  msgSet$current.msg <- current.msg;
  msgSet$summaryVec <- c(matched.len, perct, length(anot.id), sum(!hit.inx), ncol(dataSet$data.anot), ncol(dataSet$meta.info), sprintf("%4.2e", signif(totalCount ,3)), sprintf("%4.2e",signif(avgCount, 3)), sprintf("%4.2e",signif(minCount, 3)), sprintf("%4.2e",signif(maxCount,3)), lvls,length(missNum))  
  if(length(missNum)>0){
    RemoveMissingPercent(dataSet$name, 0.5)
    ImputeMissingVar(dataSet$name, method="min")
  }else{
    qs::qsave(dataSet$data.anot, file="data.missed.qs");
  }

  fast.write(dataSet$data.anot, file="data_annotated.csv");

  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet, matched.len));   
}


# Annotating genes to internal database
AnnotateGeneData <- function(dataName, org, lvlOpt, idtype){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(dataName);
  
  if(org == "NA"){
    msgSet$current.msg <- "Invalid organism!"
    saveSet(msgSet, "msgSet");
    return(1)
  }
  
  data.raw <- readDataQs("data.raw.qs", paramSet$anal.type, dataName);
  gene.vec <- rownames(data.raw);
  
  #record the info
  paramSet$data.org <- org
  dataSet$q.type.gene <- idtype;
  
  dataSet$gene.org <- org;
  dataSet$gene <- gene.vec;
  if(idtype == "NA"){
    enIDs <- gene.vec;
    symbol.map <- data.frame(gene_id=enIDs, symbol=enIDs, name=enIDs);
  }else{
    if(idtype %in% c("entrez", "symbol", "refseq", "gb", "embl_gene","embl_protein", "embl_transcript", "orf", "tair", "wormbase", "ko", "custom", "s2f")){
      enIDs <- .doGeneIDMapping(gene.vec, idtype, paramSet, "vec");
    }else{
      enIDs <- .doProbeMapping(gene.vec, idtype, paramSet);
      names(enIDs) <- gene.vec;
    }
  
  
  tblNm <- getEntrezTableName(org, "entrez");
  symbol.map <- queryGeneDB(tblNm, org);
  symbol.map <- symbol.map[which(symbol.map$gene_id %in% enIDs),];
  }
  saveDataQs(symbol.map, "symbol.map.qs", paramSet$anal.type, dataName);

  
  if(idtype == "kos"){
    kos <- enIDs$kos;
    enIDs <- enIDs$entrezs;
    dataSet$kos.name.map <- kos
  }
  
  # Handle case when only KOs are mapped with no corresponding entrez id
  na.inx <- is.na(enIDs);
  
  if(sum(!na.inx) == 0 && idtype == "kos"){
    na.inx <- is.na(kos);
  }
  
  dataSet$gene.name.map <- list(
    hit.values=enIDs,
    match.state = ifelse(is.na(enIDs), 0, 1)
  );
  
  hit.inx <- which(!is.na(enIDs));
  matched.len <- length(hit.inx);
  if(matched.len > 1){
    data.norm <- data.raw[hit.inx,];
    matched.entrez <- enIDs[hit.inx];
    
    
    # now, deal with duplicated entrez id
    # first, average duplicate rows
    dataSet$lvl.opt <- lvlOpt;
    res <- RemoveDuplicates(data.norm, lvlOpt, quiet=F, paramSet, msgSet);
    int.mat <- res[[1]];
    msgSet <- res[[2]];
    # update
    
    data.annotated <-int.mat;
    rownames(int.mat) <- matched.entrez
    if(idtype %in% c("mir_id", "mir_acc", "mirnet")){
      rownames(data.annotated) <- rownames(int.mat);
    }else{
      rownames(data.annotated) <- matched.entrez
    }
    dataSet$enrich_ids = rownames(int.mat);
    names(dataSet$enrich_ids) = doEntrez2SymbolMapping(rownames(int.mat), paramSet$data.org, paramSet$data.idType)
    
    dataSet$id.type <- "entrez";
    
  }else{
    data.annotated <- data.raw;
    dataSet$enrich_ids = rownames(data.annotated)
    dataSet$id.type <- "none";
  }
  
  if(idtype != "NA"){
    if(length(unique(enIDs))/length(gene.vec) < 0.3){
      msg <- paste("Less than ", round( length(unique(enIDs))/length(gene.vec) * 100, 2), "% features were mapped");
      msgSet$current.msg <- msg;
      saveSet(msgSet, "msgSet");
      return(0)
    }else{
      msg <- paste("A total of ", length(unique(enIDs)), "unique features were mapped");
    }
  }else{
    msg <- paste("There is a total of ", length(unique(gene.vec)), "unique features.");
  }
  
  saveDataQs(data.annotated, "data.annotated.qs", paramSet$anal.type, dataName);
  msgSet$current.msg <- msg;
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  return(RegisterData(dataSet));
}

#Convert a vector of ids to vector of entrez ids
.doAnnotation <- function(feature.vec, idType, paramSet){
  if(idType %in% c("entrez", "symbol", "refseq", "gb", "embl_gene","embl_protein","uniprot", "embl_transcript", "orf", "tair", "wormbase", "ko", "custom", "cds", "s2f", "string")){
    anot.id <- .doGeneIDMapping(feature.vec, idType, paramSet, "vec");
  }else{
    anot.id <- .doProbeMapping(feature.vec, idType, paramSet);
    names(anot.id) <- feature.vec;
  }
  return(anot.id);        
}


# mapping between genebank, refseq and entrez
.doGeneIDMapping <- function(feature.vec, idType, paramSet, outputType="vec"){
    org <- paramSet$data.org;
  if(is.null(feature.vec)){
    db.map <-  queryGeneDB("entrez", org);
    feature.vec <- db.map[, "gene_id"];
    idType <-"entrez";
    print(".doGeneIDMapping, empty feature.vec, get whole table");
  }
  col.nm = "";
  db.nm = "";
  
  if(idType %in% c("s2f", "ko") || paramSet$data.idType %in% c("s2f", "ko")){ # only for ko
    col.nm = "gene_id";
    db.nm = paste0("entrez_", idType);
  }else if(idType == "symbol"){
    col.nm = "symbol";
    db.nm = "entrez";
  }else if(idType == "entrez"){
    col.nm = "gene_id";
    db.nm = "entrez";
  }else if(idType == "custom"){ # only for ko
    db.nm = "custom";
    col.nm = "gene_id";
  }else if(idType == "cds"){
    col.nm = "accession";
    db.nm = "entrez";
    db.map <- queryGeneDB(paste0("entrez_",idType), org);
  }else{
    # note, some ID can have version number which is not in the database
    # need to strip it off NM_001402.5 => NM_001402
    if(!(idType == "refseq" && org == "fcd") && !(idType == "string" && org == "cel") ){ # do not strip, database contains version number.
      q.mat <- do.call(rbind, strsplit(feature.vec, "\\."));
      feature.vec <- q.mat[,1];
    }
    col.nm = "accession";
    if(idType == "tair"){ # only for ath
      db.nm = "tair";
    }else { 
      # if(idType %in% c("gb", "refseq", "embl_gene", "embl_transcript", "embl_protein", "orf", "wormbase", "string")){
      db.nm <- paste0("entrez_", idType);
    }
  }
  
  db.map <-  queryGeneDB(db.nm, org);
  if(org == "smm" && idType == "symbol"){
    q.mat <- do.call(rbind, strsplit(feature.vec, "\\."));
    feature.vec <- q.mat[,1];
    q.mat <- do.call(rbind, strsplit(db.map[, col.nm], "\\."));
    db.map[, col.nm] <- q.mat[,1];
  }
  
  hit.inx <- match(feature.vec, db.map[, col.nm]);
  
  if(outputType == "vec"){
    entrezs <- db.map[hit.inx, "gene_id"];
    
    mode(entrezs) <- "character";
    rm(db.map, feature.vec); gc();
    return(entrezs);
  }else{
    entrezs <- db.map[hit.inx, ]; 
    if(idType == "entrez"){
      entrezs <- entrezs[,c(2,1)];
    }else{
      entrezs <- entrezs[,c(2,1)];
    }
    na.inx <- is.na(entrezs[,1]);
    entrezs[,1][na.inx] <- feature.vec[na.inx];
    na.inx <- is.na(entrezs[,2]);
    entrezs[,2][na.inx] <- feature.vec[na.inx];
    colnames(entrezs) <- c("accession", "gene_id")
    return(entrezs);
  }
}


# from probe ID to entrez ID 
.doProbeMapping <- function(probe.vec, platform, paramSet){
  org <- paramSet$data.org;

  if(exists("api.lib.path")){
    lib.path <- api.lib.path;
  }else{
    lib.path <- paramSet$lib.path;
  }
  platform.path <- paste(lib.path, org, "/", platform, ".rds", sep="");

  if(!paramSet$on.public.web && !file.exists(platform.path)){
    nmdb <- basename(platform.path);
    download.file(platform.path, destfile = nmdb, method="libcurl", mode = "wb");
    platform.path <- nmdb;
  }
  

  probe.map <- readRDS(platform.path);
  if(is.null(probe.vec)){
    entrez <- probe.map[, "entrez"];
  }else{
    hit.inx <- match(probe.vec, probe.map[, "probe"]);
    entrez <- probe.map[hit.inx, "entrez"];
  }
  return(entrez);
}


queryGeneDB <- function(db.nm, org){
  if(org == "na"){
      print("Not available when organism is not specified");
      return(0);
  }
  paramSet <- readSet(paramSet, "paramSet");    
  if(db.nm == "custom" || org == "custom"){
    db.map <- qs::qread("anot_table.qs");
  }else{
    require('RSQLite');
    
    db.path <- paste(paramSet$sqlite.path, org, "_genes.sqlite", sep="")
    if(!PrepareSqliteDB(db.path, paramSet$on.public.web)){
      stop("Sqlite database is missing, please check your internet connection!");
    }
    conv.db <- dbConnect(SQLite(), db.path); 
    tbls <- dbListTables(conv.db)
    if(!db.nm %in% tbls){
        return(0);
    }
    db.map <- dbReadTable(conv.db, db.nm);
    dbDisconnect(conv.db); cleanMem();
  }
  
  return(db.map)
}

getEntrezTableName <- function(data.org, data.idType){
    if(data.org == "generic"){
        tblNm <- paste0("entrez_", data.idType);
    }else{
        tblNm <- "entrez";
    }
    return(tblNm);
}


doEntrez2SymbolMapping <- function(entrez.vec,data.org="NA", data.idType="NA"){

  if(data.org == "NA" && data.idType=="NA"){
    paramSet <- readSet(paramSet, "paramSet");
    data.org <- paramSet$data.org;
    data.idType <- paramSet$data.idType;
  }

  if(data.org == "na"){
    return(entrez.vec); # nothing to do
  }
  tblNm <- getEntrezTableName(data.org, data.idType);
  gene.map <-  queryGeneDB(tblNm, data.org);
  gene.map[] <- lapply(gene.map, as.character)
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  symbols <- gene.map[hit.inx, "symbol"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
}

# note, entrez.vec could contain NA/null, cannot use rownames
doEntrezIDAnot <- function(entrez.vec,data.org="hsa", data.idType){
  tblNm <- getEntrezTableName(data.org, data.idType);
  gene.map <-  queryGeneDB(tblNm, data.org);
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  anot.mat <- gene.map[hit.inx, c("gene_id", "symbol", "name")];
  
  na.inx <- is.na(hit.inx);
  anot.mat[na.inx, "symbol"] <- entrez.vec[na.inx];
  anot.mat[na.inx, "name"] <- 'NA';
  return(anot.mat);
}

doIdMappingGeneric <- function(orig.vec, gene.map, colNm1, colNm2, type="vec"){
  
  hit.inx <- match(orig.vec, gene.map[, colNm1]);
  if(colNm2 =="symbol"){
    if(!colNm2 %in% colnames(gene.map)){
        colnames(gene.map)[which(colnames(gene.map) == "accession")] <- "symbol";
    }
  }
  if(type == "vec"){
  result.vec <- gene.map[hit.inx, colNm2];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(result.vec);
  result.vec[na.inx] <- orig.vec[na.inx];
  return(result.vec);
  }else{
  na.inx <- is.na(hit.inx);
  anot.mat <- gene.map[hit.inx,];
  anot.mat[na.inx, "symbol"] <- orig.vec[na.inx];
  anot.mat[na.inx, "name"] <- orig.vec[na.inx];
  return(anot.mat);
  }
}

##########################################
############# private utility methods #### 
##########################################

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}