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
#'License: GNU GPL (>= 2)
#'@export
#'
PerformDataAnnot <- function(dataName="", org="hsa", dataType="array", idType="entrez", lvlOpt="mean"){
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
  data.proc <- qs::qread("data.proc.qs");
  dataSet$data.anot <- data.proc;
  
  
  if (org != 'NA' & idType != 'NA'){
    feature.vec <- rownames(data.proc);
    
    anot.id <- .doAnnotation(feature.vec, idType, paramSet);
    anot.id <- unname(anot.id);
    
    symbol.map <- .doGeneIDMapping(anot.id, "entrez", paramSet, "matrix");
    dataSet$symbol.map <- symbol.map;
    
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
  
  if(length(dataSet$meta)==1){
    lvls <- paste(levels(dataSet$meta[,1]),collapse="; ")
  }else{
    conc1 <- paste0("<b>", colnames(dataSet$meta)[1], "</b>", ": ", paste(levels(dataSet$meta[,1]), collapse="; "))
    conc2 <- paste0("<b>", colnames(dataSet$meta)[2], "</b>", ": ", paste(levels(dataSet$meta[,2]), collapse="; "))
    lvls <- paste("Two factors found -", conc1, conc2)
  }
  msgSet$current.msg <- current.msg;
  print(current.msg);
  msgSet$summaryVec <- c(matched.len, perct, length(anot.id), sum(!hit.inx), ncol(dataSet$data.anot), ncol(dataSet$meta), sprintf("%4.2e", signif(totalCount ,3)), sprintf("%4.2e",signif(avgCount, 3)), sprintf("%4.2e",signif(minCount, 3)), sprintf("%4.2e",signif(maxCount,3)), lvls)  
  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet, matched.len));   
}

# Annotating genes to internal database
AnnotateGeneData <- function(dataName, org, idtype){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(dataName);
  
  if(org == "NA"){
    msgSet$current.msg <- "Invalid organism!"
    saveSet(msgSet, "msgSet");
    return(1)
  }
  
  data <- dataSet$data.raw;
  gene.vec <- rownames(data);
  
  #record the info
  paramSet$data.org <- org
  dataSet$q.type.gene <- idtype;
  dataSet$gene.org <- org;
  dataSet$gene <- gene.vec;
  if(idtype %in% c("entrez", "symbol", "refseq", "gb", "embl_gene","embl_protein", "embl_transcript", "orf", "tair", "wormbase", "ko", "custom", "s2f")){
    enIDs <- .doGeneIDMapping(gene.vec, idtype, paramSet, "vec");
  }else{
    enIDs <- .doProbeMapping(gene.vec, idtype, paramSet);
    names(enIDs) <- gene.vec;
  }

  tblNm <- getEntrezTableName(org, "entrez");
  symbol.map <- queryGeneDB(tblNm, org);
  symbol.map <- symbol.map[which(symbol.map$gene_id %in% enIDs),];
  dataSet$symbol.map <- symbol.map;


  dataSet$rawToEntrez <- enIDs
  names(dataSet$rawToEntrez) <- gene.vec;
  
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
    data.norm <- dataSet$data.raw[hit.inx,];
    matched.entrez <- enIDs[hit.inx];
    
    
    # now, deal with duplicated entrez id
    # first, average duplicate rows
    
    myave <- function (x, ...) {
      n <- length(list(...))
      if (n) {
        g <- interaction(...)
        split(x, g) <- lapply(split(x, g), mean, na.rm=T)
      }
      else x[] <- FUN(x, na.rm=T)
      return(x);
    }
    ave.data <- apply(data.norm, 2, myave, matched.entrez); 
    # then removed duplicated entries
    dup.inx <- duplicated(matched.entrez);
    matched.entrez <- matched.entrez[!dup.inx]
    int.mat <- ave.data[!dup.inx,];
    # update
    dataSet$data.annotated <-int.mat;
    rownames(int.mat) <- matched.entrez
    if(idtype %in% c("mir_id", "mir_acc", "mirnet")){
      rownames(dataSet$data.annotated) <- rownames(int.mat);
    }else{
      rownames(dataSet$data.annotated) <- matched.entrez
    }
    dataSet$enrich_ids = rownames(int.mat);
    names(dataSet$enrich_ids) = doEntrez2SymbolMapping(rownames(int.mat), paramSet$data.org, paramSet$data.idType)

    dataSet$id.type <- "entrez";

  }else{
    dataSet$data.annotated <- dataSet$data.raw;
    dataSet$enrich_ids = rownames(dataSet$data.annotated)
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

  dataSet$data.missed <- dataSet$data.filtered <- dataSet$data.annotated;
  msgSet$current.msg <- msg;
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  return(RegisterData(dataSet));
}


#Convert a vector of ids to vector of entrez ids
.doAnnotation <- function(id.vec, idType, paramSet){
  if(idType %in% c("entrez", "symbol", "refseq", "gb", "embl_gene","embl_protein", "embl_transcript", "orf", "tair", "wormbase", "ko", "custom", "cds", "s2f")){
    anot.id <- .doGeneIDMapping(id.vec, idType, paramSet, "vec");
  }else{
    anot.id <- .doProbeMapping(id.vec, idType, paramSet);
    names(anot.id) <- id.vec;
  }
  return(anot.id);        
}



# mapping between genebank, refseq and entrez
.doGeneIDMapping <- function(q.vec, type, paramSet, outputType="vec"){

    org <- paramSet$data.org;
  if(is.null(q.vec)){
    db.map <-  queryGeneDB("entrez", org);
    q.vec <- db.map[, "gene_id"];
    type <-"entrez";
  }
    col.nm = "";
    db.nm = "";

  if(paramSet$data.idType %in% c("s2f", "ko")){ # only for ko
    col.nm = "gene_id";
    db.nm = paste0("entrez_", paramSet$data.idType);
  }else if(type == "symbol"){
    col.nm = "symbol";
    db.nm = "entrez";
  }else if(type == "entrez"){
    col.nm = "gene_id";
    db.nm = "entrez";
  }else if(type == "custom"){ # only for ko
    db.nm = "custom";
    col.nm = "gene_id";
  }else if(type == "cds"){
    col.nm = "accession";
    db.nm = "entrez";
    db.map <- queryGeneDB(paste0("entrez_",type), org);
  }else{
    # note, some ID can have version number which is not in the database
    # need to strip it off NM_001402.5 => NM_001402
    if(!(type == "refseq" && org == "fcd")){ # do not strip, database contains version number.
    q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
    q.vec <- q.mat[,1];
    }
    col.nm = "accession";
    if(type == "tair"){ # only for ath
      db.nm = "tair";
    }else { 
      # if(type %in% c("gb", "refseq", "embl_gene", "embl_transcript", "embl_protein", "orf", "wormbase")){
      db.nm <- paste0("entrez_", type);
    }
  }

  db.map <-  queryGeneDB(db.nm, org);
  if(org == "smm" && type == "symbol"){
    q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
    q.vec <- q.mat[,1];
    q.mat <- do.call(rbind, strsplit(db.map[, col.nm], "\\."));
    db.map[, col.nm] <- q.mat[,1];
  }

    hit.inx <- match(q.vec, db.map[, col.nm]);

  if(outputType == "vec"){
    entrezs <- db.map[hit.inx, "gene_id"];

    mode(entrezs) <- "character";
    rm(db.map, q.vec); gc();
    return(entrezs);
  }else{
    entrezs <- db.map[hit.inx, ]; 
    if(type == "entrez"){
      entrezs <- entrezs[,c(2,1)];
    }else{
      entrezs <- entrezs[,c(2,1)];
    }
    na.inx <- is.na(entrezs[,1]);
    entrezs[,1][na.inx] <- q.vec[na.inx];
    na.inx <- is.na(entrezs[,2]);
    entrezs[,2][na.inx] <- q.vec[na.inx];
    colnames(entrezs) <- c("accession", "gene_id")
    return(entrezs);
  }
}


# from probe ID to entrez ID 
.doProbeMapping <- function(probe.vec, platform, paramSet){
  data.org <- paramSet$data.org;

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


queryGeneDB <- function(table.nm, data.org){
  paramSet <- readSet(paramSet, "paramSet");    
  if(table.nm == "custom" || data.org == "custom"){
    db.map <- qs::qread("anot_table.qs");
  }else{
    require('RSQLite');
    db.path <- paste(paramSet$sqlite.path, data.org, "_genes.sqlite", sep="")

    if(!PrepareSqliteDB(db.path, paramSet$on.public.web)){
      stop("Sqlite database is missing, please check your internet connection!");
    }
    conv.db <- dbConnect(SQLite(), db.path); 
    tbls <- dbListTables(conv.db)
    if(!table.nm %in% tbls){
        return(0);
    }
    db.map <- dbReadTable(conv.db, table.nm);
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


doEntrez2SymbolMapping <- function(entrez.vec,data.org="hsa", data.idType="entrez"){
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