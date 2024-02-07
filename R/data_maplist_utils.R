##################################################
## R script for ExpressAnalyst
## Description: functions for mapping gene list ids (single or multiple)
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#'Mapping user supplied IDs
#'@description Mapping user supplied IDs to internal IDs (in most cases Entrez)
#'@param dataSetObj Input the name of the created datasetObj (see Init.Data).
#'@param listNm File name of the list
#'@param geneIDs Raw string of the gene list
#'@param org Three letters annotation of organism (i.e hsa, mmu)
#'@param idType The orginal ID type
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
MapListIds <- function(listNm, geneIDs, org, idType){
  print("maplistids");
  #geneIDs is text one string, need to make to vector
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- list();
  dataSet$name <- listNm;
  dataSet$orig <- geneIDs;
  current.msg <- NULL;
  paramSet$data.org <- org;
  paramSet$data.idType <- idType;
  listNms <- vector();
  res <- .parseListInput(geneIDs, paramSet, msgSet);
  dataList <- res[[1]];
  paramSet <- res[[2]];
  msgSet <- res[[3]];
  paramSet$numOfLists <- length(dataList)
paramSet$backgroundUniverse <-NULL;
  all.prot.mat <- list(); 
  inx <- 0;
  for(i in 1:length(dataList)){
    dataSet$name = paste0("datalist", i);
    listNms[i] = dataSet$name;
    gene.mat <- prot.mat <- dataList[[i]];
    GeneAnotDB <-.doGeneIDMapping(rownames(gene.mat), idType, paramSet, "table");
    
    na.inx <- is.na(GeneAnotDB[,1]) | is.na(GeneAnotDB[,2]);
    if(sum(!na.inx) < 2){
      msgSet$current.msg <- "Less than two hits found in uniprot database. "; 
    }
    rownames(prot.mat) <- GeneAnotDB[,2];
    prot.mat <- prot.mat[!na.inx, , drop=F];
    
    # now merge duplicates
    res <- RemoveDuplicates(prot.mat, "mean", quiet=T, paramSet, msgSet); 
    prot.mat <- res[[1]];
    msgSet <- res[[2]];
    dataSet$listInx <- paste0("datalist", inx);
    if(is.null(dim(prot.mat))){
      prot.mat <- matrix(prot.mat);
    }
    
    seed.proteins <- rownames(prot.mat);
    dataSet$GeneAnotDB <- GeneAnotDB;
    dataSet$sig.mat <- gene.mat;
    dataSet$prot.mat <- prot.mat;
    dataSet$seeds.proteins <- seed.proteins;
    if(i == 1){
      all.prot.mat <- prot.mat;
      totalseed.proteins = seed.proteins
      list.num <- length(seed.proteins);
    }else{
      totalseed.proteins  = c(totalseed.proteins, seed.proteins);
      all.prot.mat <- rbind(all.prot.mat, prot.mat)
      list.num <-  paste(list.num, length(seed.proteins), sep="; ");

    }
    dataSet$listNum <- length(dataSet$seeds.proteins);
    print(dataSet$listNum);
    inx <- inx + 1;
    fast.write.csv(dataSet$prot.mat, paste0(dataSet$name, ".csv"));
    RegisterData(dataSet); 
  }
  paramSet$all.ent.mat <- all.prot.mat;
  rownames(all.prot.mat) = doEntrez2SymbolMapping(rownames(all.prot.mat), paramSet$data.org, paramSet$data.idType);
  all.prot.mat <- data.frame(as.numeric(all.prot.mat[,1]), rownames(all.prot.mat));
  paramSet$all.prot.mat <- all.prot.mat;
  paramSet$listNms <- listNms;
  mdata.all <- list();
  for(i in 1:length(listNms)){
    mdata.all[i] <- 1;
  }
  names(mdata.all) <- listNms;
  paramSet$partialToBeSaved <- c( paramSet$partialToBeSaved, listNms);
  saveSet(paramSet, "paramSet");
  paramSet$mdata.all <- mdata.all
  paramSet$anal.type <- "genelist";
  paramSet$list.num <- list.num

  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  #RegisterData(dataSet);
  return(1);
}


MapMultiListIds <- function(listNm, org, geneIDs, type){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- list();
  dataSet$orig <- "";
  msgSet$current.msg <- NULL;
  paramSet$data.org <- org;
  paramSet$data.idType <- type;

  listNms <- multiFileNamesU; # multiFilesNamesU assigned from java
  paramSet$numOfLists <-length(multiFileNamesU);
  notOk = 0
  for(i in 1:length(listNms)){
    dataSet = readDataset(listNms[i])
    dataSet$name <- listNms[i];
    gene.mat <- prot.mat <- dataSet$prot.mat;
    GeneAnotDB <-.doGeneIDMapping(rownames(gene.mat), idType, paramSet,"table");
    
    na.inx <- is.na(GeneAnotDB[,1]) | is.na(GeneAnotDB[,2]);
    if(sum(!na.inx) < 2){
      msgSet$current.msg <- paste0("Less than two hits found in database for ", listNms[i]);
      saveSet(msgSet, "msgSet");
      return(0);
    }
    rownames(prot.mat) <- GeneAnotDB[,2];
    prot.mat <- prot.mat[!na.inx, , drop=F];
    
    # now merge duplicates
    dataSet$listInx <- listNms[i];
    res <- RemoveDuplicates(prot.mat, "mean", quiet=F, paramSet, msgSet); 
    prot.mat <- res[[1]];
    msgSet <- res[[2]];
    if(is.null(dim(prot.mat))){
      prot.mat <- matrix(prot.mat);
    }
    
    seed.proteins <- rownames(prot.mat);
    dataSet$GeneAnotDB <- GeneAnotDB;
    dataSet$sig.mat <- gene.mat;
    dataSet$prot.mat <- prot.mat;
    dataSet$seeds.proteins <- seed.proteins;
    if(i == 1){
      all.prot.mat <- prot.mat;
      totalseed.proteins = seed.proteins
      list.num <- length(seed.proteins);
    }else{
      totalseed.proteins  = c(totalseed.proteins, seed.proteins);
      all.prot.mat <- rbind(all.prot.mat, prot.mat)
      list.num <-  paste(list.num, length(seed.proteins), sep="; ");

    }
    RegisterData(dataSet); 
  }
  paramSet$all.ent.mat <- all.prot.mat
  rownames(all.prot.mat) = doEntrez2SymbolMapping(rownames(all.prot.mat), paramSet$data.org, paramSet$data.idType)
  all.prot.mat <- data.frame(as.numeric(all.prot.mat[,1]), rownames(all.prot.mat));
  paramSet$all.prot.mat <- all.prot.mat
  paramSet$listNms <- listNms
  paramSet$list.num <- list.num
  mdata.all <- list();
  for(i in 1:length(listNms)){
    mdata.all[i] <- 1;
  }
  paramSet$backgroundUniverse <-NULL;
  names(mdata.all) <- listNms;
  paramSet$mdata.all <- mdata.all;

  paramSet$anal.type <- "genelist";
  saveSet(paramSet, "paramSet");

  return(RegisterData(dataSet));
}


#########################################
##########################################
############# private utility methods #### 
##########################################
##########################################
# given a text from input area: one or two-col entries (geneID and logFC)
# parse out return the a matrix containing the logFc, with rows named by gene ID
# if no 2nd col (logFC), the value will be padded by 0s
.parseListInput <- function(geneIDs, paramSet, msgSet){
  spl <- unlist(strsplit(geneIDs, "\\//")[1]);
  spl <- spl[unlist(lapply(spl,function(x){!x %in% ""}))]
  spl <- lapply(spl,function(x){gsub("\\/", "",x)})
  dataList <- list();
  inxU <- 0;
  for (i in 1:length(spl)){
    lines <- unlist(strsplit(spl[[i]], "\r|\n|\r\n")[1]);
    # remove the beginning & trailing space 
    lines <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", lines, perl=TRUE);
    if(substring(lines[1],1,1)=="#"){
      lines <- lines[-1];
    }
    gene.lists <- strsplit(lines, "\\s+");
    gene.mat <- do.call(rbind, gene.lists);

    if(dim(gene.mat)[2] == 1){ # add 0
      gene.mat <- cbind(gene.mat, rep(0, nrow(gene.mat)));
      msgSet$current.msg <- "Only one column found in the list. Abundance will be all zeros. ";
    }else if(dim(gene.mat)[2] > 2){
      gene.mat <- gene.mat[,1:2];
      msgSet$current.msg <- "More than two columns found in the list. Only first two columns will be used. ";
    }

    inx <- .is_valid_numeric(gene.mat[,2]);
    gene.mat[!inx ,2] <- 0;

    rownames(gene.mat) <- gene.mat[,1];
    gene.mat <- gene.mat[,-1, drop=F];
    res <- RemoveDuplicates(gene.mat, "mean", quiet=F, paramSet, msgSet); 
    gene.mat <- res[[1]];
    msgSet <- res[[2]];
    good.inx <- !is.na(gene.mat[,1]);
    gene.mat <- gene.mat[good.inx, , drop=F];
    dataList[[i]] <- gene.mat;
  }
  return(list(dataList, paramSet, msgSet));
}

#'Read gene list
#'@description read gene list, Fold Change (optional) and store the data in dataSet list object;
#'@param fileName file name of the data, .txt format
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
ReadListFile <- function(fileName) {
  paramSet <- readSet(paramSet, "paramSet");
  dat1 <- data.table::fread(fileName, header=FALSE, check.names=FALSE, data.table=FALSE);
  dataSet$name <- fileName
  rowNms <- dat1[,1]
  if(length(dat1) == 1){
    dat1[,1] <- 0
  }else{
    dat1[,1] <- dat1[,2]
    dat1 <- dat1[,-2];
  }
  dataSet$prot.mat <- as.matrix(dat1)
  rownames(dataSet$prot.mat) <- rowNms;

  paramSet$anal.type <- "genelist";
  saveSet(paramSet, "paramSet");

  saveDataset(dataSet);# keep original copy, not in mem
  return(1)
}

.is_valid_numeric <- function(x) {
  as_numeric <- suppressWarnings(as.numeric(x))
  !is.na(as_numeric)
}

ReadUniverseFile <- function(fileName) {
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");

  dat1 <- data.table::fread(fileName, header=FALSE, check.names=FALSE, data.table=FALSE);
  rowNms <- dat1[,1]
  if(length(dat1) == 1){
    dat1[,1] <- 0
  }else{
    dat1[,1] <- dat1[,2]
    dat1 <- dat1[,-2];
  }

    GeneAnotDB <-unique(.doGeneIDMapping(rowNms, paramSet$data.idType, paramSet,"table"));
    
    na.inx <- is.na(GeneAnotDB[,1]) | is.na(GeneAnotDB[,2]);
    if(sum(!na.inx) < 2){
      msgSet$current.msg <- paste0("Less than two hits found in database for the background universe!");
      saveSet(msgSet, "msgSet");
      return(0);
    }
    backgroundUniverse <- GeneAnotDB[!na.inx,2];


    paramSet$backgroundUniverse <- backgroundUniverse;
    saveSet(paramSet, "paramSet");

    return(length(backgroundUniverse))
}