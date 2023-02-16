##################################################
## R script for ExpressAnalyst
## Description: Functions related to web interface
## Author: Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
##################################################

GetSigGeneCount <- function(){
  analSet <- readSet(analSet, "analSet");
  return(analSet$sig.gene.count);
}


CheckRawDataAlreadyNormalized <- function(dataName=""){
  dataSet <- readDataset(dataName);
  data <- dataSet$data.anot
  if(sum(data > 100) > 100){ # now we think it is raw counts
    return(0);
  }else{
    return(1);
  }
}

GetMetaCol<- function(dataName=""){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  if(anal.type == "onedata"){
    colNms <- colnames(dataSet$comp.res);
    if (dataSet$de.method=="limma"){
      inx <- match("AveExpr", colNms)
    } else if (dataSet$de.method=="deseq2"){
      inx <- match("baseMean", colNms)
      return(colnames(dataSet$contrast.matrix));
    } else {
      inx <- match("logCPM", colNms)
    }
    resT <- dataSet$comp.res;
    if(inx > 2){
      resT <- resT[,1:inx-1];
      nms <- gsub("logFC.", "logFC_", colnames(resT));
      nms <- gsub("\\.", " vs ", nms);
      return(as.vector(nms));
    }else{
      return(dataSet$par1);
    }
  }else{
    nms <- paste(unique(dataSet$cls), collapse=" vs ");
    return(nms);
  }
}

GetSummaryData <- function(){
  msgSet <- readSet(msgSet, "msgSet");
  return(msgSet$summaryVec);
}

GetMetaColLength<- function(dataName=""){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");;

  if (dataSet$de.method=="limma"){
    inx <- match("AveExpr", colnames(dataSet$comp.res))
  } else if (dataSet$de.method=="deseq2"){
    inx <- match("baseMean", colnames(dataSet$comp.res))
    if(dataSet$contrast.type == "default"){
        return(dim(dataSet$contrast.matrix)[2]);
    }
  } else {
    inx <- match("logCPM", colnames(dataSet$comp.res))
  }
  resT <- dataSet$comp.res;
  resT <- resT[,1:inx-1]
  return(length(colnames(resT)));
}

GetInitLib <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  init.lib <- paramSet$init.lib;
  return(init.lib)
}

GetMetaDatasets<- function(){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  sel.nms <- names(mdata.all)[mdata.all==1];
  return(sel.nms);
}

SetSelMetaData<- function(selNm){
    paramSet <- readSet(paramSet, "paramSet");;
    paramSet$selDataNm <- selNm;
    paramSet$jsonNms$dataName <- selNm;
    saveSet(paramSet, "paramSet");
}

# only for switching single expression data results
SetCurrentData <- function(nm){
  dataSet <- readDataset(nm);
  return(1);
}

GetOmicsDataDims <- function(dataName){
  dataSet <- readDataset(dataName);
  dm <- dim(dataSet$data.norm);
  naNum <- sum(is.na(dataSet$data.norm));
  return(c(dm, naNum));
} 

# given dataSet Name, sample name, and class name, do update
# note, for multiple #class, this set which one to use in the subsequent steps
# last one wins

# read in the data and perform
# gene ID mapping using built in libraries
# matchMin is minimal matched probe (%)
# return the total matched gene number

# obtain sample names and their class labels
GetSampleInfo <- function(dataName, clsLbl){
    dataSet <- readDataset(dataName);
    grpInfo <- dataSet$meta[[clsLbl]];
    grpLbls <- paste(levels(grpInfo), collapse="\n");
    smplInfo <- paste(Sample = colnames(dataSet$data.orig), "\t", Class=grpInfo, collapse="\n");
    return(c(grpLbls, smplInfo));
}

GetMetaSummary<- function(){
   inmex.meta <- qs::qread("inmex_meta.qs");
    sel.nms <- unique(inmex.meta$data.lbl)
    sel.nms <- paste(sel.nms, collapse="; ")
    cls.lbls <- unique(inmex.meta$cls.lbl)
    cls.lbls <- paste(cls.lbls, collapse="; ")
    return(c(length(colnames(inmex.meta$data)),nrow(inmex.meta$data), sel.nms, cls.lbls))
}

GetDatasetNamesString <- function(){
    inmex.meta <- qs::qread("inmex_meta.qs");
    paste(unique(inmex.meta$data.lbl), collapse="||");
}

##Single matrix
GetSampleNumber <-function(){
  data.proc <- qs::qread("data.raw.qs");
  return(ncol(data.proc));
}


GetFilesToBeSaved <-function(naviString){
  paramSet <- readSet(paramSet, "paramSet");
  return(unique(paramSet$partialToBeSaved));
}

GetMetaInfo <- function(dataName=""){
  dataSet <- readDataset(dataName);
  return(colnames(dataSet$meta));
}

GetExpressResultGeneSymbols<-function(){
  analSet <- readSet(analSet, "analSet");
  return(analSet$sig.genes.symbols);
}

GetExpressResultGeneIDLinks <- function(dataName=""){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");;
  ids <- rownames(dataSet$comp.res);
  if(paramSet$data.org == "generic"){
    if(paramSet$data.idType == "ko"){
        annots <- paste("<a href='https://www.genome.jp/dbget-bin/www_bget?", ids, "' target='_blank'>KEGG</a>", sep="");
    }else if(paramSet$data.idType == "s2f"){
        annots <- paste("<a href='https://www.ecoomicsdb.ca/query?ortho=", ids, "' target='_blank'>EODB</a>", sep="");
    }else{
        annots <- ids;
    }
  } else if (paramSet$data.org == "custom"){
    annots <- ids;
  }else{
    annots <- paste("<a href='http://www.ncbi.nlm.nih.gov/gene?term=", ids, "' target='_blank'>NCBI</a>", sep="");
  }
  return(annots);
}


GetExpressResultColNames<-function(){
  resT <- qs::qread("express.de.res.qs");
  colnames(resT);
}

GetExpressResultGeneIDs<-function(dataName=""){
    dataSet <- readDataset(dataName);
    return(rownames(dataSet$comp.res));
}

GetExpressGeneIDType<-function(dataName=""){
  dataSet <- readDataset(dataName);
  return(dataSet$id.current);
}

GetExpressResultMatrix <-function(dataName="", inxt){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");;

  inxt <- as.numeric(inxt)
    if (dataSet$de.method=="limma"){
    inx <- match("AveExpr", colnames(dataSet$comp.res))
  } else if (dataSet$de.method=="deseq2"){
    inx <- match("baseMean", colnames(dataSet$comp.res))
    inxt <- 1;
  } else {
    inx <- match("logCPM", colnames(dataSet$comp.res))
  }
    res <- dataSet$comp.res;
    res <- res[,-(1:inx-1)]
    res <- cbind(dataSet$comp.res[,inxt], res);
    colnames(res)[1] <- colnames(dataSet$comp.res)[inxt];

    dataSet$comp.res <- dataSet$comp.res[order(dataSet$comp.res$adj.P.Val),] 
    dataSet$comp.res <- dataSet$comp.res[which(!rownames(dataSet$comp.res) %in% rownames(dataSet$sig.mat)),]
    dataSet$comp.res <- rbind(dataSet$sig.mat, dataSet$comp.res);
    dataSet$comp.res <- dataSet$comp.res[complete.cases(dataSet$comp.res), ];
    RegisterData(dataSet);

    qs::qsave(res, "express.de.res.qs");
  
  # max 1000 sig for display
  if(nrow(res) > 1000){
    res <- res[1:1000,];
  }
  return(signif(as.matrix(res), 5));
}

###Gene list
GetNumOfLists <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  return(paramSet$numOfLists)
}

GetMetaSigGeneCount <- function(){
  analSet <- readSet(analSet, "analSet");
  return(nrow(analSet$meta.mat));
}

GetCurrentJson <-function(type){
  paramSet <- readSet(paramSet, "paramSet");
  return(paramSet$jsonNms[[type]]);
}