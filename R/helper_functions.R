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
  data <- dataSet$data.anot;
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
 
      nmidx <- sapply(nms, function(x) length(unlist(gregexpr(" vs ",x))))
      if(any(nmidx>1)){
      nmv <- names(nmidx)[which(nmidx>1)]
      nmv <- sapply(nmv, function(x) unlist(gregexpr(" vs [0-9]",x)))
      for(i in 1:length(nmv)){
        for(j in 1:length(nmv[[i]])){
          substr(names(nmv)[i], nmv[[i]][j],nmv[[i]][j]+3) <- "...."
        }
        names(nmv)[i] <- gsub("\\....",".", names(nmv)[i])
       }
       nms[which(nmidx>1)] <- names(nmv)
      }
      
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
#print(msgSet$summaryVec);
  return(msgSet$summaryVec);
}

GetMetaColLength<- function(dataName=""){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");

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
    paramSet <- readSet(paramSet, "paramSet");
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
  paramSet <- readSet(paramSet, "paramSet");
  if(paramSet$anal.type == "genelist"){
  dm <- c(nrow(dataSet$prot.mat), 0);
  naNum <- 0;
  }else{
  dm <- dim(dataSet$data.norm);
  naNum <- sum(is.na(dataSet$data.norm));
  }

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
    grpInfo <- dataSet$meta.info[[clsLbl]];
    grpLbls <- paste(levels(grpInfo), collapse="\n");
    smplInfo <- paste(Sample = colnames(dataSet$data.orig), "\t", Class=grpInfo, collapse="\n");
    return(c(grpLbls, smplInfo));
}

GetMetaSummaryData<- function(){
    paramSet <- readSet(paramSet, "paramSet");
    inmex.meta <- qs::qread("inmex_meta.qs");
    sel.nms <- unique(inmex.meta$data.lbl)
    sel.nms <- paste(sel.nms, collapse="; ")
    cls.lbls <- unique(inmex.meta$cls.lbl)
    cls.lbls <- paste(cls.lbls, collapse="; ")
    paramSet$summaryVec <- c(length(colnames(inmex.meta$data)),nrow(inmex.meta$data), sel.nms, cls.lbls);
    saveSet(paramSet);
    return(c(length(colnames(inmex.meta$data)),nrow(inmex.meta$data), sel.nms, cls.lbls))
}

GetDatasetNamesString <- function(){
    inmex.meta <- qs::qread("inmex_meta.qs");
    paste(unique(inmex.meta$data.lbl), collapse="||");
}

##Single matrix
GetSampleNumber <-function(){
  data.orig <- qs::qread("data.raw.qs");
  return(ncol(data.orig));
}


GetFilesToBeSaved <-function(naviString){
  paramSet <- readSet(paramSet, "paramSet");
  return(unique(paramSet$partialToBeSaved));
}

GetMetaInfo <- function(dataName=""){
  paramSet <- readSet(paramSet, "paramSet");
  if(paramSet$anal.type == "metadata"){

  }else{
  dataSet <- readDataset(dataName);
  metaNms<-setdiff(colnames(dataSet$meta.info),dataSet$rmMetaCol)
  }
  return(metaNms);
}

GetExpressResultGeneSymbols<-function(){
  analSet <- readSet(analSet, "analSet");
  return(analSet$sig.genes.symbols);
}

GetExpressResultGeneIDLinks <- function(dataName=""){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  ids <- rownames(dataSet$comp.res);
  if(paramSet$data.org == "generic"){
    if(paramSet$data.idType == "ko"){
        annots <- paste("<a href='https://www.genome.jp/dbget-bin/www_bget?", ids, "' target='_blank'>KEGG</a>", sep="");
    }else if(paramSet$data.idType == "s2f"){
        annots <- paste("<a href='https://www.ecoomicsdb.ca/#/query?ortho=", ids, "' target='_blank'>EODB</a>", sep="");
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
  paramSet <- readSet(paramSet, "paramSet");

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


SelectDataSet <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  if(!exists('nm.vec')){
    AddErrMsg("No dataset is selected for analysis!");
    return(0);
  }
  mdata.all <- paramSet$mdata.all
  all.nms <- names(mdata.all);
  for(nm in all.nms){
    if(nm %in% nm.vec){
      mdata.all[[nm]] <- 1;
    }else{
      mdata.all[[nm]] <- 0;
    }
  }
  
  if("meta_dat" %in% nm.vec){
    meta.selected <<- TRUE;
  }else{
    meta.selected <<- FALSE;
  }
  
  rm('nm.vec', envir = .GlobalEnv);

  paramSet$mdata.all <- mdata.all
  return(1);
  
}


GetFeatureNum <-function(dataName){
  dataSet <- readDataset(dataName);
  
  return(nrow(dataSet$data.norm));
}

# get qualified inx with at least number of replicates
GetDiscreteInx <- function(my.dat, min.rep=2){
  good.inx <- apply(my.dat, 2, function(x){
    x <- x[x!="NA"]
    good1.inx <- length(x) > length(unique(x));
    good2.inx <- min(table(x)) >= min.rep;
    return (good1.inx & good2.inx);
  });
  return(good.inx);
}

GetNumbericalInx <- function(my.dat){
  suppressWarnings({
  good.inx <- apply(my.dat, 2, function(x){
    isNum = as.numeric(as.character(x[x!="NA"]))
    return(all(!is.na(as.numeric(as.character(isNum)))));
  });
  })
  return(good.inx);
}

.set.dataSet <- function(dataSetObj=NA){
  RegisterData(dataSetObj);
  return (1);
}

# remove data object, the current dataSet will be the last one by default 
RemoveData <- function(dataName){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  if(!is.null(paramSet$mdata.all[[dataName]])){
    paramSet$mdata.all[[dataName]] <- NULL;
  }
  saveSet(paramSet, "paramSet");
}


GetCovSigFileName <-function(dataName){
  dataSet <- readDataset(dataName);
  dataSet$analSet$cov$sig.nm;
}

GetCovSigMat<-function(dataName){
  dataSet <- readDataset(dataName);
  drops <- c("ids","label")
  return(CleanNumber(as.matrix(dataSet$analSet$cov$sig.mat[, !(names(dataSet$analSet$cov$sig.mat) %in% drops)])));
}

GetCovSigIds<-function(dataName){
  dataSet <- readDataset(dataName);
  dataSet$analSet$cov$sig.mat$ids;
}

GetCovSigSymbols<-function(dataName){
  dataSet <- readDataset(dataName);
  dataSet$analSet$cov$sig.mat$label
}

GetCovSigColNames<-function(dataName){
  dataSet <- readDataset(dataName);
  drops <- c("ids","label");
  colnames(dataSet$analSet$cov$sig.mat[,!(names(dataSet$analSet$cov$sig.mat) %in% drops)]);
}

GetCovDENums <- function(dataName){
    deNum <- nrow(dataSet$analSet$cov$sig.mat);
    nonDeNum <- nrow(dataSet$comp.res) - deNum;
    return(c(deNum, nonDeNum));
}


#'Replace infinite numbers
#'@description Replace -Inf, Inf to 99999 and -99999
#'@param bdata Input matrix to clean numbers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CleanNumber <-function(bdata){
  if(sum(bdata==Inf)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- 999999;
  }
  if(sum(bdata==-Inf)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- -999999;
  }
  bdata;
}

GetMetaMethodPVal <-function(){
  paramSet <- readSet(paramSet, "paramSet");
  return(paramSet$BHth);
}