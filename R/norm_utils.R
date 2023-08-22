##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#'Perform data normalization
#'@description Filtering and Normalizing gene expression data
#'@param norm.opt Normalization method to be used
#'@param var.thresh Variance threshold
#'@param abundance Relative abundance threshold
#'@param count.thresh Count threshold for RNA-seq data and abundance threshold for microarray data
#'@param filterUnmapped, boolean, whether to filter unmapped genes or not
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'

PerformNormalization <- function(dataName, norm.opt, var.thresh, count.thresh, filterUnmapped, islog="false"){

  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(dataName);
  msg <- ""; 
  #Filter data
  data <- PerformFiltering(dataSet, var.thresh, count.thresh, filterUnmapped);
  dataSet$data.anot <- data;
  msg <- paste(filt.msg, msg);
  
  if(dataSet$type=="prot"){
    if(islog=="true"|norm.opt=="Rlr" | norm.opt=="Loess"){
      data <- NormalizeData(data, "log", "NA", "NA");
      msg <- paste(norm.msg, msg);
   }
  }
  
  # save parameters for report
  dataSet$norm.opt <- norm.opt;
  dataSet$var.perc <- var.thresh;
  dataSet$abun.perc <- count.thresh;

  #Normalize data
  data <- NormalizeData(data, norm.opt, "NA", "NA");

  # Curve-fitting can't handle negative values
  if(paramSet$oneDataAnalType == "dose" & min(data) < 0){
    add.val <- abs(min(data)) + 0.05*abs(min(data))
    data <- data + add.val
  }
  
  msg <- paste(norm.msg, msg);
  dataSet$data.norm <- data

  # save normalized data for download user option
  fast.write(dataSet$data.norm, file="data_normalized.csv");
  qs::qsave(data, file="data.stat.qs");
  
  msgSet$current.msg <- msg; 
  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet));
}

PerformFiltering <- function(dataSet, var.thresh, count.thresh, filterUnmapped){
  msg <- "";
  if(filterUnmapped == "false"){
    # need to update those with annotations
    data1 <- qs::qread("data.raw.qs");
    colnames(data1) <- colnames(dataSet$data.norm)
    anot.id <- qs::qread("annotation.qs");
    hit.inx <- !is.na(anot.id);
    rownames(data1)[hit.inx] <- anot.id[hit.inx];
    res <- RemoveDuplicates(data1, "mean", quiet=T, paramSet, msgSet);
    data1 <- res[[1]];
    msgSet <- res[[2]];
    raw.data.anot <- data1;
    msg <- "Only features with annotations are kept for further analysis.";
  }else{
    if(dataSet$type=="prot"){
     raw.data.anot <- qs::qread("data.missed.qs");
    }else{
     raw.data.anot <- qs::qread("orig.data.anot.qs");
    }
   colnames(raw.data.anot) <- colnames(dataSet$data.norm)
  }
  
  data <- raw.data.anot;
  data<- data[,which(colnames(data)%in% rownames(dataSet$meta.info))]
  if (dataSet$type == "count"){
    sum.counts <- apply(data, 1, sum, na.rm=TRUE);
    rm.inx <- sum.counts < count.thresh;
    msg <- paste(msg, "Filtered ", sum(rm.inx), " genes with low counts.", collapse=" ");
  }else{
    avg.signal <- apply(data, 1, mean, na.rm=TRUE)
    abundance.pct <- count.thresh/100;
    p05 <- quantile(avg.signal, abundance.pct)
    all.rows <- nrow(data)
    rm.inx <- avg.signal < p05;
    msg <- paste(msg, "Filtered ", sum(rm.inx), " genes with low relative abundance (average expression signal).", collapse=" ");
  }
  
  data <- data[!rm.inx,];
  filter.val <- apply(data, 1, IQR, na.rm=T);
  nm <- "Interquantile Range";
  filter.val <- -filter.val
  rk <- rank(filter.val, ties.method='random');
  # remove constant values
  good.inx <- -filter.val > 0;
  kp.pct <- (100 - var.thresh)/100;
  remain <- rk < nrow(data)*kp.pct;
  data <- data[remain&good.inx,];
  filt.msg <<- paste(msg, paste("Filtered ", nrow(data), " low variance genes based on IQR"), collapse=" ");

  return(data);
}

NormalizeDataMetaMode <-function (nm, opt, colNorm="NA", scaleNorm="NA"){
  if(nm == "NA"){
    paramSet <- readSet(paramSet, "paramSet");
    mdata.all <- paramSet$mdata.all;
    sel.nms <- names(mdata.all);
    for(i in 1:length(sel.nms)){
      dataName <- sel.nms[i];
      dataSet = readDataset(dataName);
      data.filtered <- readDataQs("data.filtered.qs", paramSet$anal.type, dataName);
      data <- NormalizeData(data.filtered,opt, colNorm, scaleNorm);
      if(length(data) == 1){
        return(0);
      }
      dataSet$data.norm <- data;
      RegisterData(dataSet);
    }
    return(1)
  }else{
    dataSet <- readDataset(nm);
    data.filtered <- readDataQs("data.filtered.qs", paramSet$anal.type, nm);
    data <- NormalizeData(data.filtered,opt, colNorm, scaleNorm);
    if(length(data) == 1){
      return(0);
    }
    dataSet$data.norm <- data;
    qs::qsave(data, file="data.stat.qs");
    return(RegisterData(dataSet));
    
  }
}

NormalizeData <-function (data, norm.opt, colNorm="NA", scaleNorm="NA"){
  msg <- ""
  row.nms <- rownames(data);
  col.nms <- colnames(data);
  msgSet <- readSet(msgSet, "msgSet");
  
  # column(sample)-wise normalization
  if(colNorm=="SumNorm"){
    data<-t(apply(data, 2, SumNorm));
    rownm<-"Normalization to constant sum";
  }else if(colNorm=="MedianNorm"){
    data<-t(apply(data, 2, MedianNorm));
    rownm<-"Normalization to sample median";
  }else{
    # nothing to do
    rownm<-"N/A";
  }
  # norm.opt
  if(norm.opt=="log"){
    min.val <- min(data[data>0], na.rm=T)/10;
    numberOfNeg = sum(data<=0, na.rm = TRUE) + 1; 
    totalNumber = length(data)
    if((numberOfNeg/totalNumber)>0.2){
      msg <- paste(msg, "Can't perform log2 normalization, over 20% of data are negative. Try a different method or maybe the data already normalized?", collapse=" ");
      msgSet$norm.msg <- msgSet$current.msg <- msg;
      saveSet(msgSet, "msgSet");
      return(0);
    }
    data[data<=0] <- min.val;
    data <- log2(data);
    msg <- paste(msg, "Log2 transformation.", collapse=" ");
  }else if(norm.opt=="vsn"){
    require(limma);
    data <- normalizeVSN(data);
    msg <- paste(msg, "VSN normalization.", collapse=" ");
  }else if(norm.opt=="quantile"){
    require('preprocessCore');
    data <- normalize.quantiles(as.matrix(data), copy=TRUE);
    msg <- paste(msg, "Quantile normalization.", collapse=" ");
  }else if(norm.opt=="combined"){
    require(limma);
    data <- normalizeVSN(data);
    require('preprocessCore');
    data <- normalize.quantiles(as.matrix(data), copy=TRUE);
    msg <- paste(msg, "VSN followed by quantile normalization.", collapse=" ");
  }else if(norm.opt=="logcount"){ # for count data, do it in DE analysis, as it is dependent on design matrix
    require(edgeR);
    nf <- calcNormFactors(data, method = "none");
    y <- voom(data,plot=F,lib.size=colSums(data)*nf);
    data <- y$E; # copy per million
    msg <- paste(msg, "Limma based on log2-counts per million transformation.", collapse=" ");
  } else if(norm.opt=="RLE"){
    suppressMessages(require(edgeR))
    nf <- calcNormFactors(data,method="RLE");
    y <- voom(data,plot=F,lib.size=colSums(data)*nf);
    data <- y$E; # copy per million
    msg <- c(msg, paste("Performed RLE Normalization"));
  }else if(norm.opt=="TMM"){
    suppressMessages(require(edgeR))
    nf <- calcNormFactors(data,method="TMM");
    y <- voom(data,plot=F,lib.size=colSums(data)*nf);
    data <- y$E; # copy per million
    msg <- c(msg, paste("Performed TMM Normalization"));
  }else if(norm.opt=="clr"){
    data <- apply(data, 2, clr_transform);
    msg <- "Performed centered-log-ratio normalization.";
  }else if(norm.opt=='LogNorm'){
    min.val <- min(abs(data[data!=0]))/10;
    data<-apply(data, 2, LogNorm, min.val);
  }else if(norm.opt=='CrNorm'){
    norm.data <- abs(data)^(1/3);
    norm.data[data<0] <- - norm.data[data<0];
    data <- norm.data;
  }else if(norm.opt=='Rlr'){
    norm.data <- RLRNorm(data)
    msg <- paste(msg, "Performed Linear Regression Normalization.", collapse=" ");
  }else if(norm.opt=='Loess'){
    norm.data <- LoessNorm(data)
    msg <- paste(msg, "Performed Local Regression Normalization.", collapse=" ");
  }else if(norm.opt=='EigenMS'){
     msg <- paste(msg, "Performed EigenMS Normalization.", collapse=" ");
  }else if(norm.opt=='median'){
    data<- apply(data, 2, MedianNorm);
    msg <- paste(msg, "Normalization to sample median.", collapse=" ");
  }
  
  
  # scaling
  if(scaleNorm=='MeanCenter'){
    data<-apply(data, 1, MeanCenter);
    scalenm<-"Mean Centering";
  }else if(scaleNorm=='AutoNorm'){
    data<-apply(data, 1, AutoNorm);
    scalenm<-"Autoscaling";
  }else if(scaleNorm=='ParetoNorm'){
    data<-apply(data, 1, ParetoNorm);
    scalenm<-"Pareto Scaling";
  }else if(scaleNorm=='RangeNorm'){
    data<-apply(data, 1, RangeNorm);
    scalenm<-"Range Scaling";
  }else if(scaleNorm=="colsum"){
    data <- sweep(data, 2, colSums(data), FUN="/")
    data <- data*10000000;
    msg <- c(msg, paste("Performed total sum normalization."));
  }else if(scaleNorm=="upperquartile" || norm.opt == "upperquartile"){
    suppressMessages(require(edgeR))
    nf <- calcNormFactors(data,method="upperquartile");
    y <- voom(data,plot=F,lib.size=colSums(data)*nf);
    data <- y$E; # copy per million
    msg <- c(msg, paste("Performed upper quartile normalization"));
  }else if(scaleNorm=="CSS"){
    suppressMessages(require(metagenomeSeq))
    #biom and mothur data also has to be in class(matrix only not in phyloseq:otu_table)
    data1 <- as(data,"matrix");
    dataMR <- newMRexperiment(data1);
    data <- cumNorm(dataMR,p=cumNormStat(dataMR));
    data <- MRcounts(data,norm = T);
    msg <- c(msg, paste("Performed cumulative sum scaling normalization"));
  }else{
    scalenm<-"N/A";
  }
  
  if(scaleNorm %in% c('MeanCenter', 'AutoNorm', 'ParetoNorm', 'RangeNorm')){
    data <- t(data)
  }
  
  norm.msg <<- msg;

  rownames(data) <- row.nms;
  colnames(data) <- col.nms;

  msgSet$current.msg <- msg;
  saveSet(msgSet, "msgSet");
  return(data)
}


########
#
#Normalization internal methods
#
########

# based on phyloseq post: https://github.com/joey711/shiny-phyloseq/blob/master/panels/paneldoc/Transform.md
clr_transform <- function(x, base=2){
  x <- log((x / gm_mean(x)), base)
  x[!is.finite(x) | is.na(x)] <- 0.0
  return(x)
}


# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log10((x + sqrt(x^2 + min.val^2))/2)
}


SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}


# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but variance/SE
ParetoNorm<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but variance/SE
MeanCenter<-function(x){
  x - mean(x);
}

# normalize to zero mean but variance/SE
RangeNorm<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}
########### adapted from NormalyzerDE (https://github.com/ComputationalProteomics/NormalyzerDE)

RLRNorm <- function(data) {
  
  sampleLog2Median <- apply(data, 1, median,na.rm=T)
  
  calculateRLMForCol <- function(colIndex, sampleLog2Median, data) {
    
    lrFit <- MASS::rlm(as.matrix(data[, colIndex])~sampleLog2Median, na.action=stats::na.exclude)
    coeffs <- lrFit$coefficients
    coefIntercept <- coeffs[1]
    coefSlope <- coeffs[2]
    globalFittedRLRCol <- (data[, colIndex] - coefIntercept) / coefSlope
    globalFittedRLRCol
  }
  
  globalFittedRLR <- vapply(
    seq_len(ncol(data)),
    calculateRLMForCol,
    rep(0, nrow(data)),
    sampleLog2Median=sampleLog2Median,
    data=data
  )
  
  colnames(globalFittedRLR) <- colnames(data)
  
  return(globalFittedRLR)
}

LoessNorm <- function(x, weights = NULL, span=0.7, iterations = 3){
  x <- as.matrix(x)
  n <- ncol(x)
    for (k in 1:iterations) {
      a <- rowMeans(x,na.rm=TRUE)
      for (i in 1:n){
        m <- x[,i] - a
        f <- limma::loessFit(m, a, weights=weights, span=span)$fitted
        x[,i] <- x[,i] - f
      }
    }
  
  return(x)
}