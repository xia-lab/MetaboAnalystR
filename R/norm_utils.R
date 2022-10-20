##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#'Perform data normalization
#'@description Normalize gene expression data
#'@param norm.opt Normalization method to be used
#'@param var.thresh Variance threshold
#'@param abundance Relative abundance threshold
#'@param count.thresh Count threshold for RNA-seq data and abundance threshold for microarray data
#'@param filterUnmapped, boolean, whether to filter unmapped genes or not
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

PerformExpressNormalization <- function(dataName, norm.opt, var.thresh, count.thresh, filterUnmapped){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(dataName);
  print(paste("normalizing ....", norm.opt, var.thresh, count.thresh, filterUnmapped));
  msg <- "Only features with annotations are kept for further analysis.";
  
  if(filterUnmapped == "false"){
    # need to update those with annotations
    data1 <- qs::qread("data.proc.qs");
    anot.id <- qs::qread("annotation.qs");
    hit.inx <- !is.na(anot.id);
    rownames(data1)[hit.inx] <- anot.id[hit.inx];
    res <- RemoveDuplicates(data1, "mean", quiet=T, paramSet, msgSet);
    data1 <- res[[1]];
    msgSet <- res[[2]];
    raw.data.anot <- data <- dataSet$data.anot <- data1;
  }else{
    raw.data.anot <- data <- qs::qread("orig.data.anot.qs");
  }
  
  if (dataSet$type == "count"){
    sum.counts <- apply(data, 1, sum, na.rm=TRUE);
    rm.inx <- sum.counts < count.thresh;
    data <- data[!rm.inx,];
    msg <- paste(msg, "Filtered ", sum(rm.inx), " genes with low counts.", collapse=" ");
  }else{
    avg.signal <- apply(data, 1, mean, na.rm=TRUE)
    abundance.pct <- count.thresh/100;
    p05 <- quantile(avg.signal, abundance.pct)
    all.rows <- nrow(data)
    rm.inx <- avg.signal < p05
    data <- data[!rm.inx,]
    msg <- paste(msg, "Filtered ", sum(rm.inx), " genes with low relative abundance (average expression signal).", collapse=" ");
  }
  
  data <- NormalizingDataOmics(data, norm.opt, "NA", "NA");

  if(length(data)==1 && data == 0){
    return(0);
  }
  msg <- paste(norm.msg, msg);
  
  filter.val <- apply(data, 1, IQR, na.rm=T);
  nm <- "Interquantile Range";
  rk <- rank(-filter.val, ties.method='random');
  kp.pct <- (100 - var.thresh)/100;
  
  remain <- rk < nrow(data)*kp.pct;
  data <- data[remain,];
  msg <- paste(msg, paste("Filtered ", sum(!remain), " low variance genes based on IQR"), collapse=" ");
  
  dataSet$data.anot <- raw.data.anot[remain,]
  dataSet$data.norm <- data;
  
  # save normalized data for download user option
  fast.write(dataSet$data.norm, file="data_normalized.csv");
  
  qs::qsave(data, file="data.stat.qs");


  msgSet$current.msg <- msg; 
  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet));
}


NormalizingDataMeta <-function (nm, opt, colNorm="NA", scaleNorm="NA"){
  if(nm == "NA"){
    paramSet <- readSet(paramSet, "paramSet");;
    mdata.all <- paramSet$mdata.all;

    sel.nms <- names(mdata.all)
    for(i in 1:length(sel.nms)){
      dataSet = qs::qread(sel.nms[i])
      data <- NormalizingDataOmics(dataSet$data.filtered,opt, colNorm, scaleNorm)
      if(length(data) == 1){
        return(0);
      }
      dataSet$data.norm <- data;
      dataSet$data <- data;
      qs::qsave(data, file="data.stat.qs");
      RegisterData(dataSet)
    }
    return(1)
  }else{
    dataSet <- qs::qread(nm);
    data <- NormalizingDataOmics(dataSet$data.filtered,opt, colNorm, scaleNorm)
    if(length(data) == 1){
      return(0);
    }
    dataSet$data.norm <- data;
    dataSet$data <- data;
    qs::qsave(data, file="data.stat.qs");
    RegisterData(dataSet)
    return(1)
  }
}

NormalizingDataOmics <-function (data, norm.opt, colNorm="NA", scaleNorm="NA"){
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

  if(norm.opt=="log"){
    min.val <- min(data[data>0], na.rm=T)/10;
    numberOfNeg = sum(data<=0, na.rm = TRUE) + 1; 
    totalNumber = length(data)
    if((numberOfNeg/totalNumber)>0.2){
      msg <- paste(msg, "Can't perform log2 normalization, over 20% of data are negative. Try a different method or maybe the data already normalized?", collapse=" ");
      print(msg);
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
    nf <- calcNormFactors(data);
    y <- voom(data,plot=F,lib.size=colSums(data)*nf);
    data <- y$E; # copy per million
    msg <- paste(msg, "Limma based on log2-counts per million transformation.", collapse=" ");
  } else if(norm.opt=="rle"){
      suppressMessages(require(edgeR))
      otuRLE <- edgeRnorm(data,method="RLE");
      data <- as.matrix(otuRLE$counts);
      msg <- c(msg, paste("Performed RLE Normalization"));
    }else if(norm.opt=="TMM"){
      suppressMessages(require(edgeR))
      otuTMM <- edgeRnorm(data,method="TMM");
      data <- as.matrix(otuTMM$counts);
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
    }else if(scaleNorm=="upperquartile"){
      suppressMessages(require(edgeR))
      otuUQ <- edgeRnorm(data,method="upperquartile");
      data <- as.matrix(otuUQ$counts);
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
  #data <- as.data.frame(data)
  rownames(data) <- row.nms;
  colnames(data) <- col.nms;
  msgSet$current.msg <- msg;
  saveSet(msgSet, "msgSet");
  return(data)
}

