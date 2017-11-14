#'Remove a group from the data 
#'@description This function removes a user-specified group from the data set.
#'This must be performed following data processing and filtering. If the data was normalized prior to removal,
#'you must re-normalize the data. 
#'@usage UpdateGroupItems(mSetObj, grp.nm.vec)  
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param grp.nm.vec Input the name of the group you would like to remove from the data set in quotation marks 
#'(ex: "Disease B") The name must be identical to a class label. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada

UpdateGroupItems <- function(mSetObj=NA, grp.nm.vec){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  if(!grp.nm.vec %in% cls){
    mSetObj$msgSet$current.msg <- "Cannot find group names!"
    return ("Cannot find group names!")}
  
  hit.inx <- cls %in% grp.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[!hit.inx,,drop=FALSE]);
  cleaned.cls <- factor(cls[!hit.inx])
  droplevels(cleaned.cls) # in case factor didn't drop level
  mSetObj$dataSet$prenorm.cls <- cleaned.cls
  
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    dataSet$prenorm.facA <- droplevels(factor(facA[hit.inx]));
    dataSet$prenorm.facB <- droplevels(factor(facB[hit.inx]));
    mSetObj$dataSet$prenorm.facA <- cleaned.facA;
    mSetObj$dataSet$prenorm.facB <- cleaned.facB;
  }
  
  mSetObj$msgSet$current.msg <- "Successfully updated the group items!";
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(length(levels(mSetObj$dataSet$prenorm.cls)));
  }
  return(.set.mSet(mSetObj));
}


CleanDataMatrix <- function(ndata){
  # make sure no costant columns crop up
  varCol <- apply(data.frame(ndata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame 
  constCol <- (varCol == 0 | is.na(varCol));
  return(ndata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
}

#' Remove samples
#' @description This function removes samples from the data set. This must be performed following data processing and filtering.
#' If the data was normalized prior to removal, you must re-normalize the data.  
#' @usage MetaboAnalystR:::UpdateSampleItems(mSetObj, smpl.nm.vec)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#' @param smpl.nm.vec Input the name of the sample to remove from the data in quotation marks. The name must be identical to the 
#' sample names found in the data set.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'
UpdateSampleItems <- function(mSetObj=NA, smpl.nm.vec){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }

  if(!smpl.nm.vec %in% rownames(data)){
    mSetObj$msgSet$current.msg <- "Cannot find the sample names!"
    return ("Cannot find the sample names!")}
    
  hit.inx <- rownames(data) %in% smpl.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[!hit.inx,,drop=FALSE]);
  mSetObj$dataSet$prenorm.cls <- as.factor(as.character(cls[!hit.inx]));
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$prenorm.facA <- as.factor(as.character(facA[!hit.inx]));
    mSetObj$dataSet$prenorm.facB <- as.factor(as.character(facB[!hit.inx]));
  }

  mSetObj$msgSet$current.msg <- "Successfully updated the sample items!";
  print(length(levels(mSetObj$dataSet$prenorm.cls)))
  return(.set.mSet(mSetObj));
}

#' Remove feature items
#' @description This function removes user-selected features from the data set. 
#' This must be performed following data processing and filtering.
#' If the data was normalized prior to removal, you must re-normalize the data.  
#' @usage MetaboAnalystR:::UpdateFeatureItems(mSetObj, feature.nm.vec)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#' @param feature.nm.vec Input the name of the feature to remove from the data in quotation marks. 
#' The name must be identical to the feature names found in the data set.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'
UpdateFeatureItems <- function(mSetObj=NA, feature.nm.vec){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  if(!feature.nm.vec %in% colnames(data)){
    mSetObj$msgSet$current.msg <- "Cannot find the feature names!"
    return ("Cannot find the feature names!")}
  
  hit.inx <- colnames(data) %in% feature.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[,!hit.inx,drop=FALSE]);
  mSetObj$dataSet$prenorm.cls <- cls; # this is the same
  
  mSetObj$msgSet$current.msg <- "Successfully updated the feature items!";
  print("Successfully updated the feature items!");
  return(.set.mSet(mSetObj));
}

#'Normalization
#'@description This function performs row-wise normalization, transformation, and 
#'scaling of your metabolomic data. 
#'@usage Normalization(mSetObj, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param rowNorm Select the option for row-wise normalization, "QuantileNorm" for Quantile Normalization, 
#'"ProbNormT" for Probabilistic Quotient Normalization without using a reference sample,
#'"ProbNormF" for Probabilistic Quotient Normalization based on a reference sample, 
#'"CompNorm" for Normalization by a reference feature,
#'"SumNorm" for Normalization to constant sum, 
#'"MedianNorm" for Normalization to sample median, and 
#'"SpecNorm" for Normalization by a sample-specific factor.
#'@param transNorm Select option to transform the data, "LogNorm" for Log Normalization,
#'and "CrNorm" for Cubic Root Transformation. 
#'@param scaleNorm Select option for scaling the data, "MeanCenter" for Mean Centering,
#'"AutoNorm" for Autoscaling, "ParetoNorm" for Pareto Scaling, amd "RangeNorm" for Range Scaling.
#'@param ref Input the name of the reference sample or the reference feature, use " " around the name.  
#'@param ratio This option is only for biomarker analysis.
#'@param ratioNum Relevant only for biomarker analysis.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada

Normalization <- function(mSetObj=NA, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20){
  
  mSetObj <- .get.mSet(mSetObj);
   
  if(is.null(mSetObj$dataSet$procr)){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet$prenorm)){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
    
  if(is.null(mSetObj$dataSet$prenorm.cls)){ # can be so for regression 
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
  }
  
  cls <- mSetObj$dataSet$prenorm.cls;
  
  # note, setup time factor
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    if(exists('prenorm.facA', where = mSetObj$dataSet)){
      nfacA <- mSetObj$dataSet$prenorm.facA;
      nfacB <- mSetObj$dataSet$prenorm.facB;
    }else{
      nfacA <- mSetObj$dataSet$facA;
      nfacB <- mSetObj$dataSet$facB;
    }
    
    mSetObj$dataSet$facA <- nfacA;
    mSetObj$dataSet$facB <- nfacB;
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor and should order first by subject then by each time points
      if(tolower(mSetObj$dataSet$facA.lbl) == "time"){ 
        time.fac <- nfacA;
        exp.fac <- nfacB;
      }else{
        time.fac <- nfacB;
        exp.fac <- nfacA;
      }
      mSetObj$dataSet$time.fac <- time.fac;
      mSetObj$dataSet$exp.fac <- exp.fac;
    }
  }
  
  colNames <- colnames(data);
  rowNames <- rownames(data);
  
  # row-wise normalization
  if(rowNorm=="QuantileNorm"){
    data<-QuantileNormalize(data);
    # this can introduce constant variables if a variable is 
    # at the same rank across all samples (replaced by its average across all)
    
    varCol <- apply(data, 2, var, na.rm=T);
    constCol <- (varCol == 0 | is.na(varCol));
    constNum <- sum(constCol, na.rm=T);
    if(constNum > 0){
      print(paste("After quantile normalization", constNum, "columns with constant value were found and deleted."));
      data <- data[,!constCol];
      colNames <- colnames(data);
      rowNames <- rownames(data);
    }
    rownm<-"Quantile Normalization";
  }else if(rowNorm=="ProbNormT"){
    grp.inx <- cls == ref;
    ref.smpl <- apply(data[grp.inx, ], 2, mean);
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization";
  }else if(rowNorm=="ProbNormF"){
    ref.smpl <- data[ref,];
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization";
  }else if(rowNorm=="CompNorm"){
    data<-t(apply(data, 1, CompNorm, ref));
    rownm<-"Normalization by a reference feature";
  }else if(rowNorm=="SumNorm"){
    data<-t(apply(data, 1, SumNorm));
    rownm<-"Normalization to constant sum";
  }else if(rowNorm=="MedianNorm"){
    data<-t(apply(data, 1, MedianNorm));
    rownm<-"Normalization to sample median";
  }else if(rowNorm=="SpecNorm"){
    if(!exists("norm.vec")){
      norm.vec <- rep(1,nrow(data)); # default all same weight vec to prevent error
      print("No sample specific information were given, all set to 1.0");
    }
    rownm<-"Normalization by sample-specific factor";
    data<-data/norm.vec;
  }else{
    # nothing to do
    rownm<-"N/A";
  }
  
  # use apply will lose dimesion info (i.e. row names and colnames)
  rownames(data)<-rowNames;
  colnames(data)<-colNames;
  
  # note: row-normed data is based on biological knowledge, since the previous
  # replacing zero/missing values by half of the min positive (a constant) 
  # now may become different due to different norm factor, which is artificial
  # variance and should be corrected again
  #
  # stopped, this step cause troubles
  # minConc<-round(min(data)/2, 5);
  # data[dataSet$fill.inx]<-minConc;
  
  # if the reference by feature, the feature column should be removed, since it is all 1
  if(rowNorm=="CompNorm" && !is.null(ref)){
    inx<-match(ref, colnames(data));
    data<-data[,-inx];
    colNames <- colNames[-inx];
  }
  
  # this is for biomarker analysis only (for compound concentration data)
  if(ratio){
    min.val <- min(abs(data[data!=0]))/2;
    norm.data <- log2((data + sqrt(data^2 + min.val))/2);
    transnm<-"Log Normalization";
    ratio.mat <- CalculatePairwiseDiff(norm.data);
    
    fstats <- Get.Fstat(ratio.mat, cls);
    hit.inx <- rank(-fstats) < ratioNum;  # get top n
    
    
    ratio.mat <- ratio.mat[, hit.inx];
    
    data <- cbind(norm.data, ratio.mat);
    
    colNames <- colnames(data);
    rowNames <- rownames(data);
  
  }
  
  if(!ratio){
    # transformation
    if(transNorm=='LogNorm'){
      min.val <- min(abs(data[data!=0]))/10;
      data<-apply(data, 2, LogNorm, min.val);
      transnm<-"Log Normalization";
    }else if(transNorm=='CrNorm'){
      norm.data <- abs(data)^(1/3);
      norm.data[data<0] <- - norm.data[data<0];
      data <- norm.data;
      transnm<-"Cubic Root Transformation";
    }else{
      transnm<-"N/A";
    }
  }

  # record row-normed data for fold change analysis (b/c not applicable for mean-centered data)
  rownorm <- CleanData(data, T, T)
  mSetObj$dataSet$row.norm <- as.data.frame(rownorm); #moved below ratio 
  
  # scaling
  if(scaleNorm=='MeanCenter'){
    data<-apply(data, 2, MeanCenter);
    scalenm<-"Mean Centering";
  }else if(scaleNorm=='AutoNorm'){
    data<-apply(data, 2, AutoNorm);
    scalenm<-"Autoscaling";
  }else if(scaleNorm=='ParetoNorm'){
    data<-apply(data, 2, ParetoNorm);
    scalenm<-"Pareto Scaling";
  }else if(scaleNorm=='RangeNorm'){
    data<-apply(data, 2, RangeNorm);
    scalenm<-"Range Scaling";
  }else{
    scalenm<-"N/A";
  }
  
  # note after using "apply" function, all the attribute lost, need to add back
  rownames(data)<-rowNames;
  colnames(data)<-colNames;
  
  # need to do some sanity check, for log there may be Inf values introduced
  data <- CleanData(data, T, F);
  
  if(ratio){
    mSetObj$dataSet$ratio <- CleanData(ratio.mat, T, F)
  }
  
  mSetObj$dataSet$norm <- as.data.frame(data);
  mSetObj$dataSet$cls <- cls;
  
  mSetObj$dataSet$rownorm.method <- rownm;
  mSetObj$dataSet$trans.method <- transnm;
  mSetObj$dataSet$scale.method <- scalenm;
  mSetObj$dataSet$combined.method <- FALSE;
  mSetObj$dataSet$norm.all <- NULL; # this is only for biomarker ROC analysis
  print(1);
  return(.set.mSet(mSetObj));
}

#'Row-wise Normalization
#'@description Row-wise norm methods, when x is a row
#'@usage Options for normalize by sum median, reference sample,
#'reference reference (compound), or quantile normalization
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada

# normalize by a sum of each sample, assume constant sum (1000)
# return: normalized data
SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}

# normalize by a reference sample (probability quotient normalization)
# ref should be the name of the reference sample
ProbNorm<-function(x, ref.smpl){
  x/median(as.numeric(x/ref.smpl), na.rm=T)
}

# normalize by a reference reference (i.e. creatinine)
# ref should be the name of the cmpd
CompNorm<-function(x, ref){
  1000*x/x[ref];
}

# perform quantile normalization on the raw data (can be log transformed later by user)
# https://stat.ethz.ch/pipermail/bioconductor/2005-April/008348.html
QuantileNormalize <- function(data){
  library('preprocessCore');
  return(t(normalize.quantiles(t(data), copy=FALSE)));
}

#'Column-wise Normalization
#'@description Column-wise norm methods, when x is a column
#'@usage Options for log, zero mean and unit variance, and
#'several zero mean and variance/SE 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada

# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log2((x + sqrt(x^2 + min.val^2))/2)
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


#'Two plot summary plot: Feature View of before and after normalization
#'@description For each plot, the top is a box plot, bottom is a density plot
#'@usage PlotNormSummary(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'
PlotNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width==0){
    w = 7.2
    h = 9.5
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$norm <- imgName
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,2,2,2,3,4,4,4), 4, 2, byrow = FALSE))
  
  # since there may be too many compounds, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(ncol(mSetObj$dataSet$procr), sub.num=50);
  namesVec <- colnames(mSetObj$dataSet$procr[,pre.inx]);
  
  # only get common ones
  nm.inx <- namesVec %in% colnames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, colnames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$procr[, pre.inx], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[, norm.inx], na.rm=T);
  
  x.label<-GetValueLabel(mSetObj);
  y.label<-GetVariableLabel(mSetObj);
  
  # fig 1
  op<-par(mar=c(4,7,4,0), xaxt="s");
  plot(density(apply(mSetObj$dataSet$procr, 2, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext("Density", 2, 5);
  mtext("Before Normalization",3, 1)
  
  # fig 2
  op<-par(mar=c(7,7,0,0), xaxt="s");
  boxplot(mSetObj$dataSet$procr[,pre.inx], names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext(x.label, 1, 5);
  
  # fig 3
  op<-par(mar=c(4,7,4,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 2, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext("After Normalization",3, 1);
  
  # fig 4
  op<-par(mar=c(7,7,0,2), xaxt="s");
  boxplot(mSetObj$dataSet$norm[,norm.inx], names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", horizontal=T);
  mtext(paste("Normalized",x.label),1, 5);
  
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Two plot summary plot: Sample View of before and after normalization
#'@description For each plot, the top is a density plot and the bottom is a box plot.
#'@usage PlotNormSummary(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'

PlotSampleNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width == 0){
    w <- 7.2;h <- 9.5;
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$summary_norm <-imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,1,1,2,3,3,3,4), 4, 2, byrow = FALSE))
  
  # since there may be too many samples, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(nrow(mSetObj$dataSet$procr), sub.num=50);
  namesVec <- rownames(mSetObj$dataSet$procr[pre.inx,]);
  
  # only get common ones
  nm.inx <- namesVec %in% rownames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, rownames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$procr[pre.inx,], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[norm.inx,], na.rm=T);
  
  x.label<-GetValueLabel(mSetObj);
  y.label<-"Samples";
  
  # fig 1
  op<-par(mar=c(5.75,8,4,0), xaxt="s");
  boxplot(t(mSetObj$dataSet$procr[pre.inx, ]), names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext("Before Normalization", 3,1)
 
  # fig 2
  op<-par(mar=c(6.5,7,0,0), xaxt="s");
  plot(density(apply(mSetObj$dataSet$procr, 1, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext(x.label, 1, 4);
  mtext("Density", 2, 5);
 
  # fig 3
  
  op<-par(mar=c(5.75,8,4,2), xaxt="s");
  boxplot(t(mSetObj$dataSet$norm[norm.inx,]), names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", ylab="", horizontal=T);
  mtext("After Normalization", 3, 1);
  
  # fig 4
  op<-par(mar=c(6.5,7,0,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 1, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext(paste("Normalized",x.label),1, 4)
  
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################


# get the dropdown list for sample normalization view
GetPrenormSmplNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$prenorm)){
    if(is.null(mSetObj$dataSet$filt)){
      mSetObj$dataSet$prenorm <- mSetObj$dataSet$procr;
    }else{
      mSetObj$dataSet$prenorm <- mSetObj$dataSet$filt;
    }
    .set.mSet(mSetObj)
  }
  return(rownames(mSetObj$dataSet$prenorm));
}

GetPrenormFeatureNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$prenorm)){
    if(is.null(mSetObj$dataSet$filt)){
      mSetObj$dataSet$prenorm <- mSetObj$dataSet$procr;
    }else{
      mSetObj$dataSet$prenorm <- mSetObj$dataSet$filt;
    }
    .set.mSet(mSetObj)
  }
  return(colnames(mSetObj$dataSet$prenorm));
}

GetPrenormClsNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$prenorm.cls)){
    if(is.null(mSetObj$dataSet$filt)){
      mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
      if(substring(mSetObj$dataSet$format,4,5) == "ts"){
        mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$proc.facA;
        mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$proc.facB;
      }
    }else{
      mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$filt.cls;
      if(substring(mSetObj$dataSet$format,4,5)=="ts"){
        mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$filt.facA;
        mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$filt.facB;
      }
    }
    .set.mSet(mSetObj)
  }
  return(levels(mSetObj$dataSet$prenorm.cls));
}