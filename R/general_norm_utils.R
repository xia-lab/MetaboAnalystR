#'Clean the data matrix
#'@description Function used in higher functinos to clean data matrix
#'@param ndata Input the data to be cleaned
#'@export
#'
CleanDataMatrix <- function(ndata){
  # make sure no costant columns crop up
  varCol <- apply(data.frame(ndata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame 
  constCol <- (varCol == 0 | is.na(varCol));
  return(ndata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
}

setRatioOption <- function(mSetObj=NA, opt = "sum"){
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$dataSet$ratioFilterOpt <- opt
    return(.set.mSet(mSetObj));
}

getFeatureNum <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);    
    return(as.integer(mSetObj[["dataSet"]][["proc.feat.num"]]));
}


#'Normalization
#'@description This function performs row-wise normalization, transformation, and 
#'scaling of your metabolomic data. 
#'@usage Normalization(mSetObj, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param rowNorm Select the option for row-wise normalization, "QuantileNorm" for Quantile Normalization, 
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
#'@import qs
#'@export
#'
Normalization <- function(mSetObj=NA, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20){

  if(!.on.public.web){
    # call this to make sure always restart from the same footing, and taking care of prefiltering steps
    # only required to run local R package, as web will be called from interface during bean init
    .prepare.prenorm.data(mSetObj);
  }

  mSetObj <- .get.mSet(mSetObj);
  
  data <- qs::qread("prenorm.qs");

  cls <- mSetObj$dataSet$prenorm.cls;
  
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
      print(paste("After quantile normalization", constNum, "features with a constant value were found and deleted."));
      data <- data[,!constCol, drop=FALSE];
      colNames <- colnames(data);
      rowNames <- rownames(data);
    }
    rownm<-"Quantile Normalization";
  }else if(rowNorm=="GroupPQN"){
    grp.inx <- cls == ref;
    ref.smpl <- apply(data[grp.inx, , drop=FALSE], 2, mean);
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference group";
  }else if(rowNorm=="SamplePQN"){
    ref.smpl <- data[ref, , drop=FALSE];
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference sample";
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
  
  # use apply will lose dimension info (i.e. row names and colnames)
  rownames(data)<-rowNames;
  colnames(data)<-colNames;
  
  # if the reference by feature, the feature column should be removed, since it is all 1
  if(rowNorm=="CompNorm" && !is.null(ref)){
    inx<-match(ref, colnames(data));
    data<-data[,-inx, drop=FALSE];
    colNames <- colNames[-inx];
  }

  # record row-normed data for fold change analysis (b/c not applicable for mean-centered data)
  row.norm <- as.data.frame(CleanData(data, T, T)); #moved below ratio 
  qs::qsave(row.norm, file="row_norm.qs");
  # this is for biomarker analysis only (for compound concentration data)
  if(ratio){
    min.val <- min(abs(data[data!=0]))/2;
    norm.data <- log2((data + sqrt(data^2 + min.val))/2);
    transnm<-"Log2 Normalization";

    if(ncol(norm.data)>1000){
        mSetObj$dataSet$ratioFilterOpt -> ratioFilterOpt;
        norm.data0 <- extractTopFeatures(norm.data, 1000, ratioFilterOpt);
        ratio.mat <- CalculatePairwiseDiff(norm.data0);        
    } else {
        ratio.mat <- CalculatePairwiseDiff(norm.data);
    }    
    
    fstats <- Get.Fstat(ratio.mat, cls);
    hit.inx <- rank(-fstats) <= ratioNum;  # get top n
    
    ratio.mat <- ratio.mat[, hit.inx, drop=FALSE];
    
    data <- cbind(norm.data, ratio.mat);
    
    colNames <- colnames(data);
    rowNames <- rownames(data);
    mSetObj$dataSet$use.ratio <- TRUE;
    mSetObj$dataSet$proc.ratio <- data;

  }else{
    mSetObj$dataSet$use.ratio <- FALSE;
    # transformation
    # may not be able to deal with 0 or negative values
    if(transNorm=='LogNorm'){
      min.val <- min(abs(data[data!=0]))/10;
      data<-apply(data, 2, LogNorm, min.val);
      transnm<-"Log10 Normalization";
    }else if(transNorm=='Log2Norm'){
      min.val <- min(abs(data[data!=0]))/10;
      data<-apply(data, 2, Log2Norm, min.val);
      transnm<-"Log2 Normalization";
    }else if(transNorm=='SrNorm'){
      min.val <- min(abs(data[data!=0]))/10;
      data<-apply(data, 2, SquareRootNorm, min.val);
      transnm<-"Square Root Transformation";
    }else if(transNorm=='CrNorm'){
      norm.data <- abs(data)^(1/3);
      norm.data[data<0] <- - norm.data[data<0];
      data <- norm.data;
      transnm<-"Cubic Root Transformation";
    }else if(transNorm=='VsnNorm'){
      data <- t(limma::normalizeVSN(t(data)));
      transnm<-"Variance Stabilizing Normalization";
    }else{
      transnm<-"N/A";
    }
  }
  
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
  
  ## drop features whose column-names are NA or literal "NA" ─────
  badCol <- is.na(colnames(data)) | tolower(colnames(data)) == "na"
  if (any(badCol)) {
    data <- data[ , !badCol, drop = FALSE]
    colNames <- colnames(data)           # refresh vector
    msg      <- paste0("Removed ", sum(badCol),
                       " feature(s) whose name is NA after normalization.")
    mSetObj$msgSet$norm.nafeat.msg <- msg
  }

  if(ratio){
    mSetObj$dataSet$ratio <- CleanData(ratio.mat, T, F);

  }

  mSetObj$dataSet$norm <- as.data.frame(data);

  qs::qsave(mSetObj$dataSet$norm, file="complete_norm.qs");
  mSetObj$dataSet$cls <- cls;
  
  mSetObj$dataSet$rownorm.method <- rownm;
  mSetObj$dataSet$trans.method <- transnm;
  mSetObj$dataSet$scale.method <- scalenm;
  mSetObj$dataSet$norm.all <- NULL; # this is only for biomarker ROC analysis

  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
    
    # make sure the data and metadata are in sync 
    my.sync <- .sync.data.metadata(mSetObj$dataSet$norm, mSetObj$dataSet$meta.info);
    mSetObj$dataSet$norm <- my.sync$data;
    mSetObj$dataSet$meta.info <- my.sync$metadata;

    if(is.null(mSetObj$dataSet$prenorm.facA)){
        nfacA <- mSetObj$dataSet$facA;
        nfacB <- mSetObj$dataSet$facB;
    }else{
       nfacA <- mSetObj$dataSet$prenorm.facA;
       nfacB <- mSetObj$dataSet$prenorm.facB;
    }
    
    mSetObj$dataSet$facA <- nfacA;
    mSetObj$dataSet$facB <- nfacB;
    # note, setup time factor
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor and should order first by subject then by each time points
      if(tolower(mSetObj$dataSet$facA.lbl) == "time"){ 
        time.fac <- nfacA;
        exp.fac <- nfacB;
      }else{
        time.fac <- nfacB;
        exp.fac <- nfacA;
      }
      # now make sure time fac is ordered
      lvls <- levels(time.fac);
      time.points <- as.numeric(as.character(lvls));
      ord.lvls <- lvls[order(time.points)];
      time.fac <- ordered(time.fac, levels = ord.lvls);
      mSetObj$dataSet$time.fac <- time.fac;
      mSetObj$dataSet$exp.fac <- exp.fac;
    }
  }

  if(file.exists("limma.sig.qs")){
    file.remove("limma.sig.qs") # so limma is reperformed
  }
  
  return(.set.mSet(mSetObj));
}

#'Row-wise Normalization
#'@description Row-wise norm methods, when x is a row.
#'Normalize by a sum of each sample, assume constant sum (1000).
# Return: normalized data.
#'Options for normalize by sum median, reference sample,
#'reference reference (compound), or quantile normalization
#'@param x Input data to normalize
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'@export
#'
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
  return(t(preprocessCore::normalize.quantiles(t(data), copy=FALSE)));
}

#'Column-wise Normalization
#'@description Column-wise norm methods, when x is a column
#'Options for log, zero mean and unit variance, and
#'several zero mean and variance/SE 
#'@param x Input data
#'@param min.val Input minimum value
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada

# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log10((x + sqrt(x^2 + min.val^2))/2)
}

Log2Norm<-function(x, min.val){
  log2((x + sqrt(x^2 + min.val^2))/2)
}

# square root, tolerant to negative values
SquareRootNorm<-function(x, min.val){
  ((x + sqrt(x^2 + min.val^2))/2)^(1/2);
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
#'@export
#'
PlotNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  #save.image("normsum.RData");
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
  }
  
  mSetObj$imgSet$norm <- imgName
  
  proc.data <- qs::qread("data_proc.qs");

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  layout(matrix(c(1,2,2,2,3,4,4,4), 4, 2, byrow = FALSE))
  
  # since there may be too many compounds, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(ncol(proc.data), sub.num=50);
  namesVec <- colnames(proc.data[,pre.inx, drop=FALSE]);
  
  # only get common ones
  if(anal.type == "roc" & !is.null(mSetObj$dataSet$norm.orig)){
    if(ncol(mSetObj[["dataSet"]][["norm.orig"]])!=ncol(mSetObj[["dataSet"]][["norm"]])){
      # keep using original normalization results to avoid over-written issue
      norm_dt <- mSetObj$dataSet$norm.orig;
    } else {
      norm_dt <- mSetObj$dataSet$norm;
    }
  } else {
    norm_dt <- mSetObj$dataSet$norm;
  }
  nm.inx <- namesVec %in% colnames(norm_dt)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, colnames(norm_dt));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(proc.data[, pre.inx, drop=FALSE], na.rm=T);
  rangex.norm <- range(norm_dt[, norm.inx, drop=FALSE], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-GetVariableLabel(mSetObj$dataSet$type);
  
  # fig 1
  if(anal.type == "roc" & mSetObj$dataSet$roc_cols == 1){
    op<-par(mar=c(4,7,4,0), xaxt="s");
    plot.new()
  }else{
    op<-par(mar=c(4,7,4,0), xaxt="s");
    plot(density(apply(proc.data, 2, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
    mtext("Density", 2, 5);
    mtext("Before Normalization",3, 1)
  }
  
  # fig 2
  op<-par(mar=c(7,7,0,0), xaxt="s");
  boxplot(proc.data[,pre.inx, drop=FALSE], names=namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T, show.names=T);
  mtext(x.label, 1, 5);
  
  # fig 3
  if(anal.type == "roc" & mSetObj$dataSet$roc_cols == 1){
    op<-par(mar=c(4,7,4,2), xaxt="s");
    plot.new()
  }else{
    op<-par(mar=c(4,7,4,2), xaxt="s");
    plot(density(apply(norm_dt, 2, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
    mtext("After Normalization",3, 1);
  }

  # fig 4
  op<-par(mar=c(7,7,0,2), xaxt="s");
  boxplot(norm_dt[,norm.inx, drop=FALSE], names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", horizontal=T, show.names=T);
  mtext(paste("Normalized",x.label),1, 5);
  
  dev.off();
  
  return(.set.mSet(mSetObj));
}


#'Two plot summary plot: Sample View of before and after normalization
#'@description For each plot, the top is a density plot and the bottom is a box plot.
#'@usage PlotSampleNormSummary(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export

PlotSampleNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){

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
  
  proc.data <- qs::qread("data_proc.qs");
  mSetObj$imgSet$summary_norm <-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,2,2,2,3,4,4,4), 4, 2, byrow = FALSE));

  # since there may be too many samples, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(nrow(proc.data), sub.num=50);
  namesVec <- rownames(proc.data[pre.inx, , drop=FALSE]);
  
  # only get common ones
  nm.inx <- namesVec %in% rownames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, rownames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(proc.data[pre.inx, , drop=FALSE], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[norm.inx, , drop=FALSE], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-"Samples";
  
  # fig 1
  op<-par(mar=c(6.5,7,0,0), xaxt="s");
  plot(density(apply(proc.data, 1, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext(x.label, 1, 4);
  mtext("Density", 2, 5);

  # fig 2
  op<-par(mar=c(5.75,8,4,0), xaxt="s");
  boxplot(t(proc.data[pre.inx, , drop=FALSE]), names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext("Before Normalization", 3,1)
  
  # fig 3
  op<-par(mar=c(6.5,7,0,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 1, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext(paste("Normalized",x.label),1, 4)
  
  # fig 4
  op<-par(mar=c(5.75,8,4,2), xaxt="s");
  boxplot(t(mSetObj$dataSet$norm[norm.inx, , drop=FALSE]), names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", ylab="", horizontal=T);
  mtext("After Normalization", 3, 1);
  dev.off();

  return(.set.mSet(mSetObj));
}

#'Update data for filtering
#'@description Function to update the mSetObj after removing features or samples.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
# note: feature.nm.vec, smpl.nm.vec, grp.nm.vec all set up
UpdateData <- function(mSetObj = NA, order.group = FALSE) {

  mSetObj <- .get.mSet(mSetObj)              # fetch current object
  mSetObj$dataSet$edit <- NULL               # reset edit slot

  ## ------------------------------------------------------------------
  ## 0)  choose the working abundance table + class vector
  ## ------------------------------------------------------------------
    data <- qs::qread("data_proc.qs")
    cls  <- mSetObj$dataSet$proc.cls

  print(sum(is.na(colnames(data))))
  print("missing rownames");

  feat.hit.inx <- colnames(data) %in% feature.nm.vec        # feature.nm.vec: to delete
  data <- CleanDataMatrix(data[, !feat.hit.inx, drop = FALSE])


  smpl.hit.inx <- rownames(data) %in% smpl.nm.vec           # smpl.nm.vec: to delete
  data <- CleanDataMatrix(data[!smpl.hit.inx, , drop = FALSE])
  cls  <- as.factor(as.character(cls[!smpl.hit.inx]))

  if (length(grp.nm.vec) == 1) {
    if (grp.nm.vec != "") {
      grp.hit.inx <- cls %in% grp.nm.vec
      data <- CleanDataMatrix(data[grp.hit.inx, , drop = FALSE])
      cls  <- droplevels(factor(cls[grp.hit.inx]))
    }
  } else {
    grp.hit.inx <- cls %in% grp.nm.vec
    data <- CleanDataMatrix(data[grp.hit.inx, , drop = FALSE])
    cls  <- droplevels(factor(cls[grp.hit.inx]))
  }

  if (order.group) {
    cls <- ordered(cls, levels = grp.nm.vec)
  }

  qc.left    <- any(grepl("^\\s*QC",    rownames(data), ignore.case = TRUE))
  blank.left <- any(grepl("^\\s*BLANK", rownames(data), ignore.case = TRUE))
  if (!(qc.left || blank.left)) {
    mSetObj$msgSet$qc.replace.msg <- NULL
  }


  keep.smp  <- rownames(data)
  keep.feat <- colnames(data)

  if (file.exists("preproc.qs")) {
    preproc <- qs::qread("preproc.qs")
    preproc <- preproc[intersect(keep.smp,  rownames(preproc)),
                       intersect(keep.feat, colnames(preproc)),
                       drop = FALSE]
    qs::qsave(preproc, "preproc.qs")
  }

  if (!is.null(mSetObj$dataSet$meta.info)) {
    meta <- mSetObj$dataSet$meta.info

    ## keep only surviving rows, preserve order exactly as in data
    meta <- meta[intersect(rownames(meta), keep.smp), , drop = FALSE]
    meta <- meta[match(keep.smp, rownames(meta)), , drop = FALSE]

    ## re-drop unused factor levels
    meta[] <- lapply(meta, function(x)
      if (is.factor(x)) droplevels(x[ , drop = TRUE]) else x)

    mSetObj$dataSet$meta.info <- meta
    #print("dim(meta)");
    #print(dim(meta));
  }

  #mSetObj$dataSet$edit     <- data
  qs::qsave(data,"data.edit.qs");
  mSetObj$dataSet$edit.cls <- cls
  AddMsg("Successfully updated the data & metadata!")

  # save(mSetObj, file = "mSetObj__UpdateData.rda")

  if (.on.public.web) {
    .set.mSet(mSetObj)
    return(length(levels(mSetObj$dataSet$edit.cls)))
  } else {
    return(.set.mSet(mSetObj))
  }
}



# should always init (new or overwrite previous prenorm object)
# note in right order that dataSet$edit will always performed using dataSet$filt (if it exists)
# note dataSet$filt can be re-performed after dataSet$edit during analysis
# need to make sure prenorm created using the latest information (based on both)

#'Prepare data for normalization
#'@description Function should always be initialized (new or overwrite previous prenorm object).
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@import qs
#'@export


PreparePrenormData <- function(mSetObj=NA){
    print("save data - prenorm ....");
    .prepare.prenorm.data(mSetObj);
}

.prepare.prenorm.data <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  if(file.exists("data.edit.qs")){
    prenorm <- qs::qread("data.edit.qs");;
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$edit.cls;
  }else {
    prenorm <- qs::qread("data_proc.qs");
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
  }

  # make sure the meta.info is synchronized with data
  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
    my.sync <- .sync.data.metadata(prenorm, mSetObj$dataSet$meta.info);
    prenorm <- my.sync$data;
    mSetObj$dataSet$meta.info <- my.sync$metadata;
  }

  qs::qsave(prenorm, "prenorm.qs");
  mSetObj$dataSet$prenorm.smpl.nms <- rownames(prenorm);
  mSetObj$dataSet$prenorm.feat.nms <- colnames(prenorm);
  .set.mSet(mSetObj)
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

# get the dropdown list for sample normalization view
GetPrenormSmplNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);  
  smp.nms <- mSetObj$dataSet$prenorm.smpl.nms;
  if(is.null(smp.nms)){smp.nms <- mSetObj[["dataSet"]][["url.smp.nms"]]}
  return(smp.nms);
}

GetPrenormFeatureNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(length(mSetObj$dataSet$prenorm.feat.nms));
}

GetPrenormFeatureNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$prenorm.feat.nms);
}

ValidateFeatureName<- function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  if(nm %in% mSetObj$dataSet$prenorm.feat.nms){
    return(1);
  }
  return(0);
}

GetPrenormClsNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(levels(mSetObj$dataSet$prenorm.cls));
}

########## Utility Functions ###############
GetRandomSubsetIndex<-function(total, sub.num = 50){
  if(total < sub.num){
    1:total;
  }else{
    sample(1:total, sub.num);
  }
}

# Test if data require genefilter version 
# if so, then microservice will be used
RequireFastUnivTests <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);

  if(ncol(mSetObj$dataSet$norm) < 1000){
        return(FALSE);
  }else{
        return(TRUE);
  }
}

UpdateLoadingCmpd<-function(mSetObj=NA, cmpdNm){
  mSetObj <- .get.mSet(mSetObj);
  # need to record the clicked compounds
  mSetObj$custom.cmpds <- c(mSetObj$custom.cmpds, cmpdNm);
  return(.set.mSet(mSetObj));
}
