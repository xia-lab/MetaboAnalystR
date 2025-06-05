
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
