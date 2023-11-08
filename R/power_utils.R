#'Function for power analysis
#'@description Perform power analysis, requires the SSPA R package. 
#'@usage InitPowerAnal(mSetObj, clsOpts)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param clsOpts For data with >2 groups, specify the two classes on which to perform power analysis, 
#'otherwise for data with 2 groups, "NA" will automatically select the 2 groups.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
InitPowerAnal <- function(mSetObj=NA, clsOpts){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(clsOpts == "NA"){
    grp.nms <- levels(mSetObj$dataSet$cls)[1:2];
  }else{
    grp.nms <- strsplit(clsOpts, " vs. ", fixed=TRUE)[[1]];
  }
  inx1 <- which(mSetObj$dataSet$cls==grp.nms[1]);
  inx2 <- which(mSetObj$dataSet$cls==grp.nms[2]);
  stats <- apply(as.matrix(mSetObj$dataSet$norm), 2, function(x) {
    tmp <- try(t.test(x[inx1], x[inx2], paired = mSetObj$dataSet$paired, var.equal = T));
    if(class(tmp) == "try-error") {
      return(NA);
    }else{
      return(tmp$statistic);
    }
  })
  
  stats <- stats[!is.na(stats)];
  n1 <- length(inx1);
  n2 <- length(inx2);
  pdD <- SSPA_pilotData(statistics = stats, 
                   samplesize = sqrt(n1+n2), 
                   distribution="t",
                   df=n1+n2-2);
  mSetObj$analSet$power <- list(pdD = pdD);
  return(.set.mSet(mSetObj));
}

#'Plot power statistics
#'@description Create plot for power statistics
#'@usage PlotPowerStat(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Specify the name to save the image as.
#'@param format Specify the format of the image to save it as, either "png" or "pdf"
#'@param dpi Specify the dots-per-inch (dpi). By default it is 72, for publications
#'the recommended dpi is 300.
#'@param width Specify the width of the image. NA or 0 specifies a width of 10, otherwise input a chosen width. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import lattice

PlotPowerStat <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 10;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$powerstat<-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(.on.public.web){
    load_lattice()
  }
  SSPA_plot(mSetObj$analSet$power$pdD);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Retrieve sample size ladder
#'@description Return sample size ladder, used in higher functions
#'@param maxNum Numeric
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetSampleSizeLadder <- function(maxNum){
  Jpred <- c(3, 6, 10, 16, 24, 40, 60, 100, 150, seq(200, 1000, 100));
  inx <- which(Jpred == min(Jpred[Jpred>=maxNum]))
  return(Jpred[1:inx]);
}

#'Perform power profiling
#'@description Perform power profiling of data
#'@usage PerformPowerProfiling(mSetObj=NA, fdr.lvl, smplSize)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fdr.lvl Specify the false-discovery rate level.
#'@param smplSize Specify the maximum sample size, the number must be between 60-1000. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformPowerProfiling <- function(mSetObj=NA, fdr.lvl, smplSize){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- round(length(mSetObj$analSet$power$pdD@statistics)/2);
  ssD <- sampleSize(mSetObj$analSet$power$pdD, method="congrad", control=list(from=-6, to=6, resolution=res));
  Jpred <- GetSampleSizeLadder(smplSize);
  N <- sqrt(Jpred/2);
  
  pi0 <- ssD@pi0;
  if(fdr.lvl >= pi0){
    fdr.lvl <- signif(pi0-pi0/10, 3);
  }

  pwrD <- predictpower(ssD, samplesizes=N, alpha=fdr.lvl);
  mSetObj$analSet$power$fdr.lvl <- fdr.lvl;
  mSetObj$analSet$power$smplSize <- smplSize;
  mSetObj$analSet$power$ssD <- ssD;
  mSetObj$analSet$power$Jpred <- Jpred;
  mSetObj$analSet$power$pwrD <- pwrD;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(fdr.lvl);
  }
  return(.set.mSet(mSetObj));
}

#'Plot power profile
#'@description Plot power profile, specifying FDR level and sample size. It will 
#'return the image as well as the predicted power at various sample sizes. 
#'@usage PlotPowerProfile(mSetObj=NA, fdr.lvl, smplSize, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fdr.lvl Specify the false-discovery rate level.
#'@param smplSize Specify the maximum sample size, the number must be between 60-1000. 
#'@param imgName Specify the name to save the image as.
#'@param format Specify the format of the image to save it as, either "png" or "pdf".
#'@param dpi Specify the dots-per-inch (dpi). By default it is 72, for publications
#'the recommended dpi is 300.
#'@param width Specify the width of the image. NA specifies a width of 9, 0 specifies a width
#'of 7, otherwise input a chosen width. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotPowerProfile <- function(mSetObj=NA, fdr.lvl, smplSize, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  Jpred <- GetSampleSizeLadder(smplSize);
  N <- sqrt(Jpred/2);
  # SSPA::predictpower
  pwrD <- predictpower(mSetObj$analSet$power$ssD, samplesizes=N, alpha=fdr.lvl)
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  
  h <- w*(6/9);
  
  mSetObj$imgSet$powerprofile<-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(Jpred, pwrD, type="n", ylim=c(0,1), ylab="Predicted power", xlab="Sample Size (per group)");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  lines(Jpred, pwrD, lwd=4, col="orange");
  points(Jpred, pwrD, pch=17);
  dev.off();
  
  power.mat <- signif(cbind(Jpred, pwrD), 5);
  colnames(power.mat) <- c("Sample Size (per group)", "Predicted power");
  fast.write.csv(power.mat, file="predicted_sample_powers.csv");
  mSetObj$analSet$power.mat <- power.mat;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(pwrD);
  }
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetPowerValuesX <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$power.mat[,1]);
}

