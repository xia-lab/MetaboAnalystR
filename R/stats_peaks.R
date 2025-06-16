#' @title Group peak list
#'@description Group peaks from the peak list based on position
#'using the XCMS grouping algorithm (align peaks wrt, rt, and mz).
#'For NMR peaks, need to change ppm -> mz and add dummy rt.
#'If the data is 2-column MS, first need to add dummy rt.
#'If the data is 3-column MS, the data can be used directly.
#'The default mzwid for MS is 0.25 m/z, and for NMR is 0.03 ppm.
#'The default bw is 30 for LCMS, and 5 for GCMS.
#'@usage GroupPeakList(mSetObj=NA, mzwid, bw, minfrac, minsamp, max)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param mzwid, define the width of overlapping m/z slices to use for creating peak density chromatograms
#'and grouping peaks across samples
#'@param bw, define the bandwidth (standard deviation or half width at half maximum) of gaussian smoothing
#'kernel to apply to the peak density chromatogram
#'@param minfrac, define the minimum fraction of samples necessary in at least one of the sample groups for
#'it to be a valid group
#'@param minsamp, define the minimum number of samples necessary in at least one of the sample groups for 
#'it to be a valid group 
#'@param max, define the maximum number of groups to identify in a single m/z slice
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
GroupPeakList <- function(mSetObj=NA, mzwid = 0.25, bw = 30, minfrac = 0.5, minsamp = 1, max = 50) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- qs::qread("peakSet.qs");
  samples <- peakSet$sampnames;
  classlabel <- peakSet$sampclass;
  classnames <- levels(classlabel)
  
  classlabel <- as.vector(unclass(classlabel))
  classnum <- integer(max(classlabel))
  for (i in seq(along = classnum)){
    classnum[i] <- sum(classlabel == i)
  }
  
  peakmat <- peakSet$peaks;
  porder <- order(peakmat[,"mz"]);
  peakmat <- peakmat[porder,,drop=F]
  rownames(peakmat) <- NULL
  retrange <- range(peakmat[,"rt"])
  
  minpeakmat <- min(classnum)/2
  
  mass <- seq(peakmat[1,"mz"], peakmat[nrow(peakmat),"mz"] + mzwid, by = mzwid/2)
  masspos <- findEqualGreaterM(peakmat[,"mz"], mass)
  
  groupmat <- matrix(nrow = 512, ncol = 7 + length(classnum))
  groupindex <- vector("list", 512)
  
  endidx <- 0
  num <- 0
  gcount <- integer(length(classnum))
  for (i in seq(length = length(mass)-2)) {
    startidx <- masspos[i]
    endidx <- masspos[i+2]-1
    if (endidx - startidx + 1 < minpeakmat)
      next
    speakmat <- peakmat[startidx:endidx,,drop=FALSE]
    den <- density(speakmat[,"rt"], bw, from = retrange[1]-3*bw, to = retrange[2]+3*bw)
    maxden <- max(den$y)
    deny <- den$y
    gmat <- matrix(nrow = 5, ncol = 2+length(classnum))
    snum <- 0
    while (deny[maxy <- which.max(deny)] > maxden/20 && snum < max) {
      grange <- descendMin(deny, maxy)
      deny[grange[1]:grange[2]] <- 0
      gidx <- which(speakmat[,"rt"] >= den$x[grange[1]] & speakmat[,"rt"] <= den$x[grange[2]])
      gnum <- classlabel[unique(speakmat[gidx,"sample"])]
      for (j in seq(along = gcount))
        gcount[j] <- sum(gnum == j)
      if (! any(gcount >= classnum*minfrac & gcount >= minsamp))
        next
      snum <- snum + 1
      num <- num + 1
      ### Double the size of the output containers if they're full
      if (num > nrow(groupmat)) {
        groupmat <- rbind(groupmat, matrix(nrow = nrow(groupmat), ncol = ncol(groupmat)))
        groupindex <- c(groupindex, vector("list", length(groupindex)))
      }
      groupmat[num, 1] <- median(speakmat[gidx, "mz"])
      groupmat[num, 2:3] <- range(speakmat[gidx, "mz"])
      groupmat[num, 4] <- median(speakmat[gidx, "rt"])
      groupmat[num, 5:6] <- range(speakmat[gidx, "rt"])
      groupmat[num, 7] <- length(gidx)
      groupmat[num, 7+seq(along = gcount)] <- gcount
      groupindex[[num]] <- sort(porder[(startidx:endidx)[gidx]])
    }
  }
  colnames(groupmat) <- c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
                          "npeaks", classnames)
  
  groupmat <- groupmat[seq(length = num),]
  groupindex <- groupindex[seq(length = num)]
  
  # Remove groups that overlap with more "well-behaved" groups
  numsamp <- rowSums(groupmat[,(match("npeaks", colnames(groupmat))+1):ncol(groupmat),drop=FALSE])
  uorder <- order(-numsamp, groupmat[,"npeaks"])
  uindex <- rectUnique(groupmat[,c("mzmin","mzmax","rtmin","rtmax"),drop=FALSE],uorder)
  
  peakSet$groups <- groupmat[uindex,];
  peakSet$groupidx<- groupindex[uindex];
  qs::qsave(peakSet, "peakSet.qs");
  return(.set.mSet(mSetObj));
}

#'Set peak list group values
#'@param mSetObj Input name of mSetObj, the data used is the nmr.xcmsSet object
#'@import qs
#'@export
#'
SetPeakList.GroupValues <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- qs::qread("peakSet.qs");
  msg <- mSetObj$msgSet$peakMsg;
  
  peakmat <- peakSet$peaks;
  groupmat <- peakSet$groups;
  groupindex <- peakSet$groupidx;
  
  sampnum <- seq(length = length(peakSet$sampnames))
  intcol <- match("int", colnames(peakmat))
  sampcol <- match("sample", colnames(peakmat))
  
  # row is peak, col is sample
  values <- matrix(nrow = length(groupindex), ncol = length(sampnum))
  
  for (i in seq(along = groupindex)) {
    # for each group, replace multiple peaks from the same sample by their sum
    for(m in sampnum){
      samp.inx<-which(peakmat[groupindex[[i]], sampcol]==m)
      if(length(samp.inx)>0){
        values[i, m] <- sum(peakmat[groupindex[[i]][samp.inx], intcol]);
      }else{
        values[i, m] <- NA;
      }
    }
  }
  
  msg<-c(msg, paste("A total of", length(groupindex), "peak groups were formed. "));
  msg<-c(msg, paste("Peaks of the same group were summed if they are from one sample. "));
  msg<-c(msg, paste("Peaks must appear in at least half of the samples in at least one group to be included."));
  
  smpl.nms <- colnames(values) <- peakSet$sampnames;
  
  if(peakSet$ncol==2){
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5));
  }else{
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5), "/", round(groupmat[,paste("rt", "med", sep="")],2), sep="");
    mSetObj$dataSet$three.col <- TRUE;
  }
  
  #mSetObj$dataSet$orig <- t(values);
  qs::qsave(t(values), file="data_orig.qs");
  mSetObj$msgSet$proc.msg <- msg;
  mSetObj$dataSet$orig.cls <- as.factor(peakSet$sampclass);
  mSetObj$dataSet$type.cls.lbl <- class(peakSet$sampclass);

  # syn with other table input
  url.smp.nms <- CleanNames(smpl.nms);
  names(url.smp.nms) <- smpl.nms;
  ord.inx <- order(mSetObj$dataSet$orig.cls);
  mSetObj$dataSet$orig.cls <- mSetObj$dataSet$orig.cls[ord.inx];
  mSetObj$dataSet$url.smp.nms <- url.smp.nms[ord.inx];
  return(.set.mSet(mSetObj));
}


