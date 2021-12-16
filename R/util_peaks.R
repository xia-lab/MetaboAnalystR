#' @title my.parse.peaklist 
#' @name my.parse.peaklist 
#' @description Read peak list files. This function reads peak list files and fills the data into a dataSet object.  
#' For NMR peak lists, the input should be formatted as two-columns containing numeric values (ppm, int).
#' Further, this function will change ppm to mz, and add a dummy 'rt'.
#' For MS peak data, the lists can be formatted as two-columns (mz, int), in which case the function will add a dummy 'rt', or
#' the lists can be formatted as three-columns (mz, rt, int).
#' @param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#' @param foldername Name of the folder containing the NMR or MS peak list files to read.
#' @author Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @import qs
my.parse.peaklist<-function(mSetObj=NA, foldername="upload"){
  mSetObj <- .get.mSet(mSetObj);

  if(.on.public.web){
    dyn.load(.getDynLoadPath());
  }
  
  msg <- c("The uploaded files are peak lists and intensities data.");
  
  # the "upload" folder should contain several subfolders (groups)
  # each of the subfolder contains samples (.csv files)
  files<-dir(foldername, pattern=".[Cc][Ss][Vv]$", recursive=T, full.names = TRUE)
  if (length(files) == 0) {
    AddErrMsg("No peak list files (.csv) were found.");
    return(0);
  }
  
  snames <- gsub("\\.[^.]*$", "", basename(files));
  msg<-c(msg, paste("A total of ", length(files), "samples were found."));
  
  sclass <- gsub("^\\.$", "sample", dirname(files));
  
  scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "", fixed=TRUE);
  scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp));
  i <- 1
  while(all(scomp[i,1] == scomp[i,-1]) && i < nrow(scomp)){
    i <- i + 1;
  }
  i <- min(i, tail(c(0, which(scomp[1:i,1] == .Platform$file.sep)), n = 1) + 1)
  if (i > 1 && i <= nrow(scomp)){
    sclass <- substr(sclass, i, max(nchar(sclass)))
  }
  
  # some sanity check before proceeds
  sclass <- as.factor(sclass);
  if(length(levels(sclass))<2){
    AddErrMsg("You must provide classes labels (at least two classes)!");
    return(0);
  }
  
  # check for class labels at least three replicates per class
  if(min(table(sclass)) < 3){
    AddErrMsg("At least three replicates are required in each group!");
    return(0);
  }
  
  # check for unique sample names
  if(length(unique(snames))!=length(snames)){
    AddErrMsg("Duplcate sample names are not allowed!");
    dup.nm <- paste(snames[duplicated(snames)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  # change sample names to numbers
  samp.num<-seq(1:length(snames));
  names(samp.num)<-snames;
  
  # create a matrix "all.peaks" which is compatible with the xcmsSet@peaks matrix, so that the grouping algorithm can be used directly
  # the matrix should have "mz", "rt", "into", "sample" - 4 columns used for grouping
  # check 2 or 3 column
 
  ############## use try block to catch any error ##############
  pks<- .readDataTable(files[1]);
  if(class(pks) == "try-error") {
    AddErrMsg("The CSV file is not formatted correctly!");
    return(0);
  };
  pks <- as.matrix(pks);
  ########################################################
  
  n.col<-ncol(pks);
  if(n.col==2){
    add=TRUE;
  }else if(n.col==3){
    add=FALSE;
  }else{
    AddErrMsg("The peak list file can only contain 2 or 3 columns.");
    return(0);
  }
  
  all.peaks<-NULL;
  
  for(i in 1:length(files)){
    print(files[i]);
    pks<- as.matrix(.readDataTable(files[i]));
    if(ncol(pks)!=n.col){
      AddErrMsg("The number of columns in each file are not the same!");
      return(0);
    }
    
    if(add){ # NMR ppm+int or MS mz+int
      pks<-cbind(pks[,1], 1000, pks[,2],samp.num[i]);
    }else{
      pks<-cbind(pks,samp.num[i]);
    }
    all.peaks<-rbind(all.peaks, pks);
  }


  # make sure all values are numeric, sometimes users give other text values, need to exclude them
  all.peaks <- apply(all.peaks, 2, as.numeric);
  gd.inx <- complete.cases(all.peaks);
  all.peaks <- all.peaks[gd.inx,]
   
  if(sum(!gd.inx) > 0){
    msg<-c(msg, paste("<font color='red'>A total of", sum(!gd.inx), "peaks were excluded due to non-numeric values. </font>" ));
  }
  msg<-c(msg, paste("These samples contain a total of ", dim(all.peaks)[1], "peaks," ));
  msg<-c(msg, paste("with an average of ", round(dim(all.peaks)[1]/length(files), 1), "peaks per sample" ));
  
  colnames(all.peaks)<-c("mz","rt","int","sample");
  
  peakSet<-list(
    peaks = all.peaks,
    ncol = n.col,
    sampclass = sclass,
    sampnames = snames
  );

  qs::qsave(peakSet, "peakSet.qs");
  mSetObj$msgSet$read.msg <- msg;
  return(.set.mSet(mSetObj));
}


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
  
  colnames(values) <- peakSet$sampnames;
  
  if(peakSet$ncol==2){
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5));
  }else{
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5), "/", round(groupmat[,paste("rt", "med", sep="")],2), sep="");
    mSetObj$dataSet$three.col <- TRUE;
  }
  
  #mSetObj$dataSet$orig <- t(values);
  qs::qsave(t(values), file="data_orig.qs");
  mSetObj$msgSet$proc.msg <- msg
  mSetObj$dataSet$orig.cls <- as.factor(peakSet$sampclass);
  mSetObj$dataSet$type.cls.lbl <- class(peakSet$sampclass);
  return(.set.mSet(mSetObj));
}

########## Utility Functions ##############
#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param y Input peaks
#'@param istart Performs which.max on y
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
descendMin <- function(y, istart = which.max(y)) {
   
  if (!is.double(y)) y <- as.double(y)
  unlist(.C("DescendMin",
            y,
            length(y),
            as.integer(istart-1),
            ilower = integer(1),
            iupper = integer(1))[4:5]) + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param x Input the data
#'@param values Input the values 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
findEqualGreaterM <- function(x, values) {
  
  if (!is.double(x)) x <- as.double(x)
  if (!is.double(values)) values <- as.double(values)
  .C("FindEqualGreaterM",
     x,
     length(x),
     values,
     length(values),
     index = integer(length(values)))$index + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param m Peaks
#'@param order Performs seq(length = nrow(m))
#'@param xdiff Default set to 0
#'@param ydiff Default set to 0
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
rectUnique <- function(m, order = seq(length = nrow(m)), xdiff = 0, ydiff = 0) {
  
  nr <- nrow(m)
  nc <- ncol(m)
  if (!is.double(m))
    m <- as.double(m)
  .C("RectUnique",
     m,
     as.integer(order-1),
     nr,
     nc,
     as.double(xdiff),
     as.double(ydiff),
     logical(nrow(m)))[[7]]
}