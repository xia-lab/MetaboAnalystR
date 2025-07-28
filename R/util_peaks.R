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
  files<-dir(foldername, pattern=".[Cc][Ss][Vv]$", recursive=T, full.name=TRUE)
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
    #print(files[i]);
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