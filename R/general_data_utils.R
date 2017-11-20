# internal variables and functions not to be modified by users
.on.public.web <- FALSE; # only TRUE when on metaboanalyst web server

.set.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    mSet <<- mSetObj;
    return (1);
  }else{
    mSetObj;
  }
}

.get.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    return(mSet)
  }else{
    return(mSetObj);
  }
}

#'Constructs a dataSet object for storing data 
#'@description This functions handles the construction of a dataSet object for storing data for further processing and analysis.
#'It is necessary to utilize this function to specify to MetaboAnalystR the type of data and the type of analysis you will perform. 
#'@usage InitDataObjects(dataType, analType, paired=F)
#'@param dataType The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data)
#'@param analType Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, 
#'cmpdmap, smpmap, or inmex
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

InitDataObjects <- function(data.type, anal.type, paired=FALSE){
  
  mSetObj <- list();
  dataSet <- list();
  dataSet$type <- data.type;
  dataSet$design.type <- "regular"; # one factor to two factor
  dataSet$cls.type <- "disc"; # default until specified otherwise
  dataSet$format <- "rowu";
  dataSet$paired <- paired;
  
  analSet <- list();
  analSet$type <- anal.type;
  
  mSetObj$dataSet <- dataSet;
  mSetObj$analSet <- analSet;
  mSetObj$imgSet <- list();
  mSetObj$msgSet <- list();
  mSetObj$cmdSet <- vector(mode="character");
  
  # plotting required by all
  library(Cairo)  
  CairoFonts(regular="Arial:style=Regular",bold="Arial:style=Bold",italic="Arial:style=Italic",bolditalic = "Arial:style=Bold Italic",symbol = "Symbol")
  
  print("R objects intialized ...");
  
  return(.set.mSet(mSetObj));
}

#'For two factor time series only
#'@description For two factor time series only
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetDesignType <-function(mSetObj=NA, design){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$design.type <- tolower(design);
  return(.set.mSet(mSetObj));
}

#'@export
RecordRCommand <- function(mSetObj=NA, cmd){
  write(cmd, file = "Rhistory.R", append = TRUE);
  mSetObj <- .get.mSet(mSetObj); 
  mSetObj$cmdSet <- c(mSetObj$cmdSet, cmd);
  return(.set.mSet(mSetObj));
}

#'@export
GetRCommandHistory <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj); 
  return(mSetObj$cmdSet);
}

GetRCMD<-function(mSetObj=NA, pattern){
  mSetObj <- .get.mSet(mSetObj); 
  
  rhist <- mSetObj$cmdSet
  
  all.matches<-grep(regexp, rhist, value=T);
  if(length(all.matches)==0){
    return(NULL);
  }else{
    # only return the last command
    return(all.matches[length(all.matches)]);
  }
}

DeDuplicateRCommandHistory <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj); 
  
  #data processing
  
  init <- GetRCMD("mSet<-InitDataObjects")
  readdata <- GetRCMD("mSet<-Read.TextData")
  unzipmsdata <- GetRCMD("mSet<-UnzipUploadedFile")
  readmsdata <- GetRCMD("mSet<-Read.MSspec")
  mscorr <- GetRCMD("mSet<-MSspec.rtCorrection")
  mspeaks <- GetRCMD("mSet<-MSspec.fillPeaks")
  msmatrix <- GetRCMD("mSet<-SetupMSdataMatrix")
  specproc <- GetRCMD("mSet<-IsSpectraProcessingOK")
  readpeak <- GetRCMD("mSet<-Read.PeakList")
  grouppeak <- GetRCMD("mSet<-GroupPeakList")
  setpeak <- GetRCMD("mSet<-SetPeakList.GroupValues")
  sanity <- GetRCMD("mSet<-SanityCheckData")
  replace <- GetRCMD("mSet<-ReplaceMin")
  norm <- GetRCMD("mSet<-Normalization")
  plotnorm <- GetRCMD("mSet<-PlotNormSummary")
  plotsnorm <- GetRCMD("mSet<-PlotSampleNormSummary")

  #stat analysis
  
  fcanal <- GetRCMD("mSet<-FC.Anal.unpaired")
  fcplt <- GetRCMD("mSet<-PlotFC")
  ttanal <- GetRCMD("mSet<-Ttests.Anal")
  ttplt <- GetRCMD("mSet<-PlotTT")
  volcano <- GetRCMD("mSet<-Volcano.Anal")
  plotvolcano <- GetRCMD("mSet<-PlotVolcano")
  anova <- GetRCMD("mSet <- ANOVA.Anal")
  anovaplot <- GetRCMD("mSet <- PlotANOVA")
  corr <- GetRCMD("mSet<-PlotCorrHeatMap")
  pattern <- GetRCMD("mSet<-FeatureCorrelation")
  plotpattern <- GetRCMD("mSet<-PlotCorr(mSet")
  pcaanal <- GetRCMD("mSet<-PCA.Anal")
  pcaplot <- GetRCMD("mSet<-PlotPCAPairSummary")
  pcascree <- GetRCMD("mSet<-PlotPCAScree")
  pca2D <- GetRCMD("mSet<-PlotPCA2DScore")
  pca3d <- GetRCMD("mSet<-PlotPCA3DScore")
  pcaload <- GetRCMD("mSet<-PlotPCALoading")
  pcabiplot <- GetRCMD("mSet<-PlotPCABiplot")
  plsranal <- GetRCMD("mSet<-PLSR.Anal")
  plsrsum <- GetRCMD("mSet<-PlotPLSPairSummary")
  pls2d <- GetRCMD("mSet<-PlotPLS2DScore")
  plsload <- GetRCMD("mSet<-PlotPLSLoading")
  plscv <- GetRCMD("mSet<-PLSDA.CV")
  plsimp <- GetRCMD("mSet<-PlotPLS.Imp")
  plsperm <- GetRCMD("mSet<-PLSDA.Permut")
  plotplsperm <- GetRCMD("mSet<-PlotPLS.Permutation")
  splsanal <- GetRCMD("mSet<-SPLSR.Anal")
  splspair <- GetRCMD("mSet<-PlotSPLSPairSummary")
  spls2d <- GetRCMD("mSet<-PlotSPLS2DScore")
  spls3d <- GetRCMD("mSet<-PlotSPLS3DScore")
  splsload <- GetRCMD("mSet<-PlotSPLSLoading")
  splsclass <- GetRCMD("mSet<-PlotSPLSDA.Classification")
  oplsanal <- GetRCMD("mSet<-OPLSR.Anal")
  opls2d <- GetRCMD("mSet<-PlotOPLS2DScore")
  oplssplot <- GetRCMD("mSet<-PlotOPLS.Splot")
  oplsmodel <- GetRCMD("mSet<-PlotOPLS.MDL")
  oplsperm <- GetRCMD("mSet<-PlotOPLS.Permutation")
  samanal <- GetRCMD("mSet<-SAM.Anal")
  sammat <- GetRCMD("mSet<-SetSAMSigMat")
  samfdr <- GetRCMD("mSet<-PlotSAM.FDR")
  samcmpd <- GetRCMD("mSet<-PlotSAM.Cmpd")
  ebamanal <- GetRCMD("mSet<-EBAM.A0.Init")
  ebamplot <- GetRCMD("mSet<-PlotEBAM.A0")
  ebamcmpd <- GetRCMD("mSet<-EBAM.Cmpd.Init")
  ebammat <- GetRCMD("mSet<-SetEBAMSigMat")
  ebamplotcmpd <- GetRCMD("mSet<-PlotEBAM.Cmpd")
  hicl <- GetRCMD("mSet<-PlotHCTree")
  heatmap <- GetRCMD("mSet<-PlotHeatMap")
  kmeans <- GetRCMD("mSet<-Kmeans.Anal")
  kmeansplot <- GetRCMD("mSet<-PlotKmeans")
  som <- GetRCMD("mSet<-SOM.Anal")
  plotsom <- GetRCMD("mSet<-PlotSOM")
  rfanal <- GetRCMD("mSet<-RF.Anal")
  rfplot <- GetRCMD("mSet<-PlotRF.Classify")
  rfvip <- GetRCMD("mSet<-PlotRF.VIP")
  rfout <- GetRCMD("mSet<-PlotRF.Outlier")
  rsvm <- GetRCMD("mSet<-RSVM.Anal")
  rsvmplot <- GetRCMD("mSet<-PlotRSVM.Classification")
  rsvmcmptplot <- GetRCMD("mSet<-PlotRSVM.Cmpd")

  # continue...
  
}

#'Constructor to read uploaded CSV or TXT files into the dataSet object
#'@description This function handles reading in CSV or TXT files and filling in the dataSet object created using "InitDataObjects". 
#'@usage Read.TextData(mSetObj=NA, filePath, format, lbl.type)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param filePath Input the path name for the CSV/TXT files to read.
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@param lbl.type Specify the data label type, either discrete (disc) or continuous (cont).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Read.TextData <- function(mSetObj=NA, filePath, format="rowu", lbl.type="disc"){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cls.type <- lbl.type;
  mSetObj$dataSet$format <- format;
  
  dat <- .readDataTable(filePath);
  
  # try to guess column numers and class labels (starts with #) from the top 20 rows
  if(class(dat) == "try-error") {
    AddErrMsg(mSetObj, "Data format error. Failed to read in the data!");
    AddErrMsg(mSetObj, "Please check the followings: ");
    AddErrMsg(mSetObj, "Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg(mSetObj, "We recommend using a combination of English letters, underscore, and numbers for naming purpose");
    AddErrMsg(mSetObj, "Make sure sample names and feature (peak, compound) names are unique;");
    AddErrMsg(mSetObj, "Missing values should be blank or NA without quote.");
    return(0);
  }
  
  if(ncol(dat) == 1){
    AddErrMsg(mSetObj, "Error: Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg(mSetObj, "Please also check the followings: ");
    AddErrMsg(mSetObj, "Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg(mSetObj, "We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg(mSetObj, "Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg(mSetObj, "Missing values should be blank or NA without quote.");
    return(0);
  }
  
  msg <- NULL;
  
  if(substring(format,4,5)=="ts"){
    # two factor time series data
    if(substring(format,1,3)=="row"){ # sample in row
      msg<-c(msg, "Samples are in rows and features in columns");
      smpl.nms <-dat[,1];
      all.nms <- colnames(dat);
      facA.lbl <- all.nms[2];
      cls.lbl <-facA <- dat[,2]; # default assign facA to cls.lbl in order for one-factor analysis
      facB.lbl <- all.nms[3];
      facB <- dat[,3];
      conc <- dat[,-c(1:3)];
      var.nms <- colnames(conc);
    }else{ # sample in col
      msg<-c(msg, "Samples are in columns and features in rows.");
      all.nms <- dat[,1];
      facA.lbl <- all.nms[1];
      cls.lbl <- facA <- dat[1,-1];
      facB.lbl <- all.nms[2];
      facB <- dat[2,-1];
      var.nms <- dat[-c(1:2),1];
      conc<-t(dat[-c(1:2),-1]);
      smpl.nms <- rownames(conc);
    }
    
    facA <- as.factor(as.character(facA));
    facB <- as.factor(as.character(facB));
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor
      if(!(tolower(facA.lbl) == "time" | tolower(facB.lbl) == "time")){
        AddErrMsg(mSetObj, "No time points found in your data");
        AddErrMsg(mSetObj, "The time points group must be labeled as <b>Time</b>");
        return(0);
      }
    }
  }else{
    if(substring(format,1,3)=="row"){ # sample in row
      msg <- c(msg, "Samples are in rows and features in columns");
      smpl.nms <-dat[,1];
      dat[,1] <- NULL;
      if(lbl.type == "qc"){
        rownames(dat) <- smpl.nms;
        mSetObj$dataSet$orig <- dat;
        mSetObj$dataSet$cmpd <- colnames(dat);
        return(1);
      }
      
      cls.lbl <- dat[,1];
      conc <- dat[,-1];
      var.nms <- colnames(conc);
    }else{ # sample in col
      msg<-c(msg, "Samples are in columns and features in rows.");
      var.nms <- dat[-1,1];
      dat[,1] <- NULL;
      smpl.nms <- colnames(dat);
      cls.lbl <- dat[1,];
      conc <- t(dat[-1,]);
    }
  }
  
  # free memory
  dat <- NULL;
  
  msg <- c(msg, "The uploaded file is in comma separated values (.csv) format.");
  
  # try to remove empty line if present
  # identified if no sample names provided
  
  empty.inx <- is.na(smpl.nms) | smpl.nms == ""
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty rows</font> were detected and excluded from your data."));
    smpl.nms <- smpl.nms[!empty.inx];
    cls.lbl <-  cls.lbl[!empty.inx];
    conc <- conc[!empty.inx, ];
  }
  
  # try to check & remove empty lines if class label is empty
  # Added by B. Han
  empty.inx <- is.na(cls.lbl) | cls.lbl == ""
  if(sum(empty.inx) > 0){
    if(mSetObj$analSet$type != "roc"){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
      smpl.nms <- smpl.nms[!empty.inx];
      cls.lbl <-  cls.lbl[!empty.inx];
      conc <- conc[!empty.inx, ];
    }else{
      # force all NA to empty string, otherwise NA will become "NA" class label
      cls.lbl[is.na(cls.lbl)] <- "";
      msg <- c(msg, paste("<font color=\"orange\">", sum(empty.inx), "new samples</font> were detected from your data."));
    }
  }
  
  if(mSetObj$analSet$type == "roc"){
    if(length(unique(cls.lbl[!empty.inx])) > 2){
      AddErrMsg(mSetObj, "ROC analysis is only defined for two-group comparisions!");
      return(0);
    }
  }
  
  # try to remove check & remove empty line if sample name is empty
  empty.inx <- is.na(smpl.nms) | smpl.nms == "";
  if(sum(empty.inx) > 0){
    msg <- c(msg,paste("<font color=\"red\">", sum(empty.inx), "empty samples</font> were detected and excluded from your data."));
    smpl.nms <- smpl.nms[!empty.inx];
    cls.lbl <-  cls.lbl[!empty.inx];
    conc <- conc[!empty.inx, ];
  }
  
  # check for uniqueness of dimension name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg(mSetObj, "Duplicate sample names are not allowed!");
    AddErrMsg(mSetObj, dup.nm);
    return(0);
  }
  
  # try to remove check & remove empty line if feature name is empty
  empty.inx <- is.na(var.nms) | var.nms == "";
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty features</font> were detected and excluded from your data."));
    var.nms <- var.nms[!empty.inx];
    conc <- conc[,!empty.inx];
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    AddErrMsg(mSetObj, "Duplicate feature names are not allowed!");
    AddErrMsg(mSetObj, dup.nm);
    return(0);
  }
  
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    na.inx <- is.na(iconv(smpl.nms));
    nms <- paste(smpl.nms[na.inx], collapse="; ");
    AddErrMsg(mSetObj, paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
    return(0);
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    na.inx <- is.na(iconv(var.nms));
    nms <- paste(var.nms[na.inx], collapse="; ");
    AddErrMsg(mSetObj, paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
    return(0);
  }
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  smpl.nms <- gsub("[^[:alnum:]./_-]", "", smpl.nms);
  var.nms <- gsub("[^[:alnum:][:space:],'./_-]", "", var.nms); # allow space, comma and period
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  # check if paired or not
  if(mSetObj$dataSet$paired){
    # save as it is and process in sanity check step
    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$pairs <- cls.lbl
  }else{
    if(lbl.type == "disc"){
      # check for class labels at least two replicates per class
      if(min(table(cls.lbl)) < 3){
        AddErrMsg(mSetObj, paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
        AddErrMsg(mSetObj, "At least three replicates are required in each group!");
        AddErrMsg(mSetObj, "Or maybe you forgot to specify the data format?");
        return(0);
      }
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.factor(as.character(cls.lbl));
      if(substring(format,4,5)=="ts"){
        mSetObj$dataSet$facA <- as.factor(as.character(facA));
        mSetObj$dataSet$orig.facA <- as.factor(as.character(facA))
        mSetObj$dataSet$facA.lbl <- facA.lbl;
        mSetObj$dataSet$facB <- as.factor(as.character(facB));
        mSetObj$dataSet$orig.facB <- as.factor(as.character(facB));
        mSetObj$dataSet$facB.lbl <- facB.lbl;
      }
    }else{ # continuous
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.numeric(cls.lbl);
    }
  }
  
  # for the current being to support MSEA and MetPA
  if(mSetObj$dataSet$type == "conc"){
    mSetObj$dataSet$cmpd <- var.nms;
  }
  
  mSetObj$dataSet$orig <- conc; # copy to be processed in the downstream
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj)), ") data matrix.", sep=""));
  print(mSetObj$msgSet$read.msg);
  
  return(.set.mSet(mSetObj));
}

#'Read peak list files
#'@description This function reads peak list files and fills the data into a dataSet object.  
#'For NMR peak lists, the input should be formatted as two-columns containing numeric values (ppm, int).
#'Further, this function will change ppm to mz, and add a dummy 'rt'.
#'For MS peak data, the lists can be formatted as two-columns (mz, int), in which case the function will add a dummy 'rt', or
#'the lists can be formatted as three-columns (mz, rt, int).
#'@usage Read.PeakList(mSetObj=NA, foldername)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param foldername name of the folder containing the NMR or MS peak list files to read
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Read.PeakList<-function(mSetObj=NA, foldername){
  mSetObj <- .get.mSet(mSetObj);
  suppressMessages(library(xcms));
  msg <- c("The uploaded files are peak lists and intensities data.");
  
  # the "upload" folder should contain several subfolders (groups)
  # each of the subfolder contains samples (.csv files)
  files<-dir(foldername, pattern=".[Cc][Ss][Vv]$", recursive=T, full.name=TRUE)
  if (length(files) == 0) {
    AddErrMsg(mSetObj, "No peak list files (.csv) were found.");
    return(0);
  }
  
  snames <- gsub("\\.[^.]*$", "", basename(files));
  msg<-c(msg, paste("A total of ", length(files), "samples were found."));
  
  sclass <- gsub("^\\.$", "sample", dirname(files));
  
  scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "");
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
    AddErrMsg(mSetObj, "You must provide classes labels (at least two classes)!");
    return(0);
  }
  
  # check for class labels at least three replicates per class
  if(min(table(sclass)) < 3){
    AddErrMsg(mSetObj, "At least three replicates are required in each group!");
    return(0);
  }
  
  # check for unique sample names
  if(length(unique(snames))!=length(snames)){
    AddErrMsg(mSetObj, "Duplcate sample names are not allowed!");
    dup.nm <- paste(snames[duplicated(snames)], collapse=" ");
    AddErrMsg(mSetObj, "Duplicate sample names are not allowed!");
    AddErrMsg(mSetObj, dup.nm);
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
    AddErrMsg(mSetObj, "The CSV file is not formatted correctly!");
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
    AddErrMsg(mSetObj, "The peak list file can only contain 2 or 3 columns.");
    return(0);
  }
  
  all.peaks<-NULL;
  
  for(i in 1:length(files)){
    print(files[i]);
    pks<- as.matrix(.readDataTable(files[i]));
    if(ncol(pks)!=n.col){
      AddErrMsg(mSetObj, "The number of columns in each file are not the same!");
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
  mSetObj$dataSet$peakSet <- peakSet;
  mSetObj$msgSet$read.msg <- msg;
  return(.set.mSet(mSetObj));
}

#'Read LC/GC-MS spectra (.netCDF, .mzXML, mzData)
#'@description This function handles reading in LC/GC-MS spectra files and fills in the dataSet object.
#'It uses functions from the XCMS package to perform peak detection and alignment (grouping).
#'@usage Read.MSspec(mSetObj, folderName, profmethod, fwhm, bw)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param folderName the name of the folder containing the MS spectra
#'@param profmethod specify the method to use for profile generation, supports "bin", "binlin",
#'"binlinbase" and "intlin" 
#'@param fwhm  specify the full width at half maximum of the matched filtration
#'gaussian model peak
#'@param bw define the bandwidth (standard deviation of the smoothing kernel) to be used
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Read.MSspec<-function(mSetObj=NA, folderName, profmethod='bin', fwhm=30, bw=30){
  mSetObj <- .get.mSet(mSetObj);
  suppressMessages(library(xcms));
  msfiles <- list.files(folderName, recursive=T, full.names=TRUE);
  
  # first do some sanity check b4 spending more time on that
  # note the last level is the file names, previous one should be the class label
  
  dir.table <- t(data.frame(strsplit(msfiles, "/")));
  cls.all<-dir.table[,ncol(dir.table)-1];
  smpl.all <- dir.table[,ncol(dir.table)];
  
  # check for groups
  if(length(levels(as.factor(cls.all))) < 2){
    mSetObj$msgSet$read.msg <- "<font color='red'>At least two groups are required!</font>";
    return(0);
  }
  
  # check for min samples in each group
  #if(min(table(cls.all)) < 3){
  #  mSetObj$msgSet$read.msg <- "<font color='red'>At least three replicates are required in each group!</font>";
  #  return(0);
  #}
  
  # check for unique sample names
  if(length(unique(smpl.all))!=length(smpl.all)){
    mSetObj$msgSet$read.msg <- "<font color='red'>Duplcate sample names are not allowed!</font>";
    return(0);
  }
  
  xset <- xcmsSet(msfiles, profmethod = profmethod, fwhm=fwhm);
  msg<-c(paste("In total,", length(xset@filepaths), "sample files were detected. "),
         paste("They are divided into ", length(levels(xset@phenoData[,1]))," classes: ", paste(levels(xset@phenoData[,1]), collapse=', '), ".", sep=""));
  
  xset <- group(xset, bw=bw);
  mSetObj$dataSet$xset.orig <- xset;
  mSetObj$msgSet$read.msg <- msg;
  return(.set.mSet(mSetObj));
}

#'Read paired peak or spectra files
#'@description This function reads paired peak lists or spectra files. The pair information
#'is stored in a file where each line is a pair and names are separated by ":".
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ReadPairFile <- function(filePath="pairs.txt"){
  all.pairs <- scan(filePath, what='character', strip.white = T);
  labels <- as.vector(rbind(1:length(all.pairs), -(1:length(all.pairs))));
  all.names <- NULL;
  for(i in 1:length(all.pairs)){
    all.names=c(all.names, unlist(strsplit(all.pairs[i],":"), use.names=FALSE));
  }
  names(labels) <- all.names;
  labels;
}

#'Save the processed data with class names
#'@description This function saves the processed data with class names as CSV files. 
#'Several files may be generated, the original data, processed data, peak normalized, and/or normalized data. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SaveTransformedData<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(!is.null(mSetObj$dataSet$orig)){
    lbls <- NULL;
    tsFormat <- substring(mSetObj$dataSet$format,4,5)=="ts";
    if(tsFormat){
      lbls <- cbind(as.character(mSetObj$dataSet$orig.facA),as.character(mSetObj$dataSet$orig.facB));
      colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
    }else{
      lbls <- cbind("Label"= as.character(mSetObj$dataSet$orig.cls));
    }
    orig.data<-cbind(lbls, mSetObj$dataSet$orig);
    if(dim(orig.data)[2]>200){
      orig.data<-t(orig.data);
    }
    write.csv(orig.data, file="data_original.csv");
    if(!is.null(mSetObj$dataSet$procr)){
      if(tsFormat){
        lbls <- cbind(as.character(mSetObj$dataSet$proc.facA),as.character(mSetObj$dataSet$proc.facB));
        colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      }else{
        lbls <- cbind("Label"= as.character(mSetObj$dataSet$proc.cls));
      }
      proc.data<-cbind(lbls, mSetObj$dataSet$procr);
      if(dim(proc.data)[2]>200){
        proc.data<-t(proc.data);
      }
      write.csv(proc.data, file="data_processed.csv");
      if(!is.null(mSetObj$dataSet$norm)){
        if(tsFormat){
          lbls <- cbind(as.character(mSetObj$dataSet$facA),as.character(mSetObj$dataSet$facB));
          colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
        }else{
          lbls <- cbind("Label"= as.character(mSetObj$dataSet$cls));
        }
        
        # for ms peaks with rt and ms, insert two columns, without labels
        # note in memory, features in columns
        
        if(!is.null(mSetObj$dataSet$three.col)){ 
          ids <- matrix(unlist(strsplit(colnames(mSetObj$dataSet$norm), "/")),ncol=2, byrow=T);
          colnames(ids) <- c("mz", "rt");
          new.data <- data.frame(ids, t(mSetObj$dataSet$norm));
          write.csv(new.data, file="peak_normalized_rt_mz.csv");
        }
        
        norm.data<-cbind(lbls, mSetObj$dataSet$norm);
        if(dim(norm.data)[2]>200){
          norm.data<-t(norm.data);
        }
        write.csv(norm.data, file="data_normalized.csv");
      }
    }
  }
  return(.set.mSet(mSetObj));
}

#'@export
#'
AddErrMsg <- function(mSetObj=NA, msg){
  mSetObj <- .get.mSet(mSetObj);
  if(!exists('msg.vec', where = mSetObj$msgSet)){
    mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  }
  mSetObj$msgSet$msg.vec <- c(mSetObj$msgSet$msg.vec, msg);
  return(.set.mSet(mSetObj));
}

#'Plot compound summary
#'change to use dataSet$procr instead of dataSet$orig in
#'case of too many NAs
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotCmpdSummary<-function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_summary_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else{
    w <- width;
  }
  
  if(substring(mSetObj$dataSet$format,4,5)!="ts"){
    
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height= w*5/9, type=format, bg="white");
    par(mar=c(4,4,2,2), mfrow = c(1,2), oma=c(0,0,2,0));
    
    mns <- by(as.numeric(mSetObj$dataSet$procr[, cmpdNm]), mSetObj$dataSet$proc.cls, mean, na.rm=T);
    sds <- by(as.numeric(mSetObj$dataSet$procr[, cmpdNm]), mSetObj$dataSet$proc.cls, sd, na.rm=T);
    
    ups <- mns + sds;
    dns <- mns - sds;
    
    # all concentration need start from 0
    y <- c(0, dns, mns, ups);
    
    rg <- range(y) + 0.05 * diff(range(y)) * c(-1, 1)
    pt <- pretty(y)
    
    axp=c(min(pt), max(pt[pt <= max(rg)]),length(pt[pt <= max(rg)]) - 1);
    
    # ymk <- pretty(c(0,ymax));
    x <- barplot(mns, col= unique(GetColorSchema(mSetObj)), las=2, yaxp=axp, ylim=range(pt));
    arrows(x, dns, x, ups, code=3, angle=90, length=.1);
    axis(1, at=x, col="white", col.tick="black", labels=F);
    box();
    mtext("Original Conc.", line=1);
    
    boxplot(mSetObj$dataSet$norm[, cmpdNm]~mSetObj$dataSet$cls,las=2, col= unique(GetColorSchema(mSetObj)));
    mtext("Normalized Conc.", line=1);
    title(main=cmpdNm, out=T);
    dev.off();
    
  }else if(mSetObj$dataSet$design.type =="time0"){
    Cairo(file = imgName, unit="in", dpi=dpi, width=8, height= 6, type=format, bg="white");
    plotProfile(mSetObj, cmpdNm);
    dev.off();
    
  }else{
    if(mSetObj$dataSet$design.type =="time"){ # time trend within phenotype
      out.fac <- mSetObj$dataSet$exp.fac;
      in.fac <- mSetObj$dataSet$time.fac;
      xlab = "Time";
    }else{ # factor a split within factor b
      out.fac <- mSetObj$dataSet$facB;
      in.fac <- mSetObj$dataSet$facA;
      xlab = mSetObj$dataSet$facA.lbl;
    }
    
    # two images per row
    img.num <- length(levels(out.fac));
    row.num <- ceiling(img.num/2)
    
    if(row.num == 1){
      h <- w*5/9;
    }else{
      h <- w*0.5*row.num;
    }
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mar=c(3,4,4,2), mfrow=c(row.num, 2));
    # make sure all at the same range
    ylim.ext <-  GetExtendRange (mSetObj$dataSet$norm[, cmpdNm], 12);
    for(lv in levels(out.fac)){
      inx <- out.fac == lv;
      dat <- mSetObj$dataSet$norm[inx, cmpdNm];
      cls <- in.fac[inx];
      boxplot(dat ~ cls, col="#0000ff22", ylim=ylim.ext, outline=FALSE, boxwex=c(0.5, 0.5), xlab=xlab, ylab="Abundance", main=lv);
      stripchart(dat ~ cls, method = "jitter", ylim=ylim.ext, vertical=T, add = T, pch=19, cex=0.7, names = c("",""));
    }
    dev.off();
  }
  return(imgName);
}

#' Read RDS files from the internet
#' @description Function downloads the required file and reads it only if not already in working directory.
#' Need to specify the file URL and the destfile. 
#' @usage read_compound(fileURL, destfile)
#' @param fileURL Name of the file path
#' @param destfile Name of the downloaded file to current working directory 
#'
#' example: cmpd <- read_MetAnal_RDS("http://www.metaboanalyst.ca/resources/libs/syn_nms.rds", destfile="syn_nms.rds")

read_MetAnal_RDS <- function(fileURL, destfile){
  
  if(.on.public.web){
    metanal <- readRDS("../../libs/compound_db.rds");
  }else if(!file.exists(destfile)) {
    res <- download.file(fileURL, destfile, method="libcurl");
    metanal <- readRDS(destfile);
  }else{
    time <- file.info(destfile)
    diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
    
    if(diff_time>30){
      res <- download.file(fileURL, destfile, method="libcurl");
    }
    metanal <- readRDS(destfile);
  }
  
  return(metanal);
}

#' Synonym Names Database
read_synnames_RDS <- function(destfile){
  
  if(.on.public.web){
    syn.db <- readRDS("../../libs/syn_nms.rds")
  }else if(!file.exists(destfile)){
    res <- download.file("http://www.metaboanalyst.ca/resources/libs/syn_nms.rds", destfile="syn_nms.rds", method="libcurl")
    syn.db <- readRDS("syn_nms.rds")
  }else{
    time <- file.info(destfile)
    diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
    
    if(diff_time>30){
      res <- download.file("http://www.metaboanalyst.ca/resources/libs/syn_nms.rds", destfile="syn_nms.rds", method="libcurl")
    }
    syn.db <- readRDS("syn_nms.rds")
  }
  return(syn.db)
}

#' Read metabolite sets from library 
read_mSet <- function(libname){
  
  destfile <- paste(libname, ".rda", sep="");
  
  if(.on.public.web){
    libPath <- paste("../../libs/msets/", libname, ".rda", sep="");
    load(libPath, .GlobalEnv);
  }else if(!file.exists(destfile)){
    libPath <- paste("http://www.metaboanalyst.ca/resources/libs/msets/", libname, ".rda", sep="");
    download.file(libPath, destfile);
    load(destfile, .GlobalEnv);
  }else{
    time <- file.info(destfile)
    diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
    
    if(diff_time>30){
      libPath <- paste("http://www.metaboanalyst.ca/resources/libs/msets/", libname, ".rda", sep="");
      download.file(libPath, destfile);
    }
    load(destfile, .GlobalEnv);  
  }
}

#' Read KEGG files from library

read_KEGG <- function(kegg.rda){
  
  destfile <- paste(kegg.rda, ".rda", sep="");
  
  if(.on.public.web){
    libPath <- paste("../../libs/kegg/", kegg.rda, ".rda", sep="");
    load(libPath, .GlobalEnv);
  }else if(!file.exists(destfile)){
    libPath <- paste("http://www.metaboanalyst.ca/resources/libs/kegg/", kegg.rda, ".rda", sep="");
    download.file(libPath, destfile);
    load(destfile, .GlobalEnv);
  }else{
    time <- file.info(destfile)
    diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
    
    if(diff_time>30){
      libPath <- paste("http://www.metaboanalyst.ca/resources/libs/kegg/", kegg.rda, ".rda", sep="");
      download.file(libPath, destfile);
    }
    load(destfile, .GlobalEnv);  
  }
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################


GetErrMsg<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$msgSet$msg.vec);
}


GetKEGG.PathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(names(metpa$path.ids));
}


#'Given a vector containing KEGGIDs, returns a vector of KEGG compound names
#'@description This function, given a vector containing KEGGIDs, returns a vector of KEGG compound names.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2Name<-function(ids){
  fileURL <- "http://www.metaboanalyst.ca/resources/libs/compound_db.rds";
  destfile <- "compound_db.rds";
  cmpd.db <- read_MetAnal_RDS(fileURL, destfile);
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, 3]);
}

#'Given a vector containing KEGG pathway IDs, return a vector containing SMPDB IDs (only for hsa)
#'@description This function, when given a vector of KEGG pathway IDs, return a vector of SMPDB IDs (only for hsa).
#'SMPDB standing for the Small Molecule Pathway Database, and hsa standing for human serum albumin. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGPATHID2SMPDBIDs<-function(ids){
  hit.inx<-match(ids, path.map[,1]);
  return(path.map[hit.inx, 3]);
}

#'Given a vector of HMDBIDs, return a vector of HMDB compound names
#'@description This function, when given a vector of HMDBIDs, return a vector of HMDB compound names. HMDB standing
#'for the Human Metabolome Database. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2Name<-function(ids){
  fileURL <- "http://www.metaboanalyst.ca/resources/libs/compound_db.rds";
  destfile <- "compound_db.rds";
  cmpd.db <- read_MetAnal_RDS(fileURL, destfile);
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "name"]);
}

#'Given a vector of KEGGIDs, return a vector of HMDB ID
#'@description This functionn, when given a vector of KEGGIDs, returns a vector of HMDB IDs. HMDB standing
#'for the Human Metabolome Database. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2HMDBID<-function(ids){
  
  fileURL <- "http://www.metaboanalyst.ca/resources/libs/compound_db.rds";
  destfile <- "compound_db.rds";
  cmpd.db <- read_MetAnal_RDS(fileURL, destfile);
  
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, "hmdb_id"]);
}

#'Given a vector of HMDBIDs, return a vector of KEGG IDs
#'@description This function, when given a vector of HMDBIDs, returns a vector of KEGG ID. HMDB standing
#'for the Human Metabolome Database. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2KEGGID<-function(ids){
  fileURL <- "http://www.metaboanalyst.ca/resources/libs/compound_db.rds";
  destfile <- "compound_db.rds";
  cmpd.db <- read_MetAnal_RDS(fileURL, destfile);
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "kegg_id"]);
}

#'Save compound name for mapping
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.MapData<-function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpd <- qvec;
  return(.set.mSet(mSetObj));
}

#'Save concentration data
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.ConcData<-function(mSetObj=NA, conc){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$norm <- conc;
  return(.set.mSet(mSetObj));
}

#'Save biofluid type for SSP
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
Setup.BiofluidType<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$biofluid <- type;
  return(.set.mSet(mSetObj));
}

# all groups
GetGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  cls.lbl <- mSetObj$dataSet$prenorm.cls;
  if(mSetObj$analSet$type=="roc"){
    empty.inx <- is.na(cls.lbl) | cls.lbl == "";
    # make sure re-factor to drop level
    lvls <- levels(factor(cls.lbl[!empty.inx]));
  }else{
    lvls <- levels(cls.lbl);
  }
  return(lvls);
}

# groups entering analysis
GetNormGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  levels(mSetObj$dataSet$cls);
}

GetLiteralGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.character(mSetObj$dataSet$prenorm.cls);
}

IsReadyForEditor <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$prenorm)){
    return(0);
  }
  return(1);
}

SetOrganism <- function(org){
  inmex.org <<- org;
}