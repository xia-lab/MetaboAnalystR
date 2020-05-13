# internal variables and functions not to be modified by users
# This is only for web version 
.on.public.web <- FALSE; # only TRUE when on metaboanalyst web server

# note, this is usually used at the end of a function
# for local, return itself; for web, push to global environment
.set.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    mSet <<- mSetObj;
    return (1);
  }
  return(mSetObj);
}

.get.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    return(mSet)
  }else{
    return(mSetObj);
  }
}

# Used to defined the parallel namespace for peak picking
peak_function_list <- list("PerformPeakPicking",
                           "PeakPicking_centWave_slave",
                           "PerformPeakGrouping",
                           "Densitygrouping_slave",
                           "PerformPeakAlignment",
                           "RT.Adjust_Slave",
                           "PerformPeakFiling",
                           "mSet2xcmsSet",
                           "updateRawSpectraParam",
                           "continuousPtsAboveThreshold",
                           "getLocalNoiseEstimate",
                           "continuousPtsAboveThresholdIdx",
                           "MSW.cwt",
                           "MSW.extendNBase",
                           "MSW.extendLength",
                           "MSW.getLocalMaximumCWT",
                           "MSW.localMaximum",
                           "MSW.getRidge",
                           "descendMin",
                           "descendMinTol",
                           "joinOverlappingPeaks",
                           "trimm",
                           "findEqualGreaterM",
                           "na.flatfill",
                           "SSgauss",
                           "rectUnique",
                           ".narrow_rt_boundaries",
                           ".rawMat",
                           ".getPeakGroupsRtMatrix",
                           ".peakIndex",
                           ".applyRtAdjToChromPeaks",
                           ".applyRtAdjustment",
                           ".getChromPeakData",
                           ".feature_values",
                           ".groupval",
                           "binYonX",
                           "imputeLinInterpol",
                           "colMax",
                           "filtfft",
                           "descendZero",
                           "which.colMax",
                           "breaks_on_nBins",
                           ".aggregateMethod2int"
                           
                           
)

# Used to defined the parallel namespace for parameters optimization
optimize_function_list <- c(list("PeakPicking_prep",
                                 "Statistic_doe",
                                 "SlaveCluster_doe",
                                 "calculateSet_doe",
                                 "SlaveCluster_doe",
                                 "calculateSet_doe",
                                 "calculatePPKs",
                                 "calculateGPRT",
                                 "calcPPS2",
                                 "calcCV",
                                 "resultIncreased_doe",
                                 "calcRCS_GSValues",
                                 "calcGaussianS",
                                 "peaks_IPO",
                                 "getClusterType",
                                 "calcMaximumCarbon",
                                 "getMaximumLevels",
                                 "getMaxSettings",
                                 "expand.grid.subset",
                                 "typeCastParams",
                                 "getCcdParameter",
                                 "combineParams",
                                 "getRGTVValues",
                                 "findIsotopes.IPO",
                                 "creatPeakTable",
                                 "createModel",
                                 "decode",
                                 "decodeAll",
                                 "encode",
                                 "attachList",
                                 "checkParams"),
                            peak_function_list)




#'Constructs a dataSet object for storing data 
#'@description This functions handles the construction of a mSetObj object for storing data for further processing and analysis.
#'It is necessary to utilize this function to specify to MetaboAnalystR the type of data and the type of analysis you will perform. 
#'@usage InitDataObjects(data.type, anal.type, paired=FALSE)
#'@param data.type The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data)
#'@param anal.type Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, 
#'cmpdmap, smpmap, or pathinteg
#'@param paired Indicate if the data is paired or not. Logical, default set to FALSE
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import methods

InitDataObjects <- function(data.type, anal.type, paired=FALSE){
  
  if(!.on.public.web){
    if(exists("mSet")){
      mSetObj <- .get.mSet(mSet);
      mSetObj$dataSet$type <- data.type;
      mSetObj$analSet$type <- anal.type;
      return(.set.mSet(mSetObj));
    }
  }
  
  dataSet <- list();
  dataSet$type <- data.type;
  dataSet$design.type <- "regular"; # one factor to two factor
  dataSet$cls.type <- "disc"; # default until specified otherwise
  dataSet$format <- "rowu";
  dataSet$paired <- paired;
  analSet <- list();
  analSet$type <- anal.type;
  
  mSetObj <- list();
  mSetObj$dataSet <- dataSet;
  mSetObj$analSet <- analSet;
  mSetObj$imgSet <- list();
  mSetObj$msgSet <- list(); # store various message during data processing
  mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  mSetObj$cmdSet <- vector(mode="character"); # store R command
  
  # other global variables
  msg.vec <<- "";
  err.vec <<- "";

  # for network analysis
  module.count <<- 0;
  # counter for naming different json file (pathway viewer)
  smpdbpw.count <<- 0; 
  # for mummichog
  peakFormat <<- "mpt"  

  # for meta-analysis
  mdata.all <<- list(); 
  mdata.siggenes <<- vector("list");
  meta.selected <<- TRUE;
  anal.type <<- anal.type;
  
  if(.on.public.web){
    # disable parallel prcessing for public server
    library(BiocParallel);
    register(SerialParam());
  }else{
    if("stat" %in% anal.type | "msetqea" %in% anal.type | "pathqea" %in% anal.type | "roc" %in% anal.type)
    # start Rserve engine for Rpackage
    load_Rserve();
  }
  
  # plotting required by all
  Cairo::CairoFonts(regular="Arial:style=Regular",bold="Arial:style=Bold",italic="Arial:style=Italic",bolditalic = "Arial:style=Bold Italic",symbol = "Symbol")
  
  # sqlite db path for gene annotation
  if(file.exists("/home/glassfish/sqlite/")){ #.on.public.web
     url.pre <- "/home/glassfish/sqlite/";
  }else if(file.exists("/home/jasmine/Downloads/sqlite/")){ #jasmine's local
     url.pre <- "/home/jasmine/Downloads/sqlite/";
  }else if(file.exists("/Users/soufanom/Documents/Projects/gene-id-mapping/")){ # soufan laptop
     url.pre <- "/Users/soufanom/Documents/Projects/gene-id-mapping/";
  }else if(file.exists("~/Documents/Projects/gene-id-mapping/")){
     url.pre <- "~/Documents/Projects/gene-id-mapping/"
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){ # xia local
     url.pre <- "/Users/xia/Dropbox/sqlite/";
  }else if(file.exists("/home/zzggyy/Downloads/netsqlite/")){
     url.pre <<-"/home/zzggyy/Downloads/netsqlite/"; #zgy local)
  }else{
     url.pre <- paste0(dirname(system.file("database", "sqlite/GeneID_25Species_JE/ath_genes.sqlite", package="MetaboAnalystR")), "/")
  }

  gene.sqlite.path <<- url.pre;
  api.base <<- "api.metaboanalyst.ca" #"localhost:8987"

  print("MetaboAnalyst R objects initialized ...");
  return(.set.mSet(mSetObj));
}

#'For two factor time series only
#'@description For two factor time series only
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param design Input the design type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetDesignType <-function(mSetObj=NA, design){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$design.type <- tolower(design);
  return(.set.mSet(mSetObj));
}

#'Record R Commands
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmd Commands 
#'@export
RecordRCommand <- function(mSetObj=NA, cmd){
  mSetObj <- .get.mSet(mSetObj); 
  mSetObj$cmdSet <- c(mSetObj$cmdSet, cmd);
  return(.set.mSet(mSetObj));
}

SaveRCommands <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj); 
  cmds <- paste(mSetObj$cmdSet, collapse="\n");
  write(cmds, file = "Rhistory.R", append = FALSE);
}

#'Export R Command History
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetRCommandHistory <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj); 
  return(mSetObj$cmdSet);
}

#'Constructor to read uploaded CSV or TXT files into the dataSet object
#'@description This function handles reading in CSV or TXT files and filling in the dataSet object 
#'created using "InitDataObjects". 
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
  
  if(class(dat) == "try-error" || ncol(dat) == 1){
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    AddErrMsg("Make sure the file delimeters are commas.");
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
      cls.lbl <- facA <- dat[,2]; # default assign facA to cls.lbl in order for one-factor analysis
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
    
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor
      if(!(tolower(facA.lbl) == "time" | tolower(facB.lbl) == "time")){
        AddErrMsg("No time points found in your data");
        AddErrMsg("The time points group must be labeled as <b>Time</b>");
        return(0);
      }
    }
  }else{
    
    if(substring(format,1,3)=="row"){ # sample in row
      msg <- c(msg, "Samples are in rows and features in columns");
      smpl.nms <-dat[,1];
      dat[,1] <- NULL; #remove sample names
      if(lbl.type == "qc"){
        rownames(dat) <- smpl.nms;
        mSetObj$dataSet$orig <- dat;
        mSetObj$dataSet$cmpd <- colnames(dat);
        return(1);
      }
      
      cls.lbl <- dat[,1];
      conc <- dat[,-1, drop=FALSE];
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

  mSetObj$dataSet$type.cls.lbl <- class(cls.lbl);
  
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

  if(anal.type == "roc"){
    if(length(unique(cls.lbl[!empty.inx])) > 2){
      AddErrMsg("ROC analysis is only defined for two-group comparisions!");
      return(0);
    }
  }

  # check for uniqueness of dimension name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
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
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }

  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    na.inx <- is.na(iconv(smpl.nms));
    nms <- paste(smpl.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
    return(0);
  }
 
  if(sum(is.na(iconv(var.nms)))>0){
    na.inx <- is.na(iconv(var.nms));
    nms <- paste(var.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
    return(0);
  }
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  smpl.nms <- CleanNames(smpl.nms, "sample_name");


  # keep a copy of original names for saving tables 
  orig.var.nms <- var.nms;
  var.nms <- CleanNames(var.nms, "var_name"); # allow space, comma and period
  names(orig.var.nms) <- var.nms;

  cls.lbl <- ClearStrings(as.vector(cls.lbl));

  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;

  # check if paired or not
  if(mSetObj$dataSet$paired){
    # save as it is and process in sanity check step
    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$pairs <- cls.lbl;
  }else{
    if(lbl.type == "disc"){
      # check for class labels at least two replicates per class
      if(min(table(cls.lbl)) < 3){
        AddErrMsg(paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
        AddErrMsg("At least three replicates are required in each group!");
        AddErrMsg("Or maybe you forgot to specify the data format?");
        return(0);
      }
     
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.factor(as.character(cls.lbl));
      
      if(substring(format,4,5)=="ts"){
        
        mSetObj$dataSet$facA.type <- is.numeric(facA);
        mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA <- as.factor(as.character(facA));
        mSetObj$dataSet$facA.lbl <- facA.lbl;
        
        mSetObj$dataSet$facB.type <- is.numeric(facB);
        mSetObj$dataSet$orig.facB <- mSetObj$dataSet$facB <- as.factor(as.character(facB));
        mSetObj$dataSet$facB.lbl <- facB.lbl;
      }
      
    }else{ # continuous

      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- tryCatch({
        as.numeric(cls.lbl);
      },warning=function(na) {
        print("Class labels must be numeric and continuous!");
        return(0);
      })
      
      if(mSetObj$dataSet$cls == 0){
        AddErrMsg("Class labels must be numeric and continuous!");
        return(0)
      }
    }
  }

  # for the current being to support MSEA and MetPA
  if(mSetObj$dataSet$type == "conc"){
    mSetObj$dataSet$cmpd <- var.nms;
  }

  mSetObj$dataSet$mumType <- "table";
  mSetObj$dataSet$orig.var.nms <- orig.var.nms;
  mSetObj$dataSet$orig <- conc; # copy to be processed in the downstream
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));

  return(.set.mSet(mSetObj));
}

#'Read peak list files
#'@description This function reads peak list files and fills the data into a dataSet object.  
#'For NMR peak lists, the input should be formatted as two-columns containing numeric values (ppm, int).
#'Further, this function will change ppm to mz, and add a dummy 'rt'.
#'For MS peak data, the lists can be formatted as two-columns (mz, int), in which case the function will add a dummy 'rt', or
#'the lists can be formatted as three-columns (mz, rt, int).
#'@usage Read.PeakList(mSetObj=NA, foldername)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param foldername Name of the folder containing the NMR or MS peak list files to read.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Read.PeakList<-function(mSetObj=NA, foldername="upload"){
  mSetObj <- .get.mSet(mSetObj);

  if(.on.public.web){
       dyn.load("../../rscripts/metaboanalystr/src/MetaboAnalyst.so")
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

  if(.on.public.web){
    load_xcms()
  }

  msfiles <- list.files(folderName, recursive=T, full.names=TRUE);
  
  # first do some sanity check b4 spending more time on that
  # note the last level is the file names, previous one should be the class label
  
  dir.table <- t(data.frame(strsplit(msfiles, "/")));
  cls.all <- dir.table[,ncol(dir.table)-1];
  smpl.all <- dir.table[,ncol(dir.table)];
  
  # check for groups
  if(length(levels(as.factor(cls.all))) < 2){
    mSetObj$msgSet$read.msg <- "<font color='red'>At least two groups are required!</font>";
    return(0);
  }
  
  # check for unique sample names
  if(length(unique(smpl.all))!=length(smpl.all)){
    mSetObj$msgSet$read.msg <- "<font color='red'>Duplcate sample names are not allowed!</font>";
    return(0);
  }
  
  xset <- xcms::xcmsSet(msfiles, profmethod = profmethod, fwhm=fwhm);
  msg<-c(paste("In total,", length(xset@filepaths), "sample files were detected. "),
         paste("They are divided into ", length(levels(xset@phenoData[,1]))," classes: ", paste(levels(xset@phenoData[,1]), collapse=', '), ".", sep=""));
  
  xset <- xcms::group(xset, bw=bw);
  mSetObj$dataSet$xset.orig <- xset;
  mSetObj$msgSet$read.msg <- msg;
  return(.set.mSet(mSetObj));
}

#'Read paired peak or spectra files
#'@description This function reads paired peak lists or spectra files. The pair information
#'is stored in a file where each line is a pair and names are separated by ":".
#'@param filePath Set file path
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
  return(labels);
}

#' Read an mzTab tab separated file from the passed in file.
#' Adapted from: https://github.com/lifs-tools/rmzTab-m/blob/master/R/MzTabReader.r
#' @param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#' @param filename The name of the mzTab file to parse.
#' @param identifier The identifier to be used when the table is parsed. Use "name"
#' to use the chemical_name, "mass" to use the theoretical_neutral_mass and "sml_id"
#' to use the SML_ID. If the number of missing name and mass entries is greater than 90%,
#' then the SML_ID will be used.
#' @export
Read.MzTab <- function(mSetObj=NA, filename, identifier = "name") {

  mSetObj <- .get.mSet(mSetObj);
  
  msg <- NULL;
  
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=filename, sep = "\t")))
  
  mztab.table = utils::read.table(file=filename, header=FALSE,
                                  row.names=NULL, dec = ".", fill = TRUE,
                                  col.names = paste0("V", seq_len(ncol)),
                                  sep="\t", na.strings="null", quote = "",
                                  stringsAsFactors = FALSE)
  
  # first sanity check
  # check if contains MTD, SMH and SML
  if(sum(sapply(c("MTD", "SMH", "SML"), grepl, unique(mztab.table$V1))) == 3){
    msg <- ("mzTab format ok!")
  }else{
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  } 
  
  # first set up metadata
  metadata <- mztab.table[startsWith(as.character(mztab.table$V1), "MTD"),]
  variables <- metadata[grepl("study_variable", metadata$V2),]
  
  if(length(variables) < 1){
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  }
  
  variables.groups <- unique(gsub("-.*", "", variables$V2))
  group.names <- metadata$V3[match(variables.groups, metadata$V2)] 
  
  variables.list <- vector("list", length=length(variables.groups)) 
  names(variables.list) <- variables.groups
  
  for(i in 1:length(variables.groups)){
    group2match <- gsub("\\[", "\\\\[", variables.groups[i])
    group2match <- gsub("\\]", "\\\\]", group2match)
    all.info <- variables[grepl(group2match, variables$V2),] 
    assay.refs <- all.info$V3[grepl("assay_refs", all.info$V2)]
    variables.list[i] <- assay.refs
  }
  
  # second set up small molecule summary
  smh <- mztab.table[startsWith(as.character(mztab.table$V1), "SMH"),,drop=FALSE]
  sml <- mztab.table[startsWith(as.character(mztab.table$V1), "SML"),,drop=FALSE]
  
  if(nrow(sml) < 1){
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  }
  
  sml.data.frame <- data.frame(sml, stringsAsFactors = FALSE)
  colnames(sml.data.frame) <- as.character(smh[1,])
  
  # sanity check to see if selected identifier is valid
  # if more than 90% of names are null, switch to use m/z
  # if more than 90% of m/z are null, switch to use sml_id
  if(sum(is.null(sml.data.frame$chemical_name))/length(sml.data.frame$chemical_name) > 0.1) {
    msg <- c(msg, "Too many missing chemical names, will use theoretical neutral mass instead!")
    identifier <- "mass"
  }else if(sum(is.null(sml.data.frame$theoretical_neutral_mass))/length(sml.data.frame$theoretical_neutral_mass) > 0.1){
    msg <- c(msg, "Too many missing m/z, will use mzTab SML_ID instead!")
    identifier <- "sml_id"
  }else if(sum(is.na(sml.data.frame$theoretical_neutral_mass))/length(sml.data.frame$theoretical_neutral_mass) > 0.1){ # sometime it's NA
    msg <- c(msg, "Too many missing m/z, will use mzTab SML_ID instead!")
    identifier <- "sml_id"
  }
  
  # deal with duplicates in selected ids
  if(identifier == "name"){
    id.og <- id <- sml.data.frame$chemical_name
    dup.id <- paste(sml.data.frame$chemical_name, sml.data.frame$adduct_ions, sep="_")
    id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] <- dup.id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] 
  }else if(identifier == "mass"){
    id.og <- id <- round(as.numeric(sml.data.frame$theoretical_neutral_mass), 5)
    dup.id <- paste( round(as.numeric(sml.data.frame$theoretical_neutral_mass), 5), sml.data.frame$adduct_ions, sep="_")
    id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] <- dup.id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] 
  }else{
    id <- sml.data.frame$SML_ID;
  }
  
  # sanity check to see if selected id is valid
  # if ids are still duplicates, switch to append sml_id
  if(sum(duplicated(id)) > 1){
    id <- paste(id.og, sml.data.frame$SML_ID, sep="_")
  }
  
  assay_data <- trimws(unlist( lapply(variables.list, function(x) strsplit(x, "\\|")) ))
  assay_data <- paste("abundance_", assay_data, sep="")
  assay_df <- sml.data.frame[,match(assay_data, colnames(sml.data.frame))]
  
  assay_table <- cbind.data.frame(Sample=id, assay_df, stringsAsFactors = FALSE)

  # replace colnames with actual variable groups
  samples <- colnames(assay_table)[-1]
  samples_base <- gsub("abundance_", "", samples)
  variables.list <- lapply(variables.list, function(x) trimws(unlist(strsplit(x, "\\|"))))
  
  samples2groups <- vector(length = length(samples_base))
  
  for(i in 1:length(samples_base)){
    samples2groups[i] <- group.names[sapply(variables.list, function(x) samples_base[i] %in% x)]
  } 
  
  # remove blank from dataframe
  blank.inx <- grepl("blank", samples2groups, ignore.case = TRUE)
  
  if(sum(blank.inx) > 0){
    samples2groups <- samples2groups[!blank.inx]
    assay_table <- cbind.data.frame(Sample=id, assay_df[,!blank.inx], stringsAsFactors = FALSE) 
  }
  
  assay_table_all <- rbind.data.frame(c("Group", samples2groups), assay_table)
  colnames(assay_table_all) <- gsub("\\[|\\]", "", colnames(assay_table_all))
  
  write.csv(assay_table_all, "mzTab_parsed.csv", row.names = F)
  
  mSetObj$dataSet$cls.type <- "disc";
  mSetObj$dataSet$format <- "colu";
  
  dat <- assay_table_all 
  msg <- c(msg, "Samples are in columns and features in rows.");
  var.nms <- dat[-1,1];
  dat[,1] <- NULL;
  smpl.nms <- colnames(dat);
  cls.lbl <- unname(unlist(dat[1,]));
  conc <- t(dat[-1,]);
  mSetObj$dataSet$type.cls.lbl <- class(cls.lbl);
 
  # free memory
  dat <- NULL;
  
  msg <- c(msg, "The uploaded file is in the mzTab format.");
  
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
  
  # check for uniqueness of dimension name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
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
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    na.inx <- is.na(iconv(smpl.nms));
    nms <- paste(smpl.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
    return(0);
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    na.inx <- is.na(iconv(var.nms));
    nms <- paste(var.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
    return(0);
  }
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  smpl.nms <- CleanNames(smpl.nms, "sample_name");
  
  
  # keep a copy of original names for saving tables 
  orig.var.nms <- var.nms;
  var.nms <- CleanNames(var.nms, "var_name"); # allow space, comma and period
  names(orig.var.nms) <- var.nms;
  
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  # check for class labels at least two replicates per class
  if(min(table(cls.lbl)) < 3){
    AddErrMsg(paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
    AddErrMsg("At least three replicates are required in each group!");
    AddErrMsg("Or maybe you forgot to specify the data format?");
    return(0);
  }

  if(length(unique(cls.lbl)) == 1){
    AddErrMsg("At least two groups are required for statistical analysis!");
    return(0);
  }
  
  mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.factor(as.character(cls.lbl));
  mSetObj$dataSet$mumType <- "table";
  mSetObj$dataSet$orig.var.nms <- orig.var.nms;
  mSetObj$dataSet$orig <- conc; # copy to be processed in the downstream
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));
  return(.set.mSet(mSetObj));
}

#'Save the processed data with class names
#'@description This function saves the processed data with class names as CSV files. 
#'Several files may be generated, the original data, processed data, peak normalized, and/or normalized data. 
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SaveTransformedData <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  data.type <- mSetObj$dataSet$type
  
  # mummichog data is not processed, so just mSet$orig and mSet$proc
  
  if(anal.type=="mummichog"){
    
    orig.data<- mSetObj$dataSet$mummi.orig;

if(mSetObj$dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx <- rep(TRUE, nrow(mSetObj$dataSet$mummi.orig))
  }else if(mSetObj$dataSet$mode == "negative"){
    mSetObj$dataSet$pos_inx <- rep(FALSE, nrow(mSetObj$dataSet$mummi.orig) )
  }else{
    mSetObj$dataSet$pos_inx <- input$mode == "positive"
  }
    #colnames(orig.data) <- c("p.value", "m.z", "t.score")
    write.csv(orig.data, file="data_original.csv", row.names = FALSE);
    
    proc.data<- mSetObj$dataSet$mummi.proc
    #colnames(proc.data) <- c("p.value", "m.z", "t.score")
    write.csv(proc.data, file="data_processed.csv", row.names = FALSE);
    
  }else{
    if(!is.null(mSetObj$dataSet[["orig"]])){
      lbls <- NULL;
      tsFormat <- substring(mSetObj$dataSet$format,4,5)=="ts";
      if(tsFormat){
        lbls <- cbind(as.character(mSetObj$dataSet$orig.facA),as.character(mSetObj$dataSet$orig.facB));
        colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      }else{
        lbls <- cbind("Label"= as.character(mSetObj$dataSet$orig.cls));
      }
      
      orig.var.nms <- mSetObj$dataSet$orig.var.nms;
      orig.data<- mSetObj$dataSet$orig;
      
      
      # convert back to original names
      if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
        colnames(orig.data) <- orig.var.nms[colnames(orig.data)];
      }
        orig.data<-cbind(lbls, orig.data);

      if(dim(orig.data)[2]>200){
        orig.data<-t(orig.data);
      }
        
      write.csv(orig.data, file="data_original.csv");
      
      if(!is.null(mSetObj$dataSet[["proc"]])){
        
        if(!is.null(mSetObj$dataSet[["filt"]])){
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$filt.facA),as.character(mSetObj$dataSet$filt.facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$filt.cls));
          }
          proc.data<-mSetObj$dataSet$filt;  
        }else{
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$proc.facA),as.character(mSetObj$dataSet$proc.facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$proc.cls));
          }
          proc.data<-mSetObj$dataSet$proc;
        }
        
        # convert back to original names
        if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
          colnames(proc.data) <- orig.var.nms[colnames(proc.data)];
        }
        proc.data<-cbind(lbls, proc.data);
        
        if(dim(proc.data)[2]>200){
          proc.data<-t(proc.data);
        }
        write.csv(proc.data, file="data_processed.csv");
        
        if(!is.null(mSetObj$dataSet[["norm"]])){
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$facA),as.character(mSetObj$dataSet$facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$cls));
          }
          
          norm.data <- mSetObj$dataSet$norm;
          
          # for ms peaks with rt and ms, insert two columns, without labels
          # note in memory, features in columns
          if(!is.null(mSetObj$dataSet$three.col)){ 
            ids <- matrix(unlist(strsplit(colnames(norm.data), "/")),ncol=2, byrow=T);
            colnames(ids) <- c("mz", "rt");
            new.data <- data.frame(ids, t(norm.data));
            write.csv(new.data, file="peak_normalized_rt_mz.csv");
          }else{
            # convert back to original names
            if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
              colnames(norm.data) <- orig.var.nms[colnames(norm.data)];   
            }
            norm.data<-cbind(lbls, norm.data);
            if(dim(norm.data)[2]>200){
              norm.data<-t(norm.data);
            }
            write.csv(norm.data, file="data_normalized.csv");
          }
        }
      }
    }
  }
  return(.set.mSet(mSetObj));
}

#' Adds an error message
#'@description The error message will be printed in all cases.
#'Used in higher functions. 
#'@param msg Error message to print 
#'@export
AddErrMsg <- function(msg){
  err.vec <<- c(err.vec, msg);
  print(msg);
}

# general message only print when running local
AddMsg <- function(msg){
  msg.vec <<- c(msg.vec, msg);
  if(!.on.public.web){
    print(msg);
  }
}

# return the latest message
GetCurrentMsg <- function(){
  return(msg.vec[length(msg.vec)]);
}

GetMetaCheckMsg <- function(mSetObj=NA){
  return(current.msg);
}

#'Plot compound summary
#'change to use dataSet$proc instead of dataSet$orig in
#'case of too many NAs
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpdNm Input the name of the compound to plot
#'@param format Input the format of the image to create
#'@param dpi Input the dpi of the image to create
#'@param width Input the width of the image to create
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotCmpdSummary <- function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
    load_grid()
  }
  
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_summary_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 7.5;
  }else{
    w <- width;
  }
  
  if(substring(mSetObj$dataSet$format,4,5)!="ts"){
    
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height= w*0.65, type=format, bg="white");
    
    # need to consider norm data were edited, different from proc
    smpl.nms <- rownames(mSetObj$dataSet$norm);
    
    mns <- by(as.numeric(mSetObj$dataSet$proc[smpl.nms, cmpdNm]), mSetObj$dataSet$cls, mean, na.rm=T);
    sds <- by(as.numeric(mSetObj$dataSet$proc[smpl.nms, cmpdNm]), mSetObj$dataSet$cls, sd, na.rm=T);
    
    ups <- mns + sds;
    dns <- mns - sds;
    
    # all concentration need start from 0
    y <- c(0, dns, mns, ups);
    
    rg <- range(y) + 0.05 * diff(range(y)) * c(-1, 1)
    pt <- pretty(y)
    
    axp=c(min(pt), max(pt[pt <= max(rg)]),length(pt[pt <= max(rg)]) - 1);
    
    # ggplot alternative
    col <- unique(GetColorSchema(mSetObj));
    
    df.orig <- data.frame(value = as.vector(mns), name = levels(mSetObj$dataSet$cls), up = as.vector(ups), down = as.vector(dns))
    p.orig <- ggplot(df.orig, aes(x = name, y = value, fill = name)) + geom_bar(stat = "identity", colour = "black") + theme_bw()
    p.orig <- p.orig + scale_y_continuous(breaks=pt, limits = range(pt)) + ggtitle("Original Conc.")
    p.orig <- p.orig + theme(plot.title = element_text(size = 11, hjust=0.5)) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p.orig <- p.orig + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
    p.orig <- p.orig + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p.orig <- p.orig + geom_segment(aes(xend=name, y=up, yend=dns)) + scale_fill_manual(values=col)
    p.orig <- p.orig + theme(plot.margin = margin(t=0.35, r=0.5, b=0.15, l=0.15, "cm"), axis.text = element_text(size=10))
    
    df.norm <- data.frame(value=mSetObj$dataSet$norm[, cmpdNm], name = mSetObj$dataSet$cls)
    p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
    p.norm <- p.norm + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
    p.norm <- p.norm + stat_summary(fun.y=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
    p.norm <- p.norm + scale_fill_manual(values=col) + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p.norm <- p.norm + ggtitle("Normalized Conc.") + theme(plot.title = element_text(size = 11, hjust=0.5)) 
    p.norm <- p.norm + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p.norm <- p.norm + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    
    gridExtra::grid.arrange(p.orig, p.norm, ncol=2, top = grid::textGrob(paste(cmpdNm), gp=grid::gpar(fontsize=14, fontface="bold")))
    
    dev.off();
    
  }else if(mSetObj$dataSet$design.type =="time0"){
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=8, height= 6, type=format, bg="white");
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
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    
    groupNum <- length(levels(in.fac))
    pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
              "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928");
    col.fun <- grDevices::colorRampPalette(pal12)
    group_colors <- col.fun(groupNum)
    
    p_all <- list()
    
    for(lv in levels(out.fac)){
      inx <- out.fac == lv;
      df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = in.fac[inx])
      p_all[[lv]] <- df.orig
    }
    
    alldata <- do.call(rbind, p_all)
    
    p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + geom_boxplot(outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1) 
    p.time <- p.time + facet_wrap(~facA, nrow = row.num) + theme(axis.title.x = element_blank(), legend.position = "none")
    p.time <- p.time + scale_fill_manual(values=group_colors) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance")
    p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
  
    print(p.time)
    dev.off()
  }
  
  if(.on.public.web){
    return(imgName);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#' Read RDS files from the internet
#' @description Function downloads the required file and reads it only if not already in working directory.
#' Need to specify the file URL and the destfile. 
#' @param filenm Input the name of the file to download

# read binary RDS files
.read.metaboanalyst.lib <- function(filenm){
  if(.on.public.web){
    lib.path <- paste("../../libs/", filenm, sep="");
    return(readRDS(lib.path));
  }else{
    lib.download <- FALSE;
    if(!file.exists(filenm)){
      lib.download <- TRUE;
    }else{
      time <- file.info(filenm)
      diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
      if(diff_time>30){
        lib.download <- TRUE;
      }
    }
    # Deal with curl issues
    if(lib.download){
      lib.url <- paste("https://www.metaboanalyst.ca/resources/libs/", filenm, sep="");
      tryCatch(
        {
          download.file(lib.url, destfile=filenm, method="curl")
        }, warning = function(w){ print('warning in curl download') },
        error = function(e) {
          print("Download unsucceful. Ensure that curl is downloaded on your computer.")
          print("Attempting to re-try download using libcurl...")
          download.file(lib.url, destfile=filenm, method="libcurl")
        }
      )
    }
    lib.path <- filenm;
  }
  
  # Deal w. corrupt downloaded files
  tryCatch({
    my.lib <- readRDS(lib.path); # this is a returned value, my.lib never called outside this function, should not be in global env.
    print("Loaded files from MetaboAnalyst web-server.")
  },
  warning = function(w) { print('warning in read') },
  error = function(err) {
    print("Reading data unsuccessful, attempting to re-download file...")
    tryCatch(
      {
        lib.url <- paste("https://www.metaboanalyst.ca/resources/libs/", filenm, sep="");
        download.file(lib.url, destfile=filenm, method="curl")
        my.lib <- readRDS(lib.path);
        print("Loaded necessary files.")
      },
      warning = function(w) { print('warning in final try again') },
      error = function(err) {
        print("Loading files from server unsuccessful. Ensure curl is downloaded on your computer.")
      }
    )
  })
  return(my.lib)
}

#'Load KEGG library
#'@description Load different libraries
# libType: kegg/metpa, kegg/jointpa, msets
# libNm: for kegg org.code; for msets, specific names
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
LoadKEGGLib<-function(libType, libNm){
    
    destfile <- paste(libNm, ".rda", sep = "")
    
    if(.on.public.web){
        my.rda  <- paste("../../libs/", libType, "/", destfile, sep="");
     }else{
        my.rda <- paste("https://www.metaboanalyst.ca/resources/libs/", libType, "/", destfile, sep="");
    }

    print(paste("adding library:", my.rda));
    if(libType!= "msets"){
        load_igraph();
    }
    if(.on.public.web){
        load(my.rda, .GlobalEnv);
    }else if(!file.exists(destfile)){
        download.file(my.rda, destfile);
        load(destfile, .GlobalEnv);
    }else{
        load(destfile, .GlobalEnv);  
    }
}

# load old (2018 version)
LoadKEGGLib_2018<-function(mSetObj=NA, libOpt){

  mSetObj <- .get.mSet(mSetObj);
  org.code <- mSetObj$org;
  anal.type <- mSetObj$analSet$type;
  
  if(anal.type == ""){ #metpa
     if(.on.public.web){
        kegg.rda  <- paste("../../libs/kegg/2018/metpa/", org.code, ".rda", sep="");
     }else{
        kegg.rda <- paste("https://www.metaboanalyst.ca/resources/libs/kegg/2018/metpa/", org.code, ".rda", sep="");
     }
  }else{ # joint pathway
    if(.on.public.web){
        kegg.rda <- paste("../../libs/kegg/2018/jointpa/", libOpt, "/", org.code, ".rda", sep=""); 
    }else{
        kegg.rda <- paste("https://www.metaboanalyst.ca/resources/libs/kegg/2018/jointpa/", libOpt, "/", org.code, ".rda", sep=""); 
    }
  }
  print(paste("adding library:", kegg.rda));
  destfile <- paste(org.code, ".rda", sep = "")
  load_igraph();
  if(.on.public.web){
    load(kegg.rda, .GlobalEnv);
  }else if(!file.exists(destfile)){
    download.file(kegg.rda, destfile);
    load(destfile, .GlobalEnv);
  }else{
    load(destfile, .GlobalEnv);  
  }
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetErrMsg<-function(){
  return(err.vec);
}

GetKEGG.PathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(names(metpa$path.ids));
}

#'Given a vector containing KEGGIDs, returns a vector of KEGG compound names
#'@description This function, given a vector containing KEGGIDs, returns a vector of KEGG compound names.
#'@param ids Vector of KEGG ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2Name<-function(ids){
  cmpd.db <- .read.metaboanalyst.lib("compound_db.rds");
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, 3]);
}

#'Given a vector containing KEGG pathway IDs, return a vector containing SMPDB IDs (only for hsa)
#'@description This function, when given a vector of KEGG pathway IDs, return a vector of SMPDB IDs (only for hsa).
#'SMPDB standing for the Small Molecule Pathway Database, and hsa standing for human serum albumin. 
#'@param ids Vector of KEGG pathway IDs
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
#'@param ids Input the vector of HMDB Ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2Name<-function(ids){
  cmpd.db <- .read.metaboanalyst.lib("compound_db.rds");
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "name"]);
}

#'Given a vector of KEGGIDs, return a vector of HMDB ID
#'@description This functionn, when given a vector of KEGGIDs, returns a vector of HMDB IDs. HMDB standing
#'for the Human Metabolome Database. 
#'@param ids Vector of KEGG ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2HMDBID<-function(ids){
  
  cmpd.db <- .read.metaboanalyst.lib("compound_db.rds");
  
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, "hmdb_id"]);
}

#'Given a vector of HMDBIDs, return a vector of KEGG IDs
#'@description This function, when given a vector of HMDBIDs, returns a vector of KEGG ID. HMDB standing
#'for the Human Metabolome Database. 
#'@param ids Input the vector of HMDB Ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2KEGGID<-function(ids){
  cmpd.db <- .read.metaboanalyst.lib("compound_db.rds");
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "kegg_id"]);
}

#'Save compound name for mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param qvec Input the vector to query
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.MapData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpd <- qvec;
  return(.set.mSet(mSetObj));
}

#'Save adduct names for mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param qvec Input the vector to query
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.AdductData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$adduct.list <- qvec;
  mSetObj$adduct.custom <- TRUE
  return(.set.mSet(mSetObj));
}

#'Save concentration data
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param conc Input the concentration data
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param type Input the biofluid type
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

#'Set organism for further analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param org Set organism ID
#'@export
SetOrganism <- function(mSetObj=NA, org){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$org <- org;
  return(.set.mSet(mSetObj))
}


convertPNG2PDF <- function(filenm){
  library(png)
  
  nm <- strsplit(filenm, "[.]")[[1]][1]
  img <- readPNG(filenm)

  pdf(paste0(nm, ".pdf"))
  grid::grid.raster(img)
  dev.off()  
  
  return(1)
}
