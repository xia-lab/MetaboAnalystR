
.load.scripts.on.demand <- function(fileName=""){
    complete.path <- paste0("../../rscripts/MetaboAnalystR/R/", fileName);
    compiler::loadcmp(complete.path);
}

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

#'Constructs a dataSet object for storing data 
#'@description This functions handles the construction of a mSetObj object for storing data for further processing and analysis.
#'It is necessary to utilize this function to specify to MetaboAnalystR the type of data and the type of analysis you will perform. 
#'@usage InitDataObjects(data.type, anal.type, paired=FALSE)
#'@param data.type The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data)
#'@param anal.type Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, mf, 
#'cmpdmap, smpmap, or pathinteg
#'@param paired Indicate if the data is paired or not. Logical, default set to FALSE
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'@import methods

InitDataObjects <- function(data.type, anal.type, paired=FALSE){
  
  if(!.on.public.web){
    if(exists("mSet")){
      mSetObj <- .get.mSet(mSet);
      mSetObj$dataSet$type <- data.type;
      mSetObj$dataSet$paired <- paired;
      mSetObj$dataSet$pair.checked <- FALSE;
      mSetObj$analSet$type <- anal.type;
      mSetObj<-CleanDataObjects(mSetObj, anal.type);
      return(.set.mSet(mSetObj));
    }
  }

  dataSet <- list();
  dataSet$type <- data.type;
  dataSet$design.type <- "regular"; # one factor to two factor
  dataSet$cls.type <- "disc"; # default until specified otherwise
  dataSet$format <- "rowu";
  dataSet$paired <- paired;
  dataSet$pair.checked <- FALSE;
  analSet <- list();
  analSet$type <- anal.type;
  Sys.setenv("OMP_NUM_THREADS" = 2); # to control parallel computing for some packages
  Sys.setenv("OPENBLAS_NUM_THREADS" = 2);
  mSetObj <- list();
  mSetObj$dataSet <- dataSet;
  mSetObj$analSet <- analSet;
  mSetObj$imgSet <- list();
  mSetObj$msgSet <- list(); # store various message during data processing
  mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  mSetObj$cmdSet <- vector(mode="character"); # store R command
  
  if (anal.type == "mummichog") {
    # Define this parameter set to avoid global variable
    # Author: Zhiqiang
    mSetObj$paramSet$mumRT <- NA;
    mSetObj$paramSet$mumRT.type <- NA;
    mSetObj$paramSet$version <- NA;
    mSetObj$paramSet$mumDataContainsPval <- 1;
    mSetObj$paramSet$mode <- NA;
    mSetObj$paramSet$adducts <- NA;
    mSetObj$paramSet$peakFormat <- "mpt";
  } else if (anal.type == "metapaths") {
    # Define this parameter set to avoid global variable
    # Author: Zhiqiang
    paramSet <- list();
    paramSet$mumRT <- NA;
    paramSet$mumRT.type <- NA;
    paramSet$version <- NA;
    paramSet$mumDataContainsPval <- 1;
    paramSet$mode <- NA;
    paramSet$adducts <- NA;
    paramSet$peakFormat <- "mpt";
    paramSet$metaNum <- 0;
    mSetObj$paramSet <- paramSet;
    # This is an empty paramSet, and will be copied for multiple datasets
    dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
    if(length(dataNMs)>0){
      for(n in dataNMs){
        mSetObj[[n]] <- NULL;
      }
    }
  }
  
  .init.global.vars(anal.type);
  print("MetaboAnalyst R objects initialized ...");
  return(.set.mSet(mSetObj));
}

# Clean Data Objects to avoid interference
CleanDataObjects <- function(mSetObj, anal.type){
  if(anal.type == "metapaths") {
    # Define this parameter set to avoid global variable
    # Author: Zhiqiang
    paramSet <- list();
    paramSet$mumRT <- NA;
    paramSet$mumRT.type <- NA;
    paramSet$version <- NA;
    paramSet$mumDataContainsPval <- 1;
    paramSet$mode <- NA;
    paramSet$adducts <- NA;
    paramSet$peakFormat <- "mpt";
    paramSet$metaNum <- 0;
    mSetObj$paramSet <- paramSet;
    # This is an empty paramSet, and will be copied for multiple datasets
    dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
    if(length(dataNMs)>0){
      for(n in dataNMs){
        mSetObj[[n]] <- NULL;
      }
    }
  }
  return(mSetObj)
}


# for switching from spec to other modules
PrepareSpec4Switch <- function(){
  InitDataObjects("conc", "stat", FALSE);
  #TableFormatCoerce("metaboanalyst_input.csv", "OptiLCMS", "mummichog");
  Read.TextData(NA, "metaboanalyst_input.csv", "colu", "disc");
}

# this is for switching module
UpdateDataObjects <- function(data.type, anal.type, paired=FALSE){
  
  mSetObj <- .get.mSet(NA);
  mSetObj$dataSet$type <- data.type;
  mSetObj$analSet$type <- anal.type;
  .init.global.vars(anal.type);    
  
  # some specific setup 
  if(anal.type == "mummichog"){
    .set.mSet(mSetObj);
    mSetObj<-.init.MummiMSet(mSetObj);
    load("params.rda"); 
    mSet$paramSet$mumDataContainsPval <<- 1;
    
    mSetObj<-UpdateInstrumentParameters(mSetObj, peakParams$ppm, peakParams$polarity, "yes", 0.02);
    mSetObj<-.rt.included(mSetObj, "seconds");
    #mSetObj<-Read.TextData(mSetObj, "metaboanalyst_input.csv", "colu", "disc");
    mSetObj<-.get.mSet(NA);
  }
  
  return(.set.mSet(mSetObj));
}

.init.MummiMSet <- function(mSetObj) {  
  mSetObj<-SetPeakFormat(mSetObj, "pvalue");
  #TableFormatCoerce("metaboanalyst_input.csv", "OptiLCMS", "mummichog");
  anal.type <<- "mummichog";
  api.base <<- "http://api.xialab.ca";
  return(mSetObj)
}

.init.global.vars <- function(anal.type){
  # other global variables
  
  # for network analysis
  module.count <<- 0;
  # counter for naming different json file (pathway viewer)
  smpdbpw.count <<- 0; 
  # for mummichog
  #peakFormat <<- "mpt"  
  # mumRT.type <<- "NA";
  
  # raw data processing
  # rawfilenms.vec <<- vector(); # Disable for now
  
  # for meta-analysis
  mdata.all <<- list(); 
  mdata.siggenes <<- vector("list");
  meta.selected <<- TRUE;
  anal.type <<- anal.type;
  
  if(.on.public.web){
    # disable parallel prcessing for public server
    library(BiocParallel);
    register(SerialParam());
  } else {
    if("stat" %in% anal.type | 
       "msetqea" %in% anal.type | 
       "pathqea" %in% anal.type | 
       "roc" %in% anal.type)
      # start Rserve engine for Rpackage
      load_Rserve();
  }
  
  # plotting required by all
  Cairo::CairoFonts(regular="Arial:style=Regular",bold="Arial:style=Bold",italic="Arial:style=Italic",bolditalic = "Arial:style=Bold Italic",symbol = "Symbol")
  
  # sqlite db path for gene annotation
  if(file.exists("/home/glassfish/sqlite/")){ #.on.public.web
    url.pre <<- "/home/glassfish/sqlite/";
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){ # xia local
    url.pre <<- "/Users/jeffxia/Dropbox/sqlite/";
  }else if(file.exists("/Users/jeffxia/Dropbox/sqlite/")){ # xia local2
    url.pre <<- "/Users/jeffxia/Dropbox/sqlite/";
  }else if(file.exists("/media/zzggyy/disk/sqlite/")){
    url.pre <<-"/media/zzggyy/disk/sqlite/"; #zgy local)
  }else if(file.exists("/home/zgy/sqlite/")){
    url.pre <<-"/home/zgy/sqlite/"; #zgy local)
  } else if(file.exists("/home/le/sqlite/")){# le local
    url.pre <<-"/home/le/sqlite/";
  } else if(file.exists("/Users/jessicaewald/sqlite/")){# jess local
    url.pre <<-"/Users/jessicaewald/sqlite/";
  }else if(file.exists("/home/qiang/Music/")){# qiang local
    url.pre <<-"/home/qiang/sqlite/";
  }else{
    #url.pre <<- paste0(dirname(system.file("database", "sqlite/GeneID_25Species_JE/ath_genes.sqlite", package="MetaboAnalystR")), "/")
    url.pre <<- "";
  }
  
  api.base <<- "http://api.xialab.ca"
  
}
#'For two factor time series only
#'@description For two factor time series only
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param design Input the design type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
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
  pid.info <- paste0("# PID of current job: ", Sys.getpid());
  cmds <- c(pid.info, cmds);
  write(cmds, file = "Rhistory.R", append = FALSE);
}

#'Export R Command History
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetRCommandHistory <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj); 
  if(length(mSetObj$cmdSet) == 0){
    return("No commands found");
  }
  return(mSetObj$cmdSet);
}

#'Read.TextData
#'Constructor to read uploaded CSV or TXT files into the dataSet object
#'@description This function handles reading in CSV or TXT files and filling in the dataSet object 
#'created using "InitDataObjects". 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param filePath Input the path name for the CSV/TXT files to read.
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@param lbl.type Specify the data label type, either categorical (disc) or continuous (cont).
#'@param nmdr Boolean. Default set to FALSE (data is uploaded by the user and not fetched
#'through an API call to the Metabolomics Workbench).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'License: MIT License
#'@export

Read.TextData <- function(mSetObj=NA, filePath, format="rowu", 
                          lbl.type="disc", nmdr = FALSE){

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cls.type <- lbl.type;
  mSetObj$dataSet$format <- format;
  
  if(nmdr){
    dat <- qs::qread("nmdr_study.qs")
  }else{
    dat <- .readDataTable(filePath);
  }
  
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
  
  if(substring(format,4,5)=="mf"){
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

    metadata <- data.frame(metaA=as.factor(as.character(facA)), metaB=as.factor(as.character(facB)), stringsAsFactors=T);
    colnames(metadata) <- c(facA.lbl, facB.lbl)
    mSetObj$dataSet$meta.info <- metadata
    mSetObj$dataSet$types.cls.lbl <- sapply(metadata, function(x) class(x) ) 
    mSetObj$dataSet$meta.types <- c("disc", "disc");
    names(mSetObj$dataSet$types.cls.lbl) <- c(facA.lbl, facB.lbl)
    names(mSetObj$dataSet$meta.types) <- c(facA.lbl, facB.lbl)

  }else{
    
    if(substring(format,1,3)=="row"){ # sample in row
      msg <- c(msg, "Samples are in rows and features in columns");
      smpl.nms <-dat[,1];
      dat[,1] <- NULL; #remove sample names
      if(lbl.type == "qc"){
        rownames(dat) <- smpl.nms;
        #mSetObj$dataSet$orig <- dat;
        qs::qsave(dat, file="data_orig.qs");
        mSetObj$dataSet$cmpd <- colnames(dat);
        return(1);
      }
      cls.lbl <- dat[,1];
      conc <- dat[,-1, drop=FALSE];
      var.nms <- colnames(conc);
      if(lbl.type == "no"){ #no class label
        cls.lbl <- rep(1, nrow(dat));
        conc <- dat[,, drop=FALSE]; 
        var.nms <- colnames(conc);
      }
    }else{ # sample in col
      msg<-c(msg, "Samples are in columns and features in rows.");
      if(lbl.type == "no"){
        cls.lbl <- rep(1, ncol(dat));
        conc <- t(dat[,-1]);
        var.nms <- dat[,1];
        smpl.nms <- colnames(dat[,-1]);
      }else{
        var.nms <- dat[-1,1];
        dat[,1] <- NULL;
        smpl.nms <- colnames(dat);
        cls.lbl <- dat[1,];
        conc <- t(dat[-1,]);
      }
    }
  }
  
  mSetObj$dataSet$type.cls.lbl <- class(cls.lbl);
  
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
  
  if(anal.type == "mummichog"){
    is.rt <- mSetObj$paramSet$mumRT;
    if(!is.rt){
      mzs <- as.numeric(var.nms);
      if(sum(is.na(mzs) > 0)){
        AddErrMsg("Make sure that feature names are numeric values (mass or m/z)!");
        return(0);
      }
    }
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

  # black slash could kill Rserve immediately
  smpl.nms <- gsub("\\\\", "-", smpl.nms);
  url.smp.nms <- CleanNames(smpl.nms);
  names(url.smp.nms) <- smpl.nms;
  
  var.nms <- gsub("\\\\", "-", var.nms);
  url.var.nms <- CleanNames(var.nms); # allow space, comma and period
  names(url.var.nms) <- var.nms;
  
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  # check if paired or not
  if(mSetObj$dataSet$paired){
    # save as it is and process in sanity check step
    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$pairs <- cls.lbl;
  } else {
    if(lbl.type == "disc"){
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.factor(as.character(cls.lbl));
      
      if(substring(format,4,5)=="mf"){
        
        mSetObj$dataSet$facA.type <- is.numeric(facA);
        mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA <- as.factor(as.character(facA));
        mSetObj$dataSet$facA.lbl <- facA.lbl;
        
        mSetObj$dataSet$facB.type <- is.numeric(facB);
        mSetObj$dataSet$orig.facB <- mSetObj$dataSet$facB <- as.factor(as.character(facB));
        mSetObj$dataSet$facB.lbl <- facB.lbl;
      }
      
    } else { # continuous
      
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- tryCatch({
        as.numeric(cls.lbl);
      },warning=function(na) {
        print("Class labels must be numeric and continuous!");
        return(0);
      })
      
      if(!is.numeric(mSetObj$dataSet$cls) || any(is.na(mSetObj$dataSet$cls))){
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
  mSetObj$dataSet$url.var.nms <- url.var.nms;
  mSetObj$dataSet$url.smp.nms <- url.smp.nms;
  #mSetObj$dataSet$orig <- conc; # copy to be processed in the downstream
  qs::qsave(conc, file="data_orig.qs");
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));
  
  return(.set.mSet(mSetObj));
}


#'Read paired peak or spectra files
#'@description This function reads paired peak lists or spectra files. The pair information
#'is stored in a file where each line is a pair and names are separated by ":".
#'@param filePath Set file path
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
ReadPairFile <- function(filePath="pairs.txt"){
  all.pairs <- scan(filePath, what='character', strip.white = T);
  labels <- as.vector(rbind(1:length(all.pairs), -(1:length(all.pairs))));
  all.names <- NULL;
  for(i in 1:length(all.pairs)){
    all.names=c(all.names, unlist(strsplit(all.pairs[i],":", fixed=TRUE), use.names=FALSE));
  }
  names(labels) <- all.names;
  return(labels);
}

#'Save the processed data with class names
#'@description This function saves the processed data with class names as CSV files. 
#'Several files may be generated, the original data, processed data, peak normalized, and/or normalized data. 
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
SaveTransformedData <- function(mSetObj=NA){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.save.data")){ # public web on same user dir
      .load.scripts.on.demand("util_savedata.Rc");    
    }
    return(my.save.data(mSetObj));
  }else{
    return(my.save.data(mSetObj));
  }
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
Read.mzTab <- function(mSetObj=NA, filename, identifier = "name") {
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.parse.mztab")){ # public web on same user dir
      .load.scripts.on.demand("util_mztab.Rc");    
    }
    return(my.parse.mztab(mSetObj, filename, identifier));
  }else{
    return(my.parse.mztab(mSetObj, filename, identifier));
  }
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
#'License: MIT License
#'@import qs
#'@export

Read.PeakList<-function(mSetObj=NA, foldername="upload"){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.parse.peaklist")){ # public web on same user dir
      .load.scripts.on.demand("util_peaks.Rc");    
    }
    return(my.parse.peaklist(mSetObj, foldername));
  }else{
    return(my.parse.peaklist(mSetObj, foldername));
  }
}

#' Adds an error message
#'@description The error message will be printed in all cases.
#'Used in higher functions. 
#'@param msg Error message to print 
#'@export
AddErrMsg <- function(msg){
  if(!exists("err.vec")){
    err.vec <<- "";
  }
  err.vec <<- c(err.vec, msg);
  print(msg);
}

GetErrMsg<-function(){
  if(!exists("err.vec")){
    err.vec <<- "Unknown Error Occurred";
  }
  return(err.vec);
}

# general message only print when running local
AddMsg <- function(msg){
  if(!exists("msg.vec")){
    msg.vec <<- "";
  }
  msg.vec <<- c(msg.vec, msg);
  if(!.on.public.web){
    print(msg);
  }
}

# return the latest message
GetCurrentMsg <- function(){
  if(!exists("msg.vec")){
    msg.vec <<- "";
  }
  return(msg.vec[length(msg.vec)]);
}

#'Plot compound summary
#'change to use dataSet$proc instead of dataSet$orig in
#'case of too many NAs
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpdNm Input the name of the compound to plot
#'@param format Input the format of the image to create
#'@param dpi Input the dpi of the image to create
#'@param width Input the width of the image to create
#'@param meta meta is "NA"
#'@param version version
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'

PlotCmpdSummary <- function(mSetObj=NA, cmpdNm, meta="NA", version, format="png", dpi=72, width=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
    load_grid()
  }
  
  meta.info <- mSetObj$dataSet$meta.info
  
  if(meta == "NA"){
    if(mSetObj$dataSet$design.type == "multi"){
    sel.cls <- meta.info[,2]
    cls.type <- unname(mSetObj$dataSet$meta.types[2])
    }else{
    sel.cls <- mSetObj$dataSet$cls
    cls.type <- mSetObj$dataSet$cls.type
    }
  }else{
    sel.cls <- meta.info[,meta]
    cls.type <- unname(mSetObj$dataSet$meta.types[meta])
    
  }
  
  imgName <- mSetObj$dataSet$url.var.nms[cmpdNm];
  if(meta != "NA"){
    imgName <- paste(imgName, "_", meta, "_", version, "_summary_dpi", dpi, ".", format, sep="");
  }else{
    imgName <- paste(imgName, "_", version, "_summary_dpi", dpi, ".", format, sep="");
  }
  
  if(is.na(width)){
    w <- 7.5;
  }else{
    w <- width;
  }
  
  if(!mSetObj$dataSet$design.type %in% c("time", "time0", "multi")){

    proc.data <- qs::qread("data_proc.qs");
    
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height= w*0.65, type=format, bg="white");
    
    # need to consider norm data were edited, different from proc
    smpl.nms <- rownames(mSetObj$dataSet$norm);
    proc.data <- proc.data[smpl.nms, ];

    # ggplot alternative
    col <- unique(GetColorSchema(sel.cls));
    
 ## plot orignal and normalize side by side
    if(cls.type == "disc"){
      df.orig <- data.frame(value=proc.data[, cmpdNm], name = sel.cls)
      p.orig <- ggplot2::ggplot(df.orig, aes(x=name, y=value, fill=name))  
      p.orig <- p.orig + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
      p.orig <- p.orig + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.orig <- p.orig + stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
      p.orig <- p.orig + scale_fill_manual(values=col) + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.orig <- p.orig + ggtitle("Original Conc.") + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.orig <- p.orig + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      p.orig <- p.orig + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }else{
      df.orig <- data.frame(value=proc.data[, cmpdNm], name = as.numeric(as.character(sel.cls)))
      p.orig <- ggplot2::ggplot(df.orig, aes(x=name, y=value)) 
      p.orig <- p.orig + geom_point(aes(col = -value), size = 1.5) + theme_bw() 
      p.orig <- p.orig + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.orig <- p.orig + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.orig <- p.orig + ggtitle("Normalized Conc.") + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.orig <- p.orig + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      p.orig <- p.orig + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }
##   


    if(cls.type == "disc"){
      df.norm <- data.frame(value=mSetObj$dataSet$norm[, cmpdNm], name = sel.cls)
      p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name))  
      p.norm <- p.norm + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
      p.norm <- p.norm + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.norm <- p.norm + stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
      p.norm <- p.norm + scale_fill_manual(values=col) + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.norm <- p.norm + ggtitle("Normalized Conc.") + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.norm <- p.norm + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      p.norm <- p.norm + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }else{
      df.norm <- data.frame(value=mSetObj$dataSet$norm[, cmpdNm], name = as.numeric(as.character(sel.cls)))
      p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value)) 
      p.norm <- p.norm + geom_point(aes(col = -value), size = 1.5) + theme_bw() 
      p.norm <- p.norm + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.norm <- p.norm + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.norm <- p.norm + ggtitle("Normalized Conc.") + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.norm <- p.norm + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      p.norm <- p.norm + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }

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
      cls.type = "disc";
    }else{ # factor a split within factor b
      out.fac <- mSetObj$dataSet$meta.info[,1];
      in.fac <- sel.cls;
      xlab = colnames(meta.info)[1]
      cls.type = cls.type;
    }
    
    # two images per row
    img.num <- length(levels(out.fac));
    row.num <- ceiling(img.num/2)
    
    if(row.num == 1){
      h <- w*5/9;
    }else{
      h <- w*0.5*row.num;
    }

    if(cls.type == "cont"){
        h <- h +1;
    }
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    
    col <- unique(GetColorSchema(in.fac));
    
    p_all <- list()
    
    for(lv in levels(out.fac)){
      #print(lv)
      inx <- out.fac == lv;
      if(cls.type == "disc"){
      df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = in.fac[inx])
      }else{
      df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = as.numeric(as.character(in.fac[inx])))
      }
      p_all[[lv]] <- df.orig
    }

    alldata <- do.call(rbind, p_all)
    alldata$facA <- factor(as.character(alldata$facA), levels=levels(out.fac))

    if(cls.type == "disc"){
        p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + geom_boxplot(outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1) 
        p.time <- p.time + facet_wrap(~facA, nrow = row.num) + theme(axis.title.x = element_blank(), legend.position = "none")
        p.time <- p.time + scale_fill_manual(values=col) + theme(axis.text.x = element_text(angle=90, hjust=1))
        p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance")
        p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
        p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
    }else{
        p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=facA, colour=facA, shape=facA, group=facA)) + geom_point(size=2) + theme_bw()  + geom_smooth(aes(fill=facA),method=lm,se=T)     
        p.time <- p.time + theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")
        p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") +xlab(meta)
        p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
        p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
    }
    
    print(p.time)
    dev.off()
  }
  
  if(.on.public.web){
    return(imgName);
  }else{
    return(.set.mSet(mSetObj));
  }
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Save compound name for mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param qvec Input the vector to query
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
Setup.MapData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpd <- qvec;
  return(.set.mSet(mSetObj));
}

GetMetaInfo <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$dataSet$design.type == "regular"){
    return("Group");
  }else{
    return(c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl));
  }
}

#'Get all group names
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param exp.fac exp.fac
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
# 
GetGroupNames <- function(mSetObj=NA, exp.fac=NA){
  mSetObj <- .get.mSet(mSetObj);  
  if(mSetObj$dataSet$design.type == "regular"){
    cls.lbl <- mSetObj$dataSet$prenorm.cls;
    if(mSetObj$analSet$type=="roc"){
      empty.inx <- is.na(cls.lbl) | cls.lbl == "";
      # make sure re-factor to drop level
      my.cls <- factor(cls.lbl[!empty.inx]);
    }else{
      my.cls <- cls.lbl;
    }
  }else{
    if(exp.fac == mSetObj$dataSet$facA.lbl){
      my.cls <- mSetObj$dataSet$facA;  
    }else{
      my.cls <- mSetObj$dataSet$facB;
    }
    my.fac <- exp.fac;
  }
  current.cls <<- my.cls;
  
  if(.on.public.web){
    return(levels(my.cls));
  }else{
    return(.set.mSet(mSetObj));
  }
}

# groups entering analysis
GetNormGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  levels(mSetObj$dataSet$cls);
}

GetOrigSmplGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.character(mSetObj$dataSet$orig.cls);
}

GetOrigSmplGroupNamesPerMeta <- function(mSetObj=NA, metaname="NA"){
  mSetObj <- .get.mSet(mSetObj);
  if(metaname %in% colnames(mSetObj$dataSet$meta.info)){
    as.character(mSetObj$dataSet$meta.info[,metaname]);
  }else if(!is.null(mSetObj$dataSet$meta.info[,1])) {
    as.character(mSetObj$dataSet$meta.info[,1]);
  }else {
    return(GetOrigSmplGroupNames(NA))
  }
}

GetPrenormSmplGroupNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.character(mSetObj$dataSet$prenorm.cls);
}

GetFilesToBeSaved <-function(naviString){
  partialToBeSaved <- c("Rload.RData");
  return(unique(partialToBeSaved));
}

GetExampleDataPath<-function(naviString){
  return(url.pre);
}


TableFormatCoerce <- function(oriFile = NULL, oriFormat = "Unknown", targetModule = "mummichog", sampleIn = "col"){
  
  df <- read.csv(oriFile);
  df[is.na(df)] <- df[df == ""] <- 0;
  
  if (targetModule == "mummichog") {
    ## This is used to convert the data format from Spectral Processing to Mummichog
    if (oriFormat == "OptiLCMS") {
      if (sampleIn == "col") {
        newFea <- strsplit(df[, 1], "@")
        
        newFea <- lapply(newFea, function(x) {
          return(paste0(x[1], "__", x[2]))
        })
      } else {
        # cannot be in row for OptiLCMS source
      }
      
      df[, 1] <- unlist(newFea)
      df[1, 1] <- "Label"  
    }
  }
  
  fast.write.csv(df, file = oriFile, row.names = FALSE)
  
}


checkDataGenericFormat <- function() {
  
  if(!file.exists("data_orig.qs")) return(0)
  
  dt <- qs::qread("data_orig.qs");
  if(all(sapply(colnames(dt), FUN = function(x) {grepl("__",x)}))) {
    return(1)
  } 
  if(all(sapply(colnames(dt), FUN = function(x) {grepl("@",x)}))) {
    return(1)
  }
  
  return(0)    
}

GetSampleNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  metaData <- mSetObj$dataSet$meta.info;
  return(rownames(metaData));
}

#'Function to retrieve all available datasets from the Metabolomics Workbench.
#'@description This function uses the httr R package to make an API
#'call to the Metabolomics Workbench to retrieve a table of
#'all compatible datasets.
#'@usage ListNMDRStudies(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'License: MIT License
#'@export
ListNMDRStudies <- function(mSetObj=NA){
  
  # The Metabolomics Workbench API url
  call <- "https://www.metabolomicsworkbench.org/rest/study/study_id/ST/named_metabolites"
  
  # Use httr::GET to send the request to the Metabolomics Workbench API
  # The response will be saved in query_results
  query_results <- httr::GET(call, encode = "json")
  
  # Check if response is ok
  # 200 is ok! 401 means an error has occured on the user's end.
  if(query_results$status_code!=200){
    AddErrMsg("REST url to Metabolomics Workbench failed!")
    return(0)
  }
  
  # Parse the response into a table
  query_results_text <- content(query_results, "text", encoding = "UTF-8")
  query_results_json <- rjson::fromJSON(query_results_text, flatten = TRUE)
  query_results_table <- t(rbind.data.frame(query_results_json))
  rownames(query_results_table) <- query_results_table[,1]
  
  # Keep studies with > 10 metabolites
  num_metabolites <- as.numeric(query_results_table[,"num_metabolites"])
  keep.inx <- num_metabolites > 10
  query_results_table_keep <- query_results_table[keep.inx, ]
  query_results_table_keep <- subset(query_results_table_keep, select = - c(analysis_id, analysis_type, 
                                                                            num_metabolites,
                                                                            ms_type, units))
  
  query_results_table_keep <- data.frame(query_results_table_keep)
  
  load_stringr()
  query_results_table_keep$lipid <- stringr::str_detect(query_results_table_keep$study_title, fixed("lipid", ignore_case=TRUE))
  
  mSetObj$dataSet$NMDR_studies <- query_results_table_keep
  
  if(!.on.public.web){
    fast.write.csv(query_results_table_keep, "all_metabolomics_workbench_studies.csv")
  }else{
    qs::qsave(query_results_table_keep, "metabolomics_workbench_studies.qs")
  }
  
  return(.set.mSet(mSetObj));
}


#'Function to retrieve dataset from the Metabolomics Workbench.
#'@description This function uses the httr R package to make an API
#'call to the Metabolomics Workbench to download and save a dataset
#'based on the Study ID into the current working directory.
#'@usage GetNMDRStudy(mSetObj=NA, StudyID)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param StudyID Input the StudyID of the study from the Metabolomics Workbench. 
#'Use the ListNMDRStudies function to obtain a list of all available studies
#'from the Metabolomics Workbench.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'License: MIT License
#'@export
GetNMDRStudy <- function(mSetObj=NA, StudyID){
  
    # make this lazy load
    if(!exists("my.get.nmdr.data")){ # public web on same user dir
      .load.scripts.on.demand("util_nmdr.Rc");    
    }
    res <- my.get.nmdr.data(StudyID);
    if(res){
        mSetObj <- .get.mSet(mSetObj);
        mSetObj$dataSet$NMDR_id <- StudyID;
        return(.set.mSet(mSetObj));
    }
    return(0);
}

