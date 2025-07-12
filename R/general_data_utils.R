
.load.scripts.on.demand <- function(fileName=""){
    mSetObj <- .get.mSet(NA)
  # Define the relative path that is independent of rpath
  relPath <- paste0("rscripts/MetaboAnalystR/R/", fileName)
  
  # Construct the complete path using the current rpath
  complete.path <- paste0(rpath, relPath)
  
  if(!file.exists(complete.path)){
      rpath <<- "../../../"
      complete.path <- paste0(rpath, relPath)
  } else {
      rpath <<- "../../"
  }
  
  # Initialize the loaded.scripts list if needed
  if(is.null(mSetObj$paramSet$loaded.scripts)){
    mSetObj$paramSet$loaded.scripts <- list()
  }
  
  # Only store the relative path (without rpath) to ensure uniqueness
  if(!relPath %in% mSetObj$paramSet$loaded.scripts) {
    mSetObj$paramSet$loaded.scripts <- c(mSetObj$paramSet$loaded.scripts, relPath)
  }
  
  # Load the script using the complete path
  compiler::loadcmp(complete.path)
}

Reload.scripts.on.demand <- function(){
  mSetObj <- .get.mSet(NA)
  
  # Check if there are any scripts saved
  if (is.null(mSetObj$paramSet$loaded.scripts) || length(mSetObj$paramSet$loaded.scripts) == 0) {
    message("No scripts to reload.")
    return(1)
  }
  
  # Determine the rpath prefix based on the first script's existence
  first_rel <- mSetObj$paramSet$loaded.scripts[[1]]
  first_complete_path <- paste0("../../", first_rel)
  if (!file.exists(first_complete_path)) {
    rpath <<- "../../../"
  } else {
    rpath <<- "../../"
  }
  
  # Reload each script using the determined rpath prefix
  for (relPath in mSetObj$paramSet$loaded.scripts) {
    complete_path <- paste0(rpath, relPath)
    compiler::loadcmp(complete_path)
  }
  
  return(1)
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
#'@param paired indicate if the data is paired or not. Logical, default set to FALSE
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import methods

InitDataObjects <- function(data.type, anal.type, paired=FALSE, default.dpi=default.dpi){
  rpath <<- "../../";
  default.dpi <<- default.dpi;
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

  moduleNms.vec <<- NULL;

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
  mSetObj$imgSet$margin.config <- list(
                   l = 50,
                   r = 50,
                   b = 20,
                   t = 20,
                   pad = 0.5
  )
  mSetObj$msgSet <- list(); # store various message during data processing
  mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  mSetObj$cmdSet <- vector(mode="character"); # store R command
  metaboanalyst_env <<- new.env(); # init a marker env for raw data processing
  
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
    mSetObj$paramSet$ContainsMS2 <- FALSE;
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
    paramSet$ContainsMS2 <- FALSE;
    mSetObj$paramSet <- paramSet;
    # This is an empty paramSet, and will be copied for multiple datasets
    dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
    if(length(dataNMs)>0){
      for(n in dataNMs){
        mSetObj[[n]] <- NULL;
      }
    }
  }
  mSetObj$paramSet$report.format <- "html"
  .init.global.vars(anal.type);
  print("MetaboAnalyst R objects initialized ...");
  return(.set.mSet(mSetObj));
}


#' Convert2AnalObject
#' This function is used to convert mSet object from raw spectra processing for the following analysis.
#' @param mSet mSet from raw spectral processing pipeline, OptiLCMS
#' @param data.type data type, should be 'spec'
#' @param anal.type analysis type, should be 'raw'
#' @param paired data is paired or not, use FALSE by default
#' @export
Convert2AnalObject <- function(mSet, data.type, anal.type, paired=FALSE){
  
  if((typeof(mSet) == "S4") & (class(mSet) == "mSet")){
    MSnResults = mSet@MSnResults
    MSnData = mSet@MSnData
  } else {
    stop("This is not a valid mSet from raw spectral processing!")
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
  analSet$ms2res <- MSnResults;
  analSet$ms2data <- MSnData;

  Sys.setenv("OMP_NUM_THREADS" = 2); # to control parallel computing for some packages
  Sys.setenv("OPENBLAS_NUM_THREADS" = 2);
  mSetObj <- list();
  mSetObj$dataSet <- dataSet;
  mSetObj$analSet <- analSet;
  mSetObj$imgSet <- list();
  mSetObj$imgSet$margin.config <- list(
    l = 50,
    r = 50,
    b = 20,
    t = 20,
    pad = 0.5
  )
  mSetObj$msgSet <- list(); # store various message during data processing
  mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  mSetObj$cmdSet <- vector(mode="character"); # store R command
  metaboanalyst_env <<- new.env(); # init a marker env for raw data processing
  mSetObj$paramSet$report.format <- "html"
  .init.global.vars(anal.type);
  print("MetaboAnalyst R objects initialized ...");
  return(mSetObj);
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
  api.base <<- "https://www.xialab.ca/api";
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
    primary.user <<- FALSE; # default 
  }else{
    primary.user <<- TRUE;
   }
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
  
  plink.path <<- "/home/glassfish/plink/";
  # sqlite db path for gene annotation
  if(file.exists("/data/sqlite/")){ #vip server
    url.pre <<- "/data/sqlite/";
    plink.path <<- "/home/glassfish/plink/";
  }else if(file.exists("/home/glassfish/sqlite/")){ #.on.public.web
    url.pre <<- "/home/glassfish/sqlite/";
    plink.path <<- "/home/glassfish/plink/";
  }else if(file.exists("/Users/xialab/Dropbox/sqlite/")){ # xia local
    url.pre <<- "/Users/xialab/Dropbox/sqlite/";
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){ # xia local
    url.pre <<- "/Users/jeffxia/Dropbox/sqlite/";
  }else if(file.exists("/Users/jeffxia/Dropbox/sqlite/")){ # xia local2
    url.pre <<- "/Users/jeffxia/Dropbox/sqlite/";
  }else if(file.exists("/media/zzggyy/disk/sqlite/")){
    url.pre <<-"/media/zzggyy/disk/sqlite/"; #zgy local)
  }else if(file.exists("/home/zgy/sqlite/")){
    url.pre <<-"/home/zgy/sqlite/"; #zgy local)
    plink.path <<- "/home/zgy/plink/";
  }else if(file.exists("/Users/lzy/sqlite/")){
   url.pre <<- "";
   # url.pre <<-"/Users/lzy/sqlite/"; #lzy local
    plink.path <<- "/Users/lzy/sqlite/plink/";
  }else if(file.exists("/home/qiang/Music/")){# qiang local
    url.pre <<-"/home/qiang/sqlite/";
  }else{
    #url.pre <<- paste0(dirname(system.file("database", "sqlite/GeneID_25Species_JE/ath_genes.sqlite", package="MetaboAnalystR")), "/")
    url.pre <<- "";
  }
  
  api.base <<- "https://www.xialab.ca/api"
  
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
  write(cmd, file = "Rhistory.R", append = TRUE);
  return(.set.mSet(mSetObj));
}

RecordSysMessage <- function(msg){

  if(!exists("sys.msg.vec")){
    sys.msg.vec <<- NULL;
  }

  # add time
  msg <- paste0("[", Sys.time(), "] ", msg); 
  sys.msg.vec <<- c(sys.msg.vec, msg);
  write(msg, file = "Project.log", append = TRUE);
}

GetSysMessages <- function(){
  if(!exists("sys.msg.vec")){
    sys.msg.vec <<- "No message available";
  }
  return(sys.msg.vec);
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
#'License: GNU GPL (>= 2)
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

  msg <- NULL;
  
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
  
  
  mSetObj$dataSet$type.cls.lbl <- class(cls.lbl);
  
  msg <- c(msg, "The uploaded file is in comma separated values (.csv) format.");

min.n.qc    <- 3   # lowest acceptable for %RSD
min.n.blank <- 2   # one before + one after

n.qc    <- sum(grepl("^\\s*qc\\s*$",    cls.lbl, ignore.case = TRUE))
n.blank <- sum(grepl("^\\s*blank\\s*$", cls.lbl, ignore.case = TRUE))

if (isTRUE(mSetObj$dataSet$containsQC) && n.qc < min.n.qc) {
  AddErrMsg(sprintf(
    "Only %d QC samples were detected; at least %d are required to compute QC statistics.",
    n.qc, min.n.qc))
  return(0)
}


if (isTRUE(mSetObj$dataSet$containsBlank) && n.blank < min.n.blank) {
  AddErrMsg(sprintf(
    "Only %d blank samples were detected; at least %d (start + end) are required to assess carry-over.",
    n.blank, min.n.blank))
  return(0)
}


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
  empty.inx <- is.na(cls.lbl) | cls.lbl == "" | cls.lbl == " "

  if(sum(empty.inx) > 0){
    if(mSetObj$analSet$type != "roc"){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
      smpl.nms <- smpl.nms[!empty.inx];
      cls.lbl <-  cls.lbl[!empty.inx];
      conc <- conc[!empty.inx, ];
    }else{
      # force all NA to empty string, otherwise NA will become "NA" class label
      cls.lbl[is.na(cls.lbl)] <- "Unknown";
      cls.lbl[empty.inx] <- "Unknown";
      msg <- c(msg, paste("<font color=\"orange\">", sum(empty.inx), "new samples</font> were detected from your data."));
    }
  }

  #if(anal.type == "roc"){
    #if(length(unique(cls.lbl[!empty.inx])) > 2){
    #  AddErrMsg("ROC analysis is only defined for two-group comparisions!");
    #  return(0);
    #}
  #}
 
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

  # replace black slash 
  smpl.nms <- gsub("\\\\", "-", smpl.nms);

  #url.smp.nms <- CleanNames(smpl.nms);
  # now use clean names for all, as spaces causes a lot of issues
  smpl.nms <- url.smp.nms <- CleanNames(smpl.nms);

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
  mSetObj$dataSet$meta.info <- data.frame(Class=mSetObj$dataSet$orig.cls);

  if(mSetObj[["analSet"]][["type"]] %in% c("roc", "dose")){
    rownames(mSetObj$dataSet$meta.info) <- smpl.nms;
    mSetObj$dataSet$meta.types <- c("disc");
    mSetObj$dataSet$meta.status <- c("OK");
    names(mSetObj$dataSet$meta.types) <- "Class";
    names(mSetObj$dataSet$meta.status) <- "Class";
  }

  # for the current being to support MSEA and MetPA
  if(mSetObj$dataSet$type == "conc"){
    mSetObj$dataSet$cmpd <- var.nms;
  }

  mSetObj$dataSet$mum.type <- "table";
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
#'License: GNU GPL (>= 2)
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
#'License: GNU GPL (>= 2)
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
#'License: GNU GPL (>= 2)
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

SetCmpdSummaryType <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$paramSet$cmpdSummaryType <- type;
  .set.mSet(mSetObj);
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
#'@param meta2 only applicable for multifac module, secondary factor
#'@param count img count number
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotCmpdSummary <- function(mSetObj=NA, cmpdNm, meta="NA", meta2="NA",count=0, format="png", dpi=default.dpi, width=NA){
  #save.image("plot.RData");
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$paramSet$cmpdSummaryType)){
    mSetObj$paramSet$cmpdSummaryType <- "violin";
  }
  
  plotType <- mSetObj$paramSet$cmpdSummaryType

  if(.on.public.web){
    load_ggplot()
    load_grid()
  }
  
  meta.info <- mSetObj$dataSet$meta.info
  
  if(meta == "NA"){
    if(mSetObj$dataSet$design.type == "multi"){
      sel.cls <- meta.info[,1]
      cls.type <- unname(mSetObj$dataSet$meta.types[1])
    }else{
      sel.cls <- mSetObj$dataSet$cls
      cls.type <- mSetObj$dataSet$cls.type
    }
  }else{
    if(meta2 == "NA"){
      inx <- 2;
    }else{
      inx <- meta2;
    }
    sel.cls <- meta.info[,meta]
    
    cls.type <- unname(mSetObj$dataSet$meta.types[meta])
    cls.type2 <- unname(mSetObj$dataSet$meta.types[inx])
  }
  
  imgName <- mSetObj$dataSet$url.var.nms[cmpdNm];
  imgName <- gsub("@", "_", imgName); # replace special characters;
  if(meta != "NA"){
    imgName <- paste(imgName, "_", meta, "_", count, "_summary_dpi", dpi, ".", format, sep="");
  }else{
    imgName <- paste(imgName, "_", count, "_summary_dpi", dpi, ".", format, sep="");
  }
  
  my.w <- 7.5;
  my.h <- my.w * 0.65;
  
  mSetObj$imgSet$cmpdSum <- imgName;
  
  if(!mSetObj$dataSet$design.type %in% c("time", "time0", "multi") || meta2 == "NA"){
    ## Load processed data and align to normalized samples
    proc.data <- qs::qread("data_proc.qs")
    smpl.nms  <- rownames(mSetObj$dataSet$norm)
    proc.data <- proc.data[smpl.nms, , drop = FALSE]
    
    ## Open Cairo device
    Cairo::Cairo(
      file   = imgName,
      unit   = "in",
      dpi    = dpi,
      width  = my.w,
      height = my.h,
      type   = format,
      bg     = "white"
    )
    
    ## Precompute color scheme for discrete cases
    col <- unique(GetColorSchema(sel.cls))
    
    ## If grouping is discrete
    if (cls.type == "disc") {
      # ORIGINAL concentrations
      df.orig <- data.frame(value = proc.data[, cmpdNm], group = factor(sel.cls))
      p.orig <- ggplot(df.orig, aes(x = group, y = value, fill = group)) +
        (if (plotType == "violin") geom_violin(trim = FALSE)
         else geom_boxplot(outlier.shape = NA, outlier.colour = NA)) +
        geom_jitter(size = 1) +
        stat_summary(fun = mean, colour = "yellow", geom = "point", shape = 18, size = 3, show.legend = FALSE) +
        scale_fill_manual(values = col) +
        labs(title = "Original Conc.", x = NULL, y = NULL) +
        theme_bw() +
        theme(
          plot.title   = element_text(size = 11, hjust = 0.5, face = "bold"),
          axis.text.x  = element_text(angle = 90, hjust = 1),
          panel.grid   = element_blank(),
          plot.margin  = margin(t = 0.35, r = 0.25, b = 0.15, l = 0.5, "cm"),
          axis.text    = element_text(size = 10),
          legend.position = "none"
        )
      
      # NORMALIZED concentrations
      df.norm <- data.frame(value = mSetObj$dataSet$norm[, cmpdNm], group = factor(sel.cls))
      p.norm <- ggplot(df.norm, aes(x = group, y = value, fill = group)) +
        (if (plotType == "violin") geom_violin(trim = FALSE)
         else geom_boxplot(outlier.shape = NA, outlier.colour = NA)) +
        geom_jitter(size = 1) +
        stat_summary(fun = mean, colour = "yellow", geom = "point", shape = 18, size = 3, show.legend = FALSE) +
        scale_fill_manual(values = col) +
        labs(title = "Normalized Conc.", x = NULL, y = NULL) +
        theme_bw() +
        theme(
          plot.title   = element_text(size = 11, hjust = 0.5, face = "bold"),
          axis.text.x  = element_text(angle = 90, hjust = 1),
          panel.grid   = element_blank(),
          plot.margin  = margin(t = 0.35, r = 0.25, b = 0.15, l = 0.5, "cm"),
          axis.text    = element_text(size = 10),
          legend.position = "none"
        )
      
    } else {
      ## cls.type == "cont": continuous grouping for sel.cls → use as color gradient
      ## Continuous grouping: use sel.cls as color gradient
      xvar        <- as.numeric(as.character(sel.cls))
      color.range <- range(xvar, na.rm = TRUE)
      
      df.orig <- data.frame(x = xvar, y = proc.data[, cmpdNm])

      p.orig <- ggplot(df.orig, aes(x = x, y = y)) +
        geom_point(size = 1.8, color = "black") +        # points are black
        geom_smooth(method = "lm", se = TRUE, color = "blue") +  # line is blue
        labs(title = "Original Conc.", x = NULL, y = NULL) +
        theme_bw() + theme(
            plot.title      = element_text(size = 11, hjust = 0.5, face = "bold"),
            axis.text.x     = element_text(angle = 90, hjust = 1),
            panel.grid      = element_blank(),
            plot.margin     = margin(t = 0.35, r = 0.25, b = 0.15, l = 0.5, "cm"),
            axis.text       = element_text(size = 10),
            legend.position = "none"
        );

      df.norm <- data.frame(x = xvar, y = mSetObj$dataSet$norm[, cmpdNm])

      p.norm <- ggplot(df.norm, aes(x = x, y = y)) +
        geom_point(size = 1.8, color = "black") +  # points are now all black
        geom_smooth(method = "lm", se = TRUE, color = "blue") +
        labs(title = "Normalized Conc.", x = NULL, y = NULL) +
        theme_bw() + theme(
            plot.title      = element_text(size = 11, hjust = 0.5, face = "bold"),
            axis.text.x     = element_text(angle = 90, hjust = 1),
            panel.grid      = element_blank(),
            plot.margin     = margin(t = 0.35, r = 0.25, b = 0.15, l = 0.5, "cm"),
            axis.text       = element_text(size = 10),
            legend.position = "none"
        );
    }

    if (meta == "NA") {
      xlab <- colnames(mSetObj$dataSet$meta.info)[1]
    } else {
      xlab <- meta
    }
    
    ## Arrange and save both panels
    gridExtra::grid.arrange(
      p.orig, p.norm,
      ncol = 2,
      top  = grid::textGrob(paste(cmpdNm),
                            gp = grid::gpar(fontsize = 14, fontface = "bold")),
  bottom = grid::textGrob(xlab, gp = grid::gpar(fontsize = 13))

    )
    
    dev.off()
  }else if(mSetObj$dataSet$design.type =="time0"){
    # trend with subject
    
    #sel.meta.df <- mSetObj$dataSet$meta.info[, meta.vec.mb]
    sel.meta.df <- mSetObj$dataSet$meta.info;
    mSetObj$dataSet$exp.fac <- sel.meta.df[,-(which(tolower(colnames(sel.meta.df)) == "time"))]
    mSetObj$dataSet$time.fac <- sel.meta.df[,which(tolower(colnames(sel.meta.df)) == "time")]
    .set.mSet(mSetObj);
    
    my.w <- 8;
    my.h <- 6;
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=my.w, height= my.h, type=format, bg="white");
    plotProfile(mSetObj, cmpdNm);
    dev.off();
    
  }else{
    
    ####primary is cont
    if(cls.type == "cont"){
    ## x-axis variable
    if (meta == "NA") {
      xvar <- mSetObj$dataSet$meta.info[, 1]
      xlab <- colnames(mSetObj$dataSet$meta.info)[1]
    } else {
      xvar <- mSetObj$dataSet$meta.info[, meta]
      xlab <- meta
    }
    xvar <- as.numeric(as.character(xvar))
    
    ## y-axis = compound abundance
    yvar <- mSetObj$dataSet$norm[, cmpdNm]
    
    ## secondary variable (color, shape, etc.)
    group.var <- mSetObj$dataSet$meta.info[, meta2]
    
    if (cls.type2 == "disc") {
      group <- as.factor(group.var)
      cols  <- unique(GetColorSchema(group))
      df.orig <- data.frame(x = xvar, y = yvar, group = group)
      
      p.time <- ggplot(df.orig, aes(x = x, y = y, color = group)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = TRUE, aes(fill = group)) +
        scale_color_manual(name = meta2, values = cols) +
        scale_fill_manual(name = meta2, values = cols) +
        theme_bw()
      
    } else {
      ## cls.type2 == "cont"
      group <- as.numeric(as.character(group.var))
      df.orig <- data.frame(x = xvar, y = yvar, color_val = group)
      
      ## choose gradient based on sign of values
      rg <- range(df.orig$color_val, na.rm = TRUE)
      if (rg[1] < 0 && rg[2] > 0) {
        grad_scale <- scale_color_gradient2(
          name     = meta2,
          low      = "blue",
          mid      = "white",
          high     = "red",
          midpoint = 0
        )
      } else if (rg[1] >= 0) {
        grad_scale <- scale_color_gradient(
          name = meta2,
          low  = "grey",
          high = "blue"
        )
      } else {
        grad_scale <- scale_color_gradient(
          name = meta2,
          low  = "blue",
          high = "cyan"
        )
      }
      
      p.time <- ggplot(df.orig, aes(x = x, y = y, color = color_val)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "black") +
        grad_scale +
        theme_bw()
    }
    
    p.time <- p.time +
      labs(title = cmpdNm, x = xlab, y = "Abundance") +
      theme(
        plot.title      = element_text(size = 11, hjust = 0.5, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(size = 10),
        axis.text.x      = element_text(angle = 90, hjust = 1),
        legend.position  = "right"
      )
    
    ## render & save
    my.h <- 6.5
    Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
                 width = my.w, height = my.h, type = format, bg = "white")
    print(p.time)
    dev.off()

}else{
####primary is disc
############

      if(mSetObj$dataSet$design.type =="time"){ # time trend within phenotype
        out.fac <- mSetObj$dataSet$exp.fac;
        in.fac <- mSetObj$dataSet$time.fac;
        xlab = "Time";
        cls.type2 = "disc";
      }else{ # factor a split within factor b
        if(meta == "NA"){
          out.fac <- mSetObj$dataSet$meta.info[,1]
          xlab = colnames(meta.info)[1]
        }else{
          out.fac <- mSetObj$dataSet$meta.info[,meta];
          xlab = meta
        }
        in.fac <- mSetObj$dataSet$meta.info[,meta2];
        cls.type = cls.type; 
      }
      
      # two images per row
      img.num <- length(levels(out.fac));
      row.num <- ceiling(img.num/2)
      
      if(row.num == 1){
        my.h <- my.w*5/9;
      }else{
        my.h <- my.w*0.5*row.num;
      }
      
      if(cls.type2 == "cont"){
        my.h <- my.h +1;
      }

      Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=my.w, height=my.h, type=format, bg="white");
      
      col <- unique(GetColorSchema(in.fac));
      
      p_all <- list()
      
      for(lv in levels(out.fac)){
        #print(lv)
        inx <- out.fac == lv;
        if(cls.type2 == "disc"){
          df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = in.fac[inx])
        }else{
          df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = as.numeric(as.character(in.fac[inx])))
        }
        p_all[[lv]] <- df.orig
      }
      
      alldata <- do.call(rbind, p_all)
      alldata$facA <- factor(as.character(alldata$facA), levels=levels(out.fac))
      
      if(cls.type2 == "disc"){
        p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) 
        if(plotType == "boxplot"){
          p.time <- p.time + geom_boxplot(outlier.shape = NA, outlier.colour=NA) 
        }else{
          p.time <- p.time + geom_violin(trim=FALSE) 
        }
        p.time <- p.time + theme_bw() + geom_jitter(size=1) 
        p.time <- p.time + facet_wrap(~facA, nrow = row.num) + theme(axis.title.x = element_blank(), legend.position = "none")
        p.time <- p.time + scale_fill_manual(values=col) + theme(axis.text.x = element_text(angle=90, hjust=1))
        p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance")
        p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
        p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
      }else{
        p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=facA, colour=facA, shape=facA, group=facA)) + geom_point(size=2) + theme_bw()  + geom_smooth(aes(fill=facA),method=lm,se=T)     
        p.time <- p.time + theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")
        p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") +xlab(meta2)
        p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
        p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
      }
      
      print(p.time)
      dev.off()
    }
  }

  mSetObj$paramSet$cmpd.img.name <-  imgName;
  mSetObj$paramSet$cmpd.img.size <- paste0("width:", my.w, "in; height:", my.h, "in;"); 

  if(.on.public.web){
    .set.mSet(mSetObj);
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
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.MapData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpd <- qvec;
  return(.set.mSet(mSetObj));
}

# this is only for SSP: for those with conc above threshold
Update.MapData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ssp.cmpd <- qvec;
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

# current feature image size (which is not fixed)
GetCurrentCmpdImgSize <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    return(mSetObj$paramSet$cmpd.img.size);
}

#'Get all group names
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param exp.fac exp.fac
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
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
  }else if(mSetObj$dataSet$design.type %in% c("multi", "time", "time0")){
       my.cls <- mSetObj$dataSet$meta.info[,exp.fac];
     
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

GetOrigSmplSize <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  length(mSetObj$dataSet$orig.cls);
}


GetOrigSmplGroupNamesPerMeta <- function(mSetObj=NA, metaname="NA"){
  mSetObj <- .get.mSet(mSetObj);
  if(metaname %in% colnames(mSetObj$dataSet$meta.info)){
    as.character(mSetObj$dataSet$meta.info[,metaname]);
  }else if(!is.null(mSetObj$dataSet$meta.info[,1])) {
    as.character(mSetObj$dataSet$meta.info[,1]);
  }else { # single factor
    return(GetOrigGrpNms(NA))
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



GetFactors <-function(mSetObj=NA, metadata){
  mSetObj <- .get.mSet(mSetObj);  
  return(as.character(ncol(mSetObj$dataSet$meta.info)))
}


GetPrimaryInfo <-function(mSetObj=NA, metadata){
  mSetObj <- .get.mSet(mSetObj); 
  meta <- mSetObj$dataSet$meta.info;
  fac <- names(meta)[1]
  cls <- paste(unique(meta[,1]),collapse=",")
  return(paste0(fac," [",cls,"]"))
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

GetNameCheckMsgs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSet$msgSet$nmcheck.msg)){ # not applicable
    return(c("1", "Not applicable"));
  }
  return(mSet$msgSet$nmcheck.msg);
}

ValidateMetabolonData <- function(file_path = NULL) {
    if(!exists("my.validate.metabolon.data")){ # public web on same user dir
      .load.scripts.on.demand("util_metabolon.Rc");    
    }
    return(my.validate.metabolon.data(file_path));
}

ReadMetabolonSheets <- function(mSetObj = NA, metafactor, featureID){
    if(!exists("my.read.metabolon.sheets")){ # public web on same user dir
      .load.scripts.on.demand("util_metabolon.Rc");    
    }
    return(my.read.metabolon.sheets(mSetObj, metafactor, featureID));
}

ExtractMetabolonCompoundIDs  <- function(mSetObj = NA, file_path = NULL){
    if(!exists("my.extract.metabolon.compounds")){ # public web on same user dir
      .load.scripts.on.demand("util_metabolon.Rc");    
    }
    return(my.extract.metabolon.compounds(mSetObj, file_path));
}

ExtractMetabolonMetaFactors <- function(mSetObj = NA, file_path = NULL){
    if(!exists("my.extract.metabolon.metafactors")){ # public web on same user dir
      .load.scripts.on.demand("util_metabolon.Rc");    
    }
    return(my.extract.metabolon.metafactors(mSetObj, file_path));
}

GetMetabolonMetaFactor <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$metafactors -> metafactors;
  if(.on.public.web){
    return(metafactors)
  } else {
    return(mSetObj)
  }
}

GetMetabolonCMPDIDs <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpdIDs -> cmpdIDs;
  if(is.null(cmpdIDs)){cmpdIDs <- "NULL"}
  if(.on.public.web){
    return(cmpdIDs)
  } else {
    return(mSetObj)
  }
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
#'License: GNU GPL (>= 2)
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

SetAnalType <- function(mSetObj=NA, anal.type){
    mSetObj <- .get.mSet(mSet);
    mSetObj$analSet$type <- anal.type;
    anal.type <<- anal.type;
    return(.set.mSet(mSetObj))
}


process_metadata <- function(df) {
  library(caret)
  
  # Initialize the processed_df with the same number of rows as the input dataframe
  processed_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  # Loop through each column of the data frame
  for (col_name in colnames(df)) {
    
    # Check if the column is a factor or character (categorical)
    if (is.factor(df[[col_name]]) || is.character(df[[col_name]])) {
      # One-hot encode categorical variables
      one_hot_encoded <- model.matrix(~ df[[col_name]] - 1, data = df)
      colnames(one_hot_encoded) <- paste0(col_name, "_", colnames(one_hot_encoded))
      # Append to processed dataframe
      processed_df <- cbind(processed_df, one_hot_encoded)
    
    # If the column is numeric (continuous)
    } else if (is.numeric(df[[col_name]])) {
      # Handle missing values by imputing them with the mean
      df[[col_name]][is.na(df[[col_name]])] <- mean(df[[col_name]], na.rm = TRUE)
      
      # Normalize continuous variables (Z-score normalization)
      normalized_column <- scale(df[[col_name]])
      processed_df[[col_name]] <- normalized_column
    }
  }
  
  return(processed_df)
}


Read.TextDataDose <- function(mSetObj=NA, filePath, format="rowu", 
                          lbl.type="disc", nmdr = FALSE){
    mSetObj <- Read.TextData(mSetObj, filePath, format, lbl.type, nmdr);
    mSetObj <- .get.mSet(mSetObj);

    conc <- qs::qread(file="data_orig.qs");
    int.mat <- conc;
    dose <- as.numeric(gsub(".*_", "", as.character(mSetObj$dataSet$cls)))

    # Error check: Ensure metadata is numeric
    if (any(is.na(dose))) {
        AddErrMsg("Error: Metadata for dose-response analysis must be numeric. Please check the input file and ensure the dose values are properly formatted.")
        return(0);
    }

    # Check for a minimum of 3 unique doses
    unique_doses <- unique(dose)
    if (length(unique_doses) < 3) {
        AddErrMsg("Error: Dose-response analysis requires at least 3 unique doses. Please provide data with at least 3 distinct dose levels.")
        return(0)
    }

    dose.order <- order(dose);
    meta.reorder <- mSetObj$dataSet$cls[dose.order];
    int.mat <- int.mat[dose.order, ];

    meta.reorder <- format(as.numeric(as.character(meta.reorder)), scientific = FALSE) # remove scientific notation
    meta.reorder <- gsub(" ", "", meta.reorder)
    meta.reorder <- factor(meta.reorder, levels = unique(meta.reorder))


    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- meta.reorder;
    mSetObj$dataSet$url.smp.nms <- mSetObj$dataSet$url.smp.nms[order(dose)];

    qs::qsave(int.mat, file="data_orig.qs");
    return(.set.mSet(mSetObj));
} 

Read.TextDataDoseWithMeta <- function(mSetObj=NA, filePath, metaPath, format="rowu", 
                          lbl.type="disc", nmdr = FALSE){
    mSetObj <- Read.TextDataTs(mSetObj, filePath, format);
    mSetObj <- .get.mSet(mSet);

    mSetObj <- ReadMetaData(mSetObj, metaPath);
    mSetObj <- .get.mSet(mSet);

    conc <- qs::qread(file="data_orig.qs");
    int.mat <- conc;
    dose <- as.numeric(gsub(".*_", "", as.character(mSetObj$dataSet$cls)))

    # Error check: Ensure metadata is numeric
    if (any(is.na(dose))) {
        AddErrMsg("Error: Metadata for dose-response analysis must be numeric. Please check the input file and ensure the dose values are properly formatted.")
        return(0);
    }

    # Check for a minimum of 3 unique doses
    unique_doses <- unique(dose)
    if (length(unique_doses) < 3) {
        AddErrMsg("Error: Dose-response analysis requires at least 3 unique doses. Please provide data with at least 3 distinct dose levels.")
        return(0)
    }

    dose.order <- order(dose);
    meta.reorder <- mSetObj$dataSet$cls[dose.order];
    int.mat <- int.mat[dose.order, ];

    meta.reorder <- format(as.numeric(as.character(meta.reorder)), scientific = FALSE) # remove scientific notation
    meta.reorder <- gsub(" ", "", meta.reorder)
    meta.reorder <- factor(meta.reorder, levels = unique(meta.reorder))


    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- meta.reorder;
    mSetObj$dataSet$url.smp.nms <- mSetObj$dataSet$url.smp.nms[order(dose)];
    mSetObj[["dataSet"]][["cls.type"]] <- lbl.type;
    qs::qsave(int.mat, file="data_orig.qs");
    return(.set.mSet(mSetObj));
}

PrintCurrentCls <- function(mSetObj=NA){
   mSetObj <- .get.mSet(mSet);
print("Current class info ....");
print(mSetObj$dataSet$cls);
}

SetBlankQcBool <- function(mSetObj=NA, containsQC=F, containsBlank=F){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$containsQC <- containsQC;
  mSetObj$dataSet$containsBlank <- containsBlank;
  return(.set.mSet(mSetObj));
}