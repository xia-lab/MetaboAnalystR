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
  
  .init.global.vars(anal.type);
  print("MetaboAnalyst R objects initialized ...");
  return(.set.mSet(mSetObj));
}

# this is for switching module
UpdateDataObjects <- function(data.type, anal.type, paired=FALSE){

    mSetObj <- .get.mSet(NA);
    mSetObj$dataSet$type <- data.type;
    mSetObj$analSet$type <- anal.type;
    .init.global.vars(anal.type);    
    
    # some specific setup 
    if(anal.type == "mummichog"){
        .init.MummiMSet();
        load("params.rda");        
        mSetObj <- UpdateInstrumentParameters(mSetObj, peakParams$ppm, peakParams$polarity, "yes", 0.02);
        mSetObj <- .rt.included(mSetObj, "seconds");
        mSetObj <- Read.TextData(mSetObj, "metaboanalyst_input.csv", "colu", "disc");
        mSetObj <- .get.mSet(NA);
    }
    return(.set.mSet(mSetObj));
}

.init.MummiMSet <- function(mSetObj=NA) {  
  SetPeakFormat("pvalue");
  TableFormatCoerce("metaboanalyst_input.csv", "OptiLCMS", "mummichog");
  anal.type <<- "mummichog";
  api.base <<- "http://api.xialab.ca";
  err.vec <<- "";
}

.init.global.vars <- function(anal.type){
  # other global variables
  msg.vec <<- "";
  err.vec <<- "";

  # for network analysis
  module.count <<- 0;
  # counter for naming different json file (pathway viewer)
  smpdbpw.count <<- 0; 
  # for mummichog
  peakFormat <<- "mpt"  

  # raw data processing
  rawfilenms.vec <<- vector();

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
     url.pre <<- "/home/glassfish/sqlite/";
  }else if(file.exists("/home/jasmine/Downloads/sqlite/")){ #jasmine's local
     url.pre <<- "/home/jasmine/Downloads/sqlite/";
     api.base <<- "localhost:8987"
  }else if(file.exists("/Users/soufanom/Documents/Projects/gene-id-mapping/")){ # soufan laptop
     url.pre <<- "/Users/soufanom/Documents/Projects/gene-id-mapping/";
  }else if(file.exists("~/Documents/Projects/gene-id-mapping/")){
     url.pre <<- "~/Documents/Projects/gene-id-mapping/"
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){ # xia local
     url.pre <<- "/Users/xia/Dropbox/sqlite/";
  }else if(file.exists("/media/zzggyy/disk/sqlite/")){
     url.pre <<-"/media/zzggyy/disk/sqlite/"; #zgy local)
  }else if(file.exists("/home/zgy/sqlite/")){
     url.pre <<-"/home/zgy/sqlite/"; #zgy local)
  } else if(file.exists("/home/le/sqlite/GeneID_25Species_JE/")){# le local
    url.pre <<-"/home/le/sqlite/GeneID_25Species_JE/";
  }else{
     url.pre <<- paste0(dirname(system.file("database", "sqlite/GeneID_25Species_JE/ath_genes.sqlite", package="MetaboAnalystR")), "/")
     api.base <<- "http://api.xialab.ca"
  }
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
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  orig.smp.nms <- smpl.nms;
  smpl.nms <- CleanNames(smpl.nms, "sample_name");
  names(orig.smp.nms) <- smpl.nms;

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
      #if(min(table(cls.lbl)) < 3){
      #  AddErrMsg(paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
      #  AddErrMsg("At least three replicates are required in each group!");
      #  AddErrMsg("Or maybe you forgot to specify the data format?");
      #  return(0);
      #}
     
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
  mSetObj$dataSet$orig.smp.nms <- orig.smp.nms;
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

  mSetObj <- .get.mSet(mSetObj);
  data.type <- mSetObj$dataSet$type
  
  # mummichog data is not processed, so just mSet$orig and mSet$proc
  
  if(anal.type=="mummichog"){
    fast.write.csv(mSetObj$dataSet$mummi.orig, file="data_original.csv", row.names = FALSE);
    fast.write.csv(mSetObj$dataSet$mummi.proc, file="data_processed.csv", row.names = FALSE);
  }else{
    if(file.exists("data_orig.qs")){
      lbls <- NULL;
      tsFormat <- substring(mSetObj$dataSet$format,4,5)=="ts";
      if(tsFormat){
        lbls <- cbind(as.character(mSetObj$dataSet$orig.facA),as.character(mSetObj$dataSet$orig.facB));
        colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      }else{
        lbls <- cbind("Label"= as.character(mSetObj$dataSet$orig.cls));
      }
      
      orig.var.nms <- mSetObj$dataSet$orig.var.nms;
      orig.data<- qs::qread("data_orig.qs");
      
      # convert back to original names
      if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
        colnames(orig.data) <- orig.var.nms[colnames(orig.data)];
      }
        orig.data<-cbind(lbls, orig.data);

      if(dim(orig.data)[2]>200){
        orig.data<-t(orig.data);
      }
        
      fast.write.csv(orig.data, file="data_original.csv");
      
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
        fast.write.csv(proc.data, file="data_processed.csv");
        
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
            ids <- matrix(unlist(strsplit(colnames(norm.data), "/", fixed=TRUE)),ncol=2, byrow=T);
            colnames(ids) <- c("mz", "rt");
            new.data <- data.frame(ids, t(norm.data));
            fast.write.csv(new.data, file="peak_normalized_rt_mz.csv");
          }else{
            # convert back to original names
            if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
              colnames(norm.data) <- orig.var.nms[colnames(norm.data)];   
            }
            norm.data<-cbind(lbls, norm.data);
            if(dim(norm.data)[2]>200){
              norm.data<-t(norm.data);
            }
            fast.write.csv(norm.data, file="data_normalized.csv");
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
PlotCmpdSummary <- function(mSetObj=NA, cmpdNm, version, format="png", dpi=72, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
    load_grid()
  }
  
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_", version, "_summary_dpi", dpi, ".", format, sep="");
  
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
    col <- unique(GetColorSchema(mSetObj$dataSet$cls));
    
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
    
    col <- unique(GetColorSchema(in.fac));

    p_all <- list()
    
    for(lv in levels(out.fac)){
      inx <- out.fac == lv;
      df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = in.fac[inx])
      p_all[[lv]] <- df.orig
    }
    
    alldata <- do.call(rbind, p_all)
    
    p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + geom_boxplot(outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1) 
    p.time <- p.time + facet_wrap(~facA, nrow = row.num) + theme(axis.title.x = element_blank(), legend.position = "none")
    p.time <- p.time + scale_fill_manual(values=col) + theme(axis.text.x = element_text(angle=90, hjust=1))
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


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetErrMsg<-function(){
  return(err.vec);
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

GetMetaInfo <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    if(mSetObj$dataSet$design.type == "regular"){
        return("Group");
    }else{
        return(c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl));
    }
}

# all groups
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
  return(levels(my.cls));
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

GetFilesToBeSaved <-function(naviString){
    partialToBeSaved <- c("Rload.RData");
    return(unique(partialToBeSaved));
}

GetExampleDataPath<-function(naviString){
    return(url.pre);
}

Read.TextDataMumMixed <- function(mSetObj=NA, filePath,filePath2, format="rowu", lbl.type="disc", order=NULL){

    temp_mSet1 <- Read.TextData(NA, filePath, format, lbl.type)
    temp_mSet1 <- .get.mSet(mSetObj);
    qs::qsave(temp_mSet1, file="data_orig_temp1.qs");

    temp_mSet1 <- qs::qread("data_orig_temp1.qs");
    orig1 <- qs::qread("data_orig.qs");

    temp_mSet2 <- Read.TextData(NA, filePath2, format, lbl.type)
    temp_mSet2 <-.get.mSet(mSetObj);
    orig2 <- qs::qread("data_orig.qs");
    mSetObj <- temp_mSet1
    mSetObj$dataSet$mumType <- "table";
    mSetObj$dataSet$orig.var.nms <- c(mSetObj$dataSet$orig.var.nms, temp_mSet2$dataSet$orig.var.nms);
    mSetObj$dataSet$mode <- "mixed"
    orig <- cbind(orig1, orig2);
    mSetObj$dataSet$pos_inx <- c(rep(TRUE, ncol(orig1)), rep(FALSE, ncol(orig2)))
    qs::qsave(orig, file="data_orig.qs");
    .set.mSet(mSetObj)

    return(1);
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
  
  write.csv(df, file = oriFile, quote = FALSE, row.names = FALSE)
  
}