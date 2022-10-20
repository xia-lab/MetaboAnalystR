##################################################
## R script for ExpressAnalyst
## Description: functions for reading data table 
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#'Read tab delimited file
#'@description read tab delimited file, used in single gene expression;
#'@param fileName file name of the data, .txt format
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ReadTabExpressData <- function(fileName, path="") {

  dataSet <- .readTabData(paste0(path, fileName));
  if(is.null(dataSet)){
    return(0);
  }  
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");

  # rename data to data.orig
  int.mat <- dataSet$data;
  dataSet$data <- NULL;
  dataSet$name <- fileName;
  
  msg <- paste("a total of ", ncol(int.mat), " samples and ", nrow(int.mat), " features were found");
  
  # remove NA, null
  row.nas <- apply(is.na(int.mat)|is.null(int.mat), 1, sum);
  good.inx<- row.nas/ncol(int.mat) < 0.5;
  if(sum(!good.inx) > 0){
    int.mat <- int.mat[good.inx,];
    msg <- c(msg, paste("removed ", sum(!good.inx), " features with over 50% missing values"));
  }
  # remove constant values
  filter.val <- apply(int.mat, 1, IQR, na.rm=T);
  good.inx2 <- filter.val > 0;
  if(sum(!good.inx2) > 0){
    int.mat <- int.mat[good.inx2,];
    msg <- c(msg, paste("removed ", sum(!good.inx2), " features with constant values"));
  }
  
  minVal <- min(int.mat, na.rm=T);
  na.inx <- is.na(int.mat);
  if(sum(na.inx) > 0){
    int.mat[na.inx] <- minVal/2;
    # msg <- c(msg, "the remaining", sum(na.inx), "missing variables were replaced with data min");
  }
  msgSet$current.msg <- paste(msg, collapse="; ");
  res <- RemoveDuplicates(int.mat, "mean", quiet=T, paramSet, msgSet);
  data.proc <- res[[1]];
  msgSet <- res[[2]];
  paramSet$smpl.num <- ncol(data.proc);

  # save processed data for download user option
  fast.write(data.proc, file="data_processed.csv");
  qs::qsave(data.proc, "data.proc.qs");

  paramSet$anal.type <- "onedata";

  paramSet$partialToBeSaved <- c( paramSet$partialToBeSaved, fileName);
  paramSet$jsonNms$dataName <- fileName;

  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  RegisterData(dataSet);
  return(1);
}

#read annotation table file when user selects custom annotation option
ReadAnnotationTable <- function(fileName) {
  anot.data <- .readDataTable(fileName);
  msgSet <- readSet(msgSet, "msgSet");

  if(length(colnames(anot.data)) != 3){
    msgSet$current.msg <- "Please make sure the annotation contains exactly 3 columns";
  }
  colnames(anot.data) = c("gene_id", "symbol", "name");
  qs::qsave(anot.data, "anot_table.qs");
  saveSet(msgSet, "msgSet");
  return(1);
}


ReadMetaData <- function(metafilename){
    paramSet <- readSet(paramSet, "paramSet");
    metadata <- .readDataTable(metafilename);
    metadata[is.na(metadata)] = "NA"
    if(class(metadata) == "try-error"){
      AddErrMsg("Failed to read in the metadata file! Please make sure that the metadata file is in the right format and does not have empty cells or contains NA.");
      return(0);
    }
    mdata.all <- paramSet$mdata.all;
    paramSet <- readSet(paramSet, "paramSet");
    # need to add metadata sanity check
    # are sample names identical to data$orig
    # order samples in same way as in abundance table
    smpl.nms <- metadata[,1];
    smpl.var <- colnames(metadata)[-1];  
    sel.nms <- names(mdata.all);

for(i in 1:length(sel.nms)){
  dataSet <- qs::qread(sel.nms[i]);
  
  
  data.smpl.nms <- colnames(dataSet$data.norm)
  nm.hits <- data.smpl.nms %in% smpl.nms;
  if(!all(nm.hits)){
    AddErrMsg("Some sample names in your data are not in the metadata file!");
    mis.nms <- data.smpl.nms[!nm.hits];
    AddErrMsg(paste(mis.nms, collapse="; "));
    return(0);
  }
  
  # now remove extra meta if present, and order them
  nm.hits2 <- which(smpl.nms %in% data.smpl.nms);
  metadata1 <- metadata[nm.hits2,];
  metadata1 <- metadata1[,-1];

  if(!is.data.frame(metadata1)){
    metadata1 <- data.frame(metadata1, stringsAsFactors=T);
    colnames(metadata1) <- colnames(metadata)[2]
  }else{
    metadata1[] <- lapply( metadata1, factor)
  }
  rownames(metadata1) <- data.smpl.nms;
  dataSet$meta <- metadata1;
  RegisterData(dataSet);
}
  
  saveSet(paramSet, "paramSet");
  return(1);
}


# read tab delimited file
# can have many classes, stored in meta.info (starts with #) 
# return a list (data.name, data.frame, meta.data)
.readTabData <- function(dataName) {
  msgSet <- readSet(msgSet, "msgSet");
  if(length(grep('\\.zip$',dataName,perl=TRUE))>0){
    dataName <- unzip(dataName);
    if(length(dataName) > 1){
      # test if "__MACOSX" or ".DS_Store"
      osInx <- grep('MACOSX',dataName,perl=TRUE);
      if(length(osInx) > 0){
        dataName <- dataName[-osInx];
      }
      dsInx <- grep('DS_Store',dataName,perl=TRUE);
      if(length(dsInx) > 0){
        dataName <- dataName[-dsInx];
      }
      dat.inx <- grep(".[Tt][Xx][Tt]$", dataName);
      if(length(dat.inx) != 1){
        msgSet$current.msg <- "More than one text files (.txt) found in the zip file.";
        saveSet(msgSet, "msgSet");        
        return(NULL);
      }
    }
  }
  
  msg <- NULL;
  # using the powerful fread function, 10 times faster, note: default return data.table, turn off
  dat1 <- .readDataTable(dataName);
  if(is.null(dat1)){
    return(NULL);
  }
  # look for #CLASS, could have more than 1 class labels, store in a list
  meta.info <- list();
  cls.inx <- grep("^#CLASS", dat1[,1]);
  if(length(cls.inx) > 0){ 
    for(i in 1:length(cls.inx)){
      inx <- cls.inx[i];
      cls.nm <- substring(dat1[inx, 1],2); # discard the first char #
      if(nchar(cls.nm) > 6){
        cls.nm <- substring(cls.nm, 7); # remove class
      }
      cls.lbls <- dat1[inx, -1];
      # test NA
      na.inx <- is.na(cls.lbls);
      cls.lbls[na.inx] <- "NA";
      cls.lbls <- ClearFactorStrings(cls.nm, cls.lbls);
      meta.info[[cls.nm]] <- cls.lbls;
    }
  }else{
    msgSet$current.msg <- "No metadata labels #CLASS found in your data!";
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
  
  meta.info <- data.frame(meta.info);
  dat1 <- .to.numeric.mat(dat1);
  
  list(
    name= basename(dataName),
    data=dat1,
    type="count", # to be updated later
    meta=meta.info
  );
}


# note, try to use the fread, however, it has issues with 
# some windows 10 files "Line ending is \r\r\n. .... appears to add the extra \r in text mode on Windows"
# in such as, use the slower read.table method
.readDataTable <- function(fileName){
  msgSet <- readSet(msgSet, "msgSet");

  if(length(grep('\\.zip$',fileName,perl=TRUE))>0){
    fileName <- unzip(fileName);
    if(length(fileName) > 1){
      # test if "__MACOSX" or ".DS_Store"
      osInx <- grep('MACOSX',fileName,perl=TRUE);
      if(length(osInx) > 0){
        fileName <- fileName[-osInx];
      }
      dsInx <- grep('DS_Store',fileName,perl=TRUE);
      if(length(dsInx) > 0){
        fileName <- fileName[-dsInx];
      }
      dat.inx <- grep(".[Tt][Xx][Tt]$", fileName);
      if(length(dat.inx) != 1){
        msgSet$current.msg <- "More than one text files (.txt) found in the zip file.";
        return(NULL);
      }
    }
  }
  dat <- try(data.table::fread(fileName, header=TRUE, check.names=FALSE, data.table=FALSE));
  rm.inx <- apply(dat,2,function(x){all(is.na(x))});
  dat <- dat[,!rm.inx];
  if(class(dat) == "try-error"){
    #try to use "tr" to remove double return characters
    trFileName <- paste("tr -d \'\\r\' <", fileName);
    dat <- try(data.table::fread(trFileName, header=TRUE, check.names=FALSE, data.table=FALSE));
    if(class(dat) == "try-error"){
      print("Using slower file reader ...");
      formatStr <- substr(fileName, nchar(fileName)-2, nchar(fileName))
      if(formatStr == "txt"){
        dat <-try(read.table(fileName,header=TRUE,comment.char = "", check.names=F, as.is=T));
      }else{ # note, read.csv is more than read.table with sep=","
        dat <-try(read.csv(fileName,header=TRUE,comment.char = "", check.names=F, as.is=T));
      }  
    }
  }
  if(class(dat) == "try-error"){
    msgSet$current.msg <- "Failed to read the data table! Please check your data format.";
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
  
  # need to remove potential empty columns
  dat <- dat[!sapply(dat, function(x) all(x == "" | is.na(x)))];
  return(dat);
}