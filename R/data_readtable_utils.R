##################################################
## R script for ExpressAnalyst
## Description: functions for reading data table 
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#' Read Tabular Expression Data and Metadata
#'
#' This function reads tabular expression data along with metadata and processes the data.
#'
#' @param fileName A character string specifying the name of the expression data file.
#' @param metafileName A character string specifying the name of the metadata file, ignore if metaContain = true
#' @param metaContain A logical value indicating whether metadata is contained in the data file
#' @param oneDataAnalType The type of analysis to perform on the one-data setup.
#' @param path The path to the files if they are located in a different directory.
#'
#' @return A processed dataset object containing expression data and metadata.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' ReadTabExpressData(fileName = "expression_data.csv", metafileName = "metadata.csv",
#'                    metaContain = TRUE, oneDataAnalType = "default", path = "")
#' }
#'
#' @export
#' @license MIT License

ReadTabExpressData <- function(fileName, metafileName="",metaContain="true",oneDataAnalType="default", path="") {
  dataSet <- .readTabData(paste0(path, fileName));
  if(is.null(dataSet)){
    return(0);
  }  
  
  meta.info <- .readMetaData(metafileName,dataSet$data_orig,metaContain);
  
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  paramSet$isMetaContain <- metaContain
  paramSet$oneDataAnalType <- oneDataAnalType;
  
  # rename data to data.orig
  int.mat <- dataSet$data;
  int.mat <- int.mat[,which(colnames(int.mat) %in% rownames(meta.info$meta.info))]
  int.mat <- int.mat[,match(rownames(meta.info$meta.info),colnames(int.mat))]
  dataSet$data <- NULL;
  dataSet$name <- fileName;
  qs::qsave(int.mat, "int.mat.qs");
  msg <- paste("a total of ", ncol(int.mat), " samples and ", nrow(int.mat), " features were found");
  # remove NA, null
  row.nas <- apply(is.na(int.mat)|is.null(int.mat), 1, sum);
  good.inx<- row.nas/ncol(int.mat) < 0.5;
  if(sum(!good.inx) > 0){
    int.mat <- int.mat[good.inx,];
    msg <- c(msg, paste("removed ", sum(!good.inx), " features with over 50% missing values"));
  }
  
  minVal <- min(int.mat, na.rm=T);
  na.inx <- is.na(int.mat);
  if(sum(na.inx) > 0){
    int.mat[na.inx] <- minVal/2;
    # msg <- c(msg, "the remaining", sum(na.inx), "missing variables were replaced with data min");
  }
  msgSet$current.msg <- paste(msg, collapse="; ");
  #res <- RemoveDuplicates(int.mat, "mean", quiet=T, paramSet, msgSet);Gene-level summarization
  data.proc <- int.mat #res[[1]];
  #msgSet <- res[[2]];
  paramSet$smpl.num <- ncol(data.proc);
  
  metadata <- meta.info$meta.info;
  dataSet$meta.info <- metadata;
  if(oneDataAnalType == "dose"){
    
    # re-order everything numerically by dose
    dose <- as.numeric(gsub(".*_", "", as.character(metadata[,1])))
    int.mat <- int.mat[ ,order(dose)]
    meta.reorder <- as.data.frame(metadata[order(dose),])
    colnames(meta.reorder) <- colnames(metadata)
    rownames(meta.reorder) <- rownames(meta.info$meta.info)[order(dose)]
    dataSet$meta.info <- meta.reorder
    
    # re-level the factor to be numeric instead of alphabetic
    dataSet$meta.info[,1] <- factor(dataSet$meta.info[,1], levels = unique(dataSet$meta.info[,1]))
    
    # rename data to data.orig
    data.proc <- int.mat;
    paramSet$dataSet$meta.info <- dataSet$meta.info;
    dataSet$cls <- dataSet$meta.info[,1];
    dataSet$data <- NULL;
    dataSet$listData <- FALSE;
    
    dataSet$imgSet <- list();
    dataSet$reportSummary <- list();
    
  }
  
  # save processed data for download user option
  fast.write(data.proc, file="data_processed.csv");
  qs::qsave(data.proc, "data.raw.qs");
  dataSet$data.norm  <- data.proc;
  metaInx = which(rownames(dataSet$meta.info) %in% colnames(data.proc))
  
  paramSet$dataSet <- list();
  meta.types <- rep("disc", ncol(dataSet$meta.info));
  meta.types[meta.info$cont.inx] <- "cont";
  names(meta.types) <- colnames(dataSet$meta.info);
  
  paramSet$dataSet$meta.types <- meta.types;
  paramSet$dataSet$meta.info <- dataSet$metaOrig <- dataSet$meta.info[metaInx,,drop=F]
  paramSet$dataSet$disc.inx <- dataSet$disc.inx <-dataSet$disc.inx.orig <- meta.info$disc.inx
  paramSet$dataSet$cont.inx <- dataSet$cont.inx <-dataSet$cont.inx.orig  <- meta.info$cont.inx
  
  meta.types <- rep("disc", ncol(dataSet$meta.info));
  meta.types[meta.info$cont.inx] <- "cont";
  names(meta.types) <- colnames(dataSet$meta.info);
  dataSet$meta.types <-meta.types;
  paramSet$anal.type <- "onedata";
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, fileName);
  paramSet$jsonNms$dataName <- fileName;
  paramSet$dataName <- fileName;
  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet));
}

#read annotation table file when user selects custom annotdataSet$meta.info <-ation option
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


#' Read Metadata for Meta-Analysis Mode
#'
#' This function reads metadata for meta-analysis mode and performs necessary checks.
#'
#' @param metafilename A character string specifying the name of the metadata file.
#'
#' @return An integer indicating the success of reading and processing metadata.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' ReadMetaData(metafilename = "metadata.csv")
#' }
#'
#' @export
#' @license MIT License
#'
ReadMetaData <- function(metafilename){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet,"msgSet");
  metadata <- try(data.table::fread(metafilename, header=TRUE, check.names=FALSE, data.table=FALSE));
  metadata[is.na(metadata)] = "NA"
  if(class(metadata) == "try-error"){
    msgSet$current.msg = "Failed to read in the metadata file! Please make sure that the metadata file is in the right format and does not have empty cells or contains NA."
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
  # look for #NAME, store in a list
  sam.inx <- grep("^#NAME", colnames(metadata)[1]);
  if(length(sam.inx) > 0){
    smpl_nms<-metadata[,1];
    smpl_var<-colnames(metadata[-1]);
  }else{
    msgSet$current.msg = "Please make sure you have the label #NAME in your sample data file!"
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
  # converting to character matrix as duplicate row names not allowed in data frame.
  metadata <-data.frame(lapply(1:ncol(metadata),function(x){
    metadata[,x]=unlist(ClearFactorStrings(metadata[,x]))
  }))
  metadata <- metadata[,-1,drop=F];
  if(nrow(metadata)==1){
    msgSet$current.msg = "Only one sample in the dataset or the metadata file must be transposed!"
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
  rownames(metadata) <- smpl_nms;
  colnames(metadata) <- smpl_var;
  
  na.msg <- ""
  disc.inx <- GetDiscreteInx(metadata);
  if(sum(disc.inx) == length(disc.inx)){
    msgSet$na.msg <- "All metadata columns are OK!"
  }else{
    bad.meta<- paste(names(disc.inx)[!disc.inx], collapse="; ");
    msgSet$na.msg <- paste0("<font style=\"color:red\">Detected presence of unique values in the following columns: <b>", bad.meta, "</b></font>","Please make sure the metadata is in right format! You can use meta editor to update the information !");
  }
  
  
  cont.inx <- GetNumbericalInx(metadata);
  cont.inx <- !disc.inx & cont.inx; # discrete is first
  
  if(sum(cont.inx)>0){
    # make sure the discrete data is on the left side
    metadata <- cbind(metadata[,disc.inx, drop=FALSE], metadata[,cont.inx, drop=FALSE]);
  }

  metadata$Dataset <- rep("NA", nrow(metadata));
  
  mdata.all <- paramSet$mdata.all;
  # need to add metadata sanity check
  # are sample names identical to data$orig
  # order samples in same way as in abundance table
  sel.nms <- names(mdata.all);
  
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    data.smpl.nms <- colnames(dataSet$data.norm)
    nm.hits <- data.smpl.nms %in% smpl_nms;
    if(!all(nm.hits)){
      msgSet$current.msg = paste0("Some sample names including ",paste(mis.nms, collapse="; ") ," in your data are not in the metadata file!")
      saveSet(msgSet, "msgSet");
      return(NULL);
    }
    
    # now remove extra meta if present, and order them
    nm.hits2 <- which(smpl_nms %in% data.smpl.nms);
    metadata$Dataset[nm.hits2] <- sel.nms[i];
    metadata1 <- metadata[nm.hits2,,drop=F];
    metadata1[] <- lapply( metadata1, factor)
    dataSet$meta.info <- dataSet$metaOrig <- metadata1
    dataSet$disc.inx <-dataSet$disc.inx.orig <- disc.inx[colnames(metadata1)]
    dataSet$cont.inx <-dataSet$cont.inx.orig  <- cont.inx[colnames(metadata1)]

    meta.types <- rep("disc", ncol(dataSet$meta.info));
    meta.types[cont.inx[colnames(metadata1)]] <- "cont";
    names(meta.types) <- colnames(dataSet$meta.info);
    dataSet$meta.types <-meta.types;

    RegisterData(dataSet);
  }
  
  paramSet$dataSet <- list();
  meta.types <- rep("disc", ncol(metadata));
  meta.types[cont.inx] <- "cont";
  names(meta.types) <- colnames(metadata);
  
  paramSet$dataSet$meta.types <- meta.types;
  paramSet$dataSet$meta.status <- rep("OK", ncol(metadata));
  paramSet$dataSet$cont.inx <- cont.inx;
  paramSet$dataSet$disc.inx <- disc.inx;
  paramSet$dataSet$meta.info <- metadata;
  paramSet$dataSet$metaOrig <- metadata;
  saveSet(msgSet, "msgSet");
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
  datOrig <- .readDataTable(dataName);
  if(is.null(datOrig)){
    return(NULL);
  }
  dat1 <- .to.numeric.mat(datOrig);
  list(
    name= basename(dataName),
    data_orig = datOrig,
    data=dat1,
    type="count" # to be updated later
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


####read meta file
#### return a list
.readMetaData <- function(metafileName,datOrig,metaContain) {
  msgSet <- readSet(msgSet, "msgSet");
   na.msg <- ""
  if(is.null(msgSet$current.msg)){
    msg <-""
  }else{
    msg <- msgSet$current.msg
  }
  match.msg <- "";

  if(metaContain=="true"){
    meta.info <- list();
    # look for #CLASS, could have more than 1 class labels, store in a list
     cls.inx <- grep("^#CLASS", datOrig[,1]);
    if(length(cls.inx) > 0){ 
      for(i in 1:length(cls.inx)){
        inx <- cls.inx[i];
        cls.nm <- substring(datOrig[inx, 1],2); # discard the first char #
        if(nchar(cls.nm) > 6){
          cls.nm <- substring(cls.nm, 7); # remove class
        }
        if(grepl("[[:blank:]]", cls.nm)){
          cls.nm<- gsub("\\s+","_", cls.nm);
          msg <- c(msg, " Blank spaces in group names are replaced with underscore '_'! ");
        }
        cls.lbls <- setNames(as.character(datOrig[inx, -1]),colnames(datOrig)[-1]);
        # test NA
        na.inx <- is.na(cls.lbls);
        cls.lbls[na.inx] <- "NA";
        cls.lbls <- ClearFactorStrings(cls.lbls);
        meta.info[[cls.nm]] <- cls.lbls;
      }
    }else{
      msgSet$current.msg <- "No metadata labels #CLASS found in your data!";
      saveSet(msgSet, "msgSet");
      return(NULL);
    }
    
    meta.info <- data.frame(meta.info);
 rownames(meta.info) = colnames(datOrig)[-1]
  }else{ # metadata input as an individual table
    mydata <- try(data.table::fread(metafileName, header=TRUE, check.names=FALSE, data.table=FALSE));
   if(class(mydata) == "try-error"){
    msgSet$current.msg <- "Failed to read the metadata table! Please check your data format.";
    saveSet(msgSet, "msgSet");
    return(NULL);
  }
 idx = which(!colnames(datOrig) %in% mydata$`#NAME`)
 if(length(idx)>1){
  if(length(idx)==2){
    match.msg <- paste0(match.msg,"One sample ", colnames(datOrig)[idx[2]], " was not detected in metadata file and was removed from data table!   ")
   }else if(length(idx)>5){
    match.msg <- paste0(match.msg,length(idx[-1])," samples ", paste(colnames(datOrig)[idx[2:4]],collapse = ", "), ", etc. were not detected in metadata file and were removed  from data table!   ")
   }else{
    match.msg <- paste0(match.msg,length(idx[-1])," samples ", paste(colnames(datOrig)[idx[-1]],collapse = ", "), " were not detected in metadata file and were removed  from data table!   ")
   }
   datOrig <- datOrig[,-idx[-1]]
 }

 idx = which( !mydata$`#NAME` %in%colnames(datOrig) )
 if(length(idx)>1){
   if(length(idx)==1){
     match.msg <- paste0(match.msg,"One sample ", mydata$`#NAME`[idx], " was not detected in data file and was removed from metadata table!   ")
   }else if(length(idx)>3){
    match.msg <- paste0(match.msg,length(idx)," samples ", paste(mydata$`#NAME`[1:3],collapse = ", "), ", etc. were not detected in data file and were removed from metadata table!  ")
   }else{
    match.msg <- paste0(match.msg, length(idx)," samples ", paste(mydata$`#NAME`[idx],collapse = ", "), " were not detected in data file and were removed from metadata table!  ")
   }
   mydata <- mydata[-idx,]
 }
  mydata <-  mydata[match(mydata$`#NAME`,colnames(datOrig)[-1]),]
     mydata[is.na(mydata)] <- "NA";
    # look for #NAME, store in a list
    sam.inx <- grep("^#NAME", colnames(mydata)[1]);
    if(length(sam.inx) > 0){
      smpl_nm<-mydata[,1];
      smpl_var<-colnames(mydata[-1]);
    }else{
      msgSet$current.msg <- "Please make sure you have the label #NAME in your sample data file!";
      saveSet(msgSet, "msgSet");
      return(NULL);
    }
 
   # covert to factor
     mydata <-data.frame(lapply(1:ncol(mydata),function(x){
      mydata[,x]=unlist(ClearFactorStrings(mydata[,x]))
    }))
    mydata <- mydata[,-1,drop=F]; # converting to character matrix as duplicate row names not allowed in data frame.
    if(nrow(mydata)==1){
      msgSet$current.msg <- "Only one sample in the dataset or the metadata file must be transposed!";
      saveSet(msgSet, "msgSet");
      return(NULL);
    }
    rownames(mydata) <- smpl_nm;
    colnames(mydata) <- smpl_var;
   
    # empty cell or NA cannot be tolerated in metadata
    na.inx  <- is.na(mydata);
    na.msg <- na.msg1 <- NULL;
    if(sum(na.inx) > 0){
      na.msg1 <- paste("A total of", sum(na.inx), "empty or NA values were detected. Please update in using metadata editor");
    }
    
    #Check group label names for spaces and replace with underscore
    meta.info <- data.frame(mydata,check.names=FALSE);
    if(any(grepl("[[:blank:]]", names(meta.info)))){
      names(meta.info) <- gsub("\\s+","_", names(meta.info));
      na.msg1 <- c(na.msg1, "Blank spaces in group names are replaced with underscore '_'");
    }

  }
  

  disc.inx <- GetDiscreteInx(meta.info);
    if(sum(disc.inx) == length(disc.inx)){
      na.msg <- c(na.msg,"All metadata columns are OK!")
    }else{
      bad.meta<- paste(names(disc.inx)[!disc.inx], collapse="; ");
      na.msg <- c(na.msg, paste0("<font style=\"color:red\">Detected presence of unique values in the following columns: <b>", bad.meta, "</b></font>","Please make sure the metadata is in right format! You can use meta editor to update the information !"));
    }
    
    cont.inx <- GetNumbericalInx(meta.info);
    cont.inx <- !disc.inx & cont.inx; # discrete is first
    
    rmcol <- intersect(which(!disc.inx),which(!cont.inx ))
  
    if(length(rmcol)==1){
     match.msg <- paste0(match.msg, "Column ",names(meta.info)[rmcol]," is removed due to lack of replicates!   " )
    }else if(length(rmcol)>1){
     match.msg <- paste0(match.msg, "Columns ",paste(names(meta.info)[rmcol],collapse = ", ")," are removed due to lack of replicates!   " )
    }
    
    if(sum(cont.inx)>0){
      # make sure the discrete data is on the left side
      meta.info <- cbind(meta.info[,disc.inx, drop=FALSE], meta.info[,cont.inx, drop=FALSE]);
    }
    disc.inx <- disc.inx[colnames(meta.info)]
    cont.inx <- cont.inx[colnames(meta.info)]
    msgSet$match.msg <-match.msg
    msgSet$na.msg <- na.msg
    saveSet(msgSet, "msgSet");  
    return(list(meta.info=meta.info,disc.inx=disc.inx,cont.inx=cont.inx))
}

