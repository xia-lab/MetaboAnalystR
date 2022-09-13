#' Data I/O for batch effect checking
#' @description Read multiple user uploaded CSV data one by one
#' format: row, col
#' @param mSetObj Input name of the created mSet Object
#' @param filePath Input the path to the batch files
#' @param format Input the format of the batch files
#' @param label Input the label-type of the files
#' @param missingEstimate Approach to estimate the missing values
#' @author Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' 
Read.BatchDataBC<-function(mSetObj=NA, filePath, format, label, missingEstimate){
  
  dat <- .readDataTable(filePath);
  
  mSetObj <- .get.mSet(mSetObj);
  
  if (any(class(mSetObj[["dataSet"]][["batch.cls"]]) == "factor")){
    
    mSetObj[["dataSet"]][["table"]] <-
      mSetObj[["dataSet"]][["class.cls"]] <-
      mSetObj[["dataSet"]][["batch.cls"]] <-
      mSetObj[["dataSet"]][["order.cls"]] <- 
      mSetObj[["dataSet"]][["batch"]] <- NULL;
    
    if (any(grepl("_edata",names(mSetObj[["dataSet"]])))){
      mSetObj[["dataSet"]][which(grepl("_edata",names(mSetObj[["dataSet"]])))]<-NULL;
    }
  }
  
  if(class(dat) == "try-error") {
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Please check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique;");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(ncol(dat) == 1){
    AddErrMsg("Error: Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(format=="row"){ # sample in row
    smpl.nms <-dat[,1];
    cls.nms <- factor(dat[,2]);
    conc <- dat[,-c(1,2)];
    var.nms <- colnames(conc);
  }else{ # sample in col
    var.nms <- as.character(dat[-1,1]);
    cls.nms <- factor(as.character(dat[1,-1]));
    dat <- dat[-1,-1];
    smpl.nms <- colnames(dat);
    conc<-t(dat);
  }
  
  #checking and make sure QC labels are unique
  qc.inx <- toupper(substr(smpl.nms, 0, 2)) == "QC";
  
  qc.nms <- smpl.nms[qc.inx];
  qc.len <- sum(qc.inx);
  if(length(unique(qc.nms))!=length(qc.nms)){
    smpl.nms[qc.inx] <- paste("QC", length(mSetObj$dataSet$batch) + 1, 1:qc.len, sep="");
  }
  
  # check the class labels
  if(!is.null(mSetObj$dataSet$batch.cls)){
    if(!setequal(levels(cls.nms), levels(mSetObj$dataSet$batch.cls[[1]])) & !setequal(levels(cls.nms), levels(mSet[["dataSet"]][["class.cls"]]))){
      AddErrMsg("The class labels in current data is different from the previous!");
      return("F");
    }
  }
  
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names (except QC) are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in sample names!");
    return("F");
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in feature names!");
    return("F");
  }
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  label <- gsub("[/-]", "_",  label);
  
  if(nchar(label) > 10){
    label <- toupper(paste(substr(label, 0, 5), substr(label, nchar(label)-5, nchar(label)), sep=""));
  }
  
  # store the data into list of list with the name order index
  # first clean the label to get rid of unusually chars
  if(label %in% names(mSetObj$dataSet$batch) || label=="F"){
    label <- paste("Dataset", length(mSetObj$dataSet$batch) + 1, sep="");
  }
  
  # check numerical matrix
  int.mat <- conc;
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  num.mat<-apply(int.mat, 2, as.numeric)
  
  msg<-NULL;
  if(sum(is.na(num.mat)) > naNms){
    # try to remove "," in thousand seperator if it is the cause
    num.mat <- apply(int.mat,2,function(x) as.numeric(gsub(",", "", x)));
    if(sum(is.na(num.mat)) > naNms){
      msg<-c(msg,"<font color=\"red\">Non-numeric values were found and replaced by NA.</font>");
    }else{
      msg<-c(msg,"All data values are numeric.");
    }
  }else{
    msg<-c(msg,"All data values are numeric.");
  }
  
  int.mat <- num.mat;
  
  rownames(int.mat)<-rowNms;
  colnames(int.mat)<-colNms;
  
  # replace NA by Estimating
  if(missingEstimate == "lods") {
    int.mat <- ReplaceMissingByLoD(int.mat);
  } else if(missingEstimate == "rmean") {
    int.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-mean(x,na.rm=T);
      }
      x;
    });
  } else if(missingEstimate == "rmed") {
    int.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-median(x,na.rm=T);
      }
      x;
    });
  } else if(missingEstimate == "knn") {
    int.mat<-t(impute::impute.knn(t(int.mat))$data);
  } else if(missingEstimate == "ppca") {
    int.mat<-pcaMethods::pca(int.mat, nPcs =5, method="ppca", center=T)@completeObs;
  } else {
    int.mat <- ReplaceMissingByLoD(int.mat);
  }
  
  mSetObj$dataSet$batch[[label]] <- int.mat;
  mSetObj$dataSet$batch.cls[[label]] <- cls.nms;
  
  # free memory
  gc();
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(label);
  }else{
    print(label);
    return(.set.mSet(mSetObj));
  }
}

#'Data I/O for batch effect checking
#'
#'@description Read peak data tale.
#'format: row, col
#'@param mSetObj Input name of the created mSet Object
#'@param filePath Input the path to the batch files
#'@param format Input the format of the batch files
#'@param missingEstimate Approach to estimate the missing values
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Read.BatchDataTB<-function(mSetObj=NA, filePath, format, missingEstimate){
  
  dat <- .readDataTable(filePath);
  
  mSetObj <- .get.mSet(mSetObj);

  mSetObj[["dataSet"]][["table"]]<-
    mSetObj[["dataSet"]][["class.cls"]]<-
    mSetObj[["dataSet"]][["batch.cls"]]<-
    mSetObj[["dataSet"]][["order.cls"]] <- NULL;
  
  if (any(grepl("_edata",names(mSetObj[["dataSet"]])))){
    
    mSetObj[["dataSet"]][which(grepl("_edata",names(mSetObj[["dataSet"]])))]<-NULL;
    
  }
  
  
  if(class(dat) == "try-error") {
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Please check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique;");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(ncol(dat) == 1){
    AddErrMsg("Error: Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(format=="row"){ # sample in row
    smpl.nms <-dat[,1];
    cls.nms <- factor(dat[,2]);
    order.nms <- factor(dat[,4]);
    batch.nms <- factor(dat[,3]);
    conc <- dat[,-c(1:4)];
    var.nms <- colnames(conc);
  }else{ # sample in col
    smpl.nms <-colnames(dat[1,])[-1];
    cls.nms <- factor(dat[1,][-1]);
    order.nms <- factor(unname(dat[3,])[-1]);
    batch.nms <- factor(unname(dat[2,])[-1]);
    conc <- t(dat[-c(1:3),]);
    var.nms <- unname(conc[1,]);
    conc <- conc[-1,]
  }
  
  #checking and make sure QC labels are unique
  # qc.inx <- toupper(substr(smpl.nms, 0, 2)) == "QC"; # Old code, delete !
  qc.inx <- grepl("QC",smpl.nms)
  
  qc.nms <- smpl.nms[qc.inx];
  qc.len <- sum(qc.inx);
  if(length(unique(qc.nms))!=length(qc.nms)){
    smpl.nms[qc.inx] <- paste(qc.nms,as.character(batch.nms),as.character(order.nms),sep = "_");
  }
  
  # check the class labels
  
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names (except QC) are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in sample names!");
    return("F");
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in feature names!");
    return("F");
  }
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  #label <- gsub("[/-]", "_",  label);
  
  #if(nchar(label) > 10){
  #  label <- toupper(paste(substr(label, 0, 5), substr(label, nchar(label)-5, nchar(label)), sep=""));
  #}
  
  # store the data into list of list with the name order index
  # first clean the label to get rid of unusually chars
  
  #if(label %in% names(mSetObj$dataSet$batch) || label=="F"){
  #  label <- paste("Dataset", length(mSetObj$dataSet$batch) + 1, sep="");
  #}
  
  # check numerical matrix
  int.mat <- conc;
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  num.mat<-apply(int.mat, 2, as.numeric)
  
  msg<-NULL;
  if(sum(is.na(num.mat)) > naNms){
    # try to remove "," in thousand seperator if it is the cause
    num.mat <- apply(int.mat,2,function(x) as.numeric(gsub(",", "", x)));
    if(sum(is.na(num.mat)) > naNms){
      msg<-c(msg,"<font color=\"red\">Non-numeric values were found and replaced by NA.</font>");
    }else{
      msg<-c(msg,"All data values are numeric.");
    }
  }else{
    msg<-c(msg,"All data values are numeric.");
  }
  
  int.mat <- num.mat;
  
  rownames(int.mat)<-rowNms;
  colnames(int.mat)<-colNms;
  
  # replace NA by Estimating
  if(missingEstimate == "lods") {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat <- ReplaceMissingByLoD(int.mat);
  } else if(missingEstimate == "rmean") {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-mean(x,na.rm=T);
      }
      x;
    });
  } else if(missingEstimate == "rmed") {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-median(x,na.rm=T);
      }
      x;
    });
  } else if(missingEstimate == "knn") {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat<-t(impute::impute.knn(t(int.mat))$data);
  } else if(missingEstimate == "ppca") {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat<-pcaMethods::pca(int.mat, nPcs =5, method="ppca", center=T)@completeObs;
  } else {
    cat("missing value estimate: ", missingEstimate, "\n")
    int.mat <- ReplaceMissingByLoD(int.mat);
  }
  
  int.mat[is.na(int.mat)] <- 0;
  int.mat[is.nan(int.mat)] <- 0;
  
  mSetObj$dataSet$table <- int.mat;
  mSetObj$dataSet$class.cls <- cls.nms;
  mSetObj$dataSet$batch.cls <- batch.nms;
  mSetObj$dataSet$order.cls <- order.nms;
  
  # free memory
  gc();
  if(.on.public.web){
    .set.mSet(mSetObj);
    # return(label);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Data I/O for signal drift checking
#'
#'@description Read peak data tale.
#'format: row, col
#'@param mSetObj Input name of the created mSet Object
#'@param filePath Input the path to the batch files
#'@param format Input the format of the batch files
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Read.SignalDriftData<-function(mSetObj=NA, filePath, format){
  
  #dat <- .readDataTable(filePath);
  dat <- .readDataTable2(filePath);
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(class(dat) == "try-error") {
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Please check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique;");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(ncol(dat) == 1){
    AddErrMsg("Error: Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return("F");
  }
  
  if(format=="row"){ # sample in row
    smpl.nms <-dat[,1];
    cls.nms <- factor(dat[,2]);
    order.nms <- factor(dat[,4]);
    batch.nms <- factor(dat[,3]);
    conc <- dat[,-c(1:4)];
    var.nms <- colnames(conc);
  }else{ # sample in col
    smpl.nms <-dat[1,];
    cls.nms <- factor(dat[2,]);
    order.nms <- factor(dat[4,]);
    batch.nms <- factor(dat[3,]);
    conc <- t(dat[-c(1:4),]);
    var.nms <- colnames(conc);
  }
  
  #checking and make sure QC labels are unique
  # qc.inx <- toupper(substr(smpl.nms, 0, 2)) == "QC"; # Old code, delete !
  qc.inx <- grepl("QC",smpl.nms)
  
  qc.nms <- smpl.nms[qc.inx];
  qc.len <- sum(qc.inx);
  if(length(unique(qc.nms))!=length(qc.nms)){
    smpl.nms[qc.inx] <- paste(qc.nms,as.character(batch.nms),as.character(order.nms),sep = "_");
  }
  
  # check the class labels
  if(!is.null(mSetObj$dataSet$batch.cls)){
    if(!setequal(levels(cls.nms), levels(mSetObj$dataSet$batch.cls[[1]]))){
      AddErrMsg("The class labels in current data is different from the previous!");
      return("F");
    }
  }
  
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names (except QC) are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return("F");
  }
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in sample names!");
    return("F");
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    AddErrMsg("No special letters (i.e. Latin, Greek) are allowed in feature names!");
    return("F");
  }
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  #label <- gsub("[/-]", "_",  label);
  
  #if(nchar(label) > 10){
  #  label <- toupper(paste(substr(label, 0, 5), substr(label, nchar(label)-5, nchar(label)), sep=""));
  #}
  
  # store the data into list of list with the name order index
  # first clean the label to get rid of unusually chars
  
  #if(label %in% names(mSetObj$dataSet$batch) || label=="F"){
  #  label <- paste("Dataset", length(mSetObj$dataSet$batch) + 1, sep="");
  #}
  
  # check numerical matrix
  int.mat <- conc;
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  num.mat<-apply(int.mat, 2, as.numeric)
  
  msg<-NULL;
  if(sum(is.na(num.mat)) > naNms){
    # try to remove "," in thousand seperator if it is the cause
    num.mat <- apply(int.mat,2,function(x) as.numeric(gsub(",", "", x)));
    if(sum(is.na(num.mat)) > naNms){
      msg<-c(msg,"<font color=\"red\">Non-numeric values were found and replaced by NA.</font>");
    }else{
      msg<-c(msg,"All data values are numeric.");
    }
  }else{
    msg<-c(msg,"All data values are numeric.");
  }
  
  int.mat <- num.mat;
  
  rownames(int.mat)<-rowNms;
  colnames(int.mat)<-colNms;
  
  # replace NA
  minConc<-min(int.mat[int.mat>0], na.rm=T)/5;
  int.mat[is.na(int.mat)] <- minConc;
  
  mSetObj$dataSet$table <- int.mat;
  mSetObj$dataSet$class.cls <- cls.nms;
  mSetObj$dataSet$batch.cls <- batch.nms;
  mSetObj$dataSet$order.cls <- order.nms;
  
  # free memory
  gc();
  if(.on.public.web){
    .set.mSet(mSetObj);
    # return(label);
  }else{
    # print(label);
    return(.set.mSet(mSetObj));
  }
}

#'Batch Effect Correction
#'@description This function is designed to perform the batch effect correction
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the name of the plot to create
#'@param Method Batch effect correction method, default is "auto". Specific method, including "Combat",
#'"WaveICA","EigenMS","QC_RLSC","ANCOVA","RUV_random","RUV_2","RUV_s","RUV_r","RUV_g","NOMIS" and "CCMN".
#'@param center The center point of the batch effect correction, based on "QC" or "", which means correct 
#'to minimize the distance between batches.
#'@importFrom plyr join ddply . summarise id
#'@importFrom dplyr rename mutate select enquo tbl_vars group_vars grouped_df group_vars
#'@import edgeR
#'@importFrom pcaMethods pca
#'@importFrom crmn standardsFit
#'@import impute
#'@import BiocParallel
#'@author Zhiqiang Pang, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformBatchCorrection <- function(mSetObj=NA, imgName=NULL, Method=NULL, center=NULL){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.batch.correct")){ # public web on same user dir
      compiler::loadcmp("../../rscripts/metaboanalystr/_util_batch.Rc");    
    }
    return(my.batch.correct(mSetObj, imgName, Method, center)); 
  }else{
    return(my.batch.correct(mSetObj, imgName, Method, center)); 
  }
}

#'Signal Drift Correction
#'@description This function is designed to perform the signal drift correction. 
#'Batch effect and signal drift correction will be performed with QC-RLSC method in this function.
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the name of the plot to create
#'@importFrom plyr join ddply . summarise id
#'@importFrom dplyr rename mutate select enquo tbl_vars group_vars grouped_df group_vars
#'@import edgeR
#'@importFrom pcaMethods pca
#'@importFrom crmn standardsFit
#'@import impute
#'@import BiocParallel
#'@author Zhiqiang Pang, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformSignalDriftCorrection <- function(mSetObj=NA, imgName=NULL){
  
  if (is.null(imgName)){
    imgName<-"image_sg"
  }
  
  ## Extract the information from the mSetObj Object
  commonMat2 <- mSetObj[["dataSet"]][["table"]];
  batch.lbl2 <- mSetObj[["dataSet"]][["batch.cls"]];
  class.lbl2 <- mSetObj[["dataSet"]][["class.cls"]];
  order.lbl2 <- mSetObj[["dataSet"]][["order.cls"]];
  QCs<-grep("QC",as.character(class.lbl2));
  
  if (identical(QCs,integer(0))){
    stop("QC samples are required for signal driift correction. Please double check your data !")
  }
  
  if (all(!is.na(as.character(unique(batch.lbl2)))) & !is.null(batch.lbl2) & 
      all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2) &
      all(!is.na(as.character(unique(order.lbl2)))) & !is.null(order.lbl2)){
    print("Correcting with QC-RLSC...");
    QC_RLSC_edata<-suppressWarnings(suppressMessages(QC_RLSC(commonMat2,batch.lbl2,class.lbl2,order.lbl2,QCs)));
    mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$QC_RLSC_edata <- QC_RLSC_edata;
  } else {
    stop("Please double check the batch, class and order information is not missing ")
  }
  
  Plot.sampletrend(mSetObj,paste(imgName,"Trend"),method="QC_RLSC");
  
  best.table <- mSetObj$dataSet$adjusted.mat
  
  # save the meta-dataset
  res <- data.frame(colnames(t(best.table)), class.lbl2, batch.lbl2, best.table);
  colnames(res) <- c('NAME', 'CLASS', 'Dataset', colnames(best.table));
  write.table(res, sep=",", file="MetaboAnalyst_signal_drift.csv", row.names=F, quote=FALSE);
  
}

#'Scatter plot colored by different batches
#'@description Scatter plot colored by different batches
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 600.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param method method of correction
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPCA.overview <- function(mSetObj, imgName, format="png", dpi=72, width=NA,method){
  
  #mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w  <- 11;
  }else{
    w <- width;
  }
  h <- 6;
  
  mSetObj$imgSet$pca.batch.overview <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow=c(1,2));
  
  nlbls <-  mSetObj$dataSet$batch.cls;
  pca <- prcomp(mSetObj$dataSet$table, center=T, scale=T);
  sum.pca<-summary(pca);
  var.pca<-sum.pca$importance[2,]; # variance explained by each PC
  
  ## plot score plot
  pc1 = pca$x[, 1];
  pc2 = pca$x[, 2];
  
  xlabel = paste("PC1", "(", round(100*var.pca[1],1), "%)");
  ylabel = paste("PC2", "(", round(100*var.pca[2],1), "%)");
  
  semi.cols <- CreateSemiTransColors(mSetObj$dataSet$batch.cls);
  if (all(semi.cols == "")){
    semi.cols <- "#FF000080"
  };
  
  plot(pc1, pc2, xlab=xlabel, ylab=ylabel, pch=21, bg=semi.cols, col="gray", cex=1.6, main="Before Adjustment");
  legend("topright", legend=unique(nlbls), pch=15, col=unique(semi.cols));
  
  qcInx <- substr(names(pc1), 0, 2) == "QC";
  if(sum(qcInx) > 0){
    points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, lwd=2);
  }
  
  df_tmp<-apply(mSetObj$dataSet$adjusted.mat,2,FUN = function(x){
    if(length(which(x==0))==length(x)){
      x[1]<-0.000001;
      return(x)
    } else {x}
  })
  
  
  table<-df_tmp;
  table[is.na(table)] <- 0;
  table[is.nan(table)] <- 0;
  table[table < 10^-20] <- 0;
  table[is.infinite(table)] <- max(table[!is.infinite(table)]);
  same <- apply(table, 2, function(.col){all(.col[1L] == .col)})
  if(length(which(same)) > 0){
    table <- table[, -which(same)]
  }
  
  
  #a1<-mSetObj$dataSet$adjusted.mat
  mSetObj$dataSet$adjusted.mat <- table;
  pca <- prcomp(mSetObj$dataSet$adjusted.mat, center=T, scale=T);
  sum.pca<-summary(pca);
  var.pca<-sum.pca$importance[2,]; # variance explained by each PC
  
  ## plot score plot
  pc1 = pca$x[, 1];
  pc2 = pca$x[, 2];
  
  xlabel = paste("PC1", "(", round(100*var.pca[1],1), "%)");
  ylabel = paste("PC2", "(", round(100*var.pca[2],1), "%)");
  
  main_name<-paste0("After Adjustment_",method)
  
  semi.cols <- CreateSemiTransColors(mSetObj$dataSet$batch.cls);
  if (all(semi.cols == "")){
    semi.cols <- "#FF000080"
  };
  
  plot(pc1, pc2, xlab=xlabel, ylab=ylabel, pch=21, bg=semi.cols, col="gray", cex=1.6, main=main_name);
  legend("topright", legend=unique(nlbls), pch=15, col=unique(semi.cols));
  
  qcInx <- substr(names(pc1), 0, 2) == "QC";
  if(sum(qcInx) > 0){
    points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, lwd=2);
  }
  
  dev.off();
  return(mSetObj);
  #return(.set.mSet(mSetObj));
  
}

#'Sample Trend Scatter
#'@description Scatter sample trend comparison between all sample of different batches
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 600.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param method method of correction
#'@author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca}, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Plot.sampletrend <- function(mSetObj, imgName, format="png", dpi=72, width=NA,method){
  
  #mSetObj <- .get.mSet(mSetObj)
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w  <- 11;
  }else{
    w <- width;
  }
  h <- 6;
  
  mSetObj$imgSet$trend.batch.overview <- imgName;
  
  # centerin the adjusted data
  adjusted.data<-t(mSetObj$dataSet$adjusted.mat)
  original.data<-t(mSetObj$dataSet$table)
  
  complete_all_center = t(scale(t(original.data), center = TRUE, scale = FALSE))
  toplot1 = svd(complete_all_center)
  
  complete_all_center = t(scale(t(adjusted.data), center = TRUE, scale = FALSE))
  toplot3 = svd(complete_all_center)
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  # Define the image
  
  par(mfcol=c(3,2))
  par(mar = c(2,2,2,2))
  
  plot.eigentrends(toplot1, "Raw Data")
  plot.eigentrends(toplot3, paste("Normalized Data",method))
  
  dev.off()
}

#'Batch Distance Plotting
#'@description Scatter sample trend comparison between all sample of different batches
#'@param mSetObj mSetObj
#'@param imgName imgName
#'@param format format
#'@param width width
#'@param dpi dpi
#'@export
plot_dist <- function(mSetObj=NA, imgName="dist",format="png", width=NA, dpi=72){
  library(ggplot2)
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w  <- length(mSetObj[["dataSet"]])*0.4+1.6;
  }else{
    w <- width;
  }
  h <- w*6/11;
  
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  Methods<-gsub("_edata",names(mSetObj[["dataSet"]][["interbatch_dis"]]),replacement = "")
  Methods[1] <- "Original_Table"
  Distance<-unname(mSetObj[["dataSet"]][["interbatch_dis"]])
  dist.sort <- order(Distance)
  
  data<-data.frame(Methods,Distance,dist.sort)
  
  p<-ggplot2::ggplot(data,aes(fill=factor(Distance),y=Distance,x=Methods)) +
    geom_bar(position="dodge", stat="identity",width = 0.7) + 
    scale_fill_grey(start = 0.1,end = 0.85) + 
    theme_bw() + 
    theme(legend.position = "none",
          axis.text.x = element_text( angle=45, hjust = 1),
          axis.text.y = element_text( angle=-30)) +
    scale_x_discrete(limits=Methods[dist.sort])+ 
    geom_text(x=1, y=min(data$Distance)*1.05, label="*",size=10, color="black")
  print(p)
  dev.off();
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetAllBatchNames <- function(mSetObj=NA){
  
  #mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$dataSet$batch)){
    return(0);
  }
  names(mSetObj$dataSet$batch);
}

ResetBatchData <- function(mSetObj=NA){
  #mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$batch <- mSetObj$dataSet$batch.cls <- NULL;
  return(.set.mSet(mSetObj));
}

#'Create semitransparant colors
#'@description Create semitransparant colors for a given class label
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CreateSemiTransColors <- function(cls){
  
  # note, the first color (red) is for QC
  col.nms <- rainbow(length(levels(cls)));
  
  # convert to semi-transparent
  semi.nms <- ToSemiTransParent(col.nms);
  
  # now expand to the one-to-one match to cls element
  col.vec <- vector(mode="character", length=length(cls));
  for (i in 1:length(levels(cls))){
    lv <- levels(cls)[i];
    col.vec[cls==lv] <- semi.nms[i];
  }
  return(col.vec);
}

# convert rgb color i.e. "#00FF00FF" to semi transparent
ToSemiTransParent <- function (col.nms, alpha=0.5){
  rgb.mat <- t(col2rgb(col.nms));
  rgb(rgb.mat/255, alpha=alpha);
}

#################################################
##### Functions for Batch Effect Correction #####

# 1. WaveICA Function
WaveICA<-function(data,batch,group){
  ### Wavelet Decomposition
  if(.on.public.web){
    dyn.load(.getDynLoadPath());
  }
  
  batch<-as.character(batch)
  group<-as.character(group)
  wf="haar";
  K=20;t=0.05;t2=0.05;alpha=0;
  level<-floor(log(nrow(data),2))
  if (is.null(colnames(data))){
    stop("data must have colnames")
  };
  coef<-list();
  for (k in 1:(level+1)){
    coef[[k]] <-matrix(NA,nrow(data),ncol(data))
  }
  for (j in 1:ncol(data)){
    #cat(paste("######Decomposition",j,"########\n"))
    data_temp<-data[,j]
    x_modwt<-modwt(data_temp,wf=wf,n.levels =level)
    for (k in 1:(level+1)){
      coef[[k]][,j]<-x_modwt[[k]]
    }
  }
  ##### ICA
  index<-level+1
  data_wave_ICA<-list()
  for (i in (1:index)){
    #cat(paste("######### ICA",i,"#############\n"))
    data_coef<-coef[[i]]
    data_coef_ICA<-normFact(fact="stICA",X=t(data_coef),ref=batch,refType ="categorical",k=K,t=t,ref2=group,refType2="categorical",t2=t2,alpha)
    data_wave_ICA[[i]]<-t(data_coef_ICA$Xn)
  }
  ### Wavelet Reconstruction
  index<-ncol(data)
  index1<-length(data_wave_ICA)
  data_coef<-matrix(NA,nrow(data_wave_ICA[[1]]),index1)
  data_wave<-matrix(NA,nrow(data_wave_ICA[[1]]),ncol(data_wave_ICA[[1]]))
  for (i in 1:index){
    #cat(paste("######Reconstruction",i,"########\n"))
    for (j in 1:index1){
      data_coef[,j]<-data_wave_ICA[[j]][,i]
    }
    data_temp<-data[,i]
    data_coef<-as.data.frame(data_coef)
    colnames(data_coef)<-c(paste("d",1:(index1-1),sep=""),paste("s",(index1-1),sep=""))
    y<-as.list(data_coef)
    attributes(y)$class<-"modwt"
    attributes(y)$wavelet<-wf
    attributes(y)$boundary<-"periodic"
    data_wave[,i]<-imodwt(y)+mean(data_temp)
  }
  rownames(data_wave)<-rownames(data)
  colnames(data_wave)<-colnames(data)
  return(data_wave)
}

# 2. Combat Function
combat<-function(data,batch,mod){
  # note, transpose to fit the gene expression format
  correc_edata <- suppressMessages(sva::ComBat(dat=t(data), batch=batch, mod=mod, par.prior=TRUE, prior.plots=FALSE))
  return(t(correc_edata))
}

# 3. RUVSeq_sample Function
RUVs_cor<-function(data,class){
  
  data<-t(data)
  
  QCm<-grep('IS',rownames(data))
  if (identical(QCm, integer(0))){
    AddErrMsg("QCm is required to be defined in data matrix colnames !")
  }
  class.list<-list()
  for (i in levels(class)){
    class.list[[i]]<-grep(i,class)
  }
  idx<-unlist(lapply(class.list,length))
  max.idx<-max(idx)
  
  differences<-matrix(nrow = length(idx),ncol = max.idx)
  for (i in seq(length(idx))){
    differences[i,]<-c(which(class==levels(class)[i]),rep(-1,(max.idx-idx[i])))
  }
  
  data[data<=0]<-0.0001
  data.log<-log10(data)
  
  data.cor<-RUVs(log10(data),
                 cIdx = QCm,
                 k = 1,
                 scIdx = differences,
                 isLog = T)[["normalizedCounts"]]
  
  data.corrected<-exp(data.cor)
  
  return(t(data.corrected))
  
}

# 4. RUVSeq_residual Function
RUVr_cor<-function(data,class){
  
  data<-t(data);
  
  QCm<-grep('IS',rownames(data))
  if (identical(QCm, integer(0))){
    AddErrMsg("QCm is required to be defined in data matrix colnames !")
  }
  
  if (length(colnames(data))!=length(unique(colnames(data)))){
    colnames(data)[(duplicated(colnames(data)))]<-paste0(colnames(data)[duplicated(colnames(data))],"_2")
  }
  
  design <- model.matrix(~class)
  
  data[data<=0]<-0.0001
  
  y <- DGEList(counts=data, group=class)
  y <- calcNormFactors(y, method="upperquartile")
  y <- estimateGLMCommonDisp(y, design)
  y <- estimateGLMTagwiseDisp(y, design)
  
  fit <- glmFit(y, design)
  res <- residuals(fit, type="deviance")
  #res <- fit[["deviance"]]
  data.log<-log10(data)
  
  seqRUVr <- RUVr(data.log,
                  QCm,
                  k=1,
                  res,
                  isLog = T)[["normalizedCounts"]]
  
  data.corrected<-exp(seqRUVr)
  
  return(t(data.corrected))
  
}

# 5. RUVSeq_g Function
RUVg_cor<-function(data){
  
  data<-t(data);
  
  QCm<-grep('IS',rownames(data))
  if (identical(QCm, integer(0))){
    AddErrMsg("QCm is required to be defined in data matrix colnames !")
  }
  
  seqRUVg <- RUVg(data,
                  QCm,
                  k=1,
                  isLog = T)[["normalizedCounts"]]
  
  return(t(seqRUVg))
  
}

# 6. RUV-random Function
RUV_random<-function(data){
  Y <- log10(data)
  if (any(grepl("IS",colnames(Y)))){
    IS<-Y[,grepl("IS",colnames(Y))] 
  } 
  
  if (is.null(IS)){
    AddErrMsg("QCm is required to be defined in data matrix colnames !")
  }
  
  r<-numeric(dim(Y)[2])
  for(j in 1:length(r)){
    r[j]<-mean(cor(IS,Y[,j]))
  }
  ctl<-logical(length(r))
  ctl[which(r>round(quantile(r,0.7,na.rm = TRUE),2))]<-TRUE 
  
  ruv<-NormalizeRUVRand(Y=Y,ctl=ctl,k=1) 
  
  return(exp(ruv$newY))
}

# 7. RUV-2 Function (caution: nu.coeff)
RUV_2<-function(data,class){
  return(naiveRandRUV(data, class, nu.coeff=2, k=1))
}

# 8. ANCOVA Function
ANCOVA<-function(data,batch,QCs){
  #require("BatchCorrMetabolomics")
  batch<-as.factor(as.character(batch));
  minBatchOccurrence.Ave <- 2
  minBatchOccurrence.Line <- 3
  conditions <- "" #creating a vector to represent the different methods of dealing with non-detects
  experiments <- "Q" #creating a vector representing the correction strategies (QCs or samples) 
  methods <- rep("lm", length(experiments)) 
  imputeValues <- rep(NA, length(experiments)) #create a vector the same length as experiments with repeating NAs
  #PosFF.Refs <- list("Q" = which(PosMeta$SCode == "ref"))
  strategies <- rep("Q", each = length(conditions)) #make a repeating vector of Qs and Ss
  
  PosFF.Final <- lapply(seq(along = experiments), function(x)
    apply(data, 2, ANCOVA_doBC,
          ref.idx = QCs,
          batch.idx = batch,
          minBsamp = minBatchOccurrence.Line,
          seq.idx = NULL,
          method = methods[x],
          imputeVal = imputeValues[x]))
  
  names(PosFF.Final) <- experiments
  
  corrected <- do.call(rbind.data.frame, PosFF.Final)
  # remove Q from row names 
  for (i in 1:nrow(corrected)){
    rownames(corrected)[i] <- gsub("^[^.]*\\.","", rownames(corrected)[i])
  }
  
  return(as.matrix(corrected))
}

# 9. QC-RLSC Function
QC_RLSC<-function(data,batch,class,order,QCs){
  
  # format the data table
  ID=colnames(data)
  sample=rownames(data)
  
  if (length(sample)!=length(unique(sample))){
    asample<-paste0(sample,"_",as.character(batch)) # to make the sample name unique.
  } else {
    asample<-sample
  }
  
  sample_split<-as.character(sapply(asample,FUN=function(x){rep(x,length(ID))}))
  values<-as.numeric(sapply(c(1:nrow(data)), FUN = function(x){data[x,]}))
  batch2<-as.character(sapply(batch,FUN=function(x){rep(x,length(ID))}))
  
  class[QCs]<-NA
  class<-as.character(sapply(class,FUN=function(x){rep(x,length(ID))}))
  order<-as.numeric(sapply(order,FUN=function(x){rep(x,length(ID))}))
  
  peaksData<-data.frame(ID=rep(ID,length(sample)),sample=sample_split,value=values,
                        batch=batch2,class=class,order=order)
  
  ## Start the process
  
  qcData <- peaksData[is.na(peaksData$class),]
  samList<-data.frame(sample=rownames(data),batch=batch,
                      class=class,order=unique(order))
  
  maxOrder<-max(samList$order); loessDegree <- 2; loessSpan <- 0.5
  qcData$ID_batch <- paste(qcData$ID,qcData$batch,sep="_")
  
  if(loessSpan==0){
    
    intPredict <- lapply(unique(qcData$ID_batch),.runFit1,
                         qcData=qcData,maxOrder=maxOrder)
    
    intPredict <- rbindlist(intPredict)
    intPredict <- as.data.frame(intPredict)
    
  }else{
    #suppressMessages(require(BiocParallel))
    intPredict <- BiocParallel::bplapply(unique(qcData$ID_batch),
                                         .runFit2,
                                         qcData=qcData,
                                         maxOrder=maxOrder,
                                         loessSpan =loessSpan,
                                         BPPARAM = BiocParallel::bpparam())
    
    #intPredict <- lapply(unique(qcData$ID_batch),.runFit2,
    #                     qcData=qcData,maxOrder=maxOrder)
    
    intPredict <- data.table::rbindlist(intPredict)
    intPredict <- as.data.frame(intPredict)
    
  }
  
  newOrder <- 1:maxOrder
  
  intPredict <- dplyr::rename(intPredict,order=newOrder)
  ## head: sample       ID     value batch class order valuePredict
  
  peaksData$valuePredict <- NULL
  peaksData$valueNorm <- NULL
  peaksData <- plyr::join(peaksData,intPredict,
                          by=intersect(names(peaksData),names(intPredict)))
  #require(plyr)
  mpa <- plyr::ddply(peaksData,plyr::.(ID),plyr::summarise,mpa=median(value,na.rm = TRUE))
  peaksData <- plyr::join(peaksData,mpa,
                          by=intersect(names(peaksData),names(mpa)))
  peaksData <- dplyr::mutate(peaksData,valuePredict=valuePredict/mpa)
  peaksData$mpa <- NULL
  
  peaksData$value[peaksData$value<=0] <- NA
  peaksData$valueNorm <- peaksData$value/peaksData$valuePredict
  peaksData$valuePredict[peaksData$valuePredict<=0] <- NA
  
  ## calculate CV using this value
  peaksData$valueNorm[peaksData$valueNorm<=0] <- NA
  
  ## Value imputation
  peaksData<-suppressMessages(.imputation(peaksData))
  ## For each batch
  ## CV plot
  cvStat <- plyr::ddply(peaksData[is.na(peaksData$class),],plyr::.(ID,batch),
                        plyr::summarise,
                        rawCV=sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),
                        normCV=sd(valueNorm,na.rm = TRUE)/mean(valueNorm,na.rm = TRUE))
  
  
  cvStatForEachBatch <- melt(cvStat,id.vars = c("ID","batch"),
                             variable.name = "CV")
  cvStatForEachBatch$batch <- as.factor(cvStatForEachBatch$batch)
  
  #message("Summary information of the CV for QC samples:")
  cvTable <- plyr::ddply(cvStatForEachBatch,plyr::.(batch,CV),plyr::summarise,
                         lessThan30=sum(value<=0.3,na.rm = TRUE),
                         total=length(value),ratio=lessThan30/total)
  
  cvStat <- plyr::ddply(peaksData[is.na(peaksData$class),],plyr::.(ID),
                        plyr::summarise,
                        rawCV=sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE),
                        normCV=sd(valueNorm,na.rm = TRUE)/mean(valueNorm,na.rm = TRUE))
  
  
  cvStatForAll <- melt(cvStat,
                       id.vars = c("ID"),
                       variable.name = "CV")
  ## output information
  #message("Summary information of the CV for QC samples:")
  cvTable <- plyr::ddply(cvStatForAll,plyr::.(CV),plyr::summarise,
                         lessThan30=sum(value<=0.3,na.rm = TRUE),
                         total=length(value),ratio=lessThan30/total)
  #print(cvTable)
  
  
  
  ########################################################
  #message("Peaks with CV > ",0.3,"!")
  #message(sum(cvStat$normCV > 0.3,na.rm = TRUE))
  tmpPeaksData <- merge(peaksData,cvStat,by="ID")
  
  if(nrow(tmpPeaksData)!=nrow(peaksData)){
    #error_file <- paste(para@outdir,"/",para@prefix,"-doQCRLSC-error.rda",
    #                    sep="")
    #message("Please see the file: ",error_file," for detail!")
    #save(peaksData,cvStat,file=error_file)
    stop("Please see detailed data in ",error_file)
  }
  peaksData <- tmpPeaksData
  peaksData <- dplyr::rename(peaksData,cv=normCV)
  
  ### Format the final table
  #x <- peaksData %>% dplyr::select(ID,sample,!!"value") %>% 
  #  spread(sample,!!"value")
  valueID="value"
  x_tmp<-dplyr::select(peaksData,ID,sample,!!valueID)
  x<-spread(x_tmp,sample,!!valueID)
  #x<-dcast(peaksData,ID~sample,value.var = valueID)
  row.names(x) <- x$ID
  x$ID <- NULL
  x[x<=0] <- NA
  corrected.data<-x
  
  #order.list <- match(as.character(peaksData$sample),asample)
  #peaksData3<-cbind(peaksData,order.list)
  #peaksData2<-peaksData3[order(order.list),];
  #cnames<-unique(peaksData2$sample)
  #a1<-lapply(1:length(sample),FUN=function(x){peaksData2[c((length(ID)*(x-1)+1):(length(ID)*x)),c(1,8)]})
  #a1<-.cbindlist(a1);rnames<-a1$ID
  #corrected.data<-a1[,colnames(a1)!="ID"][,-1]
  #rownames(corrected.data)<-rnames;
  #colnames(corrected.data)<-cnames
  
  #return(t(corrected.data))
  
  return(t(corrected.data))
  
}

# 10. EigenMS Function
EigenMS<-function(data,class){
  ## Format the data
  a1<-as.character(class)
  a1[is.na(a1)]<-"Q"
  class<-as.factor(a1)
  m_logInts<-t(data)
  
  # Running the normalization/correction
  m_prot.info<-data.frame(peak=rownames(m_logInts),ID=seq(nrow(m_logInts)))
  m_ints_eig1 = eig_norm1(m=m_logInts,treatment=class,prot.info=m_prot.info)
  m_ints_norm1 = eig_norm2(rv=m_ints_eig1)
  
  data.table<-m_ints_norm1[["normalized"]]
  
  rownames(data.table)<-data.table[,1]
  data.table<-data.table[,-c(1:2)]
  data.table<-t(data.table)
  data.table[data.table<0]<-0
  
  return(data.table)
}

# 11. NOMIS Function
NOMIS<-function(data){
  
  object<-t(data)
  
  standards<-grepl("IS",rownames(object))
  
  ana <- lana <-t(object[!standards,])
  sta <- lsta <-t(object[standards,])
  
  if((any(!is.finite(lsta)) | any(!is.finite(lsta)) | 
      any(!is.finite(lana)) | any(!is.finite(lana))) & ncol(lsta) > 1) {
    lana[!is.finite(lana)] <- NA
    lsta[!is.finite(lsta)] <- NA
    lana <- completeObs(pca(lana, method="ppca"))
    if(all(dim(lsta)) > 1)
      lsta <- completeObs(pca(lsta, method="ppca"))
  }
  
  means <- colMeans(lana)
  sds <- apply(lana, 2, sd, na.rm=TRUE)
  
  model <- list(fit=lm(I(lana)~I(lsta)), means=means)
  lnormedData <- lana - predict(model[["fit"]], data.frame(I(lsta)))
  lnormedData <- sweep(lnormedData, 2, model[["means"]], "+")
  
  return(lnormedData)
  
}

# 12. CCMN Function
CCMN2<-function(data,class){
  
  object<-t(data);
  factors<-class;
  lg<-F
  standards<-grepl("IS",rownames(object))
  factors=model.matrix(~-1+. , data=data.frame(factors))
  ncomp=2
  
  ana <- lana <-t(object[!standards,])
  sta <- lsta <-t(object[standards,])
  
  
  if((any(!is.finite(lsta)) | any(!is.finite(lsta)) | 
      any(!is.finite(lana)) | any(!is.finite(lana))) & ncol(lsta) > 1) {
    lana[!is.finite(lana)] <- NA
    lsta[!is.finite(lsta)] <- NA
    lana <- completeObs(pca(lana, method="ppca"))
    if(all(dim(lsta)) > 1)
      lsta <- completeObs(pca(lsta, method="ppca"))
  }
  
  means <- colMeans(lana)
  sds <- apply(lana, 2, sd, na.rm=TRUE)
  
  sfit <- standardsFit(object, factors, lg=lg, ncomp=ncomp,standards=standards)
  tz <- standardsPred(sfit, object, factors, lg=lg, standards=standards)
  sclana <- scale(lana);fitfunc=lm
  pfit <- fitfunc(sclana~-1+I(tz))
  model <-  list(fit=pfit,sds=sds, means=means)
  
  if(lg){
    lana <- log10(ana)
  } else{
    lana <- ana
  }
  
  ## center/scale
  lana <- scale(lana, center=model[["means"]], scale=model[["sds"]])
  ## correct
  lnormedData <- lana - predict(model[["fit"]], data.frame(I(tz)))
  ## recenter/rescale
  lnormedData <- sweep(lnormedData, 2, model[["sds"]], "*")
  lnormedData <- sweep(lnormedData, 2, model[["means"]], "+")
  
  if(lg){
    return(exp(lnormedData))
  } else{
    return(lnormedData)
  }
  
}

# 2. Evalute Correction Model
# 2.0 Model Selection
.model.evaluation<-function(mSetObj,data.type){
  #require(vegan);
  edata.dca <- try(decorana(mSetObj[["dataSet"]][[data.type]]),silent = T);
  
  # to decide CA or PCA suitable to use (Gradient Length, first axis)
  # if greater than 3, CCA will be used, otherwise PCA instead.
  # Ref: Lepš, J. & Šmilauer, P. 2003. Multivariate Analysis of Ecological Data using CANOCO. Cambridge Press.
  if (class(edata.dca)=="try-error"){
    return ("PCA")
  } else {
    gradient.length<-max(edata.dca[["rproj"]][,1]);
    if (gradient.length > 3){
      return("CCA")
    } else {
      return("PCA")
    };
  }
}

# 2.3 Distance Calculation
.evaluate.dis<-function(mSetObj, data.type, center){
  
  table <- mSetObj$dataSet[[data.type]];
  
  if(all(is.na(table)) || all(is.infinite(table))){
    # TO FIX the potential all na failure of correction, use original for this case
    table <- mSetObj$dataSet[["table"]];
  }
  
  df_tmp<-apply(table,2,FUN = function(x){
    if(length(which(x==0))==length(x)){
      x[1]<-0.000001;
      return(x)
    } else {x}
  })
  
  table<-df_tmp;
  table[is.na(table)] <- 0;
  table[is.nan(table)] <- 0;
  table[table < 10^-20] <- 0;
  table[is.infinite(table)] <- max(table[!is.infinite(table)]);
  same <- apply(table, 2, function(.col){all(.col[1L] == .col)})
  if(length(which(same)) > 0){
    table <- table[, -which(same)]
  }
  
  model <- .model.evaluation(mSetObj, data.type);
  
  if (center=="QC"){
    QC_methods<-c("Combat_edata","WaveICA_edata","EigenMS_edata","QC_RLSC_edata","ANCOVA_edata");
  } else {
    QC_methods<-c("");
  }
  
  if (data.type %in% QC_methods){
    
    #### Dustances between QCS
    if (model=="PCA"){
      pc.original<-prcomp(table,scale.=TRUE);
      nnms<-rownames(pc.original$x);
      pc.original_select<-pc.original$x[grepl("QC",nnms),1:3];
      
    } else {
      pc.original<-cca(table,scale.=T);
      nnms<-rownames(pc.original$x);
      pc.original_select<-pc.original[["CA"]][["Xbar"]][grepl("QC",nnms),1:3];
      
    }
    
    dist.original <- dist(pc.original_select,method ="euclidean")
    dist.original <- as.matrix(dist.original)
    
    ns<-dim(pc.original_select)[1]
    interbatch.dis <- sum(dist.original)/(ns*ns-ns)
    
  } else{
    
    ##### Distances between subject samples
    if (model=="PCA"){
      
      pc.original<-prcomp(table,scale.=T);
      #nnms<-rownames(pc.original$x);
      pc.original_select<-pc.original$x[,1:3];
      
    } else {
      
      pc.original<-cca(table,scale.=T);
      #nnms<-rownames(pc.original$x);
      pc.original_select<-pc.original[["CA"]][["Xbar"]][,1:3];
      
    }
    
    dist.original<-dist(pc.original_select,method ="euclidean");
    dist.original<-as.matrix(dist.original);
    
    ns<-dim(pc.original_select)[1];
    interbatch.dis <- sum(dist.original)/(ns*ns-ns);
  }
  return(interbatch.dis);
}


#### Internal Functions- WaveICA and wavelism
normFact <- function(fact,X,ref,refType,k=20,t=0.5,ref2=NULL,refType2=NULL,t2=0.5,alpha,...) {
  if (fact=='stICA'){
    obj = unbiased_stICA(X,k,alpha=alpha)
    B=obj$B
    A=obj$A
  } else if (fact == 'SVD'){
    obj = svd(X,nu=k,nv=k)
    A = obj$u%*%diag(obj$d[1:k],k)
    B = obj$v
  } else {
    stop("Factorization method should be SVD or stICA")
  }
  
  factR2 = R2(ref,B,refType,pval=T)
  
  idx = which(factR2$allpv<t)
  
  if (t <0 | t>1){stop("t not in [0 1]")}
  
  if (!is.null(ref2)){
    if (sum(t2 <0 | t2>1)){stop("t2 not in [0 1]")}
    factR2_2 = R2(ref2,B,refType2,pval=T)
    idx_2 = c()
    if (length(t2)!=length(refType2)){
      if(length(t2)==1){
        t2=rep(t2,length(refType2))
      }
      else {
        stop("length(t2) sould be equal to 1 or length(refType2)")
      }
    }
    for (i in 1:length(refType2)){
      
      idx_2 = c(idx_2,which(factR2_2$allpv[,i]<t2[i]))
    }
    
    idx2keep =intersect(idx,idx_2)
    #print(paste("Keeping",length(idx2keep), "cmpts with P value less than t2"))
    idx = setdiff(idx, idx2keep)
  }
  
  bestcmptA = A[,idx]
  bestcmptB = B[,idx]
  
  #print(paste("Removing",length(idx),"components with P value less than",t))
  
  Xn = X - bestcmptA%*% t(bestcmptB)
  
  R2=factR2$allR2
  if (!is.null(ref2)){
    R2 = cbind(R2,factR2_2$allR2)
  }
  
  return(list(Xn=Xn,R2=R2,bestSV =bestcmptB,A=A,B=B))
}

unbiased_stICA <- function(X,k=10,alpha) {
    
  jadeCummulantMatrices <- function(X) {
    
    n <- nrow(X)
    t <- ncol(X)
    
    M <- array(0,c(n,n,n*(n+1)/2))
    scale <- matrix(1,n,1)/t  # for convenience
    
    R <- cov(t(X)) # covariance
    
    k <- 1
    for (p in 1:n){
      #case q=p
      C <- ((scale %*% (X[p,]*X[p,]))*X) %*% t(X)
      E <- matrix(0,n,n)
      E[p,p] <- 1
      M[,,k] <- C - R %*% E %*% R - sum(diag(E %*% R)) * R - R %*% t(E) %*% R
      k <- k+1
      #case q<p
      if (p > 1) {
        for (q in 1:(p-1)){
          C <- ((scale %*% (X[p,]*X[q,]))*X) %*% t(X) * sqrt(2)
          E <- matrix(0,n,n)
          E[p,q] <- 1/sqrt(2)
          E[q,p] <- E[p,q]
          M[,,k] <- C - R %*% E %*% R - sum(diag(E %*% R)) * R - R %*% t(E) %*% R
          k <- k+1
        }
      }
    }
    return(M)
  }
  
  p <- nrow(X)
  n <- ncol(X)
  
  dimmin <- min(n,p)
  
  if (dimmin < k) {
    k <- dimmin
  }
  if (alpha <0 | alpha >1){
    stop("alpha not in [0 1]")
  }
  
  # Remove the spatiotemporal mean
  
  Xc <- X - matrix(rep(colMeans(X,dims=1),p),nrow = p,byrow=T);
  Xc <- Xc - matrix(rep(rowMeans(Xc,dims=1),n),nrow = p);
  
  # SVD of Xc and dimension reduction: keeping only the k first
  # components
  udv <- svd(Xc,k,k)
  D <- diag(udv$d[1:k]); if (k==1) {D <- udv$d[1]}
  U <- udv$u;
  V <- udv$v;
  
  # Estimation of the cumulant matrices
  nummat <- k*(k+1)/2;
  M <- array(0,c(k,k,2*nummat));
  Bt <- D^(1-alpha) %*% t(V)
  if (alpha == 1) { Bt <- t(V)}
  At <- D^(alpha) %*% t(U)
  if (alpha == 0) { At <- t(U)}
  M[,,1:nummat] <- jadeCummulantMatrices(Bt);
  M[,,(nummat+1):(2*nummat)] <- jadeCummulantMatrices(At)
  
  # normalization within the groups in order to allow for comparisons using
  # alpha
  M[,,1:nummat] <- alpha*M[,,1:nummat]/mean(sqrt(apply(M[,,1:nummat]*M[,,1:nummat],3,sum)));
  M[,,(nummat+1):(2*nummat)] <- (1-alpha)*M[,,(nummat+1):(2*nummat)]/mean(sqrt(apply(M[,,(nummat+1):(2*nummat)]*M[,,(nummat+1):(2*nummat)],3,sum)));
  
  # Joint diagonalization
  Worth <- rjd(M,eps = 1e-06, maxiter = 1000);
  Wo <-t (Worth$V);
  #     Computation of A and B
  
  A0 <- U %*% D^(alpha) %*% solve(Wo);
  B0 <- V%*% D^(1-alpha) %*% t(Wo);
  if (alpha == 1) { B0 <- V %*% t(Wo)}
  if (alpha == 0) { A0 <- U %*% solve(Wo)}
  
  # Add transformed means
  meanCol <- matrix(colMeans(X,dims=1),ncol =1); # spatial means
  meanRows <- matrix(rowMeans(X,dims=1),ncol = 1); # temporal means
  
  meanB <- pseudoinverse(A0) %*% (meanRows);
  meanA <- pseudoinverse(B0) %*% (meanCol);
  
  Bfin <- B0 + matrix(rep(meanB,n),nrow = n,byrow=T)
  Afin <- A0 + matrix(rep(meanA,p),nrow = p,byrow=T)
  return(list(A=Afin,B=Bfin,W=Wo))
}

modwt<-function (x, wf = "la8", n.levels = 4, boundary = "periodic") {
  switch(boundary, reflection = x <- c(x, rev(x)), periodic = invisible(), 
         stop("Invalid boundary rule in modwt"))
  N <- length(x)
  storage.mode(N) <- "integer"
  J <- n.levels
  if (2^J > N) 
    stop("wavelet transform exceeds sample size in modwt")
  dict <- wave.filter(wf)
  L <- dict$length
  storage.mode(L) <- "integer"
  ht <- dict$hpf/sqrt(2)
  storage.mode(ht) <- "double"
  gt <- dict$lpf/sqrt(2)
  storage.mode(gt) <- "double"
  y <- vector("list", J + 1)
  names(y) <- c(paste("d", 1:J, sep = ""), paste("s", J, sep = ""))
  W <- V <- numeric(N)
  storage.mode(W) <- "double"
  storage.mode(V) <- "double"
  for (j in 1:J) {
    out <- C_modwt_r(as.double(x), N, as.integer(j), L, 
                     ht, gt, W = W, V = V)
    y[[j]] <- out$W
    x <- out$V
  }
  y[[J + 1]] <- x
  class(y) <- "modwt"
  attr(y, "wavelet") <- wf
  attr(y, "boundary") <- boundary
  return(y)
}

R2 <- function(poi,V,poiType,pval = T) {
  #
  #   R2(poi,V,poiType)
  #
  # Args:
  # - V is a p*k matrix, where the rows corresponds to the samples
  # - poi is a matrix p*l, representing the phenotypes of interest
  # - poiType (1*l) is the types of poi: 'continuous' (then a linear
  # regression is used) or 'categorical' (then the mean by class is used)
  #
  # Outputs:
  # - R2(l), higher R^2 value between a column of V and poi(l)
  # - idxCorr(l), index of the column of V giving the higher R^2 value (if many,
  # takes the first one)
  # - allR2(k,l),  R2 value for column k of V with poi l
  #
  #    IF pval =TRUE, return also:   #
  # - pv(l) smaller p-value association between a column of V and poi(l)
  # - idxcorr2(l) index of the column of V giving the smaller p-value (if many,
  #                                                                          # takes the first one)
  # - allpv(k,l),  p-value for column k of V with poi l
  #
  # if missing information in poi, remove the corresponding samples in the R2 computation
  
  
  if (is.vector(V) ){ V = matrix(V,ncol=1)}
  if (is.vector(poi)){poi = matrix(poi,nrow =length(poi))}
  
  p = nrow(V)   # number of samples
  k = ncol(V)    # number of components
  l = length(poiType)  # number of cf/poi to test
  if (is.null(l)){stop("POI type(s) neeeded")}
  p2 = nrow(poi)
  l2 = ncol(poi)
  
  if( l2 != l){ # checking poi and poiType dimensions compatiblity
    if (p2 == l){  # if poi is transposed (l*p)
      poi = t(poi)
      warning("Transposing poi to match poiType dimension")
      p2 = nrow(poi)
    } else {
      #print(poi)
      #print(poiType)
      stop("poi dimensions doesn't match poiType dimension")
    }
  }
  
  if (p != p2){ # checking poi and V dimensions compatiblity
    if (p2 == k){
      warnings("Transposing V to match poi dimension")
      V =t(V)
      k = p
      p = p2
    } else {
      stop("poi and V dimensions incompatible")
    }
  }
  
  R2 = rep(-1,l)
  names(R2) = colnames(poi)
  idxcorr = R2
  R2_tmp <- matrix(rep(-1,k*l),k,l,dimnames=list(colnames(V),colnames(poi)))    # r2_tmp(k,l) hold the R2 value for column k of V with poi l
  
  if (pval){
    pv = R2
    idxcorr2 = R2
    pv_tmp <- R2_tmp   # r2_tmp(k,l) hold the R2 value for column k of V with poi l
  }
  
  for (cmpt in 1:k){    # for each column of V
    cmpt2an <- V[,cmpt]
    for (ipoi in 1:l){
      idx_finite = is.finite(as.factor(poi[,ipoi]))
      poi2an = poi[idx_finite,ipoi]
      cmpt2an_finite=cmpt2an[idx_finite]
      if (poiType[ipoi] == "continuous") {  # estimation by linear regression
        coefs <- coef(lm(cmpt2an_finite~as.numeric(poi2an)))
        cmpt2an_est <- coefs[2]*as.numeric(poi2an)+coefs[1]
        nc <- 2;
      } else if (poiType[ipoi]=="categorical"){  # estimation by classe mean
        classes <- unique(poi2an)
        nc <- length(classes)
        cmpt2an_est <- rep(NA,length(cmpt2an_finite))
        for (icl in 1:length(classes) ){
          idxClasse <- which(poi2an==classes[icl])
          cmpt2an_est[idxClasse] <- mean(cmpt2an_finite[idxClasse])
        }
      } else {
        stop("Incorrect poiType. Select 'continuous' or 'categorical'. ")
      }
      sse <- sum((cmpt2an_finite-cmpt2an_est)^2)
      sst <- sum((cmpt2an_finite-mean(cmpt2an_finite))^2)
      R2_tmp[cmpt,ipoi] <-  1 - sse/sst
      if (pval){
        F <- ((sst-sse)/(nc-1))/(sse/(p-nc))
        pv_tmp[cmpt,ipoi] = 1-pf(F,nc-1,p-nc);
        if (!is.finite(pv_tmp[cmpt,ipoi])) {
          warning(paste("Non finite p-value for component ",cmpt," (pv=",pv_tmp[cmpt,ipoi],", F=",F,"), assigning NA", sep=""))
          pv_tmp[cmpt,ipoi] <- NA
        }
      }
    }
  }
  
  for (ipoi in 1:l){
    if (pval){
      pv[ipoi] <- min(pv_tmp[,ipoi])
      idxcorr2[ipoi] <- which(pv_tmp[,ipoi] == pv[ipoi])[1]   # if more than one component gives the best R2, takes the first one
    }
    R2[ipoi] <- max(R2_tmp[,ipoi])
    idxcorr[ipoi] <- which(R2_tmp[,ipoi] == R2[ipoi])[1]   # if more than one component gives the best R2, takes the first one
  }
  
  if (pval){
    return(list(R2=R2,idxcorr=idxcorr,allR2 = R2_tmp, pv=pv,idxcorr2=idxcorr2,allpv = pv_tmp))
  } else {
    return(list(R2=R2,idxcorr=idxcorr,allR2 = R2_tmp))
  }
}

wave.filter <- function(name){
  select.haar <- function() {
    L <- 2
    g <- c(0.7071067811865475, 0.7071067811865475)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.d4 <- function() {
    L <- 4
    g <- c(0.4829629131445341, 0.8365163037378077, 0.2241438680420134, 
           -0.1294095225512603)
    h <- qmf(g)
    
    return(list(length = L, hpf = h, lpf = g))
  }
  select.mb4 <- function() {
    L <- 4
    g <- c(4.801755e-01, 8.372545e-01, 2.269312e-01, -1.301477e-01)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.bs3.1 <- function() {
    L <- 4
    g <- c(0.1767767, 0.5303301, 0.5303301, 0.1767767)
    h <- qmf(g)
    gd <- c(0.3535534, 1.06066, -1.06066, -0.3535534)
    hd <- qmf(g)
    return(list(length = L, hpf = h, lpf = g, dhpf = hd, dlpf = gd))
  }
  select.w4 <- function() {
    L <- 4
    g <- c(-1, 3, 3, -1) / 8
    h <- c(-1, 3, -3, 1) / 8
    return(list(length = L, hpf = h, lpf = g))
  }
  select.fk4 <- function() {
    L <- 4
    g <- c(.6539275555697651, .7532724928394872, .5317922877905981e-1,
           -.4616571481521770e-1)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.d6 <- function() {
    L <- 6
    g <- c(0.3326705529500827, 0.8068915093110928, 0.4598775021184915,
           -0.1350110200102546, -0.0854412738820267, 0.0352262918857096)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.fk6 <- function() {
    L <- 6
    g <- c(.4279150324223103, .8129196431369074, .3563695110701871,
           -.1464386812725773, -.7717775740697006e-1, .4062581442323794e-1)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.d8 <- function() {
    L <- 8
    g <- c(0.2303778133074431, 0.7148465705484058, 0.6308807679358788,
           -0.0279837694166834, -0.1870348117179132, 0.0308413818353661,
           0.0328830116666778, -0.0105974017850021)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.fk8 <- function() {
    L <- 8
    g <- c(.3492381118637999, .7826836203840648, .4752651350794712,
           -.9968332845057319e-1, -.1599780974340301, .4310666810651625e-1,
           .4258163167758178e-1, -.1900017885373592e-1)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.la8 <- function() {
    L <- 8
    g <- c(-0.07576571478935668, -0.02963552764596039, 0.49761866763256290, 
           0.80373875180538600, 0.29785779560560505, -0.09921954357695636, 
           -0.01260396726226383, 0.03222310060407815)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.mb8 <- function() {
    L <- 8
    g <- rev(c(-1.673619e-01, 1.847751e-02, 5.725771e-01, 7.351331e-01,
               2.947855e-01, -1.108673e-01, 7.106015e-03, 6.436345e-02))
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.bl14 <- function() {
    L <- 14
    g <- c( 0.0120154192834842, 0.0172133762994439, -0.0649080035533744,
            -0.0641312898189170, 0.3602184608985549, 0.7819215932965554,
            0.4836109156937821, -0.0568044768822707, -0.1010109208664125,
            0.0447423494687405, 0.0204642075778225, -0.0181266051311065,
            -0.0032832978473081, 0.0022918339541009)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.fk14 <- function() {
    L <- 14
    g <- c(.2603717692913964, .6868914772395985, .6115546539595115,
           .5142165414211914e-1, -.2456139281621916, -.4857533908585527e-1,
           .1242825609215128, .2222673962246313e-1, -.6399737303914167e-1,
           -.5074372549972850e-2, .2977971159037902e-1, -.3297479152708717e-2,
           -.9270613374448239e-2, .3514100970435962e-2)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.d16 <- function() {
    L <- 16
    g <- c(0.0544158422431049, 0.3128715909143031, 0.6756307362972904,
           0.5853546836541907, -0.0158291052563816, -0.2840155429615702,
           0.0004724845739124, 0.1287474266204837, -0.0173693010018083,
           -0.0440882539307952, 0.0139810279173995, 0.0087460940474061,
           -0.0048703529934518, -0.0003917403733770, 0.0006754494064506,
           -0.0001174767841248)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.la16 <- function() {
    L <- 16
    g <- c(-0.0033824159513594, -0.0005421323316355, 0.0316950878103452, 
           0.0076074873252848, -0.1432942383510542, -0.0612733590679088, 
           0.4813596512592012, 0.7771857516997478, 0.3644418948359564, 
           -0.0519458381078751, -0.0272190299168137, 0.0491371796734768, 
           0.0038087520140601, -0.0149522583367926, -0.0003029205145516, 
           0.0018899503329007)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.mb16 <- function() {
    L <- 16
    g <- rev(c(-1.302770e-02, 2.173677e-02, 1.136116e-01, -5.776570e-02, 
               -2.278359e-01, 1.188725e-01, 6.349228e-01, 6.701646e-01, 
               2.345342e-01, -5.656657e-02, -1.987986e-02, 5.474628e-02, 
               -2.483876e-02, -4.984698e-02, 9.620427e-03, 5.765899e-03))
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.la20 <- function() {
    L <- 20
    g <- c(0.0007701598091030, 0.0000956326707837, -0.0086412992759401,
           -0.0014653825833465, 0.0459272392237649, 0.0116098939129724,
           -0.1594942788575307, -0.0708805358108615, 0.4716906668426588,
           0.7695100370143388, 0.3838267612253823, -0.0355367403054689,
           -0.0319900568281631, 0.0499949720791560, 0.0057649120455518,
           -0.0203549398039460, -0.0008043589345370, 0.0045931735836703,
           0.0000570360843390, -0.0004593294205481)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.bl20 <- function() {
    L <- 20
    g <- c(0.0008625782242896, 0.0007154205305517, -0.0070567640909701,
           0.0005956827305406, 0.0496861265075979, 0.0262403647054251,
           -0.1215521061578162, -0.0150192395413644, 0.5137098728334054,
           0.7669548365010849, 0.3402160135110789, -0.0878787107378667,
           -0.0670899071680668, 0.0338423550064691, -0.0008687519578684,
           -0.0230054612862905, -0.0011404297773324, 0.0050716491945793,
           0.0003401492622332, -0.0004101159165852)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.fk22 <- function() {
    L <- 22
    g <- c(.1938961077599566, .5894521909294277, .6700849629420265,
           .2156298491347700, -.2280288557715772, -.1644657152688429,
           .1115491437220700, .1101552649340661, -.6608451679377920e-1,
           -.7184168192312605e-1, .4354236762555708e-1, .4477521218440976e-1,
           -.2974288074927414e-1, -.2597087308902119e-1, .2028448606667798e-1,
           .1296424941108978e-1, -.1288599056244363e-1, -.4838432636440189e-2,
           .7173803165271690e-2, .3612855622194901e-3, -.2676991638581043e-2,
           .8805773686384639e-3)
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  select.mb24 <- function() {
    L <- 24
    g <- rev(c(-2.132706e-05, 4.745736e-04, 7.456041e-04, -4.879053e-03,
               -1.482995e-03, 4.199576e-02, -2.658282e-03, -6.559513e-03,
               1.019512e-01, 1.689456e-01, 1.243531e-01, 1.949147e-01,
               4.581101e-01, 6.176385e-01, 2.556731e-01, -3.091111e-01,
               -3.622424e-01, -4.575448e-03, 1.479342e-01, 1.027154e-02,
               -1.644859e-02, -2.062335e-03, 1.193006e-03, 5.361301e-05))
    h <- qmf(g)
    return(list(length = L, hpf = h, lpf = g))
  }
  
  switch(name,
         "haar" = select.haar(),
         "d4" = select.d4(),
         "mb4" = select.mb4(),
         "w4" = select.w4(),
         "bs3.1" = select.bs3.1(),
         "fk4" = select.fk4(),
         "d6" = select.d6(),
         "fk6" = select.fk6(),
         "d8" = select.d8(),
         "fk8" = select.fk8(),
         "la8" = select.la8(),
         "mb8" = select.mb8(),
         "bl14" = select.bl14(),
         "fk14" = select.fk14(),
         "d16" = select.d16(),
         "la16" = select.la16(),
         "mb16" = select.mb16(),
         "la20" = select.la20(),
         "bl20" = select.bl20(),
         "fk22" = select.fk22(),
         "mb24" = select.mb24(),
         stop("Invalid selection for wave.filter"))
}

qmf <- function(g, low2high = TRUE) {
  L <- length(g)
  if(low2high)
    h <- (-1)^(0:(L - 1)) * rev(g)
  else
    h <- (-1)^(1:L) * rev(g)
  return(h)
}

imodwt <- function(y){
  ctmp <- class(y)
  if(is.null(ctmp) || all(ctmp != "modwt"))
    stop("argument `y' is not of class \"modwt\"")
  
  J <- length(y) - 1
  
  dict <- wave.filter(attributes(y)$wavelet)
  L <- dict$length
  storage.mode(L) <- "integer"
  ht <- dict$hpf / sqrt(2)
  storage.mode(ht) <- "double"
  gt <- dict$lpf / sqrt(2)
  storage.mode(gt) <- "double"
  
  jj <- paste("s", J, sep="")
  X <- y[[jj]]
  N <- length(X)
  storage.mode(N) <- "integer"
  XX <- numeric(N)
  storage.mode(XX) <- "double"
  for(j in J:1) {
    jj <- paste("d", j, sep="")
    X <- C_imodwt_r(as.double(y[[jj]]), as.double(X), N, as.integer(j), 
                    L, ht, gt, XX)
  }
  if(attr(y, "boundary") == "reflection") return(X[1:(N/2)])
  else return(X)
}

### Internal Functions - JADE 
rjd <-function(X, eps = 1e-06, maxiter = 100, na.action = na.fail){
  X <- na.action(X)
  dim.X <- dim(X)
  
  if (length(dim.X)==2) type <- "Matrix"
  if (length(dim.X)==3) type <- "Array"
  if ((length(dim.X) %in% c(2,3))==FALSE) stop("'X' must have two or three dimensions")
  
  if (type == "Array")
  {
    if (dim.X[1] != dim.X[2]) stop("'X' must be an array with dim of the form c(p,p,k)")
    p <- dim.X[1]
    Xt <- aperm(X, c(1,3,2))
    X <- matrix(Xt, ncol=p)
  }
  
  if (!all(sapply(X, is.numeric)))  stop("'X' must be numeric")
  X <- as.matrix(X)
  X.data <- X
  
  p <- dim(X)[2]
  if (p==1) stop("'X' must be at least bivariate")
  kp <- dim(X)[1]
  
  
  k <- kp/p
  if (floor(k) != ceiling(k)) stop("'X' must me a matrix of k stacked pxp matrices")
  
  V <- diag(p)
  encore <- TRUE
  iter <- 0
  while (encore==TRUE)
  {
    iter <- iter +1
    encore <- FALSE
    for (i in 1:(p-1))
    {
      for (j in (i+1):(p))
      {
        Ii <- seq(i,kp,p)
        Ij <- seq(j,kp,p)
        
        g1 <- X[Ii,i]-X[Ij,j]
        g2 <- X[Ij,i]+X[Ii,j]
        
        g <- cbind(g1,g2)
        
        gg <- crossprod(g)
        ton <- gg[1,1]-gg[2,2]
        toff <- gg[1,2]+gg[2,1]
        theta <- 0.5*atan2(toff, ton+sqrt(ton*ton+toff*toff))
        
        cos.theta <- cos(theta)
        sin.theta <- sin(theta)
        
        if (abs(sin.theta)>eps)
        {
          encore <- TRUE
          
          Mi <- X[Ii,]
          Mj <- X[Ij,]
          
          X[Ii,] <- cos.theta * Mi + sin.theta * Mj
          X[Ij,] <- cos.theta * Mj - sin.theta * Mi
          
          col.i <- X[,i]
          col.j <- X[,j]
          
          X[,i] <- cos.theta * col.i + sin.theta * col.j
          X[,j] <- cos.theta * col.j - sin.theta * col.i
          
          temp <- V[i,]
          V[i,] <- cos.theta * V[i,] + sin.theta * V[j,]
          V[j,] <- cos.theta * V[j,] - sin.theta * temp
        }
      }
      
    }
    if (iter >= maxiter) stop("maxiter reached without convergence")    
  }
  
  recomp <- function(X,V)
  {as.data.frame(V %*% tcrossprod(as.matrix(X), V))}
  Z <- split(as.data.frame(X.data), rep(1:k, each=p))
  Z2 <- sapply(Z, recomp, V=as.matrix(V), simplify=FALSE)
  Z3 <- matrix(unlist(lapply(Z2, t)), ncol=p, byrow=TRUE) 
  if (type == "Array")
  {
    D <- aperm(array(t(Z3), dim = c(p,p,k)), c(2,1,3), resize = FALSE)
  }
  else
  {
    D <- Z3
  }
  return(list(V=t(V),D=D))
}

### Internal Functions - corpcor
pseudoinverse = function (m, tol) {
  msvd = fast.svd(m, tol)
  
  if (length(msvd$d) == 0)
  {
    return(
      array(0, dim(m)[2:1])
    )
  }
  else
  {
    return( 
      msvd$v %*% (1/msvd$d * t(msvd$u))
    )
  }    
}

fast.svd = function(m, tol) {  
  n = dim(m)[1]
  p = dim(m)[2]
  
  EDGE.RATIO = 2 # use standard SVD if matrix almost square
  if (n > EDGE.RATIO*p)
  {
    return(psmall.svd(m,tol))
  }
  else if (EDGE.RATIO*n < p)
  {  
    return(nsmall.svd(m,tol)) 
  }
  else # if p and n are approximately the same
  {
    return(positive.svd(m, tol))
  }
}

psmall.svd = function(m, tol) {
  B = crossprod(m)   # pxp matrix
  s = svd(B,nu=0)    # of which svd is easy..
  
  # determine rank of B  (= rank of m)
  if( missing(tol) ) 
    tol = dim(B)[1]*max(s$d)*.Machine$double.eps 
  Positive = s$d > tol                            
  
  # positive singular values of m  
  d = sqrt(s$d[Positive])
  
  # corresponding orthogonal basis vectors
  v = s$v[, Positive, drop=FALSE]
  u = m %*% v %*% diag(1/d, nrow=length(d))
  
  return(list(d=d,u=u,v=v))
}

nsmall.svd = function(m, tol) {
  B = m %*% t(m)     # nxn matrix
  s = svd(B,nv=0)    # of which svd is easy..
  
  # determine rank of B  (= rank of m)
  if( missing(tol) ) 
    tol = dim(B)[1]*max(s$d)*.Machine$double.eps 
  Positive = s$d > tol                            
  
  # positive singular values of m  
  d = sqrt(s$d[Positive])
  
  # corresponding orthogonal basis vectors
  u = s$u[, Positive, drop=FALSE]
  v = crossprod(m, u) %*% diag(1/d, nrow=length(d))   
  
  return(list(d=d,u=u,v=v))
}

positive.svd = function(m, tol)
{
  s = svd(m)
  
  if( missing(tol) ) 
    tol = max(dim(m))*max(s$d)*.Machine$double.eps
  Positive = s$d > tol
  
  return(list(
    d=s$d[Positive],
    u=s$u[, Positive, drop=FALSE],
    v=s$v[, Positive, drop=FALSE]
  ))
}


##### Internal Functions for ANCOVA
#' @importFrom MASS rlm
ANCOVA_doBC <- function(Xvec, ref.idx, batch.idx, seq.idx,
                        result = c("correctedX", "corrections"),
                        method = c("lm", "rlm", "tobit"),
                        correctionFormula = formula("X ~ S * B"),
                        minBsamp = ifelse(is.null(seq.idx), 2, 4),
                        imputeVal = NULL, ...) {
  
  result <- match.arg(result)
  method <- match.arg(method)
  
  if (is.null(imputeVal) & method == "tobit")
    stop("Tobit regression requires a value for 'imputeVal'")
  
  ## next line commented out to get rid of unused levels...
  ##   if (!is.factor(batch.idx))
  batch.idx <- factor(batch.idx)
  nbatches <- nlevels(batch.idx)
  if (is.factor(seq.idx)) seq.idx <- as.numeric(levels(seq.idx))[seq.idx]
  
  if (is.null(seq.idx) & method != "lm") {
    warning("Using method = 'lm' since seq.idx equals NULL")
  }
  
  ## convert TRUE/FALSE vector to vector of numerical indices
  if (is.logical(ref.idx)) ref.idx <- which(ref.idx)
  Xref <- Xvec[ref.idx]
  Bref <- batch.idx[ref.idx]
  Sref <- seq.idx[ref.idx]
  
  glMean <- mean(Xref, na.rm = TRUE)
  
  ## check that at least minBsamp samples per batch are present
  ## if less than minBsamp samples present, remove batch for correction by
  ## setting values to NA. 
  nNonNA <- tapply(Xref, Bref, function(x) sum(!is.na(x)))
  tooFew <- names(nNonNA)[nNonNA < minBsamp]
  ## no batch found with enough samples...
  if (length(tooFew) > length(levels(Bref))-2)
    return(rep(NA, length(Xvec)))
  
  if (is.null(seq.idx)) {
    if (!is.null(imputeVal))
      Xref[is.na(Xref)] <- imputeVal
    
    Bmod <- lm(Xref ~ Bref - 1)
    Bcorrections <- (glMean - coef(Bmod))[ batch.idx ]
    
    switch(result,
           correctedX = Xvec + Bcorrections,
           Bcorrections)
  } else {
    ## finally plug in imputeVal for non-detects
    if (!is.null(imputeVal))
      Xref[is.na(Xref)] <- imputeVal
    fitdf <- data.frame(S = Sref, B = Bref, X = Xref)
    ## subset argument for rlm does not work correctly: the number of
    ## levels is not diminished and as a result we get a singular
    ## matrix... the only solution is to take out the unused levels
    ## from the fitdf object.
    if (length(tooFew) > 0) {
      fitdf <- fitdf[!(fitdf$B %in% tooFew),]
      fitdf$B <- factor(fitdf$B)
    }
    
    Bmods2 <- switch(method,
                     lm = lm(correctionFormula, data = fitdf),
                     rlm = MASS::rlm(correctionFormula, data = fitdf,
                                     maxit = 50),
                     ###                     tobit = AER:::tobit(correctionFormula, data = fitdf,
                     ###                                         left = imputeVal),
                     tobit = .crch(correctionFormula, data = fitdf,
                                   left = imputeVal))
    
    ## Now make predictions for each sample, not just the ref samples
    ## The actual correction is the following:
    ## ycorr = y - pred + gm
    predictdf <- data.frame(S = seq.idx, B = batch.idx)
    predictdf$B[predictdf$B %in% tooFew] <- NA
    predictions <- rep(NA, length(Xvec))
    predictions[!(predictdf$B %in% tooFew)] <-
      predict(Bmods2, newdata = predictdf)
    
    switch(result,
           correctedX = Xvec + glMean - predictions,
           glMean - predictions)
  }
}

ANCOVA_evaluatePCA <- function(data, batch, noref.idx,
                               npc = 2, plot = FALSE, 
                               batch.colors, scaleX = TRUE,
                               legend.loc = "topright",
                               legend.col = 2, ..., perBatch = TRUE) {
  nbatches <- nlevels(batch)
  
  noref.idx <- noref.idx
  Xsample <- X[noref.idx,]
  #YSample <- Y[noref.idx,]
  
  Xsample <- Xsample[, apply(Xsample, 2, function(x) !all(is.na(x)))]
  
  ## replace NA values with column means
  for (i in 1:ncol(Xsample))
    Xsample[is.na(Xsample[,i]),i] <- mean(Xsample[,i], na.rm = TRUE)
  
  Xsample <- Xsample[, apply(Xsample, 2, sd, na.rm = TRUE) > 0]
  
  X.PCA <- PCA(scale(Xsample))
  
  
  Xscores <- scores.PCA(X.PCA)[, 1:npc, drop = FALSE]
  ## a double loop is necessary to compare all batches... we
  ## only do the top triangle.
  batch.means <-
    lapply(levels(YSample$Batch),
           function(btch)
             colMeans(Xscores[which(YSample$Batch == btch),,drop=FALSE]))
  batch.covs <-
    lapply(levels(YSample$Batch),
           function(btch)
             cov(Xscores[which(YSample$Batch == btch),,drop=FALSE]))
  noCov.idx <- which(sapply(batch.covs, function(x) all(x < 1e-8)))
  if ((nnoCov <- length(noCov.idx)) > 0) {
    warning(paste("Too little information for batch correction in the following batches:\n",
                  levels(YSample$Batch)[noCov.idx],
                  "- ignoring these batches in the PCA criterion"))
    nbatches <- nbatches - nnoCov
    batch.covs <- batch.covs[-noCov.idx]
    batch.means <- batch.means[-noCov.idx]
  }
  
  batch.dist <- matrix(0, nbatches, nbatches)
  for (i in 2:nbatches)
    for (j in 1:(i-1))
      batch.dist[j, i] <- bhattacharyya.dist(batch.means[[j]],
                                             batch.means[[i]],
                                             batch.covs[[j]],
                                             batch.covs[[i]])
  
  if (perBatch) {
    batch.dist + t(batch.dist)
  } else {
    ## Here we take the mean and not the median since one deviating
    ## batch is already a problem.
    mean(batch.dist[col(batch.dist) > row(batch.dist)])
  }
}

### Internal Functions - MetNorm Package

NormalizeRUVRand <- function(Y, ctl, k=NULL,lambda=NULL,plotk=TRUE){
  output<-RUVRand(Y=Y, ctl=ctl,lambda=lambda, k=k)
  return(structure(output, class="normdata"))  
}

RUVRand <- function(Y, ctl,lambda=NULL, k=NULL,...){
  
  Yc<-Y[, ctl]
  svdYc <- svd(Yc)
  fullW <- svdYc$u %*% diag(svdYc$d)  
  if(is.null(k))
    stop('k must be entered')    
  if (!is.null(k) & is.null(lambda)){ 
    optklambda<-opt(ktry=k, W=fullW,Yc=Yc)
    lambda<-optklambda$optmat[,3]     
  } else optklambda<-NULL
  
  W<-fullW[,1:k,drop=FALSE]
  alpha<-solve(t(W)%*%W + lambda*diag(k), t(W) %*% Y)
  uvcomp<-W %*% alpha
  newY <- Y - uvcomp 
  return(list(unadjY=Y,newY=newY,UVcomp=uvcomp,W=W,alpha= alpha,opt=optklambda,
              k=k,lambda=lambda,ctl=ctl))  
}

loglik<- function (par, Y,W){
  m <- ncol(Y)
  n<-nrow(Y)
  if (!is.null(W)){
    sigma2.a<-par[1]
    sigma2.e<-par[2]  
    if ((sigma2.a<0)|(sigma2.e<0))
      return(1e6)
    Sigma<-sigma2.a*(W%*%t(W))+sigma2.e*diag(m)
  } else{
    Sigma <- diag(m) 
    Sigma[upper.tri(Sigma, diag=TRUE)] <- par 
    Sigma <- Sigma + t(Sigma) - diag(diag(Sigma)) 
  }
  ed = eigen(Sigma, symmetric = TRUE)
  ev = ed$values
  if (!all(ev >= -1e-06 * abs(ev[1])))
    return(1e6)
  mu<-rep(0,m)
  centeredx<-sweep(Y,2,mu,"-")
  ssnew<-  t(centeredx)%*%(centeredx)
  if (is.null(tryCatch(solve(Sigma), error=function(e) NULL)))
    return(1e6)
  else
    inv.Sigma<-solve(Sigma) 
  Sigmainvss<-inv.Sigma%*%ssnew
  return(n*determinant(Sigma,logarithm=T)$mod+sum(diag(Sigmainvss)))
}

opt<-function(ktry,W,Yc){
  opt<-list()
  optmat<-matrix(NA,nrow=1,ncol=8)
  colnames(optmat)<-c("sigma2.a","sigma2.e","nu",
                      "lower_sigma2.a","upper_sigma2.a",
                      "lower_sigma2.e","upper_sigma2.e",
                      "convergence")
  
  opt<-optim(c(0.1,0.1),
             loglik,
             Y=t(Yc),
             W=W[,1:ktry,drop=FALSE],
             hessian=T)    
  fisher_info<-solve(opt$hessian/2)
  se<-sqrt(diag(fisher_info))
  upper_par1<-opt$par[1]+1.96*se[1]
  lower_par1<-opt$par[1]-1.96*se[1]
  upper_par2<-opt$par[2]+1.96*se[2]
  lower_par2<-opt$par[2]-1.96*se[2]
  
  optmat[1,]<-c(opt$par[1],
                opt$par[2],
                opt$par[2]/opt$par[1],
                lower_par1, upper_par1,
                lower_par2, upper_par2,                  
                opt$convergence)
  
  rownames(optmat)<-ktry
  return(list(optmat=optmat, opt=opt))
}

RuvRandIter <- function(RUVRand,maxIter,wUpdate=maxIter+1, lambdaUpdate=TRUE,p=p,...){
  Y<-RUVRand$unadjY
  ctl<-RUVRand$ctl
  ndim<-RUVRand$ndim
  m <- nrow(Y) 
  n <- ncol(Y) 
  
  converged <- 0
  iter <- 0
  currObj <- Inf
  cEps<-1e-6
  
  
  W <- RUVRand$W
  a <- RUVRand$alpha
  lambda<-RUVRand$lambda
  k<-RUVRand$k
  Wa <- W %*% a
  
  X <- matrix(0,m,p)
  b <- matrix(0,p,n)
  Xb <- X %*% b
  
  while(!converged){
    iter <- iter + 1
    
    #print('Estimating the factor of interest')
    XbOld <- Xb
    
    kmres <- kmeans((Y[, -ctl] - Wa[, -ctl]),centers=p,nstart=20,...)
    idx <- kmres$cluster
    for(kk in 1:p){
      X[, kk] <- cbind(as.numeric(idx==kk))
    }
    b[, -ctl] <- kmres$centers
    Xb <- X %*% b
    
    WaOld <- Wa
    WOld <- W    
    
    if(iter / wUpdate == iter %/% wUpdate){
      
      #print('Re-estimating W')
      
      svdYmXb <- svd((Y - Xb))
      fullW <- svdYmXb$u %*% diag(svdYmXb$d) 
      if (lambdaUpdate){
        #print('Re-estimating k and lambda')
        barplot(prcomp((Y - Xb),scale. =T)$sdev^2/
                  sum(prcomp((Y - Xb),scale. =T)$sdev^2),
                xlim=c(0,min(dim(Y))+1),
                names.arg =c(1:min(dim(Y))),
                ylim=c(0,1),
                xlab="k",
                ylab="proportion of the variance",
                cex.lab=1.2,cex.axis=1.2)
        
        k<-readk()
        W <- fullW[,c(1:k)]    
        optklambda<-opt(ktry=k,
                        W=W,
                        Yc=(Y - Xb))
        lambda<-optklambda$optmat[,3] 
        #print(paste("lambda =" ,lambda))      
      }
    }
    
    #print('Estimating the unwanted variation component')
    
    
    
    a <- solve(t(W)%*%W + lambda*diag(ncol(W)), t(W) %*% (Y - Xb))
    Wa <- W %*% a
    
    #print('Update done')
    
    
    l2Err <- (norm((Y - Xb - Wa), 'F')^2)/(m*n)
    oldObj <- currObj
    currObj <- norm((Y - Xb - Wa), 'F')^2 +lambda*norm(a,'F')^2
    
    dXb <- norm(Xb-XbOld,'F')/norm(Xb,'F')
    dWa <- norm(Wa-WaOld,'F')/norm(Wa,'F')
    if(ncol(W) != ncol(WOld)){
      dW <- Inf}
    else{
      dW <- norm(W-WOld,'F')/norm(W,'F')      
    }  
    dObj <- (oldObj-currObj)/oldObj
    
    if(iter >= maxIter || (!is.nan(max(dXb,dWa)) && max(dXb,dWa) < cEps)){
      converged = 1
    }
  }
  
  cY <- Y - Wa
  
  return(list(unadjY=RUVRand$unadjY, newY=cY,  UVcomp=Wa,
              W=W,alpha=a,X=X,b=b,k=k,lambda=lambda,opt=RUVRand$opt, 
              ctl=RUVRand$ctl))
}

### Internal Functions - metaX package

plotNorValue<-function(){
  
  fig_cv<- "cv.pdf"
  pdf(fig_cv,width = 6,height = 6)
  
  p<-ggplot(data=cvStatForEachBatch,aes(x=value,fill=CV,colour=CV))+
    facet_grid(batch~.)+
    geom_density(alpha = 0.5)+
    xlab(label = "CV")
  #print(p)
  
  p<-ggplot(data=cvStatForEachBatch,aes(x=value,fill=CV,colour=CV))+
    facet_grid(batch~.)+
    geom_density(alpha = 0.5)+
    xlim(0,2)+
    xlab(label = "CV")
  #print(p)
  
  dev.off()
}

myLoessFit = function(x,y,newX,span.vals=seq(0.1,1,by=0.05),log=TRUE,a=1){
  if(log==TRUE){
    y <- .glogfit(y,a=a)
  }
  #sp.obj <- smooth.spline(x,y,spar = tuneSpline(x,y,span.vals = span.vals))
  sp.obj <- smooth.spline(x,y,cv = TRUE)
  
  valuePredict=predict(sp.obj,newX)
  if(log==TRUE){
    valuePredict$y <- .glogfit(valuePredict$y,a = a,inverse = TRUE)
  }
  return(valuePredict$y)
}

tuneSpline = function(x,y,span.vals=seq(0.1,1,by=0.05)){
  mae <- numeric(length(span.vals))
  
  crossEva <- function(span,x,y) {
    
    fun.fit <- function(x,y,span) {smooth.spline(x = x,y =y ,spar = span)}
    fun.predict <- function(fit,x0) {predict(fit,x0)$y}
    y.cv <- .crossval(x,y,fun.fit,fun.predict,span=span,
                      ngroup = length(x))$cv.fit
    fltr <- !is.na(y.cv)
    return(mean(abs(y[fltr]-y.cv[fltr])))
  }
  mae <- sapply(span.vals,crossEva,x=x,y=y)
  span <- span.vals[which.min(mae)]
  return(span)
}

.runFit1=function(id,qcData,maxOrder){
  out <- tryCatch({
    
    dat <- data.frame(newOrder=1:maxOrder)
    
    piece <- qcData[qcData$ID_batch==id,]
    
    dat$valuePredict=myLoessFit(piece$order,piece$value,dat$newOrder)
    
    dat$ID <- piece$ID[1]
    dat$batch <- piece$batch[1]
    dat
    
  },
  error=function(e){
    #message("Please see the file: runFit_error.rda for related data!")
    #save(e,id,qcData,maxOrder,file="runFit_error.rda")
    #stop("error in runFit!")
    return(NULL)
  },
  warning=function(cond){
    #message("Please see the file: runFit_warning.rda for related data!")
    #save(cond,id,qcData,maxOrder,file="runFit_warning.rda")
    return(NULL)
  })
  return(out)
}

.runFit2=function(id,qcData,maxOrder,loessSpan){
  #out <- tryCatch({
  dat <- data.frame(newOrder=1:maxOrder)
  
  piece <- qcData[ qcData$ID_batch==id,]
  dat$valuePredict=predict(smooth.spline(piece$order,
                                         piece$value,
                                         spar = loessSpan),
                           dat$newOrder)$y
  dat$ID <- piece$ID[1]
  dat$batch <- piece$batch[1]
  dat
  # },
  #error=function(e){
  #message("Please see the file: runFit_error.rda for related data!")
  #save(e,id,qcData,maxOrder,file="runFit_error.rda")
  #  stop("error in runFit!")
  #  return(NULL)
  #},
  # warning=function(cond){
  #message("Please see the file: runFit_warning.rda for related data!")
  #save(cond,id,qcData,maxOrder,file="runFit_warning.rda")
  #   return(NULL)
  #})
  # return(out)
}

.glogfit=function (x, a = 1, inverse = FALSE) {
  if (inverse) {
    out <- 0.25 * exp(-x) * (4 * exp(2 * x) - (a * a))
  }else{
    out <- log10((x + sqrt(x^2 + a^2))/2)
  }
  return(out)
}

.imputation<-function(peaksData){
  #require(tidyverse);
  valueID="value"
  #x <- peaksData %>% dplyr::select(ID,sample,!!valueID) %>% 
  #  spread(sample,!!valueID)
  
  x_tmp<-dplyr::select(peaksData,ID,sample,!!valueID)
  x<-spread(x_tmp,sample,!!valueID)
  
  #x<-dcast(peaksData,ID~sample,value.var = valueID)
  row.names(x) <- x$ID
  x$ID <- NULL
  x[x<=0] <- NA
  
  #message("Missing value in total: ",sum(is.na(x)))
  if(any(is.na(peaksData$class))){
    qcValue <- peaksData[,valueID][is.na(peaksData$class)]
    #message("Missing value in QC sample: ",
    #        sum(qcValue<=0 | is.na(qcValue)))
    sValue <- peaksData[,valueID][!is.na(peaksData$class)]
    #message("Missing value in non-QC sample: ",
    #        sum(sValue<=0 | is.na(sValue)))
  }
  x <- .imputation_internal(x,method="knn",cpu=1)
  #message("Missing value in total after missing value inputation: ",
  #        sum(is.na(x)))
  
  ## maybe x has value which <=0
  #message("<=0 value in total after missing value inputation: ",
  #        sum(x<=0))
  x$ID <- row.names(x)
  y <- melt(x,id.vars = "ID",variable.name = "sample",value.name = "newValue")
  m <- plyr::join(peaksData,y,by=c("ID","sample"))
  m[,valueID] <- m$newValue
  m$newValue <- NULL
  #para@peaksData <- m
  return(m)
}

.imputation_internal<-function(x,method="knn",negValue = TRUE,cpu=1){
  if(cpu==0){
    cpu <- detectCores()
  }
  
  inputedData <- NULL
  colName <- names(x)
  rowName <- row.names(x)
  x <- as.matrix(t(x))
  ## An expression matrix with samples in the rows, features in the columns
  #save(x,file="x.rda")
  #message(date(),"\tThe ratio of missing value: ",
  #        sprintf("%.4f%%",100*sum(is.na(x))/length(x)))
  if(method == "bpca"){
    ## Numerical matrix with (or an object coercible to such) with samples 
    ## in rows and variables as columns. Also takes ExpressionSet in which 
    ## case the transposed expression matrix is used. Can also be a data 
    ## frame in which case all numberic variables are used to fit the PCA.
    ## Please note that this method is very time-consuming.
    mvd <- pca(x, nPcs = 3, method = "bpca")
    inputedData <- completeObs(mvd)    
  }else if(method == "svdImpute"){
    ## This method is very fast.
    mvd <- pca(x, nPcs = 3, method = "svdImpute")
    inputedData <- completeObs(mvd)
  }else if(method == "knn"){
    ## An expression matrix with genes in the rows, samples in the columns
    ## This method is very fast.
    #require(impute)
    mvd <- impute::impute.knn(t(x))
    inputedData <- t(mvd$data)
  }else if(method == "softImpute"){
    # https://cran.r-project.org/web/packages/softImpute/vignettes/softImpute.html
    # An m by n matrix with NAs.
    # TODO: tune the parameters, rank.max and lambda
    softfit <- softImpute(t(x))
    x_new <- complete(x,softfit)
    inputedData <- x_new
    
  }else if(method == "rf"){
    ## A data matrix with missing values. 
    ## The columns correspond to the variables and the rows to the 
    ## observations.
    ## Please note that this method is very time-consuming.
    
    if(cpu>1){
      cat("do missForest cpu=(",cpu,") ...\n")
      cl <- makeCluster(cpu)
      registerDoParallel(cl)
      # xmis: a data matrix with missing values. 
      # The columns correspond to the variables and the rows 
      # to the observations.
      mvd <- missForest(xmis = x,parallelize = "variables")
      #print(mvd$OOBerror)
      inputedData <- mvd$ximp
      stopCluster(cl)
      
    }else{
      cat("do missForest ...\n")
      mvd <- missForest(xmis = x)
      #print(mvd$OOBerror)
      inputedData <- mvd$ximp
    }
    
  }else if(method == "min"){
    ## min value / 2
    inputedData <- apply(x,1,function(y){
      y[is.na(y) | y<=0] <- min(y[y>0],na.rm = TRUE)/2.0
      y})    
    inputedData <- t(inputedData)
  }else if(method == "none"){
    cat("No missing value imputation!\n")
    inputedData <- x
  }else{
    stop("Please provide valid method for missing value inputation!")
  }
  
  if(method != "none"){
    
    if(negValue & method != "min"){
      #message("<=0: ",sum(inputedData<=0))
      x <- inputedData 
      inputedData <- apply(x,1,function(y){
        y[is.na(y) | y<=0] <- min(y[y>0],na.rm = TRUE)
        y})    
      inputedData <- t(inputedData)
      
    }
  }
  
  inputedData <- as.data.frame(t(inputedData))
  row.names(inputedData) <- rowName
  names(inputedData) <- colName
  return(inputedData)
}    

.cbindlist <- function(list) {
  n <- length(list)
  res <- matrix()
  for (i in seq(n)) {
    res <- cbind(res, list[[i]])
  }
  return(res)
}

.crossval<- function(x,y,theta.fit,theta.predict,...,ngroup=n){
  call <- match.call()
  x <- as.matrix(x)
  n <- length(y)
  ngroup <- trunc(ngroup)
  if( ngroup < 2){
    stop ("ngroup should be greater than or equal to 2")
  }
  if(ngroup > n){
    stop ("ngroup should be less than or equal to the number of observations")
  }
  
  if(ngroup==n) {groups <- 1:n; leave.out <- 1}
  if(ngroup<n){
    leave.out <- trunc(n/ngroup);
    o <- sample(1:n)
    groups <- vector("list",ngroup)
    for(j in 1:(ngroup-1)){
      jj <- (1+(j-1)*leave.out)
      groups[[j]] <- (o[jj:(jj+leave.out-1)])
    }
    groups[[ngroup]] <- o[(1+(ngroup-1)*leave.out):n]
  }
  u <- vector("list",ngroup)
  cv.fit <- rep(NA,n)
  for(j in 1:ngroup){
    u <- theta.fit(x[-groups[[j]], ],y[-groups[[j]]],...)
    cv.fit[groups[[j]]] <-  theta.predict(u,x[groups[[j]],])
    
  }
  
  if(leave.out==1) groups <- NULL
  return(list(cv.fit=cv.fit, 
              ngroup=ngroup, 
              leave.out=leave.out,
              groups=groups, 
              call=call)) 
}

# Define Internal Functions - EigenMS
makeLMFormula = function(eff, var_name='') {
  # eff - effects used in contrasts
  # var_name - for singe factor use var-name that is passed in as variable names, otherwise it has no colnmae
  #           only used for a single factor
  if(is.factor(eff))
  {
    ndims = 1
    cols1 = var_name # ftemp in EigenMS
  }
  else
  {
    ndims = dim(eff)[2] 
    cols1 = colnames(eff)
  }
  lhs = cols1[1]
  lm.fm = NULL
  # check if can have a list if only have 1 factor...
  
  params = paste('contrasts=list(', cols1[1], '=contr.sum', sep=)
  
  if (ndims > 1) { # removed ndims[2] here, now ndims holds only 1 dimention...
    for (ii in 2:length(cols1))
    {
      lhs = paste(lhs, "+", cols1[ii])  # bl="contr.sum",
      params = paste(params, ',', cols1[ii], '=contr.sum', sep='')
    }
  }
  params = paste(params,")") 
  lm.formula = as.formula(paste('~', lhs))
  lm.fm$lm.formula = lm.formula
  lm.fm$lm.params = params
  return(lm.fm)
}	

eig_norm1 = function(m, treatment, prot.info, write_to_file=''){
  # Identify significant eigentrends, allow the user to adjust the number (with causion! if desired)
  # before normalizing with eig_norm2
  # 
  # Input:
  #   m: An m x n (peptides x samples) matrix of expression data, log-transformed!
  #      peptide and protein identifiers come from the get.ProtInfo()
  #   treatment:  either a single factor indicating the treatment group of each sample i.e. [1 1 1 1 2 2 2 2...]
  #               or a frame of factors:  treatment= data.frame(cbind(data.frame(Group), data.frame(Time)) 
  #   prot.info: 2+ colum data frame, pepID, prID columns IN THAT ORDER. 
  #              IMPORTANT: pepIDs must be unique identifiers and will be used as Row Names 
  #              If normalizing non-proteomics data, create a column such as: paste('ID_',seq(1:num_rows), sep='')
  #              Same can be dome for ProtIDs, these are not used for normalization but are kept for future analyses 
  #   write_to_file='' - if a string is passed in, 'complete' peptides (peptides with NO missing observations)
  #              will be written to that file name
  #                    
  # Output: list of:
  #   m, treatment, prot.info, grp - initial parameters returned for futre reference 
  #   my.svd - matrices produced by SVD 
  #   pres - matrix of peptides that can be normalized, i.e. have enough observations for ANOVA, 
  #   n.treatment - number of factors passed in
  #   n.u.treatment - number of unique treatment facotr combinations, eg: 
  #                   Factor A: a a a a c c c c
  #                   Factor B: 1 1 2 2 1 1 2 2
  #                   then:  n.treatment = 2; n.u.treatment = 4
  #   h.c - bias trends 
  #   present - names/IDs of peptides on pres
  #   complete - complete peptides, no missing values, these were used to compute SVD
  #   toplot1 - trends automatically produced, if one wanted to plot at later time. 
  #   Tk - scores for each bias trend 
  #   ncompl - number of complete peptides with no missing observations
  #print("Data dimentions: ")  
  #print(dim(m))
  # check if treatment is a 'factor' vs data.frame', i.e. single vs multiple factors
  if(class(treatment) == "factor") { # TRUE if one factor
    n.treatment = 1 # length(treatment)
    n.u.treatment = length(unique(treatment))[1]
  } else { # data.frame
    n.treatment = dim(treatment)[2]
    n.u.treatment = dim(unique(treatment))[1] # all possible tretment combinations
  }
  # convert m to a matrix from data.frame
  m = as.matrix(m) # no loss of information
  
  # filter out min.missing, here just counting missing values
  # if 1+ treatment completely missing, cannot do ANOVA, thus cannot preserve grp diff.
  # IMPORTANT: we create a composite grp = number of unique combinations of all groups, only for 
  # 'nested' groups for single layer group is left as it is 
  grpFactors = treatment # temporary var, leftover from old times...
  
  nGrpFactors = n.treatment # length(colnames(treatment)) # not good: dim(grpFactors)
  if(nGrpFactors > 1) { # got nested factors
    ugrps = unique(grpFactors)
    udims = dim(ugrps)
    grp = NULL
    for(ii in 1:udims[1]) {
      pos = grpFactors[,1] == ugrps[ii,1] # set to initial value
      for(jj in 2:udims[2]) { 
        pos = pos & grpFactors[,jj] == ugrps[ii,jj]
      }
      grp[pos] = rep(ii, sum(pos))
    }
    grp = as.factor(grp)
  } else {
    grp = treatment
  }
  nobs = array(NA, c(nrow(m), length(unique(grp)))) # noobs = number of observations 
  
  #print('Treatmenet groups:')
  #print(grp)
  
  for(ii in 1:nrow(m)) {
    #print(ii)
    for(jj in 1:length(unique(grp))) {
      #print(jj)
      nobs[ii,jj] = sum(!is.na(m[ii, grp==unique(grp)[jj]])) # total number of groups num(g1) * num(g2) * ...
    } 
  } 
  # now 'remove' peptides with missing groups
  present.min = apply(nobs, 1, min) # number present in each group
  ii = present.min == 0   # 1+ obs present in ALL of the groups
  nmiss = sum(present.min == 0) # not used, one value of how many peptides have 1+ grp missing completely
  pmiss = rbind(m[ii,]) # these have 1+ grp missing !!!!
  # rownames must be UNIQUE, if have possible duplicates: use 'ii' ?
  rownames(pmiss) = prot.info[ii,1]  # set rownames, 
  
  # create matrix for peptides with enough observations for ANOVA
  # 'present' are names of the peptides (pepID) and 'pres' are abundances
  # NOTE: ! negates the proteins, so we get ones that have 1+ obs in each group 
  present = prot.info[which(!prot.info[,1] %in% rownames(pmiss)), ] # rownames OK
  # pres = m[which(!rownames(m) %in% rownames(pmiss)), ]
  pres = m[which(!prot.info[,1] %in% rownames(pmiss)), ] # is this OK?
  rownames(pres) = prot.info[which(!prot.info[,1] %in% rownames(pmiss)),1]
  
  #print('Selecting complete peptides')
  # Should issue an error message if we have NO complete peptides.
  # select only 'complete' peptides, no missing values
  nobs = array(NA, nrow(pres)) # reassign noobs to dims of 'present' 
  numiter = nrow(pres)
  for (ii in 1:numiter) {
    # if(ii %% 100 == 0) { print(ii) }
    nobs[ii] = sum(!is.na(pres[ii,]))
  }
  
  iii = nobs == ncol(pres)
  complete = rbind(pres[iii,])
  
  #  write out a file of complete peptides if file name is passed in
  #if(write_to_file != '') {
  #  write.table(complete, file = write_to_file, append = FALSE,
  #              quote = FALSE, sep = "\t",
  #              eol = "\n", na = "NaN", dec = ".", row.names = TRUE,
  #              col.names = TRUE, qmethod = c("escape", "double"))
  #}
  
  # compute bias with 'complete' matrix and residuals from 'present' 
  # calculate eigenpeptides for 'complete' data only
  # if have only 1 group, we do not need to preserve group differernces, everything is the same group, ex: QC samples
  # contrasts will fail if have only 1 group, thus have else
  if(n.u.treatment > 1) { 
    #print('Got 2+ treatment grps')
    # check to see if we have multiple factors
    grpdim = dim(treatment)
    
    lm.fm = makeLMFormula(treatment, 'TREAT') # using general function that can accomodate for 1+ number of factors
    TREAT = treatment
    TREAT = data.frame(treatment) # temp var to work if we got only 1 treatment vector.
    if(class(treatment) == "factor") {
      colnames(TREAT) = "TREAT"
    } else {
      colnames(TREAT) = colnames(treatment)
    }     
    attach(TREAT)
    
    mod.c = model.matrix(lm.fm$lm.formula, data=TREAT, eval(parse(text=lm.fm$lm.params))) 
    Y.c = as.matrix(complete)
    options(warn = -1)
    
    # use lm() to get residuals
    formula1 = paste('t(Y.c)~', as.character(lm.fm$lm.formula)[2], sep = '')
    TREAT = treatment
    fit_lmAll = lm(eval(parse(text=formula1)))
    R.c = residuals(fit_lmAll)  # Oct 2 messing with residuals...
  } else {  # 1 group only, set residuals to original matrix
    #print('Got 1 treatment grp')
    mod.c = as.numeric(t(treatment))
    R.c = t(as.matrix(complete))  # needs to be transposed to match the matrix returned from lm
    TREAT = treatment
  }
  
  #print('Computing SVD, estimating Eigentrends...') # let user know what is going on
  # residuals are centered around 0, here center samples not peptides/metabolites
  # centering is basic normalization
  
  R.c_center = scale(R.c, center = TRUE, scale = FALSE)  # t(scale(t(R.c), center = TRUE, scale = FALSE))
  my.svd = svd(R.c_center)  # can use wrapper below to chek if SVD has a problem...
  temp = my.svd$u
  my.svd$u = my.svd$v
  my.svd$v = temp
  
  #identify number of eigenvalues that account for a significant amount of residual variation
  numcompletepep = dim(complete)[1] # save to return to the user as part of the return list  
  # this is important info for publications
  # tell users how many peptides/metabolites the trends are based on
  # can also be determined by doing dim(return_value_fromEIg_norm1$pres)
  
  #print(paste('Number of treatments: ', n.u.treatment))
  h.c = sva.id(complete, treatment, n.u.treatment, lm.fm=lm.fm,seed=1234)$n.sv
  #print(paste("Number of significant eigenpeptides/trends", h.c) )
  
  # show RAW trends
  # center each peptide around zero (subtract its mean across samples)
  complete_center = scale(t(complete), center = TRUE, scale = FALSE)
  #print('Preparing to plot...')
  
  n.u.treatment
  toplot1 = svd(complete_center) # scales above
  temp = toplot1$u
  toplot1$u = toplot1$v
  toplot1$v = temp
  
  #par(mfcol=c(3,2))
  #par(mar = c(2,2,2,2))
  
  #plot.eigentrends(toplot1, "Raw Data")
  #plot.eigentrends(my.svd, "Residual Data")
  
  d = my.svd$d;  ss = d^2;
  Tk = signif(ss/sum(ss)* 100, 2)
  
  retval = list(m=m, treatment=treatment, my.svd=my.svd,
                pres=pres, n.treatment=n.treatment, n.u.treatment=n.u.treatment,
                h.c=h.c, present=present, prot.info=prot.info,
                complete=complete, toplot1=toplot1, Tk=Tk, ncompl=numcompletepep,
                grp=grp) 
  return(retval)
}

eig_norm2 = function(rv) { 
  # UNPUT:
  #   rv - return value from the eig_norm1
  #   if user wants to change the number of bias trends that will be eliminated h.c in rv should 
  #   be updates to the desired number
  # 
  # OUTPUT: 
  #   normalized - matrix of normalized abundances with 2 columns of protein and peptdie names
  #   norm_m - matrix of normalized abundances, no extra columns  
  #   eigentrends - found in raw data, bias trendsup to h.c
  #   rescrange - rescaling range for the addition of the while noise to avoid overfitting 
  #   norm.svd - trends in normalized data, if one wanted to plot at later time. 
  #   exPeps - excluded peptides - excluded due to exception in fitting a linear model
  
  m = rv$pres # yuliya: use pres matrix, as we cannot deal with m anyways, need to narrow it down to 'complete' peptides
  treatment = rv$treatment
  my.svd = rv$my.svd
  pres = rv$pres
  n.treatment = rv$n.treatment
  n.u.treatment = rv$n.u.treatment 
  numFact = dim(rv$treatment)[2]
  #print(paste('Unique number of treatment combinations:', n.u.treatment) )
  h.c = rv$h.c
  present = rv$present
  toplot1 = rv$toplot1
  # vector of indicators of peptides that threw exeptions 
  exPeps = vector(mode = "numeric", length = nrow(pres))
  
  #print("Normalizing...")
  treatment = data.frame(treatment) # does this need to be done?
  if(n.u.treatment > 1) {
    lm.fm = makeLMFormula(treatment, 'ftemp')
    mtmp = model.matrix(lm.fm$lm.formula, data=treatment, eval(parse(text=lm.fm$lm.params)))  #contrasts=list(bl="contr.sum", it="contr.sum",Pi="contr.sum", tp="contr.sum"))
  } else {  # have 1 treatment group
    mtmp = treatment # as.numeric(t(treatment)) 
  }
  # above needed to know how many values will get back for some matrices
  # create some variables:
  betahat = matrix(NA,nrow=dim(mtmp)[2],ncol=nrow(pres)) 
  newR = array(NA, c(nrow(pres), ncol(pres))) #, n.treatment))
  norm_m = array(NA, c(nrow(pres), ncol(pres))) # , n.treatment))
  numsamp = dim(pres)[2]
  numpep = dim(pres)[1]
  betahat_n = matrix(NA,nrow=dim(mtmp)[2],ncol=nrow(pres))
  rm(mtmp) 
  
  V0 = my.svd$v[,1:h.c,drop=F]   # residual eigenpeptides
  
  if(n.u.treatment == 1) { # got 1 treatment group
    for (ii in 1:nrow(pres)) {
      #if(ii%%250 == 0) { print(paste('Processing peptide ',ii))  }
      pep = pres[ii, ] 
      pos = !is.na(pep)
      peptemp = as.matrix(pep[pos]) # take only the observed values
      resm = rep(NA, numsamp) 
      resm[pos] = as.numeric(pep[pos])
      bias = array(NA, numsamp)
      bias[pos] = resm[pos] %*% V0[pos,] %*% t(V0[pos,])
      norm_m[ii, ] = as.numeric(pep - bias)
    }
    
  } else { # got 2+ treatment groups
    for (ii in 1:nrow(pres)) {
      if(ii %% 100 == 0) { #print(paste('Processing peptide ',ii))  
      }
      pep = pres[ii, ] 
      pos = !is.na(pep)
      peptemp = as.matrix(pep[pos]) # take only the observed values, may not be needed in R? but this works
      ftemp = treatment[pos,]
      ftemp = data.frame(ftemp)
      #### use try, not entirely sure if need for modt, need it for solve lm?!
      options(warn = -1)
      lm.fm = makeLMFormula(ftemp, 'ftemp') # using general function that can accomodate for 1+ number of factors
      modt = try(model.matrix(lm.fm$lm.formula, data=ftemp, eval(parse(text=lm.fm$lm.params))), silent=TRUE)
      options(warn = 0)
      
      if(!inherits(modt, "try-error")) { # do nothing if could not make model matrix
        options(warn = -1)
        # if we are able to solve this, we are able to estimate bias  
        bhat =  try(solve(t(modt) %*% modt) %*% t(modt) %*% peptemp)
        options(warn = 0)
        if(!inherits(bhat, "try-error")) {
          betahat[,ii] = bhat
          ceffects = modt %*% bhat  # these are the group effects, from estimated coefficients betahat
          
          resm = rep(NA, numsamp) # really a vector only, not m 
          resm[pos] = as.numeric(pep[pos] - ceffects)
          bias = array(NA, numsamp)
          bias[pos] = resm[pos] %*% V0[pos,] %*% t(V0[pos,])
          norm_m[ii, ] = as.numeric(pep - bias)
          
          # yuliya:  but newR should be computed on Normalized data
          resm_n = rep(NA, numsamp)
          bhat_n =  solve(t(modt) %*% modt) %*% t(modt) %*% norm_m[ii, pos]
          betahat_n[,ii] = bhat_n
          ceffects_n = modt %*% bhat_n
          resm_n[pos] = norm_m[ii,pos] - ceffects
          newR[ii, ] = resm_n
        } else {
          #print(paste('got exception 2 at peptide:', ii, 'should not get here...')) 
          exPeps[ii] = 2 # should not get 2 here ever...
        }
      } else {
        #print(paste('got exception at peptide:', ii)) 
        exPeps[ii] = 1 # keep track of peptides that threw exeptions, check why...
      }
    }
  } # end else - got 2+ treatment groups
  
  #####################################################################################
  # rescaling has been eliminated form the code after discussion that bias 
  # adds variation and we remove it, so no need to rescale after as we removed what was introduced
  y_rescaled = norm_m # for 1 group normalization only, we do not rescale
  # add column names to y-rescaled, now X1, X2,...
  colnames(y_rescaled) = colnames(pres) # these have same number of cols
  rownames(y_rescaled) = rownames(pres) 
  y_resc = data.frame(present, y_rescaled)  
  rownames(y_resc) = rownames(pres)  # rownames(rv$normalized)
  final = y_resc # row names are assumed to be UNIQUE, peptide IDs are unique
  
  # rows with all observations present
  complete_all = y_rescaled[rowSums(is.na(y_rescaled))==0,,drop=F]
  
  #  x11() # make R open new figure window
  #par(mfcol=c(3,2))
  #par(mar = c(2,2,2,2))
  # center each peptide around zero (subtract its mean across samples)
  # note: we are not changing matrix itself, only centerig what we pass to svd
  complete_all_center = t(scale(t(complete_all), center = TRUE, scale = FALSE))
  toplot3 = svd(complete_all_center)
  #plot.eigentrends(toplot1, "Raw Data")
  #plot.eigentrends(toplot3, "Normalized Data")
  
  #print("Done with normalization!!!")
  colnames(V0) =  paste("Trend", 1:ncol(V0), sep="_")
  
  maxrange = NULL # no rescaling # data.matrix(maxrange)
  return(list(normalized=final, norm_m=y_rescaled, eigentrends=V0, rescrange=maxrange, 
              norm.svd=toplot3, exPeps=exPeps)) 
} # end function eig_norm2

sva.id = function(dat, treatment, n.u.treatment, lm.fm, B=500, sv.sig=0.05, seed=NULL) {
  # Executes Surrogate Variable Analysis
  # Input:
  #   dat: A m peptides/genes by n samples matrix of expression data
  #   mod: A model matrix for the terms included in the analysis 
  #   n.u.treatment - 0 or 1, if we are normalizing data with NO groups or some groups, QC vs samples
  #   B: The number of null iterations to perform
  #   sv.sig: The significance cutoff for the surrogate variables
  #   seed: A seed value for reproducible results
  # Output
  #    n.sv: Number of significant surrogate variables. 
  #    id: An indicator of the significant surrogate variables
  #    B: number of permutation to do
  #    sv.sig: significance level for surrogate variables
  #print("Number of complete peptides (and samples) used in SVD")
  # print(dim(dat))
  
  
  if(!is.null(seed))  { set.seed(seed) }
  warn = NULL
  n = ncol(dat)
  m = nrow(dat)
  
  # ncomp = length(as.numeric(n.u.treatment))
  ncomp = n.u.treatment # JULY 2013: as.numeric(n.u.treatment)
  #print(paste("Number of treatment groups (in svd.id): ", ncomp))
  # should be true for either case and can be used later
  
  if(ncomp > 1) { #   
    formula1 = paste('t(dat)~', as.character(lm.fm$lm.formula)[2], sep = '')
    fit_lmAll = lm(eval(parse(text=formula1)))
    res = t(residuals(fit_lmAll))
  } else {
    res = dat
  }
  # centering was not done before...
  # center each peptide around zero (subtract its mean across samples)
  # note: we are not changing matrix itself, only centerig what we pass to svd
  res_center = t(scale(t(res), center = TRUE, scale = FALSE))
  
  uu = svd(t(res_center)) # NEED a WRAPPER for t(). the diag is min(n, m)
  temp = uu$u
  uu$u = uu$v
  uu$v = temp
  
  
  # yuliya: Sept 2014: can I get around without using H?? 
  #  ndf = min(n, m) - ceiling(sum(diag(H)))  
  #  dstat = uu$d[1:ndf]^2/sum(uu$d[1:ndf]^2)
  #  dstat0 = matrix(0,nrow=B,ncol=ndf)
  #  s0 = diag(uu$d) # no need for diag here, should investigate why this is a vector already...
  s0 = uu$d
  s0 = s0^2
  dstat = s0/sum(s0)  # this did not have 'diag' in it in Tom's code...
  ndf = length(dstat) # sticking to Tom's variable name
  # print(paste('length(dstat) = ndf = ', ndf))
  dstat0 = matrix(0,nrow=B,ncol=ndf) # num samples (?)
  
  #print("Starting Bootstrap.....")
  # this is the Bootstrap procedure that determines the number of significant eigertrends... 
  for(ii in 1:B){
    #if(ii %% 50 == 0) { print(paste('Iteration ', ii)) }
    res0 = t(apply(res, 1, sample, replace=FALSE)) # regression
    # yuliya: not sure if this is needed at all
    # not needed for 1 group normalizaiton
    ##### res0 = res0 - t(H %*% t(res0))
    # yuliya: Sept 3, 2014: REMOVED above line. Do not think this needs to be done.. 
    # center each peptide around zero (subtract its mean across samples)
    # note: we are not changing matrix itself, only centerig what we pass to svd
    res0_center = t(scale(t(res0), center = TRUE, scale = FALSE))
    uu0 = svd(res0_center)
    temp = uu0$u  # why did tom do this??
    uu0$u = uu0$v
    uu0$v = temp
    
    ss0 = uu0$d  # no need for diag.... 
    ss0 = ss0^2
    dstat0[ii,] = ss0 / sum(ss0) # Tk0 in Matlab
  }
  
  # yuliya: check p-values here, Tom had mean value...
  psv = rep(1,n)
  for(ii in 1:ndf){
    # psv[ii] = mean(dstat0[,ii] >= dstat[ii])
    # should this be compared to a MEAN?  Should this be dstat0[ii,] ?
    posGreater = dstat0[,ii] > dstat[ii]
    psv[ii] = sum(posGreater) / B
  }
  
  # p-values for peptides have to be in monotonically increasing order, 
  # set equal to previous one if not the case
  for(ii in 2:ndf){
    if(psv[(ii-1)] > psv[ii]) {
      # psv[ii] = max(psv[(ii-1)],psv[ii]) 
      psv[ii] = psv[(ii-1)] 
    }
  }
  nsv = sum(psv <= sv.sig)
  # tom - this should be at least 1
  # nsv = min(sum(psv <= sv.sig), 1, na.rm=T)
  return(list(n.sv = nsv,p.sv=psv))
}

plot.eigentrends = function(svdr, title1){
  v = svdr$v
  d = svdr$d
  ss = d^2
  Tk = signif(ss/sum(ss)* 100, 2)
  
  titles = paste("Trend ", 1:3, " (", Tk[1:3], "%)", sep = "")
  do.text = function(j) mtext(titles[j], cex=0.7, padj=-0.7, adj=1)
  range.y = range(as.numeric(v[,1:3]), na.rm=T)
  
  toplot1_1 = as.numeric(v[,1])
  toplot1_2 = as.numeric(v[,2])
  toplot1_3 = as.numeric(v[,3])
  
  plot(c(1:length(toplot1_1)), toplot1_1, type='b', ann=F, ylim=range.y)
  do.text(1)
  abline(h=0, lty=3)
  title(title1, cex.main = 1.2, font.main= 1, col.main= "purple", ylab=NULL)
  plot(c(1:length(toplot1_2)), toplot1_2, type='b', ann=F, ylim=range.y)
  do.text(2)
  abline(h=0, lty=3)
  plot(c(1:length(toplot1_3)), toplot1_3, type='b', ann=F, ylim=range.y)
  do.text(3)
  abline(h=0, lty=3)
  return(Tk)
}

# Define Internal Functions - CCMN
standardsFit <- function(object, factors, ncomp=NULL, lg=TRUE, fitfunc=lm, standards) {
  X <- factors
  
  if(lg)
    lsta <- log10(t(object[standards,]))
  else
    lsta <- t(object[standards,])
  
  clsta <- scale(lsta)
  means <- attr(clsta, "scaled:center")
  sds <- attr(clsta, "scaled:scale")
  pfit <- fitfunc(clsta~-1+I(X))
  zbzhate <- cbind(resid(pfit))
  np <- max(1, min(nrow(zbzhate) - 1, ncol(zbzhate) - 1, ncomp))
  #hp <- library(help="pcaMethods")$info[[1]]
  #ver <- gsub("Version:", "", hp[grep("Version:", hp)])
  #if(compareVersion(ver, "1.26.0") == 1)
  #  pc <- pca(zbzhate, nPcs=np, verbose=FALSE)
  #else
  withCallingHandlers(pc <- pcaMethods::pca(zbzhate, nPcs=np, method="nipals", verbose=FALSE),
                      warning=pcaMuffle
  )
  
  r2 <- NULL -> q2
  best <- min(np, ncomp)
  #if(is.null(ncomp) ) {
  #  withCallingHandlers(q2 <- Q2(pc, zbzhate, nruncv=1), warning=pcaMuffle)
  #  r2 <- pc@R2
  #  best <- which.max(q2)
  #}
  
  list(fit=list(fit=pfit,pc=pc), ncomp=best, means=means, sds=sds, q2=q2, r2=r2)
}

standardsPred <- function(model, newdata, factors, lg=TRUE, standards) {
  X <- factors
  object <- newdata
  if(lg) {
    lsta <- log10(t(object[standards,]))
  } else {
    lsta <- t(object[standards,])
  }
  
  slsta <- scale(lsta, center=model$means, scale=model$sds)
  
  ## correct for G
  cslstaE <- slsta - predict(model$fit$fit, data.frame(I(X)))
  
  ## correct for E, get Tz
  predict(model$fit$pc, cslstaE, pcs=model$ncomp)$scores
}

pcaMuffle <- function(w) {
  if(any(grepl("Precision for components", w),
         grepl("Validation incomplete", w)))
    invokeRestart( "muffleWarning" )}

# Define Internal Functions - RUV-2
naiveRandRUV <- function(Y, cIdx, nu.coeff=1e-3, k=min(nrow(Y), length(cIdx)), tol=1e-6){
  
  ## W is the square root of the empirical covariance on the control
  ## genes.
  
  svdYc <- svd(Y[, cIdx, drop=FALSE])
  k.max <- sum(svdYc$d^2/svdYc$d[1]^2 > tol)
  if(k > k.max){
    warning(sprintf('k larger than the rank of Y[, cIdx]. Using k=%d instead', k.max))
    k <- k.max
  }
  W <- svdYc$u[, 1:k, drop=FALSE] %*% diag(svdYc$d[1:k], nrow=k)
  
  ## Regularization heuristic: nu is a fraction of the largest eigenvalue of WW'
  
  nu <- nu.coeff*svdYc$d[1]^2 #/ (length(cIdx)+1)
  
  ## Naive correction: ridge regression of Y against W
  
  nY <- Y - W %*% solve(t(W)%*%W + nu*diag(k), t(W) %*% Y)
  
  return(nY)
}

# Define Internal Functions - 'crch' Package
crch <- function(formula, data, subset, na.action, weights, offset, 
                 link.scale = c("log", "identity", "quadratic"), 
                 dist = c("gaussian", "logistic", "student"), df = NULL,
                 left = -Inf, right = Inf, truncated = FALSE, type = c("ml", "crps"), 
                 control = crch.control(...), model = TRUE, x = FALSE, y = FALSE, ...){
  ## call
  cl <- match.call()
  if(missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  
  ## formula
  oformula <- as.formula(formula)
  formula <- as.Formula(formula)
  if(length(formula)[2L] < 2L) {
    formula <- as.Formula(formula(formula), ~ 1)
    simple_formula <- TRUE
  } else {
    if(length(formula)[2L] > 2L) {
      formula <- Formula(formula(formula, rhs = 1:2))
      warning("formula must not have more than two RHS parts")
    }
    simple_formula <- FALSE
  }
  mf$formula <- formula
  ## evaluate model.frame
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  ## extract terms, model matrix, response
  mt <- terms(formula, data = data, dot = control$dot)
  mtX <- terms(formula, data = data, rhs = 1L, dot = control$dot)
  mtZ <- delete.response(terms(formula, data = data, rhs = 2L, dot = control$dot))
  Y <- model.response(mf, "numeric")
  X <- model.matrix(mtX, mf)
  Z <- model.matrix(mtZ, mf)
  
  ## obtain correct subset of predvars/dataClasses to terms
  .add_predvars_and_dataClasses <- function(terms, model.frame) {
    ## original terms
    rval <- terms
    ## terms from model.frame
    nval <- if(inherits(model.frame, "terms")) model.frame else terms(model.frame, dot = control$dot)
    
    ## associated variable labels
    ovar <- sapply(as.list(attr(rval, "variables")), deparse)[-1]
    nvar <- sapply(as.list(attr(nval, "variables")), deparse)[-1]
    if(!all(ovar %in% nvar)) stop(
      paste("The following terms variables are not part of the model.frame:",
            paste(ovar[!(ovar %in% nvar)], collapse = ", ")))
    ix <- match(ovar, nvar)
    
    ## subset predvars
    if(!is.null(attr(rval, "predvars"))) 
      warning("terms already had 'predvars' attribute, now replaced")
    attr(rval, "predvars") <- attr(nval, "predvars")[1L + c(0L, ix)]
    
    ## subset dataClasses
    if(!is.null(attr(rval, "dataClasses"))) 
      warning("terms already had 'dataClasses' attribute, now replaced")
    attr(rval, "dataClasses") <- attr(nval, "dataClasses")[ix]
    
    return(rval)
  }
  mt  <- .add_predvars_and_dataClasses(mt,  mf)
  mtX <- .add_predvars_and_dataClasses(mtX, mf)
  mtZ <- .add_predvars_and_dataClasses(mtZ, mf)
  
  ## link
  if(is.character(link.scale)) link.scale <- match.arg(link.scale)
  
  
  ## distribution
  if(is.character(dist)) dist <- match.arg(dist)
  
  ## type
  if(is.character(type)) type <- match.arg(type)
  
  ## sanity checks
  if(length(Y) < 1) stop("empty model")
  if(identical(dist, "student")) {
    if(!is.null(df) && df <= 0) stop("'df' must be positive")
    if(!is.null(df) && !is.finite(df)) dist <- "gaussian"
  }
  
  ## convenience variables
  n <- length(Y)
  
  
  ## weights
  weights <- model.weights(mf)
  if(is.null(weights)) weights <- 1
  if(length(weights) == 1) weights <- rep.int(weights, n)
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)
  
  ## offsets
  expand_offset <- function(offset) {
    if(is.null(offset)) offset <- 0
    if(length(offset) == 1) offset <- rep.int(offset, n)
    as.vector(offset)
  }
  ## in location part of formula
  offsetX <- expand_offset(model.offset(model.part(formula, data = mf, rhs = 1L, terms = TRUE)))
  ## in scale part of formula
  offsetZ <- expand_offset(model.offset(model.part(formula, data = mf, rhs = 2L, terms = TRUE)))
  ## in offset argument (used for location)
  if(!is.null(cl$offset)) offsetX <- offsetX + expand_offset(mf[, "(offset)"])
  ## collect
  offset <- list(location = offsetX, scale = offsetZ)
  
  
  ## call the actual workhorse: crch.fit() or crch.boost.fit()
  fit <- control$fit
  control$fit <- NULL
  rval <- do.call(fit, list(x = X, y = Y, z = Z, left = left, right = right, 
                            link.scale = link.scale, dist = dist, df = df, weights = weights, 
                            offset = offset, control = control, truncated = truncated, type = type))
  
  
  ## further model information
  rval$call <- if(length(control$call)) control$call else cl
  rval$formula <- oformula
  rval$terms <- list(location = mtX, scale = mtZ, full = mt)
  rval$levels <- list(location = .getXlevels(mtX, mf), 
                      scale = .getXlevels(mtZ, mf), full = .getXlevels(mt, mf))
  rval$contrasts <- list(location = attr(X, "contrasts"), scale = attr(Z, "contrasts"))
  if(model) rval$model <- mf
  if(y) rval$y <- Y
  if(x) rval$x <- list(location = X, scale = Z)
  
  return(rval)
}

crch.control <- function(method = "BFGS", maxit = NULL, 
                         hessian = NULL, trace = FALSE, start = NULL, dot = "separate",
                         lower = -Inf, upper = Inf, ...){
  if(method == "boosting") {
    if(is.null(maxit)) maxit <- 100
    rval <- crch.boost(dot = dot, start = start, maxit = maxit, ...)
  } else {
    if(is.null(maxit)) maxit <- 5000
    rval <- list(method = method, maxit = maxit, hessian = hessian, trace = trace, 
                 start = start, dot = dot,lower=lower,upper=upper, fit = "crch.fit")
    rval <- c(rval, list(...))
    if(!is.null(rval$fnscale)) warning("fnscale must not be modified")
    rval$fnscale <- 1
    if(is.null(rval$reltol)) rval$reltol <- .Machine$double.eps^(1/1.2)
  }
  rval
}

crch.fit <- function(x, z, y, left, right, truncated = FALSE, 
                     dist = "gaussian", df = NULL, link.scale = "log", type = "ml",
                     weights = NULL, offset = NULL, control = .crch.control()) {
  
  ## response and regressor matrix
  n <- NROW(x)  
  k <- NCOL(x)
  if(is.null(weights)) weights <- rep.int(1, n)
  nobs <- sum(weights > 0)
  dfest <- identical(dist, "student") & is.null(df)
  if(is.null(offset)) offset <- rep.int(0, n)
  if(!is.list(offset)) offset <- list(location = offset, scale = rep.int(0, n))
  if(is.null(z)) {
    q <- 1L
    z <- matrix(1, ncol = q, nrow = n)
    colnames(z) <- "(Intercept)"
    rownames(z) <- rownames(x)
  } else {
    q <- NCOL(z)
    if(q < 1L) stop("scale regression needs to have at least one parameter")
  }
  
  ## control parameters
  ocontrol <- control
  method   <- control$method
  hessian  <- control$hessian
  start    <- control$start
  lower    <- control$lower
  upper    <- control$upper
  control$method <- control$hessian <- control$start <- control$lower <- control$upper <- NULL
  
  if(is.character(dist)){
      ## distribution functions for maximum likelihood
      if(truncated) {
        ddist2 <- switch(dist, 
                         "student"  = dtt, "gaussian" = dtnorm, "logistic" = dtlogis)
        sdist2 <- switch(dist, 
                         "student"  = stt, "gaussian" = stnorm, "logistic" = stlogis)
        hdist2 <- switch(dist, 
                         "student"  = htt, "gaussian" = htnorm, "logistic" = htlogis)
      } else {
        ddist2 <- switch(dist, 
                         "student"  = dct, "gaussian" = dcnorm, "logistic" = dclogis)
        sdist2 <- switch(dist, 
                         "student"  = sct, "gaussian" = scnorm, "logistic" = sclogis)
        hdist2 <- switch(dist, 
                         "student"  = hct, "gaussian" = hcnorm, "logistic" = hclogis)
      }
      ddist <- if(dist == "student") ddist2 else function(..., df) ddist2(...)
      sdist <- if(dist == "student") sdist2 else function(..., df) sdist2(...)
      hdist <- if(dist == "student") hdist2 else function(..., df) hdist2(...)
    
  } else { 
    ## for user defined distribution (requires list with ddist, sdist (optional)
    ## and hdist (optional), ddist, sdist, and hdist must be functions with
    ## arguments x, location, sd, df, left, right, and log)
    ddist <- dist$ddist
    sdist <- if(is.null(dist$sdist)) NULL else  dist$sdist
    if(is.null(dist$hdist)) {
      if(!is.null(hessian))
        if(hessian == FALSE) warning("no analytic hessian available. Hessian is set to TRUE and numerical Hessian from optim is employed")
      hessian <- TRUE     
    } else hdist <- dist$hdist 
  }
  
  ## analytic or numeric Hessian
  if(is.null(hessian)) {
    hessian <- dfest
    returnvcov <- TRUE  # vcov is not computed when hessian == FALSE
  } else {
    returnvcov <- hessian
  } 
  
  ## link
  if(is.character(link.scale)) {
    linkstr <- link.scale
    if(linkstr != "quadratic") {
      linkobj <- make.link(linkstr)
      linkobj$dmu.deta <- switch(linkstr, 
                                 "identity" = function(eta) rep.int(0, length(eta)), 
                                 "log" = function(eta) pmax(exp(eta), .Machine$double.eps))
    } else {
      linkobj <- structure(list(
        linkfun = function(mu) mu^2,
        linkinv = function(eta) sqrt(eta),
        mu.eta = function(eta) 1/2/sqrt(eta),
        dmu.deta = function(eta) -1/4/sqrt(eta^3),
        valideta = function(eta) TRUE,
        name = "quadratic"
      ), class = "link-glm")
    }
  } else {
    linkobj <- link.scale
    linkstr <- link.scale$name
    if(is.null(linkobj$dmu.deta) & !hessian) {
      warning("link.scale needs to provide dmu.deta component for analytical Hessian. Numerical Hessian is employed.")
      hessian <- TRUE
    }
  }
  linkfun <- linkobj$linkfun
  linkinv <- linkobj$linkinv
  mu.eta <- linkobj$mu.eta
  dmu.deta <- linkobj$dmu.deta
  
  ## starting values
  if(is.null(start)) {
    auxreg <- lm.wfit(x, y, w = weights, offset = offset[[1L]])
    beta <- auxreg$coefficients
    gamma <- c(linkfun(sqrt(sum(weights * auxreg$residuals^2)/
                              auxreg$df.residual)), rep(0, ncol(z) - 1))
    start <- if(dfest) c(beta, gamma, log10(10)) else c(beta, gamma)
  }
  if(is.list(start)) start <- do.call("c", start) 
  if(length(start) > k + q + dfest) {
    warning(paste("too many entries in start! only first", k + q + dfest, "entries are considered"))
    start <- start[1: (k + q + dfest)]
  }
  ## various fitted quantities (parameters, linear predictors, etc.)
  fitfun <- function(par) {
    beta <- par[seq.int(length.out = k)]
    gamma <- par[seq.int(length.out = q) + k]
    delta <- if(dfest) tail(par, 1) else NULL
    mu <- drop(x %*% beta) + offset[[1L]]
    zgamma <- drop(z %*% gamma) + offset[[2L]]
    sigma <- linkinv(zgamma)
    df <- if(dfest) exp(delta) else df
    list(
      beta = beta,
      gamma = gamma,
      delta = delta,
      mu = mu,
      zgamma = zgamma,
      sigma = sigma,
      df = df
    )
  }
  ## objective function
  loglikfun <- function(par) {
    fit <- fitfun(par)
    ll <- with(fit,  
               ddist(y, mu, sigma, df = df, left = left, right = right, log = TRUE))
    if(any(!is.finite(ll))) NaN else -sum(weights * ll)  
  }
  ## functions to evaluate gradients and hessian
  if(dfest | is.null(sdist)) {
    gradfun <- NULL
  } else { 
    gradfun <- function(par, type = "gradient") {
      fit <- fitfun(par)
      grad <- with(fit, 
                   sdist(y, mu, sigma, df = df, left = left, right = right))
      grad <- cbind(grad[,1]*x, grad[,2] * mu.eta(fit$zgamma) * z)
      return(-colSums(weights * grad))
    }
    hessfun <- function(par) {
      fit <- fitfun(par)
      hess <- with(fit, hdist(y, mu, sigma, left = left, right = right,
                              df = df, which = c("mu", "sigma", "mu.sigma", "sigma.mu")))
      grad <- with(fit, sdist(y, mu, sigma, left = left, right = right, 
                              df = df))[,"dsigma"]
      hess[, "d2sigma"] <- hess[, "d2sigma"]*mu.eta(fit$zgamma)^2 + grad*dmu.deta(fit$zgamma)
      hess[, "dmu.dsigma"] <- hess[, "dsigma.dmu"] <- hess[, "dmu.dsigma"]*mu.eta(fit$zgamma)
      hess <- weights*hess
      hessmu <- crossprod(hess[,"d2mu"]*x, x)
      hessmusigma <- crossprod(hess[,"dmu.dsigma"]*x, z)
      hesssigmamu <- crossprod(hess[,"dsigma.dmu"]*z, x)
      hesssigma <- crossprod(hess[,"d2sigma"]*z, z)
      -cbind(rbind(hessmu, hesssigmamu), rbind(hessmusigma, hesssigma))
    }
  }
  opt <- suppressWarnings(optim(par = start, fn = loglikfun, gr = gradfun,
                                method = method, hessian = hessian, control = control,lower=lower,upper=upper))
  if(opt$convergence > 0) {
    converged <- FALSE
    warning("optimization failed to converge")
  } else {
    converged <- TRUE
  }
  par <- opt$par
  fit <- fitfun(par)
  beta <- fit$beta
  gamma <- fit$gamma
  delta <- fit$delta
  mu <- fit$mu
  sigma <- fit$sigma
  vcov <- if(returnvcov) {
    if (hessian) solve(as.matrix(opt$hessian)) 
    else solve(hessfun(par))
  } else matrix(NA, k+q+dfest, n+k+dfest)
  df <- if(dfest) exp(delta) else df
  if(type == "crps") {
    ll <- sum(lldist(y, mu, sigma, left, right, log = TRUE, df = df))
    crps <- opt$value/n
  } else {
    ll <- -opt$value
    crps <- NULL
  } 
  
  names(beta) <- colnames(x)
  names(gamma) <- colnames(z)
  if (returnvcov) {
    colnames(vcov) <- rownames(vcov) <- c(
      colnames(x),
      paste("(scale)", colnames(z), sep = "_"),
      if(dfest) "(Log(df))")
  }
  
  rval <- list(
    coefficients = list(location = beta, scale = gamma, df = delta),
    df = df,
    residuals = y - mu,
    fitted.values = list(location = mu, scale = sigma),
    dist = dist,
    cens = list(left = left, right = right),
    optim = opt,  
    method = method,
    type = type,
    control = ocontrol,
    start = start,  
    weights = if(identical(as.vector(weights), rep.int(1, n))) NULL else weights,
    offset = list(location = if(identical(offset[[1L]], rep.int(0, n))) NULL else 
      offset[[1L]],
      scale = if(identical(offset[[2L]], rep.int(0, n))) NULL else offset[[2L]]),
    n = n,
    nobs = nobs,
    loglik = ll,
    crps = crps,
    vcov = vcov,
    link = list(scale = linkobj),
    truncated = truncated,
    converged = converged,
    iterations = as.vector(tail(na.omit(opt$counts), 1))
  )
  
  class(rval) <- "crch"
  return(rval)
}

# Define Inernal Function - "spread" (tidyr package)
#' @importFrom plyr split_labels id
spread <- function(data, key, value, fill = NA, convert = FALSE,
                   drop = TRUE, sep = NULL) {
  key_var <- vars_pull(names(data), !! dplyr::enquo(key))
  value_var <- vars_pull(names(data), !! dplyr::enquo(value))
  
  col <- data[key_var]
  col_id <- plyr::id(col, drop = drop)
  col_labels <- plyr::split_labels(col, col_id, drop = drop)
  
  rows <- data[setdiff(names(data), c(key_var, value_var))]
  if (ncol(rows) == 0 && nrow(rows) > 0) {
    # Special case when there's only one row
    row_id <- structure(1L, n = 1L)
    row_labels <- as.data.frame(matrix(nrow = 1, ncol = 0))
  } else {
    row_id <- plyr::id(rows, drop = drop)
    row_labels <- plyr::split_labels(rows, row_id, drop = drop)
    rownames(row_labels) <- NULL
  }
  
  overall <- plyr::id(list(col_id, row_id), drop = FALSE)
  n <- attr(overall, "n")
  # Check that each output value occurs in unique location
  if (anyDuplicated(overall)) {
    groups <- split(seq_along(overall), overall)
    groups <- groups[map_int(groups, length) > 1]
    
    shared <- sum(map_int(groups, length))
    
    str <- map_chr(groups, function(x) paste0(x, collapse = ", "))
    rows <- paste0(paste0("* ", str, "\n"), collapse = "")
    abort(glue(
      "Each row of output must be identified by a unique combination of keys.",
      "\nKeys are shared for {shared} rows:",
      "\n{rows}"
    ))
  }
  
  # Add in missing values, if necessary
  if (length(overall) < n) {
    overall <- match(seq_len(n), overall, nomatch = NA)
  } else {
    overall <- order(overall)
  }
  
  value <- data[[value_var]]
  ordered <- value[overall]
  if (!is.na(fill)) {
    ordered[is.na(ordered)] <- fill
  }
  
  if (convert && !is_character(ordered)) {
    ordered <- as.character(ordered)
  }
  dim(ordered) <- c(attr(row_id, "n"), attr(col_id, "n"))
  colnames(ordered) <- enc2utf8(col_names(col_labels, sep = sep))
  
  ordered <- as_tibble_matrix(ordered)
  
  # TODO: disable it for now bc convert is always F, no need to use 'map' function
  # if (convert) {
  #   ordered[] <- map(ordered, type.convert, as.is = TRUE)
  # }
  
  out <- append_df(row_labels, ordered)
  reconstruct_tibble(data, out, c(key_var, value_var))
}

col_names <- function(x, sep = NULL) {
  names <- as.character(x[[1]])
  
  if (is.null(sep)) {
    if (length(names) == 0) {
      # ifelse will return logical()
      character()
    } else {
      ifelse(is.na(names), "<NA>", names) # replace original are_na with is.na()
    }
  } else {
    paste(names(x)[[1]], names, sep = sep)
  }
}

as_tibble_matrix <- function(x) {
  # getS3method() only available in R >= 3.3
  get("as_tibble.matrix", asNamespace("tibble"), mode = "function")(x)
}

# split_labels <- function(df, id, drop = TRUE) {
#   if (length(df) == 0) {
#     return(df)
#   }
#   
#   if (drop) {
#     representative <- match(sort(unique(id)), id)
#     out <- df[representative, , drop = FALSE]
#     rownames(out) <- NULL
#     out
#   } else {
#     unique_values <- map(df, ulevels)
#     rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE))
#   }
# }
# 
# ulevels <- function(x) {
#   if (is.factor(x)) {
#     orig_levs <- levels(x)
#     x <- addNA(x, ifany = TRUE)
#     levs <- levels(x)
#     factor(levs, levels = orig_levs, ordered = is.ordered(x), exclude = NULL)
#   } else if (is.list(x)) {
#     unique(x)
#   } else {
#     sort(unique(x), na.last = TRUE)
#   }
# }

append_df <- function(x, y, after = length(x), remove = FALSE) {
  if (is.character(after)) {
    after <- match(after, dplyr::tbl_vars(x))
  } else if (!is.integer(after)) {
    stop("`after` must be character or integer", call. = FALSE)
  }
  
  # Replace duplicated variables
  x_vars <- setdiff(names(x), names(y))
  if (remove) {
    x_vars <- setdiff(x_vars, names(x)[[after]])
    after <- after - 1L
  }
  
  y <- append(x[x_vars], y, after = after)
  structure(y, class = class(x), row.names = .row_names_info(x, 0L))
}

#' @importFrom dplyr grouped_df group_vars
reconstruct_tibble <- function(input, output, ungrouped_vars = character()) {
  if (inherits(input, "grouped_df")) {
    old_groups <- dplyr::group_vars(input)
    new_groups <- intersect(setdiff(old_groups, ungrouped_vars), names(output))
    dplyr::grouped_df(output, new_groups)
  } else if (inherits(input, "tbl_df")) {
    # Assume name repair carried out elsewhere
    as_tibble(output, .name_repair = "minimal")
  } else {
    output
  }
}

#### Internal Function - RUVSeq Package
RUVs<-function(x, cIdx, k, scIdx, round=TRUE, epsilon=1, tolerance=1e-8, isLog=FALSE) {
  
  if(!isLog && !all(.isWholeNumber(x))) {
    warning(paste0("The expression matrix does not contain counts.\n",
                   "Please, pass a matrix of counts (not logged) or set isLog to TRUE to skip the log transformation"))
  }
  
  if(isLog) {
    Y <- t(x)
  } else {
    Y <- t(log10(x+epsilon))
  }
  
  scIdx <- scIdx[rowSums(scIdx > 0) >= 2, , drop = FALSE]
  Yctls <- matrix(0, prod(dim(scIdx)), ncol(Y))
  m <- nrow(Y)
  n <- ncol(Y)
  c <- 0
  for (ii in 1:nrow(scIdx)) {
    for (jj in 1:(ncol(scIdx))) {
      if (scIdx[ii, jj] == -1)
        next
      c <- c + 1
      Yctls[c, ] <- Y[scIdx[ii, jj], , drop = FALSE] -
        colMeans(Y[scIdx[ii, (scIdx[ii, ] > 0)], , drop = FALSE])
    }
  }
  Yctls <- Yctls[rowSums(Yctls) != 0, ]
  Y <- rbind(Y, Yctls)
  sctl <- (m + 1):(m + nrow(Yctls))
  svdRes <- svd(Y[sctl, ], nu = 0, nv = k)
  k <- min(k, max(which(svdRes$d > tolerance)))
  a <- diag(as.vector(svdRes$d[1:k]), ncol=k, nrow=k) %*% t(as.matrix(svdRes$v[, 1:k]))
  colnames(a) <- colnames(Y)
  W <- Y[, cIdx] %*% t(solve(a[, cIdx, drop = FALSE] %*% t(a[, cIdx, drop = FALSE]), a[, cIdx, drop = FALSE]))
  Wa <- W %*% a
  correctedY <- Y[1:m, ] - W[1:m, ] %*% a
  
  if(!isLog && all(.isWholeNumber(x))) {
    if(round) {
      correctedY <- round(exp(correctedY) - epsilon)
      correctedY[correctedY<0] <- 0
    } else {
      correctedY <- exp(correctedY) - epsilon
    }
  }
  
  W <- as.matrix(W[1:m,])
  colnames(W) <- paste("W", seq(1, ncol(W)), sep="_")
  return(list(W = W, normalizedCounts = t(correctedY)))
}

RUVr<-function(x, cIdx, k, residuals, center=TRUE, round=TRUE, epsilon=1, tolerance=1e-8, isLog=FALSE) {
  
  Y <- t(log10(x+epsilon))
  
  if(center) {
    E <- apply(residuals, 1, function(x) scale(x, center=TRUE, scale=FALSE))
  } else {
    E <- t(residuals)
  }
  m <- nrow(Y)
  n <- ncol(Y)
  svdWa <- svd(E[, cIdx])
  k <- min(k, max(which(svdWa$d > tolerance)))
  W <- svdWa$u[, (1:k), drop = FALSE]
  alpha <- solve(t(W) %*% W) %*% t(W) %*% Y
  correctedY <- Y - W %*% alpha
  if(!isLog && all(.isWholeNumber(x))) {
    if(round) {
      correctedY <- round(exp(correctedY) - epsilon)
      correctedY[correctedY<0] <- 0
    } else {
      correctedY <- exp(correctedY) - epsilon
    }
  }
  colnames(W) <- paste("W", seq(1, ncol(W)), sep="_")
  return(list(W = W, normalizedCounts = t(correctedY)))
}

RUVg<-function(x, cIdx, k, drop=0, center=TRUE, round=TRUE, epsilon=1, tolerance=1e-8, isLog=FALSE) {
  
  if(!isLog && !all(.isWholeNumber(x))) {
    warning(paste0("The expression matrix does not contain counts.\n",
                   "Please, pass a matrix of counts (not logged) or set isLog to TRUE to skip the log transformation"))
  }
  
  if(isLog) {
    Y <- t(x)
  } else {
    Y <- t(log10(x+epsilon))
  }
  
  if (center) {
    Ycenter <- apply(Y, 2, function(x) scale(x, center = TRUE, scale=FALSE))
  } else {
    Ycenter <- Y
  }
  if (drop >= k) {
    stop("'drop' must be less than 'k'.")
  }
  m <- nrow(Y)
  n <- ncol(Y)
  svdWa <- svd(Ycenter[, cIdx])
  first <- 1 + drop
  k <- min(k, max(which(svdWa$d > tolerance)))
  W <- svdWa$u[, (first:k), drop = FALSE]
  alpha <- solve(t(W) %*% W) %*% t(W) %*% Y
  correctedY <- Y - W %*% alpha
  if(!isLog && all(.isWholeNumber(x))) {
    if(round) {
      correctedY <- round(exp(correctedY) - epsilon)
      correctedY[correctedY<0] <- 0
    } else {
      correctedY <- exp(correctedY) - epsilon
    }
  }
  colnames(W) <- paste("W", seq(1, ncol(W)), sep="_")
  return(list(W = W, normalizedCounts = t(correctedY)))
}

.isWholeNumber <- function(x, tol = .Machine$double.eps^0.5) {
  !is.na(x) & abs(x - round(x)) < tol
}

residuals.DGEGLM <- function(object, type=c("deviance", "pearson"), ...) {
  y <- as.matrix(object$counts)
  mu <- as.matrix(object$fitted.values)
  theta <- 1/object$dispersion
  if(is.null(object$weights)) {
    wts <- rep(1, ncol(object$counts))
  } else {
    wts <- as.matrix(object$weights)
  }
  type <- match.arg(type)
  ymut <- cbind(y, mu, theta)
  
  res <- t(apply(ymut, 1, function(x) {
    yy <- as.vector(x[1:ncol(y)])
    mm <- as.vector(x[(ncol(y)+1):(ncol(y)+ncol(mu))])
    t <- x[length(x)]
    if(type=="deviance") {
      if(t==Inf) {
        d.res <- sqrt(pmax((poisson()$dev.resids)(yy, mm, wts), 0))
      } else {
        d.res <- sqrt(pmax((negative.binomial(theta=t)$dev.resids)(yy, pmax(mm, 1e-8), wts), 0))
      }
      return(ifelse(yy > mm, d.res, -d.res))
    } else if(type=="pearson") {
      if(t==Inf) {
        return((yy - mm) * sqrt(wts) / pmax(sqrt(poisson()$variance(mm)), 1))
      } else {
        return((yy - mm) * sqrt(wts) / pmax(sqrt(negative.binomial(theta=t)$variance(mm)), 1))
      }
    }
  }))
  return(res)
}

### Internal Function - MASS package
negative.binomial <- function(theta = stop("'theta' must be specified"), link = "log")  {
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  if (linktemp %in% c("log", "identity", "sqrt"))
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  } else {
    ## what else shall we allow?  At least objects of class link-glm.
    if(inherits(link, "link-glm")) {
      stats <- link
      if(!is.null(stats$name)) linktemp <- stats$name
    } else
      stop(gettextf("\"%s\" link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"", linktemp))
  }
  .Theta <- theta ## avoid codetools warnings
  env <- new.env(parent=.GlobalEnv)
  assign(".Theta", theta, envir=env)
  variance <- function(mu)
    mu + mu^2/.Theta
  validmu <- function(mu)
    all(mu > 0)
  dev.resids <- function(y, mu, wt)
    2 * wt * (y * log(pmax(1, y)/mu) - (y + .Theta) *
                log((y + .Theta)/ (mu + .Theta)))
  aic <- function(y, n, mu, wt, dev) {
    term <- (y + .Theta) * log(mu + .Theta) - y * log(mu) +
      lgamma(y + 1) - .Theta * log(.Theta) + lgamma(.Theta) - lgamma(.Theta+y)
    2 * sum(term * wt)
  }
  initialize <- expression({
    if (any(y < 0))
      stop("negative values not allowed for the negative binomial family")
    n <- rep(1, nobs)
    mustart <- y + (y == 0)/6
  })
  simfun <- function(object, nsim) {
    ftd <- fitted(object)
    rnegbin(nsim * length(ftd), ftd, .Theta)
  }
  environment(variance) <- environment(validmu) <-
    environment(dev.resids) <- environment(aic) <-
    environment(simfun) <- env
  famname <- paste("Negative Binomial(", format(round(theta, 4)), ")",
                   sep = "")
  structure(list(family = famname, link = linktemp, linkfun = stats$linkfun,
                 linkinv = stats$linkinv, variance = variance,
                 dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta,
                 initialize = initialize, validmu = validmu,
                 valideta = stats$valideta, simulate = simfun),
            class = "family")
}

### Internal Function - Vegan Package
decorana <- function (veg, iweigh = 0, iresc = 4, ira = 0, mk = 26, short = 0,
                      before = NULL, after = NULL)  {
  Const1 <- 1e-10
  Const2 <- 5
  Const3 <- 1e-11
  veg <- as.matrix(veg)
  aidot <- rowSums(veg)
  if (any(aidot <= 0))
    stop("all row sums must be >0 in the community matrix: remove empty sites")
  if (any(veg < 0))
    stop("'decorana' cannot handle negative data entries")
  adotj <- colSums(veg)
  if (any(adotj <= 0))
    warning("some species were removed because they were missing in the data")
  if (mk < 10)
    mk <- 10
  if (mk > 46)
    mk <- 46
  if (ira)
    iresc <- 0
  if (!is.null(before)) {
    if (is.unsorted(before))
      stop("'before' must be sorted")
    if (length(before) != length(after))
      stop("'before' and 'after' must have same lengths")
    for (i in seq_len(nrow(veg))) {
      tmp <- veg[i, ] > 0
      veg[i, tmp] <- approx(before, after, veg[i, tmp],
                            rule = 2)$y
    }
  }
  if (iweigh) {
    veg <- downweight(veg, Const2)
    aidot <- rowSums(veg)
    adotj <- colSums(veg)
  }
  v <- attr(veg, "v")
  v.fraction <- attr(veg, "fraction")
  adotj[adotj < Const3] <- Const3
  CA <- .Call('do_decorana', veg, ira, iresc, short, mk, as.double(aidot),
              as.double(adotj), PACKAGE = "MetaboAnalystR")
  if (ira)
    dnames <- paste("RA", 1:4, sep = "")
  else dnames <- paste("DCA", 1:4, sep = "")
  dimnames(CA$rproj) <- list(rownames(veg), dnames)
  dimnames(CA$cproj) <- list(colnames(veg), dnames)
  names(CA$evals) <- dnames
  origin <- apply(CA$rproj, 2, weighted.mean, aidot)
  if (ira) {
    evals.decorana <- NULL
  }
  else {
    evals.decorana <- CA$evals
    var.r <- diag(cov.wt(CA$rproj, aidot, method = "ML")$cov)
    var.c <- diag(cov.wt(CA$cproj, adotj, method = "ML")$cov)
    CA$evals <- var.r/var.c
    if (any(ze <- evals.decorana <= 0))
      CA$evals[ze] <- 0
  }
  additems <- list(evals.decorana = evals.decorana, origin = origin,
                   v = v, fraction = v.fraction, iweigh = iweigh,
                   before = before, after = after,
                   call = match.call())
  CA <- c(CA, additems)
  class(CA) <- "decorana" # c() strips class
  CA
}

# New .readDataTable2 Function
#' @importFrom data.table fread
.readDataTable2<-function(filePath){
  
  dat <- try(data.table::fread(filePath, header=T, check.names=FALSE, blank.lines.skip=TRUE, data.table=FALSE),silent=T);
  
  if(class(dat) == "try-error" || any(dim(dat) == 0)){
    print("Using slower file reader ...");
    formatStr <- substr(filePath, nchar(filePath)-2, nchar(filePath))
    if(formatStr == "txt"){
      dat <- try(read.table(filePath, header=TRUE, comment.char = "", check.names=F, as.is=T),silent=T);
    }else{ # note, read.csv is more than read.table with sep=","
      dat <- try(read.csv(filePath, header=TRUE, comment.char = "", check.names=F, as.is=T),silent=T);
    }  
  }
  return(dat);
}

## Tem Functions
.readDataTable <- function(fileName){
  
  dat <- tryCatch(
    data.table::fread(fileName, header=TRUE, check.names=FALSE, blank.lines.skip=TRUE, data.table=FALSE),
    error=function(e){
      print(e);
      return(.my.slowreaders(fileName));    
    }, 
    warning=function(w){
      print(w);
      return(.my.slowreaders(fileName));
    });
  
  if(any(dim(dat) == 0)){
    dat <- .my.slowreaders(fileName);
  }
  return(dat);
}
.get.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    return(mSet)
  }else{
    return(mSetObj);
  }
}
.set.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    mSet <<- mSetObj;
    return (1);
  }
  return(mSetObj);
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetAllBatchNames <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$dataSet$batch)){
    return(0);
  }
  names(mSetObj$dataSet$batch);
}

ResetBatchData <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$batch <- mSetObj$dataSet$batch.cls <- NULL;
  return(.set.mSet(mSetObj));
}

#'Create semitransparant colors
#'@description Create semitransparant colors for a given class label
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

CreateSemiTransColors <- function(cls){
  
  # note, the first color (red) is for QC
  col.nms <- rainbow(length(levels(cls)));
  
  # convert to semi-transparent
  semi.nms <- ToSemiTransParent(col.nms);
  
  # now expand to the one-to-one match to cls element
  col.vec <- vector(mode="character", length=length(cls));
  for (i in 1:length(levels(cls))){
    lv <- levels(cls)[i];
    col.vec[cls==lv] <- semi.nms[i];
  }
  return(col.vec);
}

# convert rgb color i.e. "#00FF00FF" to semi transparent
ToSemiTransParent <- function (col.nms, alpha=0.5){
  rgb.mat <- t(col2rgb(col.nms));
  rgb(rgb.mat/255, alpha=alpha);
}

