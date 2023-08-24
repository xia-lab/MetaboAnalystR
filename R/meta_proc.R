##################################################
## R script for ExpressAnalyst
## Description: functions only for multiple gene expression data
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#' Perform Data Sanity Check
#'
#' This function performs a sanity check on the input data, including class labels and data matrix.
#'
#' @param fileName The name of the data file to be checked.
#'
#' @return A modified dataset after performing data sanity checks.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#'
#' @examples
#' \dontrun{
#' SanityCheckData("data_filename")
#' }
#'
#' @export
#' @license MIT License
#'
SanityCheckData <- function(fileName){
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(fileName);
  
  # general sanity check then omics specific
  
  # use first column by default
  cls <- dataSet$meta.info[,1]
  
  # check class info
  cls.lbl <- as.factor(as.character(cls));
  min.grp.size <- min(table(cls.lbl));
  cls.num <- length(levels(cls.lbl));
  
  msg <- paste(cls.num, "groups were defined in samples based on the first metadata column.");
  dataSet$cls.num <- cls.num;
  dataSet$min.grp.size <- min.grp.size;
  
  # check numerical matrix  
  int.mat <- dataSet$data.norm;
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  
  num.mat <- apply(int.mat, 2, as.numeric);
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
  rownames(int.mat) <- rowNms;
  colnames(int.mat)<- colNms;
  
  # check for feats with all constant (var =0)
  varCol <- apply(int.mat, 1, var, na.rm=T);
  constCol <- (varCol == 0 | is.na(varCol));
  constNum <- sum(constCol, na.rm=T);
  
  if(constNum > 0){
    msg<-c(msg, paste("<font color=\"red\">", constNum, "features with a constant or single value across samples were found and deleted.</font>"));
    int.mat <- int.mat[!constCol, , drop=FALSE];
  }
  
  # check zero, NA values
  totalCount <- nrow(int.mat)*ncol(int.mat);
  naCount <- sum(is.na(int.mat));
  naPercent <- round(100*naCount/totalCount,1)
  
  msg <- c(msg, paste("A total of ", naCount, " (", naPercent, "%) missing values were detected.", sep=""));
  
  # obtain original half of minimal positive value (threshold)
  minConc <- min(int.mat[int.mat>0], na.rm=T)/2;
  
  # remove smpls/exp with over half missing value
  good.inx<-apply(is.na(int.mat), 2, sum)/nrow(int.mat)<0.6;
  if(sum(!good.inx)>0){
    msg <- c(msg, paste(sum(!good.inx), "Low quality samples (>60% missing) removed."));
    int.mat <- int.mat[,good.inx, drop=FALSE];
    meta.info <- dataSet$meta.info;
    meta.info <- meta.info[good.inx, , drop=F];
    dataSet$meta.info <- meta.info;
  }
  
  # remove ffeatures/variables with over half missing value    
  gd.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<0.75;
  gene.num <- nrow(int.mat);
  if(sum(!gd.inx) > 0){
    int.mat <- int.mat[gd.inx,];
    msg <- c(msg, paste(sum(!gd.inx), "low quality genes (>75% missing) removed"));
    if(nrow(int.mat)/gene.num < 0.25){
      AddErrMsg("Too many missing values - low quality data rejected.");
      return(0);
    }
  }
  
  msgSet$current.msg <- msg;
  dataSet$minConc <- minConc;
  dataSet$data.norm <- int.mat;
  dataSet$cls <- cls.lbl

  saveSet(msgSet, "msgSet");
  return(RegisterData(dataSet, 1));
}

UpdateSampleBasedOnLoading<-function(filenm, gene.id, omicstype){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  if(omicstype != "NA"){
  sel.nms <- names(mdata.all)[mdata.all==1];
    for(i in 1:length(sel.nms)){
      dat <- readDataset(sel.nms[i]);
      if(dat$type == omicstype){
        dataSet = dat;
      }
    }
  }else{
    dataSet <- .get.rdt.set();
  }
  
  inx <- which(dataSet$enrich_ids == gene.id)
  id <- unname(dataSet$enrich_ids[inx])
  vec = as.vector(dataSet$data.norm[rownames(dataSet$data.norm) == gene.id,])
  colors<- ComputeColorGradient(as.numeric(vec), "black", F, F);
  sink(filenm);
  cat(toJSON(colors));
  sink();
}

#' Plot Data Profile
#'
#' This function generates data profile plots for quality control, including box plots and PCA plots.
#'
#' @param dataName The name of the dataset to be used for plotting.
#' @param type The type of data profile to plot (e.g., "boxplot" or "pca").
#' @param boxplotName The name of the file to save the box plot image.
#' @param pcaName The name of the file to save the PCA plot image.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' PlotDataProfile("data_filename", "boxplot", "boxplot_image.png", "pca_image.png")
#' }
#'
#' @export
#' @license MIT License
#'
PlotDataProfile<-function(dataName,type, boxplotName, pcaName){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  qc.boxplot(as.matrix(dataSet$data.norm), boxplotName);
  qc.pcaplot(dataSet, as.matrix(dataSet$data.norm), pcaName);
}

ScalingData <-function (nm,opt){
  dataSet <- readDataset(nm);
  return(ScalingDataOmics(dataSet, opt))
}

ScalingDataOmics <-function (dataSet, norm.opt){
  return(1) 
}

###
### Data utilities
###

GetCurrentDatasets <-function(type){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  sel.nms <- names(mdata.all)[mdata.all==1];
  return(sel.nms);
}

SetGroupContrast <- function(dataName, grps, meta="NA"){
    dataSet <- readDataset(dataName);
  

    if(meta == "NA"){
        meta <- 1;
    }

  if(length(levels(dataSet$meta.info[,meta]))>2){ 
    cls <- dataSet$meta.info[,meta]
    print("Updating group contrasts .....");
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    sel.inx <- as.character(cls) %in% grp.nms;
    
    # regenerate factor to drop levels, force the levels order
    group <- factor(cls[sel.inx], levels=grp.nms);  
    data <- dataSet$data.norm[, sel.inx];
    dataSet$cls <- group;
    dataSet$data.norm <- data;
  }
    RegisterData(dataSet);  

}

CheckDataType <- function(dataName, type){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  isOk <- T;
  data <- readDataQs("data.raw.qs", paramSet$anal.type, dataName);
  containsNeg <- "TRUE" %in% names(table(data < 0)) ;
  current.msg <- "";
  negativeBool <- F;
  logBool <- F;
  countBool <- F;
  if(containsNeg){
    data <- data + abs(min(data));
    negativeBool <- T;
  }
  
  tbl <- table(sum(data < 50));
  if("TRUE" %in% names(tbl)){
    num2 <- tbl[["TRUE"]];
    total <- dim(data)[1]* dim(data)[2];
    pct <- num2/total;
    if(pct > 0.8){
      logBool <- T;  
    }
  }else{
    logBool <- F; 
  }
  
  decimalTbl <- table(sum(data %% 1 != 0))
  if("TRUE" %in% names(decimalTbl)){
    countBool <- T;
  }
  
  
  if(type == "false"){
    if(!countBool){
      msgSet$current.msg <- paste(msgSet$current.msg, "Decimal values detected;");
      isOk <-  F;
    }
    if(negativeBool){
      msgSet$current.msg <- paste(msgSet$current.msg, "Negative values detected;");
      isOk <-  F;
    }
  }

  dataSet$isValueNormalized <- type
  saveSet(msgSet, "msgSet");      

  if(!isOk){
    res <- 0;
  }else{
    res <- 1;
  }
  return(RegisterData(dataSet, res));

}

#' Remove Variables with High Missing Percentage
#'
#' This function removes variables (columns) from the dataset that have a high percentage of missing values.
#'
#' @param dataName The name of the dataset to be processed.
#' @param percent The threshold percentage of missing values above which variables will be removed.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' RemoveMissingPercent("data_filename", 0.2)
#' }
#'
#' @export
#' @license MIT License
#'
RemoveMissingPercent <- function(dataName="", percent=perct){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  if(paramSet$anal.type=="onedata"){
  data.annotated <- qs::qread("orig.data.anot.qs")
  }else{
  data.annotated <- readDataQs("data.annotated.qs", paramSet$anal.type, dataName);
  }
  
  int.mat <- data.annotated;
  good.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<percent;
  data.annotated <- as.data.frame(int.mat[good.inx, , drop=FALSE]);
  if(sum(!good.inx)>0){
    msgSet$current.msg <- paste(sum(!good.inx), " variables were removed for threshold", round(100*percent, 2), "percent.");
  }

  saveDataQs(data.annotated, "data.annotated.qs", paramSet$anal.type, dataName);
  RegisterData(dataSet);
}

#' Impute Missing Values in Variables
#'
#' This function imputes missing values in variables (columns) of the dataset using various methods.
#'
#' @param dataName The name of the dataset to be processed.
#' @param method The imputation method to be used. Can be one of "exclude", "min", "colmin", "mean", "median", "knn_var", "knn_smp", "bpca", "ppca", "svdImpute".
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' ImputeMissingVar("data_filename", method = "min")
#' }
#'
#' @export
#' @license MIT License
#'
ImputeMissingVar <- function(dataName="", method="min"){
  dataSet <- readDataset(dataName);
  msgSet <- readSet(msgSet, "msgSet"); 
  paramSet <- readSet(paramSet, "paramSet"); 
  data.annotated <- readDataQs("data.annotated.qs", paramSet$anal.type, dataName);
  row.nms <- rownames(data.annotated);
  current.msg <- msgSet$curren.msg;
  int.mat <- data.annotated;
  new.mat <- NULL;

  if(method=="exclude"){
    good.inx<-apply(is.na(int.mat), 1, sum)==0
    new.mat<-int.mat[good.inx,, drop=FALSE];
    current.msg <- c(current.msg ,"Variables with missing values were excluded.");
    row.nms<-row.nms[good.inx]
  }else if(method=="min"){
    new.mat<- suppressWarnings(ReplaceMissingByLoD(int.mat));
    current.msg <- c(current.msg, "Missing variables were replaced by LoDs (1/5 of the min positive value for each variable)");
  }else if(method=="colmin"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-min(x,na.rm=T)/2;
      }
      x;
    });
    new.mat = t(new.mat)
    current.msg <- c(current.msg,"Missing variables were replaced by 1/2 of min values for each feature column.");
  }else if (method=="mean"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-mean(x,na.rm=T);
      }
      x;
    });
    new.mat = t(new.mat)
    current.msg <- c(current.msg,"Missing variables were replaced with the mean value for each feature column.");
  }else if (method == "median"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-median(x,na.rm=T);
      }
      x;
    });
   new.mat = t(new.mat)
    current.msg <- c(current.msg,"Missing variables were replaced with the median for each feature column.");
  }else{
    if(method == "knn_var"){
      new.mat<-t(impute::impute.knn(as.matrix(int.mat))$data);
    }else if(method == "knn_smp"){
      new.mat<-impute::impute.knn(data.matrix(t(int.mat)))$data;
    }else{
      if(method == "bpca"){
        new.mat<-pcaMethods::pca(t(int.mat), nPcs =5, method="bpca", center=T)@completeObs;
      }else if(method == "ppca"){
        new.mat<-pcaMethods::pca(t(int.mat), nPcs =5, method="ppca", center=T)@completeObs;
      }else if(method == "svdImpute"){
        new.mat<-pcaMethods::pca(t(int.mat), nPcs =5, method="svdImpute", center=T)@completeObs;
      }
    }
    new.mat = t(new.mat)
    current.msg <- c(current.msg, paste("Missing variables were imputated using", toupper(method)));
  }
  msgSet$current.msg <- current.msg;  
  saveSet(msgSet, "msgSet");      

  data.missed <- as.data.frame(new.mat);
  rownames(data.missed) <- row.nms;
  saveDataQs(data.missed, "data.missed.qs", paramSet$anal.type, dataName);
  if(paramSet$anal.type=="onedata"){
  dataSet$data.norm <- dataSet$data.anot <- data.missed
  }
  RegisterData(dataSet);
}

#' Filter Data by Count and Variance
#'
#' This function filters the data in a dataset by count and variance thresholds.
#'
#' @param nm The name of the dataset to be processed.
#' @param countOpt The option to use for count filtering. Can be "pct" (percentage) or "count" (absolute count).
#' @param count The count threshold for filtering.
#' @param var The variance threshold for filtering.
#'
#' @return The filtered dataset.
#'
#' @export
#' @license MIT License
#'
FilteringData <- function(nm, countOpt="pct",count, var){
  dataSet <- readDataset(nm);
  
  return(FilteringDataOmics(dataSet,countOpt, count,  var))
}

FilteringDataOmics <- function(dataSet, countOpt="pct",count, var){
  msgSet <- readSet(msgSet, "msgSet");  
  paramSet <- readSet(paramSet, "paramSet");
  dataName <- dataSet$name;
  
  data <- readDataQs("data.missed.qs", paramSet$anal.type, dataName);
  msg <- "";
  
  count.thresh = as.numeric(count);
  var.thresh = as.numeric(var);
  if((count.thresh + var.thresh) >0){
  sum.counts <- apply(data, 1, sum, na.rm=TRUE);
  if(countOpt == "pct"){
  inx <- order(sum.counts);
  data <- data[inx,];
  rm.inx <- round(count.thresh/100 * nrow(data))
  data <- data[-c(1:rm.inx),];
  rmSum <- rm.inx;
}else{
  rm.inx <- sum.counts < count.thresh;
  data <- data[!rm.inx,];
  rmSum <- sum(rm.inx);
}
  msg <- paste(msg, "Filtered ",rmSum, " genes with low counts.", collapse=" ");
  filter.val <- apply(data, 1, IQR, na.rm=T);
  nm <- "Interquantile Range";
  rk <- rank(-filter.val, ties.method='random');
  kp.pct <- (100 - var.thresh)/100;
  remain <- rk < nrow(data)*kp.pct;
  data <- data[remain,];
  msg <- paste("Filtered ", sum(!remain), " low variance genes based on IQR");
  
  if((sum(!remain) + rmSum)/nrow(data) > 0.8){
    msg <- paste("Over 80% of features are removed. Please readjust filtering thresholds." );
    msgSet$current.msg <- msg;
    saveSet(msgSet, "msgSet");      
    return(0);
  }
}

  saveDataQs(data, "data.filtered.qs", paramSet$anal.type, dataName);

  dataSet$data.norm <- data
 
  saveSet(msgSet, "msgSet");      
  return(RegisterData(dataSet))
}


# Limit of detection (1/5 of min for each var)
.replace.by.lod <- function(x){
    lod <- min(x[x>0], na.rm=T)/5;
    x[x==0|is.na(x)] <- lod;
    return(x);
}

ReplaceMissingByLoD <- function(int.mat){
    int.mat <- as.matrix(int.mat);

    rowNms <- rownames(int.mat);
    colNms <- colnames(int.mat);
    int.mat <- t(apply(int.mat, 1, .replace.by.lod));
    rownames(int.mat) <- rowNms;
    colnames(int.mat) <- colNms;
    return (int.mat);
}

SetDataTypeMeta <- function(dataName, type){
    dataSet <- readDataset(dataName);
    dataSet$data.type <- type;
    RegisterData(dataSet)
}

#' Read Omics Data for Meta-Analysis
#'
#' This function reads gene expression data and parses it into an R object for meta-analysis.
#'
#' @param fileName File name of the data table.
#' @author Jeff Xia \email{jeff.xia@mcgill.ca}
#' @details McGill University, Canada
#' @license MIT License
#'
#' @return A registered dataset object for further analysis.
#'
#' @export
#'
ReadOmicsData <- function(fileName) {
  # need to handle reading .csv files too!
  msgSet <- readSet(msgSet,"msgSet");
  paramSet <- readSet(paramSet,"paramSet");
  paramSet$anal.type <- "metadata";
  data <- .readDataTable(fileName);
  dataSet <- list();
  
  meta.info <- list();
  cls.inx <- grep("^#CLASS", data[,1]);
  if(length(cls.inx) > 0){ 
    for(i in 1:length(cls.inx)){
      inx <- cls.inx[i];
      cls.nm <- substring(data[inx, 1],2); # discard the first char #
      if(nchar(cls.nm) > 6){
        cls.nm <- substring(cls.nm, 7); # remove class
      }
      if(grepl("[[:blank:]]", cls.nm)){
        cls.nm<- gsub("\\s+","_", cls.nm);
        msg <- c(msg, " Blank spaces in group names are replaced with underscore '_'! ");
      }
      cls.lbls <- setNames(as.character(data[inx, -1]),colnames(data)[-1]);
      # test NA
      na.inx <- is.na(cls.lbls);
      cls.lbls[na.inx] <- "NA";
      cls.lbls <- ClearFactorStrings(cls.lbls);
      
      meta.info[[cls.nm]] <- cls.lbls;
    }
    meta.info <- data.frame(meta.info);
    smpl.nms <- .cleanNames(colnames(data)[-1], "sample_name");
    rownames(meta.info) <- smpl.nms;
    #print(meta.info);
    disc.inx <- GetDiscreteInx(meta.info);
    if(sum(disc.inx) == length(disc.inx)){
      na.msg <- "All metadata columns are OK!"
    }else{
      bad.meta<- paste(names(disc.inx)[!disc.inx], collapse="; ");
      na.msg <- paste0("<font style=\"color:red\">Detected presence of unique values in the following columns: <b>", bad.meta, "</b></font>","Please make sure the metadata is in right format! You can use meta editor to update the information !");
    }
    
    cont.inx <- GetNumbericalInx(meta.info);
    cont.inx <- !disc.inx & cont.inx; # discrete is first
    
    if(sum(cont.inx)>0){
      # make sure the discrete data is on the left side
      meta.info <- cbind(meta.info[,disc.inx, drop=FALSE], meta.info[,cont.inx, drop=FALSE]);
    }
    dataSet$meta.info <- dataSet$metaOrig <- meta.info
    data <- data[-cls.inx,];
    dataSet$fst.cls <- dataSet$meta.info[which(dataSet$meta.info[,1]!="NA"),1]
    if(ncol(meta.info)>1){
      dataSet$sec.cls <- dataSet$meta.info[which(dataSet$meta.info[,2]!="NA"),2]
    }
    
    dataSet$disc.inx <-dataSet$disc.inx.orig <- meta.info$disc.inx
    dataSet$cont.inx <-dataSet$cont.inx.orig  <- meta.info$cont.inx
  }
  
  if(class(data) == "try-error" || ncol(data) == 1){
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
  
  var.nms <- data[,1];
  data[,1] <- NULL;
  smpl.nms <- colnames(data);
  data <- as.matrix(data);
  rownames(data) <- var.nms;
  
  res <- RemoveDuplicates(data, "mean", quiet=T, paramSet, msgSet); # remove duplicates
  data <- res[[1]];
  msgSet <- res[[2]];
  
  data <- as.data.frame(data)
  var.nms <- rownames(data)
  
  msg <- paste("A total of ", ncol(data), " samples and ", nrow(data), " features were found")
  
  # Basic checks - no duplicate samples names
  # Check for uniqueness of sample name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
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
  smpl.nms <- .cleanNames(smpl.nms, "sample_name");
  
  # keep a copy of original names for saving tables 
  orig.var.nms <- var.nms;
  var.nms <- .cleanNames(var.nms, "var_name"); # allow space, comma and period
  names(orig.var.nms) <- var.nms;
  
  msgSet <- readSet(msgSet, "msgSet");
  msgSet$current.msg <- msg;
  # now create the dataSet
  dataSet$orig.var.nms <- orig.var.nms;
  data <- data.frame(apply(data, 2, function(x) as.numeric(as.character(x))))
  # now reassgin the dimension names
  colnames(data) <- smpl.nms;
  rownames(data) <- var.nms;
  
  #dir.create() does not crash if the directory already exists, it just prints out a warning.
  dir.create(paste0(fileName, "_data"), showWarnings = FALSE);
  
  dataSet$data.norm <- data;
  saveDataQs(data, "data.raw.qs", paramSet$anal.type, fileName);
  dataSet$data.annotated <- ""
  dataSet$data.missed <- ""
  dataSet$data.filtered <- ""
  dataSet$name <- fileName;
  dataSet$de.method <- "NA"
  dataSet$type <- "rna_b";
  dataSet$readableType <- "Transcriptomics data";
  dataSet$enrich_ids <- rownames(dataSet$data.norm)
  names(dataSet$enrich_ids) <- rownames(dataSet$data.norm)
  
  # update current dataset
  saveSet(msgSet,"msgSet");
  paramSet$mdata.all[[fileName]]<-1
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  saveSet(paramSet, "paramSet");
  return(RegisterData(dataSet));
}


UpdateDE<-function(dataName, p.lvl = 0.05, fc.lvl = 1){
  dataSet <- readDataset(dataName);
  
  dataSet$pval <- p.lvl;
  
  res <- dataSet$comp.res;
  
  hit.inx <- as.numeric(res[, "adj.P.Val"]) <= p.lvl; #pval
  
  if(length(which(hit.inx == T)) == 0){
    return (c(1, 0, nrow(res)));
  }
  # note, hit.inx can contain NA, not T/F
  hit.inx <- which(hit.inx);
  
  res.sig<-res[hit.inx, , drop=F];
  #print(colnames(res.sig));
  if("logFC" %in% colnames(res.sig)){
  hit.inx <- abs(as.numeric(res.sig[, "logFC"])) > fc.lvl #foldchange
  }else{
  hit.inx <- abs(as.numeric(res.sig[, "coefficient"])) > fc.lvl #foldchange
  }
  if(length(which(hit.inx == T)) == 0){
    return (c(1, 0, nrow(res)));
  }
  # note, hit.inx can contain NA, not T/F
  hit.inx <- which(hit.inx);
  
  res.sig<-res.sig[hit.inx, , drop=F];
  
  sig.count <- nrow(res.sig);
  de.genes <- rownames(res.sig);
  non.sig.count <- nrow(res)-sig.count;
  
  dataSet$sig.mat <- res.sig;
  
  
  return(RegisterData(dataSet, c(1, sig.count, non.sig.count)))
}

#' Perform Sanity Check on Metadata
#'
#' This function performs a sanity check on the metadata associated with the datasets.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @license MIT License
#'
#' @return integer 0 or 1 to indicate success or failure
#'
#' @export
#'
SanityCheckMetaData <- function(){
   paramSet <- readSet(paramSet, "paramSet");
   meta <- paramSet$dataSet$meta.info;
   mdata.all <- paramSet$mdata.all; 
   sel.nms <- names(mdata.all)[mdata.all==1];
   sampleNms.all <- vector();
   for(i in 1:length(sel.nms)){
     dataSet <- readDataset(sel.nms[i]);
     sampleNms <- rownames(dataSet$data);
     #check if sample names match
     if(!all(sampleNms %in% rownames(meta))){
        msgSet$current.msg <- paste(msgSet$current.msg, "Some samples are not annotated in metadata file!");
        saveSet(msgSet, "msgSet");
        return(0);
     }
     meta.ind <- meta[sampleNms, , drop = FALSE]
     meta.ind <- meta.ind[match(sampleNms, rownames(meta.ind)), ]
     dataSet$meta.info <- meta.ind;
     sampleNms.all <- c(sampleNms.all, sampleNms);
     RegisterData(dataSet);
   }

   #trim metadata table based on samples contained in dataset
   meta_trimmed <- meta[sampleNms.all, , drop = FALSE]
   meta_trimmed <- meta_trimmed[match(sampleNms.all, rownames(meta_trimmed)), ]
   paramSet$dataSet$meta.info <- meta_trimmed;
   return(1)
}

#' Check the integrity and consistency of metadata for meta-analysis
#'
#' This function checks the integrity and consistency of metadata across
#' different datasets intended for meta-analysis. It ensures that metadata
#' is properly aligned, consistent, and categorized as discrete or continuous.
#'
#' @export
#'
#' @return
#' Returns 1 to indicate successful completion of checks and updates.
#'
#' @examples
#' CheckMetaIntegrity()
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#'
#' @param None
CheckMetaIntegrity <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  mdata.all <- paramSet$mdata.all;
  
  sel.nms <- names(mdata.all)
  
  msgSet <- readSet(msgSet, "msgSet");
  data.list <- list()
  cnms <- list()
  metas <- list();
  meta.dfs <- list();
  for(i in 1:length(sel.nms)){
    dat = readDataset(sel.nms[i])
    cnms[[i]] <- colnames(dat$data.norm);
    metas[[i]] <- as.vector(dat$meta.info[,1]);
    meta.dfs[[i]] <- dat$meta.info;
  }

  if(length(metas) == 0){
    msgSet$current.msg <- paste0('Please make sure row(s) corresponding to meta-data start with "#CLASS" or to include a metadata file.' );
    saveSet(msgSet, "msgSet");
    return(0)
  }
  
  for(i in 1:length(sel.nms)){
    if(length(unique(metas[[i]]))>2){
      msgSet$current.msg <- "For meta-analysis, make sure the meta-data is composed of exactly two different groups";
      saveSet(msgSet, "msgSet");
      return(0)
    }
    
    for(j in 1:length(sel.nms)){
      
      boolMeta <- identical(sort(unique(metas[[i]])),sort(unique(metas[[j]])))
      
      if(!boolMeta){
        msgSet$current.msg <- "Please make sure the meta data is consistent across all uploaded data sets.";
        saveSet(msgSet, "msgSet");
        return(0)
      }
    }
  }

  # Merge the data frames in the list while preserving the original order
  metadata <- do.call(rbind, meta.dfs)  
  na.msg <- ""
  #print(metadata);
  #print("metadata");

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
  smpl_nms <- rownames(metadata);
    
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    data.smpl.nms <- colnames(dataSet$data.norm)
    
    # now remove extra meta if present, and order them
    nm.hits2 <- which(smpl_nms %in% data.smpl.nms);
    metadata$Dataset[nm.hits2] <- sel.nms[i];
    metadata1 <- metadata[nm.hits2,,drop=F];
    metadata1[] <- lapply( metadata1, factor)
    
    
    dataSet$meta.info <- dataSet$metaOrig <- metadata1
    dataSet$disc.inx <-dataSet$disc.inx.orig <- disc.inx[colnames(metadata1)]
    dataSet$cont.inx <-dataSet$cont.inx.orig  <- cont.inx[colnames(metadata1)]
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
  return(1)
  
}


#'Plot PCA plot for meta-analysis samples
#'@description 
#'@param imgNm name of the image to output
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'

PlotMetaPCA <- function(imgNm, dpi, format,factor){
  inmex.meta <- qs::qread("inmex_meta.qs");
  x <- inmex.meta[["data"]];
  dpi <- as.numeric(dpi);
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  require('lattice');
  require('ggplot2');
  #remove infinity
  x[!is.finite(x)] <- NA;
  pca <- prcomp(t(na.omit(x)));
  imp.pca<-summary(pca)$importance;
  xlabel <- paste0("PC1"," (", 100*round(imp.pca[2,][1], 3), "%)")
  ylabel <- paste0("PC2"," (", 100*round(imp.pca[2,][2], 3), "%)")
  names <- colnames(x);
  pca.res <- as.data.frame(pca$x);
  # increase xlim ylim for text label
  xlim <- GetExtendRange(pca.res$PC1);
  ylim <- GetExtendRange(pca.res$PC2);
  Conditions <- factor(inmex.meta$cls.lbl)
  Datasets <- factor(inmex.meta$data.lbl)
  pcafig <- ggplot(pca.res, aes(x=PC1, y=PC2,  color=Conditions ,shape=Datasets)) +
    geom_point(size=4, alpha=0.5) + 
    xlim(xlim)+ ylim(ylim) + 
    xlab(xlabel) + ylab(ylabel) + 
    theme_bw()
  
  Cairo(file=imgNm, width=8, height=6, type=format, bg="white", unit="in", dpi=dpi);
  print(pcafig);
  dev.off();
  
  imgSet <- readSet(imgSet, "imgSet");
  if(is.null(paramSet$performedBatch) || !paramSet$performedBatch){
    imgSet$PlotMetaPCA <- imgNm;
  }else{
    imgSet$PlotMetaPCA_batch <- imgNm;
  }
  saveSet(imgSet);
}


PlotMetaDensity<- function(imgNm, dpi=72, format, factor){
  require("ggplot2")
  inmex.meta <- qs::qread("inmex_meta.qs");
  dat <- inmex.meta$data;
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  dpi <- as.numeric(dpi);
  
  df <- data.frame(inmex.meta$data, stringsAsFactors = FALSE);
  df <- stack(df);

  Factor <- inmex.meta$data.lbl;
  
  conv <- data.frame(ind=colnames(inmex.meta$data), class=Factor);
  conv$ind <- gsub("-", ".", conv$ind);
  df1 <- merge(df, conv, by="ind");
  Cairo(file=imgNm, width=10, height=6, type=format, bg="white", dpi=dpi, unit="in");
  g =ggplot(df1, aes(x=values)) + 
        geom_line(aes(color=class, group=ind), stat="density", alpha=0.3) + 
        geom_line(aes(color=class), stat="density", alpha=0.6, size=1.5) +
        theme_bw()
  print(g);
  dev.off();

  imgSet <- readSet(imgSet, "imgSet");
  if(is.null(paramSet$performedBatch) || !paramSet$performedBatch){
    imgSet$PlotMetaDensity <- imgNm;
  }else{
    imgSet$PlotMetaDensity_batch <- imgNm;
  }
  saveSet(imgSet);


}
