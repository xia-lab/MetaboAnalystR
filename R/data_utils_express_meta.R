##################################################
## R script for ExpressAnalyst
## Description: functions only for single gene expression data
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#'Sanity check individual dataset for meta-analysis 
#'@param fileName The filename of dataset in qs format
#'@author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
SanityCheckData <- function(fileName){
  msgSet <- readSet(msgSet, "msgSet");
  dataSet <- readDataset(fileName);
  
  
  # general sanity check then omics specific
  
  # use first column by default
  cls <- dataSet$meta[,1]
  
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
    meta.info <- dataSet$meta;
    meta.info <- meta.info[good.inx, , drop=F];
    dataSet$meta <- meta.info;
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


# remove data object, the current dataSet will be the last one by default 
RemoveData <- function(dataName){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  if(!is.null(paramSet$mdata.all[[dataName]])){
    paramSet$mdata.all[[dataName]] <- NULL;
  }
  saveSet(paramSet, "paramSet");
}

# users can select one or more data for analysis
# note, we use 1 to indicate this is selected
# and by default is all selected. 
SelectData <- function(){
  if(!exists('nm.vec')){
    msgSet <- readSet(msgSet, "msgSet");
    msgSet$current.msg <-"No dataset is selected for analysis!";
    saveSet(msgSet, "msgSet");
    return(0);
  }
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  all.nms <- names(mdata.all);
  for(nm in all.nms){
    if(nm %in% nm.vec){
      mdata.all[[nm]] <- 1;
    }else{
      mdata.all[[nm]] <- 0;
    }
  }
  

  paramSet$mdata.all <- mdata.all;
  saveSet(paramSet, "paramSet");
  rm('nm.vec', envir = .GlobalEnv);
  return(1);
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

PlotDataProfile<-function(dataName,type, boxplotName, pcaName){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  #if(type=="normalize"){
    qc.boxplot(as.matrix(dataSet$data.norm), boxplotName);
    qc.pcaplot(dataSet, as.matrix(dataSet$data.norm), pcaName);
  #}else{
  #  data.raw <- readDataQs("data.raw.qs", paramSet$anal.type, dataName);
  #  qc.boxplot(as.matrix(data.raw), boxplotName);
  #  qc.pcaplot(dataSet, as.matrix(data.raw), pcaName);
  #}
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

GetFeatureNum <-function(dataName){
  dataSet <- readDataset(dataName);
  
  return(nrow(dataSet$data.norm));
}

ClearFactorStrings<-function(cls.nm, query){
  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  
  # kill multiple white space
  query <- gsub(" +","_",query);
  # remove non alphabets and non numbers 
  query <- gsub("[^[:alnum:] ]", "_", query);
  
  # test all numbers (i.e. Time points)
  chars <- substr(query, 0, 1);
  num.inx<- chars >= '0' & chars <= '9';
  if(all(num.inx)){
    query = as.numeric(query);
    nquery <- paste(cls.nm, query, sep="_");
    query <- factor(nquery, levels=paste(cls.nm, sort(unique(query)), sep="_"));
  }else{
    query[num.inx] <- paste(cls.nm, query[num.inx], sep="_");
    query <- factor(query);
  }
  return (query);
}

.set.dataSet <- function(dataSetObj=NA){
  RegisterData(dataSetObj);
  return (1);
}

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

  if(length(levels(dataSet$meta[,meta]))>2){ 
    cls <- dataSet$meta[,meta]
    print("Updating group contrasts .....");
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    sel.inx <- as.character(cls) %in% grp.nms;
    
    # regenerate factor to drop levels, force the levels order
    group <- factor(cls[sel.inx], levels=grp.nms);  
    data <- dataSet$data.norm[, sel.inx];
    dataSet$cls <- group;
    dataSet$data <- data;
  }
    RegisterData(dataSet);  

}


# here should first try to load the original data
# the data in the memory could be changed
GetGroupNames <- function(dataName, meta="NA"){
    dataSet <- readDataset(dataName);
    
    if(meta == "NA"){
        return(levels(factor(dataSet$meta[,1])));
    }else{
        return(levels(factor(dataSet$meta[,meta])));
    }

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

RemoveMissingPercent <- function(dataName="", percent=perct){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  data.annotated <- readDataQs("data.annotated.qs", paramSet$anal.type, dataName);
  int.mat <- data.annotated;
  good.inx <- apply(is.na(int.mat), 1, sum)/ncol(int.mat)<percent;
  data.annotated <- as.data.frame(int.mat[good.inx, , drop=FALSE]);
  if(sum(!good.inx)>0){
    msgSet$current.msg <- paste(sum(!good.inx), " variables were removed for threshold", round(100*percent, 2), "percent.");
  }
  saveDataQs(data.annotated, "data.annotated.qs", paramSet$anal.type, dataName);
  return(RegisterData(dataSet));
}


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
    new.mat<-int.mat[,good.inx, drop=FALSE];
    current.msg <- c(current.msg ,"Variables with missing values were excluded.");
    
  }else if(method=="min"){
    new.mat<- ReplaceMissingByLoD(int.mat);
    current.msg <- c(current.msg, "Missing variables were replaced by LoDs (1/5 of the min positive value for each variable)");
  }else if(method=="colmin"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-min(x,na.rm=T)/2;
      }
      x;
    });
    current.msg <- c(current.msg,"Missing variables were replaced by 1/2 of min values for each feature column.");
  }else if (method=="mean"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-mean(x,na.rm=T);
      }
      x;
    });
    current.msg <- c(current.msg,"Missing variables were replaced with the mean value for each feature column.");
  }else if (method == "median"){
    new.mat<-apply(int.mat, 1, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-median(x,na.rm=T);
      }
      x;
    });
    current.msg <- c(current.msg,"Missing variables were replaced with the median for each feature column.");
  }else{
    if(method == "knn_var"){
      new.mat<-t(impute::impute.knn(int.mat)$data);
    }else if(method == "knn_smp"){
      new.mat<-impute::impute.knn(data.matrix(t(int.mat)))$data;
    }else{
      if(method == "bpca"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="bpca", center=T)@completeObs;
      }else if(method == "ppca"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="ppca", center=T)@completeObs;
      }else if(method == "svdImpute"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="svdImpute", center=T)@completeObs;
      }
    }
    current.msg <- c(current.msg, paste("Missing variables were imputated using", toupper(method)));
  }
  msgSet$current.msg <- current.msg;  
  saveSet(msgSet, "msgSet");      

  data.missed <- as.data.frame(new.mat);
  rownames(data.missed) <- row.nms;
  saveDataQs(data.missed, "data.missed.qs", paramSet$anal.type, dataName);
  return(RegisterData(dataSet));
}


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
  dataSet$data <- data;
 
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

CheckMetaIntegrity <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;

  sel.nms <- names(mdata.all)

  msgSet <- readSet(msgSet, "msgSet");
  data.list <- list()
  cnms <- list()
  metas <- list();
  for(i in 1:length(sel.nms)){
    dat = readDataset(sel.nms[i])
    cnms[[i]] <- colnames(dat$data.norm);
    metas[[i]] <- as.vector(dat$meta[,1]);
  }

  if(length(metas) == 0){
    msgSet$current.msg <- paste0('Please make sure row(s) corresponding to meta-data start with "#CLASS" or to include a metadata file.' );
    saveSet(msgSet, "msgSet");
    return(0)
  }

  for(i in 1:length(sel.nms)){
    if(length(unique(metas[[i]]))>2){
        msgSet$current.msg <- "For meta-data analysis, make sure the meta-data is composed of exactly two different groups";
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
    return(1)

}

#'Read individual dataset for meta-analysis.
#'@description parse gene expression dataset into R object.
#'@param fileName File name of data table.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
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
        cls.lbls <- data[inx, -1];
        # test NA
        na.inx <- is.na(cls.lbls);
        cls.lbls[na.inx] <- "NA";
        cls.lbls <- ClearFactorStrings(cls.nm, cls.lbls);
        
        meta.info[[cls.nm]] <- cls.lbls;
      }
      meta.info <- data.frame(meta.info);
      dataSet$meta <- meta.info
      data <- data[-cls.inx,];
      smpl.nms <- .cleanNames(colnames(data)[-1], "sample_name");
      rownames(dataSet$meta) <- smpl.nms;
      dataSet$fst.cls <- dataSet$meta[,1]
      if(ncol(meta.info)>1){
      dataSet$sec.cls <- dataSet$meta[,2]
      }
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

  dataSet$data <- data;
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
  saveSet(paramSet,"paramSet");
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  saveSet(paramSet, "paramSet");
  return(RegisterData(dataSet));
}


PerformDEAnalMeta <- function(filenm, alg="ttest", meta=1, p.lvl=0.05, fc.lvl=0, nonpar=FALSE){
    res <- DoStatComparison(filenm, alg, meta, "NA", "NA", "NA", p.lvl, fc.lvl, nonpar=FALSE);
    return(res);
}

DoStatComparison <- function(dataName, alg="ttest", meta=1, selected, meta.vec, normOpt, p.lvl=0.05, fc.lvl=0, nonpar=FALSE){
  if(meta == "null"){
    meta = 1;
  }
  
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  
  data.filtered <- readDataQs("data.filtered.qs", paramSet$anal.type, dataName);
  if(normOpt != "none" && alg == "limma"){
    data.comparison <- NormalizingDataOmics(data.filtered, normOpt, "NA", "NA");
  }else{
    data.comparison <- data.filtered;
  }

  if(alg == "deseq2" || alg == "edger"){
    if(dataSet$isValueNormalized == "false"){
      data.comparison <- round(data.comparison);
    }
  }
  
  
  if(dataSet$de.method == alg && dataSet$de.norm == normOpt){
    return(UpdateDE(dataName, p.lvl, fc.lvl));
  }
  
  if(selected == "NA"){ # process page
    if(meta == ""){
      meta <- 1;
    }
    metavec <- dataSet$meta[,meta];
    sel <- unique(metavec);
  }else{
    metavec <- dataSet$meta[,meta];
    sel <- strsplit(selected, "; ")[[1]];
  }
  
  dataSet$meta$newcolumn <- metavec;
  metadf <- dataSet$meta;

  sel_meta1 = metadf[which(metadf[,"newcolumn"] %in% sel[1]),];
  sel_meta2 = metadf[which(metadf[,"newcolumn"] %in% sel[2]),];
  nms1 <- rownames(sel_meta1);
  nms2 <- rownames(sel_meta2);
  sel_meta_more_than_2 = metadf[which(metadf[,"newcolumn"] %in% sel),];
  nms <- rownames(sel_meta_more_than_2);

  sel.meta <- "newcolumn";
  trimmed.data <-  as.matrix(data.comparison[,which(colnames(data.comparison) %in% nms)]);
  trimmed.meta <- dataSet$meta[,sel.meta][which(rownames(dataSet$meta) %in% nms)];
  trimmed.meta <- make.names(trimmed.meta);
  #if(min(trimmed.data) < 0){
  #  trimmed.data = trimmed.data + abs(min(trimmed.data));
  #}
  cls <- as.factor(trimmed.meta); 


  if(alg =="limma"){
    res <- performLimmaMeta(trimmed.data, cls, "newcolumn");
  }else if(alg=="edger"){
    res <- performEdgeRMeta(trimmed.data, cls);
  }else if(alg =="deseq2"){
    performDeseq2Meta(trimmed.data)
    .perform.computing();
    dataSet <- .save.deseq.res();
    res <- dataSet$comp.res;
  }
  colnames(res) <-  c("stat", "P.Value", "adj.P.Val");
  
  res <- res[order(res[,2], decreasing=FALSE),];

  res <- as.matrix(res);
  de <- res;

  dataSet$de.norm <- normOpt;
  dataSet$de.method <- alg;
  dataSet$comp.res <- de;
  
  RegisterData(dataSet);
  
  return(UpdateDE(dataName, p.lvl, fc.lvl));
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
  hit.inx <- abs(as.numeric(res.sig[, "stat"])) > fc.lvl #foldchange
  
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

SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}


# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but variance/SE
ParetoNorm<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but variance/SE
MeanCenter<-function(x){
  x - mean(x);
}

# normalize to zero mean but variance/SE
RangeNorm<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}


performLimmaMeta <-function(trimmed.data, cls, sel.meta="newcolumn"){
  require(edgeR);
  inx = 0;
  myargs <- list();
  grp.nms <- levels(cls);
  
  for(m in 1:(length(grp.nms)-1)){
    for(n in (m+1):length(grp.nms)){
      inx <- inx + 1;
      myargs[[inx]] <- paste(grp.nms[m], "-", grp.nms[n], sep="");
    }
  }
  
  design <- model.matrix(~0 + cls) # no intercept
  colnames(design) <- levels(cls);
  myargs[["levels"]] <- design;
  contrast.matrix <- do.call(makeContrasts, myargs);
  vfit <- lmFit(trimmed.data, design);
  vfit <- contrasts.fit(vfit, contrasts=contrast.matrix);
  vfit <- eBayes(vfit);
  topFeatures <- topTable(vfit, number = Inf, adjust.method = "fdr");

  if(length(unique(cls)) == 2){
    res = data.frame(stat=topFeatures[,"logFC"], P.Value=topFeatures[,"P.Value"], adj.P.Val=topFeatures[,"adj.P.Val"])
  }else{
    res = data.frame(stat=topFeatures[,"F"], P.Value=topFeatures[,"P.Value"], adj.P.Val=topFeatures[,"adj.P.Val"])
  }
  rownames(res) <- rownames(topFeatures);
  return(res);
}


performEdgeRMeta <-function(trimmed.data, trimmed.meta){
  require(edgeR);

  y <- DGEList(counts = trimmed.data, group = trimmed.meta);
  y <- calcNormFactors(y);
  y <- estimateCommonDisp(y, verbose = FALSE);
  y <- estimateTagwiseDisp(y);
  et <- edgeR::exactTest(y);
  tt <- edgeR::topTags(et, n=nrow(y$table), adjust.method="BH", sort.by="PValue");
  res <- tt@.Data[[1]];
  colnames(res)[which(colnames(res) == "PValue")] = "P.Value";
  res <- res[,c(1,3,2,4)];
  return(res)
}

performDeseq2Meta <-function(trimmed.data, trimmed.meta){
  rownames(cls) = c();
  
  my.fun <- function(){
    suppressMessages(require(DESeq2));
    dds <- DESeqDataSetFromMatrix(countData=round(trimmed.data), colData = trimmed.meta, design = ~newcolumn)
    geoMeans = apply(counts(dds), 1, gm_mean);
    dds <- DESeq2::estimateSizeFactors(dds, geoMeans = geoMeans);
    dds <- DESeq2::DESeq(dds, test="Wald", fitType="parametric");
    res <- DESeq2::results(dds, independentFiltering = FALSE, cooksCutoff =  Inf);
    res <- as.matrix(res);
    res <- res[,c(2,5,1,3,4,6)];
    return(res);
  }
  
  dat.in <- list(data=trimmed.data, meta=met, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");
  return(1);
}
