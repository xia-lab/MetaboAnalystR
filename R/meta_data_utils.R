#'Read in individual data 
#'@description This function determines reads in user's individual data for meta-analysis.
#'@param mSetObj Input name of the created mSet Object
#'@param dataName Name of inputted dataset. 
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

ReadIndData <- function(mSetObj=NA, dataName, format="colu"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  dat <- .readDataTable(dataName);
  if(class(dat) == "try-error" || ncol(dat) == 1){
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    return(0);
  }
  
  #mSetObj$dataSet <- list();
  mSetObj$dataSet$data.orig <- dat;
  mSetObj$dataSet$format <- format;
  mSetObj$dataSet$name <- dataName;
  
  if(.on.public.web == TRUE){
    .set.mSet(mSetObj)
    return(RegisterData(mSetObj, mSetObj$dataSet));
  }else{
    RegisterData(mSetObj, mSetObj$dataSet)
    return(.set.mSet(mSetObj));
  }
}

#'Register data in R
#'@description When there are multiple datasets, record their name and save the inputted data as
#'a .RDS file to save memory. Note, the memory will only contain one mSetObj$dataSet object. By default the last one
#'will be the most recent/current dataSet object. Users can switch which data to load into memory.
#'@param mSetObj Input name of the created mSet Object
#'@param dataSet Input dataset to be registered in R. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

RegisterData <- function(mSetObj=NA, dataSet){
  
  mSetObj <- .get.mSet(mSetObj);
  dataName <- dataSet$name;
  saveRDS(dataSet, file=dataName);
  dataSet <<- dataSet; # redundant? have mSetObj$dataSet = 2 copies
  mdata.all[[dataName]] <<- 1;
  return(1);
}

#'Sanity check of individual datasets for meta-analysis
#'@description Performs a sanity check on each-uploaded dataset for meta-analysis. Briefly, this function
#'will exclude empty rows, check class labels, ensure only 2 groups are being compared within the dataset, 
#'ensure sample names are unique, remove low quality samples/features, and replace missing values. 
#'@param mSetObj Input name of the created mSet Object
#'@param dataName Input name of the dataset to perform the sanity check.  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
# 
SanityCheckIndData<-function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }else{
    dataSet <- mSetObj$dataSet
  }
  
  dat <- dataSet$data.orig;
  msg <- NULL;
  if(substring(dataSet$format,1,3)=="row"){ # sample in row
    msg <- "Samples are in rows and features in columns";
    smpl.nms <-dat[,1];
    dat[,1] <- NULL;
    cls.lbl <- dat[,1];
    conc <- dat[,-1];
    var.nms <- colnames(conc);
  }else{ # sample in col
    msg<-"Samples are in columns and features in rows.";
    var.nms <- dat[-1,1];
    dat[,1] <- NULL;
    smpl.nms <- colnames(dat);
    cls.lbl <- dat[1,];
    conc <- t(dat[-1,]);
  }
  
  dataSet$cls.orig <- cls.lbl;
  
  empty.inx <- is.na(smpl.nms) | smpl.nms == ""
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty rows</font> were detected and excluded from your data."));
    smpl.nms <- smpl.nms[!empty.inx];
    cls.lbl <-  cls.lbl[!empty.inx];
    conc <- conc[!empty.inx, ];
  }else{
    msg <- c(msg, "No empty rows were found in your data.");
  }
  
  # try to check & remove empty lines if class label is empty
  # Added by B. Han
  empty.inx <- is.na(cls.lbl) | cls.lbl == ""
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
    smpl.nms <- smpl.nms[!empty.inx];
    cls.lbl <-  cls.lbl[!empty.inx];
    conc <- conc[!empty.inx, ];
  }else{
    msg <- c(msg, "No empty labels were found in your data.");
  }
  
  if(length(unique(cls.lbl[!empty.inx])) > 2){
    msg <- c(msg, paste(c("Groups found:", unique(cls.lbl[!empty.inx])), collapse=" "));
    msg <- c(msg, "<font color=\"red\">Meta-analysis is only defined for two-group comparisions!</font>");
    current.msg <<- msg;
    return(0);
  }else{
    lvls <- as.character(unique(unlist(cls.lbl)))
    msg <- c(msg, paste("Two groups found:", lvls[1], "and", lvls[2], collapse=" "));
  }
  
  # check for uniqueness of dimension name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    msg <- c(msg, "Duplicate sample names are not allowed!");
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, "All sample names are unique.");
  }
  
  # try to remove check & remove empty line if feature name is empty
  empty.inx <- is.na(var.nms) | var.nms == "";
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty features</font> were detected and excluded from your data."));
    var.nms <- var.nms[!empty.inx];
    conc <- conc[,!empty.inx];
  }else{
    msg <- c(msg, "No empty feature names found");
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    msg <- c(msg, "Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, "All feature names are unique");
  }
  
  # now check for special characters in the data labels
  if(sum(is.na(iconv(smpl.nms)))>0){
    na.inx <- is.na(iconv(smpl.nms));
    nms <- paste(smpl.nms[na.inx], collapse="; ");
    msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, "All sample names are OK");
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    na.inx <- is.na(iconv(var.nms));
    nms <- paste(var.nms[na.inx], collapse="; ");
    msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, "All feature names are OK");
  }
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  smpl.nms <- gsub("[^[:alnum:]./_-]", "", smpl.nms);
  var.nms <- gsub("[^[:alnum:][:space:],'./_-]", "", var.nms); # allow space, comma and period
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names
  conc <- apply(conc, 2, as.numeric);
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  proc.cls <- as.factor(as.character(cls.lbl));
  
  # now need to remove low quality samples and genes
  data <- conc;
  smpl.num <- nrow(data);
  gene.num <- ncol(data);

  # remove smpls/exp with over half missing value
  good.inx<-apply(is.na(data), 1, sum)/ncol(data)<0.6;
  smpl.msg <- "";
  if(sum(!good.inx)>0){
    
    msg <- c(msg, paste(sum(!good.inx), "low quality samples(>60% missing) removed."));
    
    data <- data[good.inx,];
    if(nrow(data)/smpl.num < 0.5){
      msg <- c(msg, paste(msg, "Low quality data rejected!"));
      current.msg <<- msg;
      return(0);
    }
    
    # update meta information
    proc.cls <- proc.cls[good.inx];
  }
  
  if(ncol(data) < 4){
    msg <- c(msg, paste("The sample # (", nrow(data), ") is too small."));
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, paste("A total of", nrow(data), "samples were found."));
  }
  
  # feature with 75% NA will be removed
  gd.inx<-apply(is.na(data), 2, sum)/nrow(data) < 0.75;
  
  feat.msg <- "";
  if(sum(!gd.inx) > 0){
    data <- data[, gd.inx];
    msg <- c(msg, paste(sum(!gd.inx), "low quality features (>75% missing) removed"));
    if(ncol(data)/gene.num < 0.25){
      msg <- c(msg, paste(feat.msg, "Low quality data rejected."));
      current.msg <<- msg;
      return(0);
    }
  }
  
  # feature with 90% ZEROs will be removed
  gd.inx<-apply(data==0, 2, function(x) {sum(x, na.rm=T)})/nrow(data) < 0.9;
  
  feat.msg <- "";
  if(sum(!gd.inx) > 0){
    data <- data[, gd.inx];
    msg <- c(msg, paste(sum(!gd.inx), "low quality features (>90% zeros) removed"));
    if(ncol(data)/gene.num< 0.25){
      msg <- c(msg, paste(feat.msg, "Low quality data rejected."));
      current.msg <<- msg;
      return(0);
    }
  }
  
  if(ncol(data) < 10){ 
    msg <- c(msg, "The feature# (", ncol(data), ") is too small (<10).");
    current.msg <<- msg;
    return(0);
  }else{
    msg <- c(msg, paste("A total of", ncol(data), "features were found.", collapse=" "));
  }
  
  # replace missing values should use median for normalized data
  min.val <- min(data[data>0], na.rm=T)/10;
  data[is.na(data)] <- min.val;
  data[data<=0] <- min.val;
  
  dataSet$check.msg <- msg;
  dataSet$data.proc <- dataSet$data <- data;
  dataSet$cls.proc <- dataSet$cls <- factor(proc.cls);
  
  mSetObj$dataSet <- dataSet
  
  if(.on.public.web == TRUE){
    .set.mSet(mSetObj)
    return(RegisterData(mSetObj, dataSet));
  }else{
    RegisterData(mSetObj, dataSet)
    return(.set.mSet(mSetObj));
  }
}

#removed setcurrentdata, never used?

#'Remove data object, the current dataSet will be the last one by default 
#'@param dataName Input name of data to remove
#'@export
RemoveData <- function(dataName){
  if(!is.null(mdata.all[[dataName]])){
    mdata.all[[dataName]] <<- NULL;
  }
}

#'Select one or more datasets for meta-analysis
#'@description This function selects one or more datasets to be used for meta-analysis. 1 is used to indicate that 
#'a dataset is selected and by default, all datasets will be selected for meta-analysis.
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SelectMultiData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!exists('nm.vec')){
    AddErrMsg("No dataset is selected for analysis!");
    return(0);
  }
  
  all.nms <- names(mdata.all);
  for(nm in all.nms){
    if(nm %in% nm.vec){
      mdata.all[[nm]] <<- 1;
    }else{
      mdata.all[[nm]] <<- 0;
    }
  }
  
  if("meta_dat" %in% nm.vec){
    meta.selected <<- TRUE;
  }else{
    meta.selected <<- FALSE;
  }
  
  rm('nm.vec', envir = .GlobalEnv);
  
  if(.on.public.web==TRUE){
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#' Get all meta-analysis name data
#'@export
GetAllDataNames <- function(){
  names(mdata.all);
}

#'Perform normalization for individually-uploaded datasets for meta-analysis
#'@description This function performs normalization of individuall-uploaded datasets prior to meta-analysis.
#'@param mSetObj Input name of the created mSet Object
#'@param dataName Input the name of the individual dataset for normalization. 
#'@param norm.opt Performs log2 normalization "log", or no normalization "none". 
#'@param auto.opt Performs auto-scaling of data (1), or no (0). 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformIndNormalization <- function(mSetObj=NA, dataName, norm.opt, auto.opt){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }else{
    dataSet <- mSetObj$dataSet
  }
  
  msg <- NULL;
  data <- dataSet$data.proc;
  
  if(norm.opt != "none"){
    data <- PerformDataNormalization(data, norm.opt);
  }
  
  if(auto.opt==1){
    row.nms <- rownames(data);
    col.nms <- colnames(data);
    data <- apply(data, 2, AutoNorm);
    msg <- paste(msg, "Autoscaling performed.", collapse=" ");
    rownames(data) <- row.nms;
    colnames(data) <- col.nms;
  }
  
  dataSet$data <- data;
  dataSet$cls <- dataSet$cls.proc;
  dataSet$auto_opt <- auto.opt
  
  mSetObj$dataSet <- dataSet
  
  RegisterData(mSetObj, dataSet);
  
  AddMsg(msg);
  
  if(.on.public.web==TRUE){
    .set.mSet(mSetObj)
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
}

PerformDataNormalization <- function(data, norm.opt){
  
  msg <- NULL;
  row.nms <- rownames(data);
  col.nms <- colnames(data);
  if(norm.opt=="log"){
    data <- log2(data);
    msg <- paste(msg, "Log2 transformation.", collapse=" ");
  }else if(norm.opt=="vsn"){
    data <- limma::normalizeVSN(data);
    msg <- paste(msg, "VSN normalization.", collapse=" ");
  }else if(norm.opt=="quantile"){
    data <- preprocessCore::normalize.quantiles(data, copy=TRUE);
    msg <- paste(msg, "Quantile normalization.", collapse=" ");
  }else{
    msg <- paste("Unknown normalization: ", norm.opt, collapse=" ");
    print(msg);
    return(null);
  }
  norm.msg <<- msg;
  rownames(data) <- row.nms;
  colnames(data) <- col.nms;
  return(data);
}

#'Perform differential expression analysis using Limma for individually-uploaded data.
#'@description This function performs DE analysis of individually-uploaded data prior to meta-analysis. 
#'@param mSetObj Input name of the created mSet Object
#'@param dataName Input the name of the individual dataset for normalization. 
#'@param p.lvl Numeric, input the p-value (FDR) cutoff.
#'@param fc.lvl Numeric, input the fold-change (FC) cutoff. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PerformLimmaDE<-function(mSetObj=NA, dataName, p.lvl=0.1, fc.lvl=0.0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }else{
    dataSet <- mSetObj$dataSet
  }
  
  res.limma <- PerformLimma(t(dataSet$data), dataSet$cls);
  res.all <- GetLimmaResTable(res.limma$fit.obj);
  
  hit.inx <- abs(res.all$logFC)>= fc.lvl & res.all$adj.P.Val <= p.lvl
  
  # note, hit.inx can contain NA, not T/F
  hit.inx <- which(hit.inx);
  res <- res.all[hit.inx,];
  
  # rm .txt suffix for new names
  shortNm <- substring(dataName, 0, nchar(dataName)-4);
  fileName <- paste("SigFeatures_", shortNm, ".csv",sep="")
  write.csv(signif(res[,-1],5), file=fileName);
  
  sig.count <- nrow(res);
  non.sig.count <- nrow(res.all)-sig.count;
  
  gc();
  
  mSetObj$dataSet$deparam <- paste(c("P value cutoff:", p.lvl, "Fold-Change cutoff:", fc.lvl))
  mSetObj$dataSet$desig <- paste(c("Number of significant features:", sig.count, "Number of non-significant features:", non.sig.count))
  # record the sig gene vec
  if(.on.public.web==TRUE){
    .set.mSet(mSetObj)
    
    return(c(1, sig.count, non.sig.count));
  }else{
    return(.set.mSet(mSetObj));
  }
}

# perfor differential analysis for array/RNA seq data
# for two groups only (used for meta-analysis)
PerformLimma<-function(data, group){
  
  data <- data;
  design <- model.matrix(~-1 + group);
  fit = limma::lmFit(data, design)
  
  grps.cmp <- paste("group", levels(group)[2], " - ", "group", levels(group)[1], sep="");
  myargs <- list(grps.cmp, levels = design);
  contrast.matrix <- do.call(limma::makeContrasts, myargs);
  fit <- limma::contrasts.fit(fit, contrast.matrix)
  fit <- limma::eBayes(fit);
  gc();
  return(list(fit.obj=fit));
}

#'Get result table from eBayes fit object
#'@param fit.obj eBayes fit object to parse to a table
#'@export
GetLimmaResTable<-function(fit.obj){
  
  resTable <- limma::topTable(fit.obj, number=Inf, adjust.method="BH");
  if(!is.null(resTable$ID)){ # for older version
    rownames(resTable) <- resTable$ID;
    resTable$ID <- NULL;
  }
  return(resTable);
}

# given a gene id, plot its expression profile as box plot

#'Create a box-plot of a feature's expression pattern across the different datasets
#'@description This function plots a box-plot of the expression pattern of a user-selected feature
#'across the different datasets included in meta-analysis.
#'@param mSetObj Input name of the created mSet Object.
#'@param gene.id Input the name of the selected feature.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import lattice

PlotSelectedFeature<-function(mSetObj=NA, gene.id){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$imgSet$meta.anal$feature <- symb <- gene.id;
  imgName <- paste(gene.id, ".png", sep="");
  mSetObj$imgSet$meta.anal$plot <- imgName

  if(.on.public.web){
    load_lattice()
  }
  num <- sum(mdata.all == 1);
  # calculate width based on the dataset number
  if(num == 1){
    Cairo::Cairo(file = imgName, width=280, height=320, type="png", bg="white");
    myplot <- bwplot(metastat.meta$plot.data[gene.id,] ~ as.character(metastat.meta$cls.lbl), fill="#0000ff22",
                     xlab="Class", ylab="Expression Pattern", main=symb, scales=list(x=list(rot=30)))
  }else{
    # calculate layout
    if(num < 6){
      layout <- c(num, 1);
      height=320;
      width=160*num;
    }else{
      rn <- round(num/2);
      layout <- c(rn, 2);
      height=500;
      width=160*rn;
    }
    
    Cairo::Cairo(file = imgName, width=width, height=height, type="png", bg="white");
    data.lbl <- as.character(metastat.meta$data.lbl);
    data.lbl <- substr(data.lbl, 0, nchar(data.lbl)-4);
    
    # get counts in each data, same order as a levels
    counts <- table(data.lbl);
    # back to factor 
    data.lbl <- factor(data.lbl);
    
    # get new lbls to cut potential long names, and add sample numbers
    nlbls <- data.lbl;
    levels(nlbls) <- abbreviate(levels(nlbls),9);
    nlbls <- paste(levels(nlbls), "( n=", as.vector(counts), ")");
    # update labels
    data.lbl <- factor(data.lbl, labels=nlbls);
    # some time the transformed plot.data can switch class label, use the original data, need to be similar scale
    myplot <- bwplot(metastat.meta$plot.data[gene.id,] ~ as.character(metastat.meta$cls.lbl) | data.lbl, 
                     xlab="Datasets", ylab="Expression Pattern", main=symb, scales=list(x=list(rot=30)),
                     fill="#0000ff22", layout=layout);
  }
  
  print(myplot); 
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetMetaSanityCheckMsg <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }
  return(dataSet$check.msg);
} 

GetDataDims <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }
  data <- dataSet$data;
  dm <- dim(data);
  naNum <- sum(is.na(data));
  zoNum <- sum(data == 0);
  return(c(dm, naNum, zoNum));
} 

GetMetaGroupNames <-function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- readRDS(dataName);
  }
  return(levels(dataSet$cls));
}

######################################
## methods for merged expression data
#######################################

GlobalCutOff = list(
  logFC = 0,
  BHth = 0.05
)

# function to set up results combining individual data analysis
# as well as to prepare for GO analysis
# no return, as set global 

SetupMetaStats <- function(BHth){
  
  GlobalCutOff$BHth <<- BHth;
  #all common genes
  gene.ids <- rownames(metastat.meta$data);
  # meta.sig genes
  metade.genes <- rownames(meta.mat);
  
  # setup individual sig genes & stats
  # that overlap with meta.sig
  metastat.de <- list();
  
  pval.mat <- fc.mat <- matrix(nrow=nrow(meta.mat), ncol=sum(mdata.all==1));
  for(i in 1:length(metastat.ind)){
    de.res <- metastat.ind[[i]];
    
    hit.inx <- de.res[,2] <= BHth;
    hit.inx <- which(hit.inx); # need to get around NA
    metastat.de[[i]] <- rownames(de.res)[hit.inx];
    
    # only choose the genes that are also meta sig genes from in
    # individual analysis for display
    de.res <- de.res[metade.genes,];
    
    fc.mat[,i] <- de.res[,1];
    pval.mat[,i] <- de.res[,2];
  }
  names(metastat.de) <- names(metastat.ind);
  
  # calculate gain/loss
  deindst <- unique(unlist(metastat.de));
  gains=metade.genes[which(!(metade.genes %in% deindst))];
  losses=deindst[which(!(deindst %in% metade.genes))];
  all.de <- cbind(gene.ids %in% metade.genes, gene.ids %in% deindst);
  colnames(all.de) <- c("Meta-DE", "Individual-DE");
  vennC <- getVennCounts(all.de);
  
  # significant features from individual 
  de.len <- sapply(metastat.de, length);
  stat <- c(length(metade.genes), de.len);
  names(stat) <- c("Meta", substr(names(metastat.de), 0, nchar(names(metastat.de))-4));
  meta.stat <- list(
    stat = stat,
    de = metade.genes,
    idd = gains,
    loss = losses,
    venn = vennC
  );
  
  fc.mat <<- fc.mat;
  pval.mat <<- pval.mat;
  metastat.de <<- metastat.de;
  meta.stat <<- meta.stat;
  
  # save the result
  res <- cbind(ID=metade.genes, meta.mat);
  
  write.csv(res, file=paste("meta_sig_features_", metastat.method, ".csv", sep=""), row.names=F);
}

