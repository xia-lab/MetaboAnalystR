#'Sanity Check Data
#'@description SanityCheckData is used for data processing, and performs a basic sanity 
#'check of the uploaded content, ensuring that the data is suitable for further analysis. 
#'The function will return a message if the data has successfully passed the check
#'and is deemed suitable for further analysis. If it fails, the function will return a 0.
#'The function will perform the check directly onto the mSet$dataSet object, and must 
#'be performed immediately after reading in data. 
#'The sanity check function evaluates the accuracy of sample and class labels, data structure, 
#'deals with non-numeric values, removes columns that are constant across all samples (variance = 0), 
#'and by default replaces missing values with half of the original minimal positive value in your dataset.
#'@usage SanityCheckData(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
SanityCheckData <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  orig.data <- qs::qread("data_orig.qs");
  msg <- NULL;
  cls <- mSetObj$dataSet$orig.cls;
  mSetObj$dataSet$small.smpl.size <- 0;
  # check class info
  if(mSetObj$dataSet$cls.type == "disc"){
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      if(mSetObj$dataSet$design.type =="time"){
        msg<-c(msg, "The data is time-series data.");
      }else if(mSetObj$dataSet$design.type =="time0"){
        msg<-c(msg, "The data is time-series only data.");
      }else{
        msg<-c(msg, "The data is not time-series data.");
      }
      clsA.num <- length(levels(mSetObj$dataSet$facA));
      clsB.num <- length(levels(mSetObj$dataSet$facB));
      msg<-c(msg, paste(clsA.num, "groups were detected in samples for factor", mSetObj$dataSet$facA.lbl));
      msg<-c(msg, paste(clsB.num, "groups were detected in samples for factor", mSetObj$dataSet$facB.lbl));
    }else{
      if(mSetObj$dataSet$paired){
        msg<-c(msg,"Samples are paired.");
        # need to first set up pair information if not csv file
        if(!(mSetObj$dataSet$type=="conc" | mSetObj$dataSet$type=="specbin" | mSetObj$dataSet$type=="pktable" )){
          pairs <- ReadPairFile();
          # check if they are of the right length
          if(length(pairs)!=length(mSetObj$dataSet$orig.smp.nms)){
            AddErrMsg("Error: the total paired names are not equal to sample names.");
            return(0);
          }else{
            # matching the names of the files
            inx<-match(mSetObj$dataSet$orig.smp.nms, names(pairs));
            #check if all matched exactly
            if(sum(is.na(inx))>0){
              AddErrMsg("Error: some paired names not match the sample names.");
              return(0);
            }else{
              mSetObj$dataSet$pairs <- pairs[inx];
            }
          }
        }
        
        pairs <- mSetObj$dataSet$pairs;
        
        # check if QC samples are present
        qc.hits <- tolower(as.character(cls)) %in% "qc";
        if(sum(qc.hits) > 0){
          AddErrMsg("<font color='red'>Error: QC samples not supported in paired analysis mode.</font>");
          AddErrMsg("You can perform QC filtering using regular two-group labels.");
          AddErrMsg("Then re-upload your data (without QC samples) for paired analysis.");
          return(0);
        }else{
          pairs <- as.numeric(pairs);
        }
        
        label <- as.numeric(pairs);
        cls <- as.factor(ifelse(label>0,1,0));
        mSetObj$dataSet$pairs <- label;
        
        lev <- unique(pairs);
        uni.cl <- length(lev);
        uni.cl.abs <- uni.cl/2;             
        sorted.pairs <- sort(pairs,index=TRUE);
        
        if(!all(sorted.pairs$x==c(-uni.cl.abs:-1,1:uni.cl.abs))){
          AddErrMsg("There are some problems in paired sample labels! ");
          if(uni.cl.abs != round(uni.cl.abs)){
            duplicates <- pairs[duplicated(pairs)]
            dup.msg <- paste0("Duplicated labels:", duplicates)
            AddErrMsg(paste("The total samples must be of even number!", dup.msg));
          }else{
            AddErrMsg(paste("And class labels between ",-uni.cl.abs,
                            " and 1, and between 1 and ",uni.cl.abs,".",sep=""));
          }
          return(0);
        }else{  
          msg <- c(msg,"The labels of paired samples passed sanity check.");
          msg <- c(msg, paste("A total of", uni.cl.abs, "pairs were detected."));
          # make sure paired samples are sorted 1:n/2 and -1:-n/2
          
          x<-sorted.pairs$ix[(uni.cl.abs+1):uni.cl]
          y<-sorted.pairs$ix[uni.cl.abs:1]
          index<-as.vector(cbind(x,y));
          cls<-cls[index];
          pairs <- pairs[index];
          mSetObj$dataSet$pairs <- pairs;
          mSetObj$dataSet$orig.cls <- cls;
          orig.data<- orig.data[index,];
          qs::qsave(orig.data, file="data_orig.qs");
        }
      }else{
        msg <- c(msg,"Samples are not paired.");
      }
      
      # checking if too many groups but a few samples in each group
      cls.lbl <- mSetObj$dataSet$orig.cls;
      # need to exclude QC or blank
      qb.inx <- tolower(cls.lbl) %in% c("qc", "blank");
      if(sum(qb.inx) > 0){
          cls.lbl <- as.factor(as.character(cls.lbl[!qb.inx])); # make sure drop level
      }
      min.grp.size <- min(table(cls.lbl));
      cls.num <- length(levels(cls.lbl));
      if(cls.num/min.grp.size > 3){
        mSetObj$dataSet$small.smpl.size <- 1;
        msg <- c(msg, "<font color='red'>Too many groups with very small number of replicates!</font>");
        msg <- c(msg, "<font color='red'>Only a subset of methods will be available for analysis!</font>");
      }
      
      msg <- c(msg, paste(cls.num, "groups were detected in samples."));
      mSetObj$dataSet$cls.num <- cls.num;
      mSetObj$dataSet$min.grp.size <- min.grp.size;
    }
    
    #samples may not be sorted properly, need to do some sorting at the beginning 
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      nfacA <- mSetObj$dataSet$facA;
      nfacB <- mSetObj$dataSet$facB;
      if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
        # determine time factor and should order first by subject then by each time points
        if(tolower(mSetObj$dataSet$facA.lbl) == "time"){ 
          time.fac <- nfacA;
          exp.fac <- nfacB;
        }else{
          time.fac <- nfacB;
          exp.fac <- nfacA;
        }
        # update with new index
        ord.inx <- order(exp.fac);
      }else{
        ord.inx <- order(nfacA);
      }
      mSetObj$dataSet$orig.cls <- mSetObj$dataSet$orig.cls[ord.inx];
      mSetObj$dataSet$facA <- mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA[ord.inx];
      mSetObj$dataSet$facB <- mSetObj$dataSet$orig.facB <- mSetObj$dataSet$facB[ord.inx];
      orig.data <- orig.data[ord.inx,];
      qs::qsave(orig.data, file="data_orig.qs");
    }else{
      ord.inx <- order(mSetObj$dataSet$orig.cls);
      mSetObj$dataSet$orig.cls <- cls[ord.inx];
      orig.data <- orig.data[ord.inx, , drop=FALSE];
      qs::qsave(orig.data, file="data_orig.qs");
      if(mSetObj$dataSet$paired){
        mSetObj$dataSet$pairs <- mSetObj$dataSet$pairs[ord.inx];
      }
    }
  }
  msg<-c(msg,"Only English letters, numbers, underscore, hyphen and forward slash (/) are allowed.");
  msg<-c(msg,"<font color=\"orange\">Other special characters or punctuations (if any) will be stripped off.</font>");
  
  int.mat <- orig.data;
  
  if(ncol(int.mat)==1){
    if(anal.type=="roc"){
        mSetObj$dataSet$roc_cols <- 1;
    }else{
      AddErrMsg("<font color='red'>One-column data is only supported for biomarker analysis.</font>");
      return(0);
    }
  }else{
    mSetObj$dataSet$roc_cols <- 2;
  }

  # check numerical matrix
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  
  num.mat <- apply(int.mat, 2, as.numeric)
  
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
  
  # check for columns with all constant (var =0)
  varCol <- apply(int.mat, 2, var, na.rm=T);
  
  constCol <- (varCol == 0 | is.na(varCol));
  constNum <- sum(constCol, na.rm=T);
  if(constNum > 0){
    msg<-c(msg, paste("<font color=\"red\">", constNum, "features with a constant or single value across samples were found and deleted.</font>"));
    int.mat <- int.mat[,!constCol, drop=FALSE];
  }
  
  # check zero, NA values
  totalCount <- nrow(int.mat)*ncol(int.mat);
  naCount <- sum(is.na(int.mat));
  naPercent <- round(100*naCount/totalCount,1)
  
  msg<-c(msg, paste("A total of ", naCount, " (", naPercent, "%) missing values were detected.", sep=""));
  msg<-c(msg, "<u>By default, missing values will be replaced by 1/5 of min positive values of their corresponding variables</u>",
         "Click the <b>Skip</b> button if you accept the default practice;",
         "Or click the <b>Missing value imputation</b> to use other methods.");
  
  qs::qsave(as.data.frame(int.mat), "preproc.qs");
  mSetObj$dataSet$proc.cls <- mSetObj$dataSet$cls <- mSetObj$dataSet$orig.cls;
  
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$proc.facA <- mSetObj$dataSet$orig.facA;
    mSetObj$dataSet$proc.facB <- mSetObj$dataSet$orig.facB;
  }
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$read.msg, msg);
  if(!.on.public.web){
        print(c("Successfully passed sanity check!", msg))
  }
  return(.set.mSet(mSetObj));
}

#'Replace missing or zero values
#'@description This function will replace zero/missing values by half of the smallest
#'positive value in the original dataset.  
#'This method will be called after all missing value imputations are conducted.
#'Also, it directly modifies the mSet$dataSet$proc if executed after normalization,
#'or the mSet$dataSet$norm if before normalization.
#'@usage ReplaceMin(mSetObj=NA) 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
ReplaceMin <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  #Reset to default
  mSetObj$dataSet$proc <- mSetObj$dataSet$filt <- mSetObj$dataSet$edit <- NULL;
  
  # replace zero and missing values using Detection Limit for each variable 
  preproc <- qs::qread("preproc.qs");
  int.mat <- ReplaceMissingByLoD(preproc);  
    
  # note, this is last step of processing, also save to proc
  mSetObj$dataSet$proc <- as.data.frame(int.mat);
  mSetObj$msgSet$replace.msg <- paste("Zero or missing values were replaced by 1/5 of the min positive value for each variable.");
  invisible(gc()); # suppress gc output

  return(.set.mSet(mSetObj));
}

#'Data processing: remove variables with missing values
#'@description Remove variables based upon a user-defined percentage cut-off of missing values.
#'If a user specifies a threshold of 20% (0.2), it will remove variables that are missing
#'in at least 20% of all samples.
#'@usage RemoveMissingPercent(mSetObj, percent)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param percent Input the percentage cut-off you wish to use. For instance, 50 percent is represented by percent=0.5. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
RemoveMissingPercent <- function(mSetObj=NA, percent=perct){

    mSetObj <- .get.mSet(mSetObj);
    if(!.on.public.web & !is.null(mSetObj$dataSet$norm)){    
        int.mat <- mSetObj$dataSet$norm;
        good.inx <- apply(is.na(int.mat), 2, sum)/nrow(int.mat)<percent;
        mSetObj$dataSet$norm <- as.data.frame(int.mat[,good.inx, drop=FALSE]);
    }else{  
        int.mat <- qs::qread("preproc.qs");
        good.inx <- apply(is.na(int.mat), 2, sum)/nrow(int.mat)<percent;
        preproc <- as.data.frame(int.mat[,good.inx, drop=FALSE]);
        qs::qsave(preproc, "preproc.qs");
    }
    mSetObj$msgSet$replace.msg <- c(mSetObj$msgSet$replace.msg, paste(sum(!good.inx), "variables were removed for threshold", round(100*percent, 2), "percent."));
    return(.set.mSet(mSetObj));
}

#'Data processing: Replace missing variables
#'@description Replace missing variables by min/mean/median/KNN/BPCA/PPCA/svdImpute.
#'@usage ImputeMissingVar(mSetObj, method)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Select the option to replace missing variables, either 
#'replacement based on the minimum ("min), the mean ("mean"), or the median ("median") value of each feature columns,
#'or several options to impute the missing values, using k-nearest neighbour ("KNN"), probabilistic PCA ("PPCA"), 
#'Bayesian PCA ("BPCA") method, or Singular Value Decomposition ("svdImpute") 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
ImputeMissingVar <- function(mSetObj=NA, method="min"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  int.mat <- qs::qread("preproc.qs");
  new.mat <- NULL;
  msg <- mSetObj$msgSet$replace.msg;
  
  if(method=="exclude"){
    good.inx<-apply(is.na(int.mat), 2, sum)==0
    new.mat<-int.mat[,good.inx, drop=FALSE];
    msg <- c(msg,"Variables with missing values were excluded.");

  }else if(method=="min"){
    new.mat<- ReplaceMissingByLoD(int.mat);
    msg <- c(msg,"Missing variables were replaced by LoDs (1/5 of the min positive value for each variable)");
  }else if(method=="colmin"){
    new.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-min(x,na.rm=T)/2;
      }
      x;
    });
    msg <- c(msg,"Missing variables were replaced by 1/2 of min values for each feature column.");
  }else if (method=="mean"){
    new.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-mean(x,na.rm=T);
      }
      x;
    });
    msg <- c(msg,"Missing variables were replaced with the mean value for each feature column.");
  }else if (method == "median"){
    new.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-median(x,na.rm=T);
      }
      x;
    });
    msg <- c(msg,"Missing variables were replaced with the median for each feature column.");
  }else{
    if(method == "knn_var"){
      new.mat<-t(impute::impute.knn(t(int.mat))$data);
    }else if(method == "knn_smp"){
      new.mat<-impute::impute.knn(data.matrix(int.mat))$data;
    }else{
      if(method == "bpca"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="bpca", center=T)@completeObs;
      }else if(method == "ppca"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="ppca", center=T)@completeObs;
      }else if(method == "svdImpute"){
        new.mat<-pcaMethods::pca(int.mat, nPcs =5, method="svdImpute", center=T)@completeObs;
      }
    }
    msg <- c(msg, paste("Missing variables were imputated using", toupper(method)));
  }

  mSetObj$dataSet$proc <- as.data.frame(new.mat);
  mSetObj$msgSet$replace.msg <- msg;
  return(.set.mSet(mSetObj))
}


#'Methods for non-specific filtering of variables
#'@description This is a function that filters the dataset, dependent on the user-specified method
#'for filtering. The function applies a filtering method, ranks the variables within the dataset,
#'and removes variables based on its rank. The final dataset should contain no more than
#'than 5000 variables for effective computing. 
#'@usage FilterVariable(mSetObj=NA, filter, qcFilter, rsd)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param filter Select the filter option, "rsd" which is the relative standard deviation, "nrsd" which
#'is the non-parametric relative standard deviation, "mean" which is the mean, "sd" which is the standard
#'deviation, "mad" which is the median absolute deviation, or "iqr" which is the interquantile range.
#'@param qcFilter Filter the variables based on QC samples - True (T), or use non-QC based filtering - False (F).  
#'@param rsd Define the relative standard deviation cut-off. Variables with a RSD greater than this number
#'will be removed from the dataset. It is only necessary to specify this argument if qcFilter is True (T). 
#'Otherwise, it will not be used in the function. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

FilterVariable <- function(mSetObj=NA, filter, qcFilter, rsd){
  
  mSetObj <- .get.mSet(mSetObj);

  #Reset to default
  mSetObj$dataSet$filt <- NULL; 
  
  int.mat <- as.matrix(mSetObj$dataSet$proc);
  cls <- mSetObj$dataSet$proc.cls;
  
  # save a copy
  mSetObj$dataSet$filt.cls <- cls;
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$filt.facA <- mSetObj$dataSet$proc.facA; 
    mSetObj$dataSet$filt.facB <- mSetObj$dataSet$proc.facB; 
  }
  
  msg <- "";
  if(qcFilter == "T"){
    rsd <- rsd/100;
    # need to check if QC exists
    qc.hits <- tolower(as.character(cls)) %in% "qc";
    if(sum(qc.hits) > 2){ # require at least 3 QC for RSD
      qc.mat <- int.mat[qc.hits,];
      sds <- apply(qc.mat, 2, sd, na.rm=T);
      mns <- apply(qc.mat, 2, mean, na.rm=T);
      rsd.vals <- abs(sds/mns);  
      gd.inx <- rsd.vals < rsd;
      int.mat <- int.mat[,gd.inx];
      msg <- paste("Removed ", sum(!gd.inx), " features based on QC RSD values. QC samples are still kept. You can remove them later.");
    }else if(sum(qc.hits) > 0){
      AddErrMsg("RSD requires at least 3 QC samples, and only non-QC based filtering can be applied.");
      return(0);
    }else{
      AddErrMsg("No QC Samples (with class label: QC) found.  Please use non-QC based filtering.");
      return(0);
    }
  }
  
  feat.num <- ncol(int.mat);
  feat.nms <- colnames(int.mat);
  nm <- NULL;
  if(filter == "none" && feat.num < 5000){ # only allow for less than 4000
    remain <- rep(TRUE, feat.num);
    msg <- paste(msg, "No non-QC based data filtering was applied");
  }else{
    if (filter == "rsd" ){
      sds <- apply(int.mat, 2, sd, na.rm=T);
      mns <- apply(int.mat, 2, mean, na.rm=T);
      filter.val <- abs(sds/mns);
      nm <- "Relative standard deviation";
    }else if (filter == "nrsd" ){
      mads <- apply(int.mat, 2, mad, na.rm=T);
      meds <- apply(int.mat, 2, median, na.rm=T);
      filter.val <- abs(mads/meds);
      nm <- "Non-paramatric relative standard deviation";
    }else if (filter == "mean"){
      filter.val <- apply(int.mat, 2, mean, na.rm=T);
      nm <- "mean";
    }else if (filter == "sd"){
      filter.val <- apply(int.mat, 2, sd, na.rm=T);
      nm <- "standard deviation";
    }else if (filter == "mad"){
      filter.val <- apply(int.mat, 2, mad, na.rm=T);
      nm <- "Median absolute deviation";
    }else if (filter == "median"){
      filter.val <- apply(int.mat, 2, median, na.rm=T);
      nm <- "median";
    }else{ # iqr
      filter.val <- apply(int.mat, 2, IQR, na.rm=T);
      nm <- "Interquantile Range";
    }
    
    # get the rank of the filtered variables
    rk <- rank(-filter.val, ties.method='random');

    var.num <- ncol(int.mat);
    if(var.num < 250){ # reduce 5%
      remain <- rk < var.num*0.95;
      msg <- paste(msg, "Further feature filtering based on", nm);
    }else if(ncol(int.mat) < 500){ # reduce 10%
      remain <- rk < var.num*0.9;
      msg <- paste(msg, "Further feature filtering based on", nm);
    }else if(ncol(int.mat) < 1000){ # reduce 25%
      remain <- rk < var.num*0.75;
      msg <- paste(msg, "Further feature filtering based on", nm);
    }else{ # reduce 40%, if still over 5000, then only use top 5000
      remain <- rk < var.num*0.6;
      msg <- paste(msg, "Further feature filtering based on", nm);

      if(mSetObj$analSet$type == "mummichog"){
        max.allow <- 7500;
      }else if(mSetObj$analSet$type == "power"){
        max.allow <- 5000;
      }else{
        max.allow <- 2500;
      }

      if(sum(remain) > max.allow){
        remain <-rk < max.allow;
        msg <- paste(msg, paste("Reduced to", max.allow, "features based on", nm));
      }
    }
  }

  mSetObj$dataSet$filt <- int.mat[, remain];
  mSetObj$msgSet$filter.msg <- msg;
  AddMsg(msg);

  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetGroupNumber<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(length(levels(mSetObj$dataSet$cls)));
}

#'Check if the sample size is small
#'@description Returns whether or not the sanity check found that there were too many
#'groups in the dataset containing too few samples. It will return a 0 if the data passes the check,
#'or will return a 1 if the data does not. 
#'@usage IsSmallSmplSize(mSetObj=NA) 
#'@param mSetObj Input name of the created mSet Object
#'@export
#'
IsSmallSmplSize<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  print(mSetObj$dataSet$small.smpl.size);
  return(.set.mSet(mSetObj));
}

GetMinGroupSize<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$min.grp.size);
}

IsDataContainsNegative<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$containsNegative);
}

# users can manually update sample names
UpdateFeatureName<-function(mSetObj=NA, old.nm, new.nm){
    mSetObj <- .get.mSet(mSetObj);
    if(!is.null(mSetObj$dataSet[["orig"]])){
        orig.data <- qs::qread("data_orig.qs");
        orig.data <- .update.feature.nm(orig.data, old.nm, new.nm);
        qs::qsave(orig.data, file="data_orig.qs");
    }

    if(!is.null(mSetObj$dataSet[["proc"]])){
        mSetObj$dataSet$proc <- .update.feature.nm(mSetObj$dataSet$proc, old.nm, new.nm);
        if(!is.null(mSetObj$dataSet[["filt"]])){
            mSetObj$dataSet$filt <- .update.feature.nm(mSetObj$dataSet$filt, old.nm, new.nm);
        }
    }

    if(!is.null(mSetObj$dataSet[["norm"]])){
        mSetObj$dataSet$norm <- .update.feature.nm(mSetObj$dataSet$norm, old.nm, new.nm);
    }
   return(.set.mSet(mSetObj));
}

.update.feature.nm<-function(dat, old.nm, new.nm){
    hit.inx <- match(old.nm, colnames(dat));
    if(!is.na(hit.inx)){
        colnames(dat)[hit.inx] <- new.nm; 
    }
    return(dat);
}