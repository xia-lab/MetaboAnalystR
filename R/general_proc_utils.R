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
#'@export
#'
SanityCheckData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

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
          if(length(pairs)!=nrow(mSetObj$dataSet$orig)){
            AddErrMsg("Error: the total paired names are not equal to sample names.");
            return(0);
          }else{
            # matching the names of the files
            inx<-match(rownames(mSetObj$dataSet$orig), names(pairs));
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
        mSetObj$dataSet$orig <- mSetObj$dataSet$orig;
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
            AddErrMsg("The total samples must be of even number!");
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
          mSetObj$dataSet$orig <- mSetObj$dataSet$orig[index,];
        }
      }else{
        msg <- c(msg,"Samples are not paired.");
      }
      
      # checking if too many groups but a few samples in each group
      cls.lbl <- mSetObj$dataSet$orig.cls;
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
      mSetObj$dataSet$orig <- mSetObj$dataSet$orig[ord.inx, ];
      mSetObj$dataSet$facA <- mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA[ord.inx];
      mSetObj$dataSet$facB <- mSetObj$dataSet$orig.facB <- mSetObj$dataSet$facB[ord.inx];
    }else{
      ord.inx <- order(mSetObj$dataSet$orig.cls);
      mSetObj$dataSet$orig.cls <- cls[ord.inx];
      mSetObj$dataSet$orig <- mSetObj$dataSet$orig[ord.inx, ];
      if(mSetObj$dataSet$paired){
        mSetObj$dataSet$pairs <- mSetObj$dataSet$pairs[ord.inx];
      }
    }
  }
  msg<-c(msg,"Only English letters, numbers, underscore, hyphen and forward slash (/) are allowed.");
  msg<-c(msg,"<font color=\"orange\">Other special characters or punctuations (if any) will be stripped off.</font>");
  
  int.mat <- mSetObj$dataSet$orig;
  
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
    int.mat <- int.mat[,!constCol];
  }
  
  # check zero, NA values
  totalCount <- nrow(int.mat)*ncol(int.mat);
  naCount <- sum(is.na(int.mat));
  naPercent <- round(100*naCount/totalCount,1)
  
  msg<-c(msg, paste("A total of ", naCount, " (", naPercent, "%) missing values were detected.", sep=""));
  msg<-c(msg, "<u>By default, these values will be replaced by a small value.</u>",
         "Click <b>Skip</b> button if you accept the default practice",
         "Or click <b>Missing value imputation</b> to use other methods");
  
  # obtain original half of minimal positive value (threshold)
  minConc<-min(int.mat[int.mat>0], na.rm=T)/2;
  
  mSetObj$dataSet$minConc <- minConc;
  mSetObj$dataSet$preproc <- as.data.frame(int.mat);

  mSetObj$dataSet$proc.cls <- mSetObj$dataSet$cls <- mSetObj$dataSet$orig.cls;
  
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$proc.facA <- mSetObj$dataSet$orig.facA;
    mSetObj$dataSet$proc.facB <- mSetObj$dataSet$orig.facB;
  }
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$read.msg, msg);
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(1)
  }
  
  print(c("Successfully passed sanity check!", msg))
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
#'@export
#'
ReplaceMin <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  #Reset to default
  mSetObj$dataSet$proc <- mSetObj$dataSet$filt <- mSetObj$dataSet$edit <- mSetObj$dataSet$prenorm <- NULL;
  
  int.mat <- as.matrix(mSetObj$dataSet$preproc)
  minConc <- mSetObj$dataSet$minConc;
    
  # replace zero and missing values
  # we leave nagative values unchanged! ? not sure if this is the best way
  int.mat[int.mat==0 | is.na(int.mat)] <- minConc;
    
  # note, this is last step of processing, also save to proc
  mSetObj$dataSet$proc <- as.data.frame(int.mat);
  mSetObj$msgSet$replace.msg <- paste("Zero or missing variables were replaced with a small value:", minConc);
  rm(int.mat);
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
#'@export
#'
RemoveMissingPercent <- function(mSetObj=NA, percent=perct){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!.on.public.web){
    if(!is.null(mSetObj$dataSet$norm)){    
      int.mat <- mSetObj$dataSet$norm
      minConc <- min(int.mat[int.mat>0], na.rm=T)/2;
      good.inx <- apply(is.na(int.mat), 2, sum)/nrow(int.mat)<percent;
      mSetObj$dataSet$norm <- as.data.frame(int.mat[,good.inx]);
      
      # need to update cls labels
      mSetObj$msgSet$replace.msg <- c(mSetObj$msgSet$replace.msg, paste(sum(!good.inx), "variables were removed for threshold", round(100*percent, 2), "percent."));
      return(.set.mSet(mSetObj));
      
    }else{  
      int.mat <- mSetObj$dataSet$preproc
      minConc <- mSetObj$dataSet$minConc;
      good.inx <- apply(is.na(int.mat), 2, sum)/nrow(int.mat)<percent;
      mSetObj$dataSet$preproc <- as.data.frame(int.mat[,good.inx]);
      
      # need to update cls labels
      mSetObj$msgSet$replace.msg <- c(mSetObj$msgSet$replace.msg, paste(sum(!good.inx), "variables were removed for threshold", round(100*percent, 2), "percent."));
      return(.set.mSet(mSetObj));
    }
  }else{
    int.mat <- mSetObj$dataSet$preproc
    minConc <- mSetObj$dataSet$minConc;
    good.inx <- apply(is.na(int.mat), 2, sum)/nrow(int.mat)<percent;
    mSetObj$dataSet$preproc <- as.data.frame(int.mat[,good.inx]);
    
    # need to update cls labels
    mSetObj$msgSet$replace.msg <- c(mSetObj$msgSet$replace.msg, paste(sum(!good.inx), "variables were removed for threshold", round(100*percent, 2), "percent."));
    
    return(.set.mSet(mSetObj));
  }
}

#'Data processing: Replace missing variables
#'@description Replace missing variables by min/mean/median/KNN/BPCA/PPCA/svdImpute.
#'@usage ImputeVar(mSetObj, method)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Select the option to replace missing variables, either 
#'replacement based on the minimum ("min), the mean ("mean"), or the median ("median") value of each feature columns,
#'or several options to impute the missing values, using k-nearest neighbour ("KNN"), probabilistic PCA ("PPCA"), 
#'Bayesian PCA ("BPCA") method, or Singular Value Decomposition ("svdImpute") 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ImputeVar <- function(mSetObj=NA, method="min"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  int.mat <- mSetObj$dataSet$preproc;
  new.mat <- NULL;
  msg <- mSetObj$msgSet$replace.msg;
  
  if(method=="exclude"){
    good.inx<-apply(is.na(int.mat), 2, sum)==0
    new.mat<-int.mat[,good.inx];
    msg <- c(msg,"Variables with missing values were excluded.");
  }else if(method=="min"){
    minConc<-min(int.mat[int.mat>0], na.rm=T)/2;
    int.mat[int.mat==0 | is.na(int.mat)] <- minConc;
    new.mat <- int.mat;
    msg <- c(msg,"Variables with missing values were replaced with a small value.");
  }else if (method=="colmin"){
    new.mat<-apply(int.mat, 2, function(x){
      if(sum(is.na(x))>0){
        x[is.na(x)]<-min(x,na.rm=T)/2;
      }
      x;
    });
    msg <- c(msg,"Missing variables were replaced with the half of minimum values for each feature column.");
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
    if(method == "knn"){
      #print("loading for KNN...");
      new.mat<-t(impute::impute.knn(t(int.mat))$data);
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

#'Data processing: Dealing with negative values
#'@description Operates on dataSet$proc after dealing with missing values
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Input the method to clear negatives
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ClearNegatives <- function(mSetObj=NA, method="abs"){
  mSetObj <- .get.mSet(mSetObj);
  int.mat <- as.matrix(mSetObj$dataSet$proc)
  
  if(mSetObj$dataSet$containsNegative){
    if(method == "min"){
      int.mat[int.mat < 0] <- mSetObj$dataSet$minConc;
      msg <- paste("Negative variables were replaced with a small value:", mSetObj$dataSet$minConc);
    }else if(method =="abs"){
      int.mat <- abs(int.mat);
      msg <- paste("Negative variables were replaced with their absolute values");
    }else{ # exclude
      good.inx <- apply(int.mat<0, 2, sum)==0
      new.mat <- int.mat[,good.inx];
      msg <- paste("Columns contains negative variables were excluded");
    }
    
    mSetObj$dataSet$containsNegative <- 0;
    mSetObj$msgSet$replace.msg <- c(mSetObj$msgSet$replace.msg, msg);
    mSetObj$dataSet$proc <- as.data.frame(int.mat);
    return(.set.mSet(mSetObj));
  }
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
  mSetObj$dataSet$filt <- mSetObj$dataSet$prenorm <- NULL; 
  
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
    }else{ # reduce 40%, if still over 8000, then only use top 5000
      remain <- rk < var.num*0.6;
      msg <- paste(msg, "Further feature filtering based on", nm);
      if(sum(remain) > 8000){
        remain <-rk < 8000;
        msg <- paste(msg, "Reduced to 8000 features based on", nm);
      }
    }
  }

  mSetObj$dataSet$filt <- int.mat[, remain];
  mSetObj$msgSet$filter.msg <- msg;
  AddMsg(msg);

  return(.set.mSet(mSetObj));
}


#'Group peak list
#'@description Group peaks from the peak list based on position
#'using the XCMS grouping algorithm (align peaks wrt, rt, and mz).
#'For NMR peaks, need to change ppm -> mz and add dummy rt.
#'If the data is 2-column MS, first need to add dummy rt.
#'If the data is 3-column MS, the data can be used directly.
#'The default mzwid for MS is 0.25 m/z, and for NMR is 0.03 ppm.
#'The default bw is 30 for LCMS, and 5 for GCMS.
#'@usage GroupPeakList(mSetObj=NA, mzwid, bw, minfrac, minsamp, max)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param mzwid, define the width of overlapping m/z slices to use for creating peak density chromatograms
#'and grouping peaks across samples
#'@param bw, define the bandwidth (standard deviation or half width at half maximum) of gaussian smoothing
#'kernel to apply to the peak density chromatogram
#'@param minfrac, define the minimum fraction of samples necessary in at least one of the sample groups for
#'it to be a valid group
#'@param minsamp, define the minimum number of samples necessary in at least one of the sample groups for 
#'it to be a valid group 
#'@param max, define the maximum number of groups to identify in a single m/z slice
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GroupPeakList <- function(mSetObj=NA, mzwid = 0.25, bw = 30, minfrac = 0.5, minsamp = 1, max = 50) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- mSetObj$dataSet$peakSet;
  samples <- peakSet$sampnames;
  classlabel <- peakSet$sampclass;
  classnames <- levels(classlabel)
  
  classlabel <- as.vector(unclass(classlabel))
  classnum <- integer(max(classlabel))
  for (i in seq(along = classnum)){
    classnum[i] <- sum(classlabel == i)
  }
  
  peakmat <- peakSet$peaks;
  porder <- order(peakmat[,"mz"]);
  peakmat <- peakmat[porder,,drop=F]
  rownames(peakmat) <- NULL
  retrange <- range(peakmat[,"rt"])
  
  minpeakmat <- min(classnum)/2
  
  mass <- seq(peakmat[1,"mz"], peakmat[nrow(peakmat),"mz"] + mzwid, by = mzwid/2)
  masspos <- findEqualGreaterM(peakmat[,"mz"], mass)
  
  groupmat <- matrix(nrow = 512, ncol = 7 + length(classnum))
  groupindex <- vector("list", 512)
  
  endidx <- 0
  num <- 0
  gcount <- integer(length(classnum))
  for (i in seq(length = length(mass)-2)) {
    startidx <- masspos[i]
    endidx <- masspos[i+2]-1
    if (endidx - startidx + 1 < minpeakmat)
      next
    speakmat <- peakmat[startidx:endidx,,drop=FALSE]
    den <- density(speakmat[,"rt"], bw, from = retrange[1]-3*bw, to = retrange[2]+3*bw)
    maxden <- max(den$y)
    deny <- den$y
    gmat <- matrix(nrow = 5, ncol = 2+length(classnum))
    snum <- 0
    while (deny[maxy <- which.max(deny)] > maxden/20 && snum < max) {
      grange <- descendMin(deny, maxy)
      deny[grange[1]:grange[2]] <- 0
      gidx <- which(speakmat[,"rt"] >= den$x[grange[1]] & speakmat[,"rt"] <= den$x[grange[2]])
      gnum <- classlabel[unique(speakmat[gidx,"sample"])]
      for (j in seq(along = gcount))
        gcount[j] <- sum(gnum == j)
      if (! any(gcount >= classnum*minfrac & gcount >= minsamp))
        next
      snum <- snum + 1
      num <- num + 1
      ### Double the size of the output containers if they're full
      if (num > nrow(groupmat)) {
        groupmat <- rbind(groupmat, matrix(nrow = nrow(groupmat), ncol = ncol(groupmat)))
        groupindex <- c(groupindex, vector("list", length(groupindex)))
      }
      groupmat[num, 1] <- median(speakmat[gidx, "mz"])
      groupmat[num, 2:3] <- range(speakmat[gidx, "mz"])
      groupmat[num, 4] <- median(speakmat[gidx, "rt"])
      groupmat[num, 5:6] <- range(speakmat[gidx, "rt"])
      groupmat[num, 7] <- length(gidx)
      groupmat[num, 7+seq(along = gcount)] <- gcount
      groupindex[[num]] <- sort(porder[(startidx:endidx)[gidx]])
    }
  }
  colnames(groupmat) <- c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
                          "npeaks", classnames)
  
  groupmat <- groupmat[seq(length = num),]
  groupindex <- groupindex[seq(length = num)]
  
  # Remove groups that overlap with more "well-behaved" groups
  numsamp <- rowSums(groupmat[,(match("npeaks", colnames(groupmat))+1):ncol(groupmat),drop=FALSE])
  uorder <- order(-numsamp, groupmat[,"npeaks"])
  uindex <- rectUnique(groupmat[,c("mzmin","mzmax","rtmin","rtmax"),drop=FALSE],uorder)
  
  peakSet$groups <- groupmat[uindex,];
  peakSet$groupidx<- groupindex[uindex];
  mSetObj$dataSet$peakSet <- peakSet;
  return(.set.mSet(mSetObj));
}

#'Set peak list group values
#'@param mSetObj Input name of mSetObj, the data used is the nmr.xcmsSet object
#'@export
#'
SetPeakList.GroupValues <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- mSetObj$dataSet$peakSet;
  msg <- mSetObj$msgSet$peakMsg;
  
  peakmat <- peakSet$peaks;
  groupmat <- peakSet$groups;
  groupindex <- peakSet$groupidx;
  
  sampnum <- seq(length = length(peakSet$sampnames))
  intcol <- match("int", colnames(peakmat))
  sampcol <- match("sample", colnames(peakmat))
  
  # row is peak, col is sample
  values <- matrix(nrow = length(groupindex), ncol = length(sampnum))
  
  for (i in seq(along = groupindex)) {
    # for each group, replace multiple peaks from the same sample by their sum
    for(m in sampnum){
      samp.inx<-which(peakmat[groupindex[[i]], sampcol]==m)
      if(length(samp.inx)>0){
        values[i, m] <- sum(peakmat[groupindex[[i]][samp.inx], intcol]);
      }else{
        values[i, m] <- NA;
      }
    }
  }
  
  msg<-c(msg, paste("A total of", length(groupindex), "peak groups were formed. "));
  msg<-c(msg, paste("Peaks of the same group were summed if they are from one sample. "));
  msg<-c(msg, paste("Peaks appearing in less than half of all samples in each group were ignored."));
  
  colnames(values) <- peakSet$sampnames;
  
  if(peakSet$ncol==2){
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5));
  }else{
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5), "/", round(groupmat[,paste("rt", "med", sep="")],2), sep="");
    mSetObj$dataSet$three.col <- TRUE;
  }
  
  mSetObj$dataSet$orig <- t(values);
  mSetObj$msgSet$proc.msg <- msg
  mSetObj$dataSet$orig.cls <- as.factor(peakSet$sampclass);
  mSetObj$dataSet$type.cls.lbl <- class(peakSet$sampclass);
  return(.set.mSet(mSetObj));
}

#'Retention time correction for LC/GC-MS spectra
#'@description Performs retention time correction for LC/GC-MS spectra using the XCMS package. Following
#'retention time correction, the object dataSet will be regrouped. 
#'@usage MSspec.rtCorrection(mSetObj=NA, bw=30)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param bw Numeric, define the bandwidth (standard deviation or half width at half maximum) of gaussian smoothing
#'kernel to apply to the peak density chromatogram
#'@export
#'
MSspec.rtCorrection <- function(mSetObj=NA, bw=30){
  mSetObj <- .get.mSet(mSetObj);
  xset2 <- xcms::retcor(mSetObj$dataSet$xset.orig)
  # re-group peaks after retention time correction
  xset2 <- xcms::group(xset2, bw=bw)
  mSetObj$dataSet$xset.rt <- xset2;
  return(.set.mSet(mSetObj));
}

#'Plot rentention time corrected spectra
#'@description Plot the retention time corrected spectra
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the name for the created plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@export
PlotMS.RT <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 9;
  }else{
    w <- width;    
  }
  h <- w*7/9;
  
  mSetObj$imgSet$msrt <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plotrt(mSetObj$dataSet$xset.rt);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Function to fill in missing peaks
#'@description For each sample in the processed MS spectra data, this function will fill in missing peaks 
#'using the fillPeaks function from the XCMS package. First, the function will identify any peak groups that are missing 
#'any peaks from the samples and will then fill in those peaks by rereading the raw data and 
#'integrating signals at those regions to create a new peak. 
#'@usage MSspec.fillPeaks(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
MSspec.fillPeaks <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  xset3 <- xcms::fillPeaks(mSetObj$dataSet$xset.rt)
  mSetObj$dataSet$xset.fill <- xset3
  
  msg <- paste("A total of", dim(xset3@peaks)[1],"peaks were detected from these samples")
  msg <- c(msg, paste("with an average of", round(dim(xset3@peaks)[1]/dim(xset3@phenoData)[1], 2), "peaks per spectrum."))
  mSetObj$msgSet$xset.msg <- msg
  return(.set.mSet(mSetObj));
}

#'Create a MS spectra data matrix of peak values for each group
#'@description This function sets up a MS spectra data matrix using the 'groupval' function
#'from XCMS. This generates a usable matrix of peak values for analysis where 
#'columns represent peak groups and rows represent samples. 
#'Collisions where more than one peak from a single sample are in the same group get resolved
#'utilizing "medret", which uses the peak closest to the median retention time.
#'@usage SetupMSdataMatrix(mSetObj, intvalue) 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param intvalue name of peak column to enter into the returned matrix, if intvalue = 'into', 
#'use integrated area of original (raw) peak intensities, 
#'if intvalue = 'intf', use integrated area of filtered peak intensities, 
#'if intvalue = 'intb', use baseline corrected integrated peak intensities,
#'if intvalue = 'maxo', use the maximum intensity of original (raw) peaks, 
#'or if intvalue = 'maxf' use the maximum intensity of filtered peaks
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetupMSdataMatrix <- function(mSetObj=NA, intvalue = c("into","maxo","intb")){
  
  mSetObj <- .get.mSet(mSetObj);
  
  values <- xcms::groupval(mSetObj$dataSet$xset.fill, "medret", value = intvalue);
  msg <- mSetObj$msgSet$xset.msg;
  # transpose to make row for samples
  orig <- as.data.frame(t(values));
  
  msg <- c(msg, paste("These peaks were aligned to", dim(orig)[2], "groups according to their mass and retention time."));
  msg <-c(msg, paste("Please note that some peaks were excluded if they appear in only a few samples."));
  mSetObj$msgSet$xset.msg <- msg;
  mSetObj$dataSet$orig <- orig;
  cls.info <- sampclass(mSetObj$dataSet$xset.fill);
  mSetObj$dataSet$orig.cls <- as.factor(cls.info);
  mSetObj$dataSet$type.cls.lbl <- class(cls.info);
  return(.set.mSet(mSetObj));
}

#'Check if the spectra processing is ok
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@export
#'
IsSpectraProcessingOK <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  msg <- mSetObj$msgSet$xset.msg;
  res = 1;
  if(is.null(mSetObj$dataSet$xset.orig)){
    mSetObj$msgSet$xset.msg<-c(msg, "Failed to read in and process the spectra.");
    res <- 0;
  }
  if(is.null(mSetObj$dataSet$xset.rt)){
    mSetObj$msgSet$xset.msg<-c(msg, "Failed in retention time correction, spectra problem?");
    res <- 0;
  }
  if(is.null(mSetObj$dataSet$xset.fill)){
    mSetObj$msgSet$xset.msg<-c(msg, "Failed in filling missing peaks, spectra problem?");
    res <- 0;
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(res);
  }
  print(res);
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

########## Utility Functions ##############
#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param y Input peaks
#'@param istart Performs which.max on y
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
descendMin <- function(y, istart = which.max(y)) {
  
  if (!is.double(y)) y <- as.double(y)
  unlist(.C("DescendMin",
            y,
            length(y),
            as.integer(istart-1),
            ilower = integer(1),
            iupper = integer(1),
            DUP = FALSE, PACKAGE = "xcms")[4:5]) + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param x Input the data
#'@param values Input the values 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
findEqualGreaterM <- function(x, values) {
  
  if (!is.double(x)) x <- as.double(x)
  if (!is.double(values)) values <- as.double(values)
  .C("FindEqualGreaterM",
     x,
     length(x),
     values,
     length(values),
     index = integer(length(values)),
     DUP = FALSE, PACKAGE = "xcms")$index + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param m Peaks
#'@param order Performs seq(length = nrow(m))
#'@param xdiff Default set to 0
#'@param ydiff Default set to 0
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
rectUnique <- function(m, order = seq(length = nrow(m)), xdiff = 0, ydiff = 0) {
  
  nr <- nrow(m)
  nc <- ncol(m)
  if (!is.double(m))
    m <- as.double(m)
  .C("RectUnique",
     m,
     as.integer(order-1),
     nr,
     nc,
     as.double(xdiff),
     as.double(ydiff),
     logical(nrow(m)),
     DUP = FALSE, PACKAGE = "xcms")[[7]]
}
