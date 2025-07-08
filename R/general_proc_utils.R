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
  save.image("san.RData");
  mSetObj <- .get.mSet(mSetObj);
  if(file.exists("data_orig.qs")){  
    orig.data <- qs::qread("data_orig.qs");
  } else {
    return(0);
  }  
  msg <- NULL;
  cls <- mSetObj$dataSet$orig.cls;
  mSetObj$dataSet$small.smpl.size <- 0;
  
  # check class info only for one factor data
  # For "mf", there is a dedicated page/step "SanityCheckMeta" for this
  
  if(mSetObj$dataSet$cls.type == "disc"){
    
    # added mSetObj$dataSet$pair.checked to allow edit group function names not overwritten by original files
    if(mSetObj$dataSet$paired & !(mSetObj$dataSet$pair.checked)){ 
      msg<-c(msg,"Samples are paired.");
      # need to first set up pair information if not csv file
      if(!(mSetObj$dataSet$type=="conc" | mSetObj$dataSet$type=="specbin" | mSetObj$dataSet$type=="pktable" | mSetObj$dataSet$type=="pktable-ma")){
        pairs <- ReadPairFile();
        # check if they are of the right length
        if(length(pairs)!=length(mSetObj$dataSet$url.smp.nms)){
          AddErrMsg("Error: the total paired names are not equal to sample names.");
          return(0);
        }else{
          # matching the names of the files
          inx<-match(rownames(orig.data), names(pairs));
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
      } 
      
      msg <- c(msg,"The labels of paired samples passed sanity check.");
      msg <- c(msg, paste("A total of", uni.cl.abs, "pairs were detected."));
      # make sure paired samples are sorted 1:n/2 and -1:-n/2
      
      x<-sorted.pairs$ix[(uni.cl.abs+1):uni.cl]
      y<-sorted.pairs$ix[uni.cl.abs:1]
      index<-as.vector(cbind(x,y));
      cls<-cls[index];
      pairs <- pairs[index];
      orig.data<- orig.data[index,];
      
      mSetObj$dataSet$pairs <- pairs;
      mSetObj$dataSet$orig.cls <- cls;
      
      #add sync for paired names
      mSetObj$dataSet$url.smp.nms <- mSetObj$dataSet$url.smp.nms[index];
      
      mSetObj$dataSet$pair.checked <- TRUE;
      #qs::qsave(orig.data, file="data_orig.qs");
      
    } else {
      
      # check for class labels at least two replicates per class but QC and BLANK
      
      cls.lbl <- mSetObj$dataSet$orig.cls;
      qb.inx <- tolower(cls.lbl) %in% c("qc", "blank");
      if(sum(qb.inx) > 0){
        cls.Clean <- as.factor(as.character(cls.lbl[!qb.inx])); # make sure drop level
      } else {
        cls.Clean <- cls.lbl;
      }
      # allow it pass to sanity check and correct there
      if(anal.type != "network" & anal.type != "mf" & anal.type != "dose"){ # add exception for DSPC correlation network 
        if(min(table(cls.Clean)) < 3 | length(levels(cls.Clean)) < 2){
          AddErrMsg(paste ("A total of", length(levels(cls.Clean)), "groups found with", length(cls.Clean), "samples."));
          AddErrMsg("<font color='red'>At least <b>two</b> groups and <b>three replicates</b> per group are required for analysis</font>!");
          if(length(levels(cls.Clean)) > 10){
            AddErrMsg("<font color='red'>It seems the number of groups is big. Make sure to specify the correct format (i.e. samples in <b>columns</b> or <b>rows</b>) in the Data Upload page</font>");
            return(-2);
          }else{
            AddErrMsg("You can click the <b>Edit Groups</b> button below to see the group labels for each sample and make corrections.");
            return(-1);
          }
        }
      } else if(anal.type == "mf"){
        if(min(table(cls.Clean)) < 3 | length(levels(cls.Clean)) < 2){
          msg <- c(msg, paste ("A total of", length(levels(cls.Clean)), "groups found with", length(cls.Clean), "samples."));
          msg <- c(msg, "The primary factor is highly possible a continuous variable.")
        }
      }
      
      if("NMDR_id" %in% names(mSetObj$dataSet)){
        msg <- c(msg, paste("Study", mSetObj$dataSet$NMDR_id, "was successfully downloaded from the Metabolomics Workbench!"))
      }
      if(!mSetObj$dataSet$paired){
        msg <- c(msg,"Samples are not paired.");
      }else{
        msg <- c(msg,"Samples are paired.");
      }
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
    if((cls.num/min.grp.size > 3) & (anal.type != "mf")){
      mSetObj$dataSet$small.smpl.size <- 1;
      msg <- c(msg, "<font color='red'>Too many groups with very small number of replicates!</font>");
      msg <- c(msg, "<font color='red'>Only a subset of methods will be available for analysis!</font>");
    }
    
    
    msg <- c(msg, paste(cls.num, "groups were detected in samples."));
    
    
    if("NMDR_id" %in% names(mSetObj$dataSet)){
      msg <- c(msg, paste("Study", mSetObj$dataSet$NMDR_id, "group labels:", paste0(unique(cls.lbl), collapse = ", ")))
    }
    
    mSetObj$dataSet$cls.num <- cls.num;
    mSetObj$dataSet$min.grp.size <- min.grp.size;
    
    
    ord.inx <- order(mSetObj$dataSet$orig.cls);
    mSetObj$dataSet$orig.cls <- cls[ord.inx];
    mSetObj$dataSet$url.smp.nms <- mSetObj$dataSet$url.smp.nms[ord.inx];
    if(!is.null(mSetObj$dataSet$meta.info)){
      mSetObj$dataSet$meta.info <- mSetObj$dataSet$meta.info[ord.inx, ,drop=F];
    }
    orig.data <- orig.data[ord.inx, , drop=FALSE];
    qs::qsave(orig.data, file="data_orig.qs");
    if(mSetObj$dataSet$paired){
      mSetObj$dataSet$pairs <- mSetObj$dataSet$pairs[ord.inx];
    }
    
  }
  msg<-c(msg,"Only English letters, numbers, underscore, hyphen and forward slash (/) are allowed.");
  msg<-c(msg,"<font color=\"orange\">Other special characters or punctuations (if any) will be stripped off.</font>");
  
  int.mat <- orig.data;
  
  if(ncol(int.mat)==1){
    if(anal.type=="roc"){
      mSetObj$dataSet$roc_cols <- 1;
    } else {
      AddErrMsg("<font color='red'>One-column data is only supported for biomarker analysis.</font>");
      return(0);
    }
  } else {
    mSetObj$dataSet$roc_cols <- 2;
  }
  
  # check numerical matrix
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  naNms <- sum(is.na(int.mat));
  
  for(c in 1:ncol(int.mat)) {
    if(class(int.mat[,c]) == "integer64"){
      int.mat[,c] <- as.double(int.mat[,c]);
    }
  }
  
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
  #  print(naCount)
  mSetObj$dataSet$missingCount <- naCount;
  
  msg<-c(msg, paste("A total of ", naCount, " (", naPercent, "%) missing values were detected.", sep=""));
  
  if(is.null(mSetObj$dataSet$meta.info)){
    mSetObj$dataSet$meta.info <- data.frame(cls);
    colnames(mSetObj$dataSet$meta.info) = "Class";
  }
  
  # make sure the meta.info is synchronized with data
  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
    my.sync <- .sync.data.metadata(int.mat, mSetObj$dataSet$meta.info);
    int.mat <- my.sync$data;
    mSetObj$dataSet$meta.info <- my.sync$metadata;
    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$meta.info[,1];
  }
  
  mSetObj$dataSet$proc.cls <- mSetObj$dataSet$cls <- mSetObj$dataSet$orig.cls;
  
  qs::qsave(as.data.frame(int.mat), "preproc.orig.qs"); # never modify this
  qs::qsave(as.data.frame(int.mat), "preproc.qs"); # working copy
  
  
  min.n.qc    <- 3   # lowest acceptable for %RSD
  min.n.blank <- 2   # one before + one after
  
  n.qc    <- sum(grepl("^\\s*qc\\s*$",    mSetObj$dataSet$cls, ignore.case = TRUE))
  n.blank <- sum(grepl("^\\s*blank\\s*$", mSetObj$dataSet$cls, ignore.case = TRUE))
  
  
  if (isFALSE(mSetObj$dataSet$containsQC) && n.qc > 0) {
    msg <- c(msg,
             paste0(
               "<font color=\"orange\">",
               n.qc, " QC sample", ifelse(n.qc == 1, "", "s"),
               " detected even though the ‘Incl. QC samples’ option was left unticked. ",
               "They will be treated as QC samples automatically.</font>"
             ))
  }
  
  if (isFALSE(mSetObj$dataSet$containsBlank) && n.blank > 0) {
    msg <- c(msg,
             paste0(
               "<font color=\"orange\">",
               n.blank, " blank injection", ifelse(n.blank == 1, "", "s"),
               " detected but the ‘Incl. Blank samples’ option was not selected.</font>"
             ))
  }
  qc.msg <- CheckQCRSD(mSetObj)
  msg    <- c(msg, qc.msg)
  
  
  if(naCount == 0){
    
    msg<-c(msg, "Click the <b>Proceed</b> button to the next step.");
  }else{  
    
    
    #msg<-c(msg, "<u>By default, missing values will be replaced by 1/5 of min positive values of their corresponding variables</u>");
    if(mSetObj$dataSet$cls.type == "disc" && length(levels(cls)) > 1){
      miss.msg <- "";
      kw.p <- .test.missing.sig(int.mat, cls);
      
      if(kw.p <= 0.05){
        miss.msg <- "<font color='red'>Missing-value patterns differ significantly between groups.</font>";
      }else{
        miss.msg <- "No significant differences were detected in missing-value patterns across different groups.";
      }
      mSetObj$dataSet$missTest <- kw.p;
      mSetObj$msgSet$miss.msg <- paste0("Kruskal-Wallis test: <b>p = ", signif(kw.p, 3), "</b>.");
      msg<-c(msg,  miss.msg);
    }
    #msg<-c(msg,
    #     "Click the <b>Proceed</b> button if you accept the default practice;",
    #     "Or click the <b>Missing Values</b> button to use other methods.");
  }
  
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$read.msg, msg);
  
  if(!.on.public.web){
    print(c("Successfully passed sanity check!", msg))
  }
  return(.set.mSet(mSetObj));
}

.test.missing.sig <- function(int.mat, cls){
  # Exclude samples with "QC" in the group label
  keep.inx <- !grepl("^qc$", cls, ignore.case = TRUE)
  
  int.mat <- int.mat[keep.inx, , drop = FALSE]
  cls <- cls[keep.inx]
  
  na.mat <- is.na(int.mat)
  miss.per.smp <- rowSums(na.mat)
  kw.res <- kruskal.test(miss.per.smp ~ cls)
  
  return(kw.res$p.value)
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
  mSetObj$dataSet$filt <- mSetObj$dataSet$edit <- NULL;
  
  # replace zero and missing values using Detection Limit for each variable 
  int.mat <- qs::qread("preproc.qs");
  #int.mat <- ReplaceMissingByLoD(preproc);  
  
  # note, this is last step of processing, also save to proc
  #mSetObj$dataSet$proc <- as.data.frame(int.mat);
  mSetObj$dataSet$proc.feat.num <- ncol(int.mat);
  qs::qsave(as.data.frame(int.mat), file="data_proc.qs");

  #mSetObj$msgSet$replace.msg <- paste("Zero or missing values were replaced by 1/5 of the min positive value for each variable.");
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
RemoveMissingPercent <- function(mSetObj = NA,
                                 percent  = 0.20,   # e.g. 0.20 = 20 %
                                 grpWise  = FALSE) {

  mSetObj <- .get.mSet(mSetObj)

  ## 1 · Choose the working matrix --------------------------------------
  if (!.on.public.web && !is.null(mSetObj$dataSet$norm)) {
    int.mat   <- mSetObj$dataSet$norm          # already-normalised
    writeBack <- TRUE
  } else {
    int.mat   <- qs::qread("preproc.orig.qs")  # raw pre-processing copy
    writeBack <- FALSE
  }

  ## 2 · Determine “good” variables -------------------------------------
  if (!grpWise) {
    ## —— global rule (original behaviour) ————————————————
    good.inx <- colMeans(is.na(int.mat)) < percent

  } else {
    ## —— group-wise rule (modified 80 %) ————————————————
    ##     Keep a feature if *at least one* group is below threshold.
    ##     Remove feature only when EVERY group exceeds threshold.

    ## 2·1  Get group vector (first column of meta.info, or cls)
    if (!is.null(mSetObj$dataSet$meta.info) &&
        ncol(mSetObj$dataSet$meta.info) >= 1) {
      grp <- as.character(mSetObj$dataSet$meta.info[, 1])
    } else {
      grp <- as.character(mSetObj$dataSet$cls)
    }
    stopifnot(length(grp) == nrow(int.mat))

    ## 2·2  % missing per feature *within each group*
    na.mat     <- is.na(int.mat)                       # logical matrix
    miss.grps  <- t(vapply(split(as.data.frame(na.mat), grp),
                           function(x) colMeans(as.matrix(x)),
                           numeric(ncol(int.mat))))    # g × p matrix

    ## 2·3  Keep if ANY group passes the threshold
    good.inx <- apply(miss.grps, 2, function(x) any(x < percent))
  }

  ## 3 · Save filtered matrix back --------------------------------------
  rm.cnt <- sum(!good.inx)            # variables removed

  if (writeBack) {
    mSetObj$dataSet$norm <- as.data.frame(int.mat[, good.inx, drop = FALSE])
  } else {
    qs::qsave(as.data.frame(int.mat[, good.inx, drop = FALSE]), "preproc.qs")
  }

  ## 4 · Log a concise message ------------------------------------------
  rule.txt <- ifelse(grpWise, "(group-wise rule)", "(overall rule)")
  msg      <- sprintf("%d variables were removed for threshold %.2f%% %s",
                      rm.cnt, 100 * percent, rule.txt)
  mSetObj$msgSet$miss.filter.msg <- c(msg)
  
  return(.set.mSet(mSetObj))
}

fetchMissFilterMsg <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)

  msgs <- mSetObj$msgSet$miss.filter.msg

  if (length(msgs) == 0 || all(nchar(trimws(msgs)) == 0)) {
    return("")                    # nothing to report
  }

  paste(msgs, collapse = ". ")
}

fetchReplaceMsg <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)

  msgs <- mSetObj$msgSet$replace.msg

  if (length(msgs) == 0 || all(nchar(trimws(msgs)) == 0)) {
    return("")                    # nothing to report
  }

  paste(msgs, collapse = ". ")
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
ImputeMissingVar <- function(mSetObj=NA, method="lod", grpLod=F, grpMeasure=F){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.impute.missing")){ # public web on same user dir
      .load.scripts.on.demand("util_missing.Rc");    
    }
    return(my.impute.missing(mSetObj, method, grpLod, grpMeasure));
  }else{
    return(my.impute.missing(mSetObj, method, grpLod, grpMeasure));
  }
}

CheckContainsBlank <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  cls <- mSetObj$dataSet$proc.cls;
  if("BLANK" %in% cls){
    return(1)
  } else {
    return(0)
  }  
}


#'Methods for non-specific filtering of variables
#'@description This is a function that filters the dataset, dependent on the user-specified method
#'for filtering. The function applies a filtering method, ranks the variables within the dataset,
#'and removes variables based on its rank. The final dataset should contain no more than
#'than 5000 variables for effective computing. 
#'@usage FilterVariable(mSetObj=NA, filter, qcFilter, rsd)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param var.filter Select the filter option, "rsd" which is the relative standard deviation, "nrsd" which
#'is the non-parametric relative standard deviation, "mean" which is the mean, "sd" which is the standard
#'deviation, "mad" which is the median absolute deviation, or "iqr" which is the interquantile range.
#'@param filter.cutoff percent to be filtered, for example, 5 (5\%)
#'@param qc.filter Filter the variables based on QC samples - True (T), or use non-QC based filtering - False (F).  
#'@param rsd Define the relative standard deviation cut-off. Variables with a RSD greater than this number
#'will be removed from the dataset. It is only necessary to specify this argument if qc.filter is True (T). 
#'Otherwise, it will not be used in the function.
#'@param int.cutoff int.cutoff value, numeric
#'@param var.cutoff var.cutoff value
#'@param int.filter int.filter value
#'@param blank.subtraction blank.subtraction boolean value
#'@param blank.threshold blank.threshold value
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

FilterVariable <- function(mSetObj=NA, qc.filter="F", rsd, var.filter="iqr", var.cutoff=NULL, int.filter="mean", int.cutoff=0, blank.subtraction=F, blank.threshold=10){

  mSetObj <- .get.mSet(mSetObj);
  
  #Reset to default
  mSetObj$dataSet$filt <- NULL;
  
  if(is.null(mSetObj$dataSet$proc)){
    int.mat <- as.matrix(qs::qread("data_proc.qs"));
  }else{
    int.mat <- as.matrix(mSetObj$dataSet$proc);
  }
  cls <- mSetObj$dataSet$proc.cls;
  
  # save a copy
  mSetObj$dataSet$filt.cls <- cls;
  
  msg <- NULL;
  if(qc.filter == "T"){
    rsd <- rsd/100;
    # need to check if QC exists
    qc.hits <- tolower(as.character(cls)) %in% "qc";
    if(sum(qc.hits) > 1){ # require at least 2 QC for RSD
      qc.mat <- int.mat[qc.hits,];
      sds <- apply(qc.mat, 2, sd, na.rm=T);
      mns <- apply(qc.mat, 2, mean, na.rm=T);
      rsd.vals <- abs(sds/mns);  
      gd.inx <- rsd.vals < rsd;

      # save a copy for user 
      fast.write.csv(cbind(RSD=rsd, t(int.mat)), file="data_prefilter_qc_rsd.csv");

      int.mat <- int.mat[,gd.inx];
      if(mSetObj$analSet$type %in% c("mummichog")){
        msg <- paste("Removed <b>", sum(!gd.inx), "</b> features based on QC RSD values. QC samples are excluded from downstream functional analysis.");
      }else{
        msg <- paste("Removed <b>", sum(!gd.inx), "</b> features based on QC RSD values. QC samples are still kept. You can remove them later.");
      }
    }else if(sum(qc.hits) > 0){
      AddErrMsg("RSD requires at least 2 QC samples, and only non-QC based filtering can be applied.");
      return(0);
    }else{
      AddErrMsg("No QC Samples (with class label: QC) found.  Please use non-QC based filtering.");
      return(0);
    }
  }

  if(blank.subtraction){
    #save(int.mat, cls, file= "FilterVariable_dosubtraction.rda")
    if("BLANK" %in% cls){
        idx2keep <- blankfeatureSubtraction(cls, blank.threshold);
        idx2keep_exist <- vapply(names(idx2keep), function(x){x %in% colnames(int.mat)}, logical(1L))
        idx2keep_all <- idx2keep & idx2keep_exist
        ft_nms2keep <- names(which(idx2keep_all))
        cols2keep <- vapply(colnames(int.mat), function(x){x %in% ft_nms2keep}, logical(1L))
        int.mat <- int.mat[,cols2keep]
        rows2keep <- which(cls != "BLANK")
        int.mat <- int.mat[rows2keep,]        
        cls <- cls[cls != "BLANK"]
        cls <- droplevels(cls, "BLANK")
        mSetObj$dataSet$proc.cls <- mSetObj$dataSet$filt.cls <- cls
    }
  }

  # no explicit user choice, will apply default empirical filtering based on variance
  if(is.null(var.cutoff)){ 
    var.cutoff <- .computeEmpiricalFilterCutoff(ncol(int.mat), mSetObj$analSet$type);
  }

  if(var.cutoff > 0){ 
     filt.res <- PerformFeatureFilter(int.mat, var.filter, var.cutoff, mSetObj$analSet$type, msg);
     int.mat <- filt.res$data;
     msg <- c(msg, filt.res$msg);
  }

  if(int.cutoff > 0){ 
     filt.res <- PerformFeatureFilter(int.mat, int.filter, int.cutoff, mSetObj$analSet$type, msg);
     int.mat <- filt.res$data;
     msg <- c(msg, filt.res$msg);
  }

  mSetObj$dataSet$filt <- int.mat;

  if(is.null(msg)){
     msg <- "No data filtering was performed."
  }

  AddMsg(msg);
  mSetObj$msgSet$filter.msg <- msg;

  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
      # make sure metadata are in sync with data
      my.sync <- .sync.data.metadata(mSetObj$dataSet$filt, mSetObj$dataSet$meta.info);
      mSetObj$dataSet$meta.info <- my.sync$metadata;
  }
 
  return(.set.mSet(mSetObj));
}

blankfeatureSubtraction <- function(cls, threshold){
  # need to evaluate raw data table without replace min
  idx_blank <- which(as.character(cls)=="BLANK")
  preproc <- qs::qread("preproc.qs");
  
  res <- apply(preproc, 2, function(x){
    vec1 <- x[idx_blank]
    vec2 <- x[-idx_blank]
    return(mean(vec1, na.rm = T)/mean(vec2, na.rm = T))
  })
  
  ## Estimating the threshold for filtering
  sort_res <- sort(res, decreasing = T)
  infectPoint <- bede(x= c(1:length(sort_res)), 
                      y = sort_res, 1)$iplast
  if(!is.nan(infectPoint)){
    blk_thresh <- round(sort_res[ceiling(infectPoint)],1)/5
  } else {
    blk_thresh <- round(sort_res[length(sort_res)*0.2]) # Keep top 20% FC (sample/blank) peaks
  }
  if(!is.numeric(blk_thresh)){
    message("No threshold can be evaluted for blank substraction, will use the threshold provided by user!")
    blk_thresh <- threshold;
  } else {
    message(paste0("Threshold for blank filtration has been estimated as ", blk_thresh, "..."))
  }
  
  InclusionVec <- apply(preproc, 2, function(x){
    blkinto <- mean(x[idx_blank], na.rm = T)
    smlinto <- mean(x[-idx_blank], na.rm = T)
    if(is.nan(smlinto)){
      return(FALSE)
    } else if (is.nan(blkinto)) {
      return(TRUE)
    } else if(smlinto/blkinto > blk_thresh){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  return(InclusionVec)
}


ede <- function (x, y, index) 
{
  n = length(x)
  if (index == 1) {
    y = -y
  }
  ifelse(n >= 4, {
    LF = y - lin2(x[1], y[1], x[n], y[n], x)
    jf1 = which.min(LF)
    xf1 = x[jf1]
    jf2 = which.max(LF)
    xf2 = x[jf2]
    ifelse(jf2 < jf1, {
      xfx <- NaN
    }, {
      xfx <- 0.5 * (xf1 + xf2)
    })
  }, {
    jf1 = NaN
    jf2 = NaN
    xfx = NaN
  })
  out = matrix(c(jf1, jf2, xfx), nrow = 1, ncol = 3, byrow = TRUE)
  rownames(out) = "EDE"
  colnames(out) = c("j1", "j2", "chi")
  return(out)
}

lin2 <- function (x1, y1, x2, y2, x) 
{
  y1 + (y2 - y1) * (x - x1)/(x2 - x1)
}

bede <- function (x, y, index) 
{
  EDE <- c()
  BEDE <- c()
  a <- c(x[1])
  b <- c(x[length(x)])
  nped <- c(length(x))
  x2 <- x
  y2 <- y
  B = ede(x, y, index)
  EDE <- c(EDE, B[1, 3])
  BEDE <- c(BEDE, B[1, 3])
  iplast = B[1, 3]
  j <- 0
  while (!(is.nan(B[1, 3]))) {
    ifelse(B[1, 2] >= B[1, 1] + 3, {
      j <- j + 1
      x2 <- x2[B[1, 1]:B[1, 2]]
      y2 <- y2[B[1, 1]:B[1, 2]]
      B <- ede(x2, y2, index)
      ifelse(!(is.nan(B[1, 3])), {
        a = c(a, x2[B[1, 1]])
        b = c(b, x2[B[1, 2]])
        nped <- c(nped, length(x2))
        EDE <- c(EDE, B[1, 3])
        BEDE <- c(BEDE, B[1, 3])
        iplast = B[1, 3]
      }, {
        break
      })
    }, {
      break
    })
  }
  iters = as.data.frame(cbind(nped, a, b, BEDE))
  colnames(iters) = c("n", "a", "b", "EDE")
  rownames(iters) = 1:length(nped)
  out = list()
  out[["iplast"]] = iplast
  out[["iters"]] = iters
  return(out)
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetOrigSmplNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(names(mSetObj$dataSet$url.smp.nms));
}

GetOrigGrpNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$orig.cls);
}

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
  #print(mSetObj$dataSet$small.smpl.size);
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
  
  if(file.exists("data_proc.qs")){
    proc.data <- qs::qread("data_proc.qs");
    proc.data <- .update.feature.nm(proc.data, old.nm, new.nm);
    mSetObj$dataSet$proc.feat.num <- ncol(proc.data);
    qs::qsave(proc.data, file="data_proc.qs");

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

UpdateSampleGroups<-function(mSetObj=NA, metadata="NA"){
  mSetObj <- .get.mSet(mSetObj);
  cls.lbl <- ClearStrings(as.vector(grp.vec));
  if(is.null(mSetObj$dataSet$meta.info)) {
    mSetObj$dataSet$meta.info <- matrix(nrow = length(cls.lbl))
  }
  meta.info <- mSetObj$dataSet$meta.info;
  inx <- 1;
  if(metadata %in% colnames(meta.info)){
    inx <- which(colnames(meta.info) == metadata);
    type <- mSetObj$dataSet$meta.types[inx];
    x <- cls.lbl

    if(type == "cont"){
        is.num <- T
        if(type == "cont"){
           isNum <- grepl("^-?[0-9.]+$", x);
           if(!all(isNum)){
                is.num <- F;
           }
        }

      if(!is.num){
          mSetObj$dataSet$meta.status[inx] <- "<font color='red'>Not all numeric</font>"
      }else{
          mSetObj$dataSet$meta.status[inx] <- "OK"
      }
    }else{

        containsMissing <-  sum(is.na(x))/length(x) + sum(x=="NA")/length(x) + sum(x=="")/length(x) + sum(x=="-")/length(x)  >0
        qb.inx <- tolower(cls.lbl) %in% c("qc", "blank");
        if(sum(qb.inx) > 0){
            cls.Clean <- as.factor(as.character(cls.lbl[!qb.inx])); # make sure drop level
        } else {
            cls.Clean <- as.factor(cls.lbl);
        }
        meta.name <- colnames(meta.info)[inx];
        min.grp.size <- min(table(cls.Clean));
        cls.num <- length(levels(cls.Clean));
        lowReplicate <- min.grp.size < 3 | cls.num < 2
        tooManyLow <- cls.num/min.grp.size > 4
        if(containsMissing){
            mSetObj$dataSet$meta.status[inx] <- "<font color='red'>Missing values</font>"
        }else if (tooManyLow){
            mSetObj$dataSet$meta.status[inx] <- "<font color='red'>Too many low replicates</font>"
        }else if (lowReplicate){
            mSetObj$dataSet$meta.status[inx] <- "<font color='darkorange'>Low replicates</font>"
        }else{
            mSetObj$dataSet$meta.status[inx] <- "OK"
        }
    }
  }else{
    mSetObj$dataSet$orig.cls <- mSetObj$dataSet$proc.cls <- mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$cls <- as.factor(cls.lbl);
  }

  mSetObj$dataSet$meta.info[,inx] = as.factor(cls.lbl);
  return(.set.mSet(mSetObj));
}

#'Check for missing data
#'@description ContainMissing is used to check if any missing data exists in the uploaded file.
#'@usage ContainMissing(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
ContainMissing <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(.on.public.web){
    if(mSetObj$dataSet$missingCount > 0){
      return(1);
    }
    return(0);
  }else{
    if(mSetObj$dataSet$missingCount > 0){
      print("Contains missing data - will be dealt with in next step.");
    }
    print("Does not contain missing data.");
    return(.set.mSet(mSetObj));
  }
}

GetMetaDataCol <- function(mSetObj=NA, colnm){
  
    mSetObj <- .get.mSet(mSetObj);
    if(colnm=="NA"){
        cls<-levels(mSetObj$dataSet$meta.info[,1])
    }else{
        meta <- factor(mSetObj$dataSet$meta.info[,colnm])
        cls<-levels(meta)
    }
    return(cls[cls!="NA"]);
}

GetMissingTestMsg <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    msg <- mSetObj$msgSet$miss.msg;
    if(is.null(msg)){
        return("NA");
    }
    return(msg);
}

#' Plot Non-Missing Value Lollipop (inch dimensions)
#'
#' @description
#'   Generates a lollipop plot showing, for every sample, the percentage
#'   of non-missing values.  The graphic device now uses inches
#'   (default 8 × 6) rather than pixels.
#'
#' @usage
#'   PlotMissingLollipop(mSetObj = NA,
#'                       imgName  = "miss_lollipop",
#'                       format   = "png",
#'                       dpi      = 300,
#'                       width    = 8,
#'                       height   = 6)
#'
#' @param mSetObj  The working mSet object.
#' @param imgName  Base file name (no extension).
#' @param format   "png", "tiff", "pdf", or "svg" (default "png").
#' @param dpi      Resolution (dots per inch).  Default 300.
#' @param width    Width **in inches**.  If \code{NA}, a heuristic
#'                 (0.25 in per sample, min 8 in) is applied.
#' @param height   Height **in inches** (default 6).
#'
#' @import qs
#' @import ggplot2
#' @importFrom Cairo Cairo
#' @export
#'
#' Plot Non-Missing Value Lollipop (auto-coloured by meta type)
#'
#' @description
#'   Generates a lollipop plot where stems and points are coloured by a
#'   user-selected metadata column.  If the column is marked
#'   \code{"cont"} in \code{mSetObj$dataSet$meta.types}, a
#'   continuous gradient is applied; otherwise a discrete palette is
#'   used.  If no \code{meta.info} exists, the function colours by the
#'   class labels and treats them as discrete.
#'
#' @usage
#'   PlotMissingLollipop(mSetObj = NA,
#'                       imgName  = "miss_lollipop",
#'                       format   = "png",
#'                       dpi      = 150,
#'                       width    = NA,
#'                       groupCol = NULL)
#'
#' @param mSetObj  MetaboAnalyst object.
#' @param imgName  Base output name (no extension).
#' @param format   Image format: "png", "tiff", "pdf", or "svg". Default "png".
#' @param dpi      Device resolution (dpi). Default 150.
#' @param width    Width in inches; if \code{NA}, uses 0.25" per sample (≥ 8").
#' @param groupCol Metadata column to colour by (default = first column or "Class").
#'
#' @export
#'
PlotMissingDistr <- function(mSetObj = NA,
                             imgName  = "miss_box",
                             format   = "png",
                             dpi      = default.dpi,
                             width    = NA,
                             groupCol = NULL) {

  require("ggplot2")
  require("qs")
  require("Cairo")

  ## -------- Retrieve data & completeness ------------------------------
  mSetObj <- .get.mSet(mSetObj)

  int.mat <- if (file.exists("preproc.qs")) {
    qs::qread("preproc.orig.qs")
  } else if (!is.null(mSetObj$dataSet$orig)) {
    mSetObj$dataSet$orig
  } else {
    AddErrMsg("No processed data found for missing-value plot!")
    return(0)
  }

  if (is.vector(int.mat)) int.mat <- t(as.matrix(int.mat))

  pct.missing <- 100 * rowMeans(is.na(int.mat))

  ## -------- Resolve group vector --------------------------------------
  has.meta <- !is.null(mSetObj$dataSet$meta.info)
  if (has.meta) {
    meta.df  <- mSetObj$dataSet$meta.info
    if (is.null(groupCol)) groupCol <- colnames(meta.df)[1]
    if (!groupCol %in% colnames(meta.df)) {
      AddErrMsg(sprintf("Column ‘%s’ not in meta.info – using first column.",
                        groupCol))
      groupCol <- colnames(meta.df)[1]
    }
    grp.vec  <- meta.df[[groupCol]]
    col.idx  <- match(groupCol, colnames(meta.df))
    grp.type <- if (!is.null(mSetObj$dataSet$meta.types)) {
      mSetObj$dataSet$meta.types[col.idx]
    } else "disc"
  } else {
    grp.vec  <- mSetObj$dataSet$cls
    groupCol <- "Class"
    grp.type <- "disc"
  }

  ## -------- Cast by type & colour scale -------------------------------
  if (grp.type == "cont") {
    grp.vec    <- as.numeric(grp.vec)
    grp.num    <- 0
    fill_scale <- scale_fill_gradient(low = "#56B1F7", high = "#132B43")
  } else {
    grp.vec    <- factor(grp.vec)
    grp.num    <- length(levels(grp.vec))
    fill_scale <- scale_fill_discrete()
  }

  ## -------- Data frame for plotting -----------------------------------
  df <- data.frame(Group = grp.vec, Percent = pct.missing)

  ## -------- Device size -----------------------------------------------
  height <- 3 + grp.num * 0.25           # simple heuristic

  ## choose width if user passed NA or non-positive
  if (is.na(width) || !is.numeric(width) || width <= 0) {
    width <- 7                           # fallback default
  }

  img.full <- paste0(imgName, "dpi", dpi, ".", format)

  ## -------- Plot & save ------------------------------------------------
  Cairo::Cairo(file   = img.full,
               width  = width,
               height = height,
               dpi    = dpi,
               units  = "in",
               type   = format)

  p <- ggplot(df, aes(x = Group, y = Percent, fill = Group)) +
       geom_boxplot(outlier.shape = 21, outlier.size = 2) +
       coord_flip() +
       labs(x = NULL, y = "Missing Percentage", fill = groupCol) +
       ylim(0, 100) +
       theme_minimal(base_size = 14) +
       theme(panel.grid.major.y = element_blank(),
             plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")) +
       fill_scale

  print(p)
  dev.off()

  ## -------- Book-keeping ----------------------------------------------
  mSetObj$imgSet$miss.box <- img.full
  mSetObj$msgSet$plot.msg <- c(
    mSetObj$msgSet$plot.msg,
    sprintf("Missing-value boxplot saved (%s, %.1f × %.1f in, %d dpi) – coloured by ‘%s’ (%s).",
            format, width, height, dpi, groupCol, grp.type)
  )

  return(.set.mSet(mSetObj))
}



#' @title Missing Value Heatmap
#' @description
#'   Generates a heatmap showing the locations of missing values across samples (rows) and features (columns).
#'   Useful for identifying patterns of group-specific or sample-level missingness.
#'
#' @usage
#'   PlotMissingHeatmap(mSetObj = NA,
#'                      imgName  = "miss_heatmap",
#'                      format   = "png",
#'                      dpi      = 150)
#'
#' @param mSetObj  MetaboAnalyst object.
#' @param imgName  Base output name (no extension).
#' @param format   Image format: "png", "tiff", "pdf", or "svg". Default "png".
#' @param dpi      Device resolution (dpi). Default 150.
#'
#' @export
#'
PlotMissingHeatmap <- function(mSetObj = NA,
                               imgName  = "miss_heatmap",
                               format   = "png",
                               dpi      = 150) {
  
  require("ggplot2")
  require("qs")
  require("Cairo")
  
  mSetObj <- .get.mSet(mSetObj)

  int.mat <- if (file.exists("preproc.orig.qs")) {
    qs::qread("preproc.orig.qs")
  } else {
    AddErrMsg("No processed data found for missing value heatmap!")
    return(0)
  }

  if (is.vector(int.mat)) int.mat <- t(as.matrix(int.mat))

  nSamples  <- nrow(int.mat)
  nFeatures <- ncol(int.mat)

  msg <- NULL
  if (nSamples > 50) {
    int.mat <- int.mat[1:50, , drop = FALSE]
    msg <- sprintf("Only the first 50 samples are shown (out of %d).", nSamples)
  }

  hide_feat_labels <- nFeatures > 100

  # Order features by missingness count (descending)
  miss.counts <- colSums(is.na(int.mat))
  feature.order <- names(sort(miss.counts, decreasing = TRUE))

  # Convert to missing indicator matrix and long format
  miss.mat <- is.na(int.mat)
  df <- as.data.frame(as.table(miss.mat))
  colnames(df) <- c("Sample", "Feature", "Missing")
  df$Missing <- ifelse(df$Missing, "Missing", "Present")

  # Apply factor levels for ordered plotting
  df$Feature <- factor(df$Feature, levels = feature.order)
  df$Sample <- factor(df$Sample, levels = rownames(int.mat))  # preserve original order

  # Heatmap plot
  img.full <- paste(imgName, "dpi", dpi, ".", format, sep = "")
  Cairo::Cairo(file = img.full, width = 8, height = 6, units = "in", dpi = dpi, type = format)

  p <- ggplot(df, aes(x = Feature, y = Sample, fill = Missing)) +
    geom_tile(color = "grey90") +
    scale_fill_manual(values = c("Present" = "white", "Missing" = "red")) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = if (hide_feat_labels) element_blank() else element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.ticks.x = if (hide_feat_labels) element_blank() else element_line(),
      legend.position = "top",
      panel.grid = element_blank()
    )

  print(p)
  dev.off()

  # Book-keeping
  mSetObj$imgSet$miss.heatmap <- img.full
  mSetObj$msgSet$plot.msg <- c(
    mSetObj$msgSet$plot.msg,
    sprintf("Missing value heatmap saved (%s, 8 × 6 in, %d dpi).", format, dpi),
    msg
  )

  return(.set.mSet(mSetObj))
}


#' @title Export Missing Value Heatmap to JSON for Plotly.js (v3)
#' @description
#'   Prepares a JSON file with the missingness matrix (0 = present, 1 = missing) formatted for Plotly.js v3 heatmap.
#'
#' @usage
#'   ExportMissingHeatmapJSON(mSetObj = NA,
#'                            fileName = "missing_heatmap.json")
#'
#' @param mSetObj   MetaboAnalyst object.
#' @param fileName  Output JSON file name.
#'
#' @export
ExportMissingHeatmapJSON <- function(mSetObj = NA,
                                     fileName = "missing_heatmap.json") {
  require("qs")
  require("rjson")

  mSetObj <- .get.mSet(mSetObj)

  int.mat <- if (file.exists("preproc.qs")) {
    qs::qread("preproc.orig.qs")
  } else {
    AddErrMsg("No processed data found for JSON heatmap export!")
    return(0)
  }

  if (is.vector(int.mat)) int.mat <- t(as.matrix(int.mat))

  # Limit to first 50 samples
  if (nrow(int.mat) > 50) {
    int.mat <- int.mat[1:50, , drop = FALSE]
  }

  # Compute missing indicator
  miss.mat <- 1 * is.na(int.mat)

  # Order features (columns) by missing count (descending)
  miss.counts <- colSums(miss.mat)
  ord.idx <- order(miss.counts, decreasing = TRUE)
  miss.mat <- miss.mat[, ord.idx]
  feature.names <- colnames(miss.mat)

  # Convert to list of rows
  z <- unname(split(miss.mat, row(miss.mat)))

  # Construct JSON object for Plotly.js v2
  out.list <- list(
    z = z,
    x = feature.names,
    y = rownames(miss.mat),
    type = "heatmap",
    colorscale = list(list(0, "white"), list(1, "red")),
    showscale = FALSE,
    hoverinfo = "x+y+z"
  )

  # Write JSON
  json.out <- rjson::toJSON(out.list)
  write(json.out, file = paste0(fileName, ".json"))

  mSetObj$msgSet$plot.msg <- c(
    mSetObj$msgSet$plot.msg,
    sprintf("Missing value heatmap exported to JSON (%s).", fileName)
  )

  return(.set.mSet(mSetObj))
}

#' Check QC %RSD with pmp::filter_peaks_by_rsd
#'
#' Uses the Peak-Matrix-Processing (pmp) package to calculate the
#' relative standard deviation (%RSD) of each feature across QC
#' injections and returns a concise QA message.
#'
#' @param mSetObj MetaboAnalystR object (default NA → pull from session)
#' @param thr     RSD threshold (%) for the “pass-rate” statistic
#' @return        Character string summarising QC precision

  CheckQCRSD <- function(mSetObj, thr = 30) {
    
meta.ok <- !is.null(mSetObj$dataSet$meta.info)          &&        
           ncol(mSetObj$dataSet$meta.info) >= 1          &&       
           length(mSetObj$dataSet$meta.info[, 1]) > 0    &&       
           !all(is.na(mSetObj$dataSet$meta.info[, 1]))            

cls.ok  <- !is.null(mSetObj$dataSet$cls) &&
           length(mSetObj$dataSet$cls)    > 0 &&
           !all(is.na(mSetObj$dataSet$cls))

cls <- if (meta.ok) {
          mSetObj$dataSet$meta.info[, 1]
       } else if (cls.ok) {
          mSetObj$dataSet$cls
       } else {
          return("");              # handle rare case where neither source is usable
       }
    
    cls    <- tolower(replace(as.character(cls), is.na(cls), "qc"))
    print(cls);
    qc.inx <- cls == "qc"
    n.qc   <- sum(qc.inx)
    
    if (n.qc == 0) {                              # no QC injections
      mSetObj$msgSet$qc.rsd.msg <- ""
      return("")
    }
    
    ## ── 2. load matrix (features × samples) ----------------------------------
    if (!file.exists("preproc.qs")) {
      msg <- "Could not locate 'preproc.qs'; %RSD calculation skipped."
      mSetObj$msgSet$qc.rsd.msg <- msg
      return(msg)
    }
    raw <- t(qs::qread("preproc.qs"))                # rows = ?  cols = ?
    
    ## make sure rows = features, cols = samples
    if (ncol(raw) != length(cls)) raw <- t(raw)
    
    ## ── 3. vignette-style RSD% ----------------------------------------------
    FUN <- function(x) stats::sd(x, na.rm = TRUE) /
      mean(x, na.rm = TRUE) * 100
    
    rsd_qc  <- apply(raw[ ,  qc.inx, drop = FALSE], 1, FUN)
    rsd_smp <- apply(raw[ , !qc.inx, drop = FALSE], 1, FUN)  # not used in msg
    
    ## ── 4. summary numbers for QC -------------------------------------------
    med.rsd   <- median(rsd_qc, na.rm = TRUE)
    prop.pass <- round(mean(rsd_qc < thr, na.rm = TRUE) * 100, 1)
    
    base.msg <- sprintf(
      "QC samples (n = %d): median RSD = %.1f%%; %.1f%% of features &lt; %d%%. ",
      n.qc, med.rsd, prop.pass, thr
    )
    link <- "<a href='#' onclick=\"rcViewRsd();return false;\">View RSD plot</a>"
    msg  <- paste0(base.msg, link)
    
    mSetObj$dataSet$qc.rsd    <- rsd_qc          # vector (length = n features)
    mSetObj$msgSet$qc.rsd.msg <- msg
    invisible(.set.mSet(mSetObj))
    
    msg
  }
  

#───────────────────────────────────────────────────────────────────────────────
#  PlotRSDViolin  ─  QC-centred RSD% violin plot (pmp vignette style)
#───────────────────────────────────────────────────────────────────────────────
#  mSetObj     : MetaboAnalyst object or NA to fetch the active one
#  imgName     : base filename (without dpi / extension)
#  format      : "png", "pdf", …
#  dpi         : raster resolution
#  width       : plot width in inches
#  thr         : reference RSD% threshold (dashed line); default 30 %
#  showSamples : also plot biological samples (default FALSE)
#
PlotRSDViolin <- function(mSetObj = NA,
                          imgName,
                          format = "png",
                          dpi    = default.dpi,
                          width  = NA,
                          thr    = 30) {

  mSetObj <- .get.mSet(mSetObj)

  if (!file.exists("preproc.qs"))
    stop("Cannot find 'preproc.qs'.")

  raw <- t(qs::qread("preproc.qs"))      # rows = features, cols = samples

  ## ── class vector --------------------------------------------------------
  cls <- if (!is.null(mSetObj$dataSet$cls) &&
             length(mSetObj$dataSet$cls) &&
             !all(is.na(mSetObj$dataSet$cls))) {
           mSetObj$dataSet$cls
         } else {
           mSetObj$dataSet$meta.info[, 1]
         }

  cls    <- tolower(replace(as.character(cls), is.na(cls), "qc"))
  qc.inx <- cls == "qc"
  hasQC  <- any(qc.inx)


  ## ── RSD vectors ---------------------------------------------------------
  rsd_smp <- apply(raw[ , !qc.inx, drop = FALSE], 1, rsd_fun)
rsd_smp <- rm_outliers(rsd_smp[is.finite(rsd_smp)])



  if (hasQC) {
rsd_qc  <- apply(raw[ ,  qc.inx, drop = FALSE], 1, rsd_fun)
rsd_qc  <- rm_outliers(rsd_qc[is.finite(rsd_qc)])
    plt_df  <- data.frame(
      Class = factor(c(rep("Sample", length(rsd_smp)),
                       rep("QC",     length(rsd_qc))),
                     levels = c("Sample", "QC")),
      RSD   = c(rsd_smp, rsd_qc)
    )
    palette <- c(Sample = "#f8766d", QC = "#00c0c7")

    ## QC summary stats
    mSetObj$analSet$rsd.stats <- c(
      n          = length(rsd_qc),
      median     = median(rsd_qc),
      q25        = quantile(rsd_qc, 0.25),
      q75        = quantile(rsd_qc, 0.75),
      pct_lt_thr = mean(rsd_qc < thr) * 100
    )

  } else {
    plt_df  <- data.frame(Class = factor(rep("Sample", length(rsd_smp)),
                                         levels = "Sample"),
                          RSD   = rsd_smp)
    palette <- c(Sample = "#f8766d")
    mSetObj$analSet$rsd.stats <- NULL        # nothing QC-specific to save
  }

  ## ── graphics bookkeeping ------------------------------------------------
  imgName <- sprintf("%sdpi%d.%s", imgName, dpi, format)
  w <- if (is.na(width)) 6 else if (width == 0) 6 else width
  h <- w * 6 / 8
  mSetObj$imgSet$rsd.violin <- imgName

  ## ── draw ---------------------------------------------------------------
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")

  library(ggplot2)
  p <- ggplot(plt_df, aes(Class, RSD, fill = Class)) +
    geom_violin(trim = FALSE, colour = "black", size = 0.4) +
    geom_boxplot(width = .1, outlier.shape = NA, fill = "white") +
    geom_hline(yintercept = thr, linetype = "dashed",
               colour = "grey40", linewidth = 0.4) +   # reference line only
    scale_fill_manual(values = palette, guide = "none") +
    labs(y = "RSD (%)", x = NULL) +
    theme_minimal(base_size = 12)
  print(p);
  dev.off()
  invisible(.set.mSet(mSetObj))
}

## — add this helper just above the main function (or inside it) ------------
rm_outliers <- function(vec) {
  q  <- stats::quantile(vec, c(.25, .75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lo  <- q[1] - 1.5 * iqr        # lower fence
  hi  <- q[2] + 1.5 * iqr        # upper fence
  vec[vec >= lo & vec <= hi]     # keep values inside the fences
}

  rsd_fun <- function(x) {
    mu <- mean(x, na.rm = TRUE)
    if (is.na(mu) || mu == 0) return(NA_real_)
    100 * stats::sd(x, na.rm = TRUE) / abs(mu)
  }
