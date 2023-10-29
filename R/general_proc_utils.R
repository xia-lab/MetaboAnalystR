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
        if(!(mSetObj$dataSet$type=="conc" | mSetObj$dataSet$type=="specbin" | mSetObj$dataSet$type=="pktable" )){
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
        if(anal.type != "network" & anal.type != "mf"){ # add exception for DSPC correlation network 
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
  msg<-c(msg, "<u>By default, missing values will be replaced by 1/5 of min positive values of their corresponding variables</u>",
         "Click the <b>Proceed</b> button if you accept the default practice;",
         "Or click the <b>Missing Values</b> button to use other methods.");
  
  mSetObj$dataSet$proc.cls <- mSetObj$dataSet$cls <- mSetObj$dataSet$orig.cls;

  if(is.null(mSetObj$dataSet$meta.info)){
    mSetObj$dataSet$meta.info <- data.frame(mSetObj$dataSet$cls);
    colnames(mSetObj$dataSet$meta.info) = "Class";
  }
  
  # make sure the meta.info is synchronized with data
  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
    my.sync <- .sync.data.metadata(int.mat, mSetObj$dataSet$meta.info);
    int.mat <- my.sync$data;
    mSetObj$dataSet$meta.info <- my.sync$metadata;
  }

  qs::qsave(as.data.frame(int.mat), "preproc.qs");
  
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
  mSetObj$dataSet$filt <- mSetObj$dataSet$edit <- NULL;
  
  # replace zero and missing values using Detection Limit for each variable 
  preproc <- qs::qread("preproc.qs");
  int.mat <- ReplaceMissingByLoD(preproc);  
  
  # note, this is last step of processing, also save to proc
  #mSetObj$dataSet$proc <- as.data.frame(int.mat);
  mSetObj$dataSet$proc.feat.num <- ncol(int.mat);
  qs::qsave(as.data.frame(int.mat), file="data_proc.qs");

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
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.impute.missing")){ # public web on same user dir
      .load.scripts.on.demand("util_missing.Rc");    
    }
    return(my.impute.missing(mSetObj, method));
  }else{
    return(my.impute.missing(mSetObj, method));
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
#'@param filter.cutoff percent to be filtered, for example, 5 (5\%)
#'@param qc.filter Filter the variables based on QC samples - True (T), or use non-QC based filtering - False (F).  
#'@param rsd Define the relative standard deviation cut-off. Variables with a RSD greater than this number
#'will be removed from the dataset. It is only necessary to specify this argument if qc.filter is True (T). 
#'Otherwise, it will not be used in the function.
#'@param privileged use could keep this option as True (T) for local MetaboAnalystR
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

FilterVariable <- function(mSetObj=NA, qc.filter="F", rsd, var.filter="iqr", var.cutoff=NULL, int.filter="mean", int.cutoff=0){

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
  
  msg <- "";
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
      if(mSetObj$analSet$type == "mummichog"){
        msg <- paste("Removed ", sum(!gd.inx), " features based on QC RSD values. QC samples are excluded from downstream functional analysis.");
      }else{
        msg <- paste("Removed ", sum(!gd.inx), " features based on QC RSD values. QC samples are still kept. You can remove them later.");
      }
    }else if(sum(qc.hits) > 0){
      AddErrMsg("RSD requires at least 2 QC samples, and only non-QC based filtering can be applied.");
      return(0);
    }else{
      AddErrMsg("No QC Samples (with class label: QC) found.  Please use non-QC based filtering.");
      return(0);
    }
  }

  # no explicit user choice, will apply default empirical filtering based on variance
  if(is.null(var.cutoff)){ 
    var.cutoff <- .computeEmpiricalFilterCutoff(ncol(int.mat), mSetObj$analSet$type);
  }

  if(var.cutoff > 0){ 
     filt.res <- PerformFeatureFilter(int.mat, var.filter, var.cutoff, mSetObj$analSet$type);
     int.mat <- filt.res$data;
     msg <- paste(msg, filt.res$msg);
  }

  if(int.cutoff > 0){ 
     filt.res <- PerformFeatureFilter(int.mat, int.filter, int.cutoff, mSetObj$analSet$type);
     int.mat <- filt.res$data;
     msg <- paste(msg, filt.res$msg);
  }

  mSetObj$dataSet$filt <- int.mat;

  if(msg == ""){
     msg <- "No data filtering is performed."
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


