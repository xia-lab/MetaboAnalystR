my.filter.var <- function(mSetObj=NA, filter, qcFilter, rsd){
  
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
