#### This script is used to contain the R funciton of duplicates extimation option in 'Other Utility' Module

Read.duplicatesDataTable <- function(mSet = NA, fileName, format){
  
  dat <- .readDataTable(fileName);
  
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
  dat[is.na(dat)] <- 0;
  mSetObj$dataSet$dataTable <- dat;
  mSetObj$dataSet$fileName <- fileName;
  .set.mSet(mSetObj);
  return ("T")
}

performDuplicateEstimation <- function(mSet = NA, format, method, smoothed){
  
  mSetObj <- .get.mSet(mSetObj);
  dat <- mSetObj$dataSet$dataTable;
  mSetObj$dataSet$fileName -> fileName;
  
  if(format == "col"){
    sampleNMs <- colnames(dat)[-1];
    dm <- t(sapply(sampleNMs, function(x) strsplit(x, "_(?!.*_)", perl=TRUE)[[1]]));
    dt <- t(apply(dat[,-1], 1, .estimateDP , dm = dm, smoother = smoothed, method = method))
    colnames(dt) <- unique(dm[,1]);
    datRes <- cbind(dat[,1], dt);
    colnames(datRes)[1] <- colnames(dat)[1]
  } else if(format == "row") {
    sampleNMs <- dat[,1];
    dm <- t(sapply(sampleNMs, function(x) strsplit(x, "_(?!.*_)", perl=TRUE)[[1]]));
    dat_t <- t(dat);
    colnames(dat_t) <- dat_t[1,];
    dat_t <- dat_t[-1,]
    rownames(dat_t) <- NULL;
    dt <- t(apply(dat_t, 1, .estimateDP , dm = dm, smoother = smoothed, method = method))
    colnames(dt) <- unique(dm[,1])
    datRes <- cbind(colnames(dat)[-1], dt);
    colnames(datRes)[1] <- colnames(dat)[1]
  }
  fast.write.csv(datRes, file = "MetaboAnalyst_duplicates_estimated_data.csv", row.names = F)
  return(1);
}

.estimateDP <- function(values, dm, smoother, method){
  res <- vector();
  
  for(i in seq_along(unique(dm[,1]))){
    idx <- which(unique(dm[,1])[i] == dm[,1]);
    val2estimate0 <- values[names(values) %in% rownames(dm)[idx]];
    val2estimate <- .estimatesCorrect(val2estimate0);

    r0 <- numeric();
    
    if(smoother){
      r0 <- density(as.numeric(val2estimate), bw = 0.5)$x
    } else {
      r0 <- as.numeric(val2estimate);
    }
    
    if(method == "mean"){
      res <- c(res, mean(r0))
    } else if(method == "max"){
      res <- c(res, max(r0))
    } else if(method == "min"){
      res <- c(res, min(r0))
    } else if(method == "median"){
      res <- c(res, median(r0))
    } else if(method == "1stQu"){
      res <- c(res, as.numeric(quantile(r0, 0.25)))
    } else if(method == "3rdQu"){
      res <- c(res, as.numeric(quantile(r0, 0.75)))
    } else if(method == "sum"){
      res <- c(res, sum(r0))
    } 
  }
  
  res[res < 0.0001] <- 0;
  return(res)
}

.estimatesCorrect <- function(values){
  
  condition1 <- (length(values) > 3); #if more than 3 samples
  condition2 <- (sum(values == 0) > 1); #if missing values more than 1
  condition3 <- (sum(values == 0)/length(values) >= 1/3); #if missing values over 1/3
  condition4 <- ((sd(values)/mean(values)) >= 1); #if the CV is over 1
  
  if(!condition1 & condition2){
    #if equal or less than 3 samples & missing values more than 1 (2 or more)
    values <- rep(0, length(values));
  } else if(!condition1 & !condition2){
    #if 1 value, missing or not do nothing
    #if 2 or 3 values, but 0 is missing, do nothing
    #if 2 values, but 1 is missing, in this case condition4 is TRUE, and do nothing
    #if 3 values, but 1 is missing, in this case condition3 is TRUE, if condition4 id TRUE, assign them as 0, otherwise, do nothing
    if(condition3 & condition4){
      values <- rep(0, length(values));
    }
  } else if(condition1 & !condition2){
    #if more than 3 values and missing values less than 1, do nothing
  } else if(condition1 & condition2){
    #if more than 3 values and missing values more than 1, in this case
    #if condition 3 is FALSE, do nothing; OTHERWISE, if condition4 is also TRUE--> data is too bad, assign as 0
    if(condition3 & condition4){
      values <- rep(0, length(values));
    }
  } 
  
  return(values)
}


