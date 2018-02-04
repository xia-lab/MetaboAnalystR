#'Data I/O for batch effect checking
#'@description Read multiple user uploaded CSV data one by one
#'format: row, col
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Read.BatchCSVdata<-function(mSetObj=NA, filePath, format, label){
  
  dat <- .readDataTable(filePath);
  
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
    if(!setequal(levels(cls.nms), levels(mSetObj$dataSet$batch.cls[[1]]))){
      AddErrMsg("The class labels in current data is different from the previous!");
      AddErrMsg(dup.nm);
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
  
  # replace NA
  minConc<-min(int.mat[int.mat>0], na.rm=T)/5;
  int.mat[is.na(int.mat)] <- minConc;
  
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

#'Set up two matrixes
#'@description One is a batch containing summed concentrations of each sample
#'the other contains the features aligned across all samples
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformBatchCorrection <- function(mSetObj=NA, imgName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # first get the common features from all batches and
  # set up class(or batch) labels for each samples after merge
  nms <- names(mSetObj$dataSet$batch);
  nm.list <- vector(length=length(mSetObj$dataSet$batch), mode="list");
  cls.lbls <- batch.lbls <- NULL; # record all batch labels
  for (i in 1:length(mSetObj$dataSet$batch)){
    batch <- mSetObj$dataSet$batch[[i]];
    cls <- as.character(mSetObj$dataSet$batch.cls[[i]]);
    nm.list[[i]] <- colnames(batch);
    cls.lbls <- c(cls.lbls,cls); 
    batch.lbls <- c(batch.lbls, rep(nms[i], length=nrow(batch)));
  }
  cm.nms <- Reduce(intersect, nm.list); # get common names
  
  # now align all the batches
  mSetObj$dataSet$batch <- lapply(mSetObj$dataSet$batch, function(x){x[,cm.nms]});
  commonMat <- do.call(rbind, mSetObj$dataSet$batch);
  batch.lbl <- factor(batch.lbls,levels=names(mSetObj$dataSet$batch), ordered=T);
  cls.lbl <- factor(cls.lbls);
  
  library('sva');
  pheno <- data.frame(cbind(cls.lbl, batch.lbl));
  modcombat <- model.matrix(~1, data=pheno);
  # note, transpose to fit the gene expression format
  combat_edata <- ComBat(dat=t(commonMat), batch=batch.lbl, mod=modcombat, par.prior=TRUE, prior.plots=FALSE)
  
  mSetObj$dataSet$commonMat <- commonMat;
  mSetObj$dataSet$batch.lbls <- batch.lbl;
  mSetObj$dataSet$cls.lbls <- cls.lbl;
  mSetObj$dataSet$adjusted.mat <- t(combat_edata);
  
  .set.mSet(mSetObj);
  
  PlotPCA.overview(mSetObj, imgName);
  
  # save the meta-dataset
  res <- data.frame(colnames(combat_edata), cls.lbl, batch.lbl, t(combat_edata));
  colnames(res) <- c('NAME', 'CLASS', 'Dataset', rownames(combat_edata));
  write.table(res, sep=",", file="MetaboAnalyst_batch_data.csv", row.names=F, quote=FALSE);
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return("T");
  }
  
  return(.set.mSet(mSetObj));
}

#'Scatter plot colored by different batches
#'@description Scatter plot colored by different batches
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPCA.overview <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w  <- 11;
  }else{
    w <- width;
  }
  h <- 6;
  
  mSetObj$imgSet$pca.batch.overview <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow=c(1,2));
  
  nlbls <-  mSetObj$dataSet$batch.lbls;
  pca <- prcomp( mSetObj$dataSet$commonMat, center=T, scale=T);
  sum.pca<-summary(pca);
  var.pca<-sum.pca$importance[2,]; # variance explained by each PC
  
  ## plot score plot
  pc1 = pca$x[, 1];
  pc2 = pca$x[, 2];
  
  xlabel = paste("PC1", "(", round(100*var.pca[1],1), "%)");
  ylabel = paste("PC2", "(", round(100*var.pca[2],1), "%)");
  
  semi.cols <- CreateSemiTransColors(mSetObj$dataSet$batch.lbls);
  plot(pc1, pc2, xlab=xlabel, ylab=ylabel, pch=21, bg=semi.cols, col="gray", cex=1.6, main="Before Adjustment");
  legend("topright", legend=unique(nlbls), pch=15, col=unique(semi.cols));
  
  qcInx <- substr(names(pc1), 0, 2) == "QC";
  if(sum(qcInx) > 0){
    points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, lwd=2);
  }
  
  pca <- prcomp( mSetObj$dataSet$adjusted.mat, center=T, scale=T);
  sum.pca<-summary(pca);
  var.pca<-sum.pca$importance[2,]; # variance explained by each PC
  
  ## plot score plot
  pc1 = pca$x[, 1];
  pc2 = pca$x[, 2];
  
  xlabel = paste("PC1", "(", round(100*var.pca[1],1), "%)");
  ylabel = paste("PC2", "(", round(100*var.pca[2],1), "%)");
  
  semi.cols <- CreateSemiTransColors( mSetObj$dataSet$batch.lbls);
  plot(pc1, pc2, xlab=xlabel, ylab=ylabel, pch=21, bg=semi.cols, col="gray", cex=1.6, main="After Adjustment");
  legend("topright", legend=unique(nlbls), pch=15, col=unique(semi.cols));
  
  qcInx <- substr(names(pc1), 0, 2) == "QC";
  if(sum(qcInx) > 0){
    points(pc1[qcInx], pc2[qcInx], pch=3, cex=2, lwd=2);
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
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