
my.impute.missing <- function(mSetObj=NA, method="min"){
  
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
