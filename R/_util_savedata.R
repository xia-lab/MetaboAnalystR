my.save.data <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  data.type <- mSetObj$dataSet$type
  
  # mummichog data is not processed, so just mSet$orig and mSet$proc
  
  if(anal.type=="mummichog"){
    fast.write.csv(mSetObj$dataSet$mummi.orig, file="data_original.csv", row.names = FALSE);
    fast.write.csv(mSetObj$dataSet$mummi.proc, file="data_processed.csv", row.names = FALSE);
  }else{
    if(file.exists("data_orig.qs")){
      lbls <- NULL;
      tsFormat <- substring(mSetObj$dataSet$format,4,5)=="ts";
      if(tsFormat){
        lbls <- cbind(as.character(mSetObj$dataSet$orig.facA),as.character(mSetObj$dataSet$orig.facB));
        colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      }else{
        lbls <- cbind("Label"= as.character(mSetObj$dataSet$orig.cls));
      }
      
      orig.data<- qs::qread("data_orig.qs");
      orig.data<-cbind(lbls, orig.data);

      if(dim(orig.data)[2]>200){
        orig.data<-t(orig.data);
      }
        
      fast.write.csv(orig.data, file="data_original.csv");
      
      if(!is.null(mSetObj$dataSet[["proc"]])){
        
        if(!is.null(mSetObj$dataSet[["filt"]])){
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$filt.facA),as.character(mSetObj$dataSet$filt.facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$filt.cls));
          }
          proc.data<-mSetObj$dataSet$filt;  
        }else{
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$proc.facA),as.character(mSetObj$dataSet$proc.facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$proc.cls));
          }
          proc.data<-mSetObj$dataSet$proc;
        }
        
        # convert back to original names
#        if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
#          colnames(proc.data) <- orig.var.nms[colnames(proc.data)];
#        }
        proc.data<-cbind(lbls, proc.data);
        
        if(dim(proc.data)[2]>200){
          proc.data<-t(proc.data);
        }
        fast.write.csv(proc.data, file="data_processed.csv");
        
        if(!is.null(mSetObj$dataSet[["norm"]])){
          if(tsFormat){
            lbls <- cbind(as.character(mSetObj$dataSet$facA),as.character(mSetObj$dataSet$facB));
            colnames(lbls) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
          }else{
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$cls));
          }
          
          norm.data <- mSetObj$dataSet$norm;
          
          # for ms peaks with rt and ms, insert two columns, without labels
          # note in memory, features in columns
          if(!is.null(mSetObj$dataSet$three.col)){ 
            ids <- matrix(unlist(strsplit(colnames(norm.data), "/", fixed=TRUE)),ncol=2, byrow=T);
            colnames(ids) <- c("mz", "rt");
            new.data <- data.frame(ids, t(norm.data));
            fast.write.csv(new.data, file="peak_normalized_rt_mz.csv");
          }else{
            # convert back to original names
#           if(!data.type %in% c("nmrpeak", "mspeak", "msspec")){
#              colnames(norm.data) <- orig.var.nms[colnames(norm.data)];   
#            }
            norm.data<-cbind(lbls, norm.data);
            if(dim(norm.data)[2]>200){
              norm.data<-t(norm.data);
            }
            fast.write.csv(norm.data, file="data_normalized.csv");
          }
        }
      }
    }
  }
  return(.set.mSet(mSetObj));
}

