my.save.data <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  data.type <- mSetObj$dataSet$type
  max.col <- 50; # transpose if more than this 

  # mummichog data is not processed, so just mSet$orig and mSet$proc
  
  if(anal.type=="mummichog"){
    fast.write.csv(mSetObj$dataSet$mummi.orig, file="data_original.csv", row.names = FALSE);
    fast.write.csv(mSetObj$dataSet$mummi.proc, file="data_processed.csv", row.names = FALSE);
  } else if(anal.type=="metapaths") {
    dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
    res <- sapply(dataNMs, FUN= function(x) {
        dataNM <- mSetObj[[x]][["name"]];
        fileDataNM <- sub(pattern = "(.*)\\..*$", replacement = "\\1", dataNM);
        dt <- mSetObj[[x]]$norm;
        fast.write.csv(dt, file=paste0(fileDataNM, "_norm.csv"));
        dtmp <- mSetObj[[x]]$mummi.proc;
        fast.write.csv(dtmp, file=paste0(fileDataNM, "_processed.csv"));
        return(1)})
    if(all(res == 1)) print("Data saving successfully!")
  } else {
    if(file.exists("data_orig.qs")){

      orig.data<- qs::qread("data_orig.qs");

      tsFormat <- substring(mSetObj$dataSet$format,4,5)=="mf";
      # combine for single factor, while keep separate for metadata 
      if(!tsFormat){
        lbls <- cbind("Label"= as.character(mSetObj$dataSet$orig.cls));
        orig.data<-cbind(lbls, orig.data);
      }else{
        meta.orig <- mSetObj$dataSet$orig.meta.info;
        orig.data <- orig.data[match(rownames(meta.orig), rownames(orig.data)), ]
        #orig.data<-cbind(meta.orig, orig.data);
        fast.write.csv(meta.orig, file="metadata_original.csv");
      }

      if(dim(orig.data)[2]>max.col){ 
        orig.data<-t(orig.data);
      }
        
      fast.write.csv(orig.data, file="data_original.csv");

      if(file.exists("data_proc.qs")){
        if(!is.null(mSetObj$dataSet[["filt"]])){
          if(!tsFormat){
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$filt.cls));
          }
          proc.data<-mSetObj$dataSet$filt;  
        }else{
          if(!tsFormat){
            lbls <- cbind("Label"= as.character(mSetObj$dataSet$proc.cls));
          }
          proc.data <- qs::qread("data_proc.qs");
        }
        if(!tsFormat){
            proc.data<-cbind(lbls, proc.data);
        }else{
            meta.proc <- mSetObj$dataSet$meta.info;
            proc.data <- proc.data[match(rownames(meta.proc), rownames(proc.data)), ]
            #proc.data<-cbind(meta.proc, proc.data);
            fast.write.csv(meta.proc, file="metadata_processed.csv");
        }
        if(dim(proc.data)[2]>max.col){
          proc.data<-t(proc.data);
        }
        fast.write.csv(proc.data, file="data_processed.csv");

        ## ── Write data_processed_input.csv with metadata row ──
        

        if(!is.null(mSetObj$dataSet[["norm"]])){
          if(!tsFormat){
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

           if(!tsFormat){
               norm.data<-cbind(lbls, norm.data);
            }else{
                meta.norm <- mSetObj$dataSet$meta.info;
                norm.data <- norm.data[match(rownames(meta.norm), rownames(norm.data)), ]
                #norm.data<-cbind(meta.norm, norm.data);
                fast.write.csv(meta.norm, file="metadata_normalized.csv");
            }
            if(dim(norm.data)[2]>max.col){
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

