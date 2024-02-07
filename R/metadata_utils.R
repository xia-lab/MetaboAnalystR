##################################################
## R script for ExpressAnalyst
## Description: functions for complex metadata
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#####'Sanity check metadata after metadata edited 
SanityCheckMeta <- function(fileName,init){
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  
  if(fileName == "NA"){
    sel.nms <- names(paramSet$mdata.all);
  }else{
    sel.nms <- c(fileName);
  }
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    meta <- dataSet$meta.info
    if(init==1){
      #rmidx=apply(meta, 2, function(x) any(is.na(x))|any(x=="NA")|any(x==""))
      #meta = meta[,!rmidx,drop=F]
      
    }else{
      if(any(is.na(meta))|any(meta=="")|any(meta=="NA")){
        return(2)
      }
    }
    # use first column by default
    cls <- meta[meta[,1]!="NA",1]
    
    # check class info
    cls.lbl <- as.factor(as.character(cls));
    min.grp.size <- min(table(cls.lbl));
    cls.num <- length(levels(cls.lbl));
    if(min.grp.size<2){
      msg <- paste0( "No replicates were detected for group  ",as.character(cls.lbl[which( table(cls.lbl)<2)])," in  ",colnames(meta)[1])
      msgSet$current.msg <- msg;
      saveSet(msgSet, "msgSet");
      return(0)
    }
    for(i in 1:ncol(meta)){
      meta[,i]=as.factor( meta[,i])
    }
    dataSet$cls <- cls.lbl
    dataSet$rmidx <- which(meta[,1]=="NA")
    dataSet$meta.info <- meta
    saveSet(msgSet, "msgSet");
    RegisterData(dataSet);
  }
  return(1);
}


# here should first try to load the original data
# the data in the memory could be changed
GetGroupNames <- function(dataName, meta="NA"){
  dataSet <- readDataset(dataName);
  if(meta == "NA"){
    grpnms = levels(factor(dataSet$meta.info[,1]));
  }else{
    grpnms =levels(factor(dataSet$meta.info[,meta]));
  }
  return(grpnms[grpnms!="NA"])
}

GetResRowNames <- function(dataName=""){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
    return(rownames(dataSet$meta.info));
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dataSet <- paramSet$dataSet;
    return(rownames(paramSet$dataSet$meta.info));
  }
}

GetResColNames <- function(dataName=""){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
    colnms<- colnames(dataSet$meta.info)[colnames(dataSet$meta.info)!="newcolumn"]
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    colnms<- colnames(paramSet$dataSet$meta.info)[colnames(paramSet$dataSet$meta.info)!="newcolumn"]
  }
  return(colnms);
}

GetDiscMetas <- function(dataName=""){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dataSet <- paramSet$dataSet;
  }
  if(length(keepVec)>0){
    keepidx <- which(keepVec %in% colnames(dataSet$meta.info))
    keepidx <- intersect(keepidx,which(dataSet$disc.inx))
  }else{
    print(dataSet$disc.inx);
    keepidx <-  which(dataSet$disc.inx)
  }
  colnms<- colnames(dataSet$meta.info)[keepidx]
  #print(colnms)
  return(colnms);
}

GetMetaDataCol <- function(dataName="",colnm){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dataSet <- paramSet$dataSet;
  }
  cls = levels(dataSet$meta.info[,colnm]);
  return(cls[cls!="NA"]);
}

GetMetaCell <- function(dataName="",ridx=1,cidx=1){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dataSet <- paramSet$dataSet;
  }
  return(dataSet$meta.info[ridx,cidx]);
}

ResetMetaTab <- function(dataName=""){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
     if(dataSet$type=="prot"){
       data.anot <- qs::qread("data.missed.qs");
    }else{
       data.anot <- qs::qread("orig.data.anot.qs");
    }
    dataSet$data.norm <- dataSet$data.anot <- data.anot;
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    sel.nms <- names(paramSet$mdata.all);
    for(i in 1:length(sel.nms)){
      dataSet <- readDataset(sel.nms[i]);
      dataSet$data.norm <- dataSet$data.anot <- readDataQs("data.annotated.qs", paramSet$anal.type, sel.nms[i]);
      RegisterData(dataSet);
    }
    dataSet <- paramSet$dataSet;
  }
  dataSet$meta.info <- dataSet$metaOrig;
  dataSet$disc.inx <- dataSet$disc.inx.orig;
  dataSet$cont.inx <- dataSet$cont.inx.orig;
  RegisterData(dataSet);
}

GetResColType <- function(dataName="",colNm="NA"){
  if(dataName != "NA"){
    dat <- readDataset(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dat <- paramSet$dataSet;
  }
  
  if(colNm=="NA"){
    meta.status <- ifelse(dat$disc.inx,"disc","cont")
  }else{
    meta.status <- ifelse(dat$disc.inx[colNm],"disc","cont")
  }
  
  if(length(meta.status == 0)){
    meta.status <- rep(T, ncol(dat$meta.info))
    names(meta.status) <- colnames(dat$meta.info);
    dat$disc.inx <- meta.status;
    if(dataName != "NA"){
      RegisterData(dat);
    }else{
      paramSet$dataSet <- dat;
      saveSet(paramSet);
    }
  }
  return(meta.status);
}

UpdateMetaType <-function(dataName="", metadata="NA", type="disc"){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
    dataSet$meta.types[metadata] = type;
  }else{
    paramSet <- readSet(paramSet, "paramSet"); 
    paramSet$dataSet$meta.types[metadata] = type;
  }
  return(.set.rdt.set(rdtSet));
}

GetUniqueMetaNames <-function(dataName="",metadata){
  paramSet <- readSet(paramSet, "paramSet"); 
  data.type <- paramSet$dataSet[["meta.types"]][metadata];
  if(data.type == "cont"){
    return("--- NA ---");
  } else {
    return(levels(as.factor(paramSet$dataSet$meta.info[,metadata])));
  }
}

UpdateMetaStatus <- function(dataName="",colNm){
  dataSet <- readDataset(dataName);
  msgSet <- readSet(msgSet, "msgSet");
  cidx <- which(colnames(dataSet$meta.info)==colNm)
  old = ifelse(dataSet$disc.inx[cidx],"Discrete","Continuous")
  if(dataSet$disc.inx[cidx]){
    if(all(is.na( as.numeric(as.character(dataSet$meta.info[,cidx]))))){
      msgSet$current.msg <- "Category metadata cannot be continuous data!"
      saveSet(msgSet, "msgSet"); 
      return(0)
    }
    dataSet$disc.inx[cidx]=FALSE;
    dataSet$cont.inx[cidx]=TRUE;
  }else{
    if(all(!duplicated(as.character(dataSet$meta.info[,cidx])))){
      msgSet$current.msg <- "No duplicates were detected! The metadata cannot be discrete!"
      saveSet(msgSet, "msgSet"); 
      return(0)
    }
    dataSet$disc.inx[cidx]=TRUE;
    dataSet$cont.inx[cidx]=FALSE;
  }
  new = ifelse(dataSet$disc.inx[cidx],"Discrete","Continuous")
  msgSet$current.msg <- paste0("Metadata type of ",colnames(dataSet$meta.info)[cidx]," has been changed to ", new, " !")
  saveSet(msgSet, "msgSet"); 
  RegisterData(dataSet);
  return(1);
}


DeleteSample <- function(dataName="",samplNm){
  #print(dataName)
  #print(samplNm)
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
    dataSet$meta.info <- dataSet$meta.info[rownames(dataSet$meta.info)!=samplNm,,drop=F]
    dataSet$data.norm <- dataSet$data.norm[,colnames(dataSet$data.norm!=samplNm)]
    RegisterData(dataSet);
  }else{
    paramSet <- readSet(paramSet, "paramSet")  
    dataName <- paramSet$dataSet$meta.info$Dataset[rownames(paramSet$dataSet$meta.info)==samplNm];
    paramSet$dataSet$meta.info <- paramSet$dataSet$meta.info[rownames(paramSet$dataSet$meta.info)!=samplNm,,drop=F];
    
    dataSet <- readDataset(dataName);
    dataSet$data.norm <- dataSet$data.norm[,colnames(dataSet$data.norm)!=samplNm];
    dataSet$meta.info <- dataSet$meta.info[rownames(dataSet$meta.info)!=samplNm,,drop=F];
    
    inmex.meta<-qs::qread("inmex_meta.qs");
    inmex.meta$data <- inmex.meta$data[,colnames(inmex.meta$data) !=samplNm]
    qs::qsave(inmex.meta, "inmex_meta.qs");
    saveSet(paramSet, "paramSet");
    RegisterData(dataSet);
  }
  
  return(1);
}

DeleteMetaCol <- function(dataName="",metaCol){
  if(dataName != "NA"){
    sel.nms <- c(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")  
    sel.nms <- names(paramSet$mdata.all);
  }
  
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    idx = which(colnames(dataSet$meta.info)==metaCol)
    dataSet$meta.info <- dataSet$meta.info[,-idx,drop=F]
    dataSet$disc.inx <- dataSet$disc.inx[-idx]
    dataSet$cont.inx <- dataSet$cont.inx[-idx]
    if(!exists("rmMetaCol",dataSet)){
      dataSet$rmMetaCol <- vector()
    }
    dataSet$rmMetaCol <- unique(c(dataSet$rmMetaCol,metaCol))
    RegisterData(dataSet);
  }
  
  #for meta-anal also remove from meta.info
  if(dataName == "NA"){
    paramSet <- readSet(paramSet, "paramSet");
    idx = which(colnames(paramSet$dataSet$meta.info)==metaCol);
    paramSet$dataSet$meta.info <- paramSet$dataSet$meta.info[,-idx,drop=F];
    paramSet$dataSet$disc.inx <- paramSet$dataSet$disc.inx[-idx]
    paramSet$dataSet$cont.inx <- paramSet$dataSet$cont.inx[-idx]
  } 
  
  return(1);
}

CleanRmCol <- function(dataName=""){
  if(dataName != "NA"){
    paramSet <- readSet(paramSet, "paramSet")
    mdata.all <- paramSet$mdata.all
    sel.nms <- names(mdata.all);
  }else{
    sel.nms <- c(dataName);
  }
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    if(exists("rmMetaCol",dataSet)){
      dataSet$rmMetaCol <- vector()
    }
    RegisterData(dataSet);
  }
  return(1);
}

GetSampleNm <- function(dataName="",ridx=1){
  if(dataName != "NA"){
    dataSet <- readDataset(dataName);
  }else{
    paramSet <- readSet(paramSet, "paramSet")
    dataSet <- paramSet$dataSet;
  }
  return( rownames(dataSet$meta.info)[ridx]);
}


UpdateSampInfo <-  function(dataName="",rowNm,colNm,cell){
  if(dataName == "NA"){
    paramSet <- readSet(paramSet, "paramSet"); 
    dataName <- paramSet$dataSet$meta.info$Dataset[rownames(paramSet$dataSet$meta.info)==rowNm];
    if(colNm==""){
      rownames(paramSet$dataSet$meta.info)[rownames(paramSet$dataSet$meta.info)==rowNm] <- cell;
      saveSet(paramSet, "paramSet");
    }
  }
  
  dataSet <- readDataset(dataName);
  meta <- dataSet$meta.info
  ridx <- which(rownames(meta)==rowNm)
  if(colNm==""){
    if(rowNm != cell){
      rownames(meta)[ridx]=cell
      colnames(dataSet$data.norm)[which(colnames(dataSet$data.norm)==rowNm)] <- cell 
      if(exists("dataSet$data.anot")){
        colnames(dataSet$data.anot)[which(colnames(dataSet$data.anot)==rowNm)] <- cell 
      }
    }
  }else{  
    cidx<- which(colnames(meta)==colNm)
    if(cell!= as.character(meta[ridx,cidx])){
      if(cell %in% levels(meta[,cidx])){
        meta[ridx,cidx] = cell
      }else{
        levels(meta[,cidx]) <- c(levels(meta[,cidx]), cell)
        meta[ridx,cidx] = cell
      }
      meta[,cidx] <- droplevels(meta[,cidx])
    }
  }
  dataSet$meta.info = meta
  RegisterData(dataSet);
  return(1);
}


GetSelectedMetaInfo <- function(dataName="",colNm){
  dataSet <- readDataset(dataName);
  lvls <- levels(dataSet$meta.info[,colNm])
  lvls <-  lvls[lvls!="NA"]
  return(lvls);
}

UpdateMetaOrder <- function(dataName="",metacol){
  dataSet <- readDataset(dataName);
  if(length(metaVec)>0 & metacol %in% colnames(dataSet$meta.info)){
    dataSet$meta.info[,metacol] <- factor(as.character(dataSet$meta.info[,metacol]),levels = metaVec)
    
  }else{
    msgSet <- readSet(msgSet, "msgSet");
    msgSet$current.msg <- "The metadata column is empty! Please check your selection!"
    saveSet(msgSet, "msgSet"); 
    return(0)
  }
  RegisterData(dataSet);
  return(1)
}

UpdateMetaName <-  function(dataName="",oldvec,newvec){
  if(dataName == "NA"){
    sel.nms <- names(paramSet$mdata.all);
    paramSet <- readSet(paramSet, "paramSet");
    idx <- which(colnames(paramSet$dataSet$meta.info)==oldvec)
    if(length(idx)==1){
      colnames(paramSet$dataSet$meta.info)[idx] <- names(paramSet$dataSet$disc.inx)[idx] <- 
        names(paramSet$dataSet$cont.inx)[idx] <- newvec
    }else{
      return(0)
    }
    saveSet(paramSet, "paramSet");
  }else{
    sel.nms <- c(dataName);
  }
  for(i in 1:length(sel.nms)){
    dataSet <- readDataset(sel.nms[i]);
    idx <- which(colnames(dataSet$meta.info)==oldvec)
    if(length(idx)==1){
      colnames(dataSet$meta.info)[idx] <- names(dataSet$disc.inx)[idx] <- 
        names(dataSet$cont.inx)[idx] <- newvec
    }else{
      return(0)
    }
    RegisterData(dataSet);
  }
  return(1);
}

GetMetaSummary <- function(dataName=""){
  dataSet <- readDataset(dataName);
  meta <- dataSet$meta.info
  disc.vec <- paste(names(dataSet$disc.inx)[which(dataSet$disc.inx)],collapse=", ")  
  cont.vec <- paste(names(dataSet$cont.inx)[which(dataSet$cont.inx)],collapse=", ")  
  na.vec <- na.check(meta)
  res <- c(ncol(meta),length(which(dataSet$disc.inx)),disc.vec,
           length(which(dataSet$cont.inx)),cont.vec,names(meta)[1],length(unique(meta[,1])),paste(unique(meta[,1]),collapse=", "),na.vec )
  paramSet$metadata.summary <- res;
  saveSet(paramSet);
  return(res);
}

na.check <- function(mydata){
  na.idx <- apply(mydata,2,function(x) "NA" %in% x)
  if(all(!na.idx)){
    return("None")
  }
  na.num <- apply(mydata,2,function(x) length(which(x=="NA")))
  naInfo <- data.frame(names(mydata)[na.idx],num = na.num[na.num>0])
  naInfo <- apply(naInfo, 1, function(x) paste0(x[1]," (",x[2],")"))
  naInfo <- paste(naInfo,collapse = ", ")
  return(naInfo)
}


UpdatePrimaryMeta <- function(fileName,primaryMeta){
  dataSet <- readDataset(fileName);
  msgSet <- readSet(msgSet,"msgSet");
  meta <- dataSet$meta.info
  if(primaryMeta %in% colnames(meta)){
    cidx <- which(colnames(meta)==primaryMeta)
    dataSet$meta.info<-cbind(meta[,cidx,drop=F],meta[,-cidx,drop=F])
    dataSet$disc.inx=c(dataSet$disc.inx[cidx],dataSet$disc.inx[-cidx])
    dataSet$cont.inx=c(dataSet$cont.inx[cidx],dataSet$cont.inx[-cidx])
  }else{
    msgSet$current.msg <- "The metadata column is empty! Please check your selection!"
    saveSet(msgSet, "msgSet"); 
    return(0)
  }
  RegisterData(dataSet);
  return(1)
}


GetMetaDataGroups <- function(dataName){
  paramSet <- readSet(paramSet, "paramSet");
  groups <- colnames(paramSet$dataSet$meta.info);
  return(groups);
}

GetMetaDataStatus <- function(dataName){
  paramSet <- readSet(paramSet, "paramSet");
  res <- unname(paramSet$dataSet$meta.status);
  return(res);
}


GetMetaTypes <- function(colNm="NA"){
  paramSet <- readSet(paramSet, "paramSet");
  if(colNm=="NA"){
    meta.types <- paramSet$dataSet$meta.types
  }else{
    meta.types <- paramSet$dataSet$meta.types[colNm]
  }
  return(unname(meta.types));
}

GetPrimaryType <- function(analysis.var){
  paramSet <- readSet(paramSet, "paramSet");
  primary.type <- unname(paramSet$dataSet$meta.types[analysis.var]);
  return(primary.type);
}