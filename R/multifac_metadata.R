#' ReadMetaData
#'
#' @param mSetObj metaboanalyst object, initialized by InitDataObjects("pktable", "mf", FALSE)
#' @param metafilename file path of data
#' @export
ReadMetaData <- function(mSetObj=NA, metafilename){
  mSetObj <- .get.mSet(mSetObj);
  
  metadata <- .readDataTable(metafilename, FALSE);
  metadata[is.na(metadata)] = "NA"
  if(class(metadata) == "try-error"){
    AddErrMsg("Failed to read in the metadata file! Please make sure that the metadata file is in the right format and does not have empty cells or contains NA.");
    return(0);
  }

  # at least 3 columns (two metadata + sample names)
  if(ncol(metadata) < 3){
    AddErrMsg("At least two metadata is required for this module! Please use <b>Statistical Analysis [one factor]</b> module instead");
    return(0);
  }

  # need to add metadata sanity check
  # are sample names identical to data$orig
  # order samples in same way as in abundance table

  smpl.nms <- metadata[,1];
  data.smpl.nms <- names(mSetObj$dataSet$url.smp.nms);

  nm.hits <- data.smpl.nms %in% smpl.nms;
  if(!all(nm.hits)){
    perct <- round(sum(!nm.hits)/length(data.smpl.nms)*100, 3);
    AddErrMsg(paste0("A total of ", sum(!nm.hits), " or (", perct, "%) sample names are not present in the metadata file!" ));
    mis.nms <- data.smpl.nms[!nm.hits];
    AddErrMsg(paste0("Sample names missing in metadata file:", paste(mis.nms, collapse="; ")));
    #AddErrMsg(paste0("Sample names [data file]:", paste(data.smpl.nms, collapse="; ")));
    #AddErrMsg(paste0("Sample names [metadata file]:", paste(smpl.nms, collapse="; ")));
    return(0);
  }
 
  mSetObj$dataSet$meta.types <- rep("disc", ncol(metadata) - 1);
  mSetObj$dataSet$meta.status <- rep("OK", ncol(metadata) - 1);
  names(mSetObj$dataSet$meta.status) <- colnames(metadata)[-1];

  # now remove extra meta if present, and order them
  nm.hits2 <- which(smpl.nms %in% data.smpl.nms);
  metadata1 <- metadata[nm.hits2,];
  smpl.nms <- smpl.nms[nm.hits2];

  # now get cleaned sample names based on data
  smpl.nms <- mSetObj$dataSet$url.smp.nms[smpl.nms];

  metadata1 <- metadata1[,-1];
  metadata1[] <- lapply( metadata1, factor);

  if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
    # determine time factor
    if(!"time" %in% tolower(colnames(metadata1))){
      AddErrMsg("No time points found in your data");
      AddErrMsg("The time points group must be labeled as <b>Time</b>");
      return(0);
    }

    if((mSetObj$dataSet$design.type =="time0" && ncol(metadata1) != 2 ) || !"subject" %in% tolower(colnames(metadata1)) ){
      AddErrMsg("Make sure the metadata table contains two columns named: time and subject!");
      return(0)
    }

    if((mSetObj$dataSet$design.type =="time") && (ncol(metadata1) != 3 || !"subject" %in% tolower(colnames(metadata1)) || !"phenotype" %in% tolower(colnames(metadata1)))){
      AddErrMsg("Make sure the metadata table contains three columns named: time, phenotype and subject");
      return(0)
    }

    # determine time factor and should order first by subject then by each time points
    time.inx <- which(tolower(colnames(metadata1)) == "time");
    sbj.inx <- which(tolower(colnames(metadata1)) == "subject");

    # check if balanced
    timeFreq <- table(metadata1[, time.inx]);
    sbjFreq <- table(metadata1[, sbj.inx]);

    timeBalanced <- min(timeFreq) == max(timeFreq);
    sbjFreq <- min(sbjFreq) == max(sbjFreq)
    
    if(!timeBalanced){
      AddErrMsg("Make sure to have equal replicates for each time points: ");
      AddErrMsg(paste("Found min: ", min(timeFreq), " max: ", max(timeFreq)));
      AddErrMsg("Maybe specify study design as Multiple factors / covariates");
      return(0)
    }

    if(!sbjFreq){
      AddErrMsg("Make sure to have equal replicates for each subjects:");
      AddErrMsg(paste("Found min: ", min(sbjFreq), " max: ", max(sbjFreq)));
      AddErrMsg("Maybe specify study design as Multiple factors / covariates");
      return(0)
    }

    enoughRep <- min(timeFreq) > 2 

    if(!enoughRep){
      AddErrMsg("At least 3 time points are required.");
      AddErrMsg("Maybe specify study design as Multiple factors / covariates");
      return(0)
    }

    ordInx <- order(metadata1[,sbj.inx], metadata1[,time.inx])

    metadata1 <- metadata1[ordInx,]
    smpl.nms <- smpl.nms[ordInx]
    time.fac <- metadata1[,time.inx]
    exp.fac <- metadata1[,-time.inx]
    if(ncol(metadata1)>2){
      exp.fac <- exp.fac[,1]
    }

    mSetObj$dataSet$time.fac <- time.fac;
    mSetObj$dataSet$exp.fac <- exp.fac;
    if(mSetObj$dataSet$design.type =="time"){
        cls.inx <- which(!(tolower(colnames(metadata1)) %in% c("time","subject")));
        metadata1 <- metadata1[,c(cls.inx, time.inx, sbj.inx)];

        facA.lbl <- colnames(metadata1)[1];
        cls.lbl <- facA <- metadata1[,1];
        mSetObj$dataSet$facA.type <- is.numeric(facA);
        mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA <- as.factor(as.character(facA));
        mSetObj$dataSet$facA.lbl <- facA.lbl;

        facB.lbl <- colnames(metadata1)[2];
        facB <- metadata1[,2];    
        mSetObj$dataSet$facB.type <- is.numeric(facB);
        mSetObj$dataSet$orig.facB <- mSetObj$dataSet$facB <- as.factor(as.character(facB));
        mSetObj$dataSet$facB.lbl <- facB.lbl;

    }else{ # time0
        metadata1 <- metadata1[,c(time.inx, sbj.inx)];
        facA.lbl <- colnames(metadata1)[1];
        cls.lbl <- facA <- metadata1[,1];
        mSetObj$dataSet$facA.type <- is.numeric(facA);
        mSetObj$dataSet$orig.facA <- mSetObj$dataSet$facA <- as.factor(as.character(facA));
        mSetObj$dataSet$facA.lbl <- facA.lbl;
    }
  }

  rownames(metadata1) <- smpl.nms;
  mSetObj$dataSet$meta.info <- mSetObj$dataSet$orig.meta.info <- metadata1;
  mSetObj$dataSet$types.cls.lbl <- sapply(metadata1, function(x) class(x) ) 
  mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- metadata1[,1]
  names(mSetObj$dataSet$meta.types) <- colnames(mSetObj$dataSet$meta.info)
  names(mSetObj$dataSet$types.cls.lbl) <- colnames(mSetObj$dataSet$meta.info)
  mSetObj$dataSet$type.cls.lbl <- class(mSetObj$dataSet$meta.info[,1]);
  return(.set.mSet(mSetObj));
}

#' Read.TextDataTs
#' @description Read.TextDataTs is used to read metabolomics data for co-vairiate analysis
#' @param mSetObj metaboanalyst object, initialized by InitDataObjects("pktable", "mf", FALSE)
#' @param filePath file path of data
#' @param format format of data table, can be "rowu" or "colu"
#' @export
Read.TextDataTs <- function(mSetObj=NA, filePath, format="rowu"){
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$format <- format;
  
  dat <- .readDataTable(filePath);
  
  if(class(dat) == "try-error" || ncol(dat) == 1){
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    AddErrMsg("Make sure the file delimeters are commas.");

    # now try to extract something for viewing
    tryCatch({
        fileConn <- file(filePath, encoding = "UTF-8");
        text <- readLines(fileConn, n=100); # max 100 lines
        write.csv(text, file="raw_dataview.csv");
    },
    error = function(e) return(e),
    finally = {
        close(fileConn)
    });

    return(0);
  }
  
  # save a table output at the earliest time for viewing
  row.num <- nrow(dat);
  col.num <- ncol(dat);
  if(row.num > 100){
      row.num <- 100;
  }
  if(col.num > 10){
      col.num <- 10;
  }
  write.csv(dat[1:row.num, 1:col.num], file="raw_dataview.csv");

  msg <- NULL;
  
  # two factor time series data
  if(substring(format,1,3)=="row"){ # sample in row
    msg<-c(msg, "Samples are in rows and features in columns");
    smpl.nms <-dat[,1];
    all.nms <- colnames(dat);
    #facA.lbl <- all.nms[2];
    #cls.lbl <- facA <- dat[,2]; # default assign facA to cls.lbl in order for one-factor analysis
    #facB.lbl <- all.nms[3];
    #facB <- dat[,3];
    conc <- dat[,-1]; #dat[,-c(1:3)];
    var.nms <- colnames(conc);
  }else{ # sample in col
    msg<-c(msg, "Samples are in columns and features in rows.");
    all.nms <- dat[,1];
    conc<-t(dat[,-1]);
    var.nms <- dat[,1];
    smpl.nms <- rownames(conc);
  }
  
  
  empty.inx <- is.na(var.nms) | var.nms == "";
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty features</font> were detected and excluded from your data."));
    var.nms <- var.nms[!empty.inx];
    conc <- conc[,!empty.inx];
  }
  
  if(length(unique(var.nms))!=length(var.nms)){
    dup.nm <- paste(var.nms[duplicated(var.nms)], collapse=" ");
    AddErrMsg("Duplicate feature names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  if(sum(is.na(iconv(smpl.nms)))>0){
    na.inx <- is.na(iconv(smpl.nms));
    nms <- paste(smpl.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
    return(0);
  }
  
  if(sum(is.na(iconv(var.nms)))>0){
    na.inx <- is.na(iconv(var.nms));
    nms <- paste(var.nms[na.inx], collapse="; ");
    AddErrMsg(paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
    return(0);
  }
  
  url.smp.nms <- CleanNames(smpl.nms);
  names(url.smp.nms) <- smpl.nms;
  
  url.var.nms <- CleanNames(var.nms); # allow space, comma and period
  names(url.var.nms) <- var.nms;
  
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  mSetObj$dataSet$url.var.nms <- url.var.nms;
  mSetObj$dataSet$url.smp.nms <- url.smp.nms;
  
  qs::qsave(conc, file="data_orig.qs");
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));
  
  return(.set.mSet(mSetObj));
}

GetMetaByCol <- function(mSetObj=NA, metaname){
  mSetObj <- .get.mSet(mSetObj);
  metaData <- mSetObj$dataSet$meta.info;
  return(metaData[,metaname]);
}

UpdateMetaData <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(!exists('meta.nm.vec')){
    sel.meta.vecs <- mSetObj$dataSet$orig.meta.info
  }else{
    sel.meta.vecs <- mSetObj$dataSet$orig.meta.info[, meta.nm.vec]
  }
  mSetObj$dataSet$meta.info <- sel.meta.vecs;
  return(.set.mSet(mSetObj))
}

RemoveSelectedMeta <- function(mSetObj=NA, meta){
  mSetObj <- .get.mSet(mSetObj);
  inx <- which(colnames(mSetObj$dataSet$meta.info) == meta);
  mSetObj$dataSet$meta.info <- mSetObj$dataSet$meta.info[, -inx];
  inx1 <- which(names(mSetObj$dataSet$meta.types) == meta);
  mSetObj$dataSet$meta.types <- mSetObj$dataSet$meta.types[-inx1];
  inx2 <- which(names(mSetObj$dataSet$meta.status) == meta);
  mSetObj$dataSet$meta.status <- mSetObj$dataSet$meta.status[-inx2];
  inx3 <- which(names(mSetObj$dataSet$types.cls.lbl) == meta);
  mSetObj$dataSet$types.cls.lbl <- mSetObj$dataSet$types.cls.lbl[-inx3];
  return(.set.mSet(mSetObj))
}

# sanity check performed after clicking on proceed button on sanity check page, for multifac module
#' SanityCheckMeta#'
#' @param mSetObj metaboanalyst object
#' @param init can be 0 or 1
#' @export
SanityCheckMeta <- function(mSetObj=NA, init = 1){

  msg <- NULL;
  mSetObj <- .get.mSet(mSetObj);

  meta.info  <- mSetObj$dataSet$meta.info
 
  cls.lbl <- meta.info[,1]
  cls.num <- length(levels(cls.lbl));
  msg <- c(msg, paste0("A total of ", length(colnames(mSetObj$dataSet$meta.info)), " metadata factors were detected: ", paste0(colnames(mSetObj$dataSet$meta.info), collapse=", "), "."));
  msg <- c(msg, paste0("The primary metadata factor is: ", colnames(mSetObj$dataSet$meta.info)[1], ", which contains ", cls.num, " groups."));

  check.inx <-apply(meta.info , 2, function(x){ ( sum(is.na(x))/length(x) + sum(x=="NA", na.rm=TRUE)/length(x) + sum(x=="", na.rm=TRUE)/length(x) ) >0});

  if(sum(check.inx)>0){
    if(init == 0){
      mSetObj$dataSet$meta.info <- meta.info[,!check.inx]
    }else{
      mSetObj$dataSet$meta.status[check.inx] = "<font color='red'>Missing values</font>";
    }
    msg <- c(msg, paste0( "<b>",paste0(colnames(meta.info)[check.inx], collapse=", "),"</b>", " meta-data factors have missing values."));
  }

  cls.vec <- vector()
  lowrep.vec <- vector()
  toolow.vec <- vector();
  for(i in 1:ncol(meta.info)){
      cls.lbl <- meta.info[,i];
      qb.inx <- tolower(cls.lbl) %in% c("qc", "blank");
      if(sum(qb.inx) > 0){
        cls.Clean <- as.factor(as.character(cls.lbl[!qb.inx])); # make sure drop level
      } else {
        cls.Clean <- cls.lbl;
      }
      meta.name <- colnames(meta.info)[i]
      min.grp.size <- min(table(cls.Clean));
      cls.num <- length(levels(cls.Clean));


    # checking if too many groups but a few samples in each group
      if(cls.num/min.grp.size > 3 && !tolower(meta.name) %in% c("subject", "time")){
        mSetObj$dataSet$small.smpl.size <- 1;
        if(init == 1){
           isNum <- grepl("^-?[0-9.]+$", cls.Clean);
           if(all(isNum)){
             mSetObj$dataSet$meta.types[i] = "cont";
             cls.vec <- c(cls.vec, meta.name)
           }else{
             if(!check.inx[i]){
             toolow.vec <- c(toolow.vec, meta.name)
             }
           }
        }
    # checking if some groups have low replicates
      } else if(min.grp.size < 3 | cls.num < 2){
        if(init == 1){
           isNum <- grepl("^-?[0-9.]+$", cls.Clean);
           if(all(isNum)){
             mSetObj$dataSet$meta.types[i] = "cont";
             cls.vec <- c(cls.vec, meta.name)
           }else{
             if(!check.inx[i] && !meta.name %in% toolow.vec){
             lowrep.vec <- c(lowrep.vec, meta.name)
             }
           }
        }
      }
    
  }
  
  if(length(cls.vec) > 0){
    if(init == 0){
        mSetObj$dataSet$meta.info <- meta.info[,which(!colnames(meta.info) %in% cls.vec)]
    }else if(init == 1){
        msg <- c(msg, paste0( "<b>",paste0(cls.vec, collapse=", "),"</b>", " meta-data factors are assigned to be continuous and remaining are categorical."));
        msg <- c(msg, "For categorical metadata, at least <b>two</b> groups with <b>three replicates</b> per group are required.");
        msg <- c(msg, "Please double check if these auto-assigned metadata types are correct.");
        #msg <- c(msg, "You can manually update the metadata using the table below.");
    }
  }


  if(length(toolow.vec)>0 && init == 1){
    msg <- c(msg, paste0( "<b>",paste0(toolow.vec, collapse=", "),"</b>", " meta-data factors have too many groups with low replicates (less than 3) per group."));
  }

  if(length(lowrep.vec)>0 && init == 1){
    msg <- c(msg, paste0( "<b>",paste0(lowrep.vec, collapse=", "),"</b>", " meta-data factors have some groups with low replicates (less than 3) per group."));
  }

  if(init == 1){
    mSetObj$msgSet$metacheck.msg <- msg;
  }
  return(.set.mSet(mSetObj));
}


#' SetDataTypeOfMeta
#' @param mSetObj metaboanalyst object
#' @export
SetDataTypeOfMeta <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  meta.info  <- mSetObj$dataSet$meta.info
  for(i in 1:ncol(meta.info)){
    metaNm <- colnames(meta.info)[i];
    if(mSetObj$dataSet$meta.types[metaNm] == "cont"){
        meta.info[,i] = as.numeric(as.character(meta.info[,i] ));
    }
  }
  mSetObj$dataSet$meta.info <- meta.info
  qs::qsave(meta.info, file = "info4batch.qs")
  return(.set.mSet(mSetObj));
}

UpdateMetaColName <- function(mSetObj=NA, oldName, newName){
  mSetObj <- .get.mSet(mSetObj);
  meta.info  <- mSetObj$dataSet$meta.info
  inx <- which(colnames(meta.info) == oldName);
  mSetObj$dataSet$meta.types <- mSetObj$dataSet$meta.types[colnames(meta.info)]
  mSetObj$dataSet$meta.status <- mSetObj$dataSet$meta.status[colnames(meta.info)]
  mSetObj$dataSet$types.cls.lbl <- mSetObj$dataSet$types.cls.lbl[colnames(meta.info)]
  colnames(meta.info)[inx] <- newName
  names(mSetObj$dataSet$meta.types)[inx] <- newName
  names(mSetObj$dataSet$meta.status)[inx] <- newName
  names(mSetObj$dataSet$types.cls.lbl)[inx] <- newName
  mSetObj$dataSet$meta.info <- meta.info
  return(.set.mSet(mSetObj));

}

UpdateMetaLevels <- function(mSetObj=NA,metaNm){
  mSetObj <- .get.mSet(mSetObj);
  if(exists("meta.lvls")){ 
      meta.info  <- mSetObj$dataSet$meta.info
      sel.meta <- meta.info[, metaNm]
      levels(sel.meta) <- meta.lvls
      meta.info <- cbind(meta.info, sel.meta);
      nms.vec <- colnames(meta.info)
      nms.vec[length(nms.vec)] <- metaNm
      nms.vec <- make.unique(nms.vec)
      colnames(meta.info) <- nms.vec
      new.nm <- colnames(meta.info)[length(nms.vec)]

     inx1 <- which(names(mSetObj$dataSet$meta.types) == metaNm);
     mSetObj$dataSet$meta.types <- c(mSetObj$dataSet$meta.types, mSetObj$dataSet$meta.types[inx1])
     names(mSetObj$dataSet$meta.types)[length(nms.vec)] <- new.nm

     
     inx2 <- which(names(mSetObj$dataSet$meta.status) == metaNm);
     mSetObj$dataSet$meta.status <- c(mSetObj$dataSet$meta.status, mSetObj$dataSet$meta.status[inx2])
     names(mSetObj$dataSet$meta.status)[length(nms.vec)] <- new.nm


     inx3 <- which(names(mSetObj$dataSet$types.cls.lbl) == metaNm);
     mSetObj$dataSet$types.cls.lbl <- c(mSetObj$dataSet$types.cls.lbl, mSetObj$dataSet$types.cls.lbl[inx3]);
     names(mSetObj$dataSet$types.cls.lbl)[length(nms.vec)] <- new.nm

      mSetObj$dataSet$meta.info <- meta.info;
  }
  return(.set.mSet(mSetObj));
}

GetMetaDataGroups <- function(){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$meta.info)
}

UpdateMetaType <-function(mSetObj=NA, metadata="NA", type="disc"){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$meta.types[metadata] = type;
  return(.set.mSet(mSetObj));
}

GetMetaTypes <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(unname(mSetObj$dataSet$meta.types));
}

SetMetaTypes <- function(mSetObj=NA, metaTypes.vec){
  mSetObj <- .get.mSet(mSetObj);
  names(metaTypes.vec) <- colnames(mSetObj$dataSet$meta.info)
  mSetObj$dataSet$meta.types <- metaTypes.vec;
  return(.set.mSet(mSetObj));
}

UpdateMetaOrder <- function(mSetObj=NA, metaName){
  mSetObj <- .get.mSet(mSetObj);
  if(exists('meta.ord.vec')){
    metadata <- mSetObj$dataSet$meta.info[,metaName];
    mSetObj$dataSet$meta.info[,metaName] <- factor(as.character(metadata), levels=meta.ord.vec)
  }
    return(.set.mSet(mSetObj));
}

GetUniqueMetaNames <-function(mSetObj=NA, metadata){
  mSetObj <- .get.mSet(mSetObj);
  data.type <- mSetObj[["dataSet"]][["meta.types"]][metadata];
  
  if(data.type == "cont"){
    return("--- NA ---");
  } else {
    return(levels(mSetObj$dataSet$meta.info[,metadata]));
  }
}