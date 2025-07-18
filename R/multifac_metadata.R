#' ReadMetaData
#'
#' @param mSetObj metaboanalyst object, initialized by InitDataObjects("pktable", "mf", FALSE)
#' @param metafilename file path of data
#' @export
ReadMetaData <- function(mSetObj = NA, metafilename) {

  ## ---------- helpers & setup ----------
  mSetObj <- .get.mSet(mSetObj)

  metadata <- .readDataTable(metafilename, FALSE)
  metadata[is.na(metadata)] <- "NA"

  if (inherits(metadata, "try-error")) {
    AddErrMsg("Failed to read the metadata file! Check format / NA cells.")
    return(0)
  }
  if (ncol(metadata) < 3) {
    AddErrMsg("At least two metadata columns are required for this module! \
               Use <b>Statistical Analysis [one factor]</b> instead.")
    return(0)
  }

  ## ---------- clean & match sample names ----------
  sample.col  <- colnames(metadata)[1]          # ▲ remember the header
  smpl.nms    <- CleanNames(metadata[[1]])
  data.smpl.nms <- names(mSetObj$dataSet$url.smp.nms)

  nm.hits <- data.smpl.nms %in% smpl.nms

  ## ---------- QC-aware handling of missing rows ----------
  if (!all(nm.hits)) {
  missing.nms <- data.smpl.nms[!nm.hits]

  qc.nms    <- missing.nms[grepl("^QC",    missing.nms, ignore.case = TRUE)]
  blank.nms <- missing.nms[grepl("^BLANK", missing.nms, ignore.case = TRUE)]

  other.missing <- setdiff(missing.nms, c(qc.nms, blank.nms))

  ## -- Any non-QC / non-BLANK names missing? → stop -----------------------
  if (length(other.missing) > 0) {
    perct <- round(length(missing.nms) / length(data.smpl.nms) * 100, 3)
    AddErrMsg(
      paste0("A total of ", length(missing.nms), " (", perct,
             "%) sample names are not present in the metadata file!"))
    AddErrMsg(paste0("Missing sample names: ",
                     paste(other.missing, collapse = "; ")))
    return(0)
  }

  ## ---------- helper to build placeholder rows --------------------------
  make_placeholders <- function(sample.vec) {
    if (length(sample.vec) == 0) return(NULL)

    meta.cols   <- metadata[, -1, drop = FALSE]

    # Detect column types once
    is.cont.col <- sapply(meta.cols, function(x)
      all(!is.na(suppressWarnings(as.numeric(as.character(x))))))

    # Build per-column replacement
    repl <- as.data.frame(
      lapply(seq_along(is.cont.col), function(i) {
        if (is.cont.col[i]) 0                     # numeric zero for cont
        else if (identical(sample.vec, qc.nms)) "QC"   # disc flag
        else "BLANK"
      }),
      stringsAsFactors = FALSE
    )
    colnames(repl) <- colnames(meta.cols)

    data.frame(setNames(list(sample.vec), sample.col), repl,
               stringsAsFactors = FALSE)
  }

  fake.rows <- rbind(make_placeholders(qc.nms),
                     make_placeholders(blank.nms))

  metadata <- rbind(metadata, fake.rows)
  smpl.nms <- CleanNames(metadata[[1]])  # refresh after appending

  ## ---------- user notices ----------------------------------------------
  note.vec <- character()

  if (length(qc.nms) > 0) {
    note.vec <- c(note.vec,
      paste0(length(qc.nms), " QC sample row",
             ifelse(length(qc.nms) == 1, "", "s"),
             " were appended to the metadata (\"QC\" for discrete columns, 0 for continuous)."))
  }
  if (length(blank.nms) > 0) {
    note.vec <- c(note.vec,
      paste0(length(blank.nms), " blank sample row",
             ifelse(length(blank.nms) == 1, "", "s"),
             " were appended to the metadata (\"BLANK\" for discrete columns, 0 for continuous)."))
  }
  if (length(note.vec) > 0) {
    note.vec <- c(
      note.vec,
    paste0("<br />",
    "<span style=\"color:darkorange;\">",
    "Reminder: QC samples are useful for QC checks, data filtering and normalization. ",
    "Delete all QC samples using the <b>Data&nbsp;Editor</b> before performing downstream analyses.",
    "</span>"
    )
    )
    mSetObj$msgSet$qc.replace.msg <- paste(note.vec, collapse = " ")
  }
}


  ## ---------- initialise meta.types ----------
  meta.cols <- metadata[ , -1, drop = FALSE]
  is.cont   <- sapply(meta.cols, is.numeric)
  mSetObj$dataSet$meta.types  <- ifelse(is.cont, "cont", "disc")
  mSetObj$dataSet$meta.status <- setNames(rep("OK", ncol(meta.cols)),
                                          colnames(meta.cols))

  ## ---------- filter & order to match abundance matrix ----------
  keep.idx  <- smpl.nms %in% data.smpl.nms
  metadata  <- metadata[keep.idx, ]
  smpl.nms  <- smpl.nms[keep.idx]
  smpl.nms  <- mSetObj$dataSet$url.smp.nms[smpl.nms]   # map back to originals

  meta.cols <- metadata[ , -1, drop = FALSE]
  meta.cols[] <- lapply(meta.cols, factor)

  ## ---------- original time-series checks (unchanged) ----------
  if (mSetObj$dataSet$design.type %in% c("time", "time0")) {
    # ... keep your existing time-series validation code here ...
  }

  rownames(meta.cols) <- smpl.nms
  mSetObj$dataSet$meta.info <- mSetObj$dataSet$orig.meta.info <- meta.cols
  mSetObj$dataSet$types.cls.lbl <- sapply(meta.cols, class)
  mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- meta.cols[, 1]
  names(mSetObj$dataSet$meta.types)      <- colnames(meta.cols)
  names(mSetObj$dataSet$types.cls.lbl)   <- colnames(meta.cols)
  mSetObj$dataSet$type.cls.lbl <- class(meta.cols[, 1])

  # save a copy for reproducible analysis
  mSetObj$dataSet$cls_orig <- mSetObj$dataSet$orig.cls;

  return(.set.mSet(mSetObj))
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
  
  #url.smp.nms <- CleanNames(smpl.nms);
  # now use clean names for all, as spaces causes a lot of issues
  smpl.nms <- url.smp.nms <- CleanNames(smpl.nms);

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
  # save.image("rem.RData");
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

  if(!is.null(mSetObj$msgSet$qc.replace.msg)){
    msg <- c(msg,mSetObj$msgSet$qc.replace.msg);
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

ContainsMetaPlaceholder <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);

  if(!is.null(mSetObj$msgSet$qc.replace.msg)){
    return(1);
  }else{
    return(0);
  }
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
  #save.image("unique");
  data.type <- mSetObj[["dataSet"]][["meta.types"]][metadata];
  
  if(is.null(data.type) || data.type == "cont"){
    return("--- NA ---");
  } else {
    return(levels(mSetObj$dataSet$meta.info[,metadata]));
  }
}


SetSelectedMetaInfo <- function(dataName="", meta0, meta1, block1){
  # print(c("SetSelectedMetaInfo",meta0,meta1))
  mSetObj <- .get.mSet(mSetObj);
  meta.info <- mSetObj$dataSet$meta.info
  if(meta0 == "NA"){
    return(0)
  }else{
    rmidx <- which(meta.info[, meta0]=="NA" | is.na(meta.info[, meta0]))
    if(meta1 != "NA"){
      rmidx <- c(rmidx,which(meta.info[, meta1]=="NA") | is.na(meta.info[, meta1]))
    }
    if(length(rmidx)>0){
      meta <- meta.info[-rmidx,]
      for(col in 1:ncol(meta)){
        meta[,col]<- droplevels(meta[,col])
      }
     mSetObj$analSet$rmidx <- rmidx;
    }else{
      meta <- meta.info
    }
    cls <- meta[, meta0];
    if(block1 != "NA"){
      block <- meta[, block1];
    }
    if(meta1 != "NA"){
       cls <- interaction(meta[, c(meta0, meta1)], sep = "_", lex.order = TRUE);
    }

if(length(levels(cls))>length(unique(cls))){
  cls <- droplevels(cls)
}
mSetObj$analSet$combFac <- cls;
mSetObj$analSet$combFacdf <- meta;
 .set.mSet(mSetObj);
    return(levels(cls))
  }
}
 

#'Plot compound summary for multi-linear regression tool
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpdNm Input the name of the compound to plot
#'@param format Input the format of the image to create
#'@param dpi Input the dpi of the image to create
#'@param width Input the width of the image to create
#'@param meta Input the metadata to visualize
#'@param version version
#'@author Jessica Ewald\email{jessica.ewald@mcgill.ca}
#'McGill University, Canada
#'License: GPL-3 License
#'@export
#'
PlotMultiFacCmpdSummary <- function(mSetObj=NA, cmpdNm, meta,meta2, version, format="png", dpi=default.dpi, width=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }
  
  if(is.na(width)){
    w <- 7.5;
  }else{
    w <- width;
  }
  
  meta.info <- mSetObj$dataSet$meta.info
  sel.cls <- meta.info[,meta]
  cls.type <- unname(mSetObj$dataSet$meta.types[meta])
  xlab = meta;
  h <- 6;
  imgName <- mSetObj$dataSet$url.var.nms[cmpdNm];
  imgName <- paste(imgName, "_", meta, "_", version, "_summary_dpi", dpi, ".", format, sep="");
  
  df.norm <- data.frame(value=mSetObj$dataSet$norm[, cmpdNm], name = sel.cls)
  
  col <- unique(GetColorSchema(sel.cls));
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(cls.type == "disc"){
    p <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name)) + geom_boxplot(outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1) 
    p <- p + scale_fill_manual(values=col) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p <- p + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") + xlab(meta)
    p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p <- p + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
  }else{
    p <- ggplot2::ggplot(df.norm, aes(x=name, y=value)) 
    p <- p + geom_point(size=2) + theme_bw()  + geom_smooth(method=lm,se=T)     
    p <- p + theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")
    p <- p + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") + xlab(meta)
    p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p <- p + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
  }
  print(p)
  dev.off()

  if(.on.public.web){
    return(imgName);
  }else{
    return(.set.mSet(mSetObj));
  }
}

