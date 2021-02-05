#'Read peak list files
#'@description This function reads peak list files and fills the data into a dataSet object.  
#'For NMR peak lists, the input should be formatted as two-columns containing numeric values (ppm, int).
#'Further, this function will change ppm to mz, and add a dummy 'rt'.
#'For MS peak data, the lists can be formatted as two-columns (mz, int), in which case the function will add a dummy 'rt', or
#'the lists can be formatted as three-columns (mz, rt, int).
#'@usage Read.PeakList(mSetObj=NA, foldername)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param foldername Name of the folder containing the NMR or MS peak list files to read.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

Read.PeakList<-function(mSetObj=NA, foldername="upload"){
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    dyn.load(.getDynLoadPath());
  }
  
  msg <- c("The uploaded files are peak lists and intensities data.");
  
  # the "upload" folder should contain several subfolders (groups)
  # each of the subfolder contains samples (.csv files)
  files<-dir(foldername, pattern=".[Cc][Ss][Vv]$", recursive=T, full.name=TRUE)
  if (length(files) == 0) {
    AddErrMsg("No peak list files (.csv) were found.");
    return(0);
  }
  
  snames <- gsub("\\.[^.]*$", "", basename(files));
  msg<-c(msg, paste("A total of ", length(files), "samples were found."));
  
  sclass <- gsub("^\\.$", "sample", dirname(files));
  
  scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "", fixed=TRUE);
  scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp));
  i <- 1
  while(all(scomp[i,1] == scomp[i,-1]) && i < nrow(scomp)){
    i <- i + 1;
  }
  i <- min(i, tail(c(0, which(scomp[1:i,1] == .Platform$file.sep)), n = 1) + 1)
  if (i > 1 && i <= nrow(scomp)){
    sclass <- substr(sclass, i, max(nchar(sclass)))
  }
  
  # some sanity check before proceeds
  sclass <- as.factor(sclass);
  if(length(levels(sclass))<2){
    AddErrMsg("You must provide classes labels (at least two classes)!");
    return(0);
  }
  
  # check for class labels at least three replicates per class
  if(min(table(sclass)) < 3){
    AddErrMsg("At least three replicates are required in each group!");
    return(0);
  }
  
  # check for unique sample names
  if(length(unique(snames))!=length(snames)){
    AddErrMsg("Duplcate sample names are not allowed!");
    dup.nm <- paste(snames[duplicated(snames)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  # change sample names to numbers
  samp.num<-seq(1:length(snames));
  names(samp.num)<-snames;
  
  # create a matrix "all.peaks" which is compatible with the xcmsSet@peaks matrix, so that the grouping algorithm can be used directly
  # the matrix should have "mz", "rt", "into", "sample" - 4 columns used for grouping
  # check 2 or 3 column
  
  ############## use try block to catch any error ##############
  pks<- .readDataTable(files[1]);
  if(class(pks) == "try-error") {
    AddErrMsg("The CSV file is not formatted correctly!");
    return(0);
  };
  pks <- as.matrix(pks);
  ########################################################
  
  n.col<-ncol(pks);
  if(n.col==2){
    add=TRUE;
  }else if(n.col==3){
    add=FALSE;
  }else{
    AddErrMsg("The peak list file can only contain 2 or 3 columns.");
    return(0);
  }
  
  all.peaks<-NULL;
  
  for(i in 1:length(files)){
    print(files[i]);
    pks<- as.matrix(.readDataTable(files[i]));
    if(ncol(pks)!=n.col){
      AddErrMsg("The number of columns in each file are not the same!");
      return(0);
    }
    
    if(add){ # NMR ppm+int or MS mz+int
      pks<-cbind(pks[,1], 1000, pks[,2],samp.num[i]);
    }else{
      pks<-cbind(pks,samp.num[i]);
    }
    all.peaks<-rbind(all.peaks, pks);
  }
  
  
  # make sure all values are numeric, sometimes users give other text values, need to exclude them
  all.peaks <- apply(all.peaks, 2, as.numeric);
  gd.inx <- complete.cases(all.peaks);
  all.peaks <- all.peaks[gd.inx,]
  
  if(sum(!gd.inx) > 0){
    msg<-c(msg, paste("<font color='red'>A total of", sum(!gd.inx), "peaks were excluded due to non-numeric values. </font>" ));
  }
  msg<-c(msg, paste("These samples contain a total of ", dim(all.peaks)[1], "peaks," ));
  msg<-c(msg, paste("with an average of ", round(dim(all.peaks)[1]/length(files), 1), "peaks per sample" ));
  
  colnames(all.peaks)<-c("mz","rt","int","sample");
  
  peakSet<-list(
    peaks = all.peaks,
    ncol = n.col,
    sampclass = sclass,
    sampnames = snames
  );
  
  qs::qsave(peakSet, "peakSet.qs");
  mSetObj$msgSet$read.msg <- msg;
  return(.set.mSet(mSetObj));
}

#' Read an mzTab tab separated file from the passed in file.
#' Adapted from: https://github.com/lifs-tools/rmzTab-m/blob/master/R/MzTabReader.r
#' @param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#' @param filename The name of the mzTab file to parse.
#' @param identifier The identifier to be used when the table is parsed. Use "name"
#' to use the chemical_name, "mass" to use the theoretical_neutral_mass and "sml_id"
#' to use the SML_ID. If the number of missing name and mass entries is greater than 90%,
#' then the SML_ID will be used.
#' @export
Read.mzTab <- function(mSetObj=NA, filename, identifier = "name") {
  
  mSetObj <- .get.mSet(mSetObj);
  
  msg <- NULL;
  
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=filename, sep = "\t")))
  
  mztab.table = utils::read.table(file=filename, header=FALSE,
                                  row.names=NULL, dec = ".", fill = TRUE,
                                  col.names = paste0("V", seq_len(ncol)),
                                  sep="\t", na.strings="null", quote = "",
                                  stringsAsFactors = FALSE)
  
  # first sanity check
  # check if contains MTD, SMH and SML
  if(sum(sapply(c("MTD", "SMH", "SML"), grepl, unique(mztab.table$V1))) == 3){
    msg <- ("mzTab format ok!")
  }else{
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  } 
  
  # first set up metadata
  metadata <- mztab.table[startsWith(as.character(mztab.table$V1), "MTD"),]
  variables <- metadata[grepl("study_variable", metadata$V2),]
  
  if(length(variables) < 1){
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  }
  
  variables.groups <- unique(gsub("-.*", "", variables$V2))
  group.names <- metadata$V3[match(variables.groups, metadata$V2)] 
  
  variables.list <- vector("list", length=length(variables.groups)) 
  names(variables.list) <- variables.groups
  
  for(i in 1:length(variables.groups)){
    group2match <- gsub("\\[", "\\\\[", variables.groups[i])
    group2match <- gsub("\\]", "\\\\]", group2match)
    all.info <- variables[grepl(group2match, variables$V2),] 
    assay.refs <- all.info$V3[grepl("assay_refs", all.info$V2)]
    variables.list[i] <- assay.refs
  }
  
  # second set up small molecule summary
  smh <- mztab.table[startsWith(as.character(mztab.table$V1), "SMH"),,drop=FALSE]
  sml <- mztab.table[startsWith(as.character(mztab.table$V1), "SML"),,drop=FALSE]
  
  if(nrow(sml) < 1){
    AddErrMsg("Invalid mzTab format! Make sure mzTab file has been validated!")
    return(0)
  }
  
  sml.data.frame <- data.frame(sml, stringsAsFactors = FALSE)
  colnames(sml.data.frame) <- as.character(smh[1,])
  
  # sanity check to see if selected identifier is valid
  # if more than 90% of names are null, switch to use m/z
  # if more than 90% of m/z are null, switch to use sml_id
  if(sum(is.null(sml.data.frame$chemical_name))/length(sml.data.frame$chemical_name) > 0.1) {
    msg <- c(msg, "Too many missing chemical names, will use theoretical neutral mass instead!")
    identifier <- "mass"
  }else if(sum(is.null(sml.data.frame$theoretical_neutral_mass))/length(sml.data.frame$theoretical_neutral_mass) > 0.1){
    msg <- c(msg, "Too many missing m/z, will use mzTab SML_ID instead!")
    identifier <- "sml_id"
  }else if(sum(is.na(sml.data.frame$theoretical_neutral_mass))/length(sml.data.frame$theoretical_neutral_mass) > 0.1){ # sometime it's NA
    msg <- c(msg, "Too many missing m/z, will use mzTab SML_ID instead!")
    identifier <- "sml_id"
  }
  
  # deal with duplicates in selected ids
  if(identifier == "name"){
    id.og <- id <- sml.data.frame$chemical_name
    dup.id <- paste(sml.data.frame$chemical_name, sml.data.frame$adduct_ions, sep="_")
    id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] <- dup.id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] 
  }else if(identifier == "mass"){
    id.og <- id <- round(as.numeric(sml.data.frame$theoretical_neutral_mass), 5)
    dup.id <- paste( round(as.numeric(sml.data.frame$theoretical_neutral_mass), 5), sml.data.frame$adduct_ions, sep="_")
    id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] <- dup.id[which(duplicated(id, fromLast = TRUE) | duplicated(id))] 
  }else{
    id <- sml.data.frame$SML_ID;
  }
  
  # sanity check to see if selected id is valid
  # if ids are still duplicates, switch to append sml_id
  if(sum(duplicated(id)) > 1){
    id <- paste(id.og, sml.data.frame$SML_ID, sep="_")
  }
  
  assay_data <- trimws(unlist( lapply(variables.list, function(x) strsplit(x, "\\|")) ))
  assay_data <- paste("abundance_", assay_data, sep="")
  assay_df <- sml.data.frame[,match(assay_data, colnames(sml.data.frame))]
  
  assay_table <- cbind.data.frame(Sample=id, assay_df, stringsAsFactors = FALSE)
  
  # replace colnames with actual variable groups
  samples <- colnames(assay_table)[-1]
  samples_base <- gsub("abundance_", "", samples)
  variables.list <- lapply(variables.list, function(x) trimws(unlist(strsplit(x, "\\|"))))
  
  samples2groups <- vector(length = length(samples_base))
  
  for(i in 1:length(samples_base)){
    samples2groups[i] <- group.names[sapply(variables.list, function(x) samples_base[i] %in% x)]
  } 
  
  # remove blank from dataframe
  blank.inx <- grepl("blank", samples2groups, ignore.case = TRUE)
  
  if(sum(blank.inx) > 0){
    samples2groups <- samples2groups[!blank.inx]
    assay_table <- cbind.data.frame(Sample=id, assay_df[,!blank.inx], stringsAsFactors = FALSE) 
  }
  
  assay_table_all <- rbind.data.frame(c("Group", samples2groups), assay_table)
  colnames(assay_table_all) <- gsub("\\[|\\]", "", colnames(assay_table_all))
  
  fast.write.csv(assay_table_all, "mzTab_parsed.csv", row.names = F)
  
  mSetObj$dataSet$cls.type <- "disc";
  mSetObj$dataSet$format <- "colu";
  
  dat <- assay_table_all 
  msg <- c(msg, "Samples are in columns and features in rows.");
  var.nms <- dat[-1,1];
  dat[,1] <- NULL;
  smpl.nms <- colnames(dat);
  cls.lbl <- unname(unlist(dat[1,]));
  conc <- t(dat[-1,]);
  mSetObj$dataSet$type.cls.lbl <- class(cls.lbl);
  
  # free memory
  dat <- NULL;
  
  msg <- c(msg, "The uploaded file is in the mzTab format.");
  
  # try to remove empty line if present
  # identified if no sample names provided
  
  empty.inx <- is.na(smpl.nms) | smpl.nms == ""
  if(sum(empty.inx) > 0){
    msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty rows</font> were detected and excluded from your data."));
    smpl.nms <- smpl.nms[!empty.inx];
    cls.lbl <-  cls.lbl[!empty.inx];
    conc <- conc[!empty.inx, ];
  }
  
  # try to check & remove empty lines if class label is empty
  # Added by B. Han
  empty.inx <- is.na(cls.lbl) | cls.lbl == ""
  if(sum(empty.inx) > 0){
    if(mSetObj$analSet$type != "roc"){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
      smpl.nms <- smpl.nms[!empty.inx];
      cls.lbl <-  cls.lbl[!empty.inx];
      conc <- conc[!empty.inx, ];
    }else{
      # force all NA to empty string, otherwise NA will become "NA" class label
      cls.lbl[is.na(cls.lbl)] <- "";
      msg <- c(msg, paste("<font color=\"orange\">", sum(empty.inx), "new samples</font> were detected from your data."));
    }
  }
  
  # check for uniqueness of dimension name
  if(length(unique(smpl.nms))!=length(smpl.nms)){
    dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  # try to remove check & remove empty line if feature name is empty
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
  
  # now check for special characters in the data labels
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
  
  # only keep alphabets, numbers, ",", "." "_", "-" "/"
  orig.smp.nms <- smpl.nms;
  smpl.nms <- CleanNames(smpl.nms, "sample_name");
  names(orig.smp.nms) <- smpl.nms;
  
  # keep a copy of original names for saving tables 
  orig.var.nms <- var.nms;
  var.nms <- CleanNames(var.nms, "var_name"); # allow space, comma and period
  names(orig.var.nms) <- var.nms;
  
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  
  # check for class labels at least two replicates per class
  if(min(table(cls.lbl)) < 3){
    AddErrMsg(paste ("A total of", length(levels(as.factor(cls.lbl))), "groups found with", length(smpl.nms), "samples."));
    AddErrMsg("At least three replicates are required in each group!");
    AddErrMsg("Or maybe you forgot to specify the data format?");
    return(0);
  }
  
  if(length(unique(cls.lbl)) == 1){
    AddErrMsg("At least two groups are required for statistical analysis!");
    return(0);
  }
  
  mSetObj$dataSet$orig.cls <- mSetObj$dataSet$cls <- as.factor(as.character(cls.lbl));
  mSetObj$dataSet$mumType <- "table";
  mSetObj$dataSet$orig.var.nms <- orig.var.nms;
  mSetObj$dataSet$orig.smp.nms <- orig.smp.nms;
  #mSetObj$dataSet$orig <- conc; # copy to be processed in the downstream
  qs::qsave(conc, file="data_orig.qs");
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));
  return(.set.mSet(mSetObj));
}


#'Group peak list
#'@description Group peaks from the peak list based on position
#'using the XCMS grouping algorithm (align peaks wrt, rt, and mz).
#'For NMR peaks, need to change ppm -> mz and add dummy rt.
#'If the data is 2-column MS, first need to add dummy rt.
#'If the data is 3-column MS, the data can be used directly.
#'The default mzwid for MS is 0.25 m/z, and for NMR is 0.03 ppm.
#'The default bw is 30 for LCMS, and 5 for GCMS.
#'@usage GroupPeakList(mSetObj=NA, mzwid, bw, minfrac, minsamp, max)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param mzwid, define the width of overlapping m/z slices to use for creating peak density chromatograms
#'and grouping peaks across samples
#'@param bw, define the bandwidth (standard deviation or half width at half maximum) of gaussian smoothing
#'kernel to apply to the peak density chromatogram
#'@param minfrac, define the minimum fraction of samples necessary in at least one of the sample groups for
#'it to be a valid group
#'@param minsamp, define the minimum number of samples necessary in at least one of the sample groups for 
#'it to be a valid group 
#'@param max, define the maximum number of groups to identify in a single m/z slice
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
GroupPeakList <- function(mSetObj=NA, mzwid = 0.25, bw = 30, minfrac = 0.5, minsamp = 1, max = 50) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- qs::qread("peakSet.qs");
  samples <- peakSet$sampnames;
  classlabel <- peakSet$sampclass;
  classnames <- levels(classlabel)
  
  classlabel <- as.vector(unclass(classlabel))
  classnum <- integer(max(classlabel))
  for (i in seq(along = classnum)){
    classnum[i] <- sum(classlabel == i)
  }
  
  peakmat <- peakSet$peaks;
  porder <- order(peakmat[,"mz"]);
  peakmat <- peakmat[porder,,drop=F]
  rownames(peakmat) <- NULL
  retrange <- range(peakmat[,"rt"])
  
  minpeakmat <- min(classnum)/2
  
  mass <- seq(peakmat[1,"mz"], peakmat[nrow(peakmat),"mz"] + mzwid, by = mzwid/2)
  masspos <- findEqualGreaterM(peakmat[,"mz"], mass)
  
  groupmat <- matrix(nrow = 512, ncol = 7 + length(classnum))
  groupindex <- vector("list", 512)
  
  endidx <- 0
  num <- 0
  gcount <- integer(length(classnum))
  for (i in seq(length = length(mass)-2)) {
    startidx <- masspos[i]
    endidx <- masspos[i+2]-1
    if (endidx - startidx + 1 < minpeakmat)
      next
    speakmat <- peakmat[startidx:endidx,,drop=FALSE]
    den <- density(speakmat[,"rt"], bw, from = retrange[1]-3*bw, to = retrange[2]+3*bw)
    maxden <- max(den$y)
    deny <- den$y
    gmat <- matrix(nrow = 5, ncol = 2+length(classnum))
    snum <- 0
    while (deny[maxy <- which.max(deny)] > maxden/20 && snum < max) {
      grange <- descendMin(deny, maxy)
      deny[grange[1]:grange[2]] <- 0
      gidx <- which(speakmat[,"rt"] >= den$x[grange[1]] & speakmat[,"rt"] <= den$x[grange[2]])
      gnum <- classlabel[unique(speakmat[gidx,"sample"])]
      for (j in seq(along = gcount))
        gcount[j] <- sum(gnum == j)
      if (! any(gcount >= classnum*minfrac & gcount >= minsamp))
        next
      snum <- snum + 1
      num <- num + 1
      ### Double the size of the output containers if they're full
      if (num > nrow(groupmat)) {
        groupmat <- rbind(groupmat, matrix(nrow = nrow(groupmat), ncol = ncol(groupmat)))
        groupindex <- c(groupindex, vector("list", length(groupindex)))
      }
      groupmat[num, 1] <- median(speakmat[gidx, "mz"])
      groupmat[num, 2:3] <- range(speakmat[gidx, "mz"])
      groupmat[num, 4] <- median(speakmat[gidx, "rt"])
      groupmat[num, 5:6] <- range(speakmat[gidx, "rt"])
      groupmat[num, 7] <- length(gidx)
      groupmat[num, 7+seq(along = gcount)] <- gcount
      groupindex[[num]] <- sort(porder[(startidx:endidx)[gidx]])
    }
  }
  colnames(groupmat) <- c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
                          "npeaks", classnames)
  
  groupmat <- groupmat[seq(length = num),]
  groupindex <- groupindex[seq(length = num)]
  
  # Remove groups that overlap with more "well-behaved" groups
  numsamp <- rowSums(groupmat[,(match("npeaks", colnames(groupmat))+1):ncol(groupmat),drop=FALSE])
  uorder <- order(-numsamp, groupmat[,"npeaks"])
  uindex <- rectUnique(groupmat[,c("mzmin","mzmax","rtmin","rtmax"),drop=FALSE],uorder)
  
  peakSet$groups <- groupmat[uindex,];
  peakSet$groupidx<- groupindex[uindex];
  qs::qsave(peakSet, "peakSet.qs");
  return(.set.mSet(mSetObj));
}

#'Set peak list group values
#'@param mSetObj Input name of mSetObj, the data used is the nmr.xcmsSet object
#'@import qs
#'@export
#'
SetPeakList.GroupValues <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj);
  peakSet <- qs::qread("peakSet.qs");
  msg <- mSetObj$msgSet$peakMsg;
  
  peakmat <- peakSet$peaks;
  groupmat <- peakSet$groups;
  groupindex <- peakSet$groupidx;
  
  sampnum <- seq(length = length(peakSet$sampnames))
  intcol <- match("int", colnames(peakmat))
  sampcol <- match("sample", colnames(peakmat))
  
  # row is peak, col is sample
  values <- matrix(nrow = length(groupindex), ncol = length(sampnum))
  
  for (i in seq(along = groupindex)) {
    # for each group, replace multiple peaks from the same sample by their sum
    for(m in sampnum){
      samp.inx<-which(peakmat[groupindex[[i]], sampcol]==m)
      if(length(samp.inx)>0){
        values[i, m] <- sum(peakmat[groupindex[[i]][samp.inx], intcol]);
      }else{
        values[i, m] <- NA;
      }
    }
  }
  
  msg<-c(msg, paste("A total of", length(groupindex), "peak groups were formed. "));
  msg<-c(msg, paste("Peaks of the same group were summed if they are from one sample. "));
  msg<-c(msg, paste("Peaks appearing in less than half of all samples in each group were ignored."));
  
  colnames(values) <- peakSet$sampnames;
  
  if(peakSet$ncol==2){
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5));
  }else{
    rownames(values) <- paste(round(groupmat[,paste("mz", "med", sep="")],5), "/", round(groupmat[,paste("rt", "med", sep="")],2), sep="");
    mSetObj$dataSet$three.col <- TRUE;
  }
  
  #mSetObj$dataSet$orig <- t(values);
  qs::qsave(t(values), file="data_orig.qs");
  mSetObj$msgSet$proc.msg <- msg
  mSetObj$dataSet$orig.cls <- as.factor(peakSet$sampclass);
  mSetObj$dataSet$type.cls.lbl <- class(peakSet$sampclass);
  return(.set.mSet(mSetObj));
}

########## Utility Functions ##############
#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param y Input peaks
#'@param istart Performs which.max on y
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
descendMin <- function(y, istart = which.max(y)) {
  
  if (!is.double(y)) y <- as.double(y)
  unlist(.C("DescendMin",
            y,
            length(y),
            as.integer(istart-1),
            ilower = integer(1),
            iupper = integer(1),
            DUP = FALSE)[4:5]) + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param x Input the data
#'@param values Input the values 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
findEqualGreaterM <- function(x, values) {
  
  if (!is.double(x)) x <- as.double(x)
  if (!is.double(values)) values <- as.double(values)
  .C("FindEqualGreaterM",
     x,
     length(x),
     values,
     length(values),
     index = integer(length(values)),
     DUP = FALSE)$index + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@param m Peaks
#'@param order Performs seq(length = nrow(m))
#'@param xdiff Default set to 0
#'@param ydiff Default set to 0
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
rectUnique <- function(m, order = seq(length = nrow(m)), xdiff = 0, ydiff = 0) {
  
  nr <- nrow(m)
  nc <- ncol(m)
  if (!is.double(m))
    m <- as.double(m)
  .C("RectUnique",
     m,
     as.integer(order-1),
     nr,
     nc,
     as.double(xdiff),
     as.double(ydiff),
     logical(nrow(m)),
     DUP = FALSE)[[7]]
}