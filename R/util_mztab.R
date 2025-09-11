my.parse.mztab <- function(mSetObj=NA, filename, identifier = "name") {

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mztab.idtype <- identifier;
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
  url.smp.nms <- CleanNames(smpl.nms);
  names(url.smp.nms) <- smpl.nms;

  url.var.nms <- CleanNames(var.nms); # allow space, comma and period
  names(url.var.nms) <- var.nms;
  
  cls.lbl <- ClearStrings(as.vector(cls.lbl));
  
  # now assgin the dimension names aand save a copy
  rownames(conc) <- smpl.nms;
  colnames(conc) <- var.nms;
  qs::qsave(conc, file="data_orig.qs");
  
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
  mSetObj$dataSet$mum.type <- "table";
  mSetObj$dataSet$url.var.nms <- url.var.nms;
  mSetObj$dataSet$url.smp.nms <- url.smp.nms;
  mSetObj$msgSet$read.msg <- c(msg, paste("The uploaded data file contains ", nrow(conc),
                                          " (samples) by ", ncol(conc), " (", tolower(GetVariableLabel(mSetObj$dataSet$type)), ") data matrix.", sep=""));
  return(.set.mSet(mSetObj));
}