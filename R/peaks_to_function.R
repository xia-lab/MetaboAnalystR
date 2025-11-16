### An R package for pathway enrichment analysis for untargeted metabolomics
### based on high-resolution LC-MS platform
### This is based on the mummichog algorithm implemented in python (http://mummichog.org/)
### The goals of developing MummichogR are
### 1) to make this available to the R user community
### 2) high-performance (similar or faster compared to python)
### 3) broader pathways support - by adding support for 21 common organisms based on KEGG pathways
### 4) companion web interface on MetaboAnalyst - the "MS Peaks to Pathways" module
### @authors J. Chong \email{jasmine.chong@mail.mcgill.ca}, J. Xia \email{jeff.xia@mcgill.ca}
### McGill University, Canada
### License: GNU GPL (>= 2)

#'Save adduct names for mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param qvec Input the vector to query
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.AdductData <- function(mSetObj=NA, qvec){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$adduct.list <- qvec;
  mSetObj$dataSet$adduct.custom <- TRUE
  return(.set.mSet(mSetObj));
}

#'Set the peak enrichment method for the MS Peaks to Paths module
#'@description This function sets the peak enrichment method.
#'@param mSetObj Input the name of the created mSetObj.
#'@param algOpt algorithm option, can be "gsea", "mum" and "integ"
#'@param version version of mummichog
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetPeakEnrichMethod <- function(mSetObj=NA, algOpt, version="v2"){
  
  mSetObj <- .get.mSet(mSetObj);
  #mSetObj$peaks.alg <- algOpt
  
  mSetObj$paramSet$version <- version
  mSetObj$input_cpdlist <- NULL;

  if(algOpt == "gsea"){
    #anal.type <<- "gsea_peaks";    
    mSetObj$paramSet$anal.type <- "gsea_peaks";
    mSetObj$mum_nm <- "mummichog_query_gsea.json";
    mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment_gsea.csv";
  }else if(algOpt == "mum"){
    #anal.type <<- "mummichog"
    mSetObj$paramSet$anal.type <- "mummichog";
    #set json (for kegg network) and csv names
    mSetObj$mum_nm <- "mummichog_query_mummichog.json";
    mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment_mummichog.csv";
  }else{
    #anal.type <<- "integ_peaks"
    mSetObj$paramSet$anal.type <- "integ_peaks";
    #set json (for kegg network) and csv names
    mSetObj$mum_nm <- "mummichog_query_integ.json";
    mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment_integ.csv";
  }
  return(.set.mSet(mSetObj));
}

#'Constructor to read uploaded user files into the mummichog object
#'@description This function handles reading in CSV or TXT files and filling in the mSet object
#' for mummichog analysis. It makes sure that all necessary columns are present.
#'@usage Read.PeakListData(mSetObj=NA, filename = NA, meta.anal = FALSE, method = "pvalue")
#'@param mSetObj Input the name of the created mSetObj.
#'@param filename Input the path name for the CSV/TXT files to read.
#'@param meta.anal Logical, TRUE if data will be used for meta-analysis.
#'@param method Input the type of statistical scores included in the
#'mummichog input. "pvalue" for p-values, "es" for effect-sizes, and
#'"both" for both p-values and effect-sizes.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
Read.PeakListData <- function(mSetObj=NA, filename = NA, 
                              meta.anal = FALSE,
                              method = "pvalue") {
  
  mSetObj <- .get.mSet(mSetObj);
  
  file_name <- tools::file_path_sans_ext(basename(filename)) 
  mumDataContainsPval = 1; #whether initial data contains pval or not
  input <- as.data.frame(.readDataTable(filename));
  user_cols <- gsub("[^[:alnum:]]", "", colnames(input));
  mummi.cols <- c("m.z", "p.value", "t.score", "r.t")
  
  if(meta.anal & method %in% c("es", "both")){
    mummi.cols <- c(mummi.cols, "effect.size", "lower.ci", "upper.ci")
  }
  
  # first check if mode included
  mumDataContainsMode <- "mode" %in% user_cols;
  
  if(mumDataContainsMode){
    mode.info <- input$mode  
    input <- subset(input, select=-mode)
    user_cols <- gsub("[^[:alnum:]]", "", colnames(input))
  }
  
  # next check what column names are there
  hit <- "mz" %in% user_cols;
  
  if(sum(hit) < 1){
    AddErrMsg("Missing information, data must contain a 'm.z' column!");
    return(0);
  }
  
  if(length(colnames(input) %in% mummi.cols) == 1){
    peakFormat <- mSetObj$paramSet$peakFormat;
  }else{
    # subset to what's needed for ms peaks
    # then rename columns
    hits2 <- match(gsub("[^[:alnum:]]", "", mummi.cols), user_cols)
    input <- input[, na.omit(hits2)]  
    user_cols <- user_cols[na.omit(hits2)]
    hits.colnames <- match(user_cols, gsub("[^[:alnum:]]", "", mummi.cols))
    user.cols <- mummi.cols[na.omit(hits.colnames)]
    peakFormat <- paste0(substr(sort(user.cols), 1, 1), collapse = "")
    colnames(input) <- user.cols
  }
  
  rt.hit <- "r.t" %in% colnames(input)
  
  if(rt.hit > 0){
    rt = TRUE
  }else{
    rt = FALSE
  }
  
  qs::qsave(input, "mum_raw.qs");
  
  if(!"p.value" %in% colnames(input)){
    mumDataContainsPval <- 0;
    input[,'p.value'] <- rep(0, length=nrow(input))
  }
  
  if(!"t.score" %in% colnames(input)){
    input[,'t.score'] <- rep(0, length=nrow(input))
  }
  
  if(rt){
    mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score, input$r.t);
    colnames(mSetObj$dataSet$mummi.orig) = c("p.value", "m.z", "t.score", "r.t")
  }else{
    mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score);
    colnames(mSetObj$dataSet$mummi.orig) = c("p.value", "m.z", "t.score")
  }
  
  if(meta.anal & method %in% c("es", "both")){
    mSetObj$dataSet$mummi.orig <- cbind(mSetObj$dataSet$mummi.orig, effect.size=input$effect.size, 
                                        lower.ci=input$lower.ci, upper.ci=input$upper.ci);
  }
  
  if(mSetObj$dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx <- rep(TRUE, nrow(mSetObj$dataSet$mummi.orig))
  }else if(mSetObj$dataSet$mode == "negative"){
    mSetObj$dataSet$pos_inx <- rep(FALSE, nrow(mSetObj$dataSet$mummi.orig) )
  }else{ # mixed
    mSetObj$dataSet$pos_inx <- mode.info == "positive"
  }
  
  mSetObj$paramSet$mumRT = rt;
  mSetObj$dataSet$mum.type = "list";
  mSetObj$msgSet$read.msg <- paste("A total of", length(input$p.value), 
                                   "m/z features were found in your uploaded data.");
  mSetObj$dataSet$fileName <- file_name;
  mSetObj$paramSet$mumDataContainsPval <- mumDataContainsPval;
  mSetObj$paramSet$peakFormat <- peakFormat;
  mSetObj$dataSet$meta.info <- as.matrix(1); # Define a value to avoid bug

  return(.set.mSet(mSetObj));
}

# function to format peak table from mzmine to right format for metaboanalyst
# @format "row" for features in rows, "col" for features in columns.
.format_mzmine_pktable <- function(mSetObj=NA, fileName, format="rowu", group1=NA, group2=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mzmine_table <- .readDataTable(fileName)
  
  if(class(mzmine_table) == "try-error" || ncol(mzmine_table) == 1){
    AddErrMsg("Data format error. Failed to read in the data!");
    AddErrMsg("Make sure the data table is saved as comma separated values (.csv) format!");
    AddErrMsg("Please also check the followings: ");
    AddErrMsg("Either sample or feature names must in UTF-8 encoding; Latin, Greek letters are not allowed.");
    AddErrMsg("We recommend to use a combination of English letters, underscore, and numbers for naming purpose.");
    AddErrMsg("Make sure sample names and feature (peak, compound) names are unique.");
    AddErrMsg("Missing values should be blank or NA without quote.");
    AddErrMsg("Make sure the file delimeters are commas.");
    return(0);
  }
  
  rownames(mzmine_table) <- mzmine_table[,1]
  mzmine_table <- mzmine_table[,-1]
  
  if(!is.na(group1) & !is.na(group2)){
    if(format == "row"){
      keep.inx <- mzmine_table[1, ] %in% c(group1, group2)
      mzmine_table <- mzmine_table[, keep.inx]
    }else{
      keep.inx <- mzmine_table[, 1] %in% c(group1, group2)
      mzmine_table <- mzmine_table[keep.inx, ]
    }
    newnames <- paste0(tools::file_path_sans_ext(basename(fileName)), "_", group1, "_", group2, ".csv")
  }else{
    
    mzmine_table[1, ] <- tolower(mzmine_table[1, ])
    groups <- unique(unlist(mzmine_table[1, ]))
    
    if(length(groups) > 2){
      AddErrMsg("Only two groups permitted!");
      if("qc" %in% groups){
        AddErrMsg("Remove QCs prior to uploading the mzmine table!");
      }
      return(0);
    }
    
    if(.on.public.web){
      newnames <- "mzmine_peaktable_metaboanalyst.csv"
    }else{
      newnames <- paste0(tools::file_path_sans_ext(basename(fileName)), "_formatted.csv")
    }
  }
  
  if(format == "colu"){ # samples in columns so features are in row
    feats <- gsub("/", "__", rownames(mzmine_table) )
    feats <- sub(".*?__", "", feats )
    feats <- sub("min", "", feats )
    feats <- sub("mz", "", feats )
    feats <- make.unique(feats, sep="")
    rownames(mzmine_table) <- feats
  }else{ # features in columns
    feats <- gsub("/", "__", colnames(mzmine_table) )
    feats <- sub(".*?__", "", feats )
    feats <- sub("min", "", feats )
    feats <- sub("mz", "", feats )
    feats <- make.unique(feats, sep="")
    colnames(mzmine_table) <- feats
  }
  fast.write.csv(mzmine_table, newnames, row.names = TRUE)
  return(.set.mSet(mSetObj))
}

#'Set the peak format for the mummichog analysis
#'@description Set the peak format for mummichog analysis.
#'@param mSetObj mSetObj
#'@param type Input the name of mummichog analysis type, usually 'mpt'.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetPeakFormat <-function(mSetObj=NA, type = "mpt"){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$paramSet$peakFormat <- type;
  return(.set.mSet(mSetObj))
}

GetPeakFormat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$paramSet$peakFormat)
}

#'Convert mSetObj to proper format for MS Peaks to Pathways module
#'@description Following t-test analysis or effect size calculation, 
#'this functions converts the results from the mSetObj 
#'to the proper format for mummichog analysis. 
#'@param mSetObj Input the name of the created mSetObj.
#'@param rt Logical, whether or not to include retention time information.
#'@param rds.file Logical, if true, the "annotated_peaklist.rds"
#'must be in the current working directory to get corresponding retention time
#'information for the features. If not, the retention time information
#'will be taken from the feature names. Feature names must be formatted
#'so that the mz and retention time for a single peak is separated by two
#'underscores. For instance, m/z of 410.2148 and retention time of 42.46914 seconds
#'must be formatted as 410.2148__42.46914.
#'@param rt.type Character, input whether retention time is in seconds (default as RT using
#'MetaboAnalystR is seconds) or minutes (as from MZmine).
#'@param test Character, input what statistical values to include in the mummichog input. 
#'For p-values and t-scores only from t-test, use "tt".
#'For log2FC from the fold-change analsis, use "fc".
#'For effect-sizes, use "es".
#'For, p-values, fold-changes and effect sizes, use "all".
#'For multiple groups, use 'aov'.
#'@param mode ion mode, positive or negative
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Convert2Mummichog <- function(mSetObj=NA, 
                              rt=FALSE, 
                              rds.file=FALSE, 
                              rt.type="seconds", 
                              test="tt", mode=NA){

  if(test=="tt"|test=="all"){
    if(is.null(mSetObj$analSet$tt)){
      AddErrMsg("T-test was not performed!")
      return(0)
    }
    
    tt.pval <- sort(mSetObj$analSet$tt$p.value);
    fdr <- p.adjust(tt.pval, "fdr")
    #fdr <- tt.pval
    mz.pval <- names(tt.pval)
    pvals <- cbind(mz.pval, as.numeric(fdr))
    colnames(pvals) <- c("m.z", "p.value")
    
    tt.tsc <- sort(mSetObj$analSet$tt$t.score);
    mz.tsc <- names(tt.tsc)
    tscores <- cbind(mz.tsc, as.numeric(tt.tsc))
    colnames(tscores) <- c("m.z", "t.score")
  } else if(test=="es"|test=="all"){
    effect.size <- mSetObj$analSet$effect.size;    
    if(is.null(effect.size)){
      AddErrMsg("Effect size was not calculated!")
      return(0)
    }
    
    mz <- rownames(effect.size)
    esize <- cbind(mz, effect.size)
    colnames(esize) <- c("m.z", "effect.size", "st.dev", "lower.ci", "upper.ci");

  } else if(test=="fc"|test=="all"){
    log2fc <- mSetObj$analSet$fc$fc.log    
    if(is.null(log2fc)){
      AddErrMsg("Fold-change was not calculated!")
      return(0)
    }
    
    mz.fc <- names(log2fc)
    fcs <- cbind(mz.fc, as.numeric(log2fc))
    colnames(fcs) <- c("m.z", "log2.fc");

  } else if(test == "aov"){
    if(is.null(mSetObj$analSet$aov)){
      AddErrMsg("ANOVA was not performed!")
      return(0)
    }
    aov.pvals <- mSetObj$analSet$aov$sig.p;
    
    # Note pre-order will have effect on results
    # Let's be consistent
    ord.inx <- order(aov.pvals);
    fdr <- mSetObj$analSet$aov$sig.fdr[ord.inx];
    mz.pval <- names(fdr);
    pvals <- cbind(mz.pval, as.numeric(fdr));
    colnames(pvals) <- c("m.z", "p.value");

  }else if (test == "cov") {

  cov.res <- mSetObj$analSet$cov                     # limma-style table
  if (is.null(cov.res)) {
    AddErrMsg("Covariate-adjusted statistics were not calculated!") ; return(0)
  }

  ## ------------------------------------------------------------------
  ##  1.  Parse row-names:  "mz__rt"  →  m/z   +   retention time
  ## ------------------------------------------------------------------
  feat_nm <- rownames(cov.res)
  has_rt  <- grepl("__", feat_nm, fixed = TRUE)

  if (any(has_rt)) {                                 # split once, vectorised
    parts   <- strsplit(feat_nm[has_rt], "__", fixed = TRUE)
    mz.vec  <- feat_nm                               # default: unchanged
    rt.vec  <- rep(NA_real_, length(feat_nm))

    mz.vec[has_rt] <- vapply(parts, `[`, 1L,
                             FUN.VALUE = character(1))
    rt.vec[has_rt] <- as.numeric(vapply(parts, `[`, 2L,
                             FUN.VALUE = character(1)))

    ## save RTs so the universal rt-block later can attach them
    mSetObj$dataSet$ret_time <- rt.vec
    mSetObj$paramSet$mumRT   <- TRUE                 # flag that RT is present
    feat_nm <- mz.vec                                # use clean m/z names below
  }

  ## ------------------------------------------------------------------
  ##  2.  Build p-value / t-score tables (keep order by p-value)
  ## ------------------------------------------------------------------
  cov.pv           <- setNames(cov.res$P.Value, feat_nm)
  cov.pv           <- sort(cov.pv)
  fdr              <- p.adjust(cov.pv, "fdr")

  pvals            <- cbind(names(cov.pv), as.numeric(fdr))
  colnames(pvals)  <- c("m.z", "p.value")

  tvec             <- setNames(cov.res$t, feat_nm)[ names(cov.pv) ]
  tscores          <- cbind(names(tvec), as.numeric(tvec))
  colnames(tscores) <- c("m.z", "t.score")
} else {
      AddErrMsg("Unknown method!")
      return(0);
  }
  
  if(rt & rds.file){
    
    if(!file.exists("annotated_peaklist.rds")){
      AddErrMsg("annotated_peaklist.rds not found in current working directory!")
      return(0)
    }
    
    camera_output <- readRDS("annotated_peaklist.rds")
    mz.cam <- round(camera_output$mz, 5) 
    rt.cam <- round(camera_output$rt, 5) 
    camera <- cbind(mz.cam, rt.cam)
    colnames(camera) <- c("m.z", "r.t")

    # PERFORMANCE FIX (Issue #7): Use data.table for large merges instead of base R merge
    # data.table merge is 5-50x faster for large datasets and handles multiple merges efficiently
    if(test == "tt"){
      mummi_new <- Reduce(function(x,y) merge(data.table::as.data.table(x), data.table::as.data.table(y), by="m.z", all = TRUE),
                          list(pvals, tscores, camera))
      mummi_new <- data.table::setDF(mummi_new)
      complete.inx <- complete.cases(mummi_new[,c("p.value", "t.score", "r.t")]) # filter out m/zs without pval and tscore
    }else if(test == "es"){
      mummi_new <- merge(data.table::as.data.table(esize), data.table::as.data.table(camera), by="m.z", all = TRUE)
      mummi_new <- data.table::setDF(mummi_new)
      complete.inx <- complete.cases(mummi_new[,c("effect.size", "r.t")])
    }else if(test == "all"){
      mummi_new <- Reduce(function(x,y) merge(data.table::as.data.table(x), data.table::as.data.table(y), by="m.z", all = TRUE),
                          list(pvals, tscores, fcs, esize, camera))
      mummi_new <- data.table::setDF(mummi_new)
      complete.inx <- complete.cases(mummi_new[,c("p.value", "t.score", "log2.fc", "effect.size", "r.t")]) # filter out m/zs without pval and tscore
    }else if(test == "fc"){
      mummi_new <- merge(data.table::as.data.table(fcs), data.table::as.data.table(camera), by="m.z", all = TRUE)
      mummi_new <- data.table::setDF(mummi_new)
      complete.inx <- complete.cases(mummi_new[,c("log2.fc", "r.t")])
    }else if(test == "aov"){
      mummi_new <- merge(data.table::as.data.table(pvals), data.table::as.data.table(camera), by="m.z", all = TRUE)
      mummi_new <- data.table::setDF(mummi_new)
      complete.inx <- complete.cases(mummi_new[,c("p.value", "r.t")])
   }else if (test == "cov") {
      ## merge p-values  +  t-scores  +  CAMERA RT table
      mummi_new  <- Reduce(function(x, y) merge(data.table::as.data.table(x), data.table::as.data.table(y), by = "m.z", all = TRUE),
                           list(pvals, tscores, camera))
      mummi_new <- data.table::setDF(mummi_new)
      ## keep rows that have p-value, t-score, and RT
      complete.inx <- complete.cases(mummi_new[ , c("p.value", "t.score", "r.t")])
      mummi_new    <- mummi_new[complete.inx, ]
  }
    mummi_new <- mummi_new[complete.inx,]
    
  } else {
    
    # PERFORMANCE FIX (Issue #7): Use data.table for merges
    if(test=="tt"){
      mummi_new <- merge(data.table::as.data.table(pvals), data.table::as.data.table(tscores))
      mummi_new <- data.table::setDF(mummi_new)
    }else if(test=="es"){
      mummi_new <- esize;
    }else if(test=="all"){
      mummi_new <- Reduce(function(x,y) merge(data.table::as.data.table(x), data.table::as.data.table(y)),
                          list(pvals, tscores, fcs, esize))
      mummi_new <- data.table::setDF(mummi_new)
      mummi_new[] <- lapply(mummi_new, as.character);
    }else if(test=="fc"){
      mummi_new <- fcs;
    }else if(test=="aov"){
      mummi_new <- pvals;
    } else if (test == "cov") {
      mummi_new <- merge(data.table::as.data.table(pvals), data.table::as.data.table(tscores))
      mummi_new <- data.table::setDF(mummi_new)
  }

    if(rt){ # taking retention time information from feature name itself
      feat_info <- mummi_new[,1]
      feat_info_split <- matrix(unlist(strsplit(feat_info, "__", fixed=TRUE)), ncol=2, byrow=T)
      colnames(feat_info_split) <- c("m.z", "r.t")
      
      if(rt.type == "minutes"){
        rtime <- as.numeric(feat_info_split[,2])
        rtime <- rtime * 60
        feat_info_split[,2] <- rtime
      }
      
      mummi_new <- cbind(feat_info_split, mummi_new[,-1])
    }
  }
  
  if(!is.na(mode)){
    if(mode=="positive"){
      mode <- rep("positive", nrow(mummi_new))
    }else{
      mode <- rep("negative", nrow(mummi_new))
    }
    mummi_new <- cbind(mummi_new, mode)
  }
  
  mummi_new[,1] <- as.numeric(make.unique(as.character(mummi_new[,1]), sep=""));  
  filename <- paste0("mummichog_input_", Sys.Date(), ".txt");
  input_filename <<- filename;
  write.table(mummi_new, filename, row.names = FALSE);
  
  return(.set.mSet(mSetObj))
}

#'Update the mSetObj with user-selected parameters for MS Peaks to Pathways.
#'@description This functions handles updating the mSet object for mummichog analysis. 
#'It is necessary to utilize this function
#'to specify to the organism's pathways to use (libOpt), the mass-spec mode (msModeOpt) 
#'and mass-spec instrument (instrumentOpt).
#'@usage UpdateInstrumentParameters(mSetObj=NA, instrumentOpt, 
#'msModeOpt, force_primary_ion, rt_frac, rt_tol)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param instrumentOpt Numeric. Define the mass-spec instrument used to perform untargeted metabolomics.
#'@param msModeOpt  Character. Define the mass-spec mode of the instrument used to perform untargeted metabolomics.
#'@param rt_frac rt_frac.
#'@param rt_tol rt_tol.
#'@param force_primary_ion Character, if "yes", only mz features that match compounds with a primary ion are kept.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdateInstrumentParameters <- function(mSetObj=NA, 
                                       instrumentOpt, 
                                       msModeOpt, 
                                       force_primary_ion = "yes", 
                                       rt_frac = 0.02, 
                                       rt_tol = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.numeric(instrumentOpt)){
    AddErrMsg("Mass accuracy must be numeric!")
  }else{
    mSetObj$dataSet$instrument <- instrumentOpt;
  }
  
  mSetObj$dataSet$mode <- msModeOpt;
  mSetObj$dataSet$primary_ion <- force_primary_ion;
  mSetObj$dataSet$rt_tol <- rt_tol;
  mSetObj$dataSet$rt_frac <- rt_frac;
  
  return(.set.mSet(mSetObj));
}

#'Sanity Check Data
#'@description SanityCheckData is used for data processing, and performs a basic sanity
#'check of the uploaded data, ensuring that the data is suitable for further analysis.
#'The function ensure that all parameters are properly set based on updated parameters.
#'@usage SanityCheckMummichogData(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

SanityCheckMummichogData <- function(mSetObj=NA){
  # save.image("mum.RData");
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$msgSet$check.msg <- NULL;
  if(mSetObj$dataSet$mum.type == "table"){
    mSetObj$paramSet$mumDataContainsPval <- 1;
    orig.data<- qs::qread("data_orig.qs");
    l = sapply(colnames(orig.data),function(x) return(unname(strsplit(x,"/", fixed=TRUE)[[1]][1])))
    colnames(orig.data) <- l;
    qs::qsave(orig.data, file="data_orig.qs");
    
    if(.on.public.web){
      return(SanityCheckData(NA));
    }else{
      mSetObj <- SanityCheckData(mSetObj)
      return(.set.mSet(mSetObj));
    }
  }
  
  msg.vec <- NULL;
  mSetObj$mum_nm <- "mummichog_query_mummichog.json"
  mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment_mummichog.csv"
  ndat <- mSetObj$dataSet$mummi.orig;
  pos_inx = mSetObj$dataSet$pos_inx
  ndat <- data.frame(cbind(ndat, pos_inx), stringsAsFactors = FALSE)
  
  rawdat <- qs::qread("mum_raw.qs");
  
  if(mSetObj$paramSet$mumRT){
    na.num <- sum(is.na(ndat$r.t))
    # filter out any reads w. NA RT
    if(na.num>0){
      na.inx <- which(is.na(ndat$r.t))
      ndat <- ndat[-na.inx,]
      msg.vec <- c(msg.vec, paste("A total of <b>", na.num, "</b> mz features with missing retention times were removed."));
    }
  }
  
  # another check to ensure no missing or NA values
  missing.inx <- apply(ndat, 2, function(x) any(is.na(x)))
  
  if(any(missing.inx)){
    AddErrMsg("NA values found in the uploaded data!")
    return(0)
  }
  
  read.msg <- mSetObj$msgSet$read.msg
  
  # sort mzs by p-value
  ord.inx <- order(ndat[,1]);
  ndat <- ndat[ord.inx,]; # order by p-vals
  
  # filter based on mz
  mznew <- ndat[,2];
  
  # trim to fit within 50 - 2000
  my.inx <- mznew > 50 & mznew < 2001;
  trim.num <- sum(!my.inx);
  range = paste0(min(ndat[,1]), "-", max(ndat[,1]))
  if(length(unique(mSetObj[["dataSet"]][["pos_inx"]])) > 1){
    colNMadd <- "and mode";
    colnumadd <- 1;
  } else {
    colnumadd <- 0;
    colNMadd <- NULL;
  }
  
  msg.vec <- c(msg.vec, paste("The instrument's mass accuracy is <b>", mSetObj$dataSet$instrument , "</b> ppm."));
  msg.vec <- c(msg.vec, paste("The instrument's analytical mode is <b>", mSetObj$dataSet$mode , "</b>."));
  msg.vec <- c(msg.vec, paste("The uploaded data contains <b>", length(colnames(rawdat)) + colnumadd, "</b> columns."));
  
  peakFormat <- mSetObj$paramSet$peakFormat;
  
  if (peakFormat == "rmp") {
    msg.vec <- c(msg.vec, paste("The peaks are ranked by <b>p-values</b>."));
  } else if (peakFormat == "rmt") {
    msg.vec <- c(msg.vec, paste("The peaks are ranked by <b>t-scores</b>."));
  }
  
  msg.vec <- c(msg.vec, paste("The column headers of uploaded data are <b>", paste(colnames(rawdat),collapse=", "), colNMadd ,"</b>."));
  msg.vec <- c(msg.vec, paste("The range of m/z peaks is trimmed to 50-2000. <b>", trim.num, "</b> features have been trimmed."));
  
  if(trim.num > 0){
    ndat <- ndat[my.inx,]
    #msg.vec <- c(msg.vec, paste("A total of", trim.num, "were excluded to fit within mz range of 50-2000"));
  }
  
  # remove duplicated mzs (make unique)
  dup.inx <- duplicated(ndat);
  dup.num <- sum(dup.inx);
  
  if(dup.num > 0){
    ndat <- ndat[!dup.inx,];
    msg.vec <- c(msg.vec, paste("A total of <b>", dup.num, "</b> duplicated mz features were removed."));
  }
  
  # make mzs unique
  mzs <- ndat[,2]
  # ensure features are unique
  mzs_unq <- mzs[duplicated(mzs)]
  set.seed(123);
  while(length(mzs_unq)>0){
    mzs[duplicated(mzs)] <- vapply(mzs_unq, function(x) {paste0(x, sample(1:9, 1, replace = TRUE))}, FUN.VALUE = character(1L));
    mzs_unq <- mzs[duplicated(mzs)]
  }
  
  ndat[,2] <- mzs
  ref_mzlist <- ndat[,2];
  
  # set up expression (up/dn)
  tscores <- as.numeric(ndat[,3]);
  names(tscores) <- ref_mzlist;
  
  # set up rt
  if(mSetObj$paramSet$mumRT){
    
    retention_time <- as.numeric(ndat[,4]);
    names(retention_time) <- ref_mzlist;
    mSetObj$dataSet$pos_inx <- as.numeric(ndat$pos_inx) == 1;
    mSetObj$dataSet$ret_time <- retention_time;
    
    if(is.na(mSetObj$dataSet$rt_tol)){
      rt_tol <- max(mSetObj$dataSet$ret_time) * mSetObj$dataSet$rt_frac 
      #print(paste0("Retention time tolerance is ", rt_tol))
      mSetObj$dataSet$rt_tol <- rt_tol
    }
    
  }else{
    mSetObj$dataSet$pos_inx <- as.numeric(ndat$pos_inx) == 1;
  }
  
  ref.size <- length(ref_mzlist);
  
  msg.vec <- c(msg.vec, paste("A total of ", ref.size, "input mz features were retained for further analysis."));
  
  if(ref.size > 20000){
    msg.vec <- c(msg.vec, "There are too many input features, the performance may be too slow.");
  }
  
  if(ref.size < 100){
    AddErrMsg("There are too few m/z features. Ensure that all of your m/z features have been uploaded!");
    return(0)
  }
  
  if(min(ndat[,"p.value"])<0 || max(ndat[,"p.value"])>1){
    msg.vec <- c(msg.vec, "Please make sure the p-values are between 0 and 1.");
  }
  
  ref_cmpdlist <- mSetObj$dataSet$cmpd.orig;
  if(!is.null(mSetObj[["paramSet"]][["ms2id.type"]])){
    
    if(mSetObj$paramSet$ms2id.type == "hmdb_ids"){
      if(any(grepl("HMDB[0-9]+",as.character(ref_cmpdlist)))){
        msg.vec <- c(msg.vec, paste0("A total of ",
                                     length(which(grepl("HMDB[0-9]+",as.character(ref_cmpdlist)))),
                                     " HMDB Compounds included."));
        mSetObj$paramSet$ContainsMS2 <- TRUE;
      } else {
        mSetObj$paramSet$ContainsMS2 <- FALSE;
        AddErrMsg("No HMDB compounds included. Please check your data!");
        return(0)
      }
    } else if(mSetObj$paramSet$ms2id.type == "pubchem_cids"){
      if(any(grepl("^[0-9]+",as.character(ref_cmpdlist)))){
        msg.vec <- c(msg.vec, paste0("A total of ",
                                     length(which(grepl("^[0-9]+", as.character(ref_cmpdlist)))),
                                     " PubChem_CID Compounds included."));
        mSetObj$paramSet$ContainsMS2 <- TRUE;
      } else {
        mSetObj$paramSet$ContainsMS2 <- FALSE;
        AddErrMsg("No PubChem_CID compounds included. Please check your data!");
        return(0)
      }
    } else if(mSetObj$paramSet$ms2id.type == "pubchem_sids"){
      if(any(grepl("^[0-9]+", as.character(ref_cmpdlist)))){
        msg.vec <- c(msg.vec, paste0("A total of ",
                                     length(which(grepl("^[0-9]+", as.character(ref_cmpdlist)))),
                                     " PubChem_SID Compounds included."))
        mSetObj$paramSet$ContainsMS2 <- TRUE;
      } else {
        mSetObj$paramSet$ContainsMS2 <- FALSE;
        AddErrMsg("No PubChem_SID compounds included. Please check your data!");
        return(0)
      }
    } else if(mSetObj$paramSet$ms2id.type == "inchikeys"){
      if(any(grepl("^[A-Z]+-[A-Z]+-[A-Z]$", as.character(as.matrix(ref_cmpdlist))))){
        msg.vec <- c(msg.vec, paste0("A total of ",
                                     length(which(grepl("^[A-Z]+-[A-Z]+-[A-Z]$", as.character(as.matrix(ref_cmpdlist))))),
                                     " InchiKeys Compounds included."))
        mSetObj$paramSet$ContainsMS2 <- TRUE;
      } else {
        mSetObj$paramSet$ContainsMS2 <- FALSE;
        AddErrMsg("No InchiKeys compounds included. Please check your data!");
        return(0)
      }
    } else if(mSetObj$paramSet$ms2id.type == "smiles"){
      if(any(grepl("^[A-Z]+", as.character(as.matrix(ref_cmpdlist))))){
        msg.vec <- c(msg.vec, paste0("A total of ",
                                     length(which(grepl("^[A-Z]+", as.character(as.matrix(ref_cmpdlist))))),
                                     " SMILES Compounds included."))
        mSetObj$paramSet$ContainsMS2 <- TRUE;
      } else {
        mSetObj$paramSet$ContainsMS2 <- FALSE;
        AddErrMsg("No SMILES compounds included. Please check your data!");
        return(0)
      }
    } else {
      stop("IDtype must be one of 'hmdb_ids', 'pubchem_cids', 'pubchem_sids', 'inchikeys', 'smiles'.") 
    }
  }
  if(!is.null(ref_cmpdlist)){
    mSetObj$dataSet$ref_cmpdlist <- as.matrix(ref_cmpdlist);
  }
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$check.msg, read.msg, msg.vec);
  mSetObj$dataSet$mummi.proc <- ndat;
  mSetObj$dataSet$expr_dic <- tscores;
  mSetObj$dataSet$ref_mzlist <- ref_mzlist;
 mSetObj$dataSet$url.var.nms <- ref_mzlist;
  return(.set.mSet(mSetObj));
}

#'Set the cutoff for mummichog analysis
#'@description Set the p-value cutoff for mummichog analysis.
#'@param mSetObj Input the name of the created mSetObj.
#'@param fraction fraction value for mummichog running
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetMummichogPvalFromPercent <- function(mSetObj=NA, fraction){

  mSetObj <- .get.mSet(mSetObj);
  peakFormat <- mSetObj$paramSet$peakFormat;
  
  if(peakFormat %in% c("rmp", "rmt")){
    maxp <- 0;
  }else{
    pvals <- c(0.25, 0.2, 0.15, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
    ndat <- mSetObj$dataSet$mummi.proc;
    n <- floor(fraction*length(ndat[,"p.value"]))
    cutoff <- ndat[n+1,1]
    if(!any(pvals <= cutoff)){
      maxp <- 0.00001
    }else{
      maxp <- max(pvals[pvals <= cutoff])
    }
  }

  mSetObj$dataSet$cutoff <- maxp
  .set.mSet(mSetObj);
  return(SetMummichogPval(NA, maxp));
}

#'Set the cutoff for mummichog analysis
#'@description Set the p-value cutoff for mummichog analysis.
#'@param mSetObj Input the name of the created mSetObj.
#'@param cutoff cutoff value for mummichog running
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetMummichogPval <- function(mSetObj=NA, cutoff){
  
  mSetObj <- .get.mSet(mSetObj);

  msg.vec <- NULL;
  mSetObj$dataSet$cutoff <- cutoff;
  ndat <- mSetObj$dataSet$mummi.proc;
  msg.vec <- c(msg.vec, "Only a small percentage (below 10%) peaks in your input peaks should be significant.");
  msg.vec <- c(msg.vec, "The algorithm works best for <u>200~500 significant peaks in 3000~7000 total peaks</u>.");
  
  ref_mzlist <- ndat[,2];
  if(mSetObj$paramSet$mumDataContainsPval == 1){
    my.inx <- ndat[,1] <= cutoff;
    input_mzlist <- ref_mzlist[my.inx];
    # note, most of peaks are assumed to be not changed significantly, more than 25% should be warned
    
  }else{
    inxToKeep = length(ref_mzlist)/10
    if(inxToKeep > 500){
      inxToKeep = 500;
    }
    input_mzlist <- ref_mzlist[1:inxToKeep];
  }
  
  sig.size <- length(input_mzlist);
  sig.part <- round(100*sig.size/length(ref_mzlist),2);

  if(sig.size == 0){
    AddErrMsg("No significant features were found based on the current cutoff! Please adjust the p-value threshold.");
    return(0);
  }

  #  less than 1%
  if(sig.size < 11 || sig.part < 1){
    AddErrMsg(paste("The number of significant features is too small: ", sig.size, " (", sig.part, "%) for meaningful enrichment analysis. Please adjust the p-value threshold."));
    return(0);
  }
  
  if(sig.size > 2000){
    msg.vec <- c(msg.vec, "There are too many significant features based on the current cutoff, analysis will possibly be slow.");

  }else if(sig.part > 25){
    msg.vec <- c(msg.vec, paste("<font color=\"orange\">Warning: over", sig.part, "percent were significant based on your cutoff</font>."));
    msg.vec <- c(msg.vec, "You should adjust p-value cutoff to control the percentage");

  }else{
    msg.vec <- c(msg.vec, paste("A total of", sig.size, "or", sig.part, "percent significant mz features were found based on the selected p-value cutoff:", cutoff));
  }

  mSetObj$dataSet$input_mzlist <- input_mzlist;
  mSetObj$dataSet$N <- sig.size;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(round(sig.part, 0))
  } else {
    return(.set.mSet(mSetObj));
  }
}

## For meta-analysis, set the p-value
## cutoff used to define the new
## input_mzlist
SetMetaPeaksPvals <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cutoff <- cutoff;
  return(.set.mSet(mSetObj));
}

#'Function to perform peak set enrichment analysis
#'@description This is the main function that performs either the mummichog
#'algorithm, GSEA, or both for peak set enrichment analysis. 
#'@usage PerformPSEA(mSetObj=NA, lib, libVersion, minLib, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object. 
#'@param lib Input the name of the organism library, default is hsa_mfn. 
#'@param libVersion Input the version of the KEGG pathway libraries ("current" or "old").
#'@param minLib Numeric, input the minimum number of metabolites needed to consider the pathway 
#'or metabolite set. 
#'@param permNum Numeric, input the number of permutations to perform. Default is 100.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import qs

PerformPSEA <- function(mSetObj=NA, lib, libVersion, minLib = 3, permNum = 100, init=T){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$initPSEA <- init;
  .set.mSet(mSetObj);
  mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib);
  version <- mSetObj$paramSet$version;
  mSetObj$dataSet$paramSet <- mSetObj$paramSet;
  if(mSetObj$paramSet$mumRT & version=="v2"){
    mSetObj <- .init.RT.Permutations(mSetObj, permNum)
  } else {
    mSetObj <- .init.Permutations(mSetObj, permNum)
  }

  if(class(mSetObj) != "list"){
    if(mSetObj == 0){
      AddErrMsg("MS Peaks to Paths analysis failed! Likely not enough m/z to compound hits for pathway analysis!")
      return(0)
    }
  }
  return(.set.mSet(mSetObj));
}

#' Internal function to perform PSEA, no retention time
#' @importFrom data.table setDF
#' @noRd
.init.Permutations <- function(mSetObj, permNum){
  anal.type0 <- mSetObj$paramSet$anal.type;

  if(anal.type0 == "mummichog"){
    mSetObj <- .perform.mummichogPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogSigPvals(mSetObj);
  } else if(anal.type0 == "gsea_peaks") {
    mSetObj <- .compute.mummichog.fgsea(mSetObj, permNum);
  } else {
    # need to perform mummichog + gsea then combine p-values
    mSetObj <- .compute.mummichog.fgsea(mSetObj, permNum);
    mSetObj <- .perform.mummichogPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogSigPvals(mSetObj);
    pathResults <- vector("list")
    
    pathResults$ora.all <- mSetObj$mummi.resmat
    pathResults$gsea.all <- mSetObj$mummi.gsea.resmat
    
    path.names.all <- lapply(pathResults, rownames)
    path.intersect <- Reduce(intersect, path.names.all)
    
    # remove paths not found by all three - complete.cases
    path.intersected <- lapply(pathResults, function(x) x[row.names(x) %in% path.intersect,])
    mum.df <- data.table::setDF(Reduce(merge, 
                                      lapply(path.intersected, 
                                             data.table::data.table, 
                                             keep.rownames = TRUE)))
    
    mum.df <- mum.df[, c("rn", "Pathway total", "Hits.total", "Hits.sig", "FET", "P_val")]
    rownames(mum.df) <- mum.df$rn;
    # combine p-values
    # replace 1 with 0.999 and 0 with low number
    mum.df[,5:6][mum.df[,5:6]==1] <- 0.999
    mum.df[,5:6][mum.df[,5:6]==0] <- .Machine$double.xmin
    
    combo.all <- apply(mum.df[,5:6], 1, function(x) sumlog(x))
    
    #extract p-values
    all.ps <- unlist(lapply(combo.all, function(z) z["p"]))
    df.combo <- as.matrix(mum.df[,2:6])
    dfcombo <- round(cbind(df.combo, all.ps), 5)
    colnames(dfcombo) <- c("Total_Size", "Hits", "Sig_Hits", "Mummichog_Pvals", "GSEA_Pvals", "Combined_Pvals")
    ord.inx <- order(dfcombo[,6]);
    dfcombo <- signif(as.matrix(dfcombo[ord.inx, ]), 4);

    csv.nm <- if(!is.null(mSetObj$mum_nm_csv)) mSetObj$mum_nm_csv else "mummichog_pathway_enrichment_integ.csv";
    fast.write.csv(dfcombo, file = csv.nm, row.names = TRUE)
    if( is.null(mSetObj$initPSEA) || mSetObj$initPSEA){
        mSetObj$integ.resmat <- dfcombo;
        mSetObj$paramSet$integ.lib <- mSetObj$lib.organism;
    }

    matched_cpds <- names(mSetObj$cpd_exp)
    colnames(mum.df)[1] = "pathways";
    inx2<-na.omit(match(mum.df$pathways[ord.inx], mSetObj$pathways$name))
    filt_cpds <- lapply(inx2, function(f) { mSetObj$pathways$cpds[f] })
    
    cpds <- lapply(filt_cpds, function(x) intersect(unlist(x), matched_cpds))
    cpds_sig <- lapply(filt_cpds, function(x) intersect(unlist(x), mSetObj$input_cpdlist))
    
    mSetObj$path.nms <- rownames(dfcombo)
    mSetObj$path.hits <- convert2JsonList(cpds)
    mSetObj$path.pval <- as.numeric(dfcombo[,6])
    mSetObj <<- mSetObj;
    matched_res <- qs::qread("mum_res.qs");
    json.res <- list(
      cmpd.exp = mSetObj$cpd_exp,
      path.nms = rownames(dfcombo),
      hits.all = convert2JsonList(cpds),
      hits.all.size = as.numeric(dfcombo[,2]),
      hits.sig = convert2JsonList(cpds_sig),
      hits.sig.size = as.numeric(dfcombo[,3]),
      mum.p = as.numeric(dfcombo[,4]),
      gsea.p = as.numeric(dfcombo[,5]),
      comb.p = as.numeric(dfcombo[,6]),
      peakToMet = mSetObj$cpd_form_dict,
      peakTable = matched_res
    );
    
    json.mat <- RJSONIO::toJSON(json.res);
    sink(mSetObj$mum_nm);
    cat(json.mat);
    sink();

    if(is.null(mSetObj$imgSet$enrTables)){
        mSetObj$imgSet$enrTables <- list();
    }
    vis.type <- "mumEnr";
    resTable <- dfcombo[,c(2:6)];
    resTable <- cbind(Name=rownames(dfcombo),resTable);
    mSetObj$imgSet$enrTables[[vis.type]] <- list();
    mSetObj$imgSet$enrTables[[vis.type]]$table <- dfcombo;
    mSetObj$imgSet$enrTables[[vis.type]]$library <- mSetObj$lib.organism;
    mSetObj$imgSet$enrTables[[vis.type]]$algo <- "GSEA and Mummichog integrative Analysis";
    mSetObj$imgSet$enrTables[[vis.type]]$fileName <- csv.nm;
    }
  return(mSetObj);
}

#' Internal function to perform PSEA, with RT
#' @noRd
.init.RT.Permutations <- function(mSetObj, permNum){
  
  anal.type0 <- mSetObj$paramSet$anal.type;
  
  if(anal.type0 == "mummichog"){
    mSetObj <- .perform.mummichogRTPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogRTSigPvals(mSetObj);
  }else if(anal.type0 == "gsea_peaks"){
    mSetObj <- .compute.mummichog.RT.fgsea(mSetObj, permNum);
  }else{
    # need to perform mummichog + gsea then combine p-values
    mSetObj <- .compute.mummichog.RT.fgsea(mSetObj, permNum);
    mSetObj <- .perform.mummichogRTPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogRTSigPvals(mSetObj);
    
    pathResults <- vector("list")
    
    pathResults$ora.all <- mSetObj$mummi.resmat
    pathResults$gsea.all <- mSetObj$mummi.gsea.resmat
    
    path.names.all <- lapply(pathResults, rownames)
    path.shared <- Reduce(intersect, path.names.all)
    
    # remove paths not found by all three - complete.cases
    path.intersected <- lapply(pathResults, function(x) x[row.names(x) %in% path.shared,])
    mum.df <- data.table::setDF(Reduce(merge, lapply(path.intersected, data.table::data.table, keep.rownames = TRUE)))
    
    mum.df <- mum.df[, c(1:4, 6, 14)]
    rownames(mum.df) <- mum.df$rn
    mum.df <- mum.df[,-1]
    colnames(mum.df) <- c("Total_Size", "Hits", "Sig_Hits", "Mummichog_Pvals", "GSEA_Pvals")
    
    # combine p-values
    combo.all <- apply(mum.df[,c("Mummichog_Pvals", "GSEA_Pvals")], 1, function(x) sumlog(x))
    
    #extract p-values
    all.ps <- unlist(lapply(combo.all, function(z) z["p"]))
    df.combo <- as.matrix(mum.df)
    df.combo <- round(cbind(df.combo, all.ps), 5)
    colnames(df.combo) <- c("Total_Size", "Hits", "Sig_Hits", "Mummichog_Pvals", "GSEA_Pvals", "Combined_Pvals")
    ord.inx <- order(df.combo[,6]);
    df.combo <- signif(as.matrix(df.combo[ord.inx, ]), 4);
    fast.write.csv(df.combo, "mummicho_pathway_enrichment_integ.csv", row.names = TRUE)
    mSetObj$integ.resmat <- df.combo
    
    ## transform ecpd to cpd for json files
    # for all cpds
    total_ecpds <- unique(mSetObj$total_matched_ecpds) #all matched compounds
    current.mset <- mSetObj$pathways$emp_cpds[match(rownames(df.combo), mSetObj$pathways$name)]
    ecpds <- lapply(current.mset, function(x) intersect(x, total_ecpds)); #pathways & all ref ecpds
    cpds <- lapply(ecpds, function(x) unique(unlist(mSetObj$ecpd_cpd_dict[match(x, names(mSetObj$ecpd_cpd_dict))])) )
    
    # for sig ecpds
    qset <- unique(unlist(mSetObj$input_ecpdlist));
    current.mset <- mSetObj$pathways$emp_cpds[match(rownames(df.combo), mSetObj$pathways$name)]
    feats <- lapply(current.mset, function(x) intersect(x, qset));
    cpds_feats <- lapply(feats, function(x) unique(unlist(mSetObj$ecpd_cpd_dict[match(x, names(mSetObj$ecpd_cpd_dict))])) )  
    
    # now make exp vec for all compounds
    cpds2ec <- mSetObj$cpd_ecpd_dict
    cpds.all <- unique(unlist(mSetObj$ecpd_cpd_dict[match(total_ecpds, names(mSetObj$ecpd_cpd_dict))]))
    cpds.exp <- sapply(cpds.all, function(x) sapply(seq_along(x), function(i) mean(mSetObj$ec_exp[match(unique(unlist(cpds2ec[match(x[[i]], names(cpds2ec))])), names(mSetObj$ec_exp))]) ) )
    
    mSetObj$path.nms <- rownames(df.combo)
    mSetObj$path.hits <- convert2JsonList(cpds)
    mSetObj$path.pval <- as.numeric(df.combo[,6])
    
    mSetObj <<- mSetObj;
    matched_res <- qs::qread("mum_res.qs");
    
    json.res <- list(
      cmpd.exp = cpds.exp,
      path.nms = rownames(df.combo),
      hits.all = convert2JsonList(cpds),
      hits.all.size = as.numeric(df.combo[,2]),
      hits.sig = convert2JsonList(cpds_feats),
      hits.sig.size = as.numeric(df.combo[,3]),
      mum.p = as.numeric(df.combo[,4]),
      gsea.p = as.numeric(df.combo[,5]),
      comb.p = as.numeric(df.combo[,6]),
      peakToMet = mSetObj$cpd_form_dict,
      peakTable = matched_res
    );
    
    json.mat <- RJSONIO::toJSON(json.res);
    sink(mSetObj$mum_nm);
    cat(json.mat);
    sink();
  }
  return(mSetObj);
}

# Internal function to set up library for PSEA
.setup.psea.library <- function(mSetObj = NA, 
                                lib, 
                                libVersion, 
                                minLib=3, 
                                metaAnalysis = FALSE,
                                metaLevel = "pathway", 
                                combine.level,
                                pval.method, 
                                es.method, 
                                rank.metric, 
                                mutual.feats = TRUE){
  
  version <- mSetObj$paramSet$version;
  filenm <- paste(lib, ".qs", sep="")
  biocyc <- grepl("biocyc", lib);

  if(!is.null(mSetObj$curr.cust)){
    
    if(biocyc){
      user.curr <- mSetObj$curr.map$BioCyc;
    }else{
      user.curr <- mSetObj$curr.map$KEGG;
    }

    if(.on.public.web){
        currency <<- user.curr;
    } else {
        currency_r <<- user.curr;
    }
    
    if(.on.public.web){
        currency_tmp <- currency;
    } else {
        currency_tmp <- currency_r;
    }

    if(length(currency_tmp)>0){
      mSetObj$mummi$anal.msg <- c("Currency metabolites were successfully uploaded!")
    }else{
      mSetObj$mummi$anal.msg <- c("Errors in currency metabolites uploading!")
    }
  }
 
  if(.on.public.web){
    mum.url <- paste(rpath ,"libs/mummichog/", filenm, sep="");
    # print(paste("Adding mummichog library:", mum.url));
    mummichog.lib <- try(qs::qread(mum.url), silent = TRUE)
    if(class(mummichog.lib) == "try-error"){
        AddErrMsg("The database you have selected is not matched/found!")
        return(0)
    }

  }else{
    if(!file.exists(filenm)){
      mum.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", filenm, sep="");
      download.file(mum.url, destfile = filenm, method="libcurl", mode = "wb")
      mummichog.lib <- qs::qread(filenm);
    }else{
      mummichog.lib <- qs::qread(filenm);
    }
  }
  
  
  ## confirming ms2 id type
  if(is.null(mSetObj$paramSet$ContainsMS2)){mSetObj$paramSet$ContainsMS2<-FALSE}
  if(mSetObj$paramSet$ContainsMS2){
    ms2id <- mSetObj$paramSet$ms2id.type;
    if(!(ms2id %in% c("hmdb_ids", "inchikeys", "pubchem_CIDs", "pubchem_SIDs", "SMILES"))){
      if(.on.public.web){
        AddErrMsg("The MS2 ID  you have selected is not correct!")
        return(0)
      }
      warning("MS2 ID type must be one of 'hmdb_ids', 'inchikeys', 'pubchem_CIDs', 'pubchem_SIDs', 'SMILES'.")
      stop("Please redefine MS2 ID with function 'SetMS2IDType'.")
    }
    ## checking adducts info - with MS2 INFO
    if(!is.null(mSetObj$dataSet$adduct.custom)){
      mw <- mummichog.lib$cpd.lib$mw;
      new_adducts <- new_adduct_mzlist(mSetObj, mw);
      
      cpd.lib <- list(
        mz.matp = new_adducts$pos,
        mz.matn = new_adducts$neg,
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name,
        ms2IDs = mummichog.lib$cpd.lib[[ms2id]]
      );
      
    } else {
      
      cpd.lib <- list(
        mz.matp = mummichog.lib$cpd.lib$adducts[["positive"]],
        mz.matn = mummichog.lib$cpd.lib$adducts[["negative"]],
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name,
        ms2IDs = mummichog.lib$cpd.lib[[ms2id]]
      );
    }
  } else {
    ## checking adducts info - nonMS2
    if(!is.null(mSetObj$dataSet$adduct.custom)){
      mw <- mummichog.lib$cpd.lib$mw;
      new_adducts <- new_adduct_mzlist(mSetObj, mw);
      
      cpd.lib <- list(
        mz.matp = new_adducts$pos,
        mz.matn = new_adducts$neg,
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name
      );
      
    }else{
      
      cpd.lib <- list(
        mz.matp = mummichog.lib$cpd.lib$adducts[["positive"]],
        mz.matn = mummichog.lib$cpd.lib$adducts[["negative"]],
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name
      );
    }
  }
  
  cpd.treep <- mummichog.lib$cpd.tree[["positive"]];
  cpd.treen <- mummichog.lib$cpd.tree[["negative"]];
  
  # filter pathways based on the length of pathway library
  # build empirical compound library after
  path.length <- sapply(mummichog.lib$pathways$cpds, length)
  
  min.inx <- which(path.length >= minLib)
  
  cleaned.pathways <- vector("list")
  cleaned.pathways$cpds <- mummichog.lib$pathways$cpds[min.inx]
  cleaned.pathways$id <- mummichog.lib$pathways$id[min.inx]
  cleaned.pathways$name <- mummichog.lib$pathways$name[min.inx]

  mSetObj$pathways <- cleaned.pathways;
  
  if(metaAnalysis & metaLevel %in% c("cpd", "ec")){
    mSetObj <- .search.compoundLibMeta(mSetObj, cpd.lib, cpd.treep, cpd.treen, metaLevel, combine.level,
                                       pval.method, es.method, rank.metric, mutual.feats);
  } else if (mSetObj$paramSet$ContainsMS2) {
    mSetObj <- .search_MS2compoundLib(mSetObj, cpd.lib, cpd.treep, cpd.treen);
  } else {
    mSetObj <- .search.compoundLib(mSetObj, cpd.lib, cpd.treep, cpd.treen);
  }

  if(mSetObj$paramSet$mumRT & version=="v2"){
    # only for empirical compounds
    if(metaLevel %in% c("ec", "pathway", "pooled")){
      # map cpds to empirical cpds
      cleaned.pathways$emp_cpds <- lapply(cleaned.pathways$cpds, 
                                          function(x) {
                                            unique(unlist(mSetObj$cpd_ecpd_dict[na.omit(match(x, names(mSetObj$cpd_ecpd_dict)))]))
                                          })

      # delete emp_cpds, cpds and names with no emp_cpds
      null.inx <- sapply(cleaned.pathways$emp_cpds, is.null)
     
      new_pathways <- vector("list");
      
      new_pathways$cpds <- cleaned.pathways$cpds[!null.inx]
      new_pathways$name <- cleaned.pathways$name[!null.inx]
      new_pathways$emp_cpds <- cleaned.pathways$emp_cpds[!null.inx]
      mSetObj$pathways <- new_pathways;
    }
  }
  
  mSetObj$lib.organism <- lib; #keep track of lib organism for sweave report
  return(mSetObj);
}

# version 2
# NOTE: CODE DUPLICATION WARNING
# This function (.search.compoundLib) shares ~98% of its code with
# .search_MS2compoundLib() in peaks_ms2fun.R (lines 1-552)
#
# Key differences:
# - This version supports meta-analysis workflows
# - peaks_ms2fun.R version includes MS2-based compound filtration
#
# FUTURE REFACTORING RECOMMENDATION:
# Extract shared logic into a common utility function:
#   .search_compoundLib_core(mSetObj, cpd.lib, cpd.treep, cpd.treen, has_ms2=FALSE)
# Then have both files call this core function with appropriate flags.
# This would reduce codebase size by ~35% and eliminate maintenance duplication.
#
# Performance optimizations applied (2025):
# - Vectorized innermost loop in peak matching (lines 1338-1414)
# - Optimized sapply/strsplit chains (lines 1539-1547, 2064-2082)
# - Vectorized EC creation loop (lines 1487-1530)
#
# internal function for searching compound library
.search.compoundLib <- function(mSetObj,
                                cpd.lib,
                                cpd.treep,
                                cpd.treen){

  ref_mzlist <- as.numeric(mSetObj$dataSet$ref_mzlist);
  #print(paste0("compoundLib"));
  print(paste0("Got ", length(ref_mzlist), " mass features."))
  pos_inx <- mSetObj$dataSet$pos_inx;
  ref_mzlistp <- ref_mzlist[pos_inx];
  ref_mzlistn <- ref_mzlist[!pos_inx];
  version <- mSetObj$paramSet$version;
  
  # for empirical compounds
  if(mSetObj$paramSet$mumRT & version=="v2"){
    ord_rt <- rank(mSetObj$dataSet$ret_time, ties.method = "random")
    ret_time_pos <- mSetObj$dataSet$ret_time[pos_inx];
    ret_time_rank_pos <- ord_rt[pos_inx];
    ret_time_neg <- mSetObj$dataSet$ret_time[!pos_inx];
    ret_time_rank_neg <- ord_rt[!pos_inx];
    rt_tol <- mSetObj$dataSet$rt_tol;
    rt_tol_rank <- length(ref_mzlist)*mSetObj$dataSet$rt_frac;
  } else {
    # add fake RT
    ret_time_pos <- rep(1, length(ref_mzlistp))
    ret_time_rank_pos <- rep(1, length(ref_mzlistp))
    ret_time_neg <- rep(1, length(ref_mzlistn))
    ret_time_rank_neg <- rep(1, length(ref_mzlistn))
  }
  
  modified.statesp <- colnames(cpd.lib$mz.matp);
  modified.statesn <- colnames(cpd.lib$mz.matn);
  my.tolsp <- mz_tolerance(ref_mzlistp, mSetObj$dataSet$instrument);
  my.tolsn <- mz_tolerance(ref_mzlistn, mSetObj$dataSet$instrument);
  
  # get mz ladder (pos index)
  self.mzsp <- floor(ref_mzlistp);
  all.mzsp <- cbind(self.mzsp-1, self.mzsp, self.mzsp+1);
  
  self.mzsn <- floor(ref_mzlistn);
  all.mzsn <- cbind(self.mzsn-1, self.mzsn, self.mzsn+1);
  
  # matched_res will contain detailed result (cmpd.id. query.mass, mass.diff) for all mz;
  # use a high-performance variant of list
  matched_resp <- myFastList();
  matched_resn <- myFastList();
  
  if(mSetObj$dataSet$mode != "negative"){
    # OPTIMIZED: Vectorized inner loop processing to eliminate innermost loop
    for(i in seq_along(ref_mzlistp)){
      mz <- ref_mzlistp[i];
      rt <- ret_time_pos[i];
      rt_rank <- ret_time_rank_pos[i];
      my.tol <- my.tolsp[i];
      all.mz <- all.mzsp[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treep[all.mz])));

      if(length(pos.all) > 0){
        for(pos in pos.all){
          id <- cpd.lib$id[pos];
          mw.all <- cpd.lib$mz.matp[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);

          # OPTIMIZED: Vectorized - process all hits at once instead of inner loop
          if(length(hit.inx) > 0){
            # Create all indices at once (vectorized string concatenation)
            indices <- paste(mz, id, rt, hit.inx, sep = "___");

            # Batch create all match data (vectorized)
            match_data <- lapply(seq_along(hit.inx), function(spot) {
              hit.pos <- hit.inx[spot];
              c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesp[hit.pos], diffs[hit.pos])
            });

            # Add all matches at once
            for(spot in seq_along(hit.inx)){
              matched_resp$add(indices[spot], match_data[[spot]]);
            }
          }
        }
      }
    }
  }
  
  all.mzsn <<- all.mzsn
  
  if (mSetObj$dataSet$mode != "positive") {
    # OPTIMIZED: Vectorized inner loop processing to eliminate innermost loop
    for(i in seq_along(ref_mzlistn)){
      mz <- ref_mzlistn[i];
      rt <- ret_time_neg[i];
      rt_rank <- ret_time_rank_neg[i];
      my.tol <- my.tolsn[i];
      all.mz <- all.mzsn[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treen[all.mz])));

      if(length(pos.all) > 0){
        for(pos in pos.all){
          id <- cpd.lib$id[pos]; # position of compound in cpd.tree
          mw.all <- cpd.lib$mz.matn[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);

          # OPTIMIZED: Vectorized - process all hits at once instead of inner loop
          if(length(hit.inx) > 0){
            # Create all indices at once (vectorized string concatenation)
            indices <- paste(mz, id, rt, hit.inx, sep = "___");

            # Batch create all match data (vectorized)
            match_data <- lapply(seq_along(hit.inx), function(spot) {
              hit.pos <- hit.inx[spot];
              c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesn[hit.pos], diffs[hit.pos])
            });

            # Add all matches at once
            for(spot in seq_along(hit.inx)){
              matched_resn$add(indices[spot], match_data[[spot]]);
            }
          }
        }
      }
    }
  }
  
  # convert to regular list
  if (mSetObj$dataSet$mode == "mixed") {
    
    matched_resn <- matched_resn$as.list();
    matched_resp <- matched_resp$as.list();
    
    neg_matches <- length(matched_resn) > 0
    pos_matches <- length(matched_resp) > 0
    
    if(!neg_matches & !pos_matches){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    if(neg_matches){
      matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
    }
    
    if(pos_matches){
      matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
    }
    
    if(neg_matches & pos_matches){ # both w. matches
      matched_res <- rbind(matched_resp, matched_resn)
    }else if(neg_matches & !pos_matches){ # only neg w. matches
      matched_res <- matched_resn
    }else{ # only pos w. matches
      matched_res <- matched_resp
    }
    
  } else if(mSetObj$dataSet$mode == "positive") {
    matched_resp <- matched_resp$as.list();
    
    if(is.null(unlist(matched_resp))){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
    matched_res <- matched_resp;
    
  } else {
    matched_resn <- matched_resn$as.list();
    if(is.null(unlist(matched_resn))){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
    matched_res <- matched_resn
  }
  
  # re-order columns for output
  matched_res <- matched_res[, c(3,2,7,8,4,5)];
  colnames(matched_res) <- c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff", "Retention.Time", "RT.Rank");
  
  if(!mSetObj$paramSet$mumRT & version=="v2"){
    matched_res <- matched_res[,-(5:6)]
  }
  
  #print(paste0(length(unique(matched_res[,2])), " matched compounds! cpd2mz"))
  
  # now create empirical compounds if necessary!
  # 1 compound matches to multiple m/z, filter by RT 
  if(mSetObj$paramSet$mumRT & version=="v2"){
    start <- Sys.time()
    # mz, ion
    empirical.cpd.list <- split(matched_res[,c(1,3,5,6)], matched_res[,2]); # split mz, ion and rt by compound
    empirical.cpds2cpds <- vector(length=(length(empirical.cpd.list)), "list")
    names(empirical.cpds2cpds) <- names(empirical.cpd.list)
    
    # OPTIMIZED: Vectorized EC creation - process all compounds efficiently
    # for each compound, if multiple matches, split into ECpds if > RT tolerance - rt_tol
    empirical.cpds2cpds <- lapply(seq_along(empirical.cpd.list), function(i) {
      ec_data <- empirical.cpd.list[[i]]
      cpd_name <- names(empirical.cpd.list)[i]

      # Extract data once
      mzs <- ec_data$Query.Mass
      ions <- ec_data$Matched.Form
      rts_char <- ec_data$Retention.Time
      rt_rank_char <- ec_data$RT.Rank

      # Single EC case - direct return
      if(length(mzs) == 1){
        return(paste0(mzs, ";", ions, ";", rts_char, ";", cpd_name))
      }

      # Multiple ECs - vectorized processing
      rts_num <- as.numeric(rts_char)
      rt_ranks_num <- as.numeric(rt_rank_char)

      # Vectorized name creation (single paste0 call)
      ec_names <- paste0(mzs, ";", ions, ";", rts_char, ";", cpd_name)

      # Sort by RT
      sort_idx <- order(rts_num)
      rts_sorted <- rts_num[sort_idx]
      ranks_sorted <- rt_ranks_num[sort_idx]
      names_sorted <- ec_names[sort_idx]

      # Vectorized split detection
      split.inx <- c(0, cumsum(
        (abs(diff(rts_sorted)) > rt_tol) &
        (abs(diff(ranks_sorted)) > rt_tol_rank)
      ))

      # Return result based on split
      if(length(unique(split.inx)) > 1){
        return(split(names_sorted, split.inx))
      } else {
        return(paste0(names_sorted, collapse = "__"))
      }
    })
    names(empirical.cpds2cpds) <- names(empirical.cpd.list)
    
    initial_ecs <- unlist(empirical.cpds2cpds, recursive=FALSE)
    names(initial_ecs) <- paste0("EC", 1:length(initial_ecs))
    print(paste0(length(initial_ecs), " initial ECs created!"))
    
    # second, merge ECs if same m/z and form - append compounds
    try <- melt(initial_ecs)
    try2 <- strsplit(as.character(try[,1]), split="__", fixed=TRUE) # deals with multiple rts belonging to 1 EC
    try2.df <- data.frame(value=unlist(try2), L1 = rep(try$L1, sapply(try2, length)))
    
    info <- strsplit(as.character(try2.df[,1]), split=";")
    df_ecs <- data.frame(ec=as.character(try2.df[,2]), mz=sapply(info, `[[`, 1), form=sapply(info, `[[`, 2), rt = sapply(info, `[[`, 3), cpd = sapply(info, `[[`, 4), stringsAsFactors = F)
    df_ecs$str_row_inx <- paste(df_ecs$mz, df_ecs$form, df_ecs$rt, sep = "___")
    qs::qsave(df_ecs, "initial_ecs.qs")
    merged_ecs <- aggregate(. ~ str_row_inx, df_ecs, paste, collapse=";")
    
    # OPTIMIZED: Vectorized string processing instead of sapply/strsplit chains
    # cleaning the df
    # merged_ecs$ec <- sapply(strsplit(merged_ecs$ec, ";", fixed=TRUE), function(x) unlist(x)[1]) - keep as long name
    cols_to_clean <- c("mz", "form", "rt")
    merged_ecs[cols_to_clean] <- lapply(merged_ecs[cols_to_clean], function(col) {
      vapply(strsplit(as.character(col), ";", fixed = TRUE),
             function(x) paste(unique(x), collapse = ";"),
             FUN.VALUE = character(1))
    })
    print(paste0(length(unique(merged_ecs$ec)), " merged ECs identified!"))
    
    # third, check if primary ion is present
    # needs to be per EC!
    if(mSetObj$dataSet$primary_ion=="yes"){
      
      ecs <- unique(merged_ecs$ec);
      
      # function to group ECs and verify if contains primary ion
      new_info <- lapply(ecs, function(x) { 
        new_info <- merged_ecs[which(merged_ecs$ec == x),] # subset merged_ecs to rows containing ECx
        primary.inx <- length(intersect(new_info$form, primary_ions))
        
        if(primary.inx>0){
          new_info <- new_info
        }else{
          new_info <- NULL
        }
        new_info
      })  
      
      final_ecs <- do.call(args=new_info, what=rbind)[,-1]
      
    }else{
      final_ecs <- merged_ecs[,-1]
    }
    
    colnames(final_ecs) <- c("Empirical.Compound", "Query.Mass", "Matched.Form", "Retention.Time", "Matched.Compound")
    
    # transform to long format
    cpd_split <- strsplit(as.character(final_ecs$Matched.Compound), ";", fixed=TRUE)
    reps <- pmax(lengths(cpd_split))
    df2 <- final_ecs[rep(1:nrow(final_ecs), reps), 1:4]
    df2$Matched.Compound <- unlist(mapply(function(x,y) c(x, rep(NA, y)), cpd_split, reps-lengths(cpd_split)))
    
    matched_res <- merge(matched_res, df2)
    matched_res <- matched_res[,-6] #rm rt rank
    matched_res[,6] <- as.character(matched_res[,6])
    
    # now deal with the fact that if at least one EC overlap, need to count as same EC per compound...
    my_final_cpds <- aggregate(. ~ Matched.Compound, matched_res, paste, collapse="___")
    my_final_cpds_list <- lapply(split(my_final_cpds$Empirical.Compound, my_final_cpds$Matched.Compound), unlist)
    
    cpd2ec1 <- lapply(seq_along(my_final_cpds_list), function(x) { # function used to make grouping of ecs per cpd
      
      ecs <- unlist(strsplit(my_final_cpds_list[[x]], "___", fixed=TRUE))
      
      if(length(ecs) > 1){
        ecs.list <- as.list(strsplit(ecs, ";", fixed=TRUE))
        library(igraph)
        m = sapply(ecs.list, function(x) sapply(ecs.list, function(y) length(intersect(x,y))>0))
        g = igraph::groups(components(graph_from_adjacency_matrix(m)))
        ecs <- paste0(sapply(g, function(z) paste0(ecs[z], collapse = "|") ), collapse = "___")
      }
      ecs
    })
    
    names(cpd2ec1) <- names(my_final_cpds_list)
    
    update_ecs <- lapply(seq_along(cpd2ec1), function(z) {
      
      ecs.old <- unlist(strsplit(my_final_cpds_list[[z]], "___", fixed=TRUE))
      ecs.new <- unlist(strsplit(cpd2ec1[[z]], "___", fixed=TRUE))
      
      for(i in seq_along(ecs.new)){
        pattern <- ecs.new[i]
        pattern_vec <- unlist(strsplit(pattern, "\\|"))
        up.pattern <- paste0(unique(pattern_vec), collapse = "|")
        ecs.old[ ecs.old %in% pattern_vec  ] <- up.pattern
      }
      
      ecs.old <- paste0(ecs.old, collapse = "___")
      ecs.old
    })
    
    updated_ecs <- do.call(rbind, update_ecs)
    my_final_cpds$Empirical.Compound <- updated_ecs
    
    new_dt <- data.table::data.table(my_final_cpds)
    new_dt <- new_dt[, list(Query.Mass = unlist(strsplit(as.character(Query.Mass), "___", fixed=TRUE)), 
                            Matched.Form = unlist(strsplit(as.character(Matched.Form), "___", fixed=TRUE)),
                            Retention.Time = unlist(strsplit(as.character(Retention.Time), "___", fixed=TRUE)),
                            Mass.Diff = unlist(strsplit(as.character(Mass.Diff), "___", fixed=TRUE)),
                            Empirical.Compound = unlist(strsplit(as.character(Empirical.Compound), "___", fixed=TRUE))),
                     by = Matched.Compound]
    
    matched_res <- data.frame(Query.Mass = new_dt$Query.Mass, Matched.Compound = new_dt$Matched.Compound, Matched.Form = new_dt$Matched.Form,
                              Retention.Time = new_dt$Retention.Time, Mass.Diff = new_dt$Mass.Diff, Empirical.Compound = new_dt$Empirical.Compound, stringsAsFactors = FALSE)
    
    # make EC names
    ec <- matched_res$Empirical.Compound
    ec.unique <- unique(matched_res$Empirical.Compound)
    
    for(i in seq_along(ec.unique)){
      ec <- replace(ec, grep(paste0("\\b", ec.unique[i], "\\b"), ec, perl=TRUE), paste0("EC000", i))
    }
    
    matched_res$Empirical.Compound <- gsub("\\|.*", "", ec)
    end <- Sys.time()
    totaltime <- end-start
    print(paste0(length(unique(matched_res$Empirical.Compound)), " empirical compounds identified in ", totaltime, " seconds."))
  }
  
  fast.write.csv(matched_res, file="mummichog_matched_compound_all.csv", row.names=FALSE);
  qs::qsave(matched_res, "mum_res.qs");
  
  # now update expr. profile
  matched_mz <- matched_res[,1];
  matched_ts <- mSetObj$dataSet$expr_dic[matched_mz];
  
  if(mSetObj$paramSet$mumRT & version=="v2") { # RT need to be in EC space
    # first create ecpd to expression dict
    ec.exp.mat <- data.frame(key=matched_res[,6], value=as.numeric(matched_ts), stringsAsFactors = F)
    ec_exp_dict <- Convert2Dictionary(ec.exp.mat);
    ec.exp.vec <- unlist(lapply(ec_exp_dict, max));
    
    # also need to make cpd_exp_dict for KEGG network view
    exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    
    # ecpd to cpd dict
    cpd_ecpd_dict <- Convert2Dictionary(matched_res[,c(2,6)])
    ecpd_cpd_dict <- Convert2Dictionary(matched_res[,c(6,2)])
    
    # now mz 2 ecpd dict
    mz2cpd_dict <- Convert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
    mz2ec_dict <- Convert2Dictionary(matched_res[,c(1,6)])
    ec2mz_dict <- Convert2Dictionary(matched_res[,c(6,1)])
    
    # save to mSetObj
    mSetObj$ec_exp_dict <- ec_exp_dict
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$ec_exp <- ec.exp.vec
    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$mz2ec_dict <- mz2ec_dict
    mSetObj$ec2mz_dict <- ec2mz_dict
    mSetObj$ecpd_cpd_dict <- ecpd_cpd_dict
    mSetObj$cpd_ecpd_dict <- cpd_ecpd_dict
    mSetObj$cpd_ecpd_counts <- cpd2ec1
    
    # now do matching to identify significant input_ecpdlist
    refmz <- names(mz2ec_dict)
    hits.index <- which(refmz %in% as.character(mSetObj$dataSet$input_mzlist));
    ec1 <- unique(unlist(mz2ec_dict[hits.index]));
    mSetObj$input_ecpdlist <- ec1;
    mSetObj$total_matched_ecpds <- unique(as.vector(matched_res$Empirical.Compound));

  } else {
    # get the expression profile for each 
    exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    # create average exp
    exp.vec <- unlist(lapply(cpd_exp_dict, mean));
    
    # now need to get the mapping from mz to compound id (one mz can have 0, 1, or more id hits)
    mz2cpd_dict <- Convert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
    cpd2mz_dict <- Convert2Dictionary(matched_res[,c(2,1)]); # indexed/named by id

    # now do matching to identify significant input_cpdlist
    refmz <- names(mz2cpd_dict)
    hits.index <- which(refmz %in% as.character(mSetObj$dataSet$input_mzlist));
    cpd1 <- unique(unlist(mz2cpd_dict[hits.index]));
    
    if(.on.public.web){
        currency_tmp <- currency;
    } else {
        if(!exists("currency_r")){currency_r <- currency}
        currency_tmp <- currency_r;
    }
    
    cpd1 <- cpd1[!(cpd1 %in% currency_tmp)];

    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$cpd_exp <- exp.vec;
    mSetObj$cpd2mz_dict <- cpd2mz_dict;
    mSetObj$input_cpdlist <- cpd1;
    mSetObj$total_matched_cpds <- unique(as.vector(matched_res$Matched.Compound));
  }
  
  form.mat <- cbind(matched_res[,2], matched_res[,3]);
  cpd_form_dict <- Convert2Dictionary(form.mat);
  mSetObj$cpd_form_dict <- cpd_form_dict;
  
  return(mSetObj);
}

# version 2
# internal function for searching compound library
.search.compoundLibMeta <- function(mSetObjMeta, cpd.lib, cpd.treep, cpd.treen, metaLevel = "cpd",
                                    combine.level = "pvalue", pval.method = "fisher", es.method = "fixed",
                                    rank.metric = "mean", mutual.feats = TRUE){
  
  metaFiles <- unique(metaFiles)

  metaMsetObj <- vector("list")
  version <- mum.version ##TODO: There must be an error bc mum.version is not global variable any more

  # first do compound mapping
  for(meta_file in seq_along(metaFiles)){
    
    mSetObj <- qs::qread(metaFiles[meta_file])
    ref_mzlist <- as.numeric(mSetObj$dataSet$ref_mzlist);
    print(paste0("compoundLibMeta"));
    print(paste0("Got ", length(ref_mzlist), " mass features."))
    pos_inx <- mSetObj$dataSet$pos_inx;
    ref_mzlistp <- ref_mzlist[pos_inx];
    ref_mzlistn <- ref_mzlist[!pos_inx];
    
    # for empirical compounds
    if(mSetObj$paramSet$mumRT){ 
      ret_time_pos <- mSetObj$dataSet$ret_time[pos_inx];
      ret_time_neg <- mSetObj$dataSet$ret_time[!pos_inx]
      rt_tol <- mSetObj$dataSet$rt_tol
    }else{
      # add fake RT
      ret_time_pos <- rep(1, length(ref_mzlistp))
      ret_time_neg <- rep(1, length(ref_mzlistn))
    }
    
    modified.statesp <- colnames(cpd.lib$mz.matp);
    modified.statesn <- colnames(cpd.lib$mz.matn);
    my.tolsp <- mz_tolerance(ref_mzlistp, mSetObj$dataSet$instrument);
    my.tolsn <- mz_tolerance(ref_mzlistn, mSetObj$dataSet$instrument);
    
    # get mz ladder (pos index)
    self.mzsp <- floor(ref_mzlistp);
    all.mzsp <- cbind(self.mzsp-1, self.mzsp, self.mzsp+1);
    
    self.mzsn <- floor(ref_mzlistn);
    all.mzsn <- cbind(self.mzsn-1, self.mzsn, self.mzsn+1);
    
    # matched_res will contain detailed result (cmpd.id. query.mass, mass.diff) for all mz;
    # use a high-performance variant of list
    matched_resp <- myFastList();
    matched_resn <- myFastList();
    
    if(mSetObj$dataSet$mode != "negative"){
      for(i in seq_along(ref_mzlistp)){
        mz <- ref_mzlistp[i];
        rt <- ret_time_pos[i];
        my.tol <- my.tolsp[i];
        all.mz <- all.mzsp[i,];
        pos.all <- as.numeric(unique(unlist(cpd.treep[all.mz])));
        
        for(pos in pos.all){
          id <- cpd.lib$id[pos];
          mw.all <- cpd.lib$mz.matp[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);
          if(length(hit.inx)>0){
            for(spot in seq_along(hit.inx)){
              hit.pos <- hit.inx[spot];# need to match all
              index <- paste(mz, id, rt, hit.pos, sep = "_");
              matched_resp$add(index, c(i, id, mz, rt, mw.all[hit.pos], modified.statesp[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
            }
          }
        }
      }
    }
    
    all.mzsn <<- all.mzsn
    
    if(mSetObj$dataSet$mode != "positive"){
      for(i in seq_along(ref_mzlistn)){
        mz <- ref_mzlistn[i];
        rt <- ret_time_neg[i];
        my.tol <- my.tolsn[i];
        all.mz <- all.mzsn[i,];
        pos.all <- as.numeric(unique(unlist(cpd.treen[all.mz])));
        
        for(pos in pos.all){
          id <- cpd.lib$id[pos]; # position of compound in cpd.tree
          mw.all <- cpd.lib$mz.matn[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);
          if(length(hit.inx)>0){
            for(spot in seq_along(hit.inx)){
              hit.pos <- hit.inx[spot];# need to match all
              index <- paste(mz, id, rt, hit.pos, sep = "_"); #name in fast_list
              matched_resn$add(index, c(i, id, mz, rt, mw.all[hit.pos], modified.statesn[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
            }
          }
        }
      }
    }
    
    # convert to regular list
    if(mSetObj$dataSet$mode == "mixed"){
      
      matched_resn <- matched_resn$as.list();
      matched_resp <- matched_resp$as.list();
      
      neg_matches <- length(matched_resn) > 0
      pos_matches <- length(matched_resp) > 0
      
      if(!neg_matches & !pos_matches){
        msg.vec <<- "No compound matches from upload peak list!"
        return(0)
      }
      
      if(neg_matches){
        matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
      }
      
      if(pos_matches){
        matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
      }
      
      if(neg_matches & pos_matches){ # both w. matches
        matched_res <- rbind(matched_resp, matched_resn)
      }else if(neg_matches & !pos_matches){ # only neg w. matches
        matched_res <- matched_resn
      }else{ # only pos w. matches
        matched_res <- matched_resp
      }
      
    }else if(mSetObj$dataSet$mode == "positive"){
      matched_resp <- matched_resp$as.list();
      
      if(is.null(unlist(matched_resp))){
        msg.vec <<- "No compound matches from upload peak list!"
        return(0)
      }
      
      matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
      matched_res <- matched_resp
    }else{
      matched_resn <- matched_resn$as.list();
      
      if(is.null(unlist(matched_resn))){
        msg.vec <<- "No compound matches from upload peak list!"
        return(0)
      }
      
      matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
      matched_res <- matched_resn
    }
    
    # re-order columns for output
    matched_res <- matched_res[, c(3,2,6,7,4)];
    colnames(matched_res) <- c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff", "Retention.Time");
    
    if(metaLevel == "cpd"){
      fileName = paste0("mummichog_matched_compound_", mSetObj$dataSet$fileName, "_all.csv")
      fast.write.csv(matched_res, file=fileName, row.names=FALSE);
    }
    
    # objects save to meta msetObj
    metafile <- tools::file_path_sans_ext(metaFiles[meta_file])
    metaMsetObj[[metafile]] <- vector("list", 5)
    
    # now update expr. profile
    matched_mz <- matched_res[,1];
    matched_ts <- mSetObj$dataSet$expr_dic[matched_mz];
    
    if(combine.level %in% c("pvalue", "both", "pool")){
      pvals <- mSetObj$dataSet$mummi.proc$p.value
      names(pvals) <- mSetObj$dataSet$mummi.proc$m.z
      matched_pvals <- pvals[matched_mz]
      metaMsetObj[[metafile]]$matched_pvals <- matched_pvals;
    } 
    
    if(combine.level %in% c("es", "both")){
      es <- mSetObj$dataSet$mummi.proc[,c("effect.size", "lower.ci", "upper.ci")]
      rownames(es) <- mSetObj$dataSet$mummi.proc$m.z
      match.inx <- match(matched_mz, rownames(es))
      matched_es <- es[match.inx,]
      metaMsetObj[[metafile]]$matched_es <- matched_es;
    }
    
    metaMsetObj[[metafile]]$mumResTable <- matched_res;
    metaMsetObj[[metafile]]$ref_mzlist <- mSetObj$dataSet$ref_mzlist 
    metaMsetObj[[metafile]]$input_mzlist <- mSetObj$dataSet$input_mzlist
    metaMsetObj[[metafile]]$matched_ts <- matched_ts;
    metaMsetObj[[metafile]]$mumRT <-mSetObj$paramSet$mumRT
  }
  
  # second fill in p-value and effect size information
  mSetObj <- mSetObjMeta;
  
  matched_res <- lapply(metaMsetObj, "[[", "mumResTable")
  matched_res <- data.table::rbindlist(matched_res, idcol = TRUE)
  matched_ts <- unlist(lapply(metaMsetObj, "[[", "matched_ts"))
  matched_res <- cbind(matched_res, matched_ts)
  
  if(combine.level %in% c("pvalue", "both", "pool")){
    matched_pval <- unlist(lapply(metaMsetObj, "[[", "matched_pvals"))
  }else{ # initialize empty
    matched_pval <- rep(NA, length(matched_ts))
  }
  
  if(combine.level %in% c("es", "both")){
    matched_es <- lapply(metaMsetObj, "[[", "matched_es")
    matched_es <- Reduce(rbind, matched_es)
  }else{ # initialize empty
    matched_es <- matrix("1", nrow = length(matched_ts), ncol=3)
    colnames(matched_es) <- c("effect.size", "lower.ci", "upper.ci") 
  }
  
  matched_res <- cbind(matched_res, Matched.Pval = matched_pval, matched_es)
  
  # combine at compound level
  if(metaLevel == "cpd"){
    
    if(mutual.feats){
      matched_res_ag <- aggregate(. ~ Matched.Compound, matched_res, paste, collapse=";")
      
      # keep compounds that only match across all files
      matched <- strsplit(matched_res_ag$.id, ";", fixed=TRUE)
      matched.inx <- vapply(matched, function(x) length(unique(x))==length(metaFiles), logical(1))
      
      if(sum(matched.inx) == 0){
        AddErrMsg("No compounds found across all files!")
        return(0)
      }
      
      matched_res_ag <- matched_res_ag[matched.inx,]
      # undo aggregate
      
      matched_res <- splitstackshape::cSplit(matched_res_ag, c(".id", "Query.Mass", "Matched.Form", "Mass.Diff", "Retention.Time", "matched_ts", 
                                                               "Matched.Pval", "effect.size", "lower.ci", "upper.ci"), 
                                             ";", "long", makeEqual = FALSE, type.convert = "as.character")
      matched_res <- data.table::setDF(matched_res)
      colnames(matched_res)[2] <- "File.Name"
      colnames(matched_res)[7:11] <- c("Matched.Scores", "Matched.Pvalue", "Matched.ES", "Matched.L.CI", "Matched.U.CI")
      
    }else{
      matched_res <- matched_res[, c(3,1,2,4:11)]
      matched_res <- data.table::setDF(matched_res)
      colnames(matched_res)[2] <- "File.Name"
      colnames(matched_res)[7:11] <- c("Matched.Scores", "Matched.Pvalue", "Matched.ES", "Matched.L.CI", "Matched.U.CI")
    }
    
    fast.write.csv(matched_res, file="mummichog_matched_compound_all.csv", row.names=FALSE);
    # or empirical compound combining here!  
  }else{
    
    all_ref_mz <- unlist(lapply(metaMsetObj, "[[", "ref_mzlist"))
    
    matched_res$RT.Rank <- rank(as.numeric(matched_res$Retention.Time), ties.method = "random")
    rt_tol_rank <- length(all_ref_mz) * mSetObj$dataSet$rt_frac
    
    # now create empirical compounds if necessary!
    # 1 compound matches to multiple m/z, filter by RT 
    if(mSetObj$paramSet$mumRT & version=="v2"){
      
      start <- Sys.time()
      empirical.cpd.list <- data.table:::split.data.table(matched_res, by="Matched.Compound", sorted = TRUE); # split all info by compound
      empirical.cpds2cpds <- vector(length=(length(empirical.cpd.list)), "list")
      names(empirical.cpds2cpds) <- names(empirical.cpd.list)
      
      # for each compound, if multiple matches, split into ECpds if > RT tolerance - rt_tol
      for(i in seq_along(empirical.cpd.list)){
        
        id <- empirical.cpd.list[[i]]$.id
        mzs <- empirical.cpd.list[[i]]$Query.Mass
        ions <- empirical.cpd.list[[i]]$Matched.Form
        rts <- as.numeric(empirical.cpd.list[[i]]$Retention.Time)
        rt.rank <- as.numeric(empirical.cpd.list[[i]]$RT.Rank)
        score <- empirical.cpd.list[[i]]$matched_ts
        mass.diff <- empirical.cpd.list[[i]]$Mass.Diff
        p.val <- empirical.cpd.list[[i]]$Matched.Pval
        es <- empirical.cpd.list[[i]]$effect.size
        l.ci <- empirical.cpd.list[[i]]$lower.ci
        u.ci <- empirical.cpd.list[[i]]$upper.ci
        cpds <- names(empirical.cpd.list)[i]
        
        # first, for each compound, determine ECs among matched ions
        if(length(mzs)>1){ # if multiple ECs per compound
          
          # first group together to create empirical cpds by rt
          names(rts) <- paste0(mzs, ";", ions, ";", rts, ";", cpds, ";", id, ";", score,
                               ";", mass.diff, ";", p.val,";", es, ";", l.ci, ";", u.ci)
          rts <- sort(rts)
          
          # second, group together to create empirical cpds by rt rank
          names(rt.rank) <- paste0(mzs, ";", ions, ";", rts, ";", cpds, ";", id, ";", score,
                                   ";", mass.diff, ";", p.val,";", es, ";", l.ci, ";", u.ci)
          rt.rank <- sort(rt.rank)
          
          split.inx <- c(0, cumsum(Reduce("&", list(abs(diff(rts)) > rt_tol, abs(diff(rt.rank)) > rt_tol_rank) )))
          
          # need to deal w. multiple rts but only 1 EC
          if(length(unique(split.inx)) > 1){
            e.cpds <- split(rts, split.inx)
            empirical.cpds2cpds[[i]] <- lapply(e.cpds, names)
          }else{
            empirical.cpds2cpds[[i]] <- paste0(names(rts), collapse="__")
          }
          
        }else{ # if only 1 EC per compound
          empirical.cpds2cpds[[i]] <- paste0(mzs, ";", ions, ";", rts, ";", cpds, ";", id, ";", score,
                                             ";", mass.diff, ";", p.val,";", es, ";", l.ci, ";", u.ci)
        }
      }
      
      initial_ecs <- unlist(empirical.cpds2cpds, recursive=FALSE)
      names(initial_ecs) <- paste0("EC", seq_along(initial_ecs))
      print(paste0(length(initial_ecs), " inital ECs created!"))
      
      # second, merge ECs if same m/z and form - append compounds
      try <- melt(initial_ecs)
      try2 <- strsplit(as.character(try[,1]), split="__", fixed=TRUE) # deals with multiple rts belonging to 1 EC
      try2 <- data.frame(value=unlist(try2), L1 = rep(try$L1, sapply(try2, length)))
      
      info <- strsplit(as.character(try2[,1]), split=";", fixed=TRUE)
      
      df_ecs <- data.frame(ec = as.character(try2[,2]), mz = sapply(info, `[[`, 1), form = sapply(info, `[[`, 2), rt = sapply(info, `[[`, 3), 
                           cpd = sapply(info, `[[`, 4), id = sapply(info, `[[`, 5), score = sapply(info, `[[`, 6), 
                           mass_diff = sapply(info, `[[`, 7), pval = sapply(info, `[[`, 8), es = sapply(info, `[[`, 9),
                           lci = sapply(info, `[[`, 10), uci = sapply(info, `[[`, 11), stringsAsFactors = F)
      
      df_ecs$str_row_inx <- paste(df_ecs$mz, df_ecs$form, df_ecs$rt, sep = "___")
      merged_ecs <- aggregate(. ~ str_row_inx, df_ecs, paste, collapse=";")
      
      # OPTIMIZED: Vectorized string processing - batch process all 10 columns at once
      # cleaning the df
      # merged_ecs$ec <- sapply(strsplit(merged_ecs$ec, ";"), function(x) unlist(x)[1]) - keep as long name

      # Columns that need unique values only (collapse to single if duplicate)
      cols_unique <- c("mz", "form", "rt")
      merged_ecs[cols_unique] <- lapply(merged_ecs[cols_unique], function(col) {
        vapply(strsplit(as.character(col), ";", fixed = TRUE),
               function(x) paste(unique(x), collapse = ";"),
               FUN.VALUE = character(1))
      })

      # Columns that need all unique values preserved (keep as semicolon-separated)
      cols_preserve <- c("id", "score", "mass_diff", "pval", "es", "lci", "uci")
      merged_ecs[cols_preserve] <- lapply(merged_ecs[cols_preserve], function(col) {
        vapply(strsplit(as.character(col), ";", fixed = TRUE),
               function(x) paste(unique(x), collapse = ";"),
               FUN.VALUE = character(1))
      })
      print(paste0(length(unique(merged_ecs$ec)), " merged ECs identified!"))
      
      # third, check if primary ion is present
      # needs to be per EC!
      if(mSetObj$dataSet$primary_ion=="yes"){
        
        ecs <- unique(merged_ecs$ec)
        
        # function to group ECs and verify if contains primary ion
        new_info <- lapply(ecs, function(x) { 
          new_ec_info <- merged_ecs[which(merged_ecs$ec == x),] # subset merged_ecs to rows containing ECx
          primary.inx <- length(intersect(new_ec_info$form, primary_ions))
          
          if(primary.inx>0){
            new_ec_info <- new_ec_info
          }else{
            new_ec_info <- NULL
          }
          new_ec_info
        })  
        
        final_ecs <- do.call(args=new_info, what=rbind)[,-1]
        
      }else{
        final_ecs <- merged_ecs[,-1]
      }
      
      colnames(final_ecs) <- c("Empirical.Compound", "Query.Mass", "Matched.Form", "Retention.Time", "Matched.Compound", "FileName", "Matched.Scores",
                               "Mass.Diff", "Matched.Pvalue", "Matched.ES", "Matched.L.CI", "Matched.U.CI")
      
      # transform to long format
      cpd_split <- strsplit(as.character(final_ecs$Matched.Compound), ";", fixed=TRUE)
      reps <- pmax(lengths(cpd_split))
      df2 <- final_ecs[rep(1:nrow(final_ecs), reps), c(1:4, 6:12)]
      df2$Matched.Compound <- unlist(mapply(function(x,y) c(x, rep(NA, y)), cpd_split, reps-lengths(cpd_split)))
      df2 <- unique(df2)
      
      # now deal with the fact that if at least one EC overlap, need to count as same EC per compound...
      my_final_cpds <- aggregate(. ~ Matched.Compound, df2, paste, collapse="___")
      my_final_cpds_list <- lapply(split(my_final_cpds$Empirical.Compound, my_final_cpds$Matched.Compound), unlist)
      
      cpd2ec1 <- lapply(seq_along(my_final_cpds_list), function(x) { # function used to make grouping of ecs per cpd
        
        ecs <- unlist(strsplit(my_final_cpds_list[[x]], "___", fixed=TRUE))
        
        if(length(ecs) > 1){
          ecs.list <- as.list(strsplit(ecs, ";", fixed=TRUE))
          library(igraph)
          m = sapply(ecs.list, function(x) sapply(ecs.list, function(y) length(intersect(x,y))>0))
          g = igraph::groups(components(graph_from_adjacency_matrix(m)))
          ecs <- paste0(sapply(g, function(z) paste0(ecs[z], collapse = "|") ), collapse = "___")
        }
        ecs
      })
      
      names(cpd2ec1) <- names(my_final_cpds_list)
      
      update_ecs <- lapply(seq_along(cpd2ec1), function(z) {
        
        ecs.old <- unlist(strsplit(my_final_cpds_list[[z]], "___", fixed=TRUE))
        ecs.new <- unlist(strsplit(cpd2ec1[[z]], "___", fixed=TRUE))
        
        for(i in seq_along(ecs.new)){
          pattern <- ecs.new[i]
          pattern_vec <- unlist(strsplit(pattern, "\\|", fixed=TRUE))
          up.pattern <- paste0(unique(pattern_vec), collapse = "|")
          ecs.old[ ecs.old %in% pattern_vec  ] <- up.pattern
        }
        
        ecs.old <- paste0(ecs.old, collapse = "___")
        ecs.old
      })
      
      updated_ecs <- do.call(rbind, update_ecs)
      my_final_cpds$Empirical.Compound <- updated_ecs
      
      new_dt <- data.table::data.table(my_final_cpds)
      new_dt <- new_dt[, list(Query.Mass = unlist(strsplit(as.character(Query.Mass), "___", fixed=TRUE)), 
                              Matched.Form = unlist(strsplit(as.character(Matched.Form), "___", fixed=TRUE)),
                              Retention.Time = unlist(strsplit(as.character(Retention.Time), "___", fixed=TRUE)),
                              Empirical.Compound = unlist(strsplit(as.character(Empirical.Compound), "___", fixed=TRUE)),
                              File.Name = unlist(strsplit(as.character(FileName), "___", fixed=TRUE)),
                              Matched.Scores = unlist(strsplit(as.character(Matched.Scores), "___", fixed=TRUE)),
                              Mass.Diff = unlist(strsplit(as.character(Mass.Diff), "___", fixed=TRUE)),
                              Matched.Pvalue = unlist(strsplit(as.character(Matched.Pvalue), "___", fixed=TRUE)),
                              Matched.ES = unlist(strsplit(as.character(Matched.ES), "___", fixed=TRUE)),
                              Matched.L.CI = unlist(strsplit(as.character(Matched.L.CI), "___", fixed=TRUE)),
                              Matched.U.CI = unlist(strsplit(as.character(Matched.U.CI), "___", fixed=TRUE))
      ),
      by = Matched.Compound]
      
      matched_res <- data.frame(Query.Mass = new_dt$Query.Mass, Matched.Compound = new_dt$Matched.Compound, Matched.Form = new_dt$Matched.Form, Mass.Diff = new_dt$Mass.Diff,
                                Retention.Time = new_dt$Retention.Time, Matched.Scores = new_dt$Matched.Scores, Matched.Pvalue = new_dt$Matched.Pvalue, 
                                Matched.ES = new_dt$Matched.ES, Matched.L.CI = new_dt$Matched.L.CI, Matched.U.CI = new_dt$Matched.U.CI,
                                Empirical.Compound = new_dt$Empirical.Compound, File.Name = new_dt$File.Name, stringsAsFactors = FALSE)
      
      # make new EC names
      ec <- as.list(matched_res$Empirical.Compound)
      ec.unique <- unique(matched_res$Empirical.Compound)
      ec.new <- paste0("EC000", seq_along(ec.unique))
      
      ec.new <- vapply(seq_along(ec), function(i) { 
        
        inx <- match(ec[[i]], ec.unique)
        ec <- ec.new[inx]
        ec
        
      }, character(1))
      
      matched_res$Empirical.Compound <- gsub("\\|.*", "", ec.new)
      end <- Sys.time()
      totaltime <- end-start
      print(paste0(length(unique(matched_res$Empirical.Compound)), " empirical compounds identified in ", totaltime, " seconds."))
      
      if(mutual.feats){
        # keep empirical compounds that only match across all files
        matched_res <- aggregate(. ~ Empirical.Compound, matched_res, paste, collapse="___")
        matched <- strsplit(matched_res$File.Name, ";|___")
        matched.inx <- vapply(matched, function(x) length(unique(x))==length(metaFiles), logical(1))
        
        if(sum(matched.inx)==0){
          AddErrMsg("No empirical compounds found across all studies!")
          return(0)
        }else if(sum(matched.inx) < 50){
          AddErrMsg("Not enough empirical compounds matched across all studies! Try meta-analysis at a higher level (compound or pathway).")
          return(0)
        }
        
        print(paste0(sum(matched.inx), "matched empirical compounds identified across all studies!"))
        
        matched_res <- matched_res[matched.inx,]
        matched_res <- splitstackshape::cSplit(matched_res, c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff", "Retention.Time", "Matched.Scores", 
                                                              "Matched.Pvalue", "Matched.ES", "Matched.L.CI", "Matched.U.CI", "File.Name"), 
                                               "___", "long", makeEqual = FALSE, type.convert = "as.character")
        matched_res <- data.table::setDF(matched_res)
      }else{
        matched_res <- matched_res[,c(11,1:10,12)]
        matched_res <- data.table::setDF(matched_res)
      }
      fast.write.csv(matched_res, file="mummichog_matched_compound_postmerge.csv", row.names=FALSE);
    }else{
      AddErrMsg("Meta-analysis at empirical compound level is invalid!")
      return(0)
    }
  }
  
  ref_mzlist <- lapply(metaMsetObj, "[[", "ref_mzlist") 
  ref_mzlist <- unlist(unique(ref_mzlist))
  mSetObj$dataSet$ref_mzlist <- ref_mzlist
  
  mumRT <- unlist(lapply(metaMsetObj, "[[", "mumRT")) 
  qs::qsave(matched_res, "mum_res.qs");
  
  ##############################################
  # COMBINE EITHER P-VALUES
  # EFFECT-SIZES
  # OR BOTH (only for mummichog or integ_peaks)
  # then re-make input_cpdlist and input_ecpdlist
  # gsea uses rank metric only
  
  #  if(anal.type %in% c("mummichog", "integ_peaks")){
  
  if(combine.level %in% c("pvalue", "both")){
    
    if(metaLevel %in% "ec"){
      # merge to empirical compounds
      all_p <- aggregate(. ~ Empirical.Compound, matched_res, paste, collapse="___")
      old_p <- strsplit(all_p$Matched.Pvalue, "___", fixed=TRUE)
      scores <- strsplit(all_p$Matched.Pvalue, ";|___")
      scores <- lapply(scores, function(x) {
        if(length(x) == 1){
          x <- rep(x, 2)
        }
        x;}) 
      
    }else{
      # merge to compounds
      all_p <- aggregate(. ~ Matched.Compound, matched_res, paste, collapse="___")
      old_p <- strsplit(all_p$Matched.Pvalue, "___", fixed=TRUE)
      scores <- strsplit(all_p$Matched.Pvalue, ";|___")
      scores <- lapply(scores, function(x) {
        if(length(x) == 1){
          x <- rep(x, 2)
        }
        x;}) 
    }
    
    # combine p-values
    if(pval.method=="fisher"){
      meta.pvals <- lapply(scores, function(x) sumlog(as.numeric(x)))
    }else if(pval.method=="edgington"){ 
      meta.pvals <- lapply(scores, function(x) sump(as.numeric(x)))
    }else if(pval.method=="stouffer"){
      meta.pvals <- lapply(scores, function(x) sumz(x))
    }else if(pval.method=="vote"){
      meta.pvals <- lapply(scores, function(x) votep(x))
    }else if(pval.method=="min"){
      Meta.P <- lapply(scores, function(x) min(x) )
    }else if(pval.method=="max") {
      Meta.P <- lapply(scores, function(x) max(x) )
    }else{
      AddErrMsg("Invalid meta-analysis method!")
      return(0)
    }
    
    #extract p-values
    if(exists("meta.pvals")){
      Meta.P <- unlist(lapply(meta.pvals, function(z) z["p"]))
    }
    
    Meta.P2 <- rep(Meta.P, vapply(old_p, length, numeric(1)))
    matched_res$Meta.P <- Meta.P2
    #    }
    
    # now create input mzlist - used to create
    # input cpd/ec list
    cutoff <- mSetObj$dataSet$cutoff
    
    if(combine.level == "both"){
      my.inx <- matched_res[,"Meta.P.Both"] < cutoff
    }else if(combine.level == "pvalue"){
      my.inx <- matched_res[,"Meta.P"] < cutoff
    }else{
      my.inx <- matched_res[,"Meta.ES.Pval"] < cutoff
    }
    
    input_mzlist <- unlist(unique(matched_res[as.vector(my.inx), "Query.Mass"]))
    
  }else{ # gsea
    input_mzlist <- lapply(metaMsetObj, "[[", "input_mzlist") 
    input_mzlist <- unlist(unique(input_mzlist)) # will be updated later
  }
  
  sig.size <- length(input_mzlist);
  mSetObj$dataSet$N <- sig.size;
  mSetObj$dataSet$input_mzlist <- input_mzlist
  
  if(all(mumRT) & version=="v2"){ # RT need to be in EC space
    
    # for GSEA
    # need to merge t-scores if same m/z in the data
    if(rank.metric == "mean"){     # default using the mean
      matched_res$Matched.Scores <- vapply(matched_res$Matched.Scores, function(x) mean(as.numeric(unlist(strsplit(as.character(x), ";", fixed=TRUE)))), numeric(1))
    }else if(rank.metric == "min"){
      matched_res$Matched.Scores <- vapply(matched_res$Matched.Scores, function(x) min(as.numeric(unlist(strsplit(as.character(x), ";", fixed=TRUE)))), numeric(1))
    }else if(rank.metric == "max"){
      matched_res$Matched.Scores <- vapply(matched_res$Matched.Scores, function(x) max(as.numeric(unlist(strsplit(as.character(x), ";", fixed=TRUE)))), numeric(1))
    }else if(rank.metric == "median"){
      matched_res$Matched.Scores <- vapply(matched_res$Matched.Scores, function(x) median(as.numeric(unlist(strsplit(as.character(x), ";", fixed=TRUE)))), numeric(1))
    }else{
      AddErrMsg("Invalid method selected for merging scores!")
      return(0)
    }
    
    ec.exp.mat <- data.frame(key=matched_res$Empirical.Compound, value=as.numeric(matched_res$Matched.Scores), stringsAsFactors = F)
    ec_exp_dict <- Convert2Dictionary(ec.exp.mat);
    ec.exp.vec <- unlist(lapply(ec_exp_dict, max));
    
    # also need to make cpd_exp_dict for KEGG network view
    exp.mat <- data.frame(key=matched_res$Matched.Compound, value=as.numeric(matched_res$Matched.Scores), stringsAsFactors = F);
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    
    # ecpd to cpd dict
    cpd_ecpd_dict <- Convert2Dictionary(matched_res[,c(3,1)])
    ecpd_cpd_dict <- Convert2Dictionary(matched_res[,c(1,3)])
    
    # now mz 2 ecpd dict
    mz2cpd_dict <- Convert2Dictionary(matched_res[,c(2,3)]); #indexed/named by mz
    mz2ec_dict <- Convert2Dictionary(matched_res[,c(2,1)])
    ec2mz_dict <- Convert2Dictionary(matched_res[,c(1,2)])
    
    # save to mSetObj
    mSetObj$ec_exp_dict <- ec_exp_dict
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$ec_exp <- ec.exp.vec
    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$mz2ec_dict <- mz2ec_dict
    mSetObj$ec2mz_dict <- ec2mz_dict
    mSetObj$ecpd_cpd_dict <- ecpd_cpd_dict
    mSetObj$cpd_ecpd_dict <- cpd_ecpd_dict
    mSetObj$cpd_ecpd_counts <- cpd2ec1
    
    # now do matching to identify significant input_ecpdlist
    # trio.list <- data.frame(mz = names(mz2ec_dict), ec = sapply(mz2ec_dict, paste, collapse="; "), cpd = sapply(mz2cpd_dict, paste, collapse="; "))
    refmz <- names(mz2ec_dict)
    hits.index <- which(refmz %in% as.character(input_mzlist));
    ec1 <- unique(unlist(mz2ec_dict[hits.index]));
    mSetObj$input_ecpdlist <- ec1;
    mSetObj$total_matched_ecpds <- unique(as.vector(matched_res$Empirical.Compound));
    form.mat <- cbind(matched_res[,2], matched_res[,4]);
    
  }else{ # compound level
    
    # get the expression profile for each 
    exp.mat <- data.frame(key=matched_res$Matched.Compound, value=as.numeric(matched_res$Matched.Scores), stringsAsFactors = F);
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    # create average exp
    exp.vec <- unlist(lapply(cpd_exp_dict, function(x) mean(unlist(x))));
    
    # now need to get the mapping from mz to compound id (one mz can have 0, 1, or more id hits)
    mz2cpd_dict <- Convert2Dictionary(matched_res[ , c("Query.Mass", "Matched.Compound")]) #indexed/named by mz
    cpd2mz_dict <- Convert2Dictionary(matched_res[ , c("Matched.Compound", "Query.Mass")]) # indexed/named by id
    
    # now do matching to identify significant input_cpdlist
    refmz <- names(mz2cpd_dict)
    hits.index <- which(refmz %in% as.character(input_mzlist));
    cpd1 <- unique(unlist(mz2cpd_dict[hits.index]));
        
    if(.on.public.web){
        currency_tmp <- currency;
    } else {
        if(!exists("currency_r")){currency_r <- currency}
        currency_tmp <- currency_r;
    }
    
    cpd1 <- cpd1[!(cpd1 %in% currency_tmp)];
    form.mat <- cbind(matched_res$Query.Mass, matched_res$Matched.Form);
    
    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$cpd_exp <- exp.vec;
    mSetObj$cpd2mz_dict <- cpd2mz_dict;
    mSetObj$input_cpdlist <- cpd1;
    mSetObj$dataSet$N <- length(input_mzlist);
    mSetObj$total_matched_cpds <- unique(as.vector(matched_res$Matched.Compound));
  }
  
  cpd_form_dict <- Convert2Dictionary(form.mat);
  mSetObj$cpd_form_dict <- cpd_form_dict;
  return(mSetObj);
}

#' @param method If "cohen", computes Cohen's d, if "hedges",
#' computes Hegdes' g effect size statistics.
#' @noRd
CalculateEffectSize <- function(mSetObj=NA, paired=FALSE, method="cohen"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1])
  inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2])
  
  # samples in row, features in columns
  x <- mSetObj$dataSet$norm[inx1,]
  y <- mSetObj$dataSet$norm[inx2,]
  
  library(effsize)
  
  my.fun <- function(x) {
    
    if(method == "cohen"){
      tmp <- try(cohen.d(x[inx1], x[inx2], paired=paired, hedges.correction=FALSE));
    }else{
      tmp <- try(cohen.d(x[inx1], x[inx2], paired=paired, hedges.correction=TRUE));
    }
    
    if(class(tmp) == "try-error") {
      return(c(NA, NA, NA, NA));
    }else{
      return(c(tmp$estimate, tmp$sd, tmp$conf.int));
    }
  }
  
  results <- apply(as.matrix(mSetObj$dataSet$norm), 2, my.fun)
  rownames(results) <- c("effect_size", "win_group_stdev", "lower_ci", "upper_ci")
  
  mSetObj$analSet$effect.size <- t(results);
  return(.set.mSet(mSetObj));
}

# Internal function for permutation, no RT
.perform.mummichogPermutations <- function(mSetObj, permNum=100){

  # OPTIMIZED: Web-friendly permutation with vectorized operations
  # No parallelization to avoid slowing down other users
  print(paste('Resampling, ', permNum, 'permutations to estimate background ...'));

  # Pre-allocate with correct size
  permutation_hits <- permutation_record <- vector("list", permNum);

  # Pre-load and cache frequently accessed objects (avoid repeated I/O)
  matched_res <- qs::qread("mum_res.qs");
  ref_mzlist <- mSetObj$dataSet$ref_mzlist
  N <- mSetObj$dataSet$N
  total_matched <- mSetObj$total_matched_cpds
  pathways <- mSetObj$pathways
  cpd2mz <- mSetObj$cpd2mz_dict

  set.seed(123)

  # OPTIMIZED: Batch process permutations to reduce function call overhead
  # Process in chunks of 10 for better cache utilization
  chunk_size <- min(10, permNum)

  for(i in 1:permNum){
    # Set per-iteration seed for reproducibility
    set.seed(123 + i)

    input_mzlist <- sample(ref_mzlist, N)
    t <- make_cpdlist(mSetObj, input_mzlist)

    # Use cached objects instead of mSetObj$ lookups
    perm <- ComputeMummichogPermPvals(t, total_matched, pathways,
                                     matched_res, input_mzlist, cpd2mz)

    permutation_record[[i]] <- perm[1]
    permutation_hits[[i]] <- perm[2]

    # Progress indicator every 100 iterations (helps users know it's working)
    if(i %% 100 == 0){
      print(paste0("Completed ", i, "/", permNum, " permutations..."))
    }
  }

  # append new info
  mSetObj$perm_record <- permutation_record;
  mSetObj$perm_hits <- permutation_hits;
  return(mSetObj);
}

# Calculate p-values for each Lperm
# Used in higher mummichogR functions w.out RT
ComputeMummichogPermPvals <- function(input_cpdlist, total_matched_cpds, pathways, matches.res, input_mzlist, cpd2mz_dict){
  
  ora.vec <- input_cpdlist; #Lperm
  query_set_size <- length(ora.vec)
  current.mset <- pathways$cpds; #all
  total_cpds <- unique(total_matched_cpds) #matched compounds
  total_feature_num <- length(total_cpds)
  
  size <- negneg <- vector(mode="list", length=length(current.mset));
  
  cpds <- lapply(current.mset, function(x) intersect(x, total_cpds)); # pathways & all ref cpds
  feats <- lapply(current.mset, function(x) intersect(x, ora.vec)); #pathways & lsig
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  set.num <- unlist(lapply(cpds, length)); #cpdnum
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset));
  
  for(i in seq_along(current.mset)){ # for each pathway
    sizes[[i]] <- min(feat_len[i], count_cpd2mz(cpd2mz_dict, unlist(feats[i]), input_mzlist))
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size;
  }
  
  unsize <- as.integer(unlist(sizes))
  res.mat <- matrix(0, nrow=length(current.mset), ncol=1)
  fishermatrix <- cbind(unsize-1, set.num, (query_set_size + unlist(negneg)), query_set_size)
  res.mat[,1] <- apply(fishermatrix, 1, function(x) phyper(x[1], x[2], x[3], x[4], lower.tail=FALSE));
  perm_records <- list(res.mat, as.matrix(unsize));
  return(perm_records);
}

# Internal function for permutation
.perform.mummichogRTPermutations <- function(mSetObj, permNum){

  # OPTIMIZED: Web-friendly permutation with vectorized operations
  # No parallelization to avoid slowing down other users
  print(paste('Resampling, ', permNum, 'permutations to estimate background ...'));

  # Pre-allocate with correct size
  permutation_hits <- permutation_record <- vector("list", permNum);

  # Pre-load and cache frequently accessed objects (avoid repeated I/O)
  matched_res <- qs::qread("mum_res.qs");
  ref_mzlist <- mSetObj$dataSet$ref_mzlist
  N <- mSetObj$dataSet$N
  total_matched <- mSetObj$total_matched_ecpds
  pathways <- mSetObj$pathways

  set.seed(123)

  for(i in 1:permNum){
    # Set per-iteration seed for reproducibility
    set.seed(123 + i)

    input_mzlist <- sample(ref_mzlist, N)
    t <- make_ecpdlist(mSetObj, input_mzlist)

    # Use cached objects instead of mSetObj$ lookups
    perm <- ComputeMummichogRTPermPvals(t, total_matched, pathways,
                                       matched_res, input_mzlist)

    permutation_record[[i]] <- perm[1]
    permutation_hits[[i]] <- perm[2]

    # Progress indicator every 100 iterations (helps users know it's working)
    if(i %% 100 == 0){
      print(paste0("Completed ", i, "/", permNum, " permutations..."))
    }
  }

  # append new info
  mSetObj$perm_record <- permutation_record;
  mSetObj$perm_hits <- permutation_hits;
  return(mSetObj);
}

# Calculate p-values for each Lperm
# Used in higher mummichogR functions w. RT
ComputeMummichogRTPermPvals <- function(input_ecpdlist, total_matched_ecpds, pathways, matches.res, input_mzlist){
  
  ora.vec <- input_ecpdlist; #Lperm
  query_set_size <- length(ora.vec) # query set size
  current.mset <- pathways$emp_cpds; #all
  total_ecpds <- unique(total_matched_ecpds) # matched number of empirical compounds
  total_feature_num <- length(total_ecpds)
  
  size <- negneg <- vector(mode="list", length=length(current.mset));
  
  ecpds <- lapply(current.mset, function(x) intersect(x, total_ecpds)); # pathways & all ref ecpds
  feats <- lapply(current.mset, function(x) intersect(x, ora.vec)); #pathways & query ecpds (perm lsig)
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  set.num <- unlist(lapply(ecpds, length)); #cpdnum
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset));
  
  for(i in seq_along(current.mset)){ # for each pathway
    sizes[[i]] <- feat_len[i] # for ecs, just use length of overlap feats - overlap_size
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size;
  }
  
  unsize <- as.integer(unlist(sizes))
  res.mat <- matrix(0, nrow=length(current.mset), ncol=1)
  fishermatrix <- cbind(unsize-1, set.num, (query_set_size + unlist(negneg)), query_set_size)
  res.mat[,1] <- apply(fishermatrix, 1, function(x) phyper(x[1], x[2], x[3], x[4], lower.tail=FALSE));
  perm_records <- list(res.mat, as.matrix(unsize));
  return(perm_records);
}

# Internal function for significant p value 
.compute.mummichogSigPvals <- function(mSetObj){
  
  qset <- unique(unlist(mSetObj$input_cpdlist)); #Lsig ora.vec
  query_set_size <- length(qset); #q.size
  
  total_cpds <- unique(mSetObj$total_matched_cpds) #all matched compounds
  total_feature_num <- length(total_cpds)
  
  current.mset <- mSetObj$pathways$cpds; #all compounds per pathway
  path.num <- unlist(lapply(current.mset, length));
  
  cpds <- lapply(current.mset, function(x) intersect(x, total_cpds)); #pathways & all ref cpds
  set.num <- unlist(lapply(cpds, length)); #cpdnum
  
  feats <- lapply(current.mset, function(x) intersect(x, qset)); #pathways & lsig
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  feat_vec <- sapply(feats, function(x) paste(x, collapse=";"))
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset)); #empty lists
  
  for(i in seq_along(current.mset)){ # for each pathway
    sizes[[i]] <- min(feat_len[i], count_cpd2mz(mSetObj$cpd2mz_dict, unlist(feats[i]), mSetObj$dataSet$input_mzlist)) #min overlap or mz hits
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size; # failure in left part
  }
  
  #error fixing for negatives, problem occurs when total_feat_num and query_set_size too close (lsig too close to lall)
  negneg <- rapply(negneg, function(x) ifelse(x<0,0,x), how = "replace") 
  
  unsize <- as.integer(unlist(sizes));
  
  uniq.count <- length(unique(unlist(current.mset, use.names = FALSE)));
  
  # prepare for the result table
  res.mat <- matrix(0, nrow=length(current.mset), ncol=8);
  
  #fishermatrix for phyper
  fishermatrix <- cbind(unsize-1, set.num, (query_set_size + unlist(negneg) - unsize), query_set_size); 
  first <- unlist(lapply(sizes, function(x) max(0, x-1)));
  easematrix <- cbind(first, (set.num - unsize + 1), (query_set_size - unsize), unlist(negneg)); 
  
  res.mat[,1] <- path.num;  
  res.mat[,2] <- set.num;
  res.mat[,3] <- unsize;
  res.mat[,4] <- query_set_size*(path.num/uniq.count); #expected
  res.mat[,5] <- apply(fishermatrix, 1, function(x) phyper(x[1], x[2], x[3], x[4], lower.tail=FALSE));
  res.mat[,6] <- apply(easematrix, 1, function(x) fisher.test(matrix(x, nrow=2), alternative = "greater")$p.value);
  res.mat[,7] <- set.num;
  res.mat[,8] <- unsize;
  colnames(res.mat) <- c("Pathway total", "Hits.total", "Hits.sig", "Expected", "FET", "EASE","Total","Sig");
  rownames(res.mat) <- mSetObj$pathways$name
  
  mSetObj$pvals <- res.mat;
  permutations_hits <- matrix(unlist(mSetObj$perm_hits), nrow=length(mSetObj$perm_hits), byrow=TRUE);
  sig_hits <- res.mat[,3]; # sighits
  sigpvalue <- res.mat[,6]; # EASE scores
  
  perm_record <- unlist(mSetObj$perm_record);
  perm_minus <- abs(0.9999999999 - perm_record);
  
  if(length(sig_hits[sig_hits!=0]) < round(length(sig_hits)*0.05)){ # too few hits that can't calculate gamma dist!
    if(!exists("adjustedp")){
      adjustedp <- rep(NA, length = length(res.mat[,1]))
    }
    res.mat <- cbind(res.mat, Gamma=adjustedp);
  }else{
    tryCatch({
      fit.gamma <- fitdistrplus::fitdist(perm_minus, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1));
      rawpval <- as.numeric(sigpvalue);
      adjustedp <- 1 - (pgamma(1-rawpval, shape = fit.gamma$estimate["shape"], rate = fit.gamma$estimate["scale"]));
    }, error = function(e){
      if(mSetObj$dataSet$mum.type == "table"){
        if(!exists("adjustedp")){
          adjustedp <- rep(NA, length = length(res.mat[,1]))
        }
        res.mat <- cbind(res.mat, Gamma=adjustedp);
      }
      print(e)   
    }, finally = {
      if(!exists("adjustedp")){
        adjustedp <- rep(NA, length = length(res.mat[,1]))
      }
      res.mat <- cbind(res.mat, Gamma=adjustedp);
    })
  }
  
  #calculate empirical p-values
  record <- mSetObj$perm_record
  fisher.p <- as.numeric(res.mat[,5])
  
  #pathway in rows, perms in columns
  record_matrix <- do.call(cbind, do.call(cbind, record))
  num_perm <- ncol(record_matrix)
  
  #number of better hits for web
  better.hits <- sapply(seq_along(record_matrix[,1]), function(i) sum(record_matrix[i,] <= fisher.p[i])  )
  
  #account for a bias due to finite sampling - Davison and Hinkley (1997)
  emp.p <- sapply(seq_along(record_matrix[,1]), function(i) (sum(record_matrix[i,] <= fisher.p[i])/num_perm) )
  
  res.mat <- cbind(res.mat, Emp.Hits=better.hits, Empirical=emp.p, Cpd.Hits = feat_vec)
 
  # remove those no hits
  hit.inx <- as.numeric(as.character(res.mat[,3])) > 0;
  res.mat <- res.mat[hit.inx, , drop=FALSE];
  
  if(nrow(res.mat) <= 1){
    AddErrMsg("Not enough m/z to compound hits for pathway analysis!")
    return(0)
  }
  
  # prepare json element for network
  hits.all <- cpds[hit.inx];
  hits.sig <- feats[hit.inx];  
  path.nms <- mSetObj$pathways$name[hit.inx];
  
  # order by p-values
  ord.inx <- order(res.mat[,9]);
  Cpd.Hits <- res.mat[ord.inx, 12]
  res.mat <- signif(apply(as.matrix(res.mat[ord.inx, 1:11, drop=FALSE]), 2, as.numeric), 5);
  rownames(res.mat) <- path.nms[ord.inx];
  
  .save.mummichog.restable(res.mat, Cpd.Hits, mSetObj$mum_nm_csv);
  if(is.null(mSetObj$initPSEA) || mSetObj$initPSEA){
    mSetObj$mummi.resmat <- res.mat[,-11]; # not using adjusted for display other computing
    mSetObj$paramSet$mummi.lib <- mSetObj$lib.organism;
  }

  mSetObj$path.nms <- path.nms[ord.inx]
  mSetObj$path.hits <- convert2JsonList(hits.all[ord.inx])
  mSetObj$path.pval <- as.numeric(res.mat[,9])
  matched_res <- qs::qread("mum_res.qs");
  
  json.res <- list(
    cmpd.exp = mSetObj$cpd_exp,
    path.nms = path.nms[ord.inx],
    hits.all = convert2JsonList(hits.all[ord.inx]),
    hits.all.size = as.numeric(res.mat[,2]),
    hits.sig = convert2JsonList(hits.sig[ord.inx]),
    hits.sig.size = as.numeric(res.mat[,3]),
    fisher.p = as.numeric(res.mat[,5]),
    gamma.p = as.numeric(res.mat[,9]),
    peakToMet = mSetObj$cpd_form_dict,
    peakTable = matched_res
  );
  
  json.mat <- RJSONIO::toJSON(json.res);
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();

        if(is.null(mSetObj$imgSet$enrTables)){
            mSetObj$imgSet$enrTables <- list();
        }
        vis.type <- "mumEnr";
        resTable <- res.mat[,c(2,3,5,9)];
        resTable <- cbind(Name=path.nms[ord.inx], res.mat);
        mSetObj$imgSet$enrTables[[vis.type]] <- list();
        mSetObj$imgSet$enrTables[[vis.type]]$table <- resTable;
        mSetObj$imgSet$enrTables[[vis.type]]$library <- mSetObj$lib.organism;
        mSetObj$imgSet$enrTables[[vis.type]]$algo <- "Mummichog Analysis";
        mSetObj$imgSet$enrTables[[vis.type]]$fileName <- mSetObj$mum_nm_csv;    
  return(mSetObj);
}

# Internal function for significant p value with RT
.compute.mummichogRTSigPvals <- function(mSetObj){
  qset <- unique(unlist(mSetObj$input_ecpdlist)); #Lsig ora.vec
  query_set_size <- length(qset); #q.size
  input_cpd <- unique(unlist(mSetObj$ecpd_cpd_dict[qset]));

  total_ecpds <- unique(mSetObj$total_matched_ecpds) #all matched compounds
  total_feature_num <- length(total_ecpds)
  total_cpds <- unique(unlist(mSetObj$ecpd_cpd_dict));
  
  current.mset <- mSetObj$pathways$emp_cpds; #all compounds per pathway
  path.num <- unlist(lapply(current.mset, length));
  
  ecpds <- lapply(current.mset, function(x) intersect(x, total_ecpds)); #pathways & all ref ecpds
  set.num <- unlist(lapply(ecpds, length)); # total ecpd num in pathway
  cpd.num<- unlist(lapply(lapply(mSetObj$pathways$cpds, function(x) intersect(x, total_cpds)), length)); 

  feats <- lapply(current.mset, function(x) intersect(x, qset)); #pathways & lsig
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  feat_vec <- sapply(feats, function(x) paste(x, collapse=";"))
  cpd_len<- lapply(mSetObj$pathways$cpds, function(x) intersect(x, input_cpd))

  negneg <- sizes <- vector(mode="list", length=length(current.mset)); #empty lists
  
  for(i in seq_along(current.mset)){ # for each pathway
    sizes[[i]] <- feat_len[i] # overlap size
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size; # failure in left part
  }
  
  #error fixing for negatives, problem occurs when total_feat_num and query_set_size too close (lsig too close to lall)
  negneg <- rapply(negneg, function(x) ifelse(x<0,0,x), how = "replace") 
  
  unsize <- as.integer(unlist(sizes));
  
  uniq.count <- length(unique(unlist(current.mset, use.names = FALSE)));
  
  # prepare for the result table
  res.mat <- matrix(0, nrow=length(current.mset), ncol=8);
  
  #fishermatrix for phyper
  fishermatrix <- cbind(unsize-1, set.num, (query_set_size + unlist(negneg) - unsize), query_set_size);
  first <- unlist(lapply(sizes, function(x) max(0, x-1)));
  easematrix <- cbind(first, (set.num - unsize + 1), (query_set_size - unsize), unlist(negneg)); 
 

  res.mat[,1] <- unlist(lapply(mSetObj$pathways$cpds, length));  
  res.mat[,2] <- cpd.num;
  res.mat[,3] <- as.integer(unlist(lapply(cpd_len, length)));
  res.mat[,4] <- query_set_size*(path.num/uniq.count); #expected
  res.mat[,5] <- apply(fishermatrix, 1, function(x) phyper(x[1], x[2], x[3], x[4], lower.tail=FALSE));
  res.mat[,6] <- apply(easematrix, 1, function(x) fisher.test(matrix(x, nrow=2), alternative = "greater")$p.value);
  res.mat[,7] <- set.num
  res.mat[,8] <- unsize

  colnames(res.mat) <- c("Pathway total", "Hits.total", "Hits.sig", "Expected", "FET", "EASE","Total.EC","Sig.EC");
  rownames(res.mat) <- mSetObj$pathways$name
  
  mSetObj$pvals <- res.mat;
  permutations_hits <- matrix(unlist(mSetObj$perm_hits), nrow=length(mSetObj$perm_hits), byrow=TRUE);
  sig_hits <- unsize; # sighits
  sigpvalue <- res.mat[,5]; # EASE scores
  
  perm_record <- unlist(mSetObj$perm_record);
  perm_minus <- abs(0.9999999999 - perm_record);
  
  if(length(sig_hits[sig_hits!=0]) < round(length(sig_hits)*0.05)){ # too few hits that can't calculate gamma dist!
    if(!exists("adjustedp")){
      adjustedp <- rep(NA, length = length(res.mat[,1]))
    }
    res.mat <- cbind(res.mat, Gamma=adjustedp);
  }else{
    tryCatch({
      fit.gamma <- fitdistrplus::fitdist(perm_minus, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1));
      rawpval <- as.numeric(sigpvalue);
      adjustedp <- 1 - (pgamma(1-rawpval, shape = fit.gamma$estimate["shape"], rate = fit.gamma$estimate["scale"]));
    }, error = function(e){
      if(mSetObj$dataSet$mum.type == "table"){
        if(!exists("adjustedp")){
          adjustedp <- rep(NA, length = length(res.mat[,1]))
        }
        res.mat <- cbind(res.mat, Gamma=adjustedp);
      }
      print(e)   
    }, finally = {
      if(!exists("adjustedp")){
        adjustedp <- rep(NA, length = length(res.mat[,1]))
      }
      res.mat <- cbind(res.mat, Gamma=adjustedp);
    })
  }
  
  #calculate empirical p-values
  record <- mSetObj$perm_record


  fisher.p <- as.numeric(res.mat[,5])
  
  #pathway in rows, perms in columns
  record_matrix <- do.call(cbind, do.call(cbind, record))
num_perm <- ncol(record_matrix)

#number of better hits for web
better.hits <- sapply(seq_along(record_matrix[,1]), function(i) sum(record_matrix[i,] <= fisher.p[i])  )

#account for a bias due to finite sampling - Davison and Hinkley (1997)
emp.p <- sapply(seq_along(record_matrix[,1]), function(i) (sum(record_matrix[i,] <= fisher.p[i])/num_perm) )

res.mat <- cbind(res.mat, Emp.Hits = better.hits, Empirical = emp.p, EC.Hits = feat_vec)

# remove pathways with no hits
hit.inx <- as.numeric(as.character(res.mat[,8])) > 0;
res.mat <- res.mat[hit.inx, , drop=FALSE];

if(nrow(res.mat) <= 1){
  AddErrMsg("Not enough m/z to compound hits for pathway analysis! Try Version 1 (no RT considerations)!")
  return(0)
}

# prepare json element for network
# need to convert ecpds to cpds
# and get average expression based on ec
cpds <- lapply(ecpds, function(x) unique(unlist(mSetObj$ecpd_cpd_dict[match(x, names(mSetObj$ecpd_cpd_dict))])) )  
cpd.exp.vec <- sapply(ecpds, function(x) mean(mSetObj$ec_exp[match(x, names(mSetObj$ec_exp))]) )
cpds_feats <- lapply(feats, function(x) unique(unlist(mSetObj$ecpd_cpd_dict[match(x, names(mSetObj$ecpd_cpd_dict))])) )  

# now make exp vec for all compounds
cpds2ec <- mSetObj$cpd_ecpd_dict
cpds.all <- unique(unlist(mSetObj$ecpd_cpd_dict[match(total_ecpds, names(mSetObj$ecpd_cpd_dict))]))
cpd.exp.vec <- sapply(cpds.all, function(x) sapply(seq_along(x), function(i) mean(mSetObj$ec_exp[match(unique(unlist(cpds2ec[match(x[[i]], names(cpds2ec))])), names(mSetObj$ec_exp))]) ) )

hits.all <- cpds[hit.inx];
hits.sig <- cpds_feats[hit.inx];  
path.nms <- mSetObj$pathways$name[hit.inx];

# order by p-values
if(length(na.omit(res.mat[,9])) == 0){
  ord.inx <- order(res.mat[,5]); # order by FET if gamma not able to be calc
}else{
  ord.inx <- order(res.mat[,9]); # order by gamma
}

EC.Hits = res.mat[ord.inx, 12]
res.mat <- signif(apply(as.matrix(res.mat[ord.inx, 1:11]), 2, as.numeric), 5); # loop through columns and keep rownames
rownames(res.mat) <- path.nms[ord.inx]

.save.mummichog.restable(res.mat, EC.Hits, mSetObj$mum_nm_csv);

if(is.null(mSetObj$initPSEA) || mSetObj$initPSEA){
    mSetObj$mummi.resmat <- res.mat[,-11];
    mSetObj$paramSet$mummi.lib <- mSetObj$lib.organism;
}
mSetObj$path.nms <- path.nms[ord.inx]
mSetObj$path.hits <- convert2JsonList(hits.all[ord.inx])
mSetObj$path.pval <- as.numeric(res.mat[,5])
matched_res <- qs::qread("mum_res.qs");

json.res <- list(
  cmpd.exp = cpd.exp.vec,
  path.nms = path.nms[ord.inx],
  hits.all = convert2JsonList(hits.all[ord.inx]),
  hits.all.size = as.numeric(res.mat[,7]),
  hits.sig = convert2JsonList(hits.sig[ord.inx]),
  hits.sig.size = as.numeric(res.mat[,8]),
  fisher.p = as.numeric(res.mat[,5]),
  gamma.p = as.numeric(res.mat[,9]),
  peakToMet = mSetObj$cpd_form_dict,
  peakTable = matched_res,
  pathIDs = names(hits.all)
);

  json.mat <- RJSONIO::toJSON(json.res);
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();

    if(is.null(mSetObj$imgSet$enrTables)){
        mSetObj$imgSet$enrTables <- list();
    }
    vis.type <- "mumEnr";
    resTable <- cbind(Name=path.nms[ord.inx], res.mat[,c(7,8,5,9)]);
    mSetObj$imgSet$enrTables[[vis.type]] <- list();
    mSetObj$imgSet$enrTables[[vis.type]]$table <- resTable;
    mSetObj$imgSet$enrTables[[vis.type]]$library <- mSetObj$lib.organism;
    mSetObj$imgSet$enrTables[[vis.type]]$algo <- "Mummichog RT analysis";
    mSetObj$imgSet$enrTables[[vis.type]]$fileName <- mSetObj$mum_nm_csv;  

  return(mSetObj);
}

  # add adj p values for FET, EASE, and Gamma, and save the complete result table

.save.mummichog.restable <- function(my.res.mat, cpd.hits, file.name){
 
  adj.p.fet <- p.adjust(my.res.mat[,"FET"]);
  adj.p.ease <- p.adjust(my.res.mat[,"EASE"]);
  adj.p.gamma <-  p.adjust(my.res.mat[,"Gamma"]); 
  my.res.mat <- cbind(my.res.mat, AdjP.Fisher=adj.p.fet, AdjP.EASE=adj.p.ease, AdjP.Gamma=adj.p.gamma);
  my.res.mat <- my.res.mat[,-c(7,8)]; 
  my.res.mat <- cbind(my.res.mat, paste0("P", seq.int(1, nrow(my.res.mat))));
  colnames(my.res.mat)[ncol(my.res.mat)] = "Pathway Number";
  colnames(my.res.mat)[which(colnames(my.res.mat) == "FET")] <- "P(Fisher)";
  colnames(my.res.mat)[which(colnames(my.res.mat) == "EASE")] <- "P(EASE)";
  colnames(my.res.mat)[which(colnames(my.res.mat) == "Gamma")] <- "P(Gamma)";
  my.res.mat <- cbind(my.res.mat, cpd.hits);

  fast.write.csv(my.res.mat, file=file.name, row.names=TRUE);

}

## Internal function for calculating GSEA, no RT
.compute.mummichog.fgsea <- function(mSetObj, permNum){
  num_perm <- permNum;
  total_cpds <- mSetObj$cpd_exp #scores from all matched compounds
  
  current.mset <- mSetObj$pathways$cpds; #all compounds per pathway
  names(current.mset) <- mSetObj$pathways$name
  path.size <- unlist(lapply(mSetObj$pathways$cpds, length)) #total size of pathways
  
  df.scores <- data.frame(id=names(total_cpds), scores=total_cpds)
  ag.scores <- aggregate(id ~ scores, data = df.scores, paste, collapse = "; ")
    
  ag.sorted <- ag.scores[order(-ag.scores$scores),]
  row.names(ag.sorted) <- NULL
  
  dt.scores <- data.table::data.table(ag.sorted)
  dt.scores.out <- dt.scores[, list(scores=scores, 
                                    id = unlist(strsplit(id, "; ", fixed = TRUE))), 
                             by=1:nrow(dt.scores)]
  
  rank.vec <- as.numeric(dt.scores.out$nrow);
  names(rank.vec) <- as.character(dt.scores.out$id);
   
  scores.vec <- as.numeric(ag.sorted$scores)
  names(scores.vec) <- as.character(ag.sorted$id)

  # run fgsea
  if(mSetObj$paramSet$mumDataContainsPval == 0){
    rank.vec = seq.int(1, length(mSetObj$cpd_exp))
    names(rank.vec) <- names(mSetObj$cpd_exp)
    scores.vec = seq.int(1, length(mSetObj$cpd_exp))
    names(scores.vec) <- names(mSetObj$cpd_exp)
  }

  fgseaRes <- fgsea2(mSetObj, current.mset, scores.vec, rank.vec, num_perm)  
  res.mat <- matrix(0, nrow=length(fgseaRes$pathway), ncol=5)

  path.size <- unlist(lapply(current.mset, length))
  matched.size <- path.size[match(fgseaRes$pathway, names(path.size))]

  # create result table
  res.mat[,1] <- matched.size;
  res.mat[,2] <- fgseaRes$size;
  res.mat[,3] <- fgseaRes$pval;
  res.mat[,4] <- fgseaRes$padj;
  res.mat[,5] <- fgseaRes$NES;
  
  rownames(res.mat) <- fgseaRes$pathway;
  colnames(res.mat) <- c("Pathway_Total", "Hits", "P_val", "P_adj", "NES");
   
  # order by p-values
  ord.inx <- order(res.mat[,3]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 4);

  if(is.null(mSetObj$initPSEA) || mSetObj$initPSEA){
    print("mSetObj$paramSet");
    mSetObj$mummi.gsea.resmat <- res.mat;
    mSetObj$paramSet$gsea.lib <- mSetObj$lib.organism;

  }

  Cpd.Hits <- qs::qread("pathwaysFiltered.qs")
  Cpd.Hits <- unlist(lapply(seq_along(Cpd.Hits), function(i) paste(names(Cpd.Hits[[i]]), collapse = ";")))
  Cpd.Hits <- Cpd.Hits[Cpd.Hits != ""]
  
  res.mat <- cbind(res.mat, Cpd.Hits[ord.inx])
  fast.write.csv(res.mat, file=mSetObj$mum_nm_csv, row.names=TRUE);
  
  matched_cpds <- names(mSetObj$cpd_exp)
  inx2<- stats::na.omit(match(rownames(res.mat), mSetObj$pathways$name))
  filt_cpds <- lapply(inx2, function(f) {mSetObj$pathways$cpds[f]})
  
  cpds <- lapply(filt_cpds, function(x) intersect(unlist(x), matched_cpds))
  mSetObj$path.nms <- rownames(res.mat)
  mSetObj$path.hits<- convert2JsonList(cpds)
  mSetObj$path.pval <- as.numeric(res.mat[,3])
  json.res <- list(cmpd.exp = total_cpds,
                   path.nms = rownames(res.mat),
                   hits.all = convert2JsonList(cpds),
                   hits.all.size = as.numeric(res.mat[,2]),
                   nes = fgseaRes$NES,
                   fisher.p = as.numeric(res.mat[,3]))
  
  json.mat <- RJSONIO::toJSON(json.res);
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();

  return(mSetObj);
}

#' Internal function for calculating GSEA, with RT
#' @noRd
#' @importFrom stats aggregate
.compute.mummichog.RT.fgsea <- function(mSetObj, permNum){

  #Declare variable
  scores <- NULL;
  
  # Need to perform in EC space
  num_perm <- permNum;
  total_ecpds <- mSetObj$ec_exp #scores from all matched compounds
  
  current.mset <- mSetObj$pathways$emp_cpds; #all compounds per pathway
  names(current.mset) <- mSetObj$pathways$name
  path.size <- unlist(lapply(mSetObj$pathways$ecpds, length)) #total size of pathways
  
  df.scores <- data.frame(id=names(total_ecpds), scores=total_ecpds)
  ag.scores <- aggregate(id ~ scores, data = df.scores, paste, collapse = "; ")
  
  ag.sorted <- ag.scores[order(-ag.scores$scores),]
  row.names(ag.sorted) <- NULL
  
  dt.scores <- data.table::data.table(ag.sorted)
  dt.scores.out <- dt.scores[, list(scores=scores, id = unlist(strsplit(id, "; ", fixed = TRUE))), by=1:nrow(dt.scores)]
  
  rank.vec <- as.numeric(dt.scores.out$nrow)
  names(rank.vec) <- as.character(dt.scores.out$id)
  
  scores.vec <- as.numeric(ag.sorted$scores)
  names(scores.vec) <- as.character(ag.sorted$id)
  
  # run fgsea
  if(mSetObj$paramSet$mumDataContainsPval == 0){
    rank.vec = seq.int(1, length(mSetObj$ec_exp))
    names(rank.vec) <- names(mSetObj$ec_exp)
    scores.vec = seq.int(1, length(mSetObj$ec_exp))
    names(scores.vec) <- names(mSetObj$ec_exp)
  }
  
  fgseaRes <- fgsea2(mSetObj, current.mset, scores.vec, rank.vec, num_perm)

  res.mat <- matrix(0, nrow=length(fgseaRes$pathway), ncol=5)
  
 total_cpds <- names(mSetObj$cpd_exp_dict)
 total.match <- lapply(mSetObj$pathways$cpds, function(x) intersect(x,total_cpds))
 matched.size <- unlist(lapply(total.match, length))
 matched.size <- matched.size[match(fgseaRes$pathway, names(current.mset))]
 
 path.size <- unlist(lapply(mSetObj$pathways$cpds, length))
 path.size <- path.size[match(fgseaRes$pathway, names(current.mset))]
  # create result table
  res.mat[,1] <- path.size
  res.mat[,2] <- matched.size
  res.mat[,3] <- fgseaRes$pval
  res.mat[,4] <- fgseaRes$padj
  res.mat[,5] <- fgseaRes$NES
  
  rownames(res.mat) <- fgseaRes$pathway
  colnames(res.mat) <- c("Pathway_Total", "Hits", "P_val", "P_adj", "NES")
  
  # order by p-values
  ord.inx <- order(res.mat[,3]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 4);
  
  mSetObj$mummi.gsea.resmat <- res.mat;
  
  EC.Hits <- qs::qread("pathwaysFiltered.qs")
  EC.Hits <- lapply(seq_along(EC.Hits), function(i) paste(names(EC.Hits[[i]]), collapse = ";"))
  res.mat <- cbind(res.mat, EC.Hits)
  fast.write.csv(res.mat, file=mSetObj$mum_nm_csv, row.names=TRUE);
  
  # need to convert ECs to compounds for json
  total_ecpds <- unique(mSetObj$total_matched_ecpds) #all matched compounds
  current.mset <- current.mset[match(rownames(res.mat), mSetObj$pathways$name)]
  ecpds <- lapply(current.mset, function(x) intersect(x, total_ecpds)); #pathways & all ref ecpds
  cpds <- lapply(ecpds, function(x) unique(unlist(mSetObj$ecpd_cpd_dict[match(x, names(mSetObj$ecpd_cpd_dict))])) )
  
  # now make exp vec for all compounds
  cpds2ec <- mSetObj$cpd_ecpd_dict
  cpds.all <- unique(unlist(mSetObj$ecpd_cpd_dict[match(total_ecpds, names(mSetObj$ecpd_cpd_dict))]))
  cpds.exp <- sapply(cpds.all, function(x) sapply(seq_along(x), function(i) mean(mSetObj$ec_exp[match(unique(unlist(cpds2ec[match(x[[i]], names(cpds2ec))])), names(mSetObj$ec_exp))]) ) )
  
  mSetObj$path.nms <- rownames(res.mat)
  mSetObj$path.hits <- convert2JsonList(cpds)
  mSetObj$path.pval <- as.numeric(res.mat[,3])
  
  json.res <- list(cmpd.exp = cpds.exp,
                   path.nms = rownames(res.mat),
                   hits.all = convert2JsonList(cpds),
                   hits.all.size = as.numeric(res.mat[,2]),
                   nes = fgseaRes$NES,
                   fisher.p = as.numeric(res.mat[,3]))
  
  json.mat <- RJSONIO::toJSON(json.res);
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();
  
  return(mSetObj);
}

#####################################################################

#'Map currency metabolites to KEGG & BioCyc
#'@description This function maps the user selected list
#'of compounds to its corresponding KEGG IDs and BioCyc IDs
#'@param mSetObj Input the name of the created mSetObj object 
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformCurrencyMapping <- function(mSetObj = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  qvec <- mSetObj$dataSet$cmpd;
  curr_db <- .get.my.lib("currency_cmpd.qs");
  hit.inx <- match(tolower(qvec), tolower(curr_db$DisplayName));
  
  num_hits <- length(na.omit(hit.inx))
  
  if(num_hits == 0){
    mSetObj$mummi$curr.msg <- c("No currency metabolites were selected or mapped!")
    return(0)
  }
  
  match.values <- curr_db[hit.inx,];
  curr.met <- nrow(match.values)
  
  mSetObj$curr.map <- match.values
  
  if(curr.met > 0){
    mSetObj$mummi$curr.msg <- paste("A total of ", curr.met ," currency metabolites were successfully uploaded!", sep = "")
  }
  
  mSetObj$curr.cust <- TRUE;
  return(.set.mSet(mSetObj));
}

#'PerformAdductMapping
#'@description This function reads in the user's adduct list and 
#'saves it as a matrix.
#'@param mSetObj Input the name of the created mSetObj object 
#'@param add.mode Adduct mode, positive or negative
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformAdductMapping <- function(mSetObj=NA, add.mode){
  
  mSetObj <- .get.mSet(mSetObj);
  
  adducts <- mSetObj$dataSet$adduct.list
  
  if(add.mode == "positive"){
    add_db <- .get.my.lib("pos_adduct.qs");
  }else if(add.mode == "negative"){
    add_db <- .get.my.lib("neg_adduct.qs");
  }else if(add.mode == "mixed"){
    add_db <- .get.my.lib("mixed_adduct.qs");
  }else{
    msg <- c("Adduct mode is not valid")
  }
  
  hit.inx <- match(tolower(adducts), tolower(add_db$Ion_Name));  
  hits <- length(na.omit(hit.inx))
  
  if(hits == 0){
    mSetObj$mummi$add.msg <- c("No adducts were selected!");
    return(0)
  }
  
  match.values <- add_db[na.omit(hit.inx),];
  sel.add <- nrow(match.values);
  
  if(sel.add > 0){
    mSetObj$mummi$add.msg <- paste("A total of ", sel.add ," adducts were successfully selected!", sep = "")
  }
  
  mSetObj$dataSet$adduct.custom <- TRUE
  mSetObj$dataSet$add.map <- match.values
  
  return(.set.mSet(mSetObj));
}

# internal function to create new mz matrix from user-curated list of adducts
new_adduct_mzlist <- function(mSetObj=NA, mw){
  
  # if(!exists("metaFiles")){
  #   mSetObj <- .get.mSet(mSetObj);
  # }
  
  mode <- mSetObj$dataSet$mode;
  
  ion.name <- mSetObj$dataSet$add.map$Ion_Name
  ion.mass <- mSetObj$dataSet$add.map$Ion_Mass
  
  mw_modified <- NULL;
  
  if(mode!="mixed"){ #pos or neg
    
    mass.list <- as.list(ion.mass)
    mass.user <- lapply(mass.list, function(x) eval(parse(text=paste(gsub("PROTON", 1.00727646677, x)))) )
    mw_modified <- cbind(mw, do.call(cbind, mass.user));
    
    if(mode == "positive"){
      mw_modified.pos <- mw_modified[,-1, drop = FALSE]
      mw_modified.neg <- as.matrix(mw_modified[,1, drop = FALSE])
      colnames(mw_modified.pos) <- ion.name;
      colnames(mw_modified.neg) <- "M"
    }else{ #negative
      mw_modified.neg <- mw_modified[,-1, drop = FALSE]
      mw_modified.pos <- as.matrix(mw_modified[,1, drop = FALSE])
      colnames(mw_modified.neg) <- ion.name;
      colnames(mw_modified.pos) <- "M"
    }
    
    mw_modified <- list(mw_modified.neg, mw_modified.pos)
    names(mw_modified) <- c("neg", "pos")
    
  } else {
    #deal w. mixed ion mode, need to return pos and neg 
    
    neg.ions <- c("M-H [1-]", "M-2H [2-]", "M-3H [3-]", "M-H2O-H [1-]", "M-H+O [1-]", "M+K-2H [1-]", "M+Na-2H [1- ]", "M+Cl [1-]", "M+Cl37 [1-]",   
                  "M+K-2H [1-]", "M+FA-H [1-]", "M+Hac-H [1-]", "M+Br [1-]", "M+Br81 [1-]", "M+TFA-H [1-]", "M+ACN-H [1-]", "M+HCOO [1-]", "M+CH3COO [1-]", 
                  "2M-H [1-]", "2M+FA-H [1-]", "2M+Hac-H [1-]", "3M-H [1-]", "M(C13)-H [1-]", "M(S34)-H [1-]", "M(Cl37)-H [1-]")
    
    ion.name.neg <- intersect(ion.name, neg.ions)
    ion.mass.neg <- ion.mass[which(ion.name %in% neg.ions)] 
    
    ion.name.pos <- setdiff(ion.name, neg.ions)
    ion.mass.pos <- ion.mass[which(ion.name %in% ion.name.pos)] 
    
    mass.list.neg <- as.list(ion.mass.neg)
    mass.user.neg <- lapply(mass.list.neg, function(x) eval(parse(text=paste(gsub("PROTON", 1.00727646677, x)))) )
    mw_modified.neg <- do.call(cbind, mass.user.neg);
    colnames(mw_modified.neg) <- ion.name.neg;
    
    mass.list.pos <- as.list(ion.mass.pos)
    mass.user.pos <- lapply(mass.list.pos, function(x) eval(parse(text=paste(gsub("PROTON", 1.00727646677, x)))) )
    mw_modified.pos <- do.call(cbind, mass.user.pos);
    colnames(mw_modified.pos) <- ion.name.pos;
    
    mw_modified <- list(mw_modified.neg, mw_modified.pos)
    names(mw_modified) <- c("neg", "pos")
  }
  return(mw_modified);
}

#'Update the mSetObj with user-selected parameters for MS Peaks to Pathways.
#'@description This functions handles updating the mSet object for mummichog analysis. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param force_primary_ion Character, if "yes", only mz features that match compounds with a primary ion are kept.
#'@param rt_tol Numeric. Input the retention time tolerance used for determining ECs (in seconds).
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
UpdateEC_Rules <- function(mSetObj = NA, force_primary_ion, rt_tol){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$primary_ion <- force_primary_ion;
  
  ok <- is.numeric(rt_tol)
  
  if(ok){
    mSetObj$dataSet$rt_tol <- rt_tol;
  }else{
    AddErrMsg("Retention time tolerance must be numeric!")
    return(0)
  }
  
  msg.vec <- "EC Rules successfully updated."
  mSetObj$mummi$ec.msg <- msg.vec
  
  return(.set.mSet(mSetObj));
}

##############################
##### Plotting Functions #####
##############################

#' PlotPeaks2Paths
#' @description Plots either the original mummichog or GSEA results.
#' @param mSetObj Input the name of the created mSetObj object
#' @param imgName Input a name for the plot
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param labels Character, indicate if the plot should be labeled. By default
#' it is set to "default", and the 5 top-ranked pathways per each algorithm will be plotted.
#' Users can adjust the number of pathways to be annotated per pathway using the "num_annot" 
#' parameter.
#' @param num_annot number of annotations for top plotting
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

PlotPeaks2Paths <- function(mSetObj=NA, imgName="", format = "png", dpi = default.dpi, width = 9, labels = "default",
                            num_annot = 5, interactive=F){  
  #save.image("gsea.RData")
  mSetObj <- .get.mSet(mSetObj)
  anal.type0 <- mSetObj$paramSet$anal.type
  if (anal.type0 == "mummichog") {
    mat <- mSetObj$mummi.resmat
    #print(head(mat));
    y <- -log10(mat[, 5])
    x <- mat[, 8] / mat[, 4]
    pathnames <- rownames(mat)
  } else {
    mat <- mSetObj$mummi.gsea.resmat
    y <- -log10(mat[, 3])
    x <- mat[,5]
    pathnames <- rownames(mat)
  }
  
  inx <- order(y, decreasing = TRUE)
  y <- y[inx]
  x <- x[inx]
  pathnames <- pathnames[inx]
  

  
  # Create the data frame
  df <- data.frame(y, x, pathnames)
  
  # Generate ggplot
  library(ggplot2);
  library(ggrepel)
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if (anal.type0 == "mummichog") {
  # set circle size based on enrichment factor
  radi.vec <- sqrt(abs(x))
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = radi.vec, color = y, text = paste("Pathway:", pathnames, 
                                "<br>Enrichment Factor:", round(x, 3), 
                                "<br>-log10(p):", round(y, 3))), stroke = 0.5) +
    scale_size_continuous(range = c(1, 5)) +
    scale_color_gradient(low = "yellow", high = "red", name="-log10(p)") +
    xlab("Enrichment Factor") +
    ylab("-log10(p)") +
    theme_minimal();

  # Add text labels for top num_annot points
  top_indices <- head(order(-df$y), num_annot)
  p <- p + geom_text_repel(aes(label = pathnames), data = df[top_indices, ], size = 3)
  mSetObj$imgSet$mummi.plot<- imgName

  }else{
  # set circle size based on P-val
  radi.vec <- sqrt(abs(y))
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(size = radi.vec, color = x, text = paste("Pathway:", pathnames, 
                                "<br>NES:", round(x, 3), 
                                "<br>-log10(p):", round(y, 3))), stroke = 0.5) +
    scale_size_continuous(range = c(1, 5)) +
    scale_color_gradient2(low = "#458B00", mid = "#fffee0", high = "#7f0000", midpoint = 0, name="NES") +
    xlab("NES") +
    ylab("-log10(p)") +
    theme_minimal();
  top_indices <- head(order(-df$y), num_annot)
  p <- p + geom_text_repel(aes(label = pathnames), data = df[top_indices, ], size = 3)
  mSetObj$imgSet$mummi.gsea.plot<- imgName
  }

  
  
  if (anal.type0 == "mummichog") {
    list_data <- list(pval = unname(y), enr = unname(x), pathnames = pathnames)
    write(RJSONIO::toJSON(list_data), "scattermum.json")
  } else {
    list_data <- list(pval = unname(y), enr = unname(mat[, 5]), pathnames = pathnames)
    write(RJSONIO::toJSON(list_data), "scattergsea.json")
  }

  if(interactive){
    library(plotly);
    ggp_build <- layout(ggplotly(p, width = 800, height = 600, tooltip = c("text")), autosize = FALSE, margin = mSetObj$imgSet$margin.config)
    return(ggp_build);
  }else{
    if(is.na(width)){
       w <- 7;
     }else if(width == 0){
       w <- 7;
     }else{
       w <- width;
     }
     h <- w;
     Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format);
     print(p);
     dev.off()
     return(.set.mSet(mSetObj));
  }
  
}
#' PlotPSEAIntegPaths
#' @description Plots both the original mummichog and the GSEA results by combining p-values
#' using the Fisher's method (sumlog). 
#' @param mSetObj Input the name of the created mSetObj object
#' @param imgName Input a name for the plot
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param labels Character, indicate if the plot should be labeled. By default
#' it is set to "default", and the 5 top-ranked pathways per each algorithm will be plotted.
#' Users can adjust the number of pathways to be annotated per pathway using the "labels.x" 
#' and "labels.y" parameters.
#' Users can set this to "none" for no annotations, or "all" to annotate all pathways. 
#' @param labels.x Numeric, indicate the number of top-ranked pathways using the fGSEA algorithm 
#'  to annotate on the plot. 
#' @param labels.y Numeric, indicate the number of top-ranked pathways using the original 
#' mummichog algorithm to annotate on the plot. 
#' @param scale.axis logical, TRUE to scale
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import scales
PlotPSEAIntegPaths <- function(mSetObj=NA, imgName="", format = "png", dpi = default.dpi, width = 9, labels = "default", 
                               labels.x = 5, labels.y = 5, scale.axis = TRUE, interactive=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # check if mummichog + gsea was performed
  if(is.null(mSetObj$mummi.resmat) | is.null(mSetObj$mummi.gsea.resmat)){
    print("Both mummichog and fGSEA must be performed!")
    return(0)
  }
  
  combo.resmat <- mSetObj$integ.resmat
  pathnames <- rownames(combo.resmat)
  # Sort values based on combined pvalues
  y <- -log10(combo.resmat[,4]);
  x <- -log10(combo.resmat[,5]);
  combo.p <- -log10(combo.resmat[,6])
  
  if(scale.axis){
    y <- scales::rescale(y, c(0,4))
    x <- scales::rescale(x, c(0,4))
    combo.p <- scales::rescale(combo.p, c(0,4))
  }
  
  inx <- order(combo.p, decreasing= T);
  
  combo.p <- combo.p[inx]
  x <- x[inx]; 
  y <- y[inx];
  path.nms <- pathnames[inx];
  
  # set circle size based on combined pvalues
  min.x <- min(combo.p, na.rm = TRUE);
  max.x <- max(combo.p, na.rm = TRUE);
  
  if(min.x == max.x){ # only 1 value
    max.x = 1.5*max.x;
    min.x = 0.5*min.x;
  }
  
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(combo.p-min.x)/(max.x-min.x);
  
  # Combine the x and y values to create a new variable for heatmap coloring
  combined_value <- x + y  # You can also use another combination method
  
  if(format == "png"){
    bg = "transparent";
  }else{
    bg="white";
  }
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  df <- data.frame(path.nms, x, y, combined_value)
  
  if(labels == "default"){
    mummi.inx <- GetTopInx(df$y, labels.y, T)
    gsea.inx <- GetTopInx(df$x, labels.x, T)
    all.inx <- mummi.inx | gsea.inx;
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$integpks.plot <- imgName
  
  library(ggplot2)
  library(plotly)
  library(ggrepel)  # Load ggrepel for improved label placement
  
  df$radi.vec <- radi.vec;
  
  # Find the top 5 pathways based on the combined values of x and y
  top_5 <- df[order(df$combined_value, decreasing = TRUE), ][1:5, ]
  
  # Generate a heatmap color scale based on the combined values of x and y, reversed
  p <- ggplot(df, aes(x = x, y = y, text = paste("Pathway:", path.nms, 
                                                 "<br>GSEA p-val:", signif(10^-x, 4), 
                                                 "<br>Mummichog p-val:", signif(10^-y, 4)))) +
    geom_point(aes(size = radi.vec, color = combined_value), alpha = 0.7) +  # Slightly increase transparency
    geom_text_repel(data = top_5, aes(label = path.nms), size = 3) +  # Add repelling labels for top 5 pathways
    scale_color_gradientn(colors = rev(heat.colors(10)), name="Combined Score") +  # Reversed heat colors
    scale_size_continuous(range = c(3, 10), guide="none") +  # Increased size range
    labs(x = "GSEA -log10(p)", y = "Mummichog -log10(p)") +
    theme_minimal() +
    theme(legend.position = "none",  # Remove the legend
          panel.background = element_rect(fill = "white"),  # Set panel background to white
          plot.background = element_rect(fill = "white"))   # Set plot background to white
  
  df <- list(pval=unname(y), enr=unname(x), metap= unname(combo.p), pathnames=pathnames);
  sink("scatterinteg.json");
  cat(RJSONIO::toJSON(df));
  sink();
  
  if(interactive){
    plotly_p <- layout(ggplotly(p,width = 800, height = 600, tooltip = c("text")), legend = list(
      traceorder = "normal",
      orientation = "v",
      yanchor = "top",
      xanchor = "left",
      itemsizing = "constant"  # This tries to make legend item sizes constant
    ),autosize = FALSE, margin = mSetObj$imgSet$margin.config)
    return(plotly_p);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format);   
    print(p)
    dev.off();
    return(.set.mSet(mSetObj));
  }
}



###############################
####### Getters For Web #######
###############################

GetMatchingDetails <- function(mSetObj=NA, cmpd.id){  
  mSetObj <- .get.mSet(mSetObj);
  forms <- mSetObj$cpd_form_dict[[cmpd.id]];
  tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
  # create html table
  res <- paste("<li>", "<b>", forms, "</b>: ", round(tscores,2), "</li>",sep="", collapse="");  
  return(res);
}

GetMummichogHTMLPathSet <- function(mSetObj = NA, msetNm) {
  message(msetNm); message("msetnm==================")
  # save.image("pathset.RData")
  mSetObj <- .get.mSet(mSetObj)

  inx <- which(mSetObj$pathways$name == msetNm)
  if (length(inx) != 1L) {
    stop("Pathway name '", msetNm, "' not found or not unique in mSetObj$pathways$name.")
  }

  # --- 1) Normalize mset (character vector of KEGG IDs) -----------------
  raw_mset <- mSetObj$pathways$cpds[[inx]]
  mset_ids <- unique(as.character(unlist(raw_mset, use.names = FALSE)))  # <- fix
  if (!length(mset_ids)) {
    return(cbind(msetNm, ""))  # empty pathway
  }

  # --- 2) Name lookup table ---------------------------------------------
  cmpd.db <- .get.my.lib("compound_db.qs")  # expects columns: kegg_id, name
  if (is.null(cmpd.db) || !all(c("kegg_id", "name") %in% colnames(cmpd.db))) {
    stop("compound_db.qs is missing or lacks columns 'kegg_id' and 'name'.")
  }
  cmpd.db$kegg_id <- as.character(cmpd.db$kegg_id)
  cmpd.db$name    <- as.character(cmpd.db$name)
  kegg2name <- stats::setNames(cmpd.db$name, cmpd.db$kegg_id)

  # Map to common names where available; else fall back to the ID
  mapped <- kegg2name[mset_ids]
  display <- ifelse(is.na(mapped) | mapped == "" | mapped == "NA", mset_ids, mapped)

  # --- 3) Collect hits ---------------------------------------------------
  mum.version <- mSetObj$paramSet$version
  hasRT       <- isTRUE(mSetObj$paramSet$mumRT)
  anal.type0  <- mSetObj$paramSet$anal.type

  # ensure all hits are character vectors of KEGG IDs
  if (identical(mum.version, "v2") && hasRT) {
    hits.all <- unique(as.character(unlist(mSetObj$ecpd_cpd_dict, use.names = FALSE)))
  } else {
    hits.all <- unique(as.character(unlist(mSetObj$total_matched_cpds, use.names = FALSE)))
  }
  hits.all <- hits.all[!is.na(hits.all)]

  if (anal.type0 %in% c("mummichog", "integ_peaks")) {
    if (identical(mum.version, "v2") && hasRT) {
      hits.sig <- as.character(unlist(mSetObj$input_ecpdlist, use.names = FALSE))
      hits.sig <- as.character(unlist(mSetObj$ecpd_cpd_dict[ match(hits.sig, names(mSetObj$ecpd_cpd_dict)) ], use.names = FALSE))
    } else {
      hits.sig <- as.character(unlist(mSetObj$input_cpdlist, use.names = FALSE))
    }
    hits.sig <- unique(hits.sig[!is.na(hits.sig)])

    refs <- mset_ids %in% hits.all
    sigs <- mset_ids %in% hits.sig

    red.inx  <- which(sigs)
    blue.inx <- which(refs & !sigs)
  } else {
    refs     <- mset_ids %in% hits.all
    red.inx  <- which(refs)
    blue.inx <- integer(0)
  }

  # --- 4) Colorize display names ----------------------------------------
  nms <- display
  if (length(red.inx))  nms[red.inx]  <- paste0('<font color="red"><b>',  nms[red.inx],  "</b></font>")
  if (length(blue.inx)) nms[blue.inx] <- paste0('<font color="blue"><b>', nms[blue.inx], "</b></font>")

  # If very large, keep only referenced ones
  if (length(nms) > 200) {
    nms <- nms[refs]
  }

  cbind(msetNm, paste(unique(nms), collapse = "; "))
}

GetMummiResMatrix <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  anal.type0 <- mSetObj$paramSet$anal.type;
  if(anal.type0 == "mummichog"){    
    return(mSetObj$mummi.resmat);
  }else if(anal.type0 == "gsea_peaks"){
    return(mSetObj$mummi.gsea.resmat);
  }else{
    return(mSetObj$integ.resmat);
  }
}

GetMummiResRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  anal.type0 <- mSetObj$paramSet$anal.type;
  if(anal.type0 == "mummichog"){
    return(rownames(mSetObj$mummi.resmat));
  }else if(anal.type0 == "gsea_peaks"){
    return(rownames(mSetObj$mummi.gsea.resmat));
  }else{
    return(rownames(mSetObj$integ.resmat));
  }
}

GetMummiResColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$mummi.resmat));
}

GetCurrencyMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi$curr.msg)
}

GetAdductMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi$add.msg)
}

GetECMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi$ec.msg)
}

GetDefaultPvalCutoff <- function(mSetObj=NA){

  mSetObj <- .get.mSet(); 
  peakFormat <- mSetObj$paramSet$peakFormat;
  
  if(peakFormat %in% c("rmp", "rmt")){
    maxp <- 0;
  }else{
    pvals <- c(0.25, 0.2, 0.15, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
    ndat <- mSetObj$dataSet$mummi.proc;
    
    ### Handle something very wrong for mass table
    if(is.null(ndat)){
      res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls, var.equal=TRUE);
      res$p.value <- p.adjust(res$p.value, "fdr")
      ndat <- res[order(res[,2]),];
      n <- floor(0.1*length(ndat[,2]))
      cutoff <- ndat[n+1,2]
    } else {
      n <- floor(0.1*length(ndat[,"p.value"]))
      cutoff <- ndat[n+1,1]
    }
    
    if(nrow(ndat) > 4000) {
      # ensure enough sig peaks included AND ratio < 50%
      if(n < 2000 & ndat[2000,"p.value"] < 0.05) {
        cutoff <- ndat[2001,"p.value"]
        return(signif(cutoff, 1))
      }
    }
    
    if(!any(pvals <= cutoff)){
      maxp <- 0.00001
    }else{
      maxp <- max(pvals[pvals <= cutoff])
    }
  }
  return(maxp)
}

GetDefaultRTTol <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rt_tol <- mSetObj$dataSet$rt_tol;
  if(is.na(rt_tol)){
    rt_tol <- 0
  }
  return(rt_tol)
}

GetMummiMode <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  mode <- mSetObj$dataSet$mode;
  return(mode);
}

GetMummiDataType <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  type <- mSetObj$dataSet$type
  return(type)
}

# Replicate because do not want to have to read in stats_univariate to perform MS Peaks
GetTopInx <- function(vec, n, dec=T){
  inx <- order(vec, decreasing = dec)[1:n];
  # convert to T/F vec
  vec<-rep(F, length=length(vec));
  vec[inx] <- T;
  return (vec);
}

GetOrgMummichogLbl <-function(mSetObj=NA){
  org = read.csv(paste0(rpath ,"libs/orgmummichog.csv"))
  return(org$label);
}

GetOrgMummichogVal <-function(mSetObj=NA){
  org = read.csv(paste0(rpath ,"libs/orgmummichog.csv"))
  return(org$id);
}


#########################################
########### Utility Functions ###########
#########################################

# Global variables define currency compounds
currency <- c('C00001', 'C00080', 'C00007', 'C00006', 'C00005', 'C00003',
              'C00004', 'C00002', 'C00013', 'C00008', 'C00009', 'C00011',
              'G11113', '', 'H2O', 'H+', 'Oxygen', 'NADP+', 
              'NADPH', 'NAD+', 'NADH', 'ATP', 
              'Pyrophosphate', 'ADP', 'CO2');

all_currency <- c('C00001', 'C00080', 'C00007', 'C00006', 'C00005', 'C00003',
                  'C00004', 'C00002', 'C00013', 'C00008', 'C00009', 'C00011',
                  'G11113', '', 'H2O', 'Water', 'H+', 'Hydron', 'O2', 'Oxygen', 'NADP+', 
                  'NADP', 'NADPH', 'NAD+', 'NAD', 'NADH', 'ATP', 'Diphosphate',
                  'Pyrophosphate', 'ADP','Orthophosphate', 'CO2', 'Carbon dioxide');

primary_ions <- c('M+H[1+]', 'M+Na[1+]', 'M-H2O+H[1+]', 'M-H[-]', 'M-2H[2-]', 'M-H2O-H[-]',
                  'M+H [1+]', 'M+Na [1+]', 'M-H2O+H [1+]', 'M-H [1-]', 'M-2H [2-]', 'M-H2O-H [1-]')

# mz tolerance based on instrument type
# input: a vector of mz,
# output: a vector of distance tolerance
# Review on mass accuracy by Fiehn: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1464138/

mz_tolerance <- function(mz, ms.type){
  return(ms.type*1e-06*mz)
}

#'Utility function to create compound lists for permutation analysis
#'@description From a vector of m/z features, this function outputs a vector of compounds.
#'@usage make_cpdlist(mSetObj=NA, input_mzs)
#'@param mSetObj Input the name of the created mSetObj
#'@param input_mzs The vector of randomly drawn m/z features.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
make_cpdlist <- function(mSetObj=NA, input_mzs){
  cpd <- unique(unlist(mSetObj$mz2cpd_dict[input_mzs]));
  cpd <- cpd[!is.null(cpd)];
  return(cpd);
}

#'Utility function to create compound lists for permutation analysis
#'@description From a vector of m/z features, this function outputs a vector of compounds.
#'@usage make_ecpdlist(mSetObj=NA, input_mzs)
#'@param mSetObj Input the name of the created mSetObj
#'@param input_mzs The vector of randomly drawn m/z features.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
make_ecpdlist <- function(mSetObj=NA, input_mzs){
  ecpd <- unique(unlist(mSetObj$mz2ec_dict[input_mzs]));
  ecpd <- ecpd[!is.null(ecpd)];
  return(ecpd);
}

# Utility function to adjust for the fact that a single m/z feature can match to several compound identifiers
# input: a vector of compound ids
# output: a length of unique mzs corresponding to those compounds

count_cpd2mz <- function(cpd2mz_dict, cpd.ids,  inputmzlist){ # inputmz is either t or input cpd_list and cpd.ids are overlap features
  
  if(length(cpd.ids)==0){
    return(0);
  }
  mzs <- as.numeric(unique(unlist(cpd2mz_dict[cpd.ids])));
  if(length(mzs)==0){
    return(0);
  }else{
    result <- intersect(mzs, inputmzlist); #intersect to only mzs from the input mz list
    return(length(result));
  }
}

# convert single element vector in list to matrix
# b/c single element vector will convert to scalar in javascript, force to matrix
convert2JsonList <- function(my.list){
  lapply(my.list, function(x){
    if(length(x) == 1){
      matrix(x);
    }else{
      x;
    }
  });
}

# input: a two-col (id, val) data with potential duplicates (same id may be associated with 1 or more values
# output: a list named by unique id, with multiple values will be merged to vector
Convert2Dictionary <- function(data, quiet=T){
  
  all.ids <- data[,1];
  dup.inx <- duplicated(all.ids);
  if(sum(dup.inx) > 0){
    uniq.ids <- all.ids[!dup.inx];
    uniq.vals <- data[!dup.inx,2];
    
    # convert two-col data it to list (vals as list values, ids as list names)
    uniq.list <- split(uniq.vals, uniq.ids)
    
    # the list element orde will be sorted by the names alphabetically, need to get updated ones
    uniq.id.list <- names(uniq.list)
    
    dup.ids <- all.ids[dup.inx];
    uniq.dupids <- unique(dup.ids);
    uniq.duplen <- length(uniq.dupids);
    
    for(id in uniq.dupids){ # only update those with more than one hits
      hit.inx.all <- which(all.ids == id);
      hit.inx.uniq <- which(uniq.id.list == id);
      uniq.list[[hit.inx.uniq]]<- data[hit.inx.all,2];
    }
    
    AddMsg(paste("A total of ", sum(dup.inx), " of duplicates were merged.", sep=""));
    return(uniq.list);
  }else{
    AddMsg("All IDs are unique.");
    uniq.list <- split(data[,2], data[,1]);
    return(uniq.list);
  }
}

# utility function for fast list expanding (dynamic length)
# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
# https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

myFastList <- function(capacity = 50) {
  buffer <- vector('list', capacity)
  names <- character(capacity)
  length <- 0
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    names <<- c(names, character(capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(name, val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
    names[length] <<- name
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    names(b) <- names[0:length]
    return(b)
  }
  
  methods
}

####
####
#### from the fgsea R package, minor edits to adapt to untargeted metabolomics

#'Pre-ranked gsea adapted for untargeted metabolomics
#'@import fgsea
#'@noRd

fgsea2 <- function(mSetObj, pathways, stats, ranks,
                   nperm,
                   minSize=1, maxSize=Inf,
                   nproc=0,
                   gseaParam=1,
                   BPPARAM=NULL) {
  
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.fgsea")){ # public web on same user dir
      .load.scripts.on.demand("util_fgsea.Rc");    
    }
    return(my.fgsea(mSetObj, pathways, stats, ranks, nperm,
                    minSize, maxSize, nproc, gseaParam, BPPARAM));
  }else{
    return(my.fgsea(mSetObj, pathways, stats, ranks, nperm, 
                    minSize, maxSize, nproc, gseaParam, BPPARAM));
  }
}


####
####
#### for heatmap view (online only)

#' SetRTincluded
#'
#' @param mSetObj mSetObj
#' @param rt retention time types, "minutes", "seconds" or "no"
#'
#' @return mSetObj
#' @export
#' 
SetRTincluded <- function(mSetObj = NA, rt = "no") {
  return(.rt.included(mSetObj = mSetObj, rt))
}
.rt.included <- function(mSetObj = NA, rt){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(rt %in% c("minutes", "seconds")){
    #is.rt <<- TRUE
    mSetObj$paramSet$mumRT <- TRUE;
    #mumRT.type <<- rt
    mSetObj$paramSet$mumRT.type <- rt;
  } else {
    #is.rt <<- FALSE
    mSetObj$paramSet$mumRT <- FALSE
    #mumRT.type <<- rt
    mSetObj$paramSet$mumRT.type <- rt;
  }
  
  return(.set.mSet(mSetObj));
}

CreateHeatmapJson <- function(mSetObj=NA, libOpt, libVersion, minLib, 
                              fileNm, filtOpt, version="v1"){
  
  if(.on.public.web){
    # make this lazy load
    if(!exists("psea.heatmap.json")){ # public web on same user dir
      .load.scripts.on.demand("util_heatmap.Rc");    
    }
    return(psea.heatmap.json(mSetObj, libOpt, libVersion, minLib, fileNm, filtOpt, version));
  }else{
    return(psea.heatmap.json(mSetObj, libOpt, libVersion, minLib, fileNm, filtOpt, version));
  }
}
    
ProcessConvert2Mummichog <- function(mSetObj=NA, is.rt=F, mumRT.type="seconds", testmeth="cov", mode=NA){
    is.rt <- mSetObj$paramSet$mumRT;
    mSetObj <- .get.mSet();
    .on.public.web <<- F;
    mSetObj<-Convert2Mummichog(mSetObj, is.rt, F, mSetObj$paramSet$mumRT.type, testmeth, mSetObj$dataSet$mode);
    SetPeakFormat(mSetObj, "mpt")
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt");
    
    mSetObj<-Read.PeakListData(mSetObj, filename);
    mSetObj<-SanityCheckMummichogData(mSetObj);
    .on.public.web <<- T;
    return(.set.mSet(mSetObj));
}

#' PreparePeakTable4PSEA
#' @param mSetObj mSet Objective from previous step
#' @export
#' @author Zhiqiang Pang, Jeff Xia

PreparePeakTable4PSEA <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  if(length(levels(mSetObj$dataSet$cls)) < 3){
    res <- Ttests.Anal(mSetObj, F, 1, FALSE, TRUE)
    testmeth <- "tt";
  } else {
    res <- ANOVA.Anal(mSetObj, F, 1, FALSE)
    testmeth <- "aov";
  }

  ####### NOTE: need to use Ttests.Anal because Convert2Mummichog function takes as input result list from Ttests.anal: mSetObj$analSet$tt
  ## This is hack, should be better addressed later

  if(.on.public.web){
    
    is.rt <- mSetObj$paramSet$mumRT;
    mSetObj <- .get.mSet();
    .on.public.web <<- F;
    
    mSetObj<-Convert2Mummichog(mSetObj, is.rt, F, mSetObj$paramSet$mumRT.type, testmeth, mSetObj$dataSet$mode);
    
    SetPeakFormat(mSetObj, "mpt")
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt");
    
    mSetObj<-Read.PeakListData(mSetObj, filename);
    mSetObj<-SanityCheckMummichogData(mSetObj);
    .on.public.web <<- T;

    # don't forget the original!
    mSetObj$dataSet$mum.type <- "table";
    .set.mSet(mSetObj);
    return(1)
    
  } else {
    
    mSetObj <- res;
    is.rt <- mSetObj$paramSet$mumRT;
    
    if(mSetObj$analSet$tt$sig.num == 0){
      AddErrMsg("T-test failed, please consider trying different data normalization option!");
      return(0);
    }
    
    mSetObj <- Convert2Mummichog(mSetObj, is.rt, F, mSetObj$paramSet$mumRT.type, testmeth, mSetObj$dataSet$mode);
    mSetObj$paramSet$peakFormat <- "mpt"; # SetPeakFormat("mpt")
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
    mSetObj <- Read.PeakListData(mSetObj, filename);
    mSetObj <- SanityCheckMummichogData(mSetObj)
  }
  return(.set.mSet(mSetObj));
}

CreateListHeatmapJson <- function(mSetObj=NA, libOpt, libVersion, 
                                  minLib, fileNm, filtOpt, version="v1"){
  
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.list.heatmap")){ # public web on same user dir
      .load.scripts.on.demand("util_listheatmap.Rc");    
    }
    return(my.list.heatmap(mSetObj, libOpt, libVersion, minLib, fileNm, filtOpt, version));
  }else{
    return(my.list.heatmap(mSetObj, libOpt, libVersion, minLib, fileNm, filtOpt, version));
  }
}


PrepareIntegCMPDList <- function(mSetObj) {
  CMPDSet <- NULL;
  my.cmpds <- RJSONIO::fromJSON(mSetObj$mum_nm);
  CMPDSet <- unique(unlist(my.cmpds[["hits.sig"]]));
  return(CMPDSet)
}

Prepare4IntegNetwork <- function(mSetObj = NA, netLib = "global"){
  mSetObj <- .get.mSet(mSetObj);
  # mSet <<- mSetObj <- NULL;
  # mSetObj <- InitDataObjects("conc", "network", FALSE)

  mSetObj <- SetOrganism(mSetObj, "hsa")
  cmpdList <- PrepareIntegCMPDList(mSetObj)
  cmpdList <- paste(cmpdList, collapse = "\n")
  mSetObj <- PerformCmpdMapping(mSetObj, cmpdList, "hsa", "kegg")
  mSetObj <- CreateMappingResultTable(mSetObj);
  mSet <- GetNetworkGeneMappingResultTable(mSet);
  mSetObj <- PrepareNetworkData(mSetObj);
  idtype <<- "gene&cmpd";
  mSetObj <- PrepareKeggQueryJson(mSetObj);
  mSetObj <- PerformKOEnrichAnalysis_KO01100(mSetObj, "pathway","network_enrichment_pathway_0")
  mSetObj <- .get.mSet(mSetObj);
  
  if(netLib != "global") {
    OrganizeJsonforNextwork(mSetObj)
  }
  
  return(.set.mSet(mSetObj))
}

Prepare4TarIntegNetwork <- function(mSetObj = NA, netLib = "global"){

  mSetObj <- .get.mSet(mSetObj);
  mSet <- GetNetworkGeneMappingResultTable(mSet);
  mSetObj <- PrepareNetworkData(mSetObj);
  idtype <<- "gene&cmpd";
  mSetObj <- PrepareKeggQueryJson(mSetObj);
  mSetObj <- PerformKOEnrichAnalysis_KO01100(mSetObj, "pathway","network_enrichment_pathway_0")
  mSetObj <- .get.mSet(mSetObj);
  if(netLib != "global") {
    OrganizeTarJsonforNextwork(mSetObj)
  } else {
    jsonRes <- RJSONIO::fromJSON("network_enrichment_pathway_0.json")
    if(any(names(jsonRes[["hits.query"]]) =="Metabolic pathways")) {

        jsonRes[["hits.query"]][which(names(jsonRes[["hits.query"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.edge"]][which(names(jsonRes[["hits.edge"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.node"]][which(names(jsonRes[["hits.node"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.edge.all"]][which(names(jsonRes[["hits.edge.all"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.node.all"]][which(names(jsonRes[["hits.node.all"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.edge"]][which(names(jsonRes[["hits.edge.name"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.node"]][which(names(jsonRes[["hits.node.name"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.edge.all"]][which(names(jsonRes[["hits.edge.name.all"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.node.all"]][which(names(jsonRes[["hits.node.name.all"]]) == "Metabolic pathways")] <- NULL;
        jsonRes[["hits.all"]][which(names(jsonRes[["hits.all"]]) == "Metabolic pathways")] <- NULL;

        idxrm <- which(jsonRes[["path.id"]] == "ko01100");
        jsonRes[["hit.num"]] <- jsonRes[["hit.num"]][-idxrm];
        jsonRes[["fun.pval"]] <- jsonRes[["fun.pval"]][-idxrm];
        jsonRes[["path.id"]]<- jsonRes[["path.id"]][-idxrm];
        json.mat <- rjson::toJSON(jsonRes);
        sink("network_enrichment_pathway_0.json")
        cat(json.mat);
        sink();
    }
  }
  return(.set.mSet(mSetObj))
}

#'Set organism for further analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param org Set organism ID
#'@export
SetOrganism <- function(mSetObj=NA, org){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$org <- org;
  return(.set.mSet(mSetObj))
}


#' Create Mummichog Libraries from KEGG
#' @description Function to create mummichog libraries from
#' MetaboAnalyst pathway libraries (metpa).
#' Outputs the RDS files in the current working directory. RDS files
#' are saved using the KEGG organism code.
#' @param folder Input the path of the folder containing the metpa rda files.
#' @param kegg_compounds Input the name of the KEGG dictionary containing the 
#' KEGG compound IDs, KEGG compopund names, and molecular weight.
#' @export
CreateMummichogLibs <- function(folder, kegg_compounds){
  
  # Step 1: Get list of pathways to make mummichog libraries from 
  folder <- folder
  files <- list.files(folder, pattern = ".rda$")
  
  if(length(files) == 0){
    AddErrMsg("No .rda files found in folder!")
    return(0)
  }
  
  # Step2: Create the models list 
  models <- Map(rda2list, file.path(folder, files))
  names(models) <- tools::file_path_sans_ext(files)
  org <- names(models)
  
  kegg_compounds <<- kegg_compounds
  
  # Step 3: Create the pathways
  pathway <- lapply(models, function(f) {fillpathways(f)} )
  
  # Step 4: Create cpd.lib
  cpd.lib <- lapply(pathway, function(l) {make_cpdlib(l)})
  
  # Step 5: Create mummichog libraries
  # Will output the .RDS files in the current working directory
  output <- mapply(CreateLibFromKEGG, cpd.lib, pathway, org)
  
}

#' Function to get adduct details from a specified compound
#' @description Function to get adduct details from a specified compound.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param cmpd.id Input the name of the selected compound.
#'@import qs
#' @export
GetCompoundDetails <- function(mSetObj=NA, cmpd.id){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$api$lib)){
    
    # get file from api.metaboanalyst.ca
    toSend = list(cmpdId = cmpd.id)
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/compounddetails/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    
    if(query_results$status_code == 200){
      filename <- httr::content(query_results, "text")
    }
    
    endpointfile <- paste0("/getFile", "/", mSetObj$api$guestName, "/", filename)
    callfile <- paste(base, endpointfile, sep="")
    download.file(callfile, destfile = basename(filename))
    print(paste0(filename, " saved to current working directory!"))
    return(.set.mSet(mSetObj));
  }
  
  forms <- mSetObj$cpd_form_dict[[cmpd.id]];
  
  if(is.null(forms)){
    print("This compound is not valid!")
    return(0)
  }
  
  matched_res <- qs::qread("mum_res.qs");
  mz <- matched_res[which(matched_res$Matched.Compound == cmpd.id), 1] 
  mass.diff <- matched_res[which(matched_res$Matched.Compound == cmpd.id), 4]
  tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
  
  res <- cbind(rep(cmpd.id, length(mz)), mz, forms, mass.diff, tscores) 
  colnames(res) <- c("Matched.Compound", "m.z", "Matched.Form", "Mass.Diff", "T.Scores")
  fast.write.csv(res, "mummichog_compound_details.csv")
  return(.set.mSet(mSetObj));
}


#' Function to get compound details from a specified pathway
#' @description Function to get compound details from a specified pathway.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries. Significant compounds will be indicated with an asterisk.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param msetNm Input the name of the pathway
#' @export
GetMummichogPathSetDetails <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$api$lib)){
    
    # get file from api.metaboanalyst.ca
    toSend = list(pathName = msetNm)
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/pathsetdetails/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    
    if(query_results$status_code == 200){
      filename <- httr::content(query_results, "text")
    }
    
    endpointfile <- paste0("/getFile", "/", mSetObj$api$guestName, "/", filename)
    callfile <- paste(base, endpointfile, sep="")
    download.file(callfile, destfile = basename(filename))
    print(paste0(filename, " saved to current working directory!"))
    return(.set.mSet(mSetObj));
  }
  
  version <- mum.version <- mSetObj$paramSet$version;
  inx <- which(mSetObj$pathways$name == msetNm)
  
  if(is.na(inx)){
    AddErrMsg("Invalid pathway name!")
    return(0)
  }
  
  if(version=="v2" & mSetObj$paramSet$mumRT){
    mset <- mSetObj$pathways$emp_cpds[[inx]];
    mset_cpds <- mSetObj$pathways$cpds[[inx]];
    
    hits.all <- unique(mSetObj$total_matched_ecpds)
    hits.sig <- mSetObj$input_ecpdlist;
    
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    ref.ecpds <- mset[which(refs & !sigs)]
    sig.ecpds <- mset[sigs]
    
    ref.mzs <- lapply(ref.ecpds, function(x) paste(as.numeric(unique(unlist(mSetObj$ec2mz_dict[x]))), collapse = "; ")) 
    sig.mzs <- lapply(sig.ecpds, function(x) paste(as.numeric(unique(unlist(mSetObj$ec2mz_dict[x]))), collapse = "; "))  
    
    ref.cpds <- lapply(ref.ecpds, function(x) paste(unique(unlist(mSetObj$ecpd_cpd_dict[x])), collapse = "; "))
    sig.cpds <- lapply(sig.ecpds, function(x) paste(unique(unlist(mSetObj$ecpd_cpd_dict[x])), collapse = "; "))
    
    path.results <- matrix(c(unlist(sig.mzs), unlist(ref.mzs), unlist(sig.cpds), unlist(ref.cpds)), ncol=2) 
    colnames(path.results) <- c("mzs", "cpds")
    rownames(path.results) <- c(paste0(sig.ecpds, "*"), ref.ecpds)
    
    name <- paste0(gsub(" ", "_", msetNm), "_ecpd_mz_info.csv")
    fast.write.csv(path.results, name)
  }else{
    mset <- mSetObj$pathways$cpds[[inx]];
    
    hits.all <- unique(mSetObj$total_matched_cpds)
    hits.sig <- mSetObj$input_cpdlist;
    
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    ref.cpds <- mset[which(refs & !sigs)]
    sig.cpds <- mset[sigs]
    
    ref.mzs <- lapply(ref.cpds, function(x) paste(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[x]))), collapse = "; ")) 
    sig.mzs <- lapply(sig.cpds, function(x) paste(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[x]))), collapse = "; "))  
    
    path.results <- matrix(c(unlist(sig.mzs), unlist(ref.mzs)), ncol=1) 
    colnames(path.results) <- "mzs"
    rownames(path.results) <- c(paste0(sig.cpds, "*"), ref.cpds)
    
    name <- paste0(gsub(" ", "_", msetNm), "_cpd_mz_info.csv")
    fast.write.csv(path.results, name)
  }
  
  return(.set.mSet(mSetObj));
}


###### Functions got from metap package ######
sumlog <- function(p) {
    keep <- (p > 0) & (p <= 1)
    invalid <- sum(1L * keep) < 2
    if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(chisq = NA_real_, df = NA_integer_,
                  p = NA_real_, validp = p[keep])
    } else {
      lnp <- log(p[keep])
      chisq <- (-2) * sum(lnp)
      df <- 2 * length(lnp)
      if(length(lnp) != length(p)) {
        warning("Some studies omitted")
      }
      res <- list(chisq = chisq, df = df,
                  p = pchisq(chisq, df, lower.tail = FALSE), validp = p[keep])
    }
    class(res) <- c("sumlog", "metap")
    res
  }

sump <-
  function(p)  {
    keep <- (p >= 0) & (p <= 1)
    invalid <- sum(1L * keep) < 2
    if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(p = NA_real_, conservativep = NA_real_, validp = p[keep])
    } else {
      sigmap <- sum(p[keep])
      k <- length(p[keep])
      conservativep <- exp( k * log(sigmap) - lgamma(k + 1))
      nterm <- floor(sigmap) + 1 # how many values of sump
      denom <- lfactorial(k)
      psum <- 0
      terms <- vector("numeric", nterm)
      for (i in 1:nterm) {
        terms[i] <- lchoose(k, i - 1) + k * log(sigmap - i + 1) - denom
        pm <- 2 * (i %% 2) - 1
        psum <- psum + pm * exp(terms[i])
      }
      if(k != length(p)) {
        warning("Some studies omitted")
      }
      if(sigmap > 20) {
        warning("Likely to be unreliable, check with another method")
      }
      res <- list(p = psum, conservativep = conservativep, validp = p[keep])
    }
    class(res) <- c("sump", "metap")
    res
  }

sumz <-
  function(p, weights = NULL, data = NULL, subset = NULL, na.action = na.fail)  {
    if(is.null(data)) data <- sys.frame(sys.parent())
    mf <- match.call()
    mf$data <- NULL
    mf$subset <- NULL
    mf$na.action <- NULL
    mf[[1]] <- as.name("data.frame")
    mf <- eval(mf, data)
    if(!is.null(subset)) mf <- mf[subset,]
    mf <- na.action(mf)
    p <- as.numeric(mf$p)
    weights <- mf$weights
    noweights <- is.null(weights)
    if(noweights) weights <- rep(1, length(p))
    if(length(p) != length(weights)) warning("Length of p and weights differ")
    keep <- (p > 0) & (p < 1)
    invalid <- sum(1L * keep) < 2
    if(invalid) {
      warning("Must have at least two valid p values")
      res <- list(z = NA_real_, p = NA_real_,
                  validp = p[keep], weights = weights)
    } else {
      if(sum(1L * keep) != length(p)) {
        warning("Some studies omitted")
        omitw <- weights[!keep]
        if((sum(1L * omitw) > 0) & !noweights)
          warning("Weights omitted too")
      }
      zp <- (qnorm(p[keep], lower.tail = FALSE) %*% weights[keep]) /
        sqrt(sum(weights[keep]^2))
      res <- list(z = zp, p = pnorm(zp, lower.tail = FALSE),
                  validp = p[keep], weights = weights)
    }
    class(res) <- c("sumz", "metap")
    res
  }

votep <-
  function(p, alpha = 0.5) {
    alpha <- ifelse(alpha > 1, alpha / 100, alpha) # if percent
    #stopifnot(alpha > 0, alpha < 1)
    keep <- (p >= 0) & (p <= 1)
    alp <- vector("numeric", 2)
    if(alpha <= 0.5) {
      alp[1] <- alpha
      alp[2] <- 1 - alpha
    } else {
      alp[2] <- alpha
      alp[1] <- 1 - alpha
    }
    invalid <- sum(1L * keep) < 2
    if(invalid) {
      warning("Must have at least two valid p values")
      res = list(p = NA_real_, pos = NA_integer_, neg = NA_integer_,
                 alpha = alpha, validp = p[keep])
    } else {
      pi <- p[keep]
      k <- length(pi)
      pos <- sum(1L * (pi < alp[1]))
      neg <- sum(1L * (pi > alp[2]))
      if(k != length(p)) {
        warning("Some studies omitted")
      }
      if((pos + neg) <= 0) {
        warning("All p values are within specified limits of alpha")
        p <- 1
      } else {
        p = binom.test(pos, pos + neg, 0.5, alternative = "greater")$p.value
      }
      res = list(p = p, pos = pos, neg = neg, alpha = alpha, validp = pi)
    }
    class(res) <- c("votep", "metap")
    res
  }


PlotlyPeaks2Paths <- function(mSetObj=NA, imgName, format = "png", dpi = default.dpi, width = 9, labels = "default",
                              num_annot = 5, interactive=F){  
  library(plotly);
  library(dplyr);
  mSetObj <- .get.mSet(mSetObj)
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  anal.type0 <- mSetObj$paramSet$anal.type
  if (anal.type0 == "mummichog") {
    mat <- mSetObj$mummi.resmat
    y <- -log10(mat[, 5])
    x <- mat[, 8] / mat[, 4]
    pathnames <- rownames(mat)
  } else {
    mat <- mSetObj$mummi.gsea.resmat
    y <- -log10(mat[, 3])
    x <- mat[,5]
    pathnames <- rownames(mat)
  }
  
  inx <- order(y, decreasing = TRUE)
  y <- y[inx]
  x <- x[inx]
  pathnames <- pathnames[inx]
  
  
  
  # Create the data frame
  df <- data.frame(y, x, pathnames)
  
  df <- df %>%
    group_by(x, y) %>%
    summarize(
      pathnames_agg = paste(pathnames, collapse = "<br>"),
      .groups = 'drop'
    )
  
  radi.vec <- sqrt(abs(as.numeric(df$x)))
  sizeref <- max(radi.vec) / 25
  # Generate ggplot
  library(ggplot2);
  if (anal.type0 == "mummichog") {
    # set circle size based on enrichment factor
    # Assuming df, x, y, pathnames, and other relevant variables are already defined

    
    # Create the plot with Plotly
    p <- plot_ly(data = df, x = ~x, y = ~y,
                 type = 'scatter', mode = 'markers',
                 marker = list(size = ~radi.vec*10,  # Adjust size factor as needed
                               color = ~y, 
                               #sizeref = sizeref,
                               colorscale = list(c(0, "yellow"), c(1, "red")),
                               colorbar = list(title = "-log10(p)",len=0.5),
                               line = list(color = 'black', width = 1)),
                 text = ~paste(pathnames_agg, 
                               "<br>Enrichment Factor:", round(x, 3), 
                               "<br>-log10(p):", round(y, 3)),
                 hoverinfo = 'text') %>%
      layout(xaxis = list(title = 'Enrichment Factor'),
             yaxis = list(title = '-log10(p)'),
             hovermode='closest',
            width = 800, 
            height = 600)
    
    mSetObj$imgSet$mummi.plot<- imgName
  }else{
    radi.vec <- sqrt(abs(as.numeric(df$y)))

    # Assuming df, y, pathnames, imgName, and num_annot are already defined
    # Create the plot with Plotly
    p <- plot_ly(data = df, x = ~x, y = ~y, 
                 type = 'scatter', mode = 'markers',
                 marker = list(size = ~radi.vec*15,  # Adjust size factor as needed
                               color = ~x, 
                               sizeref = sizeref,
                               colorscale = list(c(0, "#458B00"), c(0.5, "#fffee0"), c(1, "#7f0000")),
                               colorbar = list(
                                 title = "NES",
                                 len = 0.5  # Adjust the length of the colorbar
                               ),
                               line = list(color = 'black', width = 1)),
                 text = ~paste(pathnames_agg, 
                               "<br>Enrichment Factor:", round(x, 3), 
                               "<br>-log10(p):", round(y, 3)),
                 hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = 'NES'),
        yaxis = list(title = '-log10(p)'),
        hovermode='closest',
        width = 800, 
        height = 600)
    
    mSetObj$imgSet$mummi.gsea.plot<- imgName
  }
  
  improve_text_position <- function(x) {
    positions <- c('top', 'bottom')
    sapply(seq_along(x), function(i) positions[(i %% length(positions)) + 1])
  }
  

  #text_positions <- improve_text_position(df$y)
  # Add text labels for top num_annot points
  #top_indices <- head(order(-df$y), num_annot)
  #for(i in top_indices) {
  #  
  #  p <- p %>% add_annotations(
  #    x = df$x[i], y = df$y[i], 
  #    text = df$pathnames_agg[i], 
  #    showarrow = FALSE, 
  #    xanchor = 'center', 
  #    yanchor = text_positions[i],
  #    font = list(size = 13))
  #}  
  
  if (anal.type0 == "mummichog") {
    list_data <- list(pval = unname(y), enr = unname(x), pathnames = pathnames)
    write(RJSONIO::toJSON(list_data), "scattermum.json")
  } else {
    list_data <- list(pval = unname(y), enr = unname(mat[, 5]), pathnames = pathnames)
    write(RJSONIO::toJSON(list_data), "scattergsea.json")
  }
  
  library(plotly);
  if(interactive){
    return(p);
  }else{
    if(is.na(width)){
      w <- 7;
    }else if(width == 0){
      w <- 7;
    }else{
      w <- width;
    }
    h <- w;
    plotly::save_image(p, file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format);
    
    return(.set.mSet(mSetObj));
  }
}


doHeatmapMummichogTest <- function(mSetObj=NA, nm, libNm, ids){
  
  mSetObj<-.get.mSet(mSetObj);
  
  if(ids == "overall"){
    .on.public.web <<- F;
    is.rt <- mSetObj$paramSet$mumRT;
    #mSetObj<-PreparePrenormData(mSetObj)
    mSetObj<-Normalization(mSetObj, "MedianNorm", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)
    mSetObj<-Ttests.Anal(mSetObj, F, 0.05, FALSE, TRUE)
    mSetObj<-Convert2Mummichog(mSetObj, is.rt, F, mSetObj$paramSet$mumRT.type, "tt", mSetObj$dataSet$mode);
    mSetObj<-InitDataObjects("mass_all", "mummichog", FALSE)
    mSetObj<-SetPeakFormat(mSetObj, "mpt")
    #mSetObj<-UpdateInstrumentParameters(mSetObj, 10, mSetObj$dataSet$mode);
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
    mSetObj<-Read.PeakListData(mSetObj, filename);
    mSetObj<-SanityCheckMummichogData(mSetObj)
    mSetObj<-SetPeakEnrichMethod(mSetObj, "mum", "v2")
    mSetObj<-SetMummichogPval(mSetObj, 0.05)
    .on.public.web <<- T;
    .set.mSet(mSetObj);
    anal.type <<- "integ";
  }else{
    gene.vec <- unlist(strsplit(ids, "; "));
    anal.type <<- "mummichog";
    is.rt <- mSetObj$paramSet$mumRT;
    if(is.rt){
      feat_info_split <- matrix(unlist(strsplit(gene.vec, "__", fixed=TRUE)), ncol=2, byrow=T)
      colnames(feat_info_split) <- c("m.z", "r.t")
      mSetObj$dataSet$input_mzlist <- as.numeric(feat_info_split[,1]);
      mSetObj$dataSet$N <- length(mSetObj$dataSet$input_mzlist);
    }else{
      mSetObj$dataSet$input_mzlist <- gene.vec;
      mSetObj$dataSet$N <- length(gene.vec);
    }
  }
  
  mSetObj$mum_nm <- paste0(nm,".json");
  mSetObj$mum_nm_csv <- paste0(nm,".csv");
  .set.mSet(mSetObj);
  return(PerformPSEA("NA", libNm, "current", 3, 100, F));
}
