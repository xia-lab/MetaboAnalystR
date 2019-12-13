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

#'Constructor to read uploaded user files into the mummichog object
#'@description This function handles reading in CSV or TXT files and filling in the mSet object
#' for mummichog analysis. It makes sure that all necessary columns are present.
#'@usage Read.PeakListData(mSetObj=NA, filename = NA)
#'@param mSetObj Input the name of the created mSetObj.
#'@param filename Input the path name for the CSV/TXT files to read.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
Read.PeakListData <- function(mSetObj=NA, filename = NA) {
  
  mSetObj <- .get.mSet(mSetObj);
  mumDataContainsPval = 1; #whether initial data contains pval or not
  
  input <- as.data.frame(read.table(filename, header=TRUE, as.is=TRUE));
  
  if(peakFormat == "mpt"){
    hits <- c("m.z", "p.value", "t.score") %in% colnames(input);
    if(!all(hits)){
      AddErrMsg("Missing information, data must contain 'm.z', 'p.value' and 't.score' column");
      return(0);
    }
  }else if(peakFormat == "mp"){
    hit <- c("m.z", "p.value") %in% colnames(input);
    if(!all(hit)){
      AddErrMsg("Missing information, data must contain both 'm.z' and 'p.value' column");
      return(0);
    }
  }else if(peakFormat == "mt"){
    hits <- c("m.z", "t.score") %in% colnames(input);
    if(!all(hits)){
      AddErrMsg("Missing information, data must contain both 'm.z' and 't.score' column");
      return(0);
    }
  }else{
    hits <- "m.z" %in% colnames(input);
    if(!all(hits)){
      AddErrMsg("Missing information, data must contain at least a 'm.z' column");
      return(0);
    }
  }
  
  hit <- "m.z" %in% colnames(input);
  if(!hit){
    AddErrMsg("Missing information, data must contain a 'm.z' column");
    return(0);
  }
  
  mSetObj$dataSet$mummi.raw = input
  
  if(length(colnames(input)) == 2){
    if(!"p.value" %in% colnames(input)){
      mumDataContainsPval = 0;
      input[,'p.value'] = rep(0, length=nrow(input))
    }else{
      input[,'t.score'] = rep(0, length=nrow(input))
    }
  }
  
  if(length(colnames(input)) == 1){
    mumDataContainsPval = 0;
    input[,'p.value'] = rep(0, length=nrow(input))
    input[,'t.score'] = rep(0, length=nrow(input))
  }
  
  mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score);
  colnames(mSetObj$dataSet$mummi.orig) = c("p.value", "m.z", "t.score")
  if(mSetObj$dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx <- rep(TRUE, nrow(mSetObj$dataSet$mummi.orig))
  }else if(mSetObj$dataSet$mode == "negative"){
    mSetObj$dataSet$pos_inx <- rep(FALSE, nrow(mSetObj$dataSet$mummi.orig) )
  }else{
    mSetObj$dataSet$pos_inx <- input$mode == "positive"
  }
  mSetObj$dataSet$mumType = "list";
  mSetObj$msgSet$read.msg <- paste("A total of", length(input$p.value), "m/z features were found in your uploaded data.");
  mumDataContainsPval <<- mumDataContainsPval;
  return(.set.mSet(mSetObj));
  
}

#'Set the peak format for the mummichog analysis
#'@description Set the peak format for mummichog analysis.
#'@param mSetObj Input the name of the created mSetObj.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetPeakFormat <-function(type){
  peakFormat <<- type;
}

#'Convert mSetObj to proper format for MS Peaks to Pathways module
#'@description Following t-test analysis, this functions converts the results from the mSetObj 
#'to the proper format for mummichog analysis
#'@param mSetObj Input the name of the created mSetObj.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Convert2Mummichog <- function(mSetObj=NA, rt=FALSE){
  
  tt.pval <- mSetObj$analSet$tt$p.value
  fdr <- p.adjust(tt.pval, "fdr")
  mz.pval <- names(tt.pval)
  pvals <- cbind(as.numeric(mz.pval), as.numeric(fdr))
  colnames(pvals) <- c("m.z", "p.value")
  
  tt.tsc <- mSetObj$analSet$tt$t.score
  mz.tsc <- names(tt.tsc)
  tscores <- cbind(as.numeric(mz.tsc), as.numeric(tt.tsc))
  colnames(tscores) <- c("m.z", "t.score")
  
  if(rt == TRUE & !.on.public.web){
    camera_output <- readRDS("annotated_peaklist.rds")
    mz.cam <- round(camera_output$mz, 5) 
    rt.cam <- round(camera_output$rt, 5) 
    camera <- cbind(mz.cam, rt.cam)
    colnames(camera) <- c("m.z", "r.t")
    mummi_new <- Reduce(function(x,y) merge(x,y,by="m.z", all = TRUE), list(pvals, tscores, camera))
    complete.inx <- complete.cases(mummi_new[,c("p.value", "t.score")]) # filter out m/zs without pval and tscore
    mummi_new <- mummi_new[complete.inx,]
  }else{
    mummi_new <- merge(pvals, tscores)
  }
  
  filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
  
  write.table(mummi_new, filename, row.names = FALSE)
  
  return(.set.mSet(mSetObj))
}

#'Update the mSetObj with user-selected parameters for MS Peaks to Pathways.
#'@description This functions handles updating the mSet object for mummichog analysis. It is necessary to utilize this function
#'to specify to the organism's pathways to use (libOpt), the mass-spec mode (msModeOpt) and mass-spec instrument (instrumentOpt).
#'@usage UpdateInstrumentParameters(mSetObj=NA, instrumentOpt, msModeOpt, custom=FALSE)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param instrumentOpt Define the mass-spec instrument used to perform untargeted metabolomics.
#'@param msModeOpt Define the mass-spec mode of the instrument used to perform untargeted metabolomics.
#'@param custom Logical, select adducts for mummichog to consider.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdateInstrumentParameters <- function(mSetObj=NA, instrumentOpt, msModeOpt, custom = FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$instrument <- instrumentOpt;
  mSetObj$dataSet$mode <- msModeOpt;
  mSetObj$custom <- custom;
  
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
#'@export

SanityCheckMummichogData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$mumType == "table"){
    
    l = sapply(colnames(mSetObj$dataSet$orig),function(x) return(unname(strsplit(x,"/")[[1]][1])))
    colnames(mSetObj$dataSet$orig) = l;
    
    if(.on.public.web){
      return(SanityCheckData(NA));
    }else{
      mSetObj <- SanityCheckData(mSetObj)
      return(.set.mSet(mSetObj));
    }
  }
  
  msg.vec <- NULL;
  mSetObj$mum_nm <- "mummichog_query.json"
  mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment.csv"
  ndat <- mSetObj$dataSet$mummi.orig;
  pos_inx = mSetObj$dataSet$pos_inx
  ndat <- cbind(ndat, pos_inx)
  
  rawdat <- mSetObj$dataSet$mummi.raw
  
  read.msg <- mSetObj$msgSet$read.msg
  
  # sort mzs by p-value
  ord.inx <- order(ndat[,1]);
  ndat <- ndat[ord.inx,]; # order by p-vals
  
  
  # filter based on mz
  mznew <- ndat[,2];
  # trim to fit within 50 - 2000
  my.inx <- mznew > 50 & mznew < 2001;
  trim.num <- sum(!my.inx);
  range = paste0(min(ndat[,1], "-", max(ndat[,1])))
  msg.vec <- c(msg.vec, paste("The instrument's mass accuracy is <b>", mSetObj$dataSet$instrument , "</b> ppm."));
  msg.vec <- c(msg.vec, paste("The instrument's analytical mode is <b>", mSetObj$dataSet$mode , "</b>."));
  msg.vec <- c(msg.vec, paste("The uploaded data contains <b>", length(colnames(rawdat)), "</b> columns."));
  
  if(peakFormat == "rmp"){
    msg.vec <- c(msg.vec, paste("The peaks are ranked by <b>p-values</b>."));
  }else if(peakFormat == "rmt"){
    msg.vec <- c(msg.vec, paste("The peaks are ranked by <b>t-scores</b>."));
  }
  
  msg.vec <- c(msg.vec, paste("The column headers of uploaded data are <b>", paste(colnames(rawdat),collapse=", "), "</b>"));
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
  
  ref_mzlist <- ndat[,2];
  
  # set up expression (up/dn)
  tscores <- as.numeric(ndat[,3]);
  names(tscores) <- ref_mzlist;
  
  ref.size <- length(ref_mzlist);
  
  msg.vec <- c(msg.vec, paste("A total of ", ref.size, "input mz features were retained for further analysis."));
  
  if(ref.size > 20000){
    msg.vec <- c(msg.vec, "There are too many input features, the performance may be too slow.");
  }
  if(min(ndat[,"p.value"])<0 || max(ndat[,"p.value"])>1){
    msg.vec <- c(msg.vec, "Please make sure the p-values are between 0 and 1.");
  }
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$check.msg, read.msg, msg.vec);
  mSetObj$dataSet$mummi.proc <- ndat;
  mSetObj$dataSet$expr_dic <- tscores;
  mSetObj$dataSet$ref_mzlist <- ref_mzlist;
  mSetObj$dataSet$pos_inx <- ndat[,4] == 1;
  
  return(.set.mSet(mSetObj));
  
}

#'Set the cutoff for mummichog analysis
#'@description Set the p-value cutoff for mummichog analysis.
#'@param mSetObj Input the name of the created mSetObj.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetMummichogPvalFromPercent <- function(mSetObj=NA, fraction){
  fraction = pct/100
  mSetObj <- .get.mSet(mSetObj);
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
  return(SetMummichogPval("NA", maxp));
  
}

#'Set the cutoff for mummichog analysis
#'@description Set the p-value cutoff for mummichog analysis.
#'@param mSetObj Input the name of the created mSetObj.
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
  if(mumDataContainsPval == 1){
    my.inx <- ndat[,1] < cutoff;
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
  
  if(sig.part > 25){
    msg.vec <- c(msg.vec, paste("<font color=\"orange\">Warning: over", sig.part, "percent were significant based on your cutoff</font>."));
    msg.vec <- c(msg.vec, "You should adjust p-value cutoff to control the percentage");
  }else{
    msg.vec <- c(msg.vec, paste("A total of", sig.size, "or", sig.part, "percent signficant mz features were found based on the selected p-value cutoff:", cutoff));
  }
  
  if(sig.size > 2000){
    msg.vec <- c(msg.vec, "There are too many significant features based on the current cutoff, possibly too slow.");
  }else if(sig.size == 0){
    AddErrMsg("No significant features were found based on the current cutoff! Failed to perform analysis.");
    return(0);
  }else if(sig.size < 30){
    msg.vec <- c(msg.vec, "The number of significant features is small based on the current cutoff, possibly not accurate.");
  }
  
  mSetObj$dataSet$input_mzlist <- input_mzlist;
  mSetObj$dataSet$N <- sig.size;
  return(.set.mSet(mSetObj));
}

#'Function to perform peak set enrichment analysis
#'@description This is the main function that performs either the mummichog
#'algorithm, GSEA, or both for peak set enrichment analysis. 
#'@usage PerformPSEA(mSetObj=NA, lib, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object 
#'@param lib Input the name of the organism library, default is hsa 
#'@param permNum Numeric, the number of permutations to perform
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformPSEA <- function(mSetObj=NA, lib, permNum = 100){
  
  mSetObj <- .get.mSet(mSetObj);
  
  filenm <- paste(lib, ".rds", sep="")
  biocyc <- grepl("biocyc", lib)
  
  if(!is.null(mSetObj$curr.cust)){
    
    if(biocyc){
      user.curr <- mSetObj$curr.map$BioCyc
    }else{
      user.curr <- mSetObj$curr.map$KEGG
    }
    
    currency <<- user.curr
    
    if(length(currency)>0){
      mSetObj$mummi$anal.msg <- c("Currency metabolites were successfully uploaded!")
    }else{
      mSetObj$mummi$anal.msg <- c("Errors in currency metabolites uploading!")
    }
  }
  
  if(.on.public.web){
    mummichog.lib <- readRDS(paste("../../libs/mummichog/", filenm, sep=""));
  }else{
    if(!file.exists(filenm)){
      mum.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", filenm, sep="")
      download.file(mum.url, destfile = filenm, method="libcurl", mode = "wb")
      mummichog.lib <- readRDS(filenm);
    }else{
      mummichog.lib <- readRDS(filenm);
    }
  }
  
  if(!is.null(mSetObj$adduct.custom)){
    mw <- mummichog.lib$cpd.lib$mw
    new_adducts <- new_adduct_mzlist(mSetObj, mw)
    
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
  
  cpd.treep <- mummichog.lib$cpd.tree[["positive"]];
  cpd.treen <- mummichog.lib$cpd.tree[["negative"]];
  
  mSetObj$pathways <- mummichog.lib$pathways;
  mSetObj$lib.organism <- lib; #keep track of lib organism for sweave report
  mSetObj <- .search.compoundLib(mSetObj, cpd.lib, cpd.treep, cpd.treen);
  
  if(anal.type == "mummichog"){
    mSetObj <- .perform.mummichogPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogSigPvals(mSetObj);
  }else if(anal.type == "gsea_peaks"){
    mSetObj <- .compute.mummichog.fgsea(mSetObj, permNum);
  }else{
    # need to perform mummichog + gsea then combine p-values
    mSetObj <- .compute.mummichog.fgsea(mSetObj, permNum);
    mSetObj <- .perform.mummichogPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogSigPvals(mSetObj);
    
    ora.all <- mSetObj$mummi.resmat
    gsea.all <- mSetObj$mummi.gsea.resmat
    
    ora.pval <- ora.all[,5]
    ora.paths <- names(ora.pval)
    ora.hits <- ora.all[,1:3]
    ora.mat <- cbind(ora.paths, ora.hits, ora.pval)
    
    gsea.pval <- gsea.all[,3]
    gsea.paths <- names(gsea.pval)
    gsea.mat <- cbind(gsea.paths, gsea.pval)
    
    # gsea paths is bigger, need to subset 
    matched_paths <- na.omit(match(ora.paths, gsea.paths))
    matched_gsea <- gsea.mat[matched_paths,]
    mum.matrix <- cbind(ora.mat, matched_gsea)
    mum.df <- data.frame(pathways = mum.matrix[,1], path.size = as.numeric(mum.matrix[,2]),
                         hits.all = as.numeric(mum.matrix[,3]), hits.sig = as.numeric(mum.matrix[,4]),
                         mummichog = as.numeric(mum.matrix[,5]), gsea = as.numeric(mum.matrix[,7]))
    
    # combine p-values
    combo.all <- apply(mum.df[,c("mummichog", "gsea")], 1, function(x) sumlog(x))
    
    #extract p-values
    all.ps <- unlist(lapply(combo.all, function(z) z["p"]))
    df.combo <- as.matrix(mum.df[,2:6])
    dfcombo <- round(cbind(df.combo, all.ps), 5)
    colnames(dfcombo) <- c("Total_Size", "Hits", "Sig_Hits", "Mummichog_Pvals", "GSEA_Pvals", "Combined_Pvals")
    ord.inx <- order(dfcombo[,6]);
    dfcombo <- signif(as.matrix(dfcombo[ord.inx, ]), 4);
    write.csv(dfcombo, "mummichog_integ_pathway_enrichment.csv", row.names = TRUE)
    mSetObj$integ.resmat <- dfcombo
    
    matched_cpds <- names(mSetObj$cpd_exp)
    inx2<-na.omit(match(mum.df$pathways[ord.inx], mSetObj$pathways$name))
    filt_cpds <- lapply(inx2, function(f) { mSetObj$pathways$cpds[f] })
    
    cpds <- lapply(filt_cpds, function(x) intersect(unlist(x), matched_cpds))
    
    mSetObj$path.nms <- mum.matrix[ord.inx,1]
    mSetObj$path.hits <- convert2JsonList(cpds)
    mSetObj$path.pval <- as.numeric(dfcombo[,6])
    
    
    json.res <- list(
      cmpd.exp = mSetObj$cpd_exp,
      path.nms = mum.matrix[ord.inx,1],
      hits.all = convert2JsonList(cpds),
      hits.sig = convert2JsonList(cpds),
      mum.p = as.numeric(dfcombo[,4]),
      gsea.p = as.numeric(dfcombo[,5]),
      comb.p = as.numeric(dfcombo[,6])
    );
    
    json.mat <- RJSONIO::toJSON(json.res, .na='null');
    sink(mSetObj$mum_nm);
    cat(json.mat);
    sink();
  }
  
  mummichog.lib <- NULL;
  mum = list(
    path.nms = mSetObj$path.nms,
    path.hits = mSetObj$path.hits,
    path.pval = mSetObj$path.pval,
    cmpds = mSetObj$cpd_exp,
    peakToMet = mSetObj$cpd_form_dict,
    peakMetTable = mSetObj$dataSet$mumResTable,
    expr = mSetObj$cpd_exp_dict
  );
  json.mat <- RJSONIO::toJSON(mum, .na='null');
  sink("metaboanalyst_peaks.json");
  cat(json.mat);
  sink();
  return(.set.mSet(mSetObj));
}

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
  curr_db <- .read.metaboanalyst.lib("currency_cmpd.rds");
  hit.inx <- match(tolower(qvec), tolower(curr_db$Common.Name));
  match.values <- curr_db[hit.inx,];
  curr.met <- nrow(match.values)
  
  mSetObj$curr.map <- match.values
  
  if(curr.met > 0){
    mSetObj$mummi$curr.msg <- paste("A total of ", curr.met ," currency metabolites were successfully uploaded!", sep = "")
  }else{
    mSetObj$mummi$curr.msg <- c("No currency metabolites were selected!")
  }
  
  mSetObj$curr.cust <- TRUE;
  return(.set.mSet(mSetObj));
}

#'Read Adduct List
#'@description This function reads in the user's adduct list and 
#'saves it as a matrix. 
#'@usage Read.AdductData(mSetObj=NA, adductList)
#'@param mSetObj Input the name of the created mSetObj object 
#'@param adductList Input the name of the adduct list
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformAdductMapping <- function(mSetObj=NA, add.mode){
  
  mSetObj <- .get.mSet(mSetObj);
  
  adducts <- mSetObj$dataSet$adduct.list
  
  if(add.mode == "positive"){
    add_db <- .read.metaboanalyst.lib("pos_adduct.rds");
  }else if(add.mode == "negative"){
    add_db <- .read.metaboanalyst.lib("neg_adduct.rds");
  }else if(add.mode == "mixed"){
    add_db <- .read.metaboanalyst.lib("mixed_adduct.rds");
  }else{
    msg <- c("Adduct mode is not valid")
  }
  
  hit.inx <- match(tolower(adducts), tolower(add_db$Ion_Name));
  match.values <- add_db[hit.inx,];
  sel.add <- nrow(match.values)
  
  if(sel.add > 0){
    mSetObj$mummi$add.msg <- paste("A total of ", sel.add ," adducts were successfully selected!", sep = "")
  }else{
    mSetObj$mummi$add.msg <- c("No adducts were selected!")
  }
  
  mSetObj$adduct.custom <- TRUE
  mSetObj$add.map <- match.values
  
  return(.set.mSet(mSetObj));
}

# internal function to create new mz matrix from user-curated list of adducts
new_adduct_mzlist <- function(mSetObj=NA, mw){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mode <- mSetObj$dataSet$mode
  
  ion.name <- mSetObj$add.map$Ion_Name
  ion.mass <- mSetObj$add.map$Ion_Mass
  
  mw_modified <- NULL;

  if(mode!="mixed"){ #pos or neg
    
    mass.list <- as.list(ion.mass)
    mass.user <- lapply(mass.list, function(x) eval(parse(text=paste(gsub("PROTON", 1.007825, x)))) )
    mw_modified <- cbind(mw, do.call(cbind, mass.user));
    
    if(mode == "positive"){
      mw_modified.pos <- mw_modified
      mw_modified.neg <- mw_modified[,1]
      colnames(mw_modified) <- ion.name;
    }else{ #negative
      mw_modified.neg <- mw_modified
      mw_modified.pos <- mw_modified[,1]
      colnames(mw_modified) <- ion.name;
    }
    
    mw_modified <- list(mw_modified.neg, mw_modified.pos)
    names(mw_modified) <- c("neg", "pos")
    
  }else{
    #deal w. mixed ion mode, need to return pos and neg 
    
    neg.ions <- c("M-H [1-]", "M-2H [2-]", "M-3H [3-]", "M-H2O-H [1-]", "M-H+O [1-]", "M+K-2H [1-]", "M+Na-2H [1- ]", "M+Cl [1-]", "M+Cl37 [1-]",   
                  "M+K-2H [1-]", "M+FA-H [1-]", "M+Hac-H [1-]", "M+Br [1-]", "M+Br81 [1-]", "M+TFA-H [1-]", "M+ACN-H [1-]", "M+HCOO [1-]", "M+CH3COO [1-]", 
                  "2M-H [1-]", "2M+FA-H [1-]", "2M+Hac-H [1-]", "3M-H [1-]", "M(C13)-H [1-]", "M(S34)-H [1-]", "M(Cl37)-H [1-]")
    
    ion.name.neg <- intersect(ion.name, neg.ions)
    ion.mass.neg <- ion.mass[which(ion.name %in% neg.ions)] 
    
    ion.name.pos <- setdiff(ion.name, neg.ions)
    ion.mass.pos <- ion.mass[which(ion.name %in% ion.name.pos)] 
    
    mass.list.neg <- as.list(ion.mass.neg)
    mass.user.neg <- lapply(mass.list.neg, function(x) eval(parse(text=paste(gsub("PROTON", 1.007825, x)))) )
    mw_modified.neg <- do.call(cbind, mass.user.neg);
    colnames(mw_modified.neg) <- ion.name.neg;
    
    mass.list.pos <- as.list(ion.mass.pos)
    mass.user.pos <- lapply(mass.list.pos, function(x) eval(parse(text=paste(gsub("PROTON", 1.007825, x)))) )
    mw_modified.pos <- do.call(cbind, mass.user.pos);
    colnames(mw_modified.pos) <- ion.name.pos;
    
    mw_modified <- list(mw_modified.neg, mw_modified.pos)
    names(mw_modified) <- c("neg", "pos")
  }
  return(mw_modified);
}

# internal function for searching compound library
.search.compoundLib <- function(mSetObj, cpd.lib, cpd.treep, cpd.treen){
  
  ref_mzlist <- as.numeric(mSetObj$data$ref_mzlist);
  pos_inx <- mSetObj$data$pos_inx;
  ref_mzlistp <- mSetObj$data$ref_mzlist[pos_inx];
  ref_mzlistn <- mSetObj$data$ref_mzlist[!pos_inx];
  t.scores <- as.numeric(mSetObj$data$ref_mzlist);
  
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
    for(i in 1:length(ref_mzlistp)){
      mz <- ref_mzlistp[i];
      my.tol <- my.tolsp[i];
      all.mz <- all.mzsp[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treep[all.mz])));
      
      for(pos in pos.all){
        id <- cpd.lib$id[pos];
        mw.all <- cpd.lib$mz.matp[pos,]; #get modified mzs
        diffs <- abs(mw.all - mz); #modified mzs - mz original
        hit.inx <- which(diffs < my.tol);
        if(length(hit.inx)>0){
          for(spot in 1:length(hit.inx)){
            hit.pos <- hit.inx[spot];# need to match all
            index <- paste(mz, id, hit.pos, sep = "_");
            matched_resp$add(index, c(i, id, mz, mw.all[hit.pos], modified.statesp[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
          }
        }
      }
    }
  }
  all.mzsn <<- all.mzsn
  if(mSetObj$dataSet$mode != "positive"){
    for(i in 1:length(ref_mzlistn)){
      mz <- ref_mzlistn[i];
      my.tol <- my.tolsn[i];
      all.mz <- all.mzsn[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treen[all.mz])));
      
      for(pos in pos.all){
        id <- cpd.lib$id[pos];
        mw.all <- cpd.lib$mz.matn[pos,]; #get modified mzs
        diffs <- abs(mw.all - mz); #modified mzs - mz original
        hit.inx <- which(diffs < my.tol);
        if(length(hit.inx)>0){
          for(spot in 1:length(hit.inx)){
            hit.pos <- hit.inx[spot];# need to match all
            index <- paste(mz, id, hit.pos, sep = "_");
            matched_resn$add(index, c(i, id, mz, mw.all[hit.pos], modified.statesn[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
          }
        }
      }
    }
  }
  
  # convert to regular list
  if(mSetObj$dataSet$mode == "mixed"){
    matched_resn <- matched_resn$as.list();
    
    if(is.null(unlist(matched_resn))){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
    matched_resp <- matched_resp$as.list();
    matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
    matched_res <- rbind(matched_resp, matched_resn)
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
  matched_res <- matched_res[, c(3,2,5,6)];
  colnames(matched_res) <- c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff");
  write.csv(matched_res, file="mummichog_matched_compound_all.csv", row.names=FALSE);
  
  # now update expr. profile
  matched_mz <- matched_res[,1];
  matched_ts <- mSetObj$dataSet$expr_dic[matched_mz];
  mSetObj$dataSet$mumResTable = matched_res;
  
  # get the expression profile for each 
  exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
  cpd_exp_dict <- Covert2Dictionary(exp.mat);
  
  # create average exp
  exp.vec <- unlist(lapply(cpd_exp_dict, mean));
  
  form.mat <- cbind(matched_res[,2], matched_res[,3]);
  cpd_form_dict <- Covert2Dictionary(form.mat);
  
  # now need to get the mapping from mz to compound id (one mz can have 0, 1, or more id hits)
  mz2cpd_dict <- Covert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
  cpd2mz_dict <- Covert2Dictionary(matched_res[,c(2,1)]); # indexed/named by id
  
  # now do matching to identify significant input_cpdlist
  refmz <- names(mz2cpd_dict)
  hits.index <- which(refmz %in% as.character(mSetObj$data$input_mzlist));
  cpd1 <- unique(unlist(mz2cpd_dict[hits.index]));
  cpd1 <- cpd1[!(cpd1 %in% currency)];
  mSetObj$matches.res <- matched_res;
  mSetObj$mz2cpd_dict <- mz2cpd_dict;
  mSetObj$cpd_exp_dict <- cpd_exp_dict;
  mSetObj$cpd_exp <- exp.vec;
  mSetObj$cpd_form_dict <- cpd_form_dict;
  mSetObj$cpd2mz_dict <- cpd2mz_dict;
  mSetObj$total_matched_cpds <- unique(as.vector(mSetObj$matches.res$Matched.Compound));
  mSetObj$input_cpdlist <- cpd1;
  #res_table <<- matched_res
  return(mSetObj);
}

# Internal function for permutation
.perform.mummichogPermutations <- function(mSetObj, permNum){
  num_perm <- permNum;
  print(paste('Resampling, ', num_perm, 'permutations to estimate background ...'));
  permutation_hits <- permutation_record <- vector("list", num_perm);
  for(i in 1:num_perm){ # for each permutation, create list of input compounds and calculate pvalues for each pathway
    input_mzlist <- sample(mSetObj$dataSet$ref_mzlist, mSetObj$dataSet$N)
    t <- make_cpdlist(mSetObj, input_mzlist);
    perm <- ComputeMummichogPermPvals(t, mSetObj$total_matched_cpds, mSetObj$pathways, mSetObj$matches.res, input_mzlist, mSetObj$cpd2mz_dict);
    permutation_record[[i]] <- perm[1]
    permutation_hits[[i]] <- perm[2]
  }
  
  mSetObj$perm_record <- permutation_record;
  mSetObj$perm_hits <- permutation_hits
  
  return(mSetObj);
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
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset)); #empty lists
  
  for(i in 1:length(current.mset)){ # for each pathway
    sizes[[i]] <- min(feat_len[i], count_cpd2mz(mSetObj$cpd2mz_dict, unlist(feats[i]), mSetObj$data$input_mzlist)) #min overlap or mz hits
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size; # failure in left part
  }
  
  #error fixing for negatives, problem occurs when total_feat_num and query_set_size too close (lsig too close to lall)
  negneg <- rapply(negneg, function(x) ifelse(x<0,0,x), how = "replace") 
  
  unsize <- as.integer(unlist(sizes));
  
  uniq.count <- length(unique(unlist(current.mset, use.names = FALSE)));
  
  # prepare for the result table
  res.mat <- matrix(0, nrow=length(current.mset), ncol=6);
  
  #fishermatrix for phyper
  fishermatrix <- cbind(unsize-1, set.num, (query_set_size + unlist(negneg) - unsize), query_set_size); 
  first <- unlist(lapply(sizes, function(x) max(0, x-1)));
  easematrix <- cbind(first, (set.num - unsize + 1), (query_set_size - unsize), unlist(negneg)); 
  
  res.mat[,1] <- path.num;  
  res.mat[,2] <- set.num;
  res.mat[,3] <- unsize;
  res.mat[,4] <- query_set_size*(path.num/uniq.count); #expected
  res.mat[,6] <- apply(easematrix, 1, function(x) fisher.test(matrix(x, nrow=2), alternative = "greater")$p.value);
  res.mat[,5] <- apply(fishermatrix, 1, function(x) phyper(x[1], x[2], x[3], x[4], lower.tail=FALSE));
  colnames(res.mat) <- c("Pathway total", "Hits.total", "Hits.sig", "Expected", "FET", "EASE");
  rownames(res.mat) <- mSetObj$pathways$name
  
  mSetObj$pvals <- res.mat;
  permutations_hits <- matrix(unlist(mSetObj$perm_hits), nrow=length(mSetObj$perm_hits), byrow=TRUE);
  sig_hits <- res.mat[,3]; # sighits
  sigpvalue <- res.mat[,6]; # EASE scores
  
  perm_record <- unlist(mSetObj$perm_record);
  perm_minus <- abs(0.9999999999 - perm_record);
  
  tryCatch({
    fit.gamma <- fitdistrplus::fitdist(perm_minus, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1));
    rawpval <- as.numeric(sigpvalue);
    adjustedp <- 1 - (pgamma(1-rawpval, shape = fit.gamma$estimate["shape"], rate = fit.gamma$estimate["scale"]));
  }, error = function(e){
    if(mSetObj$dataSet$mumType == "table"){
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
  
  res.mat <- cbind(res.mat, Emp.Hits=better.hits, Empirical=emp.p)
  
  # remove those no hits
  hit.inx <- as.numeric(as.character(res.mat[,3])) > 0;
  res.mat <- res.mat[hit.inx, ];
  
  # prepare json element for network
  hits.all <- cpds[hit.inx];
  hits.sig <- feats[hit.inx];  
  path.nms <- mSetObj$pathways$name[hit.inx];
  
  # order by p-values
  ord.inx <- order(res.mat[,7]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 5);
  mSetObj$mummi.resmat <- res.mat[,-9];
  
  mSetObj$path.nms <- path.nms[ord.inx]
  mSetObj$path.hits <- convert2JsonList(hits.all[ord.inx])
  mSetObj$path.pval <- as.numeric(res.mat[,5])
  
  json.res <- list(
    cmpd.exp = mSetObj$cpd_exp,
    path.nms = path.nms[ord.inx],
    hits.all = convert2JsonList(hits.all[ord.inx]),
    hits.sig = convert2JsonList(hits.sig[ord.inx]),
    fisher.p = as.numeric(res.mat[,7]),
    peakToMet = mSetObj$cpd_form_dict,
    peakTable = mSetObj$dataSet$mumResTable
  );
  
  write.csv(res.mat[,-8], file="mummichog_pathway_enrichment.csv", row.names=TRUE);
  matri = res.mat[,-8]
  matri = suppressWarnings(cbind(res.mat, paste0("P",seq.int(0,nrow(res.mat))))) 
  colnames(matri)[ncol(matri)] = "Pathway Number"
  write.csv(matri, file=mSetObj$mum_nm_csv, row.names=TRUE);
  json.mat <- RJSONIO::toJSON(json.res, .na='null');
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();
  return(mSetObj);
}

#'Plot MS Peaks to Paths mummichog permutation p-values
#'@description Plots the mummichog permutation p-values
#'@param mSetObj Input name of the created mSet Object
#'@param pathway Input the name of the pathway
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotMSPeaksPerm <- function(mSetObj=NA, pathway, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  bw.vec <- unlist(mSetObj$perm_record);
  
  len <- length(bw.vec);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$pls.permut <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,4));
  hst <- hist(bw.vec, breaks = "FD", freq=T,
              ylab="Frequency", xlab= 'Permutation test statistics', col="lightblue", main="");
  
  # add the indicator using original label
  h <- max(hst$counts)
  inx <- which(rownames(mSetObj$mummi.resmat) == pathway)
  raw.p <- mSetObj$mummi.resmat[inx,5]
  arrows(raw.p, h/5, raw.p, 0, col="red", lwd=2);
  text(raw.p, h/3.5, paste('Raw \n statistic \n', raw.p), xpd=T);
  dev.off();
  return(.set.mSet(mSetObj));
}

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
  dt.scores.out <- dt.scores[, list(scores=scores, id = unlist(strsplit(id, "; ", fixed = TRUE))), by=1:nrow(dt.scores)]
  
  rank.vec <- as.numeric(dt.scores.out$nrow)
  names(rank.vec) <- as.character(dt.scores.out$id)
  
  scores.vec <- as.numeric(ag.sorted$scores)
  names(scores.vec) <- as.character(ag.sorted$id)
  
  # run fgsea
  if(mumDataContainsPval == 0){
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
  res.mat[,1] <- matched.size
  res.mat[,2] <- fgseaRes$size
  res.mat[,3] <- fgseaRes$pval
  res.mat[,4] <- fgseaRes$padj
  res.mat[,5] <- fgseaRes$NES
  
  rownames(res.mat) <- fgseaRes$pathway
  colnames(res.mat) <- c("Pathway_Total", "Hits", "P_val", "P_adj", "NES")
  
  # order by p-values
  ord.inx <- order(res.mat[,3]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 4);
  
  mSetObj$mummi.gsea.resmat <- res.mat;
  write.csv(res.mat, file="mummichog_fgsea_pathway_enrichment.csv", row.names=TRUE);
  
  matched_cpds <- names(mSetObj$cpd_exp)
  inx2<-na.omit(match(rownames(res.mat), mSetObj$pathways$name))
  filt_cpds <- lapply(inx2, function(f) { mSetObj$pathways$cpds[f] })
  
  cpds <- lapply(filt_cpds, function(x) intersect(unlist(x), matched_cpds))
  mSetObj$path.nms <- rownames(res.mat)
  mSetObj$path.hits<- convert2JsonList(cpds)
  mSetObj$path.pval <- as.numeric(res.mat[,3])
  json.res <- list(cmpd.exp = total_cpds,
                   path.nms = rownames(res.mat),
                   hits.all = convert2JsonList(cpds),
                   nes = fgseaRes$NES,
                   fisher.p = as.numeric(res.mat[,3]))
  
  json.mat <- RJSONIO::toJSON(json.res, .na='null');
  sink(mSetObj$mum_nm);
  cat(json.mat);
  sink();
  
  return(mSetObj);
}

#' PlotPeaks2Paths
#' @description Plots either the original mummichog or GSEA results.
#' @param mSetObj Input the name of the created mSetObj object
#' @param imgName Input a name for the plot
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param Labels Character, indicate if the plot should be labeled. By default
#' it is set to "default", and the 5 top-ranked pathways per each algorithm will be plotted.
#' Users can adjust the number of pathways to be annotated per pathway using the "num_annot" 
#' parameter.
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

PlotPeaks2Paths <- function(mSetObj=NA, imgName, format = "png", dpi = 72, width = 9, labels = "default",
                            num_annot = 5){
  
  mSetObj <- .get.mSet(mSetObj);
  if(anal.type == "mummichog"){
    mummi.mat <- mSetObj$mummi.resmat
    y <- -log(mummi.mat[,5]);
    x <- mummi.mat[,3]/mummi.mat[,4]
    pathnames <- rownames(mummi.mat)
    
  }else{
    gsea.mat <- mSetObj$mummi.gsea.resmat
    y <- -log(gsea.mat[,3])
    x <- gsea.mat[,2]/gsea.mat[,1]
    pathnames <- rownames(gsea.mat)
  }
  
  inx <- order(y, decreasing= T);
  
  y <- y[inx];
  x <- x[inx]; 
  path.nms <- pathnames[inx];
  
  # set circle size based on enrichment factor
  sqx <- sqrt(x);
  min.x <- min(sqx, na.rm = TRUE);
  max.x <- max(sqx, na.rm = TRUE);
  
  if(min.x == max.x){ # only 1 value
    max.x = 1.5*max.x;
    min.x = 0.5*min.x;
  }
  
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(sqx-min.x)/(max.x-min.x);
  
  # set background color according to combo.p
  bg.vec <- heat.colors(length(y));
  
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
  
  df <- data.frame(path.nms, x, y)
  
  if(labels == "default"){
    pk.inx <- GetTopInx(df$y, num_annot, T)
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$mummi.plot <- imgName
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op <- par(mar=c(6,5,2,3));
  plot(x, y, type="n", axes=F, xlab="Enrichment Factor", ylab="-log(p)", bty = "l");
  axis(1);
  axis(2);
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  if(labels=="default"){
    text(x[pk.inx], y[pk.inx], labels = path.nms[pk.inx], pos=3, xpd=T, cex=0.8)
  }else if(labels == "all"){
    text(x, y, labels = path.nms, pos=3, xpd=T, cex=0.8)
  }
  
  par(op);
  dev.off();
  if(anal.type == "mummichog"){
    df <- list(pval=unname(y), enr=unname(x), pathnames=pathnames);
    sink("scattermum.json");
    cat(RJSONIO::toJSON(df));
    sink();
  }else{
    df <- list(pval=unname(y), enr=unname(gsea.mat[,5]), pathnames=pathnames);
    sink("scattergsea.json");
    cat(RJSONIO::toJSON(df));
    sink();
  }
  
  return(.set.mSet(mSetObj));
  
}

#' PlotIntegPaths
#' @description Plots both the original mummichog and the GSEA results by combining p-values
#' using the Fisher's method (sumlog). 
#' @param mSetObj Input the name of the created mSetObj object
#' @param imgName Input a name for the plot
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param Labels Character, indicate if the plot should be labeled. By default
#' it is set to "default", and the 5 top-ranked pathways per each algorithm will be plotted.
#' Users can adjust the number of pathways to be annotated per pathway using the "labels.x" 
#' and "labels.y" parameters.
#' Users can set this to "none" for no annotations, or "all" to annotate all pathways. 
#' @param labels.x Numeric, indicate the number of top-ranked pathways using the fGSEA algorithm 
#'  to annotate on the plot. 
#' @param labels.y Numeric, indicate the number of top-ranked pathways using the original 
#' mummichog algorithm to annotate on the plot. 
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import metap
#' @import scales

PlotIntegPaths <- function(mSetObj=NA, imgName, format = "png", dpi = 72, width = 9, labels = "default", 
                           labels.x = 5, labels.y = 5){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # check if mummichog + gsea was performed
  if(is.null(mSetObj$mummi.resmat) | is.null(mSetObj$mummi.gsea.resmat)){
    print("Both mummichog and fGSEA must be performed!")
    return(0)
  }
  
  combo.resmat <- mSetObj$integ.resmat
  pathnames <- rownames(combo.resmat)
  # Sort values based on combined pvalues
  y <- -log(combo.resmat[,4]);
  y <- scales::rescale(y, c(0,4))
  
  x <- -log(combo.resmat[,5]);
  x <- scales::rescale(x, c(0,4))
  
  combo.p <- -log(combo.resmat[,6])
  combo.p <- scales::rescale(combo.p, c(0,4))
  
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
  
  # set background color according to combo.p
  bg.vec <- heat.colors(length(combo.p));
  
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
  
  df <- data.frame(path.nms, x, y)
  
  if(labels == "default"){
    mummi.inx <- GetTopInx(df$y, labels.y, T)
    gsea.inx <- GetTopInx(df$x, labels.x, T)
    all.inx <- mummi.inx | gsea.inx;
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$integpks.plot <- imgName
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op <- par(mar=c(6,5,2,3));
  plot(x, y, type="n", axes=F, xlab="GSEA -log(p)", ylab="Mummichog -log(p)", bty = "l");
  axis(1);
  axis(2);
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  axis.lims <- par("usr")
  
  # mummichog sig
  mum.x <- c(axis.lims[1], axis.lims[1], axis.lims[2], axis.lims[2])
  mum.y <- c(2, axis.lims[4], axis.lims[4], 2)
  polygon(mum.x, mum.y, col=rgb(82/255,193/255,188/255,0.3), border = NA)
  
  # gsea sig
  gsea.x <- c(2,2,axis.lims[4],axis.lims[4])
  gsea.y <- c(axis.lims[1],axis.lims[4],axis.lims[4],axis.lims[1])
  polygon(gsea.x, gsea.y, col=rgb(216/255,126/255,178/255,0.3), border = NA)
  
  if(labels=="default"){
    text(x[all.inx], y[all.inx], labels = path.nms[all.inx], pos=3, xpd=T, cex=0.8)
  }else if(labels == "all"){
    text(x, y, labels = path.nms, pos=3, xpd=T, cex=0.8)
  }
  
  par(op);
  dev.off();
  
  df <- list(pval=unname(y), enr=unname(x), metap= unname(combo.p), pathnames=pathnames);
  sink("scatterinteg.json");
  cat(RJSONIO::toJSON(df));
  sink();
  
  return(.set.mSet(mSetObj));
}

# Function to return the unique m/zs from the selected pathways 
# based on the compounds

GetMummichogMZHits <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  inx <- which(mSetObj$pathways$name == msetNm)
  mset <- mSetObj$pathways$cpds[[inx]];
  
  mzs <- as.numeric(unique(unlist(mSetObj$cpd2mz_dict[mset])))
  
  result <- intersect(mzs, mSetObj$dataSet$input_mzlist)
  
  return(result)
}

#' Plot m/z hits in a pathway
#' @description Function to create a boxplot of m/z features
#' within a specific pathway. m/z features used by the original
#' mummichog algorithm are highlighted with an asterisk. 
#' @param mSetObj Input the name of the created mSetObj object.
#' @param msetNM Character, input the name of the pathway. 
#' @param format Character, input the format of the image to create. 
#' @param dpi Numeric, input the dpi of the image to create. Default 
#' is set to 300.
#' @param width Numeric, input the width of the image to create.
#' Default is set to 10.
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import ggplot2
#' @import reshape2
#' @import scales

PlotPathwayMZHits <- function(mSetObj=NA, msetNM, format="png", dpi=300,
                              width=10){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    return(0)
  }
  
  # check if mummichog + gsea was performed
  if(is.null(mSetObj$mummi.resmat) | is.null(mSetObj$mummi.gsea.resmat)){
    print("Both mummichog and fGSEA must be performed!")
    return(0)
  }
  
  inx <- which(mSetObj$pathways$name == msetNM)
  
  # Brief sanity check
  if(length(inx) == 0){
    print(paste0(msetNM, " is not found in the pathway library! Please verify the spelling."))
    return(0)
  }
  
  mset <- mSetObj$pathways$cpds[[inx]];
  
  mzs <- as.numeric(unique(unlist(mSetObj$cpd2mz_dict[mset])))
  
  result <- intersect(mzs, unique(mSetObj$matches.res[,1]))
  
  pvals <- mSetObj$dataSet$mummi.proc[mSetObj$dataSet$mummi.proc[, 2] %in% result, ]
  
  pval.cutoff <- mSetObj$dataSet$cutoff
  
  used.inx <- pvals[,1] < pval.cutoff
  mummi <- which(used.inx)
  mummi_mzs <- pvals[,2]
  
  # add astericks if used by mummichog
  mummi_mzs_star <- mummi_mzs
  mummi_mzs_star[mummi] <- paste(mummi_mzs_star[mummi], "*",sep="");
  
  # create boxdata
  data <- mSetObj$dataSet$proc
  
  boxdata <- as.data.frame(data[,mummi_mzs])
  colnames(boxdata) <- mummi_mzs_star
  boxdata$class <- mSetObj$dataSet$cls
  
  num.feats <- length(result)
  
  boxdata.m <- reshape2::melt(boxdata, id.vars="class")
  boxdata.m$value <- scales::rescale(boxdata.m$value, c(0,1))
  
  boxplotName <- paste(msetNM, ".", format, sep="");
  
  if(num.feats == 1){
    
    w = width
    h = width
    p <- ggplot(data = boxdata.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=class), outlier.shape = NA, outlier.colour=NA)
    p <- p + ggtitle(msetNM) + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Group"))
    p <- p + xlab("m/z feature") + ylab("Intensity")
    
    ggsave(p, filename = boxplotName, dpi=300, width=w, height=h, limitsize = FALSE)
    
    return(mSetObj)
    
  }else if(num.feats<10){
    w = width
    h <- num.feats
    cols = 3
  }else if(num.feats<5){
    w = width
    h = width
  }else{
    w = width
    h <- num.feats * 0.35
    cols = 6
  }
  
  p <- ggplot(data = boxdata.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=class), outlier.shape = NA, outlier.colour=NA)
  p <- p + facet_wrap( ~ variable, scales="free", ncol=cols) + xlab("m/z features") + ylab("Intensity")
  p <- p + ggtitle(msetNM) + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Group"))
  
  ggsave(p, filename = boxplotName, dpi=300, width=w, height=h, limitsize = FALSE)
  
  return(.set.mSet(mSetObj));
}

######## For R package #######

#' Function to get compound details from a specified pathway
#' @description Function to get compound details from a specified pathway.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param msetNm Input the name of the pathway
#' @export
GetMummichogPathSetDetails <- function(mSetObj=NA, msetNm){
  mSetObj <- .get.mSet(mSetObj);
  inx <- which(mSetObj$pathways$name == msetNm)
  mset <- mSetObj$pathways$cpds[[inx]];
  hits.all <- unique(mSetObj$total_matched_cpds) #matched compounds
  hits.sig <- mSetObj$input_cpdlist;
  
  refs <- mset %in% hits.all;
  sigs <- mset %in% hits.sig;
  red.inx <- which(sigs);
  blue.inx <- which(refs & !sigs);
  
  nms <- mset;
  sig.cpds <- nms[red.inx]
  nsig.cpds <- nms[blue.inx]
  
  path.results <- matrix(NA, 2, 1)
  colnames(path.results) <- paste(msetNm, "Compound Hits", sep=" ")
  rownames(path.results) <- c("Signicant Compounds", "Non-Significant Compounds")
  path.results[1,1] <- paste(sig.cpds, collapse = "; ")
  path.results[2,1] <- paste(nsig.cpds, collapse = "; ")
  write.csv(path.results, "mummichog_pathway_compounds.csv")
  print(path.results);
  return(.set.mSet(mSetObj));
}

#' Function to get adduct details from a specified compound
#' @description Function to get adduct details from a specified compound.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param cmpd.id Input the name of the selected compound.
#' @export
GetCompoundDetails <- function(mSetObj=NA, cmpd.id){
  
  mSetObj <- .get.mSet(mSetObj);
  forms <- mSetObj$cpd_form_dict[[cmpd.id]];
  
  if(is.null(forms)){
    print("This compound is not valid!")
    return(0)
  }
  
  mz <- mSetObj$dataSet$mumResTable[which(mSetObj$dataSet$mumResTable$Matched.Compound == cmpd.id), 1] 
  mass.diff <- mSetObj$dataSet$mumResTable[which(mSetObj$dataSet$mumResTable$Matched.Compound == cmpd.id), 4]
  tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
  
  res <- cbind(rep(cmpd.id, length(mz)), mz, forms, mass.diff, tscores) 
  colnames(res) <- c("Matched.Compound", "m.z", "Matched.Form", "Mass.Diff", "T.Scores")
  write.csv(res, "mummichog_compound_details.csv")
  print(res)
  return(.set.mSet(mSetObj));
}

##### For Web ################

GetMatchingDetails <- function(mSetObj=NA, cmpd.id){
  
  mSetObj <- .get.mSet(mSetObj);
  forms <- mSetObj$cpd_form_dict[[cmpd.id]];
  tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
  # create html table
  res <- paste("<li>", "<b>", forms, "</b>: ", tscores, "</li>",sep="", collapse="");
  return(res);
}

GetMummichogHTMLPathSet <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  inx <- which(mSetObj$pathways$name == msetNm)
  mset <- mSetObj$pathways$cpds[[inx]];
  hits.all <- unique(mSetObj$total_matched_cpds) #matched compounds
  
  if(anal.type == "mummichog"|anal.type == "integ_peaks"){
    
    hits.sig <- mSetObj$input_cpdlist;
    
    # highlighting with different colors
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    red.inx <- which(sigs);
    blue.inx <- which(refs & !sigs);
    
    # use actual cmpd names
    #nms <- names(mset);
    nms <- mset;
    nms[red.inx] <- paste("<font color=\"red\">", "<b>", nms[red.inx], "</b>", "</font>",sep="");
    nms[blue.inx] <- paste("<font color=\"blue\">", "<b>", nms[blue.inx], "</b>", "</font>",sep="");
  }else{
    refs <- mset %in% hits.all;
    red.inx <- which(refs);
    nms <- mset;
    nms[red.inx] <- paste("<font color=\"red\">", "<b>", nms[red.inx], "</b>", "</font>",sep="");
  }
  return(cbind(msetNm, paste(unique(nms), collapse="; ")));
}

GetMummiResMatrix <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(anal.type == "mummichog"){
    return(mSetObj$mummi.resmat);
  }else if(anal.type == "gsea_peaks"){
    return(mSetObj$mummi.gsea.resmat);
  }else{
    return(mSetObj$integ.resmat);
  }
}

GetMummiResRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(anal.type == "mummichog"){
    return(rownames(mSetObj$mummi.resmat));
  }else if(anal.type == "gsea_peaks"){
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

#'Set the peak enrichment method for the MS Peaks to Paths module
#'@description This function sets the peak enrichment method.
#'@param mSetObj Input the name of the created mSetObj.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SetPeakEnrichMethod <- function(mSetObj=NA, algOpt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$peaks.alg <- algOpt
  
  if(algOpt == "gsea"){
    anal.type <<- "gsea_peaks"
  }else if(algOpt == "mum"){
    anal.type <<- "mummichog"
  }else{
    anal.type <<- "integ_peaks"
  }
  
  return(.set.mSet(mSetObj));
}

GetDefaultPvalCutoff <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(peakFormat %in% c("rmp", "rmt")){
    maxp <- 0;
  }else{
    pvals <- c(0.25, 0.2, 0.15, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
    ndat <- mSetObj$dataSet$mummi.proc;
    n <- floor(0.1*length(ndat[,"p.value"]))
    cutoff <- ndat[n+1,1]
    if(!any(pvals <= cutoff)){
      maxp <- 0.00001
    }else{
      maxp <- max(pvals[pvals <= cutoff])
    }
  }
  
  return(maxp)
  
}

GetMummiMode <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  mode <- mSetObj$dataSet$mode
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

################## Utility Functions #########
# Global variables define currency compounds
currency <- c('C00001', 'C00080', 'C00007', 'C00006', 'C00005', 'C00003',
              'C00004', 'C00002', 'C00013', 'C00008', 'C00009', 'C00011',
              'G11113', '', 'H2O', 'H+', 'Oxygen', 'NADP+', 'NADPH', 'NAD+', 'NADH', 'ATP',
              'Pyrophosphate', 'ADP', 'Orthophosphate', 'CO2');

# mz tolerance based on instrument type
# input: a vector of mz,
# output: a vector of distance tolerance
# Review on mass accuracy by Fiehn: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1464138/

mz_tolerance <- function(mz, ms.type){
  if(tolower(ms.type) == "five"){
    return(0.000005*mz)
  }else if(tolower(ms.type) == "three"){
    return(0.000003*mz)
  }else if(tolower(ms.type) == "one"){
    return(0.000001*mz)
  }else if(tolower(ms.type) == "pointone"){
    return(0.0000001*mz)
  }else{
    return(0.000010*mz)
  }
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
  cpd <- cpd[!(cpd %in% currency)];
  
  return(cpd);
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

########### Utility Functions #########

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
Covert2Dictionary <- function(data, quiet=T){
  
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

# Calculate p-values for each Lperm
# Used in higher mummichogR functions
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
  
  for(i in 1:length(current.mset)){ # for each pathway
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

# utility function for fast list expanding (dynamic length)
# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
# https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

myFastList <- function(capacity = 100) {
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
#'@export
#'@import fgsea

fgsea2 <- function(mSetObj, pathways, stats, ranks,
                   nperm,
                   minSize=1, maxSize=Inf,
                   nproc=0,
                   gseaParam=1,
                   BPPARAM=NULL) {
  
  # Warning message for ties in stats
  ties <- sum(duplicated(stats[stats != 0]))
  if (ties != 0) {
    warning("There are ties in the preranked stats (",
            paste(round(ties * 100 / length(stats), digits = 2)),
            "% of the list).\n",
            "The order of those tied m/z features will be arbitrary, which may produce unexpected results.")
  }
  
  # Warning message for duplicate gene names
  if (any(duplicated(names(stats)))) {
    warning("There are duplicate m/z feature names, fgsea may produce unexpected results")
  }
  
  granularity <- 1000
  permPerProc <- rep(granularity, floor(nperm / granularity))
  if (nperm - sum(permPerProc) > 0) {
    permPerProc <- c(permPerProc, nperm - sum(permPerProc))
  }
  seeds <- sample.int(10^9, length(permPerProc))
  
  if (is.null(BPPARAM)) {
    if (nproc != 0) {
      if (.Platform$OS.type == "windows") {
        # windows doesn't support multicore, using snow instead
        BPPARAM <- BiocParallel::SnowParam(workers = nproc)
      } else {
        BPPARAM <- BiocParallel::MulticoreParam(workers = nproc)
      }
    } else {
      BPPARAM <- BiocParallel::bpparam()
    }
  }
  
  minSize <- max(minSize, 1)
  stats <- abs(stats) ^ gseaParam
  
  # returns list of indexs of matches between pathways and rank names
  pathwaysPos <- lapply(pathways, function(p) { as.vector(na.omit(fastmatch::fmatch(p, names(ranks)))) })
  pathwaysFiltered <- lapply(pathwaysPos, function(s) { ranks[s] })
  
  # adjust for the fact that a single m/z feature can match to several compound identifiers
  # subsets m/z features responsible for a compound and matches it to total set of matched m/z features
  # returns the length
  pathway2mzSizes <- sapply(pathways, function(z) { length(intersect(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[z]))), unique(mSetObj$matches.res[,1])))} )
  oldpathwaysSizes <- sapply(pathwaysFiltered, length)
  
  pathwaysSizes <- pmin(pathway2mzSizes, oldpathwaysSizes)
  
  toKeep <- which(minSize <= pathwaysSizes & pathwaysSizes <= maxSize)
  m <- length(toKeep)
  
  if (m == 0) {
    return(data.table(pathway=character(),
                      pval=numeric(),
                      padj=numeric(),
                      ES=numeric(),
                      NES=numeric(),
                      nMoreExtreme=numeric(),
                      size=integer(),
                      leadingEdge=list()))
  }
  
  pathwaysFiltered <- pathwaysFiltered[toKeep]
  pathwaysSizes <- pathwaysSizes[toKeep]
  
  K <- max(pathwaysSizes)
  
  #perform gsea
  gseaStatRes <- do.call(rbind,
                         lapply(pathwaysFiltered, fgsea::calcGseaStat,
                                stats=stats,
                                returnLeadingEdge=TRUE))
  
  leadingEdges <- mapply("[", list(names(stats)), gseaStatRes[, "leadingEdge"], SIMPLIFY = FALSE)
  pathwayScores <- unlist(gseaStatRes[, "res"])
  
  #perform permutations
  universe <- seq_along(stats)
  
  counts <- BiocParallel::bplapply(seq_along(permPerProc), function(i) {
    nperm1 <- permPerProc[i]
    leEs <- rep(0, m)
    geEs <- rep(0, m)
    leZero <- rep(0, m)
    geZero <- rep(0, m)
    leZeroSum <- rep(0, m)
    geZeroSum <- rep(0, m)
    if (m == 1) {
      for (i in seq_len(nperm1)) {
        randSample <- sample.int(length(universe), K)
        randEsP <- fgsea::calcGseaStat(
          stats = stats,
          selectedStats = randSample,
          gseaParam = 1)
        leEs <- leEs + (randEsP <= pathwayScores)
        geEs <- geEs + (randEsP >= pathwayScores)
        leZero <- leZero + (randEsP <= 0)
        geZero <- geZero + (randEsP >= 0)
        leZeroSum <- leZeroSum + pmin(randEsP, 0)
        geZeroSum <- geZeroSum + pmax(randEsP, 0)
      }
    } else {
      aux <- fgsea:::calcGseaStatCumulativeBatch(
        stats = stats,
        gseaParam = 1,
        pathwayScores = pathwayScores,
        pathwaysSizes = pathwaysSizes,
        iterations = nperm1,
        seed = seeds[i])
      leEs = get("leEs", aux)
      geEs = get("geEs", aux)
      leZero = get("leZero", aux)
      geZero = get("geZero", aux)
      leZeroSum = get("leZeroSum", aux)
      geZeroSum = get("geZeroSum", aux)
    }
    data.table::data.table(pathway=seq_len(m),
                           leEs=leEs, geEs=geEs,
                           leZero=leZero, geZero=geZero,
                           leZeroSum=leZeroSum, geZeroSum=geZeroSum
    )
  }, BPPARAM=BPPARAM)
  
  counts <- data.table::rbindlist(counts)
  
  # Getting rid of check NOTEs
  leEs=leZero=geEs=geZero=leZeroSum=geZeroSum=NULL
  pathway=padj=pval=ES=NES=geZeroMean=leZeroMean=NULL
  nMoreExtreme=nGeEs=nLeEs=size=NULL
  leadingEdge=NULL
  .="damn notes"
  
  pval <- unlist(lapply(counts$pathway, function(c) min((1+sum(counts[c,]$leEs)) / (1 + sum(counts[c,]$leZero)),
                                                        (1+sum(counts[c,]$geEs)) / (1 + sum(counts[c,]$geZero)))))
  
  leZeroMean <- unlist(lapply(counts$pathway, function(d) sum(counts[d,]$leZeroSum) / sum(counts[d,]$leZero)))
  geZeroMean <- unlist(lapply(counts$pathway, function(e) sum(counts[e,]$geZeroSum) / sum(counts[e,]$geZero)))
  nLeEs <- unlist(lapply(counts$pathway, function(f) sum(counts[f,]$leEs)))
  nGeEs <- unlist(lapply(counts$pathway, function(g) sum(counts[g,]$geEs)))
  
  pvals <- data.frame(pval=pval, leZeroMean=leZeroMean, geZeroMean=geZeroMean, nLeEs=nLeEs, nGeEs=nGeEs)
  
  padj <- p.adjust(pvals$pval, method="fdr")
  ES <- pathwayScores
  NES <- ES / ifelse(ES > 0, pvals$geZeroMean, abs(pvals$leZeroMean))
  pvals$leZeroMean <- NULL
  pvals$geZeroMean <- NULL
  
  nMoreExtreme <- ifelse(ES > 0, pvals$nGeEs, pvals$nLeEs)
  pvals$nLeEs <- NULL
  pvals$nGeEs <- NULL
  
  size <- pathwaysSizes
  pathway <- names(pathwaysFiltered)
  
  leadingEdge <- sapply(leadingEdges, paste0, collapse = "; ")
  leadingEdge2 <- sapply(leadingEdge, function(x) strsplit(x, "; "))
  pathway.cpds <- sapply(pathwaysFiltered, attributes)
  
  matches <- mapply(intersect, leadingEdge2, pathway.cpds)
  
  leadingEdgeMatched <- sapply(matches, paste0, collapse = "; ")
  
  pvals.done <- cbind(pathway, pvals, padj, ES, NES, nMoreExtreme, size, leadingEdgeMatched)
  
  return(pvals.done)
}

calcGseaStat2 <- function(stats, selectedStats, gseaParam=1,
                          returnAllExtremes=FALSE,
                          returnLeadingEdge=FALSE) {
  
  S <- selectedStats
  r <- stats
  p <- gseaParam
  
  S <- sort(S)
  
  # account for 1 mz can be multiple cpds
  S.scores <- r[S]
  u.S <- S[!duplicated(S.scores)]
  scores <- unique(S.scores)
  
  m <- length(scores)
  N <- length(r)
  
  if (m == N) {
    stop("GSEA statistic is not defined when all genes are selected")
  }
  
  NR <- (sum(abs(scores)^p))
  rAdj <- abs(scores)^p
  if (NR == 0) {
    # this is equivalent to rAdj being rep(eps, m)
    rCumSum <- seq_along(rAdj) / length(rAdj)
  } else {
    rCumSum <- cumsum(rAdj) / NR
  }
  
  tops <- rCumSum - (u.S - seq_along(u.S)) / (N - m)
  if (NR == 0) {
    # this is equivalent to rAdj being rep(eps, m)
    bottoms <- tops - 1 / m
  } else {
    bottoms <- tops - rAdj / NR
  }
  
  maxP <- max(tops)
  minP <- min(bottoms)
  
  if(maxP > -minP) {
    geneSetStatistic <- maxP
  } else if (maxP < -minP) {
    geneSetStatistic <- minP
  } else {
    geneSetStatistic <- 0
  }
  
  if (!returnAllExtremes && !returnLeadingEdge) {
    return(geneSetStatistic)
  }
  
  res <- list(res=geneSetStatistic)
  if (returnAllExtremes) {
    res <- c(res, list(tops=tops, bottoms=bottoms))
  }
  if (returnLeadingEdge) {
    leadingEdge <- if (maxP > -minP) {
      u.S[seq_along(u.S) <= which.max(bottoms)]
    } else if (maxP < -minP) {
      rev(u.S[seq_along(u.S) >= which.min(bottoms)])
    } else {
      NULL
    }
    
    res <- c(res, list(leadingEdge=leadingEdge))
  }
  res
}

sumlog <-function(p) {
  keep <- (p > 0) & (p <= 1)
  lnp <- log(p[keep])
  chisq <- (-2) * sum(lnp)
  df <- 2 * length(lnp)
  if(sum(1L * keep) < 2)
    stop("Must have at least two valid p values")
  if(length(lnp) != length(p)) {
    warning("Some studies omitted")
  }
  res <- list(chisq = chisq, df = df,
              p = pchisq(chisq, df, lower.tail = FALSE), validp = p[keep])
  class(res) <- c("sumlog", "metap")
  res
}


CreateHeatmapJson <- function(mSetObj=NA, libOpt, fileNm, filtOpt){

  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  data <- t(dataSet$norm)
  sig.ids <- rownames(data);
  
  l = sapply(rownames(data),function(x) return(unname(strsplit(x,"/")[[1]][1])))
  l = as.numeric(unname(unlist(l)))
  
  if(length(levels(dataSet$cls))<3){
    res<-GetTtestRes(NA, FALSE, TRUE, F)
  }else{
    aov.res <- apply(as.matrix(dataSet$norm), 2, aof, cls=dataSet$cls);
    anova.res <- lapply(aov.res, anova);
    
    #extract all p values
    res <- unlist(lapply(anova.res, function(x) { c(x["F value"][1,], x["Pr(>F)"][1,])}));
    res <- data.frame(matrix(res, nrow=length(aov.res), byrow=T), stringsAsFactors=FALSE);
  }
  rownames(res) = rownames(data);
  
  if(dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx = rep(TRUE, nrow(data))
  }else{
    mSetObj$dataSet$pos_inx = rep(FALSE, nrow(data))
  }
  mSetObj$dataSet$ref_mzlist = as.numeric(rownames(data));
  mSetObj$dataSet$expr_dic= res[,1];
  names(mSetObj$dataSet$expr_dic) = rownames(res)
  
  if(filtOpt == "filtered"){
    mSetObj <- searchCompLib(mSetObj, libOpt);
    res_table <- mSetObj$matches.res;
    print(head(res_table))
    data = data[which(l %in% res_table[,"Query.Mass"]),]
    res = res[which(rownames(res) %in% res_table[,"Query.Mass"]),]
  }
  
  stat.pvals <- unname(as.vector(res[,2]));
  #stat.pvals <- unname(as.vector(data[,1]));
  org = unname(strsplit(libOpt,"_")[[1]][1])
  # scale each gene 
  dat <- t(scale(t(data)));
  
  rankPval = order(as.vector(stat.pvals))
  stat.pvals = stat.pvals[rankPval]
  dat = dat[rankPval,]
  
  # now pearson and euclidean will be the same after scaleing
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  # do clustering and save cluster info
  # convert order to rank (score that can used to sort) 
  if(nrow(dat)> 1){
    dat.dist <- dist(dat);
    gene.ward.ord <- hclust(dat.dist, "ward.D")$order;
    gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.ord]);
    gene.ave.ord <- hclust(dat.dist, "ave")$order;
    gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.ord]);
    gene.single.ord <- hclust(dat.dist, "single")$order;
    gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.ord]);
    gene.complete.ord <- hclust(dat.dist, "complete")$order;
    gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.ord]);
    
    dat.dist <- dist(t(dat));
    smpl.ward.ord <- hclust(dat.dist, "ward.D")$order;
    smpl.ward.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ward.ord])
    smpl.ave.ord <- hclust(dat.dist, "ave")$order;
    smpl.ave.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ave.ord])
    smpl.single.ord <- hclust(dat.dist, "single")$order;
    smpl.single.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.single.ord])
    smpl.complete.ord <- hclust(dat.dist, "complete")$order;
    smpl.complete.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.complete.ord])
  }else{
    # force not to be single element vector which will be scaler
    #stat.pvals <- matrix(stat.pvals);
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(dat);
  }
  
  gene.cluster <- list(
    ward = gene.ward.rk,
    average = gene.ave.rk,
    single = gene.single.rk,
    complete = gene.complete.rk,
    pval = stat.pvals
  );
  
  sample.cluster <- list(
    ward = smpl.ward.rk,
    average = smpl.ave.rk,
    single = smpl.single.rk,
    complete = smpl.complete.rk
  );
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  meta <- data.frame(dataSet$cls);
  grps <- "Condition"
  nmeta <- meta.vec <- NULL;
  uniq.num <- 0;
  for (i in 1:ncol(meta)){
    cls <- meta[,i];
    grp.nm <- grps[i];
    meta.vec <- c(meta.vec, as.character(cls))
    # make sure each label are unqiue across multiple meta data
    ncls <- paste(grp.nm, as.numeric(cls)); # note, here to retain ordered factor
    nmeta <- c(nmeta, ncls);
  }
  
  # convert back to numeric 
  nmeta <- as.numeric(as.factor(nmeta))+99;
  unik.inx <- !duplicated(nmeta)   
  
  # get corresponding names
  meta_anot <- meta.vec[unik.inx]; 
  names(meta_anot) <- nmeta[unik.inx]; # name annotatation by their numbers
  
  nmeta <- matrix(nmeta, ncol=ncol(meta), byrow=F);
  colnames(nmeta) <- grps;
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks 
  res <- t(apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))}));
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  gene.id = orig.gene.nms; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
  json.res <- list(
    data.type = dataSet$type,
    gene.id = gene.id,
    gene.entrez = gene.id,
    gene.name = gene.id,
    gene.cluster = gene.cluster,
    sample.cluster = sample.cluster,
    sample.names = orig.smpl.nms,
    meta = data.frame(nmeta),
    meta.anot = meta_anot,
    data = res,
    org = org
  );
  
  mSetObj$dataSet$hm_peak_names = gene.id
  mSetObj$dataSet$gene.cluster = gene.cluster
  
  .set.mSet(mSetObj)
  require(RJSONIO);
  json.mat <- toJSON(json.res, .na='null');
  sink(fileNm);
  cat(json.mat);
  sink();
  current.msg <<- "Data is now ready for heatmap visualization!";
  return(1);
}

doHeatmapMummichogTest <- function(mSetObj=NA, nm, lib, ids){
  gene.vec <- unlist(strsplit(ids, "; "));
  mSetObj<-.get.mSet(mSetObj);
  mSetObj$dataSet$input_mzlist <- gene.vec;
  mSetObj$dataSet$N <- length(gene.vec);
  mSetObj$mum_nm <- paste0(nm,".json");
  mSetObj$mum_nm_csv <- paste0(nm,".csv");
  .set.mSet(mSetObj);
  anal.type <<- "mummichog";
  PerformPSEA("NA", lib, 100);
}

searchCompLib <- function(mSetObj, lib){
  
  filenm <- paste(lib, ".rds", sep="")
  biocyc <- grepl("biocyc", lib)
  
  if(!is.null(mSetObj$curr.cust)){
    
    if(biocyc){
      user.curr <- mSetObj$curr.map$BioCyc
    }else{
      user.curr <- mSetObj$curr.map$KEGG
    }
    
    currency <<- user.curr
    
    if(length(currency)>0){
      mSetObj$mummi$anal.msg <- c("Currency metabolites were successfully uploaded!")
    }else{
      mSetObj$mummi$anal.msg <- c("Errors in currency metabolites uploading!")
    }
  }
  
  if(.on.public.web){
    mummichog.lib <- readRDS(paste("../../libs/mummichog/", filenm, sep=""));
  }else{
    if(!file.exists(filenm)){
      mum.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", filenm, sep="")
      download.file(mum.url, destfile = filenm, method="libcurl", mode = "wb")
      mummichog.lib <- readRDS(filenm);
    }else{
      mummichog.lib <- readRDS(filenm);
    }
  }
  
  if(!is.null(mSetObj$adduct.custom)){
    mw <- mummichog.lib$cpd.lib$mw
    new_adducts <- new_adduct_mzlist(mSetObj, mw)
    
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
  
  cpd.treep <- mummichog.lib$cpd.tree[["positive"]];
  cpd.treen <- mummichog.lib$cpd.tree[["negative"]];
  mSetObj$pathways <- mummichog.lib$pathways;
  mSetObj$lib.organism <- lib; #keep track of lib organism for sweave report
  mSetObj <- .search.compoundLib(mSetObj, cpd.lib, cpd.treep, cpd.treen);
  print(head(mSetObj$matches.res))
  return(mSetObj)
}
