### This is a online script to do some pre-define before job scheduler

#' CreateRawRscript
#' @description used to create a running for raw spectra processing
#' this file will be run by spring daemon by using OptiLCMS rather than relay on the local 
#' compiling RC files
#' @author Guangyan Zhou, Zhiqiang Pang
CreateRawRscript <- function(guestName, planString, planString2, rawfilenms.vec){
  
  guestName <- guestName;
  planString <- planString;
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  }else {
    users.path <-getwd();
  }
  print(getwd());
  ## Prepare Configuration script for slurm running
  conf_inf <- "#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=480:00\n#SBATCH --mem-per-cpu=6000\n#SBATCH --cpus-per-task=4\n"
  
  ## Prepare R script for running
  # need to require("OptiLCMS")
  str <- paste0('library(OptiLCMS)');
  
  # Set working dir & files included
  str <- paste0(str, ";\n", "setwd(\'",users.path,"\')");
  str <- paste0(str, ";\n", "mSet <- InitDataObjects(\'spec\', \'raw\', FALSE)");
  str <- paste0(str, ";\n", "mSet <- UpdateRawfiles(mSet,", rawfilenms.vec, ")");
  
  ## Construct the opt pipeline
  if(planString2 == "opt"){
    str <- paste0(str, ';\n', 'plan <- InitializaPlan(\'raw_opt\')')
    str <- paste0(str, ';\n',  planString)
    str <- paste0(str, ';\n',  "res <- ExecutePlan(plan)")
  }
  
  ## Construct the default pipeline
  if(planString2 == "default"){
    str <- paste0(str, ';\n', 'plan <- InitializaPlan(\'raw_ms\')')
    str <- paste0(str, ';\n',  planString)
    str <- paste0(str, ';\n',  "res <- ExecutePlan(plan)")
  }
  
  str <- paste0(str, ';\n',  "Export.Annotation(res[['mSet']])")
  str <- paste0(str, ';\n',  "Export.PeakTable(res[['mSet']])")
  str <- paste0(str, ';\n',  "Export.PeakSummary(res[['mSet']])")
  
  # sink command for running
  sink("ExecuteRawSpec.sh");
  
  cat(conf_inf);
  cat(paste0("\nsrun R -e \"\n", str, "\n\""));
      
  sink();
    
  # record the R command
  mSetObj <- .get.mSet(mSetObj); 
  mSetObj$cmdSet <- c(mSetObj$cmdSet, str);
  .set.mSet(mSetObj);
  
  # add job cancel control button: 0 means running, 1 means kill!
  write.table(0, quote = FALSE, append = FALSE, row.names = FALSE, col.names = FALSE, file = "JobKill");
  
  return(as.integer(1))
}

#' SetRawFileNames
#' @description #TODO: SetRawFileNames
#' @author Guangyan Zhou, Zhiqiang Pang
SetRawFileNames <- function(filenms){
  print(filenms)
  rawfilenms.vec <<- filenms
  return(1);
}

#' #Raw Spectra Upload
#' @description function used to process during uploading stage
#' @author Guangyan Zhou, Zhiqiang Pang
ReadRawMeta<-function(fileName){
  if(grepl(".txt", fileName, fixed=T)){
    tbl=read.table(fileName,header=TRUE, stringsAsFactors = F);
  }else if(grepl(".csv", fileName, fixed=T)){
    tbl = read.csv(fileName,header=TRUE, stringsAsFactors = F);
  }else{
    print("wrongfiletype")
  }
  
  rawFileNms <-as.vector(tbl[,1])
  rawClassNms <-as.vector(tbl[,2])
  rawFileNms <- sapply(strsplit(rawFileNms, "\\."), function(x) paste0(head(x,-1), collapse="."));
  clsTable = table(rawClassNms)
  #check replicate number
  clsTypes = names(table(rawClassNms))
  for(name in clsTypes){
    if(toupper(name) !="QC"){
      replicateNum = clsTable[[name]]
    }
  }
  
  rawFileNms<<-rawFileNms
  rawClassNms<<-rawClassNms
  return(1);
}

GetRawFileNms <- function(){
  return(rawFileNms)
}

GetRawClassNms <- function(){
  return(rawClassNms)
}

#' Set User Path
#' @description play roles at the first uploading and login stage
#' @author Guangyan Zhou, Zhiqiang Pang
#' TODO: can be delete in the future
SetUserPath<-function(path){
  fullUserPath <<- path;
}
.getDataPath<- function() {
  if(file.exists("/home/zgy/scheduler/MetaboAnalyst.so")){
    path = "/home/glassfish/payara5/glassfish/domains/domain1/applications/MetaboAnalyst/resources/data/"
  }else if(dir.exists("/media/zzggyy/disk/")){
    path <- "/media/zzggyy/disk/MetaboAnalyst/target/MetaboAnalyst-5.18/resources/data/"
  }else if(.on.public.web){
    path = "../../data/";
  }
  return(path)
}

#' verifyParam
#' @description Verify the example params has changed or not
#' @author Guangyan Zhou, Zhiqiang Pang
verifyParam <- function(param0_path, users.path) {
  
  load(paste0(users.path, "/params.rda"));
  
  load(param0_path);
 
  verify.vec <- NULL
  
  for (i in 1:length(peakParams)) {
    for (j in 1:length(peakParams0)) {
      if (names(peakParams[i]) == names(peakParams0[j])) {
        verify.vec_tmp <- peakParams[[i]] == peakParams0[[j]]
        verify.vec <- c(verify.vec, verify.vec_tmp)
      }
    }
  }
  
  if (all(verify.vec)) {
    print("Examples' param not changed !")
    return(1)
    
  } else{
    print("Examples' param changed !")
    return(0)
  }
  
}

#' spectraInclusion
#' @description save the spectra file info for project loading
#' @author Zhiqiang Pang
spectraInclusion <- function(files, number){
    qs::qsave(list(files, number), file = "IncludedSpectra.qs");
}

#' getSpectraInclusion
#' @description read the spectra file info for project loading
#' @author Zhiqiang Pang
getSpectraInclusion <- function(){
    return(qs::qread("IncludedSpectra.qs"));
}

#' updateSpectra3DPCA
#' @description update Spectra 3D PCA, mainly json file
#' @author Zhiqiang Pang
updateSpectra3DPCA <- function(featureNM = 100){
  load("mSet.rda");
  #.feature_values <- OptiLCMS:::.feature_values;
  
  if(!exists("mSet")){
    return(0);
  }
  
  if(!is.numeric(featureNM)){
    return(-1);
  }
  
  sample_idx <-
    mSet@rawOnDisk@phenoData@data[["sample_group"]];
  
  # feature_value <-
  #   .feature_values(
  #     pks = mSet@peakfilling$msFeatureData$chromPeaks,
  #     fts = mSet@peakfilling$FeatureGroupTable,
  #     method = "medret",
  #     value = "into",
  #     intensity = "into",
  #     colnames = mSet@rawOnDisk@phenoData@data[["sample_name"]],
  #     missing = NA
  #   );
  
  ## Pre-process here
  feature_value0 <- mSet@dataSet[-1,];
  rownames(feature_value0) <- feature_value0[,1];
  feature_value <- feature_value0[,-1];
  feature_value[is.na(feature_value)] <- 0;
  
  int.mat <- as.matrix(feature_value)
  rowNms <- rownames(int.mat);
  colNms <- colnames(int.mat);
  int.mat <- t(apply(int.mat, 1, function(x) .replace.by.lod(as.numeric(x))));
  rownames(int.mat) <- rowNms;
  colnames(int.mat) <- colNms; 
  feature_value <- int.mat;
  feature_value[feature_value==0] <- 1;

  # feature_value[is.na(feature_value)] <- 0;
  # int.mat <- feature_value
  # rowNms <- rownames(int.mat);
  # colNms <- colnames(int.mat);
  # int.mat <- t(apply(int.mat, 1, .replace.by.lod));
  # 
  # rownames(int.mat) <- rowNms;
  # colnames(int.mat) <- colNms; 
  # feature_value <- int.mat;
  # 
  # feature_value[feature_value==0] <- 1;
  
  min.val <- min(abs(feature_value[feature_value!=0]))/10;
  pca_feats <- log10((feature_value + sqrt(feature_value^2 + min.val^2))/2);
  
  pca_feats[is.na(pca_feats)] <- 0;
  df0 <- na.omit(pca_feats);
  df1 <- df0[is.finite(rowSums(df0)),];
  df <- t(df1);

  ## Filtering
  mSet_pca <- prcomp(df, center = TRUE, scale = FALSE);
  imp.pca<-summary(mSet_pca)$importance;
  coords0 <- coords <- data.frame(t(signif(mSet_pca$rotation[,1:3], 5)));
  colnames(coords) <- NULL; 

  weights <- imp.pca[2,][1:3]
  mypos <- t(coords);
  meanpos <- apply(abs(mypos),1, function(x){weighted.mean(x, weights)})
  
  df.idx <- data.frame(pos = meanpos, inx = seq.int(1,length(meanpos)))
  df.idx <- df.idx[order(-df.idx$pos),]

  selectRow_idx <- df.idx[c(1:featureNM), 2]
  feature_value1 <- feature_value[sort(selectRow_idx),];
  
  dists <- GetDist3D(coords0);
  colset <- GetRGBColorGradient(dists);
  cols <- colset[sort(selectRow_idx)];
  
  ## Processing to save Json
  coords0 <- coords <- df0 <- df1 <- df <- NULL;
  min.val <- min(abs(feature_value1[feature_value1!=0]))/10;
  pca_feats <- log10((feature_value1 + sqrt(feature_value1^2 + min.val^2))/2);
  
  pca_feats[is.na(pca_feats)] <- 0;
  df0 <- na.omit(pca_feats);
  df1 <- df0[is.finite(rowSums(df0)),];
  df <- t(df1);
  
  mSet_pca <- prcomp(df, center = TRUE, scale = FALSE);
  
  sum.pca <- summary(mSet_pca);
  var.pca <-
    sum.pca$importance[2,]; # variance explained by each PCA
  
  xlabel <- paste("PC1", "(", round(100 * var.pca[1], 1), "%)");
  ylabel <- paste("PC2", "(", round(100 * var.pca[2], 1), "%)");
  zlabel <- paste("PC2", "(", round(100 * var.pca[3], 1), "%)");
  
  # using ggplot2
  df <- as.data.frame(mSet_pca$x);
  df$group <- sample_idx;
  
  ## For score plot
  pca3d <- list();
  pca3d$score$axis <- c(xlabel, ylabel, zlabel);
  xyz0 <- df[,c(1:3)];
  colnames(xyz0) <- rownames(xyz0) <- NULL;
  pca3d$score$xyz <- data.frame(t(xyz0));
  colnames(pca3d$score$xyz) <- NULL;
  pca3d$score$name <- rownames(df);
  pca3d$score$facA <- df$group;
  
  if(length(unique(df$group)) < 9){
    col.fun <-
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(length(unique(df$group)), "Set1"));
  } else {
    col.fun <-
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(length(unique(df$group)), "Set3"));
  }
  
  pca3d$score$colors <- col.fun(length(unique(df$group)));
  
  json.obj <- RJSONIO::toJSON(pca3d, .na='null');
  
  sink(paste0("spectra_3d_score", featureNM,".json"));
  cat(json.obj);
  sink();
  
  ## For loading plot
  pca3d <- list();
  
  pca3d$loading$axis <- paste("Loading ", c(1:3), sep="");
  coords0 <- coords <- data.frame(t(signif(mSet_pca$rotation[,1:3], 5)));
  colnames(coords) <- NULL; 
  pca3d$loading$xyz <- coords;
  pca3d$loading$name <- rownames(mSet_pca$rotation);
  pca3d$loading$entrez <- paste0(round(mSet@peakfilling[["FeatureGroupTable"]]@listData$mzmed, 4), 
                                 "@", 
                                 round(mSet@peakfilling[["FeatureGroupTable"]]@listData$rtmed, 2))[sort(selectRow_idx)];
  
  pca3d$loading$cols <- cols;
  
  pca3d$cls =  df$group;
  json.obj <- RJSONIO::toJSON(pca3d, .na='null');
  
  sink(paste0("spectra_3d_loading", featureNM,".json"));
  
  cat(json.obj);
  sink();
  return(1);
}

#' mzrt2ID
#' @description convert mzATrt as Feature ID
#' @author Zhiqiang Pang
mzrt2ID <- function(mzrt){
  load("mSet.rda");
  FTID = 1;
  FTID <- which(mSet@dataSet[["Sample"]] == mzrt)-1;
  return(as.integer(FTID));
}

#' readAdductsList
#' @description readAdductsList for user customization
#' @author Zhiqiang Pang
readAdductsList <- function(polarity = NULL){
  
  res <- NULL;
  r <- OptiLCMS:::.exportmSetRuleClass();
  
  setDefaultLists <- OptiLCMS:::setDefaultLists;
  readLists <- OptiLCMS:::readLists;
  setParams <- OptiLCMS:::setParams;
  generateRules <- OptiLCMS:::generateRules;
  
  r <- setDefaultLists(r, lib.loc=.libPaths());
  r <- readLists(r);
  
  if(polarity == "positive" || is.null(polarity)){
    r <- setParams(r, maxcharge=3, mol=3, nion=2,
                   nnloss=1, nnadd=1, nh=2, polarity="positive", lib.loc = .libPaths());
    r <- generateRules(r);
    rules <- unique(r@rules);
    res <- rules[order(rules$ips,decreasing = TRUE),][["name"]];
  } else {
    r <- setParams(r, maxcharge=3, mol=3, nion=2,
                   nnloss=1, nnadd=1, nh=2, polarity="negative", lib.loc = .libPaths());
    r <- generateRules(r);
    rules <- unique(r@rules);
    res <- rules[order(rules$ips,decreasing = TRUE),][["name"]];
  }
  
  return(res);
}

################## ------------- Shell function here below ------------- ######################

#' InitializePlan
#' @description this function is used to initialize a plan before submit job to spring daemon
#' @author Zhiqiang Pang
InitializaPlan <- function(){
  plan <- OptiLCMS::InitializaPlan();
  # Nothing to return
}

#' CentroidCheck
#' @description CentroidCheck function used to check centroid or not 
#' @author Zhiqiang Pang
CentroidCheck <- function(filename){
  return(OptiLCMS::CentroidCheck(filename));
}

#' SetPeakParam
#' @description SetPeakParam, used to set the peak param 
#' @author Zhiqiang Pang
SetPeakParam <- function(platform = "general", Peak_method = "centWave", RT_method = "loess",
                         mzdiff, snthresh, bw, # used for both "centwave" and "matchedFilter"
                         ppm, min_peakwidth, max_peakwidth, noise, prefilter, value_of_prefilter, # used for "centwave"
                         fwhm, steps, sigma, peakBinSize, max, # used for "matchedFilter"
                         criticalValue, consecMissedLimit, unions, checkBack, withWave, # used for "massifquant"
                         profStep, # used for "obiwarp"
                         minFraction, minSamples, maxFeatures, mzCenterFun, integrate,# used for grouping
                         extra, span, smooth, family, fitgauss, # used for RT correction with peakgroup "loess"
                         polarity, perc_fwhm, mz_abs_iso, max_charge, max_iso, corr_eic_th, mz_abs_add, adducts, #used for annotation
                         rmConts #used to control remove contamination or not
                         ){
    OptiLCMS::SetPeakParam(platform = platform, Peak_method = Peak_method, RT_method = RT_method,
                           mzdiff, snthresh, bw, 
                           ppm, min_peakwidth, max_peakwidth, noise, prefilter, value_of_prefilter, 
                           fwhm, steps, sigma, peakBinSize, max, 
                           criticalValue, consecMissedLimit, unions, checkBack, withWave, 
                           profStep, 
                           minFraction, minSamples, maxFeatures, mzCenterFun, integrate,
                           extra, span, smooth, family, fitgauss, 
                           polarity, perc_fwhm, mz_abs_iso, max_charge, max_iso, corr_eic_th, mz_abs_add, adducts, #used for annotation
                           rmConts 
  )
  #return nothing
}

#' GeneratePeakList
#' @description GeneratePeakList is used to generate the peak summary list for result page
#' @author Zhiqiang Pang
GeneratePeakList <- function(userPath){
  return(OptiLCMS:::GeneratePeakList(userPath))
}

#' plotSingleTIC
#' @description plotSingleTIC is used to plot single TIC
#' @author Zhiqiang Pang
plotSingleTIC <- function(filename, imageNumber, format = "png", dpi = 72, width = NA){
  if (is.na(width)) {
    widthm <- "default"
    width <- 7;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  
  if(imageNumber == -1){
    imgName <- paste0(filename,".", format);
  } else {
    imgName <- paste0("TIC_", filename, "_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  }
  print(imgName);
  OptiLCMS::plotSingleTIC(NULL, filename, imgName, format = format, dpi = dpi, width = width);
  return(imgName);
}

#' plotMSfeature
#' @description plotMSfeature is used to plot MS feature stats for different groups
#' @author Zhiqiang Pang
plotMSfeature <- function(FeatureNM, format = "png", dpi = 72, width = NA){
  if (is.na(width)) {
    #width <- 6;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "enlarged"
  } else {
    widthm <- "full"
  }
  imgName <- OptiLCMS::plotMSfeature(NULL, FeatureNM, dpi = dpi, format = format, width = width)
  print(imgName);
  return(imgName)
}

#' PlotXIC
#' @description PlotXIC is used to plot both MS XIC/EIC features of different group and samples
#' @author Zhiqiang Pang
PlotXIC <- function(featureNum, format = "png", dpi = 72, width = NA){
  if(is.na(width)){
    width <- 6;
  }
  return(OptiLCMS::PlotXIC(NULL, featureNum, format = format, dpi = dpi, width = width))
}

#' PerformDataInspect
#' @description PerformDataInspect is used to plot 2D/3D structure of the MS data
#' @author Zhiqiang Pang
PerformDataInspect <- function(datapath = NULL,
                               rt.range = c(0,0),
                               mz.range = c(0,0),
                               dimension = "3D",
                               res = 100){
  OptiLCMS::PerformDataInspect(datapath, rt.range,
                               mz.range, dimension,
                               res)
}

#' FastRunningShow_customized
#' @description FastRunningShow_customized is used to showcase the customized running pipeline
#' @author Zhiqiang Pang
FastRunningShow_customized <- function(fullUserPath){
  OptiLCMS:::FastRunningShow_customized(fullUserPath)
}

#' FastRunningShow_auto
#' @description FastRunningShow_auto is used to showcase the AUTO running pipeline
#' @author Zhiqiang Pang
FastRunningShow_auto <- function(fullUserPath){
  OptiLCMS:::FastRunningShow_auto(fullUserPath)
}

#' centroidMSData
#' @description centroidMSData is used to centroid MS Data
#' @author Zhiqiang Pang
centroidMSData <- function(fileName, outFolder, ncore = 1){
  
  .CentroidSpectra <- OptiLCMS:::.CentroidSpectra;
  writeMSData <- MSnbase::writeMSData;
  
  if(tolower(tools::file_ext(fileName)) %in% c("mzml", "mzxml")){
    CMS <- try(.CentroidSpectra(paste0(outFolder, "/" , fileName)),silent = TRUE)
    
    if(class(CMS) == "try-error") {return(-1)}
    
    files.mzml <- paste0(outFolder, "/", fileName)
    file.remove(files.mzml)
    writeMSData(CMS, file = files.mzml)
    
  } else {
    return(-2)
  }
  
  return(1)
}

PlotSpectraRTadj <- function(imageNumber, format = "png", dpi = 72, width = NA) {
  if (is.na(width)) {
    widthm <- "default"
    width <- 9;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  imgName <- paste0("Adjusted_RT_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  OptiLCMS::PlotSpectraRTadj (mSet = NULL,
                              imgName = imgName, format = format, dpi = dpi, width = width)

  return(imgName); 
}

PlotSpectraBPIadj <- function(imageNumber, format = "png", dpi = 72, width = NA) {
  if (is.na(width)) {
    widthm <- "default"
    width <- 9;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  imgName <- paste0("Adjusted_BPI_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  OptiLCMS::PlotSpectraBPIadj (mSet = NULL,
                               imgName = imgName, format = format, dpi = dpi, width = width)

  return(imgName); 
}

PlotSpectraInsensityStatics <- function(imageNumber, format = "png", dpi = 72, width = NA) {
  if (is.na(width)) {
    widthm <- "default"
    width <- 8;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  imgName <- paste0("Insensity_Statics_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  OptiLCMS::PlotSpectraInsensityStistics (mSet = NULL, 
                                          imgName = imgName, format = format, dpi = dpi, width = width);

  return(imgName);  
}

plotTICs <- function(imageNumber, format = "png", dpi = 72, width = NA) {
  if (is.na(width)) {
    widthm <- "default"
    width <- 9;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  imgName <- paste0("TICs_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  OptiLCMS::plotTICs (mSet = NULL, imgName = imgName, format = format, dpi = dpi, width = width);

  return(imgName);  
}

plotBPIs <- function(imageNumber, format = "png", dpi = 72, width = NA) {
  if (is.na(width)) {
    widthm <- "default"
    width <- 9;
  } else if (width < 5) {
    widthm <- "half"
  } else if (width > 11) {
    widthm <- "12in"
  } else {
    widthm <- "7in"
  }
  imgName <- paste0("BPIs_", dpi, "_", widthm, "_",  imageNumber, ".", format);
  OptiLCMS::plotBPIs (mSet = NULL, imgName = imgName, format = format, dpi = dpi, width = width);

  return(imgName);  
}