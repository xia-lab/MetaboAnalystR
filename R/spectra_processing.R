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
  
  ## Prepare Configuration script for slurm running
  conf_inf <- "#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=360:00\n#SBATCH --mem-per-cpu=5000\n#SBATCH --cpus-per-task=4\n"
  
  
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
                         polarity, perc_fwhm, mz_abs_iso, max_charge, max_iso, corr_eic_th, mz_abs_add, #used for annotation
                         rmConts #used to control remove contamination or not
                         ){
    OptiLCMS::SetPeakParam(platform = "general", Peak_method = "centWave", RT_method = "loess",
                           mzdiff, snthresh, bw, # used for both "centwave" and "matchedFilter"
                           ppm, min_peakwidth, max_peakwidth, noise, prefilter, value_of_prefilter, # used for "centwave"
                           fwhm, steps, sigma, peakBinSize, max, # used for "matchedFilter"
                           criticalValue, consecMissedLimit, unions, checkBack, withWave, # used for "massifquant"
                           profStep, # used for "obiwarp"
                           minFraction, minSamples, maxFeatures, mzCenterFun, integrate,# used for grouping
                           extra, span, smooth, family, fitgauss, # used for RT correction with peakgroup "loess"
                           polarity, perc_fwhm, mz_abs_iso, max_charge, max_iso, corr_eic_th, mz_abs_add, #used for annotation
                           rmConts #used to control remove contamination or not
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
plotSingleTIC <- function(filename, imagename){
  OptiLCMS::plotSingleTIC(NULL, filename, imagename)
}

#' plotMSfeature
#' @description plotMSfeature is used to plot MS feature stats for different groups
#' @author Zhiqiang Pang
plotMSfeature <- function(FeatureNM){
  OptiLCMS::plotMSfeature(NULL, FeatureNM)
}

#' PlotXIC
#' @description PlotXIC is used to plot both MS XIC/EIC features of different group and samples
#' @author Zhiqiang Pang
PlotXIC <- function(featureNum){
  OptiLCMS::PlotXIC(NULL, featureNum)
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

