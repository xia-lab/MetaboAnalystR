### This is a online script to do some pre-define before job scheduler

#' InitMSObjects
#'
#' @param data.type should be "raw"
#' @param anal.type should be "spec"
#' @param paired should be "FALSE"
#' @export
InitMSObjects <- function (data.type = NULL, anal.type = NULL, paired=FALSE) {
  if(anal.type == "raw" & data.type == "spec") {
    return(OptiLCMS::InitDataObjects(anal.type = "raw", 
                                     data.type = "spec", 
                                     paired = FALSE))
  } else {
    return(OptiLCMS::InitDataObjects(anal.type = "raw", 
                                     data.type = "spec", 
                                     paired = FALSE))
  }
}


#' CreateRawRscript
#' @description used to create a running for raw spectra processing
#' this file will be run by SLURM by using OptiLCMS
#' @noRd
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
  conf_inf <- paste0("#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=720:00\n#SBATCH --mem-per-cpu=5G\n#SBATCH --cpus-per-task=2\n#SBATCH --output=", users.path, "/metaboanalyst_spec_proc.txt\n")
  
  ## Prepare R script for running
  # need to require("OptiLCMS")
  str <- paste0('library(OptiLCMS)');
  
  # Set working dir & init env & files included
  str <- paste0(str, ";\n", "metaboanalyst_env <- new.env()");
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


#' CreateMS2RawRscript#'
#' @noRd
#' @author Zhiqiang Pang
CreateMS2RawRscript <- function(guestName, planString, mode = "dda"){
  
  guestName <- guestName;
  planString <- planString;
  
  cat("current planString ---> ", planString, "\n")
  cat("current guestName  ---> ", guestName, "\n")
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  }else {
    users.path <-getwd();
  }
  
  ## Prepare Configuration script for slurm running
  conf_inf <- "#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=720:00\n#SBATCH --mem-per-cpu=5G\n#SBATCH --cpus-per-task=2\n"
  
  ## Prepare R script for running
  # need to require("OptiLCMS")
  str <- paste0('library(OptiLCMS)');
  
  # Set working dir & init env & files included
  str <- paste0(str, ";\n", "metaboanalyst_env <- new.env()");
  str <- paste0(str, ";\n", "setwd(\'",users.path,"\')");
  str <- paste0(str, ";\n", "load(\'mSet.rda\')");
  
  if(mode == "dda"){
    # import feature list
    str <- paste0(str, ";\n", "ft <- mSet@peakAnnotation[[\'camera_output\']][,c(2,3,5,6)]");
    # progress 104
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 7/12: Starting importing MS/MS data... \n\'),
      ecol = \'\',
      progress = 104
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # import data
    if(file.exists("upload/MS2")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), targetFeatures = as.matrix(ft), acquisitionMode = \'DDA\')")
    } else if(any(grepl("MS2_", list.files("upload/")))) {
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\"upload/\",
                                                  pattern = \"^MS2_\",
                                                  full.names = T, recursive = T)), targetFeatures = as.matrix(ft), acquisitionMode = \'DDA\')")
    }
    # progress 110
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 7/12: MS/MS data imported completely! \n\n\'),
      ecol = \'\',
      progress = 110
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # perform deconvolution
    # progress 120
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 8/12: MS/MS data deconvolution is starting... \n\'),
      ecol = \'\',
      progress = 120
    )";
    cmd_deco <- "mSet <- PerformDDADeconvolution(mSet,
                                    ppm1 = 5,ppm2 = 10,
                                    sn = 12,filtering = 0,
                                    window_size = 1.5, intensity_thresh = 1.6e5,database_path = \'/data/COMPOUND_DBs/Curated_DB/v09102023/MS2ID_Bio_v09102023.sqlite\',
                                    ncores = 4L)";
    str <- paste0(str, ";\n", cmd_deco)
    # progress 140
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 8/12: MS/MS data deconvolution completed ! \n\n\'),
      ecol = \'\',
      progress = 140
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
  } else {
    # for swath-dia
    # progress 102
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 7/12: Starting importing MS/MS data... \n\'),
      ecol = \'\',
      progress = 102
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # import data
    if(file.exists("upload/MS2")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(mSet = mSet, filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), acquisitionMode = \'DIA\', SWATH_file = 'swath_design_metaboanalyst.txt')")
    } else if(any(grepl("MS2_", list.files("upload/")))) {
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\"upload/\",
                                                  pattern = \"^MS2_\",
                                                  full.names = T, recursive = T)), acquisitionMode = \'DIA\', SWATH_file = 'swath_design_metaboanalyst.txt')")
    }
    
    # progress 110
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 7/12: MS/MS data imported completely!  \n\n\'),
      ecol = \'\',
      progress = 110
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # perform deconvolution
    # progress 120
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 8/12: MS/MS data deconvolution is starting... \n\'),
      ecol = \'\',
      progress = 120
    )";
    cmd_deco <- "mSet <- PerformDIADeconvolution(mSet,
                                    min_width = 5,ppm2 = 10,
                                    sn = 12,filtering = 0,
                                    ncores = 4L)";
    str <- paste0(str, ";\n", cmd_deco)
    # progress 140
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 8/12: MS/MS data deconvolution completed! \n\n\'),
      ecol = \'\',
      progress = 140
    )";
    str <- paste0(str, ";\n", cmd_prgs)
  }
  
  # PerformSpectrumConsenus
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 9/12: MS/MS spectra consensus is starting .. \n\'),
      ecol = \'\',
      progress = 145
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  cmd_consenus <- "mSet <- PerformSpectrumConsenus (mSet, ppm2 = 15, concensus_fraction = 0.2, database_path = '', use_rt = FALSE,
                                     user_dbCorrection = FALSE)";
  str <- paste0(str, ";\n", cmd_consenus)
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 9/12: MS/MS spectra consensus finished! \n\n\'),
      ecol = \'\',
      progress = 150
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # PerformDBSearchingBatch
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 10/12: MS/MS spectra database searching is starting ...\n this step may take some time.. \n\n\'),
      ecol = \'\',
      progress = 150
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  cmd_seareching <- "mSet <- PerformDBSearchingBatch (mSet,
                                     ppm1 = 10, ppm2 = 25,
                                     rt_tol = 5, database_path = \'/data/COMPOUND_DBs/Curated_DB/v09102023/MS2ID_Bio_v09102023.sqlite\', 
                                     use_rt = FALSE, enableNL = FALSE, ncores = 4L)";
  str <- paste0(str, ";\n", cmd_seareching)
  # progress 180
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 10/12: MS/MS database searching completed! \n\n\'),
      ecol = \'\',
      progress = 180
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # PerformResultsExport
  # progress 190
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 11/12: MS/MS data processing result exporting.. \n\'),
      ecol = \'\',
      progress = 190
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  cmd_export <- "mSet <- PerformResultsExport (mSet, type = 0L,
                                  topN = 10L, ncores = 4L)";
  str <- paste0(str, ";\n", cmd_export)
  # progress 190
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 11/12: MS/MS data processing result exported! \n\n\'),
      ecol = \'\',
      progress = 190
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # FormatMSnAnnotation
  cmd_annotation <- "dtx <- FormatMSnAnnotation(mSet, 5L, F)";
  str <- paste0(str, ";\n", cmd_annotation)
  # progress 198
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 12/12: MS/MS data processing finished! We are finalizing the job! \n\'),
      ecol = \'\',
      progress = 198
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # progress 200 
  cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'<b>Everything of this LC-MS/MS dataset has been completed successfully! </b>\n\n\'),
      ecol = \'\',
      progress = 200
    )";
  str <- paste0(str, ";\n", cmd_prgs)
  
  
  # sink command for running
  sink("ExecuteRawSpec.sh", append = TRUE);
  
  cat(paste0("\nsrun R -e \"\n", str, "\n\""));
  
  sink();
  
  return(1)
}



#' CreateRawRscript4Asari
#' @description used to create a running for raw spectra processing
#' this file will be run by SLURM by using OptiLCMS and Asari from python.
#' @noRd
#' @author Zhiqiang Pang
CreateRawRscript4Asari <- function(guestName, planString, asari_str, rawfilenms.vec){
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  } else {
    users.path <-getwd();
  }
  
  ## Prepare Configuration script for slurm running
  conf_inf <- paste0("#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=720:00\n#SBATCH --mem-per-cpu=5G\n#SBATCH --cpus-per-task=2\n#SBATCH --output=", users.path, "/metaboanalyst_spec_proc.txt\n")
  
  require(R.utils)
  allMSFiles <- list.files("upload", full.names = T, recursive = T)
  for(i in allMSFiles){
    if(!grepl("^upload/MS2", i)){
      createLink(link = paste0(users.path, "/uploadAll/",basename(i)), target = i)
    }
  }
  
  users_path <- paste0(users.path, "/uploadAll/")
  ## Prepare Asari code to execute
  str_asari <- paste0(asari_str, " ", users_path, " --output ", users.path, "/results", " --project metabo_asari_res ");
  #str_asari <- paste0(str_asari, "sed '1,$s/",gsub("\\/", "\\\\/", users.path), "//' > ")
  #str_asari <- paste0(str_asari, "awk '{gsub(\"", users.path, "\",\"\")}1' > ")
  #str_asari <- paste0(str_asari, users.path, "/metaboanalyst_spec_proc.txt 2>&1")
  
  ## Prepare R script for running
  # need to require("OptiLCMS")
  str <- paste0('library(OptiLCMS)');
  
  # Set working dir & init env & files included
  str <- paste0(str, ";\n", "metaboanalyst_env <- new.env()");
  str <- paste0(str, ";\n", "setwd(\'",users.path,"\')");
  str <- paste0(str, ";\n", "mSet <- InitDataObjects(\'spec\', \'raw\', FALSE)");
  str <- paste0(str, ";\n", "mSet <- UpdateRawfiles(mSet,", rawfilenms.vec, ")");
  
  ## Construct the default pipeline
  #str <- paste0(str, ';\n', 'plan <- InitializaPlan(\'raw_ms\')')
  str <- paste0(str, ';\n',  planString)
  str <- paste0(str, '\n',  "OptiLCMS:::MessageOutput('\nStep 2/6: Start raw spectral processing with Asari! 
                  \nThis step may take a long time...', '\n', NULL)")
  #str <- paste0(str, ';\n',  "res <- ExecutePlan(plan)")
  
  #str <- paste0(str, ';\n',  "Export.Annotation(res[['mSet']])")
  #str <- paste0(str, ';\n',  "Export.PeakTable(res[['mSet']])")
  #str <- paste0(str, ';\n',  "Export.PeakSummary(res[['mSet']])")
  
  # check the latest result folder
  load("params.rda")
  minFrc <- peakParams[["minFraction"]];
  
  str2 <- paste0('library(OptiLCMS)');
  str2 <- paste0(str2, ";\n", "metaboanalyst_env <- new.env()");
  str2 <- paste0(str2, ";\n", "setwd(\'",users.path,"\')");
  str2 <- paste0(str2, ";\n", "OptiLCMS:::PerformAsariResultsFormating(", minFrc, ")")
  
  # sink command for running
  sink("ExecuteRawSpec.sh");
  
  cat(conf_inf);
  
  # str_asari
  
  cat(paste0("\nsrun R -e \"\n", str, "\n\";"));
  cat(paste0("\nsrun ", str_asari, "; \n\n"));
  cat(paste0("\nsrun R -e \"\n", str2, "\n\";"));
  
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
#' @noRd
#' @author Guangyan Zhou, Zhiqiang Pang
SetRawFileNames <- function(filenms){
  print(filenms)
  rawfilenms.vec <<- filenms
  return(1);
}

#' #Raw Spectra Upload
#' @description function used to process during uploading stage
#' @noRd
#' @author Guangyan Zhou, Zhiqiang Pang
ReadRawMeta<-function(fileName){
  if(grepl(".txt", fileName, fixed=T)){
    tbl=read.table(fileName,header=TRUE, stringsAsFactors = F, sep = "\t");
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

#' @noRd
GetRawFileNms <- function(){
  return(rawFileNms)
}

#' @noRd
GetRawClassNms <- function(){
  return(rawClassNms)
}

#' Set User Path
#' @description play roles at the first uploading and login stage
#' @noRd
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
#' @noRd
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
#' @noRd
#' @author Zhiqiang Pang
#' @importFrom qs qsave
spectraInclusion <- function(files, number){
    qs::qsave(list(files, number), file = "IncludedSpectra.qs");
}

#' getSpectraInclusion
#' @description read the spectra file info for project loading
#' @noRd
#' @author Zhiqiang Pang
#' @importFrom qs qread
getSpectraInclusion <- function(){
    return(qs::qread("IncludedSpectra.qs"));
}

#' updateSpectra3DPCA
#' @description update Spectra 3D PCA, mainly json file
#' @noRd
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
  zlabel <- paste("PC3", "(", round(100 * var.pca[3], 1), "%)");
  
  # using ggplot2
  df <- as.data.frame(mSet_pca$x);
  df$group <- sample_idx;
  
  ## For score plot
  pca3d <- list();
  pca3d$score$axis <- c(xlabel, ylabel, zlabel);
  xyz0 <- df[,c(1:3)];
  colnames(xyz0) <- rownames(xyz0) <- NULL;
  pca3d$score$xyz <- data.frame(t(xyz0));
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
  
  json.obj <- rjson::toJSON(pca3d);
  
  sink(paste0("spectra_3d_score", featureNM,".json"));
  cat(json.obj);
  sink();
  
  ## For loading plot
  #pca3d <- list();
  
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
  json.obj <- rjson::toJSON(pca3d);
  
  sink(paste0("spectra_3d_loading", featureNM,".json"));
  
  cat(json.obj);
  sink();

  qs::qsave(pca3d$score, "score3d.qs");
  qs::qsave(pca3d$loading, "loading3d.qs");
  fileNm <- paste0("spectra_3d_loading", featureNM,".json");

  if(!exists("my.json.scatter")){
    .load.scripts.on.demand("util_scatter3d.Rc");    
  }

  my.json.scatter(fileNm, T);
  return(1);

}

#' mzrt2ID
#' @description convert mzATrt as Feature ID
#' @noRd
#' @author Zhiqiang Pang
mzrt2ID <- function(mzrt){
  load("mSet.rda");
  FTID = 1;
  FTID <- which(mSet@dataSet[["Sample"]] == mzrt)-1;
  return(as.integer(FTID));
}

#' mzrt2ID2
#' @description convert mzATrt as Feature ID
#' @noRd
#' @author Zhiqiang Pang
mzrt2ID2 <- function(mzrt){
  ### NOTE: this function is a temperary function, will be removed later 
  load("mSet.rda");
  FTID = 1;
  mSet@peakAnnotation[["camera_output"]]-> dt;
  if(grepl("@", mzrt)) {
      FTID <- which((abs(dt[,1] - as.numeric(unlist(strsplit(mzrt,"@")))[1]) < 1e-4) & 
                (abs(dt[,4] - as.numeric(unlist(strsplit(mzrt,"@")))[2]< 2))); 
  } else if(grepl("__", mzrt)) {
      FTID <- which((abs(dt[,1] - as.numeric(unlist(strsplit(mzrt,"__")))[1]) < 1e-4) & 
                (abs(dt[,4] - as.numeric(unlist(strsplit(mzrt,"__")))[2]) < 2)); 
  }
  return(as.integer(FTID));
}

#' featureNO2Num
#' @description convert mzATrt as Feature ID
#' @noRd
#' @author Zhiqiang Pang
FTno2Num <- function(FTno){
  FeatureOrder <- qs::qread("FeatureOrder.qs");  
  return(as.integer(FeatureOrder[FTno]));
}

#' readAdductsList
#' @description readAdductsList for user customization
#' @noRd
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

#' retrieveModeInfo
#' @description retrieveModeInfo
#' @noRd
#' @author Zhiqiang Pang
retrieveModeInfo <- function(){
  plantext <- read.delim("ExecuteRawSpec.sh")
  if(grepl("PerformROIExtraction", plantext) && grepl("PerformParamsOptimization", plantext)){
    return("auto")
  } else {
    return("customized")
  }
}

#' plotSingleXIC
#' @description plotSingleXIC
#' @param mSet 
#' @param featureNum 
#' @param sample 
#' @noRd
plotSingleXIC <- function(mSet = NA, featureNum = NULL, sample = NULL, showlabel = TRUE) {

  if(.on.public.web){
    load("mSet.rda")
  } else {
    if(is.na(mSet)){
      stop("mSet object is required! Should be an mSet generated after Peak Profiling!")
    } else if(!is(mSet, "mSet")) {
      stop("Invalid mSet Object! Please check!")
    }
  }
  
  require(MSnbase);
  require(ggrepel);
  raw_data <- mSet@rawOnDisk;
  samples_names <- raw_data@phenoData@data[["sample_name"]];
  rawFiles <- raw_data@processingData@files;
  
  if(any(raw_data@phenoData@data[["sample_group"]] == "MS2")) {
    samples_names <- samples_names[raw_data@phenoData@data[["sample_group"]] !="MS2"]
    rawFiles <- rawFiles[raw_data@phenoData@data[["sample_group"]] !="MS2"]
  }
  if(mSet@params[["BlankSub"]]){
    samples_names <- 
      samples_names[raw_data@phenoData@data[["sample_group"]] != "BLANK"];
    rawFiles <- 
      rawFiles[raw_data@phenoData@data[["sample_group"]] != "BLANK"];
  }

  if(is.null(sample)) {
    sampleOrder <- 1;
  } else {
    sampleOrder <- which(sample == samples_names);
  }
  
  if(is.null(featureNum)) {
    featureOder <- 1;
  } else {
    featureOder <- as.numeric(featureNum);
  }
 
  # Standard EIC extraction
  resDT <- mSet@peakAnnotation[["camera_output"]];
  errorProne <- is0Feature <- FALSE;
  
  if(is.na(resDT[featureNum, sample])) is0Feature = TRUE;
  
  RawData <- readMSData(files = rawFiles[sampleOrder],
                        mode = "onDisk");
  
  ## Here is to choose correct chrompeak for this specific sample

  mzValue <- resDT[featureOder, "mz"];
  rtValue <- resDT[featureOder, "rt"];
  
  chromPeaks0 <- mSet@peakfilling$msFeatureData$chromPeaks;
  if(is.null(chromPeaks0)){
    # for asari results
    chromPeaks <- resDT[featureNum,]
    orderN <- 1
    js <- rjson::fromJSON(file = paste0(featureNum,".json"))
    colv <- unique(js[["fill"]][js[["label"]] == sample])
    
  } else {
    groupval <- OptiLCMS:::groupval;
    mat <- groupval(mSet);
    
    orderN <- mat[featureOder,sampleOrder];
    if(is.na(orderN)) {
      errorProne <- TRUE;
      orderN <- numeric();
    } else {
      chromPeaks <- as.data.frame(chromPeaks0);
      rtlist <- (mSet@peakRTcorrection$adjustedRT);
      RawData@featureData@data$retentionTime <- rtlist[[sampleOrder]];
    }
    
    if(length(orderN) == 0) {
      
      chromPeaks0 <- mSet@peakpicking[["chromPeaks"]];
      chromPeaks <- as.data.frame(chromPeaks0[(chromPeaks0[,"sample"] == sampleOrder),])
      orderN <- which(chromPeaks$mzmin-0.02 < mzValue & 
                        chromPeaks$mzmax+0.02 > mzValue & 
                        chromPeaks$rtmin-5 < rtValue &
                        chromPeaks$rtmax+5 > rtValue);
    }
    
    if(length(orderN) == 0){
      # EIC extraction to handle the filled peaks from gap filling step
      
      rtlist <- (mSet@peakRTcorrection$adjustedRT);
      RawData@featureData@data$retentionTime <- rtlist[[sampleOrder]];
      chromPeaks0 <- mSet@peakAnnotation[["peaks"]];
      chromPeaks <- as.data.frame(chromPeaks0[(chromPeaks0[,"sample"] == sampleOrder),])
      orderN <- which(chromPeaks$mzmin-0.02 < mzValue & 
                        chromPeaks$mzmax+0.02 > mzValue & 
                        chromPeaks$rtmin-5 < rtValue &
                        chromPeaks$rtmax+5 > rtValue)
    } 
    
    if(length(orderN) == 0) {
      # EIC extraction of some corner cases, just in case.
      
      errorProne <- TRUE;
      rawList <- mSet@peakgrouping[[2]]@listData;
      rawList[["peakidx"]] <- NULL;
      orderN <- which((abs(rawList[["mzmed"]] - mzValue) < 2e-3)
                      & (abs(rawList[["rtmed"]] - rtValue) < 3));
      chromPeaks <- rawList;
    }
    
    js <- rjson::fromJSON(file = paste0(featureNum,".json"))
    colv <- unique(js[["fill"]][samples_names == sample])
  }
  
  if(is0Feature & errorProne) {
    minRT <- chromPeaks$rtmin[orderN];
    maxRT <- chromPeaks$rtmax[orderN];
    RTi <- 0.2;
    res <- 
      data.frame(Retention_Time = c(minRT-4*RTi, minRT-3*RTi,
                                    minRT-2*RTi, minRT-1*RTi,
                                    rtValue,  
                                    maxRT + 1*RTi, maxRT + 2*RTi,
                                    maxRT + 3*RTi, maxRT + 4*RTi),
                 Intensity = c(0,0,0,0,0,0,0,0,0),
                 colorv = colv,
                 sample = sample)
  } else {
    
    minMZ <- chromPeaks$mzmin[orderN];
    maxMZ <- chromPeaks$mzmax[orderN];
    minRT <- chromPeaks$rtmin[orderN];
    maxRT <- chromPeaks$rtmax[orderN];
    
    if(errorProne) {
      if(maxMZ - minMZ > 0.1) {
        cat("Trigger mz Correction\n")
        minMZ <- mzValue - 0.0025;
        maxMZ <- mzValue + 0.0025;
      }
      if(maxRT - minRT > 30){
        cat("Trigger rt Correction\n")
        minRT <- rtValue + 10;
        maxRT <- rtValue - 10;
      }
    }
    
    RawChrom <- chromatogram(RawData, 
                             mz = c(minMZ - 0.0001, maxMZ + 0.0001),
                             rt = c(minRT - 1, maxRT + 1));
    RawChrom[[1]]->MSTrace;

    minRT <- min(MSTrace@rtime);
    maxRT <- max(MSTrace@rtime);
    RTi <- (maxRT - minRT)/length(MSTrace@rtime);
    if(RTi == 0) RTi <- 0.2;
    res0 <- 
      data.frame(Retention_Time = unname(MSTrace@rtime),
                 Intensity = MSTrace@intensity);
    
    if(any(is.na(res0$Intensity))){
      res0 <- res0[!is.na(res0$Intensity),]
    }
    minRT <- min(res0$Retention_Time);
    maxRT <- max(res0$Retention_Time);
    res <- 
      data.frame(Retention_Time = c(minRT-4*RTi, minRT-3*RTi,
                                    minRT-2*RTi, minRT-1*RTi,
                                    res0$Retention_Time,  
                                    maxRT + 1*RTi, maxRT + 2*RTi,
                                    maxRT + 3*RTi, maxRT + 4*RTi),
                 Intensity = c(0,0,0,0, res0$Intensity,0,0,0,0),
                 colorv = colv[1],
                 sample = sample)
    
    if(any(is.na(res$Intensity))){
      res <- res[!is.na(res$Intensity),]
    }
    
    ## TO handle the big gaps of retention time of EIC
    gapPos <- which(sapply(1:(nrow(res)-1), FUN = function(x){
      res$Retention_Time[x+1] - res$Retention_Time[x] > RTi*20
    }))
    
    if(length(gapPos) > 0) {
      resk <- res;
      for(gp in gapPos){
        r0 <- data.frame(Retention_Time = seq(from = resk$Retention_Time[gapPos], 
                                              to=resk$Retention_Time[gapPos + 1], 
                                              by = RTi*2), 
                         Intensity = 0, 
                         colorv = unique(resk$colorv), 
                         sample = unique(resk$sample))
        res <- rbind(res, r0)
      }
      rm(resk)
    }
    
    if(nrow(res) < 10){
      # To smooth the spike peak
      res$Intensity[nrow(res)-3] <- 
        res$Intensity[4] <- 
        max(res$Intensity)/2;
      res$Intensity[nrow(res)-2] <- 
        res$Intensity[3] <- 
        max(res$Intensity)/4;
      res$Intensity[nrow(res)-1] <- 
        res$Intensity[2] <- 
        max(res$Intensity)/16
    }
    
  }


  if(file.exists(paste0("EIC_layer_", featureOder,".qs"))){
    res1 <- qs::qread(paste0("EIC_layer_", featureOder,".qs"));
    # Remove existing info of the peak of the sample
    if(any(res1$sample == sample)) {res1 <- res1[res1$sample != sample, ]}
    
    res2save <- rbind(res1, res)
    qs::qsave(res2save, file = paste0("EIC_layer_", featureOder,".qs"))
    if(nrow(res1)>0)  res1 <- cbind(res1, alpha1 = 0.03, alpha2 = 0.1);
    resf <- rbind(res1, cbind(res, alpha1 = 0.03, alpha2 = 0.1));
  } else {
    qs::qsave(res, file = paste0("EIC_layer_", featureOder,".qs"));
    resf <- cbind(res, alpha1 = 0.03, alpha2 = 0.1)
  }
  
  # Here we adjust the layer of figure
  resf$sample <- factor(resf$sample, 
                        levels = c(sample, 
                                   unique(resf$sample)[!sample == unique(resf$sample)]));
  # Add labels
  resf <- cbind(resf, label = NA);
  sampleNMs <- unique(as.character(resf$sample));
  row4Label <- sapply(sampleNMs, function(x) {
    
    resk <- resf[as.character(resf$sample) == x,];
    if(all(resk$Intensity == 0)) {return(as.numeric(median(row.names(resk))))}
    
    resk <- resk[resk$Intensity != 0, ]
    if(nrow(resk) > 2) {
      resk <- resk[c(-1, -nrow(resk)),] # Avoid edge extreme value
    }
    maxinto <- max(resk$Intensity)
    labelorder <- which((as.character(resf$sample) == x) & (resf$Intensity == maxinto))[1];
    return(labelorder)
    #ceiling(median(which(as.character(resf$sample) == x)))
    })
  resf[row4Label, 7] <- sampleNMs;
  
  peak_width <- (maxRT - minRT) + 6
  require(ggplot2)
  s_image0 <- ggplot(
    resf, 
    aes(x = Retention_Time, 
        y = Intensity,
        label = label)) +
    stat_smooth(
      aes(fill = colorv,
          alpha = alpha1, 
          group = sample),
      geom = 'area',
      method = "loess",
      se = FALSE,
      span = 0.25,
      formula = "y ~ x"
      ) + 
    stat_smooth(
      aes(color = colorv,
          alpha = alpha2, 
          group = sample),
      method = "loess",
      se = FALSE,
      span = 0.25,
      linewidth = 0.35,
      formula = "y ~ x") + 
    scale_color_identity() + 
    scale_fill_identity() 
  if(showlabel) {
    s_image0 <- s_image0 + 
      geom_text_repel(aes(x = Retention_Time, 
                          y = Intensity*0.8,
                          color = colorv), 
                      force = 1.5)
  }
  title = paste0(sample,"_",featureNum);
  title0 <- paste0(round(mzValue, 4), "mz@", round(rtValue, 2),"s");
  
  s_image <- s_image0 +
    theme_bw() +
    ylim(0, NA) +
    xlim(min(resf$Retention_Time) - 2 , 
         max(resf$Retention_Time) + 2) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 12,
        face = "bold",
        hjust = 0.5
      ),
      legend.title = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    ggtitle(title0)
  
  s_image[["plot_env"]][["raw_data"]] <- 
    s_image[["plot_env"]][["chromPeaks"]] <- 
    s_image[["plot_env"]][["chromPeaks0"]] <- 
    s_image[["plot_env"]][["MSTrace"]] <- 
    s_image[["plot_env"]][["raw_data"]] <- 
    s_image[["plot_env"]][["RawChrom"]] <- 
    s_image[["plot_env"]][["RawData"]] <- 
    s_image[["plot_env"]][["rtlist"]] <- 
    s_image[["plot_env"]][["mSet"]] <- 
    s_image[["plot_env"]][["js"]] <- NULL;
  
  qs::qsave(s_image, file = paste0("EIC_image_", featureOder,".qs"))
  fileName = paste0("EIC_", title, "_group_", "72", ".", "png");
  
  if (.on.public.web) {
    Cairo::Cairo(
      file = fileName,
      unit = "in",
      dpi = 72,
      width = 6,
      height = 6,
      type = "png",
      bg = "white"
    )
  }
  
  plot(s_image);
  
  if (.on.public.web) {
    dev.off()
  }
  
  cat("Ploting EIC successfully!\n")
  return(fileName)
}

PlotXICUpdate <- function(featureNum, format = "png", dpi = 72, width = NA){
  if(is.na(width)){
    width <- 6;
  }
  
  s_image <- qs::qread(file = paste0("EIC_image_", featureNum,".qs"))
  fileName = paste0("EIC_", featureNum, "_updated_", dpi, ".", format);
  
  if (.on.public.web) {
    Cairo::Cairo(
      file = fileName,
      unit = "in",
      dpi = dpi,
      width = width,
      height = width*1,
      type = format,
      bg = "white"
    )
  }
  
  plot(s_image);
  
  if (.on.public.web) {
    dev.off()
  }
  
  return(fileName)
}

cleanEICLayer <- function(featureNum) {
  if(file.exists(paste0("EIC_layer_", featureNum,".qs"))) {
   file.remove(paste0("EIC_layer_", featureNum,".qs")) 
  }
}

getMalariaRawData <- function(homedir) {
  print("Downloading raw Malaria Data.... ")
  datalink <- "https://www.dropbox.com/s/kwwe8q2yvsvbcf5/malaria.zip";
  desfile <- paste0(homedir,"/malaria.zip");
  markerfile <- paste0(homedir, "/upload/QC/QC_001.mzML");
  download.file(datalink, 
                destfile = desfile, 
                method = "wget", quiet = TRUE);
  unzip(zipfile = desfile, exdir = paste0(homedir, "/upload"))
  
  # do some verification here
  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    download.file(datalink, 
                  destfile = desfile, 
                  method = "libcurl", quiet = TRUE);
    try(unzip(zipfile = desfile, exdir = paste0(homedir, "/upload")),silent = TRUE)
  }

  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    download.file(datalink, 
                  destfile = desfile, 
                  method = "libcurl", quiet = TRUE);
    try(unzip(zipfile = desfile, exdir = paste0(homedir, "/upload")),silent = TRUE)
  } 
  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    download.file(datalink, 
                  destfile = desfile, 
                  method = "libcurl", quiet = TRUE);
    try(unzip(zipfile = desfile, exdir = paste0(homedir, "/upload")),silent = TRUE)
  }
  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    cmd <- paste0("wget -P ", homedir, " https://www.dropbox.com/s/kwwe8q2yvsvbcf5/malaria.zip")
    try(system(cmd),silent = TRUE)
    try(unzip(zipfile = desfile, exdir = paste0(homedir, "/upload")),silent = TRUE)
  }
  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    print("Downloading failed, please contact Zhiqiang Pang! ")
    return(0)
  }
}

################## ------------- Shell function here below ------------- ######################

#' InitializePlan
#' @description this function is used to initialize a plan before submit job to spring daemon
#' @author Zhiqiang Pang
#' @export
InitializaPlan <- function(){
  plan <- OptiLCMS::InitializaPlan();
  # Nothing to return
}

#' CentroidCheck
#' @description CentroidCheck function used to check centroid or not 
#' @author Zhiqiang Pang
#' @param filename filename to check
#' @export
CentroidCheck <- function(filename){
  return(OptiLCMS::CentroidCheck(filename));
}

GenerateParamFile <- function(){
  
  if(!file.exists("param_optimized.txt")){
    file.copy("param_default.txt", "param_file.txt", overwrite = TRUE)
    return(1)
  } else {
    opparam <- read.table("param_optimized.txt");
    if(nrow(opparam) > 5){
      dfparam <- read.table("param_default.txt");
      res <- vapply(1:nrow(dfparam), FUN = function(x){
        opparam[x,2] == dfparam[x,2]
      }, FUN.VALUE = vector(mode = "logical", length = 1))
      if(all(res)){
        file.copy("param_default.txt", "param_file.txt", overwrite = TRUE)
        return(1)
      } else {
        mgparam <- merge(dfparam, opparam, by = "V1");
        colnames(mgparam) <- c("Parameters", "Default", "Optimized");
        write.table(mgparam, file = "param_file.txt", quote = F, row.names = F)
      }
      return(1)
    } else {
      checksh <- read.csv("ExecuteRawSpec.sh", sep = "\n")[8,];
      if(grepl("PerformParamsOptimization", checksh)){
        write.table("Optimization is in progress.. Please download later... ", 
                    file = "param_file.txt", quote = F, col.names = F, row.names = F)
        return(1)
      } else {
        file.copy("param_default.txt", "param_file.txt", overwrite = TRUE)
        return(1)
      }
    }
  }
  return(0)
}


#' SetPeakParam
#' @description SetPeakParam, used to set the peak param 
#' @param platform platform
#' @param Peak_method Peak_method
#' @param RT_method RT_method
#' @param mzdiff mzdiff
#' @param snthresh snthresh
#' @param bw bw
#' @param ppm ppm
#' @param min_peakwidth min_peakwidth
#' @param max_peakwidth max_peakwidth
#' @param noise noise
#' @param prefilter prefilter
#' @param value_of_prefilter value_of_prefilter
#' @param fwhm fwhm
#' @param steps steps
#' @param sigma sigma
#' @param peakBinSize peakBinSize
#' @param max max
#' @param criticalValue criticalValue
#' @param consecMissedLimit consecMissedLimit
#' @param unions unions
#' @param checkBack checkBack
#' @param withWave withWave
#' @param profStep profStep
#' @param minFraction minFraction
#' @param minSamples minSamples
#' @param maxFeatures maxFeatures
#' @param mzCenterFun mzCenterFun
#' @param integrate integrate
#' @param extra extra
#' @param span span
#' @param smooth smooth
#' @param family family
#' @param fitgauss fitgauss
#' @param polarity polarity
#' @param perc_fwhm perc_fwhm
#' @param mz_abs_iso mz_abs_iso
#' @param max_charge max_charge
#' @param max_iso max_iso
#' @param corr_eic_th corr_eic_th
#' @param mz_abs_add mz_abs_add
#' @param adducts adducts
#' @param rmConts rmConts
#' @author Zhiqiang Pang
#' @export
SetPeakParam <- function(platform = "general", Peak_method = "centWave", RT_method = "loess",
                         mzdiff, snthresh, bw, # used for both "centwave" and "matchedFilter"
                         ppm, min_peakwidth, max_peakwidth, noise, prefilter, value_of_prefilter, # used for "centwave"
                         fwhm, steps, sigma, peakBinSize, max, # used for "matchedFilter"
                         criticalValue, consecMissedLimit, unions, checkBack, withWave, # used for "massifquant"
                         profStep, # used for "obiwarp"
                         minFraction, minSamples, maxFeatures, mzCenterFun, integrate,# used for grouping
                         extra, span, smooth, family, fitgauss, # used for RT correction with peakgroup "loess"
                         polarity, perc_fwhm, mz_abs_iso, max_charge, max_iso, corr_eic_th, mz_abs_add, adducts, #used for annotation
                         rmConts, #used to control remove contamination or not
                         BlankSub
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
                           rmConts, BlankSub
  )
  #return nothing
}

#' GeneratePeakList
#' @description GeneratePeakList is used to generate the peak summary list for result page
#' @author Zhiqiang Pang
#' @export
#' @param userPath userPath
GeneratePeakList <- function(userPath) {

  setwd(userPath)
  
  while(!file.exists("mSet.rda")){
    #cat("waiting data writing done...")
    Sys.sleep(3)
  }
  load("mSet.rda")
  ## Claculate the mean internsity of all groups
  sample_data <-mSet@dataSet;

  groups <- as.character(as.matrix(sample_data[1, ]))[-1]
  sample_data <- sample_data[-1, -1]
 
  if (length(unique(groups)) == 1) {
    sample_data_mean  <-
      apply(
        sample_data,
        1,
        FUN = function(x) {
          mean(as.numeric(x), na.rm = TRUE)
        }
      )
  } else {
    sample_data1 <- matrix(nrow = nrow(sample_data))
    
    for (i in seq_along(unique(groups))) {
      columnnum <- unique(groups)[i] == groups
      sample_data0  <-
        subset.data.frame(sample_data, subset = TRUE, select = columnnum)
      
      sample_data0  <-
        round(apply(
          sample_data0,
          1,
          FUN = function(x) {
            mean(as.numeric(x), na.rm = TRUE)
          }
        ), 2)
      
      sample_data1 <- cbind(sample_data1, sample_data0)
    }
    sample_data_mean <- sample_data1[, -1]
    colnames(sample_data_mean) <- unique(groups)
  }

  ## Prepare other information
  while(!file.exists("annotated_peaklist.rds")){
    cat("waiting data writing done...")
    Sys.sleep(3)
  }
  ann_data <- readRDS("annotated_peaklist.rds");
  ann_data <-
    ann_data[, c(1, 4, ncol(ann_data) - 4, ncol(ann_data) - 5, ncol(ann_data) -1 , ncol(ann_data)-2, ncol(ann_data))];
  ann_data[, 1] <- round(ann_data[, 1], 4);
  ann_data[, 2] <- round(ann_data[, 2], 2);
  
  # run t-test or annova
  data <- matrix(as.numeric(as.matrix(sample_data)), ncol = ncol(as.matrix(sample_data)));
  LogNorm<-function(x, min.val){
    log10((x + sqrt(x^2 + min.val^2))/2)
  }
  CalCV<- function(x){
    x <- as.numeric(x)
    sd(x)/mean(x)
  }
  min.val <- min(abs(data[data!=0]))/10;
  data<-apply(data, 2, LogNorm, min.val);
  
  sample_data_log <- data;
  cvs <- round(apply(data, 1,FUN = CalCV),4)*100
  lvls <- groups[groups != "QC"];
  sample_data_log <- sample_data_log[,groups != "QC"];
  
  groups <- as.factor(lvls);
  if(length(unique(groups)) != 1){
    tt.res <- PerformFastUnivTests(t(sample_data_log), groups, var.equal=TRUE);
    pvals <-tt.res$p.value;
    pvals[is.nan(pvals)] <- 1;
    pfdr <-p.adjust(pvals, method = "fdr")
    pvals <- signif(pvals, 8)
    pfdr <- round(signif(pfdr, 8), 8)
    FeatureOrder <- order(pvals)
    intenVale <- round(apply(sample_data_mean, 1, mean),1)
  } else {
    pfdr <- pvals <- rep(-10, nrow(sample_data_log));
    #featureIntSum <- apply(sample_data_log, 1, sum);
    intenVale <- round(sample_data_mean,1);    
    if(is.null(dim(intenVale))){
      FeatureOrder <- order(intenVale, decreasing = TRUE);
    } else {
      FeatureOrder <- order(intenVale[,1], decreasing = TRUE);
    }
  }
  qs::qsave(mSet@peakAnnotation[["Formula2Cmpd"]][FeatureOrder], 
            file = "formula2cmpd.qs")

  my.dat <- cbind(ann_data, intenVale, pvals, pfdr, cvs);
  my.dat <- my.dat[FeatureOrder,];
  qs::qsave(FeatureOrder, file = "FeatureOrder.qs")
  write.table(my.dat, sep = "\t",
              file = "peak_feature_summary.tsv",
              row.names = FALSE,
              quote = FALSE);
  write.csv(my.dat, 
            file = "peak_feature_summary.csv", 
            quote = TRUE, 
            row.names = FALSE);
  feat.num <- nrow(ann_data);
  cat("Total number of features is ", feat.num, "\n")
  return(feat.num);
}


generateAsariPeakList <-  function(userPath) {
  
  setwd(userPath)
  
  alfs <- list.files(".", pattern = "results_metabo_asari_res")
  alfs_idx <- as.numeric(gsub("results_metabo_asari_res_","",alfs))
  result_folder <- alfs[which.max(alfs_idx)];
  
  # generate metaboanalyst table
  load("mSet.rda")
  ftable <- read.csv(paste0(result_folder, "/preferred_Feature_table.tsv"), sep = "\t")
  features <- paste0(ftable$mz, "__", ftable$rtime)
  ftable1 <- ftable[,c(12:ncol(ftable))]
  allSamples <- colnames(ftable1)
  allGroups <- 
    vapply(allSamples, FUN = function(x){
      idx <- which(mSet@rawOnDisk@phenoData@data[["sample_name"]] == x)
      mSet@rawOnDisk@phenoData@data[["sample_group"]][idx]
    }, character(1L))
  ftable2 <- t(data.frame(Groups = allGroups))
  ftable3 <- data.frame(Samples = c("Groups", features))
  ftable0 <- rbind(ftable2, ftable1)
  ftable0 <- cbind(ftable3, ftable0)
  rownames(ftable0) <- NULL;
  mSet@dataSet <- ftable0;
  write.csv(ftable0, file = "metaboanalyst_input.csv", row.names = F, quote = F)
  
  # generate peak_feature_summary
  ftab_annotation <- read.csv(paste0(result_folder, "/Feature_annotation.tsv"), sep = "\t")
  idx_num <- ftable$id_number
  idx_row <- vapply(idx_num, FUN = function(x){
    which(ftab_annotation[,1] == x)
  }, FUN.VALUE = integer(1L))
  ftab_annotation <- ftab_annotation[idx_row, ]
  
  annots <- strsplit(ftab_annotation[,6], ",")
  adds <- vapply(annots, function(x){
    if(length(x) == 0){
      return("")
    } else {
      return(x[2])
    }
  }, FUN.VALUE = character(1L))
  isos <- vapply(annots, function(x){
    if(length(x) == 0){
      return("")
    } else {
      return(x[1])
    }
  }, FUN.VALUE = character(1L))
  #all_annots <- rjson::fromJSON(file = paste0(result_folder, "/Annotated_empiricalCompounds.json"))
  
  all_recrds <- ftab_annotation$matched_DB_records
  all_forumus <- sapply(all_recrds, function(x){
    res <- strsplit(x, "\\), \\(")
    if(length(res[[1]]) == 0){
      return("")
    } else {
      res <- gsub("\\(|\\)", "", res[[1]])
      res2 <- vapply(res, FUN = function(y){
        strsplit(strsplit(y, "'")[[1]][2], "_")[[1]][1]
      }, character(1L), USE.NAMES = F)
      if(length(res2) == 1){
        res2 <- paste0(res2, ";")
      } else {
        res2 <- paste0(res2, collapse = "; ")
      }
      return(res2)
    }
  })
  
  all_cmpds <- ftab_annotation$matched_DB_shorts
  all_cmpd <- sapply(all_cmpds, function(x){
    res <- strsplit(x, "\\), \\(")
    if(length(res[[1]]) == 0){
      return("")
    } else {
      res <- gsub("\\(|\\)", "", res[[1]])
      res2 <- lapply(res, FUN = function(y){
        w <- strsplit(y, ";")[[1]]
        strsplit(w, "\\$")
      })
      
      res2_done <- vapply(res2, function(nn){
        if(length(nn)==1){
          nn[[1]][2]
        } else {
          res2x <- vector(mode = "character", length = length(nn))
          for(kk in 1:length(nn)){
            res2x[kk] <- nn[[kk]][2]
          }
          return(paste0(res2x, collapse = "; "))
        }
      }, FUN.VALUE = character(1L))
      
      if(length(res2_done) == 1){
        res2_done <- paste0(res2_done, ";")
      } else {
        res2_done <- paste0(res2_done, collapse = "; ")
      }
      return(res2_done)
    }
  })
  all_hmdb <- sapply(all_cmpds, function(x){
    res <- strsplit(x, "\\), \\(")
    if(length(res[[1]]) == 0){
      return("")
    } else {
      res <- gsub("\\(|\\)", "", res[[1]])
      res2 <- lapply(res, FUN = function(y){
        w <- strsplit(y, ";")[[1]]
        strsplit(w, "\\$")
      })
      
      res2_done <- vapply(res2, function(nn){
        if(length(nn)==1){
          nn[[1]][1]
        } else {
          res2x <- vector(mode = "character", length = length(nn))
          for(kk in 1:length(nn)){
            res2x[kk] <- nn[[kk]][2]
          }
          return(paste0(res2x, collapse = "; "))
        }
      }, FUN.VALUE = character(1L))
      
      if(length(res2_done) == 1){
        res2_done <- paste0(res2_done, ";")
      } else {
        res2_done <- paste0(res2_done, collapse = "; ")
      }
      return(res2_done)
    }
  })
  
  Formula2Cmpd_list <- lapply(1:length(all_recrds), function(x){
    res <- list()
    if(ftab_annotation$matched_DB_records[x] == ""){
      return(res)
    }
    
    fms <- ftab_annotation$matched_DB_records[x]
    res <- strsplit(fms, "\\), \\(")
    res <- gsub("\\(|\\)", "", res[[1]])
    res2 <- vapply(res, FUN = function(y){
      strsplit(strsplit(y, "'")[[1]][2], "_")[[1]][1]
    }, character(1L), USE.NAMES = F)
    res2_ori <- res2
    res2 <- unique(res2)
    res <- rep(list(list(cmpd = vector(mode = "character"),
                         hmdb = vector(mode = "character"))), length(res2))
    
    names(res) <- res2
    
    this_cmpd <- ftab_annotation$matched_DB_shorts[x]
    #cmpd
    resx <- strsplit(this_cmpd, "\\), \\(")
    if(length(resx[[1]]) == 0){
      
    } else {
      resx <- gsub("\\(|\\)", "", resx[[1]])
      res2cmpd <- lapply(resx, FUN = function(y){
        w <- strsplit(y, ";")[[1]]
        strsplit(w, "\\$")
      })
      
      res2_done <- lapply(res2cmpd, function(nn){
        if(length(nn)==1){
          nn[[1]][2]
        } else {
          res2x <- vector(mode = "character", length = length(nn))
          for(kk in 1:length(nn)){
            res2x[kk] <- nn[[kk]][2]
          }
          return(res2x)
        }
      })
      
      for(nn in 1:length(res2_done)){
        res[[res2_ori[nn]]][["cmpd"]] <- unique(c(res[[res2_ori[nn]]][["cmpd"]], res2_done[[nn]]))
      }
    }
    #hmdb
    resy <- strsplit(this_cmpd, "\\), \\(")
    if(length(resy[[1]]) == 0){
      
    } else {
      resy <- gsub("\\(|\\)", "", resy[[1]])
      res2hmdb <- lapply(resy, FUN = function(y){
        w <- strsplit(y, ";")[[1]]
        strsplit(w, "\\$")
      })
      
      res2_done <- lapply(res2hmdb, function(nn){
        if(length(nn)==1){
          nn[[1]][1]
        } else {
          res2x <- vector(mode = "character", length = length(nn))
          for(kk in 1:length(nn)){
            res2x[kk] <- nn[[kk]][1]
          }
          return(res2x)
        }
      })
      
      for(nn in 1:length(res2_done)){
        res[[res2_ori[nn]]][["hmdb"]] <- unique(c(res[[res2_ori[nn]]][["hmdb"]], res2_done[[nn]]))
      }
    }
    
    return(res)
  })
  
  mSet@peakAnnotation[["Formula2Cmpd"]] <- Formula2Cmpd_list
  
  # peak_feature_summary
  LogNorm<-function(x, min.val){
    log10((x + sqrt(x^2 + min.val^2))/2)
  }
  CalCV<- function(x){
    x <- as.numeric(x)
    sd(x)/mean(x)
  }
  ftable1 -> data
  allGroups -> groups
  
  min.val <- min(abs(data[data!=0]))/10;
  data<-apply(data, 2, LogNorm, min.val);
  
  sample_data_log <- data;
  cvs <- round(apply(data, 1,FUN = CalCV),4)*100
  lvls <- groups[groups != "QC"];
  sample_data_log <- sample_data_log[,groups != "QC"];
  groups <- as.factor(lvls);
  
  ttest_res <- PerformFastUnivTests(t(sample_data_log), as.factor(groups))
  pvals <- ttest_res[,2]
  pfdr <-p.adjust(pvals, method = "fdr")
  pvals <- signif(pvals, 8)
  pfdr <- round(signif(pfdr, 8), 8)
  
  pvals[is.nan(pvals)] = 1
  pfdr[is.nan(pfdr)] = 1
  
  my.dat <- data.frame(mz = ftab_annotation$mz,
                       rt = ftab_annotation$rtime,
                       adduct = adds,
                       isotopes = isos,
                       Formula = all_forumus,
                       Compound = all_cmpd,
                       HMDBID = all_hmdb,
                       intenVale = ftable$peak_area,
                       pvals = pvals,
                       pfdr = pfdr,
                       cvs = cvs)
  
  HMDBIDs <- my.dat$HMDBID
  HMDBIDs[HMDBIDs==""] <- NA
  mSet@peakAnnotation[["massMatching"]] <- data.frame(Compound = my.dat$Compound, 
                                                      Formula = my.dat$Formula, 
                                                      HMDBID = HMDBIDs)
  
  write.table(my.dat, sep = "\t",
              file = "peak_feature_summary.tsv",
              row.names = FALSE,
              quote = FALSE);
  write.csv(my.dat, 
            file = "peak_feature_summary.csv", 
            quote = TRUE, 
            row.names = FALSE);
  
  FeatureOrder <- order(pvals)
  qs::qsave(mSet@peakAnnotation[["Formula2Cmpd"]][FeatureOrder], 
            file = "formula2cmpd.qs")
  
  
  # generate peak_result_summary
  mzrange <- vapply(allSamples, FUN = function(x){
    ftab_annotation$mz -> mzs;
    idx <- which(colnames(ftable1) == x)
    min_mz <- round(min(mzs[ftable1[,idx] != 0]), 4)
    max_mz <- round(max(mzs[ftable1[,idx] != 0]), 4)
    paste0(min_mz, "~", max_mz)
  }, FUN.VALUE = character(1L));
  
  rtrange <- vapply(allSamples, FUN = function(x){
    ftab_annotation$rtime -> rts;
    idx <- which(colnames(ftable1) == x)
    min_rt <- round(min(rts[ftable1[,idx] != 0]), 2)
    max_rt <- round(max(rts[ftable1[,idx] != 0]), 2)
    paste0(min_rt, "~", max_rt)
  }, FUN.VALUE = character(1L))
  
  peak_num <- vapply(allSamples, FUN = function(x){
    idx <- which(colnames(ftable1) == x)
    length(which(ftable1[,idx] != 0))
  }, FUN.VALUE = integer(1L))
  
  peak_ratio_missing <- vapply(allSamples, FUN = function(x){
    idx <- which(colnames(ftable1) == x)
    round(1-length(which(ftable1[,idx] != 0))/nrow(ftable1),4)*100
  }, FUN.VALUE = double(1L))
  
  datam <- data.frame(samples = allSamples,
                      groups = allGroups,
                      rtrange = rtrange,
                      mzrange = mzrange,
                      peak_num = peak_num,
                      missing = peak_ratio_missing)
  
  mSet@peakAnnotation[["peak_result_summary"]] <- as.matrix(datam)
  
  write.table(
    datam,
    file = paste0("peak_result_summary.txt"),
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  );
  
  feat.num <- nrow(ftable1);
  cat("Total number of features is ", feat.num, "\n")
  return(feat.num);
}


#' plotSingleTIC
#' @description plotSingleTIC is used to plot single TIC
#' @param filename filename
#' @param imageNumber imageNumber
#' @param format format
#' @param dpi dpi
#' @param width width
#' @author Zhiqiang Pang
#' @export
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

  require(ggplot2);
  require(ggrepel);
  require(MSnbase);
  #OptiLCMS::plotSingleTIC(NULL, filename, imgName, format = format, dpi = dpi, width = width);
  #load("tics.rda");
  load("mSet.rda");
  bpVec <- mSet@rawOnDisk@featureData@data[["basePeakMZ"]];
  raw_data_filt <-
    MSnbase::filterFile(mSet@rawOnDisk, file = 
                 basename(mSet@rawOnDisk@processingData@files[
                   which(sub(pattern = "(.*)\\..*$", replacement = "\\1", 
                             basename(mSet@rawOnDisk@processingData@files)) == filename)
                   ]))
  tics <- chromatogram(raw_data_filt, aggregationFun = "sum")
  
  samplels <- tics@phenoData@data$sample_name;
  al <- sapply(tics, FUN = function(x) x);
  ticls <- al[[which(filename == samplels)]];

  minRT <- min(ticls@rtime);
  maxRT <- max(ticls@rtime);
  RTi <- (maxRT - minRT)/5;
  ## Split the chromatogram as 6 regions and select 3 top peaks from each
  dt <- data.frame(Retention_Time = rtime(ticls), Intensity = intensity(ticls), mz = NA);
  #thresh <- min(mSet@rawOnDisk@featureData@data$totIonCurrent)/10
  labPos <- sapply(1:5, function(x){
    maxVec <- vector();
    ovec <- which(minRT + RTi*x >= ticls@rtime & ticls@rtime >= minRT + RTi*(x-1));
    allPeaks0 <- ovec[find_peaks(dt$Intensity[ovec],1)]
    binsize <- length(allPeaks0) %/% 3;
    maxVec <- c(maxVec, head(allPeaks0, binsize)[which.max(dt$Intensity[head(allPeaks0, binsize)])]);
    maxVec <- c(maxVec, allPeaks0[binsize:(binsize*2)][which.max(dt$Intensity[allPeaks0[binsize:(binsize*2)]])]);
    maxVec <- c(maxVec, tail(allPeaks0, binsize)[which.max(dt$Intensity[tail(allPeaks0, binsize)])]);
    maxVec;
  })
  dim(labPos) <- NULL;
  labPos <- labPos[dt$Intensity[labPos] > mean(dt$Intensity)*2/3]
  
  #dt <- data.frame(Retention_Time = rtime(ticls), Intensity = intensity(ticls), mz = NA);
  dt$mz[labPos] <- round(bpVec[labPos],4);
  ydrift <- ceiling(max(dt$Intensity)/8)
  
  p <- ggplot(dt, aes(x = Retention_Time, y = Intensity, label = mz)) + 
    geom_line()  + 
    geom_text(aes(x = Retention_Time, 
                        y = Intensity + ydrift),
              color = "darkblue",
              angle = 90,
              size = 3.75) + 
    ggtitle(filename) + theme_bw() +
    theme(legend.position = "none", 
          plot.title = element_text(size = 12, hjust = 0.5)) + 
    ylim(NA, max(dt$Intensity) + ydrift*1.85)
  
  if (.on.public.web) {
    Cairo::Cairo(
      file = imgName,
      unit = "in",
      dpi = dpi,
      width = width,
      height = width/7*5,
      type = format,
      bg = "white"
    )
  }
  
  print(p)
  
  if (.on.public.web) {
    dev.off()
  }
  
  return(imgName);
}

findPeaks <- function (x, thresh = 0) {
  require(zoo)
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 
                 0) + 2
  if (!missing(thresh)) {
    if (sign(thresh) < 0) 
      thresh <- -thresh
    pks[x[pks - 1] - coredata(x[pks]) > thresh]
  }
  else pks
}

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

#' plotMSfeature
#' @description plotMSfeature is used to plot MS feature stats for different groups
#' @param FeatureNM FeatureNM
#' @param format format
#' @param dpi dpi
#' @param width width
#' @author Zhiqiang Pang
#' @export
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

  title = FeatureNM;
  if(file.exists(paste0("EIC_layer_", FeatureNM,".qs"))){
    file.remove(paste0("EIC_layer_", FeatureNM,".qs"))
  }

  if(format == "png" | format == "pdf" | format == "tiff" | format == "svg" | format == "ps"){
    imgName <- OptiLCMS::plotMSfeature(NULL, FeatureNM, dpi = dpi, format = format, width = width)
    str <- imgName
  }else if(format == "svg2"){
    imgName0 <- OptiLCMS::plotMSfeature(NULL, FeatureNM, dpi = 72, format = "png", width = NA)
    format <- "svg";
    width <- 6;
    imgName <- paste0(title, ".", format);
    require(gridSVG);
    require(ggplot2);
    pdf(NULL, width=width, height=width/6 * 6.18); 
    p <- plotMSfeatureSvg(NULL, FeatureNM, width = width)
    print(p)
    #pcafig.svg <- gridSVG::grid.export(imgNm,addClasses=TRUE,exportCoords="file", exportMappings="file",exportJS="file")
    rootAttrs = c("svgboxplot", "interactiveSvg")
    names(rootAttrs) = c("id", "class");
    fig.svg <- gridSVG::grid.export(name=NULL, indent=F, xmldecl=NULL,addClasses=TRUE, rootAttrs = rootAttrs)
    str <- paste(capture.output(fig.svg$svg, file=NULL), collapse="\n");
    dev.off()

    jsonNm <- paste0(title, ".json");
    p = p + geom_text();
    plotData <- ggplot_build(p)
    for(i in 1:length(plotData$data)){
        df <- plotData$data[[i]]    
      if("outliers" %in% colnames(df)){
        df <- df[ , -which(colnames(df) == "outliers")]
        plotData$data[[i]] <- df
      }
    }
    json.obj <- rjson::toJSON(plotData$data[[3]]);
    sink(jsonNm);
    cat(json.obj);
    sink();
  }
  return(str)
}

#' PlotXIC
#' @description PlotXIC is used to plot both MS XIC/EIC features of different group and samples
#' @author Zhiqiang Pang
#' @param featureNum featureNum
#' @param format format
#' @param dpi dpi
#' @param width width
#' @export
PlotXIC <- function(featureNum, format = "png", dpi = 72, width = NA){
  if(is.na(width)){
    width <- 6;
  }
  return(OptiLCMS::PlotXIC(NULL, featureNum, format = format, dpi = dpi, width = width))
}

#' PerformDataInspect
#' @description PerformDataInspect is used to plot 2D/3D structure of the MS data
#' @param datapath data file path
#' @param rt.range retention time range, unit is seconds
#' @param mz.range m/z range
#' @param dimension view dimension, canbe "2D" or "3D"
#' @param res resolution number, higher of the number means higher resolution
#' @author Zhiqiang Pang
#' @export
PerformDataInspect <- function(datapath = NULL,
                               rt.range = c(0,0),
                               mz.range = c(0,0),
                               dimension = "3D",
                               res = 100){
  require(OptiLCMS)
  OptiLCMS::PerformDataInspect(datapath, rt.range,
                               mz.range, dimension,
                               res)
}

#' FastRunningShow_customized
#' @description FastRunningShow_customized is used to showcase the customized running pipeline
#' @author Zhiqiang Pang
#' @noRd
FastRunningShow_customized <- function(fullUserPath){
  metaboanalyst_env <<- new.env();
  OptiLCMS:::FastRunningShow_customized(fullUserPath)
}

#' FastRunningShow_auto
#' @description FastRunningShow_auto is used to showcase the AUTO running pipeline
#' @author Zhiqiang Pang
#' @noRd
FastRunningShow_auto <- function(fullUserPath){
  metaboanalyst_env <<- new.env();
  OptiLCMS:::FastRunningShow_auto(fullUserPath)
}

#' centroidMSData
#' @description centroidMSData is used to centroid MS Data
#' @author Zhiqiang Pang
#' @noRd
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


#' @title Perform ROI Extraction from raw MS data (PerformDataTrimming)
#' @description This function performs the raw data trimming. This function will output 
#' an trimmed MSnExp file to memory or hardisk according to the choice of users must 
#' provide the data path for 'datapath', and optionally provide other corresponding parameters.
#' @param datapath Character, the path of the raw MS data files' or folder's path (.mzXML, .CDF and .mzML) 
#' for parameters training.
#' @param mode Character, mode for data trimming to select the chraracteristic peaks. 
#' Default is 'ssm'. Users could select random trimed according to mz value (mz_random) or 
#' RT value (rt_random). Besides, specific peaks at certain mz (mz_specific) or 
#' RT (rt_specific) could also be extracted. 'none' will not trim the data.
#' @param mz Numeric, mz value(s) for specific selection. Positive values means including (the values 
#' indicted) and negative value means excluding/removing.
#' @param mzdiff Numeric, the deviation (ppm) of mz value(s).
#' @param rt Numeric, rt value for specific selection. Positive values means including 
#' and negative value means excluding.
#' @param rtdiff Numeric, the deviation (seconds) of rt value(s).
#' @param rt.idx Numeric, the relative rt (retention time) range, from 0 to 1. 1 means all retention time
#' will be retained, while 0 means none. Default is 1/15. If default rt.idx produce too few peaks, 
#' please consider increasing this value.
#' @param write Logical, if true, will write the trimmed data to the directory 'trimmed' folder 
#' in the datapath. The data in memory will be kept.
#' @param plot Logical, if TRUE, will plot the chromatogram of the trimmed data.
#' @param rmConts LOgical, whether to exclude/remove the potential contamination for parameters optimization. Default is TRUE.
#' @param running.controller The resuming pipeline running controller. Optional. Don't need to define by hand.
#' @export
#' @return will return an mSet objects with extracted ROI
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

PerformDataTrimming<- function(datapath, mode="ssm", write=FALSE, mz, mzdiff, rt, rtdiff, 
                               rt.idx=1/15, rmConts = TRUE, plot=TRUE, running.controller=NULL){
  require(OptiLCMS)
  OptiLCMS::PerformROIExtraction(datapath, mode=mode, write, mz, mzdiff, rt, rtdiff, 
                       rt.idx, rmConts = rmConts, plot,running.controller);
  
}

#' @title Perform ROI Extraction from raw MS data (PerformDataTrimming)
#' @description This function performs the raw data trimming. This function will output 
#' an trimmed MSnExp file to memory or hardisk according to the choice of users must 
#' provide the data path for 'datapath', and optionally provide other corresponding parameters.
#' @param datapath Character, the path of the raw MS data files' or folder's path (.mzXML, .CDF and .mzML) 
#' for parameters training.
#' @param mode Character, mode for data trimming to select the chraracteristic peaks. 
#' Default is 'ssm'. Users could select random trimed according to mz value (mz_random) or 
#' RT value (rt_random). Besides, specific peaks at certain mz (mz_specific) or 
#' RT (rt_specific) could also be extracted. 'none' will not trim the data.
#' @param mz Numeric, mz value(s) for specific selection. Positive values means including (the values 
#' indicted) and negative value means excluding/removing.
#' @param mzdiff Numeric, the deviation (ppm) of mz value(s).
#' @param rt Numeric, rt value for specific selection. Positive values means including 
#' and negative value means excluding.
#' @param rtdiff Numeric, the deviation (seconds) of rt value(s).
#' @param rt.idx Numeric, the relative rt (retention time) range, from 0 to 1. 1 means all retention time
#' will be retained, while 0 means none. Default is 1/15. If default rt.idx produce too few peaks, 
#' please consider increasing this value.
#' @param write Logical, if true, will write the trimmed data to the directory 'trimmed' folder 
#' in the datapath. The data in memory will be kept.
#' @param plot Logical, if TRUE, will plot the chromatogram of the trimmed data.
#' @param rmConts LOgical, whether to exclude/remove the potential contamination for parameters optimization. Default is TRUE.
#' @param running.controller The resuming pipeline running controller. Optional. Don't need to define by hand.
#' @export
#' @return will return an mSet objects with extracted ROI
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)
PerformROIExtraction<- function(datapath, mode="ssm", write=FALSE, mz, mzdiff, rt, rtdiff, 
                               rt.idx=1/15, rmConts = TRUE, plot=TRUE, running.controller=NULL){
  require(OptiLCMS)
  OptiLCMS::PerformROIExtraction(datapath, mode=mode, write, mz, mzdiff, rt, rtdiff, 
                                 rt.idx, rmConts = rmConts, plot,running.controller);
  
}

#' PerformParamsOptimization
#' @title Perform Parameters Optimization
#' @description This function is used to optimize the critical 
#' parameters of peak picking and alignment for 
#' the following data processing. It utilizes the trimed data and 
#' the internal instrument-specific parameters.
#' Parallel computing will be performed. The number of cores user 
#' want to use could be specified.
#' @param mSet mSet object, usually generated by 'PerformROIExtraction' 
#' or 'PerformDataTrimming' here.
#' @param param List, Parameters defined by 'SetPeakParam' function.
#' @param method Character, method of parameters optimization, including 
#' "DoE' only. Default is "DoE". Other method 
#' is under development.
#' @param ncore Numeric, CPU threads number used to perform the parallel 
#' based optimization. If thers is memory issue,
#' please reduce the 'ncore' used here. For default, 2/3 CPU threads of 
#' total will be used.
#' @param running.controller The resuming pipeline running controller. Optional. 
#' Don't need to define by hand.
#' @export
#' @return will a parameter object can be used for following processing
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} 
#' Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

PerformParamsOptimization <- function(mSet, 
                                      param= NULL, 
                                      method="DoE", 
                                      ncore=4, 
                                      running.controller=NULL){
  require(OptiLCMS)
  return(OptiLCMS::PerformParamsOptimization(mSet = mSet,
                                             param = param,
                                             method = method,
                                             ncore = ncore,
                                             running.controller = running.controller))
  
}


#' PerformPeakProfiling
#' @param mSet mSet
#' @param Params Params
#' @param plotSettings plotSettings
#' @param ncore number of cores
#' @export
PerformPeakProfiling <- function(mSet, Params, plotSettings, ncore) {
  return(OptiLCMS::PerformPeakProfiling(mSet, Params, plotSettings, ncore));
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
  OptiLCMS::PlotSpectraRTadj (mSet = NULL, imgName = imgName, format = format, dpi = dpi, width = width)

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
  OptiLCMS::PlotSpectraBPIadj(mSet = NULL, imgName = imgName, format = format, dpi = dpi, width = width)

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

plotMSfeatureSvg <- function (mSet = NULL, FeatureNM = 1, width = NA) {
    if (is.null(mSet)) {
        if (.on.public.web) {
            load("mSet.rda")
        }
        else {
            stop("No mSet Object found !")
        }
    }
    peakdata <- mSet@peakAnnotation$camera_output
    peakdata1 <- peakdata[, c((-1):-6, -ncol(peakdata), -ncol(peakdata) + 
        1, -ncol(peakdata) + 2, -ncol(peakdata) + 3, -ncol(peakdata) + 
          4, -ncol(peakdata) + 5)]
    peakdata1[is.na(peakdata1)] <- 0
    group_info <- mSet@dataSet[c(1), -1]
    data_table <- as.data.frame(t(
      rbind(
        round(as.numeric(peakdata1[FeatureNM, ]), 1), 
        as.character(as.matrix(group_info)), 
        as.character(as.matrix(names(group_info)))
        )
      ))
    data_table[, 1] <- as.numeric(data_table[, 1])
    colnames(data_table) <- c("value", "Group", "label")
    rownames(data_table) <- NULL

    if (is.na(width)) {
        width = 6
        title = paste0(round(peakdata[FeatureNM, 1], 4), "mz@", 
            round(peakdata[FeatureNM, 4], 2), "s")
    }else {
        title = paste0(round(peakdata[FeatureNM, 1], 4), "mz@", 
            round(peakdata[FeatureNM, 4], 2), "s")
    }
    
    
    lvls <- unique(data_table$Group)
    p1 <- ggplot(data_table, 
                 aes(x = factor(Group, levels =c(lvls[lvls != "QC"], "QC")) , y = log2(value + 1), 
                            fill = Group, label=label)) + 
      stat_boxplot(geom = "errorbar", 
                   width = 0.15, 
                   aes(color = "black")) + 
      geom_boxplot(size = 0.35, 
                   width = 0.5, 
                   fill = "white", 
                   outlier.fill = "white", 
                   outlier.color = "white") + 
      geom_jitter(aes_string(fill = "Group"), 
                  width = 0.2, 
                  shape = 21, 
                  size = 2.5) + 
      scale_color_manual(values = c("black", 
                                    "black", "black", "black", 
                                    "black", "black", "black", 
                                    "black", "black", "black", 
                                    "black", "black", "black")) + 
      theme_bw() + #ylim(log2(min(data_table$value + 1))*2/3, log2(max(data_table$value + 1))*1.15) +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 12, 
                                       angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 12, 
                                   face = "plain"), 
        axis.title.y = element_text(size = 12, 
                                    face = "plain"), 
        axis.title.x = element_text(size = 16, 
                                    face = "plain"), 
        plot.title = element_text(size = 16, 
                                  face = "bold", hjust = 0.5)) + 
      ylab("Intensity~(log2)") + 
      xlab("Groups") + 
      ggtitle(title)

return(p1)
}

PerformResultSummary <- function(mSet = NA) {

  if(is.na(mSet)){load("mSet.rda")}
  IntroMsg <- paste0("MetaboAnalyst has finished raw spectra processing with OptiLCMS (", packageVersion("OptiLCMS"), "): ");
  ProcessingMsg <- paste0("There are ", length(mSet@rawOnDisk@phenoData@data[["sample_name"]]), 
                          " samples of ", length(unique(mSet@rawOnDisk@phenoData@data[["sample_group"]])), 
                          " groups (", paste(unique(mSet@rawOnDisk@phenoData@data[["sample_group"]]), collapse = ", "), ") included for processing!")
  FeatureMsg <- paste0("Total of ", nrow(mSet@dataSet) - 1, " features have been detected and aligned across the whole sample list.")
  ppmMsg <- paste0("The mass deviation of this study was estimated/set as ", mSet@params$ppm, " ppm.")
  
  isoNum <- length(which(mSet@peakAnnotation[["camera_output"]][["isotopes"]] !=""));
  addNum <- length(which(mSet@peakAnnotation[["camera_output"]][["adduct"]] !=""));
  nonNum <- length(which(mSet@peakAnnotation[["camera_output"]][["adduct"]] =="" & 
                           mSet@peakAnnotation[["camera_output"]][["isotopes"]] ==""));

  IsotopMsg <- paste0(isoNum, 
                      " features (",round(isoNum/nrow(mSet@dataSet)*100,2),
                      "%) have been annotated as isotopes.")
  AdductsMsg <- paste0(addNum, 
                       " features (",round(addNum/nrow(mSet@dataSet)*100,2),
                       "%) have been annotated as adducts.")
  NonMsg <- paste0(nonNum, 
                       " features (",round(nonNum/nrow(mSet@dataSet)*100,2),
                       "%) cannot be annotated.")
  
  formulas <- unique(mSet@peakAnnotation$massMatching$Formula);
  formulas <- formulas[formulas != "Unknown"]
  formulass <- unique(unname(unlist(sapply(formulas, FUN = function(x){strsplit(x, "; ")}))))
  
  cmpds <- unique(mSet@peakAnnotation$massMatching$Compound);
  cmpds <- cmpds[cmpds != "Unknown"]
  cmpdss <- unique(unname(unlist(sapply(cmpds, FUN = function(x){strsplit(x, "; ")}))))
  
  formulaMsg <- paste0(length(formulass), 
                       " unique formulas have been matched to HMDB database.")
  cmpdsMsg <- paste0(length(cmpdss), 
                       " potential compounds have been matched to HMDB database.");

  return(c(IntroMsg, ProcessingMsg, FeatureMsg, ppmMsg, IsotopMsg, AdductsMsg, formulaMsg, cmpdsMsg))
}

PerformExtractHMDBCMPD <- function(formula, featureOrder) {

  res <- qs::qread("formula2cmpd.qs")
  thisRes <- res[[featureOrder]][[formula]]
  hmdbids <- thisRes$hmdb;
  cmpds <- thisRes$cmpd;
  cmpdsnew <- ""
  for (i in seq(thisRes$cmpd)) {
    cmpdsnew <- paste0(cmpdsnew, 
                       "<a href=http://www.hmdb.ca/metabolites/" , 
                       hmdbids[i] , 
                       " target='_blank'>" , 
                       cmpds[i] , "</a>; ")
  }

  return(cmpdsnew)
}

checkdataexits <- function(fileNM){
  i <- 0;
  cat(fileNM, " exists? ", file.exists(fileNM), "\n")
  if(file.exists(fileNM)){
    return(1);
  }
  while(!file.exists(fileNM)){
    i <- i + 1;
    Sys.sleep(3)
    cat(fileNM, ' not exists, waiting for writing done!\n')
    if(i > 20){
      return (0)
    }
  }
  return(1)
}

PerformSWATHDesignDetection <- function(mSetObj=NA, file = ""){
  mSetObj <- .get.mSet(mSetObj);
  DetectMS2Design <- OptiLCMS:::DetectMS2Design;
  df <- DetectMS2Design(file)
  cat("====== df ----> ", nrow(df), "\n")
  mSetObj[["dataSet"]][["swath_design"]] <- df
  return(.set.mSet(mSetObj));
}

GetSWATHDesginLow <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  df <- mSetObj[["dataSet"]][["swath_design"]]
  if(nrow(df) == 0){
    return(0)
  }
  vecs <- as.double(df[,1])
  return(vecs)
}

GetSWATHDesginUp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  df <- mSetObj[["dataSet"]][["swath_design"]]
  if(nrow(df) == 0){
    return(0)
  }
  vecs <- as.double(df[,2])
  return(vecs)
}

exportSwathDesign <- function(lowMZs, UpMZs){
  vecs <- character(length = length(lowMZs))
  for(i in seq(lowMZs)){
    vecs[i] <- paste(lowMZs[i], UpMZs[i], sep = "\t")
  }
  write.table(vecs, file = "swath_design_metaboanalyst.txt", quote = F, sep = "\n", col.names = F, row.names = F)
  return(1)
}
