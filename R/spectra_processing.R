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
  
  if(dir.exists("/home/glassfish/payara6/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  }else {
    users.path <-getwd();
  }
  
  ## create algorithm marker
  if(grepl("ROIExtraction", planString)){
    write.table("centWave_auto", file = "ms1_algorithm.txt", quote = F, sep = "", col.names = F, row.names = F)
  } else {
    write.table("centWave_manual", file = "ms1_algorithm.txt", quote = F, sep = "", col.names = F, row.names = F)
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
  str <- paste0(str, ';\n',  "Export.PeakSummary(res[['mSet']])")
  str <- paste0(str, ';\n',  "Export.PeakTable(res[['mSet']])")
  
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
  
  if(dir.exists("/home/glassfish/payara6/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  } else {
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
  
  # parse param string
  # planString <- "ppm1:5.0;ppm2:10.0;filtering:200.0;targets_opt:sigs;db_opt:[hmdb_exp, hmdb_pre, gnps];win_size:1.5;similarity_method:dot_product;ionMode:positive;intensity_threshold:10000.0;enabledDDADeco:true"
  param_strs <- strsplit(planString, ";")[[1]]
  param_list <- strsplit(param_strs, ":")
  names(param_list) <- vapply(param_list, function(x){x[1]}, FUN.VALUE = character(1L))
  param_list <- lapply(param_list, function(x){x[2]})
  if(param_list$enabledDDADeco == "true"){
    param_list$enabledDDADeco <- TRUE;
  }
  if(param_list$similarity_method =="entropy"){
    param_list$useentropy <- TRUE;
  } else {
    param_list$useentropy <- FALSE;
  }
  
  qs::qsave(param_list, file = "msn_param_list.qs");
  
  param_list$db_opt <- gsub(" ", "", param_list$db_opt);
  param_list$db_opt <- gsub("\\[", "c('", param_list$db_opt);
  param_list$db_opt <- gsub("]", "')", param_list$db_opt);
  param_list$db_opt <- gsub(",", "','", param_list$db_opt);

  if(param_list$omics_type == "exposomics"){
    write.table("exposomics", file = "omics_type.txt", quote = F, sep = "", col.names = F, row.names = F)
  } else {
    write.table("metabolomics", file = "omics_type.txt", quote = F, sep = "", col.names = F, row.names = F)
  }
  
  if(mode == "dda"){
    # import feature list
    str <- paste0(str, ";\n", "ft <- mSet@peakAnnotation[[\'camera_output\']][,c(2,3,5,6)]");

    if(param_list[["targets_opt"]] == "sigs") {
      str <- paste0(str, ";\n", "idx <- OptiLCMS:::generatePvals_SigFeatures(mSet@dataSet)");
      str <- paste0(str, ";\n", "if(length(idx)>0){ft<- ft[idx,]}");      
    } else {
      str <- paste0(str, ";\n", "idx <- NA");
    }
    
    #str <- paste0(str, ";\n", "idx <- OptiLCMS:::generatePvals_SigFeatures(mSet@dataSet)");
    #str <- paste0(str, ";\n", "if(length(idx)>0){ft<- ft[idx,]}")
    str <- paste0(str, ";\n", "cat('There are total of ', nrow(ft), ' target MS1 features included for MS2 data processing!')")
    
    # progress 104
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 7/12: Starting importing MS/MS data... \n\'),ecol = \'\',progress = 104)";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # import data
    if(file.exists("upload/MS2")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), targetFeatures = as.matrix(ft), acquisitionMode = \'DDA\')")
    } else if(any(grepl("MS2_", list.files("upload/")))) {
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\'upload/\',
                                                  pattern = \'^MS2_\',
                                                  full.names = T, recursive = T)), targetFeatures = as.matrix(ft), acquisitionMode = \'DDA\')")
    } else if(file.exists("isGoogleDriveUpload")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), targetFeatures = as.matrix(ft), acquisitionMode = \'DDA\')")
    }
    # progress 110
    cmd_prgs <- "OptiLCMS:::MessageOutput(
      mes = paste0(\'Step 7/12: MS/MS data imported completely! \n\'),
      ecol = \'\',
      progress = 110
    )";
    str <- paste0(str, ";\n", cmd_prgs)
    
    # perform deconvolution
    # progress 120
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution is starting... \n\'),ecol = \'\',progress = 120)";
    if(file.exists("/data/COMPOUND_DBs/Curated_DB/v09102023/MS2ID_Complete_v09102023.sqlite")){
      cmd_deco <- paste0("mSet <- PerformDDADeconvolution(mSet,
                                    ppm1 = ", param_list$ppm1,
                                    ", ppm2 = ", param_list$ppm2,
                                    ", sn = 12, filtering = ", param_list$filtering, 
                                    ", window_size = ", param_list$win_size,
                                    ", intensity_thresh = ", param_list$intensity_threshold, 
                                    ", database_path = \'/data/glassfish/ms2databases/MS2ID_Complete.sqlite\',
                                    ncores = 4L, decoOn = ", param_list$enabledDDADeco, 
                                    ", useEntropy = ", param_list$useentropy, ")");
    } else if (file.exists("/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite")) {
      cmd_deco <- paste0("mSet <- PerformDDADeconvolution(mSet,
                                    ppm1 = ", param_list$ppm1,
                                    ", ppm2 = ", param_list$ppm2,
                                    ", sn = 12, filtering = ", param_list$filtering, 
                                    ", window_size = ", param_list$win_size,
                                    ", intensity_thresh = ", param_list$intensity_threshold, 
                                    ", database_path = \'/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite\',
                                    ncores = 4L, decoOn = ", param_list$enabledDDADeco, 
                                    ", useEntropy = ", param_list$useentropy, ")");
    }

    str <- paste0(str, ";\n", "mSet@MSnData[[\'peak_mtx_idx\']] <- idx");
    str <- paste0(str, ";\n", cmd_deco)
    # progress 140
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution completed ! \n\'),ecol = \'\',progress = 140)";
    str <- paste0(str, ";\n", cmd_prgs)
    
  } else {
    # for swath-dia
    # progress 102
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 7/12: Starting importing MS/MS data... \n\'),ecol = \'\',progress = 102)";
    str <- paste0(str, ";\n", cmd_prgs)
    if(param_list[["targets_opt"]] == "sigs") {
      str <- paste0(str, ";\n", "idx <- OptiLCMS:::generatePvals_SigFeatures(mSet@dataSet)");      
    } else {
      str <- paste0(str, ";\n", "idx <- NA");
    }
    
    # import data
    if(file.exists("upload/MS2")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(mSet = mSet, filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), acquisitionMode = \'DIA\', SWATH_file = 'swath_design_metaboanalyst.txt')")
    } else if(any(grepl("MS2_", list.files("upload/")))) {
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(filesPath = c(list.files(\"upload/\",
                                                  pattern = \"^MS2_\",
                                                  full.names = T, recursive = T)), acquisitionMode = \'DIA\', SWATH_file = 'swath_design_metaboanalyst.txt')")
    } else if(file.exists("isGoogleDriveUpload")){
      str <- paste0(str, ";\n", "mSet <- PerformMSnImport(mSet = mSet, filesPath = c(list.files(\'upload/MS2\',
                                                  pattern = \'.mzML|.mzXML|.cdf\',
                                                  full.names = T, recursive = T)), acquisitionMode = \'DIA\', SWATH_file = 'swath_design_metaboanalyst.txt')")
    }
    
    # progress 110
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 7/12: MS/MS data imported completely!  \n\'),ecol = \'\',progress = 110)";
    str <- paste0(str, ";\n", cmd_prgs)
    
    if(param_list[["targets_opt"]] == "sigs") {
      str <- paste0(str, ";\n", "if(length(idx)>0){mSet@MSnData[['peak_mtx']] <- mSet@MSnData[['peak_mtx']][idx]}")
      str <- paste0(str, ";\n", "cat('There are total of ', length(mSet@MSnData[['peak_mtx']]), ' target MS1 features included for MS2 data processing!\n')")
    }

    
    # perform deconvolution
    # progress 120
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution is starting... \n\'),ecol = \'\',progress = 120)";
    cmd_deco <- paste0("mSet <- PerformDIADeconvolution(mSet,
                                    min_width = 5,
                                    ppm2 = ", param_list$ppm2,
                                    ", sn = 12,
                                    filtering = ", param_list$filtering, ",
                                    ncores = 4L)");
    str <- paste0(str, ";\n", cmd_deco)
    str <- paste0(str, ";\n", "mSet@MSnData[[\'peak_mtx_idx\']] <- idx");
    # progress 140
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution completed! \n\'),ecol = \'\',progress = 140)";
    str <- paste0(str, ";\n", cmd_prgs)
  }
  
  # PerformSpectrumConsenus
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 9/12: MS/MS spectra consensus is starting .. \n\'),ecol = \'\',progress = 145)";
  str <- paste0(str, ";\n", cmd_prgs)
  cmd_consenus <- paste0("mSet <- PerformSpectrumConsenus (mSet, ppm2 = ", param_list$ppm2, ", concensus_fraction = 0.2, database_path = '', use_rt = FALSE,
                                     user_dbCorrection = FALSE)");
  str <- paste0(str, ";\n", cmd_consenus)
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 9/12: MS/MS spectra consensus finished! \n\'),ecol = \'\',progress = 150)";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # PerformDBSearchingBatch
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 10/12: MS/MS spectra database searching is starting ...\n this step may take some time.. \n\'),ecol = \'\',progress = 150)";
  str <- paste0(str, ";\n", cmd_prgs)
  if(file.exists("/data/COMPOUND_DBs/Curated_DB/v09102023/MS2ID_Complete_v09102023.sqlite")){
    cmd_seareching <- paste0("mSet <- PerformDBSearchingBatch (mSet,
                                     ppm1 = ", param_list$ppm1, ",
                                     ppm2 = ", param_list$ppm2, ",
                                     rt_tol = 5, 
                                     database_path = \'/data/glassfish/ms2databases/MS2ID_Complete.sqlite\', 
                                     use_rt = FALSE, enableNL = FALSE, ncores = 4L, useEntropy = ", param_list$useentropy, ", 
                                     databaseOptions =", param_list$db_opt, ")");
  } else if (file.exists("/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite")){
    cmd_seareching <- paste0("mSet <- PerformDBSearchingBatch (mSet,
                                     ppm1 = ", param_list$ppm1, ",
                                     ppm2 = ", param_list$ppm2, ",
                                     rt_tol = 5, 
                                     database_path = \'/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite\', 
                                     use_rt = FALSE, enableNL = FALSE, ncores = 4L, useEntropy = ", param_list$useentropy, ", 
                                     databaseOptions =", param_list$db_opt, ")");
  }

  str <- paste0(str, ";\n", cmd_seareching)
  # progress 180
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 10/12: MS/MS database searching completed! \n\'),ecol = \'\',progress = 180)";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # PerformResultsExport
  # progress 190
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 11/12: MS/MS data processing result exporting.. \n\'),ecol = \'\',progress = 190)";
  str <- paste0(str, ";\n", cmd_prgs)
  cmd_export <- "mSet <- PerformResultsExport (mSet, type = 0L,
                                  topN = 10L, ncores = 4L)";
  str <- paste0(str, ";\n", cmd_export)

  cmd_metabolome <- "mSet <- OptiLCMS:::PerformMetabolomeClassify(mSet, path_repo =\'/home/glassfish/projects/metabolome_lib/complete_metabolome_taxonomies_lib.qs\')"
  str <- paste0(str, ";\n", cmd_metabolome)

  if(param_list$omics_type == "exposomics"){
    cmd_exposome <- "mSet <- OptiLCMS:::PerformExpsomeClassify(mSet, path_repo =\'/home/glassfish/projects/exposome_lib/complte_exposome_categories_lib.qs\')"
    str <- paste0(str, ";\n", cmd_exposome)
  }

  # progress 190
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 11/12: MS/MS data processing result exported! \n\'),ecol = \'\',progress = 190)";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # FormatMSnAnnotation
  cmd_annotation <- "dtx <- FormatMSnAnnotation(mSet, 5L, F)";
  str <- paste0(str, ";\n", cmd_annotation);
  str <- paste0(str, ";\n", "mSet@MSnResults[[\'ResTable\']] <- dtx");

  cmd_write <- "write.csv(dtx, file = \'compound_msn_results.csv\', row.names = F, col.names = F)";
  str <- paste0(str, ";\n", cmd_write)
  cmd_write <- "write.table(dtx, file = \'compound_msn_results.tsv\', row.names = F, quote = FALSE, sep = \'\\t\')";
  str <- paste0(str, ";\n", cmd_write)
  
  cmd_saving <- "qs::qsave(mSet, file = \'msn_mset_result.qs\')";
  str <- paste0(str, ";\n", cmd_saving)
  
  cmd_summarize <- "mSet <- OptiLCMS:::SummarizeAllResults4Reference(mSet)"
  str <- paste0(str, ";\n", cmd_summarize)

    if(file.exists("/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite")){
    export_msn_all <- "OptiLCMS:::PerformAllMirrorPlotting(\'/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite\')";
    str <- paste0(str, ";\n", export_msn_all)
  } else if (file.exists("/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite")){
    export_msn_all <- "OptiLCMS:::PerformAllMirrorPlotting(\'/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite\')";
    str <- paste0(str, ";\n", export_msn_all)
  }

  # progress 198
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 12/12: MS/MS data processing finished! We are finalizing the job! \n\'),ecol = \'\',progress = 198)";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # progress 200 
  cmd_prgs <- "OptiLCMS:::jobFinished(mSet)";
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
  
  if(dir.exists("/home/glassfish/payara6/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  } else {
    users.path <-getwd();
  }
  
  ## create algorithm marker
  write.table("asari", file = "ms1_algorithm.txt", quote = F, sep = "", col.names = F, row.names = F)
  
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
  
  cat(paste0("\nsrun R -e \"\n", str, "\n\";\n"));
  cat(paste0("\n", str_asari, "; \n\n"));
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
  # print(filenms)
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
    print("Incorrect file type!")
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
    path = "/home/glassfish/payara6/glassfish/domains/domain1/applications/MetaboAnalyst/resources/data/"
  }else if(dir.exists("/media/zzggyy/disk/")){
    path <- "/media/zzggyy/disk/MetaboAnalyst/target/MetaboAnalyst-5.18/resources/data/"
  }else if(.on.public.web){
    path = paste0(rpath ,"data/");
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
updateSpectra3DPCA <- function(featureNM = 100, msopt = "false"){
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

  if(is.null(mSet@peakfilling[["FeatureGroupTable"]])){
    pca3d$loading$entrez <- mSet@dataSet[["Samples"]][-1][sort(selectRow_idx)]
  } else {
    pca3d$loading$entrez <- paste0(round(mSet@peakfilling[["FeatureGroupTable"]]@listData$mzmed, 4), 
                                   "@", 
                                   round(mSet@peakfilling[["FeatureGroupTable"]]@listData$rtmed, 2))[sort(selectRow_idx)];
    
  }
save(selectRow_idx, file = 'selectRow_idx.rda')
  sort(selectRow_idx) -> selectRow_idx
  if(msopt=="true"){
    if(file.exists("compound_msn_results_index.qs")){
        dt_ms2 <- qs::qread("compound_msn_results_index.qs")
        dt_ms2$peak_idx_vals -> peak_idx_ms2
        vec_idx <- selectRow_idx
        vec_idx <- vec_idx[!c(vec_idx %in% peak_idx_ms2)]
        vec_idx <- vapply(selectRow_idx, function(x){x %in% vec_idx}, logical(1L))
        cols[vec_idx] <- "rgba(220,220,220,0.05)"
    }
  }

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

#' retrieveAlgorithmInfo
#' @description retrieveAlgorithmInfo
#' @noRd
#' @author Zhiqiang Pang
retrieveAlgorithmInfo <- function(){
  if(!file.exists("ms1_algorithm.txt")){return("optilcms")}
  plantext <- readLines("ms1_algorithm.txt")
  if(grepl("asari", plantext)){
    return("asari")
  } else if(grepl("centWave_auto", plantext)){
    return("centWave_auto")
  } else if(grepl("centWave_manual", plantext)){
    return("centWave_manual")
  } else {
    return("optilcms")
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
    if(nrow(res0)==0){
      res0 <- data.frame(Retention_Time = c(minRT, maxRT), Intensity = 0.001)
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
  fileName = paste0("EIC_", title, "_group_", default.dpi, ".", "png");
  
  if (.on.public.web) {
    Cairo::Cairo(
      file = fileName,
      unit = "in",
      dpi = default.dpi,
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

PlotXICUpdate <- function(featureNum, format = "png", dpi = default.dpi, width = NA){
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

getBloodRawData <- function(homedir) {
  print("Downloading raw blood Data.... ")
  datalink <- "https://www.dropbox.com/scl/fi/eofgffstyp9p4dbpj2s0f/blood_dda.zip";
  desfile <- paste0(homedir,"/blood_dda.zip");
  markerfile <- paste0(homedir, "/upload/QC/QC1.mzML");
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
    cmd <- paste0("wget -P ", homedir, " https://www.dropbox.com/scl/fi/eofgffstyp9p4dbpj2s0f/blood_dda.zip")
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

getCOVIDRawData <- function(homedir) {
  print("Downloading raw covid Data.... ")
  datalink <- "https://www.dropbox.com/scl/fi/a7kizcjw1q9y4jkyzv2fx/swath_data_file.zip";
  desfile <- paste0(homedir,"/swath_data_file.zip");
  markerfile <- paste0(homedir, "/upload/MS2/Covid_Cov_16_MS2.mzML");
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
    cmd <- paste0("wget -P ", homedir, " https://www.dropbox.com/scl/fi/a7kizcjw1q9y4jkyzv2fx/swath_data_file.zip")
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

getExposomeRawData <- function(homedir) {
  print("Downloading raw covid Data.... ")
  datalink <- "https://www.dropbox.com/scl/fi/p5j72mzihpqazehbkcxkt/exposome_data.zip";
  desfile <- paste0(homedir,"/swath_data_file.zip");
  markerfile <- paste0(homedir, "/upload/Worker/Sample010.mzML");
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
    cmd <- paste0("wget -P ", homedir, " https://www.dropbox.com/scl/fi/p5j72mzihpqazehbkcxkt/exposome_data.zip")
    try(system(cmd),silent = TRUE)
    try(unzip(zipfile = desfile, exdir = paste0(homedir, "/upload")),silent = TRUE)
  }
  if(file.exists(markerfile)) {
    print("Downloading done! ")
    return(1)
  } else {
    print("Downloading failed!")
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
  ftable <- read.csv(paste0(result_folder, "/preferred_Feature_table.tsv"), sep = "\t", check.names=FALSE)
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
  ftab_annotation <- read.csv(paste0(result_folder, "/Feature_annotation.tsv"), sep = "\t", check.names=FALSE)
  idx_num <- ftable$id_number
  idx_row <- vapply(idx_num, FUN = function(x){
    which(ftab_annotation[,1] == x)[1]
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
    if(is.na(x)){return("")}
    res <- strsplit(x, "\\), \\(")
    if(length(res[[1]]) == 0){
      return("")
    } else {
      res <- gsub("\\(|\\)", "", res[[1]])
      res2 <- vapply(res, FUN = function(y){
        strsplit(strsplit(y, "'")[[1]][2], "_")[[1]][1]
      }, character(1L), USE.NAMES = F)
      if(length(res2) == 1){
        res2 <- paste0(res2, "; ")
      } else {
        res2 <- paste0(res2, collapse = "; ")
      }
      return(res2)
    }
  })
  
  all_cmpds <- ftab_annotation$matched_DB_shorts
  all_cmpd <- sapply(all_cmpds, function(x){
    if(is.na(x)){return("")}
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
        res2_done <- paste0(res2_done, "; ")
      } else {
        res2_done <- paste0(res2_done, collapse = "; ")
      }
      return(res2_done)
    }
  })
  all_hmdb <- sapply(all_cmpds, function(x){
    if(is.na(x)){return("")}
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
  
  ftab_annotation$matched_DB_records[is.na(ftab_annotation$matched_DB_records)] <- ""
  
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
  
  FeatureOrder <- order(pvals)
  my.dat <- my.dat[FeatureOrder,]
  qs::qsave(FeatureOrder, file = "FeatureOrder.qs")
  
  write.table(my.dat, sep = "\t",
              file = "peak_feature_summary.tsv",
              row.names = FALSE,
              quote = FALSE);
  write.csv(my.dat, 
            file = "peak_feature_summary.csv", 
            quote = TRUE, 
            row.names = FALSE);
  
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
plotSingleTIC <- function(filename, imageNumber, format = "png", dpi = default.dpi, width = NA){

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
plotMSfeature <- function(FeatureNM, format = "png", dpi = default.dpi, width = NA){

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
    imgName0 <- OptiLCMS::plotMSfeature(NULL, FeatureNM, dpi = default.dpi, format = "png", width = NA)
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
PlotXIC <- function(featureNum, format = "png", dpi = default.dpi, width = NA){
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


PlotSpectraRTadj <- function(imageNumber, format = "png", dpi = default.dpi, width = NA) {
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

PlotSpectraBPIadj <- function(imageNumber, format = "png", dpi = default.dpi, width = NA) {
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

PlotSpectraInsensityStatics <- function(imageNumber, format = "png", dpi = default.dpi, width = NA) {
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

plotTICs <- function(imageNumber, format = "png", dpi = default.dpi, width = NA) {
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

plotBPIs <- function(imageNumber, format = "png", dpi = default.dpi, width = NA) {
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
  
  if(file.exists("compound_msn_results.csv")){
    
    dt <- try(read.csv("compound_msn_results.csv"), silent = T);
    if(class(dt) == "try-error"){
        dt <- data.frame()
    }
    param_list <- qs::qread("msn_param_list.qs");

    param_list$db_opt <- gsub(" ", "", param_list$db_opt);
    param_list$db_opt <- gsub("\\[", "", param_list$db_opt);
    param_list$db_opt <- gsub("]", "", param_list$db_opt);
    param_list$db_opt <- gsub(",", " , ", param_list$db_opt);

    cmpdsMsg <- paste0(nrow(dt), 
                         " compounds have been identified based on MS/MS spectra from database: ", param_list$db_opt);

    return(c(IntroMsg, ProcessingMsg, FeatureMsg, ppmMsg, IsotopMsg, AdductsMsg, cmpdsMsg))

  } else {
    formulaMsg <- paste0(length(formulass), 
                         " unique formulas have been matched to HMDB database.")
    cmpdsMsg <- paste0(length(cmpdss), 
                         " potential compounds have been matched to HMDB database.");

    return(c(IntroMsg, ProcessingMsg, FeatureMsg, ppmMsg, IsotopMsg, AdductsMsg, formulaMsg, cmpdsMsg))
  }
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

#' PerformMirrorPlottingWeb
#'
#' @param mSetObj mSetObj
#' @param fragDB_path Fragmentation database path
#' @param featurelabel featurelabel
#' @param result_num result_num
#' @param sub_idx sub_idx
#' @param ppm ppm
#' @param imageNM imageNM
#' @param dpi dpi
#' @param format format
#' @param width width
#' @param height height
#' @export

PerformMirrorPlottingWeb <- function(mSetObj=NA, 
                                     fragDB_path,
                                     featurelabel, 
                                     result_num, sub_idx, 
                                     ppm, imageNM, 
                                     dpi, format, width, height){
  mSetObj <- .get.mSet(mSetObj);
  MirrorPlotting <- OptiLCMS:::MirrorPlotting;
  parse_ms2peaks <- OptiLCMS:::parse_ms2peaks;
  
  require("RSQLite")
  require("DBI")
  
  if(is.null(mSetObj[["analSet"]][["ms2res"]])){
    mSet_raw <- qs::qread("msn_mset_result.qs")
    mSetObj[["analSet"]][["ms2res"]] <- mSet_raw@MSnResults;
    if(is.list(mSet_raw@MSnData[["peak_mtx"]])){
      peak_list <- lapply(mSet_raw@MSnData[["peak_mtx"]], function(x){x[c(2,3,5,6)]})
      peak_mtx_complete <- do.call(rbind, peak_list)
      colnames(peak_mtx_complete) <- c("mzmin", "mzmax", "rtmin", "rtmax")
      peak_mtx <- peak_mtx_complete[mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
    } else {
      peak_mtx_complete <- mSet_raw@MSnData[["peak_mtx"]]
      peak_mtx <- peak_mtx_complete[mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
    }
    
    DBAnnoteRes <- lapply(mSetObj[["analSet"]][["ms2res"]][["DBAnnoteRes"]], function(x){
     res <- x[[1]][[1]]
     if(length(res) == 0) {
       return(NULL)
     } else {
       x[[1]]
     }
    })
    mSetObj[["analSet"]][["DBAnnoteRes"]] <- DBAnnoteRes
    mSetObj[["analSet"]][["peak_mtx"]] <- peak_mtx
    mSetObj[["analSet"]][["peak_mtx_complete"]] <- peak_mtx_complete
  } else {
    mSetObj[["analSet"]][["DBAnnoteRes"]] -> DBAnnoteRes
    mSetObj[["analSet"]][["peak_mtx"]] -> peak_mtx
  }

  if(is.null(mSetObj[["analSet"]][["peak_mtx"]])){
    mSet_raw <- qs::qread("msn_mset_result.qs")    
    if(is.list(mSet_raw@MSnData[["peak_mtx"]])){
      peak_list <- lapply(mSet_raw@MSnData[["peak_mtx"]], function(x){x[c(2,3,5,6)]})
      peak_mtx <- do.call(rbind, peak_list)
      colnames(peak_mtx) <- c("mzmin", "mzmax", "rtmin", "rtmax")
      peak_mtx <- peak_mtx[mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
    } else {
      peak_mtx <- mSet_raw@MSnData[["peak_mtx"]][mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
    }
    mSetObj[["analSet"]][["peak_mtx"]] <- peak_mtx
  }
  
  #save(featurelabel, result_num, sub_idx, imageNM, ppm, format, mSetObj, file = "mSetObj___2678.rda")
  #cat("==== fragDB_path ---> ", fragDB_path, "\n");

  fl <- gsub("mz|sec|min", "", featurelabel)
  fl <- strsplit(fl,"@")[[1]]
  mz <- as.double(fl[1])
  rt <- as.double(fl[2])
  peak_mtx <- as.data.frame(peak_mtx)
  idx <- which((peak_mtx$mzmin-0.00005 <= mz) & (peak_mtx$mzmax+0.00005 >= mz) & (peak_mtx$rtmin-0.5 <= rt) & (peak_mtx$rtmax+0.5 >= rt))
  ucmpds <- unique(DBAnnoteRes[[idx]][["InchiKeys"]])
  uidx <- vapply(ucmpds, function(x){
    which(DBAnnoteRes[[idx]][["InchiKeys"]] == x)[1]
  }, FUN.VALUE = integer(1L))
  spec_bottom <- DBAnnoteRes[[idx]][["MS2Pekas"]][uidx][sub_idx+1]
  spec_bottom <- parse_ms2peaks(spec_bottom)
  
  spec_top_m <- mSetObj[["analSet"]][["ms2res"]][["Concensus_spec"]][[2]][[idx]][[1]]
  compound_name <- DBAnnoteRes[[idx]][["Compounds"]][uidx][sub_idx+1]
  title <- paste0("mz: ", mz, "; Retention time: ", rt)
  subtitle <- paste0(compound_name)
  p1 <- MirrorPlotting(spec_top_m, 
                       spec_bottom, 
                       ppm = ppm,
                       title= title, 
                       subtitle = subtitle,
                       cutoff_relative = 1)
  
  DBID_num <- DBAnnoteRes[[idx]][["IDs"]][uidx][sub_idx+1]
  frgs_vec_done <- vector("character", 0L)
  if(length(DBID_num)==1){
    
    con <- dbConnect(SQLite(), fragDB_path)
    db <- dbGetQuery(con, paste0("SELECT DB_Tables FROM Index_table where Max_ID>",DBID_num," AND Min_ID<",DBID_num))
    frgs <- dbGetQuery(con, paste0("SELECT Fragments FROM ", db, " where ID='",DBID_num,"'"))
    dbDisconnect(con)
    
    frgs_vec <- strsplit(frgs$Fragments, split = "\t")[[1]]
    normalize_spectrum <- OptiLCMS:::normalize_spectrum
    bottom <- normalize_spectrum(spec_bottom, 1)
    frgs_vec_done <- vapply(bottom$mz, function(x){
      y <- frgs_vec[spec_bottom[,1] == x]
      if(is.na(y[1])){return("")}
      return(y[1])
    }, FUN.VALUE = character(1L))
    
    write.table(bottom[,c(1:2)], 
                file = paste0("reference_spectrum_",result_num, "_", sub_idx,".txt"), 
                row.names = F, col.names = T)
    
    compoundName <- DBAnnoteRes[[idx]][["Compounds"]][sub_idx+1]
    score <- DBAnnoteRes[[idx]][["Scores"]][[1]][sub_idx+1]
    inchikeys <- DBAnnoteRes[[idx]][["InchiKeys"]][sub_idx+1]
    formulas <- DBAnnoteRes[[idx]][["Formula"]][sub_idx+1]
    
    df_cmpd <- data.frame(CompoundName = compoundName,
                          score = score,
                          InchiKeys = inchikeys,
                          Formula = formulas)
    write.table(df_cmpd, 
                file = paste0("compound_info_",result_num, "_", sub_idx,".txt"), 
                row.names = F, col.names = T)
  }
  
  # add some modification
  p1 <- p1 + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    text=element_text(family="serif", face = "plain"),
    plot.subtitle=element_text(size=13, face="plain", color="black"),
    plot.title=element_text(size=18, face="plain", color="black"))
  
  Cairo::Cairo(
    file = paste0("mirror_plotting_", result_num, "_", sub_idx, "_", default.dpi, ".png"),
    unit = "in", dpi = dpi, width = width, height = height, type = format, bg = "white")
  print(p1)
  dev.off()
  
  # Save the interactive plot with ggplot
  imageNM <- paste0("mirror_plotting_", result_num, "_", sub_idx, "_", default.dpi, ".png")
  save(p1, file = "p1.rda")
  # px <- plotly::ggplotly(p1);
  px <- ggplotly_modified(p1, tempfile_path = paste0(getwd(), "/temp_file4plotly"));
  #pxl <- list(px$x$data,px$x$layout,px$x$config);
  #names(pxl) <- c("data","layout","config");
  px[["x"]][["layout"]][["width"]] <- px[["width"]] <- 850
  px[["x"]][["layout"]][["height"]] <- px[["height"]] <- 425
  px[["x"]][["layout"]][["yaxis"]][["title"]][["font"]][["size"]] <- 16
  px[["x"]][["layout"]][["xaxis"]][["title"]][["font"]][["size"]] <- 16
  #px[["x"]][["layout"]][["title"]] <-NULL
  px[["x"]][["layout"]][["title"]][["font"]][["size"]] <- 16
  
  if(length(px[["x"]][["data"]])>2){
    px[["x"]][["data"]][[3]][["marker"]][["size"]] <- px[["x"]][["data"]][[3]][["marker"]][["size"]]/2
  }
  
  ## update the top data  -data1
  if(length(px[["x"]][["data"]][[1]])>0){
    if(length(px[["x"]][["data"]][[1]][["text"]])>0){
      px[["x"]][["data"]][[1]][["text"]] <- gsub("normalized|-normalized","Relative Intensity",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("y: 0<br />","",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("mz: [0-9]+.[0-9]+<br />mz","mz",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("-","",px[["x"]][["data"]][[1]][["text"]])
    }
  }
  
  ## update the bottom data  -data2
  if(length(px[["x"]][["data"]][[2]])>0){
    if(length(px[["x"]][["data"]][[2]][["text"]])>0){
      px[["x"]][["data"]][[2]][["text"]] <- gsub("normalized|-normalized","Relative Intensity",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("y: 0<br />","",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("mz: [0-9]+.[0-9]+<br />mz","mz",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("-","",px[["x"]][["data"]][[2]][["text"]])
      if(length(frgs_vec_done)>0){
        non_na_txt <- px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])]
        frgs_vec_done[frgs_vec_done==""] <- "Unknown"
        new_labels <- sapply(1:length(non_na_txt), function(x){
          paste0(non_na_txt[x], "<br />Fragment Formula: ", frgs_vec_done[ceiling(x/2)])
        })
        px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])] <- new_labels
      }
    }
  }
  
  ## update the matched star  -data3
  if(length(px[["x"]][["data"]])>2){
    if(length(px[["x"]][["data"]][[3]])>0){
      if(length(px[["x"]][["data"]][[3]][["text"]])>0){
        px[["x"]][["data"]][[3]][["text"]] <- gsub("<br />star_intensity.*","",px[["x"]][["data"]][[3]][["text"]])
        px[["x"]][["data"]][[3]][["text"]] <- paste0("<b>Matched Fragment: </b><br />", px[["x"]][["data"]][[3]][["text"]])
      }
    }
  }
  
  saveRDS(px, file = paste0(gsub(".png|.svg|.pdf", "", imageNM), ".rds"))
  
  jsonlist <- RJSONIO::toJSON(px, pretty = T,force = TRUE,.na = "null");
  sink(paste0(gsub(".png|.svg|.pdf", "", imageNM),".json"));
  cat(jsonlist);
  sink();
  
  if(is.null(mSetObj[["imgSet"]][["msmsmirror"]])){
    df <- data.frame(indx = result_num, imageNM = imageNM, legend = paste0(subtitle, " (", title, ")"))
    mSetObj[["imgSet"]][["msmsmirror"]] <- df
  } else {
    mSetObj[["imgSet"]][["msmsmirror"]] -> df0
    df <- data.frame(indx = result_num, imageNM = imageNM, legend = paste0(subtitle, " (", title, ")"))
    mSetObj[["imgSet"]][["msmsmirror"]] <- rbind(df, df0)
  }

  msmsmirror_set <- mSetObj[["imgSet"]][["msmsmirror"]]
  qs::qsave(msmsmirror_set, file = "msmsmirror_set.qs")

  return(.set.mSet(mSetObj));
}

#' PerformMirrorPlotting
#'
#' @param mSetObj mSetObj
#' @param fragDB_path Fragmentation database path
#' @param featurelabel featurelabel
#' @param peak_idx peak_idx
#' @param sub_idx sub_idx
#' @param ppm ppm
#' @param interactive interactive or not
#' @param imageNM imageNM
#' @param dpi dpi
#' @param format format
#' @param width width
#' @param height height
#' @export

PerformMirrorPlotting <- function(mSetObj=NA, 
                                  fragDB_path = NA,
                                  peak_idx, sub_idx, 
                                  interactive = T,
                                  ppm, 
                                  dpi, format, width, height){
  mSetObj <- .get.mSet(mSetObj);
  MirrorPlotting <- OptiLCMS:::MirrorPlotting;
  parse_ms2peaks <- OptiLCMS:::parse_ms2peaks;
  save(mSetObj, MirrorPlotting,  parse_ms2peaks,peak_idx, sub_idx, interactive, ppm, file = "PerformMirrorPlotting_input.rda" )
  if(is.null(mSetObj[["analSet"]][["ms2res"]])){
    #stop("Your mSet object does not contain MS2 analysis results!")
    mSet_raw <- qs::qread("msn_mset_result.qs")
    mSet_raw@MSnResults -> mSetObj[["analSet"]][["ms2res"]] -> MSnResults;
    mSet_raw@MSnData  -> mSetObj[["analSet"]][["ms2data"]]  -> MSnData
  } else {
    mSetObj[["analSet"]][["ms2res"]] -> MSnResults
    mSetObj[["analSet"]][["ms2data"]] -> MSnData
  }
  peak_idx0 <- peak_idx-1
  idx <- which(peak_idx0 == MSnResults[["Concensus_spec"]][[1]])
  if(!(peak_idx0 %in% MSnResults[["Concensus_spec"]][[1]])){
    message(paste0("No corresponding MS2 results found for peak: ", peak_idx))
    return(.set.mSet(mSetObj));
  }
  useFrg = FALSE;
  
  if(!is.na(fragDB_path)){
    if(file.exists(fragDB_path)){
      useFrg = TRUE;
    } else {
      useFrg = FALSE;
      warning("fragDB_path does not exist!")
    }
  }
  
  require("RSQLite")
  require("DBI")
  require("plotly")
  
  if(is.null(mSetObj[["analSet"]][["DBAnnoteRes"]])) {
    mSetObj[["analSet"]][["ms2res"]] <- MSnResults;
    if(is.list(MSnData[["peak_mtx"]])){
      peak_list <- lapply(MSnData[["peak_mtx"]], function(x){x[c(2,3,5,6)]})
      peak_mtx_complete <- do.call(rbind, peak_list)
      colnames(peak_mtx_complete) <- c("mzmin", "mzmax", "rtmin", "rtmax")
    } else {
      peak_mtx_complete <- MSnData[["peak_mtx"]]
    }
    
    DBAnnoteRes <- lapply(mSetObj[["analSet"]][["ms2res"]][["DBAnnoteRes"]], function(x){
      res <- x[[1]][[1]]
      if(length(res) == 0) {
        return(NULL)
      } else {
        x[[1]]
      }
    })
    mSetObj[["analSet"]][["DBAnnoteRes"]] <- DBAnnoteRes
    mSetObj[["analSet"]][["peak_mtx_complete"]] <- peak_mtx_complete
  } else {
    mSetObj[["analSet"]][["DBAnnoteRes"]] -> DBAnnoteRes
    mSetObj[["analSet"]][["peak_mtx_complete"]] -> peak_mtx_complete
  }
  
  # fl <- gsub("mz|sec|min", "", featurelabel)
  # fl <- strsplit(fl,"@")[[1]]
  # mz <- as.double(fl[1])
  # rt <- as.double(fl[2])
  peak_mtx <- as.data.frame(peak_mtx_complete)
  mz <- mean(as.numeric(peak_mtx_complete[peak_idx, c(1:2)]))
  rt <- mean(as.numeric(peak_mtx_complete[peak_idx, c(3:4)]))
  mz <- round(mz, 4)
  rt <- round(rt, 2)
  
  result_num <- idx #<- peak_idx;
  ucmpds <- unique(DBAnnoteRes[[idx]][["InchiKeys"]])
  uidx <- vapply(ucmpds, function(x){
    which(DBAnnoteRes[[idx]][["InchiKeys"]] == x)[1]
  }, FUN.VALUE = integer(1L))
  spec_bottom <- DBAnnoteRes[[idx]][["MS2Pekas"]][uidx][sub_idx]
  spec_bottom <- parse_ms2peaks(spec_bottom)
  
  spec_top_m <- mSetObj[["analSet"]][["ms2res"]][["Concensus_spec"]][[2]][[idx]][[1]]
  compound_name <- DBAnnoteRes[[idx]][["Compounds"]][uidx][sub_idx]
  title <- paste0(mz, "__", rt)
  subtitle <- paste0(compound_name)
  p1 <- MirrorPlotting(spec_top_m, 
                       spec_bottom, 
                       ppm = ppm,
                       title= title, 
                       subtitle = subtitle,
                       cutoff_relative = 0.1)
  
  DBID_num <- DBAnnoteRes[[idx]][["IDs"]][uidx][sub_idx]
  frgs_vec_done <- vector("character", 0L)
  if(length(DBID_num)==1){
    
    if(useFrg){
      con <- dbConnect(SQLite(), fragDB_path)
      db <- dbGetQuery(con, paste0("SELECT DB_Tables FROM Index_table where Max_ID>",DBID_num," AND Min_ID<",DBID_num))
      frgs <- dbGetQuery(con, paste0("SELECT Fragments FROM ", db, " where ID='",DBID_num,"'"))
      dbDisconnect(con)
      
      frgs_vec <- strsplit(frgs$Fragments, split = "\t")[[1]]
      normalize_spectrum <- OptiLCMS:::normalize_spectrum
      bottom <- normalize_spectrum(spec_bottom, 1)
      frgs_vec_done <- vapply(bottom$mz, function(x){
        y <- frgs_vec[spec_bottom[,1] == x]
        if(is.na(y[1])){return("")}
        return(y[1])
      }, FUN.VALUE = character(1L))
      
      write.table(bottom[,c(1:2)], 
                  file = paste0("reference_spectrum_",result_num, "_", sub_idx,".txt"), 
                  row.names = F, col.names = T)
      
      compoundName <- DBAnnoteRes[[idx]][["Compounds"]][sub_idx]
      score <- DBAnnoteRes[[idx]][["Scores"]][[1]][sub_idx]
      inchikeys <- DBAnnoteRes[[idx]][["InchiKeys"]][sub_idx]
      formulas <- DBAnnoteRes[[idx]][["Formula"]][sub_idx]
      
      df_cmpd <- data.frame(CompoundName = compoundName,
                            score = score,
                            InchiKeys = inchikeys,
                            Formula = formulas)
      write.table(df_cmpd, 
                  file = paste0("compound_info_",result_num, "_", sub_idx,".txt"), 
                  row.names = F, col.names = T)
    }
  }
  
  # add some modification
  p1 <- p1 + theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    text=element_text(family="serif", face = "plain"),
    plot.subtitle=element_text(size=13, face="plain", color="black"),
    plot.title=element_text(size=18, face="plain", color="black"))
  
  Cairo::Cairo(
    file = paste0("mirror_plotting_", result_num, "_", sub_idx, "_", default.dpi, ".png"),
    unit = "in", dpi = dpi, width = width, height = height, type = format, bg = "white")
  print(p1)
  dev.off()
  
  # Save the interactive plot with ggplot
  imageNM <- paste0("mirror_plotting_", result_num, "_", sub_idx, "_", default.dpi, ".png")
  save(p1, file = "p1.rda")
  px <- plotly::ggplotly(p1);
  # px <- ggplotly_modified(p1, tempfile_path = paste0(getwd(), "/temp_file4plotly"));
  #pxl <- list(px$x$data,px$x$layout,px$x$config);
  #names(pxl) <- c("data","layout","config");
  px[["x"]][["layout"]][["width"]] <- px[["width"]] <- 850
  px[["x"]][["layout"]][["height"]] <- px[["height"]] <- 425
  px[["x"]][["layout"]][["yaxis"]][["title"]][["font"]][["size"]] <- 16
  px[["x"]][["layout"]][["xaxis"]][["title"]][["font"]][["size"]] <- 16
  #px[["x"]][["layout"]][["title"]] <-NULL
  px[["x"]][["layout"]][["title"]][["font"]][["size"]] <- 16
  
  if(length(px[["x"]][["data"]])>2){
    px[["x"]][["data"]][[3]][["marker"]][["size"]] <- px[["x"]][["data"]][[3]][["marker"]][["size"]]/2
  }
  
  ## update the top data  -data1
  if(length(px[["x"]][["data"]][[1]])>0){
    if(length(px[["x"]][["data"]][[1]][["text"]])>0){
      px[["x"]][["data"]][[1]][["text"]] <- gsub("normalized|-normalized","Relative Intensity",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("y: 0<br />","",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("mz: [0-9]+.[0-9]+<br />mz","mz",px[["x"]][["data"]][[1]][["text"]])
      px[["x"]][["data"]][[1]][["text"]] <- gsub("-","",px[["x"]][["data"]][[1]][["text"]])
    }
  }
  
  ## update the bottom data  -data2
  if(length(px[["x"]][["data"]][[2]])>0){
    if(length(px[["x"]][["data"]][[2]][["text"]])>0){
      px[["x"]][["data"]][[2]][["text"]] <- gsub("normalized|-normalized","Relative Intensity",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("y: 0<br />","",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("mz: [0-9]+.[0-9]+<br />mz","mz",px[["x"]][["data"]][[2]][["text"]])
      px[["x"]][["data"]][[2]][["text"]] <- gsub("-","",px[["x"]][["data"]][[2]][["text"]])
      if(useFrg){
        if(length(frgs_vec_done)>0){
          non_na_txt <- px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])]
          frgs_vec_done[frgs_vec_done==""] <- "Unknown"
          new_labels <- sapply(1:length(non_na_txt), function(x){
            paste0(non_na_txt[x], "<br />Fragment Formula: ", frgs_vec_done[ceiling(x/2)])
          })
          px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])] <- new_labels
        }
      }
    }
  }
  
  ## update the matched star  -data3
  if(length(px[["x"]][["data"]])>2){
    if(length(px[["x"]][["data"]][[3]])>0){
      if(length(px[["x"]][["data"]][[3]][["text"]])>0){
        px[["x"]][["data"]][[3]][["text"]] <- gsub("<br />star_intensity.*","",px[["x"]][["data"]][[3]][["text"]])
        px[["x"]][["data"]][[3]][["text"]] <- paste0("<b>Matched Fragment: </b><br />", px[["x"]][["data"]][[3]][["text"]])
      }
    }
  }
  
  if(interactive){
    save(px, file = "px.rda")
    print(px)
  }
  saveRDS(px, file = paste0(gsub(".png|.svg|.pdf", "", imageNM), ".rds"))
  imageNM <- paste0("mirror_plotting_", peak_idx, "_", sub_idx,"_", dpi, ".png")
  
  jsonlist <- RJSONIO::toJSON(px, pretty = T,force = TRUE,.na = "null");
  sink(paste0(gsub(".png|.svg|.pdf", "", imageNM),".json"));
  cat(jsonlist);
  sink();
  
  if(is.null(mSetObj[["imgSet"]][["msmsmirror"]])){
    df <- data.frame(indx = result_num, imageNM = imageNM, legend = paste0(subtitle, " (", title, ")"))
    mSetObj[["imgSet"]][["msmsmirror"]] <- df
  } else {
    mSetObj[["imgSet"]][["msmsmirror"]] -> df0
    df <- data.frame(indx = result_num, imageNM = imageNM, legend = paste0(subtitle, " (", title, ")"))
    mSetObj[["imgSet"]][["msmsmirror"]] <- rbind(df, df0)
  }

  return(.set.mSet(mSetObj));
}

retrieveOmicsType <- function(){
    if(!file.exists("omics_type.txt")){return("metabolomics")}
    omicstype <- readLines("omics_type.txt")
    return(omicstype)
}

getExposomeInfo <- function(mSetObj=NA, featurelabel, subidx){

    mSetObj <- .get.mSet(mSetObj);

    if(is.null(mSetObj[["analSet"]][["ms2res"]])){
      mSet_raw <- qs::qread("msn_mset_result.qs")
      mSetObj[["analSet"]][["ms2res"]] <- mSet_raw@MSnResults;
      if(is.list(mSet_raw@MSnData[["peak_mtx"]])){
        peak_list <- lapply(mSet_raw@MSnData[["peak_mtx"]], function(x){x[c(2,3,5,6)]})
        peak_mtx <- do.call(rbind, peak_list)
        colnames(peak_mtx) <- c("mzmin", "mzmax", "rtmin", "rtmax")
        peak_mtx <- peak_mtx[mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
      } else {
        peak_mtx <- mSet_raw@MSnData[["peak_mtx"]][mSet_raw@MSnResults[["Concensus_spec"]][[1]]+1,]
      }

      DBAnnoteRes <- lapply(mSetObj[["analSet"]][["ms2res"]][["DBAnnoteRes"]], function(x){
       res <- x[[1]][[1]]
       if(length(res) == 0) {
         return(NULL)
       } else {
         x[[1]]
       }
      })
      mSetObj[["analSet"]][["DBAnnoteRes"]] <- DBAnnoteRes
      mSetObj[["analSet"]][["peak_mtx"]] <- peak_mtx
    } else {
      mSetObj[["analSet"]][["DBAnnoteRes"]] -> DBAnnoteRes
      mSetObj[["analSet"]][["peak_mtx"]] -> peak_mtx
    }
  save(featurelabel,peak_mtx, DBAnnoteRes, mSetObj, file = "getExposomeInfo.rda")
    fl <- gsub("mz|sec|min", "", featurelabel)
    fl <- strsplit(fl,"@")[[1]]
    mz <- as.double(fl[1])
    rt <- as.double(fl[2])
    peak_mtx <- as.data.frame(peak_mtx)
    idx <- which((peak_mtx$mzmin-0.00005 <= mz) & (peak_mtx$mzmax+0.00005 >= mz) & (peak_mtx$rtmin-0.5 <= rt) & (peak_mtx$rtmax+0.5 >= rt))
    ucmpds <- unique(DBAnnoteRes[[idx]][["InchiKeys"]])
    uidx <- vapply(ucmpds, function(x){
      which(DBAnnoteRes[[idx]][["InchiKeys"]] == x)[1]
    }, FUN.VALUE = integer(1L))

    cls_res <- mSetObj[["analSet"]][["ms2res"]][["ExposomeRes"]][[idx]][[uidx[subidx]]]

    return(paste0(cls_res, collapse = "; "))
}


extratExposomeClassName <- function(group){
    res <- qs::qread("exposome_classification_summary.qs");

    if(group == "All"){
        res_num <- vapply(unique(res$Categories), function(x){as.integer(sum(res$Number[res$Categories == x]))}, FUN.VALUE = integer(1L)) 
        res_nms <- unique(res$Categories)
    } else {
        res_num <- res$Number[res$Group == group]
        res_nms <- res$Categories[res$Group == group]
    }
    
    res_idx <- order(res_num, decreasing = TRUE)
    res_nmsx <- res_nms[res_idx]
    
    return(res_nmsx)
}

extratExposomeClassNumber <- function(group){
    res <- qs::qread("exposome_classification_summary.qs");
    if(group == "All"){
        res_num <- vapply(unique(res$Categories), function(x){as.integer(sum(res$Number[res$Categories == x]))}, FUN.VALUE = integer(1L))        
    } else {
        res_num <- res$Number[res$Group == group]
    }
    
    res_idx <- order(res_num, decreasing = TRUE)
    res_numx <- res_num[res_idx]
    return(res_numx)
}


generateCols <- function(number=NULL){
  pie.cols <- "ff6347,ffff00,ee82ee,f4a460,2e8b57,4682b4,9acd32,ffe4c4,8a2be2,a52a2a,deb887,5f9ea0,7fff00,d2691e,ff7f50,7fffd4,6495ed,fff8dc,dc143c,00ffff,00ff7f,00008b,b8860b,a9a9a9,006400,bdb76b,556b2f,9932cc,e9967a,8fbc8f,483d8b,2f4f4f,00ced1,9400d3,ff1493,00bfff,696969,1e90ff,b22222,fffaf0,228b22,ff00ff,dcdcdc,f8f8ff,ffd700,daa520,808080,008000,adff2f,f0fff0,ff69b4,cd5c5c,4b0082,fffff0,f0e68c,e6e6fa,fff0f5,7cfc00,fffacd,add8e6,f08080,e0ffff,fafad2,d3d3d3,90ee90,ffb6c1,ffa07a,20b2aa,87cefa,778899,b0c4de,ffffe0,00ff00,32cd32,faf0e6,800000,66cdaa,0000cd,ba55d3,9370d8,3cb371,7b68ee,00fa9a,48d1cc,c71585,191970,f5fffa,ffe4e1,ffe4b5,ffdead,fdf5e6,6b8e23,ffa500,ff4500,da70d6,eee8aa,98fb98,afeeee,d87093,ffefd5,ffdab9,cd853f,ffc0cb,dda0dd,b0e0e6,ff0000,bc8f8f,4169e1,8b4513,fa8072,fff5ee,a0522d,c0c0c0,87ceeb,6a5acd,708090,fffafa,d2b48c,008080,d8bfd8,40e0d0,f5deb3,ffffff,f5f5f5";
  pie.cols <- strsplit(pie.cols,",")[[1]];
  names(pie.cols) <- paste("nm", 1:length(pie.cols), sep="");
  return(paste("#", pie.cols[1:number], sep=""));
}

extratMetabolomeClassName <- function(group, level, merge_ratio=0){
    metabolome_cls <- qs::qread("metabolome_classification_summary.qs")
    if(group == "All"){
        metabolome_df <- do.call(rbind, metabolome_cls)
    } else {
        metabolome_df <- metabolome_cls[[group]]
    }
    
    idx <- which(colnames(metabolome_df) == level)
    idx <- which(colnames(metabolome_df) == level)
    metabolome_tb<-table(metabolome_df[,idx])
    nms0 <- names(metabolome_tb)
    nums0 <- as.data.frame(metabolome_tb)$Freq
    nms1 <- nms0[!is.na(nms0)]    
    nums1 <- nums0[!is.na(nms0)]
    if(merge_ratio == 0){
      num_all <- nums1
      names_all <- nms1
    } else {
      sum_nums <- sum(nums1)
      idx2merge <- which(nums1/sum_nums<merge_ratio)
      idxNot2merge <- which(nums1/sum_nums>=merge_ratio)
      num_others <- sum(nums1[idx2merge])
      num_all <- c(nums1[idxNot2merge], num_others)
      names_all <- c(nms1[idxNot2merge], "Others")
    }
    idx_order <- order(num_all, decreasing = TRUE)
    return(names_all[idx_order])
}

extratMetabolomeClassNumber <- function(group, level, merge_ratio=0){
    metabolome_cls <- qs::qread("metabolome_classification_summary.qs")
    if(group == "All"){
        metabolome_df <- do.call(rbind, metabolome_cls)
    } else {
        metabolome_df <- metabolome_cls[[group]]
    }

    idx <- which(colnames(metabolome_df) == level)
    idx <- which(colnames(metabolome_df) == level)
    metabolome_tb<-table(metabolome_df[,idx])
    nms0 <- names(metabolome_tb)
    nums0 <- as.data.frame(metabolome_tb)$Freq
    nms1 <- nms0[!is.na(nms0)]    
    nums1 <- nums0[!is.na(nms0)]
    if(merge_ratio == 0){
      num_all <- nums1
      names_all <- nms1

    } else {
      sum_nums <- sum(nums1)

      idx2merge <- which(nums1/sum_nums<merge_ratio)
      idxNot2merge <- which(nums1/sum_nums>=merge_ratio)
      num_others <- sum(nums1[idx2merge])
      num_all <- c(nums1[idxNot2merge], num_others)
      names_all <- c(nms1[idxNot2merge], "Others")
    }

    idx_order <- order(num_all, decreasing = TRUE)
    return(num_all[idx_order])
}

checkMS2annotationExists <- function(feature_idx){
    res_dt <- qs::qread("compound_msn_results_index.qs")
    if(feature_idx %in% res_dt[,1]){
        rowidx <- which(feature_idx == res_dt[,1])[1]
        colidx <- vapply(c("Compound_1", "Compound_2","Compound_3","Compound_4","Compound_5"), function(x){
            return(which(x == colnames(res_dt)))
        }, integer(1L))
        res_vec_cpmd <- res_dt[rowidx, colidx]        
        return(length(which(!is.na(res_vec_cpmd))))
    }
    return(0)
}

extractSimScores <- function(feature_idx, topN){
    res_dt <- qs::qread("compound_msn_results_index.qs")
    rowidx <- which(feature_idx == res_dt[,1])[1]
    colidx <- vapply(c("Score_1", "Score_2","Score_3","Score_4","Score_5"), function(x){
            return(which(x == colnames(res_dt)))
    }, integer(1L))
    res_vec <- res_dt[rowidx, colidx[1:topN]]
    return(as.double(res_vec))
}


extractformulaNMs <- function(feature_idx, topN){
    res_dt <- qs::qread("compound_msn_results_index.qs")
    rowidx <- which(feature_idx == res_dt[,1])[1]
    colidx <- vapply(c("Formula_1", "Formula_2","Formula_3","Formula_4","Formula_5"), function(x){
            return(which(x == colnames(res_dt)))
    }, integer(1L))
    res_vec <- res_dt[rowidx, colidx[1:topN]]
    return(as.character(res_vec))
}


extractcompoundNMs <- function(feature_idx, topN){
    res_dt <- qs::qread("compound_msn_results_index.qs")
    rowidx <- which(feature_idx == res_dt[,1])[1]
    colidx <- vapply(c("Compound_1", "Compound_2","Compound_3","Compound_4","Compound_5"), function(x){
            return(which(x == colnames(res_dt)))
    }, integer(1L))
    res_vec <- res_dt[rowidx, colidx[1:topN]]
    return(as.character(res_vec))
}

extractInchikeys <- function(feature_idx, topN){
    res_dt <- qs::qread("compound_msn_results_index.qs")
    rowidx <- which(feature_idx == res_dt[,1])[1]
    colidx <- vapply(c("InchiKey_1", "InchiKey_2","InchiKey_3","InchiKey_4","InchiKey_5"), function(x){
            return(which(x == colnames(res_dt)))
    }, integer(1L))
    res_vec <- res_dt[rowidx, colidx[1:topN]]
    return(as.character(res_vec))
}
