#' CreateMS2RawRscriptPro#'
#' @noRd
#' @author Zhiqiang Pang
CreateMS2RawRscriptPro <- function(guestName, planString, mode = "dda"){
  
  guestName <- guestName;
  planString <- planString;
  
  cat("current planString ---> ", planString, "\n")
  cat("current guestName  ---> ", guestName, "\n")
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  } else {
    users.path <-getwd();
  }
  
  ## Prepare Configuration script for slurm running
  conf_inf <- "#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=720:00\n#SBATCH --mem-per-cpu=5G\n#SBATCH --cpus-per-task=4\n"
  
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

  if(!grepl("^c\\(",param_list$db_opt)){
    param_list$db_opt <- paste0("\'", param_list$db_opt, "\'")
  }

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
      mes = paste0(\'Step 7/12: MS/MS data imported completely! \n\n\'),
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
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution completed ! \n\n\'),ecol = \'\',progress = 140)";
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
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 7/12: MS/MS data imported completely!  \n\n\'),ecol = \'\',progress = 110)";
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
    cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 8/12: MS/MS data deconvolution completed! \n\n\'),ecol = \'\',progress = 140)";
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
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 9/12: MS/MS spectra consensus finished! \n\n\'),ecol = \'\',progress = 150)";
  str <- paste0(str, ";\n", cmd_prgs)
  
  # PerformDBSearchingBatch
  # progress 150
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 10/12: MS/MS spectra database searching is starting ...\n this step may take some time.. \n\n\'),ecol = \'\',progress = 150)";
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
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 10/12: MS/MS database searching completed! \n\n\'),ecol = \'\',progress = 180)";
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
  cmd_prgs <- "OptiLCMS:::MessageOutput(mes = paste0(\'Step 11/12: MS/MS data processing result exported! \n\n\'),ecol = \'\',progress = 190)";
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

#' CreateRawRscriptPro
#' @description used to create a running for raw spectra processing
#' this file will be run by SLURM by using OptiLCMS
#' @noRd
#' @author Guangyan Zhou, Zhiqiang Pang
CreateRawRscriptPro <- function(guestName, planString, planString2, rawfilenms.vec, source_path){
  
  guestName <- guestName;
  planString <- planString;
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
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
  conf_inf <- paste0("#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=1440:00\n#SBATCH --mem-per-cpu=8G\n#SBATCH --cpus-per-task=4\n#SBATCH --output=", users.path, "/metaboanalyst_spec_proc.txt\n")
  
  ## Prepare R script for running
  # download files if needed
  require(R.utils)
  allMSFiles <- list.files("upload", full.names = T, recursive = T)
  if(length(allMSFiles)<3 & file.exists("isGoogleDriveUpload")){
    download_script <- paste0("Rscript --vanilla ", source_path, "/XiaLabPro/R/download_googledrive.R ", users.path)
  } else {
    download_script <- "";
  }
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
  if(download_script!=""){
    cat("\n\n", download_script, "\n\n")
  }
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

#' CreateRawRscript4AsariPro
#' @description used to create a running for raw spectra processing
#' this file will be run by SLURM by using OptiLCMS and Asari from python.
#' @noRd
#' @author Zhiqiang Pang
CreateRawRscript4AsariPro <- function(guestName, planString, asari_str, rawfilenms.vec, source_path){
  
  if(dir.exists("/home/glassfish/payara5/glassfish/domains/")){
    users.path <-paste0("/data/glassfish/projects/metaboanalyst/", guestName);
  } else {
    users.path <-getwd();
  }
  
  ## create algorithm marker
  write.table("asari", file = "ms1_algorithm.txt", quote = F, sep = "", col.names = F, row.names = F)
  
  ## Prepare Configuration script for slurm running
  conf_inf <- paste0("#!/bin/bash\n#\n#SBATCH --job-name=Spectral_Processing\n#\n#SBATCH --ntasks=1\n#SBATCH --time=1440:00\n#SBATCH --mem-per-cpu=8G\n#SBATCH --cpus-per-task=4\n#SBATCH --output=", users.path, "/metaboanalyst_spec_proc.txt\n")
  
  require(R.utils)
  allMSFiles <- list.files("upload", full.names = T, recursive = T)
  
  if(length(allMSFiles)<3 & file.exists("isGoogleDriveUpload")){
    download_script <- paste0("Rscript --vanilla ", source_path, "/XiaLabPro/R/download_googledrive.R ", users.path)
  } else {
    download_script <- "";
  }
  
  if(file.exists("IncludedSpectra.qs")){
    included_spec <- qs::qread("IncludedSpectra.qs")
    inc_files <- included_spec[[1]]
    inc_files_all <- str2lang(inc_files)
    inc_files_all <- eval(inc_files_all)
  } else {
    inc_files_all <- NULL;
  }
  
  for(i in allMSFiles){
    if(!is.null(inc_files_all)){
      if(!(basename(i) %in% inc_files_all)){
        next;
      }
    }
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
  if(download_script!=""){
    cat("\n\n", download_script, "\n\n")
  }
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

