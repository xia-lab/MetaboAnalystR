# This script should be called by server to load actual scripts
# based on the modules user selected
.on.public.web <<- TRUE;
rpath <<- "../../";
#print("rpathscriptloader=====");

general_files <- c("general_data_utils","general_misc_utils","general_lib_utils","generic_c_utils","depends_refs", "../../XiaLabPro/R/util_misc");
general_stat_files <- c("general_norm_utils","general_proc_utils","multifac_metadata");
general_anot_files <- "general_anot_utils";
stats_files <- c("stats_chemometrics","stats_classification","stats_clustering","stats_correlations","stats_sigfeatures","stats_univariates", "stats_peaks");
enrich_files <- c("enrich_graphics","enrich_mset","enrich_name_match","enrich_stats");
pathway_files <- c("enrich_mset","enrich_stats","enrich_name_match","enrich_path_graphics","enrich_path_stats", "gene_fun_utils")
integmex_files <- c("enrich_integ","enrich_stats","enrich_name_match", "enrich_path_graphics", "enrich_kegg")
biomarker_files <- c("biomarker_utils", "multifac_metadata");
power_files <- c("util_sspa", "power_utils");
dose_files <- c("dose_response_general", "dose_response_graphs", "dose_response_utils", "dose_response_deanal");
multifac_files <-c("multifac_metadata", "multifac_asca_heatmap2","multifac_pca_anova2", "multifac_mb","multifac_covariate", "stats_classification", "stats_chemometrics","stats_univariates", "stats_correlations");
mummichog_files <- c("peaks_to_function", "enrich_kegg", "peaks_ms2fun");
metaanal_files <- c("meta_methods", "meta_data_utils");
network_files <- c("networks_enrich", "networks_view", "enrich_kegg", "enrich_integ", "enrich_name_match", "gene_fun_utils");
spectra_files <- c("spectra_processing", "stats_chemometrics", "util_scatter3d", "plotly_utils")
metapath_files <- c("meta_pathway", "peaks_to_function", "meta_methods", "meta_data_utils")
other_files <- c("batch_effect_utils", "others_lipomics", "enrich_name_match", "duplicates_estimates");
mgwas_files <- c("mgwas_data", "mgwas_misc", "mgwas_mr");
if(file.exists("/home/zgy/sqlite/")){
  loadPath <- "../../rscripts/MetaboAnalystR/R/"
  loadPathOMS <- "../../rscripts/XiaLabPro/R/"
  compilePath <- "../rscripts/"
  workingPath <- "../rscripts/MetaboAnalystR/src/"
} else if (file.exists("/home/glassfish/payara6_micro/useVIP_2025R2")){ # this is used for testing or beta vip node/app
  compilePath = "/home/glassfish/payara6_micro/MetaboAnalyst_dir_r2_2025/applications/MetaboAnalyst/resources/rscripts/"
  loadPath = "/home/glassfish/payara6_micro/MetaboAnalyst_dir_r2_2025/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/R/"
  workingPath <- "/home/glassfish/payara6_micro/MetaboAnalyst_dir_r2_2025/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/src/"
  loadPathOMS <- "/home/glassfish/payara6_micro/MetaboAnalyst_dir_r2_2025/applications/MetaboAnalyst/resources/rscripts/XiaLabPro/R/"
} else if (file.exists("/home/glassfish/payara6_micro/useVIP")){ # this is used for regular vip node/app
  compilePath = "/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/rscripts/"
  loadPath = "/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/R/"
  workingPath <- "/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/src/"
  loadPathOMS <- "/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/rscripts/XiaLabPro/R/"
} else if (file.exists("/docker_marker")){
  compilePath = "/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/rscripts/"
  loadPath = "/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/R/"
  workingPath <- "/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/rscripts/MetaboAnalystR/src/"
  loadPathOMS <- "/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/rscripts/XiaLabPro/R/"
} else{
  loadPath <- "../../rscripts/MetaboAnalystR/R/"
  loadPathOMS <- "../../rscripts/XiaLabPro/R/"
  compilePath <- "../rscripts/"
  workingPath <- "../rscripts/MetaboAnalystR/src/"
}

LoadRscripts <- function(module.nm = "stat"){
  file.sources <- "";
  if(module.nm == "stat"){
    file.sources <- c(general_files, general_stat_files, stats_files);
  }else if(module.nm == "mf"){
    file.sources <- c(general_files, general_stat_files, multifac_files);
  }else if(module.nm == "pathinteg"){
    file.sources <- c(general_files, general_anot_files, integmex_files, mummichog_files, general_stat_files, network_files);
  }else if(substring(module.nm, 1,4) == "path"){
    file.sources <- c(general_files, general_stat_files, general_anot_files,  pathway_files);
  }else if(substring(module.nm, 1,4) == "mset" || module.nm == "enrich"){
    file.sources <- c(general_files, general_stat_files, general_anot_files, enrich_files);
  }else if(module.nm == "roc"){
    file.sources <- c(general_files, general_stat_files, biomarker_files);
  }else if(module.nm == "power"){
    file.sources <- c(general_files, general_stat_files, power_files);
  }else if(module.nm == "dose"){
    file.sources <- c(general_files, general_stat_files, dose_files);
  }else if(module.nm == "utils"){
    file.sources <- c(general_files, general_anot_files, other_files);
  }else if(module.nm == "network"){
    file.sources <- c(general_files, general_anot_files, network_files, general_stat_files);
  }else if(module.nm == "mass_all"){
    file.sources <- c(general_files, mummichog_files);
  }else if(module.nm == "mass_table" || module.nm == "mummichog"){
    file.sources <- c(general_files, mummichog_files, general_stat_files, "stats_univariates");
  }else if(module.nm == "metadata"){
    file.sources <- c(general_files, general_stat_files, metaanal_files);
  }else if(module.nm == "raw"){
    file.sources <- c(general_files, spectra_files, "../../XiaLabPro/R/google_drive_utils", "../../XiaLabPro/R/util_rawSpecPro");
  }else if(module.nm == "tandemMS"){
    file.sources <- c(general_files, "tandem_ms", "plotly_utils");
  }else if(module.nm == "metapaths"){
    file.sources <- c(general_files, metapath_files, mummichog_files, general_stat_files, general_anot_files, "stats_univariates");
  }else if(module.nm == "mgwas"){
    file.sources <- c(general_files,  mgwas_files);
  }else{
    print(paste("LoadRscripts Unknown module code: ", module.nm));
    return(0);
  }

  file_sources2 <- c("../../XiaLabPro/R/workflow_process", "../../XiaLabPro/R/project_management", "../../XiaLabPro/R/workflow_utils")
  file.sources <- c(file.sources, file_sources2)

  print(paste("Loading scripts for: ", module.nm));
  file.sources <- paste(loadPath, file.sources, ".Rc", sep="");
  library(compiler);
  sapply(file.sources,loadcmp,.GlobalEnv);
  return(1);
}

LoadReporter <- function(module.nm = "stat"){
  general_files <- c("Rmarkdown_general","Rmarkdown_reporter", "slides_reporter");
  if(module.nm == "stat"){
    file.sources <- c(general_files, "Rmarkdown_stats", "slides_stats", "../../MetaboAnalystR/R/stats_plot3d");
  }else if(module.nm == "mf"){
    file.sources <- c(general_files, "Rmarkdown_multifactor", "slides_multifactor");
  }else if(module.nm == "pathinteg"){
    file.sources <- c(general_files, "Rmarkdown_integmex", "slides_integmex");
  }else if(substring(module.nm, 1,4) == "path"){
    file.sources <- c(general_files, "Rmarkdown_pathway", "slides_pathway");
  }else if(module.nm == "roc" | module.nm == "power"){
    file.sources <- c(general_files, "Rmarkdown_biomarker", "Rmarkdown_power", "slides_biomarker", "slides_power");
  }else if(module.nm == "utils"){
    file.sources <- c(general_files, other_files);
  }else if(substring(module.nm, 1,4) == "mset" || module.nm == "enrich"){
    file.sources <- c(general_files, "Rmarkdown_enrichment", "slides_enrichment");
  }else if(module.nm == "network"){
    file.sources <- c(general_files, "Rmarkdown_network", "slides_network");
  }else if(module.nm == "mummichog" || module.nm == "mass_all" || module.nm == "mass_table"){
    file.sources <- c(general_files, "Rmarkdown_mummichog", "slides_mummichog");
  }else if(module.nm == "metapaths"){
    file.sources <- c(general_files, "Rmarkdown_mummichog", "slides_mummichog", "Rmarkdown_metamummi", "slides_metamummi");
  }else if(module.nm == "metadata"){
    file.sources <- c(general_files, "Rmarkdown_meta_analysis", "slides_meta_analysis");
  }else if(module.nm == "raw"){
    file.sources <- c(general_files, "Rmarkdown_raw_spectra", "slides_raw_spectra");
  }else if(module.nm == "tandemMS"){
    file.sources <- c(general_files, "Rmarkdown_tandemMS", "slides_tandemMS");
  }else if(module.nm == "dose"){
    file.sources <- c(general_files, "Rmarkdown_dose", "slides_dose");
  }else if(module.nm == "mgwas"){
    file.sources <- c(general_files, "Rmarkdown_mr", "slides_mr");
  }else{
    print(paste("LoadReporter Unknown module code: ", module.nm));
  }
  
  file.sources <- paste(loadPathOMS, file.sources, ".Rc", sep="");
  
  sapply(file.sources,loadcmp,.GlobalEnv);
}

# init path for anal.type == "raw"
CompileScripts <- function(){
  library(compiler);
  library(parallel);
  
  files <- list.files(compilePath, full.names=TRUE, pattern=".R$", recursive = TRUE);
  files <- files[!grepl("sweave_|test|_script_loader", files)]
  ncore <- ifelse(ceiling(detectCores()/3*2)>10, 6, ceiling(detectCores()/3*2));
  cat("compiling R codes with",ncore, "CPU Cores in parallel ...\n");
  cl <- makeCluster(getOption("cl.cores", ncore), outfile = "")
  clusterExport(cl, c("files"), envir = environment())
  res <- parSapply(cl, 
                   files, 
                   function(f){
                     compiler::cmpfile(f, paste(f, "c", sep=""), options=list(suppressAll=TRUE));
                   })
  stopCluster(cl)
  #  for(f in files){
  #    cmpfile(f, paste(f, "c", sep=""), options=list(suppressAll=TRUE));
  #  }
  
  # compile other code (including C/fortran/C++)
  print("compiling other codes .... ");
  print(workingPath);
  setwd(workingPath);
  cxxflgs <- capture.output(Rcpp:::CxxFlags())
  sink(file = "Makevars")
  cat("BATCHOBJECTS=fortran/decorana.o c/Internal_utils_batch.o\n")
  cat("UTILSOBJECTS=c/util.o c/fastmatch.o c/nncgc.o cpp/melt.o\n")
  cat("XCMSOBJECTS=c/mzROI.o c/xcms_binners.o\n")
  cat("INITOBJECT=Exports.o\n")
  cat("OBJECTS= $(BATCHOBJECTS) $(UTILSOBJECTS) $(XCMSOBJECTS) $(INITOBJECT)\n")
  cat("\n")
  cat("all: clean $(SHLIB)\n")
  cat("\n")
  cat("clean:\n")
  cat("\trm -f $(OBJECTS)\n")
  cat("\n")
  cat("CXX_STD = CXX11\n")
  cat("\n")
  cat("PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS) ")
  cat(cxxflgs, "\n")
  cat("PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)\n")
  sink()

  my.cmd <- paste("R CMD SHLIB", "decorana.f Internal_utils_batch.c util.c nncgc.c -o MetaboAnalyst.so", sep=" ");
  system(my.cmd);
  return(1);
}

LoadRscriptsOnDemand <- function(module_name) {
  # Mapping of modules to file groups
  module_files <- list(
    "stat" = c(general_stat_files, stats_files),
    "mf" = c(general_stat_files, multifac_files),
    "pathinteg" = c(general_anot_files, integmex_files, mummichog_files, general_stat_files, network_files),
    "path" = c(general_stat_files, general_anot_files, pathway_files),
    "pathora" = c(general_stat_files, general_anot_files, pathway_files),
    "pathqea" = c(general_stat_files, general_anot_files, pathway_files),
    "mset" = c(general_stat_files, general_anot_files, enrich_files),
    "msetora" = c(general_stat_files, general_anot_files, enrich_files),
    "msetqea" = c(general_stat_files, general_anot_files, enrich_files),
    "roc" = c(general_stat_files, biomarker_files),
    "power" = c(general_stat_files, power_files),
    "dose" = c(general_stat_files, dose_files),
    "utils" = c(general_anot_files, other_files),
    "network" = c(general_anot_files, network_files, general_stat_files),
    "mass_all" = c(mummichog_files),
    "mass_table" = c(mummichog_files, general_stat_files, "stats_univariates"),
    "mummichog" = c(mummichog_files, general_stat_files, "stats_univariates"),
    "metadata" = c(general_stat_files, metaanal_files),
    "raw" = c(spectra_files, "../../XiaLabPro/R/google_drive_utils", "../../XiaLabPro/R/util_rawSpecPro"),
    "tandemMS" = c("tandem_ms", "plotly_utils"),
    "metapaths" = c(metapath_files, mummichog_files, general_stat_files, general_anot_files, "stats_univariates"),
    "mgwas" = c(mgwas_files)
  )
  

  # Additional default files
  additional_files <- c("../../XiaLabPro/R/workflow_process", "../../XiaLabPro/R/project_management")

  # Compile and load scripts
  library(compiler)

  # Check if the module is known
  if (module_name %in% names(module_files)) {
    file.sources <- module_files[[module_name]]
    file.sources <- c(file.sources, additional_files)
    full_paths <- paste(loadPath, file.sources, ".Rc", sep = "")
    
    # Check if the first file exists; if not, update loadPath and recalc full paths
    if (!file.exists(full_paths[1])) {
      #loadPath <- "../../../rscripts/MetaboAnalystR/R/"
      full_paths <- paste(loadPath, file.sources, ".Rc", sep = "")
    }
    print(paste(loadPath, "======loadPath"));
    # Load only files that are not already loaded
    for (file in full_paths) {
        loadcmp(file, .GlobalEnv)
    }
  } else {
    print(paste("LoadRscriptsOnDemand Unknown module code: ", module_name))
  }

  return(1)
}
