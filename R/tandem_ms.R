# This script is used to search tandem MS spectrum 
processMSMSupload <- function(mSetObj=NA, spectrum){
  #
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$spectrum_str <- spectrum;
 
  spm_vec <- strsplit(spectrum, "\n")[[1]]
  spm_lst <- lapply(spm_vec, function(x){
    allv <- strsplit(x, "\t")[[1]]
    data.frame(mz = as.numeric(allv[1]), int = as.numeric(allv[2]))
  })
  spm_dtf <- do.call(rbind,spm_lst)
  mSetObj$dataSet$spectrum_dataframe <- spm_dtf;
  # save results into R mem for extraction/display later
  return(.set.mSet(mSetObj));
}

performMS2searchSingle <- function(mSetObj=NA, ppm1 = 10, ppm2 = 25, 
                                   dbpath = "",
                                   database = "all", 
                                   similarity_meth = 0,
                                   precMZ = NA, sim_cutoff = 30, ionMode = "positive",
                                   unit1 = "ppm", unit2 = "ppm"){
  require("OptiLCMS")
  mSetObj <- .get.mSet(mSetObj);
  # fetch the searching function
  SpectraSearchingSingle <- OptiLCMS:::SpectraSearchingSingle
  
  save(mSetObj, ppm1, ppm2, dbpath, database, similarity_meth, precMZ, sim_cutoff, ionMode, unit1, unit2, 
       file = "mSetObj___performMS2search.rda")
  # configure ppm/da param
  if(unit1 == "da"){
    ppm1 <- ppm1/precMZ*1e6;
  }
  if(unit2 == "da"){
    ppm2 <- ppm2/precMZ*1e6;
  }
  
  if(ionMode == "positive"){
    ion_mode_idx = 1;
  } else {
    ion_mode_idx = 0;
  }
  
  if(dbpath =="" || !file.exists(dbpath)){
    stop("Database file does not exist! Please check!")
  }
  
  if(database == "all"){ # "HMDB_experimental_PosDB"
    database_opt <- "all";
  } else if(database == "hmdb_exp") {
    database_opt <- "HMDB_experimental_PosDB";
  }
  # now let call OPTILCMS to search the database
  df <- as.matrix(mSetObj$dataSet$spectrum_dataframe)
  Concensus_spec <- list(as.vector(0L), list(list(df)))
  peak_mtx <- matrix(c(precMZ-1e-10, precMZ+1e-10, NA, NA), ncol = 4)
  colnames(peak_mtx) <- c("mzmin", "mzmax", "rtmin", "rtmax")
  results <- SpectraSearchingSingle(Concensus_spec, 0, peak_mtx, ppm1, ppm2, 
                                    ion_mode_idx, dbpath, database_opt)
  
  results_clean <- lapply(results, msmsResClean)
  mSetObj$dataSet$msms_result <- results_clean
  return(.set.mSet(mSetObj));
}

msmsResClean <- function(res){
  
  allcmpds <- res[["Compounds"]];
  allcmpds_unique <- unique(allcmpds);
  allscore <- res[["Scores"]][[1]];
  
  idx <- vapply(allcmpds_unique, function(cmpd){
    idx1 <- (allcmpds == cmpd)
    msco <- max(allscore[idx1])
    idx2 <- (allscore == msco)
    which(idx1 & idx2)[1]
  }, FUN.VALUE = integer(1L))
  
  res[["IDs"]] <- res[["IDs"]][idx]
  res[["Scores"]][[1]] <- res[["Scores"]][[1]][idx]
  res[["dot_product"]][[1]] <- res[["dot_product"]][[1]][idx]
  res[["Neutral_loss"]][[1]] <- res[["Neutral_loss"]][[1]][idx]
  res[["Compounds"]] <- res[["Compounds"]][idx]
  res[["Formulas"]] <- res[["Formulas"]][idx]
  
  res
}

GetMSMSCompoundNames_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj[["dataSet"]][["msms_result"]][[idx]][["Compounds"]])
}

GetMSMSFormulas_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj[["dataSet"]][["msms_result"]][[idx]][["Formulas"]])
}

GetMSMSSimScores_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj[["dataSet"]][["msms_result"]][[idx]][["Formulas"]])
}

plotMirror <- function(){
  
  
}
