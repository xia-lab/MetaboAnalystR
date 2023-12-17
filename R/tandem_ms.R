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
  # now set up the database option
  database_opt <- generateMS2dbOpt(database, ionMode);
  
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
  res[["SMILEs"]] <- res[["SMILEs"]][idx]
  res[["InchiKeys"]] <- res[["InchiKeys"]][idx]
  res[["Precursors"]] <- res[["Precursors"]][idx]
  res[["MS2refs"]] <- res[["MS2refs"]][idx]
  
  # sort based on score (high -> low)
  iddx <- order(res[["Scores"]][[1]], decreasing = T)
  res[["IDs"]] <- res[["IDs"]][iddx]
  res[["Scores"]][[1]] <- res[["Scores"]][[1]][iddx]
  res[["dot_product"]][[1]] <- res[["dot_product"]][[1]][iddx]
  res[["Neutral_loss"]][[1]] <- res[["Neutral_loss"]][[1]][iddx]
  res[["Compounds"]] <- res[["Compounds"]][iddx]
  res[["Formulas"]] <- res[["Formulas"]][iddx]
  res[["SMILEs"]] <- res[["SMILEs"]][iddx]
  res[["InchiKeys"]] <- res[["InchiKeys"]][iddx]
  res[["Precursors"]] <- res[["Precursors"]][iddx]
  res[["MS2refs"]] <- res[["MS2refs"]][iddx]
  
  return(res)
}

generateMS2dbOpt <- function(database = "all", ionMode = "positive"){
  if(database == "all"){ 
    database_opt <- "all";
  } else if(database == "hmdb_exp") {
    if(ionMode == "positive"){
      database_opt <- "HMDB_experimental_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "HMDB_experimental_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "hmdb_pre"){
    if(ionMode == "positive"){
      database_opt <- "HMDB_predicted_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "HMDB_predicted_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "gnps"){
    if(ionMode == "positive"){
      database_opt <- "GNPS_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "GNPS_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "mines"){
    if(ionMode == "positive"){
      database_opt <- "MINEs_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "MINEs_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "lipidblast"){
    if(ionMode == "positive"){
      database_opt <- "LipidBlast_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "LipidBlast_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "mona"){
    if(ionMode == "positive"){
      database_opt <- "MoNA_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "MoNA_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "massbank"){
    if(ionMode == "positive"){
      database_opt <- "MassBank_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "MassBank_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "riken"){
    if(ionMode == "positive"){
      database_opt <- "RIKEN_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "RIKEN_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "respect"){
    if(ionMode == "positive"){
      database_opt <- "ReSpect_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "ReSpect_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "msdial"){
    if(ionMode == "positive"){
      database_opt <- "MSDIAL_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "MSDIAL_NegDB";
    } else {
      database_opt <- "all";
    }
  } else if(database == "bmdms"){
    if(ionMode == "positive"){
      database_opt <- "BMDMS_PosDB";
    } else if(ionMode == "negative") {
      database_opt <- "BMDMS_PosDB";
    } else {
      database_opt <- "all";
    }
  }
  return(database_opt)
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
  return(round(mSetObj[["dataSet"]][["msms_result"]][[idx]][["Scores"]][[1]],2))
}

GetMSMSSmiles_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj[["dataSet"]][["msms_result"]][[idx]][["SMILEs"]])
}

GetMSMSInchiKeys_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj[["dataSet"]][["msms_result"]][[idx]][["InchiKeys"]])
}

GetMSMSPrecs_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(round(mSetObj[["dataSet"]][["msms_result"]][[idx]][["Precursors"]],4))
}

GetMSMSDot_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  return(round(mSetObj[["dataSet"]][["msms_result"]][[idx]][["dot_product"]][[1]],2))
}

plotMirror <- function(mSetObj=NA, featureidx = 1,
                       precMZ, ppm, 
                       imageNM = "",
                       dpi = 300, format = "png", width = 8, height = 8,
                       cutoff_relative = 5){
  # Fetch mSetobj
  mSetObj <- .get.mSet(mSetObj);
  save(mSetObj, featureidx, precMZ, ppm, imageNM, dpi, format, width, height, cutoff_relative, 
       file = "mSetObj___plotMirror.rda")
  # get plotting function
  MirrorPlotting <- OptiLCMS:::MirrorPlotting
  
  # Fetch data for plotting
  spec_df <- mSetObj[["dataSet"]][["spectrum_dataframe"]] # query spec
  spec_top <- spec_df
  
  ref_str <- mSetObj[["dataSet"]][["msms_result"]][[1]][["MS2refs"]][featureidx]
  spec_bottom <- OptiLCMS:::parse_ms2peaks(ref_str)
  
  # compoundName, score
  compoundName <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Compounds"]][featureidx]
  score <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Scores"]][[1]][featureidx]
  cat("compoundName ==> ", compoundName, "\n")
  cat("score        ==> ", score, "\n")
  
  # now, let's plot
  title <- paste0("Mirror plot of precursor: ", precMZ)
  subtitle <- paste0(compoundName, "\nMatching Score: ", round(score,3))
  p1 <- MirrorPlotting(spec_top, 
                       spec_bottom, 
                       ppm = ppm,
                       title= title, 
                       subtitle = subtitle,
                       cutoff_relative = cutoff_relative)
  
  # Save the static plot with Cairo
  Cairo::Cairo(
    file = imageNM, #paste0("mirrorplot_", precMZ, "_", dpi, ".", format),
    unit = "in", dpi = dpi, width = width, height = height, type = format, bg = "white")
  print(p1)
  dev.off()

  # Save the interactive plot with ggplot
  px <- plotly::ggplotly(p1);
  
  pxl <- list(px$x$data,px$x$layout,px$x$config);
  names(pxl) <- c("data","layout","config");
  jsonlist <- RJSONIO::toJSON(pxl, pretty = T,force = TRUE,.na = "null");
  sink(paste0(gsub(".png|.svg|.pdf", "", imageNM),".json"));
  cat(jsonlist);
  sink();
  
  if(is.null(mSetObj[["imgSet"]][["msmsmirror"]])){
    df <- data.frame(indx = featureidx, imageNM = imageNM)
    mSetObj[["imgSet"]][["msmsmirror"]] <- df
  } else {
    mSetObj[["imgSet"]][["msmsmirror"]] -> df0
    df <- data.frame(indx = featureidx, imageNM = imageNM)
    mSetObj[["imgSet"]][["msmsmirror"]] <- rbind(df, df0)
  }
  
  return(.set.mSet(mSetObj))
}

SaintyCheckMSPfile <- function(mSetObj=NA, filename = "", format_type = "mzmine"){
  
  # Fetch mSetobj
  mSetObj <- .get.mSet(mSetObj);
  
  if(filename == ""){
    AddErrMsg("The msp file is empty!");
  }
  
  
  if(format_type == "mzmine"){
    data_table <- read.delim(filename, header = F)
    
    start_idxs <- vapply(data_table[,1], function(x){x == "BEGIN IONS"}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    if(length(which(start_idxs)) > 50){
      AddMsg("Only first 50 tandem spectra will be searched!")
      keep50 = TRUE;
    } else {
      keep50 = FALSE;
    }
    
    end_idxs <- vapply(data_table[,1], function(x){x == "END IONS"}, 
                       FUN.VALUE = logical(1L), USE.NAMES = F)
    ms2_idxs <- vapply(data_table[,1], function(x){grepl(pattern = "Num peaks", x)}, 
                       FUN.VALUE = logical(1L), USE.NAMES = F)#Num peaks
    
    ms2_num <- vapply(data_table[ms2_idxs,1], function(x){
      as.integer(sub("Num peaks=", "",x))
    }, FUN.VALUE = integer(1L), USE.NAMES = F)
    
    start_idxs <- which(start_idxs)
    end_idxs <- which(end_idxs)
    ms2_idxs <- which(ms2_idxs)
    
    if(keep50){
      start_idxs <- start_idxs[1:50]
      end_idxs <- end_idxs[1:50]
      ms2_idxs <- ms2_idxs[1:50]
      ms2_num <- ms2_num[1:50]
    }
    
    ms2spec <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PEPMASS=", this_spec)
      prec_mz <- as.double(sub("PEPMASS=", "", this_spec[idx_prec]))
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num peaks=", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)-1))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, " ")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec[[i]] <- list(precursor = prec_mz, ms2_spec = ms2_spec)
    }
  } else if(format_type == "msdial"){
    data_table <- read.delim(filename, header = F,sep = "\n")
    
    start_idxs <- vapply(data_table[,1], function(x){grepl("NAME: ",x)}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    ms2_idxs <- vapply(data_table[,1], function(x){grepl("Num Peaks: ",x)}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    
    non0_idxs <- vapply(data_table[ms2_idxs,1], function(x){x == "Num Peaks: 0"}, 
                        FUN.VALUE = logical(1L), USE.NAMES = F)
    
    ms2_num <- vapply(data_table[ms2_idxs,1], function(x){
      as.integer(sub("Num Peaks: ", "",x))
    }, FUN.VALUE = integer(1L), USE.NAMES = F)
    
    if(length(which(!non0_idxs)) > 50){
      AddMsg("Only first 50 tandem spectra will be searched!")
      keep50 = TRUE;
    } else {
      keep50 = FALSE;
    }
    
    start_idxs <- which(start_idxs)
    end_idxs <- c(start_idxs[-1]-1, nrow(data_table))
    ms2_idxs <- which(ms2_idxs)
    
    start_idxs <- start_idxs[!non0_idxs]
    end_idxs <- end_idxs[!non0_idxs]
    ms2_idxs <- ms2_idxs[!non0_idxs]
    ms2_num <- ms2_num[!non0_idxs]
    if(keep50){
      start_idxs <- start_idxs[1:50]
      end_idxs <- end_idxs[1:50]
      ms2_idxs <- ms2_idxs[1:50]
      ms2_num <- ms2_num[1:50]
    }
    
    ms2spec <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PRECURSORMZ: ", this_spec)
      prec_mz <- as.double(sub("PRECURSORMZ: ", "", this_spec[idx_prec]))
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num Peaks: ", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, "\t")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec[[i]] <- list(precursor = prec_mz, ms2_spec = ms2_spec)
    }
  }

  save(ms2spec, file = "ms2spec____410.rda")
  mSetObj[["dataSet"]][["spectrum_set"]] <- ms2spec
  return(.set.mSet(mSetObj))
}

