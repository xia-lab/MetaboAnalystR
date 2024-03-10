# This script is used to search tandem MS spectrum 

#' processMSMSupload
#'
#' @param mSetObj mSetObj
#' @param spectrum spectrum for uploading
#' @export
#' @author Zhiqiang Pang

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

#' performMS2searchSingle
#'
#' @param mSetObj mSetObj
#' @param ppm1 ppm value for ms1
#' @param ppm2 ppm value for ms2
#' @param dbpath database path
#' @param frgdbpath fragmentation database path
#' @param database database option
#' @param similarity_meth similarity computing method
#' @param precMZ mz of precursor
#' @param sim_cutoff filtration cutoff of similarity score. will be enabled soon.
#' @param ionMode ion mode, for ESI+, is should be 1. for ESI-, it should be 0
#' @param unit1 ppm or da for ms1 matching
#' @param unit2 ppm or da for ms2
#' @export
performMS2searchSingle <- function(mSetObj=NA, ppm1 = 10, ppm2 = 25, 
                                   dbpath = "",
                                   frgdbpath = "",
                                   database = "all", 
                                   similarity_meth = 0,
                                   precMZ = NA, sim_cutoff = 30, ionMode = "positive",
                                   unit1 = "ppm", unit2 = "ppm"){
  require("OptiLCMS")
  mSetObj <- .get.mSet(mSetObj);
  # fetch the searching function
  SpectraSearchingSingle <- OptiLCMS:::SpectraSearchingSingle
  
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
  database <- gsub("\\[|\\]", "", database)
  database <- gsub(" ", "", database)
  database <- strsplit(database, ",")[[1]]
  database_opt <- generateMS2dbOpt(database, ionMode);
  
  # now need to check if need to convert to NL
  if(mSetObj[["dataSet"]]$MSMS_db_option == "nl"){
    mSetObj$dataSet$spectrum_dataframe$mz <- precMZ - mSetObj$dataSet$spectrum_dataframe$mz
    mSetObj$dataSet$spectrum_dataframe <- mSetObj$dataSet$spectrum_dataframe[mSetObj$dataSet$spectrum_dataframe$mz>0, ]
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
  
  # to extract fragments
  require(RSQLite)
  require(DBI)
  require(progress)
  con <- dbConnect(SQLite(), frgdbpath)
  dt_idx <- dbGetQuery(con, "SELECT * FROM Index_table")
  dbDisconnect(con)
  
  frgs_list <- lapply(results_clean[[1]][["IDs"]], function(n){
    dbs <- dt_idx$DB_Tables[which((dt_idx$Min_ID <= n) & (n <= dt_idx$Max_ID))]
    
    con <- dbConnect(SQLite(), frgdbpath)
    res <- dbGetQuery(con, paste0("SELECT Fragments FROM ", dbs, " WHERE ID=",n))
    dbDisconnect(con)
    strsplit(res$Fragments, "\t")[[1]]
  })

  mSetObj$dataSet$frgs_result[[1]] <- frgs_list
  
  res_df <- data.frame(IDs = results_clean[[1]][["IDs"]],
                       Scores = results_clean[[1]][["Scores"]][[1]],
                       Similarity_score = results_clean[[1]][["dot_product"]][[1]],
                       CompoundName = results_clean[[1]][["Compounds"]],
                       Formula = results_clean[[1]][["Formulas"]],
                       SMILES = results_clean[[1]][["SMILEs"]],
                       InchiKeys = results_clean[[1]][["InchiKeys"]],
                       Precursors = results_clean[[1]][["Precursors"]],
                       MS2_reference = results_clean[[1]][["MS2refs"]])
  
  qs::qsave(results_clean, file = "MS2searchSingle_results.qs")
  write.csv(res_df, file = "MS2searchSingle_results.csv", row.names = F)
  
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

Updatecurrentmsmsidx <- function(mSetObj=NA, label = ""){
  mSetObj <- .get.mSet(mSetObj);
  if(label != ""){
    idx <- which(label == mSetObj[["dataSet"]][["prec_mzrt_included"]])
    cat("Now the Updatecurrentmsmsidx ===>  ", idx, "\n")
    idx -> mSetObj[["dataSet"]][["current_msms_idx"]]
  }
  return(.set.mSet(mSetObj))
}

Setcurrentmsmsidx <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  cat("Now the Setcurrentmsmsidx ===>  ", idx, "\n")
  idx -> mSetObj[["dataSet"]][["current_msms_idx"]]
  return(.set.mSet(mSetObj))
}

GetMSMSFeatureLabel <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  llb <- mSetObj[["dataSet"]][["prec_mzrt_included"]][idx]
  return(llb)
}

GetMSMSCompoundNames_single <- function(mSetObj=NA, idx = 1){
  mSetObj <- .get.mSet(mSetObj);
  idx -> mSetObj[["dataSet"]][["current_msms_idx"]]
  .set.mSet(mSetObj)
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

#' plotMirror
#' @param mSetObj mSetObj
#' @param featureidx index of feature
#' @param precMZ mz of precursor
#' @param ppm ppm for ms2 fragment matching mz error
#' @param imageNM image name
#' @param dpi dpi of images
#' @param format format of images
#' @param width width of images
#' @param height height of images
#' @param cutoff_relative cutoff of relative intensity to filter out
#' @export
#' @author Zhiqiang Pang

plotMirror <- function(mSetObj=NA, featureidx = 1,
                       precMZ, ppm, 
                       imageNM = "",
                       dpi = 300, format = "png", width = 8, height = 8,
                       cutoff_relative = 5){
  # Fetch mSetobj
  mSetObj <- .get.mSet(mSetObj);
  #cat("Now the height is == > ", height, "\n")

  # get plotting function
  MirrorPlotting <- OptiLCMS:::MirrorPlotting
  
  # Fetch data for plotting
  if(!is.null(mSetObj[["dataSet"]][["spectrum_dataframe"]])){
    spec_df <- mSetObj[["dataSet"]][["spectrum_dataframe"]] # query spec
    spec_top <- spec_df
    
    ref_str <- mSetObj[["dataSet"]][["msms_result"]][[1]][["MS2refs"]][featureidx]
    frgs_vec <- mSetObj$dataSet$frgs_result[[1]][[featureidx]]
    if(is.na(ref_str)){
      return (1);
    }
    spec_bottom <- OptiLCMS:::parse_ms2peaks(ref_str)
    # compoundName, score
    compoundName <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Compounds"]][featureidx]
    score <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Scores"]][[1]][featureidx]
    smiles <- mSetObj[["dataSet"]][["msms_result"]][[1]][["SMILEs"]][featureidx]
    inchikeys <- mSetObj[["dataSet"]][["msms_result"]][[1]][["InchiKeys"]][featureidx]
    formulas <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Formulas"]][featureidx]
    cat("compoundName ==> ", compoundName, "\n")
    cat("score        ==> ", score, "\n")
    current_msms_idx <- 1;
    
  } else {
    if(!is.null(mSetObj[["dataSet"]][["current_msms_idx"]])){
      current_msms_idx <- mSetObj[["dataSet"]][["current_msms_idx"]]
    } else {
      current_msms_idx <- 1
    }
    cat("Now the current_msms_idx ==> ", current_msms_idx, "\n")
    cat("Now the featureidx ==> ", featureidx, "\n")
    spec_df <- mSetObj[["dataSet"]][["spectrum_set"]][[current_msms_idx]][["ms2_spec"]]
    spec_top <- spec_df
    
    ref_str <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["MS2refs"]][featureidx]
    frgs_vec <- mSetObj$dataSet$frgs_result[[current_msms_idx]][[featureidx]]
    if(is.na(ref_str)){
      return (1);
    }
    spec_bottom <- OptiLCMS:::parse_ms2peaks(ref_str)
    # compoundName, score
    compoundName <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["Compounds"]][featureidx]
    score <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["Scores"]][[1]][featureidx]
    smiles <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["SMILEs"]][featureidx]
    inchikeys <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["InchiKeys"]][featureidx]
    formulas <- mSetObj[["dataSet"]][["msms_result"]][[current_msms_idx]][["Formulas"]][featureidx]
    
    cat("compoundName ==> ", compoundName, "\n")
    cat("score        ==> ", score, "\n")
    
  }

  # prepare fragments
  normalize_spectrum <- OptiLCMS:::normalize_spectrum
  bottom <- normalize_spectrum(spec_bottom, cutoff_relative)
  frgs_vec_done <- vapply(bottom$mz, function(x){
    y <- frgs_vec[spec_bottom[,1] == x]
    if(is.na(y[1])){return("")}
    return(y[1])
  }, FUN.VALUE = character(1L))
  write.table(bottom[,c(1:2)], 
              file = paste0("reference_spectrum_",featureidx-1, "_", precMZ,".txt"), 
              row.names = F, col.names = T)
  
  df_cmpd <- data.frame(CompoundName = compoundName,
                        score = score,
                        SMILES = smiles,
                        InchiKeys = inchikeys,
                        Formula = formulas)
  write.table(df_cmpd, 
              file = paste0("compound_info_",featureidx-1, "_", precMZ,".txt"), 
              row.names = F, col.names = T)
  
  # now, let's plot
  title <- paste0("Precursor: ", precMZ)
  subtitle <- paste0(compoundName, "\nMatching Score: ", round(score,3))
  p1 <- MirrorPlotting(spec_top, 
                       spec_bottom, 
                       ppm = ppm,
                       title= title, 
                       subtitle = subtitle,
                       cutoff_relative = cutoff_relative)
  p1 <- p1 + theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    text=element_text(family="Helvetica", face = "plain"),
    plot.subtitle=element_text(size=13, face="plain", color="black"),
    plot.title=element_text(size=18, face="plain", color="black"))
  
  # Save the static plot with Cairo
  Cairo::Cairo(
    file = imageNM, #paste0("mirrorplot_", precMZ, "_", dpi, ".", format),
    unit = "in", dpi = dpi, width = width, height = height, type = format, bg = "white")
  print(p1)
  dev.off()

  # Save the interactive plot with ggplot
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
      non_na_txt <- px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])]
      frgs_vec_done[frgs_vec_done==""] <- "Unknown"
      new_labels <- sapply(1:length(non_na_txt), function(x){
          paste0(non_na_txt[x], "<br />Fragment Formula: ", frgs_vec_done[ceiling(x/2)])
      })
      px[["x"]][["data"]][[2]][["text"]][!is.na(px[["x"]][["data"]][[2]][["text"]])] <- new_labels
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

  
  jsonlist <- RJSONIO::toJSON(px, pretty = T,force = TRUE,.na = "null");
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


#' SaintyCheckMSPfile
#'
#' @param mSetObj mSetObj
#' @param filename filename with path
#' @param format_type format type, can be 'mzmine' or 'msdial'
#' @param keepAllspec if you want to search all spectra from your local, turn keepAllspec to TRUE. it is FALSE by default.
#' @export
#' @author Zhiqiang Pang

SaintyCheckMSPfile <- function(mSetObj=NA, filename = "", format_type = "mzmine", keepAllspec = FALSE){
  
  # Fetch mSetobj
  mSetObj <- .get.mSet(mSetObj);
  
  if(filename == ""){
    AddErrMsg("The msp file is empty!");
  }
  
  Msg <- vector(mode = "character");

  if(format_type == "mzmine"){
    data_table <- read.delim(filename, header = F)
    mSetObj[["dataSet"]][["spectrum_raw"]] <- data_table
    
    Msg <- c(Msg, "Your msp file is generated by mzMine.")
    start_idxs <- vapply(data_table[,1], function(x){x == "BEGIN IONS"}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    Msg <- c(Msg, paste0("A total of ", length(which(start_idxs)), " MS/MS spectra included in your data."))
    if(length(which(start_idxs)) > 20){
      AddMsg("Only first 20 tandem spectra will be searched!")
      Msg <- c(Msg, paste0("Only first 20 tandem spectra will be searched by default!"))
      keep20 = TRUE;
    } else {
      keep20 = FALSE;
    }
    
    end_idxs <- vapply(data_table[,1], function(x){x == "END IONS"}, 
                       FUN.VALUE = logical(1L), USE.NAMES = F)
    rt_idxs <- vapply(data_table[,1], function(x){x == "RTINSECONDS"}, 
                       FUN.VALUE = logical(1L), USE.NAMES = F)
    ms2_idxs <- vapply(data_table[,1], function(x){grepl(pattern = "Num peaks", x)}, 
                       FUN.VALUE = logical(1L), USE.NAMES = F)#Num peaks
    
    ms2_num <- vapply(data_table[ms2_idxs,1], function(x){
      as.integer(sub("Num peaks=", "",x))
    }, FUN.VALUE = integer(1L), USE.NAMES = F)
    
    start_idxs <- which(start_idxs)
    end_idxs <- which(end_idxs)
    rt_idxs <- which(rt_idxs)
    ms2_idxs <- which(ms2_idxs)
    
    idx_prec0 <- grep("PEPMASS=", data_table[,1])
    prec_mz_all <- as.double(sub("PEPMASS=", "", data_table[idx_prec0,1]))
    idx_precrt0 <- grep("RTINSECONDS=", data_table[,1])
    prec_rt_all <- as.double(sub("RTINSECONDS=", "", data_table[idx_precrt0,1]))
    mSetObj[["dataSet"]][["prec_mz_all"]] <- prec_mz_all
    mSetObj[["dataSet"]][["prec_rt_all"]] <- prec_rt_all
    mSetObj[["dataSet"]][["prec_mzrt_all"]] <- paste0(prec_mz_all, "mz@", prec_rt_all, "sec")
    
    ms2spec_full <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PEPMASS=", this_spec)
      prec_mz <- as.double(sub("PEPMASS=", "", this_spec[idx_prec]))
      idx_precrt <- grep("RTINSECONDS=", this_spec)
      prec_rt <- as.double(sub("RTINSECONDS=", "", this_spec[idx_precrt]))
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num peaks=", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)-1))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, " ")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec_full[[i]] <- list(precursor = prec_mz, prec_rt = prec_rt, ms2_spec = ms2_spec)
    }
    
    if(keep20){
      start_idxs <- start_idxs[1:20]
      end_idxs <- end_idxs[1:20]
      ms2_idxs <- ms2_idxs[1:20]
      ms2_num <- ms2_num[1:20]
    }
    
    ms2spec <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PEPMASS=", this_spec)
      prec_mz <- as.double(sub("PEPMASS=", "", this_spec[idx_prec]))
      idx_precrt <- grep("RTINSECONDS=", this_spec)
      prec_rt <- as.double(sub("RTINSECONDS=", "", this_spec[idx_precrt]))
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num peaks=", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)-1))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, " ")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec[[i]] <- list(precursor = prec_mz, prec_rt = prec_rt, ms2_spec = ms2_spec)
    }
    all_precmz <- vapply(ms2spec, function(x){x[[1]]}, FUN.VALUE = double(1L))
    all_precrt <- vapply(ms2spec, function(x){x[[2]]}, FUN.VALUE = double(1L))
    msms_frg_num <- vapply(ms2spec, function(x){nrow(x[[3]])}, FUN.VALUE = double(1L))
    Msg <- c(Msg, paste0("The m/z range of all included precursors in your data is from ", min(all_precmz), " to ", max(all_precmz), "."))
    Msg <- c(Msg, paste0("The retention time range of all included precursors in your data is from ", min(all_precrt), " to ", max(all_precrt), "."))
    Msg <- c(Msg, paste0("The minimum number of MS/MS fragments is ", min(msms_frg_num), "."))
    Msg <- c(Msg, paste0("The maximum number of MS/MS fragments is ", max(msms_frg_num), "."))
    
    mSetObj[["dataSet"]][["prec_mz_included"]] <- all_precmz
    mSetObj[["dataSet"]][["prec_rt_included"]] <- all_precrt
    mSetObj[["dataSet"]][["prec_mzrt_included"]] <- paste0(all_precmz, "mz@", all_precrt, "sec")
    
  } else if(format_type == "msdial"){
    data_table <- read.delim(filename, header = F,sep = "\n")
    mSetObj[["dataSet"]][["spectrum_raw"]] <- data_table
    
    Msg <- c(Msg, "Your msp file is exported by MS-DIAL")
    start_idxs <- vapply(data_table[,1], function(x){grepl("NAME: ",x)}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    ms2_idxs <- vapply(data_table[,1], function(x){grepl("Num Peaks: ",x)}, 
                         FUN.VALUE = logical(1L), USE.NAMES = F)
    rt_idxs <- vapply(data_table[,1], function(x){x == "RETENTIONTIME"}, 
                      FUN.VALUE = logical(1L), USE.NAMES = F)
    non0_idxs <- vapply(data_table[ms2_idxs,1], function(x){x == "Num Peaks: 0"}, 
                        FUN.VALUE = logical(1L), USE.NAMES = F)
    
    ms2_num <- vapply(data_table[ms2_idxs,1], function(x){
      as.integer(sub("Num Peaks: ", "",x))
    }, FUN.VALUE = integer(1L), USE.NAMES = F)
    Msg <- c(Msg, paste0("A total of ", length(which(start_idxs)), " MS/MS records detected in your data."))
    Msg <- c(Msg, paste0("A total of ", length(which(!non0_idxs)), " non-empty MS/MS spectra found in your data."))
    if(length(which(!non0_idxs)) > 20){
      AddMsg("Only first 20 tandem spectra will be searched!")
      Msg <- c(Msg, paste0("Only first 20 tandem spectra will be searched by default!"))
      keep20 = TRUE;
    } else {
      keep20 = FALSE;
    }
    
    start_idxs <- which(start_idxs)
    rt_idxs <- which(rt_idxs)
    end_idxs <- c(start_idxs[-1]-1, nrow(data_table))
    ms2_idxs <- which(ms2_idxs)
    
    start_idxs <- start_idxs[!non0_idxs]
    rt_idxs <- rt_idxs[!non0_idxs]
    end_idxs <- end_idxs[!non0_idxs]
    ms2_idxs <- ms2_idxs[!non0_idxs]
    ms2_num <- ms2_num[!non0_idxs]
    
    idx_prec0 <- grep("PRECURSORMZ: ", data_table[,1])
    idx_precrt0 <- grep("RETENTIONTIME: ", data_table[,1])
    prec_mz_all <- as.double(sub("PRECURSORMZ: ", "", data_table[idx_prec0,1]))
    prec_rt_all <- as.double(sub("RETENTIONTIME: ", "", data_table[idx_precrt0,1]))
    mSetObj[["dataSet"]][["prec_mz_all"]] <- prec_mz_all[!non0_idxs]
    mSetObj[["dataSet"]][["prec_rt_all"]] <- prec_rt_all[!non0_idxs]
    mSetObj[["dataSet"]][["prec_mzrt_all"]] <- paste0(prec_mz_all[!non0_idxs], "mz@", prec_rt_all[!non0_idxs], "min")
    
    ms2spec_full <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PRECURSORMZ: ", this_spec)
      prec_mz <- as.double(sub("PRECURSORMZ: ", "", this_spec[idx_prec]))
      
      idx_precrt <- grep("RETENTIONTIME: ", this_spec)
      prec_rt <- as.double(sub("RETENTIONTIME: ", "", this_spec[idx_precrt]))
      
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num Peaks: ", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, "\t")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec_full[[i]] <- list(precursor = prec_mz, prec_rt = prec_rt, ms2_spec = ms2_spec)
    }
    
    if(keep20){
      start_idxs <- start_idxs[1:20]
      end_idxs <- end_idxs[1:20]
      ms2_idxs <- ms2_idxs[1:20]
      ms2_num <- ms2_num[1:20]
    }
    
    ms2spec <- vector(mode = "list", length(ms2_num))
    for(i in 1:length(ms2_num)){
      this_spec <- data_table[c(start_idxs[i]:end_idxs[i]),]
      idx_prec <- grep("PRECURSORMZ: ", this_spec)
      prec_mz <- as.double(sub("PRECURSORMZ: ", "", this_spec[idx_prec]))
      
      idx_precrt <- grep("RETENTIONTIME: ", this_spec)
      prec_rt <- as.double(sub("RETENTIONTIME: ", "", this_spec[idx_precrt]))
      
      len_ms2_spec <- ms2_num[i]
      ms2_this_idx <- grep("Num Peaks: ", this_spec)
      spec_strs <- this_spec[c((ms2_this_idx+1):(length(this_spec)))]
      spec_list <- lapply(spec_strs, function(x){
        vals <- as.double(strsplit(x, "\t")[[1]])
        data.frame(mz = vals[1], int = vals[2])
      })
      ms2_spec <- do.call(rbind, spec_list)
      ms2spec[[i]] <- list(precursor = prec_mz,prec_rt = prec_rt, ms2_spec = ms2_spec)
    }

    all_precmz <- vapply(ms2spec, function(x){x[[1]]}, FUN.VALUE = double(1L))
    all_precrt <- vapply(ms2spec, function(x){x[[2]]}, FUN.VALUE = double(1L))
    msms_frg_num <- vapply(ms2spec, function(x){nrow(x[[3]])}, FUN.VALUE = double(1L))
    
    Msg <- c(Msg, paste0("The m/z range of all precursors in your data is from ", min(all_precmz), " to ", max(all_precmz), "."))
    Msg <- c(Msg, paste0("The retention time range of all included precursors in your data is from ", min(all_precrt), " to ", max(all_precrt), "."))
    Msg <- c(Msg, paste0("The minimum number of MS/MS fragments is ", min(msms_frg_num), "."))
    Msg <- c(Msg, paste0("The maximum number of MS/MS fragments is ", max(msms_frg_num), "."))
    
    mSetObj[["dataSet"]][["prec_mz_included"]] <- all_precmz
    mSetObj[["dataSet"]][["prec_rt_included"]] <- all_precrt
    mSetObj[["dataSet"]][["prec_mzrt_included"]] <- paste0(all_precmz, "mz@", all_precrt, "min")
  }
  if(mSetObj[["dataSet"]][["MSMS_db_option"]] == "nl"){
    Msg <- c(Msg, paste0("Database searching is based on <u>Neutral Loss</u> spectra of corresponding MS/MS spectra."))
  }
  if(keep20){
    Msg <- c(Msg, paste0("Please use <b>Edit</b> button below to manually update the inclusion list for database searching!"))
    Msg <- c(Msg, paste0("Please click <b>Proceed</b> button to start database searching."))
  } else {
    Msg <- c(Msg, paste0("Please click <b>Proceed</b> button to start database searching."))
  }

  if(mSetObj[["dataSet"]][["MSMS_db_option"]] == "nl"){
    ms2spec <- lapply(ms2spec, convert2NLspec)
    ms2spec_full <- lapply(ms2spec_full, convert2NLspec)
  }
  mSetObj[["msgSet"]][["sanity_msgvec"]] <- Msg
  mSetObj[["dataSet"]][["spectrum_set"]] <- ms2spec
  mSetObj[["dataSet"]][["spectrum_set_full"]] <- ms2spec_full
  return(.set.mSet(mSetObj))
}

convert2NLspec <- function(data_spec){
  spectrum_dataframe <- data_spec$ms2_spec
  precMZ <- data_spec$precursor
  spectrum_dataframe$mz <- precMZ - spectrum_dataframe$mz
  spectrum_dataframe <- spectrum_dataframe[spectrum_dataframe$mz>0, ]
  data_spec$ms2_spec <- spectrum_dataframe
  return(data_spec)
}

#' performMS2searchBatch
#'
#' @param mSetObj mSetObj
#' @param ppm1 ppm value for ms1
#' @param ppm2 ppm value for ms2
#' @param dbpath database path
#' @param frgdbpath fragmentation database path
#' @param database database option
#' @param similarity_meth similarity computing method
#' @param precMZ mz of precursor
#' @param sim_cutoff filtration cutoff of similarity score. will be enabled soon.
#' @param ionMode ion mode, for ESI+, is should be 1. for ESI-, it should be 0
#' @param unit1 ppm or da for ms1 matching
#' @param unit2 ppm or da for ms2
#' @param ncores number of cpu cores used to search
#' @export
#' @author Zhiqiang Pang 

performMS2searchBatch <- function(mSetObj=NA, ppm1 = 10, ppm2 = 25, 
                                  dbpath = "",
                                  frgdbpath = "",
                                  database = "all", 
                                  similarity_meth = 0,
                                  precMZ = NA, sim_cutoff = 30, ionMode = "positive",
                                  unit1 = "ppm", unit2 = "ppm", ncores = 1){
  require("OptiLCMS")
  mSetObj <- .get.mSet(mSetObj);
  # fetch the searching function
  SpectraSearchingBatch <- OptiLCMS:::SpectraSearchingBatch

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
  database <- gsub("\\[|\\]", "", database)
  database <- gsub(" ", "", database)
  database <- strsplit(database, ",")[[1]]
  database_opt <- generateMS2dbOpt(database, ionMode);
  
  # now let call OPTILCMS to search the database
  # df <- as.matrix(mSetObj$dataSet$spectrum_dataframe)
  spec_set <- mSetObj[["dataSet"]][["spectrum_set"]]
  spec_set_ms2 <- lapply(spec_set, function(x){list(as.matrix(x[[3]]))})
  spec_set_prec <- sapply(spec_set, function(x){x[[1]]})
  Concensus_spec <- list(as.vector(seq(0,length(spec_set)-1)), spec_set_ms2)
  
  peak_mtx <- cbind(spec_set_prec-1e-10, spec_set_prec+1e-10, NA, NA)
  colnames(peak_mtx) <- c("mzmin", "mzmax", "rtmin", "rtmax")
  
  if(ncores == 1){
    results <- SpectraSearchingBatch(Concensus_spec, 0:(length(spec_set_prec)-1), peak_mtx, ppm1, ppm2, 
                                     ion_mode_idx, dbpath, database_opt)
  } else {
    # for multiple cores
    require(parallel);
    cl <- makeCluster(getOption("cl.cores", ncores))
    clusterExport(cl, c("Concensus_spec", "peak_mtx", "spec_set_prec",
                        "ppm1", "ppm2", "ion_mode_idx", "dbpath", 
                        "database_opt", "SpectraSearchingBatch"), envir = environment())
    res1 <- list()
    res1 <- parLapply(cl, 
                      1:ncores, 
                      function(x, Concensus_spec, peak_mtx,  
                               ppm1, ppm2, ion_mode_idx, dbpath, 
                               database_opt, ncores){
                        length_data <- length(Concensus_spec[[1]])
                        lg_pcore <- ceiling(length_data/ncores)
                        
                        if(ncores == x){
                          vec_idx <- ((x-1)*lg_pcore+1):length_data
                        } else {
                          vec_idx <- ((x-1)*lg_pcore+1):(x*lg_pcore)
                        }
                        
                        Concensus_spec0 <- Concensus_spec
                        Concensus_spec0[[1]] <- 0:length(vec_idx)
                        Concensus_spec0[[2]] <- Concensus_spec0[[2]][vec_idx]
                        
                        res0 <- SpectraSearchingBatch(Concensus_spec0, 
                                                      0:(length(vec_idx)-1), 
                                                      peak_mtx[vec_idx,], 
                                                      ppm1, ppm2, ion_mode_idx, dbpath, database_opt)
                        res0},
                      Concensus_spec = Concensus_spec,
                      peak_mtx = peak_mtx,
                      ppm2 = ppm2,
                      ppm1 = ppm1,
                      ion_mode_idx = ion_mode_idx,
                      dbpath = dbpath,
                      database_opt = database_opt,
                      ncores = ncores)
    stopCluster(cl)
    
    res <- list()
    for(i in 1:ncores){
      res <- c(res, res1[[i]])
    }
    res -> results
  }
   
  results_clean <- lapply(results, msmsResClean)
  mSetObj$dataSet$msms_result <- results_clean
  
  # to extract fragments
  require(RSQLite)
  require(DBI)
  require(progress)
  con <- dbConnect(SQLite(), frgdbpath)
  dt_idx <- dbGetQuery(con, "SELECT * FROM Index_table")
  dbDisconnect(con)
  
  for(k in 1:length(results_clean)){
    frgs_list <- lapply(results_clean[[k]][["IDs"]], function(n){
      dbs <- dt_idx$DB_Tables[which((dt_idx$Min_ID <= n) & (n <= dt_idx$Max_ID))]
      
      con <- dbConnect(SQLite(), frgdbpath)
      res <- dbGetQuery(con, paste0("SELECT Fragments FROM ", dbs, " WHERE ID=",n))
      dbDisconnect(con)
      strsplit(res$Fragments, "\t")[[1]]
    })
    
    mSetObj$dataSet$frgs_result[[k]] <- frgs_list
  }
  
  mSetObj$dataSet$spec_set_prec <- spec_set_prec;
  return(.set.mSet(mSetObj));
}

DataUpdatefromInclusionList <- function(mSetObj=NA, included_str = ""){
  if(included_str == ""){
    return (1);
  }
  mSetObj <- .get.mSet(mSetObj);
  
  Msg = vector()
  included_list <- strsplit(included_str, "\n")[[1]]
  
  spectrum_set_full <- mSetObj[["dataSet"]][["spectrum_set_full"]]
  prec_mzrt_all <- mSetObj[["dataSet"]][["prec_mzrt_all"]]
  
  idxs <- vapply(included_list, function(x){
    which(x == prec_mzrt_all)[1]
  }, FUN.VALUE = integer(1L))
  
  if(length(included_list)>20){
    mSetObj[["dataSet"]][["prec_mz_included"]] <- 
      mSetObj[["dataSet"]][["prec_mz_all"]][idxs][1:20];
    
    mSetObj[["dataSet"]][["prec_rt_included"]] <-
      mSetObj[["dataSet"]][["prec_rt_all"]][idxs][1:20];
    
    mSetObj[["dataSet"]][["prec_mzrt_included"]] <- 
      mSetObj[["dataSet"]][["prec_mzrt_all"]][idxs][1:20];
    mSetObj[["dataSet"]][["spectrum_set"]] <- 
      mSetObj[["dataSet"]][["spectrum_set_full"]][idxs][1:20];
    
    ms2spec <- mSetObj[["dataSet"]][["spectrum_set"]];
    all_precmz <- vapply(ms2spec, function(x){x[[1]]}, FUN.VALUE = double(1L))
    all_precrt <- vapply(ms2spec, function(x){x[[2]]}, FUN.VALUE = double(1L))
    msms_frg_num <- vapply(ms2spec, function(x){nrow(x[[3]])}, FUN.VALUE = double(1L))
    
    Msg <- c(Msg, paste0("Only first 20 tandem spectra will be searched by default!"))
    Msg <- c(Msg, paste0("The m/z range of all precursors in your data is from ", min(all_precmz), " to ", max(all_precmz), "."))
    Msg <- c(Msg, paste0("The retention time range of all included precursors in your data is from ", min(all_precrt), " to ", max(all_precrt), "."))
    Msg <- c(Msg, paste0("The minimum number of MS/MS fragments is ", min(msms_frg_num), "."))
    Msg <- c(Msg, paste0("The maximum number of MS/MS fragments is ", max(msms_frg_num), "."))
    Msg <- c(Msg, paste0("Please use <b>Edit</b> button below to manually update the inclusion list for database searching!"))
    Msg <- c(Msg, paste0("Please click <b>Proceed</b> button to start database searching."))
  } else {
    mSetObj[["dataSet"]][["prec_mz_included"]] <- 
      mSetObj[["dataSet"]][["prec_mz_all"]][idxs];
    
    mSetObj[["dataSet"]][["prec_rt_included"]] <-
      mSetObj[["dataSet"]][["prec_rt_all"]][idxs];
    
    mSetObj[["dataSet"]][["prec_mzrt_included"]] <- 
      mSetObj[["dataSet"]][["prec_mzrt_all"]][idxs];
    
    mSetObj[["dataSet"]][["spectrum_set"]] <- 
      mSetObj[["dataSet"]][["spectrum_set_full"]][idxs];
    
    ms2spec <- mSetObj[["dataSet"]][["spectrum_set"]];
    all_precmz <- vapply(ms2spec, function(x){x[[1]]}, FUN.VALUE = double(1L))
    all_precrt <- vapply(ms2spec, function(x){x[[2]]}, FUN.VALUE = double(1L))
    msms_frg_num <- vapply(ms2spec, function(x){nrow(x[[3]])}, FUN.VALUE = double(1L))
    
    Msg <- c(Msg, paste0("A total of ", length(idxs), " tandem spectra will be searched!"))
    Msg <- c(Msg, paste0("The m/z range of all precursors in your data is from ", min(all_precmz), " to ", max(all_precmz), "."))
    Msg <- c(Msg, paste0("The retention time range of all included precursors in your data is from ", min(all_precrt), " to ", max(all_precrt), "."))
    Msg <- c(Msg, paste0("The minimum number of MS/MS fragments is ", min(msms_frg_num), "."))
    Msg <- c(Msg, paste0("The maximum number of MS/MS fragments is ", max(msms_frg_num), "."))
    Msg <- c(Msg, paste0("Please click <b>Proceed</b> button to start database searching."))
  }
  
  
  mSetObj[["msgSet"]][["sanity_msgvec"]] <- c(mSetObj[["msgSet"]][["sanity_msgvec"]][1:2], Msg)
  return(.set.mSet(mSetObj))
}

SummarizeCMPDResults <- function(mSetObj=NA, top_cutoff = 60, low_cutoff = 20){
  mSetObj <- .get.mSet(mSetObj);
  msms_result <- mSetObj[["dataSet"]][["msms_result"]]
  
  top_scores <- vapply(msms_result, function(x){
    if(length(x[["Scores"]][[1]]) == 0){
      return(0.0)
    }
    max(x[["Scores"]][[1]])
  }, FUN.VALUE = double(1L))
  
  res <- vector(mode = "integer", length = 3L)
  res[1] <- length(which(top_scores >= top_cutoff))
  res[2] <- length(which((top_scores < top_cutoff) & (top_scores >= low_cutoff)))
  res[3] <- length(which(top_scores < low_cutoff))
  
  return(res)
}

GetMSMSPrecMZvec_msp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$spec_set_prec -> spec_set_prec
  return(spec_set_prec)
}

GetMSMSPrecMZvec_msp_min <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$spec_set_prec -> spec_set_prec
  return(min(spec_set_prec))
}

FetchExampleMSP <- function(URL = ""){
  download.file(url = URL, destfile = basename(URL), method = "curl")
  return(1)
}

GetMSPSanityCheckMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["msgSet"]][["sanity_msgvec"]] -> Msg;
  return(Msg)
}

GetAllPrecMZs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mz_all"]] -> prec_mz_all;
  return(prec_mz_all)
}

GetIncludedPrecMZs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mz_included"]] -> all_precmz
  #cat("all_precmz    ==> ", all_precmz, "\n")
  return(all_precmz)
}

GetNonIncludedPrecMZs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mz_included"]] -> all_precmz;
  mSetObj[["dataSet"]][["prec_mz_all"]] -> prec_mz_all;
  all_nonprecmz <- prec_mz_all[!(prec_mz_all %in% all_precmz)]
  #cat("all_nonprecmz ==> ", all_nonprecmz, "\n")
  return(all_nonprecmz)
}

GetAllPrecMZRTs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mzrt_all"]] -> prec_mz_allrt;
  return(prec_mz_allrt)
}

GetIncludedPrecMZRTs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mzrt_included"]] -> all_precmzrt
  #cat("all_precmz    ==> ", all_precmz, "\n")
  return(all_precmzrt)
}

GetNonIncludedPrecMZRTs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj[["dataSet"]][["prec_mzrt_included"]] -> all_precmzrt;
  mSetObj[["dataSet"]][["prec_mzrt_all"]] -> prec_mzrt_all;
  all_nonprecmzrt <- prec_mzrt_all[!(prec_mzrt_all %in% all_precmzrt)]
  #cat("all_nonprecmz ==> ", all_nonprecmz, "\n")
  return(all_nonprecmzrt)
}


PlotMS2SummarySingle <- function(mSetObj=NA, imageNM = "", option = 0L, dpi = 72, format="png", width = 12, height = 8){
  mSetObj <- .get.mSet(mSetObj);

  if(is.null(mSetObj[["dataSet"]][["msms_result"]])){
    if(file.exists("MS2searchSingle_results.qs")){
      ms2_res <- qs::qread("MS2searchSingle_results.qs")[[1]]
    } else {
      return(0)
    }
  } else {
    ms2_res <- mSetObj[["dataSet"]][["msms_result"]][[1]]
  }
  
  if(option == 0L){
    ms2_res[["dot_product"]][[1]] -> simi_vec;
    num <- c(length(which(simi_vec>=0.8)),
             length(which(simi_vec>=0.6 & simi_vec<0.8)),
             length(which(simi_vec<0.6)))
    
    levls <- factor(c("High(>0.8)", "Medium(0.6-0.8)", "Low(<0.6)"), levels = c("High(>0.8)", "Medium(0.6-0.8)", "Low(<0.6)"))
    
    df <- data.frame(levls = levls,
                     Number = num)
    
    cbPalette <- c("#009E73", "#E69F00", "#999999")
    require(ggplot2)
    p1 <- ggplot(data=df, aes(x=levls, y=Number)) +
      geom_bar(stat="identity", aes(fill=levls), width = 0.618)+
      geom_text(aes(label=Number), vjust=1.6, color="white", size=6)+
      theme_minimal() + scale_fill_manual(values=cbPalette) + 
      scale_color_manual(values=cbPalette) + 
      guides(fill=guide_legend(title="Confidence Level"))+
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),  
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14))
    
  } else {
    ms2_res[["Scores"]][[1]] -> simi_vec;
    num <- c(length(which(simi_vec>=80)),
             length(which(simi_vec>=60 & simi_vec<80)),
             length(which(simi_vec<60)))
    
    levls <- factor(c("High(>80)", "Medium(60-80)", "Low(<60)"), levels = c("High(>80)", "Medium(60-80)", "Low(<60)"))
    
    df <- data.frame(levls = levls,
                     Number = num)
    
    cbPalette <- c("#009E73", "#E69F00", "#999999")
    require(ggplot2)
    p1 <- ggplot(data=df, aes(x=levls, y=Number)) +
      geom_bar(stat="identity", aes(fill=levls), width = 0.618)+
      geom_text(aes(label=Number), vjust=1.6, color="white", size=6)+
      theme_minimal() + scale_fill_manual(values=cbPalette) + 
      scale_color_manual(values=cbPalette) + 
      guides(fill=guide_legend(title="Confidence Level"))+
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),  
            axis.title.y = element_text(size = 14),
            axis.title.x = element_blank())
    
  }
  
  imageNM <- paste0(imageNM, "_", dpi, ".", format)
  
  Cairo::Cairo(
    file = imageNM, 
    unit = "in", dpi = dpi, width = width, height = height, type = format, bg = "white")
  print(p1)
  dev.off()
  mSetObj[["imgSet"]]$ms2sumsingle <- imageNM
  
  return(.set.mSet(mSetObj))
}


#' setMS2DBOpt
#'
#' @param mSetObj mSetObj object
#' @param DBoption database option to define neutral loss or not, can be either 'regualr" or 'nl'.
#' @export
#' @author Zhiqiang Pang 
#' 
setMS2DBOpt <- function(mSetObj=NA, DBoption = "regular") {
  mSetObj <- .get.mSet(mSetObj);
  if(DBoption!="regular" & DBoption!="nl"){
    stop("Wrong MS2DBopt. Must be either regular or nl");
  }
  mSetObj[["dataSet"]]$MSMS_db_option <- DBoption;
  return(.set.mSet(mSetObj))
}

generateMS2dbOpt <- function(database = "all", ionMode = "positive"){
  prefix = ""
  database_opts = ""
  if(length(database)>1){
    prefix = "mcst_\t"; # multiple customized
  }
  for(i in database){
    if(i == "all"){ 
      database_opt <- "all";
      return("all")
    } else if(i == "hmdb_exp") {
      if(ionMode == "positive"){
        database_opt <- "HMDB_experimental_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "HMDB_experimental_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "hmdb_pre"){
      if(ionMode == "positive"){
        database_opt <- "HMDB_predicted_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "HMDB_predicted_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "gnps"){
      if(ionMode == "positive"){
        database_opt <- "GNPS_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "GNPS_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "mines"){
      if(ionMode == "positive"){
        database_opt <- "MINEs_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "MINEs_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "lipidblast"){
      if(ionMode == "positive"){
        database_opt <- "LipidBlast_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "LipidBlast_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "mona"){
      if(ionMode == "positive"){
        database_opt <- "MoNA_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "MoNA_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "massbank"){
      if(ionMode == "positive"){
        database_opt <- "MassBank_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "MassBank_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "riken"){
      if(ionMode == "positive"){
        database_opt <- "RIKEN_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "RIKEN_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "respect"){
      if(ionMode == "positive"){
        database_opt <- "ReSpect_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "ReSpect_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "msdial"){
      if(ionMode == "positive"){
        database_opt <- "MSDIAL_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "MSDIAL_NegDB";
      } else {
        database_opt <- "all";
      }
    } else if(i == "bmdms"){
      if(ionMode == "positive"){
        database_opt <- "BMDMS_PosDB";
      } else if(ionMode == "negative") {
        database_opt <- "BMDMS_PosDB";
      } else {
        database_opt <- "all";
      }
    }
    database_opts <- paste0(database_opts, database_opt, "\t")
  }
  database_str <- paste0(prefix, database_opts)
  return(database_str)
}