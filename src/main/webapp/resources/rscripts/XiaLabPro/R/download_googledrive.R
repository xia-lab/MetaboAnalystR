#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

DownloadData <- function(path){
  setwd(path)
  data_frame_allRawSeq <- qs::qread(file = "all_raw_spec_files_list.qs")
  allRawSeq_meta  <- read.table("metadata.txt", header = TRUE)
  message("Starting downloading raw spectra data...")
  if(file.exists("IncludedSpectra.qs")){
    included_spec <- qs::qread("IncludedSpectra.qs")
    inc_files <- included_spec[[1]]
    inc_files_all <- str2lang(inc_files)
    inc_files_all <- eval(inc_files_all)
  } else {
    inc_files_all <- NULL;
  }
  
  if(!is.null(inc_files_all)){
    # remove all non-included files
    meta_idx <- allRawSeq_meta[,1] %in% inc_files_all
    allRawSeq_meta <- allRawSeq_meta[meta_idx, ]
    rownames(allRawSeq_meta) <- NULL
  }
  
  groups <- unique(allRawSeq_meta[,2])
  colnames(allRawSeq_meta) <- c("Samples", "Groups")
  
  library(googledrive)
  sink(file = paste0(path, "/log_progress.txt"))
  cat(0)
  sink()
  if(!dir.exists(paste0(path, "/upload/"))){
    dir.create(paste0(path, "/upload/"))
  }
  for(grp in groups){
    if(!dir.exists(paste0(path, "/upload/",grp))){
      dir.create(paste0(path, "/upload/", grp))
    }
  }
  #drive_deauth()
  drive_auth(email = "support@xialab.ca")
  drive_user()
  
  all_files <- c(data_frame_allRawSeq$name)
  all_files <- all_files[!is.na(all_files)]
  
  ids <- vapply(all_files, function(x){
    data_frame_allRawSeq$id[data_frame_allRawSeq$name == x]
  }, character(1L))

  for(i in 1:length(ids)){
    
    this_grp <- allRawSeq_meta$Groups[names(ids)[i] == allRawSeq_meta$Samples]
    if(length(this_grp)==0){
      next;
    }
    
    public_file <- drive_get(as_id(ids[i]))
    drive_download(public_file, overwrite = TRUE, path = paste0(path, "/upload/", this_grp, "/", names(ids)[i]))
    
    sink(file = paste0(path, "/log_progress.txt"))
    cat(signif(i/length(ids)*3, digits = 3))
    sink()
    
    sink(file = paste0(path, "/log_progress2.txt"))
    cat(signif(i/length(ids)*6, digits = 3))
    sink()
  }
  
  alg_ms1 <- readLines("ms1_algorithm.txt")
  if(alg_ms1 == "asari"){
    require(R.utils)
    allMSFiles <- list.files("upload", full.names = T, recursive = T)
    
    for(i in allMSFiles){
      if(!grepl("^upload/MS2", i)){
        createLink(link = paste0(path, "/uploadAll/",basename(i)), target = i)
      }
    }
  }
}

DownloadData(args[1])