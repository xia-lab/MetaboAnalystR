## Google drive file preparation utils functions

ValidateGoogleDriveURL <- function(URL){
  if(!require(googledrive)) install.packages("googledrive")
  library(googledrive)
  # URL <- "https://drive.google.com/drive/folders/1DaoCaR0x3WWhGn9d4M8JxUvEcG88iGAv?usp=drive_link" # sub dirs
  # URL <- "https://drive.google.com/drive/folders/1fftjj4N_bN-utlP340CDLY2vKa1l1Cji?usp=drive_link" # mixed
  # URL <- "https://drive.google.com/drive/folders/1E7P9d3MtWt8TdN0mAqlIIdwGaZCRAmax?usp=drive_link" # no dirs-> big files
  # URL <- "https://drive.google.com/drive/folders/1kqvUQjPEiCEkGF297wlj2Tzeqmpg58wu?usp=drive_link" # no folders
  # URL <- "https://drive.google.com/drive/folders/1ipk6EptXdnvEYXQhCorsNbyW257ypmzR?usp=drive_link" # CDF EXAMPLE
  # 
  jp_folder <- gsub("\\?usp=drive_link|\\??usp=sharing", "", URL)
  
  drive_auth(email = "support@xialab.ca")
  
  #get the id folder
  folder_id <- try(drive_get(as_id(jp_folder)), silent = TRUE)
  if(class(folder_id)[1]=="try-error"){
    return(0)
  } else {
    return(1)
  }
}

ExtractGoogleDriveURL <- function(URL){
  
  # return results:
  # 0. No file detected
  # 1: good, can be processed;
  # 2. bad, no metadata.txt found;
  # 3. bad, no spectra files found;
  # 4. bad, metadata and spectra are not matched => to determine in the next step
  # 5. bad, files are matched, but exceed the max limitation (20GB). => to determine in the next step
  # 6. bad, multiple formats.
  
  library(googledrive)
  # URL <- "https://drive.google.com/drive/folders/1DaoCaR0x3WWhGn9d4M8JxUvEcG88iGAv?usp=drive_link" # sub dirs
  # URL <- "https://drive.google.com/drive/folders/1fftjj4N_bN-utlP340CDLY2vKa1l1Cji?usp=drive_link" # mixed
  # URL <- "https://drive.google.com/drive/folders/1E7P9d3MtWt8TdN0mAqlIIdwGaZCRAmax?usp=drive_link" # no dirs-> big files
  # URL <- "https://drive.google.com/drive/folders/1kqvUQjPEiCEkGF297wlj2Tzeqmpg58wu?usp=drive_link" # no folders
  # URL <- "https://drive.google.com/drive/folders/1ipk6EptXdnvEYXQhCorsNbyW257ypmzR?usp=drive_link" # CDF EXAMPLE
  # 
  jp_folder <- gsub("\\?usp=drive_link|\\??usp=sharing", "", URL)
  
  drive_auth(email = "support@xialab.ca")
  folder_id <- drive_get(as_id(jp_folder))
  
  #find files in folder
  files <- drive_ls(folder_id)
  
  files_types <- vapply(1:nrow(files), FUN = function(x){
    files[[3]][[x]][["mimeType"]]
  }, FUN.VALUE = character(1L))
  
  if(nrow(files) == 0){
    return(0)
  }
  
  # detect any text files directly in the 1-level folder
  metadata_idx <- NA;
  files_txt <- NULL;
  files_types_txt_idx <- which(files_types == "text/plain")
  if(length(files_types_txt_idx)>0){
    text_names <- vapply(files_types_txt_idx, function(x){
      files[[3]][[x]][["name"]]
    }, FUN.VALUE = character(1L));
    
    files_txt <- files[files_types_txt_idx, ]
    files <- files[-files_types_txt_idx, ]
    files_types <- files_types[-files_types_txt_idx]
    
    if(any("metadata.txt" == text_names)){
      metadata_idx <- which("metadata.txt" == text_names)
    }
    
  }
  
  
  if (all(("text/xml" == files_types))){
    # case 1: found xml files directly without any sub-folder
    # all files in mzML/mzXML/mzData 
    res_sf <- DectectWhetherSingleFormat(files)
    if(!res_sf){
      return(6)
    }
    all_spec_files <- DetectAllSpectraFiles(files);
  } else if(all("application/x-netcdf" == files_types)){
    # case 1: found cdf files directly without any sub-folder
    # all files in cdf format
    res_sf <- DectectWhetherSingleFormat(files)
    if(!res_sf){
      return(6)
    }
    all_spec_files <- DetectAllSpectraFiles(files);
  } else if(all(files_types =="application/vnd.google-apps.folder") & !is.null(files_txt)){
    # multi-subfolders, metadata outside and has been detected
    file_lists <- lapply(1:nrow(files), function(x){
      drive_ls(files$id[x])})
    file_lists2 <- do.call(rbind, file_lists)
    files_types_sub <- vapply(1:nrow(file_lists2), FUN = function(x){
      file_lists2[[3]][[x]][["mimeType"]]
    }, FUN.VALUE = character(1L))
    idx_specs <- which(((files_types_sub == "text/xml") | (files_types_sub == "application/x-netcdf")))
    files <- file_lists2[idx_specs, ]

    # all files in single format
    res_sf <- DectectWhetherSingleFormat(files)
    if(!res_sf){
      return(6)
    }
    all_spec_files <- DetectAllSpectraFiles(files);

  } else if(all(files_types =="application/vnd.google-apps.folder") & is.null(files_txt)){
    # multi-subfolders, metadata outside and has not been detected
    file_lists <- lapply(1:nrow(files), function(x){
      drive_ls(files$id[x])})
    file_lists2 <- do.call(rbind, file_lists)
    files_types_sub <- vapply(1:nrow(file_lists2), FUN = function(x){
      file_lists2[[3]][[x]][["mimeType"]]
    }, FUN.VALUE = character(1L))
    idx_specs <- which(((files_types_sub == "text/xml") | (files_types_sub == "application/x-netcdf") | (files_types_sub == "text/plain")))
    files <- file_lists2[idx_specs, ]
    
    if(is.null(files_txt)){
      files_types_sub <- vapply(1:nrow(files), FUN = function(x){
        files[[3]][[x]][["mimeType"]]
      }, FUN.VALUE = character(1L))
      
      files_types_txt_idx <- which(files_types_sub == "text/plain")
      if(length(files_types_txt_idx)>0){
        text_names <- vapply(files_types_txt_idx, function(x){
          files[[3]][[x]][["name"]]
        }, FUN.VALUE = character(1L));
        
        files_txt <- files[files_types_txt_idx, ]
        files <- files[-files_types_txt_idx,]
        
        if(any("metadata.txt" == text_names)){
          metadata_idx <- which("metadata.txt" == text_names)
        }
        
      }
    }
    
    # all files in single format
    res_sf <- DectectWhetherSingleFormat(files)
    if(!res_sf){
      return(6)
    }
    all_spec_files <- DetectAllSpectraFiles(files);
    
  } else if(("application/vnd.google-apps.folder" %in% files_types) & 
            (("text/xml" %in% files_types) | ("text/plain" %in% files_types) | ("application/x-netcdf" %in% files_types))){
    # mixed
    idx_1 <- which((files_types == "text/xml") | (files_types == "text/plain") | (files_types == "application/x-netcdf"))
    file_list0 <- files[idx_1,]
    
    idx_folders <- which(files_types == "application/vnd.google-apps.folder")
    file_lists1 <- lapply(idx_folders, function(x){
      drive_ls(files$id[x])})
    
    file_lists2 <- do.call(rbind, file_lists1)
    files <- rbind(file_list0, file_lists2)
    
    if(is.null(files_txt)){
      files_types_sub <- vapply(1:nrow(files), FUN = function(x){
        files[[3]][[x]][["mimeType"]]
      }, FUN.VALUE = character(1L))
      files_types_txt_idx <- which(files_types_sub == "text/plain")
      if(length(files_types_txt_idx)>0){
        text_names <- vapply(files_types_txt_idx, function(x){
          files[[3]][[x]][["name"]]
        }, FUN.VALUE = character(1L));
        
        files_txt <- files[files_types_txt_idx, ]
        files <- files[-files_types_txt_idx,]
        if(any("metadata.txt" == text_names)){
          metadata_idx <- which("metadata.txt" == text_names)
        }
        
      }
    }
    
    # all files in single format
    res_sf <- DectectWhetherSingleFormat(files)
    if(!res_sf){
      return(6)
    }
    all_spec_files <- DetectAllSpectraFiles(files);

  } else {
    # no gzip file detected/included
    return(0)
  }
  if(is.null(files_txt)){
    return(2)
  }
  qs::qsave(all_spec_files, file = "all_raw_spec_files_list.qs")
  qs::qsave(files_txt, file = "metadata_file_src.qs")
  
  return(1)
}

DectectWhetherSingleFormat <- function(files){
  file_names <- vapply(1:nrow(files), function(x){
    files[[3]][[x]][["name"]]
  }, FUN.VALUE = character(1L))
  
  formats<- c("mzML", "mzXML", "cdf", "CDF", "mzData")
  file_exts <- tools::file_ext(file_names)
  
  if(length(unique(file_exts))>1){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

DetectAllSpectraFiles <- function(files){
  
  file_names <- vapply(1:nrow(files), function(x){
    files[[3]][[x]][["name"]]
  }, FUN.VALUE = character(1L))
  
  formats<- c("mzML", "mzXML", "cdf", "CDF", "mzData")
  file_exts <- tools::file_ext(file_names)
  
  idxs <- vapply(file_exts, function(x){
    x %in% formats
  }, FUN.VALUE = logical(1L))
  files_detected <- files[idxs,]
  return(files_detected)
}

CheckMetadataMatching <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  
  write.table("", file = "isGoogleDriveUpload")
  library(googledrive)
  drive_auth(email = "support@xialab.ca")
  drive_user()
  
  meta_data_txt <- qs::qread("metadata_file_src.qs")
  all_spec_files <- qs::qread("all_raw_spec_files_list.qs")
  
  drive_download(meta_data_txt, overwrite = TRUE, path = "metadata.txt")
  
  meta_dt <- read.table("metadata.txt", header = T)
  all_files_nms <- all_spec_files$name
  colnames(meta_dt) <- c("Samples","Groups")
  
  mSetObj$dataSet$meta_dt <- meta_dt
  mSetObj$dataSet$all_files_nms <- all_files_nms
  mSetObj$dataSet$all_spec_files <- all_spec_files
  
  if(all(meta_dt$Samples %in% all_files_nms)){
    return(.set.mSet(mSetObj));
  } else {
    missing_files <- meta_dt$Samples[!(meta_dt$Samples %in% all_files_nms)]
    mSetObj$dataSet$missing_files <- missing_files;
    .set.mSet(mSetObj)
    return(0)
  }
}

GetMissingFiles <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$missing_files -> missing_files;
  return(paste0("These files are missing: ", paste(missing_files, collapse = ", ")))
}

GetFileTotalSizeBool <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$all_spec_files -> all_spec_files;
  
  file_sizes <- vapply(1:nrow(all_spec_files), function(x){
    as.double(all_spec_files[[3]][[x]][["size"]])/(1024^2)
  }, FUN.VALUE = double(1L))
  if(sum(file_sizes) < 21*1024){
    return(1)
  } else {
    return(0)
  }
}

GetAllSpectraFiles <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$all_files_nms -> all_files_nms
  return(all_files_nms)
}

GetAllSpectraGroups <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$meta_dt -> meta_dt
  return(unique(meta_dt$Groups))
}

GetSampleNMsofGroup <- function(mSetObj = NA, group_nm){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$meta_dt -> meta_dt
  return(meta_dt$Samples[meta_dt$Groups == group_nm])
}
GetFileSizesofSpectra <- function(mSetObj = NA, spectra_nm){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$all_spec_files -> all_spec_files
  
  this_spec_file <- all_spec_files$drive_resource[all_spec_files$name == spectra_nm]
  file_size <- as.double(this_spec_file[[1]][["size"]])
  if(!is.double(file_size)){
    return(0)
  } else {
    return(signif(file_size/1024/1024, 3))
  }
}

DownloadAnMS2File <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$all_spec_files -> all_spec_files
  mSetObj$dataSet$meta_dt -> meta_dt
  
  ms2spec_file <- meta_dt$Samples[meta_dt$Groups == "MS2"]
  sizes <- vapply(ms2spec_file, FUN = function(x){
    idx <- which(x==all_spec_files$name)
    all_spec_files[[3]][[idx]][["size"]]
  }, character(1L))
  idx_min <- which.min(as.double(sizes))
  id2download <- all_spec_files$id[idx_min]
  
  library(googledrive)
  drive_auth(email = "support@xialab.ca")
  drive_user()
  
  if(!dir.exists(paste0("upload/"))){
    dir.create(paste0("upload/"))
  }
  if(!dir.exists(paste0("upload/MS2"))){
    dir.create(paste0("upload/MS2"))
  }
  
  public_file <- drive_get(id2download)
  drive_download(public_file, overwrite = TRUE, path = paste0("upload/MS2/", ms2spec_file[idx_min]))
  
}
