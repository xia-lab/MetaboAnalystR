## Interfacing with other tools: MZmine / MSDIAL / Asari / MS-FINDER / SIRIUS

#' PerformMS1ResultsFormatting
#' This function is used to format the results from other tools into the generic format of MetaboAnalystR
#' Currently,we are supporting the compatibility for four commonly used open-source tools: MS-DIAL, MZmine, Asari and XCMS online
#' The first parameter file_path should be a valid file of the result.
#' User need to specify the type in the 2nd argument, type. This argument can be msdial, mzmine, asari and xcms
#' Please note, if your original data does not contain meta information, you need to manually add them in the generated "metaboanalyst_input.csv" file
#' The formatted file is 'sample in columns'.
#' @param file_path 
#' @param type 
#' @param meta_data this is path to a table containing two columns, the first column is the sample names and second column is the group information
#' @export
PerformMS1ResultsFormatting <- function(file_path, type, meta_data = NA){
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/c18neg_ms1.csv"
  # meta_data <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/c18neg_meta_ms1.txt"
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/Height_0_20231111553.txt"
  # meta_data <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/Height_0_20231111553_meta.txt"
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/preferred_Feature_table.tsv"
  # meta_data <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/preferred_Feature_table_meta.txt"
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/XCMS.annotated.Report_1404649.tsv"
  # meta_data <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/XCMS.annotated.Report_1404649_meta.txt"
  # type can be mzmine/asari/msdial/xcms
  if(!file.exists(file_path)){
    stop("The file_path is not valid! Please provide a valid file path.")
  }
  if(!(type %in% c("mzmine", "msdial", "asari", "xcms"))){
    stop("The data type is not valid! Data type can be one of these: mzmine, msdial, asari and xcms.")
  }
  if(!file.exists(meta_data)){
    stop("The file path of meta data table is invalid! Please provide a valid file path!")
  }
  
  if(type == "msdial"){
    feadt <- read.csv(file_path, sep = "\t")
    if(!is.na(meta_data)){
      dt_ms1_meta <- read.table(meta_data, header = T)
    }
    dt_ms1 <- feadt[-c(1:3),-c(1,4,5:32, ncol(feadt), ncol(feadt)-1)]
    colnames(dt_ms1) <- dt_ms1[1,]
    dt_ms1 <- dt_ms1[-1,]
    colnm_idx_mz <- which((colnames(dt_ms1) == "Average Mz"))
    colnm_idx_rt <- which((colnames(dt_ms1) == "Average Rt(min)"))
    feats <- vapply(1:nrow(dt_ms1), function(x){
      paste0(dt_ms1[x, colnm_idx_mz], "__", dt_ms1[x, colnm_idx_rt])
    }, FUN.VALUE = character(1L))
    dt_ms1x <- cbind(features = feats, dt_ms1[,-c(colnm_idx_mz, colnm_idx_rt)])
    
    if(!is.na(meta_data)){
      grps <- vapply(colnames(dt_ms1x)[-1], FUN = function(x){
        dt_ms1_meta[which(dt_ms1_meta[,1]==x),2]
      }, FUN.VALUE = character(length = 1L))
      grps <- c("Groups", grps)
    } else {
      grps <- NA
    }
    
    dt_ms1_done <- rbind(Group = grps, dt_ms1x)
    dt_ms1_done[1,1] <- "Groups";
    write.csv(dt_ms1_done, file = "metaboanalyst_input.csv", row.names = F)
    message("Data formatting is done!")
    print(paste0("The formatted data has been saved here: ", getwd(), "/metaboanalyst_input.csv"))
    if(is.na(meta_data)){
      message("The data from MS-DIAL does not contains meta/grouping information, you need to manually add it into the first row!")
    }
  }
  
  if(type == "mzmine"){
    dt_ms1 <- read.csv(file_path)
    if(!is.na(meta_data)){
      dt_ms1_meta <- read.table(meta_data, header = T)
    }
    colnm_idx <- which(grepl("*.area", colnames(dt_ms1)))
    colnm_idx_mz <- which((colnames(dt_ms1) == "mz"))
    colnm_idx_rt <- which((colnames(dt_ms1) == "rt"))
    feats <- vapply(1:nrow(dt_ms1), function(x){
      paste0(dt_ms1[x, colnm_idx_mz], "__", dt_ms1[x, colnm_idx_rt])
    }, FUN.VALUE = character(1L))
    dt_ms1x <- cbind(features = feats, dt_ms1[,colnm_idx])
    
    if(!is.na(meta_data)){
      grps <- vapply(colnames(dt_ms1[,colnm_idx]), FUN = function(x){
        dt_ms1_meta[which(dt_ms1_meta[,1]==x),2]
      }, FUN.VALUE = character(length = 1L))
      grps <- c("Groups", grps)
    } else {
      grps <- NA
    }
    
    dt_ms1_done <- rbind(Group = grps, dt_ms1x)
    dt_ms1_done[1,1] <- "Groups";
    
    write.csv(dt_ms1_done, file = "metaboanalyst_input.csv", row.names = F)
    message("Data formatting is done!")
    print(paste0("The formatted data has been saved here: ", getwd(), "/metaboanalyst_input.csv"))
    if(is.na(meta_data)){
      message("The data from mzMine does not contains meta/grouping information, you need to manually add it into the first row!")
    }
  }
  
  if(type == "asari"){
    ftable <- read.csv(file_path, sep = "\t", check.names=FALSE)
    if(!is.na(meta_data)){
      dt_ms1_meta <- read.table(meta_data, header = T)
    }
    features <- paste0(ftable$mz, "__", ftable$rtime)
    ftable1 <- ftable[,c(12:ncol(ftable))]
    allSamples <- colnames(ftable1)
    
    if(!is.na(meta_data)){
      grps <- vapply(allSamples, FUN = function(x){
        dt_ms1_meta[which(dt_ms1_meta[,1]==x),2]
      }, FUN.VALUE = character(length = 1L))
      grps <- c("Groups", grps)
    } else {
      grps <- NA
    }
    
    ftable2 <- grps
    ftable3 <- data.frame(Samples = c("Groups", features))
    ftable0 <- rbind(ftable2, ftable1)
    ftable0 <- cbind(ftable3, ftable0)
    rownames(ftable0) <- NULL;
    
    write.csv(ftable0, file = "metaboanalyst_input.csv", row.names = F, quote = F)
    message("Data formatting is done!")
    print(paste0("The formatted data has been saved here: ", getwd(), "/metaboanalyst_input.csv"))
    if(is.na(meta_data)){
      message("The data from Asari does not contains meta/grouping information, you need to manually add it into the first row!")
    }
  }
  
  if(type == "xcms"){
    dt_ms1 <- read.csv(file_path, sep = "\t", check.names=FALSE)
    if(!is.na(meta_data)){
      dt_ms1_meta <- read.table(meta_data, header = T)
    }
    colnm_idx <- -c(1:11, ncol(dt_ms1), ncol(dt_ms1)-1, ncol(dt_ms1)-2)
    colnm_idx_mz <- which((colnames(dt_ms1) == "mzmed"))
    colnm_idx_rt <- which((colnames(dt_ms1) == "rtmed"))
    feats <- vapply(1:nrow(dt_ms1), function(x){
      paste0(round(dt_ms1[x, colnm_idx_mz],4), "__", round(dt_ms1[x, colnm_idx_rt], 2))
    }, FUN.VALUE = character(1L))
    dt_ms1x <- cbind(features = feats, dt_ms1[,colnm_idx])
    
    if(!is.na(meta_data)){
      grps <- vapply(colnames(dt_ms1x)[-1], FUN = function(x){
        dt_ms1_meta[which(dt_ms1_meta[,1]==x),2]
      }, FUN.VALUE = character(length = 1L))
      grps <- c("Groups", grps)
    } else {
      grps <- NA
    }
    
    dt_ms1_done <- rbind(Group = grps, dt_ms1x)
    dt_ms1_done[1,1] <- "Groups";
    write.csv(dt_ms1_done, file = "metaboanalyst_input.csv", row.names = F)
    message("Data formatting is done!")
    print(paste0("The formatted data has been saved here: ", getwd(), "/metaboanalyst_input.csv"))
    if(is.na(meta_data)){
      message("The data from mzMine does not contains meta/grouping information, you need to manually add it into the first row!")
    }
  }
}


#' PerformMS2ResultsFormatting
#' This function is used to format the results from other tools into the generic format of MetaboAnalystR for functional analysis
#' Currently,we are supporting the compatibility for four commonly used open-source tools: MS-FINDER, and SIRIUS
#' The first parameter file_path should be a valid file of the result.
#' User need to specify the type in the 2nd argument, type. This argument can be msfinder, or sirius
#' The 3rd argument is the MS1 peak list
#' 
#' @param file_path file path of NS2 file
#' @param type type, can be msfinder or sirius
#' @param MS1_features_list this is feature list used functional analysis 
#' @export
PerformMS2ResultsFormatting <- function(file_path, type, MS1_features_list = NA){
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/Structure_result_2072.txt"
  # MS1_features_list <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/peaks_ms1_msdial.txt"
  # file_path <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/compound_identifications_sirius.tsv"
  # MS1_features_list <- "/home/qiang/Downloads/MetaboAnalystR_2.0.0/MetaboAnalystR/vignettes/peaks_ms1_mzmine.txt"
  # type can be msfinder/sirius
  if(!file.exists(file_path)){
    stop("The file_path is not valid! Please provide a valid file path.")
  }
  if(!file.exists(MS1_features_list)){
    stop("The MS1_features_list is not valid! Please provide a valid file path.")
  }
  if(!(type %in% c("msfinder", "sirius"))){
    stop("The data type is not valid! Data type can be one of these: msfinder, sirius.")
  }
  require("progress")
  if(type == "msfinder"){
    dt_ms1 <- read.csv(MS1_features_list)
    dt_ms2 <- read.csv(file_path, sep = "\t")
    df_ms2 <- as.data.frame(matrix(nrow = nrow(dt_ms1), ncol=5))
    
    rt <- as.numeric(vapply(dt_ms2$File.name, function(y) {as.double(strsplit(y, "_")[[1]][2])}, double(1L)))*60
    pb <- progress_bar$new(
      format = "(:spin) [:bar] :percent",
      total = nrow(df_ms2), clear = FALSE, width = 60)
    for(i in 1:nrow(df_ms2)){
      pb$tick()
      k <- 0;
      for(j in 1:nrow(dt_ms2)){
        if((abs(dt_ms2$Precursor.mz[j] - dt_ms1$mz[i]) < 0.005) & 
           (abs(rt[j] - dt_ms1$rt[i]) < 25)){
          k <- k+ 1;
          df_ms2[i,k] <- dt_ms2$InChIKey[j]
          if(k>4){
            break;
          }
        }
      }
    }
    df_ms2_new <- t(apply(df_ms2, 1, function(x){
      vec <- rep(NA, 5)
      y <- unique(x[!is.na(x)])
      if(length(y)>0) vec[1:length(y)] <- y
      vec
    }))
    
    write.table(df_ms2_new, file = "metaboanalyst_msms_input.txt", row.names = F, sep = ",")
  }
  
  if(type == "sirius"){
    dt_ms1 <- read.csv(MS1_features_list)
    dt_ms2 <- read.csv(file_path, sep = "\t")
    ms2_df <- as.data.frame(cbind(dt_ms2$ionMass, dt_ms2$retentionTimeInSeconds, paste0(dt_ms2$InChIkey2D, "-UHFFFAOYSA-O")))
    colnames(ms2_df) <- c("mz", "rt", "inchikey")
    df_ms2 <- as.data.frame(matrix(nrow = nrow(dt_ms1), ncol=5))
    pb <- progress_bar$new(
      format = "(:spin) [:bar] :percent",
      total = nrow(df_ms2), clear = FALSE, width = 60)
    for(i in 1:nrow(df_ms2)){
      pb$tick()
      k <- 0;
      for(j in 1:nrow(dt_ms2)){
        if((abs(as.numeric(ms2_df$mz[j]) - dt_ms1$mz[i]) < 0.005) & 
           (abs(dt_ms1$rt[j] - as.numeric(dt_ms1$rt[i])) < 25)){
          k <- k+ 1;
          df_ms2[i,k] <- ms2_df$inchikey[j]
          if(k>4){
            break;
          }
        }
      }
    }
    df_ms2_new <- t(apply(df_ms2, 1, function(x){
      vec <- rep(NA, 5)
      y <- unique(x[!is.na(x)])
      if(length(y)>0) vec[1:length(y)] <- y
      vec
    }))
    
    write.table(df_ms2_new, file = "metaboanalyst_msms_input.txt", row.names = F, sep = ",")
    
  }
  
  message("Data formatting is done!")
  print(paste0("The formatted MS2 data has been saved here: ", getwd(), "/metaboanalyst_msms_input.txt"))
}