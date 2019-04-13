#' Import raw MS data
#' @description This function handles the reading in of 
#' raw MS data (.mzML, .CDF and .mzXML). Users must set 
#' their working directory to the folder containing their raw 
#' data, divided into two subfolders named their desired group labels. The  
#' function will output two chromatograms into the user's working directory, a 
#' base peak intensity chromatogram (BPIC) and a total ion 
#' chromatogram (TIC). Further, this function sets the number of cores
#' to be used for parallel processing. It first determines the number of cores 
#' within a user's computer and then sets it that number/2.  
#' @param foldername Character, input the file path to the folder containing
#' the raw MS spectra to be processed.
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param par.cores Logical, if true, the function will automatically 
#' set the number of parallel cores. If false, it will not.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import xcms
#' @import MSnbase
#' @import BiocParallel
#' @import parallel

ImportRawMSData <- function(foldername, format = "png", dpi = 72, width = 9, par.cores=TRUE){

  msg.vec <<- vector(mode="character")
  
  msg <- c("The uploaded files are raw MS spectra.");
  
  # the "upload" folder should contain two subfolders (groups, i.e. Healthy vs. Disease)
  # each subfolder must contain samples (.mzML/.CDF/.mzXML files)
  
  files <- dir(foldername, pattern=".mzML|.CDF|.mzXML", recursive=T, full.name=TRUE)
  
  if (length(files) == 0) {
    AddErrMsg("No spectra were found!");
    return(0);
  }
  
  snames <- gsub("\\.[^.]*$", "", basename(files));
  msg<-c(msg, paste("A total of ", length(files), "samples were found."));
  
  sclass <- gsub("^\\.$", "sample", dirname(files));
  
  scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "");
  scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp));
  i <- 1
  while(all(scomp[i,1] == scomp[i,-1]) && i < nrow(scomp)){
    i <- i + 1;
  }
  i <- min(i, tail(c(0, which(scomp[1:i,1] == .Platform$file.sep)), n = 1) + 1)
  if (i > 1 && i <= nrow(scomp)){
    sclass <- substr(sclass, i, max(nchar(sclass)))
  }
  
  # some sanity check before proceeds
  sclass <- as.factor(sclass);
  if(length(levels(sclass))<2){
    AddErrMsg("You must provide classes labels (at least two classes)!");
    return(0);
  }
  
  SetClass(sclass)
  
  # check for unique sample names
  if(length(unique(snames))!=length(snames)){
    AddErrMsg("Duplicate sample names are not allowed!");
    dup.nm <- paste(snames[duplicated(snames)], collapse=" ");
    AddErrMsg("Duplicate sample names are not allowed!");
    AddErrMsg(dup.nm);
    return(0);
  }
  
  pd <- data.frame(sample_name = snames,
                   sample_group = sclass,
                   stringsAsFactors = FALSE)
  
  if(!.on.public.web & par.cores==TRUE){
    cores <- parallel::detectCores()
    num_cores <- ceiling(cores/2) 
    print(paste0("The number of CPU cores to be used is set to ", num_cores, "."))
    
    if (.Platform$OS.type == "unix") {
      BiocParallel::register(BiocParallel::bpstart(BiocParallel::MulticoreParam(num_cores)))
      } else { # for windows
      BiocParallel::register(BiocParallel::bpstart(BiocParallel::SnowParam(num_cores)))
      }
  }
  
  raw_data <- suppressMessages(readMSData(files = files, pdata = new("NAnnotatedDataFrame", pd),
                                          mode = "onDisk")) 
  
  # Plotting functions to see entire chromatogram
  bpis <- chromatogram(raw_data, aggregationFun = "max")
  tics <- chromatogram(raw_data, aggregationFun = "sum")
  
  groupNum <- nlevels(groupInfo)
  
  if(groupNum > 9){
    col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
    group_colors <- col.fun(groupNum)
  }else{
    group_colors <- paste0(RColorBrewer::brewer.pal(3, "Set1")[1:groupNum], "60")
  }

  names(group_colors) <- levels(groupInfo)
  
  bpis_name <- paste("BPIS_", dpi, ".", format, sep="");
  tics_name <- paste("TICS_", dpi, ".", format, sep="");
  
  Cairo::Cairo(file = bpis_name, unit="in", dpi=dpi, width=width, height= width*5/9, 
               type=format, bg="white");
  plot(bpis, col = group_colors[raw_data$sample_group])
  legend("topright", legend=levels(groupInfo), pch=15, col=group_colors);
  dev.off();
  
  Cairo::Cairo(file = tics_name, unit="in", dpi=dpi, width=width, height=width*5/9, 
               type=format, bg="white");
  plot(tics, col = group_colors[raw_data$sample_group])
  legend("topright", legend=levels(groupInfo), pch=15, col=group_colors);
  dev.off();
  
  print("Successfully imported raw MS data!")
  
  return(raw_data)
}

#' Set class information for MS data
#' @description This function sets the class information
#' for preprocessing MS data.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

SetClass <- function(class){
  
  groupInfo <<- class
  
}

#' Plot EIC
#' @description This functionn creates an extracted ion chromatogram (EIC) for a specific
#' m/z and retention time. This is used for quality-control of raw m/s data.
#' @param raw_data The object created using the InspectRawMSData function,
#' containing the raw MS data.
#' @param rt_mn Numeric, specify the minimum bound of the retention time range.
#' @param rt_mx Numeric, specify the maximum bound of the retention time range.
#' @param mz_mn Numeric, specify the minimum bound of the m/z range.
#' @param mz_mx Numeric, specify the maximum bound of the m/z range.
#' @param aggreg Character, if "sum", creates a total ion chromatogram. 
#' If "max", creates a base peak chromatogram. By default it is set 
#' to "sum". 
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create. 
#' @export

PlotEIC <- function(raw_data, rt_mn, rt_mx, mz_mn, mz_mx, aggreg = "sum",
                    format = "png", dpi = 72, width = 9){
  
  filt_data <- filterRt(raw_data, rt = c(rt_mn, rt_mx))
  filt_mz <- filterMz(filt_data, mz = c(mz_mn, mz_mx))
  mz_slice <- chromatogram(filt_mz, aggregationFun = aggreg)
  
  groupNum <- nlevels(groupInfo)
  
  if(groupNum > 9){
    col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
    group_colors <- col.fun(groupNum)
  }else{
    group_colors <- paste0(RColorBrewer::brewer.pal(3, "Set1")[1:groupNum], "60")
  }
  
  names(group_colors) <- unique(raw_data$sample_group)
  
  eic_name <- paste("EIC_", dpi, ".", format, sep="");
  
  Cairo::Cairo(file = eic_name, unit="in", dpi=dpi, width=width, height= width*5/9, 
               type=format, bg="white");
  plot(mz_slice, col = group_colors[raw_data$sample_group])
  legend("topright", legend=unique(raw_data$sample_group), pch=15, col=group_colors);
  dev.off();
  
  print("EIC created!") 
  return(1)
}

#' Set parameters for peak picking using XCMS and CAMERA
#' @description This function sets all the parameters used for downstream
#' pre-processing of user's raw MS data. 
#' @param alg Character, specify the algorithm to perform peak detection. "centwave" 
#' to use the CentWave algorithm, and "match_filt" to use the MatchedFilter algorithm.
#' @param ppm Numeric, specify the mass error in ppm.
#' @param min_pkw Numeric, specify the minimum peak width in seconds.
#' @param max_pkw Numeric, specify the maximum peak width in seconds. 
#' @param sn_thresh Numeric, specify the signal to noise threshold.
#' @param mzdiff Numeric, specify the minimum m/z difference for signals to be considered as 
#' different features when retention times are overlapping. 
#' @param bw Numeric, specify the band width (sd or half width at half maximum) of gaussian 
#' smoothing kernel to be applied during peak grouping.
#' @param min_frac Numeric, specify fraction of samples in each group that contain the feature for it to be included.
#' @param min_sample_num Numeric, specify minimum number of sample(s) in each group that contain the feature for it to be included.
#' @param max_feats Numeric, specify the maximum number of features to be identified.
#' @param rt_filt Boolean, if true, users must specify the minimum and maximum retention
#' time to be included in the analysis. By default this is set to 200 - 1000. 
#' @param rt_min Numeric, specify the minimum retention time.
#' @param rt_max Numeric, specify the maximum retention time.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

SetPeakParam <- function(alg = "centwave", ppm = 10, min_pkw = 10, 
                         max_pkw = 60, sn_thresh = 6, mzdiff = 0.01, bw = 5,
                         min_frac = 0.5, min_sample_num = 1,
                         max_feats = 100, bin_size = 1, rt_filt = FALSE, 
                         rt_min = 200, rt_max = 1000){
  
  peakParams <- list()
  
  peakParams$algorithm <- alg
  peakParams$polarity <- polarity
  peakParams$ppm <- ppm
  peakParams$min_pkw <- min_pkw
  peakParams$max_pkw <- max_pkw
  peakParams$sn_thresh <- sn_thresh
  peakParams$mzdiff <- mzdiff
  peakParams$bw <- bw
  peakParams$min_frac <- min_frac
  peakParams$min_sample_num <- min_sample_num
  peakParams$max_feats <- max_feats
  peakParams$bin_size <- bin_size
  peakParams$rt_filt <- rt_filt
  peakParams$rt_min <- rt_min
  peakParams$rt_max <- rt_max
  
  return(peakParams)
}

#' Perform peak annotation
#' This function performs feature extraction of user's raw MS data using 
#' the rawData object created using the InspectRawMSData function.
#' @param rawData The object created using the InspectRawMSData function,
#' containing the raw MS data.
#' @param peakParams The object created using the SetPeakParam function, 
#' containing user's specified or default parameters for downstream 
#' raw MS data pre-processing.
#' @param rtPlot Logical, if true creates a plot of retention time correction.
#' Defaut is set to true.
#' @param pcaPlot Logical, if true creates a PCA plot to evaluate the sample grouping.
#' Default is set to true.
#' @param labels Logical, if true, the PCA plot will be annotated with sample names.
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import xcms
#' @import ggplot2

PerformPeakProfiling <- function(rawData, peakParams, rtPlot = TRUE, pcaPlot = TRUE,
                                 labels = TRUE, format = "png", dpi = 72, width = 9){
  
  if(.on.public.web){
    load_ggplot()
    load_xcms()
  }
  
  # First check if data should be filtered by RT
  if(peakParams$rt_filt == TRUE){
    rtmin <- peakParams$rt_min
    rtmax <- peakParams$rt_max
    filt_data <- filterRt(rawData, rt = c(rtmin, rtmax))
  }else{
    filt_data <- rawData
  }
  
  # Peak picking
  
  print("Step 1/3: Started peak picking! This step will take some time...")
  
  if(peakParams$algorithm == "centwave"){
    cwp <- CentWaveParam(ppm = peakParams$ppm, peakwidth = c(peakParams$min_pkw, peakParams$max_pkw),
                         snthresh = peakParams$sn_thresh, mzdiff = peakParams$mzdiff) 
    xdata <- findChromPeaks(filt_data, param = cwp)
  }else{
    mfp <- MatchedFilterParam(binSize = peakParams$bin_size, mzdiff = peakParams$mzdiff)
    xdata <- findChromPeaks(filt_data, param = mfp)
  }
  
  print("Step 1/3: Successfully performed peak picking!")
  
  # Peak alignment (group, RT correction, group, fill in missing peaks)
  gdp <- PeakDensityParam(sampleGroups = xdata$sample_group, bw = peakParams$bw, 
                          minFraction = peakParams$min_frac, minSamples = peakParams$min_sample_num,
                          maxFeatures = peakParams$max_feats)
  
  grouped_xdata <- groupChromPeaks(xdata, param = gdp)
  rt_xdata <- adjustRtime(grouped_xdata, param = ObiwarpParam(binSize = peakParams$bin_size))
  
  print("Step 2/3: Successfully performed retention time adjustment!")
  
  if(rtPlot == TRUE){
    groupNum <- nlevels(groupInfo)
    rtplot_name <- paste("RT_adjustment", dpi, ".", format, sep="")
    Cairo::Cairo(file = rtplot_name, unit="in", dpi=dpi, width=width, height=width*5/9, 
                 type=format, bg="white")
    if(groupNum > 9){
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(groupNum)
    }else{
      group_colors <- paste0(RColorBrewer::brewer.pal(3, "Set1")[1:groupNum], "60")
    }
    names(group_colors) <- unique(rt_xdata$sample_group)
    plotAdjustedRtime(rt_xdata, col = group_colors[rt_xdata$sample_group])
    legend("topright", legend=unique(rt_xdata$sample_group), pch=15, col=group_colors);
    dev.off()
  }
  
  grouped_xdata2 <- groupChromPeaks(rt_xdata, param = gdp)
  filled <- fillChromPeaks(grouped_xdata2)
  
  if(pcaPlot == TRUE){
    
    pcaplot_name <- paste("PCA_plot", dpi, ".", format, sep="")
    Cairo::Cairo(file = pcaplot_name, unit="in", dpi=dpi, width=width, height=width*6/9, 
                 type=format, bg="white");
    par(mfrow=c(1,2));
    
    pca_feats <- log2(featureValues(grouped_xdata2, value = "into"))
    xdata_pca <- prcomp(t(na.omit(pca_feats)), center = TRUE, scale=T)
    sum.pca <- summary(xdata_pca)
    var.pca <- sum.pca$importance[2,] # variance explained by each PCA
    
    xlabel <- paste("PC1", "(", round(100*var.pca[1],1), "% variance)");
    ylabel <- paste("PC2", "(", round(100*var.pca[2],1), "% variance)");
    
    # using ggplot2
    df <- as.data.frame(xdata_pca$x)
    df$group <- grouped_xdata2$sample_group

    if(labels==TRUE){
      p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group, label=row.names(df))) + geom_text() + geom_point(size = 3) + scale_color_brewer(palette="Set1")
    }else{
      p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group)) + geom_point(size = 3) + scale_color_brewer(palette="Set1")
    }
    
    p <- p + xlab(xlabel) + ylab(ylabel) + theme_bw()
    print(p)
    dev.off();
  }
  
  xset <- as(filled, "xcmsSet")
  
  print("Step 3/3: Successfully performed peak alignment!")
  return(xset)
}

#' Set annotation parameters
#' @description This function sets the parameters for peak annotation.
#' @param polarity Character, specify the polarity of the MS instrument. Either
#' "negative" or "positive".
#' @param perc_fwhm Numeric, set the percentage of the width of the FWHM for peak grouping. 
#' Default is set to 0.6.
#' @param mz_abs_iso Numeric, set the allowed variance for the search (for isotope annotation).
#' The default is set to 0.005.
#' @param max_charge Numeric, set the maximum number of the isotope charge. For example,
#' the default is 2, therefore the max isotope charge is 2+/-. 
#' @param max_iso Numeric, set the maximum number of isotope peaks. For example, the default
#' is 2, therefore the max number of isotopes per peaks is 2.
#' @param corr_eic_th Numeric, set the threshold for intensity correlations across samples.
#' Default is set to 0.85.
#' @param mz_abs_add Numeric, set the allowed variance for the search (for adduct annotation).
#' The default is set to 0.001.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

SetAnnotationParam <- function(polarity = "positive", perc_fwhm = 0.6, mz_abs_iso = 0.005,
                               max_charge = 2, max_iso = 2, corr_eic_th = 0.85,
                               mz_abs_add = 0.001){
  
  annParams <- list()
  
  annParams$polarity <- polarity
  annParams$perf.whm <- perc_fwhm
  annParams$mz.abs.iso <- mz_abs_iso
  annParams$max.charge <- max_charge
  annParams$max.iso <- max_iso
  annParams$corr.eic.th <- corr_eic_th
  annParams$mz.abs.add <- mz_abs_add
  
  return(annParams)
}

#' Perform peak annotation
#' @description This function performs peak annotation on
#' the xset object created using the PerformPeakPicking function.
#' @param xset The object created using the PerformPeakPicking function,
#' containing the peak picked MS data.
#' @param annParams The object created using the SetAnnotationParam function, 
#' containing user's specified or default parameters for downstream 
#' raw MS data pre-processing.
#' @param deci Numeric, specify the number of decimal spots for?
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import CAMERA

PerformPeakAnnotation <- function(xset, annParams){
  
  if(.on.public.web){
    load_camera()
  }
  
  # Perform peak annotation
  print("Starting peak annotation...")
  xsa <- xsAnnotate(xset)
  xsaF <- groupFWHM(xsa, perfwhm = annParams$perf.whm) 
  xsaFI <- findIsotopes(xsaF, mzabs = annParams$mz.abs.iso, maxcharge = annParams$max.charge,
                        maxiso = annParams$max.iso)
  xsaC <- groupCorr(xsaFI, cor_eic_th = annParams$corr.eic.th) 
  xsaFA <- findAdducts(xsaC, polarity = annParams$polarity, mzabs = annParams$mz.abs.add)
  
  # Edit CAMERA peak list to add sample names
  camera_output <- getPeaklist(xsaFA)
  sample_names <- xsaFA@xcmsSet@phenoData[[1]]
  sample_names_ed <- gsub(".mzML|.CDF|.mzXML", "", sample_names) 
  length <- ncol(camera_output)
  end <- length-3
  camnames <- colnames(camera_output)
  camnames[10:end] <- sample_names_ed
  colnames(camera_output) <- camnames
  camera_output <- camera_output[,-c(7:9)]
  
  saveRDS(camera_output, "annotated_peaklist.rds")
  write.csv(camera_output, "annotated_peaklist.csv")
  
  print("Successfully performed peak annotation!")
  return(xsaFA)
}


#' Format Peak List
#' @description This function formats the CAMERA output to a usable format for MetaboAanlyst.
#' @param annotPeaks The object created using the PerformPeakAnnotation.
#' @param annParams The object created using the SetAnnotationParam function, 
#' containing user's specified or default parameters for downstream 
#' raw MS data pre-processing.
#' @param filtIso Logical, filter out all isotopes except for [M]+ for 
#' positive ion mode and [M]- for negative ion mode. By default it is 
#' set to true.
#' @param filtAdducts Logical, filter out all adducts except [M+H]+ for  
#' positive ion more and [M-H]- for negative ion mode. By default it is set to false.
#' @param missPercent Numeric, specify the threshold to remove features
#' missing in X\% of samples. For instance, 0.5 specifies to remove features
#' that are missing from 50% of all samples per group.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

FormatPeakList <- function(annotPeaks, annParams, filtIso = TRUE, filtAdducts = FALSE, missPercent = 0.5){
  
  camera_output <- readRDS("annotated_peaklist.rds")
  
  length <- ncol(camera_output)
  end <- length-3
  
  # Format peaklist for MetaboAnalyst
  camera_ma <- camera_output[, -length]
  
  if(filtAdducts==TRUE){
    if(annParams$polarity=="positive"){
      
      if(filtIso==TRUE){
        camera_isotopes <- camera_ma[grepl("\\[M\\]\\+", camera_ma$isotopes),]
      }else{
        camera_isotopes <- camera_ma[(camera_ma$isotopes != ""),]
      }
      camera_adducts <- camera_ma[grepl("\\[M\\+H\\]\\+", camera_ma$adduct),]
      camera_feats <- camera_ma[(camera_ma$isotopes == "" & camera_ma$adduct == ""),]
      unique_feats <- unique(rbind(camera_isotopes, camera_adducts, camera_feats))
      
    }else{ # negative polarity
      if(filtIso==TRUE){
        camera_isotopes <- camera_ma[grepl("\\[M\\]-", camera_ma$isotopes),]
      }else{
        camera_isotopes <- camera_ma[(camera_ma$isotopes != ""),]
      }
      camera_adducts <- camera_ma[grepl("\\[M-H\\]-", camera_ma$adduct),]
      camera_feats <- camera_ma[(camera_ma$isotopes == "" & camera_ma$adduct == ""),]
      unique_feats <- unique(rbind(camera_isotopes, camera_adducts, camera_feats))
    }
  }else{
    if(annParams$polarity=="positive"){
      
      if(filtIso==TRUE){
        camera_isotopes <- camera_ma[grepl("\\[M\\]\\+", camera_ma$isotopes),]
      }else{
        camera_isotopes <- camera_ma[(camera_ma$isotopes != ""),]
      }
      camera_adducts <- camera_ma[(camera_ma$adduct != ""),]
      camera_feats <- camera_ma[(camera_ma$isotopes == ""),]
      unique_feats <- unique(rbind(camera_isotopes, camera_adducts, camera_feats))
      
    }else{ # negative polarity
      
      if(filtIso==TRUE){
        camera_isotopes <- camera_ma[grepl("\\[M\\]-", camera_ma$isotopes),]
      }else{
        camera_isotopes <- camera_ma[(camera_ma$isotopes != ""),]
      }
      camera_adducts <- camera_ma[(camera_ma$adduct != ""),]
      camera_feats <- camera_ma[(camera_ma$isotopes == ""),]
      unique_feats <- unique(rbind(camera_isotopes, camera_adducts, camera_feats))
    }
  }
  
  # adjust decimal places
  feats_info <- unique_feats[,7:end]  
  feats_digits <- round(feats_info, 5) 
  
  group_info <- annotPeaks@xcmsSet@phenoData[[2]]
  combo_info <- rbind(as.character(group_info), feats_digits)
  
  mzs_rd <- round(unique_feats[,1], 5)
  mzs <- data.frame(c("Label", mzs_rd), stringsAsFactors = FALSE)
  
  # ensure features are unique
  mzs_unq <- mzs[duplicated(mzs),] 
  
  if(length(mzs_unq)>0){
    mzs[duplicated(mzs),] <- sapply(mzs_unq, function(x) paste0(x, sample(1:999, 1, replace = FALSE)))
  }
  
  colnames(mzs) <- "Sample" 
  ma_feats <- cbind(mzs, combo_info)
  
  # remove features missing in over X% of samples per group
  ma_feats_miss <- ma_feats[which(rowMeans(is.na(ma_feats[,(ma_feats[1,]==as.character(unique(group_info[1])))])) 
                                  | rowMeans(is.na(ma_feats[,(ma_feats[1,]==as.character(unique(group_info[2])))])) < missPercent), ]
  
  write.csv(ma_feats_miss, "metaboanalyst_input.csv", row.names = FALSE)
  
  # provide index for CAMERA output
  Pklist_inx <- row.names(ma_feats_miss)
  ma_feats_miss_inx <- cbind(ma_feats_miss, Pklist_inx) 
  
  write.csv(ma_feats_miss_inx, "filtered_peaklist.csv", row.names = FALSE)
  
  return(ma_feats_miss)
}

###
### Functions for MS2 Data
###

#' Extract MS2 Data
#' @description This function returns a list of spectra that matches with 
#' a user specified precursor m/z. 
#' @param filename Name of the file (e.g. mzML, mzXML)
#' @param peakParams Object containing parameters for peak picking.
#' @param mzmin Minimum m/z when selecting a precursor from peak list 
#' @param mzmax Maximum m/z when selecting a precursor from peak list 
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import xcms

ExtractMS2data <- function(filename, peakParams, mzmin, mzmax){
  
  ## Function to extract MSMS spectra 
  xrms <- readMSData(filename, mode = "onDisk", centroided = TRUE)
  # get all MS2 spectra from DDA experiment
  ms2spectra <- spectra(filterMsLevel(xrms, msLevel = 2))  
  
  ## Detect chromatographic peaks 
  # set parameters and find chromatographic peaks 
  ms1cwp <- CentWaveParam(snthresh = peakParams$sn_thresh, 
                          noise = 100, ppm = peakParams$ppm, peakwidth = c(peakParams$min_pkw, peakParams$max_pkw))
  ms1data <- findChromPeaks(xrms, param = ms1cwp, msLevel = 1)
  
  #get all peaks 
  chromPeaks <- chromPeaks(ms1data)

  # pick one compound 
  ## find which row contains the mass we want
  chp <- as.data.frame(chromPeaks)
  rownum <- which(chp$mz >= mzmin & chp$mz <= mzmax)
  chromPeak <- chromPeaks[rownum,]
  
  #filter out fitting spectra
  filteredMs2spectra <- .getDdaMS2Scans(chromPeak, ms2spectra)
  
  return(filteredMs2spectra)
}

### Helper function
### function for DDA data 
### Credit: Michael Witting; https://github.com/michaelwitting/metabolomics2018/blob/master/R/msLevelMerge.R
### MS2 spectra need to be list of spectrum2 objects
.getDdaMS2Scans <- function(chromPeak, ms2spectra, mzTol = 0.01, rtTol = 20) {
  
  #make a new list
  filteredMs2spectra <- list()
  
  #isolate only the ones within range
  for(i in 1:length(ms2spectra)) {
    
    #isolate Ms2 spectrum
    ms2spectrum <- ms2spectra[[i]]
    
    #check if within range of peak
    if(abs(chromPeak[4] - ms2spectrum@rt) < rtTol & abs(chromPeak[1] - ms2spectrum@precursorMz) < mzTol) {
      filteredMs2spectra <- c(filteredMs2spectra, ms2spectrum)
    }
  }
  
  #return list with filtered ms2 spectra
  return(filteredMs2spectra)
}

#' Plot selected M2 spectra for an m/z feature
#' @description This function creates a plot of the user selected precursor m/z.
#' @param ms2 Spectrum2 class object containing all of the spectra for the selected
#' m/z feature.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import xcms

PlotMS2Spectra <- function(ms2, spectra = 1){
  
  MSnbase::plot(ms2[[spectra]])
  
}  
