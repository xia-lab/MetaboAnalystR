#' Import raw MS data
#' @description This function handles the reading in of 
#' raw MS data (.mzML, .CDF and .mzXML). Users must provide 
#' a matrix with meta information about file such that each file has the name,
#' file path, group class and extension type.
#' The function will output two chromatograms into the user's working directory, a 
#' base peak intensity chromatogram (BPIC) and a total ion 
#' chromatogram (TIC). Further, this function sets the number of cores
#' to be used for parallel processing. It first determines the number of cores 
#' within a user's computer and then sets it that number/2.  
#' @param dataset.meta Matrix, input the meta data for files containing
#' the raw MS spectra to be processed.
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param par.cores Logical, if true, the function will automatically 
#' set the number of parallel cores. If false, it will not.
#' @param plot Logical, if true the function will create BPIS and TICS plots.
#' @param bpis_name Character, input the name of the BPIS image to create.
#' @param tics_name Character, input the name of the TICS image to create.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import MSnbase
#' @import BiocParallel
#' @import parallel

ImportRawMSDataList <- function(dataset.meta, format = "png", dpi = 72, width = 9, 
                                par.cores=TRUE, plot=TRUE, bpis_name = "BPIS_", tics_name="TICS_"){
  
  msg.vec <<- vector(mode="character")
  
  if(bpis_name == "BPIS_"){
    bpis_name = paste("BPIS_", dpi, ".", format, sep="");
  }
  if(tics_name == "TICS_"){
    tics_name <- paste("TICS_", dpi, ".", format, sep="");
  }
  
  msg <- c("The uploaded files are raw MS spectra.");
  
  # The dataset.meta should be a matrix such that each row has the following:
  #   1- file name, 2- file path, 3- group and 4- file extension type
  # provided. The accepted file extensions are (.mzML/.CDF/.mzXML files)
  
  if(nrow(dataset.meta) == 0 || is.na(dataset.meta)){
    AddErrMsg("No spectra were found!");
    return(0);    
  }
  
  compfile.types <- sum(sapply(dataset.meta[,3], function(x){x %in% c("mzml", "cdf", "mzxml")}))
  if(compfile.types < nrow(dataset.meta)){
    AddErrMsg("Only mzML, cdf and mzXML input types can be handled!");
    return(0);    
  }
  
  snames <- dataset.meta[,1]
  files <- dataset.meta[,2]; files <- as.character(files); # Otherwise, a factor form of files will cause an error
  sclass <- dataset.meta[,4]
  
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
  
  raw_data <- suppressMessages(read.MSdata(files = files, pdata = new("NAnnotatedDataFrame", pd),
                                          mode = "onDisk")) 
  
  if(plot==TRUE){
    # Plotting functions to see entire chromatogram
    bpis <- chromatogram(raw_data, aggregationFun = "max")
    tics <- chromatogram(raw_data, aggregationFun = "sum")
    
    groupNum <- nlevels(groupInfo)
    
    if(groupNum > 9){
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(groupNum)
    }else{
      group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:groupNum], "60")
    }
    
    names(group_colors) <- levels(groupInfo)
    
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
  }
  
  print("Successfully imported raw MS data!")
  
  return(raw_data)
}

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
#' @param mode Character, the data input mode. Default is "onDisk" to avoid memory crash. "inMemory" will 
#' absorb data into the memory.
#' @param plotSettings List, plotting parameters produced by SetPlotParam Function. "plot.opts" can be added through this 
#' function for samples numbers for plotting. Defalut is "default", "all" will apply all samples for plotting and may cause 
#' memory crash, especially for large sample dataset.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca}, Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import MSnbase
#' @import BiocParallel
#' @import parallel

ImportRawMSData <- function(foldername, mode="onDisk", ncores=4, plotSettings){
  
  msg.vec <<- vector(mode="character")
  
  msg <- c("The uploaded files are raw MS spectra.");
  
  # the "upload" folder should contain two subfolders (groups, i.e. Healthy vs. Disease)
  # each subfolder must contain samples (.mzML/.CDF/.mzXML files)
  
  files <- dir(foldername, pattern=".mzML|.cdf|.mzXML|.mzData", recursive=T, full.name=TRUE)
  
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
  
  if(!.on.public.web){
    
    cores <- parallel::detectCores()
    
    if(missing(ncores)){
      num_cores <- ceiling(cores*2/3) 
    } else{
      ncores -> num_cores
    }
    
    print(paste0("The number of CPU cores to be used is set to ", num_cores, "."))
    
    if (.Platform$OS.type == "unix") {
      BiocParallel::register(BiocParallel::bpstart(BiocParallel::MulticoreParam(num_cores)))
    } else { # for windows
      BiocParallel::register(BiocParallel::bpstart(BiocParallel::SnowParam(num_cores)))
    }
    
  }
  
  raw_data <- suppressMessages(readMSData(files = files, pdata = new("NAnnotatedDataFrame", pd),
                                          mode = mode, msLevel =1)) 
  
  
  if(plotSettings$Plot==TRUE){
    
    if (is.null(plotSettings$plot.opts)){
      plot.opts <- "default";
    } else {
      plot.opts <- plotSettings$plot.opts;
    }
    
    if(plot.opts=="default"){
      #subset raw_data to first 50 samples
      print("To reduce memory usage BPIS and TICS plots will be created using only 10 samples per group.")
      
      grp_nms <- names(table(pd$sample_group))
      files <- NA
      
      for(i in 1:length(grp_nms)){
        numb2ext <- min(table(pd$sample_group)[i], 10)
        filt_df <- pd[pd$sample_group==grp_nms[i],]
        files.inx <- sample(nrow(filt_df), numb2ext)
        sel.samples <- filt_df$sample_name[files.inx]
        files <- c(files, which(pd$sample_name %in% sel.samples))
      }
      
      raw_data_filt <- filterFile(raw_data, file=na.omit(files));
      
    }else{
      
      raw_data_filt <- raw_data; # just for plotting
      
    }
    
    if(plot.opts=="all"){
      h <- readline(prompt="Using all samples to create BPIS and TICS plots may cause severe memory issues! Press [0] to continue, or [1] to cancel: ")
      h <- as.integer(h)
      if(h==1){
        print("ImportRawMSData function aborted!")
        return(0)
      }
    }
    
    print("Plotting BPIS and TICS.")
    
    # Plotting functions to see entire chromatogram
    bpis <- chromatogram(raw_data_filt, aggregationFun = "max")
    tics <- chromatogram(raw_data_filt, aggregationFun = "sum")
    
    groupNum <- nlevels(groupInfo)
    
    if(groupNum > 9){
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(groupNum)
    }else{
      group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:groupNum], "60")
    }
    
    names(group_colors) <- levels(groupInfo)
    
    bpis_name <- paste("BPIS_", plotSettings$dpi, ".", plotSettings$format, sep="");
    tics_name <- paste("TICS_", plotSettings$dpi, ".", plotSettings$format, sep="");
    
    Cairo::Cairo(file = bpis_name, unit="in", dpi=plotSettings$dpi, width=plotSettings$width, 
                 height= plotSettings$width*5/9, type=plotSettings$format, bg="white");
    
    plot(bpis, col = group_colors[raw_data_filt$sample_group])
    legend("topright", legend=levels(groupInfo), pch=15, col=group_colors);
    
    dev.off();
    
    Cairo::Cairo(file = tics_name, unit="in", dpi=plotSettings$dpi, width=plotSettings$width, 
                 height=plotSettings$width*5/9, type=plotSettings$format, bg="white");
    
    plot(tics, col = group_colors[raw_data_filt$sample_group])
    legend("topright", legend=levels(groupInfo), pch=15, col=group_colors);
    
    dev.off();
  }
  
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
#' @param raw_data The object created using the ImportRawMSData function,
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
    group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:groupNum], "60")
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


#' Perform peak profiling
#' This function performs feature extraction of user's raw MS data using 
#' the rawData object created using the ImportRawMSData function.
#' @param rawData The object created using the ImportRawMSData function,
#' containing the raw MS data.
#' @param Params The object created using the SetPeakParam function, 
#' containing user's specified or default parameters for downstream 
#' raw MS data pre-processing.
#' @param plotSettings List, plotting parameters produced by SetPlotParam Function.
#' Defaut is set to true.
#' @param ncore Numeric, used to define the cores' number for Peak Profiling.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca}, Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import stats
#' @import MSnbase
#' @import BiocParallel
#' @import ggplot2

PerformPeakProfiling <- function(rawData, Params, plotSettings, ncore){
  
  ### Update parameters' style 
  param <- updateRawSpectraParam (Params);
  
  ### Setting the different parallel method for linux or windows
  if (missing(ncore)){
    total_threads <- detectCores()*2/3
  } else {
    total_threads <- ncore
  }
  
  
  if(.Platform$OS.type=="unix" ){
    register(bpstart(MulticoreParam(ceiling(total_threads))))
  } else if(.Platform$OS.type=="windows"){
    register(bpstart(SnowParam(ceiling(total_threads))))
  }
  
  print(paste0(ceiling(total_threads)," CPU Threads will be used for peak profiling !"))
  
  #   ---------===========----- I. Peak picking -----===========------------
  
  print("Step 1/3: Started peak picking! This step will take some time...")
  
  mSet <- PerformPeakPicking(rawData, param = param); gc()
  
  #   --------===========----- II. Peak alignment -----===========------------
  
  print("Step 2/3: Started peak alignment! This step is running...")
  
  mSet <- PerformPeakAlignment(mSet, param); gc()
  
  #   --------===========----- III. Peak filling -----===========------------
  
  print("Step 3/3: Started peak filling! This step may take some time...")
  
  mSet <- PerformPeakFiling (mSet, param); gc()
  
  print("Peak picking finished successfully !")
  
  
  #  ---------------====------IV. Plotting Results --------========-----------
  sample_idx <- mSet[["onDiskData"]]@phenoData@data[["sample_group"]];
  
  if (missing(plotSettings)){
    plotSettings <- SetPlotParam(name_peal_in="Peak_Intensity",
                                name_PCA="PCA",
                                name_adj_RT="Adjusted_RT",
                                name_adj_BPI="Adjusted_BPI")
  } else {
    plotSettings$name_peal_in="Peak_Intensity";
    plotSettings$name_PCA="PCA";
    plotSettings$name_adj_RT="Adjusted_RT";
    plotSettings$name_adj_BPI="Adjusted_BPI"
  }
  
  if (plotSettings$Plot ==T){
    
    ### 1. Peak Intensity plotting -----
    
    Cairo::Cairo(file = paste0(plotSettings$name_peal_in,".",plotSettings$format), 
                 unit="in", dpi=plotSettings$dpi,
                 width=plotSettings$width,
                 height=plotSettings$width*7/9,
                 type=plotSettings$format,
                 bg="white")
    
    if(length(unique(sample_idx)) > 9){
      
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(length(unique(sample_idx)))
      
    }else{
      
      group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:length(unique(sample_idx))], "60")
    }
    
    ints <- split(log2(mSet[["msFeatureData"]][["chromPeaks"]][, "into"]),
                  f = mSet[["msFeatureData"]][["chromPeaks"]][, "sample"])
    
    names(ints) <- as.character(sample_idx)
    group_colors <- sapply(seq(length(levels(sample_idx))), FUN=function(x){
      rep(group_colors[x],length(sample_idx[sample_idx==levels(sample_idx)[x]]))
    })
    
    boxplot(ints, varwidth = TRUE, col = as.character(unlist(group_colors)),
            ylab = expression(log[2]~intensity), 
            main = "Peak intensities")
    
    grid(nx = NA, ny = NULL)
    
    dev.off()
    
    ### 2. PCA plotting -----
    
    Cairo::Cairo(file = paste0(plotSettings$name_PCA,".",plotSettings$format),
                 unit="in", dpi=plotSettings$dpi, 
                 width=plotSettings$width,
                 height=plotSettings$width*7/9,
                 type=plotSettings$format,
                 bg="white")
    
    feature_value <-  .feature_values(pks = mSet[["msFeatureData"]][["chromPeaks"]], 
                                      fts = mSet[["FeatureGroupTable"]],
                                      method = "medret", value = "into",
                                      intensity = "into", 
                                      colnames = mSet[["onDiskData"]]@phenoData@data[["sample_name"]],
                                      missing = NA)
    
    pca_feats <- log2(feature_value)
    mSet_pca <- prcomp(t(na.omit(pca_feats)), center = TRUE, scale=T)
    sum.pca <- summary(mSet_pca)
    var.pca <- sum.pca$importance[2,] # variance explained by each PCA
    
    xlabel <- paste("PC1", "(", round(100*var.pca[1],1), "% variance)");
    ylabel <- paste("PC2", "(", round(100*var.pca[2],1), "% variance)");
    
    # using ggplot2
    df <- as.data.frame(mSet_pca$x)
    df$group <- sample_idx
    
    if(plotSettings$labels==TRUE){
      
      if(length(unique(sample_idx))>9){
        col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
        
        p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group, label=row.names(df))) + 
          geom_text() + geom_point(size = 3) + fill=col.fun(length(unique(sample_idx)))
        
      }else{
        p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group, label=row.names(df))) + 
          geom_text() + geom_point(size = 3) + scale_color_brewer(palette="Set1")
      }
      
    }else{
      if(length(unique(sample_idx))>9){
        col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
        
        p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group)) + 
          geom_point(size = 3) + scale_color_brewer(palette="Set1") + fill=col.fun(length(unique(sample_idx)))
        
      }else{
        p <- ggplot2::ggplot(df, aes(x = PC1, y = PC2, color=group)) + 
          geom_point(size = 3) + scale_color_brewer(palette="Set1")
      }
    }
    
    p <- p + xlab(xlabel) + ylab(ylabel) + theme_bw()
    print(p)
    
    dev.off()
    
    ### 3. Adjusted RT plotting -----
    
    Cairo::Cairo(file = paste0(plotSettings$name_adj_RT,".",plotSettings$format), 
                 unit="in", dpi=plotSettings$dpi,
                 width=plotSettings$width,
                 height=plotSettings$width*7/9,
                 type=plotSettings$format,
                 bg="white")
    
    if(length(unique(sample_idx)) > 9){
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(length(unique(sample_idx)))
    }else{
      group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:length(unique(sample_idx))], "60")
    }
    
    
    names(group_colors) <- unique(sample_idx)
    
    ## Extract RT information
    
    rt.set <- list(mSet[["xcmsSet"]]@rt$raw,unlist(mSet[["xcmsSet"]]@rt$corrected));
    diffRt <- rt.set[[1]] - rt.set[[2]];
    diffRt <- split(diffRt, fromFile(mSet$onDiskData));
    
    xRt <- mSet[["msFeatureData"]][["adjustedRT"]];
    col = "#00000080"; lty = 1; lwd = 1;
    col <- rep(col, length(diffRt));
    lty <- rep(lty, length(diffRt));
    lwd <- rep(lwd, length(diffRt));
    
    ylim <- range(diffRt, na.rm = TRUE);
    plot(3, 3, pch = NA, xlim = range(xRt, na.rm = TRUE),
         ylim = ylim, xlab = "RT_adj", ylab = "RT_diff");
    
    for (i in 1:length(diffRt)){
      points(x = xRt[[i]], y = diffRt[[i]], col = col[i], lty = lty[i],
             type = "l", lwd = lwd[i])
    }
    
    rawRt <- split(mSet[["xcmsSet"]]@rt$raw, fromFile(mSet$onDiskData));
    adjRt <- xRt;
    
    ####
    peaks_0 <- mSet[["msFeatureData"]][["chromPeaks"]]
    subs <- seq_along(mSet[["onDiskData"]]@phenoData@data[["sample_name"]])
    nSamples <- length(subs)
    subset_names <- original_names <- mSet[["msFeatureData"]][["chromPeakData"]]@rownames
    mSet[["FeatureGroupTable"]]$peakidx <- lapply(mSet[["FeatureGroupTable"]]$peakidx, function(z) {
      idx <- base::match(original_names[z], subset_names)
      idx[!is.na(idx)]
    })
    peakIndex_0 <- mSet[["FeatureGroupTable"]][lengths(mSet[["FeatureGroupTable"]]$peakidx) > 0, ]
    peakIndex <- .peakIndex(peakIndex_0)
    pkGroup <- .getPeakGroupsRtMatrix(peaks = peaks_0,
                                      peakIndex = peakIndex,
                                      sampleIndex = subs,
                                      missingSample = nSamples - (nSamples * param$minFraction),
                                      extraPeaks = param$extraPeaks
    )
    colnames(pkGroup) <- mSet[["onDiskData"]]@phenoData@data[["sample_name"]][subs]
    
    rawRt <- rawRt[subs]
    adjRt <- adjRt[subs]
    ## Have to "adjust" these:
    pkGroupAdj <- pkGroup
    
    for (i in 1:ncol(pkGroup)) {
      pkGroupAdj[, i] <- .applyRtAdjustment(pkGroup[, i], rawRt[[i]], adjRt[[i]])
    }
    
    diffRt <- pkGroupAdj - pkGroup
    
    xRt <- pkGroupAdj
    
    ## Loop through the rows and plot points - ordered by diffRt!
    for (i in 1:nrow(xRt)) {
      idx <- order(diffRt[i, ])
      points(x = xRt[i, ][idx], diffRt[i, ][idx],
             col = group_colors, type = "b",
             pch = 16, lty = 3)
    }
    legend("topright", legend=unique(sample_idx), pch=15, col=group_colors);
    
    dev.off()
    
    
    
    ### 4. Chromatogram Generation -----
    
    Cairo::Cairo(file = paste0(plotSettings$name_adj_BPI,".",plotSettings$format), 
                 unit="in", dpi=plotSettings$dpi,
                 width=plotSettings$width,
                 height=plotSettings$width*7/9,
                 type=plotSettings$format,
                 bg="white")
    
    
    if(length(unique(sample_idx)) > 9){
      
      col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
      group_colors <- col.fun(length(unique(sample_idx)))
      
    }else{
      
      group_colors <- paste0(RColorBrewer::brewer.pal(9, "Set1")[1:length(unique(sample_idx))], "60")
    }
    group_colors2 <- group_colors;
    names(group_colors2) <- unique(sample_idx);
    
    group_colors <- sapply(seq(length(levels(sample_idx))), FUN=function(x){
      rep(group_colors[x],length(sample_idx[sample_idx==levels(sample_idx)[x]]))
    })
    
    object_od <- mSet$onDiskData
    adj_rt <- unlist(mSet$msFeatureData$adjustedRT)
    
    object_od <- selectFeatureData(
      object_od, fcol = c("fileIdx", "spIdx", "seqNum",
                          "acquisitionNum", "msLevel",
                          "polarity", "retentionTime",
                          "precursorScanNum"))
    object_od@featureData$retentionTime <- adj_rt
    
    res <- MSnbase::chromatogram(object_od, 
                                 aggregationFun = "sum",
                                 missing = NA_real_, msLevel = 1,
                                 BPPARAM = bpparam())
    
    plot(res,col=as.character(unlist(group_colors)))
    
    legend("topright", legend=unique(sample_idx), pch=15, col=group_colors2)
    
    dev.off()
    
  }
  return(mSet)
}

#' Set generic Plotting Parameters
#' @description This function sets the generic Plotting Parameters for different functions
#' @param Plot Logical, if true, the function will plot internal figures for different functions.
#' @param labels Logical, if true, the labels in the plot will be added.
#' @param format Numeric, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param ... Other specific parameters for specific function. Please set them according to the corresponding function.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

SetPlotParam<-function(Plot = F, labels = TRUE, format = "png", dpi = 72, width = 9,...){
  
  return(list(Plot = Plot,
              labels = labels,
              format = format,
              dpi = dpi,
              width = width,
              ...))
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
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca}, Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import MSnbase
#' @importFrom graph ftM2graphNEL
#' @importFrom RBGL highlyConnSG
#' @references Kuhl C, Tautenhahn R, Boettcher C, Larson TR, Neumann S (2012). 
#' "CAMERA: an integrated strategy for compound spectra extraction and annotation of 
#' liquid chromatography/mass spectrometry data sets." Analytical Chemistry, 84, 283-289. 
#' http://pubs.acs.org/doi/abs/10.1021/ac202450g.

PerformPeakAnnotation <- function(mSet, annotaParam, ncore=1){
  
  if (ncore > 1){
    print("Only single core mode is supported now. Parallel will be supported later !")
    ncore <- 1
  }
  
  ## 1. Prepare the Annotation Object-------
  
  xs <- mSet$xcmsSet
  
  if(is.null(xs)) {
    stop("No xcmsSet object in 'mSet' was given !")
  } else if (!class(xs)=="xcmsSet"){
    stop("There is correct xcmsSet object in mSet !")
  }
  
  
  mSet$AnnotateObject <- list();
  
  if(length(xs@phenoData[["sample_name"]]) > 1 && !nrow(xs@groups) > 0) {  
    stop ('No group information found or contain only one sample.') 
  }
  mSet$AnnotateObject$sample   <-  as.numeric(NA)
  
  mSet$AnnotateObject$groupInfo <- getPeaks_selection(xs);
  runParallel <- list()
  runParallel$enable   <-  0;
  
  if (ncore > 1) {
    ## If MPI is available ...
    rmpi = "Rmpi"
    opt.warn <- options("warn")$warn
    options("warn" = -1) 
    if ((Sys.info()["sysname"] != "Windows") && require(rmpi,character.only=TRUE) && !is.null(ncore)) {
      if (is.loaded('mpi_initialize')) {
        #test if not already slaves are running!
        if(mpi.comm.size() >0){ 
          warning("There are already intialized mpi slaves on your machine.\nCamera will try to uses them!\n");
          runParallel$enable <-1;
          runParallel$mode <- rmpi;
        }else{
          mpi.spawn.Rslaves(ncore=ncore, needlog=FALSE)
          if(mpi.comm.size() > 1){
            #Slaves have successfull spawned
            runParallel$enable <-1;
            runParallel$mode <- rmpi;
          }else{ warning("Spawning of mpi slaves have failed. CAMERA will run without parallelization.\n");}
        }
      }else {
        #And now??
        warning("DLL mpi_initialize is not loaded. Run single core mode!\n");
      }
    } else {
      #try local sockets using snow package
      snow = "snow"
      if (try(require(snow,character.only=TRUE,quietly=TRUE))) {
        cat("Starting snow cluster with",ncore,"local sockets.\n")
        snowclust <- makeCluster(ncore, type = "SOCK")
        runParallel$enable <- 1
        runParallel$mode <- snow;
        runParallel$cluster <- snowclust
      }
    }
    options("warn" = opt.warn)
    cat("Run cleanParallel after processing to remove the spawned slave processes!\n")
  }
  
  if(!is.null(annotaParam[["polarity"]])){
    if(is.na(match.arg(annotaParam[["polarity"]], c("positive","negative")))){
      stop("Parameter polarity has to be 'positive' or 'negative' !")  
    }else{
      mSet$AnnotateObject$polarity <- annotaParam[["polarity"]];
    }
  }
  mSet$AnnotateObject$runParallel<-runParallel;
  
  mSet$AnnotateObject$annoID <- matrix(ncol=4, nrow=0)
  mSet$AnnotateObject$annoGrp <- matrix(ncol=4, nrow=0)
  mSet$AnnotateObject$isoID <- matrix(ncol=4, nrow=0)
  
  colnames(mSet$AnnotateObject$annoID) <-  c("id","grpID","ruleID","parentID");
  colnames(mSet$AnnotateObject$annoGrp)<-  c("id","mass","ips","psgrp");
  colnames(mSet$AnnotateObject$isoID)  <-  c("mpeak","isopeak","iso","charge")
  
  ## 2. Group peaks according to their retention time into pseudospectra-groups-----
  
  intval <- "maxo"; perfwhm <- annotaParam$perf.whm; sigma <- 6;
  
  sample    <- mSet$AnnotateObject$sample;
  pspectra  <- list();
  psSamples <- NA;
  
  print("Start grouping after retention time.")
  
  if(mSet$AnnotateObject$groupInfo[1, "rt"] == -1) {
    # Like FTICR Data
    warning("Warning: no retention times avaiable. Do nothing\n");
    return(invisible(mSet$AnnotateObject));
  }else{
    if(is.na(sample[1]) || length(mSet$xcmsSet@filepaths) > 1) {
      # grouped peaktable within automatic selection or sub selection
      if(is.na(sample[1])){
        index <- 1:length(mSet$xcmsSet@filepaths);
      }else{
        index <- sample;
      }
      
      gvals    <- groupval(mSet$xcmsSet)[,index,drop=FALSE];
      peakmat  <- mSet$xcmsSet@peaks;
      groupmat <- mSet$xcmsSet@groups;
      
      #calculate highest peaks
      maxo      <- as.numeric(apply(gvals, 1, function(x, peakmat){
        val <- na.omit(peakmat[x, intval]);
        if(length(val) == 0){
          return(NA);
        }else{
          return(max(val))
        }
      }, peakmat));
      
      maxo[which(is.na(maxo))] <- -1;
      maxo      <- cbind(1:length(maxo),maxo);
      
      #highest peak index 
      int.max   <- as.numeric(apply(gvals, 1, function(x, peakmat){which.max(peakmat[x, intval])}, peakmat));
      
      peakrange <- matrix(apply(gvals, 1, function(x, peakmat) { 
        val <- peakmat[x, intval];
        if(length(na.omit(val)) == 0){
          return(c(0,1));
        } else {
          return(peakmat[x[which.max(val)], c("rtmin", "rtmax")]);
        }
      }, peakmat), ncol=2, byrow=TRUE); 
      
      colnames(peakrange) <- c("rtmin", "rtmax")
      
      while(length(maxo) > 0){
        iint   <- which.max(maxo[,2]);
        rtmed  <- groupmat[iint, "rtmed"]; #highest peak in whole spectra
        rt.min <- peakrange[iint, "rtmin"];
        rt.max <- peakrange[iint, "rtmax"]; #begin and end of the highest peak
        hwhm   <- ((rt.max-rt.min) / sigma * 2.35 * perfwhm) / 2; #fwhm of the highest peak
        #all other peaks whose retensiontimes are in the fwhm of the highest peak
        irt    <- which(groupmat[, 'rtmed'] > (rtmed-hwhm) & groupmat[, 'rtmed'] < (rtmed + hwhm)) 
        if(length(irt) > 0){
          #if peaks are found
          idx <- maxo[irt,1];
          pspectra[[length(pspectra)+1]] <- idx; #create groups
          psSamples[length(pspectra)]  <- index[int.max[maxo[iint,1]]] # saves the sample of the peak which is in charge for this pspectrum
          maxo <- maxo[-irt, ,drop=FALSE]; #set itensities of peaks to NA, due to not to be found in the next cycle
          groupmat   <- groupmat[-irt, ,drop=FALSE];
          peakrange  <- peakrange[-irt, ,drop=FALSE];
        }else{              
          idx <- maxo[iint,1];
          cat("Warning: Feature ",idx," looks odd for at least one peak. Please check afterwards.\n");
          pspectra[[length(pspectra)+1]] <- idx; #create groups
          psSamples[length(pspectra)]  <- index[int.max[maxo[iint,1]]] # saves the sample of the peak which is in charge for this pspectrum
          maxo <- maxo[-iint, ,drop=FALSE]; #set itensities of peaks to NA, due to not to be found in the next cycle
          groupmat   <- groupmat[-iint, ,drop=FALSE];
          peakrange  <- peakrange[-iint, ,drop=FALSE];
        }
      }
      
    }else{
      #One sample experiment
      peakmat <- mSet$xcmsSet@peaks;
      maxo    <- peakmat[, intval]; #max intensities of all peaks
      maxo    <- cbind(1:length(maxo),maxo);
      
      while(length(maxo)> 0){
        iint   <- which.max(maxo[,2]);
        rtmed  <- peakmat[iint, "rt"]; #highest peak in whole spectra
        rt.min <- peakmat[iint, "rtmin"];
        rt.max <- peakmat[iint, "rtmax"]; #begin and end of the highest peak
        hwhm   <- ((rt.max - rt.min) / sigma * 2.35 * perfwhm) / 2; #fwhm of the highest peak
        #all other peaks whose retensiontimes are in the fwhm of the highest peak
        irt    <- which(peakmat[, 'rt'] > (rtmed - hwhm) & peakmat[, 'rt'] < (rtmed + hwhm)) 
        if(length(irt)>0){
          #if peaks are found
          idx <- maxo[irt,1];
          pspectra[[length(pspectra)+1]] <- idx; #create groups
          maxo <- maxo[-irt, ,drop=FALSE]; #set itensities of peaks to NA, due to not to be found in the next cycle
          peakmat <- peakmat[-irt, ,drop=FALSE];
        }else{
          idx <- maxo[iint,1];
          cat("Warning: Feature ",idx," looks odd for at least one peak. Please check afterwards.\n");
          pspectra[[length(pspectra)+1]] <- idx; #create groups
          maxo       <- maxo[-iint, ,drop=FALSE]; #set itensities of peaks to NA, due to not to be found in the next cycle
          peakmat  <- peakmat[-iint, ,drop=FALSE];
        }
      }
      psSamples <- rep(sample, length(pspectra))
    }
    
    mSet$AnnotateObject$pspectra  <- pspectra;
    mSet$AnnotateObject$psSamples <- psSamples;
    message (paste("Created", length(mSet$AnnotateObject$pspectra), "pseudospectra."))
  }
  
  
  ## 3. Annotate isotope peaks -----
  
  maxcharge <- annotaParam$max.charge; maxiso <- annotaParam$max.iso; 
  mzabs <- annotaParam$mz.abs.add; intval=c("maxo");
  minfrac=0.5;  isotopeMatrix=NULL;  filter=TRUE; ppm <- 5;
  
  if(!is.wholenumber(maxcharge) || maxcharge < 1){
    stop("Invalid argument 'maxcharge'. Must be integer and > 0.\n")
  }
  if(!is.wholenumber(maxiso) || maxiso < 1){
    stop("Invalid argument 'maxiso'. Must be integer and > 0.\n")
  }
  if(!is.numeric(mzabs) || mzabs < 0){
    stop("Invalid argument 'mzabs'. Must be numeric and not negative.\n")
  }
  
  #intval <- match.arg(intval)
  
  if(!is.null(isotopeMatrix)){
    if(!is.matrix(isotopeMatrix) || ncol(isotopeMatrix) != 4 || nrow(isotopeMatrix) < 1
       || !is.numeric(isotopeMatrix)){
      stop("Invalid argument 'isotopeMatrix'. Must be four column numeric matrix.\n")
    } else {
      colnames(isotopeMatrix) <- c("mzmin", "mzmax", "intmin", "intmax")
    }
  }else if(maxiso > 8){
    stop("Invalid argument 'maxiso'. Must be lower 9 or provide your own isotopeMatrix.\n")
  }else{
    isotopeMatrix <- calcIsotopeMatrix(maxiso=maxiso)
  }
  ####End Test arguments
  
  npeaks.global <- 0; #Counter for % bar
  npspectra <- length(mSet$AnnotateObject$pspectra);
  
  # scaling
  devppm <- ppm / 1000000;
  filter <- filter;
  #generate parameter list
  params <- list(maxiso=maxiso, maxcharge=maxcharge, devppm=devppm, 
                 mzabs=mzabs, IM=isotopeMatrix, minfrac=minfrac, filter=filter)
  
  #Check if object have been preprocessed with groupFWHM
  if(npspectra < 1) {
    cat("xsAnnotate contains no pseudospectra. Regroup all peaks into one!\n")
    npspectra <- 1;
    mSet$AnnotateObject$pspectra[[1]] <- seq(1:nrow(mSet$AnnotateObject$groupInfo));
    mSet$AnnotateObject$psSamples  <- 1;
  }
  
  #number of peaks in pseudospectra
  ncl <- sum(sapply(mSet$AnnotateObject$pspectra, length));
  
  # get mz,rt and intensity values from peaktable
  if(nrow(mSet$xcmsSet@groups) > 0){
    ##multiple sample or grouped single sample
    if(is.na(mSet$AnnotateObject$sample[1])){
      index <- 1:length(mSet$xcmsSet@filepaths);
    }else{
      index <- mSet$AnnotateObject$sample;
    }
    message("Generating peak matrix...");
    mint     <- groupval(mSet$xcmsSet,value=intval)[,index,drop=FALSE];
    imz <- mSet$AnnotateObject$groupInfo[, "mz", drop=FALSE];
    irt <- mSet$AnnotateObject$groupInfo[, "rt", drop=FALSE];
  }else{
    ##one sample case
    message("Generating peak matrix...");
    imz  <- mSet$AnnotateObject$groupInfo[, "mz", drop=FALSE];
    irt  <- mSet$AnnotateObject$groupInfo[, "rt", drop=FALSE];
    mint <- mSet$AnnotateObject$groupInfo[, intval, drop=FALSE];      
  }
  
  isotope   <- vector("list", length(imz));
  
  isomatrix <- matrix(ncol=5, nrow=0);
  colnames(isomatrix) <- c("mpeak", "isopeak", "iso", "charge", "intrinsic")
  
  
  message("Run isotope peak annotation");

  lp <- -1;along = mSet$AnnotateObject$pspectra;
  pb <- progress_bar$new(format = "Isotope [:bar] :percent Time left: :eta", total = length(along), clear = T, width= 75)
  
  #look for isotopes in every pseudospectra
  for(i in seq(along)){
    #get peak indizes for i-th pseudospectrum
    ipeak <- mSet$AnnotateObject$pspectra[[i]];
    
    #Ouput counter
    pb$tick();
    
    #Pseudospectrum has more than one peak
    if(length(ipeak) > 1){
      #peak mass and intensity for pseudospectrum
      mz  <- imz[ipeak];
      int <- mint[ipeak, , drop=FALSE];
      isomatrix <-  findIsotopesPspec(isomatrix, mz, ipeak, int, params)              
    }
  }
  
  #clean isotopes
  if(is.null(nrow(isomatrix))) {
    isomatrix = matrix(isomatrix, byrow=F, ncol=length(isomatrix)) 
  }
  
  #check if every isotope has only one annotation
  if(length(idx.duplicated <- which(duplicated(isomatrix[, 2]))) > 0){
    peak.idx <- unique(isomatrix[idx.duplicated, 2]);
    for( i in 1:length(peak.idx)){
      #peak.idx has two or more annotated charge
      #select the charge with the higher cardinality
      peak <- peak.idx[i];
      peak.mono.idx <- which(isomatrix[,2] == peak)
      if(length(peak.mono.idx) < 2){
        #peak has already been deleted
        next;
      }
      peak.mono <- isomatrix[peak.mono.idx,1]
      #which charges we have
      charges.list   <- isomatrix[peak.mono.idx, 4];
      tmp <- cbind(peak.mono,charges.list);
      charges.length <- apply(tmp,1, function(x,isomatrix) { 
        length(which(isomatrix[, 1] == x[1] & isomatrix[,4] == x[2])) }, 
        isomatrix);
      idx <- which(charges.length == max(charges.length));
      if(length(idx) == 1){
        #max is unique
        isomatrix <- isomatrix[-which(isomatrix[, 1] %in% peak.mono[-idx] & isomatrix[, 4] %in% charges.list[-idx]),, drop=FALSE]
      }else{
        #select this one, which lower charge
        idx <- which.min(charges.list[idx]);
        isomatrix <- isomatrix[-which(isomatrix[, 1] %in% peak.mono[-idx] & isomatrix[, 4] %in% charges.list[-idx]),, drop=FALSE]
      }
    }
  }
  
  
  #check if every isotope in one isotope grp, have the same charge
  if(length(idx.duplicated <- which(duplicated(paste(isomatrix[, 1], isomatrix[, 3])))) > 0){
    #at least one pair of peakindex and number of isotopic peak is identical
    peak.idx <- unique(isomatrix[idx.duplicated,1]);
    for( i in 1:length(peak.idx)){
      #peak.idx has two or more annotated charge
      #select the charge with the higher cardinality
      peak <- peak.idx[i];
      #which charges we have
      charges.list   <- unique(isomatrix[which(isomatrix[, 1] == peak), 4]);
      #how many isotopes have been found, which this charges
      charges.length <- sapply(charges.list, function(x,isomatrix,peak) { length(which(isomatrix[, 1] == peak & isomatrix[, 4] == x)) },isomatrix,peak);
      #select the charge which the highest cardinality
      idx <- which(charges.length == max(charges.length));
      if(length(idx) == 1){
        #max is unique
        isomatrix <- isomatrix[-which(isomatrix[, 1] == peak & isomatrix[, 4] %in% charges.list[-idx]),, drop=FALSE]
      }else{
        #select this one, which lower charge
        idx <- which.min(charges.list[idx]);
        isomatrix <- isomatrix[-which(isomatrix[, 1] == peak & isomatrix[, 4] %in% charges.list[-idx]),, drop=FALSE]
      }
    }
  }
  
  #Combine isotope cluster, if they overlap
  index2remove <- c();
  
  if(length(idx.duplicated <- which(isomatrix[, 1] %in% isomatrix[, 2]))>0){
    for(i in 1:length(idx.duplicated)){
      index <-  which(isomatrix[, 2] == isomatrix[idx.duplicated[i], 1])
      index2 <- sapply(index, function(x, isomatrix) which(isomatrix[, 1] == isomatrix[x, 1] & isomatrix[,3] == 1),isomatrix)
      if(length(index2) == 0){
        index2remove <- c(index2remove,idx.duplicated[i])
      }
      max.index <- which.max(isomatrix[index,4]);
      isomatrix[idx.duplicated[i], 1] <- isomatrix[index[max.index], 1];
      isomatrix[idx.duplicated[i], 3] <- isomatrix[index[max.index], 3]+1;
    }
  }
  
  if(length(index <- which(isomatrix[,"iso"] > maxiso)) > 0){
    index2remove <- c(index2remove, index)
  }
  
  if(length(index2remove) > 0){
    isomatrix <- isomatrix[-index2remove,, drop=FALSE];
  }
  
  isomatrix <- isomatrix[order(isomatrix[,1]),,drop=FALSE]
  #Create isotope matrix within object
  mSet$AnnotateObject$isoID <- matrix(nrow=0, ncol=4);
  colnames(mSet$AnnotateObject$isoID)  <-  c("mpeak", "isopeak", "iso", "charge");
  
  #Add isomatrix to object
  mSet$AnnotateObject$isoID <- rbind(mSet$AnnotateObject$isoID, isomatrix[, 1:4]);
  
  # counter for isotope groups
  globalcnt <- 0;
  oldnum    <- 0;
  
  if(nrow(isomatrix) > 0){
    for( i in 1:nrow(isomatrix)){
      if(!isomatrix[i, 1] == oldnum){
        globalcnt <- globalcnt+1; 
        isotope[[isomatrix[i, 1]]] <- list(y=globalcnt, iso=0, charge=isomatrix[i, 4], val=isomatrix[i, 5]);
        oldnum <- isomatrix[i, 1];
      };
      isotope[[isomatrix[i,2]]] <- list(y=globalcnt,iso=isomatrix[i,3],charge=isomatrix[i,4],val=isomatrix[i,5]);
    }
  }
  cnt<-nrow(mSet$AnnotateObject$isoID);
  cat("\nFound isotopes:",cnt,"\n");
  mSet$AnnotateObject$isotopes <- isotope;
  
  
  ## 4. Peak grouping with information -----
  
  cor_eic_th <- annotaParam$corr.eic.th;
  cor_exp_th <- 0.75;  pval=0.05;  graphMethod="hcs"
  calcIso = FALSE; calcCaS = FALSE; psg_list=NULL; xraw=NULL; intval="into"
  
  
  if (!is.numeric(cor_eic_th) || cor_eic_th < 0 || cor_eic_th > 1){
    stop ("Parameter cor_eic_th must be numeric and between 0 and 1.\n");
  }
  
  npspectra <- length(mSet$AnnotateObject$pspectra);
  
  print("Start grouping after correlation.")
  #Data is not preprocessed with groupFWHM 
  if(npspectra < 1){
    cat("Data was not preprocessed with groupFWHM, creating one pseudospectrum with all peaks.\n")
    #Group all peaks into one group
    npspectra <- 1;
    mSet$AnnotateObject$pspectra[[1]] <- seq(1:nrow(mSet$AnnotateObject$groupInfo));
    if(is.na(mSet$AnnotateObject$sample[1])){
      mSet$AnnotateObject$psSamples <- rep(1,nrow(mSet$AnnotateObject$groupInfo)); ##TODO: Change if sample=NA or sample=number
    }else{
      mSet$AnnotateObject$psSamples <- rep(mSet$AnnotateObject$sample,nrow(mSet$AnnotateObject$groupInfo));
    }
  }
  
  #save number of pspectra before groupCorr
  cnt <- length(mSet$AnnotateObject$pspectra);
  res <- list();
  
  # Check LC information and calcCorr was selected
  
  #Autoselect sample path for EIC correlation    
  index <- rep(0, nrow(mSet$AnnotateObject$groupInfo));
  
  for(i in 1:npspectra){
    index[mSet$AnnotateObject$pspectra[[i]]] <- mSet$AnnotateObject$psSamples[[i]];
  }
  
  #Generate EIC data
  
  tmp <- getAllPeakEICs(mSet, index=index);
  
  EIC <- tmp$EIC
  
  scantimes <- tmp$scantimes
  rm(tmp);gc()
  
  res[[1]] <- calcCiS(mSet, EIC=EIC,
                      corval=cor_eic_th,
                      pval=pval, 
                      psg_list=psg_list);
  
  #Check if we have at least 2 result matrixes
  if(length(res) > 2){
    #combine the first two to create the result Table
    resMat <- combineCalc(res[[1]], res[[2]], method="sum");
    for( i in 3:length(res)){
      resMat <- combineCalc(resMat, res[[i]], method="sum");
    }         
  }else if(length(res) == 2){
    #combine one time
    resMat <- combineCalc(res[[1]], res[[2]], method="sum")
  } else {
    #Only one matrix
    resMat <- res[[1]];
  }
  
  #Perform graph seperation to seperate co-eluting pseudospectra
  mSet <- calcPC.hcs(mSet, ajc=resMat, psg_list=psg_list);                                   
  
  #Create pc groups based on correlation results
  message (paste("mSet has now", length(mSet$AnnotateObject$pspectra), "groups, instead of", cnt, "!")); 
  
  ## 5. Annotate adducts (and fragments) -----
  mSet$AnnotateObject$ruleset <- NULL
  mSet <- findAdducts (mSet, polarity = annotaParam$polarity, mzabs = annotaParam$mz.abs.add,
                       maxcharge = annotaParam$max.charge)
  
  ## 6. Data Organization -----
  camera_output <- getPeaklist(mSet)
  
  sample_names <- mSet$xcmsSet@phenoData[[1]]
  sample_names_ed <- gsub(".mzML|.CDF|.mzXML", "", sample_names) 
  
  # Account for multiple groups
  length <- ncol(camera_output)
  end <- length-3
  camnames <- colnames(camera_output)
  groupNum <- nlevels(mSet[["xcmsSet"]]@phenoData[["sample_group"]])
  start <- groupNum+8
  camnames[start:end] <- sample_names_ed
  colnames(camera_output) <- camnames
  
  endGroup <- 7+groupNum
  camera_output <- camera_output[,-c(7:endGroup)]
  
  saveRDS(camera_output, "annotated_peaklist.rds")
  write.csv(camera_output, "annotated_peaklist.csv")
  
  message("Successfully performed peak annotation!")
  
  ## 0. Final Output -----
  
  return(mSet)
  
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
#' that are missing from 50\% of all samples per group. Method is only valid
#' when there are two groups.
#' @param includeRT Logical, set to TRUE to include retention time information in
#' the feature names. Feature names must be formatted
#' so that the mz and retention time for a single peak is separated by two
#' underscores. For instance, m/z of 410.2148 and retention time of 42.46914 seconds
#' must be formatted as 410.2148__42.46914.
#' @author Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

FormatPeakList <- function(annotPeaks, annParams, filtIso = TRUE, filtAdducts = FALSE, missPercent = 0.5, includeRT = TRUE){
  
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
  
  # adjust decimal places, feats_info contains all samples
  feats_info <- unique_feats[,7:end]  
  feats_digits <- round(feats_info, 5) 
  
  group_info <- annotPeaks$xcmsSet@phenoData[[2]]
  combo_info <- rbind(as.character(group_info), feats_digits)
  
  mzs_rd <- round(unique_feats[,1], 5)
  if(includeRT){
    r_rd <- round(unique_feats[,4], 5)
    mz_rt <- paste(mzs_rd, r_rd, sep="__")
    mzs <- data.frame(c("Label", mz_rt), stringsAsFactors = FALSE)
  }else{
    mzs <- data.frame(c("Label", mzs_rd), stringsAsFactors = FALSE)
    # ensure features are unique
    mzs_unq <- mzs[duplicated(mzs),]
    while(length(mzs_unq)>0){
      mzs[duplicated(mzs),] <- sapply(mzs_unq, function(x) paste0(x, sample(1:999, 1, replace = FALSE)));
      mzs_unq <- mzs[duplicated(mzs),]
    }
  }
  
  colnames(mzs) <- "Sample" 
  ma_feats <- cbind(mzs, combo_info)
  
  # remove features missing in over X% of samples per group
  # only valid for 2 group comparisons!!
  ma_feats_miss <- ma_feats[which(rowMeans(is.na(ma_feats[,(ma_feats[1,]==as.character(unique(group_info[1])))])) 
                                  | rowMeans(is.na(ma_feats[,(ma_feats[1,]==as.character(unique(group_info[2])))])) < missPercent), ]
  
  write.csv(ma_feats_miss, "metaboanalyst_input.csv", row.names = FALSE)
  
  # provide index for CAMERA output
  Pklist_inx <- row.names(ma_feats_miss)
  ma_feats_miss_inx <- cbind(ma_feats_miss, Pklist_inx) 
  
  write.csv(ma_feats_miss_inx, "filtered_peaklist.csv", row.names = FALSE)
  
  return(ma_feats_miss)
}
