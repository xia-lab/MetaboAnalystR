#'Create report for raw spectra module
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateRawAnalysisRnwReport <- function(mSetObj, usrName){
  
  load("mSet.rda");
  # Creat the header & introduction of the report
  CreateHeader(usrName); # done
  
  CreateRawIntr(); # done
  CreateSpectraIOdoc(); # done
  CreateRawAnalMethod(); # done
  CreateRawAnalyworkflow(); # done
  CreateRawAnalDetails(); # done
  CreateSampleSum(); # done
  CreateFeatureSum(); # done
  
  createMSnFeatureSum(); # done
  
  CreateRHistAppendix(); # done
  CreateFooter(); # done
  
}


### Section 1 - Background
CreateRawIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr0 <- c("\n\n## Raw Spectra Processing\n\n",
              "Global or untargeted metabolomics is increasingly used to investigate metabolic changes of various biological
              or environmental systems in an unbiased manner. Liquid chromatography coupled to high-resolution mass 
              spectrometry (LC-HRMS) has become the main workhorse for global metabolomics. The typical LC-HRMS metabolomics 
              workflow involves spectra collection, raw data processing, statistical and functional analysis.\n\n")
  
  descr1 <- c("MetaboAnalyst aims to provide an efficient pipeline to support end-to-end analysis of LC-HRMS global metabolomics 
              data in a high-throughput manner. \n\n")
  
  descr <- c(descr0, descr1,
             "This module is designed to provide an automated workflow to process the raw spectra in six steps, including ",
             "reading and processing raw spectra, parameter optimization/customization, ",
             "peak picking, peak alignment, peak gap filing and peak annotation.");
  
  cat(descr, file=rmdFile, append=TRUE);
}

### Section 2 - Method Description
CreateSpectraIOdoc <- function(){
  
  descr <- c("\n\n### Raw Spectra Data Reading and Centroiding\n\n",
             "MetaboAnalyst MS Spectral Processing Module accepts several common open MS formats",
             "including mzXML, mzML, mzData, CDF formats. All of them have to be centroided before processing.",
             "The Data Integrity Check is performed before the data processing starts. The basic information",
             "of all spectra is summarized in Table", table.count<<-table.count+1,"shows the details of all spectra."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "\n\n",
    "```{r table_rawt1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- CreateSpectraInfoTableRMD();",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of data uploading raw spectra data')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n\n");
  cat("\n\n", file=rmdFile, append=TRUE);

};

CreateSpectraInfoTableRMD <- function(){
  
  if(file.exists("mSet.rda")){
    load("mSet.rda");
    
    filesize <- file.size(mSet@rawOnDisk@processingData@files)/(1024*1024);
    filename <- basename(mSet@rawOnDisk@processingData@files);
    Centroid <- rep("True",length(filename));
    GroupInfo <- as.character(mSet@rawOnDisk@phenoData@data[["sample_group"]]);
    
    sum.dat<-matrix(ncol = 4, nrow = length(filename));
    
    sum.dat[,1] <- filename;
    sum.dat[,2] <- Centroid;
    sum.dat[,3] <- round(filesize,2);
    sum.dat[,4] <- GroupInfo;
    
    colnames(sum.dat)<-c("Spectra","Centroid", "Size (MB)", "Group");
    return(sum.dat);
  } else {
    # If processing is not done. Just read the data infomation directly.
    if(dir.exists("upload")){
      files <- list.files("upload", full.names = T, recursive = T)
      
      snames <- gsub("\\.[^.]*$", "", basename(files))
      sclass <- gsub("^\\.$", "sample", dirname(files))
      
      scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "", fixed = TRUE)
      scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp))
      
      i <- 1
      while (all(scomp[i, 1] == scomp[i, -1]) && i < nrow(scomp)) {
        i <- i + 1
      }
      
      i <-
        min(i, tail(c(0, which(
          scomp[1:i, 1] == .Platform$file.sep
        )), n = 1) + 1)
      
      if (i > 1 && i <= nrow(scomp)) {
        sclass <- substr(sclass, i, max(nchar(sclass)))
      }
      
      if (.on.public.web &
          unique(sclass)[1] == "upload" &
          length(unique(sclass)) == 1) {
        sclass <- rep("Unknown", length(sclass))
      }
      
      fileFullName <- list.files("upload", recursive = T, full.names = T);
      sum.dat<-matrix(ncol = 4, nrow = length(fileFullName));
      
      sum.dat[,1] <- basename(fileFullName);
      sum.dat[,2] <- unname(sapply(fileFullName, function(x){CentroidCheck(x)}));
      sum.dat[,3] <- round(file.size(fileFullName),2);
      sum.dat[,4] <- sclass;
      return(sum.dat)
    } else {
      sum.dat <- matrix()
      return(sum.dat)
      #do nothing
    }
  }
  
};

CreateRawAnalMethod <- function(){
  descr <- c("\n\n## Raw Spectral Processing Parameters\n\n",
             "MetaboAnalyst offers several algorithms to process the spectral raw file, 
             including MatchedFilter, centWave and Massifquant for peak pciking, and obiwarp and loess for Retention time alignment.", 
             "Here the detailed algorithms and parameters' used in this study.");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  table.count <<- table.count+1;
  
  cmdhist2 <- c(
    "```{r table_raw2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- CreateParamsSumTableRMD();",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of all parameters used in this analysis.')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  descr3 <- "All parameters used to do the raw spectral processing are shown as above. 
    The detailed usage of every parameters are explanied below."
  
  cat(descr3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);

  descr2 <- c(
    "1. Peak Picking Algorithm -- centWave (For high resolution):",
    " + peakwidth min: expected approximate minimum peak width in chromatographic space.",
    " + peakwidth max: expected approximate maximum peak width in chromatographic space.",
    " + snthresh: defining the signal to noise ratio cutoff.",
    " + prefilter: ROI detection prefilter values: 'prefilter' minimum scan number.",
    " + value-of-prefilter: minimum peak intensity.",
    " + noise: allowing to set a minimum intensity cut-off to be considered as a peak.",
    " + mzdiff: minimum difference in m/z for peaks with overlapping retention times.",
    "2. Peak Picking Algorithm -- MatchedFilter (For low resolution):",
    " + fwhm: full width at half maximum of matched filtration gaussian model peak.",
    " + sigma: specifying the standard deviation (width) of the matched filtration model peak.",
    " + steps: defining the number of bins to be merged before filtration.",
    " + max: the maximum number of peaks that are expected/will be identified per slice.",
    "3. Retention Time Alignment Algorithm -- loess:",
    " + Bandwidth: bandwidth (standard deviation or half width at half maximum) of gaussian smoothing kernel.",
    " + minFraction: minimum fraction of samples necessary in at least one of the sample groups for it to be a valid group.",
    " + integrate: Integration method. Can be 1 and 2.",
    " + extra: number of extra peaks to allow in retention time correction correction groups",
    " + span: degree of smoothing for local polynomial regression fitting",
    " + profStep: step size (in m/z) to use for profile generation",
    "4. Retention Time Alignment Algorithm -- obiwarp:",
    " + binSize: bin size (in mz dimension) to be used for the profile matrix generation.",
    "5. Peak Grouping -- Group-density:",
    " + Bandwidth: bandwidth (standard deviation or half width at half maximum) of gaussian smoothing kernel.",
    " + maxFeatures: maximum number of peak groups to be identified in a single mz slice.",
    " + minSamples: minimum number of samples necessary in at least one of the sample groups for it to be a valid group.",
    "6. Others:",
    " + polarity: ion mode, can be negative or positive.",
    " + max charge: maximum number of charge for adducts annotation.",
    " + max iso: maximum number of charge for isotopes annotation.",
    " + rmConts: whether to remove the potentail contaminants for further parameters' optimization.");
  cat(descr2, file=rmdFile, append=TRUE, sep="\n");
};

CreateRawAnalyworkflow <- function(){
  # this function is used to generate a section to describe the whole workflow
  descr <- c("\n\n### Peak Alignment and Annotation\n\n",
             "After peak picking, MetaboAnalyst groups peaks based on the density distribution of all identified MS peaks. 
              This algorithm is developed from xcms R package (Colin A. et al. Anal. Chem. 2006, 78:779-787). 
              In this step, all peaks (from the same or from different samples) being close on the retention time axis are grouped into a feature (peak group).\n\n", 
             "The elution time of peaks (retention time) has been observed to vary in the chromatography between samples. 
             The retention times of all grouped MS features (peaks) were aligned with the alorithm LOESS (by default) or Obiwarp. 
             The retention time correction aligned all peaks as a table (matrix).\n\n",
             "Feature matrix may contain some NAs for samples in which no features was picked/detected. 
             While in many cases there might indeed be no peak signal in the m/z-rt region, it might also be that there is a corresponding MS feature, 
             but the feature is too weak to be detected. Therefore, many gaps need to re-filled to recover the weak peaks. To acheive this aim,
             all MS scans from the the raw spectra data were re-extracted to find all potential features regardless of the feature chromatographic shape.\n\n",
             "Besides the peak alignment, annotation on the peaks are quite important to reduce the redundancy (isotopes and adducts). 
             MetaboAnalyst use the algorithms from a R package, CAMERA (PMID: 22111785) to complete this task. 
             All annotated features are presented as a table in the later section. All annotated features ",
             "were further putatively annotated into compounds based on the adducts and isotopes annotation results. 
             In details, all formulas are firstly predicted based on the ppm and annotation results (adducts and/or isotopes). 
             Then, HMDB database will be used to searching the potential compounds. 
             All matched compounds were returned as the putative chemical ID of the feature. Complete chemical annotation results are summarized in the later section.\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
}

CreateParamsSumTableRMD <- function() {
  load("params.rda");
  # need to identify if the algorithm is asari  
  ms1_algo <- readLines("ms1_algorithm.txt")  
  paramdt <- data.frame(Parameters = names(peakParams),
                        values = vapply(1:length(peakParams), FUN = function(x){
                          as.character(peakParams[[x]])
                        }, FUN.VALUE = character(1L)))
  if(ms1_algo=="asari"){paramdt[1,2] <- "Asari"}
  return(paramdt)
}

CreateRawAnalDetails <- function(){
  
  descr <- c("\n\n### Raw Spectral Processing Platform\n\n",
             paste0("The spectra files are processed with OptiLCMS R package (", packageVersion("OptiLCMS"), ") 
                    based on Intel Xeon Processor equiped platform. Total of 4 cores are allocated for this task." ),
             "The customized or automated pipeline has been implemented based on your previous choice. \n\n",
             "Detailed description of the mechanism of automated pipeline has been published. Please find out more 
             introduction on the optimization process [(Pang Z. 2020)](https://www.mdpi.com/2218-1989/10/5/186).",
             "\n\n",
             "Please cite this publication if you are using this module to do your processing.");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
}

### Section 3 - Processing result
CreateSampleSum <- function(){
  descr <- c("\n\n## Raw Spectra Processing - Spectra Summary\n\n",
             "All spectra files included for processing in this module have been processed.",
             "All sample processing result are shown as below, including the Base Peak Ion (BPI), Total Ion Chromatogram (TIC),
             Peak intensity stats and a breif PCA display (under log transformation). 
             Meanwhile, the statistics of all spectra files has also been provided in this section.\n\n");
  cat(descr, file=rmdFile, append=TRUE);
  
  descr1 <- c(
    "Here is a brief content of this section:\n",
    " - Total Ion Chromatogram (TIC) .",
    " - Base Peak Ion (BPI) Chromatogram.",
    " - Spectral Intensity Stats.",
    " - Principal Component Analysis (PCA).",
    " - Spectral peaks summary.")
  
  cat(descr1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  CreateTIC(); # Done
  CreateBPI(); # Done
  CreateIntensityStats(); # Done
  CreatePCA(); # Done
  CreatePCA3D(); # Done
  createSpectraSumDoc(); # Done
  createSpectraTIC(); # Done
  
};
CreateTIC<- function(){
  
  descr <- c("\n\n### Total Ion Chromatogram (TIC)\n\n",
             "TIC is a chromatogram summed the intensities of all mass spectral peaks from same scan.",
             "All signals including noise and peak components are included in TIC", 
             "In other words, TIC is a sum of all the separate ion contributing to a mass spectrum",
             "The spectra TIC signal (ion) flow of all files is displayed in Figure", fig_tic <- fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  # TICs
  link <- GetSharingLink(mSetObj)
  reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                        '<a href="', link, '&format=pdf&imgCmd=raw_spec_tic" target="_blank">PDF</a> ',
                        '<a href="', link, '&format=svg&imgCmd=raw_spec_tic" target="_blank">SVG</a>',
                        '</div>')
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r figure_tic1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_tic, 
                  ". Total Ion Chromatogram plot of raw spectral data.', ",
                  " fig.lp='TICS_72.png', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics('TICS_72.png')",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
};
CreateBPI<- function(){
  
  descr <- c("\n\n### Base Peak Ion (BPI) Chromatogram\n\n",
             "The base peak chromatogram is similar to the TIC, however it only shows the intensity of the most intense signal at every scan across the whole spectrum.", 
             "Base peak chromatograms always have a cleaner and clearer shape and thus are more informative than TIC.",
             "The spectra signal (ion) flow of all files is displayed in Figure", fig_bpi <- fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  # BPIs
  link <- GetSharingLink(mSetObj)
  reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                        '<a href="', link, '&format=pdf&imgCmd=raw_spec_bpi" target="_blank">PDF</a> ',
                        '<a href="', link, '&format=svg&imgCmd=raw_spec_bpi" target="_blank">SVG</a>',
                        '</div>')
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r figure_bpi1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_bpi, 
                  ". Base Peak Ion plot of raw spectral data.', ",
                  " fig.lp='BPIS_72.png', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics('BPIS_72.png')",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
};
CreateIntensityStats<- function(){
  
  descr <- c("\n\n### Peak Intensity Statistics\n\n",
             "The general peaks' intensity is analyzed from different spectral files to show the peaks' intensity distribution",
             "The statistics all spectral peaks is displayed in Figure", fig_ints <- fig.count<<-fig.count+1,", as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  # intensity
  link <- GetSharingLink(mSetObj)
  reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                        '<a href="', link, '&format=pdf&imgCmd=raw_spec_int" target="_blank">PDF</a> ',
                        '<a href="', link, '&format=svg&imgCmd=raw_spec_int" target="_blank">SVG</a>',
                        '</div>')
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r figure_ints1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ints, 
                  ". Peak Intensity Statistics of all spectral files.', ",
                  " fig.lp='Peak_Intensity.png', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics('Peak_Intensity.png')",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
};
CreatePCA<- function(){
  
  descr <- c("\n\n### Principal component analysis (PCA)\n\n",
             "PCA is a dimension-reduction method used to increase the interpretability of high-dimension datasets and minimizing the information loss.", 
             "Here a primary PCA was performed with the log-transformed data.",
             "The PCA score plot is shown in ", fig_pcaraw <- fig.count<<-fig.count+1,", as below. ",
             "Please try to play your data with different modules (e.g. Statistic Analysis) to find out more statistic sense.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  # PCA
  link <- GetSharingLink(mSetObj)
  reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                        '<a href="', link, '&format=pdf&imgCmd=raw_spec_pca" target="_blank">PDF</a> ',
                        '<a href="', link, '&format=svg&imgCmd=raw_spec_pca" target="_blank">SVG</a>',
                        '</div>')
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r figure_pca2d, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pcaraw, 
                  ". Principal component analysis (PCA). Samples from different groups are marked with different colors.', ",
                  " fig.lp='PCA.png', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics('PCA.png')",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
};

CreatePCA3D<- function(){
  mSetObj <- .get.mSet(mSetObj);
  descr <- c("\n\n### Principal component analysis (PCA) - score\n\n",
             "PCA could also be shown in 3-dimensional style", 
             "Here a primary 3D-PCA was performed with the log-transformed data.",
             "The 3D-PCA score plot is shown in ", fig_pcaraw3d <- fig.count<<-fig.count+1,", as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

  print(paste0(mSetObj$imgSet$reportSet$scores_3d));

  if(!is.null(mSetObj$imgSet$reportSet$scores_3d) && file.exists(mSetObj$imgSet$reportSet$scores_3d)){
    fig <- c(paste0("```{r figure_pca3d, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pcaraw3d, 
                    ". 3D-PCA score. Samples from different groups are marked with different colors.', ",
                    " fig.lp='scores_3d.png', out.width = '", getFigWidth(mSetObj), "'}"),
             "knitr::include_graphics(mSetObj$imgSet$reportSet$scores_3d)",
             "```",
             "\n\n");
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

  } else {
    descr3 <- c("\n\nNo 3D-PCA Score is presented. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n", file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }

  ## Loading plot
  descr <- c("\n\n### Principal component analysis (PCA) - loading\n\n",
             "3D-PCA loading plot is showing the features as contributional loading factors for the difference among samples", 
             "Here a primary 3D-PCA loading was performed with the log-transformed data.",
             "The 3D-PCA loading plot is shown in ", fig_pca3dloading <- fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n", file=rmdFile, append=TRUE);
  if(file.exists("loadings3D.png")){
    fig <- c(paste0("```{r figure_pca3dloading, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pca3dloading, 
                    ". 3D-PCA loading plot. Samples from different groups are marked with different colors.', ",
                    " fig.lp='loadings3D.png', out.width = '", getFigWidth(mSetObj), "'}"),
             "knitr::include_graphics('loadings3D.png')",
             "```",
             "\n\n");
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  } else {
    descr3 <- c("\n\nNo 3D-PCA Loading is presented. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
  }
};
createSpectraSumDoc<- function(){
  
  descr <- c("\n\n## Spectra Summary\n\n",
             "The peaks information from different spectra after processing is summarized in Table ", 
             table.count<<-table.count+1,", as below."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "```{r table_rawsum3, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- createSpectraSumTableRMD();",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of peaks information of all spectra after processing.')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
};
createSpectraSumTableRMD <- function(){
  if(file.exists("peak_result_summary.txt")){
    dt <- read.table("peak_result_summary.txt")
    colnames(dt)<-c("Spectra","Groups", " RT range", "m/z Range", "Peak Number", "Missing (%)");
    return(dt)
  } else {
    return(data.frame("No peak results available from your analysis. Please wait raw spectral processing completed."))
  }
};
createSpectraTIC <- function(){
  
  descr <- c("\n\n### TIC of individual spectra\n\n",
             "The TIC plots you are interested in are shown as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n", file=rmdFile, append=TRUE);
  
  load("mSet.rda");
  files <- mSet@rawOnDisk@phenoData@data[["sample_name"]];
  
  if(!any(file.exists(paste0(files, ".png")))){
    descr3 <- c("\n\nNo Spectral TIC was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  for (i in files){
    if(file.exists(paste0(i, ".png"))){
      fig.count<<-fig.count+1;
      tic_nm <- paste0("fig_tic_", i);
      
      # single TIC
      link <- GetSharingLink(mSetObj)
      reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                            '<a href="', link, '&format=pdf&imgCmd=raw_spec_stic_', i, '" target="_blank">PDF</a> ',
                            '<a href="', link, '&format=svg&imgCmd=raw_spec_stic_', i, '" target="_blank">SVG</a>',
                            '</div>')
      cat(reportLinks, file=rmdFile, append=TRUE);
      cat("\n\n", file=rmdFile, append=TRUE);
      
      fig <- c(paste0("```{r ", tic_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
                      ". TIC plot of this spectra: ", gsub("_","-",sub(".png","",i)), ".', ",
                      " fig.lp='", paste0(i, ".png"), "', out.width = '", getFigWidth(mSetObj), "'}"),
               paste0("knitr::include_graphics('", paste0(i, ".png"), "')"),
               "```",
               "\n\n");
      cat(fig, file=rmdFile, append=TRUE, sep="\n");
      cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
  }

  cat("\n\n", file=rmdFile, append=TRUE);
};

### Section 4 - Processing feature
CreateFeatureSum <- function(){
  descr <- c("\n\n## Raw Spectra Processing - Feature summary\n\n",
             "All spectra files included for processing in this module have been processed.",
             "All features processing result across the different spectra are shown as below, including peak feature summary,
              and the corresponding Extracted Ion Chromatogram (EIC/XIC) of the features you are interested in. \n\n");
  cat(descr, file=rmdFile, append=TRUE);
  
  descr1 <- c(
    "Here is a brief content of this section: \n",
    " - EIC/XIC;",
    " - Feature (EIC/XIC) Stats;")
  
  cat(descr1, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

  createFeatureEIC(); # Done
  createFeatureEICStats(); # Done
  createFeatureSumDoc(); # Done
  createFeatureAnnoSum(); # Done
  # Adding more and do some optimization in future
  
};
createFeatureEIC <- function(){
  
  descr <- c("\n\n### EIC/XIC of individual feature\n\n",
             "The ones you are interested in at the analyzing stage are shown as below."
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n", file=rmdFile, append=TRUE);
  
  files <- list.files(pattern = "^EIC_.*group_[0-9]+")
  
  if(length(files) == 0){
    descr3 <- c("\n\nNo features' EIC was viewed by you. Please try to explore from Spectra result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n", file=rmdFile, append=TRUE);
  }
  
  featureNums <- vapply(files, function(x){
    strr <- strsplit(x,"_")[[1]]
    idx <- which(strr == "group")
    strr[idx[length(idx)]-1]
  }, character(1L), USE.NAMES = T);
  fnmsu <- unique(featureNums);
  
  filesnew <- vapply(fnmsu, function(x){
    fnms <- names(featureNums[featureNums == x])
    idx <- which.max(file.info(fnms)$ctime)
    fnms[idx]
  }, FUN.VALUE = character(1L))
  
  j <- 1;
  for (i in filesnew){
    fns <- fnmsu[j];
    j<-j+1;
    if(file.exists(i)){
      fig.count <<- fig.count+1;
      eic_nm <- paste0("fig_eic_", i);
      
      # raw_spec_sxic view
      link <- GetSharingLink(mSetObj)
      reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                            '<a href="', link, '&format=pdf&imgCmd=raw_spec_sxic_', fns, '" target="_blank">PDF</a> ',
                            '<a href="', link, '&format=svg&imgCmd=raw_spec_sxic_', fns, '" target="_blank">SVG</a>',
                            '</div>')
      cat(reportLinks, file=rmdFile, append=TRUE);
      cat("\n\n", file=rmdFile, append=TRUE);
      
      fig <- c(paste0("```{r ", eic_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
                      ". EIC of feature of individual samples: ", gsub("_","-",sub(".png","",i)), ".', ",
                      " fig.lp='", i, "', out.width = '", getFigWidth(mSetObj), "'}"),
               paste0("knitr::include_graphics('", i, "')"),
               "```",
               "\n\n");
      cat(fig, file=rmdFile, append=TRUE, sep="\n");
      cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
  }
 
};
createFeatureEICStats <- function(){
  
  descr <- c("\n\n### EIC/XIC Stats of individual feature\n\n",
             "The ones you are interested in at the analyzing stage are shown as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  files <- list.files(pattern = "^[^(EIC)].*mz@.*s*.png")
  
  if(length(files) == 0){
    
    descr3 <- c("\n\nNo features' EIC stats was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n", file=rmdFile, append=TRUE);
    
  }
  
  for (i in files){
    if(file.exists(i)){
      if(grepl("mz@", i)){
        fig.count<<-fig.count+1;
        
        eics_nm <- paste0("fig_eics_", i);
        ix <- gsub(".png", "", i)
        link <- GetSharingLink(mSetObj)
        reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                              '<a href="', link, '&format=pdf&imgCmd=raw_spec_msf_', ix, '" target="_blank">PDF</a> ',
                              '<a href="', link, '&format=svg&imgCmd=raw_spec_msf_', ix, '" target="_blank">SVG</a>',
                              '</div>')
        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        
        fig <- c(paste0("```{r ", eics_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
                        ". Feature intensity statis box plot of different groups: ", gsub("@","__",sub(".png","",i)), ".', ",
                        " fig.lp='", i, "', out.width = '", getFigWidth(mSetObj), "'}"),
                 paste0("knitr::include_graphics('", i, "')"),
                 "```",
                 "\n\n");
        cat(fig, file=rmdFile, append=TRUE, sep="\n");
        cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
      }
    }
  }
 
};
createFeatureSumDoc <- function(){
  
  descr <- c("\n\n## Feature Annotation Summary\n\n",
             "The features basic information and its annotation results after processing is summarized in as below."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  descr2 <- createResSumText();
  cat(descr2, file=rmdFile, append=TRUE, sep="\n");
};
createResSumText <- function(){
  txts <- PerformResultSummary();
  return(paste0(txts, sep = "\n"))
}
createFeatureAnnoSum <- function() {
  descr <- c("\n\n## Feature Annotation Results\n\n",
             "The features annotation (adducts and isotopes) is summarized in as below in Table ", 
             table.count<<-table.count+1, "."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "```{r table_rawres1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- createFeatureAnnotationSumTableRMD();",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of features annotation (adduct and isotope level).')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);

  descr2 <- c("\n\n## Compound Identification Results\n\n",
             "The putative compounds based on MS1 m/z are summarized below in Table ",
             table.count<<-table.count+1, "."
  );
  cat(descr2, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "```{r table_rawres2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- createFeatureCompoundSumTableRMD();",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of features annotation (putative compound level).')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
}
createFeatureAnnotationSumTableRMD <- function(){
  if(file.exists("peak_feature_summary.csv")){
    dt <- read.csv("peak_feature_summary.csv");
    dt <- dt[(dt$adduct !="" | dt$isotopes != ""), c(1:4)]
    rownames(dt) <- NULL;
    return(dt)
  } else {
    return(data.frame("No peak feature found!"))
  }
}

createFeatureCompoundSumTableRMD <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);

  if(file.exists("peak_feature_summary.csv")){
    dt <- read.csv("peak_feature_summary.csv");
    dt <- dt[(dt$Formula!= "" & dt$Compound!=""), c(1:2, 5:6)]
    all_cmpds <- strsplit(x = dt$Compound, "; ")
    all_cmpds <- vapply(all_cmpds, function(x){
      if(length(x)>5){
        rr <- paste(x[1:5], collapse = "; ")
      } else {
        rr <- paste(x, collapse = "; ")
      }
      if(nchar(rr)>200){
        rr <- x[1]
      }
      return(rr)
    }, FUN.VALUE = character(length = 1L))
    
    dt$Compound <-all_cmpds
    if(mSetObj$paramSet$report.format != "slides"){
      dt$Compound <- gsub(";", "\n\n", dt$Compound)
    } 
    if(mSetObj$paramSet$report.format == "pdf"){
      dt$Compound <- gsub("⁰", "^0", dt$Compound)
      dt$Compound <- gsub("¹", "^1", dt$Compound)
      dt$Compound <- gsub("²", "^2", dt$Compound)
      dt$Compound <- gsub("³", "^3", dt$Compound)
      dt$Compound <- gsub("⁴", "^4", dt$Compound)
      dt$Compound <- gsub("⁵", "^5", dt$Compound)
      dt$Compound <- gsub("⁶", "^6", dt$Compound)
      dt$Compound <- gsub("⁷", "^7", dt$Compound)
      dt$Compound <- gsub("⁸", "^8", dt$Compound)
      dt$Compound <- gsub("⁹", "^9", dt$Compound)
    }
    #dt <- dt[1:5,]
    rownames(dt) <- NULL;
    return(dt)
  } else {
    return(data.frame("No peak feature found!"))
  }
  
}





{
  plotSingleXIC_pro <- function(mSet = NA, featureNum = NULL, sample = NULL, showlabel = TRUE, format = "png") {
    
    title = paste0(sample,"_",featureNum);
    if(is.null(featureNum)) {
      featureOder <- 1;
    } else {
      featureOder <- as.numeric(featureNum);
    }
    
    s_image <- qs::qread(file = paste0("EIC_image_", featureOder,".qs"))
    fileName = paste0("raw_spec_sxic_", featureNum, ".", format);
    
    if (.on.public.web) {
      Cairo::Cairo(
        file = fileName,
        unit = "in",
        dpi = 72,
        width = 6,
        height = 6,
        type = format,
        bg = "white"
      )
    }
    
    plot(s_image);
    
    if (.on.public.web) {
      dev.off()
    }
    
    cat("Ploting EIC in a pro way successfully!\n")
    return(fileName)
  }
  
  
  
}

### Section 5 - MSn feature resutls summary


createMSnFeatureSum <- function(){
  
  if(!file.exists("compound_msn_results.csv")){
    return();
  } else {
    mset_msn <- read.csv("compound_msn_results.csv")
  }
  mSetObj <- .get.mSet(mSetObj);
  createMSnIntr();
  CreateMSnAnalMethod();
  CreateMSnAnalResults();
  CreateMatchingPatterns(mSetObj);
}

createMSnIntr <- function(){
  
  descr0 <- c("\n\n## MS/MS Spectra Annotation\n\n",
              "Metabolomics involves the comprehensive identification and quantification of small compounds in biological samples 
              using various analytical techniques. Liquid chromatography - mass spectrometry (LC-MS) has been the primary analytical platform 
              for global or untargeted metabolomics and exposomics. Following spectra acquisition, spectra processing and compound identification
              are two critical steps to explore significant signatures\n\n")
  
  descr1 <- c("To facilitate both quantitative analysis and compound identification, LC-MS untargeted metabolomics are typically conducted with MS1 full scans 
              coupled with tandem MS (MS/MS or MS2) using data-dependent acquisition (DDA) or data-independent acquisition (DIA) methods. MetaboAnalyst aims to provide two efficient pipelines to process MS2 spectra: \n\n ")
  
  descr2 <- c("+ (1) an auto-optimized DDA data deconvolution workflow to deal with chimeric spectra; \n\n+ (2) an efficient SWATH-DIA data deconvolution pipeline; \n\n")
  
  descr <- c(descr0, descr1, descr2,
             "This module is designed to provide an automated workflow to process the raw MS2 spectra data in four steps, including ",
             "raw spectra importing, integrity checking, database searching and result summary. The detailed algorithm and introduction on the workflows are included in [MetaboAnalystR 4.0](https://www.nature.com/articles/s41467-024-48009-6) publication.\n\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
}

CreateMSnAnalMethod <- function(){
  descr <- c("\n\n### MS/MS Spectra Searching Parameters\n\n",
             "MetaboAnalyst offers multiple database options and two algorithms for spectra searching. Besides, there are also several parameters need to be customized.", 
             "Here the detailed algorithms and parameters' used in this study. Explanations on these parameters are also included below.");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  table.count <<- table.count+1;
  
  cmdhist2 <- c(
    "```{r table_params, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "params <- qs::qread('msn_param_list.qs');",
    "params_df <- as.data.frame(params)",
    "params_df <- as.data.frame(t(params_df))",
    "rownames(params_df) <- c('ppm for MS1','ppm for MS2','Filtering threshold','Searching Targets','Database','Window Size', 'Similarity Method', 'Ion Mode', 'Threshold of Intensity', 'use Deco', 'Omics Type', 'use Entropy');",
    "colnames(params_df) <- 'Values';",
    paste0("create_dt(params_df,  caption = 'Table ", 
           table.count, 
           ".  Parameters for MS2 spectra searching of single spectrum.')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  descr3 <- "All parameters used to do the MS2 database searching are shown as below."
  
  cat(descr3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  descr4 <- c(
    "1. ppm values:",
    " + ppm for MS1: parts per million for ms1 spectrum. Ppm is a unit of measurement utilized to express the mass resolution or mass accuracy of the mass spectrometer. If the unit is Dalton, this value is the absolute Dalton deviation.",
    " + ppm for MS2: parts per million for ms1 spectrum. Same as ppm for MS1.",
    "2. Database:",
    " + All: All databases have been used for MS2-based annotation.",
    " + other: specifying one or more customized database for searching.",
    "3. Similarity Method:",
    " + Dot product: A traditional similarity calculation method for MS. This is a vector-similarity based method ([Details](https://pubs.acs.org/doi/10.1016/1044-0305%2894%2987009-8))",
    " + Spectral Entroy: A newly-developed similarity evaluation method. This method is based on the accuracy of spectral entropy similarity evaluation ([Details](https://www.nature.com/articles/s41592-021-01331-z)).",
    "4. Ion Mode:",
    " + ESI ion modes: can be either positive or negative.",
    "5. Searching Targets:",
    " + Searchin targets for MS2 spectra based on the statistical analysis results from MS1 features. Can be complete features or significant ones (p < 0.05, t-test for two groups and ANOVA for multiple groups).",
    "6. Filtering threshold:",
    " + Filtering threshold to remove the low-intensity signals.",
    "7. Window Size:",
    " + Window size of MS/MS acquisition.",
    "8. Threshold of Intensity:",
    " + Threshold of Intensity of MS/MS acquisition.");
  cat(descr4, file=rmdFile, append=TRUE, sep="\n");
}

CreateMSnAnalResults <- function(){
  
  descr <- c("\n\n## Spectra Searching Summary\n\n",
             "The MS2 spectral searching results is summarized in Table ", 
             table.count<<-table.count+1,", as below."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "```{r table_results1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- read.csv(\"compound_msn_results.csv\");",
    "sum_dt_done <- SummarizeMSnTable(sum_dt);",
    paste0("create_dt(sum_dt_done,  caption = 'Table ", 
           table.count, 
           ". Summary of all MS2 spectra searching results.', escape = FALSE)"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
}

CreateMatchingPatterns <- function(mSetObj){
  
  descr <- c("\n\n### Mirror plots of spectra searching\n\n",
             "The mirror plots you are interested in are shown as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n", file=rmdFile, append=TRUE);
  
  files <- unique(mSetObj[["imgSet"]][["msmsmirror"]][["imageNM"]])
  legends <- unique(mSetObj[["imgSet"]][["msmsmirror"]][["legend"]])

  if(is.null(files)){
    descr3 <- c("\n\nNo Spectral mirror plots was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    return();
  }
  
  if(!any(file.exists(files))){
    descr3 <- c("\n\nNo Spectral mirror plots was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  for (i in files){
    if(file.exists(i)){
      fig.count<<-fig.count+1;
      mir_nm <- i;
      this_legend <- legends[files == i][1]
      
      if (mSetObj$paramSet$report.format == 'html'){
        i <- gsub(".png|.svg|.pdf", "", i);
        fig <- c(paste0("```{r ", mir_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count,
                        ". Mirror plot of this spectra: ", this_legend, ".', ",
                        " fig.lp='", paste0(i, ".rds"), "', out.width = '", getFigWidth(mSetObj), "'}"),
                 paste0("p1 <- readRDS('", paste0(i, ".rds"), "'); p1"),
                 "```",
                 "\n\n");
      } else {
        fig <- c(paste0("```{r ", mir_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count,
                        ". Mirror plot of this spectra: ", this_legend, ".', ",
                        " fig.lp='", paste0(i), "', out.width = '", getFigWidth(mSetObj), "'}"),
                 paste0("knitr::include_graphics(\"", i, "\")"),
                 "```",
                 "\n\n");
      }
      
      cat(fig, file=rmdFile, append=TRUE, sep="\n");
      cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
  }
  
  cat("\n\n", file=rmdFile, append=TRUE);
}

SummarizeMSnTable <- function(sum_dt){
  
  colNms <- colnames(sum_dt)
  num <- length(grep(pattern = "InchiKey_", colNms))
  
  idx_compound <- which(grepl(pattern = "Compound_", colNms))
  idx_inchikey <- which(grepl(pattern = "InchiKey_", colNms))
  idx_formulas <- which(grepl(pattern = "Formula_", colNms))
  idx_scores <- which(grepl(pattern = "Score_", colNms))
  idx_database <- which(grepl(pattern = "Database_", colNms))
  
  all_res <- lapply(1:nrow(sum_dt), function(x){
    
    cpds <- as.character(sum_dt[x, idx_compound])
    icks <- as.character(sum_dt[x, idx_inchikey])
    fmls <- as.character(sum_dt[x, idx_formulas])
    scrs <- as.double(sum_dt[x, idx_scores])
    dtbs <- as.character(sum_dt[x, idx_database])
    
    cpds[cpds == "NA"] <- NA
    icks[icks == "NA"] <- NA
    fmls[fmls == "NA"] <- NA
    dtbs[dtbs == "NA"] <- NA
    
    res <- data.frame(compounds = cpds, 
               inchikeys = icks,
               formulas = fmls,
               scores = scrs,
               databases = dtbs)
    
    all_na_idx <- apply(res, 1, function(x){all(is.na(x))})
    res <- res[!all_na_idx,]
    
    ft <- sum_dt[rep(x, nrow(res)), c(1:4)]
    
    
    cbind(ft, res)
    
  })
  
  all_dt_res <- do.call(rbind, all_res)
}
