#'Create report for raw spectra module
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
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
  

  CreateRHistAppendix(); # done
  CreateFooter(); # done
  
}


### Section 1 - Background
CreateRawIntr <- function(){
  descr0 <- c("\\section{Raw Spectra Processing}\n",
              "Global or untargeted metabolomics is increasingly used to investigate metabolic changes of various biological
              or environmental systems in an unbiased manner. Liquid chromatography coupled to high-resolution mass 
              spectrometry (LC-HRMS) has become the main workhorse for global metabolomics. The typical LC-HRMS metabolomics 
              workflow involves spectra collection, raw data processing, statistical and functional analysis.\n\n")
  
  descr1 <- c("MetaboAnalyst aims to provide an efficient pipeline to support end-to-end analysis of LC-HRMS global metabolomics 
              data in a high-throughput manner. \n\n")
  
  descr <- c(descr0, descr1,
             "This module is designed to provide an automated workflow to process the raw spectra in six steps, including ",
             "reading and processing raw spectra, parameter optimization/customization, peak picking, peak alignment, peak gap filing and peak annotation.");
  
  cat(descr, file=rnwFile, append=TRUE);
}

### Section 2 - Method Description
CreateSpectraIOdoc <- function(){
  
  descr <- c("\\subsection{Raw Spectra Data Reading and Centroiding}\n",
             "MetaboAnalyst MS Spectral Processing Module accepts several common open MS formats",
             "including mzXML, mzML, mzData, CDF formats. All of them have to be centroided before processing.",
             "The Data Integrity Check is performed before the data processing starts. The basic information",
             "of all spectra is summarized in Table", table.count<<-table.count+1,"shows the details of all spectra."
  );
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "CreateSpectraInfoTable()",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
CreateSpectraInfoTable <- function(){
  
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
    print(xtable::xtable(sum.dat, 
                         caption="Summary of data uploading results"), 
          caption.placement="top", 
          size="\\scriptsize");
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
      
    } else {
      #do nothing
    }
  }
  
};
CreateRawAnalMethod <- function(){
  descr <- c("\\subsection{Raw Spectral Processing Parameters}",
             "MetaboAnalyst offers several algorithms to process the spectral raw file, 
             including MatchedFilter, centWave and Massifquant for peak pciking, and obiwarp and loess for Retention time alignment.", 
             "Here the detailed algorithms and parameters' used in this study.");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  load("params.rda");
  
  paramVec <- vector();
  for (i in seq(length(peakParams))){
    
    if(names(peakParams[i]) == "Peak_method"){
      paramVec <- c(paramVec, paste0("\\item{Peak Picking Algorithm: ", peakParams[["Peak_method"]], "}"))
    } else if(names(peakParams[i]) == "RT_method"){
      paramVec <- c(paramVec, paste0("\\item{RT Alignment Algorithm: ", peakParams[["RT_method"]], "}"))
    } else {
      paramVec <- c(paramVec, paste0("\\item{", gsub(pattern = "_", replacement = " ",names(peakParams[i])), ": ", peakParams[i], "}"))
    }
    
  }
  
  descr2 <- c(
    "\\begin{enumerate}",
    paramVec,
    "\\end{enumerate}",
    "\\texttt{All parameters used to do the raw spectral processing are shown as above. 
    The detailed usage of every parameters are explanied in next page.}",
    "\\clearpage"
  );
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
  
  descr3 <- c(
    "\\begin{enumerate}",
    
    "\\item{Peak Picking Algorithm -- centWave (For high resolution): }",
    "\\begin{itemize}",
    "\\item{peakwidth min: expected approximate minimum peak width in chromatographic space. }",
    "\\item{peakwidth max: expected approximate maximum peak width in chromatographic space.}",
    "\\item{snthresh: defining the signal to noise ratio cutoff.}",
    "\\item{prefilter: ROI detection prefilter values: 'prefilter' minimum scan number;}",
    "\\item{value-of-prefilter: minimum peak intensity.}",
    "\\item{noise: allowing to set a minimum intensity cut-off to be considered as a peak.}",
    "\\item{mzdiff: minimum difference in m/z for peaks with overlapping retention times.}",
    "\\end{itemize}",
    
    "\\item{Peak Picking Algorithm -- MatchedFilter (For low resolution):}",
    "\\begin{itemize}",
    "\\item{fwhm: full width at half maximum of matched filtration gaussian model peak.}",
    "\\item{sigma: specifying the standard deviation (width) of the matched filtration model peak.}",
    "\\item{steps: defining the number of bins to be merged before filtration.}",
    "\\item{max: the maximum number of peaks that are expected/will be identified per slice.}",
    "\\end{itemize}",
    
    "\\item{Retention Time Alignment Algorithm -- loess:}",
    "\\begin{itemize}",
    "\\item{Bandwidth: bandwidth (standard deviation or half width at half maximum) of gaussian smoothing kernel.}",
    "\\item{minFraction: minimum fraction of samples necessary in at least one of the sample groups for it to be a valid group.}",
    "\\item{integrate: Integration method. Can be 1 and 2.}",
    "\\item{extra: number of extra peaks to allow in retention time correction correction groups }",
    "\\item{span: degree of smoothing for local polynomial regression fitting }",
    "\\item{profStep: step size (in m/z) to use for profile generation}",
    "\\end{itemize}",
    
    "\\item{Retention Time Alignment Algorithm -- obiwarp:}",
    "\\begin{itemize}",
    "\\item{binSize: bin size (in mz dimension) to be used for the profile matrix generation.}",
    "\\end{itemize}",
    
    "\\item{Peak Grouping -- Group-density:}",
    "\\begin{itemize}",
    "\\item{Bandwidth: bandwidth (standard deviation or half width at half maximum) of gaussian smoothing kernel.}",
    "\\item{maxFeatures: maximum number of peak groups to be identified in a single mz slice.}",
    "\\item{minSamples: minimum number of samples necessary in at least one of the sample groups for it to be a valid group.}",
    "\\end{itemize}",
    
    "\\item{Others:}",
    "\\begin{itemize}",
    "\\item{polarity: ion mode, can be negative or positive.}",
    "\\item{max charge: maximum number of charge for adducts annotation.}",
    "\\item{max iso: maximum number of charge for isotopes annotation.}",
    "\\item{rmConts: whether to remove the potentail contaminants for further parameters' optimization.}",
    "\\end{itemize}",
    
    "\\end{enumerate}",
    "\\clearpage"
  );
  
  cat(descr3, file=rnwFile, append=TRUE, sep="\n");
  
};
CreateRawAnalyworkflow <- function(){
  # this function is used to generate a section to describe the whole workflow
  descr <- c("\\subsection{Peak Alignment and Annotation}",
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
             All annotated features are presented as a table in the later section. All annotated features were further putatively annotated into compounds based on the adducts and isotopes annotation results. 
             In details, all formulas are firstly predicted based on the ppm and annotation results (adducts and/or isotopes). 
             Then, HMDB database will be used to searching the potential compounds. 
             All matched compounds were returned as the putative chemical ID of the feature. Complete chemical annotation results are summarized in the later section.\n ");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
}
CreateRawAnalDetails <- function(){
  
  descr <- c("\\subsection{Raw Spectral Processing Platform}",
             paste0("The spectra files are processed with OptiLCMS R package (", packageVersion("OptiLCMS"), ") 
                    based on Intel Xeon Processor equiped platform. Total of 4 cores are allocated for this task." ),
             "The customized or automated pipeline has been implemented based on your previous choice. \n\n",
             "Detailed description of the mechanism of automated pipeline has been published. Please find out more 
             introduction on the optimization process from the following paper.",
             "\\begin{itemize}",
             "\\item{Pang Z, Chong J, Li S, Xia J. MetaboAnalystR 3.0: Toward an Optimized Workflow for Global Metabolomics. Metabolites. 2020 May 7;10(5):186. doi: 10.3390/metabo10050186. PMID: 32392884; PMCID: PMC7281575.}",
             "\\end{itemize}",
             "\n\n",
             "Please cite this publication if you are using this module to do your processing.");
  
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}

### Section 3 - Processing result
CreateSampleSum <- function(){
  descr <- c("\\section{Raw Spectra Processing - Spectra Summary}\n",
             "All spectra files included for processing in this module have been processed.",
             "All sample processing result are shown as below, including the Base Peak Ion (BPI), Total Ion Chromatogram (TIC),
             Peak intensity stats and a breif PCA display (under log transformation). 
             Meanwhile, the statistics of all spectra files has also been provided in this section.");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr1 <- c(
    "Here is a brief content of this section",
    "\\begin{itemize}",
    "\\item{Total Ion Chromatogram (TIC) }",
    "\\item{Base Peak Ion (BPI) Chromatogram}",
    "\\item{Spectral Intensity Stats}",
    "\\item{Principal Component Analysis (PCA)}",
    "\\item{Spectral peaks summary}",
    "\\end{itemize}")
  
  cat(descr1, file=rnwFile, append=TRUE);
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
  CreateTIC(); # Done
  CreateBPI(); # Done
  CreateIntensityStats(); # Done
  CreatePCA(); # Done
  CreatePCA3D(); # Done
  createSpectraSumDoc(); # Done
  createSpectraTIC(); # Done
  
};
CreateTIC<- function(){
  
  descr <- c("\\subsection{Total Ion Chromatogram (TIC)}\n",
             "TIC is a chromatogram summed the intensities of all mass spectral peaks from same scan.",
             "All signals including noise and peak components are included in TIC", 
             "In other words, TIC is a sum of all the separate ion contributing to a mass spectrum",
             "The spectra TIC signal (ion) flow of all files is displayed in Figure", fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  cmdhist <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=0.75\\textwidth]{TICS_72.png}", sep=""),
                "\\caption{Total Ion Chromatogram plot of the spectral processing.}",
                "\\end{center}",
                "\\end{figure}",
                "\\clearpage"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
CreateIntensityStats<- function(){
  
  descr <- c("\\subsection{Peak Intensity Statistics}\n",
             "The general peaks' intensity is analyzed from different spectral files to show the peaks' intensity distribution",
             "The statistics all spectral peaks is displayed in Figure", fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  cmdhist <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=0.75\\textwidth]{Peak_Intensity.png}", sep=""),
                "\\caption{Peak Intensity Statistics of all spectral files.}",
                "\\end{center}",
                "\\end{figure}",
                "\\clearpage"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
CreatePCA<- function(){
  
  descr <- c("\\subsection{Principal component analysis (PCA)}\n",
             "PCA is a dimension-reduction method used to increase the interpretability of high-dimension datasets and minimizing the information loss.", 
             "Here a primary PCA was performed with the log-transformed data.",
             "The PCA score plot is shown in ", fig.count<<-fig.count+1,", as below. Please try to play your data with different modules (e.g. Statistic Analysis) to find out more statistic sense."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  cmdhist <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=0.75\\textwidth]{PCA.png}", sep=""),
                "\\caption{Principal component analysis (PCA). Samples from different groups are marked with different colors.}",
                "\\end{center}",
                "\\end{figure}",
                "\\clearpage"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
CreatePCA3D<- function(){
  
  descr <- c("\\subsection{Principal component analysis (PCA)}\n",
             "PCA could also be shown in 3-dimensional style", 
             "Here a primary 3D-PCA was performed with the log-transformed data.",
             "The 3D-PCA score plot is shown in ", fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  if(file.exists("scores3D.png")){
    cmdhist <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.75\\textwidth]{scores3D.png}", sep=""),
                  "\\caption{3D-PCA score. Samples from different groups are marked with different colors.}",
                  "\\end{center}",
                  "\\end{figure}",
                  "\\clearpage"
    );
  } else {
    descr3 <- c("\n\nNo 3D-PCA Score is presented. Please try to explore from result page.");
    cat(descr3, file=rnwFile, append=TRUE);
    cat("\n", file=rnwFile, append=TRUE);
  }

  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
  ## Loading plot
  descr <- c("\\subsection{Principal component analysis (PCA)}\n",
             "3D-PCA loading plot is showing the features as contributional loading factors for the difference among samples", 
             "Here a primary 3D-PCA loading was performed with the log-transformed data.",
             "The 3D-PCA loading plot is shown in ", fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  if(file.exists("loadings3D.png")){
    cmdhist <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.75\\textwidth]{loadings3D.png}", sep=""),
                  "\\caption{3D-PCA loading plot. Samples from different groups are marked with different colors.}",
                  "\\end{center}",
                  "\\end{figure}",
                  "\\clearpage"
    );
  } else {
    descr3 <- c("\n\nNo 3D-PCA Loading is presented. Please try to explore from result page.");
    cat(descr3, file=rnwFile, append=TRUE);
    cat("\n", file=rnwFile, append=TRUE);
  }
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
createSpectraSumDoc<- function(){
  
  descr <- c("\\subsection{Spectra Summary}\n",
             "The peaks information from different spectra after processing is summarized in Table ", 
             table.count<<-table.count+1,", as below."
  );
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "createSpectraSumTable()",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
createSpectraSumTable <- function(){
  if(file.exists("peak_result_summary.txt")){
    dt <- read.table("peak_result_summary.txt")
    colnames(dt)<-c("Spectra","Groups", " RT range", "m/z Range", "Peak Number", "Missing (%)");
    print(xtable::xtable(dt, 
                         caption="Summary of peaks information of all spectra after processing"), 
          caption.placement="top", 
          size="\\scriptsize");
  }
};
CreateBPI<- function(){
  
  descr <- c("\\subsection{Base Peak Ion (BPI) Chromatogram}\n",
             "The base peak chromatogram is similar to the TIC, however it only shows the intensity of the most intense signal at every scan across the whole spectrum.", 
             "Base peak chromatograms always have a cleaner and clearer shape and thus are more informative than TIC.",
             "The spectra signal (ion) flow of all files is displayed in Figure", fig.count<<-fig.count+1,", as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  cmdhist <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=0.75\\textwidth]{BPIS_72.png}", sep=""),
                "\\caption{Base Peak Ion plot of the spectral processing.}",
                "\\end{center}",
                "\\end{figure}",
                "\\clearpage"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
};
createSpectraTIC <- function(){
  
  descr <- c("\\subsection{TIC of individual spectra}\n",
             "The TIC plots you are interested in are shown as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  load("mSet.rda");
  files <- mSet@rawOnDisk@phenoData@data[["sample_name"]];
  
  if(!any(file.exists(paste0(files, ".png")))){
    
    descr3 <- c("\n\nNo Spectral TIC was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rnwFile, append=TRUE);
    cat("\n", file=rnwFile, append=TRUE);
    
  }
  
  for (i in files){
    if(file.exists(paste0(i, ".png"))){
      fig.count<<-fig.count+1;
      cmdhist <- c( "\\begin{figure}[htp]",
                    "\\begin{center}",
                    paste("\\includegraphics[width=0.75\\textwidth]{", paste0(i, ".png"), "}", sep=""),
                    paste0("\\caption{TIC plot of this spectra: ", gsub("_","-",sub(".png","",i)) ,".}"),
                    "\\end{center}",
                    "\\end{figure}",
                    "\\clearpage"
      );
      cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
    }
  }

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
};

### Section 4 - Processing feature
CreateFeatureSum <- function(){
  descr <- c("\\section{Raw Spectra Processing - Feature summary}\n",
             "All spectra files included for processing in this module have been processed.",
             "All features processing result across the different spectra are shown as below, including peak feature summary,
              and the corresponding Extracted Ion Chromatogram (EIC/XIC) of the features you are interested in. ");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr1 <- c(
    "Here is a brief content of this section",
    "\\begin{itemize}",
    "\\item{EIC/XIC}",
    "\\item{Feature (EIC/XIC) Stats}",
    #"\\item{Peak Feature Detail}",
    "\\end{itemize}")
  
  cat(descr1, file=rnwFile, append=TRUE);
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");

  createFeatureEIC(); # Done
  createFeatureEICStats(); # Done
  createFeatureSumDoc(); # Done
  createFeatureAnnoSum(); # Done
  # Adding more and do some optimization in future
  
};
createFeatureEIC <- function(){
  
  descr <- c("\\subsection{EIC/XIC of individual feature}\n",
             "The ones you are interested in at the analyzing stage are shown as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);

  #files <- list.files(pattern = "^EIC.*mz@")
  files <- list.files(pattern = "^EIC_.*group")
  
  if(length(files) == 0){
    
    descr3 <- c("\n\nNo features' EIC was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rnwFile, append=TRUE);
    cat("\n", file=rnwFile, append=TRUE);
    
  }
  
  for (i in files){
    if(file.exists(i)){
      
      #if(grepl("s_sample_", i)){
        fig.count<<-fig.count+1;
        cmdhist <- c( "\\begin{figure}[htp]",
                      "\\begin{center}",
                      paste("\\includegraphics[width=0.75\\textwidth]{", i, "}", sep=""),
                      paste0("\\caption{EIC of feature of individual samples: ", gsub("_","-",sub(".png","",i)),"}"),
                      "\\end{center}",
                      "\\end{figure}",
                      "\\clearpage"
        )
        cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
      #}
      
      # if(grepl("s_group_", i)){
      #   fig.count<<-fig.count+1;
      #   cmdhist <- c( "\\begin{figure}[htp]",
      #                 "\\begin{center}",
      #                 paste("\\includegraphics[width=0.75\\textwidth]{", i, "}", sep=""),
      #                 paste0("\\caption{EIC of feature of groups: ", gsub("_","-",sub(".png","",i)),"}"),
      #                 "\\end{center}",
      #                 "\\end{figure}",
      #                 "\\clearpage"
      #   )
      #   cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
      # }
      
      cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
    }
  }
 
};
createFeatureEICStats <- function(){
  
  descr <- c("\\subsection{EIC/XIC Stats of individual feature}\n",
             "The ones you are interested in at the analyzing stage are shown as below."
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n", file=rnwFile, append=TRUE);
  
  files <- list.files(pattern = "^[^(EIC)].*mz@.*s*.png")
  
  if(length(files) == 0){
    
    descr3 <- c("\n\nNo features' EIC stats was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rnwFile, append=TRUE);
    cat("\n", file=rnwFile, append=TRUE);
    
  }
  
  for (i in files){
    if(file.exists(i)){
      if(grepl("mz@", i)){
        fig.count<<-fig.count+1;
        cmdhist <- c( "\\begin{figure}[htp]",
                      "\\begin{center}",
                      paste("\\includegraphics[width=0.75\\textwidth]{", i, "}", sep=""),
                      paste0("\\caption{Feature intensity statis box plot of different groups: ", 
                             gsub("_","-",sub(".png","",i)),"}"),
                      "\\end{center}",
                      "\\end{figure}",
                      "\\clearpage"
        )
        cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
        #cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
      }
    }
  }
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
};
createFeatureSumDoc <- function(){
  
  descr <- c("\\subsection{Feature Annotation Summary}\n",
             "The features basic information and its annotation results after processing is summarized in as below."
  );
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr2 <- createResSumText();
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
};
createResSumText <- function(){
  txts <- PerformResultSummary();
  txts[5] <- gsub("\\%", "\\\\%", txts[5])
  txts[6] <- gsub("\\%", "\\\\%", txts[6])
  return(paste0(txts, sep = "\n"))
}
createFeatureAnnoSum <- function() {
  descr <- c("\\subsection{Feature Annotation Results}\n",
             "The features annotation (adducts and isotopes) is summarized in as below in Table ", 
             table.count<<-table.count+1, "."
  );
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "createFeatureAnnotationSumTable()",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
  descr2 <- c("\\subsection{Compound Putative Annotation Results}\n",
             "The features putative annotation (compound) is summarized in as below in Table ",
             table.count<<-table.count+1, "."
  );
  cat(descr2, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist3 <- c("<<echo=false, results=tex>>=",
                "createFeatureCompoundSumTable()",
                "@");
  cat(cmdhist3, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}
createFeatureAnnotationSumTable <- function(){
  
  if(file.exists("peak_feature_summary.csv")){
    dt <- read.csv("peak_feature_summary.csv");
    dt <- dt[(dt$adduct !="" | dt$isotopes != ""), c(1:4)]
    rownames(dt) <- NULL
    print(xtable::xtable(dt,
                         caption="Summary of features annotation (adduct and isotope level)"),
          caption.placement="top",
          size="\\scriptsize",tabular.environment = "longtable", floating = FALSE, right = TRUE);
  }
  
}

createFeatureCompoundSumTable <- function(){
  
  if(file.exists("peak_feature_summary.csv")){
    dt <- read.csv("peak_feature_summary.csv");
    dt <- dt[(dt$Formula!= "" & dt$Compound!=""), c(1:2, 5:6)]
    dt$Compound <- gsub(";", "\n\n", dt$Compound)
    #dt <- dt[1:5,]
    rownames(dt) <- NULL
    
    print(xtable::xtable(dt,
                         caption="Summary of features annotation (putative compound level)",
                         align= c("p{0.02\\textwidth}",
                                  "p{0.1\\textwidth}", "p{0.1\\textwidth}", 
                                  "p{0.25\\textwidth}|", "p{0.35\\textwidth}")),
          caption.placement="top",tabular.environment = "longtable",
          floating = FALSE, 
          );
  }
  
}

