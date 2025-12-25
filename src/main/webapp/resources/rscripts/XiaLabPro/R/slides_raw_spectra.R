#'Create report for raw spectra module
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateRawAnalysisRnwReport_slides <- function(mSetObj, usrName){
  
  load("mSet.rda");
  # Creat the header & introduction of the report
  CreateHeader(usrName); # done
  
    CreateRawIntr_slides()  # 
    CreateSpectraIOdoc_slides()  # 
    CreateRawAnalMethod_slides()  # 
    CreateRawAnalyworkflow_slides()  #
    CreateRawAnalDetails_slides()  # 
    CreateSampleSum_slides()  # 
    CreateFeatureSum_slides()  #

  
  #CreateRHistAppendix(); # done
  CreateFooter(); # done
  
}


### Section 1 - Background
CreateRawIntr_slides <- function() {
  # Ensure global variables for table and figure counts exist, initialize if not
  if (!exists("table.count")) {
    table.count <<- 0  # Using global assignment operator <<- to modify global environment
  }
  if (!exists("fig.count")) {
    fig.count <<- 0  # Similar global assignment for figure count
  }
  
  # Preparing the introduction text for the raw spectra processing section
  descr0 <- c("\n\n## Selected Module: Raw Spectra Processing\n\n");
  
  # Combining all parts of the description
  descr <- c(descr0, 
             "This module is designed to provide an automated workflow to process the raw spectra in six steps, including ",
             "reading and processing raw spectra, parameter optimization/customization, ",
             "peak picking, peak alignment, peak gap filling, and peak annotation.")
  
  # Writing the combined text to the specified R Markdown file
  .buffer_add(descr)  # Ensure the append flag is TRUE to add content without overwriting existing data
  .buffer_add("\n\n---\n\n") 
}


### Section 2 - Method Description
CreateSpectraIOdoc_slides <- function() {
  # Description text for the raw spectra data reading and centroiding section
  descr <- c("\n\n## Raw Spectra Data Reading and Centroiding\n\n",
             "The MetaboAnalyst MS Spectral Processing Module accepts several common open MS formats ",
             "including mzXML, mzML, mzData, and CDF formats. All of them have to be centroided before processing. ",
             "A Data Integrity Check is performed before the data processing starts. Basic information ",
             "of all spectra is summarized in Table ", 
             table.count <<- table.count + 1,  # Increment table.count and use it in the text
             ", which shows the details of all spectra.")
  
  # Writing the description to the R Markdown file
  #.buffer_add(descr)
  .buffer_add("\n\n")  # Add extra line breaks for better formatting
  
  # R code for creating and displaying the table of raw spectra data
  .buffer_add(paste0("## Table ", getTableCount(), ". Summary of raw spectra data files. \n\n"));
  cmdhist2 <- c(
    "```{r table_rawt1, echo=FALSE, out.width='100%', results='asis', out.height='100%', warning=FALSE}",
    "sum_dt <- CreateSpectraInfoTableRMD();",  # Assuming this is a user-defined function to create a data table
    paste0("create_dt(sum_dt)"),
    "```", "\n\n---\n\n")
  
  # Writing the R code to the R Markdown file
  .buffer_add(cmdhist2, collapse="\n")
  .buffer_add("\n\n")  # Add extra line breaks for spacing
}

CreateRawAnalMethod_slides <- function() {
  # Section header and introduction to spectral processing parameters
  descr <- c("\n\n## Raw Spectral Processing Parameters\n\n",
             "MetaboAnalyst offers several algorithms to process spectral raw files, including MatchedFilter, centWave, ",
             "and Massifquant for peak picking, and obiwarp and loess for retention time alignment. ",
             "Below are the detailed algorithms and parameters used in this study.")
  # Write the introduction to the R Markdown file
  .buffer_add(descr, collapse="\n")
  .buffer_add("\n\n---\n\n", collapse="\n")
  # Increment the global table counter
  
  # R code to create and display the table summarizing all parameters used in the analysis
  .buffer_add(paste0("## Table ", getTableCount(), ". Summary of all parameters used in this analysis."), collapse = "\n");
  cmdhist2 <- c(
    "```{r table_raw2, echo=FALSE, out.width='100%', results='asis', out.height='100%', warning=FALSE}",
    "sum_dt <- CreateParamsSumTableRMD();",  # This function should return a summary data table of parameters
    paste0("create_dt(sum_dt)"),
    "```", "\n\n---\n\n")
  # Write the R code block for the table to the R Markdown file
  .buffer_add(cmdhist2, collapse="\n")
}

CreateRawAnalyworkflow_slides <- function() {
  # This function generates a slide section to describe the whole peak alignment and annotation workflow succinctly
  descrSlides <- c("\n\n## Peak Alignment and Annotation Workflow Summary\n\n",
                   "- **Peak Grouping**: Utilizes density distribution of MS peaks from the xcms package. Groups peaks based on retention time proximity.",
                   "- **Retention Time Alignment**: Applies LOESS (default) or Obiwarp to align retention times across samples, forming a coherent feature matrix.",
                   "- **Gap Refilling**: Identifies and fills missing peaks (NAs) by re-extracting MS scans, aiming to recover weak but relevant features.",
                   "- **Peak Annotation**: Reduces redundancy by annotating isotopes and adducts using the CAMERA package. Enhances data clarity and utility.",
                   "- **Chemical Identification**: Predicts formulas based on ppm, isotopes, and adducts, then matches against the HMDB database for compound identification. \n\n");
  # Write the slide content to the R Markdown file
  .buffer_add(descrSlides, collapse="\n");
  .buffer_add("\n\n---\n\n", collapse="\n");

}


CreateRawAnalDetails_slides <- function(){
  
  descr <- c("\n\n## Raw Spectral Processing Platform\n\n",
             paste0("The spectra files are processed with OptiLCMS R package (", packageVersion("OptiLCMS"), ") 
                    based on Intel Xeon Processor equiped platform. Total of 4 cores are allocated for this task." ),
             "The customized or automated pipeline has been implemented based on your previous choice. \n\n",
             "Detailed description of the mechanism of automated pipeline has been published. Please find out more 
             introduction on the optimization process [(Pang Z. 2020)](https://www.mdpi.com/2218-1989/10/5/186).",
             "\n\n",
             "Please cite this publication if you are using this module to do your processing.");
  
  .buffer_add(descr, collapse="\n");
  .buffer_add("\n\n---\n\n", collapse="\n");

}

### Section 3 - Processing result
CreateSampleSum_slides <- function(){
  descr <- c("\n\n## Raw Spectra Processing - Result Summary\n\n",
    " - Total Ion Chromatogram (TIC). \n",
    " - Base Peak Ion (BPI) Chromatogram.\n",
    " - Spectral Intensity Stats.\n",
    " - Principal Component Analysis (PCA).\n",
    " - Spectral peaks summary.\n")
  .buffer_add(descr);
  .buffer_add("\n\n--\n\n");


  CreateTIC_slides(); # Done
  CreateBPI_slides(); # Done
  CreateIntensityStats_slides(); # Done
  CreatePCA_slides(); # Done
  CreatePCA3D_slides(); # Done
  createSpectraSumDoc_slides(); # Done
  createSpectraTIC_slides(); # Done
  
};

CreateTIC_slides <- function() {
  # Increment the figure count first
  
  # Introduction to the Total Ion Chromatogram (TIC)
  descrTICSlides <- c("\n\n---\n\n# Total Ion Chromatogram (TIC)\n\n",
                      "* Represents summed intensities of all spectral peaks per scan.",
                      "* Includes all signals: noise and peak components.");  # Use paste0 for dynamic text
  
  # Write the TIC section introduction to the R Markdown file
  #.buffer_add(descrTICSlides);
  
  # Inserting TIC figure
  figTICSlides <- CreateTitleFigureSlide("TICS_72.png", paste0("Total Ion Chromatogram plot of raw spectral data."))  # Corrected syntax
  .buffer_add(figTICSlides);
  
  
  .buffer_add("\n\n---\n\n"); # End of slide
}


CreateBPI_slides <- function() {
  
  # Description for the BPI section
  descr <- c("\n\n## Base Peak Ion (BPI) Chromatogram\n\n",
             "The base peak chromatogram is similar to the TIC, but it only shows the intensity of the most intense signal at every scan across the whole spectrum. ", 
             "Base peak chromatograms generally have a cleaner and clearer shape, making them more informative than TICs. "
             )
  
  # Writing the BPI section introduction to the R Markdown file
  #.buffer_add(descr)
  
  # Inserting the BPI figure
  basePeakContent <- CreateTitleFigureSlide("BPIS_72.png", "Base Peak Ion plot of raw spectral data.")
  .buffer_add(basePeakContent, collapse="\n")
}

CreateIntensityStats_slides <- function() {
    # Increment global figure count and store for later use
    
    # Description for the Peak Intensity Statistics section
    #descriptionText <- paste(
    #    "The general peaks' intensity is analyzed from different spectral files to show the peaks' intensity distribution.",
    #    "The statistics of all spectral peaks are displayed in Figure ", fig_ints, ", as below."
    #)
    
    # Path to the Peak Intensity Statistics image
    imagePath <- 'Peak_Intensity.png'  # Ensure this path is correct
    
    # Construct the slide with figure and description using the provided function structure
    intensityStatsSlide <- CreateTitleFigureSlide(imagePath, "Statistics of all spectral peaks")
    
    # Write the slide to the R Markdown file
    .buffer_add(intensityStatsSlide)
}

CreatePCA_slides <- function(){
  # Increment global figure count and store for later use

  pcaImagePath <- 'PCA.png'  # Ensure this is the correct path to your PCA image

  # Caption for the PCA figure
  pcaFigureCaption <- paste("Principal component analysis (PCA) of log-transformed data.")

  # Generate the two-column slide content
  pcaSlideContent <- CreateTitleFigureSlide(pcaImagePath, pcaFigureCaption)
  
  # Write the slide content to the R Markdown file
  .buffer_add(pcaSlideContent)
}

CreatePCA3D_slides <- function(){
  mSetObj <- .get.mSet(mSetObj);

  # Description for the PCA 3D score section
  descrScore <- paste("## Principal Component Analysis (PCA) - Score Plot\n\n",
                      "PCA can also be shown in a 3-dimensional style to provide additional depth to the data interpretation. ",
                      "Here, a primary 3D-PCA was performed with the log-transformed data. ",
                      "The 3D-PCA score plot illustrates the distribution of samples in the transformed feature space, highlighting the variance between different groups.\n\n")
  #.buffer_add(descrScore)
  
  if(!is.null(mSetObj$imgSet$reportSet$scores_3d) && safeFileExists(mSetObj$imgSet$reportSet$scores_3d)){
    pca3DScoreSlide <- CreateTitleFigureSlide(mSetObj$imgSet$reportSet$scores_3d, "3D-PCA score. Samples from different groups are marked with different colors.")
    .buffer_add(pca3DScoreSlide)
  } else {
    .buffer_add("No 3D-PCA Score plot is presented. Please check the data or explore from the results page.\n\n---\n\n")
  }

  # Description for the PCA 3D loading section
  descrLoading <- paste("## Principal Component Analysis (PCA) - Loading Plot\n\n",
                        "The 3D-PCA loading plot showcases the features contributing to the differentiation among samples. ",
                        "These loadings can help identify which variables carry the weight in the separation observed in the PCA score plot.\n\n")
  #.buffer_add(descrLoading)
  
  if(file.exists("loadings3D.png")) {
    pca3DLoadingSlide <- CreateTitleFigureSlide("loadings3D.png", "Figure: 3D-PCA loading plot. Features contributing to sample differentiation are highlighted.")
    .buffer_add(pca3DLoadingSlide)
  } else {
    #.buffer_add("No 3D-PCA Loading plot is presented. Please check the data or explore from the results page.\n\n---\n\n")
  }
}

createSpectraSumDoc_slides <- function(){
  # Spectra Summary description
  descr <- paste("## Spectra Summary\n\n",
                 "The information of peaks from different spectra after processing is summarized below. This includes key metrics and statistics that provide an overview of the spectral data quality and characteristics.\n\n---\n\n")
  .buffer_add(descr)
  
  # Inserting the table of spectra summary
  .buffer_add(paste0("## Table ", table.count, ": Summary of peaks information from all spectra after processing."), collapse="\n");
  cmdhist2 <- c("```{r table_rawsum3, echo=FALSE, out.width='100%', results='asis', warning=FALSE}",
                "sum_dt <- createSpectraSumTableRMD();",  # Ensure this function is correctly defined to create summary table
                paste0("create_dt(sum_dt)"),
                "```",
                "\n\n---\n\n")  # Denote the end of a section or slide
  
  .buffer_add(cmdhist2, collapse="\n")
}


createSpectraSumTableRMD <- function(){
  if(file.exists("peak_result_summary.txt")){
    dt <- read.table("peak_result_summary.txt")
    colnames(dt)<-c("Spectra","Groups", " RT range", "m/z Range", "Peak Number", "Missing (%)");
    return(dt)
  } else {
    return(data.frame("No peak results available from your analysis. Please wait raw spectral processing completed."))
  }
};

createSpectraTIC_slides <- function() {
    # Introduction to the TIC of individual spectra
    descr <- c("## TIC of Individual Spectra\n\n",
               "The Total Ion Chromatogram (TIC) plots for individual spectra are shown below. TIC provides a comprehensive view of all ions present in the sample throughout the entire analysis.\n\n")
    .buffer_add(descr)
   
    # Load the dataset
    load("mSet.rda")
    files <- mSet@rawOnDisk@phenoData@data[["sample_name"]]
    
    # Check if any TIC files have been viewed
    if(!any(file.exists(paste0(files, ".png")))) {
        descr3 <- "No Spectral TIC was viewed by you. Please try to explore from the result page.\n\n---\n\n"
        .buffer_add(descr3)
    } else {
        for (i in files) {
            if(file.exists(paste0(i, ".png"))) {
                # Increment the global figure counter
                # Prepare the caption for the TIC plot
                tic_caption <- paste("TIC plot of the spectrum: ", gsub("_", "-", sub(".png", "", i)), ".")
                # Prepare the image path
                image_path <- paste0(i, ".png")
                # Generate the two-column slide content for the TIC plot
                ticSlideContent <- CreateTitleFigureSlide(image_path, tic_caption)
                # Write the slide content to the R Markdown file
                .buffer_add(ticSlideContent)
            }
        }
    }
}


### Section 4 - Processing feature
CreateFeatureSum_slides <- function(){
  descr <- c("\n\n## Raw Spectra Processing - Feature summary\n\n",
             "All spectra files included for processing in this module have been processed.",
             "All features processing result across the different spectra are shown as below, including peak feature summary,
              and the corresponding Extracted Ion Chromatogram (EIC/XIC) of the features you are interested in. \n\n");
  .buffer_add(descr);
  
  descr1 <- c(
    "Here is a brief content of this section: \n",
    " - EIC/XIC;",
    " - Feature (EIC/XIC) Stats;",
    "\n\n---\n\n")
  
  .buffer_add(descr1);
  .buffer_add("\n\n");

  createFeatureEIC_slides(); # Done
  createFeatureEICStats_slides(); # Done
  createFeatureSumDoc_slides(); # Done
  createFeatureAnnoSum_slides(); # Done
  # Adding more and do some optimization in future
  
};

createFeatureEIC_slides <- function() {
    # Introduction to the EIC/XIC of individual features
    descr <- c("## EIC/XIC of Individual Features\n\n",
               "The Extracted Ion Chromatograms (EIC) or XIC for features of interest during the analysis stage are presented below.\n\n")
    .buffer_add(descr)
    
    # Retrieve the list of EIC files
    files <- list.files(pattern = "^EIC_.*group_[0-9]+.png$")
    
    # Check if there are EIC files to display
    if(length(files) == 0) {
        .buffer_add("No features' EIC was viewed by you. Please try to explore from the Spectra result page.\n\n---\n\n")
    } else {
        # Process and present each EIC file
        for (i in files) {
            # Increment the global figure counter
            
            # Construct the figure caption and filename
            feature_name <- gsub("_", "-", sub(".png", "", i))
            eic_caption <- paste("EIC of feature ", feature_name, " in individual samples.")
            eic_file_path <- i  # Assuming the file path is correct
            
            # Generate the two-column slide content for the EIC
            eicSlideContent <- CreateTitleFigureSlide(eic_file_path, eic_caption)
            
            # Write the slide content to the R Markdown file
            .buffer_add(eicSlideContent)
        }
    }
}

createFeatureEICStats_slides <- function() {
    # Introduction to the EIC/XIC Stats section
    descr <- c("## EIC/XIC Stats of Individual Features\n\n",
               "Statistical insights into the Extracted Ion Chromatograms (EIC) or XIC for selected features are presented below.\n\n")
    .buffer_add(descr)
    
    # Retrieve files following the specific naming pattern
    files <- list.files(pattern = "^[^(EIC)].*mz@.*s*.png$")
    
    # Check if there are EIC stats files to display
    if(length(files) == 0) {
        .buffer_add("No features' EIC stats were viewed by you. Please try to explore from the result page.\n\n---\n\n")
    } else {
        # Process and present each EIC stats file
        for (i in files) {
            if(file.exists(i)) {
                # Increment the global figure count
                
                # Extracting feature info from filename for caption
                feature_info <- gsub("@", "__", sub(".png$", "", i))  # Clean up feature info for caption
                eicStatsCaption <- paste("Feature intensity statistical box plot for ", feature_info, ".")
                
                # Generate the two-column slide content for the EIC stats
                eicStatsSlideContent <- CreateTitleFigureSlide(i, eicStatsCaption)
                
                # Write the slide content to the R Markdown file
                .buffer_add(eicStatsSlideContent)
            }
        }
    }
}

createFeatureSumDoc_slides <- function(){
  
  descr <- c("\n\n## Feature Annotation Summary\n\n",
             "The features basic information and its annotation results after processing is summarized in as below."
  );
  .buffer_add(descr);
  .buffer_add("\n\n");
  
  descr2 <- createResSumText();
  .buffer_add(descr2, collapse="\n");
  .buffer_add("\n\n---\n\n");
};

createResSumText <- function(){
  txts <- PerformResultSummary();
  return(paste0(txts, sep = "\n"))
}

createFeatureAnnoSum_slides <- function() {
    # Introduction to the Feature Annotation Results section
    descr <- c("## Table ", getTableCount(), ": Summary of the annotation of features (including adducts and isotopes)\n\n")
    .buffer_add(descr)
    
    # Dynamic creation of annotation summary table
    cmdhist2 <- c(
        "```{r table_feature_anno, echo=FALSE, out.width='100%', results='asis', warning=FALSE}",
        "sum_dt <- createFeatureAnnotationSumTableRMD()",  # Ensure this is the correct function name and it exists
        "create_dt(sum_dt)",  # Ensure create_dt function exists and is applicable
        "```",
        "\n\n---\n\n"
    )
    .buffer_add(cmdhist2, collapse="\n")  # Note the 'sep = "\n"' to ensure new lines
    
    # Introduction to the Compound Putative Annotation Results section
    descr2 <- paste0("## Table ", getTableCount(), ": Summary of feature annotations at the putative compound level.\n\n")
    .buffer_add(descr2)
    
    # Dynamic creation of compound annotation summary table
    cmdhist3 <- c(
        "```{r table_feature_compound, echo=FALSE, out.width='100%', results='asis', warning=FALSE}",
        "sum_dt <- createFeatureCompoundSumTableRMD();",  # Again, ensure correct function naming and existence
        "create_dt(sum_dt)",  # Check for create_dt functionality
        "```",
        "\n\n---\n\n"
    )
    .buffer_add(cmdhist3, collapse="\n")  # Using 'sep = "\n"' for proper line breaks
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
