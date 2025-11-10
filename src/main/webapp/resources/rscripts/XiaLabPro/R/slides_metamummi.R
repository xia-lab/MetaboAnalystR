#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateMetaPathRnwReport_slides<-function(mSetObj, usrName){

  CreateHeader(usrName);
  CreateMummichogMetaAnalReport_slides(mSetObj)
  AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
  
}

#'Create analysis report: Functional Meta-Analysis
#'@description Report generation using Sweave
#'Functional Meta-Analysis Report 
#'@param mSetObj mSetObj
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogMetaAnalReport_slides<-function(mSetObj){
  CreateMetaMummichogIntro_slides();
  CreateMetaMummichogInputDoc_slides(mSetObj);
  CreateMetaMummichogResults_slides(mSetObj);
  CreateUpSetDoc_slides(mSetObj);
  CreateKeggNetDocMetaMummi_slides(mSetObj);
  createEnrichmentTable(mSetObj, "keggGlobal")
}

#'Create analysis report: Functional Meta-Analysis Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogIntro_slides <- function() {
    # Initialize slide and figure counters if they don't exist
    if(!exists("table.count")) {
        table.count <<- 0
    }
    if(!exists("fig.count")) {
        fig.count <<- 0
    }
    
    # Create the introduction slides
    introContent <- c(
        "## Selected Module: Functional Meta-analysis\n\n",
        "- We utilize **functional meta-analysis** to integrate multiple untargeted metabolomics datasets.",
        "- This approach identifies pathway activities based on putatively identified compounds, suitable for untargeted LC-MS data.\n",
        "- The goal is to identify robust pathway-level changes across studies, enhancing insights into metabolic mechanisms.\n",
        "\n\n---\n\n"
    )
    
    # Write the content to the R Markdown file for slides
    cat(introContent, file = rmdFile, append = TRUE, sep = "\n")
}


#'Create analysis report: Functional Meta-Analysis Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogInputDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    # Initialize slide content for data processing
    dataProcessingContent <- c(
        "## Functional Meta-Analysis: Data Processing\n\n",
        "- Accepts multiple peak tables (.csv format).",
        "- Can include or exclude retention time information.",
        "- Ensures consistency across datasets for accurate analysis.\n",
        "\n\n---\n\n"
    )
    #cat(dataProcessingContent, file = rmdFile, append = TRUE, sep = "\n")
    
    # Iterate through dataset sanity checks and prepare content
    dataNMs <- names(mSetObj)[grepl("MetaData", names(mSetObj))]
    sanity_msg <- list()

  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  if(detectPairedElements(dataNMs)){

  for (i in dataNMs) {
        parts <- split_vector_at_separator(mSetObj[[i]]$check.msg, "----------------------------------")
        formatted_msg <- "";
        if (grepl("_2$", i)) {
            second_part <- gsub(" \\* ", "", parts[[2]]) # Remove leading asterisks and spaces
            formatted_msg <- second_part
        }else{
            first_part <- gsub(" \\* ", "", parts[[1]]) # Remove leading asterisks and spaces
            formatted_msg <- first_part
        }
        sanity_msg[[i]] <- paste0("## Data Check for ", gsub("_", "", mSetObj[[i]]$name), ":\n\n", paste0("* ", formatted_msg, collapse = "\n"), "\n\n---\n\n")
            cat(sanity_msg[[i]], file = rmdFile, append = TRUE)
        }

  }else{
    for(i in dataNMs) {
        formatted_messages <- gsub(" \\* ", "", mSetObj[[i]]$check.msg)  # Remove leading bullet points from messages if they exist.
        sanity_msg[[i]] <- paste0("## Data Check for ", gsub("_", "", mSetObj[[i]]$name), ":\n\n", paste0("* ", formatted_messages, collapse = "\n"), "\n\n---\n\n")
            cat(sanity_msg[[i]], file = rmdFile, append = TRUE)

    }
}
    
    # Parameters and Method Selection
    if(!is.na(mSetObj$paramSet$version)) {

        # Add method-specific details based on selected analysis type
        alg <- mSetObj[["paramSet"]][["anal.type"]]
        if(alg == "mummichog"){
            algReadable <- "Mummichog";
        }else{
            algReadable <- "GSEA";
        }
        if (mSetObj$metaLevel == "pathway" && !is.null(mSetObj$meta_results)) {
            slideContent <- c(
            "## Functional Meta-Analysis: Parameters\n\n",
                "- Algorithm: `", algReadable, "`.\n",
                "- Method: `Pathway-Level Integration`.",
                "- P-value integrating method: `", mSetObj$meta.pval.method, "`.\n",
                "\n\n---\n\n"
            )
        } else {
            slideContent <- c(
            "## Functional Meta-Analysis: Parameters\n\n",
                "- Algorithm: `", algReadable, "`.\n",
                "- Method: `Pooled Peaks`.",
                "- P-value cutoff: `", mSetObj$pooled_cutoff, "`.\n",
                "\n\n---\n\n"
            )
        }

        cat(slideContent, file = rmdFile, append = TRUE, sep = "\n")

    }
}


#'Create analysis report: Functional Meta-Analysis Results
#'@description Report generation using Sweave
#'Mummichog analysis report overview
#'@param mSetObj mSetObj
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogResults_slides <- function(mSetObj){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # Check the analysis level
  if(mSetObj$metaLevel == "pathway"){
    
    # Check if meta-analysis results exist
    if(!is.null("mSetObj$meta_results")){
      introContent <- c(
          "## Integrated Metabolic Pathway Analysis Results\n\n",
          "- Aim: Improve biological consistency across studies.",
          "- Method: Pathway-Level Integration.",
          "- Process: Calculates m/z level statistics, putative metabolite annotation, followed by pathway activity prediction.",
          "- Goal: Create a unified matrix of pathway-level results for robust meta-signature identification.\n\n",
          "---\n\n"
      )
      cat(introContent, file = rmdFile, append = TRUE, sep = "\n")
    }
    
    # Check if there are pathway integration plot results
    if(!is.null(mSetObj$imgSet$mummi.meta.path.plot)) {
      
      # Create a slide for the bubble plot
      bubblePlotSlide <- CreateTitleFigureSlide(mSetObj$imgSet$mummi.meta.path.plot, "Summary of Pathway-Level Integration Meta-Analysis")
      cat(bubblePlotSlide, file = rmdFile, append = TRUE)

      # Results Slide
      table.count <<- table.count+1;
      cat(paste0("## Table ", 
                 table.count, 
                 ". Results of the Pathway-Level Integration Meta-Analysis."), file=rmdFile, append=TRUE, sep="\n");

        cmdhist2 <- c(
          "```{r table_mmm1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt <- CreateMummichogMetaAnalPathTableRMD(mSet);",
          paste0("create_dt(sum_dt)"),
          "```", 
          "\n\n---\n\n"
        );
      
      cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    }
  } else {
    # Pooled Peaks Results Slide
    poolingPeaksContent <- c(
        "## Pooled Peaks Method Results\n\n",
        "- Aim: Combine outputs from different instruments measuring the same samples.",
        "- Applicable when samples are homogeneous but instruments are complementary.",
        "- Processes all uploaded peaks for annotation and pathway activity prediction.\n\n",
        "---\n\n"
    )
    cat(poolingPeaksContent, file = rmdFile, append = TRUE, sep = "\n")
    
    # Insert the specific mummichog analysis results for pooled peaks
    CreateMummichogAnalysisDoc_slides(mSetObj)  # Assume there is a corresponding slides version of the function
  }
}


#'Create analysis report: Functional Meta-Analysis Results Table
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogMetaAnalPathTableRMD <- function(mSetObj){
  mummitable <- mSetObj$meta_results;
  return(mummitable)
}

####################################################
####################################################

#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogAnalysisDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj);

    if (!is.null(mSetObj$mummi.resmat)) {
        # Slide for Mummichog Analysis Introduction
        introContent <- c(
            "## Mummichog Pathway Analysis\n\n",
            "- Provides functional insights directly from m/z features.",
            "- Steps include permutations, calculation of p-values, and pathway mapping.",
            "- Results offer a comprehensive understanding of metabolic models/pathways.\n\n",
            "---\n\n"
        )
        cat(introContent, file = rmdFile, append = TRUE, sep = "\n")

        # Slide for Mummichog Pathway Analysis Plot
        if (!is.null(mSetObj$imgSet$mummi.plot)) {
            
            # Create a slide for the pathway analysis plot
            pathwayPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$mummi.plot, "Summary of Mummichog Pathway Analysis")
            cat(pathwayPlotSlide, file = rmdFile, append = TRUE)
        }

        # GSEA Analysis Slide (if applicable)
        if (!is.null(mSetObj$mummi.gsea.resmat)) {

            # Create a slide for the GSEA pathway analysis plot
            if (!is.null(mSetObj$imgSet$mummi.gsea.plot)) {
                gseaPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$mummi.gsea.plot, "Summary of GSEA Pathway Analysis")
                cat(gseaPlotSlide, file = rmdFile, append = TRUE)
            }
        }

        # Meta-Analysis Results Slide (if applicable)
        if (!is.null(mSetObj$integ.resmat)) {
            # Create a slide for the integrated pathway analysis plot
            if (!is.null(mSetObj$imgSet$integpks.plot)) {
                integPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$integpks.plot, "Summary of Integrated Pathway Analysis")
                cat(integPlotSlide, file = rmdFile, append = TRUE)
            }
        }
    }
}

split_vector_at_separator <- function(vector, separator) {
  # Find the index of the separator
  sep_index <- which(vector == separator)
  
  # Split the vector into two parts
  first_part <- vector[1:(sep_index - 1)]
  second_part <- vector[(sep_index + 1):length(vector)]
  
  # Return the two parts as a list
  return(list(first_part = first_part, second_part = second_part))
}

CreateKeggNetDocMetaMummi_slides <-function(mSetObj){
  
  if(!is.null(mSetObj$imgSet$reportSet$network_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$network_mummichog)){
    figureContent <- CreateTitleFigureSlide(mSetObj$imgSet$reportSet$network_mummichog, 
                                            paste0("KEGG Global Metabolic Network Visualization"))
    cat(figureContent, file = rmdFile, append = TRUE, sep = "\n")
    networkImageSlide <- c("\n\n---\n\n")
    cat(networkImageSlide, file = rmdFile, append = TRUE, sep = "\n")
  }
}