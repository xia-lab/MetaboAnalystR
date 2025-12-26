
if (!exists("safeFileExists", mode = "function")) {
  safeFileExists <- function(path) {
    if (is.null(path) || length(path) != 1 || !is.character(path)) {
      return(FALSE)
    }
    if (is.na(path) || path == "") {
      return(FALSE)
    }
    file.exists(path)
  }
}

if (!exists("safeIncludeGraphics", mode = "function")) {
  safeIncludeGraphics <- function(path) {
    if (!safeFileExists(path)) {
      return(NULL)
    }
    knitr::include_graphics(path)
  }
}


#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateMummichogRmdReport_slides<-function(mSetObj, usrName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateMummichogIntro_slides();
  #CreateMummichogOverview();
  
  # peak intensity table
  if(mSetObj$dataSet$type == "mass_table"){
    CreateDataProcSlides(mSetObj);
    CreateNORMSlides(mSetObj);
    
  }else{ # peak list
    CreateMummichogInputDoc_slides(mSetObj);
  }
  
  #CreateMummichogAnalysisIntroDoc_slides();
  CreateAnalCustomization_slides();
  CreateMumAnalDoc_slides();
  CreateGseaAnalDoc_slides();
  CreateIntegAnalDoc_slides();
  CreateMumHeatmap_slides();
  AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}


####################################################
####################################################

#'Create mummichog analysis report: Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'@author XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogIntro_slides <- function() {
  # Overview slide for Mummichog Functional Analysis
  introContent <- c(
    "## Functional Analysis: MS Peaks to Pathways\n\n",
    "Designed for functional analysis directly from untargeted metabolomics data based on LC-HRMS platforms.\n",
    "- Input: ranked peak lists or peak intensity table from high-resolution MS peaks (w/wo RT)",
    "- Supporting mummichog algorithm (based on ORA) and an adapted GSEA method.",
    "- Comprehensive knowledge base covering 128 organisms and 10 metabolite set libraries.",
    "- Interactive visual exploration of results via metabolic networks and heatmaps.\n",
    "---\n\n"
  )
  .buffer_add(introContent, collapse="\n")
}



#'Create Mummichog analysis report: Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogInputDoc_slides <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)  # Ensure we have the current mSetObj
  
  # Check for existing global variables
  if (!exists("fig.count")) {
    fig.count <<- 0
  }
  if (!exists("table.count")) {
    table.count <<- 0
  }
  
  # Create the slide content
  slideContent <- c(
    "## Data Processing for Functional Analysis\n\n",
    "- **Data Input Formats:**",
    "   - Three column table: m/z features, p-values, and statistical scores.",
    "   - Two column table: m/z features with p-values or t-scores.",
    "   - One column table: ranked by p-values or t-scores.",
    "- **Additional Parameters:** Specify mass accuracy (ppm), ion mode, and p-value cutoff.",
    "- **Instrument Compatibility:** Best suited for high-resolution MS instruments (e.g., Orbitrap, FT-MS).\n",
    "---\n\n"
  )
  
  # Include the user's data input parameters
  checkMsgs <- paste("*", mSetObj$msgSet$check.msg)
  checkMsgs <- paste0(checkMsgs, collapse = "\n")
  
  dataInputParams <- c(
    "## Data Input Parameters:\n\n",
    checkMsgs, "\n",
    "---\n\n"
  )
  
  # Combine all parts and write to the Rmd file
  .buffer_add(slideContent, collapse="\n")
  .buffer_add(dataInputParams, collapse="\n")
}


#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author XiaLab Analytics
#'All rights reserved
#'@export

CreateMummichogAnalysisIntroDoc_slides<-function(){
  
  mSetObj <- .get.mSet(mSetObj);
  link <- GetSharingLink(mSetObj);
  
  descr <- c("<hr/>",
             "## Parameter Settings\n\n",
             " - Enrichment algorithm: ```", mSetObj$dataSet$cutoff , "```. \n",
             " - Selected p-value cutoff: ```", mSetObj$dataSet$cutoff , "``` (only for mummuchog) \n.",
             " - Visualization method : ```", mSetObj$dataSet$cutoff , "```. \n",
             " - Functional library: ```", gsub("_", ".", mSetObj$lib.organism), "```.\n",
             "Advanced users can also adjust default values for currency metabolites and adducts.",
             "\n\n");
  .buffer_add(descr, collapse="\n");
}


CreateAnalCustomization_slides <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  if (!is.null(mSetObj$curr.cust) && mSetObj$curr.cust) {
    # Slide content for analysis customization
    
    # Currency Metabolites Customization
    if (exists("currency")) {
      currencyMetabolitesContent <- c(
        "### Currency Metabolites\n",
        "- **Definition:** Ubiquitous compounds like water and carbon dioxide, involved in many reactions.",
        "- **Default action:** Removed from analysis to avoid bias.",
        "- **User selected:** ", currency,
        "---\n\n"
      )
      .buffer_add(currencyMetabolitesContent, collapse="\n")
      
    }
  }
}
  
  
  CreateMumAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    link <- GetSharingLink(mSetObj)
    
    # Slide for Mummichog Pathway Analysis
    if (!is.null(mSetObj$mummi.resmat) && !is.null(mSetObj$imgSet$mummi.plot)) {
      mummichogSlideContent <- c(
        "## Mummichog Pathway Analysis\n\n",
        "- **Steps of Analysis:**",
        "   1. Permutations and mapping of m/z features to metabolic pathways.",
        "   2. Calculation of a null distribution of p-values for pathways.",
        "   3. Adjustment of p-values based on permutations.\n",
        "- **Result Summary:** Pathways are displayed as circles varying in color and size based on p-value and enrichment factor.",
        "- **Figure Summary:** Showcases significant pathways impacted in the uploaded data set.\n",
        "---\n\n"
      )
      .buffer_add(mummichogSlideContent, collapse="\n")
      
      figureContent <- CreateTitleFigureSlide(mSetObj$imgSet$mummi.plot, paste0("Pathway analysis using Mummichog using `", mSetObj$paramSet$mummi.lib ,"` p-value threshold: `", mSetObj$dataSet$cutoff, "`"))
      .buffer_add(figureContent, collapse="\n")
           
        .buffer_add("## Table ", getTableCount() , " Mummichog Result table")
        .buffer_add("\n\n")

        # Assemble and write the table content
        tableContent <- c(
          "```{r table_mm, echo=FALSE, out.width = '100%', results='asis', warning=FALSE}",
          "dt_res <- as.data.frame(CreateMummichogAnalTable(mSet));", 
          "create_dt(dt_res)",  # Assuming this function exists and requires no parameters
          "```",
          "\n\n"
        )
        .buffer_add(tableContent, collapse="\n")
    }
    # Slide for Network Visualization
    if(!is.null(mSetObj$imgSet$reportSet$network_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$network_mummichog)){
      figureContent <- CreateTitleFigureSlide(mSetObj$imgSet$reportSet$network_mummichog, paste0("Interactive network visualization by Mummichog."))
      .buffer_add(figureContent, collapse="\n")
    }
  }
  
  CreateGseaAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    link <- GetSharingLink(mSetObj)
    
    # Slide for GSEA Pathway Analysis
    if (!is.null(mSetObj$mummi.gsea.resmat) && !is.null(mSetObj$imgSet$mummi.gsea.plot)) {
      gseaSlideContent <- c(
        "## GSEA Pathway Analysis\n\n",
        "- **Analysis Approach:** Gene Set Enrichment Analysis (GSEA) evaluates the distribution of pathway-related genes across a ranked list, identifying pathways significantly represented at the extremes.",
        "- **Normalization:** Utilizes Normalized Enrichment Score (NES) to compare results across different gene sets.\n",
        "- **Results Interpretation:** Pathways displayed as circles vary by color and size based on their enrichment significance and factor.\n",
        "---\n\n"
      )
      .buffer_add(gseaSlideContent, collapse="\n")
      
      # Include figure and table
      figureContent <- CreateTitleFigureSlide(mSetObj$imgSet$mummi.gsea.plot, paste0("Summary of GSEA Pathway Analysis using `", mSetObj$paramSet$gsea.lib, "`"))
      .buffer_add(figureContent, collapse="\n")
      
      .buffer_add(paste0("## Table ", getTableCount(), ". Results of the GSEA Pathway Analysis."))
      .buffer_add("\n\n")

      # Table of GSEA Results
      resultsTableContent <- paste0(
        "```{r table_gsea, echo=FALSE, results='asis'}\n",
        "dt_res <- as.data.frame(CreateGSEAAnalTable(mSetObj))\n",
        "create_dt(dt_res)\n",
        "```\n",
        "---\n\n"
      )
      .buffer_add(resultsTableContent)
    }
    
    # Slide for Network Visualization
    if(!is.null(mSetObj$imgSet$reportSet$network_gsea) && safeFileExists(mSetObj$imgSet$reportSet$network_gsea)){
      networkVisualizationContent <- CreateTitleFigureSlide(mSetObj$imgSet$reportSet$network_gsea, "Interactive network visualization by GSEA.")
      .buffer_add(networkVisualizationContent, collapse="\n")
    }
  }
  
  CreateIntegAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    link <- GetSharingLink(mSetObj)
    
    # Slide for Integration of Mummichog and GSEA Results
    if (!is.null(mSetObj$integ.resmat)) {
      integSlideContent <- c(
        "## Integration of Mummichog and GSEA Results\n\n",
        "- **Integration Approach:** Uses Fisher's method to combine p-values from Mummichog and GSEA for each pathway.",
        "- **Visualization:** Highlights pathways with combined p-values indicating significant perturbation.\n",
        "---\n\n"
      )
      .buffer_add(integSlideContent, collapse="\n")
      
      figureContent <- CreateTitleFigureSlide(mSetObj$imgSet$integpks.plot, paste0("Summary of integrated pathway analysis combining Mummichog and GSEA using `",mSetObj$paramSet$integ.lib,"` and p-value threshold `",mSetObj$dataSet$cutoff ,"`"))
      .buffer_add(figureContent, collapse="\n")
      
      .buffer_add(paste0("## Table ", getTableCount(), ". Meta-Analysis of Mummichog and GSEA Results."))
      .buffer_add("\n\n")

      # Table of Integrated Results
      resultsTableContent <- paste0(
        "```{r table_integ, echo=FALSE, results='asis'}\n",
        "dt_res <- as.data.frame(CreateMetaAnalTable(mSetObj))\n",
        "create_dt(dt_res)\n",
        "```\n",
        "---\n\n"
      )
      .buffer_add(resultsTableContent)
    }
    
    # Slide for Network Visualization of Integrated Results
    if(!is.null(mSetObj$imgSet$reportSet$network_integ) && safeFileExists(mSetObj$imgSet$reportSet$network_integ)){
      networkVisualizationContent <- CreateTitleFigureSlide(mSetObj$imgSet$reportSet$network_integ, "Interactive network visualization of integrated results.")
      .buffer_add(networkVisualizationContent, collapse="\n")
    }
  }
  
  
  CreateMumHeatmap_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj);
    link <- GetSharingLink(mSetObj);
    
    if (!is.null(mSetObj$imgSet$reportSet$heatmap_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$heatmap_mummichog)) {
      heatmapSlideContent <- c(
        "## Heatmap Visualization for Mummichog Analysis\n\n",
        "- **Purpose:** Provides an overview of clustering patterns of peak intensities across samples.",
        "- **Visualization Details:** Each row represents a metabolite, each column a sample.",
        "  The color intensity represents peak intensity, helping to identify patterns and outliers.",
        "- **Interactive Feature:** Users can explore specific data points within the MetaboAnalyst platform.\n",
        "---\n\n"
      )
      .buffer_add(heatmapSlideContent, collapse="\n")
      
      # Include the heatmap figure
      heatmapFigureContent <- c(
        paste0("```{r figure_heatmap_mummichog, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), ": Interactive peak intensity heatmap visualization.',"),
        " out.width='100%', fig.align='center'}\n",
        "safeIncludeGraphics('", mSetObj$imgSet$reportSet$heatmap_mummichog, "')\n",
        "```\n\n---\n\n"
      )
      .buffer_add(heatmapFigureContent)

      createEnrichmentTable(mSetObj, "mumEnr");
    }
  }
  