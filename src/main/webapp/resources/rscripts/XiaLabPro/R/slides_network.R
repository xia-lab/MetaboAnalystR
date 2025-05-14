#'Create report of analyses (Network Explorer)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerRnwReport_slides<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateNetworkExplorerIntr_slides();
  
  CreateNetworkExplorerInputDoc_slides(mSetObj);
  CreateNetworkExplorerDoc_slides(mSetObj);
    AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Network explorer report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerIntr_slides <- function() {
  # Initialize counters for tables and figures if they don't exist
  if(!exists("table.count")) {
    table.count <<- 0
  }
  if(!exists("fig.count")) {
    fig.count <<- 0
  }
  
  # Slide content
  descr <- c(
    "## Network Explorer Module Overview\n\n",
    "- The Network Explorer module enables interactive exploration of metabolomics data within the context of biological networks.",
    "- Supports creation and visualization of six different network types, including five knowledge-based and one data-driven network.",
    "- Facilitates understanding of biological interactions and identification of key activity centers.",
    "- Steps include data upload, compound/gene mapping, network analysis selection, and detailed network visualization.",
    "\n\n---\n\n"  # Add slide break for R Markdown
  )
  
    cat(descr, file=rmdFile, append=TRUE);
}

CreateNetworkExplorerInputDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    # Initialize the slide content
    slideContent <- ""

    # Define the ID type descriptions
    compoundDesc <- if(!is.null(mSetObj$dataSet$cmpd.id.type)) mSetObj$dataSet$cmpd.id.type else ""
    geneDesc <- if(!is.null(mSetObj$dataSet$q.type.gene)) mSetObj$dataSet$q.type.gene else ""

    # Generate compound name mapping slide content if applicable
    if(exists('map.table', where = mSetObj$dataSet)) {
        compoundTableTitle <- if(compoundDesc != "") {
            paste("Compound Name Mapping (ID type:", compoundDesc, ")")
        } else {
            "Compound Name Mapping"
        }
        slideContent <- paste0(slideContent,
                              "## ", compoundTableTitle, "\n\n",
                              "```{r compound_name_mapping, echo=FALSE, results='asis'}\n",
                              "sum_dt <- CreateNetworkNameMapTableRMD(mSetObj)\n",
                              "create_dt(sum_dt)\n",
                              "```\n\n",
                              "---\n\n")
    }
    
    # Generate gene name mapping slide content if applicable
    if(exists('gene.map.table', where = mSetObj$dataSet)) {
        geneTableTitle <- if(geneDesc != "") {
            paste("Gene Name Mapping (ID type:", geneDesc, ")")
        } else {
            "Gene Name Mapping"
        }
        slideContent <- paste0(slideContent,
                              "## ", geneTableTitle, "\n\n",
                              "```{r gene_name_mapping, echo=FALSE, results='asis'}\n",
                              "sum_dt <- CreateNetworkGeneMapTableRMD(mSetObj)\n",
                              "create_dt(sum_dt)\n",
                              "```\n\n",
                              "---\n\n")
    }
    
    # Output the slide content to R Markdown or a file
    cat(slideContent, file=rmdFile, append=TRUE) 
}


#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerDoc_slides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj)

    network.types <- c("gene_metabolites", "metabo_phenotypes", "metabo_metabolites", "global", "dspc")
    network.names <- c("Gene-Metabolite Interaction Network", 
                       "Metabolite-Disease Interaction Network", 
                       "Metabolite-Metabolite Interaction Network", 
                       "Global Network", 
                       "Debiased Sparse Partial Correlation (DSPC) Network")
  
    # Slide 1: Overview
    slideContent <- paste("## Network Creation and Visualization\n\n",
                          "The Network Explorer enables the identification of cross-pathway connections and links to phenotypes for a comprehensive view of biological interactions.\n\n",
                          "- Supported network types include:\n", 
                          "   - KEGG global metabolic network\n",  
                          "   - Gene-metabolite interactions\n",   
                          "   - Metabolite-disease interactions\n",
                          "   - Metabolite-metabolite interactions\n", 
                          "   - DSPC network\n\n",                 
                          "---\n\n")

    cat(slideContent, file=rmdFile, append=TRUE) 
    CreateKeggNetDoc(mSetObj);
    createEnrichmentTable(mSetObj, "keggGlobal");

    # Loop through each network type
    for(i in 1:length(network.types)){
        net.type <- network.types[i]
        net.name <- network.names[i]
        if(file.exists(paste0(net.type, ".png"))){

            netSlide <- paste0("## ", net.name, "\n\n",
                                  "![", net.name, "](", net.type, ".png)\n\n", 
                                  "---\n\n")
            cat(netSlide, file=rmdFile, append=TRUE);
            createEnrichmentTable(mSetObj, net.type);
        }
    }
  
}
