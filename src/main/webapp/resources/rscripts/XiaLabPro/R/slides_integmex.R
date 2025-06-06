#'Create report of analyses (IntegPathwayAnalysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegPathwayAnalysisRnwReport_slides<-function(mSetObj, usrName){
  #save.image("integslides.RData");
  CreateHeader(usrName);
  CreateIntegratedPathwayAnalIntr_slides();
  
  #CreateIntegratedPathwayAnalInputDoc(mSetObj);
  
  CreateIntegratedPathwayDoc_slides(mSetObj);
  
  CreateKeggNetDoc(mSetObj);
  createEnrichmentTable(mSetObj, "keggGlobal")
  AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Integrated pathwayr analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayAnalIntr_slides <- function() {
    # Ensure counters for tables and figures are set
    if(!exists("table.count")) {
        table.count <<- 0
    }
    if(!exists("fig.count")) {
        fig.count <<- 0
    }
    
    # Slide content
    introSlide <- c(
        "## Selected Module: Integrated Metabolic Pathway Analysis\n\n",
        "- Analyzes two lists of interest: metabolites/peaks and genes/proteins.",
        "- Utilizes KEGG pathways for analysis and visualization.\n\n",
        "### Considerations\n\n",
        "- Metabolomic technologies may not cover the entire metabolome, unlike transcriptomics.",
        "- The module allows for exploration based on combined or single-omic evidence to mitigate bias.\n"
    )
    
    # Concatenate slide content into a single string with Markdown formatting
    slideContent <- paste(introSlide, collapse = "\n")
    
    # Output to R Markdown file (assuming 'rmdFile' is defined and points to your .Rmd file)
    cat(slideContent, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE) # Slide separator for R Markdown presentations
}

#'Create integrated pathway  report: Data Input
#'@description Report generation using Sweave
#'integrated pathway report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'McGill University, viewingCanada
#'All rights reserved
#'@export
CreateIntegratedPathwayAnalInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("## 2. Data Processing\n\n",
             "The integrated pathway analysis module accepts a gene list with optional fold changes, the ID type must either be an Entrez ID,", 
             " a RefSeq ID, a Genbank Accession Number, a ENSEMBL Gene Accession Number, or an Official Gene Symbol. The module also accepts ",
             "a metabolite list, with optional fold changes. Here, the ID type must be the compound name, the HMDB ID, or the KEGG ID. Finally", 
             " the organism must be specified, either Homo sapiens (human), Mus musculus (mouse), or Rattus norvegicus (rat). The genes and the metabolites", 
             " will be mapped to the respective databases collected within MetaboAnalyst. ",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  if(exists('map.table', where=mSetObj$dataSet)){
    

    table.count <<- table.count+1;
    
    cmdhist2 <- c(paste0("## Table ", table.count, ". Compound Name Mapping."),
      "```{r table_mtb1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateIntegratedPathwayNameMapTableRMD(mSet);",
      paste0("create_dt(sum_dt)"),
      "```", 
      "\n\n"
    );

    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  if (exists('gene.map.table', where=mSetObj$dataSet)){

   

    table.count <<- table.count+1;

    cmdhist2 <- c(paste0("## Table ", table.count, ". Gene Name Mapping."),
      "```{r table_mtb2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateIntegratedPathwayGeneMapTableRMD(mSet);",
      paste0("create_dt(sum_dt)"),
      "```", 
      "\n\n"
    );

    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }  
  
  cat("\n\n", file=rmdFile, append=TRUE);
}

#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    if(is.null(mSetObj$dataSet$path.mat)) {
        return()
    }

    # Pathway Results Overview
    overviewSlides <- c(
        "# Pathway Analysis Results\n\n",
        "---\n\n"
    )
    cat(overviewSlides, file = rmdFile, append = TRUE, sep = "\n")

    # Pathway Overview Slide
    if(!is.null(mSetObj$imgSet$path.overview)) {
        overviewSlideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$path.overview, "Overview of all pathways correlated with your data.")
        cat(overviewSlideContent, file = rmdFile, append = TRUE)
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }

    # Detailed Pathway View Slide
    if(!is.null(mSetObj$imgSet$pathinteg.path)) {
        detailedPathwaySlideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pathinteg.path, "Detailed view of a selected pathway with highlighted nodes.")
        cat(detailedPathwaySlideContent, file = rmdFile, append = TRUE)
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }

    # Enriched Pathways Table
    if(!is.null(mSetObj$dataSet$path.mat)) {
        table.count <<- table.count + 1
        enrichedPathwaysTableContent <- c(
            paste0("## Table ", table.count, ": Enriched pathways based on the integrated analysis.\n\n"),
            "```{r table_mtb3, echo=FALSE, results='asis', warning=FALSE}",
            "sum_dt <- CreateIntegratedPathwayResultsTableRMD(mSetObj);",
            "create_dt(sum_dt)",
            "```\n",
            "---\n\n"
        )
        cat(enrichedPathwaysTableContent, file = rmdFile, append = TRUE, sep = "\n")
    }

    # Pathway Mapping Details Table
    if(!is.null(mSetObj$dataSet$path.mat)) {
        table.count <<- table.count + 1
        pathwayDetailsTableContent <- c(
            paste0("## Table ", table.count, ": Pathway mapping details.\n\n"),
            "```{r table_mtb4, echo=FALSE, results='asis', warning=FALSE}",
            "sum_dt2 <- CreateIntegratedPathwayMemberTableRMD();",
            "create_dt(sum_dt2)",
            "```\n",
            "---\n\n"
        )
        cat(pathwayDetailsTableContent, file = rmdFile, append = TRUE, sep = "\n")
    }
}