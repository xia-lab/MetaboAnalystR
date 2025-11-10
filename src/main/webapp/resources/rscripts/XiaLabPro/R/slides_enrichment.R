#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichRmdReport_slides<-function(mSetObj, usrName){
  mSetObj <- .get.mSet(mSetObj);

  CreateHeader(usrName);
  CreateEnrichIntr_slides();
   
  CreateEnrichProcessDoc_slides(mSetObj);

  if(mSetObj$analSet$type == "msetssp"){
    CreateEnrichSSPdoc_slides(mSetObj);
  }
  if(mSetObj$analSet$type == "msetqea"){
    CreateNORMSlides(mSetObj);
  }

  CreateEnrichAnalDoc_slides();
  
  if(mSetObj$analSet$type == "msetqea"){
    CreateEnrichQEAdoc_slides(mSetObj);
  }else{
    CreateEnrichORAdoc_slides(mSetObj);
  }
    AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report introduction
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichIntr_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Introduction part
    intro <- c(
        "## Metabolite Set Enrichment Analysis (MSEA)\n\n",
        "- Identifies biologically meaningful patterns significantly enriched in metabolomic data.",
        "- Utilizes over-representation analysis (ORA) to check for enrichment in a given list of compounds.",
        "- Applies GlobalTest to examine if metabolite sets' abundance profiles are associated with the study condition directly from a compound concentration table.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")

    # Conditional content based on selected analysis type
    if (mSetObj$analSet$type == "msetora") {
        input_descr <- c(
            "- **Input:** A list of compound names.",
            "- **Analysis Aim:** To analyze enrichment from a predefined set of compounds.\n"
        )
    } else if (mSetObj$analSet$type == "msetssp") {
        input_descr <- c(
            "- **Input:** Data from a single biofluid sample.",
            "- **Supported Samples:** Blood, urine, and CSF, based on available reference data.\n"
        )
    } else {  # Assuming QEA if none of the above
        input_descr <- c(
            "- **Input:** A concentration table from quantitative metabolomics studies.",
            "- **Data Format:** Supports categorical (binary or multi-class) or continuous phenotypes.\n"
        )
    }

    # Combine and append descriptions
    cat(input_descr, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report enrichment process
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichProcessDoc_slides <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  descr <- GetNameMappingSlides();
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Single sampling profiling
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichSSPDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Insert SSP Analysis Results
    if (is.null(mSetObj$analSet$ssp.mat)) {
        return;
    }

        # If SSP results exist, create table content
        tableSSPContent <- c(
            "## SSP Results: Concentration Comparison\n\n",
            "Compounds' measured concentrations are compared to their reference ranges.",
            "L, M, H indicate whether measured concentrations are Lower, Medium (within range), or Higher than reference values.",
            "0 indicates compounds not selected for enrichment analysis; 1 indicates selection.\n",
            "```{r table_ssp, echo=FALSE, results='asis', warning=FALSE}",
            "ssp.res <- mSetObj$analSet$ssp.mat[,-c(1,3,6)];",  # Adjust according to the actual content structure
            "rownames(ssp.res) <- mSetObj$analSet$ssp.mat[,1]",
            "selected.col <- rep(0, nrow(ssp.res));",
            "inx <- match(mSetObj$dataSet$cmpd, mSetObj$analSet$ssp.mat[,1]);",
            "selected.col[inx] <- 1;",  # This line might need adjustments based on actual selection process
            "dt_ssp <- as.data.frame(cbind(ssp.res, selected = selected.col));",
            paste0("create_dt(dt_ssp, caption = 'Table ", table.count, ". Comparison with Reference Concentrations.')"),
            "```"
        )
        cat(tableSSPContent, file = rmdFile, append = TRUE, sep = "\n")
      cat("\n\n---\n\n", file = rmdFile, append = TRUE);

}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, analysis
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj);

    # Enrichment Analysis Introduction
    enrichmentAnalysis <- c(
        "## Enrichment Analysis\n\n",
        "- Conducts analysis to identify significantly enriched metabolite sets.\n",
        "- Utilizes chosen library to match against study data for biological relevance.\n",
        "- Results aid in understanding metabolic pathways or functions impacted.\n",
        "- Selected Library: `", mSetObj$analSet$msetlibname
    )
    cat(enrichmentAnalysis, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}


#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, over
#'representation analysis (ORA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichORAdoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Introduction to ORA
    intro <- c(
        "## Over Representation Analysis (ORA)\n\n",
        "- ORA investigates biologically meaningful patterns in a provided compound list.",
        "- Utilizes hypergeometric test to check if metabolite sets are more represented than expected by chance.",
        "- P-values are adjusted for multiple testing to highlight significant findings.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # ORA Summary Plot
    if (!is.null(mSetObj$imgSet$ora)) {
        oraSummarySlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$ora, "Summary of Over Representation Analysis results showing significant metabolite sets.")
        cat(oraSummarySlide, file = rmdFile, append = TRUE)
    }

    # Enrichment Network
    if (!is.null(mSetObj$imgSet$reportSet$enrichment_network) && safeFileExists(mSetObj$imgSet$reportSet$enrichment_network)) {
        enrichmentNetworkSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$enrichment_network, "Enrichment network from ORA showing the interconnectedness of significant metabolite sets.")
        cat(enrichmentNetworkSlide, file = rmdFile, append = TRUE)
    }

    # ORA Dot Plot
    if (!is.null(mSetObj$imgSet$ora_dot)) {
        oraDotPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$ora_dot, "Dot plot summary from ORA indicating the significance and impact of each metabolite set.")
        cat(oraDotPlotSlide, file = rmdFile, append = TRUE)
    }

    # ORA Results Table
    if (!is.null(mSetObj$analSet$ora.mat)) {

        tableORA <- c(
            paste0("## Table ", getTableCount(), ": Results from Over Representation Analysis showing enriched metabolite sets."), "\n\n",
            "```{r table_ora, echo=FALSE, results='asis', warning=FALSE}\n",
            "create_dt(mSetObj$analSet$ora.mat)\n",
            "```\n"
        )

        cat(tableORA, file = rmdFile, append = TRUE)
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }
}



#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Quantitative enrichment analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichQEAdoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Introduction to Quantitative Enrichment Analysis (QEA)
    intro <- c(
        "## Quantitative Enrichment Analysis (QEA)\n\n",
        "- QEA examines association between metabolite sets and clinical outcomes.",
        "- Utilizes 'globaltest' package to estimate a Q-statistic for each metabolite set.",
        "- Correlates compound concentrations with clinical outcomes.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Display summary plot
    if (!is.null(mSetObj$imgSet$qea)) {
        summaryPlot <- CreateTwoColumnFigureSlide(mSetObj$imgSet$qea, "Summary plot for Quantitative Enrichment Analysis (QEA).")
        cat(summaryPlot, file = rmdFile, append = TRUE)
    }

    # Display summary dot plot
    if (!is.null(mSetObj$imgSet$qea_dot)) {
        summaryDotPlot <- CreateTwoColumnFigureSlide(mSetObj$imgSet$qea_dot, "Summary dot plot for Quantitative Enrichment Analysis (QEA).")
        cat(summaryDotPlot, file = rmdFile, append = TRUE)
    }
}
