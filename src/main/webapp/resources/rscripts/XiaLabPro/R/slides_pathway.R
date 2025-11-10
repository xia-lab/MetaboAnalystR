
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

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreatePathRnwReport_slides <- function(mSetObj, usrName){
  mSetObj <- .get.mSet(mSetObj);
  CreateHeader(usrName);

  CreatePathIntr_slides();
  #CreatePathProcessDoc_slides(mSetObj);
  if(mSetObj$analSet$type == "pathqea"){
    CreateDataProcSlides(mSetObj);
    CreateNORMSlides(mSetObj);
  }
  #CreatePathAnalDoc_slides(mSetObj);
  CreatePathResultDoc_slides(mSetObj);
    AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
  
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Introduction
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathIntr_slides <- function() {
  # Slide content for Pathway Analysis Introduction
  slideContent <- paste(
    "## Pathway Analysis Introduction\n\n",
    "This section introduces the Pathway Analysis module, which combines pathway enrichment and visualization. The module assists in identifying relevant biological pathways related to your study conditions, utilizing the KEGG database.\n\n",
    "- **Analysis Features:**\n",
    "   - Utilizes KEGG metabolic pathways\n",
    "   - Integrates traditional and novel analysis methods\n",
    "   - Offers interactive visualization for intuitive result interpretation\n\n",
    "---\n\n"
  )

  # Output the slide content
  cat(slideContent, file=rmdFile, append=TRUE)
}


#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA process
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathProcessDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    
    # Assuming GetNameMappingDoc() is adapted for slides
    descr <- GetNameMappingDoc();
    cat(descr, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Check if name mapping is available
    if(exists('map.table', where = mSetObj$dataSet)) {

        
        # Slide for Compound Name Mapping
        table.count <<- table.count + 1
        cat("## Table ", table.count, ". Compound Name Mapping Results.\n\n", file = rmdFile, append = TRUE)
        nameMappingSlide <- paste(
            "```{r name_mapping_table, echo=FALSE, results='asis'}\n",
            "dt_res <- as.data.frame(GetMapTableRMD(mSetObj))\n",
            paste0("create_dt(dt_res)"),
            "```\n\n",
            "---\n\n"
        )
        
        # Output the name mapping slide content
        cat(nameMappingSlide, file = rmdFile, append = TRUE)
    }
    
    cat("\n\n", file = rmdFile, append = TRUE)
}


#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create pathway analysis doc
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Slide 1: Pathway Analysis Overview
    pathwayAnalysisIntro <- paste(
        "## Pathway Analysis Overview\n\n",
        "This section details the various analyses conducted, including over-representation analysis (ORA), ",
        "quantitative enrichment analysis (QEA), and pathway topology analysis.\n",
        "\n\n---\n\n"
    )
    cat(pathwayAnalysisIntro, file = rmdFile, append = TRUE)
    
    # ORA or QEA based on analysis type
    if (mSetObj$analSet$type == "pathora") {
        oraContent <- paste(
            "### Over-Representation Analysis (ORA)\n\n",
            "ORA tests if a specific group of compounds is more represented than expected by chance. ",
            "It includes Fisher's Exact test and Hypergeometric Test methods. ",
            "This analysis helps identify significantly enriched pathways based on the compounds involved.\n\n",
            "---\n\n"
        )
        cat(oraContent, file = rmdFile, append = TRUE)
    } else {
        qeaContent <- paste(
            "### Quantitative Enrichment Analysis (QEA)\n\n",
            "QEA utilizes compound concentration values for a sensitive analysis, identifying subtle changes among compounds in the same biological pathway. ",
            "It is based on methods such as GlobalTest and GlobalAncova, suitable for various phenotypes.\n\n",
            "---\n\n"
        )
        cat(qeaContent, file = rmdFile, append = TRUE)
    }

    # Slide for Pathway Topology Analysis
    topologyAnalysisContent <- paste(
        "### Pathway Topology Analysis\n\n",
        "- This analysis evaluates the importance of compounds within a pathway based on their positions and connections. ",
        "- It employs degree and betweenness centrality measures to assess the impact of each node.\n\n",
        "---\n\n"
    )
    cat(topologyAnalysisContent, file = rmdFile, append = TRUE)

    # Slide for Pathway Library Selection
    librarySelectionContent <- paste(
        "### Pathway Library Selection\n\n",
        "Multiple pathway libraries covering various organisms are supported. ",
        "This allows for a broad and relevant analysis based on the organism of interest.\n\n",
        "Selected Pathway Library: ", mSetObj$paramSet$lib.nm, "\n\n",
        "---\n\n"
    )
    cat(librarySelectionContent, file = rmdFile, append = TRUE)
}


#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA results doc
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathResultDoc_slides <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("## Pathway Analysis Result\n\n",
             "The results from pathway analysis are presented graphically below.
             Two tables are provided - the pathway analysis result table shows the numerical details of the scatter plot; while the 
             pathway mapping table shows individual compounds that are assigned to different pathways. \n\n--\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  #path_view
  if(!is.null(mSetObj$imgSet$path.overview)){

    overviewSlideContent <- CreateTitleFigureSlide(mSetObj$imgSet$path.overview, "Overview of all pathways correlated with your data.")
    cat(overviewSlideContent, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
    cat("## Detailed result table", file=rmdFile, append=TRUE, sep="\n");

  descr <- c(
    "The table below shows the detailed results from the pathway analysis.",
    "\n",
    "* the **Total** is the total number of compounds in the pathway;",
    "* the **Hits** is the actually matched number from the user uploaded data;",
    "* the **Raw p** is the original p value calculated from the enrichment analysis;",
    "* the **Holm p** is the p value adjusted by Holm-Bonferroni method;",
    "* the **FDR p** is the p value adjusted using False Discovery Rate;",
    "* the **Impact** is the pathway impact value calculated from pathway topology analysis.",
    "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");

  if(mSetObj$analSet$type == "pathora"){
    # Result from Pathway Analysis
    table.count <<- table.count+1;
    cat(paste0("Table ", table.count,". Result from ORA Pathway Analysis."), file=rmdFile, append=TRUE, sep="\n\n");
    descr <- c(
      "```{r table_paa, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(GetORATableRMD(mSet));",
      paste0("create_dt(dt_res, table.name='pathora')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n");

  }else{
    table.count <<- table.count+1;

    # Result from Quantitative Enrichment Analysis
    cat(paste0("## Table ", table.count, ". Result from Quantitative Enrichment Analysis."), file=rmdFile, append=TRUE, sep="\n\n");
    descr <- c(
      "```{r table_pqea, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(GetQEATableRMD(mSet));",
      paste0("create_dt(dt_res, table.name='pathqea')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n");

  }


    cat(paste0("## Table ", table.count, ". Pathway mapping details table."), file=rmdFile, append=TRUE, sep="\n");
    sum_dt2 <- mSet$analSet$pathwayMemberTable;
    if(nrow(sum_dt2) > 0){
        table.count <<- table.count+1;

        cmdhist2 <- c(
          "```{r table_mtb4, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt2 <- CreatePathwayMemberTableRMD(mSet);",
          paste0("create_dt(sum_dt2)"),
          "```", "\n\n");

        cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    }
}
    if(!is.null(mSetObj$imgSet$reportSet$heatmap_pathway) && safeFileExists(mSetObj$imgSet$reportSet$heatmap_pathway)){
        cat("## Figure ", getFigCount(), ". Screenshot of interactive peak heatmap.", file=rmdFile, append=TRUE, sep="\n");
        fig2 <- c(paste0("```{r figure_heatmap_pathway, echo=FALSE, fig.pos='H',",
                         " fig.lp='", 
                         mSetObj$imgSet$reportSet$heatmap_pathway, 
                         "', out.width = '", getFigWidth(mSetObj), "'}"),
                  "safeIncludeGraphics(mSetObj$imgSet$reportSet$heatmap_pathway)",
                  "```",
                  "\n\n");
        cat(fig2, file=rmdFile, append=TRUE, sep="\n");
        cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
}
