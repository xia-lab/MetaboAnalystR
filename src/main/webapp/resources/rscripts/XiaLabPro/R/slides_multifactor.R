CreateMultiFacRnwReport_slides <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateMultiFacIntr_slides();
  CreateDataProcSlides(mSetObj);
  CreateNORMSlides(mSetObj);
  
  #InitMultiFacAnal();
  if(exists("analSet", where=mSetObj)){
    CreateMetaOverview_slides(mSetObj);
    CreateiPCAdoc_slides(mSetObj);
    CreateCorHeatmap_slides(mSetObj);

    CreateCovAdj_slides(mSetObj);
    CreateCorAnalysis_slides(mSetObj);
    CreateAOV2doc_slides(mSetObj);
     
    CreateASCAdoc_slides(mSetObj);
    CreateMBdoc_slides(mSetObj);

    CreateRandomForest_slides(mSetObj);
  }else{
    CreateTimeSeriesAnalNullMsg();
  }
    AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}

CreateMultiFacIntr_slides <- function() {
  slide_text <- c("## Analysis Workflow Overview\n\n",
             "1. Metabolomics data and metadata integrity check",
             "2. Missing value estimation",
             "3. Data filtering",
             "4. Data normalization",
             "5. Statistical analysis and visualization",
             "\n\n---\n\n");
  cat(slide_text, file = rmdFile, append = TRUE,sep="\n")
}

CreateMultiFacIntr_slides <- function() {
  slide_text <- c("## Analysis Workflow Overview\n\n",
             "This module is for exploratory analysis of metabolomics data with complex factors in addition to the primary condition of interest.\n",
             "1. Metabolomics data and metadata integrity check",
             "2. Missing value estimation and metadata editing",
             "3. Data filtering",
             "4. Data normalization",
             "5. Statistical analysis and visualization",
             "  - including metadata overview, covariate adjustment, PCA, heatmaps, Random Forests, etc.",
             "\n\n---\n\n");
  cat(slide_text, file = rmdFile, append = TRUE,sep="\n")
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis, time-series analysis
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
InitMultiFacAnal <- function(){
  descr <- c("\n\n<hr/>",
             "\n\n## 3. Multi-factor Statistical Analysis\n\n",
             "For metabolomics data accompanied by complex metadata, MetaboAnalyst offers several carefully selected methods for",
             " data analysis. They include:\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  descr2 <- c(
    "1. Data and metadata overview:",
    " + Metadata Visualization",
    " + Principal Component Analysis (PCA)",
    " + Hierarchical Clustering and Heatmap Visualization",
    "2. Univariate analysis:",
    " + Linear Models with Covariate Adjustment",
    " + Correlation and Partial Correlation Analysis",
    " + Two-way ANOVA (ANOVA2)",
    "3. Multivariate analysis: ",
    " + ANOVA-Simultaneous Component Analysis (ASCA)",
    " + Multivariate Empirical Bayes Analysis (MEBA)",
    "4. Supervised classification:",
    " + Random Forest",
    "\n",
    "*Please note: MEBA is only applicable to time-series data analysis.*");
  cat(descr2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
}

#'Create null analysis message for time-series sweave report
#'@description Creates empty time-series analysis message
#'@export
CreateTimeSeriesAnalNullMsg<-function(){
  descr <- c("No analysis was performed on your data.\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

##### DATA AND METADATA OVERVIEW SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'For Metadata Overview
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaOverview_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if this process has been executed
    if (is.null(mSetObj$imgSet$metahtmaptwo)) {
        return()
    }

    # Introduction to data and metadata overview
    intro <- c(
        "## Data and Metadata Overview\n\n",
        "- Understanding the correlation structure of the metadata is crucial for model definition and interpretation.",
        "- Visualizations such as metadata heatmap and correlation heatmap help identify relationships between metadata elements.",
        "- Supports various methods for calculating distance, correlation, and clustering.",
        "- Heatmaps can be displayed in overview or detail mode for comprehensive analysis.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Metadata Correlation Heatmap Slide
    metaCorrHeatmapSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$meta.corhm, "Correlation heatmap displaying relationship between metadata.")
    cat(metaCorrHeatmapSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Metadata Heatmap Slide
    metaHeatmapSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$metahtmaptwo, "Metadata heatmap displaying relationship between metadata.")
    cat(metaHeatmapSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}


#'Create report of analyses 
#'@description Report generation using Sweave
#'For Interactive PCA
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateiPCAdoc_slides <- function(mSetObj = NA, file = "presentation.Rmd") {
    mSetObj <- .get.mSet(mSetObj)
  
    # Check if PCA process was executed
    if (is.null(mSetObj$imgSet$pca.pair)) {
        return()
    }
  
    # Add PCA section title to slides
    cat("# Principal Component Analysis (PCA)\n\n", file = file, append = TRUE)
  
    # Add Pairwise PCA scores plots slide
    if (!is.null(mSetObj$imgSet$pca.pair)) {
        pcaPairSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pca.pair, "Pairwise PCA scores plots with density outlines showing group membership.")
        cat(pcaPairSlide, file = file, append = TRUE)
        cat("\n\n---\n\n", file = file, append = TRUE)
    }
  
    # Add interactive 3D PCA plot slide if available
    if (!is.null(mSetObj$imgSet$reportSet$ipca_3d) && safeFileExists(mSetObj$imgSet$reportSet$ipca_3d) ) {
        pca3DSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$ipca_3d, "Screenshot of interactive 3-D PCA plot.")
        cat(pca3DSlide, file = file, append = TRUE)
        cat("\n\n---\n\n", file = file, append = TRUE)
    }
}


#'Create report of analyses 
#'@description Report generation using Sweave
#'For Correlation Heatmap
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCorHeatmap_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    # Check if this process is executed
    if (is.null(mSetObj$analSet$htmap2)) {
        return()
    }

    # Introduction to the Two-way Heatmap Visualization
    intro <- c(
        "## Two-way Heatmap Visualization\n\n",
        "- Direct visualization of data points as color-coded squares, indicating higher or lower values.",
        "- Allows usage of different clustering algorithms or distance measures.",
        "- Samples are ordered by two factors, with the primary factor used for initial ordering.",
        "- Option to switch the ordering of factors for a different perspective.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Metadata Heatmap Slide
    heatmapSlide <- CreateTwoColumnFigureSlide(
        mSetObj$imgSet$htmap2,
        "Metadata heatmap displaying relationship between metabolites and metadata."
    )
    cat(heatmapSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}


##### UNIVARIATE ANALYSIS SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Covariate Adjustment 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCovAdj_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    # Check if the covariate adjustment process was executed
    if (is.null(mSetObj$analSet$cov)) {
        return()
    }

    params <- mSetObj$analSet$cov
    intro <- c(
        "## Linear Models with Covariate Adjustments\n\n",
        "- Primary metadata: `", params$primary.var, "`;",
        "- Selected covariates: `", paste(params$cov.var, collapse="; "), "`;",
        "- Block factor: `", params$block, "`;",
        "- P-value cutoff: `", params$p.thresh, "` type: `", params$pval.type, "`;",
        "- Number of significant features: `", params$sig.num, "`.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Covariate Adjustment Plot Slide
    covAdjSlide <- CreateTwoColumnFigureSlide(
        mSetObj$imgSet$covAdj,
        "P-values for metabolites with and without covariate adjustment."
    )
    cat(covAdjSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Significant features table slide
    table.count <<- table.count + 1;

    # Define the title as a markdown heading and write it to the Rmd file
    title_limma <- sprintf('## Table %d. Important features identified by linear models', table.count)
    cat(paste0(title_limma, "\n\n"), file = rmdFile, append = TRUE)

    sigFeaturesSlide <- paste0(
        "```{r table_mylimma, echo=FALSE, results='asis', warning=FALSE}\n",
        "sum_dt <- GetSigTableRMD_Limma(mSet)\n",
        "create_dt(sum_dt)\n",
        "```\n\n",
        "\n\n---\n\n"  # Slide/page separator for R Markdown documents
    )
    cat(sigFeaturesSlide, file = rmdFile, append = TRUE);
}



#'Create report of analyses 
#'@description Report generation using Sweave
#'Correlation and Partial Correlation Analysis 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCorAnalysis_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the correlation analysis was executed
    if (is.null(mSetObj$analSet$corr$cor.mat)) {
        return()
    }

    params <- mSetObj$analSet$corr

    # Slide for Correlation and Partial Correlation Analysis Introduction
    intro <- c(
        "## Correlation and Partial Correlation Analysis\n\n",
        "\n",
        sprintf("- Target of interest: `%s`;", params$pattern),
        sprintf("- Selected covariates: `%s`;", paste(params$cov.vec, collapse = "; ")),
        sprintf("- Correlation Measure: `%s`.\n", params$dist.name)
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Correlation Coefficients Slide
    if (!is.null(mSetObj$imgSet$corr)) {
        corrCoeffSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$corr, "Correlation coefficients for metadata of interest.")
        cat(corrCoeffSlide, file = rmdFile, append = TRUE)
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }
}


#'Create report of analyses 
#'@description Report generation using Sweave
#'ANOVA 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateAOV2doc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the two-way ANOVA process was executed
    if (is.null(mSetObj$analSet$aov2)) {
        return()
    }

  params <- mSetObj$analSet$aov2;
    # Slide for Two-way ANOVA Introduction
    intro <- c(
        "## Two-way ANOVA Analysis\n\n",
        "\n",
        sprintf("- Selected metadata: `%s`;", paste(params$selected.meta, collapse = ", ")),
        sprintf("- Phenotype factor: `%s`;", params$phenotype.factor),
        sprintf("- Multiple correction method: `%s`;", params$multi.c),
        sprintf("- P-value threshold: `%s`.\n", params$raw.thresh)
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Two-way ANOVA Results Slide
    if (!is.null(mSetObj$imgSet$anova2)) {
        aov2Slide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$anova2, "Important features selected by two-way ANOVA.")
        cat(aov2Slide, file = rmdFile, append = TRUE)
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }

    # Table for Significant Features from Two-way ANOVA
    if (!is.null(mSetObj$analSet$aov2$sig.mat)) {
        title_aov2 <- sprintf('## Table %d. Significant features identified by two-way ANOVA', getTableCount())
        cat(title_aov2, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)  # Ensure there's a break line after the title

        r_code_chunk_aov2 <- paste0(
            "```{r table_aov2_analysis, echo=FALSE, results='asis'}\n",
            "sum_dt <- GetSigTable.Aov2RMD(mSetObj)\n",
            "if(nrow(sum_dt) > 0) {\n",
            "create_dt(sum_dt)\n",  # Removed caption; title handled separately
            "}\n",
            "```\n",
            "\n\n---\n\n"
        )

        # Write the R code chunk to the R Markdown file for PowerPoint slides
        cat(r_code_chunk_aov2, file = rmdFile, append = TRUE)
    }
}


##### MULTIVARIATE ANALYSIS SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Random Forest ASCA
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateASCAdoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if ASCA analysis was executed
    if (is.null(mSetObj$analSet$asca)) {
        return()
    }
    asca <- qs::qread("asca.qs")

    # Construct section slide for ASCA
    intro <- c(
        "## ANOVA - Simultaneous Component Analysis (ASCA)\n\n",
        "- ASCA combines ANOVA with PCA to identify patterns associated with factors in multivariate data.",
        "- It partitions variance into components related to factors and interactions in two-factor designs.",
        "- Visual analysis through PCA scores plots of each component enables major pattern detection.\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Additional ASCA analysis results slides
    # Replace following with actual slide creation or image inclusion as per your data and analysis
if (!is.null(mSetObj$analSet$asca)) {
    # Scree Plots Slide
    screePlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.scree, "Scree plots for each sub-model in ASCA analysis.")
    cat(screePlotSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Factor A Patterns Slide
    factorAPatternsSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.modelA, "Major patterns associated with Factor A in ASCA analysis.")
    cat(factorAPatternsSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Factor B Patterns Slide
    factorBPatternsSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.modelB, "Major patterns associated with Factor B in ASCA analysis.")
    cat(factorBPatternsSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Interaction Patterns Slide
    interactionPatternsSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.modelAB, "Major patterns associated with Interaction between factors in ASCA analysis.")
    cat(interactionPatternsSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Model Validation Slide
    modelValidationSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.perm, "Model validation through permutations in ASCA analysis.")
    cat(modelValidationSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Important Features for Factor A Slide
    impFeaturesAFactorSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.impA, "Important variables associated with Factor A in ASCA analysis.")
    cat(impFeaturesAFactorSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Important Features for Factor B Slide
    impFeaturesBFactorSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.impB, "Important variables associated with Factor B in ASCA analysis.")
    cat(impFeaturesBFactorSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Important Features for Interaction Slide
    impFeaturesInteractionSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$asca.impAB, "Variables important in interaction between the two factors in ASCA analysis.")
    cat(impFeaturesInteractionSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}

}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Multivariate Bayes
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMBdoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the process has been executed
    if (is.null(mSetObj$analSet$MB)) {
        return()
    }

    # Introduction to MEBA
    intro <- c(
        "## Multivariate Empirical Bayes Approach (MEBA)\n\n",
        "- MEBA compares time-course profiles under different conditions.",
        "- Higher statistical values indicate more distinct time-course profiles across conditions.",
        "\n\nSelected metadata: ", paste("`", mSetObj$analSet$MB$selected.meta, "`", collapse=", "), "\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Significant features table slide
    table.count <<- table.count + 1
    sigFeaturesSlide <- paste0(
        "### Table ", table.count, ". Significant features identified by advanced MEBA\n\n",
        "```{r table_mbsig, echo=FALSE, results='asis', warning=FALSE}\n",
        "sum_dt <- GetSigTableRMD_MB(mSet)\n",
        "create_dt(sum_dt)\n",
        "```\n\n"
    )
    cat(sigFeaturesSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}


##### SUPERVISED CLASSIFICATION SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Random Forest
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateRandomForest_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    # Check if the process has been executed
    if (is.null(mSetObj$dataSet$cls.rf)) {
        return()
    }

    # Introduction slide for Random Forest
    intro <- c(
        "## Random Forest Analysis\n\n",
        "\nPrimary metadata: ", mSetObj$dataSet$cls.rf.nm, "\n",
        "Predictor(s) metadata: ", mSetObj$analSet$meta.vec.rf, "\n",
        "Randomness: ", mSetObj$analSet$rf.random, "\n"
    )
    cat(intro, file = rmdFile, append = TRUE, sep = "\n")
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Classification error vs number of trees slide
    classificationErrorSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$rf.cls, "Classification error vs. number of trees")
    cat(classificationErrorSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Features ranked by their contributions to classification accuracy slide
    featuresRankSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$rf.imp, "Features ranked by their contributions to classification accuracy")
    cat(featuresRankSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Potential outliers flagged by random forest algorithm slide
    outliersSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$rf.outlier, "Up to five potential outliers flagged by random forest algorithm")
    cat(outliersSlide, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
}

