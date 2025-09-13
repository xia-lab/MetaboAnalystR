#'Create report for statistical analysis module
#'@description Report generation using Sweave
#'Write .Rmd file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateStatRmdReportSlides <- function(mSetObj, usrName){
  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateStatIntrSlides();
  CreateDataProcSlides(mSetObj);
  CreateNORMSlides(mSetObj);

  #InitStatAnalModeSlides();

  if(exists("analSet", where = mSetObj)){
    CreateUNIVSlides(mSetObj); #ok
    CreateANOVASlides(mSetObj);#ok
    CreateCorrSlides(mSetObj);#ok
    CreateDSPCSlides(mSetObj);#ok
    CreatePCASlides(mSetObj);#ok
    CreatePLSSlides(mSetObj);#ok
    CreateOPLSDASlides(mSetObj);#ok
    CreateSPLSDASlides(mSetObj);#ok
    CreateSAMSlides(mSetObj)#ok;
    CreateEBAMSlides(mSetObj);#ok
    CreateHCSlides(mSetObj); #ok
    CreateKMSlides(mSetObj);#ok
    CreateSOMSlides(mSetObj);#ok
    CreateRFSlides(mSetObj);#OK
    CreateSVMSlides(mSetObj);#ok
  }else{
    CreateAnalNullMsg();
  }
   AddFeatureImages_slides(mSetObj);

   CreateSlideFooter();
}

CreateStatIntrSlides <- function() {
  slide_text <- c("## Analysis Workflow Overview\n\n",
             "This module is designed for general-purpose exploratory data analysis with a single experimental factor (two- or multi-group) design.\n",
             "1. Data upload and integrity check",
             "2. Missing value estimation",
             "3. Data filtering",
             "4. Data normalization",
             "5. Statistical analysis and visualization",
             "  - including t-tests, ANOVA, volcano plot, PCA, PLSDA, heatmaps, Random Forests, SVM, etc.",
             "\n\n---\n\n");
  cat(slide_text, file = rmdFile, append = TRUE,sep="\n")
}

CreateUNIVSlides <- function(mSetObj = NA) {
    link <- GetSharingLink(mSetObj)
    mSetObj <- .get.mSet(mSetObj)
  
    # Check if the univariate analysis process was executed
    if(is.null(mSetObj$analSet$fc) && is.null(mSetObj$analSet$tt) && is.null(mSetObj$analSet$volcano)){
        return()  # Exit if univariate analysis was not performed
    }
  
    #descr <- c("# Univariate Analysis",
    #           "\n\n---\n\n")
    #cat(descr, file = rmdFile, append = TRUE)
  
    # Fold Change Analysis
    if(!is.null(mSetObj$imgSet$fc)){
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$fc, 'Important features selected by fold-change analysis.')
        cat(slideContent, file = rmdFile, append = TRUE)

        title_fc <- sprintf('## Table %d. Important features identified by Fold Change analysis', getTableCount())
        cat(title_fc, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)  # Ensures there is a newline after the title

        # Create the R code chunk for the table
        r_code_chunk_fc <- paste0(
          "```{r table_fc_analysis, echo=FALSE, results='asis'}\n",
          "dt_res_fc <- as.data.frame(GetSigTableRMD_FC(mSetObj))\n",
          "if(ncol(dt_res_fc) > 1) {\n",
          "    create_dt(dt_res_fc)\n",  # Removed caption parameter; title is handled separately
          "}\n",
          "```\n",
          "\n\n---\n\n"  # Slide/page separator for R Markdown documents
        )

        # Write the R code chunk to the R Markdown file for PowerPoint slides
        cat(r_code_chunk_fc, file = rmdFile, append = TRUE)

    }
  
    # T-tests
    if(!is.null(mSetObj$imgSet$tt)){
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$tt, 'Important features selected by T-tests.')
        cat(slideContent, file = rmdFile, append = TRUE)
        title_tt <- sprintf('## Table %d. Important features identified by T-tests', getTableCount())
        cat(title_tt, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)

        r_code_chunk_tt <- paste0(
          "```{r table_tt_analysis, echo=FALSE, results='asis'}\n",
          "dt_res_tt <- as.data.frame(GetSigTableRMD_TT(mSetObj))\n",
          "if(ncol(dt_res_tt) > 1) {\n",
          "    create_dt(dt_res_tt)\n",
          "}\n",
          "```\n",
          "\n\n---\n\n"
        )
        cat(r_code_chunk_tt, file = rmdFile, append = TRUE);
    }
  
    # Volcano Plot
    if(!is.null(mSetObj$analSet$volcano$sig.mat)){
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$volcano, 'Important features identified by volcano plot.')
        cat(slideContent, file = rmdFile, append = TRUE)

        # Define the title as a markdown heading and write it to the Rmd file
        title_volcano <- sprintf('## Table %d. Important features identified by Volcano Plot analysis', getTableCount())
        cat(title_volcano, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)  # Ensures there is a newline after the title

        # Create the R code chunk for the table
        r_code_chunk_volcano <- paste0(
          "```{r table_volcano_analysis, echo=FALSE, results='asis'}\n",
          "dt_res_volcano <- as.data.frame(GetSigTableRMD_Volcano(mSetObj))\n",
          "if(ncol(dt_res_volcano) > 1) {\n",
          "    create_dt(dt_res_volcano)\n",  # Removed caption parameter; title is handled separately
          "}\n",
          "```\n",
          "\n\n---\n\n"  # Slide/page separator for R Markdown documents
        )

        # Write the R code chunk to the R Markdown file for PowerPoint slides
        cat(r_code_chunk_volcano, file = rmdFile, append = TRUE)

    }
}

CreateANOVASlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the ANOVA process was executed
    if(is.null(mSetObj$analSet$aov)) {
        return()  # Exit if ANOVA was not performed
    }

    descr <- c("# ANOVA",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # ANOVA figure description and image
    anovaFigureDescription <- 'Significant features selected by ANOVA. The p values are transformed by -log10 so that the more significant features are higher on the graph.'
    anovaFigurePath <- mSetObj$imgSet$anova  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(anovaFigurePath, anovaFigureDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    # ANOVA table rendering
    title_anov <- sprintf('## Table %d. Important features identified by One-way ANOVA and post-hoc analysis', getTableCount())
    cat(title_anov, file = rmdFile, append = TRUE)
    cat("\n\n", file = rmdFile, append = TRUE)

    r_code_chunk_anov <- paste0(
        "```{r table_anov, echo=FALSE, results='asis'}\n",
        "dt_res <- as.data.frame(GetSigTableRMD_Anova(mSetObj))\n",
        "if(ncol(dt_res) > 1) {\n",
        "    create_dt(dt_res)\n",
        "}\n",
        "```\n",
        "\n\n---\n\n"
    )
    cat(r_code_chunk_anov, file = rmdFile, append = TRUE)

}

CreateCorrSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the correlation analysis process was executed
    if(is.null(mSetObj$imgSet$heatmap_stats_corr_param)) {
        return()  # Exit if correlation analysis was not performed
    }

    descr <- c("# Correlation Analysis",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # Correlation Heatmaps
    if(!is.null(mSetObj$imgSet$corr.heatmap) && file.exists(mSetObj$imgSet$corr.heatmap) ){
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$corr.heatmap, 'Correlation heatmaps showing significant features selected by statistical methods.')
        cat(slideContent, file = rmdFile, append = TRUE)
    }

    # Pattern Hunter Analysis
    if(!is.null(mSetObj$analSet$corr$cor.mat) && !isEmptyMatrix(mSetObj$analSet$corr$cor.mat)){
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$corr, 'Pattern Hunter analysis showing significant correlations with features of interest.')
        cat(slideContent, file = rmdFile, append = TRUE)
    }

    # Result Table for Correlation Analysis
    if(!is.null(mSetObj$analSet$corr$cor.mat)){
        # R code chunk for Correlation Analysis
        title_corr <- sprintf('## Table %d. Important features identified by correlation analysis', getTableCount())
        cat(title_corr, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)

        r_code_chunk_corr <- paste0(
            "```{r table_corr_analysis, echo=FALSE, results='asis'}\n",
            "dt_res <- as.data.frame(GetSigTableRMD_Corr(mSetObj))\n",
            "if(ncol(dt_res) > 1) {\n",
            "    create_dt(dt_res)\n",
            "}\n",
            "```\n",
            "\n\n---\n\n"
        )
        cat(r_code_chunk_corr, file = rmdFile, append = TRUE)

    }

    cat("\n\n", file = rmdFile, append = TRUE)
}


CreateDSPCSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the DSPC process was executed
    #if(!exists("pheno.net")) {
    if(!file.exists('dspc.png')){
        return()  # Exit if DSPC network was not saved
    }

    descr <- c("# Debiased Sparse Partial Correlation (DSPC) Analysis",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # Use CreateTwoColumnFigureSlide function for DSPC network figure
    dspcFigurePath <- 'dspc.png'  # Ensure this is the correct path to your DSPC network figure
    dspcFigureCaption <- sprintf("Screenshot of DSPC network. The nodes are input metabolites, while the edges represent the association measures.")
    slideContent <- CreateTwoColumnFigureSlide(dspcFigurePath, dspcFigureCaption)
    cat(slideContent, file = rmdFile, append = TRUE)
}


CreatePCASlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the PCA process was executed
    if(is.null(mSetObj$analSet$pca)) {
        return()  # Exit if PCA was not performed
    }

    descr <- c("# Principal Component Analysis (PCA)",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # Pairwise score plots
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pca.pair, sprintf("PCA Pairwise score plots showing significant features selected by statistical methods."))
    cat(slideContent, file = rmdFile, append = TRUE)

    # Scree plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pca.scree, sprintf("PCA Scree plot shows the variance explained by each principal component."))
    cat(slideContent, file = rmdFile, append = TRUE)

    # PCA 2D Scores Plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pca_score2d, sprintf("PCA 2D scores plot showing the distribution of samples in the space defined by the first two principal components."))
    cat(slideContent, file = rmdFile, append = TRUE)

    # PCA biplot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pca.biplot, sprintf("PCA Biplot showing the distribution of samples and loading of variables on the first two principal components."))
    cat(slideContent, file = rmdFile, append = TRUE)

    # 3D PCA plot
    if(!is.null(mSetObj$imgSet$reportSet$pca_3d) && file.exists(mSetObj$imgSet$reportSet$pca_3d) ) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$pca_3d, sprintf("PCA Interactive 3D plot of the first three principal components."))
        cat(slideContent, file = rmdFile, append = TRUE)
    }
}

CreateOPLSDASlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the OPLS-DA process was executed
    if(is.null(mSetObj$analSet$oplsda)) {
        return()  # Exit if OPLS-DA was not performed
    }

    # OPLS-DA Description
    cat(paste0("# Orthogonal-Orthogonal Projections to Latent Structures Discriminant Analysis (OPLS-DA)",
               "\n\n---\n\n"), file = rmdFile, append = TRUE)
  
    # Score plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$opls.score2d, "OPLS-DA score plot of all metabolite features.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # S plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$opls.loading, "OPLS-DA loadings S-plot showing the variable importance in a model.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # Classification
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$opls.class, "Model overview of the OPLS-DA model for the provided dataset.")
    cat(slideContent, file = rmdFile, append = TRUE)
    
    # Permutation
    if(!is.null(mSetObj$imgSet$opls.permut)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$opls.permut, "Permutation analysis, showing the observed and cross-validated R2Y and Q2 coefficients.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }
}


CreateSPLSDASlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
  
    # Check if the sPLS-DA process was executed
    if(is.null(mSetObj$analSet$splsr)) {
        return()  # Exit if sPLS-DA was not performed
    }
  
    # sPLS-DA Description
    cat("# Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
  
  
    # Score plot overview
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$spls.pair, "Pairwise scores plots between the selected components.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # 2D score plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$spls.score2d, "Scores plot between the selected PCs.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # Loading VIP plot
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$spls.imp, "Plot showing the variables selected by the sPLS-DA model for a given component.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # 3D score plot, if available
    if(!is.null(mSetObj$imgSet$reportSet$splsda_3d) && file.exists(mSetObj$imgSet$reportSet$splsda_3d) ) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$splsda_3d, "Screenshot of interactive 3-D plot.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }
  
    # Cross-validation, if available
    if(!is.null(mSetObj$imgSet$splsda.class)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$splsda.class, "Plot of the performance of the sPLS-DA model evaluated using cross validations.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }
}


CreateSAMSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the SAM process was executed
    if(is.null(mSetObj$analSet$sam)) {
        return()  # Exit if SAM was not performed
    }

    # SAM Description
    cat("# Significance Analysis of Microarray (SAM)\n\n", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    
  
    # SAM significant features figure
    if(!is.null(mSetObj$imgSet$sam.cmpd)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$sam.cmpd, "Significant features identified by SAM. The green circles represent features that exceed the specified threshold.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }
  
    # SAM Table
    # Assuming GetSigTableRMD_SAM is a function returning significant features table
    if(!isEmptyMatrix(mSetObj$analSet$sam.cmpds)) {
        title_sam <- sprintf('## Table %d. Important features identified by Significance Analysis of Microarray analysis', getTableCount())
        cat(title_sam, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)

        r_code_chunk_sam <- paste0(
            "```{r table_sam, echo=FALSE, results='asis', warning=FALSE}\n",
            "dt_res <- as.data.frame(GetSigTableRMD_SAM(mSet));\n",
            "if(ncol(dt_res) != 1) {\n",
            "    create_dt(dt_res)\n",
            "}\n",
            "```\n",
            "\n\n---\n\n"
        )
        cat(r_code_chunk_sam, file = rmdFile, append = TRUE)

    }
}

CreateEBAMSlides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj)

    # Check if the EBAM process was executed
    if(is.null(mSetObj$analSet$ebam)){
        return()  # Exit if EBAM was not performed
    }

    # Introduction to EBAM
    descr <- c("# Empirical Bayesian Analysis of Microarray (EBAM)");
    cat(descr, file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)


    # EBAM figure and table
    if(!isEmptyMatrix(mSetObj$analSet$ebam.cmpds)){
        # Figure for EBAM
        ebamFigureDescription <- 'Significant features identified by EBAM. The green circles represent features that exceed the specified threshold.'
        ebamFigurePath <- mSetObj$imgSet$ebam.cmpd  # Ensure this is the correct path
        slideContent <- CreateTwoColumnFigureSlide(ebamFigurePath, ebamFigureDescription)
        cat(slideContent, file = rmdFile, append = TRUE)

        title_ebam <- sprintf('## Table %d. Important features identified by Empirical Bayesian Analysis of Microarray analysis', getTableCount())
        cat(title_ebam, file = rmdFile, append = TRUE)
        cat("\n\n", file = rmdFile, append = TRUE)

        r_code_chunk_ebam <- paste0(
            "```{r table_ebam_analysis, echo=FALSE, results='asis'}\n",
            "dt_res_ebam <- as.data.frame(GetSigTableRMD_EBAM(mSetObj))\n",
            "if(ncol(dt_res_ebam) > 1) {\n",
            "    create_dt(dt_res_ebam)\n",
            "}\n",
            "```\n",
            "\n\n---\n\n"
        )
        cat(r_code_chunk_ebam, file = rmdFile, append = TRUE)


    }
}

CreateHCSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the process is executed
    if (is.null(mSetObj$analSet$tree) && is.null(mSetObj$analSet$htmap)) {
        return()
    }

    cat("# Hierarchical Clustering analysis\n\n", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)


    # Create content for dendrogram if available
    if (!is.null(mSetObj$analSet$tree)) {
        dendrogramSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$tree, paste0("Clustering result shown as dendrogram (distance measure using `", 
                                                                         paste0(mSetObj$analSet$tree$dist.par, "` and clustering algorithm using `", 
                                                                         mSetObj$analSet$tree$clust.par, "`).")))
        # Append slide content to Rmd file
        cat(dendrogramSlide, file = rmdFile, append = TRUE, sep = "\n")
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }

    # Create content for heatmap if available
    if (!is.null(mSetObj$imgSet$reportSet$heatmap_static) && file.exists(mSetObj$imgSet$reportSet$heatmap_static)) {

        heatmapSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$heatmap_static, paste0("Clustering result shown as heatmap (distance measure using `", 
                                                                       paste0(mSetObj$analSet$htmap$dist.par, "` and clustering algorithm using `", 
                                                                       mSetObj$analSet$htmap$clust.par, "`).")))
        # Append slide content to Rmd file
        cat(heatmapSlide, file = rmdFile, append = TRUE, sep = "\n")
        cat("\n\n---\n\n", file = rmdFile, append = TRUE)
    }

}


CreateKMSlides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj)

    # Check if K-means clustering was executed
    if(is.null(mSetObj$analSet$kmeans)){
        return()  # Exit if K-means clustering was not performed
    }

    # Introduction to K-means Clustering
    descr <- c("# K-means Clustering",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # K-means clustering results figure
    kmeansDescription <- 'K-means cluster analysis. The x-axes are variable indices and y-axes are relative intensities...'
    kmeansPath <- mSetObj$imgSet$kmeans  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(kmeansPath, kmeansDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    # K-means PCA results figure
    kmeansPCADescription <- 'K-means cluster analysis, shown as PCA score plot...'
    kmeansPCAPath <- mSetObj$imgSet$kmeans.pca  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(kmeansPCAPath, kmeansPCADescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    # Define the title as a markdown heading and write it to the Rmd file
    title_km <- sprintf('## Table %d. Clustering result using K-means', getTableCount())
    cat(title_km, file = "presentation.Rmd", append = TRUE)
    cat("\n\n", file = "presentation.Rmd", append = TRUE)  # Ensures there is a newline after the title

    # Create the R code chunk for the table
    r_code_chunk_km <- paste0(
        "```{r table_km_analysis, echo=FALSE, results='asis'}\n",
        "dt_res_km <- as.data.frame(GetAllKMClusterMembersRMD(mSetObj))\n",
        "if(nrow(dt_res_km) > 0) {\n",
        "    create_dt(dt_res_km)\n",  # Removed caption parameter; title is handled separately
        "}\n",
        "```\n",
        "\n\n---\n\n"  # Slide/page separator for R Markdown documents
    )

    # Write the R code chunk to the R Markdown file for PowerPoint slides
    cat(r_code_chunk_km, file = "presentation.Rmd", append = TRUE)


}

CreateRFSlides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj)

    # Check if Random Forest analysis was executed
    if(is.null(mSetObj$analSet$rf)){
        return()  # Exit if Random Forest analysis was not performed
    }

    # Introduction to Random Forest
    descr <- c("# Random Forest (RF)",
               "\n\n---\n\n")
    cat(descr, file = rmdFile, append = TRUE)

    # Error rate figure
    rfErrorDescription <- 'Cumulative error rates by Random Forest classification...'
    rfErrorPath <- mSetObj$imgSet$rf.cls  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(rfErrorPath, rfErrorDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    # Variable importance figure
    rfImportanceDescription <- 'Significant features identified by Random Forest...'
    rfImportancePath <- mSetObj$imgSet$rf.imp  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(rfImportancePath, rfImportanceDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    # Outlier measure figure
    rfOutlierDescription <- 'Potential outliers identified by Random Forest...'
    rfOutlierPath <- mSetObj$imgSet$rf.outlier  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(rfOutlierPath, rfOutlierDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    title_rf <- sprintf('## Table %d. Random Forest Classification Performance', getTableCount())
    cat(title_rf, file = rmdFile, append = TRUE)
    cat("\n\n", file = rmdFile, append = TRUE)

    r_code_chunk_rf <- paste0(
        "```{r table_rf_analysis, echo=FALSE, results='asis'}\n",
        "dt_res_rf <- as.data.frame(GetRFConfTableRMD(mSetObj))\n",
        "if(nrow(dt_res_rf) > 0) {\n",
        "    create_dt(dt_res_rf)\n",
        "}\n",
        "```\n",
        "\n\n---\n\n"
    )
    cat(r_code_chunk_rf, file = rmdFile, append = TRUE)


}


CreateSVMSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
  
    # Check if the SVM process was executed
    if(is.null(mSetObj$analSet$svm)) {
        return()  # Exit if SVM was not performed
    }

    cat("# Support Vector Machine (SVM)\n\n", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    show=F
    if(show){
    # SVM Description
    cat("## Support Vector Machine (SVM)\n\n", file = rmdFile, append = TRUE)
    cat("SVM aims to find a nonlinear decision function in the input space by mapping the data into a higher dimensional feature space and separating it there by means of a maximum margin hyperplane. ",
        "The SVM-based recursive feature selection and classification is performed using R-SVM script. The process is performed recursively using decreasing series of feature subsets so that different classification models can be calculated. ",
        "Feature importance is evaluated based on its frequencies being selected in the best classifier identified by recursive classification and cross-validation. ",
        "Only the top 50 features (ranked by their p values from t-tests) will be evaluated.\n\n---\n\n", file = rmdFile, append = TRUE)
  
    }
    # SVM Classification Performance
    svmClassPath <- mSetObj$imgSet$svm.class  # Ensure this is the correct path
    svmClassCaption <- "Recursive classification with SVM. The red circle indicates the best classifier."
    slideContent <- CreateTwoColumnFigureSlide(svmClassPath, svmClassCaption)
    cat(slideContent, file = rmdFile, append = TRUE)
  
    # Significant Features by SVM
    svmFeaturesPath <- mSetObj$imgSet$svm  # Ensure this is the correct path
    svmFeaturesCaption <- "Significant features identified by R-SVM. Features are ranked by their frequencies of being selected in the classifier."
    slideContent <- CreateTwoColumnFigureSlide(svmFeaturesPath, svmFeaturesCaption)
    cat(slideContent, file = rmdFile, append = TRUE)
}

CreatePLSSlides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Check if the PLS process was executed
    if(is.null(mSetObj$analSet$plsr) && is.null(mSetObj$analSet$plsda)){
        return()
    }


    cat("# Partial Least Squares - Discriminant Analysis (PLS-DA)\n\n", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # Pairwise score plots
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.pair, "Pairwise scores plots between the selected components.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # 2D score plots
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.score2d, "Scores plot between the selected PCs.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # 3D scores plot, if available
    if(!is.null(mSetObj$imgSet$reportSet$plsda_3d) && file.exists(mSetObj$imgSet$reportSet$plsda_3d)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$reportSet$plsda_3d, "3D scores plot between selected components.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }

    # Loading plots
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.loading, "Loading plot between the selected PCs.")
    cat(slideContent, file = rmdFile, append = TRUE)

    # Classification performance
    if(!is.null(mSetObj$imgSet$pls.class)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.class, "Classification performance with different number of components.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }

    # Permutation test
    if(!is.null(mSetObj$imgSet$pls.permut)) {
        slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.permut, "Results of permutation test for model validation.")
        cat(slideContent, file = rmdFile, append = TRUE)
    }

    # VIP features
    slideContent <- CreateTwoColumnFigureSlide(mSetObj$imgSet$pls.imp, "Important features identified by PLS-DA.")
    cat(slideContent, file = rmdFile, append = TRUE)
}

CreateSOMSlides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj)

    # Check if SOM analysis was executed
    if(is.null(mSetObj$analSet$som)){
        return()  # Exit if SOM analysis was not performed
    }

    cat("# Self Organizing Map (SOM)\n\n", file = rmdFile, append = TRUE)
    cat("\n\n---\n\n", file = rmdFile, append = TRUE)

    # SOM clustering results figure
    somDescription <- 'SOM cluster analysis. The x-axes are features and y-axes are relative intensities...'
    somPath <- mSetObj$imgSet$som  # Ensure this is the correct path
    slideContent <- CreateTwoColumnFigureSlide(somPath, somDescription)
    cat(slideContent, file = rmdFile, append = TRUE)

    title_som <- sprintf('## Table %d. Clustering result using SOM', getTableCount())
    cat(title_som, file = rmdFile, append = TRUE)
    cat("\n\n", file = rmdFile, append = TRUE)

    r_code_chunk_som <- paste0(
        "```{r table_som_analysis, echo=FALSE, results='asis'}\n",
        "dt_res_som <- as.data.frame(GetAllSOMClusterMembersRMD(mSetObj))\n",
        "if(nrow(dt_res_som) > 0) {\n",
        "    create_dt(dt_res_som)\n",
        "}\n",
        "```\n",
        "\n\n---\n\n"
    )
    cat(r_code_chunk_som, file = rmdFile, append = TRUE)

}

