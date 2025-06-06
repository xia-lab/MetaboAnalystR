#'Create report for statistical analysis module
#'@description Report generation using Sweave
#'Write .Rmd file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateStatRmdReport <- function(mSetObj, usrName){

  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateStatIntr();
  
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  InitStatAnalMode();

  if(exists("analSet", where = mSetObj)){
    CreateUNIVdoc(mSetObj);
    CreateANOVAdoc(mSetObj);
    CreateCorrDoc(mSetObj);
    CreateDSPCdoc(mSetObj);
    CreateSAMdoc(mSetObj);
    CreateEBAMdoc(mSetObj);
    CreatePCAdoc(mSetObj);
    CreatePLSdoc(mSetObj);
    CreateOPLSDAdoc(mSetObj);
    CreateSPLSDAdoc(mSetObj);
    CreateHCdoc(mSetObj);
    CreateKMdoc(mSetObj);
    CreateSOMdoc(mSetObj);
    CreateRFdoc(mSetObj);
    CreateSVMdoc(mSetObj);
  }else{
    CreateAnalNullMsg();
  }
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create header
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateStatIntr <- function(){
  descr <- c("## 1. Overview \n",
             "The *Statistics [one factor]* module is designed for general-purpose exploratory analysis of metabolomics data with 
              a simple study design containing one experimental factor (two group or multiple group). A variety of statistics, visualization and machine learning methods are supported. 
              It accepts a data table from targeted metabolomics (i.e., concentration table) or untargeted metabolomics (i.e. peak intensity or binned 
              spectral table). You can also upload peak list files from NMR or GC/LC-MS. For raw LC-MS spectra data, 
              please use the *LC-MS Spectra Processing* module to first obtain a peak intensity table.
              For complex study design with many factors (treatment, time, gender, BMI, etc), please use the 
              *Statistics [metadata table]* module.\n") 
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Introduction for statistical analysis module report
#'Initialize Statistical Analysis Report
#'@export
InitStatAnalMode <- function(){

  cat("<hr/>", file=rmdFile, append=TRUE, sep="\n");

  descr <- c("## 3. Exploratory Statistical Data Analysis\n\n",
             "MetaboAnalyst offers a variety of methods commonly used for metabolomic data analyses.",
             "They include:\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  descr2 <- c(
    "1. Classical univariate analysis methods:",
    " + Fold Change Analysis",
    " + T-tests",
    " + Volcano Plot",
    " + One-way ANOVA and post-hoc analysis",
    " + Correlation analysis",
    "   - Correlation Heatmaps",
    "   - Pattern Search",
    "   - Correlation Networks (DSPC)",
    "2. Advanced significance analysis methods developed for omics data:",
    " + Significance Analysis of Microarray (SAM)",
    " + Empirical Bayesian Analysis of Microarray (EBAM)",
    "3. Multivariate analysis methods:",
    " + Principal Component Analysis (PCA)",
    " + Partial Least Squares - Discriminant Analysis (PLS-DA)",
    " + Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
    " + Orthogonal Partial Least Squares - Discriminant Analysis (orthoPLS-DA)",
    "4. Clustering Analysis:",
    " + Hierarchical Clustering",
    "   - Dendrogram",
    "   - Heatmap",
    " + Partitional Clustering",
    "   - K-means Clustering",
    "   - Self-Organizing Map (SOM)",
    "5. Supervised Classification and Feature Selection methods:",
    " + Random Forest",
    " + Support Vector Machine (SVM)",
    "\n",
    "*Please note some methods are available only for two-group analysis*");
  cat(descr2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
}


#'Create null message for analysis
#'Creates a message for the Sweave report
#'@description Creates a message stating that no analyses were performed on your data.
#'@export
CreateAnalNullMsg <- function(){
  descr <- c("No analysis was performed on your data.\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create univariate analyses document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateUNIVdoc <- function(mSetObj=NA){
  link <- GetSharingLink(mSetObj)
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$fc) & is.null(mSetObj$analSet$tt) & is.null(mSetObj$analSet$volcano)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$fc$sig.mat)){
    fc.img <- fc.tab<-NULL;
  }else{
    fc.img <- paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by fold change analysis.");
    fc.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
    fig_fc <- fig.count;
    table_fc <- table.count;
  }
  if(isEmptyMatrix(mSetObj$analSet$tt$sig.mat)){
    tt.img <- tt.tab<-NULL;
  }else{
    if(!is.null(mSetObj$imgSet$tt)){
      tt.img <- paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by t-tests.");
    } else {
      tt.img <- NULL;
    }
    tt.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
    fig_tt <- fig.count;
    table_tt <- table.count;
  }
  if(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat)){
    volcano.img <- volcano.tab<-NULL;
  }else{
    volcano.img <-paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by volcano plot.");
    volcano.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features.");
    fig_vc <- fig.count;
    table_vc <- table.count;
  }
  
  descr <- c("### Univariate Analysis\n\n",
             "Univariate methods are very useful to get a quick overview of significant features and their patterns of change.
             For two-group data, MetaboAnalyst provides Fold Change (FC) analysis, t-tests, and volcano
             plot which is a combination of the first two methods. All three these methods support both 
             unpaired and paired analyses. For multi-group analysis, MetaboAnalyst provides two types of
             analysis - one-way analysis of variance (ANOVA) with associated post-hoc analyses, and correlation
             analysis to identify significant features that follow a pattern of interest. Univariate analyses 
             is a starting point before conducting more advanced analysis.",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  
  # Fold change
  if(!(isEmptyMatrix(mSetObj$analSet$fc$sig.mat))){

    if(!is.null(mSetObj$imgSet$fc)){
        # fold change view
        descr <- c("#### - Fold Change Analysis\n\n",
                    "Important features selected by fold-change analysis with FC threshold ```", 
                    mSetObj$analSet$fc$raw.thresh, "```. ",
                    "The red circles represent features above the threshold. Note the values are on log scale, so that both up-regulated 
                    and down-regulated features can be plotted in a symmetrical way. A total of ```", 
                    nrow(mSetObj$analSet$fc$sig.mat), "``` significant features were identified.",
               "\n\n");

        cat(descr, file=rmdFile, append=TRUE, sep="\n");

        link <- GetSharingLink(mSetObj)
        reportLinks <- getReportLinks(link, "fc", "fc");

        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        cat("<div style='text-align: center;'>\n\n", file=rmdFile, append=TRUE);

        fig <- c(paste0("```{r figure_fc, echo=FALSE,  fig.pos='H', fig.cap='Figure ", fig_fc, 
                    ". Important features selected by fold-change analysis',", 
                    " fig.lp='", 
                    mSetObj$imgSet$fc, 
                    "', out.width = '75%', out.height='650px'}"),
                "if (mSetObj$paramSet$report.format == 'html') {",
                   "PlotFC(NA, '', 'png', 72, NA, interactive=T)",
                "} else {",
                "  knitr::include_graphics(mSetObj$imgSet$fc)",
                "}",
             "```",
             "\n\n");
        cat(fig, file=rmdFile, append=TRUE, sep="\n");

        cat("</div>\n\n", file=rmdFile, append=TRUE);

        reportLinks <- getReportLinks(link, "fc_table");

        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        descr <- c(
            "```{r table_fc, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
            "dt_res <- as.data.frame(GetSigTableRMD_FC(mSet));",
            paste0("create_dt(dt_res,  caption = 'Table ", 
             table_fc, 
             ". Result from Fold Change Analysis.')"),
             "```", "\n\n")
    
        cat(descr, file=rmdFile, append=TRUE, sep="\n");

    }
  }
  
  # T-tests
  if(!(isEmptyMatrix(mSetObj$analSet$tt$sig.mat))){

    if(!is.null(mSetObj$imgSet$tt)){
        descr <- c("#### T-tests\n\n",
               "Important features selected by ```", mSetObj$analSet$tt$tt.nm, "``` with threshold ```", 
               mSetObj$analSet$tt$raw.thresh, "```, type ```", mSetObj$analSet$tt$pval.type, "```. ",
               "A total of ```", mSetObj$analSet$tt$sig.num, "``` significant features were identified.",
               "\n\n");
        cat(descr, file=rmdFile, append=TRUE, sep="\n");

        # tt view
        reportLinks <- getReportLinks(link, "tt", "tt");
        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        cat("<div style='text-align: center;'>\n\n", file=rmdFile, append=TRUE);

        fig <- c(paste0("```{r figure_tt, echo=FALSE,  fig.pos='H', fig.cap='Figure ", fig_tt, 
                      ". Significant features selected by T-tests. The p values are transformed by -log10 so that the more significant features are higher on the graph.',", 
                      " fig.lp='", mSetObj$imgSet$tt, 
                      "', out.width = '75%', out.height = '650px'}"),
                "if (mSetObj$paramSet$report.format == 'html') {",
                   "PlotTT(NA, '', 'png', 72, NA, interactive=T)",
                "} else {",
                "  knitr::include_graphics(mSetObj$imgSet$tt)",
                "}",
               "```",
               "\n\n");

        cat(fig, file=rmdFile, append=TRUE, sep="\n");
        cat("</div>\n\n", file=rmdFile, append=TRUE);

        reportLinks <- getReportLinks(link, "tt_table");

        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        descr <- c(
            "```{r table_tt, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
            "dt_res <- as.data.frame(GetSigTableRMD_TT(mSet));",
            paste0("create_dt(dt_res,  caption = 'Table ", 
            table_tt, ". Result from T test Analysis.')"),
            "```", "\n\n");
        cat(descr, file=rmdFile, append=TRUE, sep="\n"); 
    }
  }
  
  # Volcano plot
  if(!(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat))){
    # volcano view
     descr <- c("#### - Volcano Plot\n\n",
               "Important features selected by FC (x-axis) with threshold ```", 
               mSetObj$analSet$volcano$raw.threshx, "```, and by t-tests p-value (y-axis) with threshold ```", mSetObj$analSet$volcano$raw.threshy, 
               "```, type ```", mSetObj$analSet$volcano$pval.type, "```.  A total of ```", nrow(mSetObj$analSet$volcano$sig.mat), 
               "``` significant features were identified. Note both fold changes and p values are log transformed. 
               The further its position away from the (0,0), the more significant the feature is.",
               "\n\n");
      cat(descr, file=rmdFile, append=TRUE, sep="\n");


    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "volcano", "volcano");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    plotLabel = mSetObj$analSet$volcano.plot.config$plotLbl
    plotTheme = mSetObj$analSet$volcano.plot.config$plotTheme
    cat("<div style='text-align: center;'>\n\n", file=rmdFile, append=TRUE);

    fig <- c(paste0("```{r figure_volcano, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_vc, 
                    ". Important features selected by volcano plot', ",
                    " fig.lp='", 
                    mSetObj$imgSet$volcano, 
                    "', out.width = '", getFigWidth(mSetObj), "', out.height='650px'}"),
                "if (mSetObj$paramSet$report.format == 'html') {",
                   "PlotVolcano(NA, '', mSet$analSet$volcano.plot.config$plotLbl, mSet$analSet$volcano.plot.config$plotTheme, 'png', 72, NA, interactive=T)",
                "} else {",
                "  knitr::include_graphics(mSetObj$imgSet$volcano)",
                "}",
             "```",
             "\n\n");
    
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("</div>\n\n", file=rmdFile, append=TRUE);

    
    reportLinks <- getReportLinks(link, "volcano_table");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    descr <- c(
      "```{r table_vc, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(GetSigTableRMD_Volcano(mSet));",
      paste0("create_dt(dt_res,  caption = 'Table ", 
             table_vc, 
             ". Result from volcano plotting.')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create ANOVA document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateANOVAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$aov)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$aov$sig.mat)){
    anova.tab<-NULL;
  } else {
    anova.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.",
                       "The *post-hoc Sig. Comparison* column shows the comparisons between different levels",
                       "that are significant given the p value threshold. ");
  }
  # \n\n## 
  descr <- c("#### - One-way ANOVA\n\n",
             "Univariate analysis methods are the most common methods used for exploratory data analysis. ",
             "For multi-group analysis, MetaboAnalyst provides one-way Analysis",
             "of Variance (ANOVA). As ANOVA only tells whether the overall comparison is significant or not,",
             "it is usually followed by post-hoc analyses in order to identify which two levels are different.",
             "MetaboAnalyst provides two most commonly used methods for this purpose - Fisher's",
             "least significant difference method (Fisher's LSD) and Tukey's Honestly Significant Difference",
             "(Tukey's HSD). The univariate analyses provide a preliminary overview about features that are",
             "potentially significant in discriminating the conditions under study.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by ANOVA analysis."),
             anova.tab,
             "\n\n");

  cat(descr, file=rmdFile, append=TRUE);
  
  cat("<div style='text-align: center;'>\n\n", file=rmdFile, append=TRUE);
  # ANOVA
  fig <- c(
      paste0("```{r figure_anov, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
             ". Important features selected by ANOVA plot with p value threshold ", 
             mSetObj$analSet$aov$raw.thresh, ".', ",
             "fig.lp='fig:", "anova', ", # Customize the label pointer as needed
             "out.width='", getFigWidth(mSetObj), "'}"),
      "if (mSetObj$paramSet$report.format == 'html') {",
      "  PlotANOVA(NA, interactive=T)",
      "} else {",
      "  knitr::include_graphics(mSetObj$imgSet$anova)",
      "}",
      "```",
      "\n\n"
    )

  
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("</div>\n\n", file=rmdFile, append=TRUE);

  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  descr <- c(
    "```{r table_anov, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetSigTableRMD_Anova(mSet));",
    paste0("if(ncol(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Important features identified by One-way ANOVA and post-hoc analysis.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  addPageBegin();
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create correlation document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCorrDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  link <- GetSharingLink(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$imgSet$heatmap_stats_corr_param)){
    return();
  }
  
  # need to check if this process is executed
  if(!is.null(mSetObj$imgSet$heatmap_stats_corr_param)){
    descr <- c("#### - Correlation Analysis - Heatmaps \n\n",
               "Correlation analysis can be used to visualize the overall correlations between different features or samples based on",
               "Pearson r, Spearman rank correlation or Kendall rank correlation method. You can further apply a correlation cutoff to view",
               "only strong correlations while keep the remaining as consistent background.",
               "The heatmaps can show correlations for a maximum of 1000 features. For larger data, only the top 1000 features",
               "will be selected based on their interquantile range (IQR).",
               #"\n\n",
               #paste("Figure", fig.count<<-fig.count+1, "shows the overall correlation heatmap."),
               "\n\n");
    
    cat(descr, file=rmdFile, append=TRUE);
    
    # corr view
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "correlation_heatmap", "corr");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    fig <- "";
    if (mSetObj$paramSet$report.format == 'html'){
        fig <- c(paste0("```{r figure_corr1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count<<-fig.count+1, 
                        ". Correlation Heatmaps.', ",
                        " fig.lp='", 
                        mSetObj$imgSet$corr.heatmap, 
                        "', out.width = '",mSetObj$imgSet$heatmap_stats_corr_param$width,"px', out.height = '",mSetObj$imgSet$heatmap_stats_corr_param$width,"px'}"),
                          "p <- readRDS('heatmap_stats_corr.rds')",
                          "p",
                 "```",
                 "\n\n");
      }else if(file.exists(mSetObj$imgSet$corr.heatmap)){
        fig <- c(paste0("```{r figure_corr1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
                        ". Correlation Heatmaps.', ",
                        " fig.lp='", 
                        mSetObj$imgSet$corr.heatmap, 
                        "', out.width = '90%'}"),
                          "knitr::include_graphics(mSetObj$imgSet$corr.heatmap)",
                 "```",
                 "\n\n");
      }

    cat(fig, file=rmdFile, append=TRUE, sep="\n\n");
  }
  
  if(!is.null(mSetObj$analSet$corr$cor.mat)){
    if(isEmptyMatrix(mSetObj$analSet$corr$cor.mat)){
      cor.tab <- NULL;
    }else{
      cor.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
    }
    
    
    descr <- c("#### - Correlation Analysis - Pattern Hunter\n\n",
               "Correlation analysis can be used to identify which features are correlated with a feature of interest.",
               "Correlation analysis can also be used to identify if certain features show particular patterns",
               "under different conditions. Users first need to define a pattern in the form of a series of hyphenated numbers.",
               "For example, in a time-series study with four time points, a pattern of of",
               "*1-2-3-4* is used to search compounds with increasing the concentration as",
               "time changes; while a pattern of *3-2-1-3* can be used to search compounds",
               "that decrease at first, then bounce back to the original level.",
               "\n\n",
               paste("Figure", fig.count<<-fig.count+1, "shows the important features identified by correlation analysis."),
               cor.tab,
               "\n");
    
    cat(descr, file=rmdFile, append=TRUE);
    
    # ptn view
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "ptn", "ptn");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig <- c(paste0("```{r figure_corr2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count, 
                    ". Important features selected by correlation analysis with light purple indicates positive correlation and blue indicate negative correlations.', ",
                    " fig.lp='", 
                    mSetObj$imgSet$corr, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "knitr::include_graphics(mSetObj$imgSet$corr)",
             "```",
             "\n\n");
    
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    
    reportLinks <- getReportLinks(link, "ptn_table");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    descr <- c(
      "```{r table_corr, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(GetSigTableRMD_Corr(mSet));",
      paste0("if(ncol(dt_res) != 1) {",
             "create_dt(dt_res,  caption = 'Table ", 
             table.count, 
             ". Important features identified by Pattern search using correlation analysis.')}"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n");
  }
  
  cat("\\clearpage", file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create PCA document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePCAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$pca)){
    return();
  }
  
  descr <- c("#### - Principal Component Analysis (PCA)\n\n",
             "PCA is an unsupervised method aiming to find the directions that best",
             "explain the variance in a data set (X) without referring to class labels (Y).",
             "The data are summarized into much fewer variables called *scores* which",
             "are weighted average of the original variables. The weighting profiles are called",
             "*loadings*. The PCA analysis is performed using the *prcomp* package.",
             "The calculation is based on singular value decomposition.",
             "\n\n",
             paste("Figure", fig_pw <- fig.count<<-fig.count+1,"is pairwise score plots providing an overview of the various seperation patterns among the most significant PCs;"),
             paste("Figure", fig_st <- fig.count<<-fig.count+1,"is the scree plot showing the variances explained by the selected PCs;"),
             paste("Figure", fig_2s <- fig.count<<-fig.count+1,"shows the 2-D scores plot between selected PCs;"),
             #paste("Figure", fig.count<<-fig.count+1,"shows the 3-D scores plot between selected PCs;"),
             #paste("Figure", fig.count<<-fig.count+1,"shows the loadings plot between the selected PCs;"),
             paste("Figure", fig_bp <- fig.count<<-fig.count+1,"shows the biplot between the selected PCs.\n"),
             paste("Figure", fig_3d <- fig.count<<-fig.count+1,"shows screenshot of interactive 3-D plot.\n"));
  
  cat(descr, file=rmdFile, append=TRUE);
  
  # pca view
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pca_pair", "pca_pair");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r figure_pca1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pw, 
                  ". Pairwise score plots between the selected PCs. The explained variance of each PC is shown in the corresponding diagonal cell.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$pca.pair, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics(mSetObj$imgSet$pca.pair)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # pca scree
  link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "pca_scree", "pca_scree");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_pca2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_st, 
                  ". Scree plot shows the variance explained by PCs. The green line on top shows the accumulated variance explained; ",
                  " the blue line underneath shows the variance explained by individual PC.',",
                  " fig.lp='", 
                  mSetObj$imgSet$pca.scree, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics(mSetObj$imgSet$pca.scree)",
           "```",
           "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # pca scree
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pca_score2d", "pca_score2f");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_pca3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_2s, 
                  ". Scores plot between the selected PCs. The explained variances are shown in brackets.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$pca.score2d, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics(mSetObj$imgSet$pca.score2d)",
           "```",
           "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # pca biplot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pca_biplot", "pca_biplot");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig4 <- c(paste0("```{r figure_pca4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_bp, 
                   ". PCA biplot between the selected PCs. Note, you may want to test different centering and scaling",
                   " normalization methods for the biplot to be displayed properly. ',",
                   " fig.lp='", 
                   mSetObj$imgSet$pca.biplot, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$pca.biplot)",
            "```",
            "\n\n");
  cat(fig4, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  if(!is.null(mSetObj$imgSet$reportSet$pca_3d) && file.exists(mSetObj$imgSet$reportSet$pca_3d) ){
  reportLinks <- getReportLinks(link, "pca_3d", "pca_3d");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    fig5 <- c(paste0("```{r figure_pca5, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_3d, 
                     ". Screenshot of interactive 3-D plot.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$pca_3d, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$reportSet$pca_3d)",
              "```",
              "\n\n");
    cat(fig5, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  ###
   # "\\begin{figure}[htp]",
   # "\\begin{center}",
   # paste("\\includegraphics[width=1.0\\textwidth]{", "scores3D.png","}", sep=""),
   # "\\caption{3D score plot between the selected PCs. The explained variances are shown in brackets.}",
   # "\\end{center}",
   # paste("\\label{","scores3D.png","}", sep=""),
   # "\\end{figure}",
   # "\\begin{figure}[htp]",
   # "\\begin{center}",
   # paste("\\includegraphics[width=1.0\\textwidth]{", "loadings3D.png","}", sep=""),
   # "\\caption{Loadings plot for the selected PCs. }",
   # "\\end{center}",
   # paste("\\label{","loadings3D.png","}", sep=""),
   # "\\end{figure}",
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create PLS document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePLSdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$plsr) & is.null(mSetObj$analSet$plsda)){
    return();
  }
  
  descr <- c("#### - Partial Least Squares - Discriminant Analysis (PLS-DA)\n\n",
             "PLS is a supervised method that uses multivariate regression techniques to extract via",
             "linear combination of original variables (X) the information that can predict the",
             "class membership (Y). The PLS regression is performed using the *plsr* function",
             "provided by R [*pls* package](https://cran.r-project.org/web/packages/pls/index.html).",
             "The classification and cross-validation are performed using the corresponding wrapper",
             "function offered by the [*caret* package](https://cran.r-project.org/web/packages/caret/).",
             "\n\n",
             "To assess the significance of class discrimination, a permutation test was performed. In each permutation, a PLS-DA model was",
             "built between the data (X) and the permuted class labels (Y) using the optimal number of components determined",
             "by cross validation for the model based on the original class assignment. MetaboAnalyst supports two types of test",
             "statistics for measuring the class discrimination. The first one is based on prediction accuracy during training.",
             "The second one is separation distance based on the ratio of the between group sum of the squares and the within",
             "group sum of squares (B/W-ratio).",
             "If the observed test statistic is part of the distribution based on the permuted class assignments,",
             "the class discrimination cannot be considered significant from a statistical point of",
             "view ([Bijlsma et al.](https://pubs.acs.org/doi/10.1021/ac051495j)).",
             "\n\n",
             "There are two variable importance measures in PLS-DA. The first, Variable Importance in Projection (VIP) is",
             "a weighted sum of squares of the PLS loadings taking into account the amount of explained Y-variation",
             "in each dimension. Please note, VIP scores are calculated for each components. When more than components are used to calculate", 
             "the feature importance, the average of the VIP scores are used. The other importance measure is based",
             "on the weighted sum of PLS-regression. The weights are a function of the reduction of the sums of squares across the number",
             "of PLS components. Please note, for multiple-group (more than two) analysis, the same number of predictors will be built for each",
             "group. Therefore, the coefficient of each feature will be different depending on which group you want to predict.",
             "The average of the feature coefficients are used to indicate the overall coefficient-based importance. ",
             "\n\n",
             paste("Figure", fig_pls_ov <- fig.count<<-fig.count+1,"shows the overview of scores plots;"),
             paste("Figure", fig_pls_2d <- fig.count<<-fig.count+1,"shows the 2-D scores plot between selected components;"),
             paste("Figure", fig_pls_3d <- fig.count<<-fig.count+1,"shows the 3-D scores plot between selected components;"),
             paste("Figure", fig_pls_ld <- fig.count<<-fig.count+1,"shows the loading plot between the selected components;"));
  cat(descr, file=rmdFile, append=TRUE);
  
  descr <- c()
  if(!is.null(mSetObj$imgSet$pls.class)){
    descr <- paste("Figure", fig_pls_cp <- fig.count<<-fig.count+1,"shows the classification performance with different number of components;");
  } 
  if(!is.null(mSetObj$imgSet$pls.permut)){
    descr <- c(descr,
               paste("Figure", fig_pls_mv <- fig.count<<-fig.count+1,"shows the results of permutation test for model validation;"),
               paste("Figure", fig_pls_if <- fig.count<<-fig.count+1,"shows important features identified by PLS-DA.\n"));
    
  } else {
    descr <- c(descr,
               paste("Figure", fig_pls_if <- fig.count<<-fig.count+1,"shows important features identified by PLS-DA.\n"));
  }

  cat(descr, file=rmdFile, append=TRUE);
  
  ### overview
  # PLSDA view
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pls_pair", "pls_pair");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_pls1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_ov, 
                  ". Pairwise scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$pls.pair, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics(mSetObj$imgSet$pls.pair)",
           "```",
           "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ### 2d
  # PLSDA 2D score
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pls_score2d", "pls_score2d");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_pls2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_2d, 
                   ". Scores plot between the selected PCs. The explained variances are shown in brackets.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$pls.score2d, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$pls.score2d)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ### 3d
  if(!is.null(mSetObj$imgSet$reportSet$plsda_3d) && file.exists(mSetObj$imgSet$reportSet$plsda_3d) ){
  reportLinks <- getReportLinks(link, "plsda_3d");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    fig3 <- c(paste0("```{r figure_pls_3d, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_3d, 
                     ". Screenshot of interactive 3-D plot.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$plsda_3d, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$reportSet$plsda_3d)",
              "```",
              "\n\n");
    cat(fig3, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  ### loading
  # Loading of PLSDA
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pls_loading", "pls_loading");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig4 <- c(paste0("```{r figure_pls4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_ld, 
                   ". Loadings plot between the selected PCs.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$pls.loading, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$pls.loading)",
            "```",
            "\n\n");
  cat(fig4, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  
  ## Optional plots - classification + permutation
  ### classification
  if(!is.null(mSetObj$imgSet$pls.class)){
    # pls_cv
    link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pls_cv", "pls_cv");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig5 <- c(paste0("```{r figure_pls5, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_cp, 
                     ". PLS-DA classification using different number of components. The red star indicates the best classifier.', ",
                     " fig.lp='", 
                     mSetObj$imgSet$pls.class, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$pls.class)",
              "```",
              "\n\n");
    cat(fig5, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  ### permutation
  if(!is.null(mSetObj$imgSet$pls.permut)){ # may not be performed (not by default)
    
    # pls_perm
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "pls_perm", "pls_perm");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig6 <- c(paste0("```{r figure_pls6, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_mv, 
                     ". PLS-DA model validation by permutation tests based on ", mSetObj$analSet$plsda$permut.type, ". ",
                     "The p value based on permutation is ", 
                     mSetObj$analSet$plsda$permut.p, 
                     ".', ",
                     " fig.lp='", 
                     mSetObj$imgSet$pls.permut, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$pls.permut)",
              "```",
              "\n\n");
    cat(fig6, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  ### VIP
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pls_imp", "pls_imp");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig7 <- c(paste0("```{r figure_pls7, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pls_if, 
                   ". Important features identified by PLS-DA. ",
                   "The colored boxes on the right indicate the relative concentrations of the corresponding metabolite in each group under study.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$pls.imp, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$pls.imp)",
            "```",
            "\n\n");
  cat(fig7, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create sPLS-DA document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateSPLSDAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$splsr)){
    return();
  }
  
  descr <- c("#### - Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)\n\n",
             "The sparse PLS-DA (sPLS-DA) algorithm can be used to effectively reduce the number of variables (metabolites)", 
             "in high-dimensional metabolomics data to produce robust and easy-to-interpret models.", 
             "Users can control the sparseness of the model by controlling the number of components in the model and the number ",
             "of variables in each component. For more information, please refer to 
             [Cao et al. 2011 (PMC3133555)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3133555/). ",
             "\n\n",
             paste("Figure", fig_spls_sc <- fig.count<<-fig.count+1,"shows the overview of scores plots;"),
             paste("Figure", fig_spls_2d <- fig.count<<-fig.count+1,"shows the 2-D scores plot between selected components;"),
             paste("Figure", fig_spls_lp <- fig.count<<-fig.count+1,"shows the loading plot of the top ranked features;"),
             paste("Figure", fig_spls_3d <- fig.count<<-fig.count+1,"shows the 3-D scores plot between selected components;"));
  
  if(!is.null(mSetObj$imgSet$splsda.class)){
    descr <- c(descr,
               paste("Figure", fig_spls_cv <- fig.count<<-fig.count+1,"shows the performance of the sPLS-DA model evaluated using cross-validations;"))
  }
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ## score plot overview
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "spls_pair", "spls_pair");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_spls1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_spls_sc, 
                   ". Pairwise scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$spls.pair, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$spls.pair)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ## 2d score plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "spls_score2d", "spls_score2d");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_spls2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_spls_2d, 
                   ". Scores plot between the selected PCs. The explained variances are shown in brackets.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$spls.score2d, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$spls.score2d)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ## loading VIP plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "spls_loading", "spls_loading");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_spls3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_spls_lp, 
                   ". Plot showing the variables selected by the sPLS-DA model for a given component. The variables are ranked by the absolute values of their loadings.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$spls.imp, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$spls.imp)",
            "```",
            "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ## 3d score plot
  if(!is.null(mSetObj$imgSet$reportSet$splsda_3d) && file.exists(mSetObj$imgSet$reportSet$splsda_3d) ){
  reportLinks <- getReportLinks(link, "splsda_3d");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    fig4 <- c(paste0("```{r figure_spls_3d, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_spls_3d, 
                     ". Screenshot of interactive 3-D plot.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$splsda_3d, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$reportSet$splsda_3d)",
              "```",
              "\n\n");
    cat(fig4, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  ## cross-validation
  if(!is.null(mSetObj$imgSet$splsda.class)){
    # spls_cv
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "spls_cv", "spls_cv");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig5 <- c(paste0("```{r figure_spls5, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_spls_cv, 
                     ". Plot of the performance of the sPLS-DA model evaluated using cross validations (CV) with increasing numbers ", 
                     "of components created using the specified number of the variables. The error rate is on the y-axis and the number of components ",
                     "is on the x-axis.', ",
                     " fig.lp='", 
                     mSetObj$imgSet$splsda.class, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$splsda.class)",
              "```",
              "\n\n");
    cat(fig5, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create OPLSDA document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateOPLSDAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$oplsda)){
    return();
  }
  
  descr <- c("#### - Orthogonal-Orthogonal Projections to Latent Structures Discriminant Analysis (OPLS-DA)\n\n", 
             "OPLS-DA, like PLS-DA, is a powerful tool used for dimension reduction and identification of spectral features that ",
             "drive group separation. It is a supervised modeling method, and may be used instead of PLS-DA due to its capablitities ",
             "to distinguish between variations in a dataset relevant to predicting group-labels and variations irrelevant",
             " to predicting group-labels. In this sense, OPLS-DA tends to make models that are less complex and more insightful",
             " than PLS-DA. However, both OPLS-DA and PLS-DA are prone to create models that over-fit data, therefore requiring ",
             "cross-validation to ensure model reliability. For further details, please refer to Worley and Powers 2013 (PMC4465187) ",
             "and Worley and Powers 2016 (PMC4990351). The permutation testing for OPLS-DA is provided from ",
             "[Szymanska et al. 2012](https://link.springer.com/article/10.1007/s11306-011-0330-3).",
             "\n\n",
             paste("Figure", fig_ops <- fig.count<<-fig.count+1,"shows the score plot for all metabolite features;"),
             paste("Figure", fig_opv <- fig.count<<-fig.count+1,"shows the variable importance in an OPLS-DA model;"),
             paste("Figure", fig_opm <- fig.count<<-fig.count+1,"shows the model overview;")
             );
  
  if(!is.null(mSetObj$imgSet$opls.permut)){
    descr <- c(descr, 
               paste("Figure", fig_opp <- fig.count<<-fig.count+1,"shows the results of the permutation tests for the models;"))
  }
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # score plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "opls_score2d", "opls_score2d");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_opls1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ops, 
                  ". OPLS-DA score plot of all metabolite features.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$opls.score2d, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "knitr::include_graphics(mSetObj$imgSet$opls.score2d)",
           "```",
           "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # s plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "opls_splot", "opls_splot");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_opls2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_opv, 
                   ". OPLS-DA loadings S-plot showing the variable importance in a model, combining the covariance and the correlation (p(corr)) loading profile.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$opls.loading, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$opls.loading)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # classification
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "opls_mdl", "opls_mdl");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_opls3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_opm, 
                   ". Model overview of the OPLS-DA model for the provided dataset. It shows the R2X, R2Y, and Q2 coefficients for the groups.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$opls.class, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$opls.class)",
            "```",
            "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # permutation
  if(!is.null(mSetObj$imgSet$opls.permut)){
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "opls_perm", "opls_perm");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig4 <- c(paste0("```{r figure_opls4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_opp, 
                     ". Permutation analysis, showing the observed and cross-validated R2Y and Q2 coefficients.', ",
                     " fig.lp='", 
                     mSetObj$imgSet$opls.permut, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$opls.permut)",
              "```",
              "\n\n");
    cat(fig4, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    
  }
  
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create SAM document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateSAMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$sam)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$sam.cmpds)){
    sam.tab <- NULL;
  }else{
    sam.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
  }
  
  descr <- c("#### - Significance Analysis of Microarray (SAM)\n\n",
             "SAM is a well-established statistical method for identification",
             "of differentially expressed genes in microarray data analysis. It is designed",
             "to address the false discovery rate (FDR) when running multiple tests on high-dimensional",
             "microarray data. SAM assigns a significance score to each variable based on its change",
             "relative to the standard deviation of repeated measurements. For a variable with scores",
             "greater than an adjustable threshold, its relative difference is compared to the",
             "distribution estimated by random permutations of the class labels. For each threshold,",
             "a certain proportion of the variables in the permutation set will be found to be significant",
             "by chance. The proportion is used to calculate the FDR. SAM is performed using the",
             "[*siggenes* package](https://www.bioconductor.org/packages/release/bioc/html/siggenes.html).",
             "Users need to specify the *Delta* value to control FDR in order to proceed.",
             "\n\n",
             paste("Figure", fig_sam <- fig.count<<-fig.count+1,"shows the significant features identified by SAM."),
             sam.tab,
             "\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  
  
  ## Figure SAM
  # SAM view
  link <- GetSharingLink(mSetObj)

    reportLinks <- getReportLinks(link, "sam", "sam_imp");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_sam1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_sam, 
                   ".Significant features identified by SAM. The green circles represent features that exceed the specified threshold.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$sam.cmpd, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$sam.cmpd)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
    reportLinks <- getReportLinks(link, "sam_table");

    cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  ## Table sam
  descr <- c(
    "```{r table_sam, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetSigTableRMD_SAM(mSet));",
    paste0("if(ncol(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Important features identified by Significance Analysis of Microarray analysis.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");

}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create EBAM document
#'Note: the search for delta (SAM) and a0 (EBAM) will not be plotted
#'it is only exploration, and may cause potential inconsistentcies. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateEBAMdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$ebam)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$ebam.cmpds)){
    ebam.tab<-NULL;
  }else{
    ebam.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
  }
  
  descr <- c("#### - Empirical Bayesian Analysis of Microarray (EBAM)\n\n",
             "EBAM is an empirical Bayesian method based on moderated t-statistics.",
             "EBAM uses a two-group mixture model for null and significant features.",
             "The prior and density parameters are estimated from the data. A feature is",
             "considered significant if its calculated posterior is larger than or equal to",
             "*delta* and no other features with a more extreme test score that",
             "is not called signicant. The default is *delta* = 0.9.",
             "The suggested fudge factor (*a0*) is chosen that leads to the largest number",
             "of significant features. EBAM is performed with *ebam* function in",
             "[*siggenes* package](https://www.bioconductor.org/packages/release/bioc/html/siggenes.html)",
             "\n\n",
             paste("Figure", fig_ebam <- fig.count<<-fig.count+1," shows the important features identified by EBAM."),
             ebam.tab,
             "\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "ebam", "ebam_imp");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_ebam1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ebam, 
                   ".Significant features identified by EBAM. The green circles represent features that exceed the specified threshold.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$ebam.cmpd, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$ebam.cmpd)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  reportLinks <- getReportLinks(link, "ebam_table");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  ## Table ebam
  descr <- c(
    "```{r table_ebam, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetSigTableRMD_EBAM(mSet));",
    paste0("if(ncol(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Important features identified by Empirical Bayesian Analysis of Microarray analysis.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create hierarchical clustering document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateHCdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$tree) & is.null(mSetObj$analSet$htmap)){
    return();
  }
  
  descr <- c("#### - Hierarchical Clustering\n\n",
             "In (agglomerative) hierarchical cluster analysis, each sample begins
             as a separate cluster and the algorithm proceeds to combine them until all 
             samples belong to one cluster. Two parameters need to be considered when performing
             hierarchical clustering. The first one is similarity measure - Euclidean distance,
             Pearson's correlation, Spearman's rank correlation. The other parameter is clustering 
             algorithms, including average linkage (clustering uses the centroids of the observations),
             complete linkage (clustering uses the farthest pair of observations between the two groups),
             single linkage (clustering uses the closest pair of observations) and Ward's linkage 
             (clustering to minimize the sum of squares of any two clusters). Heatmap is often presented
             as a visual aid in addition to the dendrogram.",
             "\n\n",
             paste("Figure", fig_hc <- fig.count<<-fig.count+1,"shows the clustering result in the form of a dendrogram."),
             paste("Figure", fig_cr <- fig.count<<-fig.count+1,"shows the clustering result in the form of a heatmap.\n"));
  
  cat(descr, file=rmdFile, append=TRUE);
  
  if(!is.null(mSetObj$analSet$tree)){
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "tree", "tree");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig1 <- c(paste0("```{r figure_hc1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_hc, 
                     ".Clustering result shown as dendrogram (distance measure using", 
                     mSetObj$analSet$tree$dist.par,
                     " and clustering algorithm using ",
                     mSetObj$analSet$tree$clust.par,
                     ").', ",
                     " fig.lp='", 
                     mSetObj$imgSet$tree, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "knitr::include_graphics(mSetObj$imgSet$tree)",
              "```",
              "\n\n");
    cat(fig1, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(mSetObj$analSet$htmap)){
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "static_heatmap", "heatmap");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    if(!is.null(mSetObj$imgSet$reportSet$heatmap_static)){
    if (mSetObj$paramSet$report.format == 'html' ){
      if(mSetObj$dataSet$proc.feat.num>1000){
        fig2 <- c(paste0("```{r figure_hp1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_hc, 
                         ". Clustering result shown as heatmap (distance measure using ", 
                         mSetObj$analSet$tree$dist.par,
                         " and clustering algorithm using ",
                         mSetObj$analSet$tree$clust.par,
                         ").', ",
                         "fig.lp='", 
                         mSetObj$imgSet$heatmap, 
                         "', out.width ='",mSetObj$imgSet$heatmap_stats_param$width,"px', out.height ='",mSetObj$imgSet$heatmap_stats_param$height,"px'}"),
                  "knitr::include_graphics(mSetObj$imgSet$reportSet$heatmap_static)",
                  "```",
                  "\n\n");
      } else {
        fig2 <- c(paste0("```{r figure_hp1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_hc, 
                         ". Clustering result shown as heatmap (distance measure using ", 
                         mSetObj$analSet$tree$dist.par,
                         " and clustering algorithm using ",
                         mSetObj$analSet$tree$clust.par,
                         ").', ",
                         "fig.lp='", 
                         mSetObj$imgSet$heatmap, 
                         "', out.width ='",mSetObj$imgSet$heatmap_stats_param$width,"px', out.height ='",mSetObj$imgSet$heatmap_stats_param$height,"px'}"),
                  "p <- readRDS('heatmap_stats.rds')",
                  "p",
                  "```",
                  "\n\n");
      }
    } else {
      fig2 <- c(paste0("```{r figure_hp1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_hc, 
                       ". Clustering result shown as heatmap (distance measure using ", 
                       mSetObj$analSet$tree$dist.par,
                       " and clustering algorithm using ",
                       mSetObj$analSet$tree$clust.par,
                       ").', ",
                       "fig.lp='", 
                       mSetObj$imgSet$heatmap, 
                       "', out.width ='90%'}"),
                        "knitr::include_graphics(mSetObj$imgSet$reportSet$heatmap_static)",
                        "```",
                        "\n\n");
    }
    cat(fig2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }


  }
  cat("\\clearpage", file=rmdFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create SOM partitional clustering document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateSOMdoc <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  link <- GetSharingLink(mSetObj)

  # need to check if this process is executed
  if(is.null(mSetObj$analSet$som)){
    return();
  };
  
  descr <- c("#### - Self Organizing Map (SOM)\n\n",
             "SOM is an unsupervised neural network algorithm used to automatically
             identify major trends present in high-dimensional data. SOM is based
             on a grid of interconnected nodes, each of which represents a model.
             These models begin as random values, but during the process of iterative training
             they are updated to represent different subsets of the training set.
             Users need to specify the x and y dimension of the grid to perform SOM analysis.",
             "\n\n",
             paste("Figure", fig_som <- fig.count<<-fig.count+1,"shows the SOM clustering results."),
             paste("Table", table.count<<-table.count+1,"shows the members in each cluster from SOM analysis."),
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
     reportLinks <- getReportLinks(link, "som");

  cat(reportLinks, file=rmdFile, append=TRUE, sep="\n");

  fig1 <- c(paste0("```{r figure_som1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_som, 
                   ". SOM cluster analysis. The x-axes are features and y-axes are relative", 
                   " intensities. The blue lines represent median intensities of corresponding clusters.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$som, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$som)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
     reportLinks <- getReportLinks(link, "som_table");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  descr <- c(
    "```{r table_som, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetAllSOMClusterMembersRMD(mSet));",
    paste0("if(nrow(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Clustering result using SOM.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create Kmeans partitional clustering document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateKMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$kmeans)){
    return();
  }

  descr <- c("#### - K-means Clustering\n\n",
             "K-means clustering is a nonhierarchical clustering technique.",
             "It begins by creating k random clusters (k is supplied by user).",
             "The program then calculates the mean of each cluster.",
             "If an observation is closer to the centroid of another cluster",
             "then the observation is made a member of that cluster. This process is",
             "repeated until none of the observations are reassigned to a different cluster.",
             "\n\n",
             "K-means analysis is performed using the *kmeans* function in the",
             "package *stat*.",
             paste("Figure", fig_kcr <- fig.count<<-fig.count+1,"shows clustering the results."),
             paste("Figure", fig_kpca <- fig.count<<-fig.count+1,"shows clustering the results."),
             paste("Table", table.count<<-table.count+1,"shows the members in each cluster from K-means analysis.\n"));
  
  cat(descr, file=rmdFile, append=TRUE);
  
  link <- GetSharingLink(mSetObj)
     reportLinks <- getReportLinks(link, "km", "km");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_km1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_kcr, 
                   ". K-means cluster analysis. The x-axes are variable indices and y-axes", 
                   " are relative intensities. The blue lines represent median intensities of corresponding clusters.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$kmeans, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$kmeans)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "km_pca", "km_pca");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_km2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_kpca, 
                   ". K-means cluster analysis, shown as PCA score plot.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$kmeans.pca, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$kmeans.pca)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  descr <- c(
    "```{r table_km, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetAllKMClusterMembersRMD(mSet));",
    paste0("if(nrow(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Clustering result using K-means.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create Random Forest document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateRFdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$rf)){
    return();
  }
  
  descr <- c("#### - Random Forest (RF) \n\n",
             "Random Forest is a supervised learning algorithm suitable for high dimensional data analysis.",
             "It uses an ensemble of classification trees, each of which is grown by random feature",
             "selection from a bootstrap sample at each branch. Class prediction is based on the",
             "majority vote of the ensemble. RF also provides other useful information such as OOB",
             "(out-of-bag) error, variable importance measure, and outlier measures. During tree construction, about",
             "one-third of the instances are left out of the bootstrap sample. This OOB data",
             "is then used as test sample to obtain an unbiased estimate of the classification",
             "error (OOB error). Variable importance is evaluated by measuring the increase of the",
             "OOB error when it is permuted. The outlier measures are based on the proximities during tree construction.",
             "\n\n",
             "RF analysis is performed using the [*randomForest* package](https://cran.r-project.org/web/packages/randomForest/index.html).",
             paste("Table", table.count<<-table.count+1,"shows the confusion matrix of random forest."),
             paste("Figure", fig_rfg <- fig.count<<-fig.count+1,"shows the cumulative error rates of random forest analysis for given parameters.\n"),
             paste("Figure", fig_rfi <- fig.count<<-fig.count+1,"shows the important features ranked by random forest.\n"),
             paste("Figure", fig_rfo <- fig.count<<-fig.count+1,"shows the outlier measures of all samples for the given parameters.\n"));
  
  cat(descr, file=rmdFile, append=TRUE);
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_cls", "rf_cls");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_rf1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_rfg, 
                   ". Cumulative error rates by Random Forest classification. The overall error rate is shown", 
                   " as the black line; the red and green lines represent the error rates for each class.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$rf.cls, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$rf.cls)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  descr <- c(
    "```{r table_rf, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetRFConfTableRMD(mSet));",
    paste0("if(nrow(dt_res) != 1) {",
           "create_dt(dt_res,  caption = 'Table ", 
           table.count, 
           ". Random Forest Classification Performance.')}"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_imp", "rf_imp");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_rf2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_rfi, 
                   ". Significant features identified by Random Forest. The features are ranked by the mean", 
                   " decrease in classification accuracy when they are permuted.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$rf.imp, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$rf.imp)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_outlier", "rf_outlier");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_rf3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_rfo, 
                   ". Potential outliers identified by Random Forest. Only the top five are labeled.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$rf.outlier, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$rf.outlier)",
            "```",
            "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create R-SVM document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateSVMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$svm)){
    return();
  }
  
  descr <- c("#### - Support Vector Machine (SVM)\n\n",
             "SVM aims to find a nonlinear decision function in the input space by mapping the data into a",
             "higher dimensional feature space and separating it there by means of a maximum margin hyperplane.",
             "The SVM-based recursive feature selection and classification is performed using the \\texttt{R-SVM}",
             "script\\footnote{http:\\slash\\slash www.hsph.harvard.edu\\slash bioinfocore\\slash RSVMhome\\slash R-SVM.html}.",
             "The process is performed recursively using decreasing series of feature subsets (\\texttt{ladder})",
             "so that different classification models can be calculated. Feature importance is evaluated based on",
             "its frequencies being selected in the best classifier identified by recursive classification and cross-validation.",
             "Please note, R-SVM is very computationally intensive. Only the top 50 features (ranked by their p values from t-tests)",
             "will be evaluated.",
             "\n\n",
             "In total,", length(mSetObj$analSet$svm$ladder), "models (levels) were created using",paste(mSetObj$analSet$svm$ladder, collapse=', '), "selected feature subsets.",
             paste("Figure", fig_svm <- fig.count<<-fig.count+1, "shows the SVM classification performance using recursive feature selection."),
             paste("Figure", fig_sig <- fig.count<<-fig.count+1, "shows the signicant features used by the best classifiers.\n"));
  
  cat(descr, file=rmdFile, append=TRUE);
  
  cat("\n\n", file=rmdFile, append=TRUE);
  
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "svm_cls", "svm_cls");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig1 <- c(paste0("```{r figure_svm1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_svm, 
                   ". Recursive classification with SVM. The red circle indicates the best classifier.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$svm.class, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$svm.class)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  ## 
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "svm_imp", "svm_imp");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig2 <- c(paste0("```{r figure_svm2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_sig, 
                   ". Significant features identified by R-SVM. Features are ranked by their frequencies of being selected in the classifer.', ",
                   " fig.lp='", 
                   mSetObj$imgSet$svm, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "knitr::include_graphics(mSetObj$imgSet$svm)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create R-SVM document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDSPCdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(!exists("pheno.net")){
    return();
  }
  net.type <- "dspc";
  descr <- c("#### - Debiased Sparse Partial Correlation (DSPC)\n\n",
             "Debiased Sparse Partial Correlation (DSPC) algorithm is based on the de-sparsified graphical lasso modeling procedure 
               (<a href='https://projecteuclid.org/euclid.ejs/1433195859'>Jankova, 2015</a>). 
               A key assumption is that the number of true connections among the metabolites is much smaller than the available sample size.
               DSPC reconstructs a graphical model and provides partial correlation coefficients and P-values for every pair of metabolic features in the dataset. 
               Thus, DSPC allows discovering connectivity among large numbers of metabolites using fewer samples
               (<a href='https://doi.org/10.1093/bioinformatics/btx012'>Basu et al., 2017</a>).",
            # "\n\n",
            #"Summary of the resulting network is displayed below:\n\n",
            #"- Node number: ", nrow(pheno.net$node.data), "\n",
            #"- Edge number: ", nrow(pheno.net$edge.data), 
            "\n\n");
  
    cat(descr, file=rmdFile, append=TRUE);
  
    if(file.exists('dspc.png')){
        link <- GetSharingLink(mSetObj)
        reportLinks <- getReportLinks(link, net.type);

        cat(paste0(reportLinks, "\n\n"), file=rmdFile, append=TRUE);
        fig1 <- paste0(
          "```{r figure_dspc, echo=FALSE, fig.cap='Figure ", getFigCount(), 
          ". Screenshot of DSPC network. The nodes are input metabolites, while the edges represent the association measures.', out.width='", getFigWidth(mSetObj), "'}\n",
          "knitr::include_graphics('dspc.png')\n",
          "```",
          "\n\n"
        )
        cat(fig1, file=rmdFile, append=TRUE, sep="\n");
    }else{
        cat("No network image was created. Please make sure that you have visited the page.", file=rmdFile, append=TRUE, sep="\n");
    }
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
 
}


