
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
#'Create timeseries .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMultiFacRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateMultiFacIntr();
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  InitMultiFacAnal();
  if(exists("analSet", where=mSetObj)){
    CreateMetaOverview(mSetObj);
    CreateiPCAdoc(mSetObj);
    CreateCorHeatmap(mSetObj);

    CreateCovAdj(mSetObj);
    CreateCorAnalysis(mSetObj);
    CreateAOV2doc(mSetObj);
     
    CreateASCAdoc(mSetObj);
    CreateMBdoc(mSetObj);

    CreateRandomForest(mSetObj);
  }else{
    CreateTimeSeriesAnalNullMsg();
  }
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}

CreateMultiFacIntr<-function(mSetObj=NA){
  
  descr <- c("## 1. Overview\n\n",
             "Metabolomics is increasingly applied to clinical, population, or field studies to investigate complex diseases, 
              microbiomics, or exposomics. In addition to the primary condition of interest such as different phenotypes (i.e., disease vs normal), 
              these studies are often associated with complex metadata (or sample descriptors) such as gender, age, BMI, tissue, location, etc. 
              It is often necessary to take into account of these factors during exploratory data analysis to see if they play a role in the primary condition. 
              Statistical analyses that take these metadata information into account can lead to increased power and more robust conclusions about the relationships 
              between the primary condition and the omics data.\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
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
CreateMetaOverview <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$imgSet$metahtmaptwo)){
    return();
  }
  
  descr <- c("\n\n#### - Data and Metadata Overview\n\n",
             "Understanding the correlation structure of the metadata is important for model",
             "definition and interpretation. The metadata heatmap and correlation heatmap allow",
             "for visual identification of relationships between metadata.",
             "By default, all metadata are included in the visualizations.",
             "This section supports multiple methods for calculating distance, correlation, and clustering,",
             "and the heatmap can be viewed in either overview or detail mode.",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  # fig 1
  link <- GetSharingLink(mSetObj);
  reportLinks <- getReportLinks(link, "metaCorrHeatmap", "metaCorrHeatmap");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs1 <- fig.count <<- fig.count+1;
  fig <- c(paste0("```{r figure_mfs1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs1, 
                  ". Correlation heatmap displaying relationship between metadata.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$meta.corhm, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="90%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$meta.corhm)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 2
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "metaHeatmap", "metaHeatmap");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs2 <- fig.count <<- fig.count+1;
  fig <- c(paste0("```{r figure_mfs2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs2, 
                  ". Metadata heatmap displaying relationship between metadata.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$metahtmaptwo, 
                  "', out.width = '", getFigWidth(mSetObj,width="800px", widthPct="100%"),
                  "', out.height = '", getFigWidth(mSetObj,width="780px", widthPct="100%"), "'}"),
                  "if (mSetObj$paramSet$report.format == 'html') {",
                   "p <- readRDS('metadata_heatmap.rds')",
                   "p",
                  "} else {",
                  "  safeIncludeGraphics(mSetObj$imgSet$metahtmaptwo)",
                  "}",           
            "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'For Interactive PCA
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateiPCAdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # need to check if this process is executed
  if(is.null(mSetObj$imgSet$pca.pair)){
    return();
  }
  
  descr <- c("#### - Principal Component Analysis (PCA)\n\n",
             "PCA is a widely used dimensionality reduction method that can summarize main variability trends in high-dimensional omics 
             data into a few dimensions for intuitive visual exploration in the form of *scores plot* and *loadings plot*. 
             The scores are samples in the low-dimensional space defined by the components, while the loadings are feature coefficients
             that are multiplied with the omics data to obtain scores. We can annotate scores with different metadata labels to detect whether any of the main trends correspond to sample descriptors 
             like age, sex, diagnosis, etc. Once we have identified the components with interesting patterns, we can inspect the corresponding 
            loadings to see which features are influential, and further explore their functional implications.",
            "\n\n",
            "The pairwise 2D scores plots based on the top 5 PCs (default) allow quick detection of major trends between any two PCs. 
             In addition, MetaboAnalyst also provides an interactive 3D plot integrating scores (main view) and loadings plots (inset). 
             The 3D view supports pointing-and-clicking, rotating, and zooming. Users can switch the main view from scores (default) to loadings. 
             Clicking any of the data points in the loading plot will show a boxplot summary of its distribution across different sample groups.",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  # fig 3
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "pca_pair_meta", "pca_pair_meta");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs3 <- fig.count <<- fig.count+1;
  fig <- c(paste0("```{r figure_mfs3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs3, 
                  ". Pairwise PCA scores plots with density outlines showing group membership.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$pca.pair, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$pca.pair)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

if (!is.null(mSetObj$imgSet$pca_score2d_meta) &&
    safeFileExists(mSetObj$imgSet$pca_score2d_meta)) {

  # Add cross-links (uses your existing helper)
  reportLinks <- getReportLinks(link, "pca_score2d_meta", "pca_score2d_meta")
  cat(reportLinks, file = rmdFile, append = TRUE)
  cat("\n\n", file = rmdFile, append = TRUE)

  # Knit chunk that embeds the saved PNG
  fig2d <- c(
    paste0(
      "```{r figure_pca2d_meta, echo=FALSE, fig.pos='H', fig.cap='Figure ",
      getFigCount(),
      ". PCA 2D scores plot.', ",
      "fig.lp='", mSetObj$imgSet$pca_score2d_meta, "', ",
      "out.width='", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"
    ),
    "safeIncludeGraphics(mSetObj$imgSet$pca_score2d_meta)",
    "```",
    "\n\n"
  )
  cat(fig2d, file = rmdFile, append = TRUE, sep = "\n")
  cat("\n\n", file = rmdFile, append = TRUE, sep = "\n")
}

  if(!is.null(mSetObj$imgSet$reportSet$ipca_3d) && safeFileExists(mSetObj$imgSet$reportSet$ipca_3d)){
    
    reportLinks <- getReportLinks(link, "ipca_3d");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    fig2 <- c(paste0("```{r figure_ipca3, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                     ". Screenshot of interactive 3-D plot.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$ipca_3d, 
                     "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
              "safeIncludeGraphics(mSetObj$imgSet$reportSet$ipca_3d)",
              "```",
              "\n\n");
    cat(fig2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
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
CreateCorHeatmap <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$htmap2)){
    return();
  }
  
  descr <- c("#### - Two-way Heatmap Visualization \n\n",
             "The heatmap provides direct visualization of all data points in the form",
             "of colors squares. The color spectrum intuitively indicates the higher or lower values.",
             "Users can choose different clustering algorithms or distance measures to cluster the",
             "variables. The samples are ordered by the two factors with default the first factor",
             "used for primary ordering. Users can choose to switch the order.",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 4 - heatmap2
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "heatmap2", "heatmap2");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs4 <- fig.count <<- fig.count+1;
  if (mSetObj$paramSet$report.format == 'html'){
    if(mSetObj[["dataSet"]][["proc.feat.num"]]>1000){
      fig <- c(paste0("```{r figure_mfs4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs4, 
                      ". Metadata heatmap displaying relationship between metabolites and metadata.', ",
                      " fig.lp='", 
                      mSetObj$imgSet$htmaptwo, 
                      "', out.width = '",mSetObj$imgSet$heatmap_multifac_param$width,"px', out.height = '",mSetObj$imgSet$heatmap_multifac_param$height,"px'}"),
               "if (safeFileExists(mSetObj$imgSet$reportSet$heatmap_multifac)) safeIncludeGraphics(mSetObj$imgSet$reportSet$heatmap_multifac)",
               "```",
               "\n\n");
    } else {
      fig <- c(paste0("```{r figure_mfs4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs4, 
                      ". Metadata heatmap displaying relationship between metabolites and metadata.', ",
                      " fig.lp='", 
                      mSetObj$imgSet$htmaptwo, 
                      "', out.width = '",mSetObj$imgSet$heatmap_multifac_param$width,"px', out.height = '",mSetObj$imgSet$heatmap_multifac_param$height,"px'}"),
               "p <- readRDS('heatmap_multifac.rds')",
               "p",
               "```",
               "\n\n");
      }
  } else {
    if(safeFileExists(mSetObj$imgSet$reportSet$heatmap_multifac)){

    fig <- c(paste0("```{r figure_mfs4, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs4, 
                    ". Metadata heatmap displaying relationship between metabolites and metadata.', ",
                    " fig.lp='", 
                    mSetObj$imgSet$htmaptwo, 
                    "', out.width = '90%'}"),
                      "safeIncludeGraphics(mSetObj$imgSet$reportSet$heatmap_multifac)",
             "```",
             "\n\n");
    }else{
    fig <- "";
    }
  }
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
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
CreateCovAdj <- function(mSetObj=NA){ ## need to figure out the image still
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$cov)){
    return();
  }

  params <- mSetObj$analSet$cov;
  descr <- c("#### - Linear models with covariate adjustments \n\n",
             "Including metadata in the linear model adjusts for variability associated with them while ",
             "performing statistical tests for the variable of interest. Users have the option of which metadata to include ",
             "and whether to include them as fixed (normal) or random (blocking factor) effects. It is important to investigate ",
             "correlation between metadata prior to defining the model as including variables that are highly correlated ",
             "can lead to model parameter instability. \n",
             paste0("- Primary metadata: ```", params$primary.var, "```;"),
             paste0("- Selected covariates: ```", paste(params$cov.var, collapse="; "), "```;"),
             paste0("- Block factor: ```", params$block, "```;"),
             paste0("- P-value cutoff: ```", params$p.thresh, "``` type: ```", params$pval.type, "```"),
             paste0("- Number of sig features: ```", params$sig.num, "```"),
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
                
  # fig 5
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "covariate_plot", "covariate_plot");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs5 <- fig.count <<- fig.count+1;
  fig <- c(
  paste0("```{r figure_mfs5, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs5,
         ". P-values for metabolites with and without covariate adjustment.', ",
         "fig.lp='fig:", fig_mfs5, "', out.width='", getFigWidth(mSetObj,"720px", "100%"), "'}"),
  "if (mSetObj$paramSet$report.format == 'html') {",
  "  PlotCovariateMap(NA, interactive=T)",
  "} else {",
  "  safeIncludeGraphics(mSetObj$imgSet$covAdj)",
  "}",
  "```",
  "\n\n"
)
cat(paste0(fig, "\n\n"), file=rmdFile, append=TRUE, sep="\n");


    link <- GetSharingLink(mSetObj)

    reportLinks <- getReportLinks(link, "covariate_table");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

  table.count <<- table.count+1;
  
  cmdhist2 <- c(
    "```{r table_limma, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- GetSigTableRMD_Limma(mSet);",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Significant features identified by linear modeling')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
}


#'Create report of analyses 
#'@description Report generation using Sweave
#'Correlation and Partial Correlation Analysis 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCorAnalysis <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$corr$cor.mat)){
    return();
  }
  
  params <- mSetObj$analSet$corr

  descr <- c("#### - Correlation and Partial Correlation Analysis\n\n",
             "Correlation analysis can be performed for a given feature and metadata of metadata of interest.",
             "When the covariate is 'none' (default), regular correlation analysis will be performed;",
             "otherwise, partial correlation will be performed.",
             "For binary metadata, the point biserial correlation will be used;",
             "for continuous metadata, users can choose Pearson/Spearman/Kendall correlation.",
             "\n\n",
             paste0("\n- Target of interest: ```", params$pattern, "```;"),
             paste0("\n- Selected covariates: ```", paste(params$cov.vec, collapse="; "), "```;"),
             paste0("\n- Correlation Measure: ```", params$dist.name, "```;"),
            "\n\n");  
  cat(descr, file=rmdFile, append=TRUE);

  # fig 6
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "ptn_multifac", "ptn");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

  fig_mfs6 <- fig.count <<- fig.count+1;
  fig <- c(paste0("```{r figure_mfs6, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs6, 
                  ". Correlation coefficients for metadata of interest.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$corr, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$corr)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'ANOVA 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateAOV2doc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$aov2)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$aov2$sig.mat)){
    aov2.tab<-NULL;
  }else{
    aov2.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }

  params <- mSetObj$analSet$aov2;
  
descr <- c("#### - Two-way ANOVA\n\n",
           "For two-factor data, a basic approach is two-way ANOVA. ",
           "There are two options - between-subjects ANOVA and within-subjects ANOVA. When samples are all from",
           "independent subjects (i.e. general two-way ANOVA), the between-subjects option should be selected.",
           "However, time series data contains samples measured from the same subjects from different time points.",
           "Therefore within-subjects ANOVA should be used.",
           "\n\n",
           paste("Figure", fig_mfs7 <- fig.count<<-fig.count+1,"shows the important features identified by ANOVA analysis."),
           aov2.tab,
           "\n\n",
           paste0("\n- Selected metadata: ```", paste(params$selected.meta, collapse=", "), "```;"),
           paste0("\n- Phenotype factor: ```", params$phenotype.factor, "```;"),
           paste0("\n- Multiple correction method: ```", paste(params$multi.c, collapse="; "), "```;"),
           paste0("\n- P-value threshold: ```", params$raw.thresh, "```;"),
           "\n\n"
          );

  
  cat(descr, file=rmdFile, append=TRUE);
  
  # fig 7
  link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "aov2", "aov2");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs7, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs7, 
                  ". Plot of important features selected by two-way ANOVA.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$anova2, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$anova2)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  #Significant features identified by advanced ANOVA
  
  cat("\n\n", file=rmdFile, append=TRUE);
  
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "aov2_table");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

  cmdhist2 <- c(
    "```{r table_mfs3, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- GetSigTable.Aov2RMD(mSet);",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Significant features identified by advanced ANOVA.')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
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
CreateASCAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$asca)){
    return();
  }
  asca <- qs::qread("asca.qs");

  if(isEmptyMatrix(asca$sig.list[["Model.a"]])){
    asca.tab1<-NULL;
  }else{
    asca.tab1<-paste("Table", table.count<<-table.count+1,paste("shows features well-modelled by ", mSetObj$dataSet$facA.lbl, ". ", sep=""));
  }
  
  if(isEmptyMatrix(asca$sig.list[["Model.b"]])){
    asca.tab2<-NULL;
  }else{
    asca.tab2<-paste("Table", table.count<<-table.count+1,paste("shows features well-modelled by ", mSetObj$dataSet$facB.lbl, ". ", sep=""));
  }
  
  if(isEmptyMatrix(asca$sig.list[["Model.ab"]])){
    asca.tab3<-NULL;
  }else{
    asca.tab3<-paste("Table", table.count<<-table.count+1, "shows features well-modelled by Interaction model. ");
  }
  
  params <- asca;

  descr <- c("#### - ANOVA - Simultaneous Component Analysis (ASCA)\n\n",
             "ASCA is a multivariate extension of univariate ANOVA approach. It is",
             "designed to identify the major patterns associated with each factor.",
             "This implementation supports ASCA model for two factors with one interaction",
             "effect. The algorithm first partitions the overall data variance (X) into",
             "individual variances induced by each factor (A and B), as well as by the",
             "interactions (AB). The formula is shown below with (E) indicates the residual",
             "Errors: \n\n",
             "**X = A + B + AB + E**",
             "\n\n",
             "The SCA part applies PCA to A, B, AB to summarize major variations in each partition.",
             "Users then detect the major pattern by visualizing the PCA scores plot.",
             "MetaboAnalyst also provides model validation to test the significance of the effects associated",
             "with main effects. It is based on the Manly's unrestricted permutation of observation",
             "then calculate the permuted variation associated with each factor",
             "Finally, the permuted values are compared with the original variations",
             "The significant variables are identified based on the leverage and the",
             "Squared Prediction Errors (SPE) associated with each variables",
             "Variables with low SPE and higher leverage are modeled well after the major patterns. Here are the selected parameters: \n\n",
             paste0("\n- Selected metadata: ```", paste(params$meta.vec, collapse="; "), "```;"),
             paste0("\n- Model A: ```", params$model.a, "```;"),
             paste0("\n- Model B: ```", params$model.b, "```;"),
             paste0("\n- Interaction model: ```", params$model.ab, "```;"),
             paste0("\n- Residual model: ```", params$model.res, "```"),
             "\n\n",
             paste("Figure", fig_mfs11 <- fig.count<<-fig.count+1,"shows the scree plots for each effect model.\n"),
             paste("Figure", fig_mfs12 <- fig.count<<-fig.count+1,"shows the major patterns associated with factor A.\n"),
             paste("Figure", fig_mfs13 <- fig.count<<-fig.count+1,"shows the major patterns associated with factor B.\n"),
             paste("Figure", fig_mfs14 <- fig.count<<-fig.count+1,"shows the major patterns associated with interaction.\n"),
             paste("Figure", fig_mfs15 <- fig.count<<-fig.count+1,"shows the results of model validations through permutations.\n"),
             paste("Figure", fig_mfs16 <- fig.count<<-fig.count+1,"shows the important features associated with factor A.\n"),
             paste("Figure", fig_mfs17 <- fig.count<<-fig.count+1,"shows the important features associated with factor B.\n"),
             paste("Figure", fig_mfs18 <- fig.count<<-fig.count+1,"shows the features that are important in the interaction.\n"),
             "\n\n",
             asca.tab1,
             asca.tab2,
             asca.tab3,
             "The other details are available as .csv documents in your downloaded zip file.");
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 11
  if(!is.null(mSetObj$imgSet$asca.scree)){
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_scree", "asca_scree");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  fig <- c(paste0("```{r fig_mfs11, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs11, 
                  ". Scree plots for each sub model.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.scree, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.scree)",
           "```",
           "\n\n");

  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  # fig 12
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_fa", "asca_fa");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs12, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs12, 
                  ". Major patterns associated with ",mSetObj$dataSet$facA.lbl, ".', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.modelA, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.modelA)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 13
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_fb", "asca_fb");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs13, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs13, 
                  ". Major patterns associated with ",mSetObj$dataSet$facB.lbl, ".', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.modelB, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.modelB)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 14
    if(!is.null(mSetObj$imgSet$asca.modelAB)){

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_fab", "asca_fab");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs14, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs14, 
                  ". Major patterns associated with the Interaction between the two factors.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.modelAB, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.modelAB)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  # fig 15
  if(!is.null(mSetObj$imgSet$asca.perm)){
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "asca_perm", "asca_perm");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig <- c(paste0("```{r fig_mfs15, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs15, 
                    ". Model validation through permutations.', ",
                    " fig.lp='", 
                    mSetObj$imgSet$asca.perm, 
                    "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
             "safeIncludeGraphics(mSetObj$imgSet$asca.perm)",
             "```",
             "\n\n");
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }

      if(!is.null(mSetObj$imgSet$asca.impA)){

  # fig 16
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_impa", "asca_impa");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs16, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs16, 
                  ". Important variables associated with ", mSetObj$dataSet$facA.lbl, ".', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.impA, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.impA)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  # fig 17
        if(!is.null(mSetObj$imgSet$asca.impB)){

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_impb", "asca_impb");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs17, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs17, 
                  ". Important variables associated with ", mSetObj$dataSet$facB.lbl, ".', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.impB, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.impB)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }

  # fig 18
          if(!is.null(mSetObj$imgSet$asca.impAB)){

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "asca_impab", "asca_impab");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0("```{r fig_mfs18, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs18, 
                  ". Variables important in interaction between the two factors.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$asca.impAB, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$asca.impAB)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(asca.tab1)){
    
    cat("\n\n", file=rmdFile, append=TRUE);

    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "asca.sigA");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mfs4, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- GetSigTableRMD_ASCA(mSet, \"Model.a\");",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Important features identified by ASCA.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  if(!is.null(asca.tab2)){
    cat("\n\n", file=rmdFile, append=TRUE);

    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "asca.sigB", "asca.sigB");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mfs5, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- GetSigTableRMD_ASCA(mSet, \"Model.b\");",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Important features identified by ASCA.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  if(!is.null(asca.tab3)){
    cat("\n\n", file=rmdFile, append=TRUE);

    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "asca.sigAB");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mfs6, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- GetSigTableRMD_ASCA(mSet, \"Model.ab\");",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Important features identified by ASCA.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
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
CreateMBdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$MB)){
    return();
  }
  
  descr <- c("#### - Multivariate Empirical Bayes Approach (MEBA)\n\n",
             "The MEBA approach is designed to compare the time-course profiles under different conditions.
              The multivariate empirical Bayes model is designed to rank features in order of interest 
              from longitudinal replicated time course experiments for one or two group study design.
              The result is a list of variables that are ranked by their difference in temporal profiles (time-series only),
              or by their difference in temporal profiles across different biological conditions (time-series and one factor). 
              The Hotelling-T2 is used to rank the variables with different temporal profiles between two biological conditions 
              under study; and the MB-statistics is used for more than two biological conditions. Higher statistical value 
              indicates the time-course profiles are more different across the biological conditions under study.",
              "\n\n",
              paste0("\n- Selected metadata: `", paste(mSetObj$analSet$MB$selected.meta, collapse=", "), "`;"),
              "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE);
  
  cat("\n\n", file=rmdFile, append=TRUE);

    link <- GetSharingLink(mSetObj)

    reportLinks <- getReportLinks(link, "meba");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

  table.count <<- table.count+1;
  
  cmdhist2 <- c(
    "```{r table_mfs11, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- GetSigTableRMD_MB(mSet);",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Significant features identified by advanced MEBA.')"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
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
CreateRandomForest <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$dataSet$cls.rf)){
    return();
  }

  descr <- c("#### - Random Forest\n\n",
             "This implementation of random forest can only be used for classification, thus the primary metadata must be categorical.",
             "Predictors in the model, including both metabolites and other metadata, can include both categorical and numeric variables.",
             "Since random forest uses some random processes, turning randomness off means that",
             "the results will be the same each time that you run the tool. This can be better for reproducibility, but is not necessary.",
             "\n\n",
             paste0("\n- Primary metadata: `", mSetObj$dataSet$cls.rf.nm, "`;"),
             paste0("\n- Predictor(s) metadata: `", mSetObj$analSet$meta.vec.rf, "`;"),
             paste0("\n- Randomness: `", mSetObj$analSet$rf.random, "`;")
            );
  
  cat(descr, file=rmdFile, append=TRUE);
  
  # fig 21
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_cls_multifac", "rf_cls_meta");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs21  <- fig.count<<-fig.count+1;
  fig <- c(paste0("```{r fig_mfs21, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs21, 
                  ". Classification error vs. number of trees.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$rf.cls, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$rf.cls)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 22
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_imp_multifac", "rf_imp_meta");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs22  <- fig.count<<-fig.count+1;
  fig <- c(paste0("```{r fig_mfs22, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs22, 
                  ". Features ranked by their contributions to classification accuracy.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$rf.imp, 
                  "', out.width = '", getFigWidth(mSetObj,width="720px", widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$rf.imp)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # fig 23
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "rf_outlier_multifac", "rf_outlier_meta");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig_mfs23  <- fig.count<<-fig.count+1;
  fig <- c(paste0("```{r fig_mfs23, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mfs23, 
                  ". Up to five potential outliers flagged by random forest algorithm.', ",
                  " fig.lp='", 
                  mSetObj$imgSet$rf.outlier, 
                  "', out.width = '", getFigWidth(mSetObj,widthPct="100%"), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$rf.outlier)",
           "```",
           "\n\n");
  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
}
