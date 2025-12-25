
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

#'Create report of analyses (Meta-Analysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisRnwReport<-function(mSetObj, usrName, link){
  
  CreateHeader(usrName);
  CreateMetaAnalysisIntr();
  
  CreateMetaAnalysisInputDoc(mSetObj);
  CreateMetaAnalysisNORMdoc(mSetObj);
  CreateMetaAnalysisDEdoc(mSetObj);
  
  CreateMetaAnalysisOutput(mSetObj);
  CreateUpSetDoc(mSetObj)
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create MetaAnalysis analysis report: Introduction  
#'@description Report generation using Sweave
#'MetaAnalysis analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisIntr<-function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "Biomarker identification remains a large area of research in metabolomics, and their validation is challenging due to inconsistencies 
             amongst similar experiments. Performing meta-analysis across multiple studies will help increase the chance to identify robust biomarkers. 
             By leveraging the collective power of multiple studies that have been collected to investigate the same or similar phenotypes overcome 
             potential noise, bias, and small sample sizes. The meta analysis consists of six steps:",
             "\n\n",
             "1. uploading the individual datasets;\n", 
             "2. processing each individual dataset;\n",
             "3. differential expression analysis of individual datasets;\n", 
             "4. data integrity check prior to meta-analysis;\n", 
             "5. selection of the statistical method and perform meta-analysis;\n",
             "6. visual exploration of shared or unique features between different datasets.\n",
             "\n\n");
  .buffer_add(descr);
}

#'Create MetaAnalysis analysis report: Data Input
#'@description Report generation using Sweave
#'Power analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\n\n<hr/>\n\n",
             "## 2. Data Processing\n\n",
             "The Meta-Analysis module accepts individual datasets which must be prepared by users prior to being uploaded. ",
             "In general, the datasets must have been collected under comparable experimental conditions/share the same hypothesis",
             " or have the same mechanistic underpinnings. At the moment, the module only supports two-group comparisons (ex: control vs disease).",
             "Further, the module accepts either a compound concentration table, spectral binned data, or a peak intensity",
             " table. The format of the data must be specified, identifying whether the samples are in rows or columns, ",
             "or may either be .csv or .txt files.",
             " \n\n");
  
  .buffer_add(descr, collapse="\n");
  
  descr<-c("### - Data Integrity Check\n\n",
           " Before data analysis, a data quality check is performed to make sure that all of the necessary",
           " information has been collected. The class labels must be present and must contain only two classes for meta-analysis.",
           " By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
           " found within the data (see next section).\n");
  .buffer_add(descr, collapse="\n");

}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisNORMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr<-c("### - Normalization of individual data\n\n",
           " Before differential expression analysis, datasets may be normalized using Log2 transformation.",
           " Additionally, users may choose to auto-scale their data.\n\n"
           );
  .buffer_add(descr, collapse="\n");
  
  if(exists("norm.msg")){
    norm.desc <- paste(norm.msg);
  }else{
    norm.desc <- " No normalization methods were applied.";
  } 
  .buffer_add(norm.desc, collapse="\n");
  
  if(mSetObj$dataSet$auto_opt == 1){
    autoscale <- "```Autoscaling``` of data was performed.\n";
  }else{
    autoscale <- " No data autoscaling was performed.\n";
  }
  .buffer_add(autoscale, collapse="\n");
}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisDEdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr<-c("### - Differential expression analysis of individual data\n\n",
           " Before meta-analysis, differential expression analysis using linear models (Limma) may be performed",
           " for exploratory analysis. Here, users must specify the p-value (FDR) cut-off and the fold-change (FC) cutoff.\n\n"
  );
  .buffer_add(descr);
  
  if(!is.null(mSetObj$dataSet[["deparam"]])){
    de.desc <- paste(mSetObj$dataSet$deparam);
  }else{
    de.desc <- " No differential-expression analysis was performed."
  } 
  .buffer_add(de.desc, collapse="\n");
  
  if(!is.null(mSetObj$dataSet[["desig"]])){
    .buffer_add("Please refer to **Table 2** below for the numbers of sig features from individual datasets.\n", collapse="\n");
  }

  descr<-c("### - Final data integrity check\n\n",
           " Before performing meta-analysis, one final data integrity check is performed to ensure meta-data are consistent between datasets and",
           " that there are at least more than 25 percent common features between the collective datasets.\n\n");
  .buffer_add(descr, collapse="\n");
}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'MetaAnalysis analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisOutput <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
    
  descr<-c("\n\n<hr/>\n\n",
           "## 3. Meta-analysis\n\n",
           "After the data has passed the final integrity check, users have the option to select one of three methods to perform meta-analysis: 
           1) Combining p-values, 2) vote counting, or 3) directly merging the datasets into a mega-dataset.",
           "\n\n",
           "Calculating and **combining p-values** for the meta-analysis has been a long standing method and now we apply it to metabolomics studies.
           It includes two popular approaches, the Fisher's method and the Stouffer's method, which have similar levels of performance
           and are generally interpreted as larger scores reflecting greater differential abundance. The main difference between
           the two methods are weights (which are based on sample size), which are used in the Stouffer's method but not used in the Fisher's method. 
           It should be noted that larger sample sizes do not warrant larger weights, as study quality can be variable. Users should use the Stouffer's method only when all studies are of similar quality.",
           "\n\n",
           "**Vote counting** is considered the most simple yet most intuitive method for meta-analysis. Here, significant features are selected based 
            on a selected criteria (i.e. an adjusted p-value <0.05 and the same direction of FC) for each dataset. The votes are then calculated for 
            each feature by counting the total of number of times a feature is significant across all included datasets. However, this method is 
            statistically inefficient and should be considered the last resort in situations where other methods to perform meta-analysis cannot be applied.",
           "\n\n",
           "The final method of meta-analysis is the **direct merging** of individual data into a mega-dataset, which results in an analysis of that 
            mega-dataset as if the individual data were derived from the same experiment. This method thereby ignores any inherent bias and heterogeneity 
            between the different data. Because of this, potential confounders such as different experimental protocols, technical platforms, and raw 
            data processing procedures that can mask true underlying differences. It is therefore highly suggested that this approach be used only when 
            individual data are very similar (i.e. from the same lab, same platform without batch effects).",
            "\n\n");
  .buffer_add(descr, collapse="\n");
  
  resTitle <- c("\n\n",
              "#### - Meta-analysis Result Table", 
              "\n\n");
  .buffer_add(resTitle, collapse="\n");
    
  if(mSetObj$dataSet$metastat.method =="metap"){
    method <- paste("P-value combination was the selected method to perform meta-analysis.\n", 
                    "The method of p-value combination used is: ```", mSetObj$dataSet$pvalmethod, "```\n",
                    "The p-value significance threshold is: ```", mSetObj$dataSet$pvalcutoff, "```\n");
  }else if(mSetObj$dataSet$metastat.method =="votecount"){
    method <- paste("Vote counting was the selected method to perform meta-analysis.\n",
                    "The minimum vote count used is: ```", mSetObj$dataSet$vote, "```\n",
                    "The p-value significance threshold is: ```", mSetObj$dataSet$pvalcutoff, "```\n");
  }else{
    method <- paste("Direct merging of individual data was the selected method to perform meta-analysis.\n", 
                    "The p-value significance threshold is: ```", mSetObj$dataSet$pvalcutoff, "```\n");
    
  }
  .buffer_add(method, collapse="\n");
  
  if(is.null(mSetObj$analSet$meta.mat)){
    return();
  }else{
    
    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "metastat_restbl");

    .buffer_add(reportLinks);

    .buffer_add("\n\n");
    table.count <<- table.count+1;

    cmdhist2 <- c(
      "```{r table_s1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateMetaTableRMD(mSet);",
      paste0("create_dt(sum_dt, 'Table ", 
             table.count, 
             ". Top-ranking features from meta-analysis.')"),
      "```", 
      "\n\n"
    );
    .buffer_add(cmdhist2, collapse="\n");
  }
  
  metafeature <- mSetObj$imgSet$meta.anal$feature
  
  if(!is.null(mSetObj$imgSet$meta.anal$plot)){
    for(s in 1:length(mSetObj$imgSet$meta.anal$ids)) {
      fig_mp1 <- fig.count <<- fig.count+1;
      snm <- mSetObj$imgSet$meta.anal$ids[s];
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                            '<a href="', link, '&format=pdf&imgCmd=meta_ft_' , snm, '" target="_blank">PDF</a> ',
                            '<a href="', link, '&format=svg&imgCmd=meta_ft_' , snm, '" target="_blank">SVG</a>',
                            '</div>')
      .buffer_add(reportLinks);
      .buffer_add("\n\n");
      
      fig <- c(paste0("```{r figure_metap1", s, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mp1, 
                      ". Box plot of the expression pattern of the selected feature between the two experimental groups across all studies.", 
                      " The expression pattern is on the y-axis, and the group labels are on the x-axis.", 
                      " The median expression for the feature is indicated with a black dot in the centre of the boxplot.",
                      "Selected feature: ", metafeature, 
                      "', ",
                      " fig.lp='", 
                      mSetObj$imgSet$meta.anal$plot[s], 
                      "', out.width = '", getFigWidth(mSetObj), "'}"),
               "safeIncludeGraphics(mSetObj$imgSet$meta.anal$plot[", s, "])",
               "```",
               "\n\n");
      .buffer_add(fig, collapse="\n");
      .buffer_add("\n\n", collapse="\n");
    }
  }  

  if(!is.null(mSetObj$imgSet$venn)){
    fig_mp2 <- fig.count <<- fig.count+1;
    
    fig <- c(paste0("```{r figure_metap2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mp2, 
                    ". Venn diagram of the top differentially expressed features from the meta-analysis. ", 
                    "On the left side are features that are DE only from the meta-analysis,",
                    " in the center are DE features that were identified in both the meta-analysis and the individual studies, ",
                    "and on the right side are features that were DE in the individual analysis, but did not show up as DE during meta-analysis.",
                    "', ",
                    " fig.lp='", 
                    mSetObj$imgSet$venn, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "safeIncludeGraphics(mSetObj$imgSet$venn)",
             "```",
             "\n\n");
    .buffer_add(fig, collapse="\n");
    .buffer_add("\n\n", collapse="\n");
  }

  if(!is.null(mSetObj$analSet$sigfeat.matrix)){
    
    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_s2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateVennMetaTableRMD(mSet);",
      paste0("create_dt(sum_dt, 'Table ", 
             table.count, 
             ". Differentially expressed features by individual study and from meta-analysis.')"),
      "```", 
      "\n\n"
    );
    .buffer_add(cmdhist2, collapse="\n");
  }
  .buffer_add("\n\n", collapse="\n");

}

#'Create MetaAnalysis table of results
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  metatable <- mSetObj$analSet$meta.mat;
  return(metatable)
}

#'Create MetaAnalysis table of results for Venn Diagram
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateVennMetaTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$sigfeat.matrix);
}

