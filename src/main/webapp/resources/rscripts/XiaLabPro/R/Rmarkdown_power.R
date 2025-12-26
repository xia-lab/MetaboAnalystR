
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

#'Create report of analyses (Power)
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreatePowerIntr();
  
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  CreatePowerParametersDoc(mSetObj);
  CreatePowerAnalDoc(mSetObj);
    AddFeatureImages(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
}

#'Create power analysis report: Introduction  
#'@description Report generation using Sweave
#'Power analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "The Power Analysis module is designed to help researchers to determine the minimal sample size that is suitable to detect the effect of a 
             given test at the desired level of significance (i.e. to control Type II error or false negative). This is critical as when sample size is 
             small, it will be hard to reach 0.05 level of significance due to insufficient power, and researchers could fail to reject the 
             null hypothesis when the effect exists. Power analysis is often required for designing clinical or population-based metabolomic studies.", 
             "\n\n",
             "Traditional power analysis methods are considered unsuitable for metabolomics data which is highly dimensional and features are often correlated. 
             Modified methods of power analysis are needed to address such concerns. This module is based on the bioconductor ```SSPA``` R package 
             which uses the average power of all metabolites, and to correct for multiple testing using false discovery rate (FDR) 
             instead of raw p-values. For more information, please refer to the original paper by <a href='https://pubmed.ncbi.nlm.nih.gov/19758461/' target='_blank'>van Iterson et al.</a>.",
             "\n\n");
  .buffer_add(descr);
}


#'Create power analysis report: Power Parameter Selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerParametersDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\n\n<hr/>",
             "\n\n## 3. Diagnostic Plots\n\n",
             "Before proceeding to the power analysis, the two groups on which to perform said analysis",
             " must be selected. Further, there will be four diagnostic plots which display a visual overview",
             " of the test-statistics and p-values, providing context for whether or not the normalization was sufficient.",
             " The shape of the test-statistic should follow a near-normal distribution, and the majority of p-values",
             " should be close to zero. \n", 
             paste("Figure", fig_ppw <- fig.count<<-fig.count+1, "shows various diagnostic plots of the pilot data for power analysis."));
  .buffer_add(descr);
  .buffer_add("\n\n", collapse="\n");
  
  # power_stat
  link <- GetSharingLink(mSetObj)

  reportLinks <- getReportLinks(link, "power_stat", "power_stat");


  .buffer_add(reportLinks);
  .buffer_add("\n\n");
  
  fig <- c(paste0("```{r figure_ppw, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ppw, 
                  ". Various plots overviewing the test-statistics and p-values calculated from the pilot data",
                  " to evaluate if they follow a normal distribution. The top left bar chart shows the",
                  " distribution of the test-statistic, which should show a bell-curve.", 
                  " The top right bar chart shows the distribution of the p-values, with an",
                  " expectation that the majority of p-values hover around 0.",
                  " The bottom plots are Quantile-Quantile plots (QQ plots), with an expectation that",
                  " if the test-statistics and the p-values follow a standard normal distribution",
                  " the data points will follow the straight line. For the qq-plot, the p-values are sorted against", 
                  " their ranks.", 
                  "', ",
                  " fig.lp='", 
                  mSetObj$imgSet$powerstat, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$powerstat)",
           "```",
           "\n\n");
  .buffer_add(fig, collapse="\n");
  .buffer_add("\n\n", collapse="\n");
  
}

#'Create power analysis report: Power Analysis
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerAnalDoc <- function(mSetObj){
  descr <- c("\n\n<hr/>",
             "\n\n## 4. Power Analysis\n\n",
             "Following parameter selection, power analysis can begin. The ultimate aim of power analysis",
             " is to determine the minimum sample size used to detect an effect size of interest.",  
             " The sample size x statistical power relationship will be used to guide future",
             " study design based upon the pilot data. To begin, the false-discovery rate and the",
             " maximum sample size (between 60-1000) must be specified. The false-discovery rate will represent the",
             " significance criterion or the alpha level. The magnitude of the effect of interest in the population",
             " (effect size) will be calculated from the pilot data. A plot will be created to visualize the",
             " the predicted power curve. From the plot, users will be able to determine the most appropriate sample size and",
             " its associated predicted power for future studies.",
             paste("Figure", fig_panar1 <- fig.count<<-fig.count+1," shows the predicted power curve."),
            "\n\n");

  .buffer_add(descr, collapse="\n");
  
    descr <- c("The specified false discovery rate (FDR):  ```", mSetObj$analSet$power$fdr.lvl, "```.",
               "The specified max sample size (per group): ```", mSetObj$analSet$power$smplSize, "```.",
               "\n",
               "Note the FDR threshold may be adjusted automatically if it is too stringent requiring larger sample size than the max specified.");
    .buffer_add(descr, collapse="\n");

  # power_profile
  if(!is.null(mSetObj$imgSet$powerprofile)){
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "power_profile", "power_profile");

  .buffer_add(reportLinks);
  .buffer_add("\n\n");

  fig1 <- c(paste0("```{r figure_ppwana, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_panar1, 
                  ". Plot of the predicted power curve.", 
                  "', ",
                  " fig.lp='", 
                  mSetObj$imgSet$powerprofile, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$powerprofile)",
           "```",
           "\n\n");
  .buffer_add(fig1, collapse="\n");
  }
   # show the power table
    descr <- c(
      "```{r table_power, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(mSetObj$analSet$power.mat);",
      paste0("create_dt(dt_res,  caption = 'Table ", 
             table.count <<- table.count + 1,  ". Sample sizes and their predicted powers.')"),
      "```", "\n\n")
    
    .buffer_add(descr, "\n\n"); 
}

