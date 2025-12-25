#'Create report of analyses (Power)
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerRnwReport_slides <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreatePowerIntr_slides ();
  
  CreateDataProcSlides(mSetObj);
  CreateNORMSlides(mSetObj);
  
  CreatePowerParametersDoc(mSetObj);
  CreatePowerAnalDoc(mSetObj);
    AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
}

#'Create power analysis report: Introduction  
#'@description Report generation using Sweave
#'Power analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerIntr_slides <- function() {
    # Initialize slide and figure counters if they don't exist
    if(!exists("table.count")) {
        table.count <<- 0;
    }
    if(!exists("fig.count")) {
        fig.count <<- 0;
    }
  
    # Introduction slide for Power Analysis
    introSlide <- c(
        "## Power Analysis Overview\n\n",
        "- **Purpose:** Helps determine the minimal sample size to detect test effects at a specific significance level.\n",
        "- **Importance:** Critical for avoiding Type II errors (false negatives) in studies with small sample sizes.\n",
        "- **Challenge:** Traditional power analysis methods are unsuitable for high-dimensional, correlated metabolomics data.\n",
        "- **Solution:** Utilizes the SSPA method from the Bioconductor R package, focusing on average metabolite power and adjusting for multiple testing using FDR.\n",
        "- **Reference:** For detailed methodology, see [van Iterson et al.](https://pubmed.ncbi.nlm.nih.gov/19758461/).\n",
        "\n---\n\n"
    )
    .buffer_add(introSlide, collapse="\n")
}


#'Create power analysis report: Power Parameter Selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerParametersDoc_slides <- function(mSetObj=NA) {
    mSetObj <- .get.mSet(mSetObj);
  
    # Diagnostic Plots Introduction
    intro <- c(
        "## Diagnostic Plots for Power Analysis\n\n",
        "- **Selection of Groups**: Choose two groups for analysis.",
        "- **Visualization**: Four diagnostic plots provide an overview of test-statistics and p-values.",
        "- **Distribution Expectation**: Test-statistics should resemble a near-normal distribution.",
        "- **P-value Distribution**: Majority should be close to zero for sufficient normalization.\n"
    )
    .buffer_add(intro, collapse="\n")
    .buffer_add("\n\n---\n\n")
    
    # Diagnostic Plots
    diagnosticPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$powerstat, "Various diagnostic plots assessing normalization sufficiency and data distribution.")
    .buffer_add(diagnosticPlotSlide)
    .buffer_add("\n\n---\n\n")
}


#'Create power analysis report: Power Analysis
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePowerAnalDoc_slides <- function(mSetObj=NA) {
    mSetObj <- .get.mSet(mSetObj);

    if (is.null(mSetObj$analSet$multiROC$auc.vec)) {
        return()
    }

    # Power Analysis Introduction
    intro <- c(
        "# Power Analysis Overview\n\n"
    )
    .buffer_add(intro, collapse="\n")
    .buffer_add("\n\n---\n\n")

    # Power Analysis Plots and Tables
    powerAnalysisSlide1 <- CreateTwoColumnFigureSlide(mSetObj$imgSet$powerprofile, "Predicted power curve based on the average cross validation performance.")
    .buffer_add(powerAnalysisSlide1)
    .buffer_add("\n\n---\n\n")

    # Additional slides can be appended similarly for other plots and tables if necessary
}
