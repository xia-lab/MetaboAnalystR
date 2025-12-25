#'Create report of Causal Analysis based on 2SMR
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalRnwReport_slides <- function(mSetObj, usrName) {
  CreateHeader(usrName);
  CreateCausalIntr_slides();
  CreateCausalInputDoc_slides(mSetObj);
  CreateCausalParametersDoc_slides(mSetObj);
  CreateCausalAnalDoc_slides(mSetObj);
  CreateSlideFooter();
}

#'Create MR analysis report: Introduction
#'@description Report generation using Sweave
#'Power analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalIntr_slides <- function() {
  if (!exists("table.count")) {
    table.count <<- 0;
  }
  if (!exists("fig.count")) {
    fig.count <<- 0;
  }

  descr <- c(
    "## Overview",
    "Untargeted LC–MS based metabolomics is used to study the effects of metabolites and small compounds on health. Identifying causal links from biomarkers is essential. MR uses genetic variation to infer causal effects.",
    "GWAS links genetic variants to phenotypes, and mGWAS links genotypes to metabolites. MR analysis in MetaboAnalyst uses the 2SMR approach with summary statistics to estimate causal relationships.",
    "\n\n---\n\n"
  )
  .buffer_add(descr, collapse="\n\n")
}

#'Create MR analysis report: Input selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalInputDoc_slides <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)

  descr <- c(
    "## Data Input",
    "Select an exposure (metabolite) and an outcome (disease) for MR analysis. The program finds instrumental variables (SNPs) associated with both from mGWAS and OpenGWAS databases.",
    "Your selected exposure and outcome are shown below:",
    "\n\n---\n\n"
  )
  
  .buffer_add(descr, collapse="\n\n")
  
  selected_exposure <- unique(mSetObj$dataSet$exposure$`Common Name`)[1]
  selected_outcome <- unique(mSetObj$dataSet$outcome$trait)[1]
  
  exposure_text <- paste("### Selected Exposure\n\n", paste("`", selected_exposure, "`", sep=""), "\n\n")
  .buffer_add(exposure_text)
  
  outcome_text <- paste("### Selected Outcome\n\n", paste("`", selected_outcome, "`", sep=""), "\n\n")
  .buffer_add(outcome_text)
  
  .buffer_add("\n\n", collapse="\n")
}

#'Create MR analysis report: Parameter setting
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalParametersDoc_slides <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  if (!exists("table.count")) {
    table.count <<- 0
  }
  
  descr <- c(
    "## SNP Filtering & Harmonization",
    "Steps include LD clumping, using LD proxies, allele harmonization, and controlling horizontal pleiotropy. The selected parameters and harmonized table are shown below:",
    "\n\n---\n\n"
  )
  
  .buffer_add(descr, collapse="\n\n")
  
  snp_filter_params <- mSetObj$dataSet$snp_filter_params
  params_descr <- paste(
    "### Selected SNP Filtering Parameters\n\n",
    "- **LD Clumping**: `", snp_filter_params$ldclumpOpt, "`\n",
    "- **LD Proxy**: `", snp_filter_params$ldProxyOpt, "`\n",
    "- **Use LD Proxies**: `", snp_filter_params$ldProxies, "`\n",
    "- **LD Threshold**: `", snp_filter_params$ldThresh, "`\n",
    "- **Allow Palindromic SNPs**: `", snp_filter_params$pldSNPs, "`\n",
    "- **MAF Threshold**: `", snp_filter_params$mafThresh, "`\n",
    "- **Harmonization**: `", snp_filter_params$harmonizeOpt, "`\n",
    "\n\n---\n\n"
  )
  
  .buffer_add(params_descr, collapse="\n")
  
  table.count <<- table.count + 1
  harmonized_data <- mSetObj$dataSet$harmonized.dat
    # Keep only selected columns
    selected_columns <- c("SNP", "pval.exposure",
                          "pval.outcome", 
                          "exposure",  "genes", "pop_code", "biofluid");
    filtered_harmonized_data <<- harmonized_data[harmonized_data$mr_keep == TRUE & harmonized_data$mr_keep.outcome == TRUE, selected_columns]
  
  # Add harmonized data table
  if (!is.null(filtered_harmonized_data)) {
      tableName <-  "Harmonized Data Table"
      CreateTitleTableSlide("dt_res <- as.data.frame(filtered_harmonized_data)", tableName)
  }
}

#'Create MR analysis report: Analysis and results
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
CreateCausalAnalDoc_slides <- function(mSetObj) {
  mSetObj <- .get.mSet(mSetObj)
  
  descr <- c(
    "## Causal Analysis using MR",
    "Multiple statistical methods are available for MR analysis. Each method has its own strengths and limitations. For more details, refer to the [Guidelines for performing Mendelian randomization investigations](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7384151/).",
    "The results from the selected methods and their associated diagnostic figures are given below:",
    "\n\n---\n\n"
  )
  
  .buffer_add(descr, collapse="\n")
  
  exposures <- unique(mSetObj$dataSet$mr_results_merge$exposure)
  
  for (i in seq_along(exposures)) {
    exposure <- exposures[i]
    exposure_curr <<- exposure;
    
    exposure_results <- mSetObj$dataSet$mr_results_merge[mSetObj$dataSet$mr_results_merge$exposure == exposure, ]
    table.count <<- table.count + 1
    
    # MR results table
    if (!is.null(exposure_results)) {
        tableName <- sprintf("MR Results for %s", exposure)
        tableStr <- paste0("exposure_results <- mSetObj$dataSet$mr_results_merge[mSetObj$dataSet$mr_results_merge$exposure == '", exposure_curr, "',]; dt <- as.data.frame(exposure_results);")
        CreateTitleTableSlide(tableStr, tableName)
    }
    
    # Diagnostic plots
    scatter_plot <- mSetObj$imgSet$mr_scatter_plot
    forest_plot <- mSetObj$imgSet$mr_forest_plot
    leaveoneout_plot <- mSetObj$imgSet$mr_leaveoneout_plot
    funnel_plot <- mSetObj$imgSet$mr_funnel_plot
    
    # Scatter plot
    slide <- CreateTwoColumnFigureSlide(scatter_plot, "Scatter plot for MR analysis")
    .buffer_add(slide)
    
    # Forest plot
    slide <- CreateTwoColumnFigureSlide(forest_plot, "Forest plot for MR analysis")
    .buffer_add(slide)
    
    # Leave-one-out plot
    slide <- CreateTwoColumnFigureSlide(leaveoneout_plot, "Leave-one-out plot for MR analysis")
    .buffer_add(slide)
    
    # Funnel plot
    slide <- CreateTwoColumnFigureSlide(funnel_plot, "Funnel plot for MR analysis")
    .buffer_add(slide)
  }
}
