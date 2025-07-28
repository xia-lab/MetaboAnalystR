#'Create report of Causal Analysis based on 2SMR
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalRnwReport <- function(mSetObj, usrName){
  CreateHeader(usrName);
  CreateCausalIntr();
  CreateCausalInputDoc(mSetObj);
  CreateCausalParametersDoc(mSetObj);
  CreateCausalAnalDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create MR analysis report: Introduction  
#'@description Report generation using Sweave
#'Power analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "LC–MS based metabolomics is increasingly applied to exposomics and toxicology studies to understand the potential effects of endogenous metabolites as 
             well as small compounds derived from food, gut microbes, or chemical contaminants on our health. These studies typically produce long lists of potential biomarkers 
             that are significantly associated with phenotypes of interest. Identification of causal links from this large number of compound-phenotype relations is a natural 
             next step. Traditionally, causal analysis require randomized control trials (RCT) which are expensive and time consuming. Sometimes, 
             this is infeasible or unethical to conduct such studies. In the past two decades, an approach to causal inference using natural genetic variation 
             (i.e. genome-wide association studies (GWAS) data), known as Mendelian Randomization (MR) has gained popularity. This approach has been referred to as ‘nature’s randomised 
              trials’ and is based on the randomisation from parents to offspring of genetic variants encapsulated in Mendel’s laws of segregation and independent assortment. 
             At a population level, the randomisation is approximate, but still allows genetic variants that are robustly associated with the measured exposure to be used to 
              estimate the unbiased causal effect of an exposure (generally acting across life) on health outcomes.", 
             "\n\n",
             "GWAS have established links between genetic variants (e.g. single nucleotide polymorphism, or SNPs) and various phenotypes, while recent mGWAS provide connections 
             between genotypes with metabolites or metabolite concentration changes. It becomes possible to estimate causal relationships between metabolites and a phenotype of interest. If a metabolite is causal for a given disease, genetic 
             variants which influence the levels of that metabolite, either directly through affecting related enzymes or indirectly through influencing lifestyle choices (such as dietary habits or lifestyle exposure), 
             should result in a higher risk of the disease. These causal effects can be estimated through Mendelian randomization (MR) analysis.", 
             "\n\n",
             "MR analysis in MetaboAnalyst is based on the 2SMR approach (using the TwoSampleMR and MRInstruments R packages) which enables application of MR methods using summary statistics 
            from non-overlapping individuals. Users should first select an exposure (i.e. a metabolite) and an outcome (i.e. a disease) of interest. Based on the selections, the program searches 
            for potential instrumental variables (i.e. SNPs) that are associated with both the metabolite from our large collections of the recent mGWAS studies and the disease from the 
            OpenGWAS database. ",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE);
}


#'Create MR analysis report: Input selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalInputDoc <- function(mSetObj = NA) {
  
  # Retrieve the mSet object
  mSetObj <- .get.mSet(mSetObj)
  link <- GetSharingLink(mSetObj)

  # Description content
  descr <- c(
    "\n\n<hr/>",
    "\n\n## 2. Data Input\n\n",
    "To start, users should first select an exposure (i.e., a metabolite) and an outcome (i.e., a disease) of interest. Based on the selections, ",
    "the program searches for potential instrumental variables (i.e., SNPs) that are associated with both the metabolite from our large collections ",
    "of the recent <a href='https://www.mgwas.ca/mGWAS/upload/MGBrowseView.xhtml' target='_blank'>mGWAS studies</a> and the disease from the ",
    "<a href='https://gwas.mrcieu.ac.uk/' target='_blank'>OpenGWAS database</a>. Due to its complex and computing-intensive nature, MR analysis is ",
    "typically performed with one metabolite/exposure at a time, to ensure that each step is performed properly and to avoid performance issues.",
    "\n\nYour selected exposure and outcome are given below:\n\n"
  )
  
  # Write description to Rmd file
  cat(descr, file = rmdFile, append = TRUE, sep = "")
  
  # Add user-selected exposure and outcome
  selected_exposure <- unique(mSetObj$dataSet$exposure$`Common Name`)[1]
  selected_outcome <- unique(mSetObj$dataSet$outcome$trait)[1]
  
  # Write exposure to Rmd file with backticks
  exposure_text <- paste("### Selected Exposure\n\n", paste("`", selected_exposure, "`", sep=""), "\n\n")
  cat(exposure_text, file = rmdFile, append = TRUE, sep = "")
  
  # Write outcome to Rmd file with backticks
  outcome_text <- paste("### Selected Outcome\n\n", paste("`", selected_outcome, "`", sep=""), "\n\n")
  cat(outcome_text, file = rmdFile, append = TRUE, sep = "")
  
  cat("\n\n", file = rmdFile, append = TRUE, sep = "\n")
}


#'Create MR analysis report: Parameter setting
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalParametersDoc <- function(mSetObj = NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  link <- GetSharingLink(mSetObj)
  
  # Initialize table count if not already done
  if (!exists("table.count")) {
    table.count <<- 0
  }
  
  # Description content
  descr <- c(
    "\n\n<hr/>",
    "\n\n## 3. SNP Filtering & Harmonization\n\n",
    "The next step is to perform SNP filtering and harmonization. This step provides the following procedures to facilitate proper MR analysis:\n\n",
    "1. **LD Clumping**: This step aims to obtain independent IVs by performing linkage disequilibrium (LD) clumping.\n",
    "2. **LD Proxies**: In cases where the SNP query is absent in the outcome GWAS, a proxy SNP in LD with the input SNP is used, utilizing the 1000 Genomes Project (phase 3).\n",
    "3. **Allele Harmonization**: Harmonizing exposure and outcome data to ensure that the effects of the SNPs on exposure and outcome are associated with the same allele.\n",
    "4. **Horizontal Pleiotropy Control**: To control horizontal pleiotropy, exclude SNPs that are associated with multiple metabolites.\n\n",
    "The selected parameters and harmonized table are shown below:\n"
  )
  
  # Write description to Rmd file
  cat(descr, file = rmdFile, append = TRUE, sep = "")
  
  # Add selected parameters
  snp_filter_params <- mSetObj$dataSet$snp_filter_params
  params_descr <- paste(
    "\n",
    "### Selected SNP Filtering Parameters\n\n",
    "- **LD Clumping Option**: `", snp_filter_params$ldclumpOpt, "`\n",
    "- **LD Proxy Option**: `", snp_filter_params$ldProxyOpt, "`\n",
    "- **Use LD Proxies**: `", snp_filter_params$ldProxies, "`\n",
    "- **LD Threshold**: `", snp_filter_params$ldThresh, "`\n",
    "- **Allow Palindromic SNPs**: `", snp_filter_params$pldSNPs, "`\n",
    "- **MAF Threshold**: `", snp_filter_params$mafThresh, "`\n",
    "- **Harmonization Option**: `", snp_filter_params$harmonizeOpt, "`\n\n",
    sep = ""
  )
  
  cat(params_descr, file = rmdFile, append = TRUE, sep = "\n")
  
  # Increment table count
  table.count <<- table.count + 1
  
  harmonized_data <- mSetObj$dataSet$harmonized.dat
  filtered_harmonized_data <<- harmonized_data[harmonized_data$mr_keep == TRUE & harmonized_data$mr_keep.outcome == TRUE, ]
  
  reportLinks <- getReportLinks(link=link, analNavi="harmonized_dat");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  # Add harmonized data table
  harmonized_table_descr <- c(
    "```{r harmonized_table, echo=FALSE, out.width = '60%', results='asis'}", 
    "dt1 <- as.data.frame(filtered_harmonized_data);",
    "dt1$remove <- NULL",
    "dt1$outcome.deprecated <- NULL",
    "dt1$mr_keep.outcome<- NULL",
    "dt1$date_source.outcome <- NULL",
    "dt1$mr_keep.outcome<- NULL",
    "dt1$mr_keep<- NULL",
    paste0("create_dt(dt1, 'Table ", table.count, ". Harmonized Data Table', 'harmonized_table')"),
    "```",
    "\n\n"
  )
  
  cat(harmonized_table_descr, file = rmdFile, append = TRUE, sep = "\n")
  
  cat("\n\n", file = rmdFile, append = TRUE, sep = "\n")
  
  
  descr <- c(
    "\n\n<hr/>",
    "\n\n## 4. Statistical Methods for MR Analysis\n\n",
    "MR analysis methods are based on the TwoSampleMR and MRInstruments R packages. \n",
    "Among these methods, the median estimator and MR Egger regression allow for genetic pleiotropy. The methods you have chosen are summarized as below:\n\n")
  
  # Write description to Rmd file
  cat(descr, file = rmdFile, append = TRUE, sep = "")
  
  descr_md <- "";
  methods <- mSetObj$dataSet$methodType;
  for(m in methods){
    if(m=="mr_wald_ratio"){
      m <- "Wald ratio: each genetic variant is used as an instrumental variable, and the causal effect of the exposure on the outcome is estimated as the ratio of the genetic association with the outcome to the genetic association with the exposure."
    }
    if(m=="mr_two_sample_ml"){
      m <- "Maximum likelihood: The genetic effects on the exposure and outcome are modeled as a bivariate normal distribution using a maximum likelihood method, similar to IVW (fixed effect)."
    }
    if(m=="mr_egger_regression"){
      m <- "MR Egger: Estimates the causal effect adjusted for any directional pleiotropy by combining the Wald ratio into a meta-regression (with an intercept and slope parameter)."
    }
    if(m=="mr_egger_regression_bootstrap"){
      m <- "MR Egger (bootstrap): Run bootstrap to obtain standard errors for MR Egger."
    }
    if(m=="mr_simple_median"){
      m <- "Simple median: This method calculates the median of the ratio estimates."
    }
    if(m=="mr_weighted_median"){
      m <- "Weighted median: This method calculates the median of the ratio estimates, but each ratio estimate is weighted by the inverse of its variance."
    }
    if(m=="mr_penalised_weighted_median"){
      m <- "Penalised weighted median"
    }
    if(m=="mr_ivw"){
      m <- "Inverse variance weighted: IVW combines two or more random variables to minimize the variance of the weighted average, which assumes no pleiotropy"
    }
    if(m=="mr_ivw_radial"){
      m <- "Inverse variance weighted radial:  Fits a radial IVW model as described by <a href='https://pubmed.ncbi.nlm.nih.gov/29961852/' target='_blank'>Bowden et al</a>";
    }
    if(m=="mr_ivw_mre"){
      m <- "Inverse variance weighted (MRE): the multiplicative random effects model permits over-dispersion in the regression model, which permits variability between the causal estimates targeted by the genetic variations"
    }
    if(m=="mr_ivw_fe"){
      m <- "Inverse variance weighted (FE): In a fixed-effect meta-analysis, Wald ratios are combined,with each ratio's weight equaling the inverse of the variance of the SNP-outcome association"
    }
    if(m=="mr_simple_mode"){
      m <- "Simple mode: The simple mode-based estimator clusters the SNPs into groups based on similarity of causal effect estimates."
    }
    if(m=="mr_weighted_mode"){
      m <- "Weighted mode: The weighted mode-based estimator weights the number of SNPs within each cluster by the inverse variance of each SNP’s effect on the outcome and returns the casual estimate based on the cluster that has the largest weighted number of SNPs"
    }
    if(m=="mr_weighted_mode_nome"){
      m <- "Weighted mode (NOME): Weighted mode with NO Measurement Error (NOME) assumption and simple mode (NOME)."
    }
    if(m=="mr_simple_mode_nome"){
      m <- "Simple mode (NOME): Simple mode with NO Measurement Error (NOME) assumption."
    }
    if(m=="mr_raps"){
      m <- "Robust adjusted profile score (RAPS)"
    }
    if(m=="mr_sign"){
      m <- "Sign concordance test: conducts a binomial test to determine if the proportion of positive signs is greater (in the event of a positive effect) or smaller (in the case of a negative effect) than would be predicted by chance"
    }
    if(m=="mr_uwr"){
      m <- "Unweighted regression: Similar to the IVW (fixed effects), but all SNPs are weighted equally."
    }
    m_ds <- paste0("- ", m)
    descr_md <- paste(descr_md, m_ds, sep = "\n")
  }
  cat(descr_md, file = rmdFile, append = TRUE, sep = "")
  
  cat("\n\n", file = rmdFile, append = TRUE, sep = "\n")
}

#'Create MR analysis report: Analysis and results
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateCausalAnalDoc <- function(mSetObj) {
  
  # Retrieve mSetObj
  mSetObj <- .get.mSet(mSetObj)
  link <- GetSharingLink(mSetObj)
  
  # Description content
  descr <- c(
    "\n\n<hr/>",
    "\n\n## 5. Causal Analysis using MR\n\n",
    "Multiple statistical methods are available for MR analysis. The module currently provides 12 methods based on the TwoSampleMR and MRInstruments R packages, ",
    "each of which has its own strengths and limitations. For instance, the weighted median method is robust to the violation of MR assumptions by some of the genetic variants, ",
    "while Egger regression method is more robust to horizontal pleiotropy. Users can point their mouse over the corresponding question marks beside each method to learn more details.",
    "\n\n",
    "There is no single right way to perform a MR investigation. Best practice will depend on the aim of the investigation and the specific exposure and outcome variables. ",
    "For more detailed information, we refer to the latest <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7384151/' target='_blank'>Guidelines for performing Mendelian randomization investigations</a>.",
    "\n\n",
    "The results from the selected methods and their associated diagnostic figures are given below:\n\n"
  )
  
  # Write description to Rmd file
  cat(descr, file = rmdFile, append = TRUE, sep = "\n")
  
  # Get unique exposures
  exposures <- unique(mSetObj$dataSet$mr_results_merge$exposure)
  
  for (i in seq_along(exposures)) {
    exposure <- exposures[i]
    exposure_curr <<- exposure;
    
    reportLinks <- getReportLinks(link=link, analNavi="mr_results_merge");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    # Filter the MR results by exposure
    exposure_results <- mSetObj$dataSet$mr_results_merge[mSetObj$dataSet$mr_results_merge$exposure == exposure, ]
    table.count <<- table.count + 1
    # Write the MR results table
    table_descr <- c(
      paste0("```{r mr_results_table_", i, ", echo=FALSE, out.width = '60%', results='asis'}"), 
      paste0("exposure_results <- mSetObj$dataSet$mr_results_merge[mSetObj$dataSet$mr_results_merge$exposure == '", exposure_curr, "', ];"),
      "dt <- as.data.frame(exposure_results);",
      paste0("create_dt(dt, 'Table ", table.count, ". MR Results for ", exposure, "', 'mr_results_table')"),
      "```",
      "\n\n"
    )
    
    cat(table_descr, file = rmdFile, append = TRUE, sep = "\n")
    
    # Write the diagnostic plots
    scatter_plot <- mSetObj$imgSet$mr_scatter_plot[exposure]
    forest_plot <- mSetObj$imgSet$mr_forest_plot[exposure]
    leaveoneout_plot <-mSetObj$imgSet$mr_leaveoneout_plot[exposure]
    funnel_plot <- mSetObj$imgSet$mr_funnel_plot[exposure]
    
    reportLinks <- getReportLinks(link=link, "mr_results_merge", gsub("_[0-9]+_dpi150\\.png$", "", scatter_plot));
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    # Scatter plot
    descr1 <- "The scatter plot shows the relationships between SNP effects on exposure against the SNP effects on the outcome, with the slope indicating the causal association."
    cat(descr1, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig.count<<-fig.count+1;
    scatter_descr <- c(
      paste0("```{r diagnostic_plots_scatter_", i, ", echo=FALSE, out.width = '", getFigWidth(mSetObj), "', fig.cap='Figure ",fig.count,". Scatter plot of of exposure: ", exposure, "'}"), 
      paste0("knitr::include_graphics('", scatter_plot, "')"),
      "```",
      "\n\n"
    )
    cat(scatter_descr, file = rmdFile, append = TRUE, sep = "\n")
    
    reportLinks <- getReportLinks(link=link, "mr_results_merge", gsub("_[0-9]+_dpi150\\.png$", "",forest_plot));
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    # Forest plot
    descr2 <- "The forest plot compares the causal effect calculated using the methods that include all the SNPs to using each SNP separately."
    cat(descr2, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig.count<<-fig.count+1;
    forest_descr <- c(
      paste0("```{r diagnostic_plots_forest_", i, ", echo=FALSE, out.width = '", getFigWidth(mSetObj), "', fig.cap='Figure ",fig.count,". Forest plot of of exposure: ", exposure, "'}"), 
      paste0("knitr::include_graphics('", forest_plot, "')"),
      "```",
      "\n\n"
    )
    cat(forest_descr, file = rmdFile, append = TRUE, sep = "\n")
    
    reportLinks <- getReportLinks(link=link, "mr_results_merge", gsub("_[0-9]+_dpi150\\.png$", "",leaveoneout_plot));
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    # Leave-one-out plot
    descr3 <- "To determine whether a single SNP is having a disproportionately larger impact on an association. Each dot represents the MR analysis excluding that specific SNP using IVW method."
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig.count<<-fig.count+1;
    leaveoneout_descr <- c(
      paste0("```{r diagnostic_plots_leaveoneout_", i, ", echo=FALSE, out.width = '", getFigWidth(mSetObj), "', fig.cap='Figure ",fig.count,". Leave-one-out plot of of exposure: ", exposure, "'}"), 
      paste0("knitr::include_graphics('", leaveoneout_plot, "')"),
      "```",
      "\n\n"
    )
    cat(leaveoneout_descr, file = rmdFile, append = TRUE, sep = "\n")
    
    reportLinks <- getReportLinks(link=link, "mr_results_merge", gsub("_[0-9]+_dpi150\\.png$", "",funnel_plot));
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    # Funnel plot
    descr4 <- "A funnel plot’s asymmetry can be used to determine how reliable a certain MR method is. Wider spread implies greater heterogeneity, which may be due to horizontal pleiotropy."
    cat(descr4, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig.count<<-fig.count+1;
    funnel_descr <- c(
      paste0("```{r diagnostic_plots_funnel_", i, ", echo=FALSE, out.width = '", getFigWidth(mSetObj), "', fig.cap='Figure ",fig.count,". Funnel plot of of exposure: ", exposure, "'}"), 
      paste0("knitr::include_graphics('", funnel_plot, "')"),
      "```",
      "\n\n"
    )
    cat(funnel_descr, file = rmdFile, append = TRUE, sep = "\n")
    
  }
}


