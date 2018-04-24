#'Create report of analyses (Power)
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreatePowerIntr();
  
  CreatePowerOverview();
  CreatePowerInputDoc(mSetObj);
  CreateStatNORMdoc(mSetObj)
  
  CreatePowerParametersDoc(mSetObj);
  CreatePowerAnalDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create power analysis report: Introduction  
#'@description Report generation using Sweave
#'Power analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerIntr <- function(){
  descr <- c("\\section{Background}\n",
             "The Power analysis module supports sample size estimation and power analysis for designing", 
             " population-based or clinical metabolomic studies. As metabolomics is becoming a more accessible",
             " and widely used tool, methods to ensure proper experimental design are crucial to allow for accurate", 
             " and robust identification of metabolites linked to disease, drugs, environmental or genetic differences.", 
             " Traditional power analysis methods are unsuitable for metabolomics data as the high-throughput nature of", 
             " this data means that it is highly dimensional and often correlated. Further, the number of metabolites", 
             " identified greatly outnumbers the sample size. Thus, modified methods of power analysis are needed to", 
             " address such concerns. One solution is to use the average power of all metabolites, and to correct", 
             " for multiple testing using methods such as the false discovery rate (FDR) instead of raw p-values.", 
             " MetaboAnalystR uses the SSPA R package to perform power analysis.",
             " For more information, please refer to the original paper by van Iterson et al. (PMID: 19758461)\n."
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create power analysis report: Overview
#'@description Report generation using Sweave
#'Power analysis report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerOverview <- function(){
  descr <- c("\\section{Power Analysis Overview}\n",
             "The power analysis module consists of four steps - uploading pilot data, data processing,",
             " selecting parameters for power analysis, and viewing the results. The analysis will use",
             " the entire set of uploaded pilot data \n."
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create power analysis report: Data Input
#'@description Report generation using Sweave
#'Power analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The power analysis module accepts either a compound concentration table, spectral binned data, or a peak intensity",
             " table. The format of the data must be specified, identifying whether the samples are in rows or columns, and whether",
             " or not the data is paired. The data may either be .csv or .txt files.",
             " \n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # the data filtering
  descr <- c("\\subsubsection{Data Filtering}\n",
             "The purpose of data filtering is to identify and remove variables that are unlikely to be of",
             " use when modeling the data. No phenotype information is used in the filtering process, so the result",
             " can be used with any downstream analysis. This step can usually improve the results.",
             " Data filtering is strongly recommended for datasets with a large number of variables (> 250) and",
             " for datasets which contain a lot of noise (i.e.chemometrics data). Filtering can usually improve your",
             " results\\footnote{Hackstadt AJ, Hess AM.\\textit{Filtering for increased power for microarray data analysis},",
             " BMC Bioinformatics. 2009; 10: 11.}.",
             " \n\n",
             " \\textit{For data with < 250 of variables, filtering will reduce 5\\% of variables;",
             " For a total number of variables between 250 and 500, 10\\% of variables will be removed;",
             " For a total number of variables bewteen 500 and 1000, 25\\% of variables will be removed;",
             " Finally, 40\\% of variables will be removed for data with over 1000 variables.}");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  filt.msg <- mSetObj$msgSet$filter.msg;
  if(is.null(filt.msg)){
    filt.msg <- "No data filtering was performed.";
  }
  cat(filt.msg, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr <- c("\\subsubsection{Data Integrity Check}\n",
             "Before data analysis, a data integrity check is performed to make sure that all of the necessary",
             " information has been collected. The class labels must be present and must contain only two classes.",
             " If the samples are paired, the class label must be from -n/2 to -1 for one group, and 1 to n/2 for the second group",
             " (n is the sample number and must be an even number). Class labels with the same absolute value are assumed to be pairs.",
             " Compound concentration or peak intensity values must all be non-negative numbers.",
             " By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
             " found within the data (see next section).");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr <- c("\\subsubsection{Missing value imputations}\n",
             "Too many zeroes or missing values will cause difficulties in the downstream analysis.",
             " MetaboAnalystR offers several different methods for this purpose. The default method replaces ",
             " all the missing and zero values with a small values (the half of the minimum positive",
             " values in the original data) assuming to be the detection limit. The assumption of this approach",
             " is that most missing values are caused by low abundance metabolites (i.e.below the detection limit).",
             " In addition, since zero values may cause problem for data normalization (i.e. log), they are also ",
             " replaced with this small value. User can also specify other methods, such as replace by mean/median,",
             " or use K-Nearest Neighbours, Probabilistic PCA (PPCA), Bayesian PCA (BPCA) method, Singular Value Decomposition (SVD)",
             " method to impute the missing values \\footnote{Stacklies W, Redestig H, Scholz M, Walther D, Selbig J.",
             " \\textit{pcaMethods: a bioconductor package, providing PCA methods for incomplete data.}, Bioinformatics",
             " 2007 23(9):1164-1167}. Please select the one that is the most appropriate for your data.");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  if(is.null(mSetObj$msgSet$replace.msg))
    mSetObj$msgSet$replace.msg <- "No data replacement was performed."
  
  cat(mSetObj$msgSet$replace.msg, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "CreateSummaryTable(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create power analysis report: Power Parameter Selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerParametersDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Parameter Selection}\n",
             "Before proceeding to the power analysis, the two groups on which to perform said analysis",
             " must be selected. Further, there will be four diagnostic plots which display a visual overview",
             " of the test-statistics and p-values, providing context for whether or not the normalization was sufficient.",
             " The shape of the test-statistic should follow a near-normal distribution, and the majority of p-values",
             " should be close to zero.\n");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- paste("Figure", fig.count<<-fig.count+1, "Exploratory plot overview of pilot-data for power analysis.")
  
  powerparamhist <- c( "\\begin{figure}[htp]",
                       "\\begin{center}",
                       paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$powerstat,"}", sep=""),
                       "\\caption{", paste("Various plots overviewing the test-statistics and p-values calculated from the pilot data",
                                           " to evaluate if they follow a normal distribution. The top left bar chart shows the",
                                           " distribution of the test-statistic, which should show a bell-curve.", 
                                           " The top right bar chart shows the distribution of the p-values, with an",
                                           " expectation that the majority of p-values hover around 0.",
                                           " The bottom plots are Quantile-Quantile plots (QQ plots), with an expectation that",
                                           " if the test-statistics and the p-values follow a standard normal distribution",
                                           " the data points will follow the straight line. For the qq-plot, the p-values are sorted against", 
                                           " their ranks.", sep=""),"}",
                       "\\end{center}",
                       paste("\\label{",mSetObj$imgSet$powerstat,"}", sep=""),
                       "\\end{figure}",
                       "\\clearpage"
  );
  cat(powerparamhist, file=rnwFile, append=TRUE, sep="\n");
  
}

#'Create power analysis report: Power Analysis
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreatePowerAnalDoc <- function(mSetObj){
  descr <- c("\\section{Power Analysis}\n",
             "Following parameter selection, power analysis can begin. The ultimate aim of power analysis",
             " is to determine the minimum sample size used to detect an effect size of interest.",  
             " The sample size x statistical power relationship will be used to guide future",
             " study design based upon the pilot data. To begin, the false-discovery rate and the",
             " maximum sample size (between 60-1000) must be specified. The false-discovery rate will represent the",
             " significance criterion or the alpha level. The magnitude of the effect of interest in the population",
             " (effect size) will be calculated from the pilot data. Two plots will be created to visualize the",
             " density of estimated effect sizes, and to visualize the predicted power curve. From these plots,",
             " users will be able to determine the most appropriate sample size and its associated predicted power for future studies.",
             " The power analysis provides invaluable insight for proper experimental design, as well as strengthens the ability to detect",
             " true differences within a metabolomic data set.",
             paste("Figure", fig.count<<-fig.count+1," shows the density of estimated effect sizes, and "),
             paste("Figure", fig.count<<-fig.count+1," shows the predicted power curve."))
  cat(descr, file=rnwFile, append=TRUE)
  
  powereffect <- c( "\\begin{figure}[htp]",
                    "\\begin{center}",
                    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$powereffect,"}", sep=""),
                    "\\caption{Plot of the density of effect sizes, the blue line represents the estimated density of effect sizes.","}",
                    "\\end{center}",
                    paste("\\label{",mSetObj$imgSet$powerstat,"}", sep=""),
                    "\\end{figure}",
                    "\\clearpage"
  );
  cat(powereffect, file=rnwFile, append=TRUE, sep="\n")
  
  powerprofile <- c( "\\begin{figure}[htp]",
                     "\\begin{center}",
                     paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$powerprofile,"}", sep=""),
                     "\\caption{Plot of the predicted power curve.", "}",
                     "\\end{center}",
                     paste("\\label{",mSetObj$imgSet$powerstat,"}", sep=""),
                     "\\end{figure}",
                     "\\clearpage"
  )
  cat(powerprofile, file=rnwFile, append=TRUE, sep="\n")
}

