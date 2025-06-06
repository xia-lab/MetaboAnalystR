
SummarizeNormResults <- function(mSetObj = NA, par1) {
  #save.image("sum.RData");
  mSetObj <- .get.mSet(mSetObj)
  
  # Check if selectedNormOpts has been defined
  if (!exists('selectedNormOpts')) {
    AddErrMsg("Please select which normalization options to explore")
    return(0)
  }

  # We'll store the results in a named list, one entry per normOpt
  resList <- list()

  # Initialize variables for scoring
  bestNormOpt <- NULL
  highestScore <- -Inf
  scores <- list()  # Store scores for each normalization

  # Loop over each chosen normalization
  for (i in seq_along(selectedNormOpts)) {
    normOpt <- selectedNormOpts[i]

    # 1) Load the workspace for this normOpt
    mSetObj <- NA;
    load(paste0(normOpt, '/Rload.RData'))

    mSetObj <- mSet;

    # 2) Extract the number of DE features (from Volcano analysis)
    if (!is.null(mSetObj$analSet$volcano) && !is.null(mSetObj$analSet$volcano$sig.mat)) {
      numDE <- nrow(mSetObj$analSet$volcano$sig.mat)
    } else {
      numDE <- 0  # Default to 0 if not available
    }

    mSetObj$analSet$de.method <- "limma";
    SetSelectedMetaInfo(1, "NA", "NA");
    adj.vec <<- character(0);
    mSetObj <- GenerateContrastMatrix(mSetObj, par1);
    mSetObj <- PerformLimmaAnalysis(mSetObj, par1);

    # Number of features with p-value > 0.05 (significantly differentially expressed)
    dataSet <- mSetObj$dataSet;
    #print(dim(dataSet$norm));
    numDEpval <- sum(dataSet$comp.res$P.Value < 0.05)
   
    # Number of features with log fold-change > 1 (strongly differentially expressed)
    numDEfc <- sum(dataSet$comp.res$logFC > 1)

    if(!is.null(mSetObj$analSet$pca$permanova.res)){
      statObj <- mSetObj$analSet$pca$permanova.res;
      # For convenience, store both the string summary and numeric stats
      statInfo     <- statObj$stat.info;      # e.g. "[PERMANOVA] F-value: 2.31; R-squared: 0.12; p-value: 0.03"
      statInfoVec  <- statObj$stat.info.vec;  # numeric named vector of F-value, R-squared, p-value
      pairwiseInfo <- statObj$pair.res;       # if present
      R2 <- statObj$stat.info.vec["R-squared"]

    } else {
      LoadRscriptsOnDemand("stat");
      PCA.Anal(mSetObj);
      mSetObj <- .get.mSet(mSetObj)
      PlotPCA2DScore(mSetObj, "pca_score2d_0", "png", 72, NA, 1, 2, 0.95, 1, 0);
      mSetObj <- .get.mSet(mSetObj)

      statObj <- mSetObj$analSet$pca$permanova.res;
      # For convenience, store both the string summary and numeric stats
      statInfo     <- statObj$stat.info;      # e.g. "[PERMANOVA] F-value: 2.31; R-squared: 0.12; p-value: 0.03"
      statInfoVec  <- statObj$stat.info.vec;  # numeric named vector of F-value, R-squared, p-value
      pairwiseInfo <- statObj$pair.res;       # if present
      R2 <- statObj$stat.info.vec["R-squared"]
    }

    # this is inadmissible, set to 0
    # 4) Perform Royston's test for multivariate normality
    #require(MVN)
    #mvnRes <- mvn(as.matrix(mSetObj$dataSet$norm), mvnTest = "mardia")
    #if (!is.null(mvnRes$multivariateNormality)) {
    #  skewnessPValue <- as.numeric(as.character(mvnRes$multivariateNormality[1, "p value"]))
    #  kurtosisPValue <- as.numeric(as.character(mvnRes$multivariateNormality[2, "p value"]))
    #} else {
      skewnessPValue <- 0  # Default to 0 for p-value if not available
      kurtosisPValue <- 0
    #}

    # 5) Calculate a score for the normalization method
    # Higher R2, higher numDE, and higher normality p-values (closer to normality) are better
     score <- (R2 * 100) + numDE + (skewnessPValue + kurtosisPValue) * 50
     scores[[normOpt]] <- score

    # Update the best normalization option
    if (score > highestScore) {
      highestScore <- score
      bestNormOpt <- normOpt
    }

    # Create a string summarizing Mardia's skewness and kurtosis results
    # mardiaInfo <- paste(
    #  "[Mardia Test]",
    #  "Skewness p =", signif(skewnessPValue, 5), ";",
    #  "Kurtosis p =", signif(kurtosisPValue, 5)
    #)

    # Store everything in a sub-list
    resList[[normOpt]] <- list(
      numDE = numDE,
      pcaR2 = R2,
    #  normalityInfo = mardiaInfo,
      pcaPermInfo  = statInfo,
      pcaPermStats = statInfoVec,
      pcaPairRes   = pairwiseInfo,
      score = score,
      numDEpval= numDEpval,
      numDEfc =numDEfc
    )

    #print(resList[[normOpt]])
  }

  # Add the best normalization method to the summary
  mSetObj$summaryNormResults <- resList
  mSetObj$summaryNormResults$bestNormOpt <- bestNormOpt
  mSetObj$summaryNormResults$scores <- scores

  return(.set.mSet(mSetObj))
}

.perform_limma <- function(mSetObj, robustTrend = F){
  require(limma);
  design <- mSetObj$analSet$design;
  contrast.matrix <- mSetObj$analSet$contrast.matrix;
  dataSet<- mSetObj$dataSet

  if(length(dataSet$rmidx)>0){
       data.norm <- dataSet$norm[,-dataSet$rmidx]
    }else{
       data.norm <- dataSet$norm
    }
    data.norm <- t(data.norm);

    if (is.null(dataSet$block)) {
      fit <- lmFit(data.norm, design)
    } else {
      corfit <- duplicateCorrelation(data.norm, design, block = dataSet$block)
      fit <- lmFit(data.norm, design, block = dataSet$block, correlation = corfit$consensus)
    }
    
    if (!is.fullrank(design)) {
      msgSet$current.msg <- "This metadata combination is not full rank! Please use other combination.";
      saveSet(msgSet, "msgSet");  
      return(0)
    }
   
    df.residual <- fit$df.residual
    if (all(df.residual == 0)) {
      msgSet$current.msg <- "All residuals equal 0. There is not enough replicates in each group (no residual degrees of freedom)!";
      saveSet(msgSet, "msgSet");  
      return(0);
    }
    fit2 <- contrasts.fit(fit, contrast.matrix);
    fit2 <- eBayes(fit2, trend=robustTrend, robust=robustTrend);
    topFeatures <- topTable(fit2, number = Inf, adjust.method = "fdr");
  

  nms <- colnames(topFeatures)
  nms[which(nms == "FDR")] <- "adj.P.Val";
  nms[which(nms == "PValue")] <- "P.Value";
 
  colnames(topFeatures) <- nms  
  dataSet$comp.res <- topFeatures;
  mSetObj$dataSet <- dataSet;
  return(mSetObj);
}


GetImageDesc <- function(mSetObj = NA, type) {
  mSetObj <- .get.mSet(mSetObj)
  descr <- "NA"

  descr <- switch(
  type,
  
  "volcano" = paste0(
   "<b>[Volcano plot]</b>: ", 
    "A total of <b>", 
    ifelse(!is.null(mSetObj$analSet$volcano$sig.mat) && nrow(mSetObj$analSet$volcano$sig.mat) > 0, 
           nrow(mSetObj$analSet$volcano$sig.mat), "0"), 
    "</b> significant features were identified based on FC (x-axis) threshold: <b>", 
    ifelse(!is.null(mSetObj$analSet$volcano$raw.threshx), mSetObj$analSet$volcano$raw.threshx, "N/A"), 
    "</b>, and t-tests p-value (y-axis) threshold: <b>", 
    ifelse(!is.null(mSetObj$analSet$volcano$raw.threshy), mSetObj$analSet$volcano$raw.threshy, "N/A"), 
    "</b>, type <b>", 
    ifelse(!is.null(mSetObj$analSet$volcano$pval.type), mSetObj$analSet$volcano$pval.type, "N/A"), 
    "</b>." 
  ),

  "fc" = paste0(
   "<b>[Fold Changes]</b>: ", 
    "A total of <b>", 
    ifelse(!is.null(mSetObj$analSet$fc$sig.mat) && nrow(mSetObj$analSet$fc$sig.mat) > 0, 
           nrow(mSetObj$analSet$fc$sig.mat), "0"), 
    "</b> significant features were identified with FC threshold: <b>", 
    ifelse(!is.null(mSetObj$analSet$fc$raw.thresh), mSetObj$analSet$fc$raw.thresh, "N/A"), 
    "</b>." 
  ),

  "tt" = paste0(
   "<b>[T Tests]</b>: ",
    "A total of <b>", 
    ifelse(!is.null(mSetObj$analSet$tt$sig.num), mSetObj$analSet$tt$sig.num, "0"), 
    "</b> significant features were identified.",
    "Important features selected by <b>", 
    ifelse(!is.null(mSetObj$analSet$tt$tt.nm), mSetObj$analSet$tt$tt.nm, "N/A"), 
    "</b> with threshold <b>", 
    ifelse(!is.null(mSetObj$analSet$tt$raw.thresh), mSetObj$analSet$tt$raw.thresh, "N/A"), 
    "</b>, type <b>", 
    ifelse(!is.null(mSetObj$analSet$tt$pval.type), mSetObj$analSet$tt$pval.type, "N/A"), 
    "</b>" 
  ),

  "ptn_multifac" = paste0(
   "<b>[Correlation Analysis]</b>: Metadata of interest. ",
    "<br>- Target of interest: <b>", 
    ifelse(!is.null(params$pattern), params$pattern, "N/A"), 
    "</b>;<br>- Selected covariates: <b>", 
    ifelse(!is.null(params$cov.vec) && length(params$cov.vec) > 0, 
           paste(params$cov.vec, collapse = "; "), "None"), 
    "</b>;<br>- Correlation Measure: <b>", 
    ifelse(!is.null(params$dist.name), params$dist.name, "N/A"), 
    "</b>."
  ),
  "tree" = paste0(
   "<b>[Clustering Dendrograms]</b>: ",
    "Distance measure: ", 
    mSetObj$analSet$tree$dist.par,
    " and clustering algorithm: ",
    mSetObj$analSet$tree$clust.par,
    "."
  ),

  "heatmap" = paste0(
   "<b>[Clustering Heatmaps]</b>: ",
    "Distance measure: ", 
    mSetObj$analSet$htmap$dist.par,
    " and clustering algorithm: ",
    mSetObj$analSet$htmap$clust.par,
    "."
  ),

  "som" = paste0(
   "<b>[SOM Clusters]</b>: ",
    "Each cluster shows the abundance profiles (y) across different features (x). ",
    "The blue lines are the median intensities of the corresponding clusters."
  ),

  "rf_cls" = paste0(
   "<b>[Classification errors]</b>: ",
    "Cumulative error rates by Random Forest classification. ",
    "The overall error rate is shown as the black line, while the red and green lines represent the error rates for each class."
  ),

  "rf_imp" = paste0(
   "<b>[Important features]</b>: ",
    "Features are ranked by the mean decrease in Random Forests classification accuracy when they are permuted."
  ),

  "rf_outlier" = "<b>[Potential outliers]</b>: Potential outliers identified by Random Forest. Only the top five are labeled.",
  "rf_cls_multifac" = "<b>[RF Parameters]</b>: Classification error vs. number of trees.",
  "rf_imp_multifac" = "<b>[Important Features]</b>: Features ranked by their contributions to classification accuracy.",
  "rf_outlier_multifac" = "<b>[Potential Outliers]</b>: Up to five potential outliers flagged by random forest algorithm.",
  "km" = "<b>[K-means cluster profiles]</b>: Each cluster shows the abundance profiles (y) across different features (x). The blue lines represent median intensities of corresponding clusters.",
  "km_pca" = "<b>[K-means cluster profiles]</b>: PCA score plot colored by K-means cluster.",
  "svm_imp" = "<b>[SVM Sig. Features]</b>: Significant features identified by R-SVM. Features are ranked by their frequencies of being selected in the classifer.",
  "svm_cls" = "<b>[SVM performance]</b>: Recursive classification with SVM with different features. The red circle indicates the best classifier.",
  "roc_univ" = "<b>[Classical ROC]</b>: The ROC curve of an individual biomarker.",
  "cls_roc" = paste0(
    "<b>[ROC Overview]</b>: ",
    "ROC curves for all biomarker models based on their average performance ",
    "across all MCCV runs. For a single biomarker model, the 95 percent confidence interval is shown. ",
    "Selected model: ", paste(mSetObj$imgSet$roc.multi.model), "."
    ),
  "cls_accu" = "<b>[Predictive accuracy]</b>: Plot of the predictive accuracy of biomarker models with an increasing number of features.",
  "cls_prob" = paste0(
    "<b>[Class Probabilities]</b>: ",
    "Predicted class probabilities for all samples. ",
    "The classification boundary is at the center (x = 0.5, dotted line). ",
    "Selected model: ", paste(mSetObj$imgSet$roc.prob.name), "."
    ),

  "cls_roc" = paste0(
   "<b>[Predictive accuracy]</b>: ",
    "Performances of biomarker models with an increasing number of features. ",
    "The most accurate biomarker model is highlighted with a red dot."
    ),

  "cls_imp" = paste0(
    "<b>[Feature importance]</b>: ",
    "Important features of a selected model ranked from most to least important. ",
    "Selected model: ", paste(mSetObj$imgSet$roc.imp.name), "."
    ),

  "cls_test_roc" = paste0(
    "<b>[ROC Overview]</b>: ",
    "ROC curve for the created biomarker model based on its average performance.",
    "The 95 percent confidence interval can be computed. ",
    "Selected method: ", paste(mSetObj$imgSet$roc.testcurve.method), "."
    ),

  "cls_test_prob" = paste0(
    "<b>[Class Probabilities]</b>: ",
    "Plot of the predicted class probabilities for all samples using the created biomarker model. ",
    "The classification boundary is at the center (x = 0.5, dotted line). "
    ),

  "cls_test_accu" = "<b>[Predictive accuracy]</b>: Box plot of the predictive accuracy of the created biomarker model.",
  "roc_perm" = paste0(
    "<b>[Permutation Tests]</b>: ",
    "The plot shows the performance of all permutations compared with original labels.",
    "The empirical p-value is also shown. ",
    "Performance measure: ", paste(mSetObj$imgSet$roc.perm.method), "."
    ),
"qc_meta_pca" = paste0(
  "<b>[PCA Plot]</b>: ",
  "Principal Component Analysis (PCA) was performed on the merged feature matrix across all selected datasets. ",
  "Each point represents a sample, colored by condition and shaped by dataset. "
),
  "roc_new_samples" = "<b>[Class Prediction]</b>: Predicted class labels with probabilities for new samples.",
  
    "power_stat" = paste0(
        "<b>[Diagnostic Plots]</b>: ",
        "The test statistics (t-statistics) are expected to follow a near-normal distribution. ",
        "Some compounds should be significantly different between the two selected conditions."
    ),
  "power_profile" = "<b>[Power curve]</b>: showing predicted power with increasing sample sizes.",
  "ora" = "<b>[ORA Bar plot]</b>: showing enriched functions from over representation analysis.",
  "ora_dot" = "<b>[ORA Dot Plot]</b>: showing enriched functions from over representation analysis.",
  "qea" = "<b>[QEA Bar plot]</b>: showing enriched functions from quantitative enrichment analysis.",
  "qea_dot" = "<b>[QEA Dot plot]</b>: showing enriched functions from quantitative enrichment analysis.",
  "meta_peaks_to_paths" = "<b>[Pathway Overview]</b>: Summary of meta-pathway analysis results based on mummichog.",
  "meta_peaks_to_paths_gsea" = "<b>[Pathway Overview]</b>: Summary of meta-pathway analysis results based on GSEA.",
  "peaks_to_paths" = "<b>[Pathway Overview]</b>: Summary of pathway analysis based on mummichog.",
  "integ_peaks" = "<b>[Pathway Overview]</b>: Summary of integrative pathway analysis based on mummichog and GSEA.",
  "peaks_to_paths_gsea" = "<b>[Pathway Overview]</b>: Summary of GSEA Pathway Analysis.",
  "network_MetaboNet" = "<b>[Network View]</b>: KEGG global metabolic network.",
  "path_view" = "<b>[Pathway Overview]</b>: Summary of Pathway Analysis.",
  "path_view_integ"="<b>[Pathway Overview]</b>: Summary of Joint Pathway Analysis.",
  "aov2"="<b>[Two-way ANOVA]</b>: Plot of important features selected by two-way ANOVA.",
  "aov"="<b>[Two-way ANOVA]</b>: Plot of important features selected by two-way ANOVA.",
  "asca_scree"="<b>[ASCA Scree Plot]</b>: The plots shows the explained variability of each model including interaction and residual.",
  "asca_perm"="<b>[Permutations]</b>: ASCA model validation based on permutation tests with empirical p-values.",
  "asca_fa"=paste0(
    "<b>[Type 1 Pattern]</b>: Major patterns associated with ", mSetObj$dataSet$facA.lbl, "."
),
  "asca_fab"="<b>[ASCA Interaction]</b>: Major patterns associated with the Interaction between the two factors.",
  "asca_fb"=paste0(
  "<b>[Type 2 Pattern]</b>: Major patterns associated with ", mSetObj$dataSet$facB.lbl, "."
),
  "asca_impa"=paste0("<b>[Type 1 Sig. Features]</b>: Important variables associated with ", mSetObj$dataSet$facA.lbl, "."),
  "asca_impab"="<b>[Interaction Sig. Features]</b>: Variables important in interaction between the two factors.",
  "asca_impb"=paste0("<b>[Type 2 Sig. Features]</b>: Important variables associated with ", mSetObj$dataSet$facB.lbl, "."),
  "covariate_plot"="<b>[Covariate adjustment effect]</b>: Scatter plot comparing the p-values for features with and without covariate adjustment.",
  "metaHeatmap"="<b>[Metadata heatmap]</b>: showing metadata factors and their similarities.",
  "metaCorrHeatmap"= "<b>[Correlation heatmap]</b>: The plot shows relationship between metadata factors",
  "gene_metabolites" = paste0("<b>Screenshot</b>: Gene-Metabolite Interaction Network"),
  "metabo_phenotypes" = paste0("<b>Screenshot</b>: Metabolite-Disease Interaction Network"),
  "metabo_metabolites" = paste0("<b>Screenshot</b>: Metabolite-Metabolite Interaction Network"),
  "global" = paste0("<b>Screenshot</b>: KEGG Global Metabolic Network"),
  "dspc" = paste0("<b>Screenshot</b>: Debiased Sparse Partial Correlation (DSPC) Network"),
  "enrichment_network" = paste0("<b>Screenshot</b>: Enrichment network based on functions from over representation analysis (ORA)."),
  "dose_volcano" = paste0(
  "<b>Volcano plot</b>: showing a total of ", 
  sum(mSetObj$analSet$dose$inx.up, mSetObj$analSet$dose$inx.down), 
  " features selected for curve fitting."
  ), 
  "PlotDRHistogram"="<b>Histogram</b>: 20th feature - the 20th lowest BMD value; 10th percentile: the 10th percentile of the BMD values; Max 1st peak: the mode of the first peak in the distribution of BMD values.",
  "PlotDRModelBars"="<b>Bar plot</b>: Frequency of statistical models among best fit curves",
  "snorm"="<b>[Normalization View]</b>: Sample-wise box plots and density plots before and after normalization (max 50 samples)",
  "pca_loading"="<b>[Loadings Plot]</b>: PCA Loadings plot between the selected PCs.",
  "sam_view" = "<b>[SAM Overview]</b>: The plot shows delta value in relation to FDR and number of significant features",
  "sam_imp" = "<b>[SAM Sig. Features]</b>: The green circles represent features that exceed the specified threshold.",
  "ebam_imp" = "<b>[EBAM Sig. Features]</b>: The green circles represent features that exceed the specified threshold.",
  "ebam_view" = "<b>[EBAM Overview]</b>: The plot displays delta value in relation to FDR and number of significant features",
  "opls_imp" ="<b>[Important Variables]</b>: Important variables identified by OPLS-DA",
  "pls_biplot" ="<b>[Biplot]</b> PLS-DA biplot that visualizes both the individual samples (observations) and the variables (features) on two selected components.",
  "corr" ="<b>[Correlation]</b> Heatmap that visualizes the overall correlations between different features or samples based on Pearson r, Spearman rank correlation or Kendall rank correlation method.",
  "corr_heatmap" ="<b>[Correlation heatmap]</b>: visualizing the overall correlations between different features or samples based on selected measures.",
  "BPIS_72" ="<b>[Base Peak Ion Plot]</b> The base peak chromatogram is similar to the TIC, however it only shows the intensity of the most intense signal at every scan across the whole spectrum.",
  "TICS_72" ="<b>[Total Ion Chromatogram]</b> TIC is a chromatogram summed the intensities of all mass spectral peaks from same scan. All signals including noise and peak components are included in TIC",
  "PCA" ="<b>[PCA Plot]</b>: Principal Component Analysis performed on log-transformed data obtained from spectra processing",
  "Peak_Intensity" ="<b>[Peak Intensity Statistics]</b> The general peaks' intensity is analyzed from different spectral files to show the peaks' intensity distribution",
   "meta_bubble" = paste0(
       "<b>[Meta-Analysis Bubble Plot]</b>: ",
       "Each bubble represents a pathway, with its size indicating the enrichment ratio and its color representing the meta p-value. ",
       "Pathways are ranked based on statistical significance, with the most significant ones appearing at the top."
   ),
  # Default case if `type` doesn't match any of the above
  "NA"
)

if(descr == "NA"){
    #grepl(substring, text)  # TRUE
if (grepl("roc_univ", type)) {
    base_type <- gsub("roc_univ_", "", type)
    descr <- paste0(
        "<b>[", base_type, " - Classical ROC]</b>: ",
        "The ROC curve of an individual biomarker."
    )
} else if (grepl("roc_boxplot", type)) {
    base_type <- gsub("roc_boxplot_", "", type)
    descr <- paste0(
        "<b>[", base_type, " - Optimal Cutoff]</b>: ",
        "Box-plot of the concentrations of the selected feature."
    )
} else if (grepl("mr_scatter_plot", type)) {
    base_type <- gsub("_mr_scatter_plot", "", type)
    descr <- paste0(
        "<b>[", base_type, " - Scatter Plot]</b>: ",
        "The scatter plot shows the relationships between SNP effects on exposure against the SNP effects on the outcome, ",
        "with the slope indicating the causal association."
    )
} else if (grepl("mr_forest_plot", type)) {
    base_type <- gsub("_mr_forest_plot", "", type)
    print(base_type);
    descr <- paste0(
        "<b>[", base_type, " - Forest Plot]</b>: ",
        "The forest plot compares the causal effect calculated using the methods that include all the SNPs to using each SNP separately."
    )
} else if (grepl("mr_leaveoneout_plot", type)) {
    base_type <- gsub("_mr_leaveoneout_plot", "", type)
    descr <- paste0(
        "<b>[", base_type, " - Sensitivity Analysis]</b>: ",
        "To test whether a single SNP is having a disproportionately larger impact on an association. ",
        "Each dot represents the MR analysis excluding that specific SNP using the IVW method."
    )
} else if (grepl("mr_funnel_plot", type)) {
    base_type <- gsub("_mr_funnel_plot", "", type)
    descr <- paste0(
        "<b>[", base_type, " - Funnel Plot]</b>: ",
        "A funnel plot’s asymmetry can be used to determine how reliable a certain MR method is. ",
        "Wider spread implies greater heterogeneity, which may be due to horizontal pleiotropy."
    )
}else if (grepl("norm_boxdpi", type)) {
    file_base <- sub("_norm_boxdpi72", "", type)  # Remove the suffix
    descr <- paste0(
        "<b>[", file_base, " - Normalized Box Plot]</b>: ",
        "The box plot  displays the distribution of normalized values for the selected feature across different groups. ",
        "It highlights differences in feature expression and potential outliers."
    )
}
  }
  return(descr)
}


PerformLimmaAnalysis <- function(mSetObj, par1) {
  library(limma)
  
  # Step 1: Extract normalized data and metadata
  norm_data <- t(mSetObj$dataSet$norm)  # Expression data (genes x samples)
  meta_info <- mSetObj$dataSet$meta.info  # Metadata
  
  # Ensure the data is in the correct format
  if (is.null(norm_data) || nrow(norm_data) == 0 || ncol(norm_data) == 0) {
    print("Error: Normalized data is empty or not available.");
    return;
  }
  
  # Step 2: Generate design and contrast matrices
  mSetObj <- GenerateContrastMatrix(mSetObj, par1)
  design <- mSetObj$analSet$design
  contrast_matrix <- mSetObj$analSet$contrast.matrix
  
  # Step 3: Fit linear model
  fit <- lmFit(norm_data, design)
  
  # Step 4: Apply contrast matrix
  fit2 <- contrasts.fit(fit, contrast_matrix)
  
  # Step 5: Perform empirical Bayes moderation
  fit2 <- eBayes(fit2)
  
  # Step 6: Extract results
  results <- topTable(fit2, coef = 1, adjust = "fdr", number = Inf)
  
  # Store results in mSetObj
  mSetObj$dataSet$comp.res <- results
  
  return(mSetObj)
}

GenerateContrastMatrix <- function(mSetObj=NA, par1) {
  library(limma)
  
  meta_info <- mSetObj$dataSet$meta.info
  
  # Ensure meta_info has at least one column
  if (ncol(meta_info) < 1) {
    stop("Error: meta_info must have at least one column.")
  }
  
  # Get the first column dynamically
  group_vector <- factor(meta_info[[1]])  # Ensure factor type
  group_name <- colnames(meta_info)[1]  # Store the name of the first column
  
  # Convert levels to valid R variable names
  valid_levels <- make.names(levels(group_vector))  # Convert levels to valid names
  names(valid_levels) <- levels(group_vector)  # Map original levels to valid names
  
  # Create the design matrix (no intercept)
  design <- model.matrix(~ 0 + group_vector)  # `0 +` removes intercept
  
  # Rename columns to match valid level names
  colnames(design) <- valid_levels
  
  # Extract levels from par1 (expected format: "A vs B")
  levels_split <- unlist(strsplit(par1, " vs. "))
  if (length(levels_split) != 2) {
    stop("Error: par1 should be in the format 'A vs B'.")
  }
  
  ref_group <- levels_split[1]  # Reference group (e.g., "0")
  test_group <- levels_split[2]  # Test group (e.g., "1")
  
  # Ensure the levels exist in the dataset
  if (!(ref_group %in% names(valid_levels)) || !(test_group %in% names(valid_levels))) {
    stop("Error: Specified levels not found in the dataset.")
  }
  
  # Generate contrast string using valid names
  contrast_string <- paste0(valid_levels[test_group], " - ", valid_levels[ref_group])
  contrast_matrix <- makeContrasts(contrasts = contrast_string, levels = colnames(design))
  
  # Store in mSetObj
  mSetObj$analSet$contrast.matrix <- contrast_matrix
  mSetObj$analSet$design <- design
  mSetObj$analSet$design.noadj <- design
  mSetObj$analSet$par1 <- par1
  
  return(mSetObj)
}
