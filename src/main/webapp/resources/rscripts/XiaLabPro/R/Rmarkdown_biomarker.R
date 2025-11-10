
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

#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateBiomarkerIntr();
  
 # CreateBiomarkerOverview();
 # CreateBiomarkerInputDoc(mSetObj);
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  CreateBiomarkerRatioOverview(mSetObj);

  CreateUnivarBiomarkersDoc(mSetObj);
  CreateMultiBiomarkersDoc(mSetObj);
  CreateModelBiomarkersDoc(mSetObj);
  
  CreateRHistAppendix();
  AddFeatureImages(mSetObj);
  CreateFooter();
}


#'Create biomarker analysis report: Introduction  
#'@description Report generation using Sweave
#'Biomarker analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerIntr<-function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "The metabolome is known to be sensitive to individuals' development, physiology, life style and environmental exposure.
             Therefore, it has tremendous potential to identify biomarkers for diagnosis, prognosis, and monitoring of disease or exposures.
             Biomarker analysis aims to build predictive models capable of classifying conditions with high sensitivity 
             (true-positive rate) and specificity (true negative rate). A biomarker model consists of three key elements 
             - <u>biomarkers</u>, <u>algorithm</u>, and <u>parameters</u>. In general, the main activities in biomarker analysis involve 
             selecting biomarkers and tuning parameters based on a few well-established classification algorithms. 
             Evaluating the performance of biomarker models is based on their capacities to classify new samples using cross validation (CV). In addition,
             permutation tests are often used to evaluate whether a classifier has learned anything better than random guessing (null model).",
             "\n\n",
             "The Biomarker analysis module provides comprehensive support for common biomarker analyses, including 
             single (univariate) biomarker analysis, biomarker panel (multivariate) analysis, as well as manual biomarker model creation and evaluation.
             In particular, it offers three well-known algorithms (PLS-DA, SVM, RandomForest) coupled with robust performance measures - ROC curve analysis, Monte-Carlo cross
             validation (MCCV) through balanced subsampling (Explorer), and permutation tests for model evaluation (Tester). 
             Please refer to <a href='https://link.springer.com/article/10.1007/s11306-012-0482-9' target='_blank'> Xia et al. </a> for a comprehensive tutorial.",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE);
}


#'Create biomarker analysis report: Normalization, ratio
#'@description Report generation using Sweave
#'Biomarker analysis, ratio option
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerRatioOverview <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("#### - Computing compound ratios to increase biomarker discovery\n\n",
             "Ratios between two metabolite concentrations may provide more information than the two metabolite concentrations separately. 
             To improve the chance of biomarker discovery, MetaboAnalyst will compute ratios between all possible metabolite pairs 
             and then select the top ranked ratios (based on p-values) to include with the data for further biomarker analysis. Please note, 
             <u>there is a potential overfitting issue associated with this procedure</u> since the  p-value is used for selection. Therefore users will need to validate their performance 
             in future, independent studies. Log normalization of the data will be performed during the process.",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  if(!is.null(mSetObj$dataSet$use.ratio) && mSetObj$dataSet$use.ratio){

    table_rocrr <- table.count <<- table.count + 1;
    
    descr <- c(
      "```{r table_rocratio, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}", 
      "dt_res <- as.data.frame(CreateRatioTableRMD(mSet));", 
      paste0("create_dt(dt_res, 'Table ", 
             table_rocrr, 
             ". Top ranked metabolite ratios included for biomarker analysis.')"),
      "```", 
      "\n\n"
    )

    cat(descr, file=rmdFile, append=TRUE, sep="\n");
    
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  else{
    rationo <- "No ratios between metabolite concentration pairs were computed.";
    cat(rationo, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  cat("<hr/>","## 3. Biomarker Analysis\n\n", file=rmdFile, append=TRUE);
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table for biomarker analysis: included metabolite ratios
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateRatioTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  ratiodf <- as.data.frame(mSetObj$dataSet$ratio)
  ldf <- lapply(ratiodf, mean)
  mdf <- do.call(rbind.data.frame, ldf)
  colnames(mdf)[1] <- "Mean concentration ratios across all samples"
  mdf[,1] <- round(mdf[,1],4)
  rownames(mdf) <- colnames(ratiodf)
  return(mdf)
  #print(xtable::xtable(mdf, caption="Top ranked included ratios for biomarker analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create power analysis report: Biomarker Univariate Analysis
#'@description Report generation using Sweave
#'Biomarker analysis report, Univariate Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateUnivarBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("### - Classical Univariate Biomarker Analysis\n\n",
             "The aim of classical univariate ROC curve analysis is to evaluate the performance of a single feature, either
             one metabolite or a combined metabolite ratio pair, as a biomarker. The ROC curve summarizes the sensitivity
             and specificity of that single feature to accurately classify data, which can then be used to compare 
             the overall accuracy of different biomarkers. Note the performance of individual biomarker
             reported here can be better than multiple biomarker models. This is because the classical univariate ROC curve is generated based on 
             the data itself (training and testing on the same data), not using cross validation (CV) as done in generating the multivariate ROC curve. 
             The performance is too optimistic (i.e. overfitting).",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE);
  
  if(exists("feat.rank.mat")){
    
    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "uniroc_table", "", "");
        
    cat(paste(reportLinks, "\n\n"), file=rmdFile, append=TRUE);

    colnames(feat.rank.mat) <- c("Area-under-the-curve", "T-test", "Log 2 Fold-Change", "Cluster")
    # AUC, Log2FC, T-test and K-Means Cluster for univariate biomarker analysis
    table_uniroc <- table.count <<- table.count + 1;
    descr <- c(
      "```{r table_uniroc, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}", # Start of R chunk with options
      "dt_res <- as.data.frame(feat.rank.mat);", # Convert to data frame
      paste0("create_dt(dt_res, 'Table ", 
             table_uniroc, # Ensure this variable is defined and holds the intended value
             ". AUC, Log2FC, T-test and K-Means Cluster for univariate biomarker analysis.')"),
      "```", # End of R chunk
      "\n\n" # Two line breaks
    )
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n");
  }

  if(is.null(mSetObj$imgSet$roc.univ.plot)){
      cat( "No unvariate ROC plot was generated.", file=rmdFile, append=TRUE);
      return();
  }

  ftnames <- mSetObj$imgSet$roc.univ.name;
  
  for(i in 1:length(ftnames)){
    
    ft.name <- mSetObj$imgSet$roc.univ.name[i];
    
    cat('<div style="display: flex; justify-content: space-around;">', file=rmdFile, append=TRUE)
    # roc_univ_+featNm
    link <- GetSharingLink(mSetObj)
    
    reportLinks <- getReportLinks(link, "roc_unic", paste0("roc_univ_",ft.name), ft.name)
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    reportLinks <- getReportLinks(link, "roc_boxplot", paste0("roc_boxplot_",ft.name), ft.name)

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    cat('</div>', file=rmdFile, append=TRUE)


    cat('<div style="display: flex; justify-content: space-around;">', file=rmdFile, append=TRUE)
    cat('\n\n', file=rmdFile, append=TRUE)

    # First figure (ROC curve)
    cat(paste0("```{r figure_rocu2", i, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
               ". The ROC curve of an individual biomarker...', ",
               "out.height='400px', out.width='", getFigWidth(mSetObj, "auto", "400px"), "'}\n"),
        "safeIncludeGraphics(mSetObj$imgSet$roc.univ.plot[",i,"])",
        "\n```\n",
        file=rmdFile, append=TRUE)
    cat('\n\n', file=rmdFile, append=TRUE)

    # Second figure (Box plot)
    cat(paste0("```{r figure_boxu2", i, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
               ". Box-plot of the concentrations of the selected feature...', ",
               "out.height='400px', out.width='", getFigWidth(mSetObj, "auto", "400px"), "'}\n"),
        "safeIncludeGraphics(mSetObj$imgSet$roc.univ.boxplot[",i,"])",
        "\n```\n",
        file=rmdFile, append=TRUE)
    cat('\n\n', file=rmdFile, append=TRUE)

    # Close the HTML container
    cat('</div>', file=rmdFile, append=TRUE)
  }
}


#'Create biomarker analysis report: Multivariate Biomarker Analysis
#'@description Report generation using Sweave
#'Biomarker analysis report, Multivariate Biomarker Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMultiBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$analSet$multiROC$auc.vec)){
    return()
  }
  
  descr <- c("\n\n### - Multivariate Biomarker Analysis\n\n",
             "The aim of the multivariate exploratory ROC curve analysis is to evaluate the performance of biomarker models created",
             " through automated feature selection. MetaboAnalyst currently supports three multivariate algorithms:", 
             " partial least squares discriminant analysis (PLS-DA), random forests (RF), and support vector machines (SVM). To begin,",  
             " ROC curves are generated by Monte-Carlo cross validation (MCCV) using balanced subsampling. In each MCCV, 2/3 of the samples", 
             " are used to evaluate feature importance, and the remaining 1/3 are used to validate the models created in the first step.", 
             " The top ranking features (max top 100) in terms of importance are used to build the classification models. The process is repeated",
             " several times to calculate the performance and confidence intervals of each model. Users",
             " must specify the classification method and the feature ranking method for ROC curve analysis. For large datasets,", 
             " with more than 1000 features, the univariate feature ranking method is recommended to avoid long computation times. For",
             " the PLS-DA method, users have the option to specify the number of latent variables (LV) to use (default top 2 LVs).", 
             " In the plots below, users have selected to create plots for all biomarker models, or a single biomarker model. The plot description",
             " will indicate the model selected. If it is 0, it means the plot is for all biomarker models. A -1 means it used the best model, and an",
             " input 1-6 to plot a ROC curve for one of the top six models.",
             "\n\n",
             paste("Figure", fig_ral <- fig.count <<- fig.count+1, ". shows the ROC curves of all or a single biomarker model based on the average cross validation performance."),
             paste("Figure", fig_pcp <- fig.count <<- fig.count+1, ". shows the predicted class probabilities of all samples using a selected biomarker model."),
             paste("Figure", fig_pab <- fig.count <<- fig.count+1, ". shows the predictive accuracy of biomarker models with an increasing number of features."),
             paste("Figure", fig_smi <- fig.count <<- fig.count+1, ". shows the significant features of single biomarker model ranked by importance."),
             "\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
   descr <- c("* Selected algorithm:",
                paste0("`",mSetObj$analSet$exp.method,"`"),
                "* Feature rank method:",
                paste0("`",mSetObj$analSet$rank.method,"`"),
                "* Number of latent variables (PLS-DA only):",
                paste0("`",mSetObj$analSet$exp.lvNum,"`"),
                "\n\n");
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n");

  # ROC plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_roc", "cls_roc")

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  modelindex <- paste(mSetObj$imgSet$roc.multi.model);
  
  fig1 <- c(paste0("```{r figure_ral, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ral, 
                  ". ROC curves for all or a single biomarker model based on its average performance ",
                  "across all MCCV runs. For a single biomarker, the 95 percent confidence interval. Selected model :", modelindex, "', ",
                  " fig.lp='", 
                  mSetObj$imgSet$roc.multi.plot, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$roc.multi.plot)",
           "```",
           "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

  # Prob
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_prob", "cls_prob")

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  modelindex2 <- paste(mSetObj$imgSet$roc.prob.name);
  
  fig2 <- c(paste0("```{r figure_pcp, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pcp, 
                   ". Plot of predicted class probabilities for all samples using a single biomarker model. ",
                   "Due to balanced subsampling, the classification boundary is at the center (x=0.5, dotted line). Selected model :", modelindex2, "', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.prob.plot, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.prob.plot)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # Pred
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_accu", "cls_accu")
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_pab, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_pab, 
                   ". Plot of the predictive accuracy of biomarker models with an increasing number of features. ",
                   " The most accurate biomarker model will be highlighted with a red dot. ', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.pred, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.pred)",
            "```",
            "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # Sig features
if (!is.null(mSetObj$imgSet$roc.imp.plot) && safeFileExists(mSetObj$imgSet$roc.imp.plot)) {
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_imp", "cls_imp")
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  modelindex3 <- paste(mSetObj$imgSet$roc.imp.name);
  
  fig4 <- c(paste0("```{r figure_smi, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_smi, 
                   ". The most important features of a selected model ranked from most to least important. ",
                   " Selected model :", modelindex3, ". ', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.imp.plot, 
                   "', out.width = '", getFigWidth(mSetObj), "'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.imp.plot)",
            "```",
            "\n\n");
  cat(fig4, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create biomarker analysis report: ROC Curve Based Model Creation and Evaluation
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateModelBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$analSet$ROCtest$auc.vec)){
    return()
  } 
  
  descr <- c("### - Manual Biomarker Model Creation and Performance Evaluation\n\n",
             "The aim of ROC curve based model creation and evaluation is to allow users to manually select any combination of features to create biomarker", 
             " models using any of the three algorithms mentioned previously (PLS-DA, SVM, or RF). The user also has the option to withhold a subset of samples", 
             " for extra validation purposes. Additionally, it allows a user to predict the class labels of new samples (unlabeled samples within the imported dataset).", 
             " Features should be selected based on the user's own judgement or prior knowledge (not from the current data). Note, selection of features based on overall ranks", 
             " (AUC, t-statistic, or fold-change) from current data increases the risk of overfitting. These features may be the best biomarkers for a user's own data,", 
             " but not for new samples. Additionally, in order to get a decent ROC curve for validation, it is recommended that the hold-out data contains a balanced number", 
             " of samples from both groups and that it contain at least 8 hold-out samples (i.e. 4 from each group).", 
             "\n\n",
             paste("Figure", fig_mriv <- fig.count<<-fig.count+1, ". shows the ROC curve of the created biomarker model based upon its average cross validation performance."),
             paste("Figure", fig_mppc <- fig.count<<-fig.count+1, ". shows the predicted class probabilities of all samples using the user-created classifier."),
             paste("Figure", fig_mpab <- fig.count<<-fig.count+1, ". shows the predictive accuracy of the user-created biomarker model."),
             paste("Figure", fig_mptm <- fig.count<<-fig.count+1, ". shows the results of the permutation tests for the user-created biomarker model."),
             "\n\n");
  
    cat(descr, file=rmdFile, append=TRUE);
  
    # selected parameters 
    descr <- c("* Selected features:",
                paste0("[```",mSetObj$dataSet$selected.cmpds,"```]"),
                "\n",
                "* Selected algorithm:",
                paste0("```",mSetObj$analSet$tester.method,"```"),
                "\n",
                "* Number of latent variables (PLS-DA only):",
                paste0("```",mSetObj$analSet$tester.lvNum,"```"),
                "\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n");
    
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

  # ROC plot
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_test_roc", "cls_test_roc")

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  modelindex <- paste(mSetObj$imgSet$roc.testcurve.name)
  modelmethod <- paste(mSetObj$imgSet$roc.testcurve.method)
  
  fig1 <- c(paste0("```{r figure_mriv, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mriv, 
                   ". Plot of the ROC curve for the created biomarker model based upon its average performance ",
                   "across all MCCV runs. The 95 percent confidence interval can be computed. Selected model :", modelindex, ". Selected method :", modelmethod, "', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.testcurve.plot, 
                   "', out.width = '", getFigWidth(mSetObj, width="780px"),"'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.testcurve.plot)",
            "```",
            "\n\n");
  cat(fig1, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");

  # Probability
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_test_prob", "cls_test_prob")

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  modelindex2 <- paste(mSetObj$imgSet$roc.testprob.name)
  
  fig2 <- c(paste0("```{r figure_mppc, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mppc, 
                   ". Plot of the predicted class probabilities for all samples using the created biomarker model. ",
                   "Due to balanced subsampling, the classification boundary is at the center (x=0.5, dotted line). Selected model :", modelindex2, "', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.testprob.plot, 
                   "', out.width = '",getFigWidth(mSetObj, width="780px"),"'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.testprob.plot)",
            "```",
            "\n\n");
  cat(fig2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # Accuracy
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "cls_test_accu", "cls_test_accu")

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig3 <- c(paste0("```{r figure_mpab, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mpab, 
                   ". Box plot of the predictive accuracy of the created biomarker model. ', ",
                   " fig.lp='", 
                   mSetObj$imgSet$roc.testpred, 
                   "', out.width = '",getFigWidth(mSetObj, width="780px"),"'}"),
            "safeIncludeGraphics(mSetObj$imgSet$roc.testpred)",
            "```",
            "\n\n");
  cat(fig3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  

  # Permutation
  
  if(is.null(mSetObj$imgSet$roc.perm.method)){
    return()
  }else{
    permmethod <- paste(mSetObj$imgSet$roc.perm.method)
    
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "roc_perm", "roc_perm")

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig4 <- c(paste0("```{r figure_mptm, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mptm, 
                     ". Plot of the permutations tests using the area under the ROC curve or the predictive accuracy", 
                     " of the model as a measure of performance. The plot shows the AUC of all permutations, highlighting",
                     " the actual observed AUC in blue, along with showing the empirical p-value. ",
                     "Selected permutation method :", permmethod, 
                     " ', ",
                     " fig.lp='", 
                     mSetObj$imgSet$roc.perm.plot, 
                     "', out.width = '", getFigWidth(mSetObj, width="780px"),"'}"),
              "safeIncludeGraphics(mSetObj$imgSet$roc.perm.plot)",
              "```",
              "\n\n");
    cat(fig4, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  if(is.null(mSetObj$analSet$ROCtest$pred.samples.table)){
    return()
  }else{

    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "roc_new_samples")

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    table_roclb <- table.count <<- table.count + 1;

    descr <- c(
      "```{r table_roclbb, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}", # Start of R chunk with options
      "dt_res <- as.data.frame(CreateROCLabelsTableRMD(mSet));", # Convert to data frame
      paste0("create_dt(dt_res, 'Table ", 
             table_roclb, # Ensure this variable is defined and holds the intended value
             ". Predicted class labels with probabilities for new samples.')"),
      "```", # End of R chunk
      "\n\n" # Two line breaks
    )
    cat(descr, file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create a table of newly classified samples
#'@description Function to create the table of newly classified samples
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'Function to create the table of newly classified samples
#'@export
#'@importFrom tibble remove_rownames column_to_rownames
ROCPredSamplesTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  groups <- as.data.frame(GetNewSampleGrps(mSetObj));
  prob <- as.data.frame(GetNewSampleProbs(mSetObj));
  predtable <- merge(prob, groups, by="row.names");
  pred.samples.table <- predtable %>% remove_rownames %>% column_to_rownames(var="Row.names");
  colnames(pred.samples.table) <- c("Probability", "Class Label");
  
  mSetObj$analSet$ROCtest$pred.samples.table <- pred.samples.table;
  .set.mSet(mSetObj);
}


#'Create a x-table for newly classified samples
#'@description Report generation using Sweave
#'Function to create a table for newly classified samples
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author zhiqiang pang
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateROCLabelsTableRMD<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ROCtest$pred.samples.table);
}
