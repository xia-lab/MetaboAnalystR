#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerRnwReport_slides<-function(mSetObj, usrName){
  #save.image("biomarker.Rdata");
  CreateHeader(usrName);
  CreateBiomarkerIntr_slides();
  
 # CreateBiomarkerOverview();
 # CreateBiomarkerInputDoc(mSetObj);
  CreateDataProcSlides(mSetObj);
  CreateNORMSlides(mSetObj);
  CreateBiomarkerRatioOverview_slides(mSetObj);

  CreateUnivarBiomarkersDoc_slides(mSetObj);
  CreateMultiBiomarkersDoc_slides(mSetObj);
  CreateModelBiomarkersDoc_slides(mSetObj);
  AddFeatureImages_slides(mSetObj);
  CreateSlideFooter();
}


#'Create biomarker analysis report: Introduction  
#'@description Report generation using Sweave
#'Biomarker analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerIntr_slides <- function() {
    # Initialize counters if they do not exist
    if(!exists("table.count")) {
        table.count <<- 0;
    }
    if(!exists("fig.count")) {
        fig.count <<- 0;
    }
    
    # Introduction to Biomarker Analysis
    intro <- c(
        "## Biomarker Analysis Overview\n\n",
        "- Biomarker analysis aims to construct models with high sensitivity and specificity by selecting biomarkers, algorithms, and parameters.",
        "- The process includes biomarker selection, model tuning, and performance evaluation.",
        "- This module supports univariate analysis, multivariate panel analysis, and manual biomarker model creation.",
        "- It supports algorithms like PLS-DA, SVM, and Random Forests, and utilizes ROC curves, MCCV, and permutation tests for performance assessment.",
        "\n\n---\n\n"    
    );
    .buffer_add(intro, collapse="\n");
}


#'Create biomarker analysis report: Normalization, ratio
#'@description Report generation using Sweave
#'Biomarker analysis, ratio option
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateBiomarkerRatioOverview_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    if (!mSetObj$dataSet$use.ratio) {
        return;
    }

    intro <- c(
        "## Biomarker Ratio Analysis\n\n",
        "- Exploring metabolite ratios can provide insights beyond individual concentrations.",
        "- MetaboAnalyst computes ratios between all possible pairs to identify potential biomarkers.",
        "- **Note**: This process may introduce overfitting due to selection based on p-values. Independent validation is recommended.",
        "- Log normalization is applied during this computation.\n"
    )
    .buffer_add(intro, collapse="\n")
    .buffer_add("\n\n---\n\n")

    if (mSetObj$dataSet$use.ratio) {
        table_rocrr <- table.count <<- table.count + 1
        tableContent <- c(
            "## Top Ranked Metabolite Ratios for Biomarker Analysis\n\n",
            "```{r table_rocratio, echo=FALSE, results='asis', warning=FALSE}", 
            "dt_res <- as.data.frame(CreateRatioTableRMD(mSet))", 
            paste0("create_dt(dt_res, caption = 'Table ", table_rocrr, ". Top ranked metabolite ratios included for biomarker analysis.')"),
            "```", 
            "\n\n---\n\n"
        )
        .buffer_add(tableContent, collapse="\n")
    } else {
        noRatios <- c(
            "## Ratios Computation\n\n",
            "- No ratios between metabolite concentration pairs were computed.\n",
            "\n\n---\n\n"
        )
        .buffer_add(noRatios, collapse="\n")
    }
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
CreateUnivarBiomarkersDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Univariate Biomarker Analysis Introduction
    intro <- c(
        "## Classical Univariate Biomarker Analysis\n\n",
        "- Evaluates the performance of individual biomarkers using ROC curve analysis.",
        "- Highlights the sensitivity and specificity for classifying conditions.",
        "- Note: Performance might appear optimistic due to training and testing on the same data without cross-validation.\n"
    )
    .buffer_add(intro, collapse="\n")
    .buffer_add("\n\n---\n\n")

    # Check if feature ranking matrix exists and create a slide for the univariate analysis table
    if(exists("feat.rank.mat")) {
        CreateTitleTableSlide("dt_res <- feat.rank.mat", "Summary of univariate biomarker analysis.")
    }

    # Check for existence of univariate ROC plots and create slides for each
    if (!is.null(mSetObj$imgSet$roc.univ.plot)) {
        for(i in seq_along(mSetObj$imgSet$roc.univ.name)) {
            ft_name <- mSetObj$imgSet$roc.univ.name[i]
            rocCurveSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.univ.plot[[i]], paste0("ROC Curve Analysis for ", ft_name))
            .buffer_add(rocCurveSlide, collapse="\n")
            boxPlotSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.univ.boxplot[[i]], paste0("Concentration Distribution for ", ft_name))
            .buffer_add(boxPlotSlide, collapse="\n")
        }
    } else {
        # Handle the case where no univariate ROC plots are generated
        .buffer_add("## No Univariate ROC Plots Generated\n\nNo univariate ROC plots were generated for the selected biomarkers.\n\n---\n\n")
    }
}


#'Create biomarker analysis report: Multivariate Biomarker Analysis
#'@description Report generation using Sweave
#'Biomarker analysis report, Multivariate Biomarker Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMultiBiomarkersDoc_slides <- function(mSetObj=NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    if(is.null(mSetObj$analSet$multiROC$auc.vec)){
        return()
    }
    
    # Introduction
    intro <- c(
        "## Multivariate Biomarker Analysis\n\n",
        "\n",
        paste0("- **Selected algorithm:** `" , mSetObj$analSet$exp.method, "`\n"),
        paste0("- **Feature rank method:** `", mSetObj$analSet$rank.method, "`\n"),
        paste0("- **Number of latent variables (PLS-DA only):** `", mSetObj$analSet$exp.lvNum, "`\n"),
        "\n---\n\n"
    )
    .buffer_add(intro, collapse="\n")
    
    # ROC Curve Slide
    rocCurveSlide <- CreateTwoColumnFigureSlide(
        figurePath = mSetObj$imgSet$roc.multi.plot,
        caption = "ROC curves for all or a single biomarker model based on average MCCV performance."
    )
    .buffer_add(rocCurveSlide)
    
    # Predicted Class Probabilities Slide
    classProbSlide <- CreateTwoColumnFigureSlide(
        figurePath = mSetObj$imgSet$roc.prob.plot,
        caption = "Predicted class probabilities for all samples using a selected biomarker model."
    )
    .buffer_add(classProbSlide)
    
    # Predictive Accuracy Slide
    predAccuracySlide <- CreateTwoColumnFigureSlide(
        figurePath = mSetObj$imgSet$roc.pred,
        caption = "Predictive accuracy of biomarker models with an increasing number of features."
    )
    .buffer_add(predAccuracySlide)
    
    # Significant Features Slide
    sigFeaturesSlide <- CreateTwoColumnFigureSlide(
        figurePath = mSetObj$imgSet$roc.imp.plot,
        caption = "Most important features of a selected model ranked by importance."
    )
    .buffer_add(sigFeaturesSlide)
    .buffer_add("\n\n---\n\n")
}


#'Create biomarker analysis report: ROC Curve Based Model Creation and Evaluation
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateModelBiomarkersDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    if(is.null(mSetObj$analSet$ROCtest$auc.vec)){
        return()
    }

    # Slide for Manual Biomarker Model Creation and Evaluation
    intro <- c(
        "## Manual Biomarker Model Creation and Performance Evaluation\n\n",
        "- Allows users to select any combination of features to create biomarker models.\n",
        "- Supports PLS-DA, SVM, or RF algorithms for model creation.\n",
        "- Option to withhold a subset of samples for extra validation.\n",
        "- Features should be chosen based on user's judgement or prior knowledge to avoid overfitting.\n",
        "- A balanced number of samples from both groups is recommended for validation.\n",
        "\n---\n\n"
    )
    .buffer_add(intro, collapse="\n")
    
    # Slide for selected parameters
    paramsSlide <- c(
        "## Selected Parameters for Model Creation\n\n",
        "- **Selected features:** `", paste(mSetObj$dataSet$selected.cmpds, collapse=", "), "`\n",
        "- **Selected algorithm:** `", mSetObj$analSet$tester.method, "`\n",
        "- **Number of latent variables (PLS-DA only):** `", mSetObj$analSet$tester.lvNum, "`\n",
        "\n---\n\n"
    )
    .buffer_add(paramsSlide, collapse="\n")
    
    # Slides for ROC curve, Probability, Accuracy, and Permutation
    roc.test <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.testcurve.plot, "ROC curve for the created biomarker model based upon its average cross-validation performance.")
    .buffer_add(roc.test)
    roc.test2 <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.testprob.plot, "Predicted class probabilities for all samples using the created biomarker model.")
    .buffer_add(roc.test2)
    if(!is.null(mSetObj$imgSet$roc.testpred.plot)){
    roc.test3 <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.testpred.plot, "Predictive accuracy of the created biomarker model.")
    .buffer_add(roc.test3)
    }
    if (!is.null(mSetObj$imgSet$roc.perm.plot)) {
        roc.test4 <- CreateTwoColumnFigureSlide(mSetObj$imgSet$roc.perm.plot, "Results of permutation tests for the user-created biomarker model.")
        .buffer_add(roc.test4)
    }

    # Check for predicted class labels and probabilities for new samples
    if(!is.null(mSetObj$analSet$ROCtest$pred.samples.table)) {
        # Slide for Predicted Class Labels
        predictedLabelsSlide <- c(
            "## Predicted Class Labels with Probabilities for New Samples\n\n",
            "- Provides class predictions and probabilities for new unlabeled samples within the dataset.\n",
            "\n---\n\n"
        )
        .buffer_add(predictedLabelsSlide, collapse="\n")
        # You can add code here to create a table slide for new samples if applicable
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
