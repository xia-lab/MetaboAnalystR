#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateBiomarkerRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateBiomarkerIntr();
  
  CreateBiomarkerOverview();
  CreateBiomarkerInputDoc(mSetObj);
  CreateNORMdoc(mSetObj);
  CreateBiomarkerRatioOverview(mSetObj);
  
  CreateUnivarBiomarkersDoc(mSetObj);
  CreateMultiBiomarkersDoc(mSetObj);
  CreateModelBiomarkersDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}


#'Create biomarker analysis report: Introduction  
#'@description Report generation using Sweave
#'Biomarker analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateBiomarkerIntr<-function(){
  descr <- c("\\section{Background}\n",
             "The metabolome is well known to be a sensitive measure of health and disease, reflecting alterations",
             " to the genome, proteome, and transcriptome, as well as changes in life style and environment. As such,",
             " one common goal of metabolomic studies is biomarker discovery, which aims to identify a metabolite or a set",
             " of metabolites capable of classifying conditions or disease with high sensitivity (true-positive rate) and",
             " specificity (true negative rate). Biomarker discovery is achieved through building predictive models of one or multiple metabolites",
             " and evaluating the performance and robustness of the model to classify new patients into diseased or healthy categories.",
             " The Biomarker analysis module supports all common ROC-curve based biomarker analyses. It includes several options for single biomarker",
             " or biomarker panel analysis, as well as for manual biomarker model creation and evaluation.",
             " For a comprehensive introductory tutorial and further details concerning biomarker analysis, please refer to",
             " \\textbf{Translational biomarker discovery in clinical metabolomics: an introductory tutorial} by Xia et al. 2013 (PMID: 23543913).\n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}


#'Create biomarker analysis report: Overview
#'@description Report generation using Sweave
#'Power analysis report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateBiomarkerOverview <- function(){
  descr <- c("\\section{Biomarker Analysis Overview}\n",
             "The module consists of five steps - uploading the data, data processing,",
             " biomarker selection, performance evaluation, and model creation. There are several options within",
             " MetaboAnalyst to perform each of these steps, supporting all common ROC-curve based biomarker analyses. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create biomarker analysis report: Data Input
#'@description Report generation using Sweave
#'Power analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateBiomarkerInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The biomarker analysis module accepts either a compound concentration table, spectral binned data, or a peak intensity table.",
             " The class label must contain only two groups. Multi-group biomarker analysis is supported. The format of the data must be specified, identifying whether the samples are in rows or columns.",
             " The data may either be .csv or .txt files.",
             " \n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  descr<-c("\\subsubsection{Data Integrity Check}\n",
           " Before data analysis, a data integrity check is performed to make sure that all of the necessary",
           " information has been collected. The class labels must be present and must contain only two groups.",
           " Compound concentration or peak intensity values must all be non-negative numbers.",
           " By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
           " found within the data (see next section).");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  # the data filtering
  descr<-c("\\subsubsection{Data Filtering}\n",
           " The purpose of data filtering is to identify and remove variables that are unlikely to be of",
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

  descr<-c("\\subsubsection{Missing value imputations}\n",
           "Too many zeroes or missing values will cause difficulties in the downstream analysis.",
           " MetaboAnalystR offers several different methods for this purpose. The default method replaces ",
           " all the missing and zero values with a small values (the half of the minimum positive",
           " values in the original data) assuming to be the detection limit. The assumption of this approach",
           " is that most missing values are caused by low abundance metabolites (i.e.below the detection limit).",
           " In addition, since zero values may cause problem for data normalization (i.e. log), they are also ",
           " replaced with this small value. User can also specify other methods, such as replace by mean/median,",
           " or use K-Nearest Neighbours (KNN), Probabilistic PCA (PPCA), Bayesian PCA (BPCA) method, Singular Value Decomposition (SVD)",
           " method to impute the missing values \\footnote{Stacklies W, Redestig H, Scholz M, Walther D, Selbig J.",
           " \\textit{pcaMethods: a bioconductor package, providing PCA methods for incomplete data.}, Bioinformatics",
           " 2007 23(9):1164-1167}. Please select the one that is the most appropriate for your data.");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  if(is.null(mSetObj$msgSet$replace.msg)){
    mSetObj$msgSet$replace.msg <- "No missing value imputation was performed.";
  }
  
  cat(mSetObj$msgSet$replace.msg, file=rnwFile, append=TRUE); 
  cat("\n\n", file=rnwFile, append=TRUE);
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create biomarker analysis report: Normalization, ratio
#'@description Report generation using Sweave
#'Biomarker analysis, ratio option
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateBiomarkerRatioOverview <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\subsection{Biomarker Analysis Normalization}\n",
             "The normalization step for the biomarker analysis module includes an additional option for",
             " calculating ratios between metabolite concentrations. Ratios between two metabolite concentrations",
             " may provide more information than the two metabolite concentrations separately. MetaboAnalystR",
             " will compute ratios between all possible metabolite pairs and then select the top ranked ratios (based on p-values)",
             " to include with the data for further biomarker analysis. Please note, there is a potential overfitting issue", 
             " associated with this procedure. The main purpose of computing ratios of metabolite concentrations is to improve the chances of biomarker discovery,", 
             " therefore users will need to validate their performance in future, independent studies. Log normalization of the data will be performed", 
             " during the process. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
  
  if(exists('ratio', where=mSetObj$analSet)){
    
    ratiotable<-c("<<echo=false, results=tex>>=",
                  "CreateRatioTable(mSet)",
                  "@");
    cat(ratiotable, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  else{
    rationo <- "No ratios between metabolite concentration pairs were computed.";
    cat(rationo, file=rnwFile, append=TRUE);
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table for biomarker analysis: included metabolite ratios
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateRatioTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  ratiodf <- mSetObj$dataSet$ratio
  ldf <- lapply(ratiodf, mean)
  mdf <- do.call(rbind.data.frame, mdf)
  colnames(mdf)[1] <- "Mean concentration ratios across all samples"
  
  print(xtable::xtable(mdf, caption="Top ranked included ratios for biomarker analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create power analysis report: Biomarker Univariate Analysis
#'@description Report generation using Sweave
#'Biomarker analysis report, Univariate Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong 
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateUnivarBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$imgSet$roc.univ.plot)){
    return()
  }
  
  if(is.null(mSetObj$imgSet$roc.univ.boxplot)){
    return()
  }
  
  descr <- c("\\section{Classical ROC curve analysis}\n",
             "The aim of classical ROC curve analysis is to evaluate the performance of a single feature, either",
             " one metabolite or a combined metabolite ratio pair, as a biomarker. The ROC curve summarizes the sensitivity",
             " and specificity of that single feature to accurately classify data, which can then be used to compare",
             " the overall accuracy of different biomarkers. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- paste("Figure", fig.count<<-fig.count+1, "ROC curve of an individual biomarker.")
  
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- paste("Figure", fig.count<<-fig.count+1, "Boxplot of an individual biomarker.")
  
  cat(descr, file=rnwFile, append=TRUE);
  
  ft.name <- paste(mSetObj$imgSet$roc.univ.name);
  
  univbio<-c( "\\begin{figure}[htp]",
              "\\begin{center}",
              paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.univ.plot,"}", sep=""),
              "\\caption{The ROC curve of an individual biomarker.",
              " The sensitivity is on the y-axis, and the specificity is on the", 
              " x-axis. The area-under-the-curve (AUC) is in blue.",
              "Selected individual biomarker name :", ft.name, "}",
              "\\end{center}",
              paste("\\label{",mSetObj$imgSet$roc.univ.plot,"}", sep=""),
              "\\end{figure}",
              "\\clearpage"
  );
  cat(univbio, file=rnwFile, append=TRUE, sep="\n");
  
  ft.name <- paste(mSetObj$imgSet$roc.univ.name);
  
  univbio<-c( "\\begin{figure}[htp]",
              "\\begin{center}",
              paste("\\includegraphics[width=0.35\\textwidth]{", mSetObj$imgSet$roc.univ.boxplot,"}", sep=""),
              "\\caption{Box-plot of the concentrations of the selected",
              " feature between two groups within the dataset.", 
              " A horizontal line is in red indicating the optimal cutoff.",
              "Selected individual biomarker name :", ft.name, "}",
              "\\end{center}",
              paste("\\label{",mSetObj$imgSet$roc.univ.boxplot,"}", sep=""),
              "\\end{figure}",
              "\\clearpage"
  );
  cat(univbio, file=rnwFile, append=TRUE, sep="\n");
  
  if(exists("feat.rank.mat")){
    univROCtable<-c("<<echo=false, results=tex>>=",
                    "CreateUnivROCTable()",
                    "@");
    cat(univROCtable, file=rnwFile, append=TRUE, sep="\n");
  }
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}

#'Create summary table for univariate ROC analysis 
#'@description Report generation using Sweave
#'Function to create a summary table for univariate biomarker analysis
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateUnivROCTable<-function(){
  
  univROCtable <- feat.rank.mat
  
  colnames(univROCtable) <- c("Area-under-the-curve", "T-test", "Log 2 Fold-Change", "Cluster")
  
  print(xtable::xtable(univROCtable, caption="AUC, Log2FC, T-test and K-Means Cluster for univariate biomarker analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create biomarker analysis report: Multivariate Biomarker Analysis
#'@description Report generation using Sweave
#'Biomarker analysis report, Multivariate Biomarker Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMultiBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$analSet$multiROC$auc.vec)){
    return()
  }
  
  descr <- c("\\section{Multivariate ROC curve exploration}\n",
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
             paste("Figure", fig.count<<-fig.count+1, ". shows the ROC curves of all or a single biomarker model based on the average cross validation performance."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the predicted class probabilities of all samples using a selected biomarker model."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the predictive accuracy of biomarker models with an increasing number of features."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the significant features of single biomarker model ranked by importance."),
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # ROC plot
  
  modelindex <- paste(mSetObj$imgSet$roc.multi.model)
  
  ROCplot <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.multi.plot,"}", sep=""),
                "\\caption{", paste("Plot of ROC curves for all or a single biomarker model based on its average performance",
                                    " across all MCCV runs. For a single biomarker, the 95 percent confidence interval",
                                    " can be computed and will appear as a band around the ROC curve.", sep=""),"}",
                "Selected model :", modelindex, 
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$roc.multi.plot,"}", sep=""),
                "\\end{figure}",
                "\\clearpage"
  );
  cat(ROCplot, file=rnwFile, append=TRUE, sep="\n");
  
  # Prob
  
  modelindex2 <- paste(mSetObj$imgSet$roc.prob.name)
  
  ROCplot <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.prob.plot,"}", sep=""),
                "\\caption{", paste("Plot of predicted class probabilities for all samples using a single biomarker model.",
                                    " Due to balanced subsampling, the classification boundary is at the center (x=0.5, dotted line).", sep=""),"}",
                "Selected model :", modelindex2, 
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$roc.prob.plot,"}", sep=""),
                "\\end{figure}",
                "\\clearpage"
  );
  cat(ROCplot, file=rnwFile, append=TRUE, sep="\n");
  
  # Pred
  
  ROCplot <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.pred,"}", sep=""),
                "\\caption{", paste("Plot of the predictive accuracy of biomarker models with an increasing number of features.", 
                                    " The most accurate biomarker model will be highlighted with a red dot.", sep=""),"}",
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$roc.pred,"}", sep=""),
                "\\end{figure}",
                "\\clearpage"
  );
  cat(ROCplot, file=rnwFile, append=TRUE, sep="\n");
  
  # Sig features
  
  modelindex3 <- paste(mSetObj$imgSet$roc.imp.name)
  
  ROCplot <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.imp.plot,"}", sep=""),
                "\\caption{", paste("Plot of the most important features of a selected model ranked from most to least important.",
                                    " Selected model :", modelindex3, sep=""), "}", 
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$roc.imp.plot,"}", sep=""),
                "\\end{figure}",
                "\\clearpage"
  );
  cat(ROCplot, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}

#'Create biomarker analysis report: ROC Curve Based Model Creation and Evaluation
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateModelBiomarkersDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$analSet$ROCtest$auc.vec)){
    return()
  } 
  
  descr <- c("\\section{ROC Curve Based Model Creation and Evaluation}\n",
             "The aim of ROC curve based model creation and evaluation is to allow users to manually select any combination of features to create biomarker", 
             " models using any of the three algorithms mentioned previously (PLS-DA, SVM, or RF). The user also has the option to withhold a subset of samples", 
             " for extra validation purposes. Additionally, it allows a user to predict the class labels of new samples (unlabeled samples within the imported dataset).", 
             " Features should be selected based on the user's own judgement or prior knowledge (not from the current data). Note, selection of features based on overall ranks", 
             " (AUC, t-statistic, or fold-change) from current data increases the risk of overfitting. These features may be the best biomarkers for a user's own data,", 
             " but not for new samples. Additionally, in order to get a decent ROC curve for validation, it is recommended that the hold-out data contains a balanced number", 
             " of samples from both groups and that it contain at least 8 hold-out samples (i.e. 4 from each group).", 
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1, ". shows the ROC curve of the created biomarker model based upon its average cross validation performance."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the predicted class probabilities of all samples using the user-created classifier."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the predictive accuracy of the user-created biomarker model."),
             paste("Figure", fig.count<<-fig.count+1, ". shows the results of the permutation tests for the user-created biomarker model."),
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # ROC plot
  
  modelindex <- paste(mSetObj$imgSet$roc.testcurve.name)
  modelmethod <- paste(mSetObj$imgSet$roc.testcurve.method)
  
  ROCplot <- c( "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.testcurve.plot,"}", sep=""),
                "\\caption{", paste("Plot of the ROC curve for the created biomarker model based upon its average performance",
                                    " across all MCCV runs. The 95 percent confidence interval can be computed.", sep=""), "}",
                "Selected model :", modelindex, 
                "Selected method :", modelmethod, 
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$roc.testcurve.plot,"}", sep=""),
                "\\end{figure}",
                "\\clearpage"
  );
  cat(ROCplot, file=rnwFile, append=TRUE, sep="\n");
  
  # Probability
  
  modelindex2 <- paste(mSetObj$imgSet$roc.testprob.name)
  
  probplot <- c( "\\begin{figure}[htp]",
                 "\\begin{center}",
                 paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.testprob.plot,"}", sep=""),
                 "\\caption{", paste("Plot of the predicted class probabilities for all samples using the created biomarker model.",
                                     " Due to balanced subsampling, the classification boundary is at the center (x=0.5, dotted line)." , sep=""),"}",
                 "Selected model :", modelindex2, 
                 "\\end{center}",
                 paste("\\label{", mSetObj$imgSet$roc.testprob.plot,"}", sep=""),
                 "\\end{figure}",
                 "\\clearpage"
  );
  cat(probplot, file=rnwFile, append=TRUE, sep="\n");
  
  # Accuracy
  
  acc.plot <- c( "\\begin{figure}[htp]",
                 "\\begin{center}",
                 paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.testpred,"}", sep=""),
                 "\\caption{", paste("Box plot of the predictive accuracy of the created biomarker model.", sep=""),"}",
                 "\\end{center}",
                 paste("\\label{", mSetObj$imgSet$roc.testpred,"}", sep=""),
                 "\\end{figure}",
                 "\\clearpage"
  );
  cat(acc.plot, file=rnwFile, append=TRUE, sep="\n");
  
  # Permutation
  
  if(is.null(mSetObj$imgSet$roc.perm.method)){
    return()
  }else{
    permmethod <- paste(mSetObj$imgSet$roc.perm.method)
    
    permplot <- c( "\\begin{figure}[htp]",
                   "\\begin{center}",
                   paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$roc.perm.plot,"}", sep=""),
                   "\\caption{", paste("Plot of the permutations tests using the area under the ROC curve or the predictive accuracy",
                                       " of the model as a measure of performance. The plot shows the AUC of all permutations, highlighting", 
                                       " the actual observed AUC in blue, along with showing the empirical p-value.", sep=""),"}",
                   "Selected permutation method :", permmethod, 
                   "\\end{center}",
                   paste("\\label{",mSetObj$imgSet$roc.perm.plot,"}", sep=""),
                   "\\end{figure}",
                   "\\clearpage"
    );
    cat(permplot, file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(is.null(mSetObj$analSet$ROCtest$pred.samples.table)){
    return()
  }else{
    ROCLabelstable <- c("<<echo=false, results=tex>>=",
                        "CreateROCLabelsTable(mSet)",
                        "@");
    cat(ROCLabelstable, file=rnwFile, append=TRUE, sep="\n");
    
  }
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
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
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateROCLabelsTable<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  predlabeltable <- mSetObj$analSet$ROCtest$pred.samples.table;
  print(xtable::xtable(predlabeltable, caption="Predicted class labels with probabilities for new samples"), caption.placement="top", size="\\scriptsize");
  
}
