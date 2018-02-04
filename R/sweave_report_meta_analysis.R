#'Create report of analyses (Meta-Analysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateMetaAnalysisIntr();
  CreateMetaAnalysisOverview();
  
  CreateMetaAnalysisInputDoc(mSetObj);
  CreateMetaAnalysisNORMdoc(mSetObj);
  CreateMetaAnalysisDEdoc(mSetObj);
  
  CreateMetaAnalysisOutput(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create MetaAnalysis analysis report: Introduction  
#'@description Report generation using Sweave
#'MetaAnalysis analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisIntr<-function(){
  descr <- c("\\section{Background}\n",
             "The combination of multiple independent metabolomics studies investigating the same condition in similar populations, ",
             "which is often termed “horizontal integration”, or “metabolomic meta-analysis”. The aim of metabolomic ",
             "meta-analysis is to leverage the collective power of multiple studies to overcome potential noise, bias, and small ",
             "effect sizes to improve the precision in identifying true patterns within data. Specifically, biomarker ",
             "identification remains a large area of research in metabolomics, and their validation is challenging due to ",
             "inconsistencies in identified biomarkers amongst similar experiments. Performing meta-analysis across similar ",
             "studies will thereby increase the sample size and the power to identify robust and precise biomarkers of disease. ",
             " The aim of the Meta-Analysis module for the integration of individual metabolomic studies to identify consistent and ",
             "robust biomarkers of disease. This module supports three methods for performing meta-analysis: 1) Combining p-values,",
             " 2) Vote counting, and 3) Direct merging of data into a mega-dataset.\n"
             );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create MetaAnalysis analysis report: Overview
#'@description Report generation using Sweave
#'Power analysis report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisOverview <- function(){
  descr <- c("\\section{Meta-Analysis Overview}\n",
             "The Meta-Analysis module consists of six steps: 1) uploading the individual datasets; 2) data processing of each individual dataset,",
             " however it is suggested that the data-processing steps are consistent amongst the studies; 3) differential expression analysis of individual datasets; ",
             "4) data integrity check prior to meta-analysis ; 5) selection of the statistical method for meta-analysis, ",
             "and 6) visualization of results as a Venn diagram to view all possible combinations of shared features between the datasets. \n"
             );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create MetaAnalysis analysis report: Data Input
#'@description Report generation using Sweave
#'Power analysis report, data input documentation. 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The Meta-Analysis module accepts individual datasets which must be prepared by users prior to being uploaded. ",
             "In general, the datasets must have been collected under comparable experimental conditions/share the same hypothesis",
             " or have the same mechanistic underpinnings. At the moment, the module only supports two-group comparisons (ex: control vs disease).",
             "Further, the module accepts either a compound concentration table, spectral binned data, or a peak intensity",
             " table. The format of the data must be specified, identifying whether the samples are in rows or columns, ",
             "or may either be .csv or .txt files.",
             " \n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  descr<-c("\\subsubsection{Sanity Check}\n",
           " Before data analysis, a sanity check is performed to make sure that all of the necessary",
           " information has been collected. The class labels must be present and must contain only two classes.",
           " If the samples are paired, the class label must be from -n/2 to -1 for one group, and 1 to n/2 for the second group",
           " (n is the sample number and must be an even number). Class labels with the same absolute value are assumed to be pairs.",
           " Compound concentration or peak intensity values must all be non-negative numbers.",
           " By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
           " found within the data (see next section).");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);

}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisNORMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr<-c("\\subsubsection{Normalization of Individual Data}\n",
           " Before differential expression analysis, datasets may be normalized using Log2 transformation.",
           " Additionally, users may choose to auto-scale their data."
           );
  cat(descr, file=rnwFile, append=TRUE);
  
  if(exists(norm.msg)){
    norm.desc <- paste(norm.msg);
  }else{
    norm.desc <- " No normalization methods were applied.";
  } 
  cat(norm.desc, file=rnwFile, append=TRUE, sep="\n");
  
  if(mSetObj$dataSet$auto_opt == 1){
    autoscale <- " Autoscaling of data was performed.";
  }else{
    autoscale <- " No data autoscaling was performed.";
  }
  cat(autoscale, file=rnwFile, append=TRUE, sep="\n");
}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisDEdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr<-c("\\subsubsection{Differential Expression Analysis of Individual Data}\n",
           " Before meta-analysis, differential expression analysis using linear models (Limma) may be performed",
           " for exploratory analysis. Here, users must specify the p-value (FDR) cut-off and the fold-change (FC) cutoff."
  );
  cat(descr, file=rnwFile, append=TRUE);
  
  if(exists(mSetObj$dataSet$deparam)){
    de.desc <- paste(mSetObj$dataSet$deparam);
  }else{
    de.desc <- " No differential-expression analysis was performed."
  } 
  cat(de.desc, file=rnwFile, append=TRUE, sep="\n");
  
  if(exists(mSetObj$dataSet$desig)){
    de.sig <- paste(mSetObj$dataSet$desig);
    cat(de.sig, file=rnwFile, append=TRUE, sep="\n");
  }
}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'MetaAnalysis analysis, data normalization documentation. 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalysisOutput <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr<-c("\\subsubsection{Data Integrity Check}\n",
           " Before meta-analysis, one final data integrity check is performed to ensure meta-data are consistent between datasets and",
           " that there are at least more than 25 percent common features between the collective datasets.");
  cat(descr, file=rnwFile, append=TRUE);
  
  if(!is.null(mSetObj$dataSet$studyinfo)){
    studyinfo <- paste("The following is information about your uploaded dataset for meta-analysis:" ,mSetObj$dataSet$studyinfo);
    cat(studyinfo, file=rnwFile, append=TRUE);
  }
  
  descr<-c("\\subsection{Meta-Analysis Output}\n",
           "After the data has passed the final integrity check, users have the option to select one of three methods to perform meta-analysis: ",
           "1) Combining p-values, 2) vote counting, or 3) directly mering the datasets into a mega-dataset.");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr<-c("\\subsection{Combining P-Values}\n",
           "Calculating and combining p-values for the meta-analysis of microarray studies has been a long standing method and now we apply it to metabolomics studies.",
           " It includes two popular approaches, the Fisher's method and the Stouffer's method, which have similar levels of performance",
           " and are generally interpreted as larger scores reflecting greater differential abundance. The main difference between",
           " the two methods are weights (which are based on sample size), which are used in the Stouffer's method but not used in the Fisher's method.", 
           " It should be noted that larger sample sizes do not warrant larger weights, as study quality can be variable. Further, users should ", 
           "use the Stouffer's method only when all studies are of similar quality.");
  cat(descr, file=rnwFile, append=TRUE);

  descr<-c("\\subsection{Vote Counting}\n",
           "Vote counting is considered the most simple", 
           " yet most intuitive method for meta-analysis. Here, significant features are selected based on a selected criteria (i.e. an adjusted p-value", 
           " <0.05 and the same direction of FC) for each dataset. The votes are then calculated for each feature by counting the total of number of times", 
           " a feature is significant across all included datasets. However, this method is statistically inefficient and should be considered the", 
           " last resort in situations where other methods to perform meta-analysis cannot be applied.");
  cat(descr, file=rnwFile, append=TRUE);


  descr<-c("\\subsection{Direct Merging}\n",
           "The final method of meta-analysis is the direct merging of individual data into", 
           " a mega-dataset, which results in an analysis of that mega-dataset as if the individual data were derived from the same experiment. This", 
           " method thereby ignores any inherent bias and heterogeneity between the different data. Because of this, there exists several confounders", 
           " such as different experimental protocols, technical platforms, and raw data processing procedures that can mask true underlying differences.", 
           " It is therefore highly suggested that this approach be used only when individual data are very similar (i.e. from the same lab, same platform,", 
           " without batch effects).");
  cat(descr, file=rnwFile, append=TRUE);

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");

  if(metastat.method=="metap"){
    method <- paste(" P-value combination was the selected method to perform meta-analysis.", 
                    "The method of p-value combination used is: ", mSetObj$dataSet$pvalmethod,
                    "The p-value significance threshold is: ", mSetObj$dataSet$pvalcutoff);
  }else if(metastat.method=="votecount"){
    method <- paste(" Vote counting was the selected method to perform meta-analysis.",
                    "The minimum vote count used is: ", mSetObj$dataSet$vote,
                    "The p-value significance threshold is: ", mSetObj$dataSet$pvalcutoff);
  }else{
    method <- paste(" Direct merging of individual data was the selected method to perform meta-analysis.", 
                    "The p-value significance threshold is: ", mSetObj$dataSet$pvalcutoff);
    
  }
  cat(method, file=rnwFile, append=TRUE, sep="\n");
  
  if(is.null(mSetObj$analSet$meta.mat)){
    return();
  }else{
    MetaAnalysisTable <- c("<<echo=false, results=tex>>=",
                           "CreateMetaTable(mSet)",
                           "@");
    cat(MetaAnalysisTable, file=rnwFile, append=TRUE, sep="\n");
  }
  
  metafeature <- mSetObj$imgSet$meta.anal$feature
  
  if(!is.null(mSetObj$imgSet$meta.anal$plot)){
    featplot <- c( "\\begin{figure}[htp]",
                   "\\begin{center}",
                   paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$meta.anal$plot,"}", sep=""),
                   "\\caption{", paste("Box plot of the expression pattern of the selected feature between the two experimental groups across all studies.", 
                                       " The expression pattern is on the y-axis, and the group labels are on the x-axis.", 
                                       " The median expression for the feature is indicated with a black dot in the centre of the boxplot.", sep=""),"}",
                   "Selected feature: ", metafeature, 
                   "\\end{center}",
                   paste("\\label{",mSetObj$imgSet$meta.anal$plot,"}", sep=""),
                   "\\end{figure}",
                   "\\clearpage"
    );
    cat(featplot, file=rnwFile, append=TRUE, sep="\n");
  }  

  if(!is.null(mSetObj$imgSet$venn)){
    featplot <- c( "\\begin{figure}[htp]",
                   "\\begin{center}",
                   paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$venn,"}", sep=""),
                   "\\caption{", paste("Venn diagram of the top differentially expressed features from the meta-analysis. ", 
                                       "On the left side are features that are DE only from the meta-analysis,",
                                       " in the center are DE features that were identified in both the meta-analysis and the individual studies, ",
                                       "and on the right side are features that are were DE in the individual analysis, but did not show up as DE during meta-analysis.", sep=""),"}",
                   "\\end{center}",
                   paste("\\label{",mSetObj$imgSet$venn,"}", sep=""),
                   "\\end{figure}",
                   "\\clearpage"
    );
    cat(featplot, file=rnwFile, append=TRUE, sep="\n");
  }  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");

  if(!is.null(mSetObj$analSet$sigfeat.matrix)){
    MetaAnalysisVennTable <- c("<<echo=false, results=tex>>=",
                           "CreateVennMetaTable(mSet)",
                           "@");
    cat(MetaAnalysisVennTable, file=rnwFile, append=TRUE, sep="\n");
  }

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");

}

#'Create MetaAnalysis table of results
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  suppressMessages(library(xtable));
  
  metatable <- mSetObj$analSet$meta.mat;
  
  print(xtable(metatable, caption="Predicted top-ranking features from meta-analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create MetaAnalysis table of results for Venn Diagram
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateVennMetaTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  suppressMessages(library(xtable));

  metatable <- mSetObj$analSet$sigfeat.matrix;
  
  print(xtable(metatable, caption="Differentially expressed features by individual study and from meta-analysis"), caption.placement="top", size="\\scriptsize", scalebox = "0.6");
  
}























