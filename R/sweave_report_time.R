#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create timeseries .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateTimeSeriesRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateTimeSeriesIOdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  InitTimeSeriesAnal();
  if(exists("analSet", where=mSetObj)){
    CreateMetaOverview(mSetObj);
    CreateiPCAdoc(mSetObj);
    CreateCorHeatmap(mSetObj);

    CreateCovAdj(mSetObj);
    CreateCorAnalysis(mSetObj);
    CreateAOV2doc(mSetObj);

    CreateASCAdoc(mSetObj);
    CreateMBdoc(mSetObj);

    CreateRandomForest(mSetObj);
  }else{
    CreateTimeSeriesAnalNullMsg();
  }
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis, time-series
#'Read and process the raw data
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateTimeSeriesIOdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  descr <- c("\\section{Data Upload and Integrity Checking}\n");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- c("\\subsection{Upload your data}\n",
             "For statistical analysis involving complex metadata, MetaboAnalyst accepts data table and \\textbf{metadata table} uploaded as two comma separated values (.csv) files.",
             "Samples can be in rows or columns for data file. The metadata table must have the same sample names. ",
             "For time-series data, the time points group must be named as \\textbf{Time} and \\textbf{Subject}.",
             "Users need to specify the data types when uploading their data in order for MetaboAnalyst to select the correct algorithm to process them.\n",
             paste("Table", table.count<<-table.count+1,"summarizes the result of the data checking steps.\n")
  );
  cat(descr, file=rnwFile, append=TRUE);
  
  # error checking
  if(is.null(mSetObj$dataSet$url.var.nms) | 
     is.null(mSetObj$dataSet$proc.feat.num) | 
     is.null(mSetObj$dataSet$facA) | 
     is.null(mSetObj$dataSet$facB)){
    errorMsg<- c(descr, "Error occured during reading the raw data ....",
                 "Failed to proceed. Please check if the data format you uploaded is correct.",
                 "Please use the OmicsForum (omicsforum.ca) for community based support!\n");
    cat(errorMsg, file=rnwFile, append=TRUE);
    return();
  }
  
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr <- paste(mSetObj$msgSet$read.msg, collapse="\n");
  cat(paste("\\texttt{", PrepareLatex(descr), "}"), file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "CreateSummaryTable(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
  # the next step is sanity check
  descr <- c("\\subsection{Data Integrity Check}\n",
             "Before data analysis, a data integrity check is performed to make sure that all the necessary",
             "information has been collected.",
             "Compound concentration or peak intensity values should all be non-negative numbers.",
             "By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
             "found within the data (detection limits).\n\n");
  cat(descr, file=rnwFile, append=TRUE);
  
  msgLen <- length(mSetObj$msgSet$check.msg);
  descr <- paste(mSetObj$msgSet$check.msg[1:(msgLen-2)], collapse="\n");
  cat(paste("\\texttt{", PrepareLatex(descr), "}"), file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis, time-series analysis
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
InitTimeSeriesAnal <- function(){
  descr <- c("\\section{Statistical Analysis [metadata table]}",
             "For metabolomics data accompanied by complex metadata, MetaboAnalyst offers several carefully selected methods for",
             " data analysis. They include:\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr2 <- c(
    "\\begin{itemize}",
    
    "\\item{Data and metadata overview: }",
    "\\begin{itemize}",
    "\\item{Metadata Visualization }",
    "\\item{Interactive Principal Component Analysis (iPCA) }",
    "\\item{Hierarchical Clustering and Heatmap Visualization }",
    "\\end{itemize}",
    
    "\\item{Univariate analysis: }",
    "\\begin{itemize}",
    "\\item{Linear Models with Covariate Adjustment }",
    "\\item{Correlation and Partial Correlation Analysis }",
    "\\item{Two-way ANOVA (ANOVA2) }",
    "\\end{itemize}",
    
    "\\item{Multivariate analysis: }",
    "\\begin{itemize}",
    "\\item{ANOVA-Simultaneous Component Analysis (ASCA) }",
    "\\item{Multivariate Empirical Bayes Analysis (MEBA) }",
    "\\end{itemize}",

    "\\item{Supervised classification: }",
    "\\begin{itemize}",
    "\\item{Random Forest }",
    "\\end{itemize}",
    
    "\\end{itemize}",
    
    "\\texttt{Please note: MEBA is only applicable to time-series data analyais.}",
    "\\clearpage"
  );
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
}

#'Create null analysis message for time-series sweave report
#'@description Creates empty time-series analysis message
#'@export
CreateTimeSeriesAnalNullMsg<-function(){
  descr <- c("No analysis was performed on your data.\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

##### DATA AND METADATA OVERVIEW SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'For Metadata Overview
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateMetaOverview <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$imgSet$metahtmaptwo)){
    return();
  }
  
  descr <- c("\\subsection{Data and Metadata Overview}\n",
             "Understanding the correlation structure of the metadata is important for model",
             "definition and interpretation. The metadata heatmap and correlation heatmap allow",
             "for visual identification of relationships between metadata.",
             "By default, all metadata are included in the visualizations.",
             "This section supports multiple methods for calculating distance, correlation, and clustering,",
             "and the heatmap can be viewed in either overview or detail mode.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$meta.corhm,"}", sep=""),
    "\\caption{Correlation heatmap displaying relationship between metadata.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$meta.corhm,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$metahtmaptwo,"}", sep=""),
    "\\caption{Metadata heatmap displaying relationship between metadata.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$metahtmaptwo,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");


  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'For Interactive PCA
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateiPCAdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$ipca)){
    return();
  }
  
  descr <- c("\\subsection{Interactive Principal Component Analysis (iPCA)}\n",
             "PCA is an unsupervised method that aims to find the directions that best",
             "explain the variance in a data set (X) without referring to class labels (Y).",
             "The data are summarized into the top three components or PCs that explain most of the",
             "variations in the data. The result is displayed in an interactive 3D",
             "visualization system. The system supports pointing-and-clicking, rotating",
             "zooming(hold down SHIFT key and drag vertically). Clicking any of the displayed",
             "data points will show the corresponding sample summarized by the top 20 most",
             "different variables that deviate from the data center.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);

  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.pair,"}", sep=""),
    "\\caption{Pairwise PCA with density outlines showing group membership.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.pair,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'For Correlation Heatmap
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateCorHeatmap <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$htmap2)){
    return();
  }
  
  descr <- c("\\subsection{Two-way Heatmap Visualization}\n",
             "The heatmap provides direct visualization of all data points in the form",
             "of colors squares. The color spectrum intuitively indicates the higher or lower values.",
             "Users can choose different clustering algorithms or distance measures to cluster the",
             "variables. The samples are ordered by the two factors with default the first factor",
             "used for primary ordering. Users can choose to switch the order.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);

  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$htmaptwo,"}", sep=""),
    "\\caption{Metadata heatmap displaying relationship between metabolites and metadata.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$htmaptwo,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

##### UNIVARIATE ANALYSIS SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Covariate Adjustment 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export

CreateCovAdj <- function(mSetObj=NA){ ## need to figure out the image still
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$cov)){
    return();
  }
  
  descr <- c("\\subsection{Linear models with covariate adjustments}\n",
             "Including metadata in the linear model adjusts for variability associated with them while ",
             "performing statistical tests for the variable of interest. Users have the option of which metadata to include ",
             "and whether to include them as fixed (normal) or random (blocking factor) effects. It is important to investigate ",
             "correlation between metadata prior to defining the model as including variables that are highly correlated ",
             "can lead to model parameter instability. ",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);


  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$covAdj,"}", sep=""),
    "\\caption{P-values for metabolites with and without covariate adjustment.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$covAdj,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}


#'Create report of analyses 
#'@description Report generation using Sweave
#'Correlation and Partial Correlation Analysis 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateCorAnalysis <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$corr$cor.mat)){
    return();
  }
  
  descr <- c("\\subsection{Correlation and Partial Correlation Analysis}\n",
             "Correlation analysis can be performed for a given feature and metadata of metadata of interest.",
             "When the covariate is 'none' (default), regular correlation analysis will be performed;",
             "otherwise, partial correlation will be performed.",
             "For binary metadata, the point biserial correlation will be used;",
             "for continuous metadata, users can choose Pearson/Spearman/Kendall correlation.",
             "\n\n");  
  cat(descr, file=rnwFile, append=TRUE);

  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$corr,"}", sep=""),
    "\\caption{Correlation coefficients for metadata of interest.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$corr,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'ANOVA 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateAOV2doc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$aov2)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$aov2$sig.mat)){
    aov2.tab<-NULL;
  }else{
    aov2.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }
  
  descr <- c("\\subsection{Univariate Analysis}\n",
             "Univariate analysis methods are the most common methods used for exploratory data analysis. ",
             "For two-factor data, the basic approach is two-way ANOVA. ",
             "There are two options - between-subjects ANOVA and within-subjects ANOVA. When samples are all from",
             "independent subjects (i.e. general two-way ANOVA), the between-subjects option should be selected.",
             "However, time series data contains samples measured from the same subjects from different time points.",
             "Therefore within-subjects ANOVA should be used.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by ANOVA analysis."),
             aov2.tab,
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$anova2,"}", sep=""),
    "\\caption{Plot of important features selected by two-way ANOVA.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$anova2,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist2<-c("<<echo=false, results=tex>>=",
              "GetSigTable.Aov2(mSet)",
              "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

##### MULTIVARIATE ANALYSIS SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Random Forest ASCA
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateASCAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$asca)){
    return();
  }
  asca <- qs::qread("asca.qs");

  if(isEmptyMatrix(asca$sig.list[["Model.a"]])){
    asca.tab1<-NULL;
  }else{
    asca.tab1<-paste("Table", table.count<<-table.count+1,paste("shows features well-modelled by ", mSetObj$dataSet$facA.lbl, ". ", sep=""));
  }
  
  if(isEmptyMatrix(asca$sig.list[["Model.b"]])){
    asca.tab2<-NULL;
  }else{
    asca.tab2<-paste("Table", table.count<<-table.count+1,paste("shows features well-modelled by ", mSetObj$dataSet$facB.lbl, ". ", sep=""));
  }
  
  if(isEmptyMatrix(asca$sig.list[["Model.ab"]])){
    asca.tab3<-NULL;
  }else{
    asca.tab3<-paste("Table", table.count<<-table.count+1, "shows features well-modelled by Interaction model. ");
  }
  
  descr <- c("\\subsection{ANOVA - Simultaneous Component Analysis (ASCA)}\n",
             "ASCA is a multivariate extension of univariate ANOVA approach. It is",
             "designed to identify the major patterns associated with each factor.",
             "This implementation supports ASCA model for two factors with one interaction",
             "effect. The algorithm first partitions the overall data variance (X) into",
             "individual variances induced by each factor (A and B), as well as by the",
             "interactions (AB). The formula is shown below with (E) indicates the residual",
             "Errors: \n\n",
             "\\textbf{X = A + B + AB + E}",
             "\n\n",
             "The SCA part applies PCA to A, B, AB to summarize major variations in each partition.",
             "Users then detect the major pattern by visualizing the PCA scores plot.",
             "MetaboAnalyst also provides model validation to test the significance of the effects associated",
             "with main effects. It is based on the Manly's unrestricted permutation of observation",
             "then calculate the permuted variation associated with each factor",
             "Finally, the permuted values are compared with the original variations",
             "The significant variables are identified based on the leverage and the",
             "Squared Prediction Errors (SPE) associated with each variables",
             "Variables with low SPE and higher leverage are modeled well after the major patterns.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the scree plots for each effect model.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the major patterns associated with factor A.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the major patterns associated with factor B.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the major patterns associated with interaction.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the results of model validations through permutations.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the important features associated with factor A.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the important features associated with factor B.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the features that are important in the interaction.\n"),
             "\n\n",
             asca.tab1,
             asca.tab2,
             asca.tab3,
             "The other details are available as .csv documents in your downloaded zip file.");
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\\clearpage", file=rnwFile, append=TRUE);
  
  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.scree,"}", sep=""),
    "\\caption{Scree plots for each sub model.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.scree,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.modelA,"}", sep=""),
    paste("\\caption{Major patterns associated with", mSetObj$dataSet$facA.lbl, "}"),
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.modelA,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.modelB,"}", sep=""),
    paste("\\caption{Major patterns associated with", mSetObj$dataSet$facB.lbl, "}"),
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.modelB,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.modelAB,"}", sep=""),
    "\\caption{Major patterns associated with the Interaction between the two factors.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.modelAB,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.perm,"}", sep=""),
    "\\caption{Model validation through permutations}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.perm,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.impA,"}", sep=""),
    paste("\\caption{Important variables associated with", mSetObj$dataSet$facA.lbl, "}"),
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.impA,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.impB,"}", sep=""),
    paste("\\caption{Important variables associated with", mSetObj$dataSet$facB.lbl, "}"),
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.impB,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$asca.impAB,"}", sep=""),
    "\\caption{Variables important in interaction between the two factors}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$asca.impAB,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cat("\\clearpage\n\n", file=rnwFile, append=TRUE);
  
  if(!is.null(asca.tab1)){
    cmdhist<-c("<<echo=false, results=tex>>=",
               "GetSigTable.ASCA(mSet, \"Model.a\")",
               "@");
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(asca.tab2)){
    cmdhist <- c("<<echo=false, results=tex>>=",
                 "GetSigTable.ASCA(mSet, \"Model.b\")",
                 "@");
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(asca.tab3)){
    cmdhist <- c("<<echo=false, results=tex>>=",
                 "GetSigTable.ASCA(mSet, \"Model.ab\")",
                 "@");
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Multivariate Bayes
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateMBdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$MB)){
    return();
  }
  
  descr <- c("\\subsection{Multivariate Empirical Bayes Approach - MEBA}\n",
             "The approach is designed to compare the time-course profiles under different conditions.",
             "The result is a list of variables that are ranked by their difference in temporal profiles",
             "across different biological conditions. The Hotelling-T2 is used to rank the variables with",
             "different temporal profiles between two biological conditions under study; And the",
             "MB-statistics is used for more than two biological conditions. Higher statistical value",
             "indicates the time-course profiles are more different across the biological conditions under study.",
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "GetSigTable.MB(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}


##### SUPERVISED CLASSIFICATION SECTIONS #####

#'Create report of analyses 
#'@description Report generation using Sweave
#'Random Forest
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jessica Ewald \email{jessica.ewald@mail.mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
CreateRandomForest <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$dataSet$cls.rf)){
    return();
  }
  
  descr <- c("\\subsection{Random Forest}\n",
             "This implementation of random forest can only be used for classification, thus the primary metadata must be categorical.",
             "Predictors in the model, including both metabolites and other metadata, can include both categorical and numeric variables.",
             "Since random forest uses some random processes, turning randomness off means that",
             "the results will be the same each time that you run the tool. This can be better for reproducibility, but is not necessary.",
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);

  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.cls,"}", sep=""),
    "\\caption{Classification error vs. number of trees}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$rf.cls,"}", sep=""),
    "\\end{figure}"
  ); 
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.imp,"}", sep=""),
    "\\caption{Features ranked by their contributions to classification accuracy}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$rf.imp,"}", sep=""),
    "\\end{figure}"
  ); 
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");

  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.outlier,"}", sep=""),
    "\\caption{Up to five potential outliers flagged by random forest algorithm}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$rf.outlier,"}", sep=""),
    "\\end{figure}"
  ); 
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cat("\\clearpage", file=rnwFile, append=TRUE);
}