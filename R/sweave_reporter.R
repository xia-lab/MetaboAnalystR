#'Create report of analyses
#'@description Report generation using Sweave
#'Note: most analyses were already performed, only need to embed
#'the results to the right place without rerunning the whole analysis
#'through Sweave. Only some auxilliary info (i.e. time, version etc need to
#'run in R through Sweave
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreatePDFReport<-function(mSetObj=NA, usrName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # cannot detect whether PDF generation will fail, so do this all the time
  if(.on.public.web){
    file.copy("../../libs/Sweave.sty", ".")
    save.image("SweaveImage.RData");
  }

  # create the Rnw file
  file.create("Analysis_Report.Rnw");
  # open for write
  rnwFile <<- file("Analysis_Report.Rnw", "w")
  
  # create a global counter to label figures
  fig.count <<- 0;
  table.count <<- 0;
  
  if(mSetObj$analSet$type == "stat" ){
    CreateStatRnwReport(mSetObj, usrName);
  }else if(mSetObj$analSet$type == "ts"){
    CreateTimeSeriesRnwReport(mSetObj, usrName);
  }else if(substr(mSetObj$analSet$type, 0, 4) == "mset"){
    CreateEnrichRnwReport(mSetObj, usrName);
  }else if(mSetObj$analSet$type == "power"){
    CreatePowerRnwReport(mSetObj, usrName)
  }else if(mSetObj$analSet$type == "roc"){
    CreateBiomarkerRnwReport(mSetObj, usrName)
  }else if(mSetObj$analSet$type == "inmex"){
    CreateIntegPathwayAnalysisRnwReport(mSetObj, usrName)
  }else{
    CreatePathRnwReport(mSetObj, usrName);
  }
  
  # close opened files
  close(rnwFile);
  
  Sweave("Analysis_Report.Rnw");
  
  res <- try(tools::texi2dvi("Analysis_Report.tex", pdf = TRUE, quiet=TRUE));
  
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreateStatRnwReport <- function(mSetObj, usrName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateStatIntr();
  
  CreateStatIOdoc(mSetObj);
  CreateStatNORMdoc(mSetObj);
  
  InitStatAnalMode();
  if(exists("analSet", where = mSetObj)){
    CreateUNIVdoc(mSetObj);
    CreateANOVAdoc(mSetObj);
    CreateCorrDoc(mSetObj);
    CreatePCAdoc(mSetObj);
    CreatePLSdoc(mSetObj);
    CreateOPLSDAdoc(mSetObj);
    CreateSPLSDAdoc(mSetObj);
    CreateSAMdoc(mSetObj);
    CreateEBAMdoc(mSetObj);
    CreateHCdoc(mSetObj);
    CreateKMdoc(mSetObj);
    CreateSOMdoc(mSetObj);
    CreateRFdoc(mSetObj);
    CreateSVMdoc(mSetObj);
  }else{
    CreateAnalNullMsg();
  }
  CreateRHistAppendix();
  CreateStatFooter();
  
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create header
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreateHeader <- function(usrName){
  header <- c("\\documentclass[a4paper]{article}",
              "\\usepackage[margin=1.0in]{geometry}",
              "\\usepackage{longtable}",
              "<<echo=false>>=",
              "options(width=60);",
              "@",
              "\\SweaveOpts{eps=FALSE,pdf=TRUE}",
              "\\title{Metabolomic Data Analysis with MetaboAnalystR}",
              paste("\\author{ Name: ", usrName, " }", sep=""),
              "\\begin{document}",
              "\\parskip=.3cm",
              "\\maketitle");
  cat(header, file=rnwFile, sep="\n", append=TRUE);
  
}

CreateStatIntr <- function(){
  descr <- c("\\section{Data Processing and Normalization}\n");
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Read and process raw data
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateStatIOdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\subsection{Reading and Processing the Raw Data}\n",
             "MetaboAnalyst accepts a variety of data types generated in metabolomic studies,",
             "including compound concentration data, binned NMR/MS spectra data, NMR/MS peak",
             "list data, as well as MS spectra (NetCDF, mzXML, mzDATA).",
             "Users need to specify the data types when uploading their data in order for",
             "MetaboAnalyst to select the correct algorithm to process them.\n",
             paste("Table", table.count<<-table.count+1,"summarizes the result of the data processing steps.\n")
  );
  cat(descr, file=rnwFile, append=TRUE);
  
  # error checking
  if(is.null(mSetObj$dataSet$orig) | is.null(mSetObj$dataSet$procr) | is.null(mSetObj$dataSet$cls)){
    errorMsg <- c(descr, "Error occured during reading the raw data ....",
                  "Failed to proceed. Please check if the data format you uploaded is correct.",
                  "Please visit our FAQs, Data Formats, and TroubleShooting pages for more information!\n");
    cat(errorMsg, file=rnwFile, append=TRUE);
    return();
  }
  
  if(mSetObj$dataSet$type=="conc"){
    descr <- c("\\subsubsection{Reading Concentration Data}\n",
               "The concentration data should be uploaded in comma separated values (.csv) format.",
               "Samples can be in rows or columns, with class labels immediately following the sample IDs.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
  }else if(mSetObj$dataSet$type=="specbin"){
    descr <- c("\\subsubsection{Reading Binned Spectral Data}\n",
               "The binned spectra data should be uploaded in comma seperated values (.csv) format.",
               "Samples can be in rows or columns, with class labels immediately following the sample IDs.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
  }else if(mSetObj$dataSet$type=="pktable"){
    descr <- c("\\subsubsection{Reading Peak Intensity Table}\n",
               "The peak intensity table should be uploaded in comma separated values (.csv) format.",
               "Samples can be in rows or columns, with class labels immediately following the sample IDs.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
  }else if(mSetObj$dataSet$type=="nmrpeak"){
    descr <- c("\\subsubsection{Reading NMR Peak List and Intensities Data}\n",
               "NMR peak list and intensities data should be uploaded as one zip file. It contains subfolders, one for",
               "each group. Each folder contains peak list files, one per spectrum. The peak list format",
               "is a a two-column comma separated values - the first column indicates peak position (ppm)",
               "and the second one for peak intensities. The first line is assumed to be column labels.",
               "The files should be saved in .csv format. For paired analysis, users need to upload",
               "a text file specifying the paired information. Each pair is indicated by their sample names",
               "seperated by a colon \":\" with one pair per line.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
    descr <- c("\\subsubsection{Peak List Alignment}\n",
               "Proximal peaks are first grouped together based on their position using a moving window of 0.03 ppm",
               "and a step of 0.015 ppm. Peaks of the same group are aligned to their median positions across all samples.",
               "If more than one peak from the same sample appear in the same group, they will be replaced by their sum.",
               "Some peaks that are detected in very few samples (less than half in both classes) are excluded.",
               "The aligned peaks are reorganized into a single data matrix for further analysis. The name of the parent",
               "folder is used as class label for each sample.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$proc.msg, file=rnwFile, append=TRUE, sep="\n");
    
  }else if(mSetObj$dataSet$type=="mspeak"){
    descr <- c("\\subsubsection{Reading MS Peak List and Intensities Data}\n",
               "MS peak list and intensities data should be uploaded as one zip file. It contains subfoulders with one for",
               "each group. Each folder contains peak list files, one per spectrum. The MS peak list format",
               "is either a two-column (mass and intensities) or three-column (mass, retention time, and intensities)",
               "comma separated values. The first line is assumed to be column labels.",
               "The files should be saved in .csv format. For paired analysis, users need to upload separately",
               "a text file specifying the paired information. Each pair is indicated by their sample names",
               "seperated by a colon \":\" with one pair per line.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
    descr <- c("\\subsubsection{Peak Matching and Alignment}\n",
               "Peaks need to be matched across samples in order to be compared. For two-column data, the",
               "program matches peaks by their m/z values. For three-column data, the program will further",
               "group peaks based on their retention time. During the process, mz and rt of each peak will",
               "be changed to their group median values. If a sample has more than one peak in a group,",
               "they will be replaced by their sum. Some peaks are excluded if they appear in less than half",
               "of both classes. The aligned peaks are reorganized into a single data matrix for further analysis.",
               "The name of the parent folder is used as class label for each sample.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$proc.msg, file=rnwFile, append=TRUE, sep="\n");
    
  }else{ # spectra
    descr <- c("\\subsubsection{Reading GC/LC-MS Spectra}\n",
               "The spectra processing is carried out using the XCMS package.",
               "\\footnote{Colin A. Smith and Ralf Tautenhahn. \\textit{xcms: LC\\slash MS and",
               "GC\\slash MS Data Analysis}, 2008, R package version 1.14.0}",
               "Raw GC/LC-MS spectra can be in either NetCDF, mzXML or mzData format.",
               "You should create a separate folder for each group and create a",
               "single zip file to upload to MetaboAnalyst. The size limit for each uploaded file is 50M.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
    
    descr <- c("\\subsubsection{Peak Alignment and Retention Time Correction}\n",
               "In this step, the program automatically performs peak detection and peak alignment based on mass and",
               "retention time. The aligned peaks are reorganized into a data matrix for further analysis.",
               "Please note, the name of the parent folder is used as class label for each sample.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    cat(c(mSetObj$msgSet$xset.msg, paste("Please see Figure", fig.count<<-fig.count+1, "for a summary graph.")), file=rnwFile, append=TRUE, sep="\n");
    
    cmdhist <- c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$msrt,"}", sep=""),
      "\\caption{Retention time deviation profiles used for sample alignment.
      The data points used for generating each profile are also shown.
      All times are in seconds. A negative number indicates a sample was eluting
      before most of the others, and vice versa.}",
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$msrt,"}", sep=""),
      "\\end{figure}"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
}
  
  # the last step is sanity check
  descr <- c("\\subsubsection{Data Integrity Check}\n",
             "Before data analysis, a data integrity check is performed to make sure that all the necessary",
             "information has been collected. The class labels must be present and contain only two classes.",
             "If samples are paired, the class label must be from -n/2 to -1 for one group, and 1 to n/2 for the other group",
             "(n is the sample number and must be an even number). Class labels with same absolute value are assumed to be pairs.",
             "Compound concentration or peak intensity values should all be non-negative numbers.",
             "By default, all missing values, zeros and negative values will be replaced by the half of the minimum positive value",
             "found within the data (see next section)");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr <- c("\\subsubsection{Missing value imputations}\n",
             "Too many zeroes or missing values will cause difficulties for downstream analysis.",
             "MetaboAnalyst offers several different methods for this purpose. The default method replaces ",
             "all the missing and zero values with a small values (the half of the minimum positive",
             "values in the original data) assuming to be the detection limit. The assumption of this approach",
             "is that most missing values are caused by low abundance metabolites (i.e.below the detection limit).",
             "In addition, since zero values may cause problem for data normalization (i.e. log), they are also ",
             "replaced with this small value. User can also specify other methods, such as replace by mean/median,",
             "or use K-Nearest Neighbours, Probabilistic PCA (PPCA), Bayesian PCA (BPCA) method, Singular Value Decomposition (SVD)",
             "method to impute the missing values \\footnote{Stacklies W, Redestig H, Scholz M, Walther D, Selbig J.",
             "\\textit{pcaMethods: a bioconductor package, providing PCA methods for incomplete data.}, Bioinformatics",
             "2007 23(9):1164-1167}. Please choose the one that is the most appropriate for your data.");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cat(mSetObj$msgSet$replace.msg, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  # the data filtering
  descr <- c("\\subsubsection{Data Filtering}\n",
             "The purpose of the data filtering is to identify and remove variables that are unlikely to be of",
             "use when modeling the data. No phenotype information are used in the filtering process, so the result",
             "can be used with any downstream analysis. This step can usually improves the results.",
             "Data filter is strongly recommended for datasets with large number of variables (> 250)",
             "datasets contain much noise (i.e.chemometrics data). Filtering can usually improve your",
             
             "results\\footnote{Hackstadt AJ, Hess AM.\\textit{Filtering for increased power for microarray data analysis},",
             "BMC Bioinformatics. 2009; 10: 11.}.",
             "\n\n",
             "\\textit{For data with number of variables  < 250, this step will reduce 5\\% of variables;",
             "For variable number between 250 and 500, 10\\% of variables will be removed;",
             "For variable number bwteen 500 and 1000, 25\\% of variables will be removed;",
             "And 40\\% of variabled will be removed for data with over 1000 varaibles.}");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  filt.msg <- mSetObj$msgSet$filter.msg;
  if(is.null(filt.msg)){
    filt.msg <- "No data filtering was performed.";
  }
  cat(filt.msg, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "CreateSummaryTable(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create normalization document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateStatNORMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$dataSet$norm)){
    errorMsg <- c("Error occured during normalization of your data ....",
                  "Fail to proceed. Please check if the data format you uploaded is correct.",
                  "Please visit our FAQs, Data Formats, and TroubleShooting pages for more information!");
    cat(errorMsg, file=rnwFile, append=TRUE);
    return();
  }
  
  descr1 <- c("\\subsection{Data Normalization}\n",
              "The data is stored as a table with one sample per row and one variable (bin\\slash peak\\slash",
              "metabolite) per column. The normalization procedures implemented below are grouped into four categories.",
              "Sample specific normalization allows users to manually adjust concentrations based on biological inputs",
              "(i.e. volume, mass); row-wise normalization allows general-purpose adjustment for differences among samples;",
              "data transformation and scaling are two different approaches to make features more comparable.",
              "You can use one or combine both to achieve better results.",
              "\n\n",
              "The normalization consists of the following options:");
  
  cat(descr1, file=rnwFile, append=TRUE);
  
  descr2 <- c("\\begin{enumerate}",
              "\\item{Sample specific normalization (i.e. normalize by dry weight, volume) }",
              "\\item{Row-wise procedures: }",
              "\\begin{itemize}",
              "\\item{Normalization by the sum }",
              "\\item{Normalization by the sample median }",
              "\\item{Normalization by a reference sample (probabilistic quotient",
              "normalization)\\footnote{Dieterle F, Ross A, Schlotterbeck G, Senn H. \\textit{Probabilistic quotient normalization as robust",
              "method to account for dilution of complex biological mixtures. Application in 1H NMR metabonomics}, 2006,",
              "Anal Chem 78 (13);4281 - 4290}}",
              "\\item{Normalization by a reference feature (i.e. creatinine, internal control) }",
              "\\end{itemize}",
              "\\item{Data transformation : }",
              "\\begin{itemize}",
              "\\item{Generalized log transformation (glog 2) }",
              "\\item{Cube root transformation}",
              "\\end{itemize}",
              "\\item{Data scaling: }",
              "\\begin{itemize}",
              "\\item{Unit scaling (mean-centered and divided by standard deviation of each variable)}",
              "\\item{Pareto scaling (mean-centered and divided by the square root of standard deviation of each variable)}",
              "\\item{Range scaling (mean-centered and divided by the value range of each variable)}",
              "\\end{itemize}",
              "\\end{enumerate}",
              "\n\n",
              if(exists("norm", where=mSetObj$imgSet)){
                paste("Figure", fig.count<<-fig.count+1,"shows the effects before and after normalization.\n");
              })
  
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
  
  if(mSetObj$dataSet$combined.method){
    norm.desc <- "Combined approach using quantile normalization within replicates after log transformation.";
  }else{
    if(exists(mSetObj$dataSet$rownorm.method)){
      norm.desc <- paste("Row-wise normalization: ", mSetObj$dataSet$rownorm.method, "; ",
                         "Data transformation: ",mSetObj$dataSet$trans.method, "; ",
                         "Data scaling: ",mSetObj$dataSet$scale.method, ".", sep="");
    }else{
      norm.desc <- "No normalization methods were applied."
    }
  }
  
  if(exists("norm", where=mSetObj$imgSet)){
    cmdhist <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$norm,"}", sep=""),
                  "\\caption{Box plots and kernel density plots before and after normalization.",
                  "The boxplots show at most 50 features due to space limit. The density plots are based on all samples.",
                  "Selected methods :", norm.desc, "}",
                  "\\end{center}",
                  paste("\\label{",mSetObj$imgSet$norm,"}", sep=""),
                  "\\end{figure}",
                  "\\clearpage"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

InitStatAnalMode <- function(){
  descr <- c("\\section{Statistical and Machine Learning Data Analysis}",
             "MetaboAnalyst offers a variety of methods commonly used in metabolomic data analyses.",
             "They include:\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr2 <- c(
    "\\begin{enumerate}",
    "\\item{Univariate analysis methods: }",
    "\\begin{itemize}",
    "\\item{Fold Change Analysis }",
    "\\item{T-tests}",
    "\\item{Volcano Plot}",
    "\\item{One-way ANOVA and post-hoc analysis}",
    "\\item{Correlation analysis}",
    "\\end{itemize}",
    "\\item{Multivariate analysis methods: }",
    "\\begin{itemize}",
    "\\item{Principal Component Analysis (PCA) }",
    "\\item{Partial Least Squares - Discriminant Analysis (PLS-DA) }",
    "\\end{itemize}",
    "\\item{Robust Feature Selection Methods in microarray studies }",
    "\\begin{itemize}",
    "\\item{Significance Analysis of Microarray (SAM)}",
    "\\item{Empirical Bayesian Analysis of Microarray (EBAM)}",
    "\\end{itemize}",
    "\\item{Clustering Analysis}",
    "\\begin{itemize}",
    "\\item{Hierarchical Clustering}",
    "\\begin{itemize}",
    "\\item{Dendrogram}",
    "\\item{Heatmap}",
    "\\end{itemize}",
    "\\item{Partitional Clustering}",
    "\\begin{itemize}",
    "\\item{K-means Clustering}",
    "\\item{Self-Organizing Map (SOM)}",
    "\\end{itemize}",
    "\\end{itemize}",
    "\\item{Supervised Classification and Feature Selection methods}",
    "\\begin{itemize}",
    "\\item{Random Forest}",
    "\\item{Support Vector Machine (SVM)}",
    "\\end{itemize}",
    "\\end{enumerate}",
    "\\texttt{Please note: some advanced methods are available only for two-group sample analyais.}",
    "\\clearpage"
  );
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
}

CreateAnalNullMsg <- function(){
  descr <- c("No analysis was performed on your data.\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create univariate analyses document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateUNIVdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$fc) & is.null(mSetObj$analSet$tt) & is.null(mSetObj$analSet$volcano)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$fc$sig.mat)){
    fc.tab<-NULL;
  }else{
    fc.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }
  if(isEmptyMatrix(mSetObj$analSet$tt$sig.mat)){
    tt.tab<-NULL;
  }else{
    tt.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }
  if(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat)){
    volcano.tab<-NULL;
  }else{
    volcano.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features.");
  }
  
  descr <- c("\\subsection{Univariate Analysis}\n",
             "Univariate analysis methods are the most common methods used for exploratory data analysis. ",
             "For two-group data, MetaboAnalyst provides Fold Change (FC) analysis, t-tests, and volcano",
             "plot which is a combination of the first two methods. All three these methods support both",
             "unpaired and paired analyses. For multi-group analysis, MetaboAnalyst provides two types of",
             "analysis - one-way analysis of variance (ANOVA) with associated post-hoc analyses, and correlation",
             "analysis to identify signficant compounds that follow a given pattern. The univariate analyses provide",
             "a preliminary overview about features that are potentially significant in discriminating",
             "the conditions under study.",
             "\n\n",
             "For paired fold change analysis, the algorithm first counts the total number of pairs with fold changes",
             "that are consistently above/below the specified FC threshold for each variable. A variable will be",
             "reported as significant if this number is above a given count threshold (default > 75\\% of pairs/variable)",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by fold change analysis."),
             fc.tab,
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by t-tests."),
             tt.tab,
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by volcano plot."),
             volcano.tab,
             "\n\n",
             "Please note, the purpose of fold change is to compare absolute value changes between two group means.",
             "Therefore, the data before column normlaization will be used instead. Also note, the result is plotted",
             "in log2 scale, so that same fold change (up/down-regulated) will have the same distance to the zero baseline.",
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # Fold change
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$fc,"}", sep=""),
    "\\caption{", paste("Important features selected by fold-change analysis with threshold ", mSetObj$analSet$fc$raw.thresh, ". ",
                        "The red circles represent features above the threshold. Note the values are on log scale, so that both up-regulated ",
                        "and downregulated features can be plotted in a symmetrical way", sep=""), "}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$fc,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  
  if(!(isEmptyMatrix(mSetObj$analSet$fc$sig.mat))){
    cmdhist2 <- c("<<echo=false, results=tex>>=",
                  "GetSigTable.FC(mSet)",
                  "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  # T-tests
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$tt,"}", sep=""),
    "\\caption{", paste("Important features selected by t-tests with threshold ", mSetObj$analSet$tt$raw.thresh, ". ",
                        "The red circles represent features above the threshold. Note the p values are transformed by -log10 so that the more significant ",
                        "features (with smaller p values) will be plotted higher on the graph. ", sep=""),"}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$tt,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  
  if(!(isEmptyMatrix(mSetObj$analSet$tt$sig.mat))){
    cmdhist2<-c("<<echo=false, results=tex>>=",
                "GetSigTable.TT(mSet)",
                "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  # Volcano plot
  cmdhist <- c( 
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$volcano,"}", sep=""),
    "\\caption{", paste("Important features selected by volcano plot with fold change threshold (x) ",
                        mSetObj$analSet$volcano$raw.threshx, " and t-tests threshold (y) ", mSetObj$analSet$volcano$raw.threshy, ". ",
                        "The red circles represent features above the threshold. Note both fold changes and p values are log ",
                        "transformed. The further its position away from the (0,0), the more significant the feature is. ", sep=""),"}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$volcano,"}", sep=""),
    "\\end{figure}"
  );
  
  if(!(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat))){
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
    cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
    cmdhist2 <- c("<<echo=false, results=tex>>=",
                  "GetSigTable.Volcano(mSet)",
                  "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create ANOVA document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateANOVAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$aov)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$aov$sig.mat)){
    anova.tab<-NULL;
  }else{
    anova.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.",
                       "The \\texttt{post-hoc Sig. Comparison} column shows the comparisons between different levels",
                       "that are significant given the p value threshold. ");
  }
  
  descr <- c("\\subsection{One-way ANOVA}\n",
             "Univariate analysis methods are the most common methods used for exploratory data analysis. ",
             "For multi-group analysis, MetaboAnalyst provides one-way Analysis",
             "of Variance (ANOVA). As ANOVA only tells whether the overall comparison is significant or not,",
             "it is usually followed by post-hoc analyses in order to identify which two levels are different.",
             "MetaboAnalyst provides two most commonly used methods for this purpose - Fisher's",
             "least significant difference method (Fisher's LSD) and Tukey's Honestly Significant Difference",
             "(Tukey's HSD). The univariate analyses provide a preliminary overview about features that are",
             "potentially significant in discriminating the conditions under study.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by ANOVA analysis."),
             anova.tab,
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # ANOVA
  cmdhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$anova,"}", sep=""),
    "\\caption{", paste("Important features selected by ANOVA plot with p value threshold ",
                        mSetObj$analSet$aov$raw.thresh, ". ", sep=""),"}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$anova,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "GetSigTable.Anova(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create correlation document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateCorrDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$cor.res) & is.null(mSetObj$imgSet$corr.heatmap)){
    return();
  }
  
  # need to check if this process is executed
  if(!is.null(mSetObj$imgSet$corr.heatmap)){
    descr <- c("\\subsection{Correlation Analysis}\n",
               "Correlation analysis can be used to visualize the overall correlations between different features",
               "It can also be used to identify which features are correlated with a feature of interest.",
               "Correlation analysis can also be used to identify if certain features show particular patterns",
               "under different conditions. Users first need to define a pattern in the form of a series of hyphenated numbers.",
               "For example, in a time-series study with four time points, a pattern of of",
               "\\texttt{1-2-3-4} is used to search compounds with increasing the concentration as",
               "time changes; while a pattern of \\texttt{3-2-1-3} can be used to search compounds",
               "that decrease at first, then bounce back to the original level.",
               "\n\n",
               paste("Figure", fig.count<<-fig.count+1, "shows the overall correlation heatmap."),
               "\n");
    
    cat(descr, file=rnwFile, append=TRUE);
    
    cmdhist<-c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$corr.heatmap,"}", sep=""),
      "\\caption{Correlation Heatmaps}",
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$corr.heatmap,"}", sep=""),
      "\\end{figure}"
    );
    
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
    cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(mSetObj$analSet$cor.res)){
    if(isEmptyMatrix(mSetObj$analSet$cor.res)){
      cor.tab <- NULL;
    }else{
      cor.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
    }
    
    descr <- c("\\subsection{Correlation Analysis}\n",
               "Correlation analysis can be used to identify which features are correlated with a feature of interest.",
               "Correlation analysis can also be used to identify if certain features show particular patterns",
               "under different conditions. Users first need to define a pattern in the form of a series of hyphenated numbers.",
               "For example, in a time-series study with four time points, a pattern of of",
               "\\texttt{1-2-3-4} is used to search compounds with increasing the concentration as",
               "time changes; while a pattern of \\texttt{3-2-1-3} can be used to search compounds",
               "that decrease at first, then bounce back to the original level.",
               "\n\n",
               paste("Figure", fig.count<<-fig.count+1, "shows the important features identified by correlation analysis."),
               cor.tab,
               "\n");
    
    cat(descr, file=rnwFile, append=TRUE);
    
    cmdhist <- c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$corr,"}", sep=""),
      "\\caption{Important features selected by correlation analysis with light",
      "purple indicates positive correlation and blue indicate negative correlations.}",
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$corr,"}", sep=""),
      "\\end{figure}"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
    cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
    
    cmdhist2<-c("<<echo=false, results=tex>>=",
                "GetSigTable.Corr(mSet)",
                "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  }
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create PCA document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePCAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$pca)){
    return();
  }
  
  descr <- c("\\subsection{Principal Component Analysis (PCA)}\n",
             "PCA is an unsupervised method aiming to find the directions that best",
             "explain the variance in a data set (X) without referring to class labels (Y).",
             "The data are summarized into much fewer variables called \\textit{scores} which",
             "are weighted average of the original variables. The weighting profiles are called",
             "\\textit{loadings}. The PCA analysis is performed using the \\texttt{prcomp} package.",
             "The calculation is based on singular value decomposition.",
             "\n\n",
             "The Rscript \\texttt{chemometrics.R} is required.",
             paste("Figure", fig.count<<-fig.count+1,"is pairwise score plots providing an overview of the various seperation patterns among the most significant PCs;"),
             paste("Figure", fig.count<<-fig.count+1,"is the scree plot showing the variances explained by the selected PCs;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 2-D scores plot between selected PCs;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 3-D scores plot between selected PCs;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the loadings plot between the selected PCs;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the biplot between the selected PCs.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.pair,"}", sep=""),
    "\\caption{Pairwise score plots between the selected PCs. The explained variance of each PC is shown in the corresponding diagonal cell. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.pair,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.scree,"}", sep=""),
    "\\caption{Scree plot shows the variance explained by PCs. The green line on top shows the accumulated variance explained; the blue line underneath shows the variance explained by individual PC.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.scree,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.score2d,"}", sep=""),
    "\\caption{Scores plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.score2d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.score3d,"}", sep=""),
    "\\caption{3D score plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.score3d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.loading,"}", sep=""),
    "\\caption{Loadings plot for the selected PCs. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.loading,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pca.biplot,"}", sep=""),
    "\\caption{PCA biplot between the selected PCs. Note, you may want to test different centering and scaling
    normalization methods for the biplot to be displayed properly.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pca.biplot,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create PLS document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePLSdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$plsr) & is.null(mSetObj$analSet$plsda)){
    return();
  }
  
  descr <- c("\\subsection{Partial Least Squares - Discriminant Analysis (PLS-DA)}\n",
             "PLS is a supervised method that uses multivariate regression techniques to extract via",
             "linear combination of original variables (X) the information that can predict the",
             "class membership (Y). The PLS regression is performed using the \\texttt{plsr} function",
             "provided by R \\texttt{pls} package\\footnote{Ron Wehrens and Bjorn-Helge Mevik.\\textit{pls: Partial Least",
             "Squares Regression (PLSR) and Principal Component Regression (PCR)}, 2007,",
             "R package version 2.1-0}. The classification and cross-validation are performed using the corresponding wrapper",
             "function offered by the \\texttt{caret} package\\footnote{Max Kuhn. Contributions from",
             "Jed Wing and Steve Weston and Andre Williams.\\textit{caret: Classification and Regression",
             "Training}, 2008, R package version 3.45}.",
             "\n\n",
             "To assess the significance of class discrimination, a permutation test was performed. In each permutation, a PLS-DA model was",
             "built between the data (X) and the permuted class labels (Y) using the optimal number of components determined",
             "by cross validation for the model based on the original class assignment. MetaboAnalyst supports two types of test",
             "statistics for measuring the class discrimination. The first one is based on prediction accuracy during training.",
             "The second one is separation distance based on the ratio of the between group sum of the squares and the within",
             "group sum of squares (B/W-ratio).",
             "If the observed test statistic is part of the distribution based on the permuted class assignments,",
             "the class discrimination cannot be considered significant from a statistical point of",
             "view.\\footnote{Bijlsma et al.\\textit{Large-Scale Human Metabolomics Studies: A Strategy for Data",
             "(Pre-) Processing and Validation}, Anal Chem. 2006, 78 567 - 574}.",
             "\n\n",
             "There are two variable importance measures in PLS-DA. The first, Variable Importance in Projection (VIP) is",
             "a weighted sum of squares of the PLS loadings taking into account the amount of explained Y-variation",
             "in each dimension. Please note, VIP scores are calculated for each components. When more than componetnts are used to calculate", 
             "the feature importance, the average of the VIP scores are used. The other importance measure is based",
             "on the weighted sum of PLS-regression. The weights are a function of the reduction of the sums of squares across the number",
             "of PLS components. Please note, for multiple-group (more than two) analysis, the same number of predictors will be built for each",
             "group. Therefore, the coefficient of each feature will be different depending on which group you want to predict.",
             "The average of the feature coefficients are used to indicate the overall coefficient-based importance. ",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the overview of scores plots;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 2-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 3-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the loading plot between the selected components;"));
  cat(descr, file=rnwFile, append=TRUE);
  
  
  descr <- c(paste("Figure", fig.count<<-fig.count+1,"shows the classification performance with different number of components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the results of permutation test for model validation;"),
             paste("Figure", fig.count<<-fig.count+1,"shows important features identified by PLS-DA.\n"));
  cat(descr, file=rnwFile, append=TRUE);
  
  
  plsrhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.pair,"}", sep=""),
    "\\caption{Pairwise scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.pair,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.score2d,"}", sep=""),
    "\\caption{Scores plot between the selected PCs. The explained variances are shown in brackets. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.score2d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.score3d,"}", sep=""),
    "\\caption{3D scores plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.score3d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.loading,"}", sep=""),
    "\\caption{Loadings plot between the selected PCs. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.loading,"}", sep=""),
    "\\end{figure}"
  );
  cat(plsrhist, file=rnwFile, append=TRUE, sep="\n");
  
  plsdahist <- c(
    # classification fig
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.class,"}", sep=""),
    "\\caption{PLS-DA classification using different number of components. The red star indicates the best classifier.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.class,"}", sep=""),
    "\\end{figure}"
  );
  cat(plsdahist, file=rnwFile, append=TRUE, sep="\n");
  
  plsdahist <- c(
    # permutation fig
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.permut,"}", sep=""),
    paste("\\caption{PLS-DA model validation by permutation tests based on ", mSetObj$analSet$plsda$permut.type, ". ",
          "The p value based on permutation is ", mSetObj$analSet$plsda$permut.p, ".", sep=""), "}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.permut,"}", sep=""),
    "\\end{figure}"
  );
  cat(plsdahist, file=rnwFile, append=TRUE, sep="\n");
  
  
  plsdahist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pls.imp,"}", sep=""),
    paste("\\caption{Important features identified by PLS-DA. The colored boxes on the right indicate the relative concentrations of the corresponding metabolite in each group
          under study. }"),
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.imp,"}", sep=""),
    "\\end{figure}"
    );
  cat(plsdahist, file=rnwFile, append=TRUE, sep="\n");
  
  
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create sPLS-DA document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateSPLSDAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$splsr)){
    return();
  }
  
  descr <- c("\\subsection{Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)}\n",
             "The sparse PLS-DA (sPLS-DA) algorithm can be used to effectively reduce the number of variables (metabolites)", 
             "in high-dimensional metabolomics data to produce robust and easy-to-interpret models.", 
             "Users can control the sparseness of the model by controling the number of components in the model and the number ",
             "of variables in each component. For more information, please refer to Cao et al. 2011 (PMC3133555). ",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the overview of scores plots;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 2-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the loading plot of the top ranked features;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 3-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the performance of the sPLS-DA model evaluted using cross-validations;"));
  cat(descr, file=rnwFile, append=TRUE);
  
  
  plsrhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$spls.pair,"}", sep=""),
    "\\caption{Pairwise scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$spls.pair,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$spls.score2d,"}", sep=""),
    "\\caption{Scores plot between the selected PCs. The explained variances are shown in brackets. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$spls.score2d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$spls.score3d,"}", sep=""),
    "\\caption{Plot showing the variables selected by the sPLS-DA model for a given component. The variables are ranked by the absolute values of their loadings.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.score3d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$spls.imp,"}", sep=""),
    "\\caption{3D scores plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$spls.imp,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$splsda.class,"}", sep=""),
    "\\caption{Plot of the performance of the sPLS-DA model evaluated using cross validations (CV) with increasing numbers
    of components created using the specified number of the variables. The error rate is on the y-axis and the number of components
    is on the x-axis.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$splsda.class,"}", sep=""),
    "\\end{figure}"
  );
  cat(plsrhist, file=rnwFile, append=TRUE, sep="\n");
  
  
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create OPLSDA document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateOPLSDAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$oplsda)){
    return();
  }
  
  descr <- c("\\subsection{Orthogonal-Orthogonal Projections to Latent Structures Discriminant Analysis (OPLS-DA)}\n", 
             "OPLS-DA, like PLS-DA, is a powerful tool used for dimension reduction and identification of spectral features that ",
             "drive group separation. It is a supervised modeling method, and may be used instead of PLS-DA due to its capablitities ",
             "to distinguish between variations in a dataset relevant to predicting group-labels and variations irrelevant",
             " to predicting group-labels. In this sense, OPLS-DA tends to make models that are less complex and more insightful",
             " than PLS-DA. However, both OPLS-DA and PLS-DA are prone to create models that over-fit data, therefore requiring ",
             "cross-validation to ensure model reliability. For further details, please refer to Worley and Powers 2013 (PMC4465187) ",
             "and Worley and Powers 2016 (PMC4990351). The permutation testing for OPLS-DA is provided from Szymanska et al. 2012.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the score plot for all metabolite features;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the variable importance in an OPLS-DA model;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the model overview;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the results of the permutation tests for the models;"));
  cat(descr, file=rnwFile, append=TRUE);
  
  
  oplsrhist<-c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$opls.score2d,"}", sep=""),
    "\\caption{OPLS-DA score plot of all metabolite features. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$opls.score2d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$opls.loading,"}", sep=""),
    "\\caption{OPLS-DA loadings S-plot showing the variable importance in a model, combining the covariance and the correlation (p(corr)) loading profile. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$opls.loading,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$opls.class,"}", sep=""),
    "\\caption{Model overview of the OPLS-DA model for the provided dataset. It shows the R2X, R2Y, and Q2 coefficients for the groups.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$opls.class,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$opls.permut,"}", sep=""),
    "\\caption{Permutation analysis, showing the observed and cross-validated R2Y and Q2 coefficients. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$opls.permut,"}", sep=""),
    "\\end{figure}"
  );
  cat(oplsrhist, file=rnwFile, append=TRUE, sep="\n");
  
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create SAM document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateSAMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$sam)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$sam.cmpds)){
    sam.tab <- NULL;
  }else{
    sam.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
  }
  
  descr <- c("\\subsection{Significance Analysis of Microarray (SAM)}\n",
             "SAM is a well-established statistical method for identification",
             "of differentially expressed genes in microarray data analysis. It is designed",
             "to address the false discovery rate (FDR) when running multiple tests on high-dimensional",
             "microarray data. SAM assigns a significance score to each variable based on its change",
             "relative to the standard deviation of repeated measurements. For a variable with scores",
             "greater than an adjustable threshold, its relative difference is compared to the",
             "distribution estimated by random permutations of the class labels. For each threshold,",
             "a certain proportion of the variables in the permutation set will be found to be significant",
             "by chance. The proportion is used to calculate the FDR. SAM is performed using the",
             "\\texttt{siggenes} package\\footnote{Holger Schwender. \\textit{siggenes: Multiple testing using",
             "SAM and Efron's empirical Bayes approaches},2008, R package version 1.16.0}.",
             "Users need to specify the \\texttt{Delta} value to control FDR in order to proceed.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the significant features identified by SAM."),
             sam.tab,
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c( 
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$sam.cmpd,"}", sep=""),
    "\\caption{Significant features identified by SAM. The green circles represent features
    that exceed the specified threshold. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$sam.cmpd,"}", sep=""),
    "\\end{figure}"
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "GetSigTable.SAM(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create EBAM document
#'Note: the search for delta (SAM) and a0 (EBAM) will not be plotted
#'it is only exploration, and may cause potential inconsistentcies. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreateEBAMdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$ebam)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$ebam.cmpds)){
    ebam.tab<-NULL;
  }else{
    ebam.tab <- paste("Table", table.count<<-table.count+1,"shows the details of these features.");
  }
  
  descr <- c("\\subsection{Empirical Bayesian Analysis of Microarray (EBAM)}\n",
             "EBAM is an empirical Bayesian method based on moderated t-statistics.",
             "EBAM uses a two-group mixture model for null and significant features.",
             "The prior and density parameters are estimated from the data. A feature is",
             "considered significant if its calculated posterior is larger than or equal to",
             "\\texttt{delta} and no other features with a more extreme test score that",
             "is not called signicant. The default is \\texttt{delta} = 0.9.",
             "The suggested fudge factor (\\texttt{a0}) is chosen that leads to the largest number",
             "of significant features. EBAM is performed with \\texttt{ebam} function in",
             "\\texttt{siggenes} package\\footnote{Holger Schwender. \\textit{siggenes: Multiple testing using",
             "SAM and Efron's empirical Bayes approaches},2008,R package version 1.16.0}.",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by EBAM."),
             ebam.tab,
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c( 
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$ebam.cmpd,"}", sep=""),
    "\\caption{Significant features identified by EBAM. The green circles represent features
    that exceed the specified threshold. }",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$ebam.cmpd,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "GetSigTable.EBAM(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create hierarchical clustering document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateHCdoc <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$tree) & is.null(mSetObj$analSet$htmap)){
    return();
  }
  
  descr <- c("\\subsection{Hierarchical Clustering}\n",
             "In (agglomerative) hierarchical cluster analysis, each sample begins",
             " as a separate cluster and the algorithm proceeds to combine them until all",
             "samples belong to one cluster. Two parameters need to be considered when performing",
             "hierarchical clustering. The first one is similarity measure - Euclidean distance,",
             "Pearson's correlation, Spearman's rank correlation. The other parameter is clustering",
             "algorithms, including average linkage (clustering uses the centroids of the observations),",
             "complete linkage (clustering uses the farthest pair of observations between the two groups),",
             "single linkage (clustering uses the closest pair of observations) and Ward's linkage",
             "(clustering to minimize the sum of squares of any two clusters). Heatmap is often presented",
             "as a visual aid in addition to the dendrogram.",
             "\n\n",
             "Hierachical clustering is performed with the \\texttt{hclust} function in package \\texttt{stat}.",
             paste("Figure", fig.count<<-fig.count+1,"shows the clustering result in the form of a dendrogram."),
             paste("Figure", fig.count<<-fig.count+1,"shows the clustering result in the form of a heatmap.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  if(!is.null(mSetObj$analSet$tree)){
    cmdhist <- c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$tree,"}", sep=""),
      paste("\\caption{Clustering result shown as dendrogram (", 
            "distance measure using ", "\\texttt{", mSetObj$analSet$tree$dist.par, "}, and clustering algorithm using ", "\\texttt{", mSetObj$analSet$tree$clust.par, "}).}", sep=""),
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$tree,"}", sep=""),
      "\\end{figure}"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  if(!is.null(mSetObj$analSet$htmap)){
    cmdhist <- c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$heatmap,"}", sep=""),
      paste("\\caption{Clustering result shown as heatmap (", 
            "distance measure using ", "\\texttt{", mSetObj$analSet$htmap$dist.par, "}, and clustering algorithm using ", "\\texttt{", mSetObj$analSet$htmap$clust.par, "}).}", sep=""),
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$heatmap,"}", sep=""),
      "\\end{figure}"
    );                    
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create SOM partitional clustering document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateSOMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$som)){
    return();
  };
  
  descr <- c("\\subsection{Self Organizing Map (SOM)}\n",
             "SOM is an unsupervised neural network algorithm used to automatically",
             "identify major trends present in high-dimensional data. SOM is based",
             "on a grid of interconnected nodes, each of which represents a model.",
             "These models begin as random values, but during the process of iterative training",
             "they are updated to represent different subsets of the training set.",
             "Users need to specify the x and y dimension of the grid to perform SOM analysis.",
             "\n\n",
             "The SOM is performed using the R \\texttt{som} package\\footnote{Jun Yan. \\textit{som:",
             "Self-Organizing Map}, 2004, R package version 0.3-4}.",
             paste("Figure", fig.count<<-fig.count+1,"shows the SOM clustering results."),
             paste("Table", table.count<<-table.count+1,"shows the members in each cluster from SOM analysis.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c( 
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$som,"}", sep=""),
    "\\caption{SOM cluster analysis. The x-axes are features and y-axes are relative",
    "intensities. The blue lines represent median intensities of corresponding clusters}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$som,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "GetAllSOMClusterMembers(mSet)",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create Kmeans partitional clustering document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreateKMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$kmeans)){
    return();
  }
  
  descr <- c("\\subsection{K-means Clustering}\n",
             "K-means clustering is a nonhierarchical clustering technique.",
             "It begins by creating k random clusters (k is supplied by user).",
             "The program then calculates the mean of each cluster.",
             "If an observation is closer to the centroid of another cluster",
             "then the observation is made a member of that cluster. This process is",
             "repeated until none of the observations are reassigned to a different cluster.",
             "\n\n",
             "K-means analysis is performed using the \\texttt{kmeans} function in the",
             "package \\texttt{stat}.",
             paste("Figure", fig.count<<-fig.count+1,"shows clustering the results."),
             paste("Table", table.count<<-table.count+1,"shows the members in each cluster from K-means analysis.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$kmeans,"}", sep=""),
    "\\caption{K-means cluster analysis. The x-axes are variable indices and y-axes",
    "are relative intensities. The blue lines represent median intensities of corresponding clusters}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$kmeans,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
  cmdhist2<-c("<<echo=false, results=tex>>=",
              "GetAllKMClusterMembers(mSet)",
              "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create Random Forest document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateRFdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$rf)){
    return();
  }
  
  descr <- c("\\subsection{Random Forest (RF) }\n",
             "Random Forest is a supervised learning algorithm suitable for high dimensional data analysis.",
             "It uses an ensemble of classification trees, each of which is grown by random feature",
             "selection from a bootstrap sample at each branch. Class prediction is based on the",
             "majority vote of the ensemble. RF also provides other useful information such as OOB",
             "(out-of-bag) error, variable importance measure, and outlier measures. During tree construction, about",
             "one-third of the instances are left out of the bootstrap sample. This OOB data",
             "is then used as test sample to obtain an unbiased estimate of the classification",
             "error (OOB error). Variable importance is evaluated by measuring the increase of the",
             "OOB error when it is permuted. The outlier measures are based on the proximities during tree construction.",
             "\n\n",
             "RF analysis is performed using the \\texttt{randomForest} package\\footnote{Andy Liaw and",
             "Matthew Wiener. \\textit{Classification and Regression by randomForest}, 2002, R News}.",
             paste("Table", table.count<<-table.count+1,"shows the confusion matrix of random forest."),
             paste("Figure", fig.count<<-fig.count+1,"shows the cumulative error rates of random forest analysis for given parameters.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the important features ranked by random forest.\n"),
             paste("Figure", fig.count<<-fig.count+1,"shows the outlier measures of all samples for the given parameters.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.cls,"}", sep=""),
    "\\caption{Cumulative error rates by Random Forest classification. The overall error rate is shown
    as the black line; the red and green lines represent the error rates for each class.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$rf.cls,"}", sep=""),
    "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c("<<echo=false, results=tex>>=",
               "GetRFConf.Table(mSet)",
               "@",
               paste("The OOB error is ", GetRFOOB(mSetObj))
  );
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c( "\n\n",
                "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.imp,"}", sep=""),
                "\\caption{Significant features identified by Random Forest. The features are ranked by the mean
                decrease in classification accuracy when they are permuted.}",
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$rf.imp,"}", sep=""),
                "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  
  cmdhist <- c( "\n\n",
                "\\begin{figure}[htp]",
                "\\begin{center}",
                paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$rf.outlier,"}", sep=""),
                "\\caption{Potential outliers identified by Random Forest. Only the top five are labeled.}",
                "\\end{center}",
                paste("\\label{",mSetObj$imgSet$rf.outlier,"}", sep=""),
                "\\end{figure}"
  );
  
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create R-SVM document
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateSVMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$svm)){
    return();
  }
  
  descr <- c("\\subsection{Support Vector Machine (SVM)}\n",
             "SVM aims to find a nonlinear decision function in the input space by mapping the data into a",
             "higher dimensional feature space and separating it there by means of a maximum margin hyperplane.",
             "The SVM-based recursive feature selection and classification is performed using the \\texttt{R-SVM}",
             "script\\footnote{http:\\slash\\slash www.hsph.harvard.edu\\slash bioinfocore\\slash RSVMhome\\slash R-SVM.html}.",
             "The process is performed recursively using decreasing series of feature subsets (\\texttt{ladder})",
             "so that different classification models can be calculated. Feature importance is evaluated based on",
             "its frequencies being selected in the best classifier identified by recursive classification and cross-validation.",
             "Please note, R-SVM is very computationally intensive. Only the top 50 features (ranked by their p values from t-tests)",
             "will be evaluated.",
             "\n\n",
             "In total,", length(mSetObj$analSet$svm$ladder), "models (levels) were created using",paste(mSetObj$analSet$svm$ladder, collapse=', '), "selected feature subsets.",
             paste("Figure", fig.count<<-fig.count+1, "shows the SVM classification performance using recursive feature selection."),
             paste("Figure", fig.count<<-fig.count+1, "shows the signicant features used by the best classifiers.\n"));
  
  cat(descr, file=rnwFile, append=TRUE);
  
  cat("\n\n", file=rnwFile, append=TRUE);
  cmdhist <- c(
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$svm.class,"}", sep=""),
    "\\caption{Recursive classification with SVM. The red circle indicates the best classifier.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$svm.class,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$svm,"}", sep=""),
    "\\caption{Significant features identified by R-SVM. Features are ranked by their frequencies of being selected in the classifer.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$svm,"}", sep=""),
    "\\end{figure}"
  ); 
  cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateStatFooter <- function(){
  cat("\\vspace{5 mm}\n", file=rnwFile, append=TRUE);
  cat("--------------------------------", file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  end <- c("The report was generated on \\Sexpr{date()} with \\Sexpr{print(version$version.string)}.\n");
  cat(end, file=rnwFile, append=TRUE);
  cat("\\end{document}\n\n",file=rnwFile, append=TRUE);
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateRHistAppendix <- function(){
  
  descr <- c("\\section{Appendix: R Command History}\n");
  cat(descr, file=rnwFile, append=TRUE);
  
  if(!is.null("Rhistory.R")){
    
    cmdhist<-c("<<echo=false, results=verbatim>>=",
               "GetRCommandHistory(mSet);",
               "@"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateFooter <- function(){
  
  end <- c("\\vspace{5 mm}\n--------------------------------\n\n",
           "The report was generated on \\Sexpr{date()} with \\Sexpr{print(version$version.string)}.\n",
           "\\end{document}\n\n"
  );
  cat(end, file=rnwFile, append=TRUE);
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Create a summary table for each type of uploaded data
#'csv table has 5 col: sampleID, feature #, zero,  missing #
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CreateSummaryTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  suppressMessages(library(xtable));
  sum.dat<-NULL;
  plenth<-dim(mSetObj$dataSet$procr)[2];
  if(mSetObj$dataSet$type=='conc'| mSetObj$dataSet$type=='pktable'| mSetObj$dataSet$type=='specbin'){
    for(i in 1:nrow(mSetObj$dataSet$orig)){
      srow<-mSetObj$dataSet$orig[i,];
      newrow<-c(sum(srow[!is.na(srow)]>0), (sum(is.na(srow)) + sum(srow[!is.na(srow)]<=0)), plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Features (positive)","Missing/Zero","Features (processed)");
    rownames(sum.dat)<-row.names(mSetObj$dataSet$orig);
  }else if(mSetObj$dataSet$type=="nmrpeak"| mSetObj$dataSet$type=="mspeak"){ # peak list
    pkSet<-mSetObj$dataSet$peakSet;
    snames<-pkSet$sampnames;
    for(i in 1:length(snames)){
      samp.inx<-pkSet$peaks[,"sample"]==i;
      srow<-mSetObj$dataSet$orig[i,];
      newrow<-c(sum(samp.inx),(sum(is.na(srow)) + sum(srow[!is.na(srow)]<=0)), plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Peaks (raw)","Missing/Zero", "Peaks (processed)");
    rownames(sum.dat)<-row.names(mSetObj$dataSet$orig);
  }else{ # spectra
    rawxset<-mSetObj$dataSet$xset.orig;
    fillxset<-mSetObj$dataSet$xset.fill;
    snames<-row.names(rawxset@phenoData)
    
    for(i in 1:length(snames)){
      rawno<-sum(rawxset@peaks[,"sample"]==i);
      fillno<-sum(fillxset@peaks[,"sample"]==i);
      newrow<-c(rawno,fillno,plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Peaks (raw)","Peaks (fill)", "Peaks(processed)");
    rownames(sum.dat)<-row.names(mSetObj$dataSet$orig);
  }
  print(xtable(sum.dat, caption="Summary of data processing results"), caption.placement="top", size="\\scriptsize");
}

