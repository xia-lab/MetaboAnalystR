#'Create report for raw spectra module
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CreateStatRnwReport <- function(mSetObj, usrName){
  
  load("mSet.rda");
  # Creat the header & introduction of the report
  CreateHeader(usrName); # done
  CreateStatIntr(); # done
  
  CreateSpectraIOdoc(mSetObj); # done

  InitStatAnalMode();
  if(exists("analSet", where = mSetObj)){
    CreateUNIVdoc(mSetObj);
    CreateANOVAdoc(mSetObj);
    CreateCorrDoc(mSetObj);
    CreatePCAdoc(mSetObj);
    CreatePLSdoc(mSetObj);
    CreateOPLSDAdoc(mSetObj);
    CreateSPLSDAdoc(mSetObj);
  }else{
    CreateAnalNullMsg();
  }
  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create header
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CreateStatIntr <- function(){
  descr <- c("\\section{Raw Spectra Processing}\n",
             "This module is designed to provide an automated workflow to process the raw spectra. 5 steps including parameters ",
             "optimization/custimization, peak picking, peak alignment, peak gap filing and peak annotation.");
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Read and process raw data
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateSpectraIOdoc <- function(mSetObj=NULL){
  
  if(missing(mSetObj) | is.null(mSetObj)){
    load("mSet.rda");
  }
  
  descr <- c("\\subsection{Reading and Processing the Raw Data}\n",
             "MetaboAnalyst MS Spectral Processing Module accepts several common MS formats",
             "including mzXML, mzML, mzData. But all of them have to be centroided before processing.",
             "The Data Integrity Check is performed before the data processing starts. The basic information",
             "of all spetra is summaried in Table", table.count<<-table.count+1,"shows the details of all spectra."
  );
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cmdhist2 <- c("<<echo=false, results=tex>>=",
                "CreateSpectraInfoTable()",
                "@");
  cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}



#' CreateSpectraInfoTable
#'
#' @return
#' @noRd
#' @noMd
CreateSpectraInfoTable <- function(){
  
  if(file.exists("mSet.rda")){
    load("mSet.rda");
    
    filesize <- file.size(fileNames(mSet@rawOnDisk))/(1024*1024);
    filename <- basename(fileNames(mSet@rawOnDisk));
    Centroid <- rep("True",length(filename));
    GroupInfo <- as.character(mSet@rawOnDisk@phenoData@data[["sample_group"]]);
    
    sum.dat<-matrix(ncol = 4, nrow = length(filename));
    
    sum.dat[,1] <- filename;
    sum.dat[,2] <- Centroid;
    sum.dat[,3] <- round(filesize,2);
    sum.dat[,4] <- GroupInfo;
    
    colnames(sum.dat)<-c("Spectra","Centroid", "Size", "Group");
    print(xtable::xtable(sum.dat, 
                         caption="Summary of data processing results"), 
          caption.placement="top", 
          size="\\scriptsize");
  } else {
    # If processing is not done. Just read the data infomation directly.
    if(dir.exists("upload")){
      files <- list.files("upload", full.names = T, recursive = T)
      
      snames <- gsub("\\.[^.]*$", "", basename(files))
      sclass <- gsub("^\\.$", "sample", dirname(files))
      
      scomp <- strsplit(substr(sclass, 1, min(nchar(sclass))), "", fixed = TRUE)
      scomp <- matrix(c(scomp, recursive = TRUE), ncol = length(scomp))
      
      i <- 1
      while (all(scomp[i, 1] == scomp[i, -1]) && i < nrow(scomp)) {
        i <- i + 1
      }
      
      i <-
        min(i, tail(c(0, which(
          scomp[1:i, 1] == .Platform$file.sep
        )), n = 1) + 1)
      
      if (i > 1 && i <= nrow(scomp)) {
        sclass <- substr(sclass, i, max(nchar(sclass)))
      }
      
      if (.on.public.web &
          unique(sclass)[1] == "upload" &
          length(unique(sclass)) == 1) {
        sclass <- rep("Unknown", length(sclass))
      }
      
      fileFullName <- list.files("upload", recursive = T, full.names = T);
      sum.dat<-matrix(ncol = 4, nrow = length(fileFullName));
      
      sum.dat[,1] <- basename(fileFullName);
      sum.dat[,2] <- unname(sapply(fileFullName, function(x){CentroidCheck(x)}));
      sum.dat[,3] <- round(file.size(fileFullName),2);
      sum.dat[,4] <- sclass;
      
    } else {
      #do nothing
    }
  }
  
}

#'Introduction for statistical analysis module report
#'Initialize Statistical Analysis Report
#'@export
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


#'Create null message for analysis
#'Creates a message for the Sweave report
#'@description Creates a message stating that no analyses were performed on your data.
#'@export
CreateAnalNullMsg <- function(){
  descr <- c("No analysis was performed on your data.\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create univariate analyses document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateUNIVdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$fc) & is.null(mSetObj$analSet$tt) & is.null(mSetObj$analSet$volcano)){
    return();
  }
  
  if(isEmptyMatrix(mSetObj$analSet$fc$sig.mat)){
    fc.img <- fc.tab<-NULL;
  }else{
    fc.img <- paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by fold change analysis.");
    fc.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }
  if(isEmptyMatrix(mSetObj$analSet$tt$sig.mat)){
    tt.img <- tt.tab<-NULL;
  }else{
    tt.img <- paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by t-tests.");
    tt.tab<-paste("Table", table.count<<-table.count+1,"shows the details of these features;");
  }
  if(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat)){
    volcano.img <- volcano.tab<-NULL;
  }else{
    volcano.img <-paste("Figure", fig.count<<-fig.count+1,"shows the important features identified by volcano plot.");
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
             fc.img,
             fc.tab,
             tt.img,
             tt.tab,
             volcano.img,
             volcano.tab,
             "\n\n",
             "Please note, the purpose of fold change is to compare absolute value changes between two group means.",
             "Therefore, the data before column normalization will be used instead. Also note, the result is plotted",
             "in log2 scale, so that same fold change (up/down regulated) will have the same distance to the zero baseline.",
             "\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # Fold change
  if(!(isEmptyMatrix(mSetObj$analSet$fc$sig.mat))){
    cmdhist <- c(
      "\\begin{figure}[htp]",
      "\\begin{center}",
      paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$fc,"}", sep=""),
      "\\caption{", paste("Important features selected by fold-change analysis with threshold ", mSetObj$analSet$fc$raw.thresh, ". ",
                          "The red circles represent features above the threshold. Note the values are on log scale, so that both up-regulated ",
                          "and down-regulated features can be plotted in a symmetrical way", sep=""), "}",
      "\\end{center}",
      paste("\\label{",mSetObj$imgSet$fc,"}", sep=""),
      "\\end{figure}"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
    cat("\n\n", file=rnwFile, append=TRUE, sep="\n");
    
    cmdhist2 <- c("<<echo=false, results=tex>>=",
                  "GetSigTable.FC(mSet)",
                  "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  # T-tests
  if(!(isEmptyMatrix(mSetObj$analSet$tt$sig.mat))){
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
    
    cmdhist2<-c("<<echo=false, results=tex>>=",
                "GetSigTable.TT(mSet)",
                "@");
    cat(cmdhist2, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  # Volcano plot
  if(!(isEmptyMatrix(mSetObj$analSet$volcano$sig.mat))){
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
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
    paste("\\includegraphics[width=1.0\\textwidth]{", "scores3D.png","}", sep=""),
    "\\caption{3D score plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{","scores3D.png","}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", "loadings3D.png","}", sep=""),
    "\\caption{Loadings plot for the selected PCs. }",
    "\\end{center}",
    paste("\\label{","loadings3D.png","}", sep=""),
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
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
             "in each dimension. Please note, VIP scores are calculated for each components. When more than components are used to calculate", 
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
  
  if(!is.null(mSetObj$imgSet$pls.permut)){ # may not be performed (not by default)
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
  }
  
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateSPLSDAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$analSet$splsr)){
    return();
  }
  
  descr <- c("\\subsection{Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)}\n",
             "The sparse PLS-DA (sPLS-DA) algorithm can be used to effectively reduce the number of variables (metabolites)", 
             "in high-dimensional metabolomics data to produce robust and easy-to-interpret models.", 
             "Users can control the sparseness of the model by controlling the number of components in the model and the number ",
             "of variables in each component. For more information, please refer to Cao et al. 2011 (PMC3133555). ",
             "\n\n",
             paste("Figure", fig.count<<-fig.count+1,"shows the overview of scores plots;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 2-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the loading plot of the top ranked features;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the 3-D scores plot between selected components;"),
             paste("Figure", fig.count<<-fig.count+1,"shows the performance of the sPLS-DA model evaluated using cross-validations;"));
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
    "\\caption{3D scores plot between the selected PCs. The explained variances are shown in brackets.}",
    "\\end{center}",
    paste("\\label{",mSetObj$imgSet$pls.score3d,"}", sep=""),
    "\\end{figure}",
    "\\begin{figure}[htp]",
    "\\begin{center}",
    paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$spls.imp,"}", sep=""),
    "\\caption{Plot showing the variables selected by the sPLS-DA model for a given component. The variables are ranked by the absolute values of their loadings.}",
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
