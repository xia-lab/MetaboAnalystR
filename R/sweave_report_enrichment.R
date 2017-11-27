#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

#' write .Rnw file template
CreateEnrichRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateEnrichIntr();
  
  CreateEnrichOverview();
  CreateEnrichInputDoc(mSetObj);
  CreateEnrichProcessDoc(mSetObj);
  CreateEnrichAnalDoc();
  
  if(mSetObj$analSet$type == "msetora"){
    CreateEnrichORAdoc(mSetObj);
  }else if(mSetObj$analSet$type == "msetssp"){
    CreateEnrichSSPdoc(mSetObj);
    CreateEnrichORAdoc(mSetObj);
  }else{
    CreateEnrichQEAdoc(mSetObj);
  }
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report introduction
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichIntr <- function(){
  descr <- c("\\section{Background}\n",
             "MSEA or Metabolite Set Enrichment Analysis is a way to identify biologically meaningful",
             "patterns that are significantly enriched in quantitative metabolomic data. In conventional",
             "approaches, metabolites are evaluated individually for their significance under conditions",
             "of study. Those compounds that have passed certain sigificance level are then combined to",
             "see if any meaningful patterns can be discerned. In contrast, MSEA directly investigates if",
             "a set of functionally related metabolites without the need to preselect compounds based on",
             "some arbituary cut-off threshold. It has the potentail to identify subtle but consistent changes",
             "among a group of related compounds, which may go undetected with the conventional approaches.",
             "\n\n",
             "Essentially, MSEA is a metabolomic version of the popular GSEA (Gene Set Enrichment Analysis)",
             "software with its own collection of metabolite set libraries as well as an implementation of",
             "user-friendly web-interfaces. GSEA is widely used in genomics data analysis and has proven to",
             "be a powerful alternative to conventional approaches. For more information, please refer to",
             "the original paper by Subramanian A, and a nice review paper by Nam D, Kim SY.", 
             "\\footnote{Subramanian\\textit{Gene set enrichment analysis: A knowledge-based approach for interpreting",
             "genome-wide expression profiles.}, Proc Natl Acad Sci USA. 2005 102(43): 15545-50}.",
             "\\footnote{Nam D, Kim SY. \\textit{Gene-set approach for expression pattern analysis},",
             "Briefings in Bioinformatics. 2008 9(3): 189-197.}\n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report overview
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CreateEnrichOverview <- function(){
  descr <- c("\\section{MSEA Overview}\n",
             "Metabolite set enrichment analysis consists of four steps - data input, data processing,",
             "data analysis, and results download. Different analysis procedures are performed based on",
             "different input types. In addition, users can also browse and search the metabolite set",
             "libraries as well as upload their self-defined metabolite sets for enrichment analysis.",
             "Users can also perform metabolite name mapping between a variety of compound names, synonyms,",
             "and major database identifiers."
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report data input
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "There are three enrichment analysis algorithms offered by MSEA. Accordingly, three",
             "different types of data inputs are required by these three approaches:\n"
  );
  
  cat(descr, file=rnwFile, append=TRUE);
  descr <- c(
    "\\begin{itemize}",
    "\\item{A list of important compound names - entered as a one column data",
    "(\\textit{Over Representation Analysis (ORA)});}",
    "\\item{A single measured biofluid (urine, blood, CSF) sample- entered as",
    "tab separated two-column data with the first column for compound name,",
    "and the second for concentration values",
    "(\\textit{Single Sample Profiling (SSP)});}",
    "\\item{A compound concentration table - entered as a comma separated (.csv) file",
    "with the each sample per row and each metabolite concentration per column.",
    "The first column is sample names and the second column for sample phenotype labels",
    "(\\textit{Quantitative Enrichment Analysis (QEA)})}",
    "\\end{itemize}",
    "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  if(mSetObj$analSet$type == "msetora"){
    descr <- c("You selected Over Representation Analysis (ORA) which requires a list of compound",
               "names as input. \n\n"
    );
  }else if(mSetObj$analSet$type == "msetssp"){
    descr <- c("You selected Single Sample Profiling (SSP) which requires a two-column data from",
               "measurement of a single biofluid sample. Currently, only data from blood, urine and CSF",
               "can be analysed as restricted by the availability of reference concentration data. \n\n"
    );
  }else{
    descr <- c("You selected Quantitative Enrichment Analysis (QEA) which requires a concentration table.",
               "This is the most common data format generated from quantitative metabolomcs studies.",
               "The phenotype label can be can be discrete (binary or multi-class) or continuous. \n\n"
    );
  }
  
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report enrichment process
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichProcessDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Process}\n",
             "The first step is to standardize the compound labels. It is an essential step since the compound",
             "labels will be subsequently compared with compounds contained in the metabolite set library.",
             "MSEA has a built-in tool to convert between compound common names, synonyms, identifiers used in",
             "HMDB ID, PubChem, ChEBI, BiGG, METLIN, KEGG, or Reactome.",
             "\\textbf{Table 1} shows the conversion results. Note: \\textit{1} indicates exact match, \\textit{2}",
             "indicates approximate match, and \\textit{0} indicates no match. A text file contain the result can be",
             "found the downloaded file \\textit{name\\_map.csv}\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("<<echo=false, results=tex>>=",
             "GetMapTable(mSet)",
             "@",
             "\\clearpage\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("The second step is to check concentration values. For SSP analysis,",
             "the concentration must be measured in \\textit{umol} for blood and CSF samples.",
             "The urinary concentrations must be first converted to \\textit{umol/mmol\\_creatinine}",
             "in order to compare with reported concentrations in literature. No missing or negative values",
             "are allowed in SSP analysis. The concentration data for QEA analysis is more flexible.",
             "Users can upload either the original concentration data or normalized data. Missing or negative values",
             "are allowed (coded as \\textit{NA}) for QEA."
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, analysis
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichAnalDoc <- function(){
  
  descr <- c("\\section{Selection of Metabolite Set Library}\n",
             "Before proceeding to enrichment analysis, a metabolite set library has to be chosen.",
             "There are seven built-in libraries offered by MSEA:\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c(
    "\\begin{itemize}",
    "\\item{Metabolic pathway associated metabolite sets",
    "(\\textit{currently contains 88 entries});}",
    "\\item{Disease associated metabolite sets (reported in blood)",
    "(\\textit{currently contains 416 entries});}",
    "\\item{Disease associated metabolite sets (reported in urine)",
    "(\\textit{currently contains 346 entries})}",
    "\\item{Disease associated metabolite sets (reported in CSF)",
    "(\\textit{currently contains 124 entries})}",
    "\\item{Metabolite sets associated with SNPs",
    "(\\textit{currently contains 4500 entries})}",
    "\\item{Predicted metabolite sets based on computational enzyme knockout model",
    "(\\textit{currently contains 912 entries})}",
    "\\item{Metabolite sets based on locations",
    "(\\textit{currently contains 57 entries})}",
    "\\end{itemize}",
    "\n\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("In addition, MSEA also allows user-defined metabolite sets to be uploaded to perform",
             "enrichment analysis on arbitrary groups of compounds which researchers want to test.",
             "The metabolite set library is simply a two-column comma separated text file with the",
             "first column for metabolite set names and the second column for its comound names (\\textbf{must use HMDB compound name})",
             "separated by \"; \". Please note, the",
             "built-in libraries are mainly from human studies. The functional grouping of metabolites",
             "may not be valid. Therefore, for data from subjects other than human being, users are",
             "suggested to upload their self-defined metabolite set libraries for enrichment analysis."
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  cat("\\section{Enrichment Analysis}\n",file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, over
#'representation analysis (ORA)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichORAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c(
    "Over Representation Analysis (ORA) is performed when",
    "a list of compound names is provided. The list of compound list can be obtained through",
    "conventional feature selection methods, or from a clustering algorithm,",
    "or from the compounds with abnormal concentrations detected in SSP,",
    "to investigate if some biologically meaningful patterns can be identified.",
    "\n\n",
    "ORA was implemented using the \\textit{hypergeometric test} to evaluate whether a particular",
    "metabolite set is represented more than expected by chance within the given compound list.",
    "One-tailed p values are provided after adjusting for multiple testing. \\textbf{Figure 2} below",
    "summarizes the result."
  );
  cat(descr, file=rnwFile, append=TRUE);
  fig <- c(  "\\begin{figure}[htp]",
             "\\begin{center}",
             paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$ora,"}",sep=""),
             "\\caption{Summary Plot for Over Representation Analysis (ORA)}",
             "\\end{center}",
             paste("\\label{",mSetObj$imgSet$ora,"}", sep=""),
             "\\end{figure}",
             "\\clearpage\n\n"
  );
  cat(fig, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("<<echo=false, results=tex>>=",
             "GetORATable(mSet)",
             "@",
             "\\clearpage\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Single sampling profiling
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichSSPdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c(
    "Single Sample Profiling (SSP) is designed to detect whether certain compound",
    "concentrations measured from a particular sample are higher or lower compared",
    "to their normal ranges reported in literature. Those selected",
    "compound list are then subject to over representation analysis (ORA) to see",
    "if certain biologically meaningful patterns can be identified. Please note,",
    "those reference concentrations could be measured from heterogeneous analytical",
    "technologies. It is advisable to compare only to those values that were",
    "measured by similar technologies by referring to the original literature.",
    "By default, if measured concentration values are higher or lower",
    "than \\textbf{all} those reported concentration ranges will be selected for",
    "enrichment analysis. Users can overwrite the default selections by manually",
    "selecting or de-selecting them.",
    "\n\n",
    "\\textbf{Table 2} shows the comparison between the measured concentrations",
    "and the reference concentrations. \\textit{L, M, H} means to the",
    "measured concentration are \\textit{Lower, Within (Medium), Higher} compared",
    "to the reference values. \\textit{0} means not selected for",
    "subsequent enrichment analysis, while \\textit{1} means the corresponding",
    "compound was selected."
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n")
  descr<-c("<<echo=false, results=tex>>=",
           "GetSSPTable(mSet)",
           "@",
           "\\clearpage\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Quantitative enrichment analysis
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreateEnrichQEAdoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c(
    "Quantitative enrichment analysis (QEA) will be performed when the user uploads",
    "a concentration table. The enrichment analysis is performed using package",
    "\\textbf{globaltest} \\footnote{Jelle J. Goeman, Sara A. van de Geer, Floor de Kort",
    "and Hans C. van Houwelingen.\\textit{A global test for groups of genes: testing",
    "association with a clinical outcome}, Bioinformatics Vol. 20 no. 1 2004, pages 93-99}.",
    "It uses a generalized linear model to estimate a",
    "\\textit{Q-statistic} for each metabolite set, which describes the correlation",
    "between compound concentration profiles, X, and clinical outcomes, Y. The \\textit{Q statistic}",
    "for a metabolite set is the average of the Q statistics for each metabolite in the set.",
    "\\textbf{Figure 2} below summarizes the result."
  );
  cat(descr, file=rnwFile, append=TRUE);
  fig <- c(  "\\begin{figure}[htp]",
             "\\begin{center}",
             paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$qea, "}", sep=""),
             "\\caption{Summary plot for Quantitative Enrichment Analysis (QEA).}",
             "\\end{center}",
             paste("\\label{",mSetObj$imgSet$qea, "}", sep=""),
             "\\end{figure}",
             "\\clearpage\n\n"
  );
  cat(fig, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("<<echo=false, results=tex>>=",
             "GetQEATable(mSet)",
             "@",
             "\\clearpage\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}