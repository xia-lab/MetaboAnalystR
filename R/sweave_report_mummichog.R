#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateMummichogRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateMummichogIntro();
  
  CreateMummichogOverview();
  CreateMummichogInputDoc(mSetObj);
  CreateMummichogAnalysisDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create mummichog analysis report: Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogIntro <- function(){
  descr <- c("\\section{Background}\n",
             "Understanding the functional importance of metabolites in untargeted metabolomics is limited due to challenges with metabolite ",
             "identication. to reduce problems associated with compound misidentification and thereby pathway misinterpretation is to shift the unit of analysis from individual", 
             " compounds to individual pathways. In particular, the mummichog algorithm (5) offers an elegant and efficient implementation of this concept.",
             " Mummichog bypasses the bottleneck of metabolite identification prior to pathway analysis by leveraging a priori pathway and network knowledge",
             " to directly infer biological activity based on MS peaks. Due of its popularity and repeated user requests, we have implemented",
             " the mummichog algorithm (version 1.0.10) from Li et al. 2013, which has been carefully translated from the Python programming ",
             "language to R, and includes a expanded knowledgebase of 21 organisms for pathway analysis. In particular, this module ",
             "by-passes the bottle-neck of metabolite identification prior to pathway analysis, leveraging a priori knowledge from",
             " genome-scale metabolic models and KEGG metabolic pathways. For instance, conventional approaches require statistically significant metabolites to be identified ",
             "prior to pathway analysis, which can be incredibly time-consuming, whereas here, m/z features are used in conjuction with metabolic models/pathways ",
             "to directly infer pathway level knowledge from a m/z features. For further details, please refer to Li et al. 2013 (PMC3701697).\n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create Mummichog analysis report: Overview
#'@description Report generation using Sweave
#'Mummichog analysis report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogOverview <- function(){
  descr <- c("\\section{Overview}\n",
             "The MS Peaks to Pathways module consists of three steps - uploading the user's data, selection of a pathway library,",
             " and pathway analysis. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create Mummichog analysis report: Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The MS Peaks to Pathways module accepts either a list of significant m/z features, a list of all m/z features, or a peak intensity",
             " table. The format of the data must be specified, identifying whether the samples are in rows or columns, and whether",
             " or not the data is paired. The data may either be .csv or .txt files.",
             " \n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- c("\\subsubsection{MS Peaks to Pathways: Reading Data}\n",
             "The data must be uploaded as a three column table containing the m/z features, p-values, and statistical scores (e.g. t-scores or fold-change values).",
             "\n");
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  cat(mSetObj$msgSet$read.msg, file=rnwFile, append=TRUE, sep="\n");
  
  if(!is.null(mSetObj$dataSet$instrument)){
    
    descr <- c("\\subsubsection{Parameters}\n",
               "Users also need to specify the mass accuracy, the ion mode (positive or negative), and the p-value cutoff", 
               " to delineate between significantly enriched and non-significantly enriched m/z features. ", 
               "Currently, MetaboAnalyst 4.0 only supports the handling of peaks obtained from high-resolution MS instruments", 
               " such as Orbitrap, or Fourier Transform (FT)-MS instruments as recommended by the original mummichog implementation.",
               "\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    mum.descr <- paste("The selected mass accuracy of your MS instrument in ppm is: ",mSetObj$dataSet$instrument, "; ",
                       "The selected mode of your MS instrument is: ",mSetObj$dataSet$mode , "; ",
                       "The selected p-value cutoff is: ",mSetObj$dataSet$cutoff , ".", sep = "");
    
    cat(mum.descr, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  if(!is.null(mSetObj$lib.organism)){
    
    descr <- c("\\subsubsection{Library}\n",
               "The knowledge-base for this module consists of five genome-scale metabolic models obtained", 
               " from the original Python implementation which have either been manually curated or downloaded from BioCyc,", 
               " as well as an expanded library of 21 organisms derived from KEGG metabolic pathways. ",
               "Users must select one of 21 KEGG pathway libraries, or one of five metabolic models.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    mum.descr <- paste("The user's selected library is: ", mSetObj$lib.organism, ".");
                       
    cat(mum.descr, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create Mummichog report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  suppressMessages(library(xtable))
  mummitable <- mSetObj$mummi.resmat
  print(xtable(mummitable, caption="Results of the mummichog pathway analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@author Jasmine Chong 
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogAnalysisDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Output}\n",
             "The aim of of this module is to leverage the power of known metabolic models/pathways to gain functional insight ",
             "directly from m/z features. There are three steps in this module, 1) Permutations: A list of metabolites (the same length as the number ",
             "of significant m/z features) are inferred from the user's uploaded set of m/z features, considering all potential matches ",
             "(isotopes/adducts). These tentative compounds are then mapped onto known metabolic pathways for the selected organism. ",
             "For each pathway, a fisher's exact or hypergeometric p-value is calculated.",
             " 2) Step 1 is repeated multiple times to calculate the null distribution of p-values for all pathways, and ",
             "is modeled as a Gamma distribution. 3) Following this, the significant m/z features are used to calculate the p-values for each pathway (Step 1).",
             " These p-values are then adjusted for the permutations. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- c("\\section{Pathway Analysis Results Table}\n",
             "The output of the MS Peaks to Pathways module consists of a table of results containing ranked pathways that are enriched in ",
             "the user-uploaded data. The table includes the total number of hits, their raw p-values (Fisher's exact test or ",
             "Hypergeometric), their EASE score, and the p-value modeled on user data using a Gamma distribution. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  univROCtable<-c("<<echo=false, results=tex>>=",
                  "CreateMummichogAnalTable(mSet)",
                  "@");
  cat(univROCtable, file=rnwFile, append=TRUE, sep="\n");

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("\\section{MS Peaks to Pathway Output: Compound Matching Table}\n",
             "The output of the MS Peaks to Pathways module also consists of a comprehensive table containing the compound matching", 
             " information for all user-uploaded m/z features. The table has four columns, containing the Query.Mass of each feature, the predicted Matched.Compound for each feature,",
             "the Matched.Form, and the Mass.Diff. As the file can be very long (>40 pages), please download it separately on the Downloads page of MetaboAnalyst. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}
