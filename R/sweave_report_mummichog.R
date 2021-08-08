#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateMummichogRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  
  if(exists("metaFiles")){
    CreateMummichogMetaAnalReport(mSetObj)
  }else{
    CreateMummichogIntro();
    CreateMummichogOverview();
    CreateMummichogInputDoc(mSetObj);
    CreateMummichogAnalysisDoc(mSetObj);
  }
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create analysis report: Functional Meta-Analysis
#'@description Report generation using Sweave
#'Functional Meta-Analysis Report 
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogMetaAnalReport<-function(mSetObj){
  CreateMetaMummichogIntro()
  CreateMetaMummichogInputDoc(mSetObj)
  CreateMetaMummichogResults(mSetObj)
}

#'Create analysis report: Functional Meta-Analysis Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaMummichogIntro <- function(){
  
  back.descr <- c("\\section{Background}\n",
                  "Metabolomics is becoming an increasingly popular tool for biological research, however while individual studies may identify certain biological functions to be important,", 
                  " such results may not be reproducible in other independent studies researching the same biological question due to low sample size, sample heterogeneity,",
                  " the type of LC-MS platform used, or even metrics used for interpreting results. Meta-analysis, which is the combination of findings from independent studies,", 
                  " can be used to overcome such limitations and ultimately increase the power, precision, and generalizability of a single study.",
                  "With the increased use of metabolomics and global efforts towards science transparency and reproducibility, the amount of publicly available metabolomics data deposited in", 
                  " dedicated metabolomics repositories such as The Metabolomics Workbench, MetaboLights and OmicsDI has grown tremendously. The potential for researchers in the", 
                  " metabolomics community to enhance their findings with publicly available data is immense, but little effort has been applied for the meta-analysis of untargeted", 
                  " metabolomics data. Such a method would also reduce the bias individual studies may carry towards specific sample processing protocols or LC-MS instruments. \n"
  );
  cat(back.descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  mum.descr <- c("\\section{Algorithm Overview}\n",
                 "High-resolution LC-MS has become dominant platform for global metabolomics. The typical LC-MS workflow starts with data acquisition, followed by data pre-processing,", 
                 " feature selection, compound identification and then pathway analysis. However, peak identification requires significant effort and is a major challenge for downstream", 
                 " functional interpretation. One strategy is to completely bypass the bottleneck of metabolite identification and directly infer functional activity from MS peaks by leveraging", 
                 " the collective knowledge of metabolic pathways and networks. This is the idea of the mummichog algorithm (Li et al. 2013). Using this algorithm, a list of significant peaks,", 
                 " are matched to potential compounds using their molecular weight considering different adducts/isotopes. These putatively matched compounds are then mapped onto known biological pathways.", 
                 "If a list of truly significant features reflect biological activity, then the representation of these true metabolites would be enriched on functional structures,", 
                 " while false matches would be distributed at random. We have extended this idea to support the meta-analysis of untargeted metabolomics data. \n"
                 
  );
  cat(mum.descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  meta.descr <- c("\\section{Functional Meta-Analysis Workflow}\n",
                 "The Functional Meta-Analysis module supports meta-analysis at either i) the pathway level or ii) by pooling all MS peaks. Each method of integration has their own merits,", 
                 " for instance at the pathway level, compounds do not have to be matched across all studies whereas pooling all MS peaks will better bring out weaker signals.",  
                 "The workflow also considers data heterogeneity, meaning that the accuracy of the LC-MS instrument and the MS ionization mode are taken into account when performing putative", 
                 " metabolite annotation. Ultimately, the goal of this workflow is to enable the reuse/repurposong of multiple datasets to identify a robust meta-signature for the phenotype in question. \n"
                 
  );
  cat(meta.descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  descr <- c("\\section{Functional Meta-Analysis Overview}\n",
             "The Functional Meta-Analysis module consists of three steps - 1) uploading the user's data, 2) parameter selection,",
             " and 3) functional enrichment meta-analysis. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create analysis report: Functional Meta-Analysis Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaMummichogInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The Functional Meta-Analysis module accepts multiple peak tables with or without retention time information.",  
             "All inputted files must be in .csv format. \n");
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  
  sanity_msg <- vector("list")
  
  for(i in 1:length(metaFiles)){
    metaSetObj <- qs::qread(metaFiles[i]) 
    sanity_msg[metaFiles[i]] <- paste0(gsub("mummichoginput.qs", "", metaFiles[i]), ": ", paste0(metaSetObj$msgSet$check.msg, collapse = " "))
  }

  cat(unlist(sanity_msg), file=rnwFile, append=TRUE, sep="\n");
  
  if(!is.na(mSetObj$paramSet$version)){
    mum.version <- mSet$paramSet$version;
    
    descr <- c("\\subsubsection{Parameters}\n",
               "Depending on the meta-analysis method, users must select the algorithm (original mummichog or GSEA), p-value cutoff", 
               " to delineate between significantly and non-significantly enriched pathways (pathway-level only), and pathway library used.", 
               " Currently, MetaboAnalyst 5.0 only supports the handling of peaks obtained from high-resolution MS instruments", 
               " such as Orbitrap, or Fourier Transform (FT)-MS instruments as recommended by the original mummichog implementation.",
               "\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    if(mSetObj$metaLevel == "pathway"){
      if(!is.null("mSet$meta_results")){
        # pathway level
        metaLevel <- paste("The selected method for meta-analysis is 'Pathway-Level Integration'.")
        anal.descr <- paste("The selected algorithm is: ", anal.type, ".", sep = "");
        integ.desrc <- paste("The selected p-value cutoff for integrating pathways is: ", mSetObj$meta.pval.method, ".", sep = "");
        
        cat(anal.descr, file=rnwFile, append=TRUE, sep="\n");
        cat(metaLevel, file=rnwFile, append=TRUE, sep="\n");
        cat(integ.desrc, file=rnwFile, append=TRUE, sep="\n");
      }
    }else{
      # pooled
      metaLevel <- paste("The selected method for meta-analysis is 'Pooled Peaks'.")
      anal.descr <- paste("The selected algorithm is: ", anal.type, " ", mum.version, ".", sep = "");
      pval.descr <- paste("The selected p-value cutoff: ", mSetObj$pooled_cutoff, ".", sep = "");
      
      cat(metaLevel, file=rnwFile, append=TRUE, sep="\n");
      cat(anal.descr, file=rnwFile, append=TRUE, sep="\n");
      cat(pval.descr, file=rnwFile, append=TRUE, sep="\n");
    }
  }
  
  if(!is.null(mSetObj$lib.organism)){
    
    descr <- c("\\subsubsection{Pathway Library}\n",
               "The knowledge-base for this module consists of five genome-scale metabolic models obtained", 
               " from the original Python implementation which have either been manually curated or downloaded from BioCyc,", 
               " as well as an expanded library of 21 organisms derived from KEGG metabolic pathways. ",
               "Users must select one of 21 KEGG pathway libraries, or one of five metabolic models.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    mum.descr <- paste("The user's selected library is: ", gsub("_", ".", mSetObj$lib.organism), ".");
    
    cat(mum.descr, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create analysis report: Functional Meta-Analysis Results
#'@description Report generation using Sweave
#'Mummichog analysis report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaMummichogResults <- function(mSetObj){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$metaLevel == "pathway"){
    
    if(!is.null("mSet$meta_results")){
      descr <- c("\\section{Pathway-Level Integration Results}\n",
                 "The aim of the Pathway-Level Integration method is to improve the biological consistency across studies to identify a robust meta-signature for the phenotype in question.",
                 " For pathway-level integration, each individual study undergoes the steps of calculating m/z level statistics and", 
                 " putative metabolite annotation, followed by pathway activity prediction to ultimately create a unified matrix of pathway-level results (keeping only pathways found across all-studies).", 
                 " Pathway activity scores are then combined using one of several p-value integration methods. \n");
      cat(descr, file=rnwFile, append=TRUE);
    }
    
    if(!is.null(mSetObj$imgSet$mummi.meta.path.plot)){
      
      descr <- c("\\subsection{Pathway-Level Integration Plot}\n",
                 "The bubble plot below represents the results of the Pathway-Level Integration. The plot displays all matched pathways per study as circles.", 
                 "The color and size of each circle corresponds to its p-value and enrichment factor, respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant", 
                 " hits and the expected number of hits within the pathway. \n");
      cat(descr, file=rnwFile, append=TRUE);
      
      fig <- c(  "\\begin{figure}[htp]",
                 "\\begin{center}",
                 paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$mummi.meta.path.plot,"}",sep=""),
                 "\\caption{Summary of Pathway-Level Integration Meta-Analysis}",
                 "\\end{center}",
                 paste("\\label{",mSetObj$imgSet$mummi.meta.path.plot,"}", sep=""),
                 "\\end{figure}",
                 "\\clearpage\n\n"
      );
      cat(fig, file=rnwFile, append=TRUE, sep="\n");
      
      descr <- c("\\subsection{Pathway-Level Integration Meta-Analysis Results Table}\n",
                 "The output of the Pathway-Level Integration Meta-Analysis consists of a table of results containing ranked pathways that are enriched in ",
                 "the user-uploaded datasets. The table includes the raw p-values (per individual study), the mean enrichment ratio and finally the integrated p-value (Meta.P). \n");
      cat(descr, file=rnwFile, append=TRUE);
      
      pathMetatable<-c("<<echo=false, results=tex>>=",
                      "CreateMummichogMetaAnalPathTable(mSet)",
                      "@",
                      "\\clearpage\n\n");
      cat(pathMetatable, file=rnwFile, append=TRUE, sep="\n");
      cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
    }
  }else{
    
    descr <- c("\\section{Pooled Peaks Results}\n",
               "The aim of of the Pooling Peaks method is to computationally combine the outputs from different instruments that measure the same set of samples.", 
               " In this case, all uploaded peaks are merged into a single input for putative compound annotation (with consideration for different mass tolerances)", 
               " followed by pathway activity prediction. This method should be used when samples are homogeneous but instruments are complementary,",  
               " for instance samples that comes from the same lab but were obtained using different columns, extraction methods or data collected using complementary LC-MS instruments. \n");
    cat(descr, file=rnwFile, append=TRUE);
    
    CreateMummichogAnalysisDoc(mSetObj)
  }
}

#'Create analysis report: Functional Meta-Analysis Results Table
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogMetaAnalPathTable <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$meta_results
  print(xtable::xtable(mummitable, caption="Results of the Pathway-Level Integration Meta-Analysis"), caption.placement="top", size="\\scriptsize");
}

####################################################
####################################################

#'Create mummichog analysis report: Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogIntro <- function(){
  descr <- c("\\section{Background}\n",
             "Even with high mass accuracy afforded by current high-resolution MS platforms, it is often impossible to uniquely identify a given peak based on its ",
             "mass alone. To get around this issue, a key concept is to shift the unit of analysis from individual compounds to individual pathways or a group of ",
             "functionally related compounds (i.e. metabolite sets (PMID: 20457745)). The general assumption is that the collective behavior of a group is more ",
             "robust against a certain degree of random errors of individuals. The mummichog algorithm is the first implementation of this concept to infer ",
             "pathway activities from a ranked list of MS peaks identified by untargeted metabolomics. The original algorithm implements an over-representation analysis (ORA) ",
             "method to evaluate pathway-level enrichment based on significant features. Users need to specify a pre-defined cutoff based on p-values. ",
             "For further details about the original implementation, please refer to Li et al. 2013 (PMC3701697). ",
             "A complementary approach is the Gene Set Enrichment Analysis (GSEA) method, a widely used method to extract biological ",
             "meaning from a ranked gene list (PMID: 16199517). Unlike ORA, this method considers the overall ranks of MS peaks without using a significance cutoff. ", 
             "It is able to detect subtle and consistent changes which can be missed from ORA methods. Both the mummichog algorithm (Version 1.0.10), ",
             "which has been carefully translated from the Python programming to R, and the adapted GSEA method have been implemented in the MS Peaks to Paths module. ",
             "The module also includes an expanded knowledgebase of 21 organisms for pathway analysis.\n"
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The MS Peaks to Pathways module accepts either a three column table containing the m/z features, p-values, and statistical scores,",
             " a two-column table containing m/z features and either p-values or t-scores, or a one-column table ranked by either",
             " p-values or t-scores. All inputted files must be in .txt format. If the input is a three column table, both the mummichog",
             " and GSEA algorithms can be applied. If only p-values (or ranked by p-values) are provided, only the mummichog algorithm will be applied.",
             " If only t-scores (or ranked by t-scores) are provided, only the GSEA algorithm will be applied.",
             " \n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  cat("\n\n", file=rnwFile, append=TRUE);
  cat(mSetObj$msgSet$check.msg, file=rnwFile, append=TRUE, sep="\n");
  
  if(!is.null(mSetObj$dataSet$instrument)){
    
    descr <- c("\\subsubsection{Parameters}\n",
               "Users also need to specify the mass accuracy (ppm), the ion mode (positive or negative), and the p-value cutoff", 
               " to delineate between significantly enriched and non-significantly enriched m/z features (for mummichog only). ", 
               "Currently, MetaboAnalyst 5.0 only supports the handling of peaks obtained from high-resolution MS instruments", 
               " such as Orbitrap, or Fourier Transform (FT)-MS instruments as recommended by the original mummichog implementation.",
               "\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    mum.descr <- paste("The selected p-value cutoff is: ",mSetObj$dataSet$cutoff , ".", sep = "");
    
    cat(mum.descr, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  if(!is.null(mSetObj$lib.organism)){
    
    descr <- c("\\subsubsection{Library}\n",
               "The knowledge-base for this module consists of five genome-scale metabolic models obtained", 
               " from the original Python implementation which have either been manually curated or downloaded from BioCyc,", 
               " an expanded library of 21 organisms derived from KEGG metabolic pathways, and 10 distinct metabolite set libraries. ",
               "Users must select one of 21 KEGG pathway libraries, or one of five metabolic models.\n");
    cat(descr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    mum.descr <- paste("The user's selected library is: ", mSetObj$lib.organism, ".");
    
    cat(mum.descr, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  if(!is.null(mSetObj$curr.cust) && mSetObj$curr.cust){
    
    descr.cust <- c("\\subsubsection{Analysis Customization}\n",
                    "The aim of this module is to use the mummichog algorithm (Li et al. 2013) to predict pathway-level activity from untargeted", 
                    "metabolomics, bypassing the need for conventional metabolite identification prior",
                    "to functional enrichment analysis. In brief, the algorithm uses the collective power of the organizational",
                    "structure of metabolic pathways/networks and maps m/z features, including all of its adducts and forms, onto",
                    "these structures to infer activity. Here, the assumption is that if a list of significant m/z features truly reflects",
                    "biological activity, then the true metabolites will show enrichment on the pathways/networks, while falsely matched",
                    "metabolites will be more randomly distributed.\n");
    cat(descr.cust, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    descr.curr <- c("\\subsubsection{Analysis Customization: Currency Metabolites}\n",
                    "Currency metabolites are abundant substances such as water and carbon dioxide known to occur in normal",
                    " functioning cells and participate in a large number of metabolic reactions (Huss and Holme 2007, PMID 17907676). ", 
                    "Because of their ubiquitous nature, they will be removed from further analysis. There is no formal consensus of a set of currency ", 
                    "metabolites, therefore users who are unsatisfied with the default list of currency metabolites", 
                    " are provided an option to select the metabolites to use as currency.\n");
    cat(descr.curr, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
    curr.desc <- paste("The user's selected list of currency metabolites is: ", currency, ".");
    cat(curr.desc, file=rnwFile, append=TRUE, sep="\n");
    
    descr.add <- c("\\subsubsection{Analysis Customization: Adducts}\n",
                   "In addition to pathway information, the mummichog libraries contain a set of adducts tailored to the analytical mode of the MS instrument.",
                   "These options however, may not be optimal for users data, therefore users are provided the option", 
                   "to customize the adduct list used in the mummichog analysis.\n");
    cat(descr.add, file=rnwFile, append=TRUE);
    cat("\n\n", file=rnwFile, append=TRUE);
    
  }
  
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create Mummichog report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$mummi.resmat
  print(xtable::xtable(mummitable, caption="Results of the Mummichog Pathway Analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create Mummichog report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateGSEAAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$mummi.gsea.resmat
  print(xtable::xtable(mummitable, caption="Results of the GSEA Pathway Analysis"), caption.placement="top", size="\\scriptsize");
  
}

#'Create Mummichog report of analyses 
#'@description Report generation using Sweave
#'Function to create a summary table of mummichog analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMetaAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$integ.resmat
  print(xtable::xtable(mummitable, caption="Meta-Analysis of Mummichog and GSEA Results"), caption.placement="top", size="\\scriptsize");
  
}

#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong 
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateMummichogAnalysisDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$mummi.resmat)){
    
    descr <- c("\\section{Mummichog Output}\n",
               "The aim of of this module is to leverage the power of known metabolic models/pathways to gain functional insight ",
               "directly from m/z features. There are three steps for the mummichog algorithm, 1) Permutations: A list of metabolites (the same length as the number ",
               "of significant m/z features) are inferred from the user's uploaded set of m/z features, considering all potential matches ",
               "(isotopes/adducts). These tentative compounds are then mapped onto known metabolic pathways for the selected organism. ",
               "For each pathway, a hypergeometric p-value is calculated.",
               " 2) Step 1 is repeated multiple times to calculate the null distribution of p-values for all pathways, and ",
               "is modeled as a Gamma distribution. 3) Following this, the significant m/z features are used to calculate the p-values for each pathway (Step 1).",
               " These p-values are then adjusted for the permutations. \n");
    cat(descr, file=rnwFile, append=TRUE);
    
    # Because mummi.resmat is created for meta-analysis
    if(!is.null(mSetObj$imgSet$mummi.plot)){
      
      descr <- c("\\subsection{Mummichog Pathway Analysis Plot}\n",
                 "The pathway summary plot below displays all matched pathways as circles. The color and size of each circle corresponds", 
                 " to its p-value and enrichment factor, respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant", 
                 " pathway hits and the expected number of compound hits within the pathway. \n");
      cat(descr, file=rnwFile, append=TRUE);
      
      fig <- c(  "\\begin{figure}[htp]",
                 "\\begin{center}",
                 paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$mummi.plot,"}",sep=""),
                 "\\caption{Summary of Pathway Analysis}",
                 "\\end{center}",
                 paste("\\label{",mSetObj$imgSet$mummi.plot,"}", sep=""),
                 "\\end{figure}",
                 "\\clearpage\n\n"
      );
      cat(fig, file=rnwFile, append=TRUE, sep="\n");
    }
    
    descr <- c("\\subsection{Mummichog Pathway Analysis Results Table}\n",
               "The output of the mummichog analysis consists of a table of results containing ranked pathways that are enriched in ",
               "the user-uploaded data. The table includes the total number of hits per pathway (all, significant, and expected), the raw p-values (",
               "Hypergeometric), and the p-value modeled on user data using a Gamma distribution. \n");
    cat(descr, file=rnwFile, append=TRUE);
    
    univROCtable<-c("<<echo=false, results=tex>>=",
                    "CreateMummichogAnalTable(mSet)",
                    "@",
                    "\\clearpage\n\n");
    cat(univROCtable, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(mSetObj$mummi.gsea.resmat)){
    descr <- c("\\section{GSEA Pathway Analysis Results}\n",
               "The output of the GSEA analysis consists of a table of results containing ranked pathways that are enriched in ",
               "the user-uploaded data. The table includes the total number of hits (all and expected), their raw p-values and adjusted p-values.",
               " On the web, user's can explore the results in an interactive volcano plot. \n");
    cat(descr, file=rnwFile, append=TRUE);
    
    # Because mummi.gsea.resmat is created for meta-analysis
    if(!is.null(mSetObj$imgSet$mummi.plot)){
      
      descr <- c("\\subsection{GSEA Pathway Analysis Plot}\n",
                 "The pathway summary plot below displays all matched pathways as circles. The color and size of each circle corresponds", 
                 " to its p-value and enrichment factor, respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant", 
                 " pathway hits and the expected number of compound hits within the pathway. \n");
      cat(descr, file=rnwFile, append=TRUE);
      
      fig <- c(  "\\begin{figure}[htp]",
                 "\\begin{center}",
                 paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$mummi.plot,"}",sep=""),
                 "\\caption{Summary of Pathway Analysis}",
                 "\\end{center}",
                 paste("\\label{",mSetObj$imgSet$mummi.plot,"}", sep=""),
                 "\\end{figure}",
                 "\\clearpage\n\n"
      );
      cat(fig, file=rnwFile, append=TRUE, sep="\n");
    }
    
    univROCtable<-c("<<echo=false, results=tex>>=",
                    "CreateGSEAAnalTable(mSet)",
                    "@",
                    "\\clearpage\n\n");
    cat(univROCtable, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(!is.null(mSetObj$integ.resmat)){
    descr <- c("\\section{Meta-Analysis of mummichog and GSEA results}\n",
               "The MS Peaks to Paths module uses the (Fisher's method) for combining the mummichog and GSEA p-values.", 
               "It takes the raw p-values per pathway to perform p-value combination.",
               "The Integrated MS Peaks to Paths plot below summarizes the results of the", 
               " Fisher's method for combining mummichog (y-axis) and GSEA (x-axis) p-values. ",
               "The size and color of the circles correspond to their transformed combined p-values.",
               " Large and red circles are considered the most perturbed pathways.",
               " The blue and pink areas highlight the significant pathways based on either GSEA ",
               "(pink) or mummichog (blue), and the purple area highlights significant pathways identified by both algorithms. \n");
    cat(descr, file=rnwFile, append=TRUE);
    
    fig <- c(  "\\begin{figure}[htp]",
               "\\begin{center}",
               paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$integpks.plot,"}",sep=""),
               "\\caption{Summary of Pathway Analysis}",
               "\\end{center}",
               paste("\\label{",mSetObj$imgSet$integpks.plot,"}", sep=""),
               "\\end{figure}",
               "\\clearpage\n\n"
    );
    cat(fig, file=rnwFile, append=TRUE, sep="\n");
    
    univROCtable<-c("<<echo=false, results=tex>>=",
                    "CreateMetaAnalTable(mSet)",
                    "@",
                    "\\clearpage\n\n");
    cat(univROCtable, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  descr <- c("\\section{Compound Matching Table}\n",
             "The output of the MS Peaks to Pathways module also consists of a comprehensive table containing the compound matching", 
             " information for all user-uploaded m/z features. The table has four columns, containing the Query.Mass of each feature, the predicted Matched.Compound for each feature,",
             "the Matched.Form, and the Mass.Diff. As the file can be very long (>40 pages), please download it separately on the Downloads page of MetaboAnalyst. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  descr <- c("\\section{Network Visualization}\n",
             "The MS Peaks to Pathways module also allows users to interactively view their data in a global KEGG metabolic network.",
             "Users will be able to their network as a SVG or PNG file on the Network Viewer page of MetaboAnalyst. \n");
  cat(descr, file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}
