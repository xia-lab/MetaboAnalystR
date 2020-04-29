#'Create report of analyses (IntegPathwayAnalysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegPathwayAnalysisRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateIntegratedPathwayAnalIntr();
  
  CreateIntegratedPathwayAnalInputDoc(mSetObj);
  
  CreateIntegratedPathwayDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Integrated pathwayr analysis report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayAnalIntr <- function(){
  descr <- c("\\section{Background}\n",
             "This module performs integrated metabolic pathway analysis on results obtained from combined metabolomics ", 
             "and gene expression studies conducted under the same experimental conditions. This approach exploits KEGG metabolic ", 
             "pathway models to complete the analysis. The underlying assumption behind this module is that by combining evidence ", 
             "from both changes in gene expression and metabolite concentrations, one is more likely to pinpoint the pathways involved ", 
             "in the underlying biological processes. To this end, users need to supply a list of genes and metabolites of interest that", 
             " have been identified from the same samples or obtained under similar conditions. The metabolite list can be selected from the results", 
             " of a previous analysis downloaded from MetaboAnalyst. Similarly, the gene list can be easily obtained using many excellent web-based", 
             " tools such as GEPAS or INVEX. After users have uploaded their data, the genes and metabolites are then mapped to KEGG metabolic pathways for", 
             " over-representation analysis and pathway topology analysis. Topology analysis uses the structure of a given pathway to evaluate the relative", 
             " importance of the genes/compounds based on their relative location. Inputting the name of a specific pathway will generate a graphical representation", 
             " of that pathway highlighted with the matched genes/metabolites. Users must keep in mind that unlike transcriptomics, where the entire transcriptome", 
             " is routinely mapped, current metabolomic technologies only capture a small portion of the metabolome. This difference can lead to potentially biased", 
             " results. To address this issue, the current implementation of this omic integration module allows users to explore the enriched pathways based either", 
             " on joint evidence or on the evidence obtained from one particular omic platform for comparison.\n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create integrated pathway  report: Data Input
#'@description Report generation using Sweave
#'integrated pathway report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, viewingCanada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayAnalInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "The integrated pathway analysis module accepts a gene list with optional fold changes, the ID type must either be an Entrez ID,", 
             " a RefSeq ID, a Genbank Accession Number, a ENSEMBL Gene Accession Number, or an Official Gene Symbol. The module also accepts ",
             "a metabolite list, with optional fold changes. Here, the ID type must be the compound name, the HMDB ID, or the KEGG ID. Finally", 
             " the organism must be specified, either Homo sapiens (human), Mus musculus (mouse), or Rattus norvegicus (rat). The genes and the metabolites", 
             " will be mapped to the respective databases collected within MetaboAnalyst. ",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  if(exists('map.table', where=mSetObj$dataSet)){
    nametable<-c("<<echo=false, results=tex>>=",
                 "CreateIntegratedPathwayNameMapTable(mSet)",
                 "@");
    cat(nametable, file=rnwFile, append=TRUE, sep="\n");
  }
  
  if (exists('gene.map.table', where=mSetObj$dataSet)){
    
    genetable<-c("<<echo=false, results=tex>>=",
                 "CreateIntegratedPathwayGeneMapTable(mSet)",
                 "@");
    cat(genetable, file=rnwFile, append=TRUE, sep="\n");
  }  
  
  cat("\n\n", file=rnwFile, append=TRUE);
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create a x-table for compound name mapping
#'@description Report generation using Sweave
#'Function to create a table for compound name mapping 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayNameMapTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  namemapped <- mSetObj$dataSet$map.table;
  
  colnames(namemapped) <- c("Query", "Match", "HMDB", "PubChem", "KEGG", "Comment");
  
  print(xtable::xtable(namemapped, caption="Compound Name Mapping"), caption.placement="top", size="\\scriptsize");
  
}

#'Create a x-table for gene name mapping
#'@description Report generation using Sweave
#'Function to create a table for gene name mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayGeneMapTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  genemapped <- mSetObj$dataSet$gene.map.table;
  
  colnames(genemapped) <- c("Query", "Entrez", "Symbol", "Name", "Comment");
  
  print(xtable::xtable(genemapped, caption="Gene Name Mapping"), caption.placement="top", size="\\scriptsize");
  
}

#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$path.mat)){
    return()
  } 
  
  descr <- c("\\section{Integrated Pathway Analysis Overview}\n",
             "The aim of Integrated Pathway Analysis is to perform integrated metabolic pathway analysis on results obtained from combined metabolomics and gene expression", 
             " studies conducted under the same experimental conditions. Users must select the method of enrichment analysis, which aims to evaluate whether the observed genes", 
             " and metabolites in a particular pathway are significantly enriched (appear more than expected by random chance) within the dataset. Users can choose ", 
             "over-representation analysis (ORA) based on either hypergenometrics analysis or Fisher's exact method.",
             " Users must also select the method for topology analysis, which aims to evaluate whether a given gene or metabolite plays an important role in a biological", 
             "response based on its position within a pathway. One method is Degree Centrality, which measures the number of links that connect to a node (representing either", 
             " a gene or metabolite) within a pathway. A second method is Closeness Centrality, which measures the overall distance from a given node to all other nodes in a pathway.", 
             " Finally there is Betweenness Centrality, which measures the number of shortest paths from all nodes to all the others that pass through a given node within a pathway.",
             " Users must finally choose one of three different kinds of pathways for analysis: the gene-metabolite mode (default) which allows for joint-analysis and visualization", 
             " of both significant genes and metabolites. There are also gene-centric or metabolite-centric pathways which allows users to identify enriched pathways", 
             " driven by significant genes or metabolites, respectively.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  # PlotInmexPath
  descr <- c("\\section{Pathway Analysis Result}\n",
             "The results from pathway analysis are presented graphically as well as in a detailed table.",
             "The graphical output contains three levels of view: \\textbf{overview}, \\textbf{pathway view},",
             "and \\textbf{molecule view}. Only the overview is shown below.",
             "Pathway views and molecule views are generated dynamically based on your interactions with the",
             "visualization system. They are available in your downloaded files. \n",
             paste("Figure", fig.count<<-fig.count+1, " shows the overview of all pathways with hits to your queries."),
             paste("Figure", fig.count<<-fig.count+1, " shows the last pathway you inspected."),
             "\n"
            );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  fig <- c(  "\\begin{figure}[htp]",
             "\\begin{center}",
             paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$path.overview,"}",sep=""),
             "\\caption{Summary of Joint Pathway Analysis}",
             "\\end{center}",
             paste("\\label{",mSetObj$imgSet$path.overview,"}", sep=""),
             "\\end{figure}",
             "\\clearpage\n\n"
  );
  cat(fig, file=rnwFile, append=TRUE, sep="\n");

  # PlotReKEGGPath
  
  if(!is.null(mSetObj$imgSet$pathinteg.path)){
    rekeggplot <- c( "\\begin{figure}[htp]",
                     "\\begin{center}",
                     paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$pathinteg.path,"}", sep=""),
                     "\\caption{", paste("A selected pathway from the joint pathway analysis.",
                                         " The matched nodes are highlighted in different colors - red (upregulated), yellow (unknown), green (downregulated)", 
                                         " based on fold change (FC) values.", sep=""),"}",
                     "\\end{center}",
                     paste("\\label{",mSetObj$imgSet$pathinteg.path,"}", sep=""),
                     "\\end{figure}",
                     "\\clearpage"
    );
    cat(rekeggplot, file=rnwFile, append=TRUE, sep="\n");
  }
  
  if(is.null(mSetObj$dataSet$path.mat)){
    return()
  }else{
    resultstable<-c("<<echo=false, results=tex>>=",
                    "CreateIntegratedPathwayResultsTable(mSet)",
                    "@");
    cat(resultstable, file=rnwFile, append=TRUE, sep="\n");
    
  }
  
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  
}

#'Create a x-table for pathway results
#'@description Report generation using Sweave
#'Function to create a table for pathway results
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateIntegratedPathwayResultsTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  results <- mSetObj$dataSet$path.mat;  
  colnames(results) <- c("Total", "Expected", "Hits", "Raw p", "-log(p)", "Holm adjust", "FDR", "Impact");
  path.nms <- names(rownames(results));  
  rownames(results) <- path.nms;

  print(xtable::xtable(results, caption="Enriched pathways based on the integrated pathway analysis."), caption.placement="top", size="\\scriptsize");
  
}

