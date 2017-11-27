#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'write .Rnw file template
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreatePathRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreatePathIntr();
  CreatePathInputDoc();
  CreatePathProcessDoc(mSetObj);
  CreatePathAnalDoc(mSetObj);
  CreatePathResultDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Introduction
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

CreatePathIntr<-function(){
  descr <- c("\\section{Background}\n",
             "The Pathway Analysis module combines results from powerful pathway",
             "enrichment analysis with the pathway topology analysis to help researchers identify the most",
             "relevant pathways involved in the conditions under study.",
             "\n\n",
             "There are many commercial pathway analysis software tools, such as Pathway Studio, MetaCore, or",
             "Ingenuity Pathway Analysis (IPA), etc. Compared to them, the pathway analysis module was specially developed",
             "for metabolomics studies. It uses the high-quality KEGG metabolic pathways as the backend knowledgebase.",
             "It integrates many well-established (i.e. univariate analysis, over-representation analysis) methods,",
             "as well as novel algorithms and concepts (i.e. Global Test, GlobalAncova, network topology analysis) into",
             "pathway analysis. Another feature is a Google-Map style interactive visualization system to deliver",
             "the analysis results in an intuitive manner. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create data input doc
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePathInputDoc<-function(){
  
  descr <- c("\\section{Data Input}\n",
             "Pathway Analysis accepts either a list of compound labels (common names, HMDB IDs or KEGG IDs) with one compound per row,",
             "or a compound concentration table with samples in rows and compounds in columns. The second column must be",
             "phenotype labels (binary, multi-group, or continuous). The table is uploaded as comma separated values (.csv).",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA process
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePathProcessDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Compound Name Matching}\n",
             "The first step is to standardize the compound labels used in user uploaded data. This is a necessary step since",
             "these compounds will be subsequently compared with compounds contained in the pathway library.",
             "There are three outcomes from the step - exact match, approximate match (for common names only), and no match.",
             "Users should click the textbf{View} button from the approximate matched results to manually select the correct one.",
             "Compounds without match will be excluded from the subsequently pathway analysis.",
             "\n\n",
             "\\textbf{Table 1} shows the conversion results. Note: \\textit{1} indicates exact match, \\textit{2}",
             "indicates approximate match, and \\textit{0} indicates no match. A text file contain the result can be",
             "found the downloaded file \\textit{name\\_map.csv}\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr<-c("<<echo=false, results=tex>>=",
           "GetMapTable(mSet)",
           "@",
           "\\clearpage\n\n"
  );
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create pathway analysis doc
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePathAnalDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Pathway Analysis}\n",
             "In this step, users are asked to select a pathway library, as well as specify the algorithms for pathway",
             "enrichment analysis and pathway topology analysis. \n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("\\subsection{Pathway Library}\n",
             "There are 15 pathway libraries currently supported, with a total of 1173 pathways :\n",
             "\\begin{itemize}",
             "\\item{Homo sapiens (human) [80]}",
             "\\item{Mus musculus (mouse) [82]}",
             "\\item{Rattus norvegicus (rat) [81]}",
             "\\item{Bos taurus (cow) [81]}",
             "\\item{Danio rerio (zebrafish) [81]}",
             "\\item{Drosophila melanogaster (fruit fly) [79]}",
             "\\item{Caenorhabditis elegans (nematode) [78]}",
             "\\item{Saccharomyces cerevisiae (yeast) [65]}",
             "\\item{Oryza sativa japonica (Japanese rice) [83]}",
             "\\item{Arabidopsis thaliana (thale cress) [87]}",
             "\\item{Escherichia coli K-12 MG1655 [87]}",
             
             "\\item{Bacillus subtilis [80]}",
             "\\item{Pseudomonas putida KT2440 [89]}",
             "\\item{Staphylococcus aureus N315 (MRSA/VSSA)[73]}",
             "\\item{Thermotoga maritima [57]}",
             "\\end{itemize}",
             "\n\n",
             mSetObj$msgSet$lib.msg,
             "\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  if(mSetObj$analSet$type == "pathora"){
    descr <- c("\\subsection{Over Representation Analysis}\n",
               "Over-representation analysis is to test if a particular group of compounds",
               "is represented more than expected by chance within the user uploaded compound",
               "list. In the context of pathway analysis, we are testing if compounds involved",
               "in a particular pathway is enriched compared by random hits. MetPA offers two",
               "most commonly used methods for over-representation analysis: \n",
               "\\begin{itemize}",
               "\\item{Fishers'Exact test}",
               "\\item{Hypergeometric Test}",
               "\\end{itemize}",
               "\\textit{Please note, MetPA uses one-tailed Fisher's exact test which will give essentially",
               "the same result as the result calculated by the hypergeometric test.}",
               "\n\n",
               mSetObj$msgSet$rich.msg,
               "\n\n");
  }else{
    descr <- c("\\subsection{Pathway Enrichment Analysis}\n",
               "Pathway enrichment analysis usually refers to quantitative enrichment analysis directly",
               "using the compound concentration values, as compared to compound lists used by over-representation",
               "analysis. As a result, it is more sensitive and has the potentail to identify",
               "\\textbf{subtle but consistent} changes among compounds involved in the same biological pathway.",
               "\n\n",
               "Many procedures have been developed in the last decade for quantitative enrichment analysis, the most famous",
               "being the Gene Set Enrichment Analysis. Many new and improved methods have been implemented ever since.",
               "The enrichment analysis is based on GlobalTest and GlobalAncova. Both methods support enrichment analysis with",
               "binary, multi-group, as well as continuous phenotypes. The p values can be approximated based on the asymptotic",
               "distribution without using permutations which is computationally very intensive and is not suitable for web applications.",
               "Please note, when sample sizes are small, the approximated p values may be slightly less accurate compared to",
               "p values obtained by permutation-based method (for details, please refer to the paper by Goeman, J.J. et al.",
               "\\footnote{Jelle J. Goeman and Peter Buhlmann. \\textit{Analyzing gene expression data in terms of gene",
               "sets: methodological issues}, Bioinformatics 2007 23(8):980-987}",
               "and by Hummel, M. et al.",
               "\\footnote{Manuela Hummel, Reinhard Meister and Ulrich Mansmann. \\textit{GlobalANCOVA: exploration and assessment of gene group",
               "effects}, Bioinformatics 2008 24(1):78-85})",
               "However, since our focus is to identify the most relevant pathways within the pathways in the library,",
               "we are more interested in the rank of the pathway, not its absolote p-value. Therefore, this disadvantage may be tolerated.",
               "\n\n",
               mSetObj$msgSet$rich.msg,
               "\n\n"
    );
  }
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c("\\subsection{Pathway Topology Analysis}\n",
             "The structure of biological pathways represent our knowledge about the complex relationships among molecules",
             "within a cell or a living organism. However, most pathway analysis algorithms fail to take the structural information",
             "into consideration when estimating which pathways are significantly changed under conditions of study",
             "It is well-known that changes in more important positions of a network will trigger a more severe",
             "impact on the pathway than changes occured in marginal or relatively isolated positions.",
             "\n\n",
             "The pathway toplogy analysis uses two well-established node centrality measures to estimate node importance - \\textbf{degree centrality}",
             "and \\textbf{betweenness centrality}. Degree centrality is defined as the number of links occured upon a node.",
             "For directed graph, there are two types of degree: in-degree for links come from other nodes, and out-degree",
             "for links initiated from the current node. Metabolic networks are directed graph. Here we only consider the",
             "out-degree for node importance measure. It is assumed that nodes in upstream will have regulatory roles for",
             "the downstream nodes, not vice versa. The betweenness centrality measures number of shortest paths going",
             "through the node. Since metabolic network is directed, we use relative betweenness centrality for metabolite",
             "importance measure. The degree centrality measures focus more on local connectivities, while the betweenness",
             "centrality measures focus more on global network topology. For more detailed discussions on various graph-based",
             "methods for analysing biological networks, please refer to the article by Tero Aittokallio, T. et al.",
             "\\footnote{Tero Aittokallio and Benno Schwikowski. \\textit{Graph-based methods for analysing networks in cell biology},",
             "Briefings in Bioinformatics 2006 7(3):243-255}",
             "\n\n",
             "\\textit{Please note, for comparison among different pathways, the node importance values calculated from centrality measures",
             "are further normalized by the sum of the importance of the pathway. Therefore,  the total/maximum importance of each pathway",
             "is 1; the importance measure of each metabolite node is actually the percentage w.r.t the total pathway importance,",
             "and the pathway impact value is the cumulative percentage from the matched metabolite nodes.}",
             "\n\n",
             mSetObj$msgSet$topo.msg,
             "\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA results doc
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
CreatePathResultDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Pathway Analysis Result}\n",
             "The results from pathway analysis are presented graphically as well as in a detailed table.",
             "\n\n",
             "A Google-map style interactive visualization system was implemented to facilitate data exploration.",
             "The graphical output contains three levels of view: \\textbf{metabolome view}, \\textbf{pathway view},",
             "and \\textbf{compound view}. Only the metabolome view is shown below.",
             "Pathway views and compound views are generated dynamically based on your interactions with the",
             "visualization system. They are available in your downloaded files. \n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  fig <- c(  "\\begin{figure}[htp]",
             "\\begin{center}",
             paste("\\includegraphics[width=1.0\\textwidth]{",mSetObj$imgSet$path.overview,"}",sep=""),
             "\\caption{Summary of Pathway Analysis}",
             "\\end{center}",
             paste("\\label{",mSetObj$imgSet$path.overview,"}", sep=""),
             "\\end{figure}",
             "\\clearpage\n\n"
  );
  cat(fig, file=rnwFile, append=TRUE, sep="\n");
  
  descr <- c(
    "The table below shows the detailed results from the pathway analysis. Since",
    "we are testing many pathways at the same time, the statistical p values from",
    "enrichment analysis are further adjusted for multiple testings. In particular, ",
    "the \\textbf{Total} is the total number of compounds in the pathway;",
    "the \\textbf{Hits} is the actually matched number from the user uploaded data;",
    "the \\textbf{Raw p} is the original p value calculated from the enrichment analysis;",
    "the \\textbf{Holm p} is the p value adjusted by Holm-Bonferroni method;",
    "the \\textbf{FDR p} is the p value adjusted using False Discovery Rate;",
    "the \\textbf{Impact} is the pathway impact value calculated from pathway topology analysis.",
    "\n");
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  if(mSetObj$analSet$type == "pathora"){
    descr<-c("<<echo=false, results=tex>>=",
             "GetORATable(mSet)",
             "@",
             "\\clearpage\n\n"
    );
  }else{
    descr<-c("<<echo=false, results=tex>>=",
             "GetQEATable(mSet)",
             "@",
             "\\clearpage\n\n"
    );
  }
  cat(descr, file=rnwFile, append=TRUE, sep="\n");
  
}

