#'Create report of analyses (Network Explorer)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkExplorerRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateNetworkExplorerIntr();
  
  CreateNetworkExplorerOverview();
  CreateNetworkExplorerInputDoc(mSetObj);
  CreateNetworkExplorerDoc(mSetObj);
  
  CreateRHistAppendix();
  CreateFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Network explorer report introduction
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkExplorerIntr <- function(){
  descr <- c("\\section{Background}\n",
             "The aim of the Network Explorer module is to provide a comprehensive tool to describe user's biological data as a network,", 
             " as well as to explore interactions between biological entities and their potential impacts to help inform hypothesis generation.", 
             " This module supports network visualization of both metabolomic and gene list data. Here, we aim to provide support for the", 
             " integration of transcriptomics and metabolomics data, as well as metagenomics and metabolomics data. MetaboAnalyst 5.0 implements ", 
             "a knowledge-based network approach, whereby user's data (metabolites and genes) can be projected onto five existing biological", 
             " networks: 1) Pathway-based network discovery, 2) Gene-metabolite interaction network, 3) Metabolite-phenotype interaction network, ", 
             "4) Metabolite-metabolite interaction network, and 5) Metabolite-gene-phenotype interaction network.\n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create network explorer report: Overview
#'@description Report generation using Sweave
#'for the network explorer report overview
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkExplorerOverview <- function(){
  descr <- c("\\section{Network Explorer Overview}\n",
             "The Network Explorer module consists of three steps: uploading the data, compound name/gene id mapping,",
             " selecting the network analysis option, and then viewing the generated network in greater detail. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#'Create network explorer: Data Input
#'@description Report generation using Sweave
#'network explorer report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, viewingCanada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkExplorerInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\\section{Data Input}\n",
             "For this module, users may upload either a list of metabolites, a list of genes, or both metabolites and genes. ",
             "MetaboAnalyst 5.0 currently accepts only compound names, HMDB IDs, or KEGG IDs as metabolite identifiers. As well,", 
             "we only accept Entrez IDs, RefSeqIDs, Genbank accession numbers, ENSEMBL gene accession numbers, Official Gene Symbol IDs, ",
             "or KEGG orthologs (KO) as gene identifiers. The uploaded list of metabolites and/or genes is then mapped using our internal ",
             "databases of metabolites and gene annotations. Following this step, users can select which of the five networks to begin", 
             " exploring their data.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  
  if(exists('map.table', where=mSetObj$dataSet)){
    nametable<-c("<<echo=false, results=tex>>=",
                 "CreateNetworkNameMapTable(mSet)",
                 "@");
    cat(nametable, file=rnwFile, append=TRUE, sep="\n");
    cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
  }
  
  if (exists('gene.map.table', where=mSetObj$dataSet)){
    
    genetable<-c("<<echo=false, results=tex>>=",
                 "CreateNetworkGeneMapTable(mSet)",
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
CreateNetworkNameMapTable <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  namemapped <- mSetObj$dataSet$map.table;
  print(xtable::xtable(namemapped, caption="Compound Name Mapping"), table.placement="!h", caption.placement="top", size="\\scriptsize");  
}

#'Create a x-table for gene name mapping
#'@description Report generation using Sweave
#'Function to create a table for gene name mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkGeneMapTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  genemapped <- mSetObj$dataSet$gene.map.table;
  
  colnames(genemapped) <- c("Query", "Entrez", "Symbol", "KO", "Name", "Comment");
  
  print(xtable::xtable(genemapped, caption="Gene Name Mapping"), table.placement="!h", tabular.environment = "longtable", caption.placement="top", size="\\tiny");
  
}

#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jasmine Chong
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateNetworkExplorerDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  descr <- c("\\section{Network Explorer}\n",
             "The Network Explorer analysis module complements MetaboAnalyst's joint-Pathway Analysis module by allowing the ", 
             "identification of connections that cross pathway boundaries (e.g. metabolite-disease interactions) as well as ", 
             "enabling a more global view of the pathways which may not be obvious when examined individually.",
             "The Network Explorer module currently supports five types of biological networks including the ", 
             "KEGG global metabolic network, a gene-metabolite interaction network, a metabolite-disease interaction network, ", 
             "a metabolite-metabolite interaction network, and a metabolite-gene-disease interaction network. The last four", 
             " networks are created based on information gathered from HMDB and STITCH databases, and are applicable to ", 
             "human studies only. The integration of network topological analysis, interactive network exploration, and ", 
             "functional enrichment analysis provides users with different views on their data. Interpreting metabolomic ", 
             "data and/or gene expression data in such a context is far more insightful, and will lead to the generation of ", 
             "testable experimental hypotheses.",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");  

  if(exists('network.type')){

    if(network.type=="gene_metabolites"){
      network = "Gene-Metabolite Interaction Network."
      
      }else if(network.type=="metabo_phenotypes"){
        network = "Metabolite-Disease Interaction Network."
        
        }else if(network.type=="metabo_metabolites"){
          network = "Metabolite-Metabolite Interaction Network."
          
          }else{
            network = "Metabolite-Gene-Disease Interaction Network."
          }

    net.type <- paste("The selected interaction network is: ", network);
    cat(net.type, file=rnwFile, append=TRUE);
    image <- paste(" Figure 1. shows the plot of the created interaction network.");
    cat(image, file=rnwFile, append=TRUE);
    note <- paste (" The plot (Figure 1) shows feature names, which may be hard for users to decipher in the generated plot within
                     this report. It is therefore recommended that users with large datasets to use the Network Explorer to visually explore their data 
                     online, where high-quality SVG/PNG images can be saved.")
    cat(note, file=rnwFile, append=TRUE);

    }else{
      net.type <- paste("The selected network is the KEGG Global Metabolic Network. Please use the web-server to visualize your data as a network and save the file as a PNG/SVG file.");
      cat(net.type, file=rnwFile, append=TRUE);
  }

  if(exists('network.type')){

  networkplot <- c( "\\begin{figure}[htp]",
                        "\\begin{center}",
                        paste("\\includegraphics[width=1.0\\textwidth]{", "network.png","}", sep=""),
                        "\\caption{", "Plot of the selected interaction network","}",
                        "\\end{center}", 
                        paste("\\label{", "network.png" ,"}", sep=""),
                        "\\end{figure}",
                        "\\clearpage"
    );

  cat(networkplot, file=rnwFile, append=TRUE, sep="\n");
 }

  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");  
}