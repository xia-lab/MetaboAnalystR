
if (!exists("safeFileExists", mode = "function")) {
  safeFileExists <- function(path) {
    if (is.null(path) || length(path) != 1 || !is.character(path)) {
      return(FALSE)
    }
    if (is.na(path) || path == "") {
      return(FALSE)
    }
    file.exists(path)
  }
}

if (!exists("safeIncludeGraphics", mode = "function")) {
  safeIncludeGraphics <- function(path) {
    if (!safeFileExists(path)) {
      return(NULL)
    }
    knitr::include_graphics(path)
  }
}

#'Create report of analyses (Network Explorer)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateNetworkExplorerIntr();
  
  CreateNetworkExplorerInputDoc(mSetObj);
  CreateNetworkExplorerDoc(mSetObj);
    AddFeatureImages(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Network explorer report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "The Network Explorer module allows users to explore their metabolomics data within network context. Networks can intuitively 
             capture interactions between biological entities as well as reveal their patterns (i.e. activity centers or hot spot) to gain novel insights 
             or to inform new hypothesis. This module supports network creation and visualization of **six** different types of networks, 
             including 5 knowledge-based networks and 1 data-driven network. User's data will be projected onto these networks for interactive visualization 
             and functional enrichment analysis. These methods are primarily developed for targeted (or annotated untargeted) metabolomics data.", 
             "\n\n",
             "There are several steps: uploading the data, compound name/gene id mapping, selecting the network analysis option, and then viewing the 
             generated network in greater detail. It accepts lists of metabolites and/or genes or KOs generated from metabolomics, transcriptomics or microbiome studies.
             Note only data from *human (including human associated microbiome)* are currently supported since the underlying 
             knowledge networks are based on human studies.",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}


#'Create network explorer: Data Input
#'@description Report generation using Sweave
#'network explorer report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr2 <- "";
  if(!is.null(mSetObj$dataSet$q.type.gene)){
    descr2 <- paste("Input gene ID type: ```", mSetObj$dataSet$q.type.gene, "```;"); 
  }
  if(!is.null(mSetObj$dataSet$q.type.gene)){
    descr2 <- paste("Input compound ID type: ```", mSetObj$dataSet$cmpd.id.type, "```.");
  }
  descr <- c("## 2. Data Processing\n\n",
             "### Name mapping\n", descr2, "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  #cat("\n\n", file=rmdFile, append=TRUE);
  
  if(exists('map.table', where=mSetObj$dataSet)){
    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "name_map_network");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    table.count <<- table.count+1;
    cmdhist2 <- c(
      "```{r table_ntt1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateNetworkNameMapTableRMD(mSet);",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Compound Name Mapping.', 'namemap')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  if (exists('gene.map.table', where=mSetObj$dataSet)){
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "name_map_network_gene");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    table.count <<- table.count+1;
    cmdhist2 <- c(
      "```{r table_ntt2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateNetworkGeneMapTableRMD(mSet);",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Gene Name Mapping.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  cat("\n\n", file=rmdFile, append=TRUE);
}


#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkExplorerDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  network.types <- c("gene_metabolites", "metabo_phenotypes", "metabo_metabolites", "global", "dspc");

  network.names <- c("Gene-Metabolite Interaction Network", 
                     "Metabolite-Disease Interaction Network", 
                     "Metabolite-Metabolite Interaction Network", 
                     "Global Network", 
                     "Debiased Sparse Partial Correlation (DSPC) Network");
  
  descr <- c("## 3. Network Creation and Visualization\n\n",
             "The Network Explorer analysis module allows the identification of connections that cross pathway boundaries, 
              linking to phenotypes (e.g. metabolite-disease interactions) to enable a global view of the interactions and impact.",
             "The Network Explorer module currently supports **six** types of biological networks:",
             "\n\n",
             "1. KEGG global metabolic network",
             "2. Gene-metabolite interaction network",
             "3. Metabolite-disease interaction network",
             "4. Metabolite-metabolite interaction network",
             "5. Metabolite-gene-disease interaction network",
             "6. Debiased Sparse Partial Correlation (DSPC) network",
             "\n\n",
             "Note the networks (2-5) are created based on information gathered from HMDB and STITCH databases, and are applicable to 
             human studies only. The DSPC network is created by a data-driven algorithm for *de novo* discovery of connectivity patterns from metabolomics data 
             (see <a href='https://doi.org/10.1093/bioinformatics/btx012' target='_blank'>Basu et al.</a>)",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
 # netSel <- "";
 # net.sel <- FALSE;
 # for(i in 1:length(network.types)){
 #   net.type <- network.types[i]
 #   net.name <- network.names[i];
  #  if(file.exists(paste0(net.type, ".png"))){
  #    net.sel <- TRUE;
  #    netSel <- c(netSel, paste("- ```", net.name, "``` \n"))
  #  }
  #}

  #if(net.sel){
  #  descr2 <- c("You have selected the following interaction network(s): \n\n", netSel);
  #  descr2 <- c(descr2, "\n\n");
  #  cat(descr2, file=rmdFile, append=TRUE)
  #}

  CreateKeggNetDoc(mSetObj);
  createEnrichmentTable(mSetObj, "keggGlobal")

  for(i in 1:length(network.types)){
    net.type <- network.types[i];
    net.name <- network.names[i];
    if(file.exists(paste0(net.type, ".png"))){
      cat(paste("\n\n####", net.name, "\n\n"),file=rmdFile, append=TRUE);
      link <- GetSharingLink(mSetObj);
      reportLinks <- getReportLinks(link, net.type);
      cat(paste(reportLinks, "\n\n"), file=rmdFile, append=TRUE);
      figCaption <- paste('Figure ', getFigCount(), '. Screenshot of ', net.name)
      figCode <- paste0("```{r figure_ntw", i, ", echo=FALSE, fig.pos='H', fig.cap='", figCaption, 
                        "', out.width='", getFigWidth(mSetObj), "'}\n",
                        "safeIncludeGraphics('", net.type, ".png')\n",
                        "```")
      cat(figCode, file=rmdFile, append=TRUE)
      cat("\n\n", file=rmdFile, append=TRUE)
      
      createEnrichmentTable(mSetObj, net.type)
    }
  }
  
}

#'Create a x-table for compound name mapping
#'@description Report generation using Sweave
#'Function to create a table for compound name mapping 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkNameMapTableRMD <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  namemapped <- mSetObj$dataSet$map.table;
  return(namemapped)
}

#'Create a x-table for gene name mapping
#'@description Report generation using Sweave
#'Function to create a table for gene name mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateNetworkGeneMapTableRMD <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  genemapped <- mSetObj$dataSet$gene.map.table;
  colnames(genemapped) <- c("Query", "Entrez", "Symbol", "KO", "Name", "Comment");
  return(genemapped)
}