#'Create report of analyses (IntegPathwayAnalysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegPathwayAnalysisRnwReport<-function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateIntegratedPathwayAnalIntr();
  
  CreateIntegratedPathwayAnalInputDoc(mSetObj);
  
  CreateIntegratedPathwayDoc(mSetObj);
  
  CreateKeggNetDoc(mSetObj);
  createEnrichmentTable(mSetObj, "keggGlobal")
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}

#'Create integrated pathway analysis report: Introduction  
#'@description Report generation using Sweave
#'Integrated pathwayr analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayAnalIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "This module performs integrated metabolic pathway analysis from two lists (metabolites/peaks and genes/proteins) of interest. 
             The lists can be obtained from targeted/untargeted metabolomics and transcriptomics/proteomics experiments studying 
             the same or similar conditions. This approach leverages metabolic pathway models to integrate genes/proteins and metabolites. 
             After data upload, the genes and metabolites are then mapped to KEGG metabolic pathways for over-representation analysis and pathway 
             topology analysis. In addition to a global summary of the enriched pathways, you can directly visualize the mapped genes and metabolites 
             within individual pathways or the global metabolic network.",
             "\n\n",
             "Users must keep in mind that unlike transcriptomics, where the entire transcriptome is routinely mapped, 
             current metabolomic technologies only capture a small portion of the metabolome. This difference can lead to potentially biased results. 
             To address this issue, the current implementation of this omic integration module allows users to explore the enriched pathways based either 
             on joint evidence or on the evidence obtained from one particular omic platform for comparison.\n\n"
  );
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Create integrated pathway  report: Data Input
#'@description Report generation using Sweave
#'integrated pathway report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'McGill University, viewingCanada
#'All rights reserved
#'@export
CreateIntegratedPathwayAnalInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("## 2. Data Processing\n\n",
             "The integrated pathway analysis module accepts a gene list with optional fold changes, the ID type must either be an Entrez ID,", 
             " a RefSeq ID, a Genbank Accession Number, a ENSEMBL Gene Accession Number, or an Official Gene Symbol. The module also accepts ",
             "a metabolite list, with optional fold changes. Here, the ID type must be the compound name, the HMDB ID, or the KEGG ID. Finally", 
             " the organism must be specified, either Homo sapiens (human), Mus musculus (mouse), or Rattus norvegicus (rat). The genes and the metabolites", 
             " will be mapped to the respective databases collected within MetaboAnalyst. ",
             "\n\n");
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  if(exists('map.table', where=mSetObj$dataSet)){
    
    cat("\n\n", file=rmdFile, append=TRUE);
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "name_map_pathinteg_cmpd")

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);


    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mtb1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateIntegratedPathwayNameMapTableRMD(mSet);",
      paste0("create_dt(sum_dt, 'Table ", 
             table.count, 
             ". Compound Name Mapping.', 'namemap')"),
      "```", 
      "\n\n"
    );

    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
  
  if (exists('gene.map.table', where=mSetObj$dataSet)){

    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "name_map_pathinteg")
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;

    cmdhist2 <- c(
      "```{r table_mtb2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateIntegratedPathwayGeneMapTableRMD(mSet);",
      paste0("create_dt(sum_dt, 'Table ", 
             table.count, 
             ". Gene Name Mapping.')"),
      "```", 
      "\n\n"
    );

    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }  
  
  cat("\n\n", file=rmdFile, append=TRUE);
}

#'Create a x-table for compound name mapping
#'@description Report generation using Sweave
#'Function to create a table for compound name mapping 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayNameMapTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  namemapped <- mSetObj$dataSet$map.table;
  
  colnames(namemapped) <- c("Query", "Match", "HMDB", "PubChem", "KEGG", "SMILES", "Comment");
  return(namemapped)
}


#'Create a x-table for gene name mapping
#'@description Report generation using Sweave
#'Function to create a table for gene name mapping
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayGeneMapTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  genemapped <- mSetObj$dataSet$gene.map.table;
  if(ncol(genemapped) == 5){
    col.names <- c("Query", "Entrez", "Symbol", "Name", "Comment");
  }else{
    col.names <- c("Query", "Entrez", "Symbol", "KO", "Name", "Comment");
  }
  colnames(genemapped) <- col.names;
  return(genemapped)
}

#'Create integrated pathway analysis report
#'@description Report generation using Sweave
#'Biomarker analysis report, ROC Curve Based Model Creation and Evaluation
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$path.mat)){
    return()
  } 
  
  descr <- c("## 3. Integrated Pathway Analysis\n\n",
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
  
  cat(descr, file=rmdFile, append=TRUE);
  
  # PlotInmexPath
  descr <- c("The results from pathway analysis are presented graphically as well as in a detailed table.",
             "The graphical output contains three levels of view: **overview**, **pathway view**,",
             "and **molecule view**. Only the overview is shown below.",
             "Pathway views and molecule views are generated dynamically based on your interactions with the",
             "visualization system. They are available in your downloaded files. \n",
             paste("Figure", fig_ov <- fig.count<<-fig.count+1, " shows the overview of all pathways with hits to your queries."),
             paste("Figure", fig_lp <- fig.count<<-fig.count+1, " shows the last pathway you inspected."),
             "\n\n"
            );
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "path_view_integ", "path_view");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
fig <- c(paste0("```{r figure_pars1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ov, 
                ". Summary of Joint Pathway Analysis', ",
                " fig.lp='", 
                mSetObj$imgSet$path.overview, 
                "', out.width = '", getFigWidth(mSetObj), "', out.height = '650px'}"),
         "if (mSetObj$paramSet$report.format == 'html') {",
         "  PlotPathSummaryGG(NA, F ,interactive=T)",
         "} else  {",
         "  knitr::include_graphics(mSetObj$imgSet$path.overview)",
         "}",
         "```",
         "\n\n");

  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  # PlotReKEGGPath
  if(!is.null(mSetObj$imgSet$pathinteg.path)){
    # plot_kegg_graph
    link <- GetSharingLink(mSetObj)
    reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                          '<a href="', link, '&format=pdf&imgCmd=plot_kegg_graph" target="_blank">PDF</a> ',
                          '<a href="', link, '&format=svg&imgCmd=plot_kegg_graph" target="_blank">SVG</a>',
                          '</div>')
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    path.img <- mSetObj$imgSet$pathinteg.path; 
    fig <- c(paste0("```{r figure_pars2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_lp, 
                    ". A selected pathway: ```", substr(path.img, 0, nchar(path.img)-4), 
                    "```. The matched nodes are highlighted in different colors - red (upregulated), yellow (unknown), green (downregulated)", 
                    " based on fold change (FC) values.', ",
                    " fig.lp='", 
                    path.img, 
                    "', out.width = '630px'}"),
             "knitr::include_graphics(mSetObj$imgSet$pathinteg.path)",
             "```",
             "\n\n");
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  }
  
  if(is.null(mSetObj$dataSet$path.mat)){
    return()
  } else {
   
  link <- GetSharingLink(mSetObj);
  reportLinks <- getReportLinks(link, "path_view_integ");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mtb3, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateIntegratedPathwayResultsTableRMD(mSet);",
      paste0("create_dt(sum_dt, 'Table ", 
             table.count, 
             ". Enriched pathways based on the integrated pathway analysis.')"),
      "```", 
      "\n\n"
    );

    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);

  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "path_view_integ");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);


    table.count <<- table.count+1;

    cmdhist2 <- c(
      "```{r table_mtb4, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt2 <- CreateIntegratedPathwayMemberTableRMD();",
      paste0("create_dt(sum_dt2, 'Table ", 
             table.count, 
             ". Pathway mapping details table.')"),
      "```", 
      "\n\n"
    );

    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");

  }
}

#'Create a x-table for pathway results
#'@description Report generation using Sweave
#'Function to create a table for pathway results
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateIntegratedPathwayResultsTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  results <- mSetObj$dataSet$path.mat;
  if(ncol(results)==8){
    colnames(results) <- c("Total", "Expected", "Hits", "Raw p", "-log10(p)", "Holm adjust", "FDR", "Impact");
  }
  #path.nms <- names(rownames(results)); 
  path.nms <- GetIntegResultPathNames(mSetObj);  
  rownames(results) <- path.nms;
  return(results)
}
