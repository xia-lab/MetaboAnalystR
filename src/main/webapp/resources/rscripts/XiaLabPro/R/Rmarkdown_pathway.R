#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreatePathRnwReport <- function(mSetObj, usrName){
  mSetObj <- .get.mSet(mSetObj);
  CreateHeader(usrName);

  CreatePathIntr();
  CreatePathProcessDoc(mSetObj);
  if(mSetObj$analSet$type == "pathqea"){
    CreateDataProcdoc(mSetObj);
    CreateNORMdoc(mSetObj);
  }
  CreatePathAnalDoc(mSetObj);
  CreatePathResultDoc(mSetObj);
    AddFeatureImages(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Introduction
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreatePathIntr<-function(){
  descr <- c("## 1. Overview\n\n",
             "The Pathway Analysis module combines results from pathway enrichment analysis with 
              pathway visualization to help researchers identify the most relevant pathways 
              involved in the conditions under study. It uses KEGG metabolic pathways as the back-end knowledgebase.
              This module integrates many well-established (i.e. univariate analysis, over-representation analysis) methods 
              as well as novel algorithms and concepts (i.e. Global Test, GlobalAncova, network topology analysis) into
              pathway analysis. Another feature is an interactive visualization system to deliver
              the analysis results in an intuitive manner.",
              "\n\n",
              "The module accepts either a list of compound labels (common names, HMDB IDs or KEGG IDs) 
              with one compound per row, or a compound concentration table with samples in rows and compounds in columns. 
              The second column must be phenotype labels (binary, multi-group, or continuous). The table is uploaded as 
              comma separated values (.csv).",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE);
}


#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA process
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathProcessDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\n\n<hr/>",
             "\n\n## 2. Data Processing\n\n",
             GetNameMappingDoc(),
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "name_map");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);

  descr <- c(
    "```{r table_cnm, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "dt_res <- as.data.frame(GetMapTableRMD(mSet));",
    paste0("create_dt(dt_res,  caption = 'Table 1", 
           ". Result from Compound Name Mapping.')"),
    "```", "\n\n")
  
  cat(descr, file=rmdFile, append=TRUE, sep="\n\n");
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create pathway analysis doc
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathAnalDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\n\n<hr/>",
             "\n\n## 3. Pathway Analysis\n\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  if(mSetObj$analSet$type == "pathora") {
    descr <- c("#### - Over Representation Analysis (ORA)\n\n",
               "Over-representation analysis tests if a particular group of compounds",
               "is represented more than expected by random chance within the user uploaded compound",
               "list. In the context of pathway analysis, we are testing if compounds involved",
               "in a particular pathway are enriched compared to random hits. MetPA offers two of the",
               "most commonly used methods for over-representation analysis: \n\n",
               "1. Fishers'Exact test",
               "2. Hypergeometric Test",
               "*Please note, MetPA uses one-tailed Fisher's exact test which will give essentially",
               "the same result as the result calculated by the hypergeometric test.*",
               "\n\n",
               mSetObj$msgSet$rich.msg,
               "\n\n");
  } else {
    descr <- c("#### - Quantitative Enrichment Analysis (QEA)\n\n",
               "The quantitative enrichment analysis directly use the compound concentration values,",
               "as compared to compound lists used by over-representation",
               "analysis. As a result, it is more sensitive and has the potential to identify",
               "<u>subtle but consistent</u> changes amongst compounds involved in the same biological pathway.",
               "\n\n",
               "Many procedures have been developed in the last decade for quantitative enrichment analysis, the most famous",
               "being the Gene Set Enrichment Analysis. Many new and improved methods have been implemented since.",
               "The enrichment analysis is based on GlobalTest and GlobalAncova. Both methods support enrichment analysis with",
               "binary, multi-group, as well as continuous phenotypes. The p-values can be approximated based on the asymptotic",
               "distribution without using permutations which is computationally very intensive and is not suitable for web applications.",
               "Please note, when sample sizes are small, the approximated p values may be slightly less accurate compared to",
               "p values obtained by using a permutation-based method (for details, please refer to the paper by [Goeman, J.J. et al.](",
               "https://academic.oup.com/bioinformatics/article-lookup/doi/10.1093/bioinformatics/btm051)",
               "and by [Hummel, M. et al.](https://academic.oup.com/bioinformatics/article/24/1/78/205159).",
               "However, since our focus is to identify the most relevant pathways within the pathways in the library,",
               "we are more interested in the rank of the pathway, not its absolute p-value. Therefore, this disadvantage may be tolerated.",
               "\n\n",
               mSetObj$msgSet$rich.msg,
               "\n\n"
    );
  }
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  descr <- c("#### - Pathway Topology Analysis\n\n",
             "The structure of biological pathways represent our knowledge about the complex relationships among molecules",
             "within a cell or a living organism. Changes in more important positions of a network will be more likely to have larger",
             "impact on the pathway than changes occurred in marginal or relatively isolated positions.",
             "The pathway topology analysis uses two well-established node centrality measures to estimate node importance - <u>degree centrality</u>",
             "and <u>betweenness centrality</u>. Degree centrality is defined as the number of links occurred upon a node.",
             "For a directed graph there are two types of degree: in-degree for links come from other nodes, and out-degree",
             "for links initiated from the current node. Metabolic networks are directed graph. Here we only consider the",
             "out-degree for node importance measure. It is assumed that nodes upstream will have regulatory roles for",
             "the downstream nodes, not vice versa. The betweenness centrality measures the number of shortest paths going",
             "through the node. Since the metabolic network is directed, we use the relative betweenness centrality for a metabolite",
             "as the importance measure. The degree centrality measure focuses more on local connectivities, while the betweenness",
             "centrality measure focuses more on global network topology. For more detailed discussions on various graph-based",
             "methods for analyzing biological networks, please refer to the article by <a href='https://pubmed.ncbi.nlm.nih.gov/16880171/' target='_blank'>Tero Aittokallio et al. </a>.",
             "\n\n",
             "*Please note, for comparison among different pathways, the node importance values calculated from centrality measures",
             "are further normalized by the sum of the importance of the pathway. Therefore, the total/maximum importance of each pathway",
             "is 1; the importance measure of each metabolite node is actually the percentage w.r.t the total pathway importance,",
             "and the pathway impact value is the cumulative percentage from the matched metabolite nodes.*",
             "\n\n",
             mSetObj$msgSet$topo.msg,
             "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");

  descr <- c("#### - Pathway Library Selection\n\n",
             "A total of 128 pathway libraries are currently supported, covering 11 organism group:\n",
             "* Mammals [20]",
             "* Birds [2]",
             "* Fish [2]",
             "* Flatworms [2]",
             "* Fungi [11]",
             "* Insects [8]",
             "* Nematodes [2]",
             "* Plants [11]",
             "* Prokaryotes [52]",     
             "* Protists [16]",  
             "* Other animals [3]",           
             "\n\n",
             paste("Your selected pathway library (KEGG code): ```", mSetObj$paramSet$lib.nm, "```"),
             "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

#'Create report of analyses (Met Pathway)
#'@description Report generation using Sweave
#'Metabolomic pathway analysis
#'Create MetPA results doc
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreatePathResultDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("### Pathway Analysis Result\n\n",
             "The results from pathway analysis are presented graphically below. **Mouse over** a particular node to view the pathway name. 
             Two tables are provided - the pathway analysis result table shows the numerical details of the scatter plot; while the 
             pathway mapping table shows individual compounds that are assigned to different pathways. \n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  #path_view
  if(!is.null(mSetObj$imgSet$path.overview)){
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "path_view", "path_view");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    fig <- c(
      paste0("```{r figure_pr, echo=FALSE, fig.pos='H', fig.cap='Figure 1. Summary of Pathway Analysis.', ",
             "fig.lp='fig:", "path_overview', ", # Customize the label pointer as needed
             "out.width='", getFigWidth(mSetObj), "', out.height='650px'}"),
      "if (mSetObj$paramSet$report.format == 'html') {",
      "  PlotPathSummaryGG(NA, F ,interactive=T)",
      "} else {",
      "  knitr::include_graphics(mSetObj$imgSet$path.overview)",
      "}",
      "```",
      "\n\n"
    )

    cat(fig, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  descr <- c(
    "The table below shows the detailed results from the pathway analysis. Since",
    "we are testing many pathways at the same time, the statistical p values from",
    "enrichment analysis are further adjusted for multiple testings. In particular,",
    "\n",
    "* the **Total** is the total number of compounds in the pathway;",
    "* the **Hits** is the actually matched number from the user uploaded data;",
    "* the **Raw p** is the original p value calculated from the enrichment analysis;",
    "* the **Holm p** is the p value adjusted by Holm-Bonferroni method;",
    "* the **FDR p** is the p value adjusted using False Discovery Rate;",
    "* the **Impact** is the pathway impact value calculated from pathway topology analysis.",
    "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");

  link <- GetSharingLink(mSetObj);
  reportLinks <- getReportLinks(link, "path_view_restbl");
  #save.image("pathrmd.RData");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  #print(mSetObj$analSet$type);
  #print("analType=======");
  if(mSetObj$analSet$type == "pathora"){
    # Result from Pathway Analysis
    table.count <<- table.count+1;
    #print(head(mSet$analSet$path.ora.mat));
    #print("pathoramat");
    descr <- c(
      "```{r table_paa, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(mSet$analSet$path.ora.mat);",
      paste0("create_dt(dt_res,  caption = 'Table ", table.count,
             ". Result from ORA Pathway Analysis.', table.name='pathora')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n");

  }else{
    # Result from Quantitative Enrichment Analysis
    descr <- c(
      "```{r table_pqea, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(mSet$analSet$path.qea.mat);",
      paste0("create_dt(dt_res,  caption = 'Table 2", 
             ". Result from Quantitative Enrichment Analysis.', table.name='pathqea')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n");

  }



    table.count <<- table.count+1;
    sum_dt2 <- mSetObj$analSet$pathwayMemberTable;
    if(nrow(sum_dt2) > 0){

    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "path_view_restbl");

    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

        cmdhist2 <- c(
          "```{r table_mtb4, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt2 <- mSet$analSet$pathwayMemberTable;",
          paste0("create_dt(sum_dt2,  caption = 'Table ", 
                 table.count, 
                 ". Pathway mapping details table.')"),
          "```", "\n\n");

        cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    }
}
    if(!is.null(mSetObj$imgSet$reportSet$heatmap_pathway) && file.exists(mSetObj$imgSet$reportSet$heatmap_pathway)){
        reportLinks <- getReportLinks(link, "heatmap_pathway");

        cat(reportLinks, file=rmdFile, append=TRUE);
        cat("\n\n", file=rmdFile, append=TRUE);
        fig2 <- c(paste0("```{r figure_heatmap_pathway, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                         ". Screenshot of interactive peak heatmap.',",
                         " fig.lp='", 
                         mSetObj$imgSet$reportSet$heatmap_pathway, 
                         "', out.width = '", getFigWidth(mSetObj), "'}"),
                  "knitr::include_graphics(mSetObj$imgSet$reportSet$heatmap_pathway)",
                  "```",
                  "\n\n");
        cat(fig2, file=rmdFile, append=TRUE, sep="\n");
        cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
}

