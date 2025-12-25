
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

#'Create report of analyses (Biomarker)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateMummichogRmdReport<-function(mSetObj, usrName){
  
  mSetObj <- .get.mSet(mSetObj);

  CreateHeader(usrName);
  CreateMummichogIntro();
  #CreateMummichogOverview();

  # peak intensity table
  if(mSetObj$dataSet$type == "mass_table"){
      CreateDataProcdoc(mSetObj);
      CreateNORMdoc(mSetObj);

  }else{ # peak list
      CreateMummichogInputDoc(mSetObj);
  }

  CreateMummichogAnalysisDoc(mSetObj);
  CreateNetworkDoc(mSetObj);

  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}


####################################################
####################################################

#'Create mummichog analysis report: Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'@author XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogIntro <- function(){
  descr <- c("## 1. Overview\n\n",
             "The *Functional Analysis* (MS Peaks to Pathways) module is designed to help understand the functional changes directly from the untargeted metabolomics data from LC-HRMS platforms. 
             The traditional approach requires a time-consuming compound identification step for untargeted metabolomics before functional analysis. 
             It has been shown (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701697/' target=''>Li et al. 2013</a>)
             that by leveraging the collective behaviors of a group (pathways or metabolite sets), pathway activities can be reliably inferred from 
             a ranked list of MS peaks identified by untargeted metabolomics, without performing accurate compound identification. 
             This observation has been proven quantitatively recently by a benchmark study (<a href='https://doi.org/10.1093/bib/bbac553' target=''>Yao et al. 2023</a>) which 
             has shown that ~30% annotation rate is sufficient to achieve high recall (~90% based on mummichog) of perturbed pathways in LC-HRMS data. 
             This module offers the mummichog algorithm (based on ORA), and an adapted GSEA method for LC-HRMS global metabolomics data. The module also includes an expanded knowledge base 
             of 128 organisms for pathway analysis as well as 10 metabolite set libraries.\n\n");
  .buffer_add(descr, collapse="\n");
}


#'Create Mummichog analysis report: Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!exists("fig.count")){
    fig.count <<- 0;
  }
  if(!exists("table.count")){
    table.count <<- 0;
  }
  
  descr <- c("<hr/>",
             "## 2. Data Processing\n\n",
             "### - Data Input\n\n",
             "The module accepts either a three column table containing the m/z features, p-values, and statistical scores,",
             " a two-column table containing m/z features and either p-values or t-scores, or a one-column table ranked by either",
             " p-values or t-scores. All inputted files must be in .txt format. If the input is a three column table, both the mummichog",
             " and GSEA algorithms can be applied. If only p-values (or ranked by p-values) are provided, only the mummichog algorithm will be applied.",
             " If only t-scores (or ranked by t-scores) are provided, only the GSEA algorithm will be applied.",
             "\n\n",
             "Users also need to specify the mass accuracy (ppm), the ion mode (positive or negative), and the p-value cutoff", 
               " to delineate between significantly enriched and non-significantly enriched m/z features (for mummichog only). ", 
               "Currently, MetaboAnalyst 5.0 only supports the handling of peaks obtained from high-resolution MS instruments", 
               " such as Orbitrap, or Fourier Transform (FT)-MS instruments as recommended by the original mummichog implementation.",
             " \n\n");
  
  .buffer_add(descr, collapse="\n");

  check.msgs <- paste("*", mSetObj$msgSet$check.msg);
  check.msgs <- paste0(check.msgs, collapse = "\\\n");

  descr <- c("A summary of your data input parameters: \n",
             check.msgs, "\n");
  .buffer_add(descr, collapse="\n");

}


#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author XiaLab Analytics
#'All rights reserved
#'@export

CreateMummichogAnalysisDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  link <- GetSharingLink(mSetObj);

    descr <- c("<hr/>",
               "## 3. Functional Analysis\n\n",
             "### - Parameter Setting\n\n",
             "The pre-defined functional libraries are primarily curated from recent KEGG, with a few from BioCyc and GEM models as provided by the original mummichog libraries.",
             "These libraries currently cover 128 species, with 10 additional metabolite set libraries based on chemical classes and other information such as ",
             "diseases, genetic associations, locations, etc.\n");
    .buffer_add(descr, collapse="\n");
    
    descr <- c("The selected library: ```", gsub("_", ".", mSetObj$lib.organism), "```.",
               "The selected p-value cutoff: ```", mSetObj$dataSet$cutoff , "```.",
               "\n");
    .buffer_add(descr, collapse="\n");

  if(!is.null(mSetObj$curr.cust) && mSetObj$curr.cust){
    
    descr.cust <- c("### - Analysis Customization\n\n",
                    "The algorithm uses the collective power of metabolic pathways/networks and maps m/z features, including all of its adducts and forms, 
                    onto these pre-defined functions to predict their activities. Here, the assumption is that if a list of significant m/z features truly reflects
                    biological activities, then the underlying metabolites will show enrichment on the pathways/networks, while falsely matched 
                    metabolites will be more randomly distributed. Advanced users can further customized the default settings. \n");
    .buffer_add(descr.cust);
    .buffer_add("\n\n");
    
    descr.curr <- c("#### - Analysis Customization: Currency Metabolites\n\n",
                    "Currency metabolites are abundant compound species such as water and carbon dioxide known to occur in normal 
                    functioning cells and participate in a large number of metabolic reactions. Because of their ubiquitous nature, 
                    they will be removed from further analysis. There is no formal consensus of a set of currency metabolites, 
                    therefore users who are unsatisfied with the default list of currency metabolites can further customize the metabolites 
                    to considered as currency compounds.\n");
    .buffer_add(descr.curr);
    .buffer_add("\n\n");
    
    curr.desc <- paste("The user's selected list of currency metabolites is: ", currency, ".");
    .buffer_add(curr.desc, collapse="\n");
    
    descr.add <- c("#### - Analysis Customization: Adducts\n\n",
                   "In addition to pathway information, the mummichog libraries contain a set of adducts commonly seen from the 
                   MS instruments. These options however, may not be optimal for users' data, therefore users are provided the option
                   to customize the adduct list used in the mummichog analysis.\n");
    .buffer_add(descr.add);
    .buffer_add("\n\n");
    
  }

  if(!is.null(mSetObj$mummi.resmat) && !is.null(mSetObj$imgSet$mummi.plot)){
    
    descr <- c("### - Mummichog pathway analysis\n\n",
               "There are three steps for the mummichog algorithm, 1) Permutations: A list of metabolites (the same length as the number 
               of significant m/z features) are inferred from the user's uploaded set of m/z features, considering all potential matches 
               (isotopes/adducts). These tentative compounds are then mapped onto known metabolic pathways for the selected organism. 
               For each pathway, a hypergeometric p-value is calculated. 2) Step 1 is repeated multiple times to calculate the null distribution of p-values for all pathways, 
               and is modeled as a Gamma distribution. 3) Following this, the significant m/z features are used to calculate the p-values for each pathway 
               (Step 1). These p-values are then adjusted for the permutations.",
               "\n",
               "Pathway analysis results are summarized in Figure ", getFigCount(),
               ". The pathway summary plot below displays all matched pathways as circles. The color and size of each circle corresponds
                to its p-value and enrichment factor, respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant
                pathway hits and the expected number of compound hits within the pathway.",
                "\n",
               "The pathway results are summarized in Table. ", table.count<<-table.count+1, ". The result table contains ranked pathways that are 
                enriched in the user-uploaded data. The table includes the total number of hits per pathway (all, significant, and expected), the raw p-values
               (hypergeometric), and the p-value modeled on user data using a Gamma distribution.",
                "\n\n");
      .buffer_add(descr, collapse="\n");
      .buffer_add("\n\n");
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- getReportLinks(link, "peaks_to_paths",  "peaks_to_paths");
      .buffer_add(reportLinks);
      .buffer_add("\n\n");
      
        fig <- c(paste0("```{r figure_mummi, echo=FALSE, fig.pos='H', fig.cap='Figure ", getCurrentFigCount(), 
                     ". Summary of Mummichog pathway analysis', ",
                      " fig.lp='", 
                      mSetObj$imgSet$mummi.plot, 
                      "',  out.width = '", getFigWidth(mSetObj,width="1000px", widthPct="100%"), "', out.height= '", getFigWidth(mSetObj,width="650px", widthPct="100%"), "'}"),
               "mSet$paramSet$anal.type <<- 'mummichog'",
         "if (mSet$paramSet$report.format == 'html') {",
              "PlotlyPeaks2Paths(NA, '','png', 72, 9, 'default', 5, interactive=T)",
         "} else {",
         "  safeIncludeGraphics(mSet$imgSet$mummi.plot)",
         "}",  
            "```",
              "\n\n")

      .buffer_add(fig, collapse="\n");
      .buffer_add("\n\n", collapse="\n");
    
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- getReportLinks(link, "mum_resTbl");
      .buffer_add(reportLinks);
      .buffer_add("\n\n");

    descr <- c(
      "```{r table_mm, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(CreateMummichogAnalTable(mSet));",
      paste0("create_dt(dt_res,  caption = 'Table ", 
             table.count, 
             ". Results of the Mummichog Pathway Analysis.')"),
      "```", "\n\n")
    
    .buffer_add(descr, collapse="\n"); 

  if(!is.null(mSetObj$imgSet$reportSet$network_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$network_mummichog)){
        descr <- c("### - Network Visualization\n\n",
                 "Users can interactively explore the mapped LC-MS peaks within the global KEGG metabolic network.\n",
             " + Double click a node to view its matched peaks and adducts",
             " + Click on a pathway name in the left table to show its compounds with potential hits. Solid circles indicate significant compounds, while empty circles indicate compounds detected but not significant.",
             "Note the KEGG global map may not contain all the compounds for certain pathways.",
             "\n\n");
        .buffer_add(descr, collapse="\n");
        reportLinks <- getReportLinks(link, "network_mummichog");

        .buffer_add(reportLinks);
        .buffer_add("\n\n");
        fig8 <- c(paste0("```{r figure_network_mummichog, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                     ". Screenshot of interactive network visualization displaying enriched pathways from functional prediction by Mummichog in the context of KEGG global metabolic network.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$network_mummichog, 
                     "', out.width = '", getFigWidth(mSetObj, width="800px"), "'}"),
              "safeIncludeGraphics(mSetObj$imgSet$reportSet$network_mummichog)",
              "```",
              "\n\n");
        .buffer_add(fig8, collapse="\n");
        .buffer_add("\n\n", collapse="\n");
    }   
  }

  if(!is.null(mSetObj$mummi.gsea.resmat) && !is.null(mSetObj$imgSet$mummi.gsea.plot)){

    descr <- c("### - GSEA Pathway Analysis\n\n",
               "GSEA analyzes lists for concentrated occurrences of pathway genes, particularly at the extreme ends of the list, by keeping a running enrichment score (ES) 
               as it proceeds down the list. The algorithm reports the p-value associated with the maximum magnitude reached by the ES for every pathway. 
               A positive ES indicates that pathway occurrence was enriched at the beginning of the list; negative scores indicate enrichment at the end.
               The normalized enrichment score (NES, ES normalized to the mean enrichment of random samples of the same size) is the primary statistic for examining gene set enrichment results. 
               NES accounts for differences in gene set size and in correlations between gene sets and the expression dataset, and can be used to compare analysis results across gene sets.",
               "\n",
              "Pathway analysis results are summarized as an interactive volcano plot in Figure ", fig_gsea1 <- fig.count<<-fig.count+1,
               "and Table ", table.count <<- table.count+1,
               "The pathway summary plot below displays all matched pathways as circles. The color and size of each circle corresponds
               to its p-value and enrichment factor, respectively. The result table contains ranked pathways that are enriched in 
               the user-uploaded data. It includes the total number of hits (all and expected), their raw p-values, adjusted p-values and NES.",
               "\n\n");
      .buffer_add(descr, collapse="\n");
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- getReportLinks(link, "peaks_to_paths_gsea",  "peaks_to_paths_gsea");
      .buffer_add(reportLinks);
      .buffer_add("\n\n");

      # Set the global variable for analysis type outside of the chunk
      mSet$paramSet$anal.type <<- 'gsea'
      mSetObj$paramSet$anal.type <- 'gsea'

        fig2 <- c(paste0("```{r figure_mummi2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_gsea1, 
                     ". Summary of GSEA Pathway Analysis', ",
                      " fig.lp='", 
                      mSetObj$imgSet$mummi.gsea.plot, 
                      "', out.width = '1000px', out.height='650px'}"),
               "load('Rload.RData')",
               "mSet$paramSet$anal.type <<- 'gsea'",
         "if (mSet$paramSet$report.format == 'html') {",
              "PlotlyPeaks2Paths(NA, '','png', 72, 9, 'default', 5, interactive=T)",
         "} else {",
         "  safeIncludeGraphics(mSet$imgSet$mummi.gsea.plot)",
         "}",
            "```",
              "")

        # Concatenate all parts and separate them by new line
        finalFigCode <- paste(fig2, collapse="\n")
      
      .buffer_add(finalFigCode, collapse="\n");
      .buffer_add("\n\n", collapse="\n");
      
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "gsea_resTbl",  "peaks_to_paths_gsea");

    .buffer_add(reportLinks);
    .buffer_add("\n\n");

    descr <- c(
      "```{r table_gsea, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(CreateGSEAAnalTable(mSet));",
      paste0("create_dt(dt_res,  caption = 'Table ", 
             table.count, 
             ". Results of the GSEA pathway analysis.')"),
      "```", "\n\n")
    
    .buffer_add(descr, collapse="\n"); 

    if(!is.null(mSetObj$imgSet$reportSet$network_gsea) && safeFileExists(mSetObj$imgSet$reportSet$network_gsea)){
        reportLinks <- getReportLinks(link, "network_gsea");

        .buffer_add(reportLinks);
        .buffer_add("\n\n");
        fig9 <- c(paste0("```{r figure_network_gsea, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                     ". Screenshot of interactive network visualization displaying enriched pathways from functional prediction by GSEA in the context of KEGG global metabolic network.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$network_gsea, 
                     "', out.width = '", getFigWidth(mSetObj, width="800px"), "'}"),
              "safeIncludeGraphics(mSet$imgSet$reportSet$network_gsea)",
              "```",
              "\n\n");
        .buffer_add(fig9, collapse="\n");
        .buffer_add("\n\n", collapse="\n");
    }
  }

 reportLinks <- paste0('<a href="', link, '&download=mummichog_matched_compound_all.csv" target="_blank">mummichog_matched_compound_all.csv</a> ')

  descr <- c("### - Compound Matching Table\n\n",
             "The output of the MS Peaks to Pathways module also consists of a comprehensive table containing the compound matching",
             " information for all user-uploaded m/z features. The table has four columns, containing the Query.Mass of each feature, the predicted Matched.Compound for each feature,",
             "the Matched.Form, and the Mass.Diff. You can access the file here: ", reportLinks ," \n");
  .buffer_add(descr);
  .buffer_add("\n\n", collapse="\n");
 

 if(!is.null(mSetObj$imgSet$reportSet$heatmap_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$heatmap_mummichog)){
  descr <- c("### - Exploratory Functional Analysis based on Clustering Heatmaps\n\n",
             "This interactive visualization tool provides an overview of the peak intensities across samples. 
              Users can first apply different clustering algorithms to identify strong patterns that are associated with group labels.
              Then use a mouse to drag-select one or more metabolic patterns of interest from the Overview (left) to the Focus View (middle). 
              The functional analysis will be performed on the basis of the selected metabolic patterns in the Focus View as shown below. \n");
  .buffer_add(descr);
    reportLinks <- getReportLinks(link, "heatmap_mummichog");

    .buffer_add(reportLinks);
    .buffer_add("\n\n");
    fig4 <- c(paste0("```{r figure_heatmap_mummichog, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                     ". Screenshot of interactive peak intensity heatmap.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$heatmap_mummichog, 
                     "', out.width = '", getFigWidth(mSetObj), "'}"),
              "safeIncludeGraphics(mSet$imgSet$reportSet$heatmap_mummichog)",
              "```",
              "\n\n");
    .buffer_add(fig4, collapse="\n");
    .buffer_add("\n\n", collapse="\n");
    createEnrichmentTable(mSetObj, "mumEnr");

  }

  if(!is.null(mSetObj$integ.resmat)){
    figCount <- getFigCount();
    descr <- c("### - Integration of mummichog and GSEA results\n\n",
               "The integration employs the Fisher's method to combine the raw p-values computed from mummichog and GSEA per pathway.
               The plot below summarizes the results of the Fisher's method for combining mummichog (y-axis) and GSEA (x-axis) p-values. 
               The size and color of the circles correspond to their transformed combined p-values.
               Large and red circles are considered the most perturbed pathways.\n",
               "Pathway analysis results are summarized in Figure ", figCount, 
               "and Table ", table.count <<- table.count+1,
               ".\n\n");
    .buffer_add(descr);
    
    #integ_peaks
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "integ_peaks",  "integ_peaks");
    .buffer_add(reportLinks);
    .buffer_add("\n\n");
    
    #imagePath <- mSetObj$imgSet$integpks.plot

    # Construct the R Markdown chunk as a character vector
    fig3 <- c(
        paste0("```{r figure_mummi3, echo=FALSE, fig.pos='H', fig.cap='Figure ", figCount, 
         ": Summary of Integrative Pathway Analysis', ",
         "fig.lp='fig:", figCount, "', out.width='", getFigWidth(mSetObj), "'}"),
    "if (mSet$paramSet$report.format == 'html') {",
    "  PlotPSEAIntegPaths(NA, interactive=T)",
    "} else {",
    "  safeIncludeGraphics(mSet$imgSet$integpks.plot)",
    "}",
    "\n```\n\n");

    .buffer_add(fig3, collapse="\n");

    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "integ_resTbl");
    .buffer_add(reportLinks);
    .buffer_add("\n\n");

    descr <- c(
      "```{r table_integ, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(CreateMetaAnalTable(mSet));",
      paste0("create_dt(dt_res,  caption = 'Table ", 
             table.count, 
             ". Meta-Analysis of Mummichog and GSEA Results.')"),
      "```", "\n\n")
    
    .buffer_add(descr, collapse="\n"); 
    
    if(!is.null(mSetObj$imgSet$reportSet$network_integ) && safeFileExists(mSetObj$imgSet$reportSet$network_integ)){
        reportLinks <- getReportLinks(link, "network_integ");
        .buffer_add(reportLinks);
        .buffer_add("\n\n");


        fig10 <- c(paste0("```{r figure_network_integ, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), 
                     ". Screenshot of interactive network visualization displaying enriched pathways from functional prediction by integrative approach in the context of KEGG global metabolic network.',",
                     " fig.lp='", 
                     mSetObj$imgSet$reportSet$network_integ, 
                     "', out.width = '", getFigWidth(mSetObj, width="800px"), "'}"),
              "safeIncludeGraphics(mSet$imgSet$reportSet$network_integ)",
              "```",
              "\n\n");
        .buffer_add(fig10, collapse="\n");
        .buffer_add("\n\n", collapse="\n");
    }
  }
}

CreateMummichogAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$mummi.resmat;
  keep.inx <- c(1,2,3,5,9);
  mummitable <-  mummitable[,keep.inx];
  colnames(mummitable) <- c("Total", "All.Hit", "Sig.Hit", "Fisher.P", "Gamma.P");
  return(mummitable)
}

CreateGSEAAnalTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$mummi.gsea.resmat;
  return(mummitable)
  # Results of the GSEA Pathway Analysis
}

CreateMetaAnalTable <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mummitable <- mSetObj$integ.resmat;
  return(mummitable)
}
