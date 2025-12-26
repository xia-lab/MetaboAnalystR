
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
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateMetaPathRnwReport<-function(mSetObj, usrName){

  CreateHeader(usrName);
  CreateMummichogMetaAnalReport(mSetObj);
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
  
}

#'Create analysis report: Functional Meta-Analysis
#'@description Report generation using Sweave
#'Functional Meta-Analysis Report 
#'@param mSetObj mSetObj
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogMetaAnalReport<-function(mSetObj){
  CreateMetaMummichogIntro();
  CreateMetaMummichogInputDoc(mSetObj);
  CreateMetaMummichogResults(mSetObj);
  CreateUpSetDoc(mSetObj);
  CreateKeggNetDocMetaMummi(mSetObj);
  createEnrichmentTable(mSetObj, "keggGlobal")
}

#'Create analysis report: Functional Meta-Analysis Introduction  
#'@description Report generation using Sweave
#'Mummichog analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogIntro <- function(){
  
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  
  back.descr <- c("## 1. Overview\n\n",
                  "The amount of untargeted metabolomics data deposited in public repositories 
                   has grown tremendously in recently years. There is a growing interest to leverage these 
                   public datasets to identify robust biomarkers and functional changes - a process generally known as meta-analysis.
                   The process is relatively straightforward for targeted metabolomics data where individual compounds can be 
                   compared across different datasets. However, it remains a challenging task to perform meta-analysis for 
                   untargeted metabolomics data such as LC-MS peak intensity tables. This is because the LC-MS peaks are generally not 
                   comparable across different studies, as retention time (RT) and mass (m/z) tend to be platform specific.",
                   "\n\n",
                  "To get around this challenge, one approach is perform compound identification first to convert peak intensity 
                   table into a compound abundance table before meta-analysis. However, the process is time consuming. 
                   Another promising approach is to perform meta-analysis at pathway level. As shown by the mummichog algorithm 
                   (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701697/' target=''>Li et al. 2013</a>), high-resolution 
                   LC-MS data allows reliably identify pathway activities based on putatively identified compounds. We have extended 
                   this idea for **functional meta-analysis** to identify robust pathway-level changes by integrating multiple 
                   LC-MS untargeted metabolomics data.",
                   "\n\n");
  .buffer_add(back.descr, collapse="\n");
  
}

#'Create analysis report: Functional Meta-Analysis Data Input
#'@description Report generation using Sweave
#'Mummichog analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("## 2. Data Processing\n\n",
             "The Functional Meta-Analysis module accepts multiple peak tables with or without retention time information.",  
             "All inputted files must be in .csv format.",
             "\n\n");
  
  .buffer_add(descr, collapse="\n");
  
  sanity_msg <- vector("list");
  
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  if(detectPairedElements(dataNMs)){
    newDataNMs <- detectPairedElementsUnique(dataNMs);
    for(i in newDataNMs) {
        formatted_messages <- gsub(" \\* ", "", mSetObj[[i]]$check.msg)  # Remove leading bullet points from messages if they exist.
        sanity_msg[[i]] <- paste0("\n\n", gsub("_", "", mSetObj[[i]]$name), " and negative", ":\n\n", paste0("* ", formatted_messages, collapse = "\n"))
    }
  }else{
    for(i in dataNMs) {
        formatted_messages <- gsub(" \\* ", "", mSetObj[[i]]$check.msg)  # Remove leading bullet points from messages if they exist.
        sanity_msg[[i]] <- paste0("\n\n", gsub("_", "", mSetObj[[i]]$name), ":\n\n", paste0("* ", formatted_messages, collapse = "\n"))
    }
}
  
  .buffer_add(unlist(sanity_msg));
  .buffer_add("\n\n");


if(is.null(mSetObj$metaLevel)){
    return;
}
  if(!is.na(mSetObj$paramSet$version)){
    mum.version <- mSetObj$paramSet$version;
    descr <- c("\n## 3. Parameter Setting\n\n",
                "The Functional Meta-Analysis module supports two types of meta-analysis: 
                **pathway level integration** perform pathway activity analysis on individual data and integrate
                their p values; while **pooling peaks** aims to improve the metabolome coverage by directly merging peak data. 
                The latter approach is suitable when the same set of samples were obtained using the same (or very similar) MS instrument under 
                different experimental conditions (i.e., different extraction methods or ion modes).
                Users must select the algorithm (mummichog or GSEA), and pathway library used for meta-analysis.", 
               "\n");
    .buffer_add(descr, collapse="\n");
    
    if(mSetObj$metaLevel == "pathway"){
      if(!is.null("mSetObj$meta_results")){
        # pathway level
        integ.desrc <- c(
                paste("The selected method for meta-analysis is ```Pathway-Level Integration```."),
                paste("The selected p-value integrating pathways is: ```", mSetObj$meta.pval.method, "```."),
                paste("The user's selected pathway library is: ```", mSetObj$paramSet$lib, "```."),
                "\n\n");
        .buffer_add(integ.desrc, collapse="\n");
      }
    } else {
      # pooling peaks
      pval.descr <- c(
                "The selected method for meta-analysis is ```Pooled Peaks```.",
                paste("The selected p-value cutoff: ```", mSetObj$pooled_cutoff, "```."),
                paste("The user's selected pathway library is: ```", mSetObj$paramSet$lib, "```."),
                "\n\n");
      .buffer_add(pval.descr, collapse="\n");
    }
  }
}

#'Create analysis report: Functional Meta-Analysis Results
#'@description Report generation using Sweave
#'Mummichog analysis report overview
#'@param mSetObj mSetObj
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaMummichogResults <- function(mSetObj){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$metaLevel == "pathway"){
    
    if(!is.null("mSetObj$meta_results")){
      descr <- c("## 4. Integration Results\n\n",
                 "The aim of the Pathway-Level Integration method is to improve the biological ",
                 "consistency across studies to identify a robust meta-signature for the phenotype in question.",
                 " For pathway-level integration, each individual study undergoes the steps of calculating m/z level statistics and", 
                 " putative metabolite annotation, followed by pathway activity prediction to ultimately ",
                 " create a unified matrix of pathway-level results (keeping only pathways found across all-studies).", 
                 " Pathway activity scores are then combined using one of several p-value integration methods. \n\n");
      .buffer_add(descr, collapse="\n");
    }
    
    if(!is.null(mSetObj$imgSet$mummi.meta.path.plot)) {
      
      descr <- c("\n#### Graphical Summary\n\n",
                 "The bubble plot below represents the results of the Pathway-Level Integration. ",
                 "The plot displays all matched pathways per study as circles.", 
                 "The color and size of each circle corresponds to its p-value and enrichment factor, ",
                 "respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant", 
                 " hits and the expected number of hits within the pathway. \n\n");
      .buffer_add(descr, collapse="\n");
      
      fig_mmm1 <- fig.count <<- fig.count+1;
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- paste0('<div style="text-align: center; padding-left: 55%;">',
                            '<a href="', link, '&format=pdf&imgCmd=meta_bubble" target="_blank">PDF</a> ',
                            '<a href="', link, '&format=svg&imgCmd=meta_bubble" target="_blank">SVG</a>',
                            '</div>')
      .buffer_add(reportLinks, collapse="\n");
      
      fig <- c(paste0("```{r figure_mmm1, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mmm1, 
                      ". Summary of Pathway-Level Integration Meta-Analysis', ",
                      " fig.lp='", 
                      mSetObj$imgSet$mummi.meta.path.plot, 
                      "', out.width = '", getFigWidth(mSetObj), "'}"),
               "safeIncludeGraphics(mSetObj$imgSet$mummi.meta.path.plot)",
               "```",
               "\n\n");
      .buffer_add(fig, collapse="\n");
      
      descr <- c("\n#### Pathway-Level Meta-Analysis Results\n\n",
                 "The output of the Pathway-Level Integration Meta-Analysis consists of ",
                 "a table of results containing ranked pathways that are enriched in ",
                 "the user-uploaded datasets. The table includes the raw p-values (per individual study), ",
                 " the mean enrichment ratio and finally the integrated p-value (Meta.P). \n");
      .buffer_add(descr, collapse="\n");
      
      table.count <<- table.count+1;
      
        cmdhist2 <- c(
          "```{r table_mmm1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt <- CreateMummichogMetaAnalPathTableRMD(mSet);",
          paste0("create_dt(sum_dt, 'Table ", 
                 table.count, 
                 ". Results of the Pathway-Level Integration Meta-Analysis.')"),
          "```", 
          "\n\n"
        );

      
      .buffer_add(cmdhist2, collapse="\n");
    }
  } else {
    
    descr <- c("## 4. Pooled Peaks Results\n\n",
               "The aim of of the Pooling Peaks method is to computationally combine the outputs from ",
               "different instruments that measure the same set of samples.", 
               " In this case, all uploaded peaks are merged into a single input for putative compound ",
               "annotation (with consideration for different mass tolerances)", 
               " followed by pathway activity prediction. This method should be used when samples are ",
               "homogeneous but instruments are complementary,",  
               " for instance samples that comes from the same lab but were obtained using different columns, ",
               "extraction methods or data collected using complementary LC-MS instruments. \n\n");
    .buffer_add(descr, collapse="\n");
    
    CreateMummichogAnalysisDoc(mSetObj)
  }
}


####################################################
####################################################

#'Create mummichog analysis report
#'@description Report generation using Sweave
#'Mummichog analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMummichogAnalysisDoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$mummi.resmat)){
    
    # Because mummi.resmat is created for meta-analysis
    if(!is.null(mSetObj$imgSet$mummi.plot)){
      
      descr <- c("\n\n### - Pathway Analysis Graphical Summary\n\n",
                 "The pathway summary plot below displays all matched pathways as circles. ",
                 "The color and size of each circle corresponds", 
                 " to its p-value and enrichment factor, respectively. The enrichment factor ",
                 "of a pathway is calculated as the ratio between the number of significant", 
                 " pathway hits and the expected number of compound hits within the pathway. \n");
      .buffer_add(descr);
      
      fig_mmm2 <- fig.count <<- fig.count+1;
      # peaks_to_paths
      link <- GetSharingLink(mSetObj)
      reportLinks <- getReportLinks(link, "meta_peaks_to_paths", "peaks_to_paths");

      .buffer_add(reportLinks);
      .buffer_add("\n\n");
      mSet$paramSet$anal.type <<- "mummichog";
      fig <- c(paste0("```{r figure_mmm2, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mmm2, 
                ". Summary of Meta-Mummichog Pathway Analysis', ",
                " fig.lp='", 
                mSetObj$imgSet$mummi.plot, 
                "', out.width = '", getFigWidth(mSetObj), "'}"),
         "if (mSetObj$paramSet$report.format == 'html') {",
         "  PlotPeaks2Paths(NA, interactive=T)",
         "} else {",
         "  safeIncludeGraphics(mSetObj$imgSet$mummi.plot)",
         "}",
         "```",
         "\n\n");

      .buffer_add(fig, collapse="\n");
      .buffer_add("\n\n", collapse="\n");
    }
    
    descr <- c("\n\n### - Pathway Analysis Results Table\n\n",
               "The output of the mummichog analysis consists of a table of results containing ranked pathways that are enriched in ",
               "the user-uploaded data. The table includes the total number of hits per pathway (all, significant, and expected), the raw p-values (",
               "Hypergeometric), and the p-value modeled on user data using a Gamma distribution. \n");
    .buffer_add(descr);
    
    .buffer_add("\n\n");
    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mmm2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateMummichogAnalTable(mSet);",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Results of the Mummichog Pathway Analysis.')"),
      "```", "\n\n");
    
    .buffer_add(cmdhist2, collapse="\n");
    .buffer_add("\n\n");
  }
  
  if(!is.null(mSetObj$mummi.gsea.resmat)){
    descr <- c("\n\n## GSEA Pathway Analysis Results\n\n",
               "The output of the GSEA analysis consists of a table of results containing ranked pathways that are enriched in ",
               "the user-uploaded data. The table includes the total number of hits (all and expected), their raw p-values and adjusted p-values.",
               " On the web, user's can explore the results in an interactive volcano plot. \n\n");
    .buffer_add(descr);
    
    # Because mummi.gsea.resmat is created for meta-analysis
    if(!is.null(mSetObj$imgSet$mummi.gsea.plot)){

      descr <- c("\n\n### - Pathway Analysis Graphical Summary\n\n",
                 "The GSEA pathway summary plot below displays all matched pathways as circles. The color and size of each circle corresponds", 
                 " to its p-value and enrichment factor, respectively. The enrichment factor of a pathway is calculated as the ratio between the number of significant", 
                 " pathway hits and the expected number of compound hits within the pathway. \n\n");
      .buffer_add(descr);
      
      fig_mmm3 <- fig.count <<- fig.count+1;
      # peaks_to_paths
      link <- GetSharingLink(mSetObj);

      
      reportLinks <- getReportLinks(link, "meta_peaks_to_paths_gsea", "peaks_to_paths_gsea");
      .buffer_add("\n\n");
      
      mSet$paramSet$anal.type <<- "gsea";

    fig <- c(
      paste0("```{r figure_mmm3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mmm3, 
             ". Summary of Meta-GSEA Pathway Analysis', ",
             "fig.lp='fig:", fig_mmm3, "', out.width='", getFigWidth(mSetObj), "'}"),
      "if (mSetObj$paramSet$report.format == 'html') {",
      "  PlotPeaks2Paths(NA, interactive=T)",
      "} else {",
      "  safeIncludeGraphics(mSetObj$imgSet$mummi.gsea.plot)",
      "}",
      "```",
      "\n\n"
    )

    cat(fig, sep = "\n")

      .buffer_add(fig, collapse="\n");
      .buffer_add("\n\n", collapse="\n");
    }
    
    .buffer_add("\n\n");
    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mmm3, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateGSEAAnalTable(mSet);",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Results of the GSEA Pathway Analysis.')"),
      "```", "\n\n");
    
    .buffer_add(cmdhist2, collapse="\n");
    .buffer_add("\n\n");
  }
  
  if(!is.null(mSetObj$integ.resmat)){
    descr <- c("\n\n## Meta-Analysis of mummichog and GSEA results\n\n",
               "The MS Peaks to Paths module uses the (Fisher's method) for combining the mummichog and GSEA p-values.", 
               "It takes the raw p-values per pathway to perform p-value combination.",
               "The Integrated MS Peaks to Paths plot below summarizes the results of the", 
               " Fisher's method for combining mummichog (y-axis) and GSEA (x-axis) p-values. ",
               "The size and color of the circles correspond to their transformed combined p-values.",
               " Large and red circles are considered the most perturbed pathways.",
               " The blue and pink areas highlight the significant pathways based on either GSEA ",
               "(pink) or mummichog (blue), and the purple area highlights significant pathways identified by both algorithms. \n\n");
    .buffer_add(descr);
    
    fig_mmm3 <- fig.count <<- fig.count+1;
    
    # integ_peaks
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "meta_integ_peaks", "integ_peaks");

    .buffer_add(reportLinks);
    .buffer_add("\n\n");

    fig <- c(paste0("```{r figure_mmm3, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_mmm3, 
                    ". Summary of Pathway Analysis', ",
                    " fig.lp='", 
                    mSetObj$imgSet$integpks.plot, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "safeIncludeGraphics(mSetObj$imgSet$integpks.plot)",
             "```",
             "\n\n");
    .buffer_add(fig, collapse="\n");
    .buffer_add("\n\n", collapse="\n");
    
    
    .buffer_add("\n\n");
    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_mmm2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "sum_dt <- CreateMetaAnalTableRMD(mSet);",
      paste0("create_dt(sum_dt,  caption = 'Table ", 
             table.count, 
             ". Meta-Analysis of Mummichog and GSEA Results.')"),
      "```", "\n\n");
    
    .buffer_add(cmdhist2, collapse="\n");
    .buffer_add("\n\n");
  }
  
  descr <- c("\n\n### - Compound Matching Table\n\n",
             "The output of the MS Peaks to Pathways module also consists of a comprehensive table containing the compound matching", 
             " information for all user-uploaded m/z features. The table has four columns, containing the Query.",
             " Mass of each feature, the predicted Matched.Compound for each feature,",
             "the Matched.Form, and the Mass.Diff. As the file can be very long (>40 pages), please ",
             "download it separately on the Downloads page of MetaboAnalyst. \n\n");
  .buffer_add(descr);
 
}

detectPairedElements <- function(sampleNames) {
  # Create a list to store base names without pair suffixes
  baseNames <- sapply(sampleNames, function(name) {
    # Split by underscore and take the first part
    strsplit(name, "_")[[1]][1]
  })
  
  # Count occurrences of each base name
  nameCounts <- table(baseNames)
  
  # Check if all elements have a paired counterpart (each base name appears exactly twice)
  all(nameCounts == 2)
}

detectPairedElementsUnique <- function(sampleNames) {
  # Create a list to store base names without pair suffixes
  baseNames <- sapply(sampleNames, function(name) {
    # Split by underscore and take the first part
    strsplit(name, "_")[[1]][1]
  })
  
  # Get unique base names
  uniqueBaseNames <- unique(baseNames)
  
  # Return the list of unique base names
  return(uniqueBaseNames)
}

CreateKeggNetDocMetaMummi <-function(mSetObj){
  
  if (!is.null(mSetObj$imgSet$reportSet$network_mummichog) && safeFileExists(mSetObj$imgSet$reportSet$network_mummichog)) {
    if(mSetObj$paramSet$report.format == "slides"){
      # Insert Network Image
      networkImageSlide <- c(
        "## KEGG Global Metabolic Network Visualization\n\n",
        "```{r figure_kegg_network, echo=FALSE, fig.cap='', out.width='100%'}",
        sprintf("safeIncludeGraphics('%s')", mSetObj$imgSet$reportSet$network_mummichog),
        "```\n",
        "\n\n---\n\n"
      )
      .buffer_add(networkImageSlide, collapse="\n")
      
    }else{
      descr <- c("\n\n#### Global KEGG Metabolic Network\n\n",
                 "This interactive visualization allows users to interactively view their data in a global KEGG metabolic network.", "\n\n");
      .buffer_add(descr);
      
      link <- GetSharingLink(mSetObj)
      reportLinks <- getReportLinks(link, "network_mummichog")
      .buffer_add(reportLinks);
      .buffer_add("\n\n");
      
      img <- c(
        paste0("```{r figure_kegg_net, echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ",
               getFigCount() , 
               ": KEGG global metabolic network', out.width='", getFigWidth(mSetObj), "'}"),
        sprintf("safeIncludeGraphics('%s')", mSetObj$imgSet$reportSet$network_mummichog),
        "```",
        "\n\n")
      
      .buffer_add(img, collapse="\n");
    }
  }
}