
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

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichRmdReport<-function(mSetObj, usrName){
  mSetObj <- .get.mSet(mSetObj);

  CreateHeader(usrName);
  CreateEnrichIntr();
   
  CreateEnrichProcessDoc(mSetObj);

  if(mSetObj$analSet$type == "msetssp"){
    CreateEnrichSSPdoc(mSetObj);
  }
  if(mSetObj$analSet$type == "msetqea"){
    CreateNORMdoc(mSetObj);
  }

  CreateEnrichAnalDoc();
  
  if(mSetObj$analSet$type == "msetqea"){
    CreateEnrichQEAdoc(mSetObj);
  }
  if(mSetObj$analSet$type == "msetora"){
    CreateEnrichORAdoc(mSetObj);
  }

  CreateNetworkDoc(mSetObj);
  AddFeatureImages(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report introduction
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichIntr <- function(){
  descr <- c("## 1. Overview \n\n",
             "MSEA or Metabolite Set Enrichment Analysis is a way to identify biologically meaningful 
             patterns that are significantly enriched in targeted or quantitative metabolomic data. 
             MSEA can investigate if a set of functionally related metabolites (metabolite sets) are enriched in <u>a given list of 
             compounds</u> based on **over-representation analysis (ORA)**, or test whether the abundance profiles of these metabolite sets 
             are significantly associated with the study condition based on **GlobalTest** directly from a <u>compound concentration table</u>.",
             "\n\n");
  .buffer_add(descr, collapse="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report data input
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichInputDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("## 2. Data Processing\n\n",
             "Users can upload three different data types:\n",
            "* A list of compound names - as a one column data for ```Over Representation Analysis (ORA)```\n",
            "* A single measured human biofluid sample (urine, blood, CSF) for ```Single Sample Profiling (SSP)```\n", 
            "* A compound concentration table as a comma separated (.csv) file for ```Quantitative Enrichment Analysis (QEA)```\n",
            "\n");
  
  .buffer_add(descr);
  
  if(mSetObj$analSet$type == "msetora"){
    descr <- c("You selected Over Representation Analysis (ORA) which requires a list of compound",
               "names as input. \n\n"
    );
  }else if(mSetObj$analSet$type == "msetssp"){
    descr <- c("You selected Single Sample Profiling (SSP) which requires a two-column data from",
               "measurement of a single biofluid sample. Currently, only data from blood, urine and CSF",
               "can be analysed as restricted by the availability of reference concentration data. \n\n"
    );
  }else{
    descr <- c("You selected Quantitative Enrichment Analysis (QEA) which requires a concentration table.",
               "This is the most common data format generated from quantitative metabolomics studies.",
               "The phenotype label can be can be categorical (binary or multi-class) or continuous. \n\n"
    );
  }
  
  .buffer_add(descr, collapse="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report enrichment process
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichProcessDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("\n\n<hr/>\n\n",
            "## 2. Data Processing\n\n",
            GetNameMappingDoc(),
            "\n\n");
  .buffer_add(descr, collapse="\n");

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "name_map")
  .buffer_add(reportLinks);
  .buffer_add("\n\n");

  table.count <<- table.count+1;
  descr <- c("```{r table1, echo=FALSE, out.width = '60%', results='asis'}", 
           "dt1 <- as.data.frame(mSetObj$dataSet$map.table);",
           paste0("create_dt(dt1, 'Table ", 
           table.count, ". Result from Compound Name Mapping.', 'namemap')"),
           "```",
           "\n\n");

  
  .buffer_add(descr, collapse="\n");
  
 # descr <- c("The second step is to check concentration values. For SSP analysis,",
 #            "the concentration must be measured in *umol* for blood and CSF samples.",
 #            "The urinary concentrations must be first converted to *umol/mmol_creatinine*",
 #            "in order to compare with reported concentrations in literature. No missing or negative values",
 #            "are allowed in SSP analysis. The concentration data for QEA analysis is more flexible.",
 #            "Users can upload either the original concentration data or normalized data. Missing or negative values",
 #            "are allowed (coded as *NA*) for QEA. \n\n"
 # );
 # .buffer_add(descr, collapse="\n");

}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Single sampling profiling
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichSSPdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c("* **Concentration check**\n\n",
    "Single Sample Profiling (SSP) is designed to detect whether certain compound",
    "concentrations measured from a particular sample are higher or lower compared",
    "to their normal ranges reported in literature. Those selected",
    "compound list are then subject to over representation analysis (ORA) to see",
    "if certain biologically meaningful patterns can be identified. Please note,",
    "those reference concentrations could be measured from heterogeneous analytical",
    "technologies. It is advisable to compare only to those values that were",
    "measured by similar technologies by referring to the original literature.",
    "By default, if measured concentration values are higher or lower",
    "than **all** those reported concentration ranges will be selected for",
    "enrichment analysis. Users can overwrite the default selections by manually",
    "selecting or de-selecting them.",
    "\n\n",
    "**Table 2** shows the comparison between the measured concentrations",
    "and the reference concentrations. *L, M, H* means to the",
    "measured concentration are *Lower, Within (Medium), Higher* compared",
    "to the reference values. *0* means not selected for",
    "subsequent enrichment analysis, while *1* means the corresponding",
    "compound was selected.\n\n"
  );
  .buffer_add(descr, collapse="\n")
  
  table.count <<- table.count+1;
  
descr <- c(
    "```{r table_ssp, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "ssp.res<-mSetObj$analSet$ssp.mat[,-c(1,3,6)];",
    "rownames(ssp.res)<-mSetObj$analSet$ssp.mat[,1]",
    "selected.col<-rep(0, nrow(ssp.res));",
    "inx<-match(mSetObj$dataSet$cmpd, mSetObj$analSet$ssp.mat[,1]);",
    "selected.col[inx]<-1;",
    "dt_ssp <- as.data.frame(cbind(ssp.res, selected = selected.col));",
    paste0("create_dt(dt_ssp, 'Table ", table.count, ". Comparison with Reference Concentrations.')"),
    "```",
    "\n\n"
)

  
  .buffer_add(descr, collapse="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, analysis
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichAnalDoc <- function(){
  
  mSetObj <- .get.mSet(mSetObj);

  descr <- c("\n\n<hr/>",
             "\n\n## 3. Selection of a Metabolite Set Library",
             "\n\nBefore proceeding to enrichment analysis, a metabolite set library has to be chosen.",
             "There are seven built-in libraries offered by MSEA:\n\n"
  );
  .buffer_add(descr, collapse="\n");
  
  descr <- c("1. Metabolite set libraries based on pathways: ",
             " + SMPDB, containing 99 metabolite sets based on normal human metabolic pathways; ",
             " + KEGG, containing 84 metabolite sets based on KEGG human metabolic pathways; ",
             " + Drug-related, containing 461 metabolite sets based on drug pathways from SMPDB; ",
             " + RaMP, containing 3694 pathways from RaMP (integrating HMDB, KEGG, Reactome, WikiPathways); ",

             "2. Metabolite set libraries based on disease signatures from different tissues: ",
             " + Blood, containing 344 entries; ",
             " + Urine, containing 384 entries; ",
             " + CSF, containing 166 entries; ",
             " + Fecal samples, containing 44 entries; ",

             "3. Metabolite set libraries based on chemical structures: ",
             " + Super-class, containing 35 super chemical class metabolite sets or lipid sets ",
             " + Main-class, containing 464 main chemical class metabolite sets or lipid sets ",
             " + Sub-class, containing 1072 sub chemical class metabolite sets or lipid sets ",

             "4. Metabolite set libraries based on other definitions: ",
             " + SNPs, containing 4,598 metabolite sets based on their associations with SNPs loci.",
             " + Predicted, containing 912 metabolite sets based on computational enzyme knockout model ",
             " + Location, containing 73 metabolite sets based on locations",
             "\n\n")
  
  .buffer_add(descr, collapse="\n");
  
  descr <- c("The built-in libraries are mainly from human studies. For data from subjects other than human being, 
             users are suggested to upload their self-defined metabolite set libraries for enrichment analysis.
             The self-defined metabolite set library is simply a two-column comma separated text file with the
             first column for metabolite set names and the second column must use  **common compound name**
             separated by \"; \". You can use the *ID Conversion* program from Other Utilities to standardize the names.
             Please note the library must be comprehensive in order to get meaningful results.",
             "\n\n",
             paste("Your library choice: ```", mSetObj$analSet$msetlibname, "```"),
             "\n\n");
  .buffer_add(descr, collapse="\n");

  .buffer_add("\n\n<hr/>", "\n\n## 4. Enrichment Analysis\n\n", collapse="\n");
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report, over
#'representation analysis (ORA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichORAdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);



  descr <- c(
    "\n\nOver Representation Analysis (ORA) is performed when
    a list of compound names is provided. The list of compound list can be obtained through 
    conventional feature selection methods (i.e. significant metabolites), 
    from a clustering algorithm (i.e., co-regulated metabolites), or from the compounds showing abnormal concentrations 
    (i.e. those detected in SSP) to investigate if some biologically meaningful patterns can be identified.
    ORA was implemented using the *hypergeometric test* to evaluate whether a particular,
    metabolite set is represented more than expected by chance within the given compound list.
    One-tailed p values are provided after adjusting for multiple testing. The figures below
    summarizes the result.\n\n");
  .buffer_add(descr);

  if(!is.null(mSetObj$imgSet$ora)){

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "ora", "ora")
  .buffer_add(reportLinks);
  .buffer_add("\n\n");
  
  fig1 <- c(paste0("```{r figure_ora1, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), ". Summary plot for Over Representation Analysis (ORA).', ",
                  " fig.lp='", 
                  mSetObj$imgSet$ora, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$ora)",
           "```",
           "\n\n");
  
  .buffer_add(fig1, collapse="\n");
  }
  if(!is.null(mSetObj$imgSet$reportSet$enrichment_network) && safeFileExists(mSetObj$imgSet$reportSet$enrichment_network)){
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "enrichment_network")

    .buffer_add(reportLinks);
    .buffer_add("\n\n");

    fig2 <-c(paste0("```{r figure_ora2, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), ". Enrichment network for Over Representation Analysis (ORA).', ",
                    " fig.lp='", 
                    mSetObj$imgSet$ora_dot, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "safeIncludeGraphics(mSetObj$imgSet$reportSet$enrichment_network)",
             "```",
             "\n\n");

    .buffer_add(fig2, collapse="\n");
  }

    if(!is.null(mSetObj$imgSet$ora_dot)){

  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "ora_dot", "ora_dot");
  .buffer_add(reportLinks);
  .buffer_add("\n\n");
  
  fig3 <-c(paste0("```{r figure_ora3, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), ". Summary dot plot for Over Representation Analysis (ORA).', ",
                  " fig.lp='", 
                  mSetObj$imgSet$ora_dot, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$ora_dot)",
           "```",
           "\n\n");

  .buffer_add(fig3, collapse="\n");
  }
      if(!is.null(mSetObj$analSet$ora.mat)){

  reportLinks <- getReportLinks(link, "ora_resTbl");

  .buffer_add(reportLinks);
  .buffer_add("\n\n");

  table.count <<- table.count+1;
  
    descr <- c(
      "```{r table_ora, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_ora <- as.data.frame(mSetObj$analSet$ora.mat);",
      paste0("create_dt(dt_ora, 'Table ", 
             table.count, 
             ". Result from Over Representation Analysis.')"),
      "```", "\n\n"
    )


  .buffer_add(descr, collapse="\n");
}
}


#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report
#'Quantitative enrichment analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateEnrichQEAdoc<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  descr <- c(
    "Quantitative enrichment analysis (QEA) will be performed when the user uploads",
    "a concentration table. The enrichment analysis is performed using package",
    "[**globaltest**](https://academic.oup.com/bioinformatics/article/20/1/93/229017).",
    "It uses a generalized linear model to estimate a",
    "*Q-statistic* for each metabolite set, which describes the correlation",
    "between compound concentration profiles, X, and clinical outcomes, Y. The *Q statistic*",
    "for a metabolite set is the average of the Q statistics for each metabolite in the set.",
    "**Figure 2** below summarizes the result.\n\n"
  );
  .buffer_add(descr);
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "qea", "qea");

  .buffer_add(reportLinks);
  .buffer_add("\n\n");
  
  fig1 <-c(paste0("```{r figure_qea1, echo=FALSE, fig.pos='H', fig.cap='Figure 1. Summary plot for Quantitative Enrichment Analysis (QEA).', ",
                  " fig.lp='", 
                  mSetObj$imgSet$qea, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$qea)",
           "```",
           "\n\n");
  
  .buffer_add(fig1, collapse="\n");
  
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "qea_dot", "qea_dot")

  .buffer_add(reportLinks);
  .buffer_add("\n\n");
  
  fig2 <- c(paste0("```{r figure_qea2, echo=FALSE, fig.pos='H', fig.cap='Figure 2. Summary dot plot for Quantitative Enrichment Analysis (QEA).', ",
                  " fig.lp='", 
                  mSetObj$imgSet$qea, 
                  "', out.width = '", getFigWidth(mSetObj), "'}"),
           "safeIncludeGraphics(mSetObj$imgSet$qea_dot)",
           "```",
           "\n\n");
  
  .buffer_add(fig2, collapse="\n");

  
  if(!is.null(mSetObj$imgSet$reportSet$enrichment_network) && safeFileExists(mSetObj$imgSet$reportSet$enrichment_network)){
    
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "enrichment_network")

    .buffer_add(reportLinks);
    .buffer_add("\n\n");

    fig2 <-c(paste0("```{r figure_ora2, echo=FALSE, fig.pos='H', fig.cap='Figure ", getFigCount(), ". Enrichment network for Over Representation Analysis (ORA).', ",
                    " fig.lp='", 
                    mSetObj$imgSet$ora_dot, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "safeIncludeGraphics(mSetObj$imgSet$reportSet$enrichment_network)",
             "```",
             "\n\n");

    .buffer_add(fig2, collapse="\n");
  }
  
  reportLinks <- getReportLinks(link, "qea_resTbl")

  .buffer_add(reportLinks);
  .buffer_add("\n\n");


descr <- c(
    "```{r table_qea, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "res <- as.data.frame(mSetObj$analSet$qea.mat);",
    paste0("create_dt(res, 'Table ", 
           table.count, 
           ". Result from Quantitative Enrichment Analysis.')"),
    "```", 
    "\n\n"
)

  
  .buffer_add(descr, collapse="\n");
}
