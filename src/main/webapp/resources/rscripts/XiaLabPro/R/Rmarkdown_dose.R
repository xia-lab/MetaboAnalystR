#'Create report of Dose Response Analysis
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDoseRnwReport <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateDoseIntr();
  
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  CreateDoseParametersDoc(mSetObj);
  CreateDoseAnalDoc(mSetObj);

  # this is for BMD for individual features when user click or default to top 4?
  AddFeatureImages(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
}

#'Create dose response analysis report: Introduction  
#'@description Report generation using Sweave
#'Power analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDoseIntr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr <- c("## 1. Overview\n\n",
             "Dose–response analysis is commonly used in toxicology and pharmacology for understanding how varying concentrations of a chemical can impact a biological system. 
              It plays a pivotal role in risk assessment of chemical exposures. Chemicals identified from exposomics studies are often followed up by in vitro or in vivo 
              dose–response studies for validation as well as to understand their mechanism of action or adverse outcome pathways. A key output of dose-response analysis is 
              the benchmark dose (BMD), the minimum dose of a substance that produces a clear, low level health risk relative to the control group. 
              The BMD is inferred by curve fitting based on data obtained from dose–response experiment.", 
             "\n\n",
             "Dose–response experiment design includes a control group (dose = 0) and at least three different dose groups, typically with the same number of replicates in each group. 
             The analysis workflow consists of four main steps: (i) data upload, integrity checking, processing and normalization; (ii) differential analysis to select features that vary with dose levels; 
             (iii) curve fitting on the intensity or concentration values of those selected features against a suite of linear and non-linear models, and 
             (iv) computing BMD values for each feature based on the selected model. The algorithm for dose–response analysis was adapted from the algorithm we developed for transcriptomics BMD analysis. 
             For more information, please refer to the paper by <a href='https://doi.org/10.1093/bioinformatics/btaa700' target='_blank'>Ewald et al.</a>.",
             "\n\n");
  cat(descr, file=rmdFile, append=TRUE);
}


#'Create dose response analysis report: Parameter Selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDoseParametersDoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
    link <- GetSharingLink(mSetObj)

  descr <- c("\n\n<hr/>",
             "\n\n## 3. Identification of potential features associated with dose\n\n",
             "Unlike traditional dose response analysis which usually deals with a few chemicals, omics data contain 1000s of features. 
             Therefore, before proceeding to the dose response analysis, it is necessary to perform screening to identify features that 
             have shown significant variations in ANY of the dose treatment with respective the control (dose 0). 
             This is achieved using a general ANOVA-like tests based on the well established limma (linear modeling for omics data analysis).", 
             "\n\n",
             paste("Figure", fig_ppw <- fig.count<<-fig.count+1, " summarizes the results in a volcano plot based on their average fold changes against their p-values."),
             "Since these features will be subject to additional filters during the curve-fitting step, you can use relaxed thresholds. 
             If there is a strong effect, we recommend increasing the threshold values to control the total number of features to be 
             evaluated in the next step.\n");

  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
  link <- GetSharingLink(mSetObj)
  reportLinks <- getReportLinks(link, "dose_volcano");

  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  fig <- c(paste0(
        "```{r figure_dose_volcano, echo=FALSE, fig.pos='H', fig.cap='Figure ", fig_ppw, 
        ". Significant feature selection based on a volcano plot. A total of`", sum(mSetObj$analSet$dose$inx.up, mSetObj$analSet$dose$inx.down),"` significant features identified from DE analysis.', ",
        " fig.lp='H', ",  # This sets the LaTeX placement specifier to 'H'
        "out.width = '", getFigWidth(mSetObj), "'}"
      ),
      "knitr::include_graphics(mSetObj$imgSet$dose_volcano_filename)",
      "```",
      "\n\n"
  )

  cat(fig, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
  
}

#'Create dose response analysis report: BMD Analysis and results
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDoseAnalDoc <- function(mSetObj){

  mSetObj <- .get.mSet(mSetObj);
  link <- GetSharingLink(mSetObj)

  imgSet <- mSetObj$imgSet;
  descr <- c("\n\n<hr/>",
             "\n\n## 4. Dose Response Analysis\n\n",
             "Dose response analysis aims to estimate BMDs of those selected features. The process uses the doses (x) and intensity or concentration values (y)
              against a suite of linear and non-linear models, and compute BMD values for each feature based on the selected model. The module currently supports 10 statistical models that can be 
              fit to the concentration of each compound. Any model with a poor fit are filtered out, and then the best fitting model is chosen based on AIC (Akaike Information Criterion). 
              The selected fit is used to compute the BMD.",
              "\n\n",
              "We recommend selecting all statistical models except for Poly3 and Poly4, which should only be used if you expect a non-monotonic response. 
              These higher order polynomials should be used with caution since they sometimes have unpredictable behavior, especially for dose-response experiments 
              with a log-scale dosing scheme.",
             "\n\n",
             paste("Figure", fig_panar1 <- fig.count<<-fig.count+1, " is a bar plot shows how many times each model was found to be the best-fitting model for a feature. "), 
             "In most cases, the reason that a model fit does not have a BMD is that the feature abundance never exceeds the standard deviation of the control. 
             You can view the detailed curve fitting results of each feature in the detailed result table.",
            "\n\n");

  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  reportLinks <- getReportLinks(link=link, analNavi="PlotDRModelBars");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
     # show the curve fitting summary image
  bar.plot <- c(paste0(
                  "```{r figure_best_fit_models, echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ", fig_panar1, ": Frequency of statistical models among best fit curves', ",
                  "fig.lp='h', fig.pos='H',out.width = '",getFigWidth(),"'}"
                ),
                sprintf("knitr::include_graphics('%s')", imgSet$PlotDRModelBars),
                "```",
                "\n\n"
  )
    
    cat(paste(bar.plot, "\n\n"), file=rmdFile, append=TRUE, sep="\n"); 

  reportLinks <- getReportLinks(link=link, analNavi="PlotDRHistogram");
  cat(reportLinks, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
hist.plot <- c(
  "Below is a density plot showing the distribution of data, in this case, metabolite-level BMDs. It is normalized so that the area under the curve is equal to 1, and thus the units on the y-axis are of little interest. The colored vertical lines show the metabolomic-level BMDs. The main statistic of interest is usually the dose at which the whole metabolome is responding to an exposure, or the metabolomic point-of-departure (mPOD). This is adapted from the concept of transcriptomic point-of-departure (tPOD). \n\n",
  paste0(
    "```{r figure_fit_histo, echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ", getFigCount(), ": Metabolite-level BMDs histogram. 20th feature: the mPOD is equal to the 20th lowest geneBMD value. 10th percentile: the mPOD is equal to the 10th percentile of the geneBMD values. Max 1st peak: the mPOD is equal to the mode of the first peak in the distribution of geneBMD values.', ",
    "fig.lp='h', fig.pos='H', out.width = '", getFigWidth(), "'}"
  ),
  sprintf("knitr::include_graphics('%s')", imgSet$PlotDRHistogram),
  "```",
  "\n\n"
)

  
    cat(hist.plot, file=rmdFile, append=TRUE, sep="\n")
  cat("\n\n", file=rmdFile, append=TRUE);

    table.count <<- table.count+1;
    descr <- c(
      "```{r table_paa, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "dt_res <- as.data.frame(mSet$dataSet$html.resTable);",
      "rownames(dt_res) <- NULL",
      paste0("create_dt(dt_res,  caption = 'Table ", table.count,
             ". Detailed result table of feature-level BMDs', table.name='fitres')"),
      "```", "\n\n")
    
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n");
  cat("\n\n", file=rmdFile, append=TRUE);


   # show the BMD from selected metabolites or top 4 
    AddDoseFeatureImages(mSetObj);
    

}

AddDoseFeatureImages <- function(mSetObj=NA) {
  #save.image("doseFeature.RData");
  mSetObj <- .get.mSet(mSetObj)
  link <- GetSharingLink(mSetObj)
  imgSet <- mSetObj$imgSet
  
  # Check if the doseFeatureList exists and is not empty
  if (!is.null(imgSet$doseFeatureList) && length(imgSet$doseFeatureList) > 0) {    
    # Descriptive text about the dose feature list
    descr <- c(
      "## Feature of interest", "\n",
      "Below, you will find the curve fitting result of key features that have been saved during the analysis.",
      "\n"
    )
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n")
    cat("\n\n", file=rmdFile, append=TRUE)
  } else {
    # Descriptive text about the top features
    descr <- c(
      "## Top features", "\n",
      "Below, you will find the curve fitting result of the top 10 features based on estimated benchmark dose.",
      "\n"
    )
    imgSet$doseFeatureList <- list()
    resTable <- GetFitResultMatrix()
    ids <- GetFitResultFeatureIDs()
    models <- GetFitResultModelNms()
    #print(ids);
    nFeat <- min(10L, length(ids))
    if (nFeat == 0L) {
      warning("No features returned by GetFitResultFeatureIDs(); nothing to plot.")
      return(invisible(NULL))
    }

    for (i in seq_len(nFeat)) {
      imgName <- PlotMetaboliteDRCurve(
        mSetObj = NA, 
        feat.id = ids[i], 
        feat.lbl = ids[i], 
        model.nm = models[i], 
        b = resTable[i, "b"], 
        c = resTable[i, "c"], 
        d = resTable[i, "d"], 
        e = resTable[i, "e"], 
        bmdl = resTable[i, "BMDl"], 
        bmd = resTable[i, "BMD"], 
        bmdu = resTable[i, "BMDu"], 
        scale = "natural", 
        dpi = 72, 
        format = "png"
      )
      imgSet$doseFeatureList[[ids[i]]] <- imgName
    }
    cat(descr, file=rmdFile, append=TRUE, sep="\n\n")
    cat("\n\n", file=rmdFile, append=TRUE)
  }

  # Image rendering in R Markdown
  img_blocks <- list()
  for (i in seq_along(imgSet$doseFeatureList)) {
    imgCmd <- gsub(".png", "", imgSet$doseFeatureList[[i]])
    reportLinks <- paste0('<div style="text-align: center;">',
                          '<a href="', link, '&format=pdf&imgCmd=', imgCmd, '" target="_blank">PDF</a> ',
                          '<a href="', link, '&format=svg&imgCmd=', imgCmd, '" target="_blank">SVG</a>',
                          '</div>')
    
    if(mSetObj$paramSet$report.format == "pdf"){
        figCap <- gsub("_", "\\\\\\\\_", imgSet$doseFeatureList[[i]]);
    }else{
        figCap <- imgSet$doseFeatureList[[i]];
    }

    chunkOptions <- paste0(
      "```{r figure_feature_dose", i ,", echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ", 
      getFigCount(), ": ", figCap, "', out.width='", getFigWidth(mSetObj, "75%"), "'}"
    )

    img_block <- paste0("<div style='display: flex; flex-direction: column; width: 75%; margin: 10px;'>",
                        reportLinks, 
                        "\n\n", chunkOptions, "\n",
                        sprintf("knitr::include_graphics('%s')", imgSet$doseFeatureList[[i]]),
                        "\n```\n",
                        "</div>")
    img_blocks[[i]] <- img_block
  }
  
  # Create HTML structure for a grid layout with two units per row
  html_start <- '<div style="display: flex; flex-wrap: wrap; gap: 10px;">'
  html_end <- '</div>'
  html_content <- c(html_start)
  
  for (i in seq(1, length(img_blocks), by=2)) {
    html_content <- c(html_content, 
                      '<div style="display: flex; width: 100%;">',
                      img_blocks[[i]],
                      if (i+1 <= length(img_blocks)) img_blocks[[i+1]] else '',
                      '</div>')
  }
  
  html_content <- c(html_content, html_end)
  
  cat(html_content, file = rmdFile, append = TRUE, sep="\n")
}
