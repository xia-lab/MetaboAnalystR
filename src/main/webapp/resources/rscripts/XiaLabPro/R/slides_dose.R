#'Create report of Dose Response Analysis
#'@description Report generation using Sweave
#'Put together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateDoseRnwReport_slides <- function(mSetObj, usrName){
  
  CreateHeader(usrName);
  CreateDoseIntr_slides();
  
  CreateDataProcSlides(mSetObj);
  CreateNORMSlides(mSetObj);
  
  CreateDoseParametersDoc_slides(mSetObj);
  CreateDoseAnalDoc_slides(mSetObj);

  CreateSlideFooter();
}
CreateDoseIntr_slides <- function() {
  descr <- c(
    "## Overview of Dose–Response Analysis",
    "- **Purpose:** Commonly used in toxicology to understand how varying concentrations of a chemical affect a biological system.",
    "- **Key Metric:** Benchmark dose (BMD), the minimal dose causing a predefined change in biological response.",
    "- **Study Design:** Involves a control group and multiple dose groups with replicates, using dose-response curves for analysis.",
    "- **Procedure:** Consists of data upload, integrity checking, normalization, differential analysis, curve fitting, and BMD computation.",
    "- **Further Info:** For an in-depth understanding, refer to [Ewald et al.](https://doi.org/10.1093/bioinformatics/btaa700).",
    "\n\n---\n\n"
  )
  
  # Assuming 'cat' writes to a Markdown file for slides
  cat(descr, file = rmdFile, append = TRUE, sep = "\n\n")
}

#'Create dose response analysis report: Parameter Selection
#'@description Report generation using Sweave
#'Power analysis report, parameter selection
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
CreateDoseParametersDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)

    # Slide content
    slide_content <- c(
        "## Identification of Potential Features Associated with Dose",
        "- **Omics Data Complexity:** Unlike traditional dose-response analysis, omics data contain thousands of features. Thus, screening is necessary to identify features showing significant variations in any dose treatment compared to the control (dose 0).",
        "- **Screening Method:** Achieved using ANOVA-like tests based on the well-established Limma package for linear modeling in omics data analysis.",
        "- **Volcano Plot:** Features can be selected based on fold-change and/or p-values. For computational reasons, we recommend adjusting thresholds to ensure less than 1000 features pass this step.",
        paste("Figure", fig_ppw <- fig.count <<- fig.count + 1, "shows the average fold change between each dose and the control."),
        "\n\n---\n\n"
    )
    cat(slide_content, file = rmdFile, append = TRUE, sep = "\n\n")

    # Volcano plot slide using CreateTwoColumnFigureSlide
    slide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$dose_volcano_filename, "Volcano plot showing significant features identified from DE analysis.")
    cat(slide, file = rmdFile, append = TRUE)

}

#'Create dose response analysis report: BMD Analysis and results
#'@description Report generation using Sweave
#'Power analysis report, analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
CreateDoseAnalDoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    link <- GetSharingLink(mSetObj)
    imgSet <- mSetObj$imgSet

    # Slide content
    slide_content <- c(
        "## Dose Response Analysis",
        "- **Curve Fitting:** Following the selection of significant features, curve fitting is performed to calculate feature-level benchmark doses (BMD). This involves fitting the intensity or concentration values of selected features to a suite of linear and non-linear models.",
        "- **Model Selection:** The module supports 10 statistical models, with the best fitting model chosen based on Akaike Information Criterion (AIC). Poor fits are filtered out. For most cases, we recommend excluding Poly3 and Poly4 unless a non-monotonic response is expected.",
        "- **Computational Limits:** The process is computationally intensive. If using the public server, a maximum of 1000 features can be used for curve fitting.",
        "- **Bar Plot Summary:** The bar plot below shows how many times each model was found to be the best-fitting model for a gene. In most cases, models without a BMD indicate gene expression levels that never exceed the control's standard deviation.",
        paste("Figure", fig_panar1 <- fig.count <<- fig.count + 1, "shows the summary of the model fitting results."),
        "\n\n---\n\n"
    )

    cat(slide_content, file = rmdFile, append = TRUE, sep = "\n\n")

    # Bar plot slide using CreateTwoColumnFigureSlide
    slide <- CreateTwoColumnFigureSlide(imgSet$PlotDRModelBars, "Frequency of statistical models among best fit curves")
    cat(slide, file = rmdFile, append = TRUE)

    # Density plot slide using CreateTwoColumnFigureSlide
    slide <- CreateTwoColumnFigureSlide(imgSet$PlotDRHistogram, "Metabolite-level BMDs histogram")
    cat(slide, file = rmdFile, append = TRUE)

    # Results Table slide (if applicable)
    if (!is.null(mSetObj$dataSet$comp.res)) {
        tableName <- "Detailed result table of feature-level BMDs"
        tableStr <- "dt_res <- as.data.frame(mSet$dataSet$comp.res)"
        CreateTitleTableSlide(tableStr, tableName)
    }

    # Adding dose feature images
    AddDoseFeatureImages_slides(mSetObj)
}

AddDoseFeatureImages_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    imgSet <- mSetObj$imgSet

    # Check if the doseFeatureList exists and is not empty
    if (!is.null(imgSet$doseFeatureList) && length(imgSet$doseFeatureList) > 0) {
        descr <- c(
            "## Feature of Interest",
            "Below, you will find the curve fitting result of key features that have been saved during the analysis.",
            "\n\n---\n\n"
        )
        cat(descr, file = rmdFile, append = TRUE, sep = "\n\n")
    } else {
        descr <- c(
            "## Top Features",
            "Below, you will find the curve fitting result of the top 5 features based on estimated benchmark dose.",
            "\n\n---\n\n"
        )
        imgSet$doseFeatureList <- list()
        resTable <- GetFitResultMatrix()
        ids <- GetFitResultFeatureIDs()
        models <- GetFitResultModelNms()

        for (i in 1:5) {
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
        cat(descr, file = rmdFile, append = TRUE, sep = "\n\n")
    }
    print(imgSet$doseFeatureList);
    # Image rendering in R Markdown
    for (i in seq_along(imgSet$doseFeatureList)) {
        slide <- CreateTwoColumnFigureSlide(imgSet$doseFeatureList[[i]], paste("Curve fitting result for feature:", imgSet$doseFeatureList[[i]]))
        cat(slide, file = rmdFile, append = TRUE)
    }
}
