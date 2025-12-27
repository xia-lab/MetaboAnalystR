#'Create report of analyses (Meta-Analysis)
#'@description Report generation using Sweave
#'Puts together the analysis report
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisRnwReport_slides<-function(mSetObj, usrName, link){
  
  CreateHeader(usrName);
  CreateMetaAnalysisIntr_slides();
  
  CreateMetaAnalysisInputDoc_slides(mSetObj);
  CreateMetaAnalysisNORMdoc_slides(mSetObj);
  CreateMetaAnalysisDEdoc_slides(mSetObj);
  
  CreateMetaAnalysisOutput_slides(mSetObj);
  CreateUpSetDoc_slides(mSetObj)
  AddFeatureImages_slides(mSetObj);

  CreateSlideFooter();
  
}

#'Create MetaAnalysis analysis report: Introduction  
#'@description Report generation using Sweave
#'MetaAnalysis analysis report introduction
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisIntr_slides <- function(mSetObj) {
    # Check if table and figure counters exist, if not initialize them
    if(!exists("table.count")) {
        table.count <<- 0;
    }
    if(!exists("fig.count")) {
        fig.count <<- 0;
    }
  
    # Introduction to Meta Analysis
    introSlide <- c(
        "## Selected Module: Statistical Meta-Analysis\n\n",
        "- This module supports meta-analysis of datasets prepared under similar experimental conditions.",
        "- It is tailored for two-group comparisons (e.g., control vs. disease).",
        "- Acceptable data formats include compound concentration tables, spectral binned data, or peak intensity tables.")

    .buffer_add(introSlide, collapse="\n")
}

#'Create MetaAnalysis analysis report: Data Input
#'@description Report generation using Sweave
#'Power analysis report, data input documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisInputDoc_slides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);

    # Slide 2: Data integrity check
    integrityCheckSlide <- c(
        "## Data Integrity Check\n\n",
        "- Preliminary data quality assessment ensures all necessary information is included for analysis.",
        "- The process checks for appropriate class labels and deals with missing values, zeros, and negative values.",
        "- Ensures data consistency across datasets for accurate comparative analysis."
    )
    .buffer_add(integrityCheckSlide, collapse="\n")
    .buffer_add("\n\n---\n\n")  # Slide break
}


#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisNORMdoc_slides <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);

    # Slide content: Normalization and Autoscaling
    normalizationSlide <- c(
        "## Data Normalization\n\n"
    )
    # Check if there is a normalization message
    if(exists("norm.msg")){
        normDetails <- norm.msg;
    }else{
        normDetails <- "- No normalization methods were applied."
    } 

    # Check if autoscaling was performed
    if(mSetObj$dataSet$auto_opt == 1){
        autoscaleInfo <- "- Autoscaling of data was performed to ensure equal variance among features."
    }else{
        autoscaleInfo <- "- No data autoscaling was performed."
    }

    # Combine all elements into one slide content
    slideContent <- c(normalizationSlide, normDetails, autoscaleInfo)
    .buffer_add(slideContent, collapse="\n")
    .buffer_add("\n\n---\n\n")  # Slide break for next section
}




#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'Meta-Analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisDEdoc_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
  
    # Slide 1: Differential Expression Analysis Introduction
    deAnalysisDetails <- c(
        "## Differential Expression Analysis\n\n",
        "- Prior to meta-analysis, differential expression analysis using linear models (e.g., Limma) is performed for exploratory analysis.",
        "\n\n"
    )
    if(!is.null(mSetObj$dataSet[["deparam"]])){
        deDetails <- c("Differential Expression Parameters Applied: \n\n", mSetObj$dataSet$deparam, "\n\n")
    } else {
        deDetails <- "No differential expression analysis was performed.\n\n"
    }
    .buffer_add(deAnalysisDetails, deDetails, collapse="")
    .buffer_add("\n\n---\n\n")  # Slide break for next section
}

#'Create MetaAnalysis analysis report: Data Normalization
#'@description Report generation using Sweave
#'MetaAnalysis analysis, data normalization documentation. 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaAnalysisOutput_slides <- function(mSetObj = NA) {
    mSetObj <- .get.mSet(mSetObj)
    

    # Start building the meta-analysis method details
    metaMethod <- if(mSetObj$dataSet$metastat.method  == "metap") {
        "P-value Combination"
    } else if(mSetObj$dataSet$metastat.method  == "votecount") {
        "Vote Counting"
    } else {
        "Direct Merging"
    }

    # Initialize variables
    appliedMethod <- NULL
    minimumVotes <- NULL
    significanceThreshold <- mSetObj$dataSet$pvalcutoff  # This seems to be common to all methods

    # Check if meta-analysis method is 'metap' and assign the corresponding p-value method
    if(mSetObj$dataSet$metastat.method  == "metap" && !is.null(mSetObj$dataSet$pvalmethod)) {
        appliedMethod <- mSetObj$dataSet$pvalmethod
    }

    # Check if meta-analysis method is 'votecount' and assign the corresponding minimum vote count
    if(mSetObj$dataSet$metastat.method  == "votecount" && !is.null(mSetObj$dataSet$vote)) {
        minimumVotes <- mSetObj$dataSet$vote
    }


    # Meta-analysis Results Table
    if(!is.null(mSetObj$analSet$meta.mat)){

        # Prepare the table description with method details
        tableDescription <- paste0(
          "Table ", table.count, 
          ". Top-ranking features from meta-analysis using ", metaMethod, 
          if(!is.null(appliedMethod)) paste0(" (Method: ", appliedMethod, ")"),
          if(!is.null(minimumVotes)) paste0(" (Minimum Votes Required: ", minimumVotes, ")"),
          ". Significance Threshold: ", significanceThreshold, "."
        )

        title <- paste0("## Table ", 
                 table.count, 
                 tableDescription)
        .buffer_add(title, collapse="\n")
        table.count <<- table.count+1;

        cmdhist2 <- c(
          "```{r table_s1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt <- CreateMetaTableRMD(mSet);",
          paste0("create_dt(sum_dt)"),
          "```", 
          "\n\n"
        );
        .buffer_add(cmdhist2, collapse="\n")
        .buffer_add("\n\n---\n\n")  # Slide break for next section
    }

    # Box Plots for Selected Features
    if(!is.null(mSetObj$imgSet$meta.anal$plot)){
        for(s in 1:length(mSetObj$imgSet$meta.anal$ids)) {
            featureName <- mSetObj$imgSet$meta.anal$feature
            featureSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$meta.anal$plot[s], "Expression pattern of the selected feature between groups.")
            .buffer_add(featureSlide)
        }
    }

    # Venn Diagram Slide
    if(!is.null(mSetObj$imgSet$venn)){
        vennDiagramSlide <- CreateTwoColumnFigureSlide(mSetObj$imgSet$venn, "Venn diagram of differentially expressed features.")
        .buffer_add(vennDiagramSlide)
    }

    # Table for Venn Diagram DE Features
    if(!is.null(mSetObj$analSet$sigfeat.matrix)){
        .buffer_add(paste0("Table ",
                 table.count,
                 ". DE features by individual study and from meta-analysis.\n"))
        table.count <<- table.count+1;
        
        cmdhist2 <- c(
          "```{r table_s2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
          "sum_dt <- CreateVennMetaTableRMD(mSet);",
          paste0("create_dt(sum_dt)"),
          "```", 
          "\n\n"
        );
        .buffer_add(cmdhist2, collapse="\n");
        .buffer_add("\n\n---\n\n")
    }
}

#'Create MetaAnalysis table of results
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateMetaTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  metatable <- mSetObj$analSet$meta.mat;
  return(metatable)
}

#'Create MetaAnalysis table of results for Venn Diagram
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateVennMetaTableRMD <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$sigfeat.matrix);
}
