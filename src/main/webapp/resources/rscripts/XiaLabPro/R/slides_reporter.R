
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

CreateDataProcSlides <- function(mSetObj = NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  # Slide start: Data Type
  .buffer_add("## Processing Parameters\n\n")
  .buffer_add("Data Type\n\n")
  # Determine and print data type in the desired format
    data.type <- switch(mSetObj$dataSet$type,
                        "conc" = "- `Concentration Table`",
                        "specbin" = "- `Binned Spectral Table`",
                        "pktable" = "- `Peak Intensity Table`",
                        "mass_table" = "- `Peak Intensity Table`",
                        "nmrpeak" = "- `NMR Peak List and Intensity Data`",
                        "mspeak" = "- `MS Peak List and Intensity Data`",
                        "mztab" = "- `mzTab-M 2.0 Format`",
                        "- `Peak Intensity Table`") # Default case

    .buffer_add(data.type, "\n\n")

    # Print the chosen method for missing value imputation in the desired format
    if (!is.null(mSetObj$msgSet$replace.msg)) {
        missingMsg <- paste("Missing Value Imputation Method:\n\n  - `", mSetObj$msgSet$replace.msg, "`", sep="")
        .buffer_add(missingMsg, "\n\n")
    }

    # Check if filtering was performed and summarize in the desired format
    if (is.null(mSetObj$msgSet$filter.msg)) {
        .buffer_add("Data Filtering:\n\n  - `No data filtering was performed.`\n\n")
    } else {
        filt.msg <- paste("Data Filtering Method:\n  - `", mSetObj$msgSet$filter.msg, "`", sep="")
        .buffer_add(filt.msg, "\n\n")
    }

  .buffer_add("\n---\n\n")  # Slide separator for ioslides or similar formats
}


GetNameMappingSlides <- function() {
    # Slide title
    mSetObj <- .get.mSet(NA)

    match_states <- mSetObj$name.map$match.state

    # Count the number of exact, approximate, and no matches
    total_exact_matches <- sum(match_states == 1)
    total_approximate_matches <- sum(match_states == 2)
    total_no_matches <- sum(match_states == 0)
    total_entries <- length(match_states)

    # Create the description with the results
    descr <- c("## Name Mapping\n\n",
               "- **Objective:** Standardize compound labels for consistency.",
               "- **Outcome:** Name mapping table (name_map.csv) can be found in your project folder.",
               paste("    - Exact match -", total_exact_matches),
               paste("    - Approximate match -", total_approximate_matches),
               paste("    - No match -", total_no_matches),
               "---\n\n")

    return(descr);
}

CreateNORMSlides <- function(mSetObj = NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  # Check if normalization has been performed
  if(is.null(mSetObj$dataSet$norm)){
    errorMsg <- c("## Data Normalization Not Performed\n\n",
                  "- It seems that data normalization has not been performed yet.",
                  "- Please choose a proper data normalization to proceed.",
                  "- You can also turn off normalization by selecting the `None` option.\n\n",
                  "---\n\n");
    .buffer_add(errorMsg, collapse="\n");
    return()
  }
  
  # Normalization Options Slide
  descr1 <- c("## Data normalization\n\n",
              "Normalization aims to make data more comparable across samples, and to transform them 
               to be more suitable for statistical analysis and visualization.",
              "\n\n",
              "The normalization consists of the following options:\n\n");
  .buffer_add(descr1)

  cat("- **Row-wise Procedures:**\n",
      "   + Sample specific (e.g., by dry weight, volume)\n",
      "   + By sample sum, median, reference sample\n\n",
      append=TRUE)
  
  cat("- **Data Transformation:**\n",
      "   + Log, square root, cube root transformations\n\n",
      append=TRUE)
  
  cat("- **Data Scaling:**\n",
      "   + Mean centering, auto scaling, pareto scaling, range scaling\n\n",
      append=TRUE)
  
  # Applied Normalization Methods Slide
  normMethod <- if(!is.null(mSetObj$dataSet[["rownorm.method"]])){
    paste(" + Row-wise normalization: `", mSetObj$dataSet$rownorm.method, "`\n",
          " + Data transformation: `", mSetObj$dataSet$trans.method, "`\n",
          " + Data scaling: `", mSetObj$dataSet$scale.method, "`\n\n",
          "---\n\n")
  } else {
    "- **Applied Methods:** No normalization methods were applied.\n\n---\n\n"
  }
  .buffer_add(normMethod)
  
  # Visualization Slide (if applicable)
  if(exists("norm", where=mSetObj$imgSet)){
    # Extract and write the caption as standalone text
    caption <- "Data distribution before and after normalization";
    slideContent <- CreateTitleFigureSlide(mSetObj$imgSet$norm, caption)
    .buffer_add(slideContent)
  }
}

CreateTitleFigureSlide <- function(figurePath, caption) {
  figCount <- getFigCount();
    slideContent <- c(
      sprintf("## Figure %d: %s", figCount, caption),
      sprintf("![](%s)", figurePath),
      "\n\n---\n\n"
    )
 
  return(paste(slideContent, collapse = "\n"))
}


CreateTwoColumnFigureSlide <- function(figurePath, caption) {
  figCount <- getFigCount();
  if(figCount %% 2 == 0){
    slideContent <- c(
      ":::::: {.columns}",
      "::: {.column width=34%}",
      sprintf("Figure %d: %s", figCount, caption),
      ":::",
      "::: {.column width=66%}",
      sprintf("![](%s)", figurePath),
      ":::",
      "::::::",
      "\n\n---\n\n"
    )
  }else{
    slideContent <- c(
      ":::::: {.columns}",
      "::: {.column width=34%}",
      sprintf("![](%s)", figurePath),
      ":::",
      "::: {.column width=66%}",
      sprintf("Figure %d: %s", figCount, caption),
      ":::",
      "::::::",
      "\n\n---\n\n"
    )
  }
  return(paste(slideContent, collapse = "\n"))
}


CreateSlideFooter <- function(){
    # Introduction to Biomarker Analysis
    end <- c(
        "## The End\n\n",
        "* For questions and support:",
        "  + Post your questions on the OmicsForum (https://omicsforum.ca)",
        "  + Contact us at support@xialab.ca",
        "* Make sure to include sufficient details (data and analysis steps) in order to reproduce the issue.",
        "\n\n---\n\n"    
    );
    .buffer_add(end, collapse="\n");
}


#'Create MetaAnalysis table of results for Upset Diagram
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Guangyan Zhou
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateUpSetDoc_slides <- function(mSetObj = NA) {
    # Retrieve the metabolomics set object
    mSetObj <- .get.mSet(mSetObj);
    
    # Check if the UpSet plot exists
    if (!is.null(mSetObj$imgSet$reportSet$upset) && safeFileExists(mSetObj$imgSet$reportSet$upset)) {
        link <- GetSharingLink(mSetObj)

        # Description of the UpSet plot for slides
        descr <- c(
            "## UpSet Diagram Overview\n\n",
            "- An UpSet plot provides a scalable and interpretable representation of set intersections.",
            "- This plot is used here to analyze the overlap of significant features identified across different datasets.",
            "- Each bar represents the number of significant features for a particular combination of datasets.\n"
        )
        .buffer_add(descr, collapse="\n")
        .buffer_add("\n\n---\n\n") # Slide separator

        # Insert the UpSet plot image into the slides

    upset.desc <- paste("## Figure ", getFigCount(), ": Upset diagram comparing multiple results.");
    .buffer_add(upset.desc, collapse="\n");

    # image rendering in R Markdown
    img <- paste0("```{r figure_upset, echo=FALSE, fig.align='center', fig.pos='H', out.width='", getFigWidth(mSetObj), "'}\n",
       "  safeIncludeGraphics('", mSetObj$imgSet$reportSet$upset, "')\n",
       "```",
       "\n\n---\n\n"
       )

    
    .buffer_add(img, collapse="\n");
  }
    
}

CreateTitleTableSlide <- function(tableStr, tableName) {
  tableCount <- getTableCount()  # Assuming getTableCount() is a function you've defined elsewhere
  descr_table <- c(
    paste0("## Table ", tableCount, ": ", tableName),  # Table title
    paste0("```{r table_", tableCount, ", echo=FALSE, results='asis', out.width='100%', out.height='100%', warning=FALSE}"),  # Setup R code chunk
    tableStr,  # Insert your data frame creation R code
    paste0("create_dt(dt_res)"),  # DataTables rendering call
    "```",  # Close R code chunk
    "\n\n---\n\n"  # Slide division
  )
  .buffer_add(descr_table, collapse="\n\n")  # Write to R Markdown file
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create correlation document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
AddFeatureImages_slides <- function(mSetObj=NA) {
    mSetObj <- .get.mSet(mSetObj);

    imgSet <- mSetObj$imgSet 
    
    # Check if there are saved feature images to display
    if (!is.null(imgSet$featureList) && length(imgSet$featureList) > 0) {
        # Introduction text for the features of interest section
        descr <- paste0(
            "## Features of Interest\n\n",
            "This section displays gene expression profiles for key features identified during the analysis.",
            "\n\n---\n\n",  # End of slide
            sep = ""
        )
        .buffer_add(descr, collapse="\n")
        
        # Iteratively render images of selected features
        for (i in 1:length(imgSet$featureList)) {
            # Extract just the name or description of the feature from the filename if necessary
            featureDesc <- gsub("(.*)_[0-9]+_dpi[0-9]+\\.png$", "\\1", imgSet$featureList[[i]])
            featureDesc <- gsub("_", " ", featureDesc)  # Replace underscores with spaces for better readability if needed
            
            # Use the custom function for inserting images into slides
            CreateTitleFigureSlide(imgSet$featureList[[i]], paste0("Feature:", featureDesc))
        }
    }
}