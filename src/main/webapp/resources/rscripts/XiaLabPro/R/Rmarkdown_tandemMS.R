
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

# Process tandemMS module report

#'Create report for raw spectra module
#'@description Report generation using Sweave
#'Write .Rnw file template
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export

CreateTandemMSAnalysisRnwReport <- function(mSetObj, usrName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # Creat the header & introduction of the report
  CreateHeader(usrName);
  
  CreateMS2Intr();
  CreateMS2SpecIOdoc(mSetObj);
  CreateMS2AnalMethod(mSetObj);
  CreateMS2AnalResults(mSetObj);
  CreateMatchingPatterns(mSetObj);

  CreateRHistAppendix();
  CreateFooter();
  
}


### Section 1 - Introduction & Background
CreateMS2Intr <- function(){
  
  if(!exists("table.count")){
    table.count <<- 0;
  }
  if(!exists("fig.count")) {
    fig.count <<- 0;
  }
  
  descr0 <- c("\n\n## MS/MS Spectra Annotation\n\n",
              "Metabolomics involves the comprehensive identification and quantification of small compounds in biological samples 
              using various analytical techniques. Liquid chromatography - mass spectrometry (LC-MS) has been the primary analytical platform 
              for global or untargeted metabolomics and exposomics. Following spectra acquisition, spectra processing and compound identification
              are two critical steps to explore significant signatures\n\n")
  
  descr1 <- c("To facilitate both quantitative analysis and compound identification, LC-MS untargeted metabolomics are typically conducted with MS1 full scans 
              coupled with tandem MS (MS/MS or MS2) using data-dependent acquisition (DDA) or data-independent acquisition (DIA) methods. MetaboAnalyst aims to provide two efficient pipelines to process MS2 spectra: \n\n ")
  
  descr2 <- c("+ (1) an auto-optimized DDA data deconvolution workflow to deal with chimeric spectra; \n\n+ (2) an efficient SWATH-DIA data deconvolution pipeline; \n\n")
  
  descr <- c(descr0, descr1, descr2,
             "This module is designed to provide an automated workflow to process the raw MS2 spectra data in four steps, including ",
             "raw spectra importing, integrity checking, database searching and result summary. The detailed algorithm and introduction on the workflows are included in [MetaboAnalystR 4.0](https://www.nature.com/articles/s41467-024-48009-6) publication.\n\n");
  
  cat(descr, file=rmdFile, append=TRUE);
}

### Section 2 - Spectra IO summary and sanity check
CreateMS2SpecIOdoc <- function(mSetObj){
  
  descr <- c("\n\n## MS/MS Spectra and Integrity Check\n\n",
             "MetaboAnalyst MS2 Annotation Module accepts either single spectrum or multiple spectra (in common open MS formats: msp or mgf) ",
             "All of these options require the information of precursor ions and their corresponding MS/MS spectra. All empty spectra will be exclided automatically.",
             "The Data Integrity Check is performed before the data processing starts. ");
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  if(!is.null(mSetObj$dataSet$spectrum_dataframe)){ # single spectrum
    descr1 <- c( "You have uploaded a single spectrum. The basic information of this spectrum is provided below.\n\n")
    cat(descr1, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    table.count <<- table.count+1;
    
    cmdhist2 <- c(
      "```{r table_raw1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "spc_dt <- mSetObj$dataSet$spectrum_dataframe;",
      "colnames(spc_dt) <- c('mz','intensity');",
      paste0("create_dt(spc_dt,  caption = 'Table ", 
             table.count, 
             ". Input spectrum for MS2 annotation searching.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
    
  } else if(!is.null(mSetObj[["dataSet"]][["prec_mzrt_all"]])){ # multiple spectra
    descr1 <- c( "You have uploaded multiple spectra. The detailed information of all spectra is summarized below.\n\n")
    cat(descr1, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    descr2 <- mSet[["msgSet"]][["sanity_msgvec"]];
    descr2 <- paste0("+ ", descr2)
    cat(descr2, file=rmdFile, append=TRUE, sep = "\n\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }
}

### Section 3 - MS2 searching method and params
CreateMS2AnalMethod <- function(mSetObj){
  descr <- c("\n\n## MS/MS Spectra Searching Parameters\n\n",
             "MetaboAnalyst offers multiple database options and two algorithms for spectra searching. Besides, there are also several parameters need to be customized.", 
             "Here the detailed algorithms and parameters' used in this study. Explanations on these parameters are also included below.");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  table.count <<- table.count+1;
  if(!is.null(mSetObj$dataSet$spectrum_dataframe)){ # single spectrum
    cmdhist2 <- c(
      "```{r table_raw2, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "spc_dt <- as.data.frame(t(mSetObj$dataSet$params));",
      "spc_dt[4,1] <- ifelse(spc_dt[4,1]==0, 'Dot product', 'Spectral Entropy')",
      "rownames(spc_dt) <- c('ppm for MS1','ppm for MS2','Database', 'Similarity Method', 'mz of Precursor', 'Ion Mode', 'Unit for MS1', 'Unit for MS2', 'Version of OptiLCMS');",
      "colnames(spc_dt) <- 'Values';",
      paste0("create_dt(spc_dt,  caption = 'Table ", 
             table.count, 
             ".  Parameters for MS2 spectra searching of single spectrum.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  } else {
    cmdhist2 <- c(
      "```{r table_raw3, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
      "spc_dt <- as.data.frame(t(mSetObj$dataSet$params));",
      "spc_dt[4,1] <- ifelse(spc_dt[4,1]==0, 'Dot product', 'Spectral Entropy')",
      "rownames(spc_dt) <- c('ppm for MS1','ppm for MS2','Database', 'Similarity Method', 'mz of Precursor', 'Ion Mode', 'Unit for MS1', 'Unit for MS2', 'Version of OptiLCMS');",
      "colnames(spc_dt) <- 'Values';",
      paste0("create_dt(spc_dt,  caption = 'Table ", 
             table.count, 
             ".  Parameters for MS2 spectra searching of multiple spectra.')"),
      "```", "\n\n");
    
    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
  }

  descr3 <- "All parameters used to do the MS2 database searching are shown as below."
  
  cat(descr3, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  descr4 <- c(
    "1. ppm values:",
    " + ppm for MS1: parts per million for ms1 spectrum. Ppm is a unit of measurement utilized to express the mass resolution or mass accuracy of the mass spectrometer. If the unit is Dalton, this value is the absolute Dalton deviation.",
    " + ppm for MS2: parts per million for ms1 spectrum. Same as ppm for MS1.",
    "2. Database:",
    " + All: All databases have been used for MS2-based annotation.",
    " + other: specifying one or more customized database for searching.",
    "3. Similarity Method:",
    " + Dot product: A traditional similarity calculation method for MS. This is a vector-similarity based method ([Details](https://pubs.acs.org/doi/10.1016/1044-0305%2894%2987009-8))",
    " + Spectral Entroy: A newly-developed similarity evaluation method. This method is based on the accuracy of spectral entropy similarity evaluation ([Details](https://www.nature.com/articles/s41592-021-01331-z)).",
    "4. mz of Precursor (for single spectrum only):",
    " + mz value of the corresponding precursor. If you upload multiple spectra, this value will be 0.",
    "5. Ion Mode:",
    " + ESI ion modes: can be either positive or negative.",
    "6. Units for MZ deviation:",
    " + ppm:  parts per million.",
    " + da: Dalton, showing the absolute deviation.");
  cat(descr4, file=rmdFile, append=TRUE, sep="\n");
}

### Section 4 - MS2 searching results
CreateMS2AnalResults <- function(mSetObj){
  
  descr <- c("\n\n## Spectra Searching Summary\n\n",
             "The MS2 spectral searching results is summarized in Table ", 
             table.count<<-table.count+1,", as below."
  );
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  cmdhist2 <- c(
    "```{r table_results1, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
    "sum_dt <- createMS2SumTableRMD(mSetObj);",
    paste0("create_dt(sum_dt,  caption = 'Table ", 
           table.count, 
           ". Summary of all MS2 spectra searching results.', escape = FALSE, rmRowNM = TRUE)"),
    "```", "\n\n");
  
  cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  cat("\n\n", file=rmdFile, append=TRUE);
  
  if(is.null(mSetObj$dataSet$spectrum_dataframe)){
    
    descr3 <- "The first column ***Indexes*** refers to the order of corresponding spectrum in your original MSP/MGF files. 
    The column called as ***Ranks*** is the ranking of the compounds based on the scores in desending order."
    
    cat(descr3, file=rmdFile, append=TRUE, sep="\n");
    cat("\n\n", file=rmdFile, append=TRUE);
    
  }
  
}

createMS2SumTableRMD <- function(mSetObj){
  
  if(!is.null(mSetObj$dataSet$spectrum_dataframe)){
    if(length(mSetObj[["dataSet"]][["msms_result"]][[1]][["IDs"]])>0){
      Indexs <- mSetObj[["dataSet"]][["msms_result"]][[1]][["IDs"]]
      Scores <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Scores"]][[1]]
      dot_products <- mSetObj[["dataSet"]][["msms_result"]][[1]][["dot_product"]][[1]]
      compounds <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Compounds"]]
      formulas <- mSetObj[["dataSet"]][["msms_result"]][[1]][["Formulas"]]
      inchikeys <- mSetObj[["dataSet"]][["msms_result"]][[1]][["InchiKeys"]]

      if(mSetObj[["paramSet"]][["report.format"]] == "html"){

            details <- paste0("<a href=\"https://pubchem.ncbi.nlm.nih.gov/#query=", inchikeys, "\" target=\"_blank\">Details</a>");
            viewlinks <- "";

            link <- GetSharingLink(mSetObj)
            mz_val <- mSetObj[["dataSet"]][["params"]][["precMZ"]]
            reportLinks <- vapply(1:length(Indexs), function(l){
              getReportMS2MirrorLinks(link, paste0(mz_val, "mz@NonSpecified", "__", Indexs[l]), "ms2");
            }, FUN.VALUE = character(length = 1L))

            viewlinks <- paste0("<a href=\"", reportLinks, "\" target=\"_blank\">View</a>");

            res_df <- data.frame(Scores = Scores,
                                 Similarity = dot_products, Compounds = compounds,
                                 Formulas = formulas, InChiKeys = inchikeys, Details = details, View = viewlinks)

      } else {

            res_df <- data.frame(Scores = Scores,
                                 Similarity = dot_products, Compounds = compounds,
                                 Formulas = formulas, InChiKeys = inchikeys)
      }
    }
  } else if(length(mSetObj[["dataSet"]][["msms_result"]])>0){
    res_df_list <- lapply(1:length(mSetObj[["dataSet"]][["msms_result"]]), function(x){
      ID <- x;
      featurelabel <- mSetObj[["dataSet"]][["prec_mzrt_included"]][x]
      Indexs <- mSetObj[["dataSet"]][["msms_result"]][[x]][["IDs"]]
      Scores <- mSetObj[["dataSet"]][["msms_result"]][[x]][["Scores"]][[1]]
      dot_products <- mSetObj[["dataSet"]][["msms_result"]][[x]][["dot_product"]][[1]]
      compounds <- mSetObj[["dataSet"]][["msms_result"]][[x]][["Compounds"]]
      formulas <- mSetObj[["dataSet"]][["msms_result"]][[x]][["Formulas"]]
      mz <- mSetObj[["dataSet"]][["msms_result"]][[x]][["Precursors"]]
      inchikeys <- mSetObj[["dataSet"]][["msms_result"]][[x]][["InchiKeys"]]
      details <- paste0("<a href=\"https://pubchem.ncbi.nlm.nih.gov/#query=", inchikeys, "\" target=\"_blank\">Details</a>")
      ranks <- seq_along(Indexs)
      
      viewlinks <- "";
      link <- GetSharingLink(mSetObj)
      reportLinks <- vapply(1:length(Indexs), function(l){
        getReportMS2MirrorLinks(link, paste0(featurelabel, "__", Indexs[l]), "ms2");
      }, FUN.VALUE = character(length = 1L))
      
      viewlinks <- paste0("<a href=\"", reportLinks, "\" target=\"_blank\">View</a>");
      
      if(length(Indexs)==0){
        res_dx <- NULL
      } else {
        if(mSetObj[["paramSet"]][["report.format"]] == "html"){
            res_dx <- data.frame(Indexes = ID, Features = featurelabel, Ranks = ranks, Scores = Scores,
                                 Similarity = dot_products, Compounds = compounds,
                                 Formulas = formulas, mz = mz, 
                                 InChiKeys = inchikeys, Details = details, View = viewlinks);
        } else {
            res_dx <- data.frame(Indexes = ID, Features = featurelabel, Ranks = ranks, Scores = Scores,
                                 Similarity = dot_products, Compounds = compounds,
                                 Formulas = formulas, mz = mz, 
                                 InChiKeys = inchikeys);
        }
      }
      return(res_dx)
    })
    
    res_df <- do.call(rbind, res_df_list)
  } else {
    res_df <- data.frame(Results = "No results found!")
  }
  return(res_df)
}

### Section 5 - MS2 spectra matching figure
CreateMatchingPatterns <- function(mSetObj){
  
  descr <- c("\n\n## Mirror plots of spectra searching\n\n",
             "The mirror plots you are interested in are shown as below.\n\n"
  );
  
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n", file=rmdFile, append=TRUE);
  
  files <- unique(mSetObj[["imgSet"]][["msmsmirror"]][["imageNM"]])
  files[grepl("mirror_plotting_", files)] -> files

  if(!any(file.exists(files))){
    descr3 <- c("\n\nNo Spectral mirror plots was viewed by you. Please try to explore from result page.");
    cat(descr3, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
  }

  for (i in files){
    if(file.exists(i)){
      fig.count<<-fig.count+1;
      mir_nm <- i;

      idx_file <- which(mSetObj[["imgSet"]][["msmsmirror"]][["imageNM"]] == i)[1]
      ft_idx <- mSetObj[["imgSet"]][["msmsmirror"]][["ft_idx"]][idx_file]
      candi_idx <- mSetObj[["imgSet"]][["msmsmirror"]][["indx"]][idx_file]

      compound_name <- mSetObj[["dataSet"]][["msms_result"]][[ft_idx]][["Compounds"]][candi_idx]
      formulas_name <- mSetObj[["dataSet"]][["msms_result"]][[ft_idx]][["Formulas"]][candi_idx]
      inchikey_name <- mSetObj[["dataSet"]][["msms_result"]][[ft_idx]][["InchiKeys"]][candi_idx]
      
      if (mSetObj$paramSet$report.format == 'html'){
        i <- gsub(".png|.svg|.pdf", "", i);
        fig <- c(paste0("```{r ", mir_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count,
                        ". Mirror plot of this spectra: ", gsub("mirror_plotting_[0-9]+_","",sub("_72","",i)), 
                        ". Compound name: ", compound_name, ". Formula: ", formulas_name, ". InchiKey: ", inchikey_name,  ".' , ",
                        " fig.lp='", paste0(i, ".rds"), "', out.width = '", getFigWidth(mSetObj), "'}"),
                 paste0("p1 <- readRDS('", paste0(i, ".rds"), "'); p1"),
                 "```",
                 "\n\n");
      } else {
        fig <- c(paste0("```{r ", mir_nm, ", echo=FALSE, fig.pos='H', fig.cap='Figure ", fig.count,
                        ". Mirror plot of this spectra: ", gsub("mirror_plotting_[0-9]+_","",sub("_72.png","",i)), 
                        ". Compound name: ", compound_name, ". Formula: ", formulas_name, ". InchiKey: ", inchikey_name,  ".' , ",
                        " fig.lp='", paste0(i), "', out.width = '", getFigWidth(mSetObj), "'}"),
                 paste0("safeIncludeGraphics(\"", i, "\")"),
                 "```",
                 "\n\n");
      }

      cat(fig, file=rmdFile, append=TRUE, sep="\n");
      cat("\n\n", file=rmdFile, append=TRUE, sep="\n");
    }
  }
  
  cat("\n\n", file=rmdFile, append=TRUE);
}


