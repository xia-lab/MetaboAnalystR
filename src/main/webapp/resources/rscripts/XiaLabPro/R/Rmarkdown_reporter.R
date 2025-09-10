PrepareHTMLReportModule<-function(mSetObj=NA, usrName, link="NA", module="NA"){
    PrepareHTMLReport(mSetObj, usrName, link, module)
}
#'Create report of analyses
#'@description Report generation using Sweave
#'Note: most analyses were already performed, only need to embed
#'the results to the right place without rerunning the whole analysis
#'through Sweave. Only some auxilliary info (i.e. time, version etc need to
#'run in R through Sweave
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
PrepareHTMLReport<-function(mSetObj=NA, usrName, link="NA", module="NA"){
  mSetObj <- .get.mSet(mSetObj);

    if (file.exists('mSet.rda') && file.exists('Rload.RData')) {
        if (module != "raw") {
            print("LOADING RLOAD=========");
            print(getwd());
            load('Rload.RData')
        }
    }
    if(!is.null(mSetObj$dataSet$meta.info)){
    print(head(mSetObj$dataSet$meta.info))
    }

  # create the Rnw file
  if(module != "NA"){
    if(mSet$paramSet$report.format == "slides"){
    file.create(paste0("Analysis_Presentation_",module, ".Rmd"));
    rmdFile <<- file(paste0("Analysis_Presentation_",module, ".Rmd"), "w")
    }else{
    file.create(paste0("Analysis_Report_",module, ".Rmd"));
    rmdFile <<- file(paste0("Analysis_Report_",module, ".Rmd"), "w")
    }
  }else{
    if(mSet$paramSet$report.format == "slides"){
    file.create("Analysis_Presentation.Rmd");
    rmdFile <<- file("Analysis_Presentation.Rmd", "w")
    }else{
    file.create("Analysis_Report.Rmd");
    rmdFile <<- file("Analysis_Report.Rmd", "w")
    }
  }

  # create a global counter to label figures
  fig.count <<- 0;
  table.count <<- 0;
  if(module != "NA"){
  anal.type <- module;
  }else{
  anal.type <- mSetObj$analSet$type;
  }
  anal.type <<- anal.type;

  if(mSet$paramSet$report.format == "slides"){
    if(anal.type == "stat" ){
      CreateStatRmdReportSlides(mSetObj, usrName);
    }else if(anal.type == "mf"){
      CreateMultiFacRnwReport_slides(mSetObj, usrName);
    }else if(substr(anal.type, 0, 4) == "mset"){
      CreateEnrichRmdReport_slides(mSetObj, usrName);
    }else if(anal.type == "power"){
      CreatePowerRnwReport_slides(mSetObj, usrName)
    }else if(anal.type == "roc"){
      CreateBiomarkerRnwReport_slides(mSetObj, usrName)
    }else if(anal.type == "pathinteg"){
      CreateIntegPathwayAnalysisRnwReport_slides(mSetObj, usrName);
    }else if(substr(anal.type, 0, 4) == "path"){ # must be after pathiteg
      CreatePathRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "network"){
      CreateNetworkExplorerRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "mummichog" || anal.type == "mass_table" || anal.type == "mass_all"  ){
      CreateMummichogRmdReport_slides(mSetObj, usrName);
    }else if(anal.type == "metapaths"){
      CreateMetaPathRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "metadata"){
      CreateMetaAnalysisRnwReport_slides(mSetObj, usrName, link);
    }else if(anal.type == "raw"){
      CreateRawAnalysisRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "tandemMS"){
      CreateTandemMSAnalysisRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "dose"){
      CreateDoseRnwReport_slides(mSetObj, usrName);
    }else if(anal.type == "mgwas"){
      CreateCausalRnwReport_slides(mSetObj, usrName);
    }else{
      AddErrMsg(paste("No template found for this module:", anal.type));
      return(0);
    }
  }else{
    if(anal.type == "stat" ){
      CreateStatRmdReport(mSetObj, usrName);
    }else if(anal.type == "mf"){
      CreateMultiFacRnwReport(mSetObj, usrName);
    }else if(substr(anal.type, 0, 4) == "mset"){
      CreateEnrichRmdReport(mSetObj, usrName);
    }else if(anal.type == "power"){
      CreatePowerRnwReport(mSetObj, usrName)
    }else if(anal.type == "roc"){
      CreateBiomarkerRnwReport(mSetObj, usrName)
    }else if(anal.type == "pathinteg"){
      CreateIntegPathwayAnalysisRnwReport(mSetObj, usrName);
    }else if(substr(anal.type, 0, 4) == "path"){ # must be after pathiteg
      CreatePathRnwReport(mSetObj, usrName);
    }else if(anal.type == "network"){
      CreateNetworkExplorerRnwReport(mSetObj, usrName);
    }else if(anal.type == "mummichog" || anal.type == "mass_table" || anal.type == "mass_all"  ){
      CreateMummichogRmdReport(mSetObj, usrName);
    }else if(anal.type == "metapaths"){
      CreateMetaPathRnwReport(mSetObj, usrName);
    }else if(anal.type == "metadata"){
      CreateMetaAnalysisRnwReport(mSetObj, usrName, link);
    }else if(anal.type == "raw"){
      CreateRawAnalysisRnwReport(mSetObj, usrName);
    }else if(anal.type == "tandemMS"){
      CreateTandemMSAnalysisRnwReport(mSetObj, usrName);
    }else if(anal.type == "dose"){
      CreateDoseRnwReport(mSetObj, usrName);
    }else if(anal.type == "mgwas"){
      CreateCausalRnwReport(mSetObj, usrName);
    }else{
      AddErrMsg(paste("No template found for this module:", anal.type));
      return(0);
    }
  }

  # close opened files
  close(rmdFile);

  # find out pandoc
  pandoc_path <- sub("pandoc:", "", system("whereis pandoc",intern = T));
  if(pandoc_path == ""){
    if(file.exists("/usr/lib/rstudio/resources/app/bin/quarto/bin/tools")){ # for ubuntu with rstudio
        pandoc_path <- "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools";
    } else {
        warning("You must install pandoc or specify the path here !")
    }
    if(file.exists("/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/pandoc")){
        pandoc_path <- "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64";
    }
  }
  Sys.setenv(RSTUDIO_PANDOC=pandoc_path)
  #save.image("rmd.RData");

if(module != "NA"){
  if(mSet$paramSet$report.format == "slides"){
    rmarkdown::render(paste0("Analysis_Presentation_",module, ".Rmd"))
  }else{
    rmarkdown::render(paste0("Analysis_Report_",module, ".Rmd"))
  }
}else{

  if(mSet$paramSet$report.format == "slides"){
    rmarkdown::render("Analysis_Presentation.Rmd")
  }else{
    rmarkdown::render("Analysis_Report.Rmd")
  }
}
  return(1);
}

# this is for PDF report generation from bash
SaveCurrentSession <- function(){
  file.copy("../../rscripts/_sweave.sty", ".")
  save.image("SweaveImage.RData");
}

CreateHeader <- function(usrName){
    
  module.nm <- GetModuleName();
  if(mSet$paramSet$report.format == "html"){
     header <- c("---",
   paste0('title: "', module.nm, '"'),
'output:',
'  rmdformats::robobook:',
  '    includes:',
  '      in_header: "custom-scripts.html"',
  "---",
  "**Completed**: `r Sys.Date()`",
  "\n\n"
)

html_content <- '
<script>
   function updateTheme(theme="dim") {

        const body = document.body;
        const pageInner = document.querySelector(".book .book-body .page-inner");
        const withSum = document.querySelector(".book .book-summary");
        const sections = document.querySelectorAll(".book .book-body .page-inner section.normal");
        const navText = document.querySelectorAll(".book-summary nav#toc a");
        const tags = ["h1", "h2", "h3", "p"];
        const dataTables = document.querySelectorAll(".dataTables_wrapper");
        withSum.style.zIndex = "1";
        const htelements = document.querySelectorAll(".iheatmapr.html-widget.html-fill-item.html-widget-static-bound.js-plotly-plot");

        if (theme == "dark") {
            if (pageInner) {
                pageInner.style.background =   "#444444";
                pageInner.style.color = "#e0e0e0";
            }
            if (withSum) {
                withSum.style.background = "#444444";
                withSum.style.color = "white";       
                withSum.style.boxShadow = "inset -15px 0 15px -10px rgba(0, 0, 0, 0.5)";
            }

     sections.forEach(section => {
                section.style.background = "#1e1e1e";
                section.style.color = "#e0e0e0";
            });
 
           navText.forEach(nav => {
                nav.style.color = "white";
            });
            
            tags.forEach(tag => {
                const elements = document.querySelectorAll(tag);
                elements.forEach(element => {
                    element.style.color = "";
                });
            });
      
            
             dataTables.forEach(dataTable => {
                dataTable.style.color = "white";
                 const elements = dataTable.querySelectorAll("*");
                elements.forEach(element => {
                    element.style.color = "white";
                });
            });
              if(htelements){
                       newBgColor = "#071426";
                            newFontColor = "#FFFFFF";
                      htelements.forEach(htelement => {
        Plotly.update(htelement, {}, {
            "paper_bgcolor": newBgColor,
            "plot_bgcolor": newBgColor,
            "font.color": newFontColor
        });
    });

                 }
        } else if (theme == "light") {
            if (pageInner) {
                pageInner.style.background ="";
                pageInner.style.color = "";
            }
            if (withSum) {
                withSum.style.background = "";
                withSum.style.color = "";
                 withSum.style.boxShadow = "inset -10px 0 15px -10px rgba(0, 0, 0, 0.3)";
            }
            sections.forEach(section => {
                section.style.background = "";
                section.style.color = "";
            });
            navText.forEach(nav => {
                nav.style.color = "";
            });
               tags.forEach(tag => {
                const elements = document.querySelectorAll(tag);
                elements.forEach(element => {
                    element.style.color = "";
                });
            });
           
          dataTables.forEach(dataTable => {
                dataTable.style.color = "";
                 const elements = dataTable.querySelectorAll("*");
                elements.forEach(element => {
                    element.style.color = "";
                });
            });
         if(htelements){ 
                  newBgColor = "#ffffff";
                            newFontColor = "#000000";
                     htelements.forEach(htelement => {
        Plotly.update(htelement, {}, {
            "paper_bgcolor": newBgColor,
            "plot_bgcolor": newBgColor,
            "font.color": newFontColor
        });
    });


                 }
        }else{

            if (pageInner) {
                pageInner.style.background =   "rgba(28, 36, 52, 1)";
                pageInner.style.color = "#e0e0e0";
            }
            if (withSum) {
                withSum.style.background = "rgba(28, 36, 52, 1)";
                withSum.style.color = "white";       
                withSum.style.boxShadow = "inset -15px 0 15px -10px rgba(0, 0, 0, 0.5)";
            }

     sections.forEach(section => {
                section.style.background = "#071426";
                section.style.color = "#e0e0e0";
            });
 
           navText.forEach(nav => {
                nav.style.color = "white";
            });
            
            tags.forEach(tag => {
                const elements = document.querySelectorAll(tag);
                elements.forEach(element => {
                    element.style.color = "";
                });
            });
      
            
             dataTables.forEach(dataTable => {
                dataTable.style.color = "white";
                 const elements = dataTable.querySelectorAll("*");
                elements.forEach(element => {
                    element.style.color = "white";
                });
            });
              if(htelements){
                       newBgColor = "#071426";
                            newFontColor = "#FFFFFF";
                      htelements.forEach(htelement => {
        Plotly.update(htelement, {}, {
            "paper_bgcolor": newBgColor,
            "plot_bgcolor": newBgColor,
            "font.color": newFontColor
        });
    });

                 }
        }

    }

    window.updateTheme = updateTheme;

    $(document).ready(function () {
        updateTheme();
    });
</script>
'
writeLines(html_content, "custom-scripts.html")

  }else if (mSet$paramSet$report.format == "slides") {
      if(!file.exists(paste0(rpath, 'libs/slides_template.pptx'))){
            rpath <<- "../../../";
      }
      header <- c(
      "---",
      'title: "Metabolomic Data Analysis by MetaboAnalyst Pro"',
      paste0('author: ', GetModuleName()),
      paste0('date: ', Sys.Date()),  # Dynamically adding the date
      'output:', 
      '  powerpoint_presentation:', 
      paste0('    reference_doc: "', rpath, 'libs/slides_template.pptx', '"'),
      "---",
      "\n\n"
    )


  }else{

    header <- c("---",
              'title: "Metabolomic Data Analysis with MetaboAnalyst Pro"',
              paste0('author: "MetaboAnalyst Pro Support Team: guest user - ', usrName, ' "'),
            paste0('date: "', Sys.Date(), '"'),  # To add the current date
            'output:',
            '   pdf_document:', 
            '    toc: true',
            '    toc_depth: 3',
            '    number_sections: false',
            'header-includes:',
            '   - \\usepackage{colortbl}',
            '   - \\usepackage{booktabs}',
            '   - \\usepackage{caption}',
            '   - \\captionsetup[table]{labelformat=empty}',
            '   - \\captionsetup[figure]{labelformat=empty}',
            '   - \\usepackage{float}',
            '   - \\usepackage[utf8]{inputenc}',
            '   - \\usepackage{textcomp}',
            '   - \\usepackage{newunicodechar}',
            '   - \\usepackage{hyperref}',
            '   - \\hypersetup{',
            '       colorlinks = true,',
            '       linkcolor = blue,',
            '       filecolor = magenta,',
            '       urlcolor = cyan,',
            '     }',
            "---",
           # "**Completed**: `r date()`",
            "\n\n")
  }
  cat(header, file=rmdFile, sep="\n", append=TRUE);
  
  if (mSet$paramSet$report.format %in% c("html")) {
    container_css <- c(
   
   "<style type='text/css'>
     .main-container {
        max-width: 2000px;
        margin-left: auto;
        margin-right: auto;
     }
    
     .book .book-body .page-inner {
       max-width: 2000px;
     }
    
    .book .book-body .page-inner section.normal table {
      width: 100%;
      margin-left: auto;
      margin-right: auto;
    }
    
    .book .book-body .page-inner section.normal .dataTables_wrapper .table-wrapper {
      margin-top: 0px;
      width: 100%;
    }
    
    .book .book-body .page-inner section.normal .figure {
      text-align: center;
      margin: auto;
    }

    .iheatmapr.html-widget.html-fill-item.html-widget-static-bound.js-plotly-plot {
            margin: 0 auto; 
    }

    </style>\n\n")
    cat(container_css, file=rmdFile, sep="\n", append=TRUE)

    ## ── unified setup chunk (TMPDIR + figure dir + device) ─────────────
    setup_chunk <- c(
      "```{r setup, include=FALSE}",
      "# Stable temp & figure directories to avoid /tmp cleanup issues",
      "if (!nzchar(Sys.getenv('TMPDIR'))) Sys.setenv(TMPDIR = file.path(getwd(), 'tmp'))",
      "dir.create(Sys.getenv('TMPDIR'), showWarnings = FALSE, recursive = TRUE)",
      "dir.create('figure', showWarnings = FALSE, recursive = TRUE)",
      "",
      "# Device: prefer ragg, fall back to png; set DPI and default fig path",
      "dev_choice <- if (requireNamespace('ragg', quietly = TRUE)) 'ragg_png' else 'png'",
      "knitr::opts_chunk$set(",
      "  echo = FALSE, warning = FALSE, message = FALSE,",
      "  fig.path = 'figure/', dev = dev_choice, dpi = 150",
      ")",
      "",
      "# Headless bitmap safety (Cairo on Linux servers)",
      "options(bitmapType = 'cairo')",
      "```"
    )
    cat(setup_chunk, file = rmdFile, sep = "\n", append = TRUE)
  }
  
  code_settings <- c("```{r echo=FALSE}",
                     "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)",
                     "```")
  cat(code_settings, file=rmdFile, sep="\n", append=TRUE)
  
}

#"Create report of analyses 
#'@description Report generation using Sweave
#'Create a summary table for each type of uploaded data
#'csv table has 5 col: sampleID, feature #, zero,  missing #
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'@import qs
CreateSummaryTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  sum.dat<-NULL;
  plenth <- mSetObj$dataSet$proc.feat.num;
  if(mSetObj$dataSet$type %in% c('conc', 'pktable','specbin','mztab', 'mass_table')){
    orig.data<- qs::qread("data_orig.qs");
    for(i in 1:nrow(orig.data)){
      srow<-orig.data[i,];
      newrow<-c(sum(srow[!is.na(srow)]>0), (sum(is.na(srow)) + sum(srow[!is.na(srow)]<=0)), plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Features (original)","Missing/Zero","Features (processed)");
    rownames(sum.dat)<-names(mSetObj$dataSet$url.smp.nms);
  }else if(mSetObj$dataSet$type=="nmrpeak"| mSetObj$dataSet$type=="mspeak"){ # peak list
    pkSet<-qs::qread("peakSet.qs");
    orig.data<- qs::qread("data_orig.qs");
    snames<-pkSet$sampnames;
    for(i in 1:length(snames)){
      samp.inx<-pkSet$peaks[,"sample"]==i;
      srow <- orig.data[i,];
      newrow<-c(sum(samp.inx),(sum(is.na(srow)) + sum(srow[!is.na(srow)]<=0)), plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Peaks (original)","Missing/Zero", "Peaks (processed)");
    rownames(sum.dat)<-names(mSetObj$dataSet$url.smp.nms);
  }else{ # spectra
    rawxset<-mSetObj$dataSet$xset.orig;
    fillxset<-mSetObj$dataSet$xset.fill;
    snames<-row.names(rawxset@phenoData)
    
    for(i in 1:length(snames)){
      rawno<-sum(rawxset@peaks[,"sample"]==i);
      fillno<-sum(fillxset@peaks[,"sample"]==i);
      newrow<-c(rawno,fillno,plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Peaks (raw)","Peaks (fill)", "Peaks(processed)");
    rownames(sum.dat)<-names(mSetObj$dataSet$url.smp.nms);
  }
  return(sum.dat)
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Read and process raw data
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateDataProcdoc <- function(mSetObj=NA){
  
  cat("<hr/>", file=rmdFile, append=TRUE, sep="\n");

  mSetObj <- .get.mSet(mSetObj);

  descr <- c("## 2. Data Processing\n",
            "Data processing includes four steps: *data integrity check*, *missing value estimation*, *data filtering* and *data normalization*. 
            Together, these steps enable comprehensive data treatments to produce a clean, high-quality table suitable for downstream statistical 
            or machine learning algorithms. Some steps are optional depending on your data type. For instance, missing value estimation and 
            data filtering are primarily for untargeted metabolomics data which typically contain a lot of missing values and noises. 
            \n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
  
  # error checking
  proc.ok <- TRUE;
  if(substring(mSetObj$dataSet$format,4,5)=="mf"){
    if(is.null(mSetObj$dataSet$meta.info) | !file.exists("data_orig.qs")){
        proc.ok <- FALSE;
    }
  }else{
    if(is.null(mSetObj$dataSet$cls) | !file.exists("data_orig.qs")){
        proc.ok <- FALSE;
    }
  }

  if(!proc.ok){
      descr <- c("Could not find your data.\n");
      cat(descr, file=rmdFile, append=TRUE, sep="\n");
      return();
  }

  if(mSetObj$dataSet$type=="conc"){
     data.type <- "* Data type: concentration table" 
  }else if(mSetObj$dataSet$type=="specbin"){
     data.type <- "* Data type: binned spectral table" 
  }else if(mSetObj$dataSet$type %in% c("pktable", "mass_table")){
     data.type <- "* Data type: peak intensity table" 
  }else if(mSetObj$dataSet$type=="nmrpeak"){
     data.type <- c(
                  "* Data type: NMR peak list and intensity data\n\n",
                  "  + Proximal peaks are first grouped together based on their position using a moving window of 0.03 ppm and a step of 0.015 ppm.", 
                  "  + Peaks of the same group are aligned to their median positions across all samples.",
                  "  + The aligned peaks are reorganized into a single data matrix for further analysis.",
                  "  + The name of the parent folder is used as class label for each sample.");    
  }else if(mSetObj$dataSet$type=="mspeak"){
     data.type <- c(
                  "* Data type: MS peak list and intensity data\n\n",
                  " + Peaks are first matched across samples based on their m/z (and rt if provided) in order to be compared.",
                  " + The aligned peaks are reorganized into a single data matrix for further analysis.",
                  " + The name of the parent folder is used as class label for each sample.");

  }else if(mSetObj$dataSet$type=="mztab"){
     data.type <- "* Data type: mzTab-M 2.0 format"; 

  }else{ # mummichog
      data.type <- "* Data type: peak intensity table"; 
  }
  
  # get the sanity check message and format as bullet list
   read.msgs <- paste("*", mSetObj$msgSet$read.msg);
   read.msgs <- paste0(read.msgs, collapse = "\\\n");

  descr <- c("### - Data integrity check \n\n",
               "A data integrity check is performed to make sure that all the necessary information has been collected. A summary of your data is below:\n\n",
                data.type, read.msgs, "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");

  # need to add metadata check
  if(substring(mSetObj$dataSet$format,4,5)=="mf"){

    read.msgs <- paste("*", mSet$msgSet$metacheck.msg);
    read.msgs <- paste0(read.msgs, collapse = "\\\n");

    descr <- c("### - Metadata integrity check\n\n",
               "A metadata integrity check is performed to make sure that all the necessary information has been collected. A summary of your metadata is below: \n",
                read.msgs, "\n");
    cat(descr, file=rmdFile, append=TRUE, sep="\n");

    link <- GetSharingLink(mSetObj);
    reportLinks <- getReportLinks(link, "metainfo");

    cat(reportLinks, file=rmdFile, append=TRUE);

    cat("\n\n", file=rmdFile, append=TRUE);

    # now the metadata table
    cmdhist2 <- c(
        "```{r table_metadata, echo=FALSE, out.width = '100%', results='asis', out.height= '100%', warning=FALSE}",
        "sum_dt <- mSetObj$dataSet$meta.info;",
         paste0("create_dt(sum_dt,  caption = 'Table ", 
            table.count<<-table.count+1, ". Summary of metadata.', table.name='pathora')"),
        "```", "\n\n")

    cat(cmdhist2, file=rmdFile, append=TRUE, sep="\n");
  }
  
  # the data filtering
  descr <- c("### - Data filtering\n\n",
             "Data filtering is crucial for improving data quality and statistical power by removing features that are unlikely to contribute to downstream", 
              "analysis. MetaboAnalyst provides four complementary filters: low-quality filter, low-repeatability filter, low-variance filter and low-abundance filter.", 
              "The <u>low-quality filter</u> removes features identified as background or contaminants (when blank samples are provided), or containing high proportions", 
              "of missing values; <u>low-repeatability filter</u> removes features exhibiting high relative standard deviation (RSD) among QC replicates;", 
              "<u>low-variance filter</u> discards near-constant features; and <u>low-abundance filter</u> excludes features with baseline-level intensities.", 
              "Data filter is strongly recommended for datasets with large number of variables especially for untargeted metabolomics data.",
             "\n");
  cat(descr, file=rmdFile, append=TRUE);
  cat("\n\n", file=rmdFile, append=TRUE);
  
  filt.msg <- mSetObj$msgSet$filter.msg;
  if(is.null(filt.msg)){
    cat("No data filtering was performed.\n\n", file=rmdFile, append=TRUE, sep="\n");
  }else{
    filt.msg <- paste("*", filt.msg);
    filt.msg <- paste0(filt.msg, collapse = "\\\n");
    descr <- c("\n",
                filt.msg,
                "\n\n");
    cat(descr, file=rmdFile, append=TRUE);
  }

  if(is.null(mSet$msgSet$replace.msg))
    mSet$msgSet$replace.msg <- "No missing value imputation was performed."

  missingMsg <- paste("*", mSet$msgSet$replace.msg);
  missingMsg <- paste0(missingMsg, collapse = "\\\n");

  descr <- c("### - Missing value imputations\n\n",
             "Too many zeroes or missing values will cause difficulties during downstream analysis.",
              "MetaboAnalyst provides three imputation strategies – <u>left-censored data estimation</u>, <u>univariate statistical methods</u>, and <u>multivariate statistical methods</u>.", 
              "The default approach assumes missing values are due to values falling below the detection limit (left-censored data). Users can replace missing", 
              "values with their estimated detection limits (1/5 of the minimum positive value observed for each individual feature). A drawback of this method", 
              "is the introduction of many identical, small constant values. The quantile regression imputation of left-censored data (QRILC) method models the", 
              "low tail of each feature as log-normal and samples replacements, thereby preserving inherent variance without introducing downward bias. Users can", 
              "also explore other univariate (min, mean, median) or multivariate (KNN, PCA, or Random Forest) methods which leverage correlations between features", 
              "or samples to estimate the missing entries.",
             missingMsg,
             "\n");
  cat(descr, file=rmdFile, append=TRUE, sep="\n");
}

GetNameMappingDoc <- function(){
    descr <- c("### - Name mapping\n\n",
             "The first step is to standardize the compound labels. It is an essential step since the compound",
             "labels will be subsequently compared with compounds contained in the metabolite set library.",
             "MSEA has a built-in tool to convert between compound common names, synonyms, identifiers used in",
             "HMDB ID, PubChem, ChEBI, BiGG, METLIN, KEGG, or Reactome.",
             "**Table 1** shows the conversion results. Note: *1* indicates exact match, *2*",
             "indicates approximate match, and *0* indicates no match. A text file contain the result can be",
             "found the downloaded file <a href='./name_map.csv' target='_blank'>*name_map.csv*</a>.");
    return(descr);
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create normalization document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateNORMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$dataSet$norm)){
    errorMsg <- c("It seems that data normalization has not been performed yet.",
                  "Please choose a proper data normalization to proceed.",
                  "You can also turn off normalization by selecting the ```None``` option.");
    cat(errorMsg, file=rmdFile, sep="\n", append=TRUE);
    return();
  }
  
  descr1 <- c("### - Data normalization\n\n",
              "Normalization aims to make data more comparable across samples, and to transform them 
               to be more suitable for statistical analysis and visualization. The normalization procedures are grouped into three categories.
              Sample normalization allows adjustment for systematic differences among samples that are not related to the conditions of interest; 
              data transformation and scaling are two different approaches to make features more comparable. The main difference is that 
              data transformation operates on individual values, while data scaling also takes into account of the variable distributions. 
              You can use one or combine both to achieve better results.",
              "\n\n",
              "The normalization consists of the following options:\n\n");
  
  cat(descr1, file=rmdFile, append=TRUE);
  
  if(!is.null(mSetObj$dataSet[["rownorm.method"]])){
    norm.desc <- paste("Sample normalization: ```", mSetObj$dataSet$rownorm.method, "```; ",
                       "Data transformation: ```",mSetObj$dataSet$trans.method, "```; ",
                       "Data scaling: ```",mSetObj$dataSet$scale.method, "```.", sep="");
  } else {
    norm.desc <- "No normalization methods were applied."
  }

  descr2 <- c("1. Sample normalization:",
              " + Sample specific normalization (i.e. normalize by dry weight, volume)",
              " + Normalization by the sample sum",
              " + Normalization by the sample median",
              " + Normalization by a reference sample (probabilistic quotient normalization or PQN)",
              " + Normalization by a pooled or average sample from a particular group (group PQN)",
              " + Normalization by a reference feature (i.e. creatinine, internal control)",
              " + Quantile normalization",
              
              "2. Data transformation:",
              " + Log transformation (base 10)",
              " + Log transformation (base 2)",
              " + Square root transformation",
              " + Cube root transformation",
              " + Variance stabilizing normalization (data-adaptive transformation)",
              
              "3. Data scaling:",
              " + Mean centering (mean-centered only)",
              " + Auto scaling (mean-centered and divided by standard deviation of each variable)",
              " + Pareto scaling (mean-centered and divided by the square root of standard deviation of each variable)",
              " + Range scaling (mean-centered and divided by the value range of each variable)",
              "\n\n",
              norm.desc,
              "\n\n",
              if(exists("norm", where=mSetObj$imgSet)){
                paste("Figure", fig.count<<-fig.count+1," shows the effects before and after normalization.\n\n");
              })
  
  cat(descr2, file=rmdFile, append=TRUE, sep="\n");
  
  if(exists("norm", where=mSetObj$imgSet)){
    # norm view (box plots + kernal density)
    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "norm", "norm");
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);
    
    fig <- c(paste0("```{r figure_sn, echo=FALSE, fig.pos='H', fig.cap='Figure ", 
                    fig.count, 
                    ". Box plots and kernel density plots before and after normalization. The boxplots show at most 50 features due to space limit. The density plots are based on all samples.", 
                    "', ",
                    " fig.lp='", 
                    mSetObj$imgSet$norm, 
                    "', out.width = '", getFigWidth(mSetObj), "'}"),
             "knitr::include_graphics(mSetObj$imgSet$norm)",
             "```",
             "\n\n");
    
    cat(fig, file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create footer
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateRHistAppendix_old <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  # Corrected file existence check
  if(file.exists("Rhistory.R")){
    if(length(mSetObj$cmdSet) == 0){
      cmdhist <- "No commands found";
    } else {
      cmdhist <- mSetObj$cmdSet;
    }
    cmdhist <- c("```{r cmd_hist2, eval=FALSE}",
                 cmdhist,
                 "```", "\n\n")
    cat(cmdhist, file=rmdFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create footer
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateRHistAppendix <- function(mSetObj=NA){
  #cmdSet <- readSet(cmdSet, "cmdSet")
  mSetObj <- .get.mSet(mSetObj);
  descr <- c("\n\n## Appendix: R Command History\n\n")
    cat(descr, file=rmdFile, append=TRUE, sep="\n");

  if(mSetObj$paramSet$report.format == "pdf"){
    # Corrected file existence check

      cmdhist <- "To access R Command history, please download it from your project folder."
      cat(cmdhist, file=rmdFile, append=TRUE, sep="\n");
    
  }else{
  cmdhist_js_safe <- jsonlite::toJSON(mSetObj$cmdSet, auto_unbox = TRUE)
  
  js_code <- sprintf('
<script>
  var cmdHistContent = %s;
  
  function downloadCmdHist() {
    var blob = new Blob([cmdHistContent.join("\\n")], { type: "text/plain" });
    var url = window.URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.style.display = "none";
    a.href = url;
    a.download = "R_Command_History.txt";
    document.body.appendChild(a);
    a.click();
    window.URL.revokeObjectURL(url);
    document.body.removeChild(a);
  }
</script>

[Download R Command History](javascript:downloadCmdHist())
', cmdhist_js_safe)
  
  cat(js_code, file=rmdFile, append=TRUE)
}
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report footer
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'XiaLab Analytics
#'All rights reserved
#'@export
#'
CreateFooter <- function(){
  # # R Session information
  # 
  # ```{r sessionInfo, echo=FALSE}
  # sessionInfo()
  # ```
  # 
  end <- c("<br/>\n\n--------------------------------<br/>\n\n", 
           "The report was generated on `r date()` with OS system: `r Sys.info()['sysname']`, version: `r gsub('#[0-9]+', '', Sys.info()['version'])`");
  
  cat(end, file=rmdFile, append=TRUE);
}

SetReportImgMap <- function(mSetObj=NA, mapStr){
  mSetObj <- .get.mSet(mSetObj);
  library(rjson);
  reportSet <- rjson::fromJSON(mapStr)
  mSetObj$imgSet$reportSet <- reportSet;
  .set.mSet(mSetObj);
}

getFigCount <- function(){
  fig.count <<- fig.count + 1;
  return(fig.count);
}

getTableCount <- function(){
  table.count <<- table.count + 1;
  return(table.count);
}

getCurrentFigCount <- function(){
  fig.count <<- fig.count + 1;
  return(fig.count);
}

#table.name is for select/unselect table columns, especially for pdf
create_dt <- function(x, caption="", table.name="", escape = TRUE, rmRowNM = FALSE){
  library(dplyr)
  
  # Check if the dataframe is empty
  if(nrow(x) == 0 || ncol(x) == 0) {
    message("The data frame is empty. No table generated.")
    return(NULL)
  }
  
  output_format <- mSet$paramSet$report.format;
  x <- as.data.frame(x)
  x <- x %>%
    mutate_if(is.numeric, ~ signif(., digits = 5))
  
  if(output_format == "html") {

    if(rmRowNM){
        DT::datatable(x,
                      extensions = 'Buttons',
                      caption = caption,
                      escape = escape,
                      rownames = NULL,
                      options = list(dom = 'Blfrtip',
                                     fillContainer = TRUE,
                                     buttons = c('copy', 'csv'),
                                     lengthMenu = list(c(10, 25, 50, -1),
                                                       c(10, 25, 50, "All"))))
    } else {
        DT::datatable(x,
                      extensions = 'Buttons',
                      caption = caption,
                      escape = escape,
                      options = list(dom = 'Blfrtip',
                                     fillContainer = TRUE,
                                     buttons = c('copy', 'csv'),
                                     lengthMenu = list(c(10, 25, 50, -1),
                                                       c(10, 25, 50, "All"))))
    }

  } else if(output_format == "pdf") {
    if(table.name != ""){
      x <- adjustTable(x, table.name);
    }
    
    # Escape special LaTeX characters in the data frame before using knitr::kable
    x <- mutate_all(x, function(column) {
      if(is.character(column)) {
        column <- gsub("%", "\\%", column, fixed = TRUE)
        column <- gsub("&", "\\&", column, fixed = TRUE)
        column <- gsub("_", "\\_", column, fixed = TRUE)
        column <- gsub("#", "\\#", column, fixed = TRUE)
        column <- gsub("\\$", "\\\\$", column, fixed = TRUE)
      }
      column
    })
    x_top50 <- head(x, 50) # Take top 50 for the table in PDF
    # Adding options to make sure the table is not too wide
    library(kableExtra);
    kable_output <- kable(x_top50, format = "latex", booktabs = TRUE, caption = caption, escape = escape)
    max_row_length <- 0
    
    if(nrow(x_top50) > 0) {
      max_row_length <- max(sapply(1:nrow(x_top50), function(i) {
        row_length <- sum(nchar(as.character(x_top50[i, ]))) + (ncol(x_top50) - 1) * 2  # Adding 2*n for column spaces
        if (is.na(row_length)) 0 else row_length  # Replace NA with 0
      }))
    }
    message("max_row_length: ", max_row_length)
    # Define a threshold for what you consider "too wide"
    # This number will depend on your typical font, font size, and page width
    threshold_length <- 80;
    # Check if the table is too wide
    if (max_row_length > threshold_length) {
      kable_output <- kable_output %>%
        kable_styling(latex_options = c("striped", "scale_down", "HOLD_position", "longtable"))
    }
    kable_output;
    } else if(output_format == "slides") {
        library(kableExtra)
        x_top <- head(x, 10) # Take top 50 for the table in PDF

        # Calculate the maximum row length - might adjust based on your needs
        max_row_length <- max(sapply(1:nrow(x_top), function(i) {
            row_length <- sum(nchar(as.character(x_top[i, ]))) + (ncol(x_top) - 1) * 2  # Adding 2*n for column spaces
            if (is.na(row_length)) 0 else row_length  # Replace NA with 0
        }))
        message("max_row_length: ", max_row_length)

        # Define a threshold for what you consider "too wide"
        threshold_length <- 80  # Adjust based on your slide's content area

        # Create the table using kbl() for HTML output
        kable_output <- kbl(x_top, format = "simple", caption = caption,  escape = escape) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center", font_size = 12)

        # If the table is too wide, adjust styling
        if (max_row_length > threshold_length) {
            kable_output <- kable_output %>%
                scroll_box(width = "700px", height = "200px")  # Adjust dimensions as needed
        }
        
        return(kable_output)
    } else {
    stop("Unsupported output format")
  }
}


makeReadable <- function(str){
    result <- switch(str,
                 pct = "Percent",
                 abs = "Absolute",
                 log = "Log2",
                 rle = "RLE",
                 array = "Microarray",
                 count= "RNA-Seq",
                 hsa = "H. sapiens (human)",
                 mmu = "M. musculus (mouse)",
                 rno = "R. norvegicus (rat)",
                 cel = "C. elegans (roundworm)",
                 dme = "D. melanogaster (fruitfly)",
                 dre = "D. rerio (zebrafish)",
                 sce = "S. cerevisiae (yeast)",
                 eco = "E. coli",
                 ath = "A. thaliana (Arabidopsis)",
                 bta = "B. taurus (cow)",
                 gga = "G. gallus (chicken)",
                 mun = "M. unguiculatus (Mongolian gerbil)",
                 bsu = "B. subtilis",
                 pae = "P. aeruginosa",
                 mtb = "M. tuberculosis",
                 smm = "S. mansoni (schistosomiasis)",
                 tbr = "T. brucei (trypanosoma)",
                 pfa = "P. falciparum (malaria)",
                 cjo = "C. japonica (japanese quail)",
                 xla = "X. laevis (African clawed frog)",
                 ppr = "P. promelas (fathead minnow; custom)",
                 fhm = "P. promelas (fathead minnow; NCBI)",
                 nlf = "L. pipiens (northern leopard frog)",
                 omk = "O. mykiss (rainbow trout)",
                 ham = "H. americanus (American lobster)",
                 cdi = "C. dilutus",
                 dma = "D. magna",
                 rsu = "R. subcapitata",
                 haz = "H. azteca",
                 fcd = "F. candida",
                 "entrez" = "Entrez ID",
                 "refseq" = "RefSeq ID",
                   "gb" = "Genbank ID",
                   "symbol" = "Official Gene Symbol",
                   "embl_gene" = "Ensembl Gene ID",
                   "embl_transcript" = "Ensemble Transcript ID",
                   "embl_protein" = "Ensembl Protein ID",
                   "uniprot" = "Uniprot Accession ID",
                   "kegg" = "KEGG",
                    "reactome" = "Reactome",
                    "go_bp" = "GO:BP",
                    "go_mf" = "GO:MF",
                    "go_cc" = "GO:CC",
                    "panth" = "PANTHER Slim",
                    "motif_set" = "Motif",
                 str)
}

getFigWidth <- function(mSetObj=NA, width="720px", widthPct="100%"){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$paramSet$report.format == "html"){
    output <- width;
  }else{
    output <- widthPct;
  }
  return(output);
}

#remove unnecessary columns
adjustTable <- function(x, table.name){

    if(table.name == "pathora"){
        x[["-log10(p)"]] <- NULL;
    }else if(table.name == "namemap"){
        x[["SMILES"]] <- NULL;
    }
    return(x)
}

addParam <- function(param){
    if(report.format == "pdf"){
        paste0("");
    }else{

    }
}

CreateRHistSlides <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  # Start of the R Command History Slide
  cat("## R Command History Overview\n\n", file = rmdFile, append = TRUE)
  
  # Simplified content for slide presentation
  cat("This section provides an overview of the key R commands used during the analysis. For a detailed command history, please refer to the downloadable file provided below.\n\n", file = rmdFile, append = TRUE)
  
  # Provide a conditional approach for command history availability
  if(file.exists("Rhistory.R") && length(mSetObj$cmdSet) > 0) {
    # Assuming a limited set of key commands is summarized
    cat("### Key Commands Used:\n\n", file = rmdFile, append = TRUE)
    # Example: Displaying a few key commands or steps
    keyCommands <- head(mSetObj$cmdSet, 5)  # Adjust as necessary
    for(cmd in keyCommands) {
      cat("- `", cmd, "`\n", file = rmdFile, append = TRUE, sep = "")
    }
    cat("\n", file = rmdFile, append = TRUE)
    
    # Provide a download link for the full command history
    cat("For the full list of commands, please download the R Command History file.\n\n", file = rmdFile, append = TRUE)
    
    # Assuming a mechanism to provide a download link, simplified for slides
    cat("[Download Full R Command History](path/to/Rhistory.R)\n\n", file = rmdFile, append = TRUE)  # Adjust link/path as necessary
  } else {
    # Case where command history is not available or not performed
    cat("No commands were recorded or the analysis has not been performed yet.\n\n", file = rmdFile, append = TRUE)
  }
  
  cat("\n---\n\n", file = rmdFile, append = TRUE)  # Slide separator for ioslides or similar formats
}


#'Create MetaAnalysis table of results for Upset Diagram
#'@description Report generation using Sweave
#'Function to create a table containing meta-analysis results.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Guangyan Zhou
#'XiaLab Analytics
#'All rights reserved
#'@export
CreateUpSetDoc <- function(mSetObj=NA) {
  # Read the parameter set
  mSetObj <- .get.mSet(mSetObj);
  
  # Check if the heatmap key exists in the jsonNms
  if (!is.null(mSetObj$imgSet$reportSet$upset) && file.exists(mSetObj$imgSet$reportSet$upset)) {
    link <- GetSharingLink(mSetObj)

    descr <- c(
      "#### - UpSet Diagram"," \n",
      "An UpSet plot is a specialized type of visualization tool designed for 
       the quantitative analysis of sets, their intersections, and aggregates.
       Developed as an alternative to Venn and Euler diagrams, UpSet plots aim to provide a more scalable 
       and interpretable representation of set intersections, particularly when dealing with a large number of sets 
       and/or complex overlapping relationships among them. In this case, we are looking at the overlap of significant compounds from each dataset and meta-analysis result",
      "\n");
    cat(descr, file = rmdFile, append = TRUE, sep="\n");

    reportLinks <- getReportLinks(link, "upset");
    cat(paste(reportLinks, "\n\n"), file=rmdFile, append=TRUE);

    upset.desc <- paste("Upset diagram comparing multiple results. Each bar indicates the number of significant features identified in",
                   "individual datasets. The dots underneath the bars show where each list came from. Each row corresponds to one dataset.",
                   "If a dot is filled in, it means that that dataset contributed features towards the bar above it.");
    fig_num <- fig.count <<- fig.count+1;
    # image rendering in R Markdown
    img <- paste0("```{r figure_upset, echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ",
       fig_num, 
       ": ", upset.desc, "', out.width='", getFigWidth(mSetObj), "'}\n",
       "  knitr::include_graphics('", mSetObj$imgSet$reportSet$upset, "')\n",
       "```",
       "\n\n")

    
    cat(img, file = rmdFile, append = TRUE, sep="\n");
  }
}

GetModuleName <- function(){
  module.nm <- "";
  if(anal.type == "stat" ){
    module.nm <- "Statistical Analysis with Single Factor";
  }else if(anal.type == "mf"){
    module.nm <- "Statistical Analysis with Complex Metadata";
  }else if(substr(anal.type, 0, 4) == "mset"){
    module.nm <- "Enrichment Analysis";
  }else if(anal.type == "power"){
    module.nm <- "Power Analysis";
  }else if(anal.type == "roc"){
    module.nm <- "Biomarker Analysis";
  }else if(anal.type == "pathinteg"){
    module.nm <- "Integrative Pathway Analysis";
  }else if(substr(anal.type, 0, 4) == "path"){ # must be after pathiteg
    module.nm <- "Pathway Analysis";
  }else if(anal.type == "network"){
    module.nm <- "Network Analysis";
  }else if(anal.type == "mummichog"){
    module.nm <- "Functional Analysis of LC-MS Metabolomics Data";
  }else if(anal.type == "metapaths"){
    module.nm <- "Functional Meta-analysis of LC-MS Metabolomics Data";
  }else if(anal.type == "metadata"){
    module.nm <- "Statistical Meta-analysis";
  }else if(anal.type == "raw"){
    module.nm <- "LC-MS Spectra Processing";
  }else if(anal.type == "tandemMS"){
    module.nm <- "MS/MS Spectra Annotation";
  }else if(anal.type == "mgwas"){
    module.nm <- "Causal Analysis (Mendelian Randomization)";
  }else if(anal.type == "dose"){
    module.nm <- "Dose Response Analysis";
  }
  return(module.nm);
}


#'Create report of analyses
#'@description Report generation using Sweave
#'Create correlation document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@xialab.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
AddFeatureImages <- function(mSetObj=NA) {
  # Read the parameter set
  mSetObj <- .get.mSet(mSetObj)
  link <- GetSharingLink(mSetObj)
  imgSet <- mSetObj$imgSet
  print(imgSet$featureList);
  if (!is.null(imgSet$featureList) && length(imgSet$featureList) > 0) {    
    descr <- c(
      "## Feature of interest", "\n",
      "Below, you will find the gene expression profiles of key features that have been saved throughout the course of the analysis.",
      "\n"
    )
    
    cat(descr, file = rmdFile, append = TRUE, sep="\n")

    # Image rendering in R Markdown
    img_blocks <- list()
    for(i in seq_along(imgSet$featureList)) {
      imgCmd <- gsub("(.*)_[0-9]+_dpi[0-9]+\\.png", "\\1", imgSet$featureList[[i]])
      figCaption <- sprintf("Figure %s: %s", getFigCount(), names(imgSet$featureList)[i])
      
      if (mSetObj$paramSet$report.format == 'html') {
        reportLinks <- paste0('<div style="text-align: center;">',
                              '<a href="', link, '&format=pdf&imgCmd=', imgCmd, '" target="_blank">PDF</a> ',
                              '<a href="', link, '&format=svg&imgCmd=', imgCmd, '" target="_blank">SVG</a>',
                              '</div>')
      } else {
        # Escape special characters for LaTeX
        reportLinks <- ""
        figCaption <- gsub("_", "\\\\\\\\_", figCaption)
      }

      chunkOptions <- paste0(
        "```{r figure_feature", i ,", echo=FALSE, fig.align='center', fig.pos='H', fig.cap='", figCaption, "', out.width='", getFigWidth("40%"), "'}"
      )

      img_block <- paste0(reportLinks, 
                          "\n\n", chunkOptions, "\n",
                          sprintf("knitr::include_graphics('%s')", imgSet$featureList[[i]]),
                          "\n```\n")
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
}
