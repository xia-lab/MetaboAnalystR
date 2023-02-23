#'Create report of analyses
#'@description Report generation using Sweave
#'Note: most analyses were already performed, only need to embed
#'the results to the right place without rerunning the whole analysis
#'through Sweave. Only some auxilliary info (i.e. time, version etc need to
#'run in R through Sweave
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param usrName Input the name of the user
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PreparePDFReport <- function(analType, usrName, page){

  tmp.dir <- tempdir()
  if(!dir.exists(file.path(tmp.dir))){

    err.pdf.msg <<- "There is an issue in PreparePDFReport. tempdir() of R does not exist. Please make sure to create the folder or run Rserve under sudo privilege."
    print(err.pdf.msg);
    save.image("PDFErrors.RData"); # the error message might not show. Therefore, save image can be useful

    result = tryCatch({
      dir.create(tmp.dir)
    }, warning = function(w) {
      return(0)
    }, error = function(e) {
      return(0)
    }, finally = {
      doNothing = 1;
    })    
  }
  
  # create the Rnw file
  file.create("Analysis_Report.Rnw");

  # open for write
  rnwFile <<- file("Analysis_Report.Rnw", "w")
  
  # create a global counter to label figures
  fig.count <<- 0;
  table.count <<- 0;
  
  CreateRNAseqSummaryReport(usrName, page);
  
  # close opened files
  close(rnwFile);
  
  if(!.on.public.web){
    Sweave("Analysis_Report.Rnw", encoding="utf8");
    res <- try(tools::texi2dvi("Analysis_Report.tex", pdf = TRUE, quiet=TRUE));
  }
  
  return(1);
}

# this is for PDF report generation from bash
SaveCurrentSession <- function(){
    file.copy("../../data/Sweave.sty", ".")
    save.image("SweaveImage.RData");
}

CreateHeader <- function(usrName){
  header <- c("\\documentclass[a4paper]{article}",
              "\\usepackage[margin=1.0in]{geometry}",
              "\\usepackage{longtable}",
              "\\usepackage{lmodern}",
              "<<echo=false>>=",
              "options(width=60);",
              "@",
              "\\SweaveOpts{eps=FALSE,pdf=TRUE}",
              "\\title{ExpressAnalyst Analysis Report}",
              "\\begin{document}",
              "\\parskip=.3cm",
              "\\maketitle");
  cat(header, file=rnwFile, sep="\n", append=TRUE);
  
}


#'Report footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateFooter <- function(){
  end <- c("\\vspace{5 mm}\n--------------------------------\n\n",
           "The report was generated on \\Sexpr{date()} with \\Sexpr{print(version$version.string)}.\n",
           "\\end{document}\n\n"
  );
  cat(end, file=rnwFile, append=TRUE);
}
