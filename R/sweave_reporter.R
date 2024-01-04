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
PreparePDFReport<-function(mSetObj=NA, usrName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # create the Rnw file
  file.create("Analysis_Report.Rnw");
  # open for write
  rnwFile <<- file("Analysis_Report.Rnw", "w")
  
  # create a global counter to label figures
  fig.count <<- 0;
  table.count <<- 0;
  
  anal.type <- mSetObj$analSet$type;
  
  if(anal.type == "stat" ){
    CreateStatRnwReport(mSetObj, usrName);
  }else if(anal.type == "mf"){
    CreateTimeSeriesRnwReport(mSetObj, usrName);
  }else if(substr(anal.type, 0, 4) == "mset"){
    CreateEnrichRnwReport(mSetObj, usrName);
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
  }else if(anal.type == "mummichog"){
    CreateMummichogRnwReport(mSetObj, usrName);
  }else if(anal.type == "metapaths"){
    CreateMetaPathRnwReport(mSetObj, usrName);
  }else if(anal.type == "metadata"){
    CreateMetaAnalysisRnwReport(mSetObj, usrName);
  }else if(anal.type == "raw"){
    CreateRawAnalysisRnwReport(mSetObj, usrName);
  }else if(anal.type == "metapaths"){
    CreateMummichogRnwReport(mSetObj, usrName);
  }else{
    AddErrMsg(paste("No template found for this module:", anal.type));
    return(0);
  }
  
  # close opened files
  close(rnwFile);
  
  if(!.on.public.web){
    utils::Sweave("Analysis_Report.Rnw", encoding="utf8");
    res <- try(tools::texi2dvi("Analysis_Report.tex", pdf = TRUE, quiet=TRUE));
  }
  return(1);
}

# this is for PDF report generation from bash
SaveCurrentSession <- function(){
  file.copy("../../rscripts/_sweave.sty", ".")
  save.image("SweaveImage.RData");
}

CreateHeader <- function(usrName){
  header <- c("\\documentclass[a4paper]{article}",
              "\\usepackage[margin=1.0in]{geometry}",
              "\\usepackage{longtable}",
              "\\usepackage{graphicx}",
              "\\usepackage{grffile}",
              "<<echo=false>>=",
              "options(width=60);",
              "@",
              "\\SweaveOpts{eps=FALSE,pdf=TRUE}",
              "\\title{Metabolomic Data Analysis with MetaboAnalyst 6.0}",
              paste("\\author{ Name: ", usrName, " }", sep=""),
              "\\begin{document}",
              "\\parskip=.3cm",
              "\\maketitle");
  cat(header, file=rnwFile, sep="\n", append=TRUE);
  
}

#'Create report of analyses 
#'@description Report generation using Sweave
#'Create a summary table for each type of uploaded data
#'csv table has 5 col: sampleID, feature #, zero,  missing #
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import qs
CreateSummaryTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  sum.dat<-NULL;
  plenth <- mSetObj$dataSet$proc.feat.num;
  if(mSetObj$dataSet$type=='conc'| mSetObj$dataSet$type=='pktable'| mSetObj$dataSet$type=='specbin' | mSetObj$dataSet$type=='pktable-ma'){
    orig.data<- qs::qread("data_orig.qs");
    for(i in 1:nrow(orig.data)){
      srow<-orig.data[i,];
      newrow<-c(sum(srow[!is.na(srow)]>0), (sum(is.na(srow)) + sum(srow[!is.na(srow)]<=0)), plenth);
      sum.dat<-rbind(sum.dat, newrow);
    }
    colnames(sum.dat)<-c("Features (positive)","Missing/Zero","Features (processed)");
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
    colnames(sum.dat)<-c("Peaks (raw)","Missing/Zero", "Peaks (processed)");
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
  print(xtable::xtable(sum.dat, caption="Summary of data processing results"), caption.placement="top", size="\\scriptsize");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create normalization document
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateNORMdoc <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to check if this process is executed
  if(is.null(mSetObj$dataSet$norm)){
    errorMsg <- c("Error occured during normalization of your data ....",
                  "Fail to proceed. Please check if the data format you uploaded is correct.",
                  "Please use the OmicsForum (omicsforum.ca) for community based support!");
    cat(errorMsg, file=rnwFile, append=TRUE);
    return();
  }
  
  descr1 <- c("\\subsection{Data Normalization}\n",
              "The data is stored as a table with one sample per row and one variable (bin\\slash peak\\slash",
              "metabolite) per column. The normalization procedures implemented below are grouped into four categories.",
              "Sample specific normalization allows users to manually adjust concentrations based on biological inputs",
              "(i.e. volume, mass); row-wise normalization allows general-purpose adjustment for differences among samples;",
              "data transformation and scaling are two different approaches to make features more comparable.",
              "You can use one or combine both to achieve better results.",
              "\n\n",
              "The normalization consists of the following options:");
  
  cat(descr1, file=rnwFile, append=TRUE);
  
  if(!is.null(mSetObj$dataSet[["rownorm.method"]])){
      norm.desc <- paste("Row-wise normalization: ", mSetObj$dataSet$rownorm.method, "; ",
                         "Data transformation: ",mSetObj$dataSet$trans.method, "; ",
                         "Data scaling: ",mSetObj$dataSet$scale.method, ".", sep="");
  }else{
      norm.desc <- "No normalization methods were applied."
  }

  descr2 <- c("\\begin{enumerate}",
              "\\item{Row-wise procedures: }",
              "\\begin{itemize}",
              "\\item{Sample specific normalization (i.e. normalize by dry weight, volume) }",
              "\\item{Normalization by the sum }",
              "\\item{Normalization by the sample median }",
              "\\item{Normalization by a reference sample (probabilistic quotient",
              "normalization)\\footnote{Dieterle F, Ross A, Schlotterbeck G, Senn H. \\textit{Probabilistic quotient normalization as robust",
              "method to account for dilution of complex biological mixtures. Application in 1H NMR metabonomics}, 2006,",
              "Anal Chem 78 (13);4281 - 4290}}",
              "\\item{Normalization by a pooled or average sample from a particular group }",
              "\\item{Normalization by a reference feature (i.e. creatinine, internal control) }",
              "\\item{Quantile normalization }",
              "\\end{itemize}",
              "\\item{Data transformation : }",
              "\\begin{itemize}",
              "\\item{Log transformation (base 10)}",
              "\\item{Square root transformation}",
              "\\item{Cube root transformation}",
              "\\end{itemize}",
              "\\item{Data scaling: }",
              "\\begin{itemize}",
              "\\item{Mean centering (mean-centered only)}",
              "\\item{Auto scaling (mean-centered and divided by standard deviation of each variable)}",
              "\\item{Pareto scaling (mean-centered and divided by the square root of standard deviation of each variable)}",
              "\\item{Range scaling (mean-centered and divided by the value range of each variable)}",
              "\\end{itemize}",
              "\\end{enumerate}",
              "\n\n",
              if(exists("norm", where=mSetObj$imgSet)){
                paste("Figure", fig.count<<-fig.count+1,"shows the effects before and after normalization.\n");
              },
              norm.desc,
              "\n\n")
  
  cat(descr2, file=rnwFile, append=TRUE, sep="\n");
    
  if(exists("norm", where=mSetObj$imgSet)){
    cmdhist <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=1.0\\textwidth]{", mSetObj$imgSet$norm,"}", sep=""),
                  "\\caption{Box plots and kernel density plots before and after normalization.",
                  "The boxplots show at most 50 features due to space limit. The density plots are based on all samples.}",
                  "\\end{center}",
                  paste("\\label{",mSetObj$imgSet$norm,"}", sep=""),
                  "\\end{figure}",
                  "\\clearpage"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
  cat("\\clearpage", file=rnwFile, append=TRUE, sep="\n");
}

#'Create report of analyses
#'@description Report generation using Sweave
#'Create footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
CreateRHistAppendix <- function(){
  
  descr <- c("\\section{Appendix: R Command History}\n");
  cat(descr, file=rnwFile, append=TRUE);
  
  if(!is.null("Rhistory.R")){
    
    cmdhist<-c("<<echo=false, results=verbatim>>=",
               "GetRCommandHistory(mSet);",
               "@"
    );
    cat(cmdhist, file=rnwFile, append=TRUE, sep="\n");
  }
}

#'Create report of analyses (Met Enrichment)
#'@description Report generation using Sweave
#'Metabolite enrichment analysis report footer
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CreateFooter <- function(){
  end <- c("\\vspace{5 mm}\n--------------------------------\n\n",
           "The report was generated on \\Sexpr{date()} with \\Sexpr{print(version$version.string)}, OS system:",
           "\\Sexpr{Sys.info()['sysname']}, version: \\Sexpr{gsub('#[0-9]+', '', Sys.info()['version'])} .\n",
           "\\end{document}\n\n"
  );
  cat(end, file=rnwFile, append=TRUE);
}
