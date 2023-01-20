CreateRNAseqSummaryReport <- function(usrName, page){
  CreateHeader(usrName);  
  CreateReportIntr();

  home.dir <- getwd();
  data.orig <- qs::qread("data.orig.qs");  
  if(page == "Upload Data"){
    upload <- list(org = data.org, type = dataSet$type, id = dataSet$id.orig, glsum = dataSet$gl.summary);
    CreateUploadSection(upload);

  } else if(page == "quality control") {
    upload <- list(org = data.org, type = dataSet$type, id = dataSet$id.orig, glsum = dataSet$gl.summary);
    qualch <- list(tot.feat = as.character(dim(data.orig)[1]), 
                     match.ID = as.character(dim(dataSet$data.anot)[1]));
    files.name <- list.files(home.dir)

    CreateUploadSection(upload);   
    CreateQCSection(qualch, files.name);

  } else if(page == "Normalization") {
    upload <- list(org = data.org, type = dataSet$type, id = dataSet$id.orig, glsum = dataSet$gl.summary);
    qualch <- list(tot.feat = as.character(dim(data.orig)[1]), 
                     match.ID = as.character(dim(dataSet$data.anot)[1]));
    norm <- list(var.perc = dataSet$var.perc, abun.perc = dataSet$abun.perc, norm.method = dataSet$norm.opt);
    files.name <- list.files(home.dir)

    CreateUploadSection(upload);   
    CreateQCSection(qualch, files.name);
    CreateNormSection(norm, files.name);

  } else if(page == "DE Analysis") {
    upload <- list(org = data.org, type = dataSet$type, id = dataSet$id.orig, glsum = dataSet$gl.summary);
    qualch <- list(tot.feat = as.character(dim(data.orig)[1]), 
                     match.ID = as.character(dim(dataSet$data.anot)[1]));
    norm <- list(var.perc = dataSet$var.perc, abun.perc = dataSet$abun.perc, norm.method = dataSet$norm.opt);
    dea <- list(ctrl.cond = ref.cond, pval = dataSet$pval, fdr = dataSet$fdr, lfc = dataSet$lfc);
    files.name <- list.files(home.dir)

    CreateUploadSection(upload);   
    CreateQCSection(qualch, files.name);
    CreateNormSection(norm, files.name);
    CreateDEASection(dea);

  } else {
    upload <- list(org = data.org, type = dataSet$type, id = dataSet$id.orig, glsum = dataSet$gl.summary);
    qualch <- list(tot.feat = as.character(dim(data.orig)[1]), 
                     match.ID = as.character(dim(dataSet$data.anot)[1]));
    norm <- list(var.perc = dataSet$var.perc, abun.perc = dataSet$abun.perc, norm.method = dataSet$norm.opt);
    dea <- list(ctrl.cond = ref.cond, pval = dataSet$pval, fdr = dataSet$fdr, lfc = dataSet$lfc);
    fitcur <- list(models = models, lof.pval = lof.pval, bmr.fact = num.sds);
    files.name <- list.files(home.dir)

    CreateUploadSection(upload);   
    CreateQCSection(qualch, files.name);
    CreateNormSection(norm, files.name);
    CreateDEASection(dea);
    CreateCurveSection(fitcur, files.name);

  }

  CreateFooter();
}

#### Introduction sections
CreateReportIntr <- function(){
  descr <- c("\\section{EcoOmicsAnalyst Overview}\n",
             "EcoOmicsAnalyst is a computationally efficient implementation of the National Toxicology Program Approach to Genomic Dose-Response Modeling. ", 
             "Key features include a simplified workflow, ability to download results at each step, and interactive exploration ",
             "of pathway-level BMDs. In addition to model organisms, EcoOmicsAnalyst is designed to support non-model organisms by ",
             "enabling an annotation-free pipeline that can do gene-level and transcriptome-level BMD analysis. ",
             "The EcoOmicsAnalyst analysis pipeline consists of several steps: uploading the data, quality control,",
             " normalization, differential expression analysis, curve fitting and gene-level BMD calculation, ",
             "and then interactive exploration of pathway-level BMDs. This report displays the parameters used ",
             "for each of the sections in the analysis pipeline. \n"
  );
  cat(descr, file=rnwFile, append=TRUE);
}

#### Analysis Sections
CreateUploadSection <- function(params){
  
  descr <- c("\\section{Data Upload}\n",
            "EcoOmicsAnalyst accepts tab-delimited text files (.txt) that contain a matrix of gene expression values as input. ",
            "The parameters that you chose are listed in the bullets below: ",
             "\\begin{itemize}",
             "\\item{Organism: ", params[["org"]], "}",
             "\\item{Data type: ", params[["type"]], "}",
             "\\item{Gene IDs: ", params[["id"]], "}",
             "\\item{Gene-level summarization: ", params[["glsum"]], "}",
             "\\end{itemize}",
             "\n\n");
  
  cat(descr, file=rnwFile, append=TRUE);
  CreateDataSummaryTable();
}

CreateQCSection <- function(params, files){

    pca.img <- getImageName(files, "qc_pca_");
    bp.img <- getImageName(files, "qc_boxplot_");
    convertPNG2PDF(bp.img);
    convertPNG2PDF(pca.img);

    descr <- c("\\section{quality control}\n",
            "Figures 1 and 2 are summaries of the data before normalization. The results of the ",
            "data annotation are described below: ",
             "\\begin{itemize}",
             "\\item{Number of uploaded features: ", params[["tot.feat"]], "}",
             "\\item{Number of matched IDs: ", params[["match.ID"]], "}",
             "\\end{itemize}",
             "\n\n");
    
    cat(descr, file=rnwFile, append=TRUE);

    pca.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", pca.img,"}", sep=""),
                  "\\caption{", "PCA plot of raw uploaded data.","}",
                  "\\end{center}", 
                  paste("\\label{",pca.img,"}", sep=""),
                  "\\end{figure}");

    cat(pca.plot, file=rnwFile, append=TRUE, sep="\n");

    bp.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", bp.img,"}", sep=""),
                  "\\caption{", "Box plot of raw uploaded data.","}",
                  "\\end{center}", 
                  paste("\\label{",bp.img,"}", sep=""),
                  "\\end{figure}");

    cat(bp.plot, file=rnwFile, append=TRUE, sep="\n");
}

CreateNormSection <- function(params, files){

    norm.pca.img <- getImageName(files, "qc_norm_pca_");
    norm.bp.img <- getImageName(files, "qc_norm_boxplot_");
    convertPNG2PDF(norm.bp.img);
    convertPNG2PDF(norm.pca.img);

    descr <- c("\\section{Normalization and Filtering}\n",
               "Filtering removes data that are unlikely to be informative or are simply erroneous. ",
               "Normalization ensures that the distribution of expression values for each sample are similar across the entire experiment. ", 
               "It is crucial for statistically robust and reliable detection of transcriptional differences across treatment groups. ",
               "Figures 3 and 4 summarize the data after normalization. The parameters that you chose for this section are ",
               "summarized in the bullets below: ",
               "\\begin{itemize}",
               "\\item{Variance filter percentile: ", params[["var.perc"]], "}",
               "\\item{Abundance filter percentile: ", params[["abun.perc"]], "}",
               "\\item{Normalization method: ", params[["norm.method"]], "}",
               "\\end{itemize}",
               "\n\n");
    cat(descr, file=rnwFile, append=TRUE);

    npca.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", norm.pca.img,"}", sep=""),
                  "\\caption{", "PCA plot of normalized data.","}",
                  "\\end{center}", 
                  paste("\\label{",norm.pca.img,"}", sep=""),
                  "\\end{figure}");

    cat(npca.plot, file=rnwFile, append=TRUE, sep="\n");

    nbp.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", norm.bp.img,"}", sep=""),
                  "\\caption{", "Box plot of normalized data.","}",
                  "\\end{center}", 
                  paste("\\label{",norm.bp.img,"}", sep=""),
                  "\\end{figure}");

    cat(nbp.plot, file=rnwFile, append=TRUE, sep="\n");
}

CreateDEASection <- function(params){
  descr <- c("\\section{Differential Expression Analysis}\n",
             "Prior to the computationally intensive curve fitting steps, genes are removed that are unlikely to have a dose-dependent behavior ", 
             "based on fold-changes and p-values computed using Limma. Since genes will be subject to additional filters during the curve-fitting ", 
             "steps, it is convention to use relaxed thresholds compared to traditional differential expression analysis. However, if there is a ", 
             "strong transcriptomic response, we recommend increasing the threshold values such that less than 800 genes pass this step for best ", 
             "performance of the interactive pathway-level BMD analysis. The parameters that you chose for this section are listed below: ",
             "\\begin{itemize}",
             "\\item{Control condition: $", params[["ctrl.cond"]], "$}",
             "\\item{P-value: ", params[["pval"]], "}",
             "\\item{FDR: ", params[["fdr"]], "}",
             "\\item{log2FC: ", params[["lfc"]], "}",
             "\\end{itemize}",
             "\n\n");
  cat(descr, file=rnwFile, append=TRUE);
}

CreateCurveSection <- function(params, files){

    hist.img <- getImageName(files, "dr_histogram_");
    bar.img <- getImageName(files, "dr_barplot_");
    convertPNG2PDF(hist.img);
    convertPNG2PDF(bar.img);

    descr <- c("\\section{Curve Fitting and Gene-level BMDs}\n",
               "To calculate gene-level BMDs, up to 10 statistical models are fit to the expression of each gene. Any model ", 
               "fits with a poor fit are filtered out, and then the best fitting model is chosen based on AIC. The selected fit is ", 
               "used to compute the BMD. We recommend selecting all statistical models except for Poly3 and Poly4, which should only ", 
               "be used if you expect a non-monotonic response. These higher order polynomials should be used with caution since they ", 
               "sometimes have unpredictable behavior, especially for dose-response experiments with a log-scale dosing scheme. ",
               "Figure 5 is a density plot showing the distribution of geneBMDs with vertical lines for the omicBMDs. Figure 6 ",
               "is a bar plot showing the breakdown of best fit models. The parameters that you chose for this section are listed below: ",
               "\\begin{itemize}",
               "\\item{Selected statistical models: ", params[["models"]], "}",
               "\\item{Lack-of-fit p-value: ", params[["lof.pval"]], "}",
               "\\item{BMR factor: ", params[["bmr.fact"]], "}",
               "\\end{itemize}",
               "\n\n");
    cat(descr, file=rnwFile, append=TRUE);

    hist.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", hist.img,"}", sep=""),
                  "\\caption{", "Density plot of gene-level benchmark doses (BMDs).","}",
                  "\\end{center}", 
                  paste("\\label{",hist.img,"}", sep=""),
                  "\\end{figure}");

    cat(hist.plot, file=rnwFile, append=TRUE, sep="\n");

    bar.plot <- c( "\\begin{figure}[htp]",
                  "\\begin{center}",
                  paste("\\includegraphics[width=0.8\\textwidth]{", bar.img,"}", sep=""),
                  "\\caption{", "Frequency of statistical models among best fit curves.","}",
                  "\\end{center}", 
                  paste("\\label{",bar.img,"}", sep=""),
                  "\\end{figure}");

    cat(bar.plot, file=rnwFile, append=TRUE, sep="\n");

    if (file.exists("densityjsview.png")){
        idens.plot <- c( "\\begin{figure}[htp]",
                      "\\begin{center}",
                      paste("\\includegraphics[width=0.8\\textwidth]{", "densityjsview.png","}", sep=""),
                      "\\caption{", "Snapshot of interactive density plot view.","}",
                      "\\end{center}", 
                      paste("\\label{","densityjsview.png","}", sep=""),
                      "\\end{figure}");

        cat(idens.plot, file=rnwFile, append=TRUE, sep="\n");
    }

}


#### Report utils
CreateDataSummaryTable <- function(){
  sum.dat <- dataSet$meta.info;
  sum.dat <- cbind(sum.dat, rep(1, nrow(sum.dat)));
  colnames(sum.dat)[ncol(sum.dat)] <- "vals";
  
  require(plyr)
  sum.dat <- ddply(sum.dat,colnames(dataSet$meta.info),summarise,Samples=sum(vals));
  sum.dat <<- sum.dat;
  genetable<-c("<<echo=false, results=tex>>=",
               "FormatDataTable()",
               "@");
  cat(genetable, file=rnwFile, append=TRUE, sep="\n");
  cat("\n\n", file=rnwFile, append=TRUE);
  rm(sum.dat);
}

FormatDataTable <- function(){
  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
                               "\\endhead \n",
                               "\\hline \n",
                               "{\\footnotesize Continued on next page} \n",
                               "\\endfoot \n",
                               "\\endlastfoot \n",sep=""))
  
  print(xtable::xtable(sum.dat, caption="Summary of sample size for each treatment condition"), 
        tabular.environment = "longtable",
        include.rownames = FALSE,
        floating = FALSE,
        add.to.row = addtorow,
        table.placement="!h", 
        hline.after=c(-1),
        caption.placement="top", size="\\scriptsize");
}


convertPNG2PDF <- function(filenm){
  library(png)
  
  nm <- strsplit(filenm, "[.]")[[1]][1]
  img <- readPNG(filenm)

  pdf(paste0(nm, ".pdf"))
  grid::grid.raster(img)
  dev.off()  
  
  return(1)
}

getImageName <- function(files, pattern){

    inds <- grep(pattern, files);
    files <- files[inds];
    inds <- grep("dpi72.png", files);
    files <- files[inds]

    count <- gsub(pattern, "",files);
    count <- gsub("dpi72.png", "", count);
    count <- as.numeric(count);
    img.i <- which(count == max(count));
    file <- files[img.i];

    return(file)

}
