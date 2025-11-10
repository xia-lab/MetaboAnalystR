# Get result table

  
  GetSigTableRMD<-function(mat, method, data.type){
    if(!isEmptyMatrix(mat)){ # test if empty
      col1<-rownames(mat);
      cname<-colnames(mat);
      cname<-c(GetVariableLabel(data.type), cname);
      mat<-cbind(col1, mat);
      rownames(mat)<-NULL;
      colnames(mat)<-cname;
      res <- mat
    }else{
      res <- paste("No significant features were found using the given threshold for", method)
    }
    return(res)
  }
  
  GetSigTableRMD_FC <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$fc$sig.mat, "fold change analysis", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_TT <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$tt$sig.mat, "t-tests", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_Volcano <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$volcano$sig.mat, "volcano plot", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_Anova <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$aov$sig.mat, "One-way ANOVA and post-hoc analysis", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_Corr <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$corr$cor.mat, "Pattern search using correlation analysis", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_SAM <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$sam.cmpds, "SAM", mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_EBAM <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$ebam.cmpds, "EBAM", mSetObj$dataSet$type);
  }
  
  GetAllKMClusterMembersRMD <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    clust.df = data.frame();
    rowNameVec = c();
    i = 1;
    clust.num<-max(mSetObj$analSet$kmeans$cluster);
    while(i<=clust.num){
      if(i==1){
        clust.df <- rbind(paste0(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = "; "));
      }else{
        clust.df <- rbind(clust.df,paste0(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = "; "));
      }
      rowNameVec <- c(rowNameVec, paste0("Cluster(", i, ")"));
      i = i+1;
    }
    row.names(clust.df) <- rowNameVec;
    colnames(clust.df) <-"Samples in each cluster";
    return(clust.df)
    #print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using K-means"), caption.placement="top", size="\\scriptsize");
  }
  
  GetAllSOMClusterMembersRMD <- function(mSetObj=NA){
    
    mSetObj <- .get.mSet(mSetObj);
    clust <- mSetObj$analSet$som$visual;
    xdim <- mSetObj$analSet$som$xdim;
    ydim <- mSetObj$analSet$som$ydim;
    
    clust.df = data.frame();
    rowNameVec = c();
    i = 0;
    while(i < xdim){
      j = 0;
      while(j < ydim){
        xTrue<-clust$x == i;
        yTrue<-clust$y == j;
        if(i==0 & j==0){ # bug in R, the first one need to be different
          clust.df <- rbind(paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse = "; "));
          rowNameVec <- c(paste0("Cluster(", i, ",", j,")"));
        }else{
          clust.df <- rbind(clust.df, paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse="; "));
          rowNameVec <- c(rowNameVec, paste0("Cluster(", i, ",", j,")"));
        }
        j = j+1;
      }
      i = i+1;
    }
    row.names(clust.df) <- rowNameVec;
    colnames(clust.df) <- "Samples in each cluster";
    return(clust.df)
    #print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using SOM"),caption.placement="top", size="\\scriptsize");
  }
  
  GetRFConfTableRMD <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    return(mSetObj$analSet$rf$confusion)
  }
  
  GetMapTableRMD <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    return(mSetObj$dataSet$map.table)
  }
  
  GetORATableRMD <-function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    if(substr(mSetObj$analSet$type, 0, 4) == 'mset'){
    res <- mSetObj$analSet$ora.mat;

      return(res)
      # print(xtable::xtable(mSetObj$analSet$ora.mat,align="p{5cm}|l|l|l|l|l|l", display=c("s","d","f","d","E","E","E"), caption="Result from Over Representation Analysis"),
      #       tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
    }else{
      res <- mSetObj$analSet$path.ora.mat;

      rownames(res)<-GetORA.pathNames(mSetObj);
      #print("pathoramat");
      #print(res);

      return(res)
      # print(xtable::xtable(res,align="p{5cm}|l|l|l|l||ll|l|l", display=c("s","d","f","d","E","E", "E","E", "f"),
      #                      caption="Result from Pathway Analysis"),
      #       tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
    }      
  }
  
  GetQEATableRMD<-function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    res <- mSetObj$analSet$qea.mat;
    if(substr(mSetObj$analSet$type, 0, 4) == 'mset'){
      return(res);
      # print(xtable::xtable(res,align="p{4cm}|l|l|l|l|l|l|l", display=c("s","d","d","f","f","E","E","E"),
      #                      caption="Result from Quantitative Enrichment Analysis"),
      #       tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
    }else{
      rownames(res)<- GetQEA.pathNames();
      return(res)
      # print(xtable::xtable(res,align="p{5cm}|l|l|l|l|l|l|l", display=c("s","d","d","E","E", "E","E","f"),
      #                      caption="Result from Pathway Analysis"),
      #       tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
    }
  }
  
  GetSigTable.Aov2RMD<-function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$aov2$sig.mat, "Significant features identified by advanced ANOVA", mSetObj$dataSet$type);
    #Important features identified by
  }
  
  GetSigTableRMD_ASCA <- function(mSetObj=NA, nm){
    mSetObj <- .get.mSet(mSetObj);
    if(nm == "Model.a"){
      nmLbl <- paste("main effect", mSetObj$dataSet$facA.lbl);
    }else if(nm == "Model.b"){
      nmLbl <- paste("main effect", mSetObj$dataSet$facB.lbl);
    }else{
      nmLbl <- paste("interaction effect between", mSetObj$dataSet$facA.lbl, "and",  mSetObj$dataSet$facB.lbl);
    }
    asca <- qs::qread("asca.qs");
    GetSigTableRMD(asca$sig.list[[nm]], 
                   paste("ASCA. The table shows features that are well modelled by ", nmLbl, ".", sep=""), 
                   mSetObj$dataSet$type);
  }
  
  GetSigTableRMD_MB<-function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    GetSigTableRMD(mSetObj$analSet$MB$stats, "MEBA", mSetObj$dataSet$type);
  }

  GetSigTableRMD_Limma<-function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    tbl <- GetSigTableRMD(mSetObj$analSet$cov$sig.mat, "Limma", mSetObj$dataSet$type);
    return(tbl)

  }

  SetSharingLink <- function(link){
    mSetObj <- .get.mSet(NA);
    mSetObj$dataSet$shareURLLink <- link;
    mSet <<- mSetObj;
    return(1)
  }
  
  GetSharingLink <- function(mSetObj){
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$dataSet$shareURLLink -> link;
    return(link)
  }
  

createEnrichmentTable <- function(mSetObj, type, description = "") {
  imgSet <- mSetObj$imgSet;
  if (!is.null(imgSet$enrTables[[type]])) {
    enr.res <- imgSet$enrTables[[type]]$table;
    rownames(enr.res) <- NULL;  # Clean up the table for presentation

    if (!exists("enr.res.list")) {
      enr.res.list <<- list();
    }
    enr.res.list[[type]] <- list();
    enr.res.list[[type]][["table"]] <- enr.res;
    enr.res.list[[type]][["library"]] <- makeReadable(imgSet$enrTables[[type]]$library);
    print(imgSet$enrTables[[type]]$algo);
    enr.res.list <<- enr.res.list;

    # Set the description for the table
    if (description == "") {
      descriptionText <- paste0('Table ', getTableCount(), ': Top pathways/categories from enrichment analysis using ',
                               enr.res.list[[type]][['library']], ' library.');
    } else {
      descriptionText <- paste0('Table ', getTableCount(), ': ', description);
    }

    # Check if the report format is for slides
    if (mSetObj$paramSet$report.format == "slides") {
      # Adaptation for slides
      table_code <- sprintf("create_dt(%s, caption = '')", 
                            paste0("enr.res.list[['", type, "']][['table']]"));
      # Construct slide content
      slideStr <- c(paste0("## ", descriptionText, "\n"),
                    '```{r echo=FALSE, results="asis"}',
                    table_code,
                    '```',
                    "\n---\n\n");  # Slide separator for R Markdown
      cat(slideStr, file = rmdFile, append = TRUE, sep = "\n");
    } else {
      # Existing function for non-slide formats
      table_code <- sprintf("create_dt(%s, '%s')",
                            paste0("enr.res.list[['", type, "']][['table']]"),
                            descriptionText);
      tableStr <- c('```{r echo=FALSE, results="asis"}',
                    table_code,
                    '```');
      cat(tableStr, file = rmdFile, append = TRUE, sep = "\n");
    }
  }
}



CreateKeggNetDoc <-function(mSetObj){

  if (!is.null(mSetObj$imgSet$reportSet$network_MetaboNet) && file.exists(mSetObj$imgSet$reportSet$network_MetaboNet)) {
    if(mSetObj$paramSet$report.format == "slides"){
        # Insert Network Image
        networkImageSlide <- c(
            "## KEGG Global Metabolic Network Visualization\n\n",
            "```{r figure_kegg_network, echo=FALSE, fig.cap='', out.width='100%'}",
            sprintf("knitr::include_graphics('%s')", mSetObj$imgSet$reportSet$network_MetaboNet),
            "```\n",
            "\n\n---\n\n"
        )
        cat(networkImageSlide, file = rmdFile, append = TRUE, sep = "\n")

    }else{
    descr <- c("\n\n#### Global KEGG Metabolic Network\n\n",
             "This interactive visualization allows users to interactively view their data in a global KEGG metabolic network.", "\n\n");
    cat(descr, file=rmdFile, append=TRUE);

    link <- GetSharingLink(mSetObj)
    reportLinks <- getReportLinks(link, "network_MetaboNet")
    cat(reportLinks, file=rmdFile, append=TRUE);
    cat("\n\n", file=rmdFile, append=TRUE);

    img <- c(
      paste0("```{r figure_kegg_net, echo=FALSE, fig.align='center', fig.pos='H', fig.cap='Figure ",
             getFigCount() , 
             ": KEGG global metabolic network', out.width='", getFigWidth(mSetObj), "'}"),
      sprintf("knitr::include_graphics('%s')", mSetObj$imgSet$reportSet$network_MetaboNet),
      "```",
      "\n\n")
    
    cat(img, file = rmdFile, append = TRUE, sep="\n");
  }
}
}


SetReportFormat <- function(mSetObj=NA, format="html"){
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$paramSet$report.format <- format;
    .set.mSet(mSetObj);
}

escapeLatexURL <- function(url) {
    mSetObj <- .get.mSet(mSetObj);

if(mSetObj$paramSet$report.format == "pdf"){
  url <- gsub("%", "\\%", url, fixed = TRUE)
  url <- gsub("#", "\\#", url, fixed = TRUE)
  #url <- gsub("&", "\\&", url, fixed = TRUE)
  url <- gsub("_", "\\_", url, fixed = TRUE)
  url <- gsub("{", "\\{", url, fixed = TRUE)
  url <- gsub("}", "\\}", url, fixed = TRUE)
  url <- gsub("~", "\\textasciitilde{}", url, fixed = TRUE)
  url <- gsub("\\^", "\\textasciicircum{}", url, fixed = TRUE)
  url <- gsub("\\\\", "\\textbackslash{}", url, fixed = TRUE)
}
  return(url)
}

getReportLinks <- function(link, analNavi="", imgCmd="", cmpd=""){
 format <- mSet$paramSet$report.format;
 link <- escapeLatexURL(link);

 if (format == "pdf") {
        # PDF specific link generation
        reportLinks <- "\\begin{flushright}\n"
        if (imgCmd == "") {
            if(cmpd == ""){
            reportLinks <- paste0(reportLinks, 
                '\\href{', link, '&analNavi=', analNavi, '}{Live Link}\n')
            }else{
            reportLinks <- paste0(reportLinks, 
                '\\href{', link, '&analNavi=', analNavi, '&cmpd=',cmpd ,'}{Live Link}\n')
            }
        } else {
            if(cmpd == ""){
            reportLinks <- paste0(reportLinks,
                '\\href{', link, '&analNavi=', analNavi, '}{Live Link} \n',
                '\\href{', link, '&format=pdf&imgCmd=', imgCmd, '}{PDF} \n',
                '\\href{', link, '&format=svg&imgCmd=', imgCmd, '}{SVG}\n')
            }else{
            reportLinks <- paste0(reportLinks,
                '\\href{', link, '&analNavi=', analNavi, '&cmpd=',cmpd ,'}{Live Link} \n',
                '\\href{', link, '&format=pdf&imgCmd=', imgCmd, '}{PDF} \n',
                '\\href{', link, '&format=svg&imgCmd=', imgCmd, '}{SVG}\n')
            }
        }
        reportLinks <- paste0(reportLinks, "\\end{flushright}\n")
        reportLinks <- ""; #for now;
    }else{
    if(imgCmd == ""){
       if(cmpd == ""){
        reportLinks <- paste0(
                '<div style="text-align: right;">',
                '<a href="', link, '&analNavi=', analNavi,'" target="_top">Live Link</a> ',
                '</div>');
       }else{
        reportLinks <- paste0(
                '<div style="text-align: right;">',
                '<a href="', link, '&analNavi=', analNavi,'&cmpd=',cmpd,'" target="_top">Live Link</a> ',
                '</div>');
       }
    }else{
       if(cmpd == ""){
            reportLinks <- paste0(
                '<div style="text-align: right;">',
                '<a href="', link, '&analNavi=', analNavi,'" target="_top">Live Link</a> ',
                '<a href="', link, '&format=pdf&imgCmd=', imgCmd,'" target="_blank">PDF</a> ',
                '<a href="', link, '&format=svg&imgCmd=', imgCmd,'" target="_blank">SVG</a>',
                '</div>');
            
        }else{
            reportLinks <- paste0(
                '<div style="text-align: right;">',
                '<a href="', link, '&analNavi=', analNavi,'&cmpd=',cmpd,'" target="_top">Live Link</a> ',
                '<a href="', link, '&format=pdf&imgCmd=', imgCmd,'" target="_blank">PDF</a> ',
                '<a href="', link, '&format=svg&imgCmd=', imgCmd,'" target="_blank">SVG</a>',
                '</div>');
            }
        }
    }
    return(reportLinks);
    #cat(reportLinks, file=rmdFile, append=TRUE, sep="\n")
}

getReportMS2MirrorLinks <- function(link, feature ="", moduleType="ms2"){
#print(paste0("==== =link     ==== ", link))
#print(paste0("=====feature   ==== ", feature))
#print(paste0("=====moduleType==== ", moduleType))
    link <- escapeLatexURL(link);
    format <- "svg";
    reportLinks <- "";
    if(moduleType=="ms2"){
        reportLinks <- paste0(link, "&format=svg&imgCmd=plotms2Mirror__", feature)
    } else {
        reportLinks <- paste0(link, "&format=svg&imgCmd=plotrawms2Mirror__", feature)
    }
    return(reportLinks)
#print(paste0("=====reportLinks==== ", reportLinks))
}

addReportLinks <- function(link, analNavi="", imgCmd=""){
    linkStr <- getReportLinks(link, analNavi, imgCmd);
    cat(linkStr, file=rmdFile, append=TRUE, sep="\n")
}


addDownloadLink <- function(link, fileName){ 
    reportLinks <- getDownloadLink(link, fileName);
    cat(reportLinks, file=rmdFile, append=TRUE, sep="\n");
}

getDownloadLink<- function(link, fileName){
    link <- escapeLatexURL(link);
 fileName_escaped <- escapeLatexURL(fileName);

    print(paste("download link", link));
    if(mSet$paramSet$report.format == "pdf"){    
    reportLinks <- paste0('\\href{', link, '&download=', fileName, '}{', fileName_escaped, '}');
    }else{    
    reportLinks <- paste0('<a href="', link, '&download=', fileName, '" target="_blank">', fileName_escaped, '</a>');
    }
    return(reportLinks);
}

addPageBegin <- function(){
    if (mSet$paramSet$report.format == "pdf") {
        cat("\\newpage", file=rmdFile, append=TRUE, sep="\n")
    }
}

