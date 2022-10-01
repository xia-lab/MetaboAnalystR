##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PlotDataBox <- function(fileName, boxplotName, dpi, format){
  dataSet <- qs:::qread(fileName);
  res <- qc.boxplot(dataSet$data.norm, boxplotName, dpi, format);
  return(res);
}

qc.boxplot <- function(dat, imgNm, dpi=72, format="png"){
  dpi <- as.numeric(dpi)
  library('ggplot2')
  library('lattice');
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");  subgene <- 10000;
  if (nrow(dat)>subgene) {
    set.seed(28051968);
    sg  <- sample(nrow(dat), subgene);
    Mss <- dat[sg,,drop=FALSE];
  } else {
    Mss <- dat;
  }
  
  subsmpl=100;
  if (ncol(Mss)>subsmpl) {
    set.seed(28051968);
    ss  <- sample(ncol(Mss), subsmpl)
    Mss <- Mss[,ss,drop=FALSE]
  } else {
    Mss <- Mss
  }
  sample_id <- rep(seq_len(ncol(Mss)), each = nrow(Mss));
  values  <- as.numeric(Mss)
  
  df <- cbind(values, sample_id)
  
  df <- data.frame(df)
  df$sample_id <- factor(df$sample_id)
  xlower <- unname(quantile(df$values, probs = c(0.01, 0.99), na.rm=TRUE)[1])
  xupper <- unname(quantile(df$values, probs = c(0.01, 0.99), na.rm=TRUE)[2])
  height <- length(unique(df$sample_id)) *20;
  if(height<450){
    height <- 450
  }
  bp <- ggplot(df, aes(sample_id, values)) +
    ylab("Values") + 
    xlab("Samples") + 
    scale_x_discrete(labels=colnames(dat)) + 
    ylim(xlower, xupper) + 
    stat_boxplot(geom = "errorbar", color="black") + 
    geom_boxplot(outlier.size=0.5, outlier.alpha=0.4) +
    theme_bw()
  bp <- bp + coord_flip();
  
  if(format == "svg"){
    require(gridSVG)
    png("NULL", width=600*dpi/72, height=height*dpi/72, unit="px", res=dpi); 
    print(bp)
    rootAttrs = c("svgboxplot")
    names(rootAttrs) = c("id");
    fig.svg <- gridSVG::grid.export(imgNm,addClasses=TRUE, uniqueNames=TRUE, annotate=T, rootAttrs=rootAttrs)
    str <- paste(capture.output(fig.svg$svg, file=NULL), collapse="\n");
    dev.off();
    jsonNm <- paste0(fileNm, "json");
    plotData <- ggplot_build(bp)
    plotData$data[[1]] <- plotData$data[[1]][, -6]; #remove outlier column
    json.obj <- rjson::toJSON(plotData$data[[1]]);
    sink(jsonNm);
    cat(json.obj);
    sink();
  }else{
    Cairo(file=imgNm, width=600*dpi/72, height=height*dpi/72, unit="px",dpi=dpi, type=format, bg="white");
    print(bp);
    dev.off();
    str <- "NA"
  }
  
  return(str)
}
