##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PlotDataDensity <- function(fileName, imgNm, dpi,format, factor){
  dataSet <- qs:::qread(fileName);
  res <- qc.density(dataSet, imgNm, dpi, format, factor);
  return(res);
}

qc.density<- function(dataSet, imgNm="abc", dpi=72, format, factor){
  library("ggplot2")
  dat <- dataSet$data.norm
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  dpi <- as.numeric(dpi)
  
  df <- data.frame(dataSet$data.norm, stringsAsFactors = FALSE)
  df <- stack(df)
  sampleNms <- gsub("-", ".", colnames(dataSet$data.norm))
  if(length(dataSet$meta) == 2){

    Factor1 <- as.vector(dataSet$meta[,1])
    factorNm1 <- colnames(dataSet$meta)[1]
    conv <- data.frame(ind=sampleNms, Factor1=Factor1)
    colnames(conv) <- c("ind", factorNm1);
    df1 <- merge(df, conv, by="ind")
    Factor2 <- as.vector(dataSet$meta[,2])
    factorNm2 <- colnames(dataSet$meta)[2]
    conv <- data.frame(ind=sampleNms, Factor2=Factor2)
    colnames(conv) <- c("ind", factorNm2);
    df1 <- merge(df1, conv, by="ind")
    df2 <- reshape::melt(df1, measure.vars=c(factorNm1,factorNm2))
    colnames(df2)[4] <- "Conditions"

    g = ggplot(df2, aes(x=values)) + 
        geom_line(aes(color=Conditions, group=ind), stat="density", alpha=0.6) + 
        facet_grid(. ~ variable) +
        theme_bw()

    width <- 12
    height <- 6
  }else{
    Conditions= as.character(dataSet$meta[,1]);
    conv <- data.frame(ind=sampleNms, Conditions=Conditions)
    df1 <- merge(df, conv, by="ind")

    g = ggplot(df1, aes(x=values)) + 
        geom_line(aes(color=Conditions, group=ind), stat="density", alpha=0.6) +
        theme_bw()

    width <- 8
    height <- 6
  }
  
  if(format == "svg"){
    require(gridSVG)
    png("NULL", unit="in", width=width, height=height, res=dpi); 
    print(g)
    rootAttrs = c("svgdensity")
    names(rootAttrs) = c("id");
    pcafig.svg <- gridSVG::grid.export(imgNm,addClasses=TRUE, rootAttrs = rootAttrs)
    str <- paste(capture.output(pcafig.svg$svg, file=NULL), collapse="\n");
    dev.off();
    jsonNm <- paste0(fileNm, "json");
    plotData <- ggplot_build(g)
    json.obj <- rjson::toJSON(plotData$data[[1]]);
    sink(jsonNm);
    cat(json.obj);
    sink()
  }else{
    Cairo(file=imgNm, width=width, height=height, type=format, bg="white", dpi=dpi, unit="in");
    print(g)
    dev.off();
    str <- "NA"
  }
  
  return(str);
}
