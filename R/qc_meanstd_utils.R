##################################################
## R script for ExpressAnalyst
## Description: functions for quality check MeanSD plot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PlotDataMeanStd <- function(fileName, densityName, dpi,format){
  dataSet <- qs:::qread(fileName);
  res <- qc.meanstd(dataSet$data.norm, densityName, dpi, format);
  return(res);
}

qc.meanstd <- function(dat, imgNm,dpi=72, format="png"){
  dpi <- as.numeric(dpi)
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  print(format)
  if(format == "svg"){
    require(gridSVG)
    png("NULL", width=8, height=6, unit="in", res=dpi); 
    plot <- meanSdPlot(dat, ranks=FALSE)
    print(plot)
    rootAttrs = c("svgmeansd")
    names(rootAttrs) = c("id");
    pcafig.svg <- gridSVG::grid.export(imgNm,addClasses=TRUE, rootAttrs = rootAttrs)
    str <- paste(capture.output(pcafig.svg$svg, file=NULL), collapse="\n");
    dev.off()
    jsonNm <- paste0(fileNm, "json");
    plotData <- ggplot_build(plot$gg)
    json.obj <- rjson::toJSON(plotData$data[[1]]);
    sink(jsonNm);
    cat(json.obj);
    sink()
  }else{
    Cairo(file=imgNm, width=8, height=6, type=format, bg="white", dpi=dpi, unit="in");
    plot <- meanSdPlot(dat, ranks=FALSE) 
    dev.off();
    str <- "NA"
  }
  return(str);
}


meanSdPlot <- function(x, ranks = TRUE, xlab = ifelse(ranks, "rank(mean)", "mean"),
                       ylab = "sd", pch, plot = TRUE, bins = 50, ...) {
  
  stopifnot(is.logical(ranks), length(ranks) == 1, !is.na(ranks))
  
  n <- nrow(x)
  if (n == 0L) {
    warning("In 'meanSdPlot': input matrix 'x' has 0 rows. There is nothing to be done.")
    return()
  }
  if (!missing(pch)) {
    warning("In 'meanSdPlot': 'pch' is ignored.")
  }
  
  px   = rowMeans(x, na.rm = TRUE)
  py   = sqrt(rowV(x, mean = px, na.rm = TRUE))
  rpx  = rank(px, na.last = FALSE, ties.method = "random")
  
  ## run median with centers at dm, 2*dm, 3*dm,... and width 2*dm
  dm        = 0.025
  midpoints = seq(dm, 1-dm, by = dm)
  within    = function(x, x1, x2) { (x >= x1) & (x <= x2) }
  mediwind  = function(mp) median(py[within(rpx/n, mp - 2*dm, mp + 2*dm)], na.rm = TRUE)
  rq.sds    = sapply(midpoints, mediwind)
  
  res = if(ranks) {
    list(rank = midpoints*n, sd = rq.sds, px = rpx, py = py)
  } else {
    list(quantile = quantile(px, probs = midpoints, na.rm = TRUE), sd = rq.sds, px = px, py = py)
  }
  
  fmt = function() function(x) format(round(x, 0), nsmall = 0L, scientific = FALSE)
  
  res$gg = ggplot(data.frame(px = res$px, py = res$py),
                  aes_string(x = "px", y = "py")) + xlab(xlab) + 
    ylab(ylab) +
    geom_hex(bins = bins, ...) +
    scale_fill_gradient(name = "count", trans = "log", labels = fmt()) + 
    geom_line(aes_string(x = "x", y = "y"),
              data = data.frame(x = res[[1]], y = res$sd), color = "red") +
    theme_bw()
  
  if (plot) print(res$gg)
  
  return(invisible(res))
}


rowV = function(x, mean, ...) {
  sqr     = function(x)  x*x
  n       = rowSums(!is.na(x))
  n[n<1]  = NA
  if(missing(mean))
    mean=rowMeans(x, ...)
  return(rowSums(sqr(x-mean), ...)/(n-1))
}
