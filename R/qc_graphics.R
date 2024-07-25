##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PlotDataBox <- function(fileName, boxplotName, dpi, format){
  dataSet <- readDataset(fileName);
  qc.boxplot(as.matrix(dataSet$data.norm), boxplotName, dpi, format, F);
  return("NA");
}

qc.boxplot <- function(dat, imgNm, dpi=72, format="png", interactive=F){
  dpi <- as.numeric(dpi)
  require('ggplot2')
  require('lattice');
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");  subgene <- 10000;

  if(class(dat)[1] == "data.frame"){
    dat <- as.matrix(dat);
  }

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


    str <- "NA"

  if(interactive){
    library(plotly);
        m <- list(
                l = 50,
                r = 50,
                b = 20,
                t = 20,
                pad = 0.5
            )
    if(length(dataSet$meta.info) == 2){
    w=1000;
    }else{
    w=800;
    }
    ggp_build <- layout(ggplotly(bp), autosize = FALSE, width = w, height = 600, margin = m)
    return(ggp_build);
  }else{
  imgSet <- readSet(imgSet, "imgSet");
  if(grepl("norm", imgNm)){
    imgSet$qc_norm_boxplot <- imgNm;
  }else{
    imgSet$qc_boxplot <- imgNm;
  }
  saveSet(imgSet);
  Cairo(file=imgNm, width=600*dpi/72, height=height*dpi/72, unit="px",dpi=dpi, type=format, bg="white");
  print(bp);
  dev.off();
  return("NA")
  }
}


PlotDataDensity <- function(fileName, imgNm, dpi,format){
  dataSet <- readDataset(fileName);
  res <- qc.density(dataSet, imgNm, dpi, format, FALSE);
  return(res);
}

qc.density<- function(dataSet, imgNm="abc", dpi=72, format, interactive){
  require("ggplot2")
  dat <- dataSet$data.norm
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  dpi <- as.numeric(dpi)
  
  df <- data.frame(dataSet$data.norm, stringsAsFactors = FALSE)
  df <- stack(df)
  sampleNms <- gsub("-", ".", colnames(dataSet$data.norm))
  if(length(dataSet$meta.info) == 2){

    Factor1 <- as.vector(dataSet$meta.info[,1])
    factorNm1 <- colnames(dataSet$meta.info)[1]
    conv <- data.frame(ind=sampleNms, Factor1=Factor1)
    colnames(conv) <- c("ind", factorNm1);
    df1 <- merge(df, conv, by="ind")
    Factor2 <- as.vector(dataSet$meta.info[,2])
    factorNm2 <- colnames(dataSet$meta.info)[2]
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
    Conditions= as.character(dataSet$meta.info[,1]);
    conv <- data.frame(ind=sampleNms, Conditions=Conditions)
    df1 <- merge(df, conv, by="ind")

    g = ggplot(df1, aes(x=values)) + 
        geom_line(aes(color=Conditions, group=ind), stat="density", alpha=0.6) +
        theme_bw()

    width <- 8
    height <- 6
  }

  if(interactive){
    library(plotly);
        m <- list(
                l = 50,
                r = 50,
                b = 20,
                t = 20,
                pad = 0.5
            )
    if(length(dataSet$meta.info) == 2){
    w=1000;
    }else{
    w=800;
    }
    ggp_build <- layout(ggplotly(g), autosize = FALSE, width = w, height = 600, margin = m)
    return(ggp_build);
  }else{
    imgSet <- readSet(imgSet, "imgSet");
    imgSet$qc.density_norm <- imgNm;
    saveSet(imgSet);
    Cairo(file=imgNm, width=width, height=height, type=format, bg="white", dpi=dpi, unit="in");
    print(g)
    dev.off();
    return("NA")
  }
}


PlotLibSizeView<-function(fileName, imgNm,dpi=72, format="png"){
  dataSet <- readDataset(fileName);
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  dpi <- as.numeric(dpi)
  data_bef<-data.matrix(dataSet$data.anot);
  
  smpl.sums <- colSums(data_bef);
  
  require("ggplot2")
  data_bef<-data.matrix(dataSet$data.anot);
  smpl.sums <- colSums(data_bef);
  names(smpl.sums) <- colnames(data_bef);
  sampleNms <- names(smpl.sums)
  df <- data.frame(count=smpl.sums,ind=colnames(data_bef))
  
  if(length(dataSet$meta.info) == 2){
    Factor1 <- as.vector(dataSet$meta.info[,1])
    factor1Nm <- colnames(dataSet$meta.info)[1]
    conv <- data.frame(ind=sampleNms, Factor1=Factor1)
    colnames(conv) <- c("ind", factor1Nm)
    df1 <- merge(df, conv, by="ind")
    Factor2 <- as.vector(dataSet$meta.info[,2])
    factor2Nm <- colnames(dataSet$meta.info)[2]
    conv <- data.frame(ind=sampleNms, Factor2=Factor2)
    colnames(conv) <- c("ind", factor2Nm)
    df1 <- merge(df1, conv, by="ind")
    df2 <- reshape::melt(df1, measure.vars=c(factor1Nm,factor2Nm))
    colnames(df2)[4] <- "Conditions"
    if(length(df2$ind)>20){

      g <- ggplot(df2, aes(x = Conditions, y = count, fill=Conditions, label= ind))+
        geom_dotplot(binaxis = 'y', stackdir = 'center',position = position_dodge(), dotsize=0.7) + 
        geom_text() + 
        ylab("Sum") + 
        facet_grid(. ~ variable) +
        theme_bw()

      plotData <- ggplot_build(g)
      g$layers[[2]] = NULL;
    }else{

      g <- ggplot(df2, aes(x = Conditions, y = count, fill=Conditions, label=ind))+
        geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(), dotsize=0.7) + 
        geom_text_repel(force=5) + 
        ylab("Sum") + 
        facet_grid(. ~ variable) +
        theme_bw()

      plotData <- ggplot_build(g)
    }
    width <- 12
    height <- 6
    
  }else{
    Conditions= as.character(dataSet$meta.info[,1]);
    conv <- data.frame(ind=sampleNms, Conditions=Conditions)
    df1 <- merge(df, conv, by="ind")
    if(length(df1$ind)>20){
      g <- ggplot(df1, aes(x = Conditions, y = count, fill=Conditions, label= ind))+
        geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(), dotsize=0.7) + 
        xlab("Sum") + 
        geom_text() +
        theme_bw()

      plotData <- ggplot_build(g)
      g$layers[[2]] = NULL;
    }else{

      g <- ggplot(df1, aes(x = Conditions, y = count, label=ind, fill=Conditions))+
        geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(), dotsize=0.7) + 
        geom_text_repel(force=5) + 
        xlab("Sum") +
        theme_bw()

      plotData <- ggplot_build(g)
      
    }
    width <- 8
    height <- 6
    
  }
  
 
  Cairo(file=imgNm, width=width, height=height, type=format, bg="white", unit="in", dpi=dpi);
  print(g);
  dev.off();
  str <- "NA"

  imgSet <- readSet(imgSet, "imgSet");
  imgSet$libsize <- imgNm;
  saveSet(imgSet);

  return(str);
}

PlotDataMeanStd <- function(fileName, densityName, dpi,format){
  dataSet <- readDataset(fileName);
  res <- qc.meanstd(dataSet$data.norm, densityName, dpi, format);
  return(res);
}

qc.meanstd <- function(dat, imgNm,dpi=72, format="png"){
  dpi <- as.numeric(dpi)
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  #print(format)
  Cairo(file=imgNm, width=8, height=6, type=format, bg="white", dpi=dpi, unit="in");
  plot <- meanSdPlot(dat, ranks=FALSE) 
  dev.off();
  str <- "NA"
  
  imgSet <- readSet(imgSet, "imgSet");
  imgSet$qc.meanstd <- imgNm;
  saveSet(imgSet);

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


PlotDataPCA <- function(fileName, pcaName, dpi, format){
  dataSet <- readDataset(fileName);
  qc.pcaplot(dataSet, dataSet$data.norm, pcaName, dpi, format, F);
  return("NA");
}

qc.pcaplot <- function(dataSet, x, imgNm, dpi=72, format="png", interactive=F){
  dpi <- as.numeric(dpi)
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="")
  imgNm <- paste0(fileNm, format, sep="")
  
  require('lattice')
  require('ggplot2')
  require('reshape')
  require('see')
  require('ggrepel')
  
  pca <- prcomp(t(na.omit(x)))
  imp.pca <- summary(pca)$importance
  xlabel <- paste0("PC1"," (", 100 * round(imp.pca[2,][1], 3), "%)")
  ylabel <- paste0("PC2"," (", 100 * round(imp.pca[2,][2], 3), "%)")
  pca.res <- as.data.frame(pca$x)
  pca.res <- pca.res[, c(1, 2)]
  
  # Increase xlim and ylim for text label
  xlim <- GetExtendRange(pca.res$PC1)
  ylim <- GetExtendRange(pca.res$PC2)
  
  if ("newcolumn" %in% colnames(dataSet$meta.info)) {
    dataSet$meta.info <- data.frame(dataSet$meta.info[, -which(colnames(dataSet$meta.info) == "newcolumn")])
  }
  
  if (!(all(rownames(pca.res) == rownames(dataSet$meta.info)))) {
    pca.res <- pca.res[match(rownames(dataSet$meta.info), rownames(pca.res)), ]
  }
  
  if (length(dataSet$meta.info) == 2) {
    Factor1 <- as.vector(dataSet$meta.info[, 1])
    factorNm1 <- colnames(dataSet$meta.info)[1]
    pca.res[, factorNm1] <- Factor1
    Factor2 <- as.vector(dataSet$meta.info[, 2])
    factorNm2 <- colnames(dataSet$meta.info)[2]
    pca.res[, factorNm2] <- Factor2
    pca.rest <- reshape::melt(pca.res, measure.vars = c(factorNm1, factorNm2))
    colnames(pca.rest)[4] <- "Conditions"
    pca.rest$names <- rep(rownames(pca.res), times = 2)
    
    # Calculate group centroids
    centroids <- aggregate(. ~ Conditions, data = pca.rest[, c("PC1", "PC2", "Conditions")], mean)
    # Merge centroids back to the pca.rest dataframe
    pca.rest <- merge(pca.rest, centroids, by = "Conditions", suffixes = c("", "_centroid"))
    # Calculate the distance to the centroid
    pca.rest$distance <- sqrt((pca.rest$PC1 - pca.rest$PC1_centroid)^2 + (pca.rest$PC2 - pca.rest$PC2_centroid)^2)
    # Identify outliers based on variance threshold (20% here)
    threshold <- 0.2 * mean(pca.rest$distance, na.rm = TRUE)
    pca.rest$outlier <- pca.rest$distance > threshold
    
    if (length(pca.rest$names) > 20) {
      pcafig <- ggplot(pca.rest, aes(x = PC1, y = PC2, color = Conditions, label = names)) +
        geom_point(size = 3, alpha = 0.5) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) + 
        facet_grid(. ~ variable) +
        theme_bw() +
        scale_color_okabeito() +
        scale_fill_okabeito()
    } else {
      pcafig <- ggplot(pca.rest, aes(x = PC1, y = PC2, color = Conditions, label = names)) +
        geom_point(size = 4) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) + 
        geom_text_repel(force = 1.5) + 
        facet_grid(. ~ variable) + theme_bw() +
        scale_color_okabeito() +
        scale_fill_okabeito()
    }
    width <- 12
    height <- 6
  } else {
    Factor <- dataSet$meta.info[, 1]
    pca.res$Conditions <- Factor
    pca.res$names <- rownames(pca.res)
    
    # Calculate group centroids
    centroids <- aggregate(. ~ Conditions, data = pca.res[, c("PC1", "PC2", "Conditions")], mean)
    # Merge centroids back to the pca.res dataframe
    pca.res <- merge(pca.res, centroids, by = "Conditions", suffixes = c("", "_centroid"))
    # Calculate the distance to the centroid
    pca.res$distance <- sqrt((pca.res$PC1 - pca.res$PC1_centroid)^2 + (pca.res$PC2 - pca.res$PC2_centroid)^2)
    # Identify outliers based on variance threshold (20% here)
    threshold <- 0.2 * mean(pca.res$distance, na.rm = TRUE)
    pca.res$outlier <- pca.res$distance > threshold
    
    if (length(rownames(pca.res)) > 20) {
      pcafig <- ggplot(pca.res, aes(x = PC1, y = PC2, color = Conditions)) +
        geom_point(size = 3, alpha = 0.5) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) +
        theme_bw()
    } else {
      pcafig <- ggplot(pca.res, aes(x = PC1, y = PC2, color = Conditions, label = names)) +
        geom_point(size = 4) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) +
        geom_text_repel(force = 1.5) +
        theme_bw()
    }
    width <- 10
    height <- 6
    if(grepl("norm", imgNm)){
        if (paramSet$oneDataAnalType == "dose") {
          pal <- colorRampPalette(c("#2196F3", "#DE690D"))
          col.pal <- pal(length(unique(pca.res$Conditions)))
          pcafig <- pcafig + scale_fill_manual(values = col.pal) + scale_color_manual(values = col.pal)
        } else {
          pcafig <- pcafig + scale_fill_okabeito() + scale_color_okabeito()
        }
    }
  }
  

  # Print the outliers
  outliers <- pca.res[pca.res$outlier, "names"]
  if(!is.null(outliers)){
    paramSet <- readSet(paramSet, "paramSet")
    paramSet$pca.outliers <- outliers;
  }else{
    paramSet$pca.outliers <- c("NA");
  }
  saveSet(paramSet);
  
  if (interactive) {
    library(plotly)
    m <- list(
      l = 50,
      r = 50,
      b = 20,
      t = 20,
      pad = 0.5
    )
    if (length(dataSet$meta.info) == 2) {
      w = 1000
    } else {
      w = 800
    }
    ggp_build <- layout(ggplotly(pcafig), autosize = FALSE, width = w, height = 600, margin = m)
    return(ggp_build)
  } else {
    imgSet <- readSet(imgSet, "imgSet")
    if (grepl("norm", imgNm)) {
      imgSet$qc_norm_pca <- imgNm
    } else {
      imgSet$qc_pca <- imgNm
    }
    saveSet(imgSet)
    Cairo(file = imgNm, width = width, height = height, type = format, bg = "white", unit = "in", dpi = dpi)
    print(pcafig)
    dev.off()
    return("NA")
  }
}

GetPcaOutliers <- function(){
    paramSet <- readSet(paramSet, "paramSet")
    return(paramSet$pca.outliers);
}

PlotDataNcov5 <- function(fileName, imgName, dpi, format){
  dataSet <- readDataset(fileName);
  qc.ncov5(dataSet, dataSet$data.anot, imgName, dpi, format, F);
  return("NA");
}

qc.ncov5 <- function(dataSet, x, imgNm="NCov5_plot", dpi=72, format="png", interactive=FALSE) {
  require("ggplot2")
  require("Cairo")
  
  # Ensure dpi is a positive number
  dpi <- as.numeric(dpi)
  if (dpi <= 0) {
    stop("DPI must be a positive number.")
  }
  
  # Calculate NCov5 (HighCoverageGeneCount) for each sample
  HighCoverageGeneCount <- colSums(x > 5)
  
  df <- data.frame(Sample = names(HighCoverageGeneCount), HighCoverageGeneCount = as.numeric(HighCoverageGeneCount), stringsAsFactors = FALSE)
  
  # Check for non-finite values and remove them
  df <- df[is.finite(df$HighCoverageGeneCount),]
  
  # Calculate IQR and identify outliers
  Q1 <- quantile(df$HighCoverageGeneCount, 0.25)
  Q3 <- quantile(df$HighCoverageGeneCount, 0.75)
  IQR_value <- IQR(df$HighCoverageGeneCount)
  lower_bound <- Q1 - 3 * IQR_value
  upper_bound <- Q3 + 3 * IQR_value
  
  df$outlier <- ifelse(df$HighCoverageGeneCount < lower_bound | df$HighCoverageGeneCount > upper_bound, "Outlier", "Normal")
  
  # Create plot
  g <- ggplot(df, aes(x = Sample, y = HighCoverageGeneCount, group = Sample, fill = outlier)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Normal" = "steelblue", "Outlier" = "red")) +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "blue") +
    theme_minimal() +
    labs(title = "NCov5 Scores for Each Sample",
         x = "Sample",
         y = "Number of Genes with > 5 Uniquely Mapped Reads") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  
  width <- 12
  height <- 6
  
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="")
  imgNm <- paste0(fileNm, format, sep="")
  
  if (interactive) {
    require("plotly")
    m <- list(
      l = 50,
      r = 50,
      b = 20,
      t = 20,
      pad = 0.5
    )
    w <- 1000
    
    ggp_build <- layout(ggplotly(g), autosize = FALSE, width = w, height = 600, margin = m)
    return(ggp_build)
  } else {
    Cairo(file = imgNm, width = width, height = height, type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
}
qc.ncov5 <- function(dataSet, x, imgNm="NCov5_plot", dpi=72, format="png", interactive=FALSE) {
  require("ggplot2")
  require("Cairo")
  
  # Ensure dpi is a positive number
  dpi <- as.numeric(dpi)
  if (dpi <= 0) {
    stop("DPI must be a positive number.")
  }
  
  # Calculate NCov5 (HighCoverageGeneCount) for each sample
  HighCoverageGeneCount <- colSums(x > 5)
  
  df <- data.frame(Sample = names(HighCoverageGeneCount), HighCoverageGeneCount = as.numeric(HighCoverageGeneCount), stringsAsFactors = FALSE)
  
  # Check for non-finite values and remove them
  df <- df[is.finite(df$HighCoverageGeneCount),]
  
  # Calculate IQR and identify outliers
  Q1 <- quantile(df$HighCoverageGeneCount, 0.25)
  Q3 <- quantile(df$HighCoverageGeneCount, 0.75)
  IQR_value <- IQR(df$HighCoverageGeneCount)
  lower_bound <- Q1 - 3 * IQR_value
  upper_bound <- Q3 + 3 * IQR_value
  
  df$outlier <- ifelse(df$HighCoverageGeneCount < lower_bound | df$HighCoverageGeneCount > upper_bound, "Outlier", "Normal")
  
  # Create plot
  g <- ggplot(df, aes(x = Sample, y = HighCoverageGeneCount, group = Sample, fill = outlier)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Normal" = "steelblue", "Outlier" = "red")) +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "blue") +
    theme_minimal() +
    labs(x = "Sample",
         y = "Number of Genes with > 5 Uniquely Mapped Reads") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  
  width <- 12
  height <- 6
  
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="")
  imgNm <- paste0(fileNm, format, sep="")
  
  if (interactive) {
    require("plotly")
    m <- list(
      l = 50,
      r = 50,
      b = 20,
      t = 20,
      pad = 0.5
    )
    w <- 1000
    
    ggp_build <- layout(ggplotly(g), autosize = FALSE, width = w, height = 600, margin = m)
    return(ggp_build)
  } else {
    Cairo(file = imgNm, width = width, height = height, type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
}

