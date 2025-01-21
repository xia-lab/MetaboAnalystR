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

    Factor1 <- dataSet$meta.info[,1]
    factorNm1 <- colnames(dataSet$meta.info)[1]
    conv <- data.frame(ind=sampleNms, Factor1=Factor1)
    colnames(conv) <- c("ind", factorNm1);
    df1 <- merge(df, conv, by="ind")
    Factor2 <- dataSet$meta.info[,2]
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
    Conditions <- dataSet$meta.info[,1];
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
  library("ggrepel");
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

PlotDataMeanStd <- function(fileName, imgName, dpi,format){
  dataSet <- readDataset(fileName);
  if(grepl("_norm", imgName)){
    res <- qc.meanstd(dataSet$data.norm, imgName, dpi, format);
  }else{
    res <- qc.meanstd(dataSet$data.anot, imgName, dpi, format);
  }
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
                  aes_string(x = "px", y = "py")) + 
    xlab(xlab) + ylab(ylab) +
    geom_hex(bins = bins, ...) +
    scale_fill_gradient(name = "count", trans = "log", labels = fmt()) + 
    geom_line(aes_string(x = "x", y = "y"),
              data = data.frame(x = res[[1]], y = res$sd), color = "red") +
    theme_bw();
  
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



PlotDataPCA <- function(fileName, imgName, dpi, format){
  dataSet <- readDataset(fileName);
  if(grepl("_norm", imgName)){
    qc.pcaplot(dataSet, dataSet$data.norm, imgName, dpi, format, F);
  }else{
    qc.pcaplot(dataSet, dataSet$data.anot, imgName, dpi, format, F);
  }
  return("NA");
}

qc.pcaplot <- function(dataSet, x, imgNm, dpi=72, format="png", interactive=FALSE) {
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
        scale_fill_okabeito() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)
        )
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
        scale_fill_okabeito() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)
        )
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
        theme_bw() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)
        )
    } else {
      pcafig <- ggplot(pca.res, aes(x = PC1, y = PC2, color = Conditions, label = names)) +
        geom_point(size = 4) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) +
        geom_text_repel(force = 1.5) +
        theme_bw() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)
        )
    }
    width <- 8
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

  permanova_results <- ComputePERMANOVA(pca.res$PC1, pca.res$PC2, dataSet$meta.info[, 1], 999);
  analSet <- readSet(analSet, "analSet");
  analSet$permanova.res <-permanova_results;
  saveSet(analSet);
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
  if(grepl("_norm", imgName)){
    qc.ncov5(dataSet, dataSet$data.norm, imgName, dpi, format, F);
  }else{
    qc.ncov5(dataSet, dataSet$data.anot, imgName, dpi, format, F);
  }
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
     scale_fill_manual(name = "Sample Status", values = c("Normal" = "grey", "Outlier" = "red")) +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "blue") +
    theme_minimal() +
    labs( x = "Sample",
         y = "Number of Genes with > 5 Uniquely Mapped Reads") +
    theme(
      text = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1) # Rotate and increase size of x-axis labels
    )
  
  width <- 8
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

PlotDataNsig <- function(fileName, imgName, dpi, format){
  dataSet <- readDataset(fileName);
  if(grepl("_norm", imgName)){
    qc.nsig(dataSet, dataSet$data.norm, imgName, dpi, format, F);
  }else{
    qc.nsig(dataSet, dataSet$data.anot, imgName, dpi, format, F);
  }
  return("NA");
}

qc.nsig <- function(dataSet, x, imgNm="NSig80_plot", dpi=72, format="png", interactive=FALSE) {
  require("ggplot2")
  require("Cairo")
  
  # Ensure dpi is a positive number
  dpi <- as.numeric(dpi)
  if (dpi <= 0) {
    stop("DPI must be a positive number.")
  }
  
  # Calculate NSig80 for each sample
  NSig80 <- apply(x, 2, function(col) sum(cumsum(sort(col, decreasing = TRUE)) <= 0.8 * sum(col)))
  
  df <- data.frame(Sample = names(NSig80), NSig80 = as.numeric(NSig80), stringsAsFactors = FALSE)
  
  # Check for non-finite values and remove them
  df <- df[is.finite(df$NSig80),]
  
  # Calculate IQR and identify outliers
  Q1 <- quantile(df$NSig80, 0.25)
  Q3 <- quantile(df$NSig80, 0.75)
  IQR_value <- IQR(df$NSig80)
  lower_bound <- Q1 - 3 * IQR_value
  upper_bound <- Q3 + 3 * IQR_value
  
  df$outlier <- ifelse(df$NSig80 < lower_bound | df$NSig80 > upper_bound, "Outlier", "Normal")
  
  # Create plot
  g <- ggplot(df, aes(x = Sample, y = NSig80, group = Sample, fill = outlier)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Sample Status", values = c("Normal" = "grey", "Outlier" = "red")) +
    geom_hline(yintercept = lower_bound, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = upper_bound, linetype = "dashed", color = "blue") +
    theme_minimal() +
    labs(x = "Sample",
         y = "NSig80 (Number of Genes Capturing 80% of the Signal)") +
    theme(
      text = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for better readability
    )
  
  width <- 8
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


PlotDataDendrogram <- function(fileName, imgName,threshold, dpi, format){
  dataSet <- readDataset(fileName);

  if(grepl("_norm", imgName)){
    qc.dendrogram(dataSet, dataSet$data.norm, threshold, imgName, dpi, format, F);
  }else{
    qc.dendrogram(dataSet, dataSet$data.anot, threshold, imgName, dpi, format, F);
  }
  return("NA");
}

qc.dendrogram <- function(dataSet, x, threshold = 0.1, imgNm = "Dendrogram_plot", dpi = 72, format = "png", interactive = FALSE) {
  require("Cairo")
  
  # Ensure dpi is a positive number
  dpi <- as.numeric(dpi)
  if (dpi <= 0) {
    stop("DPI must be a positive number.")
  }
  
  # Calculate Spearman's rank correlation matrix
  spearman_corr <- cor(x, method = "spearman", use = "pairwise.complete.obs")
  
  # Convert correlation matrix to distance matrix (1 - correlation)
  distance_matrix <- as.dist(1 - spearman_corr)
  
  # Perform hierarchical clustering
  hc <- hclust(distance_matrix, method = "average")
  
  # Identify samples with a maximum distance greater than the threshold
  distance_matrix_full <- as.matrix(distance_matrix)
  max_distances <- apply(distance_matrix_full, 1, max)
  samples_to_highlight <- names(max_distances[max_distances > threshold])
  
  # Create a dendrogram object
  dend <- as.dendrogram(hc)
  
  # Determine the labels' colors based on outlier status
  label_colors <- ifelse(labels(dend) %in% samples_to_highlight, "red", "black")
  
  # Plotting parameters
  width <- 8
  height <- 6
  
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep = "")
  imgNm <- paste0(fileNm, format, sep = "")
  
  if (interactive) {
    stop("Interactive mode is not supported with base R plotting.")
  } else {
    Cairo(file = imgNm, width = width, height = height, type = format, bg = "white", dpi = dpi, unit = "in")
    
    # Plot the dendrogram with base R
    plot(dend, main = "Dendrogram", ylab = "Height", xlab = "", cex.axis = 0.8, cex.lab = 1.2)
    
    # Add colors to the labels
    labels <- labels(dend)
    n <- length(labels)
    for (i in 1:n) {
      x <- i
      y <- -0.1 # Positioning below the axis (adjust as necessary)
      text(x, y, labels[i], srt = 90, adj = 1, xpd = NA, col = label_colors[i], cex = 0.8)
    }
    
    dev.off()
    return("NA")
  }
}

SummarizeQC <- function(fileName, imgNameBase, dpi = 72, format = "png", threshold = 0.1) {
  dataSet <- readDataset(fileName)
  
  summary_df <- data.frame(Sample = character(), 
                           HighCoverageGeneCount = numeric(), 
                           NSig80 = numeric(), 
                           Gini = numeric(), 
                           Dendrogram_Distance = numeric(),
                           Outlier_HighCoverageGeneCount = numeric(),
                           Outlier_NSig80 = numeric(),
                           Outlier_Gini = numeric(),
                           Outlier_Dendrogram = numeric(),
                           stringsAsFactors = FALSE)
  
  if (grepl("_norm", imgNameBase)) {
    data <- dataSet$data.norm
  } else {
    data <- dataSet$data.anot
  }
  
  HighCoverageGeneCount <- colSums(data > 5)
  ncov5_df <- data.frame(Sample = names(HighCoverageGeneCount), 
                         HighCoverageGeneCount = as.numeric(HighCoverageGeneCount), 
                         stringsAsFactors = FALSE)
  
  NSig80 <- apply(data, 2, function(col) sum(cumsum(sort(col, decreasing = TRUE)) <= 0.8 * sum(col)))
  nsig_df <- data.frame(Sample = names(NSig80), NSig80 = as.numeric(NSig80), stringsAsFactors = FALSE)
  
  gini_scores <- apply(data, 2, calculate_gini)
  gini_df <- data.frame(Sample = colnames(data), Gini = gini_scores, stringsAsFactors = FALSE)
  
  spearman_corr <- cor(data, method = "spearman", use = "pairwise.complete.obs")
  distance_matrix <- as.dist(1 - spearman_corr)
  max_distances <- apply(as.matrix(distance_matrix), 1, max)

  dendrogram_df <- data.frame(Sample = names(max_distances), 
                              Dendrogram_Distance = max_distances, 
                              stringsAsFactors = FALSE)
  
  # Identify outliers based on NSig80, Gini, and Dendrogram Distance
  Q1_nsig <- quantile(nsig_df$NSig80, 0.25)
  Q3_nsig <- quantile(nsig_df$NSig80, 0.75)
  IQR_nsig <- IQR(nsig_df$NSig80)
  
  nsig_outliers <- as.numeric((nsig_df$NSig80 < (Q1_nsig - 3 * IQR_nsig)) | 
                              (nsig_df$NSig80 > (Q3_nsig + 3 * IQR_nsig)))
  
  gini_outliers <- as.numeric(gini_df$Gini > 0.95)
  
  dendrogram_outliers <- as.numeric(dendrogram_df$Dendrogram_Distance > 0.1)
  
  high_coverage_outliers <- as.numeric(ncov5_df$HighCoverageGeneCount < (quantile(ncov5_df$HighCoverageGeneCount, 0.25) - 3 * IQR(ncov5_df$HighCoverageGeneCount)) | 
                                       ncov5_df$HighCoverageGeneCount > (quantile(ncov5_df$HighCoverageGeneCount, 0.75) + 3 * IQR(ncov5_df$HighCoverageGeneCount)))
  
  # Merge all metrics into a single dataframe
  summary_df <- merge(ncov5_df, nsig_df, by = "Sample")
  summary_df <- merge(summary_df, gini_df, by = "Sample")
  summary_df <- merge(summary_df, dendrogram_df, by = "Sample")
  
  summary_df$Outlier_HighCoverageGeneCount <- high_coverage_outliers
  summary_df$Outlier_NSig80 <- nsig_outliers
  summary_df$Outlier_Gini <- gini_outliers
  summary_df$Outlier_Dendrogram <- dendrogram_outliers
  dataSet$summary_df <- summary_df;
  RegisterData(dataSet)
  return(1)
}

GetSummaryTable <- function(dataName){
  dataSet <- readDataset(dataName)
  df <- dataSet$summary_df;
  df_rounded <- df;
  df_rounded[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], signif, digits = 3)
  return(df_rounded);
}

calculate_gini <- function(x) {
  n <- length(x)
  sorted_x <- sort(x)
  index <- 1:n
  gini <- (2 * sum(index * sorted_x) / sum(sorted_x)) - (n + 1)
  return(gini / n)
}


ComputePERMANOVA <- function(pc1, pc2, cls, numPermutations = 999) {
  # Combine PC1 and PC2 scores into a matrix
  pc.mat <- cbind(pc1, pc2)
  
  # Calculate PERMANOVA significance
  res <- .calculateDistSig(pc.mat, cls)
  
  # Extract the main results
  resTab <- res[[1]][1, ]
  
  # Format and create the PERMANOVA summary statistics
  stat.info <- paste("[PERMANOVA] F-value: ", signif(resTab$F, 5),
                     "; R-squared: ", signif(resTab$R2, 5),
                     "; p-value (based on ", numPermutations, " permutations): ",
                     signif(resTab$Pr, 5), sep = "")
  
  # Create a named vector for the statistics
  stat.info.vec <- c(F_value = signif(resTab$F, 5), 
                     R_squared = signif(resTab$R2, 5), 
                     p_value = signif(resTab$Pr, 5))
  names(stat.info.vec) <- c("F-value", "R-squared", "p-value");

  # Extract pairwise PERMANOVA results if available
  pair.res <- res[[2]]
  
  # Return the results as a list
  list(
    stat.info = stat.info,
    stat.info.vec = stat.info.vec,
    pair.res = pair.res
  )
}

# use a PERMANOVA to partition the euclidean distance by groups based on current score plot:
.calculateDistSig <- function(pc.mat, grp){

    data.dist <- dist(as.matrix(pc.mat), method = 'euclidean');
    res <- vegan::adonis2(formula = data.dist ~ grp);

    # pairwise for multi-grp
    if(length(levels(grp)) > 2){
      pair.res <- .permanova_pairwise(x = data.dist, grp);
      rownames(pair.res) <- pair.res$pairs;
      pair.res$pairs <- NULL;
      pair.res <- signif(pair.res,5);
      fast.write.csv(pair.res, file="pca_pairwise_permanova.csv");
    }else{
      pair.res <- NULL;
    }

    return(list(res, pair.res));
}

###adopted from ecole package https://rdrr.io/github/phytomosaic/ecole/
.permanova_pairwise <- function(x,
                                 grp,
                                 permutations = 999,
                                 method = 'bray',
                                 padj = 'fdr', ...) {
  f     <- grp
  if (!all(table(f) > 1)) warning('factor has singletons! perhaps lump them?')
  co    <- combn(unique(as.character(f)),2)
  nco   <- NCOL(co)
  out   <- data.frame(matrix(NA, nrow=nco, ncol=5))
  dimnames(out)[[2]] <- c('pairs', 'SumOfSqs', 'F.Model', 'R2', 'pval')
  if (!inherits(x, 'dist')) {
    D <- vegan::vegdist(x, method=method)
  } else {
    D <- x
  }
  #cat('Now performing', nco, 'pairwise comparisons. Percent progress:\n')
  for(j in 1:nco) {
    cat(round(j/nco*100,0),'...  ')
    ij  <- f %in% c(co[1,j],co[2,j])
    Dij <- as.dist(as.matrix(D)[ij,ij])
    fij <- data.frame(fij = f[ij])
    a   <- vegan::adonis2(Dij ~ fij, data=fij, permutations = permutations, ...);
    out[j,1] <- paste(co[1,j], 'vs', co[2,j])
    out[j,2] <- a$SumOfSqs[1]
    out[j,3] <- a$F[1]
    out[j,4] <- a$R2[1]
    out[j,5] <- a$`Pr(>F)`[1]
  }
  #cat('\n')
  out$p.adj <- p.adjust(out$pval, method=padj)
  out$SumOfSqs <-NULL
  #attr(out, 'p.adjust.method') <- padj
  #cat('\np-adjust method:', padj, '\n\n');
  return(out)
}
