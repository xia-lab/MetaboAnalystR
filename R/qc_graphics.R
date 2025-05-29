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
  if(dpi == 72){
  dpi <- dpi *1.34
  }
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

  ######check.names=F is important for names not following conventions (i.e. with -, starts with number)
  df <- data.frame(dataSet$data.norm, stringsAsFactors = FALSE, check.names = FALSE)
  df <- stack(df)
  sampleNms <-colnames(dataSet$data.norm)
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
    conv <- data.frame(ind=sampleNms, Conditions=Conditions, stringsAsFactors = FALSE, check.names = FALSE)
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
  if(dpi == 72){
  dpi <- dpi *1.34
  }
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
  if(dpi == 72){
  dpi <- dpi *1.34
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
  if(dpi == 72){
  dpi <- dpi *1.34
  }
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
  qc.pcaplot.json(dataSet, dataSet$data.norm, imgName);

  }else{
    qc.pcaplot(dataSet, dataSet$data.anot, imgName, dpi, format, F);
  qc.pcaplot.json(dataSet, dataSet$data.anot, imgName);

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
  analSet$pca <- pca;
  analSet$permanova.res <-permanova_results;
  saveSet(analSet);
  saveSet(paramSet);
  
  if (interactive) {
    library(plotly)
    m <- list(l=50, r=50, b=20, t=20, pad=0.5)
    w <- if (length(dataSet$meta.info)==2) 1000 else 800
    ggp_build <- layout( ggplotly(pcafig), autosize=FALSE, width=w, height=600, margin=m )

    return(ggp_build)
  } else {
    # … your existing non‐interactive saving logic …
  if(dpi == 72){
  dpi <- dpi *1.34
  }
    Cairo(file = imgNm, width=width, height=height, type=format, bg="white", unit="in", dpi=dpi)
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
qc.ncov5 <- function(dataSet, x,
                     imgNm = "NCov5_plot",
                     dpi   = 72,
                     format = "png",
                     interactive = FALSE) {
  ## ── packages ───────────────────────────────────────
  require(ggplot2)
  require(ggrepel)
  require(Cairo)
  
  ## ── dpi check ──────────────────────────────────────
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")
  
  ## ── compute NCov5 per sample ───────────────────────
  HighCoverageGeneCount <- colSums(x > 5)
  
  df <- data.frame(Sample = names(HighCoverageGeneCount),
                   HighCoverageGeneCount = as.numeric(HighCoverageGeneCount),
                   stringsAsFactors = FALSE)
  df <- df[is.finite(df$HighCoverageGeneCount), ]
  
  ## ── outlier thresholds (± 3×IQR) ───────────────────
  Q1  <- quantile(df$HighCoverageGeneCount, 0.25)
  Q3  <- quantile(df$HighCoverageGeneCount, 0.75)
  IQRv <- IQR(df$HighCoverageGeneCount)
  lower <- Q1 - 3 * IQRv
  upper <- Q3 + 3 * IQRv
  
  df$Status <- ifelse(df$HighCoverageGeneCount < lower |
                        df$HighCoverageGeneCount > upper,
                      "Outlier", "Normal")
  
  ## ── build box-plot ─────────────────────────────────
  g <- ggplot(df, aes(x = "", y = HighCoverageGeneCount)) +
        geom_boxplot(outlier.shape = NA, fill = "grey80") +
        geom_jitter(aes(color = Status), width = 0.25, height = 0) +
        geom_hline(yintercept = c(lower, upper),
                   linetype = "dashed", color = "blue") +
        geom_text_repel(data = subset(df, Status == "Outlier"),
                        aes(label = Sample), nudge_x = 0.35, size = 3) +
        scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                           name = "Sample status") +
        theme_minimal(base_size = 11) +
        labs(x = NULL,
             y = "Genes with > 5 uniquely mapped reads") +
        theme(axis.text.x  = element_blank(),
              axis.ticks.x = element_blank())
  
  ## ── output ─────────────────────────────────────────
  width  <- 8
  height <- 6
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep = "")
  imgNm  <- paste0(fileNm, format)
  
  if (interactive) {
    require(plotly)
    m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
    return(layout(plotly::ggplotly(g),
                  autosize = FALSE, width = 1000, height = 600, margin = m))
  } else {
    if (dpi == 72) dpi <- dpi * 1.34   # keep existing scaling rule
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
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
qc.nsig <- function(dataSet, x, imgNm = "NSig80_plot", dpi = 72,
                    format = "png", interactive = FALSE) {
  require("ggplot2")
  require("Cairo")
  require("ggrepel")      # for non-overlapping labels
  
  ## ensure dpi is positive ----
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")
  
  ## compute NSig80 per sample ----
  NSig80 <- apply(x, 2, function(col)
    sum(cumsum(sort(col, decreasing = TRUE)) <= 0.8 * sum(col)))
  
  df <- data.frame(Sample = names(NSig80),
                   NSig80  = as.numeric(NSig80),
                   stringsAsFactors = FALSE)
  df <- df[is.finite(df$NSig80), ]
  
  ## identify outliers (± 3×IQR) ----
  Q1  <- quantile(df$NSig80, 0.25)
  Q3  <- quantile(df$NSig80, 0.75)
  IQRv <- IQR(df$NSig80)
  lower <- Q1 - 3 * IQRv
  upper <- Q3 + 3 * IQRv
  
  df$outlier <- ifelse(df$NSig80 < lower | df$NSig80 > upper,
                       "Outlier", "Normal")
  
  ## build box-plot with outlier labels ----
  g <- ggplot(df, aes(x = "", y = NSig80)) +
    geom_boxplot(outlier.shape = NA, fill = "grey80") +
    geom_jitter(aes(color = outlier), width = 0.25, height = 0) +
    scale_color_manual(values = c("Normal" = "grey40", "Outlier" = "red")) +
    geom_text_repel(data = subset(df, outlier == "Outlier"),
                    aes(label = Sample), nudge_x = 0.35, size = 3) +
    geom_hline(yintercept = c(lower, upper), linetype = "dashed",
               color = "blue") +
    theme_minimal(base_size = 11) +
    labs(x = NULL,
         y = "NSig80 (genes reaching 80 % of signal)",
         color = "Sample Status") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  ## output ----
  width  <- 8
  height <- 6
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep = "")
  imgNm  <- paste0(fileNm, format)
  
  if (interactive) {
    require("plotly")
    m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
    return(layout(plotly::ggplotly(g),
                  autosize = FALSE, width = 1000, height = 600, margin = m))
  } else {
    if (dpi == 72) dpi <- dpi * 1.34   # keep original scaling rule
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
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
  if(dpi == 72){
  dpi <- dpi *1.34
  }
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
qc.dendrogram <- function(dataSet, x, threshold = 0.1,
                          imgNm   = "Dendrogram_plot",
                          dpi     = 72,
                          format  = "png",
                          interactive = FALSE) {
  ## ── packages ───────────────────────────────────────
  require(ggplot2)
  require(ggrepel)      # for tidy labels
  require(Cairo)
  
  ## ── dpi check ──────────────────────────────────────
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")
  
  ## ── compute max pair-wise Spearman distance per sample ─
  spearman_corr <- cor(x, method = "spearman",
                       use = "pairwise.complete.obs")
  dist_mat      <- as.dist(1 - spearman_corr)
  max_dist      <- apply(as.matrix(dist_mat), 1, max)
  
  df <- data.frame(Sample      = names(max_dist),
                   MaxDistance = as.numeric(max_dist),
                   stringsAsFactors = FALSE)
  df$Status <- ifelse(df$MaxDistance > threshold, "Outlier", "Normal")
  
  ## ── build box-plot ─────────────────────────────────
  g <- ggplot(df, aes(x = "", y = MaxDistance)) +
        geom_boxplot(outlier.shape = NA, fill = "grey80") +
        geom_jitter(aes(color = Status), width = 0.25, height = 0) +
        geom_hline(yintercept = threshold, linetype = "dashed",
                   color = "blue") +
        geom_text_repel(data = subset(df, Status == "Outlier"),
                        aes(label = Sample), nudge_x = 0.35, size = 3) +
        scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                           name = "Sample status") +
        theme_minimal(base_size = 11) +
        labs(x = NULL,
             y = "Max pair-wise distance (1 − Spearman ρ)") +
        theme(axis.text.x  = element_blank(),
              axis.ticks.x = element_blank())
  
  ## ── output ─────────────────────────────────────────
  width  <- 8
  height <- 6
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep = "")
  imgNm  <- paste0(fileNm, format)
  
  if (interactive) {
    require(plotly)
    m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
    return(layout(plotly::ggplotly(g),
                  autosize = FALSE, width = 1000, height = 600, margin = m))
  } else {
    if (dpi == 72) dpi <- dpi * 1.34   # keep original scaling rule
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
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
qc.pcaplot.json <- function(dataSet, x, imgNm) {
  jsonFile <- paste0(imgNm, ".json")
  
  require(lattice)
  require(ggplot2)
  require(reshape)
  require(see)
  require(ggrepel)
  require(plotly)
  require(rjson)

  # load PCA & metadata
  analSet <- readSet(analSet, "analSet")
  pca     <- analSet$pca
  imp     <- summary(pca)$importance[2, 1:2]
  xlabel  <- sprintf("PC1 (%.1f%%)", 100 * imp[1])
  ylabel  <- sprintf("PC2 (%.1f%%)", 100 * imp[2])
  
  pca.res <- as.data.frame(pca$x)[, 1:2, drop = FALSE]
  colnames(pca.res) <- c("PC1", "PC2")
  pca.res <- pca.res[match(rownames(dataSet$meta.info), rownames(pca.res)), ]
  
  # metadata1 → color
  pca.res$group  <- as.character(dataSet$meta.info[[1]])
  pca.res$sample <- rownames(pca.res)
  
  # detect 2nd metadata for shapes
  doShape <- FALSE
  if (ncol(dataSet$meta.info) >= 2) {
    second <- dataSet$meta.info[[2]]
    isDisc  <- !is.numeric(second)
    levs    <- unique(as.character(second))
    if (isDisc && length(levs) <= 6) {
      doShape      <- TRUE
      pca.res$shape <- as.character(second)
      symbols      <- c("circle","square","diamond",
                        "cross","x","triangle-up",
                        "triangle-down","star")
      shape.map    <- setNames(symbols[seq_along(levs)], levs)
      shape.levels <- levs
    }
  }
  
  # ——— COLOR MAPPING exactly as in qc.pcaplot() ————————————
  # read paramSet for dose check
  paramSet <- readSet(paramSet, "paramSet")
  unique_grps <- unique(pca.res$group)
  
  if (grepl("norm", imgNm) &&
      !is.null(paramSet$oneDataAnalType) &&
      paramSet$oneDataAnalType == "dose") {
    
    # blue→orange ramp for dose
    pal <- colorRampPalette(c("#2196F3", "#DE690D"))(length(unique_grps))
    
  } else {
    # Okabe–Ito discrete palette
    okabe <- c("#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7")
    pal <- rep(okabe, length.out = length(unique_grps))
  }
  col.map <- setNames(pal, unique_grps)
  # ——————————————————————————————————————————————————————————

  # build traces
  traces <- lapply(unique_grps, function(g) {
    df  <- subset(pca.res, group == g)
    mkr <- list(color = col.map[g], size = 8,
                line  = list(color = "white", width = 0.5))
    if (doShape) {
      mkr$symbol <- unname(shape.map[df$shape])
    }
    list(
      x            = df$PC1,
      y            = df$PC2,
      type         = "scatter",
      mode         = if (nrow(df) > 20) "markers" else "markers+text",
      name         = g,
      marker       = mkr,
      text         = if (nrow(df) <= 20) df$sample else NULL,
      hoverinfo    = "text",
      textposition = "top center"
    )
  })

  # append dummy shape‐legend traces
  if (doShape) {
    for (sh in shape.levels) {
      traces <- c(traces, list(
        list(
          x          = c(NA), y = c(NA),
          type       = "scatter", mode = "markers",
          name       = sh,
          showlegend = TRUE,
          marker     = list(symbol = shape.map[[sh]],
                            color  = "black",
                            size   = 8)
        )
      ))
    }
  }

  # layout with legend on right
  layout <- list(
    title = "",
    xaxis = list(title = xlabel),
    yaxis = list(title = ylabel),
    legend = list(
      orientation = "v",
      x           = 1.02,
      y           = 1,
      xanchor     = "left",
      yanchor     = "top"
    )
  )

  # dump JSON
  plot_data <- list(data = traces, layout = layout)
  json.obj   <- toJSON(plot_data)
  sink(jsonFile); cat(json.obj); sink()
  
  return("NA")
}

PlotDataGini <- function(fileName, imgName, threshold, dpi, format){
  dataSet <- readDataset(fileName);
  if(grepl("_norm", imgName)){
    qc.gini(dataSet, dataSet$data.norm,0.95, imgName, dpi, format, F);
  }else{
    qc.gini(dataSet, dataSet$data.anot,0.95, imgName, dpi, format, F);
  }
  return("NA");
}

qc.gini <- function(dataSet, x, threshold = 0.95,
                    imgNm   = "Gini_plot",
                    dpi     = 72,
                    format  = "png",
                    interactive = FALSE) {
  ## ── packages ───────────────────────────────────────
  require(ggplot2)
  require(ggrepel)
  require(Cairo)
  
  ## ── dpi check ──────────────────────────────────────
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")
  
  ## ── helper: Gini coefficient ───────────────────────
  if (!exists("calculate_gini", mode = "function")) {
    calculate_gini <- function(v) {
      v <- as.numeric(v); v <- v[v >= 0]
      if (length(v) == 0 || sum(v) == 0) return(0)
      v <- sort(v)
      n <- length(v)
      G <- 1 - (2 * sum((n:1) * v)) / (n * sum(v)) + 1 / n
      return(G)
    }
  }
  
  ## ── compute Gini per sample ────────────────────────
  gini_vals <- apply(x, 2, calculate_gini)
  
  df <- data.frame(Sample = names(gini_vals),
                   Gini   = as.numeric(gini_vals),
                   stringsAsFactors = FALSE)
  df$Status <- ifelse(df$Gini > threshold, "Outlier", "Normal")
  
  ## ── build box-plot ─────────────────────────────────
  g <- ggplot(df, aes(x = "", y = Gini)) +
        geom_boxplot(outlier.shape = NA, fill = "grey80") +
        geom_jitter(aes(color = Status), width = 0.25, height = 0) +
        geom_hline(yintercept = threshold, linetype = "dashed",
                   color = "blue") +
        geom_text_repel(data = subset(df, Status == "Outlier"),
                        aes(label = Sample), nudge_x = 0.35, size = 3) +
        scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                           name = "Sample status") +
        theme_minimal(base_size = 11) +
        labs(x = NULL,
             y = "Gini coefficient") +
        theme(axis.text.x  = element_blank(),
              axis.ticks.x = element_blank())
  
  ## ── output ─────────────────────────────────────────
  width  <- 8
  height <- 6
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep = "")
  imgNm  <- paste0(fileNm, format)
  
  if (interactive) {
    require(plotly)
    m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
    return(layout(plotly::ggplotly(g),
                  autosize = FALSE, width = 1000, height = 600, margin = m))
  } else {
    if (dpi == 72) dpi <- dpi * 1.34   # keep original scaling rule
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
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