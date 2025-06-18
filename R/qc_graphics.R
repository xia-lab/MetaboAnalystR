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
  require("ggplot2");

  dataSet <- readDataset(fileName);
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  dpi <- as.numeric(dpi);

  data.anot <- .get.annotated.data();
  data_bef<-data.matrix(data.anot);
  
  smpl.sums <- colSums(data_bef);
  
  names(smpl.sums) <- sampleNms <- colnames(data_bef);
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
    data.anot <- .get.annotated.data();
    res <- qc.meanstd(data.anot, imgName, dpi, format);
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
    data.anot <- .get.annotated.data();
    qc.pcaplot(dataSet, data.anot, imgName, dpi, format, F);
    qc.pcaplot.json(dataSet, data.anot, imgName);

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

  # only save those required for json
  # never use more than top 3. Update this if you require more PCs
  pca$x <- pca$x[,1:3]; # 
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
  dataSet <- readDataset(fileName)
  if (is.null(dataSet$summary_df)) {
    stop("summary_df not found in dataSet. Please run SummarizeQC first.")
  }

  ncov5_df <- dataSet$summary_df[, c("Sample", "HighCoverageGeneCount")]
  
  ## Compute outlier limits
  Q1  <- quantile(ncov5_df$HighCoverageGeneCount, 0.25)
  Q3  <- quantile(ncov5_df$HighCoverageGeneCount, 0.75)
  IQRv <- IQR(ncov5_df$HighCoverageGeneCount)
  lower <- Q1 - 3 * IQRv
  upper <- Q3 + 3 * IQRv

  ncov5_df$Status <- ifelse(ncov5_df$HighCoverageGeneCount < lower |
                            ncov5_df$HighCoverageGeneCount > upper,
                            "Outlier", "Normal")

  qc.ncov5.plot(ncov5_df, imgName, lower, upper, dpi, format);
  qc.ncov5plot.json(ncov5_df, imgName, lower, upper);
  return("NA")
}

qc.ncov5.plot <- function(ncov5_df,
                          imgNm = "NCov5_plot",
                          lower,
                          upper,
                          dpi = 72,
                          format = "png",
                          interactive = FALSE) {
  require(ggplot2)
  require(ggrepel)
  require(Cairo)
  
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")

  g <- ggplot(ncov5_df, aes(x = "", y = HighCoverageGeneCount)) +
    geom_boxplot(outlier.shape = NA, fill = "grey80") +
    geom_jitter(aes(color = Status), width = 0.25, height = 0) +
    geom_hline(yintercept = c(lower, upper),
               linetype = "dashed", color = "blue") +
    geom_text_repel(data = subset(ncov5_df, Status == "Outlier"),
                    aes(label = Sample), nudge_x = 0.35, size = 3) +
    scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                       name = "Sample status") +
    theme_minimal(base_size = 11) +
    labs(x = NULL,
         y = "Genes with > 5 uniquely mapped reads") +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
  
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
    if (dpi == 72) dpi <- dpi * 1.34
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
}


PlotDataNsig <- function(fileName, imgName, dpi, format){
  dataSet <- readDataset(fileName)
  if (is.null(dataSet$summary_df)) {
    stop("summary_df not found in dataSet. Please run SummarizeQC first.")
  }

  nsig_df <- dataSet$summary_df[, c("Sample", "NSig80")]


  ## identify outliers (± 3×IQR)
  Q1  <- quantile(nsig_df$NSig80, 0.25)
  Q3  <- quantile(nsig_df$NSig80, 0.75)
  IQRv <- IQR(nsig_df$NSig80)
  lower <- Q1 - 3 * IQRv
  upper <- Q3 + 3 * IQRv

  nsig_df$outlier <- ifelse(nsig_df$NSig80 < lower | nsig_df$NSig80 > upper,
                            "Outlier", "Normal")

  qc.nsig.plot(nsig_df, imgName, lower, upper, dpi, format)
  qc.nsigplot.json(nsig_df, imgName, lower, upper); 
  return("NA")
}

qc.nsig.plot <- function(nsig_df,
                         imgNm = "NSig80_plot",
                         lower,
                         upper,
                         dpi = 72,
                         format = "png",
                         interactive = FALSE) {
  require("ggplot2")
  require("Cairo")
  require("ggrepel")

  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")

  g <- ggplot(nsig_df, aes(x = "", y = NSig80)) +
    geom_boxplot(outlier.shape = NA, fill = "grey80") +
    geom_jitter(aes(color = outlier), width = 0.25, height = 0) +
    scale_color_manual(values = c("Normal" = "grey40", "Outlier" = "red")) +
    geom_text_repel(data = subset(nsig_df, outlier == "Outlier"),
                    aes(label = Sample), nudge_x = 0.35, size = 3) +
    geom_hline(yintercept = c(lower, upper), linetype = "dashed",
               color = "blue") +
    theme_minimal(base_size = 11) +
    labs(x = NULL,
         y = "NSig80 (genes reaching 80 % of signal)",
         color = "Sample Status") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

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
    if (dpi == 72) dpi <- dpi * 1.34
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
}

PlotDataDendrogram <- function(fileName, imgName, threshold, dpi, format){
  dataSet <- readDataset(fileName)

  if (is.null(dataSet$summary_df)) {
    stop("summary_df not found in dataSet. Please run SummarizeQC first.")
  }
  dendro_df <- dataSet$summary_df[, c("Sample", "Dendrogram_Distance")]
  dendro_df$Status <- ifelse(dendro_df$Dendrogram_Distance > threshold, "Outlier", "Normal")

  ## Decide label set
  out_idx <- which(dendro_df$Status == "Outlier")
  label_idx <- if (length(out_idx) <= 20) {
    out_idx
  } else {
    out_idx[order(dendro_df$Dendrogram_Distance[out_idx], decreasing = TRUE)[1:20]]
  }
  dendro_df$LabelMe <- FALSE
  dendro_df$LabelMe[label_idx] <- TRUE


  qc.dendrogram.plot(dendro_df, threshold, imgName, dpi, format)
  qc.dendrogram.json(dendro_df, imgName);
  return("NA")
}

qc.dendrogram.plot <- function(dendro_df,
                               threshold = 0.1,
                               imgNm = "Dendrogram_plot",
                               dpi = 72,
                               format = "png",
                               interactive = FALSE) {
  require(ggplot2)
  require(ggrepel)
  require(Cairo)

  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be positive.")

  set.seed(1)  # For reproducible jitter
  dendro_df$xj <- jitter(rep(1, nrow(dendro_df)), amount = 0.25)

  g <- ggplot(dendro_df, aes(x = xj, y = Dendrogram_Distance)) +
    geom_boxplot(aes(x = 1), outlier.shape = NA,
                 width = 0.4, fill = "grey80") +
    geom_point(aes(color = Status), size = 2.2) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "blue") +
    geom_text_repel(data = dendro_df[dendro_df$LabelMe, ],
                    aes(label = Sample),
                    max.overlaps = Inf,
                    box.padding = 0.35,
                    segment.size = 0.2,
                    size = 4.2) +
    scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                       name = "Sample status") +
    theme_minimal(base_size = 12) +
    labs(x = NULL, y = "Max pair-wise distance (1 − Pearson ρ)") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  outFile <- paste0(imgNm, "dpi", dpi, ".", format)

  if (interactive) {
    require(plotly)
    m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
    return(layout(plotly::ggplotly(g),
                  autosize = FALSE, width = 1000, height = 600, margin = m))
  } else {
    if (dpi == 72) dpi <- dpi * 1.34
    Cairo(file = outFile, width = 8, height = 6,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return(outFile)
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
  dataSet <- readDataset(fileName)
  if (is.null(dataSet$summary_df)) {
    stop("summary_df not found in dataSet. Please run SummarizeQC first.")
  }
  
  # Select Gini data
  gini_df <- dataSet$summary_df[, c("Sample", "Gini")]
  gini_df$Status <- ifelse(gini_df$Gini > threshold, "Outlier", "Normal")
  
  ## Plot
  qc.gini.plot(gini_df, imgName, threshold, dpi, format)
  qc.giniplot.json(gini_df, imgName);
  return("NA")
}

qc.gini.plot <- function(gini_df,
                         imgNm   = "Gini_plot",
                         threshold = 0.95,
                         dpi     = 72,
                         format  = "png",
                         interactive = FALSE) {
  require(ggplot2)
  require(ggrepel)
  require(Cairo)
  
  dpi <- as.numeric(dpi)
  if (dpi <= 0) stop("DPI must be a positive number.")
  
  g <- ggplot(gini_df, aes(x = "", y = Gini)) +
    geom_boxplot(outlier.shape = NA, fill = "grey80") +
    geom_jitter(aes(color = Status), width = 0.25, height = 0) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "blue") +
    geom_text_repel(data = subset(gini_df, Status == "Outlier"),
                    aes(label = Sample), nudge_x = 0.35, size = 3) +
    scale_color_manual(values = c(Normal = "grey40", Outlier = "red"),
                       name = "Sample status") +
    theme_minimal(base_size = 11) +
    labs(x = NULL, y = "Gini coefficient") +
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
  
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
    if (dpi == 72) dpi <- dpi * 1.34
    Cairo(file = imgNm, width = width, height = height,
          type = format, bg = "white", dpi = dpi, unit = "in")
    print(g)
    dev.off()
    return("NA")
  }
}
SummarizeQC <- function(fileName, imgNameBase, threshold = 0.1) {
  save.image("summarize.RData");
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

  if (grepl("norm", imgNameBase)) {
    data <- dataSet$data.norm
  } else {
    data <- .get.annotated.data();
  }


  HighCoverageGeneCount <- colSums(data > 5)
  ncov5_df <- data.frame(Sample = names(HighCoverageGeneCount), 
                         HighCoverageGeneCount = as.numeric(HighCoverageGeneCount), 
                         stringsAsFactors = FALSE)

  NSig80 <- apply(data, 2, function(col) sum(cumsum(sort(col, decreasing = TRUE)) <= 0.8 * sum(col)))
  nsig_df <- data.frame(Sample = names(NSig80), NSig80 = as.numeric(NSig80), stringsAsFactors = FALSE)

  gini_scores <- apply(data, 2, calculate_gini)
  gini_df <- data.frame(Sample = colnames(data), Gini = gini_scores, stringsAsFactors = FALSE)

  ## Use Pearson correlation for dendrogram distance
  pearson_corr <- cor(data, method = "pearson", use = "pairwise.complete.obs")
  distance_matrix <- as.dist(1 - pearson_corr)
  dist_mat <- as.matrix(distance_matrix)

  group_info <- dataSet$meta.info[,1]
  names(group_info) <- rownames(dataSet$meta.info)

  max_distances <- sapply(colnames(data), function(sample) {
    sample_group <- group_info[sample]
    same_group_samples <- names(group_info)[group_info == sample_group]
    same_group_samples <- same_group_samples[same_group_samples != sample]

    if (length(same_group_samples) == 0) {
      return(NA)
    }

    max(dist_mat[sample, same_group_samples], na.rm = TRUE)
  })

  dendrogram_df <- data.frame(Sample = names(max_distances), 
                              Dendrogram_Distance = max_distances, 
                              stringsAsFactors = FALSE)

  # Outlier calls
  Q1_nsig <- quantile(nsig_df$NSig80, 0.25)
  Q3_nsig <- quantile(nsig_df$NSig80, 0.75)
  IQR_nsig <- IQR(nsig_df$NSig80)

  nsig_outliers <- as.numeric((nsig_df$NSig80 < (Q1_nsig - 3 * IQR_nsig)) | 
                              (nsig_df$NSig80 > (Q3_nsig + 3 * IQR_nsig)))

  gini_outliers <- as.numeric(gini_df$Gini > 0.95)

  dendrogram_outliers <- as.numeric(dendrogram_df$Dendrogram_Distance > threshold)

  high_coverage_outliers <- as.numeric(
    ncov5_df$HighCoverageGeneCount < (quantile(ncov5_df$HighCoverageGeneCount, 0.25) - 3 * IQR(ncov5_df$HighCoverageGeneCount)) | 
    ncov5_df$HighCoverageGeneCount > (quantile(ncov5_df$HighCoverageGeneCount, 0.75) + 3 * IQR(ncov5_df$HighCoverageGeneCount))
  )

  # Combine
  summary_df <- merge(ncov5_df, nsig_df, by = "Sample")
  summary_df <- merge(summary_df, gini_df, by = "Sample")
  summary_df <- merge(summary_df, dendrogram_df, by = "Sample")

  summary_df$Outlier_HighCoverageGeneCount <- high_coverage_outliers
  summary_df$Outlier_NSig80 <- nsig_outliers
  summary_df$Outlier_Gini <- gini_outliers
  summary_df$Outlier_Dendrogram <- dendrogram_outliers

  dataSet$summary_df <- summary_df
  print(summary_df);
  RegisterData(dataSet)

  return(1)
}

# -------------------------------------------------------------------------
#  qc.giniplot.json()
#  ------------------------------------------------------------------------
#  gini_df      data.frame with columns: Sample, Gini, Status
#  imgNm        stem for the JSON file (".json" is appended automatically)
#  threshold    horizontal dashed reference line
#  jitter.w     half-width of horizontal jitter (0–0.5 recommended)
# -------------------------------------------------------------------------
qc.giniplot.json <- function(gini_df,
                             imgNm     = "Gini_plot",
                             threshold = 0.95,
                             jitter.w  = 0.45) {

  stopifnot(all(c("Sample", "Gini", "Status") %in% names(gini_df)))

  ## 1 · Tukey fences & statistical-outlier flag -------------------------
  stats      <- boxplot.stats(gini_df$Gini, coef = 1.5)$stats
  q1         <- stats[2]; q3 <- stats[4]; iqr <- q3 - q1
  lowFence   <- q1 - 1.5 * iqr
  highFence  <- q3 + 1.5 * iqr
  gini_df$stat_out <- with(gini_df, Gini < lowFence | Gini > highFence)

  ## 2 · Semantic palette (Normal / Outlier) -----------------------------
  status_cols <- c(Normal = "#666666", Outlier = "#E41A1C")

  ## 3 · Traces ----------------------------------------------------------
  # 3a · Box built from in-fence values only
  tr_box <- list(
    x              = rep(0, sum(!gini_df$stat_out)),
    y              = I(gini_df$Gini[!gini_df$stat_out]),
    quartilemethod = "linear",
    type           = "box",
    width          = 0.8,
    name           = "",
    boxpoints      = FALSE,
    fillcolor      = "rgba(200,200,200,0.6)",
    line           = list(color = "#000000"),
    hoverinfo      = "skip",
    showlegend     = FALSE
  )

  # 3b · Invisible all-points trace (for autoscale)
  set.seed(1)
  tr_all <- list(
    x          = I(runif(nrow(gini_df), -jitter.w, jitter.w)),
    y          = I(gini_df$Gini),
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = "rgba(0,0,0,0)", size = 0),
    hoverinfo  = "skip",
    showlegend = FALSE
  )

  # 3c · Visible points (semantic colour, stat-outlines)
  show_labels <- nrow(gini_df) <= 20
  set.seed(2)
  points_trace <- list(
    x    = I(runif(nrow(gini_df), -jitter.w, jitter.w)),
    y    = I(gini_df$Gini),
    type = "scatter",
    mode = if (show_labels) "markers+text" else "markers",
    text = if (show_labels) gini_df$Sample else "",
    textposition = "right",
    name = "Samples",
    hoverinfo = "text",
    hovertext = paste0(
      "Sample: ", gini_df$Sample,
      "<br>Gini: ", signif(gini_df$Gini, 3),
      "<br>Status: ", gini_df$Status
    ),
    marker = list(
      color = status_cols[gini_df$Status],
      size  = 8,
      line  = list(
        color = ifelse(gini_df$stat_out, "black", "rgba(0,0,0,0)"),
        width = ifelse(gini_df$stat_out, 1, 0)
      )
    ),
    showlegend = FALSE
  )

  traces <- list(tr_box, tr_all, points_trace)

  ## 4 · Layout ----------------------------------------------------------
  layout <- list(
    plot_bgcolor  = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",
    xaxis = list(
      title          = "",
      range          = c(-jitter.w - 0.1, jitter.w + 0.1),
      zeroline       = FALSE,
      showticklabels = FALSE,
      showline       = TRUE,
      linecolor      = "#000000"
    ),
    yaxis = list(
      title     = list(text = "Gini coefficient"),
      zeroline  = FALSE,
      ticks     = "outside",
      showline  = TRUE,
      linecolor = "#000000",
      showgrid  = TRUE,
      gridcolor = "rgba(200,200,200,0.4)"
    ),
    shapes = list(list(
      type  = "line",
      xref  = "paper", x0 = 0, x1 = 1,
      yref  = "y",     y0 = threshold, y1 = threshold,
      line  = list(color = "#0026FF", dash = "dot")
    )),
    legend = list(
      title       = list(text = "Sample Status"),
      orientation = "v",
      x = 1.02, y = 1,
      xanchor = "left", yanchor = "top"
    ),
    margin = list(l = 60, r = 110, t = 20, b = 40)
  )

  ## 5 · Write JSON ------------------------------------------------------
  jsonlite::write_json(
    list(data = traces, layout = layout),
    paste0(imgNm, ".json"),
    auto_unbox = TRUE, digits = 16
  )

  invisible("NA")
}


# -------------------------------------------------------------------------
#  dendro_df   data.frame with columns:
#              Sample, Dendrogram_Distance, Status (Normal / Outlier),
#              LabelMe (TRUE/FALSE → label on plot)
#  imgNm       stem for JSON file ("<imgNm>.json")
#  threshold   horizontal dashed cut-off
#  jitter.w    half-width for horizontal jitter of points
# -------------------------------------------------------------------------
qc.dendrogram.json <- function(dendro_df,
                               imgNm     = "Dendrogram_plot",
                               threshold = 0.10,
                               jitter.w  = 0.45) {

  stopifnot(all(c("Sample", "Dendrogram_Distance", "Status", "LabelMe") %in% names(dendro_df)))

  ## ── 1 · Tukey fences and statistical-outlier flag -------------------
  stats     <- boxplot.stats(dendro_df$Dendrogram_Distance, coef = 1.5)$stats
  q1        <- stats[2];  q3 <- stats[4];  iqr <- q3 - q1
  lowFence  <- q1 - 1.5 * iqr
  highFence <- q3 + 1.5 * iqr
  dendro_df$stat_out <- with(dendro_df,
                             Dendrogram_Distance < lowFence |
                             Dendrogram_Distance > highFence)

  ## ── 2 · Semantic palette -------------------------------------------
  status_cols <- c(Normal = "#666666", Outlier = "#E41A1C")

  ## ── 3 · Traces ------------------------------------------------------
  # 3a · Box from in-fence points
  tr_box <- list(
    x              = rep(0, sum(!dendro_df$stat_out)),
    y              = I(dendro_df$Dendrogram_Distance[!dendro_df$stat_out]),
    quartilemethod = "linear",
    type           = "box",
    width          = 0.8,
    name           = "",
    boxpoints      = FALSE,
    fillcolor      = "rgba(200,200,200,0.6)",
    line           = list(color = "#000000"),
    hoverinfo      = "skip",
    showlegend     = FALSE
  )

  # 3b · Invisible all-points scatter (autoscale helper)
  set.seed(1)
  tr_all <- list(
    x          = I(runif(nrow(dendro_df), -jitter.w, jitter.w)),
    y          = I(dendro_df$Dendrogram_Distance),
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = "rgba(0,0,0,0)", size = 0),
    hoverinfo  = "skip",
    showlegend = FALSE
  )

  # 3c · Visible points (semantic colouring, stat outlines)
  set.seed(2)
  points_trace <- list(
    x    = I(runif(nrow(dendro_df), -jitter.w, jitter.w)),
    y    = I(dendro_df$Dendrogram_Distance),
    type = "scatter",
    mode = "markers+text",
    text = ifelse(dendro_df$LabelMe, dendro_df$Sample, ""),
    textposition = "right",
    name = "Samples",
    hoverinfo = "text",
    hovertext = paste0(
      "Sample: ", dendro_df$Sample,
      "<br>Distance: ", signif(dendro_df$Dendrogram_Distance, 3),
      "<br>Status: ", dendro_df$Status
    ),
    marker = list(
      color = status_cols[dendro_df$Status],
      size  = 8,
      line  = list(
        color = ifelse(dendro_df$stat_out, "black", "rgba(0,0,0,0)"),
        width = ifelse(dendro_df$stat_out, 1, 0)
      )
    ),
    showlegend = FALSE
  )

  traces <- list(tr_box, tr_all, points_trace)

  ## ── 4 · Layout ------------------------------------------------------
  layout <- list(
    plot_bgcolor  = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",
    xaxis = list(
      title          = "",
      range          = c(-jitter.w - 0.1, jitter.w + 0.1),
      zeroline       = FALSE,
      showticklabels = FALSE,
      showline       = TRUE,
      linecolor      = "#000000"
    ),
    yaxis = list(
      title     = list(text = "Max pair-wise distance (1 \u2212 Pearson \u03c1)"),
      zeroline  = FALSE,
      ticks     = "outside",
      showline  = TRUE,
      linecolor = "#000000",
      showgrid  = TRUE,
      gridcolor = "rgba(200,200,200,0.4)"
    ),
    shapes = list(list(
      type  = "line",
      xref  = "paper", x0 = 0, x1 = 1,
      yref  = "y",     y0 = threshold, y1 = threshold,
      line  = list(color = "#0026FF", dash = "dot")
    )),
    legend = list(
      title       = list(text = "Sample Status"),
      orientation = "v",
      x = 1.02, y = 1,
      xanchor = "left", yanchor = "top"
    ),
    margin = list(l = 70, r = 110, t = 20, b = 40)
  )

  ## ── 5 · Write JSON ---------------------------------------------------
  jsonlite::write_json(
    list(data = traces, layout = layout),
    paste0(imgNm, ".json"),
    auto_unbox = TRUE, digits = 16
  )
  invisible("NA")
}

qc.ncov5plot.json <- function(ncov5_df,
                              imgNm    = "NCov5_plot",
                              lower,
                              upper,
                              jitter.w = 0.45) {

  stopifnot(all(c("Sample", "HighCoverageGeneCount", "Status") %in% names(ncov5_df)),
            is.numeric(lower), length(lower) == 1,
            is.numeric(upper), length(upper) == 1)

  ## ── 1 · Tukey fences & statistical-outlier flag --------------------
  stats      <- boxplot.stats(ncov5_df$HighCoverageGeneCount, coef = 1.5)$stats
  q1         <- stats[2]; q3 <- stats[4]; iqr <- q3 - q1
  lowFence   <- q1 - 1.5 * iqr
  highFence  <- q3 + 1.5 * iqr
  ncov5_df$stat_out <- with(ncov5_df,
                             HighCoverageGeneCount < lowFence |
                             HighCoverageGeneCount > highFence)

  ## ── 2 · palettes ----------------------------------------------------
  stat_cols <- c(Normal = "#666666", Outlier = "#E41A1C")

  ## ── 3 · Traces ------------------------------------------------------
  # 3a · box (only in-fence points)
  tr_box <- list(
    x              = rep(0, sum(!ncov5_df$stat_out)),
    y              = I(ncov5_df$HighCoverageGeneCount[!ncov5_df$stat_out]),
    quartilemethod = "linear",
    type           = "box",
    width          = 0.8,
    name           = "",
    boxpoints      = FALSE,
    fillcolor      = "rgba(200,200,200,0.6)",
    line           = list(color = "#000000"),
    hoverinfo      = "skip",
    showlegend     = FALSE
  )

  # 3b · invisible all-points scatter (for autoscale)
  set.seed(1)
  tr_all <- list(
    x          = I(runif(nrow(ncov5_df), -jitter.w, jitter.w)),
    y          = I(ncov5_df$HighCoverageGeneCount),
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = "rgba(0,0,0,0)", size = 0),
    hoverinfo  = "skip",
    showlegend = FALSE
  )

  # 3c · labelled points (semantic status colouring, stat-outlines)
  # 3c · visible points (one trace for all samples)
set.seed(2)
points_trace <- list(
  x    = I(runif(nrow(ncov5_df), -jitter.w, jitter.w)),
  y    = I(ncov5_df$HighCoverageGeneCount),
  type = "scatter",
  mode = if (any(ncov5_df$stat_out)) "markers+text" else "markers",
  text = ifelse(ncov5_df$stat_out, ncov5_df$Sample, ""),
  textposition = "right",
  name = "Samples",
  hoverinfo = "text",
  hovertext = paste0(
    "Sample: ", ncov5_df$Sample,
    "<br>Count: ", ncov5_df$HighCoverageGeneCount,
    "<br>Status: ", ncov5_df$Status
  ),
  marker = list(
    color = stat_cols[ncov5_df$Status],           # vector OK
    size  = 8,
    line  = list(
      color = ifelse(ncov5_df$stat_out, "black", "rgba(0,0,0,0)"),
      width = ifelse(ncov5_df$stat_out, 1, 0)
    )
  ),
  showlegend = FALSE
)

  traces <- list(tr_box, tr_all, points_trace)

  ## ── 4 · Layout ------------------------------------------------------
  layout <- list(
    plot_bgcolor  = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",
    xaxis = list(
      title          = "",
      range          = c(-jitter.w - 0.1, jitter.w + 0.1),
      zeroline       = FALSE,
      showticklabels = FALSE,
      showline       = TRUE,
      linecolor      = "#000000"
    ),
    yaxis = list(
      title     = list(text = "Genes with > 5 uniquely mapped reads"),
      zeroline  = FALSE,
      ticks     = "outside",
      showline  = TRUE,
      linecolor = "#000000",
      showgrid  = TRUE,
      gridcolor = "rgba(200,200,200,0.4)"
    ),
    shapes = list(
      list(type="line", xref="paper", x0=0, x1=1,
           yref="y", y0=lower, y1=lower,
           line=list(color="#0026FF", dash="dot")),
      list(type="line", xref="paper", x0=0, x1=1,
           yref="y", y0=upper, y1=upper,
           line=list(color="#0026FF", dash="dot"))
    ),
    legend = list(
      title       = list(text = "Sample Status"),
      orientation = "v",
      x = 1.02, y = 1,
      xanchor = "left", yanchor = "top"
    ),
    margin = list(l = 70, r = 110, t = 20, b = 40)
  )

  ## ── 5 · Write JSON ---------------------------------------------------
  jsonlite::write_json(
    list(data = traces, layout = layout),
    paste0(imgNm, ".json"),
    auto_unbox = TRUE, digits = 16
  )

  invisible("NA")
}

qc.nsigplot.json <- function(nsig_df,
                             imgNm    = "NSig80_plot",
                             lower,
                             upper,
                             jitter.w = 0.45) {

  stopifnot(all(c("Sample", "NSig80", "outlier") %in% names(nsig_df)))

  ## ------------------------------------------------------------------
  ## 1 · Compute Tukey fences and flag statistical outliers
  ## ------------------------------------------------------------------
  stats       <- boxplot.stats(nsig_df$NSig80, coef = 1.5)$stats
  q1          <- stats[2]; q3 <- stats[4]; iqr <- q3 - q1
  lowFence    <- q1 - 1.5 * iqr
  highFence   <- q3 + 1.5 * iqr
  nsig_df$stat_out <- with(nsig_df, NSig80 < lowFence | NSig80 > highFence)

  ## ------------------------------------------------------------------
  ## 2 · Palette for semantic status (Normal / Outlier)
  ## ------------------------------------------------------------------
  status_cols <- c(Normal = "#666666", Outlier = "#E41A1C")

  ## ------------------------------------------------------------------
  ## 3 · Plotly traces
  ## ------------------------------------------------------------------
  # 3a ─ Box: only in-fence points
  tr_box <- list(
    x              = rep(0, sum(!nsig_df$stat_out)),
    y              = I(nsig_df$NSig80[!nsig_df$stat_out]),
    quartilemethod = "linear",
    type           = "box",
    width          = 0.8,
    name           = "",
    boxpoints      = FALSE,
    fillcolor      = "rgba(200,200,200,0.6)",
    line           = list(color = "#000000"),
    hoverinfo      = "skip",
    showlegend     = FALSE
  )

  # 3b ─ Invisible “all” trace for autoscaling
  set.seed(1)
  tr_all <- list(
    x          = I(runif(nrow(nsig_df), -jitter.w, jitter.w)),
    y          = I(nsig_df$NSig80),
    type       = "scatter",
    mode       = "markers",
    marker     = list(color = "rgba(0,0,0,0)", size = 0),
    hoverinfo  = "skip",
    showlegend = FALSE
  )

  # 3c ─ Points (semantic status colouring, stat-outliers outlined)
  set.seed(2)
  points_trace <- list(
    x    = I(runif(nrow(nsig_df), -jitter.w, jitter.w)),
    y    = I(nsig_df$NSig80),
    type = "scatter",
    mode = "markers+text",
    text = ifelse(nsig_df$stat_out, nsig_df$Sample, ""),
    textposition = "right",
    name = "Samples",
    hoverinfo = "text",
    hovertext = paste0(
      "Sample: ", nsig_df$Sample,
      "<br>NSig80: ", nsig_df$NSig80,
      "<br>Status: ", nsig_df$outlier
    ),
    marker = list(
      color = status_cols[nsig_df$outlier],
      size  = 8,
      line  = list(
        color = ifelse(nsig_df$stat_out, "black", "rgba(0,0,0,0)"),
        width = ifelse(nsig_df$stat_out, 1, 0)
      )
    ),
    showlegend = FALSE
  )

  traces <- list(tr_box, tr_all, points_trace)

  ## ------------------------------------------------------------------
  ## 4 · Layout (unchanged)
  ## ------------------------------------------------------------------
  layout <- list(
    plot_bgcolor  = "#FFFFFF", paper_bgcolor = "#FFFFFF",
    xaxis = list(title="", range=c(-jitter.w-0.1, jitter.w+0.1),
                 zeroline=FALSE, showticklabels=FALSE,
                 showline=TRUE, linecolor="#000"),
    yaxis = list(title=list(text="NSig80 (genes reaching 80% of signal)"),
                 zeroline=FALSE, ticks="outside", showline=TRUE,
                 linecolor="#000", showgrid=TRUE,
                 gridcolor="rgba(200,200,200,0.4)"),
    shapes = list(
      list(type="line", xref="paper", x0=0, x1=1,
           yref="y", y0=lower, y1=lower,
           line=list(color="#0026FF", dash="dot")),
      list(type="line", xref="paper", x0=0, x1=1,
           yref="y", y0=upper, y1=upper,
           line=list(color="#0026FF", dash="dot"))
    ),
    legend = list(title=list(text="Sample Status"),
                  orientation="v", x=1.02, y=1,
                  xanchor="left", yanchor="top"),
    margin = list(l=70, r=110, t=20, b=40)
  )

  ## ------------------------------------------------------------------
  ## 5 · Write JSON
  ## ------------------------------------------------------------------
  jsonlite::write_json(
    list(data = traces, layout = layout),
    paste0(imgNm, ".json"),
    auto_unbox = TRUE, digits = 16
  )
}
