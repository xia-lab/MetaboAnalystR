#'Plot Dendrogram
#'@description Dendogram
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param smplDist Method to calculate sample distance
#'@param clstDist Method to calculate clustering distance 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotHCTree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, smplDist, clstDist){
  
  mSetObj <- .get.mSet(mSetObj);
  # set up data set
  hc.dat <- as.matrix(mSetObj$dataSet$norm);
  colnames(hc.dat) <- substr(colnames(hc.dat), 1, 18) # some names are too long
  # set up distance matrix
  if(smplDist == 'euclidean'){
    dist.mat <- dist(hc.dat, method = smplDist);
  }else{
    dist.mat <- dist(1-cor(t(hc.dat), method = smplDist));
  }
  
  # record the paramters
  mSetObj$analSet$tree <- list(dist.par=smplDist, clust.par=clstDist);
  # build the tree
  hc_tree <- hclust(dist.mat, method=clstDist);
  
  # plot the tree
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- minH <- 630;
    myH <- nrow(hc.dat)*10 + 150;
    if(myH < minH){
      myH <- minH;
    }   
    w <- round(w/72,2);
    h <- round(myH/72,2);
  }else if(width == 0){
    w <- h <- 7.2;
  }else{
    w <- h <- 7.2;
  }
  
  mSetObj$imgSet$tree <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(cex=0.8, mar=c(4,2,2,8));
  if(mSetObj$dataSet$cls.type == "disc"){
    clusDendro <- as.dendrogram(hc_tree);
    cols <- GetColorSchema(mSetObj$dataSet$cls);
    names(cols) <- rownames(hc.dat);
    labelColors <- cols[hc_tree$order];
    colLab <- function(n){
      if(is.leaf(n)) {
        a <- attributes(n)
        labCol <- labelColors[a$label];
        attr(n, "nodePar") <- 
          if(is.list(a$nodePar)) c(a$nodePar, lab.col = labCol,pch=NA) else
            list(lab.col = labCol,pch=NA)
      }
      n
    }
    clusDendro <- dendrapply(clusDendro, colLab)
    plot(clusDendro,horiz=T,axes=T);
    par(cex=1);
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      legend.nm <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
    }else{
      legend.nm <- as.character(mSetObj$dataSet$cls);
    }
    
    legend("topleft", legend = unique(legend.nm), pch=15, col=unique(cols), bty = "n");
    
  }else{
    plot(as.dendrogram(hc_tree), hang=-1, main=paste("Cluster with", clstDist, "method"), xlab=NULL, sub=NULL, horiz=TRUE);
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'SOM analysis
#'@description SOM analysis
#'@param mSetObj Input name of the created mSet Object
#'@param x.dim Input X dimension for SOM analysis
#'@param y.dim Input Y dimension for SOM analysis
#'@param initMethod Input the method 
#'@param neigb Default is set to 'gaussian'
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SOM.Anal <- function(mSetObj=NA, x.dim, y.dim, initMethod, neigb = 'gaussian'){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$som <- som::som(as.matrix(mSetObj$dataSet$norm), xdim=x.dim, ydim=y.dim, init=initMethod, neigh=neigb);
  return(.set.mSet(mSetObj));
}

#'SOM Plot
#'@description Plot SOM map for  less than 20 clusters
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param colpal Character, input "default" to use the default ggplot color scheme or "colblind" to use
#'the color-blind friendly palette.
#'@param facet logical
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import ggplot2
#'@export
#'
PlotSOM <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, colpal = "default", facet=TRUE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
    load_data.table()
  } else {
    require(ggplot2);
    require(data.table);
  }
  
  xdim <- mSetObj$analSet$som$xdim;
  ydim <- mSetObj$analSet$som$ydim;
  
  total<-xdim*ydim;
  if(total>20) { return();}
  
  ylabel<-GetAbundanceLabel(mSetObj$dataSet$type);
  clust<-mSetObj$analSet$som$visual;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  if(xdim > 5){
    w <- w + 7.5
  }else if(xdim > 3){
    w <- w + 5
  }else if(xdim > 1){
    w <- w + 3
  }
  
  mSetObj$imgSet$som <- imgName;
  
  ### NEW PLOTS
  group <- paste(clust[,1], clust[,2], sep = "_")
  clust.num <- length(unique(group))
  
  df <- data.frame(Samples = as.factor(rownames(mSetObj$dataSet$norm)), Cluster = group, clust[,-3], mSetObj$dataSet$norm, check.names = F)
  long <- melt(setDT(df), id.vars = c("Samples", "Cluster", "x", "y"), variable.name = "Feature")
  long.dt <- setDT(long)[, ymax:= max(value), by=list(Feature, Cluster)]
  long.dt <- setDT(long.dt)[, ymin:= min(value), by=list(Feature, Cluster)]
  long.dt <- setDT(long.dt)[, median:= median(value), by=list(Feature, Cluster)]
  long.dt <- setDF(long.dt)
  
  p <- ggplot(long.dt, aes(x = Feature, y = median, group = Cluster, color = Cluster)) + 
    geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=Cluster), linetype=0, alpha = 0.25) + 
    geom_line() + xlab("") + ylab("Concentration") + guides(x =  guide_axis(angle = 90)) +
    theme(
      # Remove panel border
      panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "grey"))
  
  if(facet){
    p <- p + facet_grid(y ~ x)
  }
  
  if(colpal == "colblind"){
    if(clust.num <= 18){ # update color and respect default
      dist.cols <- cb_pal_18[1:clust.num];
    }else{
      dist.cols <- colorRampPalette(cb_pal_18)(clust.num);
    }
    p <- p + scale_fill_manual(values=dist.cols) + scale_color_manual(values=dist.cols)
  }
  
  if(xdim < 3){
    p <- p + theme(text = element_text(size=10))
  }else{
    p <- p + theme(text = element_text(size=8))
  }
  
  ggsave(p, filename = imgName, dpi=dpi, width=w, height=h, limitsize = FALSE)
  
  return(.set.mSet(mSetObj));
}

#'K-means analysis
#'@description Perform K-means analysis
#'@param mSetObj Input name of the created mSet Object
#'@param clust.num Numeric, input the number of clusters for K-means analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Kmeans.Anal <- function(mSetObj=NA, clust.num){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$kmeans <- kmeans(mSetObj$dataSet$norm, clust.num, nstart=100);
  return(.set.mSet(mSetObj));
}

#'Plot K-means analysis
#'@description Plot K-means analysis
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param colpal Character, input "default" to use the default ggplot color scheme or "colblind" to use
#'the color-blind friendly palette.
#'@param facet logical, TRUE to plot in multiple facets
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotKmeans <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, colpal="default", facet=FALSE){

  if(.on.public.web){
    load_ggplot()
    load_data.table()
  } else {
    require(ggplot2);
    require(data.table);
  }
  
  mSetObj <- .get.mSet(mSetObj);
  clust.num <- max(mSetObj$analSet$kmeans$cluster);
  
  if(clust.num>20) return();
  # calculate arrangement of panel
  ylabel<- GetAbundanceLabel(mSetObj$dataSet$type);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  mSetObj$imgSet$kmeans <- imgName;
  
  df <- data.frame(Samples = as.factor(rownames(mSetObj$dataSet$norm)), Cluster = as.factor(mSetObj$analSet$kmeans$cluster), mSetObj$dataSet$norm, check.names = F)
  long <- melt(setDT(df), id.vars = c("Samples","Cluster"), variable.name = "Feature")
  long.dt <- setDT(long)[, ymax:= max(value), by=list(Feature, Cluster)]
  long.dt <- setDT(long.dt)[, ymin:= min(value), by=list(Feature, Cluster)]
  long.dt <- setDT(long.dt)[, median:= median(value), by=list(Feature, Cluster)]
  long.dt <- setDF(long.dt)
  
  p <- ggplot(long.dt, aes(x = Feature, y = median, group = Cluster, color = Cluster)) + 
    geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=Cluster), linetype=0, alpha = 0.25) + 
    geom_line() + xlab("") + ylab("Concentration") + guides(x =  guide_axis(angle = 90)) +
    theme(
      # Remove panel border
      panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "grey")
    )
  
  if(facet){
    p <- p + facet_grid(Cluster ~ .)
  }
  
  if(colpal == "colblind"){
    if(clust.num <= 18){ # update color and respect default
      dist.cols <- cb_pal_18[1:clust.num];
    }else{
      dist.cols <- colorRampPalette(cb_pal_18)(clust.num);
    }
    p <- p + scale_fill_manual(values=dist.cols) + scale_color_manual(values=dist.cols)
  }

  ggsave(p, filename = imgName, dpi=dpi, width=w, height=h, limitsize = FALSE)
  
  return(.set.mSet(mSetObj));
}

#'Plot K-means summary PCA plot
#'@description Plot K-means summary PCA plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param colpal palete of color
#'@param anal analysis type
#'@param labels labels to show, default is "T"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import ggplot2
#'@export

PlotClustPCA <- function(mSetObj, 
                         imgName, 
                         format="png", 
                         dpi=72, 
                         width=NA, 
                         colpal="default", 
                         anal="km", 
                         labels = "T"){
  
  if(.on.public.web){
    load_ggplot()
    load_data.table()
  }

  mSetObj <- .get.mSet(mSetObj);

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(anal == "km"){
    clust.num <- max(mSetObj$analSet$kmeans$cluster);
    clusters <- as.factor(mSetObj$analSet$kmeans$cluster)
    mSetObj$imgSet$kmeans.pca <- imgName;
  }else{
    clust<-mSetObj$analSet$som$visual;
    clusters <- paste(clust[,1], clust[,2], sep = "_")
    clust.num <- length(unique(clusters))
    mSetObj$imgSet$som.pca <- imgName;
  }
  
  if(clust.num>20) return();
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  data <- mSetObj$dataSet$norm
  
  # Set class labels
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  res.pca <- stats::prcomp(data, scale = FALSE, center = TRUE)
  
  sum.pca <- summary(res.pca);
  imp.pca <- sum.pca$importance;
  var.pca <- imp.pca[2,];
  
  df_out <- as.data.frame(res.pca$x)
  df_out$Cluster <- clusters
  df_out$Class <- cls
  
  text.lbls <- substr(names(res.pca$x[,1]), 1, 14)
  
  xlabel = paste("PC",1, "(", round(100*var.pca[1],1), "%)");
  ylabel = paste("PC",2, "(", round(100*var.pca[2],1), "%)");
  
  p <- ggplot(df_out, aes(x=PC1, y=PC2, color=Class))
  p <- p + geom_point(size = 5) + xlab(xlabel) + ylab(ylabel) + theme_bw() 
  p <- p + stat_ellipse(geom = "polygon", alpha = 0.15, aes(group = Cluster, fill = Cluster))
  
  if(colpal == "colblind"){
    if(clust.num <= 18){ # update color and respect default
      dist.cols <- cb_pal_18[1:clust.num];
    }else{
      dist.cols <- colorRampPalette(cb_pal_18)(clust.num);
    }
    p <- p + scale_fill_manual(values = dist.cols) + scale_color_manual(values = dist.cols)
  }

  if(labels == "T"){
    p <- p + geom_text(aes(label = text.lbls), vjust="inward", hjust="inward")
  }
  
  ggsave(p, filename = imgName, dpi=dpi, width=w, height=h, limitsize = FALSE)
  return(.set.mSet(mSetObj));
}

#'Create Sub Heat Map Plot
#'@description Plot a sub heatmap based on results from t-tests/ANOVA, VIP or randomforest
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param dataOpt Set data options
#'@param scaleOpt Set the image scale
#'@param smplDist Input the sample distance method
#'@param clstDist Input the clustering distance method
#'@param palette Input color palette choice
#'@param method.nm Input the method for sub-heat map
#'@param top.num Input the top number
#'@param viewOpt Set heatmap options, default is set to "detail"
#'@param rowV Default is set to T
#'@param colV Default is set to T
#'@param border Indicate whether or not to show cell-borders, default is set to T
#'@param grp.ave Logical, default is set to F
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSubHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, dataOpt, scaleOpt, 
                           smplDist, clstDist, palette,fzCol,fzRow,fzAnno,annoPer, unitCol,unitRow,  method.nm, top.num,  rowV=T, colV=T, border=T, grp.ave=F, show.legend=T, show.annot.legend=T, showColnm=T, showRownm=T){
  
  mSetObj <- .get.mSet(mSetObj);
  var.nms = colnames(mSetObj$dataSet$norm);
  if(top.num < length(var.nms)){
    if(method.nm == 'tanova'){
      if(GetGroupNumber(mSetObj) == 2){
        if(is.null(mSetObj$analSet$tt)){
          Ttests.Anal(mSetObj);
          mSetObj <- .get.mSet(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$tt$p.value))[1:top.num];
      }else{
        if(is.null(mSetObj$analSet$aov)){
          ANOVA.Anal(mSetObj);
          mSetObj <- .get.mSet(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$aov$p.value))[1:top.num];
      }
    }else if(method.nm == 'vip'){
      if(is.null(mSetObj$analSet$plsr)){
        PLSR.Anal(mSetObj);
        #PLSDA.CV(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      vip.vars <- mSetObj$analSet$plsr$vip.mat[,1];# use the first component
      var.nms <- names(rev(sort(vip.vars)))[1:top.num];
    }else if(method.nm == 'rf'){
      if(is.null(analSet$rf)){
        RF.Anal(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      var.nms <- GetRFSigRowNames()[1:top.num];
    }else{ # mean or iqr
      filt.res <- PerformFeatureFilter(mSetObj$dataSet$norm, method.nm, top.num, mSetObj$analSet$type)$data;
      var.nms <- colnames(filt.res);
    }
  }
  var.inx <- match(var.nms, colnames(mSetObj$dataSet$norm));
  PlotHeatMap(mSetObj, imgName, format, dpi, width, dataOpt, scaleOpt, smplDist, clstDist, palette, fzCol,fzRow,fzAnno,annoPer, unitCol,unitRow, rowV, colV, var.inx, border, grp.ave, show.legend, show.annot.legend, showColnm,showRownm);
}

#'Create Heat Map Plot
#'@description Plot a heatmap based on results from t-tests/ANOVA, VIP or randomforest
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param dataOpt Set data options
#'@param scaleOpt Set the image scale
#'@param smplDist Input the sample distance method
#'@param clstDist Input the clustering distance method
#'@param palette Input color palette choice
#'@param viewOpt Set heatmap options, default is set to "detail"
#'@param rowV Default is set to T
#'@param colV Default is set to T
#'@param var.inx Default is set to NA
#'@param border Indicate whether or not to show cell-borders, default is set to T
#'@param grp.ave Logical, default is set to F
#'@param metadata metadata
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'
PlotHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, 
                        width=NA, dataOpt, scaleOpt, smplDist, 
                        clstDist, palette,  fzCol,fzRow,fzAnno,annoPer, unitCol,unitRow, rowV=T, 
                        colV=T, var.inx=NULL, border=T, grp.ave=F, show.legend=T, show.annot.legend=T, showColnm=T, showRownm=T, maxFeature=2000){

  mSetObj <- .get.mSet(mSetObj);
  plotjs <- paste0(imgName, ".json");
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  mSetObj$imgSet$heatmap <- imgName;
  require(iheatmapr);
  
  
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
  ordered.cls <- is.ordered(cls);

  # record the paramters
  mSetObj$analSet$htmap <- list(dist.par=smplDist, clust.par=clstDist);
  
  # set up data set
  if(dataOpt=="norm"){
    my.data <- mSetObj$dataSet$norm;
  }else{
    my.data <- qs::qread("prenorm.qs");
  }
  
  if(is.null(var.inx)){
    hc.dat<-as.matrix(my.data);
  }else{
    hc.dat<-as.matrix(my.data[,var.inx]);
  }
  
  # need to control for very large data plotting
  if(ncol(hc.dat) > 1000 ){
     includeRowNames <- FALSE;
  }
  if(.on.public.web){
    if(ncol(hc.dat) > maxFeature){
        filter.val <- apply(hc.dat, 2, IQR, na.rm=T);
        rk <- rank(-filter.val, ties.method='random');
        hc.dat <- hc.dat[,rk <= maxFeature];
        print(paste("Data is reduced to max allowed vars based on IQR: ", maxFeature));
    }
  }

  colnames(hc.dat) <- substr(colnames(hc.dat),1,18) # some names are too long
  
  if(!ordered.cls && cls.class == "integer"){
    hc.cls <- as.factor(as.numeric(levels(cls))[cls]);
  }else{
    hc.cls <- cls;
  }
  
  # set up colors for heatmap
    if(palette=="gbr"){
        colors <- grDevices::colorRampPalette(c("green", "black", "red"), space="rgb")(256);
    }else if(palette == "heat"){
        colors <- grDevices::heat.colors(256);
    }else if(palette == "topo"){
        colors <- grDevices::topo.colors(256);
    }else if(palette == "gray"){
        colors <- grDevices::colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
    }else if(palette == "byr"){
        colors <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(256));
    }else if(palette == "viridis") {
        colors <- rev(viridis::viridis(10))
    }else if(palette == "plasma") {
        colors <- rev(viridis::plasma(10))
    }else if(palette == "npj"){
        colors <- c("#00A087FF","white","#E64B35FF")
    }else if(palette == "aaas"){
        colors <- c("#4DBBD5FF","white","#E64B35FF");
    }else if(palette == "d3"){
        colors <- c("#2CA02CFF","white","#FF7F0EFF");
    }else {
        colors <- c("#0571b0","#92c5de","white","#f4a582","#ca0020");
    }

  if(cls.type == "disc"){
    annotation <- data.frame(class = hc.cls);
    rownames(annotation) <- rownames(hc.dat); 
  }else{
    annotation <- NA;
  }
 
  
  # make the width smaller for group average
 
  if(border){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }
 
  
    if(ordered.cls){
        ord.inx <- order(hc.cls);
        hc.cls <- hc.cls[ord.inx];
        hc.dat <- hc.dat[ord.inx,];
    }else if(cls.class =="integer"){
        hc.cls <- as.factor(as.numeric(levels(hc.cls))[hc.cls]);
    }

    # set up color schema for samples
    cols <- GetColorSchema(hc.cls, palette == "gray");
    uniq.cols <- unique(cols);    
    names(uniq.cols) <- unique(as.character(hc.cls));
    ann_colors <- list(class= uniq.cols);
  
    if(grp.ave){ # only use group average
        lvs <- levels(cls);
        my.mns <- matrix(ncol=ncol(hc.dat),nrow=length(lvs));
        for(i in 1:length(lvs)){
            my.mns[i,]<- colMeans(hc.dat[hc.cls==lvs[i], ]);
        }
        rownames(my.mns) <- lvs;
        colnames(my.mns) <- colnames(hc.dat);
        hc.dat <- my.mns;
        hc.cls <- as.factor(lvs);
        annotation <- data.frame(class = hc.cls);
        rownames(annotation) <- rownames(hc.dat); 
    }
    annotation$class <- factor(annotation$class, levels = rev(levels(annotation$class)))
    data1sc <- t(hc.dat)
    data1sc <- scale_mat(data1sc, scaleOpt)
    data1sc = round(data1sc,5)

    dend_row <- hclust(dist(data1sc, method = smplDist), method = clstDist)
    w = min(1200,ncol(data1sc)*unitCol+50)
    h = min(2000,nrow(data1sc)*unitRow+50)
    sz <- max(as.numeric(annoPer) / 100, 0.015)
    bf <- min(0.01, (sz / 3))

   if(grp.ave){
        
    p <- iheatmap(data1sc,  name = "value", x_categorical = TRUE,
                  layout = list(font = list(size = fzAnno)),
                  colors = colors,
                    colorbar_grid = setup_colorbar_grid(x_start = 2) 
    )
      w = w+300
   }else{
      
    p <- iheatmap(data1sc,  name = "value", x_categorical = TRUE,
                  layout = list(font = list(size = fzAnno),showlegend=F),
                  colors = colors ,
            colorbar_grid =setup_colorbar_grid(nrows = min(15, round(nrow(data1sc)*unitRow/140)), x_start = 1.1, y_start = 0.85, x_spacing = 0.15))
    }

    p<- p%>%
      add_col_annotation(annotation, show_colorbar=show.annot.legend,
                         side = "top", size = annoPer , buffer = bf , inner_buffer = bf / 3
      )

      
    if (showColnm) {
      p <- p%>%
      add_col_labels(size = 0.2, font = list(size = fzCol))
    } 
    
    
    if (showRownm) {
      p <- p%>%
        add_row_labels(size = 0.2, font = list(size = fzRow), side = "right") 
    } 
    
    
    if (rowV) {
    dend_col <- hclust(dist(t(data1sc), method = smplDist), method = clstDist)
    p <- p %>% add_col_dendro(dend_col)
    } 

  if (colV) {
    dend_row <- hclust(dist(data1sc, method = smplDist), method = clstDist)
    p <- p %>% add_row_dendro(dend_row)  
  } 
    
      if(ncol(data1sc)<100){
         w=w+(100-ncol(data1sc))*6
      
        }
    
     if(nrow(data1sc)<100){
         h=h+(100-nrow(data1sc))*5
      
        }

    mSetObj$imgSet$heatmap_stats_param <- list();
    mSetObj$imgSet$heatmap_stats_param$width <- w;
    mSetObj$imgSet$heatmap_stats_param$height <- h;

    # Adjust the height and width (in pixels)
    saveRDS(p, "heatmap_stats.rds")

    as_list <- to_plotly_list(p)
    as_list[["layout"]][["width"]] <- w
    as_list[["layout"]][["height"]] <- h
 
  #  as_list[["layout"]][["annoHeight"]] <- round(0.1*h, 1)
    as_json <- attr(as_list, "TOJSON_FUNC")(as_list)
    as_json <- paste0("{ \"x\":", as_json, ",\"evals\": [],\"jsHooks\": []}")
 
    print(plotjs)
    write(as_json, plotjs)


  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'SOM analysis
#'@description Get members for given cluster index, return a character string
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param i Index of X
#'@param j Index of Y
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetSOMClusterMembers <- function(mSetObj=NA, i, j){
  mSetObj <- .get.mSet(mSetObj);
  clust <- mSetObj$analSet$som$visual;
  xTrue <- clust$x == i;
  yTrue <- clust$y == j;
  hit.inx <- xTrue & yTrue;
  
  all.cols <- GetColorSchema(mSetObj$dataSet$cls);
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
}

#'SOM analysis
#'@description Get members for given cluster index, return a character string
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
GetAllSOMClusterMembers <- function(mSetObj=NA){
  
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
        clust.df <- rbind(paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse = " "));
        rowNameVec <- c(paste("Cluster(", i, ",", j,")"));
      }else{
        clust.df <- rbind(clust.df, paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse=" "));
        rowNameVec <- c(rowNameVec, paste("Cluster(", i, ",", j,")"));
      }
      j = j+1;
    }
    i = i+1;
  }
  row.names(clust.df) <- rowNameVec;
  colnames(clust.df) <- "Samples in each cluster";
  print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using SOM"),caption.placement="top", size="\\scriptsize");
}


# inx has to be 1 or 2
GetClassLabel<-function(mSetObj=NA, inx){
  mSetObj <- .get.mSet(mSetObj);
  levels(mSetObj$dataSet$cls)[inx]
}


#'K-means analysis - cluster
#'@description Get the cluster members for given index
#'add HTML color to the names based on its group membership
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param i Input the cluster index
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetKMClusterMembers <- function(mSetObj=NA, i){
  mSetObj <- .get.mSet(mSetObj);
  all.cols <- GetColorSchema(mSetObj$dataSet$cls);
  hit.inx <- mSetObj$analSet$kmeans$cluster== i;
  
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
  # paste(all.cols[hit.inx], rownames(dataSet$norm)[hit.inx], collapse =", ");
}

#'K-means analysis - cluster
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetAllKMClusterMembers <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  clust.df = data.frame();
  rowNameVec = c();
  i = 1;
  clust.num<-max(mSetObj$analSet$kmeans$cluster);
  while(i<=clust.num){
    if(i==1){
      clust.df <- rbind(paste(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = " "));
    }else{
      clust.df <- rbind(clust.df,paste(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = " "));
    }
    rowNameVec <- c(rowNameVec, paste("Cluster(", i, ")"));
    i = i+1;
  }
  row.names(clust.df) <- rowNameVec;
  colnames(clust.df) <-"Samples in each cluster";
  print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using K-means"), caption.placement="top", size="\\scriptsize");
}

########## Utility Functions ##############
#'Determine row/column number for plotting
#'@description Determine the number of rows and columns for a given total
#'number of plots (used by Kmeans and SOM plots)
#'@param total Input the total
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetXYCluster<-function(total){
  if(total>16){
    ncol<-4;
    nrow<-5;
  }else if(total>12){
    ncol<-4;
    nrow<-4;
  }else if(total>9){
    ncol<-3;
    nrow<-4;
  }else if(total>6){
    ncol<-3;
    nrow<-3;
  }else if(total>4){
    ncol<-2;
    nrow<-3;
  }else{
    ncol<-1;
    nrow<-total;
  }
  c(nrow, ncol);
}

scale_mat = function(mat, scale){
  if(!(scale %in% c("none", "row", "column"))){
    stop("scale argument shoud take values: 'none', 'row' or 'column'")
  }
  mat = switch(scale, none = mat, row = scale_rows(mat), column = t(scale_rows(t(mat))))
  return(mat)
} 
scale_rows = function(x){
  m = apply(x, 1, mean, na.rm = T)
  s = apply(x, 1, sd, na.rm = T)
  return((x - m) / s)
}