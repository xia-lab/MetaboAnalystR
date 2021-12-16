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
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
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
                           smplDist, clstDist, palette, method.nm, top.num, viewOpt, rowV=T, colV=T, border=T, grp.ave=F){
  
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
    }else if(method.nm == 'cor'){
      if(is.null(mSetObj$analSet$cor.res)){
        Match.Pattern(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      
      # re-order for pretty view
      cor.res <- mSetObj$analSet$cor.res;
      
      ord.inx<-order(cor.res[,3]);
      cor.res <- cor.res[ord.inx, ];
      
      ord.inx<-order(cor.res[,1]);
      cor.res <- cor.res[ord.inx, ];
      
      var.nms <- rownames(cor.res)[1:top.num];
    }else if(method.nm == 'vip'){
      if(is.null(mSetObj$analSet$plsda)){
        PLSR.Anal(mSetObj);
        PLSDA.CV(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      vip.vars <- mSetObj$analSet$plsda$vip.mat[,1];# use the first component
      var.nms <- names(rev(sort(vip.vars)))[1:top.num];
    }else if(method.nm == 'rf'){
      if(is.null(analSet$rf)){
        RF.Anal(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      var.nms <- GetRFSigRowNames()[1:top.num];
    }else{ # mean or iqr
      filt.res <- PerformFeatureFilter(mSetObj$dataSet$norm, method.nm, top.num, NULL)$data;
      var.nms <- colnames(filt.res);
    }
  }
  var.inx <- match(var.nms, colnames(mSetObj$dataSet$norm));
  PlotHeatMap(mSetObj, imgName, format, dpi, width, dataOpt, scaleOpt, smplDist, clstDist, palette, viewOpt, rowV, colV, var.inx, border, grp.ave);
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
                        clstDist, palette, viewOpt="detail", rowV=T, 
                        colV=T, var.inx=NULL, border=T, grp.ave=F, metadata){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$heatmap <- imgName;

  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
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
  
  colnames(hc.dat) <- substr(colnames(hc.dat),1,18) # some names are too long
  
  if(cls.class == "integer"){
    hc.cls <- as.factor(as.numeric(levels(cls))[cls]);
  }else{
    hc.cls <- cls;
  }
  
  if(grp.ave){ # only use group average
    lvs <- levels(hc.cls);
    my.mns <- matrix(ncol=ncol(hc.dat),nrow=length(lvs));
    for(i in 1:length(lvs)){
      inx <-hc.cls == lvs[i];
      my.mns[i,]<- apply(hc.dat[inx, ], 2, mean);
    }
    rownames(my.mns) <- lvs;
    colnames(my.mns) <- colnames(hc.dat);
    hc.dat <- my.mns;
    hc.cls <- as.factor(lvs);
  }
  
  # set up colors for heatmap
  if(palette=="gbr"){
    colors <- colorRampPalette(c("green", "black", "red"), space="rgb")(256);
  }else if(palette == "heat"){
    colors <- heat.colors(256);
  }else if(palette == "topo"){
    colors <- topo.colors(256);
  }else if(palette == "gray"){
    colors <- colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
  }else{
    colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256));
  }

  if(cls.type == "disc"){
    annotation <- data.frame(class = hc.cls);
    rownames(annotation) <- rownames(hc.dat); 
  }else{
    annotation <- NA;
  }
  # compute size for heatmap
  plot_dims <- get_pheatmap_dims(t(hc.dat), annotation, viewOpt, width);
  h <- plot_dims$height;
  w <- plot_dims$width;
  
  # make the width smaller fro group average
  if(grp.ave){
    w <- nrow(hc.dat)*25 + 300;
    w <- round(w/72,2);
  }
  
  if(border){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }

  if(format=="pdf"){
    pdf(file = imgName, width=w, height=h, bg="white", onefile=FALSE);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  }
  if(cls.type == "disc"){    
    # set up color schema for samples
    cols <- GetColorSchema(cls, palette == "gray");
    uniq.cols <- unique(cols);

    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(cls))[cls]);
    }else{
      cls <- cls;
    }
    
    names(uniq.cols) <- unique(as.character(sort(cls)));
    ann_colors <- list(class= uniq.cols);
  
    pheatmap::pheatmap(t(hc.dat), 
             annotation=annotation, 
             annotation_colors = ann_colors,
             fontsize=8, fontsize_row=8, 
             clustering_distance_rows = smplDist,
             clustering_distance_cols = smplDist,
             clustering_method = clstDist, 
             border_color = border.col,
             cluster_rows = colV, 
             cluster_cols = rowV,
             scale = scaleOpt, 
             color = colors);
  }else{
    heatmap(hc.dat, Rowv = rowTree, Colv=colTree, col = colors, scale="column");
  }
  dev.off();
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