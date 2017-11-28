#'Plot Dendrogram
#'@description Dendogram
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param smplDist  
#'@param clstDist 
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
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(cex=0.8, mar=c(4,2,2,8));
  if(mSetObj$dataSet$cls.type == "disc"){
    clusDendro<-as.dendrogram(hc_tree);
    cols <- GetColorSchema(mSetObj);
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
    clusDendro<-dendrapply(clusDendro, colLab)
    plot(clusDendro,horiz=T,axes=T);
    par(cex=1);
    legend.nm <- as.character(mSetObj$dataSet$cls);
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
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SOM.Anal <- function(mSetObj=NA, x.dim, y.dim, initMethod, neigb = 'gaussian'){
  mSetObj <- .get.mSet(mSetObj);
  library(som);
  mSetObj$analSet$som <- som(as.matrix(mSetObj$dataSet$norm), xdim=x.dim, ydim=y.dim, init=initMethod, neigh=neigb);
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
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSOM <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  xdim<-mSetObj$analSet$som$xdim;
  ydim<-mSetObj$analSet$som$ydim;
  total<-xdim*ydim;
  if(total>20) { return();}
  
  ylabel<-GetValueLabel(mSetObj);
  clust<-mSetObj$analSet$som$visual;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  mSetObj$imgSet$som <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow = GetXYCluster(total), mar=c(5,4,2,2));
  for (i in 0:(xdim-1)) {
    xTrue<-clust$x == i;
    for (j in 0:(ydim-1)) {
      yTrue<-clust$y == j;
      sel.inx<-xTrue & yTrue; # selected row
      if(sum(sel.inx)>0){ # some cluster may not contain any member
        matplot(t(mSetObj$dataSet$norm[sel.inx, ]), type="l", col='grey', axes=F, ylab=ylabel,
                main=paste("Cluster(", i, ",", j,")", ", n=", sum(sel.inx), sep=""))
        lines(apply(mSetObj$dataSet$norm[sel.inx, ], 2, median), type="l", col='blue', lwd=1);
      }else{ # plot a dummy 
        plot(t(mSetObj$dataSet$norm[1, ]), type="n", axes=F, ylab=ylabel,
             main=paste("Cluster(", i, ",", j,")",", n=", sum(sel.inx),sep=""))
      }
      axis(2);
      axis(1, 1:ncol(mSetObj$dataSet$norm), substr(colnames(mSetObj$dataSet$norm), 1, 7), las=2);
    }
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'K-means analysis
#'@description Perform K-means analysis
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Kmeans.Anal <- function(mSetObj=NA, clust.num){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$kmeans <- kmeans (mSetObj$dataSet$norm, clust.num, nstart=100);
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
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotKmeans <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  clust.num <- max(mSetObj$analSet$kmeans$cluster);
  
  if(clust.num>20) return();
  # calculate arrangement of panel
  ylabel<-GetValueLabel(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  mSetObj$imgSet$kmeans <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow = GetXYCluster(clust.num), mar=c(5,4,2,2));
  for (loop in 1:clust.num) {
    matplot(t(mSetObj$dataSet$norm[mSetObj$analSet$kmeans$cluster==loop,]), type="l", col='grey', ylab=ylabel, axes=F,
            main=paste("Cluster ",loop, ", n=", mSetObj$analSet$kmeans$size[loop], sep=""))
    lines(apply(mSetObj$dataSet$norm[mSetObj$analSet$kmeans$cluster==loop,], 2, median), type="l", col='blue', lwd=1);
    axis(2);
    axis(1, 1:ncol(mSetObj$dataSet$norm), substr(colnames(mSetObj$dataSet$norm), 1, 7), las=2);
  }
  dev.off();
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
      if(GetGroupNumber() == 2){
        if(is.null(mSetObj$analSet$tt)){
          Ttests.Anal(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$tt$p.value))[1:top.num];
      }else{
        if(is.null(mSetObj$analSet$aov)){
          ANOVA.Anal(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$aov$p.value))[1:top.num];
      }
    }else if(method.nm == 'cor'){
      if(is.null(mSetObj$analSet$cor.res)){
        Match.Pattern(mSetObj);
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
      }
      vip.vars <- mSetObj$analSet$plsda$vip.mat[,1];# use the first component
      var.nms <- names(rev(sort(vip.vars)))[1:top.num];
    }else if(method.nm == 'rf'){
      if(is.null(analSet$rf)){
        RF.Anal(mSetObj);
      }
      var.nms <- GetRFSigRowNames()[1:top.num];
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
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, dataOpt, scaleOpt, smplDist, 
                        clstDist, palette, viewOpt="detail", rowV=T, colV=T, var.inx=NA, border=T, grp.ave=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # record the paramters
  mSetObj$analSet$htmap <- list(dist.par=smplDist, clust.par=clstDist);
  
  # set up data set
  if(dataOpt=="norm"){
    my.data <- mSetObj$dataSet$norm;
  }else{
    my.data <- mSetObj$dataSet$procr;
  }
  
  if(is.na(var.inx)){
    hc.dat<-as.matrix(my.data);
  }else{
    hc.dat<-as.matrix(my.data[,var.inx]);
  }
  
  colnames(hc.dat) <- substr(colnames(hc.dat),1,18) # some names are too long
  hc.cls <- mSetObj$dataSet$cls;
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
    suppressMessages(library(RColorBrewer));
    colors <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256));
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    minW <- 630;
    myW <- nrow(hc.dat)*18 + 150;
    if(myW < minW){
      myW <- minW;
    }   
    w <- round(myW/72,2);
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- 7.2;
    
  }
  
  mSetObj$imgSet$heatmap <- imgName;
  
  myH <- ncol(hc.dat)*18 + 150;
  h <- round(myH/72,2);
  
  if(viewOpt == "overview"){
    if(is.na(width)){
      if(w > 9){
        w <- 9;
      }
    }else if(width == 0){
      if(w > 7.2){
        w <- 7.2;
      }
      
    }else{
      w <- 7.2;
    }
    if(h > w){
      h <- w;
    }
    
    mSetObj$imgSet$heatmap <- imgName;
    
  }
  
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
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  }
  
  if(mSetObj$dataSet$cls.type == "disc"){
    require(pheatmap);
    annotation <- data.frame(class= hc.cls);
    rownames(annotation) <-rownames(hc.dat); 
    
    # set up color schema for samples
    if(palette== "gray"){
      cols <- GetColorSchema(mSetObj, T);
      uniq.cols <- unique(cols);
    }else{
      cols <- GetColorSchema(mSetObj);
      uniq.cols <- unique(cols);
    }
    names(uniq.cols) <- unique(as.character(mSetObj$dataSet$cls));
    ann_colors <- list(class= uniq.cols);
    
    pheatmap(t(hc.dat), 
             annotation=annotation, 
             fontsize=8, fontsize_row=8, 
             clustering_distance_rows = smplDist,
             clustering_distance_cols = smplDist,
             clustering_method = clstDist, 
             border_color = border.col,
             cluster_rows = colV, 
             cluster_cols = rowV,
             scale = scaleOpt, 
             color = colors,
             annotation_colors = ann_colors
    );
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
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetSOMClusterMembers <- function(mSetObj=NA, i, j){
  mSetObj <- .get.mSet(mSetObj);
  clust<-mSetObj$analSet$som$visual;
  xTrue<-clust$x == i;
  yTrue<-clust$y == j;
  hit.inx <- xTrue & yTrue;
  
  all.cols <- GetColorSchema(mSetObj);
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
}

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
  row.names(clust.df)<- rowNameVec;
  colnames(clust.df)<-"Samples in each cluster";
  print(xtable(clust.df, align="l|p{8cm}", caption="Clustering result using SOM"),caption.placement="top", size="\\scriptsize");
}


# inx has to be 1 or 2
GetClassLabel<-function(mSetObj=NA, inx){
  mSetObj <- .get.mSet(mSetObj);
  levels(mSetObj$dataSet$cls)[inx]
}


#'K-means analysis - cluster
#'@description Get the cluster members for given index
#'add HTML color to the names based on its group membership
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetKMClusterMembers <- function(mSetObj=NA, i){
  mSetObj <- .get.mSet(mSetObj);
  all.cols <- GetColorSchema(mSetObj);
  hit.inx <- mSetObj$analSet$kmeans$cluster== i;
  
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
  # paste(all.cols[hit.inx], rownames(dataSet$norm)[hit.inx], collapse =", ");
}

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
  row.names(clust.df)<- rowNameVec;
  colnames(clust.df)<-"Samples in each cluster";
  print(xtable(clust.df, align="l|p{8cm}", caption="Clustering result using K-means"), caption.placement="top", size="\\scriptsize");
}