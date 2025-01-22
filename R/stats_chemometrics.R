#'Perform PCA analysis
#'@description Perform PCA analysis, obtain variance explained, store item to PCA object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'@param mSetObj Input name of the created mSet Object
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PCA.Anal <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  # RhpcBLASctl::blas_set_num_threads(2);
  # RhpcBLASctl::omp_set_num_threads(2);
  pca <- prcomp(mSetObj$dataSet$norm, center=TRUE, scale=F);
 
  # obtain variance explained
  sum.pca <- summary(pca);
  imp.pca <- sum.pca$importance;
  std.pca <- imp.pca[1,]; # standard devietation
  var.pca <- imp.pca[2,]; # variance explained by each PC
  cum.pca <- imp.pca[3,]; # cummulated variance explained

  # store the item to the pca object
  mSetObj$analSet$pca<-append(pca, list(std=std.pca, variance=var.pca, cum.var=cum.pca));
  fast.write.csv(signif(mSetObj$analSet$pca$x,5), file="pca_score.csv");
  fast.write.csv(signif(mSetObj$analSet$pca$rotation,5), file="pca_loadings.csv");
  mSetObj$analSet$pca$loading.type <- "all";
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
  #return(length(pca[["center"]]));
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

#'Rotate PCA analysis
#'@description Rotate PCA analysis
#'@param mSetObj Input name of the created mSet Object
#'@param axisOpt Input the axis option 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PCA.Flip <- function(mSetObj=NA, axisOpt){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pca<-mSetObj$analSet$pca;
  # store the item to the pca object
  if(axisOpt == "x"){
    pca$x[,1] <- -pca$x[,1];
    pca$rotation[,1] <- -pca$rotation[,1];
  }else if(axisOpt == "y"){
    pca$x[,2] <- -pca$x[,2];
    pca$rotation[,2] <- -pca$rotation[,2];
  }else{ # all
    pca$x <- -pca$x;
    pca$rotation <- -pca$rotation;
  }
  fast.write.csv(signif(pca$x,5), file="pca_score.csv");
  fast.write.csv(signif(pca$rotation,5), file="pca_loadings.csv");
  
  mSetObj$analSet$pca <- pca;
  return(.set.mSet(mSetObj));
}

#'Plot PCA pair summary, format image in png, tiff, pdf, ps, svg
#'@description Rotate PCA analysis
#'@usage PlotPCAPairSummary(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, input a number to indicate the number of principal components to display in the pairwise score plot.
#'@export
#'
PlotPCAPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  mSetObj <- .get.mSet(mSetObj);
  pclabels <- paste("PC", 1:pc.num, "\n", round(100*mSetObj$analSet$pca$variance[1:pc.num],1), "%");
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pca.pair <- imgName;
  Plot.PairScatter(mSetObj$analSet$pca$x[,1:pc.num], pclabels, mSetObj$dataSet$cls, mSetObj$dataSet$cls.type, imgName, format, dpi, width);
  return(.set.mSet(mSetObj));
}

#'Plot PCA scree plot
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param scree.num Numeric, input a number to indicate the number of principal components to display in the scree plot.
#'@usage PlotPCAScree(mSetObj=NA, imgName, format="png", dpi=72, width=NA, scree.num)
#'@export
#'
PlotPCAScree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, scree.num){
  
  mSetObj <- .get.mSet(mSetObj);
  
  stds <-mSetObj$analSet$pca$std[1:scree.num];
  pcvars<-mSetObj$analSet$pca$variance[1:scree.num];
  cumvars<-mSetObj$analSet$pca$cum.var[1:scree.num];
  
  ylims <- range(c(pcvars,cumvars));
  extd<-(ylims[2]-ylims[1])/10
  miny<- ifelse(ylims[1]-extd>0, ylims[1]-extd, 0);
  maxy<- ifelse(ylims[2]+extd>1, 1.0, ylims[2]+extd);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*2/3;
  
  mSetObj$imgSet$pca.scree <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,6,3));
  plot(pcvars, type='l', col='blue', main='Scree plot', xlab='PC index', ylab='Variance explained', ylim=c(miny, maxy), axes=F)
  text(pcvars, labels =paste(100*round(pcvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(pcvars, col='red');
  
  lines(cumvars, type='l', col='green')
  text(cumvars, labels =paste(100*round(cumvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(cumvars, col='red');
  
  abline(v=1:scree.num, lty=3);
  axis(2);
  axis(1, 1:length(pcvars), 1:length(pcvars));
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Create 2D PCA score plot
#'@description Rotate PCA analysis
#'@usage PlotPCA2DScore(mSetObj=NA, imgName, format="png", 
#'dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pcx Specify the principal component on the x-axis
#'@param pcy Specify the principal component on the y-axis
#'@param reg Numeric, input a number between 0 and 1, 0.95 will 
#'display the 95 percent confidence regions, and 0 will not.
#'@param show Display sample names, 1 = show names, 0 = do not show names.
#'@param grey.scale Use grey-scale colors, 1 = grey-scale, 0 = not grey-scale.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPCA2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, 
                           width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0, cex.opt="na"){

  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    pca.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    pca.cex <<- pca.cex + 0.1;
  }else{
    pca.cex <<- pca.cex - 0.1;
  }

  mSetObj <- .get.mSet(mSetObj);
  
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  
  xlabel = paste("PC",pcx, "(", round(100*mSetObj$analSet$pca$variance[pcx],1), "%)");
  ylabel = paste("PC",pcy, "(", round(100*mSetObj$analSet$pca$variance[pcy],1), "%)");
  pc1 = mSetObj$analSet$pca$x[, pcx];
  pc2 = mSetObj$analSet$pca$x[, pcy];
  text.lbls<-substr(names(pc1),1,14) # some names may be too long
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # margin big
  
  mSetObj$imgSet$pca.score2d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op<-par(mar=c(5,5,3,8));
  
  if(cls.type == "disc"){
    # obtain ellipse points to the scatter plot for each category
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(cls))[cls]);
    }else{
      cls <- cls;
    }
    
    lvs <- levels(cls);
    pts.array <- array(0, dim=c(100,2,length(lvs)));
    for(i in 1:length(lvs)){
      inx <- cls == lvs[i];
      groupVar<-var(cbind(pc1[inx],pc2[inx]), na.rm=T);
      groupMean<-cbind(mean(pc1[inx], na.rm=T),mean(pc2[inx], na.rm=T));
      pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
    }
    
    xrg <- range(pc1, pts.array[,1,]);
    yrg <- range(pc2, pts.array[,2,]);
    x.ext<-(xrg[2]-xrg[1])/12;
    y.ext<-(yrg[2]-yrg[1])/12;
    xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
    ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
    
    col.def <- GetColorSchema(cls, grey.scale==1);
    cols <- ExpandSchema(cls, col.def);
    
    plot(pc1, pc2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot",
         col=cols, pch=as.numeric(cls)+1); ## added
    grid(col = "lightgray", lty = "dotted", lwd = 1);
    
    # draw ellipse
    for(i in 1:length(lvs)){
      if (length(col.def) > 1) {
        polygon(pts.array[,,i], col=adjustcolor(col.def[lvs[i]], alpha=0.2), border=NA);
      } else {
        polygon(pts.array[,,i], col=adjustcolor(col.def, alpha=0.2), border=NA);
      }
      if(grey.scale) {
        lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
      }
    }
    
    pchs.def <- GetShapeSchema(cls, show, grey.scale);
    pchs <- ExpandSchema(cls, pchs.def);
    if(grey.scale) {
      cols <- rep("black", length(cols));
    }
    if(show == 1){
      text(pc1, pc2, label=text.lbls, pos=4, xpd=T, cex=0.75*pca.cex);
      points(pc1, pc2, pch=pchs, col=cols);
    }else{
      if(length(col.def) == 1){
        points(pc1, pc2, pch=pchs, col=cols, cex=1.0*pca.cex);
      }else{
        if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
          my.cols <- adjustcolor(cols, alpha.f = 0.4);
          my.cols[pchs == 21] <- "black";
          points(pc1, pc2, pch=pchs, col=my.cols, bg=adjustcolor(cols, alpha.f = 0.4), cex=1.8*pca.cex);
        }else{
          points(pc1, pc2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2*pca.cex);
        }
      }
    }
    
    # after the fix above
    if(grey.scale) {
      col.def <- "black";
    }

    axis.lims <- par("usr"); # x1, x2, y1 ,y2
    shift <- par("cxy")[1];
    lgd.x <- axis.lims[2] + shift;
    lgd.y <- axis.lims[4] - shift;
    par(xpd=T);
    legend(lgd.x, lgd.y, legend = names(pchs.def), pch=pchs.def, col=col.def, box.lty=0);
    
  }else{
    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, type='n', main="Scores Plot");
    points(pc1, pc2, pch=15, col="magenta");
    text(pc1, pc2, label=text.lbls, pos=4, col ="blue", xpd=T, cex=0.8*pca.cex);
  }
  par(op);
  dev.off();

  permanova_results <- ComputePERMANOVA(pc1, pc2, mSetObj$dataSet$cls, 999)
  mSetObj$analSet$pca$permanova.res <-permanova_results;
  return(.set.mSet(mSetObj));
}

#'Create 3D PCA score plot
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCA3DScore(mSetObj=NA, imgName, format="json", inx1, inx2, inx3)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@export
#'
PlotPCA3DScore <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
  pca <-  mSetObj$analSet$pca;
  pca3d <- list();
  pca3d$score$axis <- paste("PC", c(inx1, inx2, inx3), " (", 100*round( mSetObj$analSet$pca$variance[c(inx1, inx2, inx3)], 3), "%)", sep="");
  coords <- data.frame(t(signif(pca$x[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pca3d$score$xyz <- coords;
  pca3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(cls.class == "integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(cls))[cls])));
  }else{
    cls <- as.character(cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pca3d$score$facA <- cls;
  
  # now set color for each group
  col.def <- GetColorSchema(as.factor(cls));
  pca3d$score$colors <- my.col2rgb(col.def);
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- rjson::toJSON(pca3d);
  sink(imgName);
  cat(json.obj);
  sink();

  #for util_scatter3d
  qs::qsave(pca3d$score, "score3d.qs");

  if(!.on.public.web){
    return(.set.mSet(mSetObj));
  }
}

#' PlotPCA3DLoading
#' @param mSetObj mSetObj
#' @param imgName imgName
#' @param format format
#' @param inx1 inx1
#' @param inx2 inx2
#' @param inx3 inx3
#'
#'@export
PlotPCA3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
  pca <- mSetObj$analSet$pca
  pca3d <- list();
  
  pca3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");
  coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
  
  dists <- GetDist3D(coords);
  
  # color based on dist;
  pca3d$loading$cols <- GetRGBColorGradient(dists);
  
  # size based on dist
  # pca3d$loading$sizes <- GetSizeGradient(dists);
  colnames(coords) <- NULL; 
  pca3d$loading$xyz <- coords;
  pca3d$loading$name <- rownames(pca$rotation);
  pca3d$loading$entrez <-rownames(pca$rotation); 
  
  if(cls.class=="integer"){
    clss <- as.character(sort(as.factor(as.numeric(levels(cls))[cls])));
  }else{
    clss <- as.character(cls);
  }
  
  if(all.numeric(clss)){
    clss <- paste("Group", clss);
  }
  
  pca3d$cls = clss;
  # see if there is secondary
  
  imgName = paste(imgName, ".", format, sep="");
  json.mat <- rjson::toJSON(pca3d);
  sink(imgName);
  cat(json.mat);
  sink();
  AddMsg("Annotated data is now ready for PCA 3D visualization!");

  pca3d$loading$cls = cls;
  qs::qsave(pca3d$loading, "loading3d.qs");

  if(!exists("my.json.scatter")){
    .load.scripts.on.demand("util_scatter3d.Rc");    
  }

  my.json.scatter(imgName, T);

  if(.on.public.web){
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

#'Update PCA loadings
#'@description Update the PCA loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdatePCA.Loading<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$pca$loading.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}

#'Plot PCA loadings and also set up the matrix for display
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCALoading(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@export
#'
PlotPCALoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  
  loadings<-as.matrix(cbind(mSetObj$analSet$pca$rotation[,inx1],mSetObj$analSet$pca$rotation[,inx2]));
  # sort based on absolute values of 1, 2 
  ord.inx <- order(-abs(loadings[,1]), -abs(loadings[,2]));
  loadings <- signif(loadings[ord.inx,],5);
  
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2);
  colnames(loadings)<-c(ldName1, ldName2);
  mSetObj$analSet$pca$imp.loads<-loadings; # set up the loading matrix
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.loading <- imgName;
  plotType <- mSetObj$analSet$pca$loading.type;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(6,5,2,6));
  plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);
  
  mSetObj$pca.axis.lims <- par("usr"); # x1, x2, y1 ,y2
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  points(loadings[,1],loadings[,2], pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  
  if(plotType=="all"){
    text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 16), pos=4, col="blue", xpd=T);
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- rownames(loadings) %in% mSetObj$custom.cmpds;
      text(loadings[hit.inx,1],loadings[hit.inx,2], labels=rownames(loadings)[hit.inx], pos=4, col="blue", xpd=T);
    }
  }else{
    # do nothing
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
  
}

#'Create PCA Biplot, set xpd = T to plot outside margin
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCABiplot(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@export
#'
PlotPCABiplot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2,topnum=10){
  print(topnum)
  mSetObj <- .get.mSet(mSetObj);
 
  choices <- c(inx1, inx2);
  scores <- mSetObj$analSet$pca$x;
  lam <- mSetObj$analSet$pca$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.biplot<-imgName;

 library(ggplot2)
 library(ggrepel)
 library(dplyr)
 library(factoextra) 
  pca <- prcomp(mSetObj$dataSet$norm, scale=F);
 
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
   if(mSetObj$dataSet$type.cls.lbl=="integer"){
     cls <- as.factor(as.numeric(levels(cls))[cls]);
   }else{
      cls <- cls;
  }
 
  contrib <- get_pca_var(pca)$contrib
contrib_pc1_pc2 <- contrib[, 1] + contrib[, 2]

top_10_features <- names(sort(contrib_pc1_pc2, decreasing = TRUE))[1:topnum]
top_10_features <- top_10_features[!is.na(top_10_features)]

ind_data <- data.frame(
  PC1 = scores[, choices[1]] / lam[1],
  PC2 = scores[, choices[2]] / lam[2],
  Group = cls
)
 
loadings <- pca$rotation
var_data <- data.frame(
  PC1 = loadings[, choices[1]] * lam[1],
  PC2 = loadings[, choices[2]] * lam[2],
  Variable = rownames(loadings)
)%>% 
  filter(Variable %in% top_10_features)

scaling_factor <- max(abs(ind_data$PC1), abs(ind_data$PC2)) / max(abs(var_data$PC1), abs(var_data$PC2))
 
var_data$PC1 <- var_data$PC1 * scaling_factor
var_data$PC2 <- var_data$PC2 * scaling_factor

group_names <- levels(ind_data$Group)  
group_palette <- setNames(ggsci::pal_npg("nrc")(length(group_names)), group_names)

 cols <- GetColorSchema(cls); 
 
p <- ggplot() +
  geom_point(data = ind_data, aes(x = PC1, y = PC2, color = Group), size =3.5, alpha = 0.7) +
  stat_ellipse(data = ind_data, aes(x = PC1, y = PC2, fill = Group, color = Group), type = "norm", level = 0.95, geom = "polygon", alpha = 0.2) +
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "black", arrow = arrow(length = unit(0.2, "cm")), size = 0.5, show.legend = FALSE) +
  geom_text_repel(data = var_data, aes(x = PC1, y = PC2, label = Variable),
                  size = 4, color = "#3B3B3B", max.overlaps = Inf, segment.color = "#636363") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  labs(title = "Biplot",
       x = paste("PC", inx1, " (", round(summary(pca)$importance[2, inx1] * 100, 2), "%)", sep = ""),
       y = paste("PC", inx2, " (", round(summary(pca)$importance[2, inx2] * 100, 2), "%)", sep = "")) +
  theme_minimal() +
 scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
   legend.text = element_text(size = 12), 
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a black border around the plot
    panel.grid.major = element_line(color = "grey80",linetype = "dashed"),  # Lighten the grid lines if needed
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  )+
  coord_cartesian(clip = "off")+
  coord_fixed(ratio = 1)  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(p );
  dev.off();

  return(.set.mSet(mSetObj));
}

#'PLS analysis using oscorespls (Orthogonal scores algorithm)
#'so that VIP can be calculated
#'note: the VIP is calculated only after PLSDA-CV is performed
#'to determine the best # of comp. used for VIP
#'@description PLS analysis using oscorespls
#'@param mSetObj Input name of the created mSet Object
#'@param reg Logical
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PLSR.Anal <- function(mSetObj=NA, reg=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  comp.num <- dim(mSetObj$dataSet$norm)[1]-1;
  
  if(comp.num > 8) {
    #need to deal with small number of predictors
    comp.num <- min(dim(mSetObj$dataSet$norm)[2], 8)
  } else if(comp.num < 8) {
    AddMsg(paste("Too few features in your data to do PLS-DA analysis! "));
  }
  
  if(.on.public.web){
    load_pls()
  }
  
  # note, standardize the cls, to minimize the impact of categorical to numerical impact
  if(reg){
    cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls <- model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  pls.res <- pls::plsr(cls~datmat, method='oscorespls', ncomp=comp.num);
  mSetObj$analSet$plsr <- pls.res;
  mSetObj$analSet$plsr$reg <- reg;
  mSetObj$analSet$plsr$loading.type <- "all";
  mSetObj$analSet$plsr$vip.mat <- .calculate.pls.vip(pls.res, comp.num);
  mSetObj$custom.cmpds <- c();
  
  fast.write.csv(signif(mSetObj$analSet$plsr$scores,5), row.names=rownames(mSetObj$dataSet$norm), file="plsda_score.csv");
  fast.write.csv(signif(mSetObj$analSet$plsr$loadings,5), file="plsda_loadings.csv");
  return(.set.mSet(mSetObj));
}

#'Plot PLS pairwise summary
#'@description Plot PLS pairwise summary
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, indicate the number of principal components
#'@export

PlotPLSPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pls.pair <- imgName;
  vars <- round(100*mSetObj$analSet$plsr$Xvar[1:pc.num]/mSetObj$analSet$plsr$Xtotvar,1);
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%"); 
  Plot.PairScatter(mSetObj$analSet$plsr$scores[,1:pc.num], pclabels, mSetObj$dataSet$cls, mSetObj$dataSet$cls.type, imgName, format, dpi, width);
  return(.set.mSet(mSetObj));
}

#'Plot PLS score plot
#'@description Plot PLS score plot
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric, default is 0.95
#'@param show Show labels, 1 or 0
#'@param grey.scale Numeric, use a grey scale (0) or not (1)
#'@export
#'
PlotPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    pls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    pls.cex <<- pls.cex + 0.1;
  }else{
    pls.cex <<- pls.cex - 0.1;
  }

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # margin big
  
  
  cls1 <- mSetObj$dataSet$cls
  cls.type <- mSetObj$dataSet$cls.type
  
  mSetObj$imgSet$pls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$plsr$scores[,inx1];
  lv2 <- mSetObj$analSet$plsr$scores[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$plsr$Xvar[inx1]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$plsr$Xvar[inx2]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,8));
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  
  if(cls.type=="integer"){
    cls <- as.factor(as.numeric(levels(cls1))[cls1]);
  }else{
    cls <- cls1;
  }
  
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- cls1 == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
 
  col.def <- GetColorSchema(cls, grey.scale==1);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);

  # draw ellipse
  for(i in 1:length(lvs)){
    if (length(col.def) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(col.def[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(col.def, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale);
  pchs <- ExpandSchema(cls, pchs.def);

  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75*pls.cex);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(col.def) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0*pls.cex);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        my.cols <- adjustcolor(cols, alpha.f = 0.4);
        my.cols[pchs == 21] <- "black";
        points(lv1, lv2, pch=pchs, col=my.cols, bg=adjustcolor(cols, alpha.f = 0.4), cex=1.8*pls.cex);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2*pls.cex);
      }
    }
  }
  
  
  if(grey.scale) {
    col.def <- "black";
  }
  
  
  op <- par(xpd=T);
  # move legend to outside 
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  shift <- par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  lgd.y <- axis.lims[4] - shift;
  legend(lgd.x, lgd.y, legend = names(pchs.def), pch=pchs.def, col=col.def, box.lty=0);
  
  par(op);
  dev.off();


  return(.set.mSet(mSetObj));
}

#'Plot PLS 3D score plot
#'@description Plot PLS 3D score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS3DScore <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  
  cls1 <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
  pls3d <- list();
  pls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$plsr$Xvar[c(inx1, inx2, inx3)]/mSetObj$analSet$plsr$Xtotvar, 1), "%)", sep="");
  coords <- data.frame(t(signif(mSetObj$analSet$plsr$score[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pls3d$score$xyz <- coords;
  pls3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(cls1))[cls1])));
  }else{
    cls <- as.character(cls1);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pls3d$score$facA <- cls;
  
  # now set color for each group
  col.def <- GetColorSchema(cls1);
  pls3d$score$colors <- my.col2rgb(col.def);
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- rjson::toJSON(pls3d);
  sink(imgName);
  cat(json.obj);
  sink();
  mSet$imgSet$pls.score3d <- imgName;

  qs::qsave(pls3d$score, "score3d.qs");

  return(.set.mSet(mSetObj));
}

#' PlotPLS3DLoading
#' @param mSetObj mSetObj
#' @param imgName imgName
#' @param format format
#' @param inx1 inx1
#' @param inx2 inx2
#' @param inx3 inx3
#'
#'@export
PlotPLS3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  pls = mSetObj$analSet$plsr
  coords<-signif(as.matrix(cbind(pls$loadings[,inx1],pls$loadings[,inx2],pls$loadings[,inx3])),5);
  pls3d <- list();
  
  pls3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");
  coords0 <- coords <- data.frame(t(signif(pls$loadings[,c(inx1, inx2, inx3)], 5)));
  
  colnames(coords) <- NULL; 
  pls3d$loading$xyz <- coords;
  pls3d$loading$name <- rownames(pls$loadings);
  pls3d$loading$entrez <-rownames(pls$loadings); 
  
  dists <- GetDist3D(coords0);
  cols <- GetRGBColorGradient(dists);
  pls3d$loading$cols <- cols;
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pls3d$cls = cls;
  # see if there is secondary
  
  imgName <- paste(imgName, ".", format, sep="");
  mSetObj$imgSet$pls.score3d <- imgName;

  json.mat <- rjson::toJSON(pls3d);
  sink(imgName);
  cat(json.mat);
  sink();
  AddMsg("Annotated data is now ready for PCA 3D visualization!");
  
  pls3d$loading$cls = cls;
  qs::qsave(pls3d$loading, "loading3d.qs");

  if(!exists("my.json.scatter")){
    .load.scripts.on.demand("util_scatter3d.Rc");    
  }

  my.json.scatter(imgName, T);

  if(.on.public.web){
    mSet <<- mSetObj;
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

#'Update PLS loadings
#'@description Update the PLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdatePLS.Loading<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$plsr$loading.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}


#'Plot PLS loading plot, also set the loading matrix for display
#'@description Plot PLS loading plot, also set the loading matrix for display
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5. The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  # named vector
  load1<-mSetObj$analSet$plsr$loadings[,inx1];
  load2<-mSetObj$analSet$plsr$loadings[,inx2];
  loadings = as.matrix(cbind(load1, load2));
  # sort based on absolute values of 1, 2 
  ord.inx <- order(-abs(loadings[,1]), -abs(loadings[,2]));
  loadings <- signif(loadings[ord.inx,],5);
  
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2)
  colnames(loadings)<-c(ldName1, ldName2);
  mSetObj$analSet$plsr$imp.loads<-loadings; # set up loading matrix
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.loading <- imgName;
  plotType <- mSetObj$analSet$plsr$loading.type;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(6,4,4,5));
  plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);
  
  mSetObj$pls.axis.lims <- par("usr"); # x1, x2, y1 ,y2
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  points(loadings[,1],loadings[,2], pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  
  if(plotType=="all"){
    text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 16), pos=4, col="blue", xpd=T);
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
      text(loadings[hit.inx,1],loadings[hit.inx,2], labels=rownames(loadings)[hit.inx], pos=4, col="blue", xpd=T);
    }
  }else{
    # do nothing
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#' PLS-DA classification and feature selection
#' @description PLS-DA classification and feature selection
#' @param mSetObj Input name of the created mSet Object
#' @param methodName Logical, by default set to TRUE
#' @param compNum GetDefaultPLSCVComp()
#' @param choice Input the choice, by default it is Q2
#' @importFrom caret train varImp plsda R2
#' @author Jeff Xia\email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
PLSDA.CV <- function(mSetObj=NA, cvOpt="loo", foldNum=5, compNum=GetDefaultPLSCVComp(mSetObj), choice="Q2", segments = 10){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_caret()
  }
  
  # get classification accuracy using caret
  plsda.cls <- caret::train(mSetObj$dataSet$norm, mSetObj$dataSet$cls, "pls", trControl=caret::trainControl(method=ifelse(cvOpt == 'loo', "LOOCV", 'CV'), number=foldNum), tuneLength=compNum);
  
  # note, for regression, use model matrix
  if(mSetObj$analSet$plsr$reg){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  
  # use the classical regression to get R2 and Q2 measure
  # https://github.com/xia-lab/MetaboAnalystR/issues/66

  smpl.size <- length(mSetObj$dataSet$cls);
  if(smpl.size >10){
    plsda.reg <- pls::plsr(cls~datmat,method ='oscorespls', ncomp=compNum, validation= ifelse(cvOpt == 'loo', "LOO", 'CV'));
  }else{
    plsda.reg <- pls::plsr(cls~datmat,method ='oscorespls', ncomp=compNum, validation= "LOO");
  }

  require(pls);
  fit.info <- pls::R2(plsda.reg, estimate = "all")$val[,1,];
  
  # combine accuracy, R2 and Q2
  accu <- plsda.cls$results[,2]
  all.info <- rbind(accu, fit.info[,-1]);
  
  rownames(all.info) <- c("Accuracy", "R2", "Q2");
  
  # default use best number determined by Q2
  if(choice == 'Q2'){
    best.num <- which(all.info[3,] == max(all.info[3,]));
  }else if(choice == "R2"){
    best.num <- which(all.info[2,] == max(all.info[2,]));
  }else{
    best.num <- which(all.info[1,] == max(all.info[1,]));
  }
  
  # get coef. table, this can be error when class is very unbalanced
  coef.mat <- try(caret::varImp(plsda.cls, scale=T)$importance);
  if(class(coef.mat) == "try-error") {
    coef.mat <- NULL;
  }else{
    if(mSetObj$dataSet$cls.num > 2){ # add an average coef for multiple class
      coef.mat <- cbind(coef.mean = rowMeans(coef.mat), coef.mat);
    }
    # rearange in decreasing order, keep as matrix, prevent dimesion dropping if only 1 col
    inx.ord <- order(coef.mat[,1], decreasing=T);
    coef.mat <- data.matrix(coef.mat[inx.ord, ,drop=FALSE]);
    fast.write.csv(signif(coef.mat,5), file="plsda_coef.csv"); # added 27 Jan 2014
  }
  
  mSetObj$analSet$plsda<-list(best.num=best.num, choice=choice, coef.mat=coef.mat, fit.info=all.info);
  return(.set.mSet(mSetObj));
}

######### biplot for PLSDA
########################
PlotPLSBiplot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2,topnum=10){
  print(topnum)
  print(imgName)
  mSetObj <- .get.mSet(mSetObj);
  plsr <- mSetObj$analSet$plsr
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pls.biplot <- imgName;
    
  choices <- c(inx1, inx2);
  scores <- plsr$scores;
  lam <- apply(scores[,choices], 2, sd)
  n <- NROW(scores)
  lam <- lam * sqrt(n);
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
   
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(factoextra) 
 
  
  cls <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(cls))[cls]);
  }else{
    cls <- cls;
  }
  
  contrib <-  plsr$vip.mat 
  contrib_pc1_pc2 <- contrib[, 1] + contrib[, 2]
  
  top_features <- names(sort(contrib_pc1_pc2, decreasing = TRUE))[1:topnum]
  top_features <- top_features[!is.na(top_features)]
  
  ind_data <- data.frame(
    PC1 = scores[, choices[1]] / lam[1],
    PC2 = scores[, choices[2]] / lam[2],
    Group = cls
  )
  
  loadings <-  plsr$loadings 
  var_data <- data.frame(
    PC1 = loadings[, choices[1]],
    PC2 = loadings[, choices[2]] ,
    Variable = rownames(loadings)
  )%>% 
    filter(Variable %in% top_features)
  
  scaling_factor <- max(abs(ind_data$PC1), abs(ind_data$PC2)) / max(abs(var_data$PC1), abs(var_data$PC2))
  
  var_data$PC1 <- var_data$PC1 * scaling_factor
  var_data$PC2 <- var_data$PC2 * scaling_factor
 
  cols <- GetColorSchema(cls); 
  
  p <- ggplot() +
    geom_point(data = ind_data, aes(x = PC1, y = PC2, color = Group), size =3.5, alpha = 0.7) +
    stat_ellipse(data = ind_data, aes(x = PC1, y = PC2, fill = Group, color = Group), type = "norm", level = 0.95, geom = "polygon", alpha = 0.2) +
    geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 color = "black", arrow = arrow(length = unit(0.2, "cm")), size = 0.5, show.legend = FALSE) +
    geom_text_repel(data = var_data, aes(x = PC1, y = PC2, label = Variable),
                    size = 4, color = "#3B3B3B", max.overlaps = Inf, segment.color = "#636363") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    labs(title = "Biplot",
         x = paste("Component", inx1, " (", round(100*plsr$Xvar[inx1]/plsr$Xtotvar,1), "%)", sep = ""),
         y = paste("Component", inx2, " (", round(100*plsr$Xvar[inx2]/plsr$Xtotvar,1), "%)", sep = "")) +
    theme_minimal() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 11, color = "black"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 12), 
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a black border around the plot
      panel.grid.major = element_line(color = "grey80",linetype = "dashed"),  # Lighten the grid lines if needed
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  )+
    coord_cartesian(clip = "off")+
    coord_fixed(ratio = 1)  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(p );
  dev.off();
   return(.set.mSet(mSetObj));
}

########################################
# calculate VIP http://mevik.net/work/software/VIP.R
#####################
.calculate.pls.vip <- function(plsr.res, compNum){

  pls <- plsr.res;
  b <- c(pls$Yloadings)[1:compNum];
  T <- pls$scores[,1:compNum, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- pls$loading.weights[,1:compNum, drop = FALSE]
  Wnorm2 <- colSums(W^2);
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
  if(compNum > 1){
    vip.mat <- as.matrix(t(vips));
    ord.inx <- order(-abs(vip.mat[,1]), -abs(vip.mat[,2]));
  }else{
    vip.mat <- as.matrix(vips);
    ord.inx <- order(-abs(vip.mat[,1]));
  }
  vip.mat <- vip.mat[ord.inx,];
  colnames(vip.mat) <- paste("Comp.", 1:ncol(vip.mat));
  
  fast.write.csv(signif(vip.mat,5),file="plsda_vip.csv");
  return(vip.mat);
}

#'Perform PLS-DA permutation
#'@description Perform PLS-DA permutation using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param num Numeric, input the number of permutations
#'@param type Type of accuracy, if "accu" indicate prediction accuracy, else "sep" is separation distance
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PLSDA.Permut <- function(mSetObj=NA, num=100, type="accu"){
  
  mSetObj <- .get.mSet(mSetObj);

  orig.cls <- cls <- as.numeric(mSetObj$dataSet$cls);
  datmat <- as.matrix(mSetObj$dataSet$norm);
  best.num <- mSetObj$analSet$plsda$best.num;
  
  # dummy is not used, for the purpose to maintain lapply API
  Get.pls.bw <- function(dummy){
    cls <- cls[order(runif(length(cls)))];
    pls <- caret::plsda(datmat, as.factor(cls), ncomp=best.num);
    pred <- predict(pls, datmat);
    Get.bwss(pred, cls);
  }
  
  Get.pls.accu <- function(dummy){
    cls <- cls[order(runif(length(cls)))];
    pls <- caret::plsda(datmat, as.factor(cls), ncomp=best.num);
    pred <- predict(pls, datmat);
    sum(pred == cls)/length(cls);
  }
  
  # first calculate the bw values with original labels
  pls <- caret::plsda(datmat, as.factor(orig.cls), ncomp=best.num);
  pred.orig <- predict(pls, datmat);
  if(type=="accu"){
    perm.type = "prediction accuracy";
    res.orig <- sum(pred.orig == orig.cls)/length(orig.cls);
    res.perm <- Perform.permutation(num, Get.pls.accu);
  }else{
    perm.type = "separation distance";
    res.orig <- Get.bwss(pred.orig, orig.cls);
    res.perm <- Perform.permutation(num, Get.pls.bw);
  }
  # perm.num may be adjusted on public server
  perm.num <- res.perm$perm.num;
  perm.res <- res.perm$perm.res;
  perm.vec <- c(res.orig, unlist(perm.res, use.names=FALSE));
  # check for infinite since with group variance could be zero for perfect classification
  inf.found = TRUE;
  if(sum(is.finite(perm.vec))==length(perm.vec)){
    inf.found = FALSE;
  }else {
    if(sum(is.finite(perm.vec))==0){ # all are infinite, give a random number 10
      perm.vec<-rep(10, length(perm.vec));
    }else{ # if not all inf, replace with the 10 fold of non-inf values
      perm.vec[!is.finite(perm.vec)]<-10*max(perm.vec[is.finite(perm.vec)]);
    }
  }
  
  # calculate the significant p value as the proportion of sampled permutations better than or equal to original one
  # note, the precision is determined by the permutation number i.e. for 100 time, no better than original
  # p value is < 0.01, we can not say it is zero
  better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
  if(better.hits == 0) {
    p <- paste("p < ", 1/perm.num, " (", better.hits, "/", perm.num, ")", sep="");
  }else{
    p <- better.hits/perm.num;
    p <- paste("p = ", signif(p, digits=5), " (", better.hits, "/", perm.num, ")", sep="");
  }
  
  mSetObj$analSet$plsda$permut.p <- p;
  mSetObj$analSet$plsda$permut.inf <- F;
  mSetObj$analSet$plsda$permut.type <- perm.type;
  mSetObj$analSet$plsda$permut <- perm.vec;
  
  msg <- paste("Empirical p value:", p);
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(msg)
  }else{
    print(msg);
    return(.set.mSet(mSetObj));
  }
}

checkCVperformed <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  best.num <- mSetObj$analSet$plsda$best.num;
  if(is.null(best.num)){
    return(0)
  } else {
    return(1)
  }
}

#'Plot PLS important features
#'@description Plot PLS important features, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param type Indicate the type variables of importance to use, "vip" to use VIp scores, or "type"
#'for coefficients  
#'@param feat.nm Feature name
#'@param feat.num Feature numbers
#'@param color.BW Logical, true to use black and white, or false to not
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS.Imp <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, type, feat.nm, feat.num, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w-1; # margin is big
  
  mSetObj$imgSet$pls.imp<-imgName;

  # re-adjust width based on group size
  cls.len <- length(levels(mSetObj$dataSet$cls));
  rt.mrg <- cls.len + 3;
  w <- w + rt.mrg/72; # convert to inch

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  op <- par(mar=c(5,7,3,rt.mrg)); # update right side margin with the number of class

  if(type=="vip"){
    mSetObj$analSet$plsda$imp.type <- "vip";
    vips <- mSetObj$analSet$plsr$vip.mat[,feat.nm];
    PlotImpVar(mSetObj, vips, "VIP scores", feat.num, color.BW);
  }else{
    mSetObj$analSet$plsda$imp.type <- "coef";
    data<-mSetObj$analSet$plsda$coef.mat[,feat.nm];
    PlotImpVar(mSetObj, data, "Coefficients", feat.num, color.BW);
  }
  par(op);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Plot PLS important variables,
#'@description Plot PLS important variables, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imp.vec Input the vector of important variables
#'@param xlbl Input the x-label
#'@param feat.num Numeric, set the feature numbers, default is set to 15
#'@param color.BW Use black-white for plot (T) or colors (F)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotImpVar <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  
  if(feat.num <= 0){
    feat.num = 15;
  }
  
  if(feat.num > length(imp.vec)){
    feat.num <- length(imp.vec);
  }
  
  # first get the top subset
  imp.vec <- rev(sort(imp.vec))[1:feat.num];
  
  # reverser the order for display
  imp.vec <- sort(imp.vec);
  
  # as data should already be normalized, use mean/median should be the same
  # mns is a list contains means of all vars at each level
  # conver the list into a matrix with each row contains var averages across different lvls
  mns <- by(mSetObj$dataSet$norm[, names(imp.vec)], mSetObj$dataSet$cls,
            function(x){ # inner function note, by send a subset of dataframe
              apply(x, 2, mean, trim=0.1)
            });
  mns <- t(matrix(unlist(mns), ncol=feat.num, byrow=TRUE));
  
  # vip.nms <-substr(names(imp.vec), 1, 12);
  vip.nms <- substr(names(imp.vec), 1, 14);
  names(imp.vec) <- NULL;
  
  # modified for B/W color
  dotcolor <- ifelse(color.BW, "darkgrey", "#585855");
  dotchart(imp.vec, bg=dotcolor, xlab= xlbl, cex=1.3);
  
  mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)
  
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  # get character width
  shift <- 2*par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  
  x <- rep(lgd.x, feat.num);
  y <- 1:feat.num;
  par(xpd=T);
  
  nc <- ncol(mns);
  
  # modified for B/W color
  colorpalette <- ifelse(color.BW, "Greys", "RdYlBu");
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, colorpalette))(nc); # set colors for each class
  if(color.BW) col <- rev(col);
  
  # calculate background
  bg <- matrix("", nrow(mns), nc);
  for (m in 1:nrow(mns)){
    bg[m,] <- (col[nc:1])[rank(mns[m,])];
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  cls.lbl <- levels(cls);
  
  for (n in 1:ncol(mns)){
    points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
    # now add label
    text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
    # shift x, note, this is good for current size
    x <- x + shift/1.25;
  }
  
  # now add color key, padding with more intermediate colors for continuous band
  # https://github.com/xia-lab/MetaboAnalystR/issues/49
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, colorpalette))(50)
  if(color.BW) col <- rev(col);
  
  nc <- length(col);
  x <- rep(x[1] + shift, nc);
  
  shifty <- (axis.lims[4]-axis.lims[3])/3;
  starty <- axis.lims[3] + shifty;
  endy <- axis.lims[3] + 2*shifty;
  y <- seq(from = starty, to = endy, length = nc);
  
  points(x,y, bty="n", pch=15, col=rev(col), cex=2);
  
  text(x[1], endy+shifty/8, "High");
  text(x[1], starty-shifty/8, "Low");
  
}

#'Plot PLS-DA classification performance using different components
#'@description Plot plsda classification performance using different components
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
PlotPLS.Classification <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$plsda$fit.info;
  colnames(res) <- 1:ncol(res);
  best.num <- mSetObj$analSet$plsda$best.num;
  choice <- mSetObj$analSet$plsda$choice;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*5/7;
  
  mSetObj$imgSet$pls.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,7)); # put legend on the right outside
  barplot(res, beside = TRUE, col = c("lightblue", "mistyrose","lightcyan"), ylim= c(0,1.05), xlab="Number of components", ylab="Performance");
  
  if(choice == "Q2"){
    text((best.num-1)*3 + best.num + 2.5, res[3,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }else if(choice == "R2"){
    text((best.num-1)*3 + best.num + 1.5, res[2,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }else{
    text((best.num-1)*3 + best.num + 0.5, res[1,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }
  
  # calculate the maximum y position, each bar is 1, place one space between the group
  xpos <- ncol(res)*3 + ncol(res) + 1;
  legend(xpos, 1.0, rownames(res), fill = c("lightblue", "mistyrose","lightcyan"), xpd=T);
  dev.off();
  return(.set.mSet(mSetObj));
}


#'Plot PLS-DA classification performance using different components, permutation
#'@description Plot plsda classification performance using different components
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
PlotPLS.Permutation <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  bw.vec <- mSetObj$analSet$plsda$permut;
  len <- length(bw.vec);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$pls.permut <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,4));
  hst <- hist(bw.vec, breaks = "FD", freq=T,
              ylab="Frequency", xlab= 'Permutation test statistics', col="lightblue", main="");
  
  # add the indicator using original label
  h <- max(hst$counts)
  arrows(bw.vec[1], h/5, bw.vec[1], 0, col="red", lwd=2);
  text(bw.vec[1], h/3.5, paste('Observed \n statistic \n', mSetObj$analSet$plsda$permut.p), xpd=T);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA
#'@description Orthogonal PLS-DA (from ropls)
#'Add reg (regression i.e. if class order matters)
#'@param mSetObj Input name of the created mSet Object
#'@param reg Logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
OPLSR.Anal<-function(mSetObj=NA, reg=FALSE){
  mSetObj <- .prepare.oplsr.anal(mSetObj, reg);
  .perform.computing();
  .save.oplsr.anal(mSetObj);
}

.prepare.oplsr.anal <-function(mSetObj=NA, reg=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$analSet$opls.reg <- reg;  
  
  # default options for feature labels on splot
  mSetObj$custom.cmpds <- c();
  mSetObj$analSet$oplsda$splot.type <- "all";
  
  if(reg==TRUE){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 

  my.fun <- function(){
    if(file.exists("../../rscripts/MetaboAnalystR/R/stats_opls.Rc")){
        compiler::loadcmp("../../rscripts/MetaboAnalystR/R/stats_opls.Rc");
    }
    my.res <- perform_opls(dat.in$data, dat.in$cls, predI=1, permI=0, orthoI=NA, crossvalI=dat.in$cv.num);
    return(my.res);
  }
  
  dat.in <- list(data=datmat, cls=cls, cv.num=cv.num, my.fun=my.fun);
  
  qs::qsave(dat.in, file="dat.in.qs");
  return(.set.mSet(mSetObj));
}

.save.oplsr.anal <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  mSetObj$analSet$oplsda <- dat.in$my.res;
  score.mat <- cbind(mSetObj$analSet$oplsda$scoreMN[,1], mSetObj$analSet$oplsda$orthoScoreMN[,1]);
  colnames(score.mat) <- c("Score (t1)","OrthoScore (to1)");
  fast.write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="oplsda_score.csv");
  load.mat <- cbind(mSetObj$analSet$oplsda$loadingMN[,1], mSetObj$analSet$oplsda$orthoLoadingMN[,1]);
  colnames(load.mat) <- c("Loading (t1)","OrthoLoading (to1)");
  fast.write.csv(signif(load.mat,5), file="oplsda_loadings.csv");
  return(.set.mSet(mSetObj));
}

#'Create OPLS-DA score plot
#'@description Orthogonal PLS-DA (from ropls) score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric
#'@param show Show variable labels, 1 or O
#'@param grey.scale Numeric, indicate grey-scale, 0 for no, and 1 for yes 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotOPLS2DScore<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){
  
  mSetObj <- .get.mSet(mSetObj);
  cls <- mSetObj$dataSet$cls;
  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    opls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    opls.cex <<- opls.cex + 0.1;
  }else{
    opls.cex <<- opls.cex - 0.1;
  }

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$opls.score2d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

  par(mar=c(5,5,3,8));
  
  lv1 <- mSetObj$analSet$oplsda$scoreMN[,1];
  lv2 <- mSetObj$analSet$oplsda$orthoScoreMN[,1];
  xlabel <- paste("T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["p1", "R2X"],1), "%)");
  ylabel <- paste("Orthogonal T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["o1", "R2X"],1), "%)");
  
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- cls == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);

  col.def <- GetColorSchema(cls, grey.scale==1);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # draw ellipse
  for(i in 1:length(lvs)){
    if ( length(col.def) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(col.def[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(col.def, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale);
  pchs <- ExpandSchema(cls, pchs.def);

  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75*opls.cex);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(col.def) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0*opls.cex);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        my.cols <- adjustcolor(cols, alpha.f = 0.4);
        my.cols[pchs == 21] <- "black";
        points(lv1, lv2, pch=pchs, col=my.cols, bg=adjustcolor(cols, alpha.f = 0.4), cex=1.8*opls.cex);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2*opls.cex);
      }
    }
  }
  
  if(grey.scale) {
    col.def <- "black";
  }

  # move legend to outside 
  op <- par(xpd=T);
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  shift <- par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  lgd.y <- axis.lims[4] - shift;
  legend(lgd.x, lgd.y, legend = names(pchs.def), pch=pchs.def, col=col.def, box.lty=0);
  
  par(op);  

  dev.off();
  return(.set.mSet(mSetObj));
}

#'Update OPLS loadings
#'@description Update the OPLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdateOPLS.Splot<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$oplsda$splot.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}

#'S-plot for OPLS-DA
#'@description Orthogonal PLS-DA (from ropls) 
#'S-plot for important features from OPLS-DA
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param plotType plotType for the image, can be "all" or "custom"
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotOPLS.Splot <- function(mSetObj=NA, imgName, plotType="all", format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  s <- as.matrix(mSetObj$dataSet$norm);
  T <- as.matrix(mSetObj$analSet$oplsda$scoreMN)
  p1 <- c()
  for (i in 1:ncol(s)) {
    scov <- cov(s[,i], T)
    p1 <- matrix(c(p1, scov), ncol=1)
  }
  pcorr1 <- c()
  for (i in 1:nrow(p1)) {
    den <- apply(T, 2, sd)*sd(s[,i])
    corr1 <- p1[i,]/den
    pcorr1 <- matrix(c(pcorr1, corr1), ncol=1)
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 8;
  }else if(width == 0){
    w <- h <- 8;
  }else{
    w <- h <- width;
  }
  
  mSetObj$imgSet$opls.loading<-imgName;
  mSetObj$analSet$oplsda$splot.type <- plotType;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,4,7))
  plot(p1, pcorr1, type="n", xlab="p[1]", ylab ="p(corr)[1]", main = "Feature Importance");
  grid(col="lightgrey", lty=3, lwd = 1);
  points(p1, pcorr1, pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  opls.axis.lims <- par("usr");
  if(plotType=="all"){
    text(p1, pcorr1, labels=colnames(s), cex=0.8, pos=4, xpd=TRUE, col="blue");
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
      text(p1[hit.inx], pcorr1[hit.inx], labels=colnames(s)[hit.inx], pos=4, xpd=TRUE, col="blue");
    }
  }else{
    # do nothing
  }
  dev.off();
  splot.mat <- cbind(jitter(p1),p1, pcorr1);
  rownames(splot.mat) <- colnames(s); 
  colnames(splot.mat) <- c("jitter", "p[1]","p(corr)[1]");
  
  ord.inx <- order(-splot.mat[,2], -splot.mat[,3]);
  splot.mat <- signif(splot.mat[ord.inx,],5);
  
  fast.write.csv(signif(splot.mat[,2:3],5), file="oplsda_splot.csv"); 
  mSetObj$analSet$oplsda$splot.mat <- splot.mat;
  mSetObj$analSet$oplsda$opls.axis.lims <- opls.axis.lims;   
  return(.set.mSet(mSetObj));
}

# 
#' PlotOPLS.Imp
#' OPLS VIP plotting function
#' @param mSetObj mSetObj objects generated from last step
#' @param imgName image name
#' @param format image format, can be "png", "jpg", "tiff", "pdf" and "svg"
#' @param dpi numeric, dpi number
#' @param width numeric, width number
#' @param type analysis type, can be "vip" only
#' @param feat.nm feature name, should be "tscore" for now
#' @param feat.num feature number
#' @param color.BW color information
#' @export
#'
PlotOPLS.Imp <- function(mSetObj=NA, imgName, format="png", 
                         dpi=72, width=NA, type="vip", 
                         feat.nm="tscore", 
                         feat.num=15, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1;
  
  mSetObj$imgSet$opls.vip <- imgName;
  
  cls.len <- length(levels(mSetObj$dataSet$cls));
  rt.mrg <- cls.len + 3;
  w <- w + rt.mrg/72; # convert to inch

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");  
  op <- par(mar=c(5,7,3,rt.mrg)); # update right side margin with the number of class

  if(type=="vip"){
    impNm <- "VIP scores";
    mSetObj$analSet$oplsda$imp.type <- "vip";
    if(feat.nm == "tscore"){
      data <- mSetObj$analSet$oplsda$vipVn;
    }else{
      data <- mSetObj$analSet$oplsda$orthoVipVn;
    }
    vip.mat <- cbind(mSetObj$analSet$oplsda$vipVn, mSetObj$analSet$oplsda$orthoVipVn);
  }else{ #not exposed to web
    impNm <- "Weights";
    mSetObj$analSet$oplsda$imp.type <- "weight";
    if(feat.nm == "tscore"){
      data<-mSetObj$analSet$oplsda$weightMN;
    }else{
      data<-mSetObj$analSet$oplsda$orthoWeightMN;
    }
    vip.mat <- cbind(mSetObj$analSet$oplsda$weightMN, mSetObj$analSet$oplsda$orthoWeightMN);
  }
  ord.inx <- rev(order(data));
  vip.mat <- vip.mat[ord.inx,];
  mSetObj$analSet$oplsda$vip.mat <- vip.mat;
  PlotImpVar(mSetObj, data, impNm, feat.num, color.BW);

  par(op);
  dev.off();
  
  fast.write.csv(vip.mat, file="oplsda_vip.csv");
  return(.set.mSet(mSetObj));
}

#'Plot OPLS 
#'@description Plot OPLS 
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@export
#'
PlotOPLS.MDL <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 9;
    
  }else{
    w <- width; 
  }
  h <- w*6/9;
  
  mSetObj$imgSet$opls.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  # the model R2Y and Q2Y
  par(mar=c(5,5,4,8)); # put legend on the right outside
  modBarDF <- mSetObj$analSet$oplsda$modelDF[!(rownames(mSetObj$analSet$oplsda$modelDF) %in% c("rot")), ];
  mod.dat <- data.matrix(modBarDF[!rownames(modBarDF)%in% ("sum"), c("R2X", "R2Y", "Q2")]);
  mod.dat <- t(mod.dat);
  bplt <- barplot(mod.dat,beside=TRUE, names.arg = colnames(mod.dat),xlab = "");
  axis(2, lwd.ticks=1);
  barplot(mod.dat,add = TRUE, beside = TRUE, col = c("lightblue", "mistyrose", "lavender"));
  text(x=bplt, y=mod.dat+max(mod.dat)/25, labels=as.character(mod.dat), xpd=TRUE)
  xpos <- nrow(mod.dat)*ncol(mod.dat) + ncol(mod.dat) + 0.5
  ypos <- max(mod.dat)/2;
  legend(xpos, ypos, legend = c("R2X", "R2Y", "Q2"), pch=15, col=c("lightblue", "mistyrose", "lavender"), xpd=T, bty="n");
  dev.off();
  
  fast.write.csv(mod.dat, file="oplsda_model.csv");
  
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA permutation
#'@description Orthogonal PLS-DA (from ropls) 
#'perform permutation, using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param num Input the number of permutations, default is set to 100.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

OPLSDA.Permut<-function(mSetObj=NA, num=100){
  .prepare.oplsda.permut(mSetObj, num);
  .perform.computing();
  .save.oplsda.permut(mSetObj);
}

.prepare.oplsda.permut <-function(mSetObj=NA, num=100){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$analSet$opls.reg){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  my.fun <- function(){
    if(file.exists("../../rscripts/MetaboAnalystR/R/stats_opls.Rc")){
        compiler::loadcmp("../../rscripts/MetaboAnalystR/R/stats_opls.Rc");
    }
    my.res <- perform_opls(dat.in$data, dat.in$cls, predI=1, permI=dat.in$perm.num, orthoI=NA, crossvalI=dat.in$cv.num);
  }
  dat.in <- list(data=datmat, cls=cls, perm.num=num, cv.num=cv.num, my.fun=my.fun);
  
  qs::qsave(dat.in, file="dat.in.qs");
  return(.set.mSet(mSetObj));
}

.save.oplsda.permut <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  my.res <- dat.in$my.res;
  
  r.vec <- my.res$suppLs[["permMN"]][, "R2Y(cum)"];
  q.vec <- my.res$suppLs[["permMN"]][, "Q2(cum)"];
  
  # note, actual permutation number may be adjusted in public server
  perm.num <- my.res$suppLs[["permI"]];
  
  mSetObj$analSet$oplsda$perm.res <- list(r.vec=r.vec, q.vec=q.vec, perm.num=perm.num);
  return(.set.mSet(mSetObj));
}

#'Plot OPLS-DA permutation
#'@description Orthogonal PLS-DA (from ropls) 
#'perform permutation, using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotOPLS.Permutation<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  perm.res <- mSetObj$analSet$oplsda$perm.res;
  
  r.vec <- perm.res$r.vec;
  q.vec <- perm.res$q.vec;
  perm.num <- perm.res$perm.num;
  
  better.rhits <- sum(r.vec[-1]>=r.vec[1]);
  
  if(better.rhits == 0) {
    pr <- paste("p < ", 1/perm.num, " (", better.rhits, "/", perm.num, ")", sep="");
  }else{
    p <- better.rhits/perm.num;
    pr <- paste("p = ", signif(p, digits=5), " (", better.rhits, "/", perm.num, ")", sep="");
  }
  better.qhits <- sum(q.vec[-1]>=q.vec[1]);
  if(better.qhits == 0) {
    pq <- paste("p < ", 1/perm.num, " (", better.qhits, "/", perm.num, ")", sep="");
  }else{
    p <- better.qhits/perm.num;
    pq <- paste("p = ", signif(p, digits=5), " (", better.qhits, "/", perm.num, ")", sep="");
  }
  
  rng <- range(c(r.vec, q.vec, 1));
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width; 
  }
  h <- w*6/8;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,7));
  rhst <- hist(r.vec[-1], plot=FALSE);
  qhst <- hist(q.vec[-1], plot=FALSE);
  h <- max(c(rhst$counts, qhst$counts))+1;
  bin.size <- min(c(rhst$breaks[2]-rhst$breaks[1], qhst$breaks[2]-qhst$breaks[1]));
  rbins <- seq(min(rhst$breaks),max(rhst$breaks),bin.size);
  qbins <- seq(min(qhst$breaks),max(qhst$breaks),bin.size);
  hist(r.vec[-1], xlim=rng, ylim=c(0, h), breaks=rbins, border=F, ylab="Frequency", xlab= 'Permutations', 
       col=adjustcolor("lightblue", alpha=0.6), main="");
  hist(q.vec[-1], add=TRUE,breaks=qbins, border=F, col=adjustcolor("mistyrose", alpha=0.6));
  
  arrows(r.vec[1], h/3, r.vec[1], 0, length=0.1,angle=30,lwd=2);
  text(r.vec[1], h/2.5, paste('R2Y:', r.vec[1], "\n", pr), xpd=TRUE);
  
  arrows(q.vec[1], h/2, q.vec[1], 0, length=0.1,angle=30,lwd=2);
  text(q.vec[1], h/1.8, paste('Q2:', q.vec[1], "\n", pq), xpd=TRUE);
  
  legend(1, h/3, legend = c("Perm R2Y", "Perm Q2"), pch=15, col=c("lightblue", "mistyrose"), xpd=T, bty="n");
  
  dev.off();
  
  mSetObj$imgSet$opls.permut <- imgName;
  
  msg <- paste("Empirical p-values Q2: ", pq, " and R2Y: ", pr);
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(msg);
  }else{
    print(msg);
    return(.set.mSet(mSetObj));
  }
}

#'Perform SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) 
#'@param mSetObj Input name of the created mSet Object
#'@param comp.num Input the number of computations to run 
#'@param var.num Input the number of variables
#'@param compVarOpt Input the option to perform SPLS-DA
#'@param validOpt INput the valid option
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SPLSR.Anal <- function(mSetObj=NA, comp.num, var.num, compVarOpt, validOpt="Mfold", foldNum=5, doCV=FALSE){    
  .prepare.splsr.anal(mSetObj, comp.num, var.num, compVarOpt, validOpt, foldNum, doCV);
  .perform.computing();
  .save.splsr.anal(mSetObj);
}

.prepare.splsr.anal <- function(mSetObj=NA, comp.num, var.num, compVarOpt, validOpt="Mfold", foldNum, doCV){
  
  if(compVarOpt == "same"){
    comp.var.nums <- rep(var.num, comp.num);
  }else{
    if(exists("comp.var.nums") && all(comp.var.nums > 0)){
      comp.var.nums <- ceiling(comp.var.nums);
    }else{
      msg <- c("All values need to be positive integers!");
      return(0);
    }
  }
  
  mSetObj <- .get.mSet(mSetObj);  
  
  # note, standardize the cls, to minimize the impact of categorical to numerical impact
  cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  datmat <- as.matrix(mSetObj$dataSet$norm);
  
  my.fun <- function(){
    if(file.exists("../../rscripts/MetaboAnalystR/R/stats_spls.Rc")){
        compiler::loadcmp("../../rscripts/MetaboAnalystR/R/stats_spls.Rc");
    }    
    my.res <- splsda(dat.in$data, dat.in$cls, ncomp=dat.in$comp.num, keepX=dat.in$comp.var.nums);
    
    if(doCV){# perform validation
        perf.res <- perf.splsda(my.res, dist= "centroids.dist", validation=validOpt, folds = foldNum);
        my.res$error.rate <- perf.res$error.rate$overall;
    }
    return(my.res);
  }
  
  dat.in <- list(data=datmat, cls=cls, comp.num=comp.num, comp.var.nums=comp.var.nums, my.fun=my.fun);
  
  qs::qsave(dat.in, file="dat.in.qs");
  return(.set.mSet(mSetObj));
}

.save.splsr.anal <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  mSetObj$analSet$splsr <- dat.in$my.res;
  score.mat <- mSetObj$analSet$splsr$variates$X;
  fast.write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="splsda_score.csv");
  load.mat <- mSetObj$analSet$splsr$loadings$X;
  
  # sort based on absolute values of 1, 2 
  ord.inx <- order(-abs(load.mat[,1]), -abs(load.mat[,2]));
  load.mat <- signif(load.mat[ord.inx,],5);
  fast.write.csv(load.mat, file="splsda_loadings.csv");
  
  mSetObj$analSet$splsr$loadings$X <- load.mat;
  return(.set.mSet(mSetObj));
}

#'Plot SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) pairwise summary plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, indicate the number of principle components
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLSPairSummary<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$spls.pair <- imgName;

  if(pc.num > mSetObj$analSet$splsr$ncomp){
    pc.num <- mSetObj$analSet$splsr$ncomp;
  }
  vars <- round(100*mSetObj$analSet$splsr$explained_variance$X,1);
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%");
  Plot.PairScatter(mSetObj$analSet$splsr$variates$X[,1:pc.num], pclabels, mSetObj$dataSet$cls, mSetObj$dataSet$cls.type, imgName, format, dpi, width);
  return(.set.mSet(mSetObj));
}

#'Score Plot SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric, between 1 and 0
#'@param show Numeric, 1 or 0
#'@param grey.scale Numeric, use grey-scale, 0 for no, and 1 for yes. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){
  
  mSetObj <- .get.mSet(mSetObj);

  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    spls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    spls.cex <<- spls.cex + 0.1;
  }else{
    spls.cex <<- spls.cex - 0.1;
  }

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # big margin
  
  mSetObj$imgSet$spls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$splsr$variates$X[,inx1];
  lv2 <- mSetObj$analSet$splsr$variates$X[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx1],1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx2],1), "%)");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,8));
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- cls == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext <- (xrg[2]-xrg[1])/12;
  y.ext <- (yrg[2]-yrg[1])/12;
  xlims <- c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims <- c(yrg[1]-y.ext, yrg[2]+y.ext);
  
  col.def <- GetColorSchema(cls, grey.scale==1);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # draw ellipse
  for(i in 1:length(lvs)){
    if (length(col.def) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(col.def[lvs[i]], alpha=0.25), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(col.def, alpha=0.25), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale);
  pchs <- ExpandSchema(cls, pchs.def);

  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75*spls.cex);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(col.def) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0*spls.cex);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        my.cols <- adjustcolor(cols, alpha.f = 0.4);
        my.cols[pchs == 21] <- "black";
        points(lv1, lv2, pch=pchs, col=my.cols, bg=adjustcolor(cols, alpha.f = 0.4), cex=1.8*spls.cex);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2*spls.cex);
      }
    }
  }
  
  if(grey.scale) {
    col.def <- "black";
  }

  op <- par(xpd=T);
  # move legend to outside 
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  shift <- par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  lgd.y <- axis.lims[4] - shift;
  legend(lgd.x, lgd.y, legend = names(pchs.def), pch=pchs.def, col=col.def, box.lty=0);  
  par(op);

  dev.off();
  return(.set.mSet(mSetObj));
}

#'3D SPLS-DA score plot
#'@description Sparse PLS-DA (from mixOmics) 3D score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLS3DScore <- function(mSetObj=NA, imgName, format="json", inx1=1, inx2=2, inx3=3){
  
  mSetObj <- .get.mSet(mSetObj);
  
  
  cls1 <- mSetObj$dataSet$cls;
  cls.type <- mSetObj$dataSet$cls.type;
  cls.class <- mSetObj$dataSet$type.cls.lbl;
  
  spls3d <- list();
  # need to check if only two components are generated
  if(length(mSetObj$analSet$splsr$explained_variance$X)==2){
    spls3d$score$axis <- paste("Component", c(inx1, inx2), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2)], 1), "%)", sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2)], 5)));
    spls3d$score$axis <- c(spls3d$score$axis, "Component3 (NA)");
    coords <- rbind(coords, "comp 3"=rep (0, ncol(coords)));
  }else{
    spls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2, inx3)], 1), "%)", sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2, inx3)], 5)));
  }
  colnames(coords) <- NULL; 
  spls3d$score$xyz <- coords;
  spls3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(cls1))[cls1])));
  }else{
    cls <- as.character(cls1);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  spls3d$score$facA <- cls;
  
  # now set color for each group
  col.def <- GetColorSchema(cls1);
  spls3d$score$colors <- my.col2rgb(col.def);
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- rjson::toJSON(spls3d);
  sink(imgName);
  cat(json.obj);
  sink();
  mSetObj$imgSet$spls.score3d <- imgName;

  qs::qsave(spls3d$score, "score3d.qs");

  return(.set.mSet(mSetObj));
}

#' PlotSPLS3DLoading
#' @param mSetObj mSetObj
#' @param imgName imgName
#' @param format format
#' @param inx1 inx1
#' @param inx2 inx2
#' @param inx3 inx3
#' @export
PlotSPLS3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  spls = mSetObj$analSet$splsr
  spls3d <- list();
  
  if(length(mSetObj$analSet$splsr$explained_variance$X)==2){
    spls3d$loading$axis <- paste("Loading ", c(inx1, inx2), sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$loadings$X[,c(inx1, inx2)], 5)));
    spls3d$loading$axis <- c(spls3d$loading$axis, "Loading 3");
    coords0 <- coords <- rbind(coords, "comp 3"=rep (0, ncol(coords)));
  }else{
    spls3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");    
    coords0 <- coords <- data.frame(t(signif(mSetObj$analSet$splsr$loadings$X[,c(inx1, inx2, inx3)], 5)));
  }
  
  colnames(coords) <- NULL; 
  spls3d$loading$xyz <- coords;
  spls3d$loading$name <- rownames(spls$loadings$X);
  spls3d$loading$entrez <-rownames(spls$loadings$X); 
  
  dists <- GetDist3D(coords0);
  cols <- GetRGBColorGradient(dists);
  spls3d$loading$cols <- cols;
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  spls3d$cls = cls;
  # see if there is secondary
  
  imgName = paste(imgName, ".", format, sep="");
  json.mat <- rjson::toJSON(spls3d);
  sink(imgName);
  cat(json.mat);
  sink();
  AddMsg("Annotated data is now ready for PCA 3D visualization!");
  
  spls3d$loading$cls = cls;
  qs::qsave(spls3d$loading, "loading3d.qs");

  if(!exists("my.json.scatter")){
    .load.scripts.on.demand("util_scatter3d.Rc");    
  }

  
  if(.on.public.web){
    my.json.scatter(imgName, T);
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}


#'Create SPLS-DA loading plot
#'@description Sparse PLS-DA (from mixOmics) loading plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param inx Input the model index
#'@param viewOpt Detailed view "detail" 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx, viewOpt="detail"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imp.vec <- abs(mSetObj$analSet$splsr$loadings$X[,inx]);
  imp.vec <- imp.vec[imp.vec > 0];
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
    
  }else{
    w <- width;
  }
  h <- w-1;
  
  mSetObj$imgSet$spls.imp <- imgName;
  
  cls.len <- length(levels(mSetObj$dataSet$cls));
  rt.mrg <- cls.len + 3;
  w <- w + rt.mrg/72; # convert to inch

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  op <- par(mar=c(5,7,3,rt.mrg)); # update right side margin with the number of class
  PlotImpVar(mSetObj, imp.vec, paste ("Loadings", inx), 999, FALSE);

  par(op);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Create SPLS-DA classification plot
#'@description Sparse PLS-DA (from mixOmics) plot of 
#'classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotSPLSDA.Classification <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$splsr$error.rate;
  
  edge <- (max(res)-min(res))/100; # expand y uplimit for text
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$splsda.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(res, type='l', xlab='Number of Components', ylab='Error Rate',
       ylim = c(min(res)-5*edge, max(res)+18*edge), axes=F,
       main="Sparse PLS-DA Classification Error Rates")
  text(res, labels = paste(100*round(res,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  axis(2);
  axis(1, 1:length(res), names(res));
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetPCAStats<-function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$pca$permanova.res$stat.info);
}

GetPCAPermANOVA<-function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$pca$permanova.res$pair.res);
}

GetPCAPermANOVAText<-function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$analSet$pca$permanova.res$pair.res;
  # create a HTML table
  myT <- print(xtable::xtable(res, digits=5), 
                           type="html", print.results=F, xtable.width=480,
                           html.table.attributes="border=1 width=480");
  return(myT);
}

# get which number of components give best performance
GetPLSBestTune<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$analSet$plsda$best.num)){
    return(0);
  }
  mSetObj$analSet$plsda$best.num;
}

# obtain VIP score
GetPLSSigMat<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    sig.mat <- mSetObj$analSet$plsr$vip.mat;
  }else if(type == "coef"){
    sig.mat <- mSetObj$analSet$plsda$coef.mat;
  }else{
    sig.mat <- mSetObj$analSet$plsr$imp.loads;
  }
  return(CleanNumber(signif(as.matrix(sig.mat),5)));
}

GetPLSSigRowNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(rownames(mSetObj$analSet$plsr$vip.mat));
  }else if(type == "coef"){
    return(rownames(mSetObj$analSet$plsda$coef.mat));
  }else{
    return(rownames(mSetObj$analSet$plsr$imp.loads))
  }
}

GetPLSSigColNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    my.mat <- mSetObj$analSet$plsr$vip.mat;
  }else if(type == "coef"){
    my.mat <- mSetObj$analSet$plsda$coef.mat;
  }else{
    my.mat <- mSetObj$analSet$plsr$imp.loads;
  }
  if(is.null(my.mat)){
    AddErrMsg("The object does not exist. Please perform CV first.");
    return("NA");
  }
  return(colnames(my.mat));
}

GetPLS_CVRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$plsda$fit.info);
}

GetPLS_CVColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$plsda$fit.info);
}

GetPLS_CVMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  cv.mat <- signif(mSetObj$analSet$plsda$fit.info, 5);
  if(!.on.public.web){
    print(cv.mat);
  }
  return(cv.mat);
}

GetMaxPLSPairComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetMaxPLSCVComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2]));
}
GetMaxPLSBiplotNum <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(nrow(mSetObj[["analSet"]][["plsr"]][["vip.mat"]]));
}

GetDefaultPLSPairComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(5, dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetDefaultPLSCVComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(5, dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2], mSetObj$dataSet$min.grp.size));
}

GetPLSLoadAxesSpec<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pls.axis.lims;
}

GetPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$plsr$imp.loads);  
}

GetPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res<-signif(as.matrix(mSetObj$analSet$plsr$imp.loads[,c(1:2)]), 5);
  pointMap <- setNames(rownames(res), paste(0, seq_len(nrow(res)) - 1, sep = ":"))
  pointMapJson <- RJSONIO::toJSON(pointMap, pretty = TRUE)
  write(pointMapJson,"pointMap2.json")
  return(res)
}

GetPCALoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pca.axis.lims;
}

GetPCALoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res <- rownames(mSetObj$analSet$pca$imp.loads);
  return(as.character(res))
}

GetPCALoadCmpdsNlength <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res <- rownames(mSetObj$analSet$pca$imp.loads);
  return(length(res))
}

getPCALoadCmpdsSplit <- function(mSetObj=NA, n = 0, total = 10){
  mSetObj <- .get.mSet(mSetObj);
  res <- rownames(mSetObj$analSet$pca$imp.loads);
  res <- as.character(res);
  n0 <- n+1;
  nbatch <- ceiling(length(res)/total); # number of feature per batch
  if(nbatch*n0 < length(res)){
    res_vec <- c(res[c((1 + nbatch*n):(nbatch*n0))])
  } else {
    res_vec <- c(res[c((1 + nbatch*n):(length(res)))])
  }
  return(res_vec)
}

GetPCALoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res <- signif(as.matrix(mSetObj$analSet$pca$imp.loads[,c(1:2)]),5);
  pointMap <- setNames(rownames(res), paste(0, seq_len(nrow(res)) - 1, sep = ":"))
  pointMapJson <- RJSONIO::toJSON(pointMap, pretty = TRUE)
   write(pointMapJson,"pointMap.json")
  return(matrix(as.numeric(res), ncol = 2))
}

#'For plotting PCA, selects max top 9 components
#'@description Rotate PCA analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetMaxPCAComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(9, dim(mSetObj$dataSet$norm)-1));
}

GetOPLSLoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$oplsda$opls.axis.lims);
}

GetOPLSSigCmpds <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "splot"){
    res <- rownames(mSetObj$analSet$oplsda$splot.mat);
  }else{
    res <- rownames(mSetObj$analSet$oplsda$vip.mat);
  }
  return(res)
}

GetOPLSSigColNames <- function(mSetObj=NA, type){
  if(type == "splot"){
    return(c("p[1]","p(corr)[1]"));
  }else{
    return(c("VIP[t]"," VIP[ortho-t]"));
  }
}

GetOPLSSigMat <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "splot"){
    res <- as.matrix(mSetObj$analSet$oplsda$splot.mat[,c(1,3)]);
  }else{
    res <- as.matrix(mSetObj$analSet$oplsda$vip.mat);
  }
  res = signif(res,5)
  pointMap <- setNames(rownames(res), paste(0, seq_len(nrow(res)) - 1, sep = ":"))
  pointMapJson <- RJSONIO::toJSON(pointMap, pretty = TRUE)
  write(pointMapJson,"pointMap3.json")
  return(res)
}

GetDefaultSPLSCVComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (min(5, dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2], mSetObj$dataSet$min.grp.size));
}

GetDefaultSPLSPairComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (min(5, dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetSPLS_CVRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$splsda$fit.info);
}

GetSPLS_CVColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$splsda$fit.info);
}

GetSPLS_CVMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(signif(mSetObj$analSet$splsda$fit.info, 5));
}

GetSPLSLoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pls.axis.lims;
}

GetSPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$splsr$loadings$X);
}

GetSPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(signif(mSetObj$analSet$splsr$loadings$X, 5));
}

GetSPLSSigColNames <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return (colnames(mSetObj$analSet$splsda$vip.mat));
  }else if(type == "coef"){
    return (colnames(mSetObj$analSet$splsda$coef.mat));
  }else{
    return (colnames(mSetObj$analSet$splsr$loadings$X));
  }
}

# internal called by all pair plots
Plot.PairScatter <- function(mat, lbls, cls, cls.type, imgName, format, dpi, width){

  if(is.na(width)){
    w <- 11;
  }else if(width == 0){
    w <- 9;
  }else{
    w <- width;
  }
  h <- w;
  
  col.def <- GetColorSchema(cls);
  my.col <- ExpandSchema(cls, col.def);
  legend.nm <- names(col.def);


  lgd.len = length(col.def);
  if(lgd.len < 4){
     omaVal=oma=c(10,3,3,3)
  }else{
     if(lgd.len %% 3 == 0){
        lgd.len = 3;
     }else if(lgd.len %% 4 == 0){
        lgd.len = 4;
     }
     omaVal = oma=c(15,3,3,3)
  }

   pchs.def <- GetShapeSchema(cls, 1); # use different shapes here
   my.pch <- ExpandSchema(cls, pchs.def);

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(cls.type == "disc"){

    pairs(mat, col=my.col, pch=my.pch, labels=lbls, oma=omaVal);
    par(xpd = TRUE)
    if(lgd.len > 4){
      legend("bottom", legend = legend.nm, pch=pchs.def, col=col.def, cex=0.9, bty = "n", ncol =lgd.len);
    }else{
      legend("bottom", legend = legend.nm, pch=pchs.def, col=col.def, bty = "n", ncol =lgd.len);
    }
  }else{
    pairs(mat, labels=lbls);
  }
  dev.off();
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
