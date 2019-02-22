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
  
  pca <- prcomp(mSetObj$dataSet$norm, center=TRUE, scale=F);
  
  # obtain variance explained
  sum.pca <- summary(pca);
  imp.pca <- sum.pca$importance;
  std.pca <- imp.pca[1,]; # standard devietation
  var.pca <- imp.pca[2,]; # variance explained by each PC
  cum.pca <- imp.pca[3,]; # cummulated variance explained
  
  # store the item to the pca object
  mSetObj$analSet$pca<-append(pca, list(std=std.pca, variance=var.pca, cum.var=cum.pca));
  write.csv(signif(mSetObj$analSet$pca$x,5), file="pca_score.csv");
  write.csv(signif(mSetObj$analSet$pca$rotation,5), file="pca_loadings.csv");
  mSetObj$analSet$pca$loading.type <- "all";
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
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
  write.csv(signif(pca$x,5), file="pca_score.csv");
  write.csv(signif(pca$rotation,5), file="pca_loadings.csv");
  
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
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  
  mSetObj$imgSet$pca.pair <- imgName;
  
  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(mSetObj$dataSet$cls.type == "disc"){
    pairs(mSetObj$analSet$pca$x[,1:pc.num], col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels);
  }else{
    pairs(mSetObj$analSet$pca$x[,1:pc.num], labels=pclabels);
  }
  dev.off();
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
#'@usage PlotPCA2DScore(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pcx Specify the principal component on the x-axis
#'@param pcy Specify the principal component on the y-axis
#'@param reg Numeric, input a number between 0 and 1, 0.95 will display the 95 percent confidence regions, and 0 will not.
#'@param show Display sample names, 1 = show names, 0 = do not show names.
#'@param grey.scale Use grey-scale colors, 1 = grey-scale, 0 = not grey-scale.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPCA2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0){
  
  mSetObj <- .get.mSet(mSetObj);
  
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
  h <- w;
  
  mSetObj$imgSet$pca.score2d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op<-par(mar=c(5,5,3,3));
  
  if(mSetObj$dataSet$cls.type == "disc"){
    # obtain ellipse points to the scatter plot for each category
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
    }else{
      cls <- mSetObj$dataSet$cls;
    }
    
    lvs <- levels(cls);
    pts.array <- array(0, dim=c(100,2,length(lvs)));
    for(i in 1:length(lvs)){
      inx <-mSetObj$dataSet$cls == lvs[i];
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
    
    cols <- GetColorSchema(mSetObj, grey.scale==1);
    uniq.cols <- unique(cols);
    
    plot(pc1, pc2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot",
         col=cols, pch=as.numeric(mSetObj$dataSet$cls)+1); ## added
    grid(col = "lightgray", lty = "dotted", lwd = 1);
    
    # make sure name and number of the same order DO NOT USE levels, which may be different
    legend.nm <- unique(as.character(sort(cls)));
    ## uniq.cols <- unique(cols);
    
    ## BHAN: when same color is choosen; it makes an error
    if ( length(uniq.cols) > 1 ) {
      names(uniq.cols) <- legend.nm;
    }
    
    # draw ellipse
    for(i in 1:length(lvs)){
      if (length(uniq.cols) > 1) {
        polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
      } else {
        polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
      }
      if(grey.scale) {
        lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
      }
    }
    
    pchs <- GetShapeSchema(mSetObj, show, grey.scale);
    if(grey.scale) {
      cols <- rep("black", length(cols));
    }
    if(show == 1){
      text(pc1, pc2, label=text.lbls, pos=4, xpd=T, cex=0.75);
      points(pc1, pc2, pch=pchs, col=cols);
    }else{
      if(length(uniq.cols) == 1){
        points(pc1, pc2, pch=pchs, col=cols, cex=1.0);
      }else{
        if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>0))){
          points(pc1, pc2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
        }else{
          points(pc1, pc2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
        }
      }
    }
    uniq.pchs <- unique(pchs);
    if(grey.scale) {
      uniq.cols <- "black";
    }
    legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  }else{
    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, type='n', main="Scores Plot");
    points(pc1, pc2, pch=15, col="magenta");
    text(pc1, pc2, label=text.lbls, pos=4, col ="blue", xpd=T, cex=0.8);
  }
  par(op);
  dev.off();
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
  
  pca <-  mSetObj$analSet$pca;
  pca3d <- list();
  pca3d$score$axis <- paste("PC", c(inx1, inx2, inx3), " (", 100*round( mSetObj$analSet$pca$variance[c(inx1, inx2, inx3)], 3), "%)", sep="");
  coords <- data.frame(t(signif(pca$x[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pca3d$score$xyz <- coords;
  pca3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pca3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  pca3d$score$colors <- cols;
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(pca3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  
  if(!.on.public.web){
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
#'@usage PlotPCALoading(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, plotType, lbl.feat=1)
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
  
  loadings<-signif(as.matrix(cbind(mSetObj$analSet$pca$rotation[,inx1],mSetObj$analSet$pca$rotation[,inx2])),5);
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2);
  colnames(loadings)<-c(ldName1, ldName2);
  load.x.uniq <- jitter(loadings[,1]);
  names(load.x.uniq) <- rownames(loadings);
  mSetObj$analSet$pca$load.x.uniq <- load.x.uniq;
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
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
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
PlotPCABiplot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  choices = c(inx1, inx2);
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
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  biplot(t(t(scores[, choices]) / lam), t(t(mSetObj$analSet$pca$rotation[, choices]) * lam), xpd =T, cex=0.9);
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
    comp.num <- 8;
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
  mSetObj$analSet$plsr <- pls::plsr(cls~datmat,method='oscorespls', ncomp=comp.num);
  mSetObj$analSet$plsr$reg <- reg;
  mSetObj$analSet$plsr$loading.type <- "all";
  mSetObj$custom.cmpds <- c();
  
  write.csv(signif(mSetObj$analSet$plsr$scores,5), row.names=rownames(mSetObj$dataSet$norm), file="plsda_score.csv");
  write.csv(signif(mSetObj$analSet$plsr$loadings,5), file="plsda_loadings.csv");
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
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.pair <- imgName;
  
  vars <- round(100*mSetObj$analSet$plsr$Xvar[1:pc.num]/mSetObj$analSet$plsr$Xtotvar,1);
  my.data <- mSetObj$analSet$plsr$scores[,1:pc.num];
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%");
  pairs(my.data, col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels)
  dev.off();
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
#'@param use.sparse Logical, use a sparse algorithm (T) or not (F)
#'@export
#'
PlotPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, use.sparse=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$plsr$scores[,inx1];
  lv2 <- mSetObj$analSet$plsr$scores[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$plsr$Xvar[inx1]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$plsr$Xvar[inx2]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
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
    inx <- mSetObj$dataSet$cls == lvs[i];
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
  
  ## cols = as.numeric(dataSet$cls)+1;
  cols <- GetColorSchema(mSetObj, grey.scale==1);
  uniq.cols <- unique(cols);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # make sure name and number of the same order DO NOT USE levels, which may be different
  legend.nm <- unique(as.character(sort(cls)));
  ## uniq.cols <- unique(cols);
  
  ## BHAN: when same color is choosen for black/white; it makes an error
  # names(uniq.cols) <- legend.nm;
  if (length(uniq.cols) > 1) {
    names(uniq.cols) <- legend.nm;
  }
  # draw ellipse
  for(i in 1:length(lvs)){
    if ( length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs <- GetShapeSchema(mSetObj, show, grey.scale);
  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(uniq.cols) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>0))){
        points(lv1, lv2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
      }
    }
  }
  
  uniq.pchs <- unique(pchs);
  if(grey.scale) {
    uniq.cols <- "black";
  }
  legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  
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
  
  pls3d <- list();
  pls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$plsr$Xvar[c(inx1, inx2, inx3)]/mSetObj$analSet$plsr$Xtotvar, 1), "%)", sep="");
  coords <- data.frame(t(signif(mSetObj$analSet$plsr$score[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pls3d$score$xyz <- coords;
  pls3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pls3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  pls3d$score$colors <- cols;
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(pls3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  mSet$imgSet$pls.score3d <- imgName;
  return(.set.mSet(mSetObj));
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
  loadings = signif(as.matrix(cbind(load1, load2)),5);
  
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2)
  colnames(loadings)<-c(ldName1, ldName2);
  load.x.uniq <- jitter(loadings[,1]);
  names(load.x.uniq) <- rownames(loadings);
  mSetObj$analSet$plsr$load.x.uniq <- load.x.uniq;
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

#'PLS-DA classification and feature selection
#'@description PLS-DA classification and feature selection
#'@param mSetObj Input name of the created mSet Object
#'@param methodName Logical, by default set to TRUE
#'@param compNum GetDefaultPLSCVComp()
#'@param choice Input the choice, by default it is Q2
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PLSDA.CV <- function(mSetObj=NA, methodName="T", compNum=GetDefaultPLSCVComp(), choice="Q2"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_caret()
  }
  
  # get classification accuracy using caret
  plsda.cls <- caret::train(mSetObj$dataSet$norm, mSetObj$dataSet$cls, "pls", trControl=caret::trainControl(method=ifelse(methodName == 'L', "LOOCV", 'CV')), tuneLength=compNum);
  
  # note, for regression, use model matrix
  if(mSetObj$analSet$plsr$reg){
    cls<-cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  
  # use the classifical regression to get R2 and Q2 measure
  plsda.reg <- pls::plsr(cls~datmat,method ='oscorespls', ncomp=compNum, validation= ifelse(methodName == 'L', "LOO", 'CV'));
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
      coef.mean <- apply(coef.mat, 1, mean);
      coef.mat <- cbind(coef.mean = coef.mean, coef.mat);
    }
    # rearange in decreasing order, keep as matrix, prevent dimesion dropping if only 1 col
    inx.ord <- order(coef.mat[,1], decreasing=T);
    coef.mat <- data.matrix(coef.mat[inx.ord, ,drop=FALSE]);
    write.csv(signif(coef.mat,5), file="plsda_coef.csv"); # added 27 Jan 2014
  }
  # calculate VIP http://mevik.net/work/software/VIP.R
  pls <- mSetObj$analSet$plsr;
  b <- c(pls$Yloadings)[1:compNum];
  T <- pls$scores[,1:compNum, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- pls$loading.weights[,1:compNum, drop = FALSE]
  Wnorm2 <- colSums(W^2);
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
  if(compNum > 1){
    vip.mat <- as.matrix(t(vips));
  }else{
    vip.mat <- as.matrix(vips);
  }
  colnames(vip.mat) <- paste("Comp.", 1:ncol(vip.mat));
  write.csv(signif(vip.mat,5),file="plsda_vip.csv");
  
  mSetObj$analSet$plsda<-list(best.num=best.num, choice=choice, coef.mat=coef.mat, vip.mat=vip.mat, fit.info=all.info);
  return(.set.mSet(mSetObj));
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
  
  perm.vec <- c(res.orig, unlist(res.perm, use.names=FALSE));
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
    p <- paste("p < ", 1/num, " (", better.hits, "/", num, ")", sep="");
  }else{
    p <- better.hits/num;
    p <- paste("p = ", signif(p, digits=5), " (", better.hits, "/", num, ")", sep="");
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
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.imp<-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if(type=="vip"){
    mSetObj$analSet$plsda$imp.type <- "vip";
    vips <- mSetObj$analSet$plsda$vip.mat[,feat.nm];
    PlotImpVar(mSetObj, vips, "VIP scores", feat.num, color.BW);
  }else{
    mSetObj$analSet$plsda$imp.type <- "coef";
    data<-mSetObj$analSet$plsda$coef.mat[,feat.nm];
    PlotImpVar(mSetObj, data, "Coefficients", feat.num, color.BW);
  }
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
  
  cls.len <- length(levels(mSetObj$dataSet$cls));
  if(cls.len == 2){
    rt.mrg <- 5;
  }else if(cls.len == 3){
    rt.mrg <- 6;
  }else if(cls.len == 4){
    rt.mrg <- 7;
  }else if(cls.len == 5){
    rt.mrg <- 8;
  }else if(cls.len == 6){
    rt.mrg <- 9;
  }else{
    rt.mrg <- 11;
  }
  op <- par(mar=c(5,7,3,rt.mrg)); # set right side margin with the number of class
  
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
  dotcolor <- ifelse(color.BW, "darkgrey", "blue");
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
  colorpalette <- ifelse(color.BW, "Greys", "RdYlGn");
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
  
  # now add color key, padding with more intermediate colors for contiuous band
  col <- colorRampPalette(RColorBrewer::brewer.pal(25, colorpalette))(50)
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
  
  par(op);
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
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(reg==TRUE){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  
  mSetObj$analSet$oplsda <- perform_opls(datmat, cls, predI=1, permI=0, orthoI=NA, crossvalI=cv.num);
  mSetObj$analSet$oplsda$reg <- reg;
  score.mat <- cbind(mSetObj$analSet$oplsda$scoreMN[,1], mSetObj$analSet$oplsda$orthoScoreMN[,1]);
  colnames(score.mat) <- c("Score (t1)","OrthoScore (to1)");
  write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="oplsda_score.csv");
  load.mat <- cbind(mSetObj$analSet$oplsda$loadingMN[,1], mSetObj$analSet$oplsda$orthoLoadingMN[,1]);
  colnames(load.mat) <- c("Loading (t1)","OrthoLoading (to1)");
  write.csv(signif(load.mat,5), file="oplsda_loadings.csv");
  
  # default options for feature labels on splot
  mSetObj$custom.cmpds <- c();
  mSetObj$analSet$oplsda$splot.type <- "all";
  
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
PlotOPLS2DScore<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0){
  
  mSetObj <- .get.mSet(mSetObj);
  
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
  par(mar=c(5,5,3,3));
  lv1 <- mSetObj$analSet$oplsda$scoreMN[,1];
  lv2 <- mSetObj$analSet$oplsda$orthoScoreMN[,1];
  xlabel <- paste("T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["p1", "R2X"],1), "%)");
  ylabel <- paste("Orthogonal T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["o1", "R2X"],1), "%)");
  
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  lvs <- levels(mSetObj$dataSet$cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- mSetObj$dataSet$cls == lvs[i];
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
  
  ## cols = as.numeric(dataSet$cls)+1;
  cols <- GetColorSchema(mSetObj, grey.scale==1);
  uniq.cols <- unique(cols);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # make sure name and number of the same order DO NOT USE levels, which may be different
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  ## uniq.cols <- unique(cols);
  
  if (length(uniq.cols) > 1 ) {
    names(uniq.cols) <- legend.nm;
  }
  # draw ellipse
  for(i in 1:length(lvs)){
    if ( length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs <- GetShapeSchema(mSetObj, show, grey.scale);
  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(uniq.cols) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>0))){
        points(lv1, lv2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
      }
    }
  }
  
  uniq.pchs <- unique(pchs);
  if(grey.scale) {
    uniq.cols <- "black";
  }
  legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  
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
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotOPLS.Splot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
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
    
  }else{
    w <- h <- width;
  }
  
  mSetObj$imgSet$opls.loading<-imgName;
  plotType <- mSetObj$analSet$oplsda$splot.type;
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
  write.csv(signif(splot.mat[,2:3],5), file="oplsda_splot.csv"); 
  mSetObj$analSet$oplsda$splot.mat <- splot.mat;
  mSetObj$analSet$oplsda$opls.axis.lims <- opls.axis.lims;   
  return(.set.mSet(mSetObj));
}

#'Plot loading compounds
#'@description Plot loading compounds
#'@param mSetObj Input name of the created mSet Object
#'@param cmpdNm Input the name of the selected compound
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@export
#'
PlotLoadingCmpd<-function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to record the clicked compounds
  mSetObj$custom.cmpds <- c(mSetObj$custom.cmpds, cmpdNm);
  .set.mSet(mSetObj);
  
  return(PlotCmpdView(mSetObj, cmpdNm, format, dpi, width));
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
  
  write.csv(mod.dat, file="oplsda_model.csv");
  
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA permutation
#'@description Orthogonal PLS-DA (from ropls) 
#'perform permutation, using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param num Input the number of permutations, default is set to 100.
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotOPLS.Permutation<-function(mSetObj=NA, imgName, format="png", dpi=72, num=100, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$analSet$oplsda$reg){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  #perm.res<-performOPLS(datmat,cls, predI=1, orthoI=NA, permI=num, crossvalI=cv.num);
  perm.res <- perform_opls(datmat,cls, predI=1, orthoI=NA, permI=num, crossvalI=cv.num);
  r.vec <- perm.res$suppLs[["permMN"]][, "R2Y(cum)"];
  q.vec <- perm.res$suppLs[["permMN"]][, "Q2(cum)"];
  
  better.rhits <- sum(r.vec[-1]>=r.vec[1]);
  
  if(better.rhits == 0) {
    pr <- paste("p < ", 1/num, " (", better.rhits, "/", num, ")", sep="");
  }else{
    p <- better.rhits/num;
    pr <- paste("p = ", signif(p, digits=5), " (", better.rhits, "/", num, ")", sep="");
  }
  better.qhits <- sum(q.vec[-1]>=q.vec[1]);
  if(better.qhits == 0) {
    pq <- paste("p < ", 1/num, " (", better.qhits, "/", num, ")", sep="");
  }else{
    p <- better.qhits/num;
    pq <- paste("p = ", signif(p, digits=5), " (", better.qhits, "/", num, ")", sep="");
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
  
  #text(bw.vec[1], h/3.5, paste('Observed \n statistic \n', mSetObj$analSet$plsda$permut.p), xpd=T);
  
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

# Perform OPLS
# Orthogonal PLS functions (adapted from ropls package for web-based usage)
# Jeff Xia \email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
perform_opls <- function (x, y = NULL, predI = NA, orthoI = 0, crossvalI = 7, log10L = FALSE, permI = 20, 
                          scaleC = c("none", "center", "pareto", "standard")[4], ...) {
  xMN <- x;
  if(class(y) == "matrix"){
    yMCN <- y;
  }else{
    yMCN <- matrix(y, ncol = 1);
  }
  rownames(yMCN) <- rownames(xMN)
  colnames(yMCN) <- paste0("y", 1:ncol(yMCN))
  yLevelVc <- NULL;
  xZeroVarVi <- NULL;
  epsN <- .Machine[["double.eps"]]
  
  opl <- .coreOPLS(xMN = xMN, yMCN = yMCN, orthoI = orthoI, predI = predI, 
                   scaleC = scaleC, crossvalI = crossvalI);
  
  opl$suppLs[["y"]] <- y
  opl$typeC <- "OPLS-DA";
  
  ## Permutation testing (Szymanska et al, 2012)
  if(permI > 0) {
    modSumVc <- colnames(opl$summaryDF)
    permMN <- matrix(0,
                     nrow = 1 + permI,
                     ncol = length(modSumVc),
                     dimnames = list(NULL, modSumVc))
    
    perSimVn <- numeric(1 + permI)
    perSimVn[1] <- 1
    
    permMN[1, ] <- as.matrix(opl$summaryDF)
    for(k in 1:permI) {
      yVcn <- drop(opl$suppLs[["yMCN"]])
      yPerVcn <- sample(yVcn)
      yPerMCN <- matrix(yPerVcn, ncol = 1)
      perOpl <- .coreOPLS(xMN = xMN,
                          yMCN = yPerMCN,
                          orthoI = opl$summaryDF[, "ort"],
                          predI = opl$summaryDF[, "pre"],
                          scaleC = scaleC,
                          crossvalI = crossvalI)
      
      permMN[1 + k, ] <- as.matrix(perOpl$summaryDF);
      perSimVn[1 + k] <- .similarityF(opl$suppLs[["yMCN"]], yPerMCN)
    }
    
    permMN <- cbind(permMN, sim = perSimVn);
    perPvaVn <- c(pR2Y = (1 + length(which(permMN[-1, "R2Y(cum)"] >= permMN[1, "R2Y(cum)"]))) / (nrow(permMN) - 1),
                  pQ2 = (1 + length(which(permMN[-1, "Q2(cum)"] >= permMN[1, "Q2(cum)"]))) / (nrow(permMN) - 1));
    opl$summaryDF[, "pR2Y"] <- perPvaVn["pR2Y"];
    opl$summaryDF[, "pQ2"] <- perPvaVn["pQ2"];
    opl$suppLs[["permMN"]] <- permMN;
  }
  
  ##------------------------------------
  ##   Numerical results
  ##------------------------------------
  
  totN <- length(c(xMN))
  nasN <- sum(is.na(c(xMN)))
  
  if(!is.null(opl$suppLs[["yMCN"]])) {
    totN <- totN + length(c(opl$suppLs[["yMCN"]]))
    nasN <- nasN + sum(is.na(c(opl$suppLs[["yMCN"]])))
  }
  
  ## Raw summary
  ##------------
  
  opl$suppLs[["topLoadI"]] <- 3
  
  if(ncol(xMN) > opl$suppLs[["topLoadI"]]) {
    xVarVn <- apply(xMN, 2, var)
    names(xVarVn) <- 1:length(xVarVn)
    xVarVn <- sort(xVarVn)
    xVarSorVin <- as.numeric(names(xVarVn[seq(1, length(xVarVn), length = opl$suppLs[["topLoadI"]])]))
    opl$suppLs[["xSubIncVarMN"]] <- xMN[, xVarSorVin, drop = FALSE]
  } else{
    opl$suppLs[["xSubIncVarMN"]] <- xMN
  }
  
  if(ncol(xMN) <= 100) {
    xCorMN <- cor(xMN, use = "pairwise.complete.obs")
    xCorMN[lower.tri(xCorMN, diag = TRUE)] <- 0
    
    if(ncol(xMN) > opl$suppLs[["topLoadI"]]) {
      xCorNexDF <- which(abs(xCorMN) >= sort(abs(xCorMN), decreasing = TRUE)[opl$suppLs[["topLoadI"]] + 1],
                         arr.ind = TRUE);
      xCorDisMN <- matrix(0,
                          nrow = nrow(xCorNexDF),
                          ncol = nrow(xCorNexDF),
                          dimnames = list(colnames(xMN)[xCorNexDF[, "row"]],
                                          colnames(xMN)[xCorNexDF[, "col"]]))
      
      for(k in 1:nrow(xCorDisMN)){
        xCorDisMN[k, k] <- xCorMN[xCorNexDF[k, "row"], xCorNexDF[k, "col"]]
      }
    } else {
      xCorDisMN <- xCorMN
    }
    opl$suppLs[["xCorMN"]] <- xCorDisMN
    rm(xCorDisMN)
  }
  return(invisible(opl))
}

.coreOPLS <- function (xMN, yMCN, orthoI, predI, scaleC, crossvalI) {
  epsN <- .Machine[["double.eps"]]
  varVn <- NULL
  yMeanVn <- NULL
  ySdVn <- NULL
  wMN <- NULL
  cMN <- NULL
  uMN <- NULL
  rMN <- NULL
  bMN <- NULL
  vipVn <- NULL
  yPreMN <- NULL
  yTesMN <- NULL
  toMN <- NULL
  poMN <- NULL
  woMN <- NULL
  coMN <- NULL
  orthoVipVn <- NULL
  naxVi <- which(is.na(c(xMN)))
  naxL <- length(naxVi) > 0
  nayVi <- integer()
  nayL <- FALSE;
  yMN <- yMCN;
  
  obsNamVc <- rownames(xMN)
  
  autNcoL <- autNcpL <- FALSE
  autMaxN <- min(c(10, dim(xMN)))
  if (is.na(orthoI)) {
    if (autMaxN == 1) {
      orthoI <- 0
      predI <- 1
      warning("The data contain a single variable (or sample): A PLS model with a single component will be built", 
              call. = FALSE)
    }
    else {
      orthoI <- autMaxN - 1
      predI <- 1
      autNcoL <- TRUE
    }
  }
  if (is.na(predI)) {
    if (orthoI > 0) {
      if (autMaxN == 1) {
        orthoI <- 0
        warning("The data contain a single variable (or sample): A PLS model with a single component will be built", 
                call. = FALSE)
      }
      else warning("OPLS(-DA): The number of predictive component is set to 1 for a single response model", 
                   call. = FALSE)
      predI <- 1
      if ((predI + orthoI) > min(dim(xMN))) 
        stop("The sum of 'predI' (", predI, ") and 'orthoI' (", 
             orthoI, ") exceeds the minimum dimension of the 'x' data matrix (", 
             min(dim(xMN)), ")", call. = FALSE)
    }
    else {
      predI <- autMaxN
      autNcpL <- TRUE
    }
  }
  xVarVn <- apply(xMN, 2, function(colVn) var(colVn, na.rm = TRUE))
  xMeanVn <- apply(xMN, 2, function(colVn) mean(colVn, na.rm = TRUE))
  switch(scaleC, none = {
    xMeanVn <- rep(0, ncol(xMN))
    xSdVn <- rep(1, times = ncol(xMN))
  }, center = {
    xSdVn <- rep(1, times = ncol(xMN))
  }, pareto = {
    xSdVn <- apply(xMN, 2, function(colVn) sqrt(sd(colVn, 
                                                   na.rm = TRUE)))
  }, standard = {
    xSdVn <- apply(xMN, 2, function(colVn) sd(colVn, na.rm = TRUE))
  })
  xMN <- scale(xMN, center = xMeanVn, scale = xSdVn)
  if (!is.null(colnames(xMN))) {
    xvaNamVc <- colnames(xMN)
  }
  else xvaNamVc <- paste("x", 1:ncol(xMN), sep = "")
  preNamVc <- paste("p", 1:predI, sep = "")
  pMN <- matrix(0, nrow = ncol(xMN), ncol = predI, dimnames = list(xvaNamVc, 
                                                                   preNamVc))
  tMN <- uMN <- matrix(0, nrow = nrow(xMN), ncol = predI, dimnames = list(obsNamVc, 
                                                                          preNamVc))
  ssxTotN <- sum(xMN^2, na.rm = TRUE)
  
  yMeanVn <- apply(yMN, 2, function(colVn) mean(colVn,na.rm = TRUE))
  
  yMeanVn <- rep(0, times = ncol(yMN))
  ySdVn <- rep(1, times = ncol(yMN))
  yMN <- scale(yMN, center = yMeanVn, scale = ySdVn)
  yvaNamVc <- paste("y", 1:ncol(yMN), sep = "")
  wMN <- pMN
  uMN <- tMN
  cMN <- matrix(0, nrow = ncol(yMN), ncol = predI, dimnames = list(yvaNamVc, preNamVc))
  cvfNamVc <- paste("cv", 1:crossvalI, sep = "")
  cvfOutLs <- split(1:nrow(xMN), rep(1:crossvalI, length = nrow(xMN)))
  prkVn <- numeric(crossvalI)
  ru1ThrN <- ifelse(orthoI == 0, ifelse(nrow(xMN) > 100, yes = 0, no = 0.05), 0.01)
  ssyTotN <- rs0N <- sum(yMN^2, na.rm = TRUE)
  hN <- 1
  
  orthoNamVc <- paste("o", 1:orthoI, sep = "");
  toMN <- matrix(0, nrow = nrow(xMN), ncol = orthoI, 
                 dimnames = list(obsNamVc, orthoNamVc));
  woMN <- poMN <- matrix(0, nrow = ncol(xMN), ncol = orthoI, 
                         dimnames = list(xvaNamVc, orthoNamVc));
  coMN <- matrix(0, nrow = ncol(yMN), ncol = orthoI, 
                 dimnames = list(yvaNamVc, orthoNamVc));
  modelDF <- as.data.frame(matrix(NA, nrow = 1 + orthoI + 
                                    1, ncol = 7, dimnames = list(c("p1", orthoNamVc, 
                                                                   "sum"), c("R2X", "R2X(cum)", "R2Y", "R2Y(cum)", 
                                                                             "Q2", "Q2(cum)", "Signif."))));
  for (j in 1:ncol(modelDF)){ 
    mode(modelDF[, j]) <- ifelse(colnames(modelDF)[j] == "Signif.", "character", "numeric")
  }
  xcvTraLs <- lapply(cvfOutLs, function(obsVi) xMN[-obsVi, , drop = FALSE])
  xcvTesLs <- lapply(cvfOutLs, function(obsVi) xMN[obsVi, , drop = FALSE])
  ycvTraLs <- lapply(cvfOutLs, function(obsVi) yMN[-obsVi, , drop = FALSE])
  ycvTesLs <- lapply(cvfOutLs, function(obsVi) yMN[obsVi, , drop = FALSE])
  xcvTraLs <- c(xcvTraLs, list(xMN))
  ycvTraLs <- c(ycvTraLs, list(yMN))
  breL <- FALSE
  
  for (noN in 1:(orthoI + 1)) {
    if (breL){
      break
    }
    for (cvN in 1:length(xcvTraLs)) {
      
      xcvTraMN <- xcvTraLs[[cvN]]
      ycvTraMN <- ycvTraLs[[cvN]]
      if (ncol(ycvTraMN) > 1) {
        wwMN <- apply(ycvTraMN, 2, function(colVn) crossprod(xcvTraMN, colVn)/drop(crossprod(colVn)))
        wwSvdLs <- svd(wwMN)
        wwNcpVin <- which(wwSvdLs[["d"]]^2 > epsN * sum(wwSvdLs[["d"]]^2))
        twMN <- wwSvdLs[["u"]][, wwNcpVin, drop = FALSE] %*% diag(wwSvdLs[["d"]][wwNcpVin], nrow = length(wwNcpVin))
      }
      uOldVn <- ycvTraMN[, 1, drop = FALSE]
      repeat {
        wVn <- crossprod(xcvTraMN, uOldVn)/drop(crossprod(uOldVn))
        wVn <- wVn/sqrt(drop(crossprod(wVn)))
        tVn <- xcvTraMN %*% wVn
        cVn <- crossprod(ycvTraMN, tVn)/drop(crossprod(tVn))
        uVn <- ycvTraMN %*% cVn/drop(crossprod(cVn))
        dscN <- drop(sqrt(crossprod((uVn - uOldVn)/uVn)))
        if (ncol(ycvTraMN) == 1 || dscN < 1e-10) {
          break
        }else {
          uOldVn <- uVn
        }
      }
      pVn <- crossprod(xcvTraMN, tVn)/drop(crossprod(tVn))
      if (ncol(ycvTraMN) > 1){
        for (j in 1:ncol(twMN)) {
          woVn <- pVn - drop(crossprod(twMN[, 
                                            j, drop = FALSE], pVn))/drop(crossprod(twMN[, 
                                                                                        j, drop = FALSE])) * twMN[, j, drop = FALSE];
        }
      } else {
        woVn <- pVn - drop(crossprod(wVn, pVn))/drop(crossprod(wVn)) * wVn
      }
      woVn <- woVn/sqrt(drop(crossprod(woVn)))
      toVn <- xcvTraMN %*% woVn
      coVn <- crossprod(ycvTraMN, toVn)/drop(crossprod(toVn))
      poVn <- crossprod(xcvTraMN, toVn)/drop(crossprod(toVn))
      
      if (cvN <= crossvalI) {
        xcvTesMN <- xcvTesLs[[cvN]]
        ycvTesMN <- ycvTesLs[[cvN]]
        if (any(is.na(xcvTesMN))) {
          prxVn <- numeric(nrow(xcvTesMN))
          for (r in 1:length(prxVn)) {
            comVl <- complete.cases(xcvTesMN[r, ])
            prxVn[r] <- crossprod(xcvTesMN[r, comVl], wVn[comVl])/drop(crossprod(wVn[comVl]))
          }
          prkVn[cvN] <- sum((ycvTesMN - prxVn %*% t(cVn))^2, na.rm = TRUE)
        } else { 
          prkVn[cvN] <- sum((ycvTesMN - xcvTesMN %*% wVn %*% t(cVn))^2, na.rm = TRUE)
        }
        toTesVn <- xcvTesMN %*% woVn
        xcvTesLs[[cvN]] <- xcvTesMN - tcrossprod(toTesVn, poVn)
        if (cvN == crossvalI) {
          q2N <- 1 - sum(prkVn)/rs0N
          if (noN == 1) {
            modelDF["p1", "Q2(cum)"] <- modelDF["p1", "Q2"] <- q2N
          } else {
            modelDF[noN, "Q2(cum)"] <- q2N - modelDF["p1", "Q2"]
            modelDF[noN, "Q2"] <- q2N - sum(modelDF[1:(noN - 1), "Q2"], na.rm = TRUE)
          }
        }
      } else {
        r2yN <- sum(tcrossprod(tVn, cVn)^2)/ssyTotN
        if (noN == 1) {
          modelDF["p1", "R2Y(cum)"] <- modelDF["p1", "R2Y"] <- r2yN
        } else {
          modelDF[noN, "R2Y(cum)"] <- r2yN - modelDF["p1", "R2Y"]
          modelDF[noN, "R2Y"] <- r2yN - sum(modelDF[1:(noN - 1), "R2Y"], na.rm = TRUE)
        }
        if (noN <= orthoI) {
          modelDF[paste0("o", noN), "R2X"] <- sum(tcrossprod(toVn,poVn)^2)/ssxTotN
          poMN[, noN] <- poVn
          toMN[, noN] <- toVn
          woMN[, noN] <- woVn
          coMN[, noN] <- coVn
        }
        
        if (!is.na(modelDF[noN, "R2Y"]) & modelDF[noN, "R2Y"] < 0.01) {
          modelDF[noN, "Signif."] <- "N4"
        } else if (!is.na(modelDF[noN, "Q2"]) & modelDF[noN, "Q2"] < ru1ThrN) {
          modelDF[noN, "Signif."] <- "NS"
        } else {
          modelDF[noN, "Signif."] <- "R1"
        }
        
        if (autNcoL && modelDF[noN, "Signif."] !=  "R1" && noN > 2) {
          breL <- TRUE
          break
        } else {
          cMN[, 1] <- cVn
          pMN[, 1] <- pVn
          tMN[, 1] <- tVn
          uMN[, 1] <- uVn
          wMN[, 1] <- wVn
        }
      }
      if (breL) {
        break;
      }
      if (noN < orthoI + 1){
        xcvTraLs[[cvN]] <- xcvTraMN - tcrossprod(toVn, poVn);
      }
    }
  }
  
  rm(xcvTraLs)
  rm(xcvTesLs)
  rm(ycvTraLs)
  
  modelDF["p1", "R2X(cum)"] <- modelDF["p1", "R2X"] <- sum(tcrossprod(tMN, pMN)^2)/ssxTotN
  modelDF[1:(1 + orthoI), "R2X(cum)"] <- cumsum(modelDF[1:(1 + orthoI), "R2X"]);
  
  if (autNcoL) {
    if (all(modelDF[, "Signif."] == "R1", na.rm = TRUE)) {
      orthoI <- noN - 1
    }else{
      orthoI <- noN - 3
    }
    
    if (orthoI == autMaxN - 1){ 
      warning("The maximum number of orthogonal components in the automated mode (", 
              autMaxN - 1, ") has been reached whereas R2Y (", 
              round(modelDF[1 + orthoI, "R2Y"] * 100), 
              "%) is above 1% and Q2Y (", round(modelDF[1 + 
                                                          orthoI, "Q2"] * 100), "%) is still above ", 
              round(ru1ThrN * 100), "%.", call. = FALSE)
    }
    poMN <- poMN[, 1:orthoI, drop = FALSE]
    toMN <- toMN[, 1:orthoI, drop = FALSE]
    woMN <- woMN[, 1:orthoI, drop = FALSE]
    coMN <- coMN[, 1:orthoI, drop = FALSE]
    orthoNamVc <- orthoNamVc[1:orthoI]
    modelDF <- modelDF[c(1:(orthoI + 1), nrow(modelDF)), ]
  }
  
  modelDF["sum", "R2X(cum)"] <- modelDF[1 + orthoI, "R2X(cum)"]
  modelDF["sum", "R2Y(cum)"] <- sum(modelDF[, "R2Y"], na.rm = TRUE)
  modelDF["sum", "Q2(cum)"] <- sum(modelDF[, "Q2"], na.rm = TRUE)
  summaryDF <- modelDF["sum", c("R2X(cum)", "R2Y(cum)", "Q2(cum)")]
  rMN <- wMN
  bMN <- tcrossprod(rMN, cMN)
  yPreScaMN <- tcrossprod(tMN, cMN)
  yPreMN <- scale(scale(yPreScaMN, FALSE, 1/ySdVn), -yMeanVn, FALSE)
  attr(yPreMN, "scaled:center") <- NULL
  attr(yPreMN, "scaled:scale") <- NULL
  
  yActMCN <- yMCN
  yActMN <- yActMCN
  summaryDF[, "RMSEE"] <- sqrt(.errorF(yActMN, yPreMN)^2 * nrow(yActMN)/(nrow(yActMN) - (1 + predI + orthoI)))
  yTestMCN <- NULL
  
  sxpVn <- sapply(1:ncol(tMN), function(h) sum(drop(tcrossprod(tMN[, h], pMN[, h])^2)))
  sxpCumN <- sum(sxpVn)
  sxoVn <- sapply(1:ncol(toMN), function(h) sum(drop(tcrossprod(toMN[, h], poMN[, h])^2)))
  sxoCumN <- sum(sxoVn)
  ssxCumN <- sxpCumN + sxoCumN
  sypVn <- sapply(1:ncol(tMN), function(h) sum(drop(tcrossprod(tMN[, h], cMN[, h])^2)))
  sypCumN <- sum(sypVn)
  syoVn <- sapply(1:ncol(toMN), function(h) sum(drop(tcrossprod(toMN[, 
                                                                     h], coMN[, h])^2)))
  syoCumN <- sum(syoVn)
  ssyCumN <- sypCumN + syoCumN
  kpN <- nrow(wMN)/(sxpCumN/ssxCumN + sypCumN/ssyCumN)
  pNorMN <- sweep(pMN, 2, sqrt(colSums(pMN^2)), "/")
  vipVn <- sqrt(kpN * (rowSums(sweep(pNorMN^2, 2, sxpVn, 
                                     "*"))/ssxCumN + rowSums(sweep(pNorMN^2, 2, sypVn, 
                                                                   "*"))/ssyCumN))
  koN <- nrow(wMN)/(sxoCumN/ssxCumN + syoCumN/ssyCumN)
  poNorMN <- sweep(poMN, 2, sqrt(colSums(poMN^2)),"/")
  orthoVipVn <- sqrt(koN * (rowSums(sweep(poNorMN^2, 
                                          2, sxoVn, "*"))/ssxCumN + rowSums(sweep(poNorMN^2, 
                                                                                  2, syoVn, "*"))/ssyCumN))
  
  summaryDF[, "pre"] <- predI
  summaryDF[, "ort"] <- orthoI
  rownames(summaryDF) <- "Total"
  sigNamVc <- c("R2X", "R2X(cum)", "R2Y", "R2Y(cum)", "Q2", 
                "Q2(cum)", "RMSEE", "RMSEP")
  for (namC in intersect(colnames(modelDF), sigNamVc)) modelDF[, 
                                                               namC] <- signif(modelDF[, namC], 3)
  for (namC in intersect(colnames(summaryDF), sigNamVc)) summaryDF[, 
                                                                   namC] <- signif(summaryDF[, namC], 3)
  retLs <- list(typeC = NULL, modelDF = modelDF, 
                summaryDF = summaryDF, pcaVarVn = varVn, 
                vipVn = vipVn, orthoVipVn = orthoVipVn, fitted = NULL, 
                tested = NULL, coefficients = bMN, residuals = NULL, 
                xMeanVn = xMeanVn, xSdVn = xSdVn, yMeanVn = yMeanVn, 
                ySdVn = ySdVn, xZeroVarVi = NULL, scoreMN = tMN, loadingMN = pMN, 
                weightMN = wMN, orthoScoreMN = toMN, orthoLoadingMN = poMN, 
                orthoWeightMN = woMN, cMN = cMN, uMN = uMN, weightStarMN = rMN, 
                coMN = coMN, suppLs = list(yLevelVc = NULL, naxL = naxL, nayL = nayL, nayVi = nayVi, 
                                           permMN = NULL, scaleC = scaleC, topLoadI = NULL, 
                                           yMCN = yMCN, xSubIncVarMN = NULL, xCorMN = NULL, 
                                           xModelMN = xMN, yModelMN = yMN, yPreMN = yPreMN, 
                                           yTesMN = yTesMN))
}

.errorF <- function(x, y){
  sqrt(mean(drop((x - y)^2), na.rm = TRUE))
}

.similarityF <- function(x, y) {
  return(cor(x, y, use = "pairwise.complete.obs"))
} 

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

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
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsda$vip.mat),5)));
  }else if(type == "coef"){
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsda$coef.mat),5)));
  }else{
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsr$imp.loads),5)));
  }
}

GetPLSSigRowNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(rownames(mSetObj$analSet$plsda$vip.mat));
  }else if(type == "coef"){
    return(rownames(mSetObj$analSet$plsda$coef.mat));
  }else{
    return(rownames(mSetObj$analSet$plsr$imp.loads))
  }
}

GetPLSSigColNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(colnames(mSetObj$analSet$plsda$vip.mat));
  }else if(type == "coef"){
    return(colnames(mSetObj$analSet$plsda$coef.mat));
  }else{
    return(colnames(mSetObj$analSet$plsr$imp.loads));
  }
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
  return(signif(mSetObj$analSet$plsda$fit.info, 5));
}

GetMaxPLSPairComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetMaxPLSCVComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2]));
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
  names(mSetObj$analSet$plsr$load.x.uniq);
}

GetPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$plsr$load.x.uniq;
}

GetPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(cbind(mSetObj$analSet$plsr$load.x.uniq, mSetObj$analSet$plsr$imp.loads[,2]));
}

GetPCALoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pca.axis.lims;
}

GetPCALoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$pca$load.x.uniq);
}

GetPCALoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$pca$load.x.uniq;
}

GetPCALoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(cbind(mSetObj$analSet$pca$load.x.uniq, mSetObj$analSet$pca$imp.loads[,2]));
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

GetOPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$oplsda$splot.mat);
}

GetOPLSLoadColNames <- function(mSetObj=NA){
  return(c("p[1]","p(corr)[1]"));
}

GetOPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$oplsda$splot.mat[,1];
}

GetOPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(mSetObj$analSet$oplsda$splot.mat[,c(1,3)]);
}

#'Compute within group and between group sum of squares
#'(BSS/WSS) for each row of a matrix which may have NA
#'@description Columns have labels, x is a numeric vector,
#'cl is consecutive integers
#'@param x Numeric vector
#'@param cl Columns
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.bwss<-function(x, cl){
  K <- max(cl) - min(cl) + 1
  tvar <- var.na(x);
  tn <- sum(!is.na(x));
  wvar <- wn <- numeric(K);
  
  for(i in (1:K)) {
    if(sum(cl == (i + min(cl) - 1)) == 1){
      wvar[i] <- 0;
      wn[i] <- 1;
    }
    
    if(sum(cl == (i + min(cl) - 1)) > 1) {
      wvar[i] <- var.na(x[cl == (i + min(cl) - 1)]);
      wn[i] <- sum(!is.na(x[cl == (i + min(cl) - 1)]));
    }
  }
  
  WSS <- sum.na(wvar * (wn - 1));
  TSS <- tvar * (tn - 1)
  (TSS - WSS)/WSS;
}

sum.na <- function(x,...){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 0)
    res <- sum(x[tmp])
  res
}

var.na <- function(x){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 1){
    res <- var(x[tmp])
  }
  res
}
