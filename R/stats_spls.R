### R script for MetaboAnalyst
### Description: perform PCA/PLS-DA/Orthogonal PLS-DA/sparse PLS-DA
### Author: Jeff Xia, jeff.xia@mcgill.ca
### McGill University, Canada
### License: GNU GPL (>= 2)

#'Perform SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) 
#'@param mSetObj Input name of the created mSet Object
#'@param comp.num Input the number of computations to run 
#'@param var.num Input the number of variables
#'@param compVarOpt Input the option to perform SPLS-DA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SPLSR.Anal <- function(mSetObj=NA, comp.num, var.num, compVarOpt){
  
  mSetObj <- .get.mSet(mSetObj);
  
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
  
  # note, standardize the cls, to minimize the impact of categorical to numerical impact
  cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  mSetObj$analSet$splsr <- splsda(datmat, cls, ncomp=comp.num, keepX=comp.var.nums);
  score.mat <- mSetObj$analSet$splsr$variates$X;
  write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="splsda_score.csv");
  load.mat <- score.mat <- mSetObj$analSet$splsr$loadings$X;
  write.csv(signif(load.mat,5), file="splsda_loadings.csv");
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
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$spls.pair <- imgName;
  
  if(pc.num > mSetObj$analSet$splsr$ncomp){
    pc.num <- mSetObj$analSet$splsr$ncomp;
  }
  vars <- round(100*mSetObj$analSet$splsr$explained_variance$X,1);
  my.data <- mSetObj$analSet$splsr$variates$X[,1:pc.num];
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%");
  ellipse::pairs(my.data, col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels)
  dev.off();
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
PlotSPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0){
  
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
  
  mSetObj$imgSet$spls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$splsr$variates$X[,inx1];
  lv2 <- mSetObj$analSet$splsr$variates$X[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx1],1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx2],1), "%)");
  
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
  x.ext <- (xrg[2]-xrg[1])/12;
  y.ext <- (yrg[2]-yrg[1])/12;
  xlims <- c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims <- c(yrg[1]-y.ext, yrg[2]+y.ext);
  
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
    if (length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.25), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.25), border=NA);
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
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  spls3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  spls3d$score$colors <- cols;
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(spls3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  mSetObj$imgSet$spls.score3d <- imgName;
  return(.set.mSet(mSetObj));
}

#'Plot sPLS-DA 3D score plot
#'@description This function creates two 3D sPLS-DA score plots, the first is static for Analysis Report purposes, as well as 
#'an interactive 3D plot using the plotly R package. The 3D score plot is saved in the created mSetObj (mSetObj$imgSet$splsda.3d).
#'To view the score plot, if the name of your mSetObj is mSet, enter "mSet$imgSet$splsda.3d" to view the interactive score plot.
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@param angl Input the angle
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@importFrom plotly plot_ly add_markers layout
PlotSPLS3DScoreImg<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl){
  
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
  
  mSetObj$imgSet$spls.score3d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
  
  spls3d <- list();
  
  ## need to check if only 2 or 3 components generated
  if(length(mSetObj$analSet$splsr$explained_variance$X)==2){
    spls3d$score$axis <- paste("Component", c(inx1, inx2), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2)], 1), "%)", sep="");    
    coords <- data.frame(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2)], 5));
    spls3d$score$axis <- c(spls3d$score$axis, "Component3 (NA)");
    coords <- rbind(coords, "comp 3"=rep (0, ncol(coords)));
  }else{
    spls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2, inx3)], 1), "%)", sep="");    
    coords <- data.frame(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2, inx3)], 5));
  }
  
  xlabel <- spls3d$score$axis[1]
  ylabel <- spls3d$score$axis[2]
  zlabel <- spls3d$score$axis[3]
  
  # static
  cols <- GetColorSchema(mSetObj);
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  uniq.cols <- unique(cols);
  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(coords[,inx1], coords[,inx2], coords[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle = angl, color=cols, pch=pchs, box=F);
  legend("topleft", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  dev.off();
  
  if(!.on.public.web){
    # 3D View using plotly
    if(length(uniq.pchs) > 3){
      col <- RColorBrewer::brewer.pal(length(uniq.pchs), "Set3")
    }else{
      col <- c("#1972A4", "#FF7070")
    }
    p <- plotly::plot_ly(x = coords[, inx1], y = coords[, inx2], z = coords[, inx3],
                         color = mSetObj$dataSet$cls, colors = col)
    p <- plotly::add_markers(p, sizes = 5)
    p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                        yaxis = list(title = ylabel),
                                        zaxis = list(title = zlabel)))
    
    mSetObj$imgSet$splsda.3d <- p;
    print("The Interactive 3D sPLS-DA plot has been created, please find it in mSet$imgSet$splsda.3d.")
  }
  return(.set.mSet(mSetObj));
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
  h <- w;
  
  mSetObj$imgSet$spls.imp <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotImpVar(mSetObj, imp.vec, paste ("Loadings", inx), 999, FALSE);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Create SPLS-DA classification plot
#'@description Sparse PLS-DA (from mixOmics) plot of 
#'classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param validOpt "Mfold"
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import caret
PlotSPLSDA.Classification <- function(mSetObj=NA, imgName, validOpt="Mfold", format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- perf.splsda(mSetObj$analSet$splsr, dist= "centroids.dist", validation=validOpt, folds = 5);
  res <- res$error.rate$overall;
  mSetObj$analSet$splsr$error.rate <- res;
  
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

#'Perform sPLS-DA
#'@description Sparse PLS functions (adapted from mixOmics package for web-based usage)
#'this function is a particular setting of internal_mint.block
#'the formatting of the input is checked in internal_wrapper.mint
#'@param X numeric matrix of predictors
#'@param Y a factor or a class vector for the discrete outcome
#'@param ncomp the number of components to include in the model. Default to 2.
#'@param mode Default set to c("regression", "canonical", "invariant", "classic")
#'@param keepX Number of \eqn{X} variables kept in the model on the last components (once all keepX.constraint[[i]] are used).
#'@param keepX.constraint A list containing which variables of X are to be kept on each of the first PLS-components.
#'@param scale Boleean. If scale = TRUE, each block is standardized to zero means and unit variances (default: TRUE).
#'@param tol Convergence stopping value.
#'@param max.iter integer, the maximum number of iterations.
#'@param near.zero.var boolean, see the internal \code{\link{nearZeroVar}} function (should be set to TRUE in particular for data with many zero values). 
#'Setting this argument to FALSE (when appropriate) will speed up the computations
#'@param logratio "None" by default, or "CLR"
#'@param multilevel Designate multilevel design, "NULL" by default
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

splsda <- function(X,
                   Y,
                   ncomp = 2,
                   mode = c("regression", "canonical", "invariant", "classic"),
                   keepX,
                   keepX.constraint=NULL,
                   scale = TRUE,
                   tol = 1e-06,
                   max.iter = 100,
                   near.zero.var = FALSE,
                   logratio = "none",   # one of "none", "CLR"
                   multilevel = NULL)    # multilevel is passed to multilevel(design = ) in withinVariation. Y is ommited and shouldbe included in multilevel design
{
  
  
  #-- validation des arguments --#
  # most of the checks are done in the wrapper.mint.spls.hybrid function
  if(is.null(multilevel))
  {
    if (is.null(Y))
      stop("'Y' has to be something else than NULL.")
    
    if (is.null(dim(Y)))
    {
      Y = factor(Y)
    }  else {
      stop("'Y' should be a factor or a class vector.")
    }
    
    if (nlevels(Y) == 1)
      stop("'Y' should be a factor with more than one level")
    
    Y.mat = unmap(Y)
    colnames(Y.mat) = levels(Y)#paste0("Y", 1:ncol(Y.mat))
  }else{
    # we expect a vector or a 2-columns matrix in 'Y' and the repeated measurements in 'multilevel'
    multilevel = data.frame(multilevel)
    
    if ((nrow(X) != nrow(multilevel)))
      stop("unequal number of rows in 'X' and 'multilevel'.")
    
    if (ncol(multilevel) != 1)
      stop("'multilevel' should have a single column for the repeated measurements, other factors should be included in 'Y'.")
    
    if (!is.null(ncol(Y)) && !ncol(Y) %in% c(0,1,2))# multilevel 1 or 2 factors
      stop("'Y' should either be a factor, a single column data.frame containing a factor, or a 2-columns data.frame containing 2 factors.")
    
    multilevel = data.frame(multilevel, Y)
    multilevel[, 1] = as.numeric(factor(multilevel[, 1])) # we want numbers for the repeated measurements
    
    Y.mat = NULL
  }
  
  # call to 'internal_wrapper.mint'
  result = internal_wrapper.mint(X = X, Y = Y.mat, ncomp = ncomp, scale = scale, near.zero.var = near.zero.var, mode = mode,
                                 keepX = keepX, keepX.constraint = keepX.constraint, max.iter = max.iter, tol = tol, logratio = logratio,
                                 multilevel = multilevel, DA = TRUE)
  
  # choose the desired output from 'result'
  out = list(
    call = match.call(),
    X = result$X[-result$indY][[1]],
    Y = if (is.null(multilevel))
    {
      Y
    } else {
      result$Y.factor
    },
    ind.mat = result$X[result$indY][[1]],
    ncomp = result$ncomp,
    mode = result$mode,
    keepX = result$keepA[[1]],
    keepY = result$keepA[[2]],
    keepX.constraint = result$keepA.constraint[[1]],
    keepY.constraint = result$keepA.constraint[[2]],
    variates = result$variates,
    loadings = result$loadings,
    names = result$names,
    tol = result$tol,
    iter = result$iter,
    max.iter = result$max.iter,
    nzv = result$nzv,
    scale = scale,
    logratio = logratio,
    explained_variance = result$explained_variance,#[-result$indY],
    input.X = result$input.X
  )
  
  class(out) = c("splsda","spls","DA")
  # output if multilevel analysis
  if (!is.null(multilevel))
  {
    out$multilevel = multilevel
    class(out) = c("mlsplsda",class(out))
  }
  
  return(invisible(out))
}

internal_wrapper.mint = function(X,
                                 Y,
                                 study,
                                 ncomp = 2,
                                 keepX.constraint,
                                 keepY.constraint,
                                 keepX,
                                 keepY,
                                 mode,
                                 scale = FALSE,
                                 near.zero.var = FALSE,
                                 max.iter = 100,
                                 tol = 1e-06,
                                 logratio = "none",   # one of "none", "CLR"
                                 DA = FALSE,           # indicate whether it's a DA analysis, only used for the multilvel approach with withinVariation
                                 multilevel = NULL)    # multilevel is passed to multilevel(design=) in withinVariation. Y is ommited and should be included in multilevel design
{
  
  if (is.null(ncomp) || !is.numeric(ncomp) || ncomp <= 0 || length(ncomp)>1)
    stop("invalid number of variates, 'ncomp'.")
  
  #-- validation des arguments --#
  
  check = Check.entry.pls(X, Y, ncomp, keepX, keepY, keepX.constraint, keepY.constraint, mode=mode, scale=scale,
                          near.zero.var=near.zero.var, max.iter=max.iter ,tol=tol ,logratio=logratio ,DA=DA, multilevel=multilevel)
  X = check$X
  input.X = X # save the checked X, before logratio/multileve/scale
  Y = check$Y
  ncomp = check$ncomp
  mode = check$mode
  keepX.constraint = check$keepX.constraint
  keepY.constraint = check$keepY.constraint
  keepX = check$keepX
  keepY = check$keepY
  nzv.A = check$nzv.A
  
  #set the default study factor
  if (missing(study))
  {
    study = factor(rep(1,nrow(X)))
  } else {
    study = as.factor(study)
  }
  if (length(study) != nrow(X))
    stop(paste0("'study' must be a factor of length ",nrow(X),"."))
  
  if (any(table(study) <= 1))
    stop("At least one study has only one sample, please consider removing before calling the function again")
  if (any(table(study) < 5))
    warning("At least one study has less than 5 samples, mean centering might not do as expected")
  
  design = matrix(c(0,1,1,0), ncol = 2, nrow = 2, byrow = TRUE)
  
  
  #-----------------------------#
  #-- logratio transformation --#
  
  X = logratio.transfo(X=X, logratio=logratio)
  
  #as X may have changed
  if (ncomp > min(ncol(X), nrow(X)))
    stop("'ncomp' should be smaller than ", min(ncol(X), nrow(X)), call. = FALSE)
  
  #-- logratio transformation --#
  #-----------------------------#
  
  
  #---------------------------------------------------------------------------#
  #-- multilevel approach ----------------------------------------------------#
  
  if (!is.null(multilevel))
  {
    if (!DA)
    {
      Xw = withinVariation(X, design = multilevel)
      Yw = withinVariation(Y, design = multilevel)
      X = Xw
      Y = Yw
    } else {
      Xw = withinVariation(X, design = multilevel)
      X = Xw
      
      #-- Need to set Y variable for 1 or 2 factors
      Y = multilevel[, -1,drop=FALSE]
      if (ncol(Y)>0)
        Y = apply(Y, 1, paste, collapse = ".")  #  paste is to combine in the case we have 2 levels
      
      Y = as.factor(Y)
      Y.factor = Y
      Y = unmap(Y)
      colnames(Y) = levels(Y)
      rownames(Y) = rownames(X)
      # if DA keepY should be all the levels (which is not happening in the check because of multilevel
      keepY = rep(ncol(Y),ncomp)
    }
  }
  #-- multilevel approach ----------------------------------------------------#
  #---------------------------------------------------------------------------#
  
  
  #---------------------------------------------------------------------------#
  #-- pls approach ----------------------------------------------------#
  
  result = internal_mint.block(A = list(X = X, Y = Y), indY = 2, mode = mode, ncomp = c(ncomp, ncomp), tol = tol, max.iter = max.iter,
                               design = design, keepA = list(keepX, keepY), keepA.constraint = list(keepX.constraint, keepY.constraint),
                               scale = scale, scheme = "horst",init="svd", study = study)
  
  #-- pls approach ----------------------------------------------------#
  #---------------------------------------------------------------------------#
  
  result$ncomp = ncomp
  if(near.zero.var)
    result$nzv = nzv.A
  
  if(!is.null(multilevel) & DA)
    result$Y.factor = Y.factor
  
  result$input.X = input.X
  
  class(result) = c("mint.spls.hybrid")
  return(invisible(result))
  
}

internal_mint.block = function (A, indY = NULL,  design = 1 - diag(length(A)), tau=NULL,#rep(1, length(A)),
                                ncomp = rep(1, length(A)), scheme = "horst", scale = TRUE,  bias = FALSE,
                                init = "svd.single", tol = 1e-06, verbose = FALSE,
                                mode = "canonical", max.iter = 100,study = NULL, keepA = NULL,
                                keepA.constraint = NULL, penalty = NULL)
{
  # A: list of matrices
  # indY: integer, pointer to one of the matrices of A
  # design: design matrix, links between matrices. Diagonal must be 0
  # ncomp: vector of ncomp, per matrix
  # scheme: a function "g", refer to the article (thanks Benoit)
  # scale: do you want to scale ? mean is done by default and cannot be changed (so far)
  # bias: scale the data with n or n-1
  # init: one of "svd" or "random", initialisation of the algorithm
  # tol: nobody cares about this
  # verbose: show the progress of the algorithm
  # mode: canonical, classic, invariant, regression
  # max.iter: nobody cares about this
  # study: factor for each matrix of A, must be a vector
  # keepA: keepX of spls for each matrix of A. must be a list. Each entry must be of the same length (max ncomp)
  # keepA.constraint: keepX.constraint, which variables are kept on the first num.comp-1 components. It is a list of characters
  # near.zero.var: do you want to remove variables with very small variance
  
  names(ncomp) = names(A)
  
  # keepA is updated to be of length length(A) now, the first entries correspond to the keepA.constraint if it was provided
  for (q in 1:length(A))
    keepA[[q]] = c(unlist(lapply(keepA.constraint[[q]], length)), keepA[[q]]) #of length ncomp, can contains 0
  
  # center the data per study, per matrix of A, scale if scale=TRUE, option bias
  mean_centered = lapply(A, function(x){mean_centering_per_study(x, study, scale, bias)})
  A = lapply(mean_centered, function(x){as.matrix(x$concat.data)})
  ni = table(study) #number of samples per study
  
  ### Start: Initialization parameters
  pjs = sapply(A, NCOL)
  nb_ind = NROW(A[[1]])
  J = length(A)
  R = A # R: residuals matrices, will be a list of length ncomp
  N = max(ncomp)
  
  AVE_inner = AVE_outer = rep(NA, max(ncomp))
  defl.matrix = AVE_X = crit = loadings.partial.A = variates.partial.A = tau.rgcca = list()
  P = loadings.A = loadings.Astar = c = t = b = variates.A = vector("list", J)
  
  for (k in 1:J)
    t[[k]] = variates.A[[k]] = matrix(NA, nb_ind, N)
  
  for (k in 1:J)
    P[[k]] = loadings.A[[k]] = loadings.Astar[[k]]= matrix(NA, pjs[[k]], N)
  
  for (k in 1:J)
  {
    loadings.partial.A[[k]] = variates.partial.A[[k]] = vector("list", length = nlevels(study))
    for(m in 1:nlevels(study))
    {
      loadings.partial.A[[k]][[m]] = matrix(nrow = NCOL(A[[k]]), ncol = N)
      variates.partial.A[[k]][[m]] = matrix(nrow = ni[m], ncol = N)
    }
  }
  
  defl.matrix[[1]] = A
  ndefl = ncomp - 1
  J2 = J-1
  
  if (is.vector(tau))
    tau = matrix(rep(tau, N), nrow = N, ncol = length(tau), byrow = TRUE)
  ### End: Initialization parameters
  
  
  iter=NULL
  for (n in 1 : N)
  {
    ### Start: Estimation ai
    if (is.null(tau))
    {
      mint.block.result = sparse.mint.block_iteration(R, design, study = study,
                                                      keepA.constraint = if (!is.null(keepA.constraint)) {lapply(keepA.constraint, function(x){unlist(x[n])})} else {NULL} ,
                                                      keepA = if (!is.null(keepA)) {lapply(keepA, function(x){x[n]})} else {NULL},
                                                      scheme = scheme, init = init, max.iter = max.iter, tol = tol, verbose = verbose,penalty = penalty)
    } 
    ### End: Estimation ai
    
    if(is.null(tau))
    {
      #recording loadings.partials, $Ai$study[,ncomp]
      # recording variates.partials, $Ai[,ncomp]
      for(k in 1:J)
      {
        for(m in 1:nlevels(study))
        {
          loadings.partial.A[[k]][[m]][, n] = matrix(mint.block.result$loadings.partial.A.comp[[k]][[m]], ncol=1)
          variates.partial.A[[k]][[m]][, n] = matrix(mint.block.result$variates.partial.A.comp[[k]][[m]], ncol=1)
        }
        #variates.partial.A[[k]][,n]=matrix(unlist(mint.block.result$variates.partial.A.comp[[k]]),ncol=1)
      }
    }
    
    AVE_inner[n] = mint.block.result$AVE_inner
    crit[[n]] = mint.block.result$crit
    tau.rgcca[[n]] = mint.block.result$tau
    
    for (k in 1:J)
      variates.A[[k]][, n] = mint.block.result$variates.A[, k]
    
    # deflation if there are more than 1 component and if we haven't reach the max number of component (N)
    if (N != 1 & n != N)
    {
      defla.result = defl.select(mint.block.result$variates.A, R, ndefl, n, nbloc = J, indY = indY, mode = mode, aa = mint.block.result$loadings.A)
      R = defla.result$resdefl
      defl.matrix[[n + 1]] = R
    }
    
    for (k in 1 : J)
    {
      if (N != 1)
      {
        P[[k]][, n - 1] = defla.result$pdefl[[k]]
      }
      loadings.A[[k]][, n] = mint.block.result$loadings.A[[k]]
    }
    
    if (n == 1)
    {
      for (k in 1 : J)
        loadings.Astar[[k]][, n] = mint.block.result$loadings.A[[k]]
    } else {
      for (k in 1 : J)
        loadings.Astar[[k]][, n] = mint.block.result$loadings.A[[k]] - loadings.Astar[[k]][, (1 : n - 1), drop = F] %*% drop(t(loadings.A[[k]][, n]) %*% P[[k]][, 1 : (n - 1), drop = F])
    }
    iter = c(iter, mint.block.result$iter)
  }
  
  if (verbose)
    cat(paste0("Computation of the SGCCA block components #", N , " is under progress...\n"))
  
  shave.matlist = function(mat_list, nb_cols) mapply(function(m, nbcomp) m[, 1:nbcomp, drop = FALSE], mat_list, nb_cols, SIMPLIFY = FALSE)
  shave.veclist = function(vec_list, nb_elts) mapply(function(m, nbcomp) m[1:nbcomp], vec_list, nb_elts, SIMPLIFY = FALSE)
  
  for (k in 1:J)
  {
    rownames(loadings.A[[k]]) = rownames(loadings.Astar[[k]])=colnames(A[[k]])
    rownames(variates.A[[k]]) = rownames(A[[k]])
    colnames(variates.A[[k]]) = colnames(loadings.A[[k]]) = paste0("comp ", 1:max(ncomp))
    AVE_X[[k]] = apply(cor(A[[k]], variates.A[[k]])^2, 2, mean)
    if (is.null(tau))
    {
      names(loadings.partial.A[[k]]) = names(variates.partial.A[[k]]) = levels(study)
      for (m in 1:nlevels(study))
      {
        rownames(loadings.partial.A[[k]][[m]]) = colnames(A[[k]])
        colnames(loadings.partial.A[[k]][[m]]) = paste0("comp ", 1:max(ncomp))
        rownames(variates.partial.A[[k]][[m]]) = rownames(mean_centered[[1]]$rownames.study[[m]])
        colnames(variates.partial.A[[k]][[m]]) = paste0("comp ", 1:max(ncomp))
      }
    }
  }
  
  outer = matrix(unlist(AVE_X), nrow = max(ncomp))
  for (j in 1 : max(ncomp))
    AVE_outer[j] = sum(pjs * outer[j, ])/sum(pjs)
  
  variates.A = shave.matlist(variates.A, ncomp)
  AVE_X = shave.veclist(AVE_X, ncomp)
  AVE = list(AVE_X = AVE_X, AVE_outer = AVE_outer, AVE_inner = AVE_inner)
  
  #calcul explained variance
  A_split=lapply(A, study_split, study) #split the data per study
  
  expl.A=lapply(1:length(A),function(x){
    if (nlevels(study) == 1)
    {
      temp = suppressWarnings(explained_variance(A[[x]], variates = variates.A[[x]], ncomp = ncomp[[x]]))
    }else{
      temp = lapply(1:nlevels(study), function(y){
        suppressWarnings(explained_variance(A_split[[x]][[y]], variates = variates.partial.A[[x]][[y]], ncomp = ncomp[[x]]))})
      temp[[length(temp)+1]] = explained_variance(A[[x]], variates = variates.A[[x]], ncomp = ncomp[[x]])
      names(temp) = c(levels(study), "all data")
    }
    temp
  })
  names(expl.A) = names(A)
  
  ### Start: output
  names(loadings.A) = names(variates.A) = names(A)
  
  if (is.null(tau))
    names(loadings.partial.A) = names(variates.partial.A) = names(A)
  
  names = lapply(1:J, function(x) {colnames(A[[x]])})
  names(names) = names(A)
  names[[length(names) + 1]] = row.names(A[[1]])
  names(names)[length(names)] = "indiv"
  
  out = list(X = A, indY = indY, ncomp = ncomp, mode = mode,
             keepA = keepA, keepA.constraint = keepA.constraint,
             variates = variates.A, loadings = shave.matlist(loadings.A, ncomp),
             variates.partial= if(is.null(tau)) {variates.partial.A} ,loadings.partial= if(is.null(tau)) {loadings.partial.A},
             loadings.star = shave.matlist(loadings.Astar, ncomp),
             names = list(sample = row.names(A[[1]]), colnames = lapply(A, colnames), blocks = names(A)),
             tol = tol, iter=iter, max.iter=max.iter,
             design = design,
             scheme = scheme,  crit = crit, AVE = AVE, defl.matrix = defl.matrix,
             init = init, bias = bias,
             scale = scale, tau = if(!is.null(tau)) tau.rgcca, study = study,
             explained_variance = expl.A)
  ### End: Output
  
  return(out)
}

# Performance for PLS-DA and sPLS-DA
# Performance prediction for PLS-DA and sPLS-DA
# Jeff Xia \email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
perf.splsda <- function(object,
                        dist = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"),
                        constraint = FALSE,
                        validation = c("Mfold", "loo"),
                        folds = 5,
                        nrepeat = 1,
                        auc = FALSE,
                        progressBar = FALSE,
                        cpus,
                        ...){
  
  #-- initialising arguments --#
  # these data are the centered and scaled X output or the unmapped(Y) scaled and centered
  X = object$input.X
  level.Y = object$names$colnames$Y  #to make sure the levels are ordered
  Y = object$Y
  ncomp = object$ncomp
  n = nrow(X)
  
  logratio = object$logratio
  if (is.null(logratio))
    logratio = "none"
  
  multilevel = object$multilevel # repeated measurement and Y
  near.zero.var = !is.null(object$nzv) # if near.zero.var was used, we set it to TRUE. if not used, object$nzv is NULL
  
  if(any(class(object) == "plsda") & constraint == TRUE)
  {
    constraint = FALSE #no need as all variable will be included
    warning("'constraint' is set to FALSE as all variables are selected on all components (plsda object).")
  }
  #-- tells which variables are selected in X and in Y --#
  if(constraint)
  {
    keepX.constraint = apply(object$loadings$X, 2, function(x){names(which(x!=0))})
    # gives a matrix of ncomp columns, I want a list of length ncomp
    keepX.constraint = split(keepX.constraint, rep(1:ncol(keepX.constraint), each = nrow(keepX.constraint)))
    names(keepX.constraint) = paste("comp",1:length(keepX.constraint),sep="")
    
    #keepX = NULL
  } else {
    #keepX.constraint = NULL
    if (any(class(object) == "splsda"))
    {
      keepX = object$keepX
    } else {
      keepX = rep(ncol(X), ncomp)
    }
  }
  
  tol = object$tol
  max.iter = object$max.iter
  
  # initialize new objects:
  features = list()
  for(k in 1:ncomp)
    features[[k]] = NA
  
  # check input arguments
  
  if (hasArg(method.predict))
    stop("'method.predict' argument has been replaced by 'dist' to match the 'tune' function")
  method.predict = NULL # to pass R CMD check
  
  dist = match.arg(dist, choices = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"), several.ok = TRUE)
  if (any(dist == "all"))
  {
    nmthdd = 3
    dist = c("max.dist", "centroids.dist", "mahalanobis.dist")
  } else {
    nmthdd = length(dist)
  }
  
  if (length(validation) > 1 )
    validation = validation [1]
  if (!(validation %in% c("Mfold", "loo")))
    stop("Choose 'validation' among the two following possibilities: 'Mfold' or 'loo'")
  
  if (validation == "loo")
  {
    if (nrepeat != 1)
      warning("Leave-One-Out validation does not need to be repeated: 'nrepeat' is set to '1'.")
    nrepeat = 1
  }
  
  if (!is.logical(progressBar))
    stop("'progressBar' must be either TRUE or FALSE")
  
  measure = c("overall","BER") # one of c("overall","BER")
  
  
  if (!(logratio %in% c("none", "CLR")))
    stop("Choose one of the two following logratio transformation: 'none' or 'CLR'")
  #fold is checked in 'MCVfold'
  
  if(!missing(cpus))
  {
    if(!is.numeric(cpus) | length(cpus)!=1)
      stop("'cpus' must be a numerical value")
    
    parallel = TRUE
    cl = makeCluster(cpus, type = "SOCK")
    clusterExport(cl, c("splsda","selectVar"))
  } else {
    parallel = FALSE
    cl = NULL
  }
  
  
  #---------------------------------------------------------------------------#
  #-- logration + multilevel approach ----------------------------------------#
  # we can do logratio and multilevel on the whole data as these transformation are done per sample
  X = logratio.transfo(X = X, logratio = logratio)
  if (!is.null(multilevel))
  {
    Xw = withinVariation(X, design = multilevel)
    X = Xw
  }
  #-- logratio + multilevel approach -----------------------------------------#
  #---------------------------------------------------------------------------#
  
  
  # -------------------------------------
  # added: first check for near zero var on the whole data set
  if (near.zero.var == TRUE)
  {
    nzv = nearZeroVar(X)
    if (length(nzv$Position > 0))
    {
      warning("Zero- or near-zero variance predictors.\nReset predictors matrix to not near-zero variance predictors.\nSee $nzv for problematic predictors.")
      X = X[, -nzv$Position, drop=TRUE]
      
      if (ncol(X)==0)
        stop("No more predictors after Near Zero Var has been applied!")
      
      if (any(keepX > ncol(X)))
        keepX = ncol(X)
      
    }
  }
  # and then we start from the X data set with the nzv removed
  
  
  
  list.features = list()
  
  mat.error.rate = mat.sd.error = mat.mean.error = error.per.class.keepX.opt = list()
  error.per.class = list()
  final=list()
  
  for (measure_i in measure)
  {
    mat.sd.error[[measure_i]] = matrix(0,nrow = ncomp, ncol = length(dist),
                                       dimnames = list(c(paste('comp', 1 : ncomp)), dist))
    mat.mean.error[[measure_i]] = matrix(0,nrow = ncomp, ncol = length(dist),
                                         dimnames = list(c(paste('comp', 1 : ncomp)), dist))
    error.per.class.keepX.opt[[measure_i]] = list()
    mat.error.rate[[measure_i]]=list()
    for(ijk in dist)
    {
      mat.error.rate[[measure_i]][[ijk]] = array(0, c(nlevels(Y),  nrepeat ,ncomp),
                                                 dimnames = list(c(levels(Y)),c(paste('nrep', 1 : nrepeat)),c(paste('comp', 1 : ncomp))))
      
      error.per.class.keepX.opt[[measure_i]][[ijk]] = matrix(nrow = nlevels(Y), ncol = ncomp,
                                                             dimnames = list(c(levels(Y)), c(paste('comp', 1 : ncomp))))
    }
  }
  
  if(auc == TRUE)
  {
    auc.mean=list()
    auc.all=list()
  }
  
  prediction.all = class.all = auc.mean = auc.all = list()
  for(ijk in dist)
  {
    class.all[[ijk]] = array(0, c(nrow(X),  nrepeat ,ncomp),
                             dimnames = list(rownames(X),c(paste('nrep', 1 : nrepeat)),c(paste('comp', 1 : ncomp))))
  }
  
  for (comp in 1 : ncomp)
  {
    if (progressBar == TRUE)
      cat("\ncomp",comp, "\n")
    
    if(constraint)
    {
      
      if(comp > 1)
      {
        choice.keepX.constraint = keepX.constraint[1 : (comp - 1)]
      } else {
        choice.keepX.constraint = NULL
      }
      test.keepX = keepX.constraint[comp]
      #names(test.keepX) = test.keepX
      #test.keepX is a vector a variables to keep on comp 'comp'
    } else {
      if(comp > 1)
      {
        choice.keepX = keepX[1 : (comp - 1)]
      } else {
        choice.keepX = NULL
      }
      test.keepX = keepX[comp]
      names(test.keepX) = test.keepX
      #test.keepX is a value
    }
    
    # estimate performance of the model for each component
    result = MCVfold.splsda (X, Y, multilevel = multilevel, validation = validation, folds = folds, nrepeat = nrepeat, ncomp = comp,
                             choice.keepX = if(constraint){NULL}else{choice.keepX},
                             choice.keepX.constraint = if(constraint){choice.keepX.constraint}else{NULL},
                             test.keepX = test.keepX, measure = measure, dist = dist, near.zero.var = near.zero.var,
                             auc = auc, progressBar = progressBar, class.object = class(object), cl = cl)
    
    # ---- extract stability of features ----- # NEW
    if (any(class(object) == "splsda"))
      list.features[[comp]] = result$features$stable
    
    for (ijk in dist)
    {
      for (measure_i in measure)
      {
        mat.error.rate[[measure_i]][[ijk]][ ,,comp] = result[[measure_i]]$mat.error.rate[[ijk]][,1]
        mat.mean.error[[measure_i]][comp, ijk]=result[[measure_i]]$error.rate.mean[[ijk]]
        if (!is.null(result[[measure_i]]$error.rate.sd))
        {
          mat.sd.error[[measure_i]][comp, ijk]=result[[measure_i]]$error.rate.sd[[ijk]]
        } else {
          mat.sd.error= NULL
        }
        # confusion matrix for keepX.opt
        error.per.class.keepX.opt[[measure_i]][[ijk]][ ,comp]=result[[measure_i]]$confusion[[ijk]][,1]
      }
      
      #prediction of each samples for each fold and each repeat, on each comp
      class.all[[ijk]][, , comp] = result$class.comp[[ijk]][,,1]
    }
    prediction.all[[comp]] = array(unlist(result$prediction.comp),c(nrow(result$prediction.comp[[1]]), ncol(result$prediction.comp[[1]]), nrepeat),
                                   dimnames = c(dimnames(result$prediction.comp[[1]])[1:2], list(paste0("nrep",1:nrepeat))))#[[1]][, , 1] #take only one component [[1]] and one of test.keepX [,,1]
    
    if(auc == TRUE)
    {
      auc.all[[comp]] = lapply(result$auc.all, function(x) x[,,1])
      auc.mean[[comp]] = result$auc[, , 1]
    }
  }
  if (parallel == TRUE)
    stopCluster(cl)
  
  names(prediction.all) = paste('comp', 1:ncomp)
  
  result = list(error.rate = mat.mean.error,
                error.rate.sd = mat.sd.error,
                error.rate.all = mat.error.rate,
                error.rate.class = error.per.class.keepX.opt[[1]],
                predict = prediction.all,
                class = class.all)
  
  if(auc)
  {
    names(auc.mean) = c(paste('comp', 1:ncomp))
    result$auc = auc.mean
    
    names(auc.all) = c(paste('comp', 1:ncomp))
    result$auc.all =auc.all
  }
  
  if (any(class(object) == "splsda"))
  {
    names(list.features) = paste('comp', 1:ncomp)
    result$features$stable = list.features
  }
  
  if (progressBar == TRUE)
    cat('\n')
  
  # added
  if (near.zero.var == TRUE)
    result$nzvX = nzv$Position
  
  if (any(class(object) == "splsda"))
  {
    method = "splsda.mthd"
  } else if (any(class(object) == "plsda")) {
    method = "plsda.mthd"
  } else {
    warning("Something that should not happen happened. Please contact us.")
  }
  class(result) = c("perf",paste(c("perf", method), collapse ="."))
  result$call = match.call()
  
  
  #updated outputs
  return(invisible(result))
}

MCVfold.splsda = function(
  X,
  Y,
  multilevel = NULL, # repeated measurement only
  validation,
  folds,
  nrepeat = 1,
  ncomp,
  choice.keepX = NULL, #either choice.keepX or choice.keepX.constraint, not both
  choice.keepX.constraint = NULL,
  test.keepX, # can be either a vector of names (keepX.constraint) or a value(keepX). In case of a value, there needs to be names(test.keepX)
  measure = c("overall"), # one of c("overall","BER")
  dist = "max.dist",
  auc = FALSE,
  max.iter = 100,
  near.zero.var = FALSE,
  progressBar = TRUE,
  class.object = NULL,
  cl
)
{   
  
  pb = FALSE;
  M = length(folds);
  features = features.j = NULL;
  auc.all = prediction.comp = class.comp = list()
  for(ijk in dist)
    class.comp[[ijk]] = array(0, c(nrow(X), nrepeat, length(test.keepX)))# prediction of all samples for each test.keepX and  nrep at comp fixed
  folds.input = folds
  for(nrep in 1:nrepeat)
  {
    prediction.comp[[nrep]] = array(0, c(nrow(X), nlevels(Y), length(test.keepX)), dimnames = list(rownames(X), levels(Y), names(test.keepX)))
    rownames(prediction.comp[[nrep]]) = rownames(X)
    colnames(prediction.comp[[nrep]]) = levels(Y)
    
    if(nlevels(Y)>2)
    {
      auc.all[[nrep]] = array(0, c(nlevels(Y),2, length(test.keepX)), dimnames = list(paste(levels(Y), "vs Other(s)"), c("AUC","p-value"), names(test.keepX)))
    }else{
      auc.all[[nrep]] = array(0, c(1,2, length(test.keepX)), dimnames = list(paste(levels(Y)[1], levels(Y)[2], sep = " vs "), c("AUC","p-value"), names(test.keepX)))
    }
    
    n = nrow(X)
    repeated.measure = 1:n
    if (!is.null(multilevel))
    {
      repeated.measure = multilevel[,1]
      n = length(unique(repeated.measure)) # unique observation: we put every observation of the same "sample" in the either the training or test set
    }
    
    
    #-- define the folds --#
    if (validation ==  "Mfold")
    {
      
      if (nrep > 1) # reinitialise the folds
        folds = folds.input
      
      if (is.null(folds) || !is.numeric(folds) || folds < 2 || folds > n)
      {
        stop("Invalid number of folds.")
      } else {
        M = round(folds)
        if (is.null(multilevel))
        {
          temp = stratified.subsampling(Y, folds = M)
          folds = temp$SAMPLE
          if(temp$stop > 0 & nrep == 1) # to show only once
            warning("At least one class is not represented in one fold, which may unbalance the error rate.\n  
                    Consider a number of folds lower than the minimum in table(Y): ", min(table(Y)))
        } else {
          folds = split(sample(1:n), rep(1:M, length = n)) # needs to have all repeated samples in the same fold
        }
      }
      } else if (validation ==  "loo") {
        folds = split(1:n, rep(1:n, length = n))
        M = n
      }
    
    M = length(folds)
    
    error.sw = matrix(0, nrow = M, ncol = length(test.keepX))
    rownames(error.sw) = paste0("fold",1:M)
    colnames(error.sw) = names(test.keepX)
    # for the last keepX (i) tested, prediction combined for all M folds so as to extract the error rate per class
    # prediction.all = vector(length = nrow(X))
    # in case the test set only includes one sample, it is better to advise the user to
    # perform loocv
    stop.user = FALSE
    
    # function instead of a loop so we can use lapply and parLapply. Can't manage to put it outside without adding all the arguments
    fonction.j.folds = function(j)#for (j in 1:M)
    {
      if (progressBar ==  TRUE)
        setTxtProgressBar(pb, (M*(nrep-1)+j-1)/(M*nrepeat))
      
      #print(j)
      #set up leave out samples.
      omit = which(repeated.measure %in% folds[[j]] == TRUE)
      
      # get training and test set
      X.train = X[-omit, ]
      Y.train = Y[-omit]
      X.test = X[omit, , drop = FALSE]#matrix(X[omit, ], nrow = length(omit)) #removed to keep the colnames in X.test
      Y.test = Y[omit]
      
      #---------------------------------------#
      #-- near.zero.var ----------------------#
      
      # first remove variables with no variance
      var.train = apply(X.train, 2, var)
      ind.var = which(var.train == 0)
      if (length(ind.var) > 0)
      {
        X.train = X.train[, -c(ind.var),drop = FALSE]
        X.test = X.test[, -c(ind.var),drop = FALSE]
        
        # match choice.keepX, choice.keepX.constraint and test.keepX if needed
        if(is.null(choice.keepX.constraint) & !is.list(test.keepX))
        {
          # keepX = c(choice.keepX, test.keepX[i])
          # keepX.constraint = NULL
          
          # reduce choice.keepX and test.keepX if needed
          if (any(choice.keepX > ncol(X.train)))
            choice.keepX[which(choice.keepX>ncol(X.train))] = ncol(X.train)
          
          if (any(test.keepX > ncol(X.train)))
            test.keepX[which(test.keepX>ncol(X.train))] = ncol(X.train)
          
        } else if(!is.list(test.keepX)){
          # keepX = test.keepX[i]
          # keepX.constraint = choice.keepX.constraint
          
          # reduce test.keepX if needed
          if (any(test.keepX > ncol(X.train)))
            test.keepX[which(test.keepX>ncol(X.train))] = ncol(X.train)
          
          choice.keepX.constraint = match.keepX.constraint(names.remove = names(ind.var), keepX.constraint = choice.keepX.constraint)
          
        } else {
          # keepX = NULL
          # keepX.constraint = c(choice.keepX.constraint, test.keepX)
          
          # reduce choice.keepX.constraint if needed
          choice.keepX.constraint = match.keepX.constraint(names.remove = names(ind.var), keepX.constraint = c(choice.keepX.constraint, test.keepX))
          
        }
        
      }
      
      if(near.zero.var == TRUE)
      {
        remove.zero = nearZeroVar(X.train)$Position
        
        if (length(remove.zero) > 0)
        {
          names.var = colnames(X.train)[remove.zero]
          
          X.train = X.train[, -c(remove.zero),drop = FALSE]
          X.test = X.test[, -c(remove.zero),drop = FALSE]
          
          # match choice.keepX, choice.keepX.constraint and test.keepX if needed
          if(is.null(choice.keepX.constraint) & !is.list(test.keepX))
          {
            # keepX = c(choice.keepX, test.keepX[i])
            # keepX.constraint = NULL
            
            # reduce choice.keepX and test.keepX if needed
            if (any(choice.keepX > ncol(X.train)))
              choice.keepX[which(choice.keepX>ncol(X.train))] = ncol(X.train)
            
            if (any(test.keepX > ncol(X.train)))
              test.keepX[which(test.keepX>ncol(X.train))] = ncol(X.train)
            
          } else if(!is.list(test.keepX)){
            # keepX = test.keepX[i]
            # keepX.constraint = choice.keepX.constraint
            
            # reduce test.keepX if needed
            if (any(test.keepX > ncol(X.train)))
              test.keepX[which(test.keepX>ncol(X.train))] = ncol(X.train)
            
            choice.keepX.constraint = match.keepX.constraint(names.remove = names.var, keepX.constraint = choice.keepX.constraint)
            
          } else {
            # keepX = NULL
            # keepX.constraint = c(choice.keepX.constraint, test.keepX)
            
            # reduce choice.keepX.constraint if needed
            choice.keepX.constraint = match.keepX.constraint(names.remove = names.var, keepX.constraint = c(choice.keepX.constraint, test.keepX))
            
          }
          
        }
        #print(remove.zero)
      }
      
      #-- near.zero.var ----------------------#
      #---------------------------------------#
      prediction.comp.j = array(0, c(length(omit), nlevels(Y), length(test.keepX)), dimnames = list(rownames(X.test), levels(Y), names(test.keepX)))
      
      
      class.comp.j = list()
      for(ijk in dist)
        class.comp.j[[ijk]] = matrix(0, nrow = length(omit), ncol = length(test.keepX))# prediction of all samples for each test.keepX and  nrep at comp fixed
      
      
      for (i in 1:length(test.keepX))
      {
        if (progressBar ==  TRUE)
          setTxtProgressBar(pb, (M*(nrep-1)+j-1)/(M*nrepeat) + (i-1)/length(test.keepX)/(M*nrepeat))
        
        # depending on whether it is a constraint and whether it is from tune or perf, keepX and keepX.constraint differ:
        # if it's from perf, then it's only either keepX or keepX.constraint
        # if it's from tune, then it's either keepX, or a combination of keepX.constraint and keepX
        # we know if it's perf+constraint or tune+constraint depending on the test.keepX that is either a vector or a list
        object.res = splsda(X.train, Y.train, ncomp = ncomp,
                            keepX = if(is.null(choice.keepX.constraint) & !is.list(test.keepX)){c(choice.keepX, test.keepX[i])}else if(!is.list(test.keepX)){test.keepX[i]} else {NULL} ,
                            keepX.constraint = if(is.null(choice.keepX.constraint)& !is.list(test.keepX)){NULL}else if(!is.list(test.keepX)){choice.keepX.constraint} else {c(choice.keepX.constraint, test.keepX)},
                            logratio = "none", near.zero.var = FALSE, mode = "regression", max.iter = max.iter)
        
        # added: record selected features
        if (any(class.object %in% c("splsda")) & length(test.keepX) ==  1) # only done if splsda and if only one test.keepX as not used if more so far
          # note: if plsda, 'features' includes everything: to optimise computational time, we don't evaluate for plsda object
          features.j = selectVar(object.res, comp = ncomp)$name
        
        test.predict.sw <- predict.spls(object.res, newdata = X.test, method = dist)
        prediction.comp.j[, , i] =  test.predict.sw$predict[, , ncomp]
        
        for(ijk in dist)
          class.comp.j[[ijk]][, i] =  test.predict.sw$class[[ijk]][, ncomp] #levels(Y)[test.predict.sw$class[[ijk]][, ncomp]]
      } # end i
      
      
      return(list(class.comp.j = class.comp.j, prediction.comp.j = prediction.comp.j, features = features.j, omit = omit))
      
    } # end fonction.j.folds
    
    
    
    if (!is.null(cl) == TRUE)
    {
      result = parLapply(cl, 1: M, fonction.j.folds)
    } else {
      result = lapply(1: M, fonction.j.folds)
      
    }
    
    # combine the results
    for(j in 1:M)
    {
      omit = result[[j]]$omit
      prediction.comp.j = result[[j]]$prediction.comp.j
      class.comp.j = result[[j]]$class.comp.j
      
      prediction.comp[[nrep]][omit, , ] = prediction.comp.j
      
      for(ijk in dist)
        class.comp[[ijk]][omit,nrep, ] = class.comp.j[[ijk]]
      
      if (any(class.object %in% c("splsda")) & length(test.keepX) ==  1) # only done if splsda and if only one test.keepX as not used if more so far
        features = c(features, result[[j]]$features)
      
    }
    
    if (progressBar ==  TRUE)
      setTxtProgressBar(pb, (M*nrep)/(M*nrepeat))
    
    if(auc)
    {
      data=list()
      for (i in 1:length(test.keepX))
      {
        data$outcome = Y
        data$data = prediction.comp[[nrep]][, , i]
        auc.all[[nrep]][, , i] = as.matrix(statauc(data))
      }
    }
    
    } #end nrep 1:nrepeat
  
  names(prediction.comp) = names (auc.all) = paste0("nrep.", 1:nrepeat)
  # class.comp[[ijk]] is a matrix containing all prediction for test.keepX, all nrepeat and all distance, at comp fixed
  
  # average auc over the nrepeat, for each test.keepX
  if(auc)
  {
    
    if(nlevels(Y)>2)
    {
      auc.mean.sd =  array(0, c(nlevels(Y),2, length(test.keepX)), dimnames = list(rownames(auc.all[[1]]), c("AUC.mean","AUC.sd"), names(test.keepX)))
    }else{
      auc.mean.sd =  array(0, c(1,2, length(test.keepX)), dimnames = list(rownames(auc.all[[1]]), c("AUC.mean","AUC.sd"), names(test.keepX)))
    }
    
    for(i in 1:length(test.keepX))
    {
      temp = NULL
      for(nrep in 1:nrepeat)
      {
        temp = cbind(temp, auc.all[[nrep]][, 1, i])
      }
      auc.mean.sd[, 1, i] = apply(temp,1,mean)
      auc.mean.sd[, 2, i] = apply(temp,1,sd)
    }
  } else {
    auc.mean.sd = auc.all = NULL
    
  }
  
  result = list()
  error.mean = error.sd = error.per.class.keepX.opt.comp = keepX.opt = test.keepX.out = mat.error.final = choice.keepX.out = list()
  
  if (any(measure == "overall"))
  {
    for(ijk in dist)
    {
      rownames(class.comp[[ijk]]) = rownames(X)
      colnames(class.comp[[ijk]]) = paste0("nrep.", 1:nrepeat)
      dimnames(class.comp[[ijk]])[[3]] = paste0("test.keepX.",names(test.keepX))
      
      #finding the best keepX depending on the error measure: overall or BER
      # classification error for each nrep and each test.keepX: summing over all samples
      error = apply(class.comp[[ijk]],c(3,2),function(x)
      {
        sum(as.character(Y) != x)
      })
      rownames(error) = names(test.keepX)
      colnames(error) = paste0("nrep.",1:nrepeat)
      
      # we want to average the error per keepX over nrepeat and choose the minimum error
      error.mean[[ijk]] = apply(error,1,mean)/length(Y)
      if (!nrepeat ==  1)
        error.sd[[ijk]] = apply(error,1,sd)/length(Y)
      
      mat.error.final[[ijk]] = error/length(Y)  # percentage of misclassification error for each test.keepX (rows) and each nrepeat (columns)
      
      keepX.opt[[ijk]] = which(error.mean[[ijk]] ==  min(error.mean[[ijk]]))[1] # chose the lowest keepX if several minimum
      
      # confusion matrix for keepX.opt
      error.per.class.keepX.opt.comp[[ijk]] = apply(class.comp[[ijk]][, , keepX.opt[[ijk]], drop = FALSE], 2, function(x)
      {
        conf = get.confusion_matrix(Y.learn = factor(Y), Y.test = factor(Y), pred = x)
        out = (apply(conf, 1, sum) - diag(conf)) / summary(Y)
      })
      
      rownames(error.per.class.keepX.opt.comp[[ijk]]) = levels(Y)
      colnames(error.per.class.keepX.opt.comp[[ijk]]) = paste0("nrep.", 1:nrepeat)
      
      
      test.keepX.out[[ijk]] = test.keepX[keepX.opt[[ijk]]]
      if(is.null(choice.keepX))
      {
        choice.keepX.out[[ijk]] = c(lapply(choice.keepX.constraint,length), test.keepX.out)
      }else{
        choice.keepX.out[[ijk]] = c(choice.keepX, test.keepX.out)
      }
      result$"overall"$error.rate.mean = error.mean
      if (!nrepeat ==  1)
        result$"overall"$error.rate.sd = error.sd
      
      result$"overall"$confusion = error.per.class.keepX.opt.comp
      result$"overall"$mat.error.rate = mat.error.final
      result$"overall"$keepX.opt = test.keepX.out
    }
  }
  
  if (any(measure ==  "BER"))
  {
    for(ijk in dist)
    {
      rownames(class.comp[[ijk]]) = rownames(X)
      colnames(class.comp[[ijk]]) = paste0("nrep.", 1:nrepeat)
      dimnames(class.comp[[ijk]])[[3]] = paste0("test.keepX.",names(test.keepX))
      
      error = apply(class.comp[[ijk]],c(3,2),function(x)
      {
        conf = get.confusion_matrix(Y.learn = factor(Y),Y.test = factor(Y),pred = x)
        get.BER(conf)
      })
      rownames(error) = names(test.keepX)
      colnames(error) = paste0("nrep.",1:nrepeat)
      
      # average BER over the nrepeat
      error.mean[[ijk]] = apply(error,1,mean)
      if (!nrepeat ==  1)
        error.sd[[ijk]] = apply(error,1,sd)
      
      mat.error.final[[ijk]] = error  # BER for each test.keepX (rows) and each nrepeat (columns)
      
      keepX.opt[[ijk]] = which(error.mean[[ijk]] ==  min(error.mean[[ijk]]))[1]
      
      # confusion matrix for keepX.opt
      error.per.class.keepX.opt.comp[[ijk]] = apply(class.comp[[ijk]][, , keepX.opt[[ijk]], drop = FALSE], 2, function(x)
      {
        conf = get.confusion_matrix(Y.learn = factor(Y), Y.test = factor(Y), pred = x)
        out = (apply(conf, 1, sum) - diag(conf)) / summary(Y)
      })
      
      rownames(error.per.class.keepX.opt.comp[[ijk]]) = levels(Y)
      colnames(error.per.class.keepX.opt.comp[[ijk]]) = paste0("nrep.", 1:nrepeat)
      
      test.keepX.out[[ijk]] = test.keepX[keepX.opt[[ijk]]]
      if(is.null(choice.keepX))
      {
        choice.keepX.out[[ijk]] = c(lapply(choice.keepX.constraint,length), test.keepX.out)
      }else{
        choice.keepX.out[[ijk]] = c(choice.keepX, test.keepX.out)
      }
      result$"BER"$error.rate.mean = error.mean
      if (!nrepeat ==  1)
        result$"BER"$error.rate.sd = error.sd
      
      result$"BER"$confusion = error.per.class.keepX.opt.comp
      result$"BER"$mat.error.rate = mat.error.final
      result$"BER"$keepX.opt = test.keepX.out
      
    }
    
    
  }
  
  
  result$prediction.comp = prediction.comp
  result$auc = auc.mean.sd
  result$auc.all = auc.all
  result$class.comp = class.comp
  result$features$stable = sort(table(as.factor(features))/M/nrepeat, decreasing = TRUE)
  return(result)
}

#'Perform Sparse Generalized Canonical Correlation (sgccak)
#'@description Runs sgccak() modified from RGCCA
#'@param A Data
#'@param design Set design
#'@param study Default set to NULL
#'@param keepA.constraint Default set to NULL
#'@param keepA Default set to NULL
#'@param scheme Scheme, default set to "horst"
#'@param init Init mode, default set to "svd"
#'@param max.iter Max number of iterations, numeric, default set to 100
#'@param tol Tolerance, numeric, default set to 1e-06
#'@param verbose Default set to TRUE
#'@param bias Default set to FALSE
#'@param penalty Default set to NULL
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

sparse.mint.block_iteration = function(A, design, study = NULL, keepA.constraint = NULL, keepA = NULL,
                                      scheme = "horst", init = "svd", max.iter = 100, tol = 1e-06, verbose = TRUE, bias = FALSE,
                                      penalty=NULL){
  
  # keepA.constraint is a list of positions in A of the variables to keep on the component
  # keepA is a vector of numbers
  # study is a vector
  # == == == == no check needed as this function is only used in internal_mint.block, in which the checks are conducted
  
  ### Start: Initialization parameters
  J = length(A)
  J2 = J-1
  pjs = sapply(A, NCOL)
  AVE_X = rep(0, J)
  if (!is.null(penalty))
    penalty = penalty * sqrt(pjs)
  
  
  iter = 1
  converg = crit = numeric()
  variates.A = Z = matrix(0, NROW(A[[1]]), J)
  misdata = any(sapply(A, function(x){any(is.na(x))})) # Detection of missing data
  
  if (misdata)
    is.na.A = lapply(A, is.na)
  
  g = function(x) switch(scheme, horst = x, factorial = x^2, centroid = abs(x))
  
  
  # study split
  A_split = lapply(A, study_split, study)
  if (misdata)
    is.na.A_split = lapply(is.na.A, study_split, study)
  
  nlevels_study = nlevels(study)
  ### End: Initialization parameters
  
  ### Start: Initialisation "loadings.A" vector
  if (init == "svd")
  {
    ### Start: Change initialization of loadings.A
    if (misdata)
    {
      M = lapply(c(1:(J-1)), function(x){crossprod(replace(A[[x]], is.na(A[[x]]), 0), replace(A[[J]], is.na(A[[J]]), 0))})
    } else {
      M = lapply(c(1:(J-1)), function(x){crossprod(A[[x]], A[[J]])})
    }
    
    svd.M = lapply(M, function(x){svd(x, nu = 1, nv = 1)})
    loadings.A = lapply(c(1:(J-1)), function(x){svd.M[[x]]$u})
    loadings.A[[J]] = svd.M[[1]]$v
  } else if (init=="svd.single") {
    alpha =  lapply(1 : J, function(y){initsvd(lapply(y, function(x) {replace(A[[x]], is.na(A[[x]]), 0)})[[1]])})
    
    loadings.A = list()
    for (j in 1:J)
    {
      if (nrow(A[[j]]) >= ncol(A[[j]]))
      {
        loadings.A[[j]] = alpha[[j]]
      } else {
        K = as.matrix(A[[j]]) %*% as.matrix(t(A[[j]]))
        N = ifelse(bias, nrow(A[[j]]), nrow(A[[j]]) - 1)
        alpha[[j]] = drop(1/sqrt(t(alpha[[j]]) %*% K %*% alpha[[j]])) * alpha[[j]]
        loadings.A[[j]] = t(A[[j]]) %*% alpha[[j]]
      }
    }
    
    ### End: Change initialization of a
  } else {
    stop("init should be either 'svd' or 'svd.single'.")
  }
  ### End: Initialisation "a" vector
  variates.partial.A.comp = NULL
  loadings.partial.A.comp = list()
  for (q in 1:J)
  {
    if(misdata)
    {
      #variates.A[, q] =  apply(A[[q]], 1, miscrossprod, loadings.A[[q]])
      A.temp = replace(A[[q]], is.na.A[[q]], 0) # replace NA in A[[q]] by 0
      variates.A.temp = A.temp %*% loadings.A[[q]]
      temp = drop(loadings.A[[q]]) %o% rep(1, nrow(A[[q]]))
      temp[(t(is.na.A[[q]]))] = 0
      loadings.A.norm = crossprod(temp)
      variates.A[, q] = variates.A.temp / diag(loadings.A.norm)
      # we can have 0/0, so we put 0
      a = is.na(variates.A[, q])
      if (any(a))
        variates.A[a, q] = 0
    }else{
      variates.A[, q] = A[[q]]%*%loadings.A[[q]]
    }
    loadings.A[[q]] = l2.norm(as.vector(loadings.A[[q]]))
    loadings.partial.A.comp[[q]] = list()
  }
  loadings.A_old = loadings.A
  
  ### Start Algorithm 1 Sparse generalized canonical analysis (See Variable selection for generalized canonical correlation analysis (Tenenhaus))
  repeat {
    #variates.Aold can be used for the convergence of the algorithm, not used at the moment, so please disregard variates.Aold
    variates.Aold = variates.A
    for (q in 1:J)
    {
      ### Start : !!! Impact of the diag of the design matrix !!! ###
      if (scheme == "horst")
        CbyCovq = design[q, ]
      
      if (scheme == "factorial")
        CbyCovq = design[q, ] * cov2(variates.A, variates.A[, q], bias = bias)
      
      if (scheme == "centroid")
        CbyCovq = design[q, ] * sign(cov2(variates.A, variates.A[, q], bias = bias))
      ### End : !!! Impact of the diag of the design matrix !!! ###
      
      ### Step A start: Compute the inner components
      Z[, q] = rowSums(mapply("*", CbyCovq, as.data.frame(variates.A)))
      Z_split = study_split(Z[,q,drop=FALSE],study)  # split Z by the study factor
      ### Step A end: Compute the inner components
      
      
      ### Step B start: Computer the outer weight ###
      # possibility of removing NA (replacing by 0) and use crossprod, further development
      temp=0
      for (m in 1:nlevels_study)
      {
        if(misdata)
        {
          loadings.partial.A.comp[[q]][[m]] = apply(t(A_split[[q]][[m]]), 1, miscrossprod, Z_split[[m]])
        }else{
          loadings.partial.A.comp[[q]][[m]] = t(A_split[[q]][[m]])%*%Z_split[[m]]
        }
        temp=temp+loadings.partial.A.comp[[q]][[m]]
      }
      loadings.A[[q]] = temp
      
      
      # sparse using keepA / penalty
      if (!is.null(penalty))
      {
        loadings.A[[q]] = sparsity(loadings.A[[q]], keepA = NULL,keepA.constraint = NULL, penalty = penalty[q])
      }else{
        loadings.A[[q]] = sparsity(loadings.A[[q]], keepA[[q]], keepA.constraint[[q]], penalty = NULL)
      }
      
      loadings.A[[q]]=l2.norm(as.vector(loadings.A[[q]]))
      
      ### Step B end: Computer the outer weight ###
      if(misdata)
      {
        #variates.A[, q] =  apply(A[[q]], 1, miscrossprod, loadings.A[[q]])
        A.temp = replace(A[[q]], is.na.A[[q]], 0) # replace NA in A[[q]] by 0
        variates.A.temp = A.temp %*% loadings.A[[q]]
        temp = drop(loadings.A[[q]]) %o% rep(1, nrow(A[[q]]))
        temp[(t(is.na.A[[q]]))] = 0
        loadings.A.norm = crossprod(temp)
        variates.A[, q] = variates.A.temp / diag(loadings.A.norm)
        # we can have 0/0, so we put 0
        a = is.na(variates.A[, q])
        if (any(a))
          variates.A[a, q] = 0
        
      }else{
        variates.A[, q] =  A[[q]]%*%loadings.A[[q]]
      }
      
    }
    
    crit[iter] = sum(design * g(cov2(variates.A, bias = bias)))
    
    if (iter > max.iter)
      warning("The SGCCA algorithm did not converge", call. = FALSE)#cat("The SGCCA algorithm did not converge after", max.iter ,"iterations."))
    
    ### Start: Match algorithm with mixOmics algo (stopping point)
    ### if ((converg[iter] < tol & sum(stationnary_point) == J) | iter > max.iter)
    if (max(sapply(1:J, function(x){crossprod(loadings.A[[x]] - loadings.A_old[[x]])})) < tol | iter > max.iter)
      break
    ### End: Match algorithm with mixOmics algo (stopping point)
    
    loadings.A_old = loadings.A
    iter = iter + 1
  }
  ### End Algorithm 1 (See Variable selection for generalized canonical correlation analysis (Tenenhaus))
  
  
  #calculation variates.partial.A.comp
  variates.partial.A.comp = lapply(1 : J, function(x){
    lapply(1 : nlevels_study, function(y){
      if(misdata)
      {
        #apply(A_split[[x]][[y]], 1, miscrossprod, loadings.A[[x]])
        A.temp = replace(A_split[[x]][[y]], is.na.A_split[[x]][[y]], 0) # replace NA in A_split[[x]][[y]] by 0
        variates.part.A.temp = A.temp %*% loadings.A[[x]]
        temp = drop(loadings.A[[x]]) %o% rep(1, nrow(A_split[[x]][[y]]))
        temp[(t(is.na.A[[x]][[y]]))] = 0
        loadings.A.norm = crossprod(temp)
        variates.part.A = variates.part.A.temp / diag(loadings.A.norm)
        # we can have 0/0, so we put 0
        a = is.na(variates.part.A)
        if (any(a))
          variates.part.A[a] = 0
        
        return(variates.part.A)
      }else{
        A_split[[x]][[y]] %*% loadings.A[[x]]
      }
    })
  })
  
  if (verbose)
    plot(crit, xlab = "iteration", ylab = "criteria")
  
  AVE_inner = sum(design * cor(variates.A)^2/2)/(sum(design)/2)
  
  result = list(variates.A = variates.A, loadings.A = loadings.A, crit = crit[which(crit != 0)],
                AVE_inner = AVE_inner, loadings.partial.A.comp = loadings.partial.A.comp, variates.partial.A.comp = variates.partial.A.comp, iter = iter)
  return(result)
}

mean_centering_per_study=function(data, study, scale, bias=FALSE)
{
  
  M = length(levels(study))   # number of groups
  # split the data
  data.list.study = study_split(data, study)
  
  # center and scale data per group, and concatene the data
  res = lapply(data.list.study, scale.function, scale = scale, bias = bias)
  concat.data = do.call("rbind", lapply(res,function(x){x[[1]]}))
  meanX = lapply(res, function(x){x[[2]]})
  sqrt.sdX = lapply(res, function(x){x[[3]]})
  rownames.study = lapply(res, function(x){rownames(x[[1]])})
  
  #rename rows and cols of concatenated centered (and/or scaled) data
  colnames(concat.data) = colnames(data)
  
  #sort the samples as in the original X
  indice.match = match(rownames(data),rownames(concat.data))
  concat.data = concat.data[indice.match, ,drop=FALSE]
  
  if (M > 1)
  {
    for (m in 1:M)
    {
      attr(concat.data,paste0("means:", levels(study)[m])) = meanX[[m]]
      if(scale)
      {
        attr(concat.data,paste0("sigma:", levels(study)[m])) = sqrt.sdX[[m]]
      } else {
        attr(concat.data,paste0("sigma:", levels(study)[m])) = NULL
      }
    }
  } else {
    attr(concat.data,"scaled:center") = meanX[[1]]
    if (scale)
    {
      attr(concat.data,"scaled:scale") = sqrt.sdX[[1]]
    } else {
      attr(concat.data,"scaled:scale") = NULL
    }
  }
  
  return(list(concat.data=concat.data, rownames.study=rownames.study))
}

study_split = function(data, study)
{
  data = as.matrix(data)
  M = length(levels(study))
  P = ncol(data)
  
  #---------------------- split data
  data.list.study = split(data,study)
  if (!is.null(rownames(data)))
    study.name = split(rownames(data),study)
  
  for(m in 1:M)
  {
    data.list.study[[m]] = matrix(data.list.study[[m]], ncol=P)
    
    if (!is.null(colnames(data)))
      colnames(data.list.study[[m]]) = colnames(data)
    
    if (!is.null(rownames(data)))
      rownames(data.list.study[[m]]) = study.name[[m]]
  }
  result = data.list.study
  return(invisible(result))
}

scale.function=function(temp, scale = TRUE, bias = FALSE)
{
  meanX = colMeans(temp, na.rm = TRUE)
  data.list.study.scale_i = t(t(temp) - meanX)
  if (scale)
  {
    if (bias)
    {
      sqrt.sdX = sqrt(colSums(data.list.study.scale_i^2, na.rm = TRUE) / (nrow(temp)))
    } else {
      sqrt.sdX = sqrt(colSums(data.list.study.scale_i^2, na.rm = TRUE) / (nrow(temp) - 1))
    }
    data.list.study.scale_i = t(t(data.list.study.scale_i) / sqrt.sdX)
  } else {
    sqrt.sdX = NULL
  }
  
  is.na.data = is.na(data.list.study.scale_i)
  #if (sum(is.na.data) > 0)
  #data.list.study.scale_i[is.na.data] = 0
  
  out = list(data_scale=data.list.study.scale_i, meanX=meanX, sqrt.sdX=sqrt.sdX)
  return(out)
}

l2.norm=function(x)
{
  if (!is.vector(x))
    stop("x has to be a vector")
  
  out = x / drop(sqrt(crossprod(x)))
}

miscrossprod = function (x, y) {
  d.p = sum(drop(x) * drop(y), na.rm = TRUE)
  #d.p = as.vector(d.p)/norm2(d.p)     ## change made
  return(d.p)
}

sparsity=function(loadings.A, keepA, keepA.constraint=NULL, penalty=NULL)
{
  
  if (!is.null(keepA.constraint))
  {
    loadings.A[-keepA.constraint] = 0
  } else if (!is.null(keepA)) {
    nx = length(loadings.A) - keepA
    loadings.A = soft_thresholding_L1(loadings.A, nx = nx)
  } else if (!is.null(penalty)) {
    loadings.A = soft.threshold(loadings.A, penalty)
  }
  
  return(loadings.A)
}

soft_thresholding_L1 = function(x,nx)
{
  #selection on a (loadings.X). modified on 19/02/15 to make sure that a!=0
  if (nx!=0)
  {
    absa = abs(x)
    if (any(rank(absa, ties.method = "max") <= nx))
    {
      x = ifelse(rank(absa, ties.method = "max") <= nx, 0,
                 sign(x) * (absa - max(absa[rank(absa, ties.method = "max") <= nx])))
    }
  }
  
  x
}

cov2 = function (x, y = NULL, bias = TRUE) {
  n = NROW(x)
  if (is.null(y)) {
    x = as.matrix(x)
    if (bias) {
      C = ((n - 1)/n) * cov(x, use = "pairwise.complete.obs")
    } else {
      C = cov(x, use = "pairwise.complete.obs")
    }
  } else {
    if (bias) {
      C = ((n - 1)/n) * cov(x, y, use = "pairwise.complete.obs")
    } else {
      C = cov(x, y, use = "pairwise.complete.obs")
    }
  }
  return(C)
}

defl.select = function(yy, rr, nncomp, nn, nbloc, indY = NULL, mode = "canonical", aa = NULL) { ### Start: Add new parameter for estimation classic mode
  resdefl = NULL
  pdefl = NULL
  for (q in 1 : nbloc) {
    ### Start: insertion of new deflations (See La regression PLS Theorie et pratique (page 139))
    if ( nn <= nncomp[q] ) {
      if ((mode == "canonical") || (q != indY)) { #deflation of each block independently from the others, except indY
        defltmp = deflation(rr[[q]], yy[ , q])
        resdefl[[q]] = defltmp$R
        pdefl[[q]]   = defltmp$p
      } else if (mode == "classic") {
        resdefl[[q]] = Reduce("+", lapply(c(1:nbloc)[-q], function(x) {rr[[q]] - yy[ ,x]%*%t(aa[[q]])}))/(nbloc-1)
        pdefl[[q]]   =  rep(0,NCOL(rr[[q]]))
      } else if (mode == "invariant") { #no deflation
        resdefl[[q]] = rr[[q]]
        pdefl[[q]]   =  rep(0,NCOL(rr[[q]]))
      } else if (mode == "regression") {
        resdefl[[q]] = Reduce("+", lapply(c(1:nbloc)[-q], function(x) {deflation(rr[[q]],yy[, x])$R}))/(nbloc-1)
        pdefl[[q]]   =  rep(0,NCOL(rr[[q]]))
      }
      ### End: insertion of new deflations (See La regression PLS Theorie et pratique (page 139))
    } else {
      resdefl[[q]] = rr[[q]]
      pdefl[[q]]   =  rep(0,NCOL(rr[[q]]))
    }
  }
  return(list(resdefl=resdefl,pdefl=pdefl))
}

deflation = function(X, y){
  # Computation of the residual matrix R
  # Computation of the vector p.
  is.na.tX = is.na(t(X))
  if (any(is.na.tX))
  {
    #p = apply(t(X),1,miscrossprod,y)/as.vector(crossprod(y))
    
    #variates.A[, q] =  apply(A[[q]], 1, miscrossprod, loadings.A[[q]])
    A.temp = replace(t(X), is.na.tX, 0) # replace NA in A[[q]] by 0
    variates.A.temp = A.temp %*% y
    temp = drop(y) %o% rep(1, nrow(A.temp))
    temp[(t(is.na.tX))] = 0
    loadings.A.norm = crossprod(temp)
    p = variates.A.temp / diag(loadings.A.norm)
    # we can have 0/0, so we put 0
    a = is.na(p)
    if (any(a))
      p[a] = 0
    
  } else {
    p = t(X)%*%y/as.vector(crossprod(y))
  }
  
  R = X - y%*%t(p)
  return(list(p=p,R=R))
}

explained_variance = function(data, variates, ncomp)
{
  #check input data
  check = Check.entry.single(data, ncomp)
  data = check$X
  ncomp = check$ncomp
  
  isna = is.na(data)
  if (sum(isna > 0))
  {
    warning("NA values put to zero, results will differ from PCA methods used with NIPALS")
    data[isna] = 0
  }
  nor2x = sum((data)^2) # total variance in the data
  exp.varX = NULL
  for (h in 1:ncomp)
  {
    temp = variates[, h] / drop(t(variates[, h]) %*% (variates[, h]))
    exp_var_new = as.numeric(t(variates[, h]) %*% data %*% t(data) %*% temp )/nor2x
    exp.varX = append(exp.varX, exp_var_new)
    
  }
  names(exp.varX) = paste("comp", 1:ncomp)
  
  # result: vector of length ncomp with the explained variance per component
  exp.varX
}

Check.entry.single = function(X,  ncomp, q){
  
  #-- validation des arguments --#
  if (length(dim(X)) != 2)
    stop(paste0("'X[[", q, "]]' must be a numeric matrix."))
  
  X = as.matrix(X)
  
  if (!is.numeric(X))
    stop(paste0("'X[[", q, "]]'  must be a numeric matrix."))
  
  N = nrow(X)
  P = ncol(X)
  
  if (is.null(ncomp) || !is.numeric(ncomp) || ncomp <= 0)
    stop(paste0("invalid number of variates 'ncomp' for matrix 'X[[", q, "]]'."))
  
  ncomp = round(ncomp)
  
  # add colnames and rownames if missing
  X.names = dimnames(X)[[2]]
  if (is.null(X.names))
  {
    X.names = paste("X", 1:P, sep = "")
    dimnames(X)[[2]] = X.names
  }
  
  ind.names = dimnames(X)[[1]]
  if (is.null(ind.names))
  {
    ind.names = 1:N
    rownames(X)  = ind.names
  }
  
  if (length(unique(rownames(X))) != nrow(X))
    stop("samples should have a unique identifier/rowname")
  if (length(unique(X.names)) != P)
    stop("Unique indentifier is needed for the columns of X")
  
  return(list(X=X, ncomp=ncomp, X.names=X.names, ind.names=ind.names))
}








stratified.subsampling = function(Y, folds = 10)
{
  stop = 0
  for(i in 1:nlevels(Y))
  {
    ai=sample(which(Y==levels(Y)[i]),replace=FALSE) # random sampling of the samples from level i
    aai=suppressWarnings(split(ai,factor(1:min(folds,length(ai)))))                       # split of the samples in k-folds
    if(length(ai)<folds)                                                # if one level doesn't have at least k samples, the list is completed with "integer(0)"
    {
      for(j in (length(ai)+1):folds)
        aai[[j]]=integer(0)
      stop = stop +1
    }
    assign(paste("aa",i,sep="_"),sample(aai,replace=FALSE))         # the `sample(aai)' is to avoid the first group to have a lot more data than the rest
  }
  
  # combination of the different split aa_i into SAMPLE
  SAMPLE=list()
  for(j in 1:folds)
  {
    SAMPLE[[j]]=integer(0)
    for(i in 1:nlevels(Y))
    {
      SAMPLE[[j]]=c(SAMPLE[[j]],get(paste("aa",i,sep="_"))[[j]])
    }
  }# SAMPLE is a list of k splits
  
  ind0 = sapply(SAMPLE, length)
  if(any(ind0 == 0))
  {
    SAMPLE = SAMPLE [-which(ind0 == 0)]
    message("Because of a too high number of 'folds' required, ",length(which(ind0 == 0))," folds were randomly assigned no data: the number of 'folds' is reduced to ", length(SAMPLE))
  }
  
  return(list(SAMPLE = SAMPLE, stop = stop))
}

selectVar <- function(object, comp =1, block=NULL, ...){
  
  # check arguments
  # -----------------
  if (length(comp) > 1)
    stop("Expecting one single value for 'comp'")
  
  if (is.null(block))
  {
    if (any(comp > object$ncomp))
      stop("'comp' is greater than the number of components in the fitted model")
    null.block=TRUE
    block=1:length(object$loadings)
    
  }else{
    if (any(class(object)%in%c("pca")))
      object$names$blocks="X"
    
    if (is.numeric(block))
    {
      if (any(block>length(object$names$blocks)))
        stop("'block' needs to be lower than the number of blocks in the fitted model, which is length(object$names$blocks)")
      
    }else if (is.character(block) & sum(!is.na(match(block,object$names$blocks)))==0) {
      stop("No entry of 'block'  match object$names$blocks")
      
    }else if (is.character(block) & sum(is.na(match(block,object$names$blocks)))>0) {
      warning("At least one entry of 'block' does not match object$names$blocks")
    }
    
    if (length(object$ncomp)>1)
    {
      if (any(comp > object$ncomp[block]))
        stop("'comp' is greater than the number of components in the fitted model for the block you specified. See object$ncomp")
      
    }else{
      if (any(comp > object$ncomp))
        stop("'comp' is greater than the number of components in the fitted model")
    }
    
    null.block=FALSE
  }
  
  # main function: get the names and values of the non zero loadings
  # -----------------
  out = lapply(object$loadings[block],get.name.and.value,comp=comp)
  
  
  # outputs
  # ----------
  #if all blocks are considered by default (null.block=TRUE) and it's a DA analysis, then we don't show Y
  if (null.block)
  {
    if (any(class(object)%in%c("block.plsda","block.splsda")))# the position of Y is in indY
    {
      out=out[-object$indY] #remove Y
    }else if (any(class(object)%in%c("mint.plsda","mint.splsda","plsda","splsda"))) {
      # Y is always in second position
      out=out[[1]]
    }else if (any(class(object)%in%c("pca"))) { #keep the result as a list
      out=out[[1]]
    }
    
  } else {
    if (length(grep("pca",class(object)))>0)
      out=out
  }
  
  #we add comp as an output
  out$comp=comp
  
  return(out)
}

get.name.and.value=function(x,comp)
{
  if(length(x[,comp,drop=FALSE]) > 1)
  {
    name.var = names(sort(abs(x[,comp]), decreasing = T)[1:sum(x[,comp]!=0)])
  } else {
    name.var = rownames(x) # when only one number, sort loses the name of the variable
  }
  value.var=x[name.var,comp]
  return(list(name = name.var, value = data.frame(value.var)))
}

get.confusion_matrix = function(Y.learn,Y.test,pred)
{
  ClassifResult = array(0,c(nlevels(factor(Y.learn)),nlevels(factor(Y.learn))))
  rownames(ClassifResult) = levels(factor(Y.learn))
  colnames(ClassifResult) = paste("predicted.as.",levels(factor(Y.learn)),sep = "")
  #--------record of the classification accuracy for each level of Y
  for(i in 1:nlevels(factor(Y.learn)))
  {
    ind.i = which(Y.test == levels(factor(Y.learn))[i])
    for(ij in 1:nlevels(factor(Y.learn)))
    {
      ClassifResult[i,ij] = sum(pred[ind.i] == levels(Y.learn)[ij])
      
    }
  }
  ClassifResult
}

get.BER = function(X)
{
  if(!is.numeric(X)| !is.matrix(X) | length(dim(X)) != 2 | nrow(X)!=ncol(X))
    stop("'X' must be a square numeric matrix")
  
  nlev = nrow(X)
  #calculation of the BER
  ClassifResult.temp = X
  diag(ClassifResult.temp) = 0
  BER = sum(apply(ClassifResult.temp,1,sum,na.rm = TRUE)/apply(X,1,sum,na.rm = TRUE),na.rm = TRUE)/nlev
  return(BER)
}

#predict.block.pls <-predict.block.spls <- predict.mint.splsda <- predict.pls <- 
predict.spls <- function(object, newdata, study.test, dist = c("all", "max.dist", "centroids.dist", "mahalanobis.dist"), multilevel = NULL, ...)
{
  
  ncomp = object$ncomp
  newdata.input=newdata
  
  if(length(grep("plsda", class(object)))>0) # a DA analysis (mint).(block).(s)plsda
  {
    #if DA analysis, the unmap Y is in ind.mat
    Y.factor=object$Y
    Y=object$ind.mat
  }else{
    #if not DA, Y is in object$Y
    Y=object$Y
    if(is.null(Y)) # block analysis
    {
      Y=object$X[[object$indY]]
    }
  }
  q=ncol(Y)
  
  
  p=ncol(object$X)
  if(is.list(object$X))
    stop("Something is wrong, object$X should be a matrix and it appears to be a list") #this should never happen/intern check
  
  if(is.list(newdata) & !is.data.frame(newdata))
    stop("'newdata' must be a numeric matrix")
  
  # deal with near.zero.var in object, to remove the same variable in newdata as in object$X (already removed in object$X)
  if(length(object$nzv$Position) > 0)
    newdata = newdata[, -object$nzv$Position,drop=FALSE]
  
  if(all.equal(colnames(newdata),colnames(object$X))!=TRUE)
    stop("'newdata' must include all the variables of 'object$X'")
  
  #not a block, the input newdata should be a matrix
  if (length(dim(newdata)) == 2) {
    if (ncol(newdata) != p)
      stop("'newdata' must be a numeric matrix with ncol = ", p,
           " or a vector of length = ", p, ".")
  }
  
  if (length(dim(newdata)) == 0) {
    if (length(newdata) != p)
      stop("'newdata' must be a numeric matrix with ncol = ", p,
           " or a vector of length = ", p, ".")
    dim(newdata) = c(1, p)
  }
  
  #check col/rownames of newdata
  check=Check.entry.single(newdata, ncomp,q=1)
  newdata=check$X
  
  if(length(rownames(newdata))==0) rownames(newdata)=1:nrow(newdata)
  if(max(table(rownames(newdata)))>1) stop('samples should have a unique identifier/rowname')
  
  # we transform everything in lists
  X=list(X=object$X)
  object$X=X
  newdata=list(newdata=newdata)
  
  object$indY=2
  ind.match = 1
  
  
  # logratio and multilevel transform if necessary
  if (!is.null(object$logratio))
    newdata = lapply(newdata, logratio.transfo, logratio = object$logratio)
  
  if(!is.null(multilevel))
    newdata = lapply(newdata, withinVariation, design = data.frame(multilevel))
  
  p = lapply(X, ncol)
  q = ncol(Y)
  J = length(X) #at this stage we have a list of blocks
  variatesX = object$variates[-(J + 1)];
  loadingsX = object$loadings[-(J + 1)]
  
  scale = object$scale # X and Y are both mean centered by groups and if scale=TRUE they are scaled by groups
  
  # scale newdata if just one study
  if (!is.null(attr(X[[1]], "scaled:center")))
    newdata[which(!is.na(ind.match))] = lapply(which(!is.na(ind.match)), function(x){sweep(newdata[[x]], 2, STATS = attr(X[[x]], "scaled:center"))})
  if (scale)
    newdata[which(!is.na(ind.match))] = lapply(which(!is.na(ind.match)), function(x){sweep(newdata[[x]], 2, FUN = "/", STATS = attr(X[[x]], "scaled:scale"))})
  
  means.Y = matrix(attr(Y, "scaled:center"),nrow=nrow(newdata[[1]]),ncol=q,byrow=TRUE);
  if (scale)
  {sigma.Y = matrix(attr(Y, "scaled:scale"),nrow=nrow(newdata[[1]]),ncol=q,byrow=TRUE)}else{sigma.Y=matrix(1,nrow=nrow(newdata[[1]]),ncol=q)}
  concat.newdata=newdata
  names(concat.newdata)=names(X)
  
  
  ### at this stage we have
  # X         # list of blocks
  # Y         # observation
  # newdata   #list of blocks for the prediction, same length as A, scaled
  
  # replace missing data by 0
  concat.newdata = lapply(concat.newdata,function(x)
  {
    ind = which(is.na(x))
    if (length(ind) > 0)
      x[ind] = 0
    x
  })
  
  # replace missing data by 0
  X = lapply(X,function(x)
  {
    ind = which(is.na(x))
    if (length(ind) > 0)
      x[ind] = 0
    x
  })
  
  # -----------------------
  #       prediction
  # -----------------------
  
  B.hat = t.pred = Y.hat = list() #= betay
  for (i in 1 : J)
  {
    Pmat = Cmat = Wmat = NULL
    
    ### Start estimation using formula Y = XW(P'W)C (+ Yr, residuals on Y) See page 136 La regression PLS Theorie et pratique Tenenhaus
    # Estimation matrix W, P and C
    Pmat = crossprod(X[[i]], variatesX[[i]])
    Cmat = crossprod(Y, variatesX[[i]])
    Wmat = loadingsX[[i]]
    
    # Prediction Y.hat, B.hat and t.pred
    Ypred = lapply(1 : ncomp[i], function(x){concat.newdata[[i]] %*% Wmat[, 1:x] %*% solve(t(Pmat[, 1:x]) %*% Wmat[, 1:x]) %*% t(Cmat)[1:x, ]})
    Ypred = sapply(Ypred, function(x){x*sigma.Y + means.Y}, simplify = "array")
    
    Y.hat[[i]] = Ypred
    
    t.pred[[i]] = concat.newdata[[i]] %*% Wmat %*% solve(t(Pmat) %*% Wmat)
    t.pred[[i]] = matrix(data = sapply(1:ncol(t.pred[[i]]),
                                       function(x) {t.pred[[i]][, x] * apply(variatesX[[i]], 2,
                                                                             function(y){(norm(y, type = "2"))^2})[x]}), nrow = nrow(concat.newdata[[i]]), ncol = ncol(t.pred[[i]]))
    
    B.hat[[i]] = sapply(1 : ncomp[i], function(x){Wmat[, 1:x] %*% solve(t(Pmat[, 1:x]) %*% Wmat[, 1:x]) %*% t(Cmat)[1:x, ]}, simplify = "array")
    ### End estimation using formula Y = XW(P'W)C (+ Yr, residuals on Y) See page 136 La regression PLS Theorie et pratique Tenenhaus
    
    rownames(t.pred[[i]]) = rownames(newdata[[i]])
    colnames(t.pred[[i]]) = paste("dim", c(1:ncomp[i]), sep = " ")
    rownames(Y.hat[[i]]) = rownames(newdata[[i]])
    colnames(Y.hat[[i]]) = colnames(Y)
    dimnames(Y.hat[[i]])[[3]]=paste("dim", c(1:ncomp[i]), sep = " ")
    rownames(B.hat[[i]]) = colnames(newdata[[i]])
    colnames(B.hat[[i]]) = colnames(Y)
    dimnames(B.hat[[i]])[[3]]=paste("dim", c(1:ncomp[i]), sep = " ")
    
  }
  
  
  #-- valeurs sortantes --#
  names(Y.hat)=names(t.pred)=names(B.hat)=names(object$X)
  
  
  
  # basic prediction results
  if(length(grep("block",class(object)))!=0 & length(object$X)>1 )
  {
    out=list(predict=Y.hat[which(!is.na(ind.match))],variates=t.pred[which(!is.na(ind.match))],B.hat=B.hat[which(!is.na(ind.match))])
    
    # average prediction over the blocks
    temp.all =list()
    for(comp in 1:min(ncomp[-object$indY])) #note: all ncomp are the same in v6 as the input parameter is a single value
    {
      temp = array(0, c(nrow(Y.hat[[1]]), ncol(Y.hat[[1]]), J), dimnames = list(rownames(newdata[[1]]), colnames(Y),names(object$X)))
      for(i in 1 : J)
        temp[, , i] = Y.hat[[i]][, , comp]
      
      temp.all[[comp]] = temp
    }
    names(temp.all) = paste("dim", c(1:min(ncomp[-object$indY])), sep = " ")
    
    out$AveragedPredict = array(unlist(lapply(temp.all, function(x){apply(x, c(1,2), mean)})), dim(Y.hat[[1]]), dimnames = list(rownames(newdata[[1]]), colnames(Y), paste("dim", c(1:min(ncomp[-object$indY])), sep = " ")))
    
    out$WeightedPredict = array(unlist(lapply(temp.all, function(x){apply(x, c(1,2), function(z){
      temp = aggregate(object$weights,list(z),sum)
      ind = which(temp[,2]== max (temp[,2]))# if two max, then NA
      if(length(ind) == 1)
      {
        res = temp[ind, 1]
      } else {
        res = NA
      }
      res
    })})), dim(Y.hat[[1]]), dimnames = list(rownames(newdata[[1]]), colnames(Y), paste("dim", c(1:min(ncomp[-object$indY])), sep = " ")))
    
    
    #out$newdata=concat.newdata
  }else if(length(grep("block",class(object)))!=0){ # a block but can have only one block (so e.g. a pls done with a block.pls)
    out=list(predict=Y.hat,variates=t.pred,B.hat=B.hat)
    
  } else {# not a block (pls/spls/plsda/splsda/mint...)
    out=list(predict=Y.hat[[1]],variates=t.pred[[1]],B.hat=B.hat[[1]])
  }
  
  # get the classification for each new sample if the object is a DA
  if(any(class(object)=="DA")) # a DA analysis (mint).(block).(s)plsda
  {
    
    if(length(grep("block",class(object)))!=0 & length(object$X)>1 )
    {
      
      # predict class of AveragePredict, only with max.dist
      out$AveragedPredict.class$max.dist = matrix(sapply(1:ncomp[1], ### List level
                                                         function(y){apply(out$AveragedPredict[, , y, drop = FALSE], 1,  ### component level
                                                                           function(z){
                                                                             paste(levels(Y.factor)[which(z == max(z))], collapse = "/")
                                                                           }) ### matrix level
                                                         }), nrow = nrow(newdata[[1]]), ncol = ncomp[1])
      
      
      # predict class of WeightedPredict, only with max.dist
      out$WeightedPredict.class$max.dist = matrix(sapply(1:ncomp[1], ### List level
                                                         function(y){apply(out$WeightedPredict[, , y, drop = FALSE], 1,  ### component level
                                                                           function(z){
                                                                             paste(levels(Y.factor)[which(z == max(z))], collapse = "/")
                                                                           }) ### matrix level
                                                         }), nrow = nrow(newdata[[1]]), ncol = ncomp[1])
      
      rownames(out$AveragedPredict.class$max.dist) = rownames(out$WeightedPredict.class$max.dist) = rownames(newdata[[1]])
      colnames(out$AveragedPredict.class$max.dist) = colnames(out$WeightedPredict.class$max.dist) = paste("dim", c(1:min(ncomp[-object$indY])), sep = " ")
    }
    
    
    
    # creating temporary 'blocks' outputs to pass into the internal_predict.DA function
    out.temp=list(predict=Y.hat[which(!is.na(ind.match))],variates=t.pred[which(!is.na(ind.match))],B.hat=B.hat[which(!is.na(ind.match))])
    out.temp$newdata=concat.newdata[which(!is.na(ind.match))]
    
    # getting classification for each new sample
    object.temp = object
    object.temp$X = object.temp$X[which(!is.na(ind.match))]
    object.temp$variates = object.temp$variates[c(which(!is.na(ind.match)),J+1)] #J+1 is Y
    classif.DA=internal_predict.DA(object=object.temp, q=q, out=out.temp, dist=dist, weights = object$weights[which(!is.na(ind.match))])
    out=c(out,classif.DA)
    
  }
  
  out$call = match.call()
  class(out) = paste("predict")
  
  out
  
}

logratio.transfo <- function (X, logratio = "none", offset = 0) 
{
  if (logratio == "ILR") {
    if (any(class(X) != "ilr")) {
      X = ilr.transfo(X, offset = offset)
    }
  }
  else if (logratio == "CLR") {
    X = clr.transfo(X, offset = offset)
  }
  return(X)
}

internal_predict.DA = function(object, out, q, dist, weights)
{
  
  if (length(grep("plsda",class(object)))==0) # a DA analysis (mint).(block).(s)plsda
    stop("'Object' is not from a Discriminant Analysis", call.=FALSE)
  
  out.DA = list()
  J = length(object$X) #at this stage we have a list of blocks
  p = lapply(object$X, ncol)
  t.pred = out$variates
  Y.hat = out$predict
  newdata = out$newdata #actually concat.newdata
  variatesX = object$variates[-(J + 1)];
  ncomp = object$ncomp
  
  Y = object$Y
  Y.prim = unmap(object$Y)
  G = cls = list()
  for (i in 1 : J)
  {
    G[[i]] = sapply(1:q, function(x) {apply(as.matrix(variatesX[[i]][Y.prim[, x] == 1,,drop=FALSE]), 2, mean)})
    if (ncomp[i] == 1)
      G[[i]] = t(t(G[[i]]))
    else
      G[[i]] = t(G[[i]])
    colnames(G[[i]]) = paste("dim", c(1:ncomp[i]), sep = " ")
    
  }
  names(G)=names(object$X)
  
  ### Start: Maximum distance
  if (any(dist == "all") || any(dist == "max.dist"))
  {
    cls$max.dist = lapply(1:J, function(x){matrix(sapply(1:ncomp[x], ### List level
                                                         function(y){apply(Y.hat[[x]][, , y, drop = FALSE], 1,  ### component level
                                                                           function(z){
                                                                             paste(levels(Y)[which(z == max(z))], collapse = "/")
                                                                           }) ### matrix level
                                                         }), nrow = nrow(newdata[[x]]), ncol = ncomp[x])
    })
    cls$max.dist = lapply(1:J, function(x){colnames(cls$max.dist[[x]]) = paste(rep("comp", ncomp[x]), 1 : ncomp[[x]], sep = " ");
    rownames(cls$max.dist[[x]]) = rownames(newdata[[x]]); return(cls$max.dist[[x]])})
    names(cls$max.dist)=names(object$X)
  }
  
  
  ### Start: Centroids distance
  if (any(dist == "all") || any(dist == "centroids.dist"))
  {
    cl = list()
    centroids.fun = function(x, G, h, i) {
      q = nrow(G[[i]])
      x = matrix(x, nrow = q, ncol = h, byrow = TRUE)
      
      if (h > 1) {
        d = apply((x - G[[i]][, 1:h])^2, 1, sum)
      }
      else {
        d = (x - G[[i]][, 1])^2
      }
      cl.id = paste(levels(Y)[which(d == min(d))], collapse = "/")
    }
    
    for (i in 1 : J)
    {
      cl[[i]] = matrix(nrow = nrow(newdata[[1]]), ncol = ncomp[i])
      
      for (h in 1 : ncomp[[i]])
      {
        cl.id = apply(matrix(t.pred[[i]][, 1:h], ncol = h), 1, function(x) {centroids.fun(x = x, G = G, h = h, i = i)})
        cl[[i]][, h] = cl.id
      }
    }
    
    cls$centroids.dist = lapply(1:J, function(x){colnames(cl[[x]]) = paste(rep("comp", ncomp[x]), 1 : ncomp[[x]], sep = " ");
    rownames(cl[[x]]) = rownames(newdata[[x]]); return(cl[[x]])})
    names(cls$centroids.dist)=names(object$X)
  }### End: Centroids distance
  
  
  ### Start: Mahalanobis distance
  if (any(dist == "all") || any(dist == "mahalanobis.dist"))
  {
    cl = list()
    Sr.fun = function(x, G, Yprim, h, i) {
      q = nrow(G[[i]])
      Xe = Yprim %*% G[[i]][, 1:h]
      #Xr = object$variates$X[, 1:h] - Xe
      Xr = variatesX[[i]][, 1:h] - Xe
      Sr = t(Xr) %*% Xr/nrow(Yprim)
      Sr.inv = solve(Sr)
      x = matrix(x, nrow = q, ncol = h, byrow = TRUE)
      if (h > 1) {
        mat = (x - G[[i]][, 1:h]) %*% Sr.inv %*% t(x - G[[i]][, 1:h])
        d = apply(mat^2, 1, sum)
      } else {
        d = drop(Sr.inv) * (x - G[[i]][, 1])^2
      }
      cl.id = paste(levels(Y)[which(d == min(d))], collapse = "/")
    }
    
    for (i in 1 : J){
      cl[[i]] = matrix(nrow = nrow(newdata[[1]]), ncol = ncomp[i])
      
      for (h in 1:ncomp[[i]]) {
        cl.id = apply(matrix(t.pred[[i]][, 1:h], ncol = h), 1, Sr.fun, G = G, Yprim = Y.prim, h = h, i = i)
        cl[[i]][, h] = cl.id
      }
    }
    
    cls$mahalanobis.dist = lapply(1:J, function(x){colnames(cl[[x]]) = paste(rep("comp", ncomp[x]), 1 : ncomp[[x]], sep = " ");
    rownames(cl[[x]]) = rownames(newdata[[x]]);return(cl[[x]])})
    names(cls$mahalanobis.dist)=names(object$X)
  } ### End: Mahalanobis distance
  
  out.DA$class = cls
  
  ### End if discriminant analysis is performed
  
  # at this stage, we have the classification of each sample for each dataset of object$X
  # now we need to combine the classification by vote (majority wins), only when more than one block, otherwise 'vote' is classic classification
  if (length(object$X)>1)
  {
    for (ijk in 1:length(out.DA$class))# loop on the dist
    {
      # create a temporary array to make computation on the lists easier
      temp=array(c(nrow(newdata[[1]]), min(ncomp), J))
      for(i in 1:J)
      {
        temp[, , i] = out.DA$class[[ijk]][[i]][, 1:min(ncomp)]
        
      }
      # look at the majority vote for all dataset of object$X (with table), if more than a unique max, we put NA
      table.temp = apply(temp,c(1,2), function(x){a=table(x); if (length(which(a==max(a)))==1) {b=names(which.max(a))}else{b=NA}; b})
      colnames(table.temp) = colnames(out.DA$class[[ijk]][[i]])[1:min(ncomp)]
      rownames(table.temp) = rownames(out.DA$class[[ijk]][[i]])
      out.DA$MajorityVote[[ijk]] = table.temp
    }
    names(out.DA$MajorityVote) = names(out.DA$class)
    
    # weighted vote for each distance, each comp
    if(!is.null(weights))
    {
      out.DA$WeightedVote = lapply(out.DA$class, function(x){ # x is a distance
        class.per.comp = lapply(1:min(ncomp), function(y) {matrix(sapply(x, function(z)  z[,y, drop = FALSE]),ncol=J)}) # combine the results per component
        names(class.per.comp) = paste0("comp",1:min(ncomp))
        class.per.comp = lapply(class.per.comp, function(y){rownames(y) = rownames(out.DA$vote[[1]]); y})
        class.weighted.per.comp = sapply(class.per.comp, function(y){ # for each component
          apply(y,1,function(z){  # we aggregate the results of each individuals using the 'weights'
            temp = aggregate(weights,list(z),sum)
            ind = which(temp[,2]== max (temp[,2]))# if two max, then NA
            if(length(ind) == 1)
            {
              res = temp[ind, 1]
            } else {
              res = NA
            }
            res
            
          })
        })
        
      })
      out.DA$weights = weights
      
    }
  }else{
    out.DA$MajorityVote = lapply(out.DA$class,function(x){x[[1]]})
  }
  
  if (length(grep("block",class(object)))!=0 & J>1) # a block
  {
    out.DA$centroids = G
  }else{ #not a block
    out.DA$centroids = G[[1]]
    out.DA$class = out.DA$MajorityVote
  }
  if (any(dist == "all"))
    dist = "all"
  
  out.DA$dist = dist
  
  out.DA
  
}

Check.entry.pls = function(X, Y, ncomp, keepX, keepY, keepX.constraint, keepY.constraint, mode, scale,
                           near.zero.var, max.iter, tol, logratio, DA, multilevel)
{
  
  if (missing(mode))
    mode = "regression"
  
  if (length(mode)>1)
    mode = mode[1]
  
  if (!(mode %in% c("canonical", "invariant", "classic", "regression")))
    stop("Choose one of the four following modes: canonical, invariant, classic or regression")
  
  
  #-- validation des arguments --#
  if (length(dim(X)) != 2)
    stop("'X' must be a numeric matrix.")
  
  X = as.matrix(X)
  
  if (!(logratio %in% c("none", "CLR")))
    stop("Choose one of the two following logratio transformation: none or CLR")
  
  if(!is.null(multilevel))
  {
    #multilevel analysis: withinVariation and then pls-like
    # if it's DA analysis, Y and 'multilevel' are combined
    if(DA)
    {
      Y = multilevel
    }else{
      if ((nrow(X) != nrow(multilevel)))
        stop("unequal number of rows in 'X' and 'multilevel'.")
      
      Y = as.matrix(Y)
      if (!is.numeric(X) || !is.numeric(Y))
        stop("'X' and/or 'Y' must be a numeric matrix.")
    }
  }else{
    Y = as.matrix(Y)
    if (!is.numeric(X) || !is.numeric(Y))
      stop("'X' and/or 'Y' must be a numeric matrix.")
  }
  N = nrow(X)
  Q = ncol(Y)
  P= ncol(X)
  
  if ((N != nrow(Y)))
    stop("Unequal number of rows in 'X' and 'Y'.")
  
  if (is.null(ncomp) || !is.numeric(ncomp) || ncomp <= 0 || length(ncomp)>1)
    stop("invalid number of variates, 'ncomp'.")
  
  ncomp = round(ncomp)
  if(ncomp > P)
  {
    warning("Reset maximum number of variates 'ncomp' to ncol(X) = ", P, ".")
    ncomp = P
  }
  
  if (!is.numeric(tol) | tol<=0)
    stop("tol must be non negative")
  
  if (!is.numeric(max.iter) | max.iter<=0)
    stop("max.iter must be non negative")
  
  
  # add colnames and rownames if missing
  X.names = dimnames(X)[[2]]
  if (is.null(X.names))
  {
    X.names = paste("X", 1:P, sep = "")
    dimnames(X)[[2]] = X.names
  }
  
  
  
  ind.names = dimnames(X)[[1]]
  if (is.null(ind.names))
  {
    ind.names = dimnames(Y)[[1]]
    rownames(X) = ind.names
  }
  
  if (is.null(ind.names))
  {
    ind.names = 1:N
    rownames(X) = rownames(Y) = ind.names
  }
  
  rownames(X) = rownames(Y) = ind.names
  
  
  #if (dim(Y)[2] == 1) Y.names = "Y"
  Y.names = dimnames(Y)[[2]]
  if (is.null(Y.names))
  {
    if (dim(Y)[2] == 1)
    {
      Y.names = "Y"
    } else {
      Y.names = paste("Y", 1:Q, sep = "")
    }
    
    dimnames(Y)[[2]]=Y.names
  }
  
  if (length(unique(X.names)) != P)
    stop("Unique indentifier is needed for the columns of X")
  
  if (length(unique(Y.names)) != Q)
    stop("Unique indentifier is needed for the columns of Y")
  
  
  # check on keepX and keepX.constraint
  if (missing(keepX.constraint))
  {
    if (missing(keepX))
    {
      keepX = rep(P, ncomp)
    } else {
      if (length(keepX)<ncomp)
        keepX = c(keepX, rep(P, ncomp - length(keepX))) #complete (with ncomp) the keepX already provided
    }
    keepX.constraint=list()
  } else {
    if (length(keepX.constraint)>ncomp)
      stop(paste0("You should have length(keepX.constraint) lower or equal to 'ncomp' = ", ncomp, "."))
    
    if (missing(keepX))
    {
      keepX = rep(P, ncomp - length(keepX.constraint))
    } else {
      
      if ((length(keepX.constraint) + length(keepX)) < ncomp)
        keepX = c(keepX, rep(P, ncomp - length(keepX) - length(keepX.constraint)))
      
      if ((length(keepX.constraint) + length(keepX)) > ncomp)
        stop(paste0("length (keepX.constraint) + length(keepX) should be lower than 'ncomp' = ", ncomp, "."))
      
    }
  }
  
  # check on keepY and keepY.constraint
  if (missing(keepY.constraint))
  {
    if (missing(keepY))
    {
      keepY = rep(Q, ncomp)
    } else {
      if (length(keepY) < ncomp)
        keepY = c(keepY, rep(Q, ncomp - length(keepY))) #complete the keepY already provided
    }
    keepY.constraint = list()
  } else {
    if (length(keepY.constraint)>ncomp)
      stop(paste0("you should have length(keepY.constraint) lower or equal to 'ncomp' = ", ncomp, "."))
    
    if (missing(keepY))
    {
      keepY = rep(Q, ncomp - length(keepY.constraint))
    } else {
      
      if ((length(keepY.constraint) + length(keepY)) < ncomp)
        keepY = c(keepY, rep(Q, ncomp - length(keepY) - length(keepY.constraint)))
      
      if ((length(keepY.constraint) + length(keepY)) > ncomp)
        stop(paste0("length (keepY.constraint) + length(keepY) should be lower than 'ncomp' = ", ncomp,"."))
    }
  }
  
  
  
  if (any(keepX<0))
    stop("each component of 'keepX' must be non negative ")
  if (any(keepY<0))
    stop("each component of 'keepY' must be non negative ")
  
  if (any(keepX > ncol(X)))
    stop("each component of 'keepX' must be lower or equal than ", P, ".")
  if (any(keepY > ncol(Y)))
    stop("each component of 'keepY' must be lower or equal than ", Q, ".")
  
  if (is.numeric(unlist(keepX.constraint)) && any(unlist(keepX.constraint) > ncol(X)))
    stop("each entry of 'keepX.constraint' must be lower or equal than ", P, ".")
  if ( is.numeric(unlist(keepY.constraint)) && any(unlist(keepY.constraint) > ncol(Y)))
    stop("each entry of 'keepY.constraint' must be lower or equal than ", Q, ".")
  
  if (!is.logical(scale))
    stop("'scale' must be either TRUE or FALSE")
  
  if (!is.logical(near.zero.var))
    stop("'near.zero.var' must be either TRUE or FALSE")
  
  # match keepX.constraint and the colnames of X in order for keepX.constraint to be a list of character
  # safety if keepX.constraint contains a mixed of character/numeric. It should one or the other, not a mix
  if (length(keepX.constraint) > 0)
  {
    if (!is.numeric(unlist(keepX.constraint)))
    {
      ind = match(unlist(keepX.constraint), colnames(X))
      if (sum(is.na(ind)) > 0)
        stop("'keepX.constraint' must contain a subset of colnames(X) or the position of the X-variables you wish to keep.")
    }
    X.indice = X[, unlist(keepX.constraint), drop=FALSE]
    keepX.constraint = relist(colnames(X.indice), skeleton=keepX.constraint)
  }
  
  # same for keepY.constraint
  if (length(keepY.constraint) > 0)
  {
    if (!is.numeric(unlist(keepY.constraint)))
    {
      ind = match(unlist(keepY.constraint),colnames(Y))
      if (sum(is.na(ind)) > 0)
        stop("'keepY.constraint' must contain a subset of colnames(Y) or the position of the Y-variables you wish to keep.")
    }
    Y.indice = Y[, unlist(keepY.constraint), drop=FALSE]
    keepY.constraint = relist(colnames(Y.indice), skeleton=keepY.constraint)
  }
  
  
  # at this stage keepA.constraint needs to be character, to easily remove variables with near zero variance
  ### near.zero.var, remove the variables with very small variances
  if (near.zero.var == TRUE)
  {
    nzv.A = nearZeroVar(X)
    
    if (length(nzv.A$Position) > 0)
    {
      names.remove.X = colnames(X)[nzv.A$Position]
      X = X[, -nzv.A$Position, drop=FALSE]
      warning("Zero- or near-zero variance predictors.\n Reset predictors matrix to not near-zero variance predictors.\n See $nzv for problematic predictors.")
      if (ncol(X) == 0)
        stop("No more variables in X")
      
      # at this stage, keepA.constraint needs to be numbers
      if (length(keepX.constraint) > 0)
      {
        #remove the variables from keepA.constraint if removed by near.zero.var
        keepX.constraint = match.keepX.constraint(names.remove.X, keepX.constraint)
      }
      #need to check that the keepA[[q]] is now not higher than ncol(A[[q]])
      if (any(keepX > ncol(X)))
      {
        ind = which(keepX > ncol(X))
        keepX[ind] = ncol(X)
      }
    }
    
  }else{nzv.A=NULL}
  
  # we need numbers in keepX.constraint from now on
  keepX.constraint = lapply(keepX.constraint,function(x){match(x, colnames(X))})
  keepY.constraint = lapply(keepY.constraint,function(x){match(x, colnames(Y))})
  
  return(list(X=X, Y=Y, ncomp=ncomp, X.names=X.names, Y.names=Y.names, ind.names=ind.names, mode=mode, keepX.constraint=keepX.constraint,
              keepY.constraint=keepY.constraint, keepX=keepX, keepY=keepY, nzv.A=nzv.A))
}

#'sPLS-DA Map
#'@description map variable for (s)plsda
#'@param Y Input data
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

map = function (Y)
{
  nrowY = nrow(Y)
  cl = numeric(nrowY)
  I = 1:nrowY
  J = 1:ncol(Y)
  for (i in I)
  {
    cl[i] = (J[Y[i, ] == max(Y[i, ])])[1]
  }
  return(cl)
}

unmap = function (classification, groups = NULL, noise = NULL)
{
  n = length(classification)
  u = sort(unique(classification))
  levels =  levels(classification)### Add levels
  
  if (is.null(groups))
  {
    groups = u
  } else {
    if (any(match(u, groups, nomatch = 0) == 0))
      stop("groups incompatible with classification")
    miss = match(groups, u, nomatch = 0) == 0
  }
  
  cgroups = as.character(groups)
  if (!is.null(noise))
  {
    noiz = match(noise, groups, nomatch = 0)
    if (any(noiz == 0))
      stop("noise incompatible with classification")
    
    groups = c(groups[groups != noise], groups[groups == noise])
    noise = as.numeric(factor(as.character(noise), levels = unique(groups)))
  }
  
  groups = as.numeric(factor(cgroups, levels = unique(cgroups)))
  classification = as.numeric(factor(as.character(classification), levels = unique(cgroups)))
  k = length(groups) - length(noise)
  nam = levels(groups)
  
  if (!is.null(noise))
  {
    k = k + 1
    nam = nam[1:k]
    nam[k] = "noise"
  }
  
  z = matrix(0, n, k, dimnames = c(names(classification), nam))
  for (j in 1:k) z[classification == groups[j], j] = 1
  attr(z, "levels") = levels
  z
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

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

GetSPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$splsr$load.x.uniq;
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
