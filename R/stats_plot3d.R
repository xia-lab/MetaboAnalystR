### R script for MetaboAnalyst
### Description: 3D scatter plot for PDF report (not used for web display)
### Author: Jeff Xia, jeff.xia@mcgill.ca
### McGill University, Canada
### License: GNU GPL (>= 2)

Plot3D <- function(x, y, z, xlab= xlabel, ylab=ylabel, zlab=zlabel, angle =angl, color=cols, pch=pchs){
    # make this lazy load
    if(!exists("my.plot.scatter3d")){ # public web on same user dir
        compiler::loadcmp("../../rscripts/metaboanalystr/_util_plot3d.Rc");    
    }
    return(my.plot.scatter3d(x, y, z, xlab=xlab, ylab=ylab, zlab=zlab, angle =angle, color=color, pch=pch));
}


#'Create 3D PCA score plot
#'@description This function creates both a static 3D PCA score plot as well as an interactive 3D PCA score plot
#'using the plotly R package. The 3D PCA score plot is stored in the mSetObj (mSetObj$imgSet$pca.3d). To view
#'the plot, if your mSetObj is named mSet, type "mSet$imgSet$pca.3d" inro your R console, and the 3D plot will appear.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCA3DScoreImg(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl)
#'@param mSetObj Input name of the created mSet Object.
#'@param imgName Input a name for the plot.
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@param angl Input the angle 
#'@usage mSet <- PlotPCA3DScore(mSetObj=NA, imgName, format="json", dpi=72, width=NA, inx1, inx2, inx3, angl)
#'@export
#'@importFrom plotly plot_ly add_markers layout

PlotPCA3DScoreImg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl){
  
  mSetObj <- .get.mSet(mSetObj);
  
  xlabel = paste("PC",inx1, "(", round(100*mSetObj$analSet$pca$variance[inx1],1), "%)");
  ylabel = paste("PC",inx2, "(", round(100*mSetObj$analSet$pca$variance[inx2],1), "%)");
  zlabel = paste("PC",inx3, "(", round(100*mSetObj$analSet$pca$variance[inx3],1), "%)");
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.score3d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  
  if(mSetObj$dataSet$cls.type == "disc"){

    cols <- GetColorSchema(mSetObj$dataSet$cls);
    legend.nm <- unique(as.character(mSetObj$dataSet$cls));
    uniq.cols <- unique(cols);
    Plot3D(mSetObj$analSet$pca$x[, inx1], mSetObj$analSet$pca$x[, inx2], mSetObj$analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
           zlab=zlabel, angle =angl, color=cols, pch=pchs);
    legend("topleft", legend =legend.nm, pch=uniq.pchs, col=uniq.cols);
    dev.off();
    
    if(!.on.public.web){
      # 3D View using plotly
      if(length(uniq.pchs) > 3){
        col <- RColorBrewer::brewer.pal(length(uniq.pchs), "Set3")
      }else{
        col <- c("#1972A4", "#FF7070")
      }
      
      p <- plotly::plot_ly(x = mSetObj$analSet$pca$x[, inx1], y = mSetObj$analSet$pca$x[, inx2], z = mSetObj$analSet$pca$x[, inx3],
                 color = mSetObj$dataSet$cls, colors = col) 
      p <- plotly::add_markers(p, sizes = 5)
      p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                          yaxis = list(title = ylabel),
                          zaxis = list(title = zlabel)))
      mSetObj$imgSet$pca.3d <- p;
      print("The Interactive 3D PCA plot has been created, please find it in mSet$imgSet$pca.3d.")
    }
    
  }else{
    
    Plot3D(mSetObj$analSet$pca$x[, inx1], mSetObj$analSet$pca$x[, inx2], mSetObj$analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
           zlab=zlabel, angle =angl, pch=pchs);
    dev.off();
    
    if(!.on.public.web){
      # 3D View using plotly
      col <- c("#C61951", "#1972A4")
      p <- plotly::plot_ly(x = mSetObj$analSet$pca$x[, inx1], y = mSetObj$analSet$pca$x[, inx2], z = mSetObj$analSet$pca$x[, inx3],
                           color = pchs, colors = col, marker = list(colorbar = list(len = 1, tickmode = array, tickvals = range(unique(pchs)),
                                                                                     ticktext = levels(mSetObj$dataSet$cls)))); 
      p <- plotly::add_markers(p, sizes = 1000);
      p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                          yaxis = list(title = ylabel),
                                          zaxis = list(title = zlabel)));
      mSetObj$imgSet$pca.3d <- p;
      print("The Interactive 3D PCA plot has been created, please find it in mSet$imgSet$pca.3d.")
    }
  }
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
  cols <- GetColorSchema(mSetObj$dataSet$cls);
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  uniq.cols <- unique(cols);
  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(coords[,inx1], coords[,inx2], coords[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle = angl, color=cols, pch=pchs);
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
  cols <- GetColorSchema(mSetObj$dataSet$cls);
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  uniq.cols <- unique(cols);
  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(coords[,inx1], coords[,inx2], coords[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle = angl, color=cols, pch=pchs);
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


#'Plot PLS 3D score plot
#'@description This function creates two 3D PLS-DA score plots, the first is static for Analysis Report purposes, as well as 
#'an interactive 3D plot using the plotly R package. The 3D score plot is saved in the created mSetObj (mSetObj$imgSet$plsda.3d).
#'To view the score plot, if the name of your mSetObj is mSet, enter "mSet$imgSet$plsda.3d" to view the interactive score plot.
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

PlotPLS3DScoreImg<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, inx3, angl){
  
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
  
  mSetObj$imgSet$pls.score3d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
  
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$plsr$Xvar[inx1]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$plsr$Xvar[inx2]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  zlabel <- paste("Component", inx3, "(", round(100*mSetObj$analSet$plsr$Xvar[inx3]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  
  cols <- GetColorSchema(mSetObj$dataSet$cls);
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  uniq.cols <- unique(cols);
  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(mSetObj$analSet$plsr$score[,inx1], mSetObj$analSet$plsr$score[,inx2], mSetObj$analSet$plsr$score[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle =angl, color=cols, pch=pchs);
  legend("topleft", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  dev.off();
  
  if(!.on.public.web){
    # 3D View using plotly
    if(length(uniq.pchs) > 3){
      col <- RColorBrewer::brewer.pal(length(uniq.pchs), "Set3")
    }else{
      col <- c("#1972A4", "#FF7070")
    }
    p <- plotly::plot_ly(x = mSetObj$analSet$plsr$score[, inx1], y = mSetObj$analSet$plsr$score[, inx2], z = mSetObj$analSet$plsr$score[, inx3],
                         color = mSetObj$dataSet$cls, colors = col) 
    p <- plotly::add_markers(p, sizes = 5)
    p <- plotly::layout(p, scene = list(xaxis = list(title = xlabel),
                                        yaxis = list(title = ylabel),
                                        zaxis = list(title = zlabel)))
    
    mSetObj$imgSet$plsda.3d <- p;
    print("The Interactive 3D PLS-DA plot has been created, please find it in mSet$imgSet$plsda.3d.")
  }

  return(.set.mSet(mSetObj));
}
