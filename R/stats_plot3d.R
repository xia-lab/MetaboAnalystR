### R script for MetaboAnalyst
### Description: 3D scatter plot for PDF report (not used for web display)
### Author: Jeff Xia, jeff.xia@mcgill.ca
### McGill University, Canada
### License: GNU GPL (>= 2)

Plot3D <- function(x, y, z, xlab= xlabel, ylab=ylabel, zlab=zlabel, 
                   angle =angl, color=cols, pch=pchs){
  
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.plot.scatter3d")){
      .load.scripts.on.demand("util_plot3d.Rc");    
    }
    return(my.plot.scatter3d(x, y, z, xlab=xlab, ylab=ylab, 
                             zlab=zlab, angle =angle, color=color, pch=pch));
  }else{
    return(my.plot.scatter3d(x, y, z, xlab=xlab, ylab=ylab, 
                             zlab=zlab, angle =angle, color=color, pch=pch));
  }
}


#'Create 3D PCA score plot
#'@description This function creates both a static 3D PCA score plot as well as an interactive 3D PCA score plot
#'using the plotly R package. The 3D PCA score plot is stored in the mSetObj (mSetObj$imgSet$pca.3d). To view
#'the plot, if your mSetObj is named mSet, type "mSet$imgSet$pca.3d" inro your R console, and the 3D plot will appear.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCA3DScoreImg(mSetObj=NA, imgName, 
#'format="png", dpi=default.dpi, width=NA, inx1, inx2, inx3, angl)
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
#'@export
#'@importFrom plotly plot_ly add_markers layout

PlotPCA3DScoreImg <- function(mSetObj=NA, imgName, format="png", 
                              dpi=default.dpi, width=NA, inx1, inx2, inx3, angl){
  
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

  cls <- mSetObj$dataSet$cls;
  pchs <- as.numeric(cls)+1;
  uniq.pchs <- unique(pchs);
  
  if(mSetObj$dataSet$cls.type == "disc"){

    col.def <-  GetColorSchema(cls);
    all.cols <- ExpandSchema(cls, col.def);

    Plot3D(mSetObj$analSet$pca$x[, inx1], mSetObj$analSet$pca$x[, inx2], mSetObj$analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
           zlab=zlabel, angle =angl, color=cols, pch=pchs);
    legend("topleft", legend = names(col.def), pch=uniq.pchs, col=col.def);
    dev.off();
    
    
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
PlotSPLS3DScoreImg<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, inx3, angl){
  
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
  cls <- mSetObj$dataSet$cls;
  col.def <-  GetColorSchema(cls);
  cols <- ExpandSchema(cls, col.def);
  pchs <- as.numeric(cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(coords[,inx1], coords[,inx2], coords[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle = angl, color=cols, pch=pchs);
  legend("topleft", legend = names(col.def), pch=uniq.pchs, col=col.def);
  dev.off();
  
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
PlotSPLS3DScoreImg<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, inx3, angl){
  
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
  cls <- mSetObj$dataSet$cls;
  col.def <-  GetColorSchema(cls);
  cols <- ExpandSchema(cls, col.def);
  pchs <- as.numeric(cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(coords[,inx1], coords[,inx2], coords[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle = angl, color=cols, pch=pchs);
  legend("topleft", legend = names(col.def), pch=uniq.pchs, col=col.def);
  dev.off();
  
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

PlotPLS3DScoreImg<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, inx3, angl){
  
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
  
  cls <- mSetObj$dataSet$cls;
  col.def <-  GetColorSchema(cls);
  cols <- ExpandSchema(cls, col.def);

  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(mSetObj$analSet$plsr$score[,inx1], mSetObj$analSet$plsr$score[,inx2], mSetObj$analSet$plsr$score[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle =angl, color=cols, pch=pchs);
  legend("topleft", legend = names(col.def), pch=uniq.pchs, col=col.def);
  dev.off();

  return(.set.mSet(mSetObj));
}

#' Pack mesh3d object for JSON serialization
#' @description Extracts essential fields from mesh3d objects for JavaScript consumption
#' @param obj A mesh3d object or list of mesh3d objects
#' @return A list with type, colors, vertices, and vb fields
.pack_mesh3d <- function(obj) {
  if (inherits(obj, "mesh3d")) {
    out <- list()
    if (!is.null(obj$type)) out$type <- obj$type
    if (!is.null(obj$colors)) out$colors <- obj$colors
    if (!is.null(obj$vertices)) out$vertices <- obj$vertices
    if (!is.null(obj$vb)) out$vb <- obj$vb
    return(out)
  }
  if (is.list(obj)) {
    return(lapply(obj, .pack_mesh3d))
  }
  obj
}

#' Compute 3D encasing mesh (ellipse, alpha shape, or kernel density)
#' @description Computes 3D encasing for a group of samples in scatter plot
#' @param filenm Output filename for the JSON mesh data
#' @param type Type of encasing: "ellipse", "alpha", or "kde"
#' @param names.vec Sample names separated by "; "
#' @param level Confidence level (default 0.95)
#' @param omics Omics type (optional)
#' @return The filename of the created JSON file
#' @export
ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){
  level <- as.numeric(level)
  names <- strsplit(names.vec, "; ")[[1]]

  # Read SCALED coordinates from rdt.set.qs (created by my.json.scatter)
  # Coordinates are scaled to [-1, 1] to match JSON node positions (JS multiplies by 1000)
  if (!file.exists("rdt.set.qs")) {
    message("[ComputeEncasing] rdt.set.qs not found")
    return("NA")
  }
  rdt.set <- qs::qread("rdt.set.qs")
  pos.xyz <- rdt.set$pos.xyz

  inx <- rownames(pos.xyz) %in% names
  if (sum(inx) < 3) {
    message("[ComputeEncasing] Need at least 3 matching points")
    return("NA")
  }

  coords <- as.matrix(pos.xyz[inx, 1:3])

  tryCatch({
    mesh <- rsclient_isolated_exec(
      func_body = function(input_data) {
        Sys.setenv(RGL_USE_NULL = TRUE)
        cov_mat <- cov(input_data$coords)
        if (any(is.na(cov_mat)) || det(cov_mat) <= 0) return(NULL)
        center <- colMeans(input_data$coords)
        mesh <- list()
        mesh[[1]] <- rgl::ellipse3d(x = cov_mat, centre = center, level = input_data$level)
        mesh
      },
      input_data = list(coords = coords, level = level),
      packages = c("rgl", "qs"), timeout = 120, output_type = "qs"
    )
    if (is.list(mesh) && isFALSE(mesh$success)) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }
    if (is.null(mesh)) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }
    jsonlite::write_json(.pack_mesh3d(mesh), filenm, auto_unbox = TRUE, pretty = FALSE)
    return(filenm)
  }, error = function(e) {
    message("[ComputeEncasing] ", e$message)
    sink(filenm); cat("{}"); sink()
    return(filenm)
  })
}

#' Compute 3D encasing mesh for multiple groups (batch mode)
#' @description Computes 3D encasing for multiple groups defined in JSON
#' @param filenm Output filename for the JSON mesh data
#' @param type Type of encasing: "ellipse", "alpha", or "kde"
#' @param groups.json JSON string with array of groups: [{"group":"name","ids":"id1; id2; ..."},...]
#' @param level Confidence level (default 0.95)
#' @param omics Omics type (optional)
#' @return The filename of the created JSON file
#' @export
ComputeEncasingBatch <- function(filenm, type, groups.json, level=0.95, omics="NA"){
  level <- as.numeric(level)

  # Parse JSON input
  groups <- tryCatch({
    jsonlite::fromJSON(groups.json)
  }, error = function(e) {
    # Try unescaping if JSON has escaped quotes
    tryCatch({
      jsonlite::fromJSON(gsub('\\\\"', '"', groups.json))
    }, error = function(e2) {
      message("[ComputeEncasingBatch] JSON parse failed: ", e2$message)
      return(NULL)
    })
  })

  if (is.null(groups)) return("NA")
  if (!is.data.frame(groups)) groups <- as.data.frame(groups)
  if (nrow(groups) == 0) return("NA")

  # Read SCALED coordinates from rdt.set.qs (created by my.json.scatter)
  # Coordinates are scaled to [-1, 1] to match JSON node positions (JS multiplies by 1000)
  if (!file.exists("rdt.set.qs")) {
    message("[ComputeEncasingBatch] rdt.set.qs not found")
    return("NA")
  }
  rdt.set <- qs::qread("rdt.set.qs")
  pos.xyz <- rdt.set$pos.xyz

  # Extract coords per group in master
  group_data <- lapply(1:nrow(groups), function(i) {
    nms <- strsplit(groups$ids[i], "; ")[[1]]
    inx <- rownames(pos.xyz) %in% nms
    list(group = groups$group[i], coords = as.matrix(pos.xyz[inx, 1:3]))
  })

  tryCatch({
    # Single subprocess for all groups
    result_list <- rsclient_isolated_exec(
      func_body = function(input_data) {
        Sys.setenv(RGL_USE_NULL = TRUE)
        lapply(input_data$groups, function(g) {
          coords <- g$coords
          if (nrow(coords) < 3) return(NULL)
          tryCatch({
            cov_mat <- cov(coords)
            if (any(is.na(cov_mat)) || det(cov_mat) <= 0) return(NULL)
            center <- colMeans(coords)
            mesh <- rgl::ellipse3d(x = cov_mat, centre = center, level = input_data$level)
            list(group = g$group, mesh = list(mesh))
          }, error = function(e) NULL)
        })
      },
      input_data = list(groups = group_data, level = level),
      packages = c("rgl", "qs"), timeout = 120, output_type = "qs"
    )

    if (is.list(result_list) && isFALSE(result_list$success)) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }

    # Filter NULLs and pack mesh3d objects
    result_list <- result_list[!sapply(result_list, is.null)]
    if (length(result_list) == 0) {
      sink(filenm); cat("{}"); sink()
      return(filenm)
    }

    result_array <- lapply(result_list, function(r) {
      list(group = r$group, mesh = lapply(r$mesh, .pack_mesh3d))
    })
    jsonlite::write_json(result_array, filenm, auto_unbox = TRUE, pretty = FALSE)
    return(filenm)
  }, error = function(e) {
    message("[ComputeEncasingBatch] ", e$message)
    sink(filenm); cat("{}"); sink()
    return(filenm)
  })
}
