### R script for MetaboAnalyst
### Description: 3D scatter plot for PDF report (not used for web display)
### Author: Jeff Xia, jeff.xia@mcgill.ca
### McGill University, Canada
### License: GNU GPL (>= 2)

Plot3D <- function(x, y = NULL, z = NULL, color = par("col"), pch = NULL,
     main = NULL, sub = NULL, xlim = NULL, ylim = NULL, zlim = NULL,
     xlab = NULL, ylab = NULL, zlab = NULL, scale.y = 1, angle = 40,
     axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE,
     x.ticklabs = NULL, y.ticklabs = NULL, z.ticklabs = NULL,
     y.margin.add = 0, grid = TRUE, box = TRUE, lab = par("lab"),
     lab.z = mean(lab[1:2]), type = "p", highlight.3d = FALSE,
     mar = c(5, 3, 4, 3) + 0.1, col.axis = par("col.axis"),
     col.grid = "grey", col.lab = par("col.lab"), cex.symbols = par("cex"),
     cex.axis = 0.8 * par("cex.axis"), cex.lab = par("cex.lab"),
     font.axis = par("font.axis"), font.lab = par("font.lab"),
     lty.axis = par("lty"), lty.grid = 2, lty.hide = 1,
     lty.hplot = par("lty"), log = "", ...)
     # log not yet implemented
{
    ## Uwe Ligges <ligges@statistik.tu-dortmund.de>,
    ## http://www.statistik.tu-dortmund.de/~ligges
    ##
    ## For MANY ideas and improvements thanks to Martin Maechler!!!
    ## Parts of the help files are stolen from the standard plotting functions in R.

    mem.par <- par(mar = mar)
    x.scal <- y.scal <- z.scal <- 1
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    zlabel <- if (!missing(z)) deparse(substitute(z))

    ## color as part of `x' (data.frame or list):
    if(!is.null(d <- dim(x)) && (length(d) == 2) && (d[2] >= 4))
        color <- x[,4]
    else if(is.list(x) && !is.null(x$color))
        color <- x$color

    ## convert 'anything' -> vector
    xyz <- xyz.coords(x=x, y=y, z=z, xlab=xlabel, ylab=ylabel, zlab=zlabel,
                      log=log)
    if(is.null(xlab)) { xlab <- xyz$xlab; if(is.null(xlab)) xlab <- "" }
    if(is.null(ylab)) { ylab <- xyz$ylab; if(is.null(ylab)) ylab <- "" }
    if(is.null(zlab)) { zlab <- xyz$zlab; if(is.null(zlab)) zlab <- "" }

    if(length(color) == 1)
        color <- rep(color, length(xyz$x))
    else if(length(color) != length(xyz$x))
        stop("length(color) ", "must be equal length(x) or 1")

    angle <- (angle %% 360) / 90
    yz.f <- scale.y * abs(if(angle < 1) angle else if(angle > 3) angle - 4 else 2 - angle)
    yx.f <- scale.y * (if(angle < 2) 1 - angle else angle - 3)
    if(angle > 2) { ## switch y and x axis to ensure righthand oriented coord.
        temp <- xyz$x; xyz$x <- xyz$y; xyz$y <- temp
        temp <- xlab;  xlab <- ylab;   ylab <- temp
        temp <- xlim;  xlim <- ylim;   ylim <- temp
    }
    angle.1 <- (1 < angle && angle < 2) || angle > 3
    angle.2 <- 1 <= angle && angle <= 3
    dat <- cbind(as.data.frame(xyz[c("x","y","z")]), col = color)

    n <- nrow(dat);
    y.range <- range(dat$y[is.finite(dat$y)])

### 3D-highlighting / colors / sort by y
    if(type == "p" || type == "h") {
        y.ord <- rev(order(dat$y))
        dat <- dat[y.ord, ]
        if(length(pch) > 1)
            if(length(pch) != length(y.ord))
                stop("length(pch) ", "must be equal length(x) or 1")
            else pch <- pch[y.ord]
        daty <- dat$y
        daty[!is.finite(daty)] <- mean(daty[is.finite(daty)])
        if(highlight.3d && !(all(diff(daty) == 0)))
            dat$col <- rgb(seq(0, 1, length = n) * (y.range[2] - daty) / diff(y.range), g=0, b=0)
    }

### optim. axis scaling
    p.lab <- par("lab")
    ## Y
    y.range <- range(dat$y[is.finite(dat$y)], ylim)
    y.prty <- pretty(y.range, n = lab[2],
        min.n = max(1, min(.5 * lab[2], p.lab[2])))
    y.scal <- round(diff(y.prty[1:2]), digits = 12)
    y.add <- min(y.prty)
    dat$y <- (dat$y - y.add) / y.scal
    y.max <- (max(y.prty) - y.add) / y.scal

    x.range <- range(dat$x[is.finite(dat$x)], xlim)
    x.prty <- pretty(x.range, n = lab[1],
        min.n = max(1, min(.5 * lab[1], p.lab[1])))
    x.scal <- round(diff(x.prty[1:2]), digits = 12)
    dat$x <- dat$x / x.scal
    x.range <- range(x.prty) / x.scal
    x.max <- ceiling(x.range[2])
    x.min <-   floor(x.range[1])
    if(!is.null(xlim)) {
        x.max <- max(x.max, ceiling(xlim[2] / x.scal))
        x.min <- min(x.min,   floor(xlim[1] / x.scal))
    }
    x.range <- range(x.min, x.max)
    ## Z
    z.range <- range(dat$z[is.finite(dat$z)], zlim)
    z.prty <- pretty(z.range, n = lab.z,
        min.n = max(1, min(.5 * lab.z, p.lab[2])))
    z.scal <- round(diff(z.prty[1:2]), digits = 12)
    dat$z <- dat$z / z.scal
    z.range <- range(z.prty) / z.scal
    z.max <- ceiling(z.range[2])
    z.min <-   floor(z.range[1])
    if(!is.null(zlim)) {
        z.max <- max(z.max, ceiling(zlim[2] / z.scal))
        z.min <- min(z.min,   floor(zlim[1] / z.scal))
    }
    z.range <- range(z.min, z.max)

### init graphics
    plot.new()
    if(angle.2) {x1 <- x.min + yx.f * y.max; x2 <- x.max}
    else        {x1 <- x.min; x2 <- x.max + yx.f * y.max}
    plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max))
    temp <- strwidth(format(rev(y.prty))[1], cex = cex.axis/par("cex"))
    if(angle.2) x1 <- x1 - temp - y.margin.add
    else        x2 <- x2 + temp + y.margin.add
    plot.window(c(x1, x2), c(z.min, z.max + yz.f * y.max))
    if(angle > 2) par("usr" = par("usr")[c(2, 1, 3:4)])
    usr <- par("usr") # we have to remind it for use in closures
    title(main, sub, ...)

### draw axis, tick marks, labels, grid, ...
    xx <- if(angle.2) c(x.min, x.max) else c(x.max, x.min)
    if(grid) {
	## grids
	###################
	# XY wall
        i <- x.min:x.max;
        segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + z.min,
                 col = col.grid, lty = lty.grid);

        i <- 0:y.max;
        segments(x.min + (i * yx.f), i * yz.f + z.min,
                 x.max + (i * yx.f), i * yz.f + z.min,
                 col = col.grid, lty = lty.grid);

	######################
	# XZ wall
	# verticle lines
        temp <- yx.f * y.max;
        temp1 <- yz.f * y.max;
        i <- (x.min + temp):(x.max + temp);
        segments(i, z.min + temp1, i, z.max + temp1,
                 col = col.grid, lty = lty.grid);

	# horizontal lines
        i <- (z.min + temp1):(z.max + temp1);
        segments(x.min + temp, i, x.max + temp, i,
                 col = col.grid, lty = lty.grid)


	##################
	# YZ wall
	# horizontal lines
        i <- xx[2]:x.min;
	mm <- z.min:z.max;
        segments(i, mm, i + temp, mm + temp1,
                 col = col.grid, lty = lty.grid);
	# verticle lines
        i <- 0:y.max;
        segments(x.min + (i * yx.f), i * yz.f + z.min,
                 xx[2] + (i * yx.f), i * yz.f + z.max,
                 col = col.grid, lty = lty.grid)


	# make the axis into solid line
        segments(x.min, z.min, x.min + (yx.f * y.max), yz.f * y.max + z.min,
                 col = col.grid, lty = lty.hide);
        segments(x.max, z.min, x.max + (yx.f * y.max), yz.f * y.max + z.min,
                 col = col.axis, lty = lty.hide);
        segments(x.min + (y.max * yx.f), y.max * yz.f + z.min,
                 x.max + (y.max* yx.f), y.max * yz.f + z.min,
                col = col.grid, lty = lty.hide);
        segments(x.min + temp, z.min + temp1, x.min + temp, z.max + temp1,
                col = col.grid, lty = lty.hide);
        segments(x.max + temp, z.min + temp1, x.max + temp, z.max + temp1,
                col = col.axis, lty = lty.hide);
        segments(x.min + temp, z.max + temp1, x.max + temp, z.max + temp1,
                col = col.axis, lty = lty.hide);
        segments(xx[2], z.max, xx[2] + temp, z.max + temp1,
                col = col.axis, lty = lty.hide);
    }
    if(axis) {
        if(tick.marks) { ## tick marks
            xtl <- (z.max - z.min) * (tcl <- -par("tcl")) / 50
            ztl <- (x.max - x.min) * tcl / 50
            mysegs <- function(x0,y0, x1,y1)
                segments(x0,y0, x1,y1, col=col.axis, lty=lty.axis)
            ## Y
            i.y <- 0:y.max
            mysegs(yx.f * i.y - ztl + xx[1], yz.f * i.y + z.min,
                   yx.f * i.y + ztl + xx[1], yz.f * i.y + z.min)
            ## X
            i.x <- x.min:x.max
            mysegs(i.x, -xtl + z.min, i.x, xtl + z.min)
            ## Z
            i.z <- z.min:z.max
            mysegs(-ztl + xx[2], i.z, ztl + xx[2], i.z)

            if(label.tick.marks) { ## label tick marks
                las <- par("las")
                mytext <- function(labels, side, at, ...)
                    mtext(text = labels, side = side, at = at, line = -.5,
                          col=col.lab, cex=cex.axis, font=font.lab, ...)
                ## X
                if(is.null(x.ticklabs))
                    x.ticklabs <- format(i.x * x.scal)
                mytext(x.ticklabs, side = 1, at = i.x)
                ## Z
                if(is.null(z.ticklabs))
                    z.ticklabs <- format(i.z * z.scal)
                mytext(z.ticklabs, side = if(angle.1) 4 else 2, at = i.z,
                       adj = if(0 < las && las < 3) 1 else NA)
                ## Y
                temp <- if(angle > 2) rev(i.y) else i.y ## turn y-labels around
                if(is.null(y.ticklabs))
                    y.ticklabs <- format(y.prty)
                else if (angle > 2)
                    y.ticklabs <- rev(y.ticklabs)
                text(i.y * yx.f + xx[1],
                     i.y * yz.f + z.min, y.ticklabs,
                     pos=if(angle.1) 2 else 4, offset=1,
                     col=col.lab, cex=cex.axis/par("cex"), font=font.lab)
            }
        }

        ## axis and labels

        mytext2 <- function(lab, side, line, at)
            mtext(lab, side = side, line = line, at = at, col = col.lab,
                  cex = cex.lab, font = font.axis, las = 0)
        ## X
        lines(c(x.min, x.max), c(z.min, z.min), col = col.axis, lty = lty.axis)
        mytext2(xlab, 1, line = 1.5, at = mean(x.range))
        ## Y
        lines(xx[1] + c(0, y.max * yx.f), c(z.min, y.max * yz.f + z.min),
              col = col.axis, lty = lty.axis)
        mytext2(ylab, if(angle.1) 2 else 4, line= 0.5, at = z.min + y.max * yz.f)

        ## Z
        lines(xx[c(2,2)], c(z.min, z.max), col = col.axis, lty = lty.axis)
        mytext2(zlab, if(angle.1) 4 else 2, line= 1.5, at = mean(z.range))

    }

### plot points
    x <- dat$x + (dat$y * yx.f)
    z <- dat$z + (dat$y * yz.f)
    col <- as.character(dat$col)
    if(type == "h") {
        z2 <- dat$y * yz.f + z.min
        segments(x, z, x, z2, col = col, cex = cex.symbols, lty = lty.hplot, ...)
        points(x, z, type = "p", col = col, pch = pch, cex = cex.symbols, ...)
    }
    else points(x, z, type = type, col = col, pch = pch, cex = cex.symbols, ...)

### box-lines in front of points (overlay)
    if(axis && box) {
        lines(c(x.min, x.max), c(z.max, z.max),
              col = col.axis, lty = lty.axis)
        lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + z.max,
              col = col.axis, lty = lty.axis)
        lines(xx[c(1,1)], c(z.min, z.max), col = col.axis, lty = lty.axis)
    }


    # par(mem.par) # we MUST NOT set the margins back
    ### Return Function Object
    ob <- ls() ## remove all unused objects from the result's enviroment:
    rm(list = ob[!ob %in% c("angle", "mar", "usr", "x.scal", "y.scal", "z.scal", "yx.f",
        "yz.f", "y.add", "z.min", "z.max", "x.min", "x.max", "y.max",
        "x.prty", "y.prty", "z.prty")])
    rm(ob)
    invisible(list(
        xyz.convert = function(x, y=NULL, z=NULL) {
            xyz <- xyz.coords(x, y, z)
            if(angle > 2) { ## switch y and x axis to ensure righthand oriented coord.
                temp <- xyz$x; xyz$x <- xyz$y; xyz$y <- temp
            }
            y <- (xyz$y - y.add) / y.scal
            return(list(x = xyz$x / x.scal + yx.f * y,
                y = xyz$z / z.scal + yz.f * y))
        },
        points3d = function(x, y = NULL, z = NULL, type = "p", ...) {
            xyz <- xyz.coords(x, y, z)
            if(angle > 2) { ## switch y and x axis to ensure righthand oriented coord.
                temp <- xyz$x; xyz$x <- xyz$y; xyz$y <- temp
            }
            y2 <- (xyz$y - y.add) / y.scal
            x <- xyz$x / x.scal + yx.f * y2
            y <- xyz$z / z.scal + yz.f * y2
            mem.par <- par(mar = mar, usr = usr)
            on.exit(par(mem.par))
            if(type == "h") {
                y2 <- z.min + yz.f * y2
                segments(x, y, x, y2, ...)
                points(x, y, type = "p", ...)
            }
            else points(x, y, type = type, ...)
        },
        plane3d = function(Intercept, x.coef = NULL, y.coef = NULL,
            lty = "dashed", lty.box = NULL, ...){
            if(!is.atomic(Intercept) && !is.null(coef(Intercept))) Intercept <- coef(Intercept)
            if(is.null(lty.box)) lty.box <- lty
            if(is.null(x.coef) && length(Intercept) == 3){
                x.coef <- Intercept[if(angle > 2) 3 else 2]
                y.coef <- Intercept[if(angle > 2) 2 else 3]
                Intercept <- Intercept[1]
            }
            mem.par <- par(mar = mar, usr = usr)
            on.exit(par(mem.par))
            x <- x.min:x.max
            ltya <- c(lty.box, rep(lty, length(x)-2), lty.box)
            x.coef <- x.coef * x.scal
            z1 <- (Intercept + x * x.coef + y.add * y.coef) / z.scal
            z2 <- (Intercept + x * x.coef +
                (y.max * y.scal + y.add) * y.coef) / z.scal
            segments(x, z1, x + y.max * yx.f, z2 + yz.f * y.max, lty = ltya, ...)
            y <- 0:y.max
            ltya <- c(lty.box, rep(lty, length(y)-2), lty.box)
            y.coef <- (y * y.scal + y.add) * y.coef
            z1 <- (Intercept + x.min * x.coef + y.coef) / z.scal
            z2 <- (Intercept + x.max * x.coef + y.coef) / z.scal
            segments(x.min + y * yx.f, z1 + y * yz.f,
                x.max + y * yx.f, z2 + y * yz.f, lty = ltya, ...)
        },

        wall3d = function(Intercept, x.coef = NULL, y.coef = NULL,
            lty = "dashed", lty.box = NULL, ...){
            if(!is.atomic(Intercept) && !is.null(coef(Intercept))) Intercept <- coef(Intercept)
            if(is.null(lty.box)) lty.box <- lty
            if(is.null(x.coef) && length(Intercept) == 3){
                x.coef <- Intercept[if(angle > 2) 3 else 2]
                y.coef <- Intercept[if(angle > 2) 2 else 3]
                Intercept <- Intercept[1]
            }
            mem.par <- par(mar = mar, usr = usr)
            on.exit(par(mem.par))
            x <- x.min:x.max
            ltya <- c(lty.box, rep(lty, length(x)-2), lty.box)
            x.coef <- x.coef * x.scal
            z1 <- (Intercept + x * x.coef + y.add * y.coef) / z.scal
            z2 <- (Intercept + x * x.coef +
                (y.max * y.scal + y.add) * y.coef) / z.scal
            segments(x, z1, x + y.max * yx.f, z2 + yz.f * y.max, lty = ltya, ...)
            y <- 0:y.max
            ltya <- c(lty.box, rep(lty, length(y)-2), lty.box)
            y.coef <- (y * y.scal + y.add) * y.coef
            z1 <- (Intercept + x.min * x.coef + y.coef) / z.scal
            z2 <- (Intercept + x.max * x.coef + y.coef) / z.scal
            segments(x.min + y * yx.f, z1 + y * yz.f,
                x.max + y * yx.f, z2 + y * yz.f, lty = ltya, ...)
        },
        box3d = function(...){
            mem.par <- par(mar = mar, usr = usr)
            on.exit(par(mem.par))
            lines(c(x.min, x.max), c(z.max, z.max), ...)
            lines(c(0, y.max * yx.f) + x.max, c(0, y.max * yz.f) + z.max, ...)
            lines(c(0, y.max * yx.f) + x.min, c(0, y.max * yz.f) + z.max, ...)
            lines(c(x.max, x.max), c(z.min, z.max), ...)
            lines(c(x.min, x.min), c(z.min, z.max), ...)
            lines(c(x.min, x.max), c(z.min, z.min), ...)
        }
    ))
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

    cols <- GetColorSchema(mSetObj);
    legend.nm <- unique(as.character(mSetObj$dataSet$cls));
    uniq.cols <- unique(cols);
    Plot3D(mSetObj$analSet$pca$x[, inx1], mSetObj$analSet$pca$x[, inx2], mSetObj$analSet$pca$x[, inx3], xlab= xlabel, ylab=ylabel,
           zlab=zlabel, angle =angl, color=cols, pch=pchs, box=F);
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
           zlab=zlabel, angle =angl, pch=pchs, box=F);
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
  
  cols <- GetColorSchema(mSetObj);
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  uniq.cols <- unique(cols);
  pchs <- as.numeric(mSetObj$dataSet$cls)+1;
  uniq.pchs <- unique(pchs);
  Plot3D(mSetObj$analSet$plsr$score[,inx1], mSetObj$analSet$plsr$score[,inx2], mSetObj$analSet$plsr$score[,inx3], xlab= xlabel, ylab=ylabel,
         zlab=zlabel, angle =angl, color=cols, pch=pchs, box=F);
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
