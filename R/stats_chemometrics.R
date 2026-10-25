###############################################
## Shared 2D Score Plot Utilities
## Used by PCA, PLS-DA, oPLS-DA, sPLS-DA
###############################################

#' Get scatter plot options from session (safe, with defaults)
#' Returns list: level, method ("chisq"/"f"), type ("norm"/"t" for stat_ellipse),
#'               use.asp (TRUE if uniform scaling), scaleMode
#' @param default.level fallback confidence level if session not available.
#'   A level of 0 (the "Display 95% confidence region" box unchecked) means no
#'   ellipses and is never overridden by the session's confidence level.
.get.scatter.params <- function(default.level = 0.95) {
  level <- default.level
  ellipse.off <- is.numeric(default.level) && length(default.level) == 1 &&
                 !is.na(default.level) && default.level <= 0
  method <- "chisq"
  type <- "norm"
  scaleMode <- "independent"
  hideTightEllipse <- FALSE
  tryCatch({
    if (exists("GetScatterOptions")) {
      opts <- GetScatterOptions()
      if (!is.null(opts$confidenceLevel) && !ellipse.off) level <- opts$confidenceLevel
      if (!is.null(opts$confMethod)) method <- opts$confMethod
      if (!is.null(opts$scaleMode)) scaleMode <- opts$scaleMode
      if (!is.null(opts$hideTightEllipse)) hideTightEllipse <- opts$hideTightEllipse
      if (method == "f") type <- "t"
    }
  }, error = function(e) {})
  list(level = level, method = method, type = type,
       use.asp = (scaleMode == "uniform"), scaleMode = scaleMode,
       hideTightEllipse = hideTightEllipse)
}

# Keep old name as alias
.get.2d.confidence.params <- .get.scatter.params

#' Compute 2D ellipse radius (t-value) for a group
#' chi-squared: sqrt(qchisq(level, 2))
#' F-distribution: sqrt(2 * qf(level, 2, n-1))
#' @param level confidence level
#' @param method "chisq" or "f"
#' @param n group sample size
GetEllipseRadius <- function(level, method, n) {
  if (method == "f" && n > 2) {
    return(sqrt(2 * qf(level, 2, n - 1)))
  }
  return(sqrt(qchisq(level, 2)))
}

#' Axis-ratio "sliver" test shared by the base-R ellipse path (.compute.group.ellipse,
#' used by the PCA/PLS-DA/oPLS-DA/sPLS-DA score plots) and the ggplot2 biplot path
#' (.ellipse.layer.data, used by PlotPCABiplot/PlotPLSBiplot).
#' With a handful of near-collinear points the covariance is numerically singular
#' in one direction, and an ellipse drawn from it is a needle spanning the whole
#' panel -- a 4-sample group rendered as a diagonal sliver from corner to corner,
#' which reads as a huge confidence region when it actually means "no reliable
#' spread". Threshold is on the ELLIPSE AXIS ratio (sqrt of the eigenvalue ratio),
#' because that is the property actually seen: 0.1 means the minor axis is a
#' tenth of the major, i.e. already a sliver. Measured on real 4-sample groups, a
#' near-collinear one gives an eigenvalue ratio ~1e-3 (axis ratio ~0.03) while a
#' genuinely spread group gives ~0.7 (axis ratio ~0.8) -- the two regimes are
#' orders of magnitude apart, so the exact cut is not delicate.
#' @return the axis ratio, or NA if n < 3 or the covariance is singular/degenerate
#'   (those groups can't get an ellipse at all, regardless of this test)
.ellipse.axis.ratio <- function(x, y) {
  if (length(x) < 3) return(NA_real_)
  groupVar <- tryCatch(var(cbind(x, y), na.rm = TRUE), error = function(e) NULL)
  ev <- tryCatch(eigen(groupVar, symmetric = TRUE, only.values = TRUE)$values,
                 error = function(e) NULL)
  if (is.null(ev) || any(!is.finite(ev)) || max(ev) <= 0) return(NA_real_)
  sqrt(max(min(ev), 0) / max(ev))
}

#' Compute confidence ellipse points for one group
#' Handles both chi-squared and F-distribution methods, guards n < 3
#' @param x vector of x-coordinates for the group
#' @param y vector of y-coordinates for the group
#' @param params list from .get.scatter.params()
#' @param npoints number of ellipse points (default 100)
#' @return 2-column matrix of ellipse coordinates, or NA matrix if n < 3
.compute.group.ellipse <- function(x, y, params, npoints = 100) {
  n <- length(x)
  if (n < 3) return(matrix(NA, npoints, 2))
  # Confidence region switched off by the user: an all-NA slice draws nothing
  if (!is.finite(params$level) || params$level <= 0) return(matrix(NA, npoints, 2))
  groupVar <- var(cbind(x, y), na.rm = TRUE)
  axisRatio <- .ellipse.axis.ratio(x, y)
  if (is.na(axisRatio)) return(matrix(NA, npoints, 2))
  # Off by default -- an omitted ellipse with no on-screen explanation was reported
  # as a display bug (an ellipse silently missing looks identical whether the
  # group's spread is a genuine sliver or the plot just failed); showing the
  # sliver is more transparent than hiding it unless the user opts in.
  if (isTRUE(params$hideTightEllipse) && axisRatio < 0.1) {
    return(matrix(NA, npoints, 2))
  }
  groupMean <- c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE))
  radius <- GetEllipseRadius(params$level, params$method, n)
  ellipse::ellipse(groupVar, centre = groupMean, t = radius, npoints = npoints)
}

#' Initialize a 2D score plot with proper aspect ratio
#' @param x x-coordinates
#' @param y y-coordinates
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#' @param xlim x limits (or NULL for auto)
#' @param ylim y limits (or NULL for auto)
#' @param params list from .get.scatter.params()
#' @param main plot title
#' @param ... extra args to plot()
.init.score.plot <- function(x, y, xlabel, ylabel, xlim = NULL, ylim = NULL,
                              params = NULL, main = "Scores Plot", ...) {
  asp <- if (!is.null(params) && params$use.asp) 1 else NA
  if (!is.null(xlim) && !is.null(ylim)) {
    plot(x, y, xlab = xlabel, ylab = ylabel, xlim = xlim, ylim = ylim,
         type = 'n', main = main, asp = asp, ...)
  } else {
    plot(x, y, xlab = xlabel, ylab = ylabel,
         type = 'n', main = main, asp = asp, ...)
  }
  grid(col = "lightgray", lty = "dotted", lwd = 1)
}

#' Data for a ggplot2 stat_ellipse() layer, with near-collinear "sliver" groups
#' dropped when the session's hideTightEllipse option is on (see
#' .ellipse.axis.ratio). Off by default: returns data unchanged, so every
#' group's ellipse shows (ggplot2's own stat_ellipse already skips a group
#' with fewer than 3 points on its own; that is unaffected here).
#' Used by the PCA/PLS biplots -- their ellipses are drawn per Group by
#' ggplot2 itself (aes(fill = Group)), not via .compute.group.ellipse.
#' @param data data frame with one row per sample
#' @param xcol,ycol names of the x/y coordinate columns
#' @param groupcol name of the grouping column
#' @param params list from .get.scatter.params() (the session's now, unless a Draw<X> passes the one its plot data holds)
.ellipse.layer.data <- function(data, xcol, ycol, groupcol, params = .get.scatter.params()) {
  if (!isTRUE(params$hideTightEllipse)) return(data)
  groups <- as.character(data[[groupcol]])
  isTight <- vapply(split(seq_len(nrow(data)), groups), function(idx) {
    ratio <- .ellipse.axis.ratio(data[[xcol]][idx], data[[ycol]][idx])
    !is.na(ratio) && ratio < 0.1
  }, logical(1))
  tightGroups <- names(isTight)[isTight]
  data[!(groups %in% tightGroups), , drop = FALSE]
}

#'Perform PCA analysis
#'@description Perform PCA analysis, obtain variance explained, store item to PCA object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'@param mSetObj Input name of the created mSet Object
#'McGill University, Canada
#'License: MIT
#'@export
#'
PCA.Anal <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  pca <- prcomp(mSetObj$dataSet$norm, center=TRUE, scale=F);

  bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
  bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
  ov_qs_save(list(pca = pca), bridge_in, preset = "fast")
  on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

  run_func_via_microservice(
    func = function(wd, bridge_in, bridge_out) {
      setwd(wd)
      require(factoextra)
      input <- ov_qs_read(bridge_in)
      res <- factoextra::get_pca_var(input$pca)$contrib
      ov_qs_save(res, bridge_out, preset = "fast")
    },
    args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
    timeout_sec = 120
  )

  contrib <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL
  if (is.null(contrib)) { AddErrMsg("PCA contrib calculation failed"); return(0) }
  if(ncol(contrib)>10){
    contrib <- contrib[,1:10];# keep top 10 should be sufficient
  }
  
  # obtain variance explained
  sum.pca <- summary(pca);
  imp.pca <- sum.pca$importance;
  std.pca <- imp.pca[1,]; # standard devietation
  var.pca <- imp.pca[2,]; # variance explained by each PC
  cum.pca <- imp.pca[3,]; # cummulated variance explained

  # store the item to the pca object
  mSetObj$analSet$pca<-append(pca, list(std=std.pca, variance=var.pca, cum.var=cum.pca, contrib = contrib));
  fast.write.csv(signif(mSetObj$analSet$pca$x,5), file="pca_score.csv");
  fast.write.csv(signif(mSetObj$analSet$pca$rotation,5), file="pca_loadings.csv");
  mSetObj$analSet$pca$loading.type <- "all";
  mSetObj$custom.cmpds <- c();

  return(.set.mSet(mSetObj));
}

## PERMANOVA of a score matrix by a grouping (discrete) or a covariate (continuous),
## computed directly. With Euclidean distances the pseudo-F is a ratio of sums of
## squares of the centred scores, identical to vegan::adonis2(dist(M) ~ grp), so no
## distance matrix, no separate R process and no vegan call are needed; one
## permutation costs O(n). Returns c(F, R2, p, n): F value, R-squared, permutation
## p-value and the number of samples used (NA rows are dropped).
.permanova_euclid <- function(pc.mat, grp, cls.type = "disc", permutations = 999L, seed = 20261003L) {
  M <- as.matrix(pc.mat)
  ok <- stats::complete.cases(M) & !is.na(grp)
  M <- M[ok, , drop = FALSE]
  grp <- grp[ok]
  n <- nrow(M)
  na.res <- c(F = NA_real_, R2 = NA_real_, p = NA_real_, n = n)
  if (n < 3) return(na.res)
  M <- sweep(M, 2, colMeans(M))
  tot <- sum(M^2)

  if (cls.type == "cont") {
    obs <- as.numeric(grp)
    if (stats::var(obs) == 0) return(na.res)
    stat <- function(x) {
      xc <- x - mean(x)
      b <- sum(crossprod(M, xc)^2) / sum(xc^2)
      c(b / ((tot - b) / (n - 2)), b / tot)
    }
  } else {
    obs <- as.integer(droplevels(as.factor(grp)))
    g <- max(obs)
    if (g < 2 || n <= g) return(na.res)
    nk <- tabulate(obs, g)
    stat <- function(idx) {
      b <- sum(sweep(rowsum(M, idx), 1, sqrt(nk), "/")^2)
      c((b / (g - 1)) / ((tot - b) / (n - g)), b / tot)
    }
  }

  obs.stat <- stat(obs)
  # a fixed seed (the session's random stream is left as it was): the same scores and grouping always
  # give the same p-value, so the score-plot line, the all-factor table and the pair plot agree
  if (!is.null(seed)) {
    had.seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had.seed) old.seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(if (had.seed) assign(".Random.seed", old.seed, envir = .GlobalEnv) else
              if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    set.seed(seed)
  }
  fp <- vapply(seq_len(permutations), function(i) stat(sample(obs))[1], numeric(1))
  p <- (sum(fp >= obs.stat[1] - 1e-10 * abs(obs.stat[1])) + 1) / (permutations + 1)
  c(F = obs.stat[1], R2 = obs.stat[2], p = p, n = n)
}

.permanova_euclid_p <- function(pc.mat, grp, cls.type = "disc", permutations = 999L) {
  unname(.permanova_euclid(pc.mat, grp, cls.type, permutations)["p"])
}

## overall PERMANOVA on the current score plot plus, for discrete groups, a table of
## all group pairs (R2, F, p and the FDR-adjusted p)
.calculateDistSig <- function(pc.mat, grp, cls.type = "disc",
                              permutations = getOption("ov.permanova.nperm", 999L), overall = NULL) {
  # overall: the F, R2 and p of the same test when it has been computed already (it is not repeated)
  all.res <- if (is.null(overall)) .permanova_euclid(pc.mat, grp, cls.type, permutations) else overall
  res <- data.frame(F = all.res[["F"]], R2 = all.res[["R2"]], Pr = all.res[["p"]])

  pair.res <- NULL
  if (cls.type != "cont") {
    grp <- as.factor(grp)
    lvs <- levels(droplevels(grp[!is.na(grp)]))
    # >= 2, not > 2: a two-group design has exactly one pair, and reporting it
    # keeps the table describing the groups actually analysed.
    if (length(lvs) >= 2) {
      co <- combn(lvs, 2)
      out <- data.frame(F.Model = numeric(ncol(co)), R2 = numeric(ncol(co)),
                        pval = numeric(ncol(co)), row.names = paste(co[1, ], "vs", co[2, ]))
      for (j in seq_len(ncol(co))) {
        ij <- !is.na(grp) & grp %in% co[, j]
        r <- .permanova_euclid(as.matrix(pc.mat)[ij, , drop = FALSE], grp[ij], "disc", permutations)
        out[j, ] <- c(r[["F"]], r[["R2"]], r[["p"]])
      }
      out$p.adj <- p.adjust(out$pval, method = "fdr")
      pair.res <- signif(out, 5)
    }
  }

  if (!is.null(pair.res)) {
    fast.write.csv(pair.res, file = "pca_pairwise_permanova.csv")
  } else {
    # Nothing to report for this design: drop any table a previous run left behind
    # rather than let it be read as the current result.
    unlink("pca_pairwise_permanova.csv")
  }
  return(list(res, pair.res))
}

## PERMANOVA of every metadata factor on the same two PCs (the iPCA score plot shows a
## single factor as colour; this tests all of them). meta.info is a data frame with
## one column per factor and meta.types gives "disc" or "cont" for each column.
.permanova_all_meta <- function(pc.mat, meta.info, meta.types, permutations = 999L) {
  rows <- lapply(seq_len(ncol(meta.info)), function(k) {
    ty <- if (identical(as.character(meta.types[[k]]), "cont")) "cont" else "disc"
    v <- meta.info[[k]]
    r <- .permanova_euclid(pc.mat, v, ty, permutations)
    nlev <- if (ty == "disc") nlevels(droplevels(as.factor(v[!is.na(v)]))) else NA_integer_
    data.frame(Factor = colnames(meta.info)[k], Type = if (ty == "cont") "continuous" else "discrete",
               Levels = nlev, Samples = r[["n"]], F.value = r[["F"]], R2 = r[["R2"]], p.value = r[["p"]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$p.adj <- p.adjust(out$p.value, method = "fdr")
  out[order(out$p.value, na.last = TRUE), , drop = FALSE]
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

  for(j in 1:nco) {
    #cat(round(j/nco*100,0),'...  ')
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

  out$p.adj <- p.adjust(out$pval, method=padj)
  out$SumOfSqs <-NULL

  return(out)
}

#'Rotate PCA analysis
#'@description Rotate PCA analysis
#'@param mSetObj Input name of the created mSet Object
#'@param axisOpt Input the axis option 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
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
#'@usage PlotPCAPairSummary(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, pc.num)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
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
PlotPCAPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, pc.num){
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
#'License: MIT
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param scree.num Numeric, input a number to indicate the number of principal components to display in the scree plot.
#'@usage PlotPCAScree(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, scree.num)
#'@export
#'
PlotPCAScree <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, scree.num){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pcvars<-mSetObj$analSet$pca$variance[1:scree.num];
  cumvars<-mSetObj$analSet$pca$cum.var[1:scree.num];

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pca.scree <- imgName;

  # The plot data is what DrawPCAScree reads: the variance explained per component and its running total.
  pd <- list(pcvars = pcvars, cumvars = cumvars, scree.num = scree.num);
  DrawPCAScree(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawPCAScree", list(imgName = img.base, format = format, dpi = dpi, width = width),
                export = list(pca_scree = data.frame(PC = seq_along(pcvars), VarianceExplained = pcvars, CumulativeVariance = cumvars)));
  return(.set.mSet(mSetObj));
}

# Draws the PCA scree plot from its plot data alone (pd: pcvars, cumvars, scree.num).
DrawPCAScree <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  pcvars <- pd$pcvars;
  cumvars <- pd$cumvars;
  scree.num <- pd$scree.num;

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
  return(invisible(NULL));
}

#'Create 2D PCA score plot
#'@description Rotate PCA analysis
#'@usage PlotPCA2DScore(mSetObj=NA, imgName, format="png", 
#'dpi=default.dpi, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0)
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
#'@param cex.opt Character, controls label size adjustment: "na" resets to default (1.0), "increase" enlarges, or any other value shrinks. Default is "na".
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PlotPCA2DScore <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi,
                           width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0, cex.opt="na"){
  # Override reg with session confidence level if available
  sp <- .get.scatter.params(reg)
  reg <- sp$level
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

  if(cls.type == "disc"){
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(cls))[cls]);
    }
  }

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pca_score2d <- imgName;

  # The plot data is what DrawPCA2DScore reads: the scores, the groups, the confidence-region settings and the
  # session's group colours / shapes as they are now.
  pd <- list(pc1 = pc1, pc2 = pc2, xlabel = xlabel, ylabel = ylabel, text.lbls = text.lbls,
             cls = cls, cls.type = cls.type, sp = sp, pca.cex = pca.cex,
             col.vec = get0("colVec"), shape.vec = get0("shapeVec"));
  DrawPCA2DScore(pd, img.base, format, dpi, width, show, grey.scale);
  .ov_plot_data(imgName, pd, "DrawPCA2DScore",
                list(imgName = img.base, format = format, dpi = dpi, width = width, show = show, grey.scale = grey.scale),
                export = list(pca_score2d = data.frame(Sample = names(pc1), PCx = pc1, PCy = pc2, Group = mSetObj$dataSet$cls)));

  if(nrow(mSetObj[['dataSet']][['meta.info']])<=200){
    permanova_results <- ComputePERMANOVA(pc1, pc2, mSetObj$dataSet$cls)
    mSetObj$analSet$pca$permanova.res <-permanova_results;
  }
  return(.set.mSet(mSetObj));
}

# Draws the 2D PCA score plot from its plot data alone
# (pd: pc1, pc2, xlabel, ylabel, text.lbls, cls, cls.type, sp, pca.cex, col.vec, shape.vec).
DrawPCA2DScore <- function(pd, imgName, format="png", dpi=default.dpi, width=NA, show=1, grey.scale=0){
  pc1 <- pd$pc1; pc2 <- pd$pc2;
  xlabel <- pd$xlabel; ylabel <- pd$ylabel;
  text.lbls <- pd$text.lbls;
  cls <- pd$cls; cls.type <- pd$cls.type;
  sp <- pd$sp;
  pca.cex <- pd$pca.cex;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # margin big
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op<-par(mar=c(5,5,3,8));
  
  if(cls.type == "disc"){
    # obtain ellipse points to the scatter plot for each category
    
    lvs <- levels(cls);
    pts.array <- array(0, dim=c(100,2,length(lvs)));
    for(i in 1:length(lvs)){
      inx <- cls == lvs[i];
      pts.array[,,i] <- .compute.group.ellipse(pc1[inx], pc2[inx], sp)
    }
    
    # A skipped ellipse is an all-NA slice; without na.rm the range is NA and
    # plot() then fails on non-finite xlim/ylim, losing the whole scores plot.
    xrg <- range(pc1, pts.array[,1,], na.rm = TRUE);
    yrg <- range(pc2, pts.array[,2,], na.rm = TRUE);
    x.ext<-(xrg[2]-xrg[1])/12;
    y.ext<-(yrg[2]-yrg[1])/12;
    xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
    ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
    
    col.def <- GetColorSchema(cls, grey.scale==1, pd$col.vec);
    cols <- ExpandSchema(cls, col.def);
    
    plot(pc1, pc2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot",
         col=cols, pch=as.numeric(cls)+1, asp=if(sp$use.asp) 1 else NA); ## added
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
    
    pchs.def <- GetShapeSchema(cls, show, grey.scale, pd$shape.vec, pd$col.vec);
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
        if(grey.scale == 1 | (!is.null(pd$shape.vec) && all(pd$shape.vec>=0))){
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
    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, type='n', main="Scores Plot", asp=if(sp$use.asp) 1 else NA);
    points(pc1, pc2, pch=15, col="magenta");
    text(pc1, pc2, label=text.lbls, pos=4, col ="blue", xpd=T, cex=0.8*pca.cex);
  }
  par(op);
  dev.off();
  return(invisible(NULL));
}

#'Create 3D PCA score plot
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
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
  ov_qs_save(pca3d$score, "score3d.qs");

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
  ov_qs_save(pca3d$loading, "loading3d.qs");

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
#'License: MIT
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
#'License: MIT
#'@usage PlotPCALoading(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2)
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
PlotPCALoading <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
 
  loadings<-as.matrix(cbind(mSetObj$analSet$pca$rotation[,inx1],mSetObj$analSet$pca$rotation[,inx2]));
  # Keep finite rows only to avoid blank plots when rotations contain NAs/Infs.
  keep.inx <- is.finite(loadings[,1]) & is.finite(loadings[,2]);
  loadings <- loadings[keep.inx, , drop=FALSE];
  # sort based on absolute values of 1, 2 
  ord.inx <- order(-abs(loadings[,1]), -abs(loadings[,2]));
  loadings <- signif(loadings[ord.inx,,drop=FALSE],5);

  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2);
  colnames(loadings)<-c(ldName1, ldName2);
  mSetObj$analSet$pca$imp.loads<-loadings; # set up the loading matrix

  # Arrow export for zero-copy Java access (PCA loadings)
  ExportResultMatArrow(loadings, "pca_load");

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  mSetObj$imgSet$pca.loading <- imgName;

  # The plot data is what DrawPCALoading reads: the loadings of the two components and which features to label.
  pd <- list(loadings = loadings, ldName1 = ldName1, ldName2 = ldName2,
             plotType = mSetObj$analSet$pca$loading.type, custom.cmpds = mSetObj$custom.cmpds);
  mSetObj$pca.axis.lims <- DrawPCALoading(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawPCALoading", list(imgName = img.base, format = format, dpi = dpi, width = width),
                export = list(pca_loading = loadings));
  return(.set.mSet(mSetObj));
  
}

# Draws the PCA loading plot from its plot data alone (pd: loadings, ldName1, ldName2, plotType, custom.cmpds).
# Returns the axis limits of the plot (par("usr")).
DrawPCALoading <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  loadings <- pd$loadings; ldName1 <- pd$ldName1; ldName2 <- pd$ldName2; plotType <- pd$plotType;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(6,5,2,6));
  plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);

  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  points(loadings[,1],loadings[,2], pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  
  if(plotType=="all"){
    lbl.inx <- seq_len(nrow(loadings));
    # With many features the labels overprint into an unreadable mass; above 100 keep only
    # the peripheral features (largest distance from the origin = highest contribution) so the
    # informative outliers stay legible. All points are still plotted, only labels are thinned.
    if(nrow(loadings) > 100){
      d <- sqrt(loadings[,1]^2 + loadings[,2]^2);
      lbl.inx <- utils::head(order(d, decreasing=TRUE), 100);
    }
    text(loadings[lbl.inx,1],loadings[lbl.inx,2], labels=substr(rownames(loadings)[lbl.inx], 1, 16), pos=4, col="blue", xpd=T);
  }else if(plotType == "custom"){
    if(length(pd$custom.cmpds) > 0){
      hit.inx <- rownames(loadings) %in% pd$custom.cmpds;
      text(loadings[hit.inx,1],loadings[hit.inx,2], labels=rownames(loadings)[hit.inx], pos=4, col="blue", xpd=T);
    }
  }else{
    # do nothing
  }
  
  dev.off();
  return(axis.lims);
}

#'Create PCA Biplot, set xpd = T to plot outside margin
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@usage PlotPCABiplot(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param topnum Integer, the number of top contributing features to display on the biplot. Default is 10.
#'@export
#'
PlotPCABiplot <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2,topnum=10){
  # print(topnum)
  mSetObj <- .get.mSet(mSetObj);
 
  choices <- c(inx1, inx2);
  scores <- mSetObj$analSet$pca$x;
  lam <- mSetObj$analSet$pca$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n);
  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
    mSetObj$imgSet$pca.biplot<-imgName;

    library(dplyr)
    #library(factoextra) 
    #pca <- prcomp(mSetObj$dataSet$norm, scale=F);
    pca<-mSetObj$analSet$pca;
    contrib <- mSetObj$analSet$pca$contrib;
    contrib_pc1_pc2 <- contrib[, 1] + contrib[, 2]

    top_10_features <- names(sort(contrib_pc1_pc2, decreasing = TRUE))[1:topnum]
    top_10_features <- top_10_features[!is.na(top_10_features)];

    cls <- mSetObj$dataSet$cls;
    cls.type <- mSetObj$dataSet$cls.type;
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
        cls <- as.factor(as.numeric(levels(cls))[cls]);
    }else{
      cls <- cls;
    }


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

  xlabel = paste("PC",inx1, "(", round(100*mSetObj$analSet$pca$variance[inx1],1), "%)");
 ylabel = paste("PC",inx2, "(", round(100*mSetObj$analSet$pca$variance[inx2],1), "%)");

  # The plot data is what DrawPCABiplot reads: the sample scores, the labelled feature arrows, the confidence-region
  # settings and the session's group colours as they are now.
  pd <- list(ind_data = ind_data, var_data = var_data, cls = cls, xlabel = xlabel, ylabel = ylabel,
             sp = .get.scatter.params(), col.vec = get0("colVec"));
  DrawPCABiplot(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawPCABiplot", list(imgName = img.base, format = format, dpi = dpi, width = width),
                export = list(pca_biplot = ind_data));

  return(.set.mSet(mSetObj));
}

# Draws the PCA biplot from its plot data alone (pd: ind_data, var_data, cls, xlabel, ylabel, sp, col.vec).
DrawPCABiplot <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
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
  ind_data <- pd$ind_data; var_data <- pd$var_data; xlabel <- pd$xlabel; ylabel <- pd$ylabel;

 cols <- GetColorSchema(pd$cls, FALSE, pd$col.vec); 
p <- ggplot() +
  geom_point(data = ind_data, aes(x = PC1, y = PC2, color = Group), size =3.5, alpha = 0.7) +
  stat_ellipse(data = .ellipse.layer.data(ind_data, "PC1", "PC2", "Group", pd$sp), aes(x = PC1, y = PC2, fill = Group, color = Group), type = pd$sp$type, level = pd$sp$level, geom = "polygon", alpha = 0.2) +
  geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "black", arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.5, show.legend = FALSE) +
  geom_text_repel(data = var_data, aes(x = PC1, y = PC2, label = Variable),
                  size = 4, color = "#3B3B3B", max.overlaps = Inf, segment.color = "#636363") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  labs(title = "Biplot", x = xlabel, y = ylabel) +
  theme_minimal() +
 scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    legend.position = "right",
    legend.title = element_blank(),
   legend.text = element_text(size = 12), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Add a black border around the plot
    panel.grid.major = element_line(color = "grey80",linetype = "dashed"),  # Lighten the grid lines if needed
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  )+
  coord_cartesian(clip = "off")
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(p );
  dev.off();

  return(invisible(NULL));
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
#'License: MIT
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

  # The formula interface keeps a model frame (a second copy of datmat) plus a terms
  # attribute whose environment is this call frame - so the fit holds on to datmat, cls
  # and mSetObj itself, and every later save of mSetObj drags the whole session through
  # it (12.6 MB -> 82.2 MB for a 191 x 6838 table). Nothing downstream reads $model or
  # $terms, so drop them before the fit is stored. PCA is unaffected because prcomp
  # takes the matrix directly and never builds a model frame.
  pls.res$model <- NULL;
  attr(pls.res$terms, ".Environment") <- baseenv();

  mSetObj$analSet$plsr <- pls.res;
  mSetObj$analSet$plsr$reg <- reg;
  mSetObj$analSet$plsr$loading.type <- "all";
  mSetObj$analSet$plsr$vip.mat <- .calculate.pls.vip(pls.res, comp.num);
  mSetObj$custom.cmpds <- c();

  fast.write.csv(signif(mSetObj$analSet$plsr$scores,5), row.names=rownames(mSetObj$dataSet$norm), file="plsda_score.csv");
  fast.write.csv(signif(mSetObj$analSet$plsr$loadings,5), file="plsda_loadings.csv");

  # Arrow export for zero-copy Java access (PLS VIP)
  ExportResultMatArrow(mSetObj$analSet$plsr$vip.mat, "pls_vip");

  return(.set.mSet(mSetObj));
}

#'Plot PLS pairwise summary
#'@description Plot PLS pairwise summary
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, indicate the number of principal components
#'@export

PlotPLSPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, pc.num){
  
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
#'License: MIT
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
#'@param cex.opt Character, controls label size adjustment: "na" resets to default (1.0), "increase" enlarges, or any other value shrinks. Default is "na".
#'@export
#'
PlotPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){

  mSetObj <- .get.mSet(mSetObj);
  sp <- .get.scatter.params(reg)
  reg <- sp$level
  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    pls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    pls.cex <<- pls.cex + 0.1;
  }else{
    pls.cex <<- pls.cex - 0.1;
  }

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  cls1 <- mSetObj$dataSet$cls
  cls.type <- mSetObj$dataSet$cls.type
  
  mSetObj$imgSet$pls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$plsr$scores[,inx1];
  lv2 <- mSetObj$analSet$plsr$scores[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$plsr$Xvar[inx1]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$plsr$Xvar[inx2]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long

  # The plot data is what DrawPLS2DScore reads: the scores, the groups, the confidence-region settings and the
  # session's group colours / shapes as they are now.
  pd <- list(lv1 = lv1, lv2 = lv2, xlabel = xlabel, ylabel = ylabel, text.lbls = text.lbls,
             cls1 = cls1, cls.type = cls.type, sp = sp, pls.cex = pls.cex,
             col.vec = get0("colVec"), shape.vec = get0("shapeVec"));
  DrawPLS2DScore(pd, img.base, format, dpi, width, show, grey.scale);
  .ov_plot_data(imgName, pd, "DrawPLS2DScore",
                list(imgName = img.base, format = format, dpi = dpi, width = width, show = show, grey.scale = grey.scale));

  return(.set.mSet(mSetObj));
}

# Draws the 2D PLS-DA score plot from its plot data alone
# (pd: lv1, lv2, xlabel, ylabel, text.lbls, cls1, cls.type, sp, pls.cex, col.vec, shape.vec).
DrawPLS2DScore <- function(pd, imgName, format="png", dpi=default.dpi, width=NA, show=1, grey.scale=0){
  lv1 <- pd$lv1; lv2 <- pd$lv2;
  xlabel <- pd$xlabel; ylabel <- pd$ylabel;
  text.lbls <- pd$text.lbls;
  cls1 <- pd$cls1; cls.type <- pd$cls.type;
  sp <- pd$sp;
  pls.cex <- pd$pls.cex;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # margin big
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,8));
  
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
    pts.array[,,i] <- .compute.group.ellipse(lv1[inx], lv2[inx], sp)
  }

  # A skipped ellipse is an all-NA slice; without na.rm the range is NA and
  # plot() then fails on non-finite xlim/ylim, losing the whole scores plot.
  xrg <- range(lv1, pts.array[,1,], na.rm = TRUE);
  yrg <- range(lv2, pts.array[,2,], na.rm = TRUE);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
 
  col.def <- GetColorSchema(cls, grey.scale==1, pd$col.vec);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot", asp=if(sp$use.asp) 1 else NA);
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
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale, pd$shape.vec, pd$col.vec);
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
      if(grey.scale == 1 | (!is.null(pd$shape.vec) && all(pd$shape.vec>=0))){
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
  return(invisible(NULL));
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
#'License: MIT
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
  mSetObj$imgSet$pls.score3d <- imgName;

  ov_qs_save(pls3d$score, "score3d.qs");

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
  ov_qs_save(pls3d$loading, "loading3d.qs");

  my.json.scatter(imgName, T);

  return(.set.mSet(mSetObj));

}

#'Update PLS loadings
#'@description Update the PLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
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
#'License: MIT
#'@export
#'
PlotPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2){
  
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

  # Arrow export for zero-copy Java access (PLS loadings)
  ExportResultMatArrow(loadings, "pls_load");

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
    lbl.inx <- seq_len(nrow(loadings));
    # With many features the labels overprint into an unreadable mass; above 100 keep only
    # the peripheral features (largest distance from the origin = highest contribution) so the
    # informative outliers stay legible. All points are still plotted, only labels are thinned.
    if(nrow(loadings) > 100){
      d <- sqrt(loadings[,1]^2 + loadings[,2]^2);
      lbl.inx <- utils::head(order(d, decreasing=TRUE), 100);
    }
    text(loadings[lbl.inx,1],loadings[lbl.inx,2], labels=substr(rownames(loadings)[lbl.inx], 1, 16), pos=4, col="blue", xpd=T);
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
#' License: MIT
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

  # Arrow export for zero-copy Java access (PLS-DA coefficients)
  if (!is.null(coef.mat) && nrow(coef.mat) > 0) {
    ExportResultMatArrow(coef.mat, "pls_coef");
  }

  return(.set.mSet(mSetObj));
}

######### biplot for PLSDA
########################
#'Plot PLS-DA biplot
#'@description Generate a PLS-DA biplot that overlays sample scores and top contributing
#'variables based on VIP values for selected components.
#'@usage PlotPLSBiplot(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, topnum=10)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Base name of the output image file
#'@param format Output image format, default is "png"
#'@param dpi Image resolution in dots per inch, default is default.dpi
#'@param width Image width in inches; use NA for automatic sizing
#'@param inx1 Index of the first component for x-axis
#'@param inx2 Index of the second component for y-axis
#'@param topnum Number of top variables to label in the biplot
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
PlotPLSBiplot <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2,topnum=10){

  mSetObj <- .get.mSet(mSetObj);
  plsr <- mSetObj$analSet$plsr
  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$pls.biplot <- imgName;
    
  choices <- c(inx1, inx2);
  scores <- plsr$scores;
  lam <- apply(scores[,choices], 2, sd)
  n <- NROW(scores)
  lam <- lam * sqrt(n);
   
  library(dplyr)
  # factoextra not needed here — removed dead import
 
  
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

  xlabel <- paste("Component", inx1, " (", round(100*plsr$Xvar[inx1]/plsr$Xtotvar,1), "%)", sep = "")
  ylabel <- paste("Component", inx2, " (", round(100*plsr$Xvar[inx2]/plsr$Xtotvar,1), "%)", sep = "")

  # The plot data is what DrawPLSBiplot reads: the sample scores, the labelled feature arrows, the confidence-region
  # settings and the session's group colours as they are now.
  pd <- list(ind_data = ind_data, var_data = var_data, cls = cls, xlabel = xlabel, ylabel = ylabel,
             sp = .get.scatter.params(), col.vec = get0("colVec"));
  DrawPLSBiplot(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawPLSBiplot", list(imgName = img.base, format = format, dpi = dpi, width = width));
   return(.set.mSet(mSetObj));
}

# Draws the PLS-DA biplot from its plot data alone (pd: ind_data, var_data, cls, xlabel, ylabel, sp, col.vec).
DrawPLSBiplot <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
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
  ind_data <- pd$ind_data; var_data <- pd$var_data;

  cols <- GetColorSchema(pd$cls, FALSE, pd$col.vec); 
  
  p <- ggplot() +
    geom_point(data = ind_data, aes(x = PC1, y = PC2, color = Group), size =3.5, alpha = 0.7) +
    stat_ellipse(data = .ellipse.layer.data(ind_data, "PC1", "PC2", "Group", pd$sp), aes(x = PC1, y = PC2, fill = Group, color = Group), type = pd$sp$type, level = pd$sp$level, geom = "polygon", alpha = 0.2) +
    geom_segment(data = var_data, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "black", arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.5, show.legend = FALSE) +
    geom_text_repel(data = var_data, aes(x = PC1, y = PC2, label = Variable),
                    size = 4, color = "#3B3B3B", max.overlaps = Inf, segment.color = "#636363") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    labs(title = "Biplot", x = pd$xlabel, y = pd$ylabel) +
    theme_minimal() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 11, color = "black"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 12), 
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Add a black border around the plot
      panel.grid.major = element_line(color = "grey80",linetype = "dashed"),  # Lighten the grid lines if needed
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  )+
    coord_fixed(ratio = 1, clip = "off")
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(p );
  dev.off();
  return(invisible(NULL));
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
#'License: MIT
#'@export
#'
PLSDA.Permut <- function(mSetObj=NA, num=100, type="accu"){

  mSetObj <- .get.mSet(mSetObj);

  if(is.null(mSetObj$analSet$plsda$best.num)){
    msg <- "Please perform cross-validation (PLSDA.CV) before running permutation testing.";
    if(.on.public.web){
      return(msg)
    }else{
      print(msg);
      return(.set.mSet(mSetObj));
    }
  }

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
  # perm.num may be adjusted on server
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
#'License: MIT
#'@export
#'
PlotPLS.Imp <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, type, feat.nm, feat.num, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  mSetObj$imgSet$pls.imp<-imgName;

  if(type=="vip"){
    mSetObj$analSet$plsda$imp.type <- "vip";
    vips <- mSetObj$analSet$plsr$vip.mat[,feat.nm];
    imp <- .ImpVarData(mSetObj, vips, "VIP scores", feat.num, color.BW);
  }else{
    mSetObj$analSet$plsda$imp.type <- "coef";
    data<-mSetObj$analSet$plsda$coef.mat[,feat.nm];
    imp <- .ImpVarData(mSetObj, data, "Coefficients", feat.num, color.BW);
  }

  # The plot data is what DrawPLS.Imp reads: the top features, their per-group means and the number of groups.
  pd <- list(imp = imp, cls.len = length(levels(mSetObj$dataSet$cls)));
  DrawPLS.Imp(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawPLS.Imp", list(imgName = img.base, format = format, dpi = dpi, width = width));
  
  return(.set.mSet(mSetObj));
}

# Draws the PLS-DA important-features plot from its plot data alone (pd: imp (see .ImpVarData), cls.len).
DrawPLS.Imp <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w-1; # margin is big
  
  # re-adjust width based on group size
  cls.len <- pd$cls.len;
  rt.mrg <- cls.len + 3;
  w <- w + rt.mrg/72; # convert to inch

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  op <- par(mar=c(5,7,3,rt.mrg)); # update right side margin with the number of class

  DrawImpVar(pd$imp);
  par(op);
  dev.off();
  return(invisible(NULL));
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
#'License: MIT
#'@export
#'
PlotImpVar <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE){
  DrawImpVar(.ImpVarData(mSetObj, imp.vec, xlbl, feat.num, color.BW));
}

# What the important-variable plot reads: the top features, their per-group means and the group labels.
.ImpVarData <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE){
  
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
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  list(imp.vec = imp.vec, vip.nms = vip.nms, mns = mns, cls.lbl = levels(cls),
       feat.num = feat.num, xlbl = xlbl, color.BW = color.BW);
}

# Draws the important-variable plot (dot chart plus per-group heat key) on the open device from the data of .ImpVarData.
DrawImpVar <- function(pd){
  imp.vec <- pd$imp.vec; vip.nms <- pd$vip.nms; mns <- pd$mns;
  feat.num <- pd$feat.num; xlbl <- pd$xlbl; color.BW <- pd$color.BW;
  
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
  
  cls.lbl <- pd$cls.lbl;
  
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
#'License: MIT
#'@export
#'
PlotPLS.Classification <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$plsda$fit.info;
  colnames(res) <- 1:ncol(res);
  best.num <- mSetObj$analSet$plsda$best.num;
  choice <- mSetObj$analSet$plsda$choice;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 8;
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
#'License: MIT
#'@export
#'
PlotPLS.Permutation <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
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
#'@param ortho.num Number of orthogonal components to fit; NA (the default) lets ropls choose it by cross-validation
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
OPLSR.Anal<-function(mSetObj=NA, reg=FALSE, ortho.num=NA){
  mSetObj <- .get.mSet(mSetObj);

  mSetObj$analSet$opls.reg <- reg;
  mSetObj$custom.cmpds <- c();
  mSetObj$analSet$oplsda$splot.type <- "all";

  if(reg==TRUE){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }

  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1);

  bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
  bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
  ov_qs_save(list(data=datmat, cls=cls, cv.num=cv.num, ortho.num=ortho.num), bridge_in, preset = "fast")
  on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

  run_func_via_microservice(
    func = function(wd, bridge_in, bridge_out) {
      setwd(wd)
      input <- ov_qs_read(bridge_in)
      rpath <- "../../"
      if(file.exists(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_opls.R"))){
        source(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_opls.R"), local = FALSE)
      }
      res <- perform_opls(input$data, input$cls, predI=1, permI=0, orthoI=input$ortho.num, crossvalI=input$cv.num)
      ov_qs_save(res, bridge_out, preset = "fast")
    },
    args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
    timeout_sec = 300
  )

  my.res <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL
  mSetObj$analSet$oplsda <- my.res;
  score.mat <- cbind(my.res$scoreMN[,1], my.res$orthoScoreMN[,1]);
  colnames(score.mat) <- c("Score (t1)","OrthoScore (to1)");
  fast.write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="oplsda_score.csv");
  load.mat <- cbind(my.res$loadingMN[,1], my.res$orthoLoadingMN[,1]);
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
#'@param cex.opt Character, controls label size adjustment: "na" resets to default (1.0), "increase" enlarges, or any other value shrinks. Default is "na".
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PlotOPLS2DScore<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){

  mSetObj <- .get.mSet(mSetObj);
  sp <- .get.scatter.params(reg)
  reg <- sp$level
  cls <- mSetObj$dataSet$cls;
  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    opls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    opls.cex <<- opls.cex + 0.1;
  }else{
    opls.cex <<- opls.cex - 0.1;
  }

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  mSetObj$imgSet$opls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$oplsda$scoreMN[,1];
  lv2 <- mSetObj$analSet$oplsda$orthoScoreMN[,1];
  xlabel <- paste("T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["p1", "R2X"],1), "%)");
  ylabel <- paste("Orthogonal T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["o1", "R2X"],1), "%)");
  
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long

  # The plot data is what DrawOPLS2DScore reads: the scores, the groups, the confidence-region settings and the
  # session's group colours / shapes as they are now.
  pd <- list(lv1 = lv1, lv2 = lv2, xlabel = xlabel, ylabel = ylabel, text.lbls = text.lbls,
             cls = cls, sp = sp, opls.cex = opls.cex,
             col.vec = get0("colVec"), shape.vec = get0("shapeVec"));
  DrawOPLS2DScore(pd, img.base, format, dpi, width, show, grey.scale);
  .ov_plot_data(imgName, pd, "DrawOPLS2DScore",
                list(imgName = img.base, format = format, dpi = dpi, width = width, show = show, grey.scale = grey.scale));
  return(.set.mSet(mSetObj));
}

# Draws the 2D OPLS-DA score plot from its plot data alone
# (pd: lv1, lv2, xlabel, ylabel, text.lbls, cls, sp, opls.cex, col.vec, shape.vec).
DrawOPLS2DScore<-function(pd, imgName, format="png", dpi=default.dpi, width=NA, show=1, grey.scale=0){
  lv1 <- pd$lv1; lv2 <- pd$lv2;
  xlabel <- pd$xlabel; ylabel <- pd$ylabel;
  text.lbls <- pd$text.lbls;
  cls <- pd$cls;
  sp <- pd$sp;
  opls.cex <- pd$opls.cex;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- width;
  }
  h <- w;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");

  par(mar=c(5,5,3,8));
  
  # obtain ellipse points to the scatter plot for each category
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- cls == lvs[i];
    pts.array[,,i] <- .compute.group.ellipse(lv1[inx], lv2[inx], sp)
  }
  
  # A skipped ellipse is an all-NA slice; without na.rm the range is NA and
  # plot() then fails on non-finite xlim/ylim, losing the whole scores plot.
  xrg <- range(lv1, pts.array[,1,], na.rm = TRUE);
  yrg <- range(lv2, pts.array[,2,], na.rm = TRUE);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);

  col.def <- GetColorSchema(cls, grey.scale==1, pd$col.vec);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot", asp=if(sp$use.asp) 1 else NA);
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
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale, pd$shape.vec, pd$col.vec);
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
      if(grey.scale == 1 | (!is.null(pd$shape.vec) && all(pd$shape.vec>=0))){
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
  return(invisible(NULL));
}

#'Update OPLS loadings
#'@description Update the OPLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
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
#'License: MIT
#'@export
#'
PlotOPLS.Splot <- function(mSetObj=NA, imgName, plotType="all", format="png", dpi=default.dpi, width=NA){
  
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

  # Arrow export for zero-copy Java access (OPLS S-plot)
  ExportResultMatArrow(splot.mat[,c(1,3)], "opls_splot");

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
                         dpi=default.dpi, width=NA, type="vip", 
                         feat.nm="tscore", 
                         feat.num=15, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  mSetObj$imgSet$opls.vip <- imgName;
  
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

  # The plot data is what DrawOPLS.Imp reads: the top features, their per-group means and the number of groups.
  pd <- list(imp = .ImpVarData(mSetObj, data, impNm, feat.num, color.BW),
             cls.len = length(levels(mSetObj$dataSet$cls)));
  DrawOPLS.Imp(pd, img.base, format, dpi, width);
  .ov_plot_data(imgName, pd, "DrawOPLS.Imp", list(imgName = img.base, format = format, dpi = dpi, width = width));
  
  fast.write.csv(vip.mat, file="oplsda_vip.csv");

  # Arrow export for zero-copy Java access (OPLS VIP)
  ExportResultMatArrow(vip.mat, "opls_vip");

  return(.set.mSet(mSetObj));
}

# Draws the OPLS-DA important-features plot from its plot data alone (pd: imp (see .ImpVarData), cls.len).
DrawOPLS.Imp <- function(pd, imgName, format="png", dpi=default.dpi, width=NA){
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1;
  
  cls.len <- pd$cls.len;
  rt.mrg <- cls.len + 3;
  w <- w + rt.mrg/72; # convert to inch

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");  
  op <- par(mar=c(5,7,3,rt.mrg)); # update right side margin with the number of class

  DrawImpVar(pd$imp);

  par(op);
  dev.off();
  return(invisible(NULL));
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
PlotOPLS.MDL <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
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
#'License: MIT
#'@export

OPLSDA.Permut<-function(mSetObj=NA, num=100){
  mSetObj <- .get.mSet(mSetObj);

  if(mSetObj$analSet$opls.reg){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }

  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1);

  bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
  bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
  ov_qs_save(list(data=datmat, cls=cls, perm.num=num, cv.num=cv.num), bridge_in, preset = "fast")
  on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

  run_func_via_microservice(
    func = function(wd, bridge_in, bridge_out) {
      setwd(wd)
      input <- ov_qs_read(bridge_in)
      rpath <- "../../"
      if(file.exists(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_opls.R"))){
        source(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_opls.R"), local = FALSE)
      }
      res <- perform_opls(input$data, input$cls, predI=1, permI=input$perm.num, orthoI=NA, crossvalI=input$cv.num)
      ov_qs_save(res, bridge_out, preset = "fast")
    },
    args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
    timeout_sec = 300
  )

  my.res <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL
  r.vec <- my.res$suppLs[["permMN"]][, "R2Y(cum)"];
  q.vec <- my.res$suppLs[["permMN"]][, "Q2(cum)"];
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
#'License: MIT
#'@export

PlotOPLS.Permutation<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
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
#'License: MIT
#'@export

SPLSR.Anal <- function(mSetObj=NA, comp.num, var.num, compVarOpt, validOpt="Mfold", foldNum=5, doCV=FALSE){
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
  cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  datmat <- as.matrix(mSetObj$dataSet$norm);

  bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
  bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
  ov_qs_save(list(data=datmat, cls=cls, comp.num=comp.num, comp.var.nums=comp.var.nums,
                  doCV=doCV, validOpt=validOpt, foldNum=foldNum), bridge_in, preset = "fast")
  on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

  run_func_via_microservice(
    func = function(wd, bridge_in, bridge_out) {
      setwd(wd)
      input <- ov_qs_read(bridge_in)
      rpath <- "../../"
      if(file.exists(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_spls.R"))){
        source(paste0(rpath, "rscripts/MetaboAnalystR/R/stats_spls.R"), local = FALSE)
      }
      res <- splsda(input$data, input$cls, ncomp=input$comp.num, keepX=input$comp.var.nums)
      if(input$doCV){
        perf.res <- perf.splsda(res, dist="centroids.dist", validation=input$validOpt, folds=input$foldNum)
        res$error.rate <- perf.res$error.rate$overall
      }
      ov_qs_save(res, bridge_out, preset = "fast")
    },
    args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
    timeout_sec = 300
  )

  my.res <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL
  mSetObj$analSet$splsr <- my.res;
  score.mat <- my.res$variates$X;
  fast.write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="splsda_score.csv");
  load.mat <- my.res$loadings$X;

  ord.inx <- order(-abs(load.mat[,1]), -abs(load.mat[,2]));
  load.mat <- signif(load.mat[ord.inx,],5);
  fast.write.csv(load.mat, file="splsda_loadings.csv");

  mSetObj$analSet$splsr$loadings$X <- load.mat;
  ExportResultMatArrow(load.mat, "spls_load");

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
#'License: MIT
#'@export
#'
PlotSPLSPairSummary<-function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, pc.num){
  
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
#'@param cex.opt Character, controls label size adjustment: "na" resets to default (1.0), "increase" enlarges, or any other value shrinks. Default is "na".
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PlotSPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, cex.opt="na"){

  mSetObj <- .get.mSet(mSetObj);
  sp <- .get.scatter.params(reg)
  reg <- sp$level
  # add option to adjust label size. Should be global to remember previous state
  if(cex.opt=="na"){                
    spls.cex <<- 1.0;
  }else if(cex.opt=="increase"){
    spls.cex <<- spls.cex + 0.1;
  }else{
    spls.cex <<- spls.cex - 0.1;
  }

  img.base <- imgName;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  mSetObj$imgSet$spls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$splsr$variates$X[,inx1];
  lv2 <- mSetObj$analSet$splsr$variates$X[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx1],1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx2],1), "%)");
  
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }

  # The plot data is what DrawSPLS2DScore reads: the scores, the groups, the confidence-region settings and the
  # session's group colours / shapes as they are now.
  pd <- list(lv1 = lv1, lv2 = lv2, xlabel = xlabel, ylabel = ylabel, text.lbls = text.lbls,
             cls = cls, sp = sp, spls.cex = spls.cex,
             col.vec = get0("colVec"), shape.vec = get0("shapeVec"));
  DrawSPLS2DScore(pd, img.base, format, dpi, width, show, grey.scale);
  .ov_plot_data(imgName, pd, "DrawSPLS2DScore",
                list(imgName = img.base, format = format, dpi = dpi, width = width, show = show, grey.scale = grey.scale));
  return(.set.mSet(mSetObj));
}

# Draws the 2D sPLS-DA score plot from its plot data alone
# (pd: lv1, lv2, xlabel, ylabel, text.lbls, cls, sp, spls.cex, col.vec, shape.vec).
DrawSPLS2DScore <- function(pd, imgName, format="png", dpi=default.dpi, width=NA, show=1, grey.scale=0){
  lv1 <- pd$lv1; lv2 <- pd$lv2;
  xlabel <- pd$xlabel; ylabel <- pd$ylabel;
  text.lbls <- pd$text.lbls;
  cls <- pd$cls;
  sp <- pd$sp;
  spls.cex <- pd$spls.cex;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w-1; # big margin
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,8));
  
  # obtain ellipse points to the scatter plot for each category
  
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- cls == lvs[i];
    pts.array[,,i] <- .compute.group.ellipse(lv1[inx], lv2[inx], sp)
  }
  
  # A skipped ellipse is an all-NA slice; without na.rm the range is NA and
  # plot() then fails on non-finite xlim/ylim, losing the whole scores plot.
  xrg <- range(lv1, pts.array[,1,], na.rm = TRUE);
  yrg <- range(lv2, pts.array[,2,], na.rm = TRUE);
  x.ext <- (xrg[2]-xrg[1])/12;
  y.ext <- (yrg[2]-yrg[1])/12;
  xlims <- c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims <- c(yrg[1]-y.ext, yrg[2]+y.ext);
  
  col.def <- GetColorSchema(cls, grey.scale==1, pd$col.vec);
  cols <- ExpandSchema(cls, col.def);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot", asp=if(sp$use.asp) 1 else NA);
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
  
  pchs.def <- GetShapeSchema(cls, show, grey.scale, pd$shape.vec, pd$col.vec);
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
      if(grey.scale == 1 | (!is.null(pd$shape.vec) && all(pd$shape.vec>=0))){
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
  return(invisible(NULL));
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
#'License: MIT
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

  ov_qs_save(spls3d$score, "score3d.qs");

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
  ov_qs_save(spls3d$loading, "loading3d.qs");

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
#'License: MIT
#'@export
#'
PlotSPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, inx, viewOpt="detail"){
  
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
#'License: MIT
#'@export

PlotSPLSDA.Classification <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
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

GetPCAStats <- function(mSetObj) {
  mSetObj <- .get.mSet(mSetObj)
  stat.info <- mSetObj$analSet$pca$permanova.res$stat.info

  return(stat.info)
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

GetPCAPermANOVAMetaText <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  res <- mSetObj$analSet$pca$permanova.meta
  if (is.null(res) || nrow(res) == 0) return("")
  esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x)))
  fmt <- function(x) vapply(x, function(v) if (is.na(v)) "NA" else format(signif(v, 4), scientific = FALSE), character(1))
  tab <- data.frame(Factor = esc(res$Factor), Type = res$Type,
                    Levels = ifelse(is.na(res$Levels), "-", as.character(res$Levels)),
                    F = fmt(res$F.value), R2 = fmt(res$R2),
                    p.value = fmt(res$p.value), p.adj.FDR = fmt(res$p.adj), stringsAsFactors = FALSE)
  print(xtable::xtable(tab), type = "html", print.results = FALSE, include.rownames = FALSE,
        sanitize.text.function = identity, html.table.attributes = "border=1 width=100%")
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
  return(unname(CleanNumber(signif(as.matrix(sig.mat),5))));
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
  # Java reads the row names separately (GetPLSLoadCmpds); shipping them again as
  # dimnames doubles the QAP1 payload for no gain, so drop them here.
  return(unname(res))
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
#'License: MIT
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
  return(unname(res))
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

  sp <- .get.scatter.params()

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

  # Panel function that respects asp=1 for uniform scaling
  my.panel <- if(sp$use.asp) {
    function(x, y, ...) {
      usr <- par("usr")
      rx <- diff(usr[1:2]); ry <- diff(usr[3:4])
      if(rx > 0 && ry > 0) {
        rng <- max(rx, ry)
        mx <- mean(usr[1:2]); my <- mean(usr[3:4])
        par(usr = c(mx - rng/2, mx + rng/2, my - rng/2, my + rng/2))
      }
      points(x, y, ...)
    }
  } else NULL

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(cls.type == "disc"){
    if(!is.null(my.panel)){
      pairs(mat, col=my.col, pch=my.pch, labels=lbls, oma=omaVal, panel=my.panel);
    } else {
      pairs(mat, col=my.col, pch=my.pch, labels=lbls, oma=omaVal);
    }
    par(xpd = TRUE)
    if(lgd.len > 4){
      legend("bottom", legend = legend.nm, pch=pchs.def, col=col.def, cex=0.9, bty = "n", ncol =lgd.len);
    }else{
      legend("bottom", legend = legend.nm, pch=pchs.def, col=col.def, bty = "n", ncol =lgd.len);
    }
  }else{
    if(!is.null(my.panel)){
      pairs(mat, labels=lbls, panel=my.panel);
    } else {
      pairs(mat, labels=lbls);
    }
  }
  dev.off();
}

ComputePERMANOVA <- function(pc1, pc2, cls, numPermutations = getOption("ov.permanova.nperm", 999L), cls.type = "disc", overall = NULL) {
  # Combine PC1 and PC2 scores into a matrix
  pc.mat <- cbind(pc1, pc2)

  # Calculate PERMANOVA significance
  res <- .calculateDistSig(pc.mat, cls, cls.type, permutations = numPermutations, overall = overall)
  
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

ComputePERMANOVAstat <- function(pc1, pc2, cls, cls.type, numPermutations = getOption("ov.permanova.nperm", 999L)) {
  # only the overall p-value is needed here (the pair-plot panels): no group-pair table
  signif(.permanova_euclid_p(cbind(pc1, pc2), cls, cls.type, numPermutations), 5)
}

ComputeMultiVarTest <- function(pc1, pc2, cls, numPermutations = 999) {
  pc.mat <- cbind(pc1, pc2)
  distM  <- dist(pc.mat)

  bridge_in <- paste0(tempdir(), "/bridge_", paste0(sample(letters,6,replace=TRUE), collapse=""), "_in.qs")
  bridge_out <- sub("_in.qs", "_out.qs", bridge_in)
  ov_qs_save(list(distM = distM, cls = cls, n_perm = numPermutations), bridge_in, preset = "fast")
  on.exit(unlink(c(bridge_in, bridge_out)), add = TRUE)

  run_func_via_microservice(
    func = function(wd, bridge_in, bridge_out) {
      setwd(wd)
      require(vegan)
      input <- ov_qs_read(bridge_in)
      dbrda.mod <- vegan::capscale(input$distM ~ input$cls)
      dbrda.an  <- vegan::anova.cca(dbrda.mod, permutations = input$n_perm)
      res <- signif(dbrda.an$`Pr(>F)`[1], 5)
      ov_qs_save(res, bridge_out, preset = "fast")
    },
    args = list(wd = getwd(), bridge_in = bridge_in, bridge_out = bridge_out),
    timeout_sec = 300
  )

  result <- if (file.exists(bridge_out)) ov_qs_read(bridge_out) else NULL
  if (is.null(result)) { AddErrMsg("Multivariate test failed"); return(0) }
  return(result)
}
