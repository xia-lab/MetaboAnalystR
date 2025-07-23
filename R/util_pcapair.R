.plotPCAPairSummaryMeta <- function(mSetObj = NA,
                                    imgName,
                                    format = "png",
                                    dpi    = default.dpi,
                                    width  = NA,
                                    pc.num,
                                    meta,
                                    metaShape = NULL) {   # ← default = NULL
  library(ggplot2)
  library(GGally)
  library(grid)
  # save.image("pcapair.RData");
  ## ── bookkeeping ----------------------------------------------------
  mSetObj  <- .get.mSet(mSetObj)
  pclabels <- paste0("PC", 1:pc.num, " (",
                     round(100 * mSetObj$analSet$pca$variance[1:pc.num], 1),
                     "%)")
  imgName  <- paste0(imgName, "dpi", dpi, ".", format)
  
w <- ifelse(is.na(width) || width == 0, 10, width)
h       <- w 

  mSetObj$imgSet$pca.pair <- imgName
  
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")
  
  ## ── data -----------------------------------------------------------
  data      <- as.data.frame(mSetObj$analSet$pca$x[, 1:pc.num])
  if (is.null(mSetObj$dataSet$meta.info)) {
    mSetObj$dataSet$meta.info <- data.frame(Class = mSetObj$dataSet$cls)
    mSetObj$dataSet$meta.types <- list(Class = "disc")
  }
  
  ## Match sample order --------------------------------------------------
  meta.info <- mSetObj$dataSet$meta.info[
    match(rownames(data), rownames(mSetObj$dataSet$meta.info)), ]
  
  ## Extract the selected metadata variable ------------------------------
  if (meta %in% colnames(meta.info)) {
    inx       <- which(colnames(meta.info) == meta)
    cls       <- meta.info[[inx]]
    cls.type  <- mSetObj$dataSet$meta.types[[inx]]
  } else {
    cls       <- mSetObj$dataSet$cls
    cls.type  <- "disc"
  }
  
  ## shape variable (may be NULL) --------------------------------------
  shape.ok <- !is.null(metaShape) &&
    metaShape %in% colnames(meta.info)
  
  if (shape.ok) {
    cls2      <- meta.info[[metaShape]]
    cls.type2 <- mSetObj$dataSet$meta.types[[metaShape]]
  } else {
    cls2      <- NULL
    cls.type2 <- "none"            # marker meaning “no shape mapping”
  }
  
  ## PERMANOVA p-values for all PC pairs ------------------------------
  pc.mat   <- as.matrix(data)
  pval.mat <- matrix(NA_real_, pc.num, pc.num)
  for (i in 1:(pc.num - 1))
    for (j in (i + 1):pc.num)
      pval.mat[i, j] <- ComputePERMANOVAstat(pc.mat[, i], pc.mat[, j],
                                             cls, cls.type)
  
  ## helper for upper-triangle density + p-value -----------------------
  upper_density_with_p <- function(p.mat) {
    function(data, mapping, ...) {
      var_x <- sub("^~", "", deparse(mapping$x))
      var_y <- sub("^~", "", deparse(mapping$y))
      i <- match(var_y, colnames(data)); j <- match(var_x, colnames(data))
      ptxt <- if (!is.na(p.mat[i, j])) sprintf("p = %.3g", p.mat[i, j]) else ""
      xmid <- mean(range(data[[var_x]], na.rm = TRUE))
      ymid <- mean(range(data[[var_y]], na.rm = TRUE))
      GGally::ggally_density(data, mapping, alpha = 0.45, colour = NA) +
        annotate("text", x = xmid, y = ymid, label = ptxt,
                 size = 4.5, fontface = "bold") +
        theme_bw()
    }
  }
  
  ## ── plotting -------------------------------------------------------
  if (cls.type == "disc") {
    
    ## ensure cls is a factor and colours align with its levels ----------
    if (!is.factor(cls)) cls <- factor(cls)
    uniq.cols <- GetColorSchema(cls)          # one colour per level
    
    if (cls.type2 == "disc") {                        # colour + shape -----
      pch.vec   <- as.numeric(factor(cls2))
      uniq.pchs <- sort(unique(pch.vec))
      
      ## main ggpairs (hide legends inside) ------------------------------
      p <- ggpairs(
        data,
        mapping = aes(colour = cls, shape = factor(cls2)),
        lower   = list(continuous = wrap("points")),
        upper   = list(continuous = upper_density_with_p(pval.mat)),
        diag    = list(continuous = wrap("densityDiag",
                                         alpha = 0.5, colour = NA)),
        columnLabels = pclabels
      ) +
        scale_colour_manual(values = uniq.cols, name = meta) +
        scale_shape_manual(values = uniq.pchs,  name = metaShape) +
        guides(
          colour = guide_legend(order = 1, nrow = 1, title.position = "left"),
          shape  = guide_legend(order = 2, nrow = 1, title.position = "left")
        ) +
        theme(legend.position = "none")
      
      ## dummy plot to grab combined legend ------------------------------
      auxplot <- ggplot(
        data.frame(cls = cls, cls2 = factor(cls2)),
        aes(x = cls, y = cls2, colour = cls, shape = cls2)
      ) +
        geom_point(size = 6) +
        scale_colour_manual(values = uniq.cols, name = meta) +
        scale_shape_manual(values = uniq.pchs,  name = metaShape) +
        guides(
          colour = guide_legend(order = 1, nrow = 1, title.position = "left"),
          shape  = guide_legend(order = 2, nrow = 1, title.position = "left")
        ) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.box      = "vertical",     # stack rows
              legend.text     = element_text(size = 11))
      
    } else {                                         # colour only --------
      ## ggpairs with colour only
      p <- ggpairs(
        data,
        mapping = aes(colour = cls),
        lower   = list(continuous = wrap("points")),
        upper   = list(continuous = upper_density_with_p(pval.mat)),
        diag    = list(continuous = wrap("densityDiag",
                                         alpha = 0.5, colour = NA)),
        columnLabels = pclabels
      ) +
        scale_colour_manual(values = uniq.cols, name = meta) +
        guides(colour = guide_legend(nrow = 1, title.position = "left")) +
        theme(legend.position = "none")
      
      ## auxplot for colour legend only
      auxplot <- ggplot(
        data.frame(cls = cls),
        aes(x = 1, y = 1, colour = cls)
      ) +
        geom_point(size = 6) +
        scale_colour_manual(values = uniq.cols, name = meta) +
        guides(colour = guide_legend(nrow = 1, title.position = "left")) +
        theme_void() +
        theme(legend.position = "bottom")
    }
    
    ## apply bottom margin, extract legend -------------------------------
    p <- p + theme_bw() +
      theme(plot.margin = unit(c(.25, .25, 2, .25), "in"))
    
    mylegend <- grab_legend(auxplot)
  }else {  ## ---- continuous colour (rank‑based, legend: bar only) -----
    
    ## rank‑transform & palette
    cls.num  <- as.numeric(as.character(cls))
    pct.rank <- (rank(cls.num, ties.method = "average", na.last = "keep") - 1) /
      (sum(!is.na(cls.num)) - 1)
    blues20  <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(20)
    
    ## ------------------------------------------------------------------
    if (cls.type2 == "disc") {                       # colour + shape
      pch.vec   <- as.numeric(factor(cls2))
      uniq.pchs <- sort(unique(pch.vec))
      
      p <- ggpairs(
        data,
        mapping = aes(colour = pct.rank, shape = factor(cls2)),
        lower   = list(continuous = wrap("points")),
        upper   = list(continuous = upper_density_with_p(pval.mat)),
        diag    = list(continuous = wrap("densityDiag",
                                         fill = "#003366", colour = NA,
                                         show.legend = FALSE)),
        columnLabels = pclabels
      ) +
        scale_colour_gradientn(colours = blues20, name = meta) +
        scale_shape_manual(values = uniq.pchs, name = metaShape) +
        guides(
          colour = guide_colourbar(order = 1,
                                   direction      = "horizontal",
                                   barwidth       = unit(3, "in"),
                                   barheight      = unit(0.25, "in"),
                                   ticks          = FALSE,   # ⬅ no tick marks
                                   label          = FALSE,   # ⬅ no tick labels
                                   title.position = "left",
                                   title.hjust    = 0),
          shape  = guide_legend(order = 2, nrow = 1,
                                direction      = "horizontal",
                                title.position = "left",
                                title.hjust    = 0)
        ) +
        theme(legend.position = "none")
      
      ## legend extraction ------------------------------------------------
      auxplot <- ggplot(
        data.frame(rank = pct.rank, shp = factor(cls2)),
        aes(x = rank, y = 1, colour = rank, shape = shp)
      ) +
        geom_point(size = 6) +
        scale_colour_gradientn(colours = blues20, name = meta) +
        scale_shape_manual(values = uniq.pchs, name = metaShape) +
        guides(
          colour = guide_colourbar(order = 1,
                                   direction      = "horizontal",
                                   barwidth       = unit(3, "in"),
                                   barheight      = unit(0.25, "in"),
                                   ticks          = FALSE,
                                   label          = FALSE,
                                   title.position = "left",
                                   title.hjust    = 0),
          shape  = guide_legend(order = 2, nrow = 1,
                                direction      = "horizontal",
                                title.position = "left",
                                title.hjust    = 0,
                                override.aes   = list(size = 6))
        ) +
        theme_void() +
        theme(legend.position = "bottom",
              legend.box      = "vertical",
              legend.text     = element_text(size = 11))
      
    } else {                                         # colour only
      
      p <- ggpairs(
        data,
        mapping = aes(colour = pct.rank),
        lower   = list(continuous = wrap("points")),
        upper   = list(continuous = upper_density_with_p(pval.mat)),
        diag    = list(continuous = wrap("densityDiag",
                                         fill = "#003366", colour = NA,
                                         show.legend = FALSE)),
        columnLabels = pclabels
      ) +
        scale_colour_gradientn(colours = blues20, name = meta) +
        guides(colour = guide_colourbar(
          direction      = "horizontal",
          barwidth       = unit(3, "in"),
          barheight      = unit(0.25, "in"),
          ticks          = FALSE,
          label          = FALSE,
          title.position = "left",
          title.hjust    = 0)) +
        theme(legend.position = "none")
      
      auxplot <- ggplot(
        data.frame(rank = pct.rank),
        aes(x = rank, y = 1, colour = rank)
      ) +
        geom_point(size = 6) +
        scale_colour_gradientn(colours = blues20, name = meta) +
        guides(colour = guide_colourbar(
          direction      = "horizontal",
          barwidth       = unit(3, "in"),
          barheight      = unit(0.25, "in"),
          ticks          = FALSE,
          label          = FALSE,
          title.position = "left",
          title.hjust    = 0)) +
        theme_void() +
        theme(legend.position = "bottom")
    }
    
    ## styling & legend extraction --------------------------------------
    p <- p + theme_bw() +
      theme(plot.margin     = unit(c(.25, .25, 2.3, .25), "in"),
            legend.position = "none")
    
    mylegend <- grab_legend(auxplot)
  }
  
  ## draw & close (unchanged) -------------------------------------------
  grid.newpage(); grid.draw(p)
  if (!is.null(mylegend)) {
    pushViewport(viewport(x = 0.5, y = 0, width = 0.9, height = 0.32,
                          just = c("center", "bottom")))
    grid.draw(mylegend); upViewport()
  }
  dev.off()
  
  invisible(.set.mSet(mSetObj))
}


make_diag_panel <- function(label) {        # returns a function for ggpairs
  function(data, mapping, ...) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = label,
               size = 3.5, fontface = "bold") +
      theme_void()
  }
}
