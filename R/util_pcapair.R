
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
  save.image("pcapair.RData");
  ## ── bookkeeping ----------------------------------------------------
  mSetObj  <- .get.mSet(mSetObj)
  pclabels <- paste0("PC", 1:pc.num, " (",
                     round(100 * mSetObj$analSet$pca$variance[1:pc.num], 1),
                     "%)")
  imgName  <- paste0(imgName, "dpi", dpi, ".", format)
  w        <- ifelse(is.na(width) || width == 0, 10, width)
  h        <- w - 1
  mSetObj$imgSet$pca.pair <- imgName

  if (format == "pdf")
    pdf(imgName, width = w, height = h, onefile = FALSE)
  else
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

    uniq.cols <- unique(GetColorSchema(cls))

    if (cls.type2 == "disc") {                       # ---- shape *is* discrete
      pch.vec   <- as.numeric(factor(cls2))
      uniq.pchs <- sort(unique(pch.vec))

      p <- ggpairs(
        data,
        lower = list(continuous = wrap("points", shape = pch.vec)),
        upper = list(continuous = upper_density_with_p(pval.mat)),
        diag  = list(continuous = wrap("densityDiag", alpha = 0.5, colour = NA)),
        columnLabels = pclabels,
        mapping = aes(color = cls)
      )

      auxplot <- ggplot(data.frame(cls = cls, cls2 = factor(cls2)),
                        aes(x = cls, y = cls2, color = cls, shape = cls2)) +
        theme_bw() + geom_point(size = 6) +
        theme(legend.position = "bottom",
              legend.title    = element_blank(),
              legend.text     = element_text(size = 11)) +
        scale_color_manual(values = uniq.cols) +
        scale_shape_manual(values = uniq.pchs) +
        guides(col = guide_legend(nrow = 2))

    } else {                                         # ---- no shape variable
      p <- ggpairs(
        data,
        lower = list(continuous = wrap("points")),
        upper = list(continuous = upper_density_with_p(pval.mat)),
        diag  = list(continuous = wrap("densityDiag", alpha = 0.5, colour = NA)),
        columnLabels = pclabels,
        mapping = aes(color = cls)
      )

auxplot <- ggplot(data.frame(cls = cls), aes(x = 1, fill = cls)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = uniq.cols) +
  labs(color = NULL, fill = NULL) + 
  theme_void() + theme(legend.position = "bottom")
    }

    p <- p + theme_bw() +
      scale_color_manual(values = uniq.cols) +
      scale_fill_manual(values = uniq.cols) +
      theme(plot.margin = unit(c(.25, .25, .6, .25), "in"))

    mylegend <- grab_legend(auxplot)

  } else {  ## ---- continuous colour ----------------------------------

    blues   <- rev(colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(20))
    num.val <- as.numeric(as.character(cls))
    cols    <- blues[cut(num.val, breaks = 20)]

    if (cls.type2 == "disc") {                       # shape *is* discrete
      pch.vec   <- as.numeric(factor(cls2))
      uniq.pchs <- sort(unique(pch.vec))

      p <- ggpairs(
        data,
        lower = list(continuous = wrap("points", shape = pch.vec, color = cols)),
        upper = list(continuous = upper_density_with_p(pval.mat)),
        diag  = list(continuous = wrap("densityDiag", fill = "#003366", colour = NA)),
        columnLabels = pclabels
      )

      auxplot <- ggplot(data.frame(cls = num.val, cls2 = factor(cls2)),
                        aes(x = cls, y = cls2, shape = cls2)) +
        theme_bw() + geom_point(size = 6) +
        theme(legend.position = "bottom",
              legend.title    = element_blank(),
              legend.text     = element_text(size = 11)) +
        scale_shape_manual(values = uniq.pchs)

    } else {                                        # no shape variable
      p <- ggpairs(
        data,
        lower = list(continuous = wrap("points", color = cols)),
        upper = list(continuous = upper_density_with_p(pval.mat)),
        diag  = list(continuous = wrap("densityDiag", fill = "#003366", colour = NA)),
        columnLabels = pclabels
      )

      auxplot <- ggplot() + theme_void()            # empty placeholder
    }

    p <- p + theme_bw() +
      theme(plot.margin = unit(c(.25, .25, .8, .25), "in"))
    mylegend <- if (cls.type2 == "disc") grab_legend(auxplot) else NULL
  }

  ## ── draw & close ----------------------------------------------------
  grid.newpage();  grid.draw(p)
  if (!is.null(mylegend)) {
    pushViewport(viewport(x = 5, y = 0.3, width = .35, height = .3,
                          default.units = "in"))
    grid.draw(mylegend);  upViewport()
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
