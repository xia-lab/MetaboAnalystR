my.build.cemi.net <- function(dataName,
                              filter      = TRUE,
                              min_ngen    = 30,
                              cor_method  = "pearson",
                              verbose     = FALSE,
                              classCol    = NULL) {   # <-- optional argument

  ## 1 · load dataset -------------------------------------------------
  dataSet  <- readDataset(dataName)
  expr_mat <- as.data.frame(dataSet$data.norm)    # genes × samples

  ## metadata: keep *all* columns, coerce factors -> character
  meta_df <- dataSet$meta.info
  meta_df[] <- lapply(meta_df, \(x) if (is.factor(x)) as.character(x) else x)

  ## decide which column is the class
  if (is.null(classCol)) {
    classCol <- colnames(meta_df)[1]              # first column by default
  }
  if (!classCol %in% colnames(meta_df))
    stop("classCol '", classCol, "' not found in meta.info")

  ## build annotation table (SampleName + all meta)
  annot_df <- data.frame(SampleName = rownames(meta_df),
                         meta_df,
                         check.names = FALSE,
                         stringsAsFactors = FALSE)

  ## 2 · run CEMiTool -------------------------------------------------
  suppressPackageStartupMessages({library(CEMiTool); library(WGCNA)})

  cem <- cemitool(expr         = expr_mat,
                  annot        = annot_df,
                  filter       = filter,
                  min_ngen     = min_ngen,
                  cor_method   = match.arg(cor_method),
                  class_column = classCol,      # <-- tell CEMiTool!
                  verbose      = verbose,
                  plot         = TRUE)

  ## 3 · save & return -----------------------------------------------
  qs::qsave(cem, "cem.qs")
  return(1)
}


PlotCEMiDendro <- function(mode      = c("sample", "module"),
                           metaClass = "NA",
                           imgName   = "cem_dendro",
                           dpi       = 72,
                           format    = "png") {

  library(Cairo); library(WGCNA)

  cem <- qs::qread("cem.qs")
  if (!inherits(cem, "CEMiTool"))
    stop("'cem.qs' does not contain a valid CEMiTool object.")

  mode <- match.arg(mode)
  expr <- attr(cem, "expression")       # genes × samples
  mod  <- attr(cem, "module")

  ## helper ----------------------------------------------------------
  plotDendroColoured <- function(hc, colNamed, label, file, legendPal) {

    leaves <- labels(as.dendrogram(hc))
    if (!all(leaves %in% names(colNamed)))
      stop("Colour vector missing leaves:\n  ",
           paste(setdiff(leaves, names(colNamed)), collapse = ", "))

    colMat <- matrix(colNamed[leaves], nrow = length(leaves), ncol = 1,
                     dimnames = list(leaves, NULL))

    if (dpi == 72) dpi <- 96
    Cairo(file, width = 1000, height = 600, dpi = dpi,
          bg = "white", type = format)

    oldMar <- par("mar"); par(mar = oldMar + c(0, 0, 0, 4))
    plotDendroAndColors(
      dendro       = hc,
      colors       = colMat,
      groupLabels  = label,
      dendroLabels = FALSE,
      addGuide     = TRUE,
      guideHang    = 0.05,
      main         = paste("Module dendrogram"),
      ylab         = "1 − Pearson correlation")

    ## legend
    par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA)
    plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend("topright",
           legend = names(legendPal),
           fill   = legendPal,
           border = NA,
           cex    = 0.8,
           bty    = "n")

    par(oldMar); dev.off()
  }

  # ── MODULE dendrogram ────────────────────────────────────────────
  if (mode == "module") {
    ME  <- moduleEigengenes(t(expr[mod$genes, ]),
                            colors = mod$modules)$eigengenes
    hc  <- hclust(as.dist(1 - cor(ME)), method = "average")

    ids   <- colnames(ME)
    pal   <- setNames(rainbow(length(ids)), ids)
    idVec <- setNames(ids, ids)

    file <- sprintf("%s_module_dendro_dpi%d.%s", imgName, dpi, format)
    plotDendroColoured(hc, idVec, "modules", file, pal)
    return(1)
  }

  # ── SAMPLE dendrogram ────────────────────────────────────────────
  sa <- cem@sample_annotation

  ## choose metadata column sensibly
  if (is.na(metaClass) || metaClass == "NA") metaClass <- 2  # 1 is SampleName
  if (is.numeric(metaClass)) {
    if (metaClass < 1 || metaClass > ncol(sa))
      stop("'metaClass' index out of range.")
    if (metaClass == 1)
      stop("metaClass 1 is 'SampleName'; choose a metadata column (>=2).")
    classes <- sa[[metaClass]]
  } else {
    if (!metaClass %in% colnames(sa))
      stop("metaClass '", metaClass, "' not found in sample_annotation.")
    if (metaClass == "SampleName")
      stop("metaClass 'SampleName' is invalid; choose real metadata.")
    classes <- sa[[metaClass]]
  }

  names(classes) <- sa$SampleName

  hc  <- hclust(as.dist(1 - cor(expr)), method = "average")
  pal <- setNames(rainbow(length(unique(classes))), unique(classes))
  colNamed <- setNames(pal[classes], names(classes))

  file <- sprintf("%sdpi%d.%s", imgName, dpi, format)
  plotDendroColoured(hc, colNamed, "sample class", file, pal)
  return(1)
}

# =======================================================================
# PlotCEMiTreatmentHeatmap
# -----------------------------------------------------------------------
# factorName : name (character) or index (numeric) of the categorical
#              column in cem@sample_annotation to expand into dummies
# imgName    : file stem for the output
# dpi        : resolution
# format     : "png" | "pdf"
# -----------------------------------------------------------------------
# returns 1 on success, 0 on failure
# =======================================================================
PlotCEMiTreatmentHeatmap <- function(factorName,
                                     imgName = "cem_treatment_heatmap",
                                     dpi     = 96,
                                     format  = c("png", "pdf")) {

  tryCatch({

    library(CEMiTool); library(WGCNA); library(Cairo)

    cem <- qs::qread("cem.qs")
    stopifnot(inherits(cem, "CEMiTool"))

    sa <- cem@sample_annotation
    ## ── 1 · validate factorName  ----------------------------------
    if (is.numeric(factorName)) {
      stopifnot(factorName >= 2, factorName <= ncol(sa))
      fac      <- sa[[factorName]]
      colLabel <- colnames(sa)[factorName]
    } else {
      stopifnot(factorName %in% colnames(sa), factorName != "SampleName")
      fac      <- sa[[factorName]]
      colLabel <- factorName
    }
    fac <- as.factor(fac)

    ## ── 2 · dummy matrix  -----------------------------------------
    mm <- model.matrix(~ 0 + fac)
    colnames(mm) <- levels(fac)
    rownames(mm) <- sa$SampleName

    ## ── 3 · module eigengenes  ------------------------------------
    expr   <- attr(cem, "expression")
    modTbl <- attr(cem, "module")
    g      <- intersect(rownames(expr), modTbl$genes)
    colors <- modTbl$modules[match(g, modTbl$genes)]
    MEs    <- moduleEigengenes(t(expr[g, , drop = FALSE]), colors)$eigengenes

    mm <- mm[rownames(MEs), , drop = FALSE]

    ## ── 4 · correlations  -----------------------------------------
    corMat <- cor(MEs, mm, use = "p")
    pMat   <- corPvalueStudent(corMat, nSamples = nrow(MEs))
    textMat <- paste0(formatC(corMat, 2), "\n(",
                      formatC(pMat , 1, format = "e"), ")")

    ## ── 5 · device  (min 8 × 6 in)  -------------------------------
    colfun <- colorRampPalette(c("royalblue4", "white", "tomato"))

    outFile <- sprintf("%sdpi%d.%s", imgName, dpi, match.arg(format))
    if(dpi == 72){
        dpi <- 96;
    }
    width_px  <- max(50 + 40 * ncol(corMat), 8 * dpi)
    height_px <- max(50 + 20 * nrow(corMat), 6 * dpi)
    width_in  <- width_px  / dpi
    height_in <- height_px / dpi


    if (tolower(format) == "png") {
      Cairo(file   = outFile,
            width  = width_px,
            height = height_px,
            dpi    = dpi,
            bg     = "white",
            type   = "png")
    } else {
      Cairo(file   = outFile,
            width  = width_in,
            height = height_in,
            bg     = "white",
            type   = "pdf")
    }

    ## optional: tighten default margins a little
    oldMar <- par("mar"); on.exit(par(oldMar), add = TRUE)
    par(mar = c(5, 9, 4, 2) + 0.1)

    labeledHeatmap(Matrix          = corMat,
                   xLabels         = colnames(mm),
                   yLabels         = rownames(corMat),
                   ySymbols        = rownames(corMat),
                   colorLabels     = FALSE,
                   colors          = colfun(50),
                   textMatrix      = textMat,
                   setStdMargins   = FALSE,
                   cex.text        = 0.7,
                   zlim            = c(-1, 1),
                   main            = paste("Module ×", colLabel, "Levels"))

    dev.off()
    message("Heat-map written to: ", outFile)
    1

  }, error = function(e) {
    message("PlotCEMiTreatmentHeatmap: ", e$message)
    0
  })
}


