my.build.cemi.net <- function(dataName,
                              filter      = TRUE,
                              varCutOff   = 0,
                              min_ngen    = 30,
                              cor_method  = c("pearson", "spearman"),
                              imgName     = "cem_dendro",
                              dpi         = 72,
                              format      = "png",
                              verbose     = TRUE) {

  save.image("cemitool.RData")                  # snapshot

  ## 1 · load dataset -------------------------------------------------
  dataSet  <- readDataset(dataName)
  expr_mat <- as.data.frame(dataSet$data.norm)              # genes × samples
  annot_df <- data.frame(SampleName = rownames(dataSet$meta.info),
                         Class      = dataSet$meta.info[,1],
                         stringsAsFactors = FALSE)

  ## 2 · packages -----------------------------------------------------
  suppressPackageStartupMessages({
    library(CEMiTool)
    library(WGCNA)
    library(dendextend)
    library(Cairo)
    library(jsonlite)
  })

  ## 3 · run CEMiTool -------------------------------------------------
  cem <- cemitool(expr       = expr_mat,
                  annot      = annot_df,
                  filter     = filter,
                  min_ngen   = min_ngen,
                  cor_method = match.arg(cor_method),
                  plot       = TRUE,
                  verbose    = verbose)

  ## 4 · expression + modules ----------------------------------------
  expr_raw <- attr(cem, "expression")
  mod_tbl  <- attr(cem, "module")

  common_genes <- intersect(rownames(expr_raw), mod_tbl$genes)
  expr_raw     <- expr_raw[common_genes, , drop = FALSE]
  gene2mod     <- mod_tbl$modules[match(common_genes, mod_tbl$genes)]
  names(gene2mod) <- common_genes

  ## 5 · eigengenes ---------------------------------------------------
  MEs <- moduleEigengenes(t(expr_raw), colors = gene2mod,
                          impute = TRUE)$eigengenes        # samples × modules

## --------------------------------------------------------------
## 6 · Pick WHAT to cluster
## --------------------------------------------------------------

wantSampleTree <- TRUE   # <--  set to FALSE to go back to module tree

if (wantSampleTree) {
  ## ---- sample-wise dendrogram ---------------------------------
  #   • expr_raw : genes × samples
  #   • distance : 1 − sample-sample Pearson r
  dist_mat <- as.dist(1 - cor(expr_raw, method = "pearson"))
  hc       <- hclust(dist_mat, method = "average")

  # colours in the bar = sample classes
  leafIDs  <- hc$labels                       # sample IDs
  classes  <- dataSet$meta.info[leafIDs, 1]   # group factor
  palette  <- setNames(rainbow(length(unique(classes))), unique(classes))
  colorVec <- palette[classes]

  # PNG
  img_file <- paste0(imgName, "_SAMPLE_dendro_dpi", dpi, ".", format)
  Cairo(img_file, width = 1000, height = 600, dpi = dpi, bg = "white", type = format)
  plotDendroWithColors(hc,
                       leafColors = colorVec,
                       groupLabel = "Sample class",
                       showLabels = FALSE)
  dev.off()

} else {
  ## ---- module-wise dendrogram (original) ----------------------
  dist_mat <- as.dist(1 - cor(MEs, method = "pearson"))
  hc       <- hclust(dist_mat, method = "average")

  leafIDs  <- hc$labels                       # module IDs
  palette  <- setNames(rainbow(length(leafIDs)), leafIDs)
  img_file <- paste0(imgName, "_MODULE_dendro_dpi", dpi, ".", format)
  Cairo(img_file, width = 1000, height = 600, dpi = dpi, bg = "white", type = format)
  plotDendroWithColors(hc,
                       leafColors = leafIDs,
                       palette    = palette,
                       groupLabel = "CEMi modules",
                       showLabels = FALSE)
  dev.off()
}

  ## 7 · JSON summary -------------------------------------------------
  summaryJSON <- list(module       = colnames(MEs),
                      eigengeneVar = apply(MEs, 2, var))
  write_json(summaryJSON, paste0(imgName, ".json"), pretty = TRUE)

  invisible(list(cem  = cem,
                 MEs  = MEs,
                 dend = hc,
                 img  = img_file,
                 json = paste0(imgName, ".json")))
}


plotDendroWithColors <- function(hc,
                                 leafColors,
                                 palette     = NULL,
                                 groupLabel  = "Colours",
                                 showLabels  = FALSE) {
  
  # Convert to hclust if a dendrogram
  if (inherits(hc, "dendrogram"))
    hc <- as.hclust(hc)
  
  leaves <- hc$labels
  nLeaf  <- length(leaves)
  
  ## --- build colour vector ---------------------------------------
  if (length(leafColors) == 1L && is.numeric(leafColors)) {
    # treat as k = number of clusters
    k          <- leafColors
    clusters   <- cutree(hc, k = k)
    leafColors <- clusters
    palette    <- setNames(rainbow(k), sort(unique(clusters)))
  }
  
  if (!is.null(names(leafColors))) {
    # named vector – reorder to match leaf order
    if (!all(leaves %in% names(leafColors))) {
      stop("Missing colours for: ",
           paste(setdiff(leaves, names(leafColors)), collapse = ", "))
    }
    col_vec <- leafColors[leaves]
  } else {
    if (length(leafColors) != nLeaf)
      stop("Colour vector length (", length(leafColors),
           ") ≠ number of leaves (", nLeaf, ").")
    col_vec <- leafColors
  }
  
  # Translate IDs via palette if necessary
  if (!is.null(palette) && !all(col_vec %in% grDevices::colours())) {
    if (!all(col_vec %in% names(palette)))
      stop("Palette lacks entries for: ",
           paste(setdiff(col_vec, names(palette)), collapse = ", "))
    col_vec <- palette[col_vec]
  }
  
  ## --- final sanity check ----------------------------------------
  if (length(col_vec) != nLeaf)
    stop("Internal error: colour vector still not length ", nLeaf)
  
  ## --- plot -------------------------------------------------------
  suppressPackageStartupMessages(library(WGCNA))
  plotDendroAndColors(
    dendro       = hc,
    colors       = matrix(col_vec, ncol = 1, dimnames = list(labels(hc), NULL)),
    groupLabels  = "CEMi modules",
    dendroLabels = names(col_vec),
    addGuide     = TRUE,
    guideHang    = 0.05,
    main         = "Module-eigengene dendrogram",
    ylab         = "1 − Pearson correlation"
  )
}