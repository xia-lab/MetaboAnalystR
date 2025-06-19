BuildFastCoexpNet <- function(dataName,
                              power = NULL,
                              cor_func = c("pearson", "bicor", "spearman"),
                              network_type = c("signed", "signed_hybrid", "unsigned"),
                              deepSplit = 2,
                              minModuleSize = 30,
                              mergeCutHeight = 0.25,
                              maxBlockSize = 5000,
                              n_threads = 8,
                              auto_power = TRUE,
                              enrichFDR = NULL,   # skip enrichment by default for speed
                              imgName = "coexp_dendro",
                              dpi = 72,
                              format = "png") {
  ## Save session for debugging
  save.image("fast_coexp.RData")
  
  ## Load dataset
  dataSet  <- readDataset(dataName)
  datExpr  <- as.matrix(dataSet$data.norm)
  stopifnot(is.numeric(datExpr))
  
  ## Packages & threads
  suppressPackageStartupMessages({
    library(WGCNA)
    library(fastcluster)
    library(Cairo)
    library(jsonlite)
    if (!is.null(enrichFDR)) if (requireNamespace("GWENA", quietly = TRUE)) library(GWENA)
  })
  enableWGCNAThreads(nThreads = n_threads)
  
  cor_func  <- match.arg(cor_func)
  network_type <- match.arg(network_type)
  
  cor_fnc  <- if (cor_func == "bicor") "bicor" else "cor"
  cor_opts <- if (cor_func == "spearman") list(method = "spearman") else list()
  cor_type <- if (cor_func == "bicor") "bicor" else "pearson"
  
  ## Pick power if requested
  if (isTRUE(auto_power) || is.null(power)) {
    sft <- suppressWarnings(
      pickSoftThreshold(
        datExpr,
        powerVector = 1:20,
        corFnc = cor_fnc,
        corOptions = cor_opts,
        networkType = network_type,
        verbose = 0
      )
    )
    idx <- which(sft$fitIndices[, "SFT.R.sq"] >= 0.85)[1]  # slightly relaxed threshold for speed
    if (is.na(idx)) idx <- which.max(sft$fitIndices[, "SFT.R.sq"])
    power <- sft$fitIndices[idx, "Power"]
  }
  
  message("[FastCoexp] Using power = ", power)
  
  ## Build network
  net <- blockwiseModules(
    datExpr,
    power = power,
    networkType = network_type,
    deepSplit = deepSplit,
    minModuleSize = minModuleSize,
    mergeCutHeight = mergeCutHeight,
    maxBlockSize = maxBlockSize,
    corType = cor_type,
    corFnc = cor_fnc,
    corOptions = cor_opts,
    hclustMethod = "average",  # fastcluster used internally
    saveTOMs = FALSE,
    verbose = 0
  )
  
  ## Eigengenes and kME
  ME <- net$MEs <- moduleEigengenes(datExpr, net$colors)$eigengenes
  net$kME <- signedKME(datExpr, ME, outputColumnName = "kME")
  
  ## Enrichment (optional, skip for speed)
  if (!is.null(enrichFDR) && requireNamespace("GWENA", quietly = TRUE)) {
    net$enrichment <- tryCatch(
      GWENA::module_enrich(
        colours = net$colors,
        organism = "hsapiens",
        fdr = enrichFDR,
        verbose = FALSE
      ),
      error = function(e) NULL
    )
  }
  
  ## Save JSON summary
  summaryJSON <- list(
    module = names(table(net$colors)),
    size = as.integer(table(net$colors)),
    eigengeneVar = apply(ME, 2, var)
  )
  jsonNm <- paste0(imgName, ".json")
  write_json(summaryJSON, jsonNm, pretty = TRUE)
  
  ## Plot dendrogram
  dendroNm <- paste0(imgName, "dpi", dpi, ".", format)
  Cairo(file = dendroNm, width = 1000, height = 600, dpi = dpi, bg = "white", type = format)
  plotDendroAndColors(
    net$dendrograms[[1]],
    net$colors[net$blockGenes[[1]]],
    "Module colors",
    dendroLabels = FALSE,
    hang = 0.03,
    addGuide = TRUE,
    guideHang = 0.05
  )
  dev.off()
  
  ## Return summary + net object
  return(list(net = net, summary = summaryJSON, dendroFile = dendroNm, jsonFile = jsonNm))
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
