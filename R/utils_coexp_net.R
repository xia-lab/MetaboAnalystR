my.build.cemi.net <- function(dataName,
                              filter      = TRUE,
                              min_ngen    = 30,
                              cor_method  = "pearson", ##c("pearson", "spearman"),
                              verbose     = F) {

  save.image("cemitool.RData")                  # snapshot

  ## 1 · load dataset -------------------------------------------------
    dataSet  <- readDataset(dataName)
    expr_mat <- as.data.frame(dataSet$data.norm)          # genes × samples

    ## keep every metadata column, coerce factors → character
    meta_df <- dataSet$meta.info
    meta_df[] <- lapply(meta_df, function(x)
                          if (is.factor(x)) as.character(x) else x)

    annot_df <- data.frame(SampleName = rownames(meta_df),
                           meta_df,
                           check.names = FALSE,
                           stringsAsFactors = FALSE)

  ## 2 · packages -----------------------------------------------------
  suppressPackageStartupMessages({
    library(CEMiTool)
    library(WGCNA)
  })

  ## 3 · run CEMiTool -------------------------------------------------
  cem <- cemitool(expr       = expr_mat,
                  annot      = annot_df,
                  filter     = filter,
                  min_ngen   = min_ngen,
                  cor_method = match.arg(cor_method),
                  plot       = TRUE)

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

  qs::qsave(cem, "cem.qs");

  ## 7 · JSON summary -------------------------------------------------
  #summaryJSON <- list(module       = colnames(MEs),
  #                    eigengeneVar = apply(MEs, 2, var))
  #write_json(summaryJSON, paste0(imgName, ".json"), pretty = TRUE)

  #invisible(list(cem  = cem,
  #               MEs  = MEs,
  #               dend = hc,
  #               img  = img_file,
  #               json = paste0(imgName, ".json")))
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
      main         = paste("CEMiTool", label, "dendrogram"),
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



# ====================================================================
# BuildIgraphFromCEM  —  make a data-derived gene–gene network
# --------------------------------------------------------------------
# file        : path to the qs-saved CEMiTool object (“cem.qs”)
# thresh      : numeric, keep edges with weight > thresh
# return      : igraph object with vertex/edge attributes
# ====================================================================
BuildIgraphFromCEM <- function(thresh    = 0.05,
                               layoutFun = igraph::layout_nicely) {
  
  ## ── 0 · packages ─────────────────────────────────────────────────
  library(CEMiTool)
  library(igraph)
  library(reshape2)
  
  ## ── 1 · read the CEMiTool object ────────────────────────────────
  cem <- qs::qread("cem.qs")
  
  ## ── 2 · make sure we have an adjacency matrix -------------------
  ##     (CEMiTool stores β only if you explicitly asked for it.)
  get_beta <- function(cem) {
    # 1) try stored β
    if (!is.null(cem@parameters$beta))
      return(as.numeric(cem@parameters$beta))
    
    # 2) try default scale-free heuristic (≥ v1.29)
    beta <- tryCatch({
      get_cemitool_r2_beta(cem)[2]   # returns c(R2, β)
    }, error = function(e) NA)
    
    # 3) last-resort: WGCNA pickSoftThreshold on the expression matrix
    if (is.na(beta)) {
      expr <- CEMiTool:::get_expression(cem)   # matrix genes × samples
      sft  <- WGCNA::pickSoftThreshold(t(expr), verbose = 0)
      beta <- sft$powerEstimate
      if (is.na(beta)) beta <- 6              # fallback default
    }
    beta
  }
  
  if (is.null(cem@adjacency)) {
    beta <- get_beta(cem)
    cem  <- get_adj(cem, beta = beta)
  }
  adj <- adj_data(cem)                        # square genes × genes
  
  ## ── 3 · build edge list above threshold -------------------------
  edge.df <- melt(adj)
  edge.df <- subset(edge.df, value > thresh & Var1 != Var2)
  g       <- graph_from_data_frame(edge.df, directed = FALSE)
  E(g)$weight <- edge.df$value
  
  ## ── 4 · add vertex-level annotations ----------------------------
  mod.df <- cem@module                       # cols: genes, modules
  idx    <- match(V(g)$name, mod.df$genes)
  
  V(g)$module <- mod.df$modules[idx]
  # colour by module for quick QC
  module.cols <- setNames(
    RColorBrewer::brewer.pal(max(3, length(unique(mod.df$modules))), "Set3"),
    sort(unique(mod.df$modules)))
  V(g)$color <- module.cols[V(g)$module]
  V(g)$colorw <- V(g)$color                 # same palette for white BG
  
  ## size by degree
  rescale <- function(x, from = 8, to = 32)
    (x - min(x)) / (max(x) - min(x)) * (to - from) + from
  V(g)$size <- rescale(log10(degree(g) + 1))
  
  ## ── 5 · 2-D layout coordinates ----------------------------------
  xy <- layoutFun(g)
  V(g)$posx <- xy[, 1]
  V(g)$posy <- xy[, 2]
  
  return(g)
}

# ------------------------------------------------------------------
# CorrIgraph2SigmaJS
# ------------------------------------------------------------------
# g       : igraph output from BuildCorrIgraph
# netNm   : name for network
# paramSet, analSet : your usual state containers
# path    : where to write JSON file (defaults to working dir)
# ------------------------------------------------------------------
CorrIgraph2SigmaJS <- function(g,
                               netNm     = "coexp_net",
                               paramSet,
                               analSet,
                               path      = ".") {
  
  ## ── 1 · node & edge lists ---------------------------------------
  nodes <- lapply(seq_len(vcount(g)), function(i) {
    v <- V(g)[i]
    list(
      id        = v$name,
      label     = if (!is.null(v$symbol)) v$symbol else v$name,
      size      = v$size,
      true_size = v$size,
      molType   = "gene",
      colorb    = v$color,
      colorw    = v$colorw,
      exp       = if (!is.null(v$expr)) v$expr else 0,
      posx      = v$posx,
      posy      = v$posy
    )
  })
  
  edges <- lapply(seq_len(ecount(g)), function(i) {
    e <- E(g)[i]
    list(id     = paste0("e", i),
         source = ends(g, e)[1],
         target = ends(g, e)[2],
         weight = e$weight)
  })
  
  ## ── 2 · store igraph in ppi.comps -------------------------------
  if (is.null(analSet$ppi.comps)) analSet$ppi.comps <- list()
  analSet$ppi.comps[[netNm]] <- g
  
  ## ── 3 · assemble JSON payload -----------------------------------
  netData <- list(nodes            = nodes,
                  edges            = edges,
                  backgroundColor  = list("#f5f5f5", "#0066CC"),
                  naviString       = "Correlation Network",
                  org              = paramSet$data.org)
  
  fileNm <- file.path(path, paste0(netNm, ".json"))
  jsonlite::write_json(netData, fileNm, auto_unbox = TRUE)
  
  ## track for later download
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, fileNm)
  paramSet$jsonNms$network <- basename(fileNm)
  saveSet(paramSet, "paramSet")
  analSet$corNet <- netData
  saveSet(analSet, "analSet")
  
  invisible(netData)
}

SplitIgraphByModule <- function(g, keepXTalk = FALSE) {
  
  stopifnot("module" %in% vertex_attr_names(g))
  
  mods <- sort(unique(V(g)$module))
  subG <- setNames(vector("list", length(mods)), mods)
  
  for (m in mods) {
    genes.m <- V(g)[module == m]
    
    if (keepXTalk) {
      # keep all edges touching those genes
      subG[[m]] <- induced_subgraph(g, vids = genes.m)
    } else {
      # keep *only* edges whose BOTH endpoints are in module m
      eKeep <- E(g)[inc(V(g)[module == m])]
      eKeep <- eKeep[ which(ends(g, eKeep)[,1] %in% genes.m$name &
                              ends(g, eKeep)[,2] %in% genes.m$name) ]
      subG[[m]] <- subgraph.edges(g, eKeep)
    }
  }
  subG
}

GenerateCEMModuleNetworks <- function(fileName, thresh    = 0.05,
                                      keepXTalk = FALSE) {
  save.image("generate.RData");
  ## ── 0 · session containers ──────────────────────────────────────
  paramSet <- readSet(paramSet, "paramSet")
  analSet  <- readSet(analSet,  "analSet")
  
  ## ── 1 · build the full graph & split per module ─────────────────
  g.all   <- BuildIgraphFromCEM(thresh    = thresh)
  g.byMod <- SplitIgraphByModule(g.all, keepXTalk = keepXTalk)
  
  ## ── 2 · stash every igraph in ppi.comps for programmatic use ────
  analSet$ppi.comps <- g.byMod
  saveSet(analSet, "analSet")
  
  ## ── 3 · write JSON for *only the first* module in the list ─────
  firstMod <- names(g.byMod)[1]              # e.g. "M1"
  
  CorrIgraph2SigmaJS(g.byMod[[firstMod]],
                     netNm    = fileName,
                     paramSet = paramSet,
                     analSet  = analSet)
  
  return(1);
}
# ================================================================
# GenerateCEMTOMHeatmapJSON
# ----------------------------------------------------------------
# fileName   : qs-saved CEMiTool object (default "cem.qs")
# jsonFile   : output JSON for Plotly (default "tom_heatmap.json")
# threshClip : clip TOM values above this (improves contrast)
# digits     : decimals in tooltip
# ----------------------------------------------------------------
# return     : 1 = success, 0 = failure
# ================================================================
GenerateCEMTOMHeatmapJSON <- function(jsonFile   = "tom_heatmap.json",
                                      threshClip = 1) {
save.image("hm.RData");
  digits <- 3;
  tryCatch({

    library(CEMiTool)
    library(WGCNA)
    library(plotly)
    library(jsonlite)
    library(RColorBrewer)

    ## ── 1 · read cem ------------------------------------------------
    cem <- qs::qread("cem.qs")

    ## ── 2 · ensure adjacency matrix --------------------------------
    if (is.null(cem@adjacency)) {
      beta <- if (!is.null(cem@parameters$beta)) {
                as.numeric(cem@parameters$beta)
              } else {
                get_cemitool_r2_beta(cem)[2]
              }
      cem <- get_adj(cem, beta = beta)
    }
    adj <- adj_data(cem)

    ## ── 3 · compute TOM -------------------------------------------
    tom <- TOMsimilarity(adj)
    rownames(tom) <- colnames(tom) <- rownames(adj)

    ## ── 4 · module colours ----------------------------------------
    modVec <- cem@module$modules
    names(modVec) <- cem@module$genes

    # colour palette
    uniqMods  <- sort(unique(modVec))
    palette   <- if (length(uniqMods) <= 12)
                   brewer.pal(max(3, length(uniqMods)), "Set3")
                 else rainbow(length(uniqMods))
    modCols   <- setNames(palette[seq_along(uniqMods)], uniqMods)
    modColors <- modCols[modVec]

    ## reorder rows/cols by module + gene name for nicer blocks
    ord       <- order(modVec, names(modVec))
    tom       <- tom[ord, ord]
    modColors <- modColors[ord]

    ## ── 5 · build Plotly heat-map ---------------------------------
    tomClip <- pmin(tom, threshClip)

    plt <- subplot(
      # colour strip (1 row heat-map)
      plot_ly(
        y          = 1,
        x          = seq_along(modColors),
        type       = "heatmap",
        z          = matrix(1, nrow = 1, ncol = length(modColors)),
        showscale  = FALSE,
        colorscale = list(list(0,"white"), list(1,"white")),
        hoverinfo  = "skip",
        marker     = list(color = modColors)
      ),
      # main TOM heat-map
      plot_ly(
        x             = colnames(tomClip),
        y             = rownames(tomClip),
        z             = tomClip,
        type          = "heatmap",
        colorscale    = "Viridis",
        zmin          = 0,
        zmax          = threshClip,
        hovertemplate = paste0(
          "<b>%{x}</b> vs <b>%{y}</b>",
          "<br>TOM = %{z:.", digits, "f}<extra></extra>")
      ),
      nrows   = 2,
      heights = c(0.02, 0.98),
      shareX  = TRUE,
      titleY  = FALSE
    )

    ## ── 6 · dump to JSON ------------------------------------------
    write_json(plotly_json(plt, jsonedit = FALSE, digits = digits),
               path       = jsonFile,
               auto_unbox = TRUE)

    return(1)   # success
  }, error = function(e) {
    message("GenerateCEMTOMHeatmapJSON: ", e$message)
    return(0)   # failure
  })
}
