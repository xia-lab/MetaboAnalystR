
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
  
  ## keep at most 2 000 heaviest edges
  if (nrow(edge.df) > 2000) {
    edge.df <- edge.df[order(edge.df$value, decreasing = TRUE), ]
    edge.df <- edge.df[1:2000, ]
  }
  
  g       <- graph_from_data_frame(edge.df, directed = FALSE)
  E(g)$weight <- edge.df$value
  
  
  
  ## ── 4 · add vertex-level annotations ----------------------------
  mod.df <- cem@module                       # cols: genes, modules
  idx    <- match(V(g)$name, mod.df$genes)
    V(g)$module <- mod.df$modules[idx]

deg   <- igraph::degree(g)
ldeg  <- log10(deg + 1)                       # stabilise high degrees

# rescale helper
resc <- function(x) (x - min(x)) / (max(x) - min(x))

# colour ramp: yellow → dark red (works on white & black)
pal <- colorRampPalette(c("#FFD54F", "#FFA726", "#EF5350", "#B71C1C"))(10)

V(g)$color  <- pal[ ceiling( resc(ldeg) * 9 ) + 1 ]   # for light bg
V(g)$colorw <- V(g)$color                             # same for dark bg
  
  ## size by degree
  rescale <- function(x, from = 8, to = 20)
    (x - min(x)) / (max(x) - min(x)) * (to - from) + from
  V(g)$size <- rescale(log10(degree(g) + 1))
  #V(g)$size <- 8;

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
  

    symVec <- doEntrez2SymbolMapping(V(g)$name,
                                     paramSet$data.org,
                                     paramSet$data.idType)

    nodes <- lapply(seq_len(vcount(g)), function(i) {
      v   <- V(g)[i]
      lbl <- if (!is.na(symVec[i]) && nzchar(symVec[i])) symVec[i] else v$name

      list(
        id        = as.character(v$name),   # still Entrez as key
        label     = lbl,                    # SYMBOL shown in SigmaJS
        size      = unclass(v$size)[1],
        true_size = unclass(v$size)[1],
        molType   = "gene",
        colorb    = as.character(v$color),
        colorw    = as.character(v$colorw),
        exp       = if (!is.null(v$expr)) unclass(v$expr)[1] else 0,
        posx      = unclass(v$posx)[1],
        posy      = unclass(v$posy)[1]
      )
    })

  
    ## ── edges with rescaled size 0.5–2.5  ----------------------------
    el <- igraph::as_data_frame(g, what = "edges")       # from, to, weight

    wMin <- min(el$weight)
    wMax <- max(el$weight)
    rescale <- function(x, from = 0.5, to = 2.5) {
      if (wMax == wMin) return((from + to) / 2)          # avoid 0/0
      (x - wMin) / (wMax - wMin) * (to - from) + from
    }

    edges <- lapply(seq_len(nrow(el)), function(i) {
      w <- as.numeric(el$weight[i])
      list(
        id     = paste0("e", i),
        source = as.character(el$from[i]),
        target = as.character(el$to[i]),
        weight = w,                     # keeps the raw weight
        size   = rescale(w)             # stroke width 0.5–2.5
      )
    })

  
  ## ── 2 · store igraph in ppi.comps -------------------------------
  if (is.null(analSet$ppi.comps)) analSet$ppi.comps <- list()
  analSet$ppi.comps[[netNm]] <- g
  
  dataSet <- readDataset(paramSet$dataName);
  nodeTable <- BuildNodeTable(g,paramSet,dataSet,analSet);
  print(head(nodeTable));
  print("head(nodeTable)");

  ## ── 3 · assemble JSON payload -----------------------------------
  netData <- list(nodes            = nodes,
                  edges            = edges,
                  backgroundColor  = list("#f5f5f5", "#0066CC"),
                  naviString       = "Correlation Network",
                  org              = paramSet$data.org,
                  nodeTable = nodeTable);
  
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

GenerateCEMModuleNetworks <- function(fileName  = "coexp_network",
                                      thresh    = 0.05,
                                      keepXTalk = FALSE) {
  
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
  netNm <- fileName;
  
  CorrIgraph2SigmaJS(g.byMod[[firstMod]],
                     netNm    = netNm,
                     paramSet = paramSet,
                     analSet  = analSet)
  
  return(1);
}


BuildNodeTable <- function(g,
                           paramSet,
                           dataSet = NULL,
                           analSet = NULL) {

  ids    <- V(g)$name
  labels <- doEntrez2SymbolMapping(ids,
                                   paramSet$data.org,
                                   paramSet$data.idType)
  anal.type <- paramSet$anal.type
  ## --- topological features --------------------------------------
  deg  <- igraph::degree(g)
  btw  <- igraph::betweenness(g)

  ## --- expression values (log2FC) --------------------------------
  expr <- rep(0, length(ids))         # default NA

  if (anal.type == "onedata") {

    tbl <- dataSet$comp.res
    inx <- match(ids, rownames(tbl))
    expr <- tbl[inx, paramSet$selectedFactorInx]

  } else if (anal.type == "metadata") {

    if (paramSet$selDataNm == "meta_default") {
      tbl  <- analSet$meta.mat.all
      sy   <- doEntrez2SymbolMapping(rownames(tbl),
                                     paramSet$data.org,
                                     paramSet$data.idType)
      inx  <- match(ids, sy)
      expr <- analSet$meta.avgFC[rownames(tbl)][inx]

    } else {                     # metadata but user-selected dataset
      ds   <- readDataset(paramSet$selDataNm)
      tbl  <- ds$comp.res
      sy   <- doEntrez2SymbolMapping(rownames(tbl),
                                     paramSet$data.org,
                                     paramSet$data.idType)
      inx  <- match(ids, sy)
      expr <- tbl[inx, "logFC"]
    }
  }
  ## for "genelist" expr stays NA  (per requirement)

  ## --- assemble data-frame ---------------------------------------
  node.df <- data.frame(
    id          = ids,
    label       = ifelse(is.na(labels) | labels == "", ids, labels),
    degree      = deg,
    betweenness = btw,
    expr        = expr,
    stringsAsFactors = FALSE
  )

  node.df <- node.df[order(node.df$degree, decreasing = TRUE), ]

  node.df
}
