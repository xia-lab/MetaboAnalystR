
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
  overall.graph <<- g;
  return(1)
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
                                      keepXTalk = FALSE,
                                      minNodeNum = 3) {

  paramSet <- readSet(paramSet, "paramSet")
  analSet  <- readSet(analSet,  "analSet")
  
  g.all   <- overall.graph
  g.byMod <- SplitIgraphByModule(g.all, keepXTalk = keepXTalk)
  
  comps <- g.byMod


  # first compute subnet stats
  net.stats <- ComputeSubnetStats(comps);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  comps <- comps[ord.inx];
  names(comps) <- rownames(net.stats) <- paste("subnetwork", 1:length(comps), sep="");

  # note, we report stats for all nets (at least 3 nodes);
  hit.inx <- net.stats$Node >= minNodeNum;
  comps <- comps[hit.inx];


  # now record
  net.stats <<- net.stats;
  sub.stats <- unlist(lapply(comps, vcount));

  
  ## ── 3 · write JSON for *only the first* module in the list ─────
  firstMod <- names(g.byMod)[1]              # e.g. "M1"
  netNm <- fileName;
  analSet$ppi.comps <- comps;
    
  saveSet(analSet, "analSet")
  return(c(vcount(g.all), ecount(g.all), length(comps), sub.stats));
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


GetNetsName <- function(){
  rownames(net.stats);
}

GetNetsNameString <- function(){
  paste(rownames(net.stats), collapse="||");
}

GetNetsEdgeNum <- function(){
  as.numeric(net.stats$Edge);
}

GetNetsNodeNum <- function(){
  as.numeric(net.stats$Node);
}

GetNetsQueryNum <- function(){
  as.numeric(net.stats$Query);
}


ComputeSubnetStats <- function(comps){
  library(igraph);
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  colnames(net.stats) <- c("Node", "Edge", "Query");
  for(i in 1:length(comps)){
    g <- comps[[i]];
    net.stats[i,] <- c(vcount(g),ecount(g),vcount(g));
  }
  return(net.stats);
}
# ====================================================================
# filterNetByThreshold  —  edge-weight filtering for an igraph object
# --------------------------------------------------------------------
# g          : igraph object that already has a numeric edge attribute
#              called 'weight'
# thresh     : keep edges with weight > thresh
# maxEdges   : cap the network at this many heaviest edges (NULL = no cap)
# rmIsolated : TRUE → delete nodes that become isolated after filtering
# layoutFun  : (optional) layout recalculation if you need updated coords
# return     : list(graph = <filtered igraph>,
#                   stats = c(nodes, edges, n.components))
# ====================================================================
filterNetByThreshold <- function(g,
                                 thresh      = 0.05,
                                 maxEdges    = 2000,
                                 rmIsolated  = TRUE) {

  if (!inherits(g, "igraph"))
    stop("`g` must be an igraph object")
  if (!"weight" %in% edge_attr_names(g))
    stop("edge attribute 'weight' not found")

  # ── 1 · keep only edges above threshold ───────────────────────────
  g <- subgraph.edges(g, E(g)[weight > thresh], delete.vertices = FALSE)

  # ── 2 · cap total edges if requested ──────────────────────────────
  if (!is.null(maxEdges) && ecount(g) > maxEdges) {
    el <- igraph::as_data_frame(g, what = "edges")
    el <- el[order(el$weight, decreasing = TRUE), ][seq_len(maxEdges), ]
    g  <- graph_from_data_frame(el,
                                directed = FALSE,
                                vertices = igraph::as_data_frame(g, what = "vertices"))
    E(g)$weight <- el$weight                    # restore weights
  }

  # ── 3 · optionally drop newly-isolated nodes ──────────────────────
  if (rmIsolated) {
    iso <- which(igraph::degree(g) == 0)
    if (length(iso) > 0)
      g <- delete_vertices(g, iso)
  }

  # ── 4 · final tidy-up (loops / multi-edges) ───────────────────────
  g <- simplify(g, edge.attr.comb = list("first"))

  # ── 6 · book-keeping & return  ────────────────────────────────────
  AddMsg(
    paste("filterNetByThreshold:",
          vcount(g), "nodes and", ecount(g), "edges retained at thresh >", thresh)
  )

  substats <- DecomposeGraph(g)          # user’s helper: returns component stats
  outStats <- c(vcount(g), ecount(g), length(substats))
  overall.graph <<- g;
  return(outStats)                  # compatible with FilterBipartiNet-style use
}


FilterBipartiNet <- function(nd.type, min.dgr, min.btw){
    paramSet <- readSet(paramSet, "paramSet");

    all.nms <- V(overall.graph)$name;
    edge.mat <- get.edgelist(overall.graph);
    dgrs <- degree(overall.graph);
    nodes2rm.dgr <- nodes2rm.btw <- NULL;

    if(nd.type == "gene"){
        hit.inx <- all.nms %in% edge.mat[,1];
    }else if(nd.type=="other"){
        hit.inx <- all.nms %in% edge.mat[,2];
    }else{ # all
        hit.inx <- rep(TRUE, length(all.nms));
    }

    if(min.dgr > 0){
        rm.inx <- dgrs <= min.dgr & hit.inx;
        nodes2rm.dgr <- V(overall.graph)$name[rm.inx];
    }
    if(min.btw > 0){
        btws <- betweenness(overall.graph);
        rm.inx <- btws <= min.btw & hit.inx;
        nodes2rm.btw <- V(overall.graph)$name[rm.inx];
    }

    nodes2rm <- unique(c(nodes2rm.dgr, nodes2rm.btw));
    overall.graph <- simplify(delete.vertices(overall.graph, nodes2rm));
    current.msg <<- paste("A total of", length(nodes2rm) , "was reduced.");
    analSet <- DecomposeGraph(overall.graph,analSet);
    substats <- analSet$substats;
    if(!is.null(substats)){
        overall.graph <<- overall.graph;
        output <- c(vcount(overall.graph), ecount(overall.graph), length(ppi.comps), substats);
    }else{
        output <- 0;
    }

    analSet$overall.graph <- overall.graph;
    return(saveSet(analSet, "analSet", output));
}

PrepareNetwork <- function(net.nm, jsonNm){
   analSet <- readSet(analSet, "analSet");
   paramSet <- readSet(paramSet, "paramSet");
   print(analSet$ppi.comps);

   my.ppi <- analSet$ppi.comps[[net.nm]];
   nd.nms <- V(my.ppi)$name;


  CorrIgraph2SigmaJS(my.ppi,
                     netNm    = jsonNm,
                     paramSet = paramSet,
                     analSet  = analSet)


   current.net.nm <<- net.nm;
   return(saveSet(analSet, "analSet", 1));

}

PerformNetEnrichment <- function(dataName="", file.nm, fun.type, IDs){
  #dataName <<- dataName;
  #file.nm <<- file.nm;
  #fun.type <<- fun.type;
  #IDs <<- IDs;
  #save.image("PerformNetEnrichment.RData");
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  data.org <- paramSet$data.org;
  # prepare query
  ora.vec <- NULL;
  idtype <- "entrez";
  
    ora.vec <- unlist(strsplit(IDs, "; "));
    names(ora.vec) <- as.character(ora.vec);
  
  
  if(fun.type %in% c("trrust", "encode", "jaspar", "mirnet", "met", "drugbank", "disease")){
    res <- PerformRegEnrichAnalysis(dataSet, file.nm, fun.type, ora.vec, "inverse");
  }else{
    res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, ora.vec, "coexp");
  }

  return(res);
}

PerformRegEnrichAnalysis <- function(dataSet, file.nm, fun.type, ora.vec, netInv){
    if(!exists("my.reg.enrich")){ # public web on same user dir
        compiler::loadcmp(paste0(resource.dir, "rscripts/ExpressAnalystR/R/_utils_regenrich.Rc"));    
    }
    return(my.reg.enrich(dataSet, file.nm, fun.type, ora.vec, netInv));
}

FindCommunities <- function(method="walktrap", use.weight=FALSE){
  paramSet <- readSet(paramSet, "paramSet")
  seed.expr <- paramSet$seed.expr;
  # make sure this is the connected
  current.net <- ppi.comps[[current.net.nm]];
  g <- current.net;
  if(!is.connected(g)){
    g <- decompose.graph(current.net, min.vertices=2)[[1]];
  }
  total.size <- length(V(g));
  
  if(use.weight){ # this is only tested for walktrap, should work for other method
    # now need to compute weights for edges
    egs <- get.edges(g, E(g)); #node inx
    nodes <- V(g)$name;
    # conver to node id
    negs <- cbind(nodes[egs[,1]],nodes[egs[,2]]);
    
    # get min FC change
    base.wt <- min(abs(seed.expr))/10;
    
    # check if user only give a gene list without logFC or all same fake value
    if(length(unique(seed.expr)) == 1){
      seed.expr <- rep(1, nrow(negs));
      base.wt <- 0.1; # weight cannot be 0 in walktrap
    }
    
    wts <- matrix(base.wt, ncol=2, nrow = nrow(negs));
    for(i in 1:ncol(negs)){
      nd.ids <- negs[,i];
      hit.inx <- match(names(seed.expr), nd.ids);
      pos.inx <- hit.inx[!is.na(hit.inx)];
      wts[pos.inx,i]<- seed.expr[!is.na(hit.inx)]+0.1;
    }
    nwt <- apply(abs(wts), 1, function(x){mean(x)^2})    
  }
  
  if(method == "walktrap"){
    fc <- walktrap.community(g);
  }else if(method == "infomap"){
    fc <- infomap.community(g);
  }else if(method == "labelprop"){
    fc <- label.propagation.community(g);
  }else{
    print(paste("Unknown method:", method));
    return ("NA||Unknown method!");
  }
  
  if(length(fc) == 0 || modularity(fc) == 0){
    return ("NA||No communities were detected!");
  }
  
  # only get communities
  communities <- communities(fc);
  community.vec <- vector(mode="character", length=length(communities));
  gene.community <- NULL;
  qnum.vec <- NULL;
  pval.vec <- NULL;
  rowcount <- 0;
  nms <- V(g)$name;
  hit.inx <- match(nms, ppi.net$node.data[,1]);
  sybls <- ppi.net$node.data[hit.inx,2];
  names(sybls) <- V(g)$name;
  for(i in 1:length(communities)){
    # update for igraph 1.0.1 
    path.ids <- communities[[i]];
    psize <- length(path.ids);
    if(psize < 5){
      next; # ignore very small community
    }
    if(netUploadU == 1){
      qnums <- psize;
    }else{
      hits <- seed.proteins %in% path.ids;
      qnums <- sum(hits);
    }
    if(qnums == 0){
      next; # ignor community containing no queries
    }
    
    rowcount <- rowcount + 1;
    pids <- paste(path.ids, collapse="->");
    #path.sybls <- V(g)$Label[path.inx];
    path.sybls <- sybls[path.ids];
    com.mat <- cbind(path.ids, path.sybls, rep(i, length(path.ids)));
    gene.community <- rbind(gene.community, com.mat);
    qnum.vec <- c(qnum.vec, qnums);
    
    # calculate p values (comparing in- out- degrees)
    subgraph <- induced.subgraph(g, path.ids);
    in.degrees <- degree(subgraph);
    out.degrees <- degree(g, path.ids) - in.degrees;
    ppval <- suppressWarnings(wilcox.test(in.degrees, out.degrees)$p.value);
    ppval <- signif(ppval, 3);
    pval.vec <- c(pval.vec, ppval);
    
    # calculate community score
    community.vec[rowcount] <- paste(c(psize, qnums, ppval, pids), collapse=";");
  }

  # Create a dataframe to store community size and p-value
  community_data <- do.call(rbind, lapply(community.vec, function(x) {
    parts <- strsplit(x, ";")[[1]]
    return(data.frame(size = as.numeric(parts[1]), p_value = as.numeric(parts[3])))
  }))

  # Order the dataframe by size (descending) and then by p-value (ascending)
  ordered_indices <- with(community_data, order(-size, p_value))

  # Order the community.vec based on sorted indices
  community.vec <- community.vec[ordered_indices]
  
  all.communities <- paste(community.vec, collapse="||");
  if(!is.null(gene.community)){
    colnames(gene.community) <- c("Id", "Label", "Module");
    fast.write(gene.community, file="module_table.csv", row.names=F);
    return(all.communities);
  }else{
    return("NA");
  }
  
}

community.significance.test <- function(graph, vs, ...) {
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}


# from to should be valid nodeIDs
GetShortestPaths <- function(from, to){
  current.net <- ppi.comps[[current.net.nm]];
  paths <- get.all.shortest.paths(current.net, from, to)$res;
  if(length(paths) == 0){
    return (paste("No connection between the two nodes!"));
  }
  
  path.vec <- vector(mode="character", length=length(paths));
  for(i in 1:length(paths)){
    path.inx <- paths[[i]]; 
    path.ids <- V(current.net)$name[path.inx];
    path.sybls <- path.ids;
    pids <- paste(path.ids, collapse="->");
    psbls <- paste(path.sybls, collapse="->");
    path.vec[i] <- paste(c(pids, psbls), collapse=";")
  }
  
  if(length(path.vec) > 50){
    path.vec <- path.vec[1:50];
  }
  
  all.paths <- paste(path.vec, collapse="||");
  return(all.paths);
}