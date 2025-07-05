my.enrich.net <- function(mSetObj=NA, netNm="mummichog_net", overlapType="mixed", anal.opt="mum"){
  #save.image("enrich.RData");
  mSetObj <- .get.mSet(mSetObj);
  # Get the appropriate result matrix based on analysis type
  if(anal.opt == "mum"){
    enr.mat <- mSetObj$mummi.resmat
    if(is.null(enr.mat)){
      AddErrMsg("No mummichog results found!")
      return(0)
    }
    pvals <- enr.mat[,"Gamma"]
    hits <- enr.mat[,"Hits.sig"]
  } else if(anal.opt == "gsea"){
    enr.mat <- mSetObj$mummi.gsea.resmat 
    if(is.null(enr.mat)){
      AddErrMsg("No GSEA results found!")
      return(0)
    }
    pvals <- enr.mat[,"P_val"]
    hits <- enr.mat[,"Hits"] # or another appropriate column
  } else if(anal.opt == "integ"){
    enr.mat <- mSetObj$integ.resmat
    if(is.null(enr.mat)){
      AddErrMsg("No integrated results found!")
      return(0)
    }
    pvals <- enr.mat[,"Combined_Pvals"]
    hits <- enr.mat[,"Sig_Hits"] # or another appropriate column
  }else if (anal.opt == "pathora") {
    enr.mat <- mSetObj$analSet$ora.mat
    if (is.null(enr.mat) || nrow(enr.mat) == 0) {
      AddErrMsg("No ORA enrichment results found!"); return(0)
    }
    rownames(enr.mat) <- .id2name(rownames(enr.mat))
    
    pvals <- enr.mat[, "Raw p"]
    hits  <- enr.mat[, "Hits"]
    
  } else if (anal.opt == "pathqea") {
    
    ##  Try the two most common field layouts for QEA --------------------------------
    if (!is.null(mSetObj$analSet$qea.mat)) {
      enr.mat <- mSetObj$analSet$qea.mat          # preferred object (matrix)
    } else if (!is.null(mSetObj$analSet$qea.res)) {
      enr.mat <- mSetObj$analSet$qea.res          # fallback (data-frame)
    } else {
      AddErrMsg("No QEA results found!"); return(0)
    }
    rownames(enr.mat) <- .id2name(rownames(enr.mat))
    
    ##  Column names vary slightly between GlobalTest / GlobalAncova output -----------
    p.col  <- grep("^(Raw p|P[_]?[vV]al|^p(.*)?value)$", colnames(enr.mat), ignore.case = TRUE, value = TRUE)[1]
    hitcol <- grep("^(Hits|Sig[_]?Hits|Hit|Overlap)$",      colnames(enr.mat), ignore.case = TRUE, value = TRUE)[1]
    
    if (is.na(p.col) || is.na(hitcol)) {
      AddErrMsg("Unable to locate p-value or hit columns in QEA results!"); return(0)
    }
    
    pvals <- enr.mat[, p.col]
    hits  <- enr.mat[, hitcol]
    
    
  }  else {
    AddErrMsg("Unknown analysis type!")
    return(0)
  }
  
  colnames(enr.mat) <- gsub("[ .]", "_", colnames(enr.mat))   # <-- NEW
  
  max.show   <- 20
  sig.cutoff <- 0.05
  
  sig.idx <- which(pvals <= sig.cutoff)
  n.sig   <- length(sig.idx)
  
  if (n.sig == 0) {
    message("No significant pathways (p ≤ 0.05); using the top ",
            max.show, " overall.")
  }
  
  ## 1.  Start with all pathways ordered by p-value (smallest first)
  ord.all   <- order(pvals)                       # indices
  
  ## 2.  Always keep the first `max.show` entries in that order
  keep.idx  <- ord.all[ seq_len( min(max.show, length(ord.all)) ) ]
  
  ## 3.  Subset all objects
  enr.mat   <- enr.mat[ keep.idx , , drop = FALSE ]
  pvals     <- pvals  [ keep.idx ]
  hits      <- hits   [ keep.idx ]
  
  ## 4.  Ensure we still have at least two pathways
  if (nrow(enr.mat) < 2) {
    AddErrMsg("Fewer than two pathways to display — aborting network build.")
    return(0)
  }
  
  require(igraph)
  require(reshape)
  
  # Get pathway compounds for overlap calculation
  pathway.names <- rownames(enr.mat)
  n <- length(pathway.names)
  
  # Create pathway-compound mapping
  pathway.cpds <- list()
  if (anal.opt %in% c("pathora", "pathqea")) {
    if(anal.opt == "pathora"){
      pathway.cpds <- mSetObj$analSet$ora.hits;
    }else{
      pathway.cpds <- mSetObj$analSet$qea.hits;
    }
    pathway.cpds <- Filter(length, pathway.cpds)
    names(pathway.cpds) <- .id2name(names(pathway.cpds))
    
    pathway.cpds <- lapply(pathway.cpds, function(x){
      names(x)
    });
    
  } else {
    if(mSetObj$paramSet$mumRT & mSetObj$paramSet$version=="v2"){
      sig.cpds <- mSetObj$total_matched_ecpds
    }else{
      sig.cpds <- mSetObj$total_matched_cpds
    }
    
    cmpd.db   <- .get.my.lib("compound_db.qs")
    kegg2name <- setNames(cmpd.db$name,  cmpd.db$kegg_id)   # "C00022" → "Pyruvate"
    name2kegg <- setNames(cmpd.db$kegg_id, cmpd.db$name)    # "Pyruvate" → "C00022"
    
    ## Canonicalise sig.cpds  → KEGG IDs (but keep originals if unknown)
    temp <- ifelse(sig.cpds %in% names(kegg2name),          # already ID
                   sig.cpds,
                   name2kegg[ sig.cpds ])                   # name → ID
    temp[ is.na(temp) ] <- sig.cpds[ is.na(temp) ]          # restore unknowns
    sig.ids <- unique(temp)
    
    ## -----------------------------------------------------------------
    ##  Build pathway → hit list (names or original IDs)
    ## -----------------------------------------------------------------
    
    pathway.cpds <- setNames(
      lapply(pathway.names, function(pw) {
        idx   <- which(mSetObj$pathways$name == pw)          # row in master table
        allID <- unlist(mSetObj$pathways$cpds[[ idx[1] ]])   # all IDs in pathway
        intersect(allID, sig.ids)                            # keep only sig IDs
      }),
      pathway.names
    )

  }
  
  # Calculate overlap matrix
  w <- matrix(NA, nrow=n, ncol=n)
  colnames(w) <- rownames(w) <- pathway.names
  
  for (i in 1:n) {
    for (j in i:n) {
      cpds.i <- pathway.cpds[[pathway.names[i]]]
      cpds.j <- pathway.cpds[[pathway.names[j]]]
      w[i,j] <- overlap_ratio(list(cpds.i), list(cpds.j), overlapType)
    }
  }
  
  # Create network
  wd <- reshape::melt(w)
  wd <- wd[wd[,1] != wd[,2],]
  wd <- wd[!is.na(wd[,3]),]
  
  g <- graph_from_data_frame(wd[,-3], directed=F)
  
  E(g)$weight <- wd[,3]            # wd comes from reshape::melt(w)
  
  g <- delete_edges(g, E(g)[wd[,3] < 0.05])  # Remove weak connections
  
  if(vcount(g) == 0){
    AddErrMsg("No connections above threshold!")
    return(0)
  }
  
  # Define helper functions
  my.normalize <- function(x){
    if(max(x) == min(x)) return(rep(0.5, length(x)))
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  my.rescale <- function(x, from, to){
    if(max(x) == min(x)) return(rep(mean(c(from, to)), length(x)))
    (x - min(x)) / max(x - min(x)) * (to - from) + from
  }
  
  # Get data for existing vertices
  existing_vertices <- V(g)$name
  vertex_pvals <- pvals[existing_vertices]
  vertex_hits <- hits[existing_vertices]
  
  # Compute colors based on p-values
  normalized_pvalues <- -log10(vertex_pvals + min(vertex_pvals)/2)
  vertex_colors <- ComputeColorGradient(normalized_pvalues, "black", F)
  vertex_colorsw <- ComputeColorGradient(normalized_pvalues, "black", F)
  
  V(g)$color <- vertex_colors
  V(g)$colorw <- vertex_colorsw
  
  # Set node sizes based on hits
  if (all(vertex_hits == vertex_hits[1])){
    V(g)$size <- rep(12, length(vertex_hits))
  } else {
    V(g)$size <- my.rescale(log(vertex_hits + 1, base=10), 8, 24)
  }
  
  # Layout
  pos.xy <- layout_nicely(g)
  
  # Create nodes list for JSON
  nodes <- vector(mode="list")
  node.nms <- V(g)$name
  node.sizes <- V(g)$size
  node.cols <- V(g)$color
  node.colsw <- V(g)$colorw
  if(anal.opt %in% c("pathora", "pathqea")){
    pw.ids <- unname(.name2id(node.nms));
  }else{
    pw.ids <- .mumname2id(node.nms);
  }
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id = node.nms[i],
      pwId= pw.ids[i],
      label = node.nms[i],
      size = node.sizes[i],
      true_size = node.sizes[i], 
      molType = "pathway",
      colorb = node.cols[i],
      colorw = node.colsw[i],
      pval = vertex_pvals[node.nms[i]],
      hits = vertex_hits[node.nms[i]],
      posx = pos.xy[i,1],
      posy = pos.xy[i,2]
    )
  }
  
  # Create edges
  
  e.df <- as_data_frame(g, what = "edges")  # gives from / to / weight
  
  scale01   <- function(x) (x - min(x)) / (max(x) - min(x) + 1e-9)
  #e.df$width <- as.numeric(rescale2NewRange((-log10(e.df$weight)), 0.5, 0));
  e.df$width <- 0.5;
  
  edge.mat <- apply(
    cbind(id     = seq_len(nrow(e.df)),
          source = e.df$from,
          target = e.df$to,
          weight = e.df$weight,   # keep the raw value if you like
          width  = e.df$width),   # **thickness for JS**
    1, as.list)
  
  
  # Create bipartite graph (pathways to compounds)
  # Get all compounds from significant pathways
  all.pathway.cpds <- unique(unlist(pathway.cpds[existing_vertices]))
  
  # Get significant compounds based on analysis type
  if(anal.opt == "mum"){
    if(!is.null(mSetObj$input_cpdlist)){
      sig.cpds <- mSetObj$input_cpdlist;
    } else if(!is.null(mSetObj$input_ecpdlist)){
      sig.cpds <- mSetObj$input_ecpdlist;
    }else {
      sig.cpds <- character(0)
    }
  }else if (anal.opt == "pathora") {
    sig.cpds <- unique(unlist(mSetObj$analSet$ora.hits))
  }else if (anal.opt == "pathqea") {
    sig.cpds <- unique(unlist(mSetObj$analSet$qea.hits))
  }  else {
    # For GSEA, all compounds are considered
    sig.cpds <- all.pathway.cpds
  }
  
  # Create bipartite connections
  b.mat <- matrix(NA, nrow=0, ncol=2)
  for(pathway in existing_vertices){
    cpds <- pathway.cpds[[pathway]]
    if(length(cpds) > 0){
      pathway.connections <- cbind(cpds, pathway)
      b.mat <- rbind(b.mat, pathway.connections)
    }
  }
  
  if(nrow(b.mat) > 0){
    colnames(b.mat) <- c("source", "target")
    bg <- graph_from_data_frame(b.mat, directed=F)
    
    ## -----------------  COLOUR NODES  -----------------------------
    #V(bg)$color  <- "#00FFFF"          # default for compounds
    V(bg)$color  <- "#6CD0D0"          # default for compounds
    
    V(bg)$colorw <- "#6CD0D0"
    
    ## indices of pathway nodes inside bg
    pathway.idx  <- V(bg)$name %in% rownames(enr.mat)
    
    ## (A)  TOPOLOGY score for pathway nodes  – use degree() here
    path.deg     <- degree(bg)[ pathway.idx ]
    path.norm    <- my.normalize(path.deg)                       # 0–1
    V(bg)$color [ pathway.idx ] <-
      ComputeColorGradient(path.norm,  "black",  FALSE)
    V(bg)$colorw[ pathway.idx ] <-
      ComputeColorGradient(path.norm,  "black",  FALSE)
    
    
    # Color significant compounds
    if(anal.opt == "mum"){
      sig.cpd.idx <- V(bg)$name %in% sig.cpds
      if(sum(sig.cpd.idx) > 0){
        #V(bg)$color[sig.cpd.idx] <- "#FF0000"  # Red for significant compounds
        V(bg)$color[sig.cpd.idx] <- "#D22B2B"
        V(bg)$colorw[sig.cpd.idx] <- "#D22B2B"
      }
    }
    
    # Set sizes
    node.dgr2 <- as.numeric(igraph::degree(bg))
    V(bg)$size <- my.rescale(log(node.dgr2 + 1, base=10), 6, 20)
    
    # Layout
    pos.xy.b <- layout_nicely(bg)
    
    # Create bipartite nodes
    bnodes <- vector(mode="list")
    bnode.nms <- V(bg)$name
    bnode.sizes <- V(bg)$size
    bnode.cols <- V(bg)$color
    bnode.colsw <- V(bg)$colorw
    
    shapes <- rep("compound", length(bnode.nms))
    shapes[bnode.nms %in% existing_vertices] <- "pathway"
    
    for(i in 1:length(bnode.sizes)){
      bnodes[[i]] <- list(
        id = bnode.nms[i],
        label = bnode.nms[i], 
        size = bnode.sizes[i], 
        colorb = bnode.cols[i],
        colorw = bnode.colsw[i],
        true_size = bnode.sizes[i], 
        molType = shapes[i],
        posx = pos.xy.b[i,1],
        posy = pos.xy.b[i,2]
      )
    }
    
    # Create bipartite edges
    bedge.mat <- get.edgelist(bg)
    bedge.mat <- cbind(id=paste0("b", 1:nrow(bedge.mat)), source=bedge.mat[,1], target=bedge.mat[,2])
    
  } else {
    bnodes <- list()
    bedge.mat <- matrix(nrow=0, ncol=3)
  }
  
  # Convert matrices to lists for JSON
  #edge.mat <- apply(edge.mat, 1, as.list)
  if(nrow(bedge.mat) > 0){
    bedge.mat <- apply(bedge.mat, 1, as.list)
  } else {
    bedge.mat <- list()
  }
  mum.version <- mSetObj$paramSet$version <- version
  
  #if(anal.opt %in% c("pathora", "pathqea")){
  
  
  hits.query <- pathway.cpds;
  
  #}
  
  enr.mat <- apply(enr.mat, 1, as.list)
  print(paste("lib====", mSetObj$lib.organism));
  
  if(anal.opt %in% c("pathora", "pathqea")){
    pwType <- mSetObj$pathwaylibtype; 
    sig.cpds = "";
  }else{
    pwType <- mSetObj$lib.organism;
  }
  
  netData <- list(
    nodes = nodes, 
    edges = edge.mat, 
    bnodes = bnodes, 
    bedges = bedge.mat, 
    enr=unname(enr.mat), 
    id=names(enr.mat), 
    sigCmpds= sig.cpds,
    sizes=NULL,
    hits=hits.query, 
    genelist=NULL,
    analType = anal.opt, 
    pwType=pwType,
    org="NA",
    backgroundColor = list("#f5f5f5", "#0066CC"),
    naviString = "EnrichNetwork"
  )
  
  
  # Save to JSON file
  netName <- paste0(netNm, ".json")
  sink(netName)
  cat(rjson::toJSON(netData))
  sink()
  
  # Store in mSetObj
  mSetObj$enrichNet <- netData
  
  print(paste("Network saved as:", netName))
  pheno.comps <- list();
  pheno.comps[["enrichNet"]] <- g;
  pheno.comps[["enrichNet_bipartite"]] <- bg;
  pheno.comps <<- pheno.comps;
  
  return(.set.mSet(mSetObj))
}

# Helper function for overlap calculation (if not already defined)
overlap_ratio <- function(set1, set2, type="mixed"){
  if(length(set1[[1]]) == 0 | length(set2[[1]]) == 0) return(0)
  
  intersection <- length(intersect(set1[[1]], set2[[1]]))
  
  if(type == "mixed"){
    union_size <- length(union(set1[[1]], set2[[1]]))
    if(union_size == 0) return(0)
    return(intersection / union_size)  # Jaccard index
  } else {
    min_size <- min(length(set1[[1]]), length(set2[[1]]))
    if(min_size == 0) return(0)
    return(intersection / min_size)
  }
}

.id2name <- function(ids) {
  if (!exists("current.kegglib")) {
    current.kegglib <<- qs::qread("current.kegglib.qs")
  }
  ix  <- match(ids, current.kegglib$path.ids)    # numeric indices or NA
  out <- names(current.kegglib$path.ids)[ix]     # gives NA where not found
  out[is.na(out)] <- ids[is.na(out)]             # keep originals for misses
  out
}

## Convert display names such as "Glycolysis or Gluconeogenesis"
## to their internal KEGG IDs such as "hsa00010".
## If a name is not found it is returned unchanged.
.name2id <- function(names.vec) {
  
  ## lazy-load the KEGG pathway library if it is not yet in memory
  if (!exists("current.kegglib")) {
    current.kegglib <<- qs::qread("current.kegglib.qs")
  }
  
  ids <- current.kegglib$path.ids        # named vector  Name → ID
  ix  <- match(names.vec, names(ids))    # numeric indices (or NA)
  out <- ids[ix]                         # character vector of IDs (or NA)
  
  ## keep the original entry whenever no match is found
  out[ is.na(out) ] <- names.vec[ is.na(out) ]
  
  out
}

#' Convert internal pathway IDs → display names
#'
#' @param mSetObj  MetaboAnalyst object (or NA to use current)
#' @param ids      Character vector of pathway IDs (e.g. "mfn1v10path215")
#' @return         Character vector of display names (fallback to ID if missing)
#' @export
.mumid2name <- function(ids, mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  pid     <- mSetObj$pathways$id    # e.g. c("mfn1v10path215", ...)
  pname   <- mSetObj$pathways$name  # e.g. c("Vitamin D3 ...", ...)
  map     <- setNames(pname, pid)   # named lookup: ID → name
  
  out     <- map[ids]               # may produce NA for unmatched
  out[is.na(out)] <- ids[is.na(out)]# restore original ID when no match
  
  out
}

#' Convert display pathway names → internal IDs
#'
#' @param mSetObj    MetaboAnalyst object (or NA to use current)
#' @param names.vec  Character vector of display names
#' @return           Character vector of pathway IDs (fallback to name if missing)
#' @export
.mumname2id <- function(names.vec, mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  # sanity: do we have both id & name vectors?
  if (is.null(mSetObj$pathways) ||
      is.null(mSetObj$pathways$id) ||
      is.null(mSetObj$pathways$name) ||
      length(mSetObj$pathways$id) == 0) {
    warning("No pathway mapping found; returning input unchanged.")
    return(names.vec)
  }
  
  pid   <- mSetObj$pathways$id
  pname <- mSetObj$pathways$name
  
  # build lookup and map
  map <- setNames(pid, pname)
  out <- map[names.vec]
  
  # if a name wasn't found, keep the original
  out[is.na(out)] <- names.vec[is.na(out)]
  out
}
