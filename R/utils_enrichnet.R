my.enrich.net <- function(mSetObj=NA, netNm="mummichog_net", overlapType="mixed", anal.opt="mum"){
  mSetObj <- .get.mSet(mSetObj);
  save.image("enrichnet.RData");
  # Get the appropriate result matrix based on analysis type
  if(anal.opt == "mum"){
    enr.mat <- mSetObj$mummi.resmat
    if(is.null(enr.mat)){
      print("No mummichog results found!")
      return(0)
    }
    pvals <- enr.mat[,"Gamma"]
    hits <- enr.mat[,"Hits.sig"]
  } else if(anal.opt == "gsea"){
    enr.mat <- mSetObj$mummi.gsea.resmat 
    if(is.null(enr.mat)){
      print("No GSEA results found!")
      return(0)
    }
    pvals <- enr.mat[,"P_val"]
    hits <- enr.mat[,"Hits"] # or another appropriate column
  } else if(anal.opt == "integ"){
    enr.mat <- mSetObj$integ.resmat
    if(is.null(enr.mat)){
      print("No integrated results found!")
      return(0)
    }
    pvals <- enr.mat[,"Combined_Pvals"]
    hits <- enr.mat[,"Sig_Hits"] # or another appropriate column
  }else if (anal.opt == "pathora") {
    enr.mat <- mSetObj$analSet$ora.mat
    print(head(enr.mat));
    if (is.null(enr.mat) || nrow(enr.mat) == 0) {
      message("No ORA enrichment results found!"); return(0)
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
      message("No QEA results found!"); return(0)
    }
    
    ##  Column names vary slightly between GlobalTest / GlobalAncova output -----------
    p.col  <- grep("^(Raw p|P[_]?[vV]al|^p(.*)?value)$", colnames(enr.mat), ignore.case = TRUE, value = TRUE)[1]
    hitcol <- grep("^(Hits|Sig[_]?Hits|Hit|Overlap)$",      colnames(enr.mat), ignore.case = TRUE, value = TRUE)[1]
    
    if (is.na(p.col) || is.na(hitcol)) {
      message("Unable to locate p-value or hit columns in QEA results!"); return(0)
    }
    rownames(enr.mat) <- .id2name(rownames(enr.mat))
    
    pvals <- enr.mat[, p.col]
    hits  <- enr.mat[, hitcol]
    
    
  }  else {
    print("Unknown analysis type!")
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
  message("Fewer than two pathways to display — aborting network build.")
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
    if (anal.opt == "mum") {
      sig.cpds <- mSetObj$input_cpdlist
    } else {
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
    hits.query <- lapply(pathway.names, function(pw) {
      
      idx   <- which(mSetObj$pathways$name == pw)
      allID <- mSetObj$pathways$cpds[[ idx[1] ]]        # KEGG IDs in pathway
      hitID <- allID[ allID %in% sig.ids ]              # only significant IDs
      
      mapped <- kegg2name[ hitID ]                      # may be NA / "" / "NA"
      
      ## fallback for NA, empty string, or literal "NA"
      needFallback <- is.na(mapped) | mapped == "" | mapped == "NA"
      mapped[ needFallback ] <- hitID[ needFallback ]
      
      unname(mapped)                                   # plain character vector
    })
    names(hits.query) <- pathway.names
    
    ## Use hit lists downstream
    pathway.cpds <- hits.query
    
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

  g <- delete_edges(g, E(g)[wd[,3] < 0.05])  # Remove weak connections
  
  if(vcount(g) == 0){
    print("No connections above threshold!")
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
  vertex_colorsw <- ComputeColorGradient(normalized_pvalues, "white", F)
  
  V(g)$color <- vertex_colors
  V(g)$colorw <- vertex_colorsw
  
  # Set node sizes based on hits
  if (all(vertex_hits == vertex_hits[1])){
    V(g)$size <- rep(16, length(vertex_hits))
  } else {
    V(g)$size <- my.rescale(log(vertex_hits + 1, base=10), 8, 32)
  }
  
  # Layout
  pos.xy <- layout_nicely(g)
  
  # Create nodes list for JSON
  nodes <- vector(mode="list")
  node.nms <- V(g)$name
  node.sizes <- V(g)$size
  node.cols <- V(g)$color
  node.colsw <- V(g)$colorw
  
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id = node.nms[i],
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
  edge.mat <- as_edgelist(g)
  edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2])
  
  # Create bipartite graph (pathways to compounds)
  # Get all compounds from significant pathways
  all.pathway.cpds <- unique(unlist(pathway.cpds[existing_vertices]))
  
  # Get significant compounds based on analysis type
  if(anal.opt == "mum"){
    if(!is.null(mSetObj$input_cpdlist)){
      sig.cpds <- mSetObj$input_cpdlist
    } else {
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
    
    # Color nodes
    V(bg)$color <- "#00FFFF"  # Default compound color
    V(bg)$colorw <- "#668B8B"
    
    # Color pathways
    pathway.idx <- V(bg)$name %in% existing_vertices
    V(bg)$color[pathway.idx] <- vertex_colors[V(bg)$name[pathway.idx]]
    V(bg)$colorw[pathway.idx] <- vertex_colorsw[V(bg)$name[pathway.idx]]
    
    # Color significant compounds
    #sig.cpd.idx <- V(bg)$name %in% sig.cpds
    #if(sum(sig.cpd.idx) > 0){
    #V(bg)$color[sig.cpd.idx] <- "#FF0000"  # Red for significant compounds
    #V(bg)$colorw[sig.cpd.idx] <- "#8B0000"
    #}
    
    # Set sizes
    node.dgr2 <- as.numeric(degree(bg))
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
  edge.mat <- apply(edge.mat, 1, as.list)
  if(nrow(bedge.mat) > 0){
    bedge.mat <- apply(bedge.mat, 1, as.list)
  } else {
    bedge.mat <- list()
  }
  mum.version <- mSetObj$paramSet$version <- version
  
  if(anal.opt %in% c("pathora", "pathqea")){
    
    
    hits.query <- pathway.cpds;
    
  }
  
  enr.mat <- apply(enr.mat, 1, as.list)
  
  netData <- list(
    nodes = nodes, 
    edges = edge.mat, 
    bnodes = bnodes, 
    bedges = bedge.mat, 
    enr=unname(enr.mat), 
    id=names(enr.mat), 
    sizes=NULL,
    hits=hits.query, 
    genelist=NULL,
    analType = anal.opt, 
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
