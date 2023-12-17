##################################################
## R scripts for mGWAS-Explorer
## Description: Network analysis methods
## Author: Le, le.chang@mail.mcgill.ca
###################################################

# decompose to individual connected subnetworks, discard very small ones (defined by minNodeNum)
DecomposeGraph <- function(gObj, minNodeNum = 2){
  mSetObj <- .get.mSet(mSetObj);
   #save.image("DecomposeGraph.RData")
  edge_df <- as.data.frame(get.edgelist(gObj));
  names(edge_df) <- c("source", "target")
  library(dplyr)
  # Create the summary data.frame
    summary_df <- edge_df %>%
      group_by(target) %>%
      summarise(
        num_targets = n(),
        targets = paste0(source, collapse=", ")
      )

    # Display the summary data.frame
  mSetObj$analSet$net.summary <- summary_df;
  comps <-decompose.graph(gObj, min.vertices=minNodeNum);
  
  if(length(comps) == 0){
    current.msg <<- paste("No connected nodes found after this filtering!");
    return(NULL);
  }
  
  # first get stats
  queries <- unique(mSetObj$dataSet$seeds);
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  mSetObj$dataSet$query.nums <- vector()
  mSetObj$dataSet$type.nums <- vector()
  res.list <- list();
  for(i in 1:length(comps)){
    g <- comps[[i]];
    if(vcount(g) > 0){
      my.stat <- GetNetStatByType(g);
      res.list[[i]] <- list()
      res.list[[i]][["type.nums"]] <- my.stat$node.num;
      res.list[[i]][["query.nums"]] <- my.stat$query.num;
      net.stats[i,] <- c(
        vcount(g),
        ecount(g),
        sum(queries %in% V(g)$name)
      );
    }
  }
  
  # now sort graph based on node size and add names
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  res.list <- res.list[ord.inx];
  mSetObj$dataSet$type.nums <- unlist(lapply(res.list, function(x) x[["type.nums"]]))
  mSetObj$dataSet$query.nums <- unlist(lapply(res.list, function(x) x[["query.nums"]]))

  comps <- comps[ord.inx];
  names(comps) <- rownames(net.stats) <- paste("mgwas", 1:length(comps), sep="");
  net.stats <- cbind(rownames(net.stats), net.stats);
  colnames(net.stats) <- c("Name", "Node", "Edge", "Query");
  
  # note, we report stats for all nets (at least 2 nodes);
  # but only contruct at least min node
  hit.inx <- net.stats$Node >= minNodeNum;
  comps <- comps[hit.inx];
  sub.stats <- NULL;
  json.res <- rep(list(list()), length(comps));
  i <- 0;
  for(nm in names(comps)){
    sub.stats <- c(sub.stats, vcount(comps[[nm]]));
  }
  
  # now save the components
  mir.nets <<- comps;
  net.stats <<- net.stats[,-1];  # remove the first name col
  
  # update the mir.res edge table
  # both side of the edge must present in all.nodes
  all.nodes <- V(gObj)$name;
  res <- mSetObj$dataSet$mir.res;
  hit.inx <- (res[, 1] %in% all.nodes) & (res[, 2] %in% all.nodes);

  mSetObj$dataSet$mir.filtered <- res[hit.inx, ];
  .set.mSet(mSetObj);
  return(sub.stats);
}

ReduceEdgeDensity <- function(nd.type="all"){
  mSetObj <- .get.mSet(mSetObj);
  all.nms <- V(mir.graph)$name;
  edge.mat <- get.edgelist(mir.graph);
  dgrs <- degree(mir.graph);
  nodes2rm <- NULL;
  
  set.seed(8574);
  if(length(all.nms) > 50){
    # only get top 50 with highest density (degree)
    inx <- rank(-dgrs) < 50;
    seed.vec <- all.nms[inx];
  }else{
    seed.vec <- all.nms;
  }
  paths.list <-list();
  # now calculate the shortest paths only between these densely connected nodes
  for(pos in 1:length(seed.vec)){
    paths.list[[pos]] <- get.shortest.paths(mir.graph, seed.vec[pos], seed.vec[-pos])$vpath;
  }
  nds.inxs <- unique(unlist(paths.list));
  nodes2rm <- all.nms[-nds.inxs];
  
  # keep queries
  if(nd.type == "snp"){ # only apply removing to SNP nodes
    mir.nms <- unique(edge.mat[,1]);
    nodes2rm <- nodes2rm[nodes2rm %in% mir.nms];
  }else if(nd.type=="other"){
    my.nms <- unique(edge.mat[,2]);
    nodes2rm <- nodes2rm[nodes2rm %in% my.nms];
  }else{
    #nothing to do
  }
  path.list <- NULL; gc();
  nodes2rm <- unique(nodes2rm);
  mir.graph <- simplify(delete.vertices(mir.graph, nodes2rm));
  current.msg <<- paste("A total of", length(nodes2rm) , "was reduced.");
  substats <- DecomposeGraph(mir.graph, 2);
  if(!is.null(substats)){
    mir.graph <<- mir.graph;
    return(c(nrow(mSetObj$dataSet$mir.orig), length(unique(mSetObj$dataSet$mir.filtered[,"ID1"])), length(unique(mSetObj$dataSet$mir.filtered[,"ID2"])), ecount(mir.graph), length(mir.nets), substats));
  }else{
    return(0);
  }
}

FilterNetByList <- function(net.type, ids, id.type, remove){
  mSetObj <- .get.mSet(mSetObj);
  
  lines <- strsplit(ids, "\r|\n|\r\n")[[1]];
  lines<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", lines, perl=TRUE);
  nms.vec = unique(unlist(mSetObj$dataSet$mir.res[, c(1,2,3,4)]))
  # need to first convert to correct id used in the graph
  hit.inx <- nms.vec %in% lines;
  nodes2rm = nms.vec[hit.inx]
  
  if(remove== "true"){
    nodes2rm <- nodes2rm[nodes2rm %in% V(mir.graph)$name];    # make sure they are in the igraph object
  }else{
    nodes2rm <- V(mir.graph)$name[!(V(mir.graph)$name %in% nodes2rm)];    # make sure they are in the igraph object
  }
  mir.graph <- simplify(delete.vertices(mir.graph, nodes2rm));
  current.msg <<- paste("A total of", length(nodes2rm) , "was reduced.");
  substats <- DecomposeGraph(mir.graph, 2);
  if(!is.null(substats)){
    mir.graph <<- mir.graph;
    return(c(nrow(mSetObj$dataSet$mir.orig), length(unique(mSetObj$dataSet$mir.filtered[,1])), length(unique(mSetObj$dataSet$mir.filtered[,3])), ecount(mir.graph), length(mir.nets), substats));
  }else{
    return(0);
  }
}

PrepareGraphML <- function(net.nm){
  write.graph(mir.nets[[net.nm]], file=paste(net.nm, ".graphml", sep=""), format="graphml");
}

PrepareNet <- function(mir.nm, file.nm){
  my.mirnet <- mir.nets[[mir.nm]];
  current.mirnet <<- my.mirnet;
  convertIgraph2JSON(my.mirnet, file.nm);
  if(.on.public.web){
    return(1);
  }else{
    return(paste("Network files are downloaded!"))
  }
}

PerformLayOut <- function(g, layers, algo, focus=""){
  vc <- vcount(g);
  if(algo == "Default"){
    if(vc > 1000) {
      # pos.xy <- layout.fruchterman.reingold(g, area=30*vc^2);
      pos.xy <- layout.lgl(g);
    }else if(vc < 100){
      pos.xy <- layout.kamada.kawai(g);
    }else{
      pos.xy <- layout.fruchterman.reingold(g, area=40*vc^2);
    }
  }else if(algo == "FrR"){
    pos.xy <- layout.fruchterman.reingold(g, area=34*vc^2);
  }else if(algo == "circle"){
    pos.xy <- layout.circle(g);
  }else if(algo == "random"){
    pos.xy <- layout.random(g);
  }else if(algo == "lgl"){
    pos.xy <- layout.lgl(g);
  }else if(algo == "gopt"){
    pos.xy <- layout.graphopt(g)
  }else if(algo == "circular_tripartite"){
    library(ggforce)
    l <- layout_with_sugiyama(g, layers = V(g)$group*(vc/3) +30)
    layout <- l$layout
    
    radial <- radial_trans(
      r.range = rev(range(layout[,2])),
      a.range = range(layout[,1]),
      offset = 0
    )
    coords <- radial$transform(layout[,2], layout[,1])
    layout[,1] <- coords$x
    layout[,2] <- coords$y
    pos.xy= layout
  }else if(algo == "tripartite"){
    l <- layout_with_sugiyama(g, layers = V(g)$layers*(vc/4))
    pos.xy <- -l$layout[,2:1]
  }else if(algo == "concentric"){
    library(graphlayouts)
    # the fist element in the list for concentric is the central node.
    if(focus==""){
      inx=1;
    }else{
      inx = which(V(g)$name == focus)
    }
    coords <- layout_with_focus(g,inx)
    pos.xy <- coords$xy
  }else if(algo == "backbone"){
    library(graphlayouts)
    if(length(V(g)$name)<2000){
      coords = layout_with_stress(g)
      pos.xy = coords
    }else{
      coords = layout_with_sparse_stress(g,pivots=100)
      pos.xy = coords
    }
    
  }else if(algo == "mds"){
    library(graphlayouts)
    coords = layout_with_pmds(g,length(V(g)$name)/10)
    pos.xy = coords/100
    rownames(pos.xy) = NULL
  }
  pos.xy;
}

UpdateNetworkLayout <- function(algo, filenm, focus){
  # get layers
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$dataSet$mir.res;
  my.nodes <- res[, c(1, 2)];
  
  m <- as.matrix(my.nodes);
  layers = ceiling(match(V(current.mirnet)$name, m)/nrow(m));
  
  pos.xy <- PerformLayOut(current.mirnet, layers, algo, focus);
  nms <- V(current.mirnet)$name;
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i],
      x=pos.xy[i,1],
      y=pos.xy[i,2]
    );
  }
  # now only save the node pos to json
  library(RJSONIO);
  netData <- list(nodes=nodes);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(filenm);
}

#source  num_targets targets
#     A            2  [B, C]
#     B            1     [C]
#     C            1     [D]
#     D            1     [A]
GetSummaryRow <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$net.summary$target;
}

GetSummaryTargetNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$net.summary$num_targets;
}

GetSummaryTargets <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  print(head(mSetObj$analSet$net.summary));
  mSetObj$analSet$net.summary$targets;
}


GetNetNames <- function(){
  rownames(net.stats);
}

GetNetStats <- function(){
  as.matrix(net.stats);
}

GetNetsNameString <- function(){
  paste(rownames(net.stats), collapse="||");
}

UpdateSubnetStats <- function(){
  old.nms <- names(mir.nets);
  net.stats <- ComputeSubnetStats(mir.nets);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  rownames(net.stats) <- old.nms[ord.inx];
  net.stats <<- net.stats;
}

ComputeSubnetStats <- function(comps){
  mSetObj <- .get.mSet(mSetObj);
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  colnames(net.stats) <- c("Node", "Edge", "Query");
  queries <- rownames(mSetObj$dataSet$mir.mapped);
  for(i in 1:length(comps)){
    g <- comps[[i]];
    net.stats[i,] <- c(vcount(g),ecount(g),sum(queries %in% V(g)$name));
  }
  return(net.stats);
}

# from to should be valid nodeIDs
GetShortestPaths <- function(from, to){
  
  paths <- get.all.shortest.paths(current.mirnet, from, to)$res;
  if(length(paths) == 0){
    return (paste("No connection between the two nodes!"));
  }
  
  path.vec <- vector(mode="character", length=length(paths));
  for(i in 1:length(paths)){
    path.inx <- paths[[i]];
    path.ids <- V(current.mirnet)$name[path.inx];
    #path.sybls <- V(current.mirnet)$Label[path.inx];
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

ExtractNetModule<- function(nodeids){
  set.seed(8574);
  nodes <- strsplit(nodeids, ";")[[1]];
  g <- current.mirnet;
  
  # try to see if the nodes themselves are already connected
  hit.inx <- V(g)$name %in% nodes;
  gObj <- induced.subgraph(g, V(g)$name[hit.inx]);
  
  # now find connected components
  comps <-decompose.graph(gObj, min.vertices=1);
  
  if(length(comps) == 1){ # nodes are all connected
    g <- comps[[1]];
  }else{
    # extract modules
    paths.list <-list();
    sd.len <- length(nodes);
    for(pos in 1:sd.len){
      paths.list[[pos]] <- get.shortest.paths(g, nodes[pos], nodes[-(1:pos)])$vpath;
    }
    nds.inxs <- unique(unlist(paths.list));
    nodes2rm <- V(g)$name[-nds.inxs];
    g <- simplify(delete.vertices(g, nodes2rm));
  }
  nodeList <- get.data.frame(g, "vertices");
  if(nrow(nodeList) < 3){
    return ("NA");
  }
  module.count <- module.count + 1;
  module.nm <- paste("module", module.count, sep="");
  
  colnames(nodeList) <- c("Id", "Label");
  ndFileNm = "mgwas_node_list.csv";
  write.csv(nodeList, file=ndFileNm, row.names=F, quote=F);
  
  edgeList <- get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  edgFileNm = "mgwas_edge_list.csv";
  write.csv(edgeList, file=edgFileNm, row.names=F, quote=F);
  
  filenm <- paste(module.nm, ".json", sep="");
  convertIgraph2JSON(g, filenm);
  
  # record the module
  mir.nets[[module.nm]] <<- g;
  UpdateSubnetStats();
  module.count <<- module.count
  return (filenm);
}


# exclude nodes in current.net (networkview)
ExcludeNodes <- function(nodeids, filenm){
  nodes2rm <- strsplit(nodeids, ";")[[1]];
  current.mirnet <- delete.vertices(current.mirnet, nodes2rm);
  
  # need to remove all orphan nodes
  bad.vs<-V(current.mirnet)$name[degree(current.mirnet) == 0];
  current.mirnet <<- delete.vertices(current.mirnet, bad.vs);
  
  # return all those nodes that are removed
  nds2rm <- paste(c(bad.vs, nodes2rm), collapse="||");
  
  # update topo measures
  node.btw <- as.numeric(betweenness(current.mirnet));
  node.dgr <- as.numeric(degree(current.mirnet));
  node.exp <- as.numeric(get.vertex.attribute(current.mirnet, name="abundance", index = V(current.mirnet)));
  nms <- V(current.mirnet)$name;
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i],
      expr = node.exp[i],
      degree=node.dgr[i],
      between=node.btw[i],
      expr = node.exp[i]
    );
  }
  # now only save the node pos to json
  library(RJSONIO);
  netData <- list(deletes=nds2rm,nodes=nodes);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(filenm);
}

FilterBipartiNet <- function(mSetObj=NA, nd.type, min.dgr, min.btw){
  #save.image("FilterBipartiNet.RData")
  mSetObj <- .get.mSet(mSetObj);
  overall.graph <- mir.graph;
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
  substats <- DecomposeGraph(overall.graph,2);
  if(.on.public.web){
    mSetObj <- .get.mSet(mSetObj);
    if(!is.null(substats)){
      mir.graph <<- overall.graph;
      return(c(nrow(mSetObj$dataSet$mir.orig),
               length(unique(mSetObj$dataSet$mir.filtered[,1])), 
               length(unique(mSetObj$dataSet$mir.filtered[,3])), 
               ecount(mir.graph), 
               length(mir.nets), 
               substats));
    }else{
      return(0);
    }
  }else{
    return(.set.mSet(mSetObj));
  }
}

convertIgraph2JSON <- function(g, filenm){
  #filenm<<-filenm;
  mSetObj <- .get.mSet(mSetObj);
  #qs::qsave(g,"g.qs");

  #save.image("net.RData");
  nms <- V(g)$name;
  nd.ids <- as.character(node.anot[nms]);

  res <- mSetObj$dataSet$mir.res;
  my.nodes <- res[, c(1, 2)];
  
  m <- as.matrix(my.nodes);
  layers = ceiling(match(V(g)$name, m)/nrow(m));
  
  # setup shape (mir square, gene circle)
  shapes <- rep("circle", length(nms));
  
  # get edge data
  edge.mat <- get.edgelist(g);
  edge.pmids <- igraph::edge_attr(g, "Reference");
  edge.p_values <- igraph::edge_attr(g, "EdgeAttr1");
  edge.n_pmids <- igraph::edge_attr(g, "EdgeAttr2");
  edge.type <- igraph::edge_attr(g, "EdgeType");
  edge.predicate <- igraph::edge_attr(g, "Predicate");
  if(!is.null(edge.pmids)){
  edge.sizes <- as.numeric(rescale2NewRange((edge.n_pmids), 0.5, 3));
  }else{
  edge.sizes <- rep("0.5", nrow(edge.mat));
  }
  
  if(!is.null(edge.pmids)){
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], pmids=edge.pmids, p_values=edge.p_values, 
                      n_pmids=edge.n_pmids, esize=edge.sizes, etype=edge.type);
  }else if(!is.null(edge.predicate)){
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], predicate=edge.predicate, esize=edge.sizes);
  }else{
   edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], type=rep("arrow", nrow(edge.mat)), esize=edge.sizes);
  }
  
  
  # now get coords
  pos.xy <- PerformLayOut(g, layers, "Default");
  
  node.btw <- as.numeric(betweenness(g));
  node.dgr <- as.numeric(degree(g));
  
  if(anal.type %notin% c("array", "rnaseq", "qpcr")){
    node.exp <- as.character(get.vertex.attribute(g, name="abundance", index = V(g)));
  }else{
    node.exp <- as.numeric(get.vertex.attribute(g, name="abundance", index = V(g)));
  }
  
  if(vcount(g) > 1000){
    minSize = 3;
  }else if(vcount(g) > 300){
    minSize = 4;
  }else{
    minSize = 5;
  }
  node.sizes <- as.numeric(rescale2NewRange((log(node.dgr))^2, minSize, 10));

  if(net.type=="snp2dis" || net.type=="dis2snp"){
    dis.inx <- nms %in% edge.mat[,3];
    met.inx <- NULL
  } else{
    met.inx <- nms %in% edge.mat[,3];
    dis.inx <- NULL
  }
  # }
  snp.inx <- nms %in% net.info$snp.nms;
  shapes[snp.inx] <- "square";
  dis.inx <- nms %in% net.info$dis.nms;
  shapes[dis.inx] <- "circle";
  met.inx <- nms %in% net.info$met.nms;
  shapes[met.inx] <- "circle";
  gene.inx <- nms %in% net.info$gene.nms;
  egene.inx <- nms %in% net.info$egene.nms;
  prot.inx <- nms %in% net.info$prot.nms;
  shapes[gene.inx] <- "diamond";
  shapes[egene.inx] <- "diamond";
  shapes[prot.inx] <- "diamond";
  drug.inx <- nms %in% net.info$drug.nms;
  shapes[drug.inx] <- "diamond";
  exp.inx <- nd.ids %in% net.info$exp.ids;
  out.inx <- nd.ids %in% net.info$out.ids;
  overlap.inx <- nd.ids %in% net.info$overlap.ids;
  expsbj.inx <- nd.ids %in% net.info$expsbj.ids;
  outobj.inx <- nd.ids %in% net.info$outobj.ids;
  #lit.inx <- nd.ids %in% net.info$lit.ids;
  exp.final.inx <- exp.inx & !overlap.inx & !expsbj.inx
  out.final.inx <- out.inx & !overlap.inx & !outobj.inx
  

  containsGP <- any(gene.nms %in% prot.nms) ## check if any overlap between gene and protein
  cat("containsGP = ", containsGP, "\n")

  if(substring(anal.type, 1,3) == "snp"){ #highlight SNP if they are the query
    node.sizes[snp.inx] <- node.sizes[snp.inx] + 1;
  }else{
    node.sizes[snp.inx] <- node.sizes[snp.inx] + 0.4;
  }
  node.sizes[gene.inx] <- node.sizes[gene.inx] + 1;
  node.sizes[egene.inx] <- node.sizes[egene.inx] + 1;
  node.sizes[prot.inx] <- node.sizes[prot.inx] + 1;
  node.sizes[drug.inx] <- node.sizes[drug.inx] + 1;
  # diamond shape is too small, make it slightly bigger
  node.types <- rep("", length(node.dgr));
  
  node.types[snp.inx] <-  paste("SNP", node.types[snp.inx]);
  node.types[met.inx] <-  paste("Metabolite", node.types[met.inx]);
  node.types[gene.inx] <-  paste("Gene", node.types[gene.inx]);
  node.types[egene.inx] <-  paste("eGene", node.types[egene.inx]);
  node.types[prot.inx] <-  paste("Protein", node.types[prot.inx]);
  node.types[dis.inx] <-  paste("Disease", node.types[dis.inx]);
  node.types[drug.inx] <-  paste("Drug", node.types[drug.inx]);
  node.types[exp.final.inx] <-  paste("Exposure", node.types[exp.final.inx]);
  node.types[out.final.inx] <-  paste("Outcome", node.types[out.final.inx]);
  node.types[overlap.inx] <-  paste("Overlap", node.types[overlap.inx]);
  node.types[expsbj.inx] <-  paste("Exposure_Subject", node.types[expsbj.inx]);
  node.types[outobj.inx] <-  paste("Outcome_Object", node.types[outobj.inx]);
  
  n.types <- rep("", length(node.dgr));
  n.types[snp.inx] <- "SNP";
  n.types[met.inx] <- "Metabolite";
  n.types[dis.inx] <- "Disease";
  n.types[gene.inx] <- "Gene";
  n.types[egene.inx] <- "eGene";
  n.types[prot.inx] <- "Protein";
  n.types[drug.inx] <- "Drug";
  n.types[exp.final.inx] <- "Exposure";
  n.types[out.final.inx] <- "Outcome";
  n.types[overlap.inx] <- "Overlap";
  n.types[expsbj.inx] <- "Exposure_Subject";
  n.types[outobj.inx] <- "Outcome_Object";
  
  node.types <-  trimws(node.types);
  # this is the same as types in omicsnet
  #node.types <-  gsub(" ", "_", node.types);
  
  node.cols <- rep("#ff4500", length(node.dgr));
  ntype <- unique(n.types)
  color.vec = gg_color_hue(length(ntype))
  for(i in 1:length(ntype)){
    node.cols[which(n.types ==ntype[i])]=color.vec[i]
  }
  
  node.cols[snp.inx] <- "#306EFF"; # dark blue
  # update mir node color
  topo.colsw <- node.cols;
  node.cols[snp.inx] <- "#98F5FF";
  topo.colsb <- node.cols;
  
  freq <- table(node.types)
  
  duplicated.types <- node.types
  for(i in 1:length(unique(node.types))){
    duplicated.types[duplicated.types == names(freq[i])]=order(freq)[i]
  }
  
  duplicated.types <- as.numeric(duplicated.types)
  V(g)$layers = duplicated.types

  V(g)$group = as.numeric(duplicated.types); #concentric circle
  
  
  node.cols[snp.inx] <- "#306EFF"; # dark blue
  # update mir node color
  topo.colsw <- node.cols;
  node.cols[snp.inx] <- "#98F5FF";
  topo.colsb <- node.cols;
  # color based on expression
  bad.inx <- is.na(node.exp) | node.exp==0;
  if(!all(bad.inx)){
    exp.val <- node.exp;
    node.colsb.exp <- getExpColors(node.exp, c("#78ff4d", "#FA8072", "#ebebeb"));
    node.colsw.exp <- getExpColors(node.exp, c("#269b00", "#b30000", "#333333"));
    node.colsb.exp[bad.inx] <- "#d3d3d3";
    node.colsw.exp[bad.inx] <- "#c6c6c6";
  }else{
    node.colsb.exp <- rep("#d3d3d3",length(node.exp));
    node.colsw.exp <- rep("#c6c6c6",length(node.exp));
  }
  
  topo.colsw[snp.inx] <- "#bcbd22";
  topo.colsw[met.inx] <- "#2ca02c";
  topo.colsw[gene.inx] <- "#1f77b4";
  topo.colsw[prot.inx] <- "#e377c2";
  topo.colsw[egene.inx] <- "#d62728"; 
  topo.colsw[dis.inx] <- "#9467bd"; 

  topo.colsw[exp.final.inx] <- "#bcbd22";
  topo.colsw[out.final.inx] <- "#2ca02c";
  topo.colsw[overlap.inx] <- "#e49444"; # 2
  topo.colsw[expsbj.inx] <- "#5778a4";  # 1
  topo.colsw[outobj.inx] <- "#6a9f58";  # 3
  
  topo.colsb <- topo.colsw;
  colVec  <- unique(topo.colsw);

  # if(containsGP){
  #   topo.colsb[gene.inx] <- "#D3D3D3";
  #   topo.colsw[gene.inx] <- "#D3D3D3";
  #   color.vec <- c("#D3D3D3", "#FF8484", "#39FF14","#00f6ff", "#D3D3D3", "#00ffff", "#ffff00", "#ff9900", "#39FF14");
  # }else{
  #   color.vec <- c("#FF8484", "#FF8484", "#39FF14","#00f6ff", "#D3D3D3", "#00ffff", "#ffff00", "#ff9900", "#39FF14");
  # }
  
  seed.nms <- unique(rownames(mSetObj$dataSet$mir.mapped)[!is.na(rownames(mSetObj$dataSet$mir.mapped))])
  seed.inx <- nms %in% seed.nms;
  seed_arr <- rep("notSeed",length(node.dgr));
  seed_arr[seed.inx] <- "seed";
  
  # now create the json object
  nodes <- vector(mode="list");
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id=nms[i],
      label=nms[i],
      size=node.sizes[i],
      molType=node.types[i],
      type=shapes[i],
      seedArr =seed_arr[i],
      url=nd.ids[i],
      colorb=topo.colsb[i],
      colorw=topo.colsw[i],
      color=topo.colsw[i],
      x=pos.xy[i,1],
      y=pos.xy[i,2],
      attributes=list(
        expr = node.exp[i],
        expcolb=node.colsb.exp[i],
        expcolw=node.colsw.exp[i],
        degree=node.dgr[i], # actual degree in orginal network
        between=node.btw[i])
    );
  }
  
  current.mirnet <<- g
  # save node table
  nd.tbl <- data.frame(Id=nms, Label=nms, Degree=node.dgr, Betweenness=node.btw);
  fileNm <- paste("node_table_", substring(filenm, 0, nchar(filenm)-5), ".csv", sep="")
  fast.write.csv(nd.tbl, file=fileNm, row.names=FALSE);
  
  # covert to json
  library(RJSONIO);
  edge.color  <- rep("#d3d3d3",nrow(edge.mat));
  # perhaps show edge color based on beta 
  up.inx <- E(g)$direction == "+";
  down.inx <- E(g)$direction == "-";
  edge.color[up.inx] = "#FF0000" #red
  edge.color[down.inx] = "#11679A" #blue
  
  edge.mat  <- cbind(edge.mat, color=edge.color);
  netData <- list(mirnet=net.type, 
                  nodes=nodes, 
                  edges=edge.mat, 
                  nodeColors = colVec, 
                  prot.nms=prot.nms,
                  gene.nms=gene.nms, 
                  snp.nms=snp.nms, 
                  containsGP=containsGP,
                  nodeTypes=ntype);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  
  # also save to GraphML
  write.graph(g, file="mgwas.graphml", format="graphml");
  
  if(!.on.public.web){
    library(httr);
    r <- POST("localhost:8080/miRNet/faces/R_REQUEST?type=network", body = list(organism = data.org, idtype = "entrez", network = toJSON(netData))) 
    #TO-DO: need to check org and idtype
  }
}

# return information based on node type
GetNetStatByType <- function(g){
   # qs::qsave(g,"g.qs")
   # save.image("GetNetStatByType.RData")
  mSetObj <- .get.mSet(mSetObj);
  nd.queries <- V(g)$name;
  uniq.ins <- unique(rownames(mSetObj$dataSet$mir.mapped));
  sd.queries <- uniq.ins[uniq.ins %in% nd.queries];

  my.stat <- list(
    query.num = length(sd.queries),
    node.num = length(V(g)$name)
  );
  return(my.stat);
}

GetQueryNum <-function(){
  if(net.type=="metabo_phenotypes"){
    return(as.numeric(net.stats$Query))
  }
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$query.nums)
}

GetTypeNum <-function(){
  if(net.type=="metabo_phenotypes"){
    return(as.numeric(net.stats$Node))
  }
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$type.nums)
}

PrepareCSV <- function(table.nm){
  # table.nm<<-table.nm;
  # save.image("PrepareCSV.RData")
  mSetObj <- .get.mSet(mSetObj);
  if(table.nm=="mr_res_single"){
    fast.write.csv(mSetObj$dataSet$mr_res_single, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res_loo"){
    fast.write.csv(mSetObj$dataSet$mr_res_loo, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res"){
    fast.write.csv(mSetObj$dataSet$mr_results, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="manhattan"){
    fast.write.csv(mSetObj$dataSet$snp2met, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else if(anal.type == "multilist" || anal.type == "snp2mir" || anal.type == "tf2genemir" || anal.type == "gene2tfmir"){
    fast.write.csv(mSetObj$dataSet[[table.nm]], file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else {
    fast.write.csv(mSetObj$dataSet$snp.res, file=paste(net.type, ".csv", sep=""), row.names = FALSE);
  }
}

GetTableNames <- function(){
  #save.image("GetTableNames.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mirtable;
}

GetSeedsColumn <- function(mSetObj=NA){
  #save.image("GetSeedsColumn.RData")
  mSetObj <- .get.mSet(mSetObj);
  tbls = unique(mSetObj$dataSet$mirtable);
  vec = vector();
  print(tbls);
  for( i in 1:length(tbls)){
    nms = strsplit(tbls[i], "2")[[1]];
    orignms = nms;
    nms= gsub("lit", "Literature",nms);
    nms= gsub("snp", "SNP",nms)
    nms= gsub("gene_eqtl", "eGene",nms);
    nms= gsub("gene", "Gene",nms)
    nms= gsub("met_study", "Metabolite",nms)
    nms= gsub("met", "Metabolite",nms)
    nms= gsub("dis", "Disease",nms)
    nms= gsub("pos", "Positional Mapping",nms)
    nms= gsub("snp_pos", "Positional Mapping",nms)
    nms= gsub("drug", "Drug",nms);
    nms= gsub("protein", "Protein",nms); # this is for ppi
    nms= gsub("prot", "Protein",nms); # this is for snp2protein
    nms= gsub("mr", "MR",nms);
    
    if(tbls[i] %in% c("protein2protein")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])) + length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])) )
    }else if(tbls[i] %in% c("snp2gene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,9])))
    }else if(tbls[i] %in% c("snp2egene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])))
    }else if(tbls[i] %in% c("snp2prot")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c("gene2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,6])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("snp2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("dis2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c("met2gene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,5])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])))
    }else if(tbls[i] %in% c("gene2met")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,5])))
    }else if(tbls[i] %in% c("gene2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("met2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])))
    }else if(tbls[i] %in% c("snp2met")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c( "met2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c( "snp2met_study")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(orignms[1] == "dis"){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])))
    }else if(tbls[i] %in% c("mr2lit")){
      vec[i]=paste0(nms[1],":" ,length(unique(c(mSetObj$dataSet[tbls[i]][[1]][,1],mSetObj$dataSet[tbls[i]][[1]][,11]))),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,6])))
    }else{
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }
  }
  return(vec)
}

PlotDegreeHistogram <- function(imgNm, netNm = "NA", dpi=72, format="png"){
  require('Cairo');
  require('igraph');
  mSetObj <- .get.mSet(mSetObj);
  dpi<-as.numeric(dpi)
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  Cairo(file=imgNm, width=400, height=400, type="png", bg="white");
  library(ggplot2)
  # if(netNm != "NA"){
    overall.graph <- mir.graph;
  # }
  G.degrees <- degree(overall.graph)
  
  G.degree.histogram <- as.data.frame(table(G.degrees))
  G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
  
  p <- ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
    geom_point() +
    scale_x_continuous("Degree\n(nodes containing that amount of connections)",
                       breaks = c(1, 3, 10, 30, 100, 300),
                       trans = "log10") +
    scale_y_continuous("Frequency\n(number of nodes)",
                       breaks = c(1, 3, 10, 30, 100, 300, 1000),
                       trans = "log10") +
    ggtitle("Degree Distribution (log-log)") +
    theme_bw()  +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
  dev.off();
}

PlotBetweennessHistogram <- function(imgNm, netNm = "NA",dpi=72, format="png"){
  require('Cairo');
  require('igraph');
  dpi<-as.numeric(dpi)
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  Cairo(file=imgNm, width=400, height=400, type="png", bg="white");
  library(ggplot2)
  #if(netNm != "NA"){
    overall.graph <- mir.graph;
  #}
  G.degrees <- betweenness(overall.graph)
  
  G.degree.histogram <- as.data.frame(table(G.degrees))
  G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
  
  p <- ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
    geom_point() +
    scale_x_continuous("Betweenness\n(nodes with that amount of betweenness)",
                       breaks = c(1, 3, 10, 30, 100, 300,1000,3000,10000,30000),
                       trans = "log10") +
    scale_y_continuous("Frequency\n(number of nodes)",
                       breaks = c(1, 3, 10, 30, 100, 300, 1000),
                       trans = "log10") +
    ggtitle("Betweenness Distribution (log-log)") +
    theme_bw()  +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
  dev.off();
}

GetNetworkTopology <- function(netnm){
  #save.image("GetNetworkTopology.RData")
  g <- mir.graph;
  globalProperties <-list();
  globalProperties[["Diameter"]] <-diameter(g);
  globalProperties[["Radius"]] <-radius(g);
  globalProperties[["Average path length"]] <-signif(mean_distance(g), 3);
  globalProperties[["Clustering coefficient"]] <- signif(transitivity(g, type="global"), 3);
  propertiesVector <- c(globalProperties[[1]], globalProperties[[2]], globalProperties[[3]], globalProperties[[4]]);
  #print(propertiesVector);
  return(propertiesVector);
}

# support walktrap, infomap and lab propagation
FindCommunities <- function(method="walktrap", use.weight=FALSE){
  mSetObj <- .get.mSet(mSetObj);
  
  library(igraph)
  # make sure this is the connected
  current.net <- current.mirnet
  g <- current.net;
  if(!is.connected(g)){
    g <- decompose.graph(current.net, min.vertices=2)[[1]];
  }
  total.size <- length(V(g));
  
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
  
  for(i in 1:length(communities)){
    # update for igraph 1.0.1
    path.ids <- communities[[i]];
    psize <- length(path.ids);
    if(psize < 5){
      next; # ignore very small community
    }
    hits <- mSetObj$dataSet$seeds %in% path.ids;
    qnums <- sum(hits);
    if(qnums == 0){
      next; # ignor community containing no queries
    }
    
    rowcount <- rowcount + 1;
    pids <- paste(path.ids, collapse="->");
    ##path.sybls <- V(g)$name[path.inx];
    #path.sybls <- sybls[path.ids];
    com.mat <- cbind(path.ids, path.ids, rep(i, length(path.ids)));
    gene.community <- rbind(gene.community, com.mat);
    qnum.vec <- c(qnum.vec, qnums);
    
    # calculate p values (comparing in- out- degrees)
    #subgraph <- induced.subgraph(g, path.inx);
    subgraph <- induced.subgraph(g, path.ids);
    in.degrees <- degree(subgraph);
    #out.degrees <- degree(g, path.inx) - in.degrees;
    out.degrees <- degree(g, path.ids) - in.degrees;
    ppval <- wilcox.test(in.degrees, out.degrees)$p.value;
    ppval <- signif(ppval, 3);
    pval.vec <- c(pval.vec, ppval);
    
    # calculate community score
    community.vec[rowcount] <- paste(c(psize, qnums, ppval, pids), collapse=";");
  }
  
  ord.inx <- order(pval.vec, decreasing=F);
  community.vec <- community.vec[ord.inx];
  qnum.vec <- qnum.vec[ord.inx];
  ord.inx <- order(qnum.vec, decreasing=T);
  community.vec <- community.vec[ord.inx];
  
  all.communities <- paste(community.vec, collapse="||");
  colnames(gene.community) <- c("Id", "Label", "Module");
  fast.write.csv(gene.community, file="module_table.csv", row.names=F);
  return(all.communities);
}

GetMinConnectedGraphs <- function(net.type, max.len = 200){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  set.seed(8574);
  # first get shortest paths for all pair-wise seeds
  my.seeds <- unique(dataSet$seeds);
  sd.len <- length(my.seeds);
  paths.list <-list();
  
  # first trim mir.graph to remove no-seed nodes of degree 1
  dgrs <- degree(mir.graph);
  keep.inx <- dgrs > 1 | (names(dgrs) %in% my.seeds);
  nodes2rm <- V(mir.graph)$name[!keep.inx];
  mir.graph <-  simplify(delete.vertices(mir.graph, nodes2rm));
  
  # need to restrict the operation b/c get.shortest.paths is very time consuming
  # for top max.len highest degrees
  if(sd.len > max.len){
    hit.inx <- names(dgrs) %in% my.seeds;
    sd.dgrs <- dgrs[hit.inx];
    sd.dgrs <- rev(sort(sd.dgrs));
    # need to synchronize all (dataSet$seeds) and top seeds (my.seeds)
    dataSet$seeds <- names(sd.dgrs);
    my.seeds <- dataSet$seeds[1:max.len];
    sd.len <- max.len;
    current.msg <<- paste("The minimum connected network was computed using the top", sd.len, "seed nodes in the network based on their degrees.");
  }else{
    current.msg <<- paste("The minimum connected network was computed using all seed nodes in the network.");
  }
  # now calculate the shortest paths for
  # each seed vs. all other seeds (note, to remove pairs already calculated previously)
  for(pos in 1:sd.len){
    paths.list[[pos]] <- get.shortest.paths(mir.graph, my.seeds[pos], my.seeds[-(1:pos)])$vpath;
  }
  nds.inxs <- unique(unlist(paths.list));
  nodes2rm <- V(mir.graph)$name[-nds.inxs];
  g <- simplify(delete.vertices(mir.graph, nodes2rm));
  
  nodeList <- get.data.frame(g, "vertices");
  nodeList <- as.data.frame(nodeList[, 1]);
  colnames(nodeList) <- c("ID");
  fast.write.csv(nodeList, file="orig_node_list.csv", row.names=F);
  
  edgeList <- get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  fast.write.csv(edgeList, file="orig_edge_list.csv", row.names=F);
  
  path.list <- NULL;
  substats <- DecomposeGraph(g, 2);
  if(!is.null(substats)){
    mir.graph <<- g;
    return(c(nrow(dataSet$mir.orig), length(unique(dataSet$mir.filtered[,1])), length(unique(dataSet$mir.filtered[,3])), ecount(mir.graph), length(mir.nets), substats));
  }else{
    return(0);
  }
}

ComputePCSFNet <- function(){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  
  edg <- as.data.frame(get.edgelist(mir.graph));
  edg$V3 <- rep(1, nrow(edg));
  colnames(edg) <- c("from", "to", "cost");
  
  node_names <- unique(c(as.character(edg[,1]),as.character(edg[,2])))
  ppi <- graph.data.frame(edg[,1:2],vertices=node_names,directed=F)
  E(ppi)$weight <- as.numeric(edg[,3])
  ppi <- simplify(ppi)
  
  mapped.seeds <- unique(dataSet$seeds[dataSet$seeds %in% V(ppi)$name]);
  
  if(length(mapped.seeds) < 2){
    current.msg <<- "PCSF requires at least two seed nodes!";
    return(0);
  }
  
  expr.vec <- rep(1, length(mapped.seeds ));
  names(expr.vec) <- mapped.seeds;
  
  
  g <- Compute.SteinerForest(ppi, expr.vec, w = 5, b = 100, mu = 0.0005);
  
  nodeList <- get.data.frame(g, "vertices");
  colnames(nodeList) <- c("Id", "Label");
  write.csv(nodeList, file="orig_node_list.csv", row.names=F, quote=F);
  
  edgeList <- get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  write.csv(edgeList, file="orig_edge_list.csv", row.names=F, quote=F);
  
  path.list <- NULL;
  substats <- DecomposeGraph(g, 2);
  if(!is.null(substats)){
    mir.graph <<- g;
    return(c(nrow(dataSet$mir.orig), length(unique(dataSet$mir.filtered[,1])), nrow(nodeList), nrow(edgeList), length(mir.nets), substats));
  }else{
    current.msg <<- "PCSF failed! Please try other methods!";
    return(0);
  }
}

# Adapted from PCSF
# https://github.com/IOR-Bioinformatics/PCSF
Compute.SteinerForest <- function(ppi, terminals, w = 2, b = 1, mu = 0.0005, dummies){
  
  # Gather the terminal genes to be analyzed, and their scores
  terminal_names <- names(terminals)
  terminal_values <- as.numeric(terminals)
  
  # Incorporate the node prizes
  node_names <- V(ppi)$name
  node_prz <- vector(mode = "numeric", length = length(node_names))
  index <- match(terminal_names, node_names)
  percent <- signif((length(index) - sum(is.na(index)))/length(index)*100, 4)
  if (percent < 5){
    print("Less than 1% of your terminal nodes are matched in the interactome!");
    return(NULL);
  }
  paste0("  ", percent, "% of your terminal nodes are included in the interactome\n");
  terminal_names <- terminal_names[!is.na(index)]
  terminal_values <- terminal_values[!is.na(index)]
  index <- index[!is.na(index)]
  node_prz[index] <-  terminal_values
  
  if(missing(dummies)||is.null(dummies)||is.na(dummies)){
    dummies <- terminal_names #re-assign this to allow for input
  }
  
  ## Prepare input file for MST-PCSF implementation in C++
  
  # Calculate the hub penalization scores
  node_degrees <- igraph::degree(ppi)
  hub_penalization <- - mu*node_degrees
  
  # Update the node prizes
  node_prizes <- b*node_prz
  index <- which(node_prizes==0)
  node_prizes[index] <- hub_penalization[index]
  
  # Construct the list of edges
  edges <- ends(ppi,es = E(ppi))
  from <- c(rep("DUMMY", length(dummies)), edges[,1])
  to <- c(dummies, edges[,2])
  
  cost <- c(rep(w, length(dummies)), E(ppi)$weight)
  
  #PCSF will faill if there are NAs in weights, this will check and fail gracefully
  if(any(is.na(E(ppi)$weight))){
    print("NAs found in the weight vector!");
    return (NULL);
  }
  
  ## Feed the input into the PCSF algorithm
  output <- XiaLabCppLib::call_sr(from,to,cost,node_names,node_prizes)
  
  # Check the size of output subnetwork and print a warning if it is 0
  if(length(output[[1]]) != 0){
    
    # Contruct an igraph object from the MST-PCSF output
    e <- data.frame(output[[1]], output[[2]], output[[3]])
    keep.inx <- which(e[,2]!="DUMMY");
    e <- e[keep.inx, ]
    
    names(e) <- c("from", "to", "weight")
    
    # Differentiate the type of nodes
    type <- rep("Steiner", length(output[[4]]))
    index <- match(terminal_names, output[[4]])
    index <- index[!is.na(index)]
    type[index] <- "Terminal"
    
    v <- data.frame(output[[4]], output[[5]], type)
    names(v) <- c("terminals", "prize", "type")
    subnet <- graph.data.frame(e,vertices=v,directed=F)
    E(subnet)$weight <- as.numeric(output[[3]][keep.inx])
    subnet <- delete_vertices(subnet, "DUMMY")
    subnet <- delete_vertices(subnet, names(which(degree(subnet)==0)));
    return(subnet)
    
  } else{
    print("Subnetwork can not be identified for a given parameter set")
    return(NULL);
  }
}

CreateGraph <- function(mSetObj=NA, net.type){
  net.type<<-net.type; # necessary for table stats
  mSetObj <- .get.mSet(mSetObj);
  query.type <- mSetObj$analSet$type;
  print(paste0(query.type, "===========query.type"))

  if(.on.public.web){
    load_igraph()
  }
  
  res <- mSetObj$dataSet$mir.res;
  print(head(res))

    my.edges <- res[,c(1,3)];
    my.edges <- as.data.frame(my.edges);
    nd.nms <- c(res[, 1], res[, 3]);
    nd.ids <- c(res[, 2], res[, 4]);
    names(nd.ids) <- nd.nms;
    dups <- duplicated(nd.ids); #note using unique will lose the names attribute
    node.anot <<- nd.ids[!dups];
    colnames(my.edges) = c("from", "to");

  if(query.type=="studyview"){
    my.edges <- as.data.frame(res[, c(1,3,6)]); # name1, name2, and p-value
    if(nrow(my.edges) > 10000){
      top.edge <- sort(my.edges$P_value)[1:10000];
      top.inx <- match(my.edges$P_value, top.edge);
      my.edges <- my.edges[!is.na(top.inx), ,drop=F];
    }
    mir.graph <-simplify( graph_from_data_frame(my.edges, directed=FALSE, vertices=NULL), edge.attr.comb="first");
  }else if(query.type=="mgwas"){

    my.edges <- as.data.frame(res[, c(1,3,5)]); # name1, name2, and predicate
    mir.graph <-simplify( graph_from_data_frame(my.edges, directed=FALSE, vertices=NULL), edge.attr.comb="first");
    
  }else{
    my.edges <- as.data.frame(res[, c(1,3,5:8)]);
    mir.graph <-simplify( graph_from_data_frame(my.edges, directed=FALSE, vertices=NULL), edge.attr.comb="first");
  }
  
  substats <- DecomposeGraph(mir.graph, 2);
  if(!is.null(substats)){
    mir.graph <<- mir.graph;
    mir.query <- nrow(mSetObj$dataSet$mir.mapped);
    #mir.query <- nrow(dataSet$mir.orig); #original query
    mir.count <- length(unique(my.edges[,1]));#matched mir
    tgt.count <- length(unique(my.edges[,2]));#matched target
    if(.on.public.web){
      mSetObj <- .get.mSet(mSetObj);
      return(c(mir.query, mir.count, tgt.count, ecount(mir.graph), length(mir.nets), substats));
    }else{
      return(0)
    }
  }else{
    return(.set.mSet(mSetObj));
  }
  
}


FilterNetByPval <- function(mSetObj=NA, pvalThresh){
  mSetObj <- .get.mSet(mSetObj);
  overall.graph <- mir.graph;
  all.nms <- V(overall.graph)$name;
  edge.mat <- get.edgelist(overall.graph);
  dgrs <- degree(overall.graph);
  nodes2rm.dgr <- nodes2rm.btw <- NULL;

  hit.inx <- rep(TRUE, length(all.nms));
  
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
  substats <- DecomposeGraph(overall.graph,2);
  if(.on.public.web){
    mSetObj <- .get.mSet(mSetObj);
    if(!is.null(substats)){
      mir.graph <<- overall.graph;
      return(c(nrow(mSetObj$dataSet$mir.orig),
               length(unique(mSetObj$dataSet$mir.filtered[,1])), 
               length(unique(mSetObj$dataSet$mir.filtered[,3])), 
               ecount(mir.graph), 
               length(mir.nets), 
               substats));
    }else{
      return(0);
    }
  }else{
    return(.set.mSet(mSetObj));
  }
}
