#'Update integrative pathway analysis for new input list
#'@description used for integrative analysis 
#'as well as general pathways analysis for meta-analysis results
#'@usage UpdateIntegPathwayAnalysis(mSetObj=NA, qids, file.nm, topo="dc", enrich="hyper", libOpt="integ")
#'@param mSetObj Input name of the created mSet Object
#'@param qids Input the query IDs
#'@param file.nm Input the name of the file
#'@param topo Select the mode for topology analysis: Degree Centrality ("dc") measures the number of links that connect to a node 
#'(representing either a gene or metabolite) within a pathway; Closeness Centrality ("cc") measures the overall distance from a given node 
#'to all other nodes in a pathway; Betweenness Centrality ("bc")measures the number of shortest paths from all nodes to all the others that pass through a given node within a pathway.
#'@param enrich Method to perform over-representation analysis (ORA) based on either hypergenometrics analysis ("hyper")
#' or Fisher's exact method ("fisher").
#'@param libOpt Select the different modes of pathways, either the gene-metabolite mode ("integ") which allows for joint-analysis
#' and visualization of both significant genes and metabolites or the gene-centric ("genetic") and metabolite-centric mode ("metab") which allows users
#' to identify enriched pathways driven by significant genes or metabolites, respectively. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
UpdateIntegPathwayAnalysis <- function(mSetObj=NA, qids, file.nm, topo="dc", enrich="hyper", libOpt="integ"){

  mSetObj <- .get.mSet(mSetObj);  
  
  libPath <- paste("kegg/jointpa/",libOpt,sep="");
  LoadKEGGLib(libPath, mSetObj$org);

  qids <- do.call(rbind, strsplit(qids, "; "));
  idtypes <- unlist(sapply(qids, function(x) substring(x, 1, 1) == "C"))
  qcmpds <- qids[idtypes]
  qgenes <- qids[!idtypes]
  
  set.size <- length(inmexpa$mset.list);
  ms.list <- lapply(inmexpa$mset.list, function(x){strsplit(x, " ")});
  current.universe <- unique(unlist(ms.list));

  # prepare for the result table
  res.mat<-matrix(0, nrow=set.size, ncol=7);
  rownames(res.mat)<-names(inmexpa$path.ids);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "Topology", "PVal.Z",  "Topo.Z");
  
  mSetObj$dataSet$pathinteg.method <- libOpt;
  mSetObj$dataSet$path.mat <- NULL;
  
  if(libOpt == "genetic"){
    gene.vec <- paste(mSetObj$org, ":", qgenes, sep="");
    ora.vec <- gene.vec;
    ora.vec.ids <- c(qgenes);
    uniq.count <- inmexpa$uniq.gene.count;
    uniq.len <- inmexpa$gene.counts;
    
  }else if(libOpt == "metab"){
    cmpd.vec <- paste("cpd:", qcmpds, sep="");
    ora.vec <- cmpd.vec;
    ora.vec.ids <- c(qcmpds);
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;

  }else{ # integ

    cmpd.vec <- paste("cpd:", qcmpds, sep="");
    gene.vec <- paste(mSetObj$org, ":", qgenes, sep="");
    ora.vec <- c(cmpd.vec, gene.vec);
    ora.vec.ids <- c(qcmpds, qgenes);
    
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;
    uniq.count <- uniq.count + inmexpa$uniq.gene.count;
    uniq.len <- uniq.len + inmexpa$gene.counts;
      
  }
  # need to cut to the universe covered by the pathways, not all genes 
  ora.vec <- ora.vec[ora.vec %in% current.universe]
  q.size <- length(ora.vec);
  # note, we need to do twice one for nodes (for plotting)
  # one for query for calculating, as one node can be associated with multiple matches
  # get the matched nodes on each pathway
  hits.path <- lapply(ms.list, function(x) {unlist(lapply(x, function(var){any(var%in%ora.vec);}),use.names=FALSE)});
  names(hits.path) <- inmexpa$path.ids;
  
  # get the matched query for each pathway
  hits.query <- lapply(ms.list, function(x) {ora.vec%in%unlist(x);});
  
  hit.num <- unlist(lapply(hits.query, function(x){sum(x)}), use.names=FALSE);
  
  if(sum(hit.num) == 0){
    AddErrMsg("No hits found for your input!");
    return(0);
  }
  
  set.num <- uniq.len;
  res.mat[,1] <- set.num;
  res.mat[,2] <- q.size*(set.num/uniq.count);
  res.mat[,3] <- hit.num;
  
  # use lower.tail = F for P(X>x)
  if(enrich=="hyper"){
    res.mat[,4] <- phyper(hit.num-1, set.num, uniq.count-set.num, q.size, lower.tail=F);
  }else if(enrich == "fisher"){
    res.mat[,4] <- GetFisherPvalue(hit.num, q.size, set.num, uniq.count);
  }else{
    AddErrMsg(paste("Not defined enrichment method:", enrich));
    return(0);
  }
  
  # toplogy test
  if(topo == "bc"){
    imp.list <- inmexpa$bc;
  }else if(topo == "dc"){
    imp.list <- inmexpa$dc;
  }else if(topo == "cc"){
    imp.list <- inmexpa$cc;       
  }else{
    AddErrMsg(paste("Not defined topology method:", topo));
    return(0);
  }
  
  # now, perform topological analysis		
  # calculate the sum of importance
  res.mat[,5] <- mapply(function(x, y){sum(x[y])}, imp.list, hits.path);
  
  # now add two more columns for the scaled values
  res.mat[,6] <- scale(-log(res.mat[,4]));
  res.mat[,7] <- scale(res.mat[,5]);
  
  # now, clean up result, synchronize with hit.query
  res.mat <- res.mat[hit.num>0,,drop = F];
  hits.query <- hits.query[hit.num>0];
  
  if(nrow(res.mat)> 1){
    # order by p value
    ord.inx <- order(res.mat[,4]);
    res.mat <- signif(res.mat[ord.inx,],3);
    hits.query <- hits.query[ord.inx];
    
    imp.inx <- res.mat[,4] <= 0.05;
    if(sum(imp.inx) < 10){ # too little left, give the top ones
      topn <- ifelse(nrow(res.mat) > 10, 10, nrow(res.mat));
      res.mat <- res.mat[1:topn,];
      hits.query <- hits.query[1:topn];
    }else{
      res.mat <- res.mat[imp.inx,];
      hits.query <- hits.query[imp.inx];
      if(sum(imp.inx) > 120){
        # now, clean up result, synchronize with hit.query
        res.mat <- res.mat[1:120,];
        hits.query <- hits.query[1:120];
      }
    }
  }
  
  hits.names <- lapply(hits.query, function(x) ora.vec.ids[which(x == TRUE)]);
  
  #get gene symbols
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);

  fun.anot = hits.names; names(fun.anot) <- resTable[,1];
  fun.pval = resTable[,5]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  hit.num = resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  current.setlink <- "http://www.genome.jp/kegg-bin/show_pathway?";

  json.res <- list(
              fun.link = current.setlink[1],
              fun.anot = fun.anot,
              #fun.ids = fun.ids,
              fun.pval = fun.pval,
              hit.num = hit.num
  );
  json.mat <- RJSONIO::toJSON(json.res, .na='null');
  json.nm <- paste(file.nm, ".json", sep="");
  
  sink(json.nm)
  cat(json.mat);
  sink();

  # write csv
  fun.hits <<- hits.query;
  fun.pval <<- resTable[,5];
  hit.num <<- resTable[,4];
  csv.nm <- paste(file.nm, ".csv", sep="");
  write.csv(resTable, file=csv.nm, row.names=F);
  return(1);
}

#'Create igraph from the edgelist saved from graph DB and decompose into subnets
#'@description Function for the network explorer module, prepares user's data for network exploration.
#'@param mSetObj Input name of the created mSet Object
#'@export
#'@import igraph
CreateGraph <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_igraph()
  }
  
  node.list <- pheno.net$node.data;
  edge.list <- pheno.net$edge.data;
  
  seed.proteins <- pheno.net$seeds;
  overall.graph <- simplify(graph.data.frame(edge.list, directed=FALSE, vertices=node.list), remove.multiple=FALSE);
  
  # add node expression value
  #newIDs <- names(seed.expr);
  newIDs <- seed.graph;
  
  match.index <- match(V(overall.graph)$name, newIDs);
  expr.vals <- seed.expr[match.index];
  overall.graph <- set.vertex.attribute(overall.graph, "abundance", index = V(overall.graph), value = expr.vals);
  
  hit.inx <- seed.proteins %in% node.list[,1];
  seed.proteins <<- seed.proteins[hit.inx];
  
  substats <- DecomposeGraph(overall.graph);
  overall.graph <<- overall.graph;
  
  mSetObj <- PlotNetwork(mSetObj, network.type);
  
  if(.on.public.web){
    mSetObj <- .get.mSet(mSetObj);
    if(!is.null(substats)){
      return(c(length(seed.graph), length(seed.proteins), nrow(node.list), nrow(edge.list), length(pheno.comps), substats));        
    }else{
      return(0);
    }
  }else{
    return(.set.mSet(mSetObj));
  }
}

###
### Utility functions
###

# Utility function to plot network for analysis report (CreateGraph)
PlotNetwork <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  img.Name = paste(imgName, "_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
    
  }else{
    w <- width;
  }
  
  h <- w;
  
  mSetObj$imgSet$networkplot <- img.Name
  
  nodeColors <- rep("lightgrey", length(V(overall.graph)))
  idx.cmpd <- as.vector(sapply(names(V(overall.graph)), function(x) substring(x,0,1) == "C"))
  idx.genes <- names(V(overall.graph)) %in% mSetObj$dataSet$gene
  nodeColors[idx.cmpd] <- "orange"
  nodeColors[idx.genes] <- "#306EFF"
  V(overall.graph)$color <- nodeColors
  
  # annotation
  nms <- V(overall.graph)$name;
  hit.inx <- match(nms, pheno.net$node.data[,1]);
  lbls <- pheno.net$node.data[hit.inx,2];
  V(overall.graph)$name <- as.vector(lbls);
  
  Cairo::Cairo(file = img.Name, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(overall.graph)
  text(1,1, "Entrez gene ", col="#306EFF")
  text(1,0.9, "KEGG compound ", col="orange")
  if(sum(nodeColors == "lightgrey") > 0){
    text(1,0.8, "OMIM disease ", col="lightgrey")
  }
  dev.off();
  
  if(.on.public.web==FALSE){
    return(.set.mSet(mSetObj));
  }else{
    .set.mSet(mSetObj);
  }
}

PrepareNetwork <- function(net.nm, json.nm){  
  convertIgraph2JSON(net.nm, json.nm);
  current.net.nm <<- net.nm;
  return(1);
}

# from node ID (uniprot) return entrez IDs (one to many)
GetNodeEntrezIDs <- function(uniprotID){
  enIDs <- current.anot[uniprotID];
  enIDs <- paste(enIDs, collapse="||");
  enIDs;
}

GetNodeEmblEntrezIDs <- function(emblprotein){
  enIDs <- current.anot[emblprotein];
  enIDs <- paste(enIDs, collapse="||");
  enIDs;
}

GetNodeIDs <- function(){
  V(overall.graph)$name;
}

GetNodeNames <- function(){
  V(overall.graph)$Label;
}

GetNodeDegrees <- function(){
  degree(overall.graph);
}

GetNodeBetweenness <- function(){
  round(betweenness(overall.graph, directed=F, normalized=F), 2);
}

DecomposeGraph <- function(gObj, minNodeNum=3, maxNetNum=10){

  # now decompose to individual connected subnetworks
  comps <- igraph::decompose.graph(gObj, min.vertices=minNodeNum);
  if(length(comps) == 0){
    current.msg <<- paste("No subnetwork was identified with at least", minNodeNum, "nodes!");
    return(NULL);
  }
  
  # first compute subnet stats
  net.stats <- ComputeSubnetStats(comps);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  comps <- comps[ord.inx];
  names(comps) <- rownames(net.stats) <- paste("subnetwork", 1:length(comps), sep="");
  
  # note, we report stats for all nets (at least 3 nodes);
  hit.inx <- net.stats$Node >= minNodeNum;
  comps <- comps[hit.inx];

  # in case too many
  if(length(comps) > maxNetNum){
     comps <- comps[1:maxNetNum];
  }

  # now record
  pheno.comps <<- comps;
  net.stats <<- net.stats;
  sub.stats <- unlist(lapply(comps, vcount));  
  return(sub.stats);
}

PrepareSubnetDownloads <- function(nm){
  g <- pheno.comps[[nm]];
  # need to update graph so that id is compound names rather than ID
  V(g)$name <- as.character(doID2LabelMapping(V(g)$name));
  saveNetworkInSIF(g, nm);
}

ComputeSubnetStats <- function(comps){
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  colnames(net.stats) <- c("Node", "Edge", "Query");
  for(i in 1:length(comps)){
    g <- comps[[i]];
    net.stats[i,] <- c(vcount(g),ecount(g),sum(seed.proteins %in% V(g)$name));
  }
  return(net.stats);
}

    UpdateSubnetStats <- function(){
  old.nms <- names(pheno.comps);
  net.stats <- ComputeSubnetStats(pheno.comps);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  rownames(net.stats) <- old.nms[ord.inx];
  net.stats <<- net.stats;
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

# from to should be valid nodeIDs
GetShortestPaths <- function(from, to){
  current.net <- pheno.comps[[current.net.nm]];
  paths <- igraph::get.all.shortest.paths(current.net, from, to)$res;
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

# exclude nodes in current.net (networkview)
ExcludeNodes <- function(nodeids, filenm){
  nodes2rm <- strsplit(nodeids, ";")[[1]];
  current.net <- pheno.comps[[current.net.nm]];
  current.net <- igraph::delete.vertices(current.net, nodes2rm);
  
  # need to remove all orphan nodes
  bad.vs<-V(current.net)$name[degree(current.net) == 0];
  current.net <- igraph::delete.vertices(current.net, bad.vs);
  
  # return all those nodes that are removed 
  nds2rm <- paste(c(bad.vs, nodes2rm), collapse="||");
  
  # update topo measures
  node.btw <- as.numeric(igraph::betweenness(current.net));
  node.dgr <- as.numeric(igraph::degree(current.net));
  node.exp <- as.numeric(igraph::get.vertex.attribute(current.net, name="abundance", index = V(current.net)));
  nms <- V(current.net)$name;
  hit.inx <- match(nms, pheno.net$node.data[,1]);
  lbls <- pheno.net$node.data[hit.inx,2];
  
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i], 
      label=lbls[i],
      degree=node.dgr[i], 
      between=node.btw[i],
      expr = node.exp[i]
    );
  }
  # now only save the node pos to json
  netData <- list(deletes=nds2rm, nodes=nodes);
  sink(filenm);
  cat(RJSONIO::toJSON(netData));
  sink();
  
  pheno.comps[[current.net.nm]] <<- current.net;
  UpdateSubnetStats();
  
  # remember to forget the cached layout, and restart caching, as this is now different object (with the same name)
  #forget(PerformLayOut_mem);
  return(filenm);
}

# support walktrap, infomap and lab propagation
FindCommunities <- function(method="walktrap", use.weight=FALSE){
  
  # make sure this is the connected
  current.net <- pheno.comps[[current.net.nm]];
  g <- current.net;
  if(!is.connected(g)){
    g <- igraph::decompose.graph(current.net, min.vertices=2)[[1]];
  }
  total.size <- length(V(g));
  
  if(use.weight){ # this is only tested for walktrap, should work for other method
    # now need to compute weights for edges
    egs <- igraph::get.edges(g, E(g)); #node inx
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
    fc <- igraph::walktrap.community(g);
  }else if(method == "infomap"){
    fc <- igraph::infomap.community(g);
  }else if(method == "labelprop"){
    fc <- igraph::label.propagation.community(g);
  }else{
    print(paste("Unknown method:", method));
    return ("NA||Unknown method!");
  }
  
  if(length(fc) == 0 || modularity(fc) == 0){
    return ("NA||No communities were detected!");
  }
  
  # only get communities
  communities <- igraph::communities(fc);
  community.vec <- vector(mode="character", length=length(communities));
  gene.community <- NULL;
  qnum.vec <- NULL;
  pval.vec <- NULL;
  rowcount <- 0;
  nms <- V(g)$name;
  hit.inx <- match(nms, pheno.net$node.data[,1]);
  sybls <- pheno.net$node.data[hit.inx,2];
  names(sybls) <- V(g)$name;
  for(i in 1:length(communities)){
    # update for igraph 1.0.1 
    path.ids <- communities[[i]];
    psize <- length(path.ids);
    if(psize < 5){
      next; # ignore very small community
    }
    hits <- seed.proteins %in% path.ids;
    qnums <- sum(hits);
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
    #subgraph <- induced.subgraph(g, path.inx);
    subgraph <- igraph::induced.subgraph(g, path.ids);
    in.degrees <- igraph::degree(subgraph);
    #out.degrees <- degree(g, path.inx) - in.degrees;
    out.degrees <- igraph::degree(g, path.ids) - in.degrees;
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
  write.csv(gene.community, file="module_table.csv", row.names=F);
  return(all.communities);
}

community.significance.test <- function(graph, vs, ...) {
  subgraph <- igraph::induced.subgraph(graph, vs)
  in.degrees <- igraph::degree(subgraph)
  out.degrees <- igraph::degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

###################################
# Adapted from netweavers package
###################
#'@import RColorBrewer
convertIgraph2JSON <- function(net.nm, filenm){
  
  g <- pheno.comps[[net.nm]];
  # annotation
  nms <- V(g)$name;
  hit.inx <- match(nms, pheno.net$node.data[,1]);
  lbls <- pheno.net$node.data[hit.inx,2];
  gene.names <- pheno.net$node.data[hit.inx,3];
  
  if("Evidence" %in% colnames(pheno.net$node.data)){
    evidence.ids <- pheno.net$node.data[hit.inx,4];
  } else {
    evidence.ids <- rep("", length(gene.names));
  }
  
  # get edge data
  edge.mat <- igraph::get.edgelist(g);
  edge.evidence <- igraph::edge_attr(g, "Evidence")
  if(!is.null(edge.evidence)){
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], evidence=edge.evidence);
  } else{
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2]);
  }
  
  # now get coords
  #pos.xy <- PerformLayOut_mem(net.nm, "Default");
  pos.xy <- PerformLayOut(net.nm, "Default");
  # get the note data
  node.btw <- as.numeric(igraph::betweenness(g));
  node.dgr <- as.numeric(igraph::degree(g));
  node.exp <- as.numeric(igraph::get.vertex.attribute(g, name="abundance", index = V(g)));
  
  # node size to degree values
  if(vcount(g)>500){
    min.size = 1;
  }else if(vcount(g)>200){
    min.size = 2;
  }else{
    min.size = 3;
  }
  node.sizes <- as.numeric(rescale2NewRange((log(node.dgr))^2, min.size, 9));
  centered = T;
  notcentered = F;
  # update node color based on betweenness
  topo.val <- log(node.btw+1);
  topo.colsb <- ComputeColorGradient(topo.val, "black", notcentered);
  topo.colsw <-  ComputeColorGradient(topo.val, "white", notcentered);
  
  # color based on expression
  bad.inx <- is.na(node.exp) | node.exp==0;
  if(!all(bad.inx)){
    exp.val <- node.exp;
    node.colsb.exp <- ComputeColorGradient(exp.val, "black", centered); 
    node.colsw.exp <- ComputeColorGradient(exp.val, "white", centered);
    node.colsb.exp[bad.inx] <- "#d3d3d3"; 
    node.colsw.exp[bad.inx] <- "#c6c6c6"; 
    # node.colsw.exp[bad.inx] <- "#66CCFF";
  }else{
    node.colsb.exp <- rep("#d3d3d3",length(node.exp)); 
    node.colsw.exp <- rep("#c6c6c6",length(node.exp)); 
  }
  
  if(table.nm == "global"){
    # now update for bipartite network
    # setup shape (gene circle, other squares)
    # Circles for genes
    shapes <- rep("circle", length(nms));
    # Squares for phenotypes
    mir.inx <- nms %in% edge.mat[,"target"];
    shapes[mir.inx] <- "square";
    # Diamond for metabolites
    cmpds.node <- as.vector(sapply(nms, function(x) substr(x, 1, 1) == "C"))
    shapes[cmpds.node] <- "diamond"
    node.sizes[mir.inx] <- node.sizes[mir.inx] + 0.5;
    # update mir node color
    node.colsw.exp[mir.inx] <- topo.colsw[mir.inx] <- "#306EFF"; # dark blue
    node.colsb.exp[mir.inx] <- topo.colsb[mir.inx] <- "#98F5FF";
    
  } else {
    # now update for bipartite network
    # setup shape (gene circle, other squares)
    shapes <- rep("circle", length(nms));
    if(pheno.net$db.type != 'ppi' && table.nm != "metabo_metabolites"){ # the other part miRNA or TF will be in square
      mir.inx <- nms %in% edge.mat[,"target"];
      shapes[mir.inx] <- "square";
      node.sizes[mir.inx] <- node.sizes[mir.inx] + 0.5;
      
      # update mir node color
      node.colsw.exp[mir.inx] <- topo.colsw[mir.inx] <- "#306EFF"; # dark blue
      node.colsb.exp[mir.inx] <- topo.colsb[mir.inx] <- "#98F5FF";
    }
  }
  
  # now create the json object
  nodes <- vector(mode="list");
  for(i in 1:length(node.sizes)){
    # TODO: avoid which here and just attach HMDB matched IDs to the list of Compound nodes
    hmdb.id <- mSet$dataSet$map.table[which(mSet$dataSet$map.table[,1] == nms[i]), 3]

    nodes[[i]] <- list(
      id=nms[i],
      idnb = i, 
      hmdb=hmdb.id,
      label=lbls[i],
      evidence=evidence.ids[i],
      genename=gene.names[i],
      x = pos.xy[i,1],
      y = pos.xy[i,2],
      size=node.sizes[i], 
      type=shapes[i],
      colorb=topo.colsb[i],
      colorw=topo.colsw[i],
      attributes=list(
        expr = node.exp[i],
        expcolb=node.colsb.exp[i],
        expcolw=node.colsw.exp[i],
        degree=node.dgr[i], 
        between=node.btw[i])
    );
  }
  
  # save node table
  nd.tbl <- data.frame(Id=nms, Label=lbls, Degree=node.dgr, Betweenness=round(node.btw,2));
  # order 
  ord.inx <- order(nd.tbl[,3], nd.tbl[,4], decreasing = TRUE)
  nd.tbl <- nd.tbl[ord.inx, ];
  write.csv(nd.tbl, file="node_table.csv", row.names=FALSE);
  
  # covert to json
  netData <- list(nodes=nodes, edges=edge.mat);
  sink(filenm);
  cat(RJSONIO::toJSON(netData));
  sink();
}

# also save to GraphML
ExportNetwork <- function(fileName){
  current.net <- pheno.comps[[current.net.nm]];
  igraph::write.graph(current.net, file=fileName, format="graphml");
}

ExtractModule<- function(nodeids){
  set.seed(8574);
  nodes <- strsplit(nodeids, ";")[[1]];
  
  g <- pheno.comps[[current.net.nm]];
  # try to see if the nodes themselves are already connected
  hit.inx <- V(g)$name %in% nodes; 
  gObj <- igraph::induced.subgraph(g, V(g)$name[hit.inx]);
  
  # now find connected components
  comps <- igraph::decompose.graph(gObj, min.vertices=1);
  
  if(length(comps) == 1){ # nodes are all connected
    g <- comps[[1]];
  }else{
    # extract modules
    paths.list <-list();
    sd.len <- length(nodes);
    for(pos in 1:sd.len){
      paths.list[[pos]] <- igraph::get.shortest.paths(g, nodes[pos], nodes[-(1:pos)])$vpath;
    }
    nds.inxs <- unique(unlist(paths.list));
    nodes2rm <- V(g)$name[-nds.inxs];
    g <- simplify(igraph::delete.vertices(g, nodes2rm));
  }
  nodeList <- igraph::get.data.frame(g, "vertices");
  if(nrow(nodeList) < 3){
    return ("NA");
  }
  
  module.count <- module.count + 1;
  module.nm <- paste("module", module.count, sep="");
  colnames(nodeList) <- c("Id", "Label");
  ndFileNm = paste(module.nm, "_node_list.csv", sep="");
  write.csv(nodeList, file=ndFileNm, row.names=F, quote=F);
  
  edgeList <- igraph::get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  edgFileNm = paste(module.nm, "_edge_list.csv", sep="");
  write.csv(edgeList, file=edgFileNm, row.names=F, quote=F);
  
  filenm <- paste(module.nm, ".json", sep="");
  
  # record the module 
  pheno.comps[[module.nm]] <<- g;
  UpdateSubnetStats();
  
  module.count <<- module.count;
  
  convertIgraph2JSON(module.nm, filenm);
  return (filenm);
}

PerformLayOut <- function(net.nm, algo){
  g <- pheno.comps[[net.nm]];
  vc <- vcount(g);
  if(algo == "Default"){
    if(vc > 3000) {
      pos.xy <- igraph::layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- igraph::layout.lgl(g, maxiter = 150);
    }else if(vc > 1000) {
      pos.xy <- igraph::layout.lgl(g, maxiter = 200);
    }else if(vc < 150){
      pos.xy <- igraph::layout.kamada.kawai(g);
    }else{
      pos.xy <- igraph::layout.fruchterman.reingold(g);
    }
  }else if(algo == "FrR"){
    pos.xy <- igraph::layout.fruchterman.reingold(g);
  }else if(algo == "random"){
    pos.xy <- igraph::layout.random(g);
  }else if(algo == "lgl"){
    if(vc > 3000) {
      pos.xy <- igraph::layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- igraph::layout.lgl(g, maxiter = 150);
    }else {
      pos.xy <- igraph::layout.lgl(g, maxiter = 200);
    }
  }else if(algo == "gopt"){
    # this is a slow one
    if(vc > 3000) {
      maxiter = 50;
    }else if(vc > 2000) {
      maxiter = 100;
    }else if(vc > 1000) {
      maxiter = 200;
    }else{
      maxiter = 500;
    }
    pos.xy <- igraph::layout.graphopt(g, niter=maxiter);
  }
  pos.xy;
}

UpdateNetworkLayout <- function(algo, filenm){
  current.net <- pheno.comps[[current.net.nm]];
  #pos.xy <- PerformLayOut_mem(current.net.nm, algo);
  pos.xy <- PerformLayOut(current.net.nm, algo);
  nms <- V(current.net)$name;
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i], 
      x=pos.xy[i,1], 
      y=pos.xy[i,2]
    );
  }
  # now only save the node pos to json
  netData <- list(nodes=nodes);
  sink(filenm);
  cat(RJSONIO::toJSON(netData));
  sink();
  return(filenm);
}


doID2LabelMapping <- function(entrez.vec){
  
  hit.inx <- match(entrez.vec, nodeListu[, "Id"]);
  symbols <- nodeListu[hit.inx, "Label"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
} 

# new range [a, b]
rescale2NewRange <- function(qvec, a, b){
  q.min <- min(qvec);
  q.max <- max(qvec);
  if(length(qvec) < 50){
    a <- a*2;
  }
  if(q.max == q.min){
    new.vec <- rep(8, length(qvec));
  }else{
    coef.a <- (b-a)/(q.max-q.min);
    const.b <- b - coef.a*q.max;
    new.vec <- coef.a*qvec + const.b;
  }
  return(new.vec);
}


# #FFFFFF to rgb(1, 0, 0)
hex2rgba <- function(cols){
  return(apply(sapply(cols, col2rgb), 2, function(x){paste("rgba(", x[1], ",", x[2], ",", x[3], ",0.8)", sep="")})); 
}

# re-arrange one vector elements according to another vector values
# usually src is character vector to be arranged
# target is numberic vector of same length
sync2vecs <- function(src.vec, tgt.vec){
  if(length(src.vec) != length(tgt.vec)){
    print("must be of the same length!");
    return();
  }
  ord.inx <- match(rank(tgt.vec, ties.method="random"), 1:length(tgt.vec));
  src.vec[ord.inx];
}

# given a data with duplicates, dups is the one with duplicates
RemoveDuplicatesNetwork <- function(data, lvlOpt, quiet=T){
  
  all.nms <- rownames(data);
  colnms <- colnames(data);
  dup.inx <- duplicated(all.nms);
  dim.orig  <- dim(data);
  data <- apply(data, 2, as.numeric); # force to be all numeric
  dim(data) <- dim.orig; # keep dimension (will lost when only one item) 
  rownames(data) <- all.nms;
  colnames(data) <- colnms;
  if(sum(dup.inx) > 0){
    uniq.nms <- all.nms[!dup.inx];
    uniq.data <- data[!dup.inx,,drop=F];
    
    dup.nms <- all.nms[dup.inx];
    uniq.dupnms <- unique(dup.nms);
    uniq.duplen <- length(uniq.dupnms);
    
    for(i in 1:uniq.duplen){
      nm <- uniq.dupnms[i];
      hit.inx.all <- which(all.nms == nm);
      hit.inx.uniq <- which(uniq.nms == nm);
      
      # average the whole sub matrix 
      if(lvlOpt == "mean"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, mean, na.rm=T);
      }else if(lvlOpt == "median"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, median, na.rm=T);
      }else if(lvlOpt == "max"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, max, na.rm=T);
      }else{ # sum
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, sum, na.rm=T);
      }
    }
    if(!quiet){
      current.msg <<- paste(current.msg, paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""), collapse="\n");
    }
    return(uniq.data);
  }else{
    if(!quiet){
      current.msg <<- paste(current.msg, "All IDs are unique.", collapse="\n");
    }
    return(data);
  }
} 

generate_breaks = function(x, n, center = F){
  if(center){
    m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
    res = seq(-m, m, length.out = n + 1)
  }
  else{
    res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
  }
  return(res)
}

ComputeColorGradient <- function(nd.vec, background="black", centered){
  #if(sum(nd.vec<0, na.rm=TRUE) > 0){ 
  #centered <- T;
  #}else{
  #centered <- F;
  #}
  color <- GetColorGradient(background, centered);
  breaks <- generate_breaks(nd.vec, length(color), center = centered);
  return(scale_vec_colours(nd.vec, col = color, breaks = breaks));
}

GetColorGradient <- function(background, center){
  if(background == "black"){
    if(center){
      return(c(colorRampPalette(c("#31A231", "#5BC85B", "#90EE90", "#C1FFC1"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)));
    }else{
      return(colorRampPalette(rev(heat.colors(9)))(100));
    }
  }else{ # white background
    if(center){
      return(c(colorRampPalette(c("#137B13", "#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313", "#96000D"))(50)));
    }else{
      # return(colorRampPalette(c("grey", "orange", "red", "darkred"))(100));
      # return(colorRampPalette(c("#80d0f0", rainbow(8, start=0.8, end=1)))(100));
      return(colorRampPalette(hsv(h = seq(0.72, 1, 0.035), s = 0.72, v = 1))(100));
    }
  }
}

scale_vec_colours = function(x, col = rainbow(10), breaks = NA){
  breaks <- sort(unique(breaks));
  return(col[as.numeric(cut(x, breaks = breaks, include.lowest = T))])
}
