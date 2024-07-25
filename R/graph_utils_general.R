##################################################
## R script for ExpressAnalyst
## Description: General graph manipulation functions 
## Authors: 
## G. Zhou, guangyan.zhou@mail.mcgill.ca
## J. Xia, jeff.xia@mcgill.ca
###################################################

GetColorSchema <- function(my.grps){
  # test if total group number is over 9
  my.grps <- as.factor(my.grps);
  grp.num <- length(levels(my.grps));
  
  if(grp.num > 9){
    pal12 <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
               "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
               "#FFFF99", "#B15928");
    dist.cols <- colorRampPalette(pal12)(grp.num);
    lvs <- levels(my.grps);
    colors <- vector(mode="character", length=length(my.grps));
    for(i in 1:length(lvs)){
      colors[my.grps == lvs[i]] <- dist.cols[i];
    }
  }else{
    colors <- as.numeric(my.grps)+1;
  }
  return (colors);
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


ComputeColorGradient <- function(nd.vec, background="black", centered, colorblind){
  require("RColorBrewer");
  
  minval <- min(nd.vec, na.rm=TRUE);
  maxval <- max(nd.vec, na.rm=TRUE);
  res <- maxval-minval;
  
  if(res == 0){
    return(rep("#FF0000", length(nd.vec)));
  }
  color <- GetColorGradient(background, centered, colorblind);
  breaks <- generate_breaks(nd.vec, length(color), center = centered);
  return(scale_vec_colours(nd.vec, col = color, breaks = breaks));
}

generate_breaks <- function(x, n, center = F){
  if(center){
    m <- max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
    res <- seq(-m, m, length.out = n + 1)
  }
  else{
    res <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
  }
  return(res)
}

scale_vec_colours <- function(x, col = rainbow(10), breaks = NA){
  breaks <- sort(unique(breaks));
  return(col[as.numeric(cut(x, breaks = breaks, include.lowest = T))])
}

GetColorGradient <- function(background, center, colorblind=F) {
  if (background == "black") {
    if (center) {
      if (colorblind) {
        return(c(colorRampPalette(c("#3182bd", "#6baed6", "#bdd7e7"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313"))(50)))
      } else {
        return(c(colorRampPalette(c("#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313"))(50)))
      }
    } else {
      if (colorblind) {
        return(c(colorRampPalette(c("#3182bd", "#bbfdff"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)))
      } else {
        return(colorRampPalette(rev(heat.colors(9)))(100))
      }
    }
  } else {
    if (center) {
      if (colorblind) {
        return(c(colorRampPalette(c("#3182bd", "#bbfdff"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)))
      } else {
        return(c(colorRampPalette(c("#137B13", "#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313", "#96000D"))(50)))
      }
    } else {
      return(colorRampPalette(hsv(h = seq(0.72, 1, 0.035), s = 0.72, v = 1))(100))
    }
  }
}

UpdateNetworkLayout <- function(algo, filenm, focus=""){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  current.net.nm <- paramSet$current.net.nm;
  current.net <- analSet$ppi.comps[[current.net.nm]];
  pos.xy <- PerformLayOut(current.net.nm, algo, focus);
  nms <- V(current.net)$name;
  nodes <- vector(mode="list");
  if(algo %in% c("fr", "kk")){
    for(i in 1:length(nms)){
      nodes[[i]] <- list(
        id=nms[i],
        x=pos.xy[i,1],
        y=pos.xy[i,2],
        z=pos.xy[i,3]
      );
    }
  }else{
    for(i in 1:length(nms)){
      nodes[[i]] <- list(
        id=nms[i], 
        x=pos.xy[i,1], 
        y=pos.xy[i,2]
      );
    }
  }
  # now only save the node pos to json
  netData <- list(nodes=nodes);
  sink(filenm);
  cat(RJSONIO::toJSON(netData));
  sink();
  return(filenm);
}


ExtractModule<- function(nodeids){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  set.seed(8574);
  nodes <- strsplit(nodeids, ";")[[1]];
  
  g <- analSet$ppi.comps[[paramSet$current.net.nm]];
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

  if(ncol(nodeList) == 1){
    nodeList <- data.frame(Id=nodeList[,1], Label=nodeList[,1]);
  }
  
  paramSet$module.count <- paramSet$module.count + 1;
  module.nm <- paste("module", paramSet$module.count, sep="");
  colnames(nodeList) <- c("Id", "Label");
  ndFileNm <- paste(module.nm, "_node_list.csv", sep="");
  fast.write(nodeList, file=ndFileNm, row.names=F);
  
  edgeList <- get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  edgFileNm <- paste(module.nm, "_edge_list.csv", sep="");
  fast.write(edgeList, file=edgFileNm, row.names=F);
  
  filenm <- paste(module.nm, ".json", sep="");
  
  # record the module 
  analSet$ppi.comps[[module.nm]] <<- g;
  UpdateSubnetStats();
  
  saveSet(paramSet, "paramSet");
  saveSet(analSet, "analSet");

  convertIgraph2JSON(module.nm, filenm);
  return (filenm);
}

# also save to GraphML
ExportNetwork <- function(fileName){
  paramSet <- readSet(paramSet, "paramSet");
  current.net <- ppi.comps[[paramSet$current.net.nm]];
  write.graph(current.net, file=fileName, format="graphml");
}

##################################################
## R script for ExpressAnalyst
## Description: Perform network layout
## Author: 
## Jeff Xia, jeff.xia@mcgill.ca
## G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PerformLayOut <- function(net.nm, algo, focus=""){
  analSet <- readSet(analSet, "analSet");

  g <- analSet$ppi.comps[[net.nm]];
  vc <- vcount(g);
  if(algo == "Default"){
    if(vc > 5000) {
      pos.xy <- layout_with_lgl(g);
    }else if(vc < 100){
      pos.xy <- layout_with_kk(g);
    }else{
      pos.xy <- layout_with_fr(g);
    }
  }else if(algo == "FrR"){
    pos.xy <- layout_with_fr(g, area=34*vc^2);
  }else if(algo == "random"){
    pos.xy <- layout_randomly (g);
  }else if(algo == "lgl"){
    pos.xy <- layout_with_lgl(g);
  }else if(algo == "gopt"){
    # this is a slow one
    if(vc > 3000) {
      maxiter <- 50;
    }else if(vc > 2000) {
      maxiter <- 100;
    }else if(vc > 1000) {
      maxiter <- 200;
    }else{
      maxiter <- 500;
    }
    pos.xy <- layout.graphopt(g, niter=maxiter);
  }else if(algo == "fr"){
    pos.xy <- layout_with_fr(g, dim=3, niter=500);
  }else if(algo == "kk"){
    pos.xy <- layout_with_kk(g, dim=3, maxiter=500);
  }else if(algo == "tripartite"){
    l <- layout_with_sugiyama(g, layers = V(g)$layers*(vc/4));
    pos.xy <- -l$layout[,2:1];
  }else if(algo == "concentric"){
    require(graphlayouts);
    # the fist element in the list for concentric is the central node.
    if(focus==""){
      inx <- 1;
    }else{
      inx <- which(V(g)$name == focus);
    }
    coords <- layout_with_focus(g,inx);
    pos.xy <- coords$xy;
  }else if(algo == "backbone"){
    require(graphlayouts);
    if(length(V(g)$name)<2000){
      coords <- layout_with_stress(g);
      pos.xy <- coords;
    }else{
      coords <- layout_with_sparse_stress(g,pivots=100);
      pos.xy <- coords;
    }
    
  }
  pos.xy;
}
