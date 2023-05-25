##################################################
## R script for ExpressAnalyst
## Description: Compute enrichment network
## Author: Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
##################################################
my.enrich.net<-function(dataSet, netNm="abc", type="list", overlapType="mixed", analSet){
  cat("my.enrich.net===",netNm, type, overlapType);
  enr.mat <- qs:::qread("enr.mat.qs");
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  
  if(is.null(enr.mat)){
    return(0);
  }
  hits <-  enr.mat[,"Hits"];
  pvals <- enr.mat[,"P.Value"];
  require(igraph);
  require(reshape);
  pvalue <- pvals;
  id <- names(pvalue);
  current.geneset <- qs::qread("current_geneset.qs");
  hits.query <- qs::qread("hits_query.qs")
  hits.query <- hits.query[rownames(enr.mat)];
  geneSets <- hits.query;
  n <- length(pvalue);
  w <- matrix(NA, nrow=n, ncol=n);
  colnames(w) <- rownames(w) <- id;
  
  for (i in 1:n) {
    for (j in i:n) {
      w[i,j] <- overlap_ratio(geneSets[id[i]], geneSets[id[j]], overlapType)
    }
  }
  wd <- reshape::melt(w);
  wd <- wd[wd[,1] != wd[,2],];
  wd <- wd[!is.na(wd[,3]),];
  
  g <- graph.data.frame(wd[,-3], directed=F);
  if(type == "list"){
    g <- delete.edges(g, E(g)[wd[,3] < 0.3]);
  }else{
    g <- delete.edges(g, E(g)[wd[,3] < 0.3]);
  }
  idx <- unlist(sapply(V(g)$name, function(x) which(x == id)));
  
  # define local function
  my.normalize <- function(x){
    return((x- min(x)) /(max(x)-min(x)))
  }
  my.rescale <- function(x, from, to){
    (x - min(x)) / max(x - min(x)) * (to - from) + from
  }
  
  V(g)$color <- ComputeColorGradient(-log(my.normalize(pvalue) + min(pvalue/2)), "black", F, F);
  V(g)$colorw <- ComputeColorGradient(-log(my.normalize(pvalue) + min(pvalue/2)), "white", F, F);
  
  cnt <- hits;
  names(cnt) <- id;
  cnt2 <- cnt[V(g)$name];
  
  V(g)$size <- my.rescale(log(cnt2+1, base=10), 8, 32);
  
  # layout
  pos.xy <- layout.auto(g);
  
  # now create the json object
  nodes <- vector(mode="list");
  node.nms <- V(g)$name;
  node.sizes <- V(g)$size;
  node.cols <- V(g)$color;
  node.colsw <- V(g)$colorw;
  
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id = node.nms[i],
      label=node.nms[i],
      size = node.sizes[i],
      true_size=node.sizes[i], 
      molType="set",
      colorb=node.cols[i],
      colorw=node.colsw[i],
      posx = pos.xy[i,1],
      posy = pos.xy[i,2]
    );
  }
  
  edge.mat <- get.edgelist(g);
  edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2]);
  
  # covert to json
  bedges <- stack(hits.query);
  b.mat <- matrix(NA, nrow=nrow(bedges), ncol=2);
  b.mat[,1] <- bedges[,"values"];
  b.mat[,2] <- as.character(bedges[,"ind"]);
  b.mat <- b.mat[complete.cases(b.mat),]
  colnames(b.mat) <- c("source", "target");
  bg <- graph.data.frame(b.mat, directed=F);
  idx <- unlist(sapply(V(bg)$name, function(x) which(x == id)));
  cols <- color_scale("red", "#E5C494");
  
  V(bg)$color[V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(-log(pvalue), "black", F, F);
  V(bg)$colorw[V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(-log(pvalue), "white", F, F);
  node.nms <- V(bg)$name;
  if(anal.type == "onedata"){
    tbl <- dataSet$comp.res
    tbl <- tbl[which(doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType) %in% V(bg)$name),]
    expr.val <- tbl[,paramSet$selectedFactorInx];
    expvals <- expr.val;
    names(expvals) <- doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType)
    expvals <- expvals[node.nms]
    V(bg)$color[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T,T);
    V(bg)$colorw[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T, T);
  }else if(anal.type == "genelist" && sum(as.numeric(paramSet$all.prot.mat[,1])) != 0){
    tbl <- paramSet$all.prot.mat
    gene.nms <- V(bg)$name[which(!V(bg)$name %in% rownames(enr.mat))]
    tbl <- tbl[which(tbl[,2] %in% gene.nms),]
    expr.val <- tbl[,1];
    names(expr.val) <- tbl[,2]
    expvals <- expr.val
    expvals <- expvals[node.nms]
    expvals <- expvals[!is.na(expvals)]
    V(bg)$color[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T, T);
    V(bg)$colorw[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T, T);
    
  }else if(anal.type =="metadata"){
    if(paramSet$selDataNm == "meta_default"){
      tbl <- analSet$meta.mat.all
      tbl <- tbl[which(doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType) %in% V(bg)$name),]
      expvals <- analSet$meta.avgFC[rownames(tbl)]
      names(expvals) <- doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType)
      expvals <- expvals[node.nms]
      V(bg)$color[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T, T);
      V(bg)$colorw[!V(bg)$name %in% rownames(enr.mat)] <- ComputeColorGradient(unname(expvals), "black", T, T);
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      tbl <- dataSet$comp.res;
      tbl <- tbl[which(doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType) %in% V(bg)$name),]
      expvals <- tbl[,"logFC"];
      names(expvals) <- doEntrez2SymbolMapping(rownames(tbl), paramSet$data.org, paramSet$data.idType);
      expvals <- expvals[node.nms]
      expvals <- expvals[!is.na(expvals)]
      inx <- !V(bg)$name %in% rownames(enr.mat);
      V(bg)$color[inx] <- ComputeColorGradient(unname(expvals), "black", T, T);
      V(bg)$colorw[inx] <- ComputeColorGradient(unname(expvals), "black", T, T);
    }
  }else{
    expvals <- rep(0,length(V(bg)$color)); 
    V(bg)$color[!V(bg)$name %in% rownames(enr.mat)] <- "#00FFFF";
    V(bg)$colorw[!V(bg)$name %in% rownames(enr.mat)] <- "#668B8B"
  }
  node.dgr2 <- as.numeric(degree(bg));
  V(bg)$size <- my.rescale(log(node.dgr2, base=10), 8, 24); 
  
  # layout
  pos.xy <- layout.auto(bg);
  
  # now create the json object
  bnodes <- vector(mode="list");
  node.sizes <- V(bg)$size;
  node.cols <- V(bg)$color;
  node.colsw <- V(bg)$colorw;
  
  shapes <- rep("circle", length(node.nms));
  hit.inx <- node.nms %in% b.mat[,"source"];
  shapes[hit.inx] <- "gene";
  node.lbls <- doEntrez2SymbolMapping(node.nms, paramSet$data.org, paramSet$data.idType)
  
  for(i in 1:length(node.sizes)){
    bnodes[[i]] <- list(
      id = node.nms[i],
      label=node.lbls[i], 
      size=node.sizes[i], 
      colorb=node.cols[i],
      colorw=node.colsw[i],
      true_size=node.sizes[i], 
      molType=shapes[i],
      exp= unname(expvals[node.nms[i]]),
      posx = pos.xy[i,1],
      posy = pos.xy[i,2]
    );
  }
  
  ppi.comps <- vector(mode="list");
  paramSet$current.net.nm <- netNm
  ppi.comps[[netNm]] <- bg;
  analSet$ppi.comps <- ppi.comps

  bedge.mat <- get.edgelist(bg);
  bedge.mat <- cbind(id=1:nrow(bedge.mat), source=bedge.mat[,1], target=bedge.mat[,2]);
  initsbls <- doEntrez2SymbolMapping(analSet$list.genes, paramSet$data.org, paramSet$data.idType)
  names(initsbls) <- analSet$list.genes

  #for rjson generation
  edge.mat <- apply(edge.mat, 1, as.list)
  bedge.mat <- apply(bedge.mat, 1, as.list)
  enr.mat <- apply(enr.mat, 1, as.list)

  netData <- list(nodes=nodes, 
                  edges=edge.mat, 
                  bnodes=bnodes, 
                  bedges=bedge.mat, 
                  enr=unname(enr.mat), 
                  id=names(enr.mat), 
                  sizes=analSet$listSizes, 
                  hits=hits.query, 
                  genelist=initsbls, 
                  analType=anal.type, 
                  org=paramSet$data.org, 
                  naviString = "Enrichment Network");
  netName <- paste0(netNm, ".json");
  paramSet$partialToBeSaved <- c( paramSet$partialToBeSaved, c(netName));
  paramSet$jsonNms$network <- netName;
  saveSet(paramSet, "paramSet");
  saveSet(analSet, "analSet");
  sink(netName);
  cat(rjson::toJSON(netData));
  sink();
  return(analSet);
}