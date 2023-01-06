##################################################
## R script for ExpressAnalyst
## Description: Compute 3D scatter plot
## Author: Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
##################################################

#dataName is used for onedata dataset
my.json.scatter <- function(dataSet, filenm="abc"){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  anal.type <- paramSet$anal.type;
  sig.mats <- list();
  seeds <- vector();
  if(anal.type == "metadata"){
    sel.nms <- names(mdata.all)[mdata.all==1];
    for(i in 1:length(sel.nms)){
      dataSet <- readDataset(sel.nms[i]);
      if(i == 1){
        seeds <- rownames(dataSet$sig.mat);
        meta <- dataSet$meta;
        meta$dataSet <- rep(sel.nms[i],nrow(meta));
      }else{
        seeds <- c(seeds, rownames(dataSet$sig.mat));
        currMeta <- dataSet$meta;
        currMeta$dataSet <- rep(sel.nms[i],nrow(currMeta));
        meta <- rbind(meta, currMeta);
      }
    }
    sig.tbl <- qs::qread("meta.resTable.qs");
    sig.tbl$id <- rownames(sig.tbl);
    sig.mats[[1]] <- sig.tbl;
  }else{
    sigmat <- dataSet$sig.mat;
    sigmat$id <- rownames(dataSet$sig.mat);
    sig.mats[[1]] <- sigmat;
    seeds <- rownames(dataSet$sig.mat); 
    meta <- dataSet$meta;
  }
  
  reductionSet <- dataSet;
  Sys.setenv(RGL_USE_NULL = TRUE);
  require(rgl);
  require(igraph);
  pos.xyz <-qs::qread("score_pos_xyz.qs");
  nodes <- vector(mode="list");
  names <- c(rownames(pos.xyz));
  metadf <- meta;
  
  
  a<-list();
  a$objects <- "NA";
  meshes<-"NA";
  
  col <- vector();
  
  
  # can be selected meta as well if = dataSet$sel.meta
  meta.vec <- as.vector(metadf[,1]);
  meta.vec.num <- as.integer(as.factor(metadf[,1]));
  col.s <- gg_color_hue_scatter(length(unique(meta.vec)), "green");
  for(i in 1:length(meta.vec.num)){
    col[i] <- col.s[meta.vec.num[i]];
  }
  color = col;
  nodeSize = 18;
  if(length(names)>200){
    nodeSize = 16;
  }
  
  for(i in 1:length(names)){
    nodes[[i]] <- list(
      id=names[i],
      label=names[i],
      size=nodeSize,
      meta=meta.vec[i],
      cluster=meta.vec.num[i],
      fx = unname(pos.xyz[i,1])*1000,
      fy = unname(pos.xyz[i,2])*1000,
      fz = unname(pos.xyz[i,3])*1000,
      colorb=color[i],
      colorw=color[i],
      topocolb=color[i],
      topocolw=color[i],
      expcolb=color[i],
      expcolw=color[i],
      attributes=list(
        expr = 1,
        degree=1,
        between=1
      )
    );
  }
  
  edge.mat = "NA";
  
  modules = "NA";
  # save node table
  ellipse ="NA";  
  require(RJSONIO);
  
  
  metadf < meta;
  
  loading.data <- qs::qread("loading_pos_xyz.qs");
  aLoading<-list();
  aLoading$objects <- "NA";
  
  de <- dataSet$comp.res;
  de <- de[which(rownames(de) %in% rownames(loading.data)),]
  ids <- rownames(de);
  de[de == "NaN"] = 1;
  pv <- as.numeric(de[,"P.Value"]);
  pv_no_zero <- pv[pv != 0];
  minval <- min(pv_no_zero);
  pv[pv == 0] <- minval/2;
  pvals <- -log10(pv);
  type.vec <- pvals;
  colors<- ComputeColorGradient(pvals, "black", F, F);
  colorb <- colors;
  sizes <- as.numeric(rescale2NewRange(-log10(pv), 15, 25));
  nodes2 <- vector(mode="list");
  
  
  seed.inx <- names %in% unique(seeds);
  seed_arr <- rep("notSeed",length(names));
  seed_arr[seed.inx] <- "seed";
  names <- doEntrez2SymbolMapping(ids, paramSet$data.org, paramSet$data.idType);
  for(i in 1:nrow(loading.data)){
    nodes2[[i]] <- list(
      id=ids[i],
      label=names[i],
      size=sizes[i],
      cluster=1,
      omicstype=type.vec[i],
      fx = unname(loading.data[i,1])*1000,
      fy = unname(loading.data[i,2])*1000,
      fz = unname(loading.data[i,3])*1000,
      seedArr = seed_arr[i],
      colorb=colorb[i],
      colorw=colorb[i],
      topocolb="#ffa500",
      topocolw="#ffa500",
      expcolb="#ffa500",
      expcolw="#ffa500",
      attributes=list(
        expr = 1,
        degree=1,
        between=1
      )
    );
  }
  netData <- list( nodes=nodes, edges=edge.mat, modules=modules, objects=a$objects, ellipse=meshes, meta=metadf, loading=nodes2, reductionOpt="pca" , objectsLoading=aLoading$objects, sigMat=sig.mats, omicstype=c("rna.b"));
    
  netData[["misc"]] <- "";
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(filenm));
  paramSet$jsonNms["scatter3d"] <- filenm;

  saveSet(paramSet, "paramSet");
  
  sink(filenm);
  cat(toJSON(netData));
  sink();
  
  return(1);
}
