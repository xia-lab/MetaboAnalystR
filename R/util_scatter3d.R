##################################################
## R script for OmicsAnalyst
## Description: Compute 3D scatter plot from dimension reduction results
## Authors: 
## G. Zhou (guangyan.zhou@mail.mcgill.ca) 
## J. Xia, jeff.xia@mcgill.ca
###################################################

my.json.scatter <- function(filenm, containsLoading=F){

  library(igraph);
  ## 
  res <- qs::qread("score3d.qs")
  res <- res$score;
  
  nodes <- vector(mode="list");
  names <- res$name;
  orig.pos.xyz <- t(res$xyz);
  
  ticksX <- pretty(range(orig.pos.xyz[,1]),10, bound=F);
  ticksY <- pretty(range(orig.pos.xyz[,2]),10, bound=F);
  ticksZ <- pretty(range(orig.pos.xyz[,3]),10, bound=F);
  
  #add two nodes, 1 with all min values and another with all max values. For scaling purposes
  minNode <-  c(min(ticksX), min(ticksY), min(ticksZ));
  maxNode <-  c(max(ticksX), max(ticksY), max(ticksZ));
  
  # Add the new rows to the data frame
  orig.pos.xyz.mod <- rbind(orig.pos.xyz, minNode)
  orig.pos.xyz.mod <- rbind(orig.pos.xyz.mod, maxNode)
  
  #scaling
  pos.xyz <- orig.pos.xyz.mod;
  pos.xyz[,1] <- scale_range(orig.pos.xyz.mod[,1], -1,1);
  pos.xyz[,2] <- scale_range(orig.pos.xyz.mod[,2], -1,1);
  pos.xyz[,3] <- scale_range(orig.pos.xyz.mod[,3], -1,1);
  
  #remove last two rows
  pos.xyz <- pos.xyz[1:(nrow(pos.xyz) - 2), ]
  
  metadf <- res$facA
  
  col = vector();
  
  meta.vec = as.vector(metadf)
  meta.vec.num = as.integer(as.factor(metadf))
  col.s <- gg_color_hue(length(unique(meta.vec)))
  for(i in 1:length(meta.vec.num)){
    col[i] = col.s[meta.vec.num[i]];
  }
  
  if("facB" %in% names(res)){
    meta.vec2 <- as.vector()
  }
  
  nodeSize = 18;
  
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
      origX = unname(orig.pos.xyz[i,1]),
      origY = unname(orig.pos.xyz[i,2]),
      origZ = unname(orig.pos.xyz[i,3]),
      colorb=col[i],
      colorw=col[i]
    );
    
    if("facB" %in% names(res)){
      nodes[[i]][["meta2"]] <- meta.vec2[i]
    }
  }
  
  
  ticks <- list(x=ticksX, y=ticksY, z=ticksZ);
  library(RJSONIO)
  
  if(!containsLoading){
    netData <- list(nodes=nodes,
  edges=edge.mat,
  meta=metadf, 
  loading="NA",
  reductionOpt=reductionOptGlobal, 
  misc="",
  omicstype="met",
  axis=res$axis, 
  ticks=ticks,
  metaCol = list(label=unique(meta.vec),color=col.s));
  }else{
    res2 <- qs::qread("pca3d_loadings.qs");
    res2 <- res2$loading;
    orig.load.xyz <- t(res2$xyz)
    
    ticksX <- pretty(range(orig.load.xyz[,1]),10);
    ticksY <- pretty(range(orig.load.xyz[,2]),10);
    ticksZ <- pretty(range(orig.load.xyz[,3]),10);
    
    #add two nodes, 1 with all min values and another with all max values. For scaling purposes
    minNode <-  c(min(ticksX), min(ticksY), min(ticksZ));
    maxNode <-  c(max(ticksX), max(ticksY), max(ticksZ));
    
    # Add the new rows to the data frame
    orig.load.xyz.mod <- rbind(orig.load.xyz, minNode)
    orig.load.xyz.mod <- rbind(orig.load.xyz.mod, maxNode)
    
    load.xyz <- orig.load.xyz.mod;
    
    load.xyz[,1] <- scale_range(orig.load.xyz.mod[,1], -1,1);
    load.xyz[,2] <- scale_range(orig.load.xyz.mod[,2], -1,1);
    load.xyz[,3] <- scale_range(orig.load.xyz.mod[,3], -1,1);
    
    #remove last two rows
    load.xyz <- load.xyz[1:(nrow(load.xyz) - 2), ];
    
    names <- res2$name;
    ids <- res2$entrez;
    colres <- rgba_to_hex_opacity(res2$cols);
    colorb <- colres[[1]];
    opacity_array <- colres[[2]];
    nodes2 <- vector(mode="list");
    
    for(i in 1:length(names)){
      nodes2[[i]] <- list(
        id=ids[i],
        label=names[i],
        size=24,
        cluster=1,
        opacity=opacity_array[i],
        omicstype="met",
        fx = unname(load.xyz[i,1])*1000,
        fy = unname(load.xyz[i,2])*1000,
        fz = unname(load.xyz[i,3])*1000,
        origX = unname(orig.load.xyz[i,1]),
        origY = unname(orig.load.xyz[i,2]),
        origZ = unname(orig.load.xyz[i,3]),
        seedArr = "notSeed",
        colorb=colorb[i],
        colorw=colorb[i]
      );
    }
    
    ticksLoading <- list(x=ticksX, y=ticksY, z=ticksZ);
    
    netData <- list(omicstype="", 
                    nodes=nodes,
                    meta=metadf, 
                    loading=nodes2, 
                    reductionOpt=reductionOptGlobal, 
                    misc="", ticks=ticks,
                    ticksLoading=ticksLoading,
                    axis=res$axis,
                    axisLoading=res2$axis, 
                    metaCol = list(label=unique(meta.vec),color=col.s));
  #res2$pos.xyz <- load.xyz;
  #qs::qsave(res2, "pca3d_loadings.qs");
  }
  rownames(pos.xyz) <- res$name;
  res$pos.xyz <- pos.xyz;
  .set.rdt.set(res);

  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(1);
}



# Define a function to convert RGBA to Hex and opacity values
rgba_to_hex_opacity <- function(rgba_array) {
  
  # Define an empty vector to store the hex color values
  hex_values <- vector("character", length = length(rgba_array))
  
  # Define an empty vector to store the opacity values
  opacity_values <- vector("numeric", length = length(rgba_array))
  
  # Loop through each element in the input array
  for (i in seq_along(rgba_array)) {
    rgba <- rgba_array[i]
    rgba <- gsub("rgba\\(", "",rgba);
    rgba <- gsub("\\)", "",rgba);
    # Convert the RGBA value to a list of numeric values
    rgba <- strsplit(rgba, ",")[[1]]
    
    rgba <- as.numeric(rgba)
    # Convert the RGB values to hexadecimal notation
    hex <- rgb(rgba[1], rgba[2], rgba[3], maxColorValue = 255)
    hex_values[i] <- hex
    
    # Extract the opacity value from the RGBA string
    opacity_values[i] <- rgba[4]
  }
  
  # Return a list containing the hex color values and opacity values
  return(list(hex_values = hex_values, opacity_values = opacity_values))
}


scale_range <- function(x, new_min = 0, new_max = 1) {
  range <- pretty(x,10);
  old_min <- min(range);
  old_max <- max(range);
  
  
  (x - old_min) / (old_max - old_min) * (new_max - new_min) + new_min
}


ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){


  level <- as.numeric(level)
  names = strsplit(names.vec, "; ")[[1]]

  res <- .get.rdt.set();
  res <- res$pos.xyz
  pos.xyz <- res

  inx = rownames(pos.xyz) %in% names;
  coords = as.matrix(pos.xyz[inx,c(1:3)])
  mesh = list()
  if(type == "alpha"){
    library(alphashape3d)
    library(rgl)
    sh=ashape3d(coords, 1.0, pert = FALSE, eps = 1e-09);
    mesh[[1]] = as.mesh3d(sh, triangles=T);
  }else if(type == "ellipse"){
    library(rgl);
    pos=cov(coords, y = NULL, use = "everything");
    mesh[[1]] = ellipse3d(x=as.matrix(pos), level=level);
  }else{
    library(ks);
    res=kde(coords);
    r = plot(res, cont=level*100, display="rgl");
    sc = scene3d();
    mesh = sc$objects;
  }
  library(RJSONIO);
  sink(filenm);
  cat(toJSON(mesh));
  sink();
  return(filenm);
}

.get.rdt.set <- function(){
  return(qs::qread("rdt.set.qs"));
}

.set.rdt.set <- function(my.set){
  qs::qsave(my.set, file="rdt.set.qs");
}