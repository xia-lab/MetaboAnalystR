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
    if(vc > 3000) {
      pos.xy <- layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- layout.lgl(g, maxiter = 150);
    }else if(vc > 1000) {
      pos.xy <- layout.lgl(g, maxiter = 200);
    }else if(vc < 150){
      pos.xy <- layout.kamada.kawai(g);
    }else{
      pos.xy <- layout.fruchterman.reingold(g);
    }
  }else if(algo == "FrR"){
    pos.xy <- layout.fruchterman.reingold(g);
  }else if(algo == "random"){
    pos.xy <- layout.random(g);
  }else if(algo == "lgl"){
    if(vc > 3000) {
      pos.xy <- layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- layout.lgl(g, maxiter = 150);
    }else {
      pos.xy <- layout.lgl(g, maxiter = 200);
    }
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
