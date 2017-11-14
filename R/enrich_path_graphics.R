#'R script for generating various graphics for pathway analysis
#'
#'Plot metabolome pathway
#'@description Orthogonal PLS-DA (from ropls)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PlotMetPath <- function(mSetObj=NA, pathName, width, height){
  
  mSetObj <- .get.mSet(mSetObj);
  
  library('KEGGgraph');
  library('Rgraphviz');
  path.id<-metpa$path.ids[pathName];
  g <- metpa$graph.list[[path.id]];
  tooltip <- names(nodes(g));
  
  nm.vec <- NULL;
  
  fillcolvec <- rep("lightblue", length(nodes(g)));
  pvec <- histvec <- rep("NA", length(nodes(g)));
  names(tooltip) <- names(fillcolvec) <- names(histvec) <- names(pvec) <- nodes(g);
  
  if(mSetObj$analSet$type == "pathora"){
    if(!is.null(mSetObj$analSet$ora.hits)){
      fillcolvec[mSetObj$analSet$ora.hits[[path.id]]] <- "red";
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$ora.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$ora.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }else{
    if(!is.null(mSetObj$analSet$qea.hits)){
      hit.cmpds<-mSetObj$analSet$qea.hits[[path.id]];
      # now plotting summary graphs for each compounds
      for(i in 1:length(hit.cmpds)){
        cmpd <- hit.cmpds[i];
        histvec[cmpd] <- cmpd;
        cmpd.name <- paste(cmpd, ".png", sep="");
        Cairo(file=cmpd.name, width=180, height=180, bg = "transparent", type="png");
        # remember to change jscode for image size when the change the size above
        par(mar=c(4,4,1,1));
        
        y.label<-GetValueLabel(mSetObj);
        if(is.factor(mSetObj$dataSet$cls)){
          cls.lbls <- mSetObj$dataSet$cls;
          if(max(nchar(levels(mSetObj$dataSet$cls))) > 6){
            cls.lbls <- as.factor(abbreviate(as.character(cls.lbls), 6));
          }
          boxplot(mSetObj$dataSet$norm.path[, cmpd]~cls.lbls, col= unique(GetColorSchema(mSetObj)), ylab=y.label, las=2);
        }else{
          plot(mSetObj$dataSet$norm.path[, cmpd], mSetObj$dataSet$cls, pch=19, col="forestgreen", xlab="Index", ylab=y.label);
          abline(lm(mSetObj$dataSet$cls~mSetObj$dataSet$norm.path[, cmpd]), col="red")
        }
        dev.off();
        nm.vec <- c(nm.vec, cmpd.name);
      }
      
      pvals <- mSetObj$analSet$qea.univp[hit.cmpds];
      pvec[hit.cmpds]<-pvals;
      
      bg.vec <- heat.colors(length(pvals));
      
      # reorder the colors according to the ordered p values
      ord.inx<-match(pvals, sort(pvals));
      fillcolvec[hit.cmpds] <- bg.vec[ord.inx];
      
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$qea.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$qea.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }
  
  if(is.null(mSetObj$analSet$node.imp) || mSetObj$analSet$node.imp == "rbc"){
    impvec <- metpa$rbc[[path.id]];
  }else{
    impvec <- metpa$dgr[[path.id]];
  }
  
  imgName <- paste(pathName, ".png", sep="");
  
  ## Open plot device
  Cairo(file=imgName, width=width, height=height, type="png", bg="white");
  par(mai=rep(0,4));
  g.obj <- plot(g, nodeAttrs = setRendAttrs(g, fillcolor=fillcolvec));
  nodeInfo <- GetMetPANodeInfo(pathName, g.obj, tooltip, histvec, pvec, impvec, width, height);
  dev.off();
  mSetObj$imgSet$current.metpa.graph <- g.obj;
  mSetObj$analSet$nodeInfo <- nodeInfo;
  
  return(.set.mSet(mSetObj));
  
}

#'Plot KEGG pathway
#'@description Orthogonal PLS-DA (from ropls)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PlotKEGGPath<-function(mSetObj=NA, pathName, format="png", width=NA, dpi=72){
  
  library('KEGGgraph');
  library('Rgraphviz');
  
  path.id<-metpa$path.ids[pathName];
  g <- metpa$graph.list[[path.id]];
  tooltip <- names(nodes(g));
  
  nm.vec <- NULL;
  
  fillcolvec <- rep("lightblue", length(nodes(g)));
  pvec <- histvec <- rep("NA", length(nodes(g)));
  names(tooltip) <- names(fillcolvec) <- names(histvec) <- names(pvec) <- nodes(g);
  
  if(mSetObj$analSet$type == "pathora"){
    if(!is.null(mSetObj$analSet$ora.hits)){
      fillcolvec[mSetObj$analSet$ora.hits[[path.id]]] <- "red";
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$ora.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$ora.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }else{
    if(!is.null(mSetObj$analSet$qea.hits)){
      hit.cmpds<-mSetObj$analSet$qea.hits[[path.id]];
      
      pvals <- mSetObj$analSet$qea.univp[hit.cmpds];
      pvec[hit.cmpds]<-pvals;
      
      bg.vec <- heat.colors(length(pvals));
      
      # reorder the colors according to the ordered p values
      ord.inx<-match(pvals, sort(pvals));
      fillcolvec[hit.cmpds] <- bg.vec[ord.inx];
      
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$qea.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$qea.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }
  
  pathName <- gsub("\\s","_", pathName);
  pathName <- gsub(",","", pathName);
  
  imgName = paste(pathName, "_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    width <- 8;
  }
  w <- h <- width;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mai=rep(0,4));
  g.obj <- plot(g, nodeAttrs = setRendAttrs(g, fillcolor=fillcolvec));
  dev.off();
  mSetObj$imgSet$kegg.graph.opls <- g.obj
  print(imgName);
  return(.set.mSet(mSetObj));
}

# Used in higher function

GetMetPANodeInfo<-function(pathName, object, tags, histvec, pvec, impvec, width, height, usr = par("usr")){
  
  nn = sapply(AgNode(object), function(x) x@name);
  
  ## transform user to pixel coordinates
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]); stopifnot(all(rx>=0&rx<=1)); return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]); stopifnot(all(ry>=0&ry<=1)); return(ry*height) }
  
  nxy = getNodeXY(object);
  nh  = getNodeHeight(object)/2;
  xl  = floor(x.u2p( nxy$x - getNodeLW(object) ));
  xr  = ceiling(x.u2p( nxy$x + getNodeRW(object)));
  yu  = floor(y.u2p( nxy$y - nh ));
  yl  = ceiling(y.u2p( nxy$y + nh ));
  names(xl) = names(xr) = names(yu) = names(yl) = nn;
  
  # create the javascript code
  jscode <-paste("keggPathLnk=\'<a href=\"javascript:void(0);\" onclick=\"window.open(\\'http://www.genome.jp/kegg-bin/show_pathway?", metpa$path.ids[pathName], "\\',\\'KEGG\\');\">", pathName,"</a>\'", sep="");
  tag.ids <- names(tags);
  kegg.ids <- names(tags);
  hmdb.ids <- KEGGID2HMDBID(kegg.ids);
  for(i in 1:length(tag.ids)) {
    nd <- tag.ids[i];
    x1 <- floor(100*(xl[nd])/width);
    x2 <- ceiling(100*(xr[nd])/width);
    y1 <- floor(100*(yl[nd])/height);
    y2 <- ceiling(100*(yu[nd])/height);
    
    #add code for mouseover locations, basically the annotation info
    #in this case, the name of the node 
    jscode <- paste(jscode, paste("rectArray.push({x1:", x1, ", y1:", y1, ", x2:", x2, ", y2:", y2, 
                                  ", lb: \"", tags[i], "\", kegg: \"", kegg.ids[i],  "\", hmdb: \"", hmdb.ids[i],
                                  "\", icon: \"", histvec[i], "\", pvalue: \"", pvec[i], "\", impact: \"", impvec[i], "\"})", sep=""), sep="\n");
  }
  return(jscode);
}

#'Generate suitable features for nodes and edges
#'@description adapted from PathRender, used in higher functions. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

setRendAttrs = function(g, AllBorder="transparent",
                        AllFixedsize=FALSE, AllFontsize=16, AllShape="rectangle",
                        fillcolor="lightgreen", ...) {
  nn = nodes(g)
  numn = length(nn)
  color = rep(AllBorder, numn)
  names(color)=nn
  fixedsize = rep(AllFixedsize, numn)
  names(fixedsize) = nn
  if (length(fillcolor)==1) {
    fillcolvec = rep(fillcolor, numn)
    names(fillcolvec) = nn
  } else if (!identical(names(fillcolor), as.vector(nodes(g)))){
    stop("names on vector fillcolor must match nodes(g) exactly")
  } else {
    fillcolvec = fillcolor
  }
  shape = rep(AllShape, numn)
  names(shape) = nn
  fontsize = rep(AllFontsize, numn)
  names(fontsize) = nn;
  list(color=color, fixedsize=fixedsize, fillcolor=fillcolvec, shape=shape,
       fontsize=fontsize )
}

#'Plot a scatterplot (circle) overview of the matched pathways
#'@description x axis is the pathway impact factor
#'y axis is the p value (from ORA or globaltest) 
#'return the circle information
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PlotPathSummary<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, x, y){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$analSet$type == "pathora"){
    x <- mSetObj$analSet$ora.mat[,8];
    y <- mSetObj$analSet$ora.mat[,4];
  }else{
    x <- mSetObj$analSet$qea.mat[,7];
    y <- mSetObj$analSet$qea.mat[,3];
  }
  
  # first sort values based on p
  y = -log(y);
  inx <- order(y, decreasing= T);
  x <- x[inx]; 
  y <- y[inx];
  
  # set circle size according to impact
  # take sqrt to increase spread out
  sqx <- sqrt(x);
  min.x<- min(sqx);
  max.x <- max(sqx);
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(sqx-min.x)/(max.x-min.x);
  
  # set background color according to y
  bg.vec <- heat.colors(length(y));
  
  ## Open plot device
  if(format == "png"){
    bg = "transparent";
  }else{
    bg="white";
  }
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$path.overview<-imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op<-par(mar=c(6,5,2,3));
  plot(x, y, type="n", axes=F, xlab="Pathway Impact", ylab="-log(p)");
  axis(1);
  axis(2);
  grid(col="blue");
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  # convert to pixel positions
  width.px <- height.px <- w*dpi;
  mSetObj$imgSet$circleInfo <- CalculateCircleInfo(x, y, radi.vec, width.px, height.px, names(metpa$path.ids)[match(names(x),metpa$path.ids)]);
  par(op);
  dev.off();
  return(.set.mSet(mSetObj));
}

# Used in higher function
CalculateCircleInfo <- function(x, y, r, width, height, lbls){
  jscode <- paste("leftImgWidth = ", width, "\n", "leftImgHeight = ", height, sep="");
  dot.len <- length(r);
  for(i in 1:dot.len){
    xy <- cbind(c(x[i],r[i],0),c(y[i],0,0));
    xy <- usr2dev(xy,dev.cur());
    xyrc <- cbind(ceiling(xy[,1]*width),ceiling((1-xy[,2])*height));
    radius <- abs(xyrc[2,1]-xyrc[3,1]);
    
    #add code for mouseover locations, basically the annotation info
    #in this case, the name of the node
    jscode <- paste(jscode, paste("circleArray.push({xc:", xyrc[1,1], ", yc:", xyrc[1,2], 
                                  ", r:", radius, ", lb: \"", lbls[i], "\"})", sep=""), sep="\n");
  }
  return(jscode);
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Redraw current graph for zooming or clipping then return a value
#'@description Redraw current graph for zooming or clipping then return a value
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

RerenderMetPAGraph <- function(mSetObj=NA, imgName, width, height){
  mSetObj <- .get.mSet(mSetObj);
  Cairo(file=imgName, width=width, height=height,type="png", bg="white");
  plot(mSetObj$imgSet$current.metpa.graph);
  dev.off();
  return(1);
}

GetCircleInfo<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$imgSet$circleInfo);
}

