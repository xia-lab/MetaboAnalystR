#'Plot metabolome pathway
#'@description Plot KEGG pathway graph
#'@param mSetObj Input name of the created mSet Object
#'@param pathName Input the name of the selected pathway
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the created plot.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@importFrom httr content POST
#'
PlotKEGGPath <- function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=NULL){

  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_kegggraph()
    load_rgraphwiz()
    
    if(mSetObj$analSet$type == "pathinteg"){
      return(PlotInmexPath(mSetObj, pathName, width, height, format, dpi));
    }else{
      return(PlotMetpaPath(mSetObj, pathName, width, height, format, dpi));
    }
    
    # plotting via microservice   
  }else{
    
    mSetObj$api$analType <- mSetObj$analSet$type
    
    if(is.null(dpi)){
      dpi <- 72
    }
    
    if(is.na(width)){
      width <- 8;
    }
    
    if(is.na(height)){
      height <- 8;
    }
    
    # first need to post to create image on server
    if(mSetObj$api$analType == "pathinteg"){
      toSend <- list(guestName = mSetObj$api$guestName, analType = mSetObj$api$analType, pathName = pathName,
                     width = width, height = height, format = format, dpi = dpi, pathintegImpMatName = mSetObj$dataSet$pathinteg.impMat[,1],
                     pathintegImpMatFC = mSetObj$dataSet$pathinteg.impMat[,2])
    }else{
      toSend <- list(guestName = mSetObj$api$guestName, analType = mSetObj$api$analType, pathName = pathName,
                     width = width, height = height, format = format, dpi = dpi)
    }
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/createimage/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    query_results_text <- httr::content(query_results, "text", encoding = "UTF-8")
    query_results_json <- RJSONIO::fromJSON(query_results_text, flatten = TRUE)
    mSetObj$api$imageName <- query_results_json$plotName
    
    if(is.null(mSetObj$api$imageName)){
      AddErrMsg("Error! Unable to connect to api.metaboanalyst.ca!")
      return(0)
    }
    
    # second need to get image from server
    endpoint_image <- paste0("/getFile/", mSetObj$api$guestName, "/", mSetObj$api$imageName)
    image_call <- paste(base, endpoint_image, sep="")
    download.file(image_call, destfile = basename(mSetObj$api$imageName))
    print(paste0(mSetObj$api$imageName, " saved to current working directory!"))
    
    return(.set.mSet(mSetObj));
  }
}

#'Plot KEGG pathway
#'@param mSetObj Input name of the created mSet Object
#'@param pathName Input the name of the selected KEGG pathway
#'@param format Select the image format, "png", or "pdf". 
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotMetpaPath<-function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=NULL){
  
  path.id <- metpa$path.ids[pathName];
  g <- metpa$graph.list[[path.id]];
  tooltip <- names(KEGGgraph::nodes(g));
  
  nm.vec <- NULL;
  
  fillcolvec <- rep("lightblue", length(KEGGgraph::nodes(g)));
  pvec <- histvec <- rep("NA", length(KEGGgraph::nodes(g)));
  names(tooltip) <- names(fillcolvec) <- names(histvec) <- names(pvec) <- KEGGgraph::nodes(g);
  
  if(mSetObj$analSet$type == "pathora"){
    if(!is.null(mSetObj$analSet$ora.hits)){
      fillcolvec[mSetObj$analSet$ora.hits[[path.id]]] <- "red";
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$ora.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$ora.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }else{
    if(!is.null(mSetObj$analSet$qea.hits)){
      hit.cmpds <- mSetObj$analSet$qea.hits[[path.id]];
      # now plotting summary graphs for each compounds
      for(i in 1:length(hit.cmpds)){
        cmpd <- hit.cmpds[i];
        histvec[cmpd] <- cmpd;
        cmpd.name <- paste(cmpd, ".png", sep="");
        Cairo::Cairo(file=cmpd.name, width=180, height=180, bg = "transparent", type="png");
        # remember to change jscode for image size when the change the size above
        par(mar=c(4,4,1,1));
        
        y.label <- GetAbundanceLabel(mSetObj$dataSet$type);
        if(is.factor(mSetObj$dataSet$cls)){
          cls.lbls <- mSetObj$dataSet$cls;
          if(max(nchar(levels(mSetObj$dataSet$cls))) > 6){
            cls.lbls <- as.factor(abbreviate(as.character(cls.lbls), 6));
          }
          boxplot(mSetObj$dataSet$norm.path[, cmpd]~cls.lbls, col= unique(GetColorSchema(mSetObj)), ylab=y.label, las=2);
        }else{
          Rgraphviz::plot(mSetObj$dataSet$norm.path[, cmpd], mSetObj$dataSet$cls, pch=19, col="forestgreen", xlab="Index", ylab=y.label);
          abline(lm(mSetObj$dataSet$cls~mSetObj$dataSet$norm.path[, cmpd]), col="red")
        }
        dev.off();
        nm.vec <- c(nm.vec, cmpd.name);
      }
      
      pvals <- mSetObj$analSet$qea.univp[hit.cmpds];
      pvec[hit.cmpds] <- pvals;
      
      bg.vec <- heat.colors(length(pvals));
      
      # reorder the colors according to the ordered p values
      ord.inx <- match(pvals, sort(pvals));
      fillcolvec[hit.cmpds] <- bg.vec[ord.inx];
      
      if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$analSet$qea.filtered.mset)){
        fillcolvec[!(names(fillcolvec) %in% mSetObj$analSet$qea.filtered.mset[[path.id]])]<- "lightgrey";
      }
    }
  }
  
  if(is.null(dpi)){
    if(is.null(mSetObj$analSet$node.imp) || mSetObj$analSet$node.imp == "rbc"){
      impvec <- metpa$rbc[[path.id]];
    }else{
      impvec <- metpa$dgr[[path.id]];
    }
    
    imgName <- paste(pathName, ".png", sep="");
    
    ## Open plot device
    Cairo::Cairo(file=imgName, width=width, height=height, type="png", bg="white");
    par(mai=rep(0,4));
    g.obj <- plot(g, nodeAttrs = setRendAttrs(g, fillcolor=fillcolvec));
    nodeInfo <- GetMetPANodeInfo(pathName, g.obj, tooltip, histvec, pvec, impvec, width, height);
    dev.off();
    mSetObj$imgSet$current.metpa.graph <- g.obj;
    mSetObj$analSet$nodeInfo <- nodeInfo;
    
    if(.on.public.web){
      .set.mSet(mSetObj);
      return(nodeInfo);
    }else{
      return(.set.mSet(mSetObj));
    }
  }else{
    pathName <- gsub("\\s","_", pathName);
    pathName <- gsub(",","", pathName);
    
    imgName = paste(pathName, "_dpi", dpi, ".", format, sep="");
    
    if(is.na(width)){
      width <- 8;
    }
    w <- h <- width;
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mai=rep(0,4));
    g.obj <- plot(g, nodeAttrs = setRendAttrs(g, fillcolor=fillcolvec));
    dev.off();
    
    return(imgName);
  }
}

# Used in higher function

GetMetPANodeInfo<-function(pathName, object, tags, histvec, pvec, impvec, width, height, usr = par("usr")){
  
  nn = sapply(Rgraphviz::AgNode(object), function(x) x@name);
  
  ## transform user to pixel coordinates
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]); stopifnot(all(rx>=0&rx<=1)); return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]); stopifnot(all(ry>=0&ry<=1)); return(ry*height) }
  
  nxy = Rgraphviz::getNodeXY(object);
  nh  = Rgraphviz::getNodeHeight(object)/2;
  xl  = floor(x.u2p( nxy$x - Rgraphviz::getNodeLW(object) ));
  xr  = ceiling(x.u2p( nxy$x + Rgraphviz::getNodeRW(object)));
  yu  = floor(y.u2p( nxy$y - nh ));
  yl  = ceiling(y.u2p( nxy$y + nh ));
  names(xl) = names(xr) = names(yu) = names(yl) = nn;
  
  # create the javascript code
  jscode <- paste("keggPathLnk=\'<a href=\"javascript:void(0);\" onclick=\"window.open(\\'http://www.genome.jp/kegg-bin/show_pathway?", metpa$path.ids[pathName], "\\',\\'KEGG\\');\">", pathName,"</a>\'", sep="");
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

# Generate suitable features for nodes and edges
# adapted from PathRender, used in higher functions. 
# Jeff Xia \email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)

setRendAttrs = function(g, AllBorder="transparent",
                        AllFixedsize=FALSE, AllFontsize=16, AllShape="rectangle",
                        fillcolor="lightgreen", ...) {
  nn = KEGGgraph::nodes(g)
  numn = length(nn)
  color = rep(AllBorder, numn)
  names(color)=nn
  fixedsize = rep(AllFixedsize, numn)
  names(fixedsize) = nn
  if (length(fillcolor)==1) {
    fillcolvec = rep(fillcolor, numn)
    names(fillcolvec) = nn
  } else if (!identical(names(fillcolor), as.vector(KEGGgraph::nodes(g)))){
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
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'@param x Input the X
#'@param y Input the Y
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPathSummary<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, x, y){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$analSet$type == "pathora"){
    x <- mSetObj$analSet$ora.mat[,8];
    y <- mSetObj$analSet$ora.mat[,4];
    names(x) <- names(y) <- rownames(mSetObj$analSet$ora.mat);
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$ora.mat);
    }
    
  }else if(mSetObj$analSet$type == "pathqea"){
    x <- mSetObj$analSet$qea.mat[,7];
    y <- mSetObj$analSet$qea.mat[,3];
    names(x) <- names(y) <- rownames(mSetObj$analSet$qea.mat);
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$qea.mat);
    }
    
  }else if (mSetObj$analSet$type == "pathinteg"){ # this is integrative analysis
    x <-  mSetObj$dataSet$path.mat[,8];
    y <-  mSetObj$dataSet$path.mat[,4];
    names(x) <- names(y) <- rownames(mSetObj$dataSet$path.mat);
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$jointPAMatches);
    }
    
  }else{
    print(paste("Unknown analysis type: ", mSetObj$analSet$type));
    return(0);
  }
  
  # first sort values based on p
  y = -log(y);
  inx <- order(y, decreasing= T);
  x <- x[inx]; 
  y <- y[inx];
  
  # set circle size according to impact
  # take sqrt to increase spread out
  sqx <- sqrt(x);
  min.x<- min(sqx, na.rm = TRUE);
  max.x <- max(sqx, na.rm = TRUE);
  
  if(min.x == max.x){ # only 1 value
    max.x = 1.5*max.x;
    min.x = 0.5*min.x;
  }
  
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(sqx-min.x)/(max.x-min.x);
  
  # set background color according to y
  bg.vec <- heat.colors(length(y));
  
  if(.on.public.web){
    if(mSetObj$analSet$type == "pathinteg"){
      path.nms <- names(inmexpa$path.ids)[match(names(x),inmexpa$path.ids)];
    }else{
      path.nms <- names(metpa$path.ids)[match(names(x),metpa$path.ids)];
    }
  }
  
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
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op <- par(mar=c(6,5,2,3));
  plot(x, y, type="n", axes=F, xlab="Pathway Impact", ylab="-log(p)");
  axis(1);
  axis(2);
  grid(col="blue");
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  # convert to pixel positions, only for web interaction dpi=72
  if(dpi == 72){
    width.px <- height.px <- w*dpi;
    mSetObj$imgSet$circleInfo <- CalculateCircleInfo(x, y, radi.vec, width.px, height.px, path.nms);
  }
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

# Generate json file of selected pathway to visualize using sigma.js
# Jeff Xia \email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
GeneratePathwayJSON<-function(pathway.nm){
  mSetObj <- .get.mSet(mSetObj);
  
  smpdb.path <- paste("../../libs/smpdb/", mSetObj$org, ".rda", sep="");
  load(smpdb.path)
  
  jsons.path <- paste("../../libs/smpdb/jsons/", mSetObj$org, ".rds", sep="");
  smpdb.jsons <- readRDS(jsons.path) # no need to be global!
  
  if(pathway.nm == "top"){
    if(mSetObj$analSet$type == "pathora"){
      pathway.id <- rownames(mSetObj$analSet$ora.mat)[1]
    } else{
      pathway.id <- rownames(mSetObj$analSet$qea.mat)[1]
    }
    pathway.nm <- names(metpa$path.ids)[which(metpa$path.ids == pathway.id)]
  } else {
    pathway.id <- metpa$path.ids[which(names(metpa$path.ids) == pathway.nm)]
  }
  
  # Get matched metabolites
  if(mSetObj$analSet$type == "pathora"){
    metab.matches <- paste(mSetObj$analSet$ora.hits[[pathway.id]], collapse=",");
  } else{
    metab.matches <- paste(mSetObj$analSet$qea.hits[[pathway.id]], collapse=",");
  }
  
  title <- paste(pathway.id, ";", pathway.nm, sep="");
  
  # store json file  
  smpdbpw.nm <- paste("smpdb_pathway_netview", smpdbpw.count, ".json", sep="");
  smpdbpw.count <<- smpdbpw.count + 1;
  sink(smpdbpw.nm);
  cat(jsonlite::toJSON(smpdb.jsons[[pathway.id]], pretty = TRUE));
  sink();
  
  smpdbpw.nm <- paste0(smpdbpw.nm, ";", metab.matches, ";", title)
  return(smpdbpw.nm)
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Redraw current graph for zooming or clipping then return a value
#'@description Redraw current graph for zooming or clipping then return a value
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the name of the plot
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the created plot.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RerenderMetPAGraph <- function(mSetObj=NA, imgName, width, height, zoom.factor=NA){
  mSetObj <- .get.mSet(mSetObj);
  Cairo::Cairo(file=imgName, width=width, height=height,type="png", bg="white");
  if(mSetObj$analSet$type == "pathinteg"){
    font.cex <- 0.7*zoom.factor/100;
    if(font.cex < 0.6){
      font.cex=0.6;
    }
    g <- mSetObj$dataSet$current.kegg$graph;
    if(is_igraph(g)){
      g <- upgrade_graph(g);
    }
    plotGraph(g, 
              vertex.color=mSetObj$dataSet$current.kegg$bg.color, 
              vertex.frame.color=mSetObj$dataSet$current.kegg$line.color,
              vertex.label=V(mSetObj$dataSet$current.kegg$graph)$plot_name,
              vertex.label.cex=font.cex
    );
    
  }else{
    KEGGgraph::plot(mSetObj$imgSet$current.metpa.graph);
  }
  dev.off();
  return(1);
}

#' Export information about selected circle
#'@param mSetObj Input name of the created mSet Object
#'@export
GetCircleInfo<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$imgSet$circleInfo);
}

##########Utility Functions########

#'Perform utilities for MetPa
#'@description Convert user coords (as used in current plot) to pixels in a png
#'adapted from the imagemap package
#'@param xy Input coordinates
#'@param im Input coordinates
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
usr2png <- function(xy, im){
  xy <- usr2dev(xy,dev.cur())
  cbind(
    ceiling(xy[,1]*im$Width),
    ceiling((1-xy[,2])*im$Height)
  )
}

usr2plt <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  usr <- par("usr")
  dev.set(olddev)
  xytrans(xy,usr)
}

plt2fig <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  plt <- par("plt")
  dev.set(olddev)
  xytrans2(xy,plt)
}

fig2dev <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  fig <- par("fig")
  dev.set(olddev)
  xytrans2(xy,fig)
}

usr2dev <- function(xy, dev=dev.cur()){
  fig2dev(plt2fig(usr2plt(xy,dev),dev),dev)
}

xytrans2 <- function(xy, par){
  cbind(par[1]+((par[2]-par[1])*xy[,1]),
        par[3]+((par[4]-par[3])*xy[,2]))
}

xytrans <- function(xy, par){
  cbind((xy[,1]-par[1])/(par[2]-par[1]),
        (xy[,2]-par[3])/(par[4]-par[3]))
}

getndp <- function(x, tol=2*.Machine$double.eps){
  ndp <- 0
  while(!isTRUE(all.equal(x, round(x, ndp), tol=tol))) ndp <- ndp+1
  if(ndp > -log10(tol)) {
    warning("Tolerance reached, ndp possibly underestimated.")
  }
  ndp
}