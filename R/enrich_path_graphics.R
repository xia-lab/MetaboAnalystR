#'Plot metabolome pathway
#'@description Plot KEGG pathway graph
#'@param mSetObj Input name of the created mSet Object
#'@param pathName Input the name of the selected pathway
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the created plot.
#'@param format format of the image.
#'@param dpi dpi of the image.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotKEGGPath <- function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=default.dpi){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_kegggraph()
    load_rgraphwiz()
    
    if(mSetObj$analSet$type == "pathinteg"){
      return(PlotInmexPath(mSetObj, pathName, width, height, format, dpi));
    }else{
      return(PlotMetpaPath(mSetObj, pathName, width, height, format, dpi));
    }
      
  }else{     
    
   # plotting via microservice  
   # make this lazy load
    if(!exists("my.kegg.plot")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }

    mSetObj$api$analType <- mSetObj$analSet$type
    
    # first need to post to create image on server
    if(mSetObj$api$analType == "pathinteg"){
      toSend <- list(mSet = mSetObj, analType = mSetObj$api$analType, pathName = pathName,
                     pathintegImpMatName = mSetObj$dataSet$pathinteg.impMat[,1],
                     pathintegImpMatFC = mSetObj$dataSet$pathinteg.impMat[,2],
                     libNm = mSetObj$api$libNm, dpi = dpi, format = format)
    }else{
      toSend <- list(mSet = mSetObj, analType = mSetObj$api$analType, 
                     libNm = mSetObj$api$libNm, pathName = pathName,
                     dpi = dpi, format = format)
    }
    saveRDS(toSend, "tosend.rds");
    return(my.kegg.plot(dpi = dpi, format = format, width = width, height = height));
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
#'@param height height value for the image.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotMetpaPath<-function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=default.dpi){
  
  path.id <- current.kegglib$path.ids[pathName];
  g <- current.kegglib$graph.list[[path.id]];
  tooltip <- names(KEGGgraph::nodes(g));
  
  nm.vec <- NULL;
  
  fillcolvec <- rep("lightblue", length(KEGGgraph::nodes(g)));
  pvec <- histvec <- rep("NA", length(KEGGgraph::nodes(g)));
  names(tooltip) <- names(fillcolvec) <- names(histvec) <- names(pvec) <- KEGGgraph::nodes(g);
  
  if(!is.null(mSetObj$analSet$ora.hits)){
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
      print(head(mSetObj$dataSet$norm.path));
      for(i in 1:length(hit.cmpds)){
        cmpd <- hit.cmpds[i];
        histvec[cmpd] <- cmpd;
        cmpd.name <- paste(cmpd, ".png", sep="");
        Cairo::Cairo(file=cmpd.name, width=220, height=260, bg = "transparent", type="png");
        # remember to change jscode for image size when the change the size above
        par(mar=c(4,3,3,2));
        
        y.label <- GetAbundanceLabel(mSetObj$dataSet$type);

        cmpd.label <- tooltip[cmpd];

        if(is.factor(mSetObj$dataSet$cls)){
          cls.lbls <- mSetObj$dataSet$cls;
          if(max(nchar(levels(mSetObj$dataSet$cls))) > 6){
            cls.lbls <- as.factor(abbreviate(as.character(cls.lbls), 6));
          }

          boxplot(mSetObj$dataSet$norm.path[, cmpd]~cls.lbls, 
                main=cmpd.label,
                col=unique(GetColorSchema(cls.lbls)), ylab=y.label, xlab = "", las=2);
        }else{
          # Rgraphviz::plot(mSetObj$dataSet$norm.path[, cmpd], 
          # should be base plot? 

            plot(mSetObj$dataSet$norm.path[, cmpd], mSetObj$dataSet$cls, 
                pch=19, col="forestgreen", main=cmpd.label, xlab="Index", ylab=y.label);
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
      impvec <- current.kegglib$rbc[[path.id]];
    }else{
      impvec <- current.kegglib$dgr[[path.id]];
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
  jscode <- paste("keggPathLnk=\'<a href=\"javascript:void(0);\" onclick=\"window.open(\\'http://www.genome.jp/kegg-bin/show_pathway?", current.kegglib$path.ids[pathName], "\\',\\'KEGG\\');\">", pathName,"</a>\'", sep="");
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
#'@param show.grid logical, show grid or not
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'@param xlim limit of x axis
#'@param ylim limit of y axis
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPathSummary<-function(mSetObj=NA, 
                          show.grid, 
                          imgName, 
                          format="png", 
                          dpi=default.dpi, 
                          width=NA,
                          xlim = NA,
                          ylim = NA){
  
  mSetObj <- .get.mSet(mSetObj);

  jointGlobal <- FALSE;
  type <- mSetObj$analSet$type;
  if(!type %in% c("pathora", "pathqea", "pathinteg")){
    if(!is.null(mSetObj$analSet$ora.mat)){
        type <- "pathora";
    }else if(!is.null(mSetObj$analSet$qea.mat)){
        type <- "pathqea";
    }
  }
  if(type == "pathora"){
    x <- mSetObj$analSet$ora.mat[,8];
    y <- mSetObj$analSet$ora.mat[,4];
    names(x) <- names(y) <- rownames(mSetObj$analSet$ora.mat);
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$ora.mat);
    }

    mSetObj$analSet$path.ora.mat <- mSetObj$analSet$ora.mat;
    rownames(mSetObj$analSet$path.ora.mat)<-GetORA.pathNames(mSetObj);
  } else if(type == "pathqea") {
    x <- mSetObj$analSet$qea.mat[,7];
    y <- mSetObj$analSet$qea.mat[,3];
    names(x) <- names(y) <- rownames(mSetObj$analSet$qea.mat);
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$qea.mat);
    }
    mSetObj$analSet$path.qea.mat <- mSetObj$analSet$qea.mat
    rownames(mSetObj$analSet$path.qea.mat)<-GetQEA.pathNames(mSetObj);
  } else if (type == "pathinteg") { # this is integrative analysis
    
    jointGlobal <- !is.null(mSetObj[["mum_nm_csv"]]);
    if(jointGlobal) {
      combo.resmat <- mSetObj$dataSet$integResGlobal;
      # Sort values based on combined pvalues;
      y <- -log10(combo.resmat[,5]); # x is gene
      x <- -log10(combo.resmat[,6]); # y is cmpd
      y <- scales::rescale(y, c(0,4))
      x <- scales::rescale(x, c(0,4))
      if(min(combo.resmat[,7]) ==0){combo.resmat[,7][combo.resmat[,7] ==0] <- 
        min(combo.resmat[,7][!(combo.resmat[,7] ==0)])/2}
      combo.p <- -log10(combo.resmat[,7])
      combo.p <- scales::rescale(combo.p, c(0,4))

    } else {
      x <-  mSetObj$dataSet$path.mat[,8];
      y <-  mSetObj$dataSet$path.mat[,4];
      names(x) <- names(y) <- rownames(mSetObj$dataSet$path.mat);
    }
    
    if(!.on.public.web){
      path.nms <- rownames(mSetObj$analSet$jointPAMatches);
    }
    
  }else{
    print(paste("Unknown analysis type: ", mSetObj$analSet$type));
    return(0);
  }

        orig.y <- y;

  # first sort values based on p
  if(!jointGlobal){
    y = -log10(y);
    inx <- order(y, decreasing= T);
    x <- x[inx]; 
    y <- y[inx];
    # set circle size according to impact
    # take sqrt to increase spread out
    sqx <- sqrt(x);
    min.x<- min(sqx, na.rm = TRUE);
    max.x <- max(sqx, na.rm = TRUE);
    
    if(min.x == max.x){ # only 1 value
      radi.vec <- rep(0.075, length(x));
    }else{
      maxR <- (max.x - min.x)/40;
      minR <- (max.x - min.x)/160;
      radi.vec <- minR+(maxR-minR)*(sqx-min.x)/(max.x-min.x);
    }
    
    bg.vec <- heat.colors(length(y));

  } else {
    inx <- order(combo.p, decreasing= T);
    pathnames <- combo.resmat$pathways
    combo.p <- combo.p[inx]
    x <- x[inx]; 
    y <- y[inx];
    path.nms <- pathnames[inx];
    
    # set circle size based on combined pvalues
    min.x <- min(combo.p, na.rm = TRUE);
    max.x <- max(combo.p, na.rm = TRUE);
    
    if(min.x == max.x){ # only 1 value
      max.x = 1.5*max.x;
      min.x = 0.5*min.x;
    }
    
    maxR <- (max.x - min.x)/40;
    minR <- (max.x - min.x)/160;
    radi.vec <- minR+(maxR-minR)*(combo.p-min.x)/(max.x-min.x);
    bg.vec <- heat.colors(length(combo.p));
  }
  
  if(.on.public.web){
    if(mSetObj$analSet$type == "pathinteg"){
      if(jointGlobal) {
        path.nms <- mSetObj$dataSet$integResGlobal[,1];
      } else {
        path.nms <- names(current.kegglib$path.ids)[match(names(x),current.kegglib$path.ids)];
      }
    }else{
      path.nms <- names(current.kegglib$path.ids)[match(names(x),current.kegglib$path.ids)];
    }
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 6;
  }else if(width == 0){
    w <- 6;
  }else{
    w <- width;
  }
  h <- w;
  mSetObj$imgSet$path.overview<-imgName;
  
  if(jointGlobal){
    xlabNM = "Enriched Pathways of Genes/Proteins";
    ylabNM = "Enriched Pathways from Peaks";
  } else {
    xlabNM = "Pathway Impact";
    ylabNM = "-log10(p)";
  }
  
  if(is.na(xlim)) {
    max_x <- max(x)
  } else {
    if(xlim > max(x)){
      max_x <- xlim;
    } else {
      max_x <- max(x)
    }
  }
  
  if(is.na(ylim)){
    max_y <- max(y)
  } else {
    if(ylim > max(y)){
      max_y <- ylim;
    } else {
      max_y <- max(y)
    }
  }
  
  # Create a data frame for ggplot
  data <- data.frame(x = x, y = y, radi = radi.vec, color = bg.vec, path = path.nms)
  data$pval <- orig.y; 
  mSetObj$analSet$pathSummaryDf <- data;

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op <- par(mar=c(6,5,2,3));
  plot(x, 
       y, 
       type="n", 
       axes=F, 
       xlab=xlabNM, 
       ylab=ylabNM,
       xlim = c(min(x), max_x),
       ylim = c(min(y), max_y)
  );
  axis(1);
  axis(2);
  if(show.grid){
    grid(col="blue");
  }
  
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  # convert to pixel positions, only for web interaction dpi=default.dpi
  #if(dpi == 72){
    width.px <- height.px <- w*100;
    mSetObj$imgSet$circleInfo <- CalculateCircleInfo(x, y, radi.vec, width.px, height.px, path.nms);
  #}
  par(op);
  dev.off();

  tbl <- CreatePathwayMemberTableRMD(mSetObj);
  mSetObj$analSet$pathwayMemberTable <- tbl
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
  
  smpdb.path <- paste(rpath ,"libs/smpdb/", mSetObj$org, ".qs", sep="");
  current.kegglib <- qs::qread(smpdb.path);
  
  jsons.path <- paste(rpath ,"libs/smpdb/jsons/", mSetObj$org, ".qs", sep="");
  smpdb.jsons <- qs::qread(jsons.path) # no need to be global!
  
  if(pathway.nm == "top"){
    if(mSetObj$analSet$type == "pathora"){
      pathway.id <- rownames(mSetObj$analSet$ora.mat)[1]
    } else{
      pathway.id <- rownames(mSetObj$analSet$qea.mat)[1]
    }
    pathway.nm <- names(current.kegglib$path.ids)[which(current.kegglib$path.ids == pathway.id)]
  } else {
    pathway.id <- current.kegglib$path.ids[which(names(current.kegglib$path.ids) == pathway.nm)]
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
  dat <- smpdb.jsons[[pathway.id]];
  dat$nodes <- unname(apply(dat$nodes,1, function(x){as.vector(x)}))
  edges <- list()
  for(i in 1:nrow(dat$edges)){
    edges[[i]] <- list(
      source=dat$edges$source[i],
      target=dat$edges$target[i],
      type=dat$edges$type[i]
    )
  }
  dat$edges <- edges
  sink(smpdbpw.nm);
  cat(RJSONIO::toJSON(dat));
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
#'@param zoom.factor zoom factor, numeric
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RerenderMetPAGraph <- function(mSetObj=NA, imgName, width, height, zoom.factor=NA){
  mSetObj <- .get.mSet(mSetObj);
  library(igraph);

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

PlotPathSummaryGG <- function(mSetObj = NA, 
                              show.grid = FALSE, 
                              imgName = "plot", 
                              format = "png", 
                              dpi = default.dpi, 
                              width = NA, 
                              height = NA, 
                              xlim = NA, 
                              ylim = NA,
                              interactive=F) {
  library(ggplot2)
  library(scales)
  
  mSetObj <- .get.mSet(mSetObj);
  
  # Get data frame for ggplot
  data <- mSetObj$analSet$pathSummaryDf;
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
  
  jointGlobal <- FALSE;
  if (mSetObj$analSet$type == "pathinteg") { # this is integrative analysis
    jointGlobal <- !is.null(mSetObj[["mum_nm_csv"]]);
  }
  if(jointGlobal){
    xlabNM = "Enriched Pathways of Genes/Proteins";
    ylabNM = "Enriched Pathways from Peaks";
  } else {
    xlabNM = "Pathway Impact";
    ylabNM = "-log10(p)";
  }
  
  x<- data$x;
  y<- data$y;

  if(is.na(xlim)) {
    max_x <- max(x)
  } else {
    if(xlim > max(x)){
      max_x <- xlim;
    } else {
      max_x <- max(x)
    }
  }
  
  if(is.na(ylim)){
    max_y <- max(y)
  } else {
    if(ylim > max(y)){
      max_y <- ylim;
    } else {
      max_y <- max(y)
    }
  }
  
if(ylabNM == "-log10(p)"){
    ytooltipNm <- "P-value";
    ytooltipVal <- data$pval;
}else{
    ytooltipNm <- ylabNM;
    ytooltipVal <- data$y;

}

  data$text <- paste(" Pathway:", data$path, 
                            "<br>",xlabNM, ":", round(data$x, 3), 
                            "<br>",ytooltipNm, ":", signif(ytooltipVal, 3))
  
  if(interactive){
    library(plotly);
    sizeref <- max(data$radi) / 25
    ggp <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
                   marker = list(
                     size = ~radi, 
                     sizeref = sizeref,
                     color = ~y, 
                     colorscale = 'Heat',
                     line = list(color = 'black', width = 1),  # Add black outline
                     colorbar = list(
                       title = ylabNM,
                       len = 0.5  # Adjust the length of the colorbar
                     ),
                     showscale = TRUE
                   ),
                   text = ~text,  # Add tooltip
                   hoverinfo = 'text') %>%
      layout(
        autosize = FALSE, 
        xaxis = list(
          title = xlabNM,
          range = if (!is.na(xlim)) c(min(data$x), max(data$x)) else NULL,
          showgrid = show.grid,
          gridcolor = if (show.grid) "blue" else NULL
        ),
        yaxis = list(
          title = ylabNM,
          range = if (!is.na(ylim)) c(min(data$y), max(data$y)) else NULL,
          showgrid = show.grid,
          gridcolor = if (show.grid) "blue" else NULL
        ),
        width = 800, 
        height = 600
      )
    return(ggp)
  }else{
    
    p <- ggplot(data, aes(x = x, y = y, size = radi, fill = y, label = path, text = text)) +
      geom_point(shape=21) + # 21 is filled circle
      scale_size_continuous(range = c(2, 10),name=xlabNM) +
      scale_fill_gradient(low = "#FFFFED", high = "#FF0000", name=ylabNM) +
      labs(x = xlabNM, y = ylabNM) +
      theme_bw()

    if (!is.na(xlim)) {
      p <- p + xlim(min(x), max(x))
    }
    
    if (!is.na(ylim)) {
      p <- p + ylim(min(y), max(y))
    }
    
    if (show.grid) {
      p <- p + theme(panel.grid.major = element_line(colour = "blue"),
                     panel.grid.minor = element_line(colour = "blue"))
    } else {
      p <- p + theme(panel.grid = element_blank())
    }
    
    # Adjust width and height
    if (is.na(width)) {
      plot_width <- 7
    } else {
      plot_width <- width
    }
    
    if (is.na(height)) {
      plot_height <- plot_width
    } else {
      plot_height <- height
    }
    # Save the plot
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    print(p);
    dev.off();
  }
  
  return(.set.mSet(mSetObj))
}