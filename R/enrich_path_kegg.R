#'Load KEGG library
#'@description Load KEGG library
#'@param libOpt KEGG library option, "integ" for integrative, "genetic" for genetic, and "met" for metabolic
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
LoadKEGGLib<-function(libOpt){
  
  if(.on.public.web){
    load_igraph()
    if(libOpt == "integ"){
      kegg.rda <- paste("../../libs/kegg/integrative/", pathinteg.org, ".rda", sep=""); 
    }else if(libOpt == "genetic"){
      kegg.rda <- paste("../../libs/kegg/genetic/", pathinteg.org, ".rda", sep=""); 
    }else{
      kegg.rda <- paste("../../libs/kegg/metabolic/", pathinteg.org, ".rda", sep=""); 
    }
  }else{
    if(libOpt == "integ"){
      kegg.rda <- paste("http://www.metaboanalyst.ca/resources/libs/kegg/integrative/", pathinteg.org, ".rda", sep=""); 
    }else if(libOpt == "genetic"){
      kegg.rda <- paste("http://www.metaboanalyst.ca/resources/libs/kegg/genetic/", pathinteg.org, ".rda", sep=""); 
    }else{
      kegg.rda <- paste("http://www.metaboanalyst.ca/resources/libs/kegg/metabolic/", pathinteg.org, ".rda", sep=""); 
    }
  }
  
  print(paste("adding library:", kegg.rda));
  
  destfile <- paste(pathinteg.org, ".rda", sep = "")
  
  if(.on.public.web){
    load(kegg.rda, .GlobalEnv);
  }else if(!file.exists(destfile)){
    download.file(kegg.rda, destfile);
    load(destfile, .GlobalEnv);
  }else{
    load(destfile, .GlobalEnv);  
  }
  
  # now need to set up gene/cmpd universe that covered by the kegg
  # mSetObj$dataSet$current.mset <- inmexpa$mset.list;
  # mSetObj$dataSet$current.mset.list <- lapply(mSetObj$dataSet$current.mset, function(x){strsplit(x, " ")});
  # mSetObj$dataSet$current.universe <- unique(unlist(mSetObj$dataSet$current.mset.list));
  # inmexpa <<- inmexpa
  
  #if(!.on.public.web){
  #  return(.set.mSet(mSetObj));
  #}else{
  #  .set.mSet(mSetObj)
  #}
}

#'Plot integrated methods pathway analysis
#'@description Only update the background info for matched node
#'@usage PlotInmexPath(mSetObj=NA, path.id, width, height)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param path.id Input the ID of the pathway to plot. 
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the image to create.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import igraph  
#'
PlotInmexPath <- function(mSetObj=NA, path.id, width, height){

  mSetObj <- .get.mSet(mSetObj);
  
  g <- inmexpa$graph.list[[path.id]]
  g <- upgrade_graph(g); # to fix warning
  phits <- mSetObj$dataSet$path.hits[[path.id]];
  topo <- mSetObj$dataSet$pathinteg.impTopo[[path.id]];
  
  # obtain up/down/stat information
  res <- mSetObj$dataSet$pathinteg.impMat;
  
  bg.cols <- rep("#E3E4FA", length(V(g)));
  line.cols <- rep("dimgray", length(V(g)));
  
  # now, do color schema - up red, down green
  nd.inx <- which(phits);
  
  # fill with 'NA'
  stats <- vector(mode='list', length=length(V(g)));
  
  rnms <- rownames(res);
  for(inx in nd.inx){
    nm <- unlist(strsplit(V(g)$names[inx], " "));
    hit.inx <- which(rnms %in% nm)[1];
    if(length(hit.inx) > 0){
      # use logFCs to decide up/down regulated
      if(res$logFC[hit.inx] > 0){
        bg.cols[inx]<- "#F75D59";
        line.cols[inx] <- "#C11B17";
      }else if(res$logFC[hit.inx] == 0){
        bg.cols[inx]<- "#FFFF77";
        line.cols[inx] <- "#F7E259";
      }else{
        bg.cols[inx]<- "#6AFB92";
        line.cols[inx] <- "#347235";
      }
      
      # 1) update the node info (tooltip/popup) 
      V(g)$db.lnks[inx] <- paste("<a href='http://www.genome.jp/dbget-bin/www_bget?", rownames(res)[hit.inx],
                                  "' target='_blank'>", res$Name[hit.inx], "</a>", sep="", collapse=" ");
      # 2) save the stats for each node 
      stats[[inx]] <- signif(res[hit.inx, "logFC", drop=F],5);
    }
  }
  V(g)$stats <- stats;
  V(g)$topo <- topo;
  
  if(!.on.public.web){
    mSetObj <- PlotinmexGraph(mSetObj, path.id, g, width, height, bg.cols, line.cols);   
    print("pathinteg graph has been created, please find it in mSet$imgSet$pathinteg.path")
    return(.set.mSet(mSetObj));
  } 
  PlotinmexGraph(mSetObj, path.id, g, width, height, bg.cols, line.cols);   
}

#'Plot an igraph object and return the node information (position and labels)
#'@description Plot an igraph object and return the node information (position and labels)
#'Used in a higher function
#'@param mSetObj Input name of the created mSet Object
#'@param path.id Input the pathway id
#'@param g Input the graph
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param height Input the height of the graph to create
#'@param bg.color Set the background color, default is set to NULL
#'@param line.color Set the line color, default is set to NULL
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotinmexGraph <- function(mSetObj, path.id, g, width, height, bg.color=NULL, line.color=NULL){
  
  # first clean up the graph (only show connected structure, specific compounds, not isolated cmpd)
  # NO, cannot change g object after it is set up, all colors and node info will be shifted
  # g <- delete.vertices(g, which(inmexpa$dc[[path.id]]==0 & V(g)$type=='compound'));
  
  if(is.null(bg.color)){
    bg.color <- V(g)$graphics_bgcolor
  }
  if(is.null(line.color)){
    line.color <- "dimgray";
  }
  
  imgName <- paste(path.id, ".png", sep="");
  mSetObj$imgSet$pathinteg.path <- imgName
  
  ## Open plot device
  Cairo::Cairo(file=imgName, width=width, height=height, type="png", bg="transparent");
  par(mai=rep(0,4));
  plotGraph(g, vertex.label=V(g)$plot_name, vertex.color=bg.color, vertex.frame.color=line.color);
  nodeInfo <- GetKEGGNodeInfo(path.id, g, width, height);
  dev.off();
  
  mSetObj$dataSet$current.kegg <- list(graph=g, bg.color=bg.color, line.color=line.color);
  
  # remember the current graph
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(nodeInfo);
  }else{
    return(.set.mSet(mSetObj));
  } 
}

#'Redraw current graph for zooming or clipping then return a value
#'@description Redraw current graph for zooming or clipping then return a value
#'@usage RerenderKEGGGraph(mSetObj, imgName, width, height, zoom.factor)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@param height Input the heights
#'@param zoom.factor Input the zoom factor 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import igraph
RerenderKEGGGraph <- function(mSetObj=NA, imgName, width, height, zoom.factor){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$imgSet$kegg.graph.zoom <- imgName
  
  Cairo::Cairo(file=imgName, width=width, height=height,type="png", bg="transparent");
  font.cex <- 0.6*zoom.factor/150;
  if(font.cex < 0.6){
    font.cex=0.6;
  }else if(font.cex > 1.2){
    font.cex=1.2;
  }
  plotGraph(mSetObj$dataSet$current.kegg$graph, 
            vertex.color=mSetObj$dataSet$current.kegg$bg.color, 
            vertex.frame.color=mSetObj$dataSet$current.kegg$line.color,
            vertex.label=V(mSetObj$dataSet$current.kegg$graph)$plot_name,
            vertex.label.cex=font.cex
  );
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Retrieves KEGG node information
#'@param path.id Input the path ID
#'@param g Input data
#'@param width Input the width
#'@param height Input the height 
#'@param usr Input the user
#'@export
GetKEGGNodeInfo <- function(path.id, g, width, height, usr = par("usr")){
  
  ## transform user to pixel coordinates
  #x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]); stopifnot(all(rx>=0&rx<=1)); return(rx*width)  }
  #y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]); stopifnot(all(ry>=0&ry<=1)); return(ry*height) }
  
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]);  return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]);  return(ry*height) }
  
  wds <- V(g)$graphics_width;
  wds[wds == 'unknow']<- 46;
  hts <- V(g)$graphics_height;
  hts[hts == 'unknow']<- 17;     
  
  nw <- 1/200*as.numeric(wds);
  nh <-  1/200*as.numeric(hts);
  nxy <- igraph::layout.norm(getLayout(g), -1, 1, -1, 1);
  
  # note: nxy is the center of the node, need to deal differently for cirlce or rectangle
  # for circle the width/height are radius, stay the same, only adjust the rectangle
  rec.inx <- V(g)$graphics_type == "rectangle";
  
  nw[rec.inx] <- nw[rec.inx]/4;
  nh[rec.inx] <- nh[rec.inx]/2;
  
  xl  = floor(100*x.u2p(nxy[,1] - nw)/width);
  xr  = ceiling(100*x.u2p(nxy[,1] + nw)/width);
  yu  = floor(100*y.u2p(nxy[,2] - nh)/height);
  yl  = ceiling(100*y.u2p(nxy[,2] + nh)/height);
  
  tags <- V(g)$graphics_name;
  nm.lnks <- V(g)$db.lnks;
  
  # create the javascript code
  pathName <- names(inmexpa$path.ids)[which(inmexpa$path.ids == path.id)];
  
  jscode <- paste("keggPathLnk=\'(<a href=\"javascript:void(0);\" onclick=\"window.open(\\'http://www.genome.jp/kegg-bin/show_pathway?", path.id, "\\',\\'KEGG\\');\">KEGG</a>)\'", sep="");
  jscode <- paste(jscode, paste("keggPathName=\"", pathName,"\"", sep=""), sep="\n");
  
  #add code for mouseover locations, basically the annotation info. In this case, the name of the node
  if(is.null(V(g)$stats)){
    for(i in 1:length(tags)) {
      jscode <- paste(jscode, paste("rectArray.push({x1:", xl[i], ", y1:", yl[i], ", x2:", 
                                    xr[i], ", y2:", yu[i],
                                    ", lb: \"", tags[i], 
                                    "\", lnk: \"", nm.lnks[i], 
                                    "\"})", sep=""), sep="\n");
    }
  }else{
    stats <- V(g)$stats;
    topos <- signif(V(g)$topo,5);
    for(i in 1:length(tags)) {
      jscode <- paste(jscode, paste("rectArray.push({x1:", xl[i], ", y1:", yl[i], ", x2:", 
                                    xr[i], ", y2:", yu[i],
                                    ", lb: \"", tags[i], 
                                    "\", lnk: \"", nm.lnks[i], 
                                    "\", topo: ", topos[i], 
                                    ifelse(is.null(stats[[i]]), 
                                           "", 
                                           paste(", logFC:", stats[[i]][1], sep="")),
                                    "})", sep=""), sep="\n");
    }
  }
  return(jscode);
}

# Used in higher function
plotGraph <- function(graph,margin=0,vertex.label.cex=0.6,vertex.label.font=1,vertex.size=8,
                    vertex.size2=6,edge.arrow.size=0.2,edge.arrow.width=3,vertex.label=V(graph)$graphics_name,
                    vertex.shape=V(graph)$graphics_type,layout=getLayout(graph),vertex.label.color="black",
                    vertex.color=V(graph)$graphics_bgcolor,vertex.frame.color="dimgray",edge.color="dimgray",
                    edge.label=getEdgeLabel(graph),edge.label.cex=0.6,edge.label.color="dimgray",edge.lty=getEdgeLty(graph),
                    axes=FALSE,xlab="",ylab="",sub=NULL,main=NULL,...){
  if(class(graph)!="igraph") stop("the graph should be a igraph graph.")
  if(vcount(graph)==0){
    print("the graph is an empty graph.")
  }else{	 
    vertex.shape <- replace(vertex.shape,which(vertex.shape %in% c("roundrectangle","line")),"crectangle")
    vertex.color <- replace(vertex.color,which(vertex.color %in% c("unknow","none")),"white")
    if(length(vertex.shape)==0) vertex.shape<-NULL
    if(length(vertex.color)==0) vertex.color<-NULL  
    if(length(vertex.label)==0) vertex.label<-NULL 
    if(length(layout)==0) layout<-NULL 
    if(length(edge.label)==0) edge.label<-NULL
    if((axes==FALSE)&&xlab==""&&ylab==""&&is.null(sub)&&is.null(main)){
      old.mai <- par(mai=c(0.01,0.25,0.01,0.3))
      #old.mai<-par(mai=0.01+c(0,0,0,0))
      on.exit(par(mai=old.mai), add=TRUE)
    }
    plot(graph,margin=margin,vertex.label.cex=vertex.label.cex,vertex.label.font=vertex.label.font,
         vertex.size=vertex.size,vertex.size2=vertex.size2,
         edge.arrow.size=edge.arrow.size,edge.arrow.width=edge.arrow.width,vertex.label=vertex.label,
         vertex.shape=vertex.shape,layout=layout,vertex.label.color=vertex.label.color,
         vertex.color=vertex.color,vertex.frame.color=vertex.frame.color,edge.color=edge.color,
         edge.label=edge.label,edge.label.cex=edge.label.cex,edge.label.color=edge.label.color,
         edge.lty=edge.lty,axes=axes,xlab=xlab,ylab=ylab,sub=sub,main=main,...)
  }
}

getLayout<-function(graph){
  if(length(V(graph)$graphics_x)==0||length(V(graph)$graphics_y)==0) return (NULL)
  x_y<-c()
  graphics_x <- igraph::get.vertex.attribute(graph,"graphics_x")
  index <- which(graphics_x=="unknow")
  
  if(length(index)>1){
    temp<-as.numeric(graphics_x[which(graphics_x!="unknow")])
    if(length(temp)<2){temp<-as.numeric(c(100,600))}
    replace_value<-seq(min(temp),max(temp),by = (max(temp)-min(temp))/(length(index)-1))
    graphics_x<-replace(graphics_x,which(graphics_x=="unknow"),replace_value)
  }else if(length(index)==1){
    temp<-as.numeric(graphics_x[which(graphics_x!="unknow")])
    graphics_x<-replace(graphics_x,which(graphics_x=="unknow"),min(temp))
  } 
  graphics_x <- as.numeric(graphics_x)	
  graphics_y <- igraph::get.vertex.attribute(graph,"graphics_y")
  index <- which(graphics_y=="unknow")
  if(length(index)>0){
    temp <- as.numeric(graphics_y[which(graphics_y!="unknow")])
    if(length(temp)<2){temp<-as.numeric(c(100,600))}
    graphics_y <- replace(graphics_y,which(graphics_y=="unknow"),max(temp)+100)
  } 
  graphics_y <- as.numeric(graphics_y)
  
  x_y <- as.matrix(data.frame(graphics_x=graphics_x, graphics_y=graphics_y))
  x_y[,2] <- -x_y[,2]
  dimnames(x_y) <- NULL
  return(x_y)
}

getEdgeLabel<-function(graph){
  edge.name <- igraph::E(graph)$subtype_name
  edge.value <- igraph::E(graph)$subtype_value
  #edge.label<-E(graph)$subtype_value
  edge.label <- rep("",len=length(edge.name))
  for(i in seq(edge.name)){
    edge_i<-unlist(strsplit(edge.name[i],";"))
    if("phosphorylation" %in% edge_i){
      edge.label[i]<-paste("+p",edge.label[i],sep=" ")
    }
    if("dephosphorylation" %in% edge_i){
      edge.label[i]<-paste("-p",edge.label[i],sep=" ")
    }
    if("glycosylation"  %in% edge_i){
      edge.label[i]<-paste("+g",edge.label[i],sep=" ")
    }
    if("ubiquitination"  %in% edge_i){
      edge.label[i]<-paste("+u",edge.label[i],sep=" ")
    }
    if("methylation"  %in% edge_i){
      edge.label[i]<-paste("+m",edge.label[i],sep=" ")
    }
    if("missing interaction"  %in% edge_i){
      edge.label[i]<-paste("/",edge.label[i],sep=" ")
    }
    if("dissociation"  %in% edge_i){
      edge.label[i]<-paste("|",edge.label[i],sep=" ")
    }
    if("binding/association"  %in% edge_i){
      edge.label[i]<-paste("---",edge.label[i],sep=" ")
    }
    if("repression"  %in% edge_i){
      edge.label[i]<-paste("-e-|",edge.label[i],sep=" ")
    }
    if("expression"  %in% edge_i){
      edge.label[i]<-paste("-e->",edge.label[i],sep=" ")
    }
    if("inhibition"  %in% edge_i){
      edge.label[i]<-paste("--|",edge.label[i],sep=" ")
    }
    if("activation"  %in% edge_i){
      edge.label[i]<-paste("-->",edge.label[i],sep=" ")
    }
    if("indirect effect"  %in% edge_i){
      edge.label[i]<-paste("..>",edge.label[i],sep=" ")
    }
    if("state change"  %in% edge_i){
      edge.label[i]<-paste("...",edge.label[i],sep=" ")
    }
    if("compound" %in% edge_i){
      compound<-V(graph)[V(graph)$id==edge.value[i]]$graphics_name
      if(length(compound)==1){
        edge.label[i]<-paste(compound,edge.label[i],sep=" ")
      }    
    }           
  }
  return(edge.label)
}

#04350 indirect effect,04620
getEdgeLty<-function(graph){
  edge.name <- igraph::E(graph)$subtype_name
  edge.lty=rep("solid",len=length(edge.name))
  for(i in seq(edge.name)){
    if(edge.name[i]=="indirect effect"){
      edge.lty[i]<-"longdash"
    }else if(edge.name[i]=="state change"){
      edge.lty[i]<-"longdash"
    }
  }
  #new!
  if(length(edge.lty)==0){
    edge.lty="solid"
  }
  return(edge.lty)
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetKEGG.PathNames2<-function(){
  return(names(inmexpa$path.ids));
}

GetKEGGPathID <- function(pathName){
  return(inmexpa$path.ids[[pathName]]);
}

# return gene and compounds highlighted in the pathway
GetIntegHTMLPathSet<-function(mSetObj=NA, pathName){
  mSetObj <- .get.mSet(mSetObj);
  path.id <- inmexpa$path.ids[[pathName]];
  phits <- mSetObj$dataSet$path.hits[[path.id]];
  
  # now, get hit names
  
}