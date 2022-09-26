##################################################
## R script for ExpressAnalyst
## Description: functions for quality check boxplot
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


PlotDataPCA <- function(fileName, pcaName, dpi, format,factor){
  dataSet <- readDataset(fileName);
  res <- qc.pcaplot(dataSet, dataSet$data.norm, pcaName, dpi, format, factor);
  return(res);
}

qc.pcaplot <- function(dataSet, x, imgNm, dpi=72, format="png", factor){
  dpi <- as.numeric(dpi);
  fileNm <- paste(imgNm, "dpi", dpi, ".", sep="");
  imgNm <- paste0(fileNm, format, sep="");
  require('lattice');
  require('ggplot2');
  require('reshape');
  pca <- prcomp(t(na.omit(x)));
  imp.pca<-summary(pca)$importance;
  xlabel <- paste0("PC1"," (", 100*round(imp.pca[2,][1], 3), "%)")
  ylabel <- paste0("PC2"," (", 100*round(imp.pca[2,][2], 3), "%)")
  names <- colnames(x);
  pca.res <- as.data.frame(pca$x);
  pca.res <- pca.res[,c(1,2)]
  # increase xlim ylim for text label
  xlim <- GetExtendRange(pca.res$PC1);
  ylim <- GetExtendRange(pca.res$PC2);

  if("newcolumn" %in% colnames(dataSet$meta)){
    dataSet$meta <- data.frame(dataSet$meta[,-which(colnames(dataSet$meta) == "newcolumn")]);
  }

  if(length(dataSet$meta) == 2){
    Factor1 <- as.vector(dataSet$meta[,1])
    factorNm1 <- colnames(dataSet$meta)[1]
    pca.res[,factorNm1] <- Factor1
    Factor2 <- as.vector(dataSet$meta[,2])
    factorNm2 <- colnames(dataSet$meta)[2]
    pca.res[,factorNm2] <- Factor2
    pca.rest <- reshape::melt(pca.res, measure.vars=c(factorNm1,factorNm2))
    colnames(pca.rest)[4] <- "Conditions"
    pca.rest$names <- c(rownames(pca.res), rownames(pca.res))
    if(length(pca.rest$names)>20){

      pcafig <- ggplot(pca.rest, aes(x=PC1, y=PC2,  color=Conditions, label=pca.rest$names)) +
        geom_point(size=3, alpha=0.5) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) + 
        facet_grid(. ~ variable) +
        theme_bw()

    }else{

      require('ggrepel');
      pcafig <- ggplot(pca.rest, aes(x=PC1, y=PC2,  color=Conditions, label=pca.rest$names)) +
        geom_point(size=4) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) + 
        geom_text_repel(force=1.5) + 
        facet_grid(. ~ variable) + theme_bw()
    }
    width <- 12
    height <- 6
  }else{
    Factor <- dataSet$meta[,1];
    pca.rest <- pca.res
    pca.rest$Conditions <- Factor
    pca.rest$names <- rownames(pca.res)
    if(length(rownames(pca.res))>20){

      pcafig <- ggplot(pca.rest, aes(x=PC1, y=PC2,  color=Conditions)) +
        geom_point(size=3, alpha=0.5) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) +
        theme_bw()

    }else{

      require('ggrepel');
      pcafig <- ggplot(pca.rest, aes(x=PC1, y=PC2,  color=Conditions, label=rownames(pca.res))) +
        geom_point(size=4) + 
        xlim(xlim) + 
        ylim(ylim) + 
        xlab(xlabel) + 
        ylab(ylabel) +
        geom_text_repel(force=1.5)+
        scale_color_manual(breaks=unique(pca.rest$Conditions), values=c("#00BFC4" ,"#F8766D", "#006600", "#669999","#CC0000", "#00CCCC", "#660099", "#CC0066", "#FF9999", "#FF9900")) +
        theme_bw()
    }
    width <- 10
    height <- 6
  }
  if(format == "svg"){
    require(gridSVG)
    png("NULL", width=width, height=height, unit="in", res=dpi); 
    print(pcafig)
    
    rootAttrs = c("svgpca")
    names(rootAttrs) = c("id");
    pcafig.svg <- gridSVG::grid.export(imgNm,addClasses=TRUE, uniqueNames=TRUE, annotate=T, rootAttrs=rootAttrs)
    str <- paste(capture.output(pcafig.svg$svg, file=NULL), collapse="\n");   
    dev.off()  
    jsonNm <- paste0(fileNm, "json");
    json.obj <- rjson::toJSON(pca.rest);
    sink(jsonNm);
    cat(json.obj);
    sink();
  }else{
    Cairo(file=imgNm, width=width, height=height, type=format, bg="white", unit="in", dpi=dpi);
    print(pcafig);
    dev.off();
    str <- "NA"
  }
  return(str)
}