##################################################
## R script for ExpressAnalyst
## Description: GSEA functions related to interactive plot
## Authors: 
## G. Zhou, guangyan.zhou@mail.mcgill.ca
## J. Xia, jeff.xia@mcgill.ca
###################################################
PlotGShm <-function(dataName="", cmpdNm="", IDs){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  
  anal.type <- paramSet$anal.type;
  data.org <- paramSet$data.org;
  
  ids <- unlist(strsplit(IDs, "; "));
  cmpdNm <- gsub(" ", "_",  cmpdNm);
  cmpdNm <- gsub("/", "_",  cmpdNm);
  
  if(anal.type == "onedata"){
    dataSet <- readDataset(dataName);
    gene.map <- readDataQs("symbol.map.qs", paramSet$anal.type, dataName);
    subset <- dataSet$data.norm[which(doIdMappingGeneric(rownames(dataSet$data.norm), gene.map, "gene_id", "symbol") %in% ids),]
    if(length(subset)<1){
      subset <- dataSet$data.norm[which(rownames(dataSet$data.norm) %in% ids),]
    }
    
    inx <- order(dataSet$meta.info[,1]);
    subset <- subset[,inx];
    
    
  }else{
    if(paramSet$selDataNm == "meta_default"){
      inmex <- qs:::qread("inmex_meta.qs");
      dat <- inmex$plot.data
      gene.map <- data.frame(gene_id=names(inmex$gene.symbls), symbol=unname(inmex$gene.symbls));
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      gene.map <- readDataQs("symbol.map.qs", paramSet$anal.type, paramSet$selDataNm);
      dat <- dataSet$data.norm;
    }
    subset <- dat[which(doIdMappingGeneric(rownames(dat), gene.map, "gene_id", "symbol") %in% ids),]
    if(length(subset)<1){
      subset <- dat[which(rownames(dat) %in% ids),]
    }
    inx <- order(inmex$cls.lbl);
    subset <- subset[,inx];
  }
  dat <- t(scale(t(subset)));
  
  # now pearson and euclidean will be the same after scaling
  dat.dist <- dist(dat); 
  orig.gene.nms <- doIdMappingGeneric(rownames(subset), gene.map, "gene_id", "symbol");
  gene.ward.ord <- hclust(dat.dist, "ward.D")$order;
  gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.ord]);
  gene.ave.ord <- hclust(dat.dist, "ave")$order;
  gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.ord]);
  gene.single.ord <- hclust(dat.dist, "single")$order;
  gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.ord]);
  gene.complete.ord <- hclust(dat.dist, "complete")$order;
  gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.ord]);
  
  gene.cluster <- list(
    ward = orig.gene.nms[gene.ward.ord],
    average = orig.gene.nms[gene.ave.ord],
    single = orig.gene.nms[gene.single.ord],
    complete = orig.gene.nms[gene.complete.ord]
  );
  
  if(anal.type == "onedata"){
    res.tbl <- dataSet$comp.res ;
    res.tbl <- res.tbl[which(rownames(res.tbl) %in% rownames(subset)),];
    res.tbl$id <- doIdMappingGeneric(rownames(res.tbl), gene.map, "gene_id", "symbol");
    if("P.Value" %in% colnames(res.tbl)){
      res.tbl <- res.tbl[order(res.tbl$P.Value),];
    }else{
      res.tbl <- res.tbl[order(res.tbl$PValue),];
    }
    stat.pvals <- res.tbl$id;
    
    if("logFC" %in% colnames(res.tbl)){
      stat.fc <- res.tbl[order(-abs(res.tbl$logFC)),]; 
    }else{
      stat.fc <- res.tbl[order(-abs(res.tbl[,paramSet$selectedFactorInx])),]; 
    }
    stat.fc <- res.tbl$id;
    
    gene.cluster[["pval"]] <- stat.pvals;
    gene.cluster[["fc"]] <- stat.fc;
  }else{
    res.tbl <- analSet$meta.mat.all;
    res.tbl <- res.tbl[which(rownames(res.tbl) %in% rownames(subset)),];
    rownames(res.tbl) <- doIdMappingGeneric(rownames(res.tbl), gene.map, "gene_id", "symbol");  
    res.tbl <- res.tbl[order(res.tbl[,2]),];
    stat.pvals <- rownames(res.tbl);
    
    gene.cluster[["pval"]] <- stat.pvals;
  }
  
  json.res <- list(
    data=subset,
    ids=doIdMappingGeneric(rownames(subset), gene.map, "gene_id", "symbol"),
    entrez = rownames(subset),
    gene.cluster = gene.cluster
  )
  
  json.mat <- RJSONIO::toJSON(json.res);
  json.nm <- paste(cmpdNm,"_hm", ".json", sep="");
  
  sink(json.nm);
  cat(json.mat);
  sink();
  
  paramSet$jsonNms$heatmapGSEA <- json.nm
  saveSet(paramSet, "paramSet");

  return(json.nm)
}

PlotGSView <-function(cmpdNm, format="png", dpi=72, width=NA){
  require("ggplot2");
  require("fgsea");
  current.geneset <- qs::qread("current_geneset.qs");
  analSet <- readSet(analSet, "analSet");
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- gsub(" ", "_",  imgName);
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
  
  Cairo(file = imgName, dpi=72, width=340, height=300, type="png", bg="transparent");
  g <- plotEnrichment(current.geneset[[cmpdNm]], analSet$rankedVec)
  print(g)
  dev.off();
  return(imgName);
}
