
GetSigGenes <-function(dataName="", res.nm="nm", p.lvl=0.05, fc.lvl=1, inx=1){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  analSet <- readSet(analSet, "analSet");
  dataSet <- readDataset(dataName);
  
  total <- nrow(dataSet$comp.res);
  resTable <- dataSet$comp.res;
  filename <- dataSet$filename;
  filename <- paste(filename, "_", res.nm, ".csv", sep="");
  current.msg <- "";
  
  if (is.null(resTable) || nrow(resTable) == 0){
    msgSet$current.msg <- paste(msgSet$current.msg, "No significant genes were identified using the given design and cutoff."); 
  }
  # now rank by logFC, note, the logFC for each comparisons 
  # are returned in resTable before the AveExpr columns 
  # for two-class, only one column, multiple columns can be involved
  # for > comparisons - in this case, use the largest logFC among all comparisons
  #if (fc.lvl > 0){ # further filter by logFC
  if (dataSet$de.method=="deseq2"){
    dds <- qs::qread("deseq.res.obj.rds");
    vec <- as.numeric(c(dataSet$contrast.matrix[,inx]));
    inx <- 1;
    res <- results(dds, contrast = vec, independentFiltering = FALSE, cooksCutoff = Inf);
    topFeatures <- data.frame(res@listData);
    rownames(topFeatures) <- rownames(res);
    nms <- colnames(topFeatures);
    nms[which(nms == "padj")] <- "adj.P.Val";
    nms[which(nms == "pvalue")] <- "P.Value";
    nms[which(nms == "log2FoldChange")] <- "logFC";
    colnames(topFeatures) <- nms;
    topFeatures <- topFeatures[c(2,1,3,4,5,6)];
    # order the result based on raw p
    ord.inx <- order(topFeatures$P.Value);
    resTable <- topFeatures[ord.inx, ];
    hit.inx <- which(colnames(resTable) == "baseMean"); 
    dataSet$comp.res <- resTable;
  } else if (dataSet$de.method=="limma"){
    hit.inx <- which(colnames(resTable) == "AveExpr");
  } else {
    hit.inx <- which(colnames(resTable) == "logCPM");
  }
  
  resTable <- resTable[!is.na(resTable[,1]),]
  orig.resTable <- resTable;
  # select based on p-value
  if(dataSet$type == "array"){
    hit.inx.p <- resTable$adj.P.Val <= p.lvl; 
  } else {
    hit.inx.p <- resTable$adj.P.Val <= p.lvl; 
  }
  
  resTable<-resTable[hit.inx.p,];
  
  maxFC.inx <- hit.inx - 1; # not sure if this is also true for edgeR
  logfc.mat <- resTable[,1:maxFC.inx, drop=F];
  pos.mat <- abs(logfc.mat[,inx]);
  #fc.vec <- apply(pos.mat, 1, max);   # for > comparisons - in this case, use the largest logFC among all comparisons
  fc.vec <- pos.mat;
  hit.inx.fc <- fc.vec >= fc.lvl;
  resTable <- resTable[hit.inx.fc,];
  
  if (nrow(resTable) == 0){
    msgSet$current.msg <- paste(msgSet$current.msg, "No significant genes were identified using the given design and cutoff.");
  }
  
  ### Note, rowname of resTable must be entrez ID
  
  de.Num <- nrow(resTable);
  
  # display at most 5000 genes for the server (two main reasons)
  # 1) should not have more 22% (human: 23000) DE of all genes (biological)
  # 2) IE canvas can display no more than 6800 pixels (computational)
  if (nrow(resTable) > 5000){
    resTable <- resTable[1:5000,];
    msgSet$current.msg <- paste(msgSet$current.msg, " Due to computational constraints, only top 5000 genes will be used. ", collapse="\n");
  }
  
  # may need to update data, class and meta.info
  data <- dataSet$data.norm;
  cls <- dataSet$cls; 
  meta.info <- dataSet$meta;
  grp.nms <- levels(cls);
  
  hit.inx <- cls %in% grp.nms;
  if (sum(hit.inx) < length(hit.inx)){
    msgSet$current.msg <- paste(msgSet$current.msg, "Only groups selected for comparisons: ", paste(grp.nms, collapse=", "), "are included.");
    cls <- factor(cls[hit.inx]);
    cls.lvls <- levels(cls);
    data <- data[,hit.inx];
    meta.info <- dataSet$meta[hit.inx,];
  }
  qs::qsave(data, file="data.stat.qs");
  dataSet$comp.res <- dataSet$comp.res[order(dataSet$comp.res$adj.P.Val),] 
  dataSet$comp.res <- dataSet$comp.res[which(!rownames(dataSet$comp.res) %in% rownames(resTable)),]
  dataSet$comp.res <- rbind(resTable, dataSet$comp.res);
  
  
  dataSet$sig.mat <- resTable;
  
  if (dataSet$annotated){ # annotated to entrez
    anot.id <- rownames(dataSet$comp.res);
    gene.anot <- doEntrezIDAnot(anot.id, paramSet$data.org, paramSet$data.idType);
    fast.write(cbind(EntrezID=anot.id, signif (dataSet$comp.res,5), Symbols = gene.anot$symbol,  Name=gene.anot$name), row.names=F, file=filename);
  } else if (file.exists("annotation.qs")){ # annotation information available
    anot.id <- qs::qread("annotation.qs");
    feature.vec <- rownames(dataSet$comp.res);
    entrez.vec <- anot.id[feature.vec];
    gene.anot <- doEntrezIDAnot(entrez.vec, paramSet$data.org, paramSet$data.idType);
    fast.write(cbind(signif (dataSet$comp.res,5), EntrezID=entrez.vec, Symbols = gene.anot$symbol,  Name=gene.anot$name), row.names=F, file=filename);
    rownames(gene.anot) <- feature.vec;
  } else {
    gene.anot <- NULL;
    fast.write(signif(resTable,5), file=filename);
  }
  if(is.null(gene.anot)){
    analSet$sig.genes.symbols <- rownames(resTable); # use the id provided
  }else{
    analSet$sig.genes.symbols <- gene.anot$symbol;
  }
  dataSet$cls.stat <- cls;
  dataSet$meta.stat <- meta.info;
  
  # now do protein mapping for network only applicable for annotated
  
  gene <- rownames(resTable);
  
  logFC <- unname(logfc.mat[,1]);
  geneList <- paste(gene, logFC, collapse="\n");
  up <- nrow(resTable[which(logfc.mat[,paramSet$selectedFactorInx]> fc.lvl),])
  down <- nrow(resTable[which(logfc.mat[,paramSet$selectedFactorInx]< -fc.lvl),])
  saveSet(msgSet, "msgSet");
  
  data.norm <- dataSet$data.norm
  colnames(data.norm) <- NULL
  lst <- list(colnames(dataSet$data.norm),data.norm, dataSet$meta, dataSet$comp.res, rownames(data.norm), org=paramSet$data.org)
  json.obj <- rjson::toJSON(lst);
  sink("ExpressAnalyst_matrix.json");
  cat(json.obj);
  sink();
  
  analSet$sig.gene.count <- de.Num;
  saveSet(analSet, "analSet");
  res <- RegisterData(dataSet);
  if(res == 1){
    return(c(filename, de.Num, geneList, total, up, down));
  }
}