##################################################
## R script for ExpressAnalyst
## Description: Compute ORA heatmap
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

# prepare data for heatmap plotting include 
# 1. meta info (top bars)
# 2. expression matrix
# 3. function annotation (left bars)
# all matrix will be combined, 
# 1 and 2 separated by a row of 'null' 
# 3 and 1+2 separated by a column of 'null'
PrepareExpressHeatmapJSON <- function(dataSet){
  data.stat <- qs::qread("data.stat.qs");
  paramSet <- readSet(paramSet, "paramSet");
  res.tbl <- dataSet$comp.res; #dataSet$sig.mat for sig only
  if(nrow(res.tbl) > 5000){
    res.tbl <- res.tbl[c(1:5000),];
  }
  res.tbl <- res.tbl[which(rownames(res.tbl) %in% rownames(data.stat)),]
  sig.ids <- rownames(dataSet$sig.mat);
  if("P.Value" %in% colnames(res.tbl)){
    stat.pvals <- res.tbl$P.Value; 
  }else{
    stat.pvals <- res.tbl$PValue; 
  }
  gene.map <- readDataQs("symbol.map.qs", paramSet$anal.type, dataSet$name);

  all.ids <- rownames(res.tbl);
  
  if("logFC" %in% colnames(res.tbl)){
    stat.fc <- res.tbl$logFC; 
  }else{
    stat.fc <- res.tbl[,paramSet$selectedFactorInx]; 
  }
  
  # scale each gene 
  all.ids <- all.ids[all.ids %in% rownames(data.stat)];
  hit.inz <- sig.ids %in% rownames(data.stat);
  sig.ids <- sig.ids[hit.inz];
  
  
  #sig only
  #dat <- t(scale(t(data.stat[sig.ids, , drop=F])));
  
  #all genes
  dat <- t(scale(t(data.stat[all.ids, , drop=F])));
  
  # now pearson and euclidean will be the same after scaling
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  # do clustering and save cluster info
  # convert order to rank (score that can used to sort) 
  if(nrow(dat)> 1){
    dat.dist <- dist(dat);
    gene.ward.ord <- hclust(dat.dist, "ward.D")$order;
    gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.ord]);
    gene.ave.ord <- hclust(dat.dist, "ave")$order;
    gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.ord]);
    gene.single.ord <- hclust(dat.dist, "single")$order;
    gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.ord]);
    gene.complete.ord <- hclust(dat.dist, "complete")$order;
    gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.ord]);
    
    dat.dist <- dist(t(dat));
    smpl.ward.ord <- hclust(dat.dist, "ward.D")$order;
    smpl.ward.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ward.ord])
    smpl.ave.ord <- hclust(dat.dist, "ave")$order;
    smpl.ave.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ave.ord])
    smpl.single.ord <- hclust(dat.dist, "single")$order;
    smpl.single.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.single.ord])
    smpl.complete.ord <- hclust(dat.dist, "complete")$order;
    smpl.complete.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.complete.ord])
  }else{
    # force not to be single element vector which will be scaler
    stat.pvals <- matrix(stat.pvals);
    stat.fc <- matrix(stat.fc);
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(dat);
  }
  
  gene.cluster <- list(
    pval = stat.pvals, 
    fc = stat.fc,
    ward = gene.ward.rk,
    average = gene.ave.rk,
    single = gene.single.rk,
    complete = gene.complete.rk
  );
  
  sample.cluster <- list(
    ward = smpl.ward.rk,
    average = smpl.ave.rk,
    single = smpl.single.rk,
    complete = smpl.complete.rk
  );
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  meta <- data.frame(dataSet$meta.stat);
  grps <- colnames(meta);
  nmeta <- meta.vec <- NULL;
  uniq.num <- 0;
  for (i in 1:ncol(meta)){
    cls <- meta[,i];
    grp.nm <- grps[i];
    meta.vec <- c(meta.vec, as.character(cls))
    # make sure each label are unqiue across multiple meta data
    ncls <- paste(grp.nm, as.numeric(cls)); # note, here to retain ordered factor
    nmeta <- c(nmeta, ncls);
  }
  
  # convert back to numeric 
  nmeta <- as.numeric(as.factor(nmeta))+99;
  unik.inx <- !duplicated(nmeta)   
  
  # get corresponding names
  meta_anot <- meta.vec[unik.inx]; 
  names(meta_anot) <- nmeta[unik.inx]; # name annotatation by their numbers
  
  nmeta <- matrix(nmeta, ncol=ncol(meta), byrow=F);
  colnames(nmeta) <- grps;
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks 
  res <- t(apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))}));
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  sig.inx <- which(rownames(res) %in% sig.ids) -1
  
  if(dataSet$annotated){
    anot.id <- rownames(res);
    anot.res <- doIdMappingGeneric(anot.id, gene.map, "gene_id", "symbol", "matrix")
    # single element vector will be converted to scalar, not array, need to prevent that
    gene.id <- anot.res$symbol; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
    gene.entrez <- anot.res$gene_id; if(length(gene.entrez) ==1) { gene.entrez <- matrix(gene.entrez) };        
    gene.name <- anot.res$name; if(length(gene.name) ==1) { gene.name <- matrix(gene.name) };
    json.res <- list(
      data.type = dataSet$type, 
      gene.id = anot.res$symbol,
      gene.entrez = gene.entrez,
      gene.name = anot.res$name,
      gene.cluster = gene.cluster,
      sample.cluster = sample.cluster,
      sample.names = orig.smpl.nms,
      meta = data.frame(nmeta),
      meta.anot = meta_anot,
      data = res
    );
  }else if(file.exists("annotation.qs")){
    # special gene.id and new gene.symbol
    anot.id <- rownames(res);
    anot.res <- doEntrezIDAnot(anot.id, paramSet$data.org, paramSet$data.idType);
    gene.id <- rownames(anot.res); if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
    gene.entrez <- anot.res$gene_id; if(length(gene.entrez) ==1) { gene.entrez <- matrix(gene.entrez) };  
    gene.name <- paste(anot.res$symbol, anot.res$name, sep=" | "); if(length(gene.name) ==1) { gene.name <- matrix(gene.name) };
    
    json.res <- list(
      data.type = dataSet$type, 
      gene.id = gene.id,
      gene.entrez = gene.entrez,
      gene.name = gene.name,
      gene.cluster = gene.cluster,
      sample.cluster = sample.cluster,
      sample.names = orig.smpl.nms,
      meta = data.frame(nmeta),
      meta.anot = meta_anot,
      data = res
    );
  }else{          
    gene.id <- orig.gene.nms; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
    json.res <- list(
      data.type = dataSet$type,
      gene.id = gene.id,
      gene.entrez = gene.id,
      gene.name = gene.id,
      gene.cluster = gene.cluster,
      sample.cluster = sample.cluster,
      sample.names = orig.smpl.nms,
      meta = data.frame(nmeta),
      meta.anot = meta_anot,
      data = res
    );
  }
  json.res[["sigInx"]] <- sig.inx;
  return(json.res);
}
