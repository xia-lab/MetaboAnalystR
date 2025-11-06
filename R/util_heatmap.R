# Function to generate interactive heatmaps to display peak intensity table from
# mummichog/PSEA analysis

psea.heatmap.json <- function(mSetObj=NA, libOpt, libVersion, minLib, fileNm, filtOpt, 
                              version="v1"){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  data <- t(dataSet$norm)
  sig.ids <- rownames(data);
  
  res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls);
  
  if(dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx = rep(TRUE, nrow(data))
  }else if(dataSet$mode == "negative"){
    mSetObj$dataSet$pos_inx = rep(FALSE, nrow(data))
  }
  
  is.rt <- mSetObj$paramSet$mumRT;
  
  if(mSetObj$paramSet$mumRT){
    feat_info <- rownames(data)
    # OPTIMIZED: Use strsplit once with fixed=TRUE for better performance
    feat_info_split <- matrix(unlist(strsplit(feat_info, "__", fixed=TRUE)), ncol=2, byrow=T)
    colnames(feat_info_split) <- c("m.z", "r.t")

    # OPTIMIZED: Single-pass duplicate handling - vectorized operation
    duplicates <- duplicated(feat_info_split[,1])
    if(any(duplicates)){
      # ensure features are unique
      set.seed(123)
      n_dup <- sum(duplicates)
      # Vectorized: generate all random numbers at once, paste in one operation
      feat_info_split[duplicates, 1] <- paste0(feat_info_split[duplicates, 1],
                                                sample(1:999, n_dup, replace = FALSE))
    }

    if(mSetObj$paramSet$mumRT.type == "minutes"){
      # OPTIMIZED: In-place conversion, no intermediate rtime variable
      feat_info_split[,2] <- as.numeric(feat_info_split[,2]) * 60
    }

    # OPTIMIZED: Vectorized paste operation
    new_feats <- paste(feat_info_split[,1], feat_info_split[,2], sep = "__")
    rownames(data) <- make.unique(new_feats)
    
    rownames(res) <- l <- mSetObj$dataSet$ref_mzlist <- make.unique(feat_info_split[,1]);
    retention_time <- as.numeric(feat_info_split[,2]);
    names(retention_time) <- mSetObj$dataSet$ref_mzlist;
    mSetObj$dataSet$ret_time <- retention_time;
    
    if(is.na(mSetObj$dataSet$rt_tol)){
      rt_tol <- max(mSetObj$dataSet$ret_time) * mSetObj$dataSet$rt_frac 
      print(paste0("Retention time tolerance is ", rt_tol))
      mSetObj$dataSet$rt_tol <- rt_tol
    }
    mSetObj$dataSet$expr_dic= res[,1];
    names(mSetObj$dataSet$expr_dic) = rownames(res)
  }else{
    l <- sapply(rownames(data), function(x) return(unname(strsplit(x,"/")[[1]][1])))
    mSetObj$dataSet$ref_mzlist <- rownames(res) <- l <- as.numeric(unname(unlist(l)))
    mSetObj$dataSet$expr_dic= res[,1];
    names(mSetObj$dataSet$expr_dic) = rownames(data)
  }
  mum.version <- mSetObj$paramSet$version <- version
  
  #if(filtOpt == "filtered"){
  #  mSetObj <- .setup.psea.library(mSetObj, libOpt, libVersion, minLib);
  #  matched_res <- qs::qread("mum_res.qs");
  #  res_table <- matched_res;
  #  data = data[which(l %in% res_table[,"Query.Mass"]),]
  #  res = res[which(rownames(res) %in% res_table[,"Query.Mass"]),]
  #}
  
  stat.pvals <- unname(as.vector(res[,2]));
  t.stat <- unname(as.vector(res[,1]));
  org <- unname(strsplit(libOpt,"_")[[1]][1])
  
  # scale each gene, note direct overwrite data to save memory
  data <- t(scale(t(data)));
  
  rankPval = order(as.vector(stat.pvals));
  stat.pvals = stat.pvals[rankPval];
  data = data[rankPval,];
  t.stat = t.stat[rankPval];
  
  # now pearson and euclidean will be the same after scaleing
  orig.smpl.nms <- colnames(data);
  orig.gene.nms <- rownames(data);

  # OPTIMIZED: Compute distance matrices once and reuse for all clustering methods
  # do clustering and save cluster info
  # convert order to rank (score that can used to sort)
  if(nrow(data)> 1){
    # Gene/feature clustering - compute distance once
    gene.dist <- dist(data);

    # Extract ordering from each linkage method, discard full tree to save memory
    gene.ward.hc <- hclust(gene.dist, "ward.D");
    gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.hc$order]);
    rm(gene.ward.hc);  # Free memory immediately

    gene.ave.hc <- hclust(gene.dist, "ave");
    gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.hc$order]);
    rm(gene.ave.hc);

    gene.single.hc <- hclust(gene.dist, "single");
    gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.hc$order]);
    rm(gene.single.hc);

    gene.complete.hc <- hclust(gene.dist, "complete");
    gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.hc$order]);
    rm(gene.complete.hc);

    # Free gene distance matrix before computing sample distance
    rm(gene.dist);

    # Sample clustering - compute distance once
    smpl.dist <- dist(t(data));

    smpl.ward.hc <- hclust(smpl.dist, "ward.D");
    smpl.ward.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ward.hc$order]);
    rm(smpl.ward.hc);

    smpl.ave.hc <- hclust(smpl.dist, "ave");
    smpl.ave.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ave.hc$order]);
    rm(smpl.ave.hc);

    smpl.single.hc <- hclust(smpl.dist, "single");
    smpl.single.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.single.hc$order]);
    rm(smpl.single.hc);

    smpl.complete.hc <- hclust(smpl.dist, "complete");
    smpl.complete.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.complete.hc$order]);
    rm(smpl.complete.hc);

    # Free sample distance matrix
    rm(smpl.dist);
  }else{
    # force not to be single element vector which will be scaler
    #stat.pvals <- matrix(stat.pvals);
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(data);
  }
  
  gene.cluster <- list(
    ward = gene.ward.rk,
    average = gene.ave.rk,
    single = gene.single.rk,
    complete = gene.complete.rk,
    pval = stat.pvals,
    stat = t.stat
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
  meta <- data.frame(dataSet$cls);
  grps <- "Condition"
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
  res <- apply(data, 1, function(x){as.numeric(cut(x, breaks=30))}); #previously t() when using RJSONIO
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  gene.id = orig.gene.nms; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
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
    data = res,
    org = org
  );

  mSetObj$dataSet$hm_peak_names = gene.id
  mSetObj$dataSet$gene.cluster = gene.cluster
  
  .set.mSet(mSetObj)
  json.mat <- rjson::toJSON(json.res);
  sink(fileNm);
  cat(json.mat);
  sink();
  return(1);
}
