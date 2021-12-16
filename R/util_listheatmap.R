
my.list.heatmap <- function(mSetObj=NA, libOpt, libVersion, minLib, fileNm, filtOpt, version="v1"){
  
  mSetObj <- .get.mSet();
  sig.ids <- as.character(mSetObj$dataSet$mummi.proc[,"m.z"]);
  gene.symbols <- as.character(mSetObj$dataSet$mummi.proc[,"m.z"]);
  stat.pvals <- mSetObj$dataSet$mummi.proc[,"p.value"];
  if("t.score" %in% colnames(mSetObj$dataSet$mummi.proc)){
    t.stat <- mSetObj$dataSet$mummi.proc[,"t.score"];
  }else{
    t.stat <- 0;
  }
  data <- mSetObj$dataSet$mummi.proc
  res <- data
  rownames(data) <- sig.ids
  
  data <- data[,-which(colnames(data) %in% c("m.z","pos_inx","r.t"))]
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  
  grps <- "datalist1"
  cls <- "datalist1"
  
  mSetObj$dataSet$mumRT <- is.rt
  mSetObj$dataSet$mumRT.type <- mumRT.type
  
  if(mSetObj$dataSet$mumRT){
    feat_info <- rownames(data)
    feat_info_split <- matrix(unlist(strsplit(feat_info, "__", fixed=TRUE)), ncol=2, byrow=T)
    colnames(feat_info_split) <- c("m.z", "r.t")
    
    if(mSetObj$dataSet$mumRT.type == "minutes"){
      rtime <- as.numeric(feat_info_split[,2])
      rtime <- rtime * 60
      feat_info_split[,2] <- rtime
    }
    
    new_feats <- paste(feat_info_split[,1], feat_info_split[,2], sep = "__")
    rownames(data) <- new_feats
    
    rownames(res) <- l <- mSetObj$dataSet$ref_mzlist <- as.numeric(feat_info_split[,1]);
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
  
  if(filtOpt == "filtered"){
    mSetObj <- .setup.psea.library(mSetObj, libOpt, libVersion, minLib);
    matched_res <- qs::qread("mum_res.qs");
    res_table <- matched_res;
    data = data[which(l %in% res_table[,"Query.Mass"]),]
    res = res[which(rownames(res) %in% res_table[,"Query.Mass"]),]
  }
  
  expval <- 0
  expval <- sum(data)
  
  # scale each gene 
  dat <- data
  
  # now pearson and euclidean will be the same after scaleing
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  
  mum.version <<- version
  # convert back to numeric 
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks
  resl <- list();
  if(expval !=0){
    datall <- dat
    for(i in 1:ncol(dat)){
      dat_pos <- as.matrix(dat[sign(dat[,i]) == 1,i])
      dat_neg <- as.matrix(dat[sign(dat[,i]) == -1,i])
      inx_pos <- sign(dat[,i]) == 1
      inx_neg <- sign(dat[,i]) == -1
      datsub <- as.matrix(dat[,i])
      if(colnames(dat)[i] == "p.value"){
        datsub= 1 - datsub
      }
      if(nrow(dat_pos) == 0){
        datall[,i]  <- apply(unname(datsub), 2, function(x){
          y =log(abs(x)) + 0.000001
          16-as.numeric(cut(y, breaks=15))
        });
      }else if(nrow(dat_neg) == 0){
        datall[,i] <- apply(unname(datsub), 2, function(x){
          y =log(x) + 0.000001
          15+as.numeric(cut(y, breaks=15))
        });
      }else{
        res_pos <- apply(unname(dat_pos), 2, function(x){
          y =log(x) + 0.000001
          as.numeric(cut(y, breaks=15))+15
        });
        res_neg <- apply(unname(dat_neg), 2, function(x){
          y =log(abs(x)) + 0.000001
          16 - as.numeric(cut(y, breaks=15))
        });
        datall[inx_pos, i] = as.vector(res_pos)
        datall[inx_neg, i] = as.vector(res_neg)
      }
      
    }
    res = datall;
  }else{
    
    zero.inx <- dat == 0
    res <- dat;
    res[zero.inx] <- 32
  }
  
  res_list <- list()
  for(i in 1:nrow(res)){
    res_list[[i]] <- unname(res[i,])
  }
  
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
    rankPval = order(as.vector(stat.pvals))
    stat.pvals = unname(stat.pvals[rankPval])
    t.stat = unname(t.stat[rankPval])
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(dat);
  }
  
  gene.cluster <- list(
    ward = gene.ward.rk,
    average = gene.ave.rk,
    single = gene.single.rk,
    complete = gene.complete.rk,
    pval = stat.pvals
  );
  
  if(t.stat != 0){
    gene.cluster[["stat"]] = t.stat;
  }
  
  sample.cluster <- list(
    ward = smpl.ward.rk,
    average = smpl.ave.rk,
    single = smpl.single.rk,
    complete = smpl.complete.rk
  );
  
  
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  
  nmeta <- as.numeric(as.factor(colnames(data))) + 99
  nmeta.anot <- list()
  
  for(i in 1:length(unique(nmeta))){
    nmeta.anot[[colnames(data)[i]]] <- nmeta[i]
  }
  nmeta <- list(nmeta)
  names(nmeta) <- "Statistics"
  
  
  json.res <- list(
    data.type = "singlelist", 
    gene.id = gene.symbols,
    gene.entrez = sig.ids,
    gene.name = gene.symbols,
    gene.cluster = gene.cluster,
    sample.cluster = sample.cluster,
    sample.names = colnames(dat),
    meta = nmeta,
    meta.anot = nmeta.anot,
    data = res_list,
    expval = expval
  );
  .set.mSet(mSetObj);
  json.mat <- rjson::toJSON(json.res);
  sink(fileNm);
  cat(json.mat);
  sink();
  return(1);
}

