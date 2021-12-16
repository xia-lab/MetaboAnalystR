my.heatmap.json <- function(mSetObj=NA, libOpt, libVersion, minLib, fileNm, filtOpt, 
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
  #mSetObj$dataSet$mumRT <- is.rt
  #mSetObj$dataSet$mumRT.type <- mumRT.type
  
  if(mSetObj$paramSet$mumRT){
    feat_info <- rownames(data)
    feat_info_split <- matrix(unlist(strsplit(feat_info, "__", fixed=TRUE)), ncol=2, byrow=T)
    colnames(feat_info_split) <- c("m.z", "r.t")
    
    if(length(unique(feat_info_split[,1])) != length(feat_info_split[,1])){
      
      # ensure features are unique
      mzs_unq <- feat_info_split[,1][duplicated(feat_info_split[,1])] 
      set.seed(123)
      if(length(mzs_unq)>0){
        feat_info_split[,1][duplicated(feat_info_split[,1])] <- sapply(mzs_unq, function(x) paste0(x, sample(1:999, 1, replace = FALSE)))
      }
    }
    
    if(mSetObj$paramSet$mumRT.type == "minutes"){
      rtime <- as.numeric(feat_info_split[,2])
      rtime <- rtime * 60
      feat_info_split[,2] <- rtime
    }
    
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
  
  if(filtOpt == "filtered"){
    mSetObj <- .setup.psea.library(mSetObj, libOpt, libVersion, minLib);
    matched_res <- qs::qread("mum_res.qs");
    res_table <- matched_res;
    data = data[which(l %in% res_table[,"Query.Mass"]),]
    res = res[which(rownames(res) %in% res_table[,"Query.Mass"]),]
  }
  
  stat.pvals <- unname(as.vector(res[,2]));
  t.stat <- unname(as.vector(res[,1]));
  org = unname(strsplit(libOpt,"_")[[1]][1])
  # scale each gene 
  dat <- t(scale(t(data)));
  
  rankPval = order(as.vector(stat.pvals))
  stat.pvals = stat.pvals[rankPval]
  dat = dat[rankPval,]
  
  t.stat = t.stat[rankPval]
  
  # now pearson and euclidean will be the same after scaleing
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
    #stat.pvals <- matrix(stat.pvals);
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(dat);
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
  res <- apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))}); #previously t() when using RJSONIO
  
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

doHeatmapMummichogTest <- function(mSetObj=NA, nm, libNm, ids){
  
  mSetObj<-.get.mSet(mSetObj);
  
  if(ids == "overall"){
    .on.public.web <<- F;
    is.rt <- mSetObj$paramSet$mumRT;
    mSetObj<-PreparePrenormData(mSetObj)
    mSetObj<-Normalization(mSetObj, "MedianNorm", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)
    mSetObj<-Ttests.Anal(mSetObj, F, 0.05, FALSE, TRUE)
    mSetObj<-Convert2Mummichog(mSetObj, is.rt, F, mSetObj$paramSet$mumRT.type, "tt", mSetObj$dataSet$mode);
    mSetObj<-InitDataObjects("mass_all", "mummichog", FALSE)
    mSetObj<-SetPeakFormat(mSetObj, "mpt")
    #mSetObj<-UpdateInstrumentParameters(mSetObj, 10, mSetObj$dataSet$mode);
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
    mSetObj<-Read.PeakListData(mSetObj, filename);
    mSetObj<-SanityCheckMummichogData(mSetObj)
    mSetObj<-SetPeakEnrichMethod(mSetObj, "mum", "v2")
    mSetObj<-SetMummichogPval(mSetObj, 0.05)
    .on.public.web <<- T;
    .set.mSet(mSetObj);
    anal.type <<- "integ";
  }else{
    gene.vec <- unlist(strsplit(ids, "; "));
    anal.type <<- "mummichog";
    is.rt <- mSetObj$paramSet$mumRT;
    if(is.rt){
      feat_info_split <- matrix(unlist(strsplit(gene.vec, "__", fixed=TRUE)), ncol=2, byrow=T)
      colnames(feat_info_split) <- c("m.z", "r.t")
      mSetObj$dataSet$input_mzlist <- as.numeric(feat_info_split[,1]);
      mSetObj$dataSet$N <- length(mSetObj$dataSet$input_mzlist);
    }else{
      mSetObj$dataSet$input_mzlist <- gene.vec;
      mSetObj$dataSet$N <- length(gene.vec);
    }
  }
  
  mSetObj$mum_nm <- paste0(nm,".json");
  mSetObj$mum_nm_csv <- paste0(nm,".csv");
  .set.mSet(mSetObj);
  return(PerformPSEA("NA", libNm, "current", 3, 100));
}
