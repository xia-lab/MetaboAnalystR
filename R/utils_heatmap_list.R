##################################################
## R script for ExpressAnalyst
## Description: functions only for list data analysis
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################


my.prepare.list.heatmap.json <- function(dataSet){
  paramSet <- readSet(paramSet, "paramSet");
  dat <- dataSet$prot.mat;
  sig.ids <- rownames(dat);
  gene.symbols <- doEntrez2SymbolMapping(sig.ids, paramSet$data.org, paramSet$data.idType);
  stat.pvals <- dat[,1];
  expval <- 0;
  expval <- sum(dat);

  # scale each gene 
  # now pearson and euclidean will be the same after scaleing
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  
  grps <- "datalist1"
  cls <- "datalist1"
  
  # convert back to numeric 
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks
  if(expval !=0){
    dat_pos <- as.matrix(dat[sign(dat[,1]) == 1,])
    dat_neg <- as.matrix(dat[sign(dat[,1]) == -1,])
    pos_inx <- sign(dat[,1]) == 1;
    neg_inx <- sign(dat[,1]) == -1;
    if(nrow(dat_pos) == 0){
      res <- apply(unname(dat), 2, function(x){
        y =log(abs(x)) + 0.000001
        16-as.numeric(cut(y, breaks=15))
      });
    }else if(nrow(dat_neg) == 0){
      res <- apply(unname(dat), 2, function(x){
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
      dat[pos_inx,] <- res_pos;
      dat[neg_inx,] <- res_neg;
      res <- dat;
      #print(res);
    }
  }else{
    zero.inx <- dataSet$prot.mat == 0;
    res <- dataSet$prot.mat;
    res[zero.inx] <- 32;
  }
  
  res_list <- list();
  for(i in 1:length(res)){
    res_list[[i]] <- list(res[i]);
  }
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  nmeta <- list(100)
  nmeta.anot <- list()
  
  nmeta.anot["datalist1"] <- nmeta[1]
  
  nmeta <- list(nmeta)
  names(nmeta) <- "datalists"
  
  json.res <- list(
    data.type = "singlelist", 
    gene.id = gene.symbols,
    gene.entrez = sig.ids,
    gene.name = gene.symbols,
    gene.cluster = 1,
    sample.cluster = 1,
    sample.names = list("datalist1"),
    meta = nmeta,
    meta.anot = nmeta.anot,
    data = res_list,
    expval = expval
  );
  rownames(dat) <- gene.symbols
  fast.write(dat,"heatmap_matrix.csv", row.names=TRUE)
  return(json.res);
}

my.prepare.multilist.heatmap.json <- function(dataSet){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  sel.nms <- names(mdata.all)
  expval<-0;
  for(i in 1:length(sel.nms)){
    dataNm <- sel.nms[i];
    dataSet <- readDataset(dataNm);
    len <- nrow(dataSet$prot.mat)
    if(i == 1){
      expval <- sum(dataSet$prot.mat)
      gene_list <-rownames(dataSet$prot.mat)
    }else{
      gene_list <-c(gene_list, rownames(dataSet$prot.mat))
      expval <- expval + sum(dataSet$prot.mat)
    }
  }
  
  gene_list <- unique(gene_list)
  allmat <- matrix(NA, nrow=length(gene_list), ncol=length(sel.nms))
  rownames(allmat) <- gene_list
  
  for(i in 1:length(sel.nms)){
    dataName <- sel.nms[i];
    dataSet <- readDataset(dataName);
    cols <- colnames(allmat)[colnames(allmat) %in% dataName]
    if(expval ==0){
      rows <- which(rownames(allmat) %in% rownames(dataSet$prot.mat))
      inx <-match(rownames(allmat) ,rownames(dataSet$prot.mat))
      allmat[, i] <- as.vector(dataSet$prot.mat)[inx]
    }else{
      rows <- which(rownames(allmat) %in% rownames(dataSet$prot.mat))
      inx <-match(rownames(allmat) ,rownames(dataSet$prot.mat))
      allmat[, i] <- as.vector(dataSet$prot.mat)[inx]
    }
  } 
  colnames(allmat) <- sel.nms 
  inx <- apply(allmat, 1, function(x){sum(is.na(x))});  
  ord.inx <- order(inx)
  allmat <- allmat[ord.inx,]
  gene.symbols <- doEntrez2SymbolMapping(rownames(allmat), paramSet$data.org, paramSet$data.idType)
  
  na.inx <- is.na(allmat)
  zero.inx <- allmat == 0
  
  allmatb <- allmat
  
  allmatb[na.inx] <- 0
  allmatb[zero.inx] <- 1
  rownames(allmatb) <- gene.symbols
  fast.write(allmatb,"heatmap.csv", row.names=TRUE)  
  
  if(expval != 0){
    pos.inx <- allmat>0 & !na.inx
    neg.inx <- allmat<0 & !na.inx
    allmat[neg.inx] <- 16 - as.numeric(cut(log(abs(allmat[neg.inx])) , breaks=15))
    allmat[pos.inx] <- 15 + as.numeric(cut(log(allmat[pos.inx]) , breaks=15))
    allmat[zero.inx] <- 32
  }else{
    zer.inx <- allmat == 0 & !na.inx
    nb <- apply(allmat, 1, function(x){sum(!is.na(x))});
    for(i in 1:nrow(allmat)){
      row <- allmat[i,]
      inx <- row == 0
      allmat[i, inx] <- nb[i]
    }
    allmat[zer.inx] <- try(15 + as.numeric(cut(allmat[zer.inx] , breaks=15)));
    if(class(allmat[zer.inx]) == "try-error") {
      allmat[zer.inx] <- 32
    }else{
      15 + as.numeric(cut(allmat[zer.inx] , breaks=15))
    }
  }
  allmat[na.inx] <- 31;
  res_list <- list();
  for(i in 1:nrow(allmat)){
    res_list[i] <- list(unname(allmat[i,]))
  }
  
  nmeta <- as.numeric(as.factor(colnames(allmat))) + 99
  nmeta.anot <- list()
  
  for(i in 1:length(unique(nmeta))){
    nmeta.anot[[colnames(allmat)[i]]] <- nmeta[i]
  }
  nmeta <- list(nmeta)
  names(nmeta) <- "datalists"
  
  json.res <- list(
    data.type = "mutlilist",
    gene.id = gene.symbols,
    gene.entrez = rownames(allmat),
    gene.name = rownames(allmat),
    gene.cluster = 1,
    sample.cluster = 1,
    sample.names = colnames(allmat),
    meta = nmeta,
    meta.anot = nmeta.anot,
    data.lbl = "NA",
    data = res_list,
    expval = expval
  );
  
  return(json.res);
}
