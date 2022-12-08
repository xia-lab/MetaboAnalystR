##################################################
## R script for ExpressAnalyst
## Description: functions for DE analysis
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#in DE analysis page, select metadata primary factor, secondary factor and blocking factor 
SetSelectedMetaInfo <- function(dataName="", meta0, meta1, block1){
  dataSet <- readDataset(dataName);
  if(meta0 == "NA"){
    RegisterData(dataSet, 0);
  }else{
    cls <- dataSet$meta[, meta0];
    dataSet$fst.cls <- cls; # for PCA plotting
    block <- NULL;
    dataSet$sec.cls <- "NA";
    if(meta1 != "NA"){
      if(block1){
        block <- dataSet$meta[, meta1];
      }else{ # two factor
        cls <- interaction(dataSet$meta[, c(meta0, meta1)], sep = "_", lex.order = TRUE);
      }
      dataSet$sec.cls <- dataSet$meta[, meta1]; # for pca coloring
    }
    dataSet$cls <- cls; # record main cls;
    dataSet$block <- block;
    RegisterData(dataSet, levels(cls));
  }
}

# perform differential analysis
# default: all pair-wise comparison (A-B) + (B-C) + (A-C)
# custom: only compare two groups (A-C)
# time: compare consecutive groups (B-A) + (C-B)
# reference: all others against common reference (A-C) + (B-C)
# nested: (A-B)+(C-D) 
PerformDEAnal<-function (dataName="", anal.type = "default", par1 = NULL, par2 = NULL, nested.opt = "intonly"){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");;

  if (dataSet$de.method == "deseq2") {
    dataSet <- .prepareContrast(dataSet, anal.type, par1, par2, nested.opt);
    .prepare.deseq(dataSet, anal.type, par1, par2 , nested.opt);
    .perform.computing();
    dataSet <- .save.deseq.res(dataSet);
  }else{
    dataSet <- .prepareContrast(dataSet, anal.type, par1, par2, nested.opt);
    dataSet <- .perform_limma_edger(dataSet);
  }
  return(RegisterData(dataSet));
}

.prepare.deseq<-function(dataSet, anal.type, par1, par2, nested.opt){
  .prepareContrast(dataSet, anal.type, par1, par2, nested.opt);
  my.fun <- function(){
    require(DESeq2);
    dataSet <- dat.in$data;
    contrast.matrix <- dataSet$contrast.matrix;
    design <- dataSet$design
    if (is.null(dataSet$sec.cls)){
      colData <- data.frame(dataSet$fst.cls)
      colnames(colData) <- "condition"
      dds <- DESeqDataSetFromMatrix(countData=round(dataSet$data.anot), colData = colData, design=dataSet$design);
    } else {
      colData <- data.frame(dataSet$fst.cls, dataSet$sec.cls, dataSet$cls);
      colnames(colData) <- c("condition", "type", "condition_type");
      dds <- DESeqDataSetFromMatrix(countData=round(dataSet$data.anot), colData = colData, design=dataSet$design);
    }   
    
    dds <- DESeq(dds, betaPrior=FALSE) 
    qs::qsave(dds, "deseq.res.obj.rds");
    vec <- as.numeric(c(contrast.matrix[,1]));
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
    topFeatures <- topFeatures[ord.inx, ];
    return(topFeatures);
  }
  dat.in <- list(data=dataSet, contrast.matrix = dataSet$contrast.matrix, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");

  return(1);
}

.save.deseq.res <- function(dataSet){
  dat.in <- qs::qread("dat.in.qs"); 
  my.res <- dat.in$my.res;
  dataSet$comp.res <- my.res;
  qs::qsave(my.res, file="dat.comp.res.qs");
  return(dataSet);
}


.prepareContrast <-function(dataSet, anal.type = "default", par1 = NULL, par2 = NULL, nested.opt = "intonly"){
  msgSet <- readSet(msgSet, "msgSet");
  cat(anal.type, par1, par2, nested.opt, "\n")
  set.seed(1337);
  dataSet$par1 <- par1;

  myargs <- list();
  cls <- dataSet$cls
  dataSet$comp.type <- anal.type
  grp.nms <- levels(cls)
  
  if (anal.type == "default") {
    inx <- 0
    for (m in 1:(length(grp.nms) - 1)) {
      for (n in (m + 1):length(grp.nms)) {
        inx <- inx + 1
        myargs[[inx]] <- paste(grp.nms[m], "-", grp.nms[n], sep = "");
      }
    }
    filename <- "SigGene_pairwise";
  } else if (anal.type == "time") {
    for (i in 2:length(grp.nms)) {
      myargs[[i - 1]] <- paste(grp.nms[i], "-", grp.nms[i-1], sep = "")
    }
    filename <- "SigGene_time_series";
  } else if (anal.type == "custom") {
    grp.nms <- strsplit(par1, " vs. ")[[1]]
    myargs[[1]] <- paste(grp.nms, collapse = "-")
    dataSet$grp.nms <- grp.nms;
    filename <- paste("SigGene_", paste(grp.nms, collapse = "_vs_"), sep = "")
  } else if (anal.type == "reference") {
    ref <- par1;
    cntr.cls <- grp.nms[grp.nms != ref]
    myargs <- as.list(paste(cntr.cls, "-", ref, sep = ""));
    filename <- paste("SigGene_reference_", ref, sep = "");
  } else if (anal.type == "nested") {
    grp.nms1 <- strsplit(par1, " vs. ")[[1]]
    grp.nms2 <- strsplit(par2, " vs. ")[[1]]
    if (all(grp.nms1 == grp.nms2)) {
      msgSet$current.msg <-"The two nested groups are the same. Please choose two different groups."
      saveSet(msgSet, "msgSet");      
      return(0)
    }
    grp.nms <- unique(c(grp.nms1, grp.nms2))
    if (nested.opt == "intonly") {
      myargs[[1]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    } else {
      myargs[[1]] <- paste(grp.nms1, collapse = "-")
      myargs[[2]] <- paste(grp.nms2, collapse = "-")
      myargs[[3]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    }
    filename <- paste("SigGene_nested_", paste(paste(grp.nms1, collapse = "_vs_"), "_", paste(grp.nms2, collapse = "_vs_"), sep = ""), sep = "")
  } else {
    print(paste("Not supported: ", anal.type))
  }
  
  dataSet$filename <- filename;
  require(limma);
  design <- dataSet$design;
  myargs[["levels"]] <- design;
  dataSet$contrast.type <- anal.type;
  contrast.matrix <- do.call(makeContrasts, myargs);
  dataSet$contrast.matrix <- contrast.matrix;
  return(dataSet);
}

.perform_limma_edger <- function(dataSet){
  design <- dataSet$design;
  paramSet <- readSet(paramSet, "paramSet");;

  contrast.matrix <- dataSet$contrast.matrix;
  msgSet <- readSet(msgSet, "msgSet");
  if (dataSet$de.method == "limma") {
    if (is.null(dataSet$block)) {
      fit <- lmFit(dataSet$data.norm, design)
    } else {
      corfit <- duplicateCorrelation(dataSet$data.norm, design, block = dataSet$block)
      fit <- lmFit(dataSet$data.norm, design, block = dataSet$block, correlation = corfit$consensus)
    }
    
    if (!is.fullrank(design)) {
      msgSet$current.msg <- "This metadata combination is not full rank! Please use other combination.";
      saveSet(msgSet, "msgSet");  
      return(0)
    }
    
    df.residual <- fit$df.residual
    if (all(df.residual == 0)) {
      msgSet$current.msg <- "There is not enough replicates in each group (no residual degrees of freedom)!";
      saveSet(msgSet, "msgSet");  
      return(0);
    }
    fit2 <- contrasts.fit(fit, contrast.matrix)
    fit2 <- eBayes(fit2)
    topFeatures <- topTable(fit2, number = Inf, adjust.method = "fdr");
    
  } else {
    set.seed(1) 
    require(edgeR)
    y <- DGEList(counts = dataSet$data.anot, group = dataSet$cls)
    y <- calcNormFactors(y)
    y <- estimateGLMCommonDisp(y, design, verbose = FALSE)
    y <- estimateGLMTrendedDisp(y, design)
    y <- estimateGLMTagwiseDisp(y, design)
    fit <- glmFit(y, design)
    lrt <- glmLRT(fit, contrast = contrast.matrix)
    topFeatures <- topTags(lrt, n = Inf)$table
  }

  nms <- colnames(topFeatures)
  nms[which(nms == "FDR")] <- "adj.P.Val";
  nms[which(nms == "PValue")] <- "P.Value";
  
  colnames(topFeatures) <- nms  
  dataSet$comp.res <- topFeatures;
  return(dataSet);
}


SetupDesignMatrix<-function(dataName="", deMethod){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  cls <- dataSet$cls; 
  design <- model.matrix(~ 0 + cls) # no intercept
  colnames(design) <- levels(cls);
  dataSet$design <- design;
  dataSet$de.method <- deMethod;
  saveSet(paramSet, "paramSet");
  return(RegisterData(dataSet));
}


# perform limma on given two groups selected 
# used by integarative analysis

#'Perform differential analysis using Limma method (meta-analysis)
#'@description Detect number of DE genes in individual data set for quality checking purposes 
#'@param dataName File name of data set.
#'@param grps Selected two groups for comparison
#'@param p.lvl P-value threshold
#'@param fc.lvl Fold-change threshold
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

PerformLimmaDE<-function(dataName="", grps, p.lvl, fc.lvl=NULL){
  
  dataSet <- readDataset(dataName);
  dataSet$pval <- p.lvl
  if(length(levels(dataSet$cls))>2){ 
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    sel.inx <- as.character(dataSet$cls) %in% grp.nms;
  }else{
    sel.inx <- rep(T, ncol(dataSet$data));
  }
  
  group <- factor(dataSet$cls[sel.inx]); # note regenerate factor to drop levels 
  data <- dataSet$data[, sel.inx];
  
  res.limma <- PerformLimma(data, group);
  res.all <- GetLimmaResTable(res.limma$fit.obj);
  
  if(!is.null(fc.lvl)){
    hit.inx <- abs(res.all$logFC)>= fc.lvl & res.all$adj.P.Val <= p.lvl
  }else{
    hit.inx <- res.all$adj.P.Val <= p.lvl
  }
  if(sum(hit.inx) == 0){
    return (c(1, 0, nrow(res.all)));
  }
  # note, hit.inx can contain NA, not T/F
  hit.inx <- which(hit.inx);
  res <- res.all[hit.inx, , drop=F];
  
  # rm .txt suffix for new names
  shortNm <- substring(dataName, 0, nchar(dataName)-4);
  fast.write(signif(res[,-1],5), file=paste("SigGenes_", shortNm, ".csv",sep=""));
  
  sig.count <- nrow(res);
  de.genes <- rownames(res);
  res.mat <- cbind(res.all$logFC, res.all$adj.P.Val);
  rownames(res.mat) <- rownames(res.all);
  non.sig.count <- nrow(data)-sig.count;
  rm(res.all);
  
  gc();
  
  # record the sig gene vec
  output <- c(1, sig.count, non.sig.count);
  return(RegisterData(dataSet, output));
}

# perfor differential analysis for array/RNA seq data
# for two groups only (used for meta-analysis)
PerformLimma<-function(data, group){
  require(limma);
  data <- data;
  design <- model.matrix(~-1 + group);
  fit <- lmFit(data, design)
  
  grps.cmp <- paste("group", levels(group)[2], " - ", "group", levels(group)[1], sep="");
  myargs <- list(grps.cmp, levels = design);
  contrast.matrix <- do.call(makeContrasts, myargs);
  fit <- contrasts.fit(fit, contrast.matrix)
  fit <- eBayes(fit);
  gc();
  return (list(fit.obj=fit));
}

# get result table from eBayes fit object
GetLimmaResTable<-function(fit.obj){
  resTable <- topTable(fit.obj, number=Inf, adjust.method="BH");
  if(!is.null(resTable$ID)){ # for older version
    rownames(resTable) <- resTable$ID;
    resTable$ID <- NULL;
  }
  return (resTable);
}

