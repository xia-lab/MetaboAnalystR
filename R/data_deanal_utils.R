##################################################
## R script for ExpressAnalyst
## Description: functions for DE analysis
##
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#' Select Metadata Factors for DE Analysis
#'
#' This function is used to select metadata factors for a Differential Expression (DE) analysis.
#'
#' @param dataName A character string specifying the name of the dataset.
#' @param meta0 The primary metadata factor
#' @param meta1 The secondary metadata factor
#' @param block1 The blocking factor for
#'
#' @author Guangyan Zhou, \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @export
#'
SetSelectedMetaInfo <- function(dataName="", meta0, meta1, block1){
  dataSet <- readDataset(dataName);
  if(meta0 == "NA"){
    RegisterData(dataSet, 0);
  }else{
    rmidx <- which(dataSet$meta.info.[, meta0]=="NA")
    if(meta1 != "NA"){
        rmidx <- c(rmidx,which(dataSet$meta.info[, meta1]=="NA"))
    }
    if(length(rmidx)>0){
       meta<- dataSet$meta.info[-rmidx,]
     #print(meta);
       for(col in 1:ncol(meta)){
        meta[,col]<- droplevels(meta[,col])
       }
       dataSet$rmidx <- rmidx
    }else{
        meta<- dataSet$meta.info
    }
    cls <- meta[, meta0];
    dataSet$fst.cls <- cls; # for PCA plotting
    block <- NULL;
    dataSet$sec.cls <- "NA";
    if(meta1 != "NA"){
      if(block1){
        block <- meta[, meta1];
      }else{ # two factor
        cls <- interaction(meta[, c(meta0, meta1)], sep = "_", lex.order = TRUE);
      }
      dataSet$sec.cls <- meta[, meta1]; # for pca coloring
    }
    dataSet$analysisVar <- meta0 
    dataSet$secondVar <- meta1
    dataSet$cls <- cls; # record main cls;
    dataSet$block <- block;
    RegisterData(dataSet, levels(cls)[levels(cls)!="NA"]);
  }
}

#' Perform Differential Analysis
#'
#' This function performs differential analysis based on different types of comparisons.
#'
#' @param dataName A character string specifying the name of the dataset.
#' @param anal.type The type of analysis to perform. Options: "default", "custom", "time", "reference", "nested".
#' @param par1 Parameter 1, depending on the analysis type.
#' @param par2 Parameter 2, depending on the analysis type.
#' @param nested.opt Option for nested analysis. Options: "intonly" (default), "all".
#' @param robustTrend Logical. If TRUE, use robust trend test.
#'
#' @return Results of the differential analysis.
#' @details default: all pair-wise comparison (A-B) + (B-C) + (A-C), custom: only compare two groups (A-C), time: compare consecutive groups (B-A) + (C-B), reference: all others against common reference (A-C) + (B-C), nested: (A-B)+(C-D) 
#' @author Guangyan Zhou, \email{guangyan.zhou@mail.mcgill.ca}
#' @export
#' @license MIT
#'
PerformDEAnal<-function (dataName="", anal.type = "default", par1 = NULL, par2 = NULL, nested.opt = "intonly", robustTrend=F){

  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");

  if (dataSet$de.method == "deseq2") {
    dataSet <- prepareContrast(dataSet, anal.type, par1, par2, nested.opt);
    .prepare.deseq(dataSet, anal.type, par1, par2 , nested.opt);
    .perform.computing();
    dataSet <- .save.deseq.res(dataSet);
  }else{
    dataSet <- prepareContrast(dataSet, anal.type, par1, par2, nested.opt);
    dataSet <- .perform_limma_edger(dataSet, robustTrend);
  }
  return(RegisterData(dataSet));
}

.prepare.deseq<-function(dataSet, anal.type, par1, par2, nested.opt){
  my.fun <- function(){
    require(DESeq2);
    dataSet <- dat.in$data;
    contrast.matrix <- dataSet$contrast.matrix;
    design <- dataSet$design
    if(length(dataSet$rmidx)>0){
       data.anot <- dataSet$data.anot[,-dataSet$rmidx]
    }else{
       data.anot <- dataSet$data.anot
    }
   if( any(grepl("(^[0-9]+).*", dataSet$fst.cls))){
        fst.cls <- paste0(dataSet$analysisVar,"_",dataSet$fst.cls)
      }else{
        fst.cls <- dataSet$fst.cls
     }
    if (is.null(dataSet$sec.cls) | all(dataSet$sec.cls=="NA")){
     colData <- data.frame(fst.cls)
      colnames(colData) <- "condition"    
      dds <- DESeqDataSetFromMatrix(countData=round(data.anot), colData = colData, design=dataSet$design);
    } else {
      colData <- data.frame(fst.cls, dataSet$sec.cls, dataSet$cls);
      colnames(colData) <- c("condition", "type", "condition_type");
      dds <- DESeqDataSetFromMatrix(countData=round(data.anot), colData = colData, design=dataSet$design);
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


prepareContrast <-function(dataSet, anal.type = "reference", par1 = NULL, par2 = NULL, nested.opt = "intonly"){
  msgSet <- readSet(msgSet, "msgSet");
  cat(anal.type, par1, par2, nested.opt, "\n")
  set.seed(1337);
  myargs <- list();
  cls <- dataSet$cls
  dataSet$comp.type <- anal.type
  grp.nms <- levels(cls)
  analysisVar <- dataSet$analysisVar
  if(dataSet$cont.inx[analysisVar] |  any(grepl("(^[0-9]+).*", grp.nms))){
    if(grepl( "vs",par1)){
      par1 <- strsplit(par1, " vs. ")[[1]]
      par1 <- paste0(analysisVar,"_",par1[1]," vs. ",analysisVar,"_",par1[2])
    }else{
      par1<- paste0(analysisVar,"_",par1)
    }
    if(par2 != "NA"){
      if(grepl( "vs",par2)){
        par2 <- strsplit(par2, " vs. ")[[1]]
        par2 <- paste0(analysisVar,"_",par2[1]," vs. ",analysisVar,"_",par2[2])
      }else{
        par2<- paste0(analysisVar,"_",par2)
      }
    }

    if(any(grepl("(^[0-9]+).*",  colnames(dataSet$design)))){
      colnames(dataSet$design) = as.character(sapply( colnames(dataSet$design),function(x) paste0(analysisVar,"_",x)))
    }
    grp.nms <- paste0(analysisVar,"_",grp.nms)
    
  }

  dataSet$par1 <- par1;
  
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
    dataSet$contrast <- paste(grp.nms, collapse = "_vs_");
  } else if (anal.type == "reference") {
    ref <- par1;
    cntr.cls <- grp.nms[grp.nms != ref]
    myargs <- as.list(paste(cntr.cls, "-", ref, sep = ""));
    dataSet$ref <- ref; 
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
      dataSet$nested.int.opt <- "True";
      myargs[[1]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    } else {
      dataSet$nested.int.opt <- "False";
      myargs[[1]] <- paste(grp.nms1, collapse = "-")
      myargs[[2]] <- paste(grp.nms2, collapse = "-")
      myargs[[3]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    }
    dataSet$contrast <- paste(paste(paste(grp.nms1, collapse = "_vs_"), "_", paste(grp.nms2, collapse = "_vs_"), sep = ""), sep = "")
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


.perform_limma_edger <- function(dataSet, robustTrend = F){
  require(limma);
  design <- dataSet$design;
  paramSet <- readSet(paramSet, "paramSet");
  contrast.matrix <- dataSet$contrast.matrix;
  msgSet <- readSet(msgSet, "msgSet");
  if(length(dataSet$rmidx)>0){
       data.norm <- dataSet$data.norm[,-dataSet$rmidx]
    }else{
       data.norm <- dataSet$data.norm
    }

  if (dataSet$de.method == "limma") {
    if (is.null(dataSet$block)) {
      fit <- lmFit(data.norm, design)
    } else {
      corfit <- duplicateCorrelation(data.norm, design, block = dataSet$block)
      fit <- lmFit(data.norm, design, block = dataSet$block, correlation = corfit$consensus)
    }
    
    if (!is.fullrank(design)) {
      msgSet$current.msg <- "This metadata combination is not full rank! Please use other combination.";
      saveSet(msgSet, "msgSet");  
      return(0)
    }
   
    df.residual <- fit$df.residual
    if (all(df.residual == 0)) {
      msgSet$current.msg <- "All residuals equal 0. There is not enough replicates in each group (no residual degrees of freedom)!";
      saveSet(msgSet, "msgSet");  
      return(0);
    }
    fit2 <- contrasts.fit(fit, contrast.matrix);
    fit2 <- eBayes(fit2, trend=robustTrend, robust=robustTrend);
    topFeatures <- topTable(fit2, number = Inf, adjust.method = "fdr");
  } else {
    set.seed(1) 
    require(edgeR)
    if(length(dataSet$rmidx)>0){
      data.anot <- dataSet$data.anot[,-dataSet$rmidx];
    }else{
      data.anot <- dataSet$data.anot;
    }
    y <- DGEList(counts = data.anot, group = dataSet$cls)
    y <- calcNormFactors(y)
    y <- estimateGLMCommonDisp(y, design, verbose = FALSE)
    y = tryCatch(
         {estimateGLMTrendedDisp(y, design)
       }, error=function(e){
         msgSet$current.msg <- e
         }, warning=function(w){
          msgSet$current.msg <- c(msgSet$current.msg,w)
          saveSet(msgSet, "msgSet");  
         return(0)
          })
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
  dataSet$pval <- 0.05;
  dataSet$fc.val <- 1;

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
#'License: MIT
#'@export
#'
PerformLimmaDE<-function(dataName="", grps, p.lvl, fc.lvl=NULL){
  dataSet <- readDataset(dataName);
  dataSet$pval <- p.lvl
  if(length(levels(dataSet$cls))>2){ 
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    sel.inx <- as.character(dataSet$cls) %in% grp.nms;
  }else{
    sel.inx <- rep(T, ncol(dataSet$data.norm));
  }
  
  group <- factor(dataSet$cls[sel.inx]); # note regenerate factor to drop levels 
  data <- dataSet$data.norm[, sel.inx];
  
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
  #print(identical(colnames(data), group))
  #print(colnames(data))
  #print(group)

  #print("limma");
  require(limma);
  design <- model.matrix(~-1 + group);
  fit <- lmFit(data, design);
  
  grps.cmp <- paste("group", levels(group)[2], " - ", "group", levels(group)[1], sep="");
  myargs <- list(grps.cmp, levels = design);
  contrast.matrix <- do.call(makeContrasts, myargs);
  fit <- contrasts.fit(fit, contrast.matrix);
  fit <- eBayes(fit);
  gc();
  return (list(fit.obj=fit));
}

# get result table from eBayes fit object
GetLimmaResTable<-function(fit.obj){
  require(limma);
  resTable <- topTable(fit.obj, number=Inf, adjust.method="BH");
  if(!is.null(resTable$ID)){ # for older version
    rownames(resTable) <- resTable$ID;
    resTable$ID <- NULL;
  }
  return (resTable);
}

MultiCovariateRegression <- function(fileName,
                                     analysis.var, # metadata variable name
                                     ref = NULL, # reference class from analysis.var metadata (only if categorical)
                                     contrast = "anova",  # comparison class from analysis.var (only if categorical)
                                     blocking.factor = NULL, 
                                     robustTrend = F, 
                                     internal=F){ # whether returns 0/1 or dataset object

  dataSet <- readDataset(fileName);
  if(!exists('adj.vec')){ ## covariate to adjust for
    adj.vec <- "NA";
  }else{
    if(length(adj.vec) > 0){

    }else{
      adj.vec <- "NA"
    }
  }
  interim <- .multiCovariateRegression(dataSet, analysis.var, ref, contrast, blocking.factor, adj.vec, robustTrend, F)
  if(is.list(interim)){
    res <- 1;
  }else{
    res <- interim;
  }
  return(res);
}

.multiCovariateRegression <- function(dataSet,
                                     analysis.var, # metadata variable name
                                     ref = NULL, # reference class from analysis.var metadata (only if categorical)
                                     contrast = "anova",  # comparison class from analysis.var (only if categorical)
                                     # fixed.effects = NULL,  
                                     blocking.factor = NULL, 
                                     adj.factors="NA",# metadata variables to adjust for
                                     robustTrend = F, 
                                     internal=F){ # whether returns 0/1 or dataset object, T for metaanal covariate
  # load libraries
  library(limma);
  library(dplyr);
  
  # need a line for read dataSet
  msgSet <- readSet(msgSet, "msgSet");
  dataSet$rmidx <- NULL;

  # for embedded inside tools (ExpressAnalyst etc)
  if(internal){
  inmex.meta<-qs::qread("inmex_meta.qs");
  #only get shared features
  #feature_table <- dataSet$data.norm[rownames(dataSet$data.norm) %in% rownames(inmex.meta$data), ];
  feature_table <- inmex.meta$data[,colnames(inmex.meta$data) %in% colnames(dataSet$data.norm)];
  }else{
  feature_table <- dataSet$data.norm;
  }
  covariates <- dataSet$meta.info;

  matched_indices <- match(colnames(feature_table), rownames(covariates));
  covariates <- covariates[matched_indices, ,drop=F ];
  dataSet$meta.info <- covariates;
  #fixed.effects <- adj.vec
  # process covariates
  var.types <- lapply(covariates, class) %>% unlist();
  covariates[,c(var.types == "character")] <- lapply(covariates[,c(var.types == "character")], factor);

  # aggregate vars
  all.vars <- c(analysis.var);
  vars <- c(analysis.var);

  if(all(adj.factors != "NA")){
    vars = c(vars, adj.factors);
  }
  
  if(!is.null(blocking.factor) && !is.na(blocking.factor) && blocking.factor!="NA" && blocking.factor!="" ){
    all.vars = c(all.vars, blocking.factor);
  }

  all.vars<- unique(all.vars);
    
  covariates <- covariates[,unique(c(vars, all.vars)),drop=F];
  rmidx <-which(apply(covariates, 1, function(x) "NA" %in% x))
  
  if(length(rmidx)>0){
    covariates <- covariates[-rmidx,,drop=F];
    dataSet$rmidx <- rmidx;
  }
  feature_table <- feature_table[,colnames(feature_table) %in% rownames(covariates)];
  
  if(!identical(colnames(feature_table), rownames(covariates))){
 
    msgSet$current.msg <- "Error - order of samples got mixed up between tables";
    saveSet(msgSet, "msgSet");
    return(0)
  }
  
  # get analysis type
  analysis.type = ifelse(dataSet$disc.inx[analysis.var],"disc","cont")
  if(is.na(analysis.type)){
    msgSet$current.msg <- "Analysis var not found in our database!";
    saveSet(msgSet, "msgSet");
    return(0)
  }
  
  if(analysis.type == "disc"){
    # build design and contrast matrix
    #  covariates[, analysis.var] <- covariates[, analysis.var] %>% make.names() %>% factor();
    #str(covariates)
    grp.nms <- levels(covariates[, analysis.var]);
    
    if(any(grepl("(^[0-9]+).*", grp.nms))){
      grp.nms <- paste0(analysis.var,"_",grp.nms);
      if(!(is.null(ref))& ref!="NA"){
        ref <- paste0(analysis.var,"_", ref)
      }
      if(contrast!="anova"){
        contrast <- paste0(analysis.var,"_", contrast)
      }     
    }
    
    for(col in 1:ncol(covariates)){
      if(dataSet$cont.inx[colnames(covariates)[col]]){
        covariates[,col] <- as.numeric(as.character(covariates[,col]))
      }
    }
    
    design <- model.matrix(formula(paste0("~ 0", paste0(" + ", vars, collapse = ""))), data = covariates);
    colnames(design)[1:length(grp.nms)] <- grp.nms;
    myargs <- list();
    if(contrast == "anova"){ 
      contrasts <- grp.nms[grp.nms != ref];
      myargs <- as.list(paste(contrasts, "-", ref, sep = "")); 
    } else {
      myargs <- as.list(paste(contrast, "-", ref, sep = ""));
    }
    myargs[["levels"]] <- design;
    contrast.matrix <- do.call(makeContrasts, myargs);
    
    # handle blocking factor
    if (is.null(blocking.factor) | is.na(blocking.factor) | blocking.factor=="NA" | blocking.factor == "") {
      fit <- lmFit(feature_table, design);
    } else {
      block.vec <- covariates[,blocking.factor];
      corfit <- duplicateCorrelation(feature_table, design, block = block.vec);
      fit <- lmFit(feature_table, design, block = block.vec, correlation = corfit$consensus);
    }

    # get results
    fit <- contrasts.fit(fit, contrast.matrix);
    fit <- eBayes(fit, trend=robustTrend, robust=robustTrend);
    rest <- topTable(fit, number = Inf);
    
    if(contrast != "anova"){    
      colnames(rest)[1] <- myargs[[1]];
      grp.nms<-c(ref,contrast)
      
    }
    #for meta-anal
    if(internal){
        ### get results with no adjustment
        design <- model.matrix(formula(paste0("~ 0", paste0(" + ", analysis.var, collapse = ""))), data = covariates);
        colnames(design)[1:length(grp.nms)] <- grp.nms;
        myargs[["levels"]] <- design;
        contrast.matrix <- do.call(makeContrasts, myargs);
        fit <- lmFit(feature_table, design)
        fit <- contrasts.fit(fit, contrast.matrix);
        fit <- eBayes(fit, trend=robustTrend, robust=robustTrend);
        res.noadj <- topTable(fit, number = Inf);
        dataSet$res.noadj <- res.noadj;
    }

    dataSet$contrast.matrix <- contrast.matrix;
    dataSet$par1 <-  myargs[[1]];
    dataSet$grp.nms <- ifelse(any(grepl("(^[0-9]+).*", grp.nms)), paste0(analysis.var,"_",grp.nms),grp.nms);
  } else { 
    
    # build design matrix
    types <- dataSet$cont.inx[vars];
    contIdx <- as.numeric(which(types))
    covariates[,contIdx] <- unlist(lapply(covariates[,contIdx], function(x) as.numeric(as.character((x)))));
    
    if (all(types)) {
      design <- model.matrix(formula(paste0("~", paste0(" + ", vars, collapse = ""))), data = covariates);
    } else {
      design <- model.matrix(formula(paste0("~ 0", paste0(" + ", vars, collapse = ""))), data = covariates);
    }
    
    # handle blocking factor
    if (is.null(blocking.factor) | is.na(blocking.factor) | blocking.factor=="NA") {
      fit <- lmFit(feature_table, design);
    } else {
      block.vec <- covariates[, blocking.factor];
      corfit <- duplicateCorrelation(feature_table, design, block = block.vec);
      fit <- lmFit(feature_table, design, block = block.vec, correlation = corfit$consensus);
    }
    
    # get results
    fit <- eBayes(fit, trend=robustTrend, robust=robustTrend);
    rest <- topTable(fit, number = Inf, coef = analysis.var);
    colnames(rest)[1] <- dataSet$par1 <- analysis.var;
    
    ### get results with no adjustment
    if(internal){
    design <- model.matrix(formula(paste0("~", analysis.var)), data = covariates);
    fit <- eBayes(lmFit(feature_table, design), trend=robustTrend, robust=robustTrend);
    res.noadj <- topTable(fit, number = Inf);
    dataSet$res.noadj <- res.noadj;
    }
  }
  
  dataSet$design <- design;
  dataSet$contrast.type <- analysis.type;
  dataSet$comp.res <- rest;
  dataSet$de.method <- "limma"
  dataSet$comp.type <- "default"
  dataSet$fit.obj <- fit;

  dataSet$pval <- 0.05;
  dataSet$fc.val <- 1;
  dataSet$analysis.var <- analysis.var;
  dataSet$de.adj <- adj.factors;

  if(all(adj.factors != "NA")){
    dataSet$de.adj <- "NA"
  }else{
    dataSet$de.adj <- adj.factors;
  }

  RegisterData(dataSet);
  return(dataSet);
}
