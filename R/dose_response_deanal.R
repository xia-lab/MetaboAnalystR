###########################################
# Perform Differential Analysis
# ANOVA-like contrast, the null hypothesis would be that there is no DE between any of the conditions. 
# NO need to manually specify all pairwise comparisons between conditions. Just use one "reference" condition (zero dose)
# Jeff Xia (jeff.xia@xialab.ca)
###########################################


PerformDoseDEAnal <- function(mSetObj = NA, meta1="NA") {
  #save.image("PerformDoseDEAnal.RData");
  # Determine which covariates (if any) to adjust
  if (!exists("adj.vec") || length(adj.vec) == 0) {
    covariates <- c();
  } else {
    covariates <- adj.vec
  }

  # Get everything from mSetObj
  mSetObj      <- .get.mSet(mSetObj)
  expr_matrix  <- t(mSetObj$dataSet$norm)
  if(meta1 == "NA"){
  cls          <- mSetObj$dataSet$cls
  }else{
  cls          <- mSetObj$dataSet$meta.info[,meta1]
  }
  meta         <- mSetObj$dataSet$meta.info
  meta.types   <- mSetObj$dataSet$meta.types  # named vector: "cont" or "disc"
  
  # Continuous‐dose branch
  if (mSetObj$dataSet$cls.type == "cont") {
    require(limma)
    require(Hmisc)
    
    # 1) Build the basic metadata with your continuous variable
    metadata <- data.frame(
      Sample = colnames(expr_matrix),
      var    = as.numeric(as.character(cls)),
      row.names = colnames(expr_matrix),
      stringsAsFactors = FALSE
    )
    
    # 2) If you have additional covariates, pull them in and
    #    convert only the "cont" ones to numeric
    if (length(covariates) > 0) {
      covar.data <- meta[colnames(expr_matrix), covariates, drop = FALSE]
      covar.data <- as.data.frame(mapply(
        function(col, nm) {
          if (meta.types[nm] == "cont") {
            as.numeric(as.character(col))
          } else {
            factor(col, levels = unique(col))
          }
        },
        covar.data, names(covar.data),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
      ))
      metadata <- cbind(metadata, covar.data)
    }
    
    # 3) Build your design
    if (length(covariates) > 0) {
      fmla  <- as.formula(paste("~ var +", paste(covariates, collapse = " + ")))
    } else {
      fmla  <- ~ var
    }
    design <- model.matrix(fmla, data = metadata)
    
    # 4) Fit the model
    fit <- lmFit(expr_matrix, design)
    if (!"var" %in% colnames(fit$coefficients)) {
      warning("Variable 'var' not estimable (collinearity or insufficient data).")
      return(0)
    }
    fit      <- eBayes(fit)
    resTable <- topTable(fit, coef = "var", number = Inf, adjust.method = "BH")
    
    # 5) Spearman correlation
    cor_res <- apply(expr_matrix, 1, function(x) {
      rcorr(x, metadata$var, type = "spearman")$r[1,2]
    })
    qs::qsave(cor_res, file = "limma_cor_res.qs")
    qs::qsave(fit,     file = "limma_fit_res.qs")
    
    print(head(resTable))
    
  } else {
    # Categorical‐dose branch (unchanged)
    grp.nms      <- paste0("grp_", levels(cls))
    levels(cls)  <- grp.nms
    require(limma)
    
    base_design <- model.matrix(~ 0 + cls)
    colnames(base_design) <- grp.nms
    rownames(base_design) <- names(cls)
    
    if (length(covariates) > 0) {
      covar.data  <- meta[rownames(base_design), covariates, drop = FALSE]
      covar.data  <- as.data.frame(lapply(covar.data, function(x) as.numeric(as.character(x))))
      covar_design <- model.matrix(~ ., data = covar.data)[, -1, drop = FALSE]
      design       <- cbind(base_design, covar_design)
    } else {
      design <- base_design
    }
    
    ref            <- grp.nms[1]
    contrasts      <- grp.nms[grp.nms != ref]
    contrast_args  <- setNames(as.list(paste0(contrasts, "-", ref)), contrasts)
    contrast_args$levels <- design
    contrast.matrix <- do.call(makeContrasts, contrast_args)
    
    fit <- lmFit(expr_matrix, design)
    if (all(fit$df.residual == 0)) {
      current.msg <<- "No residual degrees of freedom (insufficient replicates)!"
      print(current.msg)
      return(0)
    }
    fit      <- contrasts.fit(fit, contrast.matrix)
    fit      <- eBayes(fit)
    resTable <- topTable(fit, number = Inf, adjust.method = "BH")
  }
  
  # Save and return
  fast.write.csv(resTable, file = "dose_response_limma_all.csv", row.names = TRUE)
  qs::qsave(resTable, "limma.sig.qs")
  return(.set.mSet(mSetObj))
}





# get result based on threshold (p.value and average FC) for categorical dose
ComputeDoseLimmaResTable<-function(mSetObj=NA, p.thresh=0.05, fc.thresh=0, fdr.bool=T){

    mSetObj <- .get.mSet(mSetObj); 
    res.all <- qs::qread("limma.sig.qs");

    fdr.bool <- as.logical(fdr.bool);

    colNum <- ncol(res.all);
    if(fdr.bool){
        p.value <- res.all$adj.P.Val;
    }else{
        p.value <- res.all$P.Value;
    }

    fc.mat <- res.all[,1:(colNum-4)];
    ave.fc <- apply(fc.mat, 1, mean);

    names(p.value) <- names(ave.fc) <- rownames(res.all);

    inx.unsig <- !(p.value <= p.thresh & abs(ave.fc) >= fc.thresh);
    inx.up <- p.value <= p.thresh & ave.fc >= fc.thresh;
    inx.down <- p.value <= p.thresh & ave.fc <= -fc.thresh;

    sig.count <- sum(!inx.unsig);

    if(sig.count > 0){

        res.all2 <- cbind(res.all,  "AveFC"=ave.fc);
        hit.inx <- which(!inx.unsig);
        sig.res <- signif(res.all2[hit.inx, , drop=F], 5);        
        fast.write.csv(sig.res, file="limma_sig_features.csv");
 
        # only store avarage FC + last 5 columns from the general information from limma - AveExpr, F, P.Value, adj.P.Val, AveFC 
        sig.mat <- sig.res[,(colNum-4):colNum, drop=FALSE];

        de.genes <- rownames(sig.mat);
        non.sig.count <- nrow(res.all)-sig.count;
  
        # record the sig gene vec
        output <- c(1, sig.count, non.sig.count);
    }else{
        output <- c(1, 0, nrow(res.all));
        sig.mat <- as.matrix(cbind(-1, -1));
    }

    if(nrow(sig.mat) > 1000){
        sig.mat <- sig.mat[1:1000,];
    }

    mSetObj$analSet$de.info <- output;
    mSetObj$dataSet$comp.res = res.all; # 
    mSetObj$analSet$dose <- list(
                p.thresh = p.thresh,
                fc.thresh = fc.thresh,
                p.log = -log10(p.value),
                fc.log = ave.fc, 
                inx.unsig = inx.unsig,
                inx.up = inx.up,
                inx.down = inx.down,
                sig.mat = sig.mat
              );

    fast.write.csv(res.all, "limma_restable.csv");

    if(!.on.public.web){
        return(.set.mSet(mSetObj));
    }
    .set.mSet(mSetObj);
    return(sig.count);
}


ComputeContDoseLimmaResTable<-function(mSetObj=NA, p.thresh=0.05, coef.thresh=0.25, fdr.bool=T){
  
  mSetObj <- .get.mSet(mSetObj); 
  res.all <- qs::qread("limma.sig.qs");
  fit_res <- qs::qread("limma_fit_res.qs")
  cor_res <- qs::qread(file = "limma_cor_res.qs")
  save(mSetObj, file = "mSetObj___ComputeContDoseLimmaResTable.rda")
  fdr.bool <- as.logical(fdr.bool);
  
  colNum <- ncol(res.all);
  if(fdr.bool){
    p.value <- res.all$adj.P.Val;
  }else{
    p.value <- res.all$P.Value;
  }
  
  coef_res <- cor_res #fit_res[["coefficients"]][,2]
  
  #fc.mat <- res.all[,1:(colNum-4)];
  #ave.fc <- apply(fc.mat, 1, mean);
  
  names(p.value) <- names(coef_res) <- rownames(res.all);
  
  inx.unsig <- !(p.value <= p.thresh & abs(coef_res) >= coef.thresh);
  inx.up <- p.value <= p.thresh & coef_res >= coef.thresh;
  inx.down <- p.value <= p.thresh & coef_res <= -coef.thresh;
  
  sig.count <- sum(!inx.unsig);
  
  if(sig.count > 0){
    
    res.all2 <- cbind(res.all,  "Coefficient"=coef_res);
    hit.inx <- which(!inx.unsig);
    sig.res <- signif(res.all2[hit.inx, , drop=F], 5);
    mSetObj[["dataSet"]][["limma_dose_sig_res"]] <- sig.res;
    fast.write.csv(sig.res, file="limma_sig_features.csv");
    
    # only store avarage FC + last 5 columns from the general information from limma - AveExpr, F, P.Value, adj.P.Val, AveFC 
    sig.mat <- sig.res
    
    de.genes <- rownames(sig.mat);
    non.sig.count <- nrow(res.all)-sig.count;
    
    # record the sig gene vec
    output <- c(1, sig.count, non.sig.count);
  }else{
    output <- c(1, 0, nrow(res.all));
    sig.mat <- as.matrix(cbind(-1, -1));
  }
  
  if(nrow(sig.mat) > 1000){
    sig.mat <- sig.mat[1:1000,];
  }

  idx_rm <- which(colnames(res.all) == "logFC" | colnames(res.all) == "B")
  res.all_clean <- res.all[,-idx_rm]
  res.all_clean <- cbind(res.all_clean, Coefficient = coef_res)
  
  mSetObj$analSet$de.info <- output;
  mSetObj$dataSet$comp.res <- res.all_clean; # 
  mSetObj$analSet$dose <- list(
    p.thresh = p.thresh,
    coef.thresh = coef.thresh,
    p.log = -log10(p.value),
    fc.log = coef_res, 
    inx.unsig = inx.unsig,
    inx.up = inx.up,
    inx.down = inx.down,
    sig.mat = sig.mat
  );
  
  fast.write.csv(res.all, "limma_restable.csv");
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj));
  }
  .set.mSet(mSetObj);
  return(sig.count);
}


#'Sig Table for Anova
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Dose <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$dose$sig.mat, "Limma result for dose response analysis", mSetObj$dataSet$type);
}

GetDoseUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$dose$p.log;
  fc <- mSetObj$analSet$dose$fc.log;
  inx<- which(mSetObj$analSet$dose$inx.up);
  if(sum(inx) > 0){
    return(as.matrix(cbind(fc[inx], lod[inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetDoseUpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  inx<- which(mSetObj$analSet$dose$inx.up);
  if(sum(inx) > 0){
    return(names(mSetObj$analSet$dose$p.log)[inx]);
  }else{
    return("NA");
  }
}

GetDoseDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  #save.image("TestD.RData");
  lod <- mSetObj$analSet$dose$p.log;
  fc <- mSetObj$analSet$dose$fc.log;
  if(is.null(mSetObj$analSet$dose$inx.down)){return(NULL)}
  inx <- which(mSetObj$analSet$dose$inx.down);
  if(sum(inx) > 0){
    return(as.matrix(cbind(fc[inx], lod[inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetDoseDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  inx<- which(mSetObj$analSet$dose$inx.down);
  if(sum(inx) > 0){
    return(names(mSetObj$analSet$dose$p.log)[inx]);
  }else{
    return("NA");
  }
}

GetDoseUnsigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$dose$p.log;
  fc <- mSetObj$analSet$dose$fc.log;

  inx <- which(mSetObj$analSet$dose$inx.unsig);
  if(sum(inx) > 0){
    return(as.matrix(cbind(fc[inx], lod[inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetDoseUnsigIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  inx<- which(mSetObj$analSet$dose$inx.unsig);
  if(sum(inx) > 0){
    return(names(mSetObj$analSet$dose$p.log)[inx]);
  }else{
    return("NA");
  }
}

GetDoseDEMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  #print(head(mSetObj$dataSet$comp.res));
  return(as.matrix(signif(mSetObj$dataSet$comp.res),4));
}

GetDoseDERows <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$dataSet$comp.res));
}

GetDoseDEColumns <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$comp.res));
}

SetDoseType <- function(mSetObj=NA, type = "disc"){
    mSetObj <- .get.mSet(mSetObj);
    #save(mSetObj, file = "mSetObj____SetDoseType.rda")
    #disc

    return(.set.mSet(mSetObj));
}