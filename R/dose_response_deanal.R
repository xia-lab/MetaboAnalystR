###########################################
# Perform Differential Analysis
# ANOVA-like contrast, the null hypothesis would be that there is no DE between any of the conditions. 
# NO need to manually specify all pairwise comparisons between conditions. Just use one "reference" condition (zero dose)
# Jeff Xia (jeff.xia@xialab.ca)
###########################################

PerformDoseDEAnal<-function(mSetObj=NA){

  # check if already performed. No parameter required
  # remove this file if performs normalization again
  if(file.exists("limma.sig.qs")){
    return(1);
  }

  mSetObj <- .get.mSet(mSetObj);  
  data <- t(mSetObj$dataSet$norm);
  cls <- mSetObj$dataSet$cls;

  
  if(mSetObj[["dataSet"]][["cls.type"]] == "cont"){
    # this is for contineous variable/dose
    require(limma);
    expr_matrix <- data
    # create metadata with continuous variable (e.g., var)
    metadata <- data.frame(
      Sample = colnames(expr_matrix),
      var = as.numeric(as.character(cls))  # Continuous variable
    )
    
    # Create design matrix for linear regression (include intercept)
    design <- model.matrix(~ var, data = metadata)
    
    # Fit linear model using limma
    fit <- lmFit(expr_matrix, design)
    
    # Apply empirical Bayes moderation
    fit <- eBayes(fit)
    
    # Get results for the continuous variable (var)
    resTable <- topTable(fit, coef = "var", number = Inf, adjust.method="BH")
  
    # calculate correlation coefficients
    require(Hmisc);
    cor_res <- apply(expr_matrix, 1, function(x) {
      rs1 <- rcorr(x, metadata$var, "spearman")
      return(rs1[["r"]][1,2])
    })
    qs::qsave(cor_res, file = "limma_cor_res.qs")
    qs::qsave(fit, file = "limma_fit_res.qs")    
    
  } else {
    # this is for categorical variable/dose
    # note, use numeric will lead to error in limma
    grp.nms <- paste0("grp_", levels(cls));
    levels(cls) <- grp.nms;
    
    require(limma);
    design <- model.matrix(formula(~ 0 + cls));
    colnames(design)[1:length(grp.nms)] <- grp.nms;
    
    myargs <- list();
    ref <- levels(cls)[1]; # reference dose
    contrasts <- grp.nms[grp.nms != ref];
    myargs <- as.list(paste(contrasts, "-", ref, sep = "")); 
    myargs[["levels"]] <- design;
    
    contrast.matrix <- do.call(makeContrasts, myargs);
    fit <- lmFit(data, design);
    
    #sanity check
    df.residual <- fit$df.residual;
    if (all(df.residual == 0)) {
      current.msg <<- "All residuals equal 0. There is not enough replicates in each group (no residual degrees of freedom)!";  
      print(current.msg);
      return(0);
    }
    
    fit <- contrasts.fit(fit, contrast.matrix)
    fit <- eBayes(fit);
    resTable <- topTable(fit, number=Inf, adjust.method="BH");
    
  }
  
  fast.write.csv(resTable, file="dose_response_limma_all.csv", row.names=TRUE);

  # save a qs file for update
  qs::qsave(resTable, "limma.sig.qs");

  return(.set.mSet(mSetObj));
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