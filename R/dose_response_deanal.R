#' Perform Differential Analysis
# ANOVA-like contrast, the null hypothesis would be that there is no DE between any of the conditions. 
# NO need to manually specify all pairwise comparisons between conditions. Just use one "reference" condition (zero dose)

PerformDoseDEAnal<-function(mSetObj=NA){

  # check if already performed. No parameter required
  if(file.exists("limma.sig.qs")){
    return(1);
  }

  mSetObj <- .get.mSet(mSetObj);  
  data <- t(mSetObj$dataSet$norm);
  cls <- mSetObj$dataSet$cls;

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

  fast.write.csv(resTable, file="dose_response_limma_all.csv", row.names=TRUE);

  # save a qs file for update
  qs::qsave(resTable, "limma.sig.qs");

  return(.set.mSet(mSetObj));
}


# get result based on threshold (p.value and average FC
ComputeDoseLimmaResTable<-function(mSetObj=NA, p.thresh=0.05, fc.thresh=0){

    mSetObj <- .get.mSet(mSetObj); 
    res.all <- qs::qread("limma.sig.qs");

    colNum <- ncol(res.all);
    p.value <- res.all$adj.P.Val;

    fc.mat <- res.all[,1:(colNum-4)];
    ave.fc <- apply(fc.mat, 1, mean);

    names(p.value) <- names(ave.fc) <- rownames(res.all);

    inx.imp <- p.value <= p.thresh & abs(ave.fc) >= fc.thresh;
    sig.count <- sum(inx.imp);

    if(sig.count > 0){

        res.all2 <- cbind(res.all,  "AveFC"=ave.fc);
        
        hit.inx <- which(inx.imp);
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
                inx.imp = inx.imp,
                sig.mat = sig.mat
              );
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
  red.inx<- which(mSetObj$analSet$dose$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetDoseUpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<- which(mSetObj$analSet$dose$inx.imp);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$dose$p.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetDoseDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$dose$p.log;
  blue.inx <- which(!mSetObj$analSet$dose$inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetDoseDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<- which(!mSetObj$analSet$dose$inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$dose$p.log)[blue.inx]);
  }else{
    return("NA");
  }
}