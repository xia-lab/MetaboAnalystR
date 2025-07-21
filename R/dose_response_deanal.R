###########################################
# Perform Differential Analysis
# ANOVA-like contrast, the null hypothesis would be that there is no DE between any of the conditions. 
# NO need to manually specify all pairwise comparisons between conditions. Just use one "reference" condition (zero dose)
# Jeff Xia (jeff.xia@xialab.ca)
###########################################
PerformDoseDEAnal <- function(mSetObj = NA, meta1 = "NA") {
  # save.image('dose.RData');
  # Determine covariates
  if (!exists("adj.vec") || length(adj.vec) == 0) {
    covariates <- character(0)
  } else {
    covariates <- adj.vec
  }
  
  mSetObj     <- .get.mSet(mSetObj)
  expr_matrix <- t(mSetObj$dataSet$norm)
  meta        <- mSetObj$dataSet$meta.info
  meta.types  <- mSetObj$dataSet$meta.types
  
  # pick main variable
  if (meta1 == "NA" || meta1 == "") {
    main.var <- colnames(meta)[1]
  } else {
    main.var <- meta1
  }
  metadata <- meta[colnames(expr_matrix), , drop = FALSE]
  
  # determine continuous vs categorical
  cls.type <- unname(meta.types[main.var])
  require(limma)
  require(Hmisc)
  
  # coerce each column to appropriate type
  for (nm in colnames(metadata)) {
    if (meta.types[nm] == "cont") {
      metadata[, nm] <- as.numeric(as.character(metadata[, nm]))
    } else {
      metadata[, nm] <- factor(metadata[, nm], levels = unique(metadata[, nm]))
    }
  }
  
  # keep only main.var + covariates
  all.vars <- unique(c(main.var, covariates))
  metadata <- metadata[, all.vars, drop = FALSE]
  
  if (cls.type == "cont") {

  tmp_main <- suppressWarnings(as.numeric(as.character(metadata[[main.var]])))
  if (any(is.na(tmp_main))) {
    AddErrMsg(paste("Main variable", main.var,
                    "must be numeric for dose-response analysis."))
    return(0)
  }

  ## -------- Continuous branch ------------------------------------------------
  fmla   <- as.formula(paste("~ 0 +", paste(all.vars, collapse = " + ")))
  design <- model.matrix(fmla, data = metadata)

  fit <- lmFit(expr_matrix, design)
  if (!main.var %in% colnames(fit$coefficients)) {
    warning(paste("Variable", main.var, "not estimable.")); return(0)
  }
  fit      <- eBayes(fit)
  resTable <- topTable(fit, coef = main.var, number = Inf,
                       adjust.method = "BH")

  ## -------- Effect-size vector for pre-filtering -----------------------------
  ## β*  = change (in SDs of the feature) per SD change in dose
  beta_std <- apply(expr_matrix, 1, function(x) {
    coef(lm(scale(x) ~ scale(tmp_main)))[2]
  })

  qs::qsave(beta_std, file = "limma_cor_res.qs")   # replaces cor_res
  qs::qsave(fit,      file = "limma_fit_res.qs")
  }
  
  # write results
  fast.write.csv(resTable, file = "dose_response_limma_all.csv", row.names = TRUE)
  qs::qsave(resTable, "limma.sig.qs")
  mSetObj$dataSet$cls <- metadata[[main.var]]
  mSetObj$dataSet$main.var <- main.var;
  print(mSetObj$dataSet$cls)
  return(.set.mSet(mSetObj))
}


GetUpdatedClsType <- function(){
    mSetObj <- .get.mSet(mSetObj); 
    return(mSetObj$dataSet$cls.type);
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
########################not average value anymore, use the max value.
    ave.fc <- apply(fc.mat, 1, function(x) x[which.max(abs(x))]) 

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

PlotDoseVolcano <- function(mSetObj = NA,
                            imgNm,
                            dpi    = default.dpi,
                            format = "png") {

  mSetObj <- .get.mSet(mSetObj)
  doseRes <- mSetObj$analSet$dose

  if (is.null(doseRes))
    stop("analSet$dose is missing – run the LIMMA-result function first.")

doseRes$fc.thresh <- if (is.null(doseRes$fc.thresh) ||
                         length(doseRes$fc.thresh) == 0) 0
                     else as.numeric(doseRes$fc.thresh)

  df <- data.frame(
    logFC   = doseRes$fc.log,
    negLogP = doseRes$p.log,
    category = ifelse(doseRes$inx.up,   "Sig_Up",
               ifelse(doseRes$inx.down, "Sig_Down", "Unsig.")),
    stringsAsFactors = FALSE
  )

  ## ---------- 1. Legend counts & colours ---------------------------------
  counts  <- table(df$category)[c("Sig_Down","Sig_Up","Unsig.")]        # fixed order
  labels  <- paste0(c("Sig_Down","Sig_Up","Unsig."), " [", counts, "]")
  colours <- c("Sig_Down" = "#0080ff", "Sig_Up" = "#ff3333", "Unsig." = "#c0c0c0")

  ## ---------- 2. Build plot ------------------------------------------------
  cls.type <- mSetObj$dataSet$cls.type   # "cont" or "disc"
  xlab     <- ifelse(cls.type == "cont", "log10(FC)", "AveFC (max |Δ|)")

  require(ggplot2)
  p <- ggplot(df, aes(x = logFC, y = negLogP, colour = category)) +
       geom_point(alpha = 0.9, size = 1.5) +
       geom_hline(yintercept = -log10(doseRes$p.thresh),  linetype = "dashed") +
       geom_vline(xintercept = c(-doseRes$fc.thresh, doseRes$fc.thresh),
                  linetype = "dashed") +
       scale_colour_manual(values = colours,
                           breaks = names(colours),
                           labels = labels,
                           name   = NULL) +
       labs(x = xlab, y = "-log10(P)") +
       theme_bw(base_size = 11) +
       theme(legend.position = "top",
             axis.title      = element_text(face = "bold"),
             axis.text       = element_text(colour = "black"))

  ## ---------- 3. Cairo output ---------------------------------------------
  outFile <- paste0(imgNm, "_dpi", dpi, ".", format)
  Cairo::Cairo(file   = outFile,
               width  = 8, height = 6,
               unit   = "in", dpi = dpi,
               type   = format, bg  = "white")
  print(p)
  dev.off()

  ## ---------- 4. Book-keeping ---------------------------------------------
  mSetObj$imgSet$dose_volcano_filename <- outFile
  return(.set.mSet(mSetObj))
}
