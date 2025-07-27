my.impute.missing <- function(mSetObj = NA,
                              method      = "lod",
                              grpLod      = FALSE,
                              grpMeasure  = FALSE) {
  #save.image("miss.RData");
  mSetObj <- .get.mSet(mSetObj)
  
  int.mat <- qs::qread("data.filt.qs");          # samples × variables
  new.mat <- NULL
  msg     <- "";
  
  if (!is.null(mSetObj$dataSet$meta.info) &&
      ncol(mSetObj$dataSet$meta.info) >= 1) {
    grp <- as.character(mSetObj$dataSet$meta.info[, 1])
  } else {
    grp <- as.character(mSetObj$dataSet$cls)
  }
  
  if (length(grp) != nrow(int.mat))
    stop("Group vector length mismatch with data!")
  
  uniq.grp <- unique(grp)
  
  ## Convenience: apply a column-wise FUN within each group
  group_apply <- function(mat, FUN) {
    out <- mat
    for (g in uniq.grp) {
      idx <- grp == g
      sub <- mat[idx, , drop = FALSE]
      out[idx, ] <- FUN(sub)
    }
    out
  }
  
  if (method == "exclude") {
    good.inx <- apply(is.na(int.mat), 2, sum) == 0
    new.mat  <- int.mat[, good.inx, drop = FALSE]
    msg <- "Variables with missing values were excluded.";
    
  } else if (method == "lod") {
    ## LoD replacement (1/5 of min positive value)
    if (grpLod) {
      new.mat <- ReplaceMissingByLoD.grp(int.mat, grp)
      msg <- "Missing variables were replaced by group-specific LoDs (1/5 of the min positive values in its specific group for each feature).";
    } else {
      new.mat <- ReplaceMissingByLoD(int.mat)
      msg <- "Missing variables were replaced by LoDs (1/5 of the min positive value for each feature).";
    }
    
  }else if (tolower(method) == "qrilc") {
    require("imputeLCMD")  # Quantile-Regression Imputation of Left-Censored data
    set.seed(12345)
    ## ---- 1 · log transform -------------------------------------------------
    # Offset = half of the smallest non-zero value in the entire matrix
    min.pos <- min(int.mat[int.mat > 0], na.rm = TRUE)
    offset  <- 0.5 * min.pos
    log.base <- 2  # keep log2 unless you have a specific preference
    
    log.mat <- log(int.mat + offset, base = log.base)   # samples × variables
    
    ## ---- 2 · QRILC on log data --------------------------------------------
    qr.res  <- imputeLCMD::impute.QRILC(t(log.mat))     # expects vars × samples
    
    imp.mat <- qr.res[[1]]
    if (is.data.frame(imp.mat))
      imp.mat <- as.matrix(imp.mat)
    
    ## ---- 3 · back-transform to raw scale -----------------------------------
    log.imp <- t(imp.mat)                              # samples × variables (log scale)
    new.mat <- (log.base ^ log.imp) - offset           # back to original intensity scale

    rownames(new.mat) <- rownames(int.mat)
    colnames(new.mat) <- colnames(int.mat)
    msg <- "Missing values were imputed with QRILC";

  } else if (method == "colmin") {
    ## Half-min replacement
    colmin_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- min(x, na.rm = TRUE) / 2; x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, colmin_fun)
      msg <- "Missing variables were replaced by 1/2 of the group-specific min values for each feature column."
    } else {
      new.mat <- colmin_fun(int.mat)
      msg <- "Missing variables were replaced by 1/2 of the min value for each feature column.";
    }
    
  } else if (method == "mean") {
    mean_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- mean(x, na.rm = TRUE); x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, mean_fun)
      msg <- "Missing variables were replaced with the group-specific mean for each feature column.";
    } else {
      new.mat <- mean_fun(int.mat)
      msg <- "Missing variables were replaced with the mean value for each feature column.";
    }
    
  } else if (method == "median") {
    median_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- median(x, na.rm = TRUE); x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, median_fun)
      msg <- "Missing variables were replaced with the group-specific median for each feature column.";
    } else {
      new.mat <- median_fun(int.mat)
      msg <- "Missing variables were replaced with the median for each feature column.";
    }
    
  } else {
    ## Multivariate / model-based methods (unchanged)
    if (method == "knn_var") {
      new.mat <- t(impute::impute.knn(t(int.mat))$data)
    } else if (method == "knn_smp") {
      new.mat <- impute::impute.knn(data.matrix(int.mat))$data
    } else if (method == "bpca") {
      new.mat <- pcaMethods::pca(int.mat, nPcs = 5, method = "bpca",
                                 center = TRUE)@completeObs
    } else if (method == "ppca") {
      new.mat <- pcaMethods::pca(int.mat, nPcs = 5, method = "ppca",
                                 center = TRUE)@completeObs
    } else if (method == "svdImpute") {
      new.mat <- pcaMethods::pca(int.mat, nPcs = 5, method = "svdImpute",
                                 center = TRUE)@completeObs
    } else if (method == "missForest") {## ---- missRanger imputation (fast RF) ----
      require("missRanger")     # <- fast C++/ranger backend
      set.seed(123)
      
      df <- as.data.frame(int.mat, stringsAsFactors = FALSE)
      
      check <- checkMissingBudget(df);
      if(check == 0){
        return(0);
      }
      
      num.idx   <- vapply(df, is.numeric, logical(1))
      char.idx  <- vapply(df, is.character, logical(1))
      factor.idx<- vapply(df, is.factor,  logical(1))
      
      df[char.idx] <- lapply(df[char.idx], factor)    
      # keep numeric as is; keep factors untouched
      
      result <- missRanger::missRanger(           # returns a list
        data        = df,
        num.trees   = 10,        # ≈ missForest ntree; lower is faster
        pmm.k       = 0,         # disable PMM for speed (set >0 for PMM)
        maxiter     = 4,         # ≈ missForest default 10
        verbose     = TRUE,     # prints iter-wise OOB errors
        data_only = F
      )
      
      new.mat <- as.matrix(result$data)           # imputed matrix
      
      iters <- result$iterations
      oob <- result$mean_pred_errors   # scalar or named vector
      nrms  <- if (!is.null(names(oob))) oob["NRMSE"] else oob

      msg <- paste0(
        "Missing values were imputed with missRanger (",
        result$best_iter, " iteration", ifelse(result$best_iter == 1, "", "s"),
        ", OOB NRMSE = ", signif(nrms, 4), "). OOB NRMSE refers to Out-of-Bag Normalized Root-Mean-Squared Error.");
    }
    if(method !="missForest"){
      msg <- paste("Missing variables were imputed using", toupper(gsub("_", "-", method)));
      
    }
  }
  

  mSetObj$dataSet$proc.feat.num <- ncol(int.mat)
  qs::qsave(as.data.frame(new.mat), file = "data_proc.qs")
  mSetObj$dataSet$filt <- NULL;
  mSetObj$msgSet$replace.msg <- msg
  
  return(.set.mSet(mSetObj))
}

# ------------------------------------------------------------------
# ReplaceMissingByLoD.grp()
#   • int.mat : samples × variables numeric matrix
#   • grp     : grouping factor or character vector (length = nrow(int.mat))
#   • fallback: numeric scalar when an entire group–feature slice has
#               no finite positive values   (default = 1e-6)
# ------------------------------------------------------------------
ReplaceMissingByLoD.grp <- function(int.mat, grp, fallback = 1e-6) {
  
  int.mat <- as.matrix(int.mat)                    # ensure matrix
  if (length(grp) != nrow(int.mat))
    stop("Length of 'grp' must equal nrow(int.mat)")
  
  grp      <- as.character(grp)
  uniq.grp <- unique(grp)
  rowNms   <- rownames(int.mat)
  colNms   <- colnames(int.mat)
  out      <- int.mat                              # result container
  
  # vector helper – never returns Inf/NaN
  safe_lod <- function(x) {
    pos <- x[is.finite(x) & x > 0]
    lod <- if (length(pos) == 0) fallback else min(pos) / 5
    x[is.na(x) | x == 0 | !is.finite(x)] <- lod
    x
  }
  
  # loop over groups, apply column-wise
  for (g in uniq.grp) {
    idx         <- grp == g
    sub         <- int.mat[idx, , drop = FALSE]
    out[idx, ]  <- apply(sub, 2, safe_lod)
  }
  
  # restore dimnames and return
  rownames(out) <- rowNms
  colnames(out) <- colNms
  out
}
checkMissingBudget <- function(mat,
                               maxNAcells = 30000,
                               maxNAcols  = 60) {
  dim(mat);
  na.cells <- sum(is.na(mat))
  na.cols  <- sum(apply(mat, 2, function(x) any(is.na(x))))   # <— fixed
  
  if (na.cells > maxNAcells) {
    msg.vec <<- c(
      msg.vec,
      sprintf(
        "Too many missing values for missForest imputation: limit is %d NA values.",
        maxNAcells
      )
    )

    return(0)
  } else {
    return(1)
  }
}
