my.impute.missing <- function(mSetObj = NA,
                              method      = "min",
                              grpLod      = FALSE,
                              grpMeasure  = FALSE) {

  mSetObj <- .get.mSet(mSetObj)

  int.mat <- qs::qread("preproc.qs")          # samples × variables
  new.mat <- NULL
  msg     <- mSetObj$msgSet$replace.msg

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

  ## ----------------------- imputation ----------------------- ##
  if (method == "exclude") {

    good.inx <- apply(is.na(int.mat), 2, sum) == 0
    new.mat  <- int.mat[, good.inx, drop = FALSE]
    msg <- c(msg, "Variables with missing values were excluded.")

  } else if (method == "min") {
    ## LoD replacement (1/5 of min positive value)
    if (grpLod) {
      new.mat <- ReplaceMissingByLoD.grp(int.mat, grp)
      msg <- c(msg,
               "Missing variables were replaced by group-specific LoDs ",
               "(1/5 of the group-specific min positive value).")
    } else {
      new.mat <- ReplaceMissingByLoD(int.mat)
      msg <- c(msg,
               "Missing variables were replaced by LoDs ",
               "(1/5 of the global min positive value).")
    }

  } else if (method == "colmin") {
    ## Half-min replacement
    colmin_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- min(x, na.rm = TRUE) / 2; x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, colmin_fun)
      msg <- c(msg,
               "Missing variables were replaced by 1/2 of the group-specific ",
               "min values for each feature column.")
    } else {
      new.mat <- colmin_fun(int.mat)
      msg <- c(msg,
               "Missing variables were replaced by 1/2 of the min value ",
               "for each feature column.")
    }

  } else if (method == "mean") {
    mean_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- mean(x, na.rm = TRUE); x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, mean_fun)
      msg <- c(msg,
               "Missing variables were replaced with the group-specific ",
               "mean for each feature column.")
    } else {
      new.mat <- mean_fun(int.mat)
      msg <- c(msg,
               "Missing variables were replaced with the mean value ",
               "for each feature column.")
    }

  } else if (method == "median") {
    median_fun <- function(m) {
      apply(m, 2, function(x) { if (anyNA(x)) x[is.na(x)] <- median(x, na.rm = TRUE); x })
    }
    if (grpMeasure) {
      new.mat <- group_apply(int.mat, median_fun)
      msg <- c(msg,
               "Missing variables were replaced with the group-specific ",
               "median for each feature column.")
    } else {
      new.mat <- median_fun(int.mat)
      msg <- c(msg,
               "Missing variables were replaced with the median ",
               "for each feature column.")
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
    }
    msg <- c(msg,
             paste("Missing variables were imputed using",
                   toupper(gsub("_", "-", method))))
  }


  mSetObj$dataSet$proc.feat.num <- ncol(int.mat)
  qs::qsave(as.data.frame(new.mat), file = "data_proc.qs")
  print(msg);
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
