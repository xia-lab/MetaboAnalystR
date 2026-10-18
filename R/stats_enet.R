##################################################
## R script for MetaboAnalyst
## Description: Elastic net classification / regression with feature selection
##
## One analysis for the single-factor statistics, multi-factor statistics and biomarker
## modules. The outcome is the class labels, or one metadata factor (multi-factor); a
## discrete outcome is classified (binomial for two groups, multinomial for more), a
## continuous one regressed (gaussian). The penalty mixing (alpha) and the number of
## cross-validation folds are chosen by the user; lambda comes from the cross-validation
## (1-SE rule by default, or the minimum error). Features with a non-zero coefficient at
## that lambda are the selected features.
##################################################

# ---- shared elastic-net helpers ----
# Used by the elastic net page of the statistics modules (below) and by the biomarker analysis
# (RankFeatures, Predict.class in biomarker_utils.R); this file is loaded by all of them, so there
# is one implementation. The defaults of .ov_enet_fit are the biomarker analysis' own (two groups,
# alpha 0.5, 3 to 5 folds); the page passes its own alpha, family, folds and error measure.

# Elastic-net fit that survives a small cross-validation fold.
#
# cv.glmnet needs enough members per class to hold folds out; inside MCCV the
# training subset is a fraction of the data, so it fails there and returned NULL.
# The caller then had no feature ranking, built no models, and the results page
# showed an empty "Best model (max AUC)". Fall back to a plain glmnet path and
# pick a mid-path lambda, which always yields coefficients.
# Returns list(fit, s) or NULL.
.ov_enet_fit <- function(x, y, alpha = 0.5, family = "binomial", foldid = NULL, type.measure = NULL, keep = FALSE) {
  if (!requireNamespace("glmnet", quietly = TRUE)) return(NULL);
  if (family != "gaussian") y <- as.factor(y);
  args <- list(x = x, y = y, family = family, alpha = alpha);
  if (is.null(foldid)) {
    args$nfolds <- max(3, min(5, min(table(y))));
  } else {
    args$foldid <- foldid;
  }
  if (!is.null(type.measure)) args$type.measure <- type.measure;
  if (keep) args$keep <- TRUE;
  fit <- try(do.call(glmnet::cv.glmnet, args), silent = TRUE);
  if (!inherits(fit, "try-error")) {
    return(list(fit = fit, s = "lambda.min"));
  }
  fit2 <- try(glmnet::glmnet(x, y, family = family, alpha = alpha), silent = TRUE);
  if (inherits(fit2, "try-error")) return(NULL);
  lam <- fit2$lambda[max(1L, ceiling(length(fit2$lambda) / 2L))];
  list(fit = fit2, s = lam);
}

# Tie-free elastic-net importance: total |beta| across the whole lambda path.
#
# |beta| at a SINGLE lambda is mostly exact zeros, and GetImpFeatureMat ranks each
# CV run with rank(-imp), which AVERAGES ties — so every zero-coefficient feature
# receives the same rank and "Selected Frequency" collapses to 1.0 for all of
# them. Used by BOTH RankFeatures and Predict.class: imp.cv (what the frequency
# plot actually ranks) is produced by Predict.class, so fixing only the former
# left the plot unchanged.
.ov_enet_imp <- function(x, y, feat.names) {
  fit <- try(glmnet::glmnet(as.matrix(x), as.factor(y), family = "binomial", alpha = 0.5), silent = TRUE);
  if (inherits(fit, "try-error")) {
    ef <- .ov_enet_fit(as.matrix(x), y);
    if (is.null(ef)) return(NULL);
    v <- abs(as.matrix(stats::coef(ef$fit, s = ef$s))[-1, 1]);
  } else {
    v <- rowSums(abs(as.matrix(stats::coef(fit))))[-1];
  }
  names(v) <- feat.names;
  v
}

# fixed seed: the cross-validation folds are the same every time for the same data, so
# the result is repeatable (the session's random stream is left as it was)
.enet_seed <- 20261003L

# normalised data matrix (samples x features) and the outcome: the class labels, or a
# metadata factor of the multi-factor module
.enet_get_data <- function(mSetObj, outcome) {
  x <- as.matrix(mSetObj$dataSet$norm)
  y <- mSetObj$dataSet$cls
  type <- "disc"
  label <- "Class"
  mi <- mSetObj$dataSet$meta.info
  use.meta <- !is.null(outcome) && length(outcome) == 1 && !is.na(outcome) && nzchar(outcome) &&
    !identical(outcome, "NA") && !is.null(mi) && outcome %in% colnames(mi)
  if (use.meta) {
    k <- which(colnames(mi) == outcome)[1]
    y <- mi[[k]][match(rownames(x), rownames(mi))]
    ty <- mSetObj$dataSet$meta.types[[k]]
    type <- if (identical(as.character(ty), "cont")) "cont" else "disc"
    label <- outcome
  }
  list(x = x, y = y, type = type, label = label, meta = use.meta)
}

# the samples with data and an outcome: x (samples x features) and y (a factor, or numeric when continuous)
.enet_xy <- function(mSetObj, outcome) {
  d <- .enet_get_data(mSetObj, outcome)
  ok <- stats::complete.cases(d$x) & !is.na(d$y)
  d$x <- d$x[ok, , drop = FALSE]
  d$y <- if (d$type == "disc") droplevels(as.factor(d$y[ok])) else as.numeric(d$y[ok])
  d
}

# non-zero coefficients at lambda (features x outcome columns) and their importance
.enet_coefs <- function(cv, lam, fam) {
  cf <- stats::coef(cv, s = lam)
  if (fam == "multinomial") {
    m <- vapply(cf, function(z) as.numeric(z[-1, 1]), numeric(nrow(cf[[1]]) - 1))
    if (is.null(dim(m))) m <- matrix(m, ncol = length(cf))
    dimnames(m) <- list(rownames(cf[[1]])[-1], names(cf))
    imp <- sqrt(rowSums(m^2))
  } else {
    m <- matrix(as.numeric(cf[-1, 1]), ncol = 1, dimnames = list(rownames(cf)[-1], "Coefficient"))
    imp <- abs(m[, 1])
  }
  keep <- imp > 0
  sig <- data.frame(m[keep, , drop = FALSE], Importance = imp[keep], check.names = FALSE)
  sig[order(-sig$Importance), , drop = FALSE]
}

# AUC of a score for two groups (rank statistic); the second level is the positive class
.enet_auc <- function(score, y) {
  pos <- y == levels(y)[2]
  n1 <- sum(pos); n0 <- sum(!pos)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(rank(score)[pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.enet_fit <- function(mSetObj, alpha, nfolds, outcome, rule) {
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("The glmnet package is not installed on this server.")
  alpha <- max(0, min(1, as.numeric(alpha)))
  d <- .enet_xy(mSetObj, outcome)
  x <- d$x; y <- d$y; type <- d$type
  if (ncol(x) < 2) stop("Elastic net needs at least two features.")

  if (type == "disc") {
    y <- droplevels(as.factor(y))
    if (nlevels(y) < 2) stop("The outcome has only one level.")
    tab <- table(y)
    if (min(tab) < 3) {
      stop(sprintf("Every group needs at least 3 samples (smallest: %s with %d).", names(tab)[which.min(tab)], min(tab)))
    }
    fam <- if (nlevels(y) == 2) "binomial" else "multinomial"
    tm <- "class"
  } else {
    y <- as.numeric(y)
    if (length(y) < 10) stop("At least 10 samples are needed for a continuous outcome.")
    if (stats::var(y) == 0) stop("The outcome has a single value.")
    fam <- "gaussian"
    tm <- "mse"
  }
  nfolds <- as.integer(max(3, min(round(nfolds), if (type == "disc") min(table(y)) else length(y))))

  # the same folds for the same data, whatever the session did before
  had.seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had.seed) old.seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had.seed) assign(".Random.seed", old.seed, envir = .GlobalEnv) else
            if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(.enet_seed)
  foldid <- integer(length(y))
  if (type == "disc") {   # every fold holds its share of every group
    for (g in levels(y)) {
      inx <- which(y == g)
      foldid[inx] <- sample(rep_len(seq_len(nfolds), length(inx)))
    }
  } else {
    foldid <- sample(rep_len(seq_len(nfolds), length(y)))
  }

  ef <- suppressWarnings(.ov_enet_fit(x, y, alpha = alpha, family = fam, foldid = foldid, type.measure = tm, keep = TRUE))
  if (is.null(ef) || !inherits(ef$fit, "cv.glmnet")) stop("The elastic net could not be fitted to these data.")
  cv <- ef$fit
  lam.min <- cv$lambda.min
  lam.1se <- cv$lambda.1se
  rule <- if (identical(rule, "min")) "min" else "1se"
  lam <- if (rule == "min") lam.min else lam.1se
  sig <- .enet_coefs(cv, lam, fam)
  note <- ""
  if (nrow(sig) == 0 && lam != lam.min) {
    lam <- lam.min
    rule <- "min"
    sig <- .enet_coefs(cv, lam, fam)
    note <- "No feature survived the 1-SE rule; the lambda of minimum cross-validation error is used instead."
  }
  if (nrow(sig) == 0) {
    note <- paste(note, "The cross-validated error is no better than a model without features, so no feature was selected.")
  }

  # performance of the cross-validated predictions at the chosen lambda
  i <- which.min(abs(cv$lambda - lam))
  m <- list(err = cv$cvm[i], sd = cv$cvsd[i], bacc = NA_real_, auc = NA_real_, r2 = NA_real_)
  if (type == "disc") {
    pv <- cv$fit.preval
    if (fam == "binomial") {
      sc <- as.numeric(pv[, i])
      pred <- factor(levels(y)[1 + (sc > 0)], levels = levels(y))
      m$auc <- .enet_auc(sc, y)
    } else {
      pm <- pv[, , i]
      pred <- factor(levels(y)[max.col(pm, ties.method = "first")], levels = levels(y))
    }
    conf <- table(y, pred)
    m$bacc <- mean(diag(conf) / rowSums(conf))
  } else {
    m$r2 <- 1 - cv$cvm[i] / mean((y - mean(y))^2)
  }
  cv$fit.preval <- NULL

  list(err = NULL, cv = cv, family = fam, alpha = alpha, nfolds = nfolds, rule = rule,
       lambda = lam, lambda.min = lam.min, lambda.1se = lam.1se, note = trimws(note), metrics = m, sigmat = sig,
       n = length(y), p = ncol(x), label = d$label, type = type, outcome = outcome, meta = d$meta,
       levels = if (type == "disc") levels(y) else NULL)
}

#'Elastic net classification and feature selection
#'@description Fits an elastic net (glmnet) to the normalised data and selects the
#'features with a non-zero coefficient at the cross-validated lambda
#'@param mSetObj Input name of the created mSet Object
#'@param alpha Mixing of the penalties: 1 is the lasso, 0 ridge, 0.5 the default elastic net
#'@param nfolds Number of cross-validation folds (reduced to the smallest group size)
#'@param outcome "NA" for the class labels, or the name of a metadata factor
#'@param rule "1se" (sparser model, default) or "min" (lowest cross-validation error)
#'@usage ElasticNet.Anal(mSetObj=NA, alpha=0.5, nfolds=10, outcome="NA", rule="1se")
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
ElasticNet.Anal <- function(mSetObj = NA, alpha = 0.5, nfolds = 10, outcome = "NA", rule = "1se") {
  mSetObj <- .get.mSet(mSetObj)
  res <- tryCatch(.enet_fit(mSetObj, alpha, nfolds, outcome, rule),
                  error = function(e) list(err = conditionMessage(e)))
  mSetObj$analSet$enet <- res
  if (is.null(res$err)) {
    fast.write.csv(signif(res$sigmat, 5), file = "enet_sigfeatures.csv")
  } else {
    unlink("enet_sigfeatures.csv")
  }
  return(.set.mSet(mSetObj))
}

.enet_img_size <- function(width, default.w) {
  if (is.na(width)) default.w else if (width == 0) default.w - 1 else width
}

# an empty figure with a message, so that a failed analysis does not leave a broken image
.enet_msg_plot <- function(msg) {
  plot.new()
  text(0.5, 0.5, msg, cex = 1.1, col = "#5B6470")
}

#'Plot the cross-validation curve of the elastic net
#'@description Cross-validation error against log(lambda), with the lambda of minimum error
#'and the 1-SE lambda marked; the top axis gives the number of selected features
#'@usage PlotEnet.CV(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA)
#'@export
PlotEnet.CV <- function(mSetObj = NA, imgName, format = "png", dpi = default.dpi, width = NA) {
  mSetObj <- .get.mSet(mSetObj)
  en <- mSetObj$analSet$enet
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep = "")
  w <- .enet_img_size(width, 8)
  h <- w * 0.72
  mSetObj$imgSet$enet.cv <- imgName
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  if (is.null(en) || !is.null(en$err)) {
    .enet_msg_plot("No elastic net result to show")
  } else {
    cv <- en$cv
    ll <- log(cv$lambda)
    ylab <- if (en$type == "disc") "Cross-validation misclassification error" else "Cross-validation mean-squared error"
    op <- par(mar = c(4.6, 4.8, 4.2, 1.5))
    plot(ll, cv$cvm, type = "n", xlab = expression(log(lambda)), ylab = ylab, ylim = range(cv$cvup, cv$cvlo),
         main = "", las = 1)
    segments(ll, cv$cvlo, ll, cv$cvup, col = "#B8C4D2")
    points(ll, cv$cvm, pch = 20, col = "#3A6186", cex = 0.8)
    abline(v = log(cv$lambda.min), lty = 2, col = "#21395C")
    abline(v = log(cv$lambda.1se), lty = 3, col = "#89253E", lwd = 1.5)
    ix <- unique(round(seq(1, length(ll), length.out = 8)))
    axis(3, at = ll[ix], labels = cv$nzero[ix], tick = TRUE, cex.axis = 0.85)
    mtext("Number of selected features", side = 3, line = 2.4, cex = 0.9)
    legend("topleft", bty = "n", cex = 0.85, lty = c(2, 3), col = c("#21395C", "#89253E"), lwd = c(1, 1.5),
           legend = c("lambda with minimum error", "largest lambda within 1 SE"))
    par(op)
  }
  dev.off()
  return(.set.mSet(mSetObj))
}

#'Plot the importance of the selected features
#'@description The importance figure of the Random Forest page: a dot chart of the selected features ranked by
#'the size of their coefficient (absolute value; the signed coefficients are in the details table), with the
#'class means of each feature as a coloured strip. For a metadata outcome of the multi-factor module the
#'multi-factor variants are used (discrete, continuous).
#'@usage PlotEnet.Imp(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA, feat.num=15)
#'@export
PlotEnet.Imp <- function(mSetObj = NA, imgName, format = "png", dpi = default.dpi, width = NA, feat.num = 15) {
  mSetObj <- .get.mSet(mSetObj)
  en <- mSetObj$analSet$enet
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep = "")
  w <- .enet_img_size(width, 8)
  mSetObj$imgSet$enet.imp <- imgName
  sig <- if (!is.null(en) && is.null(en$err)) en$sigmat else NULL
  n <- if (is.null(sig)) 0 else min(feat.num, nrow(sig))
  imp <- if (n > 0) stats::setNames(sig$Importance[seq_len(n)], rownames(sig)[seq_len(n)]) else NULL
  h <- w - 1
  if (n >= 2 && identical(en$meta, FALSE) && en$type == "disc") {
    rt.mrg <- nlevels(mSetObj$dataSet$cls) + 3   # as the Random Forest figure: room for the class strip
    w <- w + rt.mrg / 72
  } else if (n >= 2) {
    h <- max(5, n * 0.35 + 2)
  }
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  if (n == 0) {
    .enet_msg_plot(if (is.null(en)) "No elastic net result to show" else "No feature was selected")
  } else if (n < 2) {
    dotchart(imp, bg = "#585855", xlab = "Importance", cex = 1.3)   # a single feature has no class strip to draw
  } else if (identical(en$meta, FALSE) && en$type == "disc") {
    op <- par(mar = c(5, 7, 3, rt.mrg))
    PlotImpVar(mSetObj, imp, "Importance", feat.num = n)
    par(op)
  } else {
    d <- .enet_xy(mSetObj, en$outcome)
    if (en$type == "disc") {
      PlotImpVarDisc(mSetObj, imp, "Importance", feat.num = n, type = "meta", norm.data = d$x, cls.init = d$y)
    } else {
      PlotImpVarCont(mSetObj, imp, "Importance", feat.num = n, type = "meta", norm.data = d$x, y = d$y)
    }
  }
  dev.off()
  return(.set.mSet(mSetObj))
}

.enet_fmt <- function(x, digits = 4) {
  vapply(x, function(v) if (is.na(v)) "NA" else format(signif(v, digits), scientific = FALSE), character(1))
}

# summary of the analysis, as HTML
GetEnetStatsText <- function(mSetObj = NA) {
  mSetObj <- .get.mSet(mSetObj)
  en <- mSetObj$analSet$enet
  if (is.null(en)) return("The analysis has not been performed yet.")
  if (!is.null(en$err)) return(paste0("<b>The analysis could not be performed:</b> ", en$err))
  m <- en$metrics
  head <- sprintf("Elastic net (alpha = %s), %d-fold cross-validation; outcome: %s (%d samples, %d features).",
                  format(en$alpha), en$nfolds, en$label, en$n, en$p)
  lam <- sprintf("lambda = %s (%s); <b>%d features selected</b>.", format(signif(en$lambda, 3)),
                 if (en$rule == "min") "minimum cross-validation error" else "1-SE rule", nrow(en$sigmat))
  perf <- if (en$type == "disc") {
    sprintf("Cross-validated misclassification error %s (SE %s), balanced accuracy %s%s.", .enet_fmt(m$err, 3),
            .enet_fmt(m$sd, 2), .enet_fmt(m$bacc, 3), if (is.na(m$auc)) "" else paste0(", AUC ", .enet_fmt(m$auc, 3)))
  } else {
    sprintf("Cross-validated mean-squared error %s (SE %s), R-squared %s.", .enet_fmt(m$err, 4), .enet_fmt(m$sd, 2),
            .enet_fmt(m$r2, 3))
  }
  paste0(head, "<br/>", lam, "<br/>", perf, if (nzchar(en$note)) paste0("<br/><i>", en$note, "</i>") else "")
}

# the selected features for the details page (Feature Details Table): one row per feature, the class
# coefficients (or the single coefficient) and the importance; the table shows at most 11 columns
.enet_detail_mat <- function(en) {
  sig <- en$sigmat
  if (ncol(sig) > 11) sig <- sig[, c(seq_len(10), ncol(sig)), drop = FALSE]
  signif(as.matrix(sig), 5)
}

.enet_ok <- function(mSetObj) {
  en <- .get.mSet(mSetObj)$analSet$enet
  if (is.null(en) || !is.null(en$err)) NULL else en
}

GetEnetSelCount <- function(mSetObj = NA) {
  en <- .enet_ok(mSetObj)
  if (is.null(en)) 0L else nrow(en$sigmat)
}

GetEnetSigMat <- function(mSetObj = NA) {
  en <- .enet_ok(mSetObj)
  if (is.null(en)) return(matrix(numeric(0), 0, 0))
  .enet_detail_mat(en)
}

GetEnetSigRowNames <- function(mSetObj = NA) {
  en <- .enet_ok(mSetObj)
  if (is.null(en)) character(0) else rownames(en$sigmat)
}

GetEnetSigColNames <- function(mSetObj = NA) {
  en <- .enet_ok(mSetObj)
  if (is.null(en)) character(0) else colnames(.enet_detail_mat(en))
}
