#######################
#######################
# functions for combining effect-sizes 
# adapted from the meta and metafor R packages
my_metagen <- function(TE, comb.fixed=TRUE, comb.random=FALSE, lower, upper, method.tau="DL"){
  
  seTE <- rep(NA, length(TE))
  
  sel.NA <- is.na(seTE)
  
  seTE <- my.TE.seTE.ci(lower, upper, 0.95)$seTE
  
  if(comb.fixed){
    ## Fixed effect estimate (Cooper & Hedges, 1994, p. 265-6)
    w.fixed <- 1 / seTE^2
    w.fixed[is.na(w.fixed) | is.na(TE)] <- 0

    TE.fixed   <- weighted.mean(TE, w.fixed, na.rm = TRUE)
    seTE.fixed <- sqrt(1 / sum(w.fixed, na.rm = TRUE))

    ci.f <- my_ci(TE.fixed, seTE.fixed, 0.95, null.effect = 0)
    zval.fixed <- ci.f$z
    pval.fixed <- ci.f$p
    lower.fixed <- ci.f$lower
    upper.fixed <- ci.f$upper
    
    res <- list(TE.fixed = TE.fixed, seTE.fixed = seTE.fixed,
                lower.fixed = lower.fixed, upper.fixed = upper.fixed,
                zval.fixed = zval.fixed, pval.fixed = pval.fixed)
  }
  
  if(comb.random){
    
    meta2 <- my_rma.uni2(as.numeric(TE), sei=as.numeric(seTE), method=method.tau)
    
    pval <- meta2$pval
    beta <- as.vector(meta2$beta)
    se <- meta2$se
    ci.lower <- meta2$ci.lb
    ci.upper <- meta2$ci.ub
    zval <- meta2$zval
    
    res <- list(TE.random = beta, seTE.random = se,
                lower.random = ci.lower, upper.random = ci.upper,
                zval.random = zval, pval.random = pval)
  }
  return(res)
}

my.TE.seTE.ci <- function(lower, upper, level = 0.95,
                          df = rep(NA, length(lower))) {
  
  varTE <- ifelse(is.na(df),
                  ((upper - lower) /
                     (2 * qnorm((1 - level) / 2, lower.tail = FALSE)))^2,
                  ((upper - lower) /
                     (2 * qt((1 - level) / 2, df = df, lower.tail = FALSE)))^2)

  seTE <- sqrt(varTE)
  
  res <- list(TE = lower + (upper - lower) / 2, seTE = seTE,
              lower = lower, upper = upper,
              level = level, df = df)

  res
}

my_ci <- function(TE, seTE, level = 0.95, null.effect = 0) {
  
  alpha <- 1 - level
  
  lower  <- TE - qnorm(1 - alpha / 2) * seTE
  upper  <- TE + qnorm(1 - alpha / 2) * seTE
  zscore <- (TE - null.effect) / seTE
  pval   <- 2 * pnorm(abs(zscore), lower.tail = FALSE)
  df <- NA
  
  list(TE = TE, seTE = seTE,
       lower = lower, upper = upper,
       z = zscore, p = pval, level = level)
}

my_rma.uni2 <- function(yi, vi, sei, ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i, m1i, m2i, sd1i, sd2i, xi, mi, ri, ti, sdi, r2i, ni, mods, #scale,
                           measure="GEN", intercept=TRUE,
                           data, slab, subset,
                           add=1/2, to="only0", drop00=FALSE, vtype="LS",
                           method="REML", weighted=TRUE, test="z", #knha=FALSE,
                           level=95, digits, btt, tau2, verbose=FALSE, control, ...) {
  
  tau2 <- NULL
  control <- list()
  Z <- NULL

  digits <- c(est=4, se=4, test=4, pval=4, ci=4, var=4, sevar=4, fit=4, het=4)
  mf <- match.call()
  
  ### for certain measures, set add=0 by default unless user explicitly sets the add argument
  
  addval <- mf[[match("add", names(mf))]]
  
  ### extract yi (either NULL if not specified, a vector, a formula, or an escalc object)
  
  mf.yi <- mf[[match("yi", names(mf))]]
  data <- sys.frame(sys.parent())
  yi <- eval(mf.yi, data, enclos=sys.frame(sys.parent())) ### NULL if user does not specify this
  data <- yi
  scale <- mods <- subset <- slab <- NULL ### NULL if user does not specify this
  ai <- bi <- ci <- di <- x1i <- x2i <- t1i <- t2i <- NA
  
  if (!is.null(yi)) {
    
    yi.escalc <- FALSE
    
    ### number of outcomes before subsetting
    
    k <- length(yi)
    k.all <- k
    attr(yi, "measure") <- measure
    
    ### extract ni argument
    ni <- NULL ### NULL if user does not specify this
    vi <- sei^2

    ### check if user constrained vi to 0
    
    if (length(vi) == 1L && vi == 0) {
      vi0 <- TRUE
    } else {
      vi0 <- FALSE
    }
    
    ### allow easy setting of vi to a single value
    
    if (length(vi) == 1L)
      vi <- rep(vi, k) ### note: k is number of outcomes before subsetting
  }
  
  model <- "rma.uni"
  
  ### study ids (1:k sequence before subsetting)
  
  ids <- seq_len(k)
  
  if (is.null(slab)) {
    slab.null <- TRUE
    slab      <- ids
  }
  
  ### add slab attribute back
  
  attr(yi, "slab") <- slab
  
  ### number of outcomes after subsetting
  
  k <- length(yi)
  
  ### check for non-positive sampling variances (and set negative values to 0)
  
  if (any(vi <= 0, na.rm=TRUE)) {
    allvipos <- FALSE
    vi.neg <- vi < 0
  } else {
    allvipos <- TRUE
  }
  
  ai.f      <- ai
  bi.f      <- bi
  ci.f      <- ci
  di.f      <- di
  x1i.f     <- x1i
  x2i.f     <- x2i
  t1i.f     <- t1i
  t2i.f     <- t2i
  yi.f      <- yi
  vi.f      <- vi
  ni.f      <- ni
  mods.f    <- mods
  Z.f       <- Z
  
  k.f <- k ### total number of observed outcomes including all NAs
  
  if (k == 1) {
    method <- "FE"
    test   <- "z"
  }

  ### add vector of 1s to the X matrix for the intercept (if intercept=TRUE)
  
  X   <- cbind(intrcpt=rep(1,k), mods)
  X.f <- cbind(intrcpt=rep(1,k.f), mods.f)
  
  ### drop redundant predictors
  ### note: need to save coef.na for functions that modify the data/model and then refit the model (regtest() and the
  ### various function that leave out an observation); so we can check if there are redundant/dropped predictors then
  tmp <- lm(yi ~ X - 1)
  coef.na <- is.na(coef(tmp))
  if (any(coef.na)) {
    X   <- X[,!coef.na,drop=FALSE]
    X.f <- X.f[,!coef.na,drop=FALSE]
  }
  
  ### check whether intercept is included and if yes, move it to the first column (NAs already removed, so na.rm=TRUE for any() not necessary)
  is.int <- apply(X, 2, .is.intercept)
  if (any(is.int)) {
    int.incl <- TRUE
    int.indx <- which(is.int, arr.ind=TRUE)
    X        <- cbind(intrcpt=1,   X[,-int.indx, drop=FALSE]) ### this removes any duplicate intercepts
    X.f      <- cbind(intrcpt=1, X.f[,-int.indx, drop=FALSE]) ### this removes any duplicate intercepts
    intercept <- TRUE ### set intercept appropriately so that the predict() function works
  } else {
    int.incl <- FALSE
  }
  
  p <- NCOL(X) ### number of columns in X (including the intercept if it is included)
  
  ### check whether this is an intercept-only model
  
  if ((p == 1L) && .is.intercept(X)) {
    int.only <- TRUE
  } else {
    int.only <- FALSE
  }
  
  btt <- .set.btt(btt, p, int.incl, X)
  m <- length(btt) 
  
  #########################################################################
  ### set default control parameters
  
  con <- list(verbose = FALSE,
              tau2.init = NULL,          # initial value for iterative estimators (ML, REML, EB, SJ, SJIT, DLIT)
              tau2.min = 0,              # lower bound for tau^2 value
              tau2.max = 100,            # upper bound for tau^2 value (for PM/PMM/GENQM estimators; and passed down for tau^2 CI obtained with confint())
              threshold = 10^-5,         # convergence threshold (for ML, REML, EB, SJIT, DLIT)
              tol = .Machine$double.eps^0.25, # convergence tolerance for uniroot() as used for PM, PMM, and GENQM (also used in 'll0 - ll > con$tol' check for ML/REML)
              ll0check = TRUE,           # should the 'll0 - ll > con$tol' check be conducted for ML/REML?
              maxiter = 100,             # maximum number of iterations (for ML, REML, EB, SJIT, DLIT)
              stepadj = 1,               # step size adjustment for Fisher scoring algorithm (for ML, REML, EB)
              REMLf = TRUE,              # should |X'X| term be included in the REML log likelihood?
              evtol = 1e-07,             # lower bound for eigenvalues to determine if model matrix is positive definite (also for checking if vimaxmin >= 1/con$evtol)
              alpha.init = NULL,         # initial values for scale parameters
              optimizer = "nlminb",      # optimizer to use ("optim", "nlminb", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm", "hjk", "nmk", "ucminf", "optimParallel") for location-scale model
              optmethod = "BFGS",        # argument 'method' for optim() ("Nelder-Mead" and "BFGS" are sensible options)
              parallel = list(),         # parallel argument for optimParallel() (note: 'cl' argument in parallel is not passed; this is directly specified via 'cl')
              cl = NULL,                 # arguments for optimParallel()
              ncpus = 1L,                # arguments for optimParallel()
              hessianCtrl=list(r=8),     # arguments passed on to 'method.args' of hessian()
              scaleZ = TRUE)
  
  ### replace defaults with any user-defined values
  
  con.pos <- pmatch(names(control), names(con))
  con[c(na.omit(con.pos))] <- control[!is.na(con.pos)]
  
  ### contrain negative tau2.min values to -min(vi) (to ensure that the marginal variance is always >= 0)
  
  if (con$tau2.min < 0 && (-con$tau2.min > min(vi))) {
    con$tau2.min <- -min(vi)
  }
  
  ### convergence indicator and change variable (for iterative estimators)
  conv <- 1
  change <- con$threshold + 1
  
  ### iterations counter for iterative estimators (i.e., DLIT, SJIT, ML, REML, EB)
  ### (note: PM, PMM, and GENQM are also iterative, but uniroot() handles that)
  iter <- 0
  se.tau2 <- I2 <- H2 <- QE <- QEp <- NA
  s2w <- 1
  level <- ifelse(level == 0, 1, ifelse(level >= 1, (100-level)/100, ifelse(level > .5, 1-level, level)))
  Y <- as.matrix(yi)
  
  #########################################################################
  ###### heterogeneity estimation for standard model (rma.uni)
  
  if (model == "rma.uni") {
    
    tau2.fix <- FALSE
    tau2.val <- NA

    ### Hunter & Schmidt (HS) estimator
    
    if (method == "HS") {
      
      if (!allvipos)
        stop("HS estimator cannot be used when there are non-positive sampling variances in the data.")
      
      wi    <- 1/vi
      W     <- diag(wi, nrow=k, ncol=k)
      stXWX <- .invcalc(X=X, W=W, k=k)
      P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
      RSS   <- crossprod(Y,P) %*% Y
      tau2  <- ifelse(tau2.fix, tau2.val, (RSS-k)/sum(wi))
    }
    
    ### Hedges (HE) estimator (or initial value for ML, REML, EB)
    
    if (is.element(method, c("HE","ML","REML","EB"))) {
      
      stXX  <- .invcalc(X=X, W=diag(k), k=k)
      P     <- diag(k) - X %*% tcrossprod(stXX,X)
      RSS   <- crossprod(Y,P) %*% Y
      V     <- diag(vi, nrow=k, ncol=k)
      PV    <- P %*% V ### note: this is not symmetric
      trPV  <- .tr(PV) ### since PV needs to be computed anyway, can use .tr()
      tau2  <- ifelse(tau2.fix, tau2.val, (RSS - trPV) / (k-p))
    }
    
    ### DerSimonian-Laird (DL) estimator
    
    if (method == "DL") {
      
      if (!allvipos)
        stop("DL estimator cannot be used when there are non-positive sampling variances in the data.")
      
      wi    <- 1/vi
      W     <- diag(wi, nrow=k, ncol=k)
      stXWX <- .invcalc(X=X, W=W, k=k)
      P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
      RSS   <- crossprod(Y,P) %*% Y
      trP   <- .tr(P)
      tau2  <- ifelse(tau2.fix, tau2.val, (RSS - (k-p)) / trP)
    }
    
    ### DerSimonian-Laird (DL) estimator with iteration
    
    if (method == "DLIT") {
      
      if (is.null(con$tau2.init)) {
        tau2 <- 0
      } else {
        tau2 <- con$tau2.init
      }
      
      while (change > con$threshold) {
        iter <- iter + 1
        old2 <- tau2
        wi   <- 1/(vi + tau2)
        if (any(tau2 + vi < 0))
          stop("Some marginal variances are negative.")
        if (any(is.infinite(wi)))
          stop("Division by zero when computing the inverse variance weights.")
        W     <- diag(wi, nrow=k, ncol=k)
        stXWX <- .invcalc(X=X, W=W, k=k)
        P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
        RSS   <- crossprod(Y,P) %*% Y
        trP   <- .tr(P)
        tau2  <- ifelse(tau2.fix, tau2.val, (RSS - (k-p)) / trP)
        tau2[tau2 < con$tau2.min] <- con$tau2.min
        change <- abs(old2 - tau2)
        
        if (iter > con$maxiter) {
          conv <- 0
          break
        }
      }
      if (conv == 0L)
        stop("Algorithm did not converge.")
    }
    
    ### Sidik-Jonkman (SJ) estimator
    
    if (method == "SJ") {
      
      if (is.null(con$tau2.init)) {
        tau2.0 <- c(var(yi) * (k-1)/k)
      } else {
        tau2.0 <- con$tau2.init
      }
      
      wi    <- 1/(vi + tau2.0)
      W     <- diag(wi, nrow=k, ncol=k)
      stXWX <- .invcalc(X=X, W=W, k=k)
      P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
      RSS   <- crossprod(Y,P) %*% Y
      V     <- diag(vi, nrow=k, ncol=k)
      PV    <- P %*% V ### note: this is not symmetric
      tau2  <- ifelse(tau2.fix, tau2.val, tau2.0 * RSS / (k-p))
    }
    
    ### Sidik-Jonkman (SJ) estimator with iteration
    
    if (method == "SJIT") {
      
      if (is.null(con$tau2.init)) {
        tau2 <- var(yi) * (k-1)/k
      } else {
        tau2 <- con$tau2.init
      }
      
      tau2.0 <- tau2
      
      while (change > con$threshold) {

        iter <- iter + 1
        old2 <- tau2
        
        wi     <- 1/(vi + tau2)
        W      <- diag(wi, nrow=k, ncol=k)
        stXWX  <- .invcalc(X=X, W=W, k=k)
        P      <- W - W %*% X %*% stXWX %*% crossprod(X,W)
        RSS    <- crossprod(Y,P) %*% Y
        V      <- diag(vi, nrow=k, ncol=k)
        PV     <- P %*% V ### note: this is not symmetric
        tau2   <- ifelse(tau2.fix, tau2.val, tau2 * RSS / (k-p))
        change <- abs(old2 - tau2)
        
        if (iter > con$maxiter) {
          conv <- 0
          break
        }
        
      }
      
      if (conv == 0L)
        stop("Algorithm did not converge.")
    }
    
    ### Paule-Mandel (PM) estimator
    
    if (method == "PM") {
      
      if (!allvipos)
        stop("PM estimator cannot be used when there are non-positive sampling variances in the data.")
      
      if (!tau2.fix) {
        
        if (.QE.func(con$tau2.min, Y=Y, vi=vi, X=X, k=k, objective=0) < k-p) {
          tau2 <- con$tau2.min
        } else {
          if (.QE.func(con$tau2.max, Y=Y, vi=vi, X=X, k=k, objective=0) > k-p) {
            stop("Value of 'tau2.max' too low. Try increasing 'tau2.max' or switch to another 'method'.")
          } else {
            tau2 <- try(uniroot(.QE.func, interval=c(con$tau2.min, con$tau2.max), tol=con$tol, maxiter=con$maxiter, Y=Y, vi=vi, X=X, k=k, objective=k-p, verbose=verbose, digits=digits, extendInt="no")$root, silent=TRUE)
            if (inherits(tau2, "try-error"))
              stop("Error in iterative search for tau2 using uniroot().")
          }
        }
      } else {
        tau2 <- tau2.val
      }
      wi <- 1/(vi + tau2)
    }
    
    ### Paule-Mandel (PM) estimator (median unbiased version)
    
    if (method == "PMM") {
      
      if (!allvipos)
        stop("PMM estimator cannot be used when there are non-positive sampling variances in the data.")
      
      if (!tau2.fix) {
        if (.QE.func(con$tau2.min, Y=Y, vi=vi, X=X, k=k, objective=0) < qchisq(0.5, df=k-p)) {
          tau2 <- con$tau2.min
        } else {
          if (.QE.func(con$tau2.max, Y=Y, vi=vi, X=X, k=k, objective=0) > qchisq(0.5, df=k-p)) {
            stop("Value of 'tau2.max' too low. Try increasing 'tau2.max' or switch to another 'method'.")
          } else {
            tau2 <- try(uniroot(.QE.func, interval=c(con$tau2.min, con$tau2.max), tol=con$tol, maxiter=con$maxiter, Y=Y, vi=vi, X=X, k=k, objective=qchisq(0.5, df=k-p), verbose=verbose, digits=digits, extendInt="no")$root, silent=TRUE)
            if (inherits(tau2, "try-error"))
              stop("Error in iterative search for tau2. Try increasing 'tau2.max' or switch to another 'method'.")
          }
        }
      } else {
        tau2 <- tau2.val
      }
      wi <- 1/(vi + tau2)
    }
    
    ### maximum-likelihood (ML), restricted maximum-likelihood (REML), and empirical Bayes (EB) estimators
    
    if (is.element(method, c("ML","REML","EB"))) {
      
      tau2 <- max(0, tau2, con$tau2.min)
      
      while (change > con$threshold) {
        
        iter <- iter + 1
        old2 <- tau2
        wi   <- 1/(vi + tau2)
        if (any(tau2 + vi < 0))
          stop("Some marginal variances are negative.")
        if (any(is.infinite(wi)))
          stop("Division by zero when computing the inverse variance weights.")
        W     <- diag(wi, nrow=k, ncol=k)
        stXWX <- .invcalc(X=X, W=W, k=k)
        P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
        
        if (method == "ML") {
          PP  <- P %*% P
          adj <- (crossprod(Y,PP) %*% Y - sum(wi)) / sum(wi^2)
        }
        if (method == "REML") {
          PP  <- P %*% P
          adj <- (crossprod(Y,PP) %*% Y - .tr(P)) / .tr(PP)
        }
        if (method == "EB") {
          adj <- (crossprod(Y,P) %*% Y * k/(k-p) - k) / sum(wi)
        }
        
        adj <- adj * con$stepadj ### apply (user-defined) step adjustment
        
        while (tau2 + adj < con$tau2.min) ### use step-halving if necessary
          adj <- adj / 2
        
        tau2   <- ifelse(tau2.fix, tau2.val, tau2 + adj)
        change <- abs(old2 - tau2)
        
        if (iter > con$maxiter) {
          conv <- 0
          break
        }
      }
      
      if (conv == 0L)
        stop("Fisher scoring algorithm did not converge. See 'help(rma)' for possible remedies.")
      
      ### check if ll is larger when tau^2 = 0 (only if ll0check=TRUE and only possible/sensible if allvipos and !tau2.fix)
      ### note: this doesn't catch the case where tau^2 = 0 is a local maximum
      
      if (is.element(method, c("ML","REML")) && con$ll0check && allvipos && !tau2.fix) {
        
        wi    <- 1/vi
        W     <- diag(wi, nrow=k, ncol=k)
        stXWX <- .invcalc(X=X, W=W, k=k)
        beta  <- stXWX %*% crossprod(X,W) %*% Y
        RSS   <- sum(wi*(yi - X %*% beta)^2)
        if (method == "ML")
          ll0 <- -1/2 * (k)   * log(2*base::pi) - 1/2 * sum(log(vi)) - 1/2 * RSS
        if (method == "REML")
          ll0 <- -1/2 * (k-p) * log(2*base::pi) - 1/2 * sum(log(vi)) - 1/2 * determinant(crossprod(X,W) %*% X, logarithm=TRUE)$modulus - 1/2 * RSS
        
        wi    <- 1/(vi + tau2)
        if (any(tau2 + vi < 0))
          stop("Some marginal variances are negative.")
        if (any(is.infinite(wi)))
          stop("Division by zero when computing the inverse variance weights.")
        W     <- diag(wi, nrow=k, ncol=k)
        stXWX <- .invcalc(X=X, W=W, k=k)
        beta  <- stXWX %*% crossprod(X,W) %*% Y
        RSS   <- sum(wi*(yi - X %*% beta)^2)
        if (method == "ML")
          ll <- -1/2 * (k)   * log(2*base::pi) - 1/2 * sum(log(vi + tau2)) - 1/2 * RSS
        if (method == "REML")
          ll <- -1/2 * (k-p) * log(2*base::pi) - 1/2 * sum(log(vi + tau2)) - 1/2 * determinant(crossprod(X,W) %*% X, logarithm=TRUE)$modulus - 1/2 * RSS
        
        if (ll0 - ll > con$tol && tau2 > con$threshold) {
          tau2 <- 0
        }
      }
      
      ### need to run this so that wi and P are based on the final tau^2 value
      
      wi     <- 1/(vi + tau2)
      if (any(tau2 + vi < 0))
        stop("Some marginal variances are negative.")
      if (any(is.infinite(wi)))
        stop("Division by zero when computing the inverse variance weights.")
      W     <- diag(wi, nrow=k, ncol=k)
      stXWX <- .invcalc(X=X, W=W, k=k)
      P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
      
    }
    
    ### make sure that tau2 is >= con$tau2.min
    tau2 <- max(con$tau2.min, c(tau2))
    
    ### check if any marginal variances are negative (only possible if user has changed tau2.min)
    
    if (!is.na(tau2) && any(tau2 + vi < 0))
      stop("Some marginal variances are negative.")
    
    if (method == "HS")
      se.tau2 <- sqrt(1/sum(wi)^2 * (2*(k-p) + 4*max(tau2,0)*.tr(P) + 2*max(tau2,0)^2*sum(P*P))) ### note: wi = 1/vi
    if (method == "HE")
      se.tau2 <- sqrt(1/(k-p)^2 * (2*sum(PV*t(PV)) + 4*max(tau2,0)*trPV + 2*max(tau2,0)^2*(k-p)))
    if (method == "DL" || method == "DLIT")
      se.tau2 <- sqrt(1/trP^2 * (2*(k-p) + 4*max(tau2,0)*trP + 2*max(tau2,0)^2*sum(P*P)))
    if (method == "GENQ" || method == "GENQM")
      se.tau2 <- sqrt(1/trP^2 * (2*sum(PV*t(PV)) + 4*max(tau2,0)*sum(PV*P) + 2*max(tau2,0)^2*sum(P*P)))
    if (method == "SJ")
      se.tau2 <- sqrt(tau2.0^2/(k-p)^2 * (2*sum(PV*t(PV)) + 4*max(tau2,0)*sum(PV*P) + 2*max(tau2,0)^2*sum(P*P)))
    if (method == "ML")
      se.tau2 <- sqrt(2/sum(wi^2)) ### note: wi = 1/(vi + tau2) for ML, REML, EB, PM, and SJIT
    if (method == "REML")
      se.tau2 <- sqrt(2/sum(P*P))
    if (method == "EB" || method == "PM" || method == "PMM" || method == "SJIT") {
      se.tau2 <- sqrt(2*k^2/(k-p) / sum(wi)^2) ### these two equations are actually identical, but this one is much simpler
    }
    
  }
  
  ### fixed-effects model (note: set tau2 to zero even when tau2 is specified)
  
  if (method == "FE")
    tau2 <- 0
  
  ###### model fitting, test statistics, and confidence intervals
  
  wi <- 1/(vi + tau2)
  W  <- diag(wi, nrow=k, ncol=k)
  M  <- diag(vi + tau2, nrow=k, ncol=k)
  
  stXX  <- .invcalc(X=X, W=diag(k), k=k)
  beta  <- stXX %*% crossprod(X,Y)
  vb    <- tcrossprod(stXX,X) %*% M %*% X %*% stXX
  RSS.f <- sum(wi*(yi - X %*% beta)^2)

  ### calculate scaling factor for Knapp & Hartung method
  
  if (is.element(test, c("knha","adhoc"))) {
    
    if (any(is.infinite(wi)))
      stop("Division by zero when computing the inverse variance weights.")
    
    stXWX     <- .invcalc(X=X, W=W, k=k)
    beta.knha <- stXWX %*% crossprod(X,W) %*% Y
    RSS.knha  <- sum(wi*(yi - X %*% beta.knha)^2)
    #P         <- W - W %*% X %*% stXWX %*% crossprod(X,W)
    #RSS.knha  <- c(crossprod(Y,P) %*% Y)
    
    if (RSS.knha <= .Machine$double.eps) {
      s2w <- 0
    } else {
      s2w <- RSS.knha / (k-p)
    }
    
  }
  
  ### Knapp & Hartung method with ad-hoc correction so that the scale factor is always >= 1
  
  if (test == "adhoc")
    s2w[s2w < 1] <- 1
  
  ### for Knapp & Hartung method, apply scaling to vb
  
  vb <- s2w * vb
  
  ### QM calculation
  
  QM <- try(as.vector(t(beta)[btt] %*% chol2inv(chol(vb[btt,btt])) %*% beta[btt]), silent=TRUE)
  
  if (inherits(QM, "try-error"))
    QM <- NA
  
  se <- sqrt(diag(vb))
  names(se) <- NULL
  zval <- c(beta/se)
  
  if (is.element(test, c("knha","adhoc","t"))) {
    dfs <- k-p
    QM  <- QM / m
    if (dfs > 0) {
      QMp  <- pf(QM, df1=m, df2=dfs, lower.tail=FALSE)
      pval <- 2*pt(abs(zval), df=dfs, lower.tail=FALSE)
      crit <- qt(level/2, df=dfs, lower.tail=FALSE)
    } else {
      QMp  <- NaN
      pval <- NaN
      crit <- NaN
    }
  } else {
    dfs  <- NA
    QMp  <- pchisq(QM, df=m, lower.tail=FALSE)
    pval <- 2*pnorm(abs(zval), lower.tail=FALSE)
    crit <- qnorm(level/2, lower.tail=FALSE)
  }
  
  ci.lb <- c(beta - crit * se)
  ci.ub <- c(beta + crit * se)
  
  res <- list(b=beta, beta=beta, se=se, zval=zval, pval=pval, ci.lb=ci.lb, ci.ub=ci.ub)
  
  class(res) <- c("rma.uni", "rma")
  return(res)
}

.is.intercept <- function(x, eps=1e-08)
  return(all(abs(x - 1) < eps))

.invcalc <- function(X, W, k) {
  sWX <- sqrt(W) %*% X
  res.qrs <- qr.solve(sWX, diag(k))
  return(tcrossprod(res.qrs))
}

.tr <- function(X)
  return(sum(diag(X)))

.QE.func <- function(tau2val, Y, vi, X, k, objective, verbose=FALSE, digits=4) {
  W     <- diag(1/(vi + tau2val), nrow=k, ncol=k)
  stXWX <- .invcalc(X=X, W=W, k=k)
  P     <- W - W %*% X %*% stXWX %*% crossprod(X,W)
  RSS   <- crossprod(Y,P) %*% Y
  return(RSS - objective)
}

.set.btt <- function(btt, p, int.incl, X) {
  if (p > 1) {                        ### if the model matrix has more than one column
    if (int.incl) {
      btt <- seq.int(from=2, to=p)     ### and the model has an intercept term, test all coefficients except the intercept
    } else {
      btt <- seq_len(p)                ### and the model does not have an intercept term, test all coefficients
    }
  } else {
    btt <- 1                         ### if the model matrix has a single column, test that single coefficient
  }
  return(btt)
}
