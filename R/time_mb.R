#'Timecourse analysis
#'@description Adapted from the timecourse package by Yu Chuan Tai
#'This method is only applicable for time-series, not for general case
#'two/multiple factor analysis
#'@usage performMB(mSetObj, topPerc)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param topPerc select the cut-off, default is 10
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
performMB <- function(mSetObj=NA, topPerc = 10){

  mSetObj <- .get.mSet(mSetObj);
  
  time.fac <- mSetObj$dataSet$time.fac;
  exp.fac <- mSetObj$dataSet$exp.fac;
  sbj <- vector(mode="character", length=nrow(mSetObj$dataSet$norm));
  
  time.len <- length(levels(time.fac));
  exp.len <- numeric(length(levels(exp.fac)));
  # to record the replicates in each exp condition
  # note, this is not sample number, but replicates/subjects
  k = 1;
  len = 0;
  m = 0;
  for(lv1 in levels(exp.fac)){
    m = m + 1;
    # same subjects must in the same exp. condition
    inx1 <- exp.fac == lv1;
    
    # same subjects must not in the same time points
    # all subjects will appear in each time points once
    for (lv2 in levels(time.fac)){
      inx2 <- time.fac == lv2;
      len <- sum(inx1 & inx2);
      exp.len[m] <- len;
      sbj[inx1 & inx2] <- paste("S", k:(k+len-1), sep="");
    }
    k = k + len;
  }
  
  # the size matrix specify the number of replicates in each exp. condition
  size <- matrix(exp.len, byrow=TRUE, nrow=ncol(mSetObj$dataSet$norm), ncol=length(levels(exp.fac)));
  exp.grp <- as.character(exp.fac);
  rep.grp <- as.character(sbj);

  if(mSetObj$dataSet$design == "time0"){ # time series only
    time.len <- length(levels(time.fac));
    subj.len <- rep(length(levels(exp.fac)), ncol(mSetObj$dataSet$norm));
    rep.grp <- as.character(exp.fac);
    time.grp <- as.numeric(time.fac);
    MB.stats <- suppressWarnings(mb.1D(t(mSetObj$dataSet$norm), time.len, subj.len, n.grp=rep.grp, k.grp=time.grp));
  }else if(length(levels(exp.fac)) > 2){
    MB.stats <- suppressWarnings(mb.MANOVA(t(mSetObj$dataSet$norm), times=time.len, D=length(exp.len), size=size, rep.grp=rep.grp, condition.grp=exp.grp));
  }else{
    MB.stats <- suppressWarnings(mb.2D(t(mSetObj$dataSet$norm), time.len, size, exp.grp, mn.grp=rep.grp));
  }
  if(is.null(MB.stats)) {
    AddErrMsg("Please make sure the data are formatted properly for time-series analysis. In particular, for each time point, all experiments must exist and cannot be missing!"); 
    mSetObj$analSet$MB <- NULL;
    return(0);
  }
  write.csv(signif(MB.stats, 5), file="meba_sig_features.csv");
  mSetObj$analSet$MB <-list(stats=MB.stats);
  return(.set.mSet(mSetObj));
}

#'Plot MB Time Profile
#'@description Plot MB Time Profile
#'@param mSetObj Input name of the created mSet Object
#'@param cmpdNm Input the name of the compound
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotMBTimeProfile <- function(mSetObj=NA, cmpdNm,  format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 5.6;
  }else if(width == 0){
    w <- 5;
  }else{
    w <- width;
  }
  h <- w;
  mSetObj$imgSet$mb <- imgName;
  Cairo::Cairo(file = imgName,  unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plotProfile(mSetObj, cmpdNm);
  dev.off();
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(imgName);  
  }
  
  return(.set.mSet(mSetObj));
}

# Used in higher function
mb.MANOVA <- function (object, times, D, size, nu = NULL, Lambda = NULL, beta.d = NULL,
                       beta = NULL, alpha.d = NULL, alpha = NULL, condition.grp,
                       time.grp = NULL, rep.grp = NULL, p = 0.02)
{
  M <- as.matrix(object)
  tr <- function(X) {
    sum(diag(X))
  }
  G <- nrow(M)
  
  max.size <- apply(size, 2, max)
  if (ncol(size) != D)
    stop("The sample sizes are incorrect!")
  for (i in 1:D) {
    if ((max.size[i] * times) != sum(condition.grp ==
                                     sort(unique(condition.grp))[i]))
      stop("The sample sizes or the biological condition group assignments are incorrect!")
  }
  
  
  time.grp <- rep(1:times, ncol(M)/times);
  
  if (length(unique(time.grp)) != times)
    stop("The number of time points or the time group \n    assignments is incorrect!")
  if (is.null(rep.grp)) {
    rep.grp <- rep(1:(ncol(M)/times), rep(times, ncol(M)/times))
    cat("Replicate group assignments are set to default.",
        "\n")
  }
  mydata <- M
  indx <- order(condition.grp, rep.grp, time.grp)
  M <- M[, indx]
  mis <- apply(!apply(M, 1, is.na), 2, sum)
  mis <- sum((mis/times - floor(mis/times)) != 0)
  if (mis > 0)
    stop(mis, " genes may have within replicate missing values.")
  N <- apply(size, 1, sum)
  Sp <- apply(M, 1, matrix.cov, times, trans = FALSE, c.grp = condition.grp)
  diagSp <- apply(Sp, 2, function(x) diag(matrix(x, ncol = times)))
  Sp.avg <- matrix(apply(Sp, 1, mean, na.rm = TRUE), ncol = times)
  if (is.null(nu) || is.null(Lambda)) {
    nu.lim <- times + 6
    if (!is.null(nu)) {
      nu0 <- nu
      nu <- max(nu0, nu.lim)
      if (is.infinite(nu) & is.null(Lambda)) {
        Lambda <- Sp.avg
      }
      if (is.finite(nu) & is.null(Lambda)) {
        Lambda <- (nu - times - 1) * Sp.avg/nu
      }
      nu <- nu0
    }
    if (is.null(nu)) {
      nu0 <- mean(sapply(1:times, function(x) squeezeVar(diagSp[x,
                                                                ], N - D)$df.prior))
      nu <- max(nu0, nu.lim)
      if (is.infinite(nu) & is.null(Lambda)) {
        Lambda <- Sp.avg
      }
      if (is.finite(nu) & is.null(Lambda)) {
        Lambda <- (nu - times - 1) * Sp.avg/nu
      }
      nu <- nu0
    }
  }
  max.size <- apply(size, 2, max)
  xbar.d <- as.list(NULL)
  for (i in 1:D) {
    grp.indx <- condition.grp == sort(unique(condition.grp))[i]
    xbar.d[[i]] <- apply(M[, grp.indx], 1, function(x) apply(matrix(x,
                                                                    byrow = TRUE, ncol = times), 2, mean, na.rm = TRUE))
  }
  
  simple.stat <- NULL
  for(i in 1:(D-1))
    for(j in (i+1):D)
      simple.stat <- cbind(simple.stat, apply(abs(xbar.d[[i]]-xbar.d[[j]]),2,sum,na.rm=TRUE))
  
  simple.stat <- apply(simple.stat,1,sum,na.rm=TRUE)
  simple.rank <- G-rank(simple.stat)+1
  indx1 <- simple.rank<=G*p
  xbar <- sapply(1:G, function(x) apply(matrix(M[x, ], byrow = TRUE,
                                               ncol = times), 2, mean, na.rm = TRUE))
  if (is.null(alpha.d))
    alpha.d <- sapply(1:D, function(x) apply(xbar.d[[x]][,indx1],
                                             1, mean, na.rm = TRUE))
  if (is.null(alpha))
    alpha <- apply(xbar[,!indx1], 1, mean, na.rm = TRUE)
  
  
  if (is.null(beta.d) || is.null(beta)) {
    U.d <- lapply(1:D, function(x) apply(xbar.d[[x]][,indx1] - alpha.d[,
                                                                       x], 2, function(y) y %*% t(y)))
    U <- apply(xbar[,!indx1] - alpha, 2, function(y) y %*% t(y))
    Ubar.d <- sapply(1:D, function(x) apply(U.d[[x]], 1,
                                            mean, na.rm = TRUE))
    Ubar <- apply(U, 1, mean, na.rm = TRUE)
    if (is.null(beta.d)){
      Sp <- apply(M[indx1,], 1, matrix.cov, times, trans = FALSE, c.grp = condition.grp)
      Sp.avg <- matrix(apply(Sp, 1, mean, na.rm = TRUE), ncol = times)
      beta.d <- sapply(1:D, function(x) tr(Sp.avg)/tr(matrix(Ubar.d[,
                                                                    x], ncol = times)))
    }
    if (is.null(beta)){
      Sp <- apply(M[!indx1,], 1, matrix.cov, times, trans = FALSE, c.grp = condition.grp)
      Sp.avg <- matrix(apply(Sp, 1, mean, na.rm = TRUE), ncol = times)
      beta <- tr(Sp.avg)/tr(matrix(Ubar, ncol = times))
    }
  }
  Sp <- apply(M, 1, matrix.cov, times, trans = FALSE, c.grp = condition.grp)
  Sp.avg <- matrix(apply(Sp, 1, mean, na.rm = TRUE), ncol = times)
  U.d <- lapply(1:D, function(x) apply(xbar.d[[x]] - alpha.d[,
                                                             x], 2, function(y) y %*% t(y)))
  U <- apply(xbar- alpha, 2, function(y) y %*% t(y))
  total <- (N - 1) * apply(M, 1, matrix.cov, times, trans = FALSE,
                           c.grp = rep(1, ncol(M)))
  within <- Sp * (N - D)
  if (sum(N == max(N)) == G)
    M <- U/(N[1]^(-1) + beta^(-1))
  if (sum(N == max(N)) < G)
    M <- sapply(1:G, function(x) U[, x]/(N[x]^(-1) + beta^(-1)))
  M.d <- as.list(NULL)
  for (i in 1:D) {
    if (sum(size[, i] == max.size[i]) == G)
      M.d[[i]] <- U.d[[i]]/(size[1, i]^(-1) + beta.d[i]^(-1))
    if (sum(size[, i] == max.size[i]) < G)
      M.d[[i]] <- sapply(1:G, function(x) U.d[[i]][, x]/(size[x,
                                                              i]^(-1) + beta.d[i]^(-1)))
  }
  M1 <- matrix(0, nrow = times^2, ncol = G)
  for (i in 1:D) M1 <- M1 + M.d[[i]]
  tol <- .Machine$double.eps
  
  if (nu < 0)
    stop("The estimation of prior degrees of freedom <0 !")
  if (is.finite(nu) & nu > tol) {
    MB1 <- log(p, 10) - log(1 - p, 10)
    MB2 <- 0.5 * times * (log(N + beta, 10) - log(beta, 10))
    MB3 <- 0.5 * times * apply((log(beta.d, 10) - log(size +
                                                        beta.d, 10)), 1, sum)
    MB4 <- sapply(1:G, function(x) 0.5 * (N[x] + nu) * (log(det(matrix(total[,
                                                                             x], ncol = times) + matrix(M[, x], ncol = times) +
                                                                  nu * Lambda), 10) - log(det(matrix(within[, x], ncol = times) +
                                                                                                matrix(M1[, x], ncol = times) + nu * Lambda), 10)))
    MB <- MB1 + MB2 + MB3 + MB4
  }
  if (is.infinite(nu)) {
    MB1 <- log(p, 10) - log(1 - p, 10)
    MB2 <- 0.5 * times * (log(N + beta, 10) - log(beta, 10))
    MB3 <- 0.5 * times * apply((log(beta.d, 10) - log(size +
                                                        beta.d, 10)), 1, sum)
    MB4 <- sapply(1:G, function(x) tr(matrix(total[, x],
                                             ncol = times) - matrix(within[, x], ncol = times) +
                                        matrix(M[, x], ncol = times) - matrix(M1[, x], ncol = times)) -
                    log(10))
    MB <- MB1 + MB2 + MB3 + MB4
  }
  if (nu < tol & nu >= 0) {
    MB1 <- log(p, 10) - log(1 - p, 10)
    MB2 <- 0.5 * times * (log(N + beta, 10) - log(beta, 10))
    MB3 <- 0.5 * times * apply((log(beta.d, 10) - log(size +
                                                        beta.d, 10)), 1, sum)
    MB4 <- sapply(1:G, function(x) 0.5 * N[x] * (log(det(matrix(total[,
                                                                      x], ncol = times) + matrix(M[, x], ncol = times)),
                                                     10) - log(det(matrix(within[, x], ncol = times) +
                                                                     matrix(M1[, x], ncol = times)), 10)))
    MB <- MB1 + MB2 + MB3 + MB4
  }
  
  names(MB) <- rownames(object);
  MB <- round(sort(MB, decreasing = TRUE),5);
  MB <- as.matrix(MB, ncol=1);
  colnames(MB) <- c("MB-statistics");
  MB;
}

#'Plot the variable across time points (x)
#'@description Colored by experimental conditions, used in higher function 
#'@param mSetObj Input name of the created mSet Object
#'@param varName Input the name of the variable
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
plotProfile <-function (mSetObj=NA, varName) {
  
  mSetObj <- .get.mSet(mSetObj);
  varInx <- colnames(mSetObj$dataSet$norm) == varName;
  var <- mSetObj$dataSet$norm[,varInx];
  
  time.fac <- mSetObj$dataSet$time.fac;
  exp.fac <- mSetObj$dataSet$exp.fac;
  
  cols <- (1:length(levels(exp.fac))) + 1;
  
  # fold the var into a matrix with
  # each row contains values for one time poinst
  tpNum <- length(levels(time.fac));
  colVec <- NULL;
  lvlVec <- NULL;
  varMat <- NULL;
  for(m in 1:length(levels(exp.fac))){
    lv1 <- levels(exp.fac)[m];
    expInx <- exp.fac == lv1;
    expLen <- sum(expInx);
    colNum <- expLen/tpNum;
    subMat <- matrix(0, nrow=tpNum, ncol=colNum);
    subCol <- rep(cols[m], colNum);
    subLvl <- rep(lv1, colNum);
    for(i in 1:length(levels(time.fac))){
      lv2 <- levels(time.fac)[i];
      timeInx <- time.fac == lv2;
      hitInx <- expInx & timeInx;
      subMat[i,] <- var[hitInx];
    }
    varMat <- cbind(varMat, subMat);
    colVec <- c(colVec, subCol);
    lvlVec <- c(lvlVec, subLvl);
  }
  
  # increase y lim for legend
  ylim <- GetExtendRange(range(varMat));
  
  # note, matplot plot each col, need to transpose
  matplot(varMat, type="b", pch=19, lty=1, col=colVec,ylim=ylim,
          xlab="Time", ylab="Abundance",
          main=varName,axes=FALSE);
  
  #time.lbl <- unique(as.character(time.fac));
  time.lbl <- unique(levels(time.fac));
  
  legend("top", legend=unique(lvlVec), horiz=TRUE, lty=1, bty="n", col=unique(colVec));
  
  axis(1, label=time.lbl, at=1:length(time.lbl));
  axis(2);
  box();
}

# Used in higher function 
matrix.cov <- function(x, k, trans=TRUE, c.grp=NULL, use="complete.obs")
{
  # x is already sorted by c.grp, n.grp and k.grp
  if(!is.numeric(x))  x <- as.numeric(x)
  if(missing(k)) stop("The number of time points is missing.")
  
  if(length(unique(c.grp))==1)
  {
    res <-cov(matrix(x, byrow=TRUE, ncol=k),use=use)
    
    if(trans)
    {
      OT1 <- ot.helmert(k)[2:k,]
      if(k==2) OT1 <- t(as.matrix(OT1))
      res <- OT1%*%res%*%t(OT1)
    }
    
  }
  
  if(length(unique(c.grp))>1 & !trans)
  {
    D <- length(unique(c.grp))
    size <- max.size <- NULL
    for(i in 1:D)
    {
      grp.indx <- c.grp==sort(unique(c.grp))[i]
      if(k==1) size[i] <- sum(apply(t(as.matrix(!apply(matrix(x[grp.indx], byrow=TRUE,ncol=k),1, is.na))),2,sum)==k)
      if(k>1) size[i] <- sum(apply((!apply(matrix(x[grp.indx], byrow=TRUE,ncol=k),1, is.na)),2,sum)==k)
      max.size[i] <- sum(grp.indx)/k
    }
    cumsize <- cumsum(max.size)
    cumsize1 <- cumsum(size)
    res1 <- as.list(NULL)
    for(i in 1:D)
    {
      if(size[i]==1) res1[[i]] <- matrix(0,k,k)
      if(i==1 & size[i]>1) res1[[i]] <- cov(matrix(x[1:(k*cumsize[1])], byrow=TRUE, ncol=k),use=use)
      if(i>1& size[i]>1) res1[[i]] <- cov(matrix(x[(k*cumsize[i-1]+1):(k*cumsize[i])], byrow=TRUE, ncol=k),use=use)
    }
    
    if(k>1) res <- matrix(apply(sapply(1:D, function(x) (size[x]-1)*res1[[x]]),1,sum),ncol=k)/(cumsize1[D]-D)
    if(k==1) res <- matrix(sum(sapply(1:D, function(x) (size[x]-1)*res1[[x]])),ncol=k)/(cumsize1[D]-D)
  }
  res
}

# Used in higher function 
squeezeVar <- function(var, df){
  n <- length(var)
  if(n == 0) stop("var is empty")
  if(n == 1) return(list(var.post=var,var.prior=var,df.prior=0))
  if(length(df)==1) {
    df <- rep.int(df,n)
  } else {
    if(length(df) != n) stop("lengths differ")
  }
  out <- fitFDist(var, df1=df)
  if(is.null(out$df2) || is.na(out$df2)) stop("Could not estimate prior df")
  out$var.prior <- out$scale
  out$df.prior <- out$df2
  out$df2 <- out$scale <- NULL
  df.total <- df + out$df.prior
  if(out$df.prior == Inf)
    out$var.post <- rep.int(out$var.prior,n)
  else {
    var[df==0] <- 0 # guard against missing or infinite values
    out$var.post <- (df*var + out$df.prior*out$var.prior) / df.total
  }
  out
}

# Used in higher function 
fitFDist <- function(x, df1) {
  #	Moment estimation of the parameters of a scaled F-distribution
  #	The first degrees of freedom is given
  #	Gordon Smyth
  #	8 Sept 2002.  Last revised 6 April 2006.
  
  #	Remove missing or infinite values and zero degrees of freedom
  o <- is.finite(x) & is.finite(df1) & (x >= 0) & (df1 > 0)
  if(any(!o)) {
    x <- x[o]
    df1 <- df1[o]
  }
  n <- length(x)
  if(n==0) return(list(scale=NA,df2=NA))
  
  #	Avoid exactly zero values
  m <- median(x)
  if(m==0) {
    warning("More than half of residual variances are exactly zero: eBayes unreliable")
    m <- 1
  } else {
    if(any(x==0)) warning("Zero sample variances detected, have been offset",call.=FALSE)
  }
  x <- pmax(x, 1e-5 * median(x))
  
  #	Better to work on with log(F)
  z <- log(x)
  e <- z-digamma(df1/2)+log(df1/2)
  emean <- mean(e)
  evar <- mean(n/(n-1)*(e-emean)^2-trigamma(df1/2))
  if(evar > 0) {
    df2 <- 2*trigammaInverse(evar)
    s20 <- exp(emean+digamma(df2/2)-log(df2/2))
  } else {
    df2 <- Inf
    s20 <- exp(emean)
  }
  list(scale=s20,df2=df2)
}

# Used in higher function 
trigammaInverse <- function(x) {
  #	Solve trigamma(y) = x for y
  #	Gordon Smyth
  #	8 Sept 2002.  Last revised 12 March 2004.
  
  #	Non-numeric or zero length input
  if(!is.numeric(x)) stop("Non-numeric argument to mathematical function")
  if(length(x)==0) return(numeric(0))
  
  #	Treat out-of-range values as special cases
  omit <- is.na(x)
  if(any(omit)) {
    y <- x
    if(any(!omit)) y[!omit] <- Recall(x[!omit])
    return(y)
  }
  omit <- (x < 0)
  if(any(omit)) {
    y <- x
    y[omit] <- NaN
    warning("NaNs produced")
    if(any(!omit)) y[!omit] <- Recall(x[!omit])
    return(y)
  }
  omit <- (x > 1e7)
  if(any(omit)) {
    y <- x
    y[omit] <- 1/sqrt(x[omit])
    if(any(!omit)) y[!omit] <- Recall(x[!omit])
    return(y)
  }
  omit <- (x < 1e-6)
  if(any(omit)) {
    y <- x
    y[omit] <- 1/x[omit]
    if(any(!omit)) y[!omit] <- Recall(x[!omit])
    return(y)
  }
  
  #	Newton's method
  #	1/trigamma(y) is convex, nearly linear and strictly > y-0.5,
  #	so iteration to solve 1/x = 1/trigamma is monotonically convergent
  y <- 0.5+1/x
  iter <- 0
  repeat {
    iter <- iter+1
    tri <- trigamma(y)
    dif <- tri*(1-tri/x)/psigamma(y,deriv=2)
    y <- y+dif
    if(max(-dif/y) < 1e-8) break
    if(iter > 50) {
      warning("Iteration limit exceeded")
      break
    }
  }
  y
}

# Used in higher function 
mb.2D <- function(object, k, mn, c.grp, nu=NULL, Lambda=NULL, eta=NULL, k.grp=NULL,
                  mn.grp=NULL, r=FALSE, vec=FALSE, d=NULL, prop=0.02, T2.only=TRUE)
{
  
  M <- as.matrix(object);
  
  G <- nrow(M)

  if(!is.null(mn))
  {
    max.mn1 <- max(mn[,1])
    max.mn2 <- max(mn[,2])
    if(ncol(mn) != length(unique(c.grp))) stop("The sample sizes are incorrect!")
    if(((max.mn1*k) != sum(c.grp==sort(unique(c.grp))[1])) || ((max.mn2*k) != sum(c.grp==sort(unique(c.grp))[2])))
      stop("The sample sizes or the biological condition group assignments are incorrect!")
  }
  
  if(is.null(k.grp))
  {
    k.grp <- rep(1:k, ncol(M)/k)
    cat("Time group assignments are set to default.","\n")
  }
  if(length(unique(k.grp)) != k) stop("The number of time points or the time group assignments are
                                      incorrect")
  if(is.null(mn.grp))
  {
    mn.grp <- rep(1:(ncol(M)/k), rep(k, ncol(M)/k))
    cat("Replicate group assignments are set to default.","\n")
  }
  
  
  mydata <- M
  indx <- order(c.grp, mn.grp, k.grp)
  M <- M[,indx]
  
  mis <- apply(!apply(M, 1, is.na), 2, sum)
  mis <- sum((mis/k-floor(mis/k)) !=0)
  if(mis>0) stop(mis, " metabolites may have within replicate missing values.")
  
  
  N <- apply(mn,1,sum)
  
  ## sample sizes across genes are the same
  ## only need to input the common sample size
  Sp <- apply(M, 1, matrix.cov, k, trans=FALSE, c.grp=c.grp)
  diagSp <- apply(Sp, 2, function(x) diag(matrix(x,ncol=k)))
  Sp.avg <- matrix(apply(Sp, 1, mean, na.rm=TRUE),ncol=k)
  
  if(is.null(nu)||is.null(Lambda))
  {
    
    nu.lim <- k+6
    
    if(!is.null(nu))
    {
      nu0 <- nu
      nu <- max(nu0, nu.lim)
      if(is.infinite(nu)& is.null(Lambda))  {Lambda <- Sp.avg}
      if(is.finite(nu)& is.null(Lambda))    {Lambda <- (nu-k-1)*Sp.avg/nu}
      nu<- nu0
    }
    
    if(is.null(nu))
    {
      nu0 <- mean(sapply(1:k, function(x) squeezeVar(diagSp[x,], N-2)$df.prior))
      nu <- max(nu0, nu.lim)
      if(is.infinite(nu)& is.null(Lambda))  {Lambda <- Sp.avg}
      if(is.finite(nu)& is.null(Lambda))    {Lambda <- (nu-k-1)*Sp.avg/nu}
      nu<- nu0
    }
  }
  
  max.n1 <- max(mn[,1])
  max.n2 <- max(mn[,2])
  xbar1 <- apply(M[, 1:(k*max.n1)], 1, function(x)
    apply(matrix(x,  byrow=FALSE, ncol=max.n1),1,mean,na.rm=TRUE))
  xbar2 <- apply(M[, -c(1:(k*max.n1))], 1, function(x)
    apply(matrix(x,  byrow=FALSE, ncol=max.n2),1,mean,na.rm=TRUE))
  if(r)
  {
    e1 <- sapply(1:G, function(x) matrix(M[x, 1:(k*max.n1)], byrow=FALSE, ncol=max.n1)-xbar1[,x])
    tune1 <- sapply(1:G, function(x) d * median(abs(e1[, x]),na.rm = TRUE)/0.6745)
    indx1 <- sapply(1:G, function(x) abs(e1[, x]) > tune1[x])
    na.indx1 <- sapply(1:G, function(x) c(1:(k * max.n1))[!is.na(indx1[,x])])
    wt1 <- matrix(rep(1, G * max.n1 * k), ncol = max.n1 * k)
    wt1 <- t(sapply(1:G, function(x) {wt1[x,][-na.indx1[,x]] <- 0
    wt1[x,]}))
    wt1 <- t(sapply(1:G, function(x) {wt1[x, ][na.indx1[,x]][indx1[,x][na.indx1[,x]]] <-
      tune1[x]/abs(e1[,x][na.indx1[,x]][indx1[,x][na.indx1[,x]]])
    wt1[x,]}))
    totalw1 <- sapply(1:G, function(x) apply(matrix(wt1[x,], byrow=FALSE, ncol=max.n1), 1, sum,
                                             na.rm=TRUE))
    w1 <- sapply(1:G, function(x) matrix(wt1[x,], byrow=FALSE, ncol=max.n1)/totalw1[,x])
    xbar1 <- sapply(1:G, function(x)
      apply(matrix(w1[, x] * M[x, 1:(k*max.n1)], byrow = FALSE, ncol = max.n1),1, sum,na.rm=TRUE))
    
    
    e2 <- sapply(1:G, function(x) matrix(M[x, -(1:(k*max.n1))], byrow=FALSE, ncol=max.n2)-xbar2[,x])
    tune2 <- sapply(1:G, function(x) d * median(abs(e2[, x]),na.rm = TRUE)/0.6745)
    indx2 <- sapply(1:G, function(x) abs(e2[, x]) > tune2[x])
    na.indx2 <- sapply(1:G, function(x) c(1:(k * max.n2))[!is.na(indx2[,x])])
    wt2 <- matrix(rep(1, G * max.n2 * k), ncol = max.n2 * k)
    wt2 <- t(sapply(1:G, function(x) {wt2[x,][-na.indx2[,x]] <- 0
    wt2[x,]}))
    wt2 <- t(sapply(1:G, function(x) {wt2[x, ][na.indx2[,x]][indx2[,x][na.indx2[,x]]] <-
      tune2[x]/abs(e2[,x][na.indx2[,x]][indx2[,x][na.indx2[,x]]])
    wt2[x,]}))
    totalw2 <- sapply(1:G, function(x) apply(matrix(wt2[x,], byrow=FALSE, ncol=max.n2), 1, sum,
                                             na.rm=TRUE))
    w2 <- sapply(1:G, function(x) matrix(wt2[x,], byrow=FALSE, ncol=max.n2)/totalw2[,x])
    xbar2 <- sapply(1:G, function(x)
      apply(matrix(w2[, x] * M[x, -(1:(k*max.n1))], byrow = FALSE, ncol = max.n2),1, sum,na.rm=TRUE))
    wt <- cbind(wt1, wt2)
  }
  
  
  X <- xbar1-xbar2
  tol <- .Machine$double.eps
  if(is.finite(nu) & nu > tol)
  {
    modSp <- sapply(1:G, function(x) ((N[x]-2)*matrix(Sp[,x],ncol=k)+nu*Lambda)/(N[x]-2+nu))
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modSp <- apply(modSp, 2, function(x)
        svd(matrix(x,ncol=k))$u%*%diag(sqrt(svd(matrix(x,ncol=k))$d))%*%t(svd(matrix(x,ncol=k))$v))
      modt <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1/2)*solve(matrix(sqrt.modSp[,x],ncol=k))%*%X[,x])
      HotellingT2 <- apply(modt, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1)*t(X[,x])%*%solve(matrix(modSp[,x],ncol=k))%*%X[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
  }
  
  if(is.infinite(nu))
  {
    modSp <- sapply(1:G, function(x) Lambda)
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modSp <- apply(modSp, 2, function(x)
        svd(matrix(x,ncol=k))$u%*%diag(sqrt(svd(matrix(x,ncol=k))$d))%*%t(svd(matrix(x,ncol=k))$v))
      modt <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1/2)*solve(matrix(sqrt.modSp[,x],ncol=k))%*%X[,x])
      HotellingT2 <- apply(modt, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1)*t(X[,x])%*%solve(matrix(modSp[,x],ncol=k))%*%X[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
  }
  if(nu < tol & nu >=0)
  {
    modSp <- Sp
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modSp <- apply(modSp, 2, function(x)
        svd(matrix(x,ncol=k))$u%*%diag(sqrt(svd(matrix(x,ncol=k))$d))%*%t(svd(matrix(x,ncol=k))$v))
      modt <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1/2)*ginv(matrix(sqrt.modSp[,x],ncol=k))%*%X[,x])
      HotellingT2 <- apply(modt, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x)
        (mn[x,1]^(-1)+mn[x,2]^(-1))^(-1)*t(X[,x])%*%ginv(matrix(modSp[,x],ncol=k))%*%X[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
  }
  if(is.null(eta)& (T2.only==FALSE)) eta <- mean(sapply(1:k, function(x) tmixture.matrix(modt[x,],
                                                                                         sqrt(mn[,1]^(-1)+mn[,2]^(-1)), N+nu-2, prop, c(0.1, 4)^2/Lambda[x,x]))^(-1))
  
  
  if(nu < 0) stop("The estimation of prior degrees of freedom <0 !")
  
  names(HotellingT2) <- rownames(object);
  HotellingT2 <- round(sort(HotellingT2, decreasing = TRUE),5);
  HotellingT2 <- as.matrix(HotellingT2, ncol=1);
  colnames(HotellingT2) <- c("Hotelling-T2");
  HotellingT2;
}

# Used in higher function 
mb.1D <- function(object, k, n, nu=NULL, Lambda1=NULL, eta=NULL, k.grp=NULL, n.grp=NULL, r=FALSE, vec=FALSE, d=NULL, prop=0.01, T2.only=TRUE) {
  
  res <- as.list(NULL)
  M <- as.matrix(object)
  
  G <- nrow(M)
  max.n <- max(n)
  
  if(is.null(k.grp)){
    k.grp <- rep(1:k, max.n)
    cat("Time group assignments are set to default.","\n")
  }
  
  if(is.null(n.grp)){
    n.grp <- rep(c(1:max.n), rep(k, max.n))
    cat("Replicate group assignments are set to default.", "\n")
  }
  
  mydata <- M
  indx <- order(n.grp, k.grp)
  M <- M[,indx]
  
  mis <- apply(!apply(M, 1, is.na), 2, sum)
  mis <- sum((mis/k-floor(mis/k)) !=0)
  if(mis>0) stop(mis, " genes may have missing values in some of the replicates.")
  
  
  if((max.n*k) !=ncol(M)) stop("The number of time points or sample sizes are incorrect.")
  if(length(unique(n.grp)) != max.n) stop("Sample sizes or replicate group assignments is incorrect.")   
  c.grp <- rep(1, k*max.n)
  
  S1 <- apply(M, 1, matrix.cov, k, c.grp=c.grp)
  if(is.null(nu)) diagS1 <- apply(S1, 2, function(x) diag(matrix(x,ncol=k-1)))
  S1.avg <- matrix(apply(S1, 1, mean, na.rm=TRUE),ncol=k-1)
  
  
  if(is.null(nu)||is.null(Lambda1)){
    nu.lim <- k-1+6
    
    
    if(!is.null(nu))
    {
      nu0 <- nu
      nu <- max(nu0, nu.lim)
      if(is.infinite(nu) & is.null(Lambda1))  {Lambda1 <- S1.avg} 
      if(is.finite(nu)& is.null(Lambda1))    {Lambda1 <- (nu-k)*S1.avg/nu }
      nu<- nu0
    }
    
    if(is.null(nu))
    {
      nu0 <- mean(sapply(1:(k-1), function(x) squeezeVar(diagS1[x,], n-1)$df.prior))
      nu <- max(nu0, nu.lim)
      if(is.infinite(nu)& is.null(Lambda1))  {Lambda1 <- S1.avg} 
      if(is.finite(nu)& is.null(Lambda1))    {Lambda1 <- (nu-k)*S1.avg/nu }
      nu<- nu0
    }
  }
  
  max.n <- max(n)
  T1 <- ot.helmert(k)[2:k,]
  xbar <- apply(M, 1, function(x) apply(matrix(x,  byrow=FALSE, ncol=max.n),1,mean,na.rm=TRUE))
  
  if(r){    
    e <- sapply(1:G, function(x) matrix(M[x,], byrow=FALSE, ncol=max.n)-xbar[,x])
    tune <- sapply(1:G, function(x) d * median(abs(e[, x]),na.rm = TRUE)/0.6745)
    indx <- sapply(1:G, function(x) abs(e[, x]) > tune[x])
    na.indx <- sapply(1:G, function(x) c(1:(k * max.n))[!is.na(indx[,x])])
    wt <- matrix(rep(1, G * max.n * k), ncol = max.n * k)
    wt <- t(sapply(1:G, function(x) {wt[x,][-na.indx[,x]] <- 0 
    wt[x,]}))        
    wt <- t(sapply(1:G, function(x) {wt[x, ][na.indx[,x]][indx[,x][na.indx[,x]]] <- 
      tune[x]/abs(e[,x][na.indx[,x]][indx[,x][na.indx[,x]]])
    wt[x,]}))
    totalw <- sapply(1:G, function(x) apply(matrix(wt[x,], byrow=FALSE, ncol=max.n), 1, sum, 
                                            na.rm=TRUE))
    w <- sapply(1:G, function(x) matrix(wt[x,], byrow=FALSE, ncol=max.n)/totalw[,x])
    xbar <- sapply(1:G, function(x) 
      apply(matrix(w[, x] * M[x, ], byrow = FALSE, ncol = max.n),1, sum,na.rm=TRUE));       
  }
  
  tol <- .Machine$double.eps
  if(is.finite(nu) & nu > tol) 
  {
    modS1 <- sapply(1:G, function(x) ((n[x]-1)*matrix(S1[,x],ncol=k-1)+nu*Lambda1)/(n[x]-1+nu))
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modS1 <- apply(modS1, 2, function(x)
        svd(matrix(x,ncol=k-1))$u%*%diag(sqrt(svd(matrix(x,ncol=k-1))$d))%*%t(svd(matrix(x,ncol=k-1))$v))
      modt1 <- sapply(1:G, function(x) n[x]^(1/2)*solve(matrix(sqrt.modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
      HotellingT2 <- apply(modt1, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x) 
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
    
  }
  
  if(is.infinite(nu))
  {
    modS1 <- sapply(1:G, function(x) Lambda1)
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modS1 <- apply(modS1, 2, function(x)
        svd(matrix(x,ncol=k-1))$u%*%diag(sqrt(svd(matrix(x,ncol=k-1))$d))%*%t(svd(matrix(x,ncol=k-1))$v))
      modt1 <- sapply(1:G, function(x) n[x]^(1/2)*solve(matrix(sqrt.modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
      HotellingT2 <- apply(modt1, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
  }
  if(nu < tol & nu >=0)
  {
    modS1 <- S1
    if(is.null(eta) & (T2.only==FALSE))
    {
      sqrt.modS1 <- apply(modS1, 2, function(x)
        svd(matrix(x,ncol=k-1))$u%*%diag(sqrt(svd(matrix(x,ncol=k-1))$d))%*%t(svd(matrix(x,ncol=k-1))$v))
      modt1 <- sapply(1:G, function(x) n[x]^(1/2)*ginv(matrix(sqrt.modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
      HotellingT2 <- apply(modt1, 2, function(x) t(x)%*%x)
    }
    if(T2.only)
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%ginv(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
    
    if(!is.null(eta) & (T2.only==FALSE))
      HotellingT2 <- sapply(1:G, function(x)
        n[x]*t(T1%*%xbar[,x])%*%solve(matrix(modS1[,x],ncol=k-1))%*%T1%*%xbar[,x])
  }
  
  
  # HotellingT2 does not require eta
  if(is.null(eta) & (T2.only==FALSE)) eta <- mean(sapply(1:(k-1), function(x) tmixture.matrix(modt1[x,], 
                                                                                              sqrt(1/n), n+nu-1, prop, c(0.1, 4)^2/Lambda1[x,x]))^(-1))
  
  res$M <- mydata
  res$prop <- prop
  res$nu <- nu
  res$Lambda1 <- Lambda1 
  res$eta <- eta
  
  
  if(nu < 0) stop("The degrees of moderation <0 !")
  
  tol <- .Machine$double.eps
  if(is.finite(nu) & nu > tol)
  {
    MB <- NULL
    if(T2.only==FALSE)
    {
      MB1 <- log(prop,10)-log(1-prop,10)
      MB2 <- ((k-1)/2)*log((eta/(n+eta)),10)
      MB3 <- ((n+nu)/2)*log(((n-1+nu+HotellingT2)/(n-1+nu+(eta/(n+eta))*HotellingT2)), 10)
      MB <- MB1+MB2+MB3
    }
    unique.n <- unique(n)
    res$percent <- matrix(c(unique.n, round(100*nu/(nu+unique.n-1))), ncol=2)
    res$size <- n
    res$con.group <- rep(1, k*max.n)
    res$rep.group <- n.grp
    res$time.group <- k.grp
    if(r) res$weights <- wt
    if(vec) res$modt1 <- modt1
    res$HotellingT2 <- HotellingT2 
    res$MB <- MB
  }
  
  if(is.infinite(nu)){
    MB <- NULL
    if(T2.only==FALSE)
    {
      MB1 <- log(prop,10)-log(1-prop,10)
      MB2 <- ((k-1)/2)*log((eta/(n+eta)),10)
      MB3 <- 0.5*(n/(n+eta))*HotellingT2-log(10)
      MB <- MB1+MB2+MB3
    }
    
    unique.n <- unique(n)
    res$percent <- matrix(c(unique.n, 100), ncol=2)
    res$size <- n
    res$con.group <- rep(1, k*max.n)
    res$rep.group <- n.grp
    res$time.group <- k.grp
    if(r) res$weights <- wt
    if(vec) res$modt1 <- modt1
    res$HotellingT2 <- HotellingT2 
    res$MB <- MB
  }
  
  if(nu < tol & nu >=0){
    MB <- NULL
    if(T2.only==FALSE)
    {
      MB1 <- log(prop,10)-log(1-prop,10)
      MB2 <- ((k-1)/2)*log((eta/(n+eta)),10)
      MB3 <- (n/2)*log(((n-1+HotellingT2)/(n-1+(eta/(n+eta))*HotellingT2)), 10)
      MB <- MB1+MB2+MB3
    }
    
    unique.n <- unique(n)
    res$percent <- matrix(c(unique.n, 0), ncol=2)
    res$size <- n
    res$con.group <- rep(1, k*max.n)
    res$rep.group <- n.grp
    res$time.group <- k.grp
    if(r) res$weights <- wt
    if(vec) res$modt1 <- modt1
    res$HotellingT2 <- HotellingT2 
    res$MB <- MB
  }
  
  names(HotellingT2) <- rownames(object);
  HotellingT2 <- round(sort(HotellingT2, decreasing = TRUE),5);
  HotellingT2 <- as.matrix(HotellingT2, ncol=1);
  colnames(HotellingT2) <- c("Hotelling-T2");
  HotellingT2;
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetMBSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$MB$stats));
}

GetMBSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$analSet$MB$stats));
}

GetMBSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$analSet$MB$stats));
}

#'Sig table for MB analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.MB<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$MB$stats, "MEBA", mSetObj$dataSet$type);
}

######### Utility Functions ###########
ot.helmert <- function(k){
  
  if(missing(k)) stop("The number of time points is missing.")
  
  if (is.numeric(k) && length(k) == 1)
    if(k > trunc(k)) stop("The number of time points is not an integer.")
  
  
  levels <- 1:k
  
  T0 <- matrix(rep(1/sqrt(k), k), byrow=TRUE, ncol=k)
  
  T1 <- matrix(rep(0,(k-1)*k), ncol=k, byrow=TRUE)
  T1 <- array(1/sqrt(diag(outer(row(T1)[,1]+1, row(T1)[,1], "*"))),c(k-1,k))
  T1[col(T1) > row(T1)] <- 0
  T1[col(T1) == row(T1)+1] <- -(row(T1)[,1])/sqrt(diag(outer(row(T1)[,1]+1, row(T1)[,1], "*")))
  
  OT <- rbind(T0, T1)
  
  OT
}