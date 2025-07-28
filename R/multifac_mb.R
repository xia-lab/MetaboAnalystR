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
  
  if(length(meta.vec.mb) == 0){
    sel.meta.df <- mSetObj$dataSet$meta.info[, c(1,2)]
    mSetObj$dataSet$exp.fac <- sel.meta.df[,2]
    mSetObj$dataSet$time.fac <- sel.meta.df[,1]
    meta.vec.mb <- colnames(sel.meta.df);
  }else{
    sel.meta.df <- mSetObj$dataSet$meta.info[, meta.vec.mb]
    mSetObj$dataSet$exp.fac <- sel.meta.df[,-(which(tolower(colnames(sel.meta.df)) == "time"))]
    mSetObj$dataSet$time.fac <- sel.meta.df[,which(tolower(colnames(sel.meta.df)) == "time")]
} 
    
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
  fast.write.csv(signif(MB.stats, 5), file="meba_sig_features.csv");
  mSetObj$analSet$MB <-list(stats=MB.stats, selected.meta=meta.vec.mb);
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
#'@param version image mark
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotMBTimeProfile <- function(mSetObj=NA, cmpdNm, version, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName <- mSetObj$dataSet$url.var.nms[cmpdNm];
  imgName <- paste(imgName, "_", version, "_dpi", dpi, ".", format, sep="");

  # adjust width based on the time points
  time.len <- length(levels(mSetObj$dataSet$time.fac));
  if(is.na(width)){
    w <- 5.0 + time.len/4; # 4 time points per inch? 
  }else if(width == 0){
    w <- 5.4;
  }else{
    w <- width;
  }
  h <- min(5.4, w);
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
   if(.on.public.web){
    # make this lazy load
    if(!exists("my.time.mb.manova")){ # public web on same user dir
      .load.scripts.on.demand("util_multifac_mb_manova.Rc");    
    }
    return(my.time.mb.manova(object, times, D, size, nu, Lambda, beta.d, beta, alpha.d, alpha, condition.grp, time.grp, rep.grp, p));
  }else{
    return(my.time.mb.manova(object, times, D, size, nu, Lambda, beta.d, beta, alpha.d, alpha, condition.grp, time.grp, rep.grp, p));
  }
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

  cols <- unique(GetColorSchema(exp.fac));

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
  if(!is.null(lvlVec)){
    lgd <- unique(lvlVec);
    if(length(lgd)>0){
        legend("top", legend=lgd, horiz=TRUE, lty=1, bty="n", col=unique(colVec));
    }
  }
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
      if(k==1) size[i] <- sum(colSums(t(as.matrix(!apply(matrix(x[grp.indx], byrow=TRUE,ncol=k),1, is.na))))==k)
      if(k>1) size[i] <- sum(colSums((!apply(matrix(x[grp.indx], byrow=TRUE,ncol=k),1, is.na)))==k)
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
  
   if(.on.public.web){
    # make this lazy load
    if(!exists("my.time.mb.2d")){ # public web on same user dir
      .load.scripts.on.demand("util_multifac_mb_2d.Rc");    
    }
    return(my.time.mb.2d(object, k, mn, c.grp, nu, Lambda, eta, k.grp, mn.grp, r, vec, d, prop, T2.only));
  }else{
    return(my.time.mb.2d(object, k, mn, c.grp, nu, Lambda, eta, k.grp, mn.grp, r, vec, d, prop, T2.only));
  }
}

# Used in higher function 
mb.1D <- function(object, k, n, nu=NULL, Lambda1=NULL, eta=NULL, k.grp=NULL, n.grp=NULL, r=FALSE, vec=FALSE, d=NULL, prop=0.01, T2.only=TRUE) {
    if(.on.public.web){
    # make this lazy load
    if(!exists("my.time.mb.1d")){ # public web on same user dir
      .load.scripts.on.demand("util_multifac_mb_1d.Rc");    
    }
    return(my.time.mb.1d(object, k, n, nu, Lambda1, eta, k.grp, n.grp, r, vec, d, prop, T2.only));
  }else{
    return(my.time.mb.1d(object, k, n, nu, Lambda1, eta, k.grp, n.grp, r, vec, d, prop, T2.only));
  }

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