my.time.mb.2d <- function(object, k, mn, c.grp, nu=NULL, Lambda=NULL, eta=NULL, k.grp=NULL,
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