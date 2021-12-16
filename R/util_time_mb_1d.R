my.time.mb.1d <- function(object, k, n, nu=NULL, Lambda1=NULL, eta=NULL, k.grp=NULL, n.grp=NULL, r=FALSE, vec=FALSE, d=NULL, prop=0.01, T2.only=TRUE) {
  
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
