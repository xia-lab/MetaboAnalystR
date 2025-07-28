my.time.mb.manova <- function (object, times, D, size, nu = NULL, Lambda = NULL, beta.d = NULL,
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
  mis <- colSums(!apply(M, 1, is.na));
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
  indx1 <- simple.rank<=G*p;

  if(sum(indx1)==0){ # none pass the test => then use top 5
    indx1 <- simple.rank<=5;
  }

  xbar <- sapply(1:G, function(x) apply(matrix(M[x, ], byrow = TRUE, ncol = times), 2, mean, na.rm = TRUE))
  if (is.null(alpha.d)){
    alpha.d <- sapply(1:D, function(x) apply(xbar.d[[x]][,indx1],1, mean, na.rm = TRUE));
  }
  if (is.null(alpha))
    alpha <- apply(xbar[,!indx1], 1, mean, na.rm = TRUE)
  
  
  if (is.null(beta.d) || is.null(beta)) {
    U.d <- lapply(1:D, function(x) apply(xbar.d[[x]][,indx1] - alpha.d[,x], 2, function(y) y %*% t(y)));
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
    MB3 <- 0.5 * times * rowSums(log(beta.d, 10) - log(size + beta.d, 10));
    MB4 <- sapply(1:G, function(x) 0.5 * (N[x] + nu) * (log(det(matrix(total[,
                                                                             x], ncol = times) + matrix(M[, x], ncol = times) +
                                                                  nu * Lambda), 10) - log(det(matrix(within[, x], ncol = times) +
                                                                                                matrix(M1[, x], ncol = times) + nu * Lambda), 10)))
    MB <- MB1 + MB2 + MB3 + MB4
  }
  if (is.infinite(nu)) {
    MB1 <- log(p, 10) - log(1 - p, 10)
    MB2 <- 0.5 * times * (log(N + beta, 10) - log(beta, 10))
    MB3 <- 0.5 * times * rowSums(log(beta.d, 10) - log(size + beta.d, 10));
    MB4 <- sapply(1:G, function(x) tr(matrix(total[, x],
                                             ncol = times) - matrix(within[, x], ncol = times) +
                                        matrix(M[, x], ncol = times) - matrix(M1[, x], ncol = times)) -
                    log(10))
    MB <- MB1 + MB2 + MB3 + MB4
  }
  if (nu < tol & nu >= 0) {
    MB1 <- log(p, 10) - log(1 - p, 10)
    MB2 <- 0.5 * times * (log(N + beta, 10) - log(beta, 10))
    MB3 <- 0.5 * times * rowSums(log(beta.d, 10) - log(size + beta.d, 10));
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