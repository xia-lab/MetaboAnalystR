## This script is sourced from SSPA package as it is not available from CRAN
## Reference: van Iterson, M., Hoen ', A. P, Pedotti, P., Hooiveld, J. G, den Dunnen, T. J, van Ommen, J. G, Boer, 
## M. J, Menezes, X. R (2009). “Relative power and sample size analysis on gene expression profiling data.” BMC Genomics, 
## 10, 439. http://www.biomedcentral.com/1471-2164/10/439.

## Set Classes
setClass("Distribution",
         representation(distribution = "character",
                        args         = "list"),
         prototype(distribution = character(1),
                   args         = list())
         )

setClass("PilotData", 
         representation(statistics   = "numeric",
                        samplesize   = "numeric",
                        pvalues      = "numeric"),
         prototype(statistics   = numeric(1),
                   samplesize   = numeric(1),
                   pvalues      = numeric(1)),
         contains = ("Distribution"),
         validity = function(object){
           if(any(is.na(object@statistics)))
             return("Test statistics contains missing values, not allowed!")           
           else if(object@samplesize < 0)
             return("Sample size should be large then 0!")
           else
             return(TRUE)}
         )

setClass("SampleSize",
         representation("PilotData",
                        pi0           = "numeric",
                        lambda        = "numeric",
                        theta         = "numeric",
                        control = "list",
                        info    = "list"),
         prototype(PilotData     = "PilotData",
                   pi0           = numeric(1),
                   lambda        = numeric(1),
                   theta         = numeric(1),
                   control = list(),
                   info    = list())
         )


### Set method
#setGeneric("pvalue", function(object) { standardGeneric ("pvalue") })
pvalue <-  function(object){
  distribution <- object@distribution
  if(distribution == "norm" | distribution == "t")
    return(function(x) 2*(1 - pf0(object)(abs(x)))) ##two-sided
  else
    return(function(x) 1 - pf0(object)(x)) ##one-sided
}

setGeneric("pf0", function(object) { standardGeneric ("pf0") })
setMethod("pf0","Distribution", function(object){
  distribution <- object@distribution
  args <- object@args
  pf0 <- switch(object@distribution,
                norm = function(q) pnorm(q, mean=0, sd=1),
                t = function(q, dof = args$df) pt(q, df=dof, ncp=0),
                f = function(q, dof1 = args$df1, dof2 = args$df2) pf(q, df1=dof1, df2=dof2, ncp=0),
                chisq = function(q, dof = args$df) pchisq(q, df=dof, ncp=0)
  )
  return(pf0)
})

setGeneric("Samplesize", function(object) { standardGeneric ("Samplesize") })
setMethod("Samplesize","PilotData", function(object){ return(object@samplesize)})

setGeneric("Pi0", function(object) { standardGeneric ("Pi0")})
setGeneric("Lambda", function(object) { standardGeneric ("Lambda")})
setGeneric("Theta", function(object) { standardGeneric ("Theta")})
setGeneric("Control", function(object) { standardGeneric ("Control")})
setGeneric("Info", function(object) { standardGeneric ("Info")})
setGeneric("df1", function(object) { standardGeneric ("df1") })

setMethod("Pi0","SampleSize", function(object){ return(object@pi0)})
setMethod("Lambda","SampleSize", function(object){ return(object@lambda)})
setMethod("Theta","SampleSize", function(object){ return(object@theta)})
setMethod("Control","SampleSize", function(object){return(object@control)})
setMethod("Info","SampleSize", function(object){ return(object@info)})
setMethod("df1","Distribution", function(object){
  distribution <- object@distribution
  args <- object@args
  df1 <- switch(object@distribution,
                norm = function(x, y) dnorm(x, mean=y, sd=1),
                t = function(x, y, dof = args$df) suppressWarnings(dt(x, df=dof, ncp=y)),
                f = function(x, y, dof1 = args$df1, dof2 = args$df2) df(x, df1=dof1, df2=dof2, ncp=y),
                chisq = function(x, y, dof = args$df) dchisq(x, df=dof, ncp=y)
  )
  return(df1)
})

SSPA_plot <- function(x, y, ...) {
  object <- x
  
  ##extract functions
  qf0 <- qf0(object)
  
  plot.line <- lattice::trellis.par.get("plot.line")
  add.line <- lattice::trellis.par.get("add.line")
  
  hstat <- histogram(Statistics(object), col = plot.line$col, border="white", breaks = "Scott", type = "density",
                     ylab="", xlab = "test statistic", main="",
                     panel = function(x, ...) {
                       panel.histogram(x, ...)
                       ##panel.mathdensity(dmath = df0(object), col=add.line$col, lwd=add.line$lwd, lty=add.line$lty) ##doesn't work?
                     })
  
  hpval <- histogram(Pvalues(object), breaks = "Scott", col = plot.line$col, border="white", type = "density",
                     ylab="", xlab = "p-value", main = "")
  
  xypval <- xyplot(seq(along = Pvalues(object))/length(Pvalues(object)) ~ sort(Pvalues(object)), type=c("l", "g"),
                   ylab="", xlab="p-value (sorted)", main="",
                   panel = function(x, y, ...){
                     panel.xyplot(x, y, ...)
                     panel.abline(a=0, b=1, col=add.line$col, lwd=add.line$lwd, lty=add.line$lty)
                   })
  
  qqstat <- qqmath(Statistics(object), type=c("p", "g"),
                   distribution = function(x) qf0(x),
                   ylab="test statistic", xlab = paste("q", distribution(object), sep=""), main="",
                   prepanel = prepanel.qqmathline,
                   panel = function(x, ...) {
                     panel.qqmathline(x, col=add.line$col, lwd=add.line$lwd, lty=add.line$lty, ...)
                     panel.qqmath(x, ...)
                   })
  
  print(hstat, split=c(1, 1, 2, 2), more=TRUE)
  print(qqstat, split=c(1, 2, 2, 2), more=TRUE)
  print(hpval, split=c(2, 1, 2, 2), more=TRUE)
  print(xypval, split=c(2, 2, 2, 2))
  
  invisible()
}

setGeneric("qf0", function(object) { standardGeneric ("qf0") })
setMethod("qf0","Distribution", function(object){
  distribution <- object@distribution
  args <- object@args
  qf0 <- switch(object@distribution,
                norm = function(p) qnorm(p, mean=0, sd=1),
                t = function(p, dof = args$df) qt(p, df=dof, ncp=0),
                f = function(p, dof1 = args$df1, dof2 = args$df2) qf(p, df1=dof1, df2=dof2, ncp=0),
                chisq = function(p, dof = args$df) qchisq(p, df=dof, ncp=0)
  )
  return(qf0)
})

setGeneric("pf1", function(object) { standardGeneric ("pf1") })
setMethod("pf1","Distribution", function(object){
  distribution <- object@distribution
  args <- object@args
  pf1 <- switch(object@distribution,
                norm = function(q, y) pnorm(q, mean=y, sd=1),
                t = function(q, y, dof = args$df) suppressWarnings(pt(q, df=dof, ncp=y)),
                f = function(q, y, dof1 = args$df1, dof2 = args$df2) pf(q, df1=dof1, df2=dof2, ncp=y),
                chisq = function(q, y, dof = args$df) pchisq(q, df=dof, ncp=y)
  )
  return(pf1)
})

setGeneric("Statistics", function(object) { standardGeneric ("Statistics") })
setMethod("Statistics","PilotData", function(object){ return(object@statistics) })
setGeneric("Pvalues", function(object) { standardGeneric ("Pvalues") })
setMethod("Pvalues","PilotData", function(object){ return(object@pvalues)})
setGeneric("distribution", function(object) { standardGeneric ("distribution") })
setMethod("distribution","PilotData", function(object){ return(object@distribution)})

### pilotData
SSPA_pilotData <- function(statistics = NULL, samplesize = NULL, distribution = c("norm", "t", "f", "chisq"), ...)
{

  distribution <- match.arg(distribution)

  args <- list(...)

  if(missing(statistics))
    stop("Test statistics missing!")

  if(missing(samplesize))
    stop("sample size missing!")

  DistObject <- new("Distribution", distribution=distribution, args=args)
  
  ##create the object
  object <- new("PilotData", statistics = unname(statistics), samplesize = unname(samplesize),
                DistObject)

  ##extract function for calculating the p-values from the Distribution object
  calcPval <- pvalue(DistObject)
  
  object@pvalues <- calcPval(unname(statistics))

  object
}

### predictpower
predictpower <- function(object, samplesizes, alpha=0.1, verbose=FALSE, plot=FALSE)
{
  lambda <- Lambda(object)
  theta <- Theta(object)
  pi0 <- Pi0(object)
  distribution <- distribution(object)

  if(!is.numeric(alpha))
    stop("FDR-level shoud be numeric!")
  if(alpha <= 0 || alpha >= 1)
    stop("FDR-level should be between [0,1]!")  
  if(alpha >= pi0)   ##note by Ferreira
    warning("FDR-level should be smaller than the proportion of non-differentially expressed genes!")
  
  ##extract functions from object
  pf1 <- pf1(object)
  qf0 <- qf0(object)

  if(distribution == "norm" || distribution == "t")
    G <- function(x, y, N) 1 - pf1(q=qf0(p=1 - y/2), y=N*x) + pf1(q=-qf0(p=1 - y/2), y=N*x)  ##two-sided t norm
  else
    G <- function(x, y, N) 1 - pf1(q=qf0(p=1 - y), y=N*x) ##one-side f chisq

  if(missing(samplesizes))
    samplesizes <- Samplesize(object)

  h <- function(x, a=pi0*(1-alpha)/(alpha*(1-pi0))) a*x
  w <- lambda*(theta[2]-theta[1])
  averagePower <- numeric(length(samplesizes))
  for(i in 1:length(samplesizes))
    {
      ##midpoint
      g <- function(u, t=theta, n=samplesizes[i], w = lambda*(theta[2]-theta[1])) sapply(u, function(x) sum(w*G(t, x, n)))
      u <- findroot(g, h, alpha, verbose=verbose, plot=plot)
      averagePower[i] <- g(u)
    }
  averagePower
}

### findroot
findroot <- function(g, h, umax, verbose=FALSE, plot=FALSE)
{
    if(plot)
      {
        curve(g, 0, 1, n=1000, lwd=2, xlab="u", ylab=expression(hat(G)[1](u)), xlim=c(0, 0.1))
        curve(h, add=TRUE, lwd=2, lty=2)
        grid()
      }

    u <- umax

    while(abs(g(u) - h(u)) > 0.01)
      {
        u <- u + sign(g(u) - h(u))*u/2
        if(u < 0 | u > 1)
          {
            print("u outside boundary!!!") ##or return -Inf or +Inf
            break;
          }
        if(verbose)
          print(u)

        if(plot)
          {
            abline(v=u, col=1, lty=3)
            abline(h=g(u), col=1, lty=3)
          }
      }

    if(plot)
      {
        curve(g, add=TRUE, lwd=2, n=1000)
        curve(h, add=TRUE, lwd=2, lty=2)
        abline(v=u, col=2)
        abline(h=g(u), col=2)
      }
    u
  }


checking <- function(x, y)
{
  if(length(y) > 1)
    {
      if(is.character(y)[1])
        {
          if(length(setdiff(x, y)) > 0)
            stop(paste("Parameter '", x, "' is not allowed. Should be one of '", paste(y, collapse="', '"), "'!", sep=""))
        }
    }
  else
    {
      if(typeof(x) != typeof(y))
        stop(paste("type mismatch: ", x, y))
    }
}

deconvControl <- function(control)
{
  ##defaults
  defpar <- list(method = c("deconv","ferreira"),
                 pi0Method = c("Langaas", "Storey", "Ferreira", "Userdefined"),
                 pi0 = seq(0.1, 0.99, 0.01),
                 adjust = TRUE,
                 a=0.5,
                 bandwith = NULL,
                 kernel = c("fan", "wand", "sinc"),
                 from =-6,
                 to = 6,
                 resolution = 2^9,
                 verbose = FALSE)

  if(length(setdiff(names(control), names(defpar))) > 0)
    stop(paste("Unknown control parameter:", setdiff(names(control), names(defpar))), collapse=", ")

  ##set defaults
  conpar <- defpar

  ##overwrite defaults if given
  conpar[names(control)] <- control

  ##some more checks
  tmp <- lapply(names(conpar), function(x) checking(conpar[[x]], defpar[[x]]))

  ##get only one option
  conpar <- lapply(conpar, function(x){
    if(is.character(x[1]))
      x[1]
    else
      x
  })

  ##some logical checks
  if(conpar$pi0Method == "Userdefined" && all(names(conpar) != "pi0"))
    stop("must specify fraction of non-differently expressed genes!")
  else if(conpar$pi0Method == "Userdefined" && length(conpar$pi0) > 1)
    stop("must specify a single fraction of non-differently expressed genes!")
  else if(conpar$pi0Method == "Ferreira" && length(conpar$pi0) == 1)
    stop("must specify a grid of values!")
  else if(all(conpar$pi0 < 0) || all(conpar$pi0 > 1))
    stop("All pi0-values must be between 0 and 1")

  conpar
}

congradControl <- function(control)
{

  defpar <- list(method = "congrad",
                 integration = c("midpoint", "trapezoidal", "simpson"),
                 scale = c("pdfstat", "cdfstat", "cdfpval"),
                 trim = c(0.01, 0.99),
                 symmetric = TRUE,
                 bin = c("epdf", "ecdf"),
                 from = -6,
                 to = 6,
                 resolution = 500,
                 verbose = FALSE)

  if(length(setdiff(names(control), names(defpar))) > 0)
    stop(paste("Unknown control parameter:", setdiff(names(control), names(defpar))), collapse=", ")

  ##set defaults
  conpar <- defpar

  ##overwrite defaults if given
  conpar[names(control)] <- control

  ##some more checks
  tmp <- lapply(names(conpar), function(x) checking(conpar[[x]], defpar[[x]]))

  ##get only one option
  conpar <- lapply(conpar, function(x){
    if(is.character(x[1]))
      x[1]
    else
      x
  })

  ##some logical checks
  if(conpar$scale == "pdfstat")
    conpar$bin <- "epdf"
  else
    conpar$bin <- "ecdf"

  if(conpar$scale == "cdfpval")
    {
      conpar$symmetric <- FALSE
      conpar$trim <- c(0, 1)         #always between 0, 1 and sum to 1
    }

  if(conpar$trim[1] < 0 | conpar$trim[2] > 1)
    stop("trimming values should be within 0 and 1!")

  conpar
}

tikhonovControl <- function(control)
{
  ##defaults
  defpar <- list(method = "tikhonov",
                 integration = c("midpoint", "trapezoidal", "simpson"),
                 scale = c("pdfstat", "cdfstat", "cdfpval"),
                 trim = c(0.01, 0.99),
                 symmetric = TRUE,
                 bin = c("epdf", "ecdf"),
                 from = -6,
                 to = 6,
                 resolution = 500,
                 modelselection = c("lcurve", "gcv", "aic"),
                 log = TRUE,
                 penalty = 0,
                 lambda = 10^seq(-10, 10, length=100),
                 verbose = FALSE)

  if(length(setdiff(names(control), names(defpar))) > 0)
    stop(paste("Unknown control parameter:", setdiff(names(control), names(defpar))), collapse=", ")

  ##set defaults
  conpar <- defpar

  ##overwrite defaults if given
  conpar[names(control)] <- control

  ##some more checks
  tmp <- lapply(names(conpar), function(x) checking(conpar[[x]], defpar[[x]]))

  ##get only one option
  conpar <- lapply(conpar, function(x){
    if(is.character(x[1]))
      x[1]
    else
      x
  })

  ##check set binning
  if(conpar$scale == "pdfstat")
    conpar$bin <- "epdf"
  else
    conpar$bin <- "ecdf"

  conpar
}

define_EffectSizeRange <- function(object, from, to, resolution)
  {
    ##TODO use trimmingbinning function for this!

    ##Resolution must be a power of 2, for the FFT and smaller than test statistics
    if(resolution >= length(Statistics(object)))
      {
        resolution <- 2^floor(log2(length(Statistics(object))))
        warning("Resolution should be smaller than number of test statistics!")
      }
    else if(resolution%%2 != 0)
      {
        resolution <- 2^ceiling(log2(resolution))
        warning("Resolution is set to a power of 2!")
      }

    ##Input range must be symmetric for the fast gnhat calculation
    if(abs(from) != to)
      {
        to <- max(c(abs(from), to))
        from = -to
        warning("Input range is made symmetric!")
      }

    seq(from = from, to = to, length = resolution)
  }

## SampleSize
sampleSize <- function(PilotData, 
                       method = c("deconv", "congrad", 
                                  "tikhonov", "ferreira"), 
                       control=list(from=-6, to=6, resolution=2^9))
{
  ##create new SampleSize-object with PilotData
  object <- new("SampleSize", PilotData)

  ##verify method
  method <- match.arg(method)

  control[["method"]] <- method

  ##fill control parameters for the specific method
  control <- switch(method,
                    deconv = deconvControl(control),
                    congrad = congradControl(control),
                    tikhonov = tikhonovControl(control),
                    ferreira = deconvControl(control))

  if(method == "deconv")
    {
      theta <- define_EffectSizeRange(object, control$from, control$to, control$resolution)
      if(distribution(object) !=  "norm")
        stop("Deconvolution estimator can only be used with distribution = 'norm'!")
    }
  else
    theta <- seq(from = control$from, to = control$to, length = control$resolution)

  ##update SampleSize-object
  object@control <- control
  object@theta <- theta

  ##estimate density of effect sizes
  object <- switch(method,
                   deconv = deconvolution(object),
                   congrad = congrad(object),
                   tikhonov = Tikhonov(object),
                   ferreira = Dn(object, doplot=control$verbose))
  object
}

congrad <- function(object)
{
  ##extract control parameters
  samplesize <- Samplesize(object)
  theta <- Theta(object)
  distribution <- distribution(object)
  bin <- object@control$bin
  scale <- object@control$scale
  trim <- object@control$trim
  symmetric <- object@control$symmetric
  verbose <- object@control$verbose
  from <- object@control$from
  to <- object@control$to
  
  if(scale == "cdfpval")
    statistics <- Pvalues(object)
  else
    statistics <- Statistics(object)
  
  resolution <- length(theta)+1
  
  tb <- trimbin(statistics, nbins=resolution, bin=bin, plot=verbose, symmetric=symmetric, trim=trim)
  
  x <- tb$x
  b <- tb$y
  
  ##extract functions from object
  df1 <- df1(object)
  pf1 <- pf1(object)
  qf0 <- qf0(object)
  
  N <- samplesize
  
  K <- switch(scale,
              pdfstat = function(x, y) df1(x=y, y=N*x),
              cdfstat = function(x, y) pf1(q=y, y=N*x),
              cdfpval = if(distribution == "norm" || distribution == "t")
                function(x, y) 1 - pf1(q=qf0(p=1 - y/2), y=N*x) + pf1(q=-qf0(p=1 - y/2), y=N*x)  ##two-sided t norm
              else
                function(x, y) 1 - pf1(q=qf0(p=1 - y), y=N*x) ##one-side f chisq
  )
  
  A <- midpoint(K, from, to, resolution-1, x)
  A <- cbind(K(0, x), t(A))
  
  beta <- nncg(A, b, trace=verbose)$par
  
  object@pi0 <- beta[1]
  if(beta[1] > 1)
    warning("Estimated pi0 > 1!")
  
  const <- sum(beta[-1]/(1 - beta[1])) * (theta[2] - theta[1])
  lambda.hat <- beta[-1]/(const * (1 - beta[1]))
  
  object@lambda <- lambda.hat
  object@theta <- theta
  
  object
}

nncg <- function(A, b, type=1, trace=FALSE)
{
  #require("SSPA")
  if(.on.public.web){
    dyn.load(.getDynLoadPath());
  }
  if(type < 1 || type > 3)
    stop("Wrong type!")
  
  ##some error checking
  if(!all(is.finite(b)) || !all(is.finite(A)))
    stop("Data contains non-finite values!")
  
  if(ncol(A) != nrow(A) | nrow(A) != length(b))
    stop("Dimensions do not match!")
  if(.on.public.web) {
    output <- .C("nncgc",
                 n = as.integer(ncol(A)),
                 x0 = as.vector(b*0, mode="double"),
                 A = as.vector(A, mode="double"),
                 b = as.vector(b, mode="double"),
                 fmin = as.double(0),
                 fail = as.integer(0),
                 type = as.integer(type),
                 trace = as.integer(trace),
                 objfcount = as.integer(0),
                 gradcount = as.integer(0))
  } else {
    # For MetaboAnalystR
    output <- .C("nncgc",
                 n = as.integer(ncol(A)),
                 x0 = as.vector(b*0, mode="double"),
                 A = as.vector(A, mode="double"),
                 b = as.vector(b, mode="double"),
                 fmin = as.double(0),
                 fail = as.integer(0),
                 type = as.integer(type),
                 trace = as.integer(trace),
                 objfcount = as.integer(0),
                 gradcount = as.integer(0))
  }
  
  list(par=output$x0, value=output$fmin, counts=list('function'=output$objfcount, gradient=output$gradcount), convergence=output$fail)
}

trimbin <- function(statistics, nbins=100, trim=c(0.01, 0.99), bin=c("epdf", "ecdf"), symmetric=TRUE, plot=TRUE)
{
  ##trimming  
  q <- quantile(statistics, prob=trim)
  trimmed <- statistics
  
  ##symmetric trimmming
  ##doesn't make sense if quantiles are different
  if(symmetric==TRUE & all.equal(trim[1], 1 - trim[2]) == TRUE)
  {
    q[2] <- max(abs(q[1]), q[2])
    q[1] <- -q[2]
  }
  
  trimmed[statistics <= q[1] | statistics >= q[2]] <- NA    
  
  ##binning
  breaks <- seq(q[1], q[2], length = nbins + 1)
  x <- breaks[-c(nbins+1)] + 0.5*(q[2] - q[1])/(nbins) #identical to h$mids
  
  if(bin == "epdf")
  {
    if(plot)
      h <- hist(trimmed, breaks = breaks, plot=plot, freq=FALSE)
    else
      h <- hist(trimmed, breaks = breaks, plot=plot)
    y <- h$density
  }
  else if(bin == "ecdf") 
  {
    Fn <- ecdf(trimmed)
    y <- Fn(x)
    if(plot)
      plot(Fn)
  }
  
  ##define range of the noncentrality parameter in some way?
  list(y=y, x=x)    
}

midpoint <- function(f, a, b, n, ...)
{    
  x <- seq(a, b, length=n)
  ##trick for handling two variable functions
  input <- list(...)
  if(length(input) == 0)
    z <- f(x)
  else
  {
    y <- unlist(input)  
    z <- outer(x, y, f)
  }
  z*(x[2]-x[1])    
}

trapezoidal <- function(f, a, b, n, ...)
{
  x <- seq(a, b, length=n)
  w <- c(1, rep(2, n-2), 1)
  ##trick for handling two variable functions
  input <- list(...)
  if(length(input) == 0)
    z <- f(x)
  else
  {
    y <- unlist(input)  
    z <- outer(x, y, f)
  }
  z*w*(x[2]-x[1])/2
}

simpson <- function(f, a, b, n = 5, ...)
{  
  #n should be odd and >= 5
  if(n < 5)
    n <- 5
  if(n%%2 == 0) #n is even add 1
    n <- n + 1
  
  x <- seq(a, b, length=n)
  w <- c(1, rep(c(4, 2), (n-3)/2), 4, 1)
  
  ##trick for handling two variable functions
  input <- list(...)
  if(length(input) == 0)
    z <- f(x)
  else
  {
    y <- unlist(input)  
    z <- outer(x, y, f)
  }
  
  z*w*(x[2]-x[1])/3
}
