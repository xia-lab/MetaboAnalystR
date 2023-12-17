###########################################
# Basic functions defining models 
# for plot, fit, BMD calculation
# adapted from ExpressAnalystR
# Jeff Xia (jeff.xia@xialab.ca)
###########################################


####### functions for drcfit ########
# define formulas for non-linear models, and functions to find the starting parameter values for nonlinear models.
### Hill model and starting values
formHill <- as.formula(signal ~ c + (d - c) / (1 + (dose/e)^b ) )
startvalHillnls2 <- function(x, y, xm, ym, increase) # requires the definition of increase from min and max values
  # which is the first one
  # inputs
  # - x values of the dose
  # - y values the corresponding signal
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  # 
{
  maxi <- max(y, na.rm = TRUE)
  mini <- min(y, na.rm = TRUE)
  ampl <- maxi - mini
  # inflate maxi and mini so as all values are strictly inside the interval [mini; maxi]
  maxi <- maxi + 0.001 * ampl
  mini <- mini - 0.001 * ampl
  # initial value of c
  c <- ifelse(increase, maxi, mini) 
  # initial value of d
  d <-ifelse(increase, mini, maxi) 
  # initial value of e and b from regression
  yreg <- log((d - c) / (y[x!=0] - c) - 1)
  xreg <- log(x[x!=0])
  reg <- lm(yreg ~ xreg)
  b <- reg$coefficients[2]
  e <- reg$coefficients[1] / (-b)
  startval <- list(b = b, c = c, d = d, e = e)
}


### Exp2 model
# define the model formula
formExp2 <- as.formula(signal ~ e*exp(b*dose))
# get starting values
startvalExp2 <- function(xm, ym)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of a
  e <- ym[1]
  
  # transform y for regression
  yreg <- log(ym[xm != 0])
  reg <- lm(yreg ~ xm[xm != 0])
  
  # estimate slope from regression
  b <- coef(reg)[2]
  
  startval <- list(e = e, b = b)
}


### Exp3 model
# define the model formula
formExp3 <- as.formula(signal ~ e*(exp(sign(b)*(abs(b)*dose)^d)))

# get starting values
startvalExp3 <- function(xm, ym)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # transform y for regression
  yreg <- log(ym[xm != 0])
  reg <- lm(yreg ~ xm[xm != 0])
  
  # estimate b and d from regression
  b <- coef(reg)[2]
  
  d <- (exp(coef(reg)[1]))/e
  
  startval <- list(e = e, b = b, d = d)
}


### Exp4 model
formExp4 <- as.formula(signal ~ e*(c - (c-1)*exp((-1)*b*dose)))

# get starting values
startvalExp4 <- function(xm, ym, ad.dir)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # initial value of c (since the asymptote is always to the right, we can calculate c based on a and the max/min val)
  if(ad.dir == TRUE){
    c <- max(ym)/e + 0.001*(max(ym)/e)
  }else{
    c <- min(ym)/e - 0.001*(min(ym)/e)
  }
  
  # initial value of b
  yreg <- log((ym - e*c)/(e-e*c))
  reg <- lm(yreg ~ xm)
  
  b <- abs(coef(reg)[2])
  
  startval <- list(e = e, b = b, c = c)
}


#### Exp5 ####
formExp5 <- as.formula(signal ~ e*(c - (c-1)*exp((-1)*(b*dose)^d)))

# get starting values
startvalExp5 <- function(xm, ym, ad.dir)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # initial value of c
  if(ad.dir){
    c <- max(ym)/e + 0.001*(max(ym)/e)
  }else{
    c <- min(ym)/e - 0.001*(min(ym)/e)
  }
  
  # initial value of b and d
  yreg <- log((ym - e*c)/(e-e*c))
  reg <- lm(yreg ~ xm)
  
  b <- abs(coef(reg)[2])
  d <- exp(coef(reg)[1])
  
  startval <- list(e = e, b = b, c = c, d = d)
}


#### power ####
formPow <- as.formula(signal ~ e + b*(dose^c))

# get starting values
startvalPow <- function(xm, ym, ad.dir, dset){
  
  require(dplyr)
  
  if(ad.dir){
    
    e <- min(ym) - 0.001*min(ym)
    
    yreg <- log(ym[xm!=0] - e)[-1]
    xreg <- log(xm[xm!=0])[-1]
    
    reg <- lm(yreg ~ xreg)
    
    c <- max(c(coef(reg)[2],1))
    b <- exp(coef(reg)[1])
    
  } else {
    
    e <- max(ym) + 0.001*max(ym)
    
    yreg <- log((ym[xm!=0] - e)*(-1))
    xreg <- log(xm[xm!=0])
    
    reg <- lm(yreg ~ xreg)
    
    c <- max(c(coef(reg)[2],1))
    b <- exp(coef(reg)[1])*(-1)
  }
  
  start.1 <- list(e = e, b = b, c = c)
  
  Pow <- suppressWarnings(try(nls(formPow, start = start.1, data = dset,
                                  lower = c(1, -Inf, 0.999), control = nls.control(maxiter = 500),
                                  upper = c(Inf, Inf, 18), algorithm = "port",
                                  weights = 1/(signal^2)), silent = TRUE))
  
  if (!inherits(Pow, "try-error")){
    startval <- coef(Pow) %>% as.list()
  } else {
    startval <- list(e = e, b = b, c = c)
  }
  
}

#### function to get bmd results
bmdres <- function(fit){
  
  bmd.ci <- suppressMessages(confint(fit, "bmd"))
  bmd.mean <- coef(fit)[1] # get bmd estimate from fit
  c(bmd.mean, bmd.ci)
  
}

#### function for bootstrapping the median
fun.boot <- function(x, i) {
  median(x[i])
}

#######################################################
## set of functions to init/read details of ItemSelect 
##  object needed for DR curve fitting
#######################################################

####################ItemSelec object###################

GetItemSelectData <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # double matrix
  return(itemselect$data);
}

GetItemSelectDataRowNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(rownames(itemselect$data));
}

GetItemSelectDataColNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(colnames(itemselect$data));
}

GetItemSelectDose <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # numeric vector
  return(itemselect$dose)
}

GetItemSelectItems <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # numeric vector
  return(itemselect$item)
}

GetItemSelectDataMean <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # double matrix
  return(itemselect$data.mean);
}

GetItemSelectDataMeanColNms <- function(){
  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(colnames(itemselect$data.mean));
}

InitItemSelect <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  if(!exists("data.select") || !exists("dose") || !exists("item") || !exists("data.mean") || !exists("data.mean.colnames")){
    print("Could not find data vector!");
    return(0);
  }

  rownames(data.select) <- item
  colnames(data.mean) <- data.mean.colnames
  reslist <- list(data = data.select, dose = dose,
                  item = item, data.mean = data.mean)  
  dataSet$itemselect <- structure(reslist, class = "itemselect")
  mSetObj$dataSet <- dataSet;
  .set.mSet(mSetObj);
  return(1);
}

####################DrcFit object###################
GetNumberDoses <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(length(levels(mSetObj$dataSet$cls))-1) # number of doses minus 0
}

GetDrcFitAllRes <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  res <- res[,-c(1,2,ncol(res))]
  res <- as.matrix(res)
  
  return(res)
}

GetDrcFitAllResColNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all

  return(colnames(res))
}

GetDrcFitAllGeneIds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,1])
}

GetDrcFitAllModNms <- function(){
  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,2])
}

GetDrcFitAllInvStatus <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,ncol(res)])
}

InitDrcFitObj <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;

  if(!exists("drc.res.modelnms") || !exists("drc.res.gene.ids") || !exists("drc.res") || !exists("drc.res.colnms") || !exists("drc.res.invstatus")){
    print("Could not find drc meta objects!");
    return(0);
  }
  drc.res.invstatus <- as.logical(drc.res.invstatus)
  dres <- data.frame(drc.res.gene.ids, drc.res.modelnms, drc.res, drc.res.invstatus)
  colnames(dres) <- drc.res.colnms
  
  #TODO: check if some needed objects are missing
  reslist <- list(fitres.all = dres, fitres.filt = data.frame(), data = dataSet$itemselect$data,
                  dose = dataSet$itemselect$dose, data.mean = dataSet$itemselect$data.mean, 
                  item = dataSet$itemselect$item) 
  
  dataSet$drcfit.obj <- structure(reslist, class = "drcfit")
  
  mSetObj$dataSet <- dataSet;
  .set.mSet(mSetObj);
  return(1);
}


FilterBMDResults <- function(dataSet){
  obj.bmd <- dataSet$bmdcalc.obj
  if (!inherits(obj.bmd, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects, created with the function bmdcalc")

  bmdcalc.obj <- obj.bmd
  if(bmd.pass.option == "conv"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$conv.pass, ]
  }else if(bmd.pass.option == "hd"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$hd.pass, ]
  }else if(bmd.pass.option == "ci"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$CI.pass, ]
  }else if(bmd.pass.option == "ld"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$ld.pass, ]
  }else{
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$all.pass, ]
  }
  return(bmdcalc.res)
}

GetDRRes <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  obj.bmd <- dataSet$bmdcalc.obj
  
  bmdcalc.obj <- obj.bmd
  if(bmd.pass.option == "conv"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$conv.pass, ]
  }else if(bmd.pass.option == "hd"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$hd.pass, ]
  }else if(bmd.pass.option == "ci"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$CI.pass, ]
  }else if(bmd.pass.option == "ld"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$ld.pass, ]
  }else{
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$all.pass, ]
  }
  
  curv.num <- dim(obj.bmd$bmdcalc.res)[1]
  bmd.num <- dim(bmdcalc.res)[1]
  
  res <- c(curv.num, bmd.num)
  
  #update sig genes
  dataSet$sig.mat <- dataSet$comp.res[which(rownames(dataSet$comp.res) %in% bmdcalc.res$id),];
  mSetObj$dataSet <- dataSet;
  .set.mSet(mSetObj);
  return(res)   
}


#### use the pureErrorAnova function from alr3. alr3 is now deprecated, so extracted these 
# lines from the alr3 source code in the CRAN archive
pureErrorAnova <- function(mod){UseMethod("pureErrorAnova")}
pureErrorAnova.lm <- function(mod) {
  if (is.null(mod$model)) mod <- update(mod, model=TRUE)
  p <- dim(mod$model)[2] -1
  mod$model$Lack.of.Fit <-
    factor(randomLinComb(model.matrix(mod), 101319853))
  aov1 <- anova(mod)
  #set.seed(save.seed) # restore random number seed
  if (length(levels(mod$model$Lack.of.Fit)) == length(mod$model$Lack.of.Fit))
    aov1 else {
      aov2 <- anova(lm(mod$model[ , 1]~mod$model$Lack.of.Fit, weights=weights(mod)))
      rrow <- dim(aov1)[1]
      aov2[1, 1] <- aov1[rrow, 1]-aov2[2, 1]
      aov2[1, 2] <- aov1[rrow, 2]-aov2[2, 2]
      aov2[1, 3] <- aov2[1, 2]/aov2[1, 1]
      aov1[1:(rrow-1), 4] <- aov1[1:(rrow-1), 3]/aov2[2, 3]
      aov2[1, 4] <- aov2[1, 3]/aov2[2, 3]
      row.names(aov2) <- c(" Lack of fit", " Pure Error")
      aov <- rbind(aov1, aov2)
      aov[ , 5] <- pf(aov[ , 4], aov[ , 1], aov2[2, 1], lower.tail=FALSE)
      aov
    }}


randomLinComb <- function(X, seed=NULL) {UseMethod("randomLinComb")}

randomLinComb.default <- function(X, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  std <- function(x){
    s <- sd(x)
    if( s > 0) (x-mean(x))/s else x}
  as.vector(apply(X, 2, std)%*% as.vector(2*rnorm(dim(X)[2])-1) )
}

randomLinComb.lm <- function(X, ...) {
  randomLinComb(model.matrix(X), ...)}

randomLinComb.lm <- function(X, seed=NULL) {
  if(is.null(X$model)) X <- update(X, model=TRUE)
  randomLinComb(X$model[ , -1], seed=seed)}





