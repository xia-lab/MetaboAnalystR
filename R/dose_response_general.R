#1_omicsdata.R
### import, check and optionnally normalize omics data
PrepareDataForDoseResponse <- function()
{
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  dataSet$comp.res <- dataSet$comp.res[order(rownames(dataSet$comp.res)), ]

  data <- dataSet$data.norm
  data <- data[order(rownames(data)), ]

  # definition of doses and item identifiers
  dose <- as.numeric(gsub(".*_", "", as.character(dataSet$meta.info[,1])))
  # control of the design
  design <- table(dose, dnn = "")
  fdose <- as.factor(dose)
  tdata <- t(data)
  nrowd <- nrow(data)
  item <- rownames(data)
  
  # the following 3 steps tested faster than aggregate
  calcmean <- function(i){tapply(tdata[, i], fdose, mean)}
  s <- sapply(1:nrowd, calcmean)
  data.mean <- as.matrix(t(s))
  
  reslist <- list(dose = dose, item = item, 
                  design = design, data.mean = data.mean)  
  
  dataSet$omicdata <- structure(reslist, class = "omicdata")
  RegisterData(dataSet);
  return(1);
}

#2_itemselect.R
### select significantly responsive items 
GetSigDRItems <- function(deg.pval = 1, FC = 1.5, deg.FDR = FALSE, wtt = FALSE, wtt.pval = 0.05, parallel = "no", ncpus = 1)
{

  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  #prepare param
  deg.FDR <- as.logical(deg.FDR)
  wtt <- as.logical(wtt)
  
  #get data
  data <- dataSet$data.norm
  data <- data[order(rownames(data)), ]
  omicdata <- dataSet$omicdata
  res <- dataSet$comp.res
  
  # Checks
  if (!inherits(omicdata, "omicdata"))
    stop("Use only with 'omicdata' objects, created with the function omicdata")
  
  if (!is.logical(deg.FDR))
    stop("deg.FDR must be either TRUE or FALSE")
  if (!is.logical(wtt))
    stop("wtt must be either TRUE or FALSE")
  if ((wtt.pval <=0) | (wtt.pval > 1))
    stop("deg.pval must be in ]0; 1[.")
  
  # load libraries
  #require(nparcomp)
  
  # get data
  data.mean <- omicdata$data.mean
  item <- omicdata$item
  dose <- omicdata$dose

  # get 0 replacement
  dose.uniq <- sort(unique(dose))
  dose.ratio <- c()

  for(i in c(1:(length(dose.uniq)-2))){
    temp <- dose.uniq[i+2]/dose.uniq[i+1]
    dose.ratio <- c(dose.ratio, temp)
  }

  dose.ratio <- median(dose.ratio)
  zero.rep <- dose.uniq[2]/dose.ratio    
  dataSet$zero.log <- zero.rep

  doseranks <- as.numeric(as.factor(dose))
  irow <- 1:length(item)
  nselect <- dim(data)[1]
  
  if(dataSet$de.method == "deseq2"){

    table_list <- dataSet$comp.res.list;

    num_rows <- nrow(table_list[[1]])

    # Initialize combined result vectors with FALSE
    combined_lfc_pass <- rep(FALSE, num_rows)
    combined_deg_pass <- rep(FALSE, num_rows)

    # Loop through each table
    for(i in 1:length(table_list)) {
        # Current table
        res <- table_list[[i]]
        res <- res[order(rownames(res)), ]


        # Determine if probes pass FC filter
        res$lfc.pass <- abs(res[,1]) >= FC

        # Determine if probes pass differential expression filter
        if (deg.FDR == TRUE) {
            res$deg.pass <- res$adj.P.Val < deg.pval
        } else {
            res$deg.pass <- res$P.Value < deg.pval
        }

        # Update the combined vectors
        combined_lfc_pass <- combined_lfc_pass | res$lfc.pass
        combined_deg_pass <- combined_deg_pass | res$deg.pass
    }   
    res$lfc.pass <- combined_lfc_pass;
    res$deg.pass <- combined_deg_pass;

  }else{
    # get column with max log fold change
    fc.cols <- c(1:(length(unique(dose))-1))
    res$max.lfc <- apply(res[,fc.cols], 1, function(x){max(abs(x))})

    # determine if probes pass FC filter
    res$lfc.pass <- res$max.lfc > FC

    # determine if probes pass differential expression filter
    if (deg.FDR == TRUE){
      res$deg.pass <- res$adj.P.Val < deg.pval
    } else{
      res$deg.pass <- res$P.Value < deg.pval
    }
  }
  ### Williams trend test function ###
  wtt.func <- function(i){
    if((res[i, "lfc.pass"] & res[i, "deg.pass"])){
      # get data for fitting
      signal <- data[i, ]
      dset <- data.frame(signal = signal, dose = dose)
      
      # do williams trend test and extract results
      wtt.obj <- mctp(signal ~ dose, data = dset, type = "Williams", info = FALSE, rounds = 1)
      pval <- wtt.obj$Overall[2]
    } else {
      pval <- NA
    }
    return(pval)
  }
  
  # Apply Williams trend test to features that pass all other filters
  # parallel or sequential computation
  if (wtt){
    if (parallel != "no") 
    {
      if (parallel == "snow") type <- "PSOCK"
      else if (parallel == "multicore") type <- "FORK"
      clus <- parallel::makeCluster(ncpus, type = type)
      wtt.res <- parallel::parSapply(clus, 1:nselect, wtt.func)
      parallel::stopCluster(clus)
    } else
    {
      wtt.res <- sapply(1:nselect, wtt.func)
    }
    
    # process results
    wtt.res <- unname(unlist(wtt.res))
    
    # add wtt pvals to results table
    res$wtt.pval <- wtt.res
    
    # determine which features pass wtt
    res$wtt.pass <- res$wtt.pval < wtt.pval
    
    res$all.pass <- (res$deg.pass & res$lfc.pass & res$wtt.pass)
    res$all.pass[is.na(res$all.pass)] <- FALSE
  } else {
    res$all.pass <- (res$deg.pass & res$lfc.pass)
  }
  print(table(res$lfc.pass))
  print(table(res$deg.pass))
  print(table(res$all.pass));
  # select only data that passes all filters
  data.select <- data[res$all.pass, ]
  data.mean <- data.mean[res$all.pass, ]
  item <- item[res$all.pass]

  
  reslist <- list(data = data.select, dose = dose,
                  item = item, data.mean = data.mean, itemselect.res = res)  
  
  dataSet$itemselect <- structure(reslist, class = "itemselect")
  saveRDS(dataSet, "dataSet.rds");

  RegisterData(dataSet);
  }

#3_drcfit.R
### fit different models to each dose-response curve and choose the best fit 
PerformDRFit <- function(ncpus = 2)
{

  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  if(!exists("models")){
    print("Could not find models vector!");
    return(0);
  }

  itemselect <- dataSet$itemselect;
  
  # Checks
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  model.choices <- c("Exp2","Exp3","Exp4","Exp5","Lin","Poly2","Poly3","Poly4","Hill","Power")
  if (sum(models %in% model.choices) != length(models))
    stop("You must identify statistical models with the correct identifiers")
  if (sum(duplicated(model.choices)) > 0)
    stop("Do not add duplicate model choices")
  
  require(data.table)
  require(dplyr)
  require(drc)
  require(alr3)
  
  # definition of necessary data
  dose <- itemselect$dose
  doseranks <- as.numeric(as.factor(dose)) 
  data <- itemselect$data 
  data.mean <- itemselect$data.mean 
  
  # calculations for starting values and other uses
  dosemin <- min(dose)
  dosemax <- max(dose)
  dosemed <- median(dose[dose!=0])
  doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
  
  # number of points per dose-response curve
  nptsperDR <- ncol(data)
  nselect <- nrow(data)
  
  AICdigits <- 2 # number of digits for rounding the AIC values
  
  kcrit = 2 # for defining AIC or BIC 
  
  # function to fit all the models an choose the best on one item
  ################################################################
  fitoneitem <- function(i) 
  {
    keeplin <- "Lin" %in% models
    keepHill <- "Hill" %in% models
    keepExp2 <- "Exp2" %in% models
    keepExp3 <- "Exp3" %in% models
    keepExp4 <- "Exp4" %in% models
    keepExp5 <- "Exp5" %in% models
    keepPoly2 <- "Poly2" %in% models
    keepPoly3 <- "Poly3" %in% models
    keepPoly4 <- "Poly4" %in% models
    keepPow <- "Power" %in% models
    
    signal <- data[i, ]
    gene.id <- rownames(data)[i]
    signalm <- as.vector(data.mean[i,]) # means per dose
    dose0 <- signalm[1]
    
    # preparation of data for modelling with nls 
    dset <- data.frame(dose = dose, signal = signal)
    dset <<- dset # I have to do this, otherwise the neill.test function can't find it
    
    # for choice of the linear trend (decreasing or increasing)
    modlin <- lm(signal ~ doseranks)
    adv.incr <- coef(modlin)[2] >= 0
    
    # initialize results dataframe
    item.fitres <- data.frame()
    
    ################## Exp2 fit ##########################
    if (keepExp2)
    {
      startExp2 <- startvalExp2(xm = doseu, ym = signalm)
      Exp2 <- suppressWarnings(try(nls(formExp2, start = startExp2, data = dset, 
                                       lower = c(0, -Inf), algorithm = "port"), silent = TRUE))
      if (!inherits(Exp2, "try-error"))
      {
        fit <- Exp2
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(Exp2)
        b.i <- par[2]
        c.i <- NA
        d.i <- NA
        e.i <- par[1]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Exp2"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
      } 
    }
    
    ################## Exp3 fit ##########################
    if (keepExp3)
    {
      startExp3 <- startvalExp3(xm = doseu, ym = signalm)
      Exp3 <- suppressWarnings(try(nls(formExp3, start = startExp3, data = dset, 
                                       lower = c(0, -Inf, -Inf), algorithm = "port"), silent = TRUE))
      
      if (!inherits(Exp3, "try-error"))
      {
        
        fit <- Exp3
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(fit)
        b.i <- par[2]
        c.i <- NA
        d.i <- par[3]
        e.i <- par[1]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Exp3"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
        
      } 
    }
    
    ################## Exp4 fit ##########################
    if (keepExp4)
    {
      startExp4 <- startvalExp4(xm = doseu, ym = signalm, ad.dir = adv.incr)
      
      if (adv.incr)
      {
        Exp4 <- suppressWarnings(try(nls(formExp4, start = startExp4, data = dset, 
                                         lower = c(1, 0, 1), algorithm = "port"), silent = TRUE)) 
      } else
      {
        Exp4 <- suppressWarnings(try(nls(formExp4, start = startExp4, data = dset, 
                                         lower = c(1, 0, 0), 
                                         upper = c(Inf, Inf, 1), algorithm = "port"), silent = TRUE))
      }
      
      if (!inherits(Exp4, "try-error"))
      {
        
        fit <- Exp4
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(fit)
        b.i <- par[2]
        c.i <- par[3]
        d.i <- NA
        e.i <- par[1]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Exp4"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
        
      } 
    } 
    
    ################## Exp5 fit ##########################
    if (keepExp5)
    {
      startExp5 <- startvalExp5(xm = doseu, ym = signalm, ad.dir = adv.incr)
      
      if (adv.incr)
      {
        Exp5 <- suppressWarnings(try(nls(formExp5, start = startExp5, data = dset, 
                                         lower = c(1, 0, 1, 1), algorithm = "port"), silent = TRUE)) 
      } else
      {
        Exp5 <- suppressWarnings(try(nls(formExp5, start = startExp5, data = dset, 
                                         lower = c(1, 0, 0, 1), 
                                         upper = c(Inf, Inf, 1, Inf), algorithm = "port"), silent = TRUE))
      }
      
      if (!inherits(Exp5, "try-error"))
      {
        
        fit <- Exp5
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(fit)
        b.i <- par[2]
        c.i <- par[3]
        d.i <- par[4]
        e.i <- par[1]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Exp5"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
        
      } 
    }
    
    ################## Power fit ##########################
    if (keepPow)
    {
      startPow <- startvalPow(xm = doseu, ym = signalm, ad.dir = adv.incr, dset = dset)
      
      Pow <- suppressWarnings(try(nls(formPow, start = startPow, data = dset, 
                                      lower = c(1, -Inf, 0.999), 
                                      upper = c(Inf, Inf, 18), algorithm = "port"), silent = TRUE))
      
      if (!inherits(Pow, "try-error"))
      {
        
        fit <- Pow
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(fit)
        b.i <- par[2]
        c.i <- par[3]
        d.i <- NA
        e.i <- par[1]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Power"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
        
      }
    } 
    
    ################## Hill fit ##########################
    if (keepHill)
    {
      startHill <- startvalHillnls2(x = dose, y = signal, xm = doseu, ym = signalm,  
                                    increase = adv.incr)
      Hill <- suppressWarnings(try(nls(formHill, start = startHill, data = dset, 
                                       lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
      if (!inherits(Hill, "try-error"))
      {
        
        fit <- Hill
        
        # collect parameters
        AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
        lof.pval.i <- neill.test(fit, dset$dose, display = FALSE)
        par <- coef(fit)
        b.i <- par["b"]
        c.i <- par["c"]
        d.i <- par["d"]
        e.i <- par["e"]
        f.i <- NA
        SDres.i <- sigma(fit)
        mod.name <- "Hill"
        ctrl <- predict(fit)[1]
        
        # append results to dataframe
        res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                               e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                               lof.p = lof.pval.i, ctrl.mod = ctrl)
        item.fitres <- rbind(item.fitres, res.temp)
        
      }
    }
    
    ######### Fit of the linear model ############################    
    if (keeplin)
    {
      Lin <- lm(signal ~ dose, data = dset)
      fit <- Lin
      
      # collect parameters
      AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
      lof.pval.i <- pureErrorAnova(fit)[3,5]
      par <- coef(fit)
      b.i <- par[2]
      c.i <- NA
      d.i <- par[1]
      e.i <- NA
      f.i <- NA
      SDres.i <- sigma(fit)
      mod.name <- "Lin"
      ctrl <- predict(fit)[1]
      
      # append results to dataframe
      res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                             e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                             lof.p = lof.pval.i, ctrl.mod = ctrl)
      item.fitres <- rbind(item.fitres, res.temp)
      
    }
    
    ######### Fit of the Poly2 model ############################    
    if (keepPoly2)
    {
      Poly2 <- lm(signal ~ dose + I(dose^2), data = dset)
      fit <- Poly2
      
      # collect parameters
      AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
      lof.pval.i <- pureErrorAnova(fit)[4,5]
      par <- coef(fit)
      b.i <- par[1]
      c.i <- par[2]
      d.i <- par[3]
      e.i <- NA
      f.i <- NA
      SDres.i <- sigma(fit)
      mod.name <- "Poly2"
      ctrl <- predict(fit)[1]
      
      # append results to dataframe
      res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                             e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                             lof.p = lof.pval.i, ctrl.mod = ctrl)
      item.fitres <- rbind(item.fitres, res.temp)
      
    }
    
    ######### Fit of the Poly3 model ############################    
    if (keepPoly3)
    {
      Poly3 <- lm(signal ~ dose + I(dose^2) + I(dose^3), data = dset)
      fit <- Poly3
      
      # collect parameters
      AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
      lof.pval.i <- pureErrorAnova(fit)[5,5]
      par <- coef(fit)
      b.i <- par[1]
      c.i <- par[2]
      d.i <- par[3]
      e.i <- par[4]
      f.i <- NA
      SDres.i <- sigma(fit)
      mod.name <- "Poly3"
      ctrl <- predict(fit)[1]
      
      # append results to dataframe
      res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                             e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                             lof.p = lof.pval.i, ctrl.mod = ctrl)
      item.fitres <- rbind(item.fitres, res.temp)
      
    }
    
    ######### Fit of the Poly4 model ############################    
    if (keepPoly4)
    {
      
      Poly4 <- lm(signal ~ dose + I(dose^2) + I(dose^3) + I(dose^4), data = dset)
      fit <- Poly4
      
      # collect parameters
      AIC.i <- round(AIC(fit, k = kcrit), digits = AICdigits)
      lof.pval.i <- pureErrorAnova(fit)[6,5]
      par <- coef(fit)
      b.i <- par[1]
      c.i <- par[2]
      d.i <- par[3]
      e.i <- par[4]
      f.i <- par[5]
      SDres.i <- sigma(fit)
      mod.name <- "Poly4"
      ctrl <- predict(fit)[1]
      
      # append results to dataframe
      res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                             e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                             lof.p = lof.pval.i, ctrl.mod = ctrl)
      item.fitres <- rbind(item.fitres, res.temp)
      
    }
    
    ######## Fit of the null model (constant) ###########################
    constmodel <- lm(signal ~ 1, data = dset)
    
    # collect parameters
    AIC.i <-  round(AIC(constmodel, k = kcrit), digits = AICdigits) - 2
    lof.pval.i <- 0
    b.i <- NA
    c.i <- mean(dset$signal)
    d.i <- NA
    e.i <- NA
    f.i <- NA
    SDres.i <- sigma(constmodel)
    mod.name <- "Const"
    
    # append results to dataframe
    res.temp <- data.frame(gene.id = gene.id, mod.name = mod.name, b = b.i, c = c.i, d = d.i, 
                           e = e.i, f = f.i, SDres = SDres.i, AIC.model = AIC.i,
                           lof.p = lof.pval.i, ctrl.mod = 0)
    item.fitres <- rbind(item.fitres, res.temp)

    item.fitres$item.ind <- c(rep(i, dim(item.fitres)[1]))
    item.fitres$ctrl.mean <- c(rep(dose0, dim(item.fitres)[1]))
    item.fitres$adv.incr <- c(rep(adv.incr, dim(item.fitres)[1]))
    
    rownames(item.fitres) <- NULL
    
    return(item.fitres)
    
  } ##################################### end of fitoneitem
  
  my.fitoneitem <-  function(i) {
                     tmp <- try(fitoneitem(i));
                     if(class(tmp) == "try-error") {
                       return(NA);
                     }else{
                       return(tmp);
                     }
                   };

  # Loop on items
  # parallel or sequential computation
  if (ncpus != 1) 
  {
    clus <- parallel::makeCluster(ncpus, type = "FORK")
    res <- parallel::parLapply(clus, 1:nselect, my.fitoneitem)
    parallel::stopCluster(clus)
    #res <- rbindlist(res)
  }
  else
  {
    res <- base::lapply(1:nselect, my.fitoneitem)
    #res <- rbindlist(res)
  }
  
  na.inx <- is.na(res);
  print(paste("A total of", sum(na.inx), "errors in modeling and were removed."));
  res[na.inx] <- NULL;
  dres <- as.data.frame(rbindlist(res));

  
  reslist <- list(fitres.all = dres, fitres.filt = data.frame(), data = dataSet$itemselect$data,
                  dose = dataSet$itemselect$dose, data.mean = dataSet$itemselect$data.mean, 
                  item = dataSet$itemselect$item) 
  
  dataSet$drcfit.obj <- structure(reslist, class = "drcfit")
  
  RegisterData(dataSet);

  return(1);
}

FilterDRFit <- function()
{
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  f.drc <- dataSet$drcfit.obj;
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  require(data.table)
  lof.pval <- as.numeric(lof.pval)

  # get results
  fitres.all <- as.data.table(dataSet$drcfit.obj$fitres.all)
  fitres.filt <-fitres.all[fitres.all$mod.name!="Const"]
  
  # get best fit for each feature based on selected criteria
  fitres.filt$AIC.model <- as.numeric(as.vector(fitres.filt$AIC.model))
  fitres.filt$lof.p <- as.numeric(as.vector(fitres.filt$lof.p))
  if(fit.select == "AIC"){
    fitres.filt <- fitres.filt[fitres.filt[ , AIC.model == min(AIC.model), by = item.ind]$V1]
  }else if(fit.select == "pvalue"){
    # filter based on pvalue cutoff
    fitres.filt <- fitres.filt[fitres.filt[ , lof.p == min(lof.p), by = item.ind]$V1]
  }else if(fit.select == "both"){
    fitres.filt <- fitres.filt[as.numeric(fitres.filt$lof.p) > lof.pval]
    fitres.filt <- fitres.filt[fitres.filt[ , .I[which.min(AIC.model)], by = item.ind]$V1]
  }
  
  # remove rows that had no signficant fits
  idx <- as.numeric(fitres.filt$item.ind)
  data <- dataSet$drcfit.obj$data[idx, ]
  data.mean <- dataSet$drcfit.obj$data.mean[idx, ]
  item <- dataSet$drcfit.obj$item[idx]
  
  # update drcfit object
  dataSet$drcfit.obj$fitres.filt <- as.data.frame(fitres.filt)
  dataSet$drcfit.obj$data <- data
  dataSet$drcfit.obj$data.mean <- data.mean
  dataSet$drcfit.obj$item <- item
  saveSet(paramSet);
  RegisterData(dataSet);
  return(1)
}



#4_bmdcalc.R
### Calculation of BMD values from fitted dose-response curves
PerformBMDCalc <- function(ncpus = 1)
{

  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  f.drc <- dataSet$drcfit.obj;
  f.its <- dataSet$itemselect;

  num.sds <- as.numeric(num.sds);
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  require(data.table)
  require(dplyr)
  
  dfitall <- f.drc$fitres.filt # filter this based on constant model
  dfitall$mod.name <- as.character(dfitall$mod.name)
  nselect <- nrow(dfitall)
  
  # get necessary data for fitting
  dose <- f.drc$dose
  doseranks <- as.numeric(as.factor(dose)) 
  data <- f.its$data
  data.mean <- f.its$data.mean

  # get only correct rows
  inx.bmd <- rownames(data) %in% as.character(dfitall$gene.id)
  data <- as.data.frame(data) # this keeps correct shape in case of only 1 row
  data <- data[inx.bmd, ] %>% as.matrix()

  data.mean <- as.data.frame(data.mean)
  data.mean <- data.mean[inx.bmd, ] %>% as.matrix()

  item <- dataSet$drcfit.obj$fitres.filt[,1]
  fitres.bmd <- f.drc$fitres.filt

  if(ctrl.mode == "sampleMean"){
    bmr.mode <- "ctrl.mean"
  } else {
    bmr.mode <- "ctrl.mod"
  }
  
  # function to calculate the bmd from the best fit model
  ################################################################
  bmdoneitem <- function(i) 
  {
    # determine if adverse direction is increasing or decreasing
    adv.incr <- dfitall[i, "adv.incr"];
    
    # compute the BMR
    if (adv.incr == TRUE){ 
      bmr <- as.vector(dfitall[i, bmr.mode]) + num.sds*as.vector(dfitall[i, "SDres"])
    } else { 
      bmr <- as.vector(dfitall[i, bmr.mode]) - num.sds*as.vector(dfitall[i, "SDres"])
    }
    
    # get best fit model type
    mod.name <- as.character(dfitall[i, "mod.name"])
    
    # get data for fitting
    signal <- data[i, ]
    dset <- data.frame(signal = signal, dose = dose)
    
    # get parameters
    b <- as.numeric(as.vector(dfitall[i, "b"]))
    c <- as.numeric(as.vector(dfitall[i, "c"]))
    d <- as.numeric(as.vector(dfitall[i, "d"]))
    e <- as.numeric(as.vector(dfitall[i, "e"]))
    f <- as.numeric(as.vector(dfitall[i, "f"]))
    
    # fit re-parametrized model
    switch(mod.name,
           Lin = {
             bmd0 <- (bmr - d)/b
             fit <- suppressWarnings(try(nls(signal ~ d + ((bmr - d)/bmd)*dose, 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Hill = {
             bmd0 <- e*((((d-c)/(bmr-c))-1)^(1/b))
             fit <- suppressWarnings(try(nls(signal ~ c + (((bmr - c)*(1 + ((bmd/e)^b)) + c) - c) / (1 + (dose/e)^b), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Exp2 = {
             bmd0 <- log(bmr/e)/b
             fit <- suppressWarnings(try(nls(signal ~ (bmr/exp(b*bmd))*exp(b*dose), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Exp3 = {
             bmd0 <- ((log(bmr/e)/sign(b))^(1/d))/abs(b)
             fit <- suppressWarnings(try(nls(signal ~ (bmr/exp(sign(b)*(abs(b)*bmd)^d))*(exp(sign(b)*(abs(b)*dose)^d)), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Exp4 = {
             bmd0 <- log((c-(bmr/e))/(c-1))/(-b)
             fit <- suppressWarnings(try(nls(signal ~ (bmr/(c-(c-1)*exp(-1*b*bmd)))*(c - (c-1)*exp((-1)*b*dose)), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Exp5 = {
             bmd0 <- ((-log((c-(bmr/e))/(c-1)))^(1/d))/b
             fit <- suppressWarnings(try(nls(signal ~ (bmr/(c-(c-1)*exp((-1)*(b*bmd)^d)))*(c - (c-1)*exp((-1)*(b*dose)^d)), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Poly2 = {
             bmd1 <- -((c + ((c^2) - 4*(b-bmr)*d)^(1/2))/(2*d))
             bmd2 <- -((c - ((c^2) - 4*(b-bmr)*d)^(1/2))/(2*d))
             bmds <- c(bmd1, bmd2)
             bmds <- bmds[bmds > 0]
             bmd0 <- min(bmds)
             fit <- suppressWarnings(try(nls(signal ~ (bmr - (c*bmd + d*(bmd^2))) + c*dose + d*(dose^2), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           
           Poly3 = {
             roots <- polyroot(c(b-bmr, c, d, e))
             roots <- Re(roots)[round(Im(roots), 2) == 0]
             bmd0 <- min(roots[roots > 0])
             if(adv.incr) {bmd0 <- bmd0 - 0.1} else {bmd0 <- bmd0 + 0.1}
             fit <- suppressWarnings(try(nls(signal ~ (bmr - (c*bmd + d*(bmd^2) + e*(bmd^3))) + c*dose + d*(dose^2) + e*(dose^3), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Poly4 = {
             roots <- polyroot(c(b-bmr, c, d, e, f))
             roots <- Re(roots)[round(Im(roots), 2) == 0]
             bmd0 <- min(roots[roots > 0])
             if(adv.incr) {bmd0 <- bmd0 - 0.1} else {bmd0 <- bmd0 + 0.1}
             fit <- suppressWarnings(try(nls(signal ~ (bmr - (c*bmd + d*(bmd^2) + e*(bmd^3) + f*(bmd^4))) + c*dose + d*(dose^2) + e*(dose^3) + f*(dose^4), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           Power = {
             bmd0 <- ((bmr-e)/b)^(1/c)
             fit <- suppressWarnings(try(nls(signal ~ (bmr - b*(bmd^c)) + b*(dose^c), 
                                             data = dset, start = list(bmd = bmd0), lower = c(0), algorithm = "port"), 
                                         silent = TRUE))
           },
           stop("No match here!")
    )
    
    
    # get bmd results from re-parametrized fit
    if (!inherits(fit, "try-error"))
    {
      bmd.res <- try(bmdres(fit), silent = TRUE)
    } else
    {
      bmd.res <- c(NA, NA, NA)
    }
    
    # replace where bmd.res failed with error code
    if ( inherits(bmd.res, "try-error")){
      bmd.res <- c(9999, NA, NA)
    }
    
    bmd.res <- c(bmd.res, mod.name, bmr)
    names(bmd.res) <- c("bmd", "bmdl", "bmdu", "mod.name", "bmr")
    return(bmd.res)
    
  } ##################################### end of bmdoneitem
  
  # Loop on items
  # parallel or sequential computation
  if (ncpus != 1) 
  {
    clus <- parallel::makeCluster(ncpus, type = "FORK")
    res <- parallel::parSapply(clus, 1:nselect, bmdoneitem)
    parallel::stopCluster(clus)
  } else {
    res <- sapply(1:nselect, bmdoneitem)
  }
  
  # change class of columns
  dres <- as.data.frame(t(res))
  dres$bmd <- as.numeric(as.character(dres$bmd))
  dres$bmdl <- as.numeric(as.character(dres$bmdl))
  dres$bmdu <- as.numeric(as.character(dres$bmdu))
  dres$id <- as.character(item)
  dres$bmr <- as.numeric(as.character(dres$bmr))
  
  # change order of columns
  dres <- dres[, c("id", "mod.name", "bmd", "bmdl", "bmdu", "bmr")]
  
  # does bmdcalc converge for bmd, bmdl, and bmdu?
  dres$conv.pass <- rowSums(is.na(dres)) == 0
  
  # is the bmd < highest dose?
  dres$hd.pass <- dres$bmd < max(dose)
  
  # is the CI of the bmd narrow enough?
  dres$CI.pass <- dres$bmdu/dres$bmdl < 40

  # is the CI of the bmd narrow enough?
  dres$CI.pass2 <- dres$bmd/dres$bmdl < 20
  
  # is the bmd < lowest dose/10?
  low.dose <- sort(unique(dose))[2]
  dres$ld.pass <- dres$bmdl > (low.dose/10)
  
  # aggregate all filters
  # flag genes that don't pass low dose condition by keeping the column, but do 
  # not use for the final filtering
  dres$all.pass <- (dres$conv.pass & dres$hd.pass & dres$CI.pass & dres$CI.pass2)
  
  # update the data
  data.select <- data[dres$all.pass, ]
  data.mean <- data.mean[dres$all.pass, ]
  item <- item[dres$all.pass]
  
  # make combined object
  fitres.bmd <- fitres.bmd[dres$all.pass, ]
  
  # make results to display
  disp.res <- data.frame(item = item)
  disp.res <- cbind(disp.res, fitres.bmd[,c("mod.name", "SDres", "lof.p")])
  disp.res <- cbind(disp.res, dres[dres$all.pass, c("bmr", "bmd", "bmdl", "bmdu")])
  
  dnld.file <- merge(dataSet$drcfit.obj$fitres.filt[,-12], dres[,-2], by.x = "gene.id", by.y = "id");
  data.table::fwrite(as.data.frame(dnld.file), quote = FALSE, row.names = FALSE, sep = "\t", file="bmd.txt");

  # collect results to return
  reslist <- list(bmdcalc.res = dres, fitres.bmd = fitres.bmd, 
                  data = data.select, disp.res = disp.res,
                  data.mean = data.mean, dose = dose, item = item)
  
  # return results
  dataSet$bmdcalc.obj <- structure(reslist, class = "bmdcalc")

  # make table for html display
  if(dim(disp.res)[1] > 0) {
    res <- disp.res[,c(1,2,4,7,6,8)];
    res.mods <- dataSet$drcfit.obj$fitres.filt[,c(1,3,4,5,6)];
    res <- merge(res, res.mods, by.y = "gene.id", by.x = "item");
    res[,c(3:10)] <- apply(res[,c(3:10)], 2, function(x) as.numeric(as.character(x)));
    res[,c(3:6)] <- apply(res[,c(3:6)], 2, function(x) signif(x, digits = 2));
    rownames(res) <- as.character(res$item);
    colnames(res) <- c("gene.id","mod.name","lof.p","bmdl","bmd","bmdu","b","c","d","e");
    res <- res[order(res$bmd), ];
    dataSet$html.resTable <- res;
    RegisterData(dataSet);
    if(dim(disp.res)[1] == 1){
      return(3)
    } else {
      return(1)
    }
  } else {
    return(2)
  }
  
}

#5a_sensPOD.R
### Calculation of transcriptomic POD from BMDs
sensPOD <- function(pod = c("feat.20", "feat.10th", "mode"), scale)
{
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  pod.choices <- c("feat.20", "feat.10th", "mode")
  if (sum(pod %in% pod.choices) != length(pod))
    stop("You must identify pod methods with the correct identifiers")
  if (sum(duplicated(pod.choices)) > 0)
    stop("Do not add duplicate pod methods")
  
  require(data.table)
  require(dplyr)
  require(boot)
  
  # get bmd data
  bmd.res <- FilterBMDResults(dataSet)

  if(scale == "log10"){
    bmds <- log10(bmd.res$bmd);
  } else if(scale == "log2"){
    bmds <- log2(bmd.res$bmd);
  } else {
    bmds <- bmd.res$bmd;
  }
  
  # prepare results
  trans.pod <- c(rep(NA, length(pod)))
  names(trans.pod) <- pod.choices[(pod.choices %in% pod)]
  
  # calculate transcriptomic pod from specified method
  if ("feat.20" %in% pod){
    
    if (length(bmds) < 20) {
      
      trans.pod["feat.20"] <- NA
      
    } else {
      
      # get 20 lowest BMDs
      bmd.sort <- sort(bmds)
      trans.pod["feat.20"] <- bmd.sort[20]
      
    }
    
  } 
  if ("feat.10th" %in% pod){
    
    # POD = 10th percentile BMD from all significant probes
    trans.pod["feat.10th"] <- unname(quantile(bmds, 0.1))
    
  } 
  if ("mode" %in% pod & length(bmds) > 1){
    
    # get density plot
    density.bmd <- density(bmds, na.rm = TRUE)
    
    # interpolate with high # of data points
    X <- density.bmd$x
    Y <- density.bmd$y 
    
    # calculate first derivative
    dY <- diff(Y)/diff(X)
    dX <- rowMeans(embed(X,2))
    
    # detect index of local maxima
    dY.signs <- sign(dY)
    dY.signs.1 <- c(0,dY.signs[-length(dY.signs)])
    
    # get indices of local mins and maxes
    sign.changes <- dY.signs - dY.signs.1
    inds.maxes <- which(sign.changes == -2)
    inds.mins <- which(sign.changes == 2)
    
    bmd.temp <- bmds
    bmds.tot <- length(bmd.temp)
    
    # get mins and maxes
    if((length(inds.mins) > 0) & (length(inds.maxes) > 0)){
      
      mins <- dX[inds.mins]
      names(mins) <- paste0("min", c(1:length(mins)))
      maxes <- dX[inds.maxes]
      names(maxes) <- paste0("max", c(1:length(maxes)))
      
      # order mins and maxes
      mins.maxes <- sort(c(mins,maxes))
      num.modes <- length(mins.maxes) %/% 2
      mins.maxes <- mins.maxes[1:(2*num.modes)]
      
      # initialize inputs to for loop
      size.peaks <- c(rep(NA, num.modes))
      per.peaks <- c(rep(NA, num.modes))
      
      # calculate the size of each peak
      for(i in c(1:num.modes)){
        
        size.peaks[i] <- sum(bmd.temp < mins.maxes[2*i])
        per.peaks[i] <- size.peaks[i]/bmds.tot
        
        # update bmd.temp to remove features from previous peak
        bmd.temp <- bmd.temp[!(bmd.temp < mins.maxes[2*i])]
      }
      
      # check size
      per.pass <- which(per.peaks > 0.05)
      
    } else if((length(inds.mins) == 0) & (length(inds.maxes) > 0)){
      
      maxes <- dX[inds.maxes]
      per.pass <- 1
      
    } else {
      per.pass <- NULL
    }
    
    # find transcriptomic pod
    if (length(per.pass > 0)){
      trans.pod["mode"] <- unname(maxes[per.pass[1]])
    } else {
      trans.pod["mode"] <- NA
    }
    
  }
  return(trans.pod)
}

#5b_gsPOD.R
### Calculation of transcriptomic POD from BMDs
gsPOD <- function(obj.data, bmd.res, gene.vec, geneDB, pval = 1.0, FDR = FALSE)
{ 
  paramSet <- readSet(paramSet, "paramSet");
  gs <- geneDB;
  if (!inherits(obj.data, "omicdata"))
    stop("Use only with 'omicdata' objects, created with the function omicdata")

  require(data.table)
  require(dplyr)
  require(boot)
  
  # gather components
  universe <- obj.data$item
  hits <- gene.vec
  
  # calculate transcriptomic pod from specified method
  if (gs == "panther.bp"){
    gs.lib <- "go_panthbp.rds"

  } else if (gs == "panther.mf"){
    gs.lib <- "go_panthmf.rds"

  } else if (gs == "panther.cc"){
    gs.lib <- "go_panthcc.rds" 

  } else if (gs == "kegg"){
    gs.lib <- "kegg.rds"

  } else if (gs == "go.bp"){
    gs.lib <- "go_bp.rds"

  } else if (gs == "go.mf"){
    gs.lib <- "go_mf.rds"

  } else if (gs == "go.cc"){
    gs.lib <- "go_cc.rds"
  } else if (gs == "reactome"){
    gs.lib <- "reactome.rds"
  } else {
    gs.lib <- "No match"
  }

  # do gsoa
  if(paramSet$data.org == "generic"){
    orgDir <- paramSet$data.idType
  }else{
    orgDir <- paramSet$data.org
  }
  results.summary <- gsoa.fun(paste0(paramSet$lib.path, orgDir, "/", gs.lib), 
                                universe, hits, bmd.res, 1.0, FDR)
  # append to list of results
  results <- results.summary$results
  genematches <- results.summary$genematches

  gsPOD.results <- list(results, genematches)
  names(gsPOD.results) <- c("geneset.stats", "geneset.matches")
  
  return(gsPOD.results)

}


GetFitResultMatrix <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  res <- dataSet$html.resTable;
  res <- res[,-c(1,2)];
  res <- as.matrix(res);
  res <- signif(res, 5)
  res[is.nan(res)] <- 0;
  res <- as.data.frame(res);
  colnames(res) <- c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e");
  res <- apply(res, 2, function(x) as.numeric(as.character(x)));
  RegisterData(dataSet);
  return(res);
}

GetFitResultColNames <-function(){
  names <- c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e");
  return(names);
}

GetFitResultGeneIDs <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  return(as.character(dataSet$html.resTable[,1]))
}

GetFitResultModelNms <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  return(as.character(dataSet$html.resTable[,2]))
}

GetFitResultGeneIDLinks <- function(org){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  if (org == "noAnn"){
    ids <- as.character(dataSet$html.resTable[,1]);
    symbs <- ids;
    annots <- ids;
    return(annots);
  } else if (org == "s2f"){
    ids <- as.character(dataSet$html.resTable[,1]);
    symbs <- doEntrez2SymbolMapping(ids)
    annots <- paste("<a style='color: #a7414a' href='https://www.ecoomicsdb.ca/#/query?ortho=", ids, "' target='_blank'>", symbs, "</a>", sep="");
    return(annots);    
  } else {
    ids <- as.character(dataSet$html.resTable[,1]);
    symbs <- doEntrez2SymbolMapping(ids)
    annots <- paste("<a style='color: #a7414a' href='http://www.ncbi.nlm.nih.gov/gene?term=", ids, "' target='_blank'>", symbs, "</a>", sep="");
    return(annots);
  }
}

GetFitResultGeneSymbols <-function(org){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  if (org == "noAnn") {
    return(as.character(dataSet$html.resTable[,1]));
  } else {
    return(doEntrez2SymbolMapping(as.character(dataSet$html.resTable[,1])));
  }
}