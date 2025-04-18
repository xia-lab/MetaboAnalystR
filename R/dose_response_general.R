###########################################
# methods for dose response analysis,
# adapted from ExpressAnalystR
# Jeff Xia (jeff.xia@xialab.ca)
###########################################


# Step 1: prepare data for computing
PrepareDataForDoseResponse <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);

  #important to order by rownames, otherwise code doesn't work!!!!!!!!
  mSetObj$dataSet$comp.res <- mSetObj$dataSet$comp.res[order(rownames(mSetObj$dataSet$comp.res)), ]

  data <- t(mSetObj$dataSet$norm);
  data <- data[order(rownames(data)), ]

  dose <- as.numeric(as.character(mSetObj$dataSet$cls));
  fdose <- as.factor(dose);

  # control of the design
  design <- table(dose, dnn = "");
  tdata <- t(data)
  nrowd <- nrow(data)
  item <- rownames(data)
  
  # the following 3 steps tested faster than aggregate
  calcmean <- function(i){tapply(tdata[, i], fdose, mean)};
  s <- sapply(1:nrowd, calcmean);
  data.mean <- as.matrix(t(s));
  
  mSetObj$dataSet$itemselect <- structure(list(dose = dose, item = item, design = design, data.mean = data.mean), class="omicdata");
  print("PrepareDataForDoseResponse === OK");
  return(.set.mSet(mSetObj));
}

# PrepareSigDRItems(mSet, 0.2,0.0,TRUE,FALSE,
# Step 2: select significantly responsive items 
PrepareSigDRItems <- function(mSetObj=NA, deg.pval = 1, FC = 1.5, deg.FDR = FALSE, wtt = FALSE, wtt.pval = 0.05, parallel = "yes", ncpus = 4){
  mSetObj <- .get.mSet(mSetObj);
  
  #get data
  data <- t(mSetObj$dataSet$norm);
  data <- data[order(rownames(data)), ];

  # get data
  res <- mSetObj$dataSet$comp.res;  # not sure all (current) or only sig?
  omicdata <- mSetObj$dataSet$itemselect;
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
  mSetObj$dataSet$zero.log <- zero.rep

  doseranks <- as.numeric(as.factor(dose))
  irow <- 1:length(item);
  nselect <- dim(data)[1];
  
  # get column with max log fold change
  fc.cols <- c(1:(length(unique(dose))-1));
  res$max.lfc <- apply(res[,fc.cols], 1, function(x){max(abs(x))})
  
  # determine if probes pass FC filter
  res$lfc.pass <- res$max.lfc > FC
  
  # determine if probes pass differential expression filter
  if (deg.FDR == TRUE){
    res$deg.pass <- res$adj.P.Val < deg.pval
  } else{
    res$deg.pass <- res$P.Value < deg.pval
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

  if(sum(res$all.pass) == 0){
    return(0);
  }
  # select only data that passes all filters
  data.select <- data[res$all.pass, ]
  data.mean <- data.mean[res$all.pass, ]
  item <- item[res$all.pass]

  #print(nrow(data.select));
  #print("data.select======");

  mSetObj$dataSet$itemselect <- structure(list(data = data.select, dose = dose,
                  item = item, data.mean = data.mean, itemselect.res = res), class="itemselect");  
  #saveRDS(mSetObj, "msetobj.rds");
  print("PrepareSigDRItems === OK");
  return(.set.mSet(mSetObj));
}

#3_drcfit.R
### fit different models to each dose-response curve and choose the best fit 
PerformDRFit <- function(mSetObj=NA, ncpus=4){

  if(!exists("models")){
    print("Could not find models vector!");
    return(0);
  }

  require(data.table)
  require(dplyr)
  require(drc)
  require(alr3)
  
  mSetObj <- .get.mSet(mSetObj);
  itemselect <- mSetObj$dataSet$itemselect;

  # definition of necessary data
  dose <- itemselect$dose
  doseranks <- as.numeric(as.factor(dose)); 
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
    modlin <- lm(signal ~ doseranks);
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
        save(fit, kcrit, AICdigits, dset, file = "fit_line301.rda")
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
  
  # Loop on items
  # parallel or sequential computation
  my.fitoneitem <-  function(i) {
                     tmp <- try(fitoneitem(i));
                     if(class(tmp) == "try-error") {
                       return(NA);
                     }else{
                       return(tmp);
                     }
                   };
  if (ncpus != 1) 
  {
    clus <- parallel::makeCluster(ncpus, type = "FORK")
    res <- parallel::parLapply(clus, 1:nselect, my.fitoneitem)
    parallel::stopCluster(clus)
  }
  else
  {
    res <- base::lapply(1:nselect, my.fitoneitem)

  }
  
  # remove those with errors during modeling 
  na.inx <- is.na(res);
  print(paste("A total of", sum(na.inx), "errors in modeling and were removed."));
  res[na.inx] <- NULL;
  res <- as.data.frame(rbindlist(res));
  
  reslist <- list(fitres.all = res, fitres.filt = data.frame(), data = data,
                  dose = itemselect$dose, data.mean = itemselect$data.mean, 
                  item = itemselect$item) 
  
  mSetObj$dataSet$drcfit.obj <- structure(reslist, class = "drcfit")
  
  print("Completed PerformDRFit!");
  return(.set.mSet(mSetObj));
}

PerformContDRFit <- function(mSetObj=NA, ncpus=1){
    mSetObj <- .get.mSet(mSetObj);
    #save.image("PerformContDRFit.RData");
    require(dplyr)
    require(drc)
    
    ft_names <- rownames(mSetObj[["dataSet"]][["limma_dose_sig_res"]])
    dt <- mSetObj[["dataSet"]][["norm"]]
    dose_vec <- as.numeric(as.character(mSetObj[["dataSet"]][["cls"]]))

    ft_linear_idx <- vapply(ft_names, function(x){
      which(x == colnames(dt))
    }, FUN.VALUE = integer(1L))

    data_normalised_list <- lapply(ft_linear_idx, function(u){
      vec_ints <-dt[,u]
      data_normalised <- data.frame(r_file_name = rownames(dt), 
                                    grouping = paste0("feature_", u), 
                                    normalised_intensity_log2 = as.double(vec_ints), 
                                    r_condition = dose_vec)
      return(data_normalised)
    })

    res <- rep(list(list()), 17)
    names(res) <- c("ll4","l4","ll24","ll5","l5","ll25","ll3","l3","ll23","w14","w24","w13","w23","bc4","bc5","ar3","mm3")

    if(ncpus == 1){
        # 4 params model
        if("ll4" %in% models){
            res_ll4 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll4");
            res[["ll4"]] <- res_ll4
            print("=== model ll4 completed!")
        }
        if("l4" %in% models){
            res_l4 <- lapply(data_normalised_list, fit_drc_modelling, model = "l4");
            res[["l4"]] <- res_l4
            print("=== model l4 completed!")
        }
        if("ll24" %in% models){
            res_ll24 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll24");
            res[["ll24"]] <- res_ll24
            print("=== model ll24 completed!")
        }
        # 5 params model
        if("ll5" %in% models){
            res_ll5 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll5");
            res[["ll5"]] <- res_ll5
            print("=== model ll5 completed!")
        }
        if("l5" %in% models){
            res_l5 <- lapply(data_normalised_list, fit_drc_modelling, model = "l5");
            res[["l5"]] <- res_l5
            print("=== model l5 completed!")
        }
        if("ll25" %in% models){
            res_ll25 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll25");
            res[["ll25"]] <- res_ll25
            print("=== model ll25 completed!")
        }
        # 3 params model
        if("ll3" %in% models){
            res_ll3 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll3");
            res[["ll3"]] <- res_ll3
            print("=== model ll3 completed!")
        }
        if("l3" %in% models){
            res_l3 <- lapply(data_normalised_list, fit_drc_modelling, model = "l3");
            res[["l3"]] <- res_l3
            print("=== model l3 completed!")
        }
        if("ll23" %in% models){
            res_ll23 <- lapply(data_normalised_list, fit_drc_modelling, model = "ll23");
            res[["ll23"]] <- res_ll23
            print("=== model ll23 completed!")
        }
        # Weibull model
        if("w14" %in% models){
            res_w14 <- lapply(data_normalised_list, fit_drc_modelling, model = "w14");
            res[["w14"]] <- res_w14
            print("=== model w14 completed!")
        }
        if("w24" %in% models){
            res_w24 <- lapply(data_normalised_list, fit_drc_modelling, model = "w24");
            res[["w24"]] <- res_w24
            print("=== model w24 completed!")
        }
        if("w13" %in% models){
            res_w13 <- lapply(data_normalised_list, fit_drc_modelling, model = "w13");
            res[["w13"]] <- res_w13
            print("=== model w13 completed!")
        }
        if("w23" %in% models){
            res_w23 <- lapply(data_normalised_list, fit_drc_modelling, model = "w23");
            res[["w23"]] <- res_w23
            print("=== model w23 completed!")
        }
        # Brain-Cousens hormesis model
        if("bc4" %in% models){
            res_bc4 <- lapply(data_normalised_list, fit_drc_modelling, model = "bc4");
            res[["bc4"]] <- res_bc4
            print("=== model bc4 completed!")
        }
        if("bc5" %in% models){
            res_bc5 <- lapply(data_normalised_list, fit_drc_modelling, model = "bc5");
            res[["bc5"]] <- res_bc5
            print("=== model bc5 completed!")
        }
        # Asymptotic regression model
        if("ar3" %in% models){
            res_ar3 <- lapply(data_normalised_list, fit_drc_modelling, model = "ar3");
            res[["ar3"]] <- res_ar3
            print("=== model ar3 completed!")
        }
        # Michaelis-Menten model
        if("mm3" %in% models){
            res_mm3 <- lapply(data_normalised_list, fit_drc_modelling, model = "mm3");
            res[["mm3"]] <- res_mm3
            print("=== model mm3 completed!")
        }
    } else {

    }
    
    res <- res[!vapply(res, function(x){length(x)==0}, logical(1L))]
    nms <- names(res)
    fitting_res0 <- res;
resxs <- lapply(1:length(res), function(i){
  res0 <- res[[i]]
  res_model_name <- names(res)[i]

  # Get model-level feature names
  feature_names <- names(res0)

  rex <- lapply(seq_along(res0), function(j){
    y <- res0[[j]]
    if (is.null(y$result_coefficients)) {
      return(NULL)
    }

    if (length(y$SDres) == 0) {
      y$SDres <- 0
    }

    coef_vec <- y$result_coefficients
    get_val <- function(name) if (name %in% names(coef_vec)) coef_vec[[name]] else NA

    data.frame(
      feature.id = feature_names[j],
      mod.name = res_model_name,
      b = get_val("hill:(Intercept)"),
      c = get_val("min_value:(Intercept)"),
      d = get_val("max_value:(Intercept)"),
      e = get_val("ec_50:(Intercept)"),
      lof.p = y[["lof.pval.i"]],
      AIC.model = y[["AIC.i"]],
      SDres = y[["SDres"]],
      bmd = y[["bmds_results"]][1],
      bmdl = y[["bmds_results"]][2],
      bmdu = y[["bmds_results"]][3],
      item.ind = j
    )
  })

  rex_df <- do.call(rbind, rex[!vapply(rex, is.null, logical(1))])
  rownames(rex_df) <- NULL
  return(rex_df)
})

    resxs_df <- do.call(rbind, resxs)
    resxs_df <- resxs_df[order(resxs_df$item.ind), ]
    
    drcfit.obj <- list()
    drcfit.obj$fitres.all <- resxs_df
    drcfit.obj$dose <- dose_vec;
    mSetObj$dataSet$drcfit.obj <- drcfit.obj
    
    qs::qsave(fitting_res0, file = "curve_fitting_res.qs")    
    print("Completed PerformContDRFit!");
    return(.set.mSet(mSetObj));
}

FilterDRFit <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  f.drc <- mSetObj$dataSet$drcfit.obj;
  save(mSetObj, file = "mSetObj___FilterDRFit.rda")
  require(data.table)
  lof.pval <- as.numeric(lof.pval)

  # get results
  fitres.all <- as.data.table(f.drc$fitres.all)
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
  data <- f.drc$data[idx, ]
  data.mean <- f.drc$data.mean[idx, ]
  item <- f.drc$item[idx]
  
  # update drcfit object
  f.drc$fitres.filt <- as.data.frame(fitres.filt);
  f.drc$data <- data;
  f.drc$data.mean <- data.mean;
  f.drc$item <- item;
  
  mSetObj$dataSet$drcfit.obj <- f.drc;
  print("Completed FilterDRFit!");
  save(mSetObj, file = "mSetObj___FilterDRFit2.rda")
  return(.set.mSet(mSetObj));
}

FilterDRFitCont <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  f.drc <- mSetObj$dataSet$drcfit.obj;
  save(mSetObj, file = "mSetObj___FilterDRFit3.rda")
  require(data.table)
  lof.pval <- as.numeric(lof.pval)
  
  # get results
  fitres.all <- as.data.table(f.drc$fitres.all)
  fitres.filt <-fitres.all
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
  data <- f.drc$data[idx, ]
  data.mean <- f.drc$data.mean[idx, ]
  item <- f.drc$item[idx]
  
  # update drcfit object
  f.drc$fitres.filt <- as.data.frame(fitres.filt);
  f.drc$data <- data;
  f.drc$data.mean <- data.mean;
  f.drc$item <- item;
  
  mSetObj$dataSet$drcfit.obj <- f.drc;
  print("Completed FilterDRFit!");
  save(mSetObj, file = "mSetObj___FilterDRFit4.rda")
  return(.set.mSet(mSetObj));
}


#4_bmdcalc.R
### Calculation of BMD values from fitted dose-response curves
PerformBMDCalc <- function(mSetObj=NA, ncpus=4){

  mSetObj <- .get.mSet(mSetObj);
  save(mSetObj, file = "mSetObj___PerformBMDCalc.rda")

  #save.image("TestDose44.RData");

  dataSet <- mSetObj$dataSet;
  f.drc <- dataSet$drcfit.obj;
  f.its <- dataSet$itemselect;

  num.sds <- as.numeric(num.sds);
  
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
  
  # is the bmd < lowest dose/10?
  low.dose <- sort(unique(dose))[2]
  dres$ld.pass <- dres$bmdl > (low.dose/10)
  
  # aggregate all filters
  # flag genes that don't pass low dose condition by keeping the column, but do 
  # not use for the final filtering
  dres$all.pass <- (dres$conv.pass & dres$hd.pass & dres$CI.pass & dres$ld.pass)
  
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
  
  dnld.file <- merge(f.drc$fitres.filt[,-12], dres[,-2], by.x = "gene.id", by.y = "id");
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
  res.mods <- f.drc$fitres.filt[,c(1,3,4,5,6)];
  res <- merge(res, res.mods, by.y = "gene.id", by.x = "item");
  
  res[,c(3:10)] <- apply(res[,c(3:10)], 2, function(x) as.numeric(as.character(x)));
  res[,c(3:6)] <- apply(res[,c(3:6)], 2, function(x) signif(x, digits = 2));

  rownames(res) <- as.character(res$item);
  colnames(res) <- c("gene.id","mod.name","lof.p","bmdl","bmd","bmdu","b","c","d","e");

  res <- res[order(res$bmd), ];
  dataSet$html.resTable <- res;
  dataSet$drcfit.obj <- f.drc;

  mSetObj$dataSet <- dataSet;
  .set.mSet(mSetObj);

  # Add ld.pass only for the download CSV
  #ld.pass.df <- dres[dres$all.pass, c("id", "ld.pass")]
  #csv.res <- merge(res, ld.pass.df, by.x = "item", by.y = "id")

  fast.write.csv(res, "curvefit_detailed_table.csv");
  print("Completed PerformBMDCalc");
  
  if(!.on.public.web){
      return(.set.mSet(mSetObj))
  }
  if(dim(disp.res)[1] == 1){
    return(3);
  } else {
    return(1)
  }
}else {    
    if(!.on.public.web){
        return(.set.mSet(mSetObj))
    }
    return(2)
  }

}

PerformContBMDCalc <- function(mSetObj = NA) {
  # retrieve and initialize
  mSetObj   <- .get.mSet(mSetObj)
  dataSet   <- mSetObj$dataSet
  f.drc     <- dataSet$drcfit.obj
  f.its     <- dataSet$itemselect
  
  require(data.table)
  require(dplyr)
  
  # raw fit results
  dfitall <- f.drc$fitres.filt
  
  # dose vector
  dose <- as.numeric(as.character(dataSet$cls))
  
  # subset your feature data (unchanged)
  data.mat      <- as.data.frame(f.its$data)
  data.mean.mat <- as.data.frame(f.its$data.mean)
  inx.bmd       <- rownames(data.mat) %in% as.character(dfitall$gene.id)
  data.mat      <- as.matrix(data.mat[inx.bmd, ])
  data.mean.mat <- as.matrix(data.mean.mat[inx.bmd, ])
  
  item       <- dfitall[, 1]
  fitres.bmd <- dfitall
  
  dres <- as.data.frame(dfitall)[, c("feature.id", "mod.name", "bmd", "bmdl", "bmdu")]
  dres <- dres %>%
    mutate(
      # conv.pass = no NAs in bmd, bmdl, bmdu
      conv.pass = rowSums(is.na(across(all_of(c("bmd","bmdl","bmdu"))))) == 0,
      hd.pass   = bmd < max(dose),
      CI.pass   = bmdu / bmdl < 40,
      ld.pass   = bmdl > (sort(unique(dose))[2] / 100),
      all.pass  = conv.pass & hd.pass & CI.pass & ld.pass
    )
  
  # keep only passing features
  item       <- item[dres$all.pass]
  fitres.bmd <- fitres.bmd[dres$all.pass, ]
  
  # prepare the simplified output table
  disp.res <- data.frame(
    item     = item,
    mod.name = fitres.bmd$mod.name,
    lof.p    = fitres.bmd$lof.p,
    bmd      = dres$bmd[dres$all.pass],
    bmdl     = dres$bmdl[dres$all.pass],
    bmdu     = dres$bmdu[dres$all.pass],
    stringsAsFactors = FALSE
  )
  data.table::fwrite(disp.res, "bmd.txt", sep = "\t", quote = FALSE)
  
  # store raw results in your mSetObj
  reslist <- list(
    bmdcalc.res = dres,
    fitres.bmd  = fitres.bmd,
    disp.res    = disp.res,
    data.mean   = data.mean.mat,
    dose        = dose,
    item        = item
  )
  dataSet$bmdcalc.obj <- structure(reslist, class = "bmdcalc")
  
  # if we have something to show, merge in full parameters and format
  if (nrow(disp.res) > 0) {
    res <- disp.res
    res.mods <- fitres.bmd[, c("feature.id","b","c","d","e","AIC.model","item.ind"), drop=FALSE]
    res <- merge(res, res.mods, by.x="item", by.y="feature.id", all.x=TRUE)
    
    # debug
    print("=== Column names in `res` before formatting:")
    print(colnames(res))
    
    # format numeric columns
    num.cols     <- c("lof.p","bmd","bmdl","bmdu","b","c","d","e")
    missing.cols <- setdiff(num.cols, colnames(res))
    if (length(missing.cols) > 0) {
      stop("Missing required columns in res: ", paste(missing.cols, collapse=", "))
    }
    res[, num.cols] <- lapply(res[, num.cols], function(x) {
      signif(as.numeric(as.character(x)), 2)
    })
    
    # finalize names & order
    rownames(res) <- as.character(res$item)
    colnames(res) <- c("feature.id","mod.name", num.cols, "AIC.model","item.ind")
    res <- res[order(res$bmd), ]
    
    # save back to mSetObj
    dataSet$html.resTable <- res
    dataSet$drcfit.obj   <- f.drc
    mSetObj$dataSet      <- dataSet
    .set.mSet(mSetObj)
    fast.write.csv(res, "curvefit_detailed_table.csv")
    
    print("Completed PerformBMDCalc")
    if (!.on.public.web) return(.set.mSet(mSetObj))
    return(if (nrow(disp.res) == 1) 3 else 1)
    
  } else {
    if (!.on.public.web) return(.set.mSet(mSetObj))
    return(2)
  }
}



#5a_sensPOD.R
### Calculation of metabolomic POD from BMDs
sensPOD <- function(mSetObj=NA, pod = c("feat.20", "feat.10th", "mode"), scale){
  dataSet <- mSetObj$dataSet;
  f.drc <- dataSet$drcfit.obj;
  f.its <- dataSet$itemselect;

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
  names(trans.pod) <- pod.choices[(pod.choices %in% pod)];

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

GetFitResultMatrix <- function(){
  mSetObj <- .get.mSet(NA);

  # turn off scientific notation
  options(scipen=999)

  if (mSetObj$dataSet$cls.type == "cont") {
    res <- mSetObj$dataSet$html.resTable
    res <- res[, c("lof.p", "bmdl", "bmd", "bmdu", "b", "c", "d", "e", "AIC.model")]
    res <- signif(as.matrix(res), 5)
    res[is.nan(res)] <- 0
    colnames(res) <- c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e", "AIC")
  } else {
    res <- mSetObj$dataSet$html.resTable[,-c(1,2)]
    res <- signif(as.matrix(res), 5)
    res[is.nan(res)] <- 0
    colnames(res) <- c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e")
  }

  print(head(res))
  return(res)
}

GetFitResultColNames <- function(){
  mSetObj <- .get.mSet(NA)
  if (mSetObj$dataSet$cls.type == "cont") {
    return(c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e", "AIC"))
  } else {
    return(c("P-val", "BMDl", "BMD", "BMDu", "b", "c", "d", "e"))
  }
}

GetFitResultFeatureIDs <- function(){
  mSetObj <- .get.mSet(NA)
  if (mSetObj$dataSet$cls.type == "cont") {
    return(as.character(mSetObj$dataSet$html.resTable$feature.id))
  } else {
    return(as.character(mSetObj$dataSet$html.resTable[,1]))
  }
}

GetFitResultModelNms <- function(){
  mSetObj <- .get.mSet(NA)
  if (mSetObj$dataSet$cls.type == "cont") {
    return(as.character(mSetObj$dataSet$html.resTable$mod.name))
  } else {
    return(as.character(mSetObj$dataSet$html.resTable[,2]))
  }
}
fit_drc_modelling <- function(data, model = "ll4") {
  # Determine the minimum non-zero dose for ec_50 constraint
  min_dose <- min(data$r_condition[data$r_condition > 0], na.rm = TRUE)
  bmd_min <- min_dose / 10

  # Define model structure (no constraints inside fct_obj!)
  fct_obj <- switch(model,
    "ll4"  = drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50")),
    "l4"   = drc::L.4(names = c("hill", "min_value", "max_value", "ec_50")),
    "ll24" = drc::LL2.4(names = c("hill", "min_value", "max_value", "ec_50")),
    "ll5"  = drc::LL.5(names = c("hill", "min_value", "max_value", "ec_50", "unk")),
    "l5"   = drc::L.5(names = c("hill", "min_value", "max_value", "ec_50", "unk")),
    "ll25" = drc::LL2.5(names = c("hill", "min_value", "max_value", "ec_50", "unk")),
    "bc5"  = drc::BC.5(names = c("hill", "min_value", "max_value", "ec_50", "unk")),
    "w14"  = drc::W1.4(names = c("hill", "min_value", "max_value", "ec_50")),
    "w24"  = drc::W2.4(names = c("hill", "min_value", "max_value", "ec_50")),
    "ar3"  = drc::AR.3(names = c("min_value", "max_value", "ec_50")),
    "mm3"  = drc::MM.3(names = c("min_value", "max_value", "ec_50")),
    "l3"   = drc::L.3(names = c("hill", "max_value", "ec_50")),
    "ll3"  = drc::LL.3(names = c("hill", "max_value", "ec_50")),
    "ll23" = drc::LL2.3(names = c("hill", "max_value", "ec_50")),
    "w13"  = drc::W1.3(names = c("hill", "max_value", "ec_50")),
    "w23"  = drc::W2.3(names = c("hill", "max_value", "ec_50")),
    "bc4"  = drc::BC.4(names = c("hill", "max_value", "ec_50", "unk")),
    # fallback
    drc::LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
  )

  # Estimate the number of parameters
  test_fit <- suppressWarnings(
    try(drc::drm(normalised_intensity_log2 ~ r_condition, data = data[1:5, ], fct = fct_obj, control = drc::drmc(noMessage = TRUE)), silent = TRUE)
  )

  if (inherits(test_fit, "try-error")) {
    return(list(error = paste0("Initial test fit failed for '", model, "'")))
  }

  model_length <- length(coef(test_fit))
  #lower_vec <- rep(-Inf, model_length)
  #lower_vec[model_length] <- 0

  # Fit model with lower constraint
  res_fit <- tryCatch({
    drc::drm(
      normalised_intensity_log2 ~ r_condition,
      data = data,
      fct = fct_obj,
      control = drc::drmc(otrace = TRUE)
      #,lowerl = lower_vec
    )
  }, error = function(e) {
    message("❌ Model fitting failed for model '", model, "': ", conditionMessage(e))
    return(NULL)
  })

  if (is.null(res_fit)) {
    return(list(error = paste0("Model fitting failed for '", model, "'")))
  }

  # Generate prediction range
  doses_vec <- res_fit[["origData"]][["r_condition"]]
  predictions_range <- if (model %in% c("l4", "l5", "l3", "ar3", "mm3", "bc4", "bc5", "w14", "w24", "w13")) {
    seq(max(doses_vec), min(doses_vec), length = 100)
  } else {
    exp(seq(log(max(doses_vec)), log(min(doses_vec)), length = 100))
  }

  grouping <- res_fit[["data"]][["r_condition"]]

  # Perform lack-of-fit test with fallback
  lof.pval.i <- tryCatch({
    neill.test(res_fit, grouping, display = FALSE)
  }, error = function(e) {
    message("neill.test failed; retrying with quantile-binned groups...")
    binned_grouping <- tryCatch({
      cut(grouping, breaks = quantile(grouping, probs = seq(0, 1, 0.25), na.rm = TRUE),
          include.lowest = TRUE)
    }, error = function(e) return(NA))

    if (!all(is.na(binned_grouping))) {
      tryCatch({
        neill.test(res_fit, binned_grouping, display = FALSE)
      }, error = function(e) NA)
    } else {
      NA
    }
  })

  AIC.i <- round(AIC(res_fit, k = 2), digits = 2)
  SDres <- sigma(res_fit)
  bmds <- bmdContres(res_fit)
  names(bmds) <- c("bmd", "bmdl", "bmdu")

  predictions <- suppressWarnings(
    stats::predict(res_fit, data.frame(predictions_range), interval = "confidence")
  )

  return(list(
    result_coefficients = res_fit$coefficients,
    lof.pval.i = lof.pval.i,
    AIC.i = AIC.i,
    SDres = SDres,
    prediction_interval = predictions,
    bmds_results = bmds,
    fitting_model = res_fit
  ))
}
