# Perform OPLS
# Orthogonal PLS functions (adapted from ropls package for web-based usage)
# Turn off the .on.public.web for local usage
# Jeff Xia \email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
perform_opls <- function (x, y = NULL, predI = NA, orthoI = 0, crossvalI = 7, log10L = FALSE, permI = 20, .on.public.web = TRUE,
                          scaleC = c("none", "center", "pareto", "standard")[4], ...) {
  xMN <- x;
  if(class(y) == "matrix"){
    yMCN <- y;
  }else{
    yMCN <- matrix(y, ncol = 1);
  }
  rownames(yMCN) <- rownames(xMN)
  colnames(yMCN) <- paste0("y", 1:ncol(yMCN))
  yLevelVc <- NULL;
  xZeroVarVi <- NULL;
  epsN <- .Machine[["double.eps"]]
  
  opl <- .coreOPLS(xMN = xMN, yMCN = yMCN, orthoI = orthoI, predI = predI, 
                   scaleC = scaleC, crossvalI = crossvalI);

  opl$suppLs[["y"]] <- y
  opl$typeC <- "OPLS-DA";

  ## Permutation testing (Szymanska et al, 2012)
  
  if(permI > 0) {
    
    modSumVc <- colnames(opl$summaryDF)
    permMN <- matrix(0,
                     nrow = 1 + permI,
                     ncol = length(modSumVc),
                     dimnames = list(NULL, modSumVc))
    
    perSimVn <- numeric(1 + permI)
    perSimVn[1] <- 1
    
    permMN[1, ] <- as.matrix(opl$summaryDF);
    
    # do initial evaluation time for 10 permutations
    start.time <- Sys.time();
    for(k in 1:10) {

      yVcn <- drop(opl$suppLs[["yMCN"]])
      yPerVcn <- sample(yVcn)
      yPerMCN <- matrix(yPerVcn, ncol = 1)
      perOpl <- .coreOPLS(xMN = xMN,
                          yMCN = yPerMCN,
                          orthoI = opl$summaryDF[, "ort"],
                          predI = opl$summaryDF[, "pre"],
                          scaleC = scaleC,
                          crossvalI = crossvalI)
      
      permMN[1 + k, ] <- as.matrix(perOpl$summaryDF);
      perSimVn[1 + k] <- .similarityF(opl$suppLs[["yMCN"]], yPerMCN)
    }
    end.time <- Sys.time();
    time.taken <- end.time - start.time;
    print(paste("time taken for 10 permutations: ", time.taken));
    
    if(.on.public.web){
      if(time.taken > 60){
        permI <- 20;
      }else if(time.taken > 30){
        permI <- 100;
      }
      permMN <- permMN[1:(1+permI),]; 
      perSimVn <- perSimVn[1:(1+permI)];
    }
    # continue
    for(k in 11:permI) {

      yVcn <- drop(opl$suppLs[["yMCN"]])
      yPerVcn <- sample(yVcn)
      yPerMCN <- matrix(yPerVcn, ncol = 1)
      perOpl <- .coreOPLS(xMN = xMN,
                          yMCN = yPerMCN,
                          orthoI = opl$summaryDF[, "ort"],
                          predI = opl$summaryDF[, "pre"],
                          scaleC = scaleC,
                          crossvalI = crossvalI)
      
      permMN[1 + k, ] <- as.matrix(perOpl$summaryDF);
      perSimVn[1 + k] <- .similarityF(opl$suppLs[["yMCN"]], yPerMCN)
    }
    
    permMN <- cbind(permMN, sim = perSimVn);
    perPvaVn <- c(pR2Y = (1 + length(which(permMN[-1, "R2Y(cum)"] >= permMN[1, "R2Y(cum)"]))) / (nrow(permMN) - 1),
                  pQ2 = (1 + length(which(permMN[-1, "Q2(cum)"] >= permMN[1, "Q2(cum)"]))) / (nrow(permMN) - 1));
    opl$summaryDF[, "pR2Y"] <- perPvaVn["pR2Y"];
    opl$summaryDF[, "pQ2"] <- perPvaVn["pQ2"];
    opl$suppLs[["permMN"]] <- permMN;
    opl$suppLs[["permI"]] <- permI;
  }
  
  ##------------------------------------
  ##   Numerical results
  ##------------------------------------
  
  totN <- length(c(xMN))
  nasN <- sum(is.na(c(xMN)))
  
  if(!is.null(opl$suppLs[["yMCN"]])) {
    totN <- totN + length(c(opl$suppLs[["yMCN"]]))
    nasN <- nasN + sum(is.na(c(opl$suppLs[["yMCN"]])))
  }
  
  ## Raw summary
  ##------------
  
  opl$suppLs[["topLoadI"]] <- 3
  
  if(ncol(xMN) > opl$suppLs[["topLoadI"]]) {
    xVarVn <- apply(xMN, 2, var)
    names(xVarVn) <- 1:length(xVarVn)
    xVarVn <- sort(xVarVn)
    xVarSorVin <- as.numeric(names(xVarVn[seq(1, length(xVarVn), length = opl$suppLs[["topLoadI"]])]))
    opl$suppLs[["xSubIncVarMN"]] <- xMN[, xVarSorVin, drop = FALSE]
  } else{
    opl$suppLs[["xSubIncVarMN"]] <- xMN
  }
  
  if(ncol(xMN) <= 100) {
    xCorMN <- cor(xMN, use = "pairwise.complete.obs")
    xCorMN[lower.tri(xCorMN, diag = TRUE)] <- 0
    
    if(ncol(xMN) > opl$suppLs[["topLoadI"]]) {
      xCorNexDF <- which(abs(xCorMN) >= sort(abs(xCorMN), decreasing = TRUE)[opl$suppLs[["topLoadI"]] + 1],
                         arr.ind = TRUE);
      xCorDisMN <- matrix(0,
                          nrow = nrow(xCorNexDF),
                          ncol = nrow(xCorNexDF),
                          dimnames = list(colnames(xMN)[xCorNexDF[, "row"]],
                                          colnames(xMN)[xCorNexDF[, "col"]]))
      
      for(k in 1:nrow(xCorDisMN)){
        xCorDisMN[k, k] <- xCorMN[xCorNexDF[k, "row"], xCorNexDF[k, "col"]]
      }
    } else {
      xCorDisMN <- xCorMN
    }
    opl$suppLs[["xCorMN"]] <- xCorDisMN
    rm(xCorDisMN)
  }
  return(invisible(opl))
}

.coreOPLS <- function (xMN, yMCN, orthoI, predI, scaleC, crossvalI) {
  epsN <- .Machine[["double.eps"]]
  varVn <- NULL
  yMeanVn <- NULL
  ySdVn <- NULL
  wMN <- NULL
  cMN <- NULL
  uMN <- NULL
  rMN <- NULL
  bMN <- NULL
  vipVn <- NULL
  yPreMN <- NULL
  yTesMN <- NULL
  toMN <- NULL
  poMN <- NULL
  woMN <- NULL
  coMN <- NULL
  orthoVipVn <- NULL
  naxVi <- which(is.na(c(xMN)))
  naxL <- length(naxVi) > 0
  nayVi <- integer()
  nayL <- FALSE;
  yMN <- yMCN;

  obsNamVc <- rownames(xMN)
  
  autNcoL <- autNcpL <- FALSE
  autMaxN <- min(c(10, dim(xMN)))
  if (is.na(orthoI)) {
    if (autMaxN == 1) {
      orthoI <- 0
      predI <- 1
      warning("The data contain a single variable (or sample): A PLS model with a single component will be built", 
              call. = FALSE)
    }
    else {
      orthoI <- autMaxN - 1
      predI <- 1
      autNcoL <- TRUE
    }
  }
  if (is.na(predI)) {
    if (orthoI > 0) {
      if (autMaxN == 1) {
        orthoI <- 0
        warning("The data contain a single variable (or sample): A PLS model with a single component will be built", 
                call. = FALSE)
      }
      else warning("OPLS(-DA): The number of predictive component is set to 1 for a single response model", 
                   call. = FALSE)
      predI <- 1
      if ((predI + orthoI) > min(dim(xMN))) 
        stop("The sum of 'predI' (", predI, ") and 'orthoI' (", 
             orthoI, ") exceeds the minimum dimension of the 'x' data matrix (", 
             min(dim(xMN)), ")", call. = FALSE)
    }
    else {
      predI <- autMaxN
      autNcpL <- TRUE
    }
  }
  xVarVn <- apply(xMN, 2, function(colVn) var(colVn, na.rm = TRUE))
  xMeanVn <- apply(xMN, 2, function(colVn) mean(colVn, na.rm = TRUE))
  switch(scaleC, none = {
    xMeanVn <- rep(0, ncol(xMN))
    xSdVn <- rep(1, times = ncol(xMN))
  }, center = {
    xSdVn <- rep(1, times = ncol(xMN))
  }, pareto = {
    xSdVn <- apply(xMN, 2, function(colVn) sqrt(sd(colVn, 
                                                   na.rm = TRUE)))
  }, standard = {
    xSdVn <- apply(xMN, 2, function(colVn) sd(colVn, na.rm = TRUE))
  })
  xMN <- scale(xMN, center = xMeanVn, scale = xSdVn)
  if (!is.null(colnames(xMN))) {
    xvaNamVc <- colnames(xMN)
  }
  else xvaNamVc <- paste("x", 1:ncol(xMN), sep = "")
  preNamVc <- paste("p", 1:predI, sep = "")
  pMN <- matrix(0, nrow = ncol(xMN), ncol = predI, dimnames = list(xvaNamVc, 
                                                                   preNamVc))
  tMN <- uMN <- matrix(0, nrow = nrow(xMN), ncol = predI, dimnames = list(obsNamVc, 
                                                                          preNamVc))
  ssxTotN <- sum(xMN^2, na.rm = TRUE)
  
  yMeanVn <- apply(yMN, 2, function(colVn) mean(colVn,na.rm = TRUE))
  
  yMeanVn <- rep(0, times = ncol(yMN))
  ySdVn <- rep(1, times = ncol(yMN))
  yMN <- scale(yMN, center = yMeanVn, scale = ySdVn)
  yvaNamVc <- paste("y", 1:ncol(yMN), sep = "")
  wMN <- pMN
  uMN <- tMN
  cMN <- matrix(0, nrow = ncol(yMN), ncol = predI, dimnames = list(yvaNamVc, preNamVc))
  cvfNamVc <- paste("cv", 1:crossvalI, sep = "")
  cvfOutLs <- split(1:nrow(xMN), rep(1:crossvalI, length = nrow(xMN)))
  prkVn <- numeric(crossvalI)
  ru1ThrN <- ifelse(orthoI == 0, ifelse(nrow(xMN) > 100, yes = 0, no = 0.05), 0.01)
  ssyTotN <- rs0N <- sum(yMN^2, na.rm = TRUE)
  hN <- 1
  
  orthoNamVc <- paste("o", 1:orthoI, sep = "");
  toMN <- matrix(0, nrow = nrow(xMN), ncol = orthoI, 
                 dimnames = list(obsNamVc, orthoNamVc));
  woMN <- poMN <- matrix(0, nrow = ncol(xMN), ncol = orthoI, 
                         dimnames = list(xvaNamVc, orthoNamVc));
  coMN <- matrix(0, nrow = ncol(yMN), ncol = orthoI, 
                 dimnames = list(yvaNamVc, orthoNamVc));
  modelDF <- as.data.frame(matrix(NA, nrow = 1 + orthoI + 
                                    1, ncol = 7, dimnames = list(c("p1", orthoNamVc, 
                                                                   "sum"), c("R2X", "R2X(cum)", "R2Y", "R2Y(cum)", 
                                                                             "Q2", "Q2(cum)", "Signif."))));
  for (j in 1:ncol(modelDF)){
    mode(modelDF[, j]) <- ifelse(colnames(modelDF)[j] == "Signif.", "character", "numeric")
  }
  xcvTraLs <- lapply(cvfOutLs, function(obsVi) xMN[-obsVi, , drop = FALSE])
  xcvTesLs <- lapply(cvfOutLs, function(obsVi) xMN[obsVi, , drop = FALSE])
  ycvTraLs <- lapply(cvfOutLs, function(obsVi) yMN[-obsVi, , drop = FALSE])
  ycvTesLs <- lapply(cvfOutLs, function(obsVi) yMN[obsVi, , drop = FALSE])
  xcvTraLs <- c(xcvTraLs, list(xMN))
  ycvTraLs <- c(ycvTraLs, list(yMN))
  breL <- FALSE

  for (noN in 1:(orthoI + 1)) {
    if (breL){
      break
    }
    for (cvN in 1:length(xcvTraLs)) {

      xcvTraMN <- xcvTraLs[[cvN]]
      ycvTraMN <- ycvTraLs[[cvN]]
      if (ncol(ycvTraMN) > 1) {
        wwMN <- apply(ycvTraMN, 2, function(colVn) crossprod(xcvTraMN, colVn)/drop(crossprod(colVn)))
        wwSvdLs <- svd(wwMN)
        wwNcpVin <- which(wwSvdLs[["d"]]^2 > epsN * sum(wwSvdLs[["d"]]^2))
        twMN <- wwSvdLs[["u"]][, wwNcpVin, drop = FALSE] %*% diag(wwSvdLs[["d"]][wwNcpVin], nrow = length(wwNcpVin))
      }
      uOldVn <- ycvTraMN[, 1, drop = FALSE]

      repeat {
        wVn <- crossprod(xcvTraMN, uOldVn)/drop(crossprod(uOldVn))
        wVn <- wVn/sqrt(drop(crossprod(wVn)))
        tVn <- xcvTraMN %*% wVn
        cVn <- crossprod(ycvTraMN, tVn)/drop(crossprod(tVn))
        uVn <- ycvTraMN %*% cVn/drop(crossprod(cVn))
        dscN <- drop(sqrt(crossprod((uVn - uOldVn)/uVn)))
        if (ncol(ycvTraMN) == 1 || dscN < 1e-10) {
          break
        }else {
          uOldVn <- uVn
        }
      }
      pVn <- crossprod(xcvTraMN, tVn)/drop(crossprod(tVn))
      if (ncol(ycvTraMN) > 1){
        for (j in 1:ncol(twMN)) {
          woVn <- pVn - drop(crossprod(twMN[, 
                                            j, drop = FALSE], pVn))/drop(crossprod(twMN[, 
                                                                                        j, drop = FALSE])) * twMN[, j, drop = FALSE];
        }
      } else {
        woVn <- pVn - drop(crossprod(wVn, pVn))/drop(crossprod(wVn)) * wVn
      }
      woVn <- woVn/sqrt(drop(crossprod(woVn)))
      toVn <- xcvTraMN %*% woVn
      coVn <- crossprod(ycvTraMN, toVn)/drop(crossprod(toVn))
      poVn <- crossprod(xcvTraMN, toVn)/drop(crossprod(toVn))
      
      if (cvN <= crossvalI) {
        xcvTesMN <- xcvTesLs[[cvN]]
        ycvTesMN <- ycvTesLs[[cvN]]
        if (any(is.na(xcvTesMN))) {
          prxVn <- numeric(nrow(xcvTesMN))
          for (r in 1:length(prxVn)) {
            comVl <- complete.cases(xcvTesMN[r, ])
            prxVn[r] <- crossprod(xcvTesMN[r, comVl], wVn[comVl])/drop(crossprod(wVn[comVl]))
          }
          prkVn[cvN] <- sum((ycvTesMN - prxVn %*% t(cVn))^2, na.rm = TRUE)
        } else { 
          prkVn[cvN] <- sum((ycvTesMN - xcvTesMN %*% wVn %*% t(cVn))^2, na.rm = TRUE)
        }
        toTesVn <- xcvTesMN %*% woVn
        xcvTesLs[[cvN]] <- xcvTesMN - tcrossprod(toTesVn, poVn)
        if (cvN == crossvalI) {
          q2N <- 1 - sum(prkVn)/rs0N
          if (noN == 1) {
            modelDF["p1", "Q2(cum)"] <- modelDF["p1", "Q2"] <- q2N
          } else {
            modelDF[noN, "Q2(cum)"] <- q2N - modelDF["p1", "Q2"]
            modelDF[noN, "Q2"] <- q2N - sum(modelDF[1:(noN - 1), "Q2"], na.rm = TRUE)
          }
        }
      } else {
        r2yN <- sum(tcrossprod(tVn, cVn)^2)/ssyTotN
        if (noN == 1) {
          modelDF["p1", "R2Y(cum)"] <- modelDF["p1", "R2Y"] <- r2yN
        } else {
          modelDF[noN, "R2Y(cum)"] <- r2yN - modelDF["p1", "R2Y"]
          modelDF[noN, "R2Y"] <- r2yN - sum(modelDF[1:(noN - 1), "R2Y"], na.rm = TRUE)
        }
        if (noN <= orthoI) {
          modelDF[paste0("o", noN), "R2X"] <- sum(tcrossprod(toVn,poVn)^2)/ssxTotN
          poMN[, noN] <- poVn
          toMN[, noN] <- toVn
          woMN[, noN] <- woVn
          coMN[, noN] <- coVn
        }
        
        if (!is.na(modelDF[noN, "R2Y"]) & modelDF[noN, "R2Y"] < 0.01) {
          modelDF[noN, "Signif."] <- "N4"
        } else if (!is.na(modelDF[noN, "Q2"]) & modelDF[noN, "Q2"] < ru1ThrN) {
          modelDF[noN, "Signif."] <- "NS"
        } else {
          modelDF[noN, "Signif."] <- "R1"
        }
        
        if (autNcoL && modelDF[noN, "Signif."] !=  "R1" && noN > 2) {
          breL <- TRUE
          break
        } else {
          cMN[, 1] <- cVn
          pMN[, 1] <- pVn
          tMN[, 1] <- tVn
          uMN[, 1] <- uVn
          wMN[, 1] <- wVn
        }
      }
      if (breL) {
        break;
      }
      if (noN < orthoI + 1){
        xcvTraLs[[cvN]] <- xcvTraMN - tcrossprod(toVn, poVn);
      }
    }
  }
  
  rm(xcvTraLs)
  rm(xcvTesLs)
  rm(ycvTraLs)
  
  modelDF["p1", "R2X(cum)"] <- modelDF["p1", "R2X"] <- sum(tcrossprod(tMN, pMN)^2)/ssxTotN
  modelDF[1:(1 + orthoI), "R2X(cum)"] <- cumsum(modelDF[1:(1 + orthoI), "R2X"]);
  
  if (autNcoL) {
    if (all(modelDF[, "Signif."] == "R1", na.rm = TRUE)) {
      orthoI <- noN - 1
    }else{
      orthoI <- noN - 3
    }
    
    if (orthoI == autMaxN - 1){ 
      warning("The maximum number of orthogonal components in the automated mode (", 
              autMaxN - 1, ") has been reached whereas R2Y (", 
              round(modelDF[1 + orthoI, "R2Y"] * 100), 
              "%) is above 1% and Q2Y (", round(modelDF[1 + 
                                                          orthoI, "Q2"] * 100), "%) is still above ", 
              round(ru1ThrN * 100), "%.", call. = FALSE)
    }
    poMN <- poMN[, 1:orthoI, drop = FALSE]
    toMN <- toMN[, 1:orthoI, drop = FALSE]
    woMN <- woMN[, 1:orthoI, drop = FALSE]
    coMN <- coMN[, 1:orthoI, drop = FALSE]
    orthoNamVc <- orthoNamVc[1:orthoI]
    modelDF <- modelDF[c(1:(orthoI + 1), nrow(modelDF)), ]
  }
  
  modelDF["sum", "R2X(cum)"] <- modelDF[1 + orthoI, "R2X(cum)"]
  modelDF["sum", "R2Y(cum)"] <- sum(modelDF[, "R2Y"], na.rm = TRUE)
  modelDF["sum", "Q2(cum)"] <- sum(modelDF[, "Q2"], na.rm = TRUE)
  summaryDF <- modelDF["sum", c("R2X(cum)", "R2Y(cum)", "Q2(cum)")]
  rMN <- wMN
  bMN <- tcrossprod(rMN, cMN)
  yPreScaMN <- tcrossprod(tMN, cMN)
  yPreMN <- scale(scale(yPreScaMN, FALSE, 1/ySdVn), -yMeanVn, FALSE)
  attr(yPreMN, "scaled:center") <- NULL
  attr(yPreMN, "scaled:scale") <- NULL
  
  yActMCN <- yMCN
  yActMN <- yActMCN
  summaryDF[, "RMSEE"] <- sqrt(.errorF(yActMN, yPreMN)^2 * nrow(yActMN)/(nrow(yActMN) - (1 + predI + orthoI)))
  yTestMCN <- NULL
  
  sxpVn <- sapply(1:ncol(tMN), function(h) sum(drop(tcrossprod(tMN[, h], pMN[, h])^2)))
  sxpCumN <- sum(sxpVn)
  sxoVn <- sapply(1:ncol(toMN), function(h) sum(drop(tcrossprod(toMN[, h], poMN[, h])^2)))
  sxoCumN <- sum(sxoVn)
  ssxCumN <- sxpCumN + sxoCumN
  sypVn <- sapply(1:ncol(tMN), function(h) sum(drop(tcrossprod(tMN[, h], cMN[, h])^2)))
  sypCumN <- sum(sypVn)
  syoVn <- sapply(1:ncol(toMN), function(h) sum(drop(tcrossprod(toMN[, 
                                                                     h], coMN[, h])^2)))
  syoCumN <- sum(syoVn)
  ssyCumN <- sypCumN + syoCumN
  kpN <- nrow(wMN)/(sxpCumN/ssxCumN + sypCumN/ssyCumN)
  pNorMN <- sweep(pMN, 2, sqrt(colSums(pMN^2)), "/")
  vipVn <- sqrt(kpN * (rowSums(sweep(pNorMN^2, 2, sxpVn, 
                                     "*"))/ssxCumN + rowSums(sweep(pNorMN^2, 2, sypVn, 
                                                                   "*"))/ssyCumN))
  koN <- nrow(wMN)/(sxoCumN/ssxCumN + syoCumN/ssyCumN)
  poNorMN <- sweep(poMN, 2, sqrt(colSums(poMN^2)),"/")
  orthoVipVn <- sqrt(koN * (rowSums(sweep(poNorMN^2, 
                                          2, sxoVn, "*"))/ssxCumN + rowSums(sweep(poNorMN^2, 
                                                                                  2, syoVn, "*"))/ssyCumN))

  summaryDF[, "pre"] <- predI
  summaryDF[, "ort"] <- orthoI
  rownames(summaryDF) <- "Total"
  sigNamVc <- c("R2X", "R2X(cum)", "R2Y", "R2Y(cum)", "Q2", 
                "Q2(cum)", "RMSEE", "RMSEP")
  for (namC in intersect(colnames(modelDF), sigNamVc)) modelDF[, 
                                                               namC] <- signif(modelDF[, namC], 3)
  for (namC in intersect(colnames(summaryDF), sigNamVc)) summaryDF[, 
                                                                   namC] <- signif(summaryDF[, namC], 3)
  retLs <- list(typeC = NULL, modelDF = modelDF, 
                summaryDF = summaryDF, pcaVarVn = varVn, 
                vipVn = vipVn, orthoVipVn = orthoVipVn, fitted = NULL, 
                tested = NULL, coefficients = bMN, residuals = NULL, 
                xMeanVn = xMeanVn, xSdVn = xSdVn, yMeanVn = yMeanVn, 
                ySdVn = ySdVn, xZeroVarVi = NULL, scoreMN = tMN, loadingMN = pMN, 
                weightMN = wMN, orthoScoreMN = toMN, orthoLoadingMN = poMN, 
                orthoWeightMN = woMN, cMN = cMN, uMN = uMN, weightStarMN = rMN, 
                coMN = coMN, suppLs = list(yLevelVc = NULL, naxL = naxL, nayL = nayL, nayVi = nayVi, 
                                           permMN = NULL, scaleC = scaleC, topLoadI = NULL, 
                                           yMCN = yMCN, xSubIncVarMN = NULL, xCorMN = NULL, 
                                           xModelMN = xMN, yModelMN = yMN, yPreMN = yPreMN, 
                                           yTesMN = yTesMN))

}

.errorF <- function(x, y){
  sqrt(mean(drop((x - y)^2), na.rm = TRUE))
}

.similarityF <- function(x, y) {
  return(cor(x, y, use = "pairwise.complete.obs"))
} 

