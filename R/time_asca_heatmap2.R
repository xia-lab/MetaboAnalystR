#'Plot heatmap visualization for time-series data
#'@description Plot heatmap visualization for time-series data
#'@usage PlotHeatMap2(mSetObj, imgName, format, dpi, width, smplDist, clstDist, colors, viewOpt, hiRes, sortInx, useSigFeature, drawBorder, var.inx)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param smplDist select distance measure: euclidean, pearson, or minkowski
#'@param clstDist select clustering algorithm: ward, average, complete, single 
#'@param colors select heatmap colors: bwm, gray
#'@param viewOpt select overview or detailed view: overview or detail
#'@param useSigFeature use significant features only: F or T (default false)
#'@param drawborder show cell borders: F or T (default F)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotHeatMap2<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, smplDist='pearson', 
                       clstDist='average', colors="bwm", viewOpt="overview", hiRes=FALSE, sortInx = 1, 
                       useSigFeature, drawBorder){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(sortInx == 1){
    ordInx <- order(mSetObj$dataSet$facA, mSetObj$dataSet$facB);
  }else{
    ordInx <- order(mSetObj$dataSet$facB, mSetObj$dataSet$facA);
  }
  
  new.facA <- mSetObj$dataSet$facA[ordInx];
  new.facB <- mSetObj$dataSet$facB[ordInx];
  
  # set up data set. note, need to transpose the data for two way plotting
  data <- mSetObj$dataSet$norm[ordInx, ];
  
  # use features from ANOVA2
  if(useSigFeature){
    hits <- colnames(data) %in% rownames(mSetObj$analSet$aov2$sig.mat);
    data <- mSetObj$dataSet$norm[ordInx, hits];
  }
  
  hc.dat <- as.matrix(data);
  colnames(hc.dat) <- substr(colnames(data), 1, 18) # some names are too long
  
  # set up parameter for heatmap
  suppressMessages(library(RColorBrewer));
  if(colors=="gbr"){
    colors <- colorRampPalette(c("green", "black", "red"), space="rgb")(256);
  }else if(colors == "heat"){
    colors <- heat.colors(256);
  }else if(colors == "topo"){
    colors <- topo.colors(256);
  }else if(colors == "gray"){
    colors <- colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
  }else{
    colors <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(256));
  }
  if(drawBorder){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(viewOpt == "overview"){
    if(is.na(width)){
      w <- 9;
    }else if(width == 0){
      w <- 7.2;
    }else{
      w <- 7.2;
    }
    h <- w;
    mSetObj$imgSet$heatmap <- imgName;
  }
  
  else{
    if(is.na(width)){
      minW <- 650;
      myW <- nrow(hc.dat)*11 + 150;
      if(myW < minW){
        myW <- minW;
      }   
      w <- round(myW/72,2);
    }else if(width == 0){
      w <- 7.2;
    }else{
      w <- 7.2;
    }
    if(ncol(hc.dat) >100){
      myH <- ncol(hc.dat)*12 + 120;
    }else if(ncol(hc.dat) > 50){
      myH <- ncol(hc.dat)*12 + 60;
    }else{
      myH <- ncol(hc.dat)*12 + 20;
    }
    h <- round(myH/72,2);
    
    mSetObj$imgSet$heatmap <- imgName;
  }
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  library(pheatmap);
  
  annotation <- data.frame(new.facB, new.facA);
  colnames(annotation) <- c(mSetObj$dataSet$facB.lbl, mSetObj$dataSet$facA.lbl);
  rownames(annotation) <-rownames(hc.dat); 
  
  pheatmap(t(hc.dat), 
           annotation=annotation, 
           fontsize=8, fontsize_row=8, 
           clustering_distance_rows = smplDist,
           clustering_distance_cols = smplDist,
           clustering_method = clstDist, 
           border_color = border.col,
           cluster_rows = T, 
           cluster_cols = F,
           scale = 'row', 
           color = colors);
  dev.off();
  mSetObj$analSet$htmap2 <- list(dist.par=smplDist, clust.par=clstDist);
  return(.set.mSet(mSetObj));
}

#'Perform ASCA
#'@description The ASCA algorithm was adapted from the ASCA-genes method
#'(analysis of variance (ANOVA) simultaneous component analysis)
#'by Maria Jose Nueda (mj.nueda@ua.es) and Ana Conesa (aconesa@ivia.es)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param a specify the number of components for facA
#'@param b specify the number of components for facB
#'@param c specify the number of components for interaction AB
#'@param res specify the number of model residuals
#' type is string, indicating the type of analysis
#' "abc" separately
#' "aab" facA joins with AB
#' "bab" facB joins with AB
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Perform.ASCA <- function(mSetObj=NA, a=1, b=2, x=2, res=2){
  
  mSetObj <- .get.mSet(mSetObj);
  
  X <-  mSetObj$dataSet$norm;
  Fac = c(a, b, x, res);
  
  # Designa (p x I)
  Designa <- model.matrix(~ mSetObj$dataSet$facA-1);
  
  # Designb (p x J)
  Designb <- model.matrix(~ mSetObj$dataSet$facB-1);
  
  n<-ncol(X);
  p<-nrow(X);
  I<-ncol(Designa);
  J<-ncol(Designb);
  
  Faca=Fac[1]; # number components Model a
  Facb=Fac[2]; # number components Model b
  Facab=Fac[3]; # number components Model ab to not consider interations set to 0
  Facres=Fac[4]; # number components Residues
  
  # Calculate Overall Mean
  offset<-apply(X,2,mean);
  
  # remove the overall mean from the matrix
  Xoff <- X-(cbind(matrix(1,nrow=p,ncol=1))%*%rbind(offset));
  
  Model.a<-ASCAfun1(Xoff,Designa,Faca);
  Model.b<-ASCAfun1(Xoff,Designb,Facb);
  if (Facab != 0 ) {
    Model.ab<-ASCAfun2(Xoff,Designa,Designb,Facab);
  }
  
  # Collecting models
  models <- ls(pattern="Model");
  output <- vector(mode="list");
  Xres <- Xoff;
  for (i in 1: length(models)) {
    mymodel <- get(models[i], envir=environment());
    output <- c(output, list(mymodel));
    Xres <- Xres - mymodel$X;
    rm(mymodel);
    gc();
  }
  names(output) <- models
  
  # Model.res
  Model.res <- ASCAfun.res(Xres,Facres);
  
  LIST<-list(Model.res);
  names(LIST)<-c("Model.res");
  
  mSetObj$analSet$asca <- list(
    facNum = Fac, 
    Xoff = Xoff,
    models = c(output,LIST)
  );
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA
#'@description Orthogonal PLS-DA (from ropls)
#'@usage CalculateImpVarCutoff(mSetObj, spe.thresh, lev.thresh)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param lev.thresh leverage threshold, the higher better, default more than 95 percentile of permuted leverage
#'@param spe.thresh alpha threshold, less is better, default less than 5 percentile based chi-square
#'note: spe and leverage are vectors, not a single value, but a list to store the result
#'note: the last model is Model.res, no spe
#'Calculate leverage cutoff based on permutation
#'Calculate the reference distribution of leverages
#'note: leverage.perm is a list with each member in a 3 column matrix
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CalculateImpVarCutoff <- function(mSetObj=NA, spe.thresh = 0.05, lev.thresh = 0.95){
  
  mSetObj <- .get.mSet(mSetObj);
  asca.models <- mSetObj$analSet$asca$models;
  spe.lims  <-  lev.lims <- numeric(3);
  
  md.nms <- names(asca.models)[1:3]; # note, last model is Model.res, ignore
  names(spe.lims) <- names(lev.lims)  <- md.nms;
  for (nm in md.nms) { # a, ab, b
    model <- asca.models[[nm]];
    # get SPE cutoff based on Chiq distribution
    m <- mean(model$SPE);
    v <- var(model$SPE);
    g <- v/(2*m);
    h <- 2*m*m/v;
    lim <- g*qchisq(1-spe.thresh, df=h);
    spe.lims[nm] <- lim;
  }
  mSetObj$analSet$asca$SPE.cutoff <- spe.lims;
  
  if(is.null(mSetObj$analSet$asca$leverage.perm)){
    
    mSetLev <<- mSetObj
    
    # lev.perm is a list with each 3 col matrix (lvA, lvV, lvAB)
    lev.perm <- Perform.permutation(20, Get.asca.leverage);
    
    rm(mSetLev, pos = ".GlobalEnv")
    
    # convert to a list with 3 members each is a permutation of all variable
    # for a single factor
    rowNum <- length(lev.perm);
    colNum <- ncol (mSetObj$dataSet$norm); # each col is variable
    lvA.mat <- lvB.mat <- lvAB.mat <- matrix(0, nrow = rowNum, ncol=colNum);
    for(m in 1:length(lev.perm)){
      mat <- lev.perm[[m]];
      lvA.mat[m,] <- mat[,1]; # facA
      lvB.mat[m,] <- mat[,2]; # facB
      lvAB.mat[m,] <- mat[,3]; # facAB
    }
    perm.lv <- list("Model.a"=lvA.mat,
                    "Model.b"=lvB.mat,
                    "Model.ab"=lvAB.mat);
    mSetObj$analSet$asca$leverage.perm <- perm.lv;
    rm(lev.perm);
    gc();
  }
  
  for (nm in md.nms){
    lv.mat <- mSetObj$analSet$asca$leverage.perm[[nm]];
    # get the quantile for each var
    quant1 <- apply(lv.mat, 2, quantile, probs = lev.thresh);
    # get the quantile for each model
    lim <- quantile(quant1, probs = lev.thresh);
    lev.lims[nm] <- lim;
  }
  
  mSetObj$analSet$asca$leverage.cutoff <- lev.lims;
  
  # now get all significant and outlier for each factor based on the threshold
  sig.list <- out.list <- list();
  for (nm in md.nms){
    model <- asca.models[[nm]];
    lv <- model$leverage;
    spe <- model$SPE;
    
    lv.cutoff <- mSetObj$analSet$asca$leverage.cutoff[nm];
    spe.cutoff <-  mSetObj$analSet$asca$SPE.cutoff[nm];
    
    lvInx <- lv >= lv.cutoff;
    speInx <- spe <= spe.cutoff;
    sigInx <- lvInx & speInx;
    outInx <- spe > spe.cutoff;
    
    sig.mat <- cbind(lv[sigInx], spe[sigInx]);
    colnames(sig.mat) <- c("Leverage", "SPE");
    rownames(sig.mat) <- colnames(mSetObj$dataSet$norm)[sigInx];
    # order by leverage
    ordInx <- order(sig.mat[,1], decreasing=TRUE);
    sig.mat <- sig.mat[ordInx,,drop=F];
    
    out.mat <- cbind(lv[outInx], spe[outInx]);
    colnames(out.mat) <- c("Leverage", "SPE");
    rownames(out.mat) <- colnames(mSetObj$dataSet$norm)[outInx];
    # order by SPE
    ordInx <- order(out.mat[,2], decreasing=TRUE);
    out.mat <- out.mat[ordInx,,drop=F];
    
    # must use double [[, to use dynamical name and assign arbitury list element
    sig.list[[nm]] <- sig.mat;
    out.list[[nm]]<- out.mat;
    
    nm <- gsub("\\.", "_",  nm);
    write.csv(sig.mat, file=paste("Sig_features_", nm, ".csv", sep=""));
    write.csv(out.mat, file=paste("Outliers_", nm, ".csv", sep=""));
  }
  mSetObj$analSet$asca$sig.list <- sig.list;
  mSetObj$analSet$asca$out.list <- out.list;
  return(.set.mSet(mSetObj));
}

#'Function to perform ASCA 
#'@description Perform ASCA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ASCAfun1<-function (X, Design, Fac) {
  n <- ncol(X) # number of genes
  I <- ncol(Design) # number of levels in the factor
  
  NK<-NULL
  XK<-matrix(NA,nrow=I,ncol=n)
  
  for (i in 1:I) {
    sub<-X[Design[,i]==1,]
    NK[i]<-nrow(sub)
    XK[i,]<-apply(sub,2,mean)
  }
  NK<-sqrt(NK)
  
  # Weigh the data of the Submodel with the corresponding number of measurement occasions
  XKw<- NK*XK
  
  PCA<-PCA.GENES(XKw)
  scw<-PCA$scores[,1:Fac]
  ld<-PCA$loadings[,1:Fac]
  ssq<-PCA$var.exp
  
  if(Fac==1) {
    scw<-as.matrix(scw)
    ld<-as.matrix(ld)
  }
  
  # Re-weigth the scores
  sc<-scw/NK
  XKrec<-sc%*%t(ld)
  Xa<-NULL
  TPa<-NULL
  for (i in 1:nrow(X)){
    position<-which(Design[i,]==1)
    Xa<-rbind(Xa,XK[position,])
    TPa<-rbind(TPa,XKrec[position,])
  }
  
  Ea<-Xa-TPa
  
  # leverage & SPE
  leverage<-apply(ld^2,1,sum)
  SPE<-apply(Ea^2,2,sum)
  
  output<-list(XK,sc,ld,ssq,Xa,TPa,Ea,leverage,SPE, Fac)
  names(output)<-c("data","scores","loadings","var.exp","X","TP","E","leverage","SPE", "facNum");      
  output
}

#'Function to perform ASCA 
#'@description Perform ASCA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ASCAfun2<-function (X, Desa, Desb, Fac) {
  
  n <- ncol(X) # number of genes
  I <- ncol(Desa) # number of levels in the factor TIME
  J <- ncol(Desb) # number of levels in the other factor
  
  XK1<-matrix(NA,nrow=I,ncol=n);
  for (i in 1:I) {
    sub<-X[Desa[,i]==1,]
    XK1[i,]<-apply(sub,2,mean)
  }
  
  XK2<-matrix(NA,nrow=J,ncol=n);
  for (j in 1:J) {
    sub<-X[Desb[,j]==1,]
    XK2[j,]<-apply(sub,2,mean)
  }
  
  NK<-matrix(NA,nrow=I,ncol=J)
  XK<-matrix(NA,nrow=I*J,ncol=n)
  
  nms.I <- colnames(Desa);
  nms.J <- colnames(Desb);
  row.nm <- vector(mode="character", length=I*J);
  
  k=1
  for (j in 1:J){
    for (i in 1:I){
      sub<-X[(Desa[,i]+Desb[,j])==2,]
      NK[i,j]<-sqrt(nrow(sub))
      XK[k,]<-apply(sub,2,mean)-XK1[i,]-XK2[j,];
      row.nm[k] <- paste(nms.I[i], nms.J[j], sep=".");
      k=k+1
    }
  }
  
  XKw<-XK*(as.numeric(NK))
  rownames(XKw) <- row.nm;
  
  PCA<-PCA.GENES(XKw)
  scw<-PCA$scores[,1:Fac]
  ld<-PCA$loadings[,1:Fac]
  ssq<-PCA$var.exp
  if(Fac==1) {
    scw<-as.matrix(scw)
    ld<-as.matrix(ld)
  }
  
  # Re-weigth the scores
  sc<-scw/(as.numeric(NK))
  
  XKrec<-sc%*%t(ld)
  
  Xab<-NULL
  TPab<-NULL
  for (i in 1:nrow(X)){
    position1<-which(Desa[i,]==1)
    position2<-which(Desb[i,]==1)
    Xab<-rbind(Xab,XK[I*(position2-1)+position1,])
    TPab<-rbind(TPab,XKrec[I*(position2-1)+position1,])
  }
  Eab<-Xab-TPab
  
  leverage<-apply(ld^2,1,sum)
  SPE<-apply(Eab^2,2,sum)
  
  output<-list(XK,sc,ld,ssq,Xab,TPab,Eab,leverage,SPE, Fac)
  names(output)<-c("data","scores","loadings","var.exp","X","TP","E","leverage","SPE", "facNum");
  output
}

#'Function to perform ASCA 
#'@description Perform ASCA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ASCAfun.res<-function (X, Fac) {
  PCA<-PCA.GENES(X);
  sc<-PCA$scores[,1:Fac];
  ld<-PCA$loadings[,1:Fac];
  ssq<-PCA$var.exp;
  
  if(Fac==1) {
    sc<-as.matrix(sc)
    ld<-as.matrix(ld)
  }
  TPres<-sc%*%t(ld)
  
  if(Fac==0){
    sc=0
    ld=0
    TPres<-matrix(0,nrow(X),ncol(X))
  }
  
  Eres<-X-TPres
  output<-list(sc,ld,ssq,X,TPres,Eres, Fac)
  names(output)<-c("scores","loadings","var.exp","X","TP","E", "facNum");
  output
}


#'Perform ASCA model validation by permutation
#'@description Perform ASCA model validation by permutation
#'we use Manly's unrestricted permutation of observations
#'which esentially permutes the data over all cells in the
#'designed experiment, then calculates the score for
#'each main factor or interaction components.
#'This will get the null distribution for all effects in one go
#'@usage Perform.ASCA.permute(mSetObj, perm.num)
#'@param perm.num select the number of permutations, default is 20
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Perform.ASCA.permute<-function(mSetObj=NA, perm.num=20){
  
  # since there are three factors a, b, ab, it is easier
  # to permute the data, and let factors fixed, we can pre-calculate
  # the needed information for each factor to save computation time
  
  mSetObj <- .get.mSet(mSetObj);
  
  facA <- mSetObj$dataSet$facA;
  facB <- mSetObj$dataSet$facB;
  facAB <- as.factor(paste(facA, facB, sep=":"));
  
  lvAB <- levels(facAB);
  lvAB.mat <- do.call(rbind, strsplit(lvAB, ":"))
  lvAB.mat <- cbind(lvAB,lvAB.mat);
  
  # factor size is needed for each iteration
  facA.size <- getFactorSize(facA);
  facB.size <- getFactorSize(facB);
  facAB.size <- getFactorSize(facAB);
  
  # record these information
  mSetObj$analSet$asca$perm.info <- list(
    facA.size = facA.size,
    facB.size = facB.size,
    facAB = facAB,
    facAB.size = facAB.size,
    lvAB.mat = lvAB.mat
  );

  mSetPerm <<- mSetObj
  
  perm.orig <- Get.asca.tss(dummy, perm=F);

  perm.res <- Perform.permutation(perm.num, Get.asca.tss);

  rm(mSetPerm, pos = ".GlobalEnv")

  gc(); # garbage collection
  
  # convert to matrix
  perm.res <- do.call(rbind, perm.res);
  perm.res <- rbind(perm.orig, perm.res);
  colnames(perm.res) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl, "Interaction");
  
  # calculate the significant p value as the proportion of sampled permutations better than or equal to original one
  # note, the precision is determined by the permutation number i.e. for 100 time, no better than original
  # p value is < 0.01, we can not say it is zero
  
  better.mat <- sweep(perm.res[-1,], 2, perm.res[1,]); # subtract perm.res[1,] for each permutated rows
  better.hits <- apply(better.mat>=0, 2, sum);
  
  p.vec <- better.hits/perm.num;
  p.res <- vector(mode="character", length=3);
  p.res[p.vec == 0] <- paste("p <", 1/perm.num);
  p.res[p.vec > 0] <- paste("p =", signif(p.vec[p.vec > 0], digits=5));
  
  ## added for test
  write.csv(perm.res, file="perm.res.csv");
  
  mSetObj$analSet$asca$perm.p <- p.res;
  mSetObj$analSet$asca$perm.mat <- perm.res;
  return(.set.mSet(mSetObj));
}

getFactorSize <- function(fac){
  lvs <- levels(fac);
  size.vec <- numeric(length(lvs));
  for(i in length(lvs)){
    size.vec[i] <- sum(fac == lvs[i]);
  }
  size.vec;
}

#'Function for ASCA permutation 
#'@description Dummy is used only for the purpose to maintain lapply API
#'this is used for permutation on ANOVA paritions,
#'not on the SCA/PCA part, so the number of selected components is
#'not applicable in this step
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Get.asca.tss <- function(dummy, perm=T){
  
  X <- mSetPerm$analSet$asca$Xoff;
  facA <- mSetPerm$dataSet$facA;
  facB <- mSetPerm$dataSet$facB;
  facAB <- mSetPerm$analSet$asca$perm.info$facAB;
  facA.size <- mSetPerm$analSet$asca$perm.info$facA.size;
  facB.size <- mSetPerm$analSet$asca$perm.info$facB.size;
  facAB.size <- mSetPerm$analSet$asca$perm.info$facAB.size;   
  lvAB.mat <- mSetPerm$analSet$asca$perm.info$lvAB.mat;
  
  if(perm){
    # permute factor is faster than permute data matrix?
    ordInx <- order(runif(length(mSetPerm$dataSet$facA)));
    facA <- facA[ordInx];
    facB <- facB[ordInx];
    facAB <- facAB[ordInx];
  }
  
  # BHAN: because of the mean & sd functions have been changed in latest version(R > 3.0.2)
  # to calculate column means; replace mean --> colMeans
  mnA <- by(X, facA, colMeans);
  mnB <- by(X, facB, colMeans);
  mnAB <- by(X, facAB, colMeans);
  
  # mnAB should subtract A and B effect
  for(i in 1:nrow(lvAB.mat)){
    mnAB[[lvAB.mat[i,1]]] <- mnAB[[lvAB.mat[i,1]]] - mnA[[lvAB.mat[i,2]]] - mnB[[lvAB.mat[i,3]]];
  }
  
  dist.a <- sum(unlist(lapply(mnA, sumSquare), use.names=F)*(facA.size));
  dist.b <- sum(unlist(lapply(mnB, sumSquare), use.names=F)*(facB.size));
  dist.ab <- sum(unlist(lapply(mnAB, sumSquare), use.names=F)*(facAB.size));
  
  # return all the distance
  c(dist.a, dist.b, dist.ab);
}

# euclidean distance to zero
sumSquare <- function(x){
  sum(x*x);
}

Get.asca.leverage <- function(dummy){
  
  X <- mSetLev$analSet$asca$Xoff;
  Fac <-mSetLev$analSet$asca$facNum;
  Faca=Fac[1]; # number components Model a
  Facb=Fac[2]; # number components Model b
  Facab=Fac[3]; # number components Model ab to not consider interations set to 0
  
  # permute facA and facB
  ordInx <- order(runif(length(mSetLev$dataSet$facA)));
  facA <- mSetLev$dataSet$facA[ordInx];
  facB <- mSetLev$dataSet$facB[ordInx];
  
  # Designa (p x I)
  Desa <- model.matrix(~facA-1);
  
  # Designb (p x J)
  Desb <- model.matrix(~facB-1);
  
  n<-ncol(X);
  p<-nrow(X);
  
  I <- ncol(Desa)
  J <- ncol(Desb)
  
  ################################
  ######### factor A #############
  ################################
  NK1<-numeric(I);
  XK1<-matrix(NA,nrow=I,ncol=n);
  
  for (i in 1:I) {
    sub<-X[Desa[,i]==1,]
    NK1[i]<-nrow(sub)
    XK1[i,]<-apply(sub,2,mean)
  }
  
  NK1<-sqrt(NK1);
  XKw1<- NK1*XK1;
  lvA <- Get.Leverage(XKw1, Faca);
  
  # factor B
  NK2<-numeric(J);
  XK2<-matrix(NA,nrow=J,ncol=n);
  
  for (i in 1:J) {
    sub<-X[Desb[,i]==1,]
    NK2[i]<-nrow(sub)
    XK2[i,]<-apply(sub,2,mean)
  }
  
  NK2<-sqrt(NK2);
  XKw2<- NK2*XK2;
  lvB <- Get.Leverage(XKw2, Facb);
  
  # interaction AB
  if (Facab != 0 ) {
    NK3<-matrix(NA,nrow=I,ncol=J)
    XK3<-matrix(NA,nrow=I*J,ncol=n);
    k=1
    for (j in 1:J){
      for (i in 1:I){
        sub<-X[(Desa[,i]+Desb[,j])==2,]
        NK3[i,j]<-sqrt(nrow(sub));
        XK3[k,]<-apply(sub,2,mean)-XK1[i,]-XK2[j,];
        k=k+1
      }
    }
    XKw3<-XK3*(as.numeric(NK3));
    lvAB <- Get.Leverage(XKw3, Facab);
  }else{
    lvAB <- 0;
  }
  # return all the distance, note, each is a vector
  cbind(lvA, lvB, lvAB);
}

#'Fast leverage calculation for permutation purpose
#'@description note, the leverage combines all components
#'the importance feature is for the factor not per components
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.Leverage <- function(XKw, Fac){
  PCA <- PCA.GENES(XKw);
  ld <- PCA$loadings[,1:Fac];
  if(Fac==1) {
    ld<-as.matrix(ld);
  }
  # leverage
  apply(ld^2,1,sum);
}


#'Obtain principal components into a matrix that has more variables than individuals
#'@description X is a matrix that has as columns the genes that were considered as variables in the PCA analysis.
#'First we center the matrix by columns (Xoff) and then we obtain the eigenvalues and the 
#'eigenvectors of the matrix Xoff%*%t(Xoff) and we
#'use the equivalences between the loadings and scores to obtain the solution
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PCA.GENES<-function(X){
  n<-ncol(X)
  p<-nrow(X)
  offset<-apply(X,2,mean)
  Xoff<-X-(cbind(matrix(1,p,1))%*%rbind(offset));
  Xoff <- data.matrix(Xoff);
  
  #eigen command sorts the eigenvalues in decreasing orden.
  eigen<-eigen(Xoff%*%t(Xoff)/(p-1))
  var<-cbind(eigen$values/sum(eigen$values),cumsum(eigen$values/sum(eigen$values)))
  
  loadings2<-eigen$vectors
  scores2<-t(Xoff)%*%loadings2
  
  normas2 <- sqrt(apply(scores2^2,2,sum))
  scores1 <- loadings2%*%diag(normas2);
  rownames(scores1) <- rownames(X);
  loadings1 <- scores2%*%diag(1/normas2)
  
  output <- list(eigen,var,scores1,loadings1)
  names(output) <- c("eigen","var.exp","scores","loadings")
  output
}

#'Plot scree plots for each model in ASCA
#'@description Plot scree plots for each model in ASCA
#'@usage PlotModelScree(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param imgName Input a name for the plot
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
PlotModelScree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
    }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$asca.scree <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, type=format, width=w, height=h,  bg="white");
  
  models <- mSetObj$analSet$asca$models;
  # note four plots, model a, b, ab and res
  par(mfrow=c(2,2),oma=c(0,0,3,0), cex=1.0)
  
  for ( i in 1: length (models)) {
    pc.var <- models[[i]]$var.exp[,1];
    if(length(pc.var) > 8){
      pc.var <- pc.var[1:8];
    }
    ## Correct Typo: Expalined --> ExPlained by B. Han (17 Sep 2013)
    plot(pc.var, type="b", main=paste(names(models)[[i]]),
         xlab="Component", ylab="Explained variability", axes=F);  
    axis(2);
    axis(1, at=0:length(pc.var));
    box();
  }
  title("Scree plots of each model", outer=TRUE)
  dev.off();
  return(.set.mSet(mSetObj));
  }

#'Plot score plots of each ASCA model for component 1 against time
#'@description Plot score plots of each ASCA model for component 1 against time
#'@usage PlotASCAModel(mSetObj, imgName, format="png", dpi=72, width=NA, type, colowBW)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param type select model a or b
#'@param colowBW Logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotASCAModel<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, type, colorBW=FALSE){
  mSetObj <- .get.mSet(mSetObj);
  
  if(type == "a"){
    md <- mSetObj$analSet$asca$models$Model.a;
    lbls <- as.character(levels(mSetObj$dataSet$facA));
    fac.lbl <- mSetObj$dataSet$facA.lbl;
  }else{
    md <- mSetObj$analSet$asca$models$Model.b;
    lbls <- as.character(levels(mSetObj$dataSet$facB));
    fac.lbl <- mSetObj$dataSet$facB.lbl;
  }
  pcNum <- md$facNum;
  
  # plot at most top 3 PCs
  if(pcNum > 3){
    pcNum <- 3;
  }
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  h.adj <- ifelse(md$facNum > 1, 5/6, 1)
  if(is.na(width)){
    w <- ifelse(md$facNum > 1, 6, 5);
  }else if(width == 0){
    w <- ifelse(md$facNum > 1, 6, 5);
  }else{
    w <- width;
  }
  h <- w*h.adj;
  
  if(type == "a"){
    mSetObj$imgSet$asca.modelA <- imgName;
  }else{
    mSetObj$imgSet$asca.modelB <- imgName;
  }
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow = c(1, pcNum), cex=1.0);
  for(j in 1:pcNum){
    ## add 'xlab=fac.lbl' & replace ylab as "Scores Component #1 (XX% of variation explained)"
    ## by B. Han (17 Sep 2013)
    # plot(md$scores[,j], type="b", ylab="Concentration / Intensity", col=cols[j], pch=19,
    #     main=paste(fac.lbl, ", component", j, sep=""), axes=F);
    
    # BHan: modified for Black/White color
    # cols[j] --> color
    colortype <- ifelse(colorBW, "black", (j + 1));
    plot(md$scores[,j], type="b", ylab=paste("Scores (",round(md$var.exp[j,1]*100,2),"% of variation explained)"),
         col=colortype, pch=19,
         main=paste(fac.lbl, ", component", j, sep=""), axes=F,
         xlab=fac.lbl);
    
    axis(2);
    axis(1, label=lbls, at=1:length(lbls));
    box();
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot ASCA interaction plots 
#'@description Plot ASCA interaction plots 
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param colorBW Logical, use black and white (TRUE) or colors (FALSE)
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@usage PlotInteraction(imgName, format="png", dpi=72, colowBW, width=NA)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotInteraction<-function(mSetObj=NA, imgName, format="png", dpi=72, colorBW=FALSE, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  md <- mSetObj$analSet$asca$models$Model.ab;
  ab.lbls <- as.character(levels(mSetObj$dataSet$facA));
  ba.lbls <- as.character(levels(mSetObj$dataSet$facB));
  pcNum <- md$facNum;
  if(pcNum > 3){
    pcNum <- 3;
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- ifelse(md$facNum > 1, 8, 5)
  }else if(width == 0){
    w <- ifelse(md$facNum > 1, 7, 4.6);
  }else{
    w <- width;
  }
  h <- 8;
  
  mSetObj$imgSet$asca.modelAB <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=9, type=format, bg="white");
  lmat<-matrix(1:(4*pcNum), nrow=4, byrow=F);
  lwid<-rep(4.0, pcNum);
  lhei<-rep(c(4.0, 0.4), 2);
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  
  # BHan: modified for black/white color; change line type and symbol
  numscores <- ifelse( (length(ba.lbls) > length(ab.lbls)), length(ba.lbls), length(ab.lbls) );
  if( colorBW ) {
    cols <- "black";
    linestyle<- (1:numscores) + 1;
    pchstyle <- (1:numscores) + 4;
  } else {
    cols <- (1:numscores) + 1;
    linestyle <- 1;
    pchstyle <-  19;
  }
  
  for(j in 1:pcNum){ # plot layout column wise
    scores <- md$scores[,j];
    
    #b y & a x
    md.scores <- matrix(scores, nrow=length(levels(mSetObj$dataSet$facB)), byrow=T);
    
    
    # note, matplot plot each col, need to transpose
    par(mar = c(3,4,3,2), cex=1.0);
    
    ## replace ylab as "Scores (XX% of variation explained)" by B. Han (17 Sep 2013)
    # matplot(t(md.scores), type="b", pch=19, lty=1, axes=F, col = cols,
    #        ylab="Concentration / Intensity", main=paste("Interaction, component", j, sep=""));
    
    matplot(t(md.scores), type="b", pch=pchstyle, lty=linestyle, axes=F, col = cols,
            ylab=paste("Scores (",round(md$var.exp[j,1]*100,2),"% of variation explained)"),
            main=paste("Interaction, component", j, sep=""));
    axis(1, label=ab.lbls, at=1:length(ab.lbls));
    axis(2);
    box();
    
    
    par(mar = c(0,0,0,0), cex=1.0);
    plot.new();
    # legend("center", horiz=T, legend = as.character(ba.lbls), pch=pchstyle, col=(1:length(ba.lbls))+1, lty=1, bty="n");
    legend("center", horiz=T, legend = as.character(ba.lbls), pch=pchstyle, col=cols, lty=linestyle, bty="n");
    
    #b x & a y
    op <- par(mar = c(3,4,4,2), cex=1.0);
    
    # cols <- (1:ncol(md.scores)) + 1; # duplicated
    
    ## replace ylab as "Scores (XX% of variation explained)" by B. Han (17 Sep 2013)
    # matplot(md.scores, type="b", pch=19, lty=1, col= cols, axes=F,
    #         ylab="Concentration / Intensity", main=paste("Interaction, component", j, sep=""));
    matplot(md.scores, type="b", pch=pchstyle, lty=linestyle, col= cols, axes=F,
            ylab=paste("Scores (",round(md$var.exp[j,1]*100,2),"% of variation explained)"),
            main=paste("Interaction, component", j, sep=""));
    axis(1, label=ba.lbls, at=1:length(ba.lbls));
    axis(2);
    box();
    
    op <- par(mar = c(0,0,0,0), cex=1.0);
    plot.new();
    # legend("center", horiz=T, legend = as.character(ab.lbls), pch=pchstyle, col=(1:length(ab.lbls))+1, lty=1, bty="n");
    legend("center", horiz=T, legend = as.character(ab.lbls), pch=pchstyle, col=cols, lty=linestyle, bty="n");
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Plot the important variables for each factor
#'@description Plot the important variables for each factor
#'@usage PlotASCAImpVar(imgName, format="png", dpi=72, width=NA, type)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param type select model a, b, or ab
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotAscaImpVar <- function(mSetObj=NA, imgName, format, dpi, width=NA, type){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(type == "a"){
    lvg <- mSetObj$analSet$asca$models$Model.a$leverage;
    spe <- mSetObj$analSet$asca$models$Model.a$SPE;
    lv.cutoff <- mSetObj$analSet$asca$leverage.cutoff["Model.a"];
    spe.cutoff <-  mSetObj$analSet$asca$SPE.cutoff["Model.a"];
    lbl <- mSetObj$dataSet$facA.lbl;
  }else if(type == "b"){
    lvg <- mSetObj$analSet$asca$models$Model.b$leverage;
    spe <- mSetObj$analSet$asca$models$Model.b$SPE;
    lv.cutoff <- mSetObj$analSet$asca$leverage.cutoff["Model.b"];
    spe.cutoff <- mSetObj$analSet$asca$SPE.cutoff["Model.b"];
    lbl <- mSetObj$dataSet$facB.lbl;
  }else{
    lvg <- mSetObj$analSet$asca$models$Model.ab$leverage;
    spe <- mSetObj$analSet$asca$models$Model.ab$SPE;
    lv.cutoff<- mSetObj$analSet$asca$leverage.cutoff["Model.ab"];
    spe.cutoff <- mSetObj$analSet$asca$SPE.cutoff["Model.ab"];
    lbl <- "Interaction";
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width; 
  }
  h <- w*6/7;
  
  if(type == "a"){
    mSetObj$imgSet$asca.impA<-imgName;
  }else if(type == "b"){
    mSetObj$imgSet$asca.impB<-imgName;
  }else{
    mSetObj$imgSet$asca.impAB<-imgName;
  }
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotSigVar(lvg, spe, lv.cutoff, spe.cutoff,lbl);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Supporting function for plotting important variables for each factor
#'@description Supporting function for plotting important variables for each factor
#'note, by control xpd to plot legend outside the plotting area
#'without using layout
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PlotSigVar <- function(x, y, xline, yline, title){
  
  par(mar=c(5,4,3,8), xpd=F);
  
  plot(x, y, xlab="Leverage", ylab="SPE", main=title);
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  bad.col <- rgb(0, 0, 1, 0.2);
  polygon(c(axis.lims[1], axis.lims[1], axis.lims[2], axis.lims[2]), c(yline, axis.lims[4], axis.lims[4], yline),
          col = bad.col, border = NA);
  
  good.col <- rgb(1, 0, 0, 0.2);
  polygon(c(xline, xline, axis.lims[2], axis.lims[2]), c(axis.lims[3], yline, yline, axis.lims[3]),
          col = good.col, border = NA);
  
  abline(v = xline, col="red");
  abline(h = yline, col="red");
  
  # outside x2, and middle y
  lgd.x <- axis.lims[2];
  lgd.y <- (axis.lims[4] - axis.lims[3])/2;
  
  par(xpd=T);
  legend(lgd.x, lgd.y, legend = c("Well-modelled", "Outliers"),
         fill=c(good.col, bad.col), bty="n");
  box();
}

#'Plot ASCA permutation
#'@description Plot plsda classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@usage PlotASCA.Permutation(imgName, format="png", dpi=72, width=NA)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotASCA.Permutation <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  perm.mat<-mSetObj$analSet$asca$perm.mat;
  perm.p<-mSetObj$analSet$asca$perm.p;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 9;
    
  }else{
    w <- width;
  }
  h <-2*w;
  
  mSetObj$imgSet$asca.perm <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow=c(3,1), cex=1.0);
  nms <- colnames(perm.mat);
  for(i in 1:3){
    
    limX <-range(perm.mat[,i])*c(0.9, 1.1);
    hst <- hist(perm.mat[-1,i], breaks = "FD", freq=T, xlim = limX, axes=F, bty="l",
                ylab="Frequency", xlab= 'Permutation test statistics', col="gray", main=nms[i]);
    
    ticks <- heckbert(limX[1], limX[2], 8);
    axis(1, at=ticks);
    # add the indicator using original label
    h <- max(hst$counts)
    arrows(perm.mat[1,i], h/5, perm.mat[1,i], 0, col="red", lwd=2);
    text(perm.mat[1,i], h/2, paste('Observed \n statistic \n', perm.p[i]), xpd=T);
  }
  dev.off();
  return(.set.mSet(mSetObj));
  
}
  
##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Table of features well modelled by ASCA
#'@description Table of features well modelled by ASCA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetSigTable.ASCA <- function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  
  if(nm == "Model.a"){
    nmLbl <- paste("main effect",  mSetObj$dataSet$facA.lbl);
  }else if(nm == "Model.b"){
    nmLbl <- paste("main effect",  mSetObj$dataSet$facB.lbl);
  }else{
    nmLbl <- paste("interaction effect between",  mSetObj$dataSet$facA.lbl, "and",  mSetObj$dataSet$facB.lbl);
  }
  GetSigTable( mSetObj$analSet$asca$sig.list[[nm]], paste("ASCA. The table shows features that are well modelled by ", nmLbl, ".", sep=""), mSetObj);
  return(.set.mSet(mSetObj));
}

GetAscaSigMat <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "sigA"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$sig.list[["Model.a"]])
  }
  if(type == "outA"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$out.list[["Model.a"]])
  }
  if(type == "sigB"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$sig.list[["Model.b"]]);
  }
  if(type == "outB"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$out.list[["Model.b"]]);
  }
  if(type == "sigAB"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$sig.list[["Model.ab"]]);
  }
  if(type == "outAB"){
    sig.mat <- CleanNumber(mSetObj$analSet$asca$out.list[["Model.ab"]])
  }
  
  fileNm <- paste("asca_",type, ".csv", sep="");
  write.csv(signif(sig.mat,5), file=fileNm);
  mSetObj$analSet$asca$sig.nm <- fileNm;
  return(.set.mSet(mSetObj));
}

GetAscaSigRowNames <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "sigA"){
    return(rownames(mSetObj$analSet$asca$sig.list[["Model.a"]]))
  }
  if(type == "outA"){
    return(rownames(mSetObj$analSet$asca$out.list[["Model.a"]]))
  }
  if(type == "sigB"){
    return(rownames(mSetObj$analSet$asca$sig.list[["Model.b"]]))
  }
  if(type == "outB"){
    return(rownames(mSetObj$analSet$asca$out.list[["Model.b"]]))
  }
  if(type == "sigAB"){
    return(rownames(mSetObj$analSet$asca$sig.list[["Model.ab"]]))
  }
  if(type == "outAB"){
    return(rownames(mSetObj$analSet$asca$out.list[["Model.ab"]]))
  }
  return(0);
}

GetAscaSigColNames <- function(type){
  return(c("Leverage", "SPE"));
}

GetAscaSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$asca$sig.nm
}

