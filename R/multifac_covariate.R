GetCovUpMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$cov$p.log;
  pval.no <- mSetObj$analSet$cov$p.value.no;
  red.inx<- which(mSetObj$analSet$cov$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(pval.no[red.inx], lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetCovUpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<- which(mSetObj$analSet$cov$inx.imp);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$cov$p.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetCovDnMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$cov$p.log;
  pval.no <- mSetObj$analSet$cov$p.value.no;
  blue.inx <- which(!mSetObj$analSet$cov$inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(pval.no[blue.inx], lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetCovDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<- which(!mSetObj$analSet$cov$inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$cov$p.log)[blue.inx]);
  }else{
    return("NA");
  }
}

#'Perform Random Forest Analysis
#'@description Perform Random Forest
#'@param mSetObj Input name of the created mSet Object
#'@param treeNum Input the number of trees to create, default is set to 500
#'@param tryNum Set number of tries, default is 7
#'@param randomOn Set random, default is 1
#'@param selectedMeta selected Meta elements
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

## ───────────────────────────────────────────────────────────────────────
## 1 · Random-Forest analysis (classification *or* regression)
## ───────────────────────────────────────────────────────────────────────
RF.AnalMeta <- function(mSetObj = NA,
                        treeNum = 500,
                        tryNum  = 7,
                        randomOn = 1,
                        selectedMeta) {
  save.image("rf.RData");
  mSetObj  <- .get.mSet(mSetObj)
  metaType <- mSetObj$dataSet$meta.types[selectedMeta]
  
  meta.info <- mSetObj$dataSet$meta.info
  y         <- meta.info[[selectedMeta]]        # response
  
  ## -------- predictor matrix (keep row order in sync) -----------------
  X <- mSetObj$dataSet$norm
  X <- X[match(rownames(meta.info), rownames(X)), ]
  
  ## -------- random-seed handling (unchanged) --------------------------
  if (is.null(mSetObj$dataSet$random.seeds)) {
    mSetObj$dataSet$random.seeds <- GetRandomNumbers()
    mSetObj$dataSet$cur.inx <- 0
    mSetObj$dataSet$rn.seed <- mSetObj$dataSet$random.seeds[1]
  }
  if (randomOn == -1) {
    rn.sd <- 123456
    randomness <- "constant (123456)"
  } else if (randomOn == 0) {
    rn.sd <- mSetObj$dataSet$rn.seed
    randomness <- "fix current seed"
  } else {
    cur.inx <- mSetObj$dataSet$cur.inx + 1
    rn.sd <- mSetObj$dataSet$random.seeds[cur.inx]
    mSetObj$dataSet$cur.inx <- cur.inx
    randomness <- "random"
  }
  set.seed(rn.sd)
  mSetObj$dataSet$rn.seed <- rn.sd
  
  ## -------- build the forest -----------------------------------------
  if (metaType == "disc") {
    y <- as.factor(y)
    rf_out <- randomForest::randomForest(x = X,
                                         y = y,
                                         ntree = treeNum,
                                         mtry  = tryNum,
                                         importance = TRUE,
                                         proximity = TRUE)
  } else {                          # continuous → regression
    mtry <- floor(ncol(X) / 3)
    
    y <- as.numeric(as.character(y))
    rf_out <- randomForest::randomForest(x = X,
                                         y = y,
                                         ntree = treeNum,
                                         mtry  = floor(ncol(X) / 3),
                                         importance = TRUE)   # proximity not used
  }
  
  ## -------- variable importance table --------------------------------
  impmat <- rf_out$importance
  mdAcc  <- if (metaType == "disc") "MeanDecreaseAccuracy" else "%IncMSE"
  sigmat <- impmat[, mdAcc, drop = FALSE]
  sigmat <- sigmat[order(sigmat[, 1], decreasing = TRUE), , drop = FALSE]
  sigmat <- signif(sigmat, 5)
  fast.write.csv(sigmat, file = "randomforests_sigfeatures.csv")
  
  ## -------- store & return -------------------------------------------
  mSetObj$analSet$rf         <- rf_out
  mSetObj$analSet$rf.random  <- randomness
  mSetObj$analSet$rf.sigmat  <- sigmat
  mSetObj$analSet$meta.vec.rf <- selectedMeta        # keep the name
  mSetObj$dataSet$cls.rf      <- y                   # factor or numeric
  mSetObj$dataSet$cls.rf.nm   <- selectedMeta
  mSetObj$dataSet$norm.meta   <- X
  
  .set.mSet(mSetObj)
}

## ───────────────────────────────────────────────────────────────────────
## 2 · RF model diagnostic plot (auto class / regression)
## ───────────────────────────────────────────────────────────────────────
PlotRF.ClassifyMeta <- function(mSetObj = NA,
                                imgName,
                                format = "png",
                                dpi    = default.dpi,
                                width  = NA,
                                type   = "meta") {
  
  mSetObj <- .get.mSet(mSetObj)
  rf_mod  <- mSetObj$analSet$rf
  imgName <- paste0(imgName, "dpi", dpi, ".", format)

  is.classification <- rf_mod$type == "classification"
  w <- ifelse(is.na(width), ifelse(is.classification, 9, 6), 
              ifelse(width == 0, ifelse(is.classification, 8, 5.5), width))
  h <- w * 5 / 8
  mSetObj$imgSet$rf.cls <- imgName

  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")
  
  if (is.classification) {
    par(mar = c(4, 4, 3, 2))
    cols <- rainbow(ncol(rf_mod$err.rate))
    plot(rf_mod, main = "Random Forest model (classification)", col = cols)
    legend("topright",
           legend = colnames(rf_mod$err.rate),
           lty = 1,
           col = cols,
           bty = "n")
  } else {
    par(mar = c(4, 4, 3, 1))
    plot(rf_mod, main = "OOB error (MSE)")
  }

  dev.off()
  .set.mSet(mSetObj)
}

## ───────────────────────────────────────────────────────────────────────
## 3 · Variable-importance plot (unchanged logic)
## ───────────────────────────────────────────────────────────────────────
PlotRF.VIPMeta <- function(mSetObj = NA,
                           imgName,
                           format = "png",
                           dpi    = default.dpi,
                           width  = NA,
                           type   = "meta") {
  
  mSetObj <- .get.mSet(mSetObj)
  vip.score <- rev(sort(mSetObj$analSet$rf$importance[, 1]))  # IncMSE or MDA
  imgName <- paste0(imgName, "dpi", dpi, ".", format)
  
  w <- ifelse(is.na(width), 8, ifelse(width == 0, 7, width))
  h <- w * 7 / 8
  mSetObj$imgSet$rf.imp <- imgName
  
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")
  if(mSetObj$analSet$rf$type == "regression"){
  PlotImpVarCont(mSetObj, vip.score, colnames(mSetObj$analSet$rf$importance)[1],
                 type = "meta")
  }else{
  PlotImpVarDisc(mSetObj, vip.score, colnames(mSetObj$analSet$rf$importance)[1],
                 type = "meta")
  }
  dev.off()
  .set.mSet(mSetObj)
}

PlotImpVarDisc <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE, type="meta"){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "meta"){
    norm.data <- mSetObj$dataSet$norm.meta
    cls.init <- mSetObj$dataSet$cls.rf
  }else{
    norm.data <- mSetObj$dataSet$norm
    cls.init <- mSetObj$dataSet$cls
  }


  cls.len <- length(cls.init);
  if(cls.len == 2){
    rt.mrg <- 4;
  }else if(cls.len == 3){
    rt.mrg <- 5;
  }else if(cls.len == 4){
    rt.mrg <- 6;
  }else if(cls.len == 5){
    rt.mrg <- 7;
  }else if(cls.len == 6){
    rt.mrg <- 8;
  }else{
    rt.mrg <- 11;
  }
  op <- par(mar=c(5,7,3,rt.mrg)); # set right side margin with the number of class
  
  if(feat.num <= 0){
    feat.num = 15;
  }
  
  if(feat.num > length(imp.vec)){
    feat.num <- length(imp.vec);
  }
  
  # first get the top subset
  imp.vec <- rev(sort(imp.vec))[1:feat.num];
  
  # reverser the order for display
  imp.vec <- sort(imp.vec);
  
  # as data should already be normalized, use mean/median should be the same
  # mns is a list contains means of all vars at each level
  # conver the list into a matrix with each row contains var averages across different lvls
  mns <- by(norm.data[, names(imp.vec)], cls.init,
            function(x){ # inner function note, by send a subset of dataframe
              apply(x, 2, mean, trim=0.1)
            });
  mns <- t(matrix(unlist(mns), ncol=feat.num, byrow=TRUE));
  
  # vip.nms <-substr(names(imp.vec), 1, 12);
  vip.nms <- substr(names(imp.vec), 1, 14);
  names(imp.vec) <- NULL;
  
  # modified for B/W color
  dotcolor <- ifelse(color.BW, "darkgrey", "#585855");
  dotchart(imp.vec, bg=dotcolor, xlab= xlbl, cex=1.3);
  
  mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)
  
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  # get character width
  shift <- 2*par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  
  x <- rep(lgd.x, feat.num);
  y <- 1:feat.num;
  par(xpd=T);
  
  nc <- ncol(mns);
  
  # modified for B/W color
  colorpalette <- ifelse(color.BW, "Greys", "RdYlBu");
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, colorpalette))(nc); # set colors for each class
  if(color.BW) col <- rev(col);
  
  # calculate background
  bg <- matrix("", nrow(mns), nc);
  for (m in 1:nrow(mns)){
    bg[m,] <- (col[nc:1])[rank(mns[m,])];
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(cls.init))[cls.init]);
  }else{
    cls <- cls.init;
  }
  
  cls.lbl <- levels(cls);
  
  for (n in 1:ncol(mns)){
    points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
    # now add label
    text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
    # shift x, note, this is good for current size
    x <- x + shift/1.25;
  }
    
  # now add color key, padding with more intermediate colors for contiuous band
  ncolor <- 9;
  if(colorpalette == "RdYlBu"){
    ncolor <- 11;
  }
  col <- colorRampPalette(RColorBrewer::brewer.pal(ncolor, colorpalette))(50)
  if(color.BW) col <- rev(col);
  
  nc <- length(col);
  x <- rep(x[1] + shift, nc);
  
  shifty <- (axis.lims[4]-axis.lims[3])/3;
  starty <- axis.lims[3] + shifty;
  endy <- axis.lims[3] + 2*shifty;
  y <- seq(from = starty, to = endy, length = nc);
  
  points(x,y, bty="n", pch=15, col=rev(col), cex=2);
  
  text(x[1], endy+shifty/8, "High");
  text(x[1], starty-shifty/8, "Low");
  
  par(op);
}



#'Plot PLS important variables,
#'@description Plot PLS important variables, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imp.vec Input the vector of important variables
#'@param xlbl Input the x-label
#'@param feat.num Numeric, set the feature numbers, default is set to 15
#'@param color.BW Use black-white for plot (T) or colors (F)
#'@param type type, default is "type"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotImpVarCont <- function(mSetObj  = NA,
                           imp.vec  ,
                           xlbl     ,
                           feat.num = 15,
                           nBins    = 3,
                           color.BW = FALSE,
                           type     = "meta") {
  
  mSetObj <- .get.mSet(mSetObj)
  rf_mod  <- mSetObj$analSet$rf
  
  ## ── top N features ------------------------------------------------
  feat.num <- max(1, min(feat.num, length(imp.vec)))
  imp.top  <- sort(rev(sort(imp.vec))[1:feat.num])
  full.nms <- names(imp.top)
  short.nms<- substr(full.nms, 1, 14)
  names(imp.top) <- NULL
  
  ## ── fetch data and response ---------------------------------------
  if (type == "meta") {
    norm.data <- mSetObj$dataSet$norm.meta
    y         <- as.numeric(mSetObj$dataSet$cls.rf)
  } else {
    norm.data <- mSetObj$dataSet$norm
    y         <- as.numeric(mSetObj$dataSet$cls)
  }
  
  ## ── check feature presence ----------------------------------------
  valid.nms <- full.nms[full.nms %in% colnames(norm.data)]
  if (length(valid.nms) == 0) {
    warning("None of the importance-ranked features are in normalized data.")
    return(invisible(.set.mSet(mSetObj)))
  }
  full.nms <- valid.nms
  feat.num <- length(full.nms)
  short.nms <- substr(full.nms, 1, 14)
  
  ## ── bin y into groups using quantiles -----------------------------
  bins <- cut(
    y,
    breaks = quantile(y, probs = seq(0, 1, length.out = nBins + 1), na.rm = TRUE),
    include.lowest = TRUE,
    labels = paste0("Bin", seq_len(nBins))
  )
  
  ## ── compute trimmed means in each bin -----------------------------
  trimmed.means <- by(
    norm.data[, full.nms, drop = FALSE],
    bins,
    function(x) apply(x, 2, mean, trim = 0.1)
  )
  mat <- do.call(rbind, trimmed.means)
  if (is.null(dim(mat)) || any(dim(mat) == 0)) {
    warning("Failed to compute trimmed means.")
    return(invisible(.set.mSet(mSetObj)))
  }
  
  ## ── build heatmap coloring matrix ---------------------------------
  palName <- if (color.BW) "Greys" else "Blues"
  binCols <- colorRampPalette(RColorBrewer::brewer.pal(10, palName))(nBins)
  rankMat <- t(apply(mat, 2, rank))
  
  op <- par(mar = c(5, 7, 3, 3))  # remove extra right space
  
  ## ── gradient color for main dots ----------------------------------
  colfunc <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))
  clr.vec <- colfunc(length(imp.top))
  ord     <- order(imp.top)
  cols.ordered <- clr.vec
  cols.ordered[ord] <- clr.vec
  
  ## ── draw importance dot chart -------------------------------------
dotchart(imp.top,
         pch = 21,
         xlab = xlbl,
         cex  = 1.3,               # keep axis label size normal
         bg   = "white")         # dummy fill, we will override below

## overlay larger colored points (30% larger)
points(x = imp.top,
       y = seq_along(imp.top),
       pch = 21,
       bg  = cols.ordered,
       cex = 1.6)  # this is the increased dot size

  mtext(side = 2,
        at    = seq_len(feat.num),
        text  = short.nms,
        las   = 2,
        line  = 1)
  
  par(op)
  invisible(.set.mSet(mSetObj))
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetCovSigFileName <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$cov$sig.nm;
}

GetCovSigMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);

  return(CleanNumber(as.matrix(mSetObj$analSet$cov$sig.mat)));
}

GetCovSigRowNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$cov$sig.mat);
}

GetCovSigColNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$cov$sig.mat);
}


#' CovariateScatter.Anal
#' @param mSetObj mSetObj object
#' @param imgName image name
#' @param format image format
#' @param analysis.var variable of analysis
#' @param ref reference group
#' @param block block name
#' @param thresh threshold
#' @param pval.type pvalue type (raw or fdr)
#' @param contrast.cls contrast group
#' @export
CovariateScatter.Anal <- function(mSetObj, 
                                  imgName="NA", 
                                  format="png", 
                                  analysis.var, 
                                  ref = NULL, 
                                  block = "NA", 
                                  thresh=0.05,
                                  pval.type="fdr",
                                  contrast.cls = "anova"){
  # load libraries
  library(limma)
  library(dplyr)
  
  imgName <<- imgName;
  format<- "png";
  analysis.var<<- analysis.var; 
  ref <<- ref;
  block <<- block;
  thresh<<- thresh;
  pval.type<<- pval.type;
  contrast.cls <<- contrast.cls;
  #save.image("cov.RData");
  cov.vec <- "NA";
  # get inputs
  if(!exists('adj.vec')){
    adj.bool = F;
    adj.vec = "NA"
    vars <- analysis.var;
  }else{
    if(length(adj.vec) > 0){
      adj.bool = T;
      vars <- c(analysis.var, adj.vec);
      cov.vec <- adj.vec;
    }else{    
      adj.vec= "NA"
      adj.bool = F;
      vars <- analysis.var;
    }
  }
  
  mSetObj <- .get.mSet(mSetObj);
  
  covariates <- mSetObj$dataSet$meta.info
  var.types <- mSetObj[["dataSet"]][["meta.types"]]
  feature_table <- t(mSetObj$dataSet$norm);
  
  # process inputs
  ref <- make.names(ref)
  contrast.cls <- make.names(contrast.cls)
  analysis.type <- unname(mSetObj$dataSet$meta.types[analysis.var]);
  
  # process metadata table (covariates)
  for(i in c(1:length(var.types))){ # ensure all columns are the right type
    if(var.types[i] == "disc"){
      covariates[,i] <- covariates[,i] %>% make.names() %>% factor()
    } else {
      covariates[,i] <- covariates[,i] %>% as.character() %>% as.numeric()
    }
  }
  
  covariates <- covariates[match(colnames(feature_table), rownames(covariates)),]
  if (block != "NA"){    
    if(mSetObj$dataSet$meta.types[block] == "cont"){
      AddErrMsg("Blocking factor can not be continuous data type.")
      return(c(-1,-1));
    }
    # recent update: remove check for unbalanced design. Limma can handle.
  }
  
  sig.num <- 0;
  
  if(analysis.type == "disc"){
    # build design and contrast matrix 
    covariates[, analysis.var] <- covariates[, analysis.var] %>% make.names() %>% factor();
    grp.nms <- levels(covariates[, analysis.var]);
    design <- model.matrix(formula(paste0("~ 0", paste0(" + ", vars, collapse = ""))), data = covariates);
    colnames(design)[1:length(grp.nms)] <- grp.nms;
    myargs <- list();
    
    if(contrast.cls == "anova"){
      cntr.cls <- grp.nms[grp.nms != ref];
      myargs <- as.list(paste(cntr.cls, "-", ref, sep = ""));
    } else {
      myargs <- as.list(paste(contrast.cls, "-", ref, sep = ""));
    }
    #################handle issues with factor name with space in the value
    myargs <- lapply(myargs, function(x) gsub(" ", ".", x))  # Convert spaces to dots
    myargs[["levels"]] <- design;
    contrast.matrix <- do.call(makeContrasts, myargs);
    
    # handle blocking factor
    if (block == "NA") {
      fit <- lmFit(feature_table, design)
    } else {
      block.vec <- covariates[,block];
      corfit <- duplicateCorrelation(feature_table, design, block = block.vec)
      fit <- lmFit(feature_table, design, block = block.vec, correlation = corfit$consensus)
    }
    
    fit <- contrasts.fit(fit, contrast.matrix);
    fit <- eBayes(fit);
    rest <- topTable(fit, number = Inf);
    
    ### get results with no adjustment
    design <- model.matrix(formula(paste0("~ 0", paste0(" + ", analysis.var, collapse = ""))), data = covariates);
    colnames(design)[1:length(grp.nms)] <- grp.nms;
    myargs[["levels"]] <- design;
    contrast.matrix <- do.call(makeContrasts, myargs);
    fit <- lmFit(feature_table, design)
    fit <- contrasts.fit(fit, contrast.matrix);
    fit <- eBayes(fit);
    res.noadj <- topTable(fit, number = Inf);
    
  } else { 
    covariates[, analysis.var] <- covariates[, analysis.var] %>% as.numeric();
    types <- unname(mSetObj$dataSet$meta.types[vars])
    if(sum(types == "cont") == length(vars)){ #in case of single, continuous variable, must use different intercept or limma will give unreasonable results
      design <- model.matrix(formula(paste0("~", paste0(" + ", vars, collapse = ""))), data = covariates);
    } else {
      design <- model.matrix(formula(paste0("~ 0", paste0(" + ", vars, collapse = ""))), data = covariates);
    }
    
    # recent update: enable blocking factor for continuous primary metadata
    if (block == "NA") {
      # print(head(design));
      fit <- lmFit(feature_table, design)
    } else {
      block.vec <- covariates[,block];
      corfit <- duplicateCorrelation(feature_table, design, block = block.vec)
      fit <- lmFit(feature_table, design, block = block.vec, correlation = corfit$consensus)
    }
    
    fit <- eBayes(fit);
    rest <- topTable(fit, number = Inf, coef = analysis.var);
    colnames(rest)[1] <- analysis.var;
    
    ### get results with no adjustment
    design <- model.matrix(formula(paste0("~", analysis.var)), data = covariates);
    
    fit <- eBayes(lmFit(feature_table, design));
    res.noadj <- topTable(fit, number = Inf);
  }
  
  
  # make visualization
  adj.mat <- rest[, c("P.Value", "adj.P.Val")]
  noadj.mat <- res.noadj[, c("P.Value", "adj.P.Val")]
  
  colnames(adj.mat) <- c("pval.adj", "fdr.adj")
  colnames(noadj.mat) <- c("pval.no", "fdr.no")
  
  both.mat <- merge(adj.mat, noadj.mat, by = "row.names")
  both.mat$pval.adj <- -log10(both.mat$pval.adj)
  both.mat$fdr.adj <- -log10(both.mat$fdr.adj)
  both.mat$pval.no <- -log10(both.mat$pval.no)
  both.mat$fdr.no <- -log10(both.mat$fdr.no)
  
  # make plot
  if( "F" %in% colnames(rest)){
    fstat <- rest[, "F"];
  }else{
    fstat <- rest[, "t"];
  }    
  
  p.value <- rest[,"P.Value"];
  fdr.p <- rest[,"adj.P.Val"];
  names(fstat) <- names(p.value) <- names(fdr.p) <- colnames(mSetObj$dataSet$norm);
  
  # sort and save a copy 
  fileName <- "covariate_result.csv";
  my.ord.inx <- order(p.value, decreasing = FALSE);
  rest <- signif(rest[my.ord.inx,],5);
  fast.write.csv(rest,file=fileName);
  qs::qsave(rest, file = "covariate_result.qs");
  
  # note the plot is always on raw scale for visualization purpose
  if(pval.type=="fdr"){
    inx.imp <- fdr.p <= thresh;
    # locate the cutoff on the sorted raw p value
    raw.thresh <- mean(c(p.value[sum(inx.imp)], p.value[sum(inx.imp)+1]),na.rm = T);
  }else{ # raw p value
    inx.imp <- p.value <= thresh;
    raw.thresh <- thresh;
  }
  sig.num <- sum(inx.imp);
  
  if(sig.num > 0){ 
    sig.p <- p.value[inx.imp];
    sig.mat <- signif(rest[inx.imp, ,drop=FALSE], 5);
    rownames(sig.mat) <- rownames(rest)[inx.imp]
    # order the result simultaneously
    ord.inx <- order(sig.p, decreasing = FALSE);
    sig.mat <- sig.mat[ord.inx,,drop=F];
  }else{
    # just top 10
    sig.mat <- rest[1:10,]; 
  }
  
  AddMsg(paste(c("A total of", sum(inx.imp), "significant features were found."), collapse=" "));
  rownames(both.mat) = both.mat[,1]
  both.mat <- both.mat[rownames(rest),];
  fast.write.csv(signif(both.mat[,-1],5),file="covariate_adj_vs_none.csv");
  if(sig.num> 0){
    res <- 1;
    #fileName <- "covariate_result.csv"
    #fast.write.csv(sig.mat,file=fileName);
    cov<-list (
      pval.type=pval.type,
      sig.num = sig.num,
      sig.nm = fileName,
      p.thresh = thresh,
      raw.thresh = raw.thresh,
      thresh = -log10(raw.thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      sig.mat = sig.mat,
      primary.var = analysis.var,
      cov.var = cov.vec,
      block = block
    );
  }else{
    res <- 0;
    cov<-list (
      pval.type = pval.type,
      sig.num = sig.num,
      raw.thresh = raw.thresh,
      thresh = -log10(raw.thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      sig.mat = sig.mat,
      primary.var = analysis.var,
      cov.var = cov.vec,
      block = block
    );
  }
  
  # for detail table
  mSetObj$analSet$cov <- cov; 
  # for plotting adjp vs p
  mSetObj$analSet$cov.mat <- both.mat; 
  
  jsonNm <- gsub(paste0(".", format), ".json", imgName);
  jsonObj <- RJSONIO::toJSON(both.mat);
  sink(jsonNm);
  cat(jsonObj);
  sink();
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(res);
  }else{
    return(.set.mSet(mSetObj));
  } 
}


CombineFacScatter.Anal <- function(mSetObj, 
                                  imgName="NA", 
                                  format="png", 
                                  meta0="NA",
                                  meta1="NA",
                                  anal.type = "ref",
                                  par1 = NULL, par2 = NULL, 
                                  nested.opt = "intonly",
                                  block = "NA", 
                                  thresh=0.05,
                                  pval.type="fdr"){

 mSetObj <- prepareLimmaContrast(meta0,meta1,anal.type ,par1,par2,nested.opt);
 
 if(!is.list(mSetObj)){
    return(-1);
}
  design <- mSetObj$analSet$design;
  dataSet<- mSetObj$dataSet
  contrast.matrix <- mSetObj$analSet$contrast.matrix;
  feature_table <- t(mSetObj$dataSet$norm);
  if(length(dataSet$rmidx)>0){
    data.norm <- feature_table[,-dataSet$rmidx]
  }else{
    data.norm <- feature_table
  }
  

    if (block=="NA") {
      fit <- lmFit(data.norm, design)
    } else {
     block.vec <-  mSetObj$analSet$combFacdf[,block];
      corfit <- duplicateCorrelation(data.norm, design, block = block.vec)
      fit <- lmFit(data.norm, design, block = block.vec, correlation = corfit$consensus)
    }
    
    if (!is.fullrank(design)) {
      AddErrMsg("This metadata combination is not full rank! Please use other combination.");
      return(-1)
    }
    
    df.residual <- fit$df.residual
    if (all(df.residual == 0)) {
      AddErrMsg("All residuals equal 0. There is not enough replicates in each group (no residual degrees of freedom)!");
      return(-1);
    }
    fit2 <- contrasts.fit(fit, contrast.matrix);
    fit2 <- eBayes(fit2, trend=F, robust=F);
    rest <- topTable(fit2, number = Inf, adjust.method = "fdr");

   colnames(rest)= gsub("\\X.","",colnames(rest))
   colnames(rest) <- gsub("\\.\\.\\.", "-", colnames(rest))
   colnames(rest) <- gsub("\\.$", "-", colnames(rest))
 
    fit <- lmFit(data.norm,  mSetObj$analSet$design.noadj)
    fit <- contrasts.fit(fit, mSetObj$analSet$contrast.matrix.noadj);
    fit <- eBayes(fit);
    res.noadj <- topTable(fit, number = Inf);
 
   # make visualization
  adj.mat <-   rest[, c("P.Value", "adj.P.Val")]
  noadj.mat <-  res.noadj[, c("P.Value", "adj.P.Val")]
  colnames(adj.mat) <- c("pval.adj", "fdr.adj")
  colnames(noadj.mat) <- c("pval.no", "fdr.no")
  
  both.mat <- merge(adj.mat, noadj.mat, by = "row.names")
  both.mat$pval.adj <- -log10(both.mat$pval.adj)
  both.mat$fdr.adj <- -log10(both.mat$fdr.adj)
  both.mat$pval.no <- -log10(both.mat$pval.no)
  both.mat$fdr.no <- -log10(both.mat$fdr.no)
  
  # make plot
  if( "F" %in% colnames(rest)){
    fstat <- rest[, "F"];
  }else{
    fstat <- rest[, "t"];
  }    
  
  p.value <- rest[,"P.Value"];
  fdr.p <- rest[,"adj.P.Val"];
  names(fstat) <- names(p.value) <- names(fdr.p) <- colnames(mSetObj$dataSet$norm);
  
  # sort and save a copy;
  my.ord.inx <- order(p.value, decreasing = FALSE);
  rest <- signif(rest[my.ord.inx,],5);
  fast.write.csv(rest,file= mSetObj$analSet$combineFac_filename);
  qs::qsave(rest, file = "combine_factors_result.qs");
  
  # note the plot is always on raw scale for visualization purpose
  if(pval.type=="fdr"){
    inx.imp <- fdr.p <= thresh;
    # locate the cutoff on the sorted raw p value
     raw.thresh <- mean(c(p.value[sum(inx.imp)], p.value[sum(inx.imp)+1]),na.rm = T);
  }else{ # raw p value
    inx.imp <- p.value <= thresh;
    raw.thresh <- thresh;
  }
  sig.num <- sum(inx.imp);
 
  if(sig.num > 0){ 
    sig.p <- p.value[inx.imp];
    sig.mat <- signif(rest[inx.imp, ,drop=FALSE], 5);
    rownames(sig.mat) <- rownames(rest)[inx.imp]
    # order the result simultaneously
    ord.inx <- order(sig.p, decreasing = FALSE);
    sig.mat <- sig.mat[ord.inx,,drop=F];
  }else{
    # just top 10
    sig.mat <- rest[1:10,]; 
  }
  
  AddMsg(paste(c("A total of", sum(inx.imp), "significant features were found."), collapse=" "));
  rownames(both.mat) = both.mat[,1]
  both.mat <- both.mat[rownames(rest),];
   if(sig.num> 0){
    res <- 1;
     cov<-list (
      pval.type=pval.type,
      sig.num = sig.num,
      sig.nm = mSetObj$analSet$combineFac_filename,
      p.thresh = thresh,
      raw.thresh = raw.thresh,
      thresh = -log10(raw.thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      sig.mat = sig.mat,
      primary.var = meta0,
      second.var = meta1,
      block = block
    );
  }else{
    res <- 0;
    cov<-list (
      pval.type = pval.type,
      sig.num = sig.num,
      raw.thresh = raw.thresh,
      thresh = -log10(raw.thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      sig.mat = sig.mat,
       primary.var = meta0,
      second.var = meta1,
      block = block
    );
  }
  
  # for detail table
  mSetObj$analSet$cov <- cov; 
  # for plotting adjp vs p
  mSetObj$analSet$cov.mat <- both.mat; 
  
  jsonNm <- gsub(paste0(".", format), ".json", imgName);
  jsonObj <- RJSONIO::toJSON(both.mat);
  sink(jsonNm);
  cat(jsonObj);
  sink();
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(res);
  }else{
    return(.set.mSet(mSetObj));
  } 
}

prepareLimmaContrast <-function(meta0="NA",meta1="NA",anal.type = "ref", par1 = NULL, par2 = NULL, nested.opt = "intonly"){
  library(limma)
  library(dplyr)
  
  if(!exists('adj.vec')){
    adj.bool = F;
    adj.vec= "NA"
  }else{
    if(length(adj.vec) > 0){
      adj.bool = T;
      cov.vec <- adj.vec;
    }else{    
      adj.vec= "NA"
      adj.bool = F; 
    }
  }
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.factor(mSetObj$analSet$combFac)){
    mSetObj$analSet$combFac <- factor(mSetObj$analSet$combFac)
  }
  dataSet<- mSetObj$dataSet
  cat(meta0,meta1,anal.type, par1, par2, nested.opt, "\n")
  set.seed(1337);
  myargs <- list();
  cls <- mSetObj$analSet$combFac;
  clsdf =  mSetObj$analSet$combFacdf;
  mSetObj$analSet$comp.type <- anal.type;
  grp.nms <- levels(cls);
  if (is_purely_numeric(grp.nms)) {
    grp.nms <- make.names(as.character(paste0("Group_", grp.nms)))
  } else {
    grp.nms <- make.names(grp.nms)
  }
  analysisVar <-  meta0;
  
  if (anal.type == "inter") { 
    if(mSetObj$dataSet$meta.types[meta1]=="disc"){
      formula <- paste("~ ", meta0, "*", meta1)
    }else{
      clsdf[,meta1] = as.numeric( clsdf[,meta1])
      formula <- as.formula(paste("~ 0+", meta0, "+",meta0, ":" ,meta1 ))
    }
    formula.noadj <- as.formula(formula)
    if(adj.bool){
      formula <- as.formula(paste(formula," + ",cov.vec))
      design <- model.matrix(formula, data = clsdf)
      design.noadj <- model.matrix(formula.noadj, data = clsdf)
    }else{
      design <- design.noadj <- model.matrix(formula.noadj, data = clsdf);  
    }
    if (is_purely_numeric(colnames(design))) {
      colnames(design) <- make.names(as.character(paste0("Group_", colnames(design))))
      colnames(design.noadj) <- make.names(as.character(paste0("Group_", colnames(design.noadj))))
    } else {
      colnames(design) <- make.names(colnames(design))
      colnames(design.noadj) <- make.names(colnames(design.noadj))
    }
  }else{
    
    design.noadj <-   model.matrix(~ 0 + cls)
    colnames(design.noadj) <- levels(cls); 
    if(adj.bool){
      design <- model.matrix(~ 0 + cls + clsdf[,cov.vec])
      colnames(design)[1:length(levels(cls))] <- levels(cls)
      colnames(design)[(length(levels(cls))+1):ncol(design)]  <- make.names(colnames(design)[(length(levels(cls))+1):ncol(design)]); 
    }else{
      
      design <-  design.noadj
    } 
  }
  
  if(dataSet$meta.types[analysisVar]=="cont" |  any(grepl("(^[0-9]+).*", grp.nms))){
    if(grepl( "vs",par1)){
      par1 <- strsplit(par1, " vs. ")[[1]]
      par1 <- paste0(analysisVar,"_",par1[1]," vs. ",analysisVar,"_",par1[2])
    }else{
      par1<- paste0(analysisVar,"_",par1)
    }
    if(par2 != "NA"){
      if(grepl( "vs",par2)){
        par2 <- strsplit(par2, " vs. ")[[1]]
        par2 <- paste0(analysisVar,"_",par2[1]," vs. ",analysisVar,"_",par2[2])
      }else{
        par2<- paste0(analysisVar,"_",par2)
      }
    }
    
    if(any(grepl("(^[0-9]+).*",  colnames(design)))){
      colnames(design) = as.character(sapply( colnames(design),function(x) paste0(analysisVar,"_",x)))
      colnames(design.noadj) = as.character(sapply( colnames(design.noadj),function(x) paste0(analysisVar,"_",x)))
    }
    grp.nms <- paste0(analysisVar,"_",grp.nms)
    
  }
  mSetObj$analSet$design <- design
  mSetObj$analSet$design.noadj <- design.noadj
  mSetObj$analSet$par1 <- par1
  
  if (anal.type == "inter") {
    kpidx <-  grepl(meta0,colnames(design)) |(grepl(meta1,colnames(design)) & meta1!="NA")
    myargs <- as.list(colnames(design)[kpidx])  
    filename <- paste("combine_factors_interaction", meta0,"_",meta1, sep = "");
  }else if (anal.type == "custom") {
    grp.nms <- strsplit(par1, " vs. ")[[1]]
    myargs[[1]] <- paste(grp.nms, collapse = "-")
    mSetObj$analSet$grp.nms <- grp.nms;
    filename <- paste("combine_factors_", paste(grp.nms, collapse = "_vs_"), sep = "")
    mSetObj$analSet$contrast <- paste(grp.nms, collapse = "_vs_");
  } else if (anal.type == "ref") {
    ref <- par1;
    cntr.cls <- grp.nms[grp.nms != ref]
    myargs <- as.list(paste(cntr.cls, "-", ref, sep = ""));
    mSetObj$analSet$ref <- ref;    
    filename <- paste("combine_factors_reference_", ref, sep = "");
  } else if (anal.type == "nested") {
    grp.nms1 <- strsplit(par1, " vs. ")[[1]]
    grp.nms2 <- strsplit(par2, " vs. ")[[1]]
    if (all(grp.nms1 == grp.nms2)) {
      AddErrMsg("The two nested groups are the same. Please choose two different groups.")
      return(0)
    }
    grp.nms <- unique(c(grp.nms1, grp.nms2))
    if (nested.opt) {
      mSetObj$analSet$nested.int.opt <- "True";
      myargs[[1]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    } else {
      mSetObj$analSet$nested.int.opt <- "False";
      myargs[[1]] <- paste(grp.nms1, collapse = "-")
      myargs[[2]] <- paste(grp.nms2, collapse = "-")
      myargs[[3]] <- paste("(", paste(grp.nms1, collapse = "-"), ")-(", paste(grp.nms2, collapse = "-"), ")", sep = "")
    }
    mSetObj$analSet$contrast <- paste(paste(paste(grp.nms1, collapse = "_vs_"), "_", paste(grp.nms2, collapse = "_vs_"), sep = ""), sep = "")
    filename <- paste("combine_factors_nested_", paste(paste(grp.nms1, collapse = "_vs_"), "_", paste(grp.nms2, collapse = "_vs_"), sep = ""), sep = "")
  } else {
    print(paste("Not supported: ", anal.type))
  }
  
  mSetObj$analSet$combineFac_filename <- paste0(filename,".csv");
  mSetObj$analSet$contrast.type <- anal.type;
  if (is_purely_numeric(colnames(design))) {
    colnames(design) <- make.names(as.character(paste0("Group_", colnames(design))))
  }
  myargs[["levels"]] <- design
  contrast.matrix <- do.call(makeContrasts, myargs);
  mSetObj$analSet$contrast.matrix <- contrast.matrix;
  if (is_purely_numeric(colnames(design.noadj))) {
    colnames(design.noadj) <- make.names(as.character(paste0("Group_", colnames(design.noadj))))
  }
  myargs[["levels"]] <- design.noadj;
  contrast.matrix.noadj <- do.call(makeContrasts, myargs);
  mSetObj$analSet$contrast.matrix.noadj <- contrast.matrix.noadj;
  
  return(mSetObj);
}

is_purely_numeric <- function(x) {
  all(grepl("^\\d+$", x))  # Returns TRUE if all elements are purely numeric
}

GetRawCovThresh <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj); 
  return(mSetObj$analSet$cov$thresh);
}

convertCovariate2Fun <- function(){
    if(!file.exists("covariate_result.qs")){
        return(0)
    }
    dt <- qs::qread("covariate_result.qs");
    features <- rownames(dt)
    
    mzs <- vapply(features, function(x){
      strsplit(x, "__")[[1]][1]
    }, character(1L));
    rts <- vapply(features, function(x){
      strsplit(x, "__")[[1]][2]
    }, character(1L));
    if("t" %in% colnames(dt)){
      df <- data.frame(mz = mzs, rt = rts, tscore = dt$t, pvalue = dt$adj.P.Val)
    } else {
      df <- data.frame(mz = mzs, rt = rts, pvalue = dt$adj.P.Val)
    }
    
    write.table(df, file = "metaboanalyst_input_functional_analy.txt", quote = F, row.names = F, sep = "\t")
    return(1)
}

PlotCovariateMap <- function(mSetObj, theme="default", imgName="NA", format="png", dpi=default.dpi, interactive=F){
  mSetObj <- .get.mSet(mSetObj); 
  both.mat <- mSetObj$analSet$cov.mat
  both.mat <- both.mat[order(-both.mat[,"pval.adj"]),]
  logp_val <- mSetObj$analSet$cov$thresh
  topFeature <- 5;
  if(nrow(both.mat) < topFeature){
    topFeature <- nrow(both.mat);
  }
  
  mSetObj$imgSet$covAdj <- imgName;
  
  width <- 11;
  height <- 8.18;
  
  library(plotly)
  threshold <- logp_val               
  
  both.mat$category <- with(both.mat, dplyr::case_when(
    pval.no > threshold & pval.adj > threshold ~ "Significant in both",
    pval.no > threshold & pval.adj <= threshold ~ "Significant in pval.no only",
    pval.adj > threshold & pval.no <= threshold ~ "Significant in pval.adj only",
    TRUE ~ "Non-significant"
  ))
  
  # Define a list or data frame mapping categories to properties
  category_properties <- data.frame(
    category = c("Significant in both", "Significant in pval.no only", 
                 "Significant in pval.adj only", "Non-significant"),
    color = c('#6699CC', '#94C973', '#E2808A', 'grey'),
    name = c("Significant", "Non-Sig. after adjustment", 
             "Sig. after adjustment", "Non-Significant")
  )
  
  p <- ggplot(both.mat, aes(x = pval.no, y = pval.adj, color = category, text = paste("Feature:", Row.names, 
                                                                                               "<br>Adjusted Pval:", signif(10^(-pval.adj), 4), 
                                                                                               "<br>Non-adjusted Pval:", signif(10^(-pval.no), 4)))) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = setNames(category_properties$color, category_properties$category), name="") +
    labs(x = "-log10(P-value): no covariate adjustment", y = "-log10(P-value): adjusted") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  if(interactive){
    library(plotly);
    ggp_build <- layout(ggplotly(p,width = 800, height = 600, tooltip = c("text")), autosize = FALSE, margin = mSetObj$imgSet$margin.config)
    return(ggp_build);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=width, height=height, type=format);    
    print(p)
    dev.off()
    return(.set.mSet(mSetObj));
  }
}

FeatureCorrelationMeta <- function(mSetObj=NA, dist.name="pearson", tgtType, varName){

  mSetObj <- .get.mSet(mSetObj);
  if(!exists('cov.vec')){
    adj.bool = F;
    cov.vec = "NA";
  }else{
    if(length(cov.vec) > 0){
      adj.bool = T;
    }else{
      adj.bool = F;
      cov.vec = "NA";
    }
  }

   covariates <- mSetObj$dataSet$meta.info
   var.types <- mSetObj[["dataSet"]][["meta.types"]]
   library(dplyr);
   for(i in c(1:length(var.types))){ # ensure all columns are numeric as required by correlation analysis
        if(var.types[i] == "disc"){
            covariates[,i] <- covariates[,i] %>% as.numeric()-1;
        }
   }

  input.data <- mSetObj$dataSet$norm;

  if(tgtType == "featNm"){
    # test if varName is valid
    if(!varName %in% colnames(mSetObj$dataSet$norm)){
        AddErrMsg("Invalid feature name - not found! Feature might have been filtered out!");
        return(0);
    }
    # need to exclude the target itself
    inx <- which(colnames(input.data) == varName);
    tgt.var <- input.data[,inx];
    input.data <- input.data[,-inx];
  }else{
    tgt.var <- covariates[,varName];
  }

  if(adj.bool){
    adj.vars <- covariates[, cov.vec];
    require("ppcor");
    if(max(input.data) >= 1e8){
     # values bigger than 1x10^8 will beyond the computational ability. Enforce an log transform here
      input.data[input.data <= 0] <- 1
      input.data <- log10(input.data)
    }
    cbtempl.results <- apply(input.data, 2, template.pmatch, tgt.var, dist.name, adj.vars);
  }else{
    cbtempl.results <- apply(input.data, 2, template.match, tgt.var, dist.name);
  }
  cor.res<-t(cbtempl.results);

  fdr.col <- p.adjust(cor.res[,3], "fdr");
  cor.res <- cbind(cor.res, fdr.col);
  colnames(cor.res)<-c("correlation", "t-stat", "p-value", "FDR");
  ord.inx<-order(cor.res[,3])
  sig.mat <-signif(cor.res[ord.inx,],5);
  
  fileName <- "correlation_feature.csv";
  fast.write.csv(sig.mat,file=fileName);
  
  mSetObj$analSet$corr$cov.vec <- cov.vec;  
  mSetObj$analSet$corr$dist.name <- dist.name;  
  mSetObj$analSet$corr$sig.nm <- fileName;
  mSetObj$analSet$corr$cor.mat <- sig.mat;
  mSetObj$analSet$corr$pattern <- varName;
  
  return(.set.mSet(mSetObj));
}

#'Random Forest Significance matrix
#'@description Significance measure, double brackets
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetMultiRFSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$rf.sigmat.mf))
}

GetMultiRFSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$rf.sigmat.mf);
}

GetMultiRFSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$rf.sigmat.mf);
}

PlotRF.OutlierMeta <- function(mSetObj = NA,
                               imgName,
                               format = "png",
                               dpi    = default.dpi,
                               width  = NA) {
  mSetObj <- .get.mSet(mSetObj)
  rf_mod  <- mSetObj$analSet$rf
  isClass <- !is.null(rf_mod$type) && rf_mod$type == "classification"
if(isClass){
PlotRF.Outlier(mSetObj,
                               imgName,
                               format ,
                               dpi   ,
                               width)
}else{
PlotRF.OutlierCont(mSetObj,
                               imgName,
                               format ,
                               dpi   ,
                               width)
}
return(1)
}

PlotRF.OutlierCont <- function(mSetObj = NA,
                               imgName,
                               format = "png",
                               dpi    = default.dpi,
                               width  = NA) {

  ## ── fetch mSetObj & model -------------------------------
  mSetObj <- .get.mSet(mSetObj)
  rf_mod  <- mSetObj$analSet$rf

  ## ── compute |standardized OOB residual| -----------------
  zres     <- (rf_mod$y - rf_mod$predicted) / sd(rf_mod$y - rf_mod$predicted)
  dist.res <- abs(zres)
  names(dist.res) <- rownames(mSetObj$dataSet$norm)
  ylab.txt <- "|Standardized OOB residual|"

  ## ── color bars by continuous metadata -------------------
  metaVals <- mSetObj$dataSet$cls.rf
  rng      <- range(metaVals, na.rm = TRUE)
  rampCols <- colorRampPalette(c("lightblue","blue"))(100)
  brks     <- seq(rng[1], rng[2], length.out = 101)
  idx      <- findInterval(metaVals, brks, rightmost.closed = TRUE)
  cols     <- rampCols[idx]

  ## ── open device & draw single panel ---------------------
  imgName <- paste0(imgName, "dpi", dpi, ".", format)
  w <- ifelse(is.na(width), 9, ifelse(width == 0, 8, width))
  h <- w * 7/9
  mSetObj$imgSet$rf.outlier <- imgName

  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")

  # leave a bit of right margin for padding (no legend)
  par(mar = c(8, 5, 4, 2))

  bp <- barplot(dist.res,
                border    = "black",
                col       = cols,
                las       = 2,       # rotate sample names 90°
                cex.names = 0.6,
                names.arg = NA,          # hides sample names
                ylab      = ylab.txt,
                main      = "Outlier scores")

  # label the top 5 most extreme
  topN <- head(order(dist.res, decreasing = TRUE), 5)
  text(x    = bp[topN],
       y    = dist.res[topN],
       labels = names(dist.res)[topN],
       pos   = 3,
       cex   = 0.7,
       xpd   = TRUE)

  dev.off()
  invisible(.set.mSet(mSetObj))
}

PlotRFImpRankStability <- function(mSetObj = NA,
                                   imp.vec.list,
                                   feat.num = 15,
                                   imgName = "rf_rank_stability",
                                   format = "png",
                                   dpi = 72,
                                   width = NA) {

  mSetObj <- .get.mSet(mSetObj)

  # ---- Prepare top features from each importance vector ----
  top.feat.df <- lapply(imp.vec.list, function(imp.vec) {
    imp.top <- sort(imp.vec, decreasing = TRUE)
    topN    <- names(imp.top)[1:min(feat.num, length(imp.top))]
    data.frame(Feature = topN, stringsAsFactors = FALSE)
  })

  # ---- Combine and compute frequency of each feature ----
  all.feats <- do.call(rbind, top.feat.df)
  all.feats$Feature <- factor(all.feats$Feature)
  freq.tbl  <- as.data.frame(table(all.feats$Feature))
  colnames(freq.tbl) <- c("Feature", "Count")
  freq.tbl <- freq.tbl[order(freq.tbl$Count, decreasing = TRUE), ]
  freq.tbl$Feature <- factor(freq.tbl$Feature, levels = rev(freq.tbl$Feature))

  # ---- Plot using ggplot2 ----
  require(ggplot2)
  require(Cairo)
  imgName <- paste0(imgName, ".", format)
  w <- ifelse(is.na(width), 8, width)
  h <- w * 0.7

  p <- ggplot(freq.tbl, aes(x = Feature, y = Count)) +
    geom_bar(stat = "identity", fill = "#2b8cbe") +
    coord_flip() +
    theme_bw() +
    ylab("Frequency in Top N") +
    xlab("Feature") +
    ggtitle("Rank Stability Across RF Runs") +
    theme(plot.title = element_text(hjust = 0.5))
  print(p)

  mSetObj$imgSet$rf.imp.rankstab <- imgName
  return(.set.mSet(mSetObj))
}

PlotRF.RegressionDetail <- function(mSetObj = NA,
                                    imgName  = "rf_regression_detail",
                                    format   = "png",
                                    dpi      = default.dpi,
                                    width    = NA) {
  mSetObj <- .get.mSet(mSetObj)
  rf_mod  <- mSetObj$analSet$rf

  if (rf_mod$type != "regression") {
    warning("This function is only for Random Forest regression models.")
    return(invisible(.set.mSet(mSetObj)))
  }

  imgName <- paste0(imgName, "dpi", dpi, ".", format)
  w <- ifelse(is.na(width), 9, ifelse(width == 0, 8, width))
  h <- w * 5 / 8
  mSetObj$imgSet$rf.reg.detail <- imgName

  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

  obs  <- rf_mod$y
  pred <- rf_mod$predicted

  # ── Plot 1: Observed vs Predicted ───────────────────────────────
  plot(obs, pred,
       xlab = "Observed", ylab = "OOB-Predicted",
       main = "Observed vs Predicted",
       pch = 16, asp = 1)
  abline(0, 1, lty = 2)
  r2 <- cor(obs, pred, use = "complete.obs")^2
  legend("topleft", legend = sprintf("R² = %.3f", r2),
         bty = "n", xpd = NA)

  # ── Plot 2: Rank vs Rank ────────────────────────────────────────
  rk_obs  <- rank(obs)
  rk_pred <- rank(pred)
  plot(rk_obs, rk_pred,
       xlab = "Observed Rank", ylab = "Predicted Rank",
       main = "Rank-Rank Plot",
       pch = 16, asp = 1)
  abline(0, 1, lty = 2)
  rho <- cor(obs, pred, method = "spearman", use = "complete.obs")
  legend("topleft", legend = sprintf("ρ = %.3f", rho),
         bty = "n", xpd = NA)

  dev.off()
  return(.set.mSet(mSetObj))
}
