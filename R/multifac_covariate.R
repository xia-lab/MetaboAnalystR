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
RF.AnalMeta <- function(mSetObj=NA, treeNum=500, tryNum=7, randomOn=1, selectedMeta){

  mSetObj <- .get.mSet(mSetObj);

  if(mSetObj$dataSet$meta.types[selectedMeta] == "cont"){
    return(2)
  }

  meta.info <- mSetObj$dataSet$meta.info
  if(length(meta.vec.rf) == 0){
    sel.meta.vecs <- "NA"
  }else{
    sel.meta.vecs <- meta.info[, meta.vec.rf]
  }

  norm <- mSetObj$dataSet$norm;
  norm <- norm[match(rownames(meta.info), rownames(norm)), ] # make sure the meta data and metabolites have the same row order
  if(length(meta.vec.rf) >0) {
    norm <- cbind(sel.meta.vecs, norm);
    colnames(norm)[1:length(meta.vec.rf)] <- meta.vec.rf;
  }else{
    meta.vec.rf <- "NA"
  }

  # set up random numbers
  if(is.null(mSetObj$dataSet$random.seeds)){
    mSetObj$dataSet$random.seeds <- GetRandomNumbers();
    mSetObj$dataSet$cur.inx <- 0;
    mSetObj$dataSet$rn.seed <- mSetObj$dataSet$random.seeds[1];
  }
  randomness = "";
  if(randomOn == -1){
    rn.sd <- 123456;
    randomness = "Using a constant (123456)";
  }else if(randomOn == 0){ # keep current
    rn.sd <- mSetObj$dataSet$rn.seed;
    randomness = "Fix current random seed";

  }else{ # random on
    cur.inx <- mSetObj$dataSet$cur.inx + 1;
    rn.sd <- mSetObj$dataSet$random.seeds[cur.inx];        
    mSetObj$dataSet$cur.inx <- cur.inx;
    randomness = "On";
  }
  set.seed(rn.sd);
  # save the 
  mSetObj$dataSet$rn.seed <- rn.sd;
  mSetObj$dataSet$cls.rf <- meta.info[,selectedMeta];
  mSetObj$dataSet$cls.rf.nm <- selectedMeta;
  
  rf_out <- randomForest::randomForest(norm, mSetObj$dataSet$cls.rf, importance = TRUE, proximity = TRUE);
  
  # set up named sig table for display
  impmat <- rf_out$importance;
  impmat <- impmat[rev(order(impmat[,"MeanDecreaseAccuracy"])),]
  sigmat <- impmat[,"MeanDecreaseAccuracy", drop=F];
  sigmat <- signif(sigmat, 5);
  
  fast.write.csv(sigmat, file="randomforests_sigfeatures.csv");
  mSetObj$analSet$rf <- rf_out;
  mSetObj$analSet$rf.random <- randomness;
  mSetObj$analSet$rf.sigmat <- sigmat;
  mSetObj$analSet$meta.vec.rf <- meta.vec.rf;
  mSetObj$dataSet$norm.meta <- norm;
  return(.set.mSet(mSetObj));
}


#'Plot Random Forest 
#'@description Random Forest plot 
#'@usage PlotRF.ClassifyMeta(mSetObj, imgName, format, dpi, width, type)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param type plotting type, default is "meta".
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotRF.ClassifyMeta <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, type="meta"){

  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*5/8;
  
  if(type == "meta"){
    cls.init <- mSetObj$dataSet$cls.rf

    if(mSetObj$dataSet$types.cls.lbl[mSetObj$dataSet$cls.rf.nm] == "integer"){
      cls <- as.factor(as.numeric(levels(cls.init))[cls.init]);
    }else{
      cls <- cls.init;
    }
  }else{
    cls.init <- mSetObj$dataSet$cls
    if(mSetObj$dataSet$types.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(cls.init))[cls.init]);
    }else{
      cls <- cls.init;
    }
  }

  mSetObj$imgSet$rf.cls <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  #par(mfrow=c(2,1));
  par(mar=c(4,4,3,2));
  cols <- rainbow(length(levels(cls.init))+1);
  plot(mSetObj$analSet$rf, main="Random Forest classification", col=cols);
  
  legend("topright", legend = c("Overall", levels(cls)), lty=2, lwd=1, col=cols);
  
  #PlotConfusion(analSet$rf$confusion);
  
  dev.off();
  return(.set.mSet(mSetObj));
  
}


#'Plot Random Forest variable importance
#'@description Random Forest plot of variable importance ranked by MeanDecreaseAccuracy 
#'@usage PlotRF.VIPMeta(mSetObj=NA, imgName, format, dpi, width, type)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param type type of image
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotRF.VIPMeta <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, type="meta"){
  
  mSetObj <- .get.mSet(mSetObj);
  vip.score <- rev(sort(mSetObj$analSet$rf$importance[,"MeanDecreaseAccuracy"]));
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
    
  }else{
    w <- width;    
  }
  h <- w*7/8;
  mSetObj$imgSet$rf.imp <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotImpVarMeta(mSetObj, vip.score, "MeanDecreaseAccuracy", type="meta");
  dev.off();
  
  return(.set.mSet(mSetObj));
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
PlotImpVarMeta <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE, type="meta"){
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
    rt.mrg <- 5;
  }else if(cls.len == 3){
    rt.mrg <- 6;
  }else if(cls.len == 4){
    rt.mrg <- 7;
  }else if(cls.len == 5){
    rt.mrg <- 8;
  }else if(cls.len == 6){
    rt.mrg <- 9;
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

GetPrimaryType <- function(analysis.var){
    mSetObj <- .get.mSet(mSetObj);
    primary.type <- unname(mSetObj$dataSet$meta.types[analysis.var]);
    return(primary.type);
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
  
  cov.vec <- "NA";
  # get inputs
  if(!exists('adj.vec')){
    adj.bool = F;
    adj.vec= "NA"
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
    
    # perform specified contrast
    if(contrast.cls == "anova"){
      cntr.cls <- grp.nms[grp.nms != ref];
      myargs <- as.list(paste(cntr.cls, "-", ref, sep = ""));
    } else {
      myargs <- as.list(paste(contrast.cls, "-", ref, sep = ""));
    }
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

 mSetObj <- prepareContrast(meta0,meta1,anal.type ,par1,par2,nested.opt);
 
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




prepareContrast <-function(meta0="NA",meta1="NA",anal.type = "ref", par1 = NULL, par2 = NULL, nested.opt = "intonly"){
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
  analysisVar <-  meta0
 


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
    colnames(design) <- make.names(colnames(design)); 
    colnames(design.noadj) <- make.names(colnames(design.noadj)); 
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
  myargs[["levels"]] <- design;
  contrast.matrix <- do.call(makeContrasts, myargs);
  mSetObj$analSet$contrast.matrix <- contrast.matrix;
  myargs[["levels"]] <- design.noadj;
  contrast.matrix.noadj <- do.call(makeContrasts, myargs);
  mSetObj$analSet$contrast.matrix.noadj <- contrast.matrix.noadj;
  
  return(mSetObj);
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

PlotCovariateMap <- function(mSetObj, theme="default", imgName="NA", format="png", dpi=72, interactive=F){
  mSetObj <- .get.mSet(mSetObj); 
  both.mat <- mSetObj$analSet$cov.mat
  both.mat <- both.mat[order(-both.mat[,"pval.adj"]),]
  logp_val <- mSetObj$analSet$cov$thresh
  topFeature <- 5;
  if(nrow(both.mat) < topFeature){
    topFeature <- nrow(both.mat);
  }
  
  mSetObj$imgSet$covAdj <- imgName;
  
  width <- 8;
  height <- 8.18;
  
 library(dplyr)
  library(ggplot2)
  threshold <- logp_val                
both.mat$category <- with(both.mat, dplyr::case_when(
  pval.no > threshold & pval.adj > threshold ~ "Significant",
  pval.no > threshold & pval.adj <= threshold ~ "Non-sig. when adjusted",
  pval.adj > threshold & pval.no <= threshold ~ "Sig. when adjusted",
  TRUE ~ "Non-sig."
))
  
 category_counts <- both.mat %>%
  group_by(category) %>%
  summarise(count = n(), .groups = 'drop')
 
both.mat <- both.mat %>%
  left_join(category_counts, by = "category") %>%
  mutate(name = paste0(category, " (", count, ")"))

both.mat$count = NULL


category_properties <- data.frame(
  category = c("Significant", "Non-sig. when adjusted", 
               "Sig. when adjusted", "Non-sig."),
  color = c('#6699CC', '#E2808A',  '#94C973','grey')
)

category_properties <- category_properties %>%
  left_join(category_counts, by = "category") %>%
  mutate(name = paste0(category, " (", count, ")"))
category_properties = category_properties[!is.na(category_properties$count),]
category_properties$count = category_properties$category =NULL

both.mat$name = factor(both.mat$name,levels = category_properties$name)
  p <- ggplot(both.mat, aes(x = pval.no, y = pval.adj, color = name, text = paste("Feature:", Row.names, 
                                                                                               "<br>Adjusted Pval:", signif(10^(-pval.adj), 4), 
                                                                                               "<br>Non-adjusted Pval:", signif(10^(-pval.no), 4)))) +
    geom_point(alpha = 0.8) +
  scale_color_manual(values = setNames(category_properties$color, category_properties$name)) +
  labs(x = "-log10(P-value): no covariate adjustment", y = "-log10(P-value): covariate adjusted") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        panel.grid = element_line(color = "lightgrey", size = 0.15),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"))

  
  
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
