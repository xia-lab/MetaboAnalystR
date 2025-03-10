##################################################
## R script for ExpressAnalyst
## Description: GSEA functions
## Author: G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#'Perform Gene Set Enrichment Analysis test on single or multiple gene expression matrices
#'@param dataSetObj file name of the data, .txt format
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
my.perform.gsea<- function(dataName, file.nm, fun.type, netNm, mType, selectedFactorInx=1, mode = "multi",rankOpt=""){
  message("Debug Info - my.perform.gsea:")
  message("dataName: ", dataName)
  message("file.nm: ", file.nm)
  message("fun.type: ", fun.type)
  message("netNm: ", netNm)
  message("mType: ", mType)
  message("selectedFactorInx: ", selectedFactorInx)
  message("mode: ", mode)
  message("rankOpt: ", rankOpt)
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  msgSet <- readSet(msgSet, "msgSet");

  anal.type <- paramSet$anal.type;
  setres <- .loadEnrichLib(fun.type, paramSet);
  current.geneset <- setres$current.geneset;
  current.universe <- unique(unlist(current.geneset));     

  gene.vec <- current.universe;
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  gene.nms <- sym.vec;

  current.geneset.symb <- lapply(current.geneset, 
                       function(x) {
                         gene.nms[gene.vec%in%unlist(x)];
                       }
  );

  require("fgsea");
  
  if(anal.type == "onedata"){
    datnorm <- dataSet$data.norm;
    inx  <- order(dataSet$fst.cls)
    sampleNms <- colnames(dataSet$data.norm);
    sampleNms <- sampleNms[inx]
    rankedVec<- ComputeRankedVec(dataSet, rankOpt, paramSet$selectedFactorInx);
    
  }else{
    if(paramSet$selDataNm == "meta_default"){
      inmex <- qs:::qread("inmex_meta.qs");
      sampleNms <- colnames(inmex$plot_data);
      colnums <- dim(inmex$plot.data)[2]
      inx  <- rep(T, colnums)
      rankedVec <- analSet$meta.mat.all[,1];
      names(rankedVec) <- rownames(analSet$meta.mat.all);
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      inx  <- rep(T, ncol(dataSet$data.norm))
      datnorm <- dataSet$data.norm
      sampleNms <- colnames(dataSet$data.norm);
      ds <- analSet$inmex.ind[paramSet$selDataNm][[1]]
      rankedVec <- ComputeRankedVec(dataSet, rankOpt, 1);
    }
  }

  if(length(rankedVec) == 1){
    if(rankedVec == 0){
        msgSet$current.msg <- "Selected ranking method is not suitable. Please select another one!";
        saveSet(msgSet, "msgSet");
        return(0);
    }
  }

  set.seed(123)
  if(fun.type %in% c("go_bp", "go_mf", "go_cc")){
      fgseaRes <- fgsea::fgsea(pathways = current.geneset, 
                          stats    = rankedVec,
                          minSize  = 5,
                          maxSize = 500,
                          scoreType = "std",
                          nperm=1000)    
    }else{
      fgseaRes <- fgsea::fgsea(pathways = current.geneset, 
                          stats    = rankedVec,
                          minSize  = 5,
                          maxSize = 500,
                          scoreType = "std")   
     
    }
  
  fgseaRes <- fgseaRes[!duplicated(fgseaRes$pathway),]
  
  rownames(fgseaRes) <- make.names(fgseaRes$pathway, unique=TRUE)
  fgseaRes <- fgseaRes[,c("size","ES", "pval", "pathway", "padj")]
  
  if(nrow(fgseaRes)<1){
    analSet <- SetListNms(dataSet);
    initsbls <- doEntrez2SymbolMapping(analSet$list.genes, paramSet$data.org, paramSet$data.idType);
    names(initsbls) <- analSet$list.genes
    netData <- list(sizes=analSet$listSizes, genelist=initsbls);
    netName <- paste0(netNm, ".json");
    sink(netName);
    cat(RJSONIO::toJSON(netData));
    sink();
    return(0);
  }
  
  fgseaRes <- fgseaRes[order(fgseaRes$pval),]
  #if(nrow(fgseaRes)>20 ){
  #  fgseaRes <- fgseaRes[c(1:20),]
  #} 
  
  current.mset <- current.geneset[fgseaRes$pathway]
  current.mset <- current.mset[!duplicated(names(current.mset))]
  
  ora.vec <- names(rankedVec)
  ora.nms <- doEntrez2SymbolMapping(ora.vec, paramSet$data.org, paramSet$data.idType)
  
  hits.query <- lapply(current.mset, 
                       function(x) {
                         unique(ora.nms[ora.vec%in%unlist(x)]);
                       });
  
  set.num <- unlist(lapply(current.mset, function(x){length(unique(x))}), use.names=TRUE);
  names(hits.query) <- names(current.mset);
  hit.num<-unlist(lapply(hits.query, function(x){length(x)}), use.names=TRUE);
  qs::qsave(hits.query, "hits_query.qs");
  fgseaRes$hits <- hit.num[which(fgseaRes$pathway  %in% names(hit.num))] 
  fgseaRes$total <- set.num[which(fgseaRes$pathway %in% names(set.num))]
  
  fgseaRes <- fgseaRes[which(fgseaRes$hits>1),];
  fgseaRes <- fgseaRes[which(fgseaRes$hits<500),];
  fgseaRes <- fgseaRes[which(fgseaRes$total<2000),];
  if(nrow(fgseaRes)<1){
    analSet <- SetListNms(dataSet);
    initsbls <- doEntrez2SymbolMapping(analSet$list.genes, paramSet$data.org, paramSet$data.idType)
    names(initsbls) <- analSet$list.genes
    netData <- list(sizes=analSet$listSizes, genelist=initsbls);
    netName <- paste0(netNm, ".json");
    sink(netName);
    cat(RJSONIO::toJSON(netData));
    sink();
    return(0);
  }
  
  fgseaRes=fgseaRes[order(fgseaRes$pval),]

  #if(nrow(fgseaRes)>20 ){
  #  fgseaRes <- fgseaRes[c(1:20),]
  #} 
  
  fgseaRes <- data.frame(fgseaRes, stringsAsFactors=FALSE)
  
  #get gene symbols
  msgSet$current.msg <- "Functional analysis was completed";
  
  # write json
  fun.anot <- hits.query; 
  fun.pval <- fgseaRes[,3]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  #fun.pval<-signif(fun.pval,5);  
  fun.padj <- fgseaRes[,5]; if(length(fun.padj) ==1) { fun.padj <- matrix(fun.padj) };
  #fun.padj<-signif(fun.padj,5);  
  es.num <- fgseaRes[,2]; if(length(es.num) ==1) { es.num <- matrix(es.num) };
  fun.ids <- as.vector(setres$current.setids[names(fun.anot)]); 
  if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  
  fgseaRes <- .signif_df(fgseaRes, 4);

  json.res <- list(
    fun.link = setres$current.setlink[1],
    fun.anot = fun.anot,
    fun.ids = fun.ids,
    fun.pval = fun.pval,
    fun.padj = fun.padj,
    pathname = fgseaRes[,"pathway"],
    es.num = es.num,
    hits = fgseaRes[,"hits"],
    total = fgseaRes[,"total"],
    cls = dataSet$meta.info[inx,],
    sample.nms = sampleNms
  );

    res.mat<-matrix(0, nrow=length(fun.pval), ncol=4);
    colnames(res.mat)<-c("Total","Hits", "P.Value", "FDR");
    res.mat[,"Total"] <- fgseaRes[,"total"];
    res.mat[,"Hits"] <- fgseaRes[,"hits"];
    res.mat[,"P.Value"] <- fgseaRes[,"pval"];
    res.mat[,"FDR"] <- fgseaRes[,"padj"];
    res.mat <- data.matrix(data.frame(res.mat, stringsAsFactors=FALSE));
    rownames(res.mat) <- fgseaRes[,"pathway"];

  # Check for and handle duplicate row names in enr.mat
  if(any(duplicated(rownames(res.mat)))) {
    res.mat <- res.mat[!duplicated(rownames(res.mat)), ]
    hits.query <- hits.query[match(rownames(res.mat), names(hits.query))]

    print("Duplicates in enr.mat were removed.")
  } else {
    res.mat <- res.mat
  }

    qs:::qsave(res.mat, "enr.mat.qs");

  imgSet <- readSet(imgSet, "imgSet");
  if(mType == "network"){
    res.mat <- cbind(rownames(res.mat), res.mat)
    colnames(res.mat)[1] <- "Name";
    rownames(res.mat) <- NULL;

    analSet$list.genes <- doEntrez2SymbolMapping(rownames(dataSet$sig.mat), paramSet$data.org, paramSet$data.idType);
    analSet <- SetListNms(dataSet);
    analSet <- .prepareEnrichNet(dataSet, netNm, "meta", "mixed", analSet);
    file.nm <- gsub("gsea", "enrichment", file.nm);
    json.res$naviString <- "Enrichment Network";
    imgSet$enrTables[["network"]]$table <- res.mat;
    imgSet$enrTables[["network"]]$library <- fun.type;
    imgSet$enrTables[["network"]]$algo <- "GSEA"

    saveSet(imgSet, "imgSet");
  }else{
    json.res$org <- paramSet$data.org
    json.res$analType <- anal.type
    json.res$naviString <- "GSEA";
    res.mat <- cbind(rownames(res.mat), res.mat)
    colnames(res.mat)[1] <- "Name";
    rownames(res.mat) <- NULL;

    imgSet$enrTables[["gsea"]]$table <- res.mat;
    imgSet$enrTables[["gsea"]]$library <- fun.type;
    imgSet$enrTables[["gsea"]]$algo <- "GSEA";
 
    saveSet(imgSet, "imgSet");

  }
  
  json.mat <- RJSONIO::toJSON(json.res);
  json.nm <- paste(file.nm, ".json", sep="");
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(json.nm, "current_geneset.qs"))
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  
  fgseaRes <<- fgseaRes 
  ftype <- fun.type
  if(fun.type %in% c("bp", "mf", "cc")){
    ftype <- paste0("go_", fun.type);
  }


  csvDf <- data.frame(Name=fgseaRes$pathway, Total=fgseaRes$total, Hits=fgseaRes$hits, EnrichmentScore=fgseaRes$ES, Pval=fgseaRes$pval, Padj=fgseaRes$padj);
  csvDf$IDs <- fun.ids;

  fast.write(csvDf, file=paste0(file.nm, ".csv"));

    imgSet <- readSet(imgSet, "imgSet");
    rownames(csvDf) <- NULL;
    imgSet$enrTables[["gsea"]]$table <- csvDf;
    imgSet$enrTables[["gsea"]]$current.geneset <- current.geneset;
    imgSet$enrTables[["gsea"]]$hits.query <- hits.query;
    imgSet$enrTables[["gsea"]]$current.setids <- setres$current.setids;
    imgSet$enrTables[["gsea"]]$res.mat<- csvDf[,-c(1,7)];
    imgSet$enrTables[["gsea"]]$current.geneset.symb <- current.geneset.symb;

  saveSet(imgSet);
      
  analSet$rankedVec <- rankedVec;
  saveSet(analSet, "analSet");
  saveSet(msgSet, "msgSet");

  return(1);
}

#For GSEA of AnalOverview page
my.compute.ranked.vec <- function(data, opt, inx = 1){
  cls <- data$cls
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  anal.type <- paramSet$anal.type;
  paramSet$gseaRankOpt <- opt
  if(anal.type == "metadata"){
    if(paramSet$selDataNm == "meta_default"){
      matr <- as.matrix(qs::qread("meta.resTable.qs"));
    }else{
      matr <- as.matrix(data$data)
    }
  }else{
    if(opt %in% c("mwt", "s2n", "wcx", "stu")){
      matr <- as.matrix(data$data.norm)
    }else{
      matr <- as.matrix(data$comp.res);
    }
  }
  if(opt == "mwt"){
    res <- CalculateMWT(matr, cls)
    rankedVec <- res$MWT
    names(rankedVec) <- rownames(matr)
  }else if(opt == "stu"){
    inx1 <- which(data$cls==levels(data$cls)[1]);
    inx2 <- which(data$cls==levels(data$cls)[2]);
    res <- apply(matr, 1, function(x) {
      tmp <- try(t.test(x[inx1], x[inx2], paired = FALSE, var.equal = TRUE));
      if(class(tmp) == "try-error") {
        return(c(NA, NA));
      }else{
        return(c(tmp$statistic, tmp$p.value));
      }
    })
    res <- t(res)
    rankedVec <- res[,1]
    posInx <- sign(rankedVec) == 1
    rankedVec[posInx] <- 1000-rankedVec[posInx]
    names(rankedVec) <- rownames(matr)
    
  }else if(opt == "wcx"){
    inx1 <- which(data$cls==levels(data$cls)[1]);
    inx2 <- which(data$cls==levels(data$cls)[2]);
    res <- apply(matr, 1, function(x) {
      tmp <- try(wilcox.test(x[inx1], x[inx2], paired = FALSE));
      if(class(tmp) == "try-error") {
        return(c(NA, NA));
      }else{
        return(c(tmp$statistic, tmp$p.value));
      }
    })
    res <- t(res)
    rankedVec <- res[,2]
    names(rankedVec) <- rownames(matr)
    
  }else if (opt == "s2n"){
    res <- CalculateS2N(matr, as.numeric(cls)-1)
    rankedVec <- res;
  }else if(opt == "tstat"){
    m <- length(colnames(matr))
    if (data$de.method=="limma"){
      if("t" %in% colnames(matr)){
        rankedVec <- as.vector(matr[,"t"]);
      }else{
        rankedVec <- as.vector(matr[,"F"]);
        return(0);
      }
    } else if (data$de.method=="deseq2"){
      rankedVec <- as.vector(matr[,"stat"]);
    } else {
      rankedVec <- as.vector(matr[,"LR"]);
    }
    names(rankedVec) <- rownames(matr);
  }else{
    rankedVec <- as.vector(matr[,inx]);
    names(rankedVec) <- rownames(matr);
  }
  rankedVec <- sort(rankedVec);
  rankedVec <- rankedVec[unique(names(rankedVec))];
  analSet$rankedVec <- rankedVec;
  saveSet(analSet, "analSet");
  return(rankedVec)
}


CalculateS2N <- function(data, vec = y.vec, ...) {
  
  A <- data + 0.00000001
  
  ind1 <- which(vec==1) # cases
  n1 <- length(ind1)    
  M1 <- rowMeans(A[,ind1])
  A2 <- A*A    
  S1 <- rowMeans(A2[,ind1])   
  S1 <- S1 - M1*M1    
  S1 <- sqrt(abs((n1/(n1-1)) * S1))   
  
  ind2 <- which(vec==0) # controls
  n2 <- length(ind2)
  M2 <- rowMeans(A[,ind2])
  S2 <- rowMeans(A2[,ind2])   
  S2 <- S2 - M2*M2    
  S2 <- sqrt(abs((n2/(n2-1)) * S2))   
  
  # small sigma "fix" as used in GeneCluster
  S2 <- ifelse(0.2*abs(M2) < S2, S2, 0.2*abs(M2))
  S2 <- ifelse(S2 == 0, 0.2, S2) 
  S1 <- ifelse(0.2*abs(M1) < S1, S1, 0.2*abs(M1))
  S1 <- ifelse(S1 == 0, 0.2, S1) 
  M1 <- M1 - M2
  S1 <- S1 + S2
  s2n <- M1/S1
  
  return(s2n)
}

CalculateMWT <- function(xdat,grp,na.rm=TRUE){
  ## basic statistics
  glab = unique(grp)
  n1 = sum(grp==glab[1])
  n2 = sum(grp==glab[2])
  d1 = n1-1
  d2 = n2-1
  m1 = rowMeans(xdat[,grp==glab[1]], na.rm=na.rm)
  m2 = rowMeans(xdat[,grp==glab[2]], na.rm=na.rm)
  
  s2.g1 = rowSums((xdat[,grp==glab[1]]-m1)^2, na.rm=na.rm)/d1
  
  ## We might either have all NA in one group or variance = 0
  ## (e.g. might happen with RMA with small samples)
  ## In this situation we want to remove the gene
  s2.g1[s2.g1 == 0] <- NA
  
  s2.g2 = rowSums((xdat[,grp==glab[2]]-m2)^2, na.rm=na.rm)/d2
  s2.g2[s2.g2 == 0] <- NA
  
  ## If either s2.g1 or s2.g2 are NA this will be NA
  sig2 = (d1*s2.g1 + d2*s2.g2)/(d1+d2)
  fac = 1/n1 + 1/n2
  se2 = (sig2 * fac)
  
  ## F test
  
  lev.test = levene(xdat,grp)
  fFDR = lev.test$FDR
  fStat = lev.test$statistic
  
  
  ## ordinary Welch statistics
  se2.sep = s2.g1/n1 + s2.g2/n2
  df = se2.sep^2/((s2.g1/n1)^2/d1 + (s2.g2/n2)^2/d2)
  
  ## weighted formulas
  df.w  = fFDR*(d1+d2) + (1-fFDR)*df
  se2.w = fFDR*se2 + (1-fFDR)*se2.sep
  ds = est.hyper(z=log(se2.w),D=mean(df.w,na.rm=na.rm),d12=d1+d2)   
  
  ## ....................................... moderated Welch
  se2.com = (ds$d0*ds$s2 + df.w*se2.w)/(ds$d0 + df.w)
  Wm.t = (m1-m2)/sqrt(se2.com) ## Welch t
  df.com = ds$d0 + df.w      ## df
  
  
  Wm.pval = pt(-abs(Wm.t), df= df.com) * 2
  
  ## ................. Compute Global FDR
  
  fdr <- NULL
  
  return(list(MWT= Wm.t, coefficients=cbind((m1-m2)),pvalue = Wm.pval))
  
}

levene <- function (xdat, grp, na.rm = TRUE) {
  glab = unique(grp)
  ngr = length(glab)
  n = mn = s2 = NULL
  X0 = NULL
  for (i in 1:ngr) {
    ndx = grp == glab[i]
    mni = rowMeans(xdat[, ndx], na.rm = na.rm)
    x0 = xdat[, ndx] - mni
    X0 = cbind(X0, x0)
  }
  xdat = abs(X0)
  for (i in 1:ngr) {
    ndx = grp == glab[i]
    ni = sum(ndx)
    mni = rowMeans(xdat[, ndx], na.rm = na.rm)
    x0 = xdat[, ndx] - mni
    s2i = rowSums(x0 * x0, na.rm = na.rm)/(ni - 1)
    n = c(n, ni)
    mn = cbind(mn, mni)
    s2 = cbind(s2, s2i)
  }
  N = sum(n)
  mmn = rowSums(xdat, na.rm = na.rm)/N
  mn0 = mn - mmn
  num = rowSums(t(t(mn0 * mn0) * n))/(ngr - 1)
  den = rowSums(t(t(s2 * (n - 1))))/(N - ngr)
  F3 = num/den
  pval = pf(F3, df1 = ngr - 1, df2 = N - ngr)
  pval = ifelse(pval < 0.5, 2 * pval, 2 * (1 - pval))
  lvn.FDR = pval2FDR(pval)
  return(list(statistic = F3, pvalue = pval, FDR = lvn.FDR))
}

pval2FDR <-function (pval, lim = 0.7) 
{
  n1 = length(pval)
  ok.id <- 1:n1
  if (any(is.na(pval))) {
    ok.id <- which(!is.na(pval))
    pval <- na.omit(pval)
  }
  n = length(pval)
  Fp = rank(pval)/length(pval)
  p0 = sum(pval > lim)/((1 - lim) * n)
  p0 = min(p0, 1)
  FDRp = p0 * pmin(pval/Fp, 1)
  ord = order(pval)
  FDR.o = FDRp[ord]
  b = rev(cummin(rev(FDR.o)))
  FDR = rep(0, n)
  FDR[ord] = b
  out.FDR <- rep(NA, n1)
  out.FDR[ok.id] <- FDR
  attr(out.FDR, "p0") <- p0
  return(out.FDR)
}

est.hyper <- function (z, D, d12) 
{
  f2 <- function(d0, D) {
    var(z, na.rm = TRUE) - trigamma(D/2) - trigamma(d0/2)
  }
  lim = f2(100, D)
  if (lim < 0) 
    d0.est <- 100
  if (lim > 0) 
    d0.est <- uniroot(f2, c(1, 100), D = D, extendInt = "yes")$root
  s2.est <- exp(mean(z, na.rm = TRUE) - digamma(D/2) + digamma(d0.est/2) - 
                  log(d0.est/D))
  return(list(d0 = d0.est, s2 = s2.est))
}

PlotGShm <-function(dataName="", cmpdNm="", IDs){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  
  anal.type <- paramSet$anal.type;
  data.org <- paramSet$data.org;
  
  ids <- unlist(strsplit(IDs, "; "));
  cmpdNm <- gsub(" ", "_",  cmpdNm);
  cmpdNm <- gsub("/", "_",  cmpdNm);
  
  if(anal.type == "onedata"){
    dataSet <- readDataset(dataName);
    gene.map <- readDataQs("symbol.map.qs", paramSet$anal.type, dataName);
    subset <- dataSet$data.norm[which(doIdMappingGeneric(rownames(dataSet$data.norm), gene.map, "gene_id", "symbol") %in% ids),]
    if(length(subset)<1){
      subset <- dataSet$data.norm[which(rownames(dataSet$data.norm) %in% ids),]
    }
    
    inx <- order(dataSet$meta.info[,1]);
    subset <- subset[,inx];
    
    
  }else{
    if(paramSet$selDataNm == "meta_default"){
      inmex <- qs:::qread("inmex_meta.qs");
      dat <- inmex$plot.data
      gene.map <- data.frame(gene_id=names(inmex$gene.symbls), symbol=unname(inmex$gene.symbls));
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      gene.map <- readDataQs("symbol.map.qs", paramSet$anal.type, paramSet$selDataNm);
      dat <- dataSet$data.norm;
    }
    subset <- dat[which(doIdMappingGeneric(rownames(dat), gene.map, "gene_id", "symbol") %in% ids),]
    if(length(subset)<1){
      subset <- dat[which(rownames(dat) %in% ids),]
    }
    inx <- order(inmex$cls.lbl);
    subset <- subset[,inx];
  }
  dat <- t(scale(t(subset)));
  
  # now pearson and euclidean will be the same after scaling
  dat.dist <- dist(dat); 
  orig.gene.nms <- doIdMappingGeneric(rownames(subset), gene.map, "gene_id", "symbol");
  gene.ward.ord <- hclust(dat.dist, "ward.D")$order;
  gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.ord]);
  gene.ave.ord <- hclust(dat.dist, "ave")$order;
  gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.ord]);
  gene.single.ord <- hclust(dat.dist, "single")$order;
  gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.ord]);
  gene.complete.ord <- hclust(dat.dist, "complete")$order;
  gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.ord]);
  
  gene.cluster <- list(
    ward = orig.gene.nms[gene.ward.ord],
    average = orig.gene.nms[gene.ave.ord],
    single = orig.gene.nms[gene.single.ord],
    complete = orig.gene.nms[gene.complete.ord]
  );
  
  if(anal.type == "onedata"){
    res.tbl <- dataSet$comp.res ;
    res.tbl <- res.tbl[which(rownames(res.tbl) %in% rownames(subset)),];
    res.tbl$id <- doIdMappingGeneric(rownames(res.tbl), gene.map, "gene_id", "symbol");
    if("P.Value" %in% colnames(res.tbl)){
      res.tbl <- res.tbl[order(res.tbl$P.Value),];
    }else{
      res.tbl <- res.tbl[order(res.tbl$PValue),];
    }
    stat.pvals <- res.tbl$id;
    
    if("logFC" %in% colnames(res.tbl)){
      stat.fc <- res.tbl[order(-abs(res.tbl$logFC)),]; 
    }else{
      stat.fc <- res.tbl[order(-abs(res.tbl[,paramSet$selectedFactorInx])),]; 
    }
    stat.fc <- res.tbl$id;
    
    gene.cluster[["pval"]] <- stat.pvals;
    gene.cluster[["fc"]] <- stat.fc;
  }else{
    res.tbl <- analSet$meta.mat.all;
    res.tbl <- res.tbl[which(rownames(res.tbl) %in% rownames(subset)),];
    rownames(res.tbl) <- doIdMappingGeneric(rownames(res.tbl), gene.map, "gene_id", "symbol");  
    res.tbl <- res.tbl[order(res.tbl[,2]),];
    stat.pvals <- rownames(res.tbl);
    
    gene.cluster[["pval"]] <- stat.pvals;
  }
  
  json.res <- list(
    data=subset,
    ids=doIdMappingGeneric(rownames(subset), gene.map, "gene_id", "symbol"),
    entrez = rownames(subset),
    gene.cluster = gene.cluster
  )
  
  json.mat <- RJSONIO::toJSON(json.res);
  json.nm <- paste(cmpdNm,"_hm", ".json", sep="");
  
  sink(json.nm);
  cat(json.mat);
  sink();

  paramSet$GSEAPathway <- cmpdNm;
  paramSet$jsonNms$heatmapGSEA <- json.nm
  saveSet(paramSet, "paramSet");

  return(json.nm)
}

plot.gs.view <-function(fileName, format="png", dpi=72, width=NA, imgName=NA){
  require("ggplot2");
  require("fgsea");
  current.geneset <- qs::qread("current_geneset.qs");
  analSet <- readSet(analSet, "analSet");
  if(is.na(imgName)){
   imgName <- gsub("\\/", "_",  fileName);
  }else{
   imgName <- gsub("\\/", "_",  imgName);
  }
  imgName <- gsub(" ", "_",  imgName);
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");

  cmpdNm <- gsub("barcode_", "",fileName);
  Cairo(file = imgName, dpi=dpi, width=340, height=300, type="png", bg="transparent");
  g <- plotEnrichment(current.geneset[[cmpdNm]], analSet$rankedVec)
  print(g)
  dev.off();
  imgSet <- readSet(imgSet, "imgSet");
  imgSet$GSEAbarcode <- imgName;
  saveSet(imgSet);
  return(imgName);
}
