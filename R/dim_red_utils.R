##################################################
## R script for ExpressAnalyst
## Description: Computing PCA coordinates
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

SaveClusterJSONLoading <- function(dataName="", fileNm, clustOpt, nb){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  if(anal.type == "onedata"){
    .saveExpressClusterLoadingJSON(dataSet, fileNm, clustOpt,paramSet, nb);
  }else{
    .saveMetaClusterLoadingJSON(dataSet, fileNm, clustOpt,paramSet, nb);
  }
}

SaveClusterJSON <- function(dataName="", fileNm, clustOpt, opt){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  if(anal.type == "onedata"){
    .saveExpressClusterJSON(dataSet, fileNm, clustOpt, paramSet ,opt);
  }else{
    .saveMetaClusterJSON(dataSet, fileNm, clustOpt,paramSet , opt);
  }
}

.saveMetaClusterJSON <- function(dataSet, fileName, clustOpt,paramSet, opt){
    
    msgSet <- readSet(msgSet, "msgSet");
    paramSet <- readSet(paramSet, "paramSet");
    analSet <- readSet(analSet, "analSet");

    mdata.all <- paramSet$mdata.all;

    inmex.meta <- qs::qread("inmex_meta.qs");
    datanm.vec <- names(mdata.all)[mdata.all==1];

    dat.inx <- inmex.meta$data.lbl %in% datanm.vec;
    dat <- inmex.meta$data[, dat.inx, drop=F]; 

    # need to deal with missing values 
    dat <- na.omit(dat);

    pca3d <- list();
    if(clustOpt == "pca"){
        if(opt == "all"){
            pca <- prcomp(t(dat), center=T, scale=T);
            }else{
            dat <- dat[which(rownames(dat) %in% analSet$loadEntrez),]
            pca <- prcomp(t(dat), center=T, scale=T);
            }

        imp.pca<-summary(pca)$importance;
        pca3d$score$axis <- paste("PC", 1:3, " (", 100*round(imp.pca[2,][1:3], 3), "%)", sep="");
        coords <- data.frame(t(signif(pca$x[,1:3], 5)));
    }else if(clustOpt == "umap"){
        require('uwot');
        if(ncol(dat)<100){
            neighbor_num <- ncol(dat)
        }else{
            neighbor_num <- 100;
        }

        ndat <- as.matrix(t(dat));
        res <- umap(ndat, n_components=3, n_neighbors=neighbor_num);
        pca3d$score$axis <- paste("UMAP dim ", 1:3, sep="");
        coords <- data.frame(t(signif(res, 5)));
    }else{
        require('Rtsne');
        ndat <- as.matrix(t(dat));
        max.perx <- floor((nrow(ndat)-1)/3);
        if(max.perx > 30){
            max.perx <- 30;
        }
        res <- Rtsne(ndat, dims = 3, perplexity=max.perx);
        pca3d$score$axis <- paste("t-SNE dim ", 1:3, sep="");
        coords <- data.frame(t(signif(res$Y, 5)));
    }

    colnames(coords) <- NULL; 
    pca3d$score$xyz <- coords;
    pca3d$score$name <- colnames(dat);

    facA <- as.character(inmex.meta$cls.lbl[dat.inx]);
    if(all.numeric(facA)){
        facA <- paste("Group", facA);
    }
    pca3d$score$facA <- facA;

    facB <-  as.character(inmex.meta$data.lbl[dat.inx]);
    if(all.numeric(facB)){
        facB <- paste("Group", facB);
    }
    pca3d$score$facB <- facB;

    # now set color for each group
    cols <- unique(GetColorSchema(facB));
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgba(", paste(x, collapse=","), ",1)", sep="")});
    pca3d$score$colors <- cols;

    # add shape sphere, triangles, square, pentagon (first two)
    pca3d$score$shapes <- c("sphere", "triangle");

    mypos <- t(coords);
    colnames(mypos) <- paste("Dim", 1:3, sep="");
    coords <- data.frame(Class=facA, Data=facB, mypos);

    pos.xyz <- mypos;
    pos.xyz <- unitAutoScale(pos.xyz);
    rownames(pos.xyz) = pca3d$score$name;
    qs::qsave(pos.xyz, "score_pos_xyz.qs");

    fast.write(coords, file="expressanalyst_3d_pos.csv");

    pca3d$org <- paramSet$data.org
    pca3d$analType <- paramSet$anal.type
    pca3d$naviString <- "Scatter 3D"
    paramSet$jsonNms$pcascore <- fileName
    json.mat <- rjson::toJSON(pca3d);
    paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
    sink(fileName);
    cat(json.mat);
    sink();
    msgSet$current.msg <- "Annotated data is now ready for 3D visualization!";
    saveSet(msgSet, "msgSet");
    saveSet(paramSet, "paramSet");

    return(1);
}


.saveMetaClusterLoadingJSON <- function(dataSet, fileName, clustOpt, paramSet, nb){
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  mdata.all <- paramSet$mdata.all;

  inmex.meta <- qs::qread("inmex_meta.qs");
  datanm.vec <- names(mdata.all)[mdata.all==1];
  nb <- as.numeric(5000) # set to max 5000 datapoints
  dat.inx <- inmex.meta$data.lbl %in% datanm.vec;
  dat <- inmex.meta$data[, dat.inx, drop=F]; 
  
  # need to deal with missing values 
  dat <- na.omit(dat);
  variances <- apply(dat,1, function(x){var(x)})
  df <- data.frame(var = variances, inx = seq.int(1,length(variances)))
  df <- df[order(-df$var),];

  #do not take subset of loading data points now
  if(nb < length(df$inx)){
    inx <- df$inx[c(1:nb)];
  }else{
    inx <- df$inx;
  }
  dat <- dat[inx,];
  
  pca3d <- list();
  
  pca <- prcomp(t(dat), center=T, scale=T);    
  imp.pca<-summary(pca)$importance;
  pca3d$score$axis <- paste("PC", 1:3, sep="");
  coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
  
  colnames(coords) <- NULL; 
  pca3d$score$xyz <- coords;
  pca3d$score$name <- doEntrez2SymbolMapping(rownames(pca$rotation), paramSet$data.org, paramSet$data.idType);
  pca3d$score$entrez <- rownames(pca$rotation);
  
  analSet$loadEntrez <- pca3d$score$entrez
  mypos <- t(coords);
  colnames(mypos) <- paste("Dim", 1:3, sep="");
  rownames(mypos) <- analSet$loadEntrez;
  mypos <- unitAutoScale(mypos);
  qs::qsave(mypos, "loading_pos_xyz.qs");
  
  coords <- data.frame(mypos);
  fast.write(coords, file="expressanalyst_loadings_3d_pos.csv");
  
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  paramSet$jsonNms$pcaload <- fileName;
  json.mat <- rjson::toJSON(pca3d);
  sink(fileName);
  cat(json.mat);
  sink();
  msgSet$current.msg <- "Annotated data is now ready for 3D visualization!";
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  saveSet(analSet, "analSet");

  return(1);
}


.saveExpressClusterLoadingJSON <- function(dataSet, fileName, clustOpt, paramSet, nb){  
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  dat <- dataSet$data.norm;
  pca3d <- list();
  dat <- na.omit(dat);
  nb <- as.numeric(25000) # set to max 5000 datapoints
  if(clustOpt == "pca"){
    pca <- prcomp(t(dat), center=T, scale=T);
    imp.pca<-summary(pca)$importance;
    pca3d$score$axis <- paste("PC", 1:3, sep="");
    coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
    
    colnames(coords) <- NULL; 
    pca3d$score$xyz <- coords;
    pca3d$score$name <- doEntrez2SymbolMapping(rownames(pca$rotation), paramSet$data.org, paramSet$data.idType);
    pca3d$score$entrez <-rownames(pca$rotation);
    weights <- imp.pca[2,][1:3]
    mypos <- t(coords);
    meanpos <- apply(abs(mypos),1, function(x){weighted.mean(x, weights)})
    df <- data.frame(pos = meanpos, inx = seq.int(1,length(meanpos)))
    df <- df[order(-df$pos),]
    
    if(nrow(df) > nb){
      inx <- df$inx[c(1:nb)]
      mypos <- mypos[inx,];
      pca3d$score$xyz <- coords[inx]
      pca3d$score$name <- pca3d$score$name[inx]
      pca3d$score$entrez <- pca3d$score$entrez[inx]
    }
  }
  
  pca3d$cls <- dataSet$meta;
  colnames(mypos) <- paste("Dim", 1:3, sep="");
  # see if there is secondary
  analSet$loadEntrez <- pca3d$score$entrez
  rownames(mypos) <- pca3d$score$name;
  rownames(mypos) <- analSet$loadEntrez;
  mypos <- unitAutoScale(mypos);
  qs::qsave(mypos, "loading_pos_xyz.qs");
  
  fast.write(mypos, file="expressanalyst_3d_load_pos.csv");
  json.mat <- rjson::toJSON(pca3d);
  paramSet$jsonNms$pcaload <- fileName
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  sink(fileName);
  cat(json.mat);
  sink();
  msgSet$current.msg <- "Annotated data is now ready for PCA 3D visualization!";
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  saveSet(analSet, "analSet");

  return(1);
}

# single expression data
.saveExpressClusterJSON <- function(dataSet, fileName, clustOpt,paramSet, opt){
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  dat <- dataSet$data.norm;
  pca3d <- list();
  dat <- na.omit(dat);
  
  if(clustOpt == "pca"){
    if(opt == "all"){
      pca <- prcomp(t(dat), center=T, scale=T);
    }else{
      dat <- dat[which(rownames(dat) %in% analSet$loadEntrez),]
      pca <- prcomp(t(dat), center=T, scale=T);
    }
    imp.pca<-summary(pca)$importance;
    pca3d$score$axis <- paste("PC", 1:3, " (", 100*round(imp.pca[2,][1:3], 3), "%)", sep="");
    coords <- data.frame(t(signif(pca$x[,1:3], 5)));
  }else if(clustOpt == "umap"){
    require('uwot');
    if(ncol(dat)<100){
      neighbor_num <- ncol(dat)
    }else{
      neighbor_num <- 100;
    }
    dat <- as.matrix(t(dat));
    res <- umap(dat, n_components=3, n_neighbors=neighbor_num);
    pca3d$score$axis <- paste("UMAP dim ", 1:3, sep="");
    coords <- data.frame(t(signif(res, 5)));
    
  }else{ # tsne
    require('Rtsne');
    dat <- as.matrix(t(dat));
    max.perx <- floor((nrow(dat)-1)/3);
    if(max.perx > 30){
      max.perx <- 30;
    }
    res <- Rtsne(dat, dims = 3, perplexity=max.perx);
    pca3d$score$axis <- paste("t-SNE dim ", 1:3, sep="");
    coords <- data.frame(t(signif(res$Y, 5)));
  }
  
  colnames(coords) <- NULL; 
  pca3d$score$xyz <- coords;
  pca3d$score$name <- colnames(dataSet$data.norm);
  
  pos.xyz <- data.frame(x=pca$x[,1], y=pca$x[,2], z=pca$x[,3]);
  pos.xyz <- as.data.frame(pos.xyz);
  pos.xyz <- unitAutoScale(pos.xyz);
  rownames(pos.xyz) = colnames(dataSet$data.norm);
  qs::qsave(pos.xyz, "score_pos_xyz.qs");
  
  facA <- as.character(dataSet$fst.cls);
  if(all.numeric(facA)){
    facA <- paste("Group", facA);
  }
  pca3d$score$facA <- facA;
  
  mypos <- t(coords);
  colnames(mypos) <- paste("Dim", 1:3, sep="");
  # see if there is secondary
  if(length(dataSet$sec.cls) > 1){
    facB <- as.character(dataSet$sec.cls);
    if(all.numeric(facB)){
      facB <- paste("Group", facB);
    }
    pca3d$score$facB <- facB;
    
    # set shape based on the first group
    pca3d$score$shapes <- c("sphere", "triangle");
    
    # now set color based on 2nd group
    cols <- unique(GetColorSchema(facB));
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")});
    pca3d$score$colors <- cols;
    
    mypos <- data.frame(factorA=facA, factorB=facB, mypos);
  }else{
    # now set color based on first group
    cols <- unique(GetColorSchema(facA));
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgba(", paste(x, collapse=","), ",1)", sep="")});
    pca3d$score$colors <- cols;
    mypos <- data.frame(factorA=facA, mypos);
  }
  
  pca3d$cls <- dataSet$meta;
  pca3d$org <- paramSet$data.org
  pca3d$analType <- paramSet$anal.type
  pca3d$naviString <- "Scatter 3D"
  paramSet$jsonNms$pcascore <- fileName
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  rownames(mypos) <- colnames(dataSet$data.norm);
  
  fast.write(mypos, file="expressanalyst_3d_pos.csv");
  json.mat <- rjson::toJSON(pca3d);
  sink(fileName);
  cat(json.mat);
  sink();
  msgSet$current.msg <- "Annotated data is now ready for PCA 3D visualization!";
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  return(1);
}


ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){
  paramSet <- readSet(paramSet, "paramSet");;
  mdata.all <- paramSet$mdata.all;
  level <- as.numeric(level)
  names = strsplit(names.vec, "; ")[[1]]
  reductionSet <- .get.rdt.set();
  if(reductionOptGlobal %in% c("diablo", "spls") || omics != "NA"){
    if(grepl("pca_", omics, fixed=TRUE)){
        pca.scatter <- qs::qread("pca.scatter.qs");
        pos.xyz<-pca.scatter[[ omics ]]$score/1000
    }else{
        omics.inx = 1;
        sel.nms <- names(mdata.all)[mdata.all==1];
        for(i in 1:length(sel.nms)){
        dataSet <- readDataset(sel.nms[i]);
            if(omics == dataSet$type){
                omics.inx = i;
            }
        }
        if(omics.inx == 1){
            pos.xyz = reductionSet$pos.xyz
        }else{
            pos.xyz = reductionSet$pos.xyz2
        }
    }

  }else{
  pos.xyz = reductionSet$pos.xyz
  }

  inx = rownames(pos.xyz) %in% names;
  coords = as.matrix(pos.xyz[inx,c(1:3)])
  mesh = list()
  if(type == "alpha"){
    require(alphashape3d)
    require(rgl)
    sh=ashape3d(coords, 1.0, pert = FALSE, eps = 1e-09);
    mesh[[1]] = as.mesh3d(sh, triangles=T);
  }else if(type == "ellipse"){
    require(rgl);
    pos=cov(coords, y = NULL, use = "everything");
    mesh[[1]] = ellipse3d(x=as.matrix(pos), level=level);
  }else{
    require(ks);
    res=kde(coords);
    r = plot(res, cont=level*100);
    sc = scene3d();
    mesh = sc$objects;
  }
  require(RJSONIO);
  sink(filenm);
  cat(toJSON(mesh));
  sink();
  return(filenm);
}

unitAutoScale <- function(df){
    df <- as.data.frame(df)
    row.nms <- rownames(df);
    col.nms <- colnames(df);
    df<-apply(df, 2, AutoNorm);
    rownames(df) <- row.nms;
    colnames(df) <- col.nms;
    maxVal <- max(abs(df))
    df<- df/maxVal
    return(df)
}