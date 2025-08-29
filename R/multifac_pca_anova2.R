#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform within-subjects anova
#'@param x Input the data
#'@param time.fac Input the time factor
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

aov.mixed <- function(x){
  unlist(anova_test(x ~ exp.fac*time.fac + Error(aov.sbj/time.fac),
             data = data.frame(x=x,exp.fac=exp.fac,time.fac=time.fac,aov.sbj=aov.sbj))$ANOVA[,c("F","p")])
}

aov.2wayrep <- function(x){
  unlist(anova_test(x ~ exp.fac*time.fac + Error(aov.sbj/(exp.fac*time.fac)),
             data = data.frame(x=x,exp.fac=exp.fac,time.fac=time.fac,aov.sbj=aov.sbj))$ANOVA[,c("F","p")])
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform repeated measure one-way anova
#'@param x Input the data 
#'@param time.fac Input the time factor 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
aov.1wayrep <- function(x){
  unlist(anova_test(x ~ time.fac + Error(aov.sbj/time.fac),
                    data = data.frame(x=x,time.fac=time.fac,aov.sbj=aov.sbj))$ANOVA[,c("F","p")])
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform two-way anova
#'@param x Input data to perform 2-way ANOVA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
aov.2way <- function(x){
  res <- suppressMessages(anova_test(x ~ aov.facA * aov.facB, 
                                    data = data.frame(x = x, aov.facA = aov.facA, aov.facB = aov.facB)));
  res <- c(res$F, res$p);
  res <- unlist(res);
  return(res)
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'@usage ANOVA2.Anal(mSetObj=NA, thresh=0.05, p.cor="fdr", 
#'type="time0")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param thresh Input the p-value threshold 
#'@param p.cor Select method for p-value correction, bonferroni, holm or fdr
#'@param type Select b to perform between-subjects ANOVA, 
#'and w for within-subjects ANOVA 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ANOVA2.Anal <-function(mSetObj=NA, thresh=0.05, 
                       p.cor="fdr", designType="time0", phenOpt="between", topN=200){
  mSetObj <- .get.mSet(mSetObj);
  #save.image("aov2.RData");
  if(length(meta.vec.aov) == 0){
    sel.meta.df <- mSetObj$dataSet$meta.info[, c(1,2)]
    meta.vec.aov <- colnames(sel.meta.df)[c(1,2)];
  }else{
    
    if(designType %in% c("time")){
      # make sure subject is not in the metadata of interest
      if("subject" %in% tolower((meta.vec.aov))){
        AddErrMsg("Subject is already accounted for in the analysis.");
        return(0);
      }
    }
    
    sel.meta.df <- mSetObj$dataSet$meta.info[, meta.vec.aov]
    
    if(length(meta.vec.aov) == 1){
      sel.meta.df <- as.data.frame(sel.meta.df)
    }
    
    if (!identical(rownames(mSetObj$dataSet$norm), rownames(sel.meta.df))) {
      sel.meta.df <- sel.meta.df[match(rownames(mSetObj$dataSet$norm), rownames(sel.meta.df)), , drop=FALSE]
      mSetObj$dataSet$meta.info<-mSetObj$dataSet$meta.info[match(rownames(mSetObj$dataSet$norm), rownames(sel.meta.df)), , drop=FALSE]
      # Check again if row names are aligned after reordering
      if (!identical(rownames(mSetObj$dataSet$norm), rownames(sel.meta.df))) {
        AddErrMsg("Metadata and data tables not synchronized after reordering!");
        return(0);
      }
    }
    
    if(designType %in% c("time0", "time")){
      mSetObj$dataSet$exp.fac <- sel.meta.df[,-(which(tolower(colnames(sel.meta.df)) == "time"))]
      mSetObj$dataSet$time.fac <- sel.meta.df[,which(tolower(colnames(sel.meta.df)) == "time")]
      mSetObj$dataSet$facA.lbl <- colnames(sel.meta.df)[which(tolower(colnames(sel.meta.df)) != "time")]
      mSetObj$dataSet$facB.lbl <- colnames(sel.meta.df)[which(tolower(colnames(sel.meta.df)) == "time")]
    }else{
      mSetObj$dataSet$facA = sel.meta.df[,1]
      mSetObj$dataSet$facB = sel.meta.df[,2]
      mSetObj$dataSet$facA.lbl <- colnames(sel.meta.df)[1]
      mSetObj$dataSet$facB.lbl <- colnames(sel.meta.df)[2]
    }
  }

  # make sure all metadata are factor variable types
  for(i in 1:ncol(sel.meta.df)){
    meta <- colnames(sel.meta.df)[i]
    mettype <- mSetObj$dataSet$meta.types[meta]
    if(mettype == "cont"){
      AddErrMsg("Selected metadata must be categorical.");
      return(0);
    }
  }
  
  # only do for top topN (200 by default)
  if(dim(mSetObj$dataSet$norm)[2] > topN){
    data.filt <-  qs::qread("data_proc.qs");
    metab.var <- apply(as.matrix(data.filt), 2, function(x){
      var(x/mean(x))
    })
    high.var <- names(sort(metab.var, decreasing = TRUE))[1:topN]
    dat <- mSetObj$dataSet$norm[,colnames(mSetObj$dataSet$norm) %in% high.var]
  } else {
    dat <- mSetObj$dataSet$norm
  }
  
  require(rstatix);
  
  # now perform ANOVA depending on experimental design
  if(designType == "time0"){
    
    time.fac <<- mSetObj$dataSet$time.fac;
    mSetObj$dataSet$sbj <- as.factor(mSetObj$dataSet$exp.fac);
    aov.sbj <<- mSetObj$dataSet$sbj
    
    if(.on.public.web){
      .set.mSet(mSetObj);
    }
    
    # first check if balanced
    res <- table (time.fac, mSetObj$dataSet$sbj);
    res.mean <- colMeans(res);
    all.res <- res/res.mean;
    if(sum(all.res != 1) > 0){
      AddErrMsg("Experiment design is not balanced!");
      return(0);
    }
    
    aov.mat <- apply(as.matrix(dat), 2, aov.1wayrep);
    
    if(is.null(dim(aov.mat))){ #sometimes ANOVA fails for one metabolite but throws no warnings; need to reformat
      library(data.table)
      aov.mat <- suppressWarnings(t(rbindlist(list(aov.mat))))
      colnames(aov.mat) <- c("F","p")
    } else {
      aov.mat <- t(aov.mat)
    }
    
    rm(aov.sbj, time.fac, pos=".GlobalEnv")
    
    fileName <- "oneway_anova_repeated.csv";
    rownames(aov.mat)<-colnames(dat);
    
    
    aov.mat <- cbind(aov.mat, p.adjust(aov.mat[,2], p.cor));
    colnames(aov.mat) <- c("F-value", "Raw P-val", "Adjusted P-val");
    p.value <- aov.mat[,3];
    inx.imp <-aov.mat[,3] <= thresh;
    aov.mat <- aov.mat[inx.imp, ,drop=F];
    vennC <- NULL;
    # default sort first by main effect: treatment, then by ...
    ord.inx <- order(aov.mat[,2], decreasing = FALSE);
    
  } else {
    if(designType == "time"){
      # first check if balanced
      res <- table(mSetObj$dataSet$exp.fac, mSetObj$dataSet$time.fac);
      res.mean <- colMeans(res);
      all.res <- res/res.mean;
      if(sum(all.res != 1) > 0){
        AddErrMsg("Experiment design is not balanced!");
        return(0);
      }
      
      mSetObj$dataSet$sbj <- mSetObj$dataSet$meta.info[,3]
      time.fac <<- mSetObj$dataSet$time.fac;
      exp.fac <<- mSetObj$dataSet$exp.fac;
      aov.sbj <<- mSetObj$dataSet$sbj;
      
      if(.on.public.web){
        .set.mSet(mSetObj);
      }
      
      # check if correct phenOpt selected
      phens <- unique(exp.fac)
      sbj.df <- data.frame(subject = aov.sbj, phenotype = exp.fac)
      sbjs <- list()
      for(i in c(1:length(phens))){
        sbjs[[i]] <- unique(as.character(sbj.df$subject[sbj.df$phenotype == phens[i]]))
      }
      overlap.across.phens <- Reduce(intersect, sbjs)
      if(length(overlap.across.phens) == 0 & phenOpt == "within"){
        AddErrMsg("No repeated subjects across phenotypes! Choose 'Between subjects' for the phenotype factor.")
        return(0)
      }
      
      if(length(overlap.across.phens) != 0 & phenOpt == "between"){
        AddErrMsg("There are repeated subjects across phenotypes! Choose 'Within subjects' for the phenotype factor.")
        return(0)
      }
      
      if(phenOpt == "between"){
        aov.mat <- t(apply(as.matrix(dat), 2, aov.mixed));      
      } else {
        aov.mat <- t(apply(as.matrix(dat), 2, aov.2wayrep));
      }
      
      rm(time.fac, exp.fac, aov.sbj, pos=".GlobalEnv");
      fileName <- "anova_within_sbj.csv";
      
    } else { 
      
      # g2 (two-factor analysis)
      aov.facA <<- mSetObj$dataSet$facA;
      aov.facB <<- mSetObj$dataSet$facB;
      
      tryCatch(
        {
          aov.mat <- t(apply(as.matrix(dat), 2, aov.2way));
        }, warning = function(w){ print('warning in aov.2way') },
        error = function(e) {
          if(grepl("there are aliased coefficients in the model", e$message, fixed=T)){
            AddErrMsg("Make sure the selected metadata are not linearly dependent with each other!");
            return(0);
          }
          print(e$message)
        }
      )
      
      rm(aov.facA, aov.facB, pos=".GlobalEnv");
      
      fileName <- "anova_between_sbj.csv";
    }
    
    # make table for display/download  
    aov.mat2 <- cbind (aov.mat, p.adjust(aov.mat[,4], p.cor),
                       p.adjust(aov.mat[,5], p.cor),
                       p.adjust(aov.mat[,6], p.cor));
    
    sig.facA <-(aov.mat2[,7] <= thresh);
    sig.facB <-(aov.mat2[,8] <= thresh);
    sig.intr <-(aov.mat2[,9] <= thresh);
    
    all.match <- cbind(sig.facA, sig.facB, sig.intr);
    colnames(all.match) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl, "Interaction");
    colnames(aov.mat2) <- c(paste(mSetObj$dataSet$facA.lbl, "(F.val)", sep = ""), 
                            paste(mSetObj$dataSet$facB.lbl, "(F.val)", sep = ""),
                            paste("Interaction", "(F.val)", sep = ""),
                            paste(mSetObj$dataSet$facA.lbl, "(raw.p)", sep = ""), 
                            paste(mSetObj$dataSet$facB.lbl, "(raw.p)", sep = ""),
                            paste("Interaction", "(raw.p)", sep = ""), 
                            paste(mSetObj$dataSet$facA.lbl, "(adj.p)", sep = ""), 
                            paste(mSetObj$dataSet$facB.lbl, "(adj.p)", sep = ""), 
                            paste("Interaction", "(adj.p)", sep = ""))
    
    vennC <- getVennCounts(all.match);
    p.value <- aov.mat2[,7]; 
    inx.imp <- sig.facA | sig.facB | sig.intr;
    aov.mat2 <- aov.mat2[, c(1,4,7,2,5,8,3,6,9),drop=F] 
    
    # default sort first by main effect: treatment, then by ...
    aov.mat <- aov.mat2[inx.imp, ,drop=F];
    ord.inx <- order(aov.mat[,2], aov.mat[,3], decreasing = FALSE);
  }
  
  aov.mat <- signif(aov.mat[ord.inx,,drop=F], 5);
  
  if(dim(aov.mat)[1] != 0){
    if(unname(is.na(table(is.na(aov.mat))["FALSE"]))){
      AddErrMsg("Make sure the selected metadata are not linearly dependent with each other!");
      return(0)
    }
  }
  
  fast.write.csv(aov.mat, file=fileName);
  names(p.value) <- colnames(dat);
  aov2<-list (
    type = designType,
    sig.nm = fileName,
    thresh = -log10(thresh),
    raw.thresh = thresh,
    multi.c = p.cor,
    sig.mat = na.omit(aov.mat),
    p.log = -log10(p.value),
    inx.imp = inx.imp,
    vennC = vennC,
    selected.meta = meta.vec.aov,
    phenotype.factor= phenOpt
  );
  mSetObj$analSet$aov2 <- aov2;
  return(.set.mSet(mSetObj));
}

#'Plot Venn diagram of ANOVA results
#'@description Plot Venn diagram of ANOVA results
#'@usage PlotANOVA2(mSetObj, imgName, format="png", dpi=default.dpi, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
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
PlotANOVA2 <- function(mSetObj=NA, imgName, format="png", dpi=default.dpi, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  
  mSetObj$imgSet$anova2 <- imgName;
  
  if(mSetObj$dataSet$design.type == "time0"){
    w <- 9;
    h <- w*6/9;
    lod <- mSetObj$analSet$aov2$p.log;
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plot(lod, ylab="-log10(p)", xlab = "Index", main="One-way repeated measures ANOVA", type="n");
    red.inx<- which(mSetObj$analSet$aov2$inx.imp);
    blue.inx <- which(!mSetObj$analSet$aov2$inx.imp);
    points(red.inx, lod[red.inx], bg="red", cex=1.2, pch=21);
    points(blue.inx, lod[blue.inx], bg="green", pch=21);
    abline (h=mSetObj$analSet$aov2$thresh, lty=3);
    dev.off();
  }else{
    h <- w;
    title <- "Two-way ANOVA"; # note within/between subject is left to user 
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plotVennDiagram(mSetObj$analSet$aov2$vennC, circle.col=c("red", "blue", "green"), mar=c(0,0,2,0));
    mtext(title, NORTH<-3, line=0.25, cex=1.5);
    dev.off();
  }
  
  return(.set.mSet(mSetObj));
  
}

#'Perform PCA analysis, prepare file for interactive liveGraphics3D
#'@description Perform PCA analysis, prepares a JSON file for interactive liveGraphics3D, as well as interactive 3D
#'PCA score and loading plots using the plotly R package. These plots are saved in the created mSetObj; to view these, 
#'type "mSetObj$imgSet$time$score3d" to view the interactive score plot, and "mSetObj$imgSet$time$load3d" to view
#'the interactive loading plot.  
#'@usage iPCA.Anal(mSetObj, fileNm, metaCol, metaShape)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fileNm select a file name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@importFrom plotly plot_ly add_markers layout
#'
iPCA.Anal <- function(mSetObj=NA, fileNm, metaCol, metaShape, colorGradient="heat"){
  mSetObj <- .get.mSet(mSetObj);
  
  metadata <- mSetObj$dataSet$meta.info
  data.types <- mSetObj$dataSet$meta.types
  
  pca <- prcomp(mSetObj$dataSet$norm, center=TRUE, scale=FALSE);
  imp.pca <- summary(pca)$importance;
  
  pca3d <- list();
  pca3d$score$axis <- paste("PC", 1:3, " (", 100*round(imp.pca[2,][1:3], 3), "%)", sep="");
  coords <- data.frame(t(signif(pca$x[,1:3], 5)));
  colnames(coords) <- NULL; 
  pca3d$score$xyz <- coords;
  pca3d$score$name <- rownames(mSetObj$dataSet$norm);
  facA <- mSetObj$dataSet$facA;
  facA <- as.character(facA);
  
  pca3d$score$facA <- metadata[, metaCol];
  metadata.list <- list();
  
  for(i in 1:ncol(metadata)){
    if(data.types[colnames(metadata)[i]] == "disc"){
      metadata.list[[ colnames(metadata)[i] ]] <- levels(metadata[, i])
    } else {
      metadata.list[[ colnames(metadata)[i] ]] <- unique(metadata[, i])
    }
  }
  
  if (!is.null(metaShape) && metaShape != "NA" && metaShape %in% colnames(metadata)) {
    facB <- as.character(metadata[, metaShape]);
    pca3d$score$facB <- facB;
  }

  pca3d$score$metadata_list <- metadata.list
  pca3d$score$metadata <- metadata
  pca3d$score$metadata_type <- mSetObj$dataSet$meta.types

  pca3d$loadings$axis <- paste("Loadings", 1:3);
  coords0 <- data.frame(t(signif(pca$rotation[,1:3], 5)));
  colnames(coords0) <- NULL; 
  pca3d$loadings$xyz <- coords0;
  pca3d$loadings$name <- colnames(mSetObj$dataSet$norm);
  
  dists <- GetDist3D(coords0);
  names(dists) <- rownames(pca$rotation)
  colset <- GetRGBColorGradient(dists);
  pca3d$loadings$cols <- colset;

if (pca3d$score$metadata_type[metaCol] != "disc") {
 ## continuous metadata  →  rank-based gradient ---------------------------
vals <- metadata[, metaCol]

# Handle missing values up front
na.inx <- is.na(vals)

# Rank the values (1 = smallest, N = largest); ties get the average rank
rnk <- rank(vals, ties.method = "average", na.last = "keep")

# Convert to 0–1 scale; NA stays NA
vals.norm <- (rnk - 1) / (max(rnk, na.rm = TRUE) - 1)

# Optional: centralise NA as mid-grey instead of dropping them
vals.norm[na.inx] <- 0.5

n_cols <- length(unique(rnk[!is.na(rnk)]))

# Map to a perceptually uniform gradient (viridis works well)
  if (colorGradient == "blue") {
    blues_core <- RColorBrewer::brewer.pal(9, "Blues")[3:9]  # keep index 3-9
    pal_fun    <- grDevices::colorRampPalette(blues_core)
    colors     <- pal_fun(n_cols)
  }else if (colorGradient == "gbr") {
    colors <- grDevices::colorRampPalette(c("green", "black", "red"), space="rgb")(n_cols)
  } else if (colorGradient == "heat") {
    pal_fun <- grDevices::colorRampPalette(c("#FFFF00", "#FC8D25", "#9B0000"))   # or choose your own stops
    colors  <- pal_fun(n_cols)
  } else if (colorGradient == "topo") {
    colors <- grDevices::topo.colors(100)
  } else if (colorGradient == "gray") {
    colors <- grDevices::colorRampPalette(c("grey90", "grey10"), space="rgb")(256)
  } else if (colorGradient == "byr") {
    colors <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(256))
  } else if (colorGradient == "viridis") {
    colors <- rev(viridis::viridis(10))
  } else if (colorGradient == "plasma") {
    colors <- rev(viridis::plasma(10))
  } else if (colorGradient == "npj") {
    colors <- c("#00A087FF", "white", "#E64B35FF")
  } else if (colorGradient == "aaas") {
    colors <- c("#4DBBD5FF", "white", "#E64B35FF")
  } else if (colorGradient == "d3") {
    colors <- c("#2CA02CFF", "white", "#FF7F0EFF")
  } else {
    colors <- c("#0571b0", "#92c5de", "white", "#f4a582", "#ca0020")
  }

pca3d$score$colors              <- my.col2rgb(colors)
pca3d$score$color_legend_vals   <- vals
pca3d$score$color_legend_type   <- "gradient"
pca3d$score$color_legend_breaks <- pretty(vals, 5)   # nice tick labels 
} else {
  cols <- unique(GetColorSchema(pca3d$score$facA)); 
  pca3d$score$colors <- my.col2rgb(cols);
  pca3d$score$color_legend_type <- "discrete"
}

  json.obj <- rjson::toJSON(pca3d);
  sink(fileNm);
  cat(json.obj);
  sink();

  qs::qsave(pca3d$score, "score3d.qs");
  qs::qsave(pca3d$loadings, "loading3d.qs");

  if(!exists("my.json.scatter")){
    .load.scripts.on.demand("util_scatter3d.Rc");    
  }

  my.json.scatter(fileNm, TRUE);
  return(.set.mSet(mSetObj));
}




# Plot Venn diagram
plotVennDiagram <- function(object, include="both", names, mar=rep(0,4), cex=1.2, lwd=1, circle.col, counts.col, show.include,...){
    if(!exists("my.plot.venn")){
        .load.scripts.on.demand("util_venndiagram.Rc");  
    }
    return(my.plot.venn(object, include, names, mar, cex, lwd, circle.col, counts.col, show.include,...));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetAov2SigFileName <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov2$sig.nm;
}

GetAov2SigMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(as.matrix(mSetObj$analSet$aov2$sig.mat)));
}

GetAov2SigRowNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$aov2$sig.mat);
}

GetAov2SigColNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$aov2$sig.mat);
}

#'Sig table for AOV2
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Aov2<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$aov2$sig.mat, "Significant features identified by advanced ANOVA", mSetObj$dataSet$type);
}

GetAnova2UpMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov2$p.log;
  red.inx<- which(mSetObj$analSet$aov2$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAov2UpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<- which(mSetObj$analSet$aov2$inx.imp);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$aov2$p.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetAnova2DnMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov2$p.log;
  blue.inx <- which(!mSetObj$analSet$aov2$inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAov2DnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<- which(!mSetObj$analSet$aov2$inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$aov2$p.log)[blue.inx]);
  }else{
    return("NA");
  }
}
PlotPCA2DScoreMeta <- function(mSetObj = NA, imgName, format = "png", dpi = default.dpi,
                               width = NA, pcx, pcy, reg = 0.95, show = 1,
                               grey.scale = 0, cex.opt = "na",
                               meta = NULL, metaShape = NULL) {
  save.image("2dscore.RData");
  if (!exists("pca.cex", envir = .GlobalEnv)){
    pca.cex <<- 1
  }
  if (cex.opt == "increase"){
    pca.cex <<- pca.cex + 0.1
  }else if (cex.opt == "decrease"){
    pca.cex <<- pca.cex - 0.1
  }
  
  ## ── bookkeeping -----------------------------------------------------
  mSetObj  <- .get.mSet(mSetObj)
  meta.inf <- mSetObj$dataSet$meta.info
  meta.typ <- mSetObj$dataSet$meta.types
  
  pca.rn <- rownames(mSetObj$analSet$pca$x)   # canonical sample order
  
  ## 1. Re-order full metadata table (in case you need it later)
  meta.inf <- meta.inf[match(pca.rn, rownames(meta.inf)), , drop = FALSE]
  
  ## ----- colour & shape variables ------------------------------------
  colourVar <- mSetObj$dataSet$cls
  colourTyp <- mSetObj$dataSet$cls.type
  if (!is.null(meta) && meta %in% colnames(meta.inf)) {
    idx        <- which(colnames(meta.inf) == meta)
    colourVar  <- meta.inf[[idx]]
    colourTyp  <- meta.typ[[idx]]
  }
  
  shapeVar <- NULL; shapeTyp <- NULL
  if (!is.null(metaShape) && metaShape %in% colnames(meta.inf)) {
    idx       <- which(colnames(meta.inf) == metaShape)
    shapeVar  <- meta.inf[[idx]]
    shapeTyp  <- meta.typ[[idx]]
  }
  
  ## ── PCA coordinates -------------------------------------------------
  pc1 <- mSetObj$analSet$pca$x[, pcx]
  pc2 <- mSetObj$analSet$pca$x[, pcy]
  
  ## 3. Drop any rows whose metadata became NA (keeps data consistent)
  keep <- !is.na(colourVar)            # add & !is.na(shapeVar) if needed
  pc1  <- pc1[keep];    pc2  <- pc2[keep]
  txt  <- substr(names(pc1), 1, 14)    # regenerate labels in sync
  colourVar <- colourVar[keep]
  shapeVar  <- if (!is.null(shapeVar)) shapeVar[keep] else NULL
  pca.rn    <- pca.rn[keep]            # updated canonical order
  
  txt <- substr(names(pc1), 1, 14)
  
  xlabel <- sprintf("PC%d (%.1f%%)", pcx, 100 * mSetObj$analSet$pca$variance[pcx])
  ylabel <- sprintf("PC%d (%.1f%%)", pcy, 100 * mSetObj$analSet$pca$variance[pcy])
  
  ## ── graphics device -------------------------------------------------
  imgName <- paste0(imgName, "dpi", dpi, ".", format)
  w <- ifelse(is.na(width) || width == 0, 9, width); h <- w - 1
  mSetObj$imgSet$pca.score2d <- imgName
  
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi,
               width = w, height = h, type = format, bg = "white")
  op <- par(mar = c(5, 5, 3, 8))
  
  ## ── discrete-colour branch -----------------------------------------
  if (colourTyp == "disc") {
    
    if (mSetObj$dataSet$type.cls.lbl == "integer")
      colourVar <- factor(as.numeric(levels(colourVar))[colourVar])
    lvs <- levels(colourVar)
    
    ## ellipses ---------------------------------------------------------
    pts.array <- array(0, dim = c(100, 2, length(lvs)))
    for (i in seq_along(lvs)) {
      inx <- colourVar == lvs[i]
      groupVar  <- var(cbind(pc1[inx], pc2[inx]), na.rm = TRUE)
      groupMean <- c(mean(pc1[inx], na.rm = TRUE), mean(pc2[inx], na.rm = TRUE))
      pts.array[, , i] <- ellipse::ellipse(groupVar, centre = groupMean,
                                           level = reg, npoints = 100)
    }
    
    xrg <- range(pc1, pts.array[, 1, ]);  yrg <- range(pc2, pts.array[, 2, ])
    ext <- function(r) diff(r) / 12
    xlim <- c(xrg[1] - ext(xrg), xrg[2] + ext(xrg))
    ylim <- c(yrg[1] - ext(yrg), yrg[2] + ext(yrg))
    
    col.def <- GetColorSchema(colourVar, grey.scale == 1)
    cols    <- ExpandSchema(colourVar, col.def)
    
    ## shapes -----------------------------------------------------------
    if (!is.null(shapeVar) && shapeTyp == "disc") {
      shapeVar <- factor(shapeVar)
      pchs.def <- GetShapeSchemaMeta(shapeVar, show, grey.scale)
      pchs     <- ExpandSchema(shapeVar, pchs.def)
    } else {
      pchs.def <- NULL;
      pchs     <- 21
    }
    
    if (grey.scale) cols <- rep("black", length(cols))
    
    ## base plot --------------------------------------------------------
    plot(pc1, pc2, xlab = xlabel, ylab = ylabel,
         xlim = xlim, ylim = ylim, type = "n",
         col = "black", pch = pchs, main = "Scores Plot")
    grid(col = "lightgray", lty = "dotted", lwd = 1)
    
    ## ellipses
    for (i in seq_along(lvs)) {
      poly.col <- if (length(col.def) > 1) col.def[lvs[i]] else col.def
      polygon(pts.array[, , i], col = adjustcolor(poly.col, alpha = 0.2),
              border = NA)
      if (grey.scale)
        lines(pts.array[, , i], col = adjustcolor("black", 0.5), lty = 2)
    }
    
    ## points / labels
    if (show == 1) {
      text(pc1, pc2, label = txt, pos = 4, xpd = TRUE, cex = 0.75 * pca.cex)
      points(pc1, pc2, pch = pchs, col = "black", bg = cols)
    } else {
      points(pc1, pc2, pch = pchs, col = "black",
             bg = adjustcolor(cols, 0.4), cex = 1.8 * pca.cex)
    }
    
  } else {          ## ── continuous-colour branch ──────────────────────────
    
    ## full-sample confidence ellipse (drawn before points)
    groupVar  <- var(cbind(pc1, pc2), na.rm = TRUE)
    groupMean <- c(mean(pc1, na.rm = TRUE), mean(pc2, na.rm = TRUE))
    ell.all   <- ellipse::ellipse(groupVar, centre = groupMean,
                                  level = reg, npoints = 200)
    
    blues <- colorRampPalette(brewer.pal(9, "Blues"))(20)
    numv  <- as.numeric(as.character(colourVar))
    cols  <- blues[cut(numv, breaks = 20, labels = FALSE)]
    
    ## shapes (optional second metadata)
    if (!is.null(shapeVar) && shapeTyp == "disc") {
      shapeVar <- factor(shapeVar)
      pchs.def <- GetShapeSchemaMeta(shapeVar, show, grey.scale)
      pchs     <- ExpandSchema(shapeVar, pchs.def)
    } else {
      pchs <- 21
    }
    
    ## ---- expand axis limits so the ellipse is fully visible ------------
    xrg  <- range(c(pc1, ell.all[, 1]), na.rm = TRUE)
    yrg  <- range(c(pc2, ell.all[, 2]), na.rm = TRUE)
    ext  <- function(r) diff(r) * 0.07          # 7 % margin
    xlim <- c(xrg[1] - ext(xrg), xrg[2] + ext(xrg))
    ylim <- c(yrg[1] - ext(yrg), yrg[2] + ext(yrg))
    
    ## base plot ----------------------------------------------------------
    plot(pc1, pc2,
         xlab = xlabel, ylab = ylabel,
         xlim = xlim,  ylim = ylim,
         type = "n",   main = "Scores Plot")
    grid(col = "lightgray", lty = "dotted", lwd = 1)
    
    ## draw ellipse for the whole data cloud
    polygon(ell.all,
            col = adjustcolor("grey70", alpha.f = 0.25),
            border = "grey50", lty = 2)
    
    ## points and (optional) labels --------------------------------------
    points(pc1, pc2, pch = pchs, col = "black", bg = cols, cex = 1.8 * pca.cex)
    if (show == 1)
      text(pc1, pc2, label = txt, pos = 4,
           col = cols,                 # ← use per-sample colours
           xpd = TRUE, cex = 0.8 * pca.cex)
  }
  
  ## ── legends --------------------------------------------------------------
  ## ── LEGEND HANDLING (4 explicit cases) ─────────────────────────────────
  par(xpd = TRUE)                                    # draw in margin
  usr  <- par("usr"); dx <- par("cxy")[1]
  xleg <- usr[2] + dx                                # right-hand margin
  yleg <- usr[4] - dx                                # top of plot
  
  hasShape <- !is.null(shapeVar) && shapeTyp == "disc"
  isDisc   <- colourTyp == "disc"
  isCont   <- colourTyp == "cont"
  
  if (isDisc && hasShape) {                    # 1 · colour-disc + shape-disc
    colLvls <- levels(colourVar)
    shpLvls <- levels(shapeVar)
    legend(xleg, yleg,
           legend = c(colLvls, shpLvls),
           pch    = c(rep(22, length(colLvls)), pchs.def[shpLvls]),
           pt.bg  = c(col.def[colLvls], rep(NA, length(shpLvls))),
           col    = "black", pt.cex = 1.4, box.lty = 0)
    
  } else if (isCont && hasShape) {      # 2 · colour-cont  + shape-disc ─────────
    shapeLevels <- levels(shapeVar)
    cx  <- par("cxy")[1]                # 1 char-width
    pad <- cx * 0.3                     # small vertical gap
    
    bar_w   <- cx * 0.6                 # width of the bar
    bar_x0  <- usr[2] + cx              # start one char right of plot
    bar_x1  <- bar_x0 + bar_w
    bar_y1  <- usr[4] - cx              # a little below top margin
    bar_y0  <- bar_y1 - cx * 3          # ≈ 3 text lines tall
    
    ## draw the gradient (100 stops → smooth)
    nCol  <- 100
    grad  <- colorRampPalette(brewer.pal(9, "Blues"))(nCol)
    yseq  <- seq(bar_y0, bar_y1, length.out = nCol + 1)
    for (i in 1:nCol)
      rect(bar_x0, yseq[i], bar_x1, yseq[i + 1],
           col = grad[i], border = NA)    
    
    ## “High/Low” labels
    text(bar_x1 + cx * 0.3, bar_y1, "High", adj = 0, cex = 0.75)
    text(bar_x1 + cx * 0.3, bar_y0, "Low",  adj = 0, cex = 0.75)
    
    # Estimate legend height quickly: pt.cex=1.4 ⇒ symbol ≈ 1.4 × text height
    lgd_y1 <- bar_y0 - pad              # top of legend (just below bar)
    lgd_y0 <- lgd_y1 - (1.4 * length(shapeLevels) + 1) * cx
    
    legend(x      = bar_x0,   y      = lgd_y1,
           legend = shapeLevels,
           pch    = pchs.def[shapeLevels],
           col    = "black",
           pt.cex = 1.4,
           box.lty = 0,
           xjust  = 0,      yjust = 1)  
  } else if (isCont) {                          # 3 · colour-cont only
    barW   <- dx * 0.6
    nCol   <- 100
    grad   <- colorRampPalette(brewer.pal(9, "Blues"))(nCol)
    
    fullH  <- usr[4] - usr[3]            # total plotting height
    barH   <- fullH * 0.33               # want one‑third
    y0     <- usr[3] + (fullH - barH)/2  # bottom of bar (centred)
    yseq   <- seq(y0, y0 + barH, length.out = nCol + 1)
    
    for (i in 1:nCol)
      rect(xleg, yseq[i], xleg + barW, yseq[i + 1],
           col = grad[i], border = NA)
    
    ## labels at ends of the shorter bar -------------------------
    text(xleg + barW * 1.2, yseq[length(yseq)], "High", adj = 0, cex = 0.75)
    text(xleg + barW * 1.2, yseq[1],           "Low",  adj = 0, cex = 0.75)
    
  } else if (isDisc) {                          # 4 · colour-disc only
    colLvls <- levels(colourVar)
    legend(xleg, yleg,
           legend = colLvls, pch = 22,
           pt.bg  = col.def[colLvls], col = "black",
           pt.cex = 1.4, box.lty = 0)
  }
  par(xpd = FALSE)
  
  ## ── PERMANOVA & cleanup --------------------------------------------
  mSetObj$analSet$pca$permanova.res <-
    ComputePERMANOVA(pc1, pc2,colourVar, 999, colourTyp)
  par(op); dev.off()
  invisible(.set.mSet(mSetObj))
}

GetShapeSchemaMeta<- function(my.cls, show.name = 0, grey.scale = 0) {
  lvs      <- levels(my.cls)
  grp.num  <- length(lvs)
  
  ## If user supplied a shapeVec (and all values are non-negative) → honour it
  if (exists("shapeVec") && all(shapeVec >= 0) && isTRUE(any(names(shapeVec) %in% lvs))) {
    shapes <- shapeVec[names(shapeVec) %in% lvs]
    
  } else {                                        # ---- auto-generate shapes
      ## 21–25 first, then cycle back to 1,2,3… if more groups
      base.seq <- c(21:25, 1:20)                  # covers 25 groups
      shapes   <- rep(base.seq, length.out = grp.num)
      
}
  
  names(shapes) <- lvs
  return(shapes)
}