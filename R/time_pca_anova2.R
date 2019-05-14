#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform within-subjects anova
#'@param x Input the data
#'@param time.fac Input the time factor
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

aov.within <- function(x, time.fac){
  unlist(summary(aov(x ~ (aov.facA*aov.facB) + Error(aov.sbj/time.fac))), use.names=F)[c(7,20,21,9,23,24)];
}

aov.within.wo <- function(x, time.fac){
  unlist(summary(aov(x ~ (aov.facA+aov.facB))), use.names=F)[c(10,11,13,14)];
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
aov.repeated <- function(x, time.fac){
  unlist(summary(aov(x ~ time.fac + Error(aov.sbj/time.fac))), use.names=F)[c(12,14)];
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform between-subjects anova
#'@param x Input data to perform 2-way ANOVA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

aov.between <- function(x){
  unlist(summary(aov(x ~ aov.facA*aov.facB)), use.names=F)[c(13,14,15,17,18,19)];
}

aov.between.type3 <- function(x){
  unlist(car::Anova(lm(x ~ aov.facA*aov.facB), type="3"))[c(12,13,14,17,18,19)];
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'@usage ANOVA2.Anal(mSetObj=NA, thresh=0.05, p.cor="fdr", type="time0", aov.type=1, use.interact=1)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param thresh Input the p-value threshold 
#'@param p.cor Select method for p-value correction, bonferroni, holm or fdr
#'@param type Select b to perform between-subjects ANOVA, and w for within-subjects ANOVA 
#'@param aov.type Specify 1 for ANOVA type 1, or 3 for ANOVA type 3
#'@param use.interact Numeric, whether to consider interaction in two-way repeated ANOVA (1) or not (0).
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ANOVA2.Anal <-function(mSetObj=NA, thresh=0.05, p.cor="fdr", type="time0", aov.type=1, use.interact=1){

  mSetObj <- .get.mSet(mSetObj);
  
  if(type == "time0"){
    time.fac <- mSetObj$dataSet$time.fac;
    mSetObj$dataSet$sbj <- as.factor(mSetObj$dataSet$exp.fac);
    
    if(.on.public.web){
      .set.mSet(mSetObj);
    }
    
    # first create the subjects that being measured under different time
    # points (under same phenotype/ exp. condition). The design must be balanced
    
    # first check if balanced
    res <- table (time.fac, mSetObj$dataSet$sbj);
    res.mean <- apply(res, 2, mean);
    all.res <- res/res.mean;
    if(sum(all.res != 1) > 0){
      AddErrMsg("Experiment design is not balanced!");
      return(0);
    }
    aov.sbj <<- mSetObj$dataSet$sbj
    aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.repeated, time.fac));
    
    rm(aov.sbj, pos=".GlobalEnv")
    
    fileName <- "oneway_anova_repeated.csv";
    rownames(aov.mat)<-colnames(mSetObj$dataSet$norm);
    aov.mat <- cbind(aov.mat, p.adjust(aov.mat[,2], p.cor));
    colnames(aov.mat) <- c("F-value", "Raw P-val", "Adjusted P-val");
    p.value <- aov.mat[,3];
    inx.imp <-aov.mat[,3] <= thresh;
    aov.mat <- aov.mat[inx.imp, ,drop=F];
    vennC <- NULL;
    # default sort first by main effect: treatment, then by ...
    ord.inx <- order(aov.mat[,2], decreasing = FALSE);
  }else{
    if(type=="time"){
      # first create the subjects that being measured under different time
      # points (under same phenotype/ exp. condition). The design must be balanced
      
      # first check if balanced
      res <- table(mSetObj$dataSet$facA, mSetObj$dataSet$facB);
      res.mean <- apply(res, 2, mean);
      all.res <- res/res.mean;
      if(sum(all.res != 1) > 0){
        AddErrMsg("Experiment design is not balanced!");
        return(0);
      }
      time.fac <- mSetObj$dataSet$time.fac;
      exp.fac <- mSetObj$dataSet$exp.fac;
      
      sbj <- vector(mode="character", length=nrow(mSetObj$dataSet$norm));
      k = 1;
      len = 0;
      for(lv1 in levels(exp.fac)){
        # same subjects must in the same exp. condition
        inx1 <- exp.fac == lv1;
        for(lv2 in levels(time.fac)){
          inx2 <- time.fac == lv2;
          len <- sum(inx1 & inx2);
          
          # same subjects must not in the same time points
          # so in a balanced design and ordered by the time points
          # all samples in each time points will sweep all subjects (one go)
          sbj[inx1 & inx2] <- paste("S", k:(k+len-1), sep="");
        }
        k = k + len;
      }
      
      mSetObj$dataSet$sbj <- as.factor(sbj);  
      
      if(.on.public.web){
        .set.mSet(mSetObj);
      }
      
      aov.facA <<- mSetObj$dataSet$facA;
      aov.facB <<- mSetObj$dataSet$facB;
      aov.sbj <<- mSetObj$dataSet$sbj;
      
      if(use.interact){
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.within, time.fac));
      }else{
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.within.wo, time.fac));
      }
      
      rm(aov.facA, aov.facB, aov.sbj, pos=".GlobalEnv");
      fileName <- "anova_within_sbj.csv";
      
    }else{ # g2 (two-factor analysis)
      aov.facA <<- mSetObj$dataSet$facA;
      aov.facB <<- mSetObj$dataSet$facB;
      
      if(aov.type == 1){
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.between));
      }else{
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.between.type3));
      }
      rm(aov.facA, aov.facB, pos=".GlobalEnv");
      
      fileName <- "anova_between_sbj.csv";
    }
    
    rownames(aov.mat) <- colnames(mSetObj$dataSet$norm);
    
    if(use.interact){
      if(p.cor != "none"){
        aov.mat2 <- cbind (aov.mat, p.adjust(aov.mat[,4], p.cor),
                          p.adjust(aov.mat[,5], p.cor),
                          p.adjust(aov.mat[,6], p.cor));
      }
      
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
      
    }else{
      if(p.cor != "none"){
        aov.mat2 <- cbind(aov.mat, p.adjust(aov.mat[,3], p.cor), p.adjust(aov.mat[,4], p.cor));
      }
      
      sig.facA <-(aov.mat2[,5] <= thresh);
      sig.facB <-(aov.mat2[,6] <= thresh);
      
      all.match <- cbind(sig.facA, sig.facB);
      colnames(all.match) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      colnames(aov.mat2) <- c(paste(mSetObj$dataSet$facA.lbl, "(F.val)", sep = ""), 
                              paste(mSetObj$dataSet$facB.lbl, "(F.val)", sep = ""),
                              paste(mSetObj$dataSet$facA.lbl, "(raw.p)", sep = ""), 
                              paste(mSetObj$dataSet$facB.lbl, "(raw.p)", sep = ""),
                              paste(mSetObj$dataSet$facA.lbl, "(adj.p)", sep = ""), 
                              paste(mSetObj$dataSet$facB.lbl, "(adj.p)", sep = ""))
      
      vennC <- getVennCounts(all.match);
      p.value <- aov.mat2[,5]; 
      inx.imp <- sig.facA | sig.facB;
      aov.mat2 <- aov.mat2[, c(1,3,5,2,4,6),drop=F];
      
    }
    # default sort first by main effect: treatment, then by ...
    aov.mat <- aov.mat2[inx.imp, ,drop=F];
    ord.inx <- order(aov.mat[,2], aov.mat[,3], decreasing = FALSE);
  }
  
  aov.mat <- signif(aov.mat[ord.inx,,drop=F], 5);
  write.csv(aov.mat, file=fileName);
  
  aov2<-list (
    type = type,
    sig.nm = fileName,
    thresh = -log10(thresh),
    multi.c = p.cor,
    sig.mat = aov.mat,
    p.log = -log10(p.value),
    inx.imp = inx.imp,
    vennC = vennC
  );
  
  mSetObj$analSet$aov2 <- aov2;
  return(.set.mSet(mSetObj));
}

#'Plot Venn diagram of ANOVA results
#'@description Plot Venn diagram of ANOVA results
#'@usage PlotANOVA2(mSetObj, imgName, format="png", dpi=72, width=NA)
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
PlotANOVA2 <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 7;
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
    title <- ifelse(mSetObj$analSet$aov2$type == "g2", "Two-way ANOVA (between subjects)", "Two-way ANOVA (within subject)");
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
#'@usage iPCA.Anal(mSetObj, fileNm)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fileNm select a file name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@importFrom plotly plot_ly add_markers layout
#'
iPCA.Anal<-function(mSetObj=NA, fileNm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pca <- prcomp(mSetObj$dataSet$norm, center=T, scale=F);
  imp.pca <- summary(pca)$importance;
  
  pca3d <- list();
  pca3d$score$axis <- paste("PC", 1:3, " (", 100*round(imp.pca[2,][1:3], 3), "%)", sep="");
  coords <- data.frame(t(signif(pca$x[,1:3], 5)));
  colnames(coords) <- NULL; 
  pca3d$score$xyz <- coords;
  pca3d$score$name <- rownames(mSetObj$dataSet$norm);
  facA <- as.character(mSetObj$dataSet$facA);
  
  if(all.numeric(facA)){
    facA <- paste("Group", facA);
  }
  
  pca3d$score$facA <- facA;
  facB <- as.character(mSetObj$dataSet$facB);
  
  if(all.numeric(facB)){
    facB <- paste("Group", facB);
  }
  
  pca3d$score$facB <- facB;
  
  pca3d$loadings$axis <- paste("Loadings", 1:3);
  coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
  colnames(coords) <- NULL; 
  pca3d$loadings$xyz <- coords;
  pca3d$loadings$name <- colnames(mSetObj$dataSet$norm);
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")});
  pca3d$score$colors <- cols;
  
  json.obj <- RJSONIO::toJSON(pca3d, .na='null');
  sink(fileNm);
  cat(json.obj);
  sink();
  
  if(!.on.public.web){
    
    uniq.facA <- unique(facA)
    uniq.facB <- unique(facB)
    
    if(length(uniq.facA) > 3){
      col <- RColorBrewer::brewer.pal(length(uniq.pchs), "Set3")
    }else{
      col <- c("#1972A4", "#FF7070")
    }
    
    s.xlabel <- pca3d$score$axis[1]
    s.ylabel <- pca3d$score$axis[2]
    s.zlabel <- pca3d$score$axis[3]
    
    all.symbols <- c("circle", "cross", "diamond", "x", "square", "circle-open",
                     "square-open", "diamond-open")
    
    b.symbol <- all.symbols[1:length(uniq.facB)]
    
    # first, scores plot
    data.s <- data.frame(cbind(t(pca3d$score$xyz), as.numeric(mSetObj$dataSet$facB)))
    
    p.s <- plotly::plot_ly(x = data.s[, 1], y = data.s[, 2], z = data.s[, 3],
                         color = mSetObj$dataSet$facA, colors = col) 
    p.s <- plotly::add_markers(p.s, sizes = 5, symbol = data.s[,4], symbols = b.symbol)
    p.s <- plotly::layout(p.s, scene = list(xaxis = list(title = s.xlabel),
                                        yaxis = list(title = s.ylabel),
                                        zaxis = list(title = s.zlabel)))
    
    # second, loadings plot
    l.xlabel <- pca3d$loadings$axis[1]
    l.ylabel <- pca3d$loadings$axis[2]
    l.zlabel <- pca3d$loadings$axis[3]
    
    data.l <- data.frame(cbind(t(pca3d$loadings$xyz)))
    
    p.l <- plotly::plot_ly(x = data.l[, 1], y = data.l[, 2], z = data.l[, 3], colors = col[1]) 
    p.l <- plotly::add_markers(p.l, sizes = 5)
    p.l <- plotly::layout(p.l, scene = list(xaxis = list(title = l.xlabel),
                                        yaxis = list(title = l.ylabel),
                                        zaxis = list(title = l.zlabel)))
    
    mSetObj$imgSet$time$score3d <- p.s
    mSetObj$imgSet$time$load3d <- p.l
  }
  
  if(!.on.public.web){
    print("Interactive scores and loading plots have been created, please find
          them in mSet$imgSet$time$score3d and mSet$imgSet$time$load3d.")
    return(.set.mSet(mSetObj));
  }
  save.image(file="time.RData")
}


#vertically stacked boxplot
PlotVerticalCmpdSummary<-function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  if(.on.public.web){
    load_ggplot()
    load_grid()
  }
  
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_summary_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    h <- 9;
  }else{
    h <- width;
  }
  
  if(substring(mSetObj$dataSet$format,4,5)!="ts"){
    
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height= w*5/9, type=format, bg="white");
    par(mar=c(4,4,2,2), mfrow = c(1,2), oma=c(0,0,2,0));
    
    # need to consider norm data were edited, different from proc
    smpl.nms <- rownames(mSetObj$dataSet$norm);
    
    mns <- by(as.numeric(mSetObj$dataSet$proc[smpl.nms, cmpdNm]), mSetObj$dataSet$cls, mean, na.rm=T);
    sds <- by(as.numeric(mSetObj$dataSet$proc[smpl.nms, cmpdNm]), mSetObj$dataSet$cls, sd, na.rm=T);
    
    ups <- mns + sds;
    dns <- mns - sds;
    
    # all concentration need start from 0
    y <- c(0, dns, mns, ups);
    
    rg <- range(y) + 0.05 * diff(range(y)) * c(-1, 1)
    pt <- pretty(y)
    
    axp=c(min(pt), max(pt[pt <= max(rg)]),length(pt[pt <= max(rg)]) - 1);
    
    # ymk <- pretty(c(0,ymax));
    x <- barplot(mns, col= unique(GetColorSchema(mSetObj)), las=2, yaxp=axp, ylim=range(pt));
    arrows(x, dns, x, ups, code=3, angle=90, length=.1);
    axis(1, at=x, col="white", col.tick="black", labels=F);
    box();
    mtext("Original Conc.", line=1);
    
    boxplot(mSetObj$dataSet$norm[, cmpdNm]~mSetObj$dataSet$cls,las=2, col= unique(GetColorSchema(mSetObj)));
    mtext("Normalized Conc.", line=1);
    title(main=cmpdNm, out=T);
    dev.off();
    
  }else if(mSetObj$dataSet$design.type =="time0"){
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=8, height= 6, type=format, bg="white");
    plotProfile(mSetObj, cmpdNm);
    dev.off();
    
  }else{
    if(mSetObj$dataSet$design.type =="time"){ # time trend within phenotype
      out.fac <- mSetObj$dataSet$exp.fac;
      in.fac <- mSetObj$dataSet$time.fac;
      xlab = "Time";
    }else{ # factor a split within factor b
      out.fac <- mSetObj$dataSet$facB;
      in.fac <- mSetObj$dataSet$facA;
      xlab = mSetObj$dataSet$facA.lbl;
    }
    
    # two images per row
    img.num <- length(levels(out.fac));
    row.num <- ceiling(img.num/2);
    
    if(img.num > 2){
      col.num=2
    }else{
      col.num=1
    }
    
    if(row.num == 1){
      w <- h*5/9;
    }else{
      w <- h*0.5*row.num;
    }
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    
    groupNum <- length(levels(in.fac))
    pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
              "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
              "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928");
    col.fun <- grDevices::colorRampPalette(pal12)
    group_colors <- col.fun(groupNum)
    
    p_all <- list()
    
    for(lv in levels(out.fac)){
      inx <- out.fac == lv;
      df.orig <- data.frame(facA = lv, value = mSetObj$dataSet$norm[inx, cmpdNm], name = in.fac[inx])
      p_all[[lv]] <- df.orig
    }
    
    alldata <- do.call(rbind, p_all)
    
    p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + geom_boxplot() + theme_bw() + geom_jitter(size=1) 
    p.time <- p.time + facet_wrap(~facA, ncol=col.num) + theme(axis.title.x = element_blank(), legend.position = "none")
    p.time <- p.time + scale_fill_manual(values=group_colors) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance")
    p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p.time <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
  
    print(p.time)
    dev.off()
  }
  return(imgName);
}

# Plot Venn diagram
# Capabilities for multiple counts and colors by Francois Pepin
# Gordon Smyth, James Wettenhall
# 4 July 2003.  Last modified 12 March 2010

plotVennDiagram <- function(object, include="both", names, mar=rep(0,4), cex=1.2, lwd=1, circle.col, counts.col, show.include,...){
  if (!is(object, "VennCounts")){
    if (length(include)>2) stop("Cannot plot Venn diagram for more than 2 sets of counts")
    if (length(include)==2) object.2 <- getVennCounts(object, include = include[2])
    object <- getVennCounts(object, include = include[1])
  }
  else if(length(include==2)) include <- include[1]
  nsets <- ncol(object)-1
  if(nsets > 3) stop("Can't plot Venn diagram for more than 3 sets")
  if(missing(names)) names <- colnames(object)[1:nsets]
  counts <- object[,"Counts"]
  if(length(include)==2) counts.2 <- object.2[, "Counts"]
  if(missing(circle.col)) circle.col <- par('col')
  if(length(circle.col)<nsets) circle.col <- rep(circle.col,length.out=nsets)
  if(missing(counts.col)) counts.col <- par('col')
  if(length(counts.col)<length(include)) counts.col <- rep(counts.col,length.out=length(include))
  if(missing(show.include)) show.include <- as.logical(length(include)-1)
  theta <- 2*pi*(0:360)/360
  xcentres <- list(0,c(-1,1),c(-1,1,0))[[nsets]]
  ycentres <- list(0,c(0,0),c(1/sqrt(3),1/sqrt(3),-2/sqrt(3)))[[nsets]]
  r <- c(1.5,1.5,1.5)[nsets]
  xtext <- list(-1.2,c(-1.2,1.2),c(-1.2,1.2,0))[[nsets]]
  ytext <- list(1.8,c(1.8,1.8),c(2.4,2.4,-3))[[nsets]]
  old.par <- par(mar=mar)
  on.exit(par(old.par))
  plot(x=0,y=0,type="n",xlim=c(-4,4),ylim=c(-4,4),xlab="",ylab="",axes=FALSE,...);
  
  circle.col <- col2rgb(circle.col) / 255
  circle.col <- rgb(circle.col[1,], circle.col[2,], circle.col[3,], 0.3)
  for(i in 1:nsets) {
    lines(xcentres[i]+r*cos(theta),ycentres[i]+r*sin(theta),lwd=lwd,col=circle.col[i])
    polygon(xcentres[i] + r*cos(theta), ycentres[i] + r*sin(theta), col = circle.col[i], border = NULL)
    text(xtext[i],ytext[i],names[i],cex=cex)
  }
  switch(nsets,
         {
           rect(-3,-2.5,3,2.5)
           printing <- function(counts, cex, adj,col,leg){
             text(2.3,-2.1,counts[1],cex=cex,col=col,adj=adj)
             text(0,0,counts[2],cex=cex,col=col,adj=adj)
             if(show.include) text(-2.3,-2.1,leg,cex=cex,col=col,adj=adj)
           }
           
         }, {
           rect(-3,-2.5,3,2.5)
           printing <- function(counts, cex, adj,col,leg){
             text(2.3,-2.1,counts[1],cex=cex,col=col,adj=adj)
             text(1.5,0.1,counts[2],cex=cex,col=col,adj=adj)
             text(-1.5,0.1,counts[3],cex=cex,col=col,adj=adj)
             text(0,0.1,counts[4],cex=cex,col=col,adj=adj)
             if(show.include) text(-2.3,-2.1,leg,cex=cex,col=col,adj=adj)
           }
         }, {
           rect(-3,-3.5,3,3.3)
           printing <- function(counts, cex, adj,col,leg){
             text(2.5,-3,counts[1],cex=cex,col=col,adj=adj)
             text(0,-1.7,counts[2],cex=cex,col=col,adj=adj)
             text(1.5,1,counts[3],cex=cex,col=col,adj=adj)
             text(.75,-.35,counts[4],cex=cex,col=col,adj=adj)
             text(-1.5,1,counts[5],cex=cex,col=col,adj=adj)
             text(-.75,-.35,counts[6],cex=cex,col=col,adj=adj)
             text(0,.9,counts[7],cex=cex,col=col,adj=adj)
             text(0,0,counts[8],cex=cex,col=col,adj=adj)
             if(show.include) text(-2.5,-3,leg,cex=cex,col=col,adj=adj)
           }
         }
  )
  adj <- c(0.5,0.5)
  if (length(include)==2)
    adj <- c(0.5,0)
  printing(counts,cex,adj,counts.col[1],include[1])
  if (length(include)==2) printing(counts.2,cex,c(0.5,1),counts.col[2],include[2])
  invisible()
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
  as.matrix(cbind(red.inx, lod[red.inx]));
}

GetAnova2DnMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  blue.inx <- which(!mSetObj$analSet$aov2$inx.imp);
  as.matrix(cbind(blue.inx, lod[blue.inx]));
}

GetAnova2LnMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov2$p.log;
  as.matrix(rbind(c(0, mSetObj$analSet$aov2$thresh), c(length(lod)+1,mSetObj$analSet$aov2$thresh)));
}

GetAnova2Cmpds<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$aov2$p.log);
}

GetMaxAnova2Inx <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  which.max(mSetObj$analSet$aov2$p.log);
}
