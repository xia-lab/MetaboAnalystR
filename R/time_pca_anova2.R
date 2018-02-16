#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform within-subjects anova
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

aov.within <- function(x, time.fac){
  unlist(summary(aov(x ~ (aov.facA*aov.facB) + Error(aov.sbj/time.fac))), use.names=F)[c(9,23,24)];
}

aov.within.wo <- function(x, time.fac){
  unlist(summary(aov(x ~ (aov.facA+aov.facB))), use.names=F)[c(13,14)];
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'Perform repeated measure one-way anova
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
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

aov.between <- function(x){
  unlist(summary(aov(x ~ aov.facA*aov.facB)), use.names=F)[c(17,18,19)];
}

aov.between.type3 <- function(x){
  unlist(Anova(lm(x ~ aov.facA*aov.facB), type="3"))[c(17,18,19)];
}

#'Perform Two-way ANOVA 
#'@description Perform Two-way ANOVA 
#'@usage ANOVA2.Anal(mSetObj, thresh, p.cor, type)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param thresh Input the p-value threshold 
#'@param p.cor select method for p-value correction, bonferroni, holm or fdr
#'@param type select b to perform between-subjects ANOVA, and w for within-subjects ANOVA 
#'@param use.interact is whether to consider interaction in two-way repeated ANOVA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ANOVA2.Anal <-function(mSetObj, thresh=0.05, p.cor="fdr", type="time0", aov.type=1, use.interact=1){

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
      res <- table (mSetObj$dataSet$facA, mSetObj$dataSet$facB);
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
        for (lv2 in levels(time.fac)){
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
      
      aov.facA <<- mSetObj$dataSet$facA
      aov.facB <<- mSetObj$dataSet$facB
      aov.sbj <<- mSetObj$dataSet$sbj
      if(use.interact){
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.within, time.fac));
      }else{
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.within.wo, time.fac));
      }
      rm(aov.facA, pos=".GlobalEnv")
      rm(aov.facB, pos=".GlobalEnv")
      rm(aov.sbj, pos=".GlobalEnv")
      
      fileName <- "anova_within_sbj.csv";
    }else{
      aov.facA <<- mSetObj$dataSet$facA
      aov.facB <<- mSetObj$dataSet$facB
      
      if(aov.type == 1){
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.between));
      }else{
        library(car);
        aov.mat<-t(apply(as.matrix(mSetObj$dataSet$norm), 2, aov.between.type3));
      }
      rm(aov.facA, pos=".GlobalEnv")
      rm(aov.facB, pos=".GlobalEnv")
      
      fileName <- "anova_between_sbj.csv";
    }
    
    rownames(aov.mat)<-colnames(mSetObj$dataSet$norm);
    
    if(use.interact){
      if(p.cor != "none"){
        aov.mat2 <- cbind (aov.mat, p.adjust(aov.mat[,1], p.cor),
                          p.adjust(aov.mat[,2], p.cor),
                          p.adjust(aov.mat[,3], p.cor));
      }
      
      sig.facA <-(aov.mat2[,4] <= thresh);
      sig.facB <-(aov.mat2[,5] <= thresh);
      sig.intr <-(aov.mat2[,6] <= thresh);
      
      all.match <- cbind(sig.facA, sig.facB, sig.intr);
      colnames(all.match) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl, "Interaction");
      colnames(aov.mat2) <- c(paste("RawP", mSetObj$dataSet$facA.lbl, sep = "_"), paste("RawP", mSetObj$dataSet$facB.lbl, sep = "_"),
                             paste("RawP_Interaction"), paste("AdjP", mSetObj$dataSet$facA.lbl, sep = "_"), paste("AdjP", mSetObj$dataSet$facB.lbl, sep = "_"),
                             paste("AdjP_Interaction"))
      
      vennC <- getVennCounts(all.match);
      p.value <- aov.mat2[,4]; # not used
      inx.imp <- sig.facA | sig.facB | sig.intr;
      
    }else{
      if(p.cor != "none"){
        aov.mat2 <- cbind(aov.mat, p.adjust(aov.mat[,1], p.cor), p.adjust(aov.mat[,2], p.cor));
      }
      
      sig.facA <-(aov.mat2[,3] <= thresh);
      sig.facB <-(aov.mat2[,4] <= thresh);
      
      all.match <- cbind(sig.facA, sig.facB);
      colnames(all.match) <- colnames(aov.mat2) <- c(mSetObj$dataSet$facA.lbl, mSetObj$dataSet$facB.lbl);
      colnames(aov.mat2) <- c(paste("RawP", mSetObj$dataSet$facA.lbl, sep = "_"), paste("RawP", mSetObj$dataSet$facB.lbl, sep = "_"),
                              paste("AdjP", mSetObj$dataSet$facA.lbl, sep = "_"), paste("AdjP", mSetObj$dataSet$facB.lbl, sep = "_"))
      
      vennC <- getVennCounts(all.match);
      p.value <- aov.mat2[,3]; # not used
      inx.imp <- sig.facA | sig.facB;
      
    }
    aov.mat <- aov.mat2[inx.imp, ,drop=F];
    
    # default sort first by main effect: treatment, then by ...
    ord.inx <- order(aov.mat[,1], aov.mat[,2], decreasing = FALSE);
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
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
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
    Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    plotVennDiagram(mSetObj$analSet$aov2$vennC, circle.col=c("red", "blue", "green"), mar=c(0,0,2,0));
    mtext(title, NORTH<-3, line=0.25, cex=1.5);
    dev.off();
  }
  
  return(.set.mSet(mSetObj));
  
}

#'Perform PCA analysis, prepare file for interactive liveGraphics3D
#'@description Perform PCA analysis, prepare file for interactive liveGraphics3D
#'@usage iPCA.Anal(mSetObj, fileNm)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fileNm select a file name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
iPCA.Anal<-function(mSetObj=NA, fileNm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pca<-prcomp(mSetObj$dataSet$norm, center=T, scale=F);
  imp.pca<-summary(pca)$importance;
  
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
  
  library(RJSONIO);
  json.obj <- toJSON(pca3d, .na='null');
  sink(fileNm);
  cat(json.obj);
  sink();
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj));
  }
  
}

#'Plot Venn diagram
#'@description Capabilities for multiple counts and colors by Francois Pepin
#'@author Gordon Smyth, James Wettenhall
#'4 July 2003.  Last modified 12 March 2010

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
