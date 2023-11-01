#'Fold change analysis, unpaired
#'@description Perform fold change analysis, method can be mean or median
#'@usage FC.Anal(mSetObj, fc.thresh=2, cmp.type = 0, paired=FALSE)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fc.thresh Fold-change threshold, numeric input
#'@param cmp.type Comparison type, 0 for group 1 minus group 2, and 1 for group 
#'1 minus group 2
#'@param paired Logical, TRUE or FALSE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
FC.Anal <- function(mSetObj=NA, fc.thresh=2, cmp.type = 0, paired=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make sure threshold is above 1
  fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
  max.thresh = fc.thresh;
  min.thresh = 1/fc.thresh;
  
  res <- GetFC(mSetObj, paired, cmp.type);
  fc.all <- res$fc.all;
  fc.log <- res$fc.log;
  
  inx.up <- fc.all > max.thresh;
  inx.down <- fc.all < min.thresh;
  names(inx.up) <- names(inx.down) <- names(fc.all);
  imp.inx <- inx.up | inx.down;
  sig.mat <- cbind(fc.all[imp.inx, drop=F], fc.log[imp.inx, drop=F]);
  colnames(sig.mat) <- c("Fold Change", "log2(FC)");
  
  # order by absolute log value (since symmetrical in pos and neg)
  inx.ord <- order(abs(sig.mat[,2]), decreasing=T);
  sig.mat <- sig.mat[inx.ord,,drop=F];
  
  fileName <- "fold_change.csv";
  fast.write.csv(sig.mat,file=fileName);
  
  # create a list object to store fc
  mSetObj$analSet$fc<-list (
    paired = FALSE,
    raw.thresh = fc.thresh,
    max.thresh = max.thresh,
    min.thresh = min.thresh,
    fc.all = fc.all, # note a vector
    fc.log = fc.log,
    inx.up = inx.up,
    inx.down = inx.down,
    inx.imp = imp.inx,
    sig.mat = sig.mat
  );
  return(.set.mSet(mSetObj));
}


#'Plot fold change 
#'@description Plot fold change analysis
#'@usage PlotFC(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotFC <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$fc <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(5,5,2,3));
  
  fc = mSetObj$analSet$fc;
  if(fc$paired){
    ylim <- c(-nrow(mSetObj$dataSet$norm)/2, nrow(mSetObj$dataSet$norm)/2);
    xlim <- c(0, ncol(mSetObj$dataSet$norm));
    plot(NULL, xlim=xlim, ylim=ylim, xlab = GetVariableLabel(mSetObj$dataSet$type),
         ylab=paste("Count with FC >=", fc$max.thresh, "or <=", fc$min.thresh));
    for(i in 1:ncol(fc$fc.all)){
      segments(i,0, i, fc$fc.all[1,i], col= ifelse(fc$inx.up[i],"magenta", "darkgrey"),
               lwd= ifelse(fc$inx.up[i], 2, 1));
      segments(i,0, i, -fc$fc.all[2,i], col= ifelse(fc$inx.down[i], "magenta", "darkgrey"),
               lwd= ifelse(fc$inx.down[i], 2, 1));
    }
    abline(h=fc$max.thresh, lty=3);
    abline(h=fc$min.thresh, lty=3);
    abline(h=0, lwd=1);
  }else{
    if(fc$raw.thresh > 0){
      # be symmetrical
      topVal <- max(abs(fc$fc.log));
      ylim <- c(-topVal, topVal);
      plot(fc$fc.log,  ylab="Log2 (FC)", ylim = ylim, xlab = GetVariableLabel(mSetObj$dataSet$type), pch=19, axes=F,
           col= ifelse(fc$inx.imp, "magenta", "darkgrey"));
      axis(2);
      axis(4); # added by Beomsoo
      abline(h=log2(fc$max.thresh), lty=3);
      abline(h=log2(fc$min.thresh), lty=3);
      abline(h=0, lwd=1);
    }else{ # plot side by side
      
      dat1 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 1, ];
      dat2 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 2, ];
      
      mns1 <- colMeans(dat1);
      mn1 <- mean(mns1);
      sd1 <- sd(mns1);
      msd1.top <- mn1 + 2*sd1;
      msd1.low <- mn1 - 2*sd1;
      
      mns2 <- colMeans(dat2);
      mn2 <- mean(mns2);
      sd2 <- sd(mns2);
      msd2.top <- mn2 + 2*sd2;
      msd2.low <- mn2 - 2*sd2;
      
      ylims <- range(c(mns1, mns2, msd1.top, msd2.top, msd1.low, msd2.low));
      new.mns <- c(mns1, rep(NA, 5), mns2);
      cols <- c(rep("magenta", length(mns1)), rep(NA, 5), rep("blue", length(mns2)));
      pchs <- c(rep(15, length(mns1)), rep(NA, 5), rep(19, length(mns2)));
      plot(new.mns, ylim=ylims, pch = pchs, col = cols, cex = 1.25, axes=F, ylab="");
      axis(2);
      axis(4); # added by Beomsoo
      abline(h=mn1, col="magenta", lty=3, lwd=2);
      abline(h=msd1.low, col="magenta", lty=3, lwd=1);
      abline(h=msd1.top, col="magenta", lty=3, lwd=1);
      abline(h=mn2, col="blue", lty=3, lwd=2);
      abline(h=msd2.low, col="blue", lty=3, lwd=1);
      abline(h=msd2.top, col="blue", lty=3, lwd=1);
      # abline(h=mean(all.mns), col="darkgrey", lty=3);
      axis(1, at=1:length(new.mns), labels=c(1:length(mns1),rep(NA, 5),1:length(mns2)));
    }
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Used by higher functions to calculate fold change 
#'@description Utility method to calculate FC, used in higher function
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Logical, true of false
#'@param cmpType Numeric, 0 or 1
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetFC <- function(mSetObj=NA, paired=FALSE, cmpType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(paired){ 
    # compute the average of paired FC (unit is pair)
    row.norm <- qs::qread("row_norm.qs");
    data <- log2(row.norm);
    
    G1 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]
    G2 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]
    
    if(cmpType == 0){
      fc.mat <- G1-G2;
    }else{
      fc.mat <- G2-G1;
    }
    fc.log <- colMeans(fc.mat);
    fc.all <- signif(2^fc.log, 5);
  }else{
    # compute the FC of two group means (unit is group)

      data <- qs::qread("row_norm.qs");
      m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]);
      m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]);
      
      # create a named matrix of sig vars for display
      if(cmpType == 0){
        ratio <- m1/m2;
      }else{
        ratio <- m2/m1;
      }
      fc.all <- signif(ratio, 5);
      ratio[ratio < 0] <- 0;
      fc.log <- signif(log2(ratio), 5);
      fc.log[is.infinite(fc.log) & fc.log < 0] <- -99;
      fc.log[is.infinite(fc.log) & fc.log > 0] <- 99;
    
  }
  names(fc.all) <- names(fc.log) <- colnames(data);  
  return(list(fc.all = fc.all, fc.log = fc.log));
}

#'Perform t-test analysis
#'@description This function is used to perform t-test analysis.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nonpar Logical, use a non-parametric test, T or F. False is default. 
#'@param threshp Numeric, enter the adjusted p-value (FDR) cutoff
#'@param paired Logical, is data paired (T) or not (F).
#'@param equal.var Logical, evaluates if the group variance is equal (T) or not (F).
#'@param pvalType pvalType, can be "fdr" etc.
#'@param all_results Logical, if TRUE, returns T-Test analysis results
#'for all compounds. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Ttests.Anal <- function(mSetObj=NA, nonpar=F, threshp=0.05, paired=FALSE, 
                        equal.var=TRUE, pvalType="fdr", all_results=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web & !nonpar & RequireFastUnivTests(mSetObj)){
    res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls, var.equal=equal.var);
  } else {
    res <- GetTtestRes(mSetObj, paired, equal.var, nonpar);
  }
  
  t.stat <- res[,1];
  p.value <- res[,2];
  names(t.stat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
  
  p.log <- -log10(p.value);
  fdr.p <- p.adjust(p.value, "fdr");
  
  if(all_results==TRUE){
    
    all.mat <- data.frame(signif(t.stat,5), signif(p.value,5), signif(p.log,5), signif(fdr.p,5));
    
    if(nonpar){
      tt.nm = "Wilcoxon Rank Test";  
      file.nm <- "wilcox_rank_all.csv"
      colnames(all.mat) <- c("V", "p.value", "-log10(p)", "FDR");
    }else{
      tt.nm = "T-Tests";
      file.nm <- "t_test_all.csv";
      colnames(all.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    }
    fast.write.csv(all.mat, file=file.nm);
  }
  
  if(pvalType=="fdr"){
    inx.imp <- fdr.p <= threshp;
  }else{
    inx.imp <- p.value <= threshp;
  }
  
  sig.num <- sum(inx.imp, na.rm = TRUE);
  AddMsg(paste("A total of", sig.num, "significant features were found."));
  
  if(sig.num > 0){
    sig.t <- t.stat[inx.imp];
    sig.p <- p.value[inx.imp];
    lod<- -log10(sig.p);
    sig.q <-fdr.p[inx.imp];
    
    sig.mat <- cbind(sig.t, sig.p, lod, sig.q);
    colnames(sig.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    ord.inx <- order(sig.p);
    sig.mat <- sig.mat[ord.inx,,drop=F];
    sig.mat <- signif(sig.mat, 5);
    
    if(nonpar){
      tt.nm = "Wilcoxon Rank Test";  
      file.nm <- "wilcox_rank.csv"
      colnames(sig.mat) <- c("V", "p.value", "-log10(p)", "FDR");
    }else{
      tt.nm = "T-Tests";
      file.nm <- "t_test.csv";
      colnames(sig.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    }
    fast.write.csv(sig.mat, file=file.nm);
    
    tt <- list (
      tt.nm = tt.nm,
      sig.nm = file.nm,
      sig.num = sig.num,
      paired = paired,
      pval.type = pvalType,
      raw.thresh = threshp,
      t.score = t.stat,
      p.value = p.value,
      p.log = p.log,
      inx.imp = inx.imp,
      sig.mat = sig.mat
    );
  }else{
    tt <- list (
      sig.num = sig.num,
      paired = paired,
      pval.type = pvalType,
      raw.thresh = threshp,
      t.score = t.stat,
      p.value = p.value,
      p.log = p.log,
      inx.imp = inx.imp
    );
  }
  
  mSetObj$analSet$tt <- tt;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(as.numeric(sig.num));
  }
  return(.set.mSet(mSetObj));
}

#'Plot t-test 
#'@description Plot t-test
#'@usage PlotTT(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotTT <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, interactive=F){

  library(ggplot2)
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w * 6 / 8;
  
  mSetObj$imgSet$tt <- imgName;
  
  tt_data <- data.frame(
    p_log = mSetObj$analSet$tt$p.log,
    label = names(mSetObj$analSet$tt$p.log),
    seq = seq_along(mSetObj$analSet$tt$p.log),
    Status = factor(ifelse(mSetObj$analSet$tt$inx.imp, "Significant", "Unsignificant"))
  )

  p <- ggplot(tt_data, aes(x=seq, y=p_log, col=Status, label=label)) +
       geom_point() +
       labs(x = GetVariableLabel(mSetObj$dataSet$type), y = "-log10(raw.p)") +
       scale_color_manual(
         values = c("Significant" = "magenta", "Unsignificant" = "darkgrey")
       )

  if (interactive) {
    library(plotly);
    ggp_build <- layout(ggplotly(p, tooltip = c("label", "p_log")), autosize = FALSE, width = 1200, height = 800)
    return(ggp_build);
  } else {
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    p <- p + ggrepel::geom_text_repel(data = subset(tt_data, Status == "Significant"))
    print(p);
    dev.off();
  }

  return(.set.mSet(mSetObj));
}

#'Perform Volcano Analysis
#'@description Perform volcano analysis
#'@usage Volcano.Anal(mSetObj=NA, paired=FALSE, fcthresh, 
#'cmpType, nonpar=F, threshp, equal.var=TRUE, pval.type="raw")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Logical, T if data is paired, F if data is not.
#'@param fcthresh Numeric, input the fold change threshold
#'@param cmpType Comparison type, 0 indicates group 1 vs group 2, and 1 indicates group 2 vs group 1
#'@param nonpar Logical, indicate if a non-parametric test should be used (T or F)
#'@param threshp Numeric, indicate the p-value threshold
#'@param equal.var Logical, indicates if the group variance is equal (T) or unequal (F)
#'@param pval.type To indicate raw p-values, use "raw". To indicate FDR-adjusted p-values, use "fdr".  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Volcano.Anal <- function(mSetObj=NA, paired=FALSE, fcthresh, 
                         cmpType, nonpar=F, threshp, equal.var=TRUE, pval.type="raw"){

  mSetObj <- .get.mSet(mSetObj);
  
  # Note, volcano is based on t-tests and fold change analysis
  #### t-tests and p values. If performed and identical parameters
  mSetObj <- Ttests.Anal(mSetObj, nonpar, threshp, paired, equal.var, pval.type);
  mSetObj <- .get.mSet(mSetObj);
  p.value <- mSetObj$analSet$tt$p.value;
  
  if(pval.type == "fdr"){
    p.value <- p.adjust(p.value, "fdr");
  }   
  
  inx.p <- p.value <= threshp;
  p.log <- -log10(p.value);

  #### fc analysis
  mSetObj <- FC.Anal(mSetObj, fcthresh, cmpType, paired);
  mSetObj <- .get.mSet(mSetObj);
  
  fcthresh = ifelse(fcthresh>1, fcthresh, 1/fcthresh);
  max.xthresh <- log2(fcthresh);
  min.xthresh <- log2(1/fcthresh);

  fc.log <- mSetObj$analSet$fc$fc.log;
  fc.all <- mSetObj$analSet$fc$fc.all;
  
  inx.up <- mSetObj$analSet$fc$inx.up;
  inx.down <- mSetObj$analSet$fc$inx.down;

  # subset inx.p to inx.up/down
  keep.inx <- names(inx.p) %in% names(inx.up)
  inx.p <- inx.p[keep.inx]
  p.log <- p.log[keep.inx]
  
  # create named sig table for display
  inx.imp <- (inx.up | inx.down) & inx.p;
  sig.var <- cbind(fc.all[inx.imp,drop=F], fc.log[inx.imp,drop=F], p.value[inx.imp,drop=F], p.log[inx.imp,drop=F]);
  
  if(pval.type == "fdr"){
    colnames(sig.var) <- c("FC", "log2(FC)", "p.ajusted", "-log10(p)");
  }else{
    colnames(sig.var) <- c("FC", "log2(FC)", "raw.pval", "-log10(p)");
  }
  
  # first order by log(p), then by log(FC)
  ord.inx <- order(sig.var[,4], abs(sig.var[,2]), decreasing=T);
  sig.var <- sig.var[ord.inx,,drop=F];
  sig.var <- signif(sig.var,5);
  
  fileName <- "volcano.csv";
  fast.write.csv(signif(sig.var,5), file=fileName);
  
  volcano <- list (
    pval.type = pval.type,
    raw.threshx = fcthresh,
    raw.threshy = threshp,
    paired = paired,
    max.xthresh = max.xthresh,
    min.xthresh = min.xthresh,
    thresh.y = -log10(threshp),
    fc.all = fc.all,
    fc.log = fc.log,
    inx.up = inx.up,
    inx.down = inx.down,
    p.log = p.log,
    inx.p = inx.p,
    sig.mat = sig.var
  );
  mSetObj$analSet$volcano <- volcano;
  return(.set.mSet(mSetObj));
}

#'Create volcano plot
#'@description For labelling interesting points, it is defined by the following rules:
#'need to be signficant (sig.inx) and or 2. top 5 p, or 2. top 5 left, or 3. top 5 right. 
#'@usage PlotVolcano(mSetObj=NA, imgName, plotLbl, plotTheme, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param plotLbl Logical, plot labels, 1 for yes and 0 for no.
#'@param plotTheme plotTheme, numeric, canbe 0, 1 or 2
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

# using ggplot
PlotVolcano <- function(mSetObj=NA, imgName, plotLbl, plotTheme, format="png", dpi=72, width=NA, interactive=F){

  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*6/10;
  
   mSetObj$imgSet$volcano <- imgName;

   vcn <- mSetObj$analSet$volcano;
   imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;

   de <- data.frame(cbind(vcn$fc.log, vcn$p.log));
   de$Status <- "Non-SIG";
   de$Status[vcn$inx.p & vcn$inx.up] <- "UP";
   de$Status[vcn$inx.p & vcn$inx.down] <- "DOWN";
   de$Status <- as.factor(de$Status);

   mycols <- levels(de$Status);
   mycols[mycols=="UP"] <- "firebrick";
   mycols[mycols=="DOWN"] <- "cornflowerblue";
   mycols[mycols=="Non-SIG"] <- "grey";
   
   de$label <- NA
   if(interactive){
   de$label <- rownames(de);
   }else{
   de$label[imp.inx] <- rownames(de)[imp.inx];
   }
   require(ggplot2);

   p <- ggplot(data=de, aes(x=de[,1], y=de[,2], col=Status, label=label)) +
        scale_color_manual(values=mycols) +
        geom_vline(xintercept=c(vcn$min.xthresh, vcn$max.xthresh), linetype="dashed", color="black") +
        geom_hline(yintercept=vcn$thresh.y, linetype="dashed", color="black") +
        geom_point() + 
        labs(x ="log2(FC)", y = "-log10(p)");

   if(plotLbl){
    p <- p +  ggrepel::geom_text_repel();
   }
    mSetObj$analSet$volcano.plot.config <- list(plotLbl=plotLbl, plotTheme=plotTheme);

   if(!interactive){
    if(plotTheme == 0){
       p <- p + theme_bw();
    }else if(plotTheme == 1){
       p <- p + theme_grey();
    }else if(plotTheme == 2){
       p <- p + theme_minimal();
    }else{
       p <- p + theme_classic();
    }
   }

  if(interactive){
    library(plotly);
    ggp_build <- layout(ggplotly(p, tooltip = c("label")), autosize = FALSE, width = 1200, height = 800, margin = mSetObj$imgSet$margin.config)
    return(ggp_build);
  } else {
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    print(p)
    dev.off();
  }

   return(.set.mSet(mSetObj));
}

#'ANOVA
#'@description Perform anova and only return p values and MSres (for Fisher's LSD)
#'@param x Input the data to perform ANOVA
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
aof <- function(x, cls) {
  aov(x ~ cls);
}


#'Kruskal-Wallis
#'@description Perform  Kruskal-Wallis Test
#'@param x Input data to perform Kruskal-Wallis
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
kwtest <- function(x, cls) {
  kruskal.test(x ~ cls);
}

#'Fisher for ANOVA
#'@description Perform  Fisher LSD for ANOVA, used in higher function 
#'@param aov.obj Input the anova object
#'@param thresh Numeric, input the alpha threshold 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
FisherLSD <- function(aov.obj, thresh){
    if(!exists("my.lsd.test")){ # public web on same user dir
      .load.scripts.on.demand("util_lsd.Rc");    
    }
    return(my.lsd.test(aov.obj,"cls", alpha=thresh));
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@param tukey Input tukey output
#'@param cut.off Input numeric cut-off
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseTukey <- function(tukey, cut.off){
  inx <- tukey$cls[,"p adj"] <= cut.off;
  paste(rownames(tukey$cls)[inx], collapse="; ");
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@param fisher Input fisher object 
#'@param cut.off Numeric, set cut-off
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseFisher <- function(fisher, cut.off){
  inx <- fisher[,"pvalue"] <= cut.off;
  paste(rownames(fisher)[inx], collapse="; ");
}

#' Perform ANOVA analysis
#' @description ANOVA analysis
#' @usage ANOVA.Anal(mSetObj=NA, nonpar=FALSE, thresh=0.05, all_results=FALSE)
#' @param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#' @param nonpar Logical, use a non-parametric test (T) or not (F)
#' @param thresh Numeric, from 0 to 1, indicate the p-value threshold
#' @param all_results Logical, if TRUE, it will output the ANOVA results for all compounds 
#' @author Jeff Xia\email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#'
ANOVA.Anal<-function(mSetObj=NA, nonpar=FALSE, thresh=0.05, all_results=FALSE) {
  
   mSetObj <- .get.mSet(mSetObj);
   if(!nonpar){
      aov.nm <- "One-way ANOVA";
    }else{
      aov.nm <- "Kruskal Wallis Test";
    }

    sig.num <- 0;
    aov.res <- sig.mat <- NULL;

    data <- as.matrix(mSetObj$dataSet$norm);
    cls <- mSetObj$dataSet$cls;
    if(.on.public.web & !nonpar & RequireFastUnivTests(mSetObj)){
        res <- PerformFastUnivTests(data, cls);
    } else {
        my.res <- GetFtestRes(mSetObj, nonpar);
        aov.res <- my.res$aov.res;
        res <- my.res$f.res;
    }

    fstat <- res[,1];
    p.value <- res[,2];
    names(fstat) <- names(p.value) <- colnames(data);
    fdr.p <- p.adjust(p.value, "fdr");
    
    if(all_results==TRUE){
        all.mat <- data.frame(signif(p.value,5), signif(-log10(p.value),5), signif(fdr.p,5));
        rownames(all.mat) <- names(p.value);
        colnames(all.mat) <- c("p.value", "-log10(p)", "FDR");
        fast.write.csv(all.mat, "anova_all_results.csv")
    }
    
    inx.imp <- fdr.p <= thresh;
    sig.num <- sum(inx.imp, na.rm = TRUE);
    AddMsg(paste(c("A total of", sig.num, "significant features were found."), collapse=" "));

    res <- 0;
    sig.f <- sig.p <- sig.fdr <- 1;

    if(sig.num > 0){
        res <- 1;
        sig.f <- fstat[inx.imp];
        sig.p <- p.value[inx.imp];
        sig.fdr <- fdr.p[inx.imp];
        if(exists("aov.res")){
            qs::qsave(aov.res[inx.imp], file="aov_res_imp.qs");
        }
    }

    aov<-list (
            aov.nm = aov.nm,
            nonpar = nonpar,
            sig.num = sig.num,
            raw.thresh = thresh,
            thresh = -log10(thresh), # only used for plot threshold line
            p.value = p.value,
            p.log = -log10(p.value),
            fdr.p = fdr.p,
            inx.imp = inx.imp,
            sig.f = sig.f,
            sig.p = sig.p,
            sig.fdr = sig.fdr
        );

    mSetObj$analSet$aov <- aov;
  
    if(.on.public.web){
        .set.mSet(mSetObj);
        return(res);
    }else{
        return(.set.mSet(mSetObj));
    }
}

# Do posthoc tests on significant features from ANOVA tests
Calculate.ANOVA.posthoc <- function(mSetObj=NA, post.hoc="fisher", thresh=0.05){
    
    mSetObj <- .get.mSet(mSetObj);
    sig.num <- mSetObj$analSet$aov$sig.num;
    inx.imp <- mSetObj$analSet$aov$inx.imp;
    sig.f <- mSetObj$analSet$aov$sig.f;
    sig.p <- mSetObj$analSet$aov$sig.p;
    sig.fdr <- mSetObj$analSet$aov$sig.fdr;
    nonpar <- mSetObj$analSet$aov$nonpar;
    cmp.res <- NULL;
    post.nm <- NULL;

    if(nonpar){
        sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(sig.fdr,5), 'NA');
        colnames(sig.mat) <- c("chi.squared", "p.value", "-log10(p)", "FDR", "Post-Hoc");
        fileName <- "kw_posthoc.csv";
    }else{     
        fileName <- "anova_posthoc.csv";    
 
        # do post-hoc only for signficant entries
        # note aov obj is not avaible using fast version
        # need to recompute using slower version for the sig ones
        if(.on.public.web & RequireFastUnivTests(mSetObj)){
            data <- as.matrix(mSetObj$dataSet$norm);
            cls <- mSetObj$dataSet$cls;
            aov.imp <- apply(data[,inx.imp,drop=FALSE], 2, aof, cls);
        }else{
            aov.imp <- qs::qread("aov_res_imp.qs");
        }

        # note this is only for post-hoc analysis. max 1000 in case too large
        if(sig.num > 1000){
            # update inx.imp   
            my.ranks <- rank(sig.p);
            inx.imp <- my.ranks <= 1000;
            aov.imp <- aov.imp[inx.imp];
        }

        if(post.hoc=="tukey"){
            tukey.res<-lapply(aov.imp, TukeyHSD, conf.level=1-thresh);
            my.cmp.res <- unlist(lapply(tukey.res, parseTukey, cut.off=thresh));
            post.nm = "Tukey's HSD";
        }else{
            fisher.res<-lapply(aov.imp, FisherLSD, thresh);
            my.cmp.res <- unlist(lapply(fisher.res, parseFisher, cut.off=thresh));
            post.nm = "Fisher's LSD";
        }
        
        cmp.res <- my.cmp.res;
        # post hoc only top 1000;

        if(sig.num > 1000){
            cmp.res <- rep(NA, sig.num); 
            cmp.res[inx.imp] <- my.cmp.res;
            post.nm <- paste(post.nm, "(top 1000)");
        }
        # create the result dataframe,
        # note, the last column is string, not double
        sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(sig.fdr,5), cmp.res);
        colnames(sig.mat) <- c("f.value", "p.value", "-log10(p)", "FDR", post.nm);
    }

    rownames(sig.mat) <- names(sig.p);
    # order the result simultaneously
    ord.inx <- order(sig.p, decreasing = FALSE);
    sig.mat <- sig.mat[ord.inx,,drop=F];

    # note only display top 1000 max for web (save all to the file)
    fast.write.csv(sig.mat,file=fileName);
    if(sig.num > 1000){
        sig.mat <- sig.mat[1:1000,];
    }

    aov <- mSetObj$analSet$aov; 
    # add to the list, don't use append, as it does not overwrite
    aov$sig.nm <- fileName;
    aov$post.hoc <- post.hoc;
    aov$sig.mat <- sig.mat;

    mSetObj$analSet$aov <- aov;
    return(.set.mSet(mSetObj));  
}

#'Plot ANOVA 
#'@description Plot ANOVA 
#'@usage PlotANOVA(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotANOVA <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  lod <- mSetObj$analSet$aov$p.log;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/9;
  
  mSetObj$imgSet$anova <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(lod, ylab="-log10(raw.p)", xlab = GetVariableLabel(mSetObj$dataSet$type), main=mSetObj$analSet$aov$aov.nm, type="n");
  red.inx <- which(mSetObj$analSet$aov$inx.imp);
  blue.inx <- which(!mSetObj$analSet$aov$inx.imp);
  points(red.inx, lod[red.inx], bg="red", cex=1.2, pch=21);
  points(blue.inx, lod[blue.inx], bg="green", pch=21);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot Compound View 
#'@description Plots a bar-graph of selected compound over groups 
#'@usage PlotCmpdView(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpdNm Input a name for the compound 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotCmpdView <- function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  imgName <- mSetObj$dataSet$url.var.nms[cmpdNm];
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
  
  my.width <- 200;
  adj.width <- 90*length(levels(cls))+20;
  if(adj.width > my.width){
    my.width <- adj.width;
  }
  
  x <- mSetObj$dataSet$norm[, cmpdNm]
  y <- cls
  df <- data.frame(conc = x, class = y)
  col <- unique(GetColorSchema(y))
  
  Cairo::Cairo(file = imgName, dpi=dpi, width=my.width, height=325, type=format, bg="transparent");
  
  p <- ggplot2::ggplot(df, aes(x=class, y=conc, fill=class)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  p <- p + stat_summary(fun.y=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
  p <- p + theme(text = element_text(size=15), plot.margin = margin(t=0.20, r=0.25, b=0.55, l=0.25, "cm"))
  p <- p + scale_fill_manual(values=col) + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=45, hjust=1))
  p <- p + theme(plot.title = element_text(size = 13, hjust=0.5, face="bold"), axis.text = element_text(size=10))
  print(p)
  dev.off()
  return(imgName);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Sig Table for Fold-Change Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.FC <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$fc$sig.mat, "fold change analysis", mSetObj$dataSet$type);
}

GetFCSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$fc$sig.mat));
}

GetFCSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$fc$sig.mat);
}

GetFcSigUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$fc$fc.log;
  red.inx<- which(mSetObj$analSet$fc$inx.up);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetFcSigUpIDs  <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<- which(mSetObj$analSet$fc$inx.up);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$fc$fc.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetFcSigDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$fc$fc.log;
  blue.inx<- which(mSetObj$analSet$fc$inx.down);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetFcSigDnIDs  <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<- which(mSetObj$analSet$fc$inx.down);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$fc$fc.log)[blue.inx]);
  }else{
    return("NA");
  }
}

GetFcUnsigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$fc$fc.log;
  inx.imp <- mSetObj$analSet$fc$inx.up | mSetObj$analSet$fc$inx.down;
  blue.inx<- which(!inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetFcUnsigIDs  <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$fc$fc.log;
  inx.imp <- mSetObj$analSet$fc$inx.up | mSetObj$analSet$fc$inx.down;
  blue.inx<- which(!inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$fc$fc.log)[blue.inx]);
  }else{
    return("NA");
  }
}

GetFCSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$fc$sig.mat);
}

GetAovSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(as.matrix(mSetObj$analSet$aov$sig.mat[, 1:4])));
}

GetAovSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$aov$sig.mat);
}

GetAovSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$aov$sig.mat[, 1:4]);
}

GetAovPostHocSig <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov$sig.mat[,5];
}

#'Sig Table for Anova
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Anova <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$aov$sig.mat, "One-way ANOVA and post-hoc analysis", mSetObj$dataSet$type);
}

GetAnovaUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  red.inx<- which(mSetObj$analSet$aov$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAovUpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<- which(mSetObj$analSet$aov$inx.imp);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$aov$p.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetAnovaDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  blue.inx <- which(!mSetObj$analSet$aov$inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAovDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<- which(!mSetObj$analSet$aov$inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$aov$p.log)[blue.inx]);
  }else{
    return("NA");
  }
}

GetAnovaCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$aov$p.log);
}

GetAnovaSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov$sig.nm;
}

#'Sig Table for T-test Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.TT <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$tt$sig.mat, "t-tests", mSetObj$dataSet$type);
}

#'T-test matrix
#'@description Return a double matrix with 2 columns - p values and lod
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetTTSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$tt$sig.mat));
}

GetTTSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$tt$sig.mat);
}

GetTTSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$tt$sig.mat);
}

GetTtUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$tt$p.log;
  red.inx<- which(mSetObj$analSet$tt$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetTtUpIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  red.inx<-which(mSetObj$analSet$tt$inx.imp);
  if(sum(red.inx) > 0){
    return(names(mSetObj$analSet$tt$p.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetTtDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$tt$p.log;
  blue.inx <- which(!mSetObj$analSet$tt$inx.imp);
  
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetTtDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  blue.inx<-which(!mSetObj$analSet$tt$inx.imp);
  if(sum(blue.inx) > 0){
    return(names(mSetObj$analSet$tt$p.log)[blue.inx]);
  }else{
    return("NA");
  }
}

GetTtCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$tt$p.log);
}

GetTtestSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$tt$sig.nm;
}

#'Retrieve T-test p-values
#'@description Utility method to get p values
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Default set to FALSE
#'@param equal.var Default set to TRUE
#'@param nonpar Use non-parametric tests, default is set to FALSE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetTtestRes <- function(mSetObj=NA, paired=FALSE, equal.var=TRUE, nonpar=F){
  
  mSetObj <- .get.mSet(mSetObj);  
  inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
  inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
  if(length(inx1) ==1 || length(inx2) == 1){
    equal.var <- TRUE; # overwrite use option if one does not have enough replicates
  }
    data <- as.matrix(mSetObj$dataSet$norm);
    if(!exists("mem.tt")){
        require("memoise");
        mem.tt <<- memoise(.get.ttest.res);
    }
    return(mem.tt(data, inx1, inx2, paired, equal.var, nonpar));
}

.get.ttest.res <- function(data, inx1, inx2, paired=FALSE, equal.var=TRUE, nonpar=F){

   print("Performing regular t-tests ....");
   univ.test <- function(x){t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var)};
   if(nonpar){
     univ.test <- function(x){wilcox.test(x[inx1], x[inx2], paired = paired)};
   }
   my.fun <- function(x) {
     tmp <- try(univ.test(x));
     if(class(tmp) == "try-error") {
       return(c(NA, NA));
     }else{
       return(c(tmp$statistic, tmp$p.value));
     }
   }
   res <- apply(data, 2, my.fun);
   return(t(res));
}

GetFtestRes <- function(mSetObj=NA, nonpar=F){
  
    if(!exists("mem.aov")){
        require("memoise");
        mem.aov <<- memoise(.get.ftest.res);
    }

    mSetObj <- .get.mSet(mSetObj);  
    data <- as.matrix(mSetObj$dataSet$norm);
    cls <- mSetObj$dataSet$cls;
    print("using cache .......");
    return(mem.aov(data, cls, nonpar));
}

.get.ftest.res <- function(data, cls, nonpar){

    print("Performing regular ANOVA F-tests ....");

    aov.res <- my.res <- NULL;
    if(!nonpar){
        aov.res <- apply(data, 2, aof, cls);
        anova.res <- lapply(aov.res, anova);
        my.res <- unlist(lapply(anova.res, function(x) { c(x["F value"][1,], x["Pr(>F)"][1,])}));        
    }else{
        anova.res <- apply(data, 2, kwtest, cls);
        my.res <- unlist(lapply(anova.res, function(x) {c(x$statistic, x$p.value)}));
    }
    my.res <- list(
                    aov.res = aov.res,
                    f.res = data.frame(matrix(my.res, nrow=ncol(data), byrow=T), stringsAsFactors=FALSE)
                );
    return(my.res);
}

#'Utility method to perform the univariate analysis automatically
#'@description The approach is computationally expensive,and fails more often 
#'get around: make it lazy unless users request, otherwise the default t-test will also be affected
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetUnivReport <- function(mSetObj=NA){
  if(.on.public.web){
    # make this lazy load
    if(!exists("my.univ.report")){ # public web on same user dir
      .load.scripts.on.demand("util_univreport.Rc");    
    }
    return(my.univ.report(mSetObj));
  }else{
    return(my.univ.report(mSetObj));
  }
}

ContainInfiniteTT<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(sum(!is.finite(mSetObj$analSet$tt$sig.mat))>0){
    return("true");
  }
  return("false");
}

GetVolcanoDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  blue.inx <- which(!imp.inx);
  
  if(sum(blue.inx)>0){
    xs <- vcn$fc.log[blue.inx]
    ys <- vcn$p.log[blue.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpLftMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- vcn$inx.down & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpRgtMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- vcn$inx.up & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpLftIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- vcn$inx.down & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    return(names(mSetObj$analSet$volcano$fc.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetVolcanoUpRgtIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- vcn$inx.up & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    return(names(mSetObj$analSet$volcano$fc.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetVolcanoDnIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  blue.inx <- which(!imp.inx);
  if(sum(blue.inx)>0){
    return(names(mSetObj$analSet$volcano$fc.log)[blue.inx]);
  }else{
    return("NA");
  }
}

GetVolcanoCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$volcano$fc.log);
}

#'Volcano indices
#'@description Get indices of top n largest/smallest number
#'@param vec Vector containing volcano indices
#'@param n Numeric
#'@param dec Logical, default set to TRUE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetTopInx <- function(vec, n, dec=T){
  inx <- order(vec, decreasing = dec)[1:n];
  # convert to T/F vec
  vec<-rep(F, length=length(vec));
  vec[inx] <- T;
  return (vec);
}

#'Sig table for Volcano Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Volcano <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$volcano$sig.mat, "volcano plot", mSetObj$dataSet$type);
}

GetVolcanoSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$volcano$sig.mat));
}

GetVolcanoSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$analSet$volcano$sig.mat));
}

GetVolcanoSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$analSet$volcano$sig.mat));
}

ContainInfiniteVolcano <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(sum(!is.finite(mSetObj$analSet$volcano$sig.mat))>0){
    return("true");
  }
  return("false");
}

GetAovSigNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$aov$sig.num);
}
