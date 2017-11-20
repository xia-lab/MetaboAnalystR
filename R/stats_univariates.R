#'Fold change analysis, unpaired
#'@description Perform fold change analysis, method can be mean or median
#'@usage FC.Anal.unpaired(mSetObj, fc.thresh=2, cmp.type = 0)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fc.thresh Fold-change threshold, numeric input
#'@param cmp.type Comparison type, 0 for group 1 minus group 2, and 1 for group 
#'1 minus group 2
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
FC.Anal.unpaired <- function(mSetObj=NA, fc.thresh=2, cmp.type = 0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make sure threshold is above 1
  fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
  max.thresh = fc.thresh;
  min.thresh = 1/fc.thresh;
  
  res <-GetFC(mSetObj, F, cmp.type);
  fc.all <- res$fc.all;
  fc.log <- res$fc.log;
  
  imp.inx <- fc.all > max.thresh | fc.all < min.thresh;
  sig.mat <- cbind(fc.all[imp.inx, drop=F], fc.log[imp.inx, drop=F]);
  colnames(sig.mat)<-c("Fold Change", "log2(FC)");
  
  # order by absolute log value (since symmetrical in pos and neg)
  inx.ord <- order(abs(sig.mat[,2]), decreasing=T);
  sig.mat <- sig.mat[inx.ord,,drop=F];
  
  fileName <- "fold_change.csv";
  write.csv(sig.mat,file=fileName);
  
  # create a list object to store fc
  mSetObj$analSet$fc<-list (
    paired = FALSE,
    raw.thresh = fc.thresh,
    max.thresh = max.thresh,
    min.thresh = min.thresh,
    fc.all = fc.all, # note a vector
    fc.log = fc.log,
    inx.imp = imp.inx,
    sig.mat = sig.mat
  );
  return(.set.mSet(mSetObj));
}

#'Fold change analysis, paired
#'@description Perform paired fold change analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fc.thresh Fold-change threshold, numeric input
#'@param percent.thresh Numeric input, from 0 to 1 to indicate the significant count threshold
#'@param cmp.type Comparison type, 0 for group 1 minus group 2, and 1 for group 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
FC.Anal.paired <- function(mSetObj=NA, fc.thresh=2, percent.thresh=0.75, cmp.type=0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make sure threshold is above 1
  fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
  max.thresh = fc.thresh;
  min.thresh = 1/fc.thresh;
  
  fc.mat <-GetFC(mSetObj, T, cmp.type);
  
  count.thresh<-round(nrow(mSetObj$dataSet$norm)/2*percent.thresh);
  mat.up <- fc.mat >= log(max.thresh,2);
  mat.down <- fc.mat <= log(min.thresh,2);
  
  count.up<-apply(mat.up, 2, sum);
  count.down<-apply(mat.down, 2, sum);
  fc.all<-rbind(count.up, count.down);
  
  inx.up <- count.up>=count.thresh;
  inx.down <- count.down>=count.thresh;
  
  colnames(fc.all)<-colnames(mSetObj$dataSet$norm);
  rownames(fc.all)<-c("Count (up)", "Count (down)");
  sig.var <- t(fc.all[,(inx.up|inx.down), drop=F]);
  
  # sort sig.var using absolute difference between count(up)-count(down)
  sig.dff<-abs(sig.var[,1]-sig.var[,2])
  inx<-order(sig.dff, decreasing=T);
  sig.var<-sig.var[inx,,drop=F];
  
  fileName <- "fold_change.csv";
  write.csv(signif(sig.var,5),file=fileName);
  
  # create a list object to store fc
  mSetObj$analSet$fc <-list (
    paired = TRUE,
    fc.mat = fc.mat,
    raw.thresh = fc.thresh,
    max.thresh = count.thresh,
    min.thresh = -count.thresh,
    fc.all = fc.all, # note: a 2-row matrix!
    inx.up = inx.up,
    inx.down = inx.down,
    sig.mat = sig.var
  );
  return(.set.mSet(mSetObj));
}

#'Plot fold change 
#'@description Plot fold change analysis
#'@usage PlotFC(imgName, format, dpi, width)
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
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(5,5,2,3));
  
  fc = mSetObj$analSet$fc;
  if(fc$paired){
    ylim <- c(-nrow(mSetObj$dataSet$norm)/2, nrow(mSetObj$dataSet$norm)/2);
    xlim <- c(0, ncol(mSetObj$dataSet$norm));
    plot(NULL, xlim=xlim, ylim=ylim, xlab = GetVariableLabel(mSetObj),
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
      plot(fc$fc.log,  ylab="Log2 (FC)", ylim = ylim, xlab = GetVariableLabel(mSetObj), pch=19, axes=F,
           col= ifelse(fc$inx.imp, "magenta", "darkgrey"));
      axis(2);
      axis(4); # added by Beomsoo
      abline(h=log(fc$max.thresh,2), lty=3);
      abline(h=log(fc$min.thresh,2), lty=3);
      abline(h=0, lwd=1);
    }else{ # plot side by side
      
      dat1 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 1, ];
      dat2 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 2, ];
      
      mns1 <- apply(dat1, 2, mean);
      mn1 <- mean(mns1);
      sd1 <- sd(mns1);
      msd1.top <- mn1 + 2*sd1;
      msd1.low <- mn1 - 2*sd1;
      
      mns2 <- apply(dat2, 2, mean);
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
    if(mSetObj$dataSet$combined.method){
      data <- mSetObj$dataSet$norm;
    }else{
      data <- log(mSetObj$dataSet$row.norm,2);
    }
    
    G1 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]
    G2 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]
    
    if(cmpType == 0){
      fc.mat <- G1-G2;
    }else{
      fc.mat <- G2-G1;
    }
    return (fc.mat);
  }else{
    if(mSetObj$dataSet$combined.method){
      data <- mSetObj$dataSet$norm;
      m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]);
      m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]);
      
      # create a named matrix of sig vars for display
      if(cmpType == 0){
        fc.log <- signif (m1-m2, 5);
      }else{
        fc.log <- signif (m2-m1, 5);
      }
      fc.all <- signif(2^fc.log, 5);
    }else{
      data <- mSetObj$dataSet$row.norm;
      m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]);
      m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]);
      
      # create a named matrix of sig vars for display
      if(cmpType == 0){
        ratio <- m1/m2;
      }else{
        ratio <- m2/m1;
      }
      fc.all <- signif(ratio, 5);
      fc.log <- signif(log2(ratio), 5);
    }
    
    if(mSetObj$dataSet$combined.method){
      names(fc.all) <- names(fc.log) <- colnames(mSetObj$dataSet$norm);  
    }else{
      names(fc.all) <- names(fc.log) <- colnames(mSetObj$dataSet$row.norm) # make even vectors
    }
    
    return(list(fc.all = fc.all, fc.log = fc.log));
  }
}

#'Perform t-test analysis
#'@description This function is used to perform t-test analysis.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nonpar Logical, use a non-parametric test, T or F. False is default. 
#'@param threshp Numeric, enter the adjusted p-value (FDR) cutoff
#'@param paired Logical, is data paired (T) or not (F).
#'@param equal.var Logical, evaluates if the group variance is equal (T) or not (F). 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Ttests.Anal <- function(mSetObj=NA, nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE){
  
  mSetObj <- .get.mSet(mSetObj);
  res <- GetTtestRes(mSetObj, paired, equal.var, nonpar);
  t.stat <- res[,1];
  p.value <- res[,2];
  
  names(t.stat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
  
  p.log <- -log10(p.value);
  fdr.p <- p.adjust(p.value, "fdr");
  
  inx.imp <- fdr.p <= threshp;
  # if there is no sig cmpds, it will be errors, need to improve
  msg <- NULL;
  
  mSetObj$msgSet$current.msg <- paste(c(msg, "A total of", sum(inx.imp), "significant features were found."), collapse=" ");
  sig.num <- sum(inx.imp);
  
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
    write.csv(sig.mat, file=file.nm);
    
    tt <- list (
      tt.nm = tt.nm,
      sig.nm = file.nm,
      sig.num = sig.num,
      paired = paired,
      raw.thresh = threshp,
      p.value = sort(p.value),
      p.log = p.log,
      thresh = -log10(threshp), # only used for plot threshold line
      inx.imp = inx.imp,
      sig.mat = sig.mat
    );
  }else{
    tt <- list (
      sig.num = sig.num,
      paired = paired,
      raw.thresh = threshp,
      p.value = sort(p.value),
      p.log = p.log,
      thresh = -log10(threshp), # only used for plot threshold line
      inx.imp = inx.imp
    );
  }
  
  mSetObj$analSet$tt <- tt;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(sig.num);
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
PlotTT <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
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
  
  mSetObj$imgSet$tt <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mSetObj$analSet$tt$p.log, ylab="-log10(p)", xlab=GetVariableLabel(mSetObj), main=mSetObj$analSet$tt$tt.nm, pch=19,
       col= ifelse(mSetObj$analSet$tt$inx.imp, "magenta", "darkgrey"));
  abline (h=mSetObj$analSet$tt$thresh, lty=3);
  axis(4); 
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Perform Volcano Analysis
#'@description Perform volcano analysis
#'@usage Volcano.Anal(mSetObj=NA, paired=FALSE, fcthresh, cmpType, percent.thresh, nonpar=F, threshp, equal.var=TRUE, pval.type="raw")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Logical, T if data is paired, F if data is not.
#'@param fcthresh Numeric, input the fold change threshold
#'@param cmpType Comparison type, 1 indicates group 1 vs group 2, and 2 indicates group 2 vs group 1
#'@param percent.thresh Only for paired data, numeric, indicate the significant count threshold 
#'@param nonpar Logical, indicate if a non-parametric test should be used (T or F)
#'@param threshp Numeric, indicate the p-value threshold
#'@param equal.var Logical, indicates if the group variance is equal (T) or unequal (F)
#'@param pval.type To indicate raw p-values, use "raw". To indicate FDR-adjusted p-values, use "fdr".  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Volcano.Anal <- function(mSetObj=NA, paired=FALSE, fcthresh, cmpType, percent.thresh, nonpar=F, threshp, equal.var=TRUE, pval.type="raw"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  #### t-tests
  t.res <- GetTtestRes(mSetObj, paired, equal.var, nonpar);
  p.value <- t.res[,2];
  if(pval.type == "fdr"){
    p.value <- p.adjust(p.value, "fdr");
  }   
  inx.p <- p.value <= threshp;
  p.log <- -log10(p.value);
  
  ### fold change analysis
  # make sure threshold is above 1
  fcthresh = ifelse(fcthresh>1, fcthresh, 1/fcthresh);
  max.xthresh <- log(fcthresh,2);
  min.xthresh <- log(1/fcthresh,2);
  
  res <- GetFC(mSetObj, F, cmpType);
  
  # create a named matrix of sig vars for display
  fc.log <- res$fc.log;
  fc.all <- res$fc.all;
  
  inx.up <- fc.log > max.xthresh;
  inx.down <- fc.log < min.xthresh;
  
  if(paired){
    res <- GetFC(mSetObj, T, cmpType);
    count.thresh<-round(nrow(mSetObj$dataSet$norm)/2*percent.thresh);
    mat.up <- res >= max.xthresh;
    mat.down <- res <= min.xthresh;
    
    count.up <- apply(mat.up, 2, sum);
    count.down <- apply(mat.down, 2, sum);
    fc.all <- rbind(count.up, count.down);
    
    inx.up <- count.up>=count.thresh;
    inx.down <- count.down>=count.thresh;
    
    colnames(fc.all) <- colnames(mSetObj$dataSet$norm);
    rownames(fc.all) <- c("Count (up)", "Count (down)");
    
    # replace the count.thresh for plot
    max.xthresh <- count.thresh;
    min.xthresh <- -count.thresh;
  }
  
  # create named sig table for display
  inx.imp <- (inx.up | inx.down) & inx.p;
  if(paired){ 
    sig.var <- cbind(fc.all[1,][inx.imp,drop=F], fc.all[2,][inx.imp, drop=F], p.value[inx.imp, drop=F], p.log[inx.imp, drop=F]);
    if(pval.type == "fdr"){
      colnames(sig.var)<-c("Counts (up)","Counts (down)", "p.adjusted", "-log10(p)");
    }else{
      colnames(sig.var)<-c("Counts (up)","Counts (down)", "raw.pval", "-log10(p)");
    }
    # first order by count difference, then by log(p)
    dif.count<-abs(sig.var[,1]-sig.var[,2]);
    ord.inx<-order(dif.count, sig.var[,4], decreasing=T);
    sig.var<-sig.var[ord.inx,,drop=F];
    sig.var[,c(3,4)]<-signif(sig.var[,c(3,4)],5);
  }else{
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
  }
  
  fileName <- "volcano.csv";
  write.csv(signif(sig.var,5), file=fileName);
  volcano <- list (
    raw.threshx = fcthresh,
    raw.threshy = threshp,
    paired = paired,
    max.xthresh = max.xthresh,
    min.xthresh = min.xthresh,
    thresh.y = -log10(threshp),
    fc.all = fc.all,
    fc.log = fc.log,
    fc.log.uniq = jitter(fc.log),
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
#'@usage PlotVolcano(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
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
PlotVolcano <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
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
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,4));
  vcn<-mSetObj$analSet$volcano;
  MyGray <- rgb(t(col2rgb("black")), alpha=40, maxColorValue=255);
  MyHighlight <- rgb(t(col2rgb("magenta")), alpha=80, maxColorValue=255);
  if(vcn$paired){
    xlim<-c(-nrow(mSetObj$dataSet$norm)/2, nrow(mSetObj$dataSet$norm)/2)*1.2;
    
    # merge fc.all two rows into one, bigger one win
    fc.all <- apply(vcn$fc.all, 2, function(x){ if(x[1] > x[2]){return(x[1])}else{return(-x[2])}})
    
    hit.inx <- vcn$inx.p & (vcn$inx.up | vcn$inx.down);
    plot(fc.all, vcn$p.log, xlim=xlim, pch=20, cex=ifelse(hit.inx, 1.2, 0.8),
         col = ifelse(hit.inx, MyHighlight, MyGray),
         xlab="Count of Significant Pairs", ylab="-log10(p)");
    
    sig.upInx <- vcn$inx.p & vcn$inx.up;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.up;
    fc.rtInx <- GetTopInx(vcn$fc.all[1,], 5, T);
    lblInx <- p.topInx & sig.upInx & fc.rtInx;
    if(sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.all[1,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
    }
    
    sig.dnInx <- vcn$inx.p & vcn$inx.down;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.down;
    fc.leftInx <- GetTopInx(vcn$fc.all[2,], 5, T) & vcn$inx.down;
    lblInx <-p.topInx & sig.dnInx & fc.leftInx;
    if(sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(-vcn$fc.all[2,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
    }
    
  }else{
    imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
    plot(vcn$fc.log, vcn$p.log, pch=20, cex=ifelse(imp.inx, 1.2, 0.7),
         col = ifelse(imp.inx, MyHighlight, MyGray),
         xlab="log2 (FC)", ylab="-log10(p)");
    
    sig.inx <- imp.inx;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.down);
    fc.leftInx <- GetTopInx(vcn$fc.log, 5, F);
    lblInx <-  sig.inx & (p.topInx | fc.leftInx);
    if(sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
    }
    
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.up);
    fc.rtInx <- GetTopInx(vcn$fc.log, 5, T);
    lblInx <- sig.inx & (p.topInx | fc.rtInx);
    if(sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
    }
  }
  
  abline (v = vcn$max.xthresh, lty=3);
  abline (v = vcn$min.xthresh, lty=3);
  abline (h = vcn$thresh.y, lty=3);
  axis(4); # added by Beomsoo
  dev.off();
  return(.set.mSet(mSetObj));
}

#'ANOVA
#'@description Perform anova and only return p values and MSres (for Fisher's LSD)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
aof <- function(x, cls) {
  aov(x ~ cls);
}


#'Kruskal-Wallis
#'@description Perform  Kruskal Wallis Test
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
kwtest <- function(x, cls) {
  kruskal.test(x ~ cls);
}

#'Fisher for ANOVA
#'@description Perform  Fisher LSD for ANOVA, used in higher function 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
FisherLSD<-function(aov.obj, thresh){
  LSD.test(aov.obj,"cls", alpha=thresh)
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseTukey <- function(tukey, cut.off){
  inx <- tukey$cls[,"p adj"] <= cut.off;
  paste(rownames(tukey$cls)[inx], collapse="; ");
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseFisher <- function(fisher, cut.off){
  inx <- fisher[,"pvalue"] <= cut.off;
  paste(rownames(fisher)[inx], collapse="; ");
}

#'Perform ANOVA analysis
#'@description ANOVA analysis
#'@usage ANOVA.Anal(mSetObj=NA, nonpar=F, thresh=0.05, post.hoc="fisher")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nonpar Logical, use a non-parametric test (T) or not (F)
#'@param thresh Numeric, from 0 to 1, indicate the p-value threshold
#'@param post.hoc Input the name of the post-hoc test, "fisher" or "tukey"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ANOVA.Anal<-function(mSetObj=NA, nonpar=F, thresh=0.05, post.hoc="fisher"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  sig.num <- 0;
  if(nonpar){
    aov.nm <- "Kruskal Wallis Test";
    anova.res<-apply(as.matrix(mSetObj$dataSet$norm), 2, kwtest, cls=mSetObj$dataSet$cls);
    
    #extract all p values
    res <- unlist(lapply(anova.res, function(x) {c(x$statistic, x$p.value)}));
    res <- data.frame(matrix(res, nrow=length(anova.res), byrow=T), stringsAsFactors=FALSE);
    
    fstat <- res[,1];
    p.value <- res[,2];
    
    names(fstat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
    fdr.p <- p.adjust(p.value, "fdr");
    
    #inx.imp <- p.value <= thresh;
    inx.imp <- fdr.p <= thresh;
    sig.num <- sum(inx.imp);
    if(sig.num > 0){ 
      sig.f <- fstat[inx.imp];
      sig.p <- p.value[inx.imp];
      fdr.p <- fdr.p[inx.imp];
      
      sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), 'NA');
      rownames(sig.mat) <- names(sig.p);
      colnames(sig.mat) <- c("chi.squared", "p.value", "-log10(p)", "FDR", "Post-Hoc");
      
      # order the result simultaneously
      ord.inx <- order(sig.p, decreasing = FALSE);
      sig.mat <- sig.mat[ord.inx,,drop=F];
      
      fileName <- "kw_posthoc.csv";
      my.mat <- sig.mat[,1:4];
      colnames(my.mat) <- c("chi_squared", "pval_KW", "-log10(p)", "FDR");
    }
  }else{
    aov.nm <- "One-way ANOVA";
    aov.res <- apply(as.matrix(mSetObj$dataSet$norm), 2, aof, cls=mSetObj$dataSet$cls);
    anova.res <- lapply(aov.res, anova);
    
    #extract all p values
    res <- unlist(lapply(anova.res, function(x) { c(x["F value"][1,], x["Pr(>F)"][1,])}));
    res <- data.frame(matrix(res, nrow=length(aov.res), byrow=T), stringsAsFactors=FALSE);
    
    fstat <- res[,1];
    p.value <- res[,2];
    names(fstat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
    
    fdr.p <- p.adjust(p.value, "fdr");
    
    # do post-hoc only for signficant entries
    # inx.imp <- p.value <= thresh;
    inx.imp <- fdr.p <= thresh;
    sig.num <- sum(inx.imp);
    if(sig.num > 0){ 
      aov.imp <- aov.res[inx.imp];
      sig.f <- fstat[inx.imp];
      sig.p <- p.value[inx.imp];
      fdr.p <- fdr.p[inx.imp];
      cmp.res <- NULL;
      post.nm <- NULL;
      if(post.hoc=="tukey"){
        tukey.res<-lapply(aov.imp, TukeyHSD, conf.level=1-thresh);
        cmp.res <- unlist(lapply(tukey.res, parseTukey, cut.off=thresh));
        post.nm = "Tukey's HSD";
      }else{
        fisher.res<-lapply(aov.imp, FisherLSD, thresh);
        cmp.res <- unlist(lapply(fisher.res, parseFisher, cut.off=thresh));
        post.nm = "Fisher's LSD";
      }
      
      # create the result dataframe,
      # note, the last column is string, not double
      
      sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), cmp.res);
      rownames(sig.mat) <- names(sig.p);
      colnames(sig.mat) <- c("f.value", "p.value", "-log10(p)", "FDR", post.nm);
      
      # order the result simultaneously
      ord.inx <- order(sig.p, decreasing = FALSE);
      sig.mat <- sig.mat[ord.inx,,drop=F];
      fileName <- "anova_posthoc.csv";
    }
  }
  
  mSetObj$msgSet$current.msg <- paste(c("A total of", sum(inx.imp), "significant features were found."), collapse=" ");
  if(sig.num> 0){
    res <- 1;
    write.csv(sig.mat,file=fileName);
    aov<-list (
      aov.nm = aov.nm,
      sig.num = sig.num,
      sig.nm = fileName,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      post.hoc = post.hoc,
      sig.mat = sig.mat
    );
  }else{
    res <- 0;
    aov<-list (
      aov.nm = aov.nm,
      sig.num = sig.num,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.log = -log10(p.value),
      inx.imp = inx.imp
    );
  }
  mSetObj$analSet$aov <- aov;
  .set.mSet(mSetObj);
  return(res);
}

#'Plot ANOVA 
#'@description Plot ANOVA 
#'@usage PlotANOVA(mSetObj=NA, imgName, dpi, format, width)
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
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(lod, ylab="-log10(p)", xlab = GetVariableLabel(mSetObj), main=mSetObj$analSet$aov$aov.nm, type="n");
  red.inx <- which(mSetObj$analSet$aov$inx.imp);
  blue.inx <- which(!mSetObj$analSet$aov$inx.imp);
  points(red.inx, lod[red.inx], bg="red", cex=1.2, pch=21);
  points(blue.inx, lod[blue.inx], bg="green", pch=21);
  abline (h=mSetObj$analSet$aov$thresh, lty=3);
  dev.off();
  return(.set.mSet(mSetObj));
}

PlotCmpdView <- function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  mSetObj <- .get.mSet(mSetObj);
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
  Cairo(file = imgName, dpi=dpi, width=240, height=240, type=format, bg="transparent");
  par(mar=c(4,3,1,2), oma=c(0,0,1,0));
  boxplot(mSetObj$dataSet$norm[, cmpdNm]~mSetObj$dataSet$cls,las=2, col= unique(GetColorSchema(mSetObj)));
  title(main=cmpdNm, out=T);
  dev.off();
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetSigTable.FC <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$fc$sig.mat, "fold change analysis", mSetObj);
}

GetFCSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$fc$sig.mat));
}

GetFCSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$fc$sig.mat);
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

GetSigTable.Anova <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$aov$sig.mat, "One-way ANOVA and post-hoc analysis", mSetObj);
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

GetAnovaLnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  as.matrix(rbind(c(0, mSetObj$analSet$aov$thresh), c(length(lod)+1,mSetObj$analSet$aov$thresh)));
}

GetAnovaCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$aov$p.log);
}

GetAnovaCmpdInxs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(1:length(mSetObj$analSet$aov$p.log));
}

GetMaxAnovaInx <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  which.max(mSetObj$analSet$aov$p.log);
}

GetAnovaSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov$sig.nm;
}

GetTTSigNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$tt$sig.num);
}

GetSigTable.TT <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$tt$sig.mat, "t-tests", mSetObj);
}

#'T-test matrix
#'@description Return a double matrix with 2 columns - p values and lod
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

GetTtLnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- analSet$tt$p.log;
  as.matrix(rbind(c(0, mSetObj$analSet$tt$thresh), c(length(lod)+1,mSetObj$analSet$tt$thresh)));
}

GetTtCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(1:length(mSetObj$analSet$tt$p.log));
}

GetTtCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$tt$p.log);
}

GetMaxTtInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  which.max(mSetObj$analSet$tt$p.log);
}

GetTtestSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$tt$sig.nm;
}

#'Retrieve T-test p-values
#'@description Utility method to get p values
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetTtestRes <- function(mSetObj=NA, paired=FALSE, equal.var=TRUE, nonpar=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(nonpar){
    inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
    inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
    
    res <- apply(as.matrix(mSetObj$dataSet$norm), 2, function(x) {
      tmp <- try(wilcox.test(x[inx1], x[inx2], paired = paired));
      if(class(tmp) == "try-error") {
        return(c(NA, NA));
      }else{
        return(c(tmp$statistic, tmp$p.value));
      }
    })
    
  }else{
    if(ncol(mSetObj$dataSet$norm) < 1000){
      inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
      inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
      res <- apply(as.matrix(mSetObj$dataSet$norm), 2, function(x) {
        tmp <- try(t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var));
        if(class(tmp) == "try-error") {
          return(c(NA, NA));
        }else{
          return(c(tmp$statistic, tmp$p.value));
        }
      })
    }else{ # use fast version
      library(genefilter);
      res <- try(rowttests(t(as.matrix(mSetObj$dataSet$norm)), mSetObj$dataSet$cls));
      if(class(res) == "try-error") {
        res <- c(NA, NA);
      }else{
        res <- t(cbind(res$statistic, res$p.value));
      }
    }
  }
  return(t(res));
}

#'Utility method to perform the univariate analysis automatically
#'@description The approach is computationally expensive,and fails more often 
#'get around: make it lazy unless users request, otherwise the default t-test will also be affected
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetUnivReport <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  paired <- mSetObj$analSet$tt$paired;
  threshp <- mSetObj$analSet$tt$raw.thresh;
  
  inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
  inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
  
  # output list (mean(sd), mean(sd), p-value, FoldChange, Up/Down) 
  univStat.mat <- apply(as.matrix(mSetObj$dataSet$norm), 2, function(x) {
    
    # normality test for each group
    # ks <- ks.test(x[inx1], x[inx2]); 
    if( var(x[inx1], na.rm=T) == 0 |var(x[inx2], na.rm=T) == 0 ){ # shapiro cannot work when all values are same
      method = "";
    }else{
      sw.g1 <- shapiro.test(x[inx1]); 
      sw.g2 <- shapiro.test(x[inx2]); 
      method <- ifelse( ((sw.g1$p.value <= 0.05) | (sw.g2$p.value <= 0.05)), "(W)","")
    }
    if (method == "(W)") {
      # wilcoxon test
      tmp <- try(wilcox.test(x[inx1], x[inx2], paired = paired));
    } else {
      # t-test
      equal.var <- TRUE;
      if(var(x, na.rm=TRUE) != 0) {
        anal.var <- var.test(x[inx1], x[inx2]);
        equal.var <- ifelse(anal.var$p.value <= 0.05, FALSE, TRUE);
      }
      
      tmp <- try(t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var));
    }
    if(class(tmp) == "try-error") {
      return(NA);
    }else{            
      mean1 <- mean(x[inx1]);
      mean2 <- mean(x[inx2]);
      sd1 <- sd(x[inx1]);
      sd2 <- sd(x[inx2]);
      p.value <- paste(ifelse(tmp$p.value < 0.0001, "< 0.0001", sprintf("%.4f", tmp$p.value,4))," ", method, sep="");
      p.value.origin <- tmp$p.value;
      foldChange <- mean1 / mean2;
      foldChange <- round(ifelse( foldChange >= 1, foldChange, (-1/foldChange) ), 2);
      upDown <- ifelse(mean1 > mean2, "Up","Down");
      
      univStat <- c(
        meanSD1   = sprintf("%.3f (%.3f)", mean1, sd1),
        meanSD2   = sprintf("%.3f (%.3f)", mean2, sd2),
        p.value = p.value,
        foldChange = foldChange,
        upDown  = upDown,
        p.value.origin = sprintf("%.5f", p.value.origin)
      );
      return(univStat);
    }
  })
  
  univStat.mat <- as.data.frame(t(univStat.mat));
  
  # add FDR/q-value
  q.value <- sprintf("%.4f", p.adjust(p=as.numeric(levels(univStat.mat$p.value.origin))[univStat.mat$p.value.origin], method='fdr'));
  univStat.mat <- cbind(univStat.mat[, c(1,2,3)], q.value, univStat.mat[, c(4,5)], univStat.mat[,6]);
  names(univStat.mat)[1] <- paste("Mean (SD) of ", levels(mSetObj$dataSet$cls)[1], sep='');
  names(univStat.mat)[2] <- paste("Mean (SD) of ", levels(mSetObj$dataSet$cls)[2], sep='');
  names(univStat.mat)[3] <- "p-value";
  names(univStat.mat)[4] <- "q-value (FDR)";
  names(univStat.mat)[5] <- "Fold Change";
  names(univStat.mat)[6] <- paste(levels(mSetObj$dataSet$cls)[1],"/", levels(mSetObj$dataSet$cls)[2], sep='');
  names(univStat.mat)[7] <- "p.value.origin";
  
  univStat.mat <- cbind(Name=rownames(univStat.mat), univStat.mat);
  rownames(univStat.mat) <- NULL
  
  ## generate univariate report file (univAnalReport.csv).
  ## mixed with t-test and wilcoxon test depend on each metabolite's distribution
  univAnal.mat <- univStat.mat;
  note.str <- paste("\n Univariate Analysis Result for each variable/metabolite\n\n",
                    "[NOTE]\n", 
                    "    p-value is calculated with t-test as a default.\n",
                    "    p-value with (W) is calculated by the Wilcoxon Mann Whitney test\n\n\n", sep='');
  
  cat(note.str, file="univAnalReport.csv", append=FALSE);
  write.table(univAnal.mat, file="univAnalReport.csv", append=TRUE, sep=",", row.names=FALSE);
  
  ## generate subset with the threshold (p-value)
  sigones <- which(as.numeric(as.character(univAnal.mat$p.value.origin)) <= threshp);
  
  sigDataSet.orig <- cbind(SampleID=rownames(mSetObj$dataSet$orig), Label=mSetObj$dataSet$cls, mSetObj$dataSet$orig[,c(sigones)])
  sigDataSet.norm <- cbind(SampleID=rownames(mSetObj$dataSet$orig), Label=mSetObj$dataSet$cls, mSetObj$dataSet$norm[,c(sigones)])
  
  write.table(sigDataSet.orig, file=paste("data_subset_orig_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
  write.table(sigDataSet.norm, file=paste("data_subset_norm_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
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
    xs <- vcn$fc.log.uniq[blue.inx]
    ys <- vcn$p.log[blue.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log.uniq[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoVlMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  limy <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(vcn$min.xthresh, limy[1]), c(vcn$min.xthresh,limy[2])));
}

GetVolcanoVrMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  limy <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(vcn$max.xthresh, limy[1]), c(vcn$max.xthresh,limy[2])));
}

GetVolcanoHlMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn<-mSetObj$analSet$volcano;
  limx <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(limx[1], vcn$thresh.y), c(limx[2],vcn$thresh.y)));
}

GetVolcanoRangeX <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  range(mSetObj$analSet$volcano$fc.log.uniq);
}

GetVolcanoCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$volcano$fc.log);
}

GetVolcanoCmpdInxs <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$volcano$fc.log.uniq
}

#'Volcano indices
#'@description Get indices of top n largest/smallest number
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

GetSigTable.Volcano <- function(mSetObj=NA){
  GetSigTable(mSetObj$analSet$volcano$sig.mat, "volcano plot", mSetObj);
}

GetVolcanoSigMat <- function(mSetObj=NA){
  return(CleanNumber(mSetObj$analSet$volcano$sig.mat));
}

GetVolcanoSigRowNames <- function(mSetObj=NA){
  rownames(mSetObj$analSet$volcano$sig.mat);
}

GetVolcanoSigColNames <- function(mSetObj=NA){
  colnames(mSetObj$analSet$volcano$sig.mat);
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