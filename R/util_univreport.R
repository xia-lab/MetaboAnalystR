my.univ.report <- function(mSetObj=NA){
  
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
  orig.data<- qs::qread("data_orig.qs");
  sigDataSet.orig <- cbind(SampleID=rownames(orig.data), Label=mSetObj$dataSet$cls, orig.data[,c(sigones)])
  sigDataSet.norm <- cbind(SampleID=rownames(orig.data), Label=mSetObj$dataSet$cls, orig.data[,c(sigones)])
  
  write.table(sigDataSet.orig, file=paste("data_subset_orig_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
  write.table(sigDataSet.norm, file=paste("data_subset_norm_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
}