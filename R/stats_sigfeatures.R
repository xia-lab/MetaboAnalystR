#'Perform Signifiance Analysis of Microarrays (SAM) analysis
#'@description Perform SAM
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SAM.Anal <- function(mSetObj=NA, method="d.stat", paired=FALSE, varequal=TRUE){
  
  mSetObj <- .get.mSet(mSetObj);
  suppressMessages(library(siggenes));
  mat <- t(mSetObj$dataSet$norm); # in sam the column is sample
  cl <- as.factor(mSetObj$dataSet$cls); # change to 0 and 1 for class label
  if(mSetObj$dataSet$cls.num==2){
    if(paired){
      cl<-as.numeric(mSetObj$dataSet$pairs);
    }
    if(method == "d.stat"){
      sam_out <- sam(mat, cl, method=d.stat, var.equal=varequal, R.fold=0, rand=123);
    }else{
      sam_out <- sam(mat, cl, method=wilc.stat, R.fold=0,rand=123);
    }
  }else{
    sam_out <- sam(mat, cl, rand=123);
  }
  mSetObj$analSet$sam <- sam_out;
  return(.set.mSet(mSetObj));
}

#'Set Signifiance Analysis of Microarrays (SAM) analysis matrix
#'@description Create SAM matrix
#'@param mSetObj Input name of the created mSet Object
#'@export
#'
SetSAMSigMat <- function(mSetObj=NA, delta){
  mSetObj <- .get.mSet(mSetObj);
  sam.sum <- siggenes::summary(mSetObj$analSet$sam, delta);
  summary.mat <- sam.sum@mat.sig;
  
  sig.mat <- as.matrix(signif(summary.mat[,-c(1,6)],5));
  write.csv(signif(sig.mat,5), file="sam_sigfeatures.csv");
  mSetObj$analSet$sam.cmpds <- sig.mat;
  mSetObj$analSet$sam.delta <- delta;
  return(.set.mSet(mSetObj));
}

#'Plot SAM Delta Plot 
#'@description Plot SAM Delta Plot (FDR)
#'@param mSetObj Input name of the created mSet Object
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
PlotSAM.FDR <- function(mSetObj=NA, delta, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 7.2;
  }
  h <- w*3/5;
  
  mSetObj$imgSet$sam.fdr <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow=c(1,2), mar=c(5,6,4,1));
  mat.fdr<-mSetObj$analSet$sam@mat.fdr;
  plot(mat.fdr[,"Delta"],mat.fdr[,"FDR"],xlab='Delta',ylab=NA,type="b", col='blue', las=2);
  abline(v = delta, lty=3, col="magenta");
  mtext("FDR", side=2, line=5);
  par(mar=c(5,5,4,2))
  plot(mat.fdr[,"Delta"],mat.fdr[,"Called"],xlab='Delta',ylab="Significant feaure No.",type="b", col='blue', las=2);
  abline(v = delta, lty=3, col="magenta");
  
  hit.inx <- mat.fdr[,"Delta"] <= delta;
  my.fdr <- signif(min(mat.fdr[,"FDR"][hit.inx]), 3);
  my.sigs <- min(mat.fdr[,"Called"][hit.inx]);
  mtext(paste("Delta:", delta, " FDR:", my.fdr, " Sig. cmpds:", my.sigs), line=-2, side = 3, outer = TRUE, font=2)
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Plot SAM 
#'@description Plot SAM with positive and negative metabolite sets
#'@param mSetObj Input name of the created mSet Object
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

PlotSAM.Cmpd <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  sam.out <- mSetObj$analSet$sam
  suppressMessages(library(siggenes));
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$sam.cmpd <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  siggenes::plot(mSetObj$analSet$sam, mSetObj$analSet$sam.delta);
  
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description deteriming a0, only applicable for z.ebam (default)
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
EBAM.A0.Init <- function(mSetObj=NA, isPaired, isVarEq){
  mSetObj <- .get.mSet(mSetObj);
  suppressMessages(library(siggenes));
  if(isPaired){
    cl.ebam <- as.numeric(mSetObj$dataSet$pairs); 
  }else{
    cl.ebam <- as.numeric(mSetObj$dataSet$cls)-1; # change to 0 and 1 for class label
  }
  conc.ebam <- t(mSetObj$dataSet$norm); # in sam column is sample, row is gene
  ebam_a0 <- find.a0(conc.ebam, cl.ebam, var.equal=isVarEq, gene.names = names(mSetObj$dataSet$norm), rand=123);
  mSetObj$analSet$ebam.a0 <- ebam_a0;
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description plot ebam a0 plot also return the analSet$ebam.a0 object 
#'so that the suggested a0 can be obtained
#'@usage PlotEBAM.A0(imgName, format, dpi, width)
#'@param mSetObj Input name of the created mSet Object
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
PlotEBAM.A0<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7; 
  }
  h <- 3*w/4;
  
  mSetObj$imgSet$ebam.a0 <- imgName;
  
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mSetObj$analSet$ebam.a0);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description note: if method is wilcoxon, the A0 and var equal will be ignored
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
EBAM.Cmpd.Init <- function(mSetObj=NA, method="z.ebam", A0=0, isPaired=FALSE, isVarEq=TRUE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(isPaired){
    cl.ebam <- as.numeric(mSetObj$dataSet$pairs);
  }else{
    cl.ebam <- as.numeric(mSetObj$dataSet$cls)-1;
  }
  conc.ebam <- t(mSetObj$dataSet$norm); # in sam column is sample, row is feature
  if(method=="z.ebam"){
    ebam_out <- ebam(conc.ebam, cl.ebam, method=z.ebam, a0=A0, var.equal=isVarEq, fast=TRUE, gene.names = names(mSetObj$dataSet$norm), rand=123);
  }else{
    ebam_out <- ebam(conc.ebam, cl.ebam, method=wilc.ebam, gene.names = names(mSetObj$dataSet$norm), rand=123);
  }
  mSetObj$analSet$ebam <- ebam_out;
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description return double matrix with 3 columns - z.value, posterior, local.fdr
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetEBAMSigMat <- function(mSetObj=NA, delta){
  mSetObj <- .get.mSet(mSetObj);
  ebam.sum <- siggenes::summary(mSetObj$analSet$ebam, delta);
  summary.mat <- ebam.sum@mat.sig;
  sig.mat <- as.matrix(signif(summary.mat[,-1],5));
  write.csv(signif(sig.mat,5),file="ebam_sigfeatures.csv");
  mSetObj$analSet$ebam.cmpds <- sig.mat;
  mSetObj$analSet$ebam.delta <- delta;
  return(.set.mSet(mSetObj));
}

#'Plot EBAM
#'@description Plot EBAM
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@usage PlotEBAM.Cmpd(mSetObj=NA, imgName, format, dpi, width)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotEBAM.Cmpd<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 7;
  }else if(width == 0){
    w <- h <- 7;
    mSetObj$imgSet$ebam.cmpd <-imgName;
  }else{
    w <- h <- width;
  }
  Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mSetObj$analSet$ebam, mSetObj$analSet$ebam.delta);
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetSAMDeltaRange <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mat.fdr<-mSetObj$analSet$sam@mat.fdr;
  rng <- range(mat.fdr[,"Delta"]);
  step <- (rng[2]-rng[1])/12
  return(signif(c(rng, step), 3));
}

#'For SAM analysis 
#'@description obtain a default delta with reasonable number
#'of sig features and decent FDR
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetSuggestedSAMDelta <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mat.fdr<-mSetObj$analSet$sam@mat.fdr
  deltaVec <- mat.fdr[,"Delta"];
  
  fdrVec <- mat.fdr[,"FDR"];
  signumVec <- mat.fdr[,"Called"];
  for(i in 1:length(deltaVec)){
    delta = deltaVec[i];
    fdr = fdrVec[i];
    called = signumVec[i];
    if(called > 0){ # at least 1 significant cmpd
      # check fdr, default threshold 0.01
      # if too many significant compounds, tight up and vice versa
      if(fdr < 0.001){
        return (delta);
      }else if(fdr < 0.01 & called < 100){
        return (delta);
      }else if(fdr < 0.05 & called <50){
        return (delta);
      }else if(fdr < 0.1 & called < 20){
        return (delta);
      }else if(called < 10){
        return (delta);
      }
    }
  }
  return (deltaVec[1]); # if no significant found, return the first one
}

GetSAMSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$sam.cmpds));
}

GetSAMSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$sam.cmpds);
}

GetSAMSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$sam.cmpds);
}

#'Sig table for SAM
#'@export
GetSigTable.SAM <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$sam.cmpds, "SAM", mSetObj$dataSet$type);
}

GetEBAMSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$ebam.cmpds));
}

GetEBAMSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$ebam.cmpds);
}

GetEBAMSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$ebam.cmpds);
}

#'Sig table for EBAM
#'@export
GetSigTable.EBAM <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$ebam.cmpds, "EBAM", mSetObj$dataSet$type);
}

