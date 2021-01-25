#'Perform Signifiance Analysis of Microarrays (SAM) analysis
#'@description Perform SAM
#'@param mSetObj Input name of the created mSet Object
#'@param method Method for SAM analysis, default is set to "d.stat", alternative is "wilc.stat"
#'@param paired Logical, indicates if samples are paired or not. Default is set to FALSE
#'@param varequal Logical, indicates if variance is equal. Default is set to TRUE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import siggenes
#'@import qs
SAM.Anal <- function(mSetObj=NA, method="d.stat", paired=FALSE, varequal=TRUE, delta=0, imgName){
    .prepare.sam.anal(mSetObj, method, paired, varequal, delta, imgName);
    .perform.computing();
    .save.sam.anal(mSetObj);
}

.prepare.sam.anal <- function(mSetObj=NA, method="d.stat", paired=FALSE, varequal=TRUE, delta=0, imgName){
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi72.png", sep="");
  mat <- t(mSetObj$dataSet$norm); # in sam the column is sample
  cl <- as.factor(mSetObj$dataSet$cls); # change to 0 and 1 for class label

  my.fun<-function(){  
    library(siggenes);
    if(dat.in$cls.num==2){
      if(dat.in$paired){
        dat.in$cls <- dat.in$cls.paired;
      }
      if(dat.in$method == "d.stat"){
        sam_out <- siggenes::sam(dat.in$data, dat.in$cls, method=d.stat, var.equal=dat.in$varequal, R.fold=0, rand=123);
      }else{
        sam_out <- siggenes::sam(dat.in$data, dat.in$cls, method=wilc.stat, R.fold=0,rand=123);
      }
    }else{
      sam_out <- siggenes::sam(dat.in$data, dat.in$cls, rand=123);
    }
    
    # check if need to compute a suitable delta value
    delta <- dat.in$delta;
    if(delta == 0){
      mat.fdr <- sam_out@mat.fdr
      deltaVec <- mat.fdr[,"Delta"];
      fdrVec <- mat.fdr[,"FDR"];
      signumVec <- mat.fdr[,"Called"];
      
      delta <- deltaVec[1];
      for(i in 1:length(deltaVec)){
        my.delta = deltaVec[i];
        fdr = fdrVec[i];
        called = signumVec[i];
        if(called > 0){ # at least 1 significant cmpd
          # check fdr, default threshold 0.01
          # if too many significant compounds, tight up and vice versa
          if(fdr < 0.001){
            delta <- my.delta; break;
          }else if(fdr < 0.01 & called < 100){
            delta <- my.delta; break;
          }else if(fdr < 0.05 & called <50){
            delta <- my.delta; break;
          }else if(fdr < 0.1 & called < 20){
            delta <- my.delta; break;
          }else if(called < 10){
            delta <- my.delta; break;
          }
        }
      }
    }
    
    # get the signficant features
    summary.mat <- summary(sam_out, delta)@mat.sig;
    sig.mat <- as.matrix(signif(summary.mat[,-c(1,6)],5));
    data.table::fwrite(as.data.frame(sig.mat), file="sam_sigfeatures.csv", row.names=TRUE);
    
    # plot SAM plot
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=72, width=8, height=8, type="png", bg="white");
    siggenes::plot(sam_out, delta);
    dev.off();        
    
    return(list(sam.res=sam_out, sam.delta=delta, sig.mat=sig.mat, img=imgName));
  }

  dat.in <- list(data=mat, cls=cl, cls.num=mSetObj$dataSet$cls.num, method=method, varequal=varequal,
                  paired=paired, delta=delta, cls.paired=as.numeric(mSetObj$dataSet$pairs), imgName=imgName, my.fun=my.fun);

  qs::qsave(dat.in, file="dat.in.qs");
  mSetObj$imgSet$sam.cmpd <- imgName;  
  return(.set.mSet(mSetObj));
}

.save.sam.anal <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  my.res <- dat.in$my.res;
  mSetObj$analSet$sam <- my.res$sam.res;
  mSetObj$analSet$sam.delta  <- my.res$sam.delta;
  mSetObj$analSet$sam.cmpds <- my.res$sig.mat;
  return(.set.mSet(mSetObj));
}

#'Plot SAM Delta Plot 
#'@description Plot SAM Delta Plot (FDR)
#'@param mSetObj Input name of the created mSet Object
#'@param delta Input the delta
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
PlotSAM.FDR <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 7.2;
  }
  h <- w*3/5;
  
  mSetObj$imgSet$sam.fdr <- imgName;
  delta <- mSetObj$analSet$sam.delta;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow=c(1,2), mar=c(5,6,4,1));
  mat.fdr <- mSetObj$analSet$sam@mat.fdr;
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
#'@import qs

PlotSAM.Cmpd <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
    .prepare.sam.cmpd(mSetObj, imgName, format, dpi, width);
    .perform.computing();
    # no need to collect as it is an image
}

.prepare.sam.cmpd <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  # note, this is now a remote call and only used for other formats by users
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  my.fun <- function(){
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=dat.in$dpi, width=dat.in$width, height=dat.in$height, type=dat.in$type, bg="white");
    siggenes::plot(dat.in$mSetObj$analSet$sam, dat.in$mSetObj$analSet$sam.delta);
    dev.off();
  }
  dat.in <- list(mSetObj = mSetObj, dpi=dpi, width=w, height=h, type=format, imgName=imgName, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");
  mSetObj$imgSet$sam.cmpd <- imgName;
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description deteriming a0, only applicable for z.ebam (default)
#'@param mSetObj Input name of the created mSet Object
#'@param isPaired Logical
#'@param isVarEq Logical
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import qs
EBAM.Init <- function(mSetObj=NA, isPaired, isVarEq, nonPar, A0=-99, delta, imgA0, imgSig){
    .prepare.ebam.init(mSetObj, isPaired, isVarEq, nonPar, A0, delta, imgA0, imgSig);
    .perform.computing();
    .save.ebam.init(mSetObj);
}

.prepare.ebam.init <- function(mSetObj=NA, isPaired, isVarEq, nonPar, A0=-99, delta, imgA0, imgSig){
  mSetObj <- .get.mSet(mSetObj);
  if(isPaired){
    cl.ebam <- as.numeric(mSetObj$dataSet$pairs); 
  }else{
    cl.ebam <- as.numeric(mSetObj$dataSet$cls)-1; # change to 0 and 1 for class label
  }
  method <- "z.ebam";
  if(nonPar && length(levels(mSetObj$dataSet$cls)) == 2){
    method <- "wilc.ebam"
  }
  conc.ebam <- t(mSetObj$dataSet$norm); # in sam column is sample, row is gene
  
  imgA0 = paste(imgA0, "dpi72.png", sep="");
  imgSig = paste(imgSig, "dpi72.png", sep="");

  my.fun <- function(){
    library(siggenes);
    ebam_a0 <- siggenes::find.a0(dat.in$data, dat.in$cls, var.equal=dat.in$isVarEq, gene.names=rownames(dat.in$data), rand=123);
    
    # plotting ebam A0
    Cairo::Cairo(file = dat.in$imgA0, unit="in", dpi=72, width=8, height=6, type="png", bg="white");
    plot(ebam_a0);
    dev.off();
    
    A0 <- dat.in$A0;
    if(A0 == -99){ # default
      A0 <- round(as.numeric(ebam_a0@suggested),4)
    }
    if(dat.in$method=="z.ebam"){
      ebam_out <- siggenes::ebam(dat.in$data, dat.in$cls, method=z.ebam, a0=A0, var.equal=dat.in$isVarEq, fast=TRUE, gene.names=rownames(dat.in$data), rand=123);
    }else{
      ebam_out <- siggenes::ebam(dat.in$data, dat.in$cls, method=wilc.ebam, gene.names=rownames(dat.in$data), rand=123);
    }
    
    # plotting ebam sig features
    Cairo::Cairo(file = dat.in$imgSig, unit="in", dpi=72, width=7, height=7, type="png", bg="white");
    plot(ebam_out, delta);
    dev.off();
    
    summary.mat <- summary(ebam_out, delta)@mat.sig;
    sig.mat <- as.matrix(signif(summary.mat[,-1],5));
    data.table::fwrite(as.data.frame(sig.mat),file="ebam_sigfeatures.csv", row.names=TRUE);
    
    return(list(ebam_a0=A0, ebam_out=ebam_out, sig.mat=sig.mat, a0=A0, delta=delta));
  }
  
  dat.in <- list(data=conc.ebam, cls=cl.ebam, isVarEq=isVarEq, method=method,  A0=A0, imgA0=imgA0, imgSig=imgSig, my.fun=my.fun); 
  qs::qsave(dat.in, file="dat.in.qs");

  mSetObj$imgSet$ebam.a0 <- imgA0;
  mSetObj$imgSet$ebam.cmpd <-imgSig;
  return(.set.mSet(mSetObj));
}

.save.ebam.init <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  my.res <- dat.in$my.res;
  mSetObj$analSet$ebam <- my.res$ebam_out;
  mSetObj$analSet$ebam.cmpds <- my.res$sig.mat;
  mSetObj$analSet$ebam.a0 <- my.res$ebam_a0;
  mSetObj$analSet$ebam.delta <- my.res$delta;
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
#'@import qs
#'@export
#'
PlotEBAM.Cmpd<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
    .prepare.ebam.cmpd(mSetObj, imgName, format, dpi, width);
    .perform.computing();
}
 
.prepare.ebam.cmpd <-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  # note, this is now a remote call and only used for other formats by users
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 7;
  }else if(width == 0){
    w <- h <- 7;
    mSetObj$imgSet$ebam.cmpd <-imgName;
  }else{
    w <- h <- width;
  }
  
  my.fun <- function(){
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=dat.in$dpi, width=dat.in$width, height=dat.in$height, type=dat.in$type, bg="white");
    siggenes::plot(dat.in$mSetObj$analSet$ebam, dat.in$mSetObj$analSet$ebam.delta);
    dev.off();
  }
  dat.in <- list(mSetObj = mSetObj, dpi=dpi, width=w, height=h, type=format, imgName=imgName, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");

  mSetObj$imgSet$sam.cmpd <- imgName;
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetSAMDeltaRange <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mat.fdr <- mSetObj$analSet$sam@mat.fdr;
  rng <- range(mat.fdr[,"Delta"]);
  step <- (rng[2]-rng[1])/12
  return(signif(c(rng, step), 3));
}

#'For SAM analysis 
#'@description obtain a default delta with reasonable number
#'of sig features and decent FDR
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetSuggestedSAMDelta <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$sam.delta);
}

GetEBAMSuggestedA0 <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ebam.a0);
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
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
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.EBAM <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$ebam.cmpds, "EBAM", mSetObj$dataSet$type);
}

