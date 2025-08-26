#'Perform Signifiance Analysis of Microarrays (SAM) analysis
#'@description Perform SAM
#'@param mSetObj Input name of the created mSet Object
#'@param method Method for SAM analysis, default is set to "d.stat", alternative is "wilc.stat"
#'@param paired Logical, indicates if samples are paired or not. Default is set to FALSE
#'@param varequal Logical, indicates if variance is equal. Default is set to TRUE
#'@param delta numeric
#'@param imgName image name, character
#'@param dpi image dpi, integer
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import siggenes
#'@import qs
SAM.Anal <- function(mSetObj=NA, method="d.stat", paired=FALSE, varequal=TRUE, delta=0, imgName, dpi=default.dpi){

  mSetObj <- .get.mSet(mSetObj);
  .prepare.sam.anal(mSetObj, method, paired, varequal, delta, imgName, dpi);
  .perform.computing();

  if(.on.public.web){ 
    .save.sam.anal(mSetObj);
  }else{
     mSetObj <- .save.sam.anal(mSetObj);
  }
  return(.set.mSet(mSetObj));
}

.prepare.sam.anal <- function(mSetObj=NA, method="d.stat", paired=FALSE, varequal=TRUE, delta=0, imgName, dpi=default.dpi){

  if(.on.public.web){mSetObj <- .get.mSet(mSetObj);}
  imgName = paste(imgName, "dpi", dpi, ".png", sep="");
  mat <- t(mSetObj$dataSet$norm); # in sam the column is sample

  cl <- as.factor(mSetObj$dataSet$cls); # change to 0 and 1 for class label
  dat.in <- list(data=mat, cls=cl, cls.num=mSetObj$dataSet$cls.num, method=method, varequal=varequal,
                  paired=paired, delta=delta, cls.paired=as.numeric(mSetObj$dataSet$pairs), imgName=imgName);

  dat.in$my.fun<-function(){  
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
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=dpi, width=8, height=8, type="png", bg="white");
    #siggenes::plot(sam_out, delta);
    sam.plot2(sam_out, delta);
    dev.off();        
    
    return(list(sam.res=sam_out, sam.delta=delta, sig.mat=sig.mat, img=imgName));
  }

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
PlotSAM.FDR <- function(mSetObj=NA, imgName, format="png", dpi, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
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

PlotSAM.Cmpd <- function(mSetObj=NA, imgName, format="png", dpi, width=NA){
    mSetObj <- .get.mSet(mSetObj);
    .prepare.sam.cmpd(mSetObj, imgName, format, dpi, width);
    .perform.computing();    
    if(.on.public.web){
        # need to update image name after plotting
        mSetObj <- mSet;
    }
    return(.set.mSet(mSetObj))
}

 # note, this is now a microservice call and only used for other formats by users
.prepare.sam.cmpd <- function(mSetObj=NA, imgName, format="png", dpi, width=NA){
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;

  dat.in <- list(mSetObj = mSetObj, dpi=dpi, width=w, height=h, type=format, imgName=imgName);
  dat.in$my.fun <- function(){
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=dat.in$dpi, width=dat.in$width, height=dat.in$height, type=dat.in$type, bg="white");
    #siggenes::plot(dat.in$mSetObj$analSet$sam, dat.in$mSetObj$analSet$sam.delta);
    sam.plot2(dat.in$mSetObj$analSet$sam, dat.in$mSetObj$analSet$sam.delta);
    dev.off();
  }

  qs::qsave(dat.in, file="dat.in.qs");
  mSetObj$imgSet$sam.cmpd <- imgName;
  return(.set.mSet(mSetObj));
}

#'For EBAM analysis 
#'@description deteriming a0, only applicable for z.ebam (default)
#'@param mSetObj Input name of the created mSet Object
#'@param isPaired Logical
#'@param isVarEq Logical
#'@param nonPar nonPar
#'@param A0 A0
#'@param delta delta
#'@param imgA0 imgA0
#'@param imgSig imgSig
#'@param dpi dpi value of images
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import qs
EBAM.Init <- function(mSetObj=NA, isPaired, isVarEq, nonPar, A0=-99, delta, imgA0, imgSig, dpi = default.dpi){
    .prepare.ebam.init(mSetObj, isPaired, isVarEq, nonPar, A0, delta, imgA0, imgSig, dpi);
    .perform.computing();
    .save.ebam.init(mSetObj);
}

.prepare.ebam.init <- function(mSetObj=NA, isPaired, isVarEq, nonPar, A0=-99, delta, imgA0, imgSig, dpi=default.dpi){
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
  
  imgA0 = paste(imgA0, "dpi", dpi, ".png", sep="");
  imgSig = paste(imgSig, "dpi", dpi, ".png", sep="");

  dat.in <- list(data=conc.ebam, cls=cl.ebam, isVarEq=isVarEq, method=method,  A0=A0, imgA0=imgA0, imgSig=imgSig);
  dat.in$my.fun <- function(){
    library(siggenes);
    ebam_a0 <- siggenes::find.a0(dat.in$data, dat.in$cls, var.equal=dat.in$isVarEq, gene.names=rownames(dat.in$data), rand=123);
    
    # plotting ebam A0
    Cairo::Cairo(file = dat.in$imgA0, unit="in", dpi=dpi, width=8, height=6, type="png", bg="white");
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
    Cairo::Cairo(file = dat.in$imgSig, unit="in", dpi=dpi, width=7, height=7, type="png", bg="white");
    plot(ebam_out, delta);
    dev.off();
    
    summary.mat <- summary(ebam_out, delta)@mat.sig;
    sig.mat <- as.matrix(signif(summary.mat[,-1],5));
    data.table::fwrite(as.data.frame(sig.mat),file="ebam_sigfeatures.csv", row.names=TRUE);
    
    return(list(ebam_a0=A0, ebam_out=ebam_out, sig.mat=sig.mat, a0=A0, delta=delta));
  }
   
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
PlotEBAM.Cmpd<-function(mSetObj=NA, imgName, format="png", dpi, width=NA){
    .prepare.ebam.cmpd(mSetObj, imgName, format, dpi, width);
    .perform.computing();
}
 
.prepare.ebam.cmpd <-function(mSetObj=NA, imgName, format="png", dpi, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  # note, this is now a remote call and only used for other formats by users
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 8;
  }else if(width == 0){
    w <- h <- 8;
    mSetObj$imgSet$ebam.cmpd <-imgName;
  }else{
    w <- h <- width;
  }

  dat.in <- list(mSetObj = mSetObj, dpi=dpi, width=w, height=h, type=format, imgName=imgName);
  dat.in$my.fun <- function(){
    Cairo::Cairo(file = dat.in$imgName, unit="in", dpi=dat.in$dpi, width=dat.in$width, height=dat.in$height, type=dat.in$type, bg="white");
    plotEbam(dat.in$mSetObj$analSet$ebam, dat.in$mSetObj$analSet$ebam.delta);
    dev.off();
  }

  qs::qsave(dat.in, file="dat.in.qs");

  mSetObj$imgSet$ebam.cmpd <- imgName;
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

sam.plot2 <- function (object, delta, pos.stats = NULL, sig.col = 3, xlim = NULL, 
    ylim = NULL, main = NULL, xlab = NULL, ylab = NULL, pty = "s", 
    lab = c(10, 10, 7), pch = NULL, sig.cex = 1, ...) 
{
    if (!is(object, "SAM")) 
        stop("object must be an object of class SAM.")
    if (is.null(pos.stats)) 
        pos.stats <- ifelse(all(object@d >= 0, na.rm = TRUE), 
            2, 1)
    if (!pos.stats %in% 0:2) 
        stop("pos.stats must be either 0 (statistics are not displayed),\n", 
            "1 (stats are displayed in the upper left of the plot), or 2 (lower right).")
    if (length(sig.col) == 1) 
        col.down <- col.up <- sig.col
    else {
        col.down <- sig.col[1]
        col.up <- sig.col[2]
    }
    d.sort <- sort(object@d)
    d.bar <- object@d.bar
    if (is.null(xlim)) 
        xlim <- c(min(d.sort, d.bar), max(d.sort, d.bar))
    if (is.null(ylim)) 
        ylim <- c(min(d.sort, d.bar), max(d.sort, d.bar))
    if (is.null(main)) 
        main <- paste("SAM Plot for Delta =", delta)
    if (is.null(xlab)) 
        xlab <- "Expected d(i) values"
    if (is.null(ylab)) 
        ylab <- "Observed d(i) values"
    par.store <- list(par()$pty, par()$lab)
    on.exit(par(pty = par.store[[1]], lab = par.store[[2]]))
    par(pty = pty, lab = lab)
    mat.fdr <- stats.cal(object@d, object@d.bar, object@vec.false, 
        object@p0, delta = delta)
    d.up <- which(d.sort >= mat.fdr[, "cutup"])
    d.down <- which(d.sort <= mat.fdr[, "cutlow"])
    if (length(c(d.up, d.down)) == 0) 
        plot(d.bar, d.sort, main = main, xlab = xlab, ylab = ylab, 
            xlim = xlim, ylim = ylim, pch = pch, ...)
    else {
        plot(d.bar[-c(d.up, d.down)], d.sort[-c(d.up, d.down)], 
            main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim, pch = pch, ...)
        points(d.bar[d.up], d.sort[d.up], col = col.up, cex = sig.cex, 
            pch = pch)
        points(d.bar[d.down], d.sort[d.down], col = col.down, 
            cex = sig.cex, pch = pch)
    }
    abline(0, 1)
    abline(delta, 1, lty = 2)
    abline(-delta, 1, lty = 2)
    abline(h = mat.fdr[, "cutup"], lty = 5, cex = 1.5)
    abline(h = mat.fdr[, "cutlow"], lty = 5, cex = 1.5)
    stats <- paste(c("cutlow:", "cutup:", "p0:", "Significant:", 
        "False:", "FDR:"), round(mat.fdr[1, c("cutlow", "cutup", 
        "p0", "Called", "False", "FDR")], 3), sep = "  ")
    if (pos.stats == 1) 
        text(rep(xlim[1], 6), seq(ylim[2], ylim[2] - (ylim[2] - 
            ylim[1])/4, le = 6), stats, adj = 0, cex = 0.75)
    if (pos.stats == 2) 
        text(rep(xlim[2] - (xlim[2] - xlim[1])/4, 6), seq(ylim[1], 
            ylim[1] + (ylim[2] - ylim[1])/4, le = 6), stats, 
            adj = 0, cex = 0.75)
}

stats.cal <- function (d, d.bar, vec.false, p0, delta = NULL, le.delta = 10) 
{
    d.sort <- sort(d)
    d.diff <- d.sort - d.bar
    m <- length(d.diff)
    if (is.null(delta)) {
        ra.ddiff <- range(abs(d.diff))
        delta <- round(seq(max(0.1, ra.ddiff[1]), max(1, ra.ddiff[2]), 
            le = le.delta), 1)
    }
    else {
        if (any(delta <= 0)) 
            stop("delta must be larger than 0.")
        le.delta <- length(delta)
    }
    j0 <- which(d.bar == min(d.bar[d.bar >= 0]))[1]
    mat.fdr <- matrix(0, le.delta, 9)
    dimnames(mat.fdr) <- list(1:le.delta, c("Delta", "p0", "False", 
        "Called", "FDR", "cutlow", "cutup", "j2", "j1"))
    mat.fdr[, "Delta"] <- delta
    mat.fdr[, "p0"] <- p0
    vec.order <- as.numeric(na.exclude(vec.false[order(d)]))
    for (i in 1:le.delta) {
        mat.fdr[i, "j1"] <- j1 <- ifelse(any(d.diff[j0:m] >= 
            delta[i]), j0 - 1 + min(which(d.diff[j0:m] >= delta[i])), 
            m + 1)
        mat.fdr[i, "cutup"] <- ifelse(j1 != m + 1, d.sort[j1], 
            Inf)
        mat.fdr[i, "j2"] <- j2 <- ifelse(any(d.diff[1:(j0 - 1)] <= 
            -delta[i]) & j0 != 1, max(which(d.diff[1:(j0 - 1)] <= 
            -delta[i])), 0)
        mat.fdr[i, "cutlow"] <- ifelse(j2 != 0, d.sort[j2], -Inf)
        mat.fdr[i, "Called"] <- m - j1 + 1 + j2
        mat.fdr[i, "False"] <- ifelse(j1 == m + 1, 0, vec.order[j1]) + 
            ifelse(j2 == 0, 0, vec.order[j2])
        mat.fdr[i, "FDR"] <- min(p0 * mat.fdr[i, "False"]/max(mat.fdr[i, 
            "Called"], 1), 1)
    }
    mat.fdr
}

compNumber1 <- function (z, post, p0, B, delta = 0.9, vec.pos = NULL, vec.neg = NULL) 
{
    if (any(delta <= 0 | delta > 1)) 
        stop("The delta values must be between 0 and 1.")
    z.sort <- sort(z)
    z.order <- order(z)
    post <- post[z.order]
    if (length(vec.pos) == 0) 
        probs <- 1/(B * (1 - post)/p0 + 1)
    else {
        vec.pos <- vec.pos[z.order]
        vec.neg <- vec.neg[z.order]
    }
    n.delta <- length(delta)
    m <- length(z)
    mat.delta <- matrix(0, n.delta, 5)
    rownames(mat.delta) <- 1:n.delta
    colnames(mat.delta) <- c("Delta", "Number", "FDR", "CL", 
        "CU")
    mat.delta[, 1] <- delta
    for (i in 1:n.delta) {
        if (any(z.sort < 0) & post[1] >= delta[i]) {
            tmp <- post[z.sort < 0]
            tmp.ids <- which(tmp < delta[i])
            if (length(tmp.ids) == 0) 
                j1 <- ifelse(any(tmp == delta[i]), min(which(tmp == 
                  delta[i])), length(tmp))
            else j1 <- min(tmp.ids) - 1
            f1 <- if (length(vec.neg) == 0) 
                sum(1/probs[1:j1] - 1)/B
            else vec.neg[j1]
            mat.delta[i, 4] <- z.sort[j1]
        }
        else {
            j1 <- 0
            f1 <- 0
            mat.delta[i, 4] <- -Inf
        }
        if (post[m] >= delta[i]) {
            tmp <- rev(post[z.sort >= 0])
            tmp.ids <- which(tmp < delta[i])
            if (length(tmp.ids) == 0) 
                j2 <- ifelse(any(tmp == delta[i]), min(which(tmp == 
                  delta[i])), length(tmp))
            else j2 <- min(tmp.ids) - 1
            f2 <- if (length(vec.pos) == 0) 
                sum(1/probs[(m - j2 + 1):m] - 1)/B
            else vec.pos[m - j2 + 1]
            mat.delta[i, 5] <- z.sort[m - j2 + 1]
        }
        else {
            j2 <- 0
            f2 <- 0
            mat.delta[i, 5] <- Inf
        }
        mat.delta[i, 2] <- j1 + j2
        mat.delta[i, 3] <- min(1, p0 * (f1 + f2)/max(1, j1 + 
            j2))
    }
    mat.delta
}


plotEbam <- function (x, y, ...) 
{
    .local <- function (x, y, pos.stats = 2, sig.col = 3, sig.cex = 1, 
        pch = NULL, stats.cex = 0.8, main = NULL, xlab = NULL, 
        ylab = NULL, y.intersp = 1.3, ...) 
    {
        z <- x@z
        post <- x@posterior
        if (missing(y)) 
            y <- x@mat.fdr[, 1]
        if (length(y) != 1) 
            stop("More than one delta value has been specified.")
        mat.fdr <- compNumber1(x@z, x@posterior, x@p0, nrow(x@mat.samp), 
            delta = y, vec.pos = x@vec.pos, vec.neg = x@vec.neg)
        if (is.null(main)) 
            main <- paste("EBAM Plot for Delta =", y)
        if (is.null(xlab)) 
            xlab <- "z Value"
        if (is.null(ylab)) 
            ylab <- "Posterior"
        if (length(sig.col) > 1) 
            stop("sig.col must be of length 1.")
        ids <- which(z <= mat.fdr[, 4] | z >= mat.fdr[, 5])
        twosided <- any(z < 0)
        if (is.null(pos.stats)) 
            pos.stats <- 2
        if (!pos.stats %in% (0:4)) 
            stop("pos.stats must be an integer between 0 and 4.")
        if (length(ids) == 0) 
            plot(z, post, main = main, xlab = xlab, ylab = ylab, 
                pch = pch, ...)
        else {
            plot(z[-ids], post[-ids], main = main, xlab = xlab, 
                ylab = ylab, pch = pch, xlim = range(z), ylim = range(post), 
                ...)
            points(z[ids], post[ids], cex = sig.cex, col = sig.col, 
                pch = pch)
        }
        abline(h = y, lty = "dashed")
        if (pos.stats != 0) {
            tmp <- c("Significant:", "FDR:", "p0:", if (length(x@a0) == 
                1) "a0:", if (twosided) "Cutlow:", "Cutup:")
            tmp2 <- c(mat.fdr[, 2], round(mat.fdr[, 3], 3), round(x@p0, 
                3), if (length(x@a0) == 1) round(x@a0, 3), if (twosided) round(mat.fdr[, 
                4], 3), round(mat.fdr[, 5], 3))
            textLegend <- paste(tmp, tmp2, sep = "  ")
            where <- switch(pos.stats, "top", "bottomright", 
                "bottomleft", "topleft")
            legend(where, legend = textLegend, cex = stats.cex, 
                bty = "n", y.intersp = y.intersp)
        }
    }
    .local(x, y, ...)
}