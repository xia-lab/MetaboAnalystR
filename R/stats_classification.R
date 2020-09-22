#'Perform Random Forest Analysis
#'@description Perform Random Forest
#'@param mSetObj Input name of the created mSet Object
#'@param treeNum Input the number of trees to create, default is set to 500
#'@param tryNum Set number of tries, default is 7
#'@param randomOn Set random, default is 1
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RF.Anal <- function(mSetObj=NA, treeNum=500, tryNum=7, randomOn=1){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # set up random numbers
  if(is.null(mSetObj$dataSet$random.seeds)){
    mSetObj$dataSet$random.seeds <- GetRandomNumbers();
    mSetObj$dataSet$cur.inx <- 0;
    mSetObj$dataSet$rn.seed <- mSetObj$dataSet$random.seeds[1];
  }
  
  if(randomOn == -1){
    rn.sd <- 123456;
  }else if(randomOn == 0){ # keep current
    rn.sd <- mSetObj$dataSet$rn.seed;
  }else{ # random on
    cur.inx <- mSetObj$dataSet$cur.inx + 1;
    rn.sd <- mSetObj$dataSet$random.seeds[cur.inx];        
    mSetObj$dataSet$cur.inx <- cur.inx;
  }
  set.seed(rn.sd);
  # save the 
  mSetObj$dataSet$rn.seed <- rn.sd;
  
  rf_out <- randomForest::randomForest(mSetObj$dataSet$norm, mSetObj$dataSet$cls, ntree = treeNum, mtry = tryNum, importance = TRUE, proximity = TRUE);
  
  # set up named sig table for display
  impmat <- rf_out$importance;
  impmat <- impmat[rev(order(impmat[,"MeanDecreaseAccuracy"])),]
  sigmat <- impmat[,"MeanDecreaseAccuracy", drop=F];
  sigmat <- signif(sigmat, 5);
  
  fast.write.csv(sigmat, file="randomforests_sigfeatures.csv");
  mSetObj$analSet$rf <- rf_out;
  mSetObj$analSet$rf.sigmat <- sigmat;
  return(.set.mSet(mSetObj));
}

GetRandomNumbers <- function(){
  rm(.Random.seed);
  runif(1);
  return(.Random.seed[3:626]);
}

#'Plot Random Forest 
#'@description Random Forest plot 
#'@usage PlotRF.Classify(mSetObj, imgName, format, dpi, width)
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
PlotRF.Classify <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*5/8;
  
  mSetObj$imgSet$rf.cls <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  #par(mfrow=c(2,1));
  par(mar=c(4,4,3,2));
  cols <- rainbow(length(levels(mSetObj$dataSet$cls))+1);
  plot(mSetObj$analSet$rf, main="Random Forest classification", col=cols);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  legend("topright", legend = c("Overall", levels(cls)), lty=2, lwd=1, col=cols);
  
  #PlotConfusion(analSet$rf$confusion);
  
  dev.off();
  return(.set.mSet(mSetObj));
  
}

#'Plot Random Forest variable importance
#'@description Random Forest plot of variable importance ranked by MeanDecreaseAccuracy 
#'@usage PlotRF.VIP(mSetObj=NA, imgName, format, dpi, width)
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
PlotRF.VIP <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  vip.score <- rev(sort(mSetObj$analSet$rf$importance[,"MeanDecreaseAccuracy"]));
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
    
  }else{
    w <- width;    
  }
  h <- w*7/8;
  mSetObj$imgSet$rf.imp <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotImpVar(mSetObj, vip.score, "MeanDecreaseAccuracy");
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Plot Random Forest outliers
#'@description Random Forest plot of outliers
#'@usage PlotRF.Outlier(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
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
PlotRF.Outlier <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  cols <- GetColorSchema(cls);
  uniq.cols <- unique(cols);
  
  legend.nm <- unique(as.character(sort(cls)));
  dist.res <- randomForest::outlier(mSetObj$analSet$rf);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w*7/9;
  
  mSetObj$imgSet$rf.outlier <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), width=c(4,1));
  
  op <- par(mar=c(5,5,4,0));
  plot(dist.res, type="h", col=cols, xlab="Samples", xaxt="n", ylab="Outlying Measures", bty="n");
  
  # add sample names to top 5
  rankres <- rank(-abs(dist.res), ties.method="random");
  
  inx.x <- which(rankres < 6);
  inx.y <- dist.res[inx.x];
  nms <- names(dist.res)[inx.x];
  text(inx.x, inx.y, nms, pos=ifelse(inx.y >= 0, 3, 1), xpd=T)
  op <- par(mar=c(5,0,4,1));
  plot.new();
  plot.window(c(0,1), c(0,1));
  
  legend("center", legend =legend.nm, pch=15, col=uniq.cols);
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Recursive Support Vector Machine (R-SVM)
#'@description recursive SVM for feature selection and classification
#'@param mSetObj Input name of the created mSet Object
#'@param cvType Cross-validation type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RSVM.Anal <- function(mSetObj=NA, cvType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  ladder = CreateLadder(ncol(mSetObj$dataSet$norm));
  svm.out <- RSVM(mSetObj$dataSet$norm, mSetObj$dataSet$cls, ladder, CVtype=cvType);
  
  # calculate important features
  ERInd <- max(which(svm.out$Error == min(svm.out$Error)))
  MinLevel <- svm.out$ladder[ERInd]
  FreqVec <- svm.out$SelFreq[, ERInd]
  SelInd <- which(rank(FreqVec) >= (svm.out$ladder[1]-MinLevel));
  FreqInd <- svm.out$SelFreq[SelInd, ERInd]
  names(FreqInd) <- names(mSetObj$dataSet$norm)[SelInd];
  
  #create a sig table for display
  sig.var <- rev(sort(FreqInd));
  sig.var <- as.matrix(sig.var); # 1-column matrix
  colnames(sig.var) <- "Freqency";
  
  fast.write.csv(sig.var, file="svm_sigfeatures.csv");
  
  # add sorted features frequencies as importance indicator
  svm.out <- append(svm.out, list(sig.mat=sig.var, best.inx=ERInd));
  mSetObj$analSet$svm <- svm.out;
  return(.set.mSet(mSetObj));
}

#'Recursive Support Vector Machine (R-SVM) plot
#'@description Plot recursive SVM classification
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@usage PlotRSVM.Classification(mSetObj, imgName, format, dpi, width)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotRSVM.Classification <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$svm$Error;
  edge <- (max(res)-min(res))/100; # expand y uplimit for text
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$svm.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(res,type='l',xlab='Number of variables (levels)',ylab='Error Rate',
       ylim = c(min(res)-5*edge, max(res)+18*edge), axes=F,
       main="Recursive SVM classification")
  text(res,labels =paste(100*round(res,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  
  points(res, col=ifelse(1:length(res)==mSetObj$analSet$svm$best.inx,"red","blue"));
  axis(2);
  axis(1, 1:length(res), names(res));
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Recursive Support Vector Machine (R-SVM) plot of important variables
#'@description Plot recursive SVM variables of importance
#'if too many, plot top 15
#'@usage PlotRSVM.Cmpd(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
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
PlotRSVM.Cmpd <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  sigs <- mSetObj$analSet$svm$sig.mat;
  data <- sigs[,1];
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*7/8;
  
  mSetObj$imgSet$svm <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotImpVar(mSetObj, data, "Frequency");
  dev.off();
  return(.set.mSet(mSetObj));
}

#'R-code for R-SVM
#'@description use leave-one-out / Nfold or bootstrape to permute data for external CV
#'build SVM model and use mean-balanced weight to sort genes on training set
#'and recursive elimination of least important genes
#'@param Ntotal Total number
#'@param Nmin Minimum number, default set to 5
#'@author Dr. Xin Lu, Research Scientist
#'Biostatistics Department, Harvard School of Public Health
#'create a decreasing ladder for recursive feature elimination
#'
CreateLadder <- function(Ntotal, Nmin=5){
  x <- vector()
  x[1] <- Ntotal
  # note SVM is very computationally intensive, large step first 
  # first descend with 0.5 -> 50 var left
  # then descend with 0.6 -> 25 var left
  # then desend with 0.75 -> 5 var
  
  for( i in 1:100 ){
    if(x[i]>200){
      pRatio = 0.4
    }else if(x[i]>50){
      pRatio = 0.5
    }else if(x[i]>25){
      pRatio = 0.6
    }else{
      pRatio = 0.75
    }
    pp <- round(x[i] * pRatio)
    if( pp == x[i] ){
      pp <- pp-1
    }
    if( pp >= Nmin ) {
      x[i+1] <- pp
    } else{
      break
    }
  }
  x
}

#'R-SVM core code
#'@description Core code to perform R-SVM
#'@param x Row matrix of data
#'@param y Class label: 1 / -1 for 2 classes
#'@param ladder Input the ladder
#'@param CVtype Integer (N fold CV), "LOO" leave-one-out CV, "bootstrape" bootstrape CV
#'@param CVnum Number of CVs, LOO: defined as sample size, Nfold and bootstrape:  user defined, default as sample size
#'outputs a named list
#'Error: a vector of CV error on each level
#'SelFreq: a matrix for the frequency of each gene being selected in each level
#'with each column corresponds to a level of selection
#'and each row for a gene
#'The top important gene in each level are those high-freqent ones
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

RSVM <- function(x, y, ladder, CVtype, CVnum=0){
  ## check if y is binary response
  Ytype <- names(table(y))
  if(length(Ytype) != 2){
    print("ERROR!! RSVM can only deal with 2-class problem")
    return(0)
  }
  
  ## class mean
  m1 <- apply(x[ which(y==Ytype[1]), ], 2, mean)
  m2 <- apply(x[ which(y==Ytype[2]), ], 2, mean)
  md <- m1-m2
  
  yy <- vector(length=length(y))
  yy[which(y==Ytype[1])] <- 1
  yy[which(y==Ytype[2])] <- -1
  y <- yy
  
  ## check ladder
  if(min(diff(ladder)) >= 0){
    print("ERROR!! ladder must be monotonously decreasing")
    return(0);
  }
  
  if(ladder[1] != ncol(x) ){
    ladder <- c(ncol(x), ladder)
  }
  
  nSample <- nrow(x)
  nGene   <- ncol(x)
  SampInd <- seq(1, nSample)
  
  if(CVtype == "LOO"){
    CVnum <- nSample
  }else{
    if(CVnum == 0 ){
      CVnum <- nSample
    }
  }
  
  ## vector for test error and number of tests
  ErrVec <- vector(length=length(ladder))
  names(ErrVec) <- as.character(ladder);
  nTests <- 0
  
  SelFreq <- matrix(0, nrow=nGene, ncol=length(ladder))
  colnames(SelFreq) <- paste("Level", ladder);
  
  ## for each CV
  for(i in 1:CVnum){
    ## split data
    if(CVtype == "LOO"){
      TestInd <- i
      TrainInd <- SampInd[ -TestInd]
    }else{
      if(CVtype == "bootstrape"){
        TrainInd <- sample(SampInd, nSample, replace=T);
        TestInd <- SampInd[ which(!(SampInd %in% TrainInd ))];
      }else{
        ## Nfold
        TrainInd <- sample(SampInd, nSample*(CVtype-1)/CVtype);
        TestInd <- SampInd[ which(!(SampInd %in% TrainInd ))];
      }
    }
    
    nTests <- nTests + length(TestInd)
    
    ## in each level, train a SVM model and record test error
    xTrain <- x[TrainInd, ]
    yTrain <- y[TrainInd]
    
    xTest  <- x[TestInd,]
    yTest  <- y[TestInd]
    
    ## index of the genes used in the
    SelInd <- seq(1, nGene)
    for(gLevel in 1:length(ladder))
    {
      ## record the genes selected in this ladder
      SelFreq[SelInd, gLevel] <- SelFreq[SelInd, gLevel] +1
      
      ## train SVM model and test error
      ###################################################################################
      ## note the scale is changed to T or it never returns sometime for unscaled data ###
      ## note: the classification performance is idenpendent of about scale is T/F  #####
      ## for "LOO", the test data should be as.data.frame, matrxi will trigger error #####
      ###################################################################################
      svmres <- e1071::svm(xTrain[, SelInd], yTrain, scale=T, type="C-classification", kernel="linear" )
      if( CVtype == "LOO" ){
        svmpred <- predict(svmres, as.data.frame(xTest[SelInd], nrow=1) )
      }else{
        svmpred <- predict(svmres, xTest[, SelInd] )
      }
      ErrVec[gLevel] <- ErrVec[gLevel] + sum(svmpred != yTest )
      
      ## weight vector
      W <- t(svmres$coefs*yTrain[svmres$index]) %*% svmres$SV * md[SelInd]
      rkW <- rank(W)
      
      if( gLevel < length(ladder) ){
        SelInd <- SelInd[which(rkW > (ladder[gLevel] - ladder[gLevel+1]))]
      }
    }
  }
  ret <- list(ladder=ladder, Error=ErrVec/nTests, SelFreq=SelFreq);
  ret;
}

PlotConfusion <- function(clsConf){
  prior(clsConf) <- 100 
  # The above rescales the confusion matrix such that columns sum to 100.
  opar <- par(mar=c(5.1, 6.1, 2, 2))
  x <- x.orig <- unclass(clsConf)
  x <- log(x + 0.5) * 2.33
  x[x < 0] <- NA
  x[x > 10] <- 10
  diag(x) <- -diag(x)
  image(1:ncol(x), 1:ncol(x),
        -(x[, nrow(x):1]), xlab='Actual', ylab='',
        col=colorRampPalette(c(hsv(h = 0, s = 0.9, v = 0.9, alpha = 1), 
                               hsv(h = 0, s = 0, v = 0.9, alpha = 1), 
                               hsv(h = 2/6, s = 0.9, v = 0.9, alpha = 1)))(41), 
        xaxt='n', yaxt='n', zlim=c(-10, 10))
  axis(1, at=1:ncol(x), labels=colnames(x), cex.axis=0.8)
  axis(2, at=ncol(x):1, labels=colnames(x), las=1, cex.axis=0.8)
  title(ylab='Predicted', line=4.5)
  abline(h = 0:ncol(x) + 0.5, col = 'gray')
  abline(v = 0:ncol(x) + 0.5, col = 'gray')
  text(1:6, rep(6:1, each=6), labels = sub('^0$', '', round(c(x.orig), 0)))
  box(lwd=2)
  par(opar) # reset par
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Random Forest OOB
#'@description Get the OOB error for the last signif
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetRFOOB <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  errors = mSetObj$analSet$rf$err.rate;
  nrow = dim(errors)[1];
  signif(errors[nrow, 1],3);
}

#'Sig table for random forest analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.RF <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$rf.sigmat, "Random Forest", mSetObj$dataSet$type);
}

#'Random Forest Significance matrix
#'@description Significance measure, double brackets
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetRFSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$rf.sigmat))
}

GetRFSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$rf.sigmat);
}

GetRFSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$rf.sigmat);
}

#'Classification performance table for random forest analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetRFConf.Table <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  print(xtable::xtable(mSetObj$analSet$rf$confusion, 
               caption="Random Forest Classification Performance"), size="\\scriptsize");
}

#'Random Forest Confusion Matrix
#'@description Return double confusion matrix
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetRFConfMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  signif(mSetObj$analSet$rf$confusion,3);
}

GetRFConfRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$rf$confusion);
}

GetRFConfColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$rf$confusion);
}

#'Recursive Support Vector Machine (R-SVM) Significance Measure
#'@description Return significance measure, double[][]
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetSVMSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$svm$sig.mat));
}

GetSVMSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$svm$sig.mat);
}

GetSVMSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$svm$sig.mat);
}

#'Sig table for SVM
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.SVM <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$svm$sig.mat, "Recursive SVM", mSetObj$dataSet$type);
}