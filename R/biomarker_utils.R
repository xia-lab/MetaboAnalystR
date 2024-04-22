### Miscellaneous tasks for subsetting and training
### Subset data, train, rank
### Jeff Xia \email{jeff.xia@mcgill.ca}
### McGill University, Canada
### License: GNU GPL (>= 2)

#'Numbers for subset selection
#'@description Return a series of number for subsets selection
#'@param feat.len Input the feature length
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetFeatureNumbers <- function(feat.len){
  if(feat.len > 100){
    nFeatures <- c(5, 10, 15, 25, 50, 100);
  }else if(feat.len > 50){
    nFeatures <- c(3, 5, 10, 20, round(feat.len/2), feat.len);
  }else if(feat.len > 20){
    nFeatures <- c(2, 3, 5, 10, 20, feat.len);
  }else if(feat.len > 10){
    nFeatures <- c(2, 3, 5, 7, 10, feat.len);
  }else if(feat.len > 1){
    nFeatures <- sort(sample(2:feat.len));
  }else{
    print("Feature number is less than 2!")
    return();
  }
  nFeatures;
}

#'Make random partitions
#'@description Make random partitions, returns matrices indicating
#'whether the observation is in train/test for each run
#'note: try to get a balanced sampling for each group (classification)
#'or each quantile (regression). This is very useful for unbalanced data
#'@param y Input the data
#'@param propTraining By default set to 2/3
#'@param nRuns By default set to 30
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetTrainTestSplitMat <- function(y, propTraining = 2/3, nRuns = 30){
  
  nTotalSample <- length(y);
  
  smallestClass <- names(sort(table(y)))[1];
  nSmallest <- sum(y == smallestClass);
  
  nSmallestTrain <- round(propTraining * nSmallest);
  nBiggestTrain <- nSmallestTrain;
  
  nSmallestTest <- nSmallest - nSmallestTrain;
  nBiggestTest <- nTotalSample - (nSmallestTest + nSmallestTrain + nBiggestTrain);
  
  # sanity check for very large number of samples
  # for each run max 600 - 400 train, 200 test 
  big.samples <- FALSE;
  if(nSmallestTrain > 400){
    big.samples <- TRUE;
    nSmallestTrain <- nBiggestTrain <- 400;
    nSmallestTest <- nBiggestTest <- 200;
  }
  
  # split up in smallest class indices and biggest class indices
  smallestIndices <- which(y == smallestClass)
  biggestIndices <- seq(along = y)[-smallestIndices]
  
  nTrainingSample <- nSmallestTrain + nBiggestTrain;
  nTestSample <- nSmallestTest + nBiggestTest;
  
  trainingSampleAllRuns <- matrix(0, nrow = nRuns, ncol = nTrainingSample)
  testSampleAllRuns  <- matrix(0, nrow = nRuns, ncol = nTestSample);
  
  for (irun in 1:nRuns) {
    sampleSmallestTrain <- sample(smallestIndices, nSmallestTrain);
    sampleBiggestTrain <- sample(biggestIndices, nBiggestTrain);
    trainingSampleRun <- c(sampleSmallestTrain, sampleBiggestTrain);
    indicesTrainingSample <- rep(FALSE, length = nTotalSample);
    indicesTrainingSample[trainingSampleRun] <- TRUE;
    trainingSampleAllRuns[irun, ] <- which(indicesTrainingSample);
    if(big.samples){
      testSampleAllRuns[irun, ] <- sample(which(!indicesTrainingSample), 200);
    }else{
      testSampleAllRuns[irun, ] <- which(!indicesTrainingSample);
    }
  }
  
  # return the results
  list(
    training.mat = trainingSampleAllRuns,
    testing.mat = testSampleAllRuns
  );
}

#'To choose from two groups
#'@description Choose two groups (when more than two groups uploaded)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param grps Input the groups
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetCurrentGroups <- function(mSetObj=NA, grps){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(length(levels(mSetObj$dataSet$cls.all))>2){
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    # now extract the data for the two groups
    hit.inx <- as.character(mSetObj$dataSet$cls.all) %in% grp.nms;
    mSetObj$dataSet$cls <- factor(mSetObj$dataSet$cls.all[hit.inx]);
    mSetObj$dataSet$norm <- mSetObj$dataSet$norm.all[hit.inx, ];
  }else{
    mSetObj$dataSet$cls <- mSetObj$dataSet$cls.all;
    mSetObj$dataSet$norm <- mSetObj$dataSet$norm.all;
  }
  return(.set.mSet(mSetObj));
}

#'Rank features based on different importance measures
#'@description Ranks features based on various importance measures,
#'return imp.vec which contains the importance measures
#'of unordered features
#'@param x.in Input the X features
#'@param y.in Input the Y features
#'@param method Input the method
#'@param lvNum Input the number of levels
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

RankFeatures <- function(x.in, y.in, method, lvNum){
  if(method == "rf"){ # use randomforest mean decrease accuracy
    rf <- randomForest::randomForest(x = x.in,y = y.in,importance=TRUE, keep.forest=F);
    return(randomForest::importance(rf)[ ,"MeanDecreaseAccuracy"]);
  }else if (method == "pls"){
    ncls <- as.numeric(y.in)-1;
    datmat <- as.matrix(x.in);
    pls <- pls::plsr(ncls~datmat,method='oscorespls', ncomp=lvNum);
    return(Get.VIP(pls, lvNum));
  }else if(method == "svm"){
    svmres <- e1071::svm(x.in, y.in, type = 'C', kernel="linear");
    imp.vec <- (t(svmres$coefs) %*% svmres$SV)[1,]^2;
    names(imp.vec) <- colnames(x.in);
    return(imp.vec);
  }else if(method == "auroc"){ # univariate based ou area under ROC
    imp.vec <- caTools::colAUC(x.in, y.in, plotROC=F)[1,];
    return(imp.vec);
  }else if(method == "tt"){ # univariate based ou area under ROC
    imp.vec <- Get.Fstat(x.in, as.factor(y.in)); # all f-stats
    names(imp.vec) <- colnames(x.in);
    return(imp.vec);
  }else if(method == "fisher"){ # univariate based ou area under ROC
    imp.vec <- Get.Fisher(x.in, as.factor(y.in));
    names(imp.vec) <- colnames(x.in);
    return(imp.vec);
  }else{
    print("Not supported!");
    return(NULL);
  }
}

#'Calculates feature importance
#'@description Perform calculation of feature importance (AUC, p value, fold change)
#'@usage CalculateFeatureRanking(mSetObj=NA, clust.num=5)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param clust.num Numeric, input the number of clusters for cluster-analysis
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CalculateFeatureRanking <- function(mSetObj=NA, clust.num=5){

  mSetObj <- .get.mSet(mSetObj);
  LRConverged <<- "FALSE"; 
  
  x  <- mSetObj$dataSet$norm;
  y <- mSetObj$dataSet$cls;
  
  # auc
  auc <- caTools::colAUC(x, y, plotROC=F)[1,];
  if(.on.public.web & RequireFastUnivTests(mSetObj)){
     res <- PerformFastUnivTests(x, y);
     ttp <- res[,2];
  }else{
     ttp <- GetROCTtestP(x, y);
  }

  # fold change
  # use non-transformed data, then log2
  if(mSetObj$dataSet$use.ratio){
    data <- mSetObj$dataSet$proc.ratio;
  }else{
    data <- qs::qread("data_proc.qs");
  }
  # update in case norm filtered?
  hit.inxX <- rownames(data) %in% rownames(x);
  hit.inxY <- colnames(data) %in% colnames(x);
  data <- data[hit.inxX, hit.inxY, drop=FALSE];
  min.val <- min(abs(data[data!=0]))/2;
  #data <- log2((data + sqrt(data^2 + min.val))/2);
  
  m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), , drop=FALSE]);
  m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), , drop=FALSE]);
  #fc <- m1-m2;
  ratio <- m1/m2;
  ratio[ratio < 0] <- 0;
  fc <- signif(log2(ratio), 5);
  fc[is.infinite(fc) & fc < 0] <- -99;
  fc[is.infinite(fc) & fc > 0] <- 99;

  if(mSetObj$dataSet$roc_cols > 1){ # dont need to calc kmeans if only 1 metabolite!
    # now do k-means to measure their expression similarities

    kms <- ComputeKmeanClusters(t(x), clust.num);
    feat.rank.mat <- data.frame(AUC=auc, Pval=ttp, FC=fc, clusters = kms);
    rownames(feat.rank.mat) <- colnames(x);
    
    ord.inx <- order(feat.rank.mat$AUC, decreasing=T);
    feat.rank.mat  <- data.matrix(feat.rank.mat[ord.inx, , drop=FALSE]);
  }else{
    feat.rank.mat <- data.matrix(data.frame(AUC=auc, Pval=ttp, FC=fc, clusters=1))
  }
  
  # how to format pretty, and still keep numeric
  feat.rank.mat <<- signif(feat.rank.mat, digits = 5);
  
  if(mSetObj$analSet$mode == "univ"){
    fast.write.csv(feat.rank.mat, file="metaboanalyst_roc_univ.csv");
  }
  return(.set.mSet(mSetObj));  
}


# return a vector contain the cluster index 
ComputeKmeanClusters <- function(data, clust.num){
    set.seed(28051968);
    # if feature number is too low, change clust.num
    if(nrow(data) <= clust.num){
        clust.num <- nrow(data)-1;
    }
    if(clust.num < 2){
        clust.num <- 1;
    }
    kmeans.res <- kmeans(data, clust.num, nstart=100);
    return(kmeans.res$cluster);
}

# recomputing based on new cluster number
UpdateKmeans <- function(mSetObj=NA, clust.num=5){
  mSetObj <- .get.mSet(mSetObj);
  x  <- mSetObj$dataSet$norm;
  clsts <- ComputeKmeanClusters(t(x), clust.num);
    
  # note, need to synchronize the order as feat.rank.mat is order by AUC, not the same as in the x 
  ord.inx <- match(rownames(feat.rank.mat), names(clsts));
  feat.rank.mat[,"clusters"] <- clsts[ord.inx];
  feat.rank.mat <<- feat.rank.mat;
}

#'Set biomarker analysis mode
#'@description ROC utilities
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'@usage SetAnalysisMode(mSetObj, mode)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)s
#'@param mode Input the selected mode for biomarker analysis, "univ" for univariate ROC curve analysis,
#'"explore" for multivariate ROC curve analysis, and "test" for ROC curve based model creation and evaluation.
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SetAnalysisMode <- function(mSetObj=NA, mode){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$mode <- mode;
  return(.set.mSet(mSetObj));
}

#'Perform Monte-Carlo Cross Validation (MCCV)
#'@description Classification MCCV, aims to find the best feature 
#'subsets using default model parameters
#'@usage PerformCV.explore(mSetObj, cls.method, rank.method="auroc", lvNum=2, propTraining=2/3)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cls.method Select the classification method, "rf" for random forest classification, "pls" for PLS-DA,
#'and "svm" for support vector machine 
#'@param rank.method Select the ranking method, "rf" for random forest mean decrease accuracy, 
#'"fisher" for Fisher's univariate ranking based on area under the curve
#'"auroc" for univariate ranking based on area under the curve, "tt" for T-test univariate ranking based on area under the curve,
#'"pls" for partial least squares, and "svm" for support vector machine
#'@param lvNum Input the number of latent variables to include in the analyis, only for PLS-DA classification
#'@param propTraining Input the proportion of samples to use for training 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformCV.explore <- function(mSetObj=NA, cls.method, rank.method="auroc", lvNum=2, propTraining=2/3){
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$analSet$exp.method <- cls.method;
  mSetObj$analSet$rank.method <- rank.method;
  mSetObj$analSet$exp.lvNum <- lvNum;

  data <- mSetObj$dataSet$norm;
  cls <- mSetObj$dataSet$cls;
  
  # number of subsampling to produce smooth curve
  if(nrow(data) > 500){
    nRuns <- 10;
  }else if(nrow(data) > 200){
    nRuns <- 20;
  }else if(nrow(data) > 100){
    nRuns <- 30;
  }else{
    nRuns <- 50;
  }
  
  nFeatures <- GetFeatureNumbers(ncol(data));
  
  feat.outp <- actualCls <- vector(length = nRuns, mode = "list");
  
  perf.outp <- vector(length = length(nFeatures), mode = "list");
  perf.outp <- lapply(perf.outp, function(x){x <- vector(length = nRuns, mode = "list"); return(x)});
  auc.mat <- accu.mat <- matrix(nrow=nRuns, ncol=length(nFeatures));
  
  splitMat <- GetTrainTestSplitMat(cls, propTraining, nRuns);
  trainRuns <- splitMat$training.mat;
  testRuns <- splitMat$testing.mat;
  
  for (irun in 1:nRuns){
    trainingSampleRun <- trainRuns[irun, ]
    testSampleRun <- testRuns[irun, ];
    x.in <- data[trainingSampleRun, ];
    y.train <- cls[trainingSampleRun];
    actualCls[[irun]] <- y.test <- cls[testSampleRun];
    
    # Feature ranking using only training data, 
    imp.vec <- RankFeatures(x.in, y.train, rank.method, lvNum);
    
    feat.outp[[irun]] <- imp.vec;
    ord.inx <- order(imp.vec, decreasing = TRUE);
    ordData <- data[, ord.inx];
    
    # buliding classifiers for each number of selected features and test on the test data
    for (inum in seq(along = nFeatures)){
      x.train <- ordData[trainingSampleRun, 1:nFeatures[inum]];
      x.test <- ordData[testSampleRun, 1:nFeatures[inum]];
      prob.out <- Predict.class(x.train, y.train, x.test, cls.method, lvNum);
      
      # calculate AUC for each
      pred <- ROCR::prediction(prob.out, y.test);
      auc.mat[irun, inum] <- slot(ROCR::performance(pred, "auc"), "y.values")[[1]];
      
      perf.outp[[inum]][[irun]] <- prob.out;
      pred.out <- as.factor(ifelse(prob.out > 0.5, 1, 0));
      accu.mat[irun, inum] <- Get.Accuracy(table(pred.out, y.test));
    }
  }
  
  #############################################################################
  ## prepare results for default plotting 
  ## 1) get best model based on AUROC for prob.view and imp.feature calculation
  ## 2) calculate accuracy and roc for all models under comparison
  #############################################################################
  
  preds <- vector(length = length(nFeatures), mode = "list");
  act.vec <- unlist(actualCls); # same for all subsets
  for(m in 1:length(nFeatures)){
    prob.vec <- unlist(perf.outp[[m]]);
    pred <- ROCR::prediction(prob.vec, act.vec);
    preds[[m]] <- pred; # prediction obj
    #auc.vec[m] <- slot(performance(pred, "auc"), "y.values")[[1]];
  }
  
  # accu.mat and preds are for all models
  colnames(accu.mat) <- colnames(auc.mat) <- names(preds) <- paste(nFeatures, sep = "");
  auc.vec <- colMeans(auc.mat);
  
  auc.cis <- GetCIs(auc.mat);
  # get the best based on AUROC
  best.model.inx <- which.max(auc.vec);
  
  mSetObj$analSet$multiROC <- list(
    type = mSetObj$analSet$type, # note, do not forget to carry the type "roc"
    train.mat = trainRuns,
    
    # record raw output
    test.feats = nFeatures,
    pred.cv = perf.outp, 
    true.cv = actualCls, 
    imp.cv = feat.outp,
    
    # record processed output
    pred.list = preds,
    accu.mat = accu.mat,
    auc.vec = auc.vec,
    auc.ci = auc.cis,
    best.model.inx = best.model.inx
  )
  return(.set.mSet(mSetObj));
}

#'Perform MCCV for manually selected features
#'@description MCCV for manually selected features (no additional feature selection)
#'@usage PerformCV.test(mSetObj, method, lvNum, propTraining=2/3, nRuns=100)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Select the classification method, "rf" for random forest classification, "pls" for PLS-DA,
#'and "svm" for support vector machine 
#'@param lvNum Input the number of latent variables to include in the analyis, only for PLS-DA classification  
#'@param propTraining Input the proportion of samples to use for training, by default it is 2/3 
#'@param nRuns Input the number of MCCV runs, by default it is 100 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformCV.test <- function(mSetObj=NA, method, lvNum, propTraining=2/3, nRuns=100){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$tester.method <- method;
  mSetObj$analSet$tester.lvNum <- lvNum;
  data <- mSetObj$dataSet$norm;
  cls <- mSetObj$dataSet$cls;    
  
  if(method == "lr") {
    # check cls (response variable) whether it is number 0/1 or not
    if( length(levels(mSetObj$dataSet$cls)) == 2 ) {
      mSetObj$dataSet$cls.lbl <- levels(mSetObj$dataSet$cls);  # already sorted
      cls <- as.numeric(mSetObj$dataSet$cls)-1;
      mSetObj$dataSet$cls.lbl.new <- sort(unique(cls));
      mSetObj$dataSet$cls01 <- cls; # integer values for response of logistic regression
    } else {
       AddErrMsg("level of response variable y/class has more than 2!");
       return(0);
    }
  } else {
    if(method == "pls"){# make sure the feature # > comp #
        if(lvNum > ncol(data)){
            AddErrMsg("PLS components cannot be more than total features selected!");
            return(0);
        }
    }
    cls <- mSetObj$dataSet$cls;
  }
  
  splitMat <- GetTrainTestSplitMat(cls, propTraining, nRuns);
  trainRuns <- splitMat$training.mat;
  testRuns <- splitMat$testing.mat;
  
  feat.outp <- actualCls <- perf.outp <- vector(length = nRuns, mode = "list");
  auc.vec <- accu.vec <- vector(mode="numeric", length=nRuns);
  
  for (irun in 1:nRuns){
    trainingSampleRun <- trainRuns[irun, ]
    testSampleRun <- testRuns[irun, ];
    x.train <- data[trainingSampleRun, ,drop=F];
    x.test <- data[testSampleRun, ,drop=F];
    y.train <- cls[trainingSampleRun];
    actualCls[[irun]] <- y.test <- cls[testSampleRun];
    
    res <- Predict.class(x.train, y.train, x.test, method, lvNum, imp.out=T);
    feat.outp[[irun]] <- res$imp.vec;
    prob.out <- res$prob.out;
    
    # calculate AUC for each
    pred <- ROCR::prediction(prob.out, y.test);
    auc.vec[irun] <- slot(ROCR::performance(pred, "auc"), "y.values")[[1]];
    perf.outp[[irun]] <- prob.out;
    pred.out <- as.factor(ifelse(prob.out > 0.5, 1, 0));
    accu.vec[irun] <- Get.Accuracy(table(pred.out, y.test));
  }
  
  #############################################################################
  ## prepare results for default plotting
  ## 1) get best model based on AUROC for prob.view and imp.feature calculation
  ## 2) calculate accuracy and roc for all models under comparison
  #############################################################################
  
  prob.vec <- unlist(perf.outp);
  act.vec <- unlist(actualCls);
  preds <- ROCR::prediction(prob.vec, act.vec);
  auc <- mean(auc.vec);
  auc.ci <- GetCIs(as.matrix(auc.vec));
  
  #########################################
  # if there is holdout sample, do prediction
  if(!is.null(mSetObj$dataSet$test.data)){
    test.res <- Predict.class(data, cls, mSetObj$dataSet$test.data, method, lvNum);
  }else{
    test.res <- NULL;
  }
  #######################
  #########################################
  # if there is new samples, do prediction
  if(!is.null(mSetObj$dataSet$new.data)){
    new.res <<- Predict.class(data, cls, mSetObj$dataSet$new.data, method, lvNum);
  }else{
    new.res <- NULL;
  }
  
  #########################################
  # method = Logistic Regression
  # generate report tables for model with 10-fold Cross-Validation
  if( method == "lr") {
    x.train.all <- data;
    x.test.all  <- data;
    y.train.all <- as.character(cls);
    y.test.all  <- as.character(cls);
    
    # generate LR model and AUROC statistics. Then assign to the analSet list
    # return (list(model = mdlSummary, LR.auc=LR.auc));
    
    tbl.cls <- table(cls);
    if( (tbl.cls[[1]] < 10) & (tbl.cls[[2]] < 10) ) {
       AddErrMsg("The sample size of each group should be more than 10!");
       return (0);
    } else {
      doLogisticRegMdl(x.train.all, y.train.all, x.test.all, y.test.all);
    }
  }
  #######################
  if(!is.null(mSetObj$analSet$ROCtest$perm.res)){
    mSetObj$analSet$ROCtest <-  list(
      type = mSetObj$analSet$type,  # note, do not forget to carry the type "roc" when overwrite analSet
      train.mat = trainRuns,
      pred.cv = perf.outp,
      true.cv = actualCls,
      imp.cv = feat.outp,
      perm.res = mSetObj$analSet$ROCtest$perm.res,
      # record processed output
      pred.list = preds,
      accu.mat = t(as.matrix(accu.vec)),
      auc.ci = auc.ci,
      auc.vec = auc,
      test.res = test.res,
      new.res = new.res
    );

  } else {
    mSetObj$analSet$ROCtest <-  list(
      type = mSetObj$analSet$type,  # note, do not forget to carry the type "roc" when overwrite analSet
      train.mat = trainRuns,
      pred.cv = perf.outp,
      true.cv = actualCls,
      imp.cv = feat.outp,

      # record processed output
      pred.list = preds,
      accu.mat = t(as.matrix(accu.vec)),
      auc.ci = auc.ci,
      auc.vec = auc,
      test.res = test.res,
      new.res = new.res
    );
  }

  return(.set.mSet(mSetObj));
}

#'Get predicted class probability 
#'@description Get predicted class probability
#'@param x.train Input the x training samples
#'@param y.train Input the y training samples
#'@param x.test Input the x testing samples
#'@param clsMethod Se the classification method, default is set to pls
#'@param lvNum Input the number of levels
#'@param imp.out Logical, set to F by default
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
Predict.class <- function(x.train, y.train, x.test, clsMethod="pls", lvNum, imp.out=F){
  
  # first convert class label to 0/1 so convert prob-> class will be easier
  y.train <- as.factor(as.numeric(y.train)-1);
  
  # note, we only need prob for class 1, pred can be inferred
  if (clsMethod == "rf"){
    model <- randomForest::randomForest(x.train, y.train, ntree=300, importance=T);
    prob.out <- predict(model, x.test, type="prob")[,"1"];
    if(imp.out){
      imp.vec <- randomForest::importance(model)[ ,"MeanDecreaseAccuracy"];
      return(list(imp.vec = imp.vec, prob.out = prob.out));
    }
    return(prob.out);
  }else if(clsMethod == "pls"){ # plsda
    model <- caret::plsda(x.train, y.train, method='oscorespls', ncomp=ifelse(ncol(x.train)>lvNum, lvNum, 2));
    prob.out <- predict(model, x.test, type="prob")[,"1",1];
    if(imp.out){
      imp.vec <- Get.VIP(model, lvNum);
      return(list(imp.vec = imp.vec, prob.out = prob.out));
    }
    return(prob.out);
  }else if(clsMethod == "lr"){ # logistic regression with selected variables (only in Test)
    x <- x.train;
    y <- y.train;
    xx.test <- x.test;
    
    names.x.origin <- names(x);
    names(x) <- paste0("V", 1:(ncol(x)));
    names(xx.test) <- names(x);
    
    data.xy <- data.frame(y, x);
    model <- logisticReg(data.xy);
    prob.out <- predict(model, xx.test, type="response");
    if(imp.out){
      imp.vec <- names(model$coefficients)[-1]
      return(list(imp.vec = imp.vec, prob.out = prob.out));
    }
    return(prob.out);
  }else{ # svm
    model <- e1071::svm(x.train, y.train, type = 'C', kernel="linear", probability=TRUE);
    prob.out <- attr(predict(model, x.test,  probability=TRUE), "probabilities")[,"1"];
    if(imp.out){
      imp.vec <- (t(model$coefs) %*% model$SV)[1,]^2;
      names(imp.vec) <- colnames(x.train);
      return(list(imp.vec = imp.vec, prob.out = prob.out));
    }
    return(prob.out);
  }
}

logisticReg <- function(d.xy) {
  fmla <- as.formula(paste("y ~", paste(names(d.xy)[-1], collapse="+")));
  model <- glm(fmla, data=d.xy, family=binomial(link="logit"), na.action="na.omit")
  return(model);
}

genLREquation <- function(coef.mdl){
  coef.mdl <- round(coef.mdl, 3);
  
  eq <- coef.mdl[[1]];
  for(i in 2:length(coef.mdl)) {
    eq <- paste(eq, ifelse(sign(coef.mdl[[i]])==1,"+","-"), abs(round(coef.mdl[[i]],3)), names(coef.mdl)[i]);
  }
  return(eq);
}

#'Calculate ROC performance with CV
#'An internal function called by PerformCV.test
#'@description Calculate ROC performance with CV
#'@param data.in Input matrix of data
#'@param fmla.in Input for generalized linear model
#'@param kfold Numeric
#'@param run.stepwise Logistic Regression
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

.do.CVTest.LRmodel <- function(data.in, fmla.in, kfold=10, run.stepwise=FALSE){


  dw.case <- data.in[which(data.in$y == 1), ]; nrow(dw.case)
  dw.ctrl <- data.in[which(data.in$y == 0), ]; nrow(dw.ctrl)
  
  random.seed <- 10063927;
  grpIndice.case <- createCVset(nrow(dw.case), kfold, random.seed)
  grpIndice.ctrl <- createCVset(nrow(dw.ctrl), kfold, random.seed)
  
  all.trainning.y <- NULL
  all.trainning.fit <- NULL
  all.validation.y <- NULL
  all.validation.fit <- NULL
  
  for (i in 1:kfold) 
  {
    d.train.case <- dw.case[-grpIndice.case[[i]], ]; nrow(d.train.case)
    d.train.ctrl <- dw.ctrl[-grpIndice.ctrl[[i]], ]; nrow(d.train.ctrl)
    d.train <- rbind(d.train.case, d.train.ctrl); names(d.train)
    
    d.validation.case <- dw.case[grpIndice.case[[i]], ]; nrow(d.validation.case)
    d.validation.ctrl <- dw.ctrl[grpIndice.ctrl[[i]], ]; nrow(d.validation.ctrl)
    d.validation <- rbind(d.validation.case, d.validation.ctrl)  
    
    vnames <- names(d.train); 
    
    mdl <- glm(fmla.in, data=d.train, family=binomial(link="logit"))
    if(run.stepwise) { mdl <- step(mdl) }
    
    dval.pred <- predict(mdl, d.validation, type="response");  
    
    all.validation.y <- c(all.validation.y, d.validation$y)
    all.validation.fit <- c(all.validation.fit, dval.pred)
    all.trainning.y <- c(all.trainning.y, mdl$y)
    all.trainning.fit <- c(all.trainning.fit, mdl$fitted.values)
  }
  
  # 10-fold cross validation
  cv.r <- pROC::roc(all.validation.y ~ all.validation.fit, ci=T, ci.se=T, sp=seq(0,1,0.01)) # of: se (sensitivity), sp (specificity), thresholds, auc 
  # cv.threshold <- coords(cv.r, "best", ret=c("threshold"), best.method="youden", best.weights=c(5, nrow(dw.case)/nrow(data.in)) ); 
  cv.threshold <- as.numeric(pROC::coords(cv.r, "best", ret=c("threshold"), best.method="closest.topleft", transpose=TRUE)); 
  cv.rstat <- multi.stat(all.validation.fit > cv.threshold, all.validation.y);
  if(length(cv.rstat) == 1 && cv.rstat == 0){
    return(0);
  }
  cv.tbl <- table(all.validation.fit > cv.threshold, all.validation.y);
  
  # training performance
  train.r <- pROC::roc(all.trainning.y ~  all.trainning.fit, ci=T, ci.se=T, sp=seq(0,1,0.01)) # of: se (sensitivity), sp (specificity), thresholds, auc 
  train.threshold <- pROC::coords(train.r, "best", ret=c("threshold"), best.method="youden", transpose=TRUE); 
  train.rstat <- multi.stat(all.trainning.fit > cv.threshold, all.trainning.y) 
  if(length(train.rstat) == 1 && train.rstat == 0){
    return(0);
  } 
  return(list(
    train.r = train.r$ci,
    train.threshold = train.threshold,
    train.rstat = train.rstat,
    cv.robj = cv.r,
    cv.r = cv.r$ci,
    cv.threshold = cv.threshold,
    cv.rstat = cv.rstat,
    cv.tbl = cv.tbl,
    cv.y = all.validation.y,
    cv.pred = all.validation.fit,
    cv.threshold = cv.threshold
  ));
}

#'Develop a Logistic Regression Model with all of the combined k-fold CV subsets
#'@description Develop a Logistic Regression Model with all of the combined k-fold CV subsets
#'@param x.train Input the X training set
#'@param y.train Input the Y training set
#'@param x.test Input the X test set
#'@param y.test Input the Y test set
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
doLogisticRegMdl <- function(x.train, y.train, x.test, y.test){
  
  # use 10-fold CV as default; or LOOCV if sample size < 10
  x <- x.train;
  y <- y.train;
  x.cv.test <- x.test;
  y.cv.test <- y.test;
  
  names.x.origin <- names(x);
  names(x) <- paste0("V", 1:(ncol(x)));
  names(x.cv.test) <- names(x);

  # glm can only take numeric + factor
  if(class(y) == "character"){
    y <- as.factor(y)
  }
  
  # LR Model generation
  data.xy <- data.frame(y, x);
  fmla <- as.formula(paste("y ~", paste(names(data.xy)[-1], collapse="+")));
  
  model <- glm(fmla, data=data.xy, family=binomial(link="logit"), na.action="na.omit");
  
  # if model with selected variables is failed, the use the stepwise variable selection
  if((!model$converged) | (model$deviance < 1)){
    model <- step(model, trace=0);
    fmla <- model$formula;
  }
  
  mdlSummary <- summary(model)$coefficients;
  dimnames(mdlSummary)[[1]][-1] <- names.x.origin;
  # prob.out <- predict(model, x.cv.test, type="response");
  
  Odds <- round(exp(cbind(OR=coef(model), confint(model))), 2)[,1];
  # Odds <- round(exp(OR=coef(model)), 2);
  Odds[1] <- "-";
  LRmodel <- cbind(round(mdlSummary,3), Odds);
  LRmodel[,4] <- ifelse(LRmodel[,4] < 0.001, "< 0.001", LRmodel[,4]);
  
  # LR model's summary table as html format
  LRmodel.xtable <<- print(xtable::xtable(LRmodel, align="r|rrrcc"),
                           type = "html", print.results=F, xtable.width=600,
                           html.table.attributes="border=1 width=600" );
  
  coefTable <- coef(model);
  names(coefTable)[-1] <- names.x.origin;
  if (model$converged & model$deviance > 1) {
    LReq <<- genLREquation(coefTable);
    LRConverged <<- "TRUE";
  } else {
    # (Error: Model was not converged)
    LReq <<- "(Error: Model was not converged)";
    LRConverged <<- "FALSE"; 
  }
  
  # ROC analysis with 10-fold CV test set   
  rStat <- .do.CVTest.LRmodel(data.xy, fmla.in=fmla, kfold=10, run.stepwise=FALSE)     
  # r <- roc(y.cv.test ~ prob.out, ci=T, ci.se=T, sp=seq(0,1,0.01)) # of: se (sensitivity), sp (specificity), thresholds, auc
  
  # ROC plot with k-fold CV test set for ROC plot
  LR.r <<- rStat$cv.robj;
  LR.y.origin <<- rStat$cv.y;
  LR.y.pred <<- rStat$cv.pred;
  LR.threshold <<- rStat$cv.threshold;
  
  # training/discovery; 10-fold CV
  auc  <- c( sprintf("%.3f (%.3f ~ %.3f)", rStat$train.r[2], rStat$train.r[1], rStat$train.r[3]),
             sprintf("%.3f (%.3f ~ %.3f)", round(rStat$cv.r[2],3), round(rStat$cv.r[1],3), round(rStat$cv.r[3],3)) );
  sens <- c( sprintf("%.3f (%.3f ~ %.3f)", rStat$train.rstat$sens, rStat$train.rstat$sensCI[1], rStat$train.rstat$sensCI[2]),
             sprintf("%.3f (%.3f ~ %.3f)", rStat$cv.rstat$sens, rStat$cv.rstat[1], rStat$cv.rstat$sensCI[2]) );
  spec <- c( sprintf("%.3f (%.3f ~ %.3f)", rStat$train.rstat$spec, rStat$train.rstat$specCI[1], rStat$train.rstat$specCI[2]),
             sprintf("%.3f (%.3f ~ %.3f)", rStat$cv.rstat$spec, rStat$cv.rstat$specCI[1], rStat$cv.rstat$specCI[2]) );
  
  LRperf <- cbind(auc, sens, spec);
  colnames(LRperf) <- c("AUC","Sensitivity","Specificity");
  rownames(LRperf) <- c("Training/Discovery","10-fold Cross-Validation");
  
  LRperf.xtable <<- print(xtable::xtable(LRperf, align="r|ccc"), include.rownames=TRUE, floating=FALSE,
                          type = "html", print.results=F, xtable.width=600,
                          html.table.attributes="border=1 width=600");
}

#'Plot ROC for the logistic regression model
#'@description Plot ROC for the logistic regression model
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param show.conf Logical, show confidence intervals
#'@param sp.bin Numeric, default is set to 0.01. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#' @importFrom pROC smooth plot.roc
#' @importFrom Cairo Cairo
#'@export
#'
PlotROC.LRmodel <- function(mSetObj=NA, imgName, format="png", dpi=72, show.conf=FALSE, sp.bin=0.01) {

  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- h <- 8;
  mSetObj$imgSet$roc.lr <- imgName;
  
  roc.object <- LR.r;
  y.origin <- LR.y.origin;
  y.pred <- LR.y.pred;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op <- par(mar=c(5,4,3,3));
  
  # auc.ci <- ci.auc(roc.object, method="bootstrap", boot.n=500, progress="none");
  auc.lbl <- paste("Area under the curve (AUC) = ", round(roc.object$ci[2],3), sep="");
  ci.lbl  <- paste("95% CI: ", round(roc.object$ci[1],3), " - ", round(roc.object$ci[3],3),sep="");
  perform.lbl <- paste(auc.lbl,"\n", ci.lbl, sep="");
  
  # plot ROC of specific model and save the table for details
  if(show.conf){
    # of: se (sensitivity), sp (specificity), thresholds, auc
    r.new <- roc(y.origin ~ y.pred, ci=T, of="se", sp=seq(0,1,sp.bin));
    pROC::plot.roc(r.new, add=F, ci.type="bars", ci.col="green", col="darkolivegreen", lty="dotted", legacy.axes=T,
             xlab="1 - Specificity (False positive rate)", ylab="Sensitivity (True positive rate)",
             cex.axis=1.0, cex.lab=1.0);
    pROC::plot.roc(pROC::smooth(r.new, method="density", n=512), add=T, col="blue", lwd=1);
    legend("bottomright", legend=c("Empirical","Smoothed", "95% CI"), cex=1.0, col=c(par("fg"), "blue", "green"), lwd=3, lty=c(3,1,1) );
    
    text(0.7, 0.4, perform.lbl, adj=c(0,1), col="black", cex=1.0);
  } else {
    pROC::plot.roc(roc.object, add=F, ci.type="no", col="blue", legacy.axes=T,
             xlab="1 - Specificity (False positive rate)", ylab="Sensitivity (True positive rate)",
             cex.axis=1.0, cex.lab=1.0, lwd=2, lty=1 );
    text(0.7, 0.4, perform.lbl, adj=c(0,1), col="black", cex=1.0);
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
}


#'Get multiple category statistics
#'@description Get multiple category statistics
#'@param pred Input predictions
#'@param resp Input responses
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
multi.stat <- function(pred, resp) {
  
  in.dat <- na.omit(cbind(pred,resp));
  
  if(nrow(in.dat) < length(pred)/2){ # abort if over half is invalid
    AddErrMsg("More than half of tests return NA! Insufficent sample size?");
    return(0);
  }
    
  pred <- in.dat[,1];
  resp <- in.dat[,2];

  ts <- table(pred, resp)
  
  TP <- ts[2,2]
  FP <- ts[2,1]  # identify as True/Positive  but actually Negative/False (healthy)
  FN <- ts[1,2]  # identify as False/Negative but actually Positive/True
  TN <- ts[1,1]
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (FP + TN)
  ppv <- TP / (TP + FP)
  npv <- TN / (FN + TN)
  likelihood_ratio_pos <- sensitivity / (1 - specificity) # positive
  likelihood_ratio_neg <- (1-sensitivity) / specificity  # positive
  # (TP + TN) / (TP + TN + FP + FN)
  accuracy <- (TP + TN) / sum(ts)
  # fdr = fp / (fp + tp) : false discovery rate
  fdr <- FP / (FP + TP)
  
  SEsens <- sqrt(sensitivity * (1-sensitivity) / (TP + FN) )
  SEspec <- sqrt(specificity * (1-specificity) / (FP + TN) )
  
  cise.u <- sensitivity + 1.96 * SEsens
  cisp.u <- specificity + 1.96 * SEspec
  CIsens <- c(sensitivity - 1.96 * SEsens, ifelse( cise.u > 1.0, 1.0, cise.u) )
  CIspec <- c(specificity - 1.96 * SEspec, ifelse( cisp.u > 1.0, 1.0, cisp.u) )
  
  return(list(sens=sensitivity, sensCI=CIsens, spec=specificity, specCI=CIspec) );
}

#'Get confidence intervals
#'@description For non-parametric tests, use quantiles,
#'use normal (1.96*std.err) if parametric
#'@param data Input data matrix
#'@param param Logical, False by default 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetCIs <- function(data, param=F){
  means <- colMeans(data, na.rm=T);
  if(param){
    sds <- apply(data, 2, sd, na.rm=T);
    nn <- nrow(data);
    std.err <- sds/sqrt(nn);
    LCL <- round(means-1.96*std.err,3);
    LCL[LCL<0] <- 0;
    UCL <- round(means+1.96*std.err, 3);
    UCL[UCL>1] <- 1;
    res <- paste(LCL, "-", UCL, sep="");
  }else{
    cis <- apply(data, 2, quantile, probs=c(0.025, 0.975));
    cis <- round(cis,3);
    cis[cis<0] <- 0;
    cis[cis>1] <- 1;
    res <- paste(cis[1,], "-", cis[2,], sep="");
  }
  res;
}

#'Perform Classical Univariate ROC
#'@description Perform Classical Univariate ROC
#'@usage Perform.UnivROC(mSetObj=NA, feat.nm, version, 
#'format="png", dpi=72, isAUC, isOpt, optMethod, isPartial, measure, cutoff)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param feat.nm Input the name of the feature to perform univariate ROC analysis
#'@param version image version mark, can be any character
#'@param format Select the image format, png, of pdf. 
#'@param dpi Input the dpi. If the image format is pdf, users need not define the dpi. For png images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300. 
#'@param isAUC Logical, select T to compute the 95 percent confidence interval band and "F" to not 
#'@param isOpt Logical, show the optimal cutoff, T to show it and F to not 
#'@param optMethod Select the optimal cutoff by using either closest.topleft for closest to top-left corner or 
#'youden for farthest to the diagonal line (Youden) 
#'@param isPartial Logical, input T to calculate a partial ROC curve, and F to not
#'@param measure Select the parameter to limit the calculation of the partial ROC curve, 
#'se for the X-axis (maximum false-positive rate)
#'and sp for the Y-axis, representing the minimum true positive-rate
#'@param cutoff Input the threshold to limit the calculation of the partial ROC curve, 
#'the number must be between 0 and 1.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Perform.UnivROC <- function(mSetObj=NA, feat.nm, 
                            version, format="png", 
                            dpi=72, isAUC, isOpt, 
                            optMethod, isPartial, measure, cutoff){

  mSetObj <- .get.mSet(mSetObj);
  
  imgName <- mSetObj$dataSet$url.var.nms[feat.nm];
  imgName = paste("roc_univ_", imgName, "_", version, "_dpi", dpi, ".", format, sep="");

  data_ori_norm <- mSetObj$dataSet$norm
  if(!is.null(mSetObj$dataSet$norm.orig)){
    data_ori_norm <- mSetObj$dataSet$norm.orig
  }
  
  x <- data_ori_norm[, feat.nm];
  y <- mSetObj$dataSet$cls;
  
  if(isPartial){
    if(measure == "se"){
      cutoff = cutoff;
    }else{
      cutoff = 1-cutoff;
    }
    roc.obj <- pROC::roc(y, x, partial.auc=c(1.0, cutoff), ci=TRUE, partial.auc.focus=measure, boot.n=50, percent = F, progress="none");
  }else{
    roc.obj <- pROC::roc(y, x, percent = F);
  }
  
  w <- h <- 6; 
  
  if(length(mSetObj$imgSet$roc.univ.name)==0){
    mSetObj$imgSet$roc.univ.name <- feat.nm;
    mSetObj$imgSet$roc.univ.plot <- imgName;
  } else {
    if(feat.nm %in% mSetObj$imgSet$roc.univ.name){
        idx <- which(feat.nm %in% mSetObj$imgSet$roc.univ.name);
        mSetObj$imgSet$roc.univ.name[idx] <- feat.nm;
        mSetObj$imgSet$roc.univ.plot[idx] <- imgName;
    } else {
        mSetObj$imgSet$roc.univ.name <- c(mSetObj$imgSet$roc.univ.name, feat.nm);
        mSetObj$imgSet$roc.univ.plot <- c(mSetObj$imgSet$roc.univ.plot, imgName);
    }   
  }
  #mSetObj$imgSet$roc.univ.plot <- imgName;
  #mSetObj$imgSet$roc.univ.name <- feat.nm;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(oma=c(0,0,1,0));
  par(mar=c(4,4,4,4)+.1);
  
  opt.thresh = NA;
  
  # first plot ROC curve
  if(isAUC){
    pROC::plot.roc(roc.obj, print.auc=F, legacy.axes=TRUE, col="navy", grid=T,
             xlab = "False positive rate", ylab="True positive rate", main=feat.nm);
    ci.obj <- pROC::ci.se(roc.obj, specificities=seq(0, 1, 0.05), boot.n=200, progress="none");
    ROCR::plot(ci.obj,type="shape",col="#0000ff22");
  }else{
    pROC::plot.roc(roc.obj, print.auc=F, legacy.axes=TRUE, col="navy", grid=T,
             xlab = "False positive rate", ylab="True positive rate",
             auc.polygon=TRUE, auc.polygon.col="#0000ff22", main=feat.nm);
  }
  
  auc.ci <- pROC::ci.auc(roc.obj, method="bootstrap", boot.n=500, progress="none");
  roc.obj$ci <- auc.ci;
  auc.lbl <- paste("AUC: ", round(roc.obj$ci[2],3), sep="");
  ci.lbl <- paste("(", round(roc.obj$ci[1],3), "-", round(roc.obj$ci[3],3), ")", sep="");
  text(0.5, 0.5, paste(auc.lbl, "\n", ci.lbl, sep=""), adj=c(0,1), col="navy");
  
  if(isOpt){
    par(xpd=T);
    opt.ps <- data.frame(pROC::coords(roc.obj, "best", best.method=optMethod, transpose = TRUE));
    opt.thresh <- opt.ps["threshold",]
    points(opt.ps["specificity",], opt.ps["sensitivity",], pch=19, col="red");
    lbls=paste(signif(opt.ps["threshold",],3), "(", round(opt.ps["specificity",],1), ", ", round(opt.ps["sensitivity",],1), ")", sep="");
    text(opt.ps["specificity",], opt.ps["sensitivity",], adj=c(-.05,1.25), label=lbls);
  }

  dev.off();
  
  mSetObj$analSet$opt.thresh <- opt.thresh

  if(.on.public.web){
    .set.mSet(mSetObj);
    return(imgName);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Plot a boxplot view of a selected compound
#'@description Plots a boxplot of the selected compound's concentration
#'between the groups.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param feat.nm Input the name of the selected compound.
#'@param version version mark for image name
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.
#'@param isOpt logical
#'@param isQuery logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PlotRocUnivBoxPlot <- function(mSetObj, feat.nm, version, format="png", dpi=72, isOpt, isQuery){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }

  data_ori_norm <- mSetObj$dataSet$norm
  if(!is.null(mSetObj$dataSet$norm.orig)){
    data_ori_norm <- mSetObj$dataSet$norm.orig
  }

  imgName <- mSetObj$dataSet$url.var.nms[feat.nm];
  imgName = paste("roc_boxplot_", imgName, "_", version, "_dpi", dpi, ".", format, sep="");
  
  x <- data_ori_norm[, feat.nm];
  y <- mSetObj$dataSet$cls;
  scale <- dpi/72;
  w <- 200*scale;
  h <- 400*scale; 
  col <- GetColorSchema(y);
  
  if(length(mSetObj$imgSet$roc.univ.boxplot)==0){
    mSetObj$imgSet$roc.univ.boxplot <- imgName;
    mSetObj$imgSet$roc.univ.name2 <- feat.nm;
  } else {
    if(feat.nm %in% mSetObj$imgSet$roc.univ.name2){
        idx <- which(feat.nm %in% mSetObj$imgSet$roc.univ.name);
        mSetObj$imgSet$roc.univ.boxplot[idx] <- imgName;
    } else {
        mSetObj$imgSet$roc.univ.name2 <- c(mSetObj$imgSet$roc.univ.name2, feat.nm);
        mSetObj$imgSet$roc.univ.boxplot <- c(mSetObj$imgSet$roc.univ.boxplot, imgName);
    }   
  }
  
  #mSetObj$imgSet$roc.univ.boxplot <- imgName;
  
  Cairo::Cairo(file=imgName, width=w, height=h, type=format, bg="white", dpi=dpi);
  
  df <- data.frame(conc = x, class = y)
  p <- ggplot2::ggplot(df, aes(x=class, y=conc, fill=class)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  p <- p + stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
  p <- p + theme(text = element_text(size=15), plot.margin = margin(t=0.45, r=0.25, b=1.5, l=0.25, "cm"), axis.text = element_text(size=10))
  p <- p + scale_fill_manual(values=col)
  
  if(isOpt){
    opt.thresh <- mSetObj$analSet$opt.thresh
    p <- p + geom_hline(aes(yintercept=opt.thresh), colour="red")
  }
  
  if(isQuery){
    thresh <- as.numeric(mSetObj$analSet$roc.obj$thresh)
    p <- p + geom_hline(aes(yintercept=thresh), colour="red")
  }
  
  print(p)
  dev.off()
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(imgName);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

#'Plot a summary view of the classification result
#'@description Plot of predicted class probabilities. On the x-axis is the proability, 
#'and the y-axis is the index of each predicted sample based on the probility. 
#'The samples are turned into separations at the x-axis.
#'This plot can be created for multivariate ROC curve analysis using SVM, PLS, and RandomForest.
#'Please note that sometimes, not all samples will be tested, instead they will be plotted
#'at the 0.5 neutral line. 
#'@usage PlotProbView(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, show, showPred) 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300. 
#'@param mdl.inx Model index, 0 means to compare all models, -1 means to use the best model, input 1-6 to plot a ROC curve for one of the top six models
#'@param show 1 or 0, if 1, label samples classified to the wrong groups 
#'@param showPred Show predicted samples
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotProbView <- function(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, show, showPred) {
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  smpl.nms <- rownames(mSetObj$dataSet$norm);
  prob.vec <- rep(0.5, length(smpl.nms));
  names(prob.vec) <- smpl.nms;
  
  if(mdl.inx == -1){
    mdl.inx <- mSetObj$analSet$multiROC$best.model.inx;
  }
  probs <- MergeDuplicates(unlist(mSetObj$analSet$multiROC$pred.cv[[mdl.inx]]));

  prob.vec[names(probs)] <- probs;
  
  nms <- names(prob.vec);
  ord.inx <- order(nms);
  prob.vec <- prob.vec[ord.inx];
  cls <- mSetObj$dataSet$cls[ord.inx];
  # remember to update the nms itself!
  nms <- names(prob.vec);
  
  # get html confusion matrix
  pred.out <- as.factor(ifelse(prob.vec > 0.5, 1, 0));
  act.cls <- as.numeric(cls)-1;
  
  prob.res <- data.frame(Probability=prob.vec, Predicted=pred.out, Actual=act.cls);
  
  write.table(prob.res, file="roc_pred_prob.csv", sep=",", col.names = TRUE);
  
  conf.res <- table(pred.out, act.cls);
  mSetObj$analSet$conf.table <- xtable::xtable(conf.res, caption="Confusion Matrix (Cross-Validation)");
  mSetObj$analSet$conf.mat <- print(mSetObj$analSet$conf.table, type = "html", print.results=F, caption.placement="top", html.table.attributes="border=1 width=150" )     
  

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 9; h <- 8;

  mSetObj$imgSet$roc.prob.plot <- imgName;
  mSetObj$imgSet$roc.prob.name <- mdl.inx


  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  set.seed(123);
  y <- rnorm(length(prob.vec));
  max.y <- max(abs(y));
  ylim <- max.y*c(-1.05, 1.05);
  
  xlim <- c(0, 1.0);
  
  op <- par(mar=c(4,4,3,6));
  pchs <- ifelse(as.numeric(cls) == 1, 1, 19);
  
  colors <- ifelse(show==1, "darkgrey", "black");
  ROCR::plot(prob.vec, y, pch=pchs, col=colors, xlim=xlim, ylim= ylim, xlab = "Predicted Class Probabilities", ylab="Samples");
  abline(h = 0, lty = 2, col="grey");
  abline(v = 0.5, lty = 2, col="grey");
  
  par(xpd=T);
  legend("right",inset=c(-0.11,0), legend = unique(as.character(cls)), pch=unique(pchs));
  
  test.y <- test.x <- 0;
  if(showPred){
    test.y <- rnorm(length(mSetObj$analSet$multiROC$test.res));
    test.x <- mSetObj$analSet$multiROC$test.res;

    pchs <- ifelse(as.numeric(mSetObj$dataSet$test.cls) == 1, 1, 19);
    points(test.x, test.y, pch=pchs, cex=1.5, col="red");
  }
  
  if(show == 1){ 
    
    # add labels for sample classified wrong
    # the idea is to add labels to the left side for those with prob < 0.5
    # and add labels to the right side of the point for those with prob > 0.5
    # leave 0.5 out 
    
    # first wrong pred as 1 (right side)
    act.ones <- as.numeric(cls)-1 == 1;
    pred.vec <- ifelse(prob.vec > 0.5, 1, 0);
    
    wrong.inx <- (pred.vec != as.numeric(cls)-1) & pred.vec == 1;
    if(sum(wrong.inx) > 0){
      text(prob.vec[wrong.inx], y[wrong.inx], nms[wrong.inx], pos=4);
    }
    
    # first wrong pred as 0 (left side)
    act.zeros <- as.numeric(cls)-1 == 0;
    pred.vec <- ifelse(prob.vec < 0.5, 0, 0.5);
    wrong.inx <- pred.vec != as.numeric(cls)-1 & pred.vec == 0;
    if(sum(wrong.inx) > 0){
      text(prob.vec[wrong.inx], y[wrong.inx], nms[wrong.inx], pos=2);
    }
    
    if(showPred){
      nms <- rownames(mSetObj$dataSet$test.data);
      
      act.ones <- as.numeric(mSetObj$dataSet$test.cls)-1 == 1;
      act.zeros <- as.numeric(mSetObj$dataSet$test.cls)-1 == 0;
      
      # leave 0.5 there
      pred.vec <- ifelse(test.x > 0.5, 1, 0.5);
      wrong.inx <- (pred.vec != as.numeric(mSetObj$dataSet$test.cls)-1) & act.ones;
      if(sum(wrong.inx) > 0){
        text(test.x[wrong.inx], test.y[wrong.inx], nms[wrong.inx], pos=4, cex=0.9);
      }
      
      pred.vec <- ifelse(test.x < 0.5, 0, 0.5);
      wrong.inx <- pred.vec != as.numeric(mSetObj$dataSet$test.cls)-1 & act.zeros;
      if(sum(wrong.inx) > 0){
        text(test.x[wrong.inx], test.y[wrong.inx], nms[wrong.inx], pos=2, cex=0.9);
      }
    }
  }
  par(op)
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot a summary view of the classification result of tester prediction
#'@description Plot of predicted class probabilities. On the x-axis is the proability, 
#'and the y-axis is the index of each predicted sample based on the probility. 
#'The samples are turned into separations at the x-axis.
#'This plot can be created for multivariate ROC curve analysis using SVM, PLS, and RandomForest.
#'Please note that sometimes, not all samples will be tested, instead they will be plotted
#'at the 0.5 neutral line. 
#'@usage PlotProbViewTest(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, show, showPred) 
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300. 
#'@param mdl.inx Model index, 0 means to compare all models, -1 means to use the best model, input 1-6 to plot a ROC curve for one of the top six models
#'@param show 1 or 0, if 1, label samples classified to the wrong groups 
#'@param showPred Show predicted samples
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotProbViewTest <- function(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, show, showPred) {
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  smpl.nms <- rownames(mSetObj$dataSet$norm);
  prob.vec <- rep(0.5, length(smpl.nms));
  names(prob.vec) <- smpl.nms;
  
  probs <- MergeDuplicates(unlist(mSetObj$analSet$ROCtest$pred.cv));

  prob.vec[names(probs)] <- probs;
  
  nms <- names(prob.vec);
  ord.inx <- order(nms);
  prob.vec <- prob.vec[ord.inx];
  cls <- mSetObj$dataSet$cls[ord.inx];
  # remember to update the nms itself!
  nms <- names(prob.vec);
  
  # get html confusion matrix
  pred.out <- as.factor(ifelse(prob.vec > 0.5, 1, 0));
  act.cls <- as.numeric(cls)-1;
  
  prob.res <- data.frame(Probability=prob.vec, Predicted=pred.out, Actual=act.cls);
  
  write.table(prob.res, file="roc_pred_prob1.csv", sep=",", col.names = TRUE);
  
  conf.res <- table(pred.out, act.cls);
  mSetObj$analSet$conf.table <- xtable::xtable(conf.res, caption="Confusion Matrix (Cross-Validation)");
  mSetObj$analSet$conf.mat <- print(mSetObj$analSet$conf.table, type = "html", print.results=F, caption.placement="top", html.table.attributes="border=1 width=150" )     
  
  if(anal.mode == "test"){
    if(!is.null(mSetObj$dataSet$test.data)){
      
      test.pred <- ifelse(mSetObj$analSet$ROCtest$test.res > 0.5, 1, 0);
      test.cls <- as.numeric(mSetObj$dataSet$test.cls)-1;
      
      test.df <- data.frame(Prob_HoldOut=mSetObj$analSet$ROCtest$test.res, Predicted_HoldOut=test.pred, Actual_HoldOut=test.cls);
      suppressMessages(write.table(test.df, file="roc_pred_prob1.csv", sep=",", append=TRUE, col.names = TRUE));
      
      test.res <- table(test.pred, test.cls);
      mSetObj$analSet$conf.mat.test <- print(xtable::xtable(test.res, 
                                                            caption="Confusion Matrix (Hold-out)"),
                                             type = "html", print.results=F, xtable.width=120, caption.placement="top",
                                             html.table.attributes="border=1 width=150" );
    }
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 9; h <- 8;
  
  mSetObj$imgSet$roc.testprob.plot <- imgName;
  mSetObj$imgSet$roc.testprob.name <- mdl.inx

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  set.seed(123);
  y <- rnorm(length(prob.vec));
  max.y <- max(abs(y));
  ylim <- max.y*c(-1.05, 1.05);
  
  xlim <- c(0, 1.0);
  
  op <- par(mar=c(4,4,3,6));
  pchs <- ifelse(as.numeric(cls) == 1, 1, 19);
  
  colors <- ifelse(show==1, "darkgrey", "black");
  ROCR::plot(prob.vec, y, pch=pchs, col=colors, xlim=xlim, ylim= ylim, xlab = "Predicted Class Probabilities", ylab="Samples");
  abline(h = 0, lty = 2, col="grey");
  abline(v = 0.5, lty = 2, col="grey");
  
  par(xpd=T);
  legend("right",inset=c(-0.11,0), legend = unique(as.character(cls)), pch=unique(pchs));
  
  test.y <- test.x <- 0;
  if(showPred){
    test.y <- rnorm(length(mSetObj$analSet$ROCtest$test.res));
    test.x <- mSetObj$analSet$ROCtest$test.res;
   
    pchs <- ifelse(as.numeric(mSetObj$dataSet$test.cls) == 1, 1, 19);
    points(test.x, test.y, pch=pchs, cex=1.5, col="red");
  }
  
  if(show == 1){ 
    
    # add labels for sample classified wrong
    # the idea is to add labels to the left side for those with prob < 0.5
    # and add labels to the right side of the point for those with prob > 0.5
    # leave 0.5 out 
    
    # first wrong pred as 1 (right side)
    act.ones <- as.numeric(cls)-1 == 1;
    pred.vec <- ifelse(prob.vec > 0.5, 1, 0);
    
    wrong.inx <- (pred.vec != as.numeric(cls)-1) & pred.vec == 1;
    if(sum(wrong.inx) > 0){
      text(prob.vec[wrong.inx], y[wrong.inx], nms[wrong.inx], pos=4);
    }
    
    # first wrong pred as 0 (left side)
    act.zeros <- as.numeric(cls)-1 == 0;
    pred.vec <- ifelse(prob.vec < 0.5, 0, 0.5);
    wrong.inx <- pred.vec != as.numeric(cls)-1 & pred.vec == 0;
    if(sum(wrong.inx) > 0){
      text(prob.vec[wrong.inx], y[wrong.inx], nms[wrong.inx], pos=2);
    }
    
    if(showPred){
      nms <- rownames(mSetObj$dataSet$test.data);
      
      act.ones <- as.numeric(mSetObj$dataSet$test.cls)-1 == 1;
      act.zeros <- as.numeric(mSetObj$dataSet$test.cls)-1 == 0;
      
      # leave 0.5 there
      pred.vec <- ifelse(test.x > 0.5, 1, 0.5);
      wrong.inx <- (pred.vec != as.numeric(mSetObj$dataSet$test.cls)-1) & act.ones;
      if(sum(wrong.inx) > 0){
        text(test.x[wrong.inx], test.y[wrong.inx], nms[wrong.inx], pos=4, cex=0.9);
      }
      
      pred.vec <- ifelse(test.x < 0.5, 0, 0.5);
      wrong.inx <- pred.vec != as.numeric(mSetObj$dataSet$test.cls)-1 & act.zeros;
      if(sum(wrong.inx) > 0){
        text(test.x[wrong.inx], test.y[wrong.inx], nms[wrong.inx], pos=2, cex=0.9);
      }
    }
  }
  par(op)
  dev.off();
  return(.set.mSet(mSetObj));
}



#'Plot ROC
#'@description Pred and auroc are lists containing predictions
#'and labels from different cross-validations 
#'@usage PlotROC(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, 
#'avg.method, show.conf, show.holdout, focus="fpr", cutoff = 1.0)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", 
#'users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param mdl.inx Model index, 0 means to compare all models, 
#'input 1-6 to plot a ROC curve for one of the top six models  
#'@param avg.method Input the method to compute the average ROC curve, 
#'either "threshold", "vertical" or "horizontal"
#'@param show.conf Logical, if 1, show confidence interval, if 0 do not show
#'@param show.holdout Logical, if 1, show the ROC curve for hold-out validation, if 0 do not show 
#'@param focus "fpr" 
#'@param cutoff Input the threshold to limit the calculation of the 
#'ROC curve, the number must be between 0 and 1.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotROC <- function(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, avg.method, show.conf, show.holdout, focus="fpr", cutoff = 1.0){
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 8; h <- 8;

  mSetObj$imgSet$roc.multi.plot <- imgName;
  mSetObj$imgSet$roc.multi.model <- mdl.inx;

  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  op <- par(mar=c(5,4,3,3));
  
  if(mdl.inx == 0){ # compare all models
    preds <- mSetObj$analSet$multiROC$pred.list;
    auroc <- mSetObj$analSet$multiROC$auc.vec;
    perf <- ROCR::performance(preds[[1]], "tpr", "fpr");
    perf.avg <- ComputeAverageCurve(perf, avg.method);
    
    cols <- (1:length(preds))+1;
    ROCR::plot(perf.avg@x.values[[1]], perf.avg@y.values[[1]], type="n", axes=F,
         xlim=c(0,1), ylim=c(0,1),
         xlab="1-Specificity (False positive rate)",
         ylab="Sensitivity (True positive rate)"
    );
    
    box()
    axis(side=2)
    lab.at <- seq(0, 1, 0.2);
    grid.at <- seq(0, 1, 0.1);
    lab.labels <- lab.at
    axis(side=1, at=lab.at, labels=as.graphicsAnnot(sprintf( "%.1f", lab.labels)));
    abline(v=grid.at, lty=3, col="lightgrey");
    abline(h=grid.at, lty=3, col="lightgrey");
    lines(perf.avg@x.values[[1]], perf.avg@y.values[[1]], col=cols[1]);
    for(i in 2:length(preds)){
      perf <- ROCR::performance(preds[[i]], "tpr", "fpr");
      avg <- ComputeAverageCurve(perf, avg.method);
      lines(avg@x.values[[1]], avg@y.values[[1]], col=cols[i]);
    }
    
    best.inx <- which.max(auroc);
    
    # now add and format legends to the bottom right corner
    feats <- c("Var.", names(preds));
    feats <- substr(feats, 1, 8);
    feats <- sprintf("%-5s", feats);
    
    vals <- c("AUC", round(auroc, 3));
    
    vals <- sprintf("%-8s", vals);
    
    cis <- mSetObj$analSet$multiROC$auc.ci;
    cis <- c("CI", cis);
    legends <- paste(feats, vals, cis, sep="");
    
    pch <- c(NA, rep(15, length(preds)));
    cols <- c(NA, cols);
    
    legend("bottomright", legend = legends, pch=15, col=cols);
    
  }else if(mdl.inx > 0){ 
    
    preds <- ROCR::prediction(mSetObj$analSet$multiROC$pred.cv[[mdl.inx]], mSetObj$analSet$multiROC$true.cv);
    auroc <- round(mSetObj$analSet$multiROC$auc.vec[mdl.inx],3);
    auc.ci <- mSetObj$analSet$multiROC$auc.ci[mdl.inx];
    perf <- ROCR::performance(preds, "tpr", "fpr");
    perf.avg <- ComputeAverageCurve(perf, avg.method);
    y.all <- perf.avg@y.values[[1]];
    x.all <- perf.avg@x.values[[1]];
    lgd <- paste("Area under the curve (AUC) =", auroc, "\n",
                 "95% CI:", auc.ci);
    
    ROCR::plot(x.all, y.all, type="n", axes=F,
         xlim=c(0,1), ylim=c(0,1),
         xlab="1-Specificity (False positive rate)",
         ylab="Sensitivity (True positive rate)"
    );
    
    box()
    axis(side=2)
    lab.at <- seq(0, 1, 0.2);
    grid.at <- seq(0, 1, 0.1);
    lab.labels <- lab.at
    axis(side=1, at=lab.at, labels=as.graphicsAnnot(sprintf( "%.1f", lab.labels)));
    abline(v=grid.at, lty=3, col="lightgrey");
    abline(h=grid.at, lty=3, col="lightgrey");
    lines(x.all, y.all, type="l", lwd=2, col="blue");
    
    if(show.conf){
      res <- ComputeHighLow(perf);
      suppressWarnings(polygon(c(x.all, rev(x.all)), c(res$con.low, rev(res$con.high)), col="#0000ff22"))
    }
    
    legend("center", legend = lgd, bty="n");
    
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot ROC for the ROC Curve Based Model Creation and Evaluation module
#'@description Plot the ROC curve of the biomarker model created using a user-selected subset of features.
#'Pred and auroc are lists containing predictions and labels from different cross-validations. 
#'@usage PlotROCTest(mSetObj=NA, imgName, format="png", 
#'dpi=72, mdl.inx, avg.method, show.conf, show.holdout, focus="fpr", cutoff = 1.0)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", 
#'users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param mdl.inx Model index, 0 means to compare all models, 
#'input 1-6 to plot a ROC curve for one of the top six models  
#'@param avg.method Input the method to compute the average ROC curve, 
#'either "threshold", "vertical" or "horizontal"
#'@param show.conf Logical, if 1, show confidence interval, if 0 do not show
#'@param show.holdout Logical, if 1, show the ROC curve for hold-out validation, if 0 do not show 
#'@param focus "fpr" 
#'@param cutoff Input the threshold to limit the calculation of the ROC curve, the number must be between 0 and 1.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export 

PlotROCTest<-function(mSetObj=NA, imgName, format="png", dpi=72, mdl.inx, avg.method, show.conf, show.holdout, focus="fpr", cutoff = 1.0){
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 8; h <- 8;
  mSetObj$imgSet$roc.testcurve.plot <- imgName;
  mSetObj$imgSet$roc.testcurve.name <- mdl.inx
  mSetObj$imgSet$roc.testcurve.method <- avg.method
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  op <- par(mar=c(5,4,3,3));
  
  if(anal.mode=="explore" && mdl.inx == 0){ # compare all models
    preds <- mSetObj$analSet$ROCtest$pred.list;
    auroc <- mSetObj$analSet$ROCtest$auc.vec;
    perf <- ROCR::performance(preds[[1]], "tpr", "fpr");
    perf.avg <- ComputeAverageCurve(perf, avg.method);
    
    cols <- (1:length(preds))+1;
    ROCR::plot(perf.avg@x.values[[1]], perf.avg@y.values[[1]], type="n", axes=F,
         xlim=c(0,1), ylim=c(0,1),
         xlab="1-Specificity (False positive rate)",
         ylab="Sensitivity (True positive rate)"
    );
    
    box()
    axis(side=2)
    lab.at <- seq(0, 1, 0.2);
    grid.at <- seq(0, 1, 0.1);
    lab.labels <- lab.at
    axis(side=1, at=lab.at, labels=as.graphicsAnnot(sprintf( "%.1f", lab.labels)));
    abline(v=grid.at, lty=3, col="lightgrey");
    abline(h=grid.at, lty=3, col="lightgrey");
    lines(perf.avg@x.values[[1]], perf.avg@y.values[[1]], col=cols[1]);
    for(i in 2:length(preds)){
      perf <- ROCR::performance(preds[[i]], "tpr", "fpr");
      avg <- ComputeAverageCurve(perf, avg.method);
      lines(avg@x.values[[1]], avg@y.values[[1]], col=cols[i]);
    }
    
    best.inx <- which.max(auroc);
    
    # now add and format legends to the bottom right corner
    feats <- c("Var.", names(preds));
    feats <- substr(feats, 1, 8);
    feats <- sprintf("%-5s", feats);
    
    vals <- c("AUC", round(auroc, 3));
    
    vals <- sprintf("%-8s", vals);
    
    cis <- mSetObj$analSet$multiROC$auc.ci;
    cis <- c("CI", cis);
    legends <- paste(feats, vals, cis, sep="");
    
    pch <- c(NA, rep(15, length(preds)));
    cols <- c(NA, cols);
    
    legend("bottomright", legend = legends, pch=15, col=cols);
    
  }else if(mdl.inx > 0 && anal.mode=="explore"){ 
    
    preds <- ROCR::prediction(mSetObj$analSet$ROCtest$pred.cv[[mdl.inx]], mSetObj$analSet$ROCtest$true.cv);
    auroc <- round(mSetObj$analSet$ROCtest$auc.vec[mdl.inx],3);
    auc.ci <- mSetObj$analSet$ROCtest$auc.ci[mdl.inx];
    perf <- ROCR::performance(preds, "tpr", "fpr");
    perf.avg <- ComputeAverageCurve(perf, avg.method);
    y.all <- perf.avg@y.values[[1]];
    x.all <- perf.avg@x.values[[1]];
    lgd <- paste("Area under the curve (AUC) =", auroc, "\n",
                 "95% CI:", auc.ci);
    
    ROCR::plot(x.all, y.all, type="n", axes=F,
         xlim=c(0,1), ylim=c(0,1),
         xlab="1-Specificity (False positive rate)",
         ylab="Sensitivity (True positive rate)"
    );
    
    box()
    axis(side=2)
    lab.at <- seq(0, 1, 0.2);
    grid.at <- seq(0, 1, 0.1);
    lab.labels <- lab.at
    axis(side=1, at=lab.at, labels=as.graphicsAnnot(sprintf( "%.1f", lab.labels)));
    abline(v=grid.at, lty=3, col="lightgrey");
    abline(h=grid.at, lty=3, col="lightgrey");
    lines(x.all, y.all, type="l", lwd=2, col="blue");
    
    if(show.conf){
      res <- ComputeHighLow(perf);
      suppressWarnings(polygon(c(x.all, rev(x.all)), c(res$con.low, rev(res$con.high)), col="#0000ff22"))
    }
    
    legend("center", legend = lgd, bty="n");
    
  }else{ # plot ROC of specific model and save the table for details
    
    preds <- ROCR::prediction(mSetObj$analSet$ROCtest$pred.cv, mSetObj$analSet$ROCtest$true.cv);
    auroc <- round(mSetObj$analSet$ROCtest$auc.vec[1],3)
    auc.ci <- mSetObj$analSet$ROCtest$auc.ci;
    
    perf <- ROCR::performance(preds, "tpr", "fpr");
    perf.avg <- ComputeAverageCurve(perf, avg.method);
    y.all <- perf.avg@y.values[[1]];
    x.all <- perf.avg@x.values[[1]];
    # to draw a roc curve line from (0,0)
    y.all <- c(0.0, y.all);
    x.all <- c(0.0, x.all);
    
    lgd <- paste("Area under the curve (AUC) =", auroc, "\n",
                 "95% CI:", auc.ci);
    
    ROCR::plot(x.all, y.all, type="n", axes=F,
         xlim=c(0,1), ylim=c(0,1),
         xlab="1-Specificity (False positive rate)",
         ylab="Sensitivity (True positive rate)"
    );
    
    box()
    axis(side=2)
    lab.at <- seq(0, 1, 0.2);
    grid.at <- seq(0, 1, 0.1);
    lab.labels <- lab.at
    axis(side=1, at=lab.at, labels=as.graphicsAnnot(sprintf( "%.1f", lab.labels)));
    abline(v=grid.at, lty=3, col="lightgrey");
    abline(h=grid.at, lty=3, col="lightgrey");
    lines(x.all, y.all, type="l", lwd=2, col="blue");
    
    if(show.conf){
      res <- ComputeHighLow(perf);            
      # to draw a roc curve line from (0,0)
      # suppressWarnings(polygon(c(x.all, rev(x.all)), c(res$con.low, rev(res$con.high)), col="#0000ff22"))
      suppressWarnings(polygon(c(x.all, rev(x.all)), c(c(0,res$con.low), c(rev(res$con.high),0)), col="#0000ff22"))
    }
    if(show.holdout){

      roc.obj <- pROC::roc(mSetObj$dataSet$test.cls, mSetObj$analSet$ROCtest$test.res, percent = F);
      test.x <- 1-roc.obj$spec;
      test.y <- roc.obj$sens;
      
      lbls <- c("Type", "CV","Holdout");
      lbls <- sprintf("%-10s",lbls);
      
      test.auc <- round(roc.obj$auc[[1]],3);
      aucs <- c("AUC", auroc, test.auc);
      
      lgd <- paste(lbls, aucs, sep="");
      lines(test.x, test.y, type="l", lwd=2, col="magenta");
      legend("bottomright", legend = lgd, pch=c(NA, 15, 15), col=c(NA, "blue", "magenta"));
    }else{
      legend("center", legend = lgd,  bty="n");
    }       
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot classification performance using different features for Multi-Biomarker
#'@description Plot of the accuracy of classification with an increasing number of features.
#'@usage PlotAccuracy(mSetObj=NA, imgName, format="png", dpi=72)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotAccuracy<-function(mSetObj=NA, imgName, format="png", dpi=72){
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 9; h <- 7;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if(is.null(mSetObj$analSet$multiROC$accu.mat)){
    accu.mat <- mSet$analSet$ROCtest$accu.mat;
  }else{
    accu.mat <- mSetObj$analSet$multiROC$accu.mat;
  }
  
  mn.accu <- apply(accu.mat, 2, mean);
  ylabl <- 'Predictive Accuracy';
  ylim <- c(0,1);
  title <- 'Predictive accuracies with different features';
  txt.lbls <- paste(100*round(mn.accu,3),'%');
  
  matplot(t(accu.mat),type='l', lty=2, col="grey", xlab='Number of features',ylab=ylabl, ylim=ylim,
          axes=F,main=title);
  
  lines(1:ncol(accu.mat), mn.accu, lwd=2);
  points(mn.accu, pch=19, col=ifelse(1:length(mn.accu)==which.max(mn.accu),"red","blue"));
  text(mn.accu,labels=txt.lbls, adj=c(-0.3, -0.5), srt=45, xpd=T)
  axis(2);
  
  lbls <- colnames(accu.mat);
  axis(1, 1:length(mn.accu), labels=lbls);
  
  mSetObj$imgSet$roc.pred <- imgName;
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot classification performance using different features for Biomarker Tester
#'@description Plot of the accuracy of classification with an increasing number of features.
#'@usage PlotTestAccuracy(mSetObj=NA, imgName, format="png", dpi=72)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotTestAccuracy<-function(mSetObj=NA, imgName, format="png", dpi=72){
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 9; h <- 7;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  y.vec <- mSetObj$analSet$ROCtest$accu.mat[1,];
  ylim.ext <- GetExtendRange (y.vec, 12); # first increase ylim by 1/12
  boxplot(y.vec, col="#0000ff22", ylim=ylim.ext, outline=FALSE, boxwex=c(0.5, 0.5), ylab="Predictive Accuracy");
  stripchart(t(mSetObj$analSet$ROCtest$accu.mat), method = "jitter", vertical=T, add = T, pch=19);
  
  accu.info <- paste ("The average accuracy based on 100 cross validations is", round(mean(y.vec), 3));
  
  mSetObj$imgSet$roc.testpred <- imgName;
  
  if(!is.null(mSetObj$dataSet$test.cls)){
    test.pred <- ifelse(mSetObj$analSet$ROCtest$test.res > 0.5, 1, 0);
    test.cls <- as.numeric(mSetObj$dataSet$test.cls)-1;
    
    hit <- sum(test.pred == test.cls);
    percent <- round(hit/length(test.cls), 3);
    accu.info <- paste(accu.info, ". The accuracy for hold out data prediction is ",  percent,
                       "(", hit, "/",  length(test.cls), ").", sep="");
  }
  
  mSetObj$analSet$ROCtest$accu.info <- accu.info;
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot selected compounds by their percentage frequency
#'@description Plot the important variables of single biomarker model ranked by order of importance
#'@usage PlotImpBiomarkers(mSetObj=NA, imgName, format="png", dpi=72, 
#'mdl.inx, measure = "freq", feat.num = 15)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format elect the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param mdl.inx Model index, -1 selects the model with the best AUC, input 1-6 to 
#'view the important features of one of the top six models
#'@param measure Choose to rank features by the frequency of being selected "freq", or the 
#'mean importance measure "mean"
#'@param feat.num Input the number of features to include in the plot, by default it is 15.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotImpBiomarkers <- function(mSetObj=NA, imgName, format="png", dpi=72, 
                        mdl.inx, measure = "freq", feat.num = 15) {
  
  mSetObj <- .get.mSet(mSetObj);

  anal.mode <- mSetObj$analSet$mode;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 8; h <- 8;
  mSetObj$imgSet$roc.imp.plot <- imgName;
  mSetObj$imgSet$roc.imp.name <- mdl.inx
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if(is.null(mSetObj$dataSet$norm_explore)){
    data <- mSetObj$dataSet$norm;
    mSetObj$dataSet$norm_explore <- mSetObj$dataSet$norm;
  } else {
    data <- mSetObj$dataSet$norm_explore
  }
 
  cls <- mSetObj$dataSet$cls;
  op <- par(mar=c(6,10,3,7));
  
  # if(anal.mode == "explore"){
    if(mdl.inx == -1){
      mdl.inx <- mSetObj$analSet$multiROC$best.model.inx;
    }
    imp.mat <- GetImpFeatureMat(mSetObj, mSetObj$analSet$multiROC$imp.cv, mSetObj$analSet$multiROC$test.feats[mdl.inx]);
  # }else{
  #   imp.mat <- GetImpFeatureMat(mSetObj, mSetObj$analSet$multiROC$imp.cv, null);
  # }
  
  imp.nms <- rownames(imp.mat);
  hit.nms <- imp.nms[imp.nms %in% colnames(data)];
  data <- data[, hit.nms];
  
  # note, tapply can only be applied to a vector, we need
  # to combine with apply in order to used on a data frame
  mds <- apply(data, 2,
               function(x){
                 tapply(x, cls, median);
               });

  lowhigh <- apply(mds, 2,
                   function(x){
                     ifelse(rank(x)==1, "Low", "High")
                   });
  lowhigh <- t(lowhigh);

  temp.dat <- data.frame(imp.mat, lowhigh);
  colnames(temp.dat) <- c(colnames(imp.mat), levels(cls));
  imp.fileNm <- "imp_features_cv.csv";
  fast.write.csv(temp.dat, file=imp.fileNm);
  temp.dat <- NULL;
  
  # record the imp.mat for table show
  mSetObj$analSet$imp.mat <- imp.mat;
  mSetObj$analSet$lowhigh <- lowhigh;
  mSetObj$analSet$roc.sig.nm <- imp.fileNm;

  if(measure=="freq"){
    imp.vec <- sort(imp.mat[,1], decreasing=T);
    xlbl <- "Selected Frequency (%)";
  }else{ # default sort by freq, need to reorder
    imp.vec <- sort(imp.mat[,2], decreasing=T);
    xlbl <- "Average Importance";
  }
  var.len <- length(imp.vec);
  
  if(feat.num <= 0){
    feat.num = 15;
  }else if(feat.num > var.len){
    feat.num <- var.len;
  }
  
  imp.vec<-rev(imp.vec[1:feat.num]);
  
  nms.orig <- names(imp.vec);
  vip.nms <-substr(nms.orig, 1, 20);
  names(imp.vec) <- NULL;
  
  xlim.ext <- GetExtendRange(imp.vec, 12);
  dotchart(imp.vec, bg="blue", xlab= xlbl, xlim=xlim.ext);
  
  mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)
  names(imp.vec) <- nms.orig;
  
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  # get character width
  shift <- 2*par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  
  x <- rep(lgd.x, feat.num);
  y <- 1:feat.num;
  
  par(xpd=T);

  # now synchronize lowhigh with imp.vec
  lowhigh <- lowhigh[nms.orig, ,drop=FALSE];

  nc <- ncol(lowhigh);
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(nc);
  
  # calculate background
  bg <- matrix("", nrow(lowhigh), nc);
  for (m in 1:nrow(lowhigh)){
    bg[m,] <- ifelse(lowhigh[m,]=="High", col[1], col[2]);
  } 

  cls.lbl <- levels(cls);

  for (n in 1:ncol(lowhigh)){
    points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
    # now add label
    text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
    # shift x, note, this is good for current size
    x <- x + shift/1.25;
  }
  
  # now add color key, padding with more intermediate colors for contiuous band
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(20)
  nc <- length(col);
  x <- rep(x[1] + shift, nc);
  
  shifty <- (axis.lims[4]-axis.lims[3])/3;
  starty <- axis.lims[3] + shifty;
  endy <- axis.lims[3] + 2*shifty;
  y <- seq(from = starty, to = endy, length = nc);
  
  points(x,y, bty="n", pch=15, col=rev(col), cex=2);
  
  text(x[1], endy+shifty/8, "High");
  text(x[1], starty-shifty/8, "Low");
  par(op);
  dev.off();
  
  return(.set.mSet(mSetObj));
  
}

#'Perform permutation tests only for ROC Tester 
#'@description Perform permutation tests for the ROC Curve Based Model Creation and Evaluation module
#'@usage Perform.Permut(mSetObj=NA, perf.measure, perm.num, propTraining = 2/3)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param perf.measure Input the performance measure to rate the performance of the model, either the area
#'under the ROC curve ("auroc") or the predictive accuracy ("accu")
#'@param perm.num Input the number of permutations to perform
#'@param propTraining Numeric, input the fraction of samples to set aside for training. Default is set to 2/3.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Perform.Permut<-function(mSetObj=NA, perf.measure, perm.num, propTraining = 2/3){
  
  mSetObj <- .get.mSet(mSetObj);
  
  cvRuns = perm.num;
  propTraining = propTraining
  
  cls <- mSetObj$dataSet$cls;
  datmat <- mSetObj$dataSet$norm;

  if(mSetObj$analSet$mode == "test"){
    clsMethod <- mSetObj$analSet$tester.method;
  }else{
    clsMethod <- mSetObj$analSet$exp.method;
  }

  splitMat <- GetTrainTestSplitMat(cls, propTraining, cvRuns);
  trainInx <- splitMat$training.mat;
  testInx <- splitMat$testing.mat;
  
  # now, do the permutation
  perf.outp <- actualCls <- vector(length = cvRuns, mode = "list");
  # for (irun in 1:cvRuns){
  irun <- 1;
  while(irun < cvRuns){
    cls <- cls[order(runif(length(cls)))];
    trainingSampleRun <- trainInx[irun, ]
    testSampleRun <- testInx[irun, ];
    y.in <- cls[trainingSampleRun];
    # make sure the y.in contain only one group
    if(length(unique(as.numeric(y.in))) > 1){
      y.out <- cls[testSampleRun];
      actualCls[[irun]] <- as.numeric(y.out);
      perf.outp[[irun]] <- Get.pred(datmat[trainingSampleRun,], y.in,
                                    datmat[testSampleRun, ], y.out, clsMethod);
      irun <- irun + 1;
    }else{
      print("redo....");
    }
  }
  
  # get the AUROC for permuted data
  pred <- ROCR::prediction(perf.outp, actualCls);
  aucs <- try(unlist(slot(ROCR::performance(pred, "auc"), "y.values")));
  if (class(aucs)=="try-error"){
     AddErrMsg("Not enough distinct predictions to compute area under the ROC curve. Increase sample size or reduce permutation number.");
     return(0);
  }
  accs <- unlist(slot(ROCR::performance(pred, "acc"), "y.values"));
  perf.obj <- try(ROCR::performance(pred, "tpr", "fpr"));
  if (class(perf.obj)=="try-error"){
     AddErrMsg("Something wrong with the task. Increase sample size or reduce permutation number.");
     return(0);
  }
  # now, insert the average value of the original performance
  accs <- c(mean(mSetObj$analSet$ROCtest$accu.mat[1,]), accs);
  aucs <- c(mean(mSetObj$analSet$ROCtest$auc.vec), aucs);
  mSetObj$analSet$ROCtest$perm.res <- list(perf.measure=perf.measure, perf.obj=perf.obj, auc.vec=aucs, acc.vec=accs);
  return(.set.mSet(mSetObj));
}

#'Get predicted class probability 
#'@description Get predicted class probability, used in higher function
#'@param x.train Training X
#'@param y.train Training Y
#'@param x.test Test X
#'@param y.test Test Y
#'@param clsMethod Method to predict class, by default it is PLS
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.pred <- function(x.train, y.train, x.test, y.test, clsMethod="pls"){
  
  # first convert class label to 0/1 so convert prob-> class will be easier
  y.train <- as.factor(as.numeric(y.train)-1);
  y.test <- as.factor(as.numeric(y.test)-1);
  
  # note, we only need prob for class 1, pred can be inferred
  if (clsMethod == "rf"){
    model <- randomForest::randomForest(x.train, y.train, ntree=100, importance=F);
    prob.out <- predict(model, x.test, type="prob")[,"1"];
    return(prob.out);
  }else if(clsMethod == "pls"){ # plsda
    model <- caret::plsda(x.train, y.train, method='oscorespls');
    prob.out <- predict(model, x.test, type="prob")[,"1",1];
    return(prob.out);
  }else{ # svm
    model <- e1071::svm(x.train, y.train, type = 'C', kernel="linear", probability=TRUE);
    prob.out <- attr(predict(model, x.test,  probability=TRUE), "probabilities")[,"1"];
    return(prob.out);
  }
}

#'Prepare report for permutation tests
#'@description Function to prepare a report for permutation tests, used in higher functions
#'@param perm.vec Input permutation vector 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PreparePermResult<-function(perm.vec){
  
  # check for infinite since with group variance could be zero for perfect classification
  inf.found = TRUE;
  if(sum(is.finite(perm.vec))==length(perm.vec)){
    inf.found = FALSE;
  }else {
    if(sum(is.finite(perm.vec))==0){ # all are infinite, give a random number 10
      perm.vec<-rep(10, length(perm.vec));
    }else{ # if not all inf, replace with the 10 fold of non-inf values
      perm.vec[!is.finite(perm.vec)]<-10*max(perm.vec[is.finite(perm.vec)]);
    }
  }
  
  better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
  num <- length(perm.vec);
  if(better.hits == 0) {
    p <- paste("p <", 1/num);
  }else{
    p <- better.hits/num;
    p <- paste("p =", signif(p, digits=5));
  }
  
  list(permut.p = p,
       permut.inf = F,
       permut = perm.vec);
}

#'Plot results of permutation tests
#'@description Plot results of permutation tests
#'@usage Plot.Permutation(mSetObj=NA, imgName, format="png", dpi=72)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format elect the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Plot.Permutation<-function(mSetObj=NA, imgName, format="png", dpi=72){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  w <- 8; h <- 8;
  mSetObj$imgSet$roc.perm.plot <- imgName;
  
  if(mSetObj$analSet$ROCtest$perm.res$perf.measure == "auroc"){
    mSetObj$imgSet$roc.perm.method <- "auroc"
  }else{
    mSetObj$imgSet$roc.perm.method <- "predictive accuracy"
  }
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if(mSetObj$analSet$ROCtest$perm.res$perf.measure == "auroc"){
    ROCR::plot(mSetObj$analSet$ROCtest$perm.res$perf.obj, col="grey", lwd=1, lty=2,
         xlab="1-Specificity (False Positive rate)",
         ylab="Sensitivity (True Positive rate)");
    
    # now add the original ROC
    
    preds <- ROCR::prediction(mSetObj$analSet$ROCtest$pred.cv, mSetObj$analSet$ROCtest$true.cv);
    auroc <- round(mSetObj$analSet$ROCtest$auc.vec[1],3)
    perf <- ROCR::performance(preds, "tpr", "fpr");
    # need to replace Inf with 1
    alpha.vals <- perf@alpha.values;
    perf@alpha.values <- lapply(alpha.vals, function(x){
      x[x==Inf] <- 1;
      x[x==-Inf] <- 0;
      x
    });
    
    
    ROCR::plot(perf,lwd=2,avg="threshold", col="blue", add=T);
    
    # calculate p value
    perm.vec <- mSetObj$analSet$ROCtest$perm.res$auc.vec;
    better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
    num <- length(perm.vec);
    if(better.hits == 0) {
      p <- paste("p <", 1/num);
    }else{
      p <- better.hits/num;
      p <- paste("p =", signif(p, digits=5));
    }
    legend("center", legend = paste('Empirical p-value: ', p), bty="n", cex=1.5);
  }else{ # accuracies
    perms <- PreparePermResult(mSetObj$analSet$ROCtest$perm.res$acc.vec);
    perm.vec <- perms$permut;
    perm.p <- perms$permut.p;
    
    op<-par(mar=c(5,5,2,4));
    
    xlim.ext <- GetExtendRange (perm.vec, 10);
    hst <- hist(perm.vec[-1], breaks = "FD", freq=T, ylab="Frequency", xlim=xlim.ext, xaxt="n", xlab= 'Permutation test statistics', col="lightblue", main="");
    axis(1);
    
    # add the indicator using original label
    h <- max(hst$counts)
    arrows(perm.vec[1], h/5, perm.vec[1], 0, col="red", lwd=2);
    text(perm.vec[1], h/3.5, paste('Observed \n statistic \n', perm.p), xpd=T);
    par(op);
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Calculate partial area under ROC curve
#'@description Calculate partial area under ROC curve
#'@param x Input X
#'@param y Input Y
#'@param focus Method
#'@param cutoff Numeric
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.pAUC <- function(x, y, focus, cutoff) {
  
  finite.bool <- is.finite(x) & is.finite(y);
  if(focus == "fpr"){
    x <- x[finite.bool];
    y <- y[finite.bool];
  }else{
    x <- rev(1-y[finite.bool]);
    y <- rev(1-x[finite.bool]);
    cutoff <- 1-cutoff;
  }
  if (length(x) < 2) {
    return (NA);
  }
  
  if (cutoff < 1) {
    ind <- max(which(x <= cutoff));
    stop <- try(approxfun(x[ind:(ind+1)], y[ind:(ind+1)])(cutoff));
    if(class(stop) == "try-error"){
      return(NA);
    }else{
      x <- c(x[1:ind], cutoff);
      y <- c(y[1:ind], stop);
    }
  }
  
  auc <- 0
  for (i in 2:length(x)) {
    auc <- auc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
  }
  return(round(auc,3));
}

#'Compute average ROC curve
#'@description Compute the average ROC curve
#'@param perf Input the average
#'@param avg.method Input the name of the method to compute the average curve
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ComputeAverageCurve<-function(perf, avg.method){
  # now get the average curve
  perf.avg = perf;
  if(avg.method == "vertical"){
    x.values <- seq(min(unlist(perf@x.values)), max(unlist(perf@x.values)),
                    length=max( sapply(perf@x.values, length)))
    for (i in 1:length(perf@y.values)) {
      perf.avg@y.values[[i]] <-
        approxfun(perf@x.values[[i]], perf@y.values[[i]],
                  ties=mean, rule=2)(x.values)
    }
    perf.avg@y.values <- list(rowMeans(data.frame(perf.avg@y.values )))
    perf.avg@x.values <- list(x.values)
  }else if(avg.method == "horizontal"){
    y.values <- seq(min(unlist(perf@y.values)), max(unlist(perf@y.values)),
                    length=max(sapply(perf@y.values, length)))
    for (i in 1:length(perf@x.values)) {
      perf.avg@x.values[[i]] <- approxfun(perf@y.values[[i]],
                                          perf@x.values[[i]],
                                          ties=mean, rule=2)(y.values)
    }
    perf.avg@x.values <- list(rowMeans( data.frame( perf.avg@x.values )));
    perf.avg@y.values <- list(y.values);
  }else{ # threshold
    all.alphas <- unlist(perf@alpha.values);
    min.alpha <- min(all.alphas);
    if(min.alpha == -Inf){
      min.alpha <- 0;
    }
    max.alpha <- max(all.alphas);
    if(max.alpha == Inf){
      max.alpha <- 1.0;
    }
    
    alpha.values <- rev(seq(min.alpha, max.alpha,length=max(sapply(perf@alpha.values, length))));
    perf.sampled <- perf;
    for (i in 1:length(perf.sampled@y.values)) {
      perf.sampled@x.values[[i]] <-
        approxfun(perf@alpha.values[[i]],perf@x.values[[i]],
                  rule=2, ties=mean)(alpha.values)
      perf.sampled@y.values[[i]] <-
        approxfun(perf@alpha.values[[i]], perf@y.values[[i]],
                  rule=2, ties=mean)(alpha.values)
    }
    ## compute average curve
    perf.avg <- perf.sampled
    perf.avg@x.values <- list(rowMeans(data.frame(perf.avg@x.values)))
    perf.avg@y.values <- list(rowMeans(data.frame(perf.avg@y.values)))
  }
  return(perf.avg);
}

#'Compute the 95 percent interval for threshold ROC
#'@description Computes the 95 percent interval only for the y-axis.
#'Utility function, called upon by higher functions 
#'@param perf Input the performance 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ComputeHighLow <- function(perf){
  all.alphas <- unlist(perf@alpha.values);
  min.alpha <- min(all.alphas);
  if(min.alpha == -Inf){
    min.alpha <- 0;
  }
  max.alpha <- max(all.alphas);
  if(max.alpha == Inf){
    max.alpha <- 1.0;
  }
  
  alpha.values <- rev(seq(min.alpha, max.alpha,length=max(sapply(perf@alpha.values, length))));
  perf.sampled <- perf;
  for (i in 1:length(perf.sampled@y.values)) {
    perf.sampled@x.values[[i]] <-
      approxfun(perf@alpha.values[[i]],perf@x.values[[i]],
                rule=2, ties=mean)(alpha.values)
    perf.sampled@y.values[[i]] <-
      approxfun(perf@alpha.values[[i]], perf@y.values[[i]],
                rule=2, ties=mean)(alpha.values)
  }
  ## compute average curve
  y.data <- data.frame(perf.sampled@y.values)
  con.low <- apply(y.data, 1, quantile, 0.05);
  con.high <- apply(y.data, 1, quantile, 0.95);
  res <- list( 
    con.low = con.low,
    con.high = con.high
  );
  return (res);
}

#'Prepare data for ROC analysis
#'@description Prepare data for ROC analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PrepareROCData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(is.null(mSetObj$dataSet$norm.all)){
    mSetObj$dataSet$norm.all <- mSetObj$dataSet$norm;
    mSetObj$dataSet$cls.all<- mSetObj$dataSet$cls;
  }
  
  new.inx <- is.na(mSetObj$dataSet$cls.all) | mSetObj$dataSet$cls.all == "";
  if(sum(new.inx) > 0){
    mSetObj$dataSet$new.samples <- TRUE;
    mSetObj$dataSet$new.data <- mSetObj$dataSet$norm.all[new.inx, ,drop=F];
    mSetObj$dataSet$norm <- mSetObj$dataSet$norm.all[!new.inx, ,drop=F];
    mSetObj$dataSet$cls <- factor(mSetObj$dataSet$cls.all[!new.inx])
  }else{
    mSetObj$dataSet$new.samples <- FALSE;
    mSetObj$dataSet$new.data <- NULL;
    mSetObj$dataSet$norm <- mSetObj$dataSet$norm.all;
    mSetObj$dataSet$cls <- mSetObj$dataSet$cls.all; 
  }
  return(.set.mSet(mSetObj));
}

#'Set custom data
#'@description The "selected.cmpds" should be for extraction
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param selected.cmpds Input the vector containing the compounds
#'@param selected.smpls Input the vector containing the samples
#'@export
#'
SetCustomData <- function(mSetObj=NA, selected.cmpds, selected.smpls){
  
  mSetObj <- .get.mSet(mSetObj);
  
  data.norm.orig <- mSetObj$dataSet$norm;
  cls <- mSetObj$dataSet$cls;
  
  if(length(selected.cmpds) > 0){
    data.norm <- data.norm.orig[, selected.cmpds, drop=F];
    if(!is.null(mSetObj$dataSet$new.data)){
      mSetObj$dataSet$new.data <- mSetObj$dataSet$new.data[, selected.cmpds, drop=F];
    }
  }
  
  if(length(selected.smpls) > 0){
    hit.inx <- rownames(data.norm) %in% selected.smpls;
    mSetObj$dataSet$test.data <- data.norm[hit.inx, ,drop=F];
    mSetObj$dataSet$test.cls <- cls[hit.inx];
    data.norm <- data.norm[!hit.inx, ,drop=F];
    cls <- cls[!hit.inx];
  }else{
    mSetObj$dataSet$test.data <- NULL;
    mSetObj$dataSet$test.cls <- NULL;
  }
  
  mSetObj$dataSet$norm.orig <- data.norm.orig;
  mSetObj$dataSet$norm <- data.norm;
  mSetObj$dataSet$selected.cmpds <- paste(selected.cmpds, collapse="; ");
  mSetObj$dataSet$cls <- cls;
  return(.set.mSet(mSetObj));
}

#'ROC with CI for AUC
#'@description ROC with CI for AUC
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param feat.nm Input the feature name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PrepareROCDetails <- function(mSetObj=NA, feat.nm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  x <- mSetObj$dataSet$norm[, feat.nm];
  y <- mSetObj$dataSet$cls;
  
  roc.res <- pROC::roc(y, x, ci = T, of = "auc");
  
  roc.mat <- as.matrix(data.frame(
    "Cut.Offs" = roc.res$thresholds,
    "Sensitivity" = roc.res$sensitivities,
    "Specificity" = roc.res$specificities,
    "Sens.Spec." = roc.res$sensitivities + roc.res$specificities,
    "LRP" = roc.res$sensitivities/(1-roc.res$specificities),
    "LRN" = (1-roc.res$sensitivities)/roc.res$specificities
  ));
  
  filename <- paste(mSetObj$dataSet$url.var.nms[feat.nm], "_roc.csv", sep="");
  fast.write.csv(signif(roc.mat,4), file=filename, row.names=F);
  # need to clean NA/Inf/-Inf
  #analSet$roc.mat <- ClearNumerics(roc.mat);
  mSetObj$analSet$roc.mat <- signif(roc.mat, 6);
  mSetObj$analSet$roc.obj <- roc.res;
  
  current.feat.nm <<- feat.nm;
  
  return(.set.mSet(mSetObj));
  
  #PNG.PlotUnivROC.CI(imgName);
}

#'Plot detailed ROC
#'@description Plot detailed ROC
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param thresh Input the threshold
#'@param sp Specificity
#'@param se Sensitivity
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param format Select the image format, "png", or "pdf". 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotDetailROC <- function(mSetObj=NA, imgName, thresh, sp, se, dpi=72, format="png"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "_dpi", dpi, ".", format, sep="");

  roc.obj <- mSetObj$analSet$roc.obj;
  
  w <- h <- 6;
  mSetObj$imgSet$roc.univ <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(oma = c(0,0,1,0));
  par(mar=c(4,4,4,4) + .1);
  
  pROC::plot.roc(roc.obj, print.auc=F, legacy.axes=TRUE, col="navy", grid=T,
           xlab = "False positive rate", ylab="True positive rate",
           auc.polygon=TRUE, auc.polygon.col="#0000ff22", main=current.feat.nm);
  
  points(sp, se, cex=1.8, col="red");

  dev.off();
  return(.set.mSet(mSetObj));
  
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################
#'Export biomarker accuracy information
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetAccuracyInfo<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ROCtest$accu.info);
}

#'Get the text description of a recursive partitioning (rpart) result
#'@description x must be an rpart object
#'@param x An Rpart object
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.rpart.summary <- function(x) {
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  node <- as.numeric(row.names(frame))
  
  depth <- floor(log(node, base = 2) + 1e-7)
  depth <- depth - min(depth) # doesn't seem to need as.vector.
  
  indent <- paste(rep(" ", 5 * 32L), collapse = "")
  
  if(length(node) > 1L) {
    indent <- substring(indent, 1L, 5 * seq(depth))
    indent <- paste(c("", indent[depth]), format(node), ")", sep = "")
  } else {
    indent <- paste(format(node), ")", sep = "")
  }
  tfun <- (x$functions)$print
  if (!is.null(tfun)) {
    if (is.null(frame$yval2)){
      yval <- tfun(frame$yval,  ylevel, 4)
    }else{
      yval <- tfun(frame$yval2,  ylevel, 4)
    }
  }else{
    yval <- format(signif(frame$yval, digits = 4))
  }
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels(x, digits=4, minlength=0)
  n <- frame$n
  z <- paste(indent, z, n, format(signif(frame$dev, digits = 4)),
             yval, term);
  
  msg <- NULL;
  if (x$method=="class"){
    msg <- "node), split, n, loss, yval, (yprob)";
  }else {
    msg <- "node), split, n, deviance, yval";
  }
  msg <- c(msg, "      * denotes terminal node\n");
  msg <- paste(c(msg, z), collapse="\n");
  return(msg)
}

#'Compute data points on the ROC curve
#'@description perf is the performance object from ROCR
#'@param perf Performance object from ROCR
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetMeanROC<-function(perf){
  perf@alpha.values <- lapply(perf@alpha.values,
                              function(x) { isfin <- is.finite(x);
                              x[is.infinite(x)] <-
                                (max(x[isfin]) +
                                   mean(abs(x[isfin][-1] -
                                              x[isfin][-length(x[isfin])])));
                              x } );
  ## remove samples with x or y not finite
  for (i in 1:length(perf@x.values)) {
    ind.bool <- (is.finite(perf@x.values[[i]]) & is.finite(perf@y.values[[i]]))
    
    if (length(perf@alpha.values)>0){
      perf@alpha.values[[i]] <- perf@alpha.values[[i]][ind.bool]
    }
    perf@x.values[[i]] <- perf@x.values[[i]][ind.bool]
    perf@y.values[[i]] <- perf@y.values[[i]][ind.bool]
  }
  
  
  perf.sampled <- perf;
  alpha.values <- rev(seq(min(unlist(perf@alpha.values)),
                          max(unlist(perf@alpha.values)),
                          length=max( sapply(perf@alpha.values, length))))
  for (i in 1:length(perf.sampled@y.values)) {
    perf.sampled@x.values[[i]] <- approxfun(perf@alpha.values[[i]],perf@x.values[[i]], rule=2, ties=mean)(alpha.values)
    perf.sampled@y.values[[i]] <- approxfun(perf@alpha.values[[i]], perf@y.values[[i]],rule=2, ties=mean)(alpha.values)
  }
  
  ## return the average value
  return (cbind(alpha.values, rowMeans(data.frame(perf.sampled@x.values)), rowMeans(data.frame(perf.sampled@y.values))));
}

ContainNewSamples <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  ifelse(mSetObj$dataSet$new.samples, 1, 0);
}

#'Obtain sample names and their class labels
#'@description Obtain sample names and their class labels
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetNewSampleNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(!is.null(mSetObj$dataSet$new.data)){
    smplInfo <- paste(rownames(mSetObj$dataSet$new.data), collapse="\n");
  }else{
    smplInfo <- "No new samples found";
  }
  return(smplInfo);
}

GetNewSampleNameVec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$dataSet$new.data);
}

GetNewSampleProbs <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  new.res <- mSetObj$analSet$ROCtest$new.res;
  # new res is the prob for class in level 1, 
  # need to adjust it
  lvls <- levels(mSetObj$dataSet$cls);
  grps <- ifelse(mSetObj$analSet$ROCtest$new.res >= 0.5, lvls[2], lvls[1]);
  new.res <- ifelse(new.res >= 0.5, new.res, 1-new.res);
  return(round(new.res, 5));
}

GetNewSampleGrps <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # need to figure out which is 1 or 0
  lvls <- levels(mSetObj$dataSet$cls);
  grps <- ifelse(mSetObj$analSet$ROCtest$new.res >= 0.5, lvls[2], lvls[1]);
  return(grps);
}

Get.Fisher <- function(x, fac, var.equal=TRUE) {
  inx1 <- which(y==levels(y)[1]);
  inx2 <- which(y==levels(y)[2]);
  p.value <- apply(as.matrix(x), 2,
                   function(x) {
                     tmp <- try(fisher.test(x[inx1], x[inx2]));
                     if(class(tmp) == "try-error") {
                       return(NA);
                     }else{
                       return(tmp$p.value);
                     }
                   });
  -log10(p.value);
}

Get.Fstat <-  function(x, fac, var.equal=TRUE) {
  
  x = t(x);
  sqr = function(x) x*x;
  stopifnot(length(fac)==ncol(x), is.factor(fac), is.matrix(x))
  x   <- x[,!is.na(fac), drop=FALSE]
  fac <- fac[!is.na(fac)]
  
  ## Number of levels (groups)
  k <- nlevels(fac)
  
  ## xm: a nrow(x) x nlevels(fac) matrix with the means of each factor level
  xm <- matrix(
    sapply(levels(fac), function(fl) rowMeans(x[,which(fac==fl), drop=FALSE])),
    nrow = nrow(x),
    ncol = nlevels(fac))
  
  ## x1: a matrix of group means, with as many rows as x, columns correspond to groups
  x1 <- xm[,fac, drop=FALSE]
  
  ## degree of freedom 1
  dff    <- k - 1
  
  ## x0: a matrix of same size as x with overall means
  x0 <- matrix(rowMeans(x), ncol=ncol(x), nrow=nrow(x))
  
  ## degree of freedom 2
  dfr    <- ncol(x) - dff - 1
  
  ## mean sum of squares
  mssf   <- rowSums(sqr(x1 - x0)) / dff
  mssr   <- rowSums(sqr( x - x1)) / dfr
  
  ## F statistic
  fstat  <- mssf/mssr
  return(fstat)
}

#'Return ROC corodinates with confidence intervals
#'@description Return ROC corodinates with confidence intervals
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fld.nm The kind of input coordinate
#'@param val The coordinates to look for
#'@param plot Logical, by default set to TRUE
#'@param imgNm Input the image name
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetROC.coords <- function(mSetObj=NA, fld.nm, val, plot=TRUE, imgNm){
  
  mSetObj <- .get.mSet(mSetObj);
  res <- pROC::coords(mSetObj$analSet$roc.obj, val, input=fld.nm, transpose=TRUE);
  
  sp <- res[2];
  se <- res[3];
  res <- round(res, 3);
  
  mSetObj$analSet$thresh.obj <- NULL;
  if(fld.nm == "threshold"){
    ci.s <- pROC::ci.thresholds(mSetObj$analSet$roc.obj, boot.n=100, thresholds=val, progress="none");
    specs <- round(ci.s$specificity,3);
    sens <- round(ci.s$sensitivity, 3);
    res[2] <- paste(res[2], "(", specs[1], "-", specs[3], ")", sep="");
    res[3] <- paste(res[3], "(", sens[1], "-", sens[3], ")", sep="");
    
    mSetObj$analSet$thresh.obj <- ci.s;
    
    # update pos with the thresh obj coords
    sp <- ci.s$specificity[2];
    se <- ci.s$sensitivity[2];
  }
  
  mythresh <- res[1];
  if(is.na(res[1])){
    if(fld.nm == "sensitivity"){
      fld.vals <- mSetObj$analSet$roc.obj$sensitivities;
    }else{
      fld.vals <- mSetObj$analSet$roc.obj$specificities;
    }
    
    inx1 <- which.min(abs(fld.vals-val));
    
    if(inx1 == 1){
      inxb1 <- inx1;
    }else{
      inxb1 <- inx1 - 1;
    }
    
    if(inx1 == length(fld.vals)){
      inxb2 <- inx1;
    }else{
      inxb2 <- inx1 + 1;
    }
    
    if(fld.vals[inx1] > val){
      inx2 <-ifelse(fld.vals[inxb1] > val, inxb2, inxb1);
    }else{
      inx2 <-ifelse(fld.vals[inxb1] > val, inxb1, inxb2);
    }
    
    threshs <- mSetObj$analSet$roc.obj$thresholds;
    
    if(inx1 == inx2){ # out of the threshod range
      if(threshs[inx1] > threshs[2]){ #max
        res[1] <- paste(">",threshs[inx1], sep="");
      }else{
        res[1] <- paste("<",threshs[inx1], sep="");
      }
    }else{
      inx.thresh <- c(inx1, inx2)
      ord.inx <- order(threshs[inx.thresh]);
      inx.thresh <- inx.thresh[ord.inx];
      res[1] <- paste(threshs[inx.thresh], collapse="-")
    }
  }
  if(plot){
    PlotDetailROC(mSetObj, imgNm, mythresh, sp, se);
    mSetObj$analSet$roc.obj$thresh <- mythresh;
  }
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(res);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Compute lasso frequency
#'@description Not part of default, need to perform function 
#'to compute lasso frequency
#'msg: There are more than 500 variables and n<m
#'You may wish to restart and set use.Gram=FALSE
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetLassoFreqs <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(ncol(mSetObj$dataSet$norm) < 500){
    lassoFreq <- try(GetROCLassoFreq(mSetObj$dataSet$norm, mSetObj$dataSet$cls));
    if(class(lassoFreq) == "try-error"){
      err.msg <<- "Unknown errors occured during computing lasso!";
      lassoFreq <- rep(0, ncol(mSetObj$dataSet$norm));
    }
  }else{
    err.msg <<- "Too many variables (>500) with small sample size, computing aborted!";
    lassoFreq <- rep(0, ncol(mSetObj$dataSet$norm));
  }
  names(lassoFreq) <- colnames(mSetObj$dataSet$norm);
  lassoFreq <- sort(lassoFreq, decreasing =TRUE);
  lassoFreq <<- lassoFreq;
  return(lassoFreq);
}

#'Get p-values for ROC
#'@description ROC p-vaues, used in higher function
#'@param data Input data
#'@param cls Input class labels
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetROCTtestP <- function(data, cls){
  if(ncol(data) < 1000){
    inx1 <- which(cls==levels(cls)[1]);
    inx2 <- which(cls==levels(cls)[2]);
    p.value <- apply(as.matrix(data), 2, function(x) {
      tmp <- try(t.test(x[inx1], x[inx2], paired = F, var.equal = T));
      if(class(tmp) == "try-error") {
        return(NA);
      }else{
        return(tmp$p.value);
      }
    })
  }else{ # use fast version
    p.value <- try(genefilter::rowttests(t(as.matrix(data)), cls)$p.value);
    if(class(p.value) == "try-error") {
      p.value <- NA;
    }
  }
  return(p.value);
}

#'Separate data set using k-fold cross validation (CV)
#'@description Separate data set with k-fold CV, used in higher function
#'@param groupN Input the size of the group
#'@param kfold Input the number of cross-validations
#'@param rseed Input the random seed
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
createCVset <- function(groupN, kfold, rseed)
{
  set.seed(rseed)    
  idxlist <- sample(1:groupN, size=groupN, replace=FALSE)
  CVsize <- floor(groupN / kfold)
  CV.groupIndexes <- vector(mode="list", length=kfold)
  for (i in 1:kfold) {
    CV.groupIndexes[i] <- list(idxlist[(1+CVsize*(i-1)):(CVsize*i)])
  }
  
  if((groupN %% kfold) > 0) {
    i<-1
    while( i <= (groupN %% kfold) ) {
      CV.groupIndexes[[i]] <- c(CV.groupIndexes[[i]], idxlist[CVsize*kfold + i])
      i <- i+1
    }
  }
  
  return (CV.groupIndexes)
}

#'Get p-values from lasso 
#'@description Get p-values from lasso 
#'@param data Input data
#'@param cls Input class labels
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetROCLassoFreq <- function(data, cls){

  data <- cbind(cls, data);
  d.headers <- names(data);
  names(data) <- c("cls", paste0("V", 1:(ncol(data)-1)));
  
  inx1 <- which(cls==levels(cls)[1]);
  inx2 <- which(cls==levels(cls)[2]);
  data1 <- data[inx1, ];
  data2 <- data[inx2, ];
  
  min.n <- ifelse(length(inx1) <= length(inx2), length(inx1), length(inx2));
  kfold.origin <- ifelse(min.n >= 10, 10, ifelse(min.n >= 5, 5, 0));
  
  if(kfold.origin > 0) {
    random.seed <- 10063927;
    kfold <- kfold.origin;
    CV.inx1 <- createCVset(length(inx1), kfold, random.seed);
    CV.inx2 <- createCVset(length(inx2), kfold, random.seed);        
  } else {
    kfold <- 10;
  }
  
  varlist <- NULL;
  for (i in 1:kfold) {
    # i<-1
    if (kfold.origin > 0) {
      dw1 <- data1[-CV.inx1[[i]], ]; 
      dw2 <- data2[-CV.inx2[[i]], ]; 
    } else {
      # resampling if sample size is less than 5 
      CV.inx1 <- sample(inx1, size=length(inx1), replace=TRUE);
      CV.inx2 <- sample(inx2, size=length(inx2), replace=TRUE);
      dw1 <- data1[as.character(CV.inx1), ]; 
      dw2 <- data2[as.character(CV.inx2), ]; 
    }
    
    dw.all <- rbind(dw1, dw2);   
    #rownames(dw.all) <- c(1:nrow(dw.all))
    
    # To create a formula for model with large number of independent vars
    xnam <- names(dw.all)[-1];
    (fmla <- as.formula(paste("cls ~ ", paste(xnam, collapse= "+"))));
    
    if (is.numeric(dw.all$cls)) {
      dw.all$cls <- as.numeric(as.character(dw.all$cls)); 
    } else {
      # label/cls should be integer as 0 and 1
      dw.all$cls <- as.numeric(dw.all$cls)-1; 
    }
    
    x <- model.matrix(as.formula(fmla), dw.all)[, -1];
    o <- lars::lars(x, dw.all$cls, type="lasso", trace=FALSE, intercept=TRUE, normalize=FALSE, use.Gram=FALSE);
    
    cvfit <- NULL;
    m <- NULL;
    tryCatch({
      cvfit <- lars::cv.lars(x, dw.all$cls, type="lasso", mode="fraction", plot.it=FALSE); ## Cross-Validation              
      m <- ( o$beta[which.min(cvfit$cv),] != 0 );
      varlist10 <- names(m[m==TRUE]);
    }, error = function(e) {
      ##  print(e); 
      tryCatch ( {
        cvfit <- lars::cv.lars(x, dw.all$cls, type="lar", mode="step", plot.it=FALSE); 
        m <- ( o$beta[which.min(cvfit$cv),] != 0 );
      }, error=function(e) {
        ## print(e); 
      }, finally = {
        if(is.null(cvfit)) {
          m <- ( o$beta[5,] != 0 );
        }
      })
    }, finally = {
      # cat("\n Finished CV Lasso\n"); 
    })
    
    varlist <- c(varlist, names(m[m==TRUE]));
  }
  var.lasso.10CV <- unique(varlist); 
  dt <- table(varlist); 
  
  compnames <- names(dw.all)[-1];
  selfreq <- 0.0;
  compselfreq <- cbind(compnames, selfreq);
  for (i in 1:length(dt)) {
    compselfreq[which(compnames == names(dt[i])), "selfreq"] <- dt[i]/kfold * 100.0;
  }
  
  return(as.numeric(compselfreq[,"selfreq"]));
}

#'Get important feature matrix
#'@description feat.outp is a list that contains the ranked features in each 
#'cross validation (CV) and returns a two column matrix, col 1 = median ranking 
#'and col 2 =  mean importance measure
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param feat.outp Input the list that contains the ranked features in each 
#'cross validation (CV) and returns a two column matrix, col 1 = median ranking 
#'and col 2 =  mean importance measure
#'@param bestFeatNum Numeric
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetImpFeatureMat <- function(mSetObj=NA, feat.outp, bestFeatNum){
  
  mSetObj <- .get.mSet(mSetObj);
  anal.mode <- mSetObj$analSet$mode; 
  
  # first order each run by cmpd names so that can be combined to a data frame
  feat.outp <- lapply(feat.outp, function(x) x[order(names(x))]);
  
  ####################################################################
  # First rank by frequencies of being selected in the given model ###
  ####################################################################
  # obtain their ranks
  freqRank <- lapply(feat.outp, function(x) rank(-x));
  runRanksMat <- do.call("cbind", freqRank);
  
  # order by their median rank across runs
  ordRunRanksMat <- as.data.frame(runRanksMat[order(apply(runRanksMat, 1, median)),]);
  
  # Then rank by mean importance measures
  impsMat <- as.data.frame(do.call("cbind", feat.outp));
  impsVec <- apply(impsMat, 1, mean);
  
  # if(anal.mode == "explore"){
    # now count the number being selected in the bestFeatNum
    selectedMat <- apply(ordRunRanksMat, 2, function(x) x <= bestFeatNum);
  # }else{
  #   selectedMat <- ordRunRanksMat;
  # }
  
  # calculate percentage of being selected in the best subsets
  percentVec <- apply(selectedMat, 1, sum)/ncol(ordRunRanksMat);
  
  # remove ones never being selected
  percentVec <- percentVec[percentVec > 0];
  
  # reorder the imps to percentVec
  impsVec <- impsVec[names(percentVec)];
  
  ###################################
  # combine and return the result
  ####################################
  imp.mat <- cbind(percentVec, impsVec);
  ord.inx <- order(imp.mat[,1], imp.mat[,2], decreasing=T);
  imp.mat <- imp.mat[ord.inx,];
  colnames(imp.mat) <- c("Rank Freq.", "Importance");
  
  return(imp.mat);
}

#'Calculate variable importance of projection (VIP) score for PLS object
#'@description Users give a pls object ('oscorespls'=T), function calculates VIP score
#'usually one VIP for each component, return is the average of all VIP
#'@param pls.obj Input the PLS object
#'@param comp Numeric, input the number of components, by default it is 2
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.VIP <- function(pls.obj, comp=2){
  # only use the top two comps
  b <- c(pls.obj$Yloadings)[1:comp];
  T <- pls.obj$scores[,1:comp, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- pls.obj$loading.weights[,1:comp, drop = FALSE]
  Wnorm2 <- colSums(W^2);
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
  if(is.null(dim(vips))){
    vip.mns<-vips;
  }else{
    vip.mns<-apply(vips, 2, mean);      
  }
  vip.mns;
}

GetLR_clsLbl <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(paste(mSetObj$dataSet$cls.lbl, collapse="/"));
}

GetLR_clsLblNew <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(paste(mSetObj$dataSet$cls.lbl.new, collapse="/"));
}

GetLassoFreqNames <- function(){
  return(names(lassoFreq));
}


GetLRConvergence <- function(){
  return(LRConverged);
}

GetLREquation <- function(){
  return(LReq);
}

GetLRmodelTable <- function(){
  return(LRmodel.xtable);
}

GetLRperformTable <- function() {
  return(LRperf.xtable);
}

GetLRthreshold <- function() {
  return(round(LR.threshold,2));
}

GetCurrentConfMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$conf.mat);
}
GetCurrentConfMatTest <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$conf.mat.test);
}

GetImpHighLow <- function(mSetObj=NA, inx){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$lowhigh[,levels(mSetObj$dataSet$cls)[inx]];
}

GetImpRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$imp.mat);
}

GetImpColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$imp.mat);
}

GetImpValues <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(mSetObj$analSet$imp.mat);
}

GetRocSigFileName <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$analSet$roc.sig.nm
}

GetModelNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  test.nums <- mSetObj$analSet$multiROC$test.feats;
  paste("Model", 1:length(test.nums), "(", test.nums, "features)");
}

GetBestModelIndex <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$multiROC$best.model.inx;
}

GetUnivRankedFeatureNames <- function(){
  rownames(feat.rank.mat);
}

# do the ordering before return
GetFeatureRankingMat <- function(){
  feat.rank.mat;
}

Get.Accuracy <- function(cm) {
    sum(diag(cm)) / sum(cm);
}

#'Merge duplicated columns or rows by their mean
#'@description dim 1 => row,  dim 2 => column
#'@param data Input the data
#'@param dim Numeric, input the dimensions, default is set to 2
#'@export
#'
MergeDuplicates <- function(data, dim=2){
  
  if(is.null(dim(data))){ # a vector
    if(is.null(names(data))){
      print("Cannot detect duplicate data without names!!!");
      return();
    }
    nm.cls <- as.factor(names(data));
    uniq.len <- length(levels(nm.cls));
    if(uniq.len == length(data)){
      return(data);
    }
    new.data <- vector (mode="numeric",length=uniq.len);
    for(i in 1:uniq.len){
      dup.inx <- nm.cls == levels(nm.cls)[i];
      new.data[i] <- mean(data[dup.inx]);
    }
    names(new.data) <- levels(nm.cls);
    rem.len <- length(data) - length(new.data);
  }else{
    if(dim == 1){
      data <- t(data);
    }
    if(is.null(colnames(data))){
      print("Cannot detect duplicate data without var names!!!");
      return();
    }
    
    nm.cls <- as.factor(colnames(data));
    uniq.len <- length(levels(nm.cls));
    
    if(uniq.len == ncol(data)){
      if(dim == 1){
        data <- t(data);
      }
      return(data);
    }
    
    new.data <- matrix (nrow=nrow(data), ncol=uniq.len);
    for(i in 1:uniq.len){
      dup.inx <- which(nm.cls == levels(nm.cls)[i]);
      new.data[,i] <- apply(data[,dup.inx, drop=F], 1, mean);
    }
    rownames(new.data) <- rownames(data);
    colnames(new.data) <- levels(nm.cls);
    
    rem.len <- ncol(data) - ncol(new.data);
    if(dim == 1){
      new.data <- t(new.data);
    }
  }
  print(paste(rem.len, "duplicates are merged to their average"));
  new.data;
}
