## ------------------------------------------------------------------------
library(MetaboAnalystR)

# Create objects for storing processed data from biomarker analysis 
mSet<-InitDataObjects("conc", "roc", FALSE)

# Read in data and fill in the dataSet list
mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/plasma_nmr_new.csv", "rowu", "disc")

# Sanity check, replace missing values, check if the sample size is too small
mSet<-SanityCheckData(mSet)
mSet<-ReplaceMin(mSet)
mSet<-IsSmallSmplSize(mSet)
mSet<-PreparePrenormData(mSet)

###### *** OPTION 1 FOR NORMALIZATION
# Perform no normalization, no ratio calculation
mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ref=NULL, ratio=FALSE, ratioNum=20)


## ---- eval=FALSE---------------------------------------------------------
#  ###### *** OPTION 2 FOR NORMALIZATION
#  # No normalization, and computeS metabolite ratios and includeS the top 20
#  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "C01", ratio=TRUE, ratioNum=20)
#  
#  # If ratio = TRUE: view the normalized dataset including the top ranked ratios
#  # The ratios will be towards the end of the matrix
#  mSet$dataSet$norm
#  
#  #If ratio = TRUE: view just the top ranked included ratios
#  mSet$dataSet$ratio
#  

## ---- eval=FALSE---------------------------------------------------------
#  # Set the biomarker analysis mode to perform Classical ROC curve analysis ("univ")
#  mSet<-SetAnalysisMode(mSet, "univ")
#  
#  # Prepare data for biomarker analysis
#  mSet<-PrepareROCData(mSet)
#  
#  ### OPTION 1 Perform univariate ROC curve analysis ###
#  mSet<-Perform.UnivROC(mSet, feat.nm = "Isoleucine", imgName = "Isoleucine", "png", dpi=300, isAUC=F, isOpt=T, optMethod="closest.topleft", isPartial=F, measure="sp", cutoff=0.2)
#  
#  # Perform calculation of feature importance (AUC, p value, fold change)
#  mSet<-CalculateFeatureRanking(mSet)
#  

## ---- eval=FALSE---------------------------------------------------------
#  ### OPTION 2 Perform univariate ROC curve analysis, resulting in a partial AUC with a 95% CI band ###
#  mSet<-Perform.UnivROC(mSet, feat.nm = "Valine", imgName = "Valine", "png", dpi=300, isAUC=T, isOpt=T, optMethod="closest.topleft", isPartial=T, measure="se", cutoff=0.2)
#  
#  ### OPTION 3 Perform univariate ROC curve analysis on a metabolite ratio pair, note that you cannot save an image with a "\" in the name ###
#  mSet<-Perform.UnivROC(mSet, feat.nm = "Isoleucine/Valine", imgName = "IsoleucineValine", "png", dpi=300, isAUC=T, isOpt=T, optMethod="closest.topleft", isPartial=T, measure="se", cutoff=0.2)
#  

## ------------------------------------------------------------------------
# Set the biomarker analysis mode to perform Multivariate exploratory ROC curve analysis ("explore")
mSet<-SetAnalysisMode(mSet, "explore")

# Prepare data for biomarker analysis
mSet<-PrepareROCData(mSet)

# Perform multivariate ROC curve analysis, using SVM classification and ranking
mSet<-PerformCV.explore(mSet, cls.method = "svm", rank.method = "svm", lvNum = 2)

### OPTION 1 Comparison plot of ROC curves of all models ###
mSet<-PlotROC(mSet, imgName = "ROC_all_models", format = "png", dpi = 300, mdl.inx= 0, avg.method = "threshold", show.conf = 0, show.holdout = 0, focus="fpr", cutoff=0.5)

# Plot predicted class probabilities for each sample for a selected model, not showing labels of wrongly classified samples
mSet<-PlotProbView(mSet, imgName = "multi_roc_prob", format = "png", dpi = 300, mdl.inx = -1, show = 0, showPred = 0)

# Plot the predictive accuracy of models with increasing number of features
mSet<-PlotAccuracy(mSet, imgName = "multi_roc_accuracy", format = "png", dpi = 300)

# Plot the most important features of a selected model ranked from most to least important
mSet<-PlotImpVars(mSet, imgName = "multi_roc_impvar", format="png", dpi=300, mdl.inx = -1, measure="freq", feat.num=15)


## ---- eval=FALSE---------------------------------------------------------
#  ### OPTION 2 Plot the ROC curve of a single selected model, in this case model 1 and display the confidence interval ###
#  mSet<-PlotROC(mSet, imgName = "ROC_model1", format = "png", dpi = 300, mdl.inx = 1, avg.method = "threshold", show.conf = 1, 0, "fpr", 0.2)
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  imp.feats <- GetImpFeatureMat(mSet, mSet$analSet$multiROC$imp.cv, 10)
#  
#  head(imp.feats)
#  
#  #                       Rank Freq. Importance
#  # Glycerol                     0.82 0.15103973
#  # Methionine                   0.60 0.08181467
#  # Acetate                      0.40 0.06299938
#  # Betaine                      0.36 0.06840905
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Set the biomarker analysis mode to perform ROC Curve Based Model Creation and Evaluation ("test")
#  mSet<-SetAnalysisMode(mSet, "test")
#  
#  # Prepare data for biomarker analysis
#  mSet<-PrepareROCData(mSet)
#  
#  # Perform calculation of feature importance (AUC, p value, fold change)
#  mSet<-CalculateFeatureRanking(mSet)
#  
#  # Manually select a subset of features for ROC analysis to build a classifier
#  selected.cmpds <- c("Betaine", "N,N-Dimethylglycine", "Quinolinate", "Glucose")
#  
#  # Manually select a subset of samples for ROC analysis hold-out data for validation purposes
#  selected.smpls <- c("PIF_178", "PIF_087", "PIF_090", "PIF_102", "PIF_111", "PIF_112")
#  
#  # Prepare the custom data for model creation and sample hold-out
#  mSet<-SetCustomData(mSet, selected.cmpds, selected.smpls)
#  
#  # Perform ROC curve analysis, using SVM classification
#  mSet<-PerformCV.test(mSet, method = "svm", lvNum = 2)
#  
#  # Plot the ROC curve for the created model
#  mSet<-PlotROC(mSet, imgName = "cls_roc_0_", format="png",  dpi=300, mdl.inx = 0, avg.method = "threshold", 0, 0, "fpr", 0.5)
#  
#  # Plot the predicted class probabilities for each sample using the user-created classifier, not showing labels of wrongly classified samples
#  mSet<-PlotProbView(mSet, imgName = "cls_prob_0_", format="png",  dpi=300, mdl.inx =-1, show=0, showPred= 0)
#  
#  # Plot the predictive accuracy of the model with increasing number of features
#  mSet<-PlotTestAccuracy(mSet, imgName = "cls_accu_0_", format="png",  dpi=300)
#  
#  # Perform permutations tests using the area under the ROC curve as a measure of performance
#  mSet<-Perform.Permut(mSet, perf.measure = "auroc", perm.num = 500, propTraining = 2/3)
#  
#  # Plot the results of the permutation tests
#  mSet<-Plot.Permutation(mSet, imgName = "roc_perm_1_", format="png",  dpi=300)
#  
#  # View predicted classes of new samples (only applicable if samples with empty class labels were in the uploaded dataset)
#  mSet <- ROCPredSamplesTable(mSet) # Create table

## ---- eval=FALSE---------------------------------------------------------
#  To view the example of the results:
#  # View the the predictive accuracy results of the model
#  # >R GetAccuracyInfo(mSet)
#  [1] "The average accuracy based on 100 cross validations is 0.692. The accuracy for hold out data prediction is 0.667(4/6)."
#  
#  # View table of new predictions
#  # >R mSet$analSet$ROCtest$pred.samples.table

