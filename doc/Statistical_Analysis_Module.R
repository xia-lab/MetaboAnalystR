## ------------------------------------------------------------------------
library(MetaboAnalystR)
mSet<-InitDataObjects("conc", "stat", FALSE);
mSet<-Read.TextData(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
mSet<-SanityCheckData(mSet);
mSet<-ReplaceMin(mSet);
mSet<-PreparePrenormData(mSet);
mSet<-Normalization(mSet, "NULL", "LogNorm", "MeanCenter", "S10T0", ratio=FALSE, ratioNum=20);
mSet<-PlotNormSummary(mSet, "norm_0_", format ="png", dpi=72, width=NA);
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", format = "png", dpi=72, width=NA);


## ------------------------------------------------------------------------
# Perform fold-change analysis on uploaded data, unpaired
mSet<-FC.Anal.unpaired(mSet, 2.0, 0)

# Plot fold-change analysis
mSet<-PlotFC(mSet, "fc_0_", "png", 72, width=NA)

# To view fold-change 
mSet$analSet$fc$fc.log


## ------------------------------------------------------------------------
# Perform T-test (parametric)
mSet<-Ttests.Anal(mSet, nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE)

# Plot of the T-test results
mSet<-PlotTT(mSet, imgName = "tt_0_", format = "png", dpi = 72, width=NA)


## ------------------------------------------------------------------------
# Perform the volcano analysis
mSet<-Volcano.Anal(mSet, FALSE, 2.0, 0, 0.75,F, 0.1, TRUE, "raw")

# Create the volcano plot
mSet<-PlotVolcano(mSet, "volcano_0_", 1, format ="png", dpi=72, width=NA)


## ---- eval=FALSE---------------------------------------------------------
#  # Perform ANOVA
#  mSet <- ANOVA.Anal(mSet, F, 0.05, "fisher")
#  
#  # Plot ANOVA
#  mSet <- PlotANOVA(mSet, "aov_0_", "png", 72, width=NA)
#  

## ------------------------------------------------------------------------
### OPTION 1 - Heatmap specifying pearson distance and an overview
mSet<-PlotCorrHeatMap(mSet, "corr_0_", format = "png", dpi=72, width=NA, "col", "pearson", "bwm", "overview", F, F, F, 100)


## ---- eval=FALSE---------------------------------------------------------
#  ### OPTION 2 - Heatmap specifying pearson correlation and a detailed view
#  mSet<-PlotCorrHeatMap(mSet, "corr_1_", format = "png", dpi=72, width=NA, "col", "spearman", "bwm", "detail", F, F, F, 999)
#  

## ------------------------------------------------------------------------
# Perform correlation analysis on a pattern (a feature of interest in this case)
mSet<-FeatureCorrelation(mSet, "pearson", "1,6-Anhydro-beta-D-glucose")

# Plot the correlation analysis on a pattern
mSet<-PlotCorr(mSet, "ptn_3_", format="png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform PCA analysis
mSet<-PCA.Anal(mSet)

# Create PCA overview
mSet<-PlotPCAPairSummary(mSet, "pca_pair_0_", format = "png", dpi = 300, width=NA, 5)

# Create PCA scree plot
mSet<-PlotPCAScree(mSet, "pca_scree_0_", "png", dpi = 72, width=NA, 5)

# Create a 2D PCA score plot
mSet<-PlotPCA2DScore(mSet, "pca_score2d_0_", format = "png", dpi=300, width=NA, 1, 2, 0.95, 1, 0)

# Create a 3D PCA score plot
mSet<-PlotPCA3DScoreImg(mSet, "pca_score3d_0_", "png", 72, width=NA, 1,2,3, 40)

# Create a PCA loadings Plots
mSet<-PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2);

# Create a PCA Biplot
mSet<-PlotPCABiplot(mSet, "pca_biplot_0_", format = "png", dpi = 72, width=NA, 1, 2)


## ---- eval=FALSE---------------------------------------------------------
#  # View the 3D interactive PLS-DA score plot
#  mSet$imgSet$pca.3d
#  

## ------------------------------------------------------------------------
mSet<-PLSR.Anal(mSet, reg=TRUE)

mSet<-PlotPLSPairSummary(mSet, "pls_pair_0_", "png", 72, width=NA, 5)

mSet<-PlotPLS2DScore(mSet, "pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)

mSet<-PlotPLS3DScoreImg(mSet, "pls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)

mSet<-PlotPLSLoading(mSet, "pls_loading_0_", "png", 72, width=NA, 1, 2);

mSet<-PLSDA.CV(mSet, "T",5, "Q2")

mSet<-PlotPLS.Classification(mSet, "pls_cv_0_", "png", 72, width=NA)

mSet<-PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)

mSet<-PLSDA.Permut(mSet, 100, "accu")

mSet<-PlotPLS.Permutation(mSet, "pls_perm_1_", "png", 72, width=NA)


## ---- eval=FALSE---------------------------------------------------------
#  # View the 3D interactive PLS-DA score plot
#  mSet$imgSet$plsda.3d
#  

## ------------------------------------------------------------------------
# Perform sPLS-DA analysis
mSet<-SPLSR.Anal(mSet, 5, 10, "same")

# Plot sPLS-DA overview
mSet<-PlotSPLSPairSummary(mSet, "spls_pair_0_", format = "png", dpi=72, width=NA, 5)

# Create 2D sPLS-DA Score Plot
mSet<-PlotSPLS2DScore(mSet, "spls_score2d_0_", format = "png", dpi=72, width=NA, 1, 2, 0.95, 1, 0)

# Create 3D sPLS-DA Score Plot
mSet<-PlotSPLS3DScoreImg(mSet, "spls_score3d_0_", format = "png", 72, width=NA, 1, 2, 3, 40)

# Create sPLS-DA loadings plot
mSet<-PlotSPLSLoading(mSet, "spls_loading_0_", format = "png", dpi=72, width=NA, 1,"overview")

# Perform cross-validation and plot sPLS-DA classification
mSet<-PlotSPLSDA.Classification(mSet, "spls_cv_0_", "Mfold", format = "png", dpi=72, width=NA)


## ---- eval=FALSE---------------------------------------------------------
#  # View the 3D interactive PLS-DA score plot
#  mSet$imgSet$splsda.3d
#  

## ------------------------------------------------------------------------
# Perform oPLS-DA analysis
mSet<-OPLSR.Anal(mSet, reg=TRUE)

# Create a 2D oPLS-DA score plot
mSet<-PlotOPLS2DScore(mSet, "opls_score2d_0_", format = "png", dpi=72, width=NA, 1,2,0.95,1,0)

# Create a significant features plot
mSet<-PlotOPLS.Splot(mSet, "opls_splot_0_", "png", 72, width=NA);

# Create a plot of the model overview
mSet<-PlotOPLS.MDL(mSet, "opls_mdl_0_", format = "png", dpi=72, width=NA)

# Perform and plot oPLS-DA permutation 
mSet<-PlotOPLS.Permutation(mSet, "opls_perm_2_", format = "png", dpi=72, 100, width=NA)

## ------------------------------------------------------------------------
# Perform SAM analysis
mSet<-SAM.Anal(mSet, "d.stat", FALSE, TRUE)

# Create the SAM matrix of signifiant features
mSet<-SetSAMSigMat(mSet, 1.1)

# Create a SAM plot of FDR values
mSet<-PlotSAM.FDR(mSet, 1.1, "sam_view_0_", format = "png", dpi=72, width=NA)

# Create a SAM plot of results
mSet<-PlotSAM.Cmpd(mSet, "sam_imp_0_", format = "png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform EBAM analysis
mSet<-EBAM.A0.Init(mSet, FALSE, TRUE)

# Plot EBAM analysis
mSet<-PlotEBAM.A0(mSet, "ebam_view_0_", format = "png", dpi=72, width=NA)

# Initialize EBAM compound analysis 
mSet<-EBAM.Cmpd.Init(mSet, "z.ebam", 0.0, FALSE, TRUE)

# Create the EBAM matrix of significant features
mSet<-SetEBAMSigMat(mSet, 0.9);

# Create a EBAM plot of results
mSet<-PlotEBAM.Cmpd(mSet, "ebam_imp_0_", format = "png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform hierarchical clustering and plot dendogram
mSet<-PlotHCTree(mSet, "tree_0_", format = "png", dpi=72, width=NA, "euclidean", "ward.D")


## ------------------------------------------------------------------------
# Perform hierarchical clustering and plot heat map
mSet<-PlotHeatMap(mSet, "heatmap_0_", format = "png", dpi=72, width=NA, "norm", "row", "euclidean", "ward.D","bwm", "overview", T, T, NA, T, F)


## ------------------------------------------------------------------------
# Perform K-means analysis
mSet<-Kmeans.Anal(mSet, 3)

# Plot K-means analysis 
mSet<-PlotKmeans(mSet, "km_0_", format = "png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform SOM analysis
mSet<-SOM.Anal(mSet, 1, 3,"linear","gaussian")

# Plot SOM analysis
mSet<-PlotSOM(mSet, "som_0_", format = "png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform random forest analysis
mSet<-RF.Anal(mSet, 500, 7, 1)

# Plot random forest classification
mSet<-PlotRF.Classify(mSet, "rf_cls_0_", format = "png", dpi=72, width=NA)

# Plot random forest variables of importance
mSet<-PlotRF.VIP(mSet, "rf_imp_0_", format = "png", dpi=72, width=NA)

# Plot random forest outliers 
mSet<-PlotRF.Outlier(mSet, "rf_outlier_0_", format = "png", dpi=72, width=NA)


## ------------------------------------------------------------------------
# Perform SVM 
mSet<-RSVM.Anal(mSet, 10)

mSet<-PlotRSVM.Classification(mSet, "svm_cls_0_", format = "png", dpi=72, width=NA)

mSet<-PlotRSVM.Cmpd(mSet, "svm_imp_0_", format = "png", dpi=72, width=NA)


