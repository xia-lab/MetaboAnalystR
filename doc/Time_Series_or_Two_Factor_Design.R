## ------------------------------------------------------------------------
library(MetaboAnalystR)
mSet<-InitDataObjects("pktable", "ts", FALSE);
mSet<-SetDesignType(mSet, "time");
mSet<-Read.TextData(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/cress_time.csv", "colts", "disc");
mSet<-SanityCheckData(mSet);
mSet<-ReplaceMin(mSet);
mSet<-PreparePrenormData(mSet);
mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "S10T0", ratio=FALSE, ratioNum=20);
mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA);
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA);


## ------------------------------------------------------------------------
# Create an Interactive PCA Plot
mSet<-iPCA.Anal(mSet, "ipca_3d_0_.json")

# Create a Heatmap
mSet<-PlotHeatMap2(mSet, "heatmap2_0_", "png", 72, width=NA, "euclidean","ward.D","bwm","overview", F, 1, F, F)

## ---- eval=FALSE---------------------------------------------------------
#  # View the interactive scores plot
#  mSet$imgSet$time$score3d
#  
#  # View the interactive loadings plot
#  mSet$imgSet$time$load3d

## ------------------------------------------------------------------------
# Perform ANOVA2 Analysis
mSet<-ANOVA2.Anal(mSet, thresh=0.05, p.cor="fdr", type="time")

# Plot ANOVA2
mSet<-PlotANOVA2(mSet, "aov2_0_", "png", 72, width=NA)

## ------------------------------------------------------------------------
# Perform ASCA, specifying model components
mSet<-Perform.ASCA(mSet, a=1, b=1, x=2, res=2)

# Create scree plots of each model
mSet<-PlotModelScree(mSet, "asca_scree_0_", "png", 72, width=NA)

# Plot ASCA model A
mSet<-PlotASCAModel(mSet, "asca_fa_0_", "png", 72, width=NA, "a",FALSE)

# Plot ASCA model B
mSet<-PlotASCAModel(mSet, "asca_fb_0_", "png", 72, width=NA, "b",FALSE)

# Plot ASCA Interaction
mSet<-PlotInteraction(mSet, "asca_fab_0_", "png", 72,FALSE, width=NA)

# Perform model validation, 20 permutations specified
mSet<-Perform.ASCA.permute(mSet, 20)

# Plot model validation
mSet<-PlotASCA.Permutation(mSet, "asca_perm_0_", "png", 72, width=NA)

# Calculate significant features, specifying the alpha threshold (spe.thresh) and leverage threshold (lev.thresh) 
mSet<-CalculateImpVarCutoff(mSet, spe.thresh = 0.05, lev.thresh = 0.9)

# Plots of significant features for each model
mSet<-PlotAscaImpVar(mSet, "asca_impa_0_", "png", 72, width=NA, "a")
mSet<-PlotAscaImpVar(mSet, "asca_impb_0_", "png", 72, width=NA, "b")
mSet<-PlotAscaImpVar(mSet, "asca_impab_0_", "png", 72, width=NA, "ab")

## ------------------------------------------------------------------------
# Perform MB
mSet<-performMB(mSet, topPerc = 10)

# Plot time-course profile of selected feature
mSet<-PlotMBTimeProfile(mSet, "3.1522/851", "png", 72, width=NA)


