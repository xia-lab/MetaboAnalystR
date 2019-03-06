## ------------------------------------------------------------------------
mSet<-InitDataObjects("conc", "power", FALSE);
mSet<-Read.TextData(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
mSet<-SanityCheckData(mSet);
mSet<-ReplaceMin(mSet);
mSet<-PreparePrenormData(mSet);
mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "PIF_178", ratio=FALSE, ratioNum=20);

## ------------------------------------------------------------------------
# Initiate the power analysis 
mSet<-InitPowerAnal(mSet, "NA")

# View the exploratory plots
mSet<-PlotPowerStat(mSet, "powerstat", format="png", dpi=300, width=NA)


## ------------------------------------------------------------------------
mSet<-InitPowerAnal(mSet, "cachexic vs. control")

# Perform power analysis, specifying the FDR and the max sample size
mSet<-PerformPowerProfiling(mSet, 0.1, 200)

# Plot the power profile
mSet<-PlotPowerProfile(mSet, 0.1, 200, "powerprofile", format="png", dpi=300, width=NA)

