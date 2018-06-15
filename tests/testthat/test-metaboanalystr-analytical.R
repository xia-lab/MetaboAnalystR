context("Testing MetaboAnalystR - Analytical Modules")

library(MetaboAnalystR)

test_that("Statistical Analysis Module Works", {

  mSet<-InitDataObjects("conc", "stat", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-Normalization(mSet, "NULL", "LogNorm", "MeanCenter", "S10T0", ratio=FALSE, ratioNum=20)
  
  mSet<-FC.Anal.unpaired(mSet, 2.0, 0)
  mSet<-Ttests.Anal(mSet, nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE)
  mSet<-Volcano.Anal(mSet, FALSE, 2.0, 0, 0.75,F, 0.1, TRUE, "raw")
  
  # check if right module
  expect_match(anal.type, "stat")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$norm), 77)
  expect_equal(ncol(mSet$dataSet$norm), 63)
  
  expect_equal(mSet$analSet$fc$sig.mat[1,1], 5.8685)
  expect_equal(mSet$analSet$tt$sig.num, 53)
  expect_equal(mSet$analSet$volcano$sig.mat[1,1], 2.1297)
})


test_that("Biomarker Analysis Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("conc", "roc", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/plasma_nmr_new.csv", "rowu", "disc")
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet)
  mSet<-IsSmallSmplSize(mSet)
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ref=NULL, ratio=FALSE, ratioNum=20)
  
  mSet<-SetAnalysisMode(mSet, "univ")
  mSet<-PrepareROCData(mSet)
  mSet<-Perform.UnivROC(mSet, feat.nm = "Isoleucine", imgName = "Isoleucine", "png", dpi=300, isAUC=F, isOpt=T, optMethod="closest.topleft", isPartial=F, measure="sp", cutoff=0.2)
  mSet<-CalculateFeatureRanking(mSet)
  
  # check if right module
  expect_match(anal.type, "roc")
  expect_match(mSet$analSet$mode, "univ")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$norm), 65)
  
  expect_equal(nrow(feat.rank.mat), 63)
  expect_equal(ncol(feat.rank.mat), 4)
  expect_match(rownames(feat.rank.mat)[1], "Betaine")
  expect_equal(feat.rank.mat[1,4], 2)
  
})


test_that("Time-series or Two-Factor Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("pktable", "ts", FALSE)
  mSet<-SetDesignType(mSet, "time")
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/cress_time.csv", "colts", "disc");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "S10T0", ratio=FALSE, ratioNum=20)
  
  mSet<-iPCA.Anal(mSet, "ipca_3d_0_.json")
  mSet<-ANOVA2.Anal(mSet, thresh=0.05, p.cor="fdr", type="time")
  mSet<-Perform.ASCA(mSet, a=1, b=1, x=2, res=2)
  mSet<-performMB(mSet, topPerc = 10)
  
  # check if right module
  expect_match(anal.type, "ts")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$norm), 72)
  expect_match(levels(mSet$dataSet$cls)[1], "MT")
  expect_match(levels(mSet$dataSet$cls)[2], "WT")
  
  expect_equal(nrow(mSet$analSet$aov2$sig.mat), 124)
  expect_equal(nrow(mSet$analSet$asca$Xoff), 72)
  expect_equal(nrow(mSet$analSet$MB$stats), 124)
})


test_that("Power Analysis Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("conc", "power", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "PIF_178", ratio=FALSE, ratioNum=20)
  
  mSet<-InitPowerAnal(mSet, "cachexic vs. control")
  mSet<-PerformPowerProfiling(mSet, 0.1, 200)
  mSet<-PlotPowerProfile(mSet, 0.1, 200, "powerprofile", format="png", dpi=300, width=NA)
  
  # check if right module
  expect_match(anal.type, "power")
  expect_equal(mSet$analSet$power$pwrD[1], 0.004901929)
  expect_equal(mSet$analSet$power$Jpred[1], 3)
})


test_that("MS Peaks to Paths Module Works", {
  
  rm(list =ls())
  
  mSet <- InitDataObjects("mass_all", "mummichog", FALSE)
  mSet <- Read.PeakListData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/mummichog_mzs.txt");
  mSet <- UpdateMummichogParameters(mSet, "0.1", "positive", 1.0E-4);
  mSet <- SanityCheckMummichogData(mSet)
  mSet <- PerformMummichog(mSet, "hsa_mfn", "fisher", "gamma")
  
  # check if right module
  expect_match(anal.type, "mummichog")
  expect_match(mSet$lib.organism, "hsa_mfn")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$orig), 3934)
  expect_equal(mSet$dataSet$N, 261)
  expect_equal(mSet$dataSet$input_mzlist[1], 304.2979)
  
  expect_equal(mSet$mummi.resmat[1,1], 94)
  expect_equal(mSet$mummi.resmat[1,2], 64)
  expect_match(rownames(mSet$mummi.resmat)[1], "Tryptophan metabolism")
  
})
