context("Testing MetaboAnalystR - Analytical Modules")

library(MetaboAnalystR)

test_that("Statistical Analysis Module Works", {

  mSet<-InitDataObjects("conc", "stat", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-PreparePrenormData(mSet);
  mSet<-Normalization(mSet, "NULL", "LogNorm", "MeanCenter", "S10T0", ratio=FALSE, ratioNum=20)
  
  mSet<-FC.Anal(mSet, 2.0, 0)
  mSet<-Ttests.Anal(mSet, nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE)
  mSet<-Volcano.Anal(mSet, FALSE, 2.0, 0, F, 0.1, TRUE, "raw")
  
  # check if right module
  expect_match(anal.type, "stat")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$norm), 77)
  expect_equal(ncol(mSet$dataSet$norm), 63)
  
  expect_equal(mSet$analSet$fc$sig.mat[1,1], 5.8685)
  expect_equal(mSet$analSet$tt$sig.num, 57)
  expect_equal(mSet$analSet$volcano$sig.mat[1,1], 2.1297)
})

test_that("Biomarker Analysis Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("conc", "roc", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/plasma_nmr_new.csv", "rowu", "disc")
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet)
  mSet<-PreparePrenormData(mSet);
  mSet<-IsSmallSmplSize(mSet)
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ref=NULL, ratio=FALSE, ratioNum=20)
  
  mSet<-SetAnalysisMode(mSet, "univ")
  mSet<-PrepareROCData(mSet)
  mSet<-Perform.UnivROC(mSet, feat.nm = "Isoleucine", 0, 
                        "png", dpi=300, isAUC=F, isOpt=T, 
                        optMethod="closest.topleft", isPartial=F, 
                        measure="sp", cutoff=0.2)
  mSet<-CalculateFeatureRanking(mSet)
  
  # check if right module
  expect_match(anal.type, "roc")
  expect_match(mSet$analSet$mode, "univ")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$norm), 65)
  
  expect_equal(nrow(feat.rank.mat), 63)
  expect_equal(ncol(feat.rank.mat), 4)
  expect_match(rownames(feat.rank.mat)[1], "Betaine")
  expect_equal(feat.rank.mat[1,4], 4)
  
})

test_that("Multiple factors analysis", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("pktable", "ts", FALSE)
  mSet<-SetDesignType(mSet, "multi")
  mSet<-Read.TextDataTs(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/Covid_metabolomics_data.csv", "colu");
  mSet<-ReadMetaData(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/Covid_metadata_multClass.csv");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-SanityCheckMeta(mSet, 1);
  mSet<-SetDataTypeOfMeta(mSet);
  mSet<-SanityCheckData(mSet);
  mSet<-FilterVariable(mSet, "F", 25, "none", NULL);
  mSet<-PreparePrenormData(mSet);
  mSet<-Normalization(mSet, "NULL", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)
  ## check pca & iPCA res
  mSet<-PCA.Anal(mSet);
  mSet<-iPCA.Anal(mSet, "ipca_3d_0_.json");
  expect_equal(mSet[["analSet"]][["type"]], "ts")
  expect_equal(length(mSet[["analSet"]][["pca"]]), 9)
  expect_equal(nrow(mSet[["analSet"]][["pca"]][["x"]]), 59)
  expect_true(file.exists("ipca_3d_0_.json"))
  ## check CovariateScatter.Anal res
  adj.vec <<- "Gender";
  mSet<-CovariateScatter.Anal(mSet, 
                              "covariate_plot_0_dpi72.png", 
                              "png", "Diagnosis", "COVID", 
                              "NA" , 0.05)
  expect_equal(nrow(mSet[["dataSet"]][["meta.info"]]), 59)
  expect_equal(round(mSet[["analSet"]][["cov.mat"]][["pval.adj"]][1],5), 8.65735)
  ## check anova2
  meta.vec2 <<- c("Diagnosis", "Gender")
  mSet<-ANOVA2.Anal(mSet, 0.05, "fdr", "multi", 1, 0)
  expect_equal(length(mSet[["analSet"]][["aov2"]][["inx.imp"]]), 2054)
  ## check ASCA
  mSet<-Perform.ASCA(mSet, 1, 1, 2, 2)
  mSet<-PlotASCAModelScree(mSet, "asca_scree_0_", "png", 72, width=NA)
  mSet<-CalculateImpVarCutoff(mSet, 0.05, 0.9)
  expect_true(mSet[["analSet"]][["asca"]])

})

test_that("Power Analysis Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("conc", "power", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
  mSet<-SanityCheckData(mSet);
  mSet<-ReplaceMin(mSet);
  mSet<-PreparePrenormData(mSet);
  mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "PIF_178", ratio=FALSE, ratioNum=20)
  
  mSet<-InitPowerAnal(mSet, "cachexic vs. control")
  mSet<-PerformPowerProfiling(mSet, 0.1, 200)
  mSet<-PlotPowerProfile(mSet, 0.1, 200, "powerprofile", format="png", dpi=300, width=NA)
  
  # check if right module
  expect_match(anal.type, "power")
  expect_equal(mSet$analSet$power.mat[2,1], 0.004901929)
  expect_equal(mSet$analSet$power.mat[1,1], 3)
})

test_that("MS Peaks to Paths Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("mass_all", "mummichog", FALSE)
  mSet<-SetPeakFormat(mSet, "mpt")
  mSet<-UpdateInstrumentParameters(mSet, 5.0, "negative", "yes", 0.02);
  mSet<-Read.PeakListData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/mummichog_ibd.txt");
  mSet<-SetRTincluded(mSet, "no")
  mSet<-SanityCheckMummichogData(mSet)
  mSet<-SetPeakEnrichMethod(mSet, "mum", "v2")
  mSet<-SetMummichogPval(mSet, 0.2)
  mSet<-PerformPSEA(mSet, "hsa_mfn", "current", 3 , 100)

  # check if right module
  expect_match(anal.type, "mummichog")
  expect_match(mSet$lib.organism, "hsa_mfn")
  
  # check if data downloaded and parsed successfully
  expect_equal(nrow(mSet$dataSet$mummi.orig), 4187)
  expect_equal(mSet$dataSet$N, 414)
  
  expect_equal(mSet$mummi.resmat[1,1], 54)
  expect_equal(mSet$mummi.resmat[1,2], 38)
  expect_match(rownames(mSet$mummi.resmat)[1], "Vitamin E metabolism")
  
})
