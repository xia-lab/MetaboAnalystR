context("Testing MetaboAnalystR - Functional Analysis Modules")

library(MetaboAnalystR)

test_that("Pathway Analysis Module Works", {
  
  rm(list =ls())
  tmp.vec <- c("Acetoacetic acid", "Beta-Alanine", "Creatine", 
               "Dimethylglycine", "Fumaric acid", "Glycine", "Homocysteine", "L-Cysteine", 
               "L-Isolucine", "L-Phenylalanine", "L-Serine", "L-Threonine", "L-Tyrosine", 
               "L-Valine", "Phenylpyruvic acid", "Propionic acid", 
               "Pyruvic acid", "Sarcosine")
  cat("now the working 000 dir is: ", getwd(), "\n")
  mSet<-InitDataObjects("conc", "pathora", FALSE)
  mSet<-Setup.MapData(mSet, tmp.vec)
  mSet<-CrossReferencing(mSet, "name")
  mSet<-CreateMappingResultTable(mSet)
  mSet<-SetKEGG.PathLib(mSet, "hsa", lib.version = "current")
  mSet<-SetMetabolomeFilter(mSet, F);
  mSet<-CalculateOraScore(mSet, "rbc", "hyperg")

  expect_match(anal.type, "pathora")
  expect_match(mSet[["api"]][["libNm"]], "hsa.qs")
  
  # check if data set-up successfully
  expect_equal(length(mSet$dataSet$cmpd), 18)
  expect_match(mSet$dataSet$cmpd[1], "Acetoacetic acid")
  

  # check if name-mapping ok
  expect_equal(nrow(mSet$dataSet$map.table), 18)
  expect_match(mSet$dataSet$map.table[1,3], "HMDB0000060")
  
  # check results of pathway analysis
  expect_equal(nrow(mSet$analSet$ora.mat), 28)
  expect_equal(mSet$analSet$ora.mat[1,3], 8)
  expect_equal(mSet$analSet$ora.mat[1,8], 0.62837)
})

test_that("Enrichment Analysis Module Works", {
  
  rm(list =ls())
  
  mSet<-InitDataObjects("conc", "msetqea", FALSE)
  mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet);
  mSet<-CrossReferencing(mSet, "name");
  mSet<-CreateMappingResultTable(mSet)
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "NULL", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)
  mSet<-SetMetabolomeFilter(mSet, F);
  mSet<-SetCurrentMsetLib(mSet, "smpdb_pathway", 2);
  mSet<-CalculateGlobalTestScore(mSet)


  # check if right module
  expect_match(anal.type, "msetqea")
  
  # check if data ok
  expect_match(mSet$dataSet$q.type, "name")
  expect_equal(length(mSet$dataSet$cmpd), 63)
  expect_match(mSet$dataSet$cmpd[1], "1,6-Anhydro-beta-D-glucose")
  expect_equal(nrow(mSet$dataSet$norm), 77)
  
  # check if correct library used
  expect_equal(nrow(current.msetlib), 98)
  expect_equal(ncol(current.msetlib), 5)
  
  # check if name-mapping ok
  expect_equal(nrow(mSet$dataSet$map.table), 63)
  expect_match(mSet$dataSet$map.table[1,2], "Levoglucosan")
  expect_match(mSet$dataSet$map.table[1,3], "HMDB0000640")
  
  # check results of enrichment analysis
  expect_equal(length(mSet$name.map$query.vec), 63)
  expect_equal(mSet$analSet$qea.hits[[1]][1], "Betaine")
  expect_match(rownames(mSet$analSet$qea.mat)[1], "Galactose Metabolism")
})

