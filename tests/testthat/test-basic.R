context("Testing MetaboAnalystR - Basic Functionality")

library(MetaboAnalystR)

test_that("Uploading CSV Files Works", {
  
  mSet <- InitDataObjects("conc", "stat", FALSE)
  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/human_cachexia.csv", "rowu", "disc")
  
  expect_equal(length(mSet), 5)
  expect_match(mSet$dataSet$type, "conc")
  expect_match(mSet$dataSet$cls.type, "disc")
  expect_match(mSet$dataSet$format, "rowu")
  expect_equal(length(mSet$dataSet$cmpd), 63)
  expect_match(mSet$analSet$type, "stat")
  expect_that(mSet$dataSet$paired, is_false())
  
})

test_that("Sanity Check Works", {
  
  mSet <- InitDataObjects("conc", "stat", FALSE)
  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/human_cachexia.csv", "rowu", "disc")
  mSet <- SanityCheckData(mSet)
  
  expect_equal(length(mSet$dataSet), 16)
  expect_equal(mSet$dataSet$small.smpl.size, 0)
  expect_equal(mSet$dataSet$cls.num, 2)
  expect_equal(mSet$dataSet$minConc, 0.395)
  expect_equal(nrow(mSet$dataSet$preproc), 77)
  expect_equal(ncol(mSet$dataSet$preproc), 63)
  
})

test_that("Replace Min Works", {
  
  mSet <- InitDataObjects("conc", "stat", FALSE)
  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/human_cachexia.csv", "rowu", "disc")
  mSet <- SanityCheckData(mSet)
  mSet <- ReplaceMin(mSet)
  
  expect_equal(length(mSet$dataSet), 17)
  expect_equal(nrow(mSet$dataSet$procr), 77)
  expect_equal(ncol(mSet$dataSet$procr), 63)
  expect_match(mSet$msgSet$replace.msg, "Zero or missing variables were replaced with a small value: 0.395")
})

test_that("Normalization Works", {
  
  mSet <- InitDataObjects("conc", "stat", FALSE)
  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/human_cachexia.csv", "rowu", "disc")
  mSet <- SanityCheckData(mSet)
  mSet <- ReplaceMin(mSet)
  mSet <- Normalization(mSet, "QuantileNorm", "LogNorm", "MeanCenter", ref=NULL, ratio=FALSE, ratioNum=20)  
  
  expect_equal(length(mSet$dataSet), 25)
  expect_equal(nrow(mSet$dataSet$norm), 77)
  expect_equal(ncol(mSet$dataSet$norm), 63)
  expect_match(mSet$dataSet$rownorm.method, "Quantile Normalization")
  expect_match(mSet$dataSet$trans.method, "Log Normalization")
  expect_match(mSet$dataSet$scale.method, "Mean Centering")
  expect_that(mSet$dataSet$combined.method, is_false())
  
})


