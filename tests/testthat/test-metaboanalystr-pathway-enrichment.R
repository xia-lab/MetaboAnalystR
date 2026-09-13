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

test_that("GSEA Rank-Based Enrichment Module Works", {
  # A compound-name list that ALREADY carries a per-compound score (fold change + p-value),
  # covering every compound tested -- not just a significant-hits subset -- e.g. a vendor
  # differential report (Metabolon, etc). Unlike ORA (needs a significant hit list) or QEA
  # (needs a per-sample concentration matrix), this reads the ranked table directly and runs
  # preranked GSEA (fgsea) against the compounds actually submitted, with no network fetch
  # (an inline temp CSV, unlike the QEA test above, whose dependency on a since-moved
  # metaboanalyst.ca URL is a pre-existing, unrelated failure).

  rm(list = ls())

  cmpd.df <- data.frame(
    Compound = c("Glucose", "Fructose", "Pyruvate", "Lactate", "Citrate", "Isocitrate",
                 "Succinate", "Fumarate", "Malate", "Oxaloacetate", "Alanine", "Glutamine",
                 "Glutamate", "Aspartate", "Serine", "Glycine", "Threonine", "Valine",
                 "Leucine", "Isoleucine", "Adenosine", "Guanosine", "Inosine", "Uridine",
                 "Hypoxanthine", "Creatine", "Creatinine", "Choline", "Betaine", "Carnitine"),
    FC = c(1.85, 1.42, 2.10, 1.95, -1.30, -1.15, 1.55, 1.28, -1.10, 1.35, 1.62, -1.75, 1.48,
           -1.22, 1.33, 1.18, -1.40, 1.55, 1.44, 1.39, 1.68, -1.52, 1.31, 1.24, 1.45, 1.33,
           1.60, -1.27, 1.38, 1.42),
    p.value = c(0.0003, 0.012, 0.0001, 0.0005, 0.03, 0.09, 0.008, 0.04, 0.15, 0.02, 0.006,
                0.002, 0.01, 0.06, 0.03, 0.08, 0.015, 0.009, 0.011, 0.013, 0.004, 0.007,
                0.03, 0.045, 0.015, 0.025, 0.005, 0.04, 0.02, 0.017),
    stringsAsFactors = FALSE
  );
  tmp.csv <- tempfile(fileext = ".csv");
  write.csv(cmpd.df, tmp.csv, row.names = FALSE);
  on.exit(unlink(tmp.csv), add = TRUE);

  mSet <- Setup.CmpdRankData(NA, tmp.csv, "signed_p");

  # check if right module -- Setup.CmpdRankData always inits "msetora" R-side, regardless of
  # which UI entry point (upload page, terminal, API) called it.
  expect_match(anal.type, "msetora");

  # check if data set-up successfully
  expect_equal(length(mSet$dataSet$cmpd), 30);
  expect_match(mSet$dataSet$cmpd[1], "Glucose");
  expect_false(is.null(mSet$dataSet$cmpd.rank.score));
  expect_equal(length(mSet$dataSet$cmpd.rank.score), 30);

  # check if name-mapping ok (same HMDB pipeline ORA/QEA use). CreateMappingResultTable is a
  # separate, display-table-building step in both sibling tests above -- Setup.CmpdRankData
  # runs CrossReferencing itself but deliberately leaves this display concern to the caller,
  # same division of labour the manual Name Check page's own Java-side call makes.
  mSet <- CreateMappingResultTable(mSet);
  expect_equal(nrow(mSet$dataSet$map.table), 30);

  # smpdb_pathway, not kegg_pathway: SetCurrentMsetLib's KEGG branch is API-only when
  # .on.public.web is FALSE (as it is here, the package's plain-library-load default) --
  # it never downloads current.msetlib.qs at all, so CalculateGseaScore would find nothing
  # to read. SMPDB has no such branch and is what the existing QEA test above already uses
  # for the same reason.
  mSet <- SetCurrentMsetLib(mSet, "smpdb_pathway", 2);
  mSet <- CalculateGseaScore(mSet);

  # check results of GSEA: real SMPDB pathways, matched (not library-wide) sizes, and the
  # top hit's identity -- values confirmed empirically (12-13 Sep 2026), both standalone and
  # via the live app (which uses kegg_pathway there, where the same glucose/lactate/pyruvate
  # signal tops out as "Glycolysis / Gluconeogenesis"; SMPDB splits it into "Gluconeogenesis").
  expect_true(nrow(mSet$analSet$gsea.mat) > 0);
  top.pathway <- rownames(mSet$analSet$gsea.mat)[1];
  expect_match(tolower(top.pathway), "gluc");
  expect_true(mSet$analSet$gsea.mat[top.pathway, "NES"] > 0);
  # Every pathway's matched size must be bounded by what was actually submitted, never
  # inflated to the pathway library's full membership count.
  expect_true(all(mSet$analSet$gsea.mat[, "total"] <= length(mSet$dataSet$cmpd)));
})

