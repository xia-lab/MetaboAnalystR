context("Ranked-list cutoff (Top Hits) for the permutation-based enrichment")

# A COMPLETE ranked list (20 compounds, every one in the SMPDB library) where the answer is
# known by hand. The scores are deliberately UNSORTED and carry boundary values (exactly 1.0 /
# -1.0 fold change, exactly p = 0.05), because a cutoff is where "off by one" and floating
# point live.
rl.names <- c("Glucose", "Fructose", "Pyruvate", "Lactate", "Citrate", "Isocitrate",
              "Succinate", "Fumarate", "Malate", "Oxaloacetate", "Alanine", "Glutamine",
              "Glutamate", "Aspartate", "Serine", "Glycine", "Threonine", "Valine",
              "Leucine", "Isoleucine")
rl.fc <- c(2.5, -0.3, 0.8, -3.1, 0.2, 1.0, -1.0, 0.5, 1.7, -0.1,
           0.9, 0.05, -2.2, 0.4, 0.6, -0.7, 0.35, 0.15, -0.45, 1.4)
rl.p <- c(0.001, 0.2, 0.05, 0.0005, 0.9, 0.6, 0.3, 0.7, 0.04, 0.5,
          0.8, 0.45, 0.35, 0.25, 0.15, 0.55, 0.65, 0.75, 0.85, 0.95)

rl.mset <- function(scores = NULL, type = "fc") {
  mSet <- InitDataObjects("conc", "msetora", FALSE)
  mSet <- Setup.MapData(mSet, rl.names)
  mSet <- CrossReferencing(mSet, "name")
  if (!is.null(scores)) mSet <- Setup.CmpdRankScore(mSet, scores, type)
  mSet <- SetMetabolomeFilter(mSet, FALSE)
  SetCurrentMsetLib(mSet, "smpdb_pathway", 2)
}

# the compounds (as the analysis names them: HMDB names) of the fixture rows `idx`
rl.hmdb <- function(mSet, idx) {
  nm <- GetFinalNameMap(mSet)
  sort(nm$hmdb[match(rl.names[idx], nm$query)])
}

rl.reset.errors <- function() {
  assign("current.msg", "", envir = globalenv())
  assign("err.vec", character(0), envir = globalenv())
}
rl.last.error <- function() paste(get("err.vec", envir = globalenv()), collapse = " ")

test_that("when the cutoff was never set, the top 10% by |score| is tested, as before", {
  mSet <- rl.mset(rl.fc)
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  used <- mSet$analSet$ranked.cutoff.used
  expect_equal(used$by, "top")
  expect_equal(used$n.hits, 2)
  expect_equal(used$n.list, 20)
  # the 2 largest |fold change| are Lactate (-3.1) and Glucose (2.5)
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(1, 4)))
  expect_equal(GetRankedListCutoffMsg(mSet),
               "Over-representation was tested for the top 10% (2) of your 20 compounds against all 20.")
  expect_match(mSet$msgSet$rich.msg, "top `10%` by \\|score\\|")
})

test_that("an explicit top percentage equals the default and changes the hits when it changes", {
  mSet <- rl.mset(rl.fc)
  base <- CalculateHyperScore(mSet, "mummi_like")
  same <- CalculateHyperScore(SetRankedListCutoff(mSet, "top", 0.10), "mummi_like")
  expect_equal(same$analSet$ranked.cutoff.used$hits, base$analSet$ranked.cutoff.used$hits)
  expect_equal(same$analSet$ora.hits, base$analSet$ora.hits)

  # 25% of 20 = 5 compounds: |fc| 3.1, 2.5, 2.2, 1.7, 1.4
  mSet <- CalculateHyperScore(SetRankedListCutoff(mSet, "top", 0.25), "mummi_like")
  used <- mSet$analSet$ranked.cutoff.used
  expect_equal(used$n.hits, 5)
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(1, 4, 13, 9, 20)))
  expect_match(GetRankedListCutoffMsg(mSet), "the top 25% (5) of your 20 compounds against all 20", fixed = TRUE)
})

test_that("a share like 29% of 100 is 29 compounds (0.29 * 100 is 28.999999999999996 in floating point)", {
  vec <- paste0("c", 1:100)
  sc <- setNames(seq_len(100) / 10, vec)
  cut <- MetaboAnalystR:::.ranked.list.hits(vec, sc, "fc", list(by = "top", value = 0.29, dir = NA_character_))
  expect_true(cut$ok)
  expect_equal(cut$n.hits, 29)
  # and the smallest legal share still tests one compound, never none
  one <- MetaboAnalystR:::.ranked.list.hits(vec, sc, "fc", list(by = "top", value = 0.001, dir = NA_character_))
  expect_equal(one$n.hits, 1)
})

test_that("a fold-change cutoff tests the compounds with |score| >= cutoff, boundary included", {
  mSet <- rl.mset(rl.fc)
  mSet <- SetRankedListCutoff(mSet, "score", 1)
  expect_equal(mSet$paramSet$ranked.cutoff$dir, ">=")   # default for a fold change
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  used <- mSet$analSet$ranked.cutoff.used
  # |fc| >= 1: Glucose 2.5, Lactate -3.1, Isocitrate 1.0, Succinate -1.0, Malate 1.7, Glutamate -2.2, Isoleucine 1.4
  expect_equal(used$n.hits, 7)
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(1, 4, 6, 7, 9, 13, 20)))
  expect_equal(GetRankedListCutoffMsg(mSet),
               "Over-representation was tested for the 7 compounds with |score| >= 1 of your 20 against all 20.")
  expect_match(mSet$msgSet$rich.msg, "|score| >= 1", fixed = TRUE)
})

test_that("the direction is explicit: <= on a fold change picks the small |score|, and is recorded", {
  mSet <- rl.mset(rl.fc)
  mSet <- SetRankedListCutoff(mSet, "score", 0.2, "<=")
  expect_equal(mSet$paramSet$ranked.cutoff$dir, "<=")
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  used <- mSet$analSet$ranked.cutoff.used
  # |fc| <= 0.2: Citrate 0.2, Oxaloacetate -0.1, Glutamine 0.05, Valine 0.15
  expect_equal(used$dir, "<=")
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(5, 10, 12, 18)))
  expect_match(GetRankedListCutoffMsg(mSet), "|score| <= 0.2", fixed = TRUE)
})

test_that("a p-value list defaults to p <= cutoff (boundary included) and tests the small p", {
  mSet <- rl.mset(rl.p, "p")
  mSet <- SetRankedListCutoff(mSet, "score", 0.04)
  expect_equal(mSet$paramSet$ranked.cutoff$dir, "<=")   # default for a p-value
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  used <- mSet$analSet$ranked.cutoff.used
  # p <= 0.04: Glucose .001, Lactate .0005 and Malate .04 -- exactly on the boundary, although
  # the list is held as -log10(p) and 10^(-(-log10(0.04))) is a hair ABOVE 0.04 in floating point
  expect_equal(used$n.hits, 3)
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(1, 4, 9)))
  expect_match(GetRankedListCutoffMsg(mSet), "the 3 compounds with |score| <= 0.04 of your 20 against all 20", fixed = TRUE)
})

test_that("an explicit direction beats the default for the score kind", {
  mSet <- rl.mset(rl.p, "p")
  mSet <- SetRankedListCutoff(mSet, "score", 0.3, ">=")
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  # p >= 0.3, Succinate (.3) on the boundary (10^(-(-log10(0.3))) is a hair BELOW 0.3):
  # every compound but Glucose, Fructose, Pyruvate, Lactate, Malate, Aspartate, Serine
  used <- mSet$analSet$ranked.cutoff.used
  expect_equal(used$dir, ">=")
  expect_equal(sort(used$hits), rl.hmdb(mSet, c(5, 6, 7, 8, 10, 11, 12, 13, 16, 17, 18, 19, 20)))
})

test_that("the top percentage of a p-value list is the smallest p", {
  mSet <- rl.mset(rl.p, "p")
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  # 10% of 20 = 2: p .0005 (Lactate) and .001 (Glucose)
  expect_equal(sort(mSet$analSet$ranked.cutoff.used$hits), rl.hmdb(mSet, c(1, 4)))
})

test_that("a score cutoff on a list with no score is refused in plain words", {
  mSet <- rl.mset()   # order-only ranked list
  rl.reset.errors()
  expect_equal(SetRankedListCutoff(mSet, "score", 1), 0)
  expect_match(rl.last.error(), "no scores")
  expect_match(rl.last.error(), "top percentage")
})

test_that("a score cutoff that no compound passes is refused naming the cutoff", {
  mSet <- rl.mset(rl.fc)
  mSet <- SetRankedListCutoff(mSet, "score", 50)
  rl.reset.errors()
  expect_equal(CalculateHyperScore(mSet, "mummi_like"), 0)
  expect_match(rl.last.error(), "No compound")
  expect_match(rl.last.error(), "|score| >= 50", fixed = TRUE)
  expect_match(rl.last.error(), "20")
})

test_that("nonsense settings are refused", {
  mSet <- rl.mset(rl.fc)
  rl.reset.errors()
  expect_equal(SetRankedListCutoff(mSet, "top", 0), 0)
  expect_equal(SetRankedListCutoff(mSet, "top", 1.5), 0)
  expect_equal(SetRankedListCutoff(mSet, "top", NA), 0)
  expect_equal(SetRankedListCutoff(mSet, "bottom", 0.1), 0)
  expect_equal(SetRankedListCutoff(mSet, "score", -1), 0)
  expect_equal(SetRankedListCutoff(mSet, "score", 1, "="), 0)
})

test_that("a method that does not use the cutoff records none", {
  mSet <- rl.mset(rl.fc)
  mSet <- SetRankedListCutoff(mSet, "score", 1)
  mSet <- CalculateHyperScore(mSet, "mummi_like")
  expect_false(is.null(mSet$analSet$ranked.cutoff.used))
  mSet <- CalculateHyperScore(mSet, "hyperg")
  expect_true(is.null(mSet$analSet$ranked.cutoff.used))
  expect_equal(GetRankedListCutoffMsg(mSet), "")
})

test_that("the pathway twin (CalculateOraScore) reads the same setting", {
  # An offline SMPDB-type library built from the fixture's own HMDB ids, so no download:
  # three pathways over the 20 compounds.
  mSet <- InitDataObjects("conc", "pathora", FALSE)
  mSet <- Setup.MapData(mSet, rl.names)
  mSet <- CrossReferencing(mSet, "name")
  mSet <- Setup.CmpdRankScore(mSet, rl.fc, "fc")
  mSet <- SetMetabolomeFilter(mSet, FALSE)
  ids <- GetFinalNameMap(mSet)$hmdbid[match(rl.names, GetFinalNameMap(mSet)$query)]
  expect_false(any(is.na(ids)))
  sets <- list(P1 = ids[1:8], P2 = ids[7:14], P3 = ids[13:20])
  lib <- list(mset.list = sets, uniq.count = 20,
              rbc = lapply(sets, function(x) setNames(rep(1, length(x)), x)),
              dgr = lapply(sets, function(x) setNames(rep(1, length(x)), x)),
              path.ids = c("P1" = "P1", "P2" = "P2", "P3" = "P3"))
  names(lib$path.ids) <- c("P1", "P2", "P3")
  assign("current.kegglib", lib, envir = globalenv())
  on.exit(rm("current.kegglib", envir = globalenv()), add = TRUE)
  mSet$pathwaylibtype <- "SMPDB"

  base <- CalculateOraScore(mSet, "rbc", "mummi_like")
  expect_equal(base$analSet$ranked.cutoff.used$by, "top")
  expect_equal(base$analSet$ranked.cutoff.used$n.hits, 2)

  mSet2 <- CalculateOraScore(SetRankedListCutoff(mSet, "score", 1), "rbc", "mummi_like")
  used <- mSet2$analSet$ranked.cutoff.used
  expect_equal(used$n.hits, 7)
  expect_equal(used$n.list, 20)
  expect_equal(used$dir, ">=")
  expect_equal(GetRankedListCutoffMsg(mSet2),
               "Over-representation was tested for the 7 compounds with |score| >= 1 of your 20 against all 20.")
  expect_match(mSet2$msgSet$rich.msg, "Mummichog")   # the graphics code keys on this word
  expect_match(mSet2$msgSet$rich.msg, "|score| >= 1", fixed = TRUE)

  # a legacy pathway setting (the old "top fraction" parameter) still applies when the new one is unset
  mSet3 <- mSet
  mSet3$paramSet$pathqea.mummi.top.frac <- 0.25
  mSet3 <- CalculateOraScore(mSet3, "rbc", "mummi_like")
  expect_equal(mSet3$analSet$ranked.cutoff.used$n.hits, 5)
})
