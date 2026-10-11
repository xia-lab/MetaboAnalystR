context("GSEA for a metabolite concentration table (permutation of the sample labels)")

# A table where the answer is known: 60 compounds, 24 samples in two groups. The compounds 1:8 are higher in the
# first group, so the set of those compounds must come out on top; every other set is random.
gt.make <- function(seed = 11, n1 = 12, n2 = 12, m = 60, shift = 1.6, planted = 1:8){
  set.seed(seed)
  X <- matrix(rnorm((n1 + n2) * m), nrow = n1 + n2, ncol = m)
  colnames(X) <- paste0("C", seq_len(m))
  X[seq_len(n1), planted] <- X[seq_len(n1), planted] + shift
  list(X = X, cls = factor(rep(c("A", "B"), c(n1, n2))))
}
gt.sets <- function(m = 60, n.random = 30, seed = 5){
  set.seed(seed)
  s <- c(list(planted = paste0("C", 1:8)),
         lapply(seq_len(n.random), function(i) paste0("C", sample(9:m, sample(4:15, 1)))))
  names(s) <- c("planted", paste0("random", seq_len(n.random)))
  s
}

test_that("the observed enrichment score is the one fgsea computes for the same ranking", {
  skip_if_not_installed("fgsea")
  d <- gt.make()
  stat <- MetaboAnalystR:::.gsea.table.stat(d$X, d$cls, "two")
  names(stat) <- colnames(d$X)
  sets <- gt.sets()
  idx <- lapply(sets, function(x) match(x, colnames(d$X)))
  mine <- MetaboAnalystR:::.gsea.table.es(stat, idx, TRUE)
  ref <- vapply(sets, function(x) fgsea::calcGseaStat(sort(stat, decreasing = TRUE), selectedStats = match(x, names(sort(stat, decreasing = TRUE))), gseaParam = 1), numeric(1))
  expect_equal(unname(mine), unname(ref), tolerance = 1e-9)
})

test_that("a planted set is found, with a positive NES and a small p value; random sets are not", {
  d <- gt.make()
  r <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 500, seed = 1)
  expect_false(is.character(r))
  expect_equal(rownames(r$table)[which.min(r$table$p)], "planted")
  expect_gt(r$table["planted", "ES"], 0)
  expect_gt(r$table["planted", "NES"], 1.5)
  expect_lt(r$table["planted", "p"], 0.02)
  expect_gt(min(r$table[rownames(r$table) != "planted", "p"]), 0.01)
})

test_that("the sign of the difference gives the sign of the score", {
  d <- gt.make(shift = -1.6)
  r <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 300, seed = 2)
  expect_lt(r$table["planted", "ES"], 0)
  expect_lt(r$table["planted", "NES"], -1.5)
  expect_lt(r$table["planted", "p"], 0.02)
})

test_that("with the labels unrelated to the data, p values are about uniform and few sets pass 0.05", {
  set.seed(99)
  X <- matrix(rnorm(20 * 80), 20, 80); colnames(X) <- paste0("C", 1:80)
  cls <- factor(rep(c("A", "B"), each = 10))
  sets <- lapply(1:150, function(i) paste0("C", sample(80, sample(4:20, 1))))
  names(sets) <- paste0("S", 1:150)
  r <- MetaboAnalystR:::.gsea.table.engine(X, cls, sets, nperm = 400, seed = 3)
  expect_gt(mean(r$table$p), 0.40); expect_lt(mean(r$table$p), 0.60)
  expect_lt(mean(r$table$p < 0.05), 0.12)
})

test_that("the background is the measured compounds and the size limits apply to measured members", {
  d <- gt.make()
  sets <- list(small = c("C1", "C2"),                          # 2 measured: below the minimum
               partial = c("C1", "C2", "C3", "C4", "X1", "X2", "X3"),  # 4 measured of 7
               absent = c("X1", "X2", "X3", "X4"),                # none measured
               broad = paste0("C", 1:40),                         # 40 measured: above a maximum of 30
               ok = paste0("C", 20:30))
  r <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, sets, nperm = 100, minSize = 3, maxSize = 30, seed = 4)
  expect_setequal(rownames(r$table), c("partial", "ok"))
  expect_equal(r$table["partial", "size"], 4L)
})

test_that("more than two groups use an unsigned statistic and score the top of the ranking only", {
  set.seed(21)
  X <- matrix(rnorm(30 * 50), 30, 50); colnames(X) <- paste0("C", 1:50)
  cls <- factor(rep(c("A", "B", "C"), each = 10))
  X[cls == "C", 1:7] <- X[cls == "C", 1:7] + 2
  sets <- list(planted = paste0("C", 1:7), other = paste0("C", 20:30), other2 = paste0("C", 31:44))
  r <- MetaboAnalystR:::.gsea.table.engine(X, cls, sets, nperm = 400, seed = 5)
  expect_equal(r$design, "multi")
  expect_true(all(r$table$ES >= 0))
  expect_equal(rownames(r$table)[which.min(r$table$p)], "planted")
  expect_lt(r$table["planted", "p"], 0.02)
})

test_that("a continuous outcome ranks by the correlation", {
  set.seed(31)
  n <- 30
  y <- rnorm(n)
  X <- matrix(rnorm(n * 50), n, 50); colnames(X) <- paste0("C", 1:50)
  X[, 1:7] <- X[, 1:7] + 1.2 * y
  sets <- list(planted = paste0("C", 1:7), other = paste0("C", 20:30), other2 = paste0("C", 31:44))
  r <- MetaboAnalystR:::.gsea.table.engine(X, y, sets, nperm = 400, seed = 6)
  expect_equal(r$design, "cont")
  expect_equal(rownames(r$table)[which.min(r$table$p)], "planted")
  expect_gt(r$table["planted", "NES"], 1.5)
})

test_that("the same seed gives the same result, and a table that cannot be analysed is refused", {
  d <- gt.make()
  a <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 100, seed = 7)
  b <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 100, seed = 7)
  expect_identical(a$table, b$table)
  expect_true(is.character(MetaboAnalystR:::.gsea.table.engine(d$X[c(1, 13, 14), ], factor(c("A", "B", "B")), gt.sets(), nperm = 50)))
  expect_true(is.character(MetaboAnalystR:::.gsea.table.engine(d$X, factor(rep("A", 24)), gt.sets(), nperm = 50)))
})

test_that("a group with fewer than 7 samples switches the permutation of the samples to the permutation of the features", {
  d <- gt.make()
  big <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 100, seed = 8)
  expect_equal(big$mode, "samples"); expect_false(big$switched)
  small.X <- d$X[c(1:6, 13:18), ]; small.cls <- factor(rep(c("A", "B"), each = 6))
  sw <- MetaboAnalystR:::.gsea.table.engine(small.X, small.cls, gt.sets(), nperm = 500, seed = 8)
  expect_equal(sw$mode, "features"); expect_true(sw$switched); expect_true(sw$small.group)
  expect_lt(sw$n.distinct, 1000)                 # 6 + 6 samples: 924 arrangements, the case the warning names
  forced <- MetaboAnalystR:::.gsea.table.engine(small.X, small.cls, gt.sets(), nperm = 1000, seed = 8, autoSwitch = FALSE)
  expect_equal(forced$mode, "samples"); expect_false(forced$switched); expect_true(forced$few.perm)
  seven <- MetaboAnalystR:::.gsea.table.engine(d$X[c(1:7, 13:19), ], factor(rep(c("A", "B"), each = 7)), gt.sets(), nperm = 100, seed = 8)
  expect_equal(seven$mode, "samples")            # 7 per group is enough
  cont <- MetaboAnalystR:::.gsea.table.engine(d$X[1:6, ], c(1, 3, 2, 5, 4, 6), gt.sets(), nperm = 100, seed = 8)
  expect_equal(cont$mode, "features")            # a continuous outcome with 6 samples
})

test_that("permuting the features finds a planted set and gives the same observed scores as permuting the samples", {
  d <- gt.make()
  f <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 500, seed = 9, permute = "features")
  s <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 500, seed = 9, permute = "samples")
  expect_equal(f$mode, "features"); expect_false(f$switched)
  expect_equal(f$table$ES, s$table$ES, tolerance = 1e-12)
  expect_equal(rownames(f$table)[which.min(f$table$p)], "planted")
  expect_lt(f$table["planted", "p"], 0.02)
  expect_gt(f$table["planted", "NES"], 1.5)
})

test_that("permuting the features with a random ranking gives about uniform p values", {
  set.seed(77)
  X <- matrix(rnorm(20 * 80), 20, 80); colnames(X) <- paste0("C", 1:80)
  cls <- factor(rep(c("A", "B"), each = 10))
  sets <- lapply(1:150, function(i) paste0("C", sample(80, sample(4:20, 1))))
  names(sets) <- paste0("S", 1:150)
  r <- MetaboAnalystR:::.gsea.table.engine(X, cls, sets, nperm = 400, seed = 10, permute = "features")
  expect_gt(mean(r$table$p), 0.40); expect_lt(mean(r$table$p), 0.60)
  expect_lt(mean(r$table$p < 0.05), 0.12)
})

test_that("permuting the features agrees with fgsea's simple permutation on the same ranking", {
  skip_if_not_installed("fgsea")
  skip_if_not(exists("fgseaSimple", envir = asNamespace("fgsea")), "fgseaSimple is not in this fgsea")
  d <- gt.make(seed = 15)
  sets <- gt.sets(n.random = 12)
  mine <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, sets, nperm = 3000, seed = 11, permute = "features")
  stat <- mine$obs.stat; names(stat) <- colnames(d$X)
  set.seed(12)
  ref <- suppressWarnings(fgsea::fgseaSimple(sets, stat, nperm = 3000, minSize = 3, maxSize = 300))
  ref <- as.data.frame(ref); rownames(ref) <- ref$pathway
  common <- intersect(rownames(mine$table), rownames(ref))
  expect_gt(length(common), 10)
  expect_equal(mine$table[common, "ES"], ref[common, "ES"], tolerance = 1e-9)
  expect_lt(max(abs(mine$table[common, "NES"] - ref[common, "NES"]), na.rm = TRUE), 0.35)
  expect_gt(cor(log10(mine$table[common, "p"]), log10(ref[common, "pval"])), 0.9)
})

test_that("feature permutation works for more than two groups and for a continuous outcome", {
  set.seed(41)
  X <- matrix(rnorm(30 * 50), 30, 50); colnames(X) <- paste0("C", 1:50)
  cls <- factor(rep(c("A", "B", "C"), each = 10)); X[cls == "C", 1:7] <- X[cls == "C", 1:7] + 2
  sets <- list(planted = paste0("C", 1:7), other = paste0("C", 20:30), other2 = paste0("C", 31:44))
  r <- MetaboAnalystR:::.gsea.table.engine(X, cls, sets, nperm = 400, seed = 12, permute = "features")
  expect_true(all(r$table$ES >= 0)); expect_equal(rownames(r$table)[which.min(r$table$p)], "planted")
  y <- rnorm(30); X2 <- matrix(rnorm(30 * 50), 30, 50); colnames(X2) <- paste0("C", 1:50); X2[, 1:7] <- X2[, 1:7] + 1.2 * y
  r2 <- MetaboAnalystR:::.gsea.table.engine(X2, y, sets, nperm = 400, seed = 13, permute = "features")
  expect_equal(rownames(r2$table)[which.min(r2$table$p)], "planted")
})
