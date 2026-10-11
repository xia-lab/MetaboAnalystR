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

test_that("the same seed gives the same result and a table with too few samples is refused or warned", {
  d <- gt.make()
  a <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 100, seed = 7)
  b <- MetaboAnalystR:::.gsea.table.engine(d$X, d$cls, gt.sets(), nperm = 100, seed = 7)
  expect_identical(a$table, b$table)
  expect_true(is.character(MetaboAnalystR:::.gsea.table.engine(d$X[c(1, 13, 14), ], factor(c("A", "B", "B")), gt.sets(), nperm = 50)))
  few <- MetaboAnalystR:::.gsea.table.engine(d$X[c(1:3, 13:15), ], factor(rep(c("A", "B"), each = 3)), gt.sets(), nperm = 500, seed = 8)
  expect_true(few$few.perm)       # only 20 arrangements of the labels exist
  expect_true(few$small.group)
})
