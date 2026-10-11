context("Enrichment network viewer: my.enrich.net for metabolite set enrichment (msetora, msetssp, msetqea)")

# The enrichment network viewer (Secure/viewer/EnrichNetwork.xhtml) draws what my.enrich.net writes:
# the set network (nodes, edges) and the bipartite network of the sets and their hit compounds
# (bnodes, bedges), switched by the viewer's own View option. These tests pin that both are written for
# a metabolite set enrichment result, whatever its shape. The function is read from the SOURCE files,
# not from an installed copy of the package.
skip_if_not_installed("igraph")
skip_if_not_installed("reshape")
suppressMessages(library(igraph))

env <- new.env(parent = globalenv())
load.fns <- function(file, fns) {
  for (e in parse(testthat::test_path(file))) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && as.character(e[[2]])[1] %in% fns) eval(e, env)
  }
}
load.fns("../../R/utils_enrichnet.R", c("my.enrich.net", ".id2name", ".name2id", ".mumid2name", ".mumname2id", "overlap_ratio"))
load.fns("../../R/general_misc_utils.R", c("ComputeColorGradient", "GetColorGradient", "generate_breaks",
                                           "scale_vec_colours", "fast.write.csv", ".ov_signif_cols"))
load.fns("../../R/general_data_utils.R", "AddErrMsg")
load.fns("../../R/enrich_graphics.R", "PlotEnrichNet.Overview")
assign(".get.mSet", function(obj = NA) obj, envir = env)
assign(".set.mSet", function(obj = NA) obj, envir = env)
assign(".on.public.web", FALSE, envir = env)
assign("melt", reshape2::melt, envir = env)
assign("err.vec", character(0), envir = env)
assign("current.msg", "", envir = env)

# five metabolite sets with a known answer. The first two share most of their members, the third and
# fourth share their members, the fifth stands alone.
lib <- list(
  "Citric Acid Cycle" = c("Citric acid", "Isocitric acid", "Succinic acid", "Fumaric acid", "L-Malic acid", "Oxalacetic acid"),
  "Glyoxylate and Dicarboxylate Metabolism" = c("Citric acid", "Isocitric acid", "Succinic acid", "Glycine", "Glyoxylic acid"),
  "Glycolysis" = c("D-Glucose", "Pyruvic acid", "Lactic acid", "D-Fructose"),
  "Gluconeogenesis" = c("D-Glucose", "Pyruvic acid", "Lactic acid", "L-Malic acid", "Oxalacetic acid"),
  "Purine metabolism" = c("Adenosine", "Guanosine", "Inosine", "Xanthine")
)
hits <- list(
  "Citric Acid Cycle" = c("Citric acid", "Succinic acid", "L-Malic acid"),
  "Glyoxylate and Dicarboxylate Metabolism" = c("Citric acid", "Glycine"),
  "Glycolysis" = c("Pyruvic acid", "Lactic acid"),
  "Gluconeogenesis" = c("Pyruvic acid", "Lactic acid", "L-Malic acid"),
  "Purine metabolism" = c("Adenosine", "Inosine")
)
sets <- names(lib)
pv <- c(0.001, 0.02, 0.04, 0.3, 0.01)

# the result matrices as CalculateHyperScore (ora.mat) and CalculateGlobalTestScore (qea.mat) leave them
ora.mat <- function(sets, hits, pv, expected = rep(1, length(sets))) {
  m <- cbind(total = 10, expected = expected, hits = lengths(hits[sets]), "Raw p" = pv, "Holm p" = 1, FDR = pv * 2)
  rownames(m) <- sets
  m[order(m[, "Raw p"]), , drop = FALSE]
}
qea.mat <- function(sets, hits, pv) {
  m <- cbind("Total Cmpd" = 10, "Hits" = lengths(hits[sets]), "Statistic Q" = 2, "Expected Q" = 1,
             "Raw p" = pv, "Holm p" = 1, FDR = pv * 2)
  rownames(m) <- sets
  m[order(m[, "Raw p"]), , drop = FALSE]
}
mset.for <- function(type, sets, hits, pv) {
  if (type == "msetqea") {
    list(analSet = list(qea.mat = qea.mat(sets, hits, pv), qea.hits = hits[sets], type = type))
  } else {
    list(analSet = list(ora.mat = ora.mat(sets, hits, pv), ora.hits = hits[sets], type = type))
  }
}

# what the result page does before the viewer opens: PlotORA / PlotQEA.Overview write the sets-only
# network (msea_network.json), then the button runs my.enrich.net. `overview = FALSE`: no such file.
build <- function(type, set.names = sets, set.hits = hits, set.pv = pv, set.lib = lib, overview = TRUE) {
  d <- tempfile("enr"); dir.create(d)
  old <- setwd(d); on.exit(setwd(old))
  assign("current.msetlib", list(member = set.lib), envir = env)
  mSet <- mset.for(type, set.names, set.hits, set.pv)
  if (overview) {
    mat <- if (type == "msetqea") mSet$analSet$qea.mat else mSet$analSet$ora.mat
    folds <- setNames(as.numeric(lengths(set.hits[rownames(mat)])), rownames(mat))
    set.seed(1)
    env$PlotEnrichNet.Overview(folds, setNames(mat[, "Raw p"], rownames(mat)))
  }
  res <- env$my.enrich.net(mSet, paste0("enrichNet_", type), "mixed", type)
  list(res = res, msg = tail(get("err.vec", envir = env), 1),
       json = if (file.exists(paste0("enrichNet_", type, ".json"))) rjson::fromJSON(file = paste0("enrichNet_", type, ".json")))
}
ids <- function(nodes) vapply(nodes, function(n) n$id, "")
types <- c("msetora", "msetssp", "msetqea")

for (type in types) for (overview in c(TRUE, FALSE)) {
  test_that(paste0(type, " (", if (overview) "after the overview network was written" else "no overview file",
                   "): the viewer gets the set network and the bipartite network"), {
    r <- build(type, overview = overview)
    expect_equal(r$res, 1)
    d <- r$json
    # the set network: one node per set, edges between the sets that share members
    expect_equal(sort(ids(d$nodes)), sort(sets))
    expect_true(length(d$edges) >= 1)
    for (e in d$edges) expect_true(e$source %in% sets && e$target %in% sets)
    # the bipartite network: the sets plus the compounds the query hit, a line from a set to each of its hits
    expect_equal(sort(ids(Filter(function(n) n$molType == "pathway", d$bnodes))), sort(sets))
    cpds <- ids(Filter(function(n) n$molType == "compound", d$bnodes))
    expect_equal(sort(cpds), sort(unique(unlist(hits))),
                 info = "the compounds drawn are the hits, not the whole library")
    expect_true(length(d$bedges) == sum(lengths(hits)))
    for (e in d$bedges) {
      expect_true(!is.null(e$id) && !is.null(e$source) && !is.null(e$target))
      expect_true(e$source %in% hits[[e$target]], info = "an edge joins a compound to a set that hit it")
    }
    expect_equal(d$analType, type)
    expect_equal(d$pwType, "mset")
    expect_equal(names(d$hits), sets, ignore_attr = TRUE)
    # the viewer's result table
    expect_equal(length(d$enr), length(sets))
    expect_true(all(c("Hits", "Raw_p", "FDR") %in% names(d$enr[[1]])))
  })
}

for (type in types) {
  test_that(paste0(type, ": sets that share nothing still give both networks (an overview with no edges has none to read)"), {
    lib2 <- list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3"))
    hits2 <- list(A = c("a1", "a2"), B = c("b1", "b2", "b3"), C = c("c1"))
    for (overview in c(TRUE, FALSE)) {
      r <- build(type, names(lib2), hits2, c(0.01, 0.02, 0.03), lib2, overview = overview)
      expect_equal(r$res, 1)
      expect_equal(length(r$json$edges), 0)
      expect_equal(length(r$json$nodes), 3)
      expect_equal(length(r$json$bedges), 6)
    }
  })

  test_that(paste0(type, ": sets with the same number of hits and the same p value still get a colour"), {
    lib2 <- list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), C = c("a1", "b1", "c1"))
    hits2 <- list(A = c("a1", "a2"), B = c("b1", "b2"), C = c("a1", "b1"))
    for (overview in c(TRUE, FALSE)) {
      r <- build(type, names(lib2), hits2, c(0.01, 0.01, 0.01), lib2, overview = overview)
      expect_equal(r$res, 1)
      cols <- vapply(r$json$bnodes, function(n) n$colorb, "")
      expect_true(all(grepl("^#[0-9A-Fa-f]{6}", cols)), info = "every node has a colour")
      expect_true(all(grepl("^#[0-9A-Fa-f]{6}", vapply(r$json$nodes, function(n) n$colorb, ""))))
    }
  })

  test_that(paste0(type, ": a single enriched set cannot make a network; the message says sets and nothing is half-written"), {
    r <- build(type, sets[1], hits, pv[1])
    expect_equal(r$res, 0)
    expect_match(r$msg, "Fewer than two")
    expect_match(r$msg, "sets")
    expect_false(grepl("pathway", r$msg))
    expect_null(r$json)
  })
}
