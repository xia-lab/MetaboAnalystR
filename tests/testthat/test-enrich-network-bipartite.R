context("Enrichment network: the sets-and-compounds (bipartite) view")

# The two network writers are read from the SOURCE files, not from an installed copy of the
# package: what is tested is the code in this tree. Everything else the sets-only writer needs
# (igraph, reshape2's melt, the overlap helper, the metabolite set library) is supplied here.
skip_if_not_installed("igraph")
skip_if_not_installed("reshape2")
suppressMessages(library(igraph))

net.env <- new.env(parent = globalenv())
net.load <- function(file, fns) {
  for (e in parse(testthat::test_path(file))) {
    if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]])[1] %in% fns) eval(e, net.env)
  }
}
net.load("../../R/enrich_graphics.R", c("PlotEnrichNet.Overview", "PlotEnrichNet.Bipartite"))
net.load("../../R/general_misc_utils.R", "overlap_ratio")
assign(".on.public.web", FALSE, envir = net.env)
assign("melt", reshape2::melt, envir = net.env)

net.fn <- function(nm) {
  f <- get0(nm, envir = net.env, inherits = FALSE)
  if (!is.function(f)) stop(nm, " is not defined in R/enrich_graphics.R")
  f
}

# four metabolite sets with a known answer: the first two share 3 of their 8 distinct members
# (an edge of the sets-only view, over the 25% bar), the third and fourth share nothing with any
# other; names carry the characters that break hand-made JSON (quotes, backslash, a prime, Greek)
net.lib <- list(
  "Citric Acid Cycle" = c("Citric acid", "Isocitric acid", "Succinic acid", "Fumaric acid", "L-Malic acid", "Oxalacetic acid"),
  "Glyoxylate and Dicarboxylate Metabolism" = c("Citric acid", "Isocitric acid", "Succinic acid", "Glycine", "Glyoxylic acid"),
  "Glycolysis" = c("D-Glucose", "Pyruvic acid", "Lactic acid", "D-Fructose"),
  "Purine \"salvage\" & 5'-nucleotide metabolism" = c("5'-Methylthioadenosine", "Adenosine", "\"odd\" name\\", "α-Ketoglutaric acid")
)
net.hits <- list(
  "Citric Acid Cycle" = c("Citric acid", "Succinic acid", "L-Malic acid"),
  "Glyoxylate and Dicarboxylate Metabolism" = c("Citric acid", "Glycine"),
  "Glycolysis" = c("Pyruvic acid", "Lactic acid"),
  "Purine \"salvage\" & 5'-nucleotide metabolism" = c("5'-Methylthioadenosine", "\"odd\" name\\", "α-Ketoglutaric acid")
)
net.sets <- names(net.lib)
net.folds <- setNames(c(3.2, 2.1, 1.5, 1.2), net.sets)
net.pvals <- setNames(c(0.001, 0.02, 0.04, 0.3), net.sets)

# run `code` with a clean working directory (the writers put their JSON in the working directory)
in.tmp <- function(code) {
  d <- tempfile("net"); dir.create(d)
  old <- setwd(d); on.exit(setwd(old))
  force(code)
}
net.read <- function(f) rjson::fromJSON(file = f)
bip <- function(hits = net.hits, folds = net.folds, pvals = net.pvals, ...) {
  net.fn("PlotEnrichNet.Bipartite")(hits, folds, pvals, ...)
  net.read("msea_network_bipartite.json")
}
kinds <- function(net, k) Filter(function(n) identical(n$kind, k), net$nodes)
labels.of <- function(nodes) vapply(nodes, function(n) n$label, "")

test_that("the bipartite JSON has one set node per set and one compound node per hit shown", {
  net <- in.tmp(bip())
  expect_equal(sort(labels.of(kinds(net, "set"))), sort(net.sets))
  # 3 + 2 + 2 + 3 hits, "Citric acid" is hit by two sets: 9 distinct compounds
  expect_equal(sort(labels.of(kinds(net, "member"))), sort(unique(unlist(net.hits))))
  expect_equal(length(net$nodes), 4 + 9)
  ids <- vapply(net$nodes, function(n) n$id, "")
  expect_false(any(duplicated(ids)), info = "a set and a compound must never share a node id")
})

test_that("every edge goes from a set to a compound it hit, and a shared compound is joined to every set that hit it", {
  net <- in.tmp(bip())
  id.of <- function(k) setNames(vapply(kinds(net, k), function(n) n$id, ""), labels.of(kinds(net, k)))
  set.id <- id.of("set"); cpd.id <- id.of("member")
  expect_equal(length(net$edges), 3 + 2 + 2 + 3)
  pairs <- vapply(net$edges, function(e) paste(e$source, e$target, sep = " -> "), "")
  want <- unlist(lapply(net.sets, function(s) paste(set.id[[s]], cpd.id[net.hits[[s]]], sep = " -> ")))
  expect_equal(sort(pairs), sort(unname(want)))
  expect_equal(sum(vapply(net$edges, function(e) e$target == cpd.id[["Citric acid"]], TRUE)), 2)
  expect_false(any(duplicated(pairs)))
})

test_that("edges always have a source and a target that are nodes of the same network", {
  for (hits in list(net.hits, lapply(net.hits, function(h) character(0)))) {
    net <- in.tmp(bip(hits))
    ids <- vapply(net$nodes, function(n) n$id, "")
    for (e in net$edges) {
      expect_true(!is.null(e$id) && !is.null(e$source) && !is.null(e$target))
      expect_true(e$source %in% ids && e$target %in% ids)
    }
  }
})

test_that("names with quotes, a backslash, a prime and Greek letters survive the JSON", {
  net <- in.tmp(bip())
  expect_true("\"odd\" name\\" %in% labels.of(kinds(net, "member")))
  expect_true("α-Ketoglutaric acid" %in% labels.of(kinds(net, "member")))
  expect_true("Purine \"salvage\" & 5'-nucleotide metabolism" %in% labels.of(kinds(net, "set")))
})

test_that("set nodes follow the sets-only rules: size by hits/expected (+3), colour by position of the p value", {
  net <- in.tmp(bip())
  sets <- kinds(net, "set")
  expect_equal(vapply(sets, function(n) n$size, 1), unname(net.folds) + 3)
  expect_equal(vapply(sets, function(n) n$color, ""), heat.colors(4))
  expect_equal(labels.of(sets), net.sets)
  # and they are the same numbers the sets-only network draws for the same result
  assign("current.msetlib", list(member = net.lib), envir = net.env)
  in.tmp({
    net.fn("PlotEnrichNet.Overview")(net.folds, net.pvals)
    only <- net.read("msea_network.json")
    expect_equal(vapply(only$nodes, function(n) n$size, 1), vapply(sets, function(n) n$size, 1))
    expect_equal(vapply(only$nodes, function(n) n$color, ""), vapply(sets, function(n) n$color, ""))
  })
})

test_that("compound nodes look different from set nodes and carry no set colour", {
  net <- in.tmp(bip())
  m <- kinds(net, "member")
  expect_true(length(unique(vapply(m, function(n) n$color, ""))) == 1)
  expect_false(m[[1]]$color %in% heat.colors(4))
  expect_true(max(vapply(m, function(n) n$size, 1)) < min(vapply(kinds(net, "set"), function(n) n$size, 1)))
})

test_that("like the overview, only the top 50 sets are drawn", {
  n <- 60
  sets <- sprintf("Set %02d", 1:n)
  hits <- setNames(lapply(1:n, function(k) sprintf("Cpd %03d", (k * 3 + 0:4) %% 70)), sets)
  net <- in.tmp(bip(hits, setNames(rep(2, n), sets), setNames(seq(0.001, 0.5, length.out = n), sets)))
  expect_equal(labels.of(kinds(net, "set")), sets[1:50])
})

test_that("many hits are capped: at most 100 compounds, every set keeps at least one, widely shared compounds first", {
  n <- 50
  sets <- sprintf("Set %02d", 1:n)
  # 36 hits per set: "Shared j" is hit by the first 50, 42, 34, 26, 18 sets, the other 35 are private to the set
  hits <- setNames(lapply(1:n, function(k) {
    shared <- which(k <= 50 - 8 * (0:4))
    c(sprintf("Shared %d", shared), sprintf("Cpd %02d-%02d", k, 1:35))
  }), sets)
  net <- in.tmp(bip(hits, setNames(rep(2, n), sets), setNames(seq(0.001, 0.5, length.out = n), sets)))
  m <- kinds(net, "member")
  # 50 sets share the 100 places: 2 each. The two most widely hit compounds are offered by the
  # sets that hit them; the sets that hit only the first offer a private compound as their second
  expect_lte(length(m), 100)
  expect_true(all(c("Shared 1", "Shared 2") %in% labels.of(m)), info = "compounds hit by the most sets are kept first")
  expect_false("Shared 5" %in% labels.of(m))
  expect_true("Cpd 50-01" %in% labels.of(m))
  expect_false("Cpd 05-01" %in% labels.of(m), info = "a set that shares widely does not also spend a place on a private compound")
  linked <- unique(vapply(net$edges, function(e) e$source, ""))
  expect_equal(length(linked), n, info = "every set must keep at least one compound")
  # a drawn compound is joined to every drawn set that hit it: Shared 1 is hit by all 50
  expect_equal(sum(vapply(net$edges, function(e) e$target == "cpd:Shared 1", TRUE)), n)
  expect_equal(net$summary$members, length(m))
  expect_equal(net$summary$members_total, 5 + 35 * n)
  expect_equal(net$summary$sets, n)
})

test_that("a drawn compound is joined to every drawn set that hit it, also to a set that did not pick it", {
  h10 <- sprintf("H%02d", 1:10)
  # B, C and D each hit H01-H10, so those are their ten picks; "X" is hit by A and B only, A picks it
  # (its most shared compound), B has no place left for it but still hit it
  hits <- list(A = c("X", sprintf("A%d", 1:5)), B = c(h10, "X"), C = h10, D = h10)
  folds <- c(A = 2, B = 2, C = 2, D = 2)
  net <- in.tmp(bip(hits, folds, c(A = 0.001, B = 0.01, C = 0.02, D = 0.03)))
  pairs <- vapply(net$edges, function(e) paste(e$source, e$target), "")
  expect_true("A cpd:X" %in% pairs)
  expect_true("B cpd:X" %in% pairs, info = "B hit X, and X is on the picture")
})

test_that("few sets show up to 10 compounds each, not just 2", {
  hits <- list(S = sprintf("Cpd %02d", 1:40))
  net <- in.tmp(bip(hits, c(S = 2), c(S = 0.01)))
  expect_equal(length(kinds(net, "member")), 10)
  expect_equal(net$summary$members_total, 40)
})

test_that("one set gives one set node with its compounds, and the sets-only network survives one set too", {
  hits <- net.hits[1]
  net <- in.tmp(bip(hits, net.folds[1], net.pvals[1]))
  expect_equal(length(kinds(net, "set")), 1)
  expect_equal(length(kinds(net, "member")), 3)
  expect_equal(length(net$edges), 3)
  assign("current.msetlib", list(member = net.lib), envir = net.env)
  only <- in.tmp({
    expect_error(net.fn("PlotEnrichNet.Overview")(net.folds[1], net.pvals[1]), NA)
    net.read("msea_network.json")
  })
  expect_equal(labels.of(only$nodes), net.sets[1])
})

test_that("sets that share no hits stay apart: no edge ever joins two sets, no compound is joined to nothing", {
  hits <- net.hits[3:4]
  net <- in.tmp(bip(hits, net.folds[3:4], net.pvals[3:4]))
  set.ids <- vapply(kinds(net, "set"), function(n) n$id, "")
  for (e in net$edges) expect_false(e$source %in% set.ids && e$target %in% set.ids)
  expect_equal(length(net$edges), 2 + 3)
  linked <- c(vapply(net$edges, function(e) e$target, ""))
  expect_true(all(vapply(kinds(net, "member"), function(n) n$id, "") %in% linked))
})

test_that("sets with no hits to show still appear, with an empty edge list", {
  net <- in.tmp(bip(list(), net.folds, net.pvals))
  expect_equal(length(kinds(net, "set")), 4)
  expect_equal(length(kinds(net, "member")), 0)
  expect_equal(length(net$edges), 0)
})

test_that("the layout is stable: the same result gives the same picture, whatever the random state", {
  a <- in.tmp({ set.seed(1); bip() })
  b <- in.tmp({ set.seed(99); bip() })
  expect_identical(a, b)
  xy <- unlist(lapply(a$nodes, function(n) c(n$x, n$y)))
  expect_true(all(is.finite(xy)))
  expect_gt(length(unique(round(xy, 2))), 4)
})

test_that("a failed run leaves no stale bipartite file behind", {
  in.tmp({
    writeLines("{}", "msea_network_bipartite.json")
    expect_error(net.fn("PlotEnrichNet.Bipartite")(net.hits, unname(net.folds), unname(net.pvals)))
    expect_false(file.exists("msea_network_bipartite.json"))
  })
})

test_that("the sets-only network is unchanged: same nodes, sizes, colours and edges as before the new view", {
  assign("current.msetlib", list(member = net.lib), envir = net.env)
  only <- in.tmp({ set.seed(1); net.fn("PlotEnrichNet.Overview")(net.folds, net.pvals); net.read("msea_network.json") })
  expect_equal(names(only), c("nodes", "edges"))
  expect_equal(names(only$nodes[[1]]), c("id", "label", "size", "color", "x", "y"))
  expect_equal(labels.of(only$nodes), net.sets)
  expect_equal(vapply(only$nodes, function(n) n$size, 1), c(6.2, 5.1, 4.5, 4.2))
  expect_equal(vapply(only$nodes, function(n) n$color, ""), c("#FF0000", "#FF8000", "#FFFF00", "#FFFF80"))
  expect_equal(length(only$edges), 1)
  expect_equal(c(only$edges[[1]]$source, only$edges[[1]]$target), net.sets[1:2])
})
