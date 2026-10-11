context("Enrichment network: the sets-only network PlotORA and PlotQEA.Overview write")

# The network writer is read from the SOURCE file, not from an installed copy of the
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
net.load("../../R/enrich_graphics.R", "PlotEnrichNet.Overview")
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
labels.of <- function(nodes) vapply(nodes, function(n) n$label, "")

test_that("the sets-only network survives a single set", {
  assign("current.msetlib", list(member = net.lib), envir = net.env)
  only <- in.tmp({
    expect_error(net.fn("PlotEnrichNet.Overview")(net.folds[1], net.pvals[1]), NA)
    net.read("msea_network.json")
  })
  expect_equal(labels.of(only$nodes), net.sets[1])
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
