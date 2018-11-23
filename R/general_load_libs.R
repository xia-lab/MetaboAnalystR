# Load lattice, necessary for power analysis
load_lattice <- function(){
  suppressMessages(library(lattice))
}

# Load igraph, necessary for network analysis
load_igraph <- function(){
  suppressMessages(library(igraph))
}

# Load reshape, necessary for graphics
load_reshape <- function(){
  suppressMessages(library(reshape))
}

# Load gplots, necessary for heatmap
load_gplots <- function(){
  suppressMessages(library(gplots))
}

# Load R color brewer, necessary for heatmap
load_rcolorbrewer <- function(){
  suppressMessages(library(RColorBrewer))
}

# Load siggenes, necessary for SAM/EBAM
load_siggenes <- function(){
  suppressMessages(library(siggenes))
}

# Load RSQLite, necessary for network analysis
load_rsqlite <- function(){
  suppressMessages(library(RSQLite))
}

# Load caret, necessary for stats module
load_caret <- function(){
  suppressMessages(library(caret))
}

# Load pls, necessary for stats module
load_pls <- function(){
  suppressMessages(library(pls))
}

# Load KEGGgraph
load_kegggraph <- function(){
  suppressMessages(library(KEGGgraph))
}

# Load RGraphviz
load_rgraphwiz <- function(){
  suppressMessages(library(Rgraphviz))
}

# Load XCMS
load_xcms <- function(){
  suppressMessages(library(xcms))
}
