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
  # disable parallel processing on the server
  register(SerialParam());
}

# Load fGSEA
load_fGSEA <- function(){
  suppressMessages(library(fgsea))
}

# Load data.table
load_data.table <- function(){
  suppressMessages(library(data.table))
}

# Load ggplot2
load_ggplot <- function(){
  suppressMessages(library(ggplot2))
}

# Load gridExtra
load_grid <- function(){
  suppressMessages(library(gridExtra))
  suppressMessages(library(grid))
}

# Load ggrepel
load_ggrepel <- function(){
  suppressMessages(library(ggrepel))
}

# Load dplyr
load_dplyr <- function(){
  suppressMessages(library(dplyr))
}

# Load camera
load_camera <- function(){
  suppressMessages(library(CAMERA))
}
