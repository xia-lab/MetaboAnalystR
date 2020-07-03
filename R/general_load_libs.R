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

# Load camera
load_camera <- function(){
  suppressMessages(library(CAMERA))
}

# Load stringr
load_stringr <- function(){
  suppressMessages(library(stringr))
}

# Load httr
load_httr <- function(){
  suppressMessages(library(httr))
}

# Load RSclient
load_RSclient <- function(){
  installed <- c("RSclient") %in% rownames(installed.packages())
  if(installed){
    suppressMessages(library(RSclient))
  }else{
    print("Please install RSclient R package!")
  }
}

# Load Rserve - need to open only 1 instance for R package users
load_Rserve <- function(){
  
  installed <- c("Rserve") %in% rownames(installed.packages())
  
  if(installed){
    # first need to start up an Rserve instance
    suppressMessages(library(Rserve))
    Rserve::Rserve(args = "--no-save")
  }else{
    print("Please install Rserve R package!")
  }
}
