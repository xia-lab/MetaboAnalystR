#' Read RDS files from the internet
#' @description Function downloads the required file and reads it only if not already in working directory.
#' Need to specify the file URL and the destfile. 
#' @param filenm Input the name of the file to download
#' @param sub.dir sub.dir
# read binary qs files
# sub.dir is sub folder, leave NULL is under main lib folder
.get.my.lib <- function(filenm, sub.dir=NULL){

    if(!is.null(sub.dir)){
        sub.dir <- paste0(sub.dir, "/");
    }
    if(.on.public.web){
        lib.path <- paste0("../../libs/", sub.dir, filenm);
        print(paste("loading library:", lib.path));
        return(qs::qread(lib.path));
    }

    lib.download <- FALSE;
    if(!file.exists(filenm)){
      lib.download <- TRUE;
    }else{
      time <- file.info(filenm)
      diff_time <- difftime(Sys.time(), time[,"mtime"], units="days") 
      if(diff_time>30){
        lib.download <- TRUE;
      }
    }

    lib.url <- paste0("https://www.metaboanalyst.ca/resources/libs/", sub.dir, filenm);
    # Deal with curl issues
    if(lib.download){
      tryCatch(
        {
          download.file(lib.url, destfile=filenm, method="curl")
        }, warning = function(w){ print('warning in curl download') },
        error = function(e) {
          print("Download unsucceful. Ensure that curl is downloaded on your computer.")
          print("Attempting to re-try download using libcurl...")
          download.file(lib.url, destfile=filenm, method="libcurl")
        }
      )
    }
    lib.path <- filenm;
  
  
    # Deal w. corrupt downloaded files
    tryCatch({
        my.lib <- qs::qread(lib.path); # this is a returned value, my.lib never called outside this function, should not be in global env.
        print("Loaded files from MetaboAnalyst web-server.")
        },
        warning = function(w) { print('warning in load') },
        error = function(err) {
        print("Reading data unsuccessful, attempting to re-download file...")
        tryCatch({
            download.file(lib.url, destfile=filenm, method="curl")
            my.lib <- qs::qread(lib.path);
            print("Loaded necessary files.")
        },
        warning = function(w) { print('warning in curl download') },
        error = function(err) {
            print("Loading files from server unsuccessful. Ensure curl is downloaded on your computer.")
        }
        )
        })
    return(my.lib);
}

.getDynLoadPath <- function() {
    if(file.exists("/home/glassfish/payara5/glassfish/domains/domain1/applications/MetaboAnalyst/resources/rscripts/metaboanalystr/src/MetaboAnalyst.so")){
        path = "/home/glassfish/payara5/glassfish/domains/domain1/applications/MetaboAnalyst/resources/rscripts/metaboanalystr/src/MetaboAnalyst.so";
    }else if(dir.exists("/media/zzggyy/disk")){
        path <- "/media/zzggyy/disk/MetaboAnalyst/target/MetaboAnalyst-5.18/resources/rscripts/metaboanalystr/src/MetaboAnalyst.so"
    }else if(.on.public.web){
        path = "../../rscripts/metaboanalystr/src/MetaboAnalyst.so";
    }
    return(path)
}


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

# Load MSnbase
load_msnbase <- function(){
  suppressMessages(library(MSnbase))
}

# Load progress
load_progress <- function(){
  suppressMessages(library(progress))
}

# Load graph
load_graph <- function(){
  suppressMessages(library(graph))
}

# Load RBGL
load_RBGL <- function(){
  suppressMessages(library(RBGL))
}

# Load BiocParallel
load_biocparallel <- function(){
  suppressMessages(library(BiocParallel))
}


