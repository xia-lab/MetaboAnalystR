# MetaboAnalystR: An R package for comprehensive analysis of metabolomics data

<p align="center">
  <img src="https://github.com/xia-lab/MetaboAnalystR/blob/master/docs/MetaboAnalystRlogo.png">
</p>

## Description 

**MetaboAnalystR** contains the R functions and libraries underlying the popular MetaboAnalyst web server, including 500 functions for data processing, normalization, statistical analysis, metabolite set enrichment analysis, metabolic pathway analysis, and biomarker analysis. The package is synchronized with the MetaboAnalyst web server. After installing and loading the package, users will be able to reproduce the same results from their local computers using the corresponding R command history downloaded from MetaboAnalyst, thereby achieving maximum flexibility and reproducibility.

## Getting Started

### Step 1. Install package dependencies 

To use MetaboAnalystR, first install all package dependencies. Ensure that you are able to download packages from bioconductor. To install package dependencies, enter the R function (metanr_packages) and then use the function. A printed message will appear informing you whether or not any R packages were installed. 

Function to download packages:

```R
metanr_packages <- function(){
  
  cran_pkg <- c("Rserve", "RColorBrewer", "xtable", "som", "ROCR", "RJSONIO", "gplots", "e1071", "caTools", "igraph", "randomForest", "Cairo", "pls", "pheatmap", "lattice", "rmarkdown", "knitr", "data.table", "pROC", "Rcpp", "caret", "ellipse", "scatterplot3d")
  bioconductor_pkg <- c("impute", "pcaMethods", "siggenes", "globaltest", "GlobalAncova", "Rgraphviz", "KEGGgraph", "preprocessCore", "genefilter", "SSPA", "sva")
  
  list_installed <- installed.packages()
  
  new_cran <- subset(cran_pkg, !(cran_pkg %in% list_installed[, "Package"]))
  
  if(length(new_cran)!=0){
    install.packages(new_cran, dependencies = TRUE)
    print(c(new_cran, " packages added..."))
  }
  
  new_bio <- subset(bioconductor_pkg, !(bioconductor_pkg %in% list_installed[, "Package"]))
  
  if(length(new_bio)!=0){
    
    source("https://bioconductor.org/biocLite.R")
    biocLite(new_bio, dependencies = TRUE, ask = FALSE)
    print(c(new_bio, " packages added..."))
  }
  
  if((length(new_cran)<1)&(length(new_bio)<1)){
    print("No new packages added...")
  }
    
}
```
Usage of function:
```R
metanr_packages()
```

### Step 2. Install the package

MetaboAnalystR is freely available from GitHub. The package documentation, including the vignettes for each module and user manual is available within the downloaded R package file. If all package dependencies were installed, you will be able to install the MetaboAnalylstR package. There are three options, A) using the R package devtools, B) cloning the github, C) manually downloading the .tar.gz file. Please note that of the three, Option C will be updated the quickest. 

#### Option A) Install the package directly from github using the *devtools* package. Open R and enter:

Due to issues with Latex, some users may find that they are only able to install MetaboAnalystR without any documentation (i.e. vignettes).  

```R
# Step 1: Install devtools
install.packages("devtools")
library(devtools)

# Step 2: Install MetaboAnalystR without documentation
devtools::install_github("xia-lab/MetaboAnalystR")

# Step 2: Install MetaboAnalystR with documentation
devtools::install_github("xia-lab/MetaboAnalystR", build_vignettes=TRUE)
```

#### Option B) Clone Github and install locally

The * must be replaced by what is actually downloaded and built.  

```R

git clone https://github.com/xia-lab/MetaboAnalystR.git
R CMD build metaboanalystr
R CMD INSTALL MetaboAnalytR_*.tar.gz

```

#### Option C) Manual download of MetaboAnalyst.tar.gz and install locally

Manually download the .tar.gz file from [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_0.0.0.9000.tar.gz). The * must be replaced by what is actually downloaded and built.  

```R
cd ~/Downloads
R CMD INSTALL MetaboAnalytR_*.tar.gz

```

## Usage

For detailed tutorials on how to use MetaboAnalystR, please refer to the R package vignettes. 

Within R:
```R
vignette(package="MetaboAnalystR")
```

Within a web-browser:
```R
browseVignettes("MetaboAnalystR")
```

## Citation

MetaboAnalystR has been developed by the [XiaLab](http://www.xialab.ca/) at McGill University. 

If you use the R package, please cite: ###

## Bugs or feature requests

To inform us of any bugs or requests, please open a new issue. 
