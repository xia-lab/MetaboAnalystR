# MetaboAnalystR: An R package for comprehensive analysis of metabolomics data

<p align="center">
  <img src="https://github.com/xia-lab/MetaboAnalystR/blob/master/docs/MetaboAnalystRlogo.png">
</p>

## Description 

**MetaboAnalystR** contains the R functions and libraries underlying the popular MetaboAnalyst web server, including > 500 functions for metabolomic data analysis, visualization, and functional interpretation. The package is synchronized with the MetaboAnalyst web server. After installing and loading the package, users will be able to reproduce the same results from their local computers using the corresponding R command history downloaded from MetaboAnalyst, thereby achieving maximum flexibility and reproducibility.

## Updates

We present a new update to MetaboAnalystR (In conjuction with the release of MetaboAnalyst Version 4.0) to enable comprehensive metabolomic data analysis, interpretation, and integration with other omics data. Over the years, MetaboAnalyst has continued to evolve based on user feedback and technological advancements in the field. For this year’s update, three new modules have been added to MetaboAnalyst 4.0/R, including: 1) the addition of a new module for pathway prediction from high-resolution mass spectral data using the mummichog algorithm; 2) the addition of a Biomarker Meta-Analysis module for robust biomarker identification through the combination of multiple metabolomic datasets; and 3) the addition of a Network Explorer module for integrative analysis of metabolomics, metagenomics, and/or transcriptomics data. The underlying knowledgebases (compound libraries, metabolite sets, metabolite-SNP associations and metabolic pathways) have also been updated using the latest data from the Human Metabolome Database (HMDB) and the Small Molecule Pathway Database (SMPDB).

## Getting Started

### Step 1. Install package dependencies 

To use MetaboAnalystR, first install all package dependencies. Ensure that you are able to download packages from bioconductor. To install package dependencies, enter the R function (metanr_packages) and then use the function. A printed message will appear informing you whether or not any R packages were installed. 

Function to download packages:

```R
metanr_packages <- function(){
  
  metr_pkgs <- c("Rserve", "RColorBrewer", "xtable", "som", "ROCR", "RJSONIO", "gplots", "e1071", "caTools", "igraph", "randomForest", "Cairo", "pls", "pheatmap", "lattice", "rmarkdown", "knitr", "data.table", "pROC", "Rcpp", "caret", "ellipse", "scatterplot3d", "impute", "pcaMethods", "siggenes", "globaltest", "GlobalAncova", "Rgraphviz", "KEGGgraph", "preprocessCore", "genefilter", "SSPA", "sva", "limma")
  
  list_installed <- installed.packages()
  
  new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  
  if(length(new_pkgs)!=0){
    
    source("https://bioconductor.org/biocLite.R")
    biocLite(new_pkgs, dependencies = TRUE, ask = FALSE)
    print(c(new_pkgs, " packages added..."))
  }
  
  if((length(new_pkgs)<1)){
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

Manually download the .tar.gz file from [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_1.0.02.tar.gz). The * must be replaced by what is actually downloaded and built.  

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

## Session Info

```R
> sessionInfo()

R version 3.4.1 (2017-06-30)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.5 LTS

Matrix products: default
BLAS: /usr/lib/libblas/libblas.so.3.0
LAPACK: /usr/lib/lapack/liblapack.so.3.0

locale:
 [1] LC_CTYPE=en_CA.UTF-8       LC_NUMERIC=C               LC_TIME=en_CA.UTF-8        LC_COLLATE=en_CA.UTF-8    
 [5] LC_MONETARY=en_CA.UTF-8    LC_MESSAGES=en_CA.UTF-8    LC_PAPER=en_CA.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_CA.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] MetaboAnalystR_0.0.0.9000 devtools_1.13.4          

loaded via a namespace (and not attached):
  [1] colorspace_1.3-2      class_7.3-14          siggenes_1.50.0       rprojroot_1.3-2       qvalue_2.8.0          som_0.3-5.1          
  [7] pls_2.6-0             rstudioapi_0.7        roxygen2_6.0.1        DRR_0.0.3             bit64_0.9-7           AnnotationDbi_1.38.2 
 [13] prodlim_1.6.1         lubridate_1.7.1       xml2_1.2.0            codetools_0.2-15      splines_3.4.1         mnormt_1.5-5         
 [19] impute_1.50.1         robustbase_0.92-8     RcppRoll_0.2.2        pROC_1.10.0           Cairo_1.5-9           caret_6.0-78         
 [25] broom_0.4.3           annotate_1.54.0       ddalpha_1.3.1         kernlab_0.9-25        sfsmisc_1.1-1         pheatmap_1.0.8       
 [31] globaltest_5.30.0     graph_1.54.0          compiler_3.4.1        backports_1.1.2       assertthat_0.2.0      Matrix_1.2-12        
 [37] lazyeval_0.2.1        limma_3.32.10         tools_3.4.1           bindrcpp_0.2          igraph_1.1.2          gtable_0.2.0         
 [43] glue_1.2.0            reshape2_1.4.3        dplyr_0.7.4           Rcpp_0.12.15          Biobase_2.36.2        RJSONIO_1.3-0        
 [49] multtest_2.32.0       gdata_2.18.0          preprocessCore_1.38.1 nlme_3.1-131          iterators_1.0.9       psych_1.7.8          
 [55] timeDate_3042.101     GlobalAncova_3.44.0   gower_0.1.2           stringr_1.2.0         gtools_3.5.0          XML_3.98-1.9         
 [61] DEoptimR_1.0-8        MASS_7.3-47           scales_0.5.0          ipred_0.9-6           SSPA_2.16.0           pcaMethods_1.68.0    
 [67] parallel_3.4.1        KEGGgraph_1.38.1      RColorBrewer_1.1-2    yaml_2.1.16           memoise_1.1.0         ggplot2_2.2.1        
 [73] rpart_4.1-11          stringi_1.1.6         RSQLite_2.0           genefilter_1.58.1     desc_1.1.1            S4Vectors_0.14.7     
 [79] foreach_1.4.4         randomForest_4.6-12   e1071_1.6-8           caTools_1.17.1        BiocGenerics_0.22.1   BiocParallel_1.10.1  
 [85] lava_1.6              matrixStats_0.52.2    rlang_0.1.6           pkgconfig_2.0.1       commonmark_1.4        bitops_1.0-6         
 [91] lattice_0.20-35       ROCR_1.0-7            purrr_0.2.4           bindr_0.1             recipes_0.1.2         CVST_0.2-1           
 [97] bit_1.1-12            tidyselect_0.2.3      plyr_1.8.4            magrittr_1.5          R6_2.2.2              IRanges_2.10.5       
[103] gplots_3.0.1          dimRed_0.1.0          DBI_0.7               mgcv_1.8-22           pillar_1.1.0          foreign_0.8-69       
[109] withr_2.1.1           survival_2.41-3       scatterplot3d_0.3-40  RCurl_1.95-4.8        nnet_7.3-12           tibble_1.4.2         
[115] crayon_1.3.4          KernSmooth_2.23-15    ellipse_0.4.1         Rserve_1.7-3          grid_3.4.1            data.table_1.10.4-3  
[121] sva_3.24.4            blob_1.1.0            Rgraphviz_2.20.0      ModelMetrics_1.1.0    digest_0.6.15         xtable_1.8-2         
[127] tidyr_0.8.0           stats4_3.4.1          munsell_0.4.3 
```
