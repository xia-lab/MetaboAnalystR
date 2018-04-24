# MetaboAnalystR: An R package for comprehensive analysis of metabolomics data

## Description 

**MetaboAnalystR** contains the R functions and libraries underlying the popular MetaboAnalyst web server, including > 500 functions for metabolomic data analysis, visualization, and functional interpretation. The package is synchronized with the MetaboAnalyst web server. After installing and loading the package, users will be able to reproduce the same results from their local computers using the corresponding R command history downloaded from MetaboAnalyst, thereby achieving maximum flexibility and reproducibility.

## Updates - 02/05/2018

We present a new update to MetaboAnalystR (In conjuction with the release of MetaboAnalyst Version 4.0) to enable comprehensive metabolomic data analysis, interpretation, and integration with other omics data. Over the years, MetaboAnalyst has continued to evolve based on user feedback and technological advancements in the field. For this year’s update, three new modules have been added to MetaboAnalyst 4.0/R, including: 1) the addition of a new module for pathway prediction from high-resolution mass spectral data using the mummichog algorithm; 2) the addition of a Biomarker Meta-Analysis module for robust biomarker identification through the combination of multiple metabolomic datasets; and 3) the addition of a Network Explorer module for integrative analysis of metabolomics, metagenomics, and/or transcriptomics data. The underlying knowledgebases (compound libraries, metabolite sets, metabolite-SNP associations and metabolic pathways) have also been updated using the latest data from the Human Metabolome Database (HMDB) and the Small Molecule Pathway Database (SMPDB).

## Getting Started

### Step 1. Install package dependencies 

To use MetaboAnalystR, first install all package dependencies. Ensure that you are able to download packages from bioconductor. To install package dependencies, enter the R function (metanr_packages) and then use the function. A printed message will appear informing you whether or not any R packages were installed. Note that we suggest you install the XCMS R package if you will be processing raw data, but is not necessary for the majority of MetaboAnalystR utilities. 

Function to download packages:

```R
metanr_packages <- function(){

  metr_pkgs <- c("Rserve", "ellipse", "scatterplot3d", "Cairo", "randomForest", "caTools", "e1071", "som", "impute", "pcaMethods", "RJSONIO", "ROCR", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "pheatmap", "SSPA", "sva", "Rcpp", "pROC", "data.table", "limma", "car", "fitdistrplus", "lars", "Hmisc", "magrittr", "methods", "xtable", "pls", "caret", "lattice", "igraph", "gplots", "KEGGgraph", "reshape", "RColorBrewer", "tibble")
  
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

MetaboAnalystR is freely available from GitHub. The package documentation, including the vignettes for each module and user manual is available within the downloaded R package file. If all package dependencies were installed, you will be able to install the MetaboAnalylstR package. There are three options, A) using the R package devtools, B) cloning the github, C) manually downloading the .tar.gz file.

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
R CMD INSTALL MetaboAnalystR_*.tar.gz

```

#### Option C) Manual download of MetaboAnalyst.tar.gz and install locally

Manually download the .tar.gz file from [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_1.0.0.6.tar.gz). The * must be replaced by what is actually downloaded and built.  

```R
cd ~/Downloads
R CMD INSTALL MetaboAnalystR_*.tar.gz

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

MetaboAnalystR has been developed by the [XiaLab](http://xialabresearch.com/) at McGill University. 

If you use the R package, please cite: ###

## Bugs or feature requests

To inform us of any bugs or requests, please open a new issue or send an email to #jasmine.chong@mail.mcgill.ca.

## Session Info

```R
> sessionInfo()
R version 3.4.4 (2018-03-15)
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
[1] splines   stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] MetaboAnalystR_1.0.0 pls_2.6-0            caret_6.0-79         ggplot2_2.2.1        lattice_0.20-35     

loaded via a namespace (and not attached):
  [1] tidyselect_0.2.4       RSQLite_2.1.0          AnnotationDbi_1.40.0   htmlwidgets_1.2        grid_3.4.4            
  [6] BiocParallel_1.12.0    pROC_1.11.0            devtools_1.13.5        munsell_0.4.3          codetools_0.2-15      
 [11] preprocessCore_1.40.0  withr_2.1.2            colorspace_1.3-2       BiocInstaller_1.28.0   Biobase_2.38.0        
 [16] knitr_1.20             rstudioapi_0.7         geometry_0.3-6         SSPA_2.18.0            stats4_3.4.4          
 [21] ROCR_1.0-7             robustbase_0.92-8      dimRed_0.1.0           mzID_1.16.0            lars_1.2              
 [26] git2r_0.21.0           KEGGgraph_1.38.1       mnormt_1.5-5           bit64_0.9-7            pheatmap_1.0.8        
 [31] rprojroot_1.3-2        ipred_0.9-6            randomForest_4.6-14    doParallel_1.0.11      R6_2.2.2              
 [36] RJSONIO_1.3-0          DRR_0.0.3              bitops_1.0-6           reshape_0.8.7          assertthat_0.2.0      
 [41] scales_0.5.0           nnet_7.3-12            gtable_0.2.0           affy_1.56.0            Cairo_1.5-9           
 [46] ddalpha_1.3.2          sva_3.26.0             timeDate_3043.102      rlang_0.2.0            CVST_0.2-1            
 [51] genefilter_1.60.0      mzR_2.12.0             scatterplot3d_0.3-41   RcppRoll_0.2.2         lazyeval_0.2.1        
 [56] ModelMetrics_1.1.0     acepack_1.4.1          impute_1.52.0          broom_0.4.4            checkmate_1.8.5       
 [61] yaml_2.1.18            reshape2_1.4.3         abind_1.4-5            backports_1.1.2        qvalue_2.10.0         
 [66] Hmisc_4.1-1            MassSpecWavelet_1.44.0 tools_3.4.4            lava_1.6.1             spls_2.2-2            
 [71] psych_1.8.3.3          affyio_1.48.0          gplots_3.0.1           RColorBrewer_1.1-2     BiocGenerics_0.24.0   
 [76] siggenes_1.52.0        MSnbase_2.4.2          Rcpp_0.12.16           plyr_1.8.4             zlibbioc_1.24.0       
 [81] base64enc_0.1-3        purrr_0.2.4            RCurl_1.95-4.10        rpart_4.1-13           S4Vectors_0.16.0      
 [86] sfsmisc_1.1-2          haven_1.1.1            cluster_2.0.7-1        magrittr_1.5           data.table_1.10.4-3   
 [91] openxlsx_4.0.17        RANN_2.5.1             pcaMethods_1.70.0      ProtGenerics_1.10.0    fitdistrplus_1.0-9    
 [96] matrixStats_0.53.1     xtable_1.8-2           globaltest_5.32.0      XML_3.98-1.11          rio_0.5.10            
[101] readxl_1.0.0           IRanges_2.12.0         gridExtra_2.3          compiler_3.4.4         ellipse_0.4.1         
[106] tibble_1.4.2           KernSmooth_2.23-15     crayon_1.3.4           htmltools_0.3.6        mgcv_1.8-23           
[111] Formula_1.2-2          tidyr_0.8.0            Rserve_1.7-3           lubridate_1.7.4        DBI_0.8               
[116] magic_1.5-8            MASS_7.3-49            som_0.3-5.1            Matrix_1.2-14          car_3.0-0             
[121] vsn_3.46.0             gdata_2.18.0           parallel_3.4.4         bindr_0.1.1            gower_0.1.2           
[126] igraph_1.2.1           forcats_0.3.0          pkgconfig_2.0.1        foreign_0.8-69         recipes_0.1.2         
[131] MALDIquant_1.17        xml2_1.2.0             roxygen2_6.0.1         foreach_1.4.4          annotate_1.56.2       
[136] multtest_2.34.0        prodlim_2018.04.18     GlobalAncova_3.46.0    stringr_1.3.0          digest_0.6.15         
[141] graph_1.56.0           xcms_3.0.2             cellranger_1.1.0       htmlTable_1.11.2       curl_3.2              
[146] kernlab_0.9-25         gtools_3.5.0           commonmark_1.4         nlme_3.1-137           bindrcpp_0.2.2        
[151] carData_3.0-1          desc_1.1.1             limma_3.34.9           pillar_1.2.1           DEoptimR_1.0-8        
[156] survival_2.42-3        glue_1.2.0             iterators_1.0.9        bit_1.1-12             Rgraphviz_2.22.0      
[161] class_7.3-14           stringi_1.1.7          blob_1.1.1             latticeExtra_0.6-28    caTools_1.17.1        
[166] memoise_1.1.0          dplyr_0.7.4            e1071_1.6-8         
```

## MetaboAnalystR History & Updates

04-20-2018 - Submission to CRAN

04-16-2018 - Testing with R Version 3.4.4

04-10-2018 - Updated underlying R code w. changes to MetaboAnalyst 4.0 

03-23-2018 - Added 2 more package dependencies 

02-23-2018 - Minor bug fixes based on user feedback (MetaboAnalystR_1.0.0.6.tar.gz)

02-05-2018 - Update MetaboAnalystR with 3 new modules in conjunction with the release of MetaboAnalyst Version 4

## Error: Maximal number of DLLs reached

While running MetaboAnalystR, you may come across the error "1. Error: package or namespace load failed for MetaboAnalystR in dyn.load(file, DLLpath = DLLpath, ...):maximal number of DLLs reached ...". To address this, you will need to increase the maximum number of DLLs. To do this:

ON MAC OS/LINUX - You will need to edit the ".Renviron" file which will be found in your home directory. If it does not exist, you will need to create one. Add the line R_MAX_NUM_DLLS=200, which will increase the max DLLs to 200. Restart your terminal/RStudio and continue using MetaboAnalystR.  
