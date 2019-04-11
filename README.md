# MetaboAnalystR 2.0: From Raw Spectra to Biological Insights

<p align="center">
  <img src="https://github.com/xia-lab/MetaboAnalystR/blob/master/docs/MetaboAnalystRlogo.png">
</p>

## Description 

**MetaboAnalystR 2.0** contains the R functions and libraries underlying the popular MetaboAnalyst web server, including > 500 functions for metabolomic data analysis, visualization, and functional interpretation. The package is synchronized with the MetaboAnalyst web server. After installing and loading the package, users will be able to reproduce the same results from their local computers using the corresponding R command history downloaded from MetaboAnalyst, thereby achieving maximum flexibility and reproducibility. With version 2.0, we aim to address two important gaps left in its previous version. First, raw spectral processing - the previous version offered very limited support for raw spectra processing and peak annotation. Therefore, we have implemented comprehensive support for raw LC-MS spectral data processing including peak picking, peak alignment and peak annotations leveraging the functionality of the xcms (PMIDs: 16448051, 19040729, and 20671148; version 3.4.4) and CAMERA (PMID: 22111785; version 1.38.1) R packages. Second, we have enhanced support for functional interpretation directly from m/z peaks. In addition to an efficient implementation of the mummichog algorithm (PMID: 23861661), we have added a new method to support pathway activity prediction based on the well-established GSEA algorithm (PMID: 16199517). To demonstrate this new functionality, we provide the "MetaboAnalystR 2.0 Workflow: From Raw Spectra to Biological Insights" vignette, available [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_2_Workflow_From_Raw_Spectra_to_Biological_Insights.pdf) as a PDF. In this vignette, we perform end-to-end metabolomics data analysis on a subset of clinical IBD samples.   

## Getting Started

### Step 1. Install package dependencies 

To use MetaboAnalystR 2.0, first install all package dependencies. Ensure that you are able to download packages from bioconductor. To install package dependencies, there are two options:

**Option 1** 

Enter the R function (metanr_packages) and then use the function. A printed message will appear informing you whether or not any R packages were installed.  

Function to download packages:

```R
metanr_packages <- function(){

  metr_pkgs <- c("Rserve", "ellipse", "scatterplot3d", "Cairo", "randomForest", "caTools", "e1071", "som", "impute", "pcaMethods", "RJSONIO", "ROCR", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "pheatmap", "SSPA", "sva", "Rcpp", "pROC", "data.table", "limma", "car", "fitdistrplus", "lars", "Hmisc", "magrittr", "methods", "xtable", "pls", "caret", "lattice", "igraph", "gplots", "KEGGgraph", "reshape", "RColorBrewer", "tibble", "siggenes", "plotly", "xcms", "CAMERA", "fgsea", "MSnbase", "BiocParallel", "metap", "reshape2", "scales")
  
  list_installed <- installed.packages()
  
  new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  
  if(length(new_pkgs)!=0){
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    BiocManager::install(new_pkgs, version = "3.8")
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

**Option 2** 

Use the pacman R package (for those with >R 3.5.1). 

```R
install.packages("pacman")

library(pacman)

pacman::p_load(Rserve, ellipse, scatterplot3d, Cairo, randomForest, caTools, e1071, som, impute, pcaMethods, RJSONIO, ROCR, globaltest, GlobalAncova, Rgraphviz, preprocessCore, genefilter, pheatmap, SSPA, sva, Rcpp, pROC, data.table, limma, car, fitdistrplus, lars, Hmisc, magrittr, methods, xtable, pls, caret, lattice, igraph, gplots, KEGGgraph, reshape, RColorBrewer, tibble, siggenes, plotly, xcms, CAMERA, fgsea, MSnbase, BiocParallel, metap, reshape2, scales)
```
### Step 2. Install the package

MetaboAnalystR 2.0 is freely available from GitHub. The package documentation, including the vignettes for each module and user manual is available within the downloaded R package file. If all package dependencies were installed, you will be able to install the MetaboAnalylstR 2.0 . There are three options, A) using the R package devtools, B) cloning the github, C) manually downloading the .tar.gz file. Note that the MetaboAnalystR 2.0 github will have the most up-to-date version of the package. 

#### Option A) Install the package directly from github using the *devtools* package. Open R and enter:

Due to issues with Latex, some users may find that they are only able to install MetaboAnalystR 2.0 without any documentation (i.e. vignettes). 

```R
# Step 1: Install devtools
install.packages("devtools")
library(devtools)

### For users with devtools > v2.0.0 ###

# Step 2: Install MetaboAnalystR without documentation
devtools::install_github("xia-lab/MetaboAnalystR", build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignette"))

# Step 2: Install MetaboAnalystR with documentation
devtools::install_github("xia-lab/MetaboAnalystR", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))

### For users with devtools < v2.0.0 ###

# Step 2: Install MetaboAnalystR without documentation
devtools::install_github("xia-lab/MetaboAnalystR")

# Step 2: Install MetaboAnalystR with documentation
devtools::install_github("xia-lab/MetaboAnalystR", build_vignettes=TRUE)
```

#### Option B) Clone Github and install locally

The * must be replaced by what is actually downloaded and built.  

```R
git clone https://github.com/xia-lab/MetaboAnalystR.git
R CMD build MetaboAnalystR
R CMD INSTALL MetaboAnalystR_*.tar.gz

```

#### Option C) Manual download of MetaboAnalystR_2.0.0.tar.gz and install locally

Manually download the .tar.gz file from [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_2.0.0.tar.gz). The * must be replaced by what is actually downloaded and built.  

```R
cd ~/Downloads
R CMD INSTALL MetaboAnalystR_*.tar.gz

```

## Case Studies

### MetaboAnalyst 2.0 Workflow: From Raw Spectra to Biological Insights

The R scripts to perform all of the analysis from our manuscript "MetaboAnalystR 2.0: From Raw Spectra to Biological Insights" can be found [here](https://github.com/jsychong/MetaboAnalystR/tree/master/MetaboAnalystR_2_Supplementary_Data).

To showcase how to utilize MetaboAnalystR 2.0, we provide a detailed tutorial to perform a comprehensive end-to-end metabolomics data workflow from raw data preprocessing to knowledge-based analysis. The dataset showcased in the tutorial consists of a subset of pediatric IBD stool samples obtained from the Integrative Human Microbiome Project Consortium (https://ibdmdb.org/). The tutorial is available as a PDF [here](https://github.com/jsychong/MetaboAnalystR/blob/master/MetaboAnalystR_2_Workflow_From_Raw_Spectra_to_Biological_Insights.pdf) and is also available inside the R package as a vignette.

### MetaboAnalyst 1.0

To demonstrate the functionality, flexibility, and scalability of the MetaboAnalystR v1.0.0 package, three use-cases using two sets of metabolomics data is available [here](https://github.com/jsychong/MetaboAnalystR/tree/master/Supplementary_Material). In this folder you will find detailed discussions and comparisons with the MetaboAnalyst web-platform.

## Tutorials

For detailed tutorials on how to use MetaboAnalystR 2.0, please refer to the R package vignettes. These vignettes include detailed step-by-step workflows with example data for each of the main MetaboAnalytR 2.0 modules (11), a case-study showcasing the new end-to-end functionality of MetaboAnalystR 2.0, as well as an example that demonstrates the ease of using XCMS and MetaboAnalystR 1.0 for a holisitic metabolomic data analysis (deprecated). Note, the functions below work only if the R package vignettes were built. 

Within R:
```R
vignette(package="MetaboAnalystR")
```

Within a web-browser:
```R
browseVignettes("MetaboAnalystR")
```

## Citation

MetaboAnalystR 2.0 has been developed by the [XiaLab](http://xialabresearch.com/) at McGill University. The original manuscript (Version 1.0.0) can be found [here](https://doi.org/10.1093/bioinformatics/bty528). MetaboAnalystR Version 2.0.0 has been recently [published](https://www.mdpi.com/2218-1989/9/3/57)!

We encourage users to further develop the package to suit their needs. If you use the R package, please cite us: 

Chong, Jasmine, Mai Yamamoto, and Jianguo Xia. "MetaboAnalystR 2.0: From Raw Spectra to Biological Insights." 
Metabolites 9.3 (2019): 57.

Chong, Jasmine, and Jianguo Xia. "MetaboAnalystR: an R package for flexible and reproducible analysis of metabolomics data." Bioinformatics 34.24 (2018): 4313-4314.

*From within R:*

```R
citation("MetaboAnalystR")
```

## Bugs or feature requests

To inform us of any bugs or requests, please open a new issue or send an email to #jasmine.chong@mail.mcgill.ca.

## MetaboAnalystR History & Updates

04-11-2019 - Updated underlying code of MS Peaks to Paths, new functions to create plots to summarize results of mummichog and GSEA analysis, fixed bugs with compound name matching, updated documentation

03-25-2019 - Updated MS Peaks to Paths to stop analysis if no significant peaks are identified. Updated underlying code with 
webserver and updated documentation 

03-19-2019 - Added support for MS2 data - function to read in MS2 data and extract/plot spectra for a specific m/z

03-18-2019 - Added function to plot m/z features hits in a pathway for the MS Peaks to Paths module, updated documentation and MetaboAnalystR 2.0 tutorial to reflect this new addition

03-17-2019 - Added method for combining p-values for MS Peaks to Paths (mummichog + fGSEA pvalues), updated documentation and MetaboAnalystR 2.0 workflow

03-11-2019 - Added code chunk to set the number of cores used for parallel preprocessing of raw MS data to half of the number of available cores to avoid user memory issues + updated filepath for ImportRawMSData, so users only have to provide the folder path to the samples

03-05-2019 - Version Update: 2.0.0! - added function for graphical integration of results from mummichog and fGSEA, added new tutorial with example data from the fecal metabolome of IBD patients

03-03-2019 - Version Update: 1.0.4 - added support for pathway activity prediction using fGSEA; major release coming soon after bug fixes

02-11-2019 - Version Update: 1.0.3 - updated underlying R code w. changes to MetaboAnalyst 4.53 + updated documentation

11-20-2018 - Version Update: 1.0.2 - updated links in R code (https) + underlying code w. changes to MetaboAnalyst 4.39 

07-03-2018 - Addition of XCMS to MetaboAnalystR tutorial  

06-25-2018 - Publication of MetaboAnalystR in Bioinformatics 

06-13-2018 - Addition of case studies + unit-testing + 3D visualization with plotly

05-25-2018 - Version Update: 1.0.1 - updated underlying R code w. changes to MetaboAnalyst 4.09

04-20-2018 - Submission to CRAN

04-16-2018 - Testing with R Version 3.4.4

04-10-2018 - Updated underlying R code w. changes to MetaboAnalyst 4.0 

03-23-2018 - Added 2 more package dependencies 

02-23-2018 - Minor bug fixes based on user feedback (MetaboAnalystR_1.0.0.6.tar.gz)

02-05-2018 - Update MetaboAnalystR with 3 new modules in conjunction with the release of MetaboAnalyst Version 4
