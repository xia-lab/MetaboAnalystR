# MetaboAnalystR 3.0: Towards an Optimized Workflow for Global Metabolomics

<p align="center">
  <img src="https://github.com/xia-lab/MetaboAnalystR/blob/master/docs/MetaboAnalystRlogo.png">
</p>

## Description 

**MetaboAnalystR 3.0** contains the R functions and libraries underlying the popular MetaboAnalyst web server, including metabolomic data analysis, visualization, and functional interpretation. The package is synchronized with the MetaboAnalyst web server. After installing and loading the package, users will be able to reproduce the same results from their local computers using the corresponding R command history downloaded from MetaboAnalyst web site, thereby achieving maximum flexibility and reproducibility. 

The version 3.0 aims to improve the current global metabolomics workflow by implementing a fast parameter optimization algorithm for peak picking, and automated identification of the most suitable method for batch effect correction from 12 well-established approaches. In addition, more support for functional interpretation directly from m/z peaks via mummichog2 (PMID: 23861661), and a new pathway-based method to integrate multi'omics data has been added. To demonstrate this new functionality, we provide the "MetaboAnalystR 3.0 Workflow: Towards an Optimized Workflow for Global Metabolomics" vignette, available [here](https://drive.google.com/file/d/13PRxRicyKhLSyA9PD7UmV4RsjecaQOkq/view?usp=sharing) as a PDF. In this vignette, we perform end-to-end metabolomics data analysis on the full batch of clinical IBD samples.   
 

## Getting Started

### Step 1. Install package dependencies 

To use MetaboAnalystR 3.0, first install all package dependencies. Ensure that you have necessary system environment configured. 

For Linux (e.g. Ubuntu 18.04/20.04): libcairo2-dev, libnetcdf-dev, libxml2, libxt-dev and libssl-dev should be installed at frist;

For Windows (e.g. 7/8/8.1/10): Rtools should be installed.

For Mac OS: In order to compile R for Mac OS, you need Xcode and GNU Fortran compiler [installed](https://mac.r-project.org/tools/). We suggest you follow these [steps](https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/) to help with your installation.

R base with version > 3.6.1 is recommended. The compatibility of latest version (v4.0.0) is under evaluation. As for installation of package dependencies, there are two options:

**Note: Option 1 went through well in the following process, so use it to get package dependencies.**

**Option 1** 

Enter the R function (metanr_packages) and then use the function. A printed message will appear informing you whether or not any R packages were installed.

Function to download packages:

```R
metanr_packages <- function(){

  metr_pkgs <- c("impute", "pcaMethods", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "SSPA", "sva", "limma", "KEGGgraph", "siggenes","BiocParallel", "MSnbase", "multtest","RBGL","edgeR","fgsea","devtools","crmn","httr","qs")
  
  list_installed <- installed.packages()
  
  new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  
  if(length(new_pkgs)!=0){
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    BiocManager::install(new_pkgs)
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
BiocManager::install("ctc")
BiocManager::install("glasso")
BiocManager::install("huge")
BiocManager::install("ppcor")
```

~~Option 2~~

~~Use the pacman R package (for those with >R 3.5.1).~~

~~R~~
~~install.packages("pacman")~~

~~library(pacman)~~

~~pacman::p_load(c("impute", "pcaMethods", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter", "SSPA", "sva", "limma", "KEGGgraph", "siggenes","BiocParallel", "MSnbase", "multtest","RBGL","edgeR","fgsea","httr","qs"))~~


### Step 2. Install the package

MetaboAnalystR 3.0 is freely available from GitHub. The package documentation, including the vignettes for each module and user manual is available within the downloaded R package file. You can install the MetaboAnalylstR 3.0 via any of the three options: A) using the R package devtools, B) cloning the github, C) manually downloading the .tar.gz file. Note that the MetaboAnalystR 3.0 github will have the most up-to-date version of the package. 

**Note: Leon recommand that you use the option A) ob B)(option A has been already tested in R version 3.6.3 local machine) to install the latest version of MetaboAnalystR from xlhd19980327/MetaboAnalystR(branch fix-bug) to get less bug.**

#### Option A) Install the package directly from github using the *devtools* package. Open R and enter:

Due to issues with Latex, some users may find that they are only able to install MetaboAnalystR 3.0 without any documentation (i.e. vignettes). 

```R
# Step 1: Install devtools
install.packages("devtools")
library(devtools)

# Step 2: Install MetaboAnalystR with documentation(recommanded)
devtools::install_github("xlhd19980327/MetaboAnalystR", ref = "fix-bug", build = TRUE, build_vignettes = TRUE, build_manual =T)

# Step 2: Install MetaboAnalystR without documentation
devtools::install_github("xlhd19980327/MetaboAnalystR", ref = "fix-bug", build = TRUE, build_vignettes = FALSE)

```

#### Option B) Clone Github and install locally

The * must be replaced by what is actually downloaded and built.  

```R
git clone -b fix-bug https://github.com/xlhd19980327/lipidIntegratedAnalysis.git
R CMD build MetaboAnalystR
R CMD INSTALL MetaboAnalystR_3.0.3.tar.gz

```

#### Option C) Manual download of MetaboAnalystR_3.0.3.tar.gz and install locally

Manually download the .tar.gz file from [here](https://drive.google.com/file/d/1OHRUzXFDukWXEKxTLMF9X1fHRN06uFFu/view?usp=sharing). 

```R
cd ~/Downloads
R CMD INSTALL MetaboAnalystR_3.0.3.tar.gz

```

## Case Studies

### MetaboAnalystR 3.0 Workflow: Towards an Optimized Workflow for Global Metabolomics

The case studies have been preformed in our article of this version [here](https://www.mdpi.com/2218-1989/10/5/186) (available online now). The example running R code of this article have been provided as a vignette inside the R package.


### MetaboAnalystR 2.0 Workflow: From Raw Spectra to Biological Insights

The R scripts to perform all of the analysis from our previous manuscript "MetaboAnalystR 2.0: From Raw Spectra to Biological Insights" can be found [here](https://github.com/jsychong/MetaboAnalystR/tree/master/MetaboAnalystR_2_Supplementary_Data).

The detailed tutorial of the outdated version to perform a comprehensive end-to-end metabolomics data workflow from raw data preprocessing to knowledge-based analysis still works. The tutorial is available as a PDF is also available inside the R package as a vignette.

### MetaboAnalystR 1.0

To demonstrate the functionality, flexibility, and scalability of the MetaboAnalystR v1.0.0 package, three use-cases using two sets of metabolomics data is available [here](https://github.com/jsychong/MetaboAnalystR/tree/master/Supplementary_Material). In this folder you will find detailed discussions and comparisons with the MetaboAnalyst web-platform.

## Tutorials

For detailed tutorials on how to use MetaboAnalystR 3.0, please refer to the R package vignettes. These vignettes include detailed step-by-step workflows with example data for each of the main MetaboAnalytR 3.0 modules, a case-study showcasing the new end-to-end functionality of MetaboAnalystR 3.0. The raw data processing workflow has been accelerated and gradually mature. Note, the functions below work only if the R package vignettes were built. 

Within R:
```R
vignette(package="MetaboAnalystR")
```

Within a web-browser:
```R
browseVignettes("MetaboAnalystR")
```

## Citation

MetaboAnalystR package has been developed by the [XiaLab](https://www.xialab.ca/) at McGill University. We encourage users to further develop the package to suit their needs. If you use the R package, please cite us: 

* Zhiqiang Pang, Jasmine Chong, Shuzhao Li and Jianguo Xia. "MetaboAnalystR 3.0: Toward an Optimized Workflow for Global Metabolomics" [link](https://doi.org/10.3390/metabo10050186)

* Jasmine Chong, Mai Yamamoto, and Jianguo Xia. "MetaboAnalystR 2.0: From Raw Spectra to Biological Insights." 
Metabolites 9.3 (2019): 57. [link](https://www.mdpi.com/2218-1989/9/3/57)

* Jasmine Chong, and Jianguo Xia. "MetaboAnalystR: an R package for flexible and reproducible analysis of metabolomics data." Bioinformatics 34.24 (2018): 4313-4314. [link](https://doi.org/10.1093/bioinformatics/bty528)

*From within R:*

```R
citation("MetaboAnalystR")
```

## Previous Version Releases

MetaboAnalystR 3.0.2 can be downloaded [here](https://drive.google.com/file/d/1SGv6IxhDYayTO5hqOU-uoumoHxUUAiLC/view?usp=sharing).
MetaboAnalystR 3.0.1 can be downloaded [here](https://drive.google.com/file/d/1l6Lp1VbLTlXzyViVKDr1uWWH3KNoyWty/view?usp=sharing).
MetaboAnalystR 3.0.0 can be downloaded [here](https://drive.google.com/file/d/1N70p5zEUAaQeqllXyxH7SR4dzw0STHu7/view?usp=sharing).
MetaboAnalystR 2.0.4 can be downloaded [here](https://www.dropbox.com/s/3nl69jzp9wh6sjh/MetaboAnalystR_2.0.4.tar.gz?dl=0).

## Bugs or feature requests

To inform us of any bugs or requests, please open a new issue (and @ Zhiqiang-PANG !!) or send an email to #jasmine.chong@mail.mcgill.ca or zhiqiang.pang@mail.mcgill.ca.

## MetaboAnalystR History & Updates

09-22-2020 - Sync R code w. web, change .rds files to .qs (requires .qs R package). 

07-23-2020 - added new function for dot plots for enrichment analysis (PlotEnrichDotPlot).

06-27-2020 - Version Update: 3.0.3 - update bug fixes for raw data processing, and added functions for Empty MS scan removal and MS data centroid;

05-08-2020 - Version Update: 3.0.2 - update bug fixes for raw data processing plot, and slim down the database to speed up the installation;

05-01-2020 - Version Update: 3.0.1 - update bug fixes for batch effect correction and compatible with web server, and added Signal Drift Correction Method;

04-10-2020 - Version Update: 3.0.0 - ultra-fast parameter optimization for peak picking, automated batch effect correction, and improved pathway activity prediction from LC-MS peaks;

03-12-2020 - Version Update: 2.0.4 - added retention time for MS Peaks to Paths (Beta)! + added troubleshooting for compound mapping

02-19-2020 - Version Update: 2.0.3 - update bug fixes w. web server

01-14-2020 - Version Update: 2.0.2 - updated gene mapping to use internal SQlite database (included in /inst), added Rserve engine (Rcode synchronized with web-server) + added functions for users to create their own mummichog libraries

01-08-2020 - Updating code for box plots + mummichog 

04-11-2019 - Version Update: 2.0.1 - updated underlying code of MS Peaks to Paths, new functions to create plots to summarize results of mummichog and GSEA analysis, fixed bugs with compound name matching, updated documentation

03-05-2019 - Version Update: 2.0.0! - added function for graphical integration of results from mummichog and fGSEA, added new tutorial with example data from the fecal metabolome of IBD patients

03-03-2019 - Version Update: 1.0.4 - added support for pathway activity prediction using fGSEA; major release coming soon after bug fixes

02-11-2019 - Version Update: 1.0.3 - updated underlying R code w. changes to MetaboAnalyst 4.53 + updated documentation

11-20-2018 - Version Update: 1.0.2 - updated links in R code (https) + underlying code w. changes to MetaboAnalyst 4.39 

05-25-2018 - Version Update: 1.0.1 - updated underlying R code w. changes to MetaboAnalyst 4.09

04-20-2018 - Submission to CRAN

04-16-2018 - Testing with R Version 3.4.4

04-10-2018 - Updated underlying R code w. changes to MetaboAnalyst 4.0 

03-23-2018 - Added 2 more package dependencies 

02-23-2018 - Minor bug fixes based on user feedback (MetaboAnalystR_1.0.0.6.tar.gz)

02-05-2018 - Update MetaboAnalystR with 3 new modules in conjunction with the release of MetaboAnalyst Version 4
