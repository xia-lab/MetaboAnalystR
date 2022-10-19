- [Description](#description)
- [Installation](#installation)
  * [1. Install package dependencies](#1-install-package-dependencies)
  * [2. Install the package](#2-install-the-package)
- [Tips for using the ExpressAnalystR package](#tips-for-using-the-expressanalystr-package)
- [Examples](#examples)
  * [1. Starting from a gene expression matrix](#1-starting-from-a-gene-expression-matrix)
    + [1.1 Load ExpressAnalystR library and initialize R objects](#11-load-expressanalystr-library-and-initialize-r-objects)
    + [1.2 Load ExpressAnalystR library and initialize R objects](#12-load-expressanalystr-library-and-initialize-r-objects)
    + [1.3 Annotate gene IDs to Entrez](#13-annotate-gene-ids-to-entrez)
    + [1.4 Perform data filtering and normalization](#14-perform-data-filtering-and-normalization)
    + [1.5 Prepare differential expression (DE) analysis](#15-prepare-differential-expression--de--analysis)
    + [1.6 Perform DE analysis and check DE results](#16-perform-de-analysis-and-check-de-results)
    + [1.7 Visualize gene expression pattern of individual gene](#17-visualize-gene-expression-pattern-of-individual-gene)
  * [2. Starting from three datasets for meta-analysis](#2-starting-from-three-datasets-for-meta-analysis)
    + [2.1 Load ExpressAnalystR library and initialize R objects](#21-load-expressanalystr-library-and-initialize-r-objects)
    + [2.2 Process each individual dataset](#22-process-each-individual-dataset)
    + [2.3 Perform data integrity check (compatibility)](#23-perform-data-integrity-check--compatibility-)
    + [2.4 Check diagnostic plot and perform batch correction](#24-check-diagnostic-plot-and-perform-batch-correction)
    + [2.5 Perform statistical meta-analysis using combine p-values method](#25-perform-statistical-meta-analysis-using-combine-p-values-method)
    + [2.6 View result tables](#26-view-result-tables)

## Description

**_ExpressAnalystR_** is the underlying R package synchronized with ExpressAnalyst web server. It is designed for statistical analysis, enrichment analysis and visual analytics of single and multiple gene expression data, both matrix and gene list. The R package is composed of R functions necessary for the web-server to perform network creation, trimming and analysis. 

Following installation and loading of ExpressAnalystR, users will be able to reproduce web server results from their local computers using the R command history downloaded from ExpressAnalystR. Running the R functions will allow more flexibility and reproducibility.

Note - ExpressAnalystR is still under development - we cannot guarantee full functionality
## Installation

### 1. Install package dependencies

To use ExpressAnalystR, make sure your R version is >4.0.3 and install all package dependencies. Ensure that you are able to download packages from Bioconductor. To install package dependencies, use the pacman R package. Note that some of these packages may require additional library dependencies that need to be installed prior to their own successful installation.

```
install.packages("pacman")

library(pacman)

pacman::p_load(igraph, RColorBrewer, qs, rjson, RSQLite)
```

### 2. Install the package

ExpressAnalystR is freely available from GitHub. The package documentation, including the vignettes for each module and user manual is available within the downloaded R package file. If all package dependencies were installed, you will be able to install the ExpressAnalystR. 

Install the package directly from github using the _devtools_ package. Open R and enter:

```

# Step 1: Install devtools
install.packages(devtools)
library(devtools)

# Step 2: Install ExpressAnalystR WITHOUT documentation
devtools::install_github("xia-lab/ExpressAnalystR", build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"))

# Step 2: Install ExpressAnalystR WITH documentation
devtools::install_github("xia-lab/ExpressAnalystR", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
```

## Tips for using the ExpressAnalystR package

1. The first function that you will use in every module is the `Init.Data` function, which initiates R objects that stores user's data, parameters for further processing and analysis.
2. The ExpressAnalystR package will output data files/tables/analysis/networks outputs in your current working directory.
3. Every function must be executed in sequence as it is shown on the R Command history, please do not skip any commands as this can result in errors downstream.
4. Main functions in ExpressAnalystR are documented. Use the _?Function_ format to open its documentation. For instance, use `?ExpressAnalystR::ReadTabExpression` to find out more about this function.
5. It is recommended to set the working folder to an empty folder because numerous files will be generated through the process.
6. R package is not useful for visual analytics as they are hosted on the website. It's mainly useful for statistical analysis (differential expression and meta-analysis).
7. R package is derived from R scripts used for powering web server. The values returned are often not useful in the context of local usage. The results from R functions are saved in a format qs file named as the file name of original data table. For gene list, the format qs file is named "datalist1". use this function to access: 
```
dataSet <- readDataset(fileName)
```

## Examples

### 1. Starting from a gene expression matrix
Before you start, please download the [example](https://www.expressanalyst.ca/ExpressAnalyst/resources/data/test/estrogen.txt) dataset. It is a microarray gene expression data of a human breast-cancer cell line.
#### 1.1 Load ExpressAnalystR library and initialize R objects
```
library(ExpressAnalystR)

#boolean FALSE indicates local mode (vs web mode);
Init.Data(FALSE);

# Set analysis type to single gene expression matrix
SetAnalType("onedata");
```
#### 1.2 Load ExpressAnalystR library and initialize R objects
```
ReadTabExpressData("estrogen.txt");
```
#### 1.3 Annotate gene IDs to Entrez
For this step it is imortant to please select correct organism, data type (array or RNA-seq), id type and gene-level summary (mean, median, sum).
For gene-level summary, microarray can use mean or median while RNA-seq needs to be sum.
```
PerformDataAnnot("estrogen.txt", "hsa", "array", "hgu95av2", "mean");
```
Take a look at the mapped dataset by reading the dataset's qs file using ```readDataset(fileName)``` function.
```
dataSet <- readDataset("estrogen.txt")
print(head(dataSet$data.anot[c(1:5),])
     low10-1.cel low10-2.cel high10-1.cel high10-2.cel low48-1.cel
5875    9.216562    9.290259     9.064545     8.958936    9.222260
5595   10.398169   10.254362    10.003971     9.903528   10.374866
7075    5.717613    5.881008     5.859563     5.954028    5.960540
1557    6.595252    6.828249     6.625206     6.664867    6.562788
643     7.581658    7.771235     7.672983     7.813411    7.839115
1843    9.206737    8.993802     8.237894     8.338004    9.173196
```
Check diagnostics plots to look at overall data distribution, sample separation.
```
PlotDataBox("estrogen.txt", "qc_boxplot_", 72, "png");
PlotDataPca("estrogen.txt", "qc_pca_", 72, "png");
```
Check your working directory for png images named ``qc_boxplot_dpi72.png`` and ``qc_pca_dpi72.png``, open them.
![Box Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_boxplot.png)
Box plot shows that the expression distribution of samples are between around -4 to 12.5. This shows that the data has already been normalized. 
![PCA Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_pca.png)
PCA plot shows sample separation both between absent and present, and also, low and high. Depending of your experimental design, try to see if the samples are separated by the metadata of interest, it can also be used to see whether there are potentially mislabed sample.

#### 1.4 Perform data filtering and normalization
No normalization need to be performed, PCA plot from previous step shows that the dataset is already normalized.
Filter by variance (lower 15% removed)
Filter by relative abundance (lower 4 percentile of average expression signal)
Filter by count not applied (only for RNASeq data)
Filter unannotated genes TRUE
```
PerformExpressNormalization("estrogen.txt", "none", 15, 4, 0,"true");
```
#### 1.5 Prepare differential expression (DE) analysis
Selected metadata of interest, in this case we are interested in investigating the effect of presence of Estrogen Receptor (ER) vs absence. We are not setting secondary factor and blocking factor. After selecting metadata, compute design matrix and select DE analysis algorithm by running ``SetupDesignMatrix`` function. For microarray data, only ``limma`` can be used.
```
SetSelectedMetaInfo("estrogen.txt","ER", "NA", F)
SetupDesignMatrix("estrogen.txt", "limma");
```
#### 1.6 Perform DE analysis and check DE results
Fold change is log2 transformed.
Adjusted P-value using False Discovery Rate (FDR) method.
```
PerformDEAnal("estrogen.txt", "custom", "absent vs. present", "NA", "intonly");
dataSet <- readDataset("estrogen.txt");
print(head(dataSet$comp.res));
         logFC   AveExpr         t      P.Value    adj.P.Val        B
5111 -2.235524  9.136833 -15.01586 3.213962e-08 0.0001625112 9.203192
7083 -2.898336  9.850896 -13.95419 6.517279e-08 0.0001625112 8.612374
4605 -2.924256  8.532099 -13.66086 7.991986e-08 0.0001625112 8.438401
7031 -3.198764 12.115778 -13.08278 1.208922e-07 0.0001625112 8.080844
2717 -1.581543  8.709900 -12.79062 1.499605e-07 0.0001625112 7.892350
3399  1.496948 11.529434  12.76829 1.524784e-07 0.0001625112 7.877719
```

#### 1.7 Visualize gene expression pattern of individual gene
```
PlotSelectedGene("estrogen.txt","5111");
```
Check the resulting png image (Gene_5111.png) in your working directory.
![Violin Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/Gene_5111.png)

### 2. Starting from three datasets for meta-analysis
Before you start, please download the example datasets into your working directory [E-GEOD-25713](https://dev.expressanalyst.ca/resources/data/test/E-GEOD-25713.txt), [E-GEOD-59276.txt](https://dev.expressanalyst.ca/resources/data/test/E-GEOD-59276.txt),
[GSE69588.txt](https://dev.expressanalyst.ca/resources/data/test/GSE69588.txt). These three testing datasets (containing subset of 5000 genes) are from a meta-analysis of helminth infections in mouse liver.

#### 2.1 Load ExpressAnalystR library and initialize R objects
```
library(ExpressAnalystR)

#boolean FALSE indicates local mode (vs web mode);
Init.Data(FALSE);

# Set analysis type to meta-analysis
SetAnalType("metadata");
```

#### 2.2 Process each individual dataset
```
#Read dataset text file
ReadOmicsData("E-GEOD-25713.txt");
SanityCheckData("E-GEOD-25713.txt");

#Map gene id to entrez id
AnnotateGeneData("E-GEOD-25713.txt", "mmu", "entrez");
```

Visually inspect dataset using box plot (``qc_boxplot_0_dpi72.png``) and pca plot (``qc_pca_0_dpi72.png``).
```
PlotDataProfile("E-GEOD-25713.txt", "raw", "qc_boxplot_0_", "qc_pca_0_");
```
![Box Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_boxplot_meta.png)
![PCA Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_pca_meta.png)

```
#Remove variables with more than 50% missing data
RemoveMissingPercent("E-GEOD-25713.txt", 0.5);

#Replace missing value with minimum values across dataset
ImputeMissingVar("E-GEOD-25713.txt", "min");

#Replace missing value with minimum values across dataset
NormalizingDataMeta("E-GEOD-25713.txt", "NA", "NA", "NA");
DoStatComparison("E-GEOD-25713.txt", "limma", "CLASS","NA","NA","NA", 0.05, 0.0);

#read and process the other two datasets
ReadOmicsData("E-GEOD-59276.txt");
SanityCheckData("E-GEOD-59276.txt");
AnnotateGeneData("E-GEOD-59276.txt", "mmu", "entrez");
RemoveMissingPercent("E-GEOD-59276.txt", 0.5)
ImputeMissingVar("E-GEOD-59276.txt", "min")
NormalizingDataMeta("E-GEOD-59276.txt", "NA", "NA", "NA");
DoStatComparison("E-GEOD-59276.txt", "limma", "CLASS","NA","NA","NA", 0.05, 0.0);

ReadOmicsData("GSE69588.txt");
SanityCheckData("GSE69588.txt");
AnnotateGeneData("GSE69588.txt", "mmu", "entrez");
RemoveMissingPercent("GSE69588.txt", 0.5)
ImputeMissingVar("GSE69588.txt", "min")
NormalizingDataMeta("GSE69588.txt", "NA", "NA", "NA");
DoStatComparison("GSE69588.txt", "limma", "CLASS","NA","NA","NA", 0.05, 0.0);
```
#### 2.3 Perform data integrity check (compatibility)
```
CheckMetaDataIntegrity();
```
#### 2.4 Check diagnostic plot and perform batch correction
```
PlotMetaPCA("qc_meta_pca_","72", "png", "");
```
![PCA Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_meta_pca_beforeBatch.png)
There is clear signs of batch effect. The samples from same dataset are largely clustered together. To remove the batch effect, we need to run comBat batch correction algorithm
```
#Apply batch effect correction
PerformBatchCorrection();

#Check the result 
PlotMetaPCA("qc_meta_pca_afterBatch_","72", "png", "");
```
Here is the result after batch correction.
![PCA Plot](https://dev.expressanalyst.ca/ExpressAnalyst/resources/images/RTutorial/qc_meta_pca_afterBatch.png)
#### 2.5 Perform statistical meta-analysis using combine p-values method
```
PerformPvalCombination("fisher", 0.05)
```
#### 2.6 View result tables
```
analSet <- readSet(analSet, "analSet");
print(head(analSet$meta.mat));
       CombinedTstat CombinedPval
16854         89.093   2.7728e-14
246256        99.964   2.7728e-14
105855        94.751   2.7728e-14
19241        105.030   2.7728e-14
319169        94.339   2.7728e-14
16819        100.880   2.7728e-14
```
For a more detailed table containing additionally log fold change and p-values of features for individual dataset, please check this csv file [meta_sig_genes_metap.csv](https://dev.expressanalyst.ca/ExpressAnalyst/resources/data/RTutorial/meta_sig_genes_metap.csv), it is also generated in your working directory.
