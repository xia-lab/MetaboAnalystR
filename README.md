# Description

**_ExpressAnalystR_** is the underlying R package synchronized with ExpressAnalyst web server. It is designed for statistical analysis, enrichment analysis and visual analytics of single and multiple gene expression data, both matrix and gene list. The R package is composed of R functions necessary for the web-server to perform network creation, trimming and analysis. 

Following installation and loading of ExpressAnalystR, users will be able to reproduce web server results from their local computers using the R command history downloaded from ExpressAnalystR. Running the R functions will allow more flexibility and reproducibility.

Note - ExpressAnalystR is still under development - we cannot guarantee full functionality
# Installation

**Step 1. Install package dependencies**

To use OmcisNetR, make sure your R version is >4.0.3 and install all package dependencies. Ensure that you are able to download packages from Bioconductor. To install package dependencies, use the pacman R package. Note that some of these packages may require additional library dependencies that need to be installed prior to their own successful installation.

```
install.packages("pacman")

library(pacman)

pacman::p_load(igraph, RColorBrewer, qs, rjson, RSQLite)
```

**Step 2. Install the package**

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

# Tips for using the ExpressAnalystR package

1. The first function that you will use in every module is the `Init.Data` function, which initiates R objects that stores user's data, parameters for further processing and analysis.
2. The ExpressAnalystR package will output data files/tables/analysis/networks outputs in your current working directory.
3. Every function must be executed in sequence as it is shown on the R Command history, please do not skip any commands as this can result in errors downstream.
4. Main functions in ExpressAnalystR are documented. Use the _?Function_ format to open its documentation. For instance, use `?ExpressAnalystR::ReadTabExpression` to find out more about this function.
5. It is recommended to set the working folder to an empty folder because numerous files will be generated through the process.
6. R package is not useful for visual analytics as they are hosted on the website. It's mainly useful for statistical analysis (differential expression and meta-analysis).
7. Results from R functions are saved in a format qs file named as the file name of original data table. For gene list, the format qs file is named "datalist1". use this function to access: 
```
dataSet <- readDataset(fileName)
```

# Examples

## Starting from a gene expression matrix

```
library(ExpressAnalystR)

# Step 1. Initiate R objects, boolean indicates local mode (vs web mode); Set analysis type to single gene expression matrix
Init.Data(FALSE);
SetAnalType("onedata");

# Step 2. Read in the gene expression matrix
ReadTabExpressData("estrogen.txt");

# Step 3. Annotate gene IDs to Entrez
PerformDataAnnot("estrogen.txt", "hsa", "array", "hgu95av2", "mean");

# Step 4. Perform data filtering and normalization

# Step 5. Prepare differential expression (DE) analysis
SetSelectedMetaInfo("estrogen.txt","ER", "NA", F)
SetupDesignMatrix("estrogen.txt", "limma");

# Step 6. Perform DE analysis
PerformDEAnal("estrogen.txt", "custom", "absent vs. present", "NA", "intonly");

# Step 7. Get current result and check DE result table
dataSet <- readDataset("estrogen.txt");
View(dataSet$comp.res);
```

## Starting from three datasets for meta-analysis

```
library(ExpressAnalystR)

# Step 1. Initiate R objects, boolean indicates local mode (vs web mode); Set analysis type to gene list
Init.Data(FALSE);
SetAnalType("metadata");

# Step 2. Process each individual dataset
ReadOmicsData("E-GEOD-25713.txt");
SanityCheckData("E-GEOD-25713.txt");
AnnotateGeneData("E-GEOD-25713.txt", "mmu", "entrez");
RemoveMissingPercent("E-GEOD-25713.txt", 0.5)
ImputeMissingVar("E-GEOD-25713.txt", "min")
NormalizingDataMeta("E-GEOD-25713.txt", "NA", "NA", "NA");
DoStatComparison("E-GEOD-25713.txt", "limma", "CLASS","NA","NA","NA", 0.05, 0.0);

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
FilteringData("GSE69588.txt","pct","0", "0");
NormalizingDataMeta("GSE69588.txt", "NA", "NA", "NA");
DoStatComparison("GSE69588.txt", "limma", "CLASS","NA","NA","NA", 0.05, 0.0);

# Step 3. Perform data integrity check (compatibility)
CheckMetaDataIntegrity();

# Step 4. Perform batch correction
PerformBatchCorrection();

# Step 5. Perform statistical meta-analysis by using combine p-values method
PerformPvalCombination("fisher", 0.05)

# Step 6. Get result table
analSet <- readSet(analSet, "analSet");
View(analSet$meta.mat)

```