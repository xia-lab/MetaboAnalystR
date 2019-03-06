## ---- eval=FALSE---------------------------------------------------------
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/data1.csv", "data1.csv", "curl")
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/data2.csv", "data2.csv", "curl")
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/data3.csv", "data3.csv", "curl")
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/data4.csv", "data4.csv", "curl")
#  

## ---- eval=FALSE---------------------------------------------------------
#  # Set working directory to the location of COPIES of your datasets for analysis
#  setwd("set/path/to/copies")
#  
#  # Create objects for storing processed data from meta-analysis
#  mSet <- InitDataObjects("conc", "metadata", FALSE)
#  
#  # Read in example data: adenocarcinoma data2
#  mSet <- ReadIndData(mSet, "data1.csv", "colu");
#  
#  # Sanity check data to ensure it is ready for analysis
#  mSet <- SanityCheckIndData(mSet, "data1.csv")
#  
#  ## to view any messages created during the sanity check
#  mSet$dataSet$check.msg
#  
#  # [1] "Samples are in columns and features in rows."        "No empty rows were found in your data."
#  # [3] "No empty labels were found in your data."            "Two groups found: Adenocarcinoma and Adenocarcinoma"
#  # [5] "All sample names are unique."                        "No empty feature names found"
#  # [7] "All feature names are unique"                        "All sample names are OK"
#  # [9] "All feature names are OK"                            "A total of 83 samples were found."
#  # [11] "A total of 181 features were found."
#  
#  # Perform log-transformation
#  mSet <- PerformIndNormalization(mSet, "data1.csv", "log", 1);
#  
#  #Perform differential expression analysis to identify DE features
#  mSet <- PerformLimmaDE(mSet, "data1.csv", 0.05, 0.0);
#  
#  # Repeat steps for example data3
#  mSet <- ReadIndData(mSet, "data3.csv", "colu");
#  mSet <- SanityCheckIndData(mSet, "data3.csv")
#  mSet <- PerformIndNormalization(mSet, "data3.csv", "log", 1);
#  mSet <- PerformLimmaDE(mSet, "data3.csv", 0.05, 0.0);
#  
#  # Repeat steps for example data4
#  mSet <- ReadIndData(mSet, "data4.csv", "colu");
#  mSet <- SanityCheckIndData(mSet, "data4.csv")
#  mSet <- PerformIndNormalization(mSet, "data4.csv", "log", 1);
#  mSet <- PerformLimmaDE(mSet, "data4.csv", 0.05, 0.0);
#  
#  # Check if meta-data between all uploaded datasets are consistent
#  mSet <- CheckMetaDataConsistency(mSet, F);
#  
#  ###*** Choose one of 3 methods to perform meta-analysis ***###
#  
#  ###*** OPTION 1 - COMBINE P-VALUES ***###
#  mSet <- PerformPvalCombination(mSet, "fisher", 0.05)
#  
#  ###*** OPTION 2 - PERFORM VOTE COUNTING ***###
#  mSet <- PerformVoteCounting(mSet, 0.05, 2.0)
#  
#  ###*** OPTION 3 - MERGE INTO MEGA-DATASET ***###
#  mSet <- PerformMetaMerge(mSet, 0.05)
#  
#  # Create results table
#  mSet <- GetMetaResultMatrix(mSet, "fc")
#  
#  ## To view the results table use mSet$analSet$meta.mat
#  
#  #                                            CombinedLogFC       Pval
#  #pyrophosphate           -1.01060 -0.3676500      -0.69597 0.00088803
#  #pyruvic acid            -1.15400 -0.0045231      -0.60135 0.00468560
#  #glutamine                0.92430  0.2314200       0.58772 0.00468560
#  #taurine                 -0.88704 -0.2703700      -0.58651 0.00468560
#  #lactamide               -0.99086 -0.1404900      -0.57994 0.00468560
#  #adenosine-5-phosphate   -0.89017 -0.1801400      -0.54611 0.00916250
#  #lactic acid             -1.04110  0.0108080      -0.53555 0.01015500
#  #lauric acid             -0.61304 -0.4351300      -0.52095 0.01258500
#  #alpha ketoglutaric acid -0.58456 -0.4026300      -0.49103 0.02223800
#  #maltotriose             -0.62125 -0.3406200      -0.48121 0.02488800
#  #asparagine               0.66667  0.2802600       0.47669 0.02498000
#  #hippuric acid           -0.77823 -0.1091800      -0.45495 0.03645100
#  #citrulline               0.64683  0.2343900       0.44502 0.04135600
#  
#  # Create a box-plot of the expression pattern of a selected feature across the different datasets included in the meta-analysis
#  mSet <- PlotSelectedFeature(mSet, "pyrophosphate")
#  
#  # Prepare data for the Venn Diagram, which will create a Integrated Venn diagram in your working directory (two overlapping circles, highlighting novel biomarker features from the meta-analysis, biomarkers that were consistently identified using meta-analysis and individual DE expression, and biomarkers that were only identified using individual DE expression.)
#  mSet <- PrepareVennData(mSet);
#  
#  # Explore the Venn Diagram in the "vennData" object created
#  
#  # Get names of features overlapping between selected datasets from "vennData"
#  mSet <- GetVennGeneNames(mSet, "data1.csvdata3.csvmeta_dat");
#  
#  # Enter the object below to obtain the names of the features that overlap between all of the studies
#  mSet$dataSet$venn_overlap
#  

