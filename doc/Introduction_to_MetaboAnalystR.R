## ---- eval=FALSE---------------------------------------------------------
#  # Load MetaboAnalystR
#  library(MetaboAnalystR)
#  

## ---- eval=FALSE---------------------------------------------------------
#  # Create objects for storing processed data
#  mSet <- InitDataObjects("conc", "stat", FALSE)
#  
#  # Read in the data and fill in the dataSet list
#  
#  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/human_cachexia.csv", "rowu", "disc")
#  
#  # To view messages from the data import and processing
#  mSet$msgSet$read.msg
#  
#  # Example of messages
#  [1] "Samples are in rows and features in columns"
#  [2] "The uploaded file is in comma separated values (.csv) format."
#  [3] "The uploaded data file contains 77 (samples) by 63 (compounds) data matrix."

## ---- eval=FALSE---------------------------------------------------------
#  mSet <- InitDataObjects("pktable", "stat", FALSE)
#  
#  mSet <- Read.TextData(mSet, "http://www.metaboanalyst.ca/resources/data/NMRpeaklistskidney.csv", "rowu", "disc")

## ---- eval=FALSE---------------------------------------------------------
#  # Create an object for storing processed data
#  mSet <- InitDataObjects("mspeak", "stat", FALSE)
#  
#  # Unzips the uploaded zip file/s, removes it and saves it as "upload"
#  UnzipUploadedFile("lcms_3col_peaks.zip", "upload", F)
#  
#  # Read peak lists/intensity files
#  mSet <- Read.PeakList(mSet, "upload")

## ---- eval=FALSE---------------------------------------------------------
#  # Perform grouping of peaks
#  mSet <- GroupPeakList(mSet, 0.025, 30.0)
#  
#  # Form peak groups
#  mSet <- SetPeakList.GroupValues(mSet)
#  
#  # View message resulting from peak grouping (Optional, though for your benefit)
#  mSet$dataSet$proc.msg
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Run the sanity check, it will return a series of messages if the data is suitable for subsequent analyses.
#  mSet <- SanityCheckData(mSet)
#  
#   # [1] "Successfully passed sanity check!"
#   # [2] "Samples are not paired."
#   # [3] "2 groups were detected in samples."
#   # [4] "Only English letters, numbers, underscore, hyphen and forward slash (/) are allowed."
#   # [5] "<font color=\"orange\">Other special characters or punctuations (if any) will be stripped off.</font>"
#   # [6] "All data values are numeric."
#   # [7] "A total of 0 (0%) missing values were detected."
#   # [8] "<u>By default, these values will be replaced by a small value.</u>"
#   # [9] "Click <b>Skip</b> button if you accept the default practice"
#  # [10] "Or click <b>Missing value imputation</b> to use other methods"

## ---- eval=FALSE---------------------------------------------------------
#  # Replace missing/zero values with a minimum positive value
#  mSet <- ReplaceMin(mSet)
#  
#  # View messages collected during ReplaceMin()
#  mSet$msgSet$replace.msg
#  
#  # Example of message for replacing values
#  [1] "Zero or missing variables were replaced with a small value: 0.395"
#  

## ---- eval=FALSE---------------------------------------------------------
#  # STEP 1: Remove features containing a user-defined % cut-off of missing values
#  mSet <- RemoveMissingPercent(mSet, percent=0.5)
#  
#  # STEP 2: Remove variables with missing values
#  mSet <- ImputeVar(mSet, method="exclude")
#  
#  ######### Alternative Step 2: Replace missing values with KNN imputed values
#  mSet <- ImputeVar(mSet, method="knn")

## ---- eval=FALSE---------------------------------------------------------
#  # Check if the sample size is too small, returns a 0 if the data passes the check
#  mSet<-IsSmallSmplSize(mSet)
#  [1] 0

## ---- eval=FALSE---------------------------------------------------------
#  ### OPTION 1) Perform Probabilistic Quotient Normalization based upon a reference sample
#  mSet<-PreparePrenormData(mSet)
#  mSet<-Normalization(mSet, "ProbNormF", "NULL", "NULL", "PIF_178", ratio=FALSE, ratioNum=20)
#  
#  ### OPTION 2) Normalize by reference feature
#  mSet<-PreparePrenormData(mSet)
#  mSet<-Normalization(mSet, "CompNorm", "NULL", "NULL", "1,6-Anhydro-beta-D-glucose", ratio=FALSE, ratioNum=20)
#  
#  ### OPTION 3) Perform quantile normalization, log transformation, and mean-center scaling
#  mSet<-PreparePrenormData(mSet)
#  mSet<-Normalization(mSet, "QuantileNorm", "LogNorm", "MeanCenter", ref=NULL, ratio=FALSE, ratioNum=20)

## ---- eval=FALSE---------------------------------------------------------
#  # View feature normalization
#  mSet<-PlotNormSummary(mSet, "feature_norm", format="png", dpi=300, width=0)
#  
#  # View sample normalization
#  mSet<-PlotSampleNormSummary(mSet, "sample_norm", format="pdf", width=NA)
#  

## ---- eval=FALSE---------------------------------------------------------
#  # Filter variables based on the median absolute deviation
#  mSet <- FilterVariable(mSet, "mad", "F", 15)
#  
#  # Filter variables using QC-samples and a RDS threshold of 25
#  mSet <- FilterVariable(mSet, "none", "T", 25)

## ---- eval=FALSE---------------------------------------------------------
#  # Remove a sample from the data set, in this case sample "PIF_178"
#  mSet <- UpdateSampleItems(mSet, "PIF_178")
#  
#  # Remove a feature from the data set
#  mSet <- UpdateFeatureItems(mSet, "2-Aminobutyrate")
#  
#  # Remove a group from the data set, in this case remove the "control" samples
#  mSet <- UpdateGroupItems(mSet, "control")

## ---- eval=FALSE---------------------------------------------------------
#  # Create Biomarker Sweave report
#  PreparePDFReport(mSet, "User Name")
#  
#  # To save all files created during your session
#  SaveTransformedData(mSet)
#  

