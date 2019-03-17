## ---- eval=FALSE---------------------------------------------------------
#  
#  # Load the MetaboAnalystR package
#  library("MetaboAnalystR")
#  
#  # Import mzML files, set path to folder containing two subfolders of samples
#  rawData <- ImportRawMSData("~/Desktop/MetaboAnalystR_workflow/MetaboAnalystR_workflow_iHMP/iHMP",
#                              format = "png", dpi = 72, width = 9)
#  

## ---- out.width = "70%"--------------------------------------------------
knitr::include_graphics("TICS_72.png")

## ---- out.width = "70%"--------------------------------------------------
knitr::include_graphics("BPIS_72.png")

## ---- eval=FALSE---------------------------------------------------------
#  # Set parameters for peak profiling
#  peakParams <- SetPeakParam(ppm = 5)
#  
#  # Perform peak profiling
#  extPeaks <- PerformPeakProfiling(rawData, peakParams, rtPlot = TRUE, pcaPlot = TRUE, labels = TRUE,
#                                 format = "png", dpi = 300, width = 9)
#  

## ---- out.width = "70%"--------------------------------------------------
knitr::include_graphics("PCA_plot300.png")

## ---- eval=FALSE---------------------------------------------------------
#  # Set parameters for peak annotation
#  annParams <- SetAnnotationParam(polarity = "negative", mz_abs_add = 0.005)
#  
#  # Perform peak annotation
#  annotPeaks <- PerformPeakAnnotation(extPeaks, annParams)
#  
#  # Format and filter the peak list for MetaboAnalystR
#  maPeaks <- FormatPeakList(annotPeaks, annParams, filtIso = TRUE, filtAdducts = FALSE,
#                            missPercent = 0.75)
#  

## ---- eval=FALSE---------------------------------------------------------
#  # Initialize the mSetObj
#  mSet<-InitDataObjects("pktable", "stat", FALSE)
#  
#  # Read in the filtered peak list
#  mSet<-Read.TextData(mSet, "metaboanalyst_input.csv", "colu", "disc")
#  
#  # Perform data processing
#  mSet<-SanityCheckData(mSet)
#  mSet<-ReplaceMin(mSet);
#  mSet<-FilterVariable(mSet, "iqr", "F", 25)
#  mSet<-PreparePrenormData(mSet)
#  mSet<-Normalization(mSet, "MedianNorm", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)
#  
#  # Perform t-test
#  mSet<-Ttests.Anal(mSet, F, 0.25, FALSE, TRUE)
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Convert the output of the t-test to the proper input for mummichog analysis
#  mSet<-Convert2Mummichog(mSet)
#  
#  # Read in the ranked peak list (don't forget to change the file name!)
#  mSet<-Read.PeakListData(mSet, "mummichog_input_2019-03-06.txt");
#  mSet<-UpdateMummichogParameters(mSet, "5", "negative", 0.25);
#  mSet<-SanityCheckMummichogData(mSet)
#  
#  # Perform original mummichog algorithm
#  mSet<- PerformMummichog(mSet, "hsa_mfn", permNum = 1000)
#  
#  # View the top pathway hits using head(mSet$mummi.resmat)
#  # > head(mSet$mummi.resmat)
#  #                                                   Pathway total Hits.total Hits.sig      EASE        FET     Gamma
#  # Vitamin E metabolism                                         54         33        4 0.0054183 0.00049465 0.0010642
#  # Linoleate metabolism                                         46         20        1 1.0000000 0.21962000 1.0000000
#  # Carnitine shuttle                                            72         20        1 1.0000000 0.21962000 1.0000000
#  # Androgen and estrogen biosynthesis and metabolism            95         65        1 1.0000000 0.55974000 1.0000000
#  # Drug metabolism - cytochrome P450                            53         42        1 1.0000000 0.40860000 1.0000000
#  # Ascorbate (Vitamin C) and Aldarate Metabolism                29         19        1 1.0000000 0.20981000 1.0000000

## ---- eval=FALSE---------------------------------------------------------
#  # Perform GSEA
#  mSet<- PerformGSEA(mSet, "hsa_mfn", permNum = 1000)
#  
#  # View the top pathway hits using head(mSet$mummi.gsea.resmat)
#  # > head(mSet$mummi.gsea.resmat)
#  #                                                   Pathway_Total Hits P_val    P_adj   NES
#  # Linoleate metabolism                              "46"          "1"  "0.998"  "0.998" "-0.4419"
#  # Carnitine shuttle                                 "72"          "1"  "0.998"  "0.998" "-0.2882"
#  # Vitamin E metabolism                              "54"          "4"  "0.8219" "0.998" "-0.7053"
#  # Androgen and estrogen biosynthesis and metabolism "95"          "1"  "0.998"  "0.998" "0.3674"
#  # Drug metabolism - cytochrome P450                 "53"          "1"  "0.998"  "0.998" "0.276"
#  # Ascorbate (Vitamin C) and Aldarate Metabolism     "29"          "1"  "0.998"  "0.998" "-0.6419"
#  
#  # Plot with the top 5 pathways found in both algorithms annotated
#  mSet <- PlotIntegPaths(mSet, dpi = 300, width = 10, format = "jpg", labels = "default")
#  

## ---- out.width = "70%"--------------------------------------------------

# Read in the peak table, perform data processing
mSet<-InitDataObjects("pktable", "stat", FALSE)
mSet<-Read.TextData(mSet, "iHMP2_48_metaboanalyst_input.csv", "colu", "disc")
mSet<-SanityCheckData(mSet)
mSet<-ReplaceMin(mSet);
mSet<-FilterVariable(mSet, "iqr", "F", 25)
mSet<-PreparePrenormData(mSet)
mSet<-Normalization(mSet, "MedianNorm", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)

# View the OPLS-DA plot
mSet<-OPLSR.Anal(mSet, reg=TRUE)
mSet<-PlotOPLS2DScore(mSet, "opls_score2d_0_", "png", 72, width=NA, 1,2,0.95,0,0)

knitr::include_graphics("opls_score2d_0_dpi72.png")

# Re-perform normalization, without auto-scaling 
mSet<-Normalization(mSet, "MedianNorm", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)

# Perform t-test
mSet<-Ttests.Anal(mSet, F, 0.05, FALSE, TRUE)

# Convert results to mummichog analysis
mSet<-Convert2Mummichog(mSet)

# RENAME FILE TO CREATED mummichog_input
mSet<-Read.PeakListData(mSet, "mummichog_input_2019-03-17.txt");
mSet<-UpdateMummichogParameters(mSet, "5", "negative", 0.25);
mSet<-SanityCheckMummichogData(mSet)

# First perform original mummichog algorithm
mSet<- PerformMummichog(mSet, "hsa_mfn", permNum = 1000)

# View the top enriched pathways
# > head(mSet$mummi.resmat)
#                                         Pathway total Hits.total Hits.sig      EASE       FET      Gamma
# Bile acid biosynthesis                             82         52       29 0.0062805 0.0028215 0.00027637
# Vitamin E metabolism                               54         33       20 0.0094632 0.0035614 0.00028122
# Fatty Acid Metabolism                              63         11        9 0.0152200 0.0026797 0.00029021
# Vitamin D3 (cholecalciferol) metabolism            16         10        8 0.0314750 0.0061618 0.00031722
# De novo fatty acid biosynthesis                   106         15       10 0.0520170 0.0162020 0.00035507
# Fatty acid activation                              74         15       10 0.0520170 0.0162020 0.00035507

mSet<- PerformGSEA(mSet, "hsa_mfn", permNum = 1000)

# > head(mSet$mummi.gsea.resmat)
#                                                           Pathway_Total Hits P_val      P_adj    NES     
# Bile acid biosynthesis                                    "82"          "29" "0.001805" "0.1408" "-1.984"
# Vitamin D3 (cholecalciferol) metabolism                   "16"          "10" "0.0239"   "0.6213" "-1.634"
# Biopterin metabolism                                      "22"          "7"  "0.0221"   "0.6213" "-1.59" 
# Putative anti-Inflammatory metabolites formation from EPA "27"          "4"  "0.07234"  "0.6727" "1.429" 
# Androgen and estrogen biosynthesis and metabolism         "95"          "11" "0.07762"  "0.6727" "-1.446"
# Arachidonic acid metabolism                               "95"          "8"  "0.07761"  "0.6727" "1.435" 

# View plot with no dots labeled
# mSet <- PlotIntegPaths(mSet, dpi = 300, width = 10, format = "jpg", labels = "none")

# View plot with important pathways labeled
mSet <- PlotIntegPaths(mSet, dpi = 300, width = 10, format = "jpg", labels = "default")

knitr::include_graphics("integ_path_plotdpi300.jpg")


