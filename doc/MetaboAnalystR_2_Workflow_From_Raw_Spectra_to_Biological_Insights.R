## ---- eval=FALSE---------------------------------------------------------
#  
#  # Load the MetaboAnalystR package
#  library("MetaboAnalystR")
#  
#  # Import mzML files, set path to folder containing two subfolders of samples
#  rawData <- ImportRawMSData("~/Desktop/Mai/MetaboAnalystR_workflow/MetaboAnalystR_workflow_iHMP/iHMP",
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
#  SetPeakFormat("mpt")
#  mSet<-Read.PeakListData(mSet, "INPUT_FILE_NAME");
#  mSet<-UpdateInstrumentParameters(mSet, "5", "negative");
#  mSet<-SanityCheckMummichogData(mSet)
#  
#  # Perform original mummichog algorithm
#  mSet<-SetPeakEnrichMethod(mSet, "mum")
#  mSet<-SetMummichogPval(mSet, 0.25)
#  mSet<-PerformPSEA(mSet, "hsa_mfn", permNum = 1000)
#  mSet<-PlotPeaks2Paths(mSet, "peaks_to_paths_0_", "png", 72, width=NA)
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
#  mSet<-SetPeakEnrichMethod(mSet, "gsea")
#  mSet<-PerformPSEA(mSet, "hsa_mfn", permNum = 1000)
#  mSet<-PlotPeaks2Paths(mSet, "peaks_to_paths_1_", "png", 72, width=NA)
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

## ---- eval=FALSE---------------------------------------------------------
#  #Perform meta-analysis of mummichog and GSEA results
#  mSet<-SetPeakEnrichMethod(mSet, "integ")
#  mSet<-SetMummichogPval(mSet, 0.25)
#  mSet<-PerformPSEA(mSet, "hsa_mfn")
#  mSet<-PlotIntegPaths(mSet, "integ_peaks_0_", "png", 72, width=NA)
#  # View the results with > head(mSet$integ.resmat)
#  
#  # Plot with the top 5 pathways found in both algorithms annotated
#  mSet <- PlotIntegPaths(mSet, dpi = 300, width = 10, format = "jpg", labels = "default")

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
SetPeakFormat("mpt")
mSet<-Read.PeakListData(mSet, "mummichog_input_2019-04-11.txt");
mSet<-UpdateInstrumentParameters(mSet, "5", "negative");
mSet<-SanityCheckMummichogData(mSet)

# First perform the original mummichog algorithm
mSet<-SetPeakEnrichMethod(mSet, "mum")
mSet<-SetMummichogPval(mSet, 0.25)
mSet<-PerformPSEA(mSet, "hsa_mfn", permNum = 1000)
mSet<-PlotPeaks2Paths(mSet, "peaks_to_paths_0_", "png", 300, width=NA)

# View the top enriched pathways
# > head(mSet$mummi.resmat)
#                                       Pathway total Hits.total Hits.sig Expected       FET      EASE    Gamma Emp.Hits
# Fatty Acid Metabolism                              63         11        9   9.4164 0.0026797 0.0152200 0.011247        0
# Bile acid biosynthesis                             82         52       29  12.2560 0.0028215 0.0062805 0.011051        0
# Vitamin E metabolism                               54         33       20   8.0712 0.0035614 0.0094632 0.011120        0
# Vitamin D3 (cholecalciferol) metabolism            16         10        8   2.3915 0.0061618 0.0314750 0.011613        0
# De novo fatty acid biosynthesis                   106         15       10  15.8440 0.0162020 0.0520170 0.012097      149
# Fatty acid activation                              74         15       10  11.0610 0.0162020 0.0520170 0.012097      275

mSet<-SetPeakEnrichMethod(mSet, "gsea")
mSet<-PerformPSEA(mSet, "hsa_mfn", permNum = 1000)
mSet<-PlotPeaks2Paths(mSet, "peaks_to_paths_1_", "png", 300, width=NA)

# > head(mSet$mummi.gsea.resmat)
#                                                  Pathway_Total Hits   P_val  P_adj    NES   
# Biopterin metabolism                                         22   12 0.01538 0.1486 -1.939
# Squalene and cholesterol biosynthesis                        55   33 0.01562 0.1486 -2.060
# Androgen and estrogen biosynthesis and metabolism            95   59 0.01562 0.1486 -2.354
# Bile acid biosynthesis                                       82   52 0.01667 0.1486 -2.340
# C21-steroid hormone biosynthesis and metabolism             112   84 0.01667 0.1486 -1.749
# Nucleotide Sugar Metabolism                                   7    2 0.01754 0.1486 -1.476

knitr::include_graphics("peaks_to_paths_1_dpi300.png")

mSet<-SetPeakEnrichMethod(mSet, "integ")
mSet<-SetMummichogPval(mSet, 0.25)
mSet<-PerformPSEA(mSet, "hsa_mfn")
mSet<-PlotIntegPaths(mSet, "integ_peaks_0_", "png", 300, width=10)

# > head(mSet$integ.resmat)
#                                         Total_Size Hits Sig_Hits Mummichog_Pvals GSEA_Pvals Combined_Pvals
# Bile acid biosynthesis                          82   52       29         0.00282    0.01667        0.00052
# Vitamin D3 (cholecalciferol) metabolism         16   10        8         0.00616    0.03175        0.00187
# Fatty acid activation                           74   15       10         0.01620    0.08333        0.01027
# Fatty Acid Metabolism                           63   11        9         0.00268    0.53850        0.01088
# Biopterin metabolism                            22   12        7         0.10100    0.01538        0.01160
# Vitamin E metabolism                            54   33       20         0.00356    0.65620        0.01650

knitr::include_graphics("integ_peaks_0_dpi300.png")


## ---- out.width = "70%"--------------------------------------------------
# Function to get the m/z hits from a certain metabolic pathway
mSet <- PlotPathwayMZHits(mSet, "Biopterin metabolism", format = "jpg")

knitr::include_graphics("Biopterin metabolism.jpg")


