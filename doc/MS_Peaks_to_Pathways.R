## ---- eval=FALSE---------------------------------------------------------
#  # Create objects for storing processed data from the MS peaks to pathways module
#  mSet <- InitDataObjects("mass_all", "mummichog", FALSE)
#  
#  # Read in peak-list data
#  mSet <- Read.PeakListData(mSet, "https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/mummichog_mzs.txt");
#  
#  # Set parameters for analysis, in this case the mass accuracy is set to 3 ppm, the mode of the MS instrument is positive, and the p-value cut-off is 0.0001
#  mSet <- UpdateMummichogParameters(mSet, "0.1", "positive", 1.0E-4);
#  
#  # Sanity check of the uploaded data
#  mSet <- SanityCheckMummichogData(mSet)
#  
#  # Perform the mummichog algorith, in this case the model is the human MFN model, using Fisher's p-value and gamma-adjustment of p-
#  # This function may take sometime for processing, and will output the pathway-results and the compound matching tables in your working directory
#  mSet <- PerformMummichog(mSet, "hsa_mfn", "fisher", "gamma")
#  
#  # To view the results of the pathway analysis in R, use mSet$mummi.resmat
#  

