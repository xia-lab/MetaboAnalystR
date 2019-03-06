## ---- eval = FALSE-------------------------------------------------------
#  if (packageVersion("xcms") < "3.3.3")
#      devtools::install_github("sneumann/xcms")

## ---- eval = FALSE-------------------------------------------------------
#  # Load necessary libraries
#  library(xcms)
#  library(MetaboAnalystR)
#  
#  # Create file path to example data
#  cdfpath <- system.file("cdf", package = "faahKO")
#  cdffiles <- list.files(cdfpath, recursive = TRUE, full.names = TRUE)
#  
#  # Define the group assignment of the samples
#  smp_grp <- rep("WT", length(cdffiles))
#  smp_grp[grep("ko", basename(cdffiles))] <- "KO"
#  
#  # Define a data.frame with sample descriptions
#  pd <- data.frame(file = cdffiles, sample_group = smp_grp)
#  
#  # Read the files
#  data <- readMSData(cdffiles, pd = new("NAnnotatedDataFrame", pd),
#                     mode = "onDisk")
#  
#  # Perform chromatographic peak detection using default settings.
#  data <- findChromPeaks(data, param = MatchedFilterParam())
#  
#  # Perform the alignment after a first peak grouping.
#  data <- groupChromPeaks(data, param = PeakDensityParam(
#                                    sampleGroups = data$sample_group))
#  data <- adjustRtime(data, param = PeakGroupsParam(family = "symmetric"))
#  
#  # Perform the correspondence analysis
#  data <- groupChromPeaks(data, param = PeakDensityParam(
#                                    sampleGroups = data$sample_group, bw = 10))
#  
#  # At last filling-in missing peak data.
#  data <- fillChromPeaks(data, param = FillChromPeaksParam())
#  
#  # Export the feature table in the MetaboAnalyst format. Parameter 'label'
#  # defines the group assignment of the samples.
#  exportMetaboAnalyst(data, file = "met_test1.csv", label = data$sample_group)
#  
#  # Perform data analysis using the MetaboAnalystR package
#  # First step is to create the mSet Object, specifying that the data to be uploaded
#  # is a peak table ("pktable") and that statistical analysis will be performed ("stat").
#  mSet <- InitDataObjects("pktable", "stat", FALSE)
#  
#  # The second step is to read in the processed data (created above)
#  mSet <- Read.TextData(mSet, "met_test1.csv", "colu", "disc");
#  
#  # The third step is to perform data processing using MetaboAnalystR (filtering/normalization)
#  mSet <- SanityCheckData(mSet)
#  mSet <- ReplaceMin(mSet);
#  mSet <- FilterVariable(mSet, "iqr", "F", 25)
#  mSet <- PreparePrenormData(mSet)
#  mSet <- Normalization(mSet, "NULL", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)
#  mSet <- PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA)
#  mSet <- PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA)
#  
#  # The fourth step is to perform fold-change analysis
#  mSet <- FC.Anal.unpaired(mSet, 2.0, 0)
#  mSet <- PlotFC(mSet, "fc_0_", "png", 72, width=NA)
#  
#  # The fifth step is to perform t-test analysis
#  mSet <- Ttests.Anal(mSet, F, 0.05, FALSE, TRUE)
#  mSet <- PlotTT(mSet, "tt_0_", "png", 72, width=NA)
#  
#  # The sixth step is to perform PCA
#  mSet <- PCA.Anal(mSet)
#  mSet <- PlotPCAPairSummary(mSet, "pca_pair_0_", "png", 72, width=NA, 5)
#  mSet <- PlotPCAScree(mSet, "pca_scree_0_", "png", 72, width=NA, 5)
#  mSet <- PlotPCA2DScore(mSet, "pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
#  mSet <- PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2,"scatter", 1);
#  mSet <- PlotPCABiplot(mSet, "pca_biplot_0_", "png", 72, width=NA, 1,2)
#  mSet <- PlotPCA3DScoreImg(mSet, "pca_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
#  
#  # The seventh step is to perform PLS-DA
#  mSet <- PLSR.Anal(mSet, reg=TRUE)
#  mSet <- PlotPLSPairSummary(mSet, "pls_pair_0_", "png", 72, width=NA, 5)
#  mSet <- PlotPLS2DScore(mSet, "pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,1,0)
#  mSet <- PlotPLS3DScoreImg(mSet, "pls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
#  mSet <- PlotPLSLoading(mSet, "pls_loading_0_", "png", 72, width=NA, 1, 2,"scatter", 1);
#  mSet <- PLSDA.CV(mSet, "L",5, "Q2")
#  mSet <- PlotPLS.Classification(mSet, "pls_cv_0_", "png", 72, width=NA)
#  mSet <- PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)
#  
#  # The last step is to create a summary report of the statistical analysis
#  PreparePDFReport(mSet, "User Name")

