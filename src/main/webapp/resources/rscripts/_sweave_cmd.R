# This script is called from Java directly without using Rserve
# in order to get around the PDF issue
args <- commandArgs(trailingOnly = TRUE);
setwd(args[1]);
load("SweaveImage.RData");
Sweave("Analysis_Report.Rnw");
tools::texi2dvi("Analysis_Report.tex", pdf = TRUE, quiet=TRUE);