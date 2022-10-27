##################################################
## R script for ExpressAnalyst
## Description: functions for single gene expression data
##
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

#in DE analysis page, select metadata primary factor, secondary factor and blocking factor 
SetSelectedMetaInfo <- function(dataName="", meta0, meta1, block1){
  dataSet <- readDataset(dataName);
  if(meta0 == "NA"){
    RegisterData(dataSet, 0);
  }else{
    cls <- dataSet$meta[, meta0];
    dataSet$fst.cls <- cls; # for PCA plotting
    block <- NULL;
    dataSet$sec.cls <- "NA";
    if(meta1 != "NA"){
      if(block1){
        block <- dataSet$meta[, meta1];
      }else{ # two factor
        cls <- interaction(dataSet$meta[, c(meta0, meta1)], sep = "_", lex.order = TRUE);
      }
      dataSet$sec.cls <- dataSet$meta[, meta1]; # for pca coloring
    }
    dataSet$cls <- cls; # record main cls;
    dataSet$block <- block;
    RegisterData(dataSet, 1);
  }
}

