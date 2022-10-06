##################################################
## R script for ExpressAnalyst
## Description: Functions for enrichment analysis (GSEA and ORA)
## Authors:
## G. Zhou, guangyan.zhou@mail.mcgill.ca
## Jeff Xia, jeff.xia@mcgill.ca
###################################################  

GetRidgePlot <- function(dataName, imgNm = "abc", dpi=72, format="png", fun.type = "kegg", ridgeType = "ora", ridgeColor = "orange", gseaRankOpt="", sigLevel = 0.05, pwNum=20, inx = 1){
    dataSet <- readDataset(dataName);
    if(!exists(".compute.ridgeline")){ # public web on same user dir
        compiler::loadcmp("../../rscripts/ExpressAnalystR/R/_utils_ridgeline.Rc");    
    }
    return(.compute.ridgeline(dataSet, imgNm, dpi, format, fun.type, ridgeType, ridgeColor, sigLevel, pwNum, inx));
}