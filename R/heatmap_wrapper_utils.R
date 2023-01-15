##################################################
## R script for ExpressAnalyst
## Description: Heatmap wrapper functions for onedata, metadata, genelist
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#'Prepare data for heatmap visualization
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
SaveHeatmapJSON <- function(dataName="", fileName){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");

  anal.type <- paramSet$anal.type;

  if(anal.type == "metadata"){
    json.res <- PrepareMetaHeatmapJSON(dataSet);
  }else{
    json.res <- PrepareExpressHeatmapJSON(dataSet);
  }
  
  json.res$org <- paramSet$data.org
  json.res$analType <- anal.type
  json.res$naviString <- "Heatmap Clustering"
  
  #rjson will give different format
  json.mat <- RJSONIO::toJSON(json.res); 
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  paramSet$jsonNms$heatmap <- fileName
  sink(fileName);
  cat(json.mat);
  sink();
  msgSet$current.msg <- "Data is now ready for heatmap visualization!";
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");
  return(1);
}


#'Generate JSON file for heatmap visualization of gene lists
#'@param {object}   dataSetObj Input the name of the created datasetObj (see Init.Data).
#'@param {string}   fileName file name of the output JSON file
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
SaveListHeatmapJSON <- function(dataName="", fileName){
  dataSet <- readDataset(dataName);
  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  if(paramSet$numOfLists>1){
    json.res <- .prepareMultiListHeatmapJSON(dataSet);
  }else{
    json.res <- .prepareListHeatmapJSON(dataSet);
  }

  json.res$org <- paramSet$data.org
  json.res$analType <- anal.type
  json.res$naviString <- "Heatmap List"
  json.mat <- rjson::toJSON(json.res);
  paramSet$jsonNms$heatmap <- fileName

  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(fileName))
  sink(fileName);
  cat(json.mat);
  sink();
  msgSet$current.msg <- "Data is now ready for heatmap visualization!";
  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");

  return(RegisterData(dataSet));
}