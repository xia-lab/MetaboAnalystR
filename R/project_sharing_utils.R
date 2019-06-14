PrepareSignatureOfMetaboAnalyst <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  #prepare metaboanalyst files
  LoadScripts("mset")
  
  cmpd.vec <- mSet$dataSet$cmpd;
  Setup.MapData(mSet, cmpd.vec);
  CrossReferencing(mSet, "name");
  CreateMappingResultTable(mSet);
  cmpdlist <- unlist(mSet$dataSet$map.table[,3]);
  cmpdlist <- cmpdlist[!is.na(cmpdlist)];
  mSetObj$cmpdlist <- paste(cmpdlist, collapse = "\n")
  
  return(.set.mSet(mSetObj));
}
