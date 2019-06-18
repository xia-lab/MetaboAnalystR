PrepareSignatureOfMetaboAnalyst <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  save.image("check.RData");
  if(exists('sam.cmpds', where=mSetObj$analSet)){
    if(dim(mSetObj$analSet$sam.cmpds)[1] > 0){
      signature.cmpds <- mSetObj$analSet$sam.cmpds;
      signature.cmpds <- as.matrix(signature.cmpds[,4])
    }
  }else if(exists('ebam.cmpds', where=mSetObj$analSet)){
    if(dim(mSetObj$analSet$ebam.cmpds)[1] > 0){
      signature.cmpds <- mSetObj$analSet$ebam.cmpds;
      signature.cmpds <- as.matrix(signature.cmpds[,3])
    }
  }else if(exists('fc', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$fc$sig.mat;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,2])
    }
  }else if(exists('tt', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$tt$sig.mat;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,2])
    }
  }else if(exists('volcano', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$volcano$sig.mat;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,2])
    }
  }else if(exists('pca', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$pca$imp.loads;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,1])
    }
  }else if(exists('rf', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$rf$importance;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- as.matrix(sig.mat[,ncol(sig.mat)])
    }
  }else if(exists('svm', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$svm$sig.mat;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,1])
    }
  }else if(exists('oplsda', where=mSetObj$analSet)){
    sig.mat <- mSet$analSet$oplsda$loadingMN;
    if(dim(sig.mat)[1] > 0){
      signature.cmpds <- sig.mat;
      signature.cmpds <- as.matrix(sig.mat[,1])
    }
  }else{
      return(-1);
  }

  if(mSetObj$dataSet$type == "conc"){
    LoadScripts("mset")
    cmpd.vec <- rownames(signature.cmpds);
    Setup.MapData(mSet, cmpd.vec);
    CrossReferencing(mSet, "name");
    CreateMappingResultTable(mSet);
    cmpdlist <- unlist(mSet$dataSet$map.table[,3]);
    signature.cmpds <- as.matrix(signature.cmpds[cmpdlist != "NA",]);
    cmpdlist <- cmpdlist[cmpdlist != "NA"];
    rownames(signature.cmpds) <- cmpdlist
    signature.type <- "hmdb"
    save(signature.cmpds, signature.type, file="RShare_metaboanalyst.RData"); 
  }else if((mSetObj$dataSet$type == "mspeak") || (mSetObj$dataSet$type == "pktable")){
    LoadScripts("mummichog")
    InitDataObjects("mass_all", "mummichog", FALSE)
    SetPeakFormat("rmp")
    UpdateInstrumentParameters(mSet, "1.0", "positive");
    m.z <- sapply(strsplit(rownames(signature.cmpds), "/"), function(x) {x[[1]]})
    write.table(x = m.z, file = "peaklist.csv", row.names = FALSE, col.names = "m.z", quote = FALSE);
    Read.PeakListData(mSet, "peaklist.csv");
    SanityCheckMummichogData(mSet)
    SetPeakEnrichMethod(mSet, "mum")
    SetMummichogPval(mSet, 0.0)
    PerformPSEA(mSet, "hsa_mfn")
    
    # handle cases with repeated ID matching of m.z values
    # & cases with missed matchings for m.z values
    idx <- match(unlist(unique(mSet$matches.res[,1])), unique(m.z))
    signature.cmpds <- as.matrix(signature.cmpds[idx,])
    idx <- match(unique(mSet$matches.res[,1]), mSet$matches.res[,1])
    mSet$matches.res <- mSet$matches.res[idx,]
    rownames(signature.cmpds) <- mSet$matches.res[,2]
    signature.type <- "kegg"
    save(signature.cmpds, signature.type, file="RShare_metaboanalyst.RData"); 
  }

  return(1);
}