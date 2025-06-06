
CreateTableRmdReport <- function(mSetObj, usrName, moduleNames){
  
  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateStatIntr();
  
  CreateDataProcdoc(mSetObj);
  CreateNORMdoc(mSetObj);
  
  InitStatAnalMode();

  if(exists("analSet", where = mSetObj)){
      for(nm in moduleNames){
        CreateDocs(mSetObj, nm);
      }

  }else{
    CreateAnalNullMsg();
  }
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}


CreateListRmdReport <- function(mSetObj, usrName, moduleNames){
  
  mSetObj <- .get.mSet(mSetObj);
  
  CreateHeader(usrName);
  CreateEnrichIntr();
   
  CreateEnrichProcessDoc(mSetObj);
  
  if(exists("analSet", where = mSetObj)){
      for(nm in moduleNames){
        CreateDocs(mSetObj, nm);
      }

  }else{
    CreateAnalNullMsg();
  }
  AddFeatureImages(mSetObj);
  CreateRHistAppendix();
  CreateFooter();
}

CreateDocs <-function(mSetObj, module){
    if(module == "stat"){
        CreateDocs_stat(mSetObj)
    }else if(module == "roc"){
        CreateDocs_roc(mSetObj)

    }else if(module == "mf"){
        CreateDocs_mf(mSetObj)

    }else if(module == "dose"){
        CreateDocs_dose(mSetObj)
    }else if(module == "mset" || module == "enrich" || module == "msetssp" || module == "msetqea" || module == "msetora"  ){
        CreateDocs_enrich(mSetObj)
    }else if(module == "path"){
        CreateDocs_dose(mSetObj)
    }
}

CreateDocs_enrich <- function(mSetObj){

  if(mSetObj$analSet$type == "msetssp"){
    CreateEnrichSSPdoc(mSetObj);
  }
  if(mSetObj$analSet$type == "msetqea"){
    CreateNORMdoc(mSetObj);
  }

  CreateEnrichAnalDoc();
  
  if(mSetObj$analSet$type == "msetqea"){
    CreateEnrichQEAdoc(mSetObj);
  }
  if(mSetObj$analSet$type == "msetora"){
    CreateEnrichORAdoc(mSetObj);
  }

}

CreateDocs_pathway <- function(mSetObj){
  CreatePathProcessDoc(mSetObj);
  if(mSetObj$analSet$type == "pathqea"){
    CreateDataProcdoc(mSetObj);
    CreateNORMdoc(mSetObj);
  }
  CreatePathAnalDoc(mSetObj);
  CreatePathResultDoc(mSetObj);
}

CreateDocs_stat<- function(mSetObj){
    CreateUNIVdoc(mSetObj);
    CreateANOVAdoc(mSetObj);
    CreateCorrDoc(mSetObj);
    CreateDSPCdoc(mSetObj);
    CreateSAMdoc(mSetObj);
    CreateEBAMdoc(mSetObj);
    CreatePCAdoc(mSetObj);
    CreatePLSdoc(mSetObj);
    CreateOPLSDAdoc(mSetObj);
    CreateSPLSDAdoc(mSetObj);
    CreateHCdoc(mSetObj);
    CreateKMdoc(mSetObj);
    CreateSOMdoc(mSetObj);
    CreateRFdoc(mSetObj);
    CreateSVMdoc(mSetObj);
}

CreateDocs_mf <- function(mSetObj){
    CreateMetaOverview(mSetObj);
    CreateiPCAdoc(mSetObj);
    CreateCorHeatmap(mSetObj);

    CreateCovAdj(mSetObj);
    CreateCorAnalysis(mSetObj);
    CreateAOV2doc(mSetObj);
     
    CreateASCAdoc(mSetObj);
    CreateMBdoc(mSetObj);

    CreateRandomForest(mSetObj);
}

CreateDocs_dose <- function(mSetObj){
  CreateDoseParametersDoc(mSetObj);
  CreateDoseAnalDoc(mSetObj);
}

CreateDocs_roc <- function(mSetObj){
  CreateBiomarkerRatioOverview(mSetObj);
  CreateUnivarBiomarkersDoc(mSetObj);
  CreateMultiBiomarkersDoc(mSetObj);
  CreateModelBiomarkersDoc(mSetObj);
}
