##### Functionalities for processing Metabolon dataset

my.read.metabolon.sheets <- function(mSetObj = NA, metafactor, featureID){
  mSetObj <- .get.mSet(mSetObj);
  filenm <- mSetObj$dataSet$filename;
  
  if(featureID != "NA"){
    dt_ids <- read_excel(filenm, sheet = "Chemical Annotation")
  }
  
  dt_meta <- read_excel(filenm, sheet = "Sample Meta Data")
  dt_table <- read_excel(filenm, sheet = "Peak Area Data")
  
  if(metafactor == "all_mf"){
    # format meta factor table
    colNM_num <- apply(dt_meta, 2, FUN = function(x){
      length(unique(x))
    })
    colNMs <- colnames(dt_meta)
    metafactors <- colNMs[!((colNM_num == 1) | (colNM_num == nrow(dt_meta)))]
    metafactors_levels <- colNM_num[!((colNM_num == 1) | (colNM_num == nrow(dt_meta)))]
    dt_meta_done <- data.frame(Samples = dt_meta[,1], 
                               dt_meta[,names(metafactors_levels)])
    write.csv(dt_meta_done, file = "metaboanalyst_input_meta.csv", quote = F, row.names = F);
    
    # format data table
    dt_tablex <- dt_table
    
    if(featureID == "NA"){
      dt_done <- dt_tablex
    } else {
      idx_col <- which(!is.na(dt_ids[,featureID]))
      dt_tablex2 <- dt_tablex[,idx_col+1]
      IDvecs <-as.data.frame(dt_ids[,featureID])[,1][idx_col]
      
      if((featureID =="HMDB") | (featureID =="KEGG") | (featureID =="PUBCHEM")){
        IDvecs <- vapply(IDvecs, function(x){
          vc <- strsplit(x, ",")[[1]]
          vc[length(vc)]
        }, FUN.VALUE = character(1L))
      }
      
      colnames(dt_tablex2) <- IDvecs
      dt_done <- data.frame(Samples = dt_tablex[,1],
                            dt_tablex2)
      colnames(dt_done)[-c(1)] <- colnames(dt_tablex2)
    }
    
    write.csv(dt_done, file = "metaboanalyst_input.csv", row.names = F)
    if(.on.public.web){
      return(1)
    } else {
      return(mSetObj)
    }
  }
  
  if(!all(dt_table[,1] == dt_meta$PARENT_SAMPLE_NAME)){
    AddErrMsg("Sample names in 'Peak Area Data' are not matched with the ones in 'Sample Meta Data'!")
    if(.on.public.web){
      return(0)
    } else {
      return(mSetObj)
    }
  }
  GroupsVec <- dt_meta[,metafactor]
  SampleVec <- dt_table[,1]
  dt_tablex <- dt_table[,-1]

  if(featureID == "NA"){
    dt_done <- data.frame(Samples = SampleVec,
                          Groups = GroupsVec,
                          dt_tablex)
  } else {
    idx_col <- which(!is.na(dt_ids[,featureID]))
    dt_tablex2 <- dt_tablex[,idx_col]
    IDvecs <-as.data.frame(dt_ids[,featureID])[,1][idx_col]
    
    if((featureID =="HMDB") | (featureID =="KEGG") | (featureID =="PUBCHEM")){
      IDvecs <- vapply(IDvecs, function(x){
        vc <- strsplit(x, ",")[[1]]
        vc[length(vc)]
      }, FUN.VALUE = character(1L))
    }
    
    colnames(dt_tablex2) <- IDvecs
    dt_done <- data.frame(Samples = SampleVec,
                          Groups = GroupsVec,
                          dt_tablex2)
    colnames(dt_done)[-c(1:2)] <- colnames(dt_tablex2)
  }
  
  write.csv(dt_done, file = "metaboanalyst_input.csv", row.names = F)
  if(.on.public.web){
    return(1)
  } else {
    return(mSetObj)
  }
}

my.validate.metabolon.data <- function(file_path = NULL) {
  if(is.null(file_path)){
    return(0)
  }
  sheetnms <- ReadXLSXsheetsInfo(file_path)
  if(("Chemical Annotation" %in% sheetnms) & 
     ("Sample Meta Data" %in% sheetnms) & 
     ("Peak Area Data" %in% sheetnms)){
    return(1)
  }
}

my.extract.metabolon.metafactors <- function(mSetObj = NA, file_path = NULL){
  dt <- read_excel(file_path, sheet = "Sample Meta Data")
  colNM_num <- apply(dt, 2, FUN = function(x){
    length(unique(x))
  })
  colNMs <- colnames(dt)
  metafactors <- colNMs[!((colNM_num == 1) | (colNM_num == nrow(dt)))]
  metafactors_levels <- colNM_num[!((colNM_num == 1) | (colNM_num == nrow(dt)))]
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$metafactors <- metafactors;
  mSetObj$dataSet$metafactors_levels <- metafactors_levels;
  mSetObj$dataSet$filename <- basename(file_path)
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(metafactors)
  } else {
    return(mSetObj)
  }
}

my.extract.metabolon.compounds <- function(mSetObj = NA, file_path = NULL){
  dt <- read_excel(file_path, sheet = "Chemical Annotation")
  idx <- vapply(colnames(dt), FUN = function(x){
    x %in% c("HMDB", "KEGG", "INCHIKEY","SMILES", "PUBCHEM", "CHEMICAL_NAME")
  }, logical(1L))
  cmpdIDs <- colnames(dt)[idx]
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$cmpdIDs <- cmpdIDs;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(cmpdIDs)
  } else {
    return(mSetObj)
  }
}


ReadXLSXsheetsInfo <- function(file_path){
  sheetsnms <- "";
  require(readxl)
  sheetsnms <- excel_sheets(file_path)
  return(sheetsnms)
}


ValidateMetaFactor2Level <- function(mSetObj = NA, metafactor){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$metafactors -> metafactors;
  mSetObj$dataSet$metafactors_levels -> metafactors_levels;
  
  mlvl <- metafactors_levels[metafactor == metafactors];
  
  if(.on.public.web){
    if(mlvl == 2){
      return(1)
    } else {
      return(0)
    }
  } else {
    return(mSetObj)
  }
}
