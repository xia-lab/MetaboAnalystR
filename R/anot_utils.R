##################################################
## R script for ExpressAnalyst
## Description: Gene/Probe/Protein ID Annotation
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

queryGeneDB <- function(table.nm, data.org){
  paramSet <- readSet(paramSet, "paramSet");    
  if(table.nm == "custom" || data.org == "custom"){
    db.map <- qs::qread("anot_table.qs");
  }else{
    require('RSQLite');
    db.path <- paste(paramSet$sqlite.path, data.org, "_genes.sqlite", sep="")

    if(!PrepareSqliteDB(db.path, paramSet$on.public.web)){
      stop("Sqlite database is missing, please check your internet connection!");
    }
    conv.db <- dbConnect(SQLite(), db.path); 
    tbls <- dbListTables(conv.db)
    if(!table.nm %in% tbls){
        return(0);
    }
    db.map <- dbReadTable(conv.db, table.nm);
    dbDisconnect(conv.db); cleanMem();
  }
  
  return(db.map)
}

getEntrezTableName <- function(data.org, data.idType){
    if(data.org == "generic"){
        tblNm <- paste0("entrez_", data.idType);
    }else{
        tblNm <- "entrez";
    }
    return(tblNm);
}


doEntrez2SymbolMapping <- function(entrez.vec,data.org="hsa", data.idType="entrez"){
  tblNm <- getEntrezTableName(data.org, data.idType);
  gene.map <-  queryGeneDB(tblNm, data.org);
  gene.map[] <- lapply(gene.map, as.character)
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  symbols <- gene.map[hit.inx, "symbol"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
}

# note, entrez.vec could contain NA/null, cannot use rownames
doEntrezIDAnot <- function(entrez.vec,data.org="hsa", data.idType){
  tblNm <- getEntrezTableName(data.org, data.idType);
  gene.map <-  queryGeneDB(tblNm, data.org);
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  anot.mat <- gene.map[hit.inx, c("gene_id", "symbol", "name")];
  
  na.inx <- is.na(hit.inx);
  anot.mat[na.inx, "symbol"] <- entrez.vec[na.inx];
  anot.mat[na.inx, "name"] <- 'NA';
  return(anot.mat);
}

doIdMappingGeneric <- function(orig.vec, gene.map, colNm1, colNm2){
  
  hit.inx <- match(orig.vec, gene.map[, colNm1]);
  result.vec <- gene.map[hit.inx, colNm2];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(result.vec);
  result.vec[na.inx] <- orig.vec[na.inx];
  return(result.vec);
}

##########################################
############# private utility methods #### 
##########################################

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
