##################################################
## R script for ExpressAnalyst
## Description: Gene/Probe/Protein ID Annotation
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

.loadEnrichLib <- function(fun.type, paramSet, analSet){
  
  if(paramSet$data.org == "generic"){
    folderNm <- paramSet$data.idType;
  }else{
    folderNm <- paramSet$data.org;
  }

  my.path <- paste(paramSet$lib.path, folderNm, "/", fun.type, ".rds", sep="");
  
  my.lib <- readRDS(my.path);
  
  if(substr(fun.type, 0, 2)=="go"){
    if(is.null(names(my.lib))){ # some go lib does not give names
      names(my.lib) <- c("link", "term", "sets");
    }
  }
  
  current.geneset <- my.lib$sets;

  #remove empty pathways
  keep.inx <- lapply(current.geneset,length)>0
  current.geneset <- current.geneset[keep.inx]
  my.lib$term <- my.lib$term[keep.inx]
  set.ids<- names(current.geneset); 
  names(set.ids) <- names(current.geneset) <- my.lib$term;
  
  if(substr(fun.type, 0, 2)=="go"){
    names(current.geneset) = firstup(names(current.geneset))
    names(current.geneset) = gsub("-", "_", names(current.geneset))
    names(set.ids) = firstup(names(set.ids));
    names(set.ids) = gsub("-", "_", names(set.ids));
  }
  qs::qsave(current.geneset, "current_geneset.qs");
  res <- list();
  res$current.setlink <- my.lib$link;
  res$current.setids <- set.ids;
  res$current.geneset <- current.geneset;
  return(res);
}

queryGeneDB <- function(table.nm, data.org){
  paramSet <- readSet(paramSet, "paramSet");    
  if(table.nm == "custom" || data.org == "custom"){
    db.map <- qs::qread("anot_table.qs");
  }else{
    require('RSQLite');
    conv.db <- dbConnect(SQLite(), paste(paramSet$sqlite.path, data.org, "_genes.sqlite", sep="")); 
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

##########################################
############# private utility methods #### 
##########################################

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
