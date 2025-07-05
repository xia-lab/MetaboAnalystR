##################################################
## R script for MR analysis 
## Description: Data/resource management functions
## Adapted from mGWAS-Explorer
###################################################

SetLDProxy <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ld.proxy <- opt;
  .set.mSet(mSetObj);
}

SetLDR2 <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ld.r2 <- as.numeric(opt);
  .set.mSet(mSetObj);
}

QueryExposure <- function(mSetObj=NA, itemsStr){

    library(dplyr)
    library(tidyr)

    mSetObj <- .get.mSet(mSetObj);
    itemVec <- strsplit(itemsStr, split = ", ")[[1]]

    tableName <- "exposure";
    idType <- "name";
    mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), itemVec, tableName, idType, "all", "all");   
    hit.num <- nrow(mir.dic);
    if (hit.num == 0) {
        current.msg <<- "No hits found in the database. Please make sure to select a metabolite from the drop-down list.";
        print(current.msg);
        return(0);
    } 

    res <- mir.dic[ , c("metabolite_orig","hmdb","kegg","snp_orig", "chr", "pos_hg19","note", "name","ratio_single","beta","p_value","metabolite_id","ea","nea","pmid",
                      "most_severe_consequence", "eaf","link","se", "pop_code", "biofluid")];
  
    res <- .parse_snp2met_exposure(res); # remove NA
    # update col names
  res <- res %>%
  group_by(across(-p_value)) %>%
  slice_min(p_value, with_ties = FALSE) %>%
  ungroup()# remove duplicates introduce by mistake notes
  res = as.data.frame(res)
 
    colnames(res) <- c("Metabolite","HMDB","KEGG","SNP", "Chr", "BP","Note","Common Name", "Single or Ratio","Beta", "P-value", "MetID", "A1", "A2", "PMID",
                     "Consequence", "EAF","URL", "SE", "pop_code", "biofluid");
    fast.write.csv(res, file="mr_exposure_data.csv", row.names=FALSE);  

    display.res <- res;
    met.nms <<- res[,"Common Name"];
    snp.nms <<- res[, "SNP"];
    res <- data.frame(Name1=res[,"SNP"], ID1=res[,"SNP"], Name2=res[,"Common Name"],ID2=res[,"KEGG"], Reference=res[,"PMID"], P_value=res[,"P-value"], stringsAsFactors = FALSE);
    mir.resu <- res;
    exposure <- display.res;
    mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(met.nms)),stringsAsFactors = FALSE);
    mirtableu <-  "exposure";

    ## get associated metabolites for each snp
    mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), snp.nms, "snp2met", "rsid", "all", "all");

    res <- mir.dic[, c("rsid","name","symbol","entrez")];
     
    # Create summary tables for metabolites and genes
    summary_table <- res %>%
      group_by(rsid) %>%
      summarise(
        metabolites = paste(unique(name), collapse = ", "),
        genes = paste(unique(symbol), collapse = ", "),
        gene_id = paste(unique(entrez), collapse = ", "),
      ) %>%
      ungroup()

    # Rename column for merging
    colnames(summary_table)[1] <- "SNP"

    # Merge with exposure data
    merged_table <- merge(exposure, summary_table, by = "SNP", all = TRUE)

    # Number of columns in the data frame
    num_cols <- ncol(merged_table)

    # Create a sequence of column indices with the first column moved to the fourth position
    # Adjust this as needed for your specific column arrangement
    new_order <- c(2:3, 1, 4:num_cols)

    # Reorder the columns
    merged_table <- merged_table[, new_order]

    mSetObj$dataSet$mir.res <- mir.resu;
    mSetObj$dataSet$exposure <- merged_table;
    mSetObj$dataSet$exposure.orig <- merged_table;
    #mSetObj$dataSet$mirtarget <- mirtargetu;
    mSetObj$dataSet$mirtable <- unique(mirtableu);

    if(.on.public.web){
        return(.set.mSet(mSetObj));
    }else{
        return(current.msg);
    }
}

QueryOutcome <- function(itemVec){
     
    if (file.exists("dis_snp_restable.csv")) {
        return(1);
    }

    mSetObj <- .get.mSet(mSetObj);
    itemVec.id <- trimws(itemVec);

    ieugwas.db <- .get.my.lib("ieugwas_202210.qs");
    ieugwas.res <- ieugwas.db[ieugwas.db$id == itemVec.id,];
    hit.num <- nrow(ieugwas.res);
    if (hit.num == 0) {
        current.msg <<- "No hits found in the database. Please make sure to select an outcome from the drop-down list.";
        print(current.msg);
        return(0);
    } 

    mSetObj$dataSet$outcome <- ieugwas.res;
    fast.write.csv(ieugwas.res, file="dis_snp_restable.csv");
    if(.on.public.web){
        return(.set.mSet(mSetObj));
    }else{
        return(current.msg);
    }  
}

.query_mr_results <- function(mir.dic, resOpt){
  res <- mir.dic[ , c("exposure", "outcome_nm", "nsnp", "method", "b", "se", "pval")];
  res$b <- signif(res$b, digits = 5);
  res$se <- signif(res$se, digits = 5);
  res$pval <- signif(res$pval, digits = 5)
  
  res <- res[order(res$pval),];
  # update col names
  colnames(res) <- c("Metabolite","Trait", "N SNP", "Method", "Effect Size", "S.E.", "P-value");
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_results <<- display.res;
  mirtableu <<-  "mr_results";
}

.query_mr_sensitivity <- function(mir.dic, resOpt){
  mir.dic$correct_causal_direction <- ifelse(mir.dic$correct_causal_direction == 1, "Yes", "No")
  res <- mir.dic;
  res$steiger_pval <- signif(res$steiger_pval, digits = 5);
  res$Q_pval <- signif(res$Q_pval, digits = 5);
  res$pval <- signif(res$pval, digits = 5)
  
  # update col names
  colnames(res) <- c("Metabolite","Trait", "Directionality", "Steiger P-value", "Heterogeneity Method", 
                     "Heterogeneity P-value", "Pleiotropy P-value");
  
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_sensitivity <<- display.res;
  mirtableu <<-  "mr_sensitivity";
}

.query_mr_single <- function(mir.dic, resOpt){
  res <- mir.dic[ , c("exposure", "outcome_nm", "SNP", "b", "se", "p")];
  res$b <- signif(res$b, digits = 5);
  res$se <- signif(res$se, digits = 5);
  res$p <- signif(res$p, digits = 5)
  
  res$method <- "Wald ratio";
  res <- res[order(res$p),]
  # update col names
  colnames(res) <- c("Metabolite","Trait", "SNP",  "Effect Size", "S.E.", "P-value","Method");
  res <- res[,c("Metabolite","Trait", "SNP", "Method", "Effect Size", "S.E.", "P-value")]
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_single <<- display.res;
  mirtableu <<-  "mr_single";

}


PrepareMgwasCSV <- function(table.nm){
  # table.nm<<-table.nm;
  # save.image("PrepareCSV.RData")
  mSetObj <- .get.mSet(mSetObj);
  if(table.nm=="mr_res_single"){
    fast.write.csv(mSetObj$dataSet$mr_res_single, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res_loo"){
    fast.write.csv(mSetObj$dataSet$mr_res_loo, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res"){
    fast.write.csv(mSetObj$dataSet$mr_results, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="manhattan"){
    fast.write.csv(mSetObj$dataSet$snp2met, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else if(anal.type == "multilist" || anal.type == "snp2mir" || anal.type == "tf2genemir" || anal.type == "gene2tfmir"){
    fast.write.csv(mSetObj$dataSet[[table.nm]], file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else {
    fast.write.csv(mSetObj$dataSet$snp.res, file=paste(net.type, ".csv", sep=""), row.names = FALSE);
  }
}

SetupListData <- function(mSetObj=NA, mirs, idType, tissue, population){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$idType <- idType;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;

  mir.mat <- mir.mat[,-1, drop=F];
  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

SetupIndListData <- function(mSetObj=NA, mirs, inputType, idType, tissue, population){

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;
  
  mir.mat <- mir.mat[,-1, drop=F];
  if(is.null(mSetObj$dataSet$data)){
    mSetObj$dataSet$data <- mSetObj$dataSet$id.types <- list();
  }
  mSetObj$dataSet$data[[inputType]] <- mir.mat;
  mSetObj$dataSet$id.types[[inputType]] <- idType;

  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

# "ID", "Accession","Gene", "PMID"
GetResCol <- function(netType, colInx){
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;
  dataSet <- mSetObj$dataSet;
  res <- dataSet[netType][[1]][, colInx];
  hit.inx <- is.na(res) | res == ""; # note, must use | for element-wise operation
  res[hit.inx] <- "N/A";
  return(res);
}

# "ID", "Accession","Gene", "PMID"
GetResColByName <- function(netType, name){
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;
  dataSet <- mSetObj$dataSet;

  df <-dataSet[netType][[1]];
  colInx <- which(colnames(df) == name);
  res <- df[, colInx];

  hit.inx <- is.na(res) | res == ""; # note, must use | for element-wise operation
  res[hit.inx] <- "N/A";
  return(res);
}

RemoveEntryExposure <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$exposure) == mir.id);
  if(length(inx) > 0){
    if(is.null(mSetObj$dataSet$exposure.orig)){
        mSetObj$dataSet$exposure.orig <- mSetObj$dataSet$exposure;
    }
    mSetObj$dataSet$exposure <- dataSet$exposure[-inx,];
    if(!is.null(mSetObj$dataSet$harmonized.dat)){
        inx <- which(rownames(mSetObj$dataSet$harmonized.dat) %in% rownames(mSetObj$dataSet$exposure));
        mSetObj$dataSet$harmonized.dat <- mSetObj$dataSet$harmonized.dat[inx,];
    }
  }

  return(.set.mSet(mSetObj));
}


RemoveEntriesExposure <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  if(!exists("entries.vec")){
    return(0);
  }
 
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$exposure) %in% entries.vec);
  if(length(inx) > 0){
    if(is.null(mSetObj$dataSet$exposure.orig)){
        mSetObj$dataSet$exposure.orig <- mSetObj$dataSet$exposure;
    }
    mSetObj$dataSet$exposure <- dataSet$exposure[-inx,];
    if(!is.null(mSetObj$dataSet$harmonized.dat)){
        inx <- which(rownames(mSetObj$dataSet$harmonized.dat) %in% rownames(mSetObj$dataSet$exposure));
        mSetObj$dataSet$harmonized.dat <- mSetObj$dataSet$harmonized.dat[inx,];
    }
  }

  return(.set.mSet(mSetObj));
}



AddEntryExposure <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$exposure.orig) == mir.id);
  if(length(inx) > 0){
    mSetObj$dataSet$exposure <- rbind(dataSet$exposure, dataSet$exposure.orig[inx,]);
  }
  return(.set.mSet(mSetObj));
}


GetResRowNames <- function(netType){
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;

  #if(netType == "manhattan" || analSet == "studyview"){
  #  resTable <- mSetObj$dataSet$snp2met_study; 
  #}else if (analSet == "search"){
  #  resTable <- mSetObj$dataSet$snp2met_single;
  #}else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
    resTable <- mSetObj$dataSet[netType][[1]]
  #} else{
  #  resTable <- mSetObj$dataSet$mir.res;
  #}

  if(nrow(resTable) > 1000 & netType != "phe_mr_sig"){
    resTable <- resTable[1:1000, ];
    current.msg <<- "Due to computational constraints, only the top 1000 rows will be displayed.";
  }
  rownames(resTable);
}


# Load RSQLite, necessary for network analysis
load_rsqlite <- function(){
  suppressMessages(library(RSQLite))
}

.get.sqlite.con <- function(sqlite.path){
  load_rsqlite();
  return(dbConnect(SQLite(), sqlite.path)); 
}

# private method for all sqlite queries
.query.sqlite <- function(db.con, statement, offline=TRUE){
  rs <- dbSendQuery(db.con, statement);
  res <- fetch(rs, n=-1); # get all records
  dbClearResult(rs);
  if(offline){
    dbDisconnect(db.con);
  }
  cleanMem();
  return(res);
}

doGeneIDMapping <- function(q.vec, org, type){
  #save.image("doGeneIDMapping.RData")
  
  sqlite.path <- paste0(url.pre, org, "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  
  if(type == "symbol"){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "symbol"]);
  }else if(type == "entrez" || type == "entrez2sbl" ){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
  }else{
    if(type == "genbank"){
      db.map = dbReadTable(con, "entrez_gb");
    }else if(type == "embl"){
      db.map = dbReadTable(con, "entrez_embl_gene");
    }else if(type == "refseq"){
      db.map = dbReadTable(con, "entrez_refseq");
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
    }else if(type == "kos"){
      db.map = dbReadTable(con, "entrez_ortholog");
    }
    hit.inx <- match(q.vec, db.map[, "accession"]);
  }
  
  if(type=="entrez2sbl"){
    entrezs=db.map[hit.inx, "symbol"];
  }else{
    entrezs=db.map[hit.inx, "gene_id"];
  }
  
  rm(db.map, q.vec); gc();
  dbDisconnect(con);
  return(entrezs);
}

addEntrezID <- function(data){
  #save.image("addEntrezID.RData")
  
  sqlite.path <- paste0(url.pre, "hsa", "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  embl <- data;
  
  db.map = dbReadTable(con, "entrez_embl_gene");
  all.hits <- db.map$accession %in% embl$ensembl;
  my.map <- db.map[all.hits, ];
  dat <- merge(my.map, data, by.x="accession", by.y="ensembl");

    rm(db.map, embl); gc();
  dbDisconnect(con);
  return(dat);
}


PerformCmpdMapping2 <- function(mSetObj=NA, cmpdIDs, idType, tissueType, population){
  
  mSetObj <- .get.mSet(mSetObj);
  org <- "hsa";
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
  mSetObj$dataSet$tissue <- tissueType;
  mSetObj$dataSet$population <- population;
  mSetObj$dataSet$idType <- idType;
  current.msg <<- NULL;  
  if(idType == "name"){
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "tab");
  }else{ # all other id should not contains space
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "space");
  }
  mSetObj$dataSet$cmpd <- rownames(cmpd.mat); # this is for compatibility with name_match function
  mSetObj$dataSet$cmpd.mat <- cmpd.mat;
  mSetObj$dataSet$mir.orig <- cmpd.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(cmpd.mat));
  }else{
    return (paste("A total of",  nrow(cmpd.mat), "unique items were entered."))
  }
}

SetMappingType <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mapType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Mapping options were entered!"))
  }
}

SetDbOpt <- function(){
  #save.image("SetDbOpt.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$dbOpt <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Database options were entered!"))
  }
}


SetMRMethod <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$methodType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("MR method options were entered!"))
  }
}




GetPathRowNames <- function(netType){
  mSetObj <- .get.mSet(mSetObj);
  path <- mSetObj$dataSet$path;
 if(nrow(path)==0){
   return()
  }else{
  return(1:nrow(path))
}
}

 
GetPathCol <- function(colInx){
 
  mSetObj <- .get.mSet(mSetObj);
  path <- mSetObj$dataSet$path;
  if(nrow(path)==0){
   return()
  }else{
    return(path[,colInx]);
   }
 
}