#'Convert different gene IDs into entrez IDs for downstream analysis
#'@description Gene ID mapping, gene annotation, compound
#'mapping, KEGG mapping
#'@param q.vec Input the query
#'@param org Input the organism type
#'@param type Input the type of data to annotate 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import RSQLite
#'@export
#'
doGeneIDMapping <- function(q.vec, org, type){

    ## Perfrom online DB getting ----
    filenm <- paste0(org, "_genes.sqlite")
    lib.url <- sqlite.link <- paste0(gene.sqlite.path, filenm);
    lib.download <- FALSE;
    
    if(!file.exists(filenm)){
      lib.download <- TRUE;
    }else{
      time <- file.info(filenm)
      diff_time <- difftime(Sys.time(), time[,"mtime"], unit="days") 
      if(diff_time>30){
        lib.download <- TRUE;
      }
    }
    # Deal with curl issues
    
    if(lib.download){
      tryCatch(
        {
          download.file(lib.url, destfile=filenm, method="auto")
        }, warning = function(w){ print() },
        error = function(e) {
          print("Download unsucceful. Ensure that curl is downloaded on your computer.")
          print("Attempting to re-try download using wget...")
          download.file(lib.url, destfile=filenm, method="wget")
        }
      )
    }
    
    ## SQLite data downloding finished !
    
    con <- .get.sqlite.con(filenm); 

    if(type == "symbol"){
      db.map = dbReadTable(con, "entrez")
      hit.inx <- match(q.vec, db.map[, "symbol"]);
    }else if(type == "entrez"){
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
    
    if(org %in% c("bta", "dre", "gga", "hsa", "mmu", "osa", "rno")){
      entrezs=db.map[hit.inx, "gene_id"];
    }else{
      entrezs=db.map[hit.inx, "symbol"];
    }
    
    rm(db.map, q.vec); gc();
    dbDisconnect(con);
    return(entrezs);
}

#'Perform compound mapping
#'@param cmpd.vec Input compound vector
#'@param q.type Query type
#'@export
doCompoundMapping<-function(cmpd.vec, q.type){
  
  cmpd.map <- .read.metaboanalyst.lib("compound_db.rds");
  
  if(q.type == "name"){
    
    # first find exact match to the common compound names
    hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$name));
    
    # then try to find exact match to synanyms for the remaining unmatched query names one by one
    todo.inx <-which(is.na(hit.inx));
    if(length(todo.inx) > 0){
      # then try to find exact match to synanyms for the remaining unmatched query names one by one
      syn.db <- .read.metaboanalyst.lib("syn_nms.rds")
      syns.list <-  syn.db$syns.list;
      for(i in 1:length(syns.list)){
        syns <-  syns.list[[i]];
        hitInx <- match(tolower(cmpd.vec[todo.inx]), tolower(syns));
        hitPos <- which(!is.na(hitInx));
        if(length(hitPos)>0){
          # record matched ones
          orig.inx<-todo.inx[hitPos];
          hit.inx[orig.inx] <- i;
          # update unmatched list
          todo.inx<-todo.inx[is.na(hitInx)];
        }
        if(length(todo.inx) == 0) break;
      }
    }
    dat <-  cmpd.map[hit.inx, "kegg"];
  }else{
    if(q.type == "hmdb"){
      hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$hmdb));
    }else if(q.type == "kegg"){ 
      hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$kegg));
    }else if(q.type == "pubchem"){ 
      hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$pubchem));
    }else if(q.type == "chebi"){
      hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$chebi));
    }else if(q.type == "reactome"){ 
      hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$reactome));
    }else{
      print("No support is available for this compound database");
      return(0);
    }  
    dat <-  cmpd.map[hit.inx, "kegg"];
  }
  return(dat);
}

#'Perform KEGG to compound name mapping
#'@param kegg.vec Input vector of KEGG compounds
#'@export
doKEGG2NameMapping <- function(kegg.vec){
  cmpd.map <- .read.metaboanalyst.lib("compound_db.rds");
  hit.inx <- match(tolower(kegg.vec), tolower(cmpd.map$kegg));
  nms <- cmpd.map[hit.inx, "name"];
  return(nms);
}
