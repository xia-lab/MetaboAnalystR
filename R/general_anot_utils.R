#'Perform various annotation
#'@description Gene ID mapping, gene annotation, compound
#'mapping, KEGG mapping
#'@param q.vec Input the query
#'@param org Input the organism type
#'@param type Input the type of data to annotate 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
doGeneIDMapping <- function(q.vec, org, type){
  
  if(.on.public.web){
    url.pre <- "../../libs/";
  }else{
    url.pre <- "https://www.metaboanalyst.ca/resources/libs/";
  }
  if(type == "symbol"){
    db.path <- paste(url.pre, org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "symbol"]);
    entrezs <- db.map[hit.inx, "gene_id"];
    rm(db.map, q.vec);
    gc();
    return(entrezs);
  }else if(type == "entrez"){
    db.path <- paste(url.pre, org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
    entrezs <- db.map[hit.inx, "gene_id"];
    rm(db.map, q.vec);
    return(entrezs);
  }else if(type == "kos"){
    db.path <- paste(url.pre, "network/ko_dic_new.csv", sep="");
    db.map <-  .readDataTable(db.path);
    #db.map <- db.map[as.integer(db.map[,"Entrez_hsa"]) != -1,]
    hit.inx <- match(q.vec, db.map[, "KO"]);
    entrezs <- as.integer(db.map[hit.inx, "Entrez_hsa"]);
    entrezs[entrezs==-1] <- NA
    kos <- db.map[hit.inx, "KO"];
    result <- list(entrezs = entrezs, kos = kos)
    rm(db.map, q.vec);
    return(result);
  }else{ 
    if(type == "genbank"){
      db.path <- paste(url.pre, org, "/entrez_gb.rda", sep="");
      load(db.path);
    }else if(type == "refseq"){
      db.path <- paste(url.pre, org, "/entrez_refseq.csv", sep="");
      db.map <-  .readDataTable(db.path);
      # note, ref.seq can have version number which is not in the database
      # need to strip it off NM_001402.5 => NM_001402
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
    }else if(type == "embl"){
      db.path <- paste(url.pre, org, "/entrez_embl.csv", sep="");
      db.map <-  .readDataTable(db.path);
    }else{
      print("Unknown data type");
      return(0);
    }
    
    hit.inx <- match(q.vec, db.map[, "accession"]);
    
    entrezs=db.map[hit.inx, "gene_id"];
    rm(db.map, q.vec);
    gc();
    return(entrezs);
  }
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
