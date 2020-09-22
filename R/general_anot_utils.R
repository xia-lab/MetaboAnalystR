

#'Save concentration data
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param conc Input the concentration data
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Setup.ConcData<-function(mSetObj=NA, conc){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$norm <- conc;
  return(.set.mSet(mSetObj));
}

#'Save biofluid type for SSP
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param type Input the biofluid type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
Setup.BiofluidType<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$biofluid <- type;
  return(.set.mSet(mSetObj));
}

#'Set organism for further analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param org Set organism ID
#'@export
SetOrganism <- function(mSetObj=NA, org){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$org <- org;
  return(.set.mSet(mSetObj))
}

GetKEGG.PathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(names(current.kegglib$path.ids));
}

#'Given a vector containing KEGGIDs, returns a vector of KEGG compound names
#'@description This function, given a vector containing KEGGIDs, returns a vector of KEGG compound names.
#'@param ids Vector of KEGG ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2Name<-function(ids){
  cmpd.db <- .get.my.lib("compound_db.qs");
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, 3]);
}

#'Given a vector containing KEGG pathway IDs, return a vector containing SMPDB IDs (only for hsa)
#'@description This function, when given a vector of KEGG pathway IDs, return a vector of SMPDB IDs (only for hsa).
#'SMPDB standing for the Small Molecule Pathway Database, and hsa standing for human serum albumin. 
#'@param ids Vector of KEGG pathway IDs
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGPATHID2SMPDBIDs<-function(ids){
  hit.inx<-match(ids, path.map[,1]);
  return(path.map[hit.inx, 3]);
}

#'Given a vector of HMDBIDs, return a vector of HMDB compound names
#'@description This function, when given a vector of HMDBIDs, return a vector of HMDB compound names. HMDB standing
#'for the Human Metabolome Database. 
#'@param ids Input the vector of HMDB Ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2Name<-function(ids){
  cmpd.db <- .get.my.lib("compound_db.qs");
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "name"]);
}

#'Given a vector of KEGGIDs, return a vector of HMDB ID
#'@description This functionn, when given a vector of KEGGIDs, returns a vector of HMDB IDs. HMDB standing
#'for the Human Metabolome Database. 
#'@param ids Vector of KEGG ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

KEGGID2HMDBID<-function(ids){
  
  cmpd.db <- .get.my.lib("compound_db.qs");
  
  hit.inx<- match(ids, cmpd.db$kegg);
  return(cmpd.db[hit.inx, "hmdb_id"]);
}

#'Given a vector of HMDBIDs, return a vector of KEGG IDs
#'@description This function, when given a vector of HMDBIDs, returns a vector of KEGG ID. HMDB standing
#'for the Human Metabolome Database. 
#'@param ids Input the vector of HMDB Ids
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

HMDBID2KEGGID<-function(ids){
  cmpd.db <- .get.my.lib("compound_db.qs");
  hit.inx<- match(ids, cmpd.db$hmdb);
  return(cmpd.db[hit.inx, "kegg_id"]);
}


#'Convert different gene IDs into entrez IDs for downstream analysis
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

    sqlite.path <- paste0(url.pre, org, "_genes.sqlite");
    con <- .get.sqlite.con(sqlite.path); 

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
  
  cmpd.map <- .get.my.lib("compound_db.qs");
  
  if(q.type == "name"){
    
    # first find exact match to the common compound names
    hit.inx <- match(tolower(cmpd.vec), tolower(cmpd.map$name));
    
    # then try to find exact match to synanyms for the remaining unmatched query names one by one
    todo.inx <-which(is.na(hit.inx));
    if(length(todo.inx) > 0){
      # then try to find exact match to synanyms for the remaining unmatched query names one by one
      syn.db <- .get.my.lib("syn_nms.qs")
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
  cmpd.map <- .get.my.lib("compound_db.qs");
  hit.inx <- match(tolower(kegg.vec), tolower(cmpd.map$kegg));
  nms <- cmpd.map[hit.inx, "name"];
  return(nms);
}
