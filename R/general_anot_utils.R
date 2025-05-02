#'Various functions for mapping b/w names & database identifiers

#'Given a list of compound names or ids, find matched name or ids from selected databases
#'@description Given a list of compound names or ids
#'find matched name or IDs from selected databases
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param q.type Input the query type, "name" for compound names, "hmdb" for HMDB IDs, "kegg" for KEGG IDs, "pubchem"
#'for PubChem CIDs, "chebi" for ChEBI IDs, "metlin" for METLIN IDs, and "hmdb_kegg" for a both KEGG and HMDB IDs.
#'@param hmdb Logical, T to cross reference to HMDB, F to not.
#'@param pubchem Logical, T to cross reference to PubChem, F to not.
#'@param chebi Logical, T to cross reference to CheBI, F to not.
#'@param kegg Logical, T to cross reference to KEGG, F to not.
#'@param metlin Logical, T to cross reference to MetLin, F to not.
#'@param lipid Logical, if features are lipids (T), a different database will be used for
#'compound matching.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CrossReferencing <- function(mSetObj=NA, q.type, hmdb=T, pubchem=T, 
                             chebi=F, kegg=T, metlin=F, lipid=F){

  mSetObj <- .get.mSet(mSetObj);
  
  # record the filter for 8 major databases
  mSetObj$return.cols <- c(hmdb, pubchem, chebi, kegg, metlin);
  mSetObj$lipid.feats <- lipid
  
  # record all the data
  if(!exists("name.map", where = mSetObj)){
    mSetObj$name.map <- list();
  }
  
  # distribute job
  mSetObj$dataSet$q.type <- q.type;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    MetaboliteMappingExact(mSetObj, q.type, lipid);
    mSetObj <- .get.mSet(mSetObj);
  }else{
    mSetObj <- MetaboliteMappingExact(mSetObj, q.type, lipid);
  }
  
  # do some sanity check
  todo.inx <- which(is.na(mSetObj$name.map$hit.inx));
  if(length(mSetObj$name.map$hit.inx) == 0){
    mSetObj$msgSet$nmcheck.msg <- c(0, "No hits found for the given compound ID. Please make 
                                    sure that correct compound IDs or common compound names are used.");
  }else if(length(todo.inx)/length(mSetObj$name.map$hit.inx) > 0.5){
    mSetObj$msgSet$nmcheck.msg <- c(0, "Over half of the compound IDs could not be matched to our database. Please make 
                                    sure that correct compound IDs or common compound names are used.");
  }else if (length(todo.inx) > 15){
    mSetObj$msgSet$nmcheck.msg <- c(2, "There are >15 compounds without matches. You can either proceed or if necessary, update these compound IDs and upload again.");        
  }else{
    mSetObj$msgSet$nmcheck.msg <- c(1, "Name matching OK, please inspect (and manual correct) the results then proceed.");   
  }
  
  if(!.on.public.web){
    print(mSetObj$msgSet$nmcheck.msg)
    
    if(length(todo.inx) == length(mSetObj$name.map$hit.inx)){
      AddErrMsg("Name matching failed! Please make sure that correct standardized feature names are used!")
      return(0)
    }
  }
  
  return(.set.mSet(mSetObj));
}

#'Mapping from different metabolite IDs
#'@description For compound names to other ids, can do exact or approximate matches
#'For other IDs, except HMDB ID, all others may return multiple/non-unique hits
#'Multiple hits or non-unique hits will allow users to manually select
#'@param mSetObj Input the name of the created mSetObj.
#'@param q.type Inpute the query-type, "name" for compound names, "hmdb" for HMDB IDs, "kegg" for KEGG IDs, "pubchem"
#'for PubChem CIDs, "chebi" for ChEBI IDs, "metlin" for METLIN IDs, and "hmdb_kegg" for a both KEGG and HMDB IDs.
#'@param lipid Boolean, if features are lipids, a different database will be used for
#'compound matching.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
MetaboliteMappingExact <- function(mSetObj=NA, q.type, lipid = F){

  mSetObj <- .get.mSet(mSetObj);
  
  if(lipid & anal.type == "msetqea"){
    qvec <- names(mSet$dataSet$url.var.nms);
  }else{
    qvec <- mSetObj$dataSet$cmpd;
  }
  
  # variables to record results
  hit.inx <- vector(mode='numeric', length=length(qvec)); # record hit index, initial 0
  names(hit.inx) <- qvec;
  match.values <- vector(mode='character', length=length(qvec)); # the best matched values (hit names), initial ""
  match.state <- vector(mode='numeric', length=length(qvec));  # match status - 0, no match; 1, exact match; initial 0 
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  if(q.type == "hmdb"){
    n <- 5 # Number of digits for V3 of HMDB
    hmdb.digits <- as.vector(sapply(cmpd.db$hmdb, function(x) strsplit(x, "HMDB", fixed=TRUE)[[1]][2]))
    hmdb.v3.ids <- paste0("HMDB", substr(hmdb.digits, nchar(hmdb.digits)-n+1, nchar(hmdb.digits)))
    hit.inx.v3 <- match(tolower(qvec), tolower(hmdb.v3.ids));
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx[is.na(hit.inx)] <- hit.inx.v3[is.na(hit.inx)]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "pubchem"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$pubchem));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "chebi"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$chebi));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "metlin"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$metlin));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "kegg"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$kegg));
    #hit.inx2 <- match(tolower(qvec), rev(tolower(cmpd.db$kegg)));
    
    # unique hits
    #nonuniq.hits <- hit.inx + hit.inx2 != nrow(cmpd.db) + 1;
    #hit.inx[nonuniq.hits] <- NA;
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }else if(q.type == "name"){
    # first find exact match to the common compound names
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$name));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;

    # then try to find exact match to synonyms for the remaining unmatched query names one by one
    if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
      syn.db <- .get.my.lib("lipid_syn_nms.qs")
    }else if(anal.type == "utils"){
      syn.db <- .get.my.lib("master_syn_nms.qs")
    }else{
      syn.db <- .get.my.lib("syn_nms.qs")
    }
    
    syns.list <-  syn.db$syns.list;
    todo.inx <- which(is.na(hit.inx));
    
    if(length(todo.inx) > 0) {
      for(i in 1:length(syns.list)){
        syns <-  syns.list[[i]];
        hitInx <- match(tolower(qvec[todo.inx]), tolower(syns));
        
        hitPos <- which(!is.na(hitInx));
        if(length(hitPos)>0){
          # record matched ones
          orig.inx<-todo.inx[hitPos];
          hit.inx[orig.inx] <- i;                  
          # match.values[orig.inx] <- syns[hitInx[hitPos]];  # show matched synnames
          match.values[orig.inx] <- cmpd.db$name[i];    # show common name
          match.state[orig.inx] <- 1;
          
          # update unmatched list
          todo.inx<-todo.inx[is.na(hitInx)];
        }
        if(length(todo.inx) == 0) break;
      }
    }
  } else {
    print(paste("Unknown compound ID type:", q.type));save(qvec, file = "qvec__checking.rda");save(cmpd.db, file = "cmpd.db__checking.rda")
    # guess a mix of kegg and hmdb ids

    n <- 5 # Number of digits for V3 of HMDB
    hmdb.digits <- as.vector(sapply(cmpd.db$hmdb, function(x) strsplit(x, "HMDB", fixed=TRUE)[[1]][2]))
    hmdb.v3.ids <- paste0("HMDB", substr(hmdb.digits, nchar(hmdb.digits)-n+1, nchar(hmdb.digits)))
    hit.inx.v3 <- match(tolower(qvec), tolower(hmdb.v3.ids));
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx[is.na(hit.inx)] <- hit.inx.v3[is.na(hit.inx)]

#    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx2 <- match(tolower(qvec), tolower(cmpd.db$kegg));
    nohmdbInx <- is.na(hit.inx);
    hit.inx[nohmdbInx]<-hit.inx2[nohmdbInx]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }
  # empty memory
  gc();
  
  mSetObj$name.map$query.vec <- qvec; 
  mSetObj$name.map$hit.inx <- hit.inx;
  mSetObj$name.map$hit.values <- match.values;
  mSetObj$name.map$match.state <- match.state;
  
  return(.set.mSet(mSetObj));
}

#'Perform compound mapping for integrative analysis methods
#'@description Perform compound mapping
#'@param mSetObj Input name of the created mSet Object
#'@param cmpdIDs Input the list of compound IDs 
#'@param org Input the organism code
#'@param idType Input the ID type
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformCmpdMapping <- function(mSetObj=NA, cmpdIDs, org, idType){
  
  mSetObj <- .get.mSet(mSetObj);

  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
  mSetObj$dataSet$cmpd.id.type <- idType;

  if(idType == "name"){
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "tab");
  }else{ # all other id should not contains space
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "space");
  }
  mSetObj$dataSet$cmpd <- rownames(cmpd.mat); # this is for compatibility with name_match function
  mSetObj$dataSet$cmpd.mat <- cmpd.mat;
  
  if(.on.public.web){
    .set.mSet(mSetObj);  
    return(CrossReferencing(mSetObj, idType, hmdb=T, pubchem=T, chebi=F, kegg=T, metlin=F));
  }
  
  mSetObjCR <- CrossReferencing(mSetObj, idType, hmdb=T, pubchem=T, chebi=F, kegg=T, metlin=F)
  
  return(.set.mSet(mSetObjCR));
}

#'Perform integrated gene mapping
#'@description Used for the pathinteg module
#'@param mSetObj Input name of the created mSet Object
#'@param geneIDs Input the list of gene IDs 
#'@param org Input the organism code
#'@param idType Input the ID type
#'@export
#'
PerformGeneMapping <- function(mSetObj=NA, geneIDs, org, idType){

  mSetObj <- .get.mSet(mSetObj);
  gene.mat <- getDataFromTextArea(geneIDs);
  gene.vec <- rownames(gene.mat);

  #record the info
  mSetObj$dataSet$q.type.gene <- idType;
  mSetObj$dataSet$gene.orig <- geneIDs;
  mSetObj$dataSet$gene.org <- org;
  mSetObj$dataSet$gene.mat <- gene.mat;
  mSetObj$dataSet$gene <- gene.vec;

  if(!(org %in% c("bta", "dre", "gga", "hsa", "mmu", "osa", "rno", "kpn", "kva", "dme", "pfa", "ath", "bsu", "bta", "cdi", "cel", "cjo", "cvr", "dma", "ean", "eco", "fcd", "ham", "nlf", "omk", "osa", "xla"))){
    # for newly added species (including a lot of non-model species. There are no curated gene sqlites
    # therefore, using another direct matching function to match the KEGG entry directly and quickly
    enIDs <- doGeneEntryMapping(gene.vec, org, idType);
  } else if (idType %in% c("ncbi_proteinID", "kegg_embl", "kegg_entry" , "gene_name", "KO")) {
    enIDs <- doGeneEntryMapping(gene.vec, org, idType);
  } else {
    enIDs <- doGeneIDMapping(gene.vec, org, idType);
  }

  if(idType == "kos"){
    kos <- gene.vec;
    mSetObj$dataSet$kos.name.map <- kos
  }
  
  # Handle case when only KOs are mapped with no corresponding entrez id
  na.inx <- is.na(enIDs);
  if(sum(!na.inx) == 0 && idType == "kos"){
    na.inx <- is.na(kos);
  }
  
  mSetObj$dataSet$gene.name.map <- list(
    hit.values=enIDs,
    match.state = ifelse(is.na(enIDs), 0, 1)
  );
  
  AddMsg(paste("A total of ", length(unique(enIDs)), "unique genes were uploaded."));
  if(sum(!na.inx) > 0){
    return(.set.mSet(mSetObj));
  }else{
    AddErrMsg("Error: no hits found!");
    if(.on.public.web){ 
      return(0);
    }
    return(.set.mSet(mSetObj));
  }
}


#'Return the final (after user selection) map as dataframe
#'@description Returns three columns: original name, HMDB name and KEGG ID,
#'for enrichment and pathway analysis, respectively
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param lipid Logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetFinalNameMap <- function(mSetObj=NA, lipid = FALSE){
  
  mSetObj <- .get.mSet(mSetObj);

  lipid = mSetObj$lipid.feats

  if (is.null(lipid)) {
    lipid = FALSE
  }

  hit.inx <- mSetObj$name.map$hit.inx;
  hit.values <- mSetObj$name.map$hit.values;
  match.state <- mSetObj$name.map$match.state;
  
  qvec <- mSetObj$dataSet$cmpd;
  nm.mat <- matrix(nrow=length(qvec), ncol=4);
  colnames(nm.mat) <- c("query", "hmdb",  "kegg", "hmdbid");

  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  for (i in 1:length(qvec)){

    hit <-cmpd.db[hit.inx[i], ,drop=FALSE];
    if(match.state[i]==0){
      hmdb.hit <- NA;
      hmdb.hit.id <- NA;
      kegg.hit <- NA;
    }else{
      hmdb.hit <- ifelse(nchar(hit.values[i])==0, NA, hit.values[i]);
      hmdb.hit.id <- ifelse(nchar(hit$hmdb_id)==0, NA, hit$hmdb_id);
      kegg.hit <- ifelse(nchar(hit$kegg_id)==0, NA, hit$kegg_id);
    }
    nm.mat[i, ]<-c(qvec[i], hmdb.hit, kegg.hit, hmdb.hit.id);
  }
  return(as.data.frame(nm.mat));
}

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
  return(.set.mSet(mSetObj));
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
  if(!file.exists(sqlite.path)){
    #"https://www.xialab.ca/resources/sqlite/hsa_genes.sqlite"
    sqlite_url <- paste0("https://www.xialab.ca/resources/sqlite/", 
                         org, "_genes.sqlite");
    sqlite.path <- paste0(getwd(), "/",org, "_genes.sqlite")
    download.file(sqlite_url,destfile = sqlite.path, method = "curl")
  }
  con <- .get.sqlite.con(sqlite.path); 
  
  if(type == "symbol"){
    db.map = dbReadTable(con, "entrez")
    
    if(org == "dme"){
      q.vec <- paste0("Dmel_", q.vec)
      #print(q.vec)
    }
    
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
    }else if(type == "uniprot"){
      db.map = dbReadTable(con, "entrez_uniprot");
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

doGeneEntryMapping <- function(q.vec, org, type){
  sqlite.path <- paste0(url.pre, "genes_entries_130_species.sqlite");
  if(!file.exists(sqlite.path)){
    #"https://www.xialab.ca/resources/sqlite/hsa_genes.sqlite"
    sqlite_url <- paste0("https://www.xialab.ca/resources/sqlite/genes_entries_130_species.sqlite");
    sqlite.path <- paste0(getwd(), "/","genes_entries_130_species.sqlite")
    download.file(sqlite_url,destfile = sqlite.path, method = "curl")
  }
  con <- .get.sqlite.con(sqlite.path);
  db.map = dbReadTable(con, org);

  #"ncbi_proteinID", "kegg_embl", "kegg_entry" , "gene_name", "KO"
  if(type == "symbol"){ # gene symbol
    hit.inx <- match(q.vec, db.map[, "Gene_Symbol"]);
  } else if(type == "entrez"){ # gene ID
    hit.inx <- match(q.vec, db.map[, "GeneID"]);
  } else if(type == "kegg_embl"){ # ensembel id from KEGG
    hit.inx <- match(q.vec, db.map[, "Ensembl"]);
  } else if(type == "uniprot"){ # Uniprot ID 
    hit.inx <- match(q.vec, db.map[, "UniProtID"]);
  } else if(type == "ncbi_proteinID"){
    hit.inx <- match(q.vec, db.map[, "ProteinID"]);
  } else if(type == "kegg_entry") {
    hit.inx <- match(q.vec, db.map[, "KEGG_entry"]);
  } else if(type == "gene_name") {
    hit.inx <- match(q.vec, db.map[, "Gene_Name"]);
  } else if(type == "KO") {
    hit.inx <- match(q.vec, db.map[, "KO"]);
  } else {
    # unknown ID type
  }

  kegg_entries <- db.map[hit.inx, "KEGG_entry"];
  rm(db.map, q.vec); gc();
  dbDisconnect(con);
  return(kegg_entries);
}

doAllGeneIDMapping <- function(gene.vec, org, idType){

    if(!(org %in% c("bta", "dre", "gga", "hsa", "mmu", "osa", "rno", "kpn", "kva", "dme", "pfa", "ath", "bsu", "bta", "cdi", "cel", "cjo", "cvr", "dma", "ean", "eco", "fcd", "ham", "nlf", "omk", "osa", "xla"))){
      # for newly added species (including a lot of non-model species. There are no curated gene sqlites
      # therefore, using another direct matching function to match the KEGG entry directly and quickly
      entrezs <- doGeneEntryMapping(gene.vec, org, idType);
    } else if (idType %in% c("ncbi_proteinID", "kegg_embl", "kegg_entry" , "gene_name", "KO")) {
      entrezs <- doGeneEntryMapping(gene.vec, org, idType);
    } else {
      entrezs <- doGeneIDMapping(gene.vec, org, idType);
    }
    return(entrezs);
}

convert2KeggEntry <- function(q.vec, type, org){
    sqlite.path <- paste0(url.pre, "genes_entries_130_species.sqlite");
    if(!file.exists(sqlite.path)){
      #"https://www.xialab.ca/resources/sqlite/hsa_genes.sqlite"
      sqlite_url <- paste0("https://www.xialab.ca/resources/sqlite/genes_entries_130_species.sqlite");
      sqlite.path <- paste0(getwd(), "/","genes_entries_130_species.sqlite")
      download.file(sqlite_url,destfile = sqlite.path, method = "curl")
    }
    con <- .get.sqlite.con(sqlite.path);
    db.map = dbReadTable(con, org);

    #"ncbi_proteinID", "kegg_embl", "kegg_entry" , "gene_name", "KO"
    if(type == "symbol"){ # gene symbol
      hit.inx <- match(q.vec, db.map[, "Gene_Symbol"]);
    } else if(type == "entrez"){ # gene ID
      hit.inx <- match(q.vec, db.map[, "GeneID"]);
    } else if(type == "kegg_embl"){ # ensembel id from KEGG
      hit.inx <- match(q.vec, db.map[, "Ensembl"]);
    } else if(type == "uniprot"){ # Uniprot ID 
      hit.inx <- match(q.vec, db.map[, "UniProtID"]);
    } else if(type == "ncbi_proteinID"){
      hit.inx <- match(q.vec, db.map[, "ProteinID"]);
    } else if(type == "kegg_entry") {
      hit.inx <- match(q.vec, db.map[, "KEGG_entry"]);
    } else if(type == "gene_name") {
      hit.inx <- match(q.vec, db.map[, "Gene_Name"]);
    } else if(type == "KO") {
      hit.inx <- match(q.vec, db.map[, "KO"]);
    } else {
      # unknown ID type
    }

    kegg_entries <- db.map[hit.inx, "KEGG_entry"];
    rm(db.map, q.vec); gc();
    dbDisconnect(con);
    return(kegg_entries);
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
