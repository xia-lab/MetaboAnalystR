##################################################
## R script for MetaboAnalyst
## Description: Gene/Probe/Protein ID Annotation
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

doAnnotation <- function(id.vec, idType){
  feature.vec <- id.vec;
  if(idType %in% c("entrez", "symbol", "refseq", "genbank", "emblgene","emblprotein", "embltranscript", "orfid")){
    anot.id <- doGeneIDMapping(feature.vec, idType);
  }else{
    anot.id <- doProbeMapping(feature.vec, idType);
  }   
  names(anot.id) <- id.vec;
  return(anot.id);        
}

PerformGeneAnnotation <- function(){
  if(!exists("entrez.vec")){
    print("Could not find Entrez ID list!");
    return(0);
  }
  
  db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
  gene.map <- readRDS(db.path);
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  dat <- cbind(query=entrez.vec, gene.map[hit.inx, c("symbol","name")]);
  write.csv(dat, file="EntrezID2Gene.csv", row.names=F);
  rm(entrez.vec, envir = .GlobalEnv);
  return(1);
}

# from probe ID to entrez ID 
doProbeMapping <- function(probe.vec, platform){
  platform.path <- paste(lib.path,  data.org, "/", platform, ".rds", sep="");
  probe.map <- readRDS(platform.path);
  if(is.null(probe.vec)){
    entrez <- probe.map[, "entrez"];
  }else{
    hit.inx <- match(probe.vec, probe.map[, "probe"]);
    entrez <- probe.map[hit.inx, "entrez"];
  }
  return(entrez);
}

# mapping between genebank, refseq and entrez
doGeneIDMapping <- function(q.vec, type){
  if(is.null(q.vec)){
    db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    q.vec <- db.map[, "gene_id"];
    type = "entrez";
  }
  
  if(type == "symbol"){
    db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "symbol"]);
  }else if(type == "entrez"){
    db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
  }else{
    # note, some ID can have version number which is not in the database
    # need to strip it off NM_001402.5 => NM_001402
    q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
    q.vec <- q.mat[,1];
    if(type == "genbank"){
      db.path <- paste(lib.path, data.org, "/entrez_gb.rds", sep="");
    }else if(type == "refseq"){
      db.path <- paste(lib.path, data.org, "/entrez_refseq.rds", sep="");
    }else if(type == "emblgene"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_gene.rds", sep="");
    }else if(type == "embltranscript"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_transcript.rds", sep="");
    }else if(type == "emblprotein"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_protein.rds", sep="");
    }else if(type == "orfid"){ # only for yeast
      db.path <- paste(lib.path, data.org, "/entrez_orf.rds", sep="");
    }else{
      print("Unknown data type");
      return(0);
    }
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "accession"]);
  }
  entrezs=db.map[hit.inx, "gene_id"];
  mode(entrezs) <- "character";
  rm(db.map, q.vec); gc();
  return(entrezs);
}

doEntrez2SymbolMapping <- function(entrez.vec){
  db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
  gene.map <- readRDS(db.path);
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  symbols <- gene.map[hit.inx, "symbol"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
}

# note, entrez.vec could contain NA/null, cannot use rownames
doEntrezIDAnot <- function(entrez.vec){
  db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
  gene.map <- readRDS(db.path);
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  anot.mat <- gene.map[hit.inx, c("gene_id", "symbol", "name")];
  
  na.inx <- is.na(hit.inx);
  anot.mat[na.inx, "symbol"] <- entrez.vec[na.inx];
  anot.mat[na.inx, "name"] <- 'NA';
  return(anot.mat);
}


doProteinIDMapping <- function(q.vec, type){
  if(type == "entrez"){
    # need to get only our data
    db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
    entrezs <- db.map[hit.inx, ]
    entrezs <- entrezs[c(1,1)]
    colnames(entrezs) = c("gene_id", "accession");
  }else if(type == "symbol"){
    db.path <- paste(lib.path, data.org, "/entrez.rds", sep="");
    gene.map <- readRDS(db.path);
    hit.inx <- match(q.vec, gene.map[, "symbol"]);
    entrezs <- gene.map[hit.inx, ];
    entrezs = entrezs[c(1,2)];
    colnames(entrezs) <- c("gene_id", "accession")     
  }else{
    if(type == "genbank"){
      # note, some ID can have version number which is not in the database
      # need to strip it off NM_001402.5 => NM_001402
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
      db.path <- paste(lib.path, data.org, "/entrez_gb.rds", sep="");
    }else if(type == "refseq"){
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
      db.path <- paste(lib.path, data.org, "/entrez_refseq.rds", sep="");
    }else if(type == "emblgene"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_gene.rds", sep="");
    }else if(type == "tair"){
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
      db.path <- paste(lib.path, data.org, "/entrez_embl_transcript.rds", sep="");
    }else if(type == "embltranscript"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_transcript.rds", sep="");
    }else if(type == "emblprotein"){
      db.path <- paste(lib.path, data.org, "/entrez_embl_protein.rds", sep="");
    }else if(type == "orfid"){ # only for yeast
      db.path <- paste(lib.path, data.org, "/entrez_orf.rds", sep="");
    }else if(type == "flybase"){
      db.path <- paste(lib.path, data.org, "/entrez_flybase.rds", sep="");
    }else if(type == "string"){ 
      db.path <- paste(lib.path, data.org, "/entrez_string.rds", sep="")
    }else if(type == "ecogene"){ # only for ecoli
      db.path <- paste(lib.path, data.org, "/entrez_ecogene.rds", sep="")
    }else if(type == "uniprot"){
      db.path <- paste(lib.path, data.org, "/entrez_uniprot.rds", sep="")
    }else{
      print("Unknown data type");
      return(0);
    }
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "accession"]);
    entrezs <- db.map[hit.inx, ];
  }
  entrezs = entrezs[c(2,1)];
  colnames(entrezs) <- c("accession", "gene_id")
  return(entrezs);
}

doEmblGene2EntrezMapping <- function(emblgene.vec){
  db.path <- paste(lib.path, data.org, "/entrez_embl_gene.rds", sep="");
  db.map <-  readRDS(db.path);
  hit.inx <- match(emblgene.vec, db.map[, "accession"]);
  entrezs <- db.map[hit.inx, "gene_id"];
  mode(entrezs) <- "character";
  return(entrezs);
}

