
#'Performs KO enrichment analysis based on the KO01100 map
#'@description This function performs KO enrichment analysis based on the KO01100 map
#'and saves the .JSON file
#'@param mSetObj Input name of the created mSet Object
#'@param category Input the option to perform enrichment analysis, "pathway"
#'@param file.nm Input name of file to save
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PerformKOEnrichAnalysis_KO01100 <- function(mSetObj=NA, category, file.nm){
  
  mSetObj <- .get.mSet(mSetObj);
  LoadKEGGKO_lib(category);
  #if(enrich.type == "hyper"){ else PerformKOEnrichAnalysis_Table
  PerformKOEnrichAnalysis_List(file.nm);
  
  if(.on.public.web == FALSE){
    return(.set.mSet(mSetObj)); 
  }
}

#'Perform mapping of user's data to interaction network
#'@description This function performs mapping of user's data to the internal network
#' to create a network from the seed nodes
#'@param mSetObj Input name of the created mSet Object
#'@param db.type Input the database type
#'@param table.nm Input the organism code for the sqlite table (ppi). For chemical type, the 
#'table.nm is drugbank of ctd
#'@param require.exp Logical, only used for the STRING database
#'@param min.score Input the minimal score, only used for the STRING database 
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import RSQLite
SearchNetDB <- function(mSetObj=NA, db.type, table.nm, require.exp=TRUE, min.score = 900){ 

    mSetObj <- .get.mSet(mSetObj);
  
    if(.on.public.web){
       load_rsqlite()
    }    

    result.list <- .preparePhenoListSeeds(mSetObj, table.nm);
    genes <- result.list$genes;
    protein.vec <- seed.graph;
    cmpds <- result.list$cmpds;

    network.type <<- table.nm 

    # now do the database search
    
    if(db.type == "pheno"){
        res <- QueryPhenoSQLite(table.nm, genes, cmpds, min.score);
        if(nrow(res)==0){ return(c(0,0)); }

        if(table.nm == "gene_metabolites"){
            src <- "entrez"; src.nm <- "symbol";
            src.evidence <- "protein"
            target <- "ctdid"; target.nm <- "name";
            target.evidence <- "stitch"
        } else if(table.nm == "metabo_phenotypes"){
            src <- "ctdid"; src.nm <- "name";
            target <- "omimid"; target.nm <- "phenoname";
            evidence <- "pmid"
        } else if(table.nm == "metabo_metabolites"){
            src <- "ctdid1"; src.nm <- "name1";
            src.evidence <- "stitch1"
            target <- "ctdid2"; target.nm <- "name2";
            target.evidence <- "stitch2"
        } else if(table.nm == "gene_phenotypes"){
            src <- "entrez"; src.nm <- "symbol";
            target <- "omimid"; target.nm <- "phenoname";
        } else if(table.nm == "global"){
          src <- "id1"; src.nm <- "name1";
          src.evidence <- "evidsrc"
          target <- "id2"; target.nm <- "name2";
          target.evidence <- "evidtar"
        }

        if(table.nm == "metabo_phenotypes"){
          edge.res <- data.frame(Source=res[,src],Target=res[,target], Evidence=res[,evidence]);
        } else {
          edge.res <- data.frame(Source=res[,src],Target=res[,target]);
        }
        row.names(edge.res) <- 1:nrow(res);
        write.csv(edge.res, file="orig_edge_list.csv",row.names=FALSE);
    
        node.ids <- c(res[,src], res[,target])
        node.nms <- c(res[,src.nm], res[,target.nm]);
        
        if(table.nm == "metabo_metabolites" || table.nm == "gene_metabolites" || table.nm == "global"){
          node.evidence <- c(res[,src.evidence], res[,target.evidence]);
        } else{
          node.evidence <- ""
        }
    }
    
    # Retrieve gene full names
    genes.names.idx <- match(node.ids, mSetObj$dataSet$gene.map.table[,"Entrez"])
    genes.names <- mSetObj$dataSet$gene.map.table[genes.names.idx,"Name"]

    if(node.evidence != ""){
      # Evidence is related to the STITCH database accessions for chemicals/proteins
      node.res <- data.frame(Id=node.ids, Label=node.nms, GeneNames=genes.names, Evidence=node.evidence);
    } else{
      node.res <- data.frame(Id=node.ids, Label=node.nms, GeneNames=genes.names);
    }
    node.res <- node.res[!duplicated(node.res$Id),];
    nodeListu <<- node.res
    write.csv(node.res, file="orig_node_list.csv", row.names=FALSE);

    pheno.net <<- list(
                    db.type=db.type,
                    order=1, 
                    seeds=protein.vec, 
                    table.nm=table.nm, 
                    node.data = node.res, 
                    edge.data = edge.res,
                    require.exp = require.exp,
                    min.score = min.score
                );

    if(.on.public.web){
      return(c(nrow(node.res), nrow(res)));
    }else{
      return(.set.mSet(mSetObj));
    }
}

# Utility function for SearchNetDB
# table name is org code, id.type is column name
#'@import RSQLite
QueryPhenoSQLite <- function(table.nm, genes, cmpds, min.score){
  
  if(.on.public.web){
    load_rsqlite()
    pheno.db <- dbConnect(SQLite(), "../../libs/network/MetPriCNet.sqlite");
  }else{
    download.file("https://www.metaboanalyst.ca/resources/libs/network/MetPriCNet.sqlite", "MetPriCNet.sqlite")
    pheno.db <- dbConnect(SQLite(), "MetPriCNet.sqlite");
  }
  
  if(table.nm == "global"){
    # Handle gene_metabolites
    table.nm <- "gene_metabolites";
    if((length(genes) > 0) && (length(cmpds) > 0)){
      genes.query <- paste(shQuote(genes),collapse=",");
      cmpds.query <- paste(shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(length(genes) > 0){
      genes.query <- paste(shQuote(genes),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
    } else{
      cmpds.query <- paste(shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    }
    phenotable <- dbSendQuery(pheno.db, statement);
    genemetab.res <- fetch(phenotable, n=-1); # get all records
    genemetab.res <- genemetab.res[,1:6]
    names(genemetab.res) <- c("id1", "id2", "name1", "name2", "evidsrc", "evidtar")
    
    # Handle metab_phenotypes
    table.nm <- "metabo_phenotypes";
    cmpds.query <- paste(shQuote(cmpds),collapse=",");
    statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");      
    phenotable <- dbSendQuery(pheno.db, statement);
    metabpheno.res <- fetch(phenotable, n=-1); # get all records
    evidsrc <- genemetab.res[match(metabpheno.res[,1], genemetab.res[,2]), 6]
    metabpheno.res <- cbind(metabpheno.res[,1:4], evidsrc, metabpheno.res[,7])
    names(metabpheno.res) <- c("id1", "id2", "name1", "name2", "evidsrc", "evidtar")
    
    # Handle genes_phenotypes
    table.nm <- "gene_phenotypes";
    genes.query <- paste(shQuote(genes),collapse=",");
    statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");      
    phenotable <- dbSendQuery(pheno.db, statement);
    genespheno.res <- fetch(phenotable, n=-1); # get all records
    genespheno.res <- cbind(genespheno.res[,1:4], rep(NA, nrow(genespheno.res)), rep(NA, nrow(genespheno.res)))
    names(genespheno.res) <- c("id1", "id2", "name1", "name2", "evidsrc", "evidtar")
    dbDisconnect(pheno.db);
    
    # Combine all
    pheno.dic <- rbind(genemetab.res, metabpheno.res, genespheno.res)
    
  } else{
    if(table.nm == "gene_metabolites"){
      if((length(genes) > 0) && (length(cmpds) > 0)){
        genes.query <- paste(shQuote(genes),collapse=",");
        cmpds.query <- paste(shQuote(cmpds),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
      } else if(length(genes) > 0){
        genes.query <- paste(shQuote(genes),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
      } else{
        cmpds.query <- paste(shQuote(cmpds),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
      }
    } else if(table.nm == "metabo_phenotypes"){
      cmpds.query <- paste(shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(table.nm == "metabo_metabolites"){
      cmpds.query <- paste(shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid1 IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(table.nm == "gene_phenotypes"){
      genes.query <- paste(shQuote(genes),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
    }
    
    phenotable <- dbSendQuery(pheno.db, statement);
    pheno.dic <- fetch(phenotable, n=-1); # get all records
    dbDisconnect(pheno.db);
  }
  return(pheno.dic);
}

#'Utility function for PerformKOEnrichAnalysis_KO01100
#'@description Please note: only return hits in map KO01100
#'@param file.nm Input the file name 
PerformKOEnrichAnalysis_List <- function(file.nm){
  
  if(idtype == "cmpd"){
    current.set <- current.cmpd.set;
  } else if(idtype == "gene&cmpd"){
    matchidx <- match(names(current.cmpd.set), names(current.geneset))
    current.set <- list()
    # TO-DO: Fix code to handle case if length(current.cmpd.set) > length(current.geneset).
    #   Then, start loop with current.cmpd.set
    for(i in c(1:length(current.geneset))){
      if(i %in% matchidx){
        cidx <- which(matchidx==i) 
        mergels <- c(current.cmpd.set[cidx][[1]], current.geneset[i][[1]])
        current.set[[names(current.cmpd.set[cidx])]] <- mergels
      } else{
        current.set[[names(current.geneset[i])]] <- current.geneset[i][[1]]
      }
    }
    # Add compound sets that did not match
    cidx <- which(is.na(matchidx))
    for(i in c(1:length(cidx))){
      current.set[[names(current.cmpd.set[cidx[i]])]] <- current.cmpd.set[cidx[i]][[1]]
    }
  } else{
    current.set <- current.geneset;
    
  }
  current.universe <- unique(unlist(current.set));
  
  # prepare for the result table
  set.size<-length(current.set);
  res.mat<-matrix(0, nrow=set.size, ncol=5);
  rownames(res.mat)<-names(current.set);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "Pval", "FDR");
  
  # prepare query
  ora.vec <- NULL;
  exp.vec <- dataSet$data[,1]; # drop dim for json
  ora.vec <- names(exp.vec);
  
  # need to cut to the universe covered by the pathways, not all genes 
  hits.inx <- ora.vec %in% current.universe;
  ora.vec <- ora.vec[hits.inx];
  #ora.nms <- ora.nms[hits.inx];
  
  q.size<-length(ora.vec);
  
  # get the matched query for each pathway
  hits.query <- lapply(current.set, 
                       function(x) {
                         ora.vec[ora.vec%in%unlist(x)];
                       }
  );
  names(hits.query) <- names(current.set);
  
  hit.num<-unlist(lapply(hits.query, function(x){length(x)}), use.names=FALSE);
  
  # total unique gene number
  uniq.count <- length(current.universe);
  
  # unique gene count in each pathway
  set.size <- unlist(lapply(current.set, length));
  
  res.mat[,1]<-set.size;
  res.mat[,2]<-q.size*(set.size/uniq.count);
  res.mat[,3]<-hit.num;
  
  # use lower.tail = F for P(X>x)
  raw.pvals <- phyper(hit.num-1, set.size, uniq.count-set.size, q.size, lower.tail=F);
  res.mat[,4]<- raw.pvals;
  res.mat[,5] <- p.adjust(raw.pvals, "fdr");
  
  # now, clean up result, synchronize with hit.query
  res.mat <- res.mat[hit.num>0,,drop = F];
  hits.query <- hits.query[hit.num>0];
  
  if(nrow(res.mat)> 1){
    # order by p value
    ord.inx<-order(res.mat[,4]);
    res.mat <- signif(res.mat[ord.inx,],3);
    hits.query <- hits.query[ord.inx];
    
    imp.inx <- res.mat[,4] <= 0.05;
    if(sum(imp.inx) < 10){ # too little left, give the top ones
      topn <- ifelse(nrow(res.mat) > 10, 10, nrow(res.mat));
      res.mat <- res.mat[1:topn,];
      hits.query <- hits.query[1:topn];
    }else{
      res.mat <- res.mat[imp.inx,];
      hits.query <- hits.query[imp.inx];
      if(sum(imp.inx) > 120){
        # now, clean up result, synchronize with hit.query
        res.mat <- res.mat[1:120,];
        hits.query <- hits.query[1:120];
      }
    }
  }
  
  Save2KEGGJSON(hits.query, res.mat, file.nm);
  return(1);
}

