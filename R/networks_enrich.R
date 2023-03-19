
#'Exports Gene-Mapping result into a table
#'@param mSetObj Input name of the created mSet Object
#'@export
GetNetworkGeneMappingResultTable<-function(mSetObj=NA){
  load_rsqlite()
  mSetObj <- .get.mSet(mSetObj);
  
  qvec <- mSetObj$dataSet$gene;
  enIDs <- mSetObj$dataSet$gene.name.map$hit.values;
  
  match.state<-mSetObj$dataSet$gene.name.map$match.state;
  # Map enIDs to KEGG orthologs 
  # TO-DO: run function for specific species
  if(length(qvec) > 0){
    if(mSetObj$dataSet$q.type.gene == "kos"){ 
      # Gene2KOMapping already done in PerformIntegGeneMapping in enrich_integ.R
      hit.kos <- mSetObj$dataSet$kos.name.map
    } else{
      hit.kos <- doGene2KONameMapping(enIDs)
    }
    match.state[is.na(hit.kos) & match.state!=0] <- 2
    match.state[!is.na(hit.kos) & match.state==0] <- 2
  } else{
    mSetObj$dataSet$q.type.gene = ""
    hit.kos = NULL
  }
  
  # style for highlighted background for unmatched names
  pre.style<-NULL;
  post.style<-NULL;
  
  # style for no matches
  if(mSetObj$dataSet$q.type.gene == "name"){
    no.prestyle<-"<strong style=\"background-color:yellow; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  } else{
    nokos.prestyle<-"<strong style=\"background-color:lightgrey; font-size=125%; color=\"black\">";
    nokos.poststyle<-"</strong>";
    no.prestyle<-"<strong style=\"background-color:red; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";  
  }
  
  # contruct the result table with cells wrapped in html tags
  # the unmatched will be highlighted in different background
  html.res<-matrix("", nrow=length(qvec), ncol=6);
  csv.res<-matrix("", nrow=length(qvec), ncol=6);
  colnames(csv.res)<-c("Query", "Entrez", "Symbol", "KO", "Name", "Comment");
  
  org.code <- mSetObj$org;
  sqlite.path <- paste0(url.pre, org.code, "_genes.sqlite");
  if(!file.exists(sqlite.path)){
    #"https://www.xialab.ca/resources/sqlite/hsa_genes.sqlite"
    sqlite_url <- paste0("https://www.xialab.ca/resources/sqlite/", 
                          org.code, "_genes.sqlite");
    sqlite.path <- paste0(getwd(), "/",org.code, "_genes.sqlite")
    download.file(sqlite_url,destfile = sqlite.path, method = "curl")
  }
  con <- .get.sqlite.con(sqlite.path);
  gene.db <- dbReadTable(con, "entrez")

  hit.inx <- match(enIDs, gene.db[, "gene_id"]);
  hit.values<-mSetObj$dataSet$gene.name.map$hit.values;
  mSetObj$dataSet$gene.name.map$hit.inx <- hit.inx;
  mSetObj$dataSet$gene.name.map$hit.kos <- hit.kos;
  hit.kos[is.na(hit.kos)] <- "";
  
  if(length(qvec) > 0){
    for (i in 1:length(qvec)){
      if(match.state[i]==1){
        pre.style<-"";
        post.style="";
      }else if(match.state[i]==2){
        pre.style<-nokos.prestyle;
        post.style<-nokos.poststyle;
      }else{ # no matches
        pre.style<-no.prestyle;
        post.style<-no.poststyle;
      }
      hit <-gene.db[hit.inx[i], ,drop=F];
      
      html.res[i, ]<-c(paste(pre.style, qvec[i], post.style, sep=""),
                       paste(ifelse(match.state[i]==0 || is.na(hit$gene_id),"-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>",hit$gene_id,"</a>", sep="")),  sep=""),
                       paste(ifelse(match.state[i]==0 || is.na(hit$symbol), "-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>", hit$symbol,"</a>", sep="")), sep=""),
                       paste(ifelse(is.na(hit.kos[i]), "-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>", hit.kos[i],"</a>", sep="")), sep=""),
                       paste(ifelse(match.state[i]==0 || is.na(hit$name),"-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>",hit$name,"</a>", sep="")), sep=""),
                       ifelse(match.state[i]!=1,"View",""));
      csv.res[i, ]<-c(qvec[i],
                      ifelse(match.state[i]==0, "NA", hit$gene_id),
                      ifelse(match.state[i]==0, "NA", hit$symbol),
                      ifelse(is.na(hit.kos[i]), "NA", hit.kos[i]),
                      ifelse(match.state[i]==0, "NA", hit$name),
                      match.state[i]);
    }
  }
  
  # store the value for report
  mSetObj$dataSet$gene.map.table <- csv.res;
  
  fast.write.csv(csv.res, file="gene_name_map.csv", row.names=F);
  dbDisconnect(con);

  if(.on.public.web){
    .set.mSet(mSetObj)  
    return(as.vector(html.res));
  }else{
    return(.set.mSet(mSetObj));
  }
}


# Utility function for SearchNetDB
.preparePhenoListSeeds <- function(mSetObj, table.nm){
  
  if(.on.public.web){
    libs.path <<- "../../libs/";
  }else{
    libs.path <<- "https://www.metaboanalyst.ca/resources/libs/";
  }
  
  table.nm <<- table.nm;
  # Preparing dataset variables
  mSetObj <- .get.mSet(mSetObj);
  # Retreive compounds information
  cmpds <- rownames(mSetObj$dataSet$pathinteg.imps$cmpd.mat);
  seed.compounds <- cmpds;
  seed.expr.compounds <- as.vector(mSetObj$dataSet$pathinteg.imps$cmpd.mat[,1]);
  
  # Retreive genes information
  genes <- rownames(mSetObj$dataSet$pathinteg.imps$gene.mat);
  # Prepare gene seeds for the graph when user upload no genes
  if(length(genes) == 0){
    seed.genes <- c();
    seed.expr.genes <- c();
  } else {
    seed.genes <- genes;
    seed.expr.genes <- as.vector(mSetObj$dataSet$pathinteg.imps$gene.mat[,1]);
  }
  
  # Change default seeds information (i.e. genes) to chemicals if needed
  if((table.nm == "metabo_phenotypes") || (table.nm == "metabo_metabolites")){
    seed.graph <<- seed.compounds;
    seed.expr <<- seed.expr.compounds;
  } else {
    seed.graph <<- c(seed.compounds, seed.genes);
    seed.expr <<- c(seed.expr.compounds, seed.expr.genes);
  }
  
  list(
    genes = genes,
    cmpds = cmpds
  );
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
        fast.write.csv(edge.res, file="orig_edge_list.csv",row.names=FALSE);
 
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

    if(node.evidence != "" && !is.null(genes.names)){
      # Evidence is related to the STITCH database accessions for chemicals/proteins
      node.res <- data.frame(Id=node.ids, Label=node.nms, GeneNames=genes.names, Evidence=node.evidence);
    } else if (node.evidence != "") {
      node.res <- data.frame(Id=node.ids, Label=node.nms, Evidence=node.evidence);
    } else if (!is.null(genes.names) ){
      node.res <- data.frame(Id=node.ids, Label=node.nms, GeneNames=genes.names);
    } else {
      node.res <- data.frame(Id=node.ids, Label=node.nms);
    }
    node.res <- node.res[!duplicated(node.res$Id),];
    nodeListu <<- node.res
    fast.write.csv(node.res, file="orig_node_list.csv", row.names=FALSE);

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
    sqlite.path <- paste0(url.pre, "MetPriCNet.sqlite");
    pheno.db <- .get.sqlite.con(sqlite.path);
  }else{
    download.file("https://www.xialab.ca/resources/sqlite/MetPriCNet.sqlite", "MetPriCNet.sqlite")
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
    names(genespheno.res) <- c("id1", "id2", "name1", "name2", "evidsrc", "evidtar");    
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
    
  }
    dbDisconnect(pheno.db);
    return(pheno.dic);
}

# Perform DSPC network analysis
PerformDSPC <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj); 
  dat <- mSetObj$dataSet$norm; 

  # glasso cannot work well on large data,
  # must be log or cubic root transformed
  if(!mSetObj$dataSet$trans.method %in% c("Cubic Root Transformation", "Log10 Normalization")){
      dat <- qs::qread("row_norm.qs");
      min.val <- min(abs(dat[dat!=0]))/10;
      dat <- LogNorm(dat, min.val);
      print("Perform log transformation for glasso ...");
  }

  node.nms <- colnames(dat);
  if(!is.null(mSetObj$dataSet$orig.var.ids)){
    node.ids <- mSetObj$dataSet$orig.var.ids; 
  }else{
    node.ids <- node.nms;
  }
  colnames(dat) <- node.ids;

  # this is computationally intensive, put a limit if not local
  if(.on.public.web){
    if(ncol(dat) > 1000){
        filter.val <- apply(dat, 2, IQR, na.rm=T);
        rk <- rank(-filter.val, ties.method='random');
        my.inx <- rk <= 1000;
        dat <- dat[,my.inx];
        node.ids <- node.ids[my.inx];
        node.nms <- node.nms[my.inx];
        print("Data is reduced to 1000 vars based on IQR ..");
    }
  }
  dspc.res <- tryCatch({
        DGlasso(dat, alpha=.01, FDR.type='BH');
      }, error = function(e) {
        AddErrMsg("DSPC failed - possible reason: some variables are highly collinear!");
        return(NULL); # this is the return value 
  });

  if(is.null(dspc.res)) {
        AddErrMsg("DSPC failed - possible reason: some variables are highly collinear!");
        return(0); # return the block
  }
  node.res <- data.frame(Id=node.ids, Label=node.nms);
  node.res <- node.res[!duplicated(node.res$Id),];
  fast.write.csv(node.res, file="orig_node_list.csv", row.names=FALSE);
  
  edge.res <- data.frame(Source=dspc.res[,"source"],Target=dspc.res[,"target"],
                Coefficient=signif(dspc.res[,"coeff"], 3),
                Pval=signif(dspc.res[,"pvalue"], 3),
                Adj_Pval=signif(dspc.res[,"qvalue"], 3));

  row.names(edge.res) <- 1:nrow(dspc.res);
  fast.write.csv(edge.res, file="orig_edge_list.csv",row.names=FALSE);
  nodeListu <<- node.res;
  
  pheno.net <<- list(
    order=1, 
    seeds=node.ids,  
    table.nm="dspc", 
    node.data = node.res, 
    edge.data = edge.res,
    require.exp = "TRUE",
    min.score = 900
  );
  
  if(.on.public.web){
    return(c(nrow(node.res), nrow(dspc.res)));
  }else{
    return(.set.mSet(mSetObj));
  }
}

# Utility function for PerformDSPC
#'@import glasso
DGlasso <- function(X, alpha = 0.05, lambda = NULL, FDRctrl = FALSE, FDR.type='bonferroni'){
  require(glasso);
  n = nrow(X)
  p = ncol(X)
  node.ids <- colnames(X);

  if (is.null(lambda)){
    lambda = sqrt(log(p)/n)
  }
  
  Sigma.hat_X = var(X);
  #Theta.hat_glasso = glasso(s=Sigma.hat_X, rho=lambda, approx=TRUE, penalize.diagonal=FALSE)$wi; # approx always give error?!

  Theta.hat_glasso = glasso(s=Sigma.hat_X, rho=lambda, penalize.diagonal=FALSE)$wi;

  # T.hat = as.vector(Theta.hat_glasso)-kronecker(Theta.hat_glasso,Theta.hat_glasso,"*") %*% as.vector(Sigma.hat_X - chol2inv(chol(Theta.hat_glasso)))
  # reduce memory consumption from O(n^4) to O(n^2) by avoiding kronecker calculation
  temp.mat = Sigma.hat_X - chol2inv(chol(Theta.hat_glasso))
  temp.vec = as.vector(Theta.hat_glasso %*% temp.mat %*% t(Theta.hat_glasso))
  T.hat = as.vector(Theta.hat_glasso) - temp.vec;
  
  T.hat.vec = myUpperTriangle(matrix(T.hat,nrow = p),diag=FALSE)
  
  sigma.hat2 = array(0,c(p,p));
  for (i in 1:p){
    for (j in 1:p){
      sigma.hat2[i,j] = Theta.hat_glasso[i,j]^2+Theta.hat_glasso[i,i]*Theta.hat_glasso[j,j];
    }
  }
  sigma.hat2.vec = myUpperTriangle(sigma.hat2,diag=FALSE);  
  test.stat = sqrt(n)*T.hat.vec/sqrt(sigma.hat2.vec)
  pvals = 2*(pnorm(abs(test.stat), lower.tail=FALSE))
  adj.p = p.adjust(pvals, FDR.type)
  
  # now get source target 
  rownames(sigma.hat2) <- colnames(sigma.hat2) <- node.ids;
  sigma.hat2[upper.tri(sigma.hat2,diag=TRUE)] <- NA;
  src.tgt <- as.data.frame(as.table(sigma.hat2));
  src.tgt <- na.omit(src.tgt);

  # make sure they are in sync! 
  ord.inx <- order(match(src.tgt[,3],sigma.hat2.vec));
  src.tgt <- src.tgt[ord.inx, ]

  return(data.frame(source=as.character(src.tgt[,2]), target=as.character(src.tgt[,1]), coeff=-pmax(pmin(T.hat.vec, 1), -1), pvalue=pvals, qvalue=adj.p))
}

# from gdata
myUpperTriangle <- function (x, diag = FALSE, byrow = FALSE){
    if (byrow)
        t(x)[rev(upper.tri(x, diag = diag))]
    else x[upper.tri(x, diag = diag)]
}




