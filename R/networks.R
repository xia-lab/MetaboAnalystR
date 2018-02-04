# Mapping genes and metabolites to KEGG

#'Prepare data for network exploration
#'@description Function for the network explorer module, prepares user's data for network exploration.
#'@param mSetObj Input name of the created mSet Object
#'@export
PrepareNetworkData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # prepare gene list
  if(!is.null(mSetObj$dataSet$gene.mat)){
    gene.mat <- mSetObj$dataSet$gene.mat;
    enIDs <- mSetObj$dataSet$gene.name.map$hit.values;
    kos <- mSetObj$dataSet$gene.name.map$hit.kos;
    rownames(gene.mat) <- enIDs;
    
    na.inx <- is.na(kos);
    gene.mat.clean <- gene.mat[!na.inx, ,drop=F];
    kos.clean <- kos[!na.inx]
    
    gene.names <- rownames(gene.mat.clean)
    gene.mat.clean <- RemoveDuplicates(gene.mat.clean);
    if(nrow(gene.mat.clean) < length(kos.clean)){
      mSetObj$dataSet$gene.name.map$hit.kos <- kos.clean[!duplicated(gene.names)]
    } else{
      mSetObj$dataSet$gene.name.map$hit.kos <- kos.clean
    }
    AddMsg(paste("A total of ", nrow(gene.mat.clean), "unique genes were uploaded."));
    
    if(!exists("pathinteg.imps", where = mSetObj$dataSet)){
      mSetObj$dataSet$pathinteg.imps <- list();
    }
    mSetObj$dataSet$pathinteg.imps$gene.mat <- gene.mat.clean;
    done <- 1;
  }
  
  # prepare kos list
  if(!is.null(mSetObj$dataSet$gene.mat)){
    # Handle case when upload type is KOs
    if(mSetObj$dataSet$q.type.gene == "kos"){
      rownames(gene.mat) <- kos
      gene.mat <- RemoveDuplicates(gene.mat);
      mSetObj$dataSet$gene.name.map$hit.kos <- rownames(gene.mat)
      AddMsg(paste("A total of ", nrow(gene.mat), "unique KOs were uploaded."));
      
      if(!exists("pathinteg.imps", where = mSetObj$dataSet)){
        mSetObj$dataSet$pathinteg.imps <- list();
      }
      mSetObj$dataSet$pathinteg.imps$kos.mat <- gene.mat;
      done <- 1;
    } else{
      mSetObj$dataSet$pathinteg.imps$kos.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    }
  }
  
  # prepare compound list
  if(!is.null(mSetObj$dataSet$cmpd.mat)){
    nm.map <- GetFinalNameMap(mSetObj);
    valid.inx <- !(is.na(nm.map$kegg)| duplicated(nm.map$kegg));
    cmpd.vec <- nm.map$query[valid.inx];
    kegg.id <- nm.map$kegg[valid.inx];
    
    cmpd.mat <- mSetObj$dataSet$cmpd.mat;
    hit.inx <- match(cmpd.vec, rownames(cmpd.mat));
    
    cmpd.mat <- cmpd.mat[hit.inx, ,drop=F];
    rownames(cmpd.mat) <- kegg.id;
    cmpd.mat <- RemoveDuplicates(cmpd.mat);
    AddMsg(paste("A total of ", nrow(cmpd.mat), "unique compounds were found."));
    mSetObj$dataSet$pathinteg.imps$cmpd.mat <- cmpd.mat;
    done <- 1;
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj);  
    return(done);
  }
  return(.set.mSet(mSetObj));
}

#'Prepare user's query for mapping KEGG Global Metabolic Network
#'@description This function prepares the user's data for the 
#'KEGG Global Metabolic Network
#'@param mSetObj Input name of the created mSet Object
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PrepareQueryJson <- function(mSetObj=NA){

    # Map query matched KOs with the KO database
    mSetObj <- .get.mSet(mSetObj);
    kos <- mSetObj$dataSet$gene.name.map$hit.kos
    expr.mat <- mSetObj$dataSet$pathinteg.imps$kos.mat
    kos <- cbind(kos, expr.mat)
    # Retreive compounds information
    cmpds.expr <- mSetObj$dataSet$pathinteg.imps$cmpd.mat
    cmpds <- cbind(rownames(cmpds.expr), cmpds.expr)

    enrich.type <- "hyper";

    # Perform gene enrichment
    gene.mat <- list()
    if(length(kos) > 0){
        dataSet.gene <- PerformMapping(kos, "ko")
        if(length(dataSet.gene)==0){
            return(0);
        }

        if(enrich.type == "hyper"){
            exp.vec <- dataSet.gene$data[,1]; # drop dim for json
        }else{
            # for global test, all KO measured should be highlighted
            genemat<-as.data.frame(t(otu_table(dataSet.gene$norm.phyobj)));
            exp.vec <- rep(2, ncol(genemat));
            names(exp.vec) <- colnames(genemat);
        }
        gene.mat <- MapKO2KEGGEdges(exp.vec);
    }

    # Perform compound enrichment
    cmpd.mat <- list()
    if(length(cmpds) > 1){
        dataSet.cmpd <- PerformMapping(cmpds, "cmpd")
        if(length(dataSet.cmpd)==0){
          return(0);
        }

        if(enrich.type == "hyper"){
            exp.vec <- dataSet.cmpd$data[,1]; # drop dim for json
        }else{
            # for global test, all KO measured should be highlighted
            genemat<-as.data.frame(t(otu_table(dataSet.cmpd$norm.phyobj)));
            exp.vec <- rep(2, ncol(genemat));
            names(exp.vec) <- colnames(genemat);
        }
        cmpd.mat <- MapCmpd2KEGGNodes(exp.vec);
    }
    
    # TO-DO: Refactor the following part of code for better readability
    if(length(cmpd.mat) != 0 && length(gene.mat) != 0){
      edge.mat <- as.data.frame(rbind(as.matrix(cmpd.mat), as.matrix(gene.mat)));
      dataSet <<- MergeDatasets(dataSet.cmpd, dataSet.gene);
      idtype <<- "gene&cmpd";
    } else if(length(cmpd.mat) != 0){
      edge.mat <- cmpd.mat;
      dataSet <<- dataSet.cmpd;
      idtype <<- "cmpd";
    } else{
      edge.mat <- gene.mat;
      dataSet <<- dataSet.gene;
      idtype <<- "gene";
    }
    
    row.names(edge.mat) <- eids <- rownames(edge.mat);
    query.ko <- edge.mat[,1];
    net.orig <- edge.mat[,2];
    query.res <- edge.mat[,3];# abundance
    names(query.res) <- eids; # named by edge

    library(RJSONIO);
    json.mat <- toJSON(query.res, .na='null');
    sink("network_query.json");
    cat(json.mat);
    sink();

    if(.on.public.web==TRUE){
      return(1);
    }else{
      return(.set.mSet(mSetObj)); 
    }
}

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
#'@param table.nm Input the organism code for the sqlite table (ppi). For chemical type, the 
#'table.nm is drugbank of ctd
#'@param require.exp Logical, only used for the STRING database
#'@param min.score Input the minimal score, only used for the STRING database 
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SearchNetDB <- function(mSetObj=NA, db.type, table.nm, require.exp=TRUE, min.score = 900){ 

  mSetObj <- .get.mSet(mSetObj);
  
    result.list <- .preparePhenoListSeeds(table.nm);
    genes <- result.list$genes;
    protein.vec <- seed.genes;
    cmpds <- result.list$cmpds;

    network.type <<- table.nm 

    library(RJSONIO);

    # now do the database search
    
    if(db.type == "pheno"){
        res <- QueryPhenoSQLite(table.nm, genes, cmpds, min.score);
        if(nrow(res)==0){ return(c(0,0)); }

        if(table.nm == "gene_metabolites"){
            src <- "entrez"; src.nm <- "symbol";
            target <- "ctdid"; target.nm <- "name";
        } else if(table.nm == "metabo_phenotypes"){
            src <- "ctdid"; src.nm <- "name";
            target <- "omimid"; target.nm <- "phenoname";
        } else if(table.nm == "metabo_metabolites"){
            src <- "ctdid1"; src.nm <- "name1";
            target <- "ctdid2"; target.nm <- "name2";
        } else if(table.nm == "gene_phenotypes"){
            src <- "entrez"; src.nm <- "symbol";
            target <- "omimid"; target.nm <- "phenoname";
        } else if(table.nm == "global"){
          src <- "id1"; src.nm <- "name1";
          target <- "id2"; target.nm <- "name2";
        }

        edge.res <- data.frame(Source=res[,src],Target=res[,target]);
        row.names(edge.res) <- 1:nrow(res);
        write.csv(edge.res, file="orig_edge_list.csv",row.names=FALSE);
    
        node.ids <- c(res[,src], res[,target])
        node.nms <- c(res[,src.nm], res[,target.nm]);
    }
    
    # Retrieve gene full names
    genes.names.idx <- match(node.ids, mSetObj$dataSet$gene.map.table[,"Entrez"])
    genes.names <- mSetObj$dataSet$gene.map.table[genes.names.idx,"Name"]

    node.res <- data.frame(Id=node.ids, Label=node.nms, GeneNames=genes.names);
    node.res <- node.res[!duplicated(node.res$Id),];
    nodeListu <<- node.res
    write.csv(node.res, file="orig_node_list.csv", row.names=FALSE);

    ppi.net <<- list(
                    db.type=db.type,
                    order=1, 
                    seeds=protein.vec, 
                    table.nm=table.nm, 
                    node.data = node.res, 
                    edge.data = edge.res,
                    require.exp = require.exp,
                    min.score = min.score
                );

    if(.on.public.web==TRUE){
      return(c(nrow(node.res), nrow(res)));
    }else{
      return(.set.mSet(mSetObj));
    }
}

#'Update integrative pathway analysis for new input list
#'@description used for integrative analysis 
#'as well as general pathways analysis for meta-analysis results
#'@usage PerformIntegPathwayAnalysis(mSetObj, topo="dc", enrich="hyper", libOpt="integ")
#'@param mSetObj Input name of the created mSet Object
#'@param topo Select the mode for topology analysis: Degree Centrality ("dc") measures the number of links that connect to a node 
#'(representing either a gene or metabolite) within a pathway; Closeness Centrality ("cc") measures the overall distance from a given node 
#'to all other nodes in a pathway; Betweenness Centrality ("bc")measures the number of shortest paths from all nodes to all the others that pass through a given node within a pathway.
#'@param enrich Method to perform over-representation analysis (ORA) based on either hypergenometrics analysis ("hyper")
#' or Fisher's exact method ("fisher").
#'@param libOpt Select the different modes of pathways, either the gene-metabolite mode ("integ") which allows for joint-analysis
#' and visualization of both significant genes and metabolites or the gene-centric ("genetic") and metabolite-centric mode ("metab") which allows users
#' to identify enriched pathways driven by significant genes or metabolites, respectively. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
UpdateIntegPathwayAnalysis <- function(mSetObj=NA, qids, file.nm, topo="dc", enrich="hyper", libOpt="integ"){

  qids <- do.call(rbind, strsplit(qids, "; "));
  idtypes <- unlist(sapply(qids, function(x) substring(x, 1, 1) == "C"))
  qcmpds <- qids[idtypes]
  qgenes <- qids[!idtypes]
  
  mSetObj <- .get.mSet(mSetObj);  

  LoadKEGGLib(libOpt);

  set.size <- length(inmexpa$mset.list);
  ms.list <- lapply(inmexpa$mset.list, function(x){strsplit(x, " ")});
  current.universe <- unique(unlist(ms.list));

  # prepare for the result table
  res.mat<-matrix(0, nrow=set.size, ncol=7);
  rownames(res.mat)<-names(inmexpa$path.ids);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "Topology", "PVal.Z",  "Topo.Z");
  
  mSetObj$dataSet$pathinteg.method <- libOpt;
  mSetObj$dataSet$path.mat <- NULL;
  
  if(libOpt == "genetic"){
    gene.vec <- paste(pathinteg.org, ":", qgenes, sep="");
    ora.vec <- gene.vec;
    ora.vec.ids <- c(qgenes);
    uniq.count <- inmexpa$uniq.gene.count;
    uniq.len <- inmexpa$gene.counts;
    
  }else if(libOpt == "metab"){
    cmpd.vec <- paste("cpd:", qcmpds, sep="");
    ora.vec <- cmpd.vec;
    ora.vec.ids <- c(qcmpds);
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;

  }else{ # integ

    cmpd.vec <- paste("cpd:", qcmpds, sep="");
    gene.vec <- paste(pathinteg.org, ":", qgenes, sep="");
    ora.vec <- c(cmpd.vec, gene.vec);
    ora.vec.ids <- c(qcmpds, qgenes);
    
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;
    uniq.count <- uniq.count + inmexpa$uniq.gene.count;
    uniq.len <- uniq.len + inmexpa$gene.counts;
      
  }
  # need to cut to the universe covered by the pathways, not all genes 
  ora.vec <- ora.vec[ora.vec %in% current.universe]
  q.size <- length(ora.vec);
  # note, we need to do twice one for nodes (for plotting)
  # one for query for calculating, as one node can be associated with multiple matches
  # get the matched nodes on each pathway
  hits.path <- lapply(ms.list, function(x) {unlist(lapply(x, function(var){any(var%in%ora.vec);}),use.names=FALSE)});
  names(hits.path) <- inmexpa$path.ids;
  
  # get the matched query for each pathway
  hits.query <- lapply(ms.list, function(x) {ora.vec%in%unlist(x);});
  
  hit.num <- unlist(lapply(hits.query, function(x){sum(x)}), use.names=FALSE);
  
  if(sum(hit.num) == 0){
    AddErrMsg("No hits found for your input!");
    return(0);
  }
  
  set.num <- uniq.len;
  res.mat[,1]<-set.num;
  res.mat[,2]<-q.size*(set.num/uniq.count);
  res.mat[,3]<-hit.num;
  
  # use lower.tail = F for P(X>x)
  if(enrich=="hyper"){
    res.mat[,4] <- phyper(hit.num-1, set.num, uniq.count-set.num, q.size, lower.tail=F);
    
  }else if(enrich == "fisher"){
    res.mat[,4] <- GetFisherPvalue(hit.num, q.size, set.num, uniq.count);
  }else{
    print("Not defined enrichment method!");
    print(enrich);
  }
  
  # adjust for multiple testing problems
  # res.mat[,5] <- p.adjust(res.mat[,4], "fdr");
  
  # toplogy test
  if(topo == "bc"){
    imp.list <- inmexpa$bc;
  }else if(topo == "dc"){
    imp.list <- inmexpa$dc;
  }else if(topo == "cc"){
    imp.list <- inmexpa$cc;       
  }else{
    print("Not a defined topological measure!");
    print(topo);
  }
  
  # now, perform topological analysis		
  # calculate the sum of importance
  res.mat[,5] <- mapply(function(x, y){sum(x[y])}, imp.list, hits.path);
  
  # now add two more columns for the scaled values
  res.mat[,6] <- scale(-log(res.mat[,4]));
  res.mat[,7] <- scale(res.mat[,5]);
  
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
  
  hits.names <- lapply(hits.query, function(x) ora.vec.ids[x]);
  #res.mat <- data.frame(res.mat);
  
  #get gene symbols
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  
  library(RJSONIO);
  fun.anot = hits.names; names(fun.anot) <- resTable[,1];
  fun.pval = resTable[,5]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  hit.num = resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  current.setlink <- "http://www.genome.jp/kegg-bin/show_pathway?";
  #fun.ids <- as.vector(current.setids[names(fun.anot)]); 
  #if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  json.res <- list(
              fun.link = current.setlink[1],
              fun.anot = fun.anot,
              #fun.ids = fun.ids,
              fun.pval = fun.pval,
              hit.num = hit.num
  );
  json.mat <- toJSON(json.res, .na='null');
  json.nm <- paste(file.nm, ".json", sep="");
  
  sink(json.nm)
  cat(json.mat);
  sink();

}

#'Create igraph from the edgelist saved from graph DB and decompose into subnets
#'@description Function for the network explorer module, prepares user's data for network exploration.
#'@param mSetObj Input name of the created mSet Object
#'@export
CreateGraph <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  require('igraph');
  node.list <- ppi.net$node.data;
  edge.list <- ppi.net$edge.data;
  
  seed.proteins <- ppi.net$seeds;
  overall.graph <- simplify(graph.data.frame(edge.list, directed=FALSE, vertices=node.list));
  
  # add node expression value
  #newIDs <- names(seed.expr);
  newIDs <- seed.genes;
  
  match.index <- match(V(overall.graph)$name, newIDs);
  expr.vals <- seed.expr[match.index];
  overall.graph <- set.vertex.attribute(overall.graph, "abundance", index = V(overall.graph), value = expr.vals);
  
  hit.inx <- seed.proteins %in% node.list[,1];
  seed.proteins <<- seed.proteins[hit.inx];
  
  substats <- DecomposeGraph(overall.graph);
  overall.graph <<- overall.graph;
  
  PlotNetwork(mSetObj, network.type);
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    if(!is.null(substats)){
      return(c(length(seed.genes), length(seed.proteins), nrow(node.list), nrow(edge.list), length(ppi.comps), substats));        
    }else{
      return(0);
    }
  }else{
    return(.set.mSet(mSetObj));
  }
}

###
### Utility functions
###

#'Utility function to plot network for analysis report (CreateGraph)
PlotNetwork <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  library(Cairo)
  
  img.Name = paste(imgName, "_dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
    
  }else{
    w <- width;
  }
  
  h <- w;
  
  mSetObj$imgSet$networkplot <- img.Name
  
  nodeColors <- rep("lightgrey", length(V(overall.graph)))
  idx.cmpd <- as.vector(sapply(names(V(overall.graph)), function(x) substring(x,0,1) == "C"))
  idx.genes <- names(V(overall.graph)) %in% mSetObj$dataSet$gene
  nodeColors[idx.cmpd] <- "orange"
  nodeColors[idx.genes] <- "#306EFF"
  V(overall.graph)$color <- nodeColors
  
  # annotation
  nms <- V(overall.graph)$name;
  hit.inx <- match(nms, ppi.net$node.data[,1]);
  lbls <- ppi.net$node.data[hit.inx,2];
  V(overall.graph)$name <- as.vector(lbls);
  
  
  Cairo(file = img.Name, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(overall.graph)
  text(1,1, "Entrez gene ", col="#306EFF")
  text(1,0.9, "KEGG compound ", col="orange")
  if(sum(nodeColors == "lightgrey") > 0){
    text(1,0.8, "OMIM disease ", col="lightgrey")
  }
  dev.off();
  
  if(.on.public.web==FALSE){
    return(.set.mSet(mSetObj));
  }else{
    .set.mSet(mSetObj);
  }
}

#' Utility function for PrepareNetworkData
doGene2KONameMapping <- function(enIDs){

  ko.dic <- .readDataTable("../../libs/network/ko_dic_new.csv");

  #TO-DO: map based on specific selection of a species
  ko.dic.enIDs <- as.integer(ko.dic[, "Entrez_hsa"])
  ko.dic.enIDs[is.na(ko.dic.enIDs)] <- -1
  hit.inx <- match(as.integer(enIDs), ko.dic.enIDs);
  kos <- ko.dic[hit.inx, "KO"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(kos);
  kos[na.inx] <- NA #enIDs[na.inx];
  
  return(kos);
}

#' Utility function for PerformKOEnrichAnalysis_KO01100
# note: only return hits in this map KO01100
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

#' Utility function for PerformKOEnrichAnalysis_List
# for KO01100
Save2KEGGJSON <- function(hits.query, res.mat, file.nm){
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  AddMsg("Functional enrichment analysis was completed");
  if(!exists("ko.edge.map")){
    ko.edge.path <- paste("../../libs/network/ko_edge.csv", sep="");
    ko.edge.map <- .readDataTable(ko.edge.path);
    ko.edge.map <- ko.edge.map[ko.edge.map$net=="ko01100",];  #only one map
    ko.edge.map <<- ko.edge.map;
  }
  
  hits.edge <- list();
  hits.node <- list();
  
  if(idtype == "gene"){
    ko.map <- ko.edge.map;
    colnames(ko.map) <- c("queryid", "edge", "net")
    hits.edge <- lapply(hits.query,
                        function(x) {
                          as.character(unique(ko.map$edge[ko.map$queryid%in%unlist(x)]));
                        }
    );
    hits.inx <- unlist(lapply(hits.edge, length))>0;
  } else if(idtype == "cmpd"){
    ko.map <- ko.node.map.global;
    colnames(ko.map) <- c("queryid", "edge", "net")
    hits.node <- lapply(hits.query,
                        function(x) {
                          as.character(unique(ko.map$edge[ko.map$queryid%in%unlist(x)]));
                        }
    );
    hits.inx <- unlist(lapply(hits.node, length))>0;
  } else{
    # gene&cmpd
    ko.map1 <- ko.edge.map;
    colnames(ko.map1) <- c("queryid", "edge", "net"); rownames(ko.map1)<-NULL;
    hits.edge <- lapply(hits.query,
                        function(x) {
                          as.character(unique(ko.map1$edge[ko.map1$queryid%in%unlist(x)]));
                        }
    );
    ko.map2 <- ko.node.map.global;
    colnames(ko.map2) <- c("queryid", "edge", "net"); rownames(ko.map2)<-NULL;
    hits.node <- lapply(hits.query,
                        function(x) {
                          as.character(unique(ko.map2$edge[ko.map2$queryid%in%unlist(x)]));
                        }
    );
    ko.map <- rbind(ko.map1, ko.map2)
    # TO-DO: combine results hits.edge and hits.node without applying again lapply over hits.query
    hits.both <- lapply(hits.query,
                        function(x) {
                          as.character(unique(ko.map$edge[ko.map$queryid%in%unlist(x)]));
                        }
    );
    hits.inx <- unlist(lapply(hits.both, length))>0;
  }
  
  # only keep hits with edges in the map
  hits.query <- hits.query[hits.inx];
  resTable <- resTable[hits.inx, ];
  
  # write json
  fun.pval = resTable$Pval; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  hit.num = resTable$Hits; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  fun.ids <- as.vector(current.setids[names(hits.query)]); if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  
  expr = as.list(dataSet$data)
  names(expr) <- rownames(dataSet$data)
  json.res <- list(
    expr.mat = expr,
    hits.query = hits.query,
    hits.edge = hits.edge,
    hits.node = hits.node,
    path.id = fun.ids,
    fun.pval = fun.pval,
    hit.num = hit.num
  );
  require(RJSONIO);
  json.mat <- toJSON(json.res, .na='null');
  json.nm <- paste(file.nm, ".json", sep="");
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  fun.hits <<- hits.query;
  fun.pval <<- resTable[,5];
  hit.num <<- resTable[,4];
  csv.nm <- paste(file.nm, ".csv", sep="");
  write.csv(resTable, file=csv.nm, row.names=F);
}

#' Utility function for PerformKOEnrichAnalysis_KO01100
LoadKEGGKO_lib<-function(category){
  if(category == "module"){
    kegg.rda <- "../../libs/network/ko_modules.rda";
    load(kegg.rda);
    current.setlink <- kegg.anot$link;
    current.mset <- kegg.anot$sets$"Pathway module";
  }else{
    kegg.rda <- "../../libs/network/ko_pathways_v2.rda";
    load(kegg.rda);
    current.setlink <- kegg.anot$link;
    current.mset <- kegg.anot$sets$Metabolism;
  }
  # now need to update the msets to contain only those in ko01100 map
  if(!exists("ko.edge.map")){
    ko.edge.path <- paste("../../libs/network/ko_edge.csv", sep="");
    ko.edge.map <<- .readDataTable(ko.edge.path);     
  } 
  
  kos.01100 <- ko.edge.map$gene[ko.edge.map$net == "ko01100"];
  current.mset <- lapply(current.mset, 
                         function(x) {
                           as.character(unique(x[x %in% kos.01100]));
                         }
  );
  # remove those empty ones
  mset.ln <- lapply(current.mset, length);
  current.mset <- current.mset[mset.ln > 0];
  set.ids<- names(current.mset); 
  names(set.ids) <- names(current.mset) <- kegg.anot$term[set.ids];
  
  current.setlink <<- current.setlink;
  current.setids <<- set.ids;
  current.geneset <<- current.mset;
}

# Utility function
doProteinIDMappingNetwork <- function(q.vec, type){
  print(type)
  if(type == "entrez"){
    # need to get only our data
    db.path <- paste(libs.path, data.org, "/entrez.rds", sep="");
    db.map <-  readRDS(db.path);
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
    entrezs <- db.map[hit.inx, ]
    entrezs <- entrezs[c(1,1)]
    colnames(entrezs) = c("gene_id", "accession");
  }else if(type == "symbol"){
    db.path <- paste(libs.path, data.org, "/entrez.rds", sep="");
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
      db.path <- paste(libs.path, data.org, "/entrez.rds", sep="");
    }else if(type == "refseq"){
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
      db.path <- paste(libs.path, data.org, "/entrez_refseq.rds", sep="");
    }else if(type == "emblgene"){
      db.path <- paste(libs.path, data.org, "/entrez_embl_gene.rds", sep="");
    }else if(type == "tair"){
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
      db.path <- paste(libs.path, data.org, "/entrez_embl_transcript.rds", sep="");
    }else if(type == "embltranscript"){
      db.path <- paste(libs.path, data.org, "/entrez_embl_transcript.rds", sep="");
    }else if(type == "emblprotein"){
      db.path <- paste(libs.path, data.org, "/entrez_embl_protein.rds", sep="");
    }else if(type == "orfid"){ # only for yeast
      db.path <- paste(libs.path, data.org, "/entrez_orf.rds", sep="");
    }else if(type == "flybase"){
      db.path <- paste(libs.path, data.org, "/entrez_flybase.rds", sep="");
    }else if(type == "string"){ 
      db.path <- paste(libs.path, data.org, "/entrez_string.rds", sep="")
    }else if(type == "ecogene"){ # only for ecoli
      db.path <- paste(libs.path, data.org, "/entrez_ecogene.rds", sep="")
    }else if(type == "uniprot"){
      db.path <- paste(libs.path, data.org, "/entrez_uniprot.rds", sep="")
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

#'Utility function for PerformNetEnrichment
#'@export
doEmblProtein2EntrezMapping <- function(emblprotein.vec){
  db.path <- paste(libs.path, data.org, "/entrez_embl_protein.rds", sep="");
  db.map <-  readRDS(db.path);
  hit.inx <- match(emblprotein.vec, db.map[, "accession"]);
  entrezs <- db.map[hit.inx, "gene_id"];
  mode(entrezs) <- "character";
  return(entrezs);
}

#' Utility function for SearchNetDB
.preparePhenoListSeeds <- function(table.nm){
  lib.path <<- "../../data/";
  libs.path <<- "../../libs/";
  
  table.nm <<- table.nm;
  # Preparing dataset variables
  mSetObj <- .get.mSet(mSetObj);
  # Retreive compounds information
  cmpds <- rownames(mSetObj$dataSet$pathinteg.imps$cmpd.mat);
  seed.compounds <<- cmpds;
  seed.expr.compounds <<- as.vector(mSetObj$dataSet$pathinteg.imps$cmpd.mat[,1]);
  # Retreive genes information
  genes <- rownames(mSetObj$dataSet$pathinteg.imps$gene.mat);
  seed.genes <<- genes;
  
  # Prepare gene seeds for the graph when user upload no genes
  if(length(genes) == 0){
    db.path <- paste("../../libs/", pathinteg.org, "/entrez.csv", sep="");
    db.map <-  .readDataTable(db.path);
    genes <- db.map[, "gene_id"];
    rm(db.map);
    seed.genes <<- genes;
    seed.expr <<- numeric(length(genes));
  } else{
    seed.expr <<- as.vector(mSetObj$dataSet$pathinteg.imps$gene.mat[,1]);
  }
  
  # Change default seeds information (i.e. genes) to chemicals if needed
  #   TO-DO: rename seed.genes to seed.nodes for example (more general)
  if((table.nm == "metabo_phenotypes") || (table.nm == "metabo_metabolites")){
    seed.genes <<- seed.compounds;
    seed.expr <<- seed.expr.compounds;
  } else if(table.nm == "gene_metabolites"){
    seed.genes <<- c(seed.compounds, seed.genes);
    seed.expr <<- c(seed.expr.compounds, seed.expr);
  }
  
  list(
    genes = genes,
    cmpds = cmpds
  );
}

#' Utility function for SearchNetDB
# table name is org code, id.type is column name
QueryPhenoSQLite<- function(table.nm, genes, cmpds, min.score){
  require('RSQLite');
  pheno.db <- dbConnect(SQLite(), "../../libs/network/MetPriCNet.sqlite");
  
  if(table.nm == "global"){
    # Handle gene_metabolites
    table.nm <- "gene_metabolites";
    if((length(genes) > 0) && (length(cmpds) > 0)){
      genes.query <- paste (shQuote(genes),collapse=",");
      cmpds.query <- paste (shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(length(genes) > 0){
      genes.query <- paste (shQuote(genes),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
    } else{
      cmpds.query <- paste (shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    }
    phenotable <- dbSendQuery(pheno.db, statement);
    genemetab.res <- fetch(phenotable, n=-1); # get all records
    genemetab.res <- genemetab.res[,1:4]
    names(genemetab.res) <- c("id1", "id2", "name1", "name2")
    
    # Handle metab_phenotypes
    table.nm <- "metabo_phenotypes";
    cmpds.query <- paste (shQuote(cmpds),collapse=",");
    statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");      
    phenotable <- dbSendQuery(pheno.db, statement);
    metabpheno.res <- fetch(phenotable, n=-1); # get all records
    metabpheno.res <- metabpheno.res[,1:4]
    names(metabpheno.res) <- c("id1", "id2", "name1", "name2")
    
    # Handle genes_phenotypes
    table.nm <- "gene_phenotypes";
    genes.query <- paste (shQuote(genes),collapse=",");
    statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");      
    phenotable <- dbSendQuery(pheno.db, statement);
    genespheno.res <- fetch(phenotable, n=-1); # get all records
    genespheno.res <- genespheno.res[,1:4]
    names(genespheno.res) <- c("id1", "id2", "name1", "name2")
    dbDisconnect(pheno.db);
    
    # Combine all
    print(ncol(genemetab.res))
    print(ncol(metabpheno.res))
    print(ncol(genespheno.res))
    pheno.dic <- rbind(genemetab.res, metabpheno.res, genespheno.res)
    
  } else{
    if(table.nm == "gene_metabolites"){
      if((length(genes) > 0) && (length(cmpds) > 0)){
        genes.query <- paste (shQuote(genes),collapse=",");
        cmpds.query <- paste (shQuote(cmpds),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
      } else if(length(genes) > 0){
        genes.query <- paste (shQuote(genes),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
      } else{
        cmpds.query <- paste (shQuote(cmpds),collapse=",");
        statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
      }
    } else if(table.nm == "metabo_phenotypes"){
      cmpds.query <- paste (shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(table.nm == "metabo_metabolites"){
      cmpds.query <- paste (shQuote(cmpds),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE ctdid1 IN (",cmpds.query,") AND score >= ",min.score, sep="");
    } else if(table.nm == "gene_phenotypes"){
      genes.query <- paste (shQuote(genes),collapse=",");
      statement <- paste("SELECT * FROM ", table.nm, " WHERE entrez IN (",genes.query,") AND score >= ",min.score, sep="");
    }
    
    phenotable <- dbSendQuery(pheno.db, statement);
    pheno.dic <- fetch(phenotable, n=-1); # get all records
    dbDisconnect(pheno.db);
  }
  
  return(pheno.dic);
}

#'Utility function
# return matched KO in the same order (NA if no match)
doKOFiltering <- function(ko.vec, type){
  ko.dic <- .readDataTable("../../libs/network/ko_dic_new.csv");
  hit.inx <- match(ko.vec, ko.dic$KO);
  return(ko.dic$KO[hit.inx]);
}

#' Utility function for PrepareQueryJson
MapKO2KEGGEdges<- function(kos, net="ko01100"){
  if(!exists("ko.edge.map")){
    ko.edge.path <- paste("../../libs/network/ko_edge.csv", sep="");
    ko.edge.map <<- .readDataTable(ko.edge.path);     
  } 
  all.hits <- ko.edge.map$gene %in% names(kos) & ko.edge.map$net == net;
  my.map <- ko.edge.map[all.hits, ];
  q.map <- data.frame(gene=names(kos), expr=as.numeric(kos));
  
  # first merge to get ko abundance to each edge
  dat <- merge(my.map, q.map, by="gene");
  
  # now merge duplicated edge to sum
  dup.inx <- duplicated(dat[,2]);
  dat <- dat[!dup.inx,];
  rownames(dat) <- dat[,2];
  
  return(dat[,-2]);
}

#' Utility function for PrepareQueryJson
MapCmpd2KEGGNodes<- function(cmpds, net="ko01100"){
  
  lib <- "hsa_kegg" # TO-DO: change for other species
  if(!exists("ko.node.map.global")){
    # Read original library files for a list of pathways with assigned compounds to each
    pathway.lib <- readRDS(paste("../../libs/mummichog/", lib, ".rds", sep=""));
    pathways <- pathway.lib$pathways;
    
    # Store universe for enrichment analysis
    names(pathways$cpds) <- pathways$name
    current.cmpd.set <<- pathways$cpds;
    
    # Read pathway names and ids in the target pathway map (e.g. ko01100)
    ko.pathway.names <- .readDataTable(paste("../../libs/network/ko01100_compounds_ids.csv", sep=""));
    #ko.node.map <- do.call(rbind, lapply(1:length(pathways$name), function(i) cbind(unlist(pathways$cpds[i]), pathways$name[i])));
    #ko.node.matches <- ko.pathway.names[match(ko.node.map[,2], ko.pathway.names$name),2]
    # Replace pathway names with ids
    #ko.node.map[,2] <- ko.node.matches
    # Clean missing cases
    #ko.node.map <- ko.node.map[!is.na(ko.node.matches),]
    #ko.node.map.global <<- data.frame(cmpd = ko.node.map[,1], edge = ko.node.map[,2], net = rep("ko01100", nrow(ko.node.map)))
    ko.node.map.global <<- data.frame(cmpd = ko.pathway.names[,1], edge = ko.pathway.names[,2], net = rep("ko01100", nrow(ko.pathway.names)))
  }
  
  all.hits <- ko.node.map.global$cmpd %in% names(cmpds) & ko.node.map.global$net == net;
  my.map <- ko.node.map.global[all.hits, ];
  q.map <- data.frame(cmpd=names(cmpds), expr=as.numeric(cmpds));
  
  # first merge to get cmpd abundance to each edge
  dat <- merge(my.map, q.map, by="cmpd");
  
  # now merge duplicated edge to sum
  dup.inx <- duplicated(dat[,2]);
  dat <- dat[!dup.inx,];
  rownames(dat) <- dat[,2];
  
  return(dat[,-2]);
}

#' Utility function for PrepareQueryJson
# geneIDs is text one string, need to make to vector
PerformMapping <- function(inputIDs, type){
  
  dataSet <- list();
  dataSet$orig <- inputIDs;
  
  data.mat <- as.matrix(inputIDs);
  
  if(dim(data.mat)[2] == 1){ # add 1
    data.only <- 1; # if only a list of ids are provided with abundance or confidence scores
    data.mat <- cbind(data.mat, rep(1, nrow(data.mat)));
  }else {
    data.only <- 0;
    data.mat <- data.mat[,1:2];
  }
  
  # Hanlde case when only 1 id is provided
  if(!is.matrix(data.mat)){
    data.mat <- as.matrix(t(data.mat))
  }
  
  rownames(data.mat) <- data.mat[,1];
  data.mat <- data.mat[,-1, drop=F];
  dataSet$id.orig <- data.mat;
  dataSet$data.only <- data.only;
  data.mat <- RemoveDuplicates(data.mat, "sum", quiet=F); 
  dataSet$id.uniq <- data.mat;
  
  # now get input that are in the lib
  if(type == "ko"){
    kos <-  doKOFiltering(rownames(data.mat), type);
    if(sum(!is.na(kos)) < 2){
      AddErrMsg("Less than two hits found in the database. ");
      dataSet <- list();
      return(dataSet);
    }
    rownames(data.mat) <- kos;
    gd.inx <- (!is.na(kos)) & data.mat[,1] > -Inf; # TO-DO: change -Inf to specific value based on type of uploaded scores
    data.mat <- data.mat[gd.inx, ,drop=F];
    
    AddMsg(paste("A total of unqiue", nrow(data.mat), "KO genes were mapped to KEGG network!"));
  }
  # TO-DO: check if there is a need to do compound filtering here (e.g. doKeggCmpdFiltering)
  
  dataSet$id.mapped <- dataSet$data <- data.mat;
  
  return(dataSet);
}

#' Utility function for PrepareQueryJson
MergeDatasets <- function(dataSet1, dataSet2){
  
  dataSet <- list();
  dataSet$orig <- c(dataSet1$orig, dataSet2$orig);
  dataSet$id.orig <- rbind(dataSet1$id.orig, dataSet2$id.orig)
  dataSet$id.uniq <- rbind(dataSet1$id.uniq, dataSet2$id.uniq)
  dataSet$data <- rbind(dataSet1$data, dataSet2$data)
  dataSet$id.mapped <- rbind(dataSet1$id.mapped, dataSet2$id.mapped)
  
  return(dataSet);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'@export
GetNetworkGeneMappingResultTable<-function(mSetObj=NA){
  
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
  
  db.path <- paste("../../libs/", pathinteg.org, "/entrez.csv", sep="");
  gene.db <- .readDataTable(db.path);
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
  
  write.csv(csv.res, file="gene_name_map.csv", row.names=F);
  
  if(.on.public.web){
    .set.mSet(mSetObj)  
    return(as.vector(html.res));
  }else{
    return(.set.mSet(mSetObj));
  }
}

PrepareNetwork <- function(net.nm, json.nm){
  
  #my.ppi <- ppi.comps[[net.nm]];
  #nd.nms <- V(my.ppi)$name;
  #GeneAnotDB <- doProteinIDMappingNetwork(nd.nms, "entrez");
  
  #entrezIDs <- GeneAnotDB[,1];
  #names(entrezIDs) <- nd.nms;
  #current.anot <<- entrezIDs;
  
  convertIgraph2JSON(net.nm, json.nm);
  current.net.nm <<- net.nm;
  return(1);
}

# from node ID (uniprot) return entrez IDs (one to many)
GetNodeEntrezIDs <- function(uniprotID){
  enIDs <- current.anot[uniprotID];
  enIDs <- paste(enIDs, collapse="||");
  enIDs;
}

GetNodeEmblEntrezIDs <- function(emblprotein){
  enIDs <- current.anot[emblprotein];
  enIDs <- paste(enIDs, collapse="||");
  enIDs;
}

GetNodeIDs <- function(){
  V(overall.graph)$name;
}

GetNodeNames <- function(){
  V(overall.graph)$Label;
}

GetNodeDegrees <- function(){
  degree(overall.graph);
}

GetNodeBetweenness <- function(){
  round(betweenness(overall.graph, directed=F, normalized=F), 2);
}

DecomposeGraph <- function(gObj, minNodeNum = 3){
  # now decompose to individual connected subnetworks
  comps <-decompose.graph(gObj, min.vertices=minNodeNum);
  if(length(comps) == 0){
    current.msg <<- paste("No subnetwork was identified with at least", minNodeNum, "nodes!");
    return(NULL);
  }
  
  # first compute subnet stats
  net.stats <- ComputeSubnetStats(comps);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  comps <- comps[ord.inx];
  names(comps) <- rownames(net.stats) <- paste("subnetwork", 1:length(comps), sep="");
  
  # note, we report stats for all nets (at least 3 nodes);
  hit.inx <- net.stats$Node >= minNodeNum;
  comps <- comps[hit.inx];
  sub.stats <- NULL;
  i <- 0;
  for(nm in names(comps)){
    i <- i + 1;
    g <- comps[[nm]];
    saveNetworkInSIF(g, nm)
    nodeList <- get.data.frame(g, "vertices");
    colnames(nodeList) <- c("Id", "Label");
    ndFileNm = paste(nm, "_node_list.csv", sep="");
    write.csv(nodeList, file=ndFileNm, row.names=F, quote=F);
    
    edgeList <- get.data.frame(g, "edges");
    edgeList <- cbind(rownames(edgeList), edgeList);
    colnames(edgeList) <- c("Id", "Source", "Target");
    edgFileNm = paste(nm, "_edge_list.csv", sep="");
    write.csv(edgeList, file=edgFileNm, row.names=F, quote=F);
    
    sub.stats <- c(sub.stats, nrow(nodeList));   
  }
  
  # now record
  ppi.comps <<- comps;
  net.stats <<- net.stats;
  
  # set up the cache for layout reuse
  #if(!exists("PerformLayOut_mem")){
  #    require("memoise");
  #    PerformLayOut_mem<<- memoise(PerformLayOut, ~timeout(300)); # the cache will be empty after 5 min
  #}else{
  #    forget(PerformLayOut_mem); # clear cache
  #}
  
  return(sub.stats);
}

ComputeSubnetStats <- function(comps){
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  colnames(net.stats) <- c("Node", "Edge", "Query");
  for(i in 1:length(comps)){
    g <- comps[[i]];
    net.stats[i,] <- c(vcount(g),ecount(g),sum(seed.proteins %in% V(g)$name));
  }
  return(net.stats);
}

UpdateSubnetStats <- function(){
  old.nms <- names(ppi.comps);
  net.stats <- ComputeSubnetStats(ppi.comps);
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  rownames(net.stats) <- old.nms[ord.inx];
  net.stats <<- net.stats;
}

GetNetsName <- function(){
  rownames(net.stats);
}

GetNetsNameString <- function(){
  paste(rownames(net.stats), collapse="||");
}

GetNetsEdgeNum <- function(){
  as.numeric(net.stats$Edge);
}

GetNetsNodeNum <- function(){
  as.numeric(net.stats$Node);
}

GetNetsQueryNum <- function(){
  as.numeric(net.stats$Query);
}

# from to should be valid nodeIDs
GetShortestPaths <- function(from, to){
  current.net <- ppi.comps[[current.net.nm]];
  paths <- get.all.shortest.paths(current.net, from, to)$res;
  if(length(paths) == 0){
    return (paste("No connection between the two nodes!"));
  }
  
  path.vec <- vector(mode="character", length=length(paths));
  for(i in 1:length(paths)){
    path.inx <- paths[[i]]; 
    path.ids <- V(current.net)$name[path.inx];
    path.sybls <- path.ids;
    pids <- paste(path.ids, collapse="->");
    psbls <- paste(path.sybls, collapse="->");
    path.vec[i] <- paste(c(pids, psbls), collapse=";")
  }
  
  if(length(path.vec) > 50){
    path.vec <- path.vec[1:50];
  }
  
  all.paths <- paste(path.vec, collapse="||");
  return(all.paths);
}

# exclude nodes in current.net (networkview)
ExcludeNodes <- function(nodeids, filenm){
  nodes2rm <- strsplit(nodeids, ";")[[1]];
  current.net <- ppi.comps[[current.net.nm]];
  current.net <- delete.vertices(current.net, nodes2rm);
  
  # need to remove all orphan nodes
  bad.vs<-V(current.net)$name[degree(current.net) == 0];
  current.net <- delete.vertices(current.net, bad.vs);
  
  # return all those nodes that are removed 
  nds2rm <- paste(c(bad.vs, nodes2rm), collapse="||");
  
  # update topo measures
  node.btw <- as.numeric(betweenness(current.net));
  node.dgr <- as.numeric(degree(current.net));
  node.exp <- as.numeric(get.vertex.attribute(current.net, name="abundance", index = V(current.net)));
  nms <- V(current.net)$name;
  hit.inx <- match(nms, ppi.net$node.data[,1]);
  lbls <- ppi.net$node.data[hit.inx,2];
  
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i], 
      label=lbls[i],
      degree=node.dgr[i], 
      between=node.btw[i],
      expr = node.exp[i]
    );
  }
  # now only save the node pos to json
  require(RJSONIO);
  netData <- list(deletes=nds2rm,nodes=nodes);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  
  ppi.comps[[current.net.nm]] <<- current.net;
  UpdateSubnetStats();
  
  # remember to forget the cached layout, and restart caching, as this is now different object (with the same name)
  #forget(PerformLayOut_mem);
  return(filenm);
}

# support walktrap, infomap and lab propagation
FindCommunities <- function(method="walktrap", use.weight=FALSE){
  
  # make sure this is the connected
  current.net <- ppi.comps[[current.net.nm]];
  g <- current.net;
  if(!is.connected(g)){
    g <- decompose.graph(current.net, min.vertices=2)[[1]];
  }
  total.size <- length(V(g));
  
  if(use.weight){ # this is only tested for walktrap, should work for other method
    # now need to compute weights for edges
    egs <- get.edges(g, E(g)); #node inx
    nodes <- V(g)$name;
    # conver to node id
    negs <- cbind(nodes[egs[,1]],nodes[egs[,2]]);
    
    # get min FC change
    base.wt <- min(abs(seed.expr))/10;
    
    # check if user only give a gene list without logFC or all same fake value
    if(length(unique(seed.expr)) == 1){
      seed.expr <- rep(1, nrow(negs));
      base.wt <- 0.1; # weight cannot be 0 in walktrap
    }
    
    wts <- matrix(base.wt, ncol=2, nrow = nrow(negs));
    for(i in 1:ncol(negs)){
      nd.ids <- negs[,i];
      hit.inx <- match(names(seed.expr), nd.ids);
      pos.inx <- hit.inx[!is.na(hit.inx)];
      wts[pos.inx,i]<- seed.expr[!is.na(hit.inx)]+0.1;
    }
    nwt <- apply(abs(wts), 1, function(x){mean(x)^2})    
  }
  
  if(method == "walktrap"){
    fc <- walktrap.community(g);
  }else if(method == "infomap"){
    fc <- infomap.community(g);
  }else if(method == "labelprop"){
    fc <- label.propagation.community(g);
  }else{
    print(paste("Unknown method:", method));
    return ("NA||Unknown method!");
  }
  
  if(length(fc) == 0 || modularity(fc) == 0){
    return ("NA||No communities were detected!");
  }
  
  # only get communities
  communities <- communities(fc);
  community.vec <- vector(mode="character", length=length(communities));
  gene.community <- NULL;
  qnum.vec <- NULL;
  pval.vec <- NULL;
  rowcount <- 0;
  nms <- V(g)$name;
  hit.inx <- match(nms, ppi.net$node.data[,1]);
  sybls <- ppi.net$node.data[hit.inx,2];
  names(sybls) <- V(g)$name;
  for(i in 1:length(communities)){
    # update for igraph 1.0.1 
    path.ids <- communities[[i]];
    psize <- length(path.ids);
    if(psize < 5){
      next; # ignore very small community
    }
    hits <- seed.proteins %in% path.ids;
    qnums <- sum(hits);
    if(qnums == 0){
      next; # ignor community containing no queries
    }
    
    rowcount <- rowcount + 1;
    pids <- paste(path.ids, collapse="->");
    #path.sybls <- V(g)$Label[path.inx];
    path.sybls <- sybls[path.ids];
    com.mat <- cbind(path.ids, path.sybls, rep(i, length(path.ids)));
    gene.community <- rbind(gene.community, com.mat);
    qnum.vec <- c(qnum.vec, qnums);
    
    # calculate p values (comparing in- out- degrees)
    #subgraph <- induced.subgraph(g, path.inx);
    subgraph <- induced.subgraph(g, path.ids);
    in.degrees <- degree(subgraph);
    #out.degrees <- degree(g, path.inx) - in.degrees;
    out.degrees <- degree(g, path.ids) - in.degrees;
    ppval <- wilcox.test(in.degrees, out.degrees)$p.value;
    ppval <- signif(ppval, 3);
    pval.vec <- c(pval.vec, ppval);
    
    # calculate community score
    community.vec[rowcount] <- paste(c(psize, qnums, ppval, pids), collapse=";");
  }
  
  ord.inx <- order(pval.vec, decreasing=F);
  community.vec <- community.vec[ord.inx];
  qnum.vec <- qnum.vec[ord.inx];
  ord.inx <- order(qnum.vec, decreasing=T);
  community.vec <- community.vec[ord.inx];
  
  all.communities <- paste(community.vec, collapse="||");
  colnames(gene.community) <- c("Id", "Label", "Module");
  write.csv(gene.community, file="module_table.csv", row.names=F);
  return(all.communities);
}

community.significance.test <- function(graph, vs, ...) {
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

###################################
# Adapted from netweavers package
###################

convertIgraph2JSON <- function(net.nm, filenm){
  
  g <- ppi.comps[[net.nm]];
  # annotation
  nms <- V(g)$name;
  hit.inx <- match(nms, ppi.net$node.data[,1]);
  lbls <- ppi.net$node.data[hit.inx,2];
  gene.names <- ppi.net$node.data[hit.inx,3];
  
  # get edge data
  edge.mat <- get.edgelist(g);
  edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2]);
  
  # now get coords
  #pos.xy <- PerformLayOut_mem(net.nm, "Default");
  pos.xy <- PerformLayOut(net.nm, "Default");
  # get the note data
  node.btw <- as.numeric(betweenness(g));
  node.dgr <- as.numeric(degree(g));
  node.exp <- as.numeric(get.vertex.attribute(g, name="abundance", index = V(g)));
  
  # node size to degree values
  if(vcount(g)>500){
    min.size = 1;
  }else if(vcount(g)>200){
    min.size = 2;
  }else{
    min.size = 3;
  }
  node.sizes <- as.numeric(rescale2NewRange((log(node.dgr))^2, min.size, 9));
  centered = T;
  notcentered = F;
  # update node color based on betweenness
  require("RColorBrewer");
  topo.val <- log(node.btw+1);
  topo.colsb <- ComputeColorGradient(topo.val, "black", notcentered);
  topo.colsw <-  ComputeColorGradient(topo.val, "white", notcentered);
  
  # color based on expression
  bad.inx <- is.na(node.exp) | node.exp==0;
  if(!all(bad.inx)){
    exp.val <- node.exp;
    node.colsb.exp <- ComputeColorGradient(exp.val, "black", centered); 
    node.colsw.exp <- ComputeColorGradient(exp.val, "white", centered);
    node.colsb.exp[bad.inx] <- "#d3d3d3"; 
    node.colsw.exp[bad.inx] <- "#c6c6c6"; 
    # node.colsw.exp[bad.inx] <- "#66CCFF";
  }else{
    node.colsb.exp <- rep("#d3d3d3",length(node.exp)); 
    node.colsw.exp <- rep("#c6c6c6",length(node.exp)); 
  }
  
  if(table.nm == "global"){
    # now update for bipartite network
    # setup shape (gene circle, other squares)
    # Circles for genes
    shapes <- rep("circle", length(nms));
    # Squares for phenotypes
    mir.inx <- nms %in% edge.mat[,"target"];
    shapes[mir.inx] <- "square";
    # Diamond for metabolites
    cmpds.node <- as.vector(sapply(nms, function(x) substr(x, 1, 1) == "C"))
    shapes[cmpds.node] <- "diamond"
    node.sizes[mir.inx] <- node.sizes[mir.inx] + 0.5;
    # update mir node color
    node.colsw.exp[mir.inx] <- topo.colsw[mir.inx] <- "#306EFF"; # dark blue
    node.colsb.exp[mir.inx] <- topo.colsb[mir.inx] <- "#98F5FF";
    
  } else {
    # now update for bipartite network
    # setup shape (gene circle, other squares)
    shapes <- rep("circle", length(nms));
    if(ppi.net$db.type != 'ppi' && table.nm != "metabo_metabolites"){ # the other part miRNA or TF will be in square
      mir.inx <- nms %in% edge.mat[,"target"];
      shapes[mir.inx] <- "square";
      node.sizes[mir.inx] <- node.sizes[mir.inx] + 0.5;
      
      # update mir node color
      node.colsw.exp[mir.inx] <- topo.colsw[mir.inx] <- "#306EFF"; # dark blue
      node.colsb.exp[mir.inx] <- topo.colsb[mir.inx] <- "#98F5FF";
    }
  }
  
  # now create the json object
  nodes <- vector(mode="list");
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id=nms[i],
      idnb = i, 
      label=lbls[i],
      genename=gene.names[i],
      x = pos.xy[i,1],
      y = pos.xy[i,2],
      size=node.sizes[i], 
      type=shapes[i],
      colorb=topo.colsb[i],
      colorw=topo.colsw[i],
      attributes=list(
        expr = node.exp[i],
        expcolb=node.colsb.exp[i],
        expcolw=node.colsw.exp[i],
        degree=node.dgr[i], 
        between=node.btw[i])
    );
  }
  
  # save node table
  nd.tbl <- data.frame(Id=nms, Label=lbls, Degree=node.dgr, Betweenness=round(node.btw,2));
  # order 
  ord.inx <- order(nd.tbl[,3], nd.tbl[,4], decreasing = TRUE)
  nd.tbl <- nd.tbl[ord.inx, ];
  write.csv(nd.tbl, file="node_table.csv", row.names=FALSE);
  
  # covert to json
  require(RJSONIO);
  netData <- list(nodes=nodes, edges=edge.mat);
  sink(filenm);
  cat(toJSON(netData));
  sink();
}

# also save to GraphML
ExportNetwork <- function(fileName){
  current.net <- ppi.comps[[current.net.nm]];
  write.graph(current.net, file=fileName, format="graphml");
}

ExtractModule<- function(nodeids){
  set.seed(8574);
  nodes <- strsplit(nodeids, ";")[[1]];
  
  g <- ppi.comps[[current.net.nm]];
  # try to see if the nodes themselves are already connected
  hit.inx <- V(g)$name %in% nodes; 
  gObj <- induced.subgraph(g, V(g)$name[hit.inx]);
  
  # now find connected components
  comps <-decompose.graph(gObj, min.vertices=1);
  
  if(length(comps) == 1){ # nodes are all connected
    g <- comps[[1]];
  }else{
    # extract modules
    paths.list <-list();
    sd.len <- length(nodes);
    for(pos in 1:sd.len){
      paths.list[[pos]] <- get.shortest.paths(g, nodes[pos], nodes[-(1:pos)])$vpath;
    }
    nds.inxs <- unique(unlist(paths.list));
    nodes2rm <- V(g)$name[-nds.inxs];
    g <- simplify(delete.vertices(g, nodes2rm));
  }
  nodeList <- get.data.frame(g, "vertices");
  if(nrow(nodeList) < 3){
    return ("NA");
  }
  
  module.count <- module.count + 1;
  module.nm <- paste("module", module.count, sep="");
  colnames(nodeList) <- c("Id", "Label");
  ndFileNm = paste(module.nm, "_node_list.csv", sep="");
  write.csv(nodeList, file=ndFileNm, row.names=F, quote=F);
  
  edgeList <- get.data.frame(g, "edges");
  edgeList <- cbind(rownames(edgeList), edgeList);
  colnames(edgeList) <- c("Id", "Source", "Target");
  edgFileNm = paste(module.nm, "_edge_list.csv", sep="");
  write.csv(edgeList, file=edgFileNm, row.names=F, quote=F);
  
  filenm <- paste(module.nm, ".json", sep="");
  
  # record the module 
  ppi.comps[[module.nm]] <<- g;
  UpdateSubnetStats();
  
  module.count <<- module.count
  
  convertIgraph2JSON(module.nm, filenm);
  return (filenm);
}

PerformLayOut <- function(net.nm, algo){
  g <- ppi.comps[[net.nm]];
  vc <- vcount(g);
  if(algo == "Default"){
    if(vc > 3000) {
      pos.xy <- layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- layout.lgl(g, maxiter = 150);
    }else if(vc > 1000) {
      pos.xy <- layout.lgl(g, maxiter = 200);
    }else if(vc < 150){
      pos.xy <- layout.kamada.kawai(g);
    }else{
      pos.xy <- layout.fruchterman.reingold(g);
    }
  }else if(algo == "FrR"){
    pos.xy <- layout.fruchterman.reingold(g);
  }else if(algo == "random"){
    pos.xy <- layout.random(g);
  }else if(algo == "lgl"){
    if(vc > 3000) {
      pos.xy <- layout.lgl(g, maxiter = 100);
    }else if(vc > 2000) {
      pos.xy <- layout.lgl(g, maxiter = 150);
    }else {
      pos.xy <- layout.lgl(g, maxiter = 200);
    }
  }else if(algo == "gopt"){
    # this is a slow one
    if(vc > 3000) {
      maxiter = 50;
    }else if(vc > 2000) {
      maxiter = 100;
    }else if(vc > 1000) {
      maxiter = 200;
    }else{
      maxiter = 500;
    }
    pos.xy <- layout.graphopt(g, niter=maxiter);
  }
  pos.xy;
}

UpdateNetworkLayout <- function(algo, filenm){
  current.net <- ppi.comps[[current.net.nm]];
  #pos.xy <- PerformLayOut_mem(current.net.nm, algo);
  pos.xy <- PerformLayOut(current.net.nm, algo);
  nms <- V(current.net)$name;
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i], 
      x=pos.xy[i,1], 
      y=pos.xy[i,2]
    );
  }
  # now only save the node pos to json
  require(RJSONIO);
  netData <- list(nodes=nodes);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(filenm);
}

# adapted from BioNet
saveNetworkInSIF <- function(network, name){
  edges <- .graph.sif(network=network, file=name);
  sif.nm <- paste(name, ".sif", sep="");
  if(length(list.edge.attributes(network))!=0){
    edge.nms <- .graph.eda(network=network, file=name, edgelist.names=edges);
    sif.nm <- c(sif.nm, edge.nms);
    
  }
  if(length(list.vertex.attributes(network))!=0){
    node.nms <- .graph.noa(network=network, file=name);
    sif.nm <- c(sif.nm, node.nms);
  }
  # need to save all sif and associated attribute files into a zip file for download
  zip(paste(name,"_sif",".zip", sep=""), sif.nm);
}

# internal function to write cytoscape .sif file
.graph.sif <- function(network, file){
  edgelist.names1 <<- get.edgelist(network, names=TRUE)
  edgelist.names <- get.edgelist(network, names=TRUE)
  a <- matrix(doID2LabelMapping(as.vector(edgelist.names[,1])))
  b <- matrix(doID2LabelMapping(as.vector(edgelist.names[,2])))
  edgelist.names <- cbind(a, rep("pp", length(E(network))), b);    
  write.table(edgelist.names, row.names=FALSE, col.names=FALSE, file=paste(file, ".sif", sep=""), sep="\t", quote=FALSE)
  return(edgelist.names) 
}

doID2LabelMapping <- function(entrez.vec){
  
  hit.inx <- match(entrez.vec, nodeListu[, "Id"]);
  symbols <- nodeListu[hit.inx, "Label"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
} 

# internal method to write cytoscape node attribute files
.graph.noa <- function(network, file){
  all.nms <- c();
  attrib <- list.vertex.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    noa <- cbind(V(network)$name, rep("=", length(V(network))), get.vertex.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="")
    file.nm <- paste(file, "_", attrib[i], ".NA", sep="");
    write(first.line, file=file.nm, ncolumns = 1, append=FALSE, sep=" ")
    write.table(noa, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

# internal method to write cytoscape edge attribute files
.graph.eda <- function(network, file, edgelist.names){
  all.nms <- c();
  attrib <- list.edge.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.edge.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    eda <- cbind(cbind(edgelist.names[,1], rep("(pp)", length(E(network))), edgelist.names[,3]), rep("=", length(E(network))), get.edge.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="");
    file.nm <- paste(file, "_", attrib[i], ".EA", sep="");
    write(first.line, file=file.nm, ncolumns=1, append=FALSE, sep =" ")
    write.table(eda, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

# new range [a, b]
rescale2NewRange <- function(qvec, a, b){
  q.min <- min(qvec);
  q.max <- max(qvec);
  if(length(qvec) < 50){
    a <- a*2;
  }
  if(q.max == q.min){
    new.vec <- rep(8, length(qvec));
  }else{
    coef.a <- (b-a)/(q.max-q.min);
    const.b <- b - coef.a*q.max;
    new.vec <- coef.a*qvec + const.b;
  }
  return(new.vec);
}


# #FFFFFF to rgb(1, 0, 0)
hex2rgba <- function(cols){
  return(apply(sapply(cols, col2rgb), 2, function(x){paste("rgba(", x[1], ",", x[2], ",", x[3], ",0.8)", sep="")})); 
}

# re-arrange one vector elements according to another vector values
# usually src is character vector to be arranged
# target is numberic vector of same length
sync2vecs <- function(src.vec, tgt.vec){
  if(length(src.vec) != length(tgt.vec)){
    print("must be of the same length!");
    return();
  }
  ord.inx <- match(rank(tgt.vec, ties.method="random"), 1:length(tgt.vec));
  src.vec[ord.inx];
}

# given a data with duplicates, dups is the one with duplicates
RemoveDuplicatesNetwork <- function(data, lvlOpt, quiet=T){
  
  all.nms <- rownames(data);
  colnms <- colnames(data);
  dup.inx <- duplicated(all.nms);
  dim.orig  <- dim(data);
  data <- apply(data, 2, as.numeric); # force to be all numeric
  dim(data) <- dim.orig; # keep dimension (will lost when only one item) 
  rownames(data) <- all.nms;
  colnames(data) <- colnms;
  if(sum(dup.inx) > 0){
    uniq.nms <- all.nms[!dup.inx];
    uniq.data <- data[!dup.inx,,drop=F];
    
    dup.nms <- all.nms[dup.inx];
    uniq.dupnms <- unique(dup.nms);
    uniq.duplen <- length(uniq.dupnms);
    
    for(i in 1:uniq.duplen){
      nm <- uniq.dupnms[i];
      hit.inx.all <- which(all.nms == nm);
      hit.inx.uniq <- which(uniq.nms == nm);
      
      # average the whole sub matrix 
      if(lvlOpt == "mean"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, mean, na.rm=T);
      }else if(lvlOpt == "median"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, median, na.rm=T);
      }else if(lvlOpt == "max"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, max, na.rm=T);
      }else{ # sum
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, sum, na.rm=T);
      }
    }
    if(!quiet){
      current.msg <<- paste(current.msg, paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""), collapse="\n");
    }
    return(uniq.data);
  }else{
    if(!quiet){
      current.msg <<- paste(current.msg, "All IDs are unique.", collapse="\n");
    }
    return(data);
  }
} 

generate_breaks = function(x, n, center = F){
  if(center){
    m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
    res = seq(-m, m, length.out = n + 1)
  }
  else{
    res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
  }
  return(res)
}

ComputeColorGradient <- function(nd.vec, background="black", centered){
  require("RColorBrewer");
  #if(sum(nd.vec<0, na.rm=TRUE) > 0){ 
  #centered <- T;
  #}else{
  #centered <- F;
  #}
  color <- GetColorGradient(background, centered);
  breaks <- generate_breaks(nd.vec, length(color), center = centered);
  return(scale_vec_colours(nd.vec, col = color, breaks = breaks));
}

GetColorGradient <- function(background, center){
  if(background == "black"){
    if(center){
      return(c(colorRampPalette(c("#31A231", "#5BC85B", "#90EE90", "#C1FFC1"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)));
    }else{
      return(colorRampPalette(rev(heat.colors(9)))(100));
    }
  }else{ # white background
    if(center){
      return(c(colorRampPalette(c("#137B13", "#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313", "#96000D"))(50)));
    }else{
      # return(colorRampPalette(c("grey", "orange", "red", "darkred"))(100));
      # return(colorRampPalette(c("#80d0f0", rainbow(8, start=0.8, end=1)))(100));
      return(colorRampPalette(hsv(h = seq(0.72, 1, 0.035), s = 0.72, v = 1))(100));
    }
  }
}

scale_vec_colours = function(x, col = rainbow(10), breaks = NA){
  breaks <- sort(unique(breaks));
  return(col[as.numeric(cut(x, breaks = breaks, include.lowest = T))])
}
