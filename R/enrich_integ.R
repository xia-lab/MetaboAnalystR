#'Perform compound mapping for integrative analysis methods
#'@description Perform compound mapping
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PerformIntegCmpdMapping <- function(mSetObj=NA, cmpdIDs, org, idType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$msgSet$current.msg <- NULL;
  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
  if(idType == "name"){
    cmpd.mat <- getDataFromTextInput(mSetObj, cmpdIDs, "tab");
  }else{ # all other id should not contains space
    cmpd.mat <- getDataFromTextInput(mSetObj, cmpdIDs, "space");
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

PerformIntegGeneMapping <- function(mSetObj=NA, geneIDs, org, idType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$msgSet$current.msg <- NULL;
  
  mSetObj$dataSet$q.type.gene <- idType;
  mSetObj$dataSet$gene.orig <- geneIDs;
  mSetObj$dataSet$gene.org <- org;
  gene.mat <- getDataFromTextInput(mSetObj, geneIDs);
  gene.vec <- rownames(gene.mat);
  mSetObj$dataSet$gene.mat <- gene.mat;
  mSetObj$dataSet$gene <- gene.vec;
  
  enIDs <- doGeneIDMapping(gene.vec, org, idType);
  mSetObj$dataSet$gene.name.map <- list();
  mSetObj$dataSet$gene.name.map$hit.values <- enIDs;
  mSetObj$dataSet$gene.name.map$match.state <- ifelse(is.na(enIDs), 0, 1);
  
  mSetObj$msgSet$current.msg <- c(paste("A total of ", length(unique(enIDs)), "unique genes were uploaded."));
  na.inx <- is.na(enIDs);
  if(sum(!na.inx) > 0){
    
    print("All genes were successfully mapped...")
    if(.on.public.web){
      .set.mSet(mSetObj)  
      return(1);
    }
    return(.set.mSet(mSetObj));
  }else{
    
    print("1 or more genes were unsuccessfully mapped...")
    if(.on.public.web){
      .set.mSet(mSetObj)  
      return(0);
    }
    return(.set.mSet(mSetObj));
  }
}

RemoveCmpd <- function(mSetObj=NA, inx){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$cmpd <- mSetObj$dataSet$cmpd[-inx];
  
  mSetObj$name.map$hit.inx <- mSetObj$name.map$hit.inx[-inx];
  mSetObj$name.map$hit.values <- mSetObj$name.map$hit.values[-inx];
  mSetObj$name.map$match.state <- mSetObj$name.map$match.state[-inx];
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    mSetObj$name.map
  }
  return(.set.mSet(mSetObj));
}

RemoveGene <- function(mSetObj=NA, inx){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$gene <- mSetObj$dataSet$gene[-inx];
  mSetObj$dataSet$gene.mat <- mSetObj$dataSet$gene.mat[-inx, ,drop=F];
  
  mSetObj$dataSet$gene.name.map$hit.inx <- mSetObj$dataSet$gene.name.map$hit.inx[-inx];
  mSetObj$dataSet$gene.name.map$hit.values <- mSetObj$dataSet$gene.name.map$hit.values[-inx];
  mSetObj$dataSet$gene.name.map$match.state <- mSetObj$dataSet$gene.name.map$match.state[-inx];
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    mSetObj$dataSet$gene.name.map
  }
  return(.set.mSet(mSetObj));
}

PrepareIntegData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  done <- 0;
  # prepare gene list
  if(!is.null(mSetObj$dataSet$gene.mat)){
    gene.mat <- mSetObj$dataSet$gene.mat;
    enIDs <- mSetObj$dataSet$gene.name.map$hit.values;
    rownames(gene.mat) <- enIDs;
    
    na.inx <- is.na(enIDs);
    gene.mat <- gene.mat[!na.inx, ,drop=F];
    gene.mat <- RemoveDuplicates(mSetObj, gene.mat);
    mSetObj$msgSet$current.msg <- c(paste("A total of ", nrow(gene.mat), "unique genes were uploaded."));
    
    if(!exists("inmex.imps", where = mSetObj$dataSet)){
      mSetObj$dataSet$inmex.imps <- list();
    }
    mSetObj$dataSet$inmex.imps$gene.mat <- gene.mat;
    .set.mSet(mSetObj);  
    done <- 1;
    
  }
  
  if(.on.public.web){
    .get.mSet(mSetObj);
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
    cmpd.mat <- RemoveDuplicates(mSetObj, cmpd.mat);
    mSetObj$msgSet$current.msg <- c(mSetObj$msgSet$current.msg, paste("A total of ", nrow(cmpd.mat), "unique compounds were found."));
    mSetObj$dataSet$inmex.imps$cmpd.mat <- cmpd.mat;
    done <- 1;
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj);  
    print("prep integ data")
    return(done);
  }
  return(.set.mSet(mSetObj));
  
}

#'Perform integrative pathway analysis 
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

PerformIntegPathwayAnalysis <- function(mSetObj=NA, topo="dc", enrich="hyper", libOpt="integ"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  LoadKEGGLib(mSetObj=NA, libOpt);
  
  if(.on.public.web){
    mSetObj <- .get.mSet(mSetObj);
  }  
  
  set.size <- length(inmexpa$mset.list);
  ms.list <- mSetObj$dataSet$current.mset.list;
  
  # prepare for the result table
  res.mat<-matrix(0, nrow=set.size, ncol=7);
  rownames(res.mat)<-names(inmexpa$path.ids);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "Topology", "PVal.Z",  "Topo.Z");
  
  mSetObj$dataSet$inmex.method <- libOpt;
  mSetObj$dataSet$path.mat <- NULL;
  
  if(libOpt == "genetic" && !is.null(mSetObj$dataSet$inmex.imps$gene.mat)){
    gene.sbls <- doEntrez2SymbolMapping(rownames(mSetObj$dataSet$inmex.imps$gene.mat));
    mSetObj$dataSet$inmex.imps$gene.mat <- cbind(Name=gene.sbls, mSetObj$dataSet$inmex.imps$gene.mat);
    gene.vec <- paste(inmex.org, ":", rownames(mSetObj$dataSet$inmex.imps$gene.mat), sep="");
    rownames(mSetObj$dataSet$inmex.imps$gene.mat) <- gene.vec;
    write.csv(mSetObj$dataSet$inmex.imps$gene.mat, file="MetaboAnalyst_result_genes.csv");
    
    impMat <- mSetObj$dataSet$inmex.imps$gene.mat;
    uniq.count <- inmexpa$uniq.gene.count;
    uniq.len <- inmexpa$gene.counts;
    
  }else if(libOpt == "metab" && !is.null(mSetObj$dataSet$inmex.imps$cmpd.mat)){
    cmpd.nms <- doKEGG2NameMapping(rownames(mSetObj$dataSet$inmex.imps$cmpd.mat));
    mSetObj$dataSet$inmex.imps$cmpd.mat <- cbind(Name=cmpd.nms, mSetObj$dataSet$inmex.imps$cmpd.mat);
    cmpd.vec <- paste("cpd:", rownames(mSetObj$dataSet$inmex.imps$cmpd.mat), sep="");
    rownames(mSetObj$dataSet$inmex.imps$cmpd.mat) <- cmpd.vec;
    write.csv(mSetObj$dataSet$inmex.imps$cmpd.mat, file="MetaboAnalyst_result_cmpds.csv");
    
    impMat <- mSetObj$dataSet$inmex.imps$cmpd.mat;
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;
  }else{ # integ
    impMat <- NULL;
    uniq.count <- uniq.len <- 0;
    if(!is.null(mSetObj$dataSet$inmex.imps$cmpd.mat)){
      cmpd.nms <- doKEGG2NameMapping(rownames(mSetObj$dataSet$inmex.imps$cmpd.mat));
      mSetObj$dataSet$inmex.imps$cmpd.mat <- cbind(Name=cmpd.nms, mSetObj$dataSet$inmex.imps$cmpd.mat);
      cmpd.vec <- paste("cpd:", rownames(mSetObj$dataSet$inmex.imps$cmpd.mat), sep="");
      rownames(mSetObj$dataSet$inmex.imps$cmpd.mat) <- cmpd.vec;
      write.csv(mSetObj$dataSet$inmex.imps$cmpd.mat, file="MetaboAnalyst_result_cmpds.csv");
      impMat <- mSetObj$dataSet$inmex.imps$cmpd.mat;
      uniq.count <- inmexpa$uniq.cmpd.count
      uniq.len <- inmexpa$cmpd.counts;
      
    }
    if(!is.null(mSetObj$dataSet$inmex.imps$gene.mat)){
      gene.sbls <- doEntrez2SymbolMapping(rownames(mSetObj$dataSet$inmex.imps$gene.mat));
      mSetObj$dataSet$inmex.imps$gene.mat <- cbind(Name=gene.sbls, mSetObj$dataSet$inmex.imps$gene.mat);
      gene.vec <- paste(inmex.org, ":", rownames(mSetObj$dataSet$inmex.imps$gene.mat), sep="");
      rownames(mSetObj$dataSet$inmex.imps$gene.mat) <- gene.vec;
      write.csv(mSetObj$dataSet$inmex.imps$gene.mat, file="MetaboAnalyst_result_genes.csv");
      impMat <- rbind(impMat, mSetObj$dataSet$inmex.imps$gene.mat);
      uniq.count <- uniq.count + inmexpa$uniq.gene.count;
      uniq.len <- uniq.len + inmexpa$gene.counts;
      
    }
  }
  
  # now project to pathways
  # combine results for genes and cmpds
  ora.vec <- rownames(impMat);
  colnames(impMat) <- c("Name", "logFC");
  impMat <- data.frame(Name=impMat[,1], logFC=as.numeric(impMat[,2]));
  rownames(impMat) <- ora.vec;
  
  # need to cut to the universe covered by the pathways, not all genes 
  ora.vec <- ora.vec[ora.vec %in% mSetObj$dataSet$current.universe]
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
    AddErrMsg(mSetObj, "No hits found for your input!");
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
  
  res.mat <- res.mat[hit.num>0,];
  ord.inx<-order(res.mat[,4], res.mat[,5]);
  
  res.mat <- signif(res.mat[ord.inx,,drop=FALSE],5);
  #res.mat <- data.frame(res.mat);
  
  #get gene symbols
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  
  # now save to different formats
  # csv
  write.csv(resTable, file="MetaboAnalyst_result_pathway.csv", row.names=F);
  
  mSetObj$dataSet$path.hits <- hits.path;
  
  # store results from individual analysis
  mSetObj$dataSet$inmex.impMat <- impMat; 
  mSetObj$dataSet$inmex.impTopo <- imp.list;
  mSetObj$dataSet$path.mat <- resTable;
  
  if(.on.public.web){
    mSetObj <- .set.mSet(mSetObj);
  } 
  
  SetBarParams(mSetObj);
  
  mSetObj <- .get.mSet(mSetObj);
  
  
  if(.on.public.web){
    .set.mSet(mSetObj)  
    return(1);
  }
  return(.set.mSet(mSetObj));
}

#'Plot a scatterplot bubble chart overview of the matched pathways
#'@description x axis is the pathway impact factor
#'y axis is the p value (from ORA) 
#'return the circle information
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

SetBarParams <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  y <-  mSetObj$dataSet$path.mat$Topology;
  x <-  mSetObj$dataSet$path.mat$P.Value;
  x = -log(x);
  
  x <- scale(x);
  y <- scale(y);
  base <- abs(min(c(x,y)));
  
  x <- x + base;
  y <- y + base;
  # names(y) <-  rownames(path.mat);
  
  # set circle size according to
  # sum of p and topo (since they
  # alrealy bring to same range
  
  # we do twice to reduce difference for plotting
  radi.vec <- sqrt(x+y);
  
  resTable <- data.frame(x=x, y=y); 
  rownames(resTable) <- rownames(mSetObj$dataSet$path.mat);
  
  # display only top 100 sorted by p
  if(nrow(resTable) > 20){
    resTable <- resTable[1:20,];
  }
  
  resTable <- resTable[nrow(resTable):1,];
  mSetObj$dataSet$bar.data <- resTable;
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetBarParams<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(as.matrix(mSetObj$dataSet$bar.data));
}

GetBarNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # single quote apostrophe caused trouble 
  return(rownames(mSetObj$dataSet$bar.data));
}

GetIntegResultPathIDs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  inmexpa$path.ids[rownames(mSetObj$dataSet$path.mat)];
}

GetIntegResultPathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$dataSet$path.mat));
}

GetIntegResultColNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$path.mat)[-1]);
}

GetIntegResultMatrix<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(as.matrix(mSetObj$dataSet$path.mat[,-1]));
}

GetGeneHitsRowNumber<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(length(mSetObj$dataSet$gene.name.map$match.state));
}

GetGeneMappingResultTable<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  qvec <- mSetObj$dataSet$gene;
  enIDs <- mSetObj$dataSet$gene.name.map$hit.values;
  
  # style for highlighted background for unmatched names
  pre.style<-NULL;
  post.style<-NULL;
  
  # style for no matches
  if(mSetObj$dataSet$q.type.gene == "name"){
    no.prestyle<-"<strong style=\"background-color:yellow; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }else{
    no.prestyle<-"<strong style=\"background-color:red; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }
  
  # contruct the result table with cells wrapped in html tags
  # the unmatched will be highlighted in different background
  html.res<-matrix("", nrow=length(qvec), ncol=5);
  csv.res<-matrix("", nrow=length(qvec), ncol=5);
  colnames(csv.res)<-c("Query", "Entrez", "Symbol", "Name", "Comment");
  
  db.path <- paste("../../libs/", inmex.org, "/entrez.csv", sep="");
  gene.db <- .readDataTable(db.path);
  hit.inx <- match(enIDs, gene.db[, "gene_id"]);
  hit.values<-mSetObj$dataSet$gene.name.map$hit.values;
  match.state<-mSetObj$dataSet$gene.name.map$match.state;
  mSetObj$dataSet$gene.name.map$hit.inx <- hit.inx;
  
  for (i in 1:length(qvec)){
    if(match.state[i]==1){
      pre.style<-"";
      post.style="";
    }else{ # no matches
      pre.style<-no.prestyle;
      post.style<-no.poststyle;
    }
    hit <-gene.db[hit.inx[i], ,drop=F];
    
    html.res[i, ]<-c(paste(pre.style, qvec[i], post.style, sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$gene_id),"-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>",hit$gene_id,"</a>", sep="")),  sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$symbol), "-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>", hit$symbol,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$name),"-", paste("<a href=http://www.ncbi.nlm.nih.gov/gene/", hit$gene_id, " target='_blank'>",hit$name,"</a>", sep="")), sep=""),
                     ifelse(match.state[i]!=1,"View",""));
    csv.res[i, ]<-c(qvec[i],
                    ifelse(match.state[i]==0, "NA", hit$gene_id),
                    ifelse(match.state[i]==0, "NA", hit$symbol),
                    ifelse(match.state[i]==0, "NA", hit$name),
                    match.state[i]);
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
