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
PerformIntegCmpdMapping <- function(mSetObj=NA, cmpdIDs, org, idType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
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
PerformIntegGeneMapping <- function(mSetObj=NA, geneIDs, org, idType){
  
  mSetObj <- .get.mSet(mSetObj);
  gene.mat <- getDataFromTextArea(geneIDs);
  gene.vec <- rownames(gene.mat);
  
  #record the info
  mSetObj$dataSet$q.type.gene <- idType;
  mSetObj$dataSet$gene.orig <- geneIDs;
  mSetObj$dataSet$gene.org <- org;
  mSetObj$dataSet$gene.mat <- gene.mat;
  mSetObj$dataSet$gene <- gene.vec;
  
  enIDs <- doGeneIDMapping(gene.vec, org, idType);
  
  if(idType == "kos"){
    kos <- enIDs$kos;
    enIDs <- enIDs$entrezs;
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

#'Remove selected compounds
#'@description Remove compounds
#'@param mSetObj Input name of the created mSet Object
#'@param inx Input the index of compound to remove 
#'@export
#'
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

#'Remove selected genes
#'@description Remove selected genes based on an index
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param inx Input compound index
#'@export
#'
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

#'Prepare integrated data
#'@description Used for the pathinteg module.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
#'
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
    gene.mat <- RemoveDuplicates(gene.mat);
    AddMsg(paste("A total of ", nrow(gene.mat), "unique genes were uploaded."));
    
    if(!exists("pathinteg.imps", where = mSetObj$dataSet)){
      mSetObj$dataSet$pathinteg.imps <- list();
    }
    mSetObj$dataSet$pathinteg.imps$gene.mat <- gene.mat;
    done <- 1;
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
#'@export
#'
PerformIntegPathwayAnalysis <- function(mSetObj=NA, topo="dc", enrich="hyper", libOpt="integ", integOpt="query"){

    mSetObj <- .get.mSet(mSetObj);  
    libPath <- paste("kegg/jointpa/",libOpt,sep="");
    LoadKEGGLib(libPath, mSetObj$org);
  
    mSetObj$dataSet$pathinteg.method <- libOpt;
    mSetObj$dataSet$path.mat <- NULL;
  
  if(libOpt == "genetic" && !is.null(mSetObj$dataSet$pathinteg.imps$gene.mat)){
    gene.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    gene.vec <- paste(mSetObj$org, ":", rownames(gene.mat), sep="");
    rownames(gene.mat) <- gene.vec;
    impMat <- gene.mat;
    uniq.count <- inmexpa$uniq.gene.count;
    uniq.len <- inmexpa$gene.counts;

    # saving only
    gene.sbls <- doEntrez2SymbolMapping(rownames(mSetObj$dataSet$pathinteg.imps$gene.mat), mSetObj$org);
    gene.mat <- cbind(Name=gene.sbls, mSetObj$dataSet$pathinteg.imps$gene.mat);
    write.csv(gene.mat, file="MetaboAnalyst_result_genes.csv");
    
  }else if(libOpt == "metab" && !is.null(mSetObj$dataSet$pathinteg.imps$cmpd.mat)){

    cmpd.mat <- mSetObj$dataSet$pathinteg.imps$cmpd.mat;
    cmpd.vec <- paste("cpd:", rownames(cmpd.mat), sep="");
    rownames(cmpd.mat) <- cmpd.vec;
    impMat <- cmpd.mat;
    uniq.count <- inmexpa$uniq.cmpd.count
    uniq.len <- inmexpa$cmpd.counts;

    # saving only
    cmpd.nms <- doKEGG2NameMapping(rownames(mSetObj$dataSet$pathinteg.imps$cmpd.mat));
    cmpd.mat <- cbind(Name=cmpd.nms, mSetObj$dataSet$pathinteg.imps$cmpd.mat);
    write.csv(mSetObj$dataSet$pathinteg.imps$cmpd.mat, file="MetaboAnalyst_result_cmpds.csv");
    
  }else{ # integ

    if(is.null(mSetObj$dataSet$pathinteg.imps$cmpd.mat) | is.null(mSetObj$dataSet$pathinteg.imps$gene.mat)){
        AddErrMsg("The integrative analysis require both gene and metabolite lists");
        return(0);
    }

    impMat <- NULL;
    uniq.count <- uniq.len <- 0;

    cmpd.mat <- mSetObj$dataSet$pathinteg.imps$cmpd.mat;
    cmpd.vec <- paste("cpd:", rownames(cmpd.mat), sep="");
    rownames(cmpd.mat) <- cmpd.vec;
    # saving 
    cmpd.nms <- doKEGG2NameMapping(rownames(mSetObj$dataSet$pathinteg.imps$cmpd.mat));
    write.csv(cbind(Name=cmpd.nms, mSetObj$dataSet$pathinteg.imps$cmpd.mat), file="MetaboAnalyst_result_cmpds.csv");
 
    gene.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    gene.vec <- paste(mSetObj$org, ":", rownames(gene.mat), sep="");
    rownames(gene.mat) <- gene.vec;
    # saving 
    gene.sbls <- doEntrez2SymbolMapping(rownames(mSetObj$dataSet$pathinteg.imps$gene.mat), mSetObj$org);
    write.csv(cbind(Name=gene.sbls, mSetObj$dataSet$pathinteg.imps$gene.mat), file="MetaboAnalyst_result_genes.csv");
    
    # used by both (the merge p val only updates the p values
    impMat <- rbind(cmpd.mat, gene.mat);
    uniq.count <- inmexpa$uniq.cmpd.count + inmexpa$uniq.gene.count;
    uniq.len <- inmexpa$cmpd.counts + inmexpa$gene.counts;

    if(integOpt == "pval"){ #merge queries
        impMatList <- list(cmpd=cmpd.mat, gene=gene.mat);
        uniq.count.list <- list(cmpd=inmexpa$uniq.cmpd.count, gene=inmexpa$uniq.gene.count);
        uniq.len.list <- list(cmpd=inmexpa$cmpd.counts, gene=inmexpa$gene.counts);
    }
  }

  ora.vec <- rownames(impMat);
  impMat <- data.frame(Name=ora.vec, logFC=as.numeric(impMat[,1]));
  rownames(impMat) <- ora.vec;

  # update pvals and save individual analysis 
  if(libOpt == "integ" && integOpt == "pval"){

    # get res
    res.cmpd <- .performPathEnrich(rownames(impMatList$cmpd), uniq.count.list$cmpd, uniq.len.list$cmpd, enrich, topo, TRUE, FALSE);
    res.gene <- .performPathEnrich(rownames(impMatList$gene), uniq.count.list$gene, uniq.len.list$gene, enrich, topo, TRUE, TRUE);

    if(is.null(res.gene) || is.null(res.cmpd)){
        my.res <- .performPathEnrich(ora.vec, uniq.count,uniq.len, enrich, topo);
    }else{
        # writecsv
        write.csv(res.cmpd$res.table, file="MetaboAnalyst_result_pathway_cmpd.csv", row.names=TRUE);
        write.csv(res.gene$res.table, file="MetaboAnalyst_result_pathway_gene.csv", row.names=TRUE);

        # do integ
        my.res <- .performPathEnrich(rownames(impMat), uniq.count, uniq.len, enrich, topo, TRUE, TRUE);
        res.mat <- my.res$res.table;

        # merge p val
        jointp.res <- .performIntegPathMergeP(res.cmpd$res.table, res.gene$res.table, res.mat);

        # now update the res.integ with merge p
        res.mat[rownames(jointp.res),4] <- jointp.res$"P raw";
        res.mat[,5] <- -log(res.mat[,4]);
        res.mat[,6] <- p.adjust(res.mat[,4], "holm");
        res.mat[,7] <- p.adjust(res.mat[,4], "fdr");
        my.res$res.table <- signif(res.mat, 5);
    }

  }else{
    my.res <- .performPathEnrich(ora.vec, uniq.count,uniq.len, enrich, topo);
  }

  resTable <- my.res$res.table;
  hits.path <- my.res$hits.path;

  # now save to csv
  write.csv(resTable, file="MetaboAnalyst_result_pathway.csv", row.names=TRUE);
  
  # for internal use, switch to pathway IDs (name containing special characters)
  rownames(resTable) <- inmexpa$path.ids[rownames(resTable)];
  
  # store results from individual analysis
  mSetObj$dataSet$path.mat <- resTable;
  mSetObj$dataSet$path.hits <- hits.path;
  mSetObj$dataSet$pathinteg.impMat <- impMat; 

  return(.set.mSet(mSetObj));
}

# merge p values for two matrices from regular enrichment analysis
.performIntegPathMergeP <- function(res1, res2, res3){

    inx = which(rownames(res1) %in% rownames(res2))
    subres1 = as.data.frame(res1[inx,,drop=FALSE])
    inx = which(rownames(res2) %in% rownames(res1))
    subres2 = as.data.frame(res2[inx,,drop=FALSE])
    inx = which(rownames(res3) %in% rownames(subres2))
    subres3 = as.data.frame(res3[inx,,drop=FALSE])
    
    ord = order(rownames(subres1));
    subres1 = subres1[ord,,drop=FALSE]
    ord = order(rownames(subres2));
    subres2 = subres2[ord,,drop=FALSE]
    ord = order(rownames(subres3));
    subres3 = subres3[ord,,drop=FALSE]
    
    integ=data.frame(hitsG = subres1$Hits,hitsM = subres2$Hits,hitsTotal = subres3$Hits, P.ValueG=subres1$"Raw p", P.ValueM=subres2$"Raw p", P.ValueMerge=subres3$"Raw p", P.ValueJoint=subres3$"Raw p")
    
    rownames(integ) = rownames(subres1)
    for(i in 1:nrow(integ)){
      if(integ$P.ValueG[i] != 1 && integ$P.ValueM[i] != 1){
        integ$"P raw"[i] = metap::sumz(p=c(integ$P.ValueG[i], integ$P.ValueM[i]), weight=c(stouffer_gene_percent, stouffer_compound_percent))$p
      }else{
        integ$"P raw"[i]=1
      }
    }

    return(integ);
}

# internal function called by PerformIntegPathwayAnalysis
.performPathEnrich <- function(ora.vec, uniq.count,uniq.len, enrich, topo, is.integp=FALSE, is.gene=FALSE){
  
    # set up the mset
    ms.list <- lapply(inmexpa$mset.list, function(x){strsplit(x, " ")});
    current.universe <- unique(unlist(ms.list)); 
    set.size <- length(inmexpa$mset.list);

    #calculate weight for stouffer
    if(is.integp){
        my.hits <- ora.vec %in% current.universe;
        if(is.gene){
            stouffer_gene_percent <<- length(my.hits)/length(current.universe)
        }else{
            stouffer_compound_percent <<- length(my.hits)/length(current.universe)
        }
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
        return(NULL);
    }

    # prepare for the result table
    res.mat<-matrix(0, nrow=set.size, ncol=8);
    rownames(res.mat)<-names(inmexpa$path.ids);
    colnames(res.mat)<-c("Total", "Expected", "Hits", "Raw p", "-log(p)", "Holm adjust", "FDR", "Impact");

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

    res.mat[,5] <- -log(res.mat[,4]);
    res.mat[,6] <- p.adjust(res.mat[,4], "holm");
    res.mat[,7] <- p.adjust(res.mat[,4], "fdr");
  
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
    saveRDS(imp.list, file="pathinteg.impTopo");

    # now, perform topological analysis		
    # calculate the sum of importance
    res.mat[,8] <- mapply(function(x, y){sum(x[y])}, imp.list, hits.path);
    res.mat <- res.mat[hit.num>0, , drop=FALSE];
    res.mat <- res.mat[!is.na(res.mat[,8]), , drop=FALSE];
    ord.inx<-order(res.mat[,4], res.mat[,8]);
    resTable <- signif(res.mat[ord.inx,,drop=FALSE],5);
    return(list(hits.path=hits.path, res.table=resTable));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetIntegResultPathIDs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$dataSet$path.mat));
}

GetIntegResultPathNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(names(inmexpa$path.ids)[match(rownames(mSetObj$dataSet$path.mat),inmexpa$path.ids)]);
}

GetIntegResultColNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$path.mat));
}

GetIntegResultMatrix<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(as.matrix(mSetObj$dataSet$path.mat));
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
  
  db.path <- paste("../../libs/", mSetObj$org, "/entrez.csv", sep="");
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

#'Transform two column text to data matrix
#'@description Transform two column input text to data matrix (single column data frame)
#'@param txtInput Input text
#'@param sep.type Indicate the seperator type for input text. Default set to "space"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
getDataFromTextArea <- function(txtInput, sep.type="space"){
  
  lines <- unlist(strsplit(txtInput, "\r|\n|\r\n")[1]);
  if(substring(lines[1],1,1)=="#"){
    lines <- lines[-1];
  }
  
  # separated by tab 
  if(sep.type=="tab"){
    my.lists <- strsplit(lines, "\\t");
  }else{ # from any space
    my.lists <- strsplit(lines, "\\s+");
  }
  my.mat <- do.call(rbind, my.lists);
  
  if(dim(my.mat)[2] == 1){ # add 0
    my.mat <- cbind(my.mat, rep(0, nrow(my.mat)));
  }else if(dim(my.mat)[2] > 2){
    my.mat <- my.mat[,1:2];
    msg <- "More than two columns found in the list. Only first two columns will be used."
    AddErrMsg(msg);
  }
  rownames(my.mat) <- data.matrix(my.mat[,1]);
  my.mat <- my.mat[,-1, drop=F];
  return(my.mat);
}


#'Plot integrated methods pathway analysis
#'@description Only update the background info for matched node
#'@usage PlotInmexPath(mSetObj=NA, path.id, width, height)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param path.id Input the ID of the pathway to plot. 
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the image to create.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import igraph  
#'
PlotInmexPath <- function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=NULL){

  mSetObj <- .get.mSet(mSetObj);
  path.id <- inmexpa$path.ids[pathName];
  g <- inmexpa$graph.list[[path.id]]
  g <- upgrade_graph(g); # to fix warning, remove for new version
  phits <- mSetObj$dataSet$path.hits[[path.id]];
  pathinteg.impTopo <- readRDS("pathinteg.impTopo");
  topo <- pathinteg.impTopo[[path.id]];
  
  # obtain up/down/stat information
  res <- mSetObj$dataSet$pathinteg.impMat;
  
  bg.cols <- rep("#E3E4FA", length(V(g)));
  line.cols <- rep("dimgray", length(V(g)));
  
  # now, do color schema - up red, down green
  nd.inx <- which(phits);
  
  # fill with 'NA'
  stats <- vector(mode='list', length=length(V(g)));
  
  rnms <- rownames(res);
  for(inx in nd.inx){
    nm <- unlist(strsplit(V(g)$names[inx], " "));
    hit.inx <- which(rnms %in% nm)[1];
    if(length(hit.inx) > 0){
      # use logFCs to decide up/down regulated
      if(res$logFC[hit.inx] > 0){
        bg.cols[inx]<- "#F75D59";
        line.cols[inx] <- "#C11B17";
      }else if(res$logFC[hit.inx] == 0){
        bg.cols[inx]<- "#FFFF77";
        line.cols[inx] <- "#F7E259";
      }else{
        bg.cols[inx]<- "#6AFB92";
        line.cols[inx] <- "#347235";
      }
      
      # 1) update the node info (tooltip/popup) 
      V(g)$db.lnks[inx] <- paste("<a href='http://www.genome.jp/dbget-bin/www_bget?", rownames(res)[hit.inx],
                                  "' target='_blank'>", res$Name[hit.inx], "</a>", sep="", collapse=" ");
      # 2) save the stats for each node 
      stats[[inx]] <- signif(res[hit.inx, "logFC", drop=F],5);
    }
  }
  V(g)$stats <- stats;
  V(g)$topo <- topo;
  
  if(.on.public.web){ 
    return(PlotInmexGraph(mSetObj, pathName, g, width, height, bg.cols, line.cols, format, dpi));  
  }else{ 
    mSetObj <- PlotInmexGraph(mSetObj, pathName, g, width, height, bg.cols, line.cols, format, dpi);   
    print("pathinteg graph has been created, please find it in mSet$imgSet$pathinteg.path")
    return(.set.mSet(mSetObj));
  }
}

#'Plot an igraph object and return the node information (position and labels)
#'@description Plot an igraph object and return the node information (position and labels)
#'Used in a higher function
#'@param mSetObj Input name of the created mSet Object
#'@param path.id Input the pathway id
#'@param g Input the graph
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param height Input the height of the graph to create
#'@param bg.color Set the background color, default is set to NULL
#'@param line.color Set the line color, default is set to NULL
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotInmexGraph <- function(mSetObj, pathName, g, width=NA, height=NA, bg.color=NULL, line.color=NULL, format="png", dpi=NULL){
 
  if(is.null(bg.color)){
    bg.color <- V(g)$graphics_bgcolor
  }
  if(is.null(line.color)){
    line.color <- "dimgray";
  }

  if(!is.null(dpi)){
    pathName <- gsub("\\s","_", pathName);
    pathName <- gsub(",","", pathName);

    imgName = paste(pathName, "_dpi", dpi, ".", format, sep="");
    if(is.na(width)){
        width <- 8;
    }
    w <- h <- width;
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mai=rep(0,4));
    plotGraph(g, vertex.label=V(g)$plot_name, vertex.color=bg.color, vertex.frame.color=line.color);
    dev.off();
    return(imgName);
  }else{
    imgName <- paste(pathName, ".png", sep="");
    mSetObj$imgSet$pathinteg.path <- imgName
    Cairo::Cairo(file=imgName, width=width, height=height, type="png", bg="white");
    par(mai=rep(0,4));
    plotGraph(g, vertex.label=V(g)$plot_name, vertex.color=bg.color, vertex.frame.color=line.color);
    nodeInfo <- GetKEGGNodeInfo(pathName, g, width, height);
    dev.off();

    mSetObj$dataSet$current.kegg <- list(graph=g, bg.color=bg.color, line.color=line.color);
  
    # remember the current graph
    if(.on.public.web){
        .set.mSet(mSetObj);
        return(nodeInfo);
    }else{
        return(.set.mSet(mSetObj));
    } 
   }
}

#'Retrieves KEGG node information
#'@param path.id Input the path ID
#'@param g Input data
#'@param width Input the width
#'@param height Input the height 
#'@param usr Input the user
#'@export
GetKEGGNodeInfo <- function(pathName, g, width, height, usr = par("usr")){
  
  x.u2p = function(x) { rx=(x-usr[1])/diff(usr[1:2]);  return(rx*width)  }
  y.u2p = function(y) { ry=(usr[4]-y)/diff(usr[3:4]);  return(ry*height) }
  
  wds <- V(g)$graphics_width;
  #wds[wds == 'unknow']<- 46;
  wds[is.na(wds)]<- 46;  
  hts <- V(g)$graphics_height;
  hts[is.na(hts)]<- 17;     
  
  nw <- 1/200*as.numeric(wds);
  nh <-  1/200*as.numeric(hts);
  nxy <- igraph::layout.norm(getLayout(g), -1, 1, -1, 1);
  
  # note: nxy is the center of the node, need to deal differently for cirlce or rectangle
  # for circle the width/height are radius, stay the same, only adjust the rectangle
  rec.inx <- V(g)$graphics_type == "rectangle";
  
  nw[rec.inx] <- nw[rec.inx]/4;
  nh[rec.inx] <- nh[rec.inx]/2;
  
  xl  = floor(100*x.u2p(nxy[,1] - nw)/width);
  xr  = ceiling(100*x.u2p(nxy[,1] + nw)/width);
  yu  = floor(100*y.u2p(nxy[,2] - nh)/height);
  yl  = ceiling(100*y.u2p(nxy[,2] + nh)/height);
  
  tags <- V(g)$graphics_name;
  nm.lnks <- V(g)$db.lnks;
  
  # create the javascript code
  path.id <- inmexpa$path.ids[pathName];
  jscode <- paste("keggPathLnk=\'<a href=\"javascript:void(0);\" onclick=\"window.open(\\'http://www.genome.jp/kegg-bin/show_pathway?", path.id, "\\',\\'KEGG\\');\">", pathName, "</a>\'", sep="");
  jscode <- paste(jscode, paste("keggPathName=\"", pathName,"\"", sep=""), sep="\n");
  
  #add code for mouseover locations, basically the annotation info. In this case, the name of the node
  if(is.null(V(g)$stats)){
    for(i in 1:length(tags)) {
      jscode <- paste(jscode, paste("rectArray.push({x1:", xl[i], ", y1:", yl[i], ", x2:", 
                                    xr[i], ", y2:", yu[i],
                                    ", lb: \"", tags[i], 
                                    "\", lnk: \"", nm.lnks[i], 
                                    "\"})", sep=""), sep="\n");
    }
  }else{
    stats <- V(g)$stats;
    topos <- signif(V(g)$topo,5);
    for(i in 1:length(tags)) {
      jscode <- paste(jscode, paste("rectArray.push({x1:", xl[i], ", y1:", yl[i], ", x2:", 
                                    xr[i], ", y2:", yu[i],
                                    ", lb: \"", tags[i], 
                                    "\", lnk: \"", nm.lnks[i], 
                                    "\", topo: ", topos[i], 
                                    ifelse(is.null(stats[[i]]), 
                                           "", 
                                           paste(", logFC:", stats[[i]][1], sep="")),
                                    "})", sep=""), sep="\n");
    }
  }
  return(jscode);
}

# Used in higher function
plotGraph <- function(graph,margin=0,vertex.label.cex=0.6,vertex.label.font=1,vertex.size=8,
                    vertex.size2=6,edge.arrow.size=0.2,edge.arrow.width=3,vertex.label=V(graph)$graphics_name,
                    vertex.shape=V(graph)$graphics_type,layout=getLayout(graph),vertex.label.color="black",
                    vertex.color=V(graph)$graphics_bgcolor,vertex.frame.color="dimgray",edge.color="dimgray",
                    edge.label=getEdgeLabel(graph),edge.label.cex=0.6,edge.label.color="dimgray",edge.lty=getEdgeLty(graph),
                    axes=FALSE,xlab="",ylab="",sub=NULL,main=NULL,...){
  if(class(graph)!="igraph") stop("the graph should be a igraph graph.")
  if(vcount(graph)==0){
    print("the graph is an empty graph.")
  }else{	 
    vertex.shape <- replace(vertex.shape,which(vertex.shape %in% c("roundrectangle","line")),"crectangle")
    vertex.color <- replace(vertex.color,which(vertex.color %in% c("unknow","none")),"white")
    if(length(vertex.shape)==0) vertex.shape<-NULL
    if(length(vertex.color)==0) vertex.color<-NULL  
    if(length(vertex.label)==0) vertex.label<-NULL 
    if(length(layout)==0 | sum(is.na(layout))>0){
        print("Layout contain NAs");
        layout<-NULL;
    }
    if(length(edge.label)==0) edge.label<-NULL
    if((axes==FALSE)&&xlab==""&&ylab==""&&is.null(sub)&&is.null(main)){
      old.mai <- par(mai=c(0.01,0.25,0.01,0.3))
      #old.mai<-par(mai=0.01+c(0,0,0,0))
      on.exit(par(mai=old.mai), add=TRUE)
    }
    plot(graph,margin=margin,vertex.label.cex=vertex.label.cex,vertex.label.font=vertex.label.font,
         vertex.size=vertex.size,vertex.size2=vertex.size2,
         edge.arrow.size=edge.arrow.size,edge.arrow.width=edge.arrow.width,vertex.label=vertex.label,
         vertex.shape=vertex.shape,layout=layout,vertex.label.color=vertex.label.color,
         vertex.color=vertex.color,vertex.frame.color=vertex.frame.color,edge.color=edge.color,
         edge.label=edge.label,edge.label.cex=edge.label.cex,edge.label.color=edge.label.color,
         edge.lty=edge.lty,axes=axes,xlab=xlab,ylab=ylab,sub=sub,main=main,...)
  }
}

getLayout<-function(graph){
  if(length(V(graph)$graphics_x)==0||length(V(graph)$graphics_y)==0) return (NULL)
  x_y<-c()

  graphics_x <- igraph::get.vertex.attribute(graph,"graphics_x")
  index <- which(is.na(graphics_x))
  if(length(index)>1){
    #temp<-as.numeric(graphics_x[which(graphics_x!="unknow")]) # this is old version
    temp<-as.numeric(graphics_x[which(!is.na(graphics_x))])
    if(length(temp)<2){temp<-as.numeric(c(100,600))}
    replace_value<-seq(min(temp),max(temp),by = (max(temp)-min(temp))/(length(index)-1))
    graphics_x<-replace(graphics_x,which(is.na(graphics_x)),replace_value)
  }else if(length(index)==1){
    temp<-as.numeric(graphics_x[which(!is.na(graphics_x))])
    graphics_x<-replace(graphics_x,which(is.na(graphics_x)),min(temp))
  } 
  graphics_x <- as.numeric(graphics_x);

  graphics_y <- igraph::get.vertex.attribute(graph,"graphics_y")
  index <- which(is.na(graphics_y))
  if(length(index)>0){
    temp <- as.numeric(graphics_y[which(!is.na(graphics_y))])
    if(length(temp)<2){temp<-as.numeric(c(100,600))}
    graphics_y <- replace(graphics_y,which(is.na(graphics_y)),max(temp)+100)
  } 
  graphics_y <- as.numeric(graphics_y)
  
  x_y <- as.matrix(data.frame(graphics_x=graphics_x, graphics_y=graphics_y))
  x_y[,2] <- -x_y[,2]
  dimnames(x_y) <- NULL
  return(x_y)
}

getEdgeLabel<-function(graph){
  edge.name <- igraph::E(graph)$subtype_name
  edge.value <- igraph::E(graph)$subtype_value
  #edge.label<-E(graph)$subtype_value
  edge.label <- rep("",len=length(edge.name))
  for(i in seq(edge.name)){
    edge_i<-unlist(strsplit(edge.name[i],";"))
    if("phosphorylation" %in% edge_i){
      edge.label[i]<-paste("+p",edge.label[i],sep=" ")
    }
    if("dephosphorylation" %in% edge_i){
      edge.label[i]<-paste("-p",edge.label[i],sep=" ")
    }
    if("glycosylation"  %in% edge_i){
      edge.label[i]<-paste("+g",edge.label[i],sep=" ")
    }
    if("ubiquitination"  %in% edge_i){
      edge.label[i]<-paste("+u",edge.label[i],sep=" ")
    }
    if("methylation"  %in% edge_i){
      edge.label[i]<-paste("+m",edge.label[i],sep=" ")
    }
    if("missing interaction"  %in% edge_i){
      edge.label[i]<-paste("/",edge.label[i],sep=" ")
    }
    if("dissociation"  %in% edge_i){
      edge.label[i]<-paste("|",edge.label[i],sep=" ")
    }
    if("binding/association"  %in% edge_i){
      edge.label[i]<-paste("---",edge.label[i],sep=" ")
    }
    if("repression"  %in% edge_i){
      edge.label[i]<-paste("-e-|",edge.label[i],sep=" ")
    }
    if("expression"  %in% edge_i){
      edge.label[i]<-paste("-e->",edge.label[i],sep=" ")
    }
    if("inhibition"  %in% edge_i){
      edge.label[i]<-paste("--|",edge.label[i],sep=" ")
    }
    if("activation"  %in% edge_i){
      edge.label[i]<-paste("-->",edge.label[i],sep=" ")
    }
    if("indirect effect"  %in% edge_i){
      edge.label[i]<-paste("..>",edge.label[i],sep=" ")
    }
    if("state change"  %in% edge_i){
      edge.label[i]<-paste("...",edge.label[i],sep=" ")
    }
    if("compound" %in% edge_i){
      compound<-V(graph)[V(graph)$id==edge.value[i]]$graphics_name
      if(length(compound)==1){
        edge.label[i]<-paste(compound,edge.label[i],sep=" ")
      }    
    }           
  }
  return(edge.label)
}

#04350 indirect effect,04620
getEdgeLty<-function(graph){
  edge.name <- igraph::E(graph)$subtype_name
  edge.lty=rep("solid",len=length(edge.name))
  for(i in seq(edge.name)){
    if(edge.name[i]=="indirect effect"){
      edge.lty[i]<-"longdash"
    }else if(edge.name[i]=="state change"){
      edge.lty[i]<-"longdash"
    }
  }
  #new!
  if(length(edge.lty)==0){
    edge.lty="solid"
  }
  return(edge.lty)
}

doEntrez2SymbolMapping <- function(entrez.vec, org.code){
  if(.on.public.web){
    url.pre <- "../../libs/";
  }else{
    url.pre <- "https://www.metaboanalyst.ca/resources/libs/";
  }
  db.path <- paste(url.pre, org.code, "/entrez.rds", sep="");
  gene.map <- readRDS(db.path);
  
  hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
  symbols <- gene.map[hit.inx, "symbol"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(symbols);
  symbols[na.inx] <- entrez.vec[na.inx];
  return(symbols);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

# return gene and compounds highlighted in the pathway
GetIntegHTMLPathSet<-function(mSetObj=NA, pathName){
    mSetObj <- .get.mSet(mSetObj);
    path.id <- inmexpa$path.ids[[pathName]];

    # pathway nodes
    all.ids <- inmexpa$mset.list[[path.id]];
    g <- upgrade_graph(inmexpa$graph.list[[path.id]]);
    all.nms <- V(g)$graphics_name;

    phits <- mSetObj$dataSet$path.hits[[path.id]];
    nd.inx <- which(phits);
    all.nms[nd.inx] <- paste("<font color=\"red\">", "<b>", all.nms[nd.inx], "</b>", "</font>",sep="");

    return(cbind(pathName, paste(unique(all.nms), collapse="; ")));
}