
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
#'@usage PerformIntegPathwayAnalysis(mSetObj, topo="dc", enrich="hyper", 
#'libOpt="integ", integOpt="query")
#'@param mSetObj Input name of the created mSet Object
#'@param topo Select the mode for topology analysis: Degree Centrality ("dc") measures 
#'the number of links that connect to a node 
#'(representing either a gene or metabolite) within a pathway; Closeness Centrality ("cc") measures 
#'the overall distance from a given node 
#'to all other nodes in a pathway; Betweenness Centrality ("bc")measures the number of shortest paths from 
#'all nodes to all the others that pass through a given node within a pathway.
#'@param enrich Method to perform over-representation analysis (ORA) based on either hypergenometrics analysis ("hyper")
#' or Fisher's exact method ("fisher").
#'@param libOpt Select the different modes of pathways, either the gene-metabolite mode ("integ") which 
#'allows for joint-analysis
#' and visualization of both significant genes and metabolites or the gene-centric ("genetic") and 
#' metabolite-centric mode ("metab") which allows users
#' to identify enriched pathways driven by significant genes or metabolites, respectively.
#' @param integOpt integOpt,default is "query"
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PerformIntegPathwayAnalysis <- function(mSetObj=NA, topo="dc", enrich="hyper", 
                                        libOpt="integ", integOpt="query"){

  mSetObj <- .get.mSet(mSetObj);  

  # make sure this is annotated   
  if(is.null(mSetObj$org)){
    AddErrMsg("It appears that data is not annotated yet - unknown organism and ID types! Please use other modules (pathway/network) to access this function.");
    return(0);
  }

  # shift actual enrichment to api.metaboanalyst.ca
  if(.on.public.web){
    sub.dir <- paste0("kegg/jointpa/",libOpt);
    destfile <- paste0(mSetObj$org, ".qs");
    current.kegglib <<- .get.my.lib(destfile, sub.dir);
    load_igraph();
  }
  
  mSetObj$dataSet$pathinteg.method <- libOpt;
  mSetObj$dataSet$path.mat <- NULL;
  
  if(libOpt == "genetic" && !is.null(mSetObj$dataSet$pathinteg.imps$gene.mat)){
    
    gene.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    gene.vec <- paste(mSetObj$org, ":", rownames(gene.mat), sep="");
    rownames(gene.mat) <- gene.vec;
    impMat <- gene.mat;
    
    # saving only
    gene.sbls <- doGeneIDMapping(rownames(mSetObj$dataSet$pathinteg.imps$gene.mat), mSetObj$org, "entrez");
    gene.mat <- cbind(Name=gene.sbls, mSetObj$dataSet$pathinteg.imps$gene.mat);
    fast.write.csv(gene.mat, file="MetaboAnalyst_result_genes.csv");
    
    if(.on.public.web){
      uniq.count <- current.kegglib$uniq.gene.count;
      uniq.len <- current.kegglib$gene.counts;
    }
    
  } else if (libOpt == "metab" && !is.null(mSetObj$dataSet$pathinteg.imps$cmpd.mat)){
    
    cmpd.mat <- mSetObj$dataSet$pathinteg.imps$cmpd.mat;
    cmpd.vec <- paste("cpd:", rownames(cmpd.mat), sep=""); # no need for this as the metpa compound ID does not contain "cpd:" prefix
    #cmpd.vec <- rownames(cmpd.mat);
    rownames(cmpd.mat) <- cmpd.vec;
    impMat <- cmpd.mat;

    # saving only
    cmpd.nms <- doKEGG2NameMapping(rownames(mSetObj$dataSet$pathinteg.imps$cmpd.mat));
    cmpd.mat <- cbind(Name=cmpd.nms, mSetObj$dataSet$pathinteg.imps$cmpd.mat);
    fast.write.csv(mSetObj$dataSet$pathinteg.imps$cmpd.mat, file="MetaboAnalyst_result_cmpds.csv");
    
    if(.on.public.web){
      uniq.count <- current.kegglib$uniq.cmpd.count
      uniq.len <- current.kegglib$cmpd.counts;
    }
    
  } else if(libOpt == "mgenetic" && !is.null(mSetObj$dataSet$pathinteg.imps$gene.mat)) {
    # integ (p vali option)
    
    # Step 1 do gene mapping
    gene.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    gene.vec <- paste(mSetObj$org, ":", rownames(gene.mat), sep="");
    rownames(gene.mat) <- gene.vec;
    impMat <- gene.mat;
    
    # saving only
    gene.sbls <- doGeneIDMapping(rownames(mSetObj$dataSet$pathinteg.imps$gene.mat), mSetObj$org, "entrez");
    gene.mat <- cbind(Name=gene.sbls, mSetObj$dataSet$pathinteg.imps$gene.mat);
    fast.write.csv(gene.mat, file="MetaboAnalyst_result_genes.csv");
    
    if(.on.public.web){
      uniq.count <- current.kegglib$uniq.gene.count;
      uniq.len <- current.kegglib$gene.counts;
    }
    
    
  } else { # integ (other p values)
    
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
    fast.write.csv(cbind(Name=cmpd.nms, mSetObj$dataSet$pathinteg.imps$cmpd.mat), file="MetaboAnalyst_result_cmpds.csv");
    
    gene.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    gene.vec <- paste(mSetObj$org, ":", rownames(gene.mat), sep="");
    rownames(gene.mat) <- gene.vec;
    # saving 
    gene.sbls <- doGeneIDMapping(rownames(mSetObj$dataSet$pathinteg.imps$gene.mat), mSetObj$org, "entrez");
    fast.write.csv(cbind(Name=gene.sbls, mSetObj$dataSet$pathinteg.imps$gene.mat), file="MetaboAnalyst_result_genes.csv");
    
    # used by both integ
    impMat <- rbind(cmpd.mat, gene.mat);

    # used by merge p values
    impMatList <- list(cmpd=cmpd.mat,gene=gene.mat);
    
    if(.on.public.web){
      uniq.count <- current.kegglib$uniq.cmpd.count + current.kegglib$uniq.gene.count;
      uniq.len <- current.kegglib$cmpd.counts + current.kegglib$gene.counts;
    }
  }
  
  ora.vec <- rownames(impMat);
  impMat <- data.frame(Name=ora.vec, logFC=as.numeric(impMat[,1]));
  rownames(impMat) <- ora.vec;
  mSetObj$dataSet$pathinteg.impMat <- impMat;
  if(!.on.public.web){
     # make this lazy load
    if(!exists("my.integ.kegg")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }    
    if(libOpt == "integ"){
      toSend = list(mSet = mSetObj, oraVec = ora.vec,
                    libOpt = libOpt, integOpt = integOpt, 
                    topoMethod = topo, enrichAnal = enrich,
                    org = mSetObj$org, integCmpds = rownames(impMatList$cmpd),
                    integGenes = rownames(impMatList$gene))
    }else{
      toSend = list(mSet = mSetObj, oraVec = ora.vec,
                    libOpt = libOpt, integOpt = integOpt, 
                    topoMethod = topo, enrichAnal = enrich, org = mSetObj$org)
    }
    saveRDS(toSend, "tosend.rds")
    return(my.integ.kegg());
  }
  
  my.res <- .performPathEnrich(ora.vec, uniq.count, uniq.len, enrich, topo);
  hits.path <- my.res$hits.path;
  hits.query <- my.res$hits.query;
  
  # combine pvals require performing analysis on compounds and genes separately. Note, we need to use the top from merge queries 
  if(libOpt == "integ" && integOpt != "query"){
    # perform metabolite enrichment
    res.cmpd <- .performPathEnrich(rownames(impMatList$cmpd), current.kegglib$uniq.cmpd.count, current.kegglib$cmpd.counts, enrich, topo);
    if(is.null(res.cmpd)){
      AddErrMsg("Failed to perform integration - not hits found for compound input.");
      return(0);
    }

    hits.cmpds <- my.res$hits.query;
    fast.write.csv(res.cmpd$res.table, file="MetaboAnalyst_result_pathway_cmpd.csv", row.names=TRUE);
    
    # perform gene enrichment
    res.gene <- .performPathEnrich(rownames(impMatList$gene), current.kegglib$uniq.gene.count, current.kegglib$gene.counts, enrich, topo);
    if(is.null(res.gene)){
      AddErrMsg("Failed to perform integration - not hits found for gene input.");
      return(0);
    }
    hits.genes <- my.res$hits.query;
    fast.write.csv(res.gene$res.table, file="MetaboAnalyst_result_pathway_gene.csv", row.names=TRUE);
    
    # merge two lists
    hits.both <- cbind(hits.cmpds, hits.genes);
    hitI <- apply(hits.both, 1, unlist);

    #fast.write.csv(hitI, file="MetaboAnalyst_result_pathway_combined.csv", row.names=TRUE);

    # now update the hits.query and res.table
    my.res$hits.query <- hitI;
    resI <- .performIntegPathMergeP(res.cmpd$res.table, res.gene$res.table, my.res$res.table, integOpt);    
    my.res$res.table <- resI;
  }
  
  resTable <- my.res$res.table;

  # do some sorting
  ord.inx<-order(resTable[,"Raw p"], resTable[,"Impact"]);   
  resTable <- resTable[ord.inx, , drop=FALSE];

  # now save to csv
  fast.write.csv(resTable, file="MetaboAnalyst_result_pathway.csv", row.names=TRUE);
  
  # for internal use, switch to pathway IDs (name containing special characters)
  rownames(resTable) <- current.kegglib$path.ids[rownames(resTable)];
  
  # Specific processing on the global data with genes
  if(integOpt == "pvali") {
    my.hits.genes <- lapply(names(my.res[["hits.path"]]), function(x){
      current.kegglib[["mset.list"]][[x]][my.res[["hits.path"]][[x]]]
      })
    names(my.hits.genes) <- names(current.kegglib$path.ids);
    my.hits.genes <- my.hits.genes[unname(which(sapply(my.hits.genes, length)!=0))]
    my.cmpds <- RJSONIO::fromJSON(mSetObj$mum_nm);
    my.hits.cmpds <- my.cmpds$hits.sig;
    names(my.hits.cmpds) <- my.cmpds$path.nms;
    mSetObj$dataSet$my.hits <- list(my.hits.genes = my.hits.genes, 
                                    my.hits.cmpds = my.hits.cmpds)
      
  } 
  
  # store results from individual analysis (for other analyses, not pvali)
  mSetObj$dataSet$path.mat <- resTable;
  mSetObj$dataSet$path.hits <- hits.path;
  mSetObj$dataSet$hits.query <- hits.query;
  mSetObj$dataSet$pathinteg.impMat <- impMat;
  # Perform meta integ for global metabolomics and genes
  if(integOpt == "pvali") {
    .performIntegGlobalMergeP(mSetObj, libOpt);
    mSetObj <- .plotIntegGlobalMergeP(mSetObj = NA, imgName = "integ_peakGene")
  }
    
  return(.set.mSet(mSetObj));
}

# merge p values for two matrices from regular enrichment analysis
# resM, resG, and resI are results from enrichment analysis from metabolites, genes and mergeQuery
.performIntegPathMergeP <- function(resM, resG, resI, opt){

    # get the ones with hits from both omics
    hitsM <- rownames(resM) %in% rownames(resG);
    inx <- which(hitsM);
 
    if(length(inx) > 0){
        cm.nms <- rownames(resM)[inx];

        # for overall weight
        total.count <- current.kegglib$uniq.cmpd.count + current.kegglib$uniq.gene.count;
        ow.m <- current.kegglib$uniq.cmpd.count/total.count;
        ow.g <- current.kegglib$uniq.gene.count/total.count;
    
        # for pathway weights
        path.uniq.lens <- current.kegglib$cmpd.counts + current.kegglib$gene.counts;
        pw.m <- current.kegglib$cmpd.counts/path.uniq.lens;
        pw.g <- current.kegglib$gene.counts/path.uniq.lens;
        names(pw.m) <- names(pw.g) <- names(current.kegglib$path.ids);

        for(nm in cm.nms){
            p.vec <- c(resM[nm, "Raw p"], resG[nm, "Raw p"]);
            if(opt == "pvalu"){ # unweighted
                w.vec <- c(0.5,0.5);
            }else if(opt == "pvalo"){ # overall
                w.vec <- c(ow.m,ow.g);
            }else{ # pathway level
                w.vec <- c(pw.m[nm],pw.g[nm]);
            }
            resI[nm, "Raw p"] = .performWeightedZtest(p.vec, w.vec)$p;
        }
    }
    # now replace resI two columns ("Expected" and "Hits") to individual hits
    colnames(resI)<-c("Total", "Hits.cmpd", "Hits.gene", "Raw p", "-log10(p)", "Holm adjust", "FDR", "Impact");
    resI[, "Hits.cmpd"] <- resI[,"Hits.gene"] <- rep(0, nrow(resI));
    cmpd.nms <- rownames(resM);
    resI[cmpd.nms, "Hits.cmpd"] <- resM[,"Hits"];
    gene.nms <- rownames(resG);
    resI[gene.nms, "Hits.gene"] <- resG[,"Hits"];

    # update raw p for those with hits from one type
    inxM.uniq <- which(!hitsM);
    cm.uniq <- rownames(resM)[inxM.uniq];
    resI[cm.uniq, "Raw p"] <- resM[cm.uniq,"Raw p"];

    hitsG <- rownames(resG) %in% rownames(resM);
    inxG.uniq <- which(!hitsG);
    gn.uniq <- rownames(resG)[inxG.uniq];
    resI[gn.uniq, "Raw p"] <- resG[gn.uniq,"Raw p"];
    
    resI[,"Raw p"][resI[,"Raw p"] == 0] <- min(resI[,"Raw p"][resI[,"Raw p"] != 0])/5;
    # now update the res.integ with merge p
    resI[,5] <- -log10(resI[,"Raw p"]);
    resI[,6] <- p.adjust(resI[,"Raw p"], "holm");
    resI[,7] <- p.adjust(resI[,"Raw p"], "fdr");
    resI <- signif(resI, 5);
    return(resI);
}

# internal function called by PerformIntegPathwayAnalysis
.performPathEnrich <- function(ora.vec, uniq.count, uniq.len, enrich, topo){

    # set up the mset
    ms.list <- lapply(current.kegglib$mset.list, function(x){strsplit(x, " ", fixed=TRUE)});
    current.universe <- unique(unlist(ms.list)); 
    set.size <- length(current.kegglib$mset.list);

    # need to cut to the universe covered by the pathways, not all genes 
    ora.vec <- ora.vec[ora.vec %in% current.universe]
    q.size <- length(ora.vec);
  
    # note, we need to do twice one for nodes (for plotting)
    # one for query for calculating, as one node can be associated with multiple matches
    # get the matched nodes on each pathway
    hits.path <- lapply(ms.list, function(x) {unlist(lapply(x, function(var){any(var%in%ora.vec);}),use.names=FALSE)});
    names(hits.path) <- current.kegglib$path.ids;
  
    # get the matched query for each pathway
    hits.query <- lapply(ms.list, function(x) {ora.vec%in%unlist(x);});

    hit.num <- unlist(lapply(hits.query, function(x){sum(x)}), use.names=FALSE);
  
    if(sum(hit.num) == 0){
        AddErrMsg("No hits found for your input!");
        return(NULL);
    }

    # now replace with actual ids
    hits.query <- lapply(hits.query, function(x){ora.vec[x]});
    names(hits.query) <- current.kegglib$path.ids;

    # prepare for the result table
    res.mat<-matrix(0, nrow=set.size, ncol=8);
    rownames(res.mat)<-names(current.kegglib$path.ids);
    colnames(res.mat)<-c("Total", "Expected", "Hits", "Raw p", "-log10(p)", "Holm adjust", "FDR", "Impact");

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

    
    res.mat[,4][res.mat[,4] == 0] <- min(res.mat[,4][res.mat[,4] != 0])/5;
    res.mat[,5] <- -log10(res.mat[,4]);
    res.mat[,6] <- p.adjust(res.mat[,4], "holm");
    res.mat[,7] <- p.adjust(res.mat[,4], "fdr");
  
    # toplogy test
    if(topo == "bc"){
        imp.list <- current.kegglib$bc;
    }else if(topo == "dc"){
        imp.list <- current.kegglib$dc;
    }else if(topo == "cc"){
        imp.list <- current.kegglib$cc;       
    }else{
        print("Not a defined topological measure!");
       # print(topo);
    }
    qs::qsave(imp.list, file="pathinteg.impTopo.qs");

    # now, perform topological analysis		
    # calculate the sum of importance
    res.mat[,8] <- mapply(function(x, y){sum(x[y])}, imp.list, hits.path);
    res.mat <- res.mat[hit.num>0, , drop=FALSE];
    res.mat <- res.mat[!is.na(res.mat[,8]), , drop=FALSE];
    resTable <- signif(res.mat,5);
    return(list(hits.path=hits.path, res.table=resTable, hits.query=hits.query));
}

# internal function to integrate untargeted metabolomics and genes
.performIntegGlobalMergeP <- function(mSetObj = NA, libOpt = "mgenetic"){

  # get p integ method
  integMethod <- mSetObj$dataSet$integPmethod;
  
  pathResults <- vector("list");
  pathResults$mummi <- mSetObj$mummi.resmat;
  pathResults$mgenes <- mSetObj$dataSet$path.mat;
  rownames(pathResults$mgenes) <- names(rownames(pathResults[["mgenes"]]));
  
  path.names.all <- lapply(pathResults, rownames);
  paths1 <- lapply(pathResults, 
                   data.table::data.table, 
                   keep.rownames = TRUE);
   
  path2 <- data.table::setDF(merge(paths1$mummi, paths1$mgenes, by = "rn", all.x=TRUE, all.y = TRUE))
  
  
  path2$Hits.sig[is.na(path2$Hits.sig)] <-
    path2$Hits[is.na(path2$Hits)] <-
    path2$`Pathway total`[is.na(path2$`Pathway total`)] <- 0
  if("Total.x" %in% colnames(path2)){
    path2$Total.x[is.na(path2$Total.x)] <- 0
  }
  if("Total.y" %in% colnames(path2)){
    path2$Total.y[is.na(path2$Total.y)] <- 0
    total <- path2$`Pathway total` + path2$Total.y;
  }
  if("Total" %in% colnames(path2)){
    path2$Total[is.na(path2$Total)] <- 0
    total <- path2$`Pathway total` + path2$Total;
  }
 
  path2$FET[is.na(path2$FET)] <- 
    path2$`Raw p`[is.na(path2$`Raw p`)] <- 1;
    
  hitsm <- path2$Hits.sig;
  hitsg <- path2$Hits;

  if(is.null(integMethod)){
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {sumlog(c(path2$FET[x], path2$`Raw p`[x]))$p});
  } else if(integMethod == "fisher") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {sumlog(c(path2$FET[x], path2$`Raw p`[x]))$p});
  } else if(integMethod == "edgington") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {sump(c(path2$FET[x], path2$`Raw p`[x]))$p});
  } else if(integMethod == "stouffer") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {
      if(path2$FET[x] == 1) {pval1 <- 0.999} else {pval1 <- path2$FET[x]}
      if(path2$`Raw p`[x] == 1) {pval2 <- 0.999} else {pval2 <- path2$`Raw p`[x]}
      sumz(c(pval1,pval2))$p
      });
  } else if(integMethod == "vote") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {votep(c(path2$FET[x], path2$`Raw p`[x]))$p});
  } else if(integMethod == "min") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {min(c(path2$FET[x], path2$`Raw p`[x]))});
  } else if(integMethod == "max") {
    mergP <- sapply(seq(nrow(path2)), FUN= function(x) {max(c(path2$FET[x], path2$`Raw p`[x]))});
  }

  logp <- -log10(mergP);
  holmp <- p.adjust(mergP, "holm");
  fdrp <- p.adjust(mergP, "fdr");
  order.idx <- order(mergP);
  integResGlobal <- data.frame(pathways = (path2$rn)[order.idx],
                               Total = total[order.idx],
                               hits_gene = hitsg[order.idx],
                               hits_cmpd = hitsm[order.idx],
                               P_cmpd = path2$FET[order.idx],
                               P_gene = path2$`Raw p`[order.idx],
                               P_value = round(mergP[order.idx],6),
                               LogP = round(logp[order.idx],4),
                               Holmp = round(holmp[order.idx],4),
                               FDRp = round(fdrp[order.idx],4));
                             
  #integResGlobal
  fast.write.csv(integResGlobal, file="MetaboAnalyst_result_integ.csv");
  path.mat <- as.matrix(integResGlobal[,-1])
  rownames(path.mat) <- unname(current.kegglib[["path.ids"]][integResGlobal[,1]])
  mSetObj$dataSet$path.mat <- path.mat;
  mSetObj$dataSet$integResGlobal <- integResGlobal;
  
  # correct the database as metab

  jointGlobal <- !is.null(mSetObj[["mum_nm_csv"]]);
  if(.on.public.web & jointGlobal){
    if(libOpt == "mgenetic") {
      sub.dir <- paste0("kegg/jointpa/integ");
    } else if(libOpt == "genetic") {
      sub.dir <- paste0("kegg/jointpa/all");
    }
    destfile <- paste0(mSetObj$org, ".qs");
    current.kegglib <<- .get.my.lib(destfile, sub.dir);
    load_igraph();
  }
  
  ## correct path.hits here with new current.kegglib
  my.hits <- mSetObj$dataSet$my.hits;
  comb.hits <- list();
  mypaths <- unique(c(names(my.hits$my.hits.genes), names(my.hits$my.hits.cmpds)))

  a1 <- my.hits$my.hits.genes[mypaths]
  a2 <- my.hits$my.hits.cmpds[mypaths]
  a2 <- sapply(a2, FUN= function(x) {if(!is.null(x)) paste0("cpd:",x)})
  paths.ids <- current.kegglib[["path.ids"]][mypaths];
  comb.hits <- setNames(mapply(c, a1[mypaths], a2[mypaths]), paths.ids)
  hits.path <- lapply(names(comb.hits), function(x) {
    current.kegglib$mset.list[[x]] %in% comb.hits[[x]]})
  names(hits.path) <- names(comb.hits)
  mSetObj$dataSet$path.hits <- hits.path;
  
  ## correct impMat here:
  mSetObj$dataSet$pathinteg.impMat -> impMat_genes;
  unique(unname(unlist(a2))) ->impMat_cmpds0;
  data.frame(Name = impMat_cmpds0, logFC = 100) -> impMat_cmpds
  rownames(impMat_cmpds) <- impMat_cmpds0;
  mSetObj$dataSet$pathinteg.impMat <- rbind(impMat_cmpds, impMat_genes); 
  
  .set.mSet(mSetObj);
}

.plotIntegGlobalMergeP <- function(mSetObj = NA, imgName, format = "png", dpi = 72, width = 9, labels = "default", 
                                   labels.x = 5, labels.y = 5, scale.axis = TRUE) {
  mSetObj <- .get.mSet(mSetObj);
  ### Here is the ploting function below
  combo.resmat <- mSetObj$dataSet$integResGlobal
  pathnames <- combo.resmat$pathways
  # Sort values based on combined pvalues
  y <- -log10(combo.resmat[,5]); # x is gene
  x <- -log10(combo.resmat[,6]); # y is cmpd
  if(min(combo.resmat[,7])==0){combo.resmat[,7][combo.resmat[,7]==0] <- 
    min(combo.resmat[,7][!(combo.resmat[,7] ==0)])/2}
  combo.p <- -log10(combo.resmat[,7])
  
  if(scale.axis){
    y <- scales::rescale(y, c(0,4))
    x <- scales::rescale(x, c(0,4))
    combo.p <- scales::rescale(combo.p, c(0,4))
  }
  
  inx <- order(combo.p, decreasing= T);
  
  combo.p <- combo.p[inx]
  x <- x[inx]; 
  y <- y[inx];
  path.nms <- pathnames[inx];
  
  # set circle size based on combined pvalues
  min.x <- min(combo.p, na.rm = TRUE);
  max.x <- max(combo.p, na.rm = TRUE);
  
  if(min.x == max.x){ # only 1 value
    max.x = 1.5*max.x;
    min.x = 0.5*min.x;
  }
  
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(combo.p-min.x)/(max.x-min.x);
  
  # set background color according to combo.p
  bg.vec <- heat.colors(length(combo.p));
  
  if(format == "png"){
    bg = "transparent";
  }else{
    bg="white";
  }
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  df <- data.frame(path.nms, x, y)
  
  if(labels == "default"){
    mummi.inx <- GetTopInx(df$y, labels.y, T)
    gsea.inx <- GetTopInx(df$x, labels.x, T)
    all.inx <- mummi.inx | gsea.inx;
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$integpks.plot <- imgName
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op <- par(mar=c(6,5,2,3));
  
  # color blocks only make sense if scaled...
  if(scale.axis){
    plot(x, y, type="n", axes=F, xlab="Enriched Pathways (Genes/Proteins)", ylab="Enriched Pathways from Peaks", bty = "l");
    axis(1);
    axis(2);
    symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
    
    axis.lims <- par("usr")
    
    # mummichog sig
    mum.x <- c(axis.lims[1], axis.lims[1], axis.lims[2], axis.lims[2])
    mum.y <- c(2, axis.lims[4], axis.lims[4], 2)
    polygon(mum.x, mum.y, col=rgb(82/255,193/255,188/255,0.3), border = NA)
    
    # gsea sig
    gsea.x <- c(2,2,axis.lims[4],axis.lims[4])
    gsea.y <- c(axis.lims[1],axis.lims[4],axis.lims[4],axis.lims[1])
    polygon(gsea.x, gsea.y, col=rgb(216/255,126/255,178/255,0.3), border = NA)
  }else{
    plot(x, y, type="n", xlim=c( 0, round(max(x)) ), ylim=c(0, round(max(y))), 
         xlab="Enriched Pathways (Genes/Proteins)",
         ylab="Enriched Pathways from Peaks", bty = "l");
    symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  }
  
  if(labels=="default"){
    text(x[all.inx], y[all.inx], labels = path.nms[all.inx], pos=3, xpd=T, cex=0.8)
  }else if(labels == "all"){
    text(x, y, labels = path.nms, pos=3, xpd=T, cex=0.8)
  }
  
  par(op);
  dev.off();
  
  df <- list(pval=unname(y), enr=unname(x), metap= unname(combo.p), pathnames=pathnames);
  sink("scatterinteg.json");
  cat(rjson::toJSON(df));
  sink();

  return(mSetObj);
}

DefineIntegPvalueMethod <- function(mSetObj = NA, method = "fisher"){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$integPmethod <- method;
  return(.set.mSet(mSetObj))
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
  return(names(current.kegglib$path.ids)[match(rownames(mSetObj$dataSet$path.mat),current.kegglib$path.ids)]);
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
  
  sqlite.path <- paste0(url.pre, mSetObj$org, "_genes.sqlite");
  if(!file.exists(sqlite.path)){
    #"https://www.xialab.ca/resources/sqlite/hsa_genes.sqlite"
    sqlite_url <- paste0("https://www.xialab.ca/resources/sqlite/", 
                         org.code, "_genes.sqlite");
    sqlite.path <- paste0(getwd(), "/",org.code, "_genes.sqlite")
    download.file(sqlite_url,destfile = sqlite.path, method = "curl")
  }
  conv.db <- .get.sqlite.con(sqlite.path); 
  gene.db <- dbReadTable(conv.db, "entrez")
  
  org <- mSetObj$org
   
  if(org %in% c("bta", "dre", "gga", "hsa", "mmu", "osa", "rno")){
    hit.inx <- match(enIDs, gene.db[, "gene_id"]);
  }else{
    hit.inx <- match(enIDs, gene.db[, "symbol"]);
  }
  
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
  
  fast.write.csv(csv.res, file="gene_name_map.csv", row.names=F);
  
  if(.on.public.web){
    .set.mSet(mSetObj)  
    return(as.vector(html.res));
  }else{
    return(.set.mSet(mSetObj));
  }
}


#'Plot integrated methods pathway analysis
#'@description Only update the background info for matched node
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param pathName Input the Name of the pathway to plot. 
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param height Input the height of the image to create.
#'@param format format of the image
#'@param dpi dpi, dpi of the image
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import igraph  
#'@import qs
PlotInmexPath <- function(mSetObj=NA, pathName, width=NA, height=NA, format="png", dpi=NULL){
  
  mSetObj <- .get.mSet(mSetObj);

  path.id <- current.kegglib$path.ids[pathName];
  g <- current.kegglib$graph.list[[path.id]];
  if(is_igraph(g)){
    g <- upgrade_graph(g); # to fix warning, can be removed for new version
  }
  phits <- mSetObj$dataSet$path.hits[[path.id]];
  pathinteg.impTopo <- qs::qread("pathinteg.impTopo.qs");
  topo <- pathinteg.impTopo[[path.id]];
  
  # obtain up/down/stat information
  res <- mSetObj$dataSet$pathinteg.impMat;
  
  bg.cols <- rep("#E3E4FA", length(V(g)));
  line.cols <- rep("dimgray", length(V(g)));
  
  # now, do color schema - up red, down green
  nd.inx <- which(phits);
  
  # fill with 'NA'
  stats <- vector(mode='list', length=length(V(g)));
  adducts <- vector(mode='list', length=length(V(g)));
  
  rnms <- rownames(res);
  for(inx in nd.inx){
    nm <- unlist(strsplit(V(g)$names[inx], " ", fixed=TRUE));
    #hit.inx <- which(rnms %in% nm)[1];
    hit.inx <- which(rnms %in% nm);
    if(length(hit.inx) > 0){
      hit.inx <- hit.inx[1];
      # use logFCs to decide up/down regulated
      if(res$logFC[hit.inx] == 100) {
        bg.cols[inx]<- "#1C77ED";
        line.cols[inx] <- "#1C5BED";
      } else if (res$logFC[hit.inx] > 0){
        bg.cols[inx]<- "#F75D59";
        line.cols[inx] <- "#C11B17";
      } else if (res$logFC[hit.inx] == 0){
        bg.cols[inx]<- "#FFFF77";
        line.cols[inx] <- "#F7E259";
      } else {
        bg.cols[inx]<- "#6AFB92";
        line.cols[inx] <- "#347235";
      }
      
      # 1) update the node info (tooltip/popup) 
      V(g)$db.lnks[inx] <- paste("<a href='http://www.genome.jp/dbget-bin/www_bget?", rownames(res)[hit.inx],
                                  "' target='_blank'>", res$Name[hit.inx], "</a>", sep="", collapse=" ");
      # 2) save the stats for each node 
      if(res[hit.inx, "logFC", drop=F] == 100){
        adducts[[inx]] <- GetMatchingDetails(mSetObj, sub("cpd:", "", res[hit.inx, "Name"]))
      } else {
        stats[[inx]] <- signif(as.numeric(res[hit.inx, "logFC", drop=F]),5);
      }
    }
  }

  if(length(stats)!=length(topo)) {
    topo <- current.kegglib[["bc.list"]][[path.id]]
  }
  V(g)$stats <- stats;
  V(g)$topo <- topo;
  V(g)$adducts <- adducts;

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
#'@param pathName Input the pathway name
#'@param g Input the graph
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param height Input the height of the graph to create
#'@param bg.color Set the background color, default is set to NULL
#'@param line.color Set the line color, default is set to NULL
#'@param format image format
#'@param dpi dpi of the image
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotInmexGraph <- function(mSetObj, pathName, 
                           g, width=NA, height=NA, 
                           bg.color=NULL, line.color=NULL, 
                           format="png", dpi=NULL){
 
  if(is.null(line.color)){
    line.color <- "dimgray";
  }

  if(is.null(dpi) & format == "pdf"){
    dpi <- 72;
  }

  if(is.null(dpi) & format == "svg"){
    dpi <- 72;
  }

  if(!is.null(dpi)){
    pathName <- gsub("\\s","_", pathName);
    pathName <- gsub(",","", pathName);

    imgName = paste(pathName, "_dpi", dpi, ".", format, sep="");
    mSetObj$imgSet$pathinteg.path <- imgName;
    if(is.na(width)){
        width <- 8;
    }
    w <- h <- width;

    Cairo::Cairo(file = imgName, dpi=dpi, width=w, height=h, type=format, bg="white");
    par(mai=rep(0,4));
    plotGraph(g, vertex.label=V(g)$plot_name, vertex.color=bg.color, vertex.frame.color=line.color);
    dev.off();
    if(.on.public.web){.set.mSet(mSetObj)}
    return(imgName);
  }else{
    imgName <- paste(pathName, ".png", sep="");
    mSetObj$imgSet$pathinteg.path <- imgName;
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
#'@param pathName Input the path Name
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
  path.id <- current.kegglib$path.ids[pathName];
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
    adducts <- V(g)$adducts;

    topos <- signif(V(g)$topo,5);
    for(i in 1:length(tags)) {
      jscode <- paste(jscode, paste("rectArray.push({x1:", xl[i], ", y1:", yl[i], ", x2:", 
                                    xr[i], ", y2:", yu[i],
                                    ", lb: \"", tags[i], 
                                    "\", lnk: \"", nm.lnks[i], 
                                    "\", adducts: \"", adducts[[i]], 
                                    "\", topo: ", topos[i], 
                                    ifelse(is.null(stats[[i]]), "", paste(", logFC:", stats[[i]][1], sep="")),
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
    edge_i<-unlist(strsplit(edge.name[i], ";", fixed=TRUE))
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

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

# return gene and compounds highlighted in the pathway
GetIntegHTMLPathSet<-function(mSetObj=NA, pathName){

    mSetObj <- .get.mSet(mSetObj);
    path.id <- current.kegglib$path.ids[[pathName]];

    phits <- mSetObj$dataSet$path.hits[[path.id]];
    nd.inx <- which(phits);

    # pathway nodes
    all.ids <- current.kegglib$mset.list[[path.id]];
    g <- current.kegglib$graph.list[[path.id]];

    if(is_igraph(g)){
        g <- upgrade_graph(g);
    }
    all.nms <- V(g)$graphics_name;
    all.nms[nd.inx] <- paste("<font color=\"red\">", "<b>", all.nms[nd.inx], "</b>", "</font>",sep="");

    return(cbind(pathName, paste(unique(all.nms), collapse="; ")));
}



#' CreateIntegMatchingTable
#'
#' @param mSetObj mSetObj Object
#'
#' @export
#'
CreateIntegMatchingTable <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  if(!.on.public.web){
    
    if(is.null(mSet$analSet$jointPAMatches)){
      AddErrMsg("Perform integrative pathway analysis first!")
      return(0)
    }
    
    res <- mSetObj$analSet$jointPAMatches
    fast.write.csv(res, "jointpa_matched_features.csv", row.names = T)
    return(.set.mSet(mSetObj));
  }

  results <- mSetObj$dataSet$path.mat
  match_paths <- rownames(results)

  ms.list <- lapply(current.kegglib$mset.list, function(x){strsplit(x, " ", fixed=TRUE)})
  ms.list <- ms.list[match(match_paths, names(ms.list))]
  ms.list.list <- lapply(ms.list, function(x) unique(unlist(x)))

  ora.vec <- rownames(mSetObj$dataSet$pathinteg.impMat)
  overlap.results <- lapply(ms.list.list, function(overlap) paste0(intersect(overlap, ora.vec), collapse="; ") )

  res <- data.frame(matrix(unlist(overlap.results), nrow=length(overlap.results), byrow=T), stringsAsFactors=FALSE)
  rownames(res) <- names(current.kegglib$path.ids)[match(match_paths, current.kegglib$path.ids)] 
  colnames(res) <- "matched_features"
  fast.write.csv(res, "jointpa_matched_features.csv", row.names = T)

  if(!.on.public.web){
    return(mSetObj)
  }else{
    return(.set.mSet(mSetObj));
  }
}

# perform p value combination, p.vec contain p values, w.vec contains weights
.performWeightedZtest <- function(p, weights=c(1,1)){
    zp <- (qnorm(p, lower.tail = FALSE) %*% weights)/sqrt(sum(weights^2));
    res <- list(z = zp, p = pnorm(zp, lower.tail = FALSE));
    res;
}
