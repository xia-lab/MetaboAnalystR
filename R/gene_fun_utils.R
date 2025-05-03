##################################################
## R script for MetaboAnalyst
## Description: GO/Pathway ORA 
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

# note: hit.query, resTable must synchronize
PerformNetEnrichment <- function(mSetObj=NA, file.nm, fun.type, IDs, vis.type=""){
    # prepare query
    ora.vec <- unlist(strsplit(IDs, "; ", fixed=TRUE));
    names(ora.vec) <- as.character(ora.vec);
    mSetObj <- .get.mSet(mSetObj);
    res <- PerformEnrichAnalysis(mSetObj, mSetObj$org, file.nm, fun.type, ora.vec, vis.type);
    return(res);
}

# note: hit.query, resTable must synchronize
# ora.vec should contains entrez ids, named by their gene symbols
PerformEnrichAnalysis <- function(mSetObj, org.code, file.nm, fun.type, ora.vec, vis.type=""){
    save.image("enr.RData");
    if(fun.type %in% c("keggc", "smpdb")){
        .load.enrich.compound.lib(org.code, fun.type);
    }else{
        .load.enrich.lib(org.code, fun.type);
    }

    # prepare query
    ora.nms <- names(ora.vec);
    ora <<- ora.vec

    # prepare for the result table
    set.size<-length(current.geneset);
    res.mat<-matrix(0, nrow=set.size, ncol=5);
    rownames(res.mat)<-names(current.geneset);
    colnames(res.mat)<-c("Total", "Expected", "Hits", "Pval", "FDR");

    # need to cut to the universe covered by the pathways, not all genes 
    hits.inx <- ora.vec %in% current.universe;
    ora.vec <- ora.vec[hits.inx];
    ora.nms <- ora.nms[hits.inx];

    q.size<-length(ora.vec);

    # get the matched query for each pathway

    if(fun.type %in% c("keggc", "smpdb")){
        hits.query <- lapply(current.geneset, function(x){x[names(x) %in% ora.vec]});
        hits.query<- lapply(hits.query, function(x){names(x)});
    }else{
        hits.query <- lapply(current.geneset, function(x) { ora.nms[ora.vec%in%unlist(x)];});
    }

    names(hits.query) <- names(current.geneset);
    hit.num<-unlist(lapply(hits.query, function(x){length(x)}), use.names=FALSE);

    # total unique gene number
    uniq.count <- length(current.universe);
    
    # unique gene count in each pathway
    set.size <- unlist(lapply(current.geneset, length));

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

    if(nrow(res.mat) == 0){
        AddErrMsg("No hits found for your query!");
        return(0);
    }

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
    

    #get gene symbols
    resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
    AddMsg("Functional enrichment analysis was completed");

    # write json
    fun.anot = hits.query; 
    fun.pval = resTable$Pval; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
    hit.num = paste0(resTable$Hits,"/",resTable$Total); if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    if(fun.type %in% c("keggc", "smpdb")){
        fun.ids <- as.vector(current.setids[which(current.setids %in% names(fun.anot))]); 
        names(fun.anot) = as.vector(names(current.setids[which(current.setids %in% names(fun.anot))]));
    }else{
        fun.ids <- as.vector(current.setids[names(fun.anot)]); 
    }

    if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
        json.res <- list(
                    fun.link = current.setlink[1],
                    fun.anot = fun.anot,
                    fun.ids = fun.ids,
                    fun.pval = fun.pval,
                    hit.num = hit.num
        );
     json.mat <- rjson::toJSON(json.res);
     json.nm <- paste(file.nm, ".json", sep="");
     
     sink(json.nm)
     cat(json.mat);
     sink();

    # write csv
    fun.hits <<- hits.query;
    fun.pval <<- fun.pval;
    hit.num <<- resTable$Hits;
    csv.nm <- paste(file.nm, ".csv", sep="");
    fast.write.csv(resTable, file=csv.nm, row.names=F);
    mSetObj <- .get.mSet(mSetObj);
    if(is.null(mSetObj$imgSet$enrTables)){
        mSetObj$imgSet$enrTables <- list();
    }
    mSetObj$imgSet$enrTables[[vis.type]] <- list();
    mSetObj$imgSet$enrTables[[vis.type]]$table <- resTable;
    mSetObj$imgSet$enrTables[[vis.type]]$res.mat <- res.mat;
    mSetObj$imgSet$enrTables[[vis.type]]$current.set <- current.geneset;
    mSetObj$imgSet$enrTables[[vis.type]]$hits.query <- hits.query;
    mSetObj$imgSet$enrTables[[vis.type]]$current.setids <- current.setids;

    mSetObj$imgSet$enrTables[[vis.type]]$library <- fun.type;
    print(paste0("funType=",fun.type));
    print(paste0("vis.type=", vis.type));
    mSetObj$imgSet$enrTables[[vis.type]]$algo <- "Overrepresentation Analysis";
    .set.mSet(mSetObj);
    return(1);
}

# these are geneset libraries
.load.enrich.lib<-function(org.code, fun.type){
    # prepare lib
    is.go <- FALSE;
    if(tolower(fun.type) == 'kegg'){ 
        nm <- "kegg";
    }else if(tolower(fun.type) == 'reactome'){ 
        nm <- "reactome";
    }else if(tolower(fun.type) == 'motif'){ 
        nm <- "motif_set";
    }else{ # GO
        is.go <- TRUE;
        nm <- paste0("go_", tolower(fun.type));
    }
    lib.nm <- paste0(nm, ".qs");
    sub.dir <- paste0("genesets/", org.code);
    my.lib <- .get.my.lib(lib.nm, sub.dir);
    if(is.go){ # fix some issue in go lib
        if(is.null(names(my.lib))){
            names(my.lib) <- c("link", "term", "sets");
        }
    }

    current.setlink <- my.lib$link;
    current.mset <- my.lib$sets;
    set.ids<- names(current.mset); 
    names(set.ids) <- names(current.mset) <- my.lib$term;

    current.setlink <<- current.setlink;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(unlist(current.geneset));
}


# these are geneset libraries
.load.enrich.compound.lib<-function(org.code, fun.type){
    # prepare lib
    is.go <- FALSE;
    mSetObj <- .get.mSet();
    if(tolower(fun.type) == 'smpdb'){ 
        nm <- mSetObj$org;
        sub.dir <- paste0("smpdb");
    }else if(tolower(fun.type) == 'keggc'){ 
        nm <- mSetObj$org;
        sub.dir <- paste0("kegg/metpa");
    }

    lib.nm <- paste0(nm, ".qs");

    my.lib <- .get.my.lib(lib.nm, sub.dir);

    current.setlink <- my.lib$link;
    current.mset <- my.lib$mset.list;
    set.ids<- my.lib$path.ids;

    current.setlink <<- current.setlink;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(names(unlist(unname(current.geneset))));
}