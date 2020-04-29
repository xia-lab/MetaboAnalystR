##################################################
## R script for MetaboAnalyst
## Description: GO/Pathway ORA 
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

LoadKEGGLib.gene<-function(org.code){
    kegg.path <- paste(libs.path, org.code, "/kegg.rds", sep="");
    kegg.anot <- readRDS(kegg.path)
    current.setlink <- kegg.anot$link;
    current.mset <- kegg.anot$sets;
    set.ids<- names(current.mset); 
    names(set.ids) <- names(current.mset) <- kegg.anot$term;

    current.setlink <<- current.setlink;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(unlist(current.geneset));
}

LoadREACTOMELib.gene<-function(org.code){

    reactome.path <- paste(libs.path, org.code, "/reactome.rds", sep="");
    reactome.anot <- readRDS(reactome.path)
    current.mset <- reactome.anot$sets;
    set.ids<- names(current.mset); 
    names(set.ids) <- names(current.mset) <- reactome.anot$term;
    current.setlink <<- reactome.anot$link;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(unlist(current.geneset));
}

LoadMotifLib<-function(org.code){
    motif.path <- paste(libs.path, org.code, "/motif_set.rds", sep="");
    motif_set<-readRDS(motif.path);
    current.mset <- motif_set$set;
    set.ids<- names(current.mset); 
    names(set.ids) <- names(current.mset) <- motif_set$term;
    current.setlink <<- motif_set$link;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(unlist(current.geneset));
}

LoadGOLib<-function(org.code, onto){
    go.path <- paste(libs.path, org.code, "/go_", tolower(onto), ".rds", sep="");
    if(tolower(onto) == "bp"){
        go_bp <- readRDS(go.path);
        if(is.null(names(go_bp))){ # new go lib does not give names
            names(go_bp) <- c("link", "term", "sets");
        }
        current.link <- go_bp$link;
        current.mset <- go_bp$sets;
        set.ids<- names(current.mset); 
        names(set.ids) <- names(current.mset) <- go_bp$term;
    }else if(tolower(onto) == "mf"){
        go_mf <- readRDS(go.path);
        if(is.null(names(go_mf))){
            names(go_mf) <- c("link", "term", "sets");
        }
        current.link <- go_mf$link;
        current.mset <- go_mf$sets;
        set.ids<- names(current.mset); 
        names(set.ids) <- names(current.mset) <- go_mf$term;
    }else{
        go_cc <- readRDS(go.path);
        if(is.null(names(go_cc))){
            names(go_cc) <- c("link", "term", "sets");
        }
        current.link <- go_cc$link;
        current.mset <- go_cc$sets;
        set.ids<- names(current.mset); 
        names(set.ids) <- names(current.mset) <- go_cc$term;
    }
    current.setlink <<- current.link;
    current.setids <<- set.ids;
    current.geneset <<- current.mset;
    current.universe <<- unique(unlist(current.geneset));
}

# note: hit.query, resTable must synchronize
PerformNetEnrichment <- function(mSetObj=NA, file.nm, fun.type, IDs){
    # prepare query
    ora.vec <- unlist(strsplit(IDs, "; "));
    names(ora.vec) <- as.character(ora.vec);
    mSetObj <- .get.mSet(mSetObj);
    res <- PerformEnrichAnalysis(mSetObj$org, file.nm, fun.type, ora.vec);
    return(res);
}

# note: hit.query, resTable must synchronize
# ora.vec should contains entrez ids, named by their gene symbols
PerformEnrichAnalysis <- function(org.code, file.nm, fun.type, ora.vec){

    # prepare lib
    if(tolower(fun.type) == 'kegg'){ 
        LoadKEGGLib.gene(org.code);
    }else if(tolower(fun.type) == 'reactome'){ 
        LoadREACTOMELib.gene(org.code);
    }else if(tolower(fun.type) == 'motif'){ 
        LoadMotifLib(org.code);
    }else{ # GO
        LoadGOLib(org.code,fun.type);
    }

    # prepare query
    ora.nms <- names(ora.vec);

    # prepare for the result table
    set.size<-length(current.geneset);
    res.mat<-matrix(0, nrow=set.size, ncol=5);
    rownames(res.mat)<-names(current.geneset);
    colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "FDR");

    # need to cut to the universe covered by the pathways, not all genes 
    hits.inx <- ora.vec %in% current.universe;
    ora.vec <- ora.vec[hits.inx];
    ora.nms <- ora.nms[hits.inx];

    q.size<-length(ora.vec);

    # get the matched query for each pathway
    hits.query <- lapply(current.geneset, 
        function(x) {
            ora.nms[ora.vec%in%unlist(x)];
        }
    );

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

    #get gene symbols
    resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
    AddMsg("Functional enrichment analysis was completed");

    # write json
    fun.anot = hits.query; 
    fun.pval = resTable[,5]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
    hit.num = resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    fun.ids <- as.vector(current.setids[names(fun.anot)]); 
    if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
        json.res <- list(
                    fun.link = current.setlink[1],
                    fun.anot = fun.anot,
                    fun.ids = fun.ids,
                    fun.pval = fun.pval,
                    hit.num = hit.num
        );
     json.mat <- RJSONIO::toJSON(json.res, .na='null');
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
    return(1);
}
