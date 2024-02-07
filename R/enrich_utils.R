##################################################
## R script for ExpressAnalyst
## Description: Functions for enrichment analysis (GSEA and ORA)
## Authors:
## G. Zhou, guangyan.zhou@mail.mcgill.ca
## Jeff Xia, jeff.xia@mcgill.ca
###################################################  
.performEnrichAnalysis <- function(dataSet, file.nm, fun.type, ora.vec, vis.type){
  #dataSet <<- dataSet;
  #file.nm <<- file.nm;
  #fun.type <<- fun.type;
  #ora.vec <<- ora.vec;
  #vis.type <<- vis.type;
  #save.image("enrich.RData");

  msgSet <- readSet(msgSet, "msgSet");
  paramSet <- readSet(paramSet, "paramSet");
  require(dplyr)
  # prepare lib
  setres <- .loadEnrichLib(fun.type, paramSet)
  current.geneset <- setres$current.geneset;
  
  # prepare query
  ora.nms <- names(ora.vec);
  
  if(is.null(ora.nms)){
    ora.nms <- ora.vec;
    names(ora.vec) <- ora.vec;
  }
  
  if(paramSet$universe.opt == "library"){
    current.universe <- unique(unlist(current.geneset));     
  }else{
    # cut to the universe to uploaded genes
    if(paramSet$anal.type == "onedata"){
      current.universe <- rownames(dataSet$data.anot); 
    }else if(paramSet$anal.type == "metadata"){
      inmex <- qs::qread("inmex_meta.qs");
      current.universe <- rownames(inmex$data); 
    }else{
      if(!is.null(paramSet$backgroundUniverse)){
        current.universe <- paramSet$backgroundUniverse;
      }else{
        current.universe <- unique(unlist(current.geneset)); 
      }
    }
  }
  
  # also make sure pathways only contain genes measured in experiment
  if(!is.null(dataSet$data.anot)){
    current.geneset <- lapply(current.geneset, function(x){x[x %in% current.universe]})
    inds <- lapply(current.geneset, length) > 0
    current.geneset <- current.geneset[inds]
  }
  
  # prepare for the result table
  set.size<-length(current.geneset);
  res.mat<-matrix(0, nrow=set.size, ncol=5);
  rownames(res.mat)<-names(current.geneset);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "FDR");
  
  q.size<-length(ora.vec);
  
  # get the matched query for each pathway
  hits.query <- lapply(current.geneset, 
                       function(x) {
                         ora.nms[ora.vec%in%unlist(x)];
                       }
  );
  
  qs::qsave(hits.query, "hits_query.qs");
  
  names(hits.query) <- names(current.geneset);
  hit.num<-unlist(lapply(hits.query, function(x){length(unique(x))}), use.names=FALSE);
  
  # total unique gene number
  uniq.count <- length(current.universe);
  
  # unique gene count in each pathway
  set.size <- unlist(lapply(current.geneset, length));
  
  res.mat[,1]<-set.size;
  res.mat[,2]<-q.size*(set.size/uniq.count);
  res.mat[,3]<-hit.num;
  
  # use lower.tail = F for P(X>x)
  raw.pvals <- phyper(hit.num-1, set.size, uniq.count-set.size, q.size, lower.tail=F);
  # Replace NaN values with 1
  raw.pvals[is.nan(raw.pvals)] <- 1

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

    res.mat.all <- as.data.frame(res.mat);
    res.mat.all$Pathway <- rownames(res.mat);
    res.mat.all$Genes <- rep("NA",nrow(res.mat))
    # Iterate through the list and add comma-separated values to the data frame
    for (name in names(hits.query)) {
      if (name %in% res.mat.all$Pathway) {
        res.mat.all[which(res.mat.all$Pathway == name), "Genes"] <- paste(hits.query[[name]], collapse = ",")
      }
    }

    res.mat.all <- res.mat.all[which(res.mat.all$Genes != "NA"), ];
    res.mat.all$Pathway <- NULL;
    resTable.all <- data.frame(Pathway=rownames(res.mat[which(res.mat.all$Genes != "NA"), ]), res.mat.all);
    csv.nm <- paste(file.nm, ".csv", sep="");    
    write.csv(resTable.all, file=csv.nm, row.names=F);
    
    imp.inx <- res.mat[,4] <= 0.05;
    imp.inx[is.na(imp.inx)] <- F
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
  }else{
    msgSet$current.msg <- "No overlap between queried genes and pathway library!"
    return(0);
  }
  
  #get gene symbols
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  qs:::qsave(res.mat, "enr.mat.qs");
  msgSet$current.msg <- "Functional enrichment analysis was completed";
  
  # write json
  fun.anot <- hits.query; 
  total <- resTable[,2]; if(length(total) ==1) { total <- matrix(total) };
  fun.pval <- resTable[,5]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  fun.padj <- resTable[,6]; if(length(fun.padj) ==1) { fun.padj <- matrix(fun.padj) };
  hit.num <- resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  fun.ids <- as.vector(setres$current.setids[names(fun.anot)]); 
  
  if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  json.res <- list(
    fun.link = setres$current.setlink[1],
    fun.anot = fun.anot,
    fun.ids = fun.ids,
    fun.pval = fun.pval,
    fun.padj = fun.padj,
    hit.num = hit.num,
    total= total
  );
  json.mat <- rjson::toJSON(json.res);
  json.nm <- paste(file.nm, ".json", sep="");
  
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  fun.hits <<- hits.query;
  fun.pval <<- resTable[,5];
  hit.num <<- resTable[,4];
  #csv.nm <- paste(file.nm, ".csv", sep="");    
  #fast.write(resTable, file=csv.nm, row.names=F);
  paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(json.nm))
  
  imgSet <- readSet(imgSet, "imgSet");
  rownames(resTable) <- NULL;
  imgSet$enrTables[[vis.type]] <- list()
  imgSet$enrTables[[vis.type]]$table <- resTable;
  imgSet$enrTables[[vis.type]]$library <- fun.type
  imgSet$enrTables[[vis.type]]$algo <- "Overrepresentation Analysis"
  
  saveSet(imgSet);
  saveSet(paramSet, "paramSet");
  
  saveSet(msgSet, "msgSet");
  return(1);
}

.loadEnrichLib <- function(fun.type, paramSet){
  
  if(paramSet$data.org == "generic"){
    folderNm <- paramSet$data.idType;
  }else{
    folderNm <- paramSet$data.org;
  }

  if(exists("api.lib.path")){
    lib.path <- api.lib.path;
  }else{
    lib.path <- paramSet$lib.path;
  }

  my.path <- paste(lib.path, folderNm, "/", fun.type, ".rds", sep="");
  if(!paramSet$on.public.web && !file.exists(platform.path)){
    nmdb <- basename(my.path);
    download.file(my.path, destfile = nmdb, method="libcurl", mode = "wb");
    my.path <- nmdb;
  }
  
  my.lib <- readRDS(my.path);
  
  if(substr(fun.type, 0, 2)=="go"){  
    if(is.null(names(my.lib))){ # some go lib does not give names
      names(my.lib) <- c("link", "term", "sets");
    }
  }
  
  current.geneset <- my.lib$sets;
  #remove empty pathways
  keep.inx <- lapply(current.geneset,length)>0
  current.geneset <- current.geneset[keep.inx]
  my.lib$term <- my.lib$term[keep.inx]
  set.ids<- names(current.geneset); 
  names(set.ids) <- names(current.geneset) <- my.lib$term;
  
  if(substr(fun.type, 0, 2)=="go"){
    names(current.geneset) = firstup(names(current.geneset))
    names(current.geneset) = gsub("-", "_", names(current.geneset))
    names(set.ids) = firstup(names(set.ids));
    names(set.ids) = gsub("-", "_", names(set.ids));
  }
  qs::qsave(current.geneset, "current_geneset.qs");
  res <- list();
  res$current.setlink <- my.lib$link;
  res$current.setids <- set.ids;
  res$current.geneset <- current.geneset;
  return(res);
}

GetRidgePlot <- function(dataName, imgNm = "abc", dpi=72, format="png", fun.type = "kegg", ridgeType = "ora", ridgeColor = "teal", gseaRankOpt="", sigLevel = 0.05, pwNum=20, inx = 1){
    dataSet <- readDataset(dataName);
    if(!exists("compute.ridgeline")){ # public web on same user dir
        compiler::loadcmp("../../rscripts/ExpressAnalystR/R/utils_ridgeline.Rc");    
    }
    return(compute.ridgeline(dataSet, imgNm, dpi, format, fun.type, ridgeType, ridgeColor, sigLevel, pwNum, inx));
}

PerformUpsetORA <- function(dataName="", file.nm, fun.type, IDs){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(dataName);
  gene.vec <- unlist(strsplit(IDs, "; "));
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  names(gene.vec) <- sym.vec;
  res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, gene.vec, "upset");
  return(res);
}
