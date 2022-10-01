##################################################
## R script for ExpressAnalyst
## Description: Data/resource management functions
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

# init resources for analysis

#'Initialize resources for analysis
#'@description call this function before performing any analysis
#'@param path path pointing to different built-in resources
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Init.Data <-function(onWeb=T, dataPath="data/"){
  path = "../../";
<<<<<<< HEAD

=======
  print(dataPath);
>>>>>>> 8b55fee1c60bfcb1a28c5625efc373c0d55f401b
  paramSet <- list(annotated=FALSE);
  paramSet$on.public.web <- onWeb;

  if(paramSet$on.public.web){
  dataSet <<- list(annotated=FALSE);
  analSet <<- list(annotated=FALSE);
  paramSet <<- list(annotated=FALSE);
  msgSet <<- list(annotated=FALSE);
  cmdSet <<- list(annotated=FALSE);
  }else{
  dataSet <- list(annotated=FALSE);
  analSet <- list(annotated=FALSE);
  msgSet <- list(annotated=FALSE);
  cmdSet <- list(annotated=FALSE);
  }
  paramSet$partialToBeSaved <- c("Rload.RData", "Rhistory.R");

  Sys.setenv("OMP_NUM_THREADS" = 2); # need to control parallel computing for some packages
  paramSet$init.lib <- "kegg";
  paramSet$selectedFactorInx <- 1; #in multi comparison (i.e pairwise, time-series) which contrast is used
  analSet$net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = 1));
  msgSet$summaryVec <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "NA"); 
  analSet$enr.mat <- NULL;
  paramSet$numOfLists <- 1;
  paramSet$gseaRankOpt <- "pval";
  paramSet$data.idType <- "";
  paramSet$pvalu <- 0.05;
  paramSet$selDataNm <- "meta_default";
  paramSet$mdata.all <- list();
  paramSet$anal.type <- "onedata";

  #if(!paramSet$on.public.web){
    dataSets <<- list();
  #}

  dataSet$jsonNms <- list()

  if(file.exists("/home/glassfish/sqlite/")){
    sqlite.path <- "/home/glassfish/sqlite/";  #public server
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){
    sqlite.path <- "/Users/xia/Dropbox/sqlite/"; #xia local
  }else if(file.exists("/Users/jeffxia/Dropbox/sqlite/")){
    sqlite.path <- "/Users/jeffxia/Dropbox/sqlite/"; #xia local2
  }else if(file.exists("/media/zzggyy/disk/sqlite/")){  
    sqlite.path <-"/media/zzggyy/disk/sqlite/"; #zgy local
  }else if(file.exists("/home/le/sqlite/expressanalystdatabase/")){
    sqlite.path <- "/home/le/sqlite/expressanalystdatabase/"; #le local
  }else if(file.exists("/home/zgy/sqlite/")){
    sqlite.path <-"/home/zgy/sqlite/"; #zgy local
  }else if(file.exists("/Users/jessicaewald/sqlite/")){ # ewald local
    sqlite.path <- "/Users/jessicaewald/sqlite/"
  }

  paramSet$sqlite.path <- sqlite.path;
  paramSet$lib.path <- paste0(path, dataPath);
  paramSet$data.org <- "hsa";
  paramSet$module.count <- 0;
  msgSet$current.msg <- vector(mode="character");
  msgSet$msg.list <- list(); #numbered list, each element: function name, line number, time stamp, severity

  # preload some general package
  require('Cairo');
  CairoFonts("Arial:style=Regular","Arial:style=Bold","Arial:style=Italic","Helvetica","Symbol")
  require('igraph');
  print("called expressanalyst init!");

  #saveSet(dataSet, "dataSet");
  saveSet(paramSet, "paramSet");
  saveSet(msgSet, "msgSet");
  saveSet(analSet, "analSet");
  saveSet(cmdSet, "cmdSet");

  return(1);
}

# genelist, onedata, metadata
# also set up or clear the other global objects
SetAnalType <- function(analType){
  paramSet <- readSet(paramSet, "paramSet");
  paramSet$anal.type <- analType;
  paramSet$mdata.all <- list();
  paramSet$meta.selected <- TRUE;
  paramSet$meta.upload <- FALSE; # when upload merged data from meta-analysis b4
  if(analType == "metadata"){
    paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, "inmex_meta.qs")
  }
  saveSet(paramSet, "paramSet");
}


# When multiple genelists/datasets/results, record their name and save the data as .RDS file
# a) Current dataSet object
# Note, the memory will only contain one dataSet object. By default, the last one will be the current dataSet object;
# Users can switch this (from the interface) to specify which data is load into memory (dataSet object)
# b) Include for certain analysis
# For chord and heatmap analysis, users can do multiple selection (include)
# All datasets are selected by default (1 for selected, 0 for unselected)

# note, dataSet need to have "name" property
RegisterData <- function(dataSet){
  dataName <- dataSet$name;
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  mdata.all <- paramSet$mdata.all;

  if(anal.type == "metadata"){
    mdata.all[[dataName]] <- 1;
  }else{
    mdata.all[[dataName]] <- 1;
  }

  qs::qsave(dataSet, file=dataName);
  paramSet$mdata.all <- mdata.all;
  saveSet(paramSet, "paramSet");
  if(!paramSet$on.public.web){
  #dataSets[dataSet$name] <- dataSet
  #return(dataSets)
  }else{
  #return(.set.mSet(dataSet));
  }
  return(1);
} 

# remove data object, the current dataSet will be the last one by default
RemoveData <- function(dataName){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;

  if(!is.null(mdata.all[[dataName]])){
    mdata.all[[dataName]] <- NULL;
  }
  paramSet$mdata.all <- mdata.all;
  saveSet(paramSet, "paramSet");
}

# users can select one or more data for analysis
# note, we use 1 to indicate this is selected
# and by default is all selected.
SelectDataSet <- function(){
  if(!exists('nm.vec')){
    msgSet <- readSet(msgSet, "msgSet");
    msgSet$current.msg <-"No dataset is selected for analysis!";
    saveSet(msgSet, "msgSet");
    return(0);
  }

  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;

  all.nms <- names(mdata.all);
  for(nm in all.nms){
    if(nm %in% nm.vec){
      mdata.all[[nm]] <- 1;
    }else{
      mdata.all[[nm]] <- 0;
    }
  }
  paramSet$mdata.all <- mdata.all;
  
  rm('nm.vec', envir = .GlobalEnv);
  return(1);
}

GetAllDataNames <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  names(paramSet$mdata.all);
}

SetOrganism <- function(org){
  paramSet <- readSet(paramSet, "paramSet");;
  paramSet$data.org <- org;
  saveSet(paramSet, "paramSet");
}

SetSelectedFactorInx <- function(inx){
  paramSet <- readSet(paramSet, "paramSet");
  paramSet$selectedFactorInx <- inx;
  saveSet(paramSet, "paramSet");
}

SetSelNetDataset <- function(type){
  selectedNetDataset <<- type;
}

SetSelMultiNet <- function(type){
  selectedMultiNet <<- type;
}

SetRankingMetric <- function(opt){
  paramSet <- readSet(paramSet, "paramSet");
  paramSet$gseaRankOpt <- opt;
  saveSet(paramSet, "paramSet");

}


SetListNms <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  anal.type <- paramSet$anal.type;
  newDat <- list();
  tot.count <- 0;
  listSizes <- list();
  
  # convert to entrez
  if(anal.type == "metadata"){
    inmex.meta <- qs::qread("inmex_meta.qs");
    en.ids <- rownames(inmex.meta$data);
    nm <- "meta_data"
  }else{
    en.ids <- rownames(dataSet$comp.res)
    nm <- "dataSet"
  }
  names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org, paramSet$data.idType)
  
  listSizes[[1]] <- list(
    name = nm,
    label = nm,
    size = length(en.ids)
  );
  
  analSet$list.genes <- en.ids;
  analSet$listSizes <- listSizes;
  return(analSet);
}

# note: hit.query, resTable must synchronize
# ora.vec should contains entrez ids, named by their gene symbols
.performEnrichAnalysis <- function(dataSet, file.nm, fun.type, ora.vec){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");

  require(dplyr)
    # prepare lib
  setres <- .loadEnrichLib(fun.type, paramSet)
  current.geneset <- setres$current.geneset;

  # prepare query
  ora.nms <- names(ora.vec);
  
  # need to cut to the universe covered by the pathways, not all genes
  current.universe <- unique(unlist(current.geneset)); 
  hits.inx <- ora.vec %in% current.universe;
  ora.vec <- ora.vec[hits.inx];
  ora.nms <- ora.nms[hits.inx];
  
  # also make sure universe and pathways only contain genes measured in experiment
  if(!is.null(dataSet$data.anot)){
    current.universe <- current.universe[current.universe %in% rownames(dataSet$data.anot)]
    current.geneset <- lapply(current.geneset, function(x){x[x %in% rownames(dataSet$data.anot)]})
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
  }else{
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
  csv.nm <- paste(file.nm, ".csv", sep="");    
  fast.write(resTable, file=csv.nm, row.names=F);

  saveSet(msgSet, "msgSet");
  return(1);
}

GetCurrentJson <-function(type){
  dataSet <- .get.mSet(NA);
  return(dataSet$jsonNms[[type]]);
}

PrepareJsonFromR <- function(fileNm, type, jsonString, dataSetString){
    # rjson bug use RJSONIO
    dataSet <- RJSONIO::fromJSON(dataSetString);
    sink(fileNm);
    cat(jsonString);
    sink();
    return(1)
}

# table.nm is the org code used for sqlite table (ppi)
# for chem type, table.nm is drugbank or ctd
# note, last two param only for STRING database

.prepareSigProteinJSON <- function(){
    paramSet <- readSet(paramSet, "paramSet");;
    anal.type <- paramSet$anal.type;
    if(anal.type == "genelist"){
        result.list <- .prepareListSeeds();
    }else{ # single expression data or meta.mat
        result.list <- .prepareExpressSeeds();
    }
    return(result.list);
}

.prepareListSeeds <- function(){

    protein.list <- list();
    gene.list <- list();
    paramSet <- readSet(paramSet, "paramSet");
    msgSet <- readSet(msgSet, "msgSet");

    mdata.all <- paramSet$mdata.all;

    if(paramSet$numOfLists > 1){
        if(selectedNetDataset %in% c("intersect","union")){
            dataSet <- list();
            dataSet$name <- selectedNetDataset
            my.vec <- names(mdata.all);
            com.ids <- NULL;
            list.vec <- list()
            for(i in 1:length(my.vec)){
                datSet <- qs::qread(my.vec[i]);
                if(is.null(com.ids)){
                  com.ids <- datSet$GeneAnotDB[,"gene_id"];
                  prot.mat <- datSet$prot.mat
                  list.vec[[i]] <- com.ids
                }else{
                  if(selectedNetDataset == "intersect"){
                    com.ids <- datSet$GeneAnotDB[,"gene_id"];
                    list.vec[[i]] <- com.ids
                  }else{
                    com.ids <- union(com.ids, datSet$GeneAnotDB[,"gene_id"]);
                  }
                    prot.mat <- rbind(prot.mat, as.matrix(datSet$prot.mat[rownames(datSet$prot.mat) %in% com.ids,]))
                }
           }
            if(selectedNetDataset == "intersect"){
            com.ids <- Reduce(intersect, list.vec)
            prot.mat <- as.matrix(datSet$prot.mat[rownames(datSet$prot.mat) %in% com.ids,])
            }else{
            com.ids <- unique(as.character(com.ids[!is.na(com.ids)])); # make sure it is interpreted as name not index
            }

            com.symbols <- doEntrez2SymbolMapping(com.ids, paramSet$data.org, paramSet$data.idType);
            dataSet$GeneAnotDB <- data.frame(gene_id=com.ids, accession=com.symbols);
            dataSet$prot.mat <- prot.mat;
            dataSet$sig.mat <- prot.mat
            dataSet$seeds.proteins <- com.ids
        }else{
           my.vec <- names(mdata.all);
           # make sure reference is the first
           inx <- which(my.vec == selectedNetDataset);
           my.vec <- my.vec[-inx];
           com.ids <- NULL;
           ids.list <- list()
           for(i in 1:length(my.vec)){
                dataSet <- qs::qread(my.vec[i]);
                ids.list[[i]]=dataSet$GeneAnotDB[,"gene_id"]
           }
            dataSet <- qs::qread(selectedNetDataset);
            ids <- unique(unlist(ids.list));
            com.ids <-setdiff(dataSet$GeneAnotDB[,"gene_id"], ids);
            prot.mat <- as.matrix(dataSet$prot.mat[which(rownames(dataSet$prot.mat) %in% com.ids),])
            com.symbols <- doEntrez2SymbolMapping(com.ids, paramSet$data.org, paramSet$data.idType);
            dataSet$GeneAnotDB <- data.frame(gene_id=com.ids, accession=com.symbols);
            dataSet$prot.mat <- prot.mat;
            dataSet$sig.mat <- prot.mat
            dataSet$seeds.proteins <- com.ids
        }
    }

    # return a json array object
    # each object for a single dataset its sig proteins
    meta.vec <- meta.gene.vec <- meta.seed.expr <- NULL;
    file.create("seed_proteins.txt");
    GeneAnotDB <- NULL;

    gene.mat <- dataSet$sig.mat;
    prot.mat <- dataSet$prot.mat;
    write(paste("#DataSet:", dataSet$name),file="sig_genes.txt",append=TRUE);
    write.table(dataSet$sig.mat, file="sig_genes.txt", append=TRUE);

    meta.gene.vec <- c(meta.gene.vec, rownames(gene.mat));
    gene.list[[dataSet$name]] <- list(gene=rownames(gene.mat),logFC=unname(gene.mat[,1]));
    GeneAnotDB <- rbind(GeneAnotDB, dataSet$GeneAnotDB);
    meta.seed.expr <- c(meta.seed.expr, prot.mat[,1]);
    write(paste("#DataSet:", dataSet$name),file="seed_proteins.txt",append=TRUE);
    write.table(cbind(Emblprotein=rownames(prot.mat), Expression=prot.mat[,1]), file="seed_proteins.txt", row.names=F, quote=F,append=TRUE);
    protein.vec <- prot.mat[,1];
    meta.vec <- c(meta.vec, names(protein.vec));
    if(length(protein.vec) == 1){
        protein.vec <- as.matrix(protein.vec)
    }
    protein.list[[dataSet$name]] <- signif(protein.vec, 3);

    gene.list$name <- dataSet$name;
    #seed.genes <<- unique(meta.gene.vec);

    meta.seed.df <- as.matrix(meta.seed.expr);
    rownames(meta.seed.df) <- names(meta.seed.expr);

    res <- RemoveDuplicates(meta.seed.df, "max", quiet=F, paramSet, msgSet);
    seed.expr <- res[[1]];
    msgSet <- res[[2]];
    #seed.expr <<- seed.expr[,1];
    protein.vec <- unique(meta.vec);

    result <- list(
        gene.list = gene.list,
        protein.list = protein.list,
        protein.vec = protein.vec
    );
    return(result)
}


UpdateSubnetStats <- function(){
    analSet <- readSet(analSet, "analSet");
    ppi.comps <- analSet$ppi.comps;
    old.nms <- names(ppi.comps);
    net.stats <- ComputeSubnetStats(ppi.comps);
    ord.inx <- order(net.stats[,1], decreasing=TRUE);
    net.stats <- net.stats[ord.inx,];
    rownames(net.stats) <- old.nms[ord.inx];
    analSet$net.stats <- net.stats;
    saveSet(analSet, "analSet");
}


# read the uploaded data into memory
# return the meta-data information (multiple groups)
ReadDataForMetaInfo<-function(dataName){
    dataSet <- qs::qread(dataName);
    
    return(colnames(dataSet$meta));
}

doScatterJson <- function(dataName, filenm){
    dataSet <- readDataset(dataName);
    if(!exists("my.json.scatter")){ # public web on same user dir
        compiler::loadcmp("../../rscripts/expressanalystr/_utils_scatter3d.Rc");    
    }
    return(my.json.scatter(dataSet, filenm));
}


.set.mSet <- function(dataSetObj=NA){
  dataSet <<- dataSetObj;
  paramSet <- readSet(paramSet, "paramSet");

  if(paramSet$on.public.web){
    return (1);
  }else{
    return(dataSetObj);
  }
}

.get.mSet <- function(dataSetObj=NA){
  paramSet <- readSet(paramSet, "paramSet");
  if(paramSet$on.public.web && is.na(dataSetObj)){
    return(dataSet)
  }else{
    return(dataSetObj);
  }
}
