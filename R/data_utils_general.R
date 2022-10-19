##################################################
## R script for ExpressAnalyst
## Description: Data/resource management functions
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

.onAttach <- function (libname, pkgname){
  .on.public.web <<- FALSE;
  k1 <- paste("ExpressAnalystR",
              utils::packageVersion( "ExpressAnalystR"),
              "initialized Successfully !")
  k0 <- "\n";
  packageStartupMessage(c(k1,k0));
}

#'Initialize resources for analysis
#'@description call this function before performing any analysis
#'@param onWeb whether the script is running in local or on web
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Init.Data <-function(onWeb=T, dataPath="data/"){
  path = "../../";

  paramSet <- list(annotated=FALSE);
  paramSet$on.public.web <- onWeb;
  .on.public.web <<- onWeb;
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

  paramSet$jsonNms <- list()

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

  if(!.on.public.web) {
    paramSet$sqlite.path <- paste0(getwd(), "/");
    paramSet$lib.path <- "https://www.expressanalyst.ca/ExpressAnalyst/resources/data/";
  } 

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

SetListNms <- function(dataSet){
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
    en.ids <- rownames(dataSet$comp.res);
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


GetCurrentJson <-function(type){
  paramSet <- readSet(paramSet, "paramSet");
  return(paramSet$jsonNms[[type]]);
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
    paramSet <- readSet(paramSet, "paramSet");
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
        compiler::loadcmp("../../rscripts/ExpressAnalystR/R/_utils_scatter3d.Rc");    
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


#'Record R Commands
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmd Commands 
#'@export
RecordRCommand <- function(cmd){
  cmdSet <- readSet(cmdSet, "cmdSet"); 
  cmdSet$cmdVec <- c(cmdSet$cmdVec, cmd);
  saveSet(cmdSet, "cmdSet");
  return(1);
}

SaveRCommands <- function(){
  cmdSet <- readSet(cmdSet, "cmdSet"); 
  cmds <- paste(cmdSet$cmdVec, collapse="\n");
  pid.info <- paste0("# PID of current job: ", Sys.getpid());
  cmds <- c(pid.info, cmds);
  write(cmds, file = "Rhistory.R", append = FALSE);
}

#'Export R Command History
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetRCommandHistory <- function(){
  cmdSet <- readSet(cmdSet, "cmdSet"); 
  if(length(cmdSet$cmdVec) == 0){
    return("No commands found");
  }
  return(cmdSet$cmdVec);
}

