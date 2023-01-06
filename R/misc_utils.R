##################################################
## R scripts for ExpressAnalyst 
## Various utility methods
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

SetLibPath <- function(path){
  api.lib.path <<- path;
}

#Set path of user folder in tool
SetUserFolderPath <- function(path){
  user.path <<- path;
}

GetExtendRange<-function(vec, unit=10){
  var.max <- max(vec);
  var.min <- min(vec);
  exts <- (var.max - var.min)/unit;
  c(var.min-exts, var.max+exts);
}

# given a data with duplicates, dups is the one with duplicates
RemoveDuplicates <- function(data, lvlOpt, quiet=T, paramSet, msgSet){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");

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
      if(paramSet$numOfLists == 1){
        msgSet$current.msg <- paste(msgSet$current.msg, paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""), collapse="\n");
      }else{
        msgSet$current.msg <- paste(msgSet$current.msg, paste0("<b>", dataSet$listInx, "</b> : ", length(data), " genes;"), collapse="\n");
      }
    }
    saveSet(msgSet, "msgSet");
    return(list(uniq.data,msgSet));
  }else{
    if(!quiet){
      if(paramSet$numOfLists == 1){
        msgSet$current.msg <- paste(msgSet$current.msg, "All IDs are unique.", collapse="\n");
      }else{
        msgSet$current.msg <- paste(msgSet$current.msg, paste0("<b>", dataSet$listInx, "</b> : ", length(data), " genes;"), collapse="\n");
      }
    }
    saveSet(msgSet, "msgSet");
    return(list(data,msgSet));
  }
} 

# utils to remove from
# within, leading and trailing spaces
# remove /
ClearFactorStrings<-function(cls.nm, query){
  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  
  # kill multiple white space
  query <- gsub(" +","_",query);
  # remove non alphabets and non numbers 
  query <- gsub("[^[:alnum:] ]", "_", query);
  
  # test all numbers (i.e. Time points)
  chars <- substr(query, 0, 1);
  num.inx<- chars >= '0' & chars <= '9';
  if(all(num.inx)){
    query <- as.numeric(query);
    nquery <- paste(cls.nm, query, sep="_");
    query <- factor(nquery, levels=paste(cls.nm, sort(unique(query)), sep="_"));
  }else{
    query[num.inx] <- paste(cls.nm, query[num.inx], sep="_");
    query <- factor(query);
  }
  return (query);
}

# borrowed from Hmisc
all.numeric <- function (x, what = c("test", "vector"), extras = c(".", "NA")){
  what <- match.arg(what)
  old <- options(warn = -1)
  on.exit(options(old));
  x <- sub("[[:space:]]+$", "", x);
  x <- sub("^[[:space:]]+", "", x);
  inx <- x %in% c("", extras);
  xs <- x[!inx];
  isnum <- !any(is.na(as.numeric(xs)))
  if (what == "test") 
    isnum
  else if (isnum) 
    as.numeric(x)
  else x
}

# need to obtain the full path to convert (from imagemagik) for cropping images
GetBashFullPath<-function(){
  path <- system("which bash", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find bash in the PATH!");
    return("NA");
  }
  return(path);
}


cleanMem <- function(n=8) { for (i in 1:n) gc() }

###########
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  print(lapply(dataSet, object.size));
  save.image("memcheck.RData");
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
ShowMemoryUse <- function(..., n=30) {
  require(pryr);
  sink(); # make sure print to screen
  print(mem_used());
  print(sessionInfo());
  print(.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n));
  print(warnings());
}

PerformHeatmapEnrichment <- function(dataName="", file.nm, fun.type, IDs){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  dataSet <- readDataset(dataName); #instead of function parameter
  anal.type <- paramSet$anal.type;
  if(IDs=="NA"){
    if(anal.type=="onedata"){
      gene.vec <- rownames(dataSet$sig.mat);
    }else if(anal.type=="metadata"){
      gene.vec <- rownames(analSet$meta.mat);
    }else{
      gene.vec <- rownames(paramSet$all.ent.mat);
    }
  }else{
    gene.vec <- unlist(strsplit(IDs, "; "));
  }
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  names(gene.vec) <- sym.vec;
  res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, gene.vec);
  if(res == 0){
    return(0);
  }else{
    return(1);
  }
}

.prepareEnrichNet<-function(dataSet, netNm, type, overlapType, analSet){
    if(!exists("my.enrich.net")){ 
        compiler::loadcmp("../../rscripts/ExpressAnalystR/R/_utils_enrichnet.Rc");    
    }
    return(my.enrich.net(dataSet, netNm, type, overlapType, analSet));
}

GetListEnrGeneNumber <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  mdata.all <- paramSet$mdata.all;
  anal.type <- paramSet$anal.type;
  all.enIDs <- NULL;
  listSizes <- list();
  if(anal.type == "genelist"){
    if(paramSet$numOfLists > 1){
      newDat <- list();
      tot.count <- 0;
      all.nms <- paramSet$listNms;
      for(i in 1:length(all.nms)){
        dataNm <- all.nms[i];
        dataSet <- readDataset(dataNm);
        gene.mat <- dataSet$prot.mat;
        
        # convert to entrez
        expr.val <- gene.mat[,1];
        en.ids <- rownames(gene.mat);
        
        names(expr.val) <- en.ids;
        newDat[[dataNm]] <- expr.val;
        names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org, paramSet$data.idType)
        all.enIDs <- c(all.enIDs, en.ids);
        listSizes[[i]] <- list(
          name = dataNm,
          label = dataNm,
          size = length(en.ids)
          #val = de.prct[i]
        )
      }
      
    }else{
      
      all.enIDs <- rownames(dataSet$prot.mat);
      names(all.enIDs ) <- doEntrez2SymbolMapping(all.enIDs, paramSet$data.org, paramSet$data.idType)
      listSizes[[1]] <- list(
        name = "datalist1",
        label = "datalist1",
        size = length(all.enIDs)
        #val = de.prct[i]
      )
    }
  }else if(anal.type == "onedata"){
    all.enIDs <- rownames(dataSet$sig.mat);
    names(all.enIDs) <- doEntrez2SymbolMapping(all.enIDs, paramSet$data.org, paramSet$data.idType)
    listSizes[[1]] <- list(
      name = "dataSet1",
      label = "dataSet1",
      size = length(all.enIDs)
      #val = de.prct[i]
    )
  }else{
    newDat <- list();
    tot.count <- 0;
    listSizes <- list();
    all.nms <- names(mdata.all);
    for(i in 1:length(all.nms)){
      dataNm <- all.nms[i];
      dataSet <- readDataset(dataNm);
      gene.mat <- dataSet$sig.mat;
      
      # convert to entrez
      expr.val <- gene.mat[,1];
      en.ids <- rownames(gene.mat);
      
      names(expr.val) <- en.ids;
      newDat[[dataNm]] <- expr.val;
      names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org)
      all.enIDs <- c(all.enIDs, en.ids);
      listSizes[[i]] <- list(
        name = dataNm,
        label = dataNm,
        size = length(en.ids)
      )
    }
  }
  analSet$list.genes <- all.enIDs;
  analSet$listSizes <- listSizes;
  saveSet(analSet, "analSet");
}

.getEnrNetList <- function(dataSet,analSet){
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  all.enIDs <- NULL;
  listSizes <- list();
  if(anal.type == "genelist"){
    if(paramSet$numOfLists > 1){
        dataSet <- readDataset(paramSet$selDataNm);
        gene.mat <- dataSet$prot.mat;
        
        # convert to entrez
        expr.val <- gene.mat[,1];
        en.ids <- rownames(gene.mat);
        
        names(expr.val) <- en.ids;
        names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org, paramSet$data.idType)
        all.enIDs <- en.ids;
        listSizes[[1]] <- list(
          name = paramSet$selDataNm,
          label = paramSet$selDataNm,
          size = length(en.ids)
        )
      
      
    }else{
      
      all.enIDs <- rownames(dataSet$prot.mat);
      names(all.enIDs ) <- doEntrez2SymbolMapping(all.enIDs, paramSet$data.org, paramSet$data.idType)
      listSizes[[1]] <- list(
        name = "datalist1",
        label = "datalist1",
        size = length(all.enIDs)
        #val = de.prct[i]
      )
    }
  }else if(anal.type == "onedata"){
    all.enIDs <- rownames(dataSet$sig.mat);
    names(all.enIDs) <- doEntrez2SymbolMapping(all.enIDs, paramSet$data.org, paramSet$data.idType)
    listSizes[[1]] <- list(
      name = "dataSet1",
      label = "dataSet1",
      size = length(all.enIDs)
      #val = de.prct[i]
    )
  }else{
    if(paramSet$selDataNm == "meta_default"){
      gene.mat <- analSet$meta.mat
      expr.val <- unname(analSet$meta.avgFC[rownames(gene.mat)])
      en.ids <- rownames(gene.mat);
      names(expr.val) <- en.ids;
      names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org, paramSet$data.idType)
      all.enIDs <- en.ids

    }else{
    tot.count <- 0;
    listSizes <- list();
      dataSet <- readDataset(paramSet$selDataNm);
      gene.mat <- dataSet$sig.mat;
      
      # convert to entrezs
      expr.val <- gene.mat[,1];
      en.ids <- rownames(gene.mat);
      
      names(expr.val) <- en.ids;
      names(en.ids) <- doEntrez2SymbolMapping(en.ids, paramSet$data.org, paramSet$data.idType)
      all.enIDs <- en.ids
      }
      listSizes[[1]] <- list(
        name = paramSet$selDataNm,
        label = paramSet$selDataNm,
        size = length(all.enIDs)
      )
    
  }
  analSet$list.genes <- all.enIDs;
  analSet$listSizes <- listSizes;
  return(analSet);
}


InitListEnrichment <- function(dataName, type){
  dataSet <- readDataset(dataName);
  analSet <- readSet(analSet, "analSet");

  analSet <- .getEnrNetList(dataSet, analSet);
  res <- .performEnrichAnalysis(dataSet, paste0("enrichment_", type), type, analSet$list.genes);
  if(res){
    res <- .prepareEnrichNet(dataSet, paste0('enrichNet_', type), 'list', "mixed", analSet);
  }
  return(res)
}

PerformListEnrichmentView <- function(dataName="", file.nm, fun.type, netNm, IDs){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  if(IDs == "gsea_prev"){
    analSet <- .getEnrNetList(dataSet, analSet);
  }else{
  gene.vec <- unlist(strsplit(IDs, "; "));
  gene.vec <- unique(gene.vec);
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  names(gene.vec) <- sym.vec;
  analSet$list.genes <- gene.vec
  saveSet(analSet, "analSet");
  }
  res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, analSet$list.genes);
  if(res){
    res <- .prepareEnrichNet(dataSet, netNm, 'list', "mixed", analSet);
  }
   
  return(res);

}

overlap_ratio <- function(x, y, type) {
  x <- unlist(x)
  y <- unlist(y)
  if(type == "mixed"){
    res <- 0.5 * length(intersect(x, y))/length(unique(y)) + 0.5 * length(intersect(x, y))/length(unique(x));
  }else if(type == "overlap"){
    if(length(x)>length(y)){
      res=length(intersect(x, y))/length(unique(y))
    }else{
      res=length(intersect(x, y))/length(unique(x))
    }
  }else{
    res=length(intersect(x, y))/length(unique(c(x,y)))
  }
  return(res)
}

color_scale <- function(c1="grey", c2="red") {
  pal <- colorRampPalette(c(c1, c2))
  colors <- pal(100)
  return(colors)
}


CalculateDEgeneSetEnr <- function(nms, operation, refNm, filenm){
  paramSet <- readSet(paramSet, "paramSet");;
  anal.type <- paramSet$anal.type;
  nms <- strsplit(nms, ";")[[1]];
  if(anal.type == "metadata" || anal.type == "onedata"){
    com.smbls <- PerformSetOperation_DataEnr(nms, operation, refNm);
  }else{
    com.smbls <- PerformSetOperation_ListEnr(nms, operation, refNm);
  }
  
  sink(filenm);
  cat(rjson::toJSON(com.smbls));
  sink();
}

PerformSetOperation_ListEnr <- function(nms, operation, refNm){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  anal.type <- paramSet$anal.type;
  all.nms <- names(mdata.all);
  include.inx <- all.nms %in% nms;
  my.vec <- all.nms[include.inx];
  if(anal.type == "onedata"){
    my.vec <- c("1");
  }
  if(operation == "diff"){ # make sure reference is the first
    inx <- which(my.vec == refNm);
    my.vec <- my.vec[-inx];
  }
  com.ids <- NULL;
  ids.list <- list()
  for(i in 1:length(my.vec)){
    if(anal.type != "onedata"){
      dataSet <- readDataset(my.vec[i]);
    }
    if(operation == "diff"){
      ids.list[[i]]=dataSet$GeneAnotDB[,"gene_id"]
      #com.ids <- setdiff(com.ids, dataSet$GeneAnotDB[,"gene_id"]);
    }else if(is.null(com.ids)){
      com.ids <- dataSet$GeneAnotDB[,"gene_id"];
    }else{
      if(operation == "intersect"){
        com.ids <- intersect(com.ids, dataSet$GeneAnotDB[,"gene_id"]);
      }else if(operation == "union"){
        com.ids <- union(com.ids, dataSet$GeneAnotDB[,"gene_id"]);
      }
    }
  }
  if(operation == "diff"){
    dataSet <- readDataset(refNm);
    ids <- unique(unlist(ids.list));
    com.ids <-setdiff(dataSet$GeneAnotDB[,"gene_id"], ids);
  }
  
  com.ids <- unique(as.character(com.ids[!is.na(com.ids)])); # make sure it is interpreted as name not index
  com.symbols <- doEntrez2SymbolMapping(com.ids, paramSet$data.org, paramSet$data.idType);
  names(com.symbols) <- com.ids
  
  com.symbols<-com.symbols[!is.null(com.symbols)];
  venn.genes <<- com.ids;
  return(com.symbols);
}

PerformSetOperation_DataEnr <- function(nms, operation, refNm){
  paramSet <- readSet(paramSet, "paramSet");
  anal.type <- paramSet$anal.type;
  my.vec <- nms
  if(operation == "diff"){ # make sure reference is the first
    inx <- which(my.vec == refNm);
    my.vec <- my.vec[-inx];
  }
  com.ids <- NULL;
  ids.list <- list()
  if(anal.type == "onedata"){
    my.vec <- "dat"
  }
  for(nm in my.vec){
    if(anal.type != "onedata"){
      dataSet <- readDataset(nm);
    }
    if(operation == "diff"){
      ids.list[[nm]]=rownames(dataSet$sig.mat);
      #com.ids <- setdiff(com.ids, dataSet$GeneAnotDB[,"gene_id"]);
    }else if(is.null(com.ids)){
      com.ids <- rownames(dataSet$sig.mat);
    }else{
      if(operation == "intersect"){
        com.ids <- intersect(com.ids, rownames(dataSet$sig.mat));
      }else if(operation=="union"){
        com.ids <- union(com.ids, rownames(dataSet$sig.mat));
      }
    }
  }
  if(operation == "diff"){
    dataSet <- readDataset(refNm);
    ids <- unique(unlist(ids.list));
    com.ids <-setdiff(rownames(dataSet$sig.mat), ids);
  } 
  com.symbols <- doEntrez2SymbolMapping(com.ids, paramSet$data.org, paramSet$data.idType);
  names(com.symbols) <- com.ids;
  venn.genes <<- com.ids;
  return(com.symbols);
}

# in public web, this is done by microservice
.perform.computing <- function(){
  dat.in <- qs::qread("dat.in.qs"); 
  dat.in$my.res <- dat.in$my.fun();
  qs::qsave(dat.in, file="dat.in.qs");    
}

fast.write <- function(dat, file, row.names=TRUE){
    tryCatch(
        {
           if(is.data.frame(dat)){
                # there is a rare bug in data.table (R 3.6) which kill the R process in some cases 
                data.table::fwrite(dat, file, row.names=row.names);
           }else{
                write.csv(dat, file, row.names=row.names);  
           }
        }, error=function(e){
            print(e);
            fast.write.csv(dat, file, row.names=row.names);   
        }, warning=function(w){
            print(w);
            fast.write.csv(dat, file, row.names=row.names); 
        });
}

fast.write.csv <- function(dat, file, row.names=TRUE){
    tryCatch(
        {
           if(is.data.frame(dat)){
                # there is a rare bug in data.table (R 3.6) which kill the R process in some cases 
                data.table::fwrite(dat, file, row.names=row.names);
           }else{
                write.csv(dat, file, row.names=row.names);  
           }
        }, error=function(e){
            print(e);
            write.csv(dat, file, row.names=row.names);   
        }, warning=function(w){
            print(w);
            write.csv(dat, file, row.names=row.names); 
        });
}



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# for scatter
gg_color_hue_scatter <- function(grp.num, type="green", filenm=NULL) {
  grp.num <- as.numeric(grp.num)
    if(type == "green"){
    pal18 <- c("#e6194B", "#3cb44b", "#4363d8", "#ffff00", "#f032e6", "#ffe119", "#911eb4", "#f58231", "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075");
    }else{
    pal18 <- c( "#4363d8","#e6194B" , "#3cb44b", "#f032e6", "#ffe119", "#e6194B", "#f58231", "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#42d4f4","#000075", "#ff4500");
    }
if(grp.num <= 18){ # update color and respect default
    colArr <- pal18[1:grp.num];
  }else{
    colArr <- colorRampPalette(pal18)(grp.num);
  }
  if(is.null(filenm)){
    return(colArr);
  }else{
    require(RJSONIO)
    sink(filenm);
    cat(toJSON(colArr));
    sink();
    return(filenm);
  }
}

PerformORA <- function(dataName="", file.nm, fun.type, IDs){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(dataName);
  gene.vec <- unlist(strsplit(IDs, "; "));
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  names(gene.vec) <- sym.vec;
  res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, gene.vec);
  return(res);
}

  GetGeneList <- function(dataSetObj=NA,fileNm, type){
    paramSet <- readSet(paramSet, "paramSet");
    mdata.all <- paramSet$mdata.all;
    all_str <- "";
    if(type == "genelist"){
      if(paramSet$numOfLists > 1){
        dataSet <- list();
        my.vec <- names(mdata.all);
        for(i in 1:length(my.vec)){
          datSet <- readDataset(my.vec[i]);
          if(i == 1){
            all_str = datSet$orig
          }else{
            all_str = paste0(all_str, "\n//", datSet$orig)
          }
        }
      }else{
        all_str = dataSet$orig;
      }
    }else if(type == "onedata"){
      require(readr);
      dataSet <- readDataset(dataName);
      sig.ids <- rownames(dataSet$sig.mat);
      if("logFC" %in% colnames(dataSet$sig.mat)){
        stat.fc <- dataSet$sig.mat$logFC; 
      }else{
        stat.fc <- dataSet$sig.mat[,paramSet$selectedFactorInx]; 
      }
      df <- data.frame(ids=sig.ids, fc=stat.fc);
      all_str <- readr:::format_tsv(df);
      all_str <- paste0("#", all_str);
    }else{
      require(readr);
      my.vec <- names(mdata.all);
      for(i in 1:length(my.vec)){
        dataSet <- readDataset(my.vec[i]);
        sig.ids <- rownames(dataSet$sig.mat);
        stat.fc <- dataSet$sig.mat[,1];
        df <- data.frame(ids=sig.ids, fc=stat.fc);
        df_str <- readr:::format_tsv(df)
        df_str <- paste0("#", df_str);
        if(i == 1){
          all_str <- df_str;
        }else{
          all_str <- paste0(all_str, "\n//", df_str)
        }
      }
    }
    writeLines(all_str, fileNm)
    return(all_str);
  }

ReadList <- function(dataSetObj=NA, fullPath, fileNm){
    fullUrl <- url(paste0(fullPath,"/", fileNm))
    all_str <- paste0(readLines(fullUrl),collapse="\n");
    return(all_str);
}

GetDatNms <- function(){
    paramSet <- readSet(paramSet, "paramSet");;
    if(exists("paramSet$mdata.all")){
       mdata.all <- paramSet$mdata.all;
       if(length(names(mdata.all))>1){
       mdata.all.str <- paste0(names(mdata.all), collapse=";");
       }else if(length(names(mdata.all)) == 1){
       mdata.all.str <- names(mdata.all)
       }else{
       mdata.all.str <- "";
       }
    }else{
       mdata.all.str <- "";
    }
    return(mdata.all.str);
}

# Adds an error message
AddErrMsg <- function(msg){
  msgSet <- readSet(msgSet, "msgSet");
  msgSet$current.msg <- c(msgSet$current.msg, msg);
  print(msg);
  saveSet(msgSet, "msgSet");
}


rowcolFt =  function(x, fac, var.equal, which = 1L) {
  
  if(!(which %in% c(1L, 2L)))
    stop(sQuote("which"), " must be 1L or 2L.")
  
  if(which==2L)
    x = t(x)

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  sqr = function(x) x*x
  
  stopifnot(length(fac)==ncol(x), is.factor(fac), is.matrix(x))
  x   <- x[,!is.na(fac), drop=FALSE]
  fac <- fac[!is.na(fac)]

  ## Number of levels (groups)
  k <- nlevels(fac)

  ## xm: a nrow(x) x nlevels(fac) matrix with the means of each factor
  ## level
  xm <- matrix(
     sapply(levels(fac), function(fl) rowMeans(x[,which(fac==fl), drop=FALSE])),
     nrow = nrow(x),
     ncol = nlevels(fac))

  ## x1: a matrix of group means, with as many rows as x, columns correspond to groups 
  x1 <- xm[,fac, drop=FALSE]

  ## degree of freedom 1
  dff    <- k - 1

  if(var.equal){
    ## x0: a matrix of same size as x with overall means
    x0 <- matrix(rowMeans(x), ncol=ncol(x), nrow=nrow(x))
  
    ## degree of freedom 2
    dfr    <- ncol(x) - dff - 1

    ## mean sum of squares
    mssf   <- rowSums(sqr(x1 - x0)) / dff
    mssr   <- rowSums(sqr( x - x1)) / dfr

    ## F statistic
    fstat  <- mssf/mssr

  } else{

    ## a nrow(x) x nlevels(fac) matrix with the group size  of each factor
    ## level
    ni <- t(matrix(tapply(fac,fac,length),ncol=nrow(x),nrow=k))

    ## wi: a nrow(x) x nlevels(fac) matrix with the variance * group size of each factor
    ## level
    sss <- sqr(x-x1)
    x5 <- matrix(
       sapply(levels(fac), function(fl) rowSums(sss[,which(fac==fl), drop=FALSE])),
       nrow = nrow(sss),
       ncol = nlevels(fac))          
    wi <- ni*(ni-1) /x5

    ## u : Sum of wi
    u  <- rowSums(wi)

    ## F statistic
    MR <- rowSums(sqr((1 - wi/u)) * 1/(ni-1))*1/(sqr(k)-1)
    fsno <- 1/dff * rowSums(sqr(xm - rowSums(wi*xm)/u) * wi)
    fsdeno <- 1+ 2* (k-2)*MR
    fstat <- fsno/fsdeno

    ## degree of freedom 2: Vector with length nrow(x)
    dfr <- 1/(3 * MR)
  
  }
  
  res = data.frame(statistic = fstat,
                   p.value   = pf(fstat, dff, dfr, lower.tail=FALSE),
                   row.names = rownames(x))

  attr(res, "df") = c(dff=dff, dfr=dfr)
  return(res)
}

rowcoltt =  function(x, fac, tstatOnly, which, na.rm) {
    
  dyn.load(.getDynLoadPath());
  
  if (!missing(tstatOnly) && (!is.logical(tstatOnly) || is.na(tstatOnly)))
      stop(sQuote("tstatOnly"), " must be TRUE or FALSE.")
  
  f = checkfac(fac)
  if ((f$nrgrp > 2) || (f$nrgrp <= 0))
    stop("Number of groups is ", f$nrgrp, ", but must be >0 and <=2 for 'rowttests'.")

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  cc = .Call("rowcolttests", x, f$fac, f$nrgrp, which-1L, na.rm)
    
  res = data.frame(statistic = cc$statistic,
                   dm        = cc$dm,
                   row.names = dimnames(x)[[which]])

  if (!tstatOnly)
    res = cbind(res, p.value = 2*pt(abs(res$statistic), cc$df, lower.tail=FALSE))

  attr(res, "df") = cc$df    
  return(res)
}

checkfac = function(fac) {

  if(is.numeric(fac)) {
    nrgrp = as.integer(max(fac, na.rm=TRUE)+1)
    fac   = as.integer(fac)
  }
  ## this must precede the factor test
  if(is.character(fac))
    fac = factor(fac)

  if (is.factor(fac)) {
    nrgrp = nlevels(fac)
    fac   = as.integer(as.integer(fac)-1)
  } 
  if(!is.integer(fac))
    stop("'fac' must be factor, character, numeric, or integer.")
  
  if(any(fac<0, na.rm=TRUE))
    stop("'fac' must not be negative.")
    
  return(list(fac=fac, nrgrp=nrgrp))
}

.getDynLoadPath <- function() {

    path <- "../../rscripts/ExpressAnalystR/R/src/ExpressAnalyst.so";
    path <- normalizePath(path)
    
    return(path)
}

gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


# obtain a numeric matrix, exclude comments if any
.to.numeric.mat <- function(dat1){
  # now remove all comments in dat1
  # assign rownames after covert to matrix as data.frame does not allow duplicate names
  comments.inx <- grep("^#", dat1[,1]);
  row.nms <- dat1[-comments.inx,1];
  dat1<-dat1[-comments.inx,-1];
  dimensions <- dim(dat1)
  col.nms <- colnames(dat1)
  dat1 <- sapply(dat1, as.numeric);
  dat1 <- matrix(data=dat1, ncol=dimensions[2], nrow=dimensions[1])
  rownames(dat1) <- row.nms;
  colnames(dat1) <- col.nms;
  return(dat1);
}


# note, this may leads to duplicate names, use make.unque as last step
.cleanNames <- function(query, type){

  if(type=="sample_name"){
    query <- gsub("[^[:alnum:]./_-]", "", query);
  }else{
    query <- gsub("[^[:alnum:][:space:],'./_-]", "", query)
  }
  return(make.unique(query));
}

saveSet <- function(obj=NA, set="", output=1){

    if(globalConfig$anal.mode == "api"){ 
      qs:::qsave(obj, paste0(set, ".qs"));
      return(output)
    }else{
      if(set == "dataSet"){
        dataSet <<- obj;
      }else if(set == "analSet"){
        analSet <<- obj;
      }else if(set == "imgSet"){
        imgSet <<- obj;
      }else if(set == "paramSet"){
        head(paramSet);
        paramSet <<- obj;
      }else if(set == "msgSet"){
        msgSet <<- obj;
      }else if(set == "cmdSet"){
        cmdSet <<- obj;
      }

        if(globalConfig$anal.mode == "web"){
            return(output);
        }else{
            return(obj);
        }
    }

}

readSet <- function(obj=NA, set=""){
    if(globalConfig$anal.mode == "api"){
      path <- "";
      if(exists('user.path')){
        path <- user.path;
      }

      if(path != ""){
        obj <- load_qs(paste0(path, set, ".qs"));
      }else{
        obj <- qs:::qread(paste0(set, ".qs"));
      }
    }
    return(obj);
}

load_qs <- function(url) qs::qdeserialize(curl::curl_fetch_memory(url)$content)

readDataset <- function(fileName=""){
    if(globalConfig$anal.mode == "api"){
      if(exists('user.path')){
        path <- user.path;
        obj <- load_qs(paste0(path, fileName));
      }else{
        obj <- qs:::qread(fileName);
      }
    }else{
       obj <- dataSets[[fileName]];
    }

    return(obj);
}

getCurrentMsg <- function(){
    msgSet <- readSet(msgSet, "msgSet");
    return(msgSet$current.msg);
}

PrepareSqliteDB <- function(sqlite_Path, onweb = TRUE) {
  if(onweb) {return(TRUE)};
  if(file.exists(sqlite_Path)) {return(TRUE)};

  dbNM <- basename(sqlite_Path);
  DonwloadLink <- paste0("https://www.xialab.ca/resources/sqlite/", dbNM);
  download.file(DonwloadLink, sqlite_Path);
  return(TRUE)
}

saveDataQs <-function(data, name, module.nm, dataName){
  if(module.nm == "metadata"){
    qs::qsave(data, file=paste0(dataName, "_data/", name));
  }else{
    qs::qsave(data, file=name);
  }
}

readDataQs <-function(name, module.nm, dataName){
  if(module.nm == "metadata"){
    dat <- qs::qread(file=paste0(dataName, "_data/", name));
  }else{
    dat <- qs::qread(file=name);
  }
  return(dat);
}
