##################################################
## R script for ExpressAnalyst
## Description: Compute upset diagram
## Authors: 
## G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#'Prepare data for Upset diagram
#'@param dataSetObj Input the name of the created dataSetObj (see Init.Data)
#'@param fileNm file name of the json file output 
#'@export
PrepareUpsetData <- function(fileNm){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  mdata.all <- paramSet$mdata.all;
  anal.type <- paramSet$anal.type;
  
  newDat <- list();

  # selected dataset or comparisons for onedata (single gene expression matrix)
  if(anal.type == "metadata"){
  hit.inx <- mdata.all==1;
  sel.nms <- names(mdata.all)[hit.inx];
  }else if(anal.type == "onedata"){
  
  }else{
  sel.nms <- names(mdata.all)
  }
  sel.dats <- list();
  

    if(!exists("analSet$inmex.de")){
      analSet$inmex.de <- list();
    }

  # populate gene lists for upset plot based on selected names
  for(nm in sel.nms){
    if(anal.type == "metadata"){
      dataSet <- readDataset(nm);
      sel.dats[[nm]] <- rownames(dataSet$sig.mat);
    }else if(anal.type == "onedata"){

    }else{
      dataSet <- readDataset(nm);
      gene.mat <- dataSet$prot.mat;
      
      # convert to entrez
      expr.val <- gene.mat[,1];
      en.ids <- rownames(gene.mat);
      
      names(expr.val) <- en.ids;
      analSet$inmex.de[[nm]] <- en.ids;
      sel.dats[[nm]] <- en.ids;
    }

  }

  if(anal.type == "metadata" & paramSet$meta.selected){
    sel.dats[["meta_dat"]] <- as.character(analSet$meta.stat$de);
  }
  
  if(length(sel.dats) == 0){
    AddErrMsg("No signficant features for any dataset!");
    return(0);
  }
  
  sums <- unlist(lapply(sel.dats, length));
  names <- unlist(lapply(sel.dats, paste, collapse = ", "));
  if(anal.type == "metadata"){
    metasum <- length(analSet$meta.stat$idd);
    metaname <- paste(analSet$meta.stat$idd, collapse = ", ");
    allsums <- c(sums, metasum);
    allnames <- c(names, metaname);
  }else{
    allsums <- c(sums);
    allnames <- c(names);
  }


  require(reshape2)
  df <- reshape::melt(sel.dats, value.name="id")
  colnames(df) <- c("name", 'set')
  uniq.nms <- unique(df$name)
  new.df <- dcast(df, name ~ set, value.var='set', fill=0)
  rownames(new.df) <- new.df[,1]
  new.df <- new.df[,-1, drop=F]
  
  gene.map <-  queryGeneDB("entrez", paramSet$data.org);
  gene.map[] <- lapply(gene.map, as.character)
  
  json.list <- list()
  for(i in 1:nrow(new.df)){
    json.list[[i]] <- list()
    json.list[[i]][["sets"]] <- new.df[i,][new.df[i,] != 0]
    entrez.vec <- rownames(new.df)[i];
    hit.inx <- match(entrez.vec, gene.map[, "gene_id"]);
    symbols <- gene.map[hit.inx, "symbol"];
    
    # if not gene symbol, use id by itself
    na.inx <- is.na(symbols);
    symbols[na.inx] <- entrez.vec[na.inx];
    json.list[[i]][["name"]] <- symbols;
    json.list[[i]][["entrez"]] <- entrez.vec;
  }
  
  col.vec <-gg_color_hue(length(sel.dats));
  
  jsonNm <- paste0(fileNm, ".json")
  json.mat <- RJSONIO::toJSON(list(json.list, col.vec));
  sink(jsonNm);
  cat(json.mat);
  sink();
  
  return(1); 
}