
my.reg.enrich <- function(dataSet, file.nm, fun.type, ora.vec, netInv){
  require(dplyr)
  
  paramSet <- readSet(paramSet, "paramSet");
  data.org <- paramSet$data.org;
  sqlite.path <- paramSet$sqlite.path;
  
  ora.nms <- names(ora.vec);
  # prepare for the result table
  set.size<-100;
  
  if (fun.type %in% c("chea", "encode", "jaspar", "trrust")){
    table.nm <- paste(data.org, fun.type, sep="_");
    res <- QueryTFSQLite(sqlite.path, table.nm, ora.vec);
    # no hits
    if(nrow(res)==0){ return(c(0,0)); }
    edge.res <- data.frame(gene=res[,"entrez"], symbol=res[,"symbol"],id=res[,"tfid"], name=res[,"tfname"]);       
    node.ids <- c(res[,"entrez"], res[,"tfid"]);
    node.nms <- c(res[,"symbol"], res[,"tfname"]);
    
  }else if(fun.type == "mirnet"){ 
    res <- QueryMirSQLite(sqlite.path, data.org, "entrez", ora.vec, "mirtarbase");
    if(nrow(res)==0){ return(c(0,0)); }
    edge.res <- data.frame(gene=res[,"entrez"], symbol=res[,"symbol"], id=res[,"mir_acc"], name=res[,"mir_id"] );
    node.ids <- c(res[,"entrez"], res[,"mir_acc"])
    node.nms <- c(res[,"symbol"], res[,"mir_id"]);
    
  }else if(fun.type == "met"){ 
    table.nm <- paste(data.org, "kegg", sep="_"); 
    res <- QueryMetSQLiteNet(sqlite.path, table.nm, ora.vec, "inverse");
    if(nrow(res)==0){ return(c(0,0)); }
    edge.res <- data.frame(gene=res[,"entrez"], symbol=res[,"symbol"], id=res[,"kegg"], name=res[,"met"] );
    node.ids <- c(res[,"entrez"], res[,"kegg"])
    node.nms <- c(res[,"symbol"], res[,"met"]);
    
  }else if(fun.type == "disease"){ # in miRNA, table name is org code, colname is id type
    res <- QueryDiseaseSQLite(sqlite.path, ora.vec);
    if(nrow(res)==0){ return(c(0,0)); }
    edge.res <- data.frame(gene=res[,"entrez"], symbol=res[,"symbol"], id=res[,"diseaseId"], name=res[,"diseaseName"] );
    node.ids <- c(res[,"entrez"], res[,"diseaseId"])
    node.nms <- c(res[,"symbol"], res[,"diseaseName"]);
    
  }else{
    table.nm <- paste("drug", data.org, sep="_");
    ora.vec <- doEntrez2UniprotMapping(ora.vec);
    res <- QueryDrugSQLite(sqlite.path, ora.vec);
    if(nrow(res)==0){ return(c(0,0)); }
    entrez.vec <- .doGeneIDMapping(res[,"upid"], "uniprot", data.org, "vec")
    edge.res <- data.frame(gene=entrez.vec, symbol=res[,"symbol"], id=res[,"dbid"], name=res[,"dbname"] );
    node.ids <- c(entrez.vec, res[,"dbid"])
    node.nms <- c(res[,"symbol"], res[,"dbname"]);
  }
  
  edge.res$mix = paste0(edge.res[,1], edge.res[,3]);
  edge.res = edge.res[!duplicated(edge.res$mix),];
  
  row.names(edge.res) <- 1:nrow(edge.res);
  write.csv(edge.res, file="orig_edge_list.csv",row.names=FALSE);
  
  library(dplyr)
  
  # Count the frequency of each id along with the name
  id_freq <- edge.res %>%
    group_by(id, name) %>%
    summarise(freq_id = n()) %>%
    ungroup()
  
  # Count the frequency of each name
  name_freq <- edge.res %>%
    group_by(name) %>%
    summarise(freq_name = n()) %>%
    ungroup()
  
  # Merge the two frequencies based on the 'name'
  hit.freq.aggregate <- merge(id_freq, name_freq, by = "name")
  
  # Order the result in decreasing order of frequency
  hit.freq.aggregate <- hit.freq.aggregate[order(hit.freq.aggregate$freq_id, decreasing = TRUE), ]
  
  # Display the result
  hit.freq <- hit.freq.aggregate[,c(1:3)];
  colnames(hit.freq)[3] <- "freq"
    hits.gene = list()
  id_type = "entrez";
  if(id_type == "uniprot"){
    for(i in 1:nrow(hit.freq)){
      df = edge.res[which(edge.res$id == hit.freq$id[i]),];
      gene.vec = as.vector(df$gene);
      gene.vec = doEntrez2UniprotMapping(gene.vec)
      hits.gene[[i]]=gene.vec;
    }
  } else if(id_type == "entrez") {
    for(i in 1:nrow(hit.freq)){
      df = edge.res[which(edge.res$id == hit.freq$id[i]),];
      gene.vec = as.vector(df$gene);
      hits.gene[[i]]=gene.vec;
    }
  }else {
    for(i in 1:nrow(hit.freq)){
      df = edge.res[which(edge.res$id == hit.freq$id[i]),];
      gene.vec = as.vector(df$gene);
      gene.vec = doEntrez2EmblProteinMapping(gene.vec)
      hits.gene[[i]]=gene.vec;
    }
  }
  names(hits.gene) = hit.freq$name;
  
  resTable1 = data.frame(hit.freq$id, hit.freq$name, hit.freq$freq)
  colnames(resTable1) = c("Id", "Name", "Hits")
  resTable1 = resTable1[order(-resTable1$Hits),]
  current.msg <<- "Regulatory element enrichment analysis was completed";
  
  if(regBool == "true"){
    resTable1 = resTable1[which(resTable1$Hits == regCount),]
  }
  # write json
  fun.ids = resTable1[,1]; 
  fun.nms = resTable1[,2];
  fun.hits = resTable1[,3]; 
  json1.res <- list(
    fun.ids = fun.ids, 
    fun.nms = fun.nms,
    fun.hits = fun.hits,
    fun.genes = hits.gene 
  );
  json.mat <- rjson::toJSON(json1.res);
  json.nm <- paste(file.nm, ".json", sep="");
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  csv.nm <- paste(file.nm, ".csv", sep="");
  write.csv(resTable1, file=csv.nm, row.names=F);
  
  return(1);
}

