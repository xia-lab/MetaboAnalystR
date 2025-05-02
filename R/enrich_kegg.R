#'Perform KEGG to compound name mapping
#'@param kegg.vec Input vector of KEGG compounds
#'@export
doKEGG2NameMapping <- function(kegg.vec){
  cmpd.map <- .get.my.lib("compound_db.qs");
  hit.inx <- match(tolower(kegg.vec), tolower(cmpd.map$kegg));
  nms <- cmpd.map[hit.inx, "name"];
  return(nms);
}

#'Performs KO enrichment analysis based on the KO01100 map
#'@description This function performs KO enrichment analysis based on the KO01100 map
#'and saves the .JSON file
#'@param mSetObj Input name of the created mSet Object
#'@param category Input the option to perform enrichment analysis, "pathway"
#'@param file.nm Input name of file to save
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PerformKOEnrichAnalysis_KO01100 <- function(mSetObj=NA, category, file.nm){
  
  mSetObj <- .get.mSet(mSetObj);
  LoadKEGGKO_lib(category);
  #if(enrich.type == "hyper"){ else PerformKOEnrichAnalysis_Table
  mSetObj <- PerformKOEnrichAnalysis_List(mSetObj, file.nm);
  
  if(.on.public.web == FALSE){
    return(.set.mSet(mSetObj)); 
  } else {
    .set.mSet(mSetObj)
    return(1L);
  }
}

#'Utility function for PerformKOEnrichAnalysis_KO01100
#'@description Please note: only return hits in map KO01100
#'@param file.nm Input the file name 
#'@param mSetObj mSetObj object
PerformKOEnrichAnalysis_List <- function(mSetObj, file.nm){
  save.image("list.RData");
  if(idtype == "cmpd"){
    current.set <- current.cmpd.set;
  } else if(idtype == "gene&cmpd"){
    matchidx <- match(names(current.cmpd.set), names(current.geneset))
    current.set <- list()
       # TO-DO: Fix code to handle case if length(current.cmpd.set) > length(current.geneset).
    #   Then, start loop with current.cmpd.set
    for(i in  1:length(current.geneset)){
      if(i %in% matchidx){
        cidx <- which(matchidx==i) 
        mergels <- c(current.cmpd.set[cidx][[1]], current.geneset[i][[1]])
        current.set[[names(current.cmpd.set[cidx])]] <- mergels
      } else{
        current.set[[names(current.geneset[i])]] <- current.geneset[i][[1]]
      }
    }
    # Add compound sets that did not match
    cidx <- which(is.na(matchidx))
if(length(cidx)>0){
    for(i in c(1:length(cidx))){
      current.set[[names(current.cmpd.set[cidx[i]])]] <- current.cmpd.set[cidx[i]][[1]]
    }
  } 
}else{
    current.set <- current.geneset;    
  } 
  current.universe <- unique(unlist(current.set));
  
  # prepare for the result table
  set.size<-length(current.set);
  res.mat<-matrix(0, nrow=set.size, ncol=5);
  rownames(res.mat)<-names(current.set);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "Pval", "FDR");
  
  # prepare query
  ora.vec <- NULL;
  exp.vec <- dataSet$data[,1]; # drop dim for json
  ora.vec <- names(exp.vec);
  
  # need to cut to the universe covered by the pathways, not all genes 
  hits.inx <- ora.vec %in% current.universe;
  ora.vec <- ora.vec[hits.inx];
  #ora.nms <- ora.nms[hits.inx];
  
  q.size<-length(ora.vec);
  
  # get the matched query for each pathway
  hits.query <- lapply(current.set, 
                       function(x) {
                         ora.vec[ora.vec%in%unlist(x)];
                       }
  );
  names(hits.query) <- names(current.set);
  
  hit.num<-unlist(lapply(hits.query, function(x){length(x)}), use.names=FALSE);
  
  # total unique gene number
  uniq.count <- length(current.universe);
  
  # unique gene count in each pathway
  set.size <- unlist(lapply(current.set, length));
  
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
  hits.all <- hits.query
  
  if(nrow(res.mat)> 1){
    # order by p value
    ord.inx<-order(res.mat[,4]);
    res.mat <- signif(res.mat[ord.inx,],3);
    hits.query <- hits.query[ord.inx];
    
    #imp.inx <- res.mat[,4] <= 0.01; # This is previous design [FILTER NON-SIGNIFICANT PATHWAY]
    imp.inx <- res.mat[,4] <= 1;
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
  
  mSetObj <- Save2KEGGJSON(mSetObj,hits.query, res.mat, file.nm, hits.all);

    mSetObj <- .get.mSet(mSetObj);
    vis.type <- "keggGlobal";
    if(is.null(mSetObj$imgSet$enrTables)){
        mSetObj$imgSet$enrTables <- list();
    }
    res.mat <- as.data.frame(res.mat);
    res.mat$Name <- rownames(res.mat);
    res.mat <- res.mat[c("Name", setdiff(names(res.mat), "Name"))]

  gene.vec <- current.universe;
  sym.vec <- doAllGeneIDMapping(gene.vec, mSetObj$org, "entrez");
  gene.nms <- sym.vec;

  current.geneset.symb <- lapply(current.geneset, 
                       function(x) {
                         gene.nms[gene.vec%in%unlist(x)];
  }
  );

    mSetObj$imgSet$enrTables[[vis.type]] <- list();
    mSetObj$imgSet$enrTables[[vis.type]]$table <- res.mat;
    mSetObj$imgSet$enrTables[[vis.type]]$res.mat <- res.mat;

    mSetObj$imgSet$enrTables[[vis.type]]$current.geneset <- current.set;
    mSetObj$imgSet$enrTables[[vis.type]]$hits.query <- hits.query;
    mSetObj$imgSet$enrTables[[vis.type]]$current.setids <- current.setids;
    mSetObj$imgSet$enrTables[[vis.type]]$current.geneset.symb <- current.geneset.symb;



    mSetObj$imgSet$enrTables[[vis.type]]$library <- "KEGG";
    mSetObj$imgSet$enrTables[[vis.type]]$algo <- "Overrepresentation Analysis";
    .set.mSet(mSetObj);
  return(mSetObj);
}

## Utility function for OrganizeJsonforNextwork
OrganizeJsonforNextwork <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  enrichPath <- RJSONIO::fromJSON("network_enrichment_pathway_0.json");

  my.hits <- mSetObj[["dataSet"]][["my.hits"]];
  hit.kos.res <- lapply(seq(my.hits$my.hits.genes), function(x){
    enIDs0 <- my.hits$my.hits.genes[[x]];
    enIDs <- gsub("[^0-9.-]","" ,unlist(strsplit(enIDs0, " ")));
    hit.kos <- unique(doGene2KONameMapping(enIDs));
    if(any(is.na(hit.kos))){hit.kos <- hit.kos[!is.na(hit.kos)]};
    return(hit.kos)
  })
  names(hit.kos.res) <- names(my.hits$my.hits.genes)
  hit.cmpd.res <- my.hits$my.hits.cmpds
  NMs <- names(hit.kos.res);
  if(any(NMs == "Glycolysis / Gluconeogenesis")){
    NMs[which(NMs == "Glycolysis / Gluconeogenesis")] <- "Glycolysis or Gluconeogenesis"
  }
  names(hit.kos.res) <- NMs;
  NMs2 <- names(hit.cmpd.res);
  if(any(NMs2 == "Glycolysis / Gluconeogenesis")){
    NMs2[which(NMs2 == "Glycolysis / Gluconeogenesis")] <- "Glycolysis or Gluconeogenesis"
  }
  names(hit.cmpd.res) <- NMs2;
  integPaths <- mSetObj[["dataSet"]][["integResGlobal"]];
  hit.res <- lapply(integPaths$pathways, function(x){
    c(hit.cmpd.res[[x]], hit.kos.res[[x]])    
  })
  names(hit.res) <- integPaths$pathways;
  
  enrichPath$hits.edge <- hit.kos.res;
  enrichPath$hits.node <- hit.cmpd.res;
  enrichPath$hits.query <- hits.all <- hit.res;
  
  hits.node.all <-hits.edge.all <- list();
  ko.map1 <- ko.edge.map;
  ko.map2 <- ko.node.map.global;
  colnames(ko.map1) <- c("queryid", "edge", "net"); rownames(ko.map1)<-NULL;
  ko.map2 <- ko.node.map.global;
  colnames(ko.map2) <- c("queryid", "edge", "net"); rownames(ko.map2)<-NULL;

  hits.edge.all <- MatchQueryOnKEGGMap(hits.all, ko.map1);
  hits.node.all <- MatchQueryOnKEGGMap(hits.all, ko.map2);
  
  enrichPath$hits.edge <- MatchQueryOnKEGGMap(enrichPath$hits.edge, ko.map1);
  enrichPath$hits.node <- MatchQueryOnKEGGMap(enrichPath$hits.node, ko.map2);
  
  enrichPath$hits.all <- hits.all;
  enrichPath$hits.edge.all <- hits.edge.all;
  enrichPath$hits.node.all <- hits.node.all;
  # correct hit num
  enrichPath$hit.num <- integPaths$hits_gene + integPaths$hits_cmpd;
  # correct hit p value
  enrichPath$fun.pval <- integPaths$P_value;
  # correct pathway name and order
  fun.ids <- as.vector(current.setids[names(hit.res)]); 
  if(length(fun.ids) ==1) {fun.ids <- matrix(fun.ids)};
  enrichPath$path.id <- fun.ids;
  
  ko <- qs::qread(paste0(rpath, "libs/ko.qs"));
  cmpd.map <- .get.my.lib("compound_db.qs");

  #conv.ko = ko[match(tolower(unlist(hits.all)), tolower(cmpd.map$kegg_id)),]
  #conv.cmp = cmpd.map[match(tolower(unlist(hits.all)),tolower(cmpd.map$kegg_id)), c("kegg_id", "name")]

  conv.ko <- ko
  conv.cmp <- cmpd.map[, c("kegg_id", "name")]
  colnames(conv.ko) <- c("id", "name");
  colnames(conv.cmp) <- c("id", "name");
  conv <- rbind(conv.ko, conv.cmp);
  conv <- conv[which(conv$id %in% unname(unlist(hits.all))),]
  enrichPath$conv <- conv

  json.mat <- rjson::toJSON(enrichPath);
  sink("network_enrichment_pathway_0.json")
  cat(json.mat);
  sink();

  return(1);
}

OrganizeTarJsonforNextwork <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  enrichPath <- RJSONIO::fromJSON("network_enrichment_pathway_0.json");

  # Prepare my.hits list
  my.hits <- list()
  hits.path <- mSetObj[["dataSet"]][["path.hits"]];
  my.hits0 <- lapply(names(hits.path), function(x){
    current.kegglib[["mset.list"]][[x]][hits.path[[x]]]
  })
  names(my.hits0) <- names(current.kegglib$path.ids);
  my.hits0 <- my.hits0[unname(which(sapply(my.hits0, length)!=0))]
  my.hits.cmpd <- lapply(my.hits0, FUN = function(x) {
    x0 <- x[grepl("cpd:",x)]
    if(length(x0) > 0){gsub("cpd:","",x0)}
  })
  my.hits$my.hits.cmpd <- my.hits.cmpd[sapply(my.hits.cmpd, function(x){length(x) != 0})]
  my.hits.genes <- lapply(my.hits0, FUN = function(x) {
    x[!grepl("cpd:",x)]
  })
  my.hits$my.hits.genes <- my.hits.genes[sapply(my.hits.genes, function(x){length(x) != 0})]
  
  # Start mapping and correction
  hit.kos.res <- lapply(seq(my.hits$my.hits.genes), function(x){
    enIDs0 <- my.hits$my.hits.genes[[x]];
    enIDs <- gsub("[^0-9.-]","" ,unlist(strsplit(enIDs0, " ")));
    hit.kos <- unique(doGene2KONameMapping(enIDs));
    if(any(is.na(hit.kos))){hit.kos <- hit.kos[!is.na(hit.kos)]};
    return(hit.kos)
  })
  names(hit.kos.res) <- names(my.hits$my.hits.genes)
  hit.cmpd.res <- my.hits$my.hits.cmpds
  
  pathNMs <- names(rownames(mSetObj[["dataSet"]][["path.mat"]]))
  
  hit.res <- lapply(pathNMs, function(x){
    c(hit.cmpd.res[[x]], hit.kos.res[[x]])    
  })
  names(hit.res) <- pathNMs;
  
  enrichPath$hits.edge <- hit.kos.res;
  enrichPath$hits.node <- hit.cmpd.res;
  enrichPath$hits.query <- hits.all <- hit.res;
  
  hits.node.all <-hits.edge.all <- list();
  ko.map1 <- ko.edge.map;
  ko.map2 <- ko.node.map.global;
  colnames(ko.map1) <- c("queryid", "edge", "net"); rownames(ko.map1)<-NULL;
  ko.map2 <- ko.node.map.global;
  colnames(ko.map2) <- c("queryid", "edge", "net"); rownames(ko.map2)<-NULL;
  
  hits.edge.all <- MatchQueryOnKEGGMap(hits.all, ko.map1);
  hits.node.all <- MatchQueryOnKEGGMap(hits.all, ko.map2);
  
  enrichPath$hits.edge <- MatchQueryOnKEGGMap(enrichPath$hits.edge, ko.map1);
  enrichPath$hits.node <- MatchQueryOnKEGGMap(enrichPath$hits.node, ko.map2);
  
  enrichPath$hits.all <- hits.all;
  enrichPath$hits.edge.all <- hits.edge.all;
  enrichPath$hits.node.all <- hits.node.all;
  # correct hit num
  enrichPath$hit.num <- unname(mSetObj$dataSet$path.mat[,3]);
  # correct hit p value
  enrichPath$fun.pval <- unname(mSetObj$dataSet$path.mat[,4]);
  # correct pathway name and order
  fun.ids <- as.vector(current.setids[names(hit.res)]); 
  if(length(fun.ids) ==1) {fun.ids <- matrix(fun.ids)};
  enrichPath$path.id <- fun.ids;
  
  json.mat <- rjson::toJSON(enrichPath);
  sink("network_enrichment_pathway_0.json")
  cat(json.mat);
  sink();
  
  return(1);
  
}

# Utility function for PerformKOEnrichAnalysis_List
# for KO01100
Save2KEGGJSON <- function(mSetObj, hits.query, res.mat, file.nm, hits.all){

  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  AddMsg("Functional enrichment analysis was completed");
  
  if(!exists("ko.edge.map")){    
    if(.on.public.web){
      ko.edge.path <- paste(rpath, "libs/network/ko_edge.csv", sep="");
      ko.edge.map <- .readDataTable(ko.edge.path);
    }else{
      ko.edge.path <- paste("https://www.metaboanalyst.ca/resources/libs/network/ko_edge.csv", sep="");
      download.file(ko.edge.path, destfile = "ko_edge.csv", method="libcurl", mode = "wb")
      ko.edge.map <- .readDataTable("ko_edge.csv"); 
    }
    ko.edge.map <- ko.edge.map[ko.edge.map$net=="ko01100",];  #only one map
    ko.edge.map <<- ko.edge.map;
  }

  hits.edge <- list();
  hits.node <- list();
  hits.edge.all <- list();
  hits.node.all <- list();
  
  if(idtype == "gene") {
    ko.map <- ko.edge.map;
    colnames(ko.map) <- c("queryid", "edge", "net")
    hits.edge <- MatchQueryOnKEGGMap(hits.query, ko.map)
    hits.inx <- unlist(lapply(hits.edge, length))>0;
    
    #find matches for all queries without applying pathway filtering
    hits.edge.all <- MatchQueryOnKEGGMap(hits.all, ko.map)
    hits.inx.all <- unlist(lapply(hits.edge.all, length))>0;

  } else if (idtype == "cmpd") {
    ko.map <- ko.node.map.global;
    colnames(ko.map) <- c("queryid", "edge", "net")
    hits.node <- MatchQueryOnKEGGMap(hits.query, ko.map)
    hits.inx <- unlist(lapply(hits.node, length))>0;

    #find matches for all queries without applying pathway filtering
    hits.node.all <- MatchQueryOnKEGGMap(hits.all, ko.map)
    hits.inx.all <- unlist(lapply(hits.node.all, length))>0;
  } else {
    # gene&cmpd
    ko.map1 <- ko.edge.map;
    colnames(ko.map1) <- c("queryid", "edge", "net"); rownames(ko.map1)<-NULL;
    hits.edge <- MatchQueryOnKEGGMap(hits.query, ko.map1)
    #find matches for all queries without applying pathway filtering
    hits.edge.all <- MatchQueryOnKEGGMap(hits.all, ko.map1)

    ko.map2 <- ko.node.map.global;
    colnames(ko.map2) <- c("queryid", "edge", "net"); rownames(ko.map2)<-NULL;
    hits.node <- MatchQueryOnKEGGMap(hits.query, ko.map2)
    #find matches for all queries without applying pathway filtering
    hits.node.all <- MatchQueryOnKEGGMap(hits.all, ko.map2)

    ko.map <- rbind(ko.map1, ko.map2)
    # TO-DO: combine results hits.edge and hits.node without applying again lapply over hits.query
    hits.both <- MatchQueryOnKEGGMap(hits.query, ko.map)
    hits.inx <- unlist(lapply(hits.both, length))>0;

    #find matches for all queries without applying pathway filtering
    hits.both <- MatchQueryOnKEGGMap(hits.all, ko.map)
    hits.inx.all <- unlist(lapply(hits.both, length))>0;
  }
  
  # only keep hits with edges in the map
  hits.query <- hits.query[hits.inx]; hits.all <- hits.all[hits.inx.all];
  resTable <- resTable[hits.inx, ];
  
  # write json
  fun.pval = resTable$Pval; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  hit.num = paste0(resTable$Hits,"/",resTable$Total); if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  fun.ids <- as.vector(current.setids[names(hits.query)]); if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  
  #clean non-metabolic pathways
  rm.ids <- which(is.na(fun.ids))
  if(length(rm.ids) != 0){
    fun.ids <- fun.ids[-rm.ids]
    fun.pval <- fun.pval[-rm.ids]
    hit.num <- hit.num[-rm.ids]
    hits.query <- hits.query[-rm.ids]
  }

  ko <- qs::qread(paste0(rpath ,"libs/ko.qs"));
  cmpd.map <- .get.my.lib("compound_db.qs");

  #conv.ko = ko[match(tolower(unlist(hits.all)), tolower(cmpd.map$kegg_id)),]
  #conv.cmp = cmpd.map[match(tolower(unlist(hits.all)),tolower(cmpd.map$kegg_id)), c("kegg_id", "name")]

  conv.ko <- ko
  conv.cmp <- cmpd.map[, c("kegg_id", "name")]
  colnames(conv.ko) <- c("id", "name");
  colnames(conv.cmp) <- c("id", "name");
  conv <- rbind(conv.ko, conv.cmp);
  conv <- conv[which(conv$id %in% unname(unlist(hits.all))),]


  expr = as.list(dataSet$data)
  names(expr) <- rownames(dataSet$data)
  json.res <- list(
    expr.mat = expr,
    hits.query = hits.query,
    hits.edge = hits.edge,
    hits.node = hits.node,
    hits.all = hits.all,
    hits.edge.all = hits.edge.all,
    hits.node.all = hits.node.all,
    path.id = fun.ids,
    fun.pval = fun.pval,
    hit.num = hit.num,
    conv = conv
  );
  json.mat <- rjson::toJSON(json.res);
  json.nm <- paste(file.nm, ".json", sep="");
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  fun.hits <<- hits.query;
  fun.pval <<- resTable[,5];
  hit.num <<- resTable$Hits;
  csv.nm <- paste(file.nm, ".csv", sep="");
  fast.write.csv(resTable, file=csv.nm, row.names=F);
  return(mSetObj);
}

# Utility function for Save2KEGGJSON
MatchQueryOnKEGGMap <- function(query, ko.map){
    hits <- lapply(query,
                        function(x) {
                          as.character(unique(ko.map$edge[ko.map$queryid%in%unlist(x)]));
                        }
    );

    return(hits)
}


# Utility function for PrepareNetworkData
doGene2KONameMapping <- function(enIDs){

  if(.on.public.web){
    ko.dic <- .readDataTable(paste0(rpath ,"libs/network/ko_dic.csv"));
  }else{
    ko.dic <- .readDataTable("https://www.metaboanalyst.ca/resources/libs/network/ko_dic.csv");
  }

  #TO-DO: map based on specific selection of a species
  ko.dic.enIDs <- as.integer(ko.dic[, "Entrez_hsa"])
  ko.dic.enIDs[is.na(ko.dic.enIDs)] <- -1
  hit.inx <- match(as.integer(enIDs), ko.dic.enIDs);
  kos <- ko.dic[hit.inx, "KO"];
  
  # if not gene symbol, use id by itself
  na.inx <- is.na(kos);
  kos[na.inx] <- NA #enIDs[na.inx];
  
  return(kos);
}

#'Utility function for PerformKOEnrichAnalysis_KO01100
#'@param category Module or pathway
LoadKEGGKO_lib<-function(category){

  if(category == "module"){
    kegg.anot <- .get.my.lib("ko_modules.qs", "network");
    current.setlink <- kegg.anot$link;
    current.mset <- kegg.anot$sets$"Pathway module";
  }else{
    kegg.anot <- .get.my.lib("ko_pathways.qs", "network");
    current.setlink <- kegg.anot$link;
    current.mset <- kegg.anot$sets$Metabolism;
  }
  # now need to update the msets to contain only those in ko01100 map
  if(!exists("ko.edge.map")){
    if(.on.public.web){
      ko.edge.path <- paste(rpath, "libs/network/ko_edge.csv", sep="");
      ko.edge.map <<- .readDataTable(ko.edge.path); 
    }else{
      ko.edge.path <- paste("https://www.metaboanalyst.ca/resources/libs/network/ko_edge.csv", sep="");
      download.file(ko.edge.path, destfile = "ko_edge.csv", method="libcurl", mode = "wb")
      ko.edge.map <<- .readDataTable("ko_edge.csv"); 
    }
  } 

  kos.01100 <- ko.edge.map$gene[ko.edge.map$net == "ko01100"];
  current.mset <- lapply(current.mset, 
                         function(x) {
                           as.character(unique(x[x %in% kos.01100]));
                         }
  );
  # remove those empty ones
  mset.ln <- lapply(current.mset, length);
  current.mset <- current.mset[mset.ln > 0];
  set.ids<- names(current.mset); 
  names(set.ids) <- names(current.mset) <- kegg.anot$term[set.ids];
  
  current.setlink <<- current.setlink;
  current.setids <<- set.ids;
  current.geneset <<- current.mset;
}

#'Utility function
#'@description Returns matched KO in the same order (NA if no match)
#'@param ko.vec Input the vector containing KOs
#'@param type Input the type 
doKOFiltering <- function(ko.vec, type){
  if(.on.public.web){
    ko.dic <- .readDataTable(paste0(rpath ,"libs/network/ko_dic.csv"));
  }else{
    ko.dic <- .readDataTable("https://www.metaboanalyst.ca/resources/libs/network/ko_dic.csv");
  }
  hit.inx <- match(ko.vec, ko.dic$KO);
  return(ko.dic$KO[hit.inx]);
}

#'Utility function for PrepareKeggQueryJson
#'@param kos Input the KOs
#'@param net Input the name of the network 
MapKO2KEGGEdges<- function(kos, net="ko01100"){
  if(!exists("ko.edge.map")){
    if(.on.public.web){
      ko.edge.path <- paste(rpath ,"libs/network/ko_edge.csv", sep="");
      ko.edge.map <<- .readDataTable(ko.edge.path);     
    }else{
      ko.edge.path <- paste("https://www.metaboanalyst.ca/resources/libs/network/ko_edge.csv", sep="");
      ko.edge.map <<- .readDataTable(ko.edge.path);     
    }
  } 
  all.hits <- ko.edge.map$gene %in% names(kos) & ko.edge.map$net == net;
  my.map <- ko.edge.map[all.hits, ];
  q.map <- data.frame(gene=names(kos), expr=as.numeric(kos));

  # first merge to get ko abundance to each edge
  dat <- merge(my.map, q.map, by="gene");
  
  # now merge duplicated edge to sum
  dup.inx <- duplicated(dat[,2]);
  dat <- dat[!dup.inx,];
  rownames(dat) <- dat[,2];
  
  return(dat[,-2]);
}

#'Utility function for PrepareKeggQueryJson
#'@param cmpds Input the compounds
#'@param net Input the network name
MapCmpd2KEGGNodes <- function(cmpds, net="ko01100"){
  
  lib <- "hsa_kegg.qs" # TO-DO: change for other species
  if(!exists("ko.node.map.global")){
    # Read original library files for a list of pathways with assigned compounds to each
    
    if(.on.public.web){
      pathway.lib <- qs::qread(paste(rpath ,"libs/mummichog/", lib, sep=""));
    }else{
      if(!file.exists(lib)){
        path.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", lib, sep="")
        download.file(path.url, destfile = lib, method="libcurl", mode = "wb")
        pathway.lib <- qs::qread(lib);
      }else{
        pathway.lib <- qs::qread(lib);
      }
    }

    pathways <- pathway.lib$pathways;
    
    # Store universe for enrichment analysis
    names(pathways$cpds) <- pathways$name
    current.cmpd.set <<- pathways$cpds;
    print("this current.cmpd.set ")
    # Read pathway names and ids in the target pathway map (e.g. ko01100)
    
    if(.on.public.web){
      ko.pathway.names <- .readDataTable(paste(rpath ,"libs/network/ko01100_compounds_ids.csv", sep=""));    
    }else{
      ko.pathway.names <- .readDataTable(paste("https://www.metaboanalyst.ca/resources/libs/network/ko01100_compounds_ids.csv", sep=""));    
    }
    
    #ko.node.map <- do.call(rbind, lapply(1:length(pathways$name), function(i) cbind(unlist(pathways$cpds[i]), pathways$name[i])));
    #ko.node.matches <- ko.pathway.names[match(ko.node.map[,2], ko.pathway.names$name),2]
    # Replace pathway names with ids
    #ko.node.map[,2] <- ko.node.matches
    # Clean missing cases
    #ko.node.map <- ko.node.map[!is.na(ko.node.matches),]
    #ko.node.map.global <<- data.frame(cmpd = ko.node.map[,1], edge = ko.node.map[,2], net = rep("ko01100", nrow(ko.node.map)))
    ko.node.map.global <<- data.frame(cmpd = ko.pathway.names[,1], edge = ko.pathway.names[,2], net = rep("ko01100", nrow(ko.pathway.names)))
  }
  
  all.hits <- ko.node.map.global$cmpd %in% names(cmpds) & ko.node.map.global$net == net;
  my.map <- ko.node.map.global[all.hits, ];
  q.map <- data.frame(cmpd=names(cmpds), expr=as.numeric(cmpds));
  
  # first merge to get cmpd abundance to each edge
  dat <- merge(my.map, q.map, by="cmpd");
  
  # now merge duplicated edge to sum
  dup.inx <- duplicated(dat[,2]);
  dat <- dat[!dup.inx,];
  rownames(dat) <- dat[,2];
  
  return(dat[,-2]);
}

#'Prepare user's query for mapping KEGG Global Metabolic Network
#'@description This function prepares the user's data for the 
#'KEGG Global Metabolic Network
#'@param mSetObj Input name of the created mSet Object
#'@author Othman Soufan, Jeff Xia \email{jeff.xia@mcgill.ca}, {othman.soufan@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PrepareKeggQueryJson <- function(mSetObj=NA){
    save.image("kegg.RData");
    mSetObj <- .get.mSet(mSetObj);
  
    # Map query matched KOs with the KO database
    kos <- mSetObj$dataSet$gene.name.map$hit.kos
    expr.mat <- mSetObj$dataSet$pathinteg.imps$kos.mat
    kos <- cbind(kos, expr.mat)
    # Retreive compounds information
    cmpds.expr <- mSetObj$dataSet$pathinteg.imps$cmpd.mat
    cmpds <- cbind(rownames(cmpds.expr), cmpds.expr)

    enrich.type <- "hyper";

    # Perform gene enrichment
    gene.mat <- list()
    if(!is.null(kos) && length(kos) > 0){
        dataSet.gene <- PerformMapping_ko01100(kos, "ko")
        if(length(dataSet.gene)==0){
            return(0);
        }

        if(enrich.type == "hyper"){
            exp.vec <- dataSet.gene$data[,1]; # drop dim for json
        }else{
            # for global test, all KO measured should be highlighted
            genemat <- as.data.frame(t(otu_table(dataSet.gene$norm.phyobj)));
            exp.vec <- rep(2, ncol(genemat));
            names(exp.vec) <- colnames(genemat);
        }
        gene.mat <- MapKO2KEGGEdges(exp.vec);
    }

    # Perform compound enrichment
    cmpd.mat <- list()
    if(length(cmpds) > 1){
        dataSet.cmpd <- PerformMapping_ko01100(cmpds, "cmpd")
        if(length(dataSet.cmpd)==0){
          return(0);
        }

        if(enrich.type == "hyper"){
            exp.vec <- dataSet.cmpd$data[,1]; # drop dim for json
        }else{
            # for global test, all KO measured should be highlighted
            genemat <- as.data.frame(t(otu_table(dataSet.cmpd$norm.phyobj)));
            exp.vec <- rep(2, ncol(genemat));
            names(exp.vec) <- colnames(genemat);
        }
        cmpd.mat <- MapCmpd2KEGGNodes(exp.vec);
    }

    # TO-DO: Refactor the following part of code for better readability
    if(length(cmpd.mat) != 0 && length(gene.mat) != 0){
      edge.mat <- as.data.frame(rbind(as.matrix(cmpd.mat), as.matrix(gene.mat)));
      dataSet <<- MergeDatasets(dataSet.cmpd, dataSet.gene);
      idtype <<- "gene&cmpd";
    } else if(length(cmpd.mat) != 0){
      edge.mat <- cmpd.mat;
      dataSet <<- dataSet.cmpd;
      idtype <<- "cmpd";
    } else{
      edge.mat <- gene.mat;
      dataSet <<- dataSet.gene;
      idtype <<- "gene";
    }
    
    row.names(edge.mat) <- eids <- rownames(edge.mat);
    query.ko <- edge.mat[,1];
    net.orig <- edge.mat[,2];
    query.res <- edge.mat[,3];# abundance
    names(query.res) <- eids; # named by edge

    json.mat <- rjson::toJSON(query.res);
    sink("network_query.json");
    cat(json.mat);
    sink();
    return(.set.mSet(mSetObj)); 
}

#'Utility function for PrepareKeggQueryJson
#'geneIDs is text one string, need to make to vector
#'@param inputIDs Input list of IDs
#'@param type Input the type of IDs
PerformMapping_ko01100 <- function(inputIDs, type){
  
  dataSet <- list();
  dataSet$orig <- inputIDs;
  
  data.mat <- as.matrix(inputIDs);
  
  if(dim(data.mat)[2] == 1){ # add 1
    data.only <- 1; # if only a list of ids are provided with abundance or confidence scores
    data.mat <- cbind(data.mat, rep(1, nrow(data.mat)));
  }else {
    data.only <- 0;
    data.mat <- data.mat[,1:2];
  }
  
  # Hanlde case when only 1 id is provided
  if(!is.matrix(data.mat)){
    data.mat <- as.matrix(t(data.mat))
  }
  
  rownames(data.mat) <- data.mat[,1];
  data.mat <- data.mat[,-1, drop=F];
  dataSet$id.orig <- data.mat;
  dataSet$data.only <- data.only;
  data.mat <- RemoveDuplicates(data.mat, "sum", quiet=F); 
  dataSet$id.uniq <- data.mat;
  
  # now get input that are in the lib
  if(type == "ko"){
    kos <-  doKOFiltering(rownames(data.mat), type);
    if(sum(!is.na(kos)) < 2){
      AddErrMsg("Less than two hits found in the database. ");
      dataSet <- list();
      return(dataSet);
    }
    rownames(data.mat) <- kos;
    gd.inx <- (!is.na(kos)) & data.mat[,1] > -Inf; # TO-DO: change -Inf to specific value based on type of uploaded scores
    data.mat <- data.mat[gd.inx, ,drop=F];
    
    AddMsg(paste("A total of unqiue", nrow(data.mat), "KO genes were mapped to KEGG network!"));
  }
  # TO-DO: check if there is a need to do compound filtering here (e.g. doKeggCmpdFiltering)
  
  dataSet$id.mapped <- dataSet$data <- data.mat;
  
  return(dataSet);
}

#'Utility function for PrepareKeggQueryJson
#'@param dataSet1 Input the first dataset
#'@param dataSet2 Input the second dataset
MergeDatasets <- function(dataSet1, dataSet2){
  
  dataSet <- list();
  dataSet$orig <- c(dataSet1$orig, dataSet2$orig);
  dataSet$id.orig <- rbind(dataSet1$id.orig, dataSet2$id.orig)
  dataSet$id.uniq <- rbind(dataSet1$id.uniq, dataSet2$id.uniq)
  dataSet$data <- rbind(dataSet1$data, dataSet2$data)
  dataSet$id.mapped <- rbind(dataSet1$id.mapped, dataSet2$id.mapped)
  
  return(dataSet);
}


#'Prepare data for network exploration
#'@description Function for the network explorer module, prepares user's data for network exploration.
#'@param mSetObj Input name of the created mSet Object
#'@export
PrepareNetworkData <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  # prepare gene list
  if(!is.null(mSetObj$dataSet$gene.mat)){
    gene.mat <- mSetObj$dataSet$gene.mat;
    enIDs <- mSetObj$dataSet$gene.name.map$hit.values;
    kos <- mSetObj$dataSet$gene.name.map$hit.kos;
    rownames(gene.mat) <- enIDs;
    
    na.inx <- is.na(kos);
    gene.mat.clean <- gene.mat[!na.inx, ,drop=F];
    kos.clean <- kos[!na.inx]
    
    gene.names <- rownames(gene.mat.clean)
    gene.mat.clean <- RemoveDuplicates(gene.mat.clean);
    if(nrow(gene.mat.clean) < length(kos.clean)){
      mSetObj$dataSet$gene.name.map$hit.kos <- kos.clean[!duplicated(gene.names)]
    } else{
      mSetObj$dataSet$gene.name.map$hit.kos <- kos.clean
    }
    AddMsg(paste("A total of ", nrow(gene.mat.clean), "unique genes were uploaded."));
    
    if(!exists("pathinteg.imps", where = mSetObj$dataSet)){
      mSetObj$dataSet$pathinteg.imps <- list();
    }
    mSetObj$dataSet$pathinteg.imps$gene.mat <- gene.mat.clean;
    done <- 1;
  }
  
  # prepare kos list
  if(!is.null(mSetObj$dataSet$gene.mat)){
    # Handle case when upload type is KOs
    if(mSetObj$dataSet$q.type.gene == "kos"){
      rownames(gene.mat) <- kos
      gene.mat <- RemoveDuplicates(gene.mat);
      mSetObj$dataSet$gene.name.map$hit.kos <- rownames(gene.mat)
      AddMsg(paste("A total of ", nrow(gene.mat), "unique KOs were uploaded."));
      
      if(!exists("pathinteg.imps", where = mSetObj$dataSet)){
        mSetObj$dataSet$pathinteg.imps <- list();
      }
      mSetObj$dataSet$pathinteg.imps$kos.mat <- gene.mat;
      done <- 1;
    } else{
      mSetObj$dataSet$pathinteg.imps$kos.mat <- mSetObj$dataSet$pathinteg.imps$gene.mat;
    }
  }
  
  # prepare compound list
  if((!is.null(mSetObj$dataSet$cmpd.mat) || (!is.null(mSetObj$dataSet$cmpd)))){
    nm.map <- GetFinalNameMap(mSetObj);

    # inject code here for DSPC (JX)
    my.ids <- ifelse(is.na(nm.map$kegg), paste("unmapped", rownames(nm.map), sep = "_"), nm.map$kegg); #also accept unmapped cmpd
    mSetObj$dataSet$orig.var.ids <- my.ids;

    valid.inx <- !(is.na(nm.map$kegg)| duplicated(nm.map$kegg));
    cmpd.vec <- nm.map$query[valid.inx];
    kegg.id <- nm.map$kegg[valid.inx];
    
    cmpd.mat <- mSetObj$dataSet$cmpd.mat;
    if(is.null(mSetObj$dataSet$cmpd.mat)){
      cmpd <- as.matrix(mSetObj$dataSet$cmpd); # when the input is concentration table
      cmpd.mat <- cbind(cmpd, rep("0", nrow(cmpd)))
    }
    hit.inx <- match(cmpd.vec, rownames(cmpd.mat));
    
    cmpd.mat <- cmpd.mat[hit.inx, ,drop=F];
    rownames(cmpd.mat) <- kegg.id;
    cmpd.mat <- RemoveDuplicates(cmpd.mat);
    AddMsg(paste("A total of ", nrow(cmpd.mat), "unique compounds were found."));
    mSetObj$dataSet$pathinteg.imps$cmpd.mat <- cmpd.mat;
    done <- 1;
  }
  
  return(.set.mSet(mSetObj));
}
