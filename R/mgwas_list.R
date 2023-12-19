##################################################
## R script for mGWAS
## Description: List data I/O and processing
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

SetVepOpt <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$vep.opt <- opt;
  .set.mSet(mSetObj);
}

SetVepDis <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$vep.dis <- opt;
  .set.mSet(mSetObj);
}

SetVepNum <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$vep.num <- as.numeric(opt);
  .set.mSet(mSetObj);
}

SetLDProxy <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ld.proxy <- opt;
  .set.mSet(mSetObj);
}

SetLDR2 <- function(opt){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ld.r2 <- as.numeric(opt);
  .set.mSet(mSetObj);
}

.init.multilist <- function(){
  mSetObj <- .get.mSet(mSetObj);
  anal.type <<- "multilist"
  net.info <<- list();
  mir.mappedu <<- matrix();
  mir.resu <<- data.frame();
  m2g.resu <<- data.frame();
  snp2met <<- data.frame();
  snp2gene <<- data.frame();
  gene2snp <<- data.frame();
  dis2snp <<- data.frame();
  snp2dis <<- data.frame();
  snp2prot <<- data.frame();
  gene2dis <<- data.frame();
  met2snp <<- data.frame();
  met2gene <<- data.frame();
  gene2met <<- data.frame();
  met2dis <<- data.frame();
  gene2dis <<- data.frame();
  drug2met<<-data.frame();
  snp2egene <<- data.frame();
  mr_results <<- data.frame();
  mr_sensitivity <<- data.frame();
  mr_single <<- data.frame();
  protein2protein <<- data.frame();
  mirtargetu <<- vector();
  mirtableu <<- vector();
  seedsu <<- vector();
  mSetObj$dataSet$directionInx <-vector()
  mSetObj$dataSet$regDirection <-vector()
  mSetObj$dataSet$tfTargetType <-vector()
  edgeNumU <<- vector();
  edgeu.res <<- data.frame();
  nodeu.ids <<- vector();
  nodeu.nms <<- vector();
  mir.nmsu <<- vector();
  snp.nmsu <<- vector();
  tf.nms <<- vector();
  gene.nms <<- vector();
  met.nms<<-vector();
  drug.nms<<-vector();
  dis.nms<<-vector();
  prot.nms<<-vector();
  snp.nms<<-vector();
  egene.nms<<-vector();
  .set.mSet(mSetObj);

}

.set.net.names <- function(input.type){

  if (grepl("eqtl", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$egene.nms = egene.nms
  }
  
  if (grepl("met2dis", input.type)) {
    net.info$dis.nms = dis.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("gene2dis", input.type)) {
    net.info$dis.nms = dis.nms;
    net.info$gene.nms = gene.nms;
  }
  if (grepl("gene2met", input.type)) {
    net.info$gene.nms = gene.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("met2gene", input.type)) {
    net.info$gene.nms = gene.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("snp2ld2dis", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$gene.nms = gene.nms;
    net.info$dis.nms = dis.nms;
  }
  if (grepl("snp2prot", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$prot.nms = prot.nms;
  }
  if (grepl("snp2ld2prot", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$gene.nms = gene.nms;
    net.info$prot.nms = prot.nms;
  }
  if (grepl("snp2ld2egene", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$egene.nms = egene.nms
  }
  if (grepl("snp2met", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("snp2ld2met", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$gene.nms = gene.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("drug", input.type)) {
    net.info$drug.nms = drug.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("dis", input.type)) {
    net.info$dis.nms = dis.nms;
    net.info$snp.nms = snp.nms;
  }
  if (grepl("dis2snp2met", input.type)) {
    net.info$dis.nms = dis.nms;
    net.info$snp.nms = snp.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("met2snp", input.type)) {
    net.info$met.nms = met.nms;
    net.info$snp.nms = snp.nms;
    net.info$gene.nms = gene.nms;
  }
  if (grepl("gene2snp", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$gene.nms = gene.nms;
  }
  if (grepl("snp_pos", input.type)) {
    net.info$gene.nms = gene.nms;
    net.info$snp.nms = snp.nms;
  }
  if (grepl("snp2met", input.type)) {
    net.info$snp.nms = snp.nms
    net.info$met.nms = met.nms
    net.info$gene.nms = gene.nms
    net.info$dis.nms = dis.nms
  }
  if (grepl("dis", input.type)) {
    net.info$dis.nms = dis.nms
  }
  if (grepl("snp", input.type)) {
    net.info$snp.nms = snp.nms
  }
  if (grepl("study", input.type)) {
    net.info$snp.nms = snp.nms;
    net.info$met.nms = met.nms;
  }
  if (grepl("mr2lit", input.type)) {
    net.info$exp.ids = exp.ids;
    net.info$out.ids = out.ids;
    #net.info$lit.ids = c(overlap.ids, expsbj.ids, outobj.ids)
    net.info$overlap.ids = overlap.ids;
    net.info$expsbj.ids = expsbj.ids;
    net.info$outobj.ids = outobj.ids;

  }
  if (grepl("protein", input.type)) {
    net.info$gene.nms = gene.nms;
  }
  return(net.info)
}

.searchMultiNet_gene2snp2met<-function(input.type){
  # input.type<<-input.type;
  # save.image("searchMultiNet_gene2snp2met.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  idVec <- rownames(mir.mat);
  idType <- mSetObj$dataSet$idType;
  tissue <- mSetObj$dataSet$tissue;
  population <- mSetObj$dataSet$population;
  #save.image("searchMultiNet_gene2snp2met.RData")
  tableName <- "gene2snp";
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), idVec, tableName, idType, tissue, population);

    hit.num <- nrow(mir.dic)
    if (hit.num == 0) {
      current.msg <<- "No hits found in the database. Please check your input.";
      print(current.msg);
      return(0);
    } else {
      fast.write.csv(mir.dic, file="mgwas_gene2snp.csv", row.names=FALSE); 
      res <- mir.dic[ , c("rsid", "chr", "pos_hg19","pos_hg38", "most_severe_consequence","symbol","entrez","nearest_gene_50kb","name")];
      
      # record the mapped queries and change to same IDs used in network
      uniq.mat <- unique(mir.dic[, c("symbol","rsid", idType)]);
      hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
      rownames(mir.mat) <- uniq.mat[hit.inx,"symbol"];

      mir.mappedu <<- rbind(mir.mappedu, mir.mat);

      # update col names
      colnames(res) <- c("rsID", "Chr", "POS_hg19","POS_hg38", "Consequence", "Symbol","Entrez", "Ensembl", "Name");
      res$Mapping <- rep("VEP nearest gene mapping", nrow(res));
      display.res <- res;
      edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Symbol"],stringsAsFactors = FALSE);    # IDs
      if(nrow(res)!=0){
        row.names(edge.res) <- 1:nrow(res);
      }

      node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Entrez"]);
      node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Symbol"]);

      edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
      edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
      nodeu.ids <<- c(nodeu.ids, node.ids);
      edgeNumU <<- c(edgeNumU, nrow(edge.res))
      nodeu.nms <<- c(nodeu.nms, node.nms);

      snp.nms <<- res[, "rsID"];
      gene.nms <<- res[, "Symbol"];
      res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Symbol"], ID2=res[,"Entrez"],Reference=res[,"Mapping"], EdgeAttr1=rep("", nrow(res)),
                        EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2gene", nrow(res)),stringsAsFactors = FALSE);
      
      mir.resu <<- rbind(mir.resu, res);

      gene2snp <<- display.res;         # save this for network builder and table view
        mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(gene.nms)),Mapped=length(unique(snp.nms)),stringsAsFactors = FALSE);
        seedsu <<- c(seedsu, gene.nms);
        mirtableu <<- c(mirtableu, "gene2snp");
      res <- .searchMultiNet_snp2met_all("gene2snp2met",unique(snp.nms));
      if(length(res)==0){
        current.msg <<- "No hits found in the gene2snp2met database. Please check your input.";
        return(0);
      }
    }
    return(1);
}

.searchMultiNet_snp2gene<-function(input.type){
  #input.type<<-input.type;
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  #save.image("searchMultiNet_snp2gene.RData")
  if(input.type=="met2snp2gene"){
    idVec <-  unique(unname(nodeu.ids)); 
  }else{
    idVec <- rownames(mir.mat);  
  }
  idType <- mSetObj$dataSet$idType;
  if(length(mSetObj$dataSet$data[["snp"]])!= 0){
    # this is in multisearch module
    idType <- mSetObj$dataSet$id.types[["snp"]];
    mir.mat <- mSetObj$dataSet$data[["snp"]];
    idVec <- rownames(mir.mat);
  }
  vep.opt <- mSetObj$dataSet$vep.opt;
  #save.image("searchMultiNet_snp2gene.RData")
  if(vep.opt=="haploreg"){
    .snp2gene_haploreg(input.type, idVec);
  }else if(vep.opt == "myvariant"){
    .snp2gene_myvariant(input.type, idVec);
    
  }else if(vep.opt=="phenoscanner"){
    ##### issue is phenoscanner API is not stable #####
    .snp2gene_phenoscanner(input.type);
  }else{
    .snp2gene_vep(input.type);
  }
}

.snp2gene_phenoscanner<-function(input.type){
  
  mSetObj <- .get.mSet(mSetObj);
  if(!is.null(mSetObj$dataSet$data$snp)){
    # this is for snp metabolite joint search
    mir.mat <- mSetObj$dataSet$data$snp;
    idType <- mSetObj$dataSet$id.types$snp;
  }else{
    mir.mat <- mSetObj$dataSet$mir.orig;
    idType <- mSetObj$dataSet$idType;
  }
  idVec <- rownames(mir.mat);
  
  ld.proxy <- mSetObj$dataSet$ld.proxy;
  ld.r2 <- mSetObj$dataSet$ld.r2;
  #input.type<<-input.type;
  #save.image("snp2gene_phenoscanner.RData");
  res <- phenoscanner(snpquery= idVec, proxies=ld.proxy, r2=ld.r2);
  res <- res$snps;
  colnames(res)[which(colnames(res)=="hgnc")] = "symbol";
  res$entrez = doGeneIDMapping(res$symbol,"hsa",type="symbol");
  res$entrez[is.na(res$entrez)] = res$symbol[is.na(res$entrez)];

  mir.dic <- res;
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the SNP positional mapping based on PhenoScanner. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    fast.write.csv(mir.dic, file="mgwas_snp2gene.csv", row.names=FALSE); 
    if(mSetObj$dataSet$ld.proxy != "None"){
      snp.vec <- c(unique(mir.dic$rsid), unique(mir.dic$ref_rsid));
      #### search snp2met for ld proxies ####
      if(input.type=="snp2ld2met"){
        snp.proxy.vec4met <- .searchMultiNet_snp2met_all("snp2ld2met", snp.vec);
      }else{
        snp.proxy.vec4met <- ""
      }
      #### search snp2dis for ld proxies ####
      if(input.type=="snp2ld2dis"){
        snp.proxy.vec4dis <- .searchMultiNet_snp2dis("snp2ld2dis", snp.vec);
      }else{
        snp.proxy.vec4dis <- ""
      }
      #### search snp2prot for ld proxies ####
      if(input.type=="snp2ld2prot"){
        snp.proxy.vec4prot <- .searchMultiNet_snp2prot("snp2ld2prot", snp.vec);
      }else{
        snp.proxy.vec4prot <- ""
      }
      #### search snp2egene for ld proxies ####
      if(input.type=="snp2ld2egene"){
        snp.proxy.vec4egene <- .searchMultiNet_snp2egene("snp2ld2egene", snp.vec);
      }else{
        snp.proxy.vec4egene <- ""
      }
      snp.proxy.vec <- c(snp.proxy.vec4met, snp.proxy.vec4dis, snp.proxy.vec4prot, snp.proxy.vec4egene);
      mir.dic <- mir.dic[mir.dic$rsid %in% snp.proxy.vec,];
      
    }
    res <- mir.dic;
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("rsid", "symbol", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    if(idType %in% c("rsid", "hg19_coordinates")){
      rownames(mir.mat) <- uniq.mat[hit.inx,"rsid"];
    }else{
      # b/c not all metabolites have hmdb id
      rownames(mir.mat) <- uniq.mat[hit.inx,"name"];
    }
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    if(ld.proxy=="None"){
      res$ref_rsid <- res$rsid; 
      res$r2 <- rep(1, nrow(res));
      res <- res[,c("ref_rsid","rsid", "r2","chr", "pos_hg38","a1","a2","consequence","symbol","afr","amr","eas","eur","sas", "entrez","ensembl")];
      # update col names
      colnames(res) <- c("Query_SNP","rsID","r2","Chr","Pos_hg38","A1","A2","Consequence","Symbol","AFR","AMR","EAS","EUR","SAS", "Entrez", "Ensembl");
    }else{
      res <- res[,c("ref_rsid","rsid", "r2","chr", "pos_hg38","a1","a2","consequence", "symbol","entrez","ensembl","r2",tolower(ld.proxy))];
      # update col names
      colnames(res) <- c("Query_SNP","rsID","r2","Chr","Pos_hg38","A1","A2","Consequence","Symbol", "Entrez", "Ensembl","r2", ld.proxy);
      
    }
    res$Mapping <- rep("PhenoScanner", nrow(res));
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Symbol"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Symbol"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Symbol"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[, "rsID"]));
    gene.nms <<- c(gene.nms, as.vector(res[, "Symbol"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Symbol"], ID2=res[,"Entrez"],Reference=res[,"Mapping"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2gene", nrow(res)),stringsAsFactors = FALSE);
    
      mir.resu <<- rbind(mir.resu, res);
    
    mirtargetu <<- c(mirtargetu, "snp2gene");
    
    if(input.type %in% c("snp_pos", "snp2ld2dis","snp2ld2prot", "snp2ld2egene", "snp2ld2met")){
      snp2gene <<- display.res; # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(gene.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, snp.nms);
      mirtableu <<- c(mirtableu, "snp2gene");
    } else{
      # TO-DO: this is for met2snp
      # dataSet$gene2mir <<- res;
      # dataSet$tableStats <<- data.frame(Query=length(unique(gene.nms)),Mapped=length(unique(mir.nms)),stringsAsFactors = FALSE);
      # seedsu <<- c(seedsu, gene.nms);
      # mirtableu <<- c(mirtableu, "gene2mir");
    }
  }
  return(1);
}

.snp2gene_haploreg<-function(input.type, idVec){
  #idVec<<-idVec;
  #input.type<<-input.type;
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  idType <- mSetObj$dataSet$idType;
  ld.proxy <- mSetObj$dataSet$ld.proxy;
  ld.r2 <- mSetObj$dataSet$ld.r2;
  #save.image("snp2gene_haploreg.RData");
  idVec <- idVec[grep("rs", idVec)]; # only use SNPs for query
  res <- QueryHaploreg(query = idVec, file = NULL, study = NULL, ldThresh = ld.r2,
                ldPop = ld.proxy, epi = "vanilla", cons = "siphy", genetypes = "gencode",
                url = "https://pubs.broadinstitute.org/mammals/haploreg/haploreg.php",
                timeout = 10, encoding = "UTF-8", verbose = FALSE);

  if(is.null(res)){
    return(0);
  }

  colnames(res)[which(colnames(res)=="GENCODE_name")] = "symbol";
  res$entrez = doGeneIDMapping(res$symbol,"hsa",type="symbol");
  res$entrez[is.na(res$entrez)] = res$symbol[is.na(res$entrez)];
  
  mir.dic <- res;
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the SNP positional mapping based on HaploReg. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    fast.write.csv(mir.dic, file="mgwas_snp2gene.csv", row.names=FALSE); 
    #### search snp2met for ld proxies ####
    if(mSetObj$dataSet$ld.proxy != "None"){
      if(input.type=="snp_pos"){
        res <- mir.dic;
      }else{
        #### search snp2met for ld proxies ####
        if(input.type=="snp2ld2met"){
          snp.proxy.vec4met <- .searchMultiNet_snp2met_all("snp2ld2met", unique(mir.dic$rsID));
        }else{
          snp.proxy.vec4met <- ""
        }
        #### search snp2dis for ld proxies ####
        if(input.type=="snp2ld2dis"){
          snp.proxy.vec4dis <- .searchMultiNet_snp2dis("snp2ld2dis", unique(mir.dic$rsID));
        }else{
          snp.proxy.vec4dis <- ""
        }
        #### search snp2prot for ld proxies ####
        if(input.type=="snp2ld2prot"){
          snp.proxy.vec4prot <- .searchMultiNet_snp2prot("snp2ld2prot", unique(mir.dic$rsID));
        }else{
          snp.proxy.vec4prot <- ""
        }
        #### search snp2egene for ld proxies ####
        if(input.type=="snp2ld2egene"){
          for (j in 1:length(mSetObj$dataSet$eqtlTissue)){
            tissue.type=mSetObj$dataSet$eqtlTissue[j];  
            print(tissue.type)
            snp.proxy.vec4egene <- .searchMultiNet_snp2egene(substring(tissue.type, 6), unique(mir.dic$rsID));
          }
        }else{
          snp.proxy.vec4egene <- ""
        }
        snp.proxy.vec <- c(snp.proxy.vec4met, snp.proxy.vec4dis, snp.proxy.vec4prot, snp.proxy.vec4egene);
        mir.dic <- mir.dic[mir.dic$rsID %in% snp.proxy.vec,];
      }
      res <- mir.dic;
    }
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("rsID", "symbol")]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, "rsID"]);
    rownames(mir.mat) <- uniq.mat[hit.inx,"rsID"];

    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    res <- res[,c("query_snp_rsid","rsID","r2", "chr", "pos_hg38","ref","alt","dbSNP_functional_annotation", "symbol","entrez","AFR","AMR","ASN","EUR" )];
    # update col names
    colnames(res) <- c("Query_SNP","rsID","r2","Chr","Pos_hg38","Ref","Alt","Consequence","Symbol", "Entrez", "AFR","AMR","ASN","EUR");
    res$Mapping <- rep("HaploReg_positional_mapping", nrow(res));
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Symbol"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Symbol"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Symbol"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[, "rsID"]));
    gene.nms <<- c(gene.nms, as.vector(res[, "Symbol"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Symbol"], ID2=res[,"Entrez"],Reference=res[,"Mapping"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2gene", nrow(res)),stringsAsFactors = FALSE);
    
      mir.resu <<- rbind(mir.resu, res);
    
    # mir.resu is just for eQTL mapping when query multiple tissues for now
    mirtargetu <<- c(mirtargetu, "snp2gene");
    
      snp2gene <<- display.res; # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(gene.nms)),stringsAsFactors = FALSE);
      snp.seeds <- rownames(mir.mat)[rownames(mir.mat) %notin% NA]; # this is the input snp, does not have results after ld search
      seedsu <<- c(seedsu, snp.seeds);
      mirtableu <<- c(mirtableu, "snp2gene");
  }
  return(1);
}

.snp2gene_myvariant<-function(input.type, idVec){
  mSetObj <- .get.mSet(mSetObj);
  if(!is.null(mSetObj$dataSet$data$snp)){
    # this is for snp metabolite joint search
    mir.mat <- mSetObj$dataSet$data$snp;
    idType <- mSetObj$dataSet$id.types$snp;
  }else{
    mir.mat <- mSetObj$dataSet$mir.orig;
    idType <- mSetObj$dataSet$idType;
  }
  ld.proxy <- mSetObj$dataSet$ld.proxy;
  ld.r2 <- mSetObj$dataSet$ld.r2;
  # idVec<<-idVec;
  # input.type<<-input.type;
  # save.image("snp2gene_myvariant.RData");
  idVec <- idVec[grep("rs", idVec)]; # only use SNPs for query;

  if(ld.proxy != "None"){
    # LD proxy search
    gwasvcf::set_plink();
    ldref <- paste0(plink.path, ld.proxy);

    o <- get_ld_proxies(rsid = idVec, bfile =  ldref, tag_r2 = ld.r2, out=paste0(getwd(),"/"));
    query.snp <- unique(c(idVec, o$SNP_B));
  }else{
    query.snp <- idVec;
  }
  res <- QueryMyVariant(query = query.snp, scope = "dbsnp.rsid");
  mir.dic <- res;
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the SNP positional mapping based on MyVariant. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    fast.write.csv(mir.dic, file="mgwas_snp2gene.csv", row.names=FALSE); 
    if(mSetObj$dataSet$ld.proxy != "None"){
      snp.vec <-unique(mir.dic$query_snp_rsid);
      #### search snp2met for ld proxies ####
      if(input.type=="snp2ld2met"){
        snp.proxy.vec4met <- .searchMultiNet_snp2met_all("snp2ld2met", snp.vec);
      }else{
        snp.proxy.vec4met <- ""
      }
      #### search snp2dis for ld proxies ####
      if(input.type=="snp2ld2dis"){
        snp.proxy.vec4dis <- .searchMultiNet_snp2dis("snp2ld2dis", snp.vec);
      }else{
        snp.proxy.vec4dis <- ""
     }
      #### search snp2prot for ld proxies ####
      if(input.type=="snp2ld2prot"){
        snp.proxy.vec4prot <- .searchMultiNet_snp2prot("snp2ld2prot", snp.vec);
      }else{
        snp.proxy.vec4prot <- ""
      }
      #### search snp2egene for ld proxies ####
      if(input.type=="snp2ld2egene"){
        snp.proxy.vec4egene <- .searchMultiNet_snp2egene("snp2ld2egene", snp.vec);
      }else{
        snp.proxy.vec4egene <- ""
      }
      snp.proxy.vec <- c(snp.proxy.vec4met, snp.proxy.vec4dis, snp.proxy.vec4prot, snp.proxy.vec4egene);
      mir.dic <- mir.dic[mir.dic$query_snp_rsid %in% snp.proxy.vec,];
      
    }
    

    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("query_snp_rsid", "symbol")]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, "query_snp_rsid"]);
    rownames(mir.mat) <- uniq.mat[hit.inx,"query_snp_rsid"];
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    # update col names
    colnames(res) <- c("rsID","Chr","Pos_hg19","Ref","Alt","Consequence","Symbol", "Entrez");
    res$Mapping <- rep("MyVariant_positional_mapping", nrow(res));
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Symbol"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Symbol"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Symbol"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[, "rsID"]));
    gene.nms <<- c(gene.nms, as.vector(res[, "Symbol"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Symbol"], ID2=res[,"Entrez"],Reference=res[,"Mapping"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2gene", nrow(res)),stringsAsFactors = FALSE);
    
    mir.resu <<- rbind(mir.resu, res);
    
    # mir.resu is just for eQTL mapping when query multiple tissues for now
    mirtargetu <<- c(mirtargetu, "snp2gene");
    
    snp2gene <<- display.res; # save this for network builder and table view
    mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(gene.nms)),stringsAsFactors = FALSE);
    snp.seeds <- rownames(mir.mat)[rownames(mir.mat) %notin% NA]; # this is the input snp, does not have results after ld search
    seedsu <<- c(seedsu, snp.seeds);
    mirtableu <<- c(mirtableu, "snp2gene");
  }
  return(1);
}

.snp2gene_vep<-function(input.type){
  #input.type<<-input.type;
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  idVec <- rownames(mir.mat);
  idType <- mSetObj$dataSet$idType;
  if(length(mSetObj$dataSet$data[["snp"]])!= 0){
    # this is in multisearch module
    idType <- mSetObj$dataSet$id.types[["snp"]];
    mir.mat <- mSetObj$dataSet$data[["snp"]];
    idVec <- rownames(mir.mat);
  }
  vep.opt <- mSetObj$dataSet$vep.opt;
  vep.dis <- mSetObj$dataSet$vep.dis;
  vep.num <- mSetObj$dataSet$vep.num;
  #save.image("searchMultiNet_snp2gene.RData")
  if(vep.opt=="dis"){
    res= QueryVEP(idVec, vepDis=vep.dis);
    res =  unique(res[which(res$gene_symbol !="NA"),c("rsid","gene_symbol","gene_id","hgnc_id","distance")]);
    res$distance[which(res$distance=="NA")] = 0;
    res = res[!duplicated(res[,c('rsid','gene_symbol')]),];
  }else{
    require(dplyr)
    res= QueryVEP(idVec, vepDis=50);
    res$distance[which(res$distance=="NA")] = 0; 
    res =  unique(res[which(res$gene_symbol !="NA"),c("rsid","gene_symbol","gene_id","hgnc_id","distance")]);
    # remove duplicated genes and rank by distance
    # unique on a dataframe with only selected columns
    res = res[!duplicated(res[,c('rsid','gene_symbol')]),];
    res = data.frame(res %>% arrange(distance) %>%
                       group_by(rsid) %>% 
                       mutate(rank = rank(distance)),stringsAsFactors=F);
    res = res[which(res$rank<(vep.num + 1)),];
  }
  res$entrez = doGeneIDMapping(res$gene_symbol,org="hsa",type="symbol");
  res$entrez[is.na(res$entrez)] = res$gene_symbol[is.na(res$entrez)];
  res$distance <- round(as.numeric(res$distance)/1000, digits = 1);

  mir.dic <- res;
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the SNP positional mapping based on VEP. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    fast.write.csv(mir.dic, file="mgwas_snp2gene.csv", row.names=FALSE); 
    #res <- mir.dic[ , c("rsid", "chr", "pos_hg19","pos_hg38", "consequence","embl","symbol","gene_id","name")];
    res <- mir.dic;
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("rsid", "gene_symbol", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    if(idType %in% c("rsid", "hg19_coordinates")){
      rownames(mir.mat) <- uniq.mat[hit.inx,"rsid"];
    }else{
      # b/c not all metabolites have hmdb id
      rownames(mir.mat) <- uniq.mat[hit.inx,"name"];
    }
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    # update col names
    res$ref_rsid <- res$rsid; 
    res$r2 <- rep(1, nrow(res));
    res$chr <- rep("NA", nrow(res));
    res$pos <- rep("NA", nrow(res));
    res$consequence <- rep("NA", nrow(res));
    res <- res[,c("ref_rsid","rsid","r2","chr","pos","gene_id","hgnc_id", "consequence","gene_symbol",          "distance", "entrez")];
    colnames(res) <- c("Query_SNP","rsID","r2","Chr","Pos_hg38", "Ensembl", "HGNC","Consequence","Symbol", "Distance", "Entrez");
    
    res$Mapping <- paste0("VEP_", res$Distance,"kb");
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Symbol"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Symbol"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Symbol"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[, "rsID"]));
    gene.nms <<- c(gene.nms, as.vector(res[, "Symbol"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Symbol"], ID2=res[,"Entrez"],Reference=res[,"Mapping"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2gene", nrow(res)),stringsAsFactors = FALSE);
    
      mir.resu <<- rbind(mir.resu, res);

    # mir.resu is just for eQTL mapping when query multiple tissues for now
    mirtargetu <<- c(mirtargetu, "snp2gene");
    
    if(input.type %in% c("snp_pos")){
      snp2gene <<- display.res; # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(gene.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, snp.nms);
      mirtableu <<- c(mirtableu, "snp2gene");
    } else{
      # TO-DO: this is for met2snp
      # dataSet$gene2mir <<- res;
      # dataSet$tableStats <<- data.frame(Query=length(unique(gene.nms)),Mapped=length(unique(mir.nms)),stringsAsFactors = FALSE);
      # seedsu <<- c(seedsu, gene.nms);
      # mirtableu <<- c(mirtableu, "gene2mir");
    }
  }
  return(1);
}

.searchMultiNet_snp2met_all<-function(input.type, snp.vec=NULL){
  # input.type<<-input.type;
  # snp.vec<<-snp.vec;
  # print(input.type)
  # save.image("searchMultiNet_snp2met_all.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  if(is.null(snp.vec)){
    idVec <- rownames(mir.mat);  
  }else{
    idVec <- snp.vec;
  }
  idType <- mSetObj$dataSet$idType;
  tableName <- input.type;
  if(input.type=="met2snp"){
    # this is the cmpd name
    met.vec <- mSetObj$dataSet$map.table[,2]
    idVec <- c(idVec, met.vec)
    idType <- "name";
    tableName <- "met2snp";
  }
    if(input.type=="dis2snp2met"){
      tableName <- "snp2met";
      idType <- "rsid";
      tissue <- "all";
      population <- "all"
    }
  if(input.type=="gene2snp2met"){
    tableName <- "snp2met";
    idType <- "rsid";
  }
  if(input.type=="snp2ld2met"){
    tableName <- "snp2met";
  }
  if((input.type=="snp2met" && length(mSetObj$dataSet$data[["snp"]])!= 0) ||(input.type=="snp2ld2met" && length(mSetObj$dataSet$data[["snp"]])!= 0) ){
    # this is in multisearch module
    idType <- mSetObj$dataSet$id.types[["snp"]];
    mir.mat <- mSetObj$dataSet$data[["snp"]];
    idVec <- rownames(mir.mat);
    tableName <- "snp2met";
  }

  tissue <- mSetObj$dataSet$tissue;
  population <- mSetObj$dataSet$population;
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), idVec, tableName, idType, tissue, population);

  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    current.msg <<- paste("A total of ", hit.num, "pairs of SNP-metabolite associations were identified!");
    fast.write.csv(mir.dic, file="mgwas_snp_met.csv", row.names=FALSE); 
    res <- mir.dic[ , c("name","hmdb","kegg","rsid","ea", "chr", "pos_hg19", "p_value","beta","most_severe_consequence","symbol","entrez","embl","pmid","population","biofluid","link")];
    rownames(res) <- mir.dic$snp2met_id;
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("rsid", "name", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    if(idType %in% c("rsid", "hg19_coordinates")){
      rownames(mir.mat) <- uniq.mat[hit.inx,"rsid"];
    }else{
      # b/c not all metabolites have hmdb id
      rownames(mir.mat) <- uniq.mat[hit.inx,"name"];
    }
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    # temporary for now, to remove NA and 0 values (need to clean the sqlite)
    res <- .parse_snp2met(res);
    # aggregated results
    res <- .get_snp2met_aggregate(res);
    # update col names
    res <- res[,c("name", "hmdb","rsid","ea", "chr", "pos_hg19","p_value.x","beta","pmid.x","population","biofluid","link","kegg", "most_severe_consequence", "symbol","entrez","embl","pmid.y","p_value.y","pmid")];
    colnames(res) <- c("Metabolite","HMDB","rsID","Effect Allele", "Chr", "BP", "P-value","Beta","PMID", "Population","Biofluid","URL","KEGG", "Most Severe Consequence", "Gene","Entrez","Ensembl", "PMIDs","P-values", "N_studies");
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Metabolite"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"HMDB"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Metabolite"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    met.nms <<- c(met.nms, as.vector(res[,"Metabolite"]));
    snp.nms <<- c(snp.nms, as.vector(res[, "rsID"]));
    
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Metabolite"], ID2=res[,"KEGG"], Reference=res[,"PMIDs"], EdgeAttr1=res[,"P-values"],
                      EdgeAttr2=res[,"N_studies"], EdgeType=rep("snp2met", nrow(res)), stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
    
    if(input.type %in% c("snp2met_all","snp2met_blood", "snp2met_urine", "snp2met_saliva", "snp2met_csf", "dis2snp2met", "gene2snp2met","snp2met", "snp2ld2met")){
      snp2met <<- display.res;         # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(met.nms)),stringsAsFactors = FALSE);
      if(input.type %in% c("snp2ld2met", "gene2snp2met", "dis2snp2met")){# these snp includes input snp and snp after ld, therefore, cannot use as seeds
        seedsu <<- seedsu;
      }else{
        seedsu <<- c(seedsu, snp.nms);  
      }
      mirtableu <<- c(mirtableu, "snp2met");
    } else{
      met2snp <<- display.res;
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(met.nms)),Mapped=length(unique(snp.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, met.nms);
      mirtableu <<- c(mirtableu, "met2snp");
    }
  }
    if(is.null(snp.vec)){
      return(1);  
    }else{
      return(unique(snp.nms));
    }
    
}

.searchMultiNet_snp2dis<-function(input.type, snp.vec=NULL){
  #input.type<<-input.type;
  #snp.vec<<-snp.vec;
  #save.image("searchMultiNet_snp2dis.RData")
  mSetObj <- .get.mSet(mSetObj);
  idType <- mSetObj$dataSet$idType;
  mir.mat <- mSetObj$dataSet$mir.orig;
  if(length(mSetObj$dataSet$data[["snp"]])!= 0){
    # this is in multisearch module
    idType <- mSetObj$dataSet$id.types[["snp"]];
    mir.mat <- mSetObj$dataSet$data[["snp"]];
  }
  if(is.null(snp.vec)){
    idVec <- rownames(mir.mat);  
  }else{
    idVec <- snp.vec;
  }
  if (idType=="rsid"){
    idType <- "variantId";
  } 
  tableName <- paste0("variantDiseaseNetwork");
  #save.image("searchMultiNet_snp2dis.RData")
  mir.dic <- Query.DisGeNETDB(paste(url.pre, "disgenet_2020", sep=""), idVec, tableName, idType);
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    current.msg <<- paste("A total of ", hit.num, "pairs of SNP-disease associations were identified!");
    fast.write.csv(mir.dic, file="mgwas_snp_disease.csv", row.names=FALSE);
    res <- mir.dic[ , c("diseaseName","diseaseId","variantId", "chromosome", "coord", "score","most_severe_consequence","geneName","geneId","pmid", "source")];
    #rownames(res) <- mir.dic$mgwas;
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("variantId", "diseaseName", "pmid", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    if(idType %in% c("variantId", "hg19_coordinates")){
      rownames(mir.mat) <- uniq.mat[hit.inx,"variantId"];
    }else{
      rownames(mir.mat) <- uniq.mat[hit.inx,"diseaseName"];
    }
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    # temporary for now, to remove NA and 0 values (need to clean the sqlite)
    res <- .parse_snp2dis(res);
    # aggregated results
    res <- .get_snp2dis_aggregate(res);
    res <- res[,c("diseaseName", "diseaseId", "variantId", "chromosome", "coord","score","most_severe_consequence","geneName","geneId","pmid.x","pmid.y","source.x","source.y","pmid")]
    # update col names
    colnames(res) <- c("Disease","Disease ID","rsID", "Chr", "BP", "Score", "Consequence", "Gene","Entrez", "PMID","PMIDs", "Original DB","Original DBs","N_studies");
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Disease"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Disease ID"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Disease"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[,"rsID"]));
    dis.nms <<- c(dis.nms, as.vector(res[,"Disease"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Disease"], ID2=res[,"Disease ID"], Reference=res[,"PMIDs"], EdgeAttr1=res[,"Original DBs"],
                      EdgeAttr2=res[,"N_studies"], EdgeType=rep("snp2dis", nrow(res)), stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
    if(input.type %in% c("snp2dis", "snp2ld2dis")){
      snp2dis <<- display.res;         # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(dis.nms)),stringsAsFactors = FALSE);
      if(input.type=="snp2ld2dis"){# these snp includes input snp and snp after ld, therefore, cannot use as seeds
        seedsu <<- seedsu;
      }else{
        seedsu <<- c(seedsu, snp.nms);  
      }
      mirtableu <<- c(mirtableu, "snp2dis");
    } else if(input.type=="dis2snp"){
      dis2snp <<- display.res;
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(dis.nms)),Mapped=length(unique(snp.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, dis.nms);
      mirtableu <<- c(mirtableu, "dis2snp");
    } else{ # dis2snp2met
      dis2snp <<- display.res; # this is dis2snp result
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(dis.nms)),Mapped=length(unique(snp.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, dis.nms);
      mirtableu <<- c(mirtableu, "dis2snp");
      res <- .searchMultiNet_snp2met_all("dis2snp2met",unique(snp.nms));
      if(length(res)==0){
        current.msg <<- "No hits found in the snp2met database. Please change to dis2snp network type.";
        return(0)
      }
    }
  }
  if(is.null(snp.vec)){
    return(1);  
  }else{
    return(unique(snp.nms));
  }
}

.searchMultiNet_snp2prot<-function(input.type, snp.vec=NULL){
  #input.type<<-input.type;
  #snp.vec<<-snp.vec;
  #save.image("searchMultiNet_snp2prot.RData")
  mSetObj <- .get.mSet(mSetObj);
  idType <- mSetObj$dataSet$idType;
  mir.mat <- mSetObj$dataSet$mir.orig;
  if(length(mSetObj$dataSet$data[["snp"]])!= 0){
    # this is in multisearch module
    idType <- mSetObj$dataSet$id.types[["snp"]];
    mir.mat <- mSetObj$dataSet$data[["snp"]];
  }
  if(is.null(snp.vec)){
    idVec <- rownames(mir.mat);  
  }else{
    idVec <- snp.vec;
  }
  if (idType=="rsid"){
    chrpos.mat <- .map2chrposhg19(idVec);
  } 
  mir.dic <- Query.pQTLDB(paste(url.pre, "pqtl", sep=""), chrpos.mat);
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    current.msg <<- paste("A total of ", hit.num, "pairs of SNP-protein associations were identified!");
    fast.write.csv(mir.dic, file="mgwas_snp_protein.csv", row.names=FALSE);
    res <- mir.dic[ , c("rsid","chr_hg19","Mapped_gene","Pvalue","PMID")];

    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("rsid", "chr_hg19", "PMID", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    rownames(mir.mat) <- uniq.mat[hit.inx,"rsid"];

    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    # temporary for now, to remove NA and 0 values (need to clean the sqlite)
    res <- .parse_snp2prot(res);
    # aggregated results
    res <- .get_snp2prot_aggregate(res);
    res <- res[,c("rsid", "chr_hg19", "Mapped_gene", "Pvalue.x", "Pvalue.y", "PMID.x", "PMID.y", "PMID")]
    # update col names
    colnames(res) <- c("rsID","Chr_hg19","Gene", "Pvalue", "Pvalues", "PMID", "PMIDs", "N_PMIDs");
    display.res <- unique(res[,c("rsID","Chr_hg19","Gene", "Pvalues", "PMIDs")])
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Gene"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Gene"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Gene"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    snp.nms <<- c(snp.nms, as.vector(res[,"rsID"]));
    prot.nms <<- c(prot.nms, as.vector(res[,"Gene"]));
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Gene"], ID2=res[,"Gene"], Reference=res[,"PMIDs"], EdgeAttr1=res[,"Pvalues"],
                      EdgeAttr2=res[,"N_PMIDs"], EdgeType=rep("snp2prot", nrow(res)), stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
    if(input.type %in% c("snp2prot", "snp2ld2prot")){
      snp2prot <<- display.res;         # save this for network builder and table view
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(prot.nms)),stringsAsFactors = FALSE);
      if(input.type=="snp2ld2prot"){# these snp includes input snp and snp after ld, therefore, cannot use as seeds
        seedsu <<- seedsu;
      }else{
        seedsu <<- c(seedsu, snp.nms);  
      }
      mirtableu <<- c(mirtableu, "snp2prot");
    } else if(input.type=="prot2snp"){
      prot2snp <<- display.res;
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(prot.nms)),Mapped=length(unique(snp.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, prot.nms);
      mirtableu <<- c(mirtableu, "prot2snp");
    } 
  }
  if(is.null(snp.vec)){
    return(1);  
  }else{
    return(unique(snp.nms));
  }
}

.searchMultiNet_gene2dis<-function(input.type){
  #input.type<<-input.type;
  #save.image(".searchMultiNet_gene2dis.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  idVec <- rownames(mir.mat);  
  idType <- mSetObj$dataSet$idType;
  # accept entrez as geneId and symbol as geneName
  if (idType=="entrez"){
    idType <- "geneId";
  } else if(idType=="symbol"){
    idType <- "geneName";
  }
  tableName <- paste0("geneDiseaseNetwork");
  #save.image("searchMultiNet_gene2dis.RData")
  mir.dic <- Query.DisGeNETDB(paste(url.pre, "disgenet_2020", sep=""), idVec, tableName, idType);
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please check your input.";
    print(current.msg);
    return(0);
  } else {
    current.msg <<- paste("A total of ", hit.num, "pairs of Gene-disease associations were identified!");
    fast.write.csv(mir.dic, file="mgwas_gene_disease.csv", row.names=FALSE);
    res <- mir.dic[ , c("diseaseName","diseaseId","geneId","geneName", "score","pmid", "source")];
    #rownames(res) <- mir.dic$mgwas;
    
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("geneId", "diseaseName", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    rownames(mir.mat) <- uniq.mat[hit.inx,"geneName"];

    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    res <- res[order(res$score, decreasing = TRUE),];
    # aggregated results
    res <- .get_gene2dis_aggregate(res);
    res <- res[,c("diseaseName", "diseaseId", "geneId", "score","geneName","pmid.x","pmid.y","source.x","source.y","pmid")]
    # update col names
    colnames(res) <- c("Disease","Disease ID","Entrez ID", "Score", "Gene", "PMID","PMIDs", "Original DB","Original DBs","N_studies");
    display.res <- res;
    edge.res <- data.frame(Source=res[,"Gene"],Target=res[,"Disease"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"Entrez ID"], ID2=res[,"Disease ID"]);
    node.nms <- c(Name1=res[,"Gene"], Name2=res[,"Disease"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    gene.nms <<- c(gene.nms, as.vector(res[,"Gene"]));
    dis.nms <<- c(dis.nms, as.vector(res[,"Disease"]));
    res <- data.frame(Name1=res[,"Gene"], ID1=res[,"Entrez ID"], Name2=res[,"Disease"], ID2=res[,"Disease ID"], Reference=res[,"PMIDs"], EdgeAttr1=res[,"Original DBs"],
                      EdgeAttr2=res[,"N_studies"], EdgeType=rep("snp2gene", nrow(res)), stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
 
      gene2dis <<- display.res;
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(gene.nms)),Mapped=length(unique(dis.nms)),stringsAsFactors = FALSE);
      seedsu <<- c(seedsu, gene.nms);
      mirtableu <<- c(mirtableu, "gene2dis");
    
  }
  return(1);  
}


SearchMultiNet <- function(input.type){
   #input.type<<-input.type;
   #print(input.type)
   #save.image("SearchMultiNet.RData")

  node.ids <- vector();
  res <- 0;
  if (input.type %in% c("snp2met_csf","snp2met_saliva", "snp2met_urine","snp2met_blood","snp2met", "met2snp")){
    res <- .searchMultiNet_snp2met_all(input.type);
  }else if (input.type %in% c("eqtl_adipose_subcutaneous", "eqtl_adipose_visceral_omentum", "eqtl_adrenal_gland", "eqtl_artery_aorta", "eqtl_artery_coronary", "eqtl_artery_tibial", 
                              "eqtl_brain_amygdala", "eqtl_brain_anterior_cingulate_cortex_ba24", "eqtl_brain_caudate_basal_ganglia", "eqtl_brain_cerebellar_hemisphere", 
                              "eqtl_brain_cerebellum", "eqtl_brain_cortex", "eqtl_brain_frontal_cortex_ba9", "eqtl_brain_hippocampus", "eqtl_brain_hypothalamus", 
                              "eqtl_brain_nucleus_accumbens_basal_ganglia", "eqtl_brain_putamen_basal_ganglia", "eqtl_brain_spinal_cord_cervical_c_1", "eqtl_brain_substantia_nigra", 
                              "eqtl_breast_mammary_tissue", "eqtl_cells_cultured_fibroblasts", "eqtl_cells_ebv_transformed_lymphocytes", "eqtl_colon_sigmoid", "eqtl_colon_transverse", 
                              "eqtl_esophagus_gastroesophageal_junction", "eqtl_esophagus_mucosa", "eqtl_esophagus_muscularis", "eqtl_heart_atrial_appendage", "eqtl_heart_left_ventricle", 
                              "eqtl_kidney_cortex", "eqtl_liver", "eqtl_lung", "eqtl_minor_salivary_gland", "eqtl_muscle_skeletal", "eqtl_nerve_tibial", "eqtl_ovary", "eqtl_pancreas", 
                              "eqtl_pituitary", "eqtl_prostate", "eqtl_skin_not_sun_exposed_suprapubic", "eqtl_skin_sun_exposed_lower_leg", "eqtl_small_intestine_terminal_ileum", 
                              "eqtl_spleen", "eqtl_stomach", "eqtl_testis", "eqtl_thyroid", "eqtl_uterus", "eqtl_vagina", "eqtl_whole_blood")){
    res <- .searchMultiNet_snp2egene(substring(input.type, 6));
  }else if (input.type  %in% c("snp_pos", "met2snp2gene", "snp2ld2met","snp2ld2prot","snp2ld2egene", "snp2ld2dis")){
    res <- .searchMultiNet_snp2gene(input.type)
  }else if (input.type  %in% c("gene2snp2met")){
    res <- .searchMultiNet_gene2snp2met(input.type)
  }else if (input.type  %in% c("dis2snp", "snp2dis","dis2snp2met")){
    res <- .searchMultiNet_snp2dis(input.type)
  }else if (input.type  %in% c("snp2prot")){
    res <- .searchMultiNet_snp2prot(input.type)
  }else if(input.type == "protein2protein"){
    res <- .searchMultiNet_protein2protein(input.type);
  }else if(input.type %in% c("met2gene", "gene2met", "met2gene_expand", "gene2met_expand")){
    res <- .searchMultiNet_met2gene(input.type);
  }else if(input.type == "met2dis"){
    res <- .searchMultiNet_met2dis(input.type);
  }else if(input.type == "gene2dis"){
    res <- .searchMultiNet_gene2dis(input.type);
  }

  snp.nmsu <<- unique(c(snp.nmsu, snp.nms));
  return(res);
}

QueryStudy <- function(pmid){
  .init.multilist();
  pmid<<-pmid; # need pmid for loading json for manhattan plot
  #save.image("QueryStudy.RData");
  mSetObj <- .get.mSet(mSetObj);
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), pmid, "snp2met_study", "pmid");
  res <- mir.dic[ , c("metabolite_orig","hmdb","kegg","snp_orig", "chr", "pos_hg19","note", "name","ratio_single","beta","p_value","metabolite_id","ea","nea","pmid",
                      "most_severe_consequence", "symbol")];
  # update col names
  colnames(res) <- c("Metabolite","HMDB","KEGG","SNP", "Chr", "BP","Note","Common Name", "Single or Ratio","Beta", "P-value", "MetID", "A1", "A2", "PMID",
                     "Consequence", "Gene");
  fast.write.csv(res, file="mgwas_snp_met.csv", row.names=FALSE);
  res <- res[order(res$`P-value`),];
  res <- res[res$`P-value` != 0,];
  display.res <- res;
  met.nms <<- res[,"Common Name"];
  snp.nms <<- res[, "SNP"];
  res <- data.frame(Name1=res[,"SNP"], ID1=res[,"SNP"], Name2=res[,"Common Name"],ID2=res[,"KEGG"], Reference=res[,"PMID"], P_value=res[,"P-value"], stringsAsFactors = FALSE);
  mir.resu <- res;
  snp2met_study <- display.res;
  mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(met.nms)),stringsAsFactors = FALSE);
  mirtableu <-  "snp2met_study";
  net.info <<- .set.net.names("study");
  mSetObj$dataSet$mir.res <- mir.resu;
  mSetObj$dataSet$snp2met_study <- snp2met_study;
  mSetObj$dataSet$mirtarget <- mirtargetu;
  mSetObj$dataSet$mirtable <- unique(mirtableu);
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}


QuerySigPheMR <- function(){
  #save.image("QuerySigPheMR.RData");
  
  .init.multilist();
  mSetObj <- .get.mSet(mSetObj);
  qs.dir <- "../../data/phemr_table";
  qs.filenm <- paste0(qs.dir, ".qs");
  res <- qs::qread(qs.filenm);
  res <- res[ , c("exposure", "outcome.nm", "nsnp", "method", "b", "se", "pval")];
  res$b <- signif(res$b, digits = 5);
  res$se <- signif(res$se, digits = 5);
  res$pval <- signif(res$pval, digits = 5)

  # update col names
  colnames(res) <- c("Metabolite","Trait", "N SNP", "Method", "Effect Size", "S.E.", "P-value");
  fast.write.csv(res, file="mgwas_phe_mr_sig.csv", row.names=FALSE);
  display.res <- res;
  mir.resu <- res;
  phe_mr_sig <- display.res;
  mirtableu <-  "phe_mr_sig";
  net.info <<- .set.net.names("phe_mr_sig");
  mSetObj$dataSet$mir.res <- mir.resu;
  mSetObj$dataSet$phe_mr_sig <- phe_mr_sig;
  mSetObj$dataSet$mirtarget <- mirtargetu;
  mSetObj$dataSet$mirtable <- unique(mirtableu);
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}


QuerySingleItem <- function(idType, itemVec){
  .init.multilist();
  #idType<<-idType;
  #itemVec<<-itemVec;

  #save.image("QuerySingleItem.RData");
  mSetObj <- .get.mSet(mSetObj);
  if(idType=="rsid"){
    tableName <- "snp2met";
  }else{
    tableName <- "met2snp";
  }
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), itemVec, tableName, idType, "all", "all");
  res <- mir.dic[ , c("metabolite_orig","hmdb","kegg","snp_orig", "chr", "pos_hg19","note", "name","ratio_single","beta","p_value","metabolite_id","ea","nea","pmid",
                      "most_severe_consequence", "symbol","link")];
  # update col names
  colnames(res) <- c("Metabolite","HMDB","KEGG","SNP", "Chr", "BP","Note","Common Name", "Single or Ratio","Beta", "P-value", "MetID", "A1", "A2", "PMID",
                     "Consequence", "Gene","URL");
  fast.write.csv(res, file="mgwas_snp_met.csv", row.names=FALSE);
  res <- res[order(res$`P-value`),];
  res <- res[res$`P-value` != 0,];
  display.res <- res;
  met.nms <<- res[,"Common Name"];
  snp.nms <<- res[, "SNP"];
  res <- data.frame(Name1=res[,"SNP"], ID1=res[,"SNP"], Name2=res[,"Common Name"],ID2=res[,"KEGG"], Reference=res[,"PMID"], P_value=res[,"P-value"], stringsAsFactors = FALSE);
  mir.resu <- res;
  snp2met_single <- display.res;
  mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(met.nms)),stringsAsFactors = FALSE);
  mirtableu <-  "snp2met_single";
  net.info <<- .set.net.names("study");
  mSetObj$dataSet$mir.res <- mir.resu;
  mSetObj$dataSet$snp2met_single <- snp2met_single;
  mSetObj$dataSet$search <- snp2met_single;
  mSetObj$dataSet$mirtarget <- mirtargetu;
  mSetObj$dataSet$mirtable <- unique(mirtableu);
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}

QueryExposure <- function(mSetObj=NA, itemsStr){

  .init.multilist();
  #itemVec<<-itemVec;
  #save.image("QueryExposure.RData");
  mSetObj <- .get.mSet(mSetObj);
  itemVec <- strsplit(itemsStr, split = ", ")[[1]]
  print(itemVec);
  tableName <- "exposure";
  idType <- "name";
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), itemVec, tableName, idType, "all", "all");
  hit.num <- nrow(mir.dic);
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please make sure to select a metabolite from the drop-down list.";
    print(current.msg);
    return(0);
  } else {
  res <- mir.dic[ , c("metabolite_orig","hmdb","kegg","snp_orig", "chr", "pos_hg19","note", "name","ratio_single","beta","p_value","metabolite_id","ea","nea","pmid",
                      "most_severe_consequence", "eaf","link","se")];
  res <- .parse_snp2met_exposure(res); # remove NA
  # update col names
  colnames(res) <- c("Metabolite","HMDB","KEGG","SNP", "Chr", "BP","Note","Common Name", "Single or Ratio","Beta", "P-value", "MetID", "A1", "A2", "PMID",
                     "Consequence", "EAF","URL", "SE");
  fast.write.csv(res, file="mr_exposure_data.csv", row.names=FALSE);
  display.res <- res;
  met.nms <<- res[,"Common Name"];
  snp.nms <<- res[, "SNP"];
  res <- data.frame(Name1=res[,"SNP"], ID1=res[,"SNP"], Name2=res[,"Common Name"],ID2=res[,"KEGG"], Reference=res[,"PMID"], P_value=res[,"P-value"], stringsAsFactors = FALSE);
  mir.resu <- res;
  exposure <- display.res;
  mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(met.nms)),stringsAsFactors = FALSE);
  mirtableu <-  "exposure";
  net.info <<- .set.net.names("study");

  ## get associated metabolites for each snp
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), snp.nms, "snp2met", "rsid", "all", "all");
  print(head(mir.dic));
  library(dplyr)
  library(tidyr)
  res <- mir.dic[, c("rsid","name","symbol","entrez")];
           
# Create summary tables for metabolites and genes
    summary_table <- res %>%
      group_by(rsid) %>%
      summarise(
        metabolites = paste(unique(name), collapse = ", "),
        genes = paste(unique(symbol), collapse = ", "),
        gene_id = paste(unique(entrez), collapse = ", "),
      ) %>%
      ungroup()

    # Rename column for merging
    colnames(summary_table)[1] <- "SNP"

    # Merge with exposure data
    merged_table <- merge(exposure, summary_table, by = "SNP", all = TRUE)

    # Number of columns in the data frame
    num_cols <- ncol(merged_table)

    # Create a sequence of column indices with the first column moved to the fourth position
    # Adjust this as needed for your specific column arrangement
    new_order <- c(2:3, 1, 4:num_cols)

    # Reorder the columns
    merged_table <- merged_table[, new_order]

    mSetObj$dataSet$mir.res <- mir.resu;
    mSetObj$dataSet$exposure <- merged_table;
    mSetObj$dataSet$exposure.orig <- merged_table;
    mSetObj$dataSet$mirtarget <- mirtargetu;
    mSetObj$dataSet$mirtable <- unique(mirtableu);
  
  .set.mSet(mSetObj);
}
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}

QueryOutcome <- function(itemVec){
    if (file.exists("dis_snp_restable.csv")) {
        return(1);
    }

  mSetObj <- .get.mSet(mSetObj);
  itemVec.id <- trimws(itemVec);
  # itemVec <<-itemVec;
  # save.image("QueryOutcome.RData")
  ieugwas.db <- .get.my.lib("ieugwas_202210.qs");
  ieugwas.res <- ieugwas.db[ieugwas.db$id == itemVec.id,];
  hit.num <- nrow(ieugwas.res);
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please make sure to select an outcome from the drop-down list.";
    print(current.msg);
    return(0);
  } else {
  mSetObj$dataSet$outcome <- ieugwas.res;
  .set.mSet(mSetObj);
  }

  fast.write.csv(ieugwas.res, file="dis_snp_restable.csv");

  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }  
}

QueryPheMR <- function(itemVec, resOpt){
  .init.multilist();
  # itemVec<<-itemVec;
  # resOpt<<-resOpt;
  # save.image("QueryPheMR.RData");
  mSetObj <- .get.mSet(mSetObj);
  met.nms <- .get.my.lib("phemr_browse_met_nms.qs");
  dis.nms <- .get.my.lib("phemr_browse_dis_nms.qs")
  
  if (itemVec %in% met.nms){
    colName <- "exposure"
  }else{
    colName <- "outcome_nm"
  }
  mir.dic <- Query.PheMRDB(paste(url.pre, "phemr", sep=""), itemVec, colName, resOpt);
  hit.num <- nrow(mir.dic);
  if (hit.num == 0) {
    current.msg <<- "No hits found in the database. Please make sure to select a metabolite or trait from the drop-down list.";
    print(current.msg);
    return(0);
  } else {
    
    if(resOpt == "mr_results"){
      .query_mr_results(mir.dic, resOpt)
    }else if(resOpt == "mr_sensitivity"){
      .query_mr_sensitivity(mir.dic, resOpt)
    }else if(resOpt == "mr_single"){
      .query_mr_single(mir.dic, resOpt)
    }
    mSetObj$dataSet$mir.res <- mir.resu;
    mSetObj$dataSet$mr_results <- mr_results;
    mSetObj$dataSet$mr_sensitivity <- mr_sensitivity;
    mSetObj$dataSet$mr_single <- mr_single; 
    mSetObj$dataSet$mirtarget <- mirtargetu;
    mSetObj$dataSet$mirtable <- unique(mirtableu);
    .set.mSet(mSetObj);
  }
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}


SetPpiDb  <- function(db, req, conf){
  #db<<-db;
  #req<<-req;
  #conf<<-conf;
  #save.image("SetPpiDb.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$ppiOpts$db.name=db;
  if(req=="true"){
    mSetObj$dataSet$ppiOpts$require.exp=TRUE;
  }else{
    mSetObj$dataSet$ppiOpts$require.exp=FALSE;
  }
  mSetObj$dataSet$ppiOpts$min.score=as.numeric(conf);
  .set.mSet(mSetObj);
}

.searchMultiNet_protein2protein <-function(input.type){
  #input.type<<-input.type;
  #save.image("searchMultiNet_protein2protein.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.vec <- unique(unname(nodeu.ids))
  table.nm = paste("hsa", mSetObj$dataSet$ppiOpts$db.name, sep="_")
  require.exp = mSetObj$dataSet$ppiOpts$require.exp
  min.score = as.numeric(mSetObj$dataSet$ppiOpts$min.score)
  
  mir.dic <- QueryPpiSQLiteZero(table.nm, mir.vec, require.exp, min.score);
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the PPI database. Please check your input. ";
    print(current.msg);
    return(0);
  } else{
    res <- mir.dic[ , c("id1", "id2", "name1", "name2")];
    #rownames(res) <- mir.dic$mirnet;
    current.msg <<- paste("A total of unqiue", hit.num, "pairs of  protein-protein interactions were identified!");
    
    # update the data
    
    colnames(res) <- c("ID1","ID2","Symbol1","Symbol2");
    if(table.nm == "hsa_string"){
      res$Literature <- rep("25352553", nrow(res));
    }else if(table.nm == "hsa_innate"){
      res$Literature <- rep("23180781", nrow(res));
    }else if(table.nm == "hsa_rolland"){
      res$Literature <- rep("25416956", nrow(res));
    }else if(table.nm == "hsa_huri"){
      res$Literature <- rep("32296183", nrow(res));
    }
    display.res <- res;
    edge.res <- data.frame(Source=res[,"Symbol1"],Target=res[,"Symbol2"],stringsAsFactors = FALSE);    # IDs
    
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"Symbol1"], ID2=res[,"Symbol2"]);
    node.nms <- c(Name1=res[,"ID1"], Name2=res[,"ID2"]);
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    na.res = rep("Not Applicable", nrow(res))
    ## need to change the column name ##
    gene.nms <<- c(gene.nms, res[,"Symbol1"], res[,"Symbol2"]);
    res <- data.frame(Name1=res[,"Symbol1"], ID1=res[,"ID1"], Name2=res[,"Symbol2"], ID2=res[,"ID2"], Reference=res[,"Literature"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("ppi", nrow(res)),stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
    mirtargetu <<- c(mirtargetu, "protein2protein");
    protein2protein <<- rbind(protein2protein, display.res);
    mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(mir.dic[,"name1"])),Mapped=length(unique(mir.dic[,"name2"])),stringsAsFactors = FALSE);
    mirtableu <<- c(mirtableu, "protein2protein");
  }
  return(1);
}

.searchMultiNet_met2gene <-function(input.type){
  #input.type<<-input.type;
  #save.image("searchMultiNet_met2gene.RData")
  mSetObj <- .get.mSet(mSetObj);
  idType <- mSetObj$dataSet$idType;
  mir.mat <- mSetObj$dataSet$mir.orig;
  for(i in 1:length(mSetObj$dataSet$dbOpt)){
    dbOpt <- mSetObj$dataSet$dbOpt[i];
    mir.dic <- .searchM2GDB(input.type, dbOpt);
  }

  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the met2gene database. Please check your input. ";
    print(current.msg);
    return(0);
  } else{
    if(input.type %in% c("met2gene", "gene2met")){
      # record the mapped queries and change to same IDs used in network
      uniq.mat <- unique(mir.dic[, c("hmdb", "name", idType)]);
      hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
      rownames(mir.mat) <- uniq.mat[hit.inx,"name"];
      mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    }
    current.msg <<- paste("A total of unqiue", hit.num, "pairs of knowledge-based metabolite-gene interactions were identified!");
    res <- mir.dic[,c(2,3,4,7,8,10,12,13)];
    rownames(res) <- mir.dic$met2gene_id;
    # update the data
    colnames(res) <- c("HMDB","Gene","Type","KEGG","Metabolite","Entrez","Gene Name", "Ensembl");
    res <- res[complete.cases(res[ , 5]),]; # temporary use this to remove NA 
    display.res <- res;
    edge.res <- data.frame(Source=res[,"Metabolite"],Target=res[,"Gene"],stringsAsFactors = FALSE);    # IDs
    
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"HMDB"], ID2=res[,"Entrez"]);
    node.nms <- c(Name1=res[,"Metabolite"], Name2=res[,"Gene"]);
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    met.nms <<- c(met.nms, as.vector(res[,"Metabolite"]));
    gene.nms <<- c(gene.nms, res[,"Gene"]);
    res <- data.frame(Name1=res[,"Gene"],ID1=res[,"Entrez"], Name2=res[,"Metabolite"],ID2=res[,"KEGG"], Reference=res[,"Type"], EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("met2gene", nrow(res)), stringsAsFactors = FALSE);
    
    mir.resu <<- rbind(mir.resu, res);
    if(input.type %in% c("gene2met", "gene2met_expand")){
      mirtargetu <<- c(mirtargetu, "gene2met");
      gene2met <<- rbind(gene2met, display.res);
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(res[,c(1)])),Mapped=length(unique(res[,c(2)])),stringsAsFactors = FALSE);
      mirtableu <<- c(mirtableu, "gene2met");
    }else{
      mirtargetu <<- c(mirtargetu, "met2gene");
      met2gene <<- rbind(met2gene, display.res);
      mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(res[,c(2)])),Mapped=length(unique(res[,c(1)])),stringsAsFactors = FALSE);
      mirtableu <<- c(mirtableu, "met2gene");
    }
  }
  return(1);
}

.searchMultiNet_met2dis <-function(input.type){
  #input.type<<-input.type;
  #save.image("searchMultiNet_met2dis.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  met.vec <- mSetObj$dataSet$map.table[,2]; # name
  tissue <- mSetObj$dataSet$tissue;
  idType <- mSetObj$dataSet$idType;
  population <- mSetObj$dataSet$population;
  table.nm = "metabo_phenotypes";
  mir.dic <- QueryMet2Dis(paste(url.pre, "met2dis", sep=""), met.vec, table.nm, "name");
  
  hit.num <- nrow(mir.dic)
  if (hit.num == 0) {
    current.msg <<- "No hits found in the met2gene database. Please check your input. ";
    print(current.msg);
    return(0);
  } else{
    # record the mapped queries and change to same IDs used in network
    uniq.mat <- unique(mir.dic[, c("ctdid", "name", idType)]);
    hit.inx <- match(rownames(mir.mat), uniq.mat[, idType]);
    rownames(mir.mat) <- uniq.mat[hit.inx,"name"];
    
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    current.msg <<- paste("A total of unqiue", hit.num, "pairs ofmetabolite-associations were identified!");
    res <- mir.dic
    #rownames(res) <- mir.dic$met2gene_id;
    # update the data
    colnames(res) <- c("KEGG","OMIMID","Metabolite","Disease","PubChemCID","Score","PMID");
    display.res <- res;
    edge.res <- data.frame(Source=res[,"Metabolite"],Target=res[,"Disease"],stringsAsFactors = FALSE);    # IDs
    
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"KEGG"], ID2=res[,"Disease"]);
    node.nms <- c(Name1=res[,"Metabolite"], Name2=res[,"Disease"]);
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    met.nms <<- c(met.nms, as.vector(res[,"Metabolite"]));
    dis.nms <<- c(dis.nms, res[,"Disease"]);
    res <- data.frame(Name1=res[,"Disease"], ID1=res[,"Disease"], Name2=res[,"Metabolite"],ID2=res[,"KEGG"], Reference=rep("HMDB", nrow(res)), EdgeAttr1=rep("", nrow(res)),
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("met2dis", nrow(res)), stringsAsFactors = FALSE);
    
    mir.resu <<- rbind(mir.resu, res);
    mirtargetu <<- c(mirtargetu, "met2dis");
    met2dis <<- rbind(met2dis, display.res);
    mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(res[,c(2)])),Mapped=length(unique(res[,c(1)])),stringsAsFactors = FALSE);
    mirtableu <<- c(mirtableu, "met2dis");
  }
  return(1);
}

.searchM2GDB <- function(input.type, dbOpt){
  #dbOpt<<-dbOpt;
  #save.image("searchM2GDB.RData")
  mSetObj <- .get.mSet(mSetObj);
  mir.mat <- mSetObj$dataSet$mir.orig;
  tissue <- mSetObj$dataSet$tissue;
  idType <- mSetObj$dataSet$idType;
  population <- mSetObj$dataSet$population;  
  table.nm = "met2gene";
  if(input.type=="gene2met"){
    id.vec <- rownames(mir.mat);
    col.nm <- "symbol";
  }else if(input.type=="met2gene"){ # this is starting from the metabolite module
    id.vec <- mSetObj$dataSet$map.table[,3]; # hmdb id
    col.nm <- "hmdb";
  }else if(input.type=="gene2met_expand"){# this is starting from the snp module and added met2gene expanded network
    id.vec <- unique(unname(nodeu.ids));
    col.nm <- "symbol";
  }else{# this is starting from the snp module and added met2gene expanded network
    id.vec <- unique(unname(nodeu.ids));
    col.nm <- "hmdb";
  }
  res <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), id.vec, "met2gene", col.nm, "all", "all",dbOpt);
  m2g.resu <<- rbind(m2g.resu, res);
}

.searchMultiNet_snp2egene <- function(input.type, snp.vec=NULL){
  mSetObj <- .get.mSet(mSetObj);
  idType <- mSetObj$dataSet$id.types[["snp"]];
  mir.mat <- mSetObj$dataSet$data[["snp"]];
  # input.type<<-input.type;
  # save.image("searchMultiNet_snp2egene.RData")
  if(is.null(snp.vec)){
    idVec <- rownames(mir.mat);  
  }else{
    idVec <- snp.vec;
  }
  mir.dic <- Query.eQTLDB(paste(url.pre, "eqtl/", input.type, sep=""), idVec, input.type, idType);
  
  hit.num <- length(unique(mir.dic[,c("rsid")]));
  if (hit.num == 0) {
    current.msg <<- "No hits found in the eQTL mapping. Please check your input. ";
    print(current.msg);
    return(0);
  } else {
    fast.write.csv(mir.dic, file=paste0("mgwas_snp_eqtl_",input.type,".csv"), row.names=FALSE);
    res <- mir.dic[, c("rsid","chr","pos","gene_id", "pval_nominal","slope")];
    #rownames(res) <- mir.dic$mirnet;
    res$tissue <- rep(input.type, nrow(res));
    res$gene_id <- unlist(lapply(res$gene_id, function(x) substring(x,1,15))) # remove version number in ensembl id 
    current.msg <<- paste("A total of unqiue", hit.num, "SNPs were mapped to eQTLs!");
    
    # update the data
    gd.inx <- rownames(mir.mat) %in% unique(res[, idType]);
    mir.mat <- mir.mat[gd.inx,,drop=F];
    mir.mappedu <<- rbind(mir.mappedu, mir.mat);
    
    colnames(res) <- c("rsID","Chr","BP", "Ensembl", "P-value", "Signed_stats", "Tissue");
    # need to map to gene symbol
    display.res <- res;
    edge.res <- data.frame(Source=res[,"rsID"],Target=res[,"Ensembl"],stringsAsFactors = FALSE);    # IDs
    if(nrow(res)!=0){
      row.names(edge.res) <- 1:nrow(res);
    }
    
    node.ids <- c(ID1=res[,"rsID"], ID2=res[,"Ensembl"]);
    node.nms <- c(Name1=res[,"rsID"], Name2=res[,"Ensembl"]);
    
    edgeu.res <<- rbind(edgeu.res, edge.res); #edgeu.res is an empty dataframe defined in QueryNet
    edgeu.res <<- edgeu.res[!duplicated(edgeu.res), ];
    nodeu.ids <<- c(nodeu.ids, node.ids);
    edgeNumU <<- c(edgeNumU, nrow(edge.res))
    nodeu.nms <<- c(nodeu.nms, node.nms);
    
    egene.nms <<- res[,"Ensembl"];
    snp.nms <<- res[, "rsID"];
    res <- data.frame(Name1=res[,"rsID"], ID1=res[,"rsID"], Name2=res[,"Ensembl"], ID2=res[,"Ensembl"],Reference=res[,"Tissue"], EdgeAttr1=res[,"P-value"],
                      EdgeAttr2=rep(1, nrow(res)), EdgeType=rep("snp2egene", nrow(res)),stringsAsFactors = FALSE);
    mir.resu <<- rbind(mir.resu, res);
    mirtargetu <<- c(mirtargetu, "snp2egene");
    snp2egene <<- display.res;         # save this for network builder and table view
    mSetObj$dataSet$tableStats <- data.frame(Query=length(unique(snp.nms)),Mapped=length(unique(egene.nms)),stringsAsFactors = FALSE);
    seedsu <<- c(seedsu, snp.nms);
    mirtableu <<- c(mirtableu, "snp2egene");
  }
  if(is.null(snp.vec)){
    return(1);  
  }else{
    return(unique(snp.nms));
  }
}

.query_mr_results <- function(mir.dic, resOpt){
  res <- mir.dic[ , c("exposure", "outcome_nm", "nsnp", "method", "b", "se", "pval")];
  res$b <- signif(res$b, digits = 5);
  res$se <- signif(res$se, digits = 5);
  res$pval <- signif(res$pval, digits = 5)
  
  res <- res[order(res$pval),];
  # update col names
  colnames(res) <- c("Metabolite","Trait", "N SNP", "Method", "Effect Size", "S.E.", "P-value");
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_results <<- display.res;
  mirtableu <<-  "mr_results";
  net.info <<- .set.net.names("mr_results");
}

.query_mr_sensitivity <- function(mir.dic, resOpt){
  mir.dic$correct_causal_direction <- ifelse(mir.dic$correct_causal_direction == 1, "Yes", "No")
  res <- mir.dic;
  res$steiger_pval <- signif(res$steiger_pval, digits = 5);
  res$Q_pval <- signif(res$Q_pval, digits = 5);
  res$pval <- signif(res$pval, digits = 5)
  
  # update col names
  colnames(res) <- c("Metabolite","Trait", "Directionality", "Steiger P-value", "Heterogeneity Method", 
                     "Heterogeneity P-value", "Pleiotropy P-value");
  
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_sensitivity <<- display.res;
  mirtableu <<-  "mr_sensitivity";
  net.info <<- .set.net.names("mr_sensitivity");
}

.query_mr_single <- function(mir.dic, resOpt){
  res <- mir.dic[ , c("exposure", "outcome_nm", "SNP", "b", "se", "p")];
  res$b <- signif(res$b, digits = 5);
  res$se <- signif(res$se, digits = 5);
  res$p <- signif(res$p, digits = 5)
  
  res$method <- "Wald ratio";
  res <- res[order(res$p),]
  # update col names
  colnames(res) <- c("Metabolite","Trait", "SNP",  "Effect Size", "S.E.", "P-value","Method");
  res <- res[,c("Metabolite","Trait", "SNP", "Method", "Effect Size", "S.E.", "P-value")]
  file.nm <- paste0("browse_", resOpt, ".csv")
  fast.write.csv(res, file=file.nm, row.names=FALSE);
  display.res <- res;
  mir.resu <<- res;
  mr_single <<- display.res;
  mirtableu <<-  "mr_single";
  net.info <<- .set.net.names("mr_single");

}

GetQueryNum <-function(){
  if(net.type=="metabo_phenotypes"){
    return(as.numeric(net.stats$Query))
  }
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$query.nums)
}

GetTypeNum <-function(){
  if(net.type=="metabo_phenotypes"){
    return(as.numeric(net.stats$Node))
  }
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$type.nums)
}

PrepareCSV <- function(table.nm){
  # table.nm<<-table.nm;
  # save.image("PrepareCSV.RData")
  mSetObj <- .get.mSet(mSetObj);
  if(table.nm=="mr_res_single"){
    fast.write.csv(mSetObj$dataSet$mr_res_single, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res_loo"){
    fast.write.csv(mSetObj$dataSet$mr_res_loo, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="mr_res"){
    fast.write.csv(mSetObj$dataSet$mr_results, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  }else if(table.nm=="manhattan"){
    fast.write.csv(mSetObj$dataSet$snp2met, file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else if(anal.type == "multilist" || anal.type == "snp2mir" || anal.type == "tf2genemir" || anal.type == "gene2tfmir"){
    fast.write.csv(mSetObj$dataSet[[table.nm]], file=paste(table.nm, ".csv", sep=""), row.names = FALSE);
  } else {
    fast.write.csv(mSetObj$dataSet$snp.res, file=paste(net.type, ".csv", sep=""), row.names = FALSE);
  }
}

GetTableNames <- function(){
  #save.image("GetTableNames.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mirtable;
}


