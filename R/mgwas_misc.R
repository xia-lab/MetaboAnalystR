##################################################
## R script for mGWAS
## Description: utility functions
###################################################

QueryMet2Dis <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  statement <- paste("SELECT * FROM ", table.nm, " 
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  mir.dic <- .query.sqlite(mir.db, statement);
  return(mir.dic);

}

Query.mGWASDB <- function(db.path, q.vec, table.nm, col.nm, biofluid="all", population="all", db.opt="kegg"){
  require('RSQLite');
  db.path <- paste0(db.path, ".sqlite");
 
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(table.nm=="snp_annot"){
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN entrez
                     ON entrez.symbol=",table.nm,".symbol
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");

  }else if(table.nm %in% c("snp2met_csf","snp2met_saliva", "snp2met_urine","snp2met_blood", "snp2met_all","snp2met")){
    table1.nm <- "snp2met";
    col.nm <- "rsid"; # temporary for now
    table2.nm <- "snp";
    statement <- .get_statement(table1.nm, table2.nm, col.nm, query, biofluid, population);
  }else if(table.nm=="met2snp"){
    table1.nm <- "snp2met";
    table2.nm <- "metabolites";
    statement <- .get_statement(table1.nm, table2.nm, col.nm, query, biofluid, population);
  }else if(table.nm=="exposure"){
    statement <- paste("SELECT * FROM snp2met LEFT JOIN snp
                    ON snp.snp_orig=snp2met.snp_orig
                     LEFT JOIN metabolites
                     ON metabolites.metabolite_id=snp2met.metabolite_id
                     LEFT JOIN snp_af
                     ON snp_af.rsid=snp.rsid AND snp_af.ea=snp2met.ea 
                     LEFT JOIN study
                     ON study.pmid=snp2met.pmid
                     WHERE metabolites.", col.nm," IN (", query, ")", sep="");
  }else if(table.nm=="gene2snp"){
    statement <- paste("SELECT * FROM snp LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                     WHERE gene.", col.nm," IN (", query, ")", sep="");
    
  }else if(table.nm=="snp2met_study"){
    table.nm <- "snp2met";
    if(query=="'Viñuela_medRxiv_2021_targeted'"){
      query <- "'Viñuela_medRxiv_2021'";
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                      AND (note = 'targeted')", sep="");
    } else if(query=="'Viñuela_medRxiv_2021_untargeted'"){
      query <- "'Viñuela_medRxiv_2021'";
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                      AND (note = 'untargeted')", sep="");
    }else{
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
    }
  }else if(table.nm %in% c("met2gene","gene2met")){
    if(db.opt=="kegg"){
      db.opt <- "KEGG Enzyme"
    }else if(db.opt=="tcdb"){
      db.opt <- "TCDB Transporter"
    }else if(db.opt=="recon3d"){
      db.opt <- "Recon3D"
    }
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN metabolites
                        ON metabolites.hmdb=",table.nm,".hmdb 
                        LEFT JOIN gene
                        ON gene.symbol=",table.nm,".symbol
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                       AND type = ('" ,db.opt,"')
                       ", sep="");
    
    
  }else if(table.nm=="met2dis"){
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN metabolites
                        ON metabolites.hmdb=",table.nm,".hmdb 
                       WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  }else{
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN entrez
                     ON entrez.symbol=",table.nm,".symbol
                     LEFT JOIN metabolites
                     ON metabolites.hmdb=",table.nm,".hmdb
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  }

    mir.dic <- .query.sqlite(mir.db, statement);
  # remove duplicates
  if(table.nm=="snp2met" || table.nm=="snp2met_merge"|| table.nm=="met2snp" || table.nm=="exposure" ){
    dup.inx <- duplicated(mir.dic$snp2met_id);
  }else if(table.nm %in% c("met2gene","gene2met")){
    dup.inx <- duplicated(mir.dic$met2gene_id);
  }else if(table.nm=="met2dis"){
    dup.inx <- duplicated(mir.dic$met2dis_id);
  }else if(table.nm=="gene2snp"){
    # one snp is mapped to one gene
    dup.inx <- duplicated(mir.dic$rsid);
  }else{
    dup.inx <- duplicated(mir.dic$mgwas);
  }
  mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

Query.DisGeNETDB <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  #save.image("Query.DisGeNETDB.RData")

  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      #print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(anal.type=="dis2snp"){
    statement <- paste("SELECT * FROM ", table.nm,
                       " LEFT JOIN variantAttributes
                     ON variantAttributes.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=variantDiseaseNetwork.diseaseNID
                     LEFT JOIN variantGene
                     ON variantGene.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=variantGene.geneNID",
                       " WHERE ", col.nm," IN (", query, ")", sep="");

  }else{
    if(table.nm=="geneDiseaseNetwork"){
      statement <- paste("SELECT * FROM ", table.nm,
                         " LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=geneDiseaseNetwork.geneNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=geneDiseaseNetwork.diseaseNID
                     WHERE ", col.nm," IN (", query, ")", sep="");
    }else{
      statement <- paste("SELECT * FROM ", table.nm,
                         " LEFT JOIN variantAttributes
                     ON variantAttributes.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=variantDiseaseNetwork.diseaseNID
                     LEFT JOIN variantGene
                     ON variantGene.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=variantGene.geneNID",
                         " WHERE ", col.nm," IN (", query, ")", sep="");
    }
  }

  mir.dic <- .query.sqlite(mir.db, statement);
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

`%notin%` <- Negate(`%in%`)

.get_statement <- function(table1.nm,table2.nm, col.nm, query, biofluid, population){
  statement1 <- paste("SELECT * FROM ", table1.nm,
                      " LEFT JOIN snp
                    ON snp.snp_orig=",table1.nm,".snp_orig
                     LEFT JOIN metabolites
                     ON metabolites.metabolite_id=",table1.nm,".metabolite_id
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                     LEFT JOIN study
                     ON study.pmid=",table1.nm,".pmid", sep="");
  if(biofluid=="all" && population=="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ")", sep="");
  }else if(biofluid=="all" && population!="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND pop_code=('",population,"')", sep="");
  }else if(biofluid!="all" && population=="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND biofluid = ('" ,biofluid,"')", sep="");
  }else(
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND biofluid = ('" ,biofluid,"') AND pop_code=('",population,"')", sep="")
  )
  statement <- paste(statement1, statement2)
}

.parse_snp2met <- function(res){
  res <- res[complete.cases(res[ , 7]),];
  res <- res[complete.cases(res[ , 1]),];
  res <- res[res$p_value != 0,];
  res <- res[res$p_value != "NA",];
  res <- res[order(res$p_value),];
  res$p_value <- format(res$p_value, digits = 4);
  return(res);
}

.parse_snp2dis <- function(res){
  res <- res[complete.cases(res[ , 10]),];
  res <- res[order(res$score, decreasing = TRUE),];
  return(res);
}

.parse_snp2prot <- function(res){
  res <- res[res$Pvalue != 0,];
  res <- res[order(res$Pvalue, decreasing = FALSE),];
  return(res);
}

.get_snp2met_aggregate <- function(res){
  res$`paste(rsid, name)` <- paste(res$rsid, res$name);
  agg.pmid <- aggregate(pmid ~ paste(rsid,name), data = res, paste, collapse = "|");
  agg.pval <- aggregate(p_value ~ paste(rsid,name), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(rsid,name), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(rsid, name)");
  agg.res <- merge(agg.res, agg.pval, by="paste(rsid, name)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(rsid, name)");
}

.get_snp2dis_aggregate <- function(res){
  res$`paste(variantId, diseaseId)` <- paste(res$variantId, res$diseaseId);
  agg.pmid <- aggregate(pmid ~ paste(variantId, diseaseId), data = res, paste, collapse = "|");
  agg.source <- aggregate(source ~ paste(variantId, diseaseId), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(variantId, diseaseId), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(variantId, diseaseId)");
  agg.res <- merge(agg.res, agg.source, by="paste(variantId, diseaseId)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(variantId, diseaseId)");
  
}

.get_snp2prot_aggregate <- function(res){
  res$`paste(rsid, Mapped_gene)` <- paste(res$rsid, res$Mapped_gene);
  agg.pmid <- aggregate(PMID ~ paste(rsid, Mapped_gene), data = res, paste, collapse = "|");
  agg.pval <- aggregate(Pvalue ~ paste(rsid, Mapped_gene), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(PMID ~ paste(rsid, Mapped_gene), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(rsid, Mapped_gene)");
  agg.res <- merge(agg.res, agg.pval, by="paste(rsid, Mapped_gene)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(rsid, Mapped_gene)");
  
}

.get_gene2dis_aggregate <- function(res){
  res$`paste(geneId, diseaseId)` <- paste(res$geneId, res$diseaseId);
  agg.pmid <- aggregate(pmid ~ paste(geneId, diseaseId), data = res, paste, collapse = "|");
  agg.source <- aggregate(source ~ paste(geneId, diseaseId), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(geneId, diseaseId), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(geneId, diseaseId)");
  agg.res <- merge(agg.res, agg.source, by="paste(geneId, diseaseId)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(geneId, diseaseId)");
  
}

QueryVEP <- function(q.vec,vepDis,content_type="application/json" ){
  library(httr)
  #library(jsonlite)
  #library(xml2)
  server <- "http://rest.ensembl.org"
  ext <- "/vep/human/id/"
  r=list()
  resvep = list()
  vepDis = as.numeric(vepDis)*1000
  
  for(i in 1:length(q.vec)){
    r[[i]] <- GET(paste(server, ext, q.vec[i],"?distance=",vepDis,sep = ""),  accept(content_type))
    stop_for_status(r[[i]])
    resvep[[i]] = content(r[[i]])[[1]][["transcript_consequences"]]
    # resvep[[i]] =lapply( resvep[[i]] , function(x) {c(x,q.vec[i])})
  }
  names(resvep)=q.vec
  
  #saveRDS(resvep,"~/Documents/omicsnet/omicsnet-microbiome/resvep.rds")
  resvep2 = lapply(resvep,function(s) {lapply(s, function(x) {
    list(
      gene_symbol=ifelse(length(x[["gene_symbol"]])!=0,x[["gene_symbol"]],"NA"),
      gene_id=ifelse(length(x[["gene_id"]]) !=0, x[["gene_id"]],'NA'),
      hgnc_id=ifelse(length(x[["hgnc_id"]]) !=0, x[["hgnc_id"]],'NA'),
      transcript_id=ifelse(length(x[["transcript_id"]]) !=0, x[["transcript_id"]],'NA'),
      consequence_terms=ifelse(length(x[["consequence_terms"]]) !=0, paste(unlist(x[["consequence_terms"]]),collapse = ";"),'NA'),
      distance=ifelse(length(x[["distance"]])!=0, x[["distance"]],'NA'))
  })
  }
  )
  resvep3 = do.call(rbind,lapply(resvep2,function(s) {
    do.call(rbind.data.frame,s)
  } )
  )
  resvep3$rsid = gsub("\\.[0-9]*","",rownames(resvep3))
  
  row.names(resvep3)=NULL
  return(resvep3)
}

phenoscanner <- function(snpquery=NULL, genequery=NULL, regionquery=NULL, catalogue="GWAS", pvalue=1E-5, proxies="None", r2=0.8, build=37){
  cat("PhenoScanner V2\n")
  if(is.null(snpquery) & is.null(regionquery) & is.null(genequery)) stop("no query has been requested")
  if((length(snpquery[1])+length(regionquery[1])+length(genequery[1]))>1) stop("only one query type allowed")
  if(!(catalogue=="None" | catalogue=="GWAS" | catalogue=="eQTL" | catalogue=="pQTL" | catalogue=="mQTL" | catalogue=="methQTL")) stop("catalogue has to be one of None, GWAS, eQTL, pQTL, mQTL or methQTL")
  if(!(proxies=="None" | proxies=="AFR" | proxies=="AMR" | proxies=="EAS" | proxies=="EUR" | proxies=="SAS")) stop("proxies has to be one of None, AFR, AMR, EAS, EUR or SAS")
  if(length(snpquery)>100) stop("a maximum of 100 SNP queries can be requested at one time")
  if(length(genequery)>10) stop("a maximum of 10 gene queries can be requested at one time")
  if(length(regionquery)>10) stop("a maximum of 10 region queries can be requested at one time")
  if(!(pvalue>0 & pvalue<=1)) stop("the p-value threshold has to be greater than 0 and less than or equal to 1")
  if(!(r2>=0.5 & r2<=1)) stop("the r2 threshold has to be greater than or equal to 0.5 and less than or equal to 1")
  if(!(build==37 | build==38)) stop("the build has to be equal to 37 or 38")
  if(!is.null(regionquery)){
    ub <- as.numeric(sub(".*-", "", sub(".*:", "",regionquery)))
    lb <- as.numeric(sub("-.*", "", sub(".*:", "",regionquery)))
    dist <- ub - lb
    if(any(dist>1000000)) stop("region query can be maximum of 1MB in size")
  }
  if(length(snpquery)>0){
    results <- data.frame()
    snps <- data.frame()
    n_queries <- length(snpquery) %/% 10
    if((length(snpquery) %% 10)>0){n_queries <- n_queries + 1}
    for(i in 1:n_queries){
      if(i < n_queries){qsnps <- paste0(snpquery[(1+10*(i-1)):(10*i)], collapse="+")}else{qsnps <- paste0(snpquery[(1+10*(i-1)):length(snpquery)], collapse="+")}
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?snpquery=",qsnps,"&catalogue=",catalogue,"&p=",pvalue,"&proxies=",proxies,"&r2=",r2,"&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$snps)==0){
        print(paste0("Error: ",json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          if(length(snpquery)==1){print(paste0(snpquery," -- queried"))}else{print(paste0(i," -- chunk of 10 SNPs queried"))}
        }else{if(length(snpquery)==1){print(paste0("Warning: no results found for ",snpquery))}else{print(paste0("Warning: no results found for chunk ",i))}}
      }
      if(length(json_data$snps)>0){
        fields_snps <- json_data$snps[[1]]; json_data$snps[[1]] <- NULL
        if(length(json_data$snps)>0){
          tables_snps <- as.data.frame(matrix(unlist(json_data$snps), ncol=length(fields_snps), byrow=T), stringsAsFactors=F)
          names(tables_snps) <- fields_snps
          snps <- rbind(snps,tables_snps)
          if(length(json_data$results)==0){if(length(snpquery)==1){print(paste0(snpquery," -- queried"))}else{print(paste0(i," -- chunk of 10 SNPs queried"))}}
        }else{if(length(json_data$results)==0){if(length(snpquery)==1){print(paste0("Warning: no results found for ",snpquery))}else{print(paste0("Warning: no results found for chunk ",i))}}}
      }
    }
    output <- list(snps=snps, results=results)
  }
  if(length(genequery)>0){
    results <- data.frame()
    genes <- data.frame()
    n_queries <- length(genequery)
    for(i in 1:n_queries){
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?genequery=",genequery[i],"&catalogue=",catalogue,"&p=",pvalue,"&proxies=None&r2=1&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$genes)==0){
        print(paste0("Error: ", json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          print(paste0(genequery," -- queried"))
        }else{print(paste0("Warning: no results found for ",genequery))}
      }
      if(length(json_data$genes)>0){
        fields_genes <- json_data$genes[[1]]; json_data$genes[[1]] <- NULL
        if(length(json_data$genes)>0){
          tables_genes <- as.data.frame(matrix(unlist(json_data$genes), ncol=length(fields_genes), byrow=T), stringsAsFactors=F)
          names(tables_genes) <- fields_genes
          genes <- rbind(genes,tables_genes)
        }
      }
    }
    output <- list(genes=genes, results=results)
  }
  if(length(regionquery)>0){
    results <- data.frame()
    regions <- data.frame()
    n_queries <- length(regionquery)
    for(i in 1:n_queries){
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?regionquery=",regionquery[i],"&catalogue=",catalogue,"&p=",pvalue,"&proxies=None&r2=1&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$locations)==0){
        print(paste0("Error: ",json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          print(paste0(regionquery," -- queried"))
        }else{print(paste0("Warning: no results found for ",regionquery))}
      }
      if(length(json_data$locations)>0){
        fields_regions <- json_data$locations[[1]]; json_data$locations[[1]] <- NULL
        if(length(json_data$locations)>0){
          tables_regions <- as.data.frame(matrix(unlist(json_data$locations), ncol=length(fields_regions), byrow=T), stringsAsFactors=F)
          names(tables_regions) <- fields_regions
          regions <- rbind(regions,tables_regions)
        }
      }
    }
    output <- list(regions=regions, results=results)
  }
  if(is.null(output)) stop("there is no output")
  cat("PhenoScanner done\n")
  return(output)
}

QueryHaploreg <- function(query=NULL, file=NULL,
                          study=NULL,
                          ldThresh=0.8, 
                          ldPop="EUR", 
                          epi="vanilla", 
                          cons="siphy", 
                          genetypes="gencode",
                          url=Haploreg.settings[["base.url"]],
                          timeout=100,
                          encoding="UTF-8",
                          querySNP=FALSE,
                          fields=NULL,
                          verbose=FALSE) {
  
  if(!is.null(query) & !is.null(file))
  {
    stop("'query' and 'file' can not be supplied simultaneously")
  }
  parameters <- list(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                     epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                     encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
  
  recTable <- data.frame(matrix(nrow=0, ncol=35))
  if(!is.null(query))
  {
    if(grepl("chr", query[1])) 
    {
      res <- lapply(1:length(query), function(i) {
        simpleQuery(query=query[i], file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                    epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                    encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      })
      
      recTable <- ldply(res, data.frame)
      recTable <- recTable[!duplicated(recTable), ]
    } else {
      
      recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                              epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                              encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
    }
  } else if(!is.null(file)) 
  {
    con <- file(file)
    lines <- readLines(con)
    close(con)
    if(grepl("chr", lines[1]))
    {
      res <- lapply(1:length(lines), function(i) {
        simpleQuery(query=lines[i], file=NULL, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                    epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                    encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      })
      
      recTable <- ldply(res, data.frame)
      recTable <- recTable[!duplicated(recTable), ]
    } 
    else 
    {
      recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                              epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                              encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      
    }
    
  } else if(!is.null(study))
  {
    
    recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                            epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                            encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
    
  } else 
  {
    stop("Parameters 'query', 'study' and 'file' are NULL.")
  }
  #ifelse(!is.null(recTable), as_tibble(recTable), NULL)
  #library(tibble);
  if(is.null(recTable)){
  return(recTable);
  }else{
  return(as.data.frame(recTable));
  }
}

simpleQuery <- function(query=NULL, file=NULL,
                        study=NULL,
                        ldThresh=0.8, 
                        ldPop="EUR", 
                        epi="vanilla", 
                        cons="siphy", 
                        genetypes="gencode",
                        url=Haploreg.settings[["base.url"]],
                        timeout=100,
                        encoding="UTF-8",
                        querySNP=FALSE,
                        fields=NULL,
                        verbose=FALSE)
{
  trunc <- 1000 # can be 0 2 3 4 5 1000
  oligo <- 1000 # can be 1, 6, 1000
  output <- "text"
  
  if(!is.null(study)) {
    if(class(study) == "list") {
      gwas_idx <- study$id
    } else {
      stop("Parameter study is not a list with 
           study id and study name.")
    }
  } else {
    gwas_idx <- "0"
  }
  
  
  if(!is.null(file)) {
    query <- upload_file(file)
    body <- list(upload_file=query, 
                 gwas_idx=gwas_idx,
                 ldThresh=ifelse(!is.na(ldThresh), as.character(ldThresh), "1.1"), 
                 ldPop=ldPop, epi=epi, 
                 cons=cons, 
                 genetypes=genetypes,
                 trunc=as.character(trunc),
                 oligo=as.character(oligo),
                 output=output)
    
  } else {
    query <- paste(query, collapse = ',') 
    body <- list(query=query, 
                 gwas_idx=gwas_idx,
                 ldThresh=ifelse(!is.na(ldThresh), as.character(ldThresh), "1.1"), 
                 ldPop=ldPop, epi=epi, 
                 cons=cons, 
                 genetypes=genetypes,
                 trunc=as.character(trunc),
                 oligo=as.character(oligo),
                 output=output)
    
  }
  
  
  res.table <- data.frame()
  # Form encoded: multipart encoded
  r <- getHaploregData(url, body, timeout)

  if(is.null(r)) {
    return(r)
  }
  
  dat <- content(r, "text", encoding=encoding)
  sp <- strsplit(dat, '\n')
  res.table <- data.frame(matrix(nrow=length(sp[[1]])-1, ncol=length(strsplit(sp[[1]][1], '\t')[[1]])), stringsAsFactors = FALSE)
  colnames(res.table) <- strsplit(sp[[1]][1], '\t')[[1]]
  
  for(i in 2:length(sp[[1]])) {
    res.table[i-1,] <- strsplit(sp[[1]][i], '\t')[[1]]
  }
  
  #Convert numeric-like columns to actual numeric #
  for(i in 1:dim(res.table)[2]) {
    col.num.conv <- suppressWarnings(as.numeric(res.table[,i]))
    #na.rate <- length(which(is.na(col.num.conv)))/length(col.num.conv)
    if(!any(is.na(col.num.conv))) {
      res.table[,i] <- col.num.conv
    }
  }
  
  
  if(querySNP) {
    res.table <- res.table[which(res.table$is_query_snp == 1), ]
  }
  
  if(!is.null(fields)) {
    res.table <- res.table[, fields]
  }
  
  # Removing blank rows:
  res.table <- res.table[, colSums(is.na(res.table)) <= 1] 
  # Adding additional columns: 
  user.agent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"
  body$output <- "html"
  library(httr)
  request.data <- tryCatch(
    {
      POST(url=url, body = body, encode="multipart",  timeout(timeout),user_agent(user.agent))
    },
    error=function(e) {
      library(RCurl)
      if(url.exists(url)) {
        message(paste("URL does not seem to exist:", url))
      }
      message("Here's the original error message:")
      message(e$message)
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(e) {
      message(e$message)
      # Choose a return value in case of warning
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      return(NULL)
    }
  )
  # this is the original, does not catch error
  #request.data <- POST(url=url, body=body, encode="multipart",  timeout(timeout), user_agent(user.agent))
  html.content <- content(request.data, useInternalNodes=TRUE, encoding="ISO-8859-1",as="text")
  library(XML);
  tmp.tables <- readHTMLTable(html.content)
  html.table <- NULL
  for(i in 4:length(tmp.tables)) {
    tmp.table <- tmp.tables[[i]]
    if(is.null(tmp.table))
    {
      next
    }
    
    n.col <- dim(tmp.table)[2]
    n.row <- dim(tmp.table)[1]
    
    if(n.col <= 6) 
    {
      next
    } 
    
    if(n.col < 23) {
      while(n.col < 23) {
        tmp.col <- data.frame(replicate(n.row, ""), stringsAsFactors = FALSE)
        colnames(tmp.col) <- paste("V",n.col+1, sep="")
        tmp.table <- cbind(tmp.table, tmp.col)
        n.col <- dim(tmp.table)[2]
      }
    }
    
    if(is.null(html.table)) {
      html.table <- tmp.table
    } else {   
      colnames(html.table) <- colnames(tmp.table);
      html.table <- rbind(html.table, tmp.table);
    }
  }
  
  if(!is.null(html.table)) {
    tmp.table <- data.frame(html.table[, c(5,13:14)], stringsAsFactors = FALSE)
    tmp.table <- tmp.table[!duplicated(tmp.table), ]
    if("variant" %in% colnames(tmp.table)) {
      data.merged <- merge(res.table, tmp.table, by.x="rsID", by.y="variant")
    } else {
      data.merged <- merge(res.table, tmp.table, by.x="rsID", by.y="V5")
    }
    
    data.merged1 <- cbind(data.merged[["chr"]],
                          data.merged[["pos_hg38"]],
                          data.merged[["r2"]],
                          data.merged[["D'"]],
                          data.merged[["is_query_snp"]],
                          data.merged[["rsID"]],
                          data.merged[["ref"]],
                          data.merged[["alt"]],
                          data.merged[["AFR"]],
                          data.merged[["AMR"]],
                          data.merged[["ASN"]],
                          data.merged[["EUR"]],
                          data.merged[["GERP_cons"]],
                          data.merged[["SiPhy_cons"]],
                          data.merged[["Chromatin_States"]],
                          data.merged[["Chromatin_States_Imputed"]],
                          data.merged[["Chromatin_Marks"]],
                          data.merged[["DNAse"]],
                          data.merged[["Proteins"]],
                          data.merged[["eQTL"]],
                          data.merged[["gwas"]],
                          data.merged[["grasp"]],
                          data.merged[["Motifs"]],
                          data.merged[["GENCODE_id"]],
                          data.merged[["GENCODE_name"]],
                          data.merged[["GENCODE_direction"]],
                          data.merged[["GENCODE_distance"]],
                          data.merged[["RefSeq_id"]],
                          data.merged[["RefSeq_name"]],
                          data.merged[["RefSeq_direction"]],
                          data.merged[["RefSeq_distance"]],
                          data.merged[["dbSNP_functional_annotation"]],
                          data.merged[["query_snp_rsid"]])
    data.merged <- data.frame(data.merged1, data.merged[,34:35], stringsAsFactors = FALSE)
    #data.merged <- cbind(data.merged1, data.merged[,34:35])
    
    colnames(data.merged) <- c("chr", "pos_hg38", "r2", "D'", "is_query_snp", 
                               "rsID", "ref", "alt", "AFR", "AMR", 
                               "ASN", "EUR", "GERP_cons", "SiPhy_cons", 
                               "Chromatin_States",
                               "Chromatin_States_Imputed", "Chromatin_Marks", 
                               "DNAse", "Proteins", "eQTL",
                               "gwas", "grasp", "Motifs", "GENCODE_id", 
                               "GENCODE_name",
                               "GENCODE_direction", "GENCODE_distance", "RefSeq_id", 
                               "RefSeq_name", "RefSeq_direction",
                               "RefSeq_distance", "dbSNP_functional_annotation", 
                               "query_snp_rsid", "Promoter_histone_marks", 
                               "Enhancer_histone_marks")
    
  }
  
  # Make important columns to be numeric
  data.merged[["chr"]] <- as.num(data.merged[["chr"]])
  data.merged[["r2"]] <- as.num(data.merged[["r2"]])
  data.merged[["D'"]] <- as.num(data.merged[["D'"]])
  data.merged[["is_query_snp"]] <- as.num(data.merged[["is_query_snp"]])
  data.merged[["AFR"]] <- as.num(data.merged[["AFR"]])
  data.merged[["AMR"]] <- as.num(data.merged[["AMR"]])
  data.merged[["ASN"]] <- as.num(data.merged[["ASN"]])
  data.merged[["EUR"]] <- as.num(data.merged[["EUR"]])
  
  
  return(data.merged)
}

getHaploregData <- function(url, body, timeout) {
  # Form encoded: multipart encoded
  library(httr)
  r <- tryCatch(
    {
      POST(url=url, body = body, encode="multipart",  timeout(timeout))
    },
    error=function(e) {
      library(RCurl)
      if(url.exists(url)) {
        message(paste("URL does not seem to exist:", url))
      }
      message("Here's the original error message:")
      message(e$message)
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";

      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(e) {
      message(e$message)
      # Choose a return value in case of warning
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      return(NULL)
    }
  )
  return(r)
}

as.num <- function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

QueryPpiSQLiteZero <- function(table.nm, q.vec, requireExp, min.score){
  #table.nm<<-table.nm;
  #q.vec<<-q.vec;
  #requireExp<<-requireExp;
  #min.score<<-min.score;
  #save.image("QueryPpiSQLiteZero.RData")
  require('RSQLite')
  db.path <- paste(url.pre, "ppi.sqlite", sep="");
  if(.on.public.web){
    ppi.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name);
    }
    ppi.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste(shQuote(q.vec),collapse=",");
  
  if(grepl("string$", table.nm)){
    if(requireExp){
      statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))  AND combined_score >=", min.score, " AND experimental > 0", sep="");
    }else{
      statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))  AND combined_score >=", min.score, sep="");
    }
  }else{
    statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))", sep="");
  }
  
  ppi.res <- .query.sqlite(ppi.db, statement);
  hit.inx1 <- ppi.res[,1] %in% q.vec
  # hit.inx2 <- ppi.res[,2] %in% q.vec
  ppi.res1 <- ppi.res[(hit.inx1),]
  # 
  hit.inx3 <- ppi.res[,3] %in% q.vec
  # hit.inx4 <- ppi.res[,4] %in% q.vec
  ppi.res2 <- ppi.res[(hit.inx3),]
  ppi.res = rbind(ppi.res1,ppi.res2)
  
  return(ppi.res);
}
  
getApiResult <- function(url="NA", init=TRUE){
  json_data = tryCatch({
    rjson::fromJSON(file=url);
  }, warning = function(w) {
    print(w);
    if(init){
      print("API call failed, calling again!");
      Sys.sleep(5);
      getApiResult(url, F);
    }else{
          current.msg <<- "PhenoScanner API call failed, please try again later or try another option.";
      return(0);
    }
  }, error = function(e) {
    if(init){
      print(e);
      print("API call failed, calling again2!");
      Sys.sleep(5);
      getApiResult(url, F);
    }else{
          current.msg <<- "PhenoScanner API call failed, please try again later or try another option.";
      return(0);
    }
  }, finally = {
    
  })
  return(json_data)
}

GetCompleteMetList <- function(){
  #save.image("GetCompleteMetList.RData");
  # get pre-saved metabolite name with beta and se
  met.nms <- .get.my.lib("mr_met_nms.qs");
  return(met.nms);
}

GetCompletePheMRMetDisList <- function(){
  #save.image("GetCompletePheMRMetDisList.RData");
  met.nms <- .get.my.lib("phemr_browse_met_nms.qs");
  dis.nms <- .get.my.lib("phemr_browse_dis_nms.qs")
  return(c(met.nms, dis.nms));
}

GetCompleteDisList <- function(){
  # get disease from ieugwasr
  #save.image("GetCompleteDisList.RData")
  ieugwas.db <- .get.my.lib("ieugwas_202210.qs");
  return(sort(paste(ieugwas.db$trait, ieugwas.db$id, sep = " | ")));

}

.parse_snp2met_exposure <- function(res){
  res <- res[complete.cases(res[ , 10]),]; # beta
  res <- res[complete.cases(res[ , 19]),]; # se
  res <- res[res$beta != 0,];
  res <- res[res$se != 0,];
  res <- res[res$beta != "NA",];
  res <- res[res$se != "NA",];
  res <- res[order(res$p_value),];
  res$p_value <- format(res$p_value, digits = 4);
  return(res);
}

SetEqtlTissue <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$eqtlTissue <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("eQTL mapping tissue options were entered!"))
  }
}

Query.eQTLDB <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  # db.path <<- db.path;
  # q.vec<<-q.vec;
  # table.nm<<-table.nm;
  # col.nm<<-col.nm;
  # save.image("Query.eQTLDB.RData")
  
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  statement <- paste("SELECT * FROM ", table.nm, 
                     " LEFT JOIN snp_conv 
                     ON snp_conv.variant_id=", table.nm, ".variant_id",
                     " WHERE ", col.nm," IN (", query, ")", sep="");
  
  
  mir.dic <- .query.sqlite(mir.db, statement);
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

Query.pQTLDB <- function(db.path, q.mat){
  require('RSQLite');
  #db.path<<-db.path;
  #q.mat<<-q.mat;
  #save.image("Query.pQTLDB.RData")
  
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      #print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.mat[,"chr_hg19"]),collapse=",");
  statement <- paste("SELECT * FROM pqtl WHERE chr_hg19 IN (", query, ")", sep="");
  mir.dic <- .query.sqlite(mir.db, statement);
  mir.dic <- unique(merge(mir.dic, as.data.frame(q.mat), by = "chr_hg19"))
  
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

.map2chrposhg19 <- function(snp.vec){
  #snp.vec<<-snp.vec;
  #save.image("map2chrposhg19.RData")
  snp.res.chr <- myvariant::getVariants(snp.vec, scopes="dbsnp.rsid", fields="dbsnp.chrom");
  snp.res.hg19.start <- myvariant::getVariants(snp.vec, scopes="dbsnp.rsid", fields="dbsnp.hg19.start");
  chr_hg19 <- paste(snp.res.chr@listData[["dbsnp.chrom"]], snp.res.hg19.start@listData[["dbsnp.hg19.start"]], sep = ":")
  rsid <- snp.res.chr@listData[["query"]];
  snp.res.mat <- unique(cbind(rsid, chr_hg19));
  return(snp.res.mat);
}

query_epigraphdb <- function(route, params = NULL,
                             mode = c("raw", "table"),
                             method = c("GET", "POST"),
                             retry_times = 3,
                             retry_pause_min = 1) {
  #print("query_epigraphdb====")
  # route<<-route;
  # params<<-params;
  # print(mode);
  # print(method);
  # retry_times<<-retry_times;
  # retry_pause_min<<-retry_pause_min;
  # save.image("query_epigraphdb.RData")
  mode <- match.arg(mode)
  method <- match.arg(method)
  if (method == "GET") {
    method_func <- api_get_request
  } else if (method == "POST") {
    method_func <- api_post_request
  }
  res <- api_request(
    route = route, params = params, mode = mode, method = method_func,
    retry_times = retry_times, retry_pause_min = retry_pause_min
  )
  res
}

api_get_request <- function(route, params,
                            retry_times, retry_pause_min) {
  #print("api_get_request====")
  #route<<-route;
  #params<<-params;
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("api_get_request.RData")
  api_url <- "https://api.epigraphdb.org";
  url <- glue::glue("{api_url}{route}")
  library(magrittr) # for pipe operation %>% 
  epigraphdb.ci = Sys.getenv(x = "CI", unset = c(CI = "false")) %>%
    as.logical()
  is_ci <- getOption("epigraphdb.ci") %>%
    as.character() %>%
    tolower()
  config <- httr::add_headers(.headers = c("client-type" = "R", "ci" = is_ci))
  response <- httr::RETRY(
    "GET",
    url = url, query = params, config = config,
    times = retry_times, pause_min = retry_pause_min
  )
  stop_for_status(response = response, context = list(params = params, url = url))
  response
}

api_post_request <- function(route, params,
                             retry_times, retry_pause_min) {
  #route<<-route;
  #params<<-params;
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("api_post_request.RData")
  #api_url <- "https://api.epigraphdb.org";
  api_url <- "https://melodi-presto.mrcieu.ac.uk/api"
  url <- glue::glue("{api_url}{route}");
  library(magrittr) # for pipe operation %>% 
  # Are the requests for CI usage
  epigraphdb.ci = Sys.getenv(x = "CI", unset = c(CI = "false")) %>%
    as.logical()
  is_ci <- getOption("epigraphdb.ci") %>%
    as.character() %>%
    tolower()
  config <- httr::add_headers(.headers = c("client-type" = "R", "ci" = is_ci))
  # body <- jsonlite::toJSON(params, auto_unbox = TRUE) # this is for epigraphdb query
  body <- jsonlite::toJSON(params);
  response <- httr::RETRY(
    "POST",
    url = url, body = body, config = config,
    times = retry_times, pause_min = retry_pause_min
  )
  stop_for_status(response, context = list(params = params, url = url))
  response
}

api_request <- function(route, params,
                        mode = c("table", "raw"),
                        method = method,
                        retry_times, retry_pause_min) {
  #print("api_request====")  
  # route<<-route;
  # params<<-params;
  # print(mode)
  # print(method);
  # retry_times<<-retry_times;
  # retry_pause_min<<-retry_pause_min;
  # save.image("api_request.RData")
  mode <- match.arg(mode)
  response <- do.call(method, args = list(
    route = route, params = params,
    retry_times = retry_times, retry_pause_min = retry_pause_min
  ))
  if (mode == "table") {
    return(flatten_response(response))
  }
  library(magrittr) # for pipe operation %>% 
  response %>% httr::content(as = "parsed", encoding = "utf-8") 
}

stop_for_status <- function(response, context) {
  #print("stop_for_status====")
  
  if (httr::status_code(response) < 300) {
    return(invisible(response))
  }
  
  stop(httr::http_condition(response, context))
}

flatten_response <- function(response, field = "results") {
  library(magrittr) # for pipe operation %>% 
  response %>%
    httr::content(as = "text", encoding = "utf-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    purrr::pluck(field) %>%
    tibble::as_tibble()
}

clump_data_local_ld <- function (dat, clump_kb = 10000, clump_r2 = 0.001, clump_p1 = 1, 
                                 clump_p2 = 1, pop = "EUR") 
{
  pval_column <- "pval.exposure"
  if (!is.data.frame(dat)) {
    stop("Expecting data frame returned from format_data")
  }
  if ("pval.exposure" %in% names(dat) & "pval.outcome" %in% 
      names(dat)) {
    message("pval.exposure and pval.outcome columns present. Using pval.exposure for clumping.")
  } else if (!"pval.exposure" %in% names(dat) & "pval.outcome" %in% 
           names(dat)) {
    message("pval.exposure column not present, using pval.outcome column for clumping.")
    pval_column <- "pval.outcome"
  } else if (!"pval.exposure" %in% names(dat)) {
    message("pval.exposure not present, setting clumping p-value to 0.99 for all variants")
    dat$pval.exposure <- 0.99
  } else {
    pval_column <- "pval.exposure"
  }
  if (!"id.exposure" %in% names(dat)) {
    dat$id.exposure <- random_string(1)
  }
  d <- data.frame(rsid = dat$SNP, pval = dat[[pval_column]], 
                  id = dat$id.exposure)
  ########## use a local LD reference panel, faster
  out <- ld_clump_custom(d, clump_kb = clump_kb, clump_r2 = clump_r2, 
                            clump_p = clump_p1, pop = pop,
                            plink_bin = paste0(plink.path, "plink"),
                            bfile = paste0(plink.path, pop)
                            )
  keep <- paste(dat$SNP, dat$id.exposure) %in% paste(out$rsid, 
                                                     out$id)
  return(dat[keep, ])
}

Query.PheMRDB <- function(db.path, q.vec, col.nm, table.nm){
  require('RSQLite');
  #db.path<<-db.path;
  #q.vec<<-q.vec;
  #col.nm<<-col.nm;
  #table.nm<<-table.nm
  #save.image("Query.PheMRDB.RData")
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(table.nm == "mr_results" | table.nm == "mr_single" ){
    statement <- paste("SELECT * FROM ", table.nm,"  
                     WHERE ", table.nm,".", col.nm," IN (", query, ")", sep="");
  }else if(table.nm == "mr_sensitivity"){
    statement <- paste("SELECT mr_direct.exposure,  mr_direct.outcome_nm, mr_direct.correct_causal_direction, mr_direct.steiger_pval, 
    mr_hetero.method, mr_hetero.Q_pval, mr_pleio.pval
    FROM mr_direct
    LEFT JOIN mr_hetero
    ON (mr_direct.exposure || mr_direct.outcome) = (mr_hetero.exposure || mr_hetero.outcome)
    LEFT JOIN mr_pleio
    ON (mr_direct.exposure || mr_direct.outcome) = (mr_pleio.exposure || mr_pleio.outcome)
    WHERE mr_direct.", col.nm," IN (", query, ")", sep="")
  }
  mir.dic <- .query.sqlite(mir.db, statement);
  return(mir.dic);
  
}

QueryMyVariant <- function(query, scope){
  # query<<-query;
  # scope<<-scope;
  # save.image("QueryMyVariant.RData")
  res <- myvariant::getVariants(query, scopes="dbsnp.rsid", fields="all");
  rsid <- res@listData[["query"]];
  chr <- res@listData[["chrom"]];
  position_hg19 <- res@listData[["dbsnp.hg19.start"]];
  ref <- res@listData[["dbsnp.ref"]];
  alt <- res@listData[["dbsnp.alt"]];
  consequence <- res@listData[["snpeff.ann.effect"]];
  if(is.null(consequence)){
    snpeff.ann <- res@listData[["snpeff.ann"]];
    consequence <- sapply(snpeff.ann, function(x) {
      if (is.data.frame(x)) {
        if (length(x$effect) > 0) {
          return(x$effect[1])
        } else {
          return(NA)  # return NA if x$effect has length 0
        }
      } else {
        if (length(x$effect) > 0) {
          return(x$effect)
        } else {
          return(NA)  # return NA if x$effect has length 0
        }
      }
    })
  }
  symbol <- res@listData[["snpeff.ann.gene_id"]]
  if(is.null(symbol)){
    symbol <- sapply(snpeff.ann, function(x) {
      if (is.data.frame(x)) {
        if (length(x$gene_id) > 0) {
          return(x$gene_id[1])
        } else {
          return(NA)  # return NA if x$gene_id has length 0
        }
      } else {
        if (length(x$gene_id) > 0) {
          return(x$gene_id)
        } else {
          return(NA)  # return NA if x$gene_id has length 0
        }
      }
    })
  }
  mygene.res <- mygene::queryMany(symbol, scopes = "symbol", fields = "entrezgene", species = "human");
  entrez <- mygene.res$entrezgene
  output.df <- data.frame(
    query_snp_rsid = rsid,
    chr = chr,
    position_hg19 = position_hg19,
    ref = ref,
    alt = alt,
    consequence = unlist(consequence),
    symbol = unlist(symbol),
    entrez = entrez,
    stringsAsFactors = FALSE
  )
  library(dplyr);
  # only keep the first row when there are multiple alternative alleles
  output.df <- output.df %>%
        distinct(query_snp_rsid, .keep_all = TRUE)
  return(output.df);

}

QueryLDlink <- function(){
  library(LDlinkR)
  # Your token is: 9a329febb2bf
  # this is an example
  LDhap(snps = c("rs3", "rs4", "rs148890987"), 
        pop = "EUR", 
        token = "9a329febb2bf",
        genome_build = "grch38")
  my_proxies <- LDproxy(snp = "rs657152", 
                        pop = "EUR", 
                        r2d = "r2", 
                        token = "9a329febb2bf",
                        genome_build = "grch38"
  )
  
}

# code adapted from gwasvcf R package
get_ld_proxies <- function(rsid, bfile, searchspace=NULL, tag_kb=5000, tag_nsnp=5000, tag_r2=0.6, threads=1, out=paste0(getwd(),"/"))
{
  stopifnot(gwasvcf::check_plink())
  searchspacename <- paste0(out, ".searchspace")
  targetsname <- paste0(out, ".targets")
  outname <- paste0(out, ".targets.ld.gz")
  utils::write.table(rsid, file=targetsname, row.names = FALSE, col.names = FALSE, quote = FALSE)
  if(!is.null(searchspace))
  {
    stopifnot(is.character(searchspace))
    
    utils::write.table(unique(c(rsid, searchspace)), file=searchspacename, row.names = FALSE, col.names = FALSE, quote = FALSE)
    extract_param <- paste0(" --extract ", searchspacename)
  } else {
    extract_param <- " " 
  }
  cmd <- paste0(options()[["tools_plink"]],
                " --bfile ", bfile, 
                extract_param,
                " --keep-allele-order ",
                " --r in-phase with-freqs gz",
                " --ld-snp-list ", targetsname,
                " --ld-window-kb ", tag_kb,
                " --ld-window-r2 ", tag_r2,
                " --ld-window ", tag_nsnp,
                " --out ", targetsname,
                " --threads ", threads,
                " 2>&1 > /dev/null"
  )

  print("Finding proxies...")
  system(cmd)
  
  if (Sys.info()["sysname"] == "Windows") {
    stop("Currently, this function only works on macOS and Linux")
  }
  if (!file.exists(outname)) {
    ld <- data.frame(CHR_A = integer(), BP_A = integer(), SNP_A = character(), MAF_A = double(), CHR_B = integer(), BP_B = integer(), 
                     SNP_B = character(), PHASE = character(), MAF_B = double(), R = double())
    print("Index SNP not found in the reference panel")
    return(ld)
  }

  library(magrittr);
  system(paste0("gunzip -c ", outname, " > out.txt"));
  ld <- data.table::fread("out.txt", header=TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data[["R"]]^2 > tag_r2) %>%
    dplyr::filter(.data[["SNP_A"]] != .data[["SNP_B"]]) %>%
    dplyr::mutate(PHASE=gsub("/", "", .data[["PHASE"]])) %>%
    dplyr::filter(nchar(.data[["PHASE"]]) == 4)

  unlink(searchspacename)
  unlink(targetsname)
  unlink(paste0(targetsname, c(".log", ".nosex")))
  unlink(outname)
  if(nrow(ld) == 0)
  {
    print("No proxies found")
    return(ld)
  }

  temp <- do.call(rbind, strsplit(ld[["PHASE"]], "")) %>% dplyr::as_tibble(.data, .name_repair="minimal")
  names(temp) <- c("A1", "B1", "A2", "B2")
  ld <- cbind(ld, temp) %>% 
    dplyr::as_tibble( .name_repair="minimal")
  # ld <- dplyr::arrange(ld, desc(abs(R))) %>%
  # 	dplyr::filter(!duplicated(SNP_A))
  ld <- dplyr::arrange(ld, dplyr::desc(abs(.data[["R"]])))
  print(paste0("Found ", nrow(ld), " proxies"))

  return(ld)
}

ld_clump_custom <- function (dat = NULL, clump_kb = 10000, clump_r2 = 0.001, clump_p = 0.99, 
          pop = "EUR", access_token = NULL, bfile = NULL, plink_bin = NULL) 
{
  stopifnot("rsid" %in% names(dat))
  stopifnot(is.data.frame(dat))
  if (is.null(bfile)) {
    message("Please look at vignettes for options on running this locally if you need to run many instances of this command.")
  }
  if (!"pval" %in% names(dat)) {
    if ("p" %in% names(dat)) {
      warning("No 'pval' column found in dat object. Using 'p' column.")
      dat[["pval"]] <- dat[["p"]]
    }
    else {
      warning("No 'pval' column found in dat object. Setting p-values for all SNPs to clump_p parameter.")
      dat[["pval"]] <- clump_p
    }
  }
  if (!"id" %in% names(dat)) {
    dat$id <- random_string(1)
  }
  if (is.null(bfile)) {
    access_token = check_access_token()
  }
  ids <- unique(dat[["id"]])
  res <- list()
  for (i in 1:length(ids)) {
    x <- subset(dat, dat[["id"]] == ids[i])
    if (nrow(x) == 1) {
      message("Only one SNP for ", ids[i])
      res[[i]] <- x
    }
    else {
      message("Clumping ", ids[i], ", ", nrow(x), " variants, using ", 
              pop, " population reference")
      if (is.null(bfile)) {
        res[[i]] <- ld_clump_api(x, clump_kb = clump_kb, 
                                 clump_r2 = clump_r2, clump_p = clump_p, pop = pop, 
                                 access_token = access_token)
      }
      else {
        res[[i]] <- ld_clump_local_custom(x, clump_kb = clump_kb, 
                                   clump_r2 = clump_r2, clump_p = clump_p, bfile = bfile, 
                                   plink_bin = plink_bin)
      }
    }
  }
  res <- dplyr::bind_rows(res)
  return(res)
}

ld_clump_local_custom <- function (dat, clump_kb, clump_r2, clump_p, bfile, plink_bin) 
{
  shell <- ifelse(Sys.info()["sysname"] == "Windows", "cmd", 
                  "sh")
  fn <- "clump_local_intermediate"
  write.table(data.frame(SNP = dat[["rsid"]], P = dat[["pval"]]), 
              file = fn, row.names = F, col.names = T, quote = F)
  fun2 <- paste0(shQuote(plink_bin, type = shell), " --bfile ", 
                 shQuote(bfile, type = shell), " --clump ", shQuote(fn, 
                                                                    type = shell), " --clump-p1 ", clump_p, " --clump-r2 ", 
                 clump_r2, " --clump-kb ", clump_kb, " --out ", shQuote(fn, 
                                                                        type = shell))
  system(fun2)
  res <- read.table(paste(fn, ".clumped", sep = ""), header = T)
  unlink(paste(fn, "*", sep = ""))
  y <- subset(dat, !dat[["rsid"]] %in% res[["SNP"]])
  if (nrow(y) > 0) {
    message("Removing ", length(y[["rsid"]]), " of ", nrow(dat), 
            " variants due to LD with other variants or absence from LD reference panel")
  }
  return(subset(dat, dat[["rsid"]] %in% res[["SNP"]]))
}