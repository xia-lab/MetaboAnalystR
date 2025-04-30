##################################################
## R script for mGWAS
## Description: utility functions
###################################################

Query.mGWASDB <- function(db.path, q.vec, table.nm, col.nm, biofluid="all", population="all", db.opt="kegg"){
  require('RSQLite');
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
   if(file.exists("/Users/lzy/sqlite/mgwas_202201.sqlite")){
      mir.db <- dbConnect(SQLite(), paste0("/Users/lzy/sqlite/",db.path));
   }else{
    mir.db <- dbConnect(SQLite(), db.path);
    }
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


ld_clump_custom <- function (dat = NULL, clump_kb = 10000, clump_r2 = 0.001, clump_p = 0.99, 
          pop = "EUR", access_token = NULL, bfile = NULL, plink_bin = NULL) 
{
  #stopifnot("rsid" %in% names(dat))
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

