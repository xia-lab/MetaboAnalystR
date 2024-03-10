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


QueryExposure <- function(mSetObj=NA, itemsStr){

    library(dplyr)
    library(tidyr)
    .init.multilist();

    mSetObj <- .get.mSet(mSetObj);
    itemVec <- strsplit(itemsStr, split = ", ")[[1]]

    tableName <- "exposure";
    idType <- "name";
    mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), itemVec, tableName, idType, "all", "all");
    hit.num <- nrow(mir.dic);
    if (hit.num == 0) {
        current.msg <<- "No hits found in the database. Please make sure to select a metabolite from the drop-down list.";
        print(current.msg);
        return(0);
    } 

    res <- mir.dic[ , c("metabolite_orig","hmdb","kegg","snp_orig", "chr", "pos_hg19","note", "name","ratio_single","beta","p_value","metabolite_id","ea","nea","pmid",
                      "most_severe_consequence", "eaf","link","se", "pop_code", "biofluid")];
    res <- .parse_snp2met_exposure(res); # remove NA
    # update col names
    colnames(res) <- c("Metabolite","HMDB","KEGG","SNP", "Chr", "BP","Note","Common Name", "Single or Ratio","Beta", "P-value", "MetID", "A1", "A2", "PMID",
                     "Consequence", "EAF","URL", "SE", "pop_code", "biofluid");
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

    if(.on.public.web){
        return(.set.mSet(mSetObj));
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

    ieugwas.db <- .get.my.lib("ieugwas_202210.qs");
    ieugwas.res <- ieugwas.db[ieugwas.db$id == itemVec.id,];
    hit.num <- nrow(ieugwas.res);
    if (hit.num == 0) {
        current.msg <<- "No hits found in the database. Please make sure to select an outcome from the drop-down list.";
        print(current.msg);
        return(0);
    } 

    mSetObj$dataSet$outcome <- ieugwas.res;
    fast.write.csv(ieugwas.res, file="dis_snp_restable.csv");

    if(.on.public.web){
        return(.set.mSet(mSetObj));
    }else{
        return(current.msg);
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


PrepareMgwasCSV <- function(table.nm){
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



