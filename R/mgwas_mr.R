##################################################
## R script for MR
## Description: GO/Pathway ORA 
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

CreateResTableExposure <- function(mSetObj=NA, type="ld"){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "ld"){
    exposure <- mSetObj$dataSet$exposure.ldp
  }else if(type == "harmonized"){
    exposure <- mSetObj$dataSet$harmonized.dat
  }
  ## get associated metabolites for each snp
  mir.dic <- Query.mGWASDB(paste(url.pre, "mgwas_202201", sep=""), snp.nms, "snp2met", "rsid", "all", "all");
  #print(head(mir.dic));

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
}


PerformLDClumping <- function(mSetObj=NA, ldclumpOpt){

  mSetObj <- .get.mSet(mSetObj);
  exposure.dat <- mSetObj$dataSet$exposure;
  exposure.dat <- exposure.dat[,c("P-value", "Chr", "SE","Beta","BP","HMDB","SNP","A1","A2","EAF","Common Name", "metabolites", "genes", "gene_id", "URL", "PMID", "pop_code", "biofluid")]
  colnames(exposure.dat) <- c("pval.exposure","chr.exposure","se.exposure","beta.exposure","pos.exposure","id.exposure","SNP","effect_allele.exposure","other_allele.exposure","eaf.exposure","exposure", "metabolites", "genes", "gene_id", "URL", "PMID", "pop_code", "biofluid")
  exposure.snp <- mSetObj$dataSet$exposure$SNP;

  if(ldclumpOpt!="no_ldclump"){
    exposure.dat <- clump_data_local_ld(exposure.dat);
    exposure.snp <- exposure.dat$SNP;
  }

  mSetObj$dataSet$exposure.ldp <- exposure.dat;
  .set.mSet(mSetObj)
  return(nrow(mSetObj$dataSet$exposure)-nrow(exposure.dat));
 
}

PerformLDProxies <- function(mSetObj=NA, ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh){
  mSetObj <- .get.mSet(mSetObj);
  exposure.dat <- mSetObj$dataSet$exposure.ldp;
  exposure.snp <- exposure.dat$SNP;
  outcome.id <- mSetObj$dataSet$outcome$id;

  if(ldProxyOpt == "no_proxy"){
    ldProxies <- F;
    pldSNPs <- F;
  }else{
    ldProxies <- T;
    pldSNPs <- T;
  }

  outcome.dat <- TwoSampleMR::extract_outcome_data(snps=exposure.snp, outcomes = outcome.id, proxies = as.logical(ldProxies),
                                                   rsq = as.numeric(ldThresh), palindromes=as.numeric(as.logical(pldSNPs)), maf_threshold=as.numeric(mafThresh))
  
  if(is.null(outcome.dat)){
    AddErrMsg(paste0("The selected combination of SNP(s) and disease outcome yielded no available data. The server might be busy or try different input."))
    return(-2);
  }

  mSetObj$dataSet$outcome.dat <- outcome.dat;
  .set.mSet(mSetObj)
  return(nrow(exposure.dat) - nrow(outcome.dat) + sum(!outcome.dat$mr_keep.outcome))
}

PerformHarmonization <- function(mSetObj=NA, harmonizeOpt){
  mSetObj <- .get.mSet(mSetObj);
  exposure.dat <- mSetObj$dataSet$exposure.ldp;
  outcome.dat <- mSetObj$dataSet$outcome.dat;
  
  dat <- TwoSampleMR::harmonise_data(exposure.dat, outcome.dat, action = as.numeric(harmonizeOpt));
  mSetObj$dataSet$harmonized.dat <- dat;
  return(.set.mSet(mSetObj))
}

PerformSnpFiltering <- function(mSetObj=NA, ldclumpOpt,ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh, harmonizeOpt){
      mSetObj <- .get.mSet(mSetObj);
      err.vec <<- "";
      res1 <- PerformLDClumping(mSetObj, ldclumpOpt);
      mSetObj <- .get.mSet(mSetObj);
      res2 <- PerformLDProxies(mSetObj, ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh);
      if(res2 == -2){
        #empty outcome.dat error.
        return(-2);
      }
      mSetObj <- .get.mSet(mSetObj);
      res <- PerformHarmonization(mSetObj=NA, harmonizeOpt);
      mSetObj <- .get.mSet(mSetObj);
      outcome.dat <- mSetObj$dataSet$outcome.dat;
      print(res1+res2);
      print("snpfilter");
      return(res1 + res2)
}


PerformMRAnalysis <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dat <- mSetObj$dataSet$harmonized.dat
  save.image("MR.RData");
  #4. perform mr
  method.type <- mSetObj$dataSet$methodType;
  #mr.res <- TwoSampleMR::mr(dat, method_list = method.type);
  mr.res <- mr_modified(dat, method_list = method.type);
  #rownames(mr.res) <- mr.res$method;
  #Analysing 'HMDB0000042' on 'ebi-a-GCST007799'
  # Heterogeneity tests
  mr_heterogeneity.res <- TwoSampleMR::mr_heterogeneity(dat);
  #rownames(mr_heterogeneity.res) <- mr_heterogeneity.res$method;
  fast.write.csv(mr_heterogeneity.res, file="mr_heterogeneity_results.csv", row.names=FALSE);
  #"Q"           "Q_df"        "Q_pval"
  mSetObj$dataSet$mr.hetero_mat <- round(data.matrix(mr_heterogeneity.res[6:8]),3) 
  
  # Test for directional horizontal pleiotropy
  mr_pleiotropy_test.res <- TwoSampleMR::mr_pleiotropy_test(dat);
  fast.write.csv(mr_pleiotropy_test.res, file="mr_pleiotropy_results.csv", row.names=FALSE);
  mr.hetero.num <- mr_heterogeneity.res[5:8];
  mr.res.num <- mr.res[4:9];
  mr.pleio.num <- mr_pleiotropy_test.res[5:7];
  mr.pleio.num$method <- "MR Egger";
  merge1 <- merge(mr.res.num, mr.hetero.num, by="method", all.x=TRUE);
  merge2 <- merge(merge1, mr.pleio.num, by="method", all.x=TRUE);
  #rownames(merge2) <- merge2$method;
  method.vec <- merge2$method;
  exposure.vec <- merge2$exposure;
  merge2$exposure <- NULL;
  #print(head(merge2));
  merge2 <- signif(merge2[2:11], 5);
  merge2[is.na(merge2)] <- "-";
  merge2$method <- method.vec
  merge2$exposure <- exposure.vec
  merge2 <- merge2[order(merge2$exposure, merge2$method), ]
  mSetObj$dataSet$mr_results_merge <- merge2
  mSetObj$dataSet$mr.pleio_mat <- signif(data.matrix(mr_pleiotropy_test.res[5:7]),5)
  mSetObj$dataSet$mr_results <- mr.res;
  fast.write.csv(mr.res, file="mr_results.csv", row.names=FALSE);
  mSetObj$dataSet$mr_dat <- dat;
  mSetObj$dataSet$mr.res_mat <- signif(data.matrix(mr.res[6:9]), 5) #"nsnp","b","se","pval" 

  res_single <- TwoSampleMR::mr_singlesnp(dat, all_method = method.type);
  mSetObj$dataSet$mr_res_single <- res_single;
  res_loo <- TwoSampleMR::mr_leaveoneout(dat);
  mSetObj$dataSet$mr_res_loo <- res_loo;
  #print(head(merge2))
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}


PerformMRAnalysisOld <- function(ldclumpOpt, ldProxies, ldThresh, pldSNPs, mafThresh, harmonizeOpt){
  # ldclumpOpt<<-ldclumpOpt;
  # ldProxies<<-ldProxies;
  # ldThresh<<-ldThresh;
  # pldSNPs<<-pldSNPs;
  # mafThresh<<-mafThresh;
  # harmonizeOpt<<-harmonizeOpt;
  # save.image("PerformMRAnalysis.RData")
  mSetObj <- .get.mSet(mSetObj);
  
  # get instruments
  exposure.dat <- mSetObj$dataSet$exposure;
  # missing sample size, 
  exposure.dat <- exposure.dat[,c("P-value", "Chr", "SE","Beta","BP","HMDB","SNP","A1","A2","EAF","Common Name")]
  colnames(exposure.dat) <- c("pval.exposure","chr.exposure","se.exposure","beta.exposure","pos.exposure","id.exposure","SNP","effect_allele.exposure","other_allele.exposure","eaf.exposure","exposure")
  exposure.snp <- mSetObj$dataSet$exposure$SNP;
  outcome.id <- mSetObj$dataSet$outcome$id;

  #1. LD clumping
  if(ldclumpOpt!="no_ldclump"){
    exposure.dat <- clump_data_local_ld(exposure.dat);
    exposure.snp <- exposure.dat$SNP;
  }

  #2. get effects of instruments on outcome
  outcome.dat <- TwoSampleMR::extract_outcome_data(snps=exposure.snp, outcomes = outcome.id, proxies = as.logical(ldProxies),
                                                   rsq = as.numeric(ldThresh), palindromes=as.numeric(as.logical(pldSNPs)), maf_threshold=as.numeric(mafThresh))
  
  fast.write.csv(outcome.dat, file="mr_outcome_data.csv", row.names=FALSE);

  #3. harmonise the exposure and outcome data
  dat <- TwoSampleMR::harmonise_data(exposure.dat, outcome.dat, action = as.numeric(harmonizeOpt));
  fast.write.csv(dat, file="mr_harmonized_data.csv", row.names=FALSE);


  #4. perform mr
  method.type <- mSetObj$dataSet$methodType;
  mr.res <- TwoSampleMR::mr(dat, method_list = method.type);
  #rownames(mr.res) <- mr.res$method;
  #Analysing 'HMDB0000042' on 'ebi-a-GCST007799'
  # Heterogeneity tests
  mr_heterogeneity.res <- TwoSampleMR::mr_heterogeneity(dat);
  #rownames(mr_heterogeneity.res) <- mr_heterogeneity.res$method;
  fast.write.csv(mr_heterogeneity.res, file="mr_heterogeneity_results.csv", row.names=FALSE);
  #"Q"           "Q_df"        "Q_pval"
  mSetObj$dataSet$mr.hetero_mat <- round(data.matrix(mr_heterogeneity.res[6:8]),3) 
  
  # Test for directional horizontal pleiotropy
  mr_pleiotropy_test.res <- TwoSampleMR::mr_pleiotropy_test(dat);
  fast.write.csv(mr_pleiotropy_test.res, file="mr_pleiotropy_results.csv", row.names=FALSE);
  mr.hetero.num <- mr_heterogeneity.res[5:8];
  mr.res.num <- mr.res[4:9];
  mr.pleio.num <- mr_pleiotropy_test.res[5:7];
  mr.pleio.num$method <- "MR Egger";
  merge1 <- merge(mr.res.num, mr.hetero.num, by="method", all.x=TRUE);
  merge2 <- merge(merge1, mr.pleio.num, by="method", all.x=TRUE);
  #rownames(merge2) <- merge2$method;
  method.vec <- merge2$method;
  exposure.vec <- merge2$exposure;
  merge2$exposure <- NULL;
  #print(head(merge2));
  merge2 <- signif(merge2[2:11], 5);
  merge2[is.na(merge2)] <- "-";
  merge2$method <- method.vec
  merge2$exposure <- exposure.vec
  merge2 <- merge2[order(merge2$exposure, merge2$method), ]
  mSetObj$dataSet$mr_results_merge <- merge2
  mSetObj$dataSet$mr.pleio_mat <- signif(data.matrix(mr_pleiotropy_test.res[5:7]),5)
  mSetObj$dataSet$mr_results <- mr.res;
  fast.write.csv(mr.res, file="mr_results.csv", row.names=FALSE);
  mSetObj$dataSet$mr_dat <- dat;
  mSetObj$dataSet$mr.res_mat <- signif(data.matrix(mr.res[6:9]), 5) #"nsnp","b","se","pval" 

  res_single <- TwoSampleMR::mr_singlesnp(dat, all_method = method.type);
  mSetObj$dataSet$mr_res_single <- res_single;
  res_loo <- TwoSampleMR::mr_leaveoneout(dat);
  mSetObj$dataSet$mr_res_loo <- res_loo;

  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return(current.msg);
  }
}

GetMRRes.rowNames<-function(mSetObj=NA){
  #save.image("GetMRRes.rowNames.RData")
  mSetObj <- .get.mSet(mSetObj);
  nms <- rownames(mSetObj$dataSet$mr_results_merge);
  if(is.null(nms)){
    return("NA");
  }
  return (nms);
}

GetMRRes.mat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$mr_results_merge);
}

GetHeteroRes.rowNames<-function(mSetObj=NA){
  #save.image("GetHeteroRes.rowNames.RData")
  mSetObj <- .get.mSet(mSetObj);
  nms <- rownames(mSetObj$dataSet$mr.hetero_mat);
  if(is.null(nms)){
    return("NA");
  }
  return (nms);
}

GetHeteroRes.mat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$mr.hetero_mat);
}

GetPleioRes.mat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$mr.pleio_mat);
}

GetMRMat<-function(mSetObj=NA, type){
  # type<<-type;
  # save.image("GetMRMat.RData")
  mSetObj <- .get.mSet(mSetObj);
  if(type == "single"){
    sig.mat <- mSetObj$dataSet$mr_res_single;
  }else if(type == "loo"){
    sig.mat <- mSetObj$dataSet$mr_res_loo;
  }else{
    sig.mat <- mSetObj$dataSet$mr_results;
  }
  return(CleanNumber(signif(as.matrix(sig.mat),5)));
}

GetMRMatRowNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "single"){
    return(rownames(mSetObj$dataSet$mr_res_single));
  }else if(type == "loo"){
    return(rownames(mSetObj$dataSet$mr_res_loo));
  }else{
    return(rownames(mSetObj$dataSet$mr_results))
  }
}

GetMRMatColNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "single"){
    return(colnames(mSetObj$dataSet$mr_res_single));
  }else if(type == "loo"){
    return(colnames(mSetObj$dataSet$mr_res_loo));
  }else{
    return(colnames(mSetObj$dataSet$mr_results))
  }
}

PlotScatter<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  imgName <<-imgName;
  #save.image("PlotScatter.RData")
  
  
  mSetObj <- .get.mSet(mSetObj);
  
  # get mr_results and mr_dat
  mr.res <- mSetObj$dataSet$mr_results;
  mr.dat <- mSetObj$dataSet$mr_dat;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 12;
  }else if(width == 0){
    w <- 7;
  }else{
    w <-width;
  }
  h <- 9
  
  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  plot.list <- .mr_scatterPlot(mr.res, mr.dat);
  for(exposure in names(plot.list)){
  Cairo::Cairo(file = paste0(exposure, "_", imgName), unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(plot.list[[exposure]])
  dev.off()
  }
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_scatterPlot <- function(mr_results, dat) {
  library("ggplot2",)
  library("patchwork")
  
  unique_exposures <- unique(dat$exposure)
  plot_list <- list()
  
  for (exposure in unique_exposures) {
    exposure_data <- dat[dat$exposure == exposure, ]
    if (nrow(exposure_data) < 2 || sum(exposure_data$mr_keep) == 0) {
      next
    }
    
    exposure_data <- exposure_data[exposure_data$mr_keep, ]
    exposure_data$beta.exposure.sign <- ifelse(exposure_data$beta.exposure < 0, -1, 1)
    exposure_data$beta.exposure <- abs(exposure_data$beta.exposure)
    exposure_data$beta.outcome <- exposure_data$beta.outcome * exposure_data$beta.exposure.sign
    exposure_data <- exposure_data[order(exposure_data$beta.exposure), ] # Order by beta.exposure
    mrres_exposure <- mr_results[mr_results$exposure == exposure, ]
    mrres_exposure$a <- 0
    
    # MR Egger regression
    if ("MR Egger" %in% mrres_exposure$method) {
      temp <- TwoSampleMR::mr_egger_regression(exposure_data$beta.exposure, exposure_data$beta.outcome, exposure_data$se.exposure, exposure_data$se.outcome, default_parameters())
      mrres_exposure$a[mrres_exposure$method == "MR Egger"] <- temp$b_i
    }
    
    if ("MR Egger (bootstrap)" %in% mrres_exposure$method) {
      temp <- TwoSampleMR::mr_egger_regression_bootstrap(exposure_data$beta.exposure, exposure_data$beta.outcome, exposure_data$se.exposure, exposure_data$se.outcome, default_parameters())
      mrres_exposure$a[mrres_exposure$method == "MR Egger (bootstrap)"] <- temp$b_i
    }
    
    p <- ggplot(exposure_data, aes(x=beta.exposure, y=beta.outcome)) +
      geom_errorbar(aes(ymin=beta.outcome-se.outcome, ymax=beta.outcome+se.outcome), colour="grey", width=0) +
      geom_errorbarh(aes(xmin=beta.exposure-se.exposure, xmax=beta.exposure+se.exposure), colour="grey", height=0) +
      geom_point() +
      geom_abline(data=mrres_exposure, aes(intercept=a, slope=b, colour=method), show.legend=TRUE) +
      scale_colour_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")) +
      labs(colour="MR Test", x=paste("SNP effect on", exposure), y="Outcome effect") +
      theme(legend.position="right", legend.direction="vertical") +
      guides(colour=guide_legend(ncol=2))

    plot_list[[exposure]] <- p
  }
  
  return(plot_list);
}

PlotForest<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  #imgName <<-imgName;
  #save.image("PlotForest.RData")

  mSetObj <- .get.mSet(mSetObj);
  
  # get mr_results and mr_dat
  mr.res_single <- mSetObj$dataSet$mr_res_single;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <-width;
  }
  h <- w;

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  plot.list <- .mr_forestPlot(mr.res_single);

  for(exposure in names(plot.list)){
  Cairo::Cairo(file = paste0(exposure, "_", imgName), unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(plot.list[[exposure]])
  dev.off()
  }
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_forestPlot <- function(singlesnp_results, exponentiate=FALSE) {

  library(ggplot2)
  #library(plyr)
  
  singlesnp_results$up <- singlesnp_results$b + 1.96 * singlesnp_results$se
  singlesnp_results$lo <- singlesnp_results$b - 1.96 * singlesnp_results$se
  singlesnp_results$tot <- ifelse(singlesnp_results$SNP %in% c("All - Inverse variance weighted", "All - MR Egger"), 1, 0.01)
  
  if(exponentiate) {
    singlesnp_results$b <- exp(singlesnp_results$b)
    singlesnp_results$up <- exp(singlesnp_results$up)
    singlesnp_results$lo <- exp(singlesnp_results$lo)
  }
  plot_list <- list();
for(exposure in unique(singlesnp_results$exposure)) {
    exposure_data <- singlesnp_results[singlesnp_results$exposure == exposure, ]
    exposure_data <- exposure_data[order(exposure_data$b), ] # Order by beta.exposure 
    p <- ggplot(exposure_data, aes(y=SNP, x=b)) +
      geom_vline(xintercept=ifelse(exponentiate, 1, 0), linetype="dotted") +
      geom_errorbarh(aes(xmin=lo, xmax=up, size=as.factor(tot), colour=as.factor(tot)), height=0) +
      geom_point(aes(colour=as.factor(tot))) +
      scale_colour_manual(values=c("black", "red")) +
      scale_size_manual(values=c(0.3, 1)) +
      theme_minimal() +
      theme(legend.position="none", 
            text=element_text(size=14)) +
      labs(y="", x="MR effect size")
    plot_list[[exposure]] <- p;
  }
  return(plot_list)
}

PlotLeaveOneOut<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  #imgName <<-imgName;
  #save.image("PlotLeaveOneOut.RData")
  
  mSetObj <- .get.mSet(mSetObj);
  
  mr.res_loo <- mSetObj$dataSet$mr_res_loo;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <-width;
  }
  h <- w;

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  plot.list <- .mr_looPlot(mr.res_loo);

  for(exposure in names(plot.list)){
  Cairo::Cairo(file = paste0(exposure, "_", imgName), unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(plot.list[[exposure]])
  dev.off()
  }  
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_looPlot <- function(leaveoneout_results) {
  requireNamespace("ggplot2", quietly=TRUE)

  leaveoneout_results$up <- leaveoneout_results$b + 1.96 * leaveoneout_results$se
  leaveoneout_results$lo <- leaveoneout_results$b - 1.96 * leaveoneout_results$se
  leaveoneout_results$tot <- ifelse(leaveoneout_results$SNP == "All", 1, 0.01)

  plot_list <- list();
  # Iterate through each unique exposure and create a plot
  for(exposure in unique(leaveoneout_results$exposure)) {
    exposure_data <- leaveoneout_results[leaveoneout_results$exposure == exposure, ]
    exposure_data <- exposure_data[order(exposure_data$b), ] # Order by beta.exposure 

    p <- ggplot(exposure_data, aes(y=SNP, x=b)) +
      geom_vline(xintercept=0, linetype="dotted") +
      geom_errorbarh(aes(xmin=lo, xmax=up, size=as.factor(tot), colour=as.factor(tot)), height=0) +
      geom_point(aes(colour=as.factor(tot))) +
      scale_colour_manual(values=c("black", "red")) +
      scale_size_manual(values=c(0.3, 1)) +
      theme_minimal() +
      theme(legend.position="none", 
            text=element_text(size=14)) +
      labs(y="", x="MR leave-one-out sensitivity analysis")
    plot_list[[exposure]] <- p;
  }

  return(plot_list)
}

PlotFunnel<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  #imgName <<-imgName;
  #save.image("PlotFunnel.RData")
  
  mSetObj <- .get.mSet(mSetObj);
  
  mr.res_single <- mSetObj$dataSet$mr_res_single;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 12;
  }else if(width == 0){
    w <- 7;
  }else{
    w <-width;
  }
  h <- 9

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;

  plot.list <- .mr_funnelPlot(mr.res_single);
  for(exposure in names(plot.list)){
  Cairo::Cairo(file = paste0(exposure, "_", imgName), unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  print(plot.list[[exposure]])
  dev.off()
  }    

  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_funnelPlot <- function(singlesnp_results) {
  library(ggplot2)

  # Modify the SNP column
  singlesnp_results$SNP <- gsub("All - ", "", singlesnp_results$SNP)
  am <- unique(grep("All", singlesnp_results$SNP, value=TRUE))
  am <- gsub("All - ", "", am)

  # Iterate through each unique exposure and create a plot
  plot_list <- list();
  for(exposure in unique(singlesnp_results$exposure)) {
    exposure_data <- singlesnp_results[singlesnp_results$exposure == exposure, ]
    exposure_data <- exposure_data[order(exposure_data$b), ] # Order by beta.exposure 

    p <- ggplot(exposure_data, aes(y = 1/se, x=b)) +
      geom_point() +
      geom_vline(data=subset(exposure_data, SNP %in% am), aes(xintercept=b, colour=SNP)) +
      scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
                                     "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", 
                                     "#ffff99", "#b15928")) +
      labs(y=expression(1/SE[IV]), x=expression(beta[IV]), colour="MR Method") +
      theme(legend.position="right", legend.direction="vertical", text=element_text(size=14))
    plot_list[[exposure]] <- p;
  }

  return(plot_list)

}

mr_modified <- function (dat, 
                         parameters = default_parameters(), 
                         method_list = subset(mr_method_list(), use_by_default)$obj) 
{
  library(TwoSampleMR)
  mr_raps_modified <- function (b_exp, b_out, se_exp, se_out,parameters) 
  {
    out <- try(suppressMessages(mr.raps::mr.raps(b_exp, b_out, se_exp, se_out,
                                                 over.dispersion = parameters$over.dispersion, 
                                                 loss.function = parameters$loss.function,
                                                 diagnosis = FALSE)),
               silent = T)
    
    # The estimated overdispersion parameter is very small. Consider using the simple model without overdispersion
    # When encountering such warning, change the over.dispersion as 'FASLE'
    
    if ('try-error' %in% class(out))
    {
      output = list(b = NA, se = NA, pval = NA, nsnp = NA)
    }
    else
    {
      output = list(b = out$beta.hat, se = out$beta.se, 
                    pval = pnorm(-abs(out$beta.hat/out$beta.se)) * 2, nsnp = length(b_exp))
    }
    return(output)
  }
  
  method_list_modified <- stringr::str_replace_all(method_list, "mr_raps","mr_raps_modified")
  
  mr_tab <- plyr::ddply(dat, c("id.exposure", "id.outcome"),function(x1)
  {
    x <- subset(x1, mr_keep)
    
    if (nrow(x) == 0) {
      message("No SNPs available for MR analysis of '", x1$id.exposure[1], "' on '", x1$id.outcome[1], "'")
      return(NULL)
    }
    else {
      message("Analysing '", x1$id.exposure[1], "' on '", x1$id.outcome[1], "'")
    }
    res <- lapply(method_list_modified, function(meth)
    {
      get(meth)(x$beta.exposure, x$beta.outcome, x$se.exposure, x$se.outcome, parameters)
    }
    )
    
    methl <- mr_method_list()
    mr_tab <- data.frame(outcome = x$outcome[1], exposure = x$exposure[1], 
                         method = methl$name[match(method_list, methl$obj)], 
                         nsnp = sapply(res, function(x) x$nsnp), 
                         b = sapply(res, function(x) x$b), 
                         se = sapply(res, function(x) x$se), 
                         pval = sapply(res, function(x) x$pval))
    
    mr_tab <- subset(mr_tab, !(is.na(b) & is.na(se) & is.na(pval)))
    
    return(mr_tab)
  }
  )
  return(mr_tab)
}
