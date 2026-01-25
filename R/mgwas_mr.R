##################################################
## R script for MR
## Description: GO/Pathway ORA 
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

PerformSnpFiltering <- function(mSetObj=NA, ldclumpOpt,ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh, harmonizeOpt, steigerOpt,opengwas_jwt_key = ""){
      mSetObj <- .get.mSet(mSetObj);
      #record
      mSetObj$dataSet$snp_filter_params <- list(
      ldclumpOpt=ldclumpOpt,
      ldProxyOpt=ldProxyOpt,
      ldProxies=ldProxies,
      ldThresh=ldThresh,
      pldSNPs=pldSNPs,
      mafThresh=mafThresh,
      harmonizeOpt=harmonizeOpt)
       res1 <- 0;

      err.vec <<- "";
      if(.on.public.web & (opengwas_jwt_key == "")){
        opengwas_jwt_key <- "" #readOpenGWASKey();
      }
      if(!exists("tableView.proc",mSetObj$dataSet)){
          mSetObj$dataSet$tableView.proc <- mSetObj$dataSet$tableView

       }
      exposure.dat <- mSetObj$dataSet$tableView;
      exposure.dat <- exposure.dat[,c("P-value", "Chr", "SE","Beta","BP","HMDB","SNP","A1","A2","EAF","Common Name", "metabolites", "genes", "gene_id", "URL", "PMID", "pop_code", "biofluid","sample")]
      colnames(exposure.dat) <- c("pval.exposure","chr.exposure","se.exposure","beta.exposure","pos.exposure","id.exposure","SNP","effect_allele.exposure","other_allele.exposure","eaf.exposure","exposure", "metabolites", "genes", "gene_id", "URL", "PMID", "pop_code", "biofluid","samplesize.exposure")
      exposure.snp <- mSetObj$dataSet$tableView.proc$SNP;
      outcome.id <- mSetObj$dataSet$outcome$id;
    if(is.na(mSetObj[["dataSet"]][["outcome"]][["sample_size"]]) & steigerOpt =="use_steiger"){
        AddErrMsg(paste0("Steiger filtering failed due to missing of outcome sample size. Please choose another outcome dataset or skip steiger filtering"))
             return(-2);
    }
    
      # do LD clumping
      if(ldclumpOpt!="no_ldclump"){
        exposure.dat <- clump_data_local_ld(exposure.dat);
        exposure.snp <- exposure.dat$SNP;
        res1 <- nrow(mSetObj$dataSet$exposure)-nrow(exposure.dat);
        AddMsg(paste0("LD clumping removed SNP#", res1));
      }else{
        AddMsg(paste0("No LD clumping performed."));
      }
      # mSetObj$dataSet$exposure.ldp <- mSetObj$dataSet$dat;

     exposure.dat$row <- rownames(exposure.dat)
      mSetObj$dataSet$exposure.ldp <- exposure.dat;
      # now obtain summary statistics for all available outcomes
      if(ldProxyOpt == "no_proxy"){
         ldProxies <- F;
         pldSNPs <- F;
         AddMsg(paste0("No LD proxy used."));
      }else{
         ldProxies <- T;
         pldSNPs <- T;
      }
      captured_messages <<- "";
      require('magrittr');
      if(ldProxies & ((ldThresh != 0.8) | (mafThresh != 0.3))){
        cat("Perform remote access... \n")
        outcome.dat <- capture_messages(TwoSampleMR::extract_outcome_data(snps=exposure.snp, outcomes = outcome.id, proxies = as.logical(ldProxies),
                                                    rsq = ldThresh, palindromes=as.numeric(as.logical(pldSNPs)), maf_threshold=mafThresh, opengwas_jwt = opengwas_jwt_key));
      } else {
        # use precomputed local database query
        outcome.dat <- extractGwasDB(snps=exposure.snp, outcomes = outcome.id, proxies = as.logical(ldProxies));
      }
      last_msg <- captured_messages[length(captured_messages)];
      #print(last_msg);
      
      if(length(grep("Server error: 502", captured_messages)) > 0 || length(grep("Failed to retrieve results from server", captured_messages))){
            AddErrMsg(paste0(last_msg));
            return(-2);   
      }
      if(is.null(outcome.dat) | nrow(outcome.dat) == 0){
            AddErrMsg(paste0("The selected combination of SNP(s) and disease outcome yielded no available data."))
            return(-2);
      }
      
   

      mSetObj$dataSet$outcome.dat <- outcome.dat;
      # do harmonization  
      dat <- TwoSampleMR::harmonise_data(mSetObj$dataSet$exposure.ldp, outcome.dat, action = as.numeric(harmonizeOpt));
      dat <- dat[!duplicated(dat$row),]
      rownames(dat) <- dat$row
      res = length(which(!dat$mr_keep))+(nrow(mSetObj$dataSet$tableView)-nrow(dat))
      nr = nrow(dat)
      if(steigerOpt=="use_steiger"){
       dat$samplesize.exposure <- sapply(dat$samplesize.exposure, function(x) eval(parse(text = x)))
       dat$samplesize.outcome <- mSetObj$dataSet$outcome$sample_size
      dat <- TwoSampleMR::steiger_filtering(dat)
       #print(dat$steiger_dir)
       dat<-dat[dat$steiger_dir,]
       dat$steiger_pval <- signif(dat$steiger_pval, digits = 4)
       res = c(res,(nr - nrow(dat)))
       }else{
         res = c(res,0)

      }
       dat$ifCheck = !grepl(", ",dat$metabolites)
       dat= dat[order(dat$ifCheck,dat$pval.exposure,decreasing = T),]
       mSetObj$dataSet$harmonized.dat <- dat;
     if(!exists("tableView.orig",   mSetObj$dataSet)){
        mSetObj$dataSet$tableView.orig <- dat
        mSetObj$dataSet$exposure.ldp.org <-  exposure.dat
       mSetObj$dataSet$outcome.dat.org <- outcome.dat
       }
      #print(rownames(dat))
      .set.mSet(mSetObj)
  
      return(res);
}

readOpenGWASKey <- function(){
    if(.on.public.web){
        if(file.exists("/home/zgy/opengwas_api_keys.csv")){
            df <- read.csv("/home/zgy/opengwas_api_keys.csv");
        }else if(file.exists("/Users/lzy/sqlite/opengwas_api_keys.csv")){
            df <- read.csv("/Users/lzy/sqlite/opengwas_api_keys.csv");
        }else{
            df <- read.csv("/home/glassfish/opengwas_api_keys.csv");
        }
        idx <- sample(1:nrow(df),1)
        cat("Using ", df$owner, "'s open gwas key...\n")
        this_key <- df$Key[1]
        expDate <- df$expDate
        diff_date <- as.Date(expDate, "%Y/%m/%d") - Sys.Date()
        diff_date <- as.integer(diff_date)
        #cat("diff_date ===> ", diff_date, "\n")
        #cat("this_key  ===> ", this_key, "\n")
        if(diff_date>0){
            return(this_key)
        } else {
            return("")
        }        
    } else {
        return("")
    }
}

extractGwasDB <- function(snps=exposure.snp, outcomes = outcome.id, proxies = as.logical(ldProxies)){
  
   cat("Processing into extractGwasDB from local \n")
   if(file.exists("/Users/lzy/sqlite/openGWAS_nonProxy.sqlite")){
        database_path <- "/Users/lzy/sqlite/openGWAS_nonProxy.sqlite";
        database_path2 <- "/Users/lzy/sqlite/openGWAS_withProxy.sqlite"
   }else if(file.exists("/Users/xialab/Dropbox/sqlite")){
        database_path <- "/Users/xialab/Dropbox/sqlite/openGWAS_nonProxy.sqlite";
        database_path2 <- "/Users/xialab/Dropbox/sqlite/openGWAS_withProxy.sqlite";
   }else{
        database_path <- "/home/glassfish/sqlite/openGWAS_nonProxy.sqlite"
        database_path2 <- "/home/glassfish/sqlite/openGWAS_withProxy.sqlite"
   }

  require("DBI")
  require("RSQLite")
  res_list <- list()
  # Generic step for gwas results
  outcome.idx <- paste0("\'", outcomes, "\'")
  con <- dbConnect(RSQLite::SQLite(), database_path)
  query_stat <- paste0("SELECT * FROM ", outcome.idx)
  res <- dbGetQuery(con, query_stat)
  meta_res <- dbGetQuery(con, "SELECT * FROM outcome_meta_table")
  dbDisconnect(con)
  
  res_dt1 <- res[res$SNP %in% snps,]
  meta_dt <- meta_res[meta_res$id.outcome == outcomes,]
  
  res_outcome_dt <- cbind(res_dt1, meta_dt)
  
  if(proxies){
    con <- dbConnect(RSQLite::SQLite(), database_path2)
    query_stat2 <- paste0("SELECT * FROM ", outcome.idx)
    res2 <- dbGetQuery(con, query_stat2)
    dbDisconnect(con)
    
    res_dt2 <- res2[res2$SNP %in% snps,]
    if(nrow(res_dt2)==0){
      return(res_outcome_dt)
    }
    res_outcome_dt2 <- res_dt2[,colnames(res_outcome_dt)];
    res_outcome_dt <- rbind(res_outcome_dt, res_outcome_dt2)
  }
  return(res_outcome_dt)
}


PerformMRAnalysis <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  dat <- mSetObj$dataSet$harmonized.dat[mSetObj$dataSet$harmonized.dat$ifCheck,]
     
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

capture_messages <- function(expr) {
  withCallingHandlers(expr, message = function(m) {
    # Append the message to the global variable
    captured_messages <<- c(captured_messages, conditionMessage(m))
    #invokeRestart("muffleMessage")
  })
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

PlotScatter <- function(mSetObj = NA, exposure, imgName, format = "png", dpi = default.dpi, width = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  mr.res <- mSetObj$dataSet$mr_results
  mr.dat <- mSetObj$dataSet$mr_dat
  
  if(format == "png"){
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }else{
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }
  if (is.na(width)) {
    w <- 10
  } else if (width == 0) {
    w <- 7
  } else {
    w <- width
  }
  h <- w*4/5;
  
  # Record img
  imageName <- imgName
  names(imageName) <- exposure
  if(is.null(mSetObj$imgSet$mr_scatter_plot)){
    mSetObj$imgSet$mr_scatter_plot <- imageName
  } else {
    mSetObj$imgSet$mr_scatter_plot <- c(mSetObj$imgSet$mr_scatter_plot, imageName)
  }  
  mSetObj$imgSet$current.img <- imgName
  
  plot <- .mr_scatterPlot(mr.res, mr.dat, exposure)
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  print(plot)
  dev.off()
  
  .set.mSet(mSetObj)
  if (.on.public.web) {
    return(1)
  }
}

.mr_scatterPlot <- function(mr_results, dat, exposure) {
  library("ggplot2")
  library("patchwork")
  
  exposure_data <- dat[dat$exposure == exposure, ]
  if (nrow(exposure_data) < 2 || sum(exposure_data$mr_keep) == 0) {
    return(NULL)
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
  
  
p <- ggplot(exposure_data, aes(x = beta.exposure, y = beta.outcome)) +
  geom_errorbar(aes(ymin = beta.outcome - se.outcome, ymax = beta.outcome + se.outcome), colour = "grey", width = 0) +
  geom_errorbarh(aes(xmin = beta.exposure - se.exposure, xmax = beta.exposure + se.exposure), colour = "grey", height = 0) +
  geom_point() +
  geom_abline(data = mrres_exposure, aes(intercept = a, slope = b, colour = method), show.legend = TRUE) +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")) +
  labs(colour = "MR Test", x = paste("SNP effect on", exposure), y = "Outcome effect") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    guides(colour = guide_legend(ncol = 1)),
    text = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),

    # --- Changes for grid and box ---
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Add a box around the plot area (the axis lines)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) # You can adjust linewidth as needed
  )

  return(p)
}

PlotForest <- function(mSetObj = NA, exposure, imgName, format = "png", dpi = default.dpi, width = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  mr.res_single <- mSetObj$dataSet$mr_res_single
  if(format == "png"){
  imgName <- paste0(imgName, "dpi", dpi, ".", format)
    }else{
  imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }
  if (is.na(width)) {
    w <- 8
  } else if (width == 0) {
    w <- 7
  } else {
    w <- width
  }
  h <- w
  
  # Record img
  imageName <- imgName
  names(imageName) <- exposure
  if(is.null(mSetObj$imgSet$mr_scatter_plot)){
    mSetObj$imgSet$mr_forest_plot <- imgName
  } else {
    mSetObj$imgSet$mr_forest_plot <- c(mSetObj$imgSet$mr_forest_plot, imageName)
  }  
  mSetObj$imgSet$current.img <- imgName
  
  plot <- .mr_forestPlot(mr.res_single, exposure)
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  print(plot)
  dev.off()
  
  .set.mSet(mSetObj)
  if (.on.public.web) {
    return(1)
  }
}

.mr_forestPlot <- function(singlesnp_results, exposure, exponentiate = FALSE) {
  library(ggplot2)
  
  singlesnp_results$up <- singlesnp_results$b + 1.96 * singlesnp_results$se
  singlesnp_results$lo <- singlesnp_results$b - 1.96 * singlesnp_results$se
  singlesnp_results$tot <- ifelse(grepl("^All -", singlesnp_results$SNP), 1, 0.01)
  
  if (exponentiate) {
    singlesnp_results$b <- exp(singlesnp_results$b)
    singlesnp_results$up <- exp(singlesnp_results$up)
    singlesnp_results$lo <- exp(singlesnp_results$lo)
  }
  
  exposure_data <- singlesnp_results[singlesnp_results$exposure == exposure, ]
  exposure_data <- exposure_data[order(exposure_data$b), ] # Order by beta.exposure
 
exposure_data$SNP <- gsub("\\(", "\n(",exposure_data$SNP)
  
p <- ggplot(exposure_data, aes(y = SNP, x = b)) +
  geom_vline(xintercept = ifelse(exponentiate, 1, 0), linetype = "dotted") +
  geom_errorbarh(aes(xmin = lo, xmax = up, size = as.factor(tot), colour = as.factor(tot)), height = 0) +
  geom_point(aes(colour = as.factor(tot))) +
  scale_colour_manual(values = c("black", "red")) +
  scale_size_manual(values = c(0.3, 1)) +
  labs(y = "", x = "MR effect size") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),

    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Add a box around the plot area (axis lines)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5), # Adjust linewidth as needed

    # Increase margins for axis labels
    axis.title.x = element_text(margin = margin(t = 15)), # Adds space above the x-axis label
    axis.title.y = element_text(margin = margin(r = 15))  # Adds space to the right of the y-axis label
  )
  
  return(p)
}

PlotLeaveOneOut <- function(mSetObj = NA, exposure, imgName, format = "png", dpi = default.dpi, width = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  mr.res_loo <- mSetObj$dataSet$mr_res_loo
  if(format == "png"){
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  } else {
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }
  if (is.na(width)) {
    w <- 8
  } else if (width == 0) {
    w <- 7
  } else {
    w <- width
  }
  h <- w
  
  # Record img
  imageName <- imgName
  names(imageName) <- exposure
  if(is.null(mSetObj$imgSet$mr_scatter_plot)){
    mSetObj$imgSet$mr_leaveoneout_plot <- imgName
  } else {
    mSetObj$imgSet$mr_leaveoneout_plot <- c(mSetObj$imgSet$mr_leaveoneout_plot, imageName)
  } 
  
  mSetObj$imgSet$current.img <- imgName
  
  plot <- .mr_looPlot(mr.res_loo, exposure)
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  print(plot)
  dev.off()
  
  .set.mSet(mSetObj)
  if (.on.public.web) {
    return(1)
  }
}

.mr_looPlot <- function(leaveoneout_results, exposure) {
  requireNamespace("ggplot2", quietly = TRUE)
  
  leaveoneout_results$up <- leaveoneout_results$b + 1.96 * leaveoneout_results$se
  leaveoneout_results$lo <- leaveoneout_results$b - 1.96 * leaveoneout_results$se
  leaveoneout_results$tot <- ifelse(leaveoneout_results$SNP == "All", 1, 0.01)
  
  exposure_data <- leaveoneout_results[leaveoneout_results$exposure == exposure, ]
  exposure_data <- exposure_data[order(exposure_data$b), ] #


p <- ggplot(exposure_data, aes(y = SNP, x = b)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_errorbarh(aes(xmin = lo, xmax = up, size = as.factor(tot), colour = as.factor(tot)), height = 0) +
  geom_point(aes(colour = as.factor(tot))) +
  scale_colour_manual(values = c("black", "red")) +
  scale_size_manual(values = c(0.3, 1)) +
  labs(y = "", x = "MR leave-one-out sensitivity analysis") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),

    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Add a box around the plot area (axis lines)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5), # Adjust linewidth as needed

    # Increase margins for axis labels
    axis.title.x = element_text(margin = margin(t = 15)), # Adds space above the x-axis label
    axis.title.y = element_text(margin = margin(r = 15))  # Adds space to the right of the y-axis label
  )
  
  return(p)

}

PlotFunnel <- function(mSetObj = NA, exposure, imgName, format = "png", dpi = default.dpi, width = NA) {
  mSetObj <- .get.mSet(mSetObj)
  
  mr.res_single <- mSetObj$dataSet$mr_res_single
  if(format == "png"){
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }else{
    imgName <- paste0(imgName, "dpi", dpi, ".", format)
  }
  if (is.na(width)) {
    w <- 7
  } else if (width == 0) {
    w <- 7
  } else {
    w <- width
  }
  h <- w
  
  # Record img
  imageName <- imgName
  names(imageName) <- exposure
  if(is.null(mSetObj$imgSet$mr_scatter_plot)){
    mSetObj$imgSet$mr_funnel_plot <- imgName
  } else {
    mSetObj$imgSet$mr_funnel_plot <- c(mSetObj$imgSet$mr_funnel_plot, imageName)
  }
  mSetObj$imgSet$current.img <- imgName
  
  plot <- .mr_funnelPlot(mr.res_single, exposure)
  Cairo::Cairo(file = imgName, unit = "in", dpi = dpi, width = w, height = h, type = format, bg = "white")
  print(plot)
  dev.off()
  
  .set.mSet(mSetObj)
  if (.on.public.web) {
    return(1)
  }
}

.mr_funnelPlot <- function(singlesnp_results, exposure) {
  library(ggplot2)
  
  # Modify the SNP column
  singlesnp_results$SNP <- gsub("All - ", "", singlesnp_results$SNP)
  am <- unique(grep("All", singlesnp_results$SNP, value = TRUE))
  am <- gsub("All - ", "", am)
  
  exposure_data <- singlesnp_results[singlesnp_results$exposure == exposure, ]
  exposure_data <- exposure_data[order(exposure_data$b), ] # Order by beta.exposure 
  
p <- ggplot(exposure_data, aes(y = 1/se, x = b)) +
  geom_point(shape = 21, size = 3, fill = "lightgrey") + # Modified geom_point
  geom_vline(data = subset(exposure_data, SNP %in% am), aes(xintercept = b, colour = SNP)) +
  scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                                 "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
                                 "#ffff99", "#b15928")) +
  labs(y = expression(1 / SE[IV]), x = expression(beta[IV]), colour = "MR Method") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    text = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),

    # --- Changes for grid, box, and points ---
    # Remove major and minor grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    # Add a box around the plot area (the axis lines)
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) # Adjust linewidth as needed
  )
  
  return(p)
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


QueryLiteratureMelodiPresto <- function(exposure, outcome) {
 mir.resu <<- data.frame();
 
  mSetObj <- .get.mSet(mSetObj);
  endpoint <- "/overlap/"
  params <- list(
    x = exposure,
    y = outcome
  )
 
  lit_df <- query_melodipresto(route = endpoint, params = params, mode = "raw", method = "POST")

  if(is.null(lit_df)){

mSetObj$dataSet$path <- data.frame();

    .set.mSet(mSetObj);
    return(0);
  }
  hit.num <- nrow(lit_df);
  if (hit.num == 0) {
    current.msg <<- "No hits found in the literarure evidence database.";
    print(current.msg);
    return(0);
  } else{
    # Q1 term    set_x
    # Q1 subject    subject_name_x
    # Q1 predicate   predicate_x
    # Q1 object    object_name_x **********
    # Q1 pval       pval_x
    # Q1 pmid       pmids_x
    # Q2 subject    subject_name_y **********
    # Q2 predicate    predicate_y
    # Q2 object    object_name_y
    # Q2 pval    pval_y
    # Q2 pmid    pmids_y
    # Q2 term    set_y
    #An overlap is taken to be cases where the object of a triple from the set of ‘x’ queries overlaps with a subject from the set of ‘y’ queries    
    res <- as.data.frame(lit_df[ , c("set_x","subject_name_x", "predicate_x", "pval_x","pmids_x" , "object_name_x", "predicate_y","object_name_y","pval_y","pmids_y","set_y")]);
    res$pval_x <- signif(res$pval_x, digits = 5);
    res$pval_y <- signif(res$pval_y, digits = 5);

    colnames(res) <- c("Exposure","Exposure_Subject","Exposure_Predicate", "Exposure_Pval","Exposure_PMIDs",  "Overlap", "Outcome_Predicate", "Outcome_Object", "Outcome_Pval", "Outcome_PMIDs","Outcome");
    fast.write.csv(res, file="mr_lit_evidence.csv", row.names=FALSE);
    res <- res[order(res$Outcome_Pval),];
    mSetObj$dataSet$mr2lit <- res; # for table display
    # 4 types of edges
    #exposure -> s1 subject
    edge1 <- data.frame(Name1=(res$Exposure), ID1=(paste(res$Exposure,"exposure", sep="_")), Name2=res$Exposure_Subject, ID2=paste(res$Exposure_Subject, "e_subject", sep="_"), Predicate=rep("", nrow(res)),  pmid=res$Exposure_PMIDs, stringsAsFactors = FALSE);
    exp.ids <<- edge1[,"ID1"];
    expsbj.ids <<- edge1[,"ID2"];
    #s2 object -> outcome
    edge2 <- data.frame(Name1=res$Outcome_Object, ID1=paste(res$Outcome_Object, "o_object", sep="_"), Name2=res$Outcome, ID2=paste(res$Outcome,"outcome",sep="_"), Predicate=rep("", nrow(res)),pmid=res$Outcome_PMIDs, stringsAsFactors = FALSE);
    outobj.ids <<- edge2[,"ID1"]
    out.ids <<- edge2[,"ID2"];
    #s1 subject - s1 predicate -> s1 object
    edge3 <- data.frame(Name1=res$Exposure_Subject, ID1=paste(res$Exposure_Subject,"e_subject", sep="_"), Name2=res$Overlap, ID2=paste(res$Overlap,"overlap",sep="_"), Predicate=res$Exposure_Predicate, pmid=res$Exposure_PMIDs, stringsAsFactors = FALSE);
    expsbj.ids <<- edge3[,"ID1"];
    overlap.ids <<- edge3[,"ID2"];
    # s2 subject - s2 predicate -> s2 object
    edge4 <- data.frame(Name1=res$Overlap, ID1=paste(res$Overlap, "overlap", sep="_"), Name2=res$Outcome_Object, ID2=paste(res$Outcome_Object, "o_object", sep="_"), Predicate=res$Outcome_Predicate,pmid=res$Outcome_PMIDs, stringsAsFactors = FALSE);
    outobj.ids <<- edge4[,"ID2"]
    edges.all <- list(mir.resu, edge1, edge2, edge3, edge4);
    #edges.all <- list(mir.resu, edge3, edge4);
    mir.resu <- do.call("rbind", edges.all);
   

my.edges <- as.data.frame(mir.resu[, c(1,3,5,6)])
colnames(my.edges)[1:4] <- c("from", "to", "predicate", "pmid")
library(igraph)
# Create the graph with edge attributes
mir.graph <- simplify(
  graph_from_data_frame(
    my.edges,
    directed = TRUE,
    vertices = NULL
  ),
  edge.attr.comb = "first"  # keep first value for duplicated edges
)

from = unique(res$Exposure)
to= unique(res$Outcome)
paths <- get.all.shortest.paths(mir.graph, from, to)$res;

if(length(paths) == 0){
mSetObj$dataSet$path <- data.frame();

    .set.mSet(mSetObj);
return(0) 
}


path_info <- lapply(paths, function(path) {
  nodes <- names(path)  # vertex names
  # Get edges along the path as pairs (i to i+1)
  edge_ids <- sapply(seq_along(nodes)[-length(nodes)], function(i) {
    get.edge.ids(mir.graph, vp = c(nodes[i], nodes[i+1]), directed = TRUE)
  })
  # Get all pmids from those edges
  pmids <- E(mir.graph)$pmid[edge_ids]
  # Collapse edge list into path string
  path_str <- paste(nodes, collapse = " → ")
  # Collapse pmids (optionally unique or sorted)
  pmid_str <- paste(unique(unlist(strsplit(pmids, " "))), collapse = ", ")
  
  data.frame(
    path = path_str,
    pmids = pmid_str,
    stringsAsFactors = FALSE
  )
})

# Combine all into one data frame
path_df <- do.call(rbind, path_info)


mSetObj$dataSet$path <- path_df

    .set.mSet(mSetObj);
    if(.on.public.web){
      return(1);
    }else{
      return(current.msg);
    }
  }
}



query_melodipresto <- function(route, params = NULL,
                             mode = c("raw", "table"),
                             method = c("GET", "POST"),
                             retry_times = 3,
                             retry_pause_min = 1) {
  #route<<-route;
  #params<<-params;
  #print(mode);
  #print(method);
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("query_melodipresto.RData")
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
if(length(res[["data"]])==0){
  return(NULL)
}

  l <- list();
  library(magrittr)
  for (i in 1:length(res$data)){
    l[[i]] <- res$data[[i]] %>% 
      tibble::as_tibble()
  }
  df <- do.call("rbind", l);
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


UpdateSNPEntries <- function(col.id, method, value, action, expnm="") {
  #save.image("updateentries.RData");
   mSetObj <- .get.mSet(mSetObj)

   tab <- mSetObj$dataSet$harmonized.dat
   nms <- rownames(tab)
 

  colm <- tab[[col.id]]
 
if(method == "contain"){
  hits <- grepl(value, colm, ignore.case = TRUE) ;
}else if(method == "match"){
  hits <- tolower(colm) %in% tolower(value) ;
}else{ # at least
  if(is.numericable(value)){
    
    col.val <- as.numeric(colm);  
    hits <- (col.val > as.numeric(value)) 
    return("NA");
  }
  
  }

if(action == "keep"){
  hits = !hits;
}else{
  hits = hits;
}
 
if(sum(hits) > 0){
   tab <- tab[!hits,]
   #row.ids <- rownames(tab);
   mSetObj$dataSet$harmonized.dat <- tab
 
   #nms<-nms[!nms %in% rownames(tab)]
   # tableView.proc <- tableView.proc[!rownames( tableView.proc) %in% nms,]
   # mSetObj$dataSet$tableView.proc <- tableView.proc

  .set.mSet(mSetObj);
   return(which(hits));
}else{
  return("NA");
}

}
 
is.numericable <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}


ResetSNPEntries  <- function(expnm="") {
  
  mSetObj <- .get.mSet(mSetObj) 
  mSetObj$dataSet$harmonized.dat <-   mSetObj$dataSet$tableView.orig
  mSetObj$dataSet$exposure.ldp <- mSetObj$dataSet$exposure.ldp.org
  mSetObj$dataSet$outcome.dat <-   mSetObj$dataSet$outcome.dat.org
  return(.set.mSet(mSetObj));
  
}

CheckSNPs <- function(){
   mSetObj <- .get.mSet(mSetObj)
   dat <- mSetObj$dataSet$harmonized.dat
   if(any(dat$ifCheck)){
    return(1)
  }else{
    return(0)
  }

}