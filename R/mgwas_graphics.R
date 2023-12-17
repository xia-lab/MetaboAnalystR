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
  h <- 8*length(unique(mr.res$id.exposure));
  
  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  .mr_scatterPlot(mr.res, mr.dat);
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
  
  combined_plot <- patchwork::wrap_plots(plot_list, ncol = 1) + patchwork::plot_layout(guides = "collect")
  print(combined_plot)
  dev.off();
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
    h <- h*length(unique(mr.res_single$id.exposure));

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  .mr_forestPlot(mr.res_single);
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
  
  p <- ggplot(singlesnp_results, aes(y=SNP, x=b)) +
    geom_vline(xintercept=ifelse(exponentiate, 1, 0), linetype="dotted") +
    geom_errorbarh(aes(xmin=lo, xmax=up, size=as.factor(tot), colour=as.factor(tot)), height=0) +
    geom_point(aes(colour=as.factor(tot))) +
    facet_wrap(exposure ~ ., scales = "free", ncol=1) +  # Faceting by exposure
    scale_colour_manual(values=c("black", "red")) +
    scale_size_manual(values=c(0.3, 1)) +
    theme_minimal() +
    theme(legend.position="none", 
          text=element_text(size=14)) +
    labs(y="", x="MR effect size")
  
  print(p)
  dev.off()
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
  h <- h*length(unique(mr.res_loo$id.exposure));

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  .mr_looPlot(mr.res_loo);
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_looPlot <- function(leaveoneout_results) {
  requireNamespace("ggplot2", quietly=TRUE)
  requireNamespace("plyr", quietly=TRUE)

  leaveoneout_results$up <- leaveoneout_results$b + 1.96 * leaveoneout_results$se
  leaveoneout_results$lo <- leaveoneout_results$b - 1.96 * leaveoneout_results$se
  leaveoneout_results$tot <- ifelse(leaveoneout_results$SNP == "All", 1, 0.01)

  p <- ggplot(leaveoneout_results, aes(y=SNP, x=b)) +
    geom_vline(xintercept=0, linetype="dotted") +
    geom_errorbarh(aes(xmin=lo, xmax=up, size=as.factor(tot), colour=as.factor(tot)), height=0) +
    geom_point(aes(colour=as.factor(tot))) +
    facet_wrap(exposure ~ ., scales = "free", ncol=1) +  # Faceting by exposure
    scale_colour_manual(values=c("black", "red")) +
    scale_size_manual(values=c(0.3, 1)) +
    theme_minimal() +
    theme(legend.position="none", 
          text=element_text(size=14)) +
    labs(y="", x="MR leave-one-out sensitivity analysis")

  print(p)
  dev.off();
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
  h <- 8*length(unique(mr.res_single$id.exposure));

  #record img
  mSetObj$imgSet$mr.scatter <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  .mr_funnelPlot(mr.res_single);
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }
}

.mr_funnelPlot <- function(singlesnp_results) {

  library(ggplot2)
  #library(plyr)

  # Modify the SNP column
  singlesnp_results$SNP <- gsub("All - ", "", singlesnp_results$SNP)
  am <- unique(grep("All", singlesnp_results$SNP, value=TRUE))
  am <- gsub("All - ", "", am)

  # Create the funnel plot
  p <- ggplot(singlesnp_results, aes(y = 1/se, x=b)) +
    geom_point() +
    geom_vline(data=subset(singlesnp_results, SNP %in% am), aes(xintercept=b, colour=SNP)) +
    scale_colour_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
                                   "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", 
                                   "#ffff99", "#b15928")) +
    facet_wrap(exposure ~ ., scales = "free", ncol=1) +  # Faceting by exposure
    labs(y=expression(1/SE[IV]), x=expression(beta[IV]), colour="MR Method") +
    theme(legend.position="right", legend.direction="vertical",text=element_text(size=14))

  print(p)
  dev.off();
}
