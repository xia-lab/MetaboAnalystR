#'Convert mSetObj to proper format for MS Peaks to Pathways module
#'@description Following t-test analysis or effect size calculation, 
#'this functions converts the results from the mSetObj 
#'to the proper format for mummichog analysis. 
#'@param mSetObj Input the name of the created mSetObj.
#'@param rt Logical, whether or not to include retention time information.
#'@param rds.file Logical, if true, the "annotated_peaklist.rds"
#'must be in the current working directory to get corresponding retention time
#'information for the features. If not, the retention time information
#'will be taken from the feature names. Feature names must be formatted
#'so that the mz and retention time for a single peak is separated by two
#'underscores. For instance, m/z of 410.2148 and retention time of 42.46914 seconds
#'must be formatted as 410.2148__42.46914.
#'@param rt.type Character, input whether retention time is in seconds (default as RT using
#'MetaboAnalystR is seconds) or minutes (as from MZmine).
#'@param test Character, input what statistical values to include in the mummichog input. 
#'For p-values and t-scores only from t-test, use "tt".
#'For log2FC from the fold-change analsis, use "fc".
#'For effect-sizes, use "es".
#'For, p-values, fold-changes and effect sizes, use "all". 
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Convert2MummichogMetaPath <- function(mSetObj=NA, rt=FALSE, rds.file=FALSE, rt.type="seconds", 
                                      test="tt", mode=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # JX: we will only support tt on web. It is not clear if 
  # other options will be really useful or simply bewildering to users
  
  tt.pval <- sort(mSetObj$analSet$tt$p.value);
  
  if(is.null(tt.pval)){
    AddErrMsg("T-test was not performed!")
    return(0)
  }
  
  fdr <- p.adjust(tt.pval, "fdr")
  mz.pval <- names(tt.pval)
  pvals <- cbind(mz.pval, as.numeric(fdr))
  colnames(pvals) <- c("m.z", "p.value")
  
  tt.tsc <- sort(mSetObj$analSet$tt$t.score);
  mz.tsc <- names(tt.tsc)
  tscores <- cbind(mz.tsc, as.numeric(tt.tsc))
  colnames(tscores) <- c("m.z", "t.score")
  
  if(rt & rds.file){
    
    if(!file.exists("annotated_peaklist.rds")){
      AddErrMsg("annotated_peaklist.rds not found in current working directory!")
      return(0)
    }
    
    camera_output <- readRDS("annotated_peaklist.rds")
    mz.cam <- round(camera_output$mz, 5) 
    rt.cam <- round(camera_output$rt, 5) 
    camera <- cbind(mz.cam, rt.cam)
    colnames(camera) <- c("m.z", "r.t")
    
    mummi_new <- Reduce(function(x,y) merge(x,y,by="m.z", all = TRUE), list(pvals, tscores, camera))
    complete.inx <- complete.cases(mummi_new[,c("p.value", "t.score", "r.t")]) # filter out m/zs without pval and tscore
    mummi_new <- mummi_new[complete.inx,]
    
  }else{
    
    mummi_new <- merge(pvals, tscores)
    
    if(rt){ # taking retention time information from feature name itself
      
      feat_info <- mummi_new[,1]
      feat_info_split <- matrix(unlist(strsplit(feat_info, "__", fixed=TRUE)), ncol=2, byrow=T)
      colnames(feat_info_split) <- c("m.z", "r.t")
      
      if(rt.type == "minutes"){
        rtime <- as.numeric(feat_info_split[,2])
        rtime <- rtime * 60
        feat_info_split[,2] <- rtime
      }
      
      mummi_new <- cbind(feat_info_split, mummi_new[,-1])
    }
  }
  
  if(!is.na(mode)){
    if(mode=="positive"){
      mode <- rep("positive", nrow(mummi_new))
    }else{
      mode <- rep("negative", nrow(mummi_new))
    }
    mummi_new <- cbind(mummi_new, mode)
  }
  
  mummi_new[,1] <- as.numeric(make.unique(as.character(mummi_new[,1]), sep=""))
  mSetObj$dataSet$mummi_new <- mummi_new;
  
  if(!.on.public.web){
    filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
    write.table(mummi_new, filename, row.names = FALSE)
  }
  
  return(.set.mSet(mSetObj))
}

#'Function to save each mSetObj as a RDS file
#'to be used later in PerformMetaPSEA.
#'Should be called after SetPeakEnrichMethod/SetMummichogPval
#'@import qs
#'@export
savePeakListMetaData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  fileName <- gsub("_", "", mSetObj$dataSet$fileName)
  file_name <- paste0(fileName, ".qs")
  
  if(exists("metaFiles")){
    metaFiles <<- c(metaFiles, file_name)
  }else{
    metaFiles <<- file_name
  }
  
  if(exists("meta.anal.type")){
    meta.anal.type <<- c(meta.anal.type, anal.type)
  }else{
    meta.anal.type <<- anal.type
  }
  
  qs::qsave(mSetObj, file_name)
  
  return(.set.mSet(mSetObj));
}

#'Function to perform peak set enrichment meta-analysis
#'at either the empirical compound, compound level
#'or pathway level.
#'@description This is the main function that performs either the mummichog
#'algorithm, GSEA, or both for peak set enrichment meta-analysis. 
#'@usage performMetaPSEA(mSetObj=NA, lib, libVersion, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object. 
#'@param lib Input the name of the organism library, default is hsa_mfn. 
#'@param libVersion Input the version of the KEGG pathway libraries ("current" or "old").
#'@param permNum Numeric, input the number of permutations to perform. Default is 100.
#'@param metaLevel Character, input whether the meta-analysis is at the empirical compound ("ec"),
#'compound ("cpd"), or pathway level ("pathway").
#'@param combine.level Character, input whether to combine p-values or pool the peaks.
#'@param pval.method Character, input the method to perform p-value combination.
#'@param es.method Character, input the method to perform effect-size meta-analysis.
#'@param rank.metric Character, input how to calculate pre-ranking metric. "mean"
#'to use the average, "min" to use the lowest score, "max" to use the highest score.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

performMetaPSEA <- function(mSetObj=NA, lib, libVersion, minLib = 3, permNum = 100, metaLevel = "pathway",
                            combine.level="pvalue", pval.method = "fisher", es.method = "fixed",
                            rank.metric="mean", mutual.feats = TRUE, pooled_cutoff = 0.05){
  
  metaFiles <- unique(metaFiles);
  version <- mum.version;
  
  if(.on.public.web){
    ## TO solve the strong interferation 
    if(file.exists("mum_res.qs")) file.remove("mum_res.qs")
    if(file.exists("pathwaysFiltered.qs")) file.remove("pathwaysFiltered.qs")
    if(file.exists("mummichog_pathway_enrichment.csv")) file.remove("mummichog_pathway_enrichment.csv")
    if(file.exists("mummichog_matched_compound_all.csv")) file.remove("mummichog_matched_compound_all.csv")
    if(file.exists("mummichog_integ_pathway_enrichment.csv")) file.remove("mummichog_integ_pathway_enrichment.csv")
    if(file.exists("mummichog_fgsea_pathway_enrichment.csv")) file.remove("mummichog_fgsea_pathway_enrichment.csv")
    if(file.exists("ms_peaks_meta_anal_path_res.json")) file.remove("ms_peaks_meta_anal_path_res.json")
    if(file.exists("initial_ecs.qs")) file.remove("initial_ecs.qs")
  }
  
  CMDSet <- mSet[["cmdSet"]];
  
  if(length(unique(meta.anal.type)) > 1){
    AddErrMsg("Selected algorithms are not consistent!")
    return(0)
  }
  
  pathResults <- list();
  pathResultsWhole <- list();
  
  if(metaLevel == "pathway"){
    
    # now save compound matching results
    # and which compounds are significant - note if GSEA it's all
    cpdMatchResults <- list()
    
    for(i in 1:length(metaFiles)){
      mSetObj <- qs::qread(metaFiles[i])
      mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib);
      cpdMatchResults[[metaFiles[i]]] <- qs::qread("mum_res.qs")
      
      # don't need to write path result CSV files for meta-analysis in indiv' runs
      # need to write each individual compound matching file with their own name
      if(mSetObj$dataSet$mumRT & version=="v2"){
        mSetObj <- .init.RT.Permutations(mSetObj, permNum)
      }else{
        mSetObj <- .init.Permutations(mSetObj, permNum)
      }
      
      if(anal.type == "mummichog"){
        pathResults[[metaFiles[i]]] <- mSetObj$mummi.resmat
        pathResultsWhole[[metaFiles[i]]] <- read.csv("mummichog_pathway_enrichment.csv")
      }else if(anal.type == "gsea_peaks"){
        pathResults[[metaFiles[i]]] <- mSetObj$mummi.gsea.resmat
      }else{ # integ
        pathResults[[metaFiles[i]]] <- mSetObj$integ.resmat
        pathResultsWhole[[metaFiles[i]]] <- read.csv("mummichog_pathway_enrichment.csv")
      }
      
      if(i != length(metaFiles)){
        rm(mSetObj)
      }
    }
    
    sink("ms_peaks_meta_anal_cpd_matching.json");
    cat(RJSONIO::toJSON(cpdMatchResults));
    sink();
    
  } else { 
    
    # finally for pooled
    # this method will not handle RT (empirical cpd level)
    metaMsetObj <- vector("list")
    
    for(metafile in seq_along(metaFiles)){
      mSetObj <- qs::qread(metaFiles[metafile])
      metafile <- metaFiles[metafile]
      metaMsetObj[[metafile]]$dat <- mSetObj$dataSet$mummi.proc
      metaMsetObj[[metafile]]$pos_inx <- mSetObj$dataSet$pos_inx
      metaMsetObj[[metafile]]$ins_tol <- mSetObj$dataSet$instrument
    }
    
    # first check that all instrument tol are equal
    ins_tol <- unique(unlist(lapply(metaMsetObj, "[[", "ins_tol")))
    
    # if different instrument tols
    # do individual putative compound annotation
    if(length(ins_tol) > 1){
      
      mSetObj$mum_nm <- "mummichog_query.json"
      mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment.csv"
      
      if(version == "v2"){
        mSetObj$dataSet$mumRT <- TRUE
        mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, TRUE, "ec", 
                                       combine.level, pval.method, es.method, rank.metric, FALSE)
        mSetObj <- .init.RT.Permutations(mSetObj, permNum)
      }else{
        mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, TRUE, "cpd", 
                                       combine.level, pval.method, es.method, rank.metric, FALSE)
        mSetObj <- .init.Permutations(mSetObj, permNum)
      }
      
      return(.set.mSet(mSetObj));
    }
    
    metadat <- lapply(metaMsetObj, "[[", "dat")
    metadat <- lapply(metadat, data.frame)
    metadat <- data.table::rbindlist(metadat, idcol = TRUE)
    metadat$m.z <- make.unique(as.character(metadat$m.z), sep = "")
    
    ref_mzlist <- as.numeric(unlist(metadat[,"m.z"]))
    pos_inx <- unlist(metadat[,"pos_inx"])
    pos_inx <- ifelse(pos_inx == 1, TRUE, FALSE)
    expr_dic <- unlist(metadat[,"t.score"])
    
    if(anal.type == "mummichog"){
      pval_cutoff <- pooled_cutoff
      my.inx <- metadat[,"p.value"] < pval_cutoff;
      input_mzlist <- ref_mzlist[my.inx];
      
      if(length(input_mzlist) < 10){
        AddErrMsg("Not enough significant compounds for pathway analysis! Consider
                  changing the p-value cutoff!")
        return(0)
      }
      mSetObj$dataSet$input_mzlist <- input_mzlist
      mSetObj$dataSet$N <- length(input_mzlist)
    }
    
    mSetObj$dataSet$mummi.proc <- metadat
    mSetObj$dataSet$ref_mzlist <- ref_mzlist
    mSetObj$dataSet$pos_inx <- pos_inx
    mSetObj$dataSet$expr_dic <- expr_dic
    names(mSetObj$dataSet$expr_dic) <- ref_mzlist
    
    if(version == "v2"){
      mSetObj$dataSet$mumRT <- TRUE
    }
    
    mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, FALSE, "pooled")
    
    if(version == "v2"){
      mSetObj <- .init.RT.Permutations(mSetObj, permNum)
    }else{
      mSetObj <- .init.Permutations(mSetObj, permNum)
    }
    
    if(class(mSetObj) != "list"){
      if(mSetObj == 0){
        AddErrMsg("MS Peaks to Paths analysis failed! Likely not enough m/z to compound hits for pathway analysis!")
        return(0)
      }
    }
    
    mSetObj$metaLevel <- metaLevel
    mSetObj$pooled_cutoff <- pooled_cutoff
    mSetObj$cmdSet <- CMDSet;
    
    return(.set.mSet(mSetObj));
  }
  
  if(metaLevel == "pathway"){ # need to integrate pathway results
    
    sink("ms_peaks_meta_anal_path_res.json");
    
    if(anal.type!="gsea_peaks"){
      cat(RJSONIO::toJSON(pathResultsWhole));
      sink();
    }
    
    mSetObj$dataSet$pathResults <- pathResults; #TODO: to replace the I/O with this option
    
    num_files <- length(metaFiles)
    path.names.all <- lapply(pathResults, rownames)
    path.intersect <- Reduce(intersect, path.names.all)
    
    # remove paths not found by all three - complete.cases
    path.intersected <- lapply(pathResults, function(x) x[row.names(x) %in% path.intersect,])
    
    #li_2 <- lapply(seq_along(path.intersected), function(i) {colnames(path.intersected[[i]]) <- paste0(colnames(path.intersected[[i]]), names(path.intersected)[[i]]) ; path.intersected[[i]] } )
    #path2 <- data.table::setDF(Reduce(merge, lapply(path.intersected, data.table::data.table, keep.rownames = TRUE, key = "rn")))
    path_full <- path2 <- data.table::setDF(do.call("cbind", lapply(path.intersected, data.table::data.table, keep.rownames = TRUE, key = "rn")))
    
    # now do the integration
    # extract the p-values from each option into path2
    # first if just mummichog
    if(anal.type=="mummichog"){
      
      if(pval.method == "fisher"){
        path2_keep <- grep("rn|FET", colnames(path2))
      }else{
        path2_keep <- grep("rn|Gamma", colnames(path2))
      }
      
    }else if(anal.type=="gsea_peaks"){ # second if just gsea
      path2_keep <- grep("rn|P_val", colnames(path2))
    }else{ # third if both
      path2_keep <- grep("rn|Combined_Pvals", colnames(path2))
    }
    
    # create df of just p-values for meta-analysis
    path2 <- path2[,path2_keep]
    path2[path2==0] <- 0.00001 # for sumlog to work
    rownames(path_full) <- rownames(path2) <- path2[,1]
    rm_col_inx <- grep("rn", colnames(path2))
    path2 <- path2[,-rm_col_inx]
    
    # rename path2
    if(.on.public.web){
      #colnames(path2) <- paste0("data", seq_along(colnames(path2)))
      colnames(path2) <- tools::file_path_sans_ext(metaFiles)
    }else{
      colnames(path2) <- tools::file_path_sans_ext(metaFiles)
    }
    
    # combine p-values
    if(pval.method=="fisher"){
      meta.pvals <- apply(as.matrix(path2), 1, function(x) metap::sumlog(x))
    }else if(pval.method=="edgington"){ 
      meta.pvals <- apply(as.matrix(path2), 1, function(x) metap::sump(x))
    }else if(pval.method=="stouffer"){
      meta.pvals <- apply(as.matrix(path2), 1, function(x) metap::sumz(x))
    }else if(pval.method=="vote"){
      meta.pvals <- apply(as.matrix(path2), 1, function(x) metap::votep(x))
    }else if(pval.method=="min"){
      Meta.P <- apply(as.matrix(path2), 1, function(x) min(x) )
    }else if(pval.method=="max") {
      Meta.P <- apply(as.matrix(path2), 1, function(x) max(x) )
    }else{
      AddErrMsg("Invalid meta-analysis method!")
      return(0)
    }
    
    #extract p-values
    if(exists("meta.pvals")){
      Meta.P <- unlist(lapply(meta.pvals, function(z) z["p"]))
    }
    Meta.P <- signif(Meta.P, 5);
    path2 <- cbind(path2, Meta.P)
    path2 <- path2[order(path2$Meta.P),]
    
    mSetObj$metaLevel <- metaLevel
    mSetObj$meta_results <- path2
    mSetObj$meta.pval.method <- pval.method
    
    path_full <- cbind(path_full, Meta.P)
    col2_rm <- grep("qs.rn", colnames(path_full))
    path_full <- path_full[,-col2_rm]
    path_full <- path_full[order(path_full$Meta.P),]
    fast.write.csv(path_full, "mspeaks_meta_anal_all_results.csv", row.names = TRUE)
    
  }else{
    AddErrMsg("Invalid meta-analysis level selected!")
    return(0)
  }
  
  mSetObj$cmdSet <- CMDSet;
  
  return(.set.mSet(mSetObj));
}

############### Function for visualization of MS Peaks to Paths Meta-Analysis #######################

#'Function to create summary plot of MS Peaks to Paths
#'meta-analysis at the pathway level.
#'@description This function creates a summary plot of the
#'MS Peaks to Paths meta-analysis at the pathway level. The plot
#'can either be a heatmap or a network, both of which can
#'be made interactive. 
#'NETWORK: The size of the nodes in the network correspond to the number of
#'studies in which that pathway was significant. The color of the nodes correspond
#'to the meta-p-value for each pathway, with (default coloring) red being the most 
#'significant and yellow the least. 
#'@param mSetObj Input the name of the created mSetObj object. 
#'@param plotType Use "heatmap" to create a heatmap summary, "network" to create 
#'a network summary, or "bubble" to create a bubble plot summary of the meta-analysis
#'results.
#'@param heatmap_colorType Character, "brewer" for R Color Brewer scales or
#'"viridis" for viridis color scales. Used for creating the heatmap
#'color scheme.
#'@param heatmap_palette Character, input the preferred color palette according
#'to R Color Brewer or viridis (e.g. "RdBu").
#'@param heatmap_interactive Boolean. FALSE to create a non-interactive plot
#'and TRUE for plotly generated interactive plot.
#'@param heatmap_square Boolean. TRUE for the heatmap to be squares versus
#'rectangles (FALSE).
#'@param heatmap_allPaths Boolean. TRUE to use all paths when plotting the heatmap.
#'FALSE to use a subset of paths, number defined in npaths.
#'@param heatmap_npaths Numeric. The number of pathways to subset the pathway
#'results.
#'@param heatmap_vertical Boolean. TRUE, heatmap plot will be vertical. FALSE, heatmap plot
#'will be horizontal.
#'@param heatmap_fontSize Numeric, input the preferred font size to be used in the heatmap
#'plot.
#'@param pvalCutoff The size of the nodes in the network correspond to the number of
#'studies in which that pathway was significant. This pvalCutoff (Numeric) is thus used
#'to determine whether or not a pathway was found to be significant in each
#'individual study. 
#'@param overlap Numeric, this number is used to create edges between the nodes.
#'By default it is set to 0.25, meaning that if 2 pathways (nodes) share 25% of
#'the same compounds/empirical compounds, they will be connected by a node.
#'@param networkType Character, "static" to create a static image or
#'"interactive" to create an interactive network saved as an html 
#'in your working directory.
#'@param layout Character, layout from ggraph. "kk" for the spring-based algorithm by Kamada and Kawai
#'as default. "drl" for force directed algorithm from the DrL toolbox. "lgl" for Large Graph Layout. "fr" for
#'force-directed of Fruchterman and Reingold.
#'@param net_palette Character, input the color code for the nodes in the network. Default is
#'"YlOrRd". Uses the hcl palettes from the grDevices. Use hcl.pals()
#'to view the name of all available palettes.
#'@param netTextSize Numeric, input the preferred font size to be used in the network
#'plot.
#'@param netPlotSize Numeric, input the preferred dimensions (in inches) of the network
#'to be saved.
#'@param bubble_colorType  Character, "brewer" for R Color Brewer scales or
#'"viridis" for viridis color scales. Used for creating the bubble plot
#'color scheme.
#'@param bubble_palette Character, use two/three colors max if using R ColorBrewer palettes
#'for pleasing looking plots.
PlotPathwayMetaAnalysis <- function(mSetObj = NA, imgName, plotType = "heatmap", 
                                    heatmap_colorType = "brewer", heatmap_palette = "RdYlBu",
                                    heatmap_interactive = FALSE, heatmap_square = TRUE,
                                    heatmap_allPaths = TRUE, heatmap_npaths = 25, heatmap_vertical = TRUE,
                                    heatmap_fontSize = 9, pvalCutoff = 0.05, overlap = 0.25,
                                    networkType = "static", layout="kk", net_palette = "YlOrRd",
                                    netTextSize = 2.5, netPlotSize = 7.5, 
                                    bubble_interactive = FALSE, bubbleMaxPaths = 15,
                                    bubble_colorType = "brewer", bubble_palette = "RdBu",
                                    bubbleFontSize = 9, bubblePlotSize = 7){
  
  mSetObj <- .get.mSet(mSetObj);
  metaLevel <- mSetObj$metaLevel;
  
  if(metaLevel != "pathway"){
    AddErrMsg("Function only for pathway-level meta-analysis!")
    return(0)
  }
  
  path_results <- mSetObj$meta_results
  
  if(plotType == "heatmap"){
    
    path_results <- data.frame(apply(path_results, 2, rev), stringsAsFactors = FALSE)
    path_results$pathways <- factor(rownames(path_results), as.character(unique(rownames(path_results))))
    
    if(nrow(path_results) > heatmap_npaths & !heatmap_allPaths){
      path_results <- tail(path_results, heatmap_npaths)
    }
    
    library(reshape2)
    
    path_results <- melt(path_results, id = "pathways")
    
    if(heatmap_colorType == "brewer"){
      library(RColorBrewer)
      pal <- colorRampPalette(brewer.pal(n = 9, heatmap_palette))
      size = length(unique(path_results$value))
      pal.pal <- pal(size)
    }
    
    library(ggplot2)
    
    if(heatmap_vertical){
      path.heatmap <- ggplot(data = path_results, mapping = aes(x = variable, y = pathways, fill = value))
    }else{
      path_results <- path_results[nrow(path_results):1,]
      path.heatmap <- ggplot(data = path_results, mapping = aes(x = pathways, y = variable, fill = value))
    }
    
    path.heatmap <- path.heatmap +
      geom_tile(color = "white") +
      #remove x and y axis labels
      labs(x="", y="", title="MS Peaks to Pathway Meta-Analysis") +
      #remove extra space
      scale_y_discrete(expand=c(0,0)) +
      scale_x_discrete(expand=c(0,0)) +
      #set a base size for all fonts
      theme_grey(base_size=heatmap_fontSize) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #theme options
      theme(
        #bold font for legend text
        legend.text=element_text(face="bold"),
        #set thickness of axis ticks
        axis.ticks=element_line(size=0.4),
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
    
    if(heatmap_colorType == "brewer"){
      path.heatmap <- path.heatmap +
        scale_fill_gradient2(low = pal.pal[1],
                             mid = pal.pal[size/2],
                             high = pal.pal[size],
                             midpoint = 0.5,
                             name = "P-Value",
                             na.value="transparent")
    }else{
      
      library(viridis)
      check.palette <- heatmap_palette %in% c("viridis", "magma", "plasma", "inferno")
      
      if(!check.palette){
        heatmap_palette <- "viridis"
      }
      
      path.heatmap <- path.heatmap +
        scale_fill_viridis(option=heatmap_palette)+
        labs(fill="P-Value") 
    }
    
    if(heatmap_square){
      path.heatmap <- path.heatmap + coord_fixed()
    }
    
    ggsave("mspeaks_pathway_heatmap.png")
    
    if(heatmap_interactive){
      library(plotly)
      ax <- list(
        zeroline = FALSE,
        showline = FALSE,
        showgrid = FALSE
      )
      p <- plotly::ggplotly(path.heatmap)
      p <- p %>% layout(xaxis = ax, yaxis = ax)
      htmlwidgets::saveWidget(p, "mspeaks_pathway_heatmap_interactive.html")
    }
    
  }
  
  if(plotType == "network"){
    
    hits_sig <- rowSums(path_results[, -ncol(path_results)] < pvalCutoff) + 1 # account for 0 studies < cutoff
    folds <- scales::rescale(hits_sig, to = c(1,10))
    names(folds) <- GetShortNames(rownames(path_results));
    pvals <- path_results[,ncol(path_results)];
    names(pvals) <- rownames(path_results)
    title <- "MS Peaks to Pathway Network Overview";
    
    if("emp_cpds" %in% names(mSet$pathways)){
      path.names <- mSetObj$pathways$name
      current.mset <- mSetObj$pathways$emp_cpds;
      names(current.mset) <- path.names
    }else{
      path.names <- mSetObj$pathways$name
      current.mset <- mSetObj$pathways$cpds;
      names(current.mset) <- path.names
    }
    
    if(length(folds) > 50){
      folds <- folds[1:50];
      pvals <- pvals[1:50];
      title <- "MS Peaks to Pathway Network Overview (Top 50)";
    }
    
    if(.on.public.web){
      load_igraph()
      load_reshape()
    }
    
    pvalue <- pvals;
    id <- names(pvalue);
    geneSets <- current.mset;
    n <- length(pvalue);
    w <- matrix(NA, nrow=n, ncol=n);
    colnames(w) <- rownames(w) <- id;
    
    for (i in 1:n) {
      for (j in i:n) {
        w[i,j] = overlap_ratio(geneSets[id[i]], geneSets[id[j]])
      }
    }
    
    wd <- melt(w);
    wd <- wd[wd[,1] != wd[,2],];
    wd <- wd[!is.na(wd[,3]),];
    g <- graph.data.frame(wd[,-3], directed=F);
    E(g)$width <- sqrt(wd[,3]*20);
    g <- delete.edges(g, E(g)[wd[,3] < overlap]); # change 
    V(g)$color <- hcl.colors(length(pvalue), net_palette);
    
    cnt <- folds;
    names(cnt) <- id;
    
    if(networkType == "static"){
      V(g)$size <- cnt + 3;
    }else{
      V(g)$size <- cnt + 20;
    }
    
    pos.xy <- layout.fruchterman.reingold(g,niter=500);
    
    # now create the json object
    nodes <- vector(mode="list");
    node.nms <- V(g)$name;
    node.sizes <- V(g)$size;
    node.cols <- V(g)$color;
    
    if(.on.public.web){
      for(i in 1:length(node.sizes)){
        nodes[[i]] <- list(
          id = node.nms[i],
          label=node.nms[i], 
          size=node.sizes[i], 
          color=node.cols[i],
          x = pos.xy[i,1],
          y = pos.xy[i,2]
        );
      }
      
      edge.mat <- get.edgelist(g);
      edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2]);
      # covert to json
      netData <- list(nodes=nodes, edges=edge.mat);
      sink("ms_peaks_network.json");
      cat(RJSONIO::toJSON(netData));
      sink();
      return(g);  
    }else{
      if(networkType == "static"){
        # static plot
        library(ggraph)
        p <- ggraph(g, layout=layout) + theme_void() +
          geom_edge_fan(color="gray20", width=0.5, alpha=0.5) +
          geom_node_point(color=V(g)$color, size=V(g)$size, alpha = 0.95) +
          geom_node_text(aes(label = V(g)$name), size = netTextSize, repel=TRUE, nudge_y = 0.05, nudge_x = 0.05, check_overlap = TRUE) +
          # 10% space above/to the side of x
          scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
          scale_x_continuous(expand = expansion(mult = c(.1, .1)))
        
        filename <- paste0(anal.type, "_network", ".png")
        ggsave(p, file=filename, width = netPlotSize, height = netPlotSize)
        
      }else{ 
        # interactive plot
        library(visNetwork)
        data <- toVisNetworkData(g)
        network <- visNetwork(nodes = data$nodes, edges = data$edges, 
                              idToLabel = TRUE, height = "900px", width = "100%") %>% visEdges(color=list(color="grey", highlight="red")) %>% visNodes(font = list(size = 30))
        
        filename <- paste0(anal.type, "_network", ".html")
        visSave(network, file = filename)
      }
    }
  }
  
  if(plotType == "bubble"){
    
    full_results <- read.csv("mspeaks_meta_anal_all_results.csv", row.names = 1)
    
    if(nrow(path_results) > bubbleMaxPaths){
      path_results <- path_results[seq_len(bubbleMaxPaths),]
      full_results <- full_results[seq_len(bubbleMaxPaths),]
    }
    
    if(anal.type=="gsea_peaks"){
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".qs.Pathway_Total")
      studies_sig_hits <- paste0(studies, ".qs.Hits")
      
    }else if(anal.type=="mummichog"){
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".qs.Pathway.total")
      studies_sig_hits <- paste0(studies, ".qs.Hits.sig")
      
    }else{ #integ
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".qs.Total_Size")
      studies_sig_hits <- paste0(studies, ".qs.Sig_Hits")
      
    }
    
    studies_pathway_total <- full_results[,grepl(paste(studies_pathway_total, collapse="|"), colnames(full_results))]
    studies_pathway_total_list <- lapply(split(t(studies_pathway_total), 1:nrow(t(studies_pathway_total))), unlist)
    
    studies_sig_hits <- full_results[,grepl(paste(studies_sig_hits, collapse="|"), colnames(full_results))]
    studies_sig_hits_list <- lapply(split(t(studies_sig_hits), 1:nrow(t(studies_sig_hits))), unlist)
    
    ratio <- as.data.frame(mapply(function(X, Y) {
      x = unlist(X)
      y = unlist(Y)
      ratio = x/y
      return(ratio)
    }, X = studies_sig_hits_list, Y = studies_pathway_total_list))
    
    ratio_String <- as.data.frame(mapply(function(X, Y, Z) {
      x = unlist(X)
      y = unlist(Y)
      z = unlist(Z)
      #ratio_String = paste0(z, " [", x, "/", y, "]");
      ratio_String = paste0(z);
      return(ratio_String)
    }, 
    X = studies_sig_hits_list, 
    Y = studies_pathway_total_list, 
    Z = as.list(path_results[,-ncol(path_results)])))
    
    ## Added row to calculate the average ratio
    ratio_mean <- apply(ratio, 1, mean);
    ratio <- cbind(ratio, ratio_mean);
    
    ratio_String <- cbind(ratio_String, format(round(ratio_mean, 4)*100, nsmall = 2));
    
    colnames(ratio) <- c(studies,"Meta.P");
    #colnames(ratio_String) <- c(paste0(sub("mummichoginput", "",studies),"_Pvalue [Sig/Total]"), "Mean_Enrich(%)");
    colnames(ratio_String) <- c(paste0(sub("mummichoginput", "",studies),"_Pvalue"), "Mean_Enrich(%)");
    ratio$Pathway <- rownames(path_results);
    
    ratio_String <- cbind(rownames(path_results), ratio_String);
    
    ratio2 <- reshape2::melt(ratio, id.vars = "Pathway", variable.name = "Study", value.name = "enrichment ratio")
    
    #path_results <- path_results[, -length(colnames(path_results))]
    path_results$Pathway <- rownames(path_results)
    path_results2 <- reshape2::melt(path_results, id.vars = "Pathway", variable.name = "Study", value.name = "p-value");
    
    res_table <- cbind(ratio_String, path_results$Meta.P);
    colnames(res_table)[c(1, length(colnames(res_table)))] <- c("Pathways", "Meta.P")
    
    res_table <- res_table[order(res_table$Meta.P, -as.numeric(res_table$Mean_Enrich)),]
    
    mSetObj$dataSet$res_table <- res_table;
    
    require(ggplot2);
    require(RColorBrewer);
    
    ## This oder matrix is used to order the metap (first by P value, second by enrichment ratio)
    order_matrix0 <- merge(path_results, ratio, by = c("Pathway"));
    ColNMs <- c("Pathway", paste0(sub("mummichoginput", "",studies),"p-value"),"Meta.P",paste0(sub("mummichoginput", "",studies),"Enrich"),"Average_Enrich")
    
    order_matrix <- cbind(order_matrix0$Pathway, order_matrix0$Meta.P.x, order_matrix0$Meta.P.y);
    metap_order <- order_matrix[order(order_matrix[,2], -as.numeric(order_matrix[,3]))]
    
    colnames(order_matrix0) <- ColNMs;
    write.csv(order_matrix0, file = "Result_summary.csv",row.names = FALSE)
    
    df <- merge(path_results2, ratio2, by = c("Pathway", "Study"))
    df$Pathway <- factor(df$Pathway, levels = rownames(path_results))
    df$Study <- sub("mummichoginput", "", df$Study);
    df$Study <- as.factor(df$Study);
    save(df, file = "df.rda")
    save(metap_order, file = "metap_order.rda")
    p <- ggplot(df, aes(x = Study, y = Pathway)) +
      geom_point(aes(col = `p-value`, size = `enrichment ratio`)) + 
      theme(legend.key=element_blank(), 
            axis.text = element_text(size = bubbleFontSize),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      scale_y_discrete(limits = rev(metap_order)) + 
      guides(size = guide_legend(order = 0))
    
    if(bubble_colorType == "brewer"){
      pal <- brewer.pal(n = 8, bubble_palette)
      p <- p + scale_colour_gradientn(colours = pal)
    }else{
      
      check.palette <- bubble_palette %in% c("viridis", "magma", "plasma", "inferno")
      
      if(!check.palette){
        bubble_palette <- "viridis"
      }
      
      p <- p + scale_color_viridis_c(option=bubble_palette, direction = -1)
    }
    
    filename <- paste0(imgName, ".png")
    ggsave(p, file=filename, width = bubblePlotSize, height = bubblePlotSize*0.6)
    
    if(bubble_interactive){
      library(plotly)
      ax <- list(
        zeroline = FALSE,
        showline = FALSE,
        showgrid = FALSE
      )
      p <- plotly::ggplotly(p)
      p <- p %>% layout(xaxis = ax, yaxis = ax)
      htmlwidgets::saveWidget(p, "mspeaks_pathway_bubble_interactive.html")
    }
    
    mSetObj$imgSet$mummi.meta.path.plot <- filename;
    
  }
  return(.set.mSet(mSetObj));
}

########################################
########################################
########### Functions for Web ##########
########################################
########################################

## R functions used to prepare for meta-mummichog analysis
PrepareMetaPath <- function(mSetObj = NA, mode = "negative", ppm = 30, 
                            version = "v2", pcutoff = 0.05, rt.type = "seconds") {
  
  if(file.exists("mSet.qs") & .on.public.web){
    mSet <<- mSetObj <- qs::qread("mSet.qs")
  } else {
    mSetObj <- .get.mSet(mSetObj);
    qs::qsave(mSetObj, file = "mSet.qs")
  }
  
  if(rt.type == "false"){
    rt = FALSE
  }else{
    rt = TRUE
    if(!(rt.type %in% c("seconds", "minutes"))){
      AddErrMsg("Invalid RT type! Should be seconds or minutes!")
      return(0)
    }
  }
  
  if(!exists("rt.studies")){
    rt.studies <- vector()
  }
  
  rt.studies <<- c(rt.studies, rt)
  CMDSet <- mSetObj$cmdSet
  
  if(length(mSetObj$dataSet2) == 0) {
    
    # Here is dealing with the single ion mode data
    mSetObj <- Ttests.Anal(mSetObj, F, 0.05, FALSE, TRUE);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- Convert2MummichogMetaPath(mSetObj, rt, rds.file=FALSE, rt.type, "all", mode);
    
    mSetObj <- .get.mSet(mSetObj);
    fileNM <- mSetObj$dataSet$name;
    fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
    filename <- paste0(fileNM, "_mummichog_input.txt");
    
    mSetObj <- .get.mSet(mSetObj);
    mummi_new <- mSetObj$dataSet$mummi_new;
    write.table(mummi_new, filename, row.names = FALSE)
  } else {
    
    # Here is dealing with the mix ion modes' data
    # 1st step -- handle the dataset (pos) data in mSet
    file.rename("data_orig1.qs", "data_orig.qs");
    mSetObj1 <- mSetObj2 <- vector("list")
    mSetObj1$dataSet <- mSetObj$dataSet
    mSetObj2$dataSet <- mSetObj$dataSet2
    .set.mSet(mSetObj1)
    mSetObj1 <- Ttests.Anal(mSetObj1, F, 0.05, FALSE, TRUE);
    mSetObj1 <- Convert2MummichogMetaPath(mSetObj1, rt, rds.file=FALSE, rt.type, "all", "positive");
    mSetObj1 <- .get.mSet(mSetObj1);
    dataset_pos <- mSetObj1$dataSet;
    
    file.rename("data_orig.qs", "data_orig1.qs");
    
    # 2nd step -- handle the dataset (neg) data in mSet
    .set.mSet(mSetObj2)
    file.rename("data_orig2.qs", "data_orig.qs");
    mSetObj2 <- Ttests.Anal(mSetObj2, F, 0.05, FALSE, TRUE);
    mSetObj2 <- Convert2MummichogMetaPath(mSetObj2, rt, rds.file=FALSE, rt.type, "all", "negative");
    mSetObj2 <- .get.mSet(mSetObj2);
    dataset_neg <- mSetObj2$dataSet;
    file.rename("data_orig.qs", "data_orig2.qs");
    fileNM <- mSetObj$dataSet$name;
    fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
    filename <- paste0(fileNM, "_mixed_mummichog_input.txt");
    
    # Merge and save them
    mtbl_all <- rbind(dataset_pos$mummi_new, dataset_neg$mummi_new[-1,]) 
    write.table(mtbl_all, filename, row.names = FALSE, col.names = TRUE)
    
    rm(mSetObj1)
    rm(mSetObj2)
  }
  
  mSet <<- NULL;
  mSetObj <- NULL;
  
  mSetObj<-InitDataObjects("mass_all", "mummichog", FALSE);
  SetPeakFormat("mpt");
  mSetObj<-UpdateInstrumentParameters(mSetObj, ppm, mode);
  mSetObj<-Read.PeakListData(mSetObj, filename, meta.anal=TRUE, method="both");
  mSetObj<-SanityCheckMummichogData(mSetObj);
  
  if(.on.public.web){
    res <- SetMummichogPval(mSetObj, pcutoff);
  }else{
    mSetObj <- SetMummichogPval(mSetObj, pcutoff);
  }
  
  mSetObj <- savePeakListMetaData(mSetObj)
  
  if(.on.public.web){
    mSetObj <- InitDataObjects("conc", "metapaths", FALSE)
    mSet$cmdSet <<- CMDSet;
    return(res)
  }else{
    mSetObj$analSet$type <- "metapaths";
    anal.type <<- "metapaths"
    return(.set.mSet(mSetObj))
  }
}

CheckAllRT <- function(){
  
  if(all(rt.studies)){
    rt = "true"
  }else{
    rt = "false"
  }
  
  return(rt)
}

mSetQSDelete <- function(){
  if (file.exists("mSet.qs")) {
    file.remove("mSet.qs")
  }
}

CacheQSClean <- function(){
  if(file.exists("complete_norm.qs")) file.remove("complete_norm.qs");
  if(file.exists("complete_norm1.qs")) file.remove("complete_norm1.qs");
  if(file.exists("complete_norm2.qs")) file.remove("complete_norm2.qs");
}

readMetaPathTable <- function(mSetObj = NA,  dataNM, dataFormat, dataType) {
  
  if(.on.public.web){
    if(file.exists("data_orig.qs")) file.remove("data_orig.qs");
    if(file.exists("data_orig1.qs")) file.remove("data_orig1.qs");
    if(file.exists("data_orig2.qs")) file.remove("data_orig2.qs")
    if(file.exists("mSet.qs")) file.remove("mSet.qs")
    if(file.exists("prenorm.qs")) file.remove("prenorm.qs")
    if(file.exists("preproc.qs")) file.remove("preproc.qs")
  }
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(dataType == "massPeaks"){
    # Handle mass peaks for mummichog
    mSetObj <- Read.TextData(mSetObj, dataNM, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$dataSet$name <- dataNM;
    mSetObj$dataSet$format <- dataFormat;
  } else if (dataType == "annoPeaks") {
    # Handle annotated compounds for QEA pathway
    # TODO: add more later
  } else {
    
    if(.on.public.web){
      return (0);
    }else{
      AddErrMsg("Could not upload data file!")
      return(0)
    }
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return (1)
  }else{
    return(.set.mSet(mSetObj));
  }
}

readMetaPathTableMix <- function(mSetObj = NA,  dataNM, dataNM2, dataFormat, dataType) {
  
  if(.on.public.web){
    if(file.exists("data_orig.qs")) file.remove("data_orig.qs");
    if(file.exists("data_orig1.qs")) file.remove("data_orig1.qs");
    if(file.exists("data_orig2.qs")) file.remove("data_orig2.qs")
    if(file.exists("mSet.qs")) file.remove("mSet.qs")
    if(file.exists("prenorm.qs")) file.remove("prenorm.qs")
    if(file.exists("preproc.qs")) file.remove("preproc.qs")
  }
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(dataType == "massPeaks"){
    # Handle mass peaks for mummichog
    mSetObj <- Read.TextData(mSetObj, dataNM, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
    mSetObj$dataSet$name <- dataNM;
    mSetObj$dataSet$format <- dataFormat;
    mSet0 <- mSetObj;
    file.rename("data_orig.qs", "data_orig1.qs")
    
    mSet0$dataSet2 <- list();
    mSetObj <- Read.TextData(mSetObj, dataNM2, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
    mSet0$dataSet2 <- mSetObj$dataSet;
    mSet0$dataSet2$name <- dataNM2;
    mSet0$dataSet2$format <- dataFormat;
    file.rename("data_orig.qs", "data_orig2.qs")
  } else if (dataType == "annoPeaks") {
    # Handle annotated compounds for QEA pathway
    # TODO: add more later
  } else {
    return (0);
  }
  
  if(.on.public.web){
    .set.mSet(mSet0)
    return (1)
  }else{
    return(.set.mSet(mSet0));
  }
}

SanityCheckMetaPathTable<-function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- qs::qread(dataName);
  }else{
    dataSet <- mSetObj$dataSet
  }
  
  if(length(mSetObj$dataSet2) > 0){
    data_oriNM <- c("data_orig1.qs", "data_orig2.qs");
  } else {
    data_oriNM <- "data_orig.qs";
  }
  
  msg <- NULL;
  
  for(i in seq(data_oriNM)){
    
    conc <- qs::qread(data_oriNM[i]);
    
    if(i == 2){
      dataSet <- mSetObj[[paste0("dataSet",i)]];
    }
    
    cls.lbl <-  dataSet$cls.orig <- dataSet$orig.cls;
    smpl.nms <- rownames(conc);
    var.nms <- colnames(conc);
    empty.inx <- is.na(smpl.nms) | smpl.nms == ""
    if(sum(empty.inx) > 0){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty rows</font> were detected and excluded from your data."));
      smpl.nms <- smpl.nms[!empty.inx];
      cls.lbl <-  cls.lbl[!empty.inx];
      conc <- conc[!empty.inx, ];
    }else{
      msg <- c(msg, paste0("No empty rows were found in your data: ", dataName));
    }
    
    # try to check & remove empty lines if class label is empty
    # Added by B. Han
    empty.inx <- is.na(cls.lbl) | cls.lbl == ""
    if(sum(empty.inx) > 0){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty labels</font> were detected and excluded from your data."));
      smpl.nms <- smpl.nms[!empty.inx];
      cls.lbl <-  cls.lbl[!empty.inx];
      conc <- conc[!empty.inx, ];
    }else{
      msg <- c(msg, paste0("No empty labels were found in your data: ", dataName));
    }
    
    if(length(unique(cls.lbl[!empty.inx])) > 2){
      msg <- c(msg, paste(c("Groups found:", unique(cls.lbl[!empty.inx])), collapse=" "));
      msg <- c(msg, "<font color=\"red\">Meta-analysis is only defined for two-group comparisions!</font>");
      current.msg <<- msg;
      return(0);
    }else{
      lvls <- as.character(unique(unlist(cls.lbl)))
      msg <- c(msg, paste("Two groups found:", lvls[1], "and", lvls[2], collapse=" "));
    }
    
    # check for uniqueness of dimension name
    if(length(unique(smpl.nms))!=length(smpl.nms)){
      dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
      msg <- c(msg, "Duplicate sample names are not allowed!");
      current.msg <<- msg;
      return(0);
    }else{
      msg <- c(msg, "All sample names are unique.");
    }
    
    # try to remove check & remove empty line if feature name is empty
    empty.inx <- is.na(var.nms) | var.nms == "";
    if(sum(empty.inx) > 0){
      msg <- c(msg, paste("<font color=\"red\">", sum(empty.inx), "empty features</font> were detected and excluded from your data."));
      var.nms <- var.nms[!empty.inx];
      conc <- conc[,!empty.inx];
    }else{
      msg <- c(msg, "No empty feature names found");
    }
    
    if(length(unique(var.nms))!=length(var.nms)){
      dup.inx <- which(duplicated(var.nms));
      msg <- c(msg, paste("Error: a total of", length(dup.inx), "duplicate feature names found!"));
      if(length(dup.inx) > 9){
        dup.inx <- dup.inx[1:9];
      }
      dup.nm <- paste("Duplicated names [max 9]: ", var.nms[dup.inx], collapse=" ");
      AddErrMsg(dup.nm);
      current.msg <<- msg;
      return(0);
    }else{
      msg <- c(msg, "All feature names are unique");
    }
    
    # now check for special characters in the data labels
    if(sum(is.na(iconv(smpl.nms)))>0){
      na.inx <- is.na(iconv(smpl.nms));
      nms <- paste(smpl.nms[na.inx], collapse="; ");
      msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
      current.msg <<- msg;
      return(0);
    }else{
      msg <- c(msg, "All sample names are OK");
    }
    
    if(sum(is.na(iconv(var.nms)))>0){
      na.inx <- is.na(iconv(var.nms));
      nms <- paste(var.nms[na.inx], collapse="; ");
      msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
      current.msg <<- msg;
      return(0);
    }else{
      msg <- c(msg, "All feature names are OK");
    }
    
    # only keep alphabets, numbers, ",", "." "_", "-" "/"
    smpl.nms <- gsub("[^[:alnum:]./_-]", "", smpl.nms);
    var.nms <- gsub("[^[:alnum:][:space:],'./_-]", "", var.nms); # allow space, comma and period
    cls.lbl <- ClearStrings(as.vector(cls.lbl));
    
    # now assgin the dimension names
    conc <- apply(conc, 2, as.numeric);
    rownames(conc) <- smpl.nms;
    colnames(conc) <- var.nms;
    
    proc.cls <- as.factor(as.character(cls.lbl));
    
    # now need to remove low quality samples and genes
    data <- conc;
    smpl.num <- nrow(data);
    gene.num <- ncol(data);
    
    # remove smpls/exp with over half missing value
    good.inx<-apply(is.na(data), 1, sum)/ncol(data)<0.6;
    smpl.msg <- "";
    if(sum(!good.inx)>0){
      
      msg <- c(msg, paste(sum(!good.inx), "low quality samples(>60% missing) removed."));
      
      data <- data[good.inx,];
      if(nrow(data)/smpl.num < 0.5){
        msg <- c(msg, paste(msg, "Low quality data rejected!"));
        current.msg <<- msg;
        return(0);
      }
      
      # update meta information
      proc.cls <- proc.cls[good.inx];
    }
    
    if(ncol(data) < 4){
      msg <- c(msg, paste("The sample # (", nrow(data), ") is too small."));
      current.msg <<- msg;
      return(0);
    }else{
      msg <- c(msg, paste("A total of", nrow(data), "samples were found."));
    }
    
    # feature with 75% NA will be removed
    gd.inx<-apply(is.na(data), 2, sum)/nrow(data) < 0.75;
    
    feat.msg <- "";
    if(sum(!gd.inx) > 0){
      data <- data[, gd.inx];
      msg <- c(msg, paste(sum(!gd.inx), "low quality features (>75% missing) removed"));
      if(ncol(data)/gene.num < 0.25){
        msg <- c(msg, paste(feat.msg, "Low quality data rejected."));
        current.msg <<- msg;
        return(0);
      }
    }
    
    # feature with 90% ZEROs will be removed
    gd.inx<-apply(data==0, 2, function(x) {sum(x, na.rm=T)})/nrow(data) < 0.9;
    
    feat.msg <- "";
    if(sum(!gd.inx) > 0){
      data <- data[, gd.inx];
      msg <- c(msg, paste(sum(!gd.inx), "low quality features (>90% zeros) removed"));
      if(ncol(data)/gene.num< 0.25){
        msg <- c(msg, paste(feat.msg, "Low quality data rejected."));
        current.msg <<- msg;
        return(0);
      }
    }
    
    if(ncol(data) < 10){
      msg <- c(msg, "The feature# (", ncol(data), ") is too small (<10).");
      current.msg <<- msg;
      return(0);
    } else {
      msg <- c(msg, paste("A total of", ncol(data), "features were found.", collapse=" "));
    }
    
    # replace missing values should use median for normalized data
    min.val <- min(data[data>0], na.rm=T)/10;
    data[is.na(data)] <- min.val;
    data[data<=0] <- min.val;
    
    
    dataSet$data.proc <- dataSet$data <- data;
    dataSet$cls.proc <- dataSet$cls <- factor(proc.cls);
    
    ## set 1 or 2 
    if(i == 1){
      mSetObj$dataSet <- dataSet
    } else {
      mSetObj$dataSet2 <- dataSet
    }
    
    #RegisterData(mSetObj, dataSet)
  } # end of the for loop
  
  # Collect all msg here 
  mSetObj$dataSet$check.msg <- msg;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(1);
  } else {
    RegisterData(mSetObj, dataSet)
    return(.set.mSet(mSetObj));
  }
  
}

GetMetaPathSanityCheckMsg <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$check.msg);
} 

GetMetaPathDataDims <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    data <- mSetObj$dataSet2$data;
  } else {
    data <- mSetObj$dataSet$data;
  }
  
  dm <- dim(data);
  naNum <- sum(is.na(data));
  zoNum <- sum(data == 0);
  return(c(dm, naNum, zoNum));
} 

GetMetaPathGroupNames <-function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$dataSet$name != dataName){
    dataSet <- qs::qread(dataName);
  }
  return(levels(mSetObj$dataSet$cls));
}

PlotPathDataProfile<-function(dataName, boxplotName, pcaName, dataformat){
  #dataSet <- qs::qread(dataName);
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_lattice()
  }
  datatable <- mSetObj$dataSet$data;
  
  if(is.null(datatable)){
    
    if(dataformat == "colu"){
      dt <- t(read.csv(dataName)[-1,])
      colnames(dt) <- dt[1,]
    } else if(dataformat == "rowu"){
      dt <- read.csv(dataName,header = FALSE);
      rownames(dt) <- dt[,1]
      dt <- dt[,-c(1,2)]
      colnames(dt) <- dt[1,]
      dt <- dt[-1,]
    }
    datatable <- dt;
    is.na(datatable) <-0 ;
  }
  
  qc.boxplot(datatable, boxplotName);
}

MetaPathNormalization <- function(mSetObj = NA, sampleNor, tranform, scale){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(length(mSetObj$dataSet2) == 0){
    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj, "iqr", "F", 25)
    mSetObj <- PreparePrenormData(mSetObj)
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, "NULL", ratio=FALSE, ratioNum=20) #TODO: to enable scale function later
    mSetObj <- PlotNormSummary(mSetObj, "norm")
    mSetObj <- .get.mSet(mSetObj);
  } else {
    ## This is used to deal with the mix data files
    # 1st step, process dataset 1 (pos data)
    file.rename("data_orig1.qs", "data_orig.qs");
    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj, "iqr", "F", 25)
    mSetObj <- PreparePrenormData(mSetObj)
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, "NULL", ratio=FALSE, ratioNum=20) #TODO: to enable scale function later
    mSetObj <- PlotNormSummary(mSetObj, "norm_pos")
    file.rename("data_orig.qs", "data_orig1.qs");
    file.rename("complete_norm.qs", "complete_norm1.qs");
    mSetObj <- .get.mSet(mSetObj);
    dataset_pos <- mSetObj$dataSet;
    
    # 2nd step, process dataset 2 (neg data)
    file.rename("data_orig2.qs", "data_orig.qs");
    mSetObj$dataSet <- mSet$dataSet2;
    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj, "iqr", "F", 25)
    mSetObj <- PreparePrenormData(mSetObj)
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, "NULL", ratio=FALSE, ratioNum=20) #TODO: to enable scale function later
    mSetObj <- PlotNormSummary(mSetObj, "norm_neg")
    file.rename("data_orig.qs", "data_orig2.qs");
    file.rename("complete_norm.qs", "complete_norm2.qs");
    
    mSetObj <- .get.mSet(mSetObj);
    dataset_neg <- mSetObj$dataSet;
    # refine the mSet
    mSetObj$dataSet <- dataset_pos;
    mSetObj$dataSet2 <- dataset_neg;
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(1)
  }else{
    return(.set.mSet(mSetObj));
  }
}

GetMetaPathResultColNames <- function() {
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$res_table)[-1])
}

GetMetaPathNMs <- function() {
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$res_table$Pathways)
}

GetMetaPathResultItem <- function(rowN) {
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$dataSet$res_table[,-1];
  return(as.character(unname(res[rowN,])))
}

PrepareCMPDList <- function() {
  
  MetaMummiRes <- RJSONIO::fromJSON("ms_peaks_meta_anal_cpd_matching.json");
  CMPDSet <- NULL;
  CMPDSet <- unique(unlist(lapply(MetaMummiRes, function(x) return(unique(x$Matched.Compound)))))
  
  return(CMPDSet)
}

Prepare4Network <- function(){
  
  mSet <<- mSetObj <- NULL;
  
  mSetObj <- InitDataObjects("conc", "network", FALSE)
  mSetObj <- SetOrganism(mSetObj, "hsa")
  cmpdList <- PrepareCMPDList()
  
  cmpdList <- paste(cmpdList, collapse = "\n")
  mSetObj <- PerformCmpdMapping(mSetObj, cmpdList, "hsa", "kegg")
  
  #mSetObj <- CreateMappingResultTable(mSetObj)
  mSetObj <- PrepareNetworkData(mSetObj)
  
  idtype <<- "cmpd";
  mSetObj <- PrepareKeggQueryJson(mSetObj);
  mSetObj <- PerformKOEnrichAnalysis_KO01100(mSetObj, "pathway","network_enrichment_pathway_0")
  
  return(1)
}

pathsum <- function(pvalCutoff){
  
  mSetObj <- .get.mSet(NA);
  
  pathSet <- mSetObj$dataSet$pathResults;
  
  if(anal.type == "mummichog"){
    pathSum <- lapply(pathSet, function(x)  return(rownames(x)[as.data.frame(x)$FET < pvalCutoff]));
  }else{
    pathSum <- lapply(pathSet, function(x) return(rownames(x)[as.data.frame(x)$P_val < pvalCutoff]));
  }
  
  names(pathSum) <- gsub("mummichoginput.qs", "", names(pathSum));
  mSetObj$dataSet$pathSumData <- pathSum;
  res <- .set.mSet(mSetObj);
  return(pathSum)
}

SelectMultiPathData <- function(mSetObj=NA, nmVec = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.na(nmVec[1])){
    AddErrMsg("No dataset is selected for analysis!");
    return(0);
  }
  
  mSetObj[["dataSet"]][["pathResults_SelectedFiles"]] <- 
    gsub("_","",gsub("\\.[^.]*$", "", basename(nmVec)));
  
  return(.set.mSet(mSetObj));
}

PrepareMetaPathData <- function(mSetObj = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # do something here later
  dat <- mSetObj[["dataSet"]][["pathSumData"]];
  datNum <- length(mSetObj[["dataSet"]][["pathResults_SelectedFiles"]]);
  
  dat <- dat[c(mSetObj[["dataSet"]][["pathResults_SelectedFiles"]])];
  
  if(datNum == 2){
    Prepare2Venn(dat);
  }else if(datNum == 3){
    Prepare3Venn(dat);
  }else if(datNum == 4){
    Prepare4Venn(dat);
  }else{
    AddErrMsg("Too big of a number of features for the venn diagram");
    return(0);
  }
  
  return(.set.mSet(mSetObj));
}

GetVennPathsNames <- function(mSetObj=NA, areas){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- vennData[[areas]];
  if(length(res) ==0){
    return("NA")
  } else {
    return(paste(res, collapse="||"))
  } 
}


