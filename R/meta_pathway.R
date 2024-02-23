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
#'@param mode ion mode, positive or negative
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Convert2MummichogMetaPath <- function(mSetObj=NA, 
                                      rt=FALSE, 
                                      rds.file=FALSE, 
                                      rt.type="seconds", 
                                      test="tt", 
                                      mode=NA){
  
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
    } else if (mode=="negative") {
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
#'@param mSetObj mSetObj
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

#'PerformMetaPSEA
#'Function to perform peak set enrichment meta-analysis
#'at either the empirical compound, compound level
#'or pathway level.
#'@description This is the main function that performs either the mummichog
#'algorithm, GSEA, or both for peak set enrichment meta-analysis. 
#'@param mSetObj Input the name of the created mSetObj object. 
#'@param lib Input the name of the organism library, default is hsa_mfn. 
#'@param libVersion Input the version of the KEGG pathway libraries ("current" or "old").
#'@param minLib numeric, default is 3
#'@param permNum Numeric, input the number of permutations to perform. Default is 100.
#'@param metaLevel Character, input whether the meta-analysis is at the empirical compound ("ec"),
#'compound ("cpd"), or pathway level ("pathway").
#'@param combine.level Character, input whether to combine p-values or pool the peaks.
#'@param pval.method Character, input the method to perform p-value combination.
#'@param es.method Character, input the method to perform effect-size meta-analysis.
#'@param rank.metric Character, input how to calculate pre-ranking metric. "mean"
#'to use the average, "min" to use the lowest score, "max" to use the highest score.
#'@param mutual.feats mutual.feats, logical
#'@param pooled_cutoff pooled_cutoff, numeric
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

PerformMetaPSEA <- function(mSetObj=NA, 
                            lib, 
                            libVersion, 
                            minLib = 3, 
                            permNum = 100, 
                            metaLevel = "pathway",
                            combine.level="pvalue", 
                            pval.method = "fisher", 
                            es.method = "fixed",
                            rank.metric="mean", 
                            mutual.feats = TRUE, 
                            pooled_cutoff = 0.05) {
                            mSetObj<<-NA; 
                            lib <<- "hsa_kegg"; 
                            libVersion <<- "current"; 
                            minLib <<- 3; 
                            permNum <<- 100; 
                            metaLevel <<- "pathway";
                            combine.level<<-"pvalue"; 
                            pval.method <<- "fisher"; 
                            es.method <<- "fixed";
                            rank.metric<<-"mean"; 
                            mutual.feats <<- FALSE; 
                            pooled_cutoff <<- 0.001
  save.image("metapsea.RData");

  require(plyr);
  mSetObj <- .get.mSet(mSetObj);

  # record lib
  mSetObj$paramSet$lib <- lib;
  mSetObj$initPSEA <- true;

  .set.mSet(mSetObj);

  metaFiles <- mSetObj[["IncludedDataSets"]]
  #metaFiles <- unique(metaFiles);
  version <- mum.version <- mSetObj$paramSet$version;
  
  ## TO solve the strong interferation 
  if(file.exists("mum_res.qs")) file.remove("mum_res.qs")
  if(file.exists("pathwaysFiltered.qs")) file.remove("pathwaysFiltered.qs")
  if(file.exists("mummichog_pathway_enrichment_mummichog.csv")) file.remove("mummichog_pathway_enrichment_mummichog.csv")
  if(file.exists(mSetObj$mum_nm_csv)) file.remove(mSetObj$mum_nm_csv)


  if(file.exists("mummichog_matched_compound_all.csv")) file.remove("mummichog_matched_compound_all.csv")
  if(file.exists("mummicho_pathway_enrichment_integ.csv")) file.remove("mmummicho_pathway_enrichment_integ.csv")
  if(file.exists("mummichog_pathway_enrichment_gsea.csv")) file.remove("mummichog_pathway_enrichment_gsea.csv")
  if(file.exists("ms_peaks_meta_anal_path_res.json")) file.remove("ms_peaks_meta_anal_path_res.json")
  if(file.exists("initial_ecs.qs")) file.remove("initial_ecs.qs")

  pathResults <- list();
  pathResultsWhole <- list();
  anal.type0 <- mSetObj$paramSet$anal.type;
  
  if(metaLevel == "pathway"){
    
    # now save compound matching results
    # and which compounds are significant - note if GSEA it's all
    cpdMatchResults <- list()

    for(i in 1:length(metaFiles)){
      #mSetObj <- qs::qread(metaFiles[i]);
      mSetObj0 <- FormatmSet(mSetObj, metaFiles[i])
      if(!is.null(mSetObj0$dataSet$adduct.custom)){
        mSetObj0 <- AdductMapping(mSetObj0);
      }

      mSetObj0 <- .setup.psea.library(mSetObj0, lib, libVersion, minLib);
      cpdMatchResults[[metaFiles[i]]] <- qs::qread("mum_res.qs")
     
      # don't need to write path result CSV files for meta-analysis in indiv' runs
      # need to write each individual compound matching file with their own name
      if(mSetObj0$paramSet$mumRT & version=="v2"){
        mSetObj0 <- .init.RT.Permutations(mSetObj0, permNum)
      }else{
        mSetObj0 <- .init.Permutations(mSetObj0, permNum)
      }

      if(anal.type0 == "mummichog"){
        pathResults[[metaFiles[i]]] <- mSetObj0$mummi.resmat
        pathResultsWhole[[metaFiles[i]]] <- read.csv(mSetObj$mum_nm_csv)
      }else if(anal.type0 == "gsea_peaks"){
        pathResults[[metaFiles[i]]] <- mSetObj0$mummi.gsea.resmat
      }else{ # integ
        pathResults[[metaFiles[i]]] <- mSetObj0$integ.resmat
        pathResultsWhole[[metaFiles[i]]] <- read.csv(mSetObj$mum_nm_csv)
      }
    
      if(i != length(metaFiles)){
        rm(mSetObj0)
      }
    }

    sink("ms_peaks_meta_anal_cpd_matching.json");
    cat(rjson::toJSON(cpdMatchResults));
    sink();
    
  } else { 
    
    # finally for pooled
    # this method will not handle RT (empirical cpd level)
    metaMsetObj <- vector("list")
    adduct.list <- list();
    
    for(metafile in seq_along(metaFiles)){
      #mSetObj <- qs::qread(metaFiles[metafile]);
      mSetObj0 <- FormatmSet(mSetObj, metaFiles[metafile]);
      
      if(!is.null(mSetObj0$dataSet$adduct.custom)){
        mSetObj0 <- AdductMapping(mSetObj0);
        qs::qsave(mSetObj0, file = metaFiles[metafile]);
      }
      
      metafile <- metaFiles[metafile];
      metaMsetObj[[metafile]]$dat <- mSetObj0$dataSet$mummi.proc;
      metaMsetObj[[metafile]]$pos_inx <- mSetObj0$dataSet$pos_inx;
      metaMsetObj[[metafile]]$ins_tol <- mSetObj0$dataSet$instrument;
      adduct.list[[metafile]] <- metaMsetObj[[metafile]]$adducts <- mSetObj0$dataSet$adduct.list
    }
    
    # first check that all instrument tol are equal
    ins_tol <- unique(unlist(lapply(metaMsetObj, "[[", "ins_tol")))
    
    # if different instrument tols
    # do individual putative compound annotation
    if((length(ins_tol) > 1) || (length(unique(adduct.list)) > 1)){
      
      mSetObj$mum_nm <- "mummichog_query_mummichog.json"
      mSetObj$mum_nm_csv <- "mummichog_pathway_enrichment_mummichog.csv"
      
      if(version == "v2"){
        mSetObj$paramSet$mumRT <- TRUE
        mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, TRUE, "ec", 
                                       combine.level, pval.method, es.method, rank.metric, FALSE)
        mSetObj <- .init.RT.Permutations(mSetObj, permNum)
      }else{
        mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, TRUE, "cpd", 
                                       combine.level, pval.method, es.method, rank.metric, FALSE)
        mSetObj <- .init.Permutations(mSetObj, permNum)
      }
      
      #mSetObj$cmdSet <- CMDSet;
      return(.set.mSet(mSetObj));
    } else {
      mSetObj$dataSet$instrument <- ins_tol;
    }
   
    metadat <- lapply(metaMsetObj, "[[", "dat")
    metadat <- lapply(metadat, data.frame)
    metadat <- data.table::rbindlist(metadat, idcol = TRUE)
    metadat$m.z <- make.unique(as.character(metadat$m.z), sep = "")
    
    ref_mzlist <- as.numeric(unlist(metadat[,"m.z"]))
    pos_inx <- unlist(metadat[,"pos_inx"])
    pos_inx <- ifelse(pos_inx == 1, TRUE, FALSE)
    expr_dic <- unlist(metadat[,"t.score"])
    ret_time <- as.numeric(unlist(metadat[,"r.t"]))
    
    if(anal.type0 == "mummichog"){
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
    
    mSetObj$dataSet$mummi.proc <- metadat;
    mSetObj$dataSet$ref_mzlist <- ref_mzlist;
    mSetObj$dataSet$pos_inx <- pos_inx;
    mSetObj$dataSet$expr_dic <- expr_dic;
    mSetObj$dataSet$ret_time <- ret_time;
    mSetObj$dataSet$primary_ion <- "yes";
    names(mSetObj$dataSet$expr_dic) <- ref_mzlist;
    mSetObj$dataSet$rt_tol <- AverageRTtol(mSetObj);
    mSetObj$dataSet$rt_frac <- AverageRTfrac(mSetObj);
    
    if(version == "v2"){
      mSetObj$paramSet$mumRT <- TRUE
    }
    
    # Need to check the ion mode: positive, negative or mixed? --Author:Zhiqiang Pang
    if(length(unique(pos_inx)) > 1){
      mSetObj$dataSet$mode <- "mixed"
    } else if(unique(pos_inx) == 1){
      mSetObj$dataSet$mode <- "positive"
    } else if(unique(pos_inx) == 0){
      mSetObj$dataSet$mode <- "negative"
    }
    mSetObj$dataSet[["paramSet"]] <- mSetObj[["paramSet"]];
    mSetObj <- .setup.psea.library(mSetObj, lib, libVersion, minLib, FALSE, "pooled");
    
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
    #mSetObj$cmdSet <- CMDSet;
    
    return(.set.mSet(mSetObj));
  }

  if(metaLevel == "pathway"){ # need to integrate pathway results
    
    sink("ms_peaks_meta_anal_path_res.json");
    
    if(anal.type0!="gsea_peaks"){
      cat(rjson::toJSON(pathResultsWhole));
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
    path_full <- path2 <- data.table::setDF(do.call("cbind", 
                                                    lapply(path.intersected, 
                                                           data.table::data.table, 
                                                           keep.rownames = TRUE, 
                                                           key = "rn")))

    # now do the integration
    # extract the p-values from each option into path2
    # first if just mummichog
    if(anal.type0=="mummichog"){
      
      if(pval.method == "fisher"){
        path2_keep <- grep("rn|FET", colnames(path2))
      }else{
        path2_keep <- grep("rn|Gamma", colnames(path2))
      }
      
    } else if (anal.type0=="gsea_peaks") { # second if just gsea
      path2_keep <- grep("rn|P_val", colnames(path2))
    } else { # third if both
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
      meta.pvals <- apply(as.matrix(path2), 1, function(x) sumlog(x))
    } else if(pval.method=="edgington"){ 
      meta.pvals <- apply(as.matrix(path2), 1, function(x) sump(x))
    } else if(pval.method=="stouffer"){
      meta.pvals <- apply(as.matrix(path2), 1, function(x) sumz(x))
    } else if(pval.method=="vote"){
      meta.pvals <- apply(as.matrix(path2), 1, function(x) votep(x))
    } else if(pval.method=="min"){
      Meta.P <- apply(as.matrix(path2), 1, function(x) min(x))
    } else if(pval.method=="max") {
      Meta.P <- apply(as.matrix(path2), 1, function(x) max(x))
    } else{
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
    col2_rm <- grep(".csv.rn", colnames(path_full))
    path_full <- path_full[,-col2_rm]
    path_full <- path_full[order(path_full$Meta.P),]
    fast.write.csv(path_full, "mspeaks_meta_anal_all_results.csv", row.names = TRUE)
    
  } else {
    AddErrMsg("Invalid meta-analysis level selected!")
    return(0)
  }

  #mSetObj$cmdSet <- CMDSet;
  
  return(.set.mSet(mSetObj));
}

############### Function for visualization of MS Peaks to Paths Meta-Analysis #######################

#' PlotPathwayMetaAnalysis
#'
#' @description Function to create summary plot of MS Peaks to Paths
#' meta-analysis at the pathway level. This function creates a summary plot of the
#' MS Peaks to Paths meta-analysis at the pathway level. The plot
#' can either be a heatmap or a network, both of which can
#' be made interactive. 
#' NETWORK: The size of the nodes in the network correspond to the number of
#' studies in which that pathway was significant. The color of the nodes correspond
#' to the meta-p-value for each pathway, with (default coloring) red being the most 
#' significant and yellow the least. 
#' @param mSetObj Input the name of the created mSetObj object. 
#' @param plotType Use "heatmap" to create a heatmap summary, "network" to create 
#' a network summary, or "bubble" to create a bubble plot summary of the meta-analysis
#' results.
#' @param imgName name of image.
#' @param heatmap_colorType Character, "brewer" for R Color Brewer scales or
#' "viridis" for viridis color scales. Used for creating the heatmap
#' color scheme.
#' @param heatmap_palette Character, input the preferred color palette according
#' to R Color Brewer or viridis (e.g. "RdBu").
#' @param heatmap_interactive Boolean. FALSE to create a non-interactive plot
#' and TRUE for plotly generated interactive plot.
#' @param heatmap_square Boolean. TRUE for the heatmap to be squares versus
#' rectangles (FALSE).
#' @param heatmap_allPaths Boolean. TRUE to use all paths when plotting the heatmap.
#' FALSE to use a subset of paths, number defined in npaths.
#' @param heatmap_npaths Numeric. The number of pathways to subset the pathway
#' results.
#' @param heatmap_vertical Boolean. TRUE, heatmap plot will be vertical. FALSE, heatmap plot
#' will be horizontal.
#' @param heatmap_fontSize Numeric, input the preferred font size to be used in the heatmap
#' plot.
#' @param pvalCutoff The size of the nodes in the network correspond to the number of
#' studies in which that pathway was significant. This pvalCutoff (Numeric) is thus used
#' to determine whether or not a pathway was found to be significant in each
#' individual study. 
#' @param overlap Numeric, this number is used to create edges between the nodes.
#' By default it is set to 0.25, meaning that if 2 pathways (nodes) share 25% of
#' the same compounds/empirical compounds, they will be connected by a node.
#' @param networkType Character, "static" to create a static image or
#' "interactive" to create an interactive network saved as an html 
#' in your working directory.
#' @param layout Character, layout from ggraph. "kk" for the spring-based algorithm by Kamada and Kawai
#' as default. "drl" for force directed algorithm from the DrL toolbox. "lgl" for Large Graph Layout. "fr" for
#' force-directed of Fruchterman and Reingold.
#' @param net_palette Character, input the color code for the nodes in the network. Default is
#' "YlOrRd". Uses the hcl palettes from the grDevices. Use hcl.pals()
#' to view the name of all available palettes.
#' @param netTextSize Numeric, input the preferred font size to be used in the network
#' plot.
#' @param netPlotSize Numeric, input the preferred dimensions (in inches) of the network
#' to be saved.
#' @param bubble_colorType  Character, "brewer" for R Color Brewer scales or 
#' "viridis" for viridis color scales. Used for creating the bubble plot
#' color scheme.
#' @param bubble_palette Character, use two/three colors max if using R ColorBrewer palettes
#' for pleasing looking plots.
#' @param bubble_interactive logical
#' @param bubbleMaxPaths maximum number of pathways
#' @param bubbleFontSize font size of the bubble plot
#' @param bubblePlotSize plot size of the bubble plot
#' @param format format of the image
#' @param width width of the image
#' @param height height of the image
#' @param dpi dpi of the image
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

PlotPathwayMetaAnalysis <- function(mSetObj = NA, imgName, plotType = "heatmap", 
                                    heatmap_colorType = "brewer", heatmap_palette = "RdYlBu",
                                    heatmap_interactive = FALSE, heatmap_square = TRUE,
                                    heatmap_allPaths = TRUE, heatmap_npaths = 25, heatmap_vertical = TRUE,
                                    heatmap_fontSize = 9, pvalCutoff = 0.05, overlap = 0.25,
                                    networkType = "static", layout="kk", net_palette = "YlOrRd",
                                    netTextSize = 2.5, netPlotSize = 7.5, 
                                    bubble_interactive = FALSE, bubbleMaxPaths = 15,
                                    bubble_colorType = "brewer", bubble_palette = "RdBu",
                                    bubbleFontSize = 9, bubblePlotSize = 7,
                                    format = "png", width = 7, height = 5, dpi = 300){
  
  mSetObj <- .get.mSet(mSetObj);
  metaLevel <- mSetObj$metaLevel;

  anal.type0 <- mSet[["paramSet"]][["anal.type"]]
  
  if(metaLevel != "pathway"){
    AddErrMsg("Function only for pathway-level meta-analysis!")
    return(0)
  }
  
  path_results <- mSetObj$meta_results

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
      #load_reshape()
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
      cat(rjson::toJSON(netData));
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
        
        filename <- paste0(anal.type0, "_network", ".png")
        ggsave(p, filename=filename, width = netPlotSize, height = netPlotSize)
        
      }else{ 
        # interactive plot
        library(visNetwork)
        data <- toVisNetworkData(g)
        network <- visNetwork(nodes = data$nodes, edges = data$edges, 
                              idToLabel = TRUE, height = "900px", width = "100%") %>% visEdges(color=list(color="grey", highlight="red")) %>% visNodes(font = list(size = 30))
        
        filename <- paste0(anal.type0, "_network", ".html")
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
    
    if(anal.type0=="gsea_peaks"){
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".csv.Pathway_Total")
      studies_sig_hits <- paste0(studies, ".csv.Hits")
      
    }else if(anal.type0=="mummichog"){
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".csv.Pathway.total")
      studies_sig_hits <- paste0(studies, ".csv.Hits.sig")
      
    }else{ #integ
      
      studies <- colnames(path_results)[-length(colnames(path_results))] # remove the Meta.P
      studies_pathway_total <- paste0(studies, ".csv.Total_Size")
      studies_sig_hits <- paste0(studies, ".csv.Sig_Hits")
      
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
    ratio_mean <- rowMeans(ratio);
    ratio <- cbind(ratio, ratio_mean);
    
    ratio_String <- cbind(ratio_String, format(round(ratio_mean, 4)*100, nsmall = 2));
    
    colnames(ratio) <- c(studies,"Meta.P");
    #colnames(ratio_String) <- c(paste0(sub("mummichoginput", "",studies),"_Pvalue [Sig/Total]"), "Mean_Enrich(%)");
    colnames(ratio_String) <- c(paste0(sub("mummichoginput", "",studies),"_Pvalue"), "Mean_Enrich(%)");
    ratio$Pathway <- rownames(path_results);
    
    ratio_String <- cbind(rownames(path_results), ratio_String);
    
    ratio2 <- melt(ratio, id.vars = "Pathway", variable.name = "Study", value.name = "enrichment ratio");
    
    #path_results <- path_results[, -length(colnames(path_results))];
    path_results$Pathway <- rownames(path_results);
    path_results2 <- melt(path_results, id.vars = "Pathway", variable.name = "Study", value.name = "p-value");
    
    res_table <- cbind(ratio_String, path_results$Meta.P);
    colnames(res_table)[c(1, length(colnames(res_table)))] <- c("Pathways", "Meta.P")
    
    res_table <- res_table[order(res_table$Meta.P, -as.numeric(res_table$Mean_Enrich)),];

    if(all(res_table$Mean_Enrich == "100.00")){
      res_table <- res_table[,-which(colnames(res_table) == "Mean_Enrich(%)")]
    }
    
    mSetObj$dataSet$res_table <- res_table;
    
    require(ggplot2);
    require(RColorBrewer);
    
    ## This oder matrix is used to order the metap (first by P value, second by enrichment ratio)
    order_matrix0 <- merge(path_results, ratio, by = c("Pathway"));
    ColNMs <- c("Pathway", paste0(sub("mummichoginput", "",studies),"p-value"),"Meta.P",paste0(sub("mummichoginput", "",studies),"Enrich"),"Average_Enrich")
    
    order_matrix <- cbind(order_matrix0$Pathway, order_matrix0$Meta.P.x, order_matrix0$Meta.P.y);
    metap_order <- order_matrix[order(order_matrix[,2], -as.numeric(order_matrix[,3]))]
    
    colnames(order_matrix0) <- ColNMs;
    fast.write.csv(order_matrix0, file = "Result_summary.csv",row.names = FALSE)
    
    df <- merge(path_results2, ratio2, by = c("Pathway", "Study"))
    df$Pathway <- factor(df$Pathway, levels = rownames(path_results))
    df$Study <- sub("mummichoginput", "", df$Study);
    df$Study <- as.factor(df$Study);
    
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
    
    if(missing(width)) {
        width <- bubblePlotSize;
    }

    if(missing(height)) {
        height <- bubblePlotSize*0.6;
    }    
    
    if(missing(format)) {
        format <- "png";
    }

    if(missing(dpi)) {
        dpi <- 300;
    }

    # filename <- paste0(imgName, ".png")
    # filename <- paste0(imgName, width,"_", dpi,".", format)
    filename <- paste0(imgName, "dpi", dpi,".", format)

    ggsave(p, 
           filename=filename,
           device= format,
           width = width, 
           height = height,
           dpi = dpi)
    
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

  if(.on.public.web) {
    .set.mSet(mSetObj);
    return(filename);
  } else {
    print(filename)
    return(.set.mSet(mSetObj));
  }
  
}


## R functions used to define the adducts for met-analysis
Customize.MetaAdduct <- function(mSet, name, name2, qvec, mode){
  mSetObj <- .get.mSet(mSet)
  # fileNM <- tools::file_path_sans_ext(basename(name));
  # fileNM <- gsub("_", "", fileNM);
  #
  # if(name2 != "null"){
  #   fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
  #   filename <- paste0(fileNM, "mixedmummichoginput.qs");
  # } else {
  #   fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
  #   filename <- paste0(fileNM, "mummichoginput.qs");
  # }
  #
  # mSetObj <- qs::qread(filename); 
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];

  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == name){
      mSetObj[[nm]]$mode <- mode;
      mSetObj[[nm]]$adduct.list <- qvec;
      mSetObj[[nm]]$adduct.custom <- TRUE;
    }

    if(mSetObj[[nm]]$name == name2){
      mSetObj[[nm]]$mode <- mode;
      mSetObj[[nm]]$adduct.list <- qvec;
      mSetObj[[nm]]$adduct.custom <- TRUE;
    }
  }
  
  # mSetObj$dataSet$mode <- mode;
  # mSetObj$dataSet$adduct.list <- qvec;
  # mSetObj$adduct.custom <- TRUE;
  # mSetObj$dataSet$fileName <- fileNM;
  
  # qs::qsave(mSetObj, file = filename);
  # return(1);
  return(.set.mSet(mSetObj))
}

AdductMapping <- function(mSetObj){
  
  adducts <- mSetObj$dataSet$adduct.list;
  add.mode <- mSetObj[["dataSet"]][["mode"]];
  
  if(add.mode == "positive"){
    add_db <- .get.my.lib("pos_adduct.qs");
  }else if(add.mode == "negative"){
    add_db <- .get.my.lib("neg_adduct.qs");
  }else if(add.mode == "mixed"){
    add_db <- .get.my.lib("mixed_adduct.qs");
  }else{
    msg <- c("Adduct mode is not valid")
  }
  
  hit.inx <- match(tolower(adducts), tolower(add_db$Ion_Name));  
  hits <- length(na.omit(hit.inx))
  
  if(hits == 0){
    mSetObj$mummi$add.msg <- c("No adducts were selected!");
    return(0)
  }
  
  match.values <- add_db[na.omit(hit.inx),];
  sel.add <- nrow(match.values);
  
  if(sel.add > 0){
    mSetObj$mummi$add.msg <- paste("A total of ", sel.add ," adducts were successfully selected!", sep = "")
  }
  
  mSetObj$dataSet$adduct.custom <- TRUE;
  mSetObj$dataSet$add.map <- match.values;
  
  return(mSetObj);
}

#' PrepareMetaPath
#'
#' @param mSetObj mSetObj
#' @param mode ion mode, can be "positive" or "negative"
#' @param ppm mass error, default is 30
#' @param version mummichog version, can be "v1" or "v2"
#' @param pcutoff p value cut-off, default is 0.05
#' @param rt.type character, retention time type, can be "minutes" or "seconds"
#' @param dataName file name 1 with absolute path
#' @param dataName2 file name 2 with absolute path or "null"
#' @export
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
PrepareMetaPath <- function(mSetObj = NA, 
                            mode = "negative", 
                            ppm = 30, 
                            version = "v2", 
                            pcutoff = 0.05, 
                            rt.type = "seconds", 
                            dataName, 
                            dataName2) {

  mSetObj <- .get.mSet(mSetObj);

  
  if(dataName2 == "null"){dataName2 = NULL}
  fileTitle <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName));
  fileTitle2 <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName2));
  
  if(rt.type == "false" | rt.type == "no" | rt.type == "F"){
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
  
  rt.studies <<- c(rt.studies, rt);
  
  ## if(length(mSetObj$dataSet2) == 0) {
  if(is.null(dataName2)){
    
    file.copy(from = paste0(fileTitle, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];

    dataSet <- FetchDataSet(mSetObj, dataName);
    mSetObj$dataSet <- UniqueDataSet(c(mSetObj[["dataSet"]], dataSet));
    # Here is dealing with the single ion mode data
    mSet <<- mSetObj
    mSetObj <- Ttests.Anal(mSetObj, F, pcutoff, FALSE, TRUE);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- Convert2MummichogMetaPath(mSetObj, rt, rds.file=FALSE, rt.type, "all", mode);
    mSetObj <- .get.mSet(mSetObj);
    fileNM <- mSetObj$dataSet$name;
    fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
    filename <- paste0(fileNM, "_mummichog_input.txt");
    
    mSetObj <- .get.mSet(mSetObj);
    mummi_new <- mSetObj$dataSet$mummi_new;
    write.table(mummi_new, filename, row.names = FALSE);
    
    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);
    mSetObj$dataSet <- CleanDataSet;
    mSet <<- mSetObj
    dataSet <- FetchDataSet(mSetObj, dataName);
    
  } else {
    
    # Here is dealing with the mix ion modes' data
    # 1st step <- positive mode
    file.copy(from = paste0(fileTitle, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];
    dataSet <- FetchDataSet(mSetObj, dataName);
    mSetObj$dataSet <- UniqueDataSet(c(mSetObj[["dataSet"]], dataSet));
    # Here is dealing with the single ion mode data    
    mSet <<- mSetObj
    mSetObj <- Ttests.Anal(mSetObj, F, pcutoff, FALSE, TRUE);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- Convert2MummichogMetaPath(mSetObj, 
                                         rt, rds.file=FALSE, 
                                         rt.type, "all", "positive");
    mSetObj <- .get.mSet(mSetObj);

    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);
    mSetObj$dataSet <- CleanDataSet;
    mSet <<- mSetObj;
    dataset_pos <- FetchDataSet(mSetObj, dataName);
    
    # 2nd step <- negative mode
    file.copy(from = paste0(fileTitle2, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];
    dataSet <- FetchDataSet(mSetObj, dataName2);
    mSetObj$dataSet <- UniqueDataSet(c(mSetObj[["dataSet"]], dataSet));
    # Here is dealing with the single ion mode data
    mSet <<- mSetObj
    mSetObj <- Ttests.Anal(mSetObj, F, pcutoff, FALSE, TRUE);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- Convert2MummichogMetaPath(mSetObj, 
                                         rt, rds.file=FALSE, 
                                         rt.type, "all", "negative");
    mSetObj <- .get.mSet(mSetObj);
    fileNM <- mSetObj$dataSet$name;
    fileNM <- gsub("\\.[^.]*$", "", basename(fileNM));
    filename <- paste0(fileNM, "_mixed_mummichog_input.txt");

    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);
    mSetObj$dataSet <- CleanDataSet;
    mSet <<- mSetObj
    dataset_neg <- FetchDataSet(mSetObj, dataName2);
    
    # Merge and save them
    mtbl_all <- rbind(dataset_pos$mummi_new, dataset_neg$mummi_new[-1,]) 
    write.table(mtbl_all, filename, row.names = FALSE, col.names = TRUE)
    
    # rm(mSetObj1)
    # rm(mSetObj2)
  }
  mSetObj<-defineVersion(mSetObj,dataName,dataName2,version)
  mSet <<- mSetObj;

  mSetObj<-SetPeakFormat(mSetObj, "mpt");
  mSetObj<-UpdateInstrumentParameters(mSetObj, ppm, mode);
  mSetObj<-Read.PeakListData(mSetObj, filename, meta.anal=TRUE, method="both");
  mSetObj<-SanityCheckMummichogData(mSetObj);
  
  if(.on.public.web){
    res <- SetMummichogPval(mSetObj, pcutoff);
  }else{
    mSetObj <- SetMummichogPval(mSetObj, pcutoff);
  }
  mSetObj <- .get.mSet(mSetObj);
  mSetObj<-UpgradeDataSet(mSetObj, 
                          mSetObj$dataSet, 
                          dataName);
  
  mSetObj$dataSet <- CleanDataSet;
  mSet <<- mSetObj
  
  if(.on.public.web){
    # mSetObj <- InitDataObjects("conc", "metapaths", FALSE)
    # mSet$cmdSet <<- CMDSet;
    return(res)
  } else {
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

#' ReadMetaPathTable
#'
#' @param mSetObj mSetObj
#' @param dataNM file name with absolute path
#' @param dataFormat data format, can be colu or rowu
#' @param dataType data type, usually "massPeaks"
#' @export
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
ReadMetaPathTable <- function(mSetObj = NA,  dataNM, dataFormat, dataType) {
  
  if(file.exists("data_orig.qs")) file.remove("data_orig.qs");
  if(file.exists("data_orig1.qs")) file.remove("data_orig1.qs");
  if(file.exists("data_orig2.qs")) file.remove("data_orig2.qs");
  if(file.exists("mSet.qs")) file.remove("mSet.qs");
  if(file.exists("prenorm.qs")) file.remove("prenorm.qs");
  if(file.exists("preproc.qs")) file.remove("preproc.qs")

  mSetObj <- .get.mSet(mSetObj);

  if(dataType == "massPeaks"){

    # Handle mass peaks for mummichog
    mSetObj <- Read.TextData(mSetObj, dataNM, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
  
    fileDataNM <- sub(pattern = "(.*)\\..*$", replacement = "\\1", dataNM);
    file.rename(from = "data_orig.qs", 
                to = paste0(fileDataNM, "_orig.qs"));

    MetaData <- list();
    MetaData$cls <- mSetObj$dataSet$cls;
    MetaData$orig.cls <- mSetObj$dataSet$orig.cls;
    MetaData$cmpd <- mSetObj$dataSet$cmpd;
    MetaData$url.var.nms <- mSetObj$dataSet$url.var.nms;
    MetaData$url.smp.nms <- mSetObj$dataSet$url.smp.nms;

    MetaData$name <- dataNM;
    MetaData$orig.data <- paste0(fileDataNM, "_orig.qs");
    MetaData$format <- dataFormat;
    MetaData$datamode <- "single";
    MetaData$paramSet <- mSetObj$paramSet;

    #mSetObj$paramSet$metaNum <- mSetObj$paramSet$metaNum + 1;
    #metadataNM <- paste0("MetaData",mSetObj$paramSet$metaNum);
    if(newDataset(mSetObj, dataNM)){
      mSetObj$paramSet$metaNum <- mSetObj$paramSet$metaNum + 1;
      metadataNM <- paste0("MetaData",mSetObj$paramSet$metaNum);
    } else {
      dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
      for(nm in dataNMs){
        if(mSetObj[[nm]]$name == dataNM){
          metadataNM <- nm;
        }
      }
    }

    mSetObj[[metadataNM]] <- MetaData;
    
    mSetObj$dataSet$cls <- 
      mSetObj$dataSet$orig.cls <- 
      mSetObj$dataSet$cmpd <- 
      mSetObj$dataSet$url.var.nms <- 
      mSetObj$dataSet$url.smp.nms <- 
      MetaData <- NULL;
    

    #fileTitle <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dataNM));
    #file.copy(from = "data_orig.qs", to= paste0(fileTitle, "_orig.qs"));
    #mSetObj <- .get.mSet(mSetObj);
    #mSetObj$dataSet$name <- dataNM;
    #mSetObj$dataSet$format <- dataFormat;

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

#' ReadMetaPathTableMix
#'
#' @param mSetObj mSetObj
#' @param dataNM file name 1 with absolute path (should be ESI+)
#' @param dataNM2 file name 2 with absolute path (should be ESI-)
#' @param dataFormat data format, can be colu or rowu
#' @param dataType data type, usually "massPeaks"
#' @export
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
ReadMetaPathTableMix <- function(mSetObj = NA,  dataNM, dataNM2, dataFormat, dataType) {
 
  if(file.exists("data_orig.qs")) file.remove("data_orig.qs");
  if(file.exists("data_orig1.qs")) file.remove("data_orig1.qs");
  if(file.exists("data_orig2.qs")) file.remove("data_orig2.qs");
  if(file.exists("mSet.qs")) file.remove("mSet.qs");
  if(file.exists("prenorm.qs")) file.remove("prenorm.qs");
  if(file.exists("preproc.qs")) file.remove("preproc.qs");

  mSetObj <- .get.mSet(mSetObj);
  
  if(dataType == "massPeaks"){
    # Handle mass peaks for mummichog - DATA 1
    mSetObj <- Read.TextData(mSetObj, dataNM, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
    fileDataNM <- sub(pattern = "(.*)\\..*$", replacement = "\\1", dataNM);
    file.rename(from = "data_orig.qs", 
                to = paste0(fileDataNM, "_orig.qs"));
    
    MetaData <- list();
    MetaData$cls <- mSetObj$dataSet$cls;
    MetaData$orig.cls <- mSetObj$dataSet$orig.cls;
    MetaData$cmpd <- mSetObj$dataSet$cmpd;
    MetaData$url.var.nms <- mSetObj$dataSet$url.var.nms;
    MetaData$url.smp.nms <- mSetObj$dataSet$url.smp.nms;
    
    MetaData$name <- dataNM;
    MetaData$orig.data <- paste0(fileDataNM, "_orig.qs");
    MetaData$format <- dataFormat;
    MetaData$datamode <- "paired";
    MetaData$paramSet <- mSetObj$paramSet;
    
    if(newDataset(mSetObj, dataNM, dataNM2)){
      mSetObj$paramSet$metaNum <- mSetObj$paramSet$metaNum + 1;
      metadataNM <- paste0("MetaData",mSetObj$paramSet$metaNum);
      newDS <- TRUE;
    } else {
      dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
      for(nm in dataNMs){
        if(mSetObj[[nm]]$name == dataNM){
          metadataNM <- nm;
        }
      }
      newDS <- FALSE;
    }
    
    mSetObj[[metadataNM]] <- MetaData;
    
    mSetObj$dataSet$cls <- 
      mSetObj$dataSet$orig.cls <- 
      mSetObj$dataSet$cmpd <- 
      mSetObj$dataSet$url.var.nms <- 
      mSetObj$dataSet$url.smp.nms <- 
      MetaData <- NULL;
    mSet <<- mSetObj;

    # Handle mass peaks for mummichog - DATA 2
    mSetObj <- Read.TextData(mSetObj, dataNM2, dataFormat, "disc");
    mSetObj <- .get.mSet(mSetObj);
    fileDataNM2 <- sub(pattern = "(.*)\\..*$", replacement = "\\1", dataNM2);
    file.rename(from = "data_orig.qs", 
                to = paste0(fileDataNM2, "_orig.qs"));
    
    MetaData <- list();
    MetaData$cls <- mSetObj$dataSet$cls;
    MetaData$orig.cls <- mSetObj$dataSet$orig.cls;
    MetaData$cmpd <- mSetObj$dataSet$cmpd;
    MetaData$url.var.nms <- mSetObj$dataSet$url.var.nms;
    MetaData$url.smp.nms <- mSetObj$dataSet$url.smp.nms;
    
    MetaData$name <- dataNM2;
    MetaData$orig.data <- paste0(fileDataNM2, "_orig.qs");
    MetaData$format <- dataFormat;
    MetaData$datamode <- "paired";
    MetaData$paramSet <- mSetObj$paramSet;
    
    if(newDS){
      metadataNM <- paste0("MetaData",mSetObj$paramSet$metaNum,"_2");
    } else {
      metadataNM <- paste0(metadataNM, "_2")
    }
    
    mSetObj[[metadataNM]] <- MetaData;
    
    mSetObj$dataSet$cls <- 
      mSetObj$dataSet$orig.cls <- 
      mSetObj$dataSet$cmpd <- 
      mSetObj$dataSet$url.var.nms <- 
      mSetObj$dataSet$url.smp.nms <- 
      MetaData <- NULL;
 
  } else if (dataType == "annoPeaks") {
    # Handle annotated compounds for QEA pathway
    # TODO: add more later
  } else {
    return (0);
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return (1)
  }else{
    return(.set.mSet(mSetObj));
  }
}

#' SanityCheckMetaPathTable
#'
#' @param mSetObj mSetObj
#' @param dataName file name 1 with absolute path
#' @param dataName2 file name 2 with absolute path or "null"
#' @export
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
SanityCheckMetaPathTable<-function(mSetObj=NA, dataName, dataName2){
  
  mSetObj <- .get.mSet(mSetObj);

  if(dataName2 == "null"){dataName2 = NULL}
  fileTitle <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName));
  fileTitle2 <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName2));
  # mSetObjNM <- paste0(fileTitle, "_" ,fileTitle2, "_mSetObj.qs");
  dataSet <- FetchDataSet(mSetObj, dataName)
  dataSet2 <- FetchDataSet(mSetObj, dataName2)
  
  if(is.null(dataSet2)){
    data_oriNM <- dataSet$orig.data
  } else {
    data_oriNM <- c(dataSet$orig.data, dataSet2$orig.data)
  }
  msg <- NULL;
  
  for(i in seq(data_oriNM)){

    conc <- qs::qread(data_oriNM[i]);
    
    if(i == 2){
      # dataSet <- mSetObj[[paste0("dataSet",i)]];
      dataSet <- dataSet2;
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
      if(i == 1){nn = NULL}else{nn = 2}
      msg <- c(msg, paste0("No empty rows were found in this data: ", mSetObj[[paste0("dataSet", nn)]][["name"]]));
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
      if(i == 1){nn = NULL}else{nn = 2}
      msg <- c(msg, paste0("No empty labels were found in this data: ", mSetObj[[paste0("dataSet", nn)]][["name"]]));
    }
    
    if(length(unique(cls.lbl[!empty.inx])) > 2){
      msg <- c(msg, paste(c("Groups found:", unique(cls.lbl[!empty.inx])), collapse=" "));
      msg <- c(msg, "<font color=\"red\">Meta-analysis is only defined for two-group comparisions!</font>");
      AddErrMsg(msg);
      return(0);
    }else{
      lvls <- as.character(unique(unlist(cls.lbl)))
      msg <- c(msg, paste("Two groups found:", lvls[1], "and", lvls[2], collapse=" "));
    }
    
    # check for uniqueness of dimension name
    if(length(unique(smpl.nms))!=length(smpl.nms)){
      dup.nm <- paste(smpl.nms[duplicated(smpl.nms)], collapse=" ");
      msg <- c(msg, "Duplicate sample names are not allowed!");
      AddErrMsg(msg);
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
      return(0);
    }else{
      msg <- c(msg, "All feature names are unique");
    }
    
    # now check for special characters in the data labels
    if(sum(is.na(iconv(smpl.nms)))>0){
      na.inx <- is.na(iconv(smpl.nms));
      nms <- paste(smpl.nms[na.inx], collapse="; ");
      msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in sample names!", nms, collapse=" "));
      AddErrMsg(msg);
      return(0);
    }else{
      msg <- c(msg, "All sample names are OK");
    }
    
    if(sum(is.na(iconv(var.nms)))>0){
      na.inx <- is.na(iconv(var.nms));
      nms <- paste(var.nms[na.inx], collapse="; ");
      msg <- c(msg, paste("No special letters (i.e. Latin, Greek) are allowed in feature names!", nms, collapse=" "));
      AddErrMsg(msg);
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
        AddErrMsg(msg);
        return(0);
      }
      
      # update meta information
      proc.cls <- proc.cls[good.inx];
    }
    
    if(ncol(data) < 4){
      msg <- c(msg, paste("The sample # (", nrow(data), ") is too small."));
      AddErrMsg(msg);
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
        AddErrMsg(msg);
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
        AddErrMsg(msg);
        return(0);
      }
    }
    
    if(ncol(data) < 10){
      msg <- c(msg, "The feature# (", ncol(data), ") is too small (<10).");
      AddErrMsg(msg);
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
    
    mSetObj <- UpdateDataSet(mSetObj, dataSet);    
    #RegisterData(mSetObj, dataSet)
    if(i ==1 && length(data_oriNM) > 1){
        msg <- c(msg, paste("----------------------------------"));
    }
  } # end of the for loop
  
  # Collect all msg here 
  mSetObj$dataSet$check.msg <- msg; #may consider msg missing
  # qs::qsave(mSetObj, file = mSetObjNM);

  if(.on.public.web){
    .set.mSet(mSetObj);
    return(1);
  } else {
    #RegisterData(mSetObj, dataSet)
    return(.set.mSet(mSetObj));
  }
  
}

FetchDataSet <- function(mSetObj, dataName) {
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  if(is.null(dataName)) return(NULL)
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      return(mSetObj[[nm]])
    }
  }
  return(NULL)
}

UpdateDataSet <- function(mSetObj, dataSet) {
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataSet$name){
      mSetObj[[nm]] <- dataSet
    }
  }
  return(mSetObj)
}

UniqueDataSet <- function(dataSet) {
  dataItems <- unique(names(dataSet));
  newdataSet <- list();
  for(i in dataItems) {
    newdataSet[[i]] <- dataSet[[i]];
  }
  return(newdataSet)
}

UpgradeDataSet <- function(mSetObj, dataSet, dataName) {
  #Further merge dataset with MetaData list
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      mSetObj[[nm]] <- UniqueDataSet(c(mSetObj[[nm]], dataSet))
    }
  }
  return(mSetObj)
}

ExtractNormTable <- function(mSetObj, dataName) {
  
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      return(mSetObj[[nm]]$norm)
    }
  }
  return(NULL)
  
}

newDataset <- function(mSetObj, dataName, dataName2 = NULL) {
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  cond1 <- cond2 <- FALSE;
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      cond1 <- TRUE;
      if(!is.null(mSetObj[[paste0(nm,"_2")]]$name)){
        if(mSetObj[[paste0(nm,"_2")]]$name == dataName2){
          cond2 <- TRUE;
        }
      }
    }
  }
  if(is.null(dataName2)){
    return(!cond1)
  }
  return(!(cond1 & cond2))
}

FormatmSet <- function(mSetObj, dataName){
  mSetObj0 <- list();
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      mSetObj0$dataSet <- mSetObj[[nm]];
      mSetObj0$paramSet <- mSetObj[["paramSet"]];
      mSetObj0$paramSet$anal.type <- mSetObj$paramSet$anal.type;
      mSetObj0$mum_nm_csv <- mSetObj$mum_nm_csv;
      mSetObj0$mum_nm <- mSetObj$mum_nm;
    }
  }
  
  return(mSetObj0)
}

defineVersion <- function(mSetObj, dataName, dataName2, version){
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      mSetObj[[nm]]$paramSet$version <- version;
      mSetObj[[nm]]$paramSet$mumRT <- TRUE;
    }
    if(!is.null(dataName2)){
      if(mSetObj[[nm]]$name == dataName2){
        mSetObj[[nm]]$paramSet$version <- version;
        mSetObj[[nm]]$paramSet$mumRT <- TRUE;
      }
    }
  }
  return(mSetObj)
}

AverageRTtol <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  rts <- vector()
  for(nm in dataNMs){
    rts <- c(rts, mSetObj[[nm]]$rt_tol)
  }
  return(mean(rts))
}

AverageRTfrac <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  rtfs <- vector()
  for(nm in dataNMs){
    rtfs <- c(rtfs, mSetObj[[nm]]$rt_frac)
  }
  return(mean(rtfs))
}

GetMetaPathSanityCheckMsg <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$dataSet$check.msg);
} 

GetMetaPathDataDims <- function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      data <- mSetObj[[nm]]$data;
    }
  }
  #if(mSetObj$dataSet$name != dataName){
  #  data <- mSetObj$dataSet2$data;
  #} else {
  #  data <- mSetObj$dataSet$data;
  #}
  
  dm <- dim(data);
  naNum <- sum(is.na(data));
  zoNum <- sum(data == 0);
  return(c(dm, naNum, zoNum));
} 

GetMetaPathGroupNames <-function(mSetObj=NA, dataName){
  
  mSetObj <- .get.mSet(mSetObj);
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  
  for(nm in dataNMs){
    if(mSetObj[[nm]]$name == dataName){
      cls <- mSetObj[[nm]]$cls;
    }
  }
  return(levels(cls))
  #if(mSetObj$dataSet$name != dataName){
  #  dataSet <- qs::qread(dataName);
  #}
  #return(levels(mSetObj$dataSet$cls));
}

PlotPathDataProfile<-function(dataName, dataName2= NULL, boxplotName, boxplotName2 =NULL, dataformat){
  #dataSet <- qs::qread(dataName);
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    require(lattice)
  }
  datatable <- mSetObj$dataSet$data;

  if(length(mSetObj[["dataSet"]][["name"]]) > 0) {
    if(mSetObj[["dataSet"]][["name"]] != dataName){
      datatable <- NULL;
    }
  } else {
    datatable <- NULL;
  }
  
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
    datatable[is.na(datatable)] <-0;
  }
  dataName <- tools::file_path_sans_ext(basename(dataName))

  if(!is.null(dataName2) && (dataName2 != "null")){
    if (dataformat == "colu") {
      dt <- t(read.csv(dataName2)[-1, ])
      colnames(dt) <- dt[1, ]
    } else if (dataformat == "rowu") {
      dt <- read.csv(dataName2, header = FALSE);
      rownames(dt) <- dt[, 1]
      dt <- dt[, -c(1, 2)]
      colnames(dt) <- dt[1, ]
      dt <- dt[-1, ]
    }
    datatable2 <- dt;
    datatable2[is.na(datatable2)] <-0;
    dataName2 <- tools::file_path_sans_ext(basename(dataName2));
        
    qc.biBoxPlot(datatable, datatable2, 
                 dataName, dataName2, vertical = TRUE);
  } else {
    qc.boxplot(datatable, paste0(dataName, boxplotName));
  }

}

#' MetaPathNormalization
#'
#' @param mSetObj mSetObj
#' @param sampleNor sample Normalization option
#' @param tranform sample transformation option
#' @param scale sample scale option
#' @param name file name 1 with absolute path
#' @param name2 file name 2 with absolute path or "null"
#' @export
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
MetaPathNormalization <- function(mSetObj = NA, sampleNor, tranform, scale = "NULL", name, name2) {
  mSetObj <- .get.mSet(mSetObj);
  
  if(name2 == "null"){name2 = NULL}
  fileTitle <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (name));
  fileTitle2 <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (name2));
  # mSetObjNM <- paste0(fileTitle, "_" ,fileTitle2, "_mSetObj.qs");
  # mSetNormedNM <- paste0(fileTitle, "_" ,fileTitle2, "_normalized_mSet.qs");
  ## next, when doing the path analysis, will use '*_normalized_mSet.qs'

  # mSet <<- mSetObj <- qs::qread(mSetObjNM);

  # if(length(mSetObj$dataSet2) == 0){
  if(is.null(name2)){
    file.copy(from = paste0(fileTitle, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];
    dataSet <- FetchDataSet(mSetObj, name)
    mSetObj$dataSet <- c(mSetObj[["dataSet"]],dataSet)
    .set.mSet(mSetObj);

    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj, "F", 25, "iqr", NULL);
    mSetObj <- PreparePrenormData(mSetObj);
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, scale, ratio=FALSE, ratioNum=20);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);

    qc.boxplot(as.matrix(mSetObj$dataSet$norm), 
               paste0(tools::file_path_sans_ext(basename(name)), 
                      "_norm_box"));

    mSetObj$dataSet <- CleanDataSet;
    # mSetObj <- .get.mSet(mSetObj);

  } else {

    # 1st step
    file.copy(from = paste0(fileTitle, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];
    dataSet <- FetchDataSet(mSetObj, name);
    mSetObj$dataSet <- c(mSetObj[["dataSet"]],dataSet);
    .set.mSet(mSetObj);

    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj,"F", 25, "iqr", NULL);
    mSetObj <- PreparePrenormData(mSetObj);
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, scale, ratio=FALSE, ratioNum=20);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);
    mSetObj$dataSet <- CleanDataSet;

    # 2nd step
    file.copy(from = paste0(fileTitle2, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    CleanDataSet <- mSetObj[["dataSet"]];
    dataSet <- FetchDataSet(mSetObj, name2);
    mSetObj$dataSet <- c(mSetObj[["dataSet"]],dataSet);
    .set.mSet(mSetObj);
 
    mSetObj <- SanityCheckData(mSetObj);
    mSetObj <- ReplaceMin(mSetObj);
    mSetObj <- FilterVariable(mSetObj, "F", 25, "iqr", NULL);
    mSetObj <- PreparePrenormData(mSetObj);
    mSetObj <- Normalization(mSetObj, sampleNor, tranform, scale, ratio=FALSE, ratioNum=20);
    mSetObj <- .get.mSet(mSetObj);
    mSetObj <- UpdateDataSet(mSetObj, mSetObj$dataSet);
    mSetObj$dataSet <- CleanDataSet;
    mSet <<- mSetObj;

    ## This is used to deal with the mix data files
    # 1st step, process dataset 1 (pos data)
    # file.copy(from = paste0(fileTitle, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    # mSetObj <- SanityCheckData(mSetObj);
    # mSetObj <- ReplaceMin(mSetObj);
    # mSetObj <- FilterVariable(mSetObj, "F", 25, "iqr", NULL)
    # mSetObj <- PreparePrenormData(mSetObj)
    # mSetObj <- Normalization(mSetObj, sampleNor, tranform, scale, ratio=FALSE, ratioNum=20)
    # file.rename("data_orig.qs", "data_orig1.qs");
    # file.rename("complete_norm.qs", "complete_norm1.qs");
    # mSetObj <- .get.mSet(mSetObj);
    # dataset_pos <- mSetObj$dataSet;
    # 
    # 2nd step, process dataset 2 (neg data)
    # file.copy(from = paste0(fileTitle2, "_orig.qs"), to = "data_orig.qs", overwrite = TRUE)
    # mSetObj$dataSet <- mSet$dataSet2;
    # mSetObj <- SanityCheckData(mSetObj);
    # mSetObj <- ReplaceMin(mSetObj);
    # mSetObj <- FilterVariable(mSetObj, "F", 25, "iqr", NULL)
    # mSetObj <- PreparePrenormData(mSetObj)
    # mSetObj <- Normalization(mSetObj, sampleNor, tranform, scale, ratio=FALSE, ratioNum=20)
    # file.rename("data_orig.qs", "data_orig2.qs");
    # file.rename("complete_norm.qs", "complete_norm2.qs");
    # 
    # mSetObj <- .get.mSet(mSetObj);
    # dataset_neg <- mSetObj$dataSet;
    # refine the mSet
    # mSetObj$dataSet <- dataset_pos;
    # mSetObj$dataSet2 <- dataset_neg;

    dt1 <- as.matrix(ExtractNormTable(mSetObj, name));
    dt2 <- as.matrix(ExtractNormTable(mSetObj, name2));

    qc.biBoxPlot(dt1, 
                 dt2, 
                 tools::file_path_sans_ext(basename(name)), 
                 tools::file_path_sans_ext(basename(name2)), 
                 vertical = FALSE);

  }
  
  # print(paste0("--------- mSetNormedNM to save is --->", mSetNormedNM))
  # mSet$cmdSet <<- NULL;
  # mSetObj$cmdSet <- NULL;
  # qs::qsave(mSetObj, file = mSetNormedNM)

  if(.on.public.web){
    .set.mSet(mSetObj)
    return(1)
  } else {
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

Prepare4Network <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  # mSet <<- mSetObj <- NULL;
  # mSetObj <- InitDataObjects("conc", "network", FALSE)
  mSetObj <- SetOrganism(mSetObj, "hsa")
  cmpdList <- PrepareCMPDList()
  
  cmpdList <- paste(cmpdList, collapse = "\n")
  mSetObj <- PerformCmpdMapping(mSetObj, cmpdList, "hsa", "kegg")
  
  #mSetObj <- CreateMappingResultTable(mSetObj)
  mSetObj <- PrepareNetworkData(mSetObj)
  
  idtype <<- "cmpd";
  mSetObj <- PrepareKeggQueryJson(mSetObj);
  mSetObj <- PerformKOEnrichAnalysis_KO01100(mSetObj, "pathway","network_enrichment_pathway_0")
  mSetObj <- .get.mSet(mSetObj);

  return(.set.mSet(mSetObj))
}

GetSigPathNums <- function(pvalCutoff){
  mSetObj <- .get.mSet(NA);
  pathSet <- mSetObj$dataSet$pathResults;
  anal.type0 <- mSetObj[["paramSet"]][["anal.type"]];
  
  if(class(pathSet[[1]])[1] == "matrix"){
    qs::qsave(pathSet, file = "pathSet_tmp.qs")
  } else {
    pathSet <- qs::qread("pathSet_tmp.qs")
  }
  
  if(anal.type0 == "mummichog"){
    sig.paths <- lapply(pathSet, function(x)  return(rownames(x)[as.data.frame(x)$FET <= pvalCutoff]));
  }else{
    sig.paths <- lapply(pathSet, function(x) return(rownames(x)[as.data.frame(x)$P_val <= pvalCutoff]));
  }
  resTable <- mSetObj$dataSet$res_table
  sig.paths$meta_dat <- resTable[resTable$Meta.P <= pvalCutoff, "Pathways"]
  
  names(sig.paths) <- gsub("mummichoginput.qs", "", names(sig.paths));
  
  mSetObj$dataSet$pathSumData <- sig.paths;
  .set.mSet(mSetObj);
  
  pathSum <- unlist(lapply(sig.paths, length));
  return(pathSum)
}

SelectMultiPathData <- function(mSetObj=NA, nmVec = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.na(nmVec[1])){
    AddErrMsg("No dataset is selected for analysis!");
    return(0);
  }
  
  # mSetObj[["dataSet"]][["pathResults_SelectedFiles"]] <- 
  #   gsub("_","",gsub("\\.[^.]*$", "", basename(nmVec)));
  mSetObj[["dataSet"]][["pathResults_SelectedFiles"]] <- basename(nmVec);

  return(.set.mSet(mSetObj));
}

PrepareMetaPathData <- function(mSetObj = NA, imgNm="NA"){
  #save.image("metapath.RData");
  mSetObj <- .get.mSet(mSetObj);

  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  MixNms <- gsub("_2", "", dataNMs[substr(dataNMs, nchar(dataNMs)-1, nchar(dataNMs)) == "_2"])

  # do something here later
  dat <- mSetObj[["dataSet"]][["pathSumData"]];
  datNum <- length(mSetObj[["dataSet"]][["pathResults_SelectedFiles"]]);

  nms <- gsub(".csv","",names(dat));
  for(nd in seq(names(dat))){  
    for(nm in MixNms){
      if(mSetObj[[nm]]$name == names(dat)[nd]){
         names(dat)[nd] <- paste0(gsub(".csv","",names(dat)[nd]),"mixed");
       }
    }      
  }
  names(dat) <- gsub(".csv","",names(dat));
  dat <- dat[c(mSetObj[["dataSet"]][["pathResults_SelectedFiles"]])];

  require(reshape2)
  df <- melt(dat, value.name="id")
  colnames(df) <- c("name", 'set')
  uniq.nms <- unique(df$name)
  new.df <- dcast(df, name ~ set, value.var='set', fill=0)
  rownames(new.df) <- new.df[,1]
  new.df <- new.df[,-1]
  json.list <- list()
  for(i in 1:nrow(new.df)){
    json.list[[i]] <- list()
    json.list[[i]][["sets"]] <- new.df[i,][new.df[i,] != 0]
    json.list[[i]][["name"]] <- rownames(new.df)[i]
  }
  col.vec <-gg_color_hue(length(dat));

  jsonNm <- paste0(imgNm, ".json")
  json.mat <- RJSONIO::toJSON(list(json.list, col.vec));
  sink(jsonNm);
  cat(json.mat);
  sink();

  return(.set.mSet(mSetObj));
}

GetVennPathsNames <- function(mSetObj=NA, areas){
  
  mSetObj <- .get.mSet(mSetObj);
  
  nms <- strsplit(areas, "\\|\\|")[[1]];
  path.vec <- NULL;
  for(nm in nms){
    path.vec <- c(path.vec, vennData[[nm]]);
  }
  path.vec <- unique(path.vec);
  names(path.vec) <- path.vec;

  if(length(path.vec) ==0){
    return("NA")
  } else {
    return(paste(path.vec, collapse="||"))
  } 
}

#' setInclusionDataSets#'
#' @param datasVec a vector of all files
#' @param mSetObj mSetObj 
#' @export
#' @examples #setInclusionDataSets(c("A1_pos.csv","B1_pos.csv","C1_pos.csv"));
#' @author Jeff Xia\email{jeff.xia@mcgill.ca} 
#' Zhiqiang Pang\email{zhiqiang.pang@mail.mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
setInclusionDataSets <- function(mSetObj=NA, datasVec){
  mSetObj <- .get.mSet(mSetObj);
  fileNM <- tools::file_path_sans_ext(basename(datasVec));
  
  dataNMs <- names(mSetObj)[grepl("MetaData",names(mSetObj))];
  mSetObj$IncludedDataSets <- NULL;
  for(nm in dataNMs){
    for(ds in datasVec) {
      if(mSetObj[[nm]]$name == ds){
        mSetObj$IncludedDataSets <- c(mSetObj$IncludedDataSets, ds)
      }
    }
  }
  
  #mSetObj$InclusionDataSets <- NMs[sapply(NMs, file.exists)]
  return(.set.mSet(mSetObj));
}

qc.biBoxPlot <- function(dat, dat2 = NULL, imgNm1, imgNM2 = NULL, 
                         format="png", dpi=72, width=NA, vertical = TRUE){

  imgNm <- paste(imgNm1,"_",imgNM2, "_dpi", dpi, "_norm_box", ".", format, sep="");
  
  if(vertical){
    imgNm <- paste(imgNm1,"_",imgNM2, "_dpi", dpi, "_qc_box", ".", format, sep="");
  }

  width=460;
  height=420;
  require("lattice");
  require("gridExtra");
  subgene=10000;
  if (nrow(dat)>subgene) {
    set.seed(28051968);
    sg  = sample(nrow(dat), subgene)
    Mss = dat[sg,,drop=FALSE]
  } else {
    Mss = dat
  }
  
  subsmpl=100;
  if (ncol(Mss)>subsmpl) {
    set.seed(28051968);
    ss  = sample(ncol(Mss), subsmpl)
    Mss = Mss[,ss,drop=FALSE]
  } else {
    Mss = Mss
  }
  
  sample_id = rep(seq_len(ncol(Mss)), each = nrow(Mss));
  values  = as.numeric(Mss)
  formula = sample_id ~ values

  box = bwplot(formula, groups = sample_id, layout = c(1,1), as.table = TRUE,
               strip = function(..., bg) strip.default(..., bg ="#cce6ff"),
               horizontal = TRUE, main = imgNm1,
               pch = "|",  col = "black", do.out = FALSE, box.ratio = 2,
               xlab = "", ylab = "Features",
               fill = "#1c61b6AA",
               panel = panel.superpose,
               scales = list(x=list(relation="free"), y=list(axs="i")),
               ylim = c(ncol(Mss)+0.7,0.3),
               prepanel = function(x, y) {
                 list(xlim = quantile(x, probs = c(0.01, 0.99), na.rm=TRUE))
               },
               panel.groups = function(x, y, ...) {
                 panel.bwplot(x, y, ...)
               })

  if(!is.null(dat2)){
    dat <- dat2;
    subgene=10000;
    if (nrow(dat)>subgene) {
      set.seed(28051968);
      sg  = sample(nrow(dat), subgene)
      Mss = dat[sg,,drop=FALSE]
    } else {
      Mss = dat
    }
    
    subsmpl=100;
    if (ncol(Mss)>subsmpl) {
      set.seed(28051968);
      ss  = sample(ncol(Mss), subsmpl)
      Mss = Mss[,ss,drop=FALSE]
    } else {
      Mss = Mss
    }
    
    sample_id = rep(seq_len(ncol(Mss)), each = nrow(Mss));
    values  = as.numeric(Mss)
    formula = sample_id ~ values
    
    box2 = bwplot(formula, groups = sample_id, layout = c(1,1), as.table = TRUE,
                 strip = function(..., bg) strip.default(..., bg ="#cce6ff"),
                 horizontal = TRUE, main = imgNM2,
                 pch = "|",  col = "black", do.out = FALSE, box.ratio = 2,
                 xlab = "", ylab = "Features",
                 fill = "#1c61b6AA",
                 panel = panel.superpose,
                 scales = list(x=list(relation="free"), y=list(axs="i")),
                 ylim = c(ncol(Mss)+0.7,0.3),
                 prepanel = function(x, y) {
                   list(xlim = quantile(x, probs = c(0.01, 0.99), na.rm=TRUE))
                 },
                 panel.groups = function(x, y, ...) {
                   panel.bwplot(x, y, ...)
                 })
  }

  if(vertical){
    width = width;
    height = height*2;
    nrows = 2;
  } else {
    width = width*2;
    height = height;
    nrows = 1;
  }
  if(is.null(dat2)){
    Cairo::Cairo(file=imgNm, width=460, height=420, type="png", bg="white");
    print(box);
    dev.off();
  } else {
    Cairo::Cairo(file=imgNm, width=width, height=height, type="png", bg="white");
    grid.arrange(box, box2, nrow = nrows);
    dev.off();
  }

}


Finish.DataSet <- function(dataName, dataName2 = NULL){
  if(dataName2 == "null"){dataName2 = NULL}
  fileTitle <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName));
  fileTitle2 <- sub(pattern = "(.*)\\..*$", replacement = "\\1", (dataName2));
  
  mSetObjNM <- paste0(fileTitle, "_" ,fileTitle2, "_mSet.qs");
  qs::qsave(mSet, file = mSetObjNM)
  return(1)
}

Restore.CmdHistory <- function(){
  if(file.exists("cmdSet.qs")){
    cmdSet <- qs::qread("cmdSet.qs");
    mSet$cmdSet <<- c(cmdSet, mSet$cmdSet);
  } 
}
