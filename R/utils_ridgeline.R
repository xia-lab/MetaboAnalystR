##################################################
## R script for ExpressAnalyst
## Description: Compute Ridgeline plot
## Authors: 
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
## Jessica Ewald, jessica.ewald@mail.mcgill.ca
##################################################

compute.ridgeline <- function(dataSet, imgNm = "abc", dpi=72, format="png", fun.type = "kegg", ridgeType = "ora", ridgeColor = "teal",rankOpt="fc", sigLevel = 0.05, pwNum=20, inx = 1){
  
  save.image("ridge.RData");
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  analSet <- readSet(analSet, "analSet");
  imageName <- paste0(imgNm, "dpi" , dpi, ".", format);
  anal.type <- paramSet$anal.type;
  require("dplyr");
  require("fgsea");
  require("reshape");
  require("ggplot2");
  require("ggridges");
  # process colors
  if(ridgeColor == "teal"){
    high.col = "#3C7C60";
    low.col = "#C9E1D6";
  } else if (ridgeColor == "orange"){
    high.col = "#ffa34d";
    low.col = "#994a00";    
  } else {
    high.col = "#00da00";
    low.col = "#005000";   
  }
  
  #get pw library
  setres <- .loadEnrichLib(fun.type, paramSet);
  current.geneset <- setres$current.geneset;
  # get DEGs
  if(anal.type == "genelist"){
    if(paramSet$numOfLists > 1){
      dataSet <- readDataset(paramSet$selDataNm);    
    }
    sigmat <- as.data.frame(dataSet$prot.mat)
    sigmat$entrez <- rownames(sigmat);
    universe <- unique(unlist(current.geneset)); 
    expr.vec <- sigmat[,1];
    
    if(sum(expr.vec) == 0){
      msgSet$current.msg <- "Uploaded gene list needs to contain fold-change value to perform Ridgeline analysis!";
      saveSet(msgSet, "msgSet");
      return(-2);
    }
  }else if(anal.type == "onedata"){
    if(ridgeType == "ora"){
      sigmat <- dataSet$sig.mat;
    }else{
      sigmat <- dataSet$comp.res;
      allmat <- dataSet$comp.res;
    }
    sigmat$entrez <- rownames(sigmat);
    universe <- rownames(dataSet$data.norm);
  }else{
    meta.avgFC <- analSet$meta.avgFC;
    inx <- 1;
    if(paramSet$selDataNm == "meta_default"){
      if(ridgeType == "ora"){
        sigmat <- analSet$meta.mat;
        allmat <- qs::qread("meta.resTable.qs");
        sigmat <- cbind(unname(meta.avgFC[rownames(sigmat)]), sigmat);
        
      }else{
        allmat <- qs::qread("meta.resTable.qs");
        sigmat <- allmat;
        sigmat <- cbind(unname(meta.avgFC[rownames(sigmat)]), sigmat);
        
      }
      allmat$logFC <- unname(meta.avgFC[rownames(allmat)]);
      universe <- rownames(allmat);
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      if(ridgeType == "ora"){
        sigmat <- dataSet$sig.mat;
      }else{
        sigmat <- dataSet$comp.res;
        allmat <- dataSet$comp.res;
      }
      sigmat <- as.data.frame(sigmat);
      sigmat$entrez <- rownames(sigmat);
      universe <- rownames(dataSet$data.norm);
    }
  }
  
  if(ridgeType == "ora"){
    .performEnrichAnalysis(dataSet, imgNm, fun.type, rownames(sigmat), "ridgeline")
    res <- qs::qread("enr.mat.qs");
    colnames(res) <- c("size", "expected", "overlap", "pval", "padj");
    
    res <- res[,c(4,5,3,1,2)]
    res <- as.data.frame(cbind(pathway=rownames(res), res));
    res$padj <- as.numeric(res$padj)
    res$pval <- as.numeric(res$pval)
  } else {

    rankedVec<- ComputeRankedVec(dataSet, rankOpt, paramSet$selectedFactorInx);
   
    if(fun.type %in% c("go_bp", "go_mf", "go_cc")){
      res <- fgsea::fgsea(pathways = current.geneset, 
                          stats    = rankedVec,
                          minSize  = 5,
                          maxSize = 500,
                          scoreType = "std",
                          nperm=10000)    
    }else{
      res <- fgsea::fgsea(pathways = current.geneset, 
                          stats    = rankedVec,
                          minSize  = 5,
                          maxSize = 500,
                          scoreType = "std")   
      
    }
  }
  res <- .signif_df(res, 4);
  res <- res[order(res$pval),];
  resTable <- res;
  # process results;
  res <- res[,c(1,2,3)];
  colnames(res) <- c("name", "pval", "adj.pval");
  res.sig <- res;
  totalSigPws <- dim(res[res$pval < sigLevel, ])[1];
  
  if(pwNum != -1){
    if(dim(res.sig)[1] > pwNum){ # limit size if too many sig results
      res.sig <- res.sig[1:pwNum, ]
    }
  }
  
  # prepare data for plotting
  degs.plot <- data.frame(entrez = rownames(sigmat), log2FC = sigmat[,inx]);
  degs.plot <- reshape::melt(degs.plot);
  colnames(degs.plot)[1] <- "entrez";
  
  gs.plot <- reshape::melt(current.geneset);
  colnames(gs.plot) <- c("entrez", "name");
  
  df <- merge(res.sig, gs.plot, by = "name", all.x = TRUE, all.y = FALSE);
  df <- merge(df, degs.plot, by = "entrez", all.x = TRUE, all.y = FALSE);
  df <- na.omit(df)
  
  # calculate the mean fold change to order the pathways in the plot
  means <- aggregate(df$value, by = list(df$name), mean);
  means <- means[order(means$x, decreasing = FALSE), ];
  df$name <- factor(df$name, levels = means$Group.1);
  
  # make the plot
  rp <- ggplot(df, aes(x = value, y = name, fill = adj.pval)) +
    geom_density_ridges(
      jittered_points = TRUE, point_shape = "|", point_size = 5, point_color = "#898A89",
      color = "white",
      scale = 1.5, rel_min_height = .02, size = 0.25,
      position = position_points_jitter(height = 0)) +
    geom_vline(xintercept = 0, color = "red") +
    scale_y_discrete(expand = c(0, 0), name = "Gene Set") + 
    scale_x_continuous(expand = c(0, 0), name = "log2FC") +
    scale_fill_gradient("adj. pval",
                        low = high.col, high = low.col) + 
    coord_cartesian(clip = "off") +
    theme_ridges(center = TRUE) +
    theme(legend.position = "right",
          text = element_text(size=12, color = "black"),
          axis.title = element_text(size=12, face = "bold"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(size=12,color = "black"))
  
  Cairo::Cairo(file=imageName, width=10, height=8, type=format, bg="white", dpi=dpi, unit="in");
  print(rp);
  dev.off();
  
  ##interative ridge json data
  ridge_bw <- rp$layers[[1]]$computed_stat_params$bandwidth;
  jsonNm <- paste0(imgNm, ".json");
  
  symb <- doEntrez2SymbolMapping(df$entrez, paramSet$data.org, paramSet$data.idType);
  df$symbol <- symb;
  
  data.list <- list();
  gene.list <- list();
  pval.list <- list();
  col.list <- list();
  
  for(i in 1:length(levels(df$name))){
    nm <- as.character(levels(df$name)[i]);
    data.list[[ nm ]] <- as.vector(unlist(df[which(df$name == nm), "value"]));
    gene.list[[ nm ]] <- as.vector(unlist(df[which(df$name == nm), "symbol"]));
    pval.list[[ nm ]] <- unname(unlist(res[which(res$name == nm), "pval"]));
  }
  
  minFc <- min(df$value);
  maxFc <- max(df$value);
  minPval <- min(df$pval);
  maxPval <- max(df$pval);
  
  #get hits per pathway
  hits.query <- lapply(current.geneset, 
                       function(x) {
                         x[x %in% universe];
                       }
  );
  hits.query <- hits.query[resTable$pathway];
  
  # enr result for table display
  fun.anot <- hits.query
  if(ridgeType == "ora"){
    total <- resTable[,5]; if(length(total) ==1) { total <- matrix(total) };
    fun.pval <- resTable[,"pval"]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
    fun.padj <- resTable[,"padj"]; if(length(fun.padj) ==1) { fun.padj <- matrix(fun.padj) };
    if(ridgeType == "ora"){
      hit.num <- resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    }else{
      hit.num <- resTable[,"size"]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    }  
  }else{
    total <- as.list(resTable[,5])[[1]]; if(length(total) ==1) { total <- matrix(total) };
    fun.pval <- as.list(resTable[,"pval"])[[1]]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
    fun.padj <- as.list(resTable[,"padj"])[[1]]; if(length(fun.padj) ==1) { fun.padj <- matrix(fun.padj) };
    if(ridgeType == "ora"){
      hit.num <- as.list(resTable[,4])[[1]]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    }else{
      hit.num <- as.list(resTable[,"size"])[[1]]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
    }  
    
  }
  fun.ids <- as.vector(setres$current.setids[names(fun.anot)]); 
  if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  enr.res <- list(
    fun.anot = fun.anot,
    fun.ids = fun.ids,
    fun.pval = fun.pval,
    fun.padj = fun.padj,
    hit.num = hit.num,
    total= total
  );
  
  if(ridgeType == "gsea"){
    enr.res[["ES"]] <- unname(unlist(resTable[,"ES"]));
  }
  
  res.list <- list(data=data.list, 
                   genelist=gene.list, 
                   df=df, minPval=minPval, 
                   maxPval=maxPval, 
                   min=minFc, 
                   max=maxFc, 
                   minPval = min(res.sig$pval),
                   maxPval = max(res.sig$pval),
                   bandwidth=ridge_bw,
                   pathwayPvals = pval.list, 
                   pathwayCols = col.list, 
                   enrRes = enr.res,
                   dat.opt = paramSet$selDataNm,
                   naviString="ridge");
  csv.nm <- paste0(imgNm, ".csv");
  
  fast.write(resTable, file=csv.nm);
  
  analSet$ridgeline <- res.list;
  saveSet(analSet, "analSet");
  
  json.obj <- rjson::toJSON(res.list);
  sink(jsonNm);
  cat(json.obj);
  sink();
  
  #for link sharing
  paramSet$jsonNms$ridge <- jsonNm
  paramSet$partialToBeSaved <- c( paramSet$partialToBeSaved, c(jsonNm));
  saveSet(paramSet, "paramSet");
  
  imgSet <- readSet(imgSet, "imgSet");
  rownames(resTable) <- NULL;
  imgSet$enrTables[["ridgeline"]]$table <- resTable;
  imgSet$enrTables[["ridgeline"]]$library <- fun.type;
  imgSet$enrTables[["gsea"]]$algo <- toupper(ridgeType);
  
  imgSet$compute.ridgeline <- imageName;
  saveSet(imgSet);
  
  return(totalSigPws)
}