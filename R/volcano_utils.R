  ##################################################
## R scripts for ExpressAnalyst 
## Functions related to volcano plot
## Author: Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#'Prepare data for volcano plot visualization
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
Volcano.Anal <- function(dataName="", fileNm="name", paired=FALSE, fcthresh=0, threshp=0.05, analType="NA", inx=1, dpi=72, format="png"){
  #save.image('volc.RData');

  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  anal.type <- paramSet$anal.type;

  inx <- as.numeric(inx)
  #print("Prepare volcano anal");
  limit_fc <- T; #whether limit to -10 to 10 fc
  if(anal.type == "metadata"){
    if(paramSet$selDataNm == "meta_default"){
       if(is.null(paramSet$fc.thresh)){
         paramSet$fc.thresh <- 0; 
       }

      data <- qs:::qread("allMeta.mat.qs")    
      p.value <- data[, 2]
      data <- cbind(unname(analSet$meta.avgFC[rownames(data)]), data);
      fcthresh <- paramSet$fc.thresh;
      threshp <- paramSet$BHth;
      inx <- 1;
    }else{
      dataSet <- readDataset(paramSet$selDataNm);
      
      data <- as.matrix(analSet$inmex.ind[paramSet$selDataNm][[1]])
      p.value <- data[, "Pval"]
    }
    limit_fc <-F;
  }else{
    dataSet <- readDataset(dataName);
    data <- as.matrix(dataSet$comp.res);
    if(is.null(paramSet$use.fdr) || paramSet$use.fdr){
        p.value <- data[, "adj.P.Val"];
    }else{
        p.value <- data[, "P.Value"];
    }
  }

  paramSet$fcthreshu <- fcthresh
  inx.p <- p.value <= threshp;
  
  zero.inx <- unname(p.value) == 0;
  p.value[zero.inx] <- min(p.value[!zero.inx])/10;
 
  p.log <- -log10(p.value);
  if(paramSet$data.org == "omk"){
    anot.id <- rownames(data);
    gene.anot <- data.frame(gene_id=anot.id, symbol=anot.id, name=anot.id, stringsAsFactors=FALSE)
  }else if (anal.type == "metadata" || dataSet$annotated ){ # annotated to entrez
    anot.id <- rownames(data);
    gene.anot <- doEntrezIDAnot(anot.id, paramSet$data.org, paramSet$data.idType);
  }else{
    anot.id <- rownames(data);
    gene.anot <- data.frame(gene_id=anot.id, symbol=anot.id, name=anot.id, stringsAsFactors=FALSE)
    paramSet$init.lib <- "NA"
  }
  
  #gene symbol to be used for boxplot   
  
  # create a named matrix of sig vars for display
  fc.log <- data[, inx];
  if(limit_fc){
    hit.maxPos <- (which(fc.log> 10) )
    hit.maxNeg <- (which(fc.log< -10) )
    fc.log[hit.maxPos] <- 10;
    fc.log[hit.maxNeg] <- 10;
  }
  #fc.all <- res$fc.all;
  
  if(fcthresh != 0){
    inx.up <- fc.log > fcthresh & p.value < threshp;
    inx.down <- fc.log < -fcthresh & p.value < threshp;
  }else{
    inx.up <- fc.log > 0 & p.value < threshp;
    inx.down <- fc.log < 0 & p.value < threshp;
  }
  
  # create named sig table for display
  inx.imp <- (inx.up | inx.down) & inx.p;
  sig.var <- cbind(fc.log[inx.imp,drop=F], p.value[inx.imp,drop=F], p.log[inx.imp,drop=F]);
  colnames(sig.var) <- c("log2(FC)", "p.value", "-log10(p)");
  # first order by log(p), then by log(FC)
  ord.inx <- order(sig.var[,3], abs(sig.var[,1]), decreasing=T);
  sig.var <- sig.var[ord.inx,, drop=F];
  
  sig.var <- signif (sig.var, 5);
  sig.var1 <- sig.var;
  sig.var1 <- cbind(rownames(sig.var), sig.var);
  colnames(sig.var1) <- c("name", "log2(FC)", "p.value", "-log10(p)");
  
  ###########################
  ## for Volcano data
  ##########################
  
  if(paramSet$init.lib != "NA"){
    saveSet(paramSet, "paramSet");
    PerformVolcanoEnrichment(dataName, "enrichment_result", paramSet$init.lib, "null", "all", inx)
    paramSet <- readSet(paramSet, "paramSet");
    msgSet <- readSet(msgSet, "msgSet");
  }
  
  fileName <- "volcano.csv";
  jsonNm <- "volcano.json";
  json.obj <- rjson::toJSON(sig.var1);
  sink(jsonNm);
  cat(json.obj);
  sink();
  fast.write(signif (sig.var,5),file=fileName);
  colnames(gene.anot)[1] <- "anot.id"
  
  volcano <- list (
    raw.threshx = fcthresh,
    raw.threshy = threshp,
    paired = paired,
    thresh.y = -log10(threshp),
    fc.symb =rownames(data),
    fc.log = fc.log,
    fc.log.uniq = jitter(fc.log),
    inx.up = inx.up,
    inx.down = inx.down,
    p.log = p.log,
    p.raw = p.value,
    inx.p = inx.p,
    sig.mat = sig.var,
    conv = gene.anot,
    analType = anal.type,
    org=paramSet$data.org,
    dat.opt = paramSet$selDataNm,
    naviString = "Volcano Plot"
  );
  
  analSet$volcano <- volcano;
  saveSet(analSet, "analSet");
  sigDownIds <- GetVolcanoUpLftIDs();
  sigUpIds <- GetVolcanoUpRgtIDs();
  nonSigIds <- GetVolcanoDnIDs();
  
  volcano[["sigDownIds"]] <- sigDownIds;
  volcano[["sigUpIds"]] <- sigUpIds;
  volcano[["nonSigIds"]] <- nonSigIds;
  
  jsonNm <- paste0(fileNm, ".json");
  json.obj <- rjson::toJSON(volcano);
  sink(jsonNm);
  cat(json.obj);
  sink();
  
  if(paramSet$init.lib == "NA"){
    enr.mat <- "NA"
  }else{
    enr.mat <- qs:::qread("enr.mat.qs");
    #fast.write(enr.mat, file="enrichment_result.csv", row.names=T);
  }
  sink("enrichment_result.json");
  cat(json.obj);
  sink();
  #paramSet$partialToBeSaved <- c(paramSet$partialToBeSaved, c(jsonNm, "enrichment_result.csv"))
  paramSet$jsonNms["volcano"] <- fileNm;

    # Generate volcano_data
    volcano_data <- data.frame(
        gene = gene.anot$symbol,                  # Gene names
        log2FoldChange = fc.log,                  # Log fold change values
        pValue = p.value,                         # Raw p-values
        negLog10PValue = p.log,                   # -log10 p-value values
        significant = ifelse(inx.up & inx.p, "upregulated", 
                             ifelse(inx.down & inx.p, "downregulated", "nonsignificant")) # Determine significance
    )


  # Append hover_text after defining volcano_data
  volcano_data$hover_text <- with(volcano_data, paste("Gene: ", gene, 
                                                      "<br>Log2 FC: ", log2FoldChange, 
                                                      "<br>P-value: ", pValue))
    library(ggplot2)
    library(plotly)

    # Create a ggplot
    gg_volcano <- ggplot(volcano_data, aes(x = log2FoldChange, y = negLog10PValue, 
                                           color = significant, 
                                           text = paste("Gene: ", gene, "<br>Log2 FC: ", log2FoldChange, 
                                                        "<br>P-value: ", format(pValue, scientific = TRUE)))) +
      geom_point(alpha = 0.6) +  # Adjust point transparency
      scale_color_manual(values = c("upregulated" = "red", "downregulated" = "blue", "nonsignificant" = "grey")) +
      labs(x = "Log2 Fold Change", 
           y = "-Log10 P-value") +
      theme_minimal()

    # Convert to ggplotly for interactive plot, including tooltips
    pwidget <- ggplotly(gg_volcano, tooltip = "text")

    # Customize the layout to optimize hover interaction
    pwidget <- pwidget %>% layout(hovermode = 'closest')

    # Print the plot
    pwidget

  imgSet <- readSet(imgSet, "imgSet");
  widgetNm <- paste0(fileNm, ".rda");
  imgSet$volcanoPlotly <- widgetNm;

  save(pwidget, file = widgetNm);

  imgSet$volcanoPlot <- paste0(fileNm, ".png");

  Cairo::Cairo(file = imgSet$volcanoPlot, unit="px", dpi=dpi, width=1000, height=800, type=format, bg="white");
  print(gg_volcano)
  dev.off()

  saveSet(imgSet, "imgSet");
  saveSet(paramSet, "paramSet");
  saveSet(analSet, "analSet");
  print("Volcano OK");
  
  return(1);
}


GetVolcanoDnMat <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  blue.inx <- which(!imp.inx);
  
  if(sum(blue.inx)>0){
    xs <- vcn$fc.log[blue.inx]
    ys <- vcn$p.log[blue.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}


GetVolcanoUpLftMat <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- vcn$inx.down & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpRgtMat <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- vcn$inx.up & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpLftIDs <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- vcn$inx.down & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    return(names(vcn$fc.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetVolcanoUpRgtIDs <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- vcn$inx.up & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    return(names(vcn$fc.log)[red.inx]);
  }else{
    return("NA");
  }
}

GetVolcanoDnIDs <- function(){
  analSet <- readSet(analSet, "analSet");
  vcn <- analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  blue.inx <- which(!imp.inx);
  if(sum(blue.inx)>0){
    return(names(vcn$fc.log)[blue.inx]);
  }else{
    return("NA");
  }
}


PerformVolcanoEnrichment<-function(dataName="", file.nm, fun.type, IDs, type, inx){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  fcthreshu <- paramSet$fcthreshu;
  fcthreshu <- as.numeric(fcthreshu);
  print(paste0(fcthreshu, "===fcthresh"));
  anal.type <- paramSet$anal.type;
  inx <- as.numeric(inx)
  if(anal.type == "onedata"){
    if(dataSet$type == "array"){
      sigmat <- dataSet$sig.mat
    } else {
      sigmat <- dataSet$sig.mat
    }
  }else{
    if(paramSet$selDataNm == "meta_default"){
      sigmat <- analSet$meta.mat
      sigmat <- cbind(unname(analSet$meta.avgFC[rownames(sigmat)]), sigmat);
      inx <- 1;
    }else{
      sigmat <- analSet$inmex.ind[paramSet$selDataNm][[1]][which(analSet$inmex.ind[paramSet$selDataNm][[1]][,'Pval'] < as.numeric(paramSet$pvalu)),];
    }
  }
  
  if(type == "focus"){
    gene.vec <- unlist(strsplit(IDs, "; "));
  }else if(type == "all"){
    gene.vecup <- rownames(sigmat[which(sigmat[,inx] > fcthreshu),]);
    gene.vecdown <- rownames(sigmat[which(sigmat[,inx] < -fcthreshu),]);
    gene.vec <- c(gene.vecup, gene.vecdown);
  }else if(type == "up"){
    gene.vec <- rownames(sigmat[which(sigmat[,inx] > fcthreshu),]);
  }else{
    gene.vec <- rownames(sigmat[which(sigmat[,inx] < -fcthreshu),]);
  }
  sym.vec <- doEntrez2SymbolMapping(gene.vec, paramSet$data.org, paramSet$data.idType);
  names(gene.vec) <- sym.vec;
  res <- .performEnrichAnalysis(dataSet, file.nm, fun.type, gene.vec, "volcano");
  return(res);
}


# note: hit.query, resTable must synchronize
# ora.vec should contains entrez ids, named by their gene symbols
PerformVolcanoBatchEnrichment <- function(dataName="", file.nm, fun.type, IDs, inx){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  msgSet <- readSet(msgSet, "msgSet");
  anal.type <- paramSet$anal.type;
  # prepare lib
  inx <- as.numeric(inx);
  if(anal.type == "onedata"){
    if(dataSet$type == "array"){
      sigmat <- dataSet$sig.mat
    } else {
      sigmat <- dataSet$sig.mat
    }
  }else{
    sigmat <- analSet$inmex.ind[paramSet$selDataNm][[1]][which(analSet$inmex.ind[paramSet$selDataNm][[1]][,'Pval'] < as.numeric(paramSet$pvalu)),];
  }
  
  one.path.vec <- unlist(strsplit(IDs, "; "));
  
  gene.vecup <- rownames(sigmat[which(sigmat[,inx] > paramSet$fcthreshu),]);
  gene.vecdown <- rownames(sigmat[which(sigmat[,inx] < -paramSet$fcthreshu),]);
  ora.vec <- c(gene.vecup, gene.vecdown);
  
  
  sym.vec <- doEntrez2SymbolMapping(ora.vec, paramSet$data.org, paramSet$data.idType);
  names(ora.vec) <- sym.vec;
  
  current.geneset <- list()
  current.geneset[["Set"]] <- one.path.vec
  current.geneset[["Set2"]] <- one.path.vec
  
  # prepare query
  ora.nms <- names(ora.vec);
  
  # prepare for the result table
  set.size<-length(current.geneset);
  res.mat<-matrix(0, nrow=set.size, ncol=5);
  rownames(res.mat)<-names(current.geneset);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "P.Value", "FDR");
  
  # need to cut to the universe covered by the pathways, not all genes
  if(paramSet$universe.opt == "library"){
    current.universe <- unique(unlist(current.geneset));     
  }else{
    # cut to the universe to uploaded genes
    if(paramSet$anal.type == "onedata"){
      current.universe <- rownames(dataSet$data.anot); 
    }else if(paramSet$anal.type == "metadata"){
      inmex <- qs::qread("inmex_meta.qs");
      current.universe <- rownames(inmex$data); 
    }else{
      if(!is.null(paramSet$backgroundUniverse)){
        current.universe <- paramSet$backgroundUniverse;
      }else{
        current.universe <- unique(unlist(current.geneset)); 
      }
    }
  }
  
  hits.inx <- ora.vec %in% current.universe;
  ora.vec <- ora.vec[hits.inx];
  ora.nms <- ora.nms[hits.inx];
  
  q.size<-length(ora.vec);
  
  # get the matched query for each pathway
  hits.query <- lapply(current.geneset, 
                       function(x) {
                         ora.nms[ora.vec%in%unlist(x)];
                       }
  );
  
  qs::qsave(hits.query, "hits_query.qs");
  
  names(hits.query) <- names(current.geneset);
  hit.num<-unlist(lapply(hits.query, function(x){length(unique(x))}), use.names=FALSE);
  
  # total unique gene number
  #uniq.count <- length(current.universe);
  uniq.count <- nrow(dataSet$data.norm);
  
  # unique gene count in each pathway
  set.size <- unlist(lapply(current.geneset, length));
  
  res.mat[,1]<-set.size;
  res.mat[,2]<-q.size*(set.size/uniq.count);
  res.mat[,3]<-hit.num;
  
  # use lower.tail = F for P(X>x)
  raw.pvals <- phyper(hit.num-1, set.size, uniq.count-set.size, q.size, lower.tail=F);
  # Replace NaN values with 1
  raw.pvals[is.nan(raw.pvals)] <- 1

  res.mat[,4]<- raw.pvals;
  res.mat[,5] <- raw.pvals;
  
  # now, clean up result, synchronize with hit.query
  res.mat <- res.mat[hit.num>0,,drop = F];
  hits.query <- hits.query[hit.num>0];
  if(nrow(res.mat)> 0){
    # order by p value
    ord.inx<-order(res.mat[,4]);
    #res.mat <- signif(res.mat[ord.inx,],3);
    hits.query <- hits.query[ord.inx];
    
    imp.inx <- res.mat[,4] <= 0.05;
    if(sum(imp.inx) < 10){ # too little left, give the top ones
      topn <- ifelse(nrow(res.mat) > 10, 10, nrow(res.mat));
      res.mat <- res.mat[1:topn,];
      hits.query <- hits.query[1:topn];
    }else{
      res.mat <- res.mat[imp.inx,];
      hits.query <- hits.query[imp.inx];
      if(sum(imp.inx) > 120){
        # now, clean up result, synchronize with hit.query
        res.mat <- res.mat[1:120,];
        hits.query <- hits.query[1:120];
      }
    }
  }else{
    return(0);
  }
  
  #get gene symbols
  resTable <- data.frame(Pathway=rownames(res.mat), res.mat);
  res.mat[,"Hits"] <- res.mat[,"Hits"]

  # Check for and handle duplicate row names in enr.mat
  if(any(duplicated(rownames(res.mat)))) {
    res.mat <- res.mat[!duplicated(rownames(res.mat)), ]
    hits.query <- hits.query[match(rownames(res.mat), names(hits.query))]

    print("Duplicates in enr.mat were removed.")
  } else {
    res.mat <- res.mat
  }

  qs:::qsave(res.mat, "enr.mat.qs");
  msgSet$current.msg <- "Functional enrichment analysis was completed";
  
  # write json
  fun.anot <- hits.query; 
  total <- resTable[,2]; if(length(total) ==1) { total <- matrix(total) };
  fun.pval <- resTable[,5]; if(length(fun.pval) ==1) { fun.pval <- matrix(fun.pval) };
  fun.padj <- resTable[,6]; if(length(fun.padj) ==1) { fun.padj <- matrix(fun.padj) };
  hit.num <- resTable[,4]; if(length(hit.num) ==1) { hit.num <- matrix(hit.num) };
  fun.ids <- as.vector(names(fun.anot)); 
  if(length(fun.ids) ==1) { fun.ids <- matrix(fun.ids) };
  json.res <- list(
    fun.link = "",
    fun.anot = fun.anot,
    fun.ids = fun.ids,
    fun.pval = fun.pval,
    fun.padj = fun.padj,
    hit.num = hit.num,
    total= total
  );
  json.mat <- rjson::toJSON(json.res);
  json.nm <- paste(file.nm, ".json", sep="");
  
  sink(json.nm)
  cat(json.mat);
  sink();
  
  # write csv
  fun.hits <<- hits.query;
  fun.pval <<- resTable[,5];
  hit.num <<- resTable[,4];
  csv.nm <- paste(file.nm, ".csv", sep="");    
  fast.write(resTable, file=csv.nm, row.names=F);
  
  saveSet(msgSet, "msgSet");
  return(1);
}