##################################################
## R scripts for OmicsAnalyst
## Description: Related to linear modeling
## Author: Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

#' Perform Covariate Scatter Analysis
#'
#' This function performs a covariate scatter analysis based on the specified parameters.
#'
#' @param dataName A character string specifying the name of the dataset.
#' @param imgName A character string specifying the name of the image to save.
#' @param imgFormat The format of the image to create (e.g., "png", "jpeg").
#' @param analysis.var The analysis variable.
#' @param ref The reference variable.
#' @param block The blocking variable.
#' @param thresh The significance threshold.
#' @param pval.selection The method for p-value selection ("fdr" or "raw").
#' @param contrast.cls The contrast class ("anova" or "t").
#'
#' @return Result is saved in analSet object.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#'
#' @examples
#' \dontrun{
#' CovariateScatter.Anal(dataName = "estrogen.txt", analysis.var = "variableA",
#'                       imgName = "scatter_plot", imgFormat = "png",
#'                       thresh = 0.05, pval.selection = "fdr",
#'                       contrast.cls = "anova")
#' }
#'
#' @export
#' @license MIT License
#'
CovariateScatter.Anal <- function(dataName, 
                                  imgName="NA", 
                                  imgFormat="png", 
                                  analysis.var, 
                                  ref = NULL, 
                                  block = "NA", 
                                  thresh=0.05,
                                  pval.selection="fdr",
                                  contrast.cls = "anova"
                                  ){

  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");
  msg.lm <- ""
  # load libraries
  library(limma)
  library(dplyr)
  
  # get inputs
  if(!exists('adj.vec')){
    adj.bool = F;
    vars <- analysis.var;
  }else{
    if(length(adj.vec) > 0){
      adj.bool = T;
    }else{
      adj.bool = F;
      adj.vec <- "NA"
    }
  }

  dataSet <- .multiCovariateRegression(dataSet,analysis.var, ref,contrast.cls, random.effects=adj.vec, F, T);

  rest <- dataSet$comp.res;
  res.noadj <- dataSet$res.noadj;
  #dataSet$res.noadj <- "";
  # make visualization
  adj.mat <- rest[, c("P.Value", "adj.P.Val")]
  noadj.mat <- res.noadj[, c("P.Value", "adj.P.Val")]
  
  colnames(adj.mat) <- c("pval.adj", "fdr.adj")
  colnames(noadj.mat) <- c("pval.no", "fdr.no")
  
  both.mat <- merge(adj.mat, noadj.mat, by = "row.names")
  both.mat$pval.adj <- -log10(both.mat$pval.adj)
  both.mat$fdr.adj <- -log10(both.mat$fdr.adj)
  both.mat$pval.no <- -log10(both.mat$pval.no)
  both.mat$fdr.no <- -log10(both.mat$fdr.no)
  rownames(both.mat) = both.mat[,"Row.names"]
  both.mat$label <- invert_named_vector(dataSet$enrich_ids)[as.character(rownames(both.mat))];  

  # make plot
  if( "F" %in% colnames(rest)){
    fstat <- rest[, "F"];
  }else{
    fstat <- rest[, "t"];
  }  
  if(pval.selection == "fdr"){
    p.value <- rest[,"adj.P.Val"];
  }else{
    p.value <- rest[,"P.Value"];
  }
  ord.inx <- order(p.value, decreasing = FALSE);
  rest <- rest[ord.inx,,drop=F];
  colnames(rest)[1] <- "logFC"; 
  rest$ids <- rownames(rest);

  names(fstat) <- names(p.value) <- rownames(rest);

  inx.imp <- p.value <= thresh;
  inx.imp <- ifelse(is.na(inx.imp), FALSE, inx.imp);
  sig.num <- length(which(inx.imp == TRUE));
  
  if(sig.num > 0){ 
    sig.p <- p.value[inx.imp];
    sig.mat <- rest[inx.imp,];
    sig.mat[,-ncol(sig.mat)] <- sapply(sig.mat[,-ncol(sig.mat)], function(x) signif(x, 5));
    rownames(sig.mat) <- make.names(rownames(rest)[inx.imp])
    # order the result simultaneously
  }else{
    AddMsg(paste(c("No significant genes are detected, please adjust your parameters", collapse=" ")));
    return(0);
  }
  AddMsg(paste(c("A total of", length(which(inx.imp == TRUE)), "significant features were found."), collapse=" "));

  both.mat <- both.mat[rownames(rest),]

  rest$label <- invert_named_vector(dataSet$enrich_ids)[as.character(rest$ids)];
  dataSet$comp.res <- rest;
  sig.mat$label <-  invert_named_vector(dataSet$enrich_ids)[as.character(sig.mat$ids)];


  if(sig.num> 0){
    res <- 1;
    fileName <- "covariate_result.csv"
    fast.write.csv(sig.mat,file=fileName);
    cov<-list (
      sig.num = sig.num,
      sig.nm = fileName,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      sig.mat = sig.mat
    );
  }else{
    res <- 0;
    cov<-list (
      sig.num = sig.num,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.value.no = both.mat$pval.no,
      p.log = -log10(p.value),
      inx.imp = inx.imp
    );
  }
  
  # for detail table
  dataSet$analSet$cov <- cov; 
  # for plotting adjp vs p
  dataSet$analSet$cov.mat <- both.mat; 
  both.list <- apply(both.mat, 2, function(x){unname(as.list(x))})

  both.list$thresh <- thresh;
  jsonNm <- gsub(paste0(".", imgFormat), ".json", imgName);
  jsonObj <- rjson::toJSON(both.list);
  sink(jsonNm);
  cat(jsonObj);
  sink();
    
  nonSig <- nrow(dataSet$comp.res) - sig.num;

  RegisterData(dataSet)
  return(c(sig.num, nonSig));
}

# Define function to invert named vector
invert_named_vector <- function(input_named_vec) {
  # Get names and values of input named vector
  input_names <- names(input_named_vec)
  input_values <- unname(input_named_vec)
  
  # Invert the named vector
  output_named_vec <- setNames(input_names, input_values)
  
  # Return output named vector
  return(output_named_vec)
}


PlotCovariateMap <- function(dataName, theme="default", imgName="NA", format="png", dpi=72){
  dataSet <- readDataset(dataName);
  both.mat <- dataSet$cov.mat
  both.mat <- both.mat[order(-both.mat[,"pval.adj"]),]
  logp_val <- dataSet$cov$thresh
  load_ggplot();
  library(ggrepel);
  topFeature <- 5;
  if(nrow(both.mat) < topFeature){
    topFeature <- nrow(both.mat);
  }
  if(theme == "default"){
    p <- ggplot(both.mat, mapping = aes(x = pval.no, y = pval.adj, label = Row.names)) +
      geom_rect(mapping = aes(xmin = logp_val, xmax = Inf, 
                              ymin = logp_val, ymax = Inf),
                fill = "#6699CC") +
      geom_rect(mapping = aes(xmin = -Inf, xmax = logp_val, 
                              ymin = -Inf, ymax = logp_val),
                fill = "grey") +
      geom_rect(mapping = aes(xmin = logp_val, xmax = Inf, 
                              ymin = -Inf, ymax = logp_val),
                fill = "#E2808A") +
      geom_rect(mapping = aes(xmin = -Inf, xmax = logp_val, 
                              ymin = logp_val, ymax = Inf),
                fill = "#94C973") +
      guides(size="none") +
      #annotate("text", x = 0.8, y = 0, label = "Never significant", size = 3) +
      #annotate("text", x = 2, y = 0, label = "Significant without adjustment", size = 3) +
      #annotate("text", x = 0.4, y = 1.5, label = "Significant with adjustment", size = 3) +
      #annotate("text", x = 2.25, y = 1.5, label = "Always significant", size = 3) +
      geom_point(aes(size=pval.adj), alpha=0.5) +
      geom_abline(slope=1, intercept = 0, linetype="dashed", color = "red", size = 1) +
      xlab("-log10(P-value): no covariate adjustment") +
      ylab("-log10(P-value): adjusted") +
      geom_text_repel(data = both.mat[c(1:topFeature),], 
                  aes(x=pval.no,y=pval.adj,label=Row.names)) +
      theme_bw();
  }else{
    p <- ggplot(both.mat, mapping = aes(x = pval.no, y = pval.adj, label = Row.names)) +
      guides(size="none") +
      geom_point(aes(size=pval.adj), alpha=0.5) +
      geom_abline(slope=1, intercept = 0, linetype="dashed", color = "red", size = 1) +
      geom_vline(xintercept = logp_val) +
      geom_hline(yintercept = logp_val) +
      xlab("-log10(P-value): no covariate adjustment") +
      ylab("-log10(P-value): adjusted") +
      geom_text_repel(data = both.mat[c(1:topFeature),], 
                  aes(x=pval.no,y=pval.adj,label=Row.names))
  }
  
  dataSet$covAdj <- imgName;

  width <- 8;
  height <- 8.18;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=width, height=height, type=format, bg="white");
  print(p)
  dev.off()
  
  return(RegisterData(dataSet));
}


# general message only print when running local
AddMsg <- function(msg){
  if(!exists("msg.vec")){
    msg.vec <<- "";
  }
  msg.vec <<- c(msg.vec, msg);
  if(!.on.public.web){
    print(msg);
  }
}


#'Plot compound summary for multi-linear regression tool
#'@param cmpdNm Input the name of the compound to plot
#'@param format Input the format of the image to create
#'@param dpi Input the dpi of the image to create
#'@param width Input the width of the image to create
#'@param meta Input the metadata to visualize
#'@param version version
#'@author Jessica Ewald\email{jessica.ewald@mcgill.ca}
#'McGill University, Canada
#'License: GPL-3 License
#'@export
#'
PlotMultiFacCmpdSummary <- function(dataName,imgName,name, id, meta, version, format="png", dpi=72, width=NA){
  dataSet <- readDataset(dataName);
  paramSet <- readSet(paramSet, "paramSet");

  if(.on.public.web){
    load_ggplot()
  }
  
  if(is.na(width)){
    w <- 7.5;
  }else{
    w <- width;
  }
  meta.info <- paramSet$dataSet$meta.info
  sel.cls <- meta.info[which(rownames(meta.info) %in% colnames(dataSet$data.norm)),meta]
  cls.type <- unname(paramSet$dataSet$meta.types[meta])
  xlab = meta;
  h <- 6;
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  
  inx <- which(rownames(dataSet$data.norm) == id)

  if(cls.type == "cont"){
    df.norm <- data.frame(value=as.vector(t(dataSet$data.norm)[, inx]), name = as.numeric(as.character(sel.cls)))
  }else{
    df.norm <- data.frame(value=as.vector(t(dataSet$data.norm)[, inx]), name = sel.cls)
  }
  col <- unique(GetColorSchema(sel.cls));
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(cls.type == "disc"){
    p <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name)) + geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + theme_bw() + geom_jitter(size=1) 
    p <- p + scale_fill_manual(values=col) + theme(axis.text.x = element_text(angle=90, hjust=1))
    p <- p + ggtitle(name) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") + xlab(meta)
    p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p <- p + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10))
    p <- p + theme(legend.position = "none");

  }else{
    p <- ggplot2::ggplot(df.norm, aes(x=name, y=value)) 
    p <- p + geom_point(size=2) + theme_bw()  + geom_smooth(method=lm,se=T)     
    p <- p + theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")
    p <- p + ggtitle(name) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Abundance") + xlab(meta)
    p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
    p <- p + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
  }
  print(p)
  dev.off()
  #print(imgName);
  if(.on.public.web){
    return(imgName);
  }else{
    return(1);
  }
}
