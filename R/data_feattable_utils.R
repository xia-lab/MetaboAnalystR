#' Get Significant Genes from Analysis
#'
#' This function retrieves significant genes from the DE analysis based on the specified parameters.
#'
#' @param dataName A character string specifying the name of the dataset.
#' @param res.nm A character string specifying the name of the output result table
#' @param p.lvl The significance threshold for p-values.
#' @param fc.lvl The fold change threshold.
#' @param inx The index for comparison (e.g., in case of multiple comparisons).
#'
#' @return A list of information including the filename, significant gene details, and counts.
#'
#' @author Guangyan Zhou \email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' GetSigGenes(dataName = "MyData", res.nm = "result_A", p.lvl = 0.05,
#'             fc.lvl = 1, inx = 1)
#' }
#'
#' @export
#' @license MIT License
#'
GetSigGenes <-function(dataName="", res.nm="nm", p.lvl=0.05, fc.lvl=1, inx=1, FDR = "true"){
  paramSet <- readSet(paramSet, "paramSet");
  msgSet <- readSet(msgSet, "msgSet");
  analSet <- readSet(analSet, "analSet");
  dataSet <- readDataset(dataName);

  paramSet$use.fdr <- as.logical(FDR);
  total <- nrow(dataSet$comp.res);
  resTable <- dataSet$comp.res;
  filename <- dataSet$filename;
  if(is.null(dataSet$fc.lvl)){
      dataSet$fc.lvl <- 0;
  }
  filename <- paste(filename, "_", res.nm, "_fc_" , dataSet$fc.lvl, ".csv", sep="");
  current.msg <- "";
  
  if (is.null(resTable) || nrow(resTable) == 0){
    msgSet$current.msg <- paste(msgSet$current.msg, "No significant genes were identified using the given design and cutoff."); 
  }
  # now rank by logFC, note, the logFC for each comparisons 
  # are returned in resTable before the AveExpr columns 
  # for two-class, only one column, multiple columns can be involved
  # for > comparisons - in this case, use the largest logFC among all comparisons
  # further filter by logFC
    if (dataSet$de.method == "deseq2") {
      ## use a local copy so the master table’s row order stays unchanged
      resTable <- dataSet$comp.res.list[[inx]]
      hit.inx  <- which(colnames(resTable) == "baseMean")
    } else if (dataSet$de.method == "limma") {
      hit.inx <- which(colnames(resTable) == "AveExpr")
    } else if (dataSet$de.method == "wtt") {
      hit.inx <- which(colnames(resTable) == "t")
    } else {
      hit.inx <- which(colnames(resTable) == "logCPM")
    }

  
  resTable <- resTable[!is.na(resTable[,1]),]
  orig.resTable <- resTable;
  # select based on p-value
  if(FDR == "true"){
      hit.inx.p <- resTable$adj.P.Val <= p.lvl; 
  } else {
      hit.inx.p <- resTable$P.Value <= p.lvl; 
  } 
  
  resTable<-resTable[hit.inx.p,];
  
  maxFC.inx <- hit.inx - 1;
  logfc.mat <- resTable[,1:maxFC.inx, drop=F];
  if(paramSet$oneDataAnalType == "dose"){
    pos.mat <- abs(logfc.mat);
    fc.vec <- apply(pos.mat, 1, max);   # for > comparisons - in this case, use the largest logFC among all comparisons
    hit.inx.fc <- fc.vec >= fc.lvl;
    resTable <- resTable[hit.inx.fc,];
  } else if (dataSet$de.method=="deseq2"){
    pos.mat <- abs(logfc.mat);
    fc.vec <- pos.mat[,1];
    hit.inx.fc <- fc.vec >= fc.lvl;
    resTable <- resTable[hit.inx.fc,];
  }else {
    pos.mat <- abs(logfc.mat[,inx]);
    fc.vec <- pos.mat;
    hit.inx.fc <- fc.vec >= fc.lvl;
    resTable <- resTable[hit.inx.fc,];
  }
  
  if (nrow(resTable) == 0){
    msgSet$current.msg <- paste(msgSet$current.msg, "No significant genes were identified using the given design and cutoff.");
  }
  
  ### Note, rowname of resTable must be entrez ID
  # calculate differently if dose-response
  de.Num <- nrow(resTable);

  non.de.Num <- nrow(dataSet$data.norm) - de.Num;
  
  # may need to update data, class and meta.info
  data <- dataSet$data.norm;
  cls <- dataSet$cls; 
  meta.info <- dataSet$meta.info;
  grp.nms <- levels(cls);
  
  hit.inx <- cls %in% grp.nms;
  if (sum(hit.inx) < length(hit.inx)){
    msgSet$current.msg <- paste(msgSet$current.msg, "Only groups selected for comparisons: ", paste(grp.nms, collapse=", "), "are included.");
    cls <- factor(cls[hit.inx]);
    cls.lvls <- levels(cls);
    data <- data[,hit.inx];
    meta.info <- dataSet$meta.info[hit.inx,];
  }
  qs::qsave(data, file="data.stat.qs");
dataSet$comp.res <- dataSet$comp.res[order(dataSet$comp.res$adj.P.Val), ]
dataSet$comp.res <- dataSet$comp.res[
                      !(rownames(dataSet$comp.res) %in% rownames(resTable)), ]
dataSet$comp.res <- rbind(resTable, dataSet$comp.res)
  
  
  dataSet$sig.mat <- resTable;
  
  if (dataSet$annotated){ # annotated to entrez
    anot.id <- rownames(dataSet$comp.res);
    gene.anot <- doEntrezIDAnot(anot.id, paramSet$data.org, paramSet$data.idType)
    fast.write(cbind(EntrezID=anot.id, signif (dataSet$comp.res,5), Symbols = gene.anot$symbol,  Name=gene.anot$name), row.names=F, file=filename);
  } else if (file.exists("annotation.qs")){ # annotation information available
    anot.id <- qs::qread("annotation.qs");
    feature.vec <- rownames(dataSet$comp.res);
    entrez.vec <- anot.id[feature.vec];
    gene.anot <- doEntrezIDAnot(entrez.vec, paramSet$data.org, paramSet$data.idType);
    fast.write(cbind(signif (dataSet$comp.res,5), EntrezID=entrez.vec, Symbols = gene.anot$symbol,  Name=gene.anot$name), row.names=F, file=filename);
    rownames(gene.anot) <- feature.vec;
  } else {
    gene.anot <- NULL;
    fast.write(signif(resTable,5), file=filename);
  }
  if(is.null(gene.anot)){
    analSet$comp.genes.symbols <- rownames(dataSet$comp.res); # use the id provided
  }else{
    analSet$comp.genes.symbols <- gene.anot$symbol;
  }
  dataSet$cls.stat <- cls;
  dataSet$meta.stat <- meta.info;
  
  # now do protein mapping for network only applicable for annotated
  
  gene <- rownames(resTable);
  
  logFC <- unname(logfc.mat[,1]);
  geneList <- paste(gene, logFC, collapse="\n");
  up <- nrow(resTable[which(logfc.mat[,paramSet$selectedFactorInx]> fc.lvl),])
  down <- nrow(resTable[which(logfc.mat[,paramSet$selectedFactorInx]< -fc.lvl),])
  saveSet(msgSet, "msgSet");
  
  data.norm <- dataSet$data.norm
  colnames(data.norm) <- NULL
  lst <- list(colnames(dataSet$data.norm),data.norm, dataSet$meta.info, dataSet$comp.res, rownames(data.norm), org=paramSet$data.org)
  json.obj <- rjson::toJSON(lst);
  sink("ExpressAnalyst_matrix.json");
  cat(json.obj);
  sink();

  ##---------------------------------------------------------------------------
## Collect/significant-gene tables for DESeq2 comparisons
##---------------------------------------------------------------------------

if (dataSet$de.method == "deseq2") {

  significant_gene_table <- list()    # holds one data-frame per comparison

  for (inx in seq_along(dataSet$comp.res.list)) {

    resTable <- dataSet$comp.res.list[[inx]]

    ## --- remove rows with NA logFC (or other NAs in first col) ------------
    resTable <- resTable[!is.na(resTable[, 1]), ]

    ## --- p-value / FDR filter --------------------------------------------
    deg.pass <- if (FDR == "true")  resTable$adj.P.Val <= p.lvl
                else                resTable$P.Value   <= p.lvl

    ## --- logFC filter -----------------------------------------------------
    lfc.pass <- abs(resTable[ , "logFC"]) >= fc.lvl

    all_pass <- deg.pass & lfc.pass
    if (!any(all_pass)) {                            # nothing passed
      significant_gene_table[[inx]] <- data.frame() # keep list length constant
      next
    }

    sig <- resTable[all_pass, ]
    sig$GeneID      <- rownames(sig)                # preserve raw ID
    sig$Comparison  <- names(dataSet$comp.res.list)[[inx]]

    ## --- annotation (optional) -------------------------------------------
    res.anot <- doEntrezIDAnot(sig$GeneID,
                               paramSet$data.org,
                               paramSet$data.idType)
    sig$Symbol <- res.anot$symbol
    sig$Name   <- res.anot$name

    significant_gene_table[[inx]] <- sig
  }

  ## ---------- combine & export -------------------------------------------
  final_table <- do.call(rbind, significant_gene_table)  # may have duplicates

  output_file <- paste0(dataName, "_logFC_",format(as.numeric(fc.lvl), digits = 2, nsmall = 0, trim = TRUE, scientific = FALSE),
                        "_Significant_Genes.csv")

  if (nrow(final_table) > 0) {
    write.csv(final_table[ , setdiff(names(final_table), "GeneID")],
              file = output_file, row.names = FALSE)

    all_significant_genes <- unique(final_table$GeneID)  # de-duplicate here
    de.Num.total          <- length(all_significant_genes)

    message("Significant genes table exported to: ", output_file)
  } else {
    de.Num.total <- 0
    message("No significant genes identified to export.")
  }

  ## ---------- bookkeeping -----------------------------------------------
  if (de.Num.total == 0) {
    msgSet$current.msg <- paste(
      msgSet$current.msg,
      "No significant genes were identified using the given design and cutoff."
    )
  }
  analSet$sig.gene.count.total <- de.Num.total
}


  analSet$sig.gene.count <- de.Num;
  saveSet(analSet, "analSet");

  dataSet$pval <- p.lvl;
  dataSet$fc.val <- fc.lvl;
  dataSet$comp.res.filename <- filename;
  res <- RegisterData(dataSet);
  saveSet(paramSet, "paramSet");
  return(c(filename, de.Num, geneList, total, up, down, non.de.Num));
}
