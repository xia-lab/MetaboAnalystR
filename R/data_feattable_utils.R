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
  if (dataSet$de.method=="deseq2"){
    hit.inx <- which(colnames(resTable) == "baseMean"); 
    dataSet$comp.res <- dataSet$comp.res.list[[inx]];
    resTable <- dataSet$comp.res;
  } else if (dataSet$de.method=="limma"){
    hit.inx <- which(colnames(resTable) == "AveExpr");
  } else {
    hit.inx <- which(colnames(resTable) == "logCPM");
    dataSet$comp.res <- dataSet$comp.res.list[[inx]];
    resTable <- dataSet$comp.res;
    print(head(resTable));
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
  } else if (dataSet$de.method=="deseq2" || dataSet$de.method=="edger"){
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
  dataSet$comp.res <- dataSet$comp.res[order(dataSet$comp.res$adj.P.Val),] 
  dataSet$comp.res <- dataSet$comp.res[which(!rownames(dataSet$comp.res) %in% rownames(resTable)),]
  dataSet$comp.res <- rbind(resTable, dataSet$comp.res);
  
  
  dataSet$sig.mat <- resTable;
  
  if (dataSet$annotated){ # annotated to entrez
    anot.id <- rownames(dataSet$comp.res);
    gene.anot <- doEntrezIDAnot(anot.id, paramSet$data.org, paramSet$data.idType);
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

  if (dataSet$de.method %in% c("deseq2", "edger")) {
  # Initialize a list to collect data from all comparisons
  significant_gene_table <- list()

  # Loop through each comparison again to collect significant gene details
  for (inx in seq_along(dataSet$comp.res.list)) {
    resTable <- dataSet$comp.res.list[[inx]]
    
    # Remove rows with NA in the first column
    resTable <- resTable[!is.na(resTable[, 1]), ]
    
    # Select based on p-value
    if (FDR == "true") {
      deg.pass <- resTable$adj.P.Val <= p.lvl
    } else {
      deg.pass <- resTable$P.Value <= p.lvl
    }
    
    # Filter based on fold change
    logfc.mat <- abs(resTable[, "logFC"])  # Ensure the correct column is used for logFC
    lfc.pass <- logfc.mat >= fc.lvl
    
    # Apply filters to identify significant genes
    all_pass <- deg.pass & lfc.pass
    
    # Extract significant genes and their details
    significant_genes <- resTable[all_pass, ]
    
    # Check if there are any significant genes
    if (nrow(significant_genes) > 0) {
      res.anot <- doEntrezIDAnot(rownames(significant_genes), paramSet$data.org,  paramSet$data.idType);
      significant_genes$Symbol <- res.anot$symbol;
      significant_genes$Name <- res.anot$name;
      significant_genes$Comparison <- names(dataSet$comp.res.list)[[inx]]
      
      # Append the significant genes to the list
      significant_gene_table[[inx]] <- significant_genes
    } else {
      # If no significant genes, append an empty data frame with consistent columns
      significant_gene_table[[inx]] <- data.frame()
    }
  }

  # Combine all significant genes into a single data frame
  if (length(significant_gene_table) > 0) {
    final_table <- do.call(rbind, significant_gene_table)
  } else {
    final_table <- data.frame()
  }

  # Export the final table as a CSV file if it's not empty
  output_file <- paste0(dataName, "_logFC_",fc.lvl , "_Significant_Genes.csv");
  if (nrow(final_table) > 0) {
    write.csv(final_table, file = output_file, row.names = TRUE)
  all_significant_genes <- unique(rownames(final_table))
  de.Num.total <- length(all_significant_genes)
    message(paste("Significant genes table has been successfully exported to:", output_file))
  } else {
  de.Num.total <- 0
    message("No significant genes identified to export.")
  }



  if (de.Num.total == 0) {
    msgSet$current.msg <- paste(msgSet$current.msg, "No significant genes were identified using the given design and cutoff in any comparison.")
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
  if(res == 1){
    return(c(filename, de.Num, geneList, total, up, down, non.de.Num));
  }
}
