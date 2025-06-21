my.build.cemi.net <- function(dataName,
                              filter      = TRUE,
                              min_ngen    = 30,
                              cor_method  = "pearson", ##c("pearson", "spearman"),
                              verbose     = F) {

  save.image("cemitool.RData")                  # snapshot

  ## 1 · load dataset -------------------------------------------------
  dataSet  <- readDataset(dataName)
  expr_mat <- as.data.frame(dataSet$data.norm)              # genes × samples
  annot_df <- data.frame(SampleName = rownames(dataSet$meta.info),
                         Class      = dataSet$meta.info[,1],
                         stringsAsFactors = FALSE)

  ## 2 · packages -----------------------------------------------------
  suppressPackageStartupMessages({
    library(CEMiTool)
    library(WGCNA)
  })

  ## 3 · run CEMiTool -------------------------------------------------
  cem <- cemitool(expr       = expr_mat,
                  annot      = annot_df,
                  filter     = filter,
                  min_ngen   = min_ngen,
                  cor_method = match.arg(cor_method),
                  plot       = TRUE)

  ## 4 · expression + modules ----------------------------------------
  expr_raw <- attr(cem, "expression")
  mod_tbl  <- attr(cem, "module")

  common_genes <- intersect(rownames(expr_raw), mod_tbl$genes)
  expr_raw     <- expr_raw[common_genes, , drop = FALSE]
  gene2mod     <- mod_tbl$modules[match(common_genes, mod_tbl$genes)]
  names(gene2mod) <- common_genes

  ## 5 · eigengenes ---------------------------------------------------
  MEs <- moduleEigengenes(t(expr_raw), colors = gene2mod,
                          impute = TRUE)$eigengenes        # samples × modules

  qs::qsave(cem, "cem.qs");

  ## 7 · JSON summary -------------------------------------------------
  #summaryJSON <- list(module       = colnames(MEs),
  #                    eigengeneVar = apply(MEs, 2, var))
  #write_json(summaryJSON, paste0(imgName, ".json"), pretty = TRUE)

  #invisible(list(cem  = cem,
  #               MEs  = MEs,
  #               dend = hc,
  #               img  = img_file,
  #               json = paste0(imgName, ".json")))
  return(1)
}

PlotCEMiDendro <- function(mode      = "sample",   # "sample" | "module"
                           metaClass = "NA",
                           imgName   = "cem_dendro",
                           dpi       = 72,
                           format    = "png") {
  
  library(Cairo); library(WGCNA)
  
  cem <- qs::qread("cem.qs")
  if (!inherits(cem, "CEMiTool"))
    stop("'cem.qs' does not contain a valid CEMiTool object.")
  
  mode <- match.arg(mode, c("module", "sample"))
  expr <- attr(cem, "expression")       # genes × samples
  mod  <- attr(cem, "module")
  
  # ── helper: plot + diagnostics + legend ─────────────────────────
  plotDendroColoured <- function(hc, colNamed, label, file, legendPal) {
    
    leaves <- labels(as.dendrogram(hc))
    cat("\n--- DEBUG -----------------------------------\n")
    cat("First 6 leaves         :", head(leaves), "\n")
    cat("Leaf count             :", length(leaves), "\n")
    cat("Colour vector length   :", length(colNamed), "\n")
    cat("Names match?           :", all(leaves %in% names(colNamed)), "\n")
    cat("----------------------------------------------\n")
    
    if (!all(leaves %in% names(colNamed)))
      stop("Colour vector missing leaves:\n  ",
           paste(setdiff(leaves, names(colNamed)), collapse = ", "))
    
    colMat <- matrix(colNamed[leaves], nrow = length(leaves), ncol = 1,
                     dimnames = list(leaves, NULL))
    
    if (dpi == 72) dpi <- 96
    Cairo(file, width = 1000, height = 600, dpi = dpi,
          bg = "white", type = format)
    
    oldMar <- par("mar"); par(mar = oldMar + c(0, 0, 0, 4))  # add right margin
    
    plotDendroAndColors(
      dendro       = hc,
      colors       = colMat,
      groupLabels  = label,
      dendroLabels = FALSE,
      addGuide     = TRUE,
      guideHang    = 0.05,
      main         = paste("CEMiTool", label, "dendrogram"),
      ylab         = "1 − Pearson correlation")
    
    ## ---------- legend overlay -----------------------------------
    par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA)
    plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend("topright",
           legend = names(legendPal),
           fill   = legendPal,
           border = NA,
           cex    = 0.8,
           bty    = "n")
    
    par(oldMar)    # restore margins
    dev.off()
  }
  
  # ── MODULE dendrogram ───────────────────────────────────────────
  if (mode == "module") {
    ME  <- moduleEigengenes(t(expr[mod$genes, ]),
                            colors = mod$modules)$eigengenes
    hc  <- hclust(as.dist(1 - cor(ME)), method = "average")
    
    ids   <- colnames(ME)
    pal   <- setNames(rainbow(length(ids)), ids)
    idVec <- setNames(ids, ids)                     # names = leaves
    
    file <- sprintf("%s_module_dendro_dpi%d.%s", imgName, dpi, format)
    plotDendroColoured(hc, idVec, "modules", file, pal)
    return(1)
  }
  
  # ── SAMPLE dendrogram ───────────────────────────────────────────
  if (is.na(metaClass) || metaClass == "NA") metaClass <- 1
  sa       <- cem@sample_annotation
  classes  <- sa[[metaClass]]
  names(classes) <- sa$SampleName
  
  hc <- hclust(as.dist(1 - cor(expr)), method = "average")
  
  pal      <- setNames(rainbow(length(unique(classes))), unique(classes))
  colNamed <- setNames(pal[classes], names(classes))       # names = sample IDs
  
  file <- sprintf("%sdpi%d.%s", imgName, dpi, format)
  plotDendroColoured(hc, colNamed, "sample class", file, pal)
  1
}
