##################################################
## R script for ExpressAnalyst
## Description: Coexpression Network
## Author: G. Zhou, guangyan.zhou@mail.mcgill.ca
###################################################


#' Build a weighted gene co‑expression network and store results
#'
#' All key parameters are passed directly from the Java CoexpBean
#' (`String.format(...)`) so there is no need to fetch anything from
#' `paramSet` except for descriptive metadata.  The results are saved
#' into `analSet$coexp`, a JSON summary, and (optionally) a dendrogram
#' PNG for quick inspection on the client side.
#'
#' @param dataName   name of data file
#' @param power       Soft‑thresholding power; if NULL and `auto_power`
#'                     is TRUE the function auto‑picks (R² ≥ 0.9)
#' @param cor_func    Correlation function: "pearson", "bicor", "spearman"
#' @param network_type "signed", "signed_hybrid", "unsigned"
#' @param deepSplit   DynamicTreeCut sensitivity (0–4)
#' @param minModuleSize Minimum module size
#' @param mergeCutHeight Merge threshold (0–1)
#' @param maxBlockSize  Gene block size for memory control
#' @param n_threads     CPU threads for WGCNA
#' @param filters       list(counts = c(minCounts,minSamples), var = varQuantile)
#'                      – kept for backward compatibility, but defaults
#'                        should all be zeros because filtering is done
#'                        earlier in the workflow.
#' @param auto_power  Logical flag for automatic power selection
#' @param enrichFDR   FDR cut‑off for module enrichment (GWENA)
#' @param imgName     Basename (no extension) for graphics output
#'                    – e.g. "coexp_dendro" will write PNG & JSON files
#' @param dpi, format Graphics parameters for PNG export
#' @return            1 on success (mirrors Volcano.Anal)
#' @export
my.build.coexp.net <- function(dataName,
                               power = NULL,
                               cor_func = c("pearson", "bicor", "spearman"),
                               network_type = c("signed", "signed_hybrid", "unsigned"),
                               deepSplit = 2,
                               minModuleSize = 30,
                               mergeCutHeight = 0.25,
                               maxBlockSize = 5000,
                               n_threads = 8,
                               filters = list(counts = c(0, 0), var = 0),
                               auto_power = TRUE,
                               enrichFDR = 0.05,
                               imgName = "coexp_dendro",
                               dpi = 72,
                               format = "png") {
  
  ## 0 Persist session for debugging -------------------------------------
  save.image("coexp.RData")
  
  ## 1 Load workflow state objects ---------------------------------------
  paramSet <- readSet(paramSet, "paramSet")
  analSet  <- readSet(analSet,  "analSet")
  msgSet   <- readSet(msgSet,   "msgSet")
  imgSet   <- readSet(imgSet,   "imgSet")
  
  dataSet  <- readDataset(dataName)
  datExpr  <- as.matrix(dataSet$data.norm)
  stopifnot(is.numeric(datExpr))
  
  ## 2 Packages & threads -------------------------------------------------
  suppressPackageStartupMessages({
    library(WGCNA)
    library(fastcluster)
    if (requireNamespace("GWENA", quietly = TRUE)) library(GWENA)
    library(Cairo)
    library(jsonlite)
  })
  enableWGCNAThreads(nThreads = n_threads)
  
  cor_fnc   <- if (cor_func == "bicor") "bicor" else "cor"       # for pickSoftThreshold()
  cor_opts  <- if (cor_func == "spearman") list(method = "spearman") else list()
  
  cor_type  <- if (cor_func == "bicor") "bicor" else "pearson"   # for blockwiseModules()
  
  
  ## 4 Automatic power selection (optional) ------------------------------
  if (isTRUE(auto_power) || is.null(power)) {
    sft <- pickSoftThreshold(
      datExpr,
      powerVector = 1:30,
      corFnc      = cor_fnc,
      corOptions  = cor_opts,
      networkType = network_type,
      verbose     = 0)
    idx   <- which(sft$fitIndices[, "SFT.R.sq"] >= 0.90)[1]
    if (is.na(idx)) idx <- which.max(sft$fitIndices[, "SFT.R.sq"])
    power <- sft$fitIndices[idx, "Power"]
  }
  msgSet$current.msg <- paste0("[Coexp] Using soft-threshold power = ", power)
  
  ## 5 Build network ------------------------------------------------------
  net <- blockwiseModules(
    datExpr,
    power            = power,
    networkType      = network_type,
    deepSplit        = deepSplit,
    minModuleSize    = minModuleSize,
    mergeCutHeight   = mergeCutHeight,
    maxBlockSize     = maxBlockSize,
    corType          = cor_type,  
    corFnc           = cor_fnc,
    corOptions       = cor_opts,
    hclustMethod     = "average",
    saveTOMs         = FALSE,
    verbose          = 0)
  
  
  ## 6 Eigengenes & kME ---------------------------------------------------
  ME        <- net$MEs <- moduleEigengenes(datExpr, net$colors)$eigengenes
  net$kME   <- signedKME(datExpr, ME, outputColumnName = "kME")
  
  ## 7 Optional enrichment via GWENA -------------------------------------
  if (!is.null(enrichFDR) && requireNamespace("GWENA", quietly = TRUE)) {
    net$enrichment <- tryCatch(
      GWENA::module_enrich(colours  = net$colors,
                           organism = paramSet$data.org %||% "hsapiens",
                           fdr      = enrichFDR,
                           verbose  = FALSE),
      error = function(e) NULL)
  }
  
  ## 8 Save state & JSON summary -----------------------------------------
  analSet$coexp <- net
  saveSet(analSet, "analSet")
  saveSet(msgSet,  "msgSet")
  
  summaryJSON <- list(
    module       = names(table(net$colors)),
    size         = as.integer(table(net$colors)),
    eigengeneVar = apply(ME, 2, var))
  jsonNm <- paste0(imgName, ".json")
  write_json(summaryJSON, jsonNm, pretty = TRUE)
  msgSet$partialToBeSaved <- c(msgSet$partialToBeSaved, jsonNm)
  
  ## 9 Dendrogram graphic -------------------------------------------------
  dendroNm <- paste(imgName, "dpi", dpi, ".", format, sep="");
  print(paste("dendroNm", dendroNm));
  Cairo(file = dendroNm, unit = "px", dpi = dpi,
        width = 1000, height = 600, type = format, bg = "white")
  plotDendroAndColors(net$dendrograms[[1]],
                      net$colors[net$blockGenes[[1]]],
                      "Module colors",
                      dendroLabels = FALSE,
                      hang         = 0.03,
                      addGuide     = TRUE,
                      guideHang    = 0.05)
  dev.off()
  
  imgSet$coexpDendro <- dendroNm
  saveSet(imgSet, "imgSet")
  
  msgSet$current.msg <- paste0(msgSet$current.msg,
                               " – Co-expression network built OK.")
  saveSet(msgSet, "msgSet")
  
  return(1)
}
