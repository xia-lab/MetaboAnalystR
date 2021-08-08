#' my.fgsea
#'
#' @param mSetObj mSetObj
#' @param pathways pathways
#' @param stats stats
#' @param ranks ranks
#' @param nperm nperm
#' @param minSize minSize
#' @param maxSize maxSize
#' @param nproc nproc
#' @param gseaParam gseaParam
#' @param BPPARAM BPPARAM
#' @noRd
#' @importFrom data.table data.table rbindlist
#' @return

my.fgsea <- function(mSetObj, pathways, stats, ranks,
                   nperm,
                   minSize=1, maxSize=Inf,
                   nproc=0,
                   gseaParam=1,
                   BPPARAM=NULL) {
  
  # Warning message for ties in stats
  ties <- sum(duplicated(stats[stats != 0]))
  if (ties != 0) {
    warning("There are ties in the preranked stats (",
            paste(round(ties * 100 / length(stats), digits = 2)),
            "% of the list).\n",
            "The order of those tied m/z features will be arbitrary, which may produce unexpected results.")
  }
  
  # Warning message for duplicate gene names
  if (any(duplicated(names(stats)))) {
    warning("There are duplicate m/z feature names, fgsea may produce unexpected results")
  }
  
  granularity <- 1000
  permPerProc <- rep(granularity, floor(nperm / granularity))
  if (nperm - sum(permPerProc) > 0) {
    permPerProc <- c(permPerProc, nperm - sum(permPerProc))
  }
  set.seed(123)
  seeds <- sample.int(10^9, length(permPerProc))
  
  if (is.null(BPPARAM)) {
    if (nproc != 0) {
      if (.Platform$OS.type == "windows") {
        # windows doesn't support multicore, using snow instead
        BPPARAM <- BiocParallel::SnowParam(workers = nproc)
      } else {
        BPPARAM <- BiocParallel::MulticoreParam(workers = nproc)
      }
    } else {
      BPPARAM <- BiocParallel::bpparam()
    }
  }
  
  minSize <- max(minSize, 1)
  stats <- abs(stats) ^ gseaParam
  
  # returns list of indexs of matches between pathways and rank names
  pathwaysPos <- lapply(pathways, function(p) { as.vector(na.omit(fmatch_r(p, names(ranks)))) })
  pathwaysFiltered <- lapply(pathwaysPos, function(s) { ranks[s] })
  qs::qsave(pathwaysFiltered, "pathwaysFiltered.qs")
  
  # adjust for the fact that a single m/z feature can match to several compound identifiers (not when in EC space)
  # subsets m/z features responsible for a compound and matches it to total set of matched m/z features
  # returns the length
  
  matched_res <- qs::qread("mum_res.qs");
  
  if(mSetObj$paramSet$mumRT){
    pathwaysSizes <- sapply(pathwaysFiltered, length)
  }else{
    pathway2mzSizes <- sapply(pathways, function(z) { length(intersect(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[z]))), unique(matched_res[,1])))} )
    oldpathwaysSizes <- sapply(pathwaysFiltered, length)
    pathwaysSizes <- pmin(pathway2mzSizes, oldpathwaysSizes)
  }
  
  toKeep <- which(minSize <= pathwaysSizes & pathwaysSizes <= maxSize)
  m <- length(toKeep)
  
  if (m == 0) {
    return(data.table::data.table(pathway=character(),
                                  pval=numeric(),
                                  padj=numeric(),
                                  ES=numeric(),
                                  NES=numeric(),
                                  nMoreExtreme=numeric(),
                                  size=integer(),
                                  leadingEdge=list()))
  }
  
  pathwaysFiltered <- pathwaysFiltered[toKeep]
  pathwaysSizes <- pathwaysSizes[toKeep]
  
  K <- max(pathwaysSizes)
  
  #perform gsea
  gseaStatRes <- do.call(rbind,
                         lapply(pathwaysFiltered, fgsea::calcGseaStat,
                                stats=stats,
                                returnLeadingEdge=TRUE))
  
  leadingEdges <- mapply("[", list(names(stats)), gseaStatRes[, "leadingEdge"], SIMPLIFY = FALSE)
  pathwayScores <- unlist(gseaStatRes[, "res"])
  
  #perform permutations
  universe <- seq_along(stats)
  set.seed(123)
  counts <- BiocParallel::bplapply(seq_along(permPerProc), function(i) {
    nperm1 <- permPerProc[i]
    leEs <- rep(0, m)
    geEs <- rep(0, m)
    leZero <- rep(0, m)
    geZero <- rep(0, m)
    leZeroSum <- rep(0, m)
    geZeroSum <- rep(0, m)
    if (m == 1) {
      for (i in seq_len(nperm1)) {
        randSample <- sample.int(length(universe), K)
        randEsP <- fgsea::calcGseaStat(
          stats = stats,
          selectedStats = randSample,
          gseaParam = 1)
        leEs <- leEs + (randEsP <= pathwayScores)
        geEs <- geEs + (randEsP >= pathwayScores)
        leZero <- leZero + (randEsP <= 0)
        geZero <- geZero + (randEsP >= 0)
        leZeroSum <- leZeroSum + pmin(randEsP, 0)
        geZeroSum <- geZeroSum + pmax(randEsP, 0)
      }
    } else {
      if (packageVersion("fgsea") > "1.12.0"){
        aux <- fgsea:::calcGseaStatCumulativeBatch(
          stats = stats,
          gseaParam = 1,
          pathwayScores = pathwayScores,
          pathwaysSizes = pathwaysSizes,
          iterations = nperm1,
          seed = seeds[i],
          scoreType = "std")} else {
            aux <- fgsea:::calcGseaStatCumulativeBatch(
              stats = stats,
              gseaParam = 1,
              pathwayScores = pathwayScores,
              pathwaysSizes = pathwaysSizes,
              iterations = nperm1,
              seed = seeds[i])
          }
      leEs = get("leEs", aux)
      geEs = get("geEs", aux)
      leZero = get("leZero", aux)
      geZero = get("geZero", aux)
      leZeroSum = get("leZeroSum", aux)
      geZeroSum = get("geZeroSum", aux)
    }
    data.table::data.table(pathway=seq_len(m),
                           leEs=leEs, geEs=geEs,
                           leZero=leZero, geZero=geZero,
                           leZeroSum=leZeroSum, geZeroSum=geZeroSum
    )
  }, BPPARAM=BPPARAM)
  
  counts <- data.table::rbindlist(counts)
  
  # Getting rid of check NOTEs
  leEs=leZero=geEs=geZero=leZeroSum=geZeroSum=NULL
  pathway=padj=pval=ES=NES=geZeroMean=leZeroMean=NULL
  nMoreExtreme=nGeEs=nLeEs=size=NULL
  leadingEdge=NULL
  .="damn notes"
  
  pval <- unlist(lapply(counts$pathway, function(c) min((1+sum(counts[c,]$leEs)) / (1 + sum(counts[c,]$leZero)),
                                                        (1+sum(counts[c,]$geEs)) / (1 + sum(counts[c,]$geZero)))))
  
  leZeroMean <- unlist(lapply(counts$pathway, function(d) sum(counts[d,]$leZeroSum) / sum(counts[d,]$leZero)))
  geZeroMean <- unlist(lapply(counts$pathway, function(e) sum(counts[e,]$geZeroSum) / sum(counts[e,]$geZero)))
  nLeEs <- unlist(lapply(counts$pathway, function(f) sum(counts[f,]$leEs)))
  nGeEs <- unlist(lapply(counts$pathway, function(g) sum(counts[g,]$geEs)))
  
  pvals <- data.frame(pval=pval, leZeroMean=leZeroMean, geZeroMean=geZeroMean, nLeEs=nLeEs, nGeEs=nGeEs)
  
  padj <- p.adjust(pvals$pval, method="fdr")
  ES <- pathwayScores
  NES <- ES / ifelse(ES > 0, pvals$geZeroMean, abs(pvals$leZeroMean))
  pvals$leZeroMean <- NULL
  pvals$geZeroMean <- NULL
  
  nMoreExtreme <- ifelse(ES > 0, pvals$nGeEs, pvals$nLeEs)
  pvals$nLeEs <- NULL
  pvals$nGeEs <- NULL
  
  size <- pathwaysSizes
  pathway <- names(pathwaysFiltered)
  
  leadingEdge <- sapply(leadingEdges, paste0, collapse = "; ")
  leadingEdge2 <- sapply(leadingEdge, function(x) strsplit(x, "; "))
  pathway.cpds <- sapply(pathwaysFiltered, attributes)
  
  matches <- mapply(intersect, leadingEdge2, pathway.cpds)
  
  leadingEdgeMatched <- sapply(matches, paste0, collapse = "; ")
  
  pvals.done <- cbind(pathway, pvals, padj, ES, NES, nMoreExtreme, size, leadingEdgeMatched)
  
  return(pvals.done)
}
