GetFunctionalDetails <- function(data.sorted, gene.matches){
    data.sorted$Adjusted.Pvalue[data.sorted$Adjusted.Pvalue == 1] <- 0.99999
  return(list(
    pathways = data.sorted$name,
    pathways.bmd = data.sorted$bmd.med,
    pathways.pval = data.sorted$Pvalue,
    pathways.adjpval = data.sorted$Adjusted.Pvalue,
    pathways.nodesz = data.sorted$`Observed Hits`/data.sorted$`Gene Set Size`,
    pathways.observedhits = data.sorted$`Observed Hits`,
    gene.matches = gene.matches
  ))
}

PreparePODJSON <- function(fileNm, doseScale, xMin=-Inf, xMax=Inf, geneDB, org){
  #save.image("preparePod.RData");
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  bmdcalc.res <- FilterBMDResults(dataSet)
  
  if(doseScale == "log10"){
    bmdcalc.res[,3:6] <- log10(bmdcalc.res[,3:6])
  } else if(doseScale == "log2"){
    bmdcalc.res[,3:6] <- log2(bmdcalc.res[,3:6])
  }
  
  gene.vec <- as.matrix(bmdcalc.res[which(bmdcalc.res$bmd > xMin & bmdcalc.res$bmd < xMax),1])
  
  if (org == "noAnn") {
    # fill blank gene names with NA
    geneNms <- bmdcalc.res$id[order(bmdcalc.res$bmd)]
    geneNms[geneNms == ""] <- "---"

    res <- density(bmdcalc.res$bmd)

    pod <- list(
      geneIds = bmdcalc.res$id[order(bmdcalc.res$bmd)],
      geneNms = geneNms,
      geneBmds = sort(bmdcalc.res$bmd),
      x = res$x,
      y = res$y,
      Details = NULL
    )

  } else {
    # get gene set-based POD
    gs.POD <- gsPOD(dataSet$omicdata, bmdcalc.res, gene.vec, geneDB)


    #print(head(gs.POD$geneset.stats));
    #prepare pathways summary
    data.sorted = gs.POD$geneset.stats[order(gs.POD$geneset.stats$Adjusted.Pvalue),]
    details <- GetFunctionalDetails(data.sorted, gs.POD$geneset.matches)

    # make out file
    details.out <- as.data.frame(details[-7])
    colnames(details.out) <- c("Name", "pathBMD", "pval", "adj.pval", "perc.path", "num.hits")
    data.table::fwrite(details.out, quote = FALSE, row.names = FALSE, sep = "\t", file="gse.txt");

    resTable <- details.out;
    vis.type <- "curvefit";
    imgSet <- readSet(imgSet, "imgSet");
    rownames(resTable) <- NULL;
    imgSet$enrTables[[vis.type]] <- list()
    imgSet$enrTables[[vis.type]]$table <- resTable;
    imgSet$enrTables[[vis.type]]$library <- geneDB;
    imgSet$enrTables[[vis.type]]$algo <- "Overrepresentation Analysis"
    saveSet(imgSet);

    #prepare pathway table 
    details.50 <- details$gene.matches[names(details$gene.matches) %in% data.sorted$name]
    details.50 <- lapply(details.50, as.list)
    test <- reshape2::melt(details.50)[,c(1,3)]
    colnames(test) <- c("entrez", "pathway")
    test$symbol <- doEntrez2SymbolMapping(test$entrez)
    dataSet$pathway.ids <- test
    
    #generate dataframe with model fit interpolations
    fit.interp <- interpFits();
    qs::qsave(fit.interp, "fit.interp.qs");

    # fill blank gene names with NA
    geneNms <- doEntrez2SymbolMapping(bmdcalc.res$id)[order(bmdcalc.res$bmd)]
    geneNms[geneNms == ""] <- "---"

    res <- density(bmdcalc.res$bmd)


    pod <- list(
      geneIds = bmdcalc.res$id[order(bmdcalc.res$bmd)],
      geneNms = geneNms,
      geneBmds = sort(bmdcalc.res$bmd),
      x = res$x,
      y = res$y,
      Details = details
    )
  }
  
  require(RJSONIO);
  json.obj <- toJSON(pod);
  sink(fileNm);
  cat(json.obj);
  sink();

  RegisterData(dataSet)
  return(1);
}

PlotGeneBMD <- function(gene.id, gene.symbol, scale){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  params <- dataSet$drcfit.obj$fitres.filt[dataSet$drcfit.obj$fitres.filt$gene.id == gene.id,]
  model.nm <- as.vector(params$mod.name)
  b <- as.numeric(as.vector(params$b))
  c <- as.numeric(as.vector(params$c))
  d <- as.numeric(as.vector(params$d))
  if(is.nan(d)){
    d <- 1
  }
  e <- as.numeric(as.vector(params$e))
  if(is.nan(e)){
    e <- 1
  }

  bmd.params <- dataSet$bmdcalc.obj$bmdcalc.res[dataSet$bmdcalc.obj$bmdcalc.res$id == gene.id, ]
  bmd <- bmd.params$bmd
  bmdl <- bmd.params$bmdl
  bmdu <- bmd.params$bmdu
  
  exposure <- dataSet$drcfit.obj$dose
  
  if(scale != "natural"){
    exposure[exposure == 0] <- dataSet$zero.log
  }

  df <- data.frame(Exposure = exposure, Expression = dataSet$data.norm[gene.id,])
  p <- ggplot(data=df, aes(x=Exposure, y=Expression)) + geom_point() + xlab("Concentration") +
    ggtitle(gene.symbol) + theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  if(model.nm == "Poly2"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2))
  }else if(model.nm == "Poly3"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 3))
  }else if(model.nm == "Poly4"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 4))
  }else if(model.nm == "Exp2"){
    exp2fun <- function(e,b,x){return(e*exp(b*x))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp2fun(e,b,x), n = 1000)
  }else if(model.nm == "Exp3"){
    exp3fun <- function(e,b,x,d){return(e*(exp(sign(b)*(abs(b)*x)^d)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp3fun(e,b,x,d), n = 1000)
  }else if(model.nm == "Exp4"){
    exp4fun <- function(e,c,b,x){return(e*(c - (c-1)*exp((-1)*b*x)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp4fun(e,c,b,x), n = 1000)
  }else if(model.nm == "Exp5"){
    exp5fun <- function(e,c,b,x,d){return(e*(c - (c-1)*exp((-1)*(b*x)^d)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp5fun(e,c,b,x,d), n = 1000)
  }else if(model.nm == "Lin"){
    p <- p + geom_smooth(method = "lm", formula = y ~ x)
  }else if(model.nm == "Hill"){
    hillfun <- function(c,d,x,e,b){return(c + (d - c) / (1 + (x/e)^b))}
    p <- p + geom_smooth(method = "lm", formula = y ~ hillfun(c,d,x,e,b), n = 1000)
  }else if(model.nm == "Power"){
    powerfun <- function(e,b,x,c){return(e + b*(x^c))}
    p <- p + geom_smooth(method = "lm", formula = y ~ powerfun(e,b,x,c), n = 1000)
  }

  p <- p + geom_rect(aes(xmin = bmdl, xmax = bmdu, ymin = -Inf, ymax = Inf), fill = "red3", alpha = 0.01) + 
    geom_vline(xintercept = bmdl, linetype = "dashed", color = "red3") + 
    geom_vline(xintercept = bmd, color = "red3") + 
    geom_vline(xintercept = bmdu, linetype = "dashed", color = "red3")

  if(scale == "log2"){
    if(model.nm == "Exp3" | model.nm == "Exp5" | model.nm == "Hill"){
      p <- p + scale_x_continuous(trans='pseudo_log')
    } else {
      p <- p + scale_x_continuous(trans='log2', breaks = unique(exposure), labels = labels)
    }
  } else if(scale == "log10"){
    if(model.nm == "Exp3" | model.nm == "Exp5" | model.nm == "Hill"){
      p <- p + scale_x_continuous(trans='pseudo_log')
    } else {
      p <- p + scale_x_continuous(trans='log10', breaks = unique(exposure), labels = labels)
    }
  }
  
  imgName <- paste("Gene_bmd_", gene.id, ".png", sep="");
  Cairo(file = imgName, width=280, height=320, type="png", bg="white");
  print(p)
  dev.off();

  imgSet <- readSet(imgSet, "imgSet");
  imgSet$PlotGeneBMD <- imgName;
  saveSet(imgSet);
}

PlotGeneDRCurve <- function(gene.id, gene.symbol, model.nm, b, c, d, e, bmdl, bmd, bmdu, scale){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  require(ggplot2)
  require(scales)

  exposure <- dataSet$drcfit.obj$dose
  labels <- unique(exposure)
  
  if(scale != "natural"){
    exposure[exposure == 0] <- dataSet$zero.log
  }

  df <- data.frame(Dose = exposure, Expression = dataSet$data.norm[gene.id,])
  p <- ggplot(data=df, aes(x=Dose, y=Expression)) + geom_point() + ggtitle(gene.symbol) + 
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5, face = "bold"),
             axis.text.x = element_text(angle=90)) + 
       xlab("Concentration") +
       geom_rect(aes(xmin = bmdl, xmax = bmdu, ymin = -Inf, ymax = Inf), fill = "red3", alpha = 0.01) + 
       geom_vline(xintercept = bmdl, linetype = "dashed", color = "red3") + 
       geom_vline(xintercept = bmd, color = "red3") + geom_vline(xintercept = bmdu, linetype = "dashed", color = "red3")
  
  if(model.nm == "Poly2"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2))
  }else if(model.nm == "Poly3"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 3))
  }else if(model.nm == "Poly4"){
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 4))
  }else if(model.nm == "Exp2"){
    exp2fun <- function(e,b,x){return(e*exp(b*x))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp2fun(e,b,x), n = 1000)
  }else if(model.nm == "Exp3"){
    exp3fun <- function(e,b,x,d){return(e*(exp(sign(b)*(abs(b)*x)^d)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp3fun(e,b,x,d), n = 1000)
  }else if(model.nm == "Exp4"){
    exp4fun <- function(e,c,b,x){return(e*(c - (c-1)*exp((-1)*b*x)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp4fun(e,c,b,x), n = 1000)
  }else if(model.nm == "Exp5"){
    exp5fun <- function(e,c,b,x,d){return(e*(c - (c-1)*exp((-1)*(b*x)^d)))}
    p <- p + geom_smooth(method = "lm", formula = y ~ exp5fun(e,c,b,x,d), n = 1000)
  }else if(model.nm == "Lin"){
    p <- p + geom_smooth(method = "lm", formula = y ~ x)
  }else if(model.nm == "Hill"){
    hillfun <- function(c,d,x,e,b){return(c + (d - c) / (1 + (x/e)^b))}
    p <- p + geom_smooth(method = "lm", formula = y ~ hillfun(c,d,x,e,b), n = 1000)
  }else if(model.nm == "Power"){
    powerfun <- function(e,b,x,c){return(e + b*(x^c))}
    p <- p + geom_smooth(method = "lm", formula = y ~ powerfun(e,b,x,c), n = 1000)
  }

  ## ── x-axis scaling ─────────────────────────────────────────────────────────
if (scale == "log2") {

  if (model.nm %in% c("Exp3", "Exp5")) {

    p <- p + scale_x_continuous(trans = scales::pseudo_log_trans(base = 2))

  } else {

    ## powers of 2 between the smallest >0 dose and the max dose
    rng     <- range(exposure[exposure > 0], na.rm = TRUE)
    breaks  <- 2^seq(floor(log2(rng[1])), ceiling(log2(rng[2])), by = 1)
    breaks  <- c(dataSet$zero.log, breaks)
    labelMap <- c("0", format(breaks[-1], scientific = FALSE))

    p <- p + scale_x_continuous(trans = "log2",
                                breaks = breaks,
                                labels = labelMap)
  }

} else if (scale == "log10") {

  if (model.nm %in% c("Exp3", "Exp5")) {

    p <- p + scale_x_continuous(trans = scales::pseudo_log_trans(base = 10))

  } else {

    ## powers of 10 between the smallest >0 dose and the max dose
    rng     <- range(exposure[exposure > 0], na.rm = TRUE)
    breaks  <- 10^seq(floor(log10(rng[1])), ceiling(log10(rng[2])), by = 1)
    breaks  <- c(dataSet$zero.log, breaks)
    labelMap <- c("0", format(breaks[-1], scientific = FALSE))

    p <- p + scale_x_continuous(trans = "log10",
                                breaks = breaks,
                                labels = labelMap)
  }
}

  
  imgName <- paste("Gene_", gene.symbol, "_", model.nm, "_", scale,".png", sep="");
  Cairo(file = imgName, width=280, height=320, type="png", bg="white");
  print(p)
  dev.off();

  imgSet <- readSet(imgSet, "imgSet");
  imgSet$PlotGeneDRCurve <- imgName;
  saveSet(imgSet);
}

PlotDRModelBars <- function(imgNm, dpi, format){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  require(ggplot2)
  p <- ggplot(dataSet$bmdcalc.obj$bmdcalc.res, aes(x = mod.name, fill = as.character(all.pass))) + geom_bar(position = "stack") + 
        scale_fill_manual(values = c("#282726","#6A8A82"), labels = c("No","Yes"), name = "BMD convergence?") + theme_bw()
  p <- p + xlab("Best fit model") + ylab("Count") + theme(axis.text.x = element_text(face="bold"), legend.position = "bottom")
  
  imgNm = paste(imgNm, "dpi", dpi, ".", format, sep="");
  Cairo (file=imgNm, width=8, height=6, unit="in",dpi=300, type=format, bg="white");
  print(p)
  dev.off();

  imgSet <- readSet(imgSet, "imgSet");
  imgSet$PlotDRModelBars <- imgNm;
  saveSet(imgSet);
}
PlotDRFilterSummary <- function(imgNm, dpi = 72, format = "png") {

  ## 1 ── Load data -----------------------------------------------------------
  paramSet <- readSet(paramSet, "paramSet")
  dataSet  <- readDataset(paramSet$dataName)

  require(ggplot2)
  require(Cairo)

  ## 2 ── Build counts --------------------------------------------------------
  dres   <- dataSet$bmdcalc.obj$bmdcalc.res
  fitres <- dataSet$bmdcalc.obj$fitres.bmd

  n_total   <- length(dataSet$itemselect$item)      # “Initial” = sig. features
  n_fitted  <- nrow(dres)                           # “Fitted”  = BMD-modelled

  mask_conv   <- dres$conv.pass
  mask_ci1    <- mask_conv & dres$CI.pass
  mask_ci2    <- mask_ci1  & dres$CI.pass2
  mask_low    <- mask_ci2  & dres$ld.pass
  mask_high   <- mask_low  & dres$hd.pass

  lof_thr <- 0.05
  fit_ok  <- rep(FALSE, n_fitted)
  hit     <- match(dres$id, fitres$gene.id, nomatch = 0)
  fit_ok[hit > 0] <- fitres$lof.p[hit] >= lof_thr
  mask_fitp <- mask_high & fit_ok

  counts <- c(
    Initial      = n_total,
    Fitted       = n_fitted,
    `BMD/BMDL`   = sum(mask_conv),
    `BMDU/BMDL`  = sum(mask_ci1),
    `BMDU/BMD`   = sum(mask_ci2),
    Lowdose      = sum(mask_low),
    Highdose     = sum(mask_high),
    FitP         = sum(mask_fitp),
    All          = sum(mask_fitp)
  )

  pct <- round(counts / n_total * 100)

  df <- data.frame(
    step    = factor(names(counts), levels = rev(names(counts))), # reverse for ggplot
    count   = counts,
    percent = pct
  )

  ## 3 ── Build the bar plot --------------------------------------------------
  p <- ggplot(df, aes(x = step, y = count, fill = percent)) +
    geom_col(width = 0.7, colour = "black") +
    geom_text(aes(label = paste0(count, " (", percent, "%)")),
              hjust = 1.05, colour = "white", size = 5.0) +     # was 3.8
    scale_fill_gradient(low = "#6AA2AD", high = "#C5D97B") +
    coord_flip(clip = "off") +
    labs(x = NULL, y = "Features Remaining") +
    theme_minimal(base_size = 14) +                              # was 11
    theme(
      legend.position = "none",
      plot.margin = margin(10, 40, 10, 120),   # extra left margin for long labels
      axis.text.y = element_text(face = "bold", size = 13),      # +3 pt
      axis.text.x = element_text(size = 12)                      # clearer axis
    )

  ## 4 ── Save & register -----------------------------------------------------
  outFile <- paste0(imgNm, "dpi", dpi, ".", format)
  if (dpi == 72) dpi <- 96
  Cairo(file = outFile, width = 8, height = 6, unit = "in",
        dpi = dpi, type = format, bg = "white")
  print(p)
  dev.off()

  imgSet <- readSet(imgSet, "imgSet")
  imgSet$PlotDRFilterSummary <- outFile
  saveSet(imgSet)
}


PlotDRHistogram <- function(imgNm,
                            dpi,
                            format,
                            units,
                            scale,
                            width = NA) {

  ## ── 1 · Load data ───────────────────────────────────────────────────────
  paramSet <- readSet(paramSet, "paramSet")
  dataSet  <- readDataset(paramSet$dataName)

  require(ggplot2)
  require(Cairo)

  ## ── 2 · Compute tPOD / mPOD values ──────────────────────────────────────
  s.pods <- sensPOD(pod = c("feat.20", "feat.10th", "mode"), scale)

  ## ── 3 · Filter features that passed all BMD criteria ────────────────────
  bmd.hist <- subset(dataSet$bmdcalc.obj$bmdcalc.res, all.pass)
  bmd.vals <- bmd.hist$bmd

  ## ── 4 · Optional log-transformation of BMDs and PODs ────────────────────
  if (scale == "log10") {
    bmd.vals <- log10(bmd.vals)
    s.pods   <- log10(s.pods)
    xTitle   <- "log10(Feature-level BMD)"
  } else if (scale == "log2") {
    bmd.vals <- log2(bmd.vals)
    s.pods   <- log2(s.pods)
    xTitle   <- "log2(Feature-level BMD)"
  } else {
    xTitle <- "Feature-level BMD"
  }

  bmd.df <- data.frame(bmd = bmd.vals)

  ## ── 5 · Colour palette & legend labels (copied from liked style) ────────
  pod.cols <- c(
    gene20         = "#D62728",
    percentile10th = "#2CA02C",
    mode           = "#FF7F0E"
  )

  legend.labels <- c(
    paste0("20th feature: ",    ifelse(is.finite(s.pods["feat.20"]),    signif(s.pods["feat.20"], 2), "NA")),
    paste0("Max 1st peak: ",    ifelse(is.finite(s.pods["mode"]),       signif(s.pods["mode"],     2), "NA")),
    paste0("10th percentile: ", ifelse(is.finite(s.pods["feat.10th"]),  signif(s.pods["feat.10th"],2), "NA"))
  )

  ## ── 6 · Build the histogram (styling cloned) ────────────────────────────
  p <- ggplot(bmd.df, aes(x = bmd)) +
    geom_histogram(aes(y = after_stat(count)),
                   bins   = 30,
                   fill   = "lightblue",
                   colour = "black",
                   alpha  = 0.85) +
    {if (is.finite(s.pods["feat.20"]))
        geom_vline(aes(xintercept = s.pods["feat.20"], colour = "gene20"), size = 1)} +
    {if (is.finite(s.pods["mode"]))
        geom_vline(aes(xintercept = s.pods["mode"], colour = "mode"), size = 1)} +
    {if (is.finite(s.pods["feat.10th"]))
        geom_vline(aes(xintercept = s.pods["feat.10th"], colour = "percentile10th"), size = 1)} +
    scale_color_manual(
      name   = "tPOD",
      values = pod.cols,
      labels = legend.labels
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    theme_bw(base_size = 11 * 1.3) +
    xlab(xTitle) +
    ylab("Count") +
    theme(
      axis.text.x       = element_text(face = "bold"),
      legend.position   = "right",
      legend.direction  = "vertical",
      legend.box.margin = margin(t = 6, l = 8),
      plot.margin       = margin(t = 10, r = 8, b = 5, l = 8),
      legend.title      = element_text(size = rel(1)),
      legend.text       = element_text(size = rel(0.9))
    )

  ## ── 7 · Figure size (same logic) ────────────────────────────────────────
  if (is.na(width) || width <= 0) {
    w <- 10;               # default width (inches)
    h <- 7;            # 3:1.75 aspect from original
  } else {
    w <- width
    h <- width * 0.75
  }

  imgFile <- paste0(imgNm, "dpi", dpi, ".", format)

  ## ── 8 · Render & save ───────────────────────────────────────────────────
  Cairo(file   = imgFile,
        width  = w,
        height = h,
        unit   = "in",
        dpi    = dpi,
        type   = format,
        bg     = "white")          # solid white background (matches style)
  print(p)
  dev.off()

  ## ── 9 · Register in imgSet & return mSetObj (unchanged) ────────────────
  imgSet <- readSet(imgSet, "imgSet")
  imgSet$PlotDRHistogram <- imgNm
  saveSet(imgSet)
}



PlotPWHeatmap <- function(pathway, pwcount, units){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

    require(pheatmap)
    require(grid)

    pws <- dataSet$pathway.ids
    fit.interp <- qs::qread("fit.interp.qs");
    bmd.vals <- dataSet$html.resTable[,c(1,5)]

    pw.genes <- pws[pws$pathway == pathway,][,c(1,3)]

    pw.data <- base::merge(pw.genes, fit.interp, by.x = "entrez", by.y = "row.names", all = FALSE)
    pw.data <- base::merge(bmd.vals, pw.data, by.x = "gene.id", by.y = "entrez", all = FALSE)
    pw.data <- distinct(pw.data)
    rownames(pw.data) <- pw.data$symbol
    sym.bmd <- pw.data[,c(2:3)]
    pw.data <- pw.data[,-c(1:3)]

    # reduce the number of significant digits
    labels.col <- as.numeric(colnames(pw.data))
    labels.col <- signif(labels.col, 2)

    # make labels to display on heatmap
    inds <- c(seq(1,100,by=20),100)
    labs.col <- rep("", 100)
    labs.col[inds] <- labels.col[inds]

    # find the index of each bmd
    bmd.ind <- unlist(lapply(sym.bmd[,1], function(x){min(which(labels.col > x))}))
    names(bmd.ind) <- sym.bmd$symbol

    # convert down regulated - version 2
    down.reg <- pw.data[,1] > 0.25
    names(down.reg) <- rownames(pw.data)
    pw.data[down.reg,] <- -1*(pw.data[down.reg,] - 1)

    # re-order data (BMD)
    pw.data <- pw.data[order(bmd.ind),]
    down.reg <- down.reg[order(bmd.ind)]
    bmd.ind <- bmd.ind[order(bmd.ind)]

    # change color of bmd cells
    for(i in c(1:dim(pw.data)[1])){
      pw.data[i, bmd.ind[i]] <- 1.01
    }

    # Change names of factors
    down.reg[down.reg==TRUE] <- "Down"
    down.reg[down.reg==FALSE] <- "Up"
    ann.row <- data.frame(Regulation = as.factor(down.reg))

    # Specify colors
    ann.cols <- list(Regulation = c(Up = "#821717", Down = "#4c743e"))[1]
    cell.cols <- c(colorRampPalette(c("#5c998a", "#e6a519", "#b2343e"))(100), "#000000")

    # get x-axis label
    xlab <- paste0("Concentration (", units, ")")

    # print out image
    imgNm <- paste("Pw_", pwcount, ".png", sep="");
    Cairo (file=imgNm, width=8, height=8, unit="in",dpi=300, type="png", bg="white");
    setHook("grid.newpage", function() pushViewport(viewport(x=1,y=0.95,width=1, height=0.9, name="vp", just=c("right","top"))), action="prepend")
    pheatmap(pw.data, cluster_cols = FALSE, cluster_rows = FALSE, show_rownames = TRUE, show_colnames = TRUE, 
             border_color = NA, labels_col = labs.col, annotation_row = ann.row, legend = FALSE,
             color = cell.cols, annotation_colors = ann.cols, annotation_names_row = FALSE)
    setHook("grid.newpage", NULL, "replace")
    grid.text(xlab, y=-0.02, x = 0.4, gp=gpar(fontsize=12))
    grid.text(pathway, y = 1.02, x = 0.5, gp=gpar(fontsize=14, fontface="bold"))
    dev.off();

    imgSet <- readSet(imgSet, "imgSet");
    imgSet$PlotPWHeatmap <- imgNm;
    saveSet(imgSet);
}

###################
###################

PlotDRHistogramOld <- function(imgNm, dpi, format, units, scale){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  require(ggplot2)
  s.pods <- sensPOD(pod = c("feat.20", "feat.10th", "mode"), scale)
  
  bmd.hist <- dataSet$bmdcalc.obj$bmdcalc.res[dataSet$bmdcalc.obj$bmdcalc.res$all.pass,]

  if(scale == "log10"){
    dens <- density(log10(bmd.hist$bmd));
    xTitle <- paste0("log10(Gene-level BMD) (", units, ")")
  } else if(scale == "log2"){
    dens <- density(log2(bmd.hist$bmd));
    xTitle <- paste0("log2(Gene-level BMD) (", units, ")")
  } else {
    dens <- density(bmd.hist$bmd);
    xTitle <- paste0("Gene-level BMD (", units, ")")
  }
  
  dens <- data.frame(x = dens$x, y = dens$y)
  
  require(ggplot2)
  p <- ggplot(aes(x = x, y = y), data = dens) + 
    geom_area() +
    geom_vline(aes(xintercept = s.pods[2], colour = "percentile10th"), size = 1) +
    geom_vline(aes(xintercept = s.pods[3], colour = "mode"), size = 1) +
    geom_vline(aes(xintercept = s.pods[1], colour = "gene20"), size = 1) +
    scale_color_manual(name = "tPOD", 
                       values = c(gene20 = "#A7414A", percentile10th = "#6A8A82", mode = "#CC9B31"),
                       labels = c(paste0("20th gene: ", signif(s.pods[1],2)), 
                                  paste0("Max 1st peak: ", signif(s.pods[3],2)),
                                  paste0("10th percentile: ", signif(s.pods[2],2)))) + 
    theme_bw()
  p <- p + xlab(xTitle) + ylab("Density function") + 
    theme(axis.text.x = element_text(face="bold"), legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")

  imgNm = paste(imgNm, "dpi", dpi, ".", format, sep="");
  Cairo (file=imgNm, width=8, height=6, unit="in",dpi=300, type=format, bg="white");
  print(p)
  dev.off();

  imgSet <- readSet(imgSet, "imgSet");
  imgSet$PlotDRHistogram <- imgNm;
  saveSet(imgSet);
}
