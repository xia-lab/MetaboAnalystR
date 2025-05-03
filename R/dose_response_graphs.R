########################
# Methods for dose response graphics, 
# adapted from ExpressAnalystR
# Jeff Xia (jeff.xia@xialab.ca)
###########################################

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

PlotMetaboliteDRCurve <- function(mSetObj=NA, feat.id, feat.lbl, model.nm, b, c, d, e, bmdl, bmd, bmdu, scale, dpi=72, format="png"){
  mSetObj <- .get.mSet(mSetObj)
  dataSet <- mSetObj$dataSet
  cls.type <- dataSet$cls.type
  data <- t(dataSet$norm)

  require(ggplot2)
  require(scales)

  exposure <- dataSet$drcfit.obj$dose
  labels <- unique(exposure)

  if(scale != "natural"){
    exposure[exposure == 0] <- dataSet$zero.log
  }

  # Main data
  df <- data.frame(Dose = exposure, Expression = data[feat.id,])

  # Base plot
  p <- ggplot(data = df, aes(x = Dose, y = Expression)) +
       geom_point(size = 2) +
       ggtitle(feat.lbl) +
       theme_bw() +
       theme(plot.title = element_text(hjust = 0.5, face = "bold"),
             axis.text.x = element_text(angle = 90)) +
       xlab("Dose") +
       ylab("Expression") +
       geom_rect(aes(xmin = bmdl, xmax = bmdu, ymin = -Inf, ymax = Inf), fill = "red3", alpha = 0.015) +
       geom_vline(xintercept = bmdl, linetype = "dashed", color = "red3") +
       geom_vline(xintercept = bmd, color = "red3") +
       geom_vline(xintercept = bmdu, linetype = "dashed", color = "red3")

  if(cls.type == "cont"){
    # ==== CONTINUOUS DESIGN ====
    # Use Hill model form: c + (d - c)/(1 + (x/e)^b)
    pred_range <- seq(min(exposure), max(exposure), length.out = 200)
    pred_vals <- c + (d - c) / (1 + (pred_range / e)^b)
    df.pred <- data.frame(Dose = pred_range, Expression = pred_vals)

    p <- p + geom_line(data = df.pred, aes(x = Dose, y = Expression), color = "blue", linewidth = 1)

  } else {
    # ==== DISCRETE DESIGN ====
    if(model.nm == "Poly2"){
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2))
    } else if(model.nm == "Poly3"){
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 3))
    } else if(model.nm == "Poly4"){
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 4))
    } else if(model.nm == "Exp2"){
      exp2fun <- function(e, b, x) e * exp(b * x)
      p <- p + geom_smooth(method = "lm", formula = y ~ exp2fun(e, b, x), n = 1000)
    } else if(model.nm == "Exp3"){
      exp3fun <- function(e, b, x, d) e * (exp(sign(b) * (abs(b) * x)^d))
      p <- p + geom_smooth(method = "lm", formula = y ~ exp3fun(e, b, x, d), n = 1000)
    } else if(model.nm == "Exp4"){
      exp4fun <- function(e, c, b, x) e * (c - (c - 1) * exp(-b * x))
      p <- p + geom_smooth(method = "lm", formula = y ~ exp4fun(e, c, b, x), n = 1000)
    } else if(model.nm == "Exp5"){
      exp5fun <- function(e, c, b, x, d) e * (c - (c - 1) * exp((-1) * (b * x)^d))
      p <- p + geom_smooth(method = "lm", formula = y ~ exp5fun(e, c, b, x, d), n = 1000)
    } else if(model.nm == "Lin"){
      p <- p + geom_smooth(method = "lm", formula = y ~ x)
    } else if(model.nm == "Hill"){
      hillfun <- function(c, d, x, e, b) c + (d - c) / (1 + (x/e)^b)
      p <- p + geom_smooth(method = "lm", formula = y ~ hillfun(c, d, x, e, b), n = 1000)
    } else if(model.nm == "Power"){
      powerfun <- function(e, b, x, c) e + b * (x^c)
      p <- p + geom_smooth(method = "lm", formula = y ~ powerfun(e, b, x, c), n = 1000)
    }
  }

  # Axis scaling
  if(scale == "log2"){
    if(model.nm %in% c("Exp3", "Exp5")){
      p <- p + scale_x_continuous(trans = 'pseudo_log')
    } else {
      p <- p + scale_x_continuous(trans = 'log2', breaks = unique(exposure), labels = labels)
    }
  } else if(scale == "log10"){
    if(model.nm %in% c("Exp3", "Exp5")){
      p <- p + scale_x_continuous(trans = 'pseudo_log')
    } else {
      p <- p + scale_x_continuous(trans = 'log10', breaks = unique(exposure), labels = labels)
    }
  }

  # Save the plot
  feat.id <- CleanNames(feat.id)
  imgName <- paste("Metabolite_", feat.id, "_", model.nm, ".", format, sep = "")
  Cairo::Cairo(file = imgName, width = 5.25, height = 6, type = format, unit = "in", dpi = dpi, bg = "white")
  print(p)
  dev.off()

  dataSet$PlotMetaboliteDRCurve <- imgName
  mSetObj$dataSet <- dataSet

  if(.on.public.web){
    .set.mSet(mSetObj)
    return(imgName)
  } else {
    return(.set.mSet(mSetObj))
  }
}


PlotDRModelBars <- function(mSetObj=NA, imgNm, dpi=72, format="png"){
  mSetObj <- .get.mSet(mSetObj)
  dataSet <- mSetObj$dataSet
  cls.type <- dataSet$cls.type

  bmd.res <- dataSet$bmdcalc.obj$bmdcalc.res

  # Determine column structure and fill in missing models accordingly
  existing_models <- unique(bmd.res$mod.name)
  missing_models <- setdiff(models, existing_models)

  for (model in missing_models) {
    if (cls.type == "cont") {
      # Continuous version: no 'bmr' column
      missing_row <- data.frame(
        feature.id = paste0(model, "_placeholder"),
        mod.name = model,
        bmd = NA, bmdl = NA, bmdu = NA,
        conv.pass = FALSE, hd.pass = FALSE,
        CI.pass = FALSE, ld.pass = FALSE,
        all.pass = FALSE
      )
    } else {
      # Discrete version: includes 'bmr'
      missing_row <- data.frame(
        id = paste0(model, "Sample"),
        mod.name = model,
        bmd = NA, bmdl = NA, bmdu = NA, bmr = NA,
        conv.pass = FALSE, hd.pass = FALSE,
        CI.pass = FALSE, ld.pass = FALSE,
        all.pass = FALSE
      )
    }
    bmd.res <- rbind(bmd.res, missing_row)
  }

  # Generate the plot
  require(ggplot2)
  p <- ggplot(bmd.res, aes(x = mod.name, fill = as.character(all.pass))) +
       geom_bar(position = "stack") +
       scale_fill_manual(values = c("#3d3c3b", "#0099FF"),
                         labels = c("No", "Yes"),
                         name = "BMD convergence?") +
       theme_bw() +
       xlab("Best fit model") +
       ylab("Count") +
       theme(axis.text.x = element_text(face = "bold"),
             legend.position = "bottom")

  # Output image
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep = "")
  if(dpi == 72){
    dpi = 96;
  }
  Cairo::Cairo(file = imgNm, width = 8, height = 6, unit = "in", dpi = dpi, type = format, bg = "white")
  print(p)
  dev.off()

  mSetObj$imgSet$PlotDRModelBars <- imgNm
  return(.set.mSet(mSetObj))
}

PlotDRHistogram <- function(mSetObj=NA, imgNm, dpi, format, units, scale, width=NA) {
  mSetObj <- .get.mSet(mSetObj)  
  dataSet <- mSetObj$dataSet

  require(ggplot2)
  require(Cairo)

  # compute P.O.D.s
  s.pods <- sensPOD(mSetObj, pod = c("feat.20", "feat.10th", "mode"), scale)

  # filter passed features
  bmd.hist <- dataSet$bmdcalc.obj$bmdcalc.res
  bmd.hist <- bmd.hist[bmd.hist$all.pass, ]

  # transform BMD values
  bmd.vals <- bmd.hist$bmd
  if (scale == "log10") {
    bmd.vals <- log10(bmd.vals);        xTitle <- "log10(Feature-level BMD)"
    s.pods   <- log10(s.pods)
  } else if (scale == "log2") {
    bmd.vals <- log2(bmd.vals);        xTitle <- "log2(Feature-level BMD)"
    s.pods   <- log2(s.pods)
  } else {
    xTitle <- "Feature-level BMD"
  }

  bmd.df <- data.frame(bmd = bmd.vals)

  # Base size default in ggplot2 is 11; 1.5× → 16.5
  baseSize <- 11 * 1.3

  p <- ggplot(bmd.df, aes(x = bmd)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.8) +
    scale_fill_gradient(
      low   = "lightblue",
      high  = "darkblue",
      guide = FALSE    # hide the density fill legend
    ) +
    geom_vline(aes(xintercept = s.pods["feat.20"],   colour = "gene20"),      size = 1) +
    geom_vline(aes(xintercept = s.pods["mode"],      colour = "mode"),        size = 1) +
    geom_vline(aes(xintercept = s.pods["feat.10th"], colour = "percentile10th"), size = 1) +
    scale_color_manual(
      name   = "mPOD",
      values = c(
        gene20         = "#A7414A",
        percentile10th = "#6A8A82",
        mode           = "#CC9B31"
      ),
      labels = c(
        paste0("20th feature: ", signif(s.pods["feat.20"], 2)),
        paste0("Max 1st peak: ", signif(s.pods["mode"], 2)),
        paste0("10th percentile: ", signif(s.pods["feat.10th"], 2))
      )
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(size = 2, linetype = 1)
      )
    ) +
    theme_bw(base_size = baseSize) +
    xlab(xTitle) +
    ylab("Density") +
    theme(
      # Scale all text elements by 1.5×
      axis.text         = element_text(size = rel(1)),
      axis.title        = element_text(size = rel(1)),
      legend.text       = element_text(size = rel(1)),
      legend.title      = element_text(size = rel(1)),
      axis.text.x       = element_text(face = "bold"),
      legend.position   = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just   = "right"
    )

  if(is.na(width) || width == 0){
    w <- 12;
  h <- 9;
  }else{
    w <-width;
    h<-width * 0.75
  }


  imgFile <- paste0(imgNm, "dpi", dpi, ".", format)


  Cairo(
    file   = imgFile,
    width  = w,    
    height = h,         
    unit   = "in",
    dpi    = 72,        
    type   = format,
    bg     = "white"
  )
  print(p)
  dev.off()

  mSetObj$imgSet$PlotDRHistogram <- imgFile
  return(.set.mSet(mSetObj))
}


