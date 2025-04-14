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
  save.image("PlotMetaboliteDRCurve.RData");
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
       geom_rect(aes(xmin = bmdl, xmax = bmdu, ymin = -Inf, ymax = Inf), fill = "red3", alpha = 0.05) +
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
  Cairo::Cairo(file = imgNm, width = 8, height = 6, unit = "in", dpi = dpi, type = format, bg = "white")
  print(p)
  dev.off()

  mSetObj$imgSet$PlotDRModelBars <- imgNm
  return(.set.mSet(mSetObj))
}


PlotDRHistogram <- function(mSetObj=NA,imgNm, dpi, format, units, scale){
  mSetObj <- .get.mSet(mSetObj);  
  dataSet <- mSetObj$dataSet;

  require(ggplot2)
  s.pods <- sensPOD(mSetObj, pod = c("feat.20", "feat.10th", "mode"), scale)
  
  bmd.hist <- dataSet$bmdcalc.obj$bmdcalc.res[dataSet$bmdcalc.obj$bmdcalc.res$all.pass,]

  if(scale == "log10"){
    dens <- density(log10(bmd.hist$bmd));
    xTitle <- "log10(Feature-level BMD)";
  } else if(scale == "log2"){
    dens <- density(log2(bmd.hist$bmd));
    xTitle <- "log2(Feature-level BMD)";
  } else {
    dens <- density(bmd.hist$bmd);
    xTitle <- "Feature-level BMD";
  }
  
  dens <- data.frame(x = dens$x, y = dens$y)
  
  require(ggplot2)
  p <- ggplot(aes(x = x, y = y), data = dens) + 
    geom_area() +
    geom_vline(aes(xintercept = s.pods[2], colour = "percentile10th"), size = 1) +
    geom_vline(aes(xintercept = s.pods[3], colour = "mode"), size = 1) +
    geom_vline(aes(xintercept = s.pods[1], colour = "gene20"), size = 1) +
    scale_color_manual(name = "mPOD", 
                       values = c(gene20 = "#A7414A", percentile10th = "#6A8A82", mode = "#CC9B31"),
                       labels = c(paste0("20th feature: ", signif(s.pods[1],2)), 
                                  paste0("Max 1st peak: ", signif(s.pods[3],2)),
                                  paste0("10th percentile: ", signif(s.pods[2],2)))) + 
    theme_bw()
  p <- p + xlab(xTitle) + ylab("Density function") + 
    theme(axis.text.x = element_text(face="bold"), legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right")

  imgNm = paste(imgNm, "dpi", dpi, ".", format, sep="");
  Cairo::Cairo(file=imgNm, width=8, height=6, unit="in",dpi=300, type=format, bg="white");
  print(p)
  dev.off();

  mSetObj$imgSet$PlotDRHistogram <- imgNm;
  return(.set.mSet(mSetObj));
}