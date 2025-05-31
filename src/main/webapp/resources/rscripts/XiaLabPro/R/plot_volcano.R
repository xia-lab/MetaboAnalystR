
my.plot.volcano.custom <- function(mSetObj=NA,
                            imgName="NA",
                            plotLbl    = TRUE,
                            plotTheme  = 0,     # 0=BW, 1=grey, 2=minimal, 3=classic
                            format     = "png",
                            dpi        = 72,
                            width      = NA,
                            labelNum   = 5,
                            plotStyle = 0, # 0=default, 1=grey, 2=minimal, 3=classic
                            interactive= FALSE){

  # wrapper bookkeeping ---------------------------------------------------
  mSetObj <- .get.mSet(mSetObj)
  imgName <- paste0(imgName,"dpi",dpi,".",format)
  mSetObj$imgSet$volcano <- imgName
  w <- if(is.na(width)) 10 else if(width==0) 8 else width
  h <- w*6/10

  prep <- ..prepVolcanoData(mSetObj, labelNum, interactive)

  if(plotStyle == 1){
  p    <- ..plotVolcano1(prep, plotLbl, plotTheme)
  }else if(plotStyle == 2){
  p    <- ..plotVolcano2(prep, plotLbl, plotTheme)
 }else if(plotStyle == 3){
  p    <- ..plotVolcano3(prep, plotLbl, plotTheme)
 }else{

 }


  ## optional overall theme preset
  p <- switch(as.character(plotTheme),
              `0` = p + theme_bw(),
              `1` = p + theme_grey(),
              `2` = p + theme_minimal(),
              `3` = p + theme_classic(),
              p)


  # ---------- output -----------------------------------------------------
  if(interactive){
      library(plotly)
      ggp_build <- plotly::ggplotly(
                     p,
                     tooltip = c("label","pValue","FCraw"),
                     width   = 900, height = 600) %>%
                   layout(margin = mSetObj$imgSet$margin.config)
      return(ggp_build)
  } else {
      Cairo::Cairo(file = imgName, type=format,
                   width = w, height = h, unit="in", dpi=dpi, bg="white")
      print(p); 
      dev.off()
  }

  .set.mSet(mSetObj)
}

# ---------- 1. DATA PREPARATION ---------------------------------------
 ..prepVolcanoData <- function(mSetObj, labelNum, interactive){

  vcn <- mSetObj$analSet$volcano

  ## ------------------------------------------------------------------ ##
  ##  Build data frame
  ## ------------------------------------------------------------------ ##
  df <- data.frame(
    logFC   = vcn$fc.log,
    negLogP = vcn$p.log,
    pValue  = vcn$p.value[names(vcn$p.value) %in% names(vcn$p.log)],
    FCraw   = vcn$fc.all,
    Status  = factor("Not sig",
                     levels = c("Down regulated","Not sig","Up regulated"))
  )

  # flag significant up / down
  sig.up   <- vcn$inx.p & vcn$inx.up
  sig.down <- vcn$inx.p & vcn$inx.down

  df$Status[sig.up]   <- "Up regulated"
  df$Status[sig.down] <- "Down regulated"

  ## ------------------------------------------------------------------ ##
  ##  Ranking for smart labelling
  ## ------------------------------------------------------------------ ##
  df$p.rank       <- rank(df$negLogP)          / nrow(df) * 100
  df$fc.rank      <- rank(abs(df$logFC))       / nrow(df) * 100
  df$combinedRank <- pmax(df$p.rank, df$fc.rank)

  # order by combined rank (highest first)
  ord <- order(-df$combinedRank)
  df  <- df[ord, ]

  ## ------------------------------------------------------------------ ##
  ##  Choose labels  – only for significant (blue/red) points
  ## ------------------------------------------------------------------ ##
  df$label <- NA

  if (interactive) {                         # show all labels in Plotly view
    df$label <- rownames(df)
  } else if (labelNum != 0) {

    sig_idx <- which(df$Status != "Not sig")   # blue + red after re-ordering

    if (length(sig_idx) > 0) {

      if (labelNum < 0 || labelNum >= length(sig_idx)) {
        lab_idx <- sig_idx                                    # label them all
      } else {
        lab_idx <- sig_idx[seq_len(labelNum)]                 # top N by rank
      }
      df$label[lab_idx] <- rownames(df)[lab_idx]
    }
  }

  ## ------------------------------------------------------------------ ##
  ##  Return components for downstream plotting
  ## ------------------------------------------------------------------ ##
  list(
    df        = df,
    thresh.x  = c(vcn$min.xthresh, vcn$max.xthresh),
    thresh.y  = vcn$thresh.y,
    counts    = table(df$Status),
    compName  = vcn$comparison
  )
}


..plotVolcano1 <- function(prep, plotLbl = TRUE, plotTheme = 0) {
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggrepel)
    library(scales)
  })

  df <- prep$df

  ## colour palette (blue ↓, grey = non-sig, red ↑)
  cols <- c(
    "Down regulated" = "#1f77b4",   # blue
    "Not sig"        = "#bfbfbf",   # grey
    "Up regulated"   = "#d62728"    # red
  )

  ## legend labels with counts, e.g.  “Up regulated (123)”
  leg.labs <- sprintf(
    "%s (%s)",
    names(prep$counts),
    format(prep$counts, big.mark = "")
  )

  names(cols) <- leg.labs
  df$Status   <- factor(
    df$Status,
    levels = names(prep$counts),
    labels = leg.labs
  )

  ## axis label (multi-line if comparison name present)
  xLab <- bquote(log[2]*"(Fold Change)"~.(prep$compName %||% ""))

  p <- ggplot(df, aes(logFC, negLogP)) +
       geom_point(aes(colour = Status), size = 2.2, alpha = 0.9) +
       scale_colour_manual(values = cols, name = NULL) +
       labs(
         x = xLab,
         y = expression(-log[10](P[value]))
       ) +
       guides(colour = guide_legend(override.aes = list(size = 4))) +
       theme(
         legend.position      = "right",   # <- right-side legend
         legend.direction     = "vertical",
         legend.text          = element_text(size = 10, margin = margin(r = 4)),
         axis.text            = element_text(size = 11),
         axis.title           = element_text(size = 13, face = "bold")
       )

  ## optional data labels
  if (plotLbl) {
    p <- p +
      geom_text_repel(
        aes(label = label),
        size          = 3.5,
        colour        = "red",
        max.overlaps  = Inf,
        box.padding   = 0.4
      )
  }


  return(p)
}

  ..plotVolcano2 <- function(prep, plotLbl, plotTheme){
      library(ggplot2); library(ggrepel); library(scales)

      df <- prep$df
      cols <- c("Down regulated"="#44b5d8",
                "Not sig"        ="#bfbfbf",
                "Up regulated"   ="#f2a287")

      # build legend labels with counts -----------------------------------
      leg.labs <- sprintf("%s(%s)",
                          names(prep$counts),
                          format(prep$counts, big.mark=""))

      names(cols) <- leg.labs                         # keep colour order in legend
      df$Status   <- factor(df$Status,
                            levels = names(prep$counts),
                            labels = leg.labs)

      # axis label: multiline like example -------------------------------
      xLab <- bquote(log[2]*"(Fold Change)"~.(prep$compName %||% ""))

      p <- ggplot(df, aes(logFC, negLogP)) +
           geom_point(aes(colour=Status), size=2.2, alpha=.9) +
           scale_colour_manual(values = cols, name = NULL) +
           geom_vline(xintercept = prep$thresh.x, linetype="dashed") +
           geom_hline(yintercept = prep$thresh.y, linetype="dashed") +
           labs(x = xLab,
                y = expression(-log[10](P[value]))) +
           guides(colour = guide_legend(override.aes = list(size=4))) +
           theme(
             legend.position = "top",
             legend.justification = "left",
             legend.direction = "horizontal",
             legend.text = element_text(size=10, margin=margin(r=8)),
             axis.text  = element_text(size=11),
             axis.title = element_text(size=13, face="bold")
           )

      if(plotLbl){
          p <- p + ggrepel::geom_text_repel(aes(label = label),
                                            size = 3.5,
                                            colour = "red",
                                            max.overlaps = Inf,
                                            box.padding = .4)
      }
return(p)
  }

..plotVolcano3 <- function(prep, plotLbl = TRUE, plotTheme = 0) {

  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggrepel)
    library(scales)
  })

  df <- prep$df

  cols <- c(
    "Down regulated" = "#1f77b4",  # blue
    "Not sig"        = "#bfbfbf",  # grey
    "Up regulated"   = "#d62728"   # red
  )

  ## legend labels with counts, e.g. “Up regulated (123)”
  leg.labs <- sprintf(
    "%s (%s)",
    names(prep$counts),
    format(prep$counts, big.mark = "")
  )

  names(cols) <- leg.labs
  df$Status   <- factor(
    df$Status,
    levels = names(prep$counts),
    labels = leg.labs
  )

  ## axis label
  xLab <- bquote(log[2]*"(Fold Change)"~.(prep$compName %||% ""))

  p <- ggplot(df, aes(logFC, negLogP)) +
       geom_point(aes(colour = Status),
                  size  = 2.2,
                  alpha = 0.9) +
       scale_colour_manual(values = cols, name = NULL) +
       labs(
         x = xLab,
         y = expression(-log[10](P[value]))
       ) +
       guides(colour = guide_legend(override.aes = list(size = 4))) +
       theme(
         legend.position  = "right",
         legend.direction = "vertical",
         legend.text      = element_text(size = 10, margin = margin(r = 4)),
         axis.text        = element_text(size = 11),
         axis.title       = element_text(size = 13, face = "bold"),

         ## remove background grid completely
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()
       )

  ## optional gene labels
  if (plotLbl) {
    p <- p +
      geom_text_repel(
        aes(label = label),
        size         = 3.5,
        colour       = "black",
        max.overlaps = Inf,
        box.padding  = 0.4
      )
  }

  return(p)
}

  
