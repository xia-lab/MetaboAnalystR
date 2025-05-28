
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

  save.image("volc.RData");
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
  p    <- ..plotVolcano1(prep, plotLbl, plotTheme)
 }else if(plotStyle == 3){
  p    <- ..plotVolcano1(prep, plotLbl, plotTheme)
 }
  #p    <- ..applyStylePreset(p, plotStyle)


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

      imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p
      df <- data.frame(logFC   = vcn$fc.log,
                       negLogP = vcn$p.log,
                       pValue  = vcn$p.value[names(vcn$p.value) %in% names(vcn$p.log)],
                       FCraw   = vcn$fc.all,
                       Status  = factor("Not sig",
                                        levels = c("Down regulated","Not sig","Up regulated")))
      df$Status[vcn$inx.p & vcn$inx.up]   <- "Up regulated"
      df$Status[vcn$inx.p & vcn$inx.down] <- "Down regulated"

      # ranking for smart labelling ------------------------------------------------
      df$p.rank  <- rank(df$negLogP)/nrow(df)*100
      df$fc.rank <- rank(abs(df$logFC))/nrow(df)*100
      df$combinedRank <- pmax(df$p.rank, df$fc.rank)
      df <- df[order(-df$combinedRank), ]

      # choose labels
      df$label <- NA
      if(interactive){
          df$label <- rownames(df)
      }else if(labelNum != 0){
          if(labelNum < 0 || labelNum > nrow(df)){
              df$label[imp.inx] <- rownames(df)[imp.inx]
          }else{
              df$label[seq_len(labelNum)] <- rownames(df)[seq_len(labelNum)]
          }
      }

      list(
        df        = df,
        thresh.x  = c(vcn$min.xthresh, vcn$max.xthresh),
        thresh.y  = vcn$thresh.y,
        counts    = table(df$Status),
        compName  = vcn$comparison   # stored by upstream code; fallback handled below
      )
  }

  # ---------- 2. PLOTTING ------------------------------------------------
  ..plotVolcano1 <- function(prep, plotLbl, plotTheme){
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
  }
  