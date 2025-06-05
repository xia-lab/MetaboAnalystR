
PlotVolcano <- function(mSetObj=NA, imgName, plotLbl, plotTheme, format="png", dpi=72, width=NA, labelNum=5, interactive=F){
#save.image("volc.RData");
  mSetObj <- .get.mSet(mSetObj);

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*6/10;
  
  mSetObj$imgSet$volcano <- imgName;
  
  vcn <- mSetObj$analSet$volcano;
  imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  
  de <- data.frame(cbind(vcn$fc.log, vcn$p.log));
  colnames(de) <- c("fc.log", "p.log");
  #  de$P.Value <- vcn$p.value;
  if(length(vcn$p.value) != length(de$P.Value)){
    de$P.Value <- vcn$p.value[names(vcn$p.value) %in% names(vcn$p.log)];
  } else {
    de$P.Value <- vcn$p.value;
  }
  de$Fold.Change <-vcn$fc.all
  de$Status <- "Non-SIG";
  de$Status[vcn$inx.p & vcn$inx.up] <- "UP";
  de$Status[vcn$inx.p & vcn$inx.down] <- "DOWN";
  de$Status <- as.factor(de$Status);

  # Calculate percentile ranks (0-100 scale for easier interpretation)
  de$p.rank <- rank(de$p.log) / length(de$p.log) * 100  # Higher p-values (lower significance) get lower rank
  de$fc.rank <- rank(abs(de$fc.log)) / length(de$fc.log) * 100  # Higher absolute fold changes get higher rank
  de$combinedRank <- pmax(de$p.rank, de$fc.rank)

  imp.inx <- imp.inx[order(-de$combinedRank)]
  de <- de[order(-de$combinedRank), ]
  de$label <- NA
  if(interactive){
    de$label <- rownames(de);
  }else if(labelNum == 0){
    de$label <- NA;
  }else if(labelNum < 0 || labelNum > length(rownames(de))){
    de$label[imp.inx] <- rownames(de)[imp.inx];
  }else{
    de$label[c(1:labelNum)] <- rownames(de)[c(1:labelNum)];
  }

  require(ggplot2);
  require(scales);
  
  # size on p values, further boost for sig
  de$size <- rescale(de$p.log, to = c(1.0, 4))
  de$size[imp.inx] <- de$size[imp.inx] + 0.5;

  # Create a new variable for gradient coloring based on ALL nodes, as this is continuous, threshold is for plot only

    # ---------------------------------------------------------------------
#  Prepare colour-mapping helper: gradient for sig, flat grey otherwise
# ---------------------------------------------------------------------
de$FCplot <- ifelse(de$Status == "Non-SIG", NA, de$fc.log)   # ← NA for grey pts
fc_range  <- range(de$fc.log, na.rm = TRUE)                  # mins / maxs of FC

# ---------------------------------------------------------------------
#  Build the ggplot
# ---------------------------------------------------------------------
p <- ggplot(de, aes(x = fc.log,
                    y = p.log,
                    label = label,
                    P.Value = P.Value)) +

     # 1) non-significant points (flat grey)
     geom_point(
       data  = subset(de, Status == "Non-SIG"),
       aes(size = size),
       colour = "grey60", fill = "grey60",
       shape = 21, stroke = 0.3, alpha = 0.9) +

     # 2) significant points (blue ↔ red gradient)
     geom_point(
       data  = subset(de, Status != "Non-SIG"),
       aes(size = size, colour = FCplot, fill = FCplot),
       shape = 21, stroke = 0.3, alpha = 0.9) +

     scale_color_gradient2(                    # border colour
       low       = "black",
       mid       = "black",
       high      = "black",
       midpoint  = 0,
       limits    = fc_range,
       na.value  = "grey60",                   # flat grey for NA (= non-sig)
       guide     = "none") +

     scale_fill_gradient2(                     # interior fill
       low       = "blue",
       mid       = "grey",
       high      = "red",
       midpoint  = 0,
       limits    = fc_range,
       na.value  = "grey60",                   # flat grey for NA (= non-sig)
       name      = "Log2(FC)",
       guide     = "colourbar") +

     scale_size_continuous(
       name   = "P-value",
       range  = c(1, 6),
       limits = c(0, 8),
       breaks = c(0, 1, 1.3, 2, 3),
       labels = c("1.0", "0.1", "0.05", "0.01", "0.001")) +

     geom_vline(xintercept = c(vcn$min.xthresh, vcn$max.xthresh),
                linetype = "dashed", colour = "black") +
     geom_hline(yintercept = vcn$thresh.y,
                linetype = "dashed", colour = "black") +

     labs(x = "log2(FC)", y = "-log10(p-value)") +
     theme_minimal() +
     theme(legend.position = "right")

    # optional labels
    if (plotLbl) {
      p <- p + ggrepel::geom_text_repel()
    }

  mSetObj$analSet$volcano.plot.config <- list(plotLbl=plotLbl, plotTheme=plotTheme);
  
  if(!interactive){
    if(plotTheme == 0){
      p <- p + theme_bw();
    }else if(plotTheme == 1){
      p <- p + theme_grey();
    }else if(plotTheme == 2){
      p <- p + theme_minimal();
    }else{
      p <- p + theme_classic();
    }
  }
  
  if(interactive){
    library(plotly);
    ggp_build <- layout(ggplotly(p, tooltip = c("label", "P.Value", "Fold.Change")), autosize = FALSE, width = 900, height = 600, margin = mSetObj$imgSet$margin.config)
    fig <- plotly_build(ggp_build)
    vec <- rep("rgba(22,22,22,1)", nrow(de))
    attr(vec, "apiSrc") <- TRUE
    fig$x[[1]][[1]]$marker$line$color <- vec;
    fig$x[[1]][[1]]$marker$line$size <- 1;
    return(ggp_build);
  } else {
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
    print(p)
    dev.off();
  }
  
  return(.set.mSet(mSetObj));
}
