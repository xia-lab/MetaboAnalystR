#############################################
## Volcano plot lazy call
## Jeff Xia (jeff.xia@xialab.ca) 
#######################

# using ggplot
# if labelNum = -1 --> all sig labels
# if labelNum = 0 --> no labels
# else top labels #
my.plot.volcano <- function(mSetObj=NA, imgName="NA", plotLbl=T, plotTheme=0, format="png", dpi=default.dpi, width=NA, labelNum=5, interactive=F){
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

    de$FoldChange <- de$fc.log;
    fc_range <- range(de$fc.log); 
    
    # Create the ggplot
    p <- ggplot(data = de, aes(x = fc.log, y = p.log, label = label, Fold.Change = Fold.Change, P.Value= P.Value)) +
        geom_point(aes(size = size, color =FoldChange, fill=FoldChange), shape = 21, stroke = 0.3) +
        scale_color_gradient2(low = "black", mid = "black", high = "black", midpoint = 0,
                          limits = fc_range, space = "Lab", na.value = "black", guide="none") +
        scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, name = "Log2(FC)",
                          limits = fc_range, space = "Lab", na.value = "grey", guide = "colourbar") +
        scale_size_continuous(name = "P-value", range = c(1, 6), limits = c(0, 8), breaks = c(0, 1, 1.3, 2, 3), labels = c("1.0", "0.1", "0.05", "0.01", "0.001")) +
        geom_vline(xintercept = c(vcn$min.xthresh, vcn$max.xthresh), linetype = "dashed", color = "black") +
        geom_hline(yintercept = vcn$thresh.y, linetype = "dashed", color = "black") +
        labs(x = "log2(FC)", y = "-log10(p-value)") +
        theme_minimal() +
        theme(legend.position = "right");

  if(plotLbl){
    p <- p +  ggrepel::geom_text_repel();
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