#############################################
## Volcano plot lazy call
## Jeff Xia (jeff.xia@xialab.ca) 
#######################

# using ggplot
# if labelNum = -1 --> all sig labels
# if labelNum = 0 --> no labels
# else top labels #
my.plot.volcano <- function(mSetObj=NA, imgName, plotLbl, plotTheme, format="png", dpi=72, width=NA,labelNum=5,  interactive=F){

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
  de$P.Value <- vcn$p.value;
  de$Fold.Change <-vcn$fc.all
  de$Status <- "Non-SIG";
  de$Status[vcn$inx.p & vcn$inx.up] <- "UP";
  de$Status[vcn$inx.p & vcn$inx.down] <- "DOWN";
  de$Status <- as.factor(de$Status);
  
  mycols <- levels(de$Status);
  mycols[mycols=="UP"] <- "firebrick";
  mycols[mycols=="DOWN"] <- "cornflowerblue";
  mycols[mycols=="Non-SIG"] <- "grey";
  de$compositeScore <- de$p.log * de$fc.log
  imp.inx <- imp.inx[order( -de$compositeScore)]
  de <- de[order( -de$compositeScore), ]
  de$label <- NA
  if(interactive){
    de$label <- rownames(de);
  }else if(labelNum < 0 || labelNum > length(rownames(de))){
    de$label[imp.inx] <- rownames(de)[imp.inx];
  }else{
    de$label[c(1:labelNum)] <- rownames(de)[c(1:labelNum)];
    
  }
  require(ggplot2);
  require(scales);
  
  de$size <- rescale(de$p.log, to = c(1.5, 4))
  
  # Create a new variable for gradient coloring based on significance
  de$FoldChange <- ifelse(de$Status != "Non-SIG", de$fc.log, NA)
  
  # Get the range of log fold changes for significant points
  sig_fc_range <- range(de$fc.log[de$Status != "Non-SIG"], na.rm = TRUE)
  
  # Create the ggplot
  p <- ggplot(data = de, aes(x = fc.log, y = p.log, label = label, Fold.Change = Fold.Change, P.Value= P.Value)) +
    geom_point(aes( size = size, color =FoldChange, fill=FoldChange) , shape = 21, stroke = 0.5) +
    scale_color_gradient2(low = "black", mid = "black", high = "black", midpoint = 0,
                          limits = sig_fc_range, space = "Lab", na.value = "black", guide="none") +
    scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0,
                          limits = sig_fc_range, space = "Lab", na.value = "grey", guide = "colourbar") +
    scale_size_continuous(range = c(1, 4),guide="none") +
    geom_vline(xintercept = c(vcn$min.xthresh, vcn$max.xthresh), linetype = "dashed", color = "black") +
    geom_hline(yintercept = vcn$thresh.y, linetype = "dashed", color = "black") +
    labs(x = "log2(FC)", y = "-log10(p-value)") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Print the plot
  
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