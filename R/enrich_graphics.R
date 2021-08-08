#'View individual compounds related to a given metabolite set
#'@description View individual compounds related to a given metabolite set
#'Functions for various plots for enrichment analysis
#'@usage PlotQEA.MetSet(mSetObj=NA, setNM, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param setNM Input the name of the metabolite set 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotQEA.MetSet<-function(mSetObj=NA, setNM, format="png", dpi=72, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  # clean the name, some contains space and special characters, will
  # cause trouble as image name
  imgNM <- gsub("\\|", "_", setNM);
  imgNM <- gsub("/", "_", imgNM);
  imgNM <- gsub(" *", "", imgNM);
  if(nchar(imgNM) > 18){
    imgNM <-substr(imgNM, 0, 18);
  }
  imgName <- paste(imgNM, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  
  h <- w;
  
  mSetObj$imgSet$qea.mset<-imgName;
  
  # hits in the current mset
  mset <- current.msetlib$member[[setNM]];
  hit.cmpds <- mSetObj$analSet$qea.hits[[setNM]];
  hit.pvals <- mSetObj$analSet$qea.pvals[hit.cmpds];

  #reorder based on p vals
  ord.inx <- order(hit.pvals);
  hit.cmpds <- hit.cmpds[ord.inx]; 
  hit.pvals <- signif(hit.pvals[ord.inx],3); 

  if(.on.public.web){
    load_lattice()
    load_ggplot()
  }

  # get p values
  mSetObj$analSet$qea.pvals[];

  # need to reshape data to be one vector
  num <- length(hit.cmpds);
  len <- length(mSetObj$dataSet$cls);
  conc.vec <- lbl.vec <- cls.vec <- NULL;
  
  for(i in 1:num){
        cmpd <- hit.cmpds[i];
        conc.vec <- c(conc.vec, mSetObj$analSet$msea.data[,cmpd]);
        cls.vec <- c(cls.vec, as.character(mSetObj$dataSet$cls));
        cmpd.p <- paste(cmpd, " (p=", hit.pvals[i], ")", sep="");
        lbl.vec <- c(lbl.vec, rep(cmpd.p, len));      
  }
  
  cls.vec <- as.factor(cls.vec);
  lbl.vec <- factor(lbl.vec, levels = unique(lbl.vec));

  num <- length(hit.cmpds);
  boxdata <- data.frame(Feature = lbl.vec, Abundance = conc.vec, Class = cls.vec)
  
  # calculate width based on the dataset number
  if(num == 1){
    
    p <- ggplot(data = boxdata, aes(x=Class, y=Abundance)) + geom_boxplot(aes(fill=Class), outlier.shape = NA, outlier.colour=NA)
    p <- p + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Group"))
    p <- p + xlab("") + ylab("Relative Abundance") + theme_bw()
    
    ggsave(p, filename = imgName, dpi=dpi, width=7, height=6, limitsize = FALSE)

  }else{

    if(num<10){
      w = 10
      h <- 10 * num/10
      cols = 3
    }else if(num<5){
      w = 7
      h = 7
      cols = 2
    }else{
      w = 10
      h <- num * 0.55
      cols = 4
    }

    bp <- ggplot(boxdata, aes(x=Class, y=Abundance, group=Class)) + 
      geom_boxplot(aes(fill=Class), outlier.shape = NA, outlier.colour=NA) + theme_bw()
    
    bp <- bp + facet_wrap(. ~ Feature) + theme(strip.text.x = element_text(size=7), strip.background = element_rect(size=1)) + 
      xlab("") + ylab("Relative Abundance")
    
    ggsave(bp, filename = imgName, dpi=dpi, width=w, height=h, limitsize = FALSE)
  }

  mSetObj$imgSet$current.img <- imgName;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(imgName);
  }
  return(.set.mSet(mSetObj));
}

#'Plot the compound concentration data compared to the reference concentration range
#'@description Plot the compound concentration data compared to the reference concentration range
#'@usage PlotConcRange(mSetObj, nm, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nm of the input compound
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotConcRange<-function(mSetObj=NA, nm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  inx <- which(names(mSetObj$analSet$ssp.lows)==nm);
  lows <- mSetObj$analSet$ssp.lows[[inx]];
  
  if(length(lows)==1 && is.na(lows)){
    return();
  }
  
  #conc<-dataSet$norm[inx];
  means <- mSetObj$analSet$ssp.means[[inx]];
  highs <- mSetObj$analSet$ssp.highs[[inx]];
  
  cmpdNm <- mSetObj$analSet$ssp.mat[inx,1];
  conc <- as.numeric(mSetObj$analSet$ssp.mat[inx,2]);
  hmdbID <- mSetObj$analSet$ssp.mat[inx,3];
  imgName <<- paste(hmdbID, "dpi", dpi, ".png", sep="");
  
  if(is.na(width)){
    w <- h <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- h <- width;
  }
  
  mSetObj$imgSet$conc.range <- imgName;
  mSetObj$imgSet$current.img <- imgName;
  
  rng <- range(lows, highs, conc);
  ext <- (rng[2]-rng[1])/8;
  max.rg <- rng[2]+ext;
  min.rg <- ifelse(rng[1]-ext>=0, rng[1]-ext, 0);
  unit <- "(umol)"
  if(mSetObj$dataSet$biofluid =="urine"){
    unit <- "(umol/mmol_creatine)"
  }
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  concplot(means, lows, highs, xlim=c(min.rg, max.rg), labels = paste("Study ", 1:length(lows)),
           main=cmpdNm, xlab=paste("Concentration Range", unit), ylab="Study Reference")
  abline(v=c(range(lows, highs), conc), col=c("lightgrey", "lightgrey", "orange"), lty=c(5, 5, 5), lwd=c(1,1,2));
  
  # label the measured the concentration to the side
  text(conc, 0, conc, pos=4, col="orange");
  # extend the left end of axis to look natural without labeling
  axis(1, at=c(min.rg-ext, min.rg), label=F, lwd.tick=0);
  
  dev.off();
  
  mSetObj$imgSet$current.img <- imgName;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(imgName);
  }
  
  return(.set.mSet(mSetObj));
}

#'Plot over-representation analysis (ORA)
#'@description Plot over-representation analysis (ORA)
#'@usage PlotORA(mSetObj=NA, imgName, imgOpt, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param imgOpt "net" 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotORA<-function(mSetObj=NA, imgName, imgOpt, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  #calculate the enrichment fold change
  folds <- mSetObj$analSet$ora.mat[,3]/mSetObj$analSet$ora.mat[,2];
  names(folds) <- GetShortNames(rownames(mSetObj$analSet$ora.mat));
  pvals <- mSetObj$analSet$ora.mat[,4];
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <-width;
  }
  h <- w;
  
  #record img
  mSetObj$imgSet$ora <- imgName
  mSetObj$imgSet$current.img <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  PlotMSEA.Overview(folds, pvals);
  dev.off();
  
  if(.on.public.web){
    mSetObj$analSet$enrich.net <- PlotEnrichNet.Overview(folds, pvals);
  }
  
  return(.set.mSet(mSetObj));
}

#'Plot QEA overview
#'@description Plot QEA overview
#'@usage PlotQEA.Overview(mSetObj=NA, imgName, imgOpt, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param imgOpt "net" 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotQEA.Overview <-function(mSetObj=NA, imgName, imgOpt, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  #calculate the enrichment fold change
  folds <- mSetObj$analSet$qea.mat[,3]/mSetObj$analSet$qea.mat[,4];
  names(folds) <- GetShortNames(rownames(mSetObj$analSet$qea.mat));
  pvals <- mSetObj$analSet$qea.mat[, "Raw p"];
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotMSEA.Overview(folds, pvals);
  dev.off();
  
  if(.on.public.web){
    mSetObj$analSet$enrich.net <- PlotEnrichNet.Overview(folds, pvals);
  }
  
  mSetObj$imgSet$qea <-imgName;
  mSetObj$imgSet$current.img <- imgName;
  return(.set.mSet(mSetObj));
}

#'Plot MSEA overview
#'@description Barplot height is enrichment fold change
#'color is based on p values, used in higher functions
#'@usage PlotMSEA.Overview(folds, pvals)
#'@param folds Input the fold-change values
#'@param pvals Input the p-values
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotMSEA.Overview <- function(folds, pvals){
  
  # due to space limitation, plot top 50 if more than 50 were given
  title <- "Metabolite Sets Enrichment Overview";
  ht.col <- GetMyHeatCols(length(folds));
  if(length(folds) > 25){
    folds <- folds[1:25];
    pvals <- pvals[1:25];
    ht.col <- ht.col[1:25];
    title <- "Enrichment Overview (top 25)";
  }
  
  op <- par(mar=c(5,20,4,6), oma=c(0,0,0,4));
  
  barplot(rev(folds), horiz=T, col=rev(ht.col),
          xlab="Enrichment Ratio", las=1, cex.name=0.75, space=c(0.5, 0.5),
          main= title);
  
  minP <- min(pvals);
  maxP <- max(pvals);
  medP <- (minP+maxP)/2;
  
  axs.args <- list(at=c(minP, medP, maxP), labels=format(c(maxP, medP, minP), scientific=T, digit=1), tick = F);
  image.plot(legend.only=TRUE, zlim=c(minP, maxP), col=rev(ht.col),
             axis.args=axs.args, legend.shrink = 0.4, legend.lab="P value");
  par(op);
}

#'Plot MSEA Dot Plot
#'@description Dot plot of enrichment analysis results.
#'@usage PlotEnrichDotPlot(mSet)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param enrichType Input whether the enrichment analysis was over-respresentation
#'analysis (ora) or quantitative enrichment analysis (qea).
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotEnrichDotPlot <- function(mSetObj=NA, enrichType = "ora", imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }
  
  if(enrichType == "ora"){
    results <- mSetObj$analSet$ora.mat
    my.cols <- GetMyHeatCols(nrow(results));
    if(nrow(results) > 25){
      results <- results[1:25,]
      my.cols <- my.cols[1:25];
    }
    
    df <- data.frame(Name = factor(row.names(results), levels = rev(row.names(results))),
                     rawp = results[,4],
                     logp = -log10(results[,4]),
                     folds = results[,3]/results[,2])
    
  }else if(enrichType == "qea"){
    results <- mSetObj$analSet$qea.mat
    my.cols <- GetMyHeatCols(nrow(results));   
    if(nrow(results) > 25){
      results <- results[1:25,]
      my.cols <- my.cols[1:25];
    }
    df <- data.frame(Name = factor(row.names(results), levels = rev(row.names(results))),
                     rawp = results[,5],
                     logp = -log10(results[,5]),
                     folds = results[,3]/results[,4]
                    )
    
  }

  maxp <- max(df$rawp);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 12;
    h <- 9;
  }else if(width == 0){
    h <- w <- 7;
  }else{
    h <- w <- width;
  }
  
  p <- ggplot(df, 
              aes(x = logp, y = Name)) + 
    geom_point(aes(size = folds, color = rawp)) + scale_size_continuous(range = c(2, 8)) +
    theme_bw(base_size = 14.5) +
    scale_colour_gradient(limits=c(0, maxp), low=my.cols[1], high = my.cols[length(my.cols)]) +
    ylab(NULL) + xlab("-log10 (p-value)") + 
    ggtitle("Overview of Enriched Metabolite Sets (Top 25)") +
    theme(legend.text=element_text(size=14),
          legend.title=element_text(size=15))
  
  p$labels$colour <- "P-value"
  p$labels$size <- "Enrichment Ratio"
  
  ggsave(p, filename = imgName, dpi=dpi, width=w, height=h)
  
  if(enrichType == "ora"){
    mSetObj$imgSet$ora <- imgName
  }else if(enrichType == "qea"){
    mSetObj$imgSet$qea <-imgName;
  }
  
  mSetObj$imgSet$current.img <- imgName;
  return(.set.mSet(mSetObj));
}

# Utility function
concplot <- function(mn, lower, upper, labels=NULL,
                     xlab = "Odds ratio", ylab = "Study Reference",
                     xlim = NULL, line.col = "blue", text.col="forestgreen",
                     xaxt="s", ... ) {
  
  n <- length( mn );
  nxlim <- xlim;
  nxlim[1] <- nxlim[1] - 0.25 * (nxlim[2] - nxlim[1] );
  par(xaxt = "n",yaxt = "n")
  plot(nxlim,c(1,-n-2),
       type = "n", bty = "n", xaxt = "n", yaxt = "n",
       xlab=xlab, ylab=ylab,... );
  
  text(rep(nxlim[1], n ), -( 1:n ), labels,..., col=rep(text.col,length.out=n),adj=0);
  par( xaxt = "s")
  ats<-pretty(xlim, 6);
  axis(1, at=ats)
  
  for ( i in 1:n ){
    if ( is.na( lower[i]+upper[i] ) )
      next
    arrows(lower[i], -i, upper[i], -i, lwd = 1, code=3, col=line.col, angle=90, length=0.04);
    points(mn[i], -i, pch=15, col='magenta');
  }
}

# Plot a strip of color key beside a figure
# Adapted from the image.plot in fields package to correct label
# so that the small p value is bigger, located on top of the color key
# Jeff Xia, jeff.xia@mcgill.ca
# McGill University, Canada
# License: GNU GPL (>= 2)

image.plot <- function(..., add = FALSE, nlevel = 64,
                       horizontal = FALSE, legend.shrink = 0.9, legend.width = 1.2,
                       legend.mar = ifelse(horizontal, 3.1, 5.1), legend.lab = NULL,
                       graphics.reset = FALSE, bigplot = NULL, smallplot = NULL,
                       legend.only = FALSE, col = tim.colors(nlevel), lab.breaks = NULL,
                       axis.args = NULL, legend.args = NULL, midpoint = FALSE) {
  
  old.par <- par(no.readonly = TRUE)
  #  figure out zlim from passed arguments
  info <- image.plot.info(...)
  if (add) {
    big.plot <- old.par$plt
  }
  if (legend.only) {
    graphics.reset <- TRUE
  }
  if (is.null(legend.mar)) {
    legend.mar <- ifelse(horizontal, 3.1, 5.1)
  }
  #
  # figure out how to divide up the plotting real estate.
  #
  temp <- image.plot.plt(add = add, legend.shrink = legend.shrink,
                         legend.width = legend.width, legend.mar = legend.mar,
                         horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
  #
  # bigplot are plotting region coordinates for image
  # smallplot are plotting coordinates for legend
  smallplot <- temp$smallplot
  bigplot <- temp$bigplot
  #
  # draw the image in bigplot, just call the R base function
  # or poly.image for polygonal cells note logical switch
  # for poly.grid parsed out of call from image.plot.info
  if (!legend.only) {
    if (!add) {
      par(plt = bigplot)
    }
    if (!info$poly.grid) {
      image(..., add = add, col = col)
    }
    else {
      poly.image(..., add = add, col = col, midpoint = midpoint)
    }
    big.par <- par(no.readonly = TRUE)
  }
  ##
  ## check dimensions of smallplot
  if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
    par(old.par)
    stop("plot region too small to add legend\n")
  }
  # Following code draws the legend using the image function
  # and a one column image.
  # calculate locations for colors on legend strip
  ix <- 1
  minz <- info$zlim[1]
  maxz <- info$zlim[2]
  binwidth <- (maxz - minz)/nlevel
  midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
  iy <- midpoints
  iz <- matrix(iy, nrow = 1, ncol = length(iy))
  # extract the breaks from the ... arguments
  # note the breaks delineate intervals of common color
  breaks <- list(...)$breaks
  # draw either horizontal or vertical legends.
  # using either suggested breaks or not -- a total of four cases.
  #
  # next par call sets up a new plotting region just for the legend strip
  # at the smallplot coordinates
  par(new = TRUE, pty = "m", plt = smallplot, err = -1)
  # create the argument list to draw the axis
  #  this avoids 4 separate calls to axis and allows passing extra
  # arguments.
  # then add axis with specified lab.breaks at specified breaks
  if (!is.null(breaks) & !is.null(lab.breaks)) {
    # axis with labels at break points
    axis.args <- c(list(side = ifelse(horizontal, 1, 4),
                        mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2),
                        at = breaks, labels = lab.breaks), axis.args)
  }
  else {
    # If lab.breaks is not specified, with or without breaks, pretty
    # tick mark locations and labels are computed internally,
    # or as specified in axis.args at the function call
    axis.args <- c(list(side = ifelse(horizontal, 1, 4),
                        mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)),
                   axis.args)
  }
  #
  # draw color scales the four cases are horizontal/vertical breaks/no breaks
  # add a label if this is passed.
  if (!horizontal) {
    if (is.null(breaks)) {
      image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "",
            ylab = "", col = col)
    }
    else {
      image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "",
            ylab = "", col = col, breaks = breaks)
    }
  }
  else {
    if (is.null(breaks)) {
      image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "",
            ylab = "", col = col)
    }
    else {
      image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "",
            ylab = "", col = col, breaks = breaks)
    }
  }
  
  #
  # now add the axis to the legend strip.
  # notice how all the information is in the list axis.args
  #
  do.call("axis", axis.args)
  
  # add a box around legend strip
  box()
  
  #
  # add a label to the axis if information has been  supplied
  # using the mtext function. The arguments to mtext are
  # passed as a list like the drill for axis (see above)
  #
  if (!is.null(legend.lab)) {
    legend.args <- list(text = legend.lab, side = ifelse(horizontal,
                                                         1, 3), line = 1)
  }
  #
  # add the label using mtext function
  if (!is.null(legend.args)) {
    do.call(mtext, legend.args)
  }
  #
  #
  # clean up graphics device settings
  # reset to larger plot region with right user coordinates.
  mfg.save <- par()$mfg
  if (graphics.reset | add) {
    par(old.par)
    par(mfg = mfg.save, new = FALSE)
    invisible()
  }
  else {
    par(big.par)
    par(plt = big.par$plt, xpd = FALSE)
    par(mfg = mfg.save, new = FALSE)
    invisible()
  }
}


"image.plot.info" <- function(...) {
  temp <- list(...)
  #
  xlim <- NA
  ylim <- NA
  zlim <- NA
  poly.grid <- FALSE
  #
  # go through various cases of what these can be
  #
  ##### x,y,z list is first argument
  if (is.list(temp[[1]])) {
    xlim <- range(temp[[1]]$x, na.rm = TRUE)
    ylim <- range(temp[[1]]$y, na.rm = TRUE)
    zlim <- range(temp[[1]]$z, na.rm = TRUE)
    if (is.matrix(temp[[1]]$x) & is.matrix(temp[[1]]$y) &
        is.matrix(temp[[1]]$z)) {
      poly.grid <- TRUE
    }
  }
  ##### check for polygrid first three arguments should be matrices
  #####
  if (length(temp) >= 3) {
    if (is.matrix(temp[[1]]) & is.matrix(temp[[2]]) & is.matrix(temp[[3]])) {
      poly.grid <- TRUE
    }
  }
  #####  z is passed without an  x and y  (and not a poly.grid!)
  #####
  if (is.matrix(temp[[1]]) & !poly.grid) {
    xlim <- c(0, 1)
    ylim <- c(0, 1)
    zlim <- range(temp[[1]], na.rm = TRUE)
  }
  ##### if x,y,z have all been passed find their ranges.
  ##### holds if poly.grid or not
  #####
  if (length(temp) >= 3) {
    if (is.matrix(temp[[3]])) {
      xlim <- range(temp[[1]], na.rm = TRUE)
      ylim <- range(temp[[2]], na.rm = TRUE)
      zlim <- range(temp[[3]], na.rm = TRUE)
    }
  }
  #### parse x,y,z if they are  named arguments
  # determine if  this is polygon grid (x and y are matrices)
  if (is.matrix(temp$x) & is.matrix(temp$y) & is.matrix(temp$z)) {
    poly.grid <- TRUE
  }
  xthere <- match("x", names(temp))
  ythere <- match("y", names(temp))
  zthere <- match("z", names(temp))
  if (!is.na(zthere))
    zlim <- range(temp$z, na.rm = TRUE)
  if (!is.na(xthere))
    xlim <- range(temp$x, na.rm = TRUE)
  if (!is.na(ythere))
    ylim <- range(temp$y, na.rm = TRUE)
  # overwrite zlims with passed values
  if (!is.null(temp$zlim))
    zlim <- temp$zlim
  if (!is.null(temp$xlim))
    xlim <- temp$xlim
  if (!is.null(temp$ylim))
    ylim <- temp$ylim
  list(xlim = xlim, ylim = ylim, zlim = zlim, poly.grid = poly.grid)
}

# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html

image.plot.plt <- function(x, add = FALSE, legend.shrink = 0.9,
                           legend.width = 1, horizontal = FALSE, legend.mar = NULL,
                           bigplot = NULL, smallplot = NULL, ...) {
  old.par <- par(no.readonly = TRUE)
  if (is.null(smallplot))
    stick <- TRUE
  else stick <- FALSE
  if (is.null(legend.mar)) {
    legend.mar <- ifelse(horizontal, 3.1, 5.1)
  }
  # compute how big a text character is
  char.size <- ifelse(horizontal, par()$cin[2]/par()$din[2],
                      par()$cin[1]/par()$din[1])
  # This is how much space to work with based on setting the margins in the
  # high level par command to leave between strip and big plot
  offset <- char.size * ifelse(horizontal, par()$mar[1], par()$mar[4])
  # this is the width of the legned strip itself.
  legend.width <- char.size * legend.width
  # this is room for legend axis labels
  legend.mar <- legend.mar * char.size
  # smallplot is the plotting region for the legend.
  if (is.null(smallplot)) {
    smallplot <- old.par$plt
    if (horizontal) {
      smallplot[3] <- legend.mar
      smallplot[4] <- legend.width + smallplot[3]
      pr <- (smallplot[2] - smallplot[1]) * ((1 - legend.shrink)/2)
      smallplot[1] <- smallplot[1] + pr
      smallplot[2] <- smallplot[2] - pr
    }
    else {
      smallplot[2] <- 1 - legend.mar
      smallplot[1] <- smallplot[2] - legend.width
      pr <- (smallplot[4] - smallplot[3]) * ((1 - legend.shrink)/2)
      smallplot[4] <- smallplot[4] - pr
      smallplot[3] <- smallplot[3] + pr
    }
  }
  if (is.null(bigplot)) {
    bigplot <- old.par$plt
    if (!horizontal) {
      bigplot[2] <- min(bigplot[2], smallplot[1] - offset)
    }
    else {
      bottom.space <- old.par$mar[1] * char.size
      bigplot[3] <- smallplot[4] + offset
    }
  }
  if (stick & (!horizontal)) {
    dp <- smallplot[2] - smallplot[1]
    smallplot[1] <- min(bigplot[2] + offset, smallplot[1])
    smallplot[2] <- smallplot[1] + dp
  }
  return(list(smallplot = smallplot, bigplot = bigplot))
}

#' PlotEnrichNet.Overview
#' @description Used in higher functions, the color is based on p values
#' @param folds Input fold-change for bar plot
#' @param pvals Input p-values for bar plot
#' @param layoutOpt Input the layout option, default is set to layout.fruchterman.reingold
#' @author Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import igraph

PlotEnrichNet.Overview <- function(folds, pvals, layoutOpt=layout.fruchterman.reingold){
  
  # due to space limitation, plot top 50 if more than 50 were given
  title <- "Enrichment Network Overview";
  if(length(folds) > 50){
    folds <- folds[1:50];
    pvals <- pvals[1:50];
    title <- "Enrichment Overview (top 50)";
  }
  
  if(.on.public.web){
    load_igraph()
    #load_reshape()
  }

  pvalue <- pvals;
  id <- names(pvalue);
  geneSets <- current.msetlib$member;
  n <- length(pvalue);
  w <- matrix(NA, nrow=n, ncol=n);
  colnames(w) <- rownames(w) <- id;
  
  for (i in 1:n) {
    for (j in i:n) {
      w[i,j] = overlap_ratio(geneSets[id[i]], geneSets[id[j]])
    }
  }
  
  wd <- melt(w);
  wd <- wd[wd[,1] != wd[,2],];
  wd <- wd[!is.na(wd[,3]),];
  g <- graph.data.frame(wd[,-3], directed=F);
  E(g)$width <- sqrt(wd[,3]*20);
  g <- delete.edges(g, E(g)[wd[,3] < 0.25]);
  #idx <- unlist(sapply(V(g)$name, function(x) which(x == id)));
  #cols <- color_scale("red", "#E5C494");
  #V(g)$color <- cols[sapply(pvalue, getIdx, min(pvalue), max(pvalue))];
  V(g)$color <- heat.colors(length(pvalue));

  cnt <- folds;
  names(cnt) <- id;
  #cnt2 <- (cnt[V(g)$name])^2; #make sure this is positve 
  #V(g)$size <- cnt2/sum(cnt2) * 100 #log(cnt2, base=10) * 10;
  #V(g)$size <- log(cnt2, base=10) * 10 + 1;
  V(g)$size <- cnt + 3;

  pos.xy <- layout.fruchterman.reingold(g,niter=500);
  
  # now create the json object
  nodes <- vector(mode="list");
  node.nms <- V(g)$name;
  node.sizes <- V(g)$size;
  node.cols <- V(g)$color;
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id = node.nms[i],
      label=node.nms[i], 
      size=node.sizes[i], 
      color=node.cols[i],
      x = pos.xy[i,1],
      y = pos.xy[i,2]
    );
  }
  
  edge.mat <- get.edgelist(g);
  edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2]);
  # covert to json
  netData <- list(nodes=nodes, edges=edge.mat);
  sink("msea_network.json");
  cat(RJSONIO::toJSON(netData));
  sink();
  
  return(g);  
}

PrepareSifDownloads <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  saveNetworkInSIF(mSetObj$analSet$enrich.net, "metaboanalyst_enrich");
}

##' @importFrom grDevices colorRampPalette
color_scale <- function(c1="grey", c2="red") {
  pal <- colorRampPalette(c(c1, c2))
  colors <- pal(100)
  return(colors)
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################


GetCurrentImg<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (mSetObj$imgSet$current.img);
}

getIdx <- function(v, MIN, MAX) {
  if ( MIN == MAX ) {
    return(100)
  }
  intervals <- seq(MIN, MAX, length.out=100)
  max(which(intervals <= v))
}

######### Utility Functions #############
GetShortNames<-function(nm.vec, max.len= 45){
  new.nms <- vector(mode="character", length=length(nm.vec));
  for(i in 1:length(nm.vec)){
    nm <- nm.vec[i];
    if(nchar(nm) <= max.len){
      new.nms[i] <- nm;
    }else{
      wrds <- strsplit(nm, "[[:space:]]+")[[1]];
      new.nm <- "";
      if(length(wrds)>1){
        for(m in 1:length(wrds)){
          wrd <- wrds[m];
          if(nchar(new.nm)+4+nchar(wrd) <= max.len){
            new.nm <- paste(new.nm, wrd);
          }else{
            new.nms[i] <- paste (new.nm, "...", sep="");
            break;
          }
        }
      }else{
        new.nms[i] <- paste (substr(nm, 0, 21), "...", sep="");
      }
    }
  }
  return(new.nms);
}

# return heat colors of given length
GetMyHeatCols <- function(len){
  if(len > 50){
    ht.col <- c(substr(heat.colors(50), 0, 7), rep("#FFFFFF", len-50));
  }else{
    # reduce to hex by remove the last character so HTML understand
    ht.col <- substr(heat.colors(len), 0, 7);
  }
}