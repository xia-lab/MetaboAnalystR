#'Pattern hunter
#'@description Run template on all the high region effect genes
#'@param x Input data
#'@param template Input template
#'@param dist.name Input distance method 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
template.match <- function(x, template, dist.name) {
  k <- cor.test(x,template, method=dist.name);
  c(k$estimate, k$stat, k$p.value)
}

template.pmatch <- function(x, template, dist.name, cov.vars) {
  k <- pcor.test(x, template, cov.vars, method=dist.name);
  c(k$estimate, k$stat, k$p.value)
}

#'Match pattern for correlation analysis
#'@param mSetObj Input the name of the created mSetObj
#'@param dist.name Input the distance method, default is set to pearson
#'@param pattern Set the pattern, default is set to NULL
#'@export
#'
Match.Pattern <- function(mSetObj=NA, dist.name="pearson", pattern=NULL){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(pattern)){
    pattern <- paste(1:length(levels(mSetObj$dataSet$cls)), collapse="-");
  }
  templ <- as.numeric(ClearStrings(strsplit(pattern, "-", fixed=TRUE)[[1]]));
  
  if(all(templ==templ[1])){
    AddErrMsg("Cannot calculate correlation on constant values!");
    return(0);
  }
  
  new.template <- vector(mode="numeric", length=length(mSetObj$dataSet$cls))
  # expand to match each levels in the dataSet$cls
  all.lvls <- levels(mSetObj$dataSet$cls);
  
  if(length(templ)!=length(all.lvls)){
    AddErrMsg("Wrong template - must the same length as the group number!");
    return(0);
  }
  
  for(i in 1:length(templ)){
    hit.inx <- mSetObj$dataSet$cls == all.lvls[i]
    new.template[hit.inx] = templ[i];
  }
  
  cbtempl.results <- apply(mSetObj$dataSet$norm, 2, template.match, new.template, dist.name);
  
  cor.res <- t(cbtempl.results);
  
  fdr.col <- p.adjust(cor.res[,3], "fdr");
  cor.res <- cbind(cor.res, fdr.col);
  colnames(cor.res)<-c("correlation", "t-stat", "p-value", "FDR");
  ord.inx<-order(cor.res[,3]);
  
  sig.mat <- signif(cor.res[ord.inx,],5);
  
  fileName <- "correlation_pattern.csv";
  fast.write.csv(sig.mat,file=fileName);
  
  mSetObj$analSet$corr$sig.nm <- fileName;
  mSetObj$analSet$corr$cor.mat <- sig.mat;
  mSetObj$analSet$corr$pattern <- pattern;
  return(.set.mSet(mSetObj));
}

#'Pattern hunter, correlation plot
#'@description Plot correlation
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.
#'@param searchType searchType, default is "feature"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotCorr <- function(mSetObj=NA, imgName, searchType="feature", format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  cor.res <- mSetObj$analSet$corr$cor.mat;
  pattern <- mSetObj$analSet$corr$pattern;
  if(searchType == "feature"){
  title <- paste(GetVariableLabel(mSetObj$dataSet$type), "correlated with the", pattern);
  }else{
  title <- paste("Metadata correlated with the", pattern);
  }
  if(nrow(cor.res) > 25){
    
    # first get most signficant ones (p value)
    ord.inx<-order(cor.res[,3]);
    cor.res <- cor.res[ord.inx, ];
    cor.res <- cor.res[1:25, ];
    
    # then order by their direction (correlation)
    ord.inx<-order(cor.res[,1]);
    if(sum(cor.res[,1] > 0) == 0){ # all negative correlation
      ord.inx <- rev(ord.inx);
    }
    cor.res <- cor.res[ord.inx, ];
    if(searchType == "feature"){
    title <- paste("Top 25", tolower(GetVariableLabel(mSetObj$dataSet$type)), "correlated with the", pattern);
    }else{
    title <- paste("Top 25 metadata correlated with the", pattern);
    }
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 7.2;
  }else if(width == 0){
    w <- h <- 7.2;
  }else{
    w <- h <- width;
  }
  mSetObj$imgSet$corr <- imgName;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,6,4,3))
  rownames(cor.res)<-substr(rownames(cor.res), 1, 18);
  cols <- ifelse(cor.res[,1] >0, "mistyrose","lightblue");
  
  dotchart(cor.res[,1], pch="", xlim=c(-1,1), xlab="Correlation coefficients", main=title);
  rownames(cor.res) <- NULL;
  barplot(cor.res[,1], space=c(0.5, rep(0, nrow(cor.res)-1)), xlim=c(-1,1), xaxt="n", col = cols, add=T,horiz=T);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Pattern hunter, corr heatmap
#'@description Plot correlation heatmap
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'@param mSetObj Input name of the created mSet Object.
#'@param imgName Input the name of the image to create
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param target Input "row" to select features, or "col" to select samples. 
#'@param cor.method Indicate the correlation method, 'pearson', 'spearman', or 'kendall'.
#'@param colors Indicate the colors for the heatmap, "bwm" for default, "gbr" for red/green, "heat" for heat colors,
#'"topo" for topo colors, and "gray" for gray scale.
#'@param viewOpt Indicate "overview" to get an overview of the heatmap, and "detail" to get a detailed view of the heatmap.
#'@param fix.col Logical, fix colors (TRUE) or not (FALSE).
#'@param no.clst Logical, indicate if the correlations should be clustered (TRUE) or not (FALSE).
#'@param corrCutoff set corrCutoff
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import gplots
#'
PlotCorrHeatMap<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, target, cor.method, 
                          colors, fix.col, no.clst,fz, unit, corrCutoff=0){
  
  mSetObj <- .get.mSet(mSetObj);
  main <- xlab <- ylab <- NULL;
  data <- mSetObj$dataSet$norm;
  corrCutoff <- as.numeric(corrCutoff)
  print(c(fz,unit))
  if(target == 'row'){
    data <- t(data);
  }
  
  if(.on.public.web){
    if(ncol(data) > 1000){
        filter.val <- apply(data.matrix(data), 2, IQR, na.rm=T);
        rk <- rank(-filter.val, ties.method='random');
        data <- as.data.frame(data[,rk <=1000]);
        print("Data is reduced to 1000 vars ..");
    }
  }
  # compare p-values w. hmisc + cor.test
  colnames(data) <- substr(colnames(data), 1, 18);
  corr.mat <- cor(data, method=cor.method);

  # NA gives error w. hclust
  corr.mat[abs(corr.mat) < corrCutoff] <- 0;
  
  # save data for lazy pval computing
  mSetObj$analSet$pwcor <- list();
  mSetObj$analSet$pwcor$data <- data;
  mSetObj$analSet$pwcor$cor.method <- cor.method;
  mSetObj$analSet$pwcor$no.clst <- no.clst;

  if(.on.public.web){
    load_gplots()
    load_rcolorbrewer()
  }
  
  # set up parameter for heatmap
    if(colors=="gbr"){
        colors <- grDevices::colorRampPalette(c("green", "black", "red"), space="rgb")(256);
    }else if(colors == "heat"){
        colors <- grDevices::heat.colors(256);
    }else if(colors == "topo"){
        colors <- grDevices::topo.colors(256);
    }else if(colors == "gray"){
        colors <- grDevices::colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
    }else if(colors == "byr"){
        colors <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(256));
    }else if(colors == "viridis") {
        colors <- rev(viridis::viridis(10))
    }else if(colors == "plasma") {
        colors <- rev(viridis::plasma(10))
    }else if(colors == "npj"){
        colors <- c("#00A087FF","white","#E64B35FF")
    }else if(colors == "aaas"){
        colors <- c("#4DBBD5FF","white","#E64B35FF");
    }else if(colors == "d3"){
        colors <- c("#2CA02CFF","white","#FF7F0EFF");
    }else {
        colors <- c("#0571b0","#92c5de","white","#f4a582","#ca0020");
    }
  
  require(iheatmapr);
  plotjs <- paste0(imgName, ".json");
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$corr.heatmap <- imgName;
   
    w = max(min(1300,ncol(corr.mat)*unit+50),300)
    h = max(min(1300,nrow(corr.mat)*unit+50),300)
       corr.mat = round(corr.mat,5)
  if(fix.col){
    p <- iheatmap(corr.mat,  name = "Correlation<br>Coefficient", 
                  colors = colors,zmin=-1,zmid=0, zmax=1,
          colorbar_grid = setup_colorbar_grid(y_start = 0.85)) %>%
      add_row_labels(size = 0.2, side = "right",font = list(size = fz))%>%
      add_col_labels(size = 0.2, font = list(size = fz)) 
 
  }else{
    p <- iheatmap(corr.mat,  name = "Correlation<br>Coefficient" , 
                  colors = colors,zmin=min(corr.mat),zmid=mean(min(corr.mat),max(corr.mat)), zmax=max(corr.mat),
 colorbar_grid = setup_colorbar_grid(y_start = 0.85) ) %>%
      add_row_labels(size = 0.2, side = "right",font = list(size = fz))%>%
      add_col_labels(size = 0.2,font = list(size = fz) ) 
  }
  
  if(!no.clst){ # when no clustering, tree_row is NA
   p <- p  %>% add_row_clustering() %>% 
      add_col_clustering()
     new.ord <- p@xaxes@listData[["x"]]@order;
     corr.mat <- corr.mat[new.ord, new.ord];
     mSetObj$analSet$pwcor$new.ord <- new.ord;
    
  }
  
    mSetObj$imgSet$heatmap_stats_corr_param <- list();
    mSetObj$imgSet$heatmap_stats_corr_param$width <- w;
    mSetObj$imgSet$heatmap_stats_corr_param$height <- h;

    saveRDS(p, "heatmap_stats_corr.rds")

   as_list <- to_plotly_list(p)

     if(ncol(corr.mat)<100){
         w=w+(100-ncol(corr.mat))*6
         h=h+(100-ncol(corr.mat))*6
        }

    
    as_list[["layout"]][["width"]] <- w
    as_list[["layout"]][["height"]] <- h


    as_json <- attr(as_list, "TOJSON_FUNC")(as_list)
    as_json <- paste0("{ \"x\":", as_json, ",\"evals\": [],\"jsHooks\": []}")
 
     print(plotjs)
    write(as_json, plotjs)  
  fast.write.csv(signif(corr.mat, 5), file="correlation_table.csv");
  return(.set.mSet(mSetObj));
}

#'Create high resolution static HeatMap for download only
#'@description '@param #same as PlotCorrHeatMap
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
#'

PlotStaticCorrHeatMap<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, target, cor.method, 
                          colors, viewOpt, fix.col, no.clst, corrCutoff=0){
  
  mSetObj <- .get.mSet(mSetObj);
  main <- xlab <- ylab <- NULL;
  data <- mSetObj$dataSet$norm;
  corrCutoff <- as.numeric(corrCutoff)
  
  if(target == 'row'){
    data <- t(data);
  }
  
  if(.on.public.web){
    if(ncol(data) > 1000){
        filter.val <- apply(data.matrix(data), 2, IQR, na.rm=T);
        rk <- rank(-filter.val, ties.method='random');
        data <- as.data.frame(data[,rk <=1000]);
        print("Data is reduced to 1000 vars ..");
    }
  }
  # compare p-values w. hmisc + cor.test
  colnames(data) <- substr(colnames(data), 1, 18);
  corr.mat <- cor(data, method=cor.method);

  # NA gives error w. hclust
  corr.mat[abs(corr.mat) < corrCutoff] <- 0;
  
  # save data for lazy pval computing
  mSetObj$analSet$pwcor <- list();
  mSetObj$analSet$pwcor$data <- data;
  mSetObj$analSet$pwcor$cor.method <- cor.method;
  mSetObj$analSet$pwcor$no.clst <- no.clst;

  if(.on.public.web){
    load_gplots()
    load_rcolorbrewer()
  }
  
  # set up parameter for heatmap
   if(colors=="gbr"){
    colors <- grDevices::colorRampPalette(c("green", "black", "red"), space="rgb")(256);
  }else if(colors == "heat"){
    colors <- grDevices::heat.colors(256);
  }else if(colors == "topo"){
    colors <- grDevices::topo.colors(256);
  }else if(colors == "gray"){
    colors <- grDevices::colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
  }else if(colors == "byr"){
    colors <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlBu"))(256));
  }else if(colors == "viridis") {
    colors <- rev(viridis::viridis(10))
  }else if(colors == "plasma") {
    colors <- rev(viridis::plasma(10))
  }else if(colors == "npj"){
    colors <- c("#00A087FF","white","#E64B35FF")
  }else if(colors == "aaas"){
    colors <- c("#4DBBD5FF","white","#E64B35FF");
  }else if(colors == "d3"){
    colors <- c("#2CA02CFF","white","#FF7F0EFF");
  }else {
colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256));
  }
  
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(viewOpt == "overview"){
    if(is.na(width)){
      w <- 9;
    }else if(width == 0){
      w <- 7.2;
    }else{
      w <- 7.2;
    }
    h <- w;
    mSetObj$imgSet$corr.heatmap <- imgName;
    
  }else{
    if(ncol(corr.mat) > 50){
      myH <- ncol(corr.mat)*12 + 40;
    }else if(ncol(corr.mat) > 20){
      myH <- ncol(corr.mat)*12 + 60;
    }else{
      myH <- ncol(corr.mat)*12 + 120;
    }
    h <- round(myH/72,2);
    
    if(is.na(width)){
      w <- h;
    }else if(width == 0){
      w <- h <- 7.2;
      
    }else{
      w <- h <- 7.2;
    }
    mSetObj$imgSet$corr.heatmap <- imgName;
  }
  
  if(format=="pdf"){
    pdf(file = imgName, width=w, height=h, bg="white", onefile=FALSE);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  }
  
  if(no.clst){
    rowv=FALSE;
    colv=FALSE;
    dendro= "none";
  }else{
    rowv=TRUE;
    colv=TRUE;
    dendro= "both";
  }
   
  
  if(fix.col){
    breaks <- seq(from = -1, to = 1, length = 257);
    res <- pheatmap::pheatmap(corr.mat, 
                              fontsize=8, fontsize_row=8, 
                              cluster_rows = colv, 
                              cluster_cols = rowv,
                              color = colors,
                              breaks = breaks,  silent = TRUE
    );
   if(colv){
     res$tree_row$order <- rev(res$tree_row$order)
     colv <-  res$tree_row
   }else{
     corr.mat <- corr.mat[,ncol(corr.mat):1]
   }
  pheatmap::pheatmap(corr.mat, 
                              fontsize=8, fontsize_row=8, 
                              cluster_rows = colv, 
                              cluster_cols = rowv,
                              color = colors,
                              breaks = breaks
    );
  }else{
    res <- pheatmap::pheatmap(corr.mat, 
                              fontsize=8, fontsize_row=8, 
                              cluster_rows = colv, 
                              cluster_cols = rowv,
                              color = colors,  silent = TRUE
    );
  if(colv){
     res$tree_row$order <- rev(res$tree_row$order)
     colv <-  res$tree_row
   }else{
     corr.mat <- corr.mat[,ncol(corr.mat):1]
   }
  pheatmap::pheatmap(corr.mat, 
                              fontsize=8, fontsize_row=8, 
                              cluster_rows = colv, 
                              cluster_cols = rowv,
                              color = colors
    );
  }
  
  dev.off();

  if(!no.clst){ # when no clustering, tree_row is NA
    new.ord <- res$tree_row$order;
    corr.mat <- corr.mat[new.ord, new.ord];
    mSetObj$analSet$pwcor$new.ord <- new.ord;
  }

  return(.set.mSet(mSetObj));
}



# this is for p values for correlation heatmap (all pair-wise). 
# use Hmisc for fast but lazy computing 
ComputeCorrP <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  data <- mSetObj$analSet$pwcor$data;
  cor.method <- mSetObj$analSet$pwcor$cor.method;
  pval.mat <- Hmisc::rcorr(as.matrix(data), type=cor.method)$P;

  if(!mSetObj$analSet$pwcor$no.clst){
    new.ord <- mSetObj$analSet$pwcor$new.ord;
    pval.mat <- pval.mat[new.ord, new.ord];
  }

  fast.write.csv(signif(pval.mat,5), file="pval_corr_table.csv");
  return(1);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetCorrSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$corr$sig.nm;
}

GetCorSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(CleanNumber(mSetObj$analSet$corr$cor.mat));
}

GetCorSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$corr$cor.mat);
}

GetCorSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$corr$cor.mat);
}

#'Sig table for Correlation Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Corr <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$corr$cor.mat, "Pattern search using correlation analysis", mSetObj$dataSet$type);
}

GenerateTemplates <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  level.len <- length(levels(mSetObj$dataSet$cls));
  
  # only specify 4: increasing, decreasing, mid high, mid low, constant
  incs <- 1:level.len;
  desc <- level.len:1;
  
  if(level.len > 2){
    # use ceiling, so that the peak will be right for even length
    mid.pos <- ceiling((level.len+1)/2);
    mid.high <- c(1:mid.pos, seq(mid.pos-1,by=-1,length.out=level.len-mid.pos));
    mid.low <- c(mid.pos:1, seq(2, length.out=level.len-mid.pos));
    
    res <- rbind(incs, desc, mid.high, mid.low); # add the constant one
  }else{
    res <- rbind(incs, desc);
  }
  # turn into string
  res <- apply(res, 1, paste, collapse="-");
  
  # add the legends
  res <- c(paste(levels(mSetObj$dataSet$cls), collapse="-"), res);
  return(res);
}

#'Pattern hunter
#'@description Calculate correlation of all other feature to a given feature name
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param dist.name Input the name of the distance measure
#'@param varName Input the variable name
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
FeatureCorrelation <- function(mSetObj=NA, dist.name, varName){
  mSetObj <- .get.mSet(mSetObj);
  
  # test if varName is valid
  if(!varName %in% colnames(mSetObj$dataSet$norm)){
    AddErrMsg("Invalid feature name - not found! Feature might have been filtered out!");
    return(0);
  }
  
  cbtempl.results <- apply(mSetObj$dataSet$norm, 2, template.match, mSetObj$dataSet$norm[,varName], dist.name);
  cor.res<-t(cbtempl.results);
  
  fdr.col <- p.adjust(cor.res[,3], "fdr");
  cor.res <- cbind(cor.res, fdr.col);
  colnames(cor.res)<-c("correlation", "t-stat", "p-value", "FDR");
  ord.inx<-order(cor.res[,3])
  sig.mat <-signif(cor.res[ord.inx,],5);
  
  fileName <- "correlation_feature.csv";
  fast.write.csv(sig.mat,file=fileName);
  
  mSetObj$analSet$corr$sig.nm <- fileName;
  mSetObj$analSet$corr$cor.mat <- sig.mat;
  mSetObj$analSet$corr$pattern <- varName;
  
  return(.set.mSet(mSetObj));
}

###########################################
############ Utility Functions ############
###########################################

# Set of functions to perform cor.test
PearsonCorrFunc <- function(var1, var2, data){
  result <- cor.test(data[,var1], data[,var2])
  data.frame(var1, var2, result[c("estimate", "p.value", "statistic")], stringsAsFactors = FALSE)
}

SpearmanCorrFunc <- function(var1, var2, data){
  result <- cor.test(data[,var1], data[,var2], method = "spearman", exact = FALSE)
  data.frame(var1, var2, result[c("estimate", "p.value", "statistic")], stringsAsFactors = FALSE)
}

KendallCorrFunc <- function(var1, var2, data){
  result <- cor.test(data[,var1], data[,var2], method = "kendall", exact = FALSE)
  data.frame(var1, var2, result[c("estimate", "p.value", "statistic")], stringsAsFactors = FALSE)
}

# inject functions from the Network-DSPC script
ComputeDSPC <- function(mSetObj=NA){
     if(!exists("PerformDSPC")){ # public web on same user dir
         .load.scripts.on.demand("networks_enrich.Rc");  
         .load.scripts.on.demand("networks_view.Rc");      
     }
     return(PerformDSPC(mSetObj));
}