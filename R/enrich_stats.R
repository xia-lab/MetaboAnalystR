### Various enrichment analysis algorithms
### Jeff Xia \email{jeff.xia@mcgill.ca}
### McGill University, Canada
### License: MIT License

#'Over-representation analysis using hypergeometric tests
#'@description Over-representation analysis using hypergeometric tests
#'The probability is calculated from obtaining equal or higher number
#'of hits using 1-phyper. Since phyper is a cumulative probability,
#'to get P(X>=hit.num) => P(X>(hit.num-1))
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
CalculateHyperScore <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make a clean dataSet$cmpd data based on name mapping
  # only valid hmdb name will be used
  nm.map <- GetFinalNameMap(mSetObj);
  valid.inx <- !(is.na(nm.map$hmdb)| duplicated(nm.map$hmdb));
  ora.vec <- nm.map$hmdb[valid.inx];
  
  q.size<-length(ora.vec);
  
  if(all(is.na(ora.vec)) || q.size==0) {
    AddErrMsg("No valid HMDB compound names found!");
    return(0);
  }

  # move to api only if R package + KEGG msets
  if(!.on.public.web & grepl("kegg", mSetObj$analSet$msetlibname)){
    
    # make this lazy load
    if(!exists("my.hyperscore.kegg")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }

    mSetObj$api$oraVec <- ora.vec; 
    
    if(mSetObj$api$filter){
      mSetObj$api$filterData <- mSetObj$dataSet$metabo.filter.kegg
      toSend <- list(mSet = mSetObj, libNm = mSetObj$api$libname, filter = mSetObj$api$filter,
                     oraVec = mSetObj$api$oraVec, filterData = mSetObj$api$filterData,
                     excludeNum = mSetObj$api$excludeNum)
    }else{
      toSend <- list(mSet = mSetObj,libNm = mSetObj$api$libname, 
                     filter = mSetObj$api$filter, oraVec = mSetObj$api$oraVec, excludeNum = mSetObj$api$excludeNum)
    }
    saveRDS(toSend, "tosend.rds")
    return(my.hyperscore.kegg());
  }
  
  current.mset <- current.msetlib$member;
  
  # make a clean metabilite set based on reference metabolome filtering
  # also need to update ora.vec to the updated mset
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet$metabo.filter.hmdb)){
    current.mset <- lapply(current.mset, function(x){x[x %in% mSetObj$dataSet$metabo.filter.hmdb]})
    mSetObj$dataSet$filtered.mset <- current.mset;
  }

  set.size<-length(current.mset);
  if(set.size ==1){
    AddErrMsg("Cannot perform enrichment analysis on a single metabolite set!");
    return(0);
  }
  
  # now perform enrichment analysis

  # update data & parameters for ORA stats, based on suggestion
  # https://github.com/xia-lab/MetaboAnalystR/issues/168
  # https://github.com/xia-lab/MetaboAnalystR/issues/96
  # https://github.com/xia-lab/MetaboAnalystR/issues/34

  # the universe based on reference metabolome (should not matter, because it is already filtered based on the reference previously)

  my.univ <- unique(unlist(current.mset, use.names=FALSE));
  if(!is.null(mSetObj$dataSet$metabo.filter.hmdb)){
    my.univ <- unique(mSetObj$dataSet$metabo.filter.hmdb);
  }
  uniq.count <- length(my.univ);
  ora.vec <- ora.vec[ora.vec %in% my.univ];
  q.size <- length(ora.vec); 
  
  hits<-lapply(current.mset, function(x){x[x %in% ora.vec]});  
  hit.num<-unlist(lapply(hits, function(x) length(x)), use.names = FALSE);
  
  if(sum(hit.num>0)==0){
    AddErrMsg("No match was found to the selected metabolite set library!");
    return(0);
  }
  
  set.num<-unlist(lapply(current.mset, length), use.names = FALSE);
  
  # prepare for the result table
  res.mat<-matrix(NA, nrow=set.size, ncol=6);        
  rownames(res.mat)<-names(current.mset);
  colnames(res.mat)<-c("total", "expected", "hits", "Raw p", "Holm p", "FDR");
  for(i in 1:set.size){
    res.mat[i,1]<-set.num[i];
    res.mat[i,2]<-q.size*(set.num[i]/uniq.count);
    res.mat[i,3]<-hit.num[i];
    
    # use lower.tail = F for P(X>x)
    # phyper("# of white balls drawn", "# of white balls in the urn", "# of black balls in the urn", "# of balls drawn")
    res.mat[i,4]<-phyper(hit.num[i]-1, set.num[i], uniq.count-set.num[i], q.size, lower.tail=F);
  }
  
  # adjust for multiple testing problems
  res.mat[,5] <- p.adjust(res.mat[,4], "holm");
  res.mat[,6] <- p.adjust(res.mat[,4], "fdr");
  
  res.mat <- res.mat[hit.num>0,];
  
  ord.inx<-order(res.mat[,4]);
  mSetObj$analSet$ora.mat <- signif(res.mat[ord.inx,],3);
  mSetObj$analSet$ora.hits <- hits;
  
  fast.write.csv(mSetObj$analSet$ora.mat, file="msea_ora_result.csv");
  return(.set.mSet(mSetObj));
}

#'Quantitative enrichment analysis with globaltest
#'@description Various enrichment analysis algorithms
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
CalculateGlobalTestScore <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    .prepare.globaltest.score(mSetObj);
    .perform.computing();   
    .save.globaltest.score(mSetObj);  
  }else{
    mSetObj <- .prepare.globaltest.score(mSetObj);
    
    if(!grepl("kegg", mSetObj$analSet$msetlibname)){
      .perform.computing();   
      mSetObj <- .save.globaltest.score(mSetObj);  
    }
  } 
  return(.set.mSet(mSetObj));
}

.prepare.globaltest.score <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  # now, need to make a clean dataSet$norm data based on name mapping
  # only contain valid hmdb hit will be used
  nm.map <- GetFinalNameMap(mSetObj);
  valid.inx <- !(is.na(nm.map$hmdb)| duplicated(nm.map$hmdb));
  nm.map <- nm.map[valid.inx,];
  orig.nms <- nm.map$query;
  
  hmdb.inx <- match(colnames(mSetObj$dataSet$norm),orig.nms);
  hit.inx <- !is.na(hmdb.inx);
  msea.data <- mSetObj$dataSet$norm[,hit.inx];
  colnames(msea.data) <- nm.map$hmdb[hmdb.inx[hit.inx]];

  if(!.on.public.web & grepl("kegg", mSetObj$analSet$msetlibname)){
    # make this lazy load
    if(!exists("my.qea.kegg")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }

    mSetObj$api$mseaDataColNms <- colnames(msea.data)
    msea.data <- as.matrix(msea.data)
    dimnames(msea.data) = NULL
    mSetObj$api$mseaData <- msea.data; 
    mSetObj$api$cls <- mSetObj$dataSet$cls
    
    if(mSetObj$api$filter){
      mSetObj$api$filterData <- mSetObj$dataSet$metabo.filter.hmdb
      
      toSend <- list(mSet = mSetObj, libNm = mSetObj$api$libname, filter = mSetObj$api$filter, mseaData = mSetObj$api$mseaData, mseaDataColNms = mSetObj$api$mseaDataColNms, 
                     filterData = mSetObj$api$filterData, cls = mSetObj$api$cls, excludeNum = mSetObj$api$excludeNum)
    }else{
      toSend <- list(mSet = mSetObj, libNm = mSetObj$api$libname, filter = mSetObj$api$filter, mseaData = mSetObj$api$mseaData, mseaDataColNms = mSetObj$api$mseaDataColNms, 
                     cls = mSetObj$api$cls, excludeNum = mSetObj$api$excludeNum)
    }
    saveRDS(toSend, "tosend.rds")
    return(my.qea.kegg());
  }
  
  current.mset <- current.msetlib$member;
  # make a clean metabolite set based on reference metabolome filtering
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet$metabo.filter.hmdb)){
    current.mset <- lapply(current.msetlib$member, function(x){x[x %in% mSetObj$dataSet$metabo.filter.hmdb]})
    mSetObj$dataSet$filtered.mset <- current.mset;
  }

  set.size <- length(current.msetlib);
  if(set.size == 1){
    AddErrMsg("Cannot perform enrichment analysis on a single metabolite sets!");
    return(0);
  }

  # now, perform the enrichment analysis  

  # first, get the matched entries from current.mset
  hits <- lapply(current.mset, function(x){x[x %in% colnames(msea.data)]});
  phenotype <- mSetObj$dataSet$cls;
  
  # there are more steps, better drop a function to compute in the remote env.
  my.fun <- function(){
    gt.obj <- globaltest::gt(dat.in$cls, dat.in$data, subsets=dat.in$subsets);
    gt.res <- globaltest::result(gt.obj);
    
    match.num <- gt.res[,5];
    if(sum(match.num>0)==0){
      return(NA);
    }
    all.cmpds <- unlist(gt.obj@subsets, recursive = TRUE, use.names = FALSE);
    all.cmpds <- unique(all.cmpds);
    stat.mat <- matrix(0, length(all.cmpds), 5);
    colnames(stat.mat) <-  c("p", "S", "ES", "sdS", "ncov")
    rownames(stat.mat) <- all.cmpds
    for(i in 1:length(all.cmpds)){
      stat.mat[i,] <- gt.obj@functions$test(all.cmpds[i]);
    }
    return(list(gt.res=gt.res, pvals=stat.mat[,1]));
  }
  
  dat.in <- list(cls=phenotype, data=msea.data, subsets=hits, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");
  
  # store necessary data 
  mSetObj$analSet$set.num <- unlist(lapply(current.mset, length), use.names = FALSE);
  mSetObj$analSet$qea.hits <- hits;
  mSetObj$analSet$msea.data <- msea.data;
  return(.set.mSet(mSetObj));
}

.save.globaltest.score <- function(mSetObj = NA){

  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  my.res <- dat.in$my.res;
  set.num <- mSetObj$analSet$set.num;
  
  if(length(my.res)==1 && is.na(my.res)){
    AddErrMsg("No match was found to the selected metabolite set library!");
    return(0);
  }
  
  mSetObj$analSet$qea.pvals <- my.res$pvals; # p value for individual cmpds
  gt.res <- my.res$gt.res;
  
  raw.p <- gt.res[,1];
  # add adjust p values
  bonf.p <- p.adjust(raw.p, "holm");
  fdr.p <- p.adjust(raw.p, "fdr");
  
  res.mat <- cbind(set.num, gt.res[,5], gt.res[,2], gt.res[,3], raw.p, bonf.p, fdr.p);
  rownames(res.mat) <- rownames(gt.res);
  colnames(res.mat) <- c("Total Cmpd", "Hits", "Statistic Q", "Expected Q", "Raw p", "Holm p", "FDR");
  
  hit.inx<-res.mat[,2]>0;
  res.mat<-res.mat[hit.inx, , drop = FALSE];
  ord.inx<-order(res.mat[,5]);
  res.mat<-res.mat[ord.inx, , drop = FALSE];
  mSetObj$analSet$qea.mat <- signif(res.mat,5);
  fast.write.csv(mSetObj$analSet$qea.mat, file="msea_qea_result.csv");
  
  return(.set.mSet(mSetObj));
}

#'Single sample profiling to compare with
#'@description reference concentrations stored in the library
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
CalculateSSP<-function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # first update the compound name to hmdb valid name
  nm.map <- GetFinalNameMap(mSetObj);
  
  valid.inx <- !(is.na(nm.map$hmdb)|duplicated(nm.map$hmdb));
  nm.map <- nm.map[valid.inx,];
  orig.nms <- nm.map$query;
  
  hmdb.inx <- match(mSetObj$dataSet$cmpd, orig.nms);
  match.inx <- !is.na(hmdb.inx);
  
  # note, must use "as.character" since string column from data frame will be converted to factors
  # when they used with numerics, they will be changed to numbers, not string
  ssp.nm <- as.character(nm.map$hmdb[hmdb.inx[match.inx]]);
  ssp.vec <- mSetObj$dataSet$norm[match.inx];
  
  cmpd.db <- .get.my.lib("compound_db.qs");
  
  hit.inx <- match(tolower(ssp.nm), tolower(cmpd.db$name));
  
  # create the result mat
  res.mat<-matrix("NA", nrow=length(ssp.nm), ncol=6);
  colnames(res.mat)<-c("name","conc", "hmdb", "refs", "state", "details");
  
  ssp.lows <- list();
  ssp.highs <- list();
  ssp.means <- list();
  ssp.refs <- list();
  ssp.pmids <- list();
  ssp.notes <- list();
  for(i in 1:length(ssp.nm)){
    inx <- hit.inx[i];
    if(is.na(inx)){ # no match to HMDB ID
      res.mat[i, ]<-c(ssp.nm[i],ssp.vec[i], "--", "--", "--", "");
      ssp.lows[[i]]<-NA;
      ssp.highs[[i]]<-NA;
      ssp.means[[i]]<-NA;
      ssp.refs[[i]]<-NA;
      ssp.pmids[[i]]<-NA;
      ssp.notes[[i]] <- NA;
    }else{
      hits <- Get.ConcRef(mSetObj, ssp.nm[i]);
      if(is.na(hits)){ # no conc info
        res.mat[i, ]<-c(ssp.nm[i], ssp.vec[i], cmpd.db$hmdb[inx], "--", "--", "");
        ssp.lows[[i]]<-NA;
        ssp.highs[[i]]<-NA;
        ssp.means[[i]]<-NA;
        ssp.refs[[i]]<-NA;
        ssp.pmids[[i]]<-NA;
        ssp.notes[[i]] <- NA;
      }else{ # concentration info
        concs<-as.numeric(unlist(strsplit(hits$conc, " - ", fixed=TRUE), use.names = FALSE));
        pmid <- hits$pmid;
        refs <- hits$refs;
        
        low.inx<-seq(1,length(concs)-2, 3);
        mean.inx<-seq(2,length(concs)-1, 3);
        high.inx<-seq(3,length(concs), 3);
        low.conc<-concs[low.inx];
        mean.conc <-concs[mean.inx];
        high.conc<-concs[high.inx];
        conc.show <- paste(mean.conc, " (", low.conc, " - ", high.conc, ")", sep="", collapse="; ");
        
        ssp.lows[[i]]<-low.conc;
        ssp.means[[i]]<-mean.conc;
        ssp.highs[[i]]<-high.conc;
        ssp.refs[[i]]<-hits$refs;
        ssp.pmids[[i]]<-hits$pmid;
        if(is.na(hits$note)){
          ssp.notes[[i]] <- NA;
        }else{
          ssp.notes[[i]] <- hits$note;
        }
        state <- NULL;
        if(ssp.vec[i]<min(low.conc)){
          state = "L";
        }else if(ssp.vec[i]>max(high.conc)){
          state = "H";
        }else{
          state = "M";
        }
        res.mat[i, ]<-c(ssp.nm[i], ssp.vec[i], cmpd.db$hmdb[inx], conc.show, state, "View");
      }
    }
  }
  names(ssp.highs) <- names(ssp.lows) <- names(ssp.means) <- names(ssp.refs) <- names(ssp.pmids) <- names(ssp.notes)<- ssp.nm;
  mSetObj$analSet$ssp.highs <- ssp.highs;
  mSetObj$analSet$ssp.lows <- ssp.lows;
  mSetObj$analSet$ssp.means <- ssp.means;
  mSetObj$analSet$ssp.refs <- ssp.refs;
  mSetObj$analSet$ssp.pmids <- ssp.pmids;
  mSetObj$analSet$ssp.notes <- ssp.notes;
  mSetObj$analSet$ssp.mat <- res.mat;
  return(.set.mSet(mSetObj));
}

#'Create a pie chart for compound classes
#'@description This function creates a pie-chart for compound classes
#'in the enrichment analysis if the library is based on chemical ontologies.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param enrichType enrichType
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Numeric, input the width, the default is 8.
#'@param maxClass Numeric, input the maximum number of lipid classes to
#'include in the pie-chart. By default this is set to 15.
#'@param colPal Character, input the preferred R Color Brewer palette to be
#'used for the pie chart. By default this is set to "Set1".
#'@import ggplot2

PlotEnrichPieChart <- function(mSetObj=NA, enrichType, imgName, format="png", dpi=72, width=8,
                                 maxClass = 15, colPal = "Set1"){
  
  mSetObj <- .get.mSet(mSetObj);

  if(.on.public.web){
    load_ggplot()
  }
  
  # make a clean dataSet$cmpd data based on name mapping
  # only valid hmdb name will be used
  nm.map <- GetFinalNameMap(mSetObj);
  valid.inx <- !(is.na(nm.map$hmdb)| duplicated(nm.map$hmdb));
  ora.vec <- nm.map$hmdb[valid.inx];
  
  q.size <- length(ora.vec);
  
  if(all(is.na(ora.vec)) || q.size==0) {
    AddErrMsg("No valid HMDB compound names found!");
    return(0);
  }
  
  current.mset <- current.msetlib$member
  
  # make a clean metabilite set based on reference metabolome filtering
  # also need to update ora.vec to the updated mset
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet$metabo.filter.hmdb)){
    current.mset <- lapply(current.mset, function(x){x[x %in% mSetObj$dataSet$metabo.filter.hmdb]})
    mSetObj$dataSet$filtered.mset <- current.mset;
    ora.vec <- ora.vec[ora.vec %in% unique(unlist(current.mset, use.names = FALSE))]
    q.size <- length(ora.vec);
  }
  
  set.size <- length(current.mset);
  
  if(set.size ==1){
    AddErrMsg("Cannot create pie-chart for a single metabolite set!");
    return(0);
  }
  
  hits <- lapply(current.mset, function(x){x[x %in% ora.vec]});
  hit.num <- unlist(lapply(hits, function(x) length(x)), use.names = FALSE);
  
  if(sum(hit.num>0)==0){
    AddErrMsg("No matches were found to the selected metabolite set library!");
    return(0);
  }
  
  hit.members <- unlist(lapply(hits, function(x) paste(x, collapse = "; ")))
  
  pie.data <- data.frame(Group = names(hits), Hits = as.numeric(hit.num), Members = hit.members)
  pie.data <- pie.data[!(pie.data[,2]==0), ]
  ord.inx <- order(pie.data[,2], decreasing = T);
  pie.data <- pie.data[ord.inx, , drop = FALSE];
  
  if(nrow(pie.data) > maxClass){
    pie.data <- pie.data[1:maxClass,]
  }

  mSetObj$analSet$enrich.pie.data <- pie.data
  
  if(nrow(pie.data) > 9){
    col.fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, colPal))
    group_colors <- col.fun(nrow(pie.data))
  }else{
    group_colors <- RColorBrewer::brewer.pal(8, colPal)[1:nrow(pie.data)]
  }
  
  names(group_colors) <- pie.data[,1]
  mSetObj$analSet$enrich.pie.cols <- group_colors
  
  # Basic piechart
  p <- ggplot(pie.data, aes(x="", y=Hits, fill=Group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) + theme_void() +
    scale_fill_manual(values = group_colors) +
    theme(plot.margin = unit(c(5, 7.5, 2.5, 5), "pt")) +
    theme(legend.text=element_text(size=12),
          legend.title=element_text(size=13))

  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  
  long.name <- max(nchar(pie.data[,1]))
  
  if(long.name > 25){
    w <- 10
    h <- 7
  }else{
    h <- width - 1 
    w <- width
  }

  ggsave(p, filename = imgName, dpi=dpi, width=w, height=h, limitsize = FALSE)
  fast.write.csv(mSetObj$analSet$enrich.pie.data, file="msea_pie_data.csv");
  return(.set.mSet(mSetObj));
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetSSP.Names<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,1]);
}

# measured concentration
GetSSP.Concs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,2]);
}

GetSSP.HMDB<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,3]);
}

GetSSP.RefConcs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,4]);
}

GetSSP.States<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,5]);
}

GetSSP.Details<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.mat[,6]);
}

GetSSP.RefConc<-function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  if(is.na(mSetObj$analSet$ssp.means[[nm]])){
    return ("NA");
  }
  return(paste(mSetObj$analSet$ssp.means[[nm]], " (", mSetObj$analSet$ssp.lows[[nm]], " - ", mSetObj$analSet$ssp.highs[[nm]], ")", sep=""));
}

GetSSP.Refs<-function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.refs[[nm]]);
}

GetSSP.Pmids<-function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.pmids[[nm]]);
}

GetSSP.Notes<-function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ssp.notes[[nm]]);
}

#'Replace the last column of the ssp.mat with the final selection from users
#'@description Replace the last column of the ssp.mat with the final selection from users
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
GetSSPTable<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  ssp.res<-mSetObj$analSet$ssp.mat[,-c(1,3,6)];
  rownames(ssp.res)<-mSetObj$analSet$ssp.mat[,1]
  selected.col<-rep(0, nrow(ssp.res));
  inx<-match(mSetObj$dataSet$cmpd, mSetObj$analSet$ssp.mat[,1]);
  selected.col[inx]<-1;
  
  print(xtable::xtable(cbind(ssp.res, selected = selected.col),align="l|l|p{8cm}|c|c", caption="Comparison with Reference Concentrations"),
        tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
}

#'Given a metset inx, return hmtl highlighted metset cmpds and references
#'@description Given a metset inx, return hmtl highlighted metset cmpds and references
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param msetNm Input the name of the metabolite set
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
GetHTMLMetSet<-function(mSetObj=NA, msetNm){
  mSetObj <- .get.mSet(mSetObj);
  
  hits <- NULL;
  
  if(mSetObj$analSet$type=="msetora" || mSetObj$analSet$type=="msetssp"){
    hits <- mSetObj$analSet$ora.hits;
  }else{
    hits <- mSetObj$analSet$qea.hits;
  }
  
  # highlighting with different colors
  # this is meaningless for very large (>200) metabolite sets (i.e. chemical class)
  mset <- current.msetlib$member[[msetNm]];
  if(length(mset) < 200){
    red.inx <- which(mset %in% hits[[msetNm]]);
  }else{
    mset <- hits[[msetNm]];
    red.inx <- 1:length(mset);
  }

  mset[red.inx] <- paste("<font color=\"red\">", "<b>", mset[red.inx], "</b>", "</font>",sep="");
  if(mSetObj$dataSet$use.metabo.filter && exists('filtered.mset')){
      grey.inx <- which(!(mset %in% filtered.mset[[msetNm]]));
      mset[grey.inx] <- paste("<font color=\"grey\">", "<b>", mset[grey.inx], "</b>", "</font>",sep="");
  }
  # get references
  matched.inx <- match(tolower(msetNm), tolower(current.msetlib$name))[1];
  return(cbind(msetNm, paste(mset, collapse="; "), current.msetlib$reference[matched.inx]));
}

#'Given a metset inx, give its name
#'@description Given a metset inx, give its name
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param msetInx Input the index of the metabolite set
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'@export
#'
GetMetSetName<-function(mSetObj=NA, msetInx){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$analSet$type=="msetora" || mSetObj$analSet$type=="msetssp"){
    msetNm <- rownames(mSetObj$analSet$ora.mat)[msetInx];
  }else{
    msetNm <- rownames(mSetObj$analSet$qea.mat)[msetInx];
  }
  return (msetNm);
}

GetORA.colorBar<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  len <- nrow(mSetObj$analSet$ora.mat);
  ht.col <- GetMyHeatCols(len);
  rbg.cols <- hex2rgb(ht.col);
  return (rbg.cols);
}

GetORA.rowNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  nms <- rownames(mSetObj$analSet$ora.mat);
  if(is.null(nms)){
    return("NA");
  }
  return (nms);
}

GetORA.mat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$ora.mat);
}

#'Get ORA table
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetORATable<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$analSet$ora.mat;
  if(substr(mSetObj$analSet$type, 0, 4) == 'mset'){
    print(xtable::xtable(mSetObj$analSet$ora.mat,align="p{5cm}|l|l|l|l|l|l", display=c("s","d","f","d","E","E","E"), caption="Result from Over Representation Analysis"),
          tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
  }else{
    rownames(res)<-GetORA.pathNames(mSetObj);
    print(xtable::xtable(res,align="p{5cm}|l|l|l|l||ll|l|l", display=c("s","d","f","d","E","E", "E","E", "f"),
                         caption="Result from Pathway Analysis"),
          tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
  }      
}

GetQEA.colorBar<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  len <- nrow(mSetObj$analSet$qea.mat);
  ht.col <- GetMyHeatCols(len);
  return (ht.col);
}

GetQEA.rowNames<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  nms <- rownames(mSetObj$analSet$qea.mat);
  if(is.null(nms)){
    return("NA");
  }
  return (nms);
}

GetQEA.mat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$qea.mat);
}

#'QEA table
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetQEATable<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$analSet$qea.mat;
  if(substr(mSetObj$analSet$type, 0, 4) == 'mset'){
    print(xtable::xtable(res,align="p{4cm}|l|l|l|l|l|l|l", display=c("s","d","d","f","f","E","E","E"),
                         caption="Result from Quantitative Enrichment Analysis"),
          tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
  }else{
    rownames(res)<- GetQEA.pathNames();
    print(xtable::xtable(res,align="p{5cm}|l|l|l|l|l|l|l", display=c("s","d","d","E","E", "E","E","f"),
                         caption="Result from Pathway Analysis"),
          tabular.environment = "longtable", caption.placement="top", size="\\scriptsize");
  }
}

GetEnrichPieNames <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$enrich.pie.data$Group)
}

GetEnrichPieHits <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  return(as.matrix(as.numeric(mSetObj$analSet$enrich.pie.data[,2])))
}

GetEnrichPieColors <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$enrich.pie.cols)
}