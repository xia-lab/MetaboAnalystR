### Various enrichment analysis algorithms
### Jeff Xia \email{jeff.xia@mcgill.ca}
### McGill University, Canada
### License: GNU GPL (>= 2)

#'Over-representation analysis using hypergeometric tests
#'@description Over-representation analysis using hypergeometric tests
#'The probability is calculated from obtaining equal or higher number
#'of hits using 1-phyper. Since phyper is a cumulative probability,
#'to get P(X>=hit.num) => P(X>(hit.num-1))
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
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
  
  if(is.na(ora.vec) || q.size==0) {
    AddErrMsg("No valid HMDB compound names found!");
    return(0);
  }
  
  current.mset <- current.msetlib$member
  
  # make a clean metabilite set based on reference metabolome filtering
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet$metabo.filter.hmdb)){
    current.mset <- lapply(current.mset, function(x){x[x %in% mSetObj$dataSet$metabo.filter.hmdb]})
    mSetObj$dataSet$filtered.mset <- current.mset;
  }
  
  # total uniq cmpds in the current mset lib
  uniq.count <- length(unique(unlist(current.mset, use.names = FALSE)));
  
  set.size<-length(current.mset);
  
  if(set.size ==1){
    AddErrMsg("Cannot perform enrichment analysis on a single metabolite set!");
    return(0);
  }

  hits<-lapply(current.mset, function(x){x[x %in% ora.vec]});
  # lapply(current.mset, function(x) grepl("Ammonia", x))
  #hits<-lapply(current.mset, function(x) grepl(paste(ora.vec, collapse = "|"), x))
  
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
  
  write.csv(mSetObj$analSet$ora.mat, file="msea_ora_result.csv");
  return(.set.mSet(mSetObj));
}

#'Quantitative enrichment analysis with globaltest
#'@description Various enrichment analysis algorithms
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
CalculateGlobalTestScore <- function(mSetObj=NA){

  # now, perform the enrichment analysis
  set.size <- length(current.msetlib);
  if(set.size == 1){
    AddErrMsg("Cannot perform enrichment analysis on a single metabolite sets!");
    return(0);
  }

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
  current.mset <- current.msetlib$member; 
  
  # make a clean metabolite set based on reference metabolome filtering
  if(mSetObj$dataSet$use.metabo.filter && !is.null('mSetObj$dataSet$metabo.filter.hmdb')){
    current.mset <- lapply(current.msetlib$member, function(x){x[x %in% mSetObj$dataSet$metabo.filter.hmdb]})
    mSetObj$dataSet$filtered.mset <- current.mset;
  }

  set.num <- unlist(lapply(current.mset, length), use.names = FALSE);

  # first, get the matched entries from current.mset
  hits <- lapply(current.mset, function(x){x[x %in% colnames(msea.data)]});
  mSetObj$analSet$qea.hits <- hits;
  mSetObj$analSet$msea.data <- msea.data;

  phenotype <- mSetObj$dataSet$cls;
    
  print("Performing quantitative enrichment tests ......");
  load_RSclient()
  rsc <- RS.connect();
  RS.assign(rsc, "my.dir", getwd()); 
  RS.eval(rsc, setwd(my.dir));
  
  gt.out <- list(cls=phenotype, data=msea.data, subsets=hits, set.num=set.num);
  RS.assign(rsc, "gt.in", gt.out); 
  
  # there are more steps, better drop a function to compute in the remote env.
  my.fun <- function(){
    gt.obj <- globaltest::gt(gt.in$cls, gt.in$data, subsets=gt.in$subsets);
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
  RS.assign(rsc, my.fun);
  my.res <- RS.eval(rsc, my.fun());
  RS.close(rsc);
  
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
  res.mat<-res.mat[hit.inx, ];
  ord.inx<-order(res.mat[,5]);
  res.mat<-res.mat[ord.inx,];
  mSetObj$analSet$qea.mat <- signif(res.mat,5);
  write.csv(mSetObj$analSet$qea.mat, file="msea_qea_result.csv");
  
  return(.set.mSet(mSetObj));
}

#'Single sample profiling to compare with
#'@description reference concentrations stored in the library
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
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
  
  cmpd.db <- .read.metaboanalyst.lib("compound_db.rds");
  
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
        concs<-as.numeric(unlist(strsplit(hits$conc, " - "), use.names = FALSE));
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
#'License: GNU GPL (>= 2)
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
#'License: GNU GPL (>= 2)
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
  mset <- current.msetlib$member[[msetNm]];
  red.inx <- which(mset %in% hits[[msetNm]]);
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
#'License: GNU GPL (>= 2)
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
  if(len > 50){
    ht.col <- c(substr(heat.colors(50), 0, 7), rep("#FFFFFF", len-50));
  }else{
    # reduce to hex by remove the last character so HTML understand
    ht.col <- substr(heat.colors(len), 0, 7);
  }
  return (ht.col);
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
  if(len > 50){
    ht.col <- c(substr(heat.colors(50), 0, 7), rep("#FFFFFF", len-50));
  }else{
    # reduce to hex by remove the last character so HTML understand
    ht.col <- substr(heat.colors(len), 0, 7);
  }
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
