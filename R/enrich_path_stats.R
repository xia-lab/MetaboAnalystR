### Over-representation analysis using hypergeometric tests
### The probability is calculated from obtaining the equal or higher number
### of hits using 1-phyper. Since phyper is a cumulative probability,
### to get P(X>=hit.num) => P(X>(hit.num-1))

#'Calculate ORA score
#'@description Calculate the over representation analysis score
#'@usage CalculateOraScore(mSetObj=NA, nodeImp, method)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nodeImp Indicate the pathway topology analysis, "rbc" for relative-betweeness centrality, 
#'and "dgr" for out-degree centrality. 
#'@param method is "fisher" or "hyperg"
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CalculateOraScore <- function(mSetObj=NA, nodeImp, method){

  mSetObj <- .get.mSet(mSetObj);
  # make a clean dataSet$cmpd data based on name mapping
  # only valid kegg id will be used

  nm.map <- GetFinalNameMap(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    valid.inx <- !(is.na(nm.map$kegg)| duplicated(nm.map$kegg));
    ora.vec <- nm.map$kegg[valid.inx];
  } else if(mSetObj$pathwaylibtype == "SMPDB"){
    valid.inx <- !(is.na(nm.map$hmdbid)| duplicated(nm.map$hmdbid));
    ora.vec <- nm.map$hmdbid[valid.inx];
  }

  q.size<-length(ora.vec);
  if(all(is.na(ora.vec)) | q.size==0) {
    if(mSetObj$pathwaylibtype == "KEGG"){
      AddErrMsg("No valid KEGG compounds found!");
    } else if(mSetObj$pathwaylibtype == "SMPDB"){
      AddErrMsg("No valid SMPDB compounds found!");
    }
    return(0);
  }

  if(!.on.public.web & mSetObj$pathwaylibtype == "KEGG"){
    # make this lazy load
    if(!exists("my.ora.kegg")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }

    mSetObj$api$oraVec <- ora.vec; 
    mSetObj$api$method <- method;
    mSetObj$api$nodeImp <- nodeImp;

    if(mSetObj$api$filter){
      mSetObj$api$filterData <- mSetObj$dataSet$metabo.filter.kegg
      
      toSend <- list(mSet = mSetObj, libVersion = mSetObj$api$libVersion, libNm = mSetObj$api$libNm, filter = mSetObj$api$filter, nodeImp = mSetObj$api$nodeImp,
                     method = mSetObj$api$method, oraVec = mSetObj$api$oraVec, filterData = mSetObj$api$filterData)
    }else{
      toSend <- list(mSet = mSetObj, libVersion = mSetObj$api$libVersion, libNm = mSetObj$api$libNm, filter = mSetObj$api$filter, nodeImp = mSetObj$api$nodeImp,
                     method = mSetObj$api$method, oraVec = mSetObj$api$oraVec)
    }
    saveRDS(toSend, "tosend.rds")
    return(my.ora.kegg());
  }

  current.mset <- current.kegglib$mset.list;
  uniq.count <- current.kegglib$uniq.count;
  
  # check if need to be filtered against reference metabolome
  # TODO: address the following filtering for SMPDB if needed
  gd.sets <- names(current.mset);
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet$metabo.filter.kegg)){
    current.mset <- lapply(current.mset, function(x){x[x %in% mSetObj$dataSet$metabo.filter.kegg]});
    
    # remove those with length 0 (i.e. no members after filtering)
    gd.sets <- names(current.mset)[lapply(current.mset, length) > 0];
    current.mset <- current.mset[gd.sets];
    mSetObj$analSet$ora.filtered.mset <- current.mset; 
  }
 
  set.size<-length(current.mset);
  if(set.size < 2){
    AddErrMsg("Cannot perform enrichment analysis on a single metabolite set!");
    return(0);
  }
 
  # now perform enrichment analysis

  # update data & parameters for ORA stats, based on suggestion
  # https://github.com/xia-lab/MetaboAnalystR/issues/168
  my.univ <- unique(unlist(current.mset, use.names=FALSE));
  uniq.count <- length(my.univ);
  ora.vec <- ora.vec[ora.vec %in% my.univ];
  q.size <- length(ora.vec);   
  if(q.size < 3){
    AddErrMsg("Less than 3 metabolites left - too few to perform meaningful enrichment analysis!");
    return(0);
  }

  hits <- lapply(current.mset, function(x){x[x %in% ora.vec]});
  hit.num <-unlist(lapply(hits, function(x){length(x)}), use.names=FALSE);
  set.num <- unlist(lapply(current.mset, length), use.names=FALSE);
  
  # deal with no hits
  if(length(hits)==0){
    msg.vec <<- c(msg.vec, "No hits in the selected pathway library!")
    return(0)
  }
  
  # prepare for the result table
  res.mat<-matrix(0, nrow=set.size, ncol=8);
  rownames(res.mat)<-names(current.mset);
  colnames(res.mat)<-c("Total", "Expected", "Hits", "Raw p", "-log10(p)", "Holm adjust", "FDR", "Impact");
  
  if(nodeImp == "rbc"){
    imp.list <- current.kegglib$rbc;
    mSetObj$msgSet$topo.msg <- "Your selected node importance measure for topological analysis is relative betweenness centrality.";
  }else{
    imp.list <- current.kegglib$dgr;
    mSetObj$msgSet$topo.msg <- "Your selected node importance measure for topological analysis is out degree centrality.";
  }
  imp.list <- imp.list[gd.sets];

  res.mat[,1]<-set.num;
  res.mat[,2]<-q.size*(set.num/uniq.count);
  res.mat[,3]<-hit.num;
  
  if(method == "fisher"){
    res.mat[,4] <- GetFisherPvalue(hit.num, q.size, set.num, uniq.count);
    mSetObj$msgSet$rich.msg <- "The selected over-representation analysis method is Fishers' exact test.";
  }else{
    # use lower.tail = F for P(X>x)
    res.mat[,4] <- phyper(hit.num-1, set.num, uniq.count-set.num, q.size, lower.tail=F);
    mSetObj$msgSet$rich.msg <- "The selected over-representation analysis method is Hypergeometric test.";
  }
  
  res.mat[,5] <- -log10(res.mat[,4]);
  
  # adjust for multiple testing problems
  res.mat[,6] <- p.adjust(res.mat[,4], "holm");
  res.mat[,7] <- p.adjust(res.mat[,4], "fdr");
  # calculate the sum of importance
  res.mat[,8] <- mapply(function(x, y){sum(x[y])}, imp.list, hits);
  
  res.mat <- res.mat[hit.num>0, , drop=FALSE];
  res.mat <- res.mat[!is.na(res.mat[,8]), , drop=FALSE];
  
  if(nrow(res.mat) > 1){
    ord.inx <- order(res.mat[,4], res.mat[,8]);
    res.mat <- res.mat[ord.inx,];
  }
  
  mSetObj$analSet$ora.mat <- signif(res.mat,5);
  mSetObj$analSet$ora.hits <- hits;
  mSetObj$analSet$node.imp <- nodeImp;
  
  save.mat <- mSetObj$analSet$ora.mat;  
  hit.inx <- match(rownames(save.mat), current.kegglib$path.ids);
  rownames(save.mat) <- names(current.kegglib$path.ids)[hit.inx];
  fast.write.csv(save.mat, file="pathway_results.csv");
  
  return(.set.mSet(mSetObj));
}

#'Export pathway names from ORA analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetORA.pathNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  hit.inx <- match(rownames(mSetObj$analSet$ora.mat), current.kegglib$path.ids);
  return(names(current.kegglib$path.ids)[hit.inx]);
}

#'Calculate quantitative enrichment score
#'@description Calculate quantitative enrichment score
#'@usage CalculateQeaScore(mSetObj=NA, nodeImp, method)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nodeImp Indicate the pathway topology analysis, "rbc" for relative-betweeness centrality, 
#'and "dgr" for out-degree centrality. 
#'@param method Indicate the pathway enrichment analysis, global test is "gt" and global ancova is "ga".
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

# contains three inner functions to be compatible with microservice
CalculateQeaScore <- function(mSetObj=NA, nodeImp, method){
  
  mSetObj <- .get.mSet(mSetObj);
  mSetObj <- .prepare.qea.score(mSetObj, nodeImp, method); # on local, everything is done
  
  if(.on.public.web){
    .perform.computing();
    mSetObj <- .save.qea.score(mSetObj);  
  }
  
  return(.set.mSet(mSetObj));
}

.prepare.qea.score <- function(mSetObj=NA, nodeImp, method){

  mSetObj <- .get.mSet(mSetObj);
  
  # first, need to make a clean dataSet$norm data based on name mapping
  # only contain valid kegg id will be used
  nm.map <- GetFinalNameMap(mSetObj);
  
  if(mSetObj$pathwaylibtype == "KEGG"){
    valid.inx <- !(is.na(nm.map$kegg)| duplicated(nm.map$kegg));
  } else if(mSetObj$pathwaylibtype == "SMPDB"){
    valid.inx <- !(is.na(nm.map$hmdbid)| duplicated(nm.map$hmdbid));
  }
  
  nm.map <- nm.map[valid.inx,];
  orig.nms <- nm.map$query;
  
  kegg.inx <- match(colnames(mSetObj$dataSet$norm),orig.nms);
  hit.inx <- !is.na(kegg.inx);
  path.data<-mSetObj$dataSet$norm[,hit.inx];
  
  if(mSetObj$pathwaylibtype == "KEGG"){
    colnames(path.data) <- nm.map$kegg[kegg.inx[hit.inx]];
  } else if(mSetObj$pathwaylibtype == "SMPDB"){
    colnames(path.data) <- nm.map$hmdbid[kegg.inx[hit.inx]];
  }
  
  # calculate univariate p values when click indivisual compound node
  # use lm model for t-tests (with var.equal=T), one-way anova, and linear regression (continuous);
  univ.p <- apply(as.matrix(path.data), 2, function(x) {
    tmp <- try(lm(as.numeric(mSetObj$dataSet$cls)~x));
    if(class(tmp) == "try-error") {
      return(NA);
    }else{
      tmp<-anova(tmp)
      return(tmp[1,5]);
    }
  });
  
  names(univ.p) <- colnames(path.data);
  
  if(!.on.public.web & mSetObj$pathwaylibtype == "KEGG"){
    mSetObj$api$nodeImp <- nodeImp;
    mSetObj$api$method <- method;
    mSetObj$api$pathDataColNms <- colnames(path.data)
    path.data <- as.matrix(path.data)
    dimnames(path.data) = NULL
    mSetObj$api$pathData <- path.data; 
    mSetObj$api$univP <- as.numeric(univ.p);
    mSetObj$api$cls <- mSetObj$dataSet$cls
    
    if(mSetObj$api$filter){
      mSetObj$api$filterData <- mSetObj$dataSet$metabo.filter.kegg
      
      toSend <- list(mSet = mSetObj, libVersion = mSetObj$api$libVersion, libNm = mSetObj$api$libNm, filter = mSetObj$api$filter, nodeImp = mSetObj$api$nodeImp,
                     method = mSetObj$api$method, pathData = mSetObj$api$pathData, pathDataColNms = mSetObj$api$pathDataColNms, 
                     filterData = mSetObj$api$filterData, univP = mSetObj$api$univP, cls = mSetObj$api$cls)
    }else{
      toSend <- list(mSet = mSetObj, libVersion = mSetObj$api$libVersion, libNm = mSetObj$api$libNm, filter = mSetObj$api$filter, nodeImp = mSetObj$api$nodeImp,
                     method = mSetObj$api$method, pathData = mSetObj$api$pathData, pathDataColNms = mSetObj$api$pathDataColNms, 
                     univP = mSetObj$api$univP, cls = mSetObj$api$cls)
    }
    
    # send RDS to xialab api
    saveRDS(toSend, "tosend.rds");

   # make this lazy load
    if(!exists("my.pathway.qea")){ # public web on same user dir
      .load.scripts.on.demand("util_api.Rc");    
    }
    return(my.pathway.qea());
  }
  
  # now, perform topology & enrichment analysis
  current.mset <- current.kegglib$mset.list;
  uniq.count <- current.kegglib$uniq.count;
  
  # check if a reference metabolome is applied
  if(mSetObj$dataSet$use.metabo.filter && !is.null(mSetObj$dataSet[["metabo.filter.kegg"]])){
    current.mset<-lapply(current.mset, function(x) {x[x %in% mSetObj$dataSet$metabo.filter.kegg]});

    mSetObj$analSet$qea.filtered.mset <- current.mset;
    uniq.count <- length(unique(unlist(current.mset), use.names=FALSE));
  }
  
  hits <- lapply(current.mset, function(x) {x[x %in% colnames(path.data)]});
  hit.inx <- unlist(lapply(hits, function(x) {length(x)}), use.names=FALSE) > 0;
  hits <- hits[hit.inx]; # remove no hits
  
  # deal with no hits
  if(length(hits)==0){
    msg.vec <<- c(msg.vec, "No hits in the selected pathway library!")
    return(0)
  }
  
  # calculate the impact values
  if(nodeImp == "rbc"){
    imp.list <- current.kegglib$rbc[hit.inx];
    mSetObj$msgSet$topo.msg <- "Your selected node importance measure for topological analysis is relative betweenness centrality.";
  }else{
    imp.list <- current.kegglib$dgr[hit.inx];
    mSetObj$msgSet$topo.msg <- "Your selected node importance measure for topological analysis is out degree centrality.";
  }

  imp.vec <- mapply(function(x, y){sum(x[y])}, imp.list, hits);
  set.num<-unlist(lapply(current.mset[hit.inx], length), use.names=FALSE);
  
  if(method == "gt"){
    mSetObj$msgSet$rich.msg <- "The selected pathway enrichment analysis method is \\textbf{Globaltest}.";
    my.fun <- function(){
      gt.obj <- globaltest::gt(dat.in$cls, dat.in$data, subsets=dat.in$subsets);
      gt.res <- globaltest::result(gt.obj);
      return(gt.res[,c(5,1)]);
    }
  }else{
    mSetObj$msgSet$rich.msg <- "The selected pathway enrichment analysis method is \\textbf{GlobalAncova}.";
    my.fun <- function(){
      path.data <- dat.in$data;
      ga.out <- GlobalAncova::GlobalAncova(xx=t(path.data), group=dat.in$cls, test.genes=dat.in$subsets, method="approx");
      return(ga.out[,c(1,3)]);
    }
  }
  
  dat.in <- list(cls=mSetObj$dataSet$cls, data=path.data, subsets=hits, my.fun=my.fun);
  qs::qsave(dat.in, file="dat.in.qs");
  
  # store data before microservice
  mSetObj$analSet$qea.univp <- signif(univ.p,7);
  mSetObj$analSet$node.imp <- nodeImp;
  mSetObj$dataSet$norm.path <- path.data;
  mSetObj$analSet$qea.hits <- hits;
  mSetObj$analSet$imp.vec <- imp.vec;
  mSetObj$analSet$set.num <- set.num;
  
  return(.set.mSet(mSetObj));
}

.save.qea.score <- function(mSetObj = NA){
  mSetObj <- .get.mSet(mSetObj);
  dat.in <- qs::qread("dat.in.qs"); 
  qea.res <- dat.in$my.res;
  set.num <- mSetObj$analSet$set.num;
  imp.vec <- mSetObj$analSet$imp.vec;
  
  match.num <- qea.res[,1];
  raw.p <- qea.res[,2];
  
  log.p <- -log10(raw.p);
  # add adjust p values
  holm.p <- p.adjust(raw.p, "holm");
  fdr.p <- p.adjust(raw.p, "fdr");
  
  res.mat <- cbind(set.num, match.num, raw.p, log.p, holm.p, fdr.p, imp.vec);
  rownames(res.mat)<-rownames(qea.res);
  colnames(res.mat)<-c("Total Cmpd", "Hits", "Raw p", "-log10(p)", "Holm adjust", "FDR", "Impact");
  res.mat <- res.mat[!is.na(res.mat[,7]), , drop=FALSE];
  
  ord.inx<-order(res.mat[,3], -res.mat[,7]);
  res.mat<-signif(res.mat[ord.inx,],5);
  mSetObj$analSet$qea.mat <- res.mat;
  
  hit.inx <- match(rownames(res.mat), current.kegglib$path.ids);
  pathNames <- names(current.kegglib$path.ids)[hit.inx];
  rownames(res.mat) <- pathNames; # change from ids to names for users
  fast.write.csv(res.mat, file="pathway_results.csv");
  
  mSetObj$analSet$qea.pathNames <- pathNames;
  return(.set.mSet(mSetObj)); 
}

#'Export pathway names from QEA analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetQEA.pathNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$qea.pathNames);
}

#'Only works for human (hsa.rda) data
#'@description Only works for human (hsa.rda) data
#'2018 - works for ath, eco, mmu, sce
#'@param kegg.ids Input the list of KEGG ids to add SMPDB links
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetupSMPDBLinks <- function(kegg.ids){
  smpdb.vec <- names(current.kegglib$path.smps)[match(kegg.ids,current.kegglib$path.smps)];

  lk.len <- length(smpdb.vec);

  if(lk.len==0){return("");};

  all.lks <- vector(mode="character", length=lk.len);
  for(i in 1:lk.len){
    lks <- strsplit(as.character(smpdb.vec[i]), "; ", fixed=TRUE)[[1]];
    if(!is.na(lks[1])){
      all.lks[i]<-paste("<a href=http://www.smpdb.ca/view/",lks," target=_new>SMP</a>", sep="", collapse="\n");
    }
  }
  return(all.lks);
}

#'Only works for human (hsa.rda) data
#'@description Only works for human (hsa.rda) data
#'2018 - works for ath, eco, mmu, sce
#'@param smpdb.ids Input the list of SMPD ids to add SMPDB links
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SetupKEGGLinks <- function(smpdb.ids){
  kegg.vec <- current.kegglib$path.keggs[match(smpdb.ids,names(current.kegglib$mset.list))]
  lk.len <- length(kegg.vec);
  all.lks <- vector(mode="character", length=lk.len);
  for(i in 1:lk.len){
    lks <- strsplit(kegg.vec[i], "; ", fixed=TRUE)[[1]];
    if(!is.na(lks[1])){
      all.lks[i] <- paste("<a href=http://www.genome.jp/kegg-bin/show_pathway?",lks," target=_new>KEGG</a>", sep="");
      # all.lks[i]<-paste("<a href=http://pathman.smpdb.ca/pathways/",lks,"/pathway target=_new>SMP</a>", sep="", collapse="\n");
    }
  }
  return(all.lks);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Given a metset inx, return hmtl highlighted pathway cmpds
#'@description Given a metset inx, return hmtl highlighted pathway cmpds
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param msetNm Input the name of the metabolite set
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetHTMLPathSet <- function(mSetObj=NA, msetNm){
  mSetObj <- .get.mSet(mSetObj);
  pathid <- current.kegglib$path.ids[msetNm]; 
  mset <- current.kegglib$mset.list[[pathid]];
  
  hits <- NULL;
  if(mSetObj$analSet$type=="pathora"){
    hits <- mSetObj$analSet$ora.hits;
  }else{
    hits <- mSetObj$analSet$qea.hits;
  }
  
  # highlighting with different colors
  red.inx <- which(mset %in% hits[[pathid]]);
  
  # use actual cmpd names
  nms <- names(mset);
  nms[red.inx] <- paste("<font color=\"red\">", "<b>", nms[red.inx], "</b>", "</font>",sep="");
  return(cbind(msetNm, paste(unique(nms), collapse="; ")));
}

GetORA.keggIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    kegg.vec <- rownames(mSetObj$analSet$ora.mat);
    kegg.vec <- paste("<a href=http://www.genome.jp/kegg-bin/show_pathway?",kegg.vec," target=_new>KEGG</a>", sep="");
  } else{ # pathwaylibtype == "HMDB"
    return(SetupKEGGLinks(rownames(mSetObj$analSet$ora.mat)));
  }
  return(kegg.vec);
}

#'Only for human pathways (SMPDB)
#'@description Only for human pathways + ath, eco, mmu & sce
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetORA.smpdbIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    return(SetupSMPDBLinks(rownames(mSetObj$analSet$ora.mat)));
  } else{
    hmdb.vec <- rownames(mSetObj$analSet$ora.mat);
    all.lks <-paste("<a href=http://www.smpdb.ca/view/",hmdb.vec," target=_new>SMP</a>", sep="");
    return(all.lks)
  }
}

#'Only for human pathways (KEGG)
#'@description Only for human pathways + ath, eco, mmu & sce
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetQEA.keggIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    kegg.vec <- rownames(mSetObj$analSet$qea.mat);
    kegg.vec <- paste("<a href=http://www.genome.jp/kegg-bin/show_pathway?",kegg.vec," target=_new>KEGG</a>", sep="");
  } else{ # pathwaylibtype == "HMDB"
    return(SetupKEGGLinks(rownames(mSetObj$analSet$qea.mat)));
  }
  return(kegg.vec);
}

GetQEA.smpdbIDs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    return(SetupSMPDBLinks(rownames(mSetObj$analSet$qea.mat)));
  } else{
    hmdb.vec <- rownames(mSetObj$analSet$qea.mat);
    all.lks <-paste("<a href=http://www.smpdb.ca/view/",hmdb.vec," target=_new>SMP</a>", sep="");
    return(all.lks)
  }
}

ComputePathHeatmap <-function(mSetObj=NA, libOpt, fileNm, type){
   json.res <- "";
   if(type == "pathqea"){
        json.res <- ComputePathHeatmapTable(mSetObj, libOpt, fileNm);
   }else{
        json.res <- ComputePathHeatmapList(mSetObj, libOpt, fileNm);
   }

   json.mat <- rjson::toJSON(json.res);
   sink(fileNm);
   cat(json.mat);
   sink();
   AddMsg("Data is now ready for heatmap visualization!");
   return(1);
}

ComputePathHeatmapTable <- function(mSetObj=NA, libOpt, fileNm){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  data <- t(dataSet$norm)
  sig.ids <- rownames(data);
  
  res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls);

  stat.pvals <- unname(as.vector(res[,2]));
  t.stat <- unname(as.vector(res[,1]));
  org <- unname(strsplit(libOpt,"_")[[1]][1])
  mSetObj$org <- org
  # scale each gene 
  dat <- t(scale(t(data)));
  
  rankPval = order(as.vector(stat.pvals))
  stat.pvals = stat.pvals[rankPval]
  dat = dat[rankPval,]

  t.stat = t.stat[rankPval]
  
  # now pearson and euclidean will be the same after scaleing
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  # do clustering and save cluster info
  # convert order to rank (score that can used to sort) 
  if(nrow(dat)> 1){
    dat.dist <- dist(dat);
    gene.ward.ord <- hclust(dat.dist, "ward.D")$order;
    gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[gene.ward.ord]);
    gene.ave.ord <- hclust(dat.dist, "ave")$order;
    gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[gene.ave.ord]);
    gene.single.ord <- hclust(dat.dist, "single")$order;
    gene.single.rk <- match(orig.gene.nms, orig.gene.nms[gene.single.ord]);
    gene.complete.ord <- hclust(dat.dist, "complete")$order;
    gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[gene.complete.ord]);
    
    dat.dist <- dist(t(dat));
    smpl.ward.ord <- hclust(dat.dist, "ward.D")$order;
    smpl.ward.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ward.ord])
    smpl.ave.ord <- hclust(dat.dist, "ave")$order;
    smpl.ave.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.ave.ord])
    smpl.single.ord <- hclust(dat.dist, "single")$order;
    smpl.single.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.single.ord])
    smpl.complete.ord <- hclust(dat.dist, "complete")$order;
    smpl.complete.rk <- match(orig.smpl.nms, orig.smpl.nms[smpl.complete.ord])
  }else{
    # force not to be single element vector which will be scaler
    #stat.pvals <- matrix(stat.pvals);
    gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
    smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(dat);
  }
  
  gene.cluster <- list(
    ward = gene.ward.rk,
    average = gene.ave.rk,
    single = gene.single.rk,
    complete = gene.complete.rk,
    pval = stat.pvals,
    stat = t.stat
  );
  
  sample.cluster <- list(
    ward = smpl.ward.rk,
    average = smpl.ave.rk,
    single = smpl.single.rk,
    complete = smpl.complete.rk
  );
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  meta <- data.frame(dataSet$cls);
  grps <- "Condition"
  nmeta <- meta.vec <- NULL;
  uniq.num <- 0;
  for (i in 1:ncol(meta)){
    cls <- meta[,i];
    grp.nm <- grps[i];
    meta.vec <- c(meta.vec, as.character(cls))
    # make sure each label are unqiue across multiple meta data
    ncls <- paste(grp.nm, as.numeric(cls)); # note, here to retain ordered factor
    nmeta <- c(nmeta, ncls);
  }
  
  # convert back to numeric 
  nmeta <- as.numeric(as.factor(nmeta))+99;
  unik.inx <- !duplicated(nmeta)   
  
  # get corresponding names
  meta_anot <- meta.vec[unik.inx]; 
  names(meta_anot) <- nmeta[unik.inx]; # name annotatation by their numbers
  
  nmeta <- matrix(nmeta, ncol=ncol(meta), byrow=F);
  colnames(nmeta) <- grps;
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks 
  res <- apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))});
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  gene.id = orig.gene.nms; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
  json.res <- list(
    data.type = dataSet$type,
    gene.id = gene.id,
    gene.entrez = gene.id,
    gene.name = gene.id,
    gene.cluster = gene.cluster,
    sample.cluster = sample.cluster,
    sample.names = orig.smpl.nms,
    meta = data.frame(nmeta),
    meta.anot = meta_anot,
    data = res,
    org = org
  );
  
  mSetObj$dataSet$hm_peak_names = gene.id
  mSetObj$dataSet$gene.cluster = gene.cluster
  
  .set.mSet(mSetObj)
  return(json.res);
}

ComputePathHeatmapList <- function(mSetObj=NA, libOpt, fileNm){

  mSetObj <- .get.mSet(mSetObj);
  # make a clean dataSet$cmpd data based on name mapping
  # only valid kegg id will be used
  
  nm.map <- GetFinalNameMap(mSetObj);
  if(mSetObj$pathwaylibtype == "KEGG"){
    valid.inx <- !(is.na(nm.map$kegg)| duplicated(nm.map$kegg));
    ora.vec <- mSetObj$dataSet$cmpd[valid.inx];
  } else if(mSetObj$pathwaylibtype == "SMPDB"){
    valid.inx <- !(is.na(nm.map$hmdbid)| duplicated(nm.map$hmdbid));
    ora.vec <- mSetObj$dataSet$cmpd[valid.inx];
  }

  exp.vec <- rep(0, length(ora.vec));
  dataSet <- mSetObj$dataSet
  dataSet$prot.mat <- data.frame(datalist1=exp.vec)
  rownames(dataSet$prot.mat) <- ora.vec

  sig.ids <- rownames(dataSet$prot.mat);
  gene.symbols=sig.ids
  stat.pvals <- dataSet$prot.mat[,1]
  
  expval <- 0
  expval <- sum(dataSet$prot.mat)
  
  # scale each gene 
  dat <- dataSet$prot.mat
  
  # now pearson and euclidean will be the same after scaleing
  dat.dist <- dist(dat); 
  
  orig.smpl.nms <- colnames(dat);
  orig.gene.nms <- rownames(dat);
  
  # prepare meta info    
  # 1) convert meta.data info numbers
  # 2) match number to string (factor level)
  
  grps <- "datalist1"
  cls <- "datalist1"
  
  # convert back to numeric 
  
  # for each gene/row, first normalize and then tranform real values to 30 breaks
  if(expval !=0){
    dat_pos <- as.matrix(dat[sign(dat[,1]) == 1,])
    dat_neg <- as.matrix(dat[sign(dat[,1]) == -1,])
    if(nrow(dat_pos) == 0){
      res <- apply(unname(dat), 2, function(x){
        y =log(abs(x)) + 0.000001
        16-as.numeric(cut(y, breaks=15))
      });
    }else if(nrow(dat_neg) == 0){
      res <- apply(unname(dat), 2, function(x){
        y =log(x) + 0.000001
        15+as.numeric(cut(y, breaks=15))
      });
    }else{
      res_pos <- apply(unname(dat_pos), 2, function(x){
        y =log(x) + 0.000001
        as.numeric(cut(y, breaks=15))+15
      });
      res_neg <- apply(unname(dat_neg), 2, function(x){
        y =log(abs(x)) + 0.000001
        16 - as.numeric(cut(y, breaks=15))
      });
      res <- rbind(res_pos, res_neg);
    }
  }else{
    zero.inx <- dataSet$prot.mat == 0
    res <- dataSet$prot.mat;
    res[zero.inx] <- 31
  }
  
  res_list <- list()
  for(i in 1:nrow(res)){
    res_list[[i]] <- unname(list(res[i,1]))
  }
  
  # note, use {} will lose order; use [[],[]] to retain the order
  
  nmeta <- list(100)
  nmeta.anot <- list()
  
  nmeta.anot["datalist1"] <- nmeta[1]
  
  nmeta <- list(nmeta)
  names(nmeta) <- "datalists"
  
  org <- unname(strsplit(libOpt,"_")[[1]][1])
  mSetObj$org <- org
  json.res <- list(
    data.type = "singlelist", 
    gene.id = gene.symbols,
    gene.entrez = sig.ids,
    gene.name = gene.symbols,
    gene.cluster = 1,
    sample.cluster = 1,
    sample.names = list("datalist1"),
    meta = nmeta,
    meta.anot = nmeta.anot,
    data = res_list,
    expval = expval,
    org = org
  );
  .set.mSet(mSetObj)
  return(json.res);
}
