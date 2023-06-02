##################################################
## R scripts for ExpressAnalyst 
## Description: Meta Analysis Methods
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

# for multiple class, only select two
# also record all grp lbls
SetGroupContrast <- function(dataName, grps){
  dataSet <- readDataset(dataName);
  
  if(length(levels(dataSet$cls))>2){ 
    print("Updating group contrasts .....");
    grp.nms <- strsplit(grps, " vs. ")[[1]];
    sel.inx <- as.character(dataSet$cls) %in% grp.nms;
    
    # regenerate factor to drop levels, force the levels order
    group <- factor(dataSet$cls[sel.inx], levels=grp.nms);  
    data <- dataSet$data.norm[, sel.inx];
    dataSet$cls <- group;
    dataSet$data.norm <- data;
    RegisterData(dataSet);  
  }
}

#'Check if data are ready for meta-analysis
#'@description This function determines if all annotated data are ready for meta-analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export

CheckMetaDataIntegrity <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  paramSet$performedDE <- FALSE;
  msgSet <- readSet(msgSet, "msgSet");
  for(i in 1:length(mdata.all)){
    mdata.all[i] <- 1;
  }
  
  sel.nms <- names(mdata.all);
  clss <- list();
  if(paramSet$meta.upload){
    # update meta data only for select/deselect datasets
    inmex.meta.orig <- qs::qread("inmex.meta.orig.qs");
    hit.inx <- inmex.meta.orig$data.lbl %in% sel.nms;
    data <- inmex.meta.orig$data[, hit.inx];
    id.type <- inmex.meta.orig$id.type;
    cls.lbl <- factor(inmex.meta.orig$cls.lbl[hit.inx]);
    data.lbl <- factor(inmex.meta.orig$data.lbl[hit.inx]);
    common.matrix <- data;

  }else{   
    # first check that all class labels are consistent
    dataSet <- readDataset(sel.nms[1]);
    lvls <- levels(dataSet$cls);
    id.type <- dataSet$id.type;
    clss[[1]] <- dataSet$cls;
    nms <- rownames(dataSet$data.norm);
    shared.nms <- nms;
    for(i in 2:length(sel.nms)){
      dataSet <- readDataset(sel.nms[i]);
      clss[[i]] <- dataSet$cls;
      # check if class label is consistent
      if(!all(levels(dataSet$cls) == lvls)){
        msgSet$current.msg <- paste(sel.nms[i], "has different group labels", 
                              paste(levels(dataSet$cls), collapse=":"), 
                              "from", sel.nms[1], paste(lvls, collapse=":"));
        saveSet(msgSet, "msgSet");    
        return(0);
      }
      
      # check and record if there is common genes            
      shared.nms <- intersect(shared.nms, rownames(dataSet$data.norm));
      if(length(shared.nms) < 10){
        msgSet$current.msg <- paste(sel.nms[i], "has less than 10 common genes/probes from previous data sets");
        saveSet(msgSet, "msgSet");                      
        return(0);
      }
      
      # check gene id type
      if(dataSet$id.type != id.type){
        msgSet$current.msg <- paste(sel.nms[i], "has different gene/probe ID from", sel.nms[1]);
        return(0);
      }
    }
        
    print("Passed exp condition check!");
    
    # now construct a common matrix to faciliated plotting across all studies
    dataName <- sel.nms[1];
    dataSet <- readDataset(dataName);
    common.matrix <- dataSet$data.norm[as.character(shared.nms), ];
    nms.vec <- rownames(dataSet$data.norm);
    smps.vec <- colnames(dataSet$data.norm);
    data.lbl <- rep(dataName, ncol(common.matrix));
    cls.lbl <- dataSet$cls;
    
    for(i in 2:length(sel.nms)){
      dataName <- sel.nms[i];
      dataSet <- readDataset(dataName);
      ndat <- dataSet$data.norm[as.character(shared.nms), ];
      nms.vec <- c(nms.vec, rownames(dataSet$data.norm));
      smps.vec <- c(smps.vec, colnames(dataSet$data.norm));
      plot.ndat <- t(scale(t(ndat)));
      common.matrix <- cbind(common.matrix, ndat);
      data.lbl <- c(data.lbl, rep(dataName, ncol(dataSet$data.norm[,])));
      cls.lbl <- c(cls.lbl, dataSet$cls);
    }
    cls.lbl <- factor(cls.lbl);
    levels(cls.lbl) <- lvls;
    
    smps.nms <- colnames(common.matrix)
    if(length(unique(smps.nms)) != length(smps.nms)){
      data.nb <- length(unique(data.lbl));
      data.vec <- vector()
      for(i in 1:data.nb){
        data.vec[i] <- paste0("d", i);
      }
      levels(data.lbl) <- data.vec;
      colnames(common.matrix) <- make.unique(paste(data.vec, smps.nms, sep="_"));
      
      dataSet <- readDataset(sel.nms[1]);
      colnames(dataSet$data.norm) <- paste("d1", colnames(dataSet$data.norm), sep="_");
      RegisterData(dataSet);
      
      for(i in 2:length(sel.nms)){
        dataSet <- readDataset(sel.nms[i]);
        colnames(dataSet$data.norm) <- paste0("d",i,"_",colnames(dataSet$data.norm));
        # check if class label is consistent
        RegisterData(dataSet);
      }
      smps.vec <- smps.nms;
      msgSet$current.msg <- paste("Duplicated sample names detected, samples have been renamed to make them unique.");
    }
    
    # note: index by entrez, gene symbol DO have duplicates
    rownames(common.matrix) <- shared.nms;
    
    # resort data, first on data.lbl, then on class lbl
    ord.inx <- order(data.lbl, cls.lbl);
    common.matrix <- data.matrix(common.matrix[,ord.inx]);
    cls.lbl <- cls.lbl[ord.inx];
    data.lbl <- data.lbl[ord.inx];
    smps.vec <- smps.vec[ord.inx];
    nms.vec <- unique(nms.vec)

  }
  paramSet$nms.vec <- nms.vec;
  paramSet$smps.vec <- smps.vec;

  if(ncol(common.matrix) > 1000){  # max sample number allow 1000
    msgSet$current.msg <- paste("Total combined sample #:", ncol(common.matrix), "(exceed the limit: 1000!)");
    saveSet(msgSet, "msgSet");
    return(0);
  }
  
  # save the meta-dataset
  res <- data.frame(colnames(common.matrix), cls.lbl, data.lbl, t(common.matrix));
  colnames(res) <- c('#NAME', '#CLASS.condition', paste('#CLASS.dataset',id.type, paramSet$data.org, sep="."), rownames(common.matrix));
  write.table(t(res), sep="\t", file="ExpressAnalyst_merged_data.txt", col.names=F, quote=FALSE);
  
  # need to set up the data for plotting (boxplot, heatmpa) so 
  # we need to scale row for each dataset in order to elimiate the maganitude difference 
  plot.matrix <- matrix(NA, nrow=nrow(common.matrix), ncol=ncol(common.matrix));
  rownames(plot.matrix) <- rownames(common.matrix);
  colnames(plot.matrix) <- colnames(common.matrix);
  for(i in 1:length(sel.nms)){
    data.inx <- data.lbl == sel.nms[i];
    plot.matrix[,data.inx] <- t(scale(t(common.matrix[,data.inx])));
  }
  
  # if entrez, get symbols for display
  shared.nms <- rownames(common.matrix);
  if(id.type == "entrez"){ 
    symbols <- doEntrez2SymbolMapping(shared.nms, paramSet$data.org, paramSet$data.idType);
  }else{ # display itself
    symbols <- shared.nms;
  }
  names(symbols) <- shared.nms;
  
  common.matrix[!is.finite(common.matrix)] <- NA;
  common.matrix <- na.omit(common.matrix);

  inmex.meta <- list(data=common.matrix,
                     plot.data=plot.matrix,
                     id.type = id.type,
                     gene.symbls = symbols,
                     cls.lbl=factor(cls.lbl),
                     data.lbl=data.lbl);
  
  qs::qsave(inmex.meta, "inmex_meta.qs");
  paramSet$smps.vec <- colnames(common.matrix);
  # setup common stats gene number, smpl number, grp info
  msgSet$current.msg <- paste("Sample #:", ncol(inmex.meta$data),
                        "Common ID #:", nrow(inmex.meta$data), 
                        "Condition:", paste(levels(inmex.meta$cls.lbl), collapse=" vs. "));

  saveSet(msgSet, "msgSet");
  saveSet(paramSet, "paramSet");

  return(1);
}

PerformMetaDeAnal <- function(paramSet){ 
  msgSet <- readSet(msgSet, "msgSet");
  nms.vec <- paramSet$nms.vec;
  mdata.all <- paramSet$mdata.all;

  inmex.meta <- qs::qread("inmex_meta.qs");   
  data.lbl <- inmex.meta$data.lbl
  allmat <- matrix("NA", nrow=length(nms.vec), ncol=length(data.lbl))
  rownames(allmat) <- nms.vec
  colnames(allmat) <- colnames(inmex.meta$data)
  
  include.inx <- mdata.all==1;
  if(sum(include.inx) == 0){
    msgSet$current.msg <-"No dataset is selected for analysis!";
    saveSet(msgSet, "msgSet");
    print(msgSet$current.msg);
    return(0);
  }
  analSet <- .performEachDEAnal(paramSet$meta.upload);
  paramSet$performedDE <- TRUE;
  saveSet(paramSet, "paramSet");
  return(analSet);
}

.performEachDEAnal <- function(is.meta=F){
  inmex.ind <- list();
  inmex.meta <- qs::qread("inmex_meta.qs");
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  
  mdata.all <- paramSet$mdata.all;
  sel.nms <- names(mdata.all)[mdata.all==1];
  if(is.meta){
    for(i in 1:length(sel.nms)){
      dataSet= list()
      dataName <- sel.nms[i];
      sel.inx <- inmex.meta$data.lbl == dataName;
      group <- factor(inmex.meta$cls.lbl[sel.inx]); # note regenerate factor to drop levels 
      data <- inmex.meta$data[, sel.inx];
      
      # update data set
      dataSet$type <- "array";
      dataSet$name <- dataName;
      group <- factor(inmex.meta$cls.lbl[sel.inx]); # note regenerate factor to drop levels 
      dataSet$cls <- group;
      #res.limma <- PerformLimma(data, group);
      
      # save the limma fit object for meta-analysis (such as "dataSet1.fit.obj")
      qs::qsave(dataSet$fit.obj, file=paste(dataName, "fit.obj", sep="."));
      
      res.all <- GetLimmaResTable(dataSet$fit.obj);
      qs::qsave(res.all, "meta.resTable.qs");
      res.mat <- cbind(logFC=res.all$logFC, Pval = res.all$adj.P.Val);
      
      rownames(res.mat) <- rownames(res.all);
      res.mat <- res.mat[match(rownames(data),rownames(res.mat)),]

      inmex.ind[[dataName]] <- res.mat;
      
      #register sig one
      sig.inx <- res.mat[,2]<=paramSet$BHth;
      #sig.inx <- res.mat[,2]<=dataSet$pval;
      #dataSet$sig.mat <- res.mat[sig.inx,];
      RegisterData(dataSet);
      
      # clean up
      rm(dataSet, res.all);
      gc();
    }
  }else{
    for(i in 1:length(sel.nms)){
      dataName <- sel.nms[i];
      sel.inx <- inmex.meta$data.lbl == dataName;
      group <- factor(inmex.meta$cls.lbl[sel.inx]); # note regenerate factor to drop levels 
      data <- inmex.meta$data[, sel.inx];
      
      dataSet <- readDataset(dataName);

      grp.lvl <- levels(dataSet$cls);
      
      # update data set
      dataSet$type <- "array";
      group <- factor(inmex.meta$cls.lbl[sel.inx], levels=grp.lvl, ordered=T); # note regenerate factor to drop levels 
      dataSet$cls <- group;
      
      #if(exists('dataSet$comp.res')){
      #  res.all <- dataSet$comp.res
      #}else{
      # save dataSet object for meta-analysis
      #res.limma <- PerformLimma(data, group);
      qs::qsave(dataSet$fit.obj, file=paste(dataName, "fit.obj", sep=".")); 
      res.all <- GetLimmaResTable(dataSet$fit.obj);
      #}
      qs::qsave(res.all, "meta.resTable.qs");
      
      res.mat <- cbind(logFC=res.all$logFC, Pval = res.all$adj.P.Val);
      
      rownames(res.mat) <- rownames(res.all);
      #get the features that are shared across datasets
      res.mat <- res.mat[match(rownames(data),rownames(res.mat)),]

      inmex.ind[[dataName]] <- res.mat;
      
      #sig.inx <- res.mat[,2]<=paramSet$BHth;
      sig.inx <- res.mat[,2]<=dataSet$pval;
      # dataSet$sig.mat <- res.mat[sig.inx,];
      
      RegisterData(dataSet);
      # clean up
      rm(dataSet, res.all);
      gc();
    }
  }
  analSet$inmex.ind <- inmex.ind;
  #calculate average log fc
  
  aggr <- data.frame()
  inmex.ind.ordered <- lapply(inmex.ind, function(x){
    print(dim(x))
    x<-x[order(rownames(x)),]
  })
  
  for (i in 1:length(inmex.ind.ordered)){
    if(i == 1){
      aggr <- inmex.ind.ordered[[i]]
    }else{
      aggr <- aggr + inmex.ind.ordered[[i]];
    }
  }
  aggr <- aggr/length(inmex.ind.ordered);
  analSet$meta.avgFC <- aggr[,1];
  return(analSet);
}


#'Perform combine effect size meta-analysis method
#'@param method Method used, either "rem" or "fem"
#'@param BHth P-value threshold
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PerformMetaEffectSize<- function(method="rem", BHth=0.05){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  mdata.all <- paramSet$mdata.all;

  if(!paramSet$performedDE){
    analSet <- PerformMetaDeAnal(paramSet);
    paramSet <- readSet(paramSet, "paramSet");
  }

  inmex.meta <- qs::qread("inmex_meta.qs");
  paramSet$inmex.method <- "effectsize";
  analSet$meta.mat <- meta.stat <<- NULL;
  
  sel.nms <- names(mdata.all)[mdata.all==1];
  nbstudies <- length(sel.nms);
  listgd<-vector("list", (nbstudies+3));
  ES<-array(dim=c(nrow(inmex.meta$data),4,nbstudies));
  cls.lvls <- levels(as.factor(inmex.meta$cls.lbl));
  
  for (i in 1:nbstudies){
    data.nm <- sel.nms[i];
    dataSet <- readDataset(data.nm);
    
    fit.obj.nm <- paste(data.nm, "fit.obj", sep=".");
    fit2i <- qs::qread(fit.obj.nm);
    
    pvals <- p.adjust(fit2i$p.value,method="BH");
    listgd[[i]]=which(pvals<=BHth);
    
    n1i=length(which(dataSet$cls==cls.lvls[1]));
    n2i=length(which(dataSet$cls==cls.lvls[2]));
    ES[,,i]=effectsize(fit2i$t,((n1i*n2i)/(n1i+n2i)),(fit2i$df.prior+fit2i$df.residual));
  }
  
  #only unbiased; for biased effect sizes, add ES[,1,],ES[,2,]
  listgd[[(nbstudies+1)]]=unique(unlist(listgd[1:nbstudies]))
  restempdirect=combineES(ES[,3,],ES[,4,],BHth, method);
  
  pooled.ef <- restempdirect[[3]];
  wt.mat<- restempdirect[[4]]; # one column for each study
  
  listgd[[(nbstudies+2)]]=restempdirect$DEindices
  listgd[[(nbstudies+3)]]=restempdirect$TestStatistic
  names(listgd)=c(paste("study",1:nbstudies,sep=""),"AllIndStudies","Meta","TestStatistic")  
  
  es.mat <- matrix(0, nrow=nrow(inmex.meta$data), ncol=2);
  es.mat[,1] <- pooled.ef;
  es.mat[,2] <- p.adjust(2*(1-pnorm(abs(listgd[[(nbstudies+3)]]))), method="BH");
  
  #remove infinite
  vec <- es.mat[,2];
  vec[is.infinite(vec)] <- max(vec[-is.infinite(vec)]);
  es.mat[,2] <- vec;

  vec <- es.mat[,1];
  vec[is.infinite(vec)] <- max(vec[-is.infinite(vec)]);
  es.mat[,1] <- vec;

  colnames(es.mat) <- c("CombinedES","Pval");
  rownames(es.mat) <- rownames(inmex.meta$data);
  #allMeta.mat <<- es.mat;
  qs::qsave(es.mat, "allMeta.mat.qs");
  
  # now keep only genes with at least on sig (in one study or meta analysis)
  inx <- union(listgd[[(nbstudies+1)]], listgd[[(nbstudies+2)]]);
  es.mat <- es.mat[inx,];
  
  #sort
  ord.inx <- order(abs(es.mat[, "Pval"]), decreasing = F);
  es.mat<-signif(es.mat[ord.inx,],5);
  
  sig.inx <- which(es.mat[,"Pval"]<=BHth);
  analSet$meta.mat <- es.mat[sig.inx, ];
  analSet$meta.mat.all <- es.mat;

  res <- SetupMetaStats(BHth, paramSet, analSet);
  saveSet(res[[1]], "paramSet");
  return(saveSet(res[[2]], "analSet", length(sig.inx)));
}

#'Perform combine p-value meta-analysis
#'@param method Method used, either "stouffer" or "fisher"
#'@param BHth P-value threshold
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PerformPvalCombination <- function(method="stouffer", BHth=0.05){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  analSet <- readSet(analSet, "analSet");

  if(!paramSet$performedDE){
    analSet <- PerformMetaDeAnal(paramSet);
    paramSet <- readSet(paramSet, "paramSet");
  }

  inmex.meta <- qs::qread("inmex_meta.qs");
  paramSet$inmex.method <- "metap";
  meta.mat <<- meta.stat <<- NULL;
  sel.nms <- names(mdata.all)[mdata.all==1];
  
  classes <- list();
  nbstudies <- length(sel.nms);
  listgd=vector("list", (nbstudies+3));
  
  for (i in 1:nbstudies){
    data.nm <- sel.nms[i];
    dataSet <- readDataset(data.nm);
    classes[[i]] <- dataSet$cls; 
    
    fit.obj.nm <- paste(data.nm, "fit.obj", sep=".");
    fit2i <- qs::qread(fit.obj.nm);
    
    pvals <- p.adjust(fit2i$p.value,method="BH");
    listgd[[i]]=which(pvals<=BHth);
    
    #recalculate moderated one sided p
    p1sidedLimma=pt(fit2i$t,df=(fit2i$df.prior+fit2i$df.residual))
    assign(paste("p1sidedLimma",i,sep=""), p1sidedLimma)
  }
  
  names(classes) <- sel.nms;
  tempvec=paste("p1sidedLimma",1:nbstudies,sep="");
  
  lsinglep=lapply(tempvec,FUN=function(x) get(x,inherits=TRUE));
  nrep=unlist(lapply(classes,FUN=function(x)length(x)));
  listgd[[(nbstudies+1)]]=unique(unlist(listgd[1:nbstudies]));
  
  restempdirect=combinePvals(lsinglep,nrep,BHth,method);
  
  listgd[[(nbstudies+2)]]=restempdirect$DEindices
  listgd[[(nbstudies+3)]]=restempdirect$CombinedP
  names(listgd)=c(paste("study",1:nbstudies,sep=""),"AllIndStudies","Meta","CombinedP"); 
  #remove infinite
  vec <- restempdirect$CombinedP;
  vec[vec == 0] <- min(vec[vec != 0])/2;
  restempdirect$CombinedP <- vec;
  
  vec <- restempdirect$CombinedStat;
  vec[is.infinite(vec)] <- max(vec[!is.infinite(vec)])*2;
  restempdirect$CombinedStat <- vec;
  
  pc.mat <- cbind(CombinedTstat=restempdirect$CombinedStat, CombinedPval=restempdirect$CombinedP);
  rownames(pc.mat) <- rownames(inmex.meta$data);
  qs::qsave(pc.mat, "allMeta.mat.qs");
  
  # now keep only genes with at least on sig (in one study or meta analysis)
  inx <- union(listgd[[(nbstudies+1)]], listgd[[(nbstudies+2)]]);
  pc.mat <- pc.mat[inx,];
  
  #sort
  ord.inx <- order(pc.mat[, "CombinedPval"], decreasing = F);
  pc.mat<-signif(pc.mat[ord.inx,],5);
  
  sig.inx <- which(pc.mat[, "CombinedPval"]<=BHth);
  analSet$meta.mat <- pc.mat[sig.inx, ];
  analSet$meta.mat.all <- pc.mat;
  res <- SetupMetaStats(BHth, paramSet, analSet);
  saveSet(res[[1]], "paramSet");

  return(saveSet(res[[2]], "analSet", length(sig.inx)));
}

#'Perform vote counting meta-analysis method
#'@param method Method used, either "rem" or "fem"
#'@param BHth P-value threshold for the significance level
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PerformVoteCounting <- function(BHth = 0.05, minVote){
  paramSet <- readSet(paramSet, "paramSet");
  mdata.all <- paramSet$mdata.all;
  analSet <- readSet(analSet, "analSet");

  if(!paramSet$performedDE){
    analSet <- PerformMetaDeAnal(paramSet);
    paramSet <- readSet(paramSet, "paramSet");
  }
  inmex.meta <- qs::qread("inmex_meta.qs");
  paramSet$inmex.method <- "votecount";
  DE.vec <<- NULL; # store entrez id from meta-analysis for GO
  meta.mat <<- meta.stat <<- NULL;
  sel.nms <- names(mdata.all)[mdata.all==1];
  # first create a matrix to stall the result
  # row for each feature and col for each dataset uploaded
  vc.mat <- matrix(0, nrow=nrow(inmex.meta$data), ncol=length(sel.nms)+1);
  shared.ids <- rownames(inmex.meta$data);
  for(i in 1:length(analSet$inmex.ind)){
    res.mat <- analSet$inmex.ind[[i]];
    res.mat <- res.mat[shared.ids, ];
    
    #note in meta-analysis should consider directions
    # use logFC for this purpose 
    # consider upregulated
    hit.up.inx <- res.mat[,1]> 0 & res.mat[,2] <= BHth;
    up.vote <- as.numeric(hit.up.inx);
    
    # consider downregulated
    hit.dn.inx <- res.mat[,1] < 0 & res.mat[,2] <= BHth;
    dn.vote <- -as.numeric(hit.dn.inx);
    
    vc.mat[,i] <- up.vote + dn.vote;
  }
  
  # total score (votes for each direction)
  vc.mat[,length(sel.nms)+1] <- apply(vc.mat, 1, sum);
  colnames(vc.mat) <- c(paste("Vote", substring(sel.nms,0, nchar(sel.nms)-4)), "VoteCounts");
  rownames(vc.mat) <- rownames(inmex.meta$data);
  
  # compute at least one vote (no direction)
  vote.any <- apply(abs(vc.mat), 1, sum)
  vote.any.inx <- vote.any > 0;
  
  # return results with at least one vote
  vc.mat <- vc.mat[vote.any.inx, ];
  
  #sort
  ord.inx <- order(abs(vc.mat[, "VoteCounts"]), decreasing = T);
  vc.mat <- vc.mat[ord.inx, "VoteCounts", drop=F];
  qs::qsave(vc.mat, "allMeta.mat.qs");

  sig.inx <- unname(abs(vc.mat[,"VoteCounts"]) >= minVote); 
  df <- data.frame(vc.mat[sig.inx, ]);
  colnames(df) <- c("VoteCounts");
  analSet$meta.mat <- df;
  analSet$meta.mat.all <- vc.mat;
  saveSet(paramSet, "paramSet");

  res <- SetupMetaStats(BHth, paramSet, analSet);
  saveSet(res[[1]], "paramSet");

  return(saveSet(res[[2]], "analSet", length(sig.inx)));
}


#'Perform direct merging meta-analysis
#'@description Directly merge all datasets and analyze as single data
#'@param method Method used, either "rem" or "fem"
#'@param BHth P-value threshold for the significance level
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
PerformMetaMerge<-function(BHth=0.05){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  paramSet$inmex.method <- "merge";
  meta.mat <<- meta.stat <<- NULL;
  inmex.meta <- qs::qread("inmex_meta.qs");
  # prepare for meta-stats
  # calculate sig genes for individual analysis
  shared.names <- rownames(inmex.meta$data);
  
  res.limma <- PerformLimma(inmex.meta$data, as.factor(inmex.meta$cls.lbl));
  res.all <- GetLimmaResTable(res.limma$fit.obj);
  
  ord.inx <- order(res.all$adj.P.Val, decreasing=F);
  dm.mat <- as.matrix(res.all[ord.inx,c("logFC", "adj.P.Val")]);
  qs::qsave(dm.mat, "allMeta.mat.qs");
  colnames(dm.mat) <- c("CombinedLogFC", "Pval");
  
  sig.inx <- which(dm.mat[,"Pval"] <= BHth);
  analSet$meta.mat <- dm.mat[sig.inx,];
  analSet$meta.mat.all <- dm.mat;
  #saveSet(analSet, "analSet");
  saveSet(paramSet, "paramSet");

  res <- SetupMetaStats(BHth, paramSet, analSet);
  saveSet(res[[1]], "paramSet");

  return(saveSet(res[[2]], "analSet", length(sig.inx)));
}

GetMetaGeneIDType<-function(){
  inmex.meta <- qs::qread("inmex_meta.qs");
  return(inmex.meta$id.type);
}

GetMetaResultGeneIDs<-function(){
  analSet <- readSet(analSet, "analSet");

  rnms <- rownames(as.matrix(analSet$meta.mat.all));# already sorted based on meta-p values
  if(length(rnms) > 1000){
    rnms <- rnms[1:1000];
  }
  return(rnms);
}

# note, due to limitation of get/post
# maximum gene symb for list is top 500
GetMetaResultGeneSymbols<-function(){
  analSet <- readSet(analSet, "analSet");

  ids <- rownames(as.matrix(analSet$meta.mat.all));
  if(length(ids) > 1000){
    ids <- ids[1:1000];
  }
  inmex.meta <- qs::qread("inmex_meta.qs");
  if(inmex.meta$id.type == "entrez"){ # row name gene symbols
    ids <- inmex.meta$gene.symbls[ids];
  }
  return(ids);
}

GetMetaResultPathSymbols<-function(){
  analSet <- readSet(analSet, "analSet");
  meta.mat.all <- analSet$meta.mat.all;
  return(rownames(meta.mat.all));
}

GetMetaResultGeneIDLinks <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  ids <- rownames(as.matrix(analSet$meta.mat.all));
  if(length(ids) > 1000){
    ids <- ids[1:1000];
  }
  #inmex.meta <- qs::qread("inmex_meta.qs");
  #symbs <- inmex.meta$gene.symbls[ids];
  # set up links to genbank
  if(paramSet$data.org == "ko"){
    #annots <- paste("<a href='https://www.genome.jp/dbget-bin/www_bget?", ids, "' target='_blank'>", symbs, "</a>", sep=""); 
    annots <- paste("<a href='https://www.genome.jp/dbget-bin/www_bget?", ids, "' target='_blank'>KEGG</a>", sep="");  
  }else{
    #annots <- paste("<a href='http://www.ncbi.nlm.nih.gov/gene?term=", ids, "' target='_blank'>", symbs, "</a>", sep="");
    annots <- paste("<a href='http://www.ncbi.nlm.nih.gov/gene?term=", ids, "' target='_blank'>NCBI</a>", sep="");
  }
  return(annots);
}

GetMetaResultColNames<-function(){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");

  mdata.all <- paramSet$mdata.all;
  sel.nms <- names(mdata.all)[mdata.all==1];

  # note, max 9 data columns can be displayed
  if(length(sel.nms) + ncol(analSet$meta.mat.all) > 9){
    max.col <- 9 - ncol(analSet$meta.mat.all);
    sel.nms <- sel.nms[1:max.col];
  }

  c(substring(sel.nms, 0, nchar(sel.nms)-4), colnames(analSet$meta.mat));
}

# single.type return logFC or p value for individual data analysis
GetMetaResultMatrix<-function(single.type="fc"){

  analSet <- readSet(analSet, "analSet");

  if(single.type == "fc"){
    dat.mat <- analSet$fc.mat;
  }else{
    dat.mat <- analSet$pval.mat;
  }

  # note, max 9 data columns can be displayed
  if(ncol(dat.mat) + ncol(analSet$meta.mat.all) > 9){
    max.col <- 9 - ncol(analSet$meta.mat.all);
    dat.mat <- dat.mat[,1:max.col];
  }
  meta.mat2 <- cbind(dat.mat, analSet$meta.mat.all);

  # display at most 1000 genes
  if(nrow(meta.mat2) > 1000){
    meta.mat2 <- meta.mat2[1:1000,]; # already sorted based on meta-p values
  }

  meta.mat2 <-signif(as.matrix(meta.mat2), 5);
  meta.mat2;
}

GetMetaStat<-function(){
  analSet <- readSet(analSet, "analSet");
  return (analSet$meta.stat$stat);
}

GetMetaStatNames<-function(){
  analSet <- readSet(analSet, "analSet");
  return (names(analSet$meta.stat$stat));
}

combinePvals <- function(pvalonesided,nrep,BHth=0.05, method) {
  listres <- vector("list",3);
  nbstudies <- length(pvalonesided);
  nbreptot <- sum(nrep);
  if (nbreptot <2) {
    stop("Error: the argument \"nrep\" must be a vector with at least two values higher than 1")
  } 
  
  weight <- sqrt(nrep/nbreptot);
  fstatmeta <- function(g){
    vecptime=unlist(lapply(pvalonesided, FUN = function(x) x[g]));
    vec <- qnorm(1 - vecptime);
    stattestg <- sum(weight[1:length(pvalonesided)] * vec[1:length(pvalonesided)], na.rm = TRUE);
    stattestg;
  }
  
  fishersum <- function(pvec){
    return(sum(-2*log(pvec)))
  }
  
  if(method=="stouffer"){
    statpvalc=-unlist(lapply(rep(1:length(as.vector(pvalonesided[[1]])), 1), function(x) fstatmeta(x)));
    rpvalpvalc=2*(1-pnorm(abs(statpvalc)));
  }else{ # fisher
    data <- data.frame(pvalonesided);
    #data[data == 0] <- 1*10^-10;
    
    #note, p value are calculated for one side
    # pt (lower.tail=T by default) which tests if group A < group B
    # for one side
    fsum1 <- apply(data, 1, fishersum);
    rpvalpvalc1 <- 1-pchisq(fsum1, df=(ncol(data)*2));
    
    # for the other side
    data <- 1-data;
    fsum2 <- apply(data, 1, fishersum);
    rpvalpvalc2 <- 1-pchisq(fsum2, df=(ncol(data)*2));
    
    # report the min of two direction calculation
    rpvalpvalc <- pmin(rpvalpvalc1, rpvalpvalc2);
    
    # adding direction information
    statpvalc <- pmax(fsum1, fsum2);
    # if A<B sig, then it should be negative 
    statpvalc[statpvalc == fsum1]<- -statpvalc[statpvalc == fsum1];
  }
  
  rpvalpvalc <- p.adjust(rpvalpvalc,method="BH");
  res <- which(rpvalpvalc<=BHth);
  listres[[1]] <- res
  listres[[2]] <- statpvalc;
  listres[[3]] <- rpvalpvalc
  names(listres) <- c("DEindices", "CombinedStat", "CombinedP")
  listres
}

#combine effect size
combineES <- function (ES, varES, BHth = 0.05, method){
  if(method == "rem"){
    useREM <- TRUE;
  }else{
    useREM <- FALSE;
  }
  
  num.studies <- dim(ES)[2];
  
  Qvals <- f.Q.NA(ES, varES)
  if (useREM) {
    varES <- varES + tau2.NA(Qvals, num.studies, my.weights = 1/varES)
  }
  wt <- 1/varES
  MUvals <- rowSums(ES * wt, na.rm = TRUE)/rowSums(wt, na.rm = TRUE)
  MUsES <- sqrt(abs(1/rowSums(wt, na.rm = TRUE)))
  zSco <- MUvals/MUsES
  rpvalESc <- 2 * (1 - pnorm(abs(zSco)))
  res <- which(p.adjust(rpvalESc, method = "BH") <= BHth);
  listres <- list();
  listres[[1]] <- res
  listres[[2]] <- zSco
  listres[[3]] <- MUvals; # pool effect size
  listres[[4]] <- wt; # wt for each studies, it is matrix with one column for each studies
  names(listres) <- c("DEindices", "TestStatistic", "PooledEffectSize", "Weights")
  listres
}

# first two biased ES, var, last two unbiased

f.Q.NA <- function(dadj, varadj) {
  w <- 1/varadj
  tmp1 <- w * dadj
  mu <- rowSums(tmp1, na.rm = TRUE)/rowSums(w, na.rm = TRUE)
  Q <- rowSums(w * (dadj - mu)^2, na.rm = TRUE)
}

tau2.NA <- function(Q, num.studies, my.weights) {
  vwts <- rowSums(my.weights, na.rm = TRUE)
  tmp2 <- rowSums(my.weights^2, na.rm = TRUE)
  tau2 <- pmax(0, (Q - (num.studies - 1))/(vwts - tmp2/vwts))
  return(tau2)
}

##################################
# functions for estimating Cochran’s Q
##################################


#computes Cochran’s Q gene by gene
#dadj and varadj must be matrices, in which every study is a column,
#every row a gene
f.Q <- function(dadj, varadj){
  w<-1/varadj
  tmp1<-w*dadj
  mu<-rowSums(tmp1)/rowSums(w)
  Q<-rowSums(w*(dadj - mu)^2)
}

PlotMetaDensity<- function(imgNm, dpi=72, format, factor){
  require("ggplot2")
  inmex.meta <- qs::qread("inmex_meta.qs");
  dat <- inmex.meta$data;
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  dpi <- as.numeric(dpi);
  
  df <- data.frame(inmex.meta$data, stringsAsFactors = FALSE);
  df <- stack(df);

  Factor <- inmex.meta$data.lbl;
  
  conv <- data.frame(ind=colnames(inmex.meta$data), class=Factor);
  conv$ind <- gsub("-", ".", conv$ind);
  df1 <- merge(df, conv, by="ind");
  Cairo(file=imgNm, width=10, height=6, type=format, bg="white", dpi=dpi, unit="in");
  g =ggplot(df1, aes(x=values)) + 
        geom_line(aes(color=class, group=ind), stat="density", alpha=0.3) + 
        geom_line(aes(color=class), stat="density", alpha=0.6, size=1.5) +
        theme_bw()
  print(g);
  dev.off();
}

effectsize <- function(tstat,ntilde,m){
  cm=gamma(m/2)/(sqrt(m/2)*gamma((m-1)/2))
  d=tstat/sqrt(ntilde)
  dprime=cm*d
  terme1=m/((m-2)*ntilde)
  vard=terme1+d^2*(terme1*ntilde-1/cm^2)
  vardprime=cm^2*(terme1+dprime^2*(terme1*ntilde-1/cm^2))
  result=cbind(d,vard,dprime,vardprime)
  colnames(result)=c("d","vard","dprime","vardprime")
  result
}

CalculateGsNet <- function(name, netNm, type, mType, db){
  res <- PerformMetaGseaNet(name, netNm, "pval", db, mType,0.05)
  return(1)
}

setIncludeMeta <- function(metaBool){
    paramSet <- readSet(paramSet, "paramSet");
    paramSet$meta.selected <- metaBool;
    saveSet(paramSet, "paramSet");
}

DoMetaSigUpdate <- function(BHth=0.05, voteCount=2){
    paramSet <- readSet(paramSet, "paramSet");
    analSet <- readSet(analSet, "analSet");
    meta.mat.all <- analSet$meta.mat.all;
    if("VoteCounts" %in% colnames(meta.mat.all)){
    sig.inx <- which(meta.mat.all[,ncol(meta.mat.all)] >= BHth);
    }else{
    sig.inx <- which(meta.mat.all[,ncol(meta.mat.all)] <= BHth);
    }
    analSet$meta.mat <- meta.mat.all[sig.inx,];
    res <- SetupMetaStats(BHth, paramSet, analSet);
    saveSet(res[[1]], "paramSet");
    saveSet(analSet, "analSet");

    return(nrow(analSet$meta.mat));
}