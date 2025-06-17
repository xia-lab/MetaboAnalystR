#'Check if data are ready for meta-analysis
#'@description This function determines if all annotated data are ready for meta-analysis
#'@param mSetObj Input name of the created mSet Object
#'@param combat Adjust for batch effects, logical variable: TRUE = adjust for batch effects using an empirical Bayes framework (R package sva),
#'FALSE = no batch effect adjustment.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export
CheckMetaDataConsistency <- function(mSetObj=NA, combat=TRUE) {
  
  #save.image("consist.RData");
  mSetObj <- .get.mSet(mSetObj);
  mdata.all  <- mSetObj$mdata.all;
  msg <- c()  # Initialize the message variable
  
  msg <- c(msg, "Starting the meta-analysis process...")
  
  if (length(mdata.all) == 0) {
    AddErrMsg("Please upload your data or try our example datasets!")
    return(0)  # Early return for error
  }
  
  include.inx <- mdata.all == 1
  if (sum(include.inx) < 2) {
    AddErrMsg("At least two datasets are required for meta-analysis!")
    return(0)  # Early return for error
  }
  
  sel.nms <- names(mdata.all)[include.inx]
  msg <- c(msg, paste("Number of datasets selected for analysis:", length(sel.nms)))
  
  # Check for consistent class labels
  msg <- c(msg, "Checking group labels for consistency across datasets...")
  dataSet <- qs::qread(sel.nms[1])
  lvls <- levels(dataSet$cls)
  id.type <- dataSet$id.type
  shared.nms <- colnames(dataSet$data)
  
  for (i in 2:length(sel.nms)) {
    dataSet <- qs::qread(sel.nms[i])
    
    # Check if class labels are consistent
    if (!all(levels(dataSet$cls) == lvls)) {
      AddErrMsg(paste(sel.nms[i], "has different group labels", 
                      paste(levels(dataSet$cls), collapse=":"), 
                      "from", sel.nms[1], 
                      paste(lvls, collapse=":")))
      return(0)  # Early return for error
    }
    
    # Check for shared features
    #msg <- c(msg, paste("Identifying shared features among the datasets..."))
    shared.nms <- intersect(shared.nms, colnames(dataSet$data))
    if (length(shared.nms) < ncol(dataSet$data) / 4) {
      AddErrMsg(paste(sel.nms[i], "has less than 25% common features from the previous datasets"))
      return(0)  # Early return for error
    }
  }
  
  AddMsg("Passed experimental condition check!")
  msg <- c(msg, "Passed experimental condition check!")
  
  msg <- c(msg, paste("Constructing the common matrix using", length(shared.nms), "shared features across datasets."))
  # Construct a common matrix
  common.matrix <- dataSet$data[, shared.nms]
  data.lbl <- rep(sel.nms[1], nrow(common.matrix))
  cls.lbl <- dataSet$cls
  
  for (i in 2:length(sel.nms)) {
    dataSet <- qs::qread(sel.nms[i])
    ndat <- dataSet$data[, shared.nms]
    rownames(ndat) <- paste(rownames(ndat), "_", i, sep="")
    common.matrix <- rbind(common.matrix, ndat)
    data.lbl <- c(data.lbl, rep(sel.nms[i], nrow(dataSet$data[,])))
    cls.lbl <- c(cls.lbl, dataSet$cls)
  }
  
  AddMsg("Constructed the common matrix!")
  msg <- c(msg, "Constructed the common matrix!")
  
  if (nrow(common.matrix) > 5000) {
    AddErrMsg(paste("Total combined sample #:", nrow(common.matrix), "(exceed the limit: 5000!)"))
    return(0)  # Early return for error
  }
  
  common.matrix <- t(common.matrix)
  
  
  if (combat) {
    msg <- c(msg, "Applying ComBat adjustment for batch effect correction...")
    pheno <- data.frame(cls.lbl, data.lbl)
    modcombat <- model.matrix(~1, data=pheno)
    batch <- data.lbl
    combat_edata <- sva::ComBat(dat=common.matrix, batch=batch, mod=modcombat, par.prior=TRUE, prior.plots=FALSE)
    common.matrix <- combat_edata
    write.table(common.matrix, file="combatadjusted.csv", col.names=T, quote=FALSE);
  }
  
  # save the meta-dataset
  res <- data.frame(colnames(common.matrix), cls.lbl, data.lbl, t(common.matrix));
  colnames(res) <- c('Samples', 'Conditions', 'Datasets', rownames(common.matrix));
  write.table(t(res), file="MetaboAnalyst_merged_data.csv", col.names=F, quote=FALSE);
  
  # need to set up the data for plotting (boxplot, heatmap) so 
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
  symbols <- shared.nms;
  names(symbols) <- shared.nms;
  
  metastat.meta <<- list(data=common.matrix,
                         plot.data=plot.matrix,
                         gene.symbls = symbols,
                         cls.lbl=factor(cls.lbl),
                         data.lbl=data.lbl);
  qs::qsave(metastat.meta, "metastat.meta.qs");
  PerformEachDEAnal(mSetObj);
  
  # setup common stats gene number, smpl number, grp info
  studyinfo <- paste("Sample #:", ncol(metastat.meta$data), "Common ID #:", nrow(metastat.meta$data), "Condition:", paste(levels(metastat.meta$cls.lbl), collapse=" vs. "));
  
  
  
  msg <- c(msg, "Generating and saving the merged dataset file: MetaboAnalyst_merged_data.csv")
  studyinfo <- paste("Sample #:", ncol(common.matrix), 
                     "Common ID #:", nrow(common.matrix), 
                     "Condition:", paste(levels(factor(cls.lbl)), collapse=" vs. "))
  AddMsg(studyinfo)
  msg <- c(msg, studyinfo)
  
  
  group.nms <- paste(levels(factor(cls.lbl)), collapse="; ")
  ds.names <- paste(sel.nms, collapse="; ")
  summary.txt <- paste0(
    "<ul>",
    "<li><b>Data type:</b> Statistical meta-analysis</li>",
    "<li><b>Matched feature number:</b> ", nrow(common.matrix), "</li>",
    "<li><b>Total number of samples:</b> ", ncol(common.matrix), "</li>",
    "<li><b>Group names:</b> ", group.nms, "</li>",
    "<li><b>Individual datasets:</b> ", ds.names, "</li>",
    "</ul>"
  )
  mSetObj$dataSet$summary.text <- summary.txt
  
  
  mSetObj$msgSet$check.msg <- msg;
  if (.on.public.web) {
    if (length(sel.nms) == 1) {
      .set.mSet(mSetObj)
      return(2);
    } else {
      .set.mSet(mSetObj)
      return(1);
    }
  } else {
    return(.set.mSet(mSetObj));
  }
}

GetMetaProcMsg <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    return(mSetObj$dataSet$summary.text);
}



#'Performs differential expression analysis on individual data
#'@description This function performs DE analysis on individual data using the common matrix, which
#'will be used/compared in later steps of the analysis (according to the p-value). The DE for each feature
#'may be adjusted using the p-value.  
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

PerformEachDEAnal <- function(mSetObj=NA){
  
  metastat.ind <- list();
  mdata.all <- mSetObj$mdata.all
  sel.nms <- names(mdata.all)[mdata.all==1];
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
}
  for(i in 1:length(sel.nms)){
    dataName <- sel.nms[i];
    sel.inx <- metastat.meta$data.lbl == dataName;
    group <- factor(metastat.meta$cls.lbl[sel.inx]); # note regenerate factor to drop levels 
    data <- metastat.meta$data[, sel.inx];
    
    dataSet <- qs::qread(dataName);
    grp.lvl <- levels(dataSet$cls);
    
    # update data set
    group <- factor(metastat.meta$cls.lbl[sel.inx], levels=grp.lvl, ordered=T); # note regenerate factor to drop levels 
    dataSet$cls <- group;
    
    res.limma <- PerformLimma(data, group);
    
    # save dataSet object for meta-analysis
    dataSet$fit.obj <- res.limma$fit.obj;
    
    res.all <- GetLimmaResTable(res.limma$fit.obj);
    res.mat <- cbind(logFC=res.all$logFC, Pval = res.all$adj.P.Val);
    rownames(res.mat) <- rownames(res.all);
    metastat.ind[[dataName]] <- res.mat;
    
    RegisterData(mSetObj, dataSet);
    
    # clean up
    rm(dataSet, res.all);
    gc();
  } 
   print(head(metastat.ind));
   print("metastat.ind===");
  metastat.ind <<- metastat.ind;
  qs::qsave(metastat.ind, "metastat.ind.qs");

}

#'Meta-Analysis Method: Combining p-values
#'@description This function is one of three methods to perform meta-analysis. Here, p-values are combined using either
#'the Fisher's method or the Stouffer's method. 
#'@param mSetObj Input name of the created mSet Object.
#'@param method Method of p-value combination. By default it is "stouffer", else it is "fisher". 
#'@param BHth Numeric input to set the significance level. By default it is 0.05. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@import qs
#'@export

PerformPvalCombination <- function(mSetObj=NA, method="stouffer", BHth=0.05){
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
  metastat.ind <<- qs::qread("metastat.ind.qs");
}
  mSetObj <- .get.mSet(mSetObj);
  mdata.all <- mSetObj$mdata.all

  mSetObj$dataSet$pvalmethod <- method
  mSetObj$dataSet$pvalcutoff <- BHth

  metastat.method <<- "metap";
  mSetObj$dataSet$metastat.method <-  metastat.method
  meta.mat <<- meta.stat <<- NULL;
  sel.nms <- names(mdata.all)[mdata.all==1];
  
  classes <- list();
  nbstudies <- length(sel.nms);
  listgd=vector("list", (nbstudies+3));
  
  for (i in 1:nbstudies){
    data.nm <- sel.nms[i];
    dataSet <- qs::qread(data.nm);
    classes[[i]] <- dataSet$cls; 
    
    fit2i <- dataSet$fit.obj;
    pvals <- p.adjust(fit2i$p.value, method="BH");
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
  
  pc.mat <- cbind(CombinedTstat=restempdirect$CombinedStat, CombinedPval=restempdirect$CombinedP);
  rownames(pc.mat) <- rownames(metastat.meta$data);
  
  # now keep only genes with at least on sig (in one study or meta analysis)
  inx <- union(listgd[[(nbstudies+1)]], listgd[[(nbstudies+2)]]);
  pc.mat <- pc.mat[inx,];
  
  #sort
  ord.inx <- order(pc.mat[, "CombinedPval"], decreasing = F);
  pc.mat<-signif(pc.mat[ord.inx,],5);
  sig.inx <- which(pc.mat[, "CombinedPval"]<=BHth);
  
  #meta.mat <<- pc.mat[sig.inx, ];
  meta.mat <<- pc.mat;
  qs::qsave(meta.mat, "meta.mat.qs");
  mSetObj <- SetupMetaStats(mSetObj, BHth);
  mSetObj$analSet$metap.mat <- meta.mat;

  if(.on.public.web){
    .set.mSet(mSetObj)
    return(length(sig.inx));
  }else{
    return(.set.mSet(mSetObj));
  }
} 

#'Meta-Analysis Method: Vote Counting
#'@description This function is one of three methods to perform meta-analysis. Here, significant features are selected based on a selected criteria (i.e. an adjusted p-value
#'<0.05 and the same direction of FC) for each dataset. The votes are then calculated for each feature by counting the total of number of times
#'a feature is significant across all included datasets. However, this method is statistically inefficient and should be considered the
#'last resort in situations where other methods to perform meta-analysis cannot be applied. 
#'@param mSetObj Input name of the created mSet Object.
#'@param BHth Numeric input to set the significance level. By default it is 0.05. 
#'@param minVote Numeric input to set the minimum vote-count. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformVoteCounting <- function(mSetObj=NA, BHth = 0.05, minVote){
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
  metastat.ind <<- qs::qread("metastat.ind.qs");
}
  mSetObj <- .get.mSet(mSetObj);
  mdata.all <- mSetObj$mdata.all

  mSetObj$dataSet$vote <- minVote
  mSetObj$dataSet$pvalcutoff <- BHth
  
  metastat.method <<- "votecount";
  mSetObj$dataSet$metastat.method <-  metastat.method
  DE.vec <<- NULL; # store entrez id from meta-analysis for GO
  meta.mat <<- meta.stat <<- NULL;
  sel.nms <- names(mdata.all)[mdata.all==1];
  
  # first create a matrix to store the result
  # row for each feature and col for each dataset uploaded
  vc.mat <- matrix(0, nrow=nrow(metastat.meta$data), ncol=length(sel.nms)+1);
  shared.ids <- rownames(metastat.meta$data);
  for(i in 1:length(metastat.ind)){
    res.mat <- metastat.ind[[i]];
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
  rownames(vc.mat) <- rownames(metastat.meta$data);
  
  # compute at least one vote (no direction)
  vote.any <- apply(abs(vc.mat), 1, sum)
  vote.any.inx <- vote.any > 0;
  
  # return results with at least one vote
  vc.mat <- vc.mat[vote.any.inx, ];
  
  #sort
  ord.inx <- order(abs(vc.mat[, "VoteCounts"]), decreasing = T);
  vc.mat <- vc.mat[ord.inx, "VoteCounts", drop=F];
  
  sig.inx <- abs(vc.mat[,"VoteCounts"]) >= minVote;
  #meta.mat <<- vc.mat[sig.inx, ,drop=F];
  meta.mat <<- vc.mat;
  qs::qsave(meta.mat, "meta.mat.qs");
  mSetObj <- SetupMetaStats(mSetObj, BHth);
  mSetObj$analSet$votecount.mat <- meta.mat;
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(sum(sig.inx));
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Meta-Analysis Method: Direct merging of datasets
#'@description This function is one of three methods to perform meta-analysis. Direct merging of individual data into 
#'a mega-dataset results in an analysis of that mega-dataset as if the individual data were derived from the same experiment. This 
#'method thereby ignores any inherent bias and heterogeneity between the different data. Because of this, there exists several confounders 
#'such as different experimental protocols, technical platforms, and raw data processing procedures that can mask true underlying differences. 
#'It is therefore highly suggested that this approach be used only when individual data are very similar (i.e. from the same lab, same platform, 
#'without batch effects)."
#'@param mSetObj Input name of the created mSet Object.
#'@param BHth Numeric input to set the significance level. By default it is 0.05. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformMetaMerge<-function(mSetObj=NA, BHth=0.05){
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
  metastat.ind <<- qs::qread("metastat.ind.qs");

}
  mSetObj <- .get.mSet(mSetObj);
  mdata.all <- mSetObj$mdata.all

  mSetObj$dataSet$pvalcutoff <- BHth
  
  metastat.method <<- "merge";
  mSetObj$dataSet$metastat.method <-  metastat.method
  meta.mat <<- meta.stat <<- NULL;
  
  # prepare for meta-stats
  # calculate sig genes for individual analysis
  shared.names <- rownames(metastat.meta$data);
  
  res.limma <- PerformLimma(metastat.meta$data, as.factor(metastat.meta$cls.lbl));
  res.all <- GetLimmaResTable(res.limma$fit.obj);
  
  ord.inx <- order(res.all$adj.P.Val, decreasing=F);
  dm.mat <- as.matrix(res.all[ord.inx,c("logFC", "adj.P.Val")]);
  colnames(dm.mat) <- c("CombinedLogFC", "Pval");
  
  sig.inx <- which(dm.mat[,"Pval"] <= BHth);
  
  #meta.mat <<- dm.mat[sig.inx,];
  meta.mat <<- dm.mat;
  qs::qsave(meta.mat, "meta.mat.qs");

  mSetObj <- SetupMetaStats(mSetObj, BHth);
    mSetObj$analSet$merge.mat <- meta.mat;

  if(.on.public.web){
    .set.mSet(mSetObj)
    return(length(sig.inx));
  }else{
    return(.set.mSet(mSetObj));
  }
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetMetaGeneIDType<-function(){
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
}
  return(metastat.meta$id.type);
}

GetMetaResultGeneIDs<-function(){

if(!exists('meta.mat')){
  meta.mat <<- qs::qread("meta.mat.qs");
}
  return(rownames(meta.mat));
}

# note, due to limitation of get/post
# maximum gene symb for list is top 5000
GetMetaResultGeneSymbols<-function(){

if(!exists('meta.mat')){
  meta.mat <<- qs::qread("meta.mat.qs");
}
  ids <- rownames(meta.mat);
  if(length(ids) > 5000){
    ids <- ids[1:5000];
  }
  return(ids);
}

GetMetaResultGeneIDLinks <- function(){
  ids <- rownames(meta.mat);
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
}
  symbs <- metastat.meta$gene.symbls[ids];
  # set up links to genbank
  annots <- paste("<a href='http://www.ncbi.nlm.nih.gov/gene?term=", ids,
                  "' target='_blank'>", symbs, "</a>", sep="");
  return(annots);
}

GetMetaResultColNames <- function(){

if(!exists('meta.mat')){
  meta.mat <<- qs::qread("meta.mat.qs");
}
  mSetObj <- .get.mSet(mSetObj);
  mdata.all <- mSetObj$mdata.all

  sel.nms <- names(mdata.all)[mdata.all==1];
  c(substring(sel.nms, 0, nchar(sel.nms)-4), colnames(meta.mat));
}



#'Single.type return logFC or p value for individual data analysis
#'@param mSetObj Input name of the created mSet Object
#'@param single.type Default is "fc"
#'@export
GetMetaResultMatrix <- function(mSetObj = NA, single.type="fc"){
  #save.image("metares.RData");
  mSetObj <- .get.mSet(mSetObj);

  if(!exists('meta.mat')){
    meta.mat <<- qs::qread("meta.mat.qs");
  }
  mSetObj <- SetupMetaStats(mSetObj, mSetObj$dataSet$pvalcutoff);
  
  fc.mat <- mSetObj$analSet$fc.mat;
  pval.mat <- mSetObj$analSet$pval.mat;
  metastat.de <- mSetObj$analSet$metastat.de;
  meta.stat <- mSetObj$analSet$metastat;

  colnms <- GetMetaResultColNames();
  if(single.type == "fc"){
    meta.mat <- cbind(fc.mat, meta.mat);
  }else{
    meta.mat <- cbind(pval.mat, meta.mat);
  }
  print(head(meta.mat));
  print("getting metares");
  meta.mat <- signif(as.matrix(meta.mat), 5);
  colnames(meta.mat) <- colnms;
  mSetObj$analSet$meta.mat <- meta.mat;
  
  if(.on.public.web == TRUE){  
    .set.mSet(mSetObj)
    meta.mat;
  }else{
    return(.set.mSet(mSetObj));
  }
}

GetMetaStat <- function(){
  mSetObj <- .get.mSet(mSetObj);
  meta.stat <- mSetObj$analSet$metastat;
  return (meta.stat$stat);
}

GetMetaStatNames <- function(){
  mSetObj <- .get.mSet(mSetObj);
  meta.stat <- mSetObj$analSet$metastat;
  return (names(meta.stat$stat));
}

combinePvals <- function(pvalonesided,nrep,BHth=0.05, method) {
  listres=vector("list",3);
  nbstudies=length(pvalonesided);
  nbreptot=sum(nrep);
  if (nbreptot <2) {
    stop("Error: the argument \"nrep\" must be a vector with at least two values higher than 1")
  } 
  
  weight=sqrt(nrep/nbreptot);
  fstatmeta=function(g){
    vecptime=unlist(lapply(pvalonesided, FUN = function(x) x[g]));
    vec = qnorm(1 - vecptime);
    stattestg = sum(weight[1:length(pvalonesided)] * vec[1:length(pvalonesided)], na.rm = TRUE);
    stattestg;
  }
  
  fishersum <- function(pvec){
    return(sum(-2*log10(pvec)))
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
    rpvalpvalc1 = 1-pchisq(fsum1, df=(ncol(data)*2));
    
    # for the other side
    data <- 1-data;
    fsum2 <- apply(data, 1, fishersum);
    rpvalpvalc2 = 1-pchisq(fsum2, df=(ncol(data)*2));
    
    # report the min of two direction calculation
    rpvalpvalc <- pmin(rpvalpvalc1, rpvalpvalc2);
    
    # adding direction information
    statpvalc <- pmax(fsum1, fsum2);
    # if A<B sig, then it should be negative 
    statpvalc[statpvalc == fsum1]<- -statpvalc[statpvalc == fsum1];
  }
  
  rpvalpvalc <- p.adjust(rpvalpvalc,method="BH");
  res=which(rpvalpvalc<=BHth);
  listres[[1]]=res
  listres[[2]]=statpvalc;
  listres[[3]]=rpvalpvalc
  names(listres)=c("DEindices", "CombinedStat", "CombinedP")
  listres
}

PlotDataProfile<-function(dataName, boxplotName, pcaName, format="png", dpi=default.dpi){
  dataSet <- qs::qread(dataName);
  if(.on.public.web){
    load_lattice()
  }
  qc.boxplot(dataSet$data, boxplotName, format, dpi);
  qc.pcaplot(dataSet$data, pcaName, format, dpi);
}

qc.boxplot <- function(dat, imgNm, format="png", dpi=default.dpi, width=NA){

  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  require("lattice");
  subgene=10000;
  if (nrow(dat)>subgene) {
    set.seed(28051968);
    sg  = sample(nrow(dat), subgene)
    Mss = dat[sg,,drop=FALSE]
  } else {
    Mss = dat
  }
  
  subsmpl=100;
  if (ncol(Mss)>subsmpl) {
    set.seed(28051968);
    ss  = sample(ncol(Mss), subsmpl)
    Mss = Mss[,ss,drop=FALSE]
  } else {
    Mss = Mss
  }
  
  sample_id = rep(seq_len(ncol(Mss)), each = nrow(Mss));
  values  = as.numeric(Mss)
  formula = sample_id ~ values
  
  box = bwplot(formula, groups = sample_id, layout = c(1,1), as.table = TRUE,
               strip = function(..., bg) strip.default(..., bg ="#cce6ff"),
               horizontal = TRUE,
               pch = "|",  col = "black", do.out = FALSE, box.ratio = 2,
               xlab = "", ylab = "Features",
               fill = "#1c61b6AA",
               panel = panel.superpose,
               scales = list(x=list(relation="free"), y=list(axs="i")),
               ylim = c(ncol(Mss)+0.7,0.3),
               prepanel = function(x, y) {
                 list(xlim = quantile(x, probs = c(0.01, 0.99), na.rm=TRUE))
               },
               panel.groups = function(x, y, ...) {
                 panel.bwplot(x, y, ...)
               })
  
  Cairo::Cairo(file=imgNm, width=460, height=420, type="png", bg="white");
  print(box);
  dev.off();
}

qc.pcaplot <- function(x, imgNm, format="png", dpi=default.dpi, width=NA){
  imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
  pca <- prcomp(t(na.omit(x)));
  names <- colnames(x);
  pca.res <- as.data.frame(pca$x);
  # increase xlim ylim for text label
  xlim <- GetExtendRange(pca.res$PC1);
  ylim <- GetExtendRange(pca.res$PC2);
  pcafig = lattice::xyplot(PC2~PC1, data=pca.res, pch=19, cex=1, xlim = xlim, ylim=ylim,
                           panel=function(x, y, ...) {
                             panel.xyplot(x, y, ...);
                             ltext(x=x, y=y, labels=names, pos=1, offset=1, cex=0.8, col="magenta")
                           })
  
  Cairo::Cairo(file=imgNm, width=480, height=480, type="png", bg="white");
  print(pcafig);
  dev.off();
}

#'Prepare data for Upset diagram
#'@param mSetObj Input name of the created mSet Object
#'@param fileNm json file name to save
#'@export
PrepareUpsetData <- function(mSetObj=NA, fileNm){
  
  mSetObj <- .get.mSet(mSetObj);
  metastat.de <- mSetObj$analSet$metastat.de;
  meta.stat <- mSetObj$analSet$metastat;
  mdata.all <- mSetObj$mdata.all;
  newDat <- list();
  hit.inx <- mdata.all==1;
  sel.nms <- names(mdata.all)[hit.inx];
  sel.dats <- list();
  
  for(nm in sel.nms){
    res.mat <- metastat.ind[[nm]];
    sig.inx <- res.mat[,2]<=GlobalCutOff$BHth;
    sel.dats[[nm]] <- rownames(res.mat[sig.inx,]);
  }
  
  if(anal.type == "metadata" & meta.selected){
    sel.dats[["meta_dat"]] <- as.character(meta.stat$de);
  }
  
  if(length(sel.dats) == 0){
    AddErrMsg("No signficant features for any dataset!");
    return(0);
  }

  sums <- unlist(lapply(metastat.de, length))
  names <- unlist(lapply(metastat.de, paste, collapse = ", "))
  metasum <- length(meta.stat$de)
  metaname <- paste(meta.stat$de, collapse = ", ")
  allsums <- c(sums, metasum)
  allnames <- c(names, metaname)
  sigmat <- cbind(allsums, allnames)
  colnames(sigmat) <- c("Sum of DE Features", "Names of DE Features")
  rownames(sigmat) <- c(names(metastat.de), "Meta-Analysis")
  mSetObj$analSet$sigfeat.matrix <- sigmat;

  require(reshape2)
  df <- melt(sel.dats, value.name="id")
  colnames(df) <- c("name", 'set')
  uniq.nms <- unique(df$name)
  new.df <- dcast(df, name ~ set, value.var='set', fill=0)
  rownames(new.df) <- new.df[,1]
  new.df <- new.df[,-1]
  json.list <- list()
  for(i in 1:nrow(new.df)){
    json.list[[i]] <- list()
    json.list[[i]][["sets"]] <- new.df[i,][new.df[i,] != 0]
    json.list[[i]][["name"]] <- rownames(new.df)[i]
  }
  col.vec <-gg_color_hue(length(sel.dats));

  jsonNm <- paste0(fileNm, ".json")
  json.mat <- RJSONIO::toJSON(list(json.list, col.vec));
  sink(jsonNm);
  cat(json.mat);
  sink();
  
  return(.set.mSet(mSetObj)); 
}

PlotMetaPCA <- function(mSetObj = NA, imgNm, dpi, format, interactive = FALSE) {
  mSetObj <- .get.mSet(mSetObj)
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
}
  require(ggplot2)
  require(Cairo)
  require(plotly)

  x <- metastat.meta$data
  cls.lbl <- metastat.meta$cls.lbl
  data.lbl <- metastat.meta$data.lbl

  x[!is.finite(x)] <- NA

  # Perform PCA
  pca <- prcomp(t(na.omit(x)))
  imp.pca <- summary(pca)$importance
  xlabel <- paste0("PC1 (", round(100 * imp.pca[2, 1], 1), "%)")
  ylabel <- paste0("PC2 (", round(100 * imp.pca[2, 2], 1), "%)")

  pca.res <- as.data.frame(pca$x)
  Conditions <- factor(cls.lbl)
  Datasets <- factor(data.lbl)

  xlim <- GetExtendRange(pca.res$PC1)
  ylim <- GetExtendRange(pca.res$PC2)

  pcafig <- ggplot(pca.res, aes(x = PC1, y = PC2, color = Conditions, shape = Datasets)) +
    geom_point(size = 4, alpha = 0.6) +
    scale_color_discrete(name = "Conditions") +
    scale_shape_discrete(name = "Datasets") +
    xlab(xlabel) + ylab(ylabel) +
    xlim(xlim) + ylim(ylim) +
    theme_bw()

  # Save static image
  img.path <- paste0(imgNm, "dpi", dpi, ".", format)
  plotlyNm <- paste0(imgNm, ".rda")
  dpi <- as.numeric(dpi)

  Cairo(file = img.path, width = 8, height = 6, type = format, bg = "white", units = "in", dpi = dpi)
  print(pcafig)
  dev.off()

  # Save image path and plotly name
  mSetObj$imgSet$qc_meta_pca <- img.path
  mSetObj$imgSet$qc_meta_pca_plotly <- plotlyNm

  # Save to mSetObj
  mSetObj$dataSet$qc_meta_pca_plotly <- plotlyNm

  # Save interactive plotly object
  m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
  ggp_build <- layout(ggplotly(pcafig), autosize = FALSE, width = 800, height = 600, margin = m)
  save(ggp_build, file = plotlyNm)

  return(.set.mSet(mSetObj))
}


PlotMetaDensity <- function(mSetObj=NA, imgNm, dpi = default.dpi, format, interactive = FALSE) {
if(!exists('metastat.meta')){
  metastat.meta <<- qs::qread("metastat.meta.qs");
}
  mSetObj <- .get.mSet(mSetObj);
  require(ggplot2)
  require(Cairo)
  require(plotly)

  # Use metastat.meta from memory
  dat <- metastat.meta$data
  data.lbl <- metastat.meta$data.lbl

  img.path <- paste0(imgNm, "dpi", dpi, ".", format)
  plotlyNm <- paste0(imgNm, ".rda")
  dpi <- as.numeric(dpi)

  # Prepare data
  df <- data.frame(dat, stringsAsFactors = FALSE)
  df <- stack(df)

  conv <- data.frame(ind = colnames(dat), class = data.lbl)
  conv$ind <- gsub("-", ".", conv$ind)
  df1 <- merge(df, conv, by = "ind")

  g <- ggplot(df1, aes(x = values)) +
    geom_line(aes(color = class, group = ind), stat = "density", alpha = 0.3) +
    geom_line(aes(color = class), stat = "density", alpha = 0.6, size = 1.5) +
    theme_bw()

  # Save static image
  Cairo(file = img.path, width = 10, height = 6, type = format, bg = "white", dpi = dpi, unit = "in")
  print(g)
  dev.off()

  # Save image and plotly metadata
  mSetObj$imgSet$qc_meta_density <- img.path
  mSetObj$imgSet$qc_meta_density_plotly <- plotlyNm

  # Save to mSetObj$dataSet
  mSetObj$dataSet$qc_meta_density_plotly <- plotlyNm

  # Save plotly object
  m <- list(l = 50, r = 50, b = 20, t = 20, pad = 0.5)
  ggp_build <- layout(ggplotly(g), autosize = FALSE, width = 800, height = 600, margin = m)
  save(ggp_build, file = plotlyNm)

  return(.set.mSet(mSetObj));
}

ToggleMetaRes <- function(mSetObj=NA, type) {
    mSetObj <- .get.mSet(mSetObj)
    if(type == "metap"){
      meta.mat <<- mSetObj$analSet$metap.mat
    }else if(type == "votecount"){
      meta.mat <<- mSetObj$analSet$votecount.mat
    }else{
      meta.mat <<- mSetObj$analSet$merge.mat
    }
    print(head(meta.mat));
    print("metamat====");
    mSetObj <- SetupMetaStats(mSetObj, mSetObj$dataSet$pvalcutoff);
    .set.mSet(mSetObj)
}
