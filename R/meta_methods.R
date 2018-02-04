
#'Check if data are ready for meta-analysis
#'@description This function determines if all annotated data are ready for meta-analysis
#'@param mSetObj Input name of the created mSet Object
#'@param combat Adjust for batch effects, logical variable: TRUE = adjust for batch effects using an empirical Bayes framework (R package sva), FALSE = 
#'no batch effect adjustment.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

CheckMetaDataConsistency<-function(mSetObj=NA, combat=TRUE){

  mSetObj <- .get.mSet(mSetObj);
  
    if(length(mdata.all) == 0){
        AddErrMsg("Please upload your data or try our example datasets!");
        return(0);
    }

    include.inx <- mdata.all==1;
    if(sum(include.inx) < 2){
        AddErrMsg("At least two datasets are required for meta-analysis!");
        return(0);
    }

    sel.nms <- names(mdata.all)[include.inx];

    # first check that all class labels are consistent
    dataSet <- readRDS(sel.nms[1]);
    lvls <- levels(dataSet$cls);
    id.type <- dataSet$id.type;
    
    # note, the features are in columns!!!!
    nms <- colnames(dataSet$data);
    shared.nms <- nms;
    for(i in 2:length(sel.nms)){
        dataSet <- readRDS(sel.nms[i]);
        # check if class label is consistent
        if(!all(levels(dataSet$cls) == lvls)){
            AddErrMsg(paste(sel.nms[i], "has different group labels", paste(levels(dataSet$cls), collapse=":"), "from", sel.nms[1], paste(lvls, collapse=":")));
            return(0);
        }

        # check and record if there is common genes            
        shared.nms <- intersect(shared.nms, colnames(dataSet$data));
        if(length(shared.nms) < ncol(dataSet$data)/4){
            AddErrMsg(paste(sel.nms[i], "has less than 25% common features from the previous data sets"));
            return(0);
        }
    }
    AddMsg("Passed experimental condition check!");
        
    # now construct a common matrix to faciliate plotting across all studies
    dataName <- sel.nms[1];
    dataSet <- readRDS(dataName);
    common.matrix <- dataSet$data[, shared.nms];
    data.lbl <- rep(dataName, nrow(common.matrix));
    cls.lbl <- dataSet$cls;

    for(i in 2:length(sel.nms)){
        dataName <- sel.nms[i];
        dataSet <- readRDS(dataName);
        ndat <- dataSet$data[, shared.nms];

        # note, there could be duplicate sample names across studies
        rownames(ndat) <- paste(rownames(ndat),"_",i, sep="");
        plot.ndat <- t(scale(t(ndat))); # scale sample wise (default scale column)
        common.matrix <- rbind(common.matrix, ndat);
        data.lbl <- c(data.lbl, rep(dataName, nrow(dataSet$data[,])));
        cls.lbl <- c(cls.lbl, dataSet$cls);
    }

    cls.lbl <- factor(cls.lbl);
    levels(cls.lbl) <- lvls;
    colnames(common.matrix) <- shared.nms;
    ord.inx <- order(data.lbl, cls.lbl);
    cls.lbl <- cls.lbl[ord.inx];
    data.lbl <- data.lbl[ord.inx];
    common.matrix <- data.matrix(common.matrix[ord.inx,]);

    AddMsg("Constructed the commom matrix!");

    if(nrow(common.matrix) > 1000){  # max sample number allow 1000
        AddErrMsg(paste("Total combined sample #:", nrow(common.matrix), "(exceed the limit: 1000!)"));
        return(0);
    }
    
    # now from here, we want to transpose the data as in gene expression data 
    # (i.e. samples in columns) to be easier for further analysis
    common.matrix <- t(common.matrix);

    if(combat){
        library('sva');
        pheno <- data.frame(cbind(cls.lbl, data.lbl));
        modcombat = model.matrix(~1, data=pheno)
        batch <- data.lbl;
        combat_edata = ComBat(dat=common.matrix, batch=batch, mod=modcombat, par.prior=TRUE, prior.plots=FALSE)
        common.matrix <- combat_edata;
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

    PerformEachDEAnal(mSetObj);

    # setup common stats gene number, smpl number, grp info
    studyinfo <- paste("Sample #:", ncol(metastat.meta$data), "Common ID #:", nrow(metastat.meta$data), "Condition:", paste(levels(metastat.meta$cls.lbl), collapse=" vs. "));
    
    AddMsg(studyinfo)
    mSetObj$dataSet$studyinfo <- studyinfo
    
    if(.on.public.web==TRUE){
      if(length(sel.nms) == 1){
        .set.mSet(mSetObj)
        return(2);
      }else{
        .set.mSet(mSetObj)
        return(1);
      }
    }else{
      return(.set.mSet(mSetObj));
    }
}

#'Performs differential expression analysis on individual data
#'@description This function performs DE analysis on individual data using the common matrix, which
#'will be used/compared in later steps of the analysis (according to the p-value). The DE for each feature
#'may be adjusted using the p-value.  
#'@param mSetObj Input name of the created mSet Object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformEachDEAnal <- function(mSetObj=NA){
  
    metastat.ind <- list();
    sel.nms <- names(mdata.all)[mdata.all==1];

    for(i in 1:length(sel.nms)){
        dataName <- sel.nms[i];
        sel.inx <- metastat.meta$data.lbl == dataName;
        group <- factor(metastat.meta$cls.lbl[sel.inx]); # note regenerate factor to drop levels 
        data <- metastat.meta$data[, sel.inx];

        dataSet <- readRDS(dataName);
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
     metastat.ind <<- metastat.ind;
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
#'@export

PerformPvalCombination <- function(mSetObj=NA, method="stouffer", BHth=0.05){

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$pvalmethod <- method
  mSetObj$dataSet$pvalcutoff <- BHth
  
    metastat.method <<- "metap";
    meta.mat <<- meta.stat <<- NULL;
    sel.nms <- names(mdata.all)[mdata.all==1];

    classes <- list();
    nbstudies <- length(sel.nms);
    listgd=vector("list", (nbstudies+3));

    for (i in 1:nbstudies){
        data.nm <- sel.nms[i];
        dataSet <- readRDS(data.nm);
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

    meta.mat <<- pc.mat[sig.inx, ];
    SetupMetaStats(BHth);

    if(.on.public.web==TRUE){
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

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$vote <- minVote
  mSetObj$dataSet$pvalcutoff <- BHth
  
    metastat.method <<- "votecount";
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
    meta.mat <<- vc.mat[sig.inx, ,drop=F];
    SetupMetaStats(BHth);
    
    if(.on.public.web==TRUE){
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
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$pvalcutoff <- BHth
  
    metastat.method <<- "merge";
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

    meta.mat <<- dm.mat[sig.inx,];
    SetupMetaStats(BHth);
    
    if(.on.public.web==TRUE){
      .set.mSet(mSetObj)
      return(length(sig.inx));
    }else{
      return(.set.mSet(mSetObj));
    }
}

#'Meta-Analysis: Plot Venn Diagram
#'@description This function plots a venn diagram of the individual studies.
#'@param mSetObj Input name of the created mSet Object.
#'@param imgName 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PlotMetaVenn<-function(mSetObj=NA, imgNM=NA){

  mSetObj <- .get.mSet(mSetObj);

  imgName <- paste(imgNM, ".png", sep="");
  mSetObj$imgSet$venn <- imgName;

  Cairo(file = imgName, width=420, height=300,  type="png", bg="transparent");
  plotVennDiagram(meta.stat$venn, circle.col=c("blue", "forestgreen"), mar=c(0,0,2,0));
  dev.off();

  sums <- unlist(lapply(metastat.de, length))
  names <- unlist(lapply(metastat.de, paste, collapse = ", "))
  metasum <- length(meta.stat$idd)
  metaname <- paste(meta.stat$idd, collapse = ", ")
  allsums <- c(sums, metasum)
  allnames <- c(names, metaname)
  sigmat <- cbind(allsums, allnames)
  colnames(sigmat) <- c("Sum of DE Features", "Names of DE Features")
  rownames(sigmat) <- c(names(metastat.de), "Meta-Analysis")
  mSetObj$analSet$sigfeat.matrix <- sigmat
  
  return(.set.mSet(mSetObj));
}

#' Utility function: Plot Venn diagram
#'Gordon Smyth, James Wettenhall.
#'Capabilities for multiple counts and colors by Francois Pepin.
#'4 July 2003.  Last modified 12 March 2010.
plotVennDiagram <- function(object,include="both",names,mar=rep(0,4),cex=1.2,lwd=1,circle.col,counts.col,show.include,...){

  nsets <- ncol(object)-1
  if(missing(names)) names <- colnames(object)[1:nsets]
  counts <- object[,"Counts"]
  if(length(include)==2) counts.2 <- object.2[, "Counts"]
  if(missing(circle.col)) circle.col <- par('col')
  if(length(circle.col)<nsets) circle.col <- rep(circle.col,length.out=nsets)
  if(missing(counts.col)) counts.col <- par('col')
  if(length(counts.col)<length(include)) counts.col <- rep(counts.col,length.out=length(include))
  if(missing(show.include)) show.include <- as.logical(length(include)-1)
  theta <- 2*pi*(0:360)/360
  xcentres <- c(-1.2, 1.2);
  ycentres <- c(0,0);
  r <- 2.8;
  xtext <- c(-1.5,1.5)
  ytext <- c(3.5,3.5)
  
  par(oma=c(0,0,0,0),mar=c(0,0,0,0));
  
  plot(x=0,y=0,type="n",xlim=c(-4,4),ylim=c(-4,4),xlab="",ylab="",axes=FALSE,...);
  
  circle.col <- col2rgb(circle.col) / 255
  circle.col <- rgb(circle.col[1,], circle.col[2,], circle.col[3,], 0.3)
  for(i in 1:nsets) {
    lines(xcentres[i]+r*cos(theta),ycentres[i]+r*sin(theta),lwd=lwd,col=circle.col[i])
    polygon(xcentres[i] + r*cos(theta), ycentres[i] + r*sin(theta), col = circle.col[i], border = NULL)
    text(xtext[i],ytext[i],names[i],cex=cex)
  }
  
  printing <- function(counts, cex, adj,col,leg){
    text(2.5,0.1,counts[2],cex=cex,col=col,adj=adj)
    text(-2.5,0.1,counts[3],cex=cex,col=col,adj=adj)
    text(0,0.1,counts[4],cex=cex,col=col,adj=adj)
  }
  
  printing(counts,cex,c(0.5,0.5),counts.col[1],include[1])
  invisible()
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

GetMetaGeneIDType<-function(){
    return(metastat.meta$id.type);
}

GetMetaResultGeneIDs<-function(){
     return(rownames(meta.mat));
}

# note, due to limitation of get/post
# maximum gene symb for list is top 5000
GetMetaResultGeneSymbols<-function(){
     ids <- rownames(meta.mat);
     if(length(ids) > 5000){
        ids <- ids[1:5000];
     }
     return(ids);
}

GetMetaResultGeneIDLinks <- function(){
     ids <- rownames(meta.mat);
     symbs <- metastat.meta$gene.symbls[ids];
     # set up links to genbank
     annots <- paste("<a href='http://www.ncbi.nlm.nih.gov/gene?term=", ids,
                        "' target='_blank'>", symbs, "</a>", sep="");
     return(annots);
}

GetMetaResultColNames <- function(){
    sel.nms <- names(mdata.all)[mdata.all==1];
    c(substring(sel.nms, 0, nchar(sel.nms)-4), colnames(meta.mat));
}

# single.type return logFC or p value for individual data analysis
#'@export
GetMetaResultMatrix <- function(mSetObj = NA, single.type="fc"){
  
  mSetObj <- .get.mSet(mSetObj);
  
    if(single.type == "fc"){
        meta.mat <- cbind(fc.mat, meta.mat);
    }else{
        meta.mat <- cbind(pval.mat, meta.mat);
    }
    meta.mat <- signif(as.matrix(meta.mat), 5);
    mSetObj$analSet$meta.mat <- meta.mat;

    if(.on.public.web == TRUE){  
      .set.mSet(mSetObj)
      meta.mat;
    }else{
      return(.set.mSet(mSetObj));
    }
}

GetMetaStat <- function(){
    return (meta.stat$stat);
}

GetMetaStatNames <- function(){
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

#combine effect size
combineES <- function (ES, varES, BHth = 0.05, method){
    if(method == "rem"){
        useREM = TRUE;
    }else{
        useREM = FALSE;
    }

    num.studies <- dim(ES)[2];

    Qvals <- f.Q.NA(ES, varES)
    if (useREM) {
        varES <- varES + tau2.NA(Qvals, num.studies, my.weights = 1/varES)
    }
    wt <- 1/varES
    MUvals <- rowSums(ES * wt, na.rm = TRUE)/rowSums(wt, na.rm = TRUE)
    MUsES <- sqrt(1/rowSums(wt, na.rm = TRUE))
    zSco <- MUvals/MUsES
    rpvalESc = 2 * (1 - pnorm(abs(zSco)))
    res = which(p.adjust(rpvalESc, method = "BH") <= BHth);
    listres <- list();
    listres[[1]] = res
    listres[[2]] = zSco
    listres[[3]] = MUvals; # pool effect size
    listres[[4]] = wt; # wt for each studies, it is matrix with one column for each studies
    names(listres) = c("DEindices", "TestStatistic", "PooledEffectSize", "Weights")
    listres
}

PlotDataProfile<-function(dataName, boxplotName, pcaName){
    dataSet <- readRDS(dataName);
    qc.boxplot(dataSet$data, boxplotName);
    qc.pcaplot(dataSet$data, pcaName);
}

qc.boxplot <- function(dat, imgNm, format="png", dpi=72, width=NA){
    imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");

    library('lattice');

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
        xlab = "", ylab = "Samples",
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

    Cairo(file=imgNm, width=460, height=420, type="png", bg="white");
    print(box);
    dev.off();
}

qc.pcaplot <- function(x, imgNm, format="png", dpi=72, width=NA){
    imgNm <- paste(imgNm, "dpi", dpi, ".", format, sep="");
    library('lattice');
    pca <- prcomp(t(na.omit(x)));
    names <- colnames(x);
    pca.res <- as.data.frame(pca$x);
    # increase xlim ylim for text label
    xlim <- GetExtendRange(pca.res$PC1);
    ylim <- GetExtendRange(pca.res$PC2);
    pcafig = xyplot(PC2~PC1, data=pca.res, pch=19, cex=1, xlim = xlim, ylim=ylim,
        panel=function(x, y, ...) {
               panel.xyplot(x, y, ...);
               ltext(x=x, y=y, labels=names, pos=1, offset=1, cex=0.8, col="magenta")
            })

    Cairo(file=imgNm, width=480, height=480, type="png", bg="white");
    print(pcafig);
    dev.off();
}

#'@export
PrepareVennData <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  
    # create a list store all possible combination (for a max of 4)
    # note, the whole region is divided into small areas (16 for 4; 7 for 3, 3 for 2)
    # for instance:
    # a: a unique (no b, no c)
    # ab: a and b, no c  

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
    if(length(sel.dats) == 2){
        Prepare2Venn(sel.dats);
    }else if(length(sel.dats) == 3){
        Prepare3Venn(sel.dats);
    }else if(length(sel.dats) == 4){
        Prepare4Venn(sel.dats);
    }else{
        AddErrMsg("Too big of a number of features for the venn diagram");
        return(0);
    }
    venn.list <<- sel.dats;

    PlotMetaVenn(mSetObj, "venn_diagram");

    if(.on.public.web==TRUE){
      return(1);
    }else{
      return(.set.mSet(mSetObj));
    }
}

#3
Prepare2Venn <- function(dat){
    nms <- names(dat);  
    a <- nms[1];
    b <- nms[2];
    ab <- paste(a, b, sep="");

    a.l <- dat[[a]];
    b.l <- dat[[b]];

    vennData <- list();
    vennData[[a]] <- setdiff(a.l, b.l);
    vennData[[b]] <- setdiff(b.l, a.l);    
    vennData[[ab]] <- intersect(b.l, a.l);
    vennData <<- vennData;
}

#7
Prepare3Venn <- function(dat){
    nms <- names(dat);  
    a <- nms[1];
    b <- nms[2];
    c <- nms[3];
    ab <- paste(a, b, sep="");
    ac <- paste(a, c, sep="");
    bc <- paste(b, c, sep="");
    abc <- paste(a, b, c, sep="");

    a.l <- dat[[a]];
    b.l <- dat[[b]];
    c.l <- dat[[c]];

    vennData <- list();
    vennData[[a]] <- setdiff(a.l, union(b.l, c.l));
    vennData[[b]] <- setdiff(b.l, union(a.l, c.l));    
    vennData[[c]] <- setdiff(c.l, union(a.l, b.l));    
    vennData[[ab]] <- setdiff(intersect(a.l, b.l), c.l);
    vennData[[ac]] <- setdiff(intersect(a.l, c.l), b.l);
    vennData[[bc]] <- setdiff(intersect(b.l, c.l), a.l);
    vennData[[abc]] <- intersect(intersect(a.l, b.l), c.l);
    vennData <<- vennData;
}

# 15
Prepare4Venn <- function(dat){
    nms <- names(dat);  
    a <- nms[1];
    b <- nms[2];
    c <- nms[3];
    d <- nms[4];
    ab <- paste(a, b, sep="");
    ac <- paste(a, c, sep="");
    ad <- paste(a, d, sep="");
    bc <- paste(b, c, sep="");
    bd <- paste(b, d, sep="");
    cd <- paste(c, d, sep="");
    abc <- paste(a, b, c, sep="");
    abd <- paste(a, b, d, sep="");
    acd <- paste(a, c, d, sep="");
    bcd <- paste(b, c, d, sep="");
    abcd <- paste(a, b, c, d, sep="");

    a.l <- dat[[a]];
    b.l <- dat[[b]];
    c.l <- dat[[c]];
    d.l <- dat[[d]];

    vennData <- list();
    vennData[[a]] <- setdiff(a.l, unique(c(b.l, c.l, d.l)));
    vennData[[b]] <- setdiff(b.l, unique(c(a.l, c.l, d.l)));    
    vennData[[c]] <- setdiff(c.l, unique(c(a.l, b.l, d.l)));    
    vennData[[d]] <- setdiff(d.l, unique(c(a.l, b.l, c.l))); 
    vennData[[ab]] <- setdiff(intersect(a.l, b.l), union(c.l, d.l));
    vennData[[ac]] <- setdiff(intersect(a.l, c.l), union(b.l, d.l));
    vennData[[ad]] <- setdiff(intersect(a.l, d.l), union(b.l, c.l));
    vennData[[bc]] <- setdiff(intersect(b.l, c.l), union(a.l, d.l));
    vennData[[bd]] <- setdiff(intersect(b.l, d.l), union(a.l, c.l));
    vennData[[cd]] <- setdiff(intersect(c.l, d.l), union(a.l, b.l));
    vennData[[abc]] <- setdiff(intersect(intersect(a.l, b.l), c.l), d.l);
    vennData[[abd]] <- setdiff(intersect(intersect(a.l, b.l), d.l), c.l);
    vennData[[acd]] <- setdiff(intersect(intersect(a.l, c.l), d.l), b.l);
    vennData[[bcd]] <- setdiff(intersect(intersect(b.l, c.l), d.l), a.l);
    vennData[[abcd]] <- intersect(intersect(a.l, b.l), intersect(c.l, d.l));
    vennData <<- vennData;
}

#'@export
GetSelectedDataNumber <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web==TRUE){
    return(length(venn.list));
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'@export
GetSelectedDataNames <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web==TRUE){
    return(paste(names(venn.list), collapse=";"));
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'@export
#areas is allname concated
GetVennGeneNames <- function(mSetObj=NA, areas){

  mSetObj <- .get.mSet(mSetObj);
  
    nms <- strsplit(areas, "\\|\\|")[[1]];
    gene.vec <- NULL;
    for(nm in nms){
        gene.vec <- c(gene.vec, vennData[[nm]]);
    }
    gene.vec <- unique(gene.vec);
    names(gene.vec) <- gene.vec;
    venn.genes <<- gene.vec;
    
    if(.on.public.web==TRUE){
      return(paste(unique(gene.vec), collapse="||"));
    }else{
      return(.set.mSet(mSetObj));
    }
}

#'@export
#areas is allname concated
GetMetaSigHitsTable <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$sigfeat.matrix);
}

