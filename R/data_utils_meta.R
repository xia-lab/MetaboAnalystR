##################################################
## R script for ExpressAnalyst
## Description: Data/resource management functions
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################

#'Updating meta-data info (meta-analysis)
#'@description Assign class labels to samples (meta-analysis)
#'@param dataName File name of data set.
#'@param clsLbl vector of class labels to be assigned to samples
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
UpdateSampleInfo<-function(dataName, clsLbl){
    msgSet <- readSet(msgSet, "msgSet");
    dataSet <- readDataset(dataName);
    print("updating sample info .... ");

    if(!exists("class.vec")){
        print("Could not find class label list!");
        return(0);
    }

    if(!exists("smpl.vec")){
        print("Could not find sample name list!");
        return(0);
    }

    if(length(class.vec) < 2){
        msgSet$current.msg <- "Add least two groups required!";
        saveSet(msgSet, "msgSet");
        return(0);
    }

    if(sum(class.vec != 'NA') < 2){
        msgSet$current.msg <- "Cannot be less than 2 groups";
        saveSet(msgSet, "msgSet");
        return(0);
    }

    dataSet <- readDataset(dataName);
    
    org.lvl.len <- length(levels(dataSet$meta.info[[clsLbl]]));
    if(org.lvl.len < length(class.vec)){
        msgSet$current.msg <- "You can not add new groups";
        saveSet(msgSet, "msgSet");
        return(0);
    }else if(org.lvl.len > length(class.vec)){
        msgSet$current.msg <- "To exclude a group, replace it with NA.";
        saveSet(msgSet, "msgSet");
        return(0);
    }

    # first update the meta info
    cls <- dataSet$meta.info[[clsLbl]];
    levels(cls) <- class.vec;

    data <- dataSet$data.orig;
    meta.info <- dataSet$meta.info;
 
    if(any(levels(cls) == 'NA')){
        rt.inx <- cls != 'NA';
        data <- data[,rt.inx];

        # also update the whole meta-info
        meta.info <- meta.info[rt.inx,,drop=FALSE];
        cls <- cls[rt.inx];
    }

    # need to re-construct the class, so that the level order  
    # are always alphabetic
    meta.info[[clsLbl]] <- factor(as.character(cls));

    # note, sample names could be removed (together with cls) as the whole row
    hit.inx <- colnames(data)%in%smpl.vec;
    dataSet$data.orig <- data[,hit.inx];

    # make sure the factor levels also dropped
    for(i in 1:length(meta.info)){
        meta.info[[i]] <- factor(meta.info[[i]][hit.inx]);
    }

    dataSet$meta.info <- meta.info;
    dataSet$cls <-  dataSet$meta.info[[clsLbl]];
    RegisterData(dataSet);
    gc();
    return(RegisterData(dataSet));
}


# note, here also update data type array/count
# normalize to zero mean and unit variance
AutoNorm<-function(x){
	(x - mean(x))/sd(x, na.rm=T);
}

######################################
## methods for merged expression data
#######################################

GlobalCutOff <- list(
    logFC = 0,
    BHth = 0.05
)

# read meta-dataset previously processed

#'Read merged gene expression table
#'@description Perform normalization on individual dataset in meta-analysis
#'@param dataName File name of data set.
#'@param norm.opt Normalization option (log, vsn, quantile, combined, logCount, NA)
#'@param auto.opt integer (0,1), Perform auto scaling
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'
ReadMergedExpressTable <- function(dataName){
    paramSet <- readSet(paramSet, "paramSet");
    msgSet <- readSet(msgSet, "msgSet");
    msgSet$current.msg <- "";
    paramSet$meta.upload <- TRUE;
    dataSet <- .readTabData(dataName);
    if(is.null(dataSet)){
        return(0);
    }
    common.matrix <- dataSet$data;
    meta.nms <- tolower(names(dataSet$meta.info));

    cls.inx <- grep("condition", meta.nms);
    if(length(cls.inx) == 0){
        msgSet$current.msg <- "No condition label found (#CLASS.condition)";
        saveSet(msgSet, "msgSet");
        return(0);
    }else{
        cls.inx <- cls.inx[1];
        cls.lbl <- dataSet$meta.info[[cls.inx]];
    }

    data.inx <- grep("dataset", meta.nms);
    if(length(data.inx) == 0){
        msgSet$current.msg <- "No dataset label found (#CLASS.dataset)";
        saveSet(msgSet, "msgSet");
        return(0);
    }else{
        data.inx <- data.inx[1];
        data.lbl <- dataSet$meta.info[[data.inx]];
        data.nms <- unique(as.character(data.lbl));
        
        # now create the mdata.all object
        mdata.all <- vector(mode="list", length=length(data.nms));
        names(mdata.all) <- data.nms;
        mdata.all <- lapply(mdata.all, function(x){x=1});
        paramSet$mdata.all <- mdata.all;
    }

    if(length(grep("entrez.hsa", meta.nms)) > 0){
        paramSet$data.org <- "hsa"
        paramSet$data.idType <- "entrez";
        shared.nms <- rownames(common.matrix);
        symbols <- doEntrez2SymbolMapping(shared.nms, paramSet$data.org, paramSet$data.idType);
        names(symbols) <- shared.nms;
    }else if(length(grep("entrez.mmu", meta.nms)) > 0){
        paramSet$data.org <- "mmu"
        paramSet$data.idType <- "entrez";
        shared.nms <- rownames(common.matrix);
        symbols <- doEntrez2SymbolMapping(shared.nms, paramSet$data.org, paramSet$data.idType);
        names(symbols) <- shared.nms;
    }else{
        symbols <- NULL;
        inmex.org <<- "NA"
        paramSet$data.idType <- 'NA';
    }
    paramSet$data.org <- unlist(strsplit( meta.nms[2], "[.]"))[3]
    inmex.meta.orig <- list(data=common.matrix,
                       id.type = id.type,
                       gene.symbls = symbols,
                       cls.lbl=factor(cls.lbl),
                       data.lbl=data.lbl);
    qs::qsave(inmex.meta.orig, "inmex.meta.orig.qs");
    saveSet(paramSet, "paramSet");
    if(length(levels(as.factor(data.lbl))) == 1){
        return(2);
    }else{
        return(1);
    }
}


# for gene-level meta-analysis
# function to set up results combining individual data analysis
# as well as to prepare for GO analysis
# no return, as set global 
SetupMetaStats <- function(BHth=0.05, paramSet,analSet){
  paramSet <<- paramSet;
  analSet <<- analSet;
  save.image("metastat.RData");
  meta.mat <- analSet$meta.mat.all;
  paramSet$BHth <- BHth;

  #all common genes
  inmex.meta <- qs::qread("inmex_meta.qs");
  gene.ids <- rownames(inmex.meta$data);
  # meta.sig genes
  metade.genes <- rownames(meta.mat);
  
  # setup individual sig genes & stats
  # that overlap with meta.sig
  inmex.de <- list();
  
  pval.mat <- fc.mat <- matrix(nrow=nrow(meta.mat), ncol=length(paramSet$mdata.all));
  
  for(i in 1:length(analSet$inmex.ind)){
    de.res <- analSet$inmex.ind[[i]];
    
    hit.inx <- de.res[,2] <= BHth;
    hit.inx <- which(hit.inx); # need to get around NA
    inmex.de[[i]] <- rownames(de.res)[hit.inx];
    
    # only choose the genes that are also meta sig genes from in
    # individual analysis for display
    de.res <- de.res[metade.genes,];
    
    fc.mat[,i] <- de.res[,1];
    pval.mat[,i] <- de.res[,2];
    if(i == 1){
        rownames(fc.mat) <- rownames(de.res);
        rownames(pval.mat) <- rownames(de.res);
    }
  }
  print("pval======mat");
  print(head(pval.mat))
  
  dataNms <- names(analSet$inmex.ind);
  newNms <- substring(dataNms,0, nchar(dataNms)-4);
  colnames(fc.mat) <- paste0(newNms, "_FC");
  colnames(pval.mat) <- paste0(newNms, "_Pval");
  
  names(inmex.de) <- names(analSet$inmex.ind);
  
  # calculate gain/loss
  deindst <- unique(unlist(inmex.de));
  gains=metade.genes[which(!(metade.genes %in% deindst))];
  losses=deindst[which(!(deindst %in% metade.genes))];
  all.de <- cbind(gene.ids %in% metade.genes, gene.ids %in% deindst);
  colnames(all.de) <- c("Meta-DE", "Individual-DE");
  if(inmex.meta$id.type == "entrez"){ 
    names(metade.genes) <- inmex.meta$gene.symbls[metade.genes];
    names(gains) <- inmex.meta$gene.symbls[gains];
    names(losses) <- inmex.meta$gene.symbls[losses];
  }
  
  # de genes from individual 
  de.len <- sapply(inmex.de, length);
  stat <- c(length(metade.genes), de.len);
  names(stat) <- c("Meta", substr(names(inmex.de), 0, nchar(names(inmex.de))-4));
  meta.stat <- list(
    stat = stat,
    de = metade.genes,
    idd = gains,
    loss = losses
  );
 
  fc.mat <- fc.mat[match(rownames(meta.mat), rownames(fc.mat)), ]
  pval.mat <- pval.mat[match(rownames(meta.mat), rownames(pval.mat)), ]

  analSet$fc.mat <- fc.mat;
  analSet$pval.mat <- pval.mat;
  analSet$inmex.de <- inmex.de;
  analSet$meta.stat <- meta.stat;

  if(colnames(analSet$meta.mat.all)[1] != "AverageFc" && paramSet$inmex.method != "merge"){
    #cal sampleNumbers
    meta.info <- paramSet$dataSet$meta.info;
    sampleCounts <- table(meta.info$Dataset)
    sampleNumbers <- as.vector(sampleCounts)
    sampleNumbers <- unname(sampleNumbers)

    # Compute weighted average of fold change values
    weightedAvgFC <- rowSums(fc.mat * sampleNumbers) / sum(sampleNumbers)
    
    # Convert to a data frame for easier handling and naming
    weightedAvgFC_df <- as.data.frame(weightedAvgFC)
    colnames(weightedAvgFC_df) <- "AverageFc"
    weightedAvgFC_df <- weightedAvgFC_df[match(rownames(analSet$meta.mat.all), rownames(weightedAvgFC_df)), ,drop=F]
    analSet$avg.fc.mat <- weightedAvgFC_df;
    analSet$meta.avgFC <- as.vector(analSet$avg.fc.mat[,1]);
    names(analSet$meta.avgFC) <- rownames(analSet$meta.mat.all);
    analSet$meta.mat.all <- cbind(weightedAvgFC_df, analSet$meta.mat.all);
  }

  dat.mat <- cbind(fc.mat, pval.mat, meta.mat);
  dat.mat <- signif(dat.mat, 5);
  # save the result
  if(inmex.meta$id.type == "entrez"){ # row name gene symbols
    metade.nms <- inmex.meta$gene.symbls[metade.genes];
    res <- cbind(EntrezID=metade.genes, Name=metade.nms,dat.mat);
  }else{
    res <- cbind(ID=metade.genes, dat.mat);
  }
  analSet$meta.res.table <- res;
  fast.write(res, file=paste("meta_sig_genes_", paramSet$inmex.method, ".csv", sep=""), row.names=F);
  
  return(list(paramSet, analSet))
}

#compute Cochran’s Q to help FEM/REM
# plot Q-Q plot for estimation

#'Compute Cochran's Q test
#'@description Plot Q-Q plot for estimating statistical heterogeneity. Cochran’s Q test is calculated as the weighted sum of squared differences between individual study effects and the pooled effect across studies. When the estimated Q values have approximately a chi-squared distribution, it suggests FEM assumption is appropriate. If it deviates significantly from a chi-squared distribution, REM should usually be used.
#'@param imgNm Image name to output
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT
#'@export
#'

PlotCochranQ <- function(imgNm){
    paramSet <- readSet(paramSet, "paramSet");
    if(!paramSet$performedDE){
        analSet <- PerformMetaDeAnal(paramSet);
        paramSet <- readSet(paramSet, "paramSet");
        saveSet(analSet, "analSet");
    }
    mdata.all <- paramSet$mdata.all;
    inmex.meta <- qs::qread("inmex_meta.qs");
    sel.inx <- mdata.all==1;
    sel.nms <- names(mdata.all)
    nbstudies <- sum(sel.inx);
    ES<-array(dim=c(nrow(inmex.meta$data),4,nbstudies));
    cls.lvls <- levels(as.factor(inmex.meta$cls.lbl));

    for (i in 1:nbstudies){
        data.nm <- sel.nms[i];
        dataSet <- readDataset(data.nm);

        fit.obj.nm <- paste(data.nm, "fit.obj", sep=".");
        fit2i <- qs::qread(fit.obj.nm);
        #fit2i <- dataSet$fit.obj;

        n1i=length(which(dataSet$cls==cls.lvls[1]));
        n2i=length(which(dataSet$cls==cls.lvls[2]));
        ES[,,i]=effectsize(fit2i$t,((n1i*n2i)/(n1i+n2i)),(fit2i$df.prior+fit2i$df.residual));
    }

    Qvals <- f.Q.NA(ES[,3,],ES[,4,]);

    Cairo(file=imgNm, width=400, height=400, type="png", bg="white");
    # histgram
    # hist(Qvals, breaks = 50, col = "red");
    # QQ plot
    chisqq <- qchisq(seq(0, 0.9999, 0.001), df = nbstudies-1)
    tmp <- quantile(Qvals, seq(0, 0.9999, 0.001))
    qqplot(chisqq, tmp, ylab = "Quantiles of Sample", pch = "*", 
    xlab = "Quantiles of Chi > square", main = "QQ Plot")
    lines(chisqq, chisqq, lty = "dotted", col = "red")

    dev.off(); 

    Qmean <- round(mean(Qvals),5);
    return (Qmean);
}

#' Perform Batch Correction
#'
#' This function performs batch correction using the ComBat method.
#'
#' @return A list of datasets after batch correction.
#'
#' @author Guangyan Zhou\email{guangyan.zhou@mail.mcgill.ca}
#' @details Additional details about the function, if needed.
#'
#' @examples
#' \dontrun{
#' PerformBatchCorrection()
#' }
#'
#' @export
#' @license MIT License
#'
PerformBatchCorrection <- function(){
    .prepare.batch();
    .perform.computing();
    paramSet <- readSet(paramSet, "paramSet");
    paramSet$performedBatch <- T;
    saveSet(paramSet);
    return(dataSets);
}

.prepare.batch<-function(){
    my.fun <- function(){
        require('sva');
        inmex.meta <- qs::qread("inmex_meta.qs");
        data.lbl <- inmex.meta$data.lbl
        pheno <- data.frame(cbind(inmex.meta$cls.lbl, data.lbl));
        modcombat <- model.matrix(~1, data=pheno)
        batch <- data.lbl;
        inmex.meta$data <- ComBat(dat=inmex.meta$data, batch=batch, mod=modcombat, par.prior=TRUE, prior.plots=FALSE);
        qs::qsave(inmex.meta, "inmex_meta.qs");
    }
    dat.in <- list(my.fun=my.fun);
    qs:::qsave(dat.in, file="dat.in.qs");
    return(1);
}
