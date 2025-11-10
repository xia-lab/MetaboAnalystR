CheckDetailsTablePerformed <-function(type){
  mSetObj <- .get.mSet(NA);
  performed <- T;
  if(grepl("volcano", type)){
    performed <- !is.null(mSetObj$analSet$volcano);
  }else if(startsWith(type, "pls")){
    performed <- !is.null(mSetObj$analSet$plsr);
  }else if(type == "template"){
    performed <- !is.null(mSetObj$analSet$corr);
  }else if(startsWith(type, "rf")){
    performed <- !is.null(mSetObj$analSet$rf);
  }else if(startsWith(type, "opls")){
    performed <- !is.null(mSetObj$analSet$oplsda);
  }else if(startsWith(type, "svm")){
    performed <- !is.null(mSetObj$analSet$svm);
  }else if(type == "anova"){
    performed <- !is.null(mSetObj$analSet$aov$sig.mat);
  }else if(type == "anova2"){
    performed <- !is.null(mSetObj$analSet$aov2$sig.mat);
  }else if(type == "multirf"){
    performed <- !is.null(mSetObj$analSet$rf.sigmat.mf);
  }else if(type == "cov"){
    performed <- !is.null(mSetObj$analSet$cov);
  }else if(type == "dose-de"){
    performed <- !is.null(mSetObj$dataSet$comp.res);
  }else if(type == "ebam"){
    performed <- !is.null(mSetObj$analSet$ebam.cmpds);
  }else if(type == "sam"){
    performed <- !is.null(mSetObj$analSet$sam.cmpds);
  }else if(type == "pca"){
    performed <- !is.null(mSetObj$analSet$pca$imp.loads);
  }else if(type == "spls.loadings"){
    performed <- !is.null(mSetObj$analSet$splsr$loadings);
  }else if(type %in% c("mr_results_merge", "exposure", "harmonized.dat", "outcome.dat")){
    performed <- !is.null(mSetObj$dataSet[type]);
  }else if(type == "qea"){
    performed <- !is.null(mSetObj$analSet$qea.mat);
  }else if(type == "ora"){
    performed <- !is.null(mSetObj$analSet$ora.mat);
  }else if(type == "match_integ"){
    performed <- !is.null(mSetObj$analSet$integ.match.tbl);
  }else if(endsWith(type, "_enr")){
    type_cleaned <- gsub("_enr", "", type);
    performed <- !is.null(mSetObj$imgSet$enrTables[[type_cleaned]]);
  }

  print(paste("checkPerformed=", type, "====",performed));

return(performed)
}

CheckMetaPerformed <- function(type){
  mSetObj <- .get.mSet(NA);

  if(type == "metap"){
    performed <- !is.null(mSetObj$analSet$metap.mat);
  }else if(type == "votecount"){
    performed <- !is.null(mSetObj$analSet$votecount.mat);

  }else if(type == "merge"){
    performed <- !is.null(mSetObj$analSet$merge.mat);

  }
return(performed);
}

CheckMumExists <- function(mSetObj=NA, type) {
  mSetObj <- .get.mSet(mSetObj)
  if (type == "mum") {    
    return(as.integer(!is.null(mSetObj$mummi.resmat)))
  } else if (type == "gsea") {
    return(as.integer(!is.null(mSetObj$mummi.gsea.resmat)))
  } else {
    return(as.integer(!is.null(mSetObj$integ.resmat)))
  }
}

GetViewData <- function(dataname){
  mSetObj <- .get.mSet(mSetObj);
    #fileName <- paste0(dataname,".csv")
 
 if (mSetObj[["analSet"]][["type"]]=="metadata") {
  message("Detected qs format")
  dat <- qs::qread(dataname)
   dat <- mSetObj$dataSet$data.orig
} else {
  message("Detected CSV format (or at least not qs)")
  dat <- data.table::fread(dataname, data.table = FALSE)
}       
        row.num <- nrow(dat);
        col.num <- ncol(dat);
        if(row.num > 100){
            row.num <- 100;
        }
        if(col.num > 10){
            col.num <- 10;
        }
        write.csv(dat[1:row.num, 1:col.num], file="raw_dataview.csv");
 
}

AddDoseFeatureToReport <- function(mSetObj=NA, id, imgName){
    mSetObj <- .get.mSet(mSetObj);
    if (is.null(mSetObj$imgSet$doseFeatureList)) {
      mSetObj$imgSet$doseFeatureList <- list()
    }
    mSetObj$imgSet$doseFeatureList[[id]] <- imgName; 
    return(.set.mSet(mSetObj));
}

AddFeatureToReport <- function(mSetObj=NA, id, imgName){
    mSetObj <- .get.mSet(mSetObj);
    if (is.null(mSetObj$imgSet$featureList)) {
      mSetObj$imgSet$featureList <- list()
    }
    mSetObj$imgSet$featureList[[id]] <- imgName; 
    return(.set.mSet(mSetObj));
}

LoadRHistory <- function(){
    load('Rload.RData');
    mSet <<- mSet;
    #if(file.exists('mSet.rda')){
    #   load('mSet.rda');
    #}
}

setResourceDir <- function(path){
    rpath <<- path;
    print(paste0("rpath====", path));
}

#reset memoise for volcano, workflow
ResetMemoise <- function(){
    rm(mem.tt);
    rm(mem.aov);
    rm(mem.univ);
    rm(mem.par);
}

GetMatchIntegColNames <-function(){
    mSetObj <- .get.mSet(mSetObj);
    tbl <- as.data.frame(mSetObj$analSet$integ.match.tbl);
    return(colnames(tbl)[-1]);
}

GetMatchIntegSetIDs <-function(inx){
    mSetObj <- .get.mSet(mSetObj);
    tbl <- as.data.frame(mSetObj$analSet$integ.match.tbl);
    return(unlist(tbl[,inx]));
}

GetMatchIntegNames<-function(){
    mSetObj <- .get.mSet(mSetObj);
    tbl <- as.data.frame(mSetObj$analSet$integ.match.tbl);

    return(unlist(tbl[,1]));
}

GetEnrQueryType <- function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
    res <- imgSet$enrTables[[type]]$library
    return(res);
}

GetEnrFunType <- function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
    res <- imgSet$enrTables[[type]]$library
    return(res);
}

GetEnrResSetNames<-function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
 res <- imgSet$enrTables[[type]]$table;
  if("Pathway" %in% colnames(res)){
  return(res$Pathway);
  }else if("Name" %in% colnames(res)){
  return(res$Name);
  }else{
    return(res[,1]);
  }
}

GetEnrResSetIDs<-function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
  res <- imgSet$enrTables[[type]]$table;
  return(res$IDs);
}

GetEnrResultColNames<-function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
    res <- imgSet$enrTables[[type]]$res.mat;
    colnames(res);
}

GetEnrResultMatrix <-function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
  res <- imgSet$enrTables[[type]]$res.mat
  res <- suppressWarnings(apply(res, 2, as.numeric)); # force to be all numeric
  return(signif(as.matrix(res), 5));
}

GetEnrResFileName<-function(type){
    mSetObj <- .get.mSet(mSetObj);
    imgSet <- mSetObj$imgSet;
  res <- imgSet$enrTables[[type]]$fileName
  return(res);
}