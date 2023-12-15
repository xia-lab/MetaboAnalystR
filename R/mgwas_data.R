##################################################
## R script for mGWAS-Explorer
## Description: Data/resource management functions
## Author: Le, le.chang@mail.mcgill.ca
###################################################

# internal variables and functions not to be modified by users
# This is only for web version 
.on.public.web <- TRUE; # only TRUE when on mgwas web server

# note, this is usually used at the end of a function
# for local, return itself; for web, push to global environment
.set.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    mSet <<- mSetObj;
    return (1);
  }
  return(mSetObj);
}

.get.mSet <- function(mSetObj=NA){
  if(.on.public.web){
    return(mSet)
  }else{
    return(mSetObj);
  }
}



# read binary qs files
# sub.dir is sub folder, leave NULL is under main lib folder
.get.my.lib <- function(filenm, sub.dir=NULL){
  
  if(!is.null(sub.dir)){
    sub.dir <- paste0(sub.dir, "/");
  }
  if(.on.public.web){
    lib.path <- paste0("../../libs/", sub.dir, filenm);
    print(paste("loading library:", lib.path));
    return(qs::qread(lib.path));
  }
  
  lib.download <- FALSE;
  if(!file.exists(filenm)){
    lib.download <- TRUE;
  }else{
    time <- file.info(filenm)
    diff_time <- difftime(Sys.time(), time[,"mtime"], units="days") 
    if(diff_time>30){
      lib.download <- TRUE;
    }
  }
  
  lib.url <- paste0("https://www.metaboanalyst.ca/resources/libs/", sub.dir, filenm);
  # Deal with curl issues
  if(lib.download){
    tryCatch(
      {
        download.file(lib.url, destfile=filenm, method="curl")
      }, warning = function(w){ print() },
      error = function(e) {
        print("Download unsucceful. Ensure that curl is downloaded on your computer.")
        print("Attempting to re-try download using libcurl...")
        download.file(lib.url, destfile=filenm, method="libcurl")
      }
    )
  }
  lib.path <- filenm;
  
  
  # Deal w. corrupt downloaded files
  tryCatch({
    my.lib <- qs::qread(lib.path); # this is a returned value, my.lib never called outside this function, should not be in global env.
    print("Loaded files from MetaboAnalyst web-server.")
  },
  warning = function(w) { print() },
  error = function(err) {
    print("Reading data unsuccessful, attempting to re-download file...")
    tryCatch({
      download.file(lib.url, destfile=filenm, method="curl")
      my.lib <- qs::qread(lib.path);
      print("Loaded necessary files.")
    },
    warning = function(w) { print() },
    error = function(err) {
      print("Loading files from server unsuccessful. Ensure curl is downloaded on your computer.")
    }
    )
  })
  return(my.lib);
}

#'Constructs a dataSet object for storing data 
#'@description This functions handles the construction of a mSetObj object for storing data for further processing and analysis.
#'It is necessary to utilize this function to specify to MetaboAnalystR the type of data and the type of analysis you will perform. 
#'@usage InitDataObjects(data.type, anal.type)
#'@param data.type The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data)
#'@param anal.type Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, 
#'cmpdmap, smpmap, or pathinteg
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import methods

InitDataObjects2 <- function(anal.type){
  
  if(!.on.public.web){
    if(exists("mSet")){
      mSetObj <- .get.mSet(mSet);
      mSetObj$analSet$type <- anal.type;
      return(.set.mSet(mSetObj));
    }
  }

  current.msg <<- NULL;

  dataSet <- list();
  analSet <- list();
  analSet$type <- anal.type;
  
  mSetObj <- list();
  mSetObj$dataSet <- dataSet;
  mSetObj$analSet <- analSet;
  mSetObj$imgSet <- list();
  mSetObj$msgSet <- list(); # store various message during data processing
  mSetObj$msgSet$msg.vec <- vector(mode="character");     # store error messages
  mSetObj$cmdSet <- vector(mode="character"); # store R command
  
  .init.global.vars(anal.type);
  print("mGWAS R objects initialized ...");
  return(.set.mSet(mSetObj));
}

SetAnalType <- function(analType){
  anal.type <<- analType;
}

SetupListData <- function(mSetObj=NA, mirs, idType, tissue, population){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$idType <- idType;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;

  mir.mat <- mir.mat[,-1, drop=F];
  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

SetupIndListData <- function(mSetObj=NA, mirs, inputType, idType, tissue, population){

  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$tissue <- tissue;
  mSetObj$dataSet$population <- population;
  current.msg <<- NULL;
  
  mir.mat <- .parseListData(mirs);
  mir.vec <- mir.mat[,1];
  
  rownames(mir.mat) <-  mir.vec;
  
  mir.mat <- mir.mat[,-1, drop=F];
  if(is.null(mSetObj$dataSet$data)){
    mSetObj$dataSet$data <- mSetObj$dataSet$id.types <- list();
  }
  mSetObj$dataSet$data[[inputType]] <- mir.mat;
  mSetObj$dataSet$id.types[[inputType]] <- idType;

  mSetObj$dataSet$mir.orig <- mir.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(mir.mat));
  }else{
    return (paste("A total of",  nrow(mir.mat), "unique items were entered."))
  }
}

# "ID", "Accession","Gene", "PMID"
GetResCol <- function(netType, colInx){
   #netType<<-netType;
   #colInx<<-colInx;
   #save.image("GetMirResCol.RData");
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;
  dataSet <- mSetObj$dataSet;
  if(netType == "manhattan" || analSet == "studyview" || analSet == "search"){
    res <- mSetObj$dataSet$snp2met[, colInx];
  } else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
    # mr_results_merge
    res <- dataSet[netType][[1]][, colInx];
  } else{
    res <- dataSet$mir.res[, colInx];
  }
    hit.inx <- is.na(res) | res == ""; # note, must use | for element-wise operation
    res[hit.inx] <- "N/A";
    return(res);
}

RemoveEntry <- function(mSetObj=NA, mir.id) {
  # mir.id<<-mir.id;
  # save.image("RemoveEntry.RData")
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  inx <- which(rownames(dataSet$mir.res) == mir.id);
  if(length(inx) > 0){
    mSetObj$dataSet$mir.res <- dataSet$mir.res[-inx,];
    .set.mSet(mSetObj);
  }
}

RemoveNode <- function(node.id) {
  # node ID is name of either gene or miRNA
  row.ids <- NULL;
  # first try mir names
  hits <- dataSet$mir.res[, 1] == node.id;
  if(sum(hits) > 0){
    row.ids <- rownames(dataSet$mir.res)[hits];
  }else{
    hits <- dataSet$mir.res[, 3] == node.id;
    if(sum(hits) > 0){
      row.ids <- rownames(dataSet$mir.res)[hits];
    }
  }
  if(is.null(row.ids)){
    return("NA");
  }else{
    dataSet$mir.res <<- dataSet$mir.res[!hits,];
    return(row.ids);
  }
}

# batch remove based on
UpdateEntries <- function(mSetObj=NA, col.id, method, value, action) {
  mSetObj <- .get.mSet(mSetObj);
  
  if(col.id == "name"){
    col <- mSetObj$dataSet$snp.res$Metabolite;
  }else if(col.id == "rsid"){
    col <- mSetObj$dataSet$snp.res$rsID;
  }else if(col.id == "chr"){
    col <- mSetObj$dataSet$snp.res$Chr;
  }else if(col.id == "p_value"){
    col <- mSetObj$dataSet$snp.res$`P-value`
  }else if(col.id == "consequence"){
    col <- mSetObj$dataSet$snp.res$Consequence;
  }else if(col.id == "symbol"){
    col <- mSetObj$dataSet$snp.res$Gene;
  }else if(col.id == "pmid"){
    col <- mSetObj$dataSet$snp.res$PMID;
  } else {
    print(paste("unknown column:", col.id));
  }
  
  if(method == "contain"){
    hits <- grepl(value, col, ignore.case = TRUE);
  }else if(method == "match"){
    hits <- tolower(col) %in% tolower(value);
  }else{ # at least
    if( col.id == "p_value"){
      col.val <- as.numeric(col);
      na.inx <- is.na(col.val);
      col.val[na.inx] <- max(col.val[!na.inx]);
      hits <- col.val > as.numeric(value);
    } else {
      print("This is only for P-value at this moment.");
      return("NA");
    }
  }
  
  if(action == "keep"){
    hits = !hits;
  }
  
  if(sum(hits) > 0){
    row.ids <- rownames(mSetObj$dataSet$snp.res)[hits];
    mSetObj$dataSet$snp.res <- mSetObj$dataSet$snp.res[!hits,];
    fast.write.csv(mSetObj$dataSet$snp.res, file="mgwas_snp_met.csv", row.names=FALSE);
    #.prepareIdeogramJSON(mSetObj$dataSet$snp.res);
    .set.mSet(mSetObj);
    return(row.ids);
  }else{
    return("NA");
  }
}

GetResRowNames <- function(netType){
  mSetObj <- .get.mSet(mSetObj);
  analSet <- mSetObj$analSet$type;
   #netType<<-netType
   #save.image("GetResRowNames.RData")
  if(netType == "manhattan" || analSet == "studyview"){
    resTable <- mSetObj$dataSet$snp2met_study; 
  }else if (analSet == "search"){
    resTable <- mSetObj$dataSet$snp2met_single;
  }else if (anal.type == "multilist"  || anal.type == "mrmodule" || anal.type == "mrbrowse") {
   resTable <- mSetObj$dataSet[netType][[1]]
  }  else{
    resTable <- dataSet$mir.res;
  }
  if(nrow(resTable) > 1000 & netType != "phe_mr_sig"){
    resTable <- resTable[1:1000, ];
    current.msg <<- "Due to computational constraints, only the top 1000 rows will be displayed.";
  }
  rownames(resTable);
}

getDataFromTextArea <- function(txtInput, sep.type="space"){
  
  lines <- unlist(strsplit(txtInput, "\r|\n|\r\n")[1]);
  if(substring(lines[1],1,1)=="#"){
    lines <- lines[-1];
  }
  
  # separated by tab 
  if(sep.type=="tab"){
    my.lists <- strsplit(lines, "\\t");
  }else{ # from any space
    my.lists <- strsplit(lines, "\\s+");
  }
  my.mat <- do.call(rbind, my.lists);
  
  if(dim(my.mat)[2] == 1){ # add 0
    my.mat <- cbind(my.mat, rep(0, nrow(my.mat)));
  }else if(dim(my.mat)[2] > 2){
    my.mat <- my.mat[,1:2];
    msg <- "More than two columns found in the list. Only first two columns will be used."
    AddErrMsg(msg);
  }
  rownames(my.mat) <- data.matrix(my.mat[,1]);
  my.mat <- my.mat[,-1, drop=F];
  return(my.mat);
}


AddErrMsg <- function(msg){
  err.vec <<- c(err.vec, msg);
  print(msg);
}

CrossReferencing <- function(mSetObj=NA, q.type, hmdb=T, pubchem=T, 
                             chebi=F, kegg=T, metlin=F, lipid=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # record the filter for 8 major databases
  mSetObj$return.cols <- c(hmdb, pubchem, chebi, kegg, metlin);
  mSetObj$lipid.feats <- lipid
  
  # record all the data
  if(!exists("name.map", where = mSetObj)){
    mSetObj$name.map <- list();
  }
  
  # distribute job
  mSetObj$dataSet$q.type <- q.type;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    MetaboliteMappingExact(mSetObj, q.type, lipid);
    mSetObj <- .get.mSet(mSetObj);
  }else{
    mSetObj <- MetaboliteMappingExact(mSetObj, q.type, lipid);
  }
  
  # do some sanity check
  todo.inx <- which(is.na(mSetObj$name.map$hit.inx));
  if(length(mSetObj$name.map$hit.inx) == 0){
    mSetObj$msgSet$nmcheck.msg <- c(0, "No hits found for the given compound ID. Please make 
                                    sure that correct compound IDs or common compound names are used.");
  }else if(length(todo.inx)/length(mSetObj$name.map$hit.inx) > 0.5){
    mSetObj$msgSet$nmcheck.msg <- c(0, "Over half of the compound IDs could not be matched to our database. Please make 
                                    sure that correct compound IDs or common compound names are used.");
  }else if (length(todo.inx) > 15){
    mSetObj$msgSet$nmcheck.msg <- c(2, "There are >15 compounds without matches. You can either proceed or if necessary, update these compound IDs and upload again.");        
  }else{
    mSetObj$msgSet$nmcheck.msg <- c(1, "Name matching OK, please inspect (and manual correct) the results then proceed.");   
  }
  
  if(!.on.public.web){
    print(mSetObj$msgSet$nmcheck.msg)
    
    if(length(todo.inx) == length(mSetObj$name.map$hit.inx)){
      AddErrMsg("Name matching failed! Please make sure that correct standardized feature names are used!")
      return(0)
    }
  }
  
  return(.set.mSet(mSetObj));
}

MetaboliteMappingExact <- function(mSetObj=NA, q.type, lipid = F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(lipid & anal.type == "msetqea"){
    qvec <- names(mSet$dataSet$url.var.nms);
  }else{
    qvec <- mSetObj$dataSet$cmpd;
  }
  
  # variables to record results
  hit.inx <- vector(mode='numeric', length=length(qvec)); # record hit index, initial 0
  names(hit.inx) <- qvec;
  match.values <- vector(mode='character', length=length(qvec)); # the best matched values (hit names), initial ""
  match.state <- vector(mode='numeric', length=length(qvec));  # match status - 0, no match; 1, exact match; initial 0 
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  if(q.type == "hmdb"){
    n <- 5 # Number of digits for V3 of HMDB
    hmdb.digits <- as.vector(sapply(cmpd.db$hmdb, function(x) strsplit(x, "HMDB", fixed=TRUE)[[1]][2]))
    hmdb.v3.ids <- paste0("HMDB", substr(hmdb.digits, nchar(hmdb.digits)-n+1, nchar(hmdb.digits)))
    hit.inx.v3 <- match(tolower(qvec), tolower(hmdb.v3.ids));
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx[is.na(hit.inx)] <- hit.inx.v3[is.na(hit.inx)]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "pubchem"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$pubchem));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "chebi"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$chebi));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "metlin"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$metlin));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
  }else if(q.type == "kegg"){
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$kegg));
    #hit.inx2 <- match(tolower(qvec), rev(tolower(cmpd.db$kegg)));
    
    # unique hits
    #nonuniq.hits <- hit.inx + hit.inx2 != nrow(cmpd.db) + 1;
    #hit.inx[nonuniq.hits] <- NA;
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }else if(q.type == "name"){
    # first find exact match to the common compound names
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$name));
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
    # then try to find exact match to synonyms for the remaining unmatched query names one by one
    if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
      syn.db <- .get.my.lib("lipid_syn_nms.qs")
    }else if(anal.type == "utils"){
      syn.db <- .get.my.lib("master_syn_nms.qs")
    }else{
      syn.db <- .get.my.lib("syn_nms.qs")
    }
    
    syns.list <-  syn.db$syns.list;
    todo.inx <- which(is.na(hit.inx));
    
    if(length(todo.inx) > 0) {
      for(i in 1:length(syns.list)){
        syns <-  syns.list[[i]];
        hitInx <- match(tolower(qvec[todo.inx]), tolower(syns));
        
        hitPos <- which(!is.na(hitInx));
        if(length(hitPos)>0){
          # record matched ones
          orig.inx<-todo.inx[hitPos];
          hit.inx[orig.inx] <- i;                  
          # match.values[orig.inx] <- syns[hitInx[hitPos]];  # show matched synnames
          match.values[orig.inx] <- cmpd.db$name[i];    # show common name
          match.state[orig.inx] <- 1;
          
          # update unmatched list
          todo.inx<-todo.inx[is.na(hitInx)];
        }
        if(length(todo.inx) == 0) break;
      }
    }
  } else {
    print(paste("Unknown compound ID type:", q.type));save(qvec, file = "qvec__checking.rda");save(cmpd.db, file = "cmpd.db__checking.rda")
    # guess a mix of kegg and hmdb ids
    
    n <- 5 # Number of digits for V3 of HMDB
    hmdb.digits <- as.vector(sapply(cmpd.db$hmdb, function(x) strsplit(x, "HMDB", fixed=TRUE)[[1]][2]))
    hmdb.v3.ids <- paste0("HMDB", substr(hmdb.digits, nchar(hmdb.digits)-n+1, nchar(hmdb.digits)))
    hit.inx.v3 <- match(tolower(qvec), tolower(hmdb.v3.ids));
    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx[is.na(hit.inx)] <- hit.inx.v3[is.na(hit.inx)]
    
    #    hit.inx <- match(tolower(qvec), tolower(cmpd.db$hmdb));
    hit.inx2 <- match(tolower(qvec), tolower(cmpd.db$kegg));
    nohmdbInx <- is.na(hit.inx);
    hit.inx[nohmdbInx]<-hit.inx2[nohmdbInx]
    match.values <- cmpd.db$name[hit.inx];
    match.state[!is.na(hit.inx)] <- 1;
    
  }
  # empty memory
  gc();
  
  mSetObj$name.map$query.vec <- qvec; 
  mSetObj$name.map$hit.inx <- hit.inx;
  mSetObj$name.map$hit.values <- match.values;
  mSetObj$name.map$match.state <- match.state;
  
  return(.set.mSet(mSetObj));
}


# Load RSQLite, necessary for network analysis
load_rsqlite <- function(){
  suppressMessages(library(RSQLite))
}

.get.sqlite.con <- function(sqlite.path){
  load_rsqlite();
  return(dbConnect(SQLite(), sqlite.path)); 
}

fast.write.csv <- function(dat, file, row.names=TRUE){
  tryCatch(
    {
      if(is.data.frame(dat)){
        # there is a rare bug in data.table (R 3.6) which kill the R process in some cases 
        data.table::fwrite(dat, file, row.names=row.names);
      }else{
        write.csv(dat, file, row.names=row.names);  
      }
    }, error=function(e){
      print(e);
      write.csv(dat, file, row.names=row.names);   
    }, warning=function(w){
      print(w);
      write.csv(dat, file, row.names=row.names); 
    });
}

# private method for all sqlite queries
.query.sqlite <- function(db.con, statement, offline=TRUE){
  rs <- dbSendQuery(db.con, statement);
  res <- fetch(rs, n=-1); # get all records
  dbClearResult(rs);
  if(offline){
    dbDisconnect(db.con);
  }
  cleanMem();
  return(res);
}

# general message only print when running local
AddMsg <- function(msg){
  msg.vec <<- c(msg.vec, msg);
  if(!.on.public.web){
    print(msg);
  }
}

.readDataTable <- function(fileName){
  
  dat <- tryCatch(
    {
      data.table::fread(fileName, header=TRUE, check.names=FALSE, blank.lines.skip=TRUE, data.table=FALSE);
    }, error=function(e){
      print(e);
      return(.my.slowreaders(fileName));    
    }, warning=function(w){
      print(w);
      return(.my.slowreaders(fileName));
    });
  
  if(any(dim(dat) == 0)){
    dat <- .my.slowreaders(fileName);
  }
  return(dat);
}

.my.slowreaders <- function(fileName){
  print("Using slower file reader ...");
  formatStr <- substr(fileName, nchar(fileName)-2, nchar(fileName))
  if(formatStr == "txt"){
    dat <- try(read.table(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
  }else{ # note, read.csv is more than read.table with sep=","
    dat <- try(read.csv(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
  }  
  return(dat);
}

ComputeColorGradient <- function(nd.vec, background="black", centered){
  #if(sum(nd.vec<0, na.rm=TRUE) > 0){
  #centered <- T;
  #}else{
  #centered <- F;
  #}
  color <- GetColorGradient(background, centered);
  breaks <- generate_breaks(nd.vec, length(color), center = centered);
  return(scale_vec_colours(nd.vec, col = color, breaks = breaks));
}

GetColorGradient <- function(background, center){
  if(background == "black"){
    if(center){
      return(c(colorRampPalette(c("#31A231", "#5BC85B", "#90EE90", "#C1FFC1"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)));
    }else{
      return(colorRampPalette(rev(heat.colors(9)))(100));
    }
  }else{ # white background
    if(center){
      return(c(colorRampPalette(c("#137B13", "#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313", "#96000D"))(50)));
    }else{
      # return(colorRampPalette(c("grey", "orange", "red", "darkred"))(100));
      # return(colorRampPalette(c("#80d0f0", rainbow(8, start=0.8, end=1)))(100));
      return(colorRampPalette(hsv(h = seq(0.72, 1, 0.035), s = 0.72, v = 1))(100));
    }
  }
}

generate_breaks = function(x, n, center = F){
  if(center){
    m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
    res = seq(-m, m, length.out = n + 1)
  }
  else{
    res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
  }
  return(res)
}

scale_vec_colours = function(x, col = rainbow(10), breaks = NA){
  breaks <- sort(unique(breaks));
  return(col[as.numeric(cut(x, breaks = breaks, include.lowest = T))])
}

# Load igraph, necessary for network analysis
load_igraph <- function(){
  suppressMessages(library(igraph))
}

doGeneIDMapping <- function(q.vec, org, type){
  #save.image("doGeneIDMapping.RData")
  
  sqlite.path <- paste0(url.pre, org, "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  
  if(type == "symbol"){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "symbol"]);
  }else if(type == "entrez" || type == "entrez2sbl" ){
    db.map = dbReadTable(con, "entrez")
    hit.inx <- match(q.vec, db.map[, "gene_id"]);
  }else{
    if(type == "genbank"){
      db.map = dbReadTable(con, "entrez_gb");
    }else if(type == "embl"){
      db.map = dbReadTable(con, "entrez_embl_gene");
    }else if(type == "refseq"){
      db.map = dbReadTable(con, "entrez_refseq");
      q.mat <- do.call(rbind, strsplit(q.vec, "\\."));
      q.vec <- q.mat[,1];
    }else if(type == "kos"){
      db.map = dbReadTable(con, "entrez_ortholog");
    }
    hit.inx <- match(q.vec, db.map[, "accession"]);
  }
  
  if(type=="entrez2sbl"){
    entrezs=db.map[hit.inx, "symbol"];
  }else{
    entrezs=db.map[hit.inx, "gene_id"];
  }
  
  rm(db.map, q.vec); gc();
  dbDisconnect(con);
  return(entrezs);
}

addEntrezID <- function(data){
  #save.image("addEntrezID.RData")
  
  sqlite.path <- paste0(url.pre, "hsa", "_genes.sqlite");
  con <- .get.sqlite.con(sqlite.path); 
  embl <- data;
  
  db.map = dbReadTable(con, "entrez_embl_gene");
  all.hits <- db.map$accession %in% embl$ensembl;
  my.map <- db.map[all.hits, ];
  dat <- merge(my.map, data, by.x="accession", by.y="ensembl");

    rm(db.map, embl); gc();
  dbDisconnect(con);
  return(dat);
}

# only keep alphabets, numbers, ",", "." "_", "-" "/"
# note, this may leads to duplicate names
CleanNames <- function(query, type){
  
  if(type=="sample_name"){
    query <- gsub("[^[:alnum:]./_-]", "", query);
  }else{
    query <- gsub("[^[:alnum:][:space:],'./_./@-]", "", query)
  }
  return(make.unique(query));
}

ClearStrings<-function(query){
  # kill multiple white space
  query <- gsub(" +"," ",query);
  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  return(query);
}

.init.global.vars <- function(anal.type){
  # other global variables
  msg.vec <<- "";
  err.vec <<- "";
  
  # for network analysis
  module.count <<- 0;
  # counter for naming different json file (pathway viewer)
  smpdbpw.count <<- 0; 
  # for mummichog
  peakFormat <<- "mpt";
  mumRT.type <<- "NA";
  
  # raw data processing
  rawfilenms.vec <<- vector();
  
  # for meta-analysis
  mdata.all <<- list(); 
  mdata.siggenes <<- vector("list");
  meta.selected <<- TRUE;
  anal.type <<- anal.type;
  
  if(.on.public.web){
    # disable parallel prcessing for public server
    library(BiocParallel);
    register(SerialParam());
  }else{
    if("stat" %in% anal.type | "msetqea" %in% anal.type | "pathqea" %in% anal.type | "roc" %in% anal.type)
      # start Rserve engine for Rpackage
      load_Rserve();
  }
  
  # plotting required by all
  Cairo::CairoFonts(regular="Arial:style=Regular",bold="Arial:style=Bold",italic="Arial:style=Italic",bolditalic = "Arial:style=Bold Italic",symbol = "Symbol")
  
  # sqlite db path for gene annotation
  if(file.exists("/home/glassfish/sqlite/")){ #.on.public.web at dev
    url.pre <<- "/home/glassfish/sqlite/";
    plink.path <<- "/home/glassfish/plink/"
  }else if(file.exists("/Users/xia/Dropbox/sqlite/")){ # xia local
    url.pre <<- "/Users/xia/Dropbox/sqlite/";
  }else if(file.exists("/Users/jeffxia/Dropbox/sqlite/")){ # xia local
    url.pre <<- "/Users/jeffxia/Dropbox/sqlite/";
  }else if(file.exists("/media/zzggyy/disk/sqlite/")){
    url.pre <<-"/media/zzggyy/disk/sqlite/"; #zgy local)
  }else if(file.exists("/home/zgy/sqlite/")){
    url.pre <<-"/home/zgy/sqlite/"; #zgy local)
    plink.path <<- "/home/zgy/plink/"
  } else if(file.exists("/home/le/sqlite/mgwas/")){# le local
    url.pre <<-"/home/le/sqlite/mgwas/oct_2021/";
    plink.path <<- "'/media/le/Seagate Portable Drive/ieugwasr/'"
  }else{
    print("Unknown environment! No clue of the resource location!");
  }
  api.base <<- "http://api.xialab.ca"
}

GetErrMsg<-function(){
  return(err.vec);
}

PerformCmpdMapping <- function(mSetObj=NA, cmpdIDs, idType, tissueType, population){
  
  mSetObj <- .get.mSet(mSetObj);
  org <- "hsa";
  mSetObj$dataSet$listData <- TRUE;
  mSetObj$dataSet$cmpd.orig <- cmpdIDs;
  mSetObj$dataSet$cmpd.org <- org;
  mSetObj$dataSet$tissue <- tissueType;
  mSetObj$dataSet$population <- population;
  mSetObj$dataSet$idType <- idType;
  current.msg <<- NULL;  
  if(idType == "name"){
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "tab");
  }else{ # all other id should not contains space
    cmpd.mat <- getDataFromTextArea(cmpdIDs, "space");
  }
  mSetObj$dataSet$cmpd <- rownames(cmpd.mat); # this is for compatibility with name_match function
  mSetObj$dataSet$cmpd.mat <- cmpd.mat;
  mSetObj$dataSet$mir.orig <- cmpd.mat;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return (nrow(cmpd.mat));
  }else{
    return (paste("A total of",  nrow(cmpd.mat), "unique items were entered."))
  }
  # if(.on.public.web){
  #   .set.mSet(mSetObj);  
  #   return(CrossReferencing(mSetObj, idType, hmdb=T, pubchem=T, chebi=F, kegg=T, metlin=F));
  # }
  # 
  # mSetObjCR <- CrossReferencing(mSetObj, idType, hmdb=T, pubchem=T, chebi=F, kegg=T, metlin=F)
  # 
  # return(.set.mSet(mSetObjCR));
}

SetMappingType <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mapType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Mapping options were entered!"))
  }
}

SetDbOpt <- function(){
  #save.image("SetDbOpt.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$dbOpt <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("Database options were entered!"))
  }
}

CreateMappingResultTable <- function(mSetObj=NA) {

  mSetObj <- .get.mSet(mSetObj);
  lipid <- mSetObj$lipid.feats;
  
  if(lipid & anal.type == "msetqea"){
    qvec <- names(mSet$dataSet$url.var.nms);
  }else{
    qvec <- mSetObj$dataSet$cmpd;
  }
  
  if(is.null(qvec)){
    return();
  }
  # style for highlighted background for unmatched names
  pre.style<-NULL;
  post.style<-NULL;
  
  # style for no matches
  if(mSetObj$dataSet$q.type == "name"){
    no.prestyle<-"<strong style=\"background-color:yellow; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }else{
    no.prestyle<-"<strong style=\"background-color:red; font-size=125%; color=\"black\">";
    no.poststyle<-"</strong>";
  }
  
  hit.inx<-mSetObj$name.map$hit.inx;
  hit.values<-mSetObj$name.map$hit.values;
  match.state<-mSetObj$name.map$match.state;
  
  # construct the result table with cells wrapped in html tags
  # the unmatched will be highlighted in different background
  html.res <- matrix("", nrow=length(qvec), ncol=8);
  csv.res <- matrix("", nrow=length(qvec), ncol=9);
  colnames(csv.res) <- c("Query", "Match", "HMDB", "PubChem", "ChEBI", "KEGG", "METLIN", "SMILES", "Comment");
  
  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }
  
  for (i in 1:length(qvec)){
    if(match.state[i]==1){
      pre.style<-"";
      post.style="";
    }else{ # no matches
      pre.style<-no.prestyle;
      post.style<-no.poststyle;
    }
    hit <-cmpd.db[hit.inx[i], ,drop=F];
    html.res[i, ]<-c(paste(pre.style, qvec[i], post.style, sep=""),
                     paste(ifelse(match.state[i]==0, "", hit.values[i]), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$hmdb_id) || hit$hmdb_id=="" || hit$hmdb_id=="NA","-", paste("<a href=http://www.hmdb.ca/metabolites/", hit$hmdb_id, " target='_blank'>",hit$hmdb_id,"</a>", sep="")),  sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$pubchem_id) || hit$pubchem_id=="" || hit$pubchem_id=="NA", "-", paste("<a href=http://pubchem.ncbi.nlm.nih.gov/summary/summary.cgi?cid=", hit$pubchem_id," target='_blank'>", hit$pubchem_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$chebi_id) || hit$chebi_id==""|| hit$chebi_id=="NA","-", paste("<a href=http://www.ebi.ac.uk/chebi/searchId.do?chebiId=", hit$chebi_id, " target='_blank'>",hit$chebi_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$kegg_id) || hit$kegg_id==""|| hit$kegg_id=="NA","-",paste("<a href=http://www.genome.jp/dbget-bin/www_bget?", hit$kegg_id, " target='_blank'>", hit$kegg_id,"</a>", sep="")), sep=""),
                     paste(ifelse(match.state[i]==0 || is.na(hit$metlin_id) || hit$metlin_id==""|| hit$metlin_id=="NA","-",paste("<a href=http://metlin.scripps.edu/metabo_info.php?molid=", hit$metlin_id," target='_blank'>",hit$metlin_id,"</a>", sep="")), sep=""),
                     ifelse(match.state[i]!=1,"View",""));
    csv.res[i, ]<-c(qvec[i],
                    ifelse(match.state[i]==0, "NA", hit.values[i]),
                    ifelse(match.state[i]==0, "NA", hit$hmdb_id),
                    ifelse(match.state[i]==0, "NA", hit$pubchem_id),
                    ifelse(match.state[i]==0, "NA", hit$chebi_id),
                    ifelse(match.state[i]==0, "NA", hit$kegg_id),
                    ifelse(match.state[i]==0, "NA", hit$metlin_id),
                    ifelse(match.state[i]==0, "NA", hit$smiles),
                    match.state[i]);
  }
  # return only columns user selected
  
  # add query and match columns at the the beginning, and 'Detail' at the end
  return.cols <- c(TRUE, TRUE, mSetObj$return.cols, TRUE);
  html.res <- html.res[,return.cols, drop=F];
  csv.res <- csv.res[,return.cols, drop=F];
  
  # store the value for report
  mSetObj$dataSet$map.table <- csv.res;
  fast.write.csv(csv.res, file="name_map.csv", row.names=F);
  
    if(.on.public.web){
    .set.mSet(mSetObj);
    return (nrow(csv.res));
  }else{
    return(.set.mSet(mSetObj));
  }
}

SetMRMethod <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$methodType <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("MR method options were entered!"))
  }
}

UpdateMREntries <- function(mSetObj=NA, col.id, method, value, action) {
  #col.id<<-col.id;
  #method<<-method;
  #value<<-value;
  #action<<-action;
  #save.image("UpdateMREntries.RData")
  
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(col.id == "name"){
    col <- mSetObj$dataSet$exposure$Metabolite;
  }else if(col.id == "rsid"){
    col <- mSetObj$dataSet$exposure$SNP;
  }else if(col.id == "chr"){
    col <- mSetObj$dataSet$exposure$Chr;
  }else if(col.id == "beta"){
    col <- mSetObj$dataSet$exposure$Beta
  }else if(col.id == "se"){
    col <- mSetObj$dataSet$exposure$SE;
  }else if(col.id == "pval"){
    col <- mSetObj$dataSet$exposure$`P-value`;
  }else if(col.id == "pmid"){
    col <- mSetObj$dataSet$exposure$PMID;
  } else {
    print(paste("unknown column:", col.id));
  }
  
  if(method == "contain"){
    hits <- grepl(value, col, ignore.case = TRUE);
  }else if(method == "match"){
    hits <- tolower(col) %in% tolower(value);
  }else{ # at least
    if( col.id == "pval"){
      col.val <- as.numeric(col);
      na.inx <- is.na(col.val);
      col.val[na.inx] <- max(col.val[!na.inx]);
      hits <- col.val > as.numeric(value);
    } else {
      print("This is only for P-value at this moment.");
      return("NA");
    }
  }
  
  if(action == "keep"){
    hits = !hits;
  }
  
  if(sum(hits) > 0){
    row.ids <- rownames(mSetObj$dataSet$exposure)[hits];
    mSetObj$dataSet$exposure <- mSetObj$dataSet$exposure[!hits,];
    fast.write.csv(mSetObj$dataSet$exposure, file="mr_exposure_data.csv", row.names=FALSE);
    .set.mSet(mSetObj);
    return(row.ids);
  }else{
    return("NA");
  }
}

