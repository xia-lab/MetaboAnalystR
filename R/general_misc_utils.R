#'Perform miscellaneous tasks
#'@description Perform misc tasks
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

#'merge duplicated columns or rows by their mean
#'@description dim 1 => row,  dim 2 => column
MergeDuplicates <- function(data, dim=2){
  
  if(is.null(dim(data))){ # a vector
    if(is.null(names(data))){
      print("Cannot detect duplicate data without names!!!");
      return();
    }
    nm.cls <- as.factor(names(data));
    uniq.len <- length(levels(nm.cls));
    if(uniq.len == length(data)){
      return(data);
    }
    new.data <- vector (mode="numeric",length=uniq.len);
    for(i in 1:uniq.len){
      dup.inx <- nm.cls == levels(nm.cls)[i];
      new.data[i] <- mean(data[dup.inx]);
    }
    names(new.data) <- levels(nm.cls);
    rem.len <- length(data) - length(new.data);
  }else{
    if(dim == 1){
      data <- t(data);
    }
    if(is.null(colnames(data))){
      print("Cannot detect duplicate data without var names!!!");
      return();
    }
    
    nm.cls <- as.factor(colnames(data));
    uniq.len <- length(levels(nm.cls));
    
    if(uniq.len == ncol(data)){
      if(dim == 1){
        data <- t(data);
      }
      return(data);
    }
    
    new.data <- matrix (nrow=nrow(data), ncol=uniq.len);
    for(i in 1:uniq.len){
      dup.inx <- which(nm.cls == levels(nm.cls)[i]);
      new.data[,i] <- apply(data[,dup.inx, drop=F], 1, mean);
    }
    rownames(new.data) <- rownames(data);
    colnames(new.data) <- levels(nm.cls);
    
    rem.len <- ncol(data) - ncol(new.data);
    if(dim == 1){
      new.data <- t(new.data);
    }
  }
  print(paste(rem.len, "duplicates are merged to their average"));
  new.data;
}

#'Given a data with duplicates, remove duplicates
#'@description Dups is the one with duplicates
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
RemoveDuplicates <- function(mSetObj=NA, data, lvlOpt="mean", quiet=T){
  
  mSetObj <- .get.mSet(mSetObj);
  
  all.nms <- rownames(data);
  colnms <- colnames(data);
  dup.inx <- duplicated(all.nms);
  dim.orig  <- dim(data);
  data <- apply(data, 2, as.numeric); # force to be all numeric
  dim(data) <- dim.orig; # keep dimension (will lost when only one item) 
  rownames(data) <- all.nms;
  colnames(data) <- colnms;
  if(sum(dup.inx) > 0){
    uniq.nms <- all.nms[!dup.inx];
    uniq.data <- data[!dup.inx,,drop=F];
    
    dup.nms <- all.nms[dup.inx];
    uniq.dupnms <- unique(dup.nms);
    uniq.duplen <- length(uniq.dupnms);
    
    for(i in 1:uniq.duplen){
      nm <- uniq.dupnms[i];
      hit.inx.all <- which(all.nms == nm);
      hit.inx.uniq <- which(uniq.nms == nm);
      
      # average the whole sub matrix 
      if(lvlOpt == "mean"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, mean, na.rm=T);
      }else if(lvlOpt == "median"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, median, na.rm=T);
      }else if(lvlOpt == "max"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, max, na.rm=T);
      }else{ # sum
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, sum, na.rm=T);
      }
    }
    if(!quiet){
      mSetObj$msgSet$current.msg <- paste(current.msg, paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""), collapse="\n");
      print(mSetObj$msgSet$current.msg)
    }
    return(uniq.data);
  }else{
    if(!quiet){
      mSetObj$msgSet$current.msg <- paste(current.msg, "All IDs are unique.", collapse="\n");
      print(mSetObj$msgSet$current.msg)
    }
    return(data);
  }
} 

#'Read data table
#'@description note, try to use the fread, however, it has issues with 
#'some windows 10 files "Line ending is \r\r\n. .... appears to add the extra \r in text mode on Windows"
#'in such as, use the slower read.table method
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
.readDataTable <- function(fileName){
  dat <- try(data.table::fread(fileName, header=TRUE, check.names=FALSE, data.table=FALSE));
  if(class(dat) == "try-error"){
    # try to use "tr" to remove double return characters
    trFileName <- paste("tr -d \'\\r\' <", fileName);
    dat <- try(data.table::fread(trFileName, header=TRUE, check.names=FALSE, data.table=FALSE));
    if(class(dat) == "try-error"){
      print("Using slower file reader ...");
      formatStr <- substr(fileName, nchar(fileName)-2, nchar(fileName))
      if(formatStr == "txt"){
        dat <- try(read.table(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
      }else{ # note, read.csv is more than read.table with sep=","
        dat <- try(read.csv(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
      }  
    }
  }
  return(dat);
}

#'Transform 2 column text to data matrix
#'@description Transform two column input text to data matrix (single column data frame)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
getDataFromTextInput <- function(mSetObj=NA, txtInput, sep.type="space"){
  
  mSetObj <- .get.mSet(mSetObj);
  
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
    mSetObj$msgSet$current.msg <- "More than two columns found in the list. Only first two columns will be used. ";
  }
  rownames(my.mat) <- data.matrix(my.mat[,1]);
  my.mat <- my.mat[,-1, drop=F];
  .set.mSet(mSetObj);
  return(my.mat);
}

#' need to check if necessary to include this code
#' use single core on the public server
#'Permutation
#'@description Perform permutation, options to change number of cores used
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage Perform.permutation(perm.num, fun)

Perform.permutation <- function(perm.num, fun){
  print(paste("performing", perm.num, "permutations ..."));
  #suppressMessages(library('multicore'));
  #core.num <- multicore:::detectCores();
  
  #if(core.num > 1){ # use two CPUs only, otherwise, the server will be unresponsive for other users
  #    perm.res <- mclapply(2:perm.num, fun, mc.cores =core.num-1);
  #}else{ # just regular
  perm.res <- lapply(2:perm.num, fun);
  #}
  perm.res;
}

`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}

#'Unzip .zip files
#'@description Unzips uploaded .zip files, removes the uploaded file, checks for success
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

UnzipUploadedFile<-function(mSetObj, inPath, outPath, rmFile=T){

  mSetObj <- .get.mSet(mSetObj); 
  
  a<-try(system(paste("unzip",  "-o", inPath, "-d", outPath), intern=T));
  if(class(a) == "try-error" | !length(a)>0){
    AddErrMsg(mSetObj, "Failed to unzip the uploaded files!");
    AddErrMsg(mSetObj, "Possible reason: file name contains space or special characters.");
    AddErrMsg(mSetObj, "Use only alphabets and numbers, make sure there is no space in your file name.");
    AddErrMsg(mSetObj, "For WinZip 12.x, use \"Legacy compression (Zip 2.0 compatible)\"");
    return (0);
  }
  if(rmFile){
    RemoveFile(inPath);
  }
  
  if(.on.public.web){
  return(1);
  }
  
  return(.set.mSet(mSetObj));
  
}


#'Calculate Fisher's Least Significant Difference (LSD)
#'@description Adapted from the 'agricolae' package
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

LSD.test <- function (y, trt, alpha = 0.05){
  clase<-c("aov","lm")
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    name.t <-names(A)[ipch]
    trt<-A[,ipch]
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
  sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
  nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change
  std.err <- sds[, 2]/sqrt(nn[, 2])
  Tprob <- qt(1 - alpha/2, DFerror)
  LCL <- means[,2]-Tprob*std.err
  UCL <- means[,2]+Tprob*std.err
  means <- data.frame(means, std.err, replication = nn[, 2], LCL, UCL)
  names(means)[1:2] <- c(name.t, name.y)
  #row.names(means) <- means[, 1]
  ntr <- nrow(means)
  nk <- choose(ntr, 2)
  nr <- unique(nn[, 2])
  
  comb <- combn(ntr, 2)
  nn <- ncol(comb)
  dif <- rep(0, nn)
  LCL1<-dif
  UCL1<-dif
  sig<-NULL
  pvalue <- rep(0, nn)
  for (k in 1:nn) {
    i <- comb[1, k]
    j <- comb[2, k]
    if (means[i, 2] < means[j, 2]){
      comb[1, k]<-j
      comb[2, k]<-i
    }
    dif[k] <- abs(means[i, 2] - means[j, 2])
    sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,4]))
    pvalue[k] <- 2 * (1 - pt(dif[k]/sdtdif, DFerror));
    pvalue[k] <- round(pvalue[k],6);
    LCL1[k] <- dif[k] - Tprob*sdtdif
    UCL1[k] <- dif[k] + Tprob*sdtdif
    sig[k]<-" "
    if (pvalue[k] <= 0.001) sig[k]<-"***"
    else  if (pvalue[k] <= 0.01) sig[k]<-"**"
    else  if (pvalue[k] <= 0.05) sig[k]<-"*"
    else  if (pvalue[k] <= 0.1) sig[k]<-"."
  }
  tr.i <- means[comb[1, ],1]
  tr.j <- means[comb[2, ],1]
  output<-data.frame("Difference" = dif, pvalue = pvalue,sig,LCL=LCL1,UCL=UCL1)
  rownames(output)<-paste(tr.i,tr.j,sep=" - ");
  output;
}

#'Perform data cleaning
#'@description Cleans data and removes -Inf, Inf, NA, negative and 0
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

CleanData <-function(bdata, removeNA=T, removeNeg=T, removeConst=T){
  
  if(sum(bdata==Inf, na.rm=TRUE)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- max(bdata, na.rm=T)*2
  }
  if(sum(bdata==-Inf, na.rm=TRUE)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- min(bdata, na.rm=T)/2
  }
  if(removeNA){
    if(sum(is.na(bdata))>0){
      bdata[is.na(bdata)] <- min(bdata, na.rm=T)/2
    }
  }
  if(removeNeg){
    if(sum(as.numeric(bdata<=0)) > 0){
      inx <- bdata <= 0;
      bdata[inx] <- NA;
      bdata[inx] <- min(bdata, na.rm=T)/2
    }
  }
  if(removeConst){
    varCol <- apply(data.frame(bdata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame
    constCol <- (varCol == 0 | is.na(varCol));
    constNum <- sum(constCol, na.rm=T);
    if(constNum > 0){
      bdata <- data.frame(bdata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
    }
  }
  bdata;
}

#'Replace infinite numbers
#'@description Replace -Inf, Inf to 99999 and -99999
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
#'
CleanNumber <-function(bdata){
  if(sum(bdata==Inf)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- 999999;
  }
  if(sum(bdata==-Inf)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- -999999;
  }
  bdata;
}

#'Remove spaces
#'@description Remove from, within, leading and trailing spaces
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ClearStrings<-function(query){
  # kill multiple white space
  query <- gsub(" +"," ",query);
  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  return (query);
}

#'Remove HTML tag
#'@description Removes html tag
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

PrepareLatex <- function(stringVec){
  stringVec <- gsub("<(.|\n)*?>","",stringVec);
  stringVec <- gsub("%", "\\\\%", stringVec);
  stringVec;
}

#'Determine value label for plotting
#'@description Concentration or intensity data type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetValueLabel<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$dataSet$type=="conc"){
    return("Concentration");
  }else {
    return("Intensity");
  }
}


#'Determine variable label for plotting
#'@description Determine data type, binned spectra, nmr peak, or ms peak
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
# 
GetVariableLabel<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(mSetObj$dataSet$type=="conc"){
    return("Compounds");
  }else if(mSetObj$dataSet$type=="specbin"){
    return("Spectra Bins");
  }else if(mSetObj$dataSet$type=="nmrpeak"){
    return("Peaks (ppm)");
  }else if(mSetObj$dataSet$type=="mspeak"){
    if(mSetObj$dataSet$peakSet$ncol==2){
      return("Peaks (mass)");
    }else{
      return("Peaks (mz/rt)");
    }
  }else{
    return("Peaks(mz/rt)");
  }
}

#'Determine row/column number for plotting
#'@description Determine the number of rows and columns for a given total
#'number of plots (used by Kmeans and SOM plots)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetXYCluster<-function(total){
  if(total>16){
    ncol<-4;
    nrow<-5;
  }else if(total>12){
    ncol<-4;
    nrow<-4;
  }else if(total>9){
    ncol<-3;
    nrow<-4;
  }else if(total>6){
    ncol<-3;
    nrow<-3;
  }else if(total>4){
    ncol<-2;
    nrow<-3;
  }else{
    ncol<-1;
    nrow<-total;
  }
  c(nrow, ncol);
}

#'Random subset of numbers
#'@description Obtain a random subset of numbers from a total number
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

GetRandomSubsetIndex<-function(total, sub.num = 50){
  if(total < sub.num){
    1:total;
  }else{
    sample(1:total, sub.num);
  }
}

Get.Accuracy <- function(cm) {
  sum(diag(cm)) / sum(cm);
}

#'Create Latex table
#'@description generate Latex table
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

GetSigTable<-function(mat, method, mSetObj=NA){
  suppressMessages(library(xtable));
  if(!isEmptyMatrix(mat)){ # test if empty
    cap<-"Important features identified by";
    if(nrow(mat)>50){
      smat<-as.matrix(mat[1:50,]); # only print top 50 if too many
      colnames(smat)<-colnames(mat); # make sure column names are also copied
      mat<-smat;
      cap<-"Top 50 features identified by";
    }
    # change the rowname to first column
    col1<-rownames(mat);
    cname<-colnames(mat);
    cname<-c(GetVariableLabel(mSetObj), cname);
    mat<-cbind(col1, mat);
    rownames(mat)<-NULL;
    colnames(mat)<-cname;
    print(xtable(mat, caption=paste(cap, method)), caption.placement="top", size="\\scriptsize");
  }else{
    print(paste("No significant features were found using the given threshold for", method));
  }
}

#'Sig table matrix is empty
#'@description test if a sig table matrix is empty
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

isEmptyMatrix<-function(mat){
  if(is.null(mat) | length(mat)==0){
    return(TRUE);
  }
  if(nrow(mat)==0 | ncol(mat)==0){
    return(TRUE);
  }
  if(is.na(mat[1,1])){
    return(TRUE);
  }
  return(FALSE);
}


#'Compute within group and between group sum of squares
#'(BSS/WSS) for each row of a matrix which may have NA
#'@description Columns have labels, x is a numeric vector,
#'cl is consecutive integers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.bwss<-function(x, cl){
  K <- max(cl) - min(cl) + 1
  tvar <- var.na(x);
  tn <- sum(!is.na(x));
  wvar <- wn <- numeric(K);
  
  for(i in (1:K)) {
    if(sum(cl == (i + min(cl) - 1)) == 1){
      wvar[i] <- 0;
      wn[i] <- 1;
    }
    
    if(sum(cl == (i + min(cl) - 1)) > 1) {
      wvar[i] <- var.na(x[cl == (i + min(cl) - 1)]);
      wn[i] <- sum(!is.na(x[cl == (i + min(cl) - 1)]));
    }
  }
  
  WSS <- sum.na(wvar * (wn - 1));
  TSS <- tvar * (tn - 1)
  (TSS - WSS)/WSS;
}

sum.na <- function(x,...){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 0)
    res <- sum(x[tmp])
  res
}


var.na <- function(x){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 1){
    res <- var(x[tmp])
  }
  res
}


tapply.stat <-function (y, x, stat = "mean"){
  cx<-deparse(substitute(x))
  cy<-deparse(substitute(y))
  x<-data.frame(c1=1,x)
  y<-data.frame(v1=1,y)
  nx<-ncol(x)
  ny<-ncol(y)
  namex <- names(x)
  namey <- names(y)
  if (nx==2) namex <- c("c1",cx)
  if (ny==2) namey <- c("v1",cy)
  namexy <- c(namex,namey)
  for(i in 1:nx) {
    x[,i]<-as.character(x[,i])
  }
  z<-NULL
  for(i in 1:nx) {
    z<-paste(z,x[,i],sep="&")
  }
  w<-NULL
  for(i in 1:ny) {
    m <-tapply(y[,i],z,stat)
    m<-as.matrix(m)
    w<-cbind(w,m)
  }
  nw<-nrow(w)
  c<-rownames(w)
  v<-rep("",nw*nx)
  dim(v)<-c(nw,nx)
  for(i in 1:nw) {
    for(j in 1:nx) {
      v[i,j]<-strsplit(c[i],"&")[[1]][j+1]
    }
  }
  rownames(w)<-NULL
  junto<-data.frame(v[,-1],w)
  junto<-junto[,-nx]
  names(junto)<-namexy[c(-1,-(nx+1))]
  return(junto)
}


ot.helmert <- function(k){
  
  if(missing(k)) stop("The number of time points is missing.")
  
  if (is.numeric(k) && length(k) == 1)
    if(k > trunc(k)) stop("The number of time points is not an integer.")
  
  
  levels <- 1:k
  
  T0 <- matrix(rep(1/sqrt(k), k), byrow=TRUE, ncol=k)
  
  T1 <- matrix(rep(0,(k-1)*k), ncol=k, byrow=TRUE)
  T1 <- array(1/sqrt(diag(outer(row(T1)[,1]+1, row(T1)[,1], "*"))),c(k-1,k))
  T1[col(T1) > row(T1)] <- 0
  T1[col(T1) == row(T1)+1] <- -(row(T1)[,1])/sqrt(diag(outer(row(T1)[,1]+1, row(T1)[,1], "*")))
  
  OT <- rbind(T0, T1)
  
  OT
}


#'List of objects
#'@description Improved list of objects
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
rectUnique <- function(m, order = seq(length = nrow(m)), xdiff = 0, ydiff = 0) {
  
  nr <- nrow(m)
  nc <- ncol(m)
  if (!is.double(m))
    m <- as.double(m)
  .C("RectUnique",
     m,
     as.integer(order-1),
     nr,
     nc,
     as.double(xdiff),
     as.double(ydiff),
     logical(nrow(m)),
     DUP = FALSE, PACKAGE = "xcms")[[7]]
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
findEqualGreaterM <- function(x, values) {
  
  if (!is.double(x)) x <- as.double(x)
  if (!is.double(values)) values <- as.double(values)
  .C("FindEqualGreaterM",
     x,
     length(x),
     values,
     length(values),
     index = integer(length(values)),
     DUP = FALSE, PACKAGE = "xcms")$index + 1
}

#'Perform utilities for peak grouping
#'@description Perform various utilities for peak grouping
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
descendMin <- function(y, istart = which.max(y)) {
  
  if (!is.double(y)) y <- as.double(y)
  unlist(.C("DescendMin",
            y,
            length(y),
            as.integer(istart-1),
            ilower = integer(1),
            iupper = integer(1),
            DUP = FALSE, PACKAGE = "xcms")[4:5]) + 1
}

#'Utilities for creating pathway maps for MetPA
#'@description Perform various utilities for MetPA
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
#'Function to deal with long string names
Wrap.Names<-function(cName, wrap.len=10, tol.len=5){
  
  nc <- nchar(cName);
  long.inx <- nc > (wrap.len+tol.len);
  long.nms <- cName[long.inx];
  
  # first get positions of the natural breaks space or hyphen
  pos.list <- gregexpr("[ -]", long.nms);
  
  for(i in 1:length(pos.list)){
    current.nm <- long.nms[i];
    pos <- pos.list[[i]]+1;
    start.pos<- c(0, pos);
    end.pos <- c(pos, nchar(current.nm)+1);
    splits <- sapply(1:(length(pos)+1), function(x) substring(current.nm, start.pos[x], end.pos[x]-1));
    long.nms[i]<-CheckMergeSplittedNames(splits);
  }
  
  cName[long.inx] <- long.nms;
  return (cName);
}

#'Utilities for creating pathway maps for MetPA
#'@description Perform various utilities for MetPA
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'given a vector with naturally splitted string elements
#'check if a particular element is too long and need to be
#'broken by brute force
CheckMergeSplittedNames <- function(nms, wrap.len=10, tol.len=5){
  clean.nm <- "";
  current.nm <- "";
  for(i in 1:length(nms)){
    current.nm <- paste(current.nm, nms[i], sep="");
    current.len <- nchar(current.nm);
    
    # if too long, break into halves
    if(current.len > wrap.len + tol.len){
      break.pt <- round(current.len/2);
      current.nm <- paste(substr(current.nm, 0, break.pt), "-", "\n",
                          substr(current.nm, break.pt+1, current.len),  sep="");
      clean.nm <- paste(clean.nm, "\n", current.nm, sep="");
      current.nm <- "";
    }else if(current.len > tol.len){
      clean.nm <- paste(clean.nm, "\n", current.nm, sep="");
      current.nm <- "";
    }else{
      if(i == length(nms)){
        clean.nm <- paste(clean.nm, current.nm, sep=ifelse(nchar(current.nm)<tol.len, "", "\n"));
      }
    }
  }
  return(clean.nm);
}

#'Utilities for creating pathway maps for MetPA
#'@description Perform various utilities for MetPA
#'break up a long name with brute force
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
BreakLongNames<-function(long.nm, wrap.len=10, tol.len=5){
  splits <- sapply(seq(1,nchar(long.nm),by=wrap.len), function(x) substr(long.nm, x, x+wrap.len-1));
  tot.len <- length(splits);
  pre.nms<- paste(splits[-tot.len], collapse="\n");
  last.nm <- splits[tot.len];
  if(nchar(last.nm) < tol.len){
    pre.nms<- paste(pre.nms, last.nm, sep="");
    last.nm <- "";
  }
  return(c(pre.nms,last.nm));
}

#'Perform utilities for MetPa
#'@description Return shorter names
#'break long names at space, append "..." to indicate the abbrev
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetShortNames<-function(nm.vec, max.len= 45){
  new.nms <- vector(mode="character", length=length(nm.vec));
  for(i in 1:length(nm.vec)){
    nm <- nm.vec[i];
    if(nchar(nm) <= max.len){
      new.nms[i] <- nm;
    }else{
      wrds <- strsplit(nm, "[[:space:]]+")[[1]];
      new.nm <- "";
      if(length(wrds)>1){
        for(m in 1:length(wrds)){
          wrd <- wrds[m];
          if(nchar(new.nm)+4+nchar(wrd) <= max.len){
            new.nm <- paste(new.nm, wrd);
          }else{
            new.nms[i] <- paste (new.nm, "...", sep="");
            break;
          }
        }
      }else{
        new.nms[i] <- paste (substr(nm, 0, 21), "...", sep="");
      }
    }
  }
  return(new.nms);
}


#'Perform utilities for MetPa
#'@description Get all the KEGG compounds from the pathway databases
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
getCmpdID<-function(dirName){
  library(KEGGgraph);
  folds<-dir(dirName);
  all.nms <- "";
  
  for(m in 1:length(folds)){
    files <- dir(paste(dirName, "/", folds[m], sep=""));
    cmpd.nms <- "";
    for(i in 1:length(files)){
      f <- paste(dirName, "/", folds[m],"/",files[i], sep="");
      print(f);
      g <- KEGGpathway2reactionGraph(parseKGML(f));
      nms <- nodes(g);
      start.pos <- unlist(gregexpr(":", nms))+1;
      nms <- substr(nms, start.pos, nchar(nms));
      cmpd.nms <- c(cmpd.nms, nms);
    }
    all.nms <- c(all.nms, unique(cmpd.nms));
  }
  write.csv(unique(all.nms), file="kegg_uniq.csv", row.names=F)
}

getPathName<-function(dirName, saveName){
  library(KEGGgraph);
  files<-dir(dirName);
  nm.mat<-matrix("NA", nrow=length(files), ncol=2);
  for(i in 1:length(files)){
    f <- files[i];
    print(f);
    path <- parseKGML(paste(dirName,"/",f, sep=""));
    nm.mat[i,]<-c(f, path@pathwayInfo@title);
  }
  write.csv(nm.mat, file=saveName);
}

#'Perform utilities for MetPa
#'@description Extends the axis range to both ends
#'vec is the values for that axis
#'unit is the width to extend, 10 will increase by 1/10 of the range
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetExtendRange<-function(vec, unit=10){
  var.max <- max(vec, na.rm=T);
  var.min <- min(vec, na.rm=T);
  exts <- (var.max - var.min)/unit;
  c(var.min-exts, var.max+exts);
}

#'Perform utilities for MetPa
#'@description Count the number of digits in the values
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
getndp <- function(x, tol=2*.Machine$double.eps){
  ndp <- 0
  while(!isTRUE(all.equal(x, round(x, ndp), tol=tol))) ndp <- ndp+1
  if(ndp > -log10(tol)) {
    warning("Tolerance reached, ndp possibly underestimated.")
  }
  ndp
}

#'Perform utilities for MetPa
#'@description Convert user coords (as used in current plot) to pixels in a png
#'adapted from the imagemap package
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
usr2png <- function(xy, im){
  xy <- usr2dev(xy,dev.cur())
  cbind(
    ceiling(xy[,1]*im$Width),
    ceiling((1-xy[,2])*im$Height)
  )
}

usr2plt <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  usr <- par("usr")
  dev.set(olddev)
  xytrans(xy,usr)
}

plt2fig <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  plt <- par("plt")
  dev.set(olddev)
  xytrans2(xy,plt)
}

fig2dev <- function(xy, dev=dev.cur()){
  olddev <- dev.cur()
  dev.set(dev)
  fig <- par("fig")
  dev.set(olddev)
  xytrans2(xy,fig)
}

usr2dev <- function(xy, dev=dev.cur()){
  fig2dev(plt2fig(usr2plt(xy,dev),dev),dev)
}

xytrans2 <- function(xy, par){
  cbind(par[1]+((par[2]-par[1])*xy[,1]),
        par[3]+((par[4]-par[3])*xy[,2]))
}

xytrans <- function(xy, par){
  cbind((xy[,1]-par[1])/(par[2]-par[1]),
        (xy[,2]-par[3])/(par[4]-par[3]))
}


getVennCounts <- function(x, include="both") {
  x <- as.matrix(x)
  include <- match.arg(include,c("both","up","down"))
  x <- sign(switch(include,
                   both = abs(x),
                   up = x > 0,
                   down = x < 0
  ))
  nprobes <- nrow(x)
  ncontrasts <- ncol(x)
  names <- colnames(x)
  if(is.null(names)) names <- paste("Group",1:ncontrasts)
  noutcomes <- 2^ncontrasts
  outcomes <- matrix(0,noutcomes,ncontrasts)
  colnames(outcomes) <- names
  for (j in 1:ncontrasts)
    outcomes[,j] <- rep(0:1,times=2^(j-1),each=2^(ncontrasts-j))
  xlist <- list()
  for (i in 1:ncontrasts) xlist[[i]] <- factor(x[,ncontrasts-i+1],levels=c(0,1))
  counts <- as.vector(table(xlist))
  structure(cbind(outcomes,Counts=counts),class="VennCounts")
}

#'Perform utilities for MetPa
#'@description function to calculate tick mark based on Heckbert algorithm
#'available in the "labeling" package implemented by Justin Talbot
#'adapted from the imagemap package
#'Heckbert's labeling algorithm
#'Heckbert, P. S. (1990) Nice numbers for graph labels, Graphics Gems I, Academic Press Professional, Inc.
#'@author Justin Talbot \email{jtalbot@@stanford.edu}

heckbert <- function(dmin, dmax, m)
{
  range <- .heckbert.nicenum((dmax-dmin), FALSE)
  lstep <- .heckbert.nicenum(range/(m-1), TRUE)
  lmin <- floor(dmin/lstep)*lstep
  lmax <- ceiling(dmax/lstep)*lstep
  seq(lmin, lmax, by=lstep)
}

.heckbert.nicenum <- function(x, round)
{
  e <- floor(log10(x))
  f <- x / (10^e)
  if(round)
  {
    if(f < 1.5) nf <- 1
    else if(f < 3) nf <- 2
    else if(f < 7) nf <- 5
    else nf <- 10
  }
  else
  {
    if(f <= 1) nf <- 1
    else if(f <= 2) nf <- 2
    else if(f <= 5) nf <- 5
    else nf <- 10
  }
  nf * (10^e)
}

#'Perform utilities for MetPa
#'@description borrowed from Hmisc
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
all.numeric <- function (x, what = c("test", "vector"), extras = c(".", "NA")){
  what <- match.arg(what)
  old <- options(warn = -1)
  on.exit(options(old));
  x <- sub("[[:space:]]+$", "", x);
  x <- sub("^[[:space:]]+", "", x);
  inx <- x %in% c("", extras);
  xs <- x[!inx];
  isnum <- !any(is.na(as.numeric(xs)))
  if (what == "test") 
    isnum
  else if (isnum) 
    as.numeric(x)
  else x
}

GetFileContentAsString <- function(file.nm){
  content <- paste(readLines(file.nm), collapse="\n");
  return(content);
}

ClearNumerics <-function(dat.mat){
  dat.mat[is.na(dat.mat)] <- -777;
  dat.mat[dat.mat == Inf] <- -999;
  dat.mat[dat.mat == -Inf] <- -111;
  dat.mat;
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Create semitransparant colors
#'@description Create semitransparant colors for a given class label
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

CreateSemiTransColors <- function(cls){
  
  # note, the first color (red) is for QC
  col.nms <- rainbow(length(levels(cls)));
  
  # convert to semi-transparent
  semi.nms <- ToSemiTransParent(col.nms);
  
  # now expand to the one-to-one match to cls element
  col.vec <- vector(mode="character", length=length(cls));
  for (i in 1:length(levels(cls))){
    lv <- levels(cls)[i];
    col.vec[cls==lv] <- semi.nms[i];
  }
  return(col.vec);
}

# convert rgb color i.e. "#00FF00FF" to semi transparent
ToSemiTransParent <- function (col.nms, alpha=0.5){
  rgb.mat <- t(col2rgb(col.nms));
  rgb(rgb.mat/255, alpha=alpha);
}

# col.vec should already been created
UpdateGraphSettings <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  grpnms <- GetGroupNames(mSetObj);
  names(colVec) <- grpnms;
  names(shapeVec) <- grpnms;
  colVec <<- colVec;
  shapeVec <<- shapeVec;
}

GetShapeSchema <- function(mSetObj=NA, show.name, grey.scale){
  mSetObj <- .get.mSet(mSetObj);
  if(exists("shapeVec") && all(shapeVec > 0)){
    sps <- rep(0, length=length(mSetObj$dataSet$cls));
    clsVec <- as.character(mSetObj$dataSet$cls)
    grpnms <- names(shapeVec);
    for(i in 1:length(grpnms)){
      sps[clsVec == grpnms[i]] <- shapeVec[i];
    }
    shapes <- sps;
  }else{
    if(show.name | grey.scale){
      shapes <- as.numeric(mSetObj$dataSet$cls)+1;
    }else{
      shapes <- rep(19, length(mSetObj$dataSet$cls));
    }
  }
  return(shapes);
}

GetColorSchema <- function(mSetObj=NA, grayscale=F){
  
  mSetObj <- .get.mSet(mSetObj);
  # test if total group number is over 9
  grp.num <- length(levels(mSetObj$dataSet$cls));
  
  if(grayscale){
    dist.cols <- colorRampPalette(c("grey90", "grey30"))(grp.num);
    lvs <- levels(mSetObj$dataSet$cls);
    colors <- vector(mode="character", length=length(mSetObj$dataSet$cls));
    for(i in 1:length(lvs)){
      colors[mSetObj$dataSet$cls == lvs[i]] <- dist.cols[i];
    }
  }else if(grp.num > 9){
    pal12 = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
              "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
              "#FFFF99", "#B15928");
    dist.cols <- colorRampPalette(pal12)(grp.num);
    lvs <- levels(mSetObj$dataSet$cls);
    colors <- vector(mode="character", length=length(mSetObj$dataSet$cls));
    for(i in 1:length(lvs)){
      colors[mSetObj$dataSet$cls == lvs[i]] <- dist.cols[i];
    }
  }else{
    if(exists("colVec") && !any(colVec =="#NA") ){
      cols <- vector(mode="character", length=length(mSetObj$dataSet$cls));
      clsVec <- as.character(mSetObj$dataSet$cls)
      grpnms <- names(colVec);
      for(i in 1:length(grpnms)){
        cols[clsVec == grpnms[i]] <- colVec[i];
      }
      colors <- cols;
    }else{
      colors <- as.numeric(mSetObj$dataSet$cls)+1;
    }
  }
  return (colors);
}

#'Remove folder
#'@description Remove folder
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
RemoveFolder<-function(folderName){
  # a<-unzip(inPath, exdir=outPath);
  a<-system(paste("rm",  "-r", folderName), intern=T);
  if(!length(a)>0){
    AddErrMsg(mSetObj, paste("Could not remove file -", folderName));
    return (0);
  }
  return(1);
}

#'Compute within group and between group sum of squares
#'(BSS/WSS) for each row of a matrix which may have NA
#'@description Columns have labels, x is a numeric vector,
#'cl is consecutive integers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.bwss<-function(x, cl){
  K <- max(cl) - min(cl) + 1
  tvar <- var.na(x);
  tn <- sum(!is.na(x));
  wvar <- wn <- numeric(K);
  
  for(i in (1:K)) {
    if(sum(cl == (i + min(cl) - 1)) == 1){
      wvar[i] <- 0;
      wn[i] <- 1;
    }
    
    if(sum(cl == (i + min(cl) - 1)) > 1) {
      wvar[i] <- var.na(x[cl == (i + min(cl) - 1)]);
      wn[i] <- sum(!is.na(x[cl == (i + min(cl) - 1)]));
    }
  }
  
  WSS <- sum.na(wvar * (wn - 1));
  TSS <- tvar * (tn - 1)
  (TSS - WSS)/WSS;
}

#'Remove file
#'@description Remove file
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
RemoveFile<-function(fileName){
  if(file.exists(fileName)){
    file.remove(fileName);
  }
}

#'Clear folder and memory
#'@description Clear the current folder and objects in memory
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
ClearUserDir<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # remove physical files
  unlink(dir(), recursive=T);
  mSetObj$dataSet <- mSetObj$analSet <- list();
  gc();
  return(.set.mSet(mSetObj));
}

#'Retrieve last command from the Rhistory.R file
#'@description Fetches the last command from the Rhistory.R file
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetCMD<-function(regexp){
  # store all lines into a list object
  all.lines<-readLines("Rhistory.R");
  
  all.matches<-grep(regexp, all.lines, value=T);
  if(length(all.matches)==0){
    return(NULL);
  }else{
    # only return the last command
    return(all.matches[length(all.matches)]);
  }
}

#'Subset data by p value (t-test)
#'@description Get a subsets of data ranked by their p values from t tests
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

GetTTSubsetIndex<-function(mSetObj=NA, sub.num=50){
  
  mSetObj <- .get.mSet(mSetObj);
  data <- mSetObj$dataSet$norm
  
  if(ncol(data) < sub.num){
    1:ncol(data);
  }else{
    if(is.null(mSetObj$analSet$tt)){
      Ttests.Anal(0.75);
    }
    all.lod <- -log10(mSetObj$analSet$tt$p.value);
    
    sub.inx <-order(all.lod, decreasing = T)[1:sub.num];
    sel.inx <- 1:ncol(data) %in% sub.inx;
    sel.inx;
  }
}

#'Compute sum of squares (SSQ) for each row of a matrix which may have NA
#'@description Columns have labels cl=consecutive integers
#'note: this is desgined for ASCA parition data
#'in which Within group (WSS) is zero, so, we only need TSS
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.tss<-function(x, cl){
  
  K <- max(cl) - min(cl) + 1
  tvar <- apply(x, 1, var.na);
  tn <- apply(!is.na(x), 1, sum);
  wvar <- matrix(0, nrow(x), K);
  wn <- matrix(0, nrow(x), K);
  
  for(i in (1:K)) {
    if(sum(cl == (i + min(cl) - 1)) == 1){
      wvar[, i] <- 0;
      wn[, i] <- 1;
    }
    
    if(sum(cl == (i + min(cl) - 1)) > 1) {
      wvar[, i] <- apply(x[, cl == (i + min(cl) - 1)], 1, var.na);
      wn[, i] <- apply(!is.na(x[, cl == (i + min(cl) - 1)]), 1, sum);
    }
  }
  
  WSS <- apply(wvar * (wn - 1), 1, sum.na)
  TSS <- tvar * (tn - 1)
  
  return(TSS);
}


#'Memory functions
ShowMemoryUse <- function(..., n=20) {
  print(.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n));
  print(warnings());
}

CleanMemory <- function(){
  for (i in 1:10){ 
    gc(reset = T);
  }
}

#'Perform utilities for cropping images
#'@description Obtain the full path to convert (from imagemagik)
#'for cropping images
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetConvertFullPath<-function(){
  path <- system("which convert", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find convert in the PATH!");
    return("NA");
  }
  return(path);
}

# need to obtain the full path to convert (from imagemagik) for cropping images
GetBashFullPath<-function(){
  path <- system("which bash", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find bash in the PATH!");
    return("NA");
  }
  return(path);
}

GetPdfLatexFullPath<-function(){
  path <- system("which pdflatex", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find pdflatex in the PATH!");
    return("NA");
  }
  return(path);
}

GetRandomNumbers <- function(){
  rm(.Random.seed);
  runif(1);
  return(.Random.seed[3:626]);
}

GetRScriptFullPath<-function(){
  path <- system("which RScript", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find pdflatex in the PATH!");
    return("NA");
  }
  return(path);
}

#' Matrix creation for MetaboAnalyst
#' @description Converts processed raw lc/ms data from XCMS 
#' to a suitable matrix for use in MetaboAnalyst
#' @param xset The xcmsSet object created 

MetaboAnalystMatrix <- function(xset){
  library(xcms)
  data <- groupval(xset, "medret", "into")
  data2 <- rbind(class= as.character(phenoData(xset)$class), data)
  rownames(data2) <- c("group", paste(round(groups(xset)[,"mzmed"], 3), round(groups(xset)[,"rtmed"]/60, 1), sep="/"))
  write.csv(data2, file="PeakTable.csv")
  print("Matrix successfully created...")
}