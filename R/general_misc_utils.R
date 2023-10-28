### Perform miscellaneous tasks
### Perform misc tasks
### Jeff Xia\email{jeff.xia@mcgill.ca}
### McGill University, Canada
###License: GNU GPL (>= 2)

# Limit of detection (1/5 of min for each var)
.replace.by.lod <- function(x){
    lod <- min(x[x>0], na.rm=T)/5;
    x[x==0|is.na(x)] <- lod;
    return(x);
}

ReplaceMissingByLoD <- function(int.mat){
    int.mat <- as.matrix(int.mat);

    rowNms <- rownames(int.mat);
    colNms <- colnames(int.mat);
    int.mat <- apply(int.mat, 2, .replace.by.lod);
    rownames(int.mat) <- rowNms;
    colnames(int.mat) <- colNms;
    return (int.mat);
}

#'Given a data with duplicates, remove duplicates
#'@description Dups is the one with duplicates
#'@param data Input data to remove duplicates
#'@param lvlOpt Set options, default is mean
#'@param quiet Set to quiet, logical, default is T
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RemoveDuplicates <- function(data, lvlOpt="mean", quiet=T){
  
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
    AddMsg(paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""));
    return(uniq.data);
  }else{
    AddMsg("All IDs are unique.");
    return(data);
  }
} 

# in public web, this is done by microservice
.perform.computing <- function(){
  dat.in <- qs::qread("dat.in.qs");
  dat.in$my.res <- dat.in$my.fun();
  qs::qsave(dat.in, file="dat.in.qs");  
}

#' Read data table
#' @description Function to read in a data table. First, it will try to use fread, however, it has issues with 
#' some windows 10 files. In such case, use the slower read.table method.
#' @param fileName Input filename
#' @author Jeff Xia\email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @importFrom data.table fread
#' @export

.readDataTable <- function(fileName){

  dat <- tryCatch(
            {
                data.table::fread(fileName, header=TRUE, check.names=FALSE, 
                                  blank.lines.skip=TRUE, data.table=FALSE, integer64 = "numeric");
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

  # need to remove potential empty columns !! NOTE the single |, not || which is for atomic operation
  dat <- dat[!sapply(dat, function(x) all(x == "" | is.na(x)))];

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

.get.sqlite.con <- function(sqlite.path){
    load_rsqlite();
    return(dbConnect(SQLite(), sqlite.path)); 
}

#'Transform two column text to data matrix
#'@description Transform two column input text to data matrix (single column data frame)
#'@param txtInput Input text
#'@param sep.type Indicate the seperator type for input text. Default set to "space"
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
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

#'Permutation
#'@description Perform permutation, options to change number of cores used
#'@param perm.num Numeric, input the number of permutations to perform
#'@param fun Dummy function
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage Perform.permutation(perm.num, fun)
#'@export
#'
Perform.permutation <- function(perm.num, fun){
 
  # for public server, perm.num is not always followed to make sure loop will not continue for very long time
  # before the adventure, see how long it takes for 10 permutations
  # if it is extremely slow (>60 sec) => max 20 (<0.05)
  # if it is very slow (30-60 sec) => max 100 (<0.01)

  start.num <- 1; 
  perm.res <- NULL;
  if(.on.public.web & perm.num > 20){
    start.time <- Sys.time();
    perm.res <- lapply(1:10, fun);
    end.time <- Sys.time();

    time.taken <- end.time - start.time;
    print(paste("time taken for 10 permutations: ", time.taken));

    if(time.taken > 60){
        perm.num <- 20;
    }else if(time.taken > 30){
        perm.num <- 100;
    }
    start.num <- 11;
  }
  print(paste("performing", perm.num, "permutations ..."));
  perm.res <- c(perm.res, lapply(start.num:perm.num, fun));
  return(list(perm.res=perm.res, perm.num = perm.num));
}

#'Unzip .zip files
#'@description Unzips uploaded .zip files, removes the uploaded file, checks for success
#'@param inPath Input the path of the zipped files
#'@param outPath Input the path to directory where the unzipped files will be deposited
#'@param rmFile Logical, input whether or not to remove files. Default set to T
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
UnzipUploadedFile<-function(inPath, outPath, rmFile=T){
  sys.cmd <- paste("unzip",  "-o", inPath, "-d", outPath);
  #print(sys.cmd);
  sys.res <- try(system(sys.cmd));
  if(sys.res == 0){ # success code for system call
     return (1);
  }else{  # success code for system call
     print(sys.res);
     r.res <- unzip(inPath, exdir=outPath);
     return(length(r.res)>0);
  }
}

#'Perform data cleaning
#'@description Cleans data and removes -Inf, Inf, NA, negative and 0s.
#'@param bdata Input data to clean
#'@param removeNA Logical, T to remove NAs, F to not. 
#'@param removeNeg Logical, T to remove negative numbers, F to not. 
#'@param removeConst Logical, T to remove samples/features with 0s, F to not. 
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
      bdata <- data.frame(bdata[,!constCol, drop=FALSE], check.names = F); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
    }
  }
  bdata;
}

#'Replace infinite numbers
#'@description Replace -Inf, Inf to 99999 and -99999
#'@param bdata Input matrix to clean numbers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
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


# replace space with underscore
# only keep alphabets, numbers, "." "_", "-" and @
CleanNames <- function(query){
  query <- gsub(" +","_",query);
  query <- gsub("[^[:alnum:].@_-]", "", query);
  return(make.unique(query));
}

#'Remove spaces
#'@description Remove from, within, leading and trailing spaces
#'@param query Input the query to clear
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ClearStrings<-function(query){
  # kill multiple white space
  query <- gsub(" +"," ",query);

  # black slash escape sign could kill Rserve immediately
  query <- gsub("\\\\", "-", query);

  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  return(query);
}

# Remove HTML tag
PrepareLatex <- function(stringVec){
  stringVec <- gsub("<(.|\n)*?>","",stringVec);
  stringVec <- gsub("%", "\\\\%", stringVec);
  stringVec;
}

#'Determine value label for plotting
#'@description Concentration or intensity data type
#'@param data.type Input concentration or intensity data
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetAbundanceLabel<-function(data.type){
  if(data.type=="conc"){
    return("Concentration");
  }else {
    return("Intensity");
  }
}

#'Determine variable label for plotting
#'@description Determine data type, binned spectra, nmr peak, or ms peak
#'@param data.type Input the data type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetVariableLabel<-function(data.type){
  if(data.type=="conc"){
    return("Compounds");
  }else if(data.type=="specbin"){
    return("Spectra Bins");
  }else if(data.type=="nmrpeak"){
    return("Peaks (ppm)");
  }else if(data.type=="mspeak"){
    return("Peaks (mass)");
  }else{
    return("Peaks(mz/rt)");
  }
}

#'Create Latex table
#'@description generate Latex table
#'@param mat Input matrix
#'@param method Input method to create table
#'@param data.type Input the data type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

GetSigTable<-function(mat, method, data.type){
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
    cname<-c(GetVariableLabel(data.type), cname);
    mat<-cbind(col1, mat);
    rownames(mat)<-NULL;
    colnames(mat)<-cname;
    print(xtable::xtable(mat, caption=paste(cap, method)), caption.placement="top", size="\\scriptsize");
  }else{
    print(paste("No significant features were found using the given threshold for", method));
  }
}

#'Sig table matrix is empty
#'@description Test if a sig table matrix is empty
#'@param mat Matrix to test if empty
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

isEmptyMatrix <- function(mat){
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

# from color names or code 1, 2, 3 the rbg strings
my.col2rgb <- function(cols){
  rgbcols <- col2rgb(cols);
  return(apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")}));
}

my.col2rgba <- function(cols, alpha){
  rgbcols <- col2rgb(cols);
  rgbcols <- rbind(rgbcols, alpha);
  return(as.vector(apply(rgbcols, 2, function(x){paste("rgba(", paste(x, collapse=","), ")", sep="")})));
}

# #FFFFFF to rgb(1, 0, 0)
hex2rgba <- function(cols, alpha=0.8){
  my.cols <- apply(sapply(cols, col2rgb), 2, function(x){paste("rgba(", x[1], ",", x[2], ",", x[3], ",",  alpha, ")", sep="")});
  return(as.vector(my.cols));
}

hex2rgb <- function(cols){
  return(apply(sapply(cols, col2rgb), 2, function(x){paste("rgb(", x[1], ",", x[2], ",", x[3], ")", sep="")}));
}

# List of objects
# Improved list of objects
# Jeff Xia\email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)

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
  mSetObj <- .get.mSet(mSetObj);
  print(lapply(mSetObj$dataSet, object.size));
  print(lapply(mSetObj$analSet, object.size));

  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#'Extend axis
#'@description Extends the axis range to both ends
#'vec is the values for that axis
#'unit is the width to extend, 10 will increase by 1/10 of the range
#'@param vec Input the vector
#'@param unit Numeric
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetExtendRange<-function(vec, unit=10){
  var.max <- max(vec, na.rm=T);
  var.min <- min(vec, na.rm=T);
  exts <- (var.max - var.min)/unit;
  c(var.min-exts, var.max+exts);
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

# Perform utilities for MetPa
# borrowed from Hmisc
# Jeff Xia\email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
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

ClearNumerics <-function(dat.mat){
  dat.mat[is.na(dat.mat)] <- -777;
  dat.mat[dat.mat == Inf] <- -999;
  dat.mat[dat.mat == -Inf] <- -111;
  dat.mat;
}

#'Calculate Pairwise Differences
#'@description Mat are log normalized, diff will be ratio. Used in higher functions. 
#'@param mat Input matrix of data to calculate pair-wise differences.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CalculatePairwiseDiff <- function(mat){
  f <- function(i, mat) {
    z <- mat[, i-1] - mat[, i:ncol(mat), drop = FALSE]
    colnames(z) <- paste(colnames(mat)[i-1], colnames(z), sep = "/")
    z
  }
  res <- do.call("cbind", sapply(2:ncol(mat), f, mat));
  round(res,5);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Update graph settings
#'@description Function to update the graph settings.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param colVec colVec
#'@param shapeVec shapeVec
#'@export
# col.vec should already been created
UpdateGraphSettings <- function(mSetObj=NA, colVec, shapeVec){
    mSetObj <- .get.mSet(mSetObj);
    grpnms <- levels(current.cls);

    # default styles
    grp.num <- length(grpnms);
    if(grp.num <= 18){ 
       cols <- pal_18[1:grp.num];
    }else{
       cols <- colorRampPalette(pal_18)(grp.num);
    }

    # make sure the NA
    na.inx <- colVec == "#NA";
    colVec[na.inx] <- cols[na.inx];
    shapeVec[shapeVec == 0] <- 21;

    names(colVec) <- grpnms;
    names(shapeVec) <- grpnms;

    colVec <<- colVec;
    shapeVec <<- shapeVec;
    return(.set.mSet(mSetObj));
}

GetShapeSchema <- function(mSetObj=NA, show.name, grey.scale){
  mSetObj <- .get.mSet(mSetObj);
  if(exists("shapeVec") && all(shapeVec >= 0)){
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
      shapes <- rep(21, length(mSetObj$dataSet$cls));
    }
  }
  return(shapes);
}

pal_18 <- c("#e6194B", "#3cb44b", "#4363d8", "#42d4f4", "#f032e6", "#ffe119", "#911eb4", "#f58231", "#bfef45",
                  "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075");
cb_pal_18 <- c("#E69F00", "#b12c6f", "#56B4E9", "#009E73", "#F0E442", "#004488", 
                     "#D55E00", "#EE6677", "#CCBB44", "#A95AA1", "#DCB69F", "#661100", 
                     "#63ACBE", "#332288", "#EE7733", "#EE3377", "#0072B2", "#999933");

# return a gradient color vec based on value 
GetRGBColorGradient <- function(vals){
    library(RColorBrewer);
    #seed.cols <- brewer.pal(3, "YlOrRd");
    #seed.cols <- brewer.pal(9, "Oranges")[c(2,5,7)]
    seed.cols <- c("#FCF5DF", "#FFEDA0", "#F03B20")
    cols <- colorRampPalette(seed.cols)(length(vals));

    # set alpha for 
    my.alpha <- signif(seq(from=0.3, to=0.8, length.out=length(vals)),2);
    rgb.cols <- my.col2rgba(cols, alpha=my.alpha);

    # now need make sure values and colors are matched using names
    nms.orig <- names(vals);
    names(rgb.cols) <- names(sort(vals));
    ord.cols <- rgb.cols[nms.orig];
    return(as.vector(ord.cols)); # note remove names
}


GetSizeGradient <- function(vals){

    my.sizes <- round(seq(from=4, to=6, length.out=length(vals)));

    # now need make sure values and colors are matched using names
    nms.orig <- names(vals);
    names(my.sizes) <- names(sort(vals));
    ord.sizes <- my.sizes[nms.orig];
    return(as.vector(ord.sizes)); # note remove names
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

GetColorSchema <- function(my.cls, grayscale=F){
  
   lvs <- levels(my.cls); 
   grp.num <- length(lvs);
   if(grayscale){
      dist.cols <- colorRampPalette(c("grey90", "grey30"))(grp.num);
   }else if(exists("colVec") && !any(colVec =="#NA") && length(colVec) == length(levels(my.cls))){
      dist.cols <- colVec;
   }else{             
      if(grp.num <= 18){ # update color and respect default
          dist.cols <- pal_18[1:grp.num];
      }else{
          dist.cols <- colorRampPalette(pal_18)(grp.num);
      }
   }

   colors <- vector(mode="character", length=length(my.cls));
   for(i in 1:length(lvs)){
     colors[my.cls == lvs[i]] <- dist.cols[i];
   }
   return (colors);
}

#'Remove folder
#'@description Remove folder
#'@param folderName Input name of folder to remove
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
RemoveFolder<-function(folderName){
  a<-system(paste("rm",  "-r", folderName), intern=T);
  if(!length(a)>0){
    AddErrMsg(paste("Could not remove file -", folderName));
    return (0);
  }
  return(1);
}

#'Remove file
#'@description Remove file
#'@param fileName Input name of file to remove
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
RemoveFile<-function(fileName){
  if(file.exists(fileName)){
    file.remove(fileName);
  }
}

# do memory cleaning after removing many objects
cleanMem <- function(n=10) { for (i in 1:n) gc() }

#'Retrieve last command from the Rhistory.R file
#'@description Fetches the last command from the Rhistory.R file
#'@param regexp Retrieve last command from Rhistory file
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

# Memory functions
ShowMemoryUse <- function(..., n=40) {
    library(pryr);
    sink(); # make sure print to screen
    print(mem_used());
    print(sessionInfo());
    print(.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n));
    print(warnings());
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

#'Converts xset object from XCMS to mSet object for MetaboAnalyst
#'@description This function converts processed raw LC/MS data from XCMS 
#'to a usable data object (mSet) for MetaboAnalyst. The immediate next step following using this 
#'function is to perform a SanityCheck, and then further data processing and analysis can continue.
#'@param xset The name of the xcmsSet object created.
#'@param dataType The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data).
#'@param analType Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, mf, 
#'cmpdmap, smpmap, or pathinteg.
#'@param paired Logical, is data paired (T) or not (F).
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@param lbl.type Specify the data label type, either categorical (disc) or continuous (cont).
#'@export

XSet2MSet <- function(xset, dataType, analType, paired=F, format, lbl.type){
  
  # data <- xcms::groupval(xset, "medret", "into");
  # data2 <- rbind(class= as.character(phenoData(xset)$class), data);
  # rownames(data2) <- c("group", paste(round(groups(xset)[,"mzmed"], 3), round(groups(xset)[,"rtmed"]/60, 1), sep="/"));
  # fast.write.csv(data2, file="PeakTable.csv");
  # mSet <- InitDataObjects("dataType", "analType", paired)
  # mSet <- Read.TextData(mSet, "PeakTable.csv", "format", "lbl.type")
  # print("mSet successfully created...")
  # return(.set.mSet(mSetObj));
}

#'Get fisher p-values
#'@param numSigMembers Number of significant members
#'@param numSigAll Number of all significant features
#'@param numMembers Number of members
#'@param numAllMembers Number of all members
#'@export
GetFisherPvalue <- function(numSigMembers, numSigAll, numMembers, numAllMembers){
  z <- cbind(numSigMembers, numSigAll-numSigMembers, numMembers-numSigMembers, numAllMembers-numMembers-numSigAll+numSigMembers);
  z <- lapply(split(z, 1:nrow(z)), matrix, ncol=2);
  z <- lapply(z, fisher.test, alternative = 'greater');
  p.values <- as.numeric(unlist(lapply(z, "[[", "p.value"), use.names=FALSE));
  return(p.values);
}

saveNetworkInSIF <- function(network, name){
  edges <- .graph.sif(network=network, file=name);
  sif.nm <- paste(name, ".sif", sep="");
  if(length(list.edge.attributes(network))!=0){
    edge.nms <- .graph.eda(network=network, file=name, edgelist.names=edges);
    sif.nm <- c(sif.nm, edge.nms);
  }
  if(length(list.vertex.attributes(network))!=0){
    node.nms <- .graph.noa(network=network, file=name);
    sif.nm <- c(sif.nm, node.nms);
  }
  # need to save all sif and associated attribute files into a zip file for download
  zip(paste(name,"_sif",".zip", sep=""), sif.nm);
}

.graph.sif <- function(network, file){
  edgelist.names <- igraph::get.edgelist(network, names=TRUE)
  edgelist.names <- cbind(edgelist.names[,1], rep("pp", length(E(network))), edgelist.names[,2]);
  write.table(edgelist.names, row.names=FALSE, col.names=FALSE, file=paste(file, ".sif", sep=""), sep="\t", quote=FALSE)
  return(edgelist.names) 
}

# internal method to write cytoscape node attribute files
.graph.noa <- function(network, file){
  all.nms <- c();
  attrib <- list.vertex.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    noa <- cbind(V(network)$name, rep("=", length(V(network))), get.vertex.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="")
    file.nm <- paste(file, "_", attrib[i], ".NA", sep="");
    write(first.line, file=file.nm, ncolumns = 1, append=FALSE, sep=" ")
    write.table(noa, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

# internal method to write cytoscape edge attribute files
.graph.eda <- function(network, file, edgelist.names){
  all.nms <- c();
  attrib <- list.edge.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.edge.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    eda <- cbind(cbind(edgelist.names[,1], rep("(pp)", length(E(network))), edgelist.names[,3]), rep("=", length(E(network))), get.edge.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="");
    file.nm <- paste(file, "_", attrib[i], ".EA", sep="");
    write(first.line, file=file.nm, ncolumns=1, append=FALSE, sep =" ")
    write.table(eda, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

PlotLoadBoxplot <- function(mSetObj=NA, cmpd){

  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }

  cls.lbls <- mSetObj$dataSet$cls;
  y.label <- GetAbundanceLabel(mSetObj$dataSet$type);
  cmpd.name = paste0("Met_", cmpd, ".png")
  
  Cairo::Cairo(file=cmpd.name, width=240, height=400, bg = "transparent", type="png");
  
  col <- unique(GetColorSchema(cls.lbls))
  df <- data.frame(conc = mSetObj$dataSet$norm[, cmpd], class = cls.lbls)
  p <- ggplot2::ggplot(df, aes(x=class, y=conc, fill=class)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  p <- p + stat_summary(fun.y=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
  p <- p + theme(text = element_text(size=15), plot.margin = margin(t=0.45, r=0.25, b=1.5, l=0.25, "cm"))
  p <- p + scale_fill_manual(values=col) + ggtitle(cmpd) + theme(axis.text.x = element_text(angle=90, hjust=1), axis.text = element_text(size=10))
  p <- p + theme(plot.title = element_text(size = 14, hjust=0.5, face="bold", vjust=2))
  print(p)
  
  dev.off()
}

#'Compute within group and between group sum of squares
#'(BSS/WSS) for each row of a matrix which may have NA
#'@description Columns have labels, x is a numeric vector,
#'cl is consecutive integers
#'@param x Numeric vector
#'@param cl Columns
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

# compute the distance to the centroid of the given data
# note each col is a x,y,z
# since this is centered, the centroid is origin
GetDist3D <-function(mat, target=c(0,0,0)){
    dist.vec <- apply(mat, 2, function(x) dist(rbind(x, target)));
    return(dist.vec);
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
    res <- var(as.numeric(x[tmp]))
  }
  res
}

end.with <- function(bigTxt, endTxt){
   return(substr(bigTxt, nchar(bigTxt)-nchar(endTxt)+1, nchar(bigTxt)) == endTxt);
}

## fast T-tests/F-tests, and use cache to avoid redundant computing
PerformFastUnivTests <- function(data, cls, var.equal=TRUE){
    if(!exists("mem.univ")){
        require("memoise");
        mem.univ <<- memoise(.perform.fast.univ.tests);
    }
    return(mem.univ(data, cls, var.equal));
}

.perform.fast.univ.tests <- function(data, cls, var.equal=TRUE){

    print("Performing fast univariate tests ....");
    # note, feature in rows for gene expression
    data <- t(as.matrix(data));
    if(length(levels(cls)) > 2){
        res <- try(rowcolFt(data, cls, var.equal = var.equal));
    }else{
        res <- try(rowcoltt(data, cls, FALSE, 1L, FALSE));
    }  

    if(class(res) == "try-error") {
        res <- cbind(NA, NA);
    }else{
        # res <- cbind(res$statistic, res$p.value);
        # make sure row names are kept
        res <- res[, c("statistic", "p.value")];
    }

    return(res);
}

GetCurrentPathForScheduler <- function(){
    path <-getwd();
    path <- gsub(basename(path), "", path)
    return(path)
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

rowcolFt =  function(x, fac, var.equal, which = 1L) {
  
  if(!(which %in% c(1L, 2L)))
    stop(sQuote("which"), " must be 1L or 2L.")
  
  if(which==2L)
    x = t(x)

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  sqr = function(x) x*x
  
  stopifnot(length(fac)==ncol(x), is.factor(fac), is.matrix(x))
  x   <- x[,!is.na(fac), drop=FALSE]
  fac <- fac[!is.na(fac)]

  ## Number of levels (groups)
  k <- nlevels(fac)

  ## xm: a nrow(x) x nlevels(fac) matrix with the means of each factor
  ## level
  xm <- matrix(
     sapply(levels(fac), function(fl) rowMeans(x[,which(fac==fl), drop=FALSE])),
     nrow = nrow(x),
     ncol = nlevels(fac))

  ## x1: a matrix of group means, with as many rows as x, columns correspond to groups 
  x1 <- xm[,fac, drop=FALSE]

  ## degree of freedom 1
  dff    <- k - 1

  if(var.equal){
    ## x0: a matrix of same size as x with overall means
    x0 <- matrix(rowMeans(x), ncol=ncol(x), nrow=nrow(x))
  
    ## degree of freedom 2
    dfr    <- ncol(x) - dff - 1

    ## mean sum of squares
    mssf   <- rowSums(sqr(x1 - x0)) / dff
    mssr   <- rowSums(sqr( x - x1)) / dfr

    ## F statistic
    fstat  <- mssf/mssr

  } else{

    ## a nrow(x) x nlevels(fac) matrix with the group size  of each factor
    ## level
    ni <- t(matrix(tapply(fac,fac,length),ncol=nrow(x),nrow=k))

    ## wi: a nrow(x) x nlevels(fac) matrix with the variance * group size of each factor
    ## level
    sss <- sqr(x-x1)
    x5 <- matrix(
       sapply(levels(fac), function(fl) rowSums(sss[,which(fac==fl), drop=FALSE])),
       nrow = nrow(sss),
       ncol = nlevels(fac))          
    wi <- ni*(ni-1) /x5

    ## u : Sum of wi
    u  <- rowSums(wi)

    ## F statistic
    MR <- rowSums(sqr((1 - wi/u)) * 1/(ni-1))*1/(sqr(k)-1)
    fsno <- 1/dff * rowSums(sqr(xm - rowSums(wi*xm)/u) * wi)
    fsdeno <- 1+ 2* (k-2)*MR
    fstat <- fsno/fsdeno

    ## degree of freedom 2: Vector with length nrow(x)
    dfr <- 1/(3 * MR)
  
  }
  
  res = data.frame(statistic = fstat,
                   p.value   = pf(fstat, dff, dfr, lower.tail=FALSE),
                   row.names = rownames(x))

  attr(res, "df") = c(dff=dff, dfr=dfr)
  return(res)
}

rowcoltt =  function(x, fac, tstatOnly, which, na.rm) {
    
  #if(.on.public.web){
  #  dyn.load(.getDynLoadPath());
  #}

  if (!missing(tstatOnly) && (!is.logical(tstatOnly) || is.na(tstatOnly)))
      stop(sQuote("tstatOnly"), " must be TRUE or FALSE.")
  
  f = checkfac(fac)
  if ((f$nrgrp > 2) || (f$nrgrp <= 0))
    stop("Number of groups is ", f$nrgrp, ", but must be >0 and <=2 for 'rowttests'.")

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  #cc = .Call("rowcolttests", x, f$fac, f$nrgrp, which-1L, na.rm)
   cc = XiaLabCppLib::rowcolttestsR(x, f$fac, f$nrgrp, which-1L, na.rm)

  res = data.frame(statistic = cc$statistic,
                   dm        = cc$dm,
                   row.names = dimnames(x)[[which]])

  if (!tstatOnly)
    res = cbind(res, p.value = 2*pt(abs(res$statistic), cc$df, lower.tail=FALSE))

  attr(res, "df") = cc$df    
  return(res)
}

checkfac = function(fac) {

  if(is.numeric(fac)) {
    nrgrp = as.integer(max(fac, na.rm=TRUE)+1)
    fac   = as.integer(fac)
  }
  ## this must precede the factor test
  if(is.character(fac))
    fac = factor(fac)

  if (is.factor(fac)) {
    nrgrp = nlevels(fac)
    fac   = as.integer(as.integer(fac)-1)
  } 
  if(!is.integer(fac))
    stop("'fac' must be factor, character, numeric, or integer.")
  
  if(any(fac<0, na.rm=TRUE))
    stop("'fac' must not be negative.")
    
  return(list(fac=fac, nrgrp=nrgrp))
}

# to convert all rds files to qs file for faster access
convert.rds2qs <- function(){
 rds.files <- list.files(".", pattern=".rds$");
 for(rds in rds.files){
    lib.rds <- readRDS(rds);
    nm <- substr(rds, 1, nchar(rds)-4);
    qs::qsave(lib.rds,paste0(nm, ".qs"));
 }
}

# to convert all rda files to qs file for faster access
convert.rda2qs <- function(){
 rda.files <- list.files(".", pattern=".rda$");
 for(rda in rda.files){
    nm <- substr(rda, 1, nchar(rda)-4);
    lib.rda <- load(rda);
    # here the name is inmexpa (jointpa)
    # qs::qsave(inmexpa,paste0(nm, ".qs"));
    # here the name is metpa (metpa)
    qs::qsave(metpa,paste0(nm, ".qs"));
    # here the name is current.msetlib (msets)
    #qs::qsave(current.msetlib,paste0(nm, ".qs"));
 }
}

overlap_ratio <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)
  length(intersect(x, y))/length(unique(c(x,y)))
}

# a utility function to get pheatmap image size (before saving to PNG)
# https://stackoverflow.com/questions/61874876/get-size-of-plot-in-pixels-in-r
get_pheatmap_dims <- function(dat, annotation, view.type, width, cellheight = 15, cellwidth = 15){
  png("NUL", type = "cairo"); # trick to avoid open device in server 
  heat_map <- pheatmap::pheatmap(dat, annotation=annotation, cellheight = cellheight, cellwidth = cellwidth);
  h <- sum(sapply(heat_map$gtable$heights, grid::convertHeight, "in"));
  w  <- sum(sapply(heat_map$gtable$widths, grid::convertWidth, "in"));
  dev.off();

  # further refine 
  myW <- ncol(dat)*20 + 200;  
  if(myW < 650){
      myW <- 650;
  }   
  myW <- round(myW/72,2);
  if(w < myW){
    w <- myW;
  }

  if(view.type == "overview"){
    if(is.na(width)){
      if(w > 9){
        w <- 9;
      }
    }else if(width == 0){
      if(w > 7.2){
        w <- 7.2;
      }
      
    }else{
      w <- 7.2;
    }
    if(h > w){
      h <- w;
    }
  }

  return(list(height = h, width = w));
}

##
## perform unsupervised data filter based on common measures
##
PerformFeatureFilter <- function(int.mat, filter, filter.cutoff, anal.type, privilidged){

    nm <- NULL;
    msg <- "";

    # first compute rank based on filter selected
    if (filter == "rsd"){
      sds <- apply(int.mat, 2, sd, na.rm=T);
      mns <- apply(int.mat, 2, mean, na.rm=T);
      filter.val <- abs(sds/mns);
      nm <- "Relative standard deviation";
    }else if (filter == "nrsd" ){
      mads <- apply(int.mat, 2, mad, na.rm=T);
      meds <- apply(int.mat, 2, median, na.rm=T);
      filter.val <- abs(mads/meds);
      nm <- "Non-paramatric relative standard deviation";
    }else if (filter == "mean"){
      filter.val <- apply(int.mat, 2, mean, na.rm=T);
      nm <- "mean";
    }else if (filter == "sd"){
      filter.val <- apply(int.mat, 2, sd, na.rm=T);
      nm <- "standard deviation";
    }else if (filter == "mad"){
      filter.val <- apply(int.mat, 2, mad, na.rm=T);
      nm <- "Median absolute deviation";
    }else if (filter == "median"){
      filter.val <- apply(int.mat, 2, median, na.rm=T);
      nm <- "median";
    }else{ # iqr
      filter.val <- apply(int.mat, 2, IQR, na.rm=T);
      nm <- "Interquantile Range";
    }
    
    # get the rank of the filtered variables
    rk <- rank(-filter.val, ties.method='random');

    remain.num <- ncol(int.mat)*(1-(filter.cutoff/100));
    remain <- rk <= remain.num;
    msg <- paste(msg, "Feature filtering based on", nm, "[", sum(remain), "];");

    if(!privilidged){
        max.allow <- .get.max.allow(anal.type);  
        if(sum(remain) > max.allow){
            remain <- rk <= max.allow;
            msg <- paste(msg, paste("Further reduced to", max.allow, "features based on", nm));   
        }
    }
    # save a copy for user 
    fast.write.csv(cbind(filter=filter.val, t(int.mat)), file=paste0("data_prefilter_", filter, ".csv"));

    #print(msg);
    return(list(data=int.mat[, remain], msg=msg));
}


# do default filtering based on data size
.computeEmpiricalFilterCutoff <- function(feat.num, anal.type){

    filter.cutoff <- 0;
    if(feat.num < 250){ 
        filter.cutoff <- 5;
    }else if(feat.num < 500){ # reduce 10%
        filter.cutoff <- 10;
    }else if(feat.num < 1000){ # reduce 25%
        filter.cutoff <- 25;
    }else{ # reduce 40%, 
        filter.cutoff <- 40;

        max.allow <- .get.max.allow (anal.type);      
        if(feat.num*0.6 > max.allow){
            filter.cutoff <- round(100*(feat.num - max.allow) / feat.num);
        }
    }

    return(filter.cutoff);
}

# general default control for datasize 
.get.max.allow <- function(anal.type){

    if(anal.type == "mummichog"){
        max.allow <- 7500;
    }else if(anal.type == "power" || anal.type == "mf"){
        max.allow <- 2500;
    }else{
        max.allow <- 5000;
    }
    return(max.allow);
}

# make data and metadata share the same samples and in same order    
.sync.data.metadata <- function(my.data, my.metadata){ 

     if(!identical(rownames(my.data), rownames(my.metadata))){

        # now get the overlap, using first as anchor
        smpl.nms <- rownames(my.data);
        shared.inx <- smpl.nms %in% rownames(my.metadata);
        shared.nms <- smpl.nms[shared.inx];

        if(sum(!shared.inx)>0){
            print(paste("Those samples are removed from data: ", paste(smpl.nms[!shared.inx], collapse="; "), collapse=" "));
        }

        # update both
        my.data <- my.data[shared.nms,,drop=FALSE];
        my.metadata <- my.metadata[shared.nms,,drop=FALSE];

        # drop levels for factor column in case the whole group is gone
        for(i in 1:ncol(my.metadata)){
            if(class(my.metadata[,i]) == "factor"){
                my.metadata[,i] <- droplevels(my.metadata[,i]);
            }
        }
        
        print(paste("Successfully performed synchronization: a total of", length(shared.nms), "samples that are shared between the two tables are left.", collapse=" "));
      }

      return(list(data=my.data, metadata=my.metadata));
}

