### Perform miscellaneous tasks
### Perform misc tasks
### Jeff Xia\email{jeff.xia@mcgill.ca}
### McGill University, Canada
###License: GNU GPL (>= 2)


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

#'Read data table
#'@description Function to read in a data table. First, it will try to use fread, however, it has issues with 
#'some windows 10 files. In such case, use the slower read.table method.
#'@param fileName Input filename
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

.readDataTable <- function(fileName){
  dat <- try(data.table::fread(fileName, header=TRUE, check.names=FALSE, blank.lines.skip=TRUE, data.table=FALSE));
  if(class(dat) == "try-error" || any(dim(dat) == 0)){
    print("Using slower file reader ...");
    formatStr <- substr(fileName, nchar(fileName)-2, nchar(fileName))
    if(formatStr == "txt"){
      dat <- try(read.table(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
    }else{ # note, read.csv is more than read.table with sep=","
      dat <- try(read.csv(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
    }  
  }
  return(dat);
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
  
  a <- try(system(paste("unzip",  "-o", inPath, "-d", outPath), intern=T));
  
  if(class(a) == "try-error" | !length(a)>0){
    b <- try(unzip(inPath, outPath))
    if(length(b)==0){
      AddErrMsg("Failed to unzip the uploaded files!");
      AddErrMsg("Possible reason: file name contains space or special characters.");
      AddErrMsg("Use only alphabets and numbers, make sure there is no space in your file name.");
      AddErrMsg("For WinZip 12.x, use \"Legacy compression (Zip 2.0 compatible)\"");
      return (0);
    }
  }
  if(rmFile){
    RemoveFile(inPath);
  }
  return (1);
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
      bdata <- data.frame(bdata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
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

# only keep alphabets, numbers, ",", "." "_", "-" "/"
# note, this may leads to duplicate names
CleanNames <- function(query, type){
  
  if(type=="sample_name"){
    query <- gsub("[^[:alnum:]./_-]", "", query);
  }else{
    query <- gsub("[^[:alnum:][:space:],'./_-]", "", query)
  }
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
#'@export
# col.vec should already been created
UpdateGraphSettings <- function(mSetObj=NA, colVec, shapeVec){
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
   lvs <- levels(mSetObj$dataSet$cls); 
   grp.num <- length(lvs);

   if(grayscale){
      dist.cols <- colorRampPalette(c("grey90", "grey30"))(grp.num);
   }else if(exists("colVec") && !any(colVec =="#NA")){
      dist.cols <- colVec;
   }else{
      pal18 <- c("#e6194B", "#3cb44b", "#4363d8", "#42d4f4", "#f032e6", "#ffe119", "#911eb4", "#f58231", "#bfef45",
                   "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075");
             
      if(grp.num <= 18){ # update color and respect default
          dist.cols <- pal18[1:grp.num];
      }else{
         dist.cols <- colorRampPalette(pal18)(grp.num);
      }
   }

   colors <- vector(mode="character", length=length(mSetObj$dataSet$cls));
   for(i in 1:length(lvs)){
     colors[mSetObj$dataSet$cls == lvs[i]] <- dist.cols[i];
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

#'Clear folder and memory
#'@description Clear the current folder and objects in memory
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
ClearUserDir<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # remove physical files
  unlink(dir(), recursive=T);
  mSetObj$dataSet <- mSetObj$analSet <- list();
  res <- .set.mSet(mSetObj);
  cleanMem();
  return(res);
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
ShowMemoryUse <- function(..., n=20) {
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
#'@param analType Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, 
#'cmpdmap, smpmap, or pathinteg.
#'@param paired Logical, is data paired (T) or not (F).
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@param lbl.type Specify the data label type, either discrete (disc) or continuous (cont).
#'@export

XSet2MSet <- function(xset, dataType, analType, paired=F, format, lbl.type){
  
  data <- xcms::groupval(xset, "medret", "into");
  data2 <- rbind(class= as.character(phenoData(xset)$class), data);
  rownames(data2) <- c("group", paste(round(groups(xset)[,"mzmed"], 3), round(groups(xset)[,"rtmed"]/60, 1), sep="/"));
  write.csv(data2, file="PeakTable.csv");
  mSet <- InitDataObjects("dataType", "analType", paired)
  mSet <- Read.TextData(mSet, "PeakTable.csv", "format", "lbl.type")
  print("mSet successfully created...")
  return(.set.mSet(mSetObj));
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
  
  col <- unique(GetColorSchema(mSetObj))
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

## fast T-tests/F-tests using genefilter
## It leverages RSclient to perform one-time memory intensive computing
PerformFastUnivTests <- function(data, cls, var.equal=TRUE){
  print("Peforming fast univariate tests ....");
  load_RSclient()
  rsc <- RS.connect();
  
  RS.assign(rsc, "my.dir", getwd()); 
  RS.eval(rsc, setwd(my.dir));
  
  # note, feature in rows for gene expression
  data <- t(as.matrix(data));
  dat.out <- list(data=data, cls=cls, var.equal=var.equal);
  RS.assign(rsc, "dat.in", dat.out); 
  my.fun <- function(){
    if(length(levels(cls)) > 2){
      res <- try(genefilter::rowFtests(dat.in$data, dat.in$cls, var.equal = dat.in$var.equal));
    }else{
      lvls <- levels(cls);
      ig1 <- which(cls == lvls[1]);
      ig2 <- which(cls == lvls[2]);
      res <- try(genefilter::rowttests(dat.in$data, dat.in$cls));
    }
    if(class(res) == "try-error") {
      res <- cbind(NA, NA);
    }else{
      res <- cbind(res$statistic, res$p.value);
    }
    return(res);
  }
  RS.assign(rsc, my.fun);
  my.res <- RS.eval(rsc, my.fun());
  RS.close(rsc);
  return(my.res);
}