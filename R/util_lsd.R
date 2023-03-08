#'Calculate Fisher's Least Significant Difference (LSD)
#'@description Adapted from the 'agricolae' package
#'@param y Input Y
#'@param trt Input trt
#'@param alpha Numeric, default is 0.05
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

my.lsd.test <- function(y, trt, alpha = 0.05){
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
