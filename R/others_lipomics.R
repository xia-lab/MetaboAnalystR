#'Lipid analysis pipeliner
#'@description Lipid analysis pipeliner
#'@param inFile Input the file to read in
#'@param iso Default is set to "y"
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

analyze.lipids <- function(inFile, iso='y'){
  data <- .readDataTable(inFile);
  computeConc(data, iso);
}

#'Lipid analysis
#'@description The upper limit for each combination is considered to be
#'the minimal of the fatty acid concentration (nmol fatty acid/gram of sample)
#'X is the lopomics data obtained above
#'the result is the saved as separate files for each lipid class
#'@param X Input the data
#'@param iso Default is set to "y"
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

computeConc<-function(X, iso="y"){
  
  dg.inx<-X[,2]=='DG';
  pc.inx<-X[,2]=='PC';
  pe.inx<-X[,2]=='PE';
  tg.inx<-X[,2]=='TG'
  
  if(iso=='y'){
    if(sum(dg.inx)>0){
      calculateConcISO(X[dg.inx,-2], 'DG', 2, "DG_uplimit.csv", "DG_prob.csv");
    }
    if(sum(pc.inx)>0){
      calculateConcISO(X[pc.inx,-2], 'PC', 2, "PC_uplimit.csv", "PC_prob.csv");
    }
    if(sum(pe.inx)>0){
      calculateConcISO(X[pe.inx,-2], 'PE', 2, "PE_uplimit.csv", "PE_prob.csv");
    }
    if(sum(tg.inx)>0){
      calculateConcISO(X[tg.inx,-2], 'TG', 3, "TG_uplimit.csv", "TG_prob.csv");
    }
  }else{
    if(sum(dg.inx)>0){
      calculateConcNoISO(X[dg.inx,-2], 'DG', 2, "DG_uplimit.csv", "DG_prob.csv");
    }
    if(sum(pc.inx)>0){
      calculateConcNoISO(X[pc.inx,-2], 'PC', 2, "PC_uplimit.csv", "PC_prob.csv");
    }
    if(sum(pe.inx)>0){
      calculateConcNoISO(X[pe.inx,-2], 'PE', 2, "PE_uplimit.csv", "PE_prob.csv");
    }
    if(sum(tg.inx)>0){
      calculateConcNoISO(X[tg.inx,-2], 'TG', 3, "TG_uplimit.csv", "TG_prob.csv");
    }
  }
}

#'Calculate Concentration ISO
#'@description Assuming independent random distribution of FA, the most probable frequency
#'will be the product of the each component. Note: the data is concentration,
#'we need to get frequncies - percentage w.r.t the total nmol.
#'the result is the saved as separate files for each lipid class
#'data for each FA class, first col is sample name
#'@param dat Input the data
#'@param cls.name Input the class names
#'@param cls.num Input the number of classes
#'@param min.file Input the min file
#'@param prob.file Input the prob file
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

calculateConcISO<-function(dat, cls.name, cls.num, min.file, prob.file){
  lip.nms<-names(dat[,-1]);
  conc.mat<-data.matrix(dat[,-1]);
  conc.sum<-apply(conc.mat, 1, sum);
  percent.mat<-sweep(conc.mat, 1, conc.sum, FUN="/");
  len.col<-ncol(conc.mat);
  if(cls.num==2){
    if(cls.name=='DG'){
      len<-len.col*(len.col+1)/2; # triangle matrix
    }else{
      len<-len.col*len.col; # for PC/PE, AB/BA is not isomer
    }
    nm.vec<-vector(mode="character", length=len);
    min.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
    prob.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
    
  }else if(cls.num==3){
    len<-len.col*(len.col+1)*(len.col+2)/6; # triangle cube = triangle matrix*height
    nm.vec<-vector(mode="character", length=len);
    min.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
    prob.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
  }else{
    warning("not supported class number");
    return();
  }
  colnames(min.mat)<-dat[,1];
  colnames(prob.mat)<-dat[,1];
  col.count <- 0;
  for(i in 1:nrow(conc.mat)){
    conc<-as.numeric(conc.mat[i,]);
    prct<-as.numeric(percent.mat[i,]);
    col.count<-col.count+1;
    row.count <- 0;
    for(m in 1:length(conc)){
      for(n in m:length(conc)){
        if(cls.num==3){
          for(l in n:length(conc)){
            row.count<-row.count+1;
            prob.mat[row.count,col.count]<-sum(as.numeric(conc.mat[i,]))*(prct[m]*prct[n]*prct[l]);
            suffix<-NULL;
            if(m==n){
              if(l==n){ # all same
                minC<-conc[m]/3;
              }else{
                minC<-min(conc[m]/2, conc[l])
                suffix<-"[iso3]";
              }
            }else if(m==l || l==n){
              minC<-min(conc[m]/2, conc[n])
              suffix<-"[iso3]";
            }else{
              minC<-min(conc[m], conc[n], conc[l]);
              suffix<-"[iso6]";
            }
            min.mat[row.count,col.count]<-minC;
            if(i==1){
              nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], "/", lip.nms[l],")", suffix, sep="");
            }
          }
        }else{
          suffix<-NULL;
          if(m==n){
            minC<-conc[m]/2;
          }else{
            minC<-min(conc[m], conc[n]);
            suffix<-"[iso2]";
          }
          probC<-sum(as.numeric(conc.mat[i,]))*(prct[m]*prct[n]);
          
          if(cls.name=='DG'){
            row.count<-row.count+1;
            min.mat[row.count,col.count]<-minC;
            prob.mat[row.count,col.count]<-probC;
            if(i==1){
              nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], "/0:0)", suffix, sep="");
            }
          }else{ # PE/PC don't have iso, need specify
            row.count<-row.count+1;
            min.mat[row.count,col.count]<-minC;
            prob.mat[row.count,col.count]<-probC;
            if(i==1){
              nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], ")", sep="");
            }
            if(m!=n){ # don't switch position is m and n are the same
              row.count<-row.count+1;
              min.mat[row.count,col.count]<-minC;
              prob.mat[row.count,col.count]<-probC;
              if(i==1){
                nm.vec[row.count]<-paste(cls.name, "(", lip.nms[n],"/", lip.nms[m], ")", sep="");
              }
            }
          }
        }
      }
    }
  }
  rownames(min.mat)<-nm.vec;
  min.mat<-cbind(min.mat, "Average" = apply(min.mat, 1, mean), "SD" = apply(min.mat, 1, sd));
  rownames(prob.mat)<-nm.vec;
  prob.mat<-cbind(prob.mat, "Average" = apply(prob.mat, 1, mean), "SD" = apply(prob.mat, 1, sd));
  write.csv(min.mat, file=min.file);
  write.csv(prob.mat, file=prob.file);
}

calculateConcNoISO<-function(dat, cls.name, cls.num, min.file, prob.file){
  lip.nms<-names(dat[,-1]);
  conc.mat<-data.matrix(dat[,-1]);
  conc.sum<-apply(conc.mat, 1, sum);
  percent.mat<-sweep(conc.mat, 1, conc.sum, FUN="/");
  len.col<-ncol(conc.mat);
  if(cls.num==2){
    len<-len.col^2; # triangle matrix
    nm.vec<-vector(mode="character", length=len);
    min.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
    prob.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
  }else if(cls.num==3){
    len<-len.col^3; # triangle cube = triangle matrix*height
    nm.vec<-vector(mode="character", length=len);
    min.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
    prob.mat<-matrix(0, nrow=len, ncol=nrow(conc.mat));
  }else{
    warning("not supported class number");
    return();
  }
  colnames(min.mat)<-dat[,1];
  colnames(prob.mat)<-dat[,1];
  col.count <- 0;
  for(i in 1:nrow(conc.mat)){
    conc<-as.numeric(conc.mat[i,]);
    prct<-as.numeric(percent.mat[i,]);
    col.count<-col.count+1;
    row.count <- 0;
    for(m in 1:length(conc)){
      for(n in 1:length(conc)){
        if(cls.num==3){
          for(l in 1:length(conc)){
            row.count<-row.count+1;
            prob.mat[row.count,col.count]<-sum(as.numeric(conc.mat[i,]))*(prct[m]*prct[n]*prct[l]);
            suffix<-NULL;
            if(m==n){
              if(l==n){ # all same
                minC<-conc[m]/3;
              }else{
                minC<-min(conc[m]/2, conc[l])
              }
            }else if(m==l){
              minC<-min(conc[m]/2, conc[n])
            }else{
              minC<-min(conc[m], conc[n], conc[l]);
            }
            min.mat[row.count,col.count]<-minC;
            nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], "/", lip.nms[l],")", sep="");
          }
        }else{
          if(m==n){
            minC<-conc[m]/2;
          }else{
            minC<-min(conc[m], conc[n]);
          }
          
          if(cls.name=='DG'){
            row.count<-row.count+1;
            min.mat[row.count,col.count]<-minC;
            prob.mat[row.count,col.count]<-sum(as.numeric(conc.mat[i,]))*(prct[m]*prct[n]);
            nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], "/0:0)", sep="");
          }else{ # PE/PC don't have iso, need specify
            row.count<-row.count+1;
            min.mat[row.count,col.count]<-minC;
            prob.mat[row.count,col.count]<-sum(as.numeric(conc.mat[i,]))*(prct[m]*prct[n]);
            nm.vec[row.count]<-paste(cls.name, "(", lip.nms[m],"/", lip.nms[n], ")", sep="");
          }
        }
      }
    }
  }
  rownames(min.mat)<-nm.vec;
  min.mat<-cbind(min.mat, "Average" = apply(min.mat, 1, mean), "SD" = apply(min.mat, 1, sd));
  rownames(prob.mat)<-nm.vec;
  prob.mat<-cbind(prob.mat, "Average" = apply(prob.mat, 1, mean), "SD" = apply(prob.mat, 1, sd));
  write.csv(min.mat, file=min.file);
  write.csv(prob.mat, file=prob.file);
}