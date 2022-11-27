ProcessTsne <- function(){
  reductionOptGlobal <<- "tsne"
  fileNm <- "tsne.json";
  TsneList <- qs::qread("../../data/tsne.qs");
  dat <- TsneList$tsne;
  names <- dat$SampleNM
  pos.xyz <- dat[,c(2:4)];
  pos.xyz <- unitAutoScale(pos.xyz);
  rownames(pos.xyz) <- names;
#  meta.vec <- dat$Platforms
#  meta.vec.num = as.integer(as.factor(meta.vec))
  color = dat$colors;
  reductionSet <- list();
  reductionSet$pos.xyz <- pos.xyz;
  nodes <- list();
  for(i in 1:length(names)){
    nodes[[i]] <- list(
      id=names[i],
      label=names[i],
      size=12,
#      meta=meta.vec[i],
#      cluster=meta.vec.num[i],
      fx = unname(pos.xyz[i,1])*1000,
      fy = unname(pos.xyz[i,2])*1000,
      fz = unname(pos.xyz[i,3])*1000,
      colorb=color[i],
      colorw=color[i],
      topocolb=color[i],
#      topocolw=color[i],
      expcolb=color[i],
#      expcolw=color[i],
      attributes=list(
#        expr = 1,
#        degree=1,
#        between=1
      )
    );
  }
  .set.rdt.set(reductionSet);
  #modules = "NA"
  #ellipse ="NA"
  #edge.mat = "NA"
  netData <- list(nodes=nodes, 
  #                edges=edge.mat, 
  #                modules=modules, 
                  meta=dat[,-c(2:5,14)], 
                  meta_table = TsneList$meta,
  #                loading="NA", 
                  reductionOpt="tsne", 
  #                misc="NA", 
                  omicstype="NA");
  netData[["misc"]] <- "NA"
  library(RJSONIO);
  sink(fileNm);
  cat(toJSON(netData));
  sink();
}

unitAutoScale <- function(df){
  df <- as.data.frame(df)
  row.nms <- rownames(df);
  col.nms <- colnames(df);
  df<-apply(df, 2, AutoNorm);
  rownames(df) <- row.nms;
  colnames(df) <- col.nms;
  maxVal <- max(abs(df))
  df<- df/maxVal
  return(df)
}

# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

gg_color_hue_covid <- function(grp.num, type="green", filenm=NULL) {
  grp.num <- as.numeric(grp.num)
  # if(type == "green"){
    pal64 <- c("#1C8841","#B7898B","#CE1256","#993D7A","#65689E","#C8490B","#0E6FAF","#4A8F75",
               "#268B49","#1A6E50","#114F5D","#08306B","#FD8D3C","#F46530","#EB3D26","#E01A1B",
               "#C72313","#AD2C0A","#8F3706","#5A4B14","#265E22","#067230","#329D52","#38A258",
               "#228B4B","#0C743E","#10724F","#358B86","#5AA3BD","#86A1BB","#E9725B","#F05840",
               "#E03B30","#CF1F21","#CF1E3B","#D52865","#DB328F","#C248A0","#A261AC","#827AB8",
               "#985B9A","#B33678","#3C8FBC","#808E99","#C48D75","#FC8D57","#FC8D4D","#FC8D43",
               "#F98636","#ED6E22","#E0570D","#994C26","#6B5042","#4A5A4F","#3A6E4B","#2A8247",
               "#2B925F","#39A090","#48ADC1","#3EA2CA","#2688BD","#2873B2","#5683BB","#8393C4",
               "#6E92A1")
  # pal64 <- sort(pal64)
  # } else {
    pal18 <- c( "#4363d8","#e6194B" , "#3cb44b", "#f032e6", "#ffe119", "#e6194B", "#f58231",
                "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3",
                "#808000", "#ffd8b1", "#42d4f4","#000075", "#ff4500");
  # }
    if(grp.num > 60){
      colArr <- pal64[1:grp.num];
    } else {
      colArr <- pal18[1:grp.num];
    }
    if(is.null(filenm)){
      return(colArr);
    } else {
      library(RJSONIO)
      sink(filenm);
      cat(toJSON(colArr));
      sink();
      return(filenm);
  }
}


ComputeEncasing <- function(filenm, type, names.vec, level=0.95, omics="NA"){
  #Sys.setenv(RGL_USE_NULL = TRUE)
  level <- as.numeric(level)
  names = strsplit(names.vec, "; ")[[1]]
  reductionSet <- .get.rdt.set();
  pos.xyz = reductionSet$pos.xyz

  inx = rownames(pos.xyz) %in% names;
  coords = as.matrix(pos.xyz[inx,c(1:3)])
  mesh = list()
  if(type == "alpha"){
    library(alphashape3d)
    library(rgl)
    sh=ashape3d(coords, 1.0, pert = FALSE, eps = 1e-09);
    mesh[[1]] = as.mesh3d(sh, triangles=T);
  }else if(type == "ellipse"){
    library(rgl);
    pos=cov(coords, y = NULL, use = "everything");
    mesh[[1]] = ellipse3d(x=as.matrix(pos), level=level);
  }else{
    library(ks);
    res=kde(coords);
    r = plot(res, cont=level*100);
    sc = scene3d();
    mesh = sc$objects;
  }
  library(RJSONIO);
  sink(filenm);
  cat(toJSON(mesh));
  sink();
  return(filenm);
}

.get.rdt.set <- function(){
  return(qs::qread("rdt.set.qs"));
}

.set.rdt.set <- function(my.set){
  qs::qsave(my.set, file="rdt.set.qs");
}

### organize data based on meta info for downloading
dataprocessing <- function(platformMeta = "All", 
                           BloodTypeMeta = "All", 
                           PolarityMeta = "All", 
                           CountryMeta = "All", 
                           PopulationMeta = "All", 
                           ComparisonMeta = "All"){
  if(dir.exists("/home/qiang/Documents/COVID-19/")){
    #zhiqiang's local
    datadir <- "/media/qiang/UltraData/covid/1_Working_Dir/1_DataSets/";
    metapath <- "/media/qiang/UltraData/covid/1_Working_Dir/2_DataSet_Sum/DataSet_sum_complete.csv";
  } else {
    #dev's
    datadir <- "/data/glassfish/projects/covid/datasets/";
    metapath <- "/data/glassfish/projects/covid/datasets/DataSet_sum_complete.csv";
  }
  
  if(!dir.exists(datadir)){
    stop("NO COVID data found in your local, --> Ask Zhiqiang immediately!")
  }

  metadt <- read.csv(metapath);
  metadt$Platform <- 
    apply(metadt, 1, FUN = function(x){
      if(x[3] == "LC-MS" & x[6] == "Untargeted"){
        return("LC-MSu")
      } else if(x[3] == "LC-MS" & x[6] == "Targeted") {
        return("LC-MSt")
      } else {
        return(x[3])
      }})
  platforms <- strsplit(platformMeta, ",")[[1]];
  bloods <- strsplit(BloodTypeMeta, ",")[[1]];
  polarities <- strsplit(PolarityMeta, ",")[[1]];
  countiries <- strsplit(CountryMeta, ",")[[1]];
  populations <- strsplit(PopulationMeta, ",")[[1]];
  comparisons <- strsplit(ComparisonMeta, ",")[[1]];
  res <- 
    apply(metadt,1, FUN = function(x){
      if(x[11] == "Mass Table" | x[11] == "Compound List" | x[11] == "Compound List + Mass Table"){
        return(FALSE);
      }
      condi1 <- condi2 <- condi3 <- condi4 <- condi5 <- condi6 <- FALSE;
      if(x[3] %in% platforms | (platforms == "All")[1]){
        condi1 <- TRUE;
      }
      if(x[4] %in% bloods | (bloods == "All")[1]){
        condi2 <- TRUE;
      }
      if(x[5] %in% polarities | (polarities == "All")[1]){
        condi3 <- TRUE;
      }
      if(x[8] %in% populations | (populations == "All")[1]){
        condi4 <- TRUE;
      }
      if(x[10] %in% countiries | (countiries == "All")[1]){
        condi5 <- TRUE;
      }
      
      if(all(comparisons %in% c("COVID", "nonCOVID", "HC", "Recovered"))){
        extsGroup <- vector();
        if(as.numeric(x[13]) != 0){ extsGroup <- c(extsGroup, "COVID")};
        if(as.numeric(x[14]) != 0){ extsGroup <- c(extsGroup, "nonCOVID")};
        if(as.numeric(x[15]) != 0){ extsGroup <- c(extsGroup, "HC")};
        if(as.numeric(x[16]) != 0){ extsGroup <- c(extsGroup, "Recovered")};
        if(all(comparisons %in% extsGroup)) {condi6 <- TRUE};
      } else if(all(comparisons %in% c("Mild_Moderate", "Severe", "Asymptomatic", "Critical"))) {
        extsGroup <- vector();
        if(as.numeric(x[21]) != 0){ extsGroup <- c(extsGroup, "Asymptomatic")};
        if(as.numeric(x[22]) != 0){ extsGroup <- c(extsGroup, "Mild_Moderate")};
        if(as.numeric(x[23]) != 0){ extsGroup <- c(extsGroup, "Critical")};
        if(as.numeric(x[24]) != 0){ extsGroup <- c(extsGroup, "Severe")};
        if(all(comparisons %in% extsGroup)) {condi6 <- TRUE};
      } else if(all(comparisons %in% c("Deceased", "Recovered"))){
        extsGroup <- vector();
        if(as.numeric(x[20]) != 0) {extsGroup <- c(extsGroup, "Deceased")};
        if(as.numeric(x[16]) != 0) {extsGroup <- c(extsGroup, "Recovered")};
        if(all(comparisons %in% extsGroup)) {condi6 <- TRUE};
      }
      
      return(condi1 && condi2 && condi3 && condi4 && condi5 && condi6)
    });
  
  metadt0 <- metadt[res, -c(11,12,25)]
  metadt0$DataSet_IDs <- gsub("\\.0", "", metadt0$DataSet_IDs);
  
  allfiles <- list.files(datadir, recursive = T, full.names = T)
  aimsets <- paste0("DataSet", metadt0$DataSet_IDs,"_");
  file2export <- vapply(aimsets, function(x){
    allfiles[grepl(paste0(x,"CMPD|",x,"MetaInfo|",x,"LIPID"), allfiles)][1:2]
  }, FUN.VALUE = character(length = 2));
  write.csv(metadt0, file = "0_meta_data.csv", row.names = F, quote = T);
  if(file.exists("all_exported_datasets.zip")){
    unlink("all_exported_datasets.zip", force = TRUE)
  }
  zip("all_exported_datasets.zip", files = c("0_meta_data.csv", file2export),  extras = '-j1');
  if(file.exists('all_exported_datasets.zip')){
    return("all_exported_datasets.zip");
  } else {
    return("NULL");
  }
}

## Generate dataset for further analysis
datageneration <- function(datasets = NA, comparisons = NA){
  cat("Data generating ", datasets, " for comparisons between ", comparisons, " ....\n");
  
  if(dir.exists("/home/qiang/Documents/COVID-19/")){
    #zhiqiang's local
    datadir <- "/media/qiang/UltraData/covid/1_Working_Dir/1_DataSets/";
    metapath <- "/media/qiang/UltraData/covid/1_Working_Dir/2_DataSet_Sum/DataSet_sum_complete.csv";
  } else {
    #dev's
    datadir <- "/data/glassfish/projects/covid/datasets/";
    metapath <- "/data/glassfish/projects/covid/datasets/DataSet_sum_complete.csv";
  }
  
  datasets_all <- unlist(strsplit(datasets, ","))
  
  if(length(datasets_all) == 1){
    datasets <- datasets_all;
    .coreDataGeneration(metapath, datadir, datasets, comparisons);
  } else {
    unlink(list.files(pattern = "DataSet[0-9]+_0.csv"), force = TRUE)
    for(ds in datasets_all){
      .coreDataGeneration(metapath, datadir, ds, comparisons, paste0(ds, "_"));
    }
  }
  
  return(1);
}

.coreDataGeneration <- function(metapath, datadir, datasets, comparisons, prefix="covid_data_"){
  metadt <- read.csv(metapath);
  od <- as.numeric(sub("DataSet", "", datasets));
  patn <- paste0("DataSet", sub("\\.0", "", metadt$DataSet_IDs[od]), "_");
  allfiles <- list.files(datadir, recursive = T, full.names = T)
  lfs <- allfiles[grepl(patn, allfiles)];
  f <- lfs[grep("_CMPD_tTable|_LIPID_tTable|_CMPD_uTable|_LIPID_uTable", lfs)]
  dt0 <- read.csv(f);
  
  nms1 <- gsub("CMPD_uTable|CMPD_tTable|LIPID_uTable|LIPID_tTable", 
               "MetaInfo", f);
  nms2 <- gsub("\\_[0-9]+[X-Z]\\.csv|[0-9]+[X-Z]\\.csv", "\\.csv", nms1)
  meta0 <- read.csv(nms2);
  
  compars <- unlist(strsplit(comparisons, ","))
  if(all(compars %in% c("COVID", "nonCOVID", "HC", "Recovered"))){
    sampleNMs <- meta0$Samples[meta0$Groups %in% compars]
    groups <- meta0$Groups[meta0$Groups %in% compars];
    dtx <- .dataformatting(dt0, sampleNMs, groups);
    
  } else if(all(compars %in% c("Mild_Moderate", "Severe", "Asymptomatic", "Critical"))) {
    sampleNMs <- meta0$Samples[meta0$Severities %in% compars]
    groups <- meta0$Severities[meta0$Severities %in% compars];
    dtx <- .dataformatting(dt0, sampleNMs, groups);
    
  } else if(all(compars %in% c("Deceased", "Survived"))) {
    sampleNMs <- meta0$Samples[meta0$Outcomes %in% compars]
    groups <- meta0$Outcomes[meta0$Outcomes %in% compars];
    dtx <- .dataformatting(dt0, sampleNMs, groups);
  }
  
  count <- 0;
  while(file.exists(paste0(prefix, count, ".csv"))){
    count <- count + 1;
  }
  write.csv(dtx, file = paste0(prefix, count, ".csv"), row.names = FALSE)
}


getCurrentCOVIDdata <- function(module){
  if(module == "metax"){
    return(paste0(list.files(pattern = "DataSet[0-9]+_0.csv"), collapse = ","))
  } else {
    ress <- as.numeric(gsub("covid_data_|.csv", "", list.files(pattern = "covid_data_")));
    return(list.files(pattern = "covid_data_")[which.max(ress)])
  }
}

.dataformatting <- function(dt0, sampleNMs, groups){
  dt1 <- dt0[,c(1, which(colnames(dt0) %in% sampleNMs))];
  ods <- unname(vapply(sampleNMs, FUN = function(xx) {
    which(xx == colnames(dt1))
  }, FUN.VALUE = integer(length = 1)));
  dt1[1, ods] <- groups;
  dt1;
}

dataReducing4meta <- function(){
  lfs <- list.files(pattern = "^DataSet[0-9]+_0.csv");
  alldataList <- list()
  for(jj in 1:length(lfs)){
    alldataList[[jj]] <- read.csv(lfs[jj])
  }
  hmdbs <- lapply(alldataList, FUN= function(x){
    x[-1,1]
  })
  
  OverlappedHDMBs <- Reduce(intersect, hmdbs);
  if(length(OverlappedHDMBs) > 3){
    for(kk in 1:length(hmdbs)){
      dttmp <- alldataList[[kk]];
      dttmp2 <- dttmp[c(1, which(dttmp[,1] %in% OverlappedHDMBs)),]
      write.csv(dttmp2, file = lfs[kk], row.names = FALSE)
    }
    return(1)
  } else {
    return(0)
  }
}

prepareCMPDlist <- function(){
  ress <- as.numeric(gsub("covid_data_|.csv", "", list.files(pattern = "covid_data_")));
  fs <- list.files(pattern = "covid_data_")[which.max(ress)]
  dt0 <- read.csv(fs);
  groupInfo <- as.character(dt0[1,-1])
  group1 <- which(groupInfo == unique(groupInfo)[1])
  group2 <- which(groupInfo == unique(groupInfo)[2])
  logFCs <- apply(dt0[-1,], 1, function(x){
    x0 <- x[-1];
    Xvalue1 <- as.numeric(x0[group1]);
    Xvalue2 <- as.numeric(x0[group2]);
    num1 <- mean(Xvalue1, na.rm=T);
    num2 <- mean(Xvalue2, na.rm=T);
    if((num1 == 0) & (num2 == 0)){
      return(0)
    }
    if(num1 == 0){
      return(0)
    }
    -log10(num1/num2)
  })
  res <- which(is.infinite(logFCs))
  if(length(res) != 0){
    logFCs[res] <- max(logFCs[-res])*2
  }
  
  dt1 <- data.frame(dt0[-1,1], round(logFCs,3))
  colnames(dt1) <- c("#HMDB", "#FC")
  write.table(dt1, file = "cmpd_list.txt", row.names = F, sep = "\t", quote = F)
  return(1)
}
