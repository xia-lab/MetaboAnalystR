#' Data inspectation
#' @description This functions provide a path for users to visually inspect their raw data before the data 
#' trimming so as to remove the dirty or significantly uneluted peaks.
#' @param datapath Character, the path of the raw MS data files (.mzXML, .CDF and .mzML) 
#' for the visual and intuitive data inspectation.
#' @param rt.range Numerics, a congregation of two values to define the lower and upper RT range (seconds) for 
#' users to inspect. This is an optional parameter, if absent, will display the MS of the whole RT range.
#' @param mz.range Numerics, a congregation of two values to define the lower and upper mz range for 
#' users to inspect. This is an optional parameter, if absent, will display the MS of the whole mz range.
#' @param num Numeric, the order of the sample to inspect according to the names.
#' @param dimension Character, the dimension for sample to display, including '2D' or '3D'. The default is '3D'.
#' @export
#' @import MSnbase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

PerformDataInspect<-function(datapath, rt.range, mz.range,
                             num = 1, dimension="3D", res=100){
  require("MSnbase")
  
  mzf<-list.files(datapath,recursive = F,full.names = T)[num]
  cat(basename(mzf),"\n")
  ms <- openMSfile(mzf)
  hd <- header(ms)
  
  ms1 <- which(hd$msLevel == 1)
  
  if (missing(rt.range)){
    
    rtsel <- hd$retentionTime[ms1] > min(hd$retentionTime) & hd$retentionTime[ms1] < max(hd$retentionTime);
    rt.extension <- F
    print(paste("RT range is:",min(hd$retentionTime), "and",max(hd$retentionTime),"seconds !"))
    
    
  } else{
    
    if (rt.range[2]<rt.range[1]){
      a1<-rt.range[1];
      rt.range[1]<-rt.range[2];
      rt.range[2]<-a1;
    }
    print(paste("RT range is:",rt.range[1], "and",rt.range[2],"seconds !"))
    
    rtsel <- hd$retentionTime[ms1] > rt.range[1] & hd$retentionTime[ms1] < rt.range[2]
    
    rt.min <- min(hd$retentionTime[ms1]);
    rt.max <- max(hd$retentionTime[ms1]);
    
    if (rt.range[1] < rt.min | rt.range[2] > rt.max){
      rt.extension <- T
    } else {
      rt.extension <- F
    }
    
  }
  
  if (missing(mz.range)){
    
    min.mz<-min(hd$lowMZ);
    max.mz<-max(hd$highMZ);
    
    print(paste("MZ range is:",min.mz, "and",max.mz,"Thomson !"))
    
    res.mz<-(max.mz-min.mz)/res
    M <- MSmap(ms,
               ms1[rtsel],
               min.mz,
               max.mz,
               res.mz,
               hd,
               zeroIsNA = TRUE)
    
  } else{
    
    if (mz.range[2]<mz.range[1]){
      a1<-mz.range[1];
      mz.range[1]<-mz.range[2];
      mz.range[2]<-a1;
    }
    
    print(paste("MZ range is:",mz.range[1], "and",mz.range[2],"Thomson !"))
    
    res.mz<-(mz.range[2]-mz.range[1])/res
    M <- MSmap(ms,
               ms1[rtsel],
               mz.range[1],
               mz.range[2],
               res.mz,
               hd,
               zeroIsNA = TRUE)
  }
  
  
  if (rt.extension){
    
    if (min(M@rt) > rt.range[1]){
      M@rt <- c(rt.range[1],M@rt);
      M@map <- rbind(rep(NA,dim(M@map)[2]),M@map);
      M@ms <- c(M@ms,1)
    }
    
    if (max(M@rt) < rt.range[2]){
      M@rt <- c(M@rt,rt.range[2]);
      M@map <- rbind(M@map,rep(NA,dim(M@map)[2]));
      M@ms <- c(M@ms,1)
    }
  }
  
  if (missing(dimension) | dimension=="3D"){
    plot.MS_3D(M)
  } else {
    plot(M, aspect = 1, allTicks = FALSE)
  }
}

#' Perform raw MS data trimming
#' @description This function performs the raw data trimming. This function will output 
#' an trimmed MSnExp file to memory or hardisk according to the choice of users must 
#' provide the data path for 'datapath', and optionally provide other corresponding parameters.
#' @param datapath Character, the path of the raw MS data files (.mzXML, .CDF and .mzML) 
#' for parameters training
#' @param mode Character, mode for data trimming to select the chraracteristic peaks. 
#' Default is 'ssm_trim'. Users could select random trimed according to mz value (mz_random) or 
#' RT value (rt_random). Besides, specific peaks at certain mz (mz_specific) or 
#' RT (rt_specific) could also be extracted. 'none' will not trim the data.
#' @param mz Numeric, mz value(s) for specific selection. Positive values means including and 
#' negative value means excluding.
#' @param mzdiff Numeric, the deviation (ppm) of mz value(s).
#' @param rt Numeric, rt value for specific selection. Positive values means including 
#' and negative value means excluding.
#' @param rtdiff Numeric, the deviation (seconds) of rt value(s).
#' @param rt.idx Numeric, the relative rt (retention time) range, from 0 to 1. 1 means all retention time
#' will be retained, while 0 means none. Default is 1/15. If default rt.idx produce too few peaks, 
#' please consider increasing this value.
#' @param write Logical, if true, will write the trimed data to the directory 'trimed' folder 
#' in the datapath. The data in memory will be kept.
#' @param plot Logical, if ture, will plot the chromatogram of the trimed data.
#' @export
#' @import MSnbase
#' @import progress
#' @import Biobase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

PerformDataTrimming<-function(datapath, mode="ssm_trim", write=F, mz, mzdiff, rt, rtdiff, 
                              rt.idx=1/15, plot=T){
  
  require(progress);require(Biobase);
  
  start.time<-Sys.time();
  
  dda_file1 <- list.files(datapath, recursive = T, full.names = TRUE)
  
  pd <- data.frame(sample_name = sub(basename(dda_file1), pattern = ".mzXML",
                                     replacement = "", fixed = TRUE),
                   stringsAsFactors = FALSE)
  
  message("Data Loading...")
  
  raw_data <- #suppressMessages(try(
    read.MSdata(dda_file1, pdata = new("NAnnotatedDataFrame", pd), msLevel. = 1, mode = "inMemory")#,
   # silent = T))
  
  message("Data Loaded !")
  
  if (!mode=="none"){
    ## Data Trim
    a<-suppressMessages(unlist(lapply(ls(raw_data@assayData), FUN=function(x){unlockBinding(sym = x,env = raw_data@assayData)})))
    ms_list<-sapply(ls(raw_data@assayData),FUN=function(x) raw_data@assayData[[x]]@mz)
    
    message("Data Trimming...")
    
    if (missing(rt.idx)){
      rt.idx <- 1/15
    };
    
    if (mode=="ssm_trim"){
      trimed_MSnExp<-ssm_trim(raw_data,ms_list,rt.idx=rt.idx)
    }
    
    if (mode=="mz_random"){
      suppressWarnings(try(trimed_MSnExp<-mz.trim_random(raw_data,ms_list),silent = T))
    }
    
    if (mode=="rt_random"){
      suppressWarnings(try(trimed_MSnExp<-rt.trim_random(raw_data,ms_list),silent = T))
    }
    
    if (mode=="mz_specific"){
      trimed_MSnExp<-mz.trim_specific(raw_data,ms_list,mz,mzdiff=mzdiff)
    }
    
    if (mode=="rt_specific"){
      trimed_MSnExp<-rt.trim_specific(raw_data,ms_list,rt,rtdiff=rtdiff)
    }
    
    message("Data Trimmed !")
    
    # remove the empty scans in the ms data
    
    trimed_MSnExp<-.emptyscan.remove(trimed_MSnExp,ms_list)
    
  }else{
    # keep the data without trimming.
    trimed_MSnExp<-raw_data
    
  }
  if (write==T){
    message("Data Writing...")
    
    writenames<-paste0(datapath,"/trimmed/Trimmed_",pd$sample_name,".mzML",sep = "")
    dir.create(paste0(datapath,"/trimmed",collapse = ""))
    suppressMessages(writeMSData(trimed_MSnExp, writenames, outformat = "mzml"))
    
    message("Data Writing Finished !")
  }
  if (plot==T){
    require(RColorBrewer);
    
    message("Chromatogram Plotting Begin...")
    
    ch.xdata<-chromatogram(trimed_MSnExp)
    group.col<-paste0(brewer.pal(length(trimed_MSnExp@processingData@files),"Blues"))
    plot(ch.xdata,col=group.col[1:length(trimed_MSnExp@processingData@files)])
  }
  
  message("Data Trimming Finished !")
  
     end.time<-Sys.time();
  message("Time Spent In Total:",round((as.numeric(end.time) - as.numeric(start.time))/60, 1),"mins","\n");
  
  return(trimed_MSnExp)
}

#' Standards Simulation Method
#' @description Whole mass spectra will be divided as 4 bins according to the mz range. Trimming 
#' the raw with slide window method in every bins and retained the windows with highest scan intensity
#' and remove other scan signal in mz dimension. Then the data will be trimed again in the RT dimension
#' with slide window method. The window with highest intensity scans will be kept. After the timming
#' alongside mz and RT dimension, the peaks not only the high intensity peaks, but also the relatively 
#' low intensity peaks will also be retained as the 'simulated standards' data for parameters optimization.
#' @param raw_data MSnExp object, the raw data that has been read in memory.
#' @param ms_list List, the names list of all scans.
#' @param rt.idx Numeric, the retention time percentage, from 0 to 1. Default is 1/15.
#' @import progress
#' @import BiocParallel
#' @import Biobase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

ssm_trim <- function(raw_data, ms_list, rt.idx){
  
  # mzdata_points<-unique(unlist(ms_list))
  
  
  # Slide window to choose the high abundance bins in every districts
  message("MS data Parsing ...")
  
  #if(length(names(ms_list))>1000){
  #  ms_list_s<-sort(sample(names(ms_list),1000))
  #} else {
  #  ms_list_s<-sort(names(ms_list))
  #}
  
  spectra_mz <- unlist(lapply(MSnbase::spectra(raw_data),mz))
  
  highestmz<-max(spectra_mz)
  lowestmz<-min(spectra_mz)
  
  # Split MS into 5 large districts
  bins.boundary<-seq(from=lowestmz, to= highestmz,length.out = 5)
  
  
  if (length(spectra_mz)>1000000){
    rannum<- sort(sample(length(spectra_mz),ceiling(length(spectra_mz)/50)),decreasing = F)
  } else if (length(spectra_mz)>100000){
    rannum<-sort(sample(length(spectra_mz),ceiling(length(spectra_mz)/5)),decreasing = F)
  } else {
    rannum<-seq(length(spectra_mz))
  }
  
  spectra_abundance <- unlist(lapply(MSnbase::spectra(raw_data),intensity))[rannum]
  spectra_mz <- spectra_mz[rannum]
  
  spectra_mz_set <- sapply(1:4,FUN=function(ii){
    
    spectra_mz[spectra_mz > bins.boundary[ii] & spectra_mz <  bins.boundary[ii+1]]
    
  })
  
  spectra_abundance_set <- sapply(1:4,FUN=function(ii){
    
    spectra_abundance[spectra_mz > bins.boundary[ii] &  spectra_mz  <  bins.boundary[ii+1]]
    
  })
  
  rm(spectra_abundance); rm(spectra_mz)
  
  good.bins.list<-bplapply(c(1:4),
                           FUN = function(i,spectra_abundance_set,spectra_mz_set,bins.boundary){
                             
                             binsb.low<-bins.boundary[i];
                             
                             binsb.high<-bins.boundary[i+1];
                             
                             w<-1
                             
                             bins.width<-(binsb.high-binsb.low)/5;
                             
                             wind.up<-wind.down<-0
                             
                             inten.sum.set<-numeric()
                             
                             while (!wind.up>binsb.high) {
                               
                               wind.down<-binsb.low+(w-1);
                               
                               wind.up<-binsb.low+bins.width+(w-1);
                               
                               #inten.sum<-numeric()
                               
                               
                               inten.sum.set<-c(inten.sum.set,
                                                sum (spectra_abundance_set[[i]]
                                                     [spectra_mz_set[[i]] > wind.down & spectra_mz_set[[i]] < wind.up]))
                               
                               #for (j in 1:length(ms_list_s)){
                               #  mz.set<-raw_data@assayData[[ms_list_s[j]]]@mz
                               #  a1<-which(sapply(mz.set, FUN=function(x){x>wind.down && x<wind.up}))
                               #  inten.sum<-c(inten.sum,sum(raw_data@assayData[[ms_list_s[j]]]@intensity[a1]))
                               #}
                               
                               #inten.sum.set<-c(inten.sum.set,sum(inten.sum))
                               w<-w+1;
                             }
                             
                             selected.bin.down<-binsb.low+which(max(inten.sum.set)==inten.sum.set)-1;
                             selected.bin.up<-binsb.low+which(max(inten.sum.set)==inten.sum.set)-1+bins.width
                             
                             return(list(selected.bin.down,selected.bin.up))
                             
                           },
                           spectra_abundance_set=spectra_abundance_set,
                           spectra_mz_set=spectra_mz_set,
                           bins.boundary=bins.boundary,
                           BPPARAM = SnowParam(type="SOCK"))
  
  message("MS Data Pasing Done !")
  
  # Remove the peaks out of the bins
  message("M/Z Data Trimming...")
  pb <- progress_bar$new(format = "Data Trimming in MZ Dimension [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 80)
  
  for (i in 1:length(ms_list)){
    pb$tick();
    
    ms.set<-raw_data@assayData[[names(ms_list)[i]]]@mz
    k<-which(sapply(ms.set,FUN = function(x){(x > good.bins.list[[1]][[1]] && x < good.bins.list[[1]][[2]]) | (x > good.bins.list[[2]][[1]] && x < good.bins.list[[2]][[2]]) | (x > good.bins.list[[3]][[1]] && x < good.bins.list[[3]][[2]]) | (x > good.bins.list[[4]][[1]] && x < good.bins.list[[4]][[2]])}))
    
    raw_data@assayData[[names(ms_list)[i]]]@mz<-raw_data@assayData[[names(ms_list)[i]]]@mz[k];
    raw_data@assayData[[names(ms_list)[i]]]@intensity<-raw_data@assayData[[names(ms_list)[i]]]@intensity[k];
    raw_data@assayData[[names(ms_list)[i]]]@tic<-sum(raw_data@assayData[[names(ms_list)[i]]]@intensity);
    raw_data@assayData[[names(ms_list)[i]]]@peaksCount<-length(raw_data@assayData[[names(ms_list)[i]]]@mz)
  }
  
  message("M/Z Data Trimming Done !")
  
  ## Trimed data outside the RT bins
  rt_set<-sapply(names(ms_list),FUN=function(x) raw_data@assayData[[x]]@rt)
  rt.bin.width<-(max(rt_set)-min(rt_set))*rt.idx
  
  # Slide window to determin the location of the RT trim boundary
  tic<-1;w<-0;rt.window.min<-min(rt_set);tic.sum<-numeric()
  
  while (!rt.window.min>max(rt_set)) {
    rt.window.min<-min(rt_set)+w*0.75
    rt.window.max<-min(rt_set)+rt.bin.width+w*0.75
    rt.name.set<-names(which(sapply(rt_set,FUN = function(x){x>rt.window.min && x<rt.window.max})))
    
    if(!identical(rt.name.set,character(0))){
      tic.sum<-c(tic.sum,sum(sapply(rt.name.set,FUN=function(x){raw_data@assayData[[x]]@tic})))
    }
    
    w<-w+1
  }
  
  rt.boundary.lowlimit<-min(rt_set)+which(max(tic.sum)==tic.sum)[ceiling(length(which(max(tic.sum)==tic.sum))/2)]*0.75
  rt.boundary.uplimit<-min(rt_set)+which(max(tic.sum)==tic.sum)[ceiling(length(which(max(tic.sum)==tic.sum))/2)]*0.75+rt.bin.width
  
  message("Trim data outside the RT bins...")
  
  pb <- progress_bar$new(format = "Data Trimming in RT Dimension [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 80)
  ncount<-numeric();
  
  for (w in 1:length(ms_list)){
    pb$tick();
    if (raw_data@assayData[[names(ms_list)[w]]]@rt>rt.boundary.lowlimit && raw_data@assayData[[names(ms_list)[w]]]@rt<rt.boundary.uplimit){
      ncount<-c(ncount,w)
    }
  }
  
  for (j in 1:length(ms_list)){
    if (!(j %in% ncount)){
      raw_data@assayData[[names(ms_list)[j]]]@mz<-raw_data@assayData[[names(ms_list)[j]]]@intensity<-as.double();
      raw_data@assayData[[names(ms_list)[j]]]@peaksCount<-as.integer(0);
      raw_data@assayData[[names(ms_list)[j]]]@tic<-as.double(0);
    }
  }
  return(raw_data)
}

#' Data trimming Method Based on Random MS
#' @description Trim raw data scan signal randomly in the mz dimension.
#' @param raw_data MSnExp object, the raw data that has been read in memory.
#' @param ms_list List, the names list of all scans.
#' @import progress
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

mz.trim_random <- function(raw_data, ms_list){
  
  mzdata_points <- unique(unlist(ms_list))
  highestmz <- max(mzdata_points)
  lowestmz <- min(mzdata_points)
  
  # Randomly select 100 mz center points
  mzdata_points <- sample(unique(mzdata_points),size = 100)
  
  pb <- progress_bar$new(format = "Data Trimming [:bar] :percent Time left: :eta", total = length(raw_data@assayData), clear = T, width= 60)
  
  for (i in 1:length(raw_data@assayData)){
    pb$tick();
    mz.data<-ms_list[[i]];
    remove.num.set<-numeric();k<-1
    
    for (j in 1:length(mz.data)){
      if (!(T %in% (abs(mz.data[j]-mzdata_points)<=(highestmz-lowestmz)/10000))){
        remove.num.set[k]<-j;
        k<-k+1
      }
    }
    raw_data@assayData[[names(ms_list[i])]]@mz<-mz.data[-remove.num.set];
    raw_data@assayData[[names(ms_list[i])]]@intensity<-raw_data@assayData[[names(ms_list[i])]]@intensity[-remove.num.set];
    raw_data@assayData[[names(ms_list[i])]]@tic<-sum(raw_data@assayData[[names(ms_list[i])]]@intensity);
    raw_data@assayData[[names(ms_list[i])]]@peaksCount<-length(raw_data@assayData[[names(ms_list[i])]]@mz);
  }
  return(raw_data)
}

#' Data trimming Method Based on Random RT
#' @description Trim raw data scan signal randomly in the RT dimension.
#' @param raw_data MSnExp object, the raw data that has been read in memory.
#' @param ms_list List, the names list of all scans.
#' @import progress
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

rt.trim_random <- function(raw_data, ms_list){
  
  rt_set<-sapply(names(ms_list),FUN=function(x) raw_data@assayData[[x]]@rt)
  rt_begin<-min(rt_set);
  rt_end<-max(rt_set);
  
  xn2<-character();xn3<-xn<-xcount<-ncount<-rt_var_sum<-numeric();
  rt_width_aver<-(rt_end-rt_begin)/200
  xn_list<-list()
  
  pb <- progress_bar$new(format = "Data Trimming [:bar] :percent Time left: :eta", total = 200, clear = T, width= 60)
  
  for (w in 1:200){
    pb$tick();
    
    rt_var<-rt_begin+rt_width_aver*(w-0.5);
    rt_var_sum<-c(rt_var_sum,rt_var);
    
    xn_list[w][[1]]<-0
  }
  
  xn<-sapply(names(ms_list),FUN= function(x) which(abs(raw_data@assayData[[x]]@rt-rt_var_sum)<=(rt_width_aver/2+0.000001)))
  
  for (i in 1:length(xn)){xn_list[[xn[i]]]<-sum(xn_list[[xn[i]]],raw_data@assayData[[names(xn[i])]]@peaksCount)}
  
  rt_peakcounts<-unlist(xn_list)
  #rtslots<-rt_var_sum[sapply(tail(sort(unlist(xn_list)),10),FUN = function(x){which(x==rt_peakcounts)})]
  
  # randomly select 10 RT slots
  rtslots<-rt_var_sum[sapply(sample(unlist(xn_list),10),FUN = function(x){which(x==rt_peakcounts)})]
  
  for (j in 1:length(ms_list)){
    
    if (T %in% (abs(raw_data@assayData[[names(ms_list)[j]]]@rt-rtslots)<=(rt_width_aver/2+0.000001))){
      xn2<-c(xn2,names(ms_list)[j]);xn3<-c(xn3,j)
    }
  }
  
  for (k in 1:length(ms_list))  {
    
    if (!(k %in% xn3)){
      raw_data@assayData[[names(ms_list)[k]]]@peaksCount<-as.integer(0);
      raw_data@assayData[[names(ms_list)[k]]]@mz<-as.double();
      raw_data@assayData[[names(ms_list)[k]]]@intensity<-as.double();
      raw_data@assayData[[names(ms_list)[k]]]@tic<-as.double(0);
    }
  }
  return(raw_data)
}

#' Data trimming Method Based on Specific MS
#' @description Trim data based on specific mz values. Positive values will be specially retained, 
#' while the negative values will be removed.
#' @param raw_data MSnExp object, the raw data that has been read in memory.
#' @param ms_list List, the names list of all scans.
#' @param mz Numeric, the specifric mz value that will be kept or removed.
#' @param mzdiff Numeric, the deviation (ppm) for the 'mz' values. Default is 100.
#' @import progress
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

mz.trim_specific<-function(raw_data, ms_list, mz, mzdiff=100){
  
  #mz<-c(72.323,100,120,240,360,480,720,780.524,1080);
  if (missing(mz)){
    stop("'mz' must be provided for mz_specific mode as a numeric vector!")
  }
  
  if (missing(mzdiff)){
    stop("'mzdiff' must be provided for mz_specific mode as a numeric value!")
  }
  
  if (mz[1] == 0 | mzdiff==0){
    stop("'mz' or 'mzdiff' cannot be zero!")
  }
  
  mz.neg<-mz[which(mz<0)];
  mz.pos<-mz[which(mz>0)];
  
  if (!identical(mz.pos,numeric(0))){
    pb <- progress_bar$new(format = "Data Trimming_keeping [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 60)
    
    for (w in 1:length(ms_list)){
      pb$tick();
      xn<-unlist(sapply(mz.pos,FUN=function(x){which((abs(x-raw_data@assayData[[names(ms_list)[w]]]@mz)/(x)*1000000)<=mzdiff)}));
      
      raw_data@assayData[[names(ms_list)[w]]]@mz<-raw_data@assayData[[names(ms_list)[w]]]@mz[xn];
      raw_data@assayData[[names(ms_list)[w]]]@intensity<-raw_data@assayData[[names(ms_list)[w]]]@intensity[xn];
      raw_data@assayData[[names(ms_list)[w]]]@peaksCount<-length(raw_data@assayData[[names(ms_list)[w]]]@mz);
      raw_data@assayData[[names(ms_list)[w]]]@tic<-sum(raw_data@assayData[[names(ms_list)[w]]]@intensity);
    }
    
    for (w in 1:length(ms_list)){
      if (identical(raw_data@assayData[[names(ms_list)[w]]]@mz,numeric(0))){
        raw_data@assayData[[names(ms_list)[w]]]@mz<-as.double();
        raw_data@assayData[[names(ms_list)[w]]]@intensity<-as.double();
      }
    }
  } 
  
  if (!identical(mz.neg,numeric(0))){
    
    pb <- progress_bar$new(format = "Data Trimming_removing [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 60)
    
    for (w in 1:length(ms_list)){
      pb$tick();
      xn<-unlist(sapply(abs(mz.neg),FUN=function(x){which((abs(x-raw_data@assayData[[names(ms_list)[w]]]@mz)/(x)*1000000)<=mzdiff)}));
      
      if (!identical(xn, numeric(0))){
        raw_data@assayData[[names(ms_list)[w]]]@mz<-raw_data@assayData[[names(ms_list)[w]]]@mz[-xn];
        raw_data@assayData[[names(ms_list)[w]]]@intensity<-raw_data@assayData[[names(ms_list)[w]]]@intensity[-xn];
        raw_data@assayData[[names(ms_list)[w]]]@peaksCount<-length(raw_data@assayData[[names(ms_list)[w]]]@mz);
        raw_data@assayData[[names(ms_list)[w]]]@tic<-sum(raw_data@assayData[[names(ms_list)[w]]]@intensity);
      }
    }
    
    for (w in 1:length(ms_list)){
      
      if (identical(raw_data@assayData[[names(ms_list)[w]]]@mz,numeric(0))){
        raw_data@assayData[[names(ms_list)[w]]]@mz<-as.double();
        raw_data@assayData[[names(ms_list)[w]]]@intensity<-as.double();
      }
    }
  }
  return(raw_data)
}

#' Data trimming Method Based on Specific RT
#' @description Trim data based on specific RT values. Positive values will be specially retained, 
#' while the negative values will be removed.
#' @param raw_data MSnExp object, the raw data that has been read in memory.
#' @param ms_list List, the names list of all scans.
#' @param mz Numeric, the specifric RT value that will be kept or removed.
#' @param mzdiff Numeric, the deviation (ppm) for the 'rt' values. Default is 10.
#' @import progress
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

rt.trim_specific<-function(raw_data,ms_list,rt,rtdiff=10){
  
  if (missing(rt)){
    stop("'rt' must be provided for rt_specific mode with a vector !")
  }
  
  if (missing(rtdiff)){
    stop("'rtdiff' must be provided for rt_specific mode with a numeric !")
  }
  
  if (rt[1] == 0 | rtdiff==0){
    stop("'rt' or 'rtdiff' can not be zero !")
  }
  
  if (rt[1] > 0){
    pb <- progress_bar$new(format = "Data Trimming [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 60)
    
    ncount<-numeric();
    for (w in 1:length(ms_list)){
      pb$tick();
      if (T %in% (abs(raw_data@assayData[[names(ms_list)[w]]]@rt-rt)<=rtdiff)){
        ncount<-c(ncount,w)
      }
    }
    
    for (j in 1:length(ms_list)){
      
      if (!(j %in% ncount)){
        raw_data@assayData[[names(ms_list)[j]]]@mz<-raw_data@assayData[[names(ms_list)[j]]]@intensity<-as.double();
        raw_data@assayData[[names(ms_list)[j]]]@peaksCount<-as.integer(0);
        raw_data@assayData[[names(ms_list)[j]]]@tic<-as.double(0);
      }
      
    }
  } else {
    
    pb <- progress_bar$new(format = "Data Trimming [:bar] :percent Time left: :eta", total = length(ms_list), clear = T, width= 60)
    
    ncount<-numeric();
    for (w in 1:length(ms_list)){
      pb$tick();
      if (T %in% (abs(raw_data@assayData[[names(ms_list)[w]]]@rt-abs(rt))<=rtdiff)){
        ncount<-c(ncount,w)
      }
      
    }
    for (j in 1:length(ms_list)){
      
      if (j %in% ncount){
        raw_data@assayData[[names(ms_list)[j]]]@mz<-raw_data@assayData[[names(ms_list)[j]]]@intensity<-as.double();
        raw_data@assayData[[names(ms_list)[j]]]@peaksCount<-as.integer(0);
        raw_data@assayData[[names(ms_list)[j]]]@tic<-as.double(0);
      }
    }
  }
  return(raw_data)
}

# Function for 'Empty scan' removal
.emptyscan.remove<-function(raw_data,ms_list){
  
  name.set<-sort(names(raw_data@assayData))
  
  sample.set<-unique(sapply(c(1:length(name.set)),FUN=function(x){sub(".[S][0-9]+","",name.set[x])}))
  assayData<-list();assayData1<-new.env();ki<-k<-1
  
  df.data0<-raw_data@featureData@data;
  df.data<-data.frame();
  
  nchar.num<-nchar(names(ms_list)[1])-4
  suppressWarnings(for (i in 1:length(raw_data@assayData)){
    
    if (!raw_data@assayData[[name.set[i]]]@tic==0){
      
      if (!k==1){
        if (!sample.set[which(sub(".[S][0-9]+","",name.set[i])==sample.set)]==sub(".[S][0-9]+","",names(assayData)[ki-1])){
          k<-1
        } 
      }
      
      name0<-paste0(sample.set[which(sub(".[S][0-9]+","",name.set[i])==sample.set)],".S",formatC(k, width=nchar.num, digits = nchar.num, flag="0"))
      
      assayData[name0]<-raw_data@assayData[[name.set[i]]];
      assayData[[name0]]@acquisitionNum<-as.integer(k);
      assayData[[name0]]@scanIndex<-as.integer(k);
      
      df.data[ki,1]<-k;
      row.names(df.data)[ki]<-name0;
      
      k<-k+1;ki<-ki+1;
    }
  })
  
  list2env(assayData,envir = assayData1)
  raw_data@assayData<-assayData1
  names(df.data)<-"spectrum"
  
  metaData<-raw_data@featureData@varMetadata;
  featureData<-AnnotatedDataFrame(data=df.data, varMetadata=metaData);
  raw_data@featureData<-featureData;
  
  return(raw_data)
}

PerformMSDataOutput<-function(raw_data){
  message("Data Writing...")
  writenames<-paste0("new_",raw_data@phenoData@data[["sample_name"]],sep = "")
  #dir.create(paste0(datapath,"/trimed",collapse = ""))
  suppressMessages(writeMSData(raw_data, writenames, outformat = "mzxml"))
  message("Output Data:","\n");
  message(writenames)
  message("Data Writing Finished !")
}

# Function for 3D ms plotting
plot.MS_3D<-function(object) {
  
  dd <- as(object, "data.frame")
  
  ms <- NULL ## get rid of 'no visible global function definition' note
  par.set <- list(box.3d = list(lwd=.2))
  
  cloud(intensity ~ mz + rt , data = dd,
        type="h",
        scales= list(
          arrows=FALSE,
          cex=.65,
          draw=TRUE),
        aspect=c(.8, 1),
        group = ms,
        zoom = 1, 
        par.settings = par.set,
        axis.line = list(col = "transparent"),
        xlab="M/Z", ylab="Retention time", zlab=NULL)
  
}