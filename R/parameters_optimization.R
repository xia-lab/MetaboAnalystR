#' @title Perform Parameters Optimization
#' @description This function is used to optimize the critical parameters of peak picking and alignment for 
#' the following data processing. It utilizes the trimed data and the internal instrument-specific parameters.
#' Parallel computing will be performed. The number of cores user want to use could be specified.
#' @param raw_data MSnExp object, can be the (trimmed) data in memory produced by 'PerformDataTrimming' or the 
#' orignal data read by ImportRawMSData with 'inMemory" mode.
#' @param param List, Parameters defined by 'SetPeakParam' function.
#' @param method Character, method of parameters optimization, including "DoE' only. Default is "DoE". Other method 
#' is under development.
#' @param ncore Numeric, CPU threads number used to perform the parallel based optimization. If thers is memory issue,
#' please reduce the 'ncore' used here. For default, 2/3 CPU threads of total will be used.
#' @export
#' @import MSnbase
#' @import progress
#' @import parallel
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

PerformParamsOptimization<-function(raw_data, param=p0, method="DoE", ncore=4){
  
  #require(xcms);require(progress);require(parallel);
  #suppressPackageStartupMessages(require(IPO));
  #suppressPackageStartupMessages(require(Autotuner));
  
  start.time<-Sys.time();
  
  if (missing(param)){
    stop("Please provide the param with 'SetPeakParam' function !")
  } else if (missing(raw_data)){
    stop("Please provide the data of MSnExp format!" )
  } else if (missing(method)){
    method<-"DoE";
    print("DoE Optimization Starting Now!")
  };
    
  if (missing(ncore)){
    ncore<-detectCores();
    message("'ncore' is absent, will use 2/3 CPU threads of total!")
  };
  
  
  # Define the global Parallel cores for Single Core function mode
  if (ncore == 1){
      if (.Platform$OS.type=="unix"){
      register(bpstart(MulticoreParam(ncore)))
    } else {
      register(bpstart(SnowParam(ncore)))
    };
  }

  ## Optimize the noise and prefilter indexes with AutoTuner
  if (param[["Peak_method"]] == "centWave"){
    
    print("Evaluating Noise level...");
    p2<-Noise_evaluate(raw_data);
    
    param[["ppm"]]<-round(p2$ppm,2)
    param[["noise"]]<-round(p2$noise,2);
    param[["prefilter"]]<-round(p2$prefilter,2);
    param[["value_of_prefilter"]]<-round(p2$value_of_prefilter,2);

  }
  
  if (method=="DoE"){
    p1<-optimize.xcms.doe(raw_data,param=param,ncore=ncore)
  };
  
  if (method=="OVAT"){
    stop("Only DoE is supported for now. Other Optimization Model will be supported later.")
    p1<-optimize.xcms.ovat(raw_data,param=param,ncore=ncore)
  };
  
  end.time<-Sys.time();
  message("Time Spent In Total:",round((as.numeric(end.time) - as.numeric(start.time))/60, 1),"mins","\n");
  
  return(p1)
}

#' @title Overall Funtion for DoE
#' @description This function is the overall function to handle the starting of the optimization process and 
#' pre-define the parameters' range according to the input of the parameters.
#' @param raw_data MSnExp object, The trimmed or original data input for optimization.
#' @param param List, the parameters lists set by 'SetPeakParam' function. The noise, prefilter and ppm values should 
#' be defined by AutoTuner in the previous steps.
#' @param ncore Numeric, core number used to perform the parallel based optimization. 
#' @import MSnbase
#' @import progress
#' @import parallel
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

optimize.xcms.doe<-function(raw_data, param, ncore=8){
  #### Parameters Setting for optimization!------
  if (is.null(param)){
    stop("Please provide the param with 'SetPeakParam' function !")
  } else {Parameters<-param};
 
  
  ## Define the range of the parameters for optimization
  if (Parameters$Peak_method=="centWave" && Parameters$RT_method=="peakgroup"){
    ## Keep these Parameters
          Parameters$value_of_prefilter <- Parameters$value_of_prefilter;
          ## Parameters for peak picking
          Parameters$max_peakwidth <- c(Parameters$max_peakwidth*0.5,Parameters$max_peakwidth*2)
          Parameters$min_peakwidth <- c((Parameters$min_peakwidth)*0.5,(Parameters$min_peakwidth)*2)
          #Parameters$ppm <- c(Parameters$ppm*0.5,Parameters$ppm*1.5)
          Parameters$mzdiff <- c(-Parameters$mzdiff*1.2, Parameters$mzdiff*1.2)
          Parameters$snthresh<-c(Parameters$snthresh*0.75,Parameters$snthresh*5)
          ## Parameters for Alignment
          Parameters$bw<-c(Parameters$bw*0.5,Parameters$bw*1.5)
  } else 
    if (Parameters$Peak_method=="centWave" && Parameters$RT_method=="obiwarp"){
      ## Keep these Parameters
          Parameters$value_of_prefilter <- Parameters$value_of_prefilter;
          ## Parameters for peak picking
          Parameters$max_peakwidth <- c(Parameters$min_peakwidth*2,Parameters$max_peakwidth*2)
          Parameters$min_peakwidth <- c(1,(Parameters$min_peakwidth)*2)
          #Parameters$ppm <- c(1,Parameters$ppm*2);
          Parameters$mzdiff <- c(-Parameters$mzdiff*2, Parameters$mzdiff*2);
          Parameters$snthresh<-c(1,Parameters$snthresh*10+1);
          ## Parameters for Alignment
          Parameters$bw<-c(Parameters$bw*0.5,Parameters$bw*1.5)
    } else 
    if (Parameters$Peak_method=="matchedFilter" && Parameters$RT_method=="peakgroup"){
        ## Parameters for peak picking
          Parameters$fwhm <- c((Parameters$fwhm)*0.8,(Parameters$fwhm)*1.2)
          Parameters$sigma <- c(Parameters$sigma*0.8,Parameters$sigma*1.2)
          Parameters$steps <- c(Parameters$steps-1,Parameters$steps+1)
          Parameters$mzdiff <- c(-Parameters$mzdiff*2, Parameters$mzdiff*2)
          Parameters$snthresh<-c(1,Parameters$snthresh*10)
          ## Parameters for Alignment
          Parameters$bw<-c(Parameters$bw*0.5,Parameters$bw*1.5)
      } else 
    if (Parameters$Peak_method=="matchedFilter" && Parameters$RT_method=="obiwarp"){
          ## Parameters for peak picking
          Parameters$fwhm <- c((Parameters$fwhm)*0.8,(Parameters$fwhm)*1.2)
          Parameters$sigma <- c(Parameters$sigma*0.8,Parameters$sigma*1.2)
          Parameters$steps <- c(Parameters$steps-1,Parameters$steps+1)
          Parameters$mzdiff <- c(-Parameters$mzdiff*2, Parameters$mzdiff*2)
          Parameters$snthresh<-c(1,Parameters$snthresh*10)
          ## Parameters for Alignment
          Parameters$bw<-c(Parameters$bw*0.5,Parameters$bw*1.5)
        } else {
    stop("There must be something wrong about the Peak_method value in your primary params set !")
        };
  
  #### Start to Optimize !
  result <- optimizxcms.doe.peakpicking(object = raw_data, params = Parameters, 
                                        BPPARAM = bpparam(),
                                        nSlaves = ncore, subdir = NULL, plot = F);
  
  optimizedxcmsObject <- result$best_settings$xset;
  message("Optimization Finished !");
  
  ##Parameters Out-put
  peakParams2<-list();
  peakParams2$best_parameters<-result[["best_settings"]][["parameters"]];
  peakParams2$data<-optimizedxcmsObject;
  
  message("Parameters Optimization Finished !");
  
  return(peakParams2)
}

#' @title Core Optimization Function of DoE
#' @description This function is the core for parameters optimization with Design of Experiment (DoE)
#' method.
#' @param objet MSnExp object, the trimmed or the original data.
#' @param param List, the parameters lists set by 'SetPeakParam' function. The noise, prefilter and ppm values should 
#' be defined by AutoTuner in the previous steps.
#' @param nSlave Numeric, core number used to perform the parallel based optimization.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @param plot Logical, weather to plot the Contours plots of the DoE results.
#' @import MSnbase
#' @import progress
#' @import parallel
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

optimizxcms.doe.peakpicking <- function(object = NULL, params = params, 
                                        BPPARAM = bpparam(), nSlaves = 4, plot = F,...) {
  
  isotopeIdentification = c("IPO")
  centWave <- is.null(params$fwhm)  
  history <- list()
  iterator = 1 
  best_range <- 0.25
  
  object_mslevel<-PeakPicking_prep(object)
  
  while(iterator < 20) {#Forcely stop to ensure the reasonability!
    print(paste0("Round:",iterator))
    message("DoE Running Begin...")
    
    # Parallel is unstable for matchedFilter Method, force to use only ne core
    #if (params[["Peak_method"]]=="matchedFilter"){
    #  nSlaves<-1;
    #  message("Parallel for method 'matchedFilter' is unstable, set ncore as 1 !")
    #}
    
    mSet_OPT <- 
      ExperimentsCluster_doe(
        object = object,
        object_mslevel=object_mslevel,
        params = params,
        isotopeIdentification = isotopeIdentification,
        BPPARAM = BPPARAM,
        nSlaves = nSlaves
      )
    
    ### Normalize the PPS and CV in mSet_OPT
    PPS.set<-as.numeric(sapply(1:nrow(mSet_OPT[["response"]]),FUN=function(x){
      mSet_OPT[["response"]][x,5]
    }));
    
    CV.set<-as.numeric(sapply(1:nrow(mSet_OPT[["response"]]),FUN=function(x){
      mSet_OPT[["response"]][x,6]
    }))
    
    RCS.set<-as.numeric(sapply(1:nrow(mSet_OPT[["response"]]),FUN=function(x){
      mSet_OPT[["response"]][x,7]
    }))
    
    GS.set<-as.numeric(sapply(1:nrow(mSet_OPT[["response"]]),FUN=function(x){
      mSet_OPT[["response"]][x,8]
    }))
    
    GaussianSI.set<-as.numeric(sapply(1:nrow(mSet_OPT[["response"]]),FUN=function(x){
      mSet_OPT[["response"]][x,9]
    }))
    
    index.set<-list(CV=CV.set,RCS=RCS.set,GS=GS.set,GaussianSI=GaussianSI.set)
    
    # Normalize the index
    CV.set.normalized<-(CV.set-min(CV.set))/(max(CV.set)-min(CV.set))
    RCS.set.normalized<-(RCS.set-min(RCS.set))/(max(RCS.set)-min(RCS.set))
    GS.set.normalized<-(GS.set-min(GS.set))/(max(GS.set)-min(GS.set))
    GaussianSI.set.normalized<-GaussianSI.set
    
    # Calculate QCoE
    QCoE<-CV.set.normalized*0.2+RCS.set.normalized*0.4+
      GS.set.normalized*0.4
    # Calculate QS
    QS<-PPS.set*QCoE*GaussianSI.set.normalized
    
    tmp_matrix<-mSet_OPT[["response"]]
    tmp_matrix<-cbind(tmp_matrix,PPS.set,CV.set.normalized,RCS.set.normalized,GS.set.normalized,
                      GaussianSI.set.normalized,QCoE,QS)
    mSet_OPT[["response"]]<-tmp_matrix
    
    message("Round ",iterator," Finished !")

    mSet_OPT <-
      Statistic_doe(
        object = object,
        object_mslevel=object_mslevel,
        isotopeIdentification = isotopeIdentification,
        BPPARAM = BPPARAM,
        subdir = NULL,
        plot = F,
        mSet_OPT = mSet_OPT,
        iterator = iterator,
        index.set = index.set,
        useNoise = params[["noise"]]
      )
    
    history[[iterator]] <- mSet_OPT     
    
    params <- mSet_OPT$params
    
    if(!resultIncreased_doe(history)) {
      message("No Increase Stopping !")
      maxima <- 0
      max_index <- 1
      for(i in 1:length(history)) {
        if(history[[i]]$max_settings[1] > maxima) {
          maxima <- history[[i]]$max_settings[1]
          max_index <- i
        }
      }
      
      xcms_parameters <- 
        as.list(decodeAll(history[[max_index]]$max_settings[-1],
                          history[[max_index]]$params$to_optimize))      
      
      xcms_parameters <- combineParams(xcms_parameters, 
                                       params$no_optimization)
      
      if(!is.list(xcms_parameters))
        xcms_parameters <- as.list(xcms_parameters)
      
      best_settings <- list()
      best_settings$parameters <- xcms_parameters
      best_settings$xset <- history[[max_index]]$xset
      
      target_value <- history[[max_index]]$QS 
      best_settings$result <- target_value
      history$best_settings <- best_settings
      
      message("best parameter settings:")
      message(paste(rbind(paste(names(xcms_parameters), 
                                sep="", ": "), 
                          paste(xcms_parameters, sep="", "\n")), sep=""))
      
      return(history)
      
    }
    
    for(i in 1:length(params$to_optimize)) {
      parameter_setting <- mSet_OPT$max_settings[i+1]
      bounds <- params$to_optimize[[i]] 
      fact <- names(params$to_optimize)[i]
      
      min_factor <- 
        ifelse(fact=="min_peakwidth", 3, 
               ifelse(fact=="mzdiff", 
                      ifelse(centWave,-0.02, 0.001), 
                      ifelse(fact=="step",0.0005,
                             ifelse(fact=="bw",2,
                                    ifelse(fact=="snthresh",5,1)))))
      
       step_factor <- 
        ifelse(is.na(parameter_setting), 1.2, 
               ifelse((abs(parameter_setting) < best_range),  0.8, 
                      ifelse(parameter_setting==-1 & 
                               decode(-1, params$to_optimize[[i]]) ==
                               min_factor, 0.8, 1)))
      
       step <- (diff(bounds) / 2) * step_factor
      
      if(is.na(parameter_setting))
        parameter_setting <- 0
      
      new_center <- decode(parameter_setting, bounds)
      
      if((new_center-min_factor) > step) {
        new_bounds <- c(new_center - step, new_center + step) 
      } else {
        new_bounds <- c(min_factor, 2*step+min_factor) 
      }      
      
      names(new_bounds) <- NULL         
      
      if(names(params$to_optimize)[i] == "steps" | 
         names(params$to_optimize)[i] == "prefilter") {
        params$to_optimize[[i]] <- round(new_bounds, 0)
      } else { 
        params$to_optimize[[i]] <- new_bounds
      }
    } 
    
    if(centWave) {
     
      if(!is.null(params$to_optimize$min_peakwidth) | 
         !is.null(params$to_optimize$max_peakwidth)) {
        
        pw_min <- 
          ifelse(is.null(params$to_optimize$min_peakwidth), 
                 params$no_optimization$min_peakwidth, 
                 max(params$to_optimize$min_peakwidth))
        
        pw_max <- 
          ifelse(is.null(params$to_optimize$max_peakwidth), 
                 params$no_optimization$max_peakwidth, 
                 min(params$to_optimize$max_peakwidth))
        
        if(pw_min >= pw_max) {
          additional <- abs(pw_min-pw_max) + 1
          
          if(!is.null(params$to_optimize$max_peakwidth)) {		  
            params$to_optimize$max_peakwidth <- 
              params$to_optimize$max_peakwidth + additional
          } else {
            params$no_optimization$max_peakwidth <- 
              params$no_optimization$max_peakwidth + additional
          }
          
        }
      }
    
    }
    
    params <- attachList(params$to_optimize, params$no_optimization)	    
    iterator <- iterator + 1
    
  }
  
  params <- attachList(params$to_optimize, params$no_optimization)	    
  
  return(history)
  
}

#' @title Experiment Functions of DoE
#' @description This function is used to perform the test with Design of Experiment on the parameters dataset.
#' @param object MSnExp object, the trimmed or the original data.
#' @param object_mslevel List, the parsed metabolomics scans produced by PeakPicking_prep.
#' @param isotopeIdentification Character, IsotopeIdentidication method, usually includes 'IPO' and 'CAMERA'.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @param nSlave Numeric, core number used to perform the parallel based optimization.
#' @import MSnbase
#' @importFrom rsm decode.data ccd rsm
#' @import progress
#' @import parallel
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

ExperimentsCluster_doe <-function(object, object_mslevel,params, 
                                  isotopeIdentification, BPPARAM = bpparam(),nSlaves=4, ...) { 
  
  # To form a parameters combination table
  typ_params <- typeCastParams(params) 
  
  if(length(typ_params$to_optimize)>1) {
    design <- getCcdParameter(typ_params$to_optimize)  	
    param_design <- rsm::decode.data(design) 
  } else {
    design <- data.frame(run.order=1:9, a=seq(-1,1,0.25))
    colnames(design)[2] <- names(typ_params$to_optimize)
    param_design <- design
    param_design[,2] <- 
      seq(min(typ_params$to_optimize[[1]]), 
          max(typ_params$to_optimize[[1]]), 
          diff(typ_params$to_optimize[[1]])/8)
  }
  
  param_design <- combineParams(param_design, typ_params$no_optimization)   
  
  design_list <- apply(param_design,1,FUN = function(x){
    as.list(x)
  })
  
  tasks <- 1:nrow(design);
  
  if (.Platform$OS.type=="windows"){
    print("Your OS is Windows, there might be unexpected errors.")
    print("If there is some unexpected bugs, please reduce the 'core' as 1.")
  }
  
  
  # Parallel or single core
  if(nSlaves > 1) {
    
    # Memory optimization strategy
    ncount<-object@phenoData@data[["sample_name"]];
    data.size<-round(as.numeric(object.size(object)/1024/1024),1);
    if(.Platform$OS.type=="unix" ){
      memtotal <- try(ceiling(as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))/1024/1024),silent = T)
    }
    if(.Platform$OS.type=="windows"){
      memtotal <- ceiling(as.numeric(gsub("\r","",gsub("TotalVisibleMemorySize=","",system('wmic OS get TotalVisibleMemorySize /Value',intern=TRUE)[3])))/1024/1024)
    }
    
    if (class(memtotal) == "try-error"){
      memtotal <- 8;
    }
    
    if(data.size<1){
      data.size<-0.5
    }
    
    if (memtotal/data.size>30){
      nstepby<-ceiling(memtotal*0.75/(data.size*32))
    } else if (memtotal/data.size<30 && memtotal/data.size>15){
      nstepby<-ceiling(memtotal*0.5/(data.size*32))
    } else {
      nstepby<-ceiling(memtotal*0.25/(data.size*32))
    }
    
    nstep<-ceiling(length(tasks)/nstepby)
    
    ## Parallel Setting
    if('snow' %in% rownames(installed.packages())){
      unloadNamespace("snow")
    }
    
    cl_type<-getClusterType()
    cl <- parallel::makeCluster(nSlaves,type = cl_type)
    response <- matrix(0, nrow=length(design[[1]]), ncol=9)
    
    # Parallel space setting
  
    # parallel::clusterEvalQ(cl, library("MetaboAnlystR")) 
    # Do not need the whole package to memory to reduce the memory burden
    
    parallel::clusterExport(cl, optimize_function_list, envir = asNamespace("MetaboAnalystR"))
    # Setting progress bar and start the running loop
    pb <- progress_bar$new(format = "DoE Running [:bar] :percent Time left: :eta", total = nstep, clear = T, width= 75)
    
    for (w in 1:nstep){
      pb$tick();

      value.index<-tasks[c(1:nstepby)+(w-1)*nstepby];
      if (NA %in% tasks[c(1:nstepby)+(w-1)*nstepby]){
        value.index<-value.index[-which(is.na(tasks[c(1:nstepby)+(w-1)*nstepby]))]
      }

      response0 <- parallel::parSapply(
        cl = cl,
        X = value.index,
        FUN = SlaveCluster_doe,
        Set_parameters = design_list,
        object = object,
        object_mslevel=object_mslevel,
        isotopeIdentification = isotopeIdentification,
        BPPARAM = BPPARAM,
        USE.NAMES = FALSE
      )
      
      response[value.index,]<-t(response0)
    }
    
    parallel::stopCluster(cl)
  } else {
    response <-
      sapply(
        X = tasks,
        FUN = SlaveCluster_doe,
        Set_parameters = design_list,
        object = object,
        object_mslevel=object_mslevel,
        isotopeIdentification = isotopeIdentification,
        BPPARAM = BPPARAM
      )
    response <- t(response)
  }
  
  colnames(response) <- c("exp", "num_peaks", "notLLOQP", "num_C13", "PPS","CV","RCS","GS","GaussianSI")
  response <- response[order(response[,1]),]
  
  ret <- list()
  ret$params <- typ_params
  ret$design <- design
  ret$response <- response
  return(ret)
  
}

#' @title Analyze DoE Result
#' @param object MSnExp object, the trimmed or the original data.
#' @param object_mslevel List, the parsed metabolomics scans produced by PeakPicking_prep.
#' @param isotopeIdentification Character, IsotopeIdentidication method, usually includes 'IPO' and 'CAMERA'.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @param mSet_OPT List, the result produced by 'ExperimentsCluster'.
#' @param subdir Logical, weather to creat a sub-directory (if true) or not (if false).
#' @param plot Logical, weather to plot the Contours plots of the DoE results.
#' @param iterator Numeric, the round number of the DoE.
#' @param index.set List, the indexes set (including PPS, CV, RCS, GS and Gaussian Index) produced by 
#' ExperiemntCluster.
#' @param useNoise Numeric, the noise level removed to evalute the gaussian peak.
#' @import MSnbase
#' @import parallel
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

Statistic_doe <-function(object, object_mslevel, isotopeIdentification, 
                         BPPARAM = bpparam(), mSet_OPT, subdir = NULL ,plot = F,iterator, 
                         index.set,useNoise) {
  message("Model Parsing...")
  
  # Prepare parameters for model prediction
  params <- mSet_OPT$params
  resp <- mSet_OPT$response[, "QS"]
  
  # Creat the prediction model and get the predicted best results
  model <- createModel(mSet_OPT$design, params$to_optimize, resp)
  mSet_OPT$model <- model                  
  max_settings <- getMaximumLevels(mSet_OPT$model)
  
  tmp <- max_settings[1,-1] # first row without response
  tmp[is.na(tmp)] <- 1 # if Na (i.e. -1, and 1), show +1
  
  mSet_OPT$max_settings <- max_settings
  xcms_parameters <- 
    as.list(decodeAll(max_settings[-1], params$to_optimize))      
  xcms_parameters <- 
    combineParams(xcms_parameters, params$no_optimization)
  
  if(!is.list(xcms_parameters))
    xcms_parameters <- as.list(xcms_parameters)
  
  
  # Detect the peak features with the predicted best parameters
  mSet <- suppressMessages(calculateSet_doe(object = object, object_mslevel=object_mslevel, 
                                            Set_parameters = xcms_parameters,
                                            task = 1, BPPARAM = BPPARAM));
  xset <- mSet[["xcmsSet"]]
  # Calculate the various indexes
  
  mSet_OPT$xset <- xset
  mSet_OPT$PPS <- calcPPS2(xset, isotopeIdentification)
  suppressWarnings(mSet_OPT$PPS$CV <- suppressMessages(calcCV(xset)))
  mSet_OPT$PPS$RCS <-suppressMessages(calcRCS_GSValues(xset)$RCS)
  mSet_OPT$PPS$GS <-suppressMessages(calcRCS_GSValues(xset)$GS)
  mSet_OPT$PPS$GaussianSI <-calcGaussianS(mSet,object,useNoise=useNoise)
  
  ## Normalize the CV, RCS, GS, GaussianSI
  normalized.CV<-(mSet_OPT$PPS$CV-min(index.set$CV))/(max(index.set$CV)-min(index.set$CV))
  normalized.RCS<-(mSet_OPT$PPS$RCS-min(index.set$RCS))/(max(index.set$RCS)-min(index.set$RCS))
  normalized.GS<-(mSet_OPT$PPS$GS-min(index.set$GS))/(max(index.set$GS)-min(index.set$GS))
  normalized.GaussianSI<-mSet_OPT$PPS$GaussianSI
  
  ## Calculate the QS for the best combination in current iterator!
  QCoE<-0.2*(normalized.CV)+0.4*(normalized.GS+normalized.RCS)
  
  mSet_OPT$QS<-as.numeric(mSet_OPT$PPS[5])*QCoE*normalized.GaussianSI^2
  message("Model Parsing Done !")
  
  return(mSet_OPT)
}

#' @title Core Peak Picking Slave Cluster
#' @param task Numeric, task order for XCMS paramters table to run the peak picking and alignment.
#' @param xcmsSet_parameters Matrix, the parameters combination produced automatically according to 
#' the primary parameters input.
#' @param MSnExp object, the trimmed or the original data.
#' @param object_mslevel List, the parsed metabolomics scans produced by PeakPicking_prep.
#' @param isotopeIdentification Character, IsotopeIdentidication method, usually includes 'IPO' and 'CAMERA'.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @import MSnbase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

SlaveCluster_doe <-function(task, Set_parameters, object, object_mslevel, 
                            isotopeIdentification, BPPARAM = bpparam(),...) {
  
  mSet <-
    calculateSet_doe(
      object = object,
      object_mslevel=object_mslevel,
      Set_parameters = Set_parameters,
      task = task,
      BPPARAM = BPPARAM
    )
  print(paste("Finished", task,"/",length(Set_parameters),"in this round !"))

  if (!class(mSet)=="character"){
    print("Peak Feature Analyzing...")
    
    xset <- mSet[["xcmsSet"]]
    
    result <- calcPPS2(xset, isotopeIdentification)
    result[1] <- task   
    
    tmp_cv<- try(suppressMessages(calcCV(xset)),silent = T);
    if (class(tmp_cv)=="try-error"){
      result[6]<-0;
    } else {
      result[6]<-tmp_cv
    };
    
    tmp_RCS<-try(suppressMessages(calcRCS_GSValues(xset)$RCS),silent = T);
    if (class(tmp_RCS)=="try-error"){
      result[7]<-0;
    } else {
      result[7]<-tmp_RCS
    };
    
    tmp_GS<-try(suppressMessages(calcRCS_GSValues(xset)$GS),
                silent = T);
    
    if (class(tmp_GS)=="try-error"){
      result[8]<-0;
    } else {
      result[8]<-tmp_GS
    };
    
    tmp_GaussianSI<-try(calcGaussianS(mSet,object,
                                      useNoise = as.numeric(Set_parameters[[task]]$noise)),
                        silent = T);
    
    if (class(tmp_GaussianSI)=="try-error"){
      result[9]<-0;
    } else {
      result[9]<-tmp_GaussianSI
    };
    
    names(result)[c(6,7,8,9)]<-c("CV","RCS","GS","GaussianSI")
    
    rm(xset)
    
    #result
    print("Peak Feature Analyzing Done !")
    
  } else{
    result<-c(task,0,0,0,0,0,0,0,0)
  }
  return(result)
  
}

#' @title Cluster of Peak Picking and Alignment
#' @param object MSnExp object, the trimmed or the original data.
#' @param object_mslevel List, the parsed metabolomics scans produced by PeakPicking_prep.
#' @param Set_parameters Matrix, the parameters combination produced automatically according to 
#' the primary parameters input.
#' @param task Numeric, task order for XCMS paramters table to run the peak picking and alignment.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @import MSnbase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calculateSet_doe <- function(object, object_mslevel, Set_parameters, task = 1,
                             BPPARAM = bpparam()) {
  
  if (length(Set_parameters)==22 | length(Set_parameters)==23){
    param <- updateRawSpectraParam(Set_parameters)
  } else {
    param <- updateRawSpectraParam(Set_parameters[[task]])
  }

  mSet <- calculatePPKs(object, object_mslevel, param, BPPARAM = bpparam())
  #save(mSet,file = "mSet.rda");
  #save(param,file="param.rda")
  mSet <- calculateGPRT(mSet,param)
  
  return(mSet)
}

#' @title Peak picking Method
#' @param object MSnExp object, the trimmed or the original data.
#' @param object_mslevel List, the parsed metabolomics scans produced by PeakPicking_prep.
#' @param xcmsSetParameters Matrix, the parameters combination produced automatically according to 
#' @param task Numeric, task order for XCMS paramters table to run the peak picking and alignment.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @param msLevel Numeric, to specifiy the msLevel, only 1 permitted for now. 2 will be supported 
#' in the near future.
#' @import MSnbase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calculatePPKs<-function(object, object_mslevel,param,
                        BPPARAM = bpparam(),msLevel = 1){
  
  if (param$Peak_method == "centWave" | param$Peak_method == "matchedFilter") {
    # centWave & matchedFilter
    
    mSet <- try(PeakPicking_core(object, object_mslevel, 
                                param = param, 
                                BPPARAM = BPPARAM,
                                msLevel = 1),silent = T)

        } else {

      stop("Other peak picking method cannot be supported for now !")

  }
}

#' @title Alignment Method
#' @param mSet mSet object, the data produced by 'calculatePPKs' function.
#' @param Set_parameters Matrix, the parameters combination produced automatically according to 
#' @param task Numeric, task order for XCMS paramters table to run the peak picking and alignment.
#' @import MSnbase
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calculateGPRT<-function (mSet,param){
  
  mSet <- try(PerformPeakAlignment(mSet, param),silent = T);
  gc();
  mSet <- try(PerformPeakFiling (mSet, param),silent = T);
  gc();
  if (class(mSet)=="try-error"){
    mSet<-"Xset_NA";
  }
  return(mSet)
}

# Left the old here for fix the error later
calculateGPRT_old<-function(xdata,Set_parameters,task){
  ## Peak grouping with Density Method
  gdp <- PeakDensityParam(sampleGroups = rep(1, length(fileNames(xdata))), 
                          bw = Set_parameters$bw[task], 
                          minFraction = Set_parameters$minFraction[task], 
                          minSamples = Set_parameters$minSamples[task],
                          maxFeatures = Set_parameters$maxFeatures[task]);
  #if (xcmsSetParameters$RT_method[task] == "loess"){  
  grouped_xdata <- try(groupChromPeaks(xdata, param = gdp),silent = T)
  #} else {
  #  grouped_xdata <- xdata
  #}
  
  ## RT Alignment correction
  if (class(grouped_xdata)=="try-error"){
    xdata_filled<-"Xset_NA";
  } else{
    if(Set_parameters$RT_method[task] == "loess"){
      rt_xdata <- try(adjustRtime(grouped_xdata, 
                                  param = PeakGroupsParam(
                                    minFraction = Set_parameters$minFraction[task],
                                    extraPeaks=Set_parameters$extra[task],
                                    span=Set_parameters$span[task],
                                    smooth=Set_parameters$smooth[task],
                                    family =Set_parameters$family[task])),silent = T)
    }else{
      rt_xdata <- try(adjustRtime(grouped_xdata, 
                                  param = ObiwarpParam(binSize = Set_parameters$profStep[task])),silent = T)
    }
    if(!class(rt_xdata)=="try-error"){
      ## Grouped Again
      grouped_xdata2 <- groupChromPeaks(rt_xdata, param = gdp)
      ## Filled the missing peaking forcely
      xdata_filled <- fillChromPeaks(grouped_xdata2)
    } else{
      xdata_filled<-"Xset_NA";
    }
    
  }
  return(xdata_filled)
}

#' @title Calculate PPS method
#' @param xset xcmsSet Object, this object is produced by 'calculateSet_doe' function, and transformed 
#' with as(objec,'xcmsSet') function.
#' @param isotopeIdentification Character, IsotopeIdentidication method, usually includes 'IPO' and 'CAMERA'.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calcPPS2 <- function(xset, isotopeIdentification=c("IPO", "CAMERA"), ...) {
  
  isotopeIdentification <- match.arg(isotopeIdentification)
  
  ret <- vector(mode="numeric", 5) #array(0, dim=c(1,5)) 
  names(ret) <- c("ExpId", "#peaks", "#NonRP", "#RP", "PPS")
  if(is.null(xset)) {
    return(ret)
  } 
  
  if(nrow(peaks_IPO(xset)) == 0) {
    return(ret)
  }
  
  peak_source <- peaks_IPO(xset)[,c("mz", "rt", "sample", "into", "mzmin", 
                                    "mzmax", "rtmin", "rtmax"),drop=FALSE]
  ret[2] <- nrow(peak_source)
  
  if(isotopeIdentification == "IPO")
    iso_mat <- findIsotopes.IPO(xset, ...)  
  else
    iso_mat <- findIsotopes.CAMERA(xset, ...)
  
  samples <- unique(peak_source[,"sample"])
  isotope_abundance = 0.01108    
  
  #calculating low intensity peaks
  for(sample in samples) {
    non_isos_peaks <- peak_source
    
    if(nrow(iso_mat) > 0) {
      non_isos_peaks <- peak_source[-unique(c(iso_mat)),,drop=FALSE] 
    } 
    
    speaks <- non_isos_peaks[non_isos_peaks[,"sample"]==sample,,drop=FALSE]
    intensities <- speaks[,"into"]
    na_int <- is.na(intensities)
    intensities <- intensities[!na_int]
    
    if(length(intensities)>0) {
      tmp <- intensities[order(intensities)]
      int_cutoff <- mean(tmp[1:max(round((length(tmp)/33),0),1)])
      
      masses <- speaks[!na_int, "mz"]
      #floor((masses-2*CH3)/CH2) + 2
      maximum_carbon <- calcMaximumCarbon(masses)
      carbon_probabilty <- maximum_carbon*isotope_abundance
      
      iso_int <- intensities * carbon_probabilty
      
      not_loq_peaks <- sum(iso_int>int_cutoff)
      ret[3] <- ret[3] + not_loq_peaks
    }
  }#end_for_sample    
  
  ret[4] <- length(unique(c(iso_mat)))
  if(ret[3] == 0) {
    ret[5] <- (ret[4]+1)^1.5/(ret[3]+1)  
  } else {    
    ret[5] <- ret[4]^1.5/ret[3]  
  }
  
  return(ret)
  
}

#' @title Calculatre CV method
#' @param xset XCMSnExp Object, this object is produced by 'calculateSet_doe' function.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calcCV<-function(xset){
  
  ncount<-length(xset@phenoData[["sample_name"]])
  
  table.data<-creatPeakTable(xset);
  
  table.data$abundance.mean <- apply(table.data[, 9:(8 + ncount)],1, FUN = mean, na.rm = T);
  table.data$abundance.sd <- apply(table.data[, 9:(8 + ncount)],1, FUN = sd, na.rm = T);
  table.data$abundance.cv <- (table.data$abundance.sd * 100)/table.data$abundance.mean;
  
  cv.min<-min(table.data$abundance.cv, na.rm = T);
  cv.0.25<-as.numeric(quantile(table.data$abundance.cv, probs = 0.25, na.rm = T));
  cv.med<-median(table.data$abundance.cv, na.rm = T);
  cv.0.75<-as.numeric(quantile(table.data$abundance.cv, probs = 0.75, na.rm = T));
  cv.max<-max(table.data$abundance.cv, na.rm = T);
  cv.score<-1/(0.75*cv.med+0.25*(cv.max-cv.med))
  
  return(cv.score)
  
}

#' @title Calculatre RCS and GS method
#' @param xset XCMSnExp Object, this object is produced by 'calculateSet_doe' function.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calcRCS_GSValues<-function(xset){
  score.ret<-getRGTVValues(xset)
  return(list(GS=score.ret[["GS"]],RCS=score.ret[["RCS"]]))
}

#' @title Calculatre Gaussian Peak Ratio method
#' @param mSet MetaboAnalystR Object, this object is produced by 'calculateSet_doe' function.
#' @param object MSnExp object, the trimmed or the original data (Generated by ImportRawMSData function with "inMemory" mode).
#' @param useNoise Numeric, the noise level removed to evalute the gaussian peak.
#' @param BPPARAM MulticoreParam method, used to set the parallel method. Default is bpparam().
#' @export
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

calcGaussianS<-function(mSet, object, useNoise, BPPARAM = bpparam()){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
  
  if (identical(useNoise, numeric(0))){
    useNoise <- 0
  }
  
  peakmat <- mSet$msFeatureData$chromPeaks
  peakmat_set <- split.data.frame(peakmat, peakmat[, "sample"])
  # extract everything
  
  if(.Platform$OS.type=="unix" ){
    BPPARAM = MulticoreParam()
    
      extFUN <- function(z,object,useNoise) {
    ## Subset the data to the present file keeping only spectra that
    ## are in the peak range.
    
    if (nrow(z)>150){z<-z[sort(sample(1:nrow(z),150)),]}
    currentSample <- suppressMessages(MSnbase::filterRt(
      MSnbase::filterFile(object, z[1, "sample"]),
      rt = range(z[, c("rtmin", "rtmax")])))
    
    corr <- unlist(sapply(seq_len(nrow(z)),FUN = function(i){
      
      corr <- 0.1
      
      mzRange <- z[i, c("mzmin", "mzmax")] + c(-0.001, 0.001)
      rtRange <- z[i, c("rtmin", "rtmax")]
      
      suppressWarnings(ints <- MSnbase::intensity(
        MSnbase::filterMz(
          MSnbase::filterRt(currentSample, rtRange),mzRange)))
      
      ints[lengths(ints) == 0] <- 0
      ints <- as.integer(unlist(ints))
      ints <- ints[!is.na(ints)]
      ints <- ints[ints > useNoise]
      
      if (length(ints)) {
        
        ints <- ints - min(ints)
        if (max(ints) > 0)
          ints <- ints / max(ints)
        fit <- try(nls(y ~ SSgauss(x, mu, sigma, h),
                       data.frame(x = 1:length(ints), y = ints)),
                   silent = TRUE)
        if (class(fit) == "try-error") {
          corr <- 0.1        
        } else {
          ## calculate correlation of eics against gaussian fit
          if (sum(!is.na(ints - fitted(fit))) > 4 &&
              sum(!is.na(unique(ints))) > 4 &&
              sum(!is.na(unique(fitted(fit)))) > 4) {
            cor <- NULL
            options(show.error.messages = FALSE)
            cor <- try(cor.test(ints, fitted(fit),
                                method = "pearson",
                                use = "complete"))
            
            options(show.error.messages = TRUE)
            if (!is.null(cor) && cor$p.value <= 0.05){
              corr <- cor$estimate
            } else if (!is.null(cor) && cor$p.value > 0.05) {
              corr <- cor$estimate*0.85 #give a penalty (0.85) due to the unstatistical p value
            }
          } else {
            corr <- 0.1
          }
          
          
        }
      }
      return(corr)
    }))
    
    gaussian.peak.ratio<-nrow(z[corr >= 0.9, , drop = FALSE])/nrow(z)
    
    return(gaussian.peak.ratio)
  }
  
      res <- bplapply(peakmat_set,
                      extFUN,
                      object=object,
                      useNoise=useNoise,
                      BPPARAM = BPPARAM)
  
    
    }
  
  if(.Platform$OS.type=="windows"){
    
    
    # BPPARAM = MulticoreParam() # should be SnowParam(), not sure why does not work for windows
    
    # Several Other issues existed. So give up parallel in this step for windows
    extFUN <- function(z,object,useNoise) {
      ## Subset the data to the present file keeping only spectra that
      ## are in the peak range.
      
      if (nrow(z)>150){z<-z[sort(sample(1:nrow(z),150)),]}
      currentSample <- suppressMessages(MSnbase::filterRt(
        MSnbase::filterFile(object, z[1, "sample"]),
        rt = range(z[, c("rtmin", "rtmax")])))
      
      corr <- unlist(sapply(seq_len(nrow(z)),FUN = function(i){
        
        corr <- 0.1
        
        mzRange <- z[i, c("mzmin", "mzmax")] + c(-0.001, 0.001)
        rtRange <- z[i, c("rtmin", "rtmax")]
        
        suppressWarnings(ints <- MSnbase::intensity(
          MSnbase::filterMz(
            MSnbase::filterRt(currentSample, rtRange),mzRange)))
        
        ints[lengths(ints) == 0] <- 0
        ints <- as.integer(unlist(ints))
        ints <- ints[!is.na(ints)]
        ints <- ints[ints > useNoise]
        
        if (length(ints)) {
          
          ints <- ints - min(ints)
          if (max(ints) > 0)
            ints <- ints / max(ints)
          fit <- try(nls(y ~ SSgauss(x, mu, sigma, h),
                         data.frame(x = 1:length(ints), y = ints)),
                     silent = TRUE)
          if (class(fit) == "try-error") {
            corr <- 0.1        
          } else {
            ## calculate correlation of eics against gaussian fit
            if (sum(!is.na(ints - fitted(fit))) > 4 &&
                sum(!is.na(unique(ints))) > 4 &&
                sum(!is.na(unique(fitted(fit)))) > 4) {
              cor <- NULL
              options(show.error.messages = FALSE)
              cor <- try(cor.test(ints, fitted(fit),
                                  method = "pearson",
                                  use = "complete"))
              
              options(show.error.messages = TRUE)
              if (!is.null(cor) && cor$p.value <= 0.05){
                corr <- cor$estimate
              } else if (!is.null(cor) && cor$p.value > 0.05) {
                corr <- cor$estimate*0.85 #give a penalty (0.85) due to the unstatistical p value
              }
            } else {
              corr <- 0.1
            }
            
            
          }
        }
        return(corr)
      }))
      
      gaussian.peak.ratio<-nrow(z[corr >= 0.9, , drop = FALSE])/nrow(z)
      
      return(gaussian.peak.ratio)
    }
    
    res <- lapply(peakmat_set,
                    extFUN,
                    object=object,
                    useNoise=useNoise)
    
    res <- unlist(res)
    
    }
  return(mean(sapply(res, FUN = function(x){x})))
}

#' @title Identify whether results improved or not
#' @param history List, an interal media objects used to save the optimization results of peaks.
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

resultIncreased_doe <- function(history) {
  
  index = length(history)
  if(history[[index]]$PPS["PPS"] == 0 & index == 1)
    stop(paste("No isotopes have been detected, Please renew the trimed method!"))
  
  if(index < 2)
    return(TRUE)
  
  if(history[[index-1]]$QS >= history[[index]]$QS)
    return(FALSE)
  
  return(TRUE)
  
}

#' @title Noise_evaluation based on Kernal density model
#' @description This functions handles the evaluation on the data noise (noise and prefilter parameters) 
#' and the identification on the molecule weights deviation evaluation.
#' @param raw_data MSnExp object, the (trimmed) data in memory produced by 'PerformDataTrimming'.
#' @export
#' @import MSnbase
#' @import progress
#' @references McLean C (2020). Autotuner: Automated parameter selection for untargeted metabolomics data processing
#' @author Zhiqiang Pang \email{zhiqiang.pang@mail.mcgill.ca} Jeff Xia \email{jeff.xia@mcgill.ca}
#' Mcgill University
#' License: GNU GPL (>= 2)

Noise_evaluate <- function (raw_data){
  
  mSet<-list()
  mSet$time <- mSet$intensity <- list()
  
  mSet$time <- split(MSnbase::rtime(raw_data),fromFile(raw_data))
  mSet$intensity <- split(MSnbase::tic(raw_data),fromFile(raw_data))
  
  signals <- suppressMessages(lapply(mSet$intensity, 
                                     FUN = function(y) {
                                       
                                       # correct for possible NA values in data
                                       
                                       
                                       lag <- 25;threshold<- 3.1;influence <- 0.1
                                       
                                       
                                       signals <- rep(0,length(y))
                                       filteredY <- y[seq_len(lag)]
                                       avgFilter <- NULL
                                       stdFilter <- NULL
                                       avgFilter[lag] <- mean(y[seq_len(lag)])
                                       stdFilter[lag] <- sd(y[seq_len(lag)])
                                       
                                       for (i in (lag+1):length(y)){
                                         if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
                                           if (y[i] > avgFilter[i-1]) {
                                             signals[i] <- 1;
                                           } else {
                                             signals[i] <- -1;
                                           }
                                           filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
                                         } else {
                                           signals[i] <- 0
                                           filteredY[i] <- y[i]
                                         }
                                         avgFilter[i] <- mean(filteredY[(i-lag):i])
                                         stdFilter[i] <- sd(filteredY[(i-lag):i])
                                         
                                       }
                                       return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
                                     }))
  
  factorCol <- "Groups"
  Groups<-basename(raw_data@processingData@files)
  metadata<-data.frame(Sample.Name=basename(fileNames(raw_data)),Groups=Groups)
  sample_names <- paste(unlist(metadata[,factorCol]),
                        seq_len(nrow(metadata)))
  
  
  
  # Running Algorithm to extract peaks from each sample ---------------------
  peakList <- list()
  
  for(index in seq_along(sample_names)) { #making peak table for each sample
    
    ## identifying regions within TIC where peaks were identified
    
    peaks <- rle(signals[[index]]$signals)
    counter <- 1
    peakGroups <- list()
    for(rleIndex in seq_along(peaks$lengths)) {
      
      curValue <- peaks$values[rleIndex]
      if(curValue == 1) {
        
        peakGroups[[counter]] <- data.frame(index = (startPoint + 1),
                                            length =
                                              peaks$lengths[rleIndex])
        startPoint <- startPoint + peaks$lengths[rleIndex]
        counter <- counter + 1
        
      } else {
        
        if(rleIndex == 1) {
          startPoint <- peaks$lengths[rleIndex]
        } else {
          startPoint <- startPoint + peaks$lengths[rleIndex]
        }
        
        
      }
      
    }
    peakGroups <- Reduce(peakGroups, f = rbind)
    signals[[index]]$signals[peakGroups$index[peakGroups$length
                                              == 1] + 1] <- 1
    
    #peaks <- Autotuner@time[[index]][signals[[index]]$signals == 1]
    
    #peaks <- peaks[!is.na(peaks)]
    
    ## getting and extracting peaks
    #peaks
    findPeaks <- rle(signals[[index]]$signals == 1)
    counter <- 1
    peakGroups <- list()
    for(rleIndex in seq_along(findPeaks$lengths)) {
      
      curValue <- findPeaks$values[rleIndex]
      if(curValue) {
        
        start <- (startPoint + 1)
        startPoint <- startPoint + findPeaks$lengths[rleIndex]
        end <- startPoint
        peakGroups[[counter]] <- data.frame(start,
                                            end,
                                            length = end - start + 1)
        counter <- counter + 1
        
      } else {
        
        if(rleIndex == 1) {
          startPoint <- findPeaks$lengths[rleIndex]
        } else {
          startPoint <- startPoint + findPeaks$lengths[rleIndex]
        }
        
        
      }
      
    }
    
    peakGroups <- Reduce(rbind, peakGroups)
    
    
    npeaks <-10
    if(nrow(peakGroups) < 10) {
      npeaks <- nrow(peakGroups);
      warning("You are strongly advised to use wider rt.idx or more samples or other trimming strategy !")
    }
    peakGroups <- peakGroups[order(peakGroups$length, decreasing = TRUE),]
    
    peak_times <- list()
    for(j in seq_len(nrow(peakGroups))) {
      
      peak_times[[j]] <- mSet$time[[index]][
        peakGroups$start[j]:peakGroups$end[j]]
      
    }
    
    
    max_peak_length <- max(vapply(X = peak_times, FUN = length,
                                  FUN.VALUE = numeric(1)))
    peak_table <- data.frame(matrix(nrow = max_peak_length,
                                    ncol = npeaks+1))
    peak_table[,1] <- seq_len(max_peak_length)
    colnames(peak_table) <- c("peakLenth",
                              paste("peak", seq_len(npeaks)))
    
    for(column in seq(from = 2, to = ncol(peak_table))) {
      peak <- peak_times[[column -1]]
      peak_table[c(seq_along(peak)),column] <- peak
    }
    
    peak_table$peakLenth <- NULL
    peakList[[index]] <- peak_table
    
    
  }
  
  names(peakList) <- sample_names
  
  
  # initializing storage objects --------------------------------------------
  peak_table <- data.frame()
  counter <- 1
  # Extracting peak width properties from all data TIC peaaks
  for(sampleIndex in seq_along(peakList)) {
    
    # extracting relevant info for each sample
    time <- mSet$time[[sampleIndex]]
    intensity <- mSet$intensity[[sampleIndex]]
    peaks <- peakList[[sampleIndex]]
    
    # generating peak widths for each returned peak ------------------------
    # returns a data frame with estimated sample peakwidths
    peakIndexTable <- data.frame()
    for(peakColIndex in seq_len(ncol(peaks))) {
      
      tempPeakWidthEst <- peakwidth_est(peak_vector =
                                          peaks[,peakColIndex],
                                        time,
                                        intensity,
                                        start = NULL,
                                        end = NULL,
                                        old_r2 = NULL)
      
      
      #### ADD HARD CHECK HERE TO ENSURE PEAK DOESN'T GO ON FOREVER
      if(length(time)/5 < diff(tempPeakWidthEst)) {
        stop(paste("One peak was over 1/5 of all scans in length.",
                   "This is probably an error."))
      }
      
      ## 2019-06-20
      ## Adding check to handle corner case when peak goes beyond
      ## boundary of TIC
      if(tempPeakWidthEst[2] > length(time)) {
        tempPeakWidthEst[2] <- length(time)
      }
      
      peakIndexTable <- rbind(peakIndexTable, c(tempPeakWidthEst,
                                                peakColIndex))
      
    }
    colnames(peakIndexTable) <- c("peakStart", "peakEnd", "peakID")
    
    
    # Storing peakwidth info for each peak and each sample -----------------
    # inner loop - itterating through columns of peak index table
    for(curPeakIndexCol in seq_along(peakIndexTable$peakStart)) {
      
      ## start and end indexes
      start <- peakIndexTable$peakStart[curPeakIndexCol]
      end <- peakIndexTable$peakEnd[curPeakIndexCol]
      
      
      ## extracting peak width
      storeRow <- data.frame(
        peak_width = time[end] - time[start],
        Start_time = time[start],
        End_time = time[end],
        Start_name = names(time)[start],
        End_name = names(time)[end],
        Sample = sampleIndex,
        Mid_point_time = (time[start]+time[end])/2,
        Max_intensity = max(intensity[start:end]),
        ## consider changing with with which.is.max function
        Maxima_time = time[start + which(intensity[start:end] %in%
                                           max(intensity[start:end]))]
      )
      
      peak_table <- rbind(peak_table, storeRow)
      counter <- counter + 1
      
    }
    
  }
  
  # peak_table produced
  
  
  # EIC param-----------------------------------------------------------------
  pb <- progress_bar$new(format = "Evaluating [:bar] :percent Time left: :eta", 
                         total = length(unique(peak_table$Sample)), clear = T, width= 75)
  
  totalEstimates <- list()
  for(j in unique(peak_table$Sample)) {
    
    pb$tick();
    #msnObj <- suppressMessages(MSnbase::readMSData(files = currentFile,
    #                                               mode = "onDisk",
    #                                               msLevel. = 1))
    #header0 <- suppressWarnings( MSnbase::header(msnObj))
    
    
    currentTable <- peak_table[peak_table$Sample == j,]
    
    raw_data_current <- filterFile(raw_data,j)
    
    header_rt <- unname(mSet[["time"]][[j]])
    header <- MSnbase::header(raw_data_current)
    
    
    allMzs <- MSnbase::mz(raw_data_current)
    allInt <- MSnbase::intensity(raw_data_current)
    
    mzDb <- list()
    for(i in seq_along(allInt)) {
      mzDb[[i]] <- cbind(mz = allMzs[[i]],
                         intensity = allInt[[i]])
    }
    rm(allMzs, allInt)
    
    # going through each peak from a sample -----------------------------
    pickedParams <- list()
    for(curPeak in seq_len(nrow(currentTable))) {
      
      #message("--- Currently on peak: ", curPeak)
      start <- currentTable[curPeak,"Start_time"]
      end <- currentTable[curPeak,"End_time"]
      width <- currentTable$peak_width[curPeak]
      observedPeak <- list(start = start, end = end)
      
      ## currently here
      
      rate <- mean(diff(header_rt[header$ms.level == 1L]))
      
      scansOfPeak <- which(observedPeak$start < header_rt &
                             header_rt  < observedPeak$end)
      peakHead <- header[scansOfPeak,]
      ms1 <- peakHead$ms.level == 1L
      
      ## removing if statement here since everything is entered as MS1 spectra
      ## from the get go 2019-06-19
      #scanID <- as.numeric(sub("(.* )?scan=", "", peakHead$spectrumId[ms1]))
      
      rm(peakHead,ms1)
      
      peakMassSpectras <- mzDb[scansOfPeak]
      
      for(i in seq_along(scansOfPeak)) {
        peakMassSpectras[[i]] <- cbind(peakMassSpectras[[i]],
                                       scansOfPeak[i]#,
                                       #scanID[i]
        )
      }
      peakMassSpectras <- Reduce(rbind, peakMassSpectras)
      colnames(peakMassSpectras) <- c("mz", "intensity", "scan"#, "scanID"
      )
      
      peakMassSpectras <- data.frame(peakMassSpectras)
      
      peakMassSpectras <- peakMassSpectras[order(peakMassSpectras$mz),]
      peakMassSpectras <- peakMassSpectras[peakMassSpectras$intensity > 0,]
      
      sortedAllEIC <- peakMassSpectras
      
      
      matchedMasses <- rle(diff(sortedAllEIC$mz) < .005)
      
      ### THIS COULD BE PLACE TO ADD NOISE FILTER TO MAKE SPEED FASTER
      noiseAndPeaks <- filterPeaksfromNoise(matchedMasses)
      
      no_match <- noiseAndPeaks[[1]]
      truePeaks <- noiseAndPeaks[[2]]
      rm(noiseAndPeaks)
      
      approvedPeaks <- findTruePeaks(truePeaks, sortedAllEIC)
      
      overlappingScans <- sum(approvedPeaks$multipleInScan)
      
      
      # Filtering data by variability and ppm checks ----------------------------
      ppmEst <- filterPpmError(approvedPeaks, useGap=T, varExpThresh,
                               returnPpmPlots=F, plotDir, observedPeak)
      
      
      ppmObs <- approvedPeaks$meanPPM
      ppmObs <- strsplit(split = ";", x = as.character(ppmObs))
      ppmObs <- lapply(ppmObs, as.numeric)
      
      
      noisyBin <- unlist(lapply(ppmObs, function(ppm) {
        any(ppm > ppmEst)
      }))
      
      approvScorePeaks <- approvedPeaks[!noisyBin,]
      
      # Estimating PeakPicking Parameters ---------------------------------------
      SNest <- estimateSNThresh(no_match,
                                sortedAllEIC, approvScorePeaks)
      SNest <- min(SNest)
      
      if(is.infinite(SNest)) {
        SNest <- 25
      }
      
      scanEst <- min(approvScorePeaks$scanCount)
      
      ### Noise Intensity Estimate
      noiseEst <- min(approvScorePeaks$minIntensity) - 1000
      if(noiseEst < 0) {
        noiseEst <- 0
      }
      
      ### Prefilter Intensity Estimate
      intensityEst <- min(approvScorePeaks$Intensity)/sqrt(2)
      
      estimatedPeakParams <- data.frame(ppm = ppmEst,
                                        noiseThreshold = noiseEst,
                                        peakCount = nrow(approvedPeaks),
                                        prefilterI = intensityEst,
                                        prefilterScan = scanEst,
                                        TenPercentQuanSN = unname(SNest))
      
      
      if(is.null(estimatedPeakParams)) {
        next
      }
      
      pickedParams[[curPeak]] <- cbind(estimatedPeakParams,
                                       startTime = start,
                                       endTime = end,
                                       sampleID = j)
      
      
    }
    
    sampleParams <- Reduce(rbind, pickedParams)
    
    totalEstimates[[j]] <- sampleParams
    
  }
  
  eicParamEsts <- Reduce(rbind, totalEstimates)
  
  param <-list()
  
  param$ppm <- weighted.mean(eicParamEsts$ppm, eicParamEsts$peakCount)
  param$noise <- min(eicParamEsts$noiseThreshold, na.rm = TRUE)
  param$value_of_prefilter <- min(eicParamEsts$prefilterI, na.rm = TRUE)
  param$prefilter <- min(eicParamEsts$prefilterScan, na.rm = TRUE)
  #param$sn <- min(eicParamEsts$TenPercentQuanSN, na.rm = TRUE)
  
  param
  
  
}


##### ------------------=========   function kit from package IPO  ======----------------######

#' @references Gunnar Libiseller et al. 2015. IPO: a tool for automated optimization of XCMS parameters
#' @references https://github.com/glibiseller/IPO

getClusterType <- function() {
  if( .Platform$OS.type=="unix" ) {
    return("FORK")
  }
  return("PSOCK")
}
peaks_IPO <- function(xset) {
  
  peaks_act <-xset@peaks
  if (!("sample" %in% colnames(peaks_act))) {
    colnames(peaks_act)[colnames(peaks_act) == ""] <- "sample"
  }
  peaks_act
}
calcMaximumCarbon <- function(masses) {  
  
  carbon = 12.0
  hydrogen  = 1.0078250170
  CH3 = carbon + 3 * hydrogen
  CH2 = carbon + 2 * hydrogen  
  
  maximum_carbon <- floor((masses-2*CH3)/CH2) + 2
  
}    
getMaximumLevels <- function(model) {  
  # dimension of the modeled space
  dimensions <- length(model$coding)
  
  # define grid, to test for maximum
  if(dimensions > 6) {
    testSpace <- seq(-1,1,0.2) # 11 points
  } else { 
    testSpace <- seq(-1,1,0.1) # 21 points
  }
  
  testSize <- 10^6 # maximum number of grid points for one test
  # amount for testing each point in testSpace
  testAmount <- length(testSpace)^dimensions 
  i <- 1
  max <- rep(-1, dimensions+1) # start maximum response + setting
  # if there are more test points (=testAmount), than testSize,
  # then the tests are split and each subset is tested seperately
  while(i < testAmount) {
    testdata <- expand.grid.subset(i:(i+testSize), testSpace, dimensions)
    if(dimensions==1)
      names(testdata) <- names(model$coding)
    max_tmp <- getMaxSettings(testdata, model)
    if(max_tmp[1]>max[1]) # if better solution (i.e. test response)
      max <- max_tmp
    i <- i + testSize + 1
  }
  
  return(max)
  
}
getMaxSettings <- function(testdata, model) {
  
  response <- predict(model, testdata)
  max_response <- max(response)
  # select row(s) corresponding to max
  max_settings <- testdata[response==max_response,,drop=FALSE]
  ret <- max_response
  
  for(i in 1:ncol(testdata)) {
    levels <- max_settings[,i] # all settings of variable i
    if(all(c(-1,1) %in% levels)) # if both borders are in maximum settings
      ret <- cbind(ret, NA)
    else
      ret <- cbind(ret,levels[1]) # take first setting
  }
  
  colnames(ret) <- c("response", paste("x", 1:ncol(testdata), sep=""))
  return(ret)
}
expand.grid.subset  <- function(subset, sequence, dimensions) { 
  # generate a list, with sequence for each dimension
  vars <- list()
  for(i in 1:dimensions) {
    vars[[i]] <- sequence
  }
  names(vars) <- paste("x", 1:dimensions, sep="")
  
  # number of points in sequence grid
  maximumSubset <- length(sequence)^dimensions 
  # from min(subset)) to min(maximumSubset, max(subset)) OR
  # from maximumSubset to maximumSubset
  subset <- min(maximumSubset,min(subset)):min(maximumSubset, max(subset))
  
  #nv <-  #length(vars) 
  # number of values per variable = length(sequence)
  lims <- sapply(vars,length) 
  stopifnot(length(lims) > 0, # i.e. dimensions > 0
            subset <= prod(lims), # i.e. subset <= maximumSubset
            length(names(vars)) == dimensions) # i.e. dimensions = dimensions
  # empty list of length names(vars)
  res <- structure(vector("list",dimensions), .Names = names(vars))
  
  if (dimensions > 1) {
    for(i in dimensions:2) { # count down to 2: set up grid top down
      # %% = mod, %/% = integer division
      f <- prod(lims[1:(i-1)]) # number of grid points up to variable nr. (i-1)
      # repeat each element on grid 1:f
      res[[i]] <- vars[[i]][(subset - 1)%/%f + 1] 
      subset <- (subset - 1)%%f + 1 
    } 
  }
  res[[1]] <- vars[[1]][subset] 
  as.data.frame(res) 
} 
plotContours <- function(model, maximum_slice, plot_name = NULL) {
  # generate for all variable combinations formulas
  # (which will give the plots)
  plots <- c()
  for(i in 1:(length(maximum_slice)-1)) {
    for(j in (i+1):length(maximum_slice)) {
      plots <- c(plots, as.formula(paste("~ x", i, "* x", j, sep="")))
    } 
  }
  
  # determine number of plot rows and column on single device
  plot_rows <- round(sqrt(length(plots)))
  plot_cols <- 
    if(plot_rows==1){
      length(plots)
    } else {
      ceiling(sqrt(length(plots)))
    }
  
  # save as jpeg, if plot_name is given
  if(!is.null(plot_name)) {
    plot_name = paste(plot_name, ".jpg", sep="")
    jpeg(plot_name, width=4*plot_cols, height=2*plot_rows+2, 
         units="in", res=c(200,200))
  } # otherwise plot on device
  
  op <- par(mfrow = c(plot_rows, plot_cols), oma = c(0,0,0,0))  
  # contour.lm is called
  ret_tr <- try({#may produce errors, if figure margins are too small
    contour(model, plots, image = TRUE, at = maximum_slice)
  })
  if (class(ret_tr) == "try-error") {
    message("Error in plotting (see above), but IPO continues to work normally!")
  }
  
  if (!is.null(plot_name)) {
    dev.off()
  }
  par(op)
}
typeCastParams <- function(params) {
  ret_1 <- list()
  ret_2 <- list()  
  ret <- list()
  for(i in  1:length(params)) {
    factor <- params[[i]]
    if(length(factor) == 2) {
      ret_1[[(length(ret_1)+1)]] <- factor
      names(ret_1)[length(ret_1)] <- names(params)[i]
    } else {	
      ret_2[[(length(ret_2)+1)]] <- factor
      names(ret_2)[length(ret_2)] <- names(params)[i]
    }
  }	
  ret$to_optimize <- ret_1
  ret$no_optimization <- ret_2
  
  return(ret)
}
getCcdParameter <- function(params) {
  
  lower_bounds <- unlist(lapply(X=params, FUN=function(x) x[1]))
  higher_bounds <- unlist(lapply(X=params, FUN=function(x) x[2]))
  
  steps <- (higher_bounds - lower_bounds)/2
  
  # formula for each parameter, that transformes values from the range
  # to [0, 1]
  x <- paste("x", 1:length(params), " ~ (", c(names(params)), " - ", 
             (lower_bounds + steps), ")/", steps, sep="")
  
  # list with single formulas as entries
  formulae <- list()
  for(i in 1:length(x))
    formulae[[i]] <- as.formula(x[i])  
  
  design <- rsm::ccd(length(params), # number of variables
                     n0 = 1, # number of center points
                     alpha = "face", # position of the ‘star’ points
                     randomize = FALSE, 
                     inscribed = TRUE, # TRUE: axis points are at +/- 1 and the
                     # cube points are at interior positions
                     coding = formulae) # List of coding formulas for the design
  # variables
  return(design)
  
}
combineParams <- function(params_1, params_2) {
  len <- max(unlist(sapply(params_1, length)))
  #num_params <- length(params_1)
  
  p_names <- c(names(params_1), names(params_2))
  matchedFilter <- "fwhm" %in% p_names
  
  for(i in 1:length(params_2)) {
    new_index <- length(params_1) + 1
    fact <- params_2[[i]]
    params_1[[new_index]] <- fact
    if(matchedFilter) {
      if(p_names[new_index] == "sigma" && fact == 0) {
        # update values for sigma if zero
        if("fwhm" %in% names(params_1)) {
          params_1[[new_index]][1:len] <- params_1$fwhm/2.3548
        } else {
          params_1[[new_index]][1:len] <- params_2$fwhm/2.3548
        }
      } else if(p_names[new_index] == "mzdiff" && fact == 0) {
        # update values for mzdiff if zero
        if("step" %in% names(params_1)) {
          if("steps"  %in% names(params_1)) {
            params_1[[new_index]][1:len] <- 0.8-params_1$step*params_1$steps
          } else {
            params_1[[new_index]][1:len] <- 0.8-params_1$step*params_2$steps
          }	
        } else {
          if("steps"  %in% names(params_1)) {
            params_1[[new_index]][1:len] <- 0.8-params_2$step*params_1$steps
          } else {
            params_1[[new_index]][1:len] <- 0.8-params_2$step*params_2$steps
          }	
        }
      } else {  
        # standard: replicate value
        params_1[[new_index]][1:len] <- fact
      }
    } else {
      # standard: replicate value
      params_1[[new_index]][1:len] <- fact
    }
  } 
  names(params_1) <- p_names   
  return(params_1)
  
}
getRGTVValues <- function(xset, exp_index=1, retcor_penalty=1) {
  
  relative_rt_diff <- c()
  
  if(nrow(xset@groups) > 0) {
    for(i in 1:nrow(xset@groups)) {
      feature_rtmed <- xset@groups[i, "rtmed"]
      relative_rt_diff <- 
        c(relative_rt_diff, 
          mean(abs(feature_rtmed - 
                     peaks_IPO(xset)[xset@groupidx[[i]], "rt"]) / feature_rtmed))
    }
    good_groups <- 
      sum(unlist(lapply(X=xset@groupidx, FUN = function(x, xset) {
        ifelse(length(unique(peaks_IPO(xset)[x,"sample"])) == 
                 length(xset@filepaths) & 
                 length(peaks_IPO(xset)[x,"sample"]) == 
                 length(xset@filepaths), 1, 0)
      }, xset)))
    bad_groups <- nrow(xset@groups) - good_groups
  } else {
    relative_rt_diff <- 1
    good_groups <- 0
    bad_groups <- 0   
  }
  
  tmp_good_groups <- good_groups + ifelse(bad_groups==0, 1, 0)
  tmp_bad_groups <- bad_groups + ifelse(bad_groups==0, 1, 0)
  
  ARTS <- (mean(relative_rt_diff)) * retcor_penalty
  
  ret <- list(exp_index   = exp_index, 
              good_groups = good_groups, 
              bad_groups  = bad_groups, 
              GS          = tmp_good_groups^2/tmp_bad_groups, 
              RCS         = 1/ARTS)
  
  ret$retcor_done = retcor_penalty        
  
  return(ret)  
}
findIsotopes.IPO <- function(xset, checkPeakShape=c("none", "borderIntensity", "sinusCurve", "normalDistr")) {
    
    checkPeakShape <- match.arg(checkPeakShape)
    
    iso_mat <- matrix(0, nrow=0, ncol=2)
    if(is.null(xset)) {
      return(iso_mat)
    }
    
    colnames(iso_mat) <- c("12C", "13C")
    peak_source <- peaks_IPO(xset)[,c("mz", "rt", "sample", "into", "maxo", "mzmin",
                                      "mzmax", "rtmin", "rtmax"), drop=FALSE]
    
    for(i in 1:ncol(peak_source)) {
      peak_source <- peak_source[!is.na(peak_source[,i]),,drop=FALSE]
    }
    
    peak_source <- cbind(1:nrow(peak_source), peak_source)
    colnames(peak_source)[1] <- "id"  
    
    #carbon = 12.0
    #hydrogen	= 1.0078250170
    #CH3 = carbon + 3 * hydrogen
    #CH2 = carbon + 2 * hydrogen
    isotope_mass = 1.0033548
    isotope_abundance = 0.01108
    
    samples <- max(peak_source[,"sample"])
    
    #start_sample
    for(sample in 1:samples) { 
      #only looking into peaks from current sample   
      speaks <- peak_source[peak_source[,"sample"]==sample,,drop=FALSE]
      split <- 250
      if(!(checkPeakShape=="none"))
        # rawdata <- loadRaw(xcmsSource(xset@filepaths[sample]))
        stop("Other PeakShapeChecking Method will be supported later !")
      
      if(nrow(speaks)>1) {  		      
        #speaks <- speaks[,-c("sample")]
        speaks <- speaks[order(speaks[,"mz"]),]
        
        while(!is.null(nrow(speaks)) & length(speaks) > 3) {
          part_peaks <- NULL
          #splitting the data into smaller pieces to improve speed    
          if(nrow(speaks) < split) {
            part_peaks <- speaks
          } else {          
            upper_bound <- speaks[split,"mzmax"] + isotope_mass          
            end_point <- sum(speaks[,"mz"] < upper_bound)
            part_peaks <- speaks[1:end_point,,drop=FALSE]
          }		
          
          rt <- part_peaks[,"rt"]
          rt_window <- rt * 0.005
          rt_lower <- part_peaks[,"rt"] - rt_window
          rt_upper <- part_peaks[,"rt"] + rt_window
          rt_matrix <-  
            t(matrix(rep(rt, nrow(part_peaks)), ncol=nrow(part_peaks)))
          rt_matrix_bool <- rt_matrix >= rt_lower & rt_matrix <= rt_upper
          
          mz <- part_peaks[,"mz"]
          #isotope_masses - mz_window
          mz_lower <- part_peaks[,"mzmin"] + isotope_mass
          #isotope_masses + mz_window
          mz_upper <- part_peaks[,"mzmax"] + isotope_mass
          mz_matrix <-  
            t(matrix(rep(mz, nrow(part_peaks)), ncol=nrow(part_peaks)))
          mz_matrix_bool <- mz_matrix >= mz_lower & mz_matrix <= mz_upper
          
          rt_mz_matrix_bool <- rt_matrix_bool & mz_matrix_bool
          
          rt_mz_peak_ids <- which(rowSums(rt_mz_matrix_bool)>0)
          calculations <- min(split, nrow(speaks))
          rt_mz_peak_ids <- rt_mz_peak_ids[rt_mz_peak_ids < calculations]
          
          for(i in rt_mz_peak_ids) {
            current <- part_peaks[i, ,drop=FALSE]
            rt_mz_peaks <- part_peaks[rt_mz_matrix_bool[i,],,drop=FALSE]
            rt_difference <- 
              abs(current[,"rt"] - rt_mz_peaks[, "rt"]) / current[,"rt"]
            rt_mz_peaks <- cbind(rt_mz_peaks, rt_difference)
            #test intensity_window
            #floor((current["mz"]-2*CH3)/CH2) + 2
            maximum_carbon <- calcMaximumCarbon(current[,"mz"]) 
            carbon_probabilty <- c(1,maximum_carbon)*isotope_abundance
            iso_intensity <- current[,"into"] * carbon_probabilty
            
            int_bools <- 
              rt_mz_peaks[,"into"] >= iso_intensity[1] & 
              rt_mz_peaks[,"into"] <= iso_intensity[2]
            
            if(sum(int_bools) > 0) {
              int_peaks <- rt_mz_peaks[int_bools,,drop=FALSE]
              boundary_bool <- rep(TRUE, (nrow(int_peaks)+1))
              if(!(checkPeakShape=="none")) {
                if(checkPeakShape=="borderIntensity") {
                  boundary_bool <- checkIntensitiesAtRtBoundaries(
                    rawdata, 
                    rbind(current,int_peaks[,-ncol(int_peaks), drop=FALSE]))
                } else {
                  if(checkPeakShape=="sinusCurve") {                
                    boundary_bool <- checkSinusDistribution(
                      rawdata, 
                      rbind(current,int_peaks[,-ncol(int_peaks),drop=FALSE]))
                  } else {                  
                    boundary_bool <- checkNormalDistribution(
                      rawdata, 
                      rbind(current,int_peaks[,-ncol(int_peaks),drop=FALSE]))
                  }
                }
              } #else {
              #boundary_bool <- rep(TRUE, (nrow(int_peaks)+1))
              #}              
              if(boundary_bool[1] & sum(boundary_bool[-1])>0) {                 
                iso_peaks <- int_peaks[boundary_bool[-1],,drop=FALSE]
                iso_id <- 
                  iso_peaks[which.min(iso_peaks[,"rt_difference"]), "id"]
                #iso_list[[length(iso_list)+1]] <- c(current[,"id"], iso_id)            
                iso_mat <- rbind(iso_mat, c(current[,"id"], iso_id))                
              }
            }
          }
          speaks <- speaks[-(1:calculations),]		    
          
        }#end_while_sample_peaks 
      }
    }
    return(iso_mat)
  }
checkIntensitiesAtRtBoundaries <-  function(rawdata, peaks, minBoundaryToMaxo=1/3,ppmstep=15) {
    ret <- rep(TRUE, nrow(peaks))
    for(i in 1:nrow(peaks)) {
      peak <- peaks[i,]
      for(boundary in c("rtmin", "rtmax")) {
        rtIndex <- which(rawdata$rt==peak[boundary])
        if(length(rtIndex)>0) {
          if(rtIndex==length(rawdata$scanindex)) {
            rtIndices <- c(rawdata$scanindex[rtIndex], length(rawdata$mz))
          } else {
            rtIndices <- rawdata$scanindex[c(rtIndex, rtIndex+1)]
          }
          
          #only relevant mz and intensity values regarding retention time
          mz <- rawdata$mz[(rtIndices[1]+1):rtIndices[2]]	
          intensities <- rawdata$intensity[(rtIndices[1]+1):rtIndices[2]]
          
          ppm <- peak[c("mzmin", "mzmax")]*ppmstep/1000000
          mzIntensities <- 
            c(0, intensities[mz>=peak["mzmin"]-ppm[1] & mz<=peak["mzmax"]+ppm[2]])
          maxBoundaryIntensity <- max(mzIntensities)
          ret[i] <- ret[i] & maxBoundaryIntensity<peak["maxo"]*minBoundaryToMaxo
        }
      }
    }
    
    return(ret)
    
  }
checkSinusDistribution <- function(rawdata, peaks) {
  ret <- rep(TRUE, nrow(peaks))
  for(i in 1:nrow(peaks)) {
    ret[i] <- testSinusDistribution(rawdata, peaks[i,,drop=FALSE])
  }
  
  return(ret)
}
checkNormalDistribution <- function(rawdata, peaks) {
  ret <- rep(TRUE, nrow(peaks))
  for(i in 1:nrow(peaks)) {
    ret[i] <- testNormalDistribution(rawdata, peaks[i,,drop=FALSE])
  }
  
  return(ret)
}
getIntensitiesFromRawdata <- function(rawdata, peak) {
  rt <- rawdata$rt >= peak[,"rtmin"] & rawdata$rt <= peak[,"rtmax"]
  
  rtRange <- c(min(which(rt)), max(which(rt))+1)  
  scanIndices <- 
    rawdata$scanindex[rtRange[1]:min(rtRange[2], length(rawdata$scanindex))]
  #  scanIndices <- scanIndices[!is.na(scanIndices)]
  if(rtRange[2]>length(rawdata$scanindex)) {
    scanIndices <- c(scanIndices, length(rawdata$intensity))
  }
  
  if(length(scanIndices) < 3)
    return(FALSE)  
  
  y <- c()
  for(i in 1:(length(scanIndices)-1)) {
    scanRange <- c(scanIndices[i]+1, scanIndices[i+1])
    mz <- rawdata$mz[scanRange[1]:scanRange[2]]
    y <- 
      c(y, 
        max(0, (rawdata$intensity[scanRange[1]:scanRange[2]][
          mz >= peak[,"mzmin"] & mz <= peak[,"mzmax"]])
        )
      )
  }
  
  y
}
testNormalDistribution <- function(rawdata, peak) {
  
  y <- getIntensitiesFromRawdata(rawdata, peak)
  if(length(y) < 3) {
    return(FALSE)
  }
  
  if(max(y)==0) {
    return(FALSE)
  }
  
  normY <- (y-min(y))/(max(y)-min(y))
  
  mean=10; 
  sd=3;
  
  seqModel <- seq(-4,4,length=length(normY))*sd + mean
  yModel <- dnorm(seqModel,mean,sd)
  yModel = yModel* (1/max(yModel))
  correlation <- cor(yModel, normY)
  
  correlation > 0.7
  
  
}
testSinusDistribution <- function(rawdata, peak) {
  
  y <- getIntensitiesFromRawdata(rawdata, peak)
  if(length(y) < 3) {
    return(FALSE)
  }
  if(max(y)==0) {
    return(FALSE)
  }
  
  normY <- (y-min(y))/(max(y)-min(y))
  sinCurve <- (sin(seq(-pi/2,pi+1.5,length=length(normY))) + 1) / 2
  correlation <- cor(sinCurve, normY)
  
  correlation > 0.7
  
}
findIsotopes.CAMERA <-  function(xset, ...) {
    
    iso_mat <- matrix(0, nrow=0, ncol=2)
    if(is.null(xset)) {
      return(iso_mat)
    }
    
    ids <- peaks_IPO(xset)[,"sample", drop=FALSE]
    ids <- cbind(1:length(ids), ids)
    
    xsets <- split(xset, unique(peaks_IPO(xset)[,"sample"]))
    samples <- unique(peaks_IPO(xset)[,"sample"])
    for(sample in samples) {
      an <- xsAnnotate(xset, sample=sample)
      isos <- findIsotopes(an, ...)@isoID[,c("mpeak", "isopeak"), drop=FALSE]
      #start_id <- ids[ids[,2]==sample,,drop=FALSE][1,1] - 1
      iso_mat <- rbind(iso_mat, matrix(ids[ids[,2]==sample,1][isos], ncol=2))
    }
    
    iso_mat
  }
createModel <- function(design, params, resp) {
  # add response to the design, which gives the data for the model
  design$resp <- resp
  if(length(params) > 1) {
    # create full second order (SO) model
    # use xi in model, instead of parameter names
    formula <- 
      as.formula(paste("resp ~ SO(", 
                       paste("x", 1:length(params), 
                             sep="", collapse=","),
                       ")", sep=""))
    model <- rsm:::rsm(formula, data=design) 
  } else {
    # create full second order model with one parameter
    # here: use parameter name in model
    param_name <- names(params)[1]
    formula <- as.formula(paste("resp ~ ", param_name, " + ", 
                                param_name, " ^ 2", sep="")) 
    model <- lm(formula, data=design) 
    model$coding <- list(x1=as.formula(paste(param_name, "~ x1"))) 
    names(model$coding) <- param_name
    #attr(model, "class") <- c("rsm", "lm")
  }
  return(model)  
}
decode <- function(value, bounds) {
  if(is.na(value))
    value <- 1
  x <- (value+1)/2 # from [-1,1] to [0, 1]
  x <- (x*(max(bounds)-min(bounds))) + min(bounds)
  
  return(x)
}
decodeAll <- function(values, params) {
  
  ret <- rep(0, length(params))
  for(i in 1:length(params))
    ret[i] <- decode(values[i], params[[i]])
  
  names(ret) <- names(params)
  
  return(ret)
}
encode <- function(value, bounds) {
  x <- (value - min(bounds)) / (max(bounds) - min(bounds))
  
  return(x*2-1)
}
attachList <- function(params_1, params_2) {
  params <- params_1
  for(factor in params_2)
    params[[length(params)+1]] <- factor
  
  names(params) <- c(names(params_1), names(params_2))
  return(params)
}
checkParams <- function(params, quantitative_parameters,qualitative_parameters, unsupported_parameters) { 
  if(length(typeCastParams(params)$to_optimize)==0) {
    stop("No parameters for optimization specified; stopping!")  
  }
  
  for(i in 1:length(params)) {
    param <- params[[i]]
    name <- names(params)[i]
    if(name %in% unsupported_parameters) {
      stop(paste("The parameter", name, "is not supported! Please remove
	               from parameters; stopping!"))
    }
    if(name %in% qualitative_parameters) {
      if(length(param) == 0) {
        stop(paste("The parameter", name, "has no value set!
	                 Please specify; stopping!"))
      }
      if(length(param) > 1) {
        stop(paste("Optimization of parameter", name, "not supported!
	                 Please specify only one value; stopping!"))
      }
    }
    if(name %in% quantitative_parameters) {
      if(length(param) == 0) {
        stop(paste("The parameter", name, "has no value set!
                   Please specify between one and two; stopping!"))
      } 
      if(length(param) > 2) {
        stop(paste("Too many values for parameter", name, "!
                   Please specify only one or two; stopping!"))
      }
      if(!all(diff(param) > 0)) {
        stop(paste("Parameter", name, "has wrong order!",
                   "Please specify in increasing order; stopping!"))
      }
    }
  }
  missing_params <- 
    which(!(c(quantitative_parameters, qualitative_parameters) %in% 
              names(params)))
  if(length(missing_params > 0)) {
    stop(paste("The parameter(s)", 
               paste(c(quantitative_parameters,
                       qualitative_parameters)[missing_params], 
                     collapse=", "), 
               "is/are missing! Please specify; stopping!"))
  }
  
}


#
##### -----------------==========    Bottom of this function kit   ======----------------######


###########--------------- ========= Function Kit from AutoTuner =========-----------------  
#' @references McLean C (2020). Autotuner: Automated parameter selection for untargeted metabolomics data processing
#' @references https://github.com/crmclean/Autotuner/


filterPeaksfromNoise <- function(matchedMasses) {
  
  ## Initializing variables for subsetting
  if(length(matchedMasses$values) %% 2 == 0) {
    list_length <- length(matchedMasses$values)/2
  } else {
    list_length <- as.integer(length(matchedMasses$values)/2) + 1
  }
  truePeaks <- vector("list", list_length)
  truePeakIndex <- 1
  lengthCounter <- 1
  lastTrue <- matchedMasses$values[1]
  
  ## subsets things that have a second mass w/in user error
  for(rleIndex in seq_along(matchedMasses$values)) {
    
    start <- lengthCounter
    
    if(lastTrue == TRUE) {
      start <- start + 1
    }
    
    end <- lengthCounter + matchedMasses$lengths[rleIndex]  - 1
    
    if(isTRUE(matchedMasses$values[rleIndex])) {
      end <- end + 1
      lastTrue = TRUE
      truePeaks[[truePeakIndex]] <- start:end
      truePeakIndex <- truePeakIndex + 1
    } else {
      lastTrue = FALSE
      if(!exists("no_match")) {
        no_match <- start:end
      } else {
        no_match <- c(no_match, start:end)
      }
      
    }
    lengthCounter <- lengthCounter + matchedMasses$lengths[rleIndex]
  }
  
  if(is.null(truePeaks[[list_length]])) {
    truePeaks <- truePeaks[seq_len(list_length - 1)]
  }
  
  rm(list_length,truePeakIndex,lastTrue,lengthCounter,start,end,rleIndex)
  return(list(no_match, truePeaks))
}
findTruePeaks <- function(truePeaks, sortedAllEIC) {
  #
  
  # initializing storage ----------------------------------------------------
  ppmData <- list()
  counter <- 1
  
  # Checking if bin elements come from adj scans ----------------------------
  for(i in seq_along(truePeaks)) {
    
    pickedPeak <- truePeaks[[i]]
    
    peakData <- sortedAllEIC[pickedPeak,]
    peakData <- peakData[order(peakData$scan),]
    
    # remove features that are duplicated.  -------------------------------
    if(nrow(peakData) == 1) {
      next()
    }
    
    ## checking to make sure features comes from adjacent scans
    scanDiff <- diff(sort(unique(peakData$scan)))
    
    ## checking that binned peaks:
    # are being picked up within consecutive scans
    peakDists <- (length(unique(scanDiff)) == 1 && unique(scanDiff) == 1)
    if(!peakDists) {
      next()
    }
    
    ## checking to see if any binned masses come from the same scan
    countsInScans <- table(peakData$scan)
    moreInAScan <- any(as.vector(countsInScans) > 1)
    
    if(moreInAScan) {
      
      for(w in seq_len(length(unique(peakData$scan)) - 1)) {
        
        curScan <- unique(peakData$scan)[w]
        nextScan <- unique(peakData$scan)[w+1]
        
        ### add condition here for whenever it is the end of the scan
        
        scanStates <- peakData[peakData$scan == curScan,]
        nextStates <- peakData[peakData$scan == nextScan,]
        curObsRows <- peakData$scan == curScan | peakData$scan ==
          nextScan
        
        peakData <- peakData[!curObsRows,]
        
        ## do this if there are two states in first scan
        
        if(w == 1) {
          
          errorNext <- lapply(scanStates$mz, function(x) {
            obsError <- abs(x - nextStates$mz)/x * 10^6
          })
          
          checkMins <- vapply(X = errorNext, FUN = which.min,
                              FUN.VALUE = numeric(1))
          
          initialState <- which.min(vapply(X = seq_along(errorNext),
                                 FUN = function(x) {
                                   errorNext[[x]][checkMins[x]]
                                 }, FUN.VALUE = numeric(1)))
          
          scanStates <- scanStates[initialState,]
        }
        
        nextStateIndex <- which.max(vapply(X = scanStates$mz, FUN = function(x) {
          
          obsError <- abs(x - nextStates$mz)/x * 10^6
          
          
          if(any(obsError == 0)) {
            
            ## corner case - error is 0 and there are no other
            ## options
            if(length(x) == 1) {
              obsError <- 0.001
            } else {
              obsError[obsError == 0] <-
                min(obsError[obsError != 0])/10
            }
            
          }
          
          intensityProb <- nextStates$intensity/
            sum(nextStates$intensity)
          errorInverse <- 1/obsError
          nextStateProb <- errorInverse/sum(errorInverse) *
            intensityProb
          return(max(nextStateProb/sum(nextStateProb)))
          
        }, FUN.VALUE = numeric(1)))
          
        nextStates <- nextStates[nextStateIndex,]
        
        ## store new states
        bestStates <- rbind(scanStates,nextStates)
        peakData <- rbind(bestStates, peakData)
        peakData <- peakData[order(peakData$scan),]
        
      }
      
      multipleInScan <- TRUE
      
    } else {
      
      multipleInScan <- FALSE
    }
    
    obsPPM <- vapply(X = 2:length(peakData$mz), FUN = function(mz) {
      estimatePPM(peakData$mz[(mz - 1)], peakData$mz[mz])
    }, FUN.VALUE = numeric(1))
    
    # storing output -------------------------------------------------------
    ppmData[[counter]] <- data.frame(meanMZ = mean(peakData$mz),
                                     startScan = min(peakData$scan),
                                     endScan = max(peakData$scan),
                                     scanCount = length(peakData$scan),
                                     Intensity = sum(peakData$intensity),
                                     meanIntensity = mean(
                                       peakData$intensity),
                                     intensityDispersion = sd(
                                       peakData$intensity),
                                     minIntensity = min(peakData$intensity),
                                     meanPPM = paste(signif(obsPPM),
                                                     collapse = ";"),
                                     multipleInScan,
                                     stringsAsFactors = FALSE,
                                     index = i)
    
    counter <- 1 + counter
    
  }
  approvedPeaks <- Reduce(rbind, ppmData)
  return(approvedPeaks)
}
estimatePPM <- function(first, second) {
  abs(first-second)/first * 10 ^ 6
}
estimateSNThresh <- function(no_match, sortedAllEIC, approvedPeaks) {
  
  noisePeakTable <- sortedAllEIC[no_match,]
  noise_noise <- noisePeakTable$intensity
  scanCount <- sortedAllEIC$scan
  maxScan <- max(scanCount, na.rm = TRUE)
  minScan <- min(scanCount, na.rm = TRUE)
  scanCount <- scanCount[no_match]
  
  ## generating index for subseting of fixed noise obj
  scanIntervals <- list()
  for(peakID in seq_len(nrow(approvedPeaks))) {
    
    peakStart <- approvedPeaks[peakID,"startScan"]
    peakEnd <- approvedPeaks[peakID,"endScan"]
    peakDist <- peakEnd - peakStart
    lowerBound <- peakStart - peakDist * 2
    
    if(lowerBound < minScan) {
      lowerBound <- minScan
    }
    
    upperBound <- peakEnd + peakDist * 2
    if(upperBound > maxScan) {
      upperBound <- maxScan
    }
    
    scanIntervals[[peakID]] <- which(minScan:maxScan %in% c(lowerBound,
                                                            upperBound))
    
    
  }
  
  ## calculating all fixed noise values
  scanRange <- (minScan:maxScan)
  fixedNoiseList <- list()
  counter <- 1
  for(scanID in seq_along(scanRange)) {
    
    peakNoise <- noise_noise[scanCount == scanRange[scanID]]
    
    if(length(peakNoise) == 0) {
      next
    }
    
    fixedNoise <- peakNoise[!(peakNoise %in% boxplot.stats(peakNoise)$out)]
    
    fixedNoiseMean <- mean(x = fixedNoise, na.rm = TRUE)
    fixedNoiseVar <-  stats::var(x = fixedNoise, na.rm = TRUE)
    
    ## 2019-07-08: fixed corner case where only one noise element was
    ## found within the bin
    if(is.na(fixedNoiseVar)) {
      fixedNoiseVar <- 0
    }
    
    N <- length(fixedNoise)
    
    fixedNoiseList[[counter]] <- data.frame(fixedNoiseMean,fixedNoiseVar,N)
    counter <- 1 + counter
  }
  fixedNoiseList <- Reduce(rbind, fixedNoiseList)
  
  ## calculating the sd and mean for each group
  noiseIntDb <- list()
  counter <- 1
  for(row in seq_along(scanIntervals)) {
    
    if(row == 1) {
      
      new <- TRUE
      
    } else {
      
      new <- vapply(X = noiseIntDb, FUN = function(noiseDb) {
        if(!all(scanIntervals[[row]] == as.numeric(noiseDb[,c(1,2)]))) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }, FUN.VALUE = logical(1))
      new <- all(new)
    }
    
    
    if(new) {
      ## add check to see if cur row has already been looked at
      curRow <- scanIntervals[[row]]
      curStatDb <- fixedNoiseList[curRow[1]:curRow[2],]
      
      ## added this to check for case when row is missing
      ## if a match to the noise peak was not identified
      if(any(is.na(curStatDb$fixedNoiseMean))) {
        curStatDb <- curStatDb[!is.na(curStatDb$fixedNoiseMean),]
      }
      
      eX2 <- sum((curStatDb$fixedNoiseMean^2 +
                    curStatDb$fixedNoiseVar)*curStatDb$N)
      eX2 <- eX2/sum(curStatDb$N)
      groupMean <- mean(curStatDb$fixedNoiseMean)
      groupVar <- eX2 - groupMean^2
      
      if(groupVar < 0) {
        groupVar <- groupVar * -1
      }
      
      groupSd <- suppressWarnings(sqrt(groupVar))
      noiseIntDb[[counter]] <- data.frame(start = curRow[1],
                                          end = curRow[2], groupMean,
                                          groupSd)
      counter <- counter + 1
    }
    
  }
  
  noiseIntDb <- Reduce(rbind, noiseIntDb)
  noiseIntDb$key <- apply(noiseIntDb[,c(1,2)], 1, paste, collapse = " ")
  rm(curStatDb, eX2, groupSd, groupVar, groupMean,
     curRow, fixedNoiseList)
  
  
  SN <- list()
  counter <- 1
  for(peakID in seq_along(scanIntervals)) {
    
    scanInt <- paste(scanIntervals[[peakID]], collapse = " ")
    scanStats <- noiseIntDb[noiseIntDb$key == scanInt,]
    
    if(nrow(scanStats) == 0) {
      next()
    }
    
    ## 2019-06-20 - added here to fix bug if noise calc is wrong
    if(is.nan(scanStats$groupSd) | is.na(scanStats$groupSd)) {
      next()
    }
    
    if(is.nan(scanStats$groupMean)) {
      next()
    }
    
    Peak <- approvedPeaks$Intensity[peakID]
    
    if((Peak - scanStats$groupMean) > 3*scanStats$groupSd) {
      
      ## selects as true peak
      SigNoiseThresh <- (Peak - scanStats$groupMean)/scanStats$groupSd
      SN[[counter]] <- SigNoiseThresh
      counter <- counter + 1
      
    } else {
      
      next()
      
    }
  }
  
  if(!exists("SN")) {
    return(NA)
  }
  
  return(unlist(SN))
}
filterPpmError <- function(approvedPeaks, useGap, varExpThresh,
                           returnPpmPlots, plotDir, observedPeak,
                           filename) {
  
  ppmObs <- approvedPeaks$meanPPM
  ppmObs <- unlist(lapply(strsplit(split = ";", x = as.character(ppmObs)),function(x) {as.numeric(x)}))
  
  ## 2019-06-19
  ## corner case when all error measurements are identical.
  if(diff(range(ppmObs)) < .Machine$double.eps ^ 0.5) {
    stop(paste("All calculated ppm values are identical.",
               "Error of data may be higher than the mass threshold value."
    ))
  }
  
  #message("-------- Number of ppm value across bins: ", length(ppmObs))
  if(length(ppmObs) > 10000) {
    ppmObs <- ppmObs[sample(x = seq_along(ppmObs), size = 5000)]
  }
  
  if(length(ppmObs) > 750) {
    
    checkPpm <- length(ppmObs)/2
    subsample <- TRUE
    while(subsample) {
      
      origDist <- stats::density(ppmObs, bw = 1)$y
      newDist1 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist2 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist3 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist4 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist5 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist6 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      newDist7 <-  stats::density(sample(ppmObs, checkPpm), bw = 1)$y
      
      klDistance <- list()
      subSamples <- ls()[grep("newDist",ls())]
      for(j in seq_along(subSamples)) {
        klDistance[[j]] <- suppressWarnings(entropy::KL.empirical(
          origDist,get(subSamples[j])))
      }
      klDistance <- unlist(klDistance)
      
      if(any(klDistance >= 0.5)) {
        subsample <- FALSE
      } else {
        checkPpm <- checkPpm/2
      }
      
    }
    
    ppmObs <- sample(ppmObs, checkPpm)
    #message("-------- Number of ppm value across bins after",
    #        " KL Distance Filtering: ",
    #        length(ppmObs))
    
  }
  
  
  ## 2019-04-09 added this here since it doesn't make sense to cluster too few
  ## features
  if(length(ppmObs) < 100) {
    
    kmeansPPM <- kmeans(ppmObs, 1)
    
  } else if(useGap) {
    
    gapStat <- cluster::clusGap(x = as.matrix(ppmObs),
                                FUNcluster = kmeans,
                                K.max = 5,
                                B = 7,
                                verbose = FALSE)
    
    gapStat <- gapStat$Tab
    gap <- diff(-gapStat[,3]) > 0
    if(any(gap)) {
      clusters <- max(which(gap)) + 1
    } else {
      clusters <- 1
    }
    
    kmeansPPM <- kmeans(ppmObs, clusters)
    
  } else {
    
    
    ## estimating clustering based on hard coded 80% Vexp threshold
    clustCount <- 1
    varExp <- 0
    while(varExp < varExpThresh && clustCount < length(ppmObs)/2) {
      kmeansPPM <- kmeans(ppmObs, clustCount)
      varExp <- kmeansPPM$betweenss/kmeansPPM$totss
      clustCount <- clustCount + 1
    }
    
  }
  
  ## cluster which contains smallest ppm values
  clusterSize <-sort(table(kmeansPPM$cluster),decreasing = TRUE)
  maxCluster <- names(clusterSize)[1]
  minCluster <- which(kmeansPPM$cluster == maxCluster)
  rm(clusterSize)
  
  x <- ppmObs
  n <- length(x)
  h <- 1
  ## delete this later
  gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
  gaussDKE <- function(a, x) gauss((x - a)/h)/(n * h)
  
  bumps <- vapply(X = ppmObs[minCluster], FUN = gaussDKE, x = x,
                  FUN.VALUE = numeric(length = length(x)))
  wholeKDE <- vapply(X = ppmObs, FUN = gaussDKE,
                     x,
                     FUN.VALUE = numeric(length =
                                           length(x)))
  
  ## calculating this ahead of time to avoid unnecessary downstream
  ## math
  cKdeMean <- sum(rowSums(bumps))/length(minCluster)
  OutlierScore <- rowSums(wholeKDE)/(cKdeMean)
  
  scoreSub <- which(OutlierScore > 1)
  
  
  ppmEst <- max(ppmObs[scoreSub])
  maxX <- ppmEst
  ppmEst <- ppmEst + sd(ppmObs[scoreSub])*3
  
  
  if(returnPpmPlots) {
    
    
    title <- paste(filename, 'ppm distribution:',
                   signif(observedPeak$start, digits = 4),
                   "-",
                   signif(observedPeak$end, digits = 4))
    
    output <- file.path(plotDir,paste0(gsub(" ", "_", title), ".pdf"))
    output <- sub(":", "", output)
    
    ## error here...
    par(mar=c(1,1,1,1))
    grDevices::pdf(output, width = 8, height = 6)
    
    ## adding heuristic here to make ploting easier to see
    if(length(ppmObs) < 300) {
      bw <- .1
    } else {
      bw <- .5
    }
    
    plot(stats::density(ppmObs,bw = bw),
         main = title,
         cex.main = 1.2, cex.lab = 1.3, cex.axis = 1.2) #+
    abline(v = maxX, lty = 2, col = "red") +
      abline(v = ppmEst, lty = 3, col = "blue")
    legend("topright",
           legend = c(paste("score > 1:", signif(maxX,digits = 3)),
                      paste("ppm estimate:", signif(ppmEst,digits = 3))),
           col = c("red","blue"),
           lty = c(2,3),cex = 1.1)
    grDevices::dev.off()
    
  }
  
  return(ppmEst)
}
peakwidth_est <- function(peak_vector,
                          time,
                          intensity,
                          start = NULL,
                          end = NULL,
                          old_r2 = NULL) {
  
  
  killSwitch <- FALSE
  
  # check to make sure input values come from vector
  if(!is.numeric(peak_vector)) {
    warning(paste("A non numeric vector was given to peakwidth_est().",
                  "This is incorrect. Check the function input."))
  }
  
  # updating data values to put into regression
  
  if(!is.null(start)) { # case where we are in second + iteration of algorithm
    
    end <- end + 1
    
  } else { # case where the algorithm is run for the first time
    
    peak_index <- which(time %in% peak_vector)
    if(length(peak_index) == 0) {
      stop(paste("The peak entered here could not be matched to the",
                 "chromatography data."))
    }
    
    start <- peak_index[1] - 1
    end <- peak_index[length(peak_index)]
    
  }
  
  # terms for lm
  points <- c(start,start-1,start-2,start-3,end,end+1,end+2,end+3)
  intensityObs <- intensity[points]
  modelIndex <- seq_along(intensityObs)
  
  # correcting na formation within intensity observation vector
  if(any(is.na(intensityObs))) {
    
    naObsIndex <- which(is.na(intensityObs))
    modelIndex <- modelIndex[-naObsIndex]
    intensityObs <- intensityObs[-naObsIndex]
    killSwitch <- TRUE
    
  }
  
  # running smoothing spline on the data
  if(sum(!is.na(peak_vector)) > 1) {
    splineOut <- smooth.spline(modelIndex, intensityObs)
    splineObs <- splineOut$fit$coef
    splineIndex <- seq_along(splineObs)
  } else {
    splineObs <- intensityObs
  }
  splineIndex <- seq_along(splineObs)
  
  # running a linear model on the outcome of the spline
  chrom_table <- data.frame(splineIndex, splineObs)
  model <- lm(formula = splineObs ~ splineIndex, data = chrom_table)
  new_r2 <- summary(model)$r.squared
  
  # used for comparison with previous itterations
  if(is.null(old_r2)) {
    old_r2 <- 0
  }
  
  # recursive case - returns previously calculated model fit if improvement
  if((old_r2 > new_r2 | old_r2 > .9) | killSwitch == TRUE) {
    
    # make sure to return numerical index of fit - not the values being compared
    peak_width <- c(peakStart = start-3, peakEnd = end+3)
    return(peak_width)
    
  } else {
    peakwidth_est(peak_vector, time, intensity, start,
                  end, old_r2 = new_r2)
  }
}


##### -----------------==========    Bottom of this function kit   ======----------------######
