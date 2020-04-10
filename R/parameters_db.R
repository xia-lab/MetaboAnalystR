# Parameter Function
#' Set parameters for peak picking using XCMS and CAMERA
#' @description This function sets all the parameters used for downstream
#' pre-processing of user's raw MS data based on specific LC-MS platform. 
#' @param platform Character, specify the LC-MS platform used in pratice, including "UPLC-Q/E",
#' "UPLC-Q/TOF","UPLC-T/TOF","UPLC-Ion_Trap","UPLC-Orbitrap","UPLC-G2S","HPLC-Q/TOF","HPLC-Ion_Trap","HPLC-Orbitrap","HPLC-S/Q"
#' @param Peak_method Character, specify the algorithm to perform peak detection. "centwave" 
#' to use the CentWave algorithm, and "matchedFilter" to use the MatchedFilter algorithm.
#' @param RT_method Character, specify the algorithm to perform tetention time alignment, including "loess" and "obiwarp".
#' Default is "loess".
#' @param ppm Numeric, specify the mass error in ppm.
#' @param min_peakwidth Numeric, specify the minimum peak width in seconds.
#' @param max_peakwidth Numeric, specify the maximum peak width in seconds. 
#' @param snthresh Numeric, specify the signal to noise threshold.
#' @param mzdiff Numeric, specify the minimum m/z difference for signals to be considered as 
#' different features when retention times are overlapping. 
#' @param bw Numeric, specify the band width (sd or half width at half maximum) of gaussian 
#' smoothing kernel to be applied during peak grouping.
#' @param noise Numeric, specify the noise level for peaking picking.
#' @param min_frac Numeric, specify fraction of samples in each group that contain the feature for it to be grouped.
#' @param min_sample_num Numeric, specify minimum number of sample(s) in each group that contain the feature for it to be included.
#' @param max_feats Numeric, specify the maximum number of features to be identified.
#' @param peakgroup Boolean, if true, PeakGroup algorithm is used for peak alignment; if false, Obiwarp method is used. 
#' @param bin_size Numeric, specify the bin size (in m/z) to be used for the profile matrix generation used for peak alignment (Obiwarp method).
#' @param min_frac_retcor Numeric, specify fraction of samples in all groups that contain the peaks for them to be aligned (PeakGroup method).
#' @param rt_filt Boolean, if true, users must specify the minimum and maximum retention
#' time to be included in the analysis. By default this is set to 200 - 1000. 
#' @param rt_min Numeric, specify the minimum retention time.
#' @param rt_max Numeric, specify the maximum retention time.
#' @author Zhiqiang Pang \emial {zhiqiang.pang@mail.mcgill.ca}, Jasmine Chong \email{jasmine.chong@mail.mcgill.ca},
#' Mai Yamamoto \email{yamamoto.mai@mail.mcgill.ca}, and Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export

SetPeakParam <- function(platform = "general",Peak_method = "centWave",RT_method = "loess",
                         mzdiff,snthresh,bw,
                         ppm,min_peakwidth,max_peakwidth,noise,prefilter,value_of_prefilter,
                         fwhm,steps,sigma,
                         profStep,minFraction,minSamples,maxFeatures,
                         max,extra,span,smooth,family,fitgauss,
                         verbose.columns,mzCenterFun,integrate,
                         ...){
  peakParams <- list()
  if (missing(platform)){
    platform<-"general"
  } else{
    match.arg(arg = platform ,choices = c("general","UPLC-Q/E","UPLC-Q/TOF","UPLC-T/TOF",
                                          "UPLC-Ion_Trap","UPLC-Orbitrap","UPLC-G2S",
                                          "HPLC-Q/TOF","HPLC-Ion_Trap","HPLC-Orbitrap",
                                          "HPLC-S/Q"))
  }
  ### Platform Selection--UPLC----
  if (platform=="UPLC-Q/E"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 5;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 20;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 4;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 0;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="UPLC-Q/TOF"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 15;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 20;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="UPLC-T/TOF"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 15;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 20;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 0;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="UPLC-Ion_Trap"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 50;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 20;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 4;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 0;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="UPLC-Orbitrap"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 2.5;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 20;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 5000;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="UPLC-G2S"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 15;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 2;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 25;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 500;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 0.5;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  ### Platform Selection--HPLC----
  if (platform=="HPLC-Q/TOF"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 30;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 10;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 60;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 500;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="HPLC-Ion_Trap"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 50;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 10;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 60;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="HPLC-Orbitrap"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 3;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 10;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 60;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  if (platform=="HPLC-S/Q"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 30;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 10;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 60;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 6;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 30;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.4;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  ### Platform Selection--OTHERS----
  if (platform=="general"){
    if (missing(Peak_method)){
      peakParams$Peak_method <- "centWave"
    } else{
      peakParams$Peak_method <- Peak_method;
    };
    if (missing(RT_method)){
      peakParams$RT_method <- "peakgroup"
    } else{
      peakParams$RT_method <- RT_method;
    };
    ## Parameters for Peakpicking
    if (Peak_method=="centWave"){
      if (missing(ppm)){
        peakParams$ppm <- 5;
      } else{
        peakParams$ppm <- ppm;
      };
      if (missing(min_peakwidth)){
        peakParams$min_peakwidth <- 5;
      } else{
        peakParams$min_peakwidth <- min_peakwidth;
      };
      if (missing(max_peakwidth)){
        peakParams$max_peakwidth <- 30;
      } else{
        peakParams$max_peakwidth <- max_peakwidth;
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh;
      };
      if (missing(noise)){
        peakParams$noise <- 1000;
      } else{
        peakParams$noise <- noise
      };
      if (missing(prefilter)){
        peakParams$prefilter <- 3;
      } else{
        peakParams$prefilter <- prefilter
      };
      if (missing(value_of_prefilter)){
        peakParams$value_of_prefilter <- 100;
      } else{
        peakParams$value_of_prefilter <- value_of_prefilter
      };
    }
    if (Peak_method=="matchedFilter"){
      if (missing(fwhm)){
        peakParams$fwhm <- 30;
      } else{
        peakParams$fwhm <- fwhm
      };
      if (missing(sigma)){
        peakParams$sigma <- 12.74;
      } else{
        peakParams$sigma <- sigma
      };
      if (missing(steps)){
        peakParams$steps <- 2;
      } else{
        peakParams$steps <- steps
      };
      if (missing(max)){
        peakParams$max <- 10;
      } else{
        peakParams$max <- max
      };
      if (missing(snthresh)){
        peakParams$snthresh <- 10;
      } else{
        peakParams$snthresh <- snthresh
      };
      if (missing(mzdiff)){
        peakParams$mzdiff <- 0.01;
      } else{
        peakParams$mzdiff <- mzdiff;
      };
    };
    ## Parameters for Grouping-Density Method Only
    if (missing(bw)){
      peakParams$bw <- 10;
    } else{
      peakParams$bw <- bw;
    };
    if (missing(minFraction)){
      peakParams$minFraction <- 0.5;
    } else{
      peakParams$minFraction <- minFraction
    };
    if (missing(minSamples)){
      peakParams$minSamples <- 1;
    } else{
      peakParams$minSamples <- minSamples
    };
    if (missing(maxFeatures)){
      peakParams$maxFeatures <- 100;
    } else{
      peakParams$maxFeatures <- maxFeatures
    };
    if (missing(fitgauss)){
      peakParams$fitgauss<-FALSE
    } else {
      peakParams$fitgauss<-fitgauss
    };
    if (missing(verbose.columns)){
      peakParams$verbose.columns<-FALSE
    } else {
      peakParams$verbose.columns<-verbose.columns
    };
    if (missing(mzCenterFun)){
      peakParams$mzCenterFun<-"wMean"
    } else {
      peakParams$mzCenterFun<-mzCenterFun
    };
    if (missing(integrate)){
      peakParams$integrate<-1
    } else {
      peakParams$integrate<-integrate
    }
    ## Parameters for RT correction
    if (RT_method=="loess"){
      if (missing(extra)){
        peakParams$extra <- 1;
      } else{
        peakParams$extra <- extra
      };
      if (missing(span)){
        peakParams$span <- 0.25;
      } else{
        peakParams$span <- span
      };
      if (missing(smooth)){
        peakParams$smooth <- "loess";
      } else{
        peakParams$smooth <- smooth
      };
      if (missing(family)){
        peakParams$family <- "gaussian";
      } else{
        peakParams$family <- family
      };
    }
    if (RT_method=="obiwarp"){
      if (missing(profStep)){
        peakParams$profStep <- 1;
      } else{
        peakParams$profStep <- profStep
      };
      # Set profStep only here, profStep equals binsize
      # other parameters use default
    }
  }
  #### Other Parameters
  # None for now !
  return(peakParams)
}


SetPeakParam_old <- function(alg = "centwave", ppm = 10, min_pkw = 10, 
                             max_pkw = 60, sn_thresh = 6, mzdiff = 0.01, bw = 5,
                             min_frac = 0.5, min_sample_num = 1,
                             max_feats = 100, 
                             peakgroup = FALSE,
                             bin_size = 1, min_frac_retcor = 0.9,
                             rt_filt = FALSE, 
                             rt_min = 200, rt_max = 1000){
  
  peakParams <- list()
  
  peakParams$algorithm <- alg
  peakParams$polarity <- polarity
  peakParams$ppm <- ppm
  peakParams$min_pkw <- min_pkw
  peakParams$max_pkw <- max_pkw
  peakParams$sn_thresh <- sn_thresh
  peakParams$mzdiff <- mzdiff
  peakParams$bw <- bw
  peakParams$min_frac <- min_frac
  peakParams$min_sample_num <- min_sample_num
  peakParams$max_feats <- max_feats
  peakParams$peakgroup <- peakgroup
  peakParams$bin_size <- bin_size
  peakParams$min_frac_retcor <- min_frac_retcor
  peakParams$rt_filt <- rt_filt
  peakParams$rt_min <- rt_min
  peakParams$rt_max <- rt_max
  
  return(peakParams)
}
