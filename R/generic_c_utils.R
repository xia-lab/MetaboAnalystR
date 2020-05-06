#' MetaboAnalystR: A package for computating the notorious bar statistic.
#'
#' The MetaboAnalystR package provides a pipeline for metabolomics processing.
#' 
#' @section MetaboAnalystR functions:
#' The MetaboAnalystR functions ...
#'
#' @docType package
#' @name MetaboAnalystR
#' @useDynLib MetaboAnalystR, .registration=TRUE, .fixes = "C_"
NULL
#> NULL


############# =========== ------------- C function Bin ----------- =========== ###########

#' Internal C fucntion - C_imodwt_r
#' @references Percival, D. B. and A. T. Walden (2000) Wavelet Methods for Time Series Analysis, Cambridge University Press.
C_imodwt_r <- function(y,z,N,j, L, ht, gt, XX){
  .C("imodwt", y, z, N, j, 
          L, ht, gt, out=XX)$out
}


#' Internal C fucntion - C_modwt_r
#' @references Percival, D. B. and A. T. Walden (2000) Wavelet Methods for Time Series Analysis, Cambridge University Press.
C_modwt_r <- function(X,N,j, L, ht, gt,W, V){
  .C("modwt", X, N, as.integer(j), L, 
     ht, gt, W = W, V = V)[7:8]
}

############# ============ ------------- Bin bottom ----------- ============ ###########
