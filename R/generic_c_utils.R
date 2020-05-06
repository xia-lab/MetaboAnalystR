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
<<<<<<< HEAD
  .C("imodwt", y, z, N, j, 
          L, ht, gt, out=XX)$out
=======
  .C(C_imodwt, y, z, N, j, 
          L, ht, gt, out=XX, PACKAGE = "MetaboAnalystR")$out
>>>>>>> 2ee6c5a421a6ec4ddefccb98e73fd6c5c0da0870
}


#' Internal C fucntion - C_modwt_r
#' @references Percival, D. B. and A. T. Walden (2000) Wavelet Methods for Time Series Analysis, Cambridge University Press.
C_modwt_r <- function(X,N,j, L, ht, gt,W, V){
<<<<<<< HEAD
  .C("modwt", X, N, as.integer(j), L, 
     ht, gt, W = W, V = V)[7:8]
=======
  .C(C_modwt, X, N, as.integer(j), L, 
     ht, gt, W = W, V = V, PACKAGE = "MetaboAnalystR")[7:8]
>>>>>>> 2ee6c5a421a6ec4ddefccb98e73fd6c5c0da0870
}

############# ============ ------------- Bin bottom ----------- ============ ###########
