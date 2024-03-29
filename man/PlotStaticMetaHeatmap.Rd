% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multifac_asca_heatmap2.R
\name{PlotStaticMetaHeatmap}
\alias{PlotStaticMetaHeatmap}
\title{Create high resolution static HeatMap for download only}
\usage{
PlotStaticMetaHeatmap(
  mSetObj = NA,
  viewOpt = "detailed",
  clustSelOpt = "both",
  smplDist = "pearson",
  clstDist = "average",
  colorGradient = "bwm",
  includeRowNames = T,
  imgName,
  format = "png",
  dpi = 96,
  width = NA
)
}
\arguments{
\item{mSetObj}{Input the name of the created mSetObj (see InitDataObjects)}

\item{viewOpt}{high-level summary or plotting the names inside cell}
}
\description{
Plot a heatmap showing clustering patterns among the metadata
}
\author{
Jeff Xia \email{jeff.xia@mcgill.ca}
McGill University, Canada
License: GNU GPL (>= 2)
}
