% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tandem_ms.R
\name{plotMirror}
\alias{plotMirror}
\title{plotMirror}
\usage{
plotMirror(
  mSetObj = NA,
  featureidx = 1,
  precMZ,
  ppm,
  imageNM = "",
  dpi = 300,
  format = "png",
  width = 8,
  height = 8,
  cutoff_relative = 5
)
}
\arguments{
\item{mSetObj}{mSetObj}

\item{featureidx}{index of feature}

\item{precMZ}{mz of precursor}

\item{ppm}{ppm for ms2 fragment matching mz error}

\item{imageNM}{image name}

\item{dpi}{dpi of images}

\item{format}{format of images}

\item{width}{width of images}

\item{height}{height of images}

\item{cutoff_relative}{cutoff of relative intensity to filter out}
}
\description{
plotMirror
}
\author{
Zhiqiang Pang
}
