% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_data_utils.R
\name{InitDataObjects}
\alias{InitDataObjects}
\title{Constructs a dataSet object for storing data}
\usage{
InitDataObjects(data.type, anal.type, paired=FALSE)
}
\arguments{
\item{data.type}{The type of data, either list (Compound lists), conc (Compound concentration data), 
specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
or msspec (MS spectra data)}

\item{anal.type}{Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, mf, 
cmpdmap, smpmap, or pathinteg}

\item{paired}{indicate if the data is paired or not. Logical, default set to FALSE}
}
\description{
This functions handles the construction of a mSetObj object for storing data for further processing and analysis.
It is necessary to utilize this function to specify to MetaboAnalystR the type of data and the type of analysis you will perform.
}
\author{
Jeff Xia \email{jeff.xia@mcgill.ca}
McGill University, Canada
License: GNU GPL (>= 2)
}
