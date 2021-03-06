% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_proc_utils.R
\name{IsSmallSmplSize}
\alias{IsSmallSmplSize}
\title{Check if the sample size is small}
\usage{
IsSmallSmplSize(mSetObj=NA)
}
\arguments{
\item{mSetObj}{Input name of the created mSet Object}
}
\description{
Returns whether or not the sanity check found that there were too many
groups in the dataset containing too few samples. It will return a 0 if the data passes the check,
or will return a 1 if the data does not.
}
