% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_compatibility.R
\name{PerformMS2ResultsFormatting}
\alias{PerformMS2ResultsFormatting}
\title{PerformMS2ResultsFormatting
This function is used to format the results from other tools into the generic format of MetaboAnalystR for functional analysis
Currently,we are supporting the compatibility for four commonly used open-source tools: MS-FINDER, and SIRIUS
The first parameter file_path should be a valid file of the result.
User need to specify the type in the 2nd argument, type. This argument can be msfinder, or sirius
The 3rd column}
\usage{
PerformMS2ResultsFormatting(file_path, type, MS1_features_list = NA)
}
\arguments{
\item{file_path}{file path of NS2 file}

\item{type}{type, can be msfinder or sirius}

\item{MS1_features_list}{this is feature list used functional analysis}
}
\description{
PerformMS2ResultsFormatting
This function is used to format the results from other tools into the generic format of MetaboAnalystR for functional analysis
Currently,we are supporting the compatibility for four commonly used open-source tools: MS-FINDER, and SIRIUS
The first parameter file_path should be a valid file of the result.
User need to specify the type in the 2nd argument, type. This argument can be msfinder, or sirius
The 3rd column
}
