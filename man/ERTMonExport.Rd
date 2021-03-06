% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ERTMon.R
\name{ERTMonExport}
\alias{ERTMonExport}
\title{Export processed data into CSV files.}
\usage{
ERTMonExport(
  ertObj,
  directoryName,
  modelID,
  fileNamePrefix = paste0(modelID, "-"),
  origin = "1900-01-01"
)
}
\arguments{
\item{ertObj}{An ERTMon object.}

\item{directoryName}{A directory name for the export. If \code{NULL} no files are written.}

\item{modelID}{A string.}

\item{fileNamePrefix}{A string.}

\item{origin}{A date-time object or something that can be coerced to such object, 
that can be used in as.POSIXct. 
It is passed to \code{\link{ERTMonTakeTimeSeriesDataFrame}}.}
}
\value{
An ERTMon object or \code{ERTMonFailureSymbol}.
}
\description{
Exports the computation specification, the feature matrix, and time grid
cells interpretation into CSV files.
}
\details{
The CSV files are written in the specified directory \code{directoryName}. 
The file name prefix \code{fileNamePrefix} is concatenated to the generic file names:
\code{"longFormComputationSpecification.csv", "featureMatrix.csv", "timeCellsInterpretation.csv", "featureMatrixTimeSeries.csv"}.
The conversion into long form of the computation specification is considered to be 
more convenient from a "model management" perspective. 
The data to be exported is assigned to result's \code{$Value}.
}
