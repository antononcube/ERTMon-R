% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ERTMon.R
\name{ERTMonReadComputationSpecification}
\alias{ERTMonReadComputationSpecification}
\title{Read a computation specification from a file.}
\usage{
ERTMonReadComputationSpecification(
  ertObj,
  fileName,
  ingestQ = FALSE,
  echoStepsQ = FALSE
)
}
\arguments{
\item{ertObj}{An ERTMon object.}

\item{echoStepsQ}{Should the computational steps be proclaimed?}

\item{file}{A directory name.}
}
\value{
An ERTMon object.
}
\description{
Reads a computation specification data frame from a file.
}
\details{
The specified file is expected to be a CSV file.
}
\seealso{
Other Data ingestion functions: 
\code{\link{ERTMonReadDataFromDirectory}()}
}
\concept{Data ingestion functions}
