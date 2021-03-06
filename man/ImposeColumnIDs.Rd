% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataConversionFunctions.R
\name{ImposeColumnIDs}
\alias{ImposeColumnIDs}
\title{Impose column ID's to a sparse matrix.}
\usage{
ImposeColumnIDs(colIDs, smat)
}
\arguments{
\item{colIDs}{A character vector of column ID's.}

\item{smat}{A matrix with named columns.}
}
\value{
A sparse matrix.
}
\description{
Makes sure that the rows of a matrix are in 1-to-1 correspondence to an array of row ID's
}
