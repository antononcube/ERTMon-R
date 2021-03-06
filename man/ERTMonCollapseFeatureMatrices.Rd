% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ERTMon.R
\name{ERTMonCollapseFeatureMatrices}
\alias{ERTMonCollapseFeatureMatrices}
\title{Get feature sub-matrices collapsed.}
\usage{
ERTMonCollapseFeatureMatrices(
  ertObj,
  matrixNames = NULL,
  entityIDs = NULL,
  collapseFunction = colSums,
  completeColumnRangeQ = FALSE
)
}
\arguments{
\item{ertObj}{An ERTMon object.}

\item{matrixNames}{A character vector with names of feature sub-matrices to be collapsed.
If NULL then all feature sub-matrices are collapsed.}

\item{entityIDs}{A character vector with entity ID's (that feature sub-matrices row names.)
If NULL then all entity ID's are used.}

\item{collapseFunction}{A function that can be applied to a sparse matrix.}

\item{completeColumnRangeQ}{See \code{\link{ERTMonTakeContingencyMatrices}}.}
}
\value{
An ERTMon object.
}
\description{
Get feature sub-matrices collapsed.
}
\details{
Collapses specified feature sub-matrices with a specified function.
The collapse function can be one of \code{rowSums, rowMeans, colSums, colMeans} 
or any other function that can be applied.

The obtained list of collapsed matrices is assigned to \code{ertObj$Value}.
This function is fairly simple -- it was programmed in order to have 
the corresponding operation explicitly named in formula computation workflows.
}
\seealso{
Other Feature matrices: 
\code{\link{ERTMonPlotFeatureMatrices}()},
\code{\link{ERTMonStackFeatureMatrices}()}
}
\concept{Feature matrices}
