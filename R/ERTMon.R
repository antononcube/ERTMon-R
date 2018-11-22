##===========================================================
## Event records transformations monad in R
##
## BSD 3-Clause License
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
## 
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
## 
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
## 
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
##          SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

#---
# Title: ERTMon
# Author: Anton Antonov
# Start date: 2018-11-19
#---

#' @import plyr
#' @import dplyr
#' @import stringi
#' @import Matrix
#' @import RcppRoll
#' @import devtools
NULL

##===========================================================
## ERT Monad failure symbol
##===========================================================

#' Failure symbol for ERTMon.
#' @description Failure symbol for the monad ERTMon.
#' @export
ERTMonFailureSymbol <- NA

##===========================================================
## FE Unit
##===========================================================

#' Make a ERTMon Unit
#' @description Creates a monad object.
#' @param eventRecords A data frame with event records; can be NULL.
#' @param entityAttributes A data frame with entity attributes; can be NULL.
#' @param compSpec A data frame that is a computation specification; can be NULL.
#' @return An S3 class "ERTMon". In other words, a list with the attribute "class" set to "ERTMon".
#' @export
ERTMonUnit <- function( eventRecords = NULL, entityAttributes = NULL, compSpec = NULL ){
  
  res <- list( Value = NULL, EventRecords = eventRecords, EntityAttributes = entityAttributes, ComputationSpecification = compSpec )
  attr(res, "class") <- "ERTMon"
  res
}

#' Make a ERTMon unit.
#' @description A synonym of ERTMonUnit.
#' @export
ERTMonObject <- ERTMonUnit


##===========================================================
## Setters and getters
##===========================================================

#' Take the value in a ERTMon object.
#' @description Takes the value from ERTMon monad object.
#' @param ertObj An ERTMon object.
#' @return Just ertObj$Value.
#' @export
ERTMonTakeValue <- function( ertObj ) {
  ertObj$Value
}

##-----------------------------------------------------------

#' Set event records.
#' @description Set an event records data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param eRecs A data frame with event records.
#' @return An ERTMon object.
#' @export
ERTMonSetEventRecords <- function( ertObj, eRecs ) {
  expectedColNames <- c("EntityID", "LocationID", "ObservationTime", "Variable", "Value")
  if( ! ( is.data.frame(eRecs) && length(intersect( colnames(eRecs), expectedColNames)) == length(expectedColNames) ) ) { 
    warning( paste("The argument eRecs is expected to be a data frame with columns:", paste(expectedColNames, collapse =","), "."), call. = TRUE) 
    return(ERTMonFailureSymbol)
  }
  ertObj$EventRecords <- eRecs[, expectedColNames]
  ertObj
}

##-----------------------------------------------------------

#' Set entity attributes.
#' @description Set an entity attributes data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param eAttrs A data frame with entity attributes.
#' @return An ERTMon object.
#' @export
ERTMonSetEntityAttributes <- function( ertObj, eAttrs ) {
  expectedColNames <- c("EntityID", "Attribute", "Value")
  if( ! ( is.data.frame(eAttrs) && length(intersect( colnames(eAttrs), expectedColNames)) == length(expectedColNames) ) ) { 
    warning( paste("The argument eAttrs is expected to be a data frame with columns:", paste(expectedColNames, collapse =","), "."), call. = TRUE) 
    return(ERTMonFailureSymbol)
  }
  ertObj$EntityAttributes <- eAttrs[, expectedColNames]
  ertObj
}

##-----------------------------------------------------------

#' Set computation specification.
#' @description Set a computation specification data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param compSpec A data frame that is a computation specification.
#' @return An ERTMon object.
#' @export
ERTMonSetComputationSpecification <- function( ertObj, compSpec ) {
  expectedColNames <- names(EmptyComputationSpecificationRow())
  if( ! ( is.data.frame(compSpec) && 
          length(intersect( colnames(compSpec), expectedColNames )) == length(expectedColNames) ) ) { 
    warning( paste("The argument compSpec is expected to be a data frame with columns:", paste(expectedColNames, collapse =","), "." ), call. = TRUE) 
    return(ERTMonFailureSymbol)
  }
  ertObj$ComputationSpecification <- compSpec
  ertObj
}

##-----------------------------------------------------------

#' Take feature name prefixes.
#' @description Returns the prefixes of the column names of the feature matrix 
#' using the computation spefication.
#' @param ertObj an ERTMon object.
#' @return A character vector. 
#' @details The matrix is not computed; only the computation specification is needed.
#' @export
ERTMonTakeFeatureNamePrefixes <- function( ertObj ) {
  if( is.null(ertObj$ComputationSpecification) ) {
    stop( "Cannot find computation specification.", call. = TRUE )
  }
  paste( ertObj$ComputationSpecification$Variable, ertObj$ComputationSpecification$Aggregation.function, sep = ".")
}

##-----------------------------------------------------------

#' Take feature sub-matrices.
#' @description Returns the sub-matrices of the feature matrix.
#' @param ertObj An ERTMon object.
#' @param smat If not NULL it is going to be used instead of ertObj$Value.
#' @param noColumnPrefixes If TRUE the column names are just integers.
#' @return A list of named matrices.
#' @details The sub-matrices are extracted through the corresponding prefixes.
#' An alternative is to use the sparse matrices that are in ertObj$dtObj.
#' @export
ERTMonTakeContingencyMatrices <- function( ertObj, smat = NULL, noColumnPrefixes = TRUE ) {
  
  if( is.null(smat) ) {
    smat <- ertObj$Value
  }
  
  if( !( class(smat) == "dgCMatrix") ) {
    stop("A sparse matrix is expected as a second argument or as a pipeline value.", call. = TRUE )
  }
  
  if( is.null(ertObj$dtObj) ) {
    stop("Cannot find data transformation object. Transform the data first.", call. = TRUE )
  }
  
  ## Or use ERTMonTakeFeatureNames(ertObj) .
  rnames <- paste( ertObj$dtObj@compSpec@parameters$Variable, ertObj$dtObj@compSpec@parameters$Aggregation.function, sep = ".")
  
  res <-
    map( rnames, function(x) smat[, grep(x, colnames(smat)), drop=F ] )
  
  if( noColumnPrefixes ) {
    res <-
      map( res,
           function(x) {
             if( !is.null(x) && ncol(x)>0) { colnames(x) <- 1:ncol(x) }
             x
           })
  }
  names(res) <- rnames
  
  res
}

##-----------------------------------------------------------

#' Take the feature matrix.
#' @description Returns the feature matrix.
#' @param ertObj An ERTMon object.
#' @return A (sparse) matrix or ERTMonFailureSymbol.
#' @details If the matrix does exists then ERTMonFailureSymbol is returned. 
#' The row names correspond to entities; the column names to features.
#' @export
ERTMonTakeFeatureMatrix <- function( ertObj ) {
  
  if ( !ERTMonDataTransformerCheck(ertObj = ertObj, functionName = "ERTMonTakeFeatureMatrix", logicalResult = TRUE) ) {
    ERTMonFailureSymbol
  } else if( is.null(ertObj$dtObj@dataMat) ) { 
    ERTMonFailureSymbol 
  } else { 
    ertObj$dtObj@dataMat 
  }
}


##===========================================================
## Data presence check
##===========================================================

#' Check presence of required data.
#' @description Checks does an ERTMon object have event records, entity attributes, and computation specification.
#' @param ertObj An ERTMon object.
#' @param functionName A name of the delegating function (if any).
#' @param logicalResult Should the result be logical value?
#' @return If \param(logicalValue) is FALSE the result is ERTMon object; if TRUE the result is logical value.
#' @export
ERTMonDataCheck <- function( ertObj, functionName = NULL, logicalResult = FALSE ) {
  
  res <- TRUE
  
  if( is.null(functionName) || nchar(functionName) == 0 ) { 
    functionName <- ""
  } else {
    functionName <- paste0( functionName, ":: ") 
  } 
  
  if( is.null(ertObj$EventRecords) ) {
    warning( paste0( functionName, "Cannot find event records."), call. = TRUE)
    res <- FALSE
  }
  
  if( is.null(ertObj$EntityAttributes) ) {
    warning( paste0( functionName, "Cannot find entity attributes."), call. = TRUE)
    res <- FALSE
  }
  
  if( is.null(ertObj$ComputationSpecification) ) {
    warning( paste0( functionName, "Cannot find computation specification."), call. = TRUE)
    res <- FALSE
  }
  
  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { ERTMonFailureSymbol }
  else { ertObj }
}


##===========================================================
## Member presense check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an ERTMon object.
#' @param ertObj An ERTMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an ERTMon object.
#' @export
ERTMonMemberPresenceCheck <- function( ertObj, memberName, memberPrettyName = memberName, functionName = "", logicalResult = FALSE ) {
  
  res <- TRUE
  
  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }
  
  if( is.null(ertObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, "."), call. = TRUE) )
    res <- FALSE
  }
  
  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { ERTMonFailureSymbol }
  else { ertObj }
}


##===========================================================
## Data transformer check
##===========================================================

#' Check presence of a data transformer object.
#' @description Checks the presence of a data transformation S4 object in a given ERTMon object.
#' @param ertObj An ERTMon object.
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an ERTMon object.
#' @export
ERTMonDataTransformerCheck <- function( ertObj, functionName = "", logicalResult = FALSE ) {
  
  ERTMonMemberPresenceCheck( ertObj, "dtObj", "data transformer object", functionName = functionName, logicalResult = logicalResult)
}


##===========================================================
## Feature matrix check
##===========================================================

#' Feature matrix presence check.
#' @description Checks the presence of a feature matrix in a given ERTMon object.
#' @param ertObj An ERTMon object.
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an ERTMon object.
#' @export
ERTMonFeatureMatrixCheck <- function( ertObj, functionName = "", logicalResult = FALSE ) {
  
  res <- ERTMonDataTransformerCheck(ertObj = ertObj, functionName = "ERTMonTakeFeatureMatrix", logicalResult = TRUE) 
  
  if( !res ) {
    if( logicalResult) { FALSE }
    else { ERTMonFailureSymbol }
  }
  
  if( is.null(ertObj$dtObj@dataMat) ) { 
    if( logicalResult) { FALSE }
    else { ERTMonFailureSymbol }
  } else {
    if( logicalResult) { TRUE }
    else { ertObj }
  }
}

##===========================================================
## Make time series feature extractor
##===========================================================

#' Process the data and make feature matrix.
#' @description Processes the set event records using the set computation specification in an ERTMon object. 
#' @param ertObj An ERTMon object.
#' @param echoStepsQ Should the steps be echoed?
#' @param outlierIdentifier Outlier parameters function.
#' @return An ERTMon object.
#' @export
ERTMonProcessEventRecords <- function( ertObj, echoStepsQ = TRUE, outlierIdentifier = SPLUSQuartileIdentifierParameters ) {
  
  if( !ERTMonDataCheck( ertObj, "ERTMonProcessEventRecords", logicalResult = T ) ) {
    return(ERTMonFailureSymbol)
  }

  ## In order to adhere ot ERTMon's requirements we have to provide a 'Label' attribute.
  ertObj$EntityAttributes <- 
    rbind( 
      ertObj$EntityAttributes, 
      data.frame( EntityID = unique(ertObj$EntityAttributes$EntityID), Attribute = "Label", Value = "Yes" )
    )
  
  ## Computation specification
  compSpecObj <- new( "ComputationSpecification" )
  compSpecObj <- setSpec( compSpecObj,  ertObj$ComputationSpecification )
  compSpecObj <- ingestSpec( compSpecObj )
  
  ## Data ingesting
  diObj <- new( "DataIngester")
  
  diObj <- setEventRecords( diObj, ertObj$EventRecords )
  diObj <- setEntityAttributes( diObj, ertObj$EntityAttributes )
  
  diObj <- ingestData( diObj, "Label" )
  
  dwObj <- diObj@dataObj
  
  ## Data transformer
  ## dtObj <- new( "DataTransformerCatMatrices" )
  dtObj <- new( "DataTransformer" )
  
  dtObj <- transformData( dtObj,
                          compSpecObj,
                          dwObj@eventRecords,
                          dwObj@entityAttributes )
  
  ertObj$compSpecObj <- compSpecObj
  ertObj$dtObj <- dtObj
  ertObj$Value <- dtObj@dataMat
  
  ertObj
}


##===========================================================
## Compute formula (with a formula spec)
##===========================================================

#' Compute specified formula.
#' @description Computes a formula with a given specification using the feature 
#' sub-matrices of an ERTMon object.
#' @param ertObj An ERTMon object.
#' @param formulaSpec A formula specification.
#' @param reduceFunc Reduction function, one of "+" or "*".
#' @return An ERTMon object.
#' @details The result matrix is assigned into ertObj$Value.
#' @export
ERTMonComputeFormula <- function( ertObj, formulaSpec, reduceFunc = "+" ) {
  
  if( !( is.data.frame(formulaSpec) && colnames(formulaSpec) == c("FeatureName", "Coefficient", "Exponent", "RatioPart") ) ) {
    stop( paste("The argument formulaSpec is expected to be a data frame",
                "with columns c(\"FeatureName\", \"Coefficient\", \"Exponent\", \"RatioPart\")."), call. = TRUE )
  }
  
  smats <- ERTMonTakeContingencyMatrices( ertObj, noColumnPrefixes = T )
  
  resMat <- ApplyFormulaSpecification( smats = smats[ grep("Label", names(smats), invert = T) ], formulaSpec = formulaSpec, reduceFunc = reduceFunc )
  
  ertObj$Value <- resMat
  
  ertObj
}



##===========================================================
## Export (computed) 
##===========================================================

#' Export the feature matrix into a CSV file.
#' @description Exports the feature matrix into a CSV file.
#' @param ertObj An ERTMon object.
#' @param fileName A CSV file name. 
#' @export
ERTMonExportToCSVFeatureMatrix <- function( ertObj, fileName ) {
  
  if( !ERTMonFeatureMatrixCheck(ertObj, logicalResult = TRUE) ) {
    return(ERTMonFailureSymbol)
  }
  
  if( !is.character(fileName) ) {
    warning( "The argument fileName is expected to be a string that is a valid file name.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  fMat <- ERTMonTakeFeatureMatrix(ertObj)
  fMat <- as(fMat,"dgCMatrix")
  resDF <- SparseMatrixToTriplets( fMat )
  resDF <- setNames(resDF, c("EntityID", "TimeCell", "Value") )
  
  write.csv( x = resDF, file = fileName, row.names = FALSE )
}
