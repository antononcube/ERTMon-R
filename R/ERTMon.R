##===========================================================
## Event records transformations monad in R
## Copyright (C) 2018  Anton Antonov
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


##===========================================================
## ERT Monad failure symbol
##===========================================================

ERTMonFailureSymbol <- NA

##===========================================================
## FE Unit
##===========================================================

ERTMonUnit <- function( eventRecords = NULL, entityAttributes = NULL, compSpec = NULL ){
  
  res <- list( Value = NULL, EventRecords = eventRecords, EntityAttributes = entityAttributes, ComputationSpecification = compSpec )
  attr(res, "class") <- "ERTMon"
  res
}

ERTMonObject <- ERTMonUnit


##===========================================================
## Setters and getters
##===========================================================

ERTMonTakeValue <- function( ertObj ) {
  ertObj$Value
}

##-----------------------------------------------------------

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

ERTMonTakeFeatureNames <- function( ertObj ) {
  if( is.null(ertObj$ComputationSpecification) ) {
    stop( "Cannot find computation specification.", call. = TRUE )
  }
  paste( ertObj$ComputationSpecification$Variable, ertObj$ComputationSpecification$Aggregation.function, sep = ".")
}

##-----------------------------------------------------------

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

ERTMonDataTransformerCheck <- function( ertObj, functionName = "", logicalResult = FALSE ) {
  
  ERTMonMemberPresenceCheck( ertObj, "dtObj", "data transformer object", functionName = functionName, logicalResult = logicalResult)
}


##===========================================================
## Data transformer check
##===========================================================

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

## The computation is completely wrong/not developed; just a filler at this point.

#' @param ertObj ERTMon monad object
#' @param formulaSpec formula specification
#' @param reduceFunc reduction function, one of "+" or "*"
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
