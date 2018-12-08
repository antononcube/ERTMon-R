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
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
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
#' @import magrittr
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
#' @description A synonym of \code{ERTMonUnit}.
#' @export
ERTMonObject <- ERTMonUnit


##===========================================================
## Setters and getters
##===========================================================

#' Take the value in a ERTMon object.
#' @description Takes the value from ERTMon monad object.
#' @param ertObj An ERTMon object.
#' @return Just \code{ertObj$Value}.
#' @family Set/Take functions
#' @export
ERTMonTakeValue <- function( ertObj ) {
  ertObj$Value
}

##-----------------------------------------------------------

#' Set event records.
#' @description Sets an event records data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param eRecs A data frame with event records.
#' @return An ERTMon object.
#' @family Set/Take functions
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

#' Take event records.
#' @description Takes the event records data frame from the monad object.
#' @param ertObj An ERTMon object.
#' @return A data frame.
#' @family Set/Take functions
#' @export
ERTMonTakeEventRecords <- function( ertObj ) {
  ertObj$EventRecords
}


##-----------------------------------------------------------

#' Set entity attributes.
#' @description Sets an entity attributes data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param eAttrs A data frame with entity attributes.
#' @return An ERTMon object.
#' @family Set/Take functions
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

#' Take entity attributes.
#' @description Takes the entity attributes data frame from the monad object.
#' @param ertObj An ERTMon object.
#' @return A data frame.
#' @family Set/Take functions
#' @export
ERTMonTakeEntityAttributes <- function( ertObj ) {
  ertObj$EntityAttributes
}


##-----------------------------------------------------------

#' Set computation specification.
#' @description Sets a computation specification data frame into the monad object.
#' @param ertObj An ERTMon object.
#' @param compSpec A data frame that is a computation specification.
#' @return An ERTMon object.
#' @family Set/Take functions
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

#' Take computation specification.
#' @description Takes the computation specification data frame from the monad object.
#' @param ertObj An ERTMon object.
#' @return A data frame.
#' @family Set/Take functions
#' @export
ERTMonTakeComputationSpecification <- function( ertObj ) {
  ertObj$ComputationSpecification
}

##-----------------------------------------------------------

#' Take feature name prefixes.
#' @description Returns the prefixes of the column names of the feature matrix 
#' using the computation specification.
#' @param ertObj An ERTMon object.
#' @return A character vector. 
#' @details The matrix is not computed; only the computation specification is needed.
#' @family Set/Take functions
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
#' @param smat If not NULL it is going to be used instead of \code{ertObj$Value}.
#' @param noColumnPrefixes If TRUE the column names are just integers.
#' @return A list of named matrices.
#' @details The sub-matrices are extracted through the corresponding prefixes.
#' An alternative is to use the sparse matrices that are in \code{ertObj$dtObj}.
#' @family Set/Take functions
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
    purrr::map( rnames, function(x) smat[, grep(x, colnames(smat)), drop=F ] )
  
  if( noColumnPrefixes ) {
    res <-
      purrr::map( res,
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
#' @return A (sparse) matrix or \code{ERTMonFailureSymbol}.
#' @details If the matrix does exists then \code{ERTMonFailureSymbol} is returned. 
#' The row names correspond to entities; the column names to features.
#' @family Set/Take functions
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

##-----------------------------------------------------------

#' Take transformed data.
#' @description Takes the transformed data frame from the data transformer object 
#' which is in the monad object.
#' @param ertObj An ERTMon object.
#' @return A data frame or \code{ERTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ERTMonTakeTrasformedData <- function( ertObj ) {

  if ( !ERTMonDataTransformerCheck(ertObj = ertObj, functionName = "ERTMonTakeTrasformedData", logicalResult = TRUE) ) {
    ERTMonFailureSymbol
  } else if( is.null(ertObj$dtObj@transformedData) ) { 
    ERTMonFailureSymbol 
  } else { 
    ertObj$dtObj@transformedData 
  }
}


##-----------------------------------------------------------

#' Take time cells interpretation.
#' @description Returns a data frame with the interpretation of the time cells.
#' @param ertObj An ERTMon object.
#' @return A data frame. 
#' @details Currently the entity time series can only be aligned to finish at 0. 
#' Hence the interpretation times are non-positive.
#' @family Set/Take functions
#' @export
ERTMonTakeTimeCellsInterpretation <- function( ertObj ) {
  if ( !ERTMonDataTransformerCheck(ertObj = ertObj, functionName = "ERTMonTakeFeatureMatrix", logicalResult = TRUE) ) {
    ERTMonFailureSymbol
  } else if( is.null(ertObj$dtObj@timeCellsInterpretation) ) { 
    ERTMonFailureSymbol 
  } else { 
    ertObj$dtObj@timeCellsInterpretation
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
#' @return If \code{logicalValue} is FALSE the result is ERTMon object; if TRUE the result is logical value.
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
#' @param outlierIdentifier Outlier parameters function.
#' @param alignmentSpec A time series alignment specification argument 
#' with acceptable values \"MinTime\", \"MaxTime\", or a non-negative number. 
#' @param echoStepsQ Should the steps be echoed?
#' @param progressObject An object to be used in a progress gauge.
#' @details The result feature matrix is assigned to \code{ertObj$Value}.
#' @return An ERTMon object.
#' @export
ERTMonProcessEventRecords <- function( ertObj, 
                                       outlierIdentifier = SPLUSQuartileIdentifierParameters, 
                                       alignmentSpec = "MaxTime", 
                                       echoStepsQ = TRUE, 
                                       progressObject = NULL ) {
  
  if( !ERTMonDataCheck( ertObj, "ERTMonProcessEventRecords", logicalResult = T ) ) {
    return(ERTMonFailureSymbol)
  }

  ## In order to adhere ot ERTMon's requirements we have to provide a 'Label' attribute.
  ertObj$EntityAttributes <- AddMissingLabelAttributes( ertObj$EntityAttributes, labelValue = "None" )
  
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
  
  dtObj@progressObject <- progressObject
  
  dtObj <- transformData( dtObj,
                          compSpecObj,
                          dwObj@eventRecords,
                          dwObj@entityAttributes,
                          alignmentSpec = alignmentSpec,
                          echoStepsQ = echoStepsQ )
  
  ertObj$compSpecObj <- compSpecObj
  ertObj$dtObj <- dtObj
  ertObj$Value <- dtObj@dataMat
  
  ertObj
}


##===========================================================
## Extract features
##===========================================================

#' Extract feature with an alredy made data transformer.
#' @description Processes "unseen" data with an already made data transformer object.
#' @param ertObj An ERTMon object.
#' @param eventRecords A data frame with event records.
#' @param entityAttributes A data frame with entity attributes.
#' @param echoStepsQ Should the steps be echoed?
#' @param progressObject An object to be used in a progress gauge.
#' @return An ERTMon object.
#' @details The result feature matrix is assigned to \code{ertObj$Value}.
#' @export
ERTMonExtractFeatures <- function( ertObj, eventRecords = NULL, entityAttribues = NULL, echoStepsQ = TRUE, progressObject = NULL ) {
  
  if( !ERTMonDataTransformerCheck(ertObj, functionName = "ERTMonExtractFeatures", logicalResult = TRUE) ) {
    return(ERTMonFailureSymbol)
  }
   
  dtObj <- transformData( ertObj$dtObj )
  dtObj@progressObject <- progressObject
                          
  if( is.null(eventRecords) && is.null(entityAttribues) ) {
    
    dtObj <- transformData( dtObj, ertObj$compSpecObj, ertObj$eventRecords, ertObj$entityAttributes, testDataRun = TRUE )
    
  } else {
    
    dtObj <- transformData( dtObj, ertObj$compSpecObj, eventRecords, entityAttributes, testDataRun = TRUE )
    
  }
  
  ## The result -- a contingency matrix
  ertObj$Value <- dtObj@dataMat
  
  ertObj
}


##===========================================================
## Read computation specification from a file
##===========================================================

#' Read a computation specification from a file.
#' @description Reads a computation specification data frame from a file.
#' @param ertObj An ERTMon object.
#' @param file A directory name.
#' @details The specified file is expected to be a CSV file.
#' @return An ERTMon object.
#' @export
ERTMonReadComputationSpecification <- function( ertObj, fileName, ingestQ = FALSE ) {
  
  compSpecObj <- new( "ComputationSpecification" )
  
  if ( ingestQ ) {
    
    compSpecObj <- compSpecObj %>% readSpec( fileName ) %>% ingestSpec()
    ertObj %>% ERTMonSetComputationSpecification( compSpecObj@parameters )
    
  } else {
    
    compSpecObj <- compSpecObj %>% readSpec( fileName )
    ertObj %>% ERTMonSetComputationSpecification( compSpecObj@originalParameters )
  }  
}
  

##===========================================================
## Read data from a directory
##===========================================================

#' Read data from a directory.
#' @description Reads event records, entity attributes, and computation specification
#' data frames from a directory.
#' @param ertObj An ERTMon object.
#' @param directoryName A directory name.
#' @param compSpec A computation specification data frame; if NULL it is read from the directory.
#' @param progressObject An object to use for a progress gauge.
#' @details The specified directory is expected to have the files 
#' \code{eventRecords.csv}, \code{entityAttributes.csv}, and \code{computationSpecification.csv}.
#' @return An ERTMon object.
#' @export
ERTMonReadDataFromDirectory <- function( ertObj, directoryName, readCompSpecQ = TRUE, progressObject = NULL ) {
 
  ##---------------------------------------------------------
  ## Data ingester
  ##---------------------------------------------------------
  diObj <- new( "DataIngester")  
  
  diObj@progressObject <- progressObject
  
  diObj <- diObj %>% readDataFromDirectory( directoryName ) %>% ingestData( "Label" )
  
  ##---------------------------------------------------------
  ## Computation specificaiton
  ##---------------------------------------------------------
  compSpecObj <- new( "ComputationSpecification" )
  
  
  if( readCompSpecQ ) {
    
    inSpecFileName <- file.path( directoryName, "computationSpecification.csv" )
    compSpecObj <- compSpecObj %>% readSpec( inSpecFileName ) 

    ertObj %>%
      ERTMonSetEventRecords( diObj@dataObj@eventRecords ) %>% 
      ERTMonSetEntityAttributes( diObj@dataObj@entityAttributes ) %>% 
      ERTMonSetComputationSpecification( compSpecObj@originalParameters )
    
  } else {
    
    ertObj %>%
      ERTMonSetEventRecords( diObj@dataObj@eventRecords ) %>% 
      ERTMonSetEntityAttributes( diObj@dataObj@entityAttributes )
  }    
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
#' @details The result matrix is assigned into \code{ertObj$Value}.
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
#' @param fileName A CSV file name. If \code{NULL} no file is written.
#' @param modelID A string; if \code{NULL} it is not used.
#' @return An ERTMon object or \code{ERTMonFailureSymbol}.
#' @details The value of the argument \code{modelID} (if not \code{NULL}) is
#' added as the first column to the feature matrix data frame (to be exported).
#' That data frame is assigned to the result monad object \code{$Value}.
#' @export
ERTMonExportToCSVFeatureMatrix <- function( ertObj, fileName = NULL, modelID = NULL ) {
  
  if( !ERTMonFeatureMatrixCheck(ertObj, logicalResult = TRUE) ) {
    return(ERTMonFailureSymbol)
  }
  
  if( !( is.null(fileName) || is.character(fileName) ) ) {
    warning( "The argument fileName is expected to be a string that is a valid file name.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  if( !( is.null(modelID) || is.character(modelID) ) ) {
    warning( "The argument modelID is expected to be a string.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  fMat <- ERTMonTakeFeatureMatrix(ertObj)
  fMat <- as(fMat,"dgCMatrix")
  resDF <- SparseMatrixToTriplets( fMat )
  resDF <- setNames(resDF, c("EntityID", "TimeCell", "Value") )

  
  if( is.character(modelID) ) {
    resDF <- cbind( ModelID = modelID, resDF, stringsAsFactors = FALSE )
  }     
  
  if( is.character(fileName) ) {
    write.csv( x = resDF, file = fileName, row.names = FALSE )
  }   
  
  ertObj$Value <- resDF
  
  ertObj
}

#' Export processed data into CSV files.
#' @description Exports the computation specification, the feature matrix, and time grid
#' cells interepretation into CSV files. 
#' @param ertObj An ERTMon object.
#' @param directoryName A diretory name for the export.
#' @param modelID A string.
#' @param fileNamePrefix A string.
#' @return An ERTMon object or \code{ERTMonFailureSymbol}.
#' @details The CSV files are written in the specified directory \code{directoryName}. 
#' The file name prefix \code{fileNamePrefix} is concatenated to the generic file names:
#' "longFormComputationSpecification.csv", "featureMatrix.csv", and "timeCellsInterpretation.csv".
#' The conversion into long form of the computation specification is considered to be 
#' more convenient from a "model management" perspective. 
#' @export
ERTMonExport <- function( ertObj, directoryName, modelID, fileNamePrefix = paste0(modelID,"-") ) {
  
  if( !ERTMonFeatureMatrixCheck(ertObj, logicalResult = TRUE) ) {
    return(ERTMonFailureSymbol)
  }
  
  if( !( is.character(directoryName) && file.exists(directoryName) ) ) {
    warning( "The argument directoryName is expected to be a string that is a valid directory name.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  if( !is.character(modelID) ) {
    warning( "The argument modelID is expected to be a string.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  if( !(is.null(fileNamePrefix) || is.character(fileNamePrefix) ) ) {
    warning( "The argument fileNamePrefix is expected to be a string or NULL.", call. = TRUE )
    return(ERTMonFailureSymbol)
  }
  
  if( is.null(fileNamePrefix) ) { fileNamePrefix <- "" }
  
  ## Export computation specification
  compSpec <- ERTMonTakeComputationSpecification(ertObj)
  rownames(compSpec) <- NULL

  compSpec <- cbind( RowIndex = 1:nrow(compSpec), compSpec, stringsAsFactors = FALSE )  
  compSpec <- reshape2::melt( compSpec, id.vars = "RowIndex" )
  compSpec <- cbind( ModelID = modelID, compSpec, stringsAsFactors = FALSE )
  
  write.csv( x = compSpec, file = file.path( directoryName, paste0(fileNamePrefix, "longFormComputationSpecification.csv")), row.names = FALSE )
  
  ## Export feature matrix
  ERTMonExportToCSVFeatureMatrix( ertObj, fileName = file.path( directoryName, paste0(fileNamePrefix, "featureMatrix.csv")), modelID = modelID )
  
  ## Export time grid cells interpretation
  tsDF <- ERTMonTakeTimeCellsInterpretation( ertObj )
  tsDF <- cbind( ModelID = modelID, tsDF, stringsAsFactors = FALSE )
  
  write.csv( x = tsDF, file = file.path( directoryName, paste0(fileNamePrefix, "timeCellsInterpretation.csv")), row.names = FALSE )
  
  ## Result
  ertObj
}

##===========================================================
## Support functions
##===========================================================

#' Verify does a directory have ERTMon data files.
#' @description Verify does a directory have the CSV files \code{eventRecords.csv} 
#' and \code{entityAttributes.csv}.
#' @param directoryName A directory name string.
#' @return A logical value.
#' @family Non-monadic functions.
#' @export
ERTMonVerifyDataDirectory <- function( directoryName ) {
  if( !is.character(directoryName) ) {
    stop("A string is expected for the argument directoryName.", call. = TRUE )
  }
  VerifyDataDirectory( directoryName )
}

 
#' Empty computation specification row.
#' @description Gives a data frame with an "empty" computation specification row.
#' @return A data frame with one row.
#' @details Non-monadic functions.
#' @export
ERTMonEmptyComputationSpecificationRow <- function() {
  EmptyComputationSpecificationRow()
}


#' Empty computation specification.
#' @description Gives a data frame with an "empty" computation specification row.
#' @return A data frame with one row.
#' @family Non-monadic functions.
#' @export
ERTMonEmptyComputationSpecification <- function( nrow = 1 ) {
  if( !is.integer(nrow) ) {
    stop("An integer is expected for the argument nrow.", call. = TRUE )
  }
  do.call(rbind, purrr::map(1:nrow, function(x) EmptyComputationSpecificationRow()))
}