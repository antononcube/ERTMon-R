##===========================================================
## Event records transformations OOP framework functions
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
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================
##
## TODO: 
##   1. [ ] Reimplement plyr functions with purrr.
##   2. [ ] Implement corresponding unit tests.
##   3. [ ] Add more expected arguments checks.
##   4. [ ] Implement a more general formula for a sum of terms.
##
##===========================================================
#---
# Title: Data ingestion framework functions
# Author: Anton Antonov
# Start date: 2018-10-07
#---

library(plyr)
library(dplyr)
library(purrr)

#' @description Process data specification to have actionable values for NULL, NA, or other 'automatic' values.
#' @param dataSpec data specification
ProcessDataSpecification <- function( dataSpec ) {
  
  dataSpecDF <- dataSpec
  
  ## Add 'Label' row if it does not exist.
  if ( !( "Label" %in% dataSpecDF$Variable ) ) {
    
    warning( "No 'Label' row in given in the specification. Attempt to continue with automatically added 'Label' row.", call. = T)
    
    df <- do.call( data.frame, as.list( setNames( rep("NULL", ncol(dataSpecDF)), colnames(dataSpecDF) ) ) )
    df$Variable <- "Label"; df$Aggregation.interval.length <- 0
    dataSpecDF <- rbind( dataSpecDF, df )
  }
  
  ## Defaults
  dataSpecDF <- 
    ddply( dataSpecDF, c("Variable"), function(x) { 
      if ( mean( is.na( x$Aggregation.interval.length ) ) == 1 ) { mval <- 1800 }
      else { mval <- min( x$Aggregation.interval.length, na.rm = T) }
      x$Aggregation.interval.length[ is.na(x$Aggregation.interval.length) ] <- mval
      x
    } )
  
  dataSpecDF$Aggregation.function <- gsub( "^\\W*", "", dataSpecDF$Aggregation.function)
  dataSpecDF$Aggregation.function <- gsub( "\\W*$", "", dataSpecDF$Aggregation.function)
  
  ## Process the aggreation function column ( NULL | NA ) -> Mean
  dataSpecDF$Aggregation.function <- ifelse( dataSpecDF$Aggregation.function == "NULL" | 
                                               dataSpecDF$Aggregation.function == "NA" |
                                               laply( dataSpecDF$Aggregation.function, is.na ) ,
                                             "Mean", dataSpecDF$Aggregation.function )
  
  dataSpecDF$Normalization.scope <- gsub( "^\\W*", "", dataSpecDF$Normalization.scope)
  dataSpecDF$Normalization.scope <- gsub( "\\W*$", "", dataSpecDF$Normalization.scope)
  
  dataSpecDF$Normalization.scope <- ifelse( dataSpecDF$Normalization.scope == "NULL" | 
                                        dataSpecDF$Normalization.scope == "NA" |
                                        laply( dataSpecDF$Normalization.scope, is.na ) ,
                                      "None", dataSpecDF$Normalization.scope )
  
  dataSpecDF$Normalization.function <- gsub( "^\\W*", "", dataSpecDF$Normalization.function)
  dataSpecDF$Normalization.function <- gsub( "\\W*$", "", dataSpecDF$Normalization.function)
  
  dataSpecDF$Normalization.function <- ifelse( dataSpecDF$Normalization.function == "NULL" | 
                                              dataSpecDF$Normalization.function == "NA" |
                                              laply( dataSpecDF$Normalization.function, is.na ) ,
                                            "None", dataSpecDF$Normalization.function )
  
  
  dataSpecDF <-
    cbind( dataSpecDF, 
           MatrixName = paste( dataSpecDF$Variable, dataSpecDF$Aggregation.function, sep = "."), 
           stringsAsFactors = FALSE )
  
  dataSpecDF$Aggregation.interval.length <- as.numeric(dataSpecDF$Aggregation.interval.length)
  dataSpecDF$Max.history.length <- as.numeric(dataSpecDF$Max.history.length)
  
  if( "LocationID" %in% dataSpecDF$Variable ) {
    
    if( !is.numeric( dataSpecDF[ dataSpecDF$Variable == "LocationID", "Max.history.length"] ) ||
        is.na( dataSpecDF[ dataSpecDF$Variable == "LocationID", "Max.history.length"] ) ) {
      dataSpecDF[ dataSpecDF$Variable == "LocationID", "Max.history.length"] <- max( dataSpecDF$Max.history.length, na.rm = T)
    }
    
    if( !is.numeric( dataSpecDF[ dataSpecDF$Variable == "LocationID", "Aggregation.interval.length"] ) ||
        is.na( dataSpecDF[ dataSpecDF$Variable == "LocationID", "Aggregation.interval.length"] ) ) {
      dataSpecDF[ dataSpecDF$Variable == "LocationID", "Aggregation.interval.length"] <- dataSpecDF[ dataSpecDF$Variable == "LocationID", "Max.history.length"] 
    }
    
  }
  
  # Return result
  dataSpecDF
}


#' @description Finds entity-vs-time-grid-cell aggregation values based on variable-related specification.
#' @param specRow a speficication that has columns "Variable" and "Aggregation.function"
#' @param eventRecordsData event records data in long form
#' @param entityData entity specifica data
#' @param aggrFuncSpecToFunc a named elements list of aggregation functions
#' @param outlierBoundaries a data frame with columns c("Variable", "Lower", "Upper")
#' @param outlierIdentifierFunc outlier identifier function
#' @return a data frame (tibble) with columns c("EntityID","VarID","AValue")
AggregateEventRecordsBySpec <- function(specRow, eventRecordsData, entityData, aggrFuncSpecToFunc, outlierBoundaries = NULL, outlierIdentifierFunc = QuartileIdentifierParameters) {
  ##print(paste(specRow,collapse = " "))
  func <- aggrFuncSpecToFunc[ specRow$Aggregation.function[[1]] ][[1]]
  mName <- paste( specRow$Variable, specRow$Aggregation.function, sep = ".")
  if( specRow$Variable == "Attribute" ) {
    entityData %>% 
      dplyr::mutate( VarID = paste("Attribute",Attribute,sep="."), TimeGridCell = 0 ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = 1 ) %>%
      dplyr::mutate( MatrixName = mName )
  } else if( specRow$Variable == "Label" ) {
    entityData %>% 
      dplyr::filter( Attribute == "Label" ) %>% 
      dplyr::mutate( VarID = paste("Label", Value, sep="."), TimeGridCell = 0) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = 1 ) %>%
      dplyr::mutate( MatrixName = "Label" )
  } else if( specRow$Variable == "LocationID" ) {
    ## Note the special time grid cells assignments for "LocationID",
    ## since "LocationID" is a separate column, not in "Variable" or entityAttributes .
    eventRecordsData %>% 
      dplyr::select( EntityID, LocationID, DiffToMaxObsTime ) %>%
      dplyr::filter( DiffToMaxObsTime <= specRow$Max.history.length ) %>%
      dplyr::mutate( TimeGridCell = floor( DiffToMaxObsTime / specRow$Aggregation.interval.length ) ) %>%
      dplyr::mutate( VarID = paste(LocationID, TimeGridCell, sep=".") ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = func(LocationID) ) %>%
      dplyr::mutate( MatrixName = mName )
  } else if ( specRow$Aggregation.function %in% c( "OutliersCount", "OutCnt", "OutliersFraction", "OutFrc" ) ) {
    
    if ( !is.null(outlierBoundaries) && ( specRow$Variable %in% outlierBoundaries$Variable ) ) {
      outBndrs <- outlierBoundaries[ outlierBoundaries$Variabl == specRow$Variable, ]
      outBndrs <- c( outBndrs[1,"Lower"], outBndrs[1,"Upper"] )
    } else {
      ## This probably should not be happening.
      stop( paste("No available outliers for", specRow$Variable), call. = T)
      ## Find the outliers boundaries for the variable.
      outBndrs <-
        eventRecordsData %>% 
        dplyr::filter( Variable == specRow$Variable )
      
      ## outBndrs <- HampelIdentifierParameters( outBndrs$Value )
      outBndrs <- outlierIdentifierFunc( outBndrs$Value )
    }
    
    ## Mark the outliers for each entity.
    entityOutliers <-
      eventRecordsData %>% 
      dplyr::filter( Variable == specRow$Variable ) %>% 
      dplyr::mutate( Value = ifelse( outBndrs[[1]] < Value & Value < outBndrs[[2]], 0, 1 ) ) %>% 
      dplyr::filter( Value > 0 )
    
    ## For each entity find the time grid cell averages of OutCnt and OutFrc.
    specRowOrig <- specRow
    
    if ( specRow$Aggregation.function %in% c( "OutliersCount", "OutCnt" ) ) {
      specRow$Aggregation.function <- "Count"
    } else {
      specRow$Aggregation.function <- "Mean"
    }
    
    specRow$Variable <- paste0( specRow$Variable, ".Outlier" )
    entityOutliers <- 
      entityOutliers %>% 
      dplyr::mutate( Variable = specRow$Variable )
    
    res <- AggregateEventRecordsBySpec( specRow = specRow, eventRecordsData = entityOutliers, entityData = entityData, aggrFuncSpecToFunc = aggrFuncSpecToFunc )
    
    if( nrow(res) > 0 ) { res$MatrixName <- mName }
    res
  } else {
    eventRecordsData %>% 
      dplyr::filter( Variable == specRow$Variable ) %>% 
      dplyr::mutate( VarID = paste(Variable, TimeGridCell, sep=".") ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = func(Value) ) %>%
      dplyr::mutate( MatrixName = mName )
  }
  
}

#' @description NOT USED ANYMORE. Applies a normalization function to long form contingency matrix data.
#' @param specRow a speficication that has columns "MatrixName" and "Normalization"
#' @param matLongFormData aggregated event records in long form
#' @param entityAttributes entity attributes data
#' @param normalizationFuncSpecToFunc a named elements list of normalization functions
#' @details NOT USED ANYMORE. See the method "normalizeGroupsBySpec" of the class DataTransformer.
NormalizeGroupsBySpec <- function(specRow, matLongFormData, entityAttributes, normalizationFuncSpecToFunc ) {
  
  func <- aggrFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]

  allEntityAttrs <- unique(entityAttributes$Attribute)
  
  if ( ! ( specRow$Normalization.function[[1]] %in% names(normalizationFuncSpecToFunc) ) ) {
    
    warning("Uknown normalization function. Continuing by ignoring it.", call. = TRUE )
    
    matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName )
    
  } else if ( specRow$Normalization.scope[[1]] == "Variable" ) {
    
    dfNormalizationValues <- 
      matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
      dplyr::summarise( NormalizationValue = func(AValue) ) %>% 
      dplyr::ungroup()
    
    matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
      dplyr::inner_join( dfNormalizationValues, by = "MatrixName" ) %>%
      dplyr::group_by( EntityID ) %>% 
      dplyr::mutate( AValue = AValue / NormalizationValue )
    
  } else if ( specRow$Normalization.scope[[1]] %in% allEntityAttrs ) {
    
    dfNormalizationValues <- 
      matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName ) %>%
      dplyr::inner_join( entityAttributes[, c("EntityID", "Attribute")] %>% 
                           dplyr::filter( Attribute == specRow$Normalization.scope[[1]] ),
                         by = "EntityID" ) %>% 
      dplyr::group_by( Attribute ) %>% 
      dplyr::summarise( NormalizationValue = func(AValue) ) %>% 
      dplyr::ungroup()
    
    matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
      dplyr::inner_join( entityAttributes[, c("EntityID", "Attribute")], by = "EntityID" ) %>% 
      dplyr::inner_join( dfNormalizationValues, by = "Attribute" ) %>%
      dplyr::group_by( EntityID ) %>% 
      dplyr::mutate( AValue = AValue / NormalizationValue )
    
  } else if ( specRow$Normalization.function[[1]] %in% names(normalizationFuncSpecToFunc) ) {
    
    # cat( "in:", specRow$Normalization.function[[1]], "\n" )
    func <- normalizationFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]
    matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
      group_by( EntityID ) %>% 
      dplyr::mutate( AValue = func(AValue) )
    
  } else {
    
    matLongFormData %>% 
      dplyr::filter( MatrixName == specRow$MatrixName )
    
  }
}

##-----------------------------------------------------------
## Impose row and column ID's
##-----------------------------------------------------------


##-----------------------------------------------------------
## Verify directory
##-----------------------------------------------------------
VerifyDataDirectory <- function( directoryName ) {
  file.exists( directoryName ) &&
    file.exists( file.path( directoryName, "eventRecords.csv" ) ) &&
    file.exists( file.path( directoryName, "entityAttributes.csv" ) )
}

##-----------------------------------------------------------
## Empty computation specification row
##-----------------------------------------------------------

EmptyComputationSpecificationRow <- function() {
  data.frame(
    "Variable" = NA,                    "Explanation" = "",
    "Type" = "numerical",               "Convert.type" = "NULL",               
    "Aggregation.interval.length" = 60, "Aggregation.function" = "Mean",
    "Max.history.length" = 3600,        "Normalization.scope" = "Entity",       
    "Normalization.function" = "Mean",  "Moving.average.window" = "NULL",
    "Critical.label" = "NULL",
    stringsAsFactors = FALSE)
}


##-----------------------------------------------------------
## Formula specification application to feature sub-matrices
##-----------------------------------------------------------

#' @description Applies a formula specification to a list of sub-matrices that comprise a feature matrix.
#' @param smats (sparse) matrices
#' @param formulaSpec formula specification
#' @param reduceFunc function to be applied when forming the numerator and denominator
#' @details The formula specification is expected to have the columns:
#' c("FeatureName", "Coefficient", "Exponent", "RatioPart") .
#' The column "FeatureName" is expeceted to have non-unique value.
#' The "RatioPart" can have one of the values "Denominator" or "Numerator" and no others.
#' The argument \param reduceFunc can have one of the values 'sum', "+", or "*" and no others.
#' The interpretation is:
#'   formulaMat = 
#'      reduceFunc[ Coefficient[i] * smats[ FeatureName[i] ] ^ Exponent[i], {i,NumeratorRows}] 
#'      /
#'      reduceFunc[ Coefficient[i] * smats[ FeatureName[i] ] ^ Exponent[i], {i,DenominatorRows}] 
ApplyFormulaSpecification <- function( smats, formulaSpec, reduceFunc = "+" ) {
  
  ## Verification of mat
  if( class(smats) != "list" ) {
    stop( "The arument smats is expected to be a list named matrix elements.", call. = TRUE )
  }
  
  dimsDF <- map_dfr( smats, function(x) data.frame( NRow = nrow(x), NCol = ncol(x) ) )
  if( mean( dimsDF$NRow == dimsDF$NRow[[1]] ) < 1 || mean( dimsDF$NCol == dimsDF$NCol[[1]] ) < 1 ) {
    stop( "The matrices in the argument smats are expected to have same number of rows and columns.", call. = TRUE )
  }
  
  ## Verification of formulaSpec.
  ## Additional checks have to be done for the names and types of the columns.
  if( !( class(formulaSpec) == "data.frame" && 
         colnames(formulaSpec) == c("FeatureName", "Coefficient", "Exponent", "RatioPart") ) ) {
    stop( "The arument formulaSpec is expected to be a data frame with columns: c(\"FeatureName\", \"Coefficient\", \"Exponent\", \"RatioPart\").", call. = TRUE )
  }
  
  ## Verificatoin of reduceFunc
  if ( !( reduceFunc %in% c( "+", "*") ) ) {
    stop( "The expected values for the argument reduceFunc are '+', or '*'.", call. = TRUE )
  }
  
  ## Filter out rows with unknown feature names.
  formulaSpecTemp <- formulaSpec
  formulaSpecTemp <- formulaSpecTemp %>% dplyr::filter( FeatureName %in% names(smats) )
  
  
  if( nrow(formulaSpecTemp) == 0 ) {
    stop( "The formula specification data frame has no known feature names.", call. = TRUE )
  } else if( nrow(formulaSpecTemp) < nrow(formulaSpec) ) {
    warning( "Some feature names are unknown.", call. = TRUE )
  }
  
  matNRows <- nrow(smats[[1]])
  matNCols <- ncol(smats[[1]])
  
  ## Compute numerator vector
  formulaSpecTemp <- formulaSpec[ formulaSpec$RatioPart == "Numerator", ]
  numeratorMat <- matrix( rep( 1, matNRows*matNCols ), nrow = matNRows)
  if( nrow(formulaSpecTemp) > 0 ) {
    numeratorMat <- 
      map( 1:nrow(formulaSpecTemp), 
           function(i) { 
             formulaSpecTemp$Coefficient[[i]] * smats[[ formulaSpecTemp$FeatureName[[i]] ]] ^ formulaSpecTemp$Exponent[[i]]
           } )
    numeratorMat <- Reduce( reduceFunc, numeratorMat)
  }
  
  ## Compute denominator vector
  ## Very similar / same as the code above.
  formulaSpecTemp <- formulaSpec[ formulaSpec$RatioPart == "Denominator", ]
  denominatorMat <- matrix( rep( 1, matNRows*matNCols ), nrow = matNRows)
  if( nrow(formulaSpecTemp) > 0 ) {
    denominatorMat <- 
      map( 1:nrow(formulaSpecTemp), 
           function(i) { 
             formulaSpecTemp$Coefficient[[i]] * smats[[ formulaSpecTemp$FeatureName[[i]] ]] ^ formulaSpecTemp$Exponent[[i]]
           } )
    denominatorMat <- Reduce( reduceFunc, denominatorMat)
  }
  
  ## Result
  numeratorMat / denominatorMat
}
