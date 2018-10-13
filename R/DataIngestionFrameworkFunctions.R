##===========================================================
## Event records transformations OOP framework functions
## Copyright (C) 2018  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

#---
# Title: Data ingestion framework functions
# Author: Anton Antonov
# Start date: 2018-10-07
#---
library(plyr)
library(dplyr)

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
#' @return a data frame (tibble) with columns c("EntityID","VarID","AValue")
AggregateEventRecordsBySpec <- function(specRow, eventRecordsData, entityData, aggrFuncSpecToFunc, outlierBoundaries = NULL ) {
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
      
      outBndrs <- QuartileIdentifierParameters( outBndrs$Value )
      ## outBndrs <- HampelIdentifierParameters( outBndrs$Value )
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
#' @detail NOT USED ANYMORE. See the method "normalizeGroupsBySpec" of the class DataTransformer.
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
## Verify directory
VerifyDataDirectory <- function( directoryName ) {
  file.exists( directoryName ) &&
    file.exists( file.path( directoryName, "eventRecords.csv" ) ) &&
    file.exists( file.path( directoryName, "entityAttributes.csv" ) )
}
