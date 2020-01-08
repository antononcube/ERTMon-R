##===========================================================
## Event records transformations OOP framework functions
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
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

library(dplyr)
library(purrr)

#' @description Process data specification to have actionable values for NULL, NA, or other 'automatic' values.
#' @param dataSpec data specification
#' @param addLabelRowQ A logical value should 'Label' variable be added if missing.
ProcessDataSpecification <- function( dataSpec, addLabelRowQ = FALSE ) {
  
  dataSpecDF <- dataSpec
  
  ## Add 'Label' row if it does not exist.
  if ( addLabelRowQ && !( "Label" %in% dataSpecDF$Variable ) ) {
    
    warning( "No 'Label' row in given in the specification. Attempt to continue with automatically added 'Label' row.", call. = T)
    
    df <- do.call( data.frame, as.list( setNames( rep("NULL", ncol(dataSpecDF)), colnames(dataSpecDF) ) ) )
    df$Variable <- "Label"; df$Aggregation.interval.length <- 0
    dataSpecDF <- rbind( dataSpecDF, df )
  }
  
  ## Defaults
  dataSpecDF <- 
    purrr::map_dfr( split(dataSpecDF, dataSpecDF$Variable), function(x) { 
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
                                               purrr::map_lgl( dataSpecDF$Aggregation.function, is.na ),
                                             "Mean", dataSpecDF$Aggregation.function )
  
  dataSpecDF$Normalization.scope <- gsub( "^\\W*", "", dataSpecDF$Normalization.scope)
  dataSpecDF$Normalization.scope <- gsub( "\\W*$", "", dataSpecDF$Normalization.scope)
  
  dataSpecDF$Normalization.scope <- ifelse( dataSpecDF$Normalization.scope == "NULL" | 
                                              dataSpecDF$Normalization.scope == "NA" |
                                              purrr::map_lgl( dataSpecDF$Normalization.scope, is.na ) ,
                                            "None", dataSpecDF$Normalization.scope )
  
  dataSpecDF$Normalization.function <- gsub( "^\\W*", "", dataSpecDF$Normalization.function)
  dataSpecDF$Normalization.function <- gsub( "\\W*$", "", dataSpecDF$Normalization.function)
  
  dataSpecDF$Normalization.function <- ifelse( dataSpecDF$Normalization.function == "NULL" | 
                                                 dataSpecDF$Normalization.function == "NA" |
                                                 purrr::map_lgl( dataSpecDF$Normalization.function, is.na ) ,
                                               "None", dataSpecDF$Normalization.function )
  
  
  dataSpecDF <-
    cbind( dataSpecDF, 
           MatrixName = paste( dataSpecDF$Variable, dataSpecDF$Aggregation.function, sep = "."), 
           stringsAsFactors = FALSE )
  
  dataSpecDF$MatrixName <- gsub( "^Label.*$", "Label", dataSpecDF$MatrixName)
  dataSpecDF$MatrixName <- gsub( "^LocationID.*$", "LocationID", dataSpecDF$MatrixName)
  
  dataSpecDF$Aggregation.interval.length <- as.numeric(dataSpecDF$Aggregation.interval.length)
  dataSpecDF$Max.history.length <- as.numeric(dataSpecDF$Max.history.length)
  
  dataSpecDF$Aggregation.interval.length[ is.na(dataSpecDF$Aggregation.interval.length) ] <- min(dataSpecDF$Aggregation.interval.length, na.rm = TRUE)
  dataSpecDF$Max.history.length[ is.na(dataSpecDF$Max.history.length) ] <- min(dataSpecDF$Max.history.length, na.rm = TRUE)
  
  
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

#' @description Checks does computation specification .
#' @param compSpec A computation specification data frame.
#' @param labelVariable A string designating the variable attribute.
#' @return A data frame.
HasLabelRowQ <- function( compSpec, labelVariable = "Label" ) {
  if ( is.data.frame(compSpec) && ( "Variable" %in% colnames(compSpec) ) ) {
    sum( grep( pattern = labelVariable, x = compSpec$Variable ) ) > 0
  } else {
    FALSE
  }
}


#' @description Adds a label attribute row for each entity ID that does not have one.
#' @param entityAttributes A data frame with entity attributes.
#' @param labelValue A string to be used as a label attribute.
#' @return A data frame.
AddMissingLabelAttributes <- function( entityAttributes, labelValue = "None" ) {
  
  noLabelIDs <- 
    entityAttributes %>% 
    dplyr::filter( Attribute == "Label" )
  
  if( nrow(noLabelIDs) == 0 ) { 
    noLabelIDs <- unique(entityAttributes$EntityID) 
  } else {
    noLabelIDs <- setdiff( unique(entityAttributes$EntityID), noLabelIDs$EntityID )   
  }
  
  if( length(noLabelIDs) > 0 ) { 
    entityAttributes <- 
      rbind( 
        entityAttributes[, c("EntityID", "Attribute", "Value")], 
        data.frame( EntityID = noLabelIDs, Attribute = "Label", Value = labelValue )
      )
  }
  
  entityAttributes
}

#' @description Imposes a time grid over eventRecords subset according to specification row.
#' @param eventRecordsData Event records data in long form.
#' @param entityData A data frame entity specific data.
#' @param maxHistoryLength A max history length.
#' @param aggregationIntervalLength An aggregation interval length.
#' @param alignmentSpec A time series alignment specification argument 
#' with acceptable values \"MinTime\", \"MaxTime\", or a non-negative number. 
#' @param echoStepsQ A logical should the steps be indicated with \code{cat}.
#' @return A data frame (tibble) with columns c("EntityID","VarID","AValue","TimeGridCell", "MatrixName")
#' @details If \code{alignmentSpec} is a non-negative number the alignment is done by finding 
#' the difference \code{ObservationTime - alignmentSpec} and keeping the records for which
#' that difference is non-negative.
AddTimeGrid <- function(eventRecords, maxHistoryLength, aggregationIntervalLength, 
                        alignmentSpec = "MaxTime",
                        echoStepsQ = TRUE) {
  
  if( echoStepsQ ) { cat("\n\tFind most recent observation date for each entity...\n") }
  
  minMaxTimes <-
    eventRecords %>% 
    dplyr::select( EntityID, ObservationTime ) %>% 
    dplyr::group_by( EntityID ) %>% 
    dplyr::summarise( MaxTimeEpoch = max(ObservationTime, na.rm = T), MinTimeEpoch = min(ObservationTime, na.rm = T) ) %>% 
    dplyr::filter( !is.na(MaxTimeEpoch) )
  
  eventRecords <- 
    minMaxTimes %>% 
    dplyr::left_join(eventRecords, by = "EntityID") 

  if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
  
  if( echoStepsQ ) { cat("\n\tCompute differences with max time...\n") }
  
  if( tolower(alignmentSpec) %in% c( "maxtime", "max") ) {
    
    eventRecords <- dplyr::mutate( eventRecords, 
                                   DiffToMaxObsTime = MaxTimeEpoch - ObservationTime )
    
  } else if (  tolower(alignmentSpec) %in% c( "mintime", "min") ) {
    
    eventRecords <- dplyr::mutate( eventRecords, 
                                   DiffToMaxObsTime = ObservationTime - MinTimeEpoch )
    
  } else if ( is.numeric(alignmentSpec) && alignmentSpec >= 0 ) {

    eventRecords <- 
      eventRecords %>% 
      dplyr::mutate( DiffToMaxObsTime = ObservationTime - alignmentSpec ) %>% 
      dplyr::filter( DiffToMaxObsTime >= 0 )
    
  } else {
    stop( "The argument alignmentSpec is expected to be \"MinTime\", \"MaxTime\", or a non-negative number.", call. = TRUE )
  }
  
  if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
  
  if( echoStepsQ ) { cat("\n\tRestrict event records to specfied maximal history length...\n") }

  eventRecords <- dplyr::mutate( eventRecords, DiffToMaxObsTime = as.double( DiffToMaxObsTime ) )
  eventRecords <- dplyr::filter( eventRecords, DiffToMaxObsTime <= maxHistoryLength )
  ## In order to make this work with Spark innter join has to be used (dplyr or SQL).
  
  if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
  
  if( echoStepsQ ) { cat("\n\tFor each of the specified variables find time grid intervals...\n") }
  
  eventRecords <- dplyr::mutate( eventRecords, TimeGridCell = floor( DiffToMaxObsTime / aggregationIntervalLength ) )
  # timeCellOffset < 0
  # eventRecords <- dplyr::mutate( eventRecords, TimeGridCell = TimeGridCell + timeCellOffset )
  
  if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
  
  eventRecords
}

#' @description Finds entity-vs-time-grid-cell aggregation values based on variable-related specification.
#' @param specRow a speficication that has columns "Variable" and "Aggregation.function"
#' @param eventRecordsData event records data in long form
#' @param entityData entity specifica data
#' @param aggrFuncSpecToFunc a named elements list of aggregation functions
#' @param outlierBoundaries a data frame with columns c("Variable", "Lower", "Upper")
#' @param outlierIdentifierFunc outlier identifier function
#' @param echoStepsQ A logical should the steps be echoed with \code{cat}.
#' @param alignmentSpec A time series alignment specification argument 
#' with acceptable values \"MinTime\", \"MaxTime\", or a non-negative number. 
#' @return A data frame (tibble) with columns c(EntityID, TimeGridCell, VarID, AValue, MatrixName).
#' @details See the details of \code{AddTimeGrid} for clarifications for the argument \code{alignmentSpec}.
AggregateEventRecordsBySpec <- function(specRow, 
                                        eventRecordsData, entityData, 
                                        aggrFuncSpecToFunc,
                                        outlierBoundaries = NULL, outlierIdentifierFunc = QuartileIdentifierParameters,
                                        alignmentSpec = "MaxTime",
                                        echoStepsQ = TRUE) {
  ##print(paste(specRow,collapse = " "))
  func <- aggrFuncSpecToFunc[ specRow$Aggregation.function[[1]] ][[1]]
  mName <- paste( specRow$Variable, specRow$Aggregation.function, sep = ".")

  timeGridCellMaxNumberOfDigits <- floor(log10( specRow$Max.history.length / specRow$Aggregation.interval.length )) + 1
  
  if( specRow$Variable == "Attribute" ) {
    
    entityData %>% 
      dplyr::mutate( MatrixName = mName ) %>% 
      dplyr::mutate( VarID = paste("Attribute",Attribute,sep="."), TimeGridCell = 0 ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = 1 )
    
  } else if( specRow$Variable == "Label" ) {
    
    entityData %>% 
      dplyr::filter( Attribute == "Label" ) %>% 
      dplyr::mutate( VarID = paste("Label", Value, sep="."), TimeGridCell = 0 ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = 1 ) %>% 
      dplyr::mutate( MatrixName = "Label" )
    
  } else if( specRow$Variable == "LocationID" ) {
    
    ## Note the special time grid cells assignments for "LocationID",
    ## since "LocationID" is a separate column, not in "Variable" or entityAttributes .
    eventRecordsData %>% 
      AddTimeGrid( specRow$Max.history.length, specRow$Aggregation.interval.length, alignmentSpec = alignmentSpec, echoStepsQ = echoStepsQ ) %>% 
      dplyr::select( EntityID, LocationID, DiffToMaxObsTime, TimeGridCell ) %>%
      #dplyr::filter( DiffToMaxObsTime <= specRow$Max.history.length ) %>%
      #dplyr::mutate( TimeGridCell = floor( DiffToMaxObsTime / specRow$Aggregation.interval.length ) ) %>%
      dplyr::mutate( VarID = paste(LocationID, formatC( TimeGridCell, width = timeGridCellMaxNumberOfDigits, flag = "0"), sep=".") ) %>% 
      dplyr::group_by( EntityID, TimeGridCell, VarID ) %>% 
      dplyr::summarise( AValue = func(LocationID) ) %>% 
      dplyr::mutate( MatrixName = mName ) 
    
  } else if ( specRow$Aggregation.function %in% c( "OutliersCount", "OutCnt", "OutliersFraction", "OutFrc" ) ) {
    
    if ( !is.null(outlierBoundaries) && ( specRow$Variable %in% outlierBoundaries$Variable ) ) {
      outBndrs <- outlierBoundaries[ outlierBoundaries$Variable == specRow$Variable, ]
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
    
    res <- AggregateEventRecordsBySpec( specRow = specRow, 
                                        eventRecordsData = entityOutliers, 
                                        entityData = entityData,
                                        aggrFuncSpecToFunc = aggrFuncSpecToFunc,
                                        alignmentSpec = alignmentSpec, 
                                        echoStepsQ = echoStepsQ )
    
    if( nrow(res) > 0 ) { res$MatrixName <- mName }
    
    res
    
  } else {
    
    ## The standard case.
    
    eventRecordsData %>% 
      dplyr::filter( Variable == specRow$Variable ) %>% 
      AddTimeGrid( maxHistoryLength = specRow$Max.history.length, 
                   aggregationIntervalLength = specRow$Aggregation.interval.length, 
                   alignmentSpec = alignmentSpec, 
                   echoStepsQ = echoStepsQ ) %>% 
      dplyr::mutate( VarID = paste(Variable, formatC( TimeGridCell, width = timeGridCellMaxNumberOfDigits, flag = "0"), sep=".") ) %>% 
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
  
  assertthat::assert_that(FALSE, msg = "NOT USED ANYMORE.")
    
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

#' Empty computation specification row.
#' @description Gives a data frame with an "empty" computation specification row.
#' @return A data frame with one row.
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

#' @description Applies a formula term specification to a list of sub-matrices that comprise a feature matrix.
#' @param smats (sparse) matrices
#' @param formulaSpec formula specification
#' @param reduceFunc function to be applied when forming the numerator and denominator
#' @details The formula specification is expected to have the columns:
#' c("FeatureName", "Coefficient", "Exponent", "RatioPart") .
#' The column "FeatureName" is expeceted to have non-unique value.
#' The "RatioPart" can have one of the values "Denominator" or "Numerator" and no others.
#' The argument \code{reduceFunc} can have one of the values 'sum', "+", or "*" and no others.
#' The interpretation is:
#'   formulaMat = 
#'      reduceFunc[ Coefficient[i] * smats[ FeatureName[i] ] ^ Exponent[i], {i,NumeratorRows}] 
#'      /
#'      reduceFunc[ Coefficient[i] * smats[ FeatureName[i] ] ^ Exponent[i], {i,DenominatorRows}] 
ApplyFormulaTermSpecification <- function( smats, formulaSpec, reduceFunc = "+" ) {
  
  ## Verification of mat
  if( class(smats) != "list" ) {
    stop( "The arument smats is expected to be a list named matrix elements.", call. = TRUE )
  }
  
  inFeatures <- formulaSpec$FeatureName %in% names(smats)
  
  if( sum(inFeatures) == 0 ) {
    stop( "All feature names of the formula specification are unknown.", call. = TRUE )
  }
  
  if( nrow(formulaSpec) > sum(inFeatures) ) {
    warning( "Some feature names of the formula specification are not known.", call. = TRUE )
  }
  
  formulaSpec <- formulaSpec[ inFeatures, ]
  
  dimsDF <- purrr::map_dfr( smats[unique(formulaSpec$FeatureName)], function(x) data.frame( NRow = nrow(x), NCol = ncol(x) ) )
  if( mean( dimsDF$NRow == dimsDF$NRow[[1]] ) < 1 || mean( dimsDF$NCol == dimsDF$NCol[[1]] ) < 1 ) {
    stop( "The matrices smats[unique(formulaSpec$FeatureName)] from the argument smats are expected to have same number of rows and columns.", call. = TRUE )
  }
  
  ## Verification of formulaSpec.
  ## Additional checks have to be done for the names and types of the columns.
  expectedColumnNames <- c("FeatureName", "Coefficient", "Exponent", "RatioPart") 
  if( !( class(formulaSpec) == "data.frame" && 
         length( intersect( colnames(formulaSpec), expectedColumnNames) ) == length(expectedColumnNames) ) ) {
    stop( paste( "The argument formulaSpec is expected to be a data frame with columns:", paste( expectedColumnNames, collapse = ", " ), "." ), call. = TRUE )
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

#' @description Applies a formula term specification to a list of sub-matrices that comprise a feature matrix.
#' @param smats (sparse) matrices
#' @param formulaSpec formula specification
#' @param reduceFunc function to be applied when forming the numerator and denominator
#' @details The formula specification is expected to have the columns:
#' c("TermID", "TermCoefficient", "FeatureName", "ReduceFunction", "Coefficient", "Exponent", "RatioPart") .
ApplyFormulaSpecification <- function( smats, formulaSpec ) {
  
  expectedColumnNames <- c("TermID", "TermCoefficient", "FeatureName", "ReduceFunction", "Coefficient", "Exponent", "RatioPart") 
  if( !( class(formulaSpec) == "data.frame" && 
         length( intersect( colnames(formulaSpec), expectedColumnNames) ) == length(expectedColumnNames) ) ) {
    stop( paste( "The argument formulaSpec is expected to be a data frame with columns:", paste( expectedColumnNames, collapse = ", " ), "." ), call. = TRUE )
  }
  
  termCoeffCheck <-
    purrr::map_lgl( split(formulaSpec, formulaSpec$TermID), function(x) {
      mean(x$TermCoefficient) == x$TermCoefficient[[1]]
    })
  
  if( mean(termCoeffCheck) < 1 ) {
    stop( "The rows of formulaSpec with the same \"TermID\" value should have the same \"TermCoefficient\" value.", call. = TRUE )
  }
    
  res <- 
    purrr::map( 
      split(formulaSpec, formulaSpec$TermID), 
      function(fs) {
        m <- ApplyFormulaTermSpecification(smats = smats, formulaSpec = fs, reduceFunc = formulaSpec$ReduceFunction[[1]] ) 
        m * fs$TermCoefficient[[1]]
      } 
    )
  
  dimsDF <- purrr::map_dfr( res, function(x) data.frame( NRow = nrow(x), NCol = ncol(x) ) )
  if( mean( dimsDF$NRow == dimsDF$NRow[[1]] ) < 1 || mean( dimsDF$NCol == dimsDF$NCol[[1]] ) < 1 ) {
    warning( paste( "The obtained term matrices do not have the same number of rows and columns.", 
                    "Returning the list of term matrices instead of their sum."), 
             call. = TRUE )
    return(res)
  }
  
  reduce( .x = res, .f = function(a,m) a+m, .init = 0)
}


##-----------------------------------------------------------
## Computation specification conversions and checks
##-----------------------------------------------------------

#' @description Finds the type of a given computation specification.
#' @param compSpec An object to be tested as computation specification.
#' @return A string, one of "LongForm", "WideForm", "Unknown".
ComputationSpecificationType <- function( compSpec ) {
  if( ComputationSpecificationLongFormQ(compSpec) ) {
    "LongForm"
  } else if ( ComputationSpecificationWideFormQ(compSpec) ) {
    "WideForm"
  } else {
    "Unknown"
  }
}

#' @description Checks is an object a computation specification in wide form.
#' @param compSpec An object to be tested as computation specification.
#' @return A logical.
ComputationSpecificationWideFormQ <- function( compSpec ) {
  
  if( !is.data.frame(compSpec) ) {
    return(FALSE)
  }
  
  csColNames <- names( EmptyComputationSpecificationRow() )
  if( length( intersect( colnames(compSpec), csColNames ) ) < length(csColNames) ) {
    return(FALSE)
  }
  
  TRUE
}

#' @description Checks is an object a computation specification in long form.
#' @param compSpec An object to be tested as computation specification.
#' @return A logical.
ComputationSpecificationLongFormQ <- function( compSpec ) {
  
  if( !is.data.frame(compSpec) ) {
    return(FALSE)
  }
  
  # csColNames <- c("ModelID", "RowIndex", "ColumnName", "Value")
  csColNames <- c("RowIndex", "ColumnName", "Value")
  if( length( intersect( colnames(compSpec), csColNames ) ) < length(csColNames) ) {
    return(FALSE)
  }
  
  TRUE
}

#' @description Converts a wide form computation specification into a long form.
#' @param compSpec A computation specification.
#' @param modelID A string.
#' @return A data frame.
ComputationSpecificationToLongForm <- function( compSpec, modelID = NULL ) { 
  
  rownames(compSpec) <- NULL
  
  compSpec <- cbind( RowIndex = 1:nrow(compSpec), compSpec, stringsAsFactors = FALSE )  
  compSpec <- reshape2::melt( compSpec, id.vars = "RowIndex" )
  
  if( !is.null(modelID) ) {
    compSpec <- cbind( ModelID = modelID, compSpec, stringsAsFactors = FALSE )
  }
  
  colnames(compSpec) <- gsub( pattern = "variable", replacement = "ColumnName", x = colnames(compSpec) )
  colnames(compSpec) <- gsub( pattern = "value", replacement = "Value", x = colnames(compSpec) )
  
  compSpec 
}


#' @description Converts a wide form computation specification into a long form.
#' @param compSpec A computation specification.
#' @param modelID A string.
#' @return A data frame.
ComputationSpecificationToWideForm <- function( compSpec ) { 
  
  if( !ComputationSpecificationLongFormQ(compSpec) ) {
    stop( "A long form computation specification is expected as an argument.", call. = TRUE )
  }
  
  compSpec <- reshape2::dcast( compSpec, formula =  RowIndex ~ ColumnName, value.var = "Value" )
  
  compSpec$Aggregation.interval.length <- as.numeric(compSpec$Aggregation.interval.length)
  compSpec$Max.history.length <- as.numeric(compSpec$Max.history.length)
  
  compSpec[, -1]
}


