##===========================================================
## Event records transformations monad simple test data creation
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
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================
##
## Mission
##--------------------------
##
## Create simple event records data in order to provide test data for ERTMon.
## Supported are:
##   1. [X] both small and large number of entities,
##   2. [X] both small and large number of variables,
##   3. [X] different starting and finishing times across entities.
##
##===========================================================

##===========================================================
## Load libraries
##===========================================================

library(purrr)
library(ggplot2)
library(reshape2)
library(lubridate)

##===========================================================
## Generate entities
##===========================================================
GenereateSimpleTestDataEntityAttributes <- function( numberOfEntities ) {
  
  groupIDs <- round(rnorm( n = 2*numberOfEntities, mean = 5, sd = 4))
  groupIDs <- groupIDs[ 0 <= groupIDs & groupIDs <= 10 ]
  groupIDs <- sample( groupIDs, numberOfEntities )

  locations <- runif( n = length(groupIDs), min = 0, max = 1 )
  locations <- ifelse( locations > 0.5, "far", "near" )
  
  entityAttributes <- data.frame( EntityID = 1:length(groupIDs), GroupID = groupIDs, LocationID = locations, stringsAsFactors = FALSE )
  
  entityAttributes <- reshape2::melt( entityAttributes, id.vars = "EntityID" )
  entityAttributes <- setNames( entityAttributes, c("EntityID", "Attribute", "Value" ) )
  entityAttributes <- entityAttributes[ order(entityAttributes$EntityID), ]
  
  entityAttributes
}


##===========================================================
## Generate patients vitals
##===========================================================
GenereateSimpleTestDataEventRecords <- function( entityAttributes,
                                                 numberOfVariables = 3,
                                                 timeInterval = 900, 
                                                 numberOfTimeCells = 36,
                                                 randomStartTimesQ = FALSE,
                                                 variableFunction = "Linear" ) {

  if( !is.data.frame(entityAttributes) ) {
    stop( "The argument entityAttributes is expected to be a data frame.", call. = TRUE )
  }  
  
  numberOfVariables <- round(numberOfVariables)
  if( !( is.numeric(numberOfVariables) && numberOfVariables > 0 ) ) {
    stop( "The argument numberOfVariables is expected to be a positive integer.", call. = TRUE )
  } 
  
  knownvariableFunctions <- c( "Constant", "Linear", "Random" )
  if( !( is.function(variableFunction) || is.character(variableFunction) && ( tolower(variableFunction) %in% tolower(knownvariableFunctions) ) ) ) {
    stop( paste( "The argument variableFunction is expected to be a function or one of:", paste0( knownvariableFunctions, collapse = ",") ), call. = TRUE )
  }  
  
  if( is.function(variableFunction) ) {
    varFunc <- variableFunction
  } else if( tolower(variableFunction) == "constant" ) {
    varFunc <- function(x) {1}
  } else if( tolower(variableFunction) == "linear" ) {
    varFunc <- function(x) {x/timeInterval}
  } else if( tolower(variableFunction) == "random" ) {
    varFunc <- function(x) {rnorm(n = 1, mean = 100, sd = 10)}
  }
  
  variableMeans <- runif( n = numberOfVariables, min = 0, max = 100 )
  names(variableMeans) <- paste0( "Var.", 1:numberOfVariables )
  
  timeGridIndexes <- seq(0,numberOfTimeCells-1)
  
  if( randomStartTimesQ ) {
    entityIDToRandomStartTime <- runif( n = unique(entityAttributes$EntityID), min = 0, max = 1000 )
    names(entityIDToRandomStartTime) <- unique(entityAttributes$EntityID)
  }
  
  fakeEventRecords <- 
    purrr::map_dfr( names(variableMeans), function(vMean) {

      purrr::map_dfr( unique(entityAttributes$EntityID), function(eid) {
        
        timeGrid <- timeInterval * timeGridIndexes
        if( randomStartTimesQ ) {
          timeGrid <- timeGrid + timeInterval * entityIDToRandomStartTime[[eid]]
        } 
        
        values <- purrr::map_dbl( floor(as.numeric(timeGrid)), varFunc )

        timeGrid <- Sys.time() + timeGrid
        
        ## "EntityID","LocationID","ObservationTime","Variable","Value","ObservationTime"
        data.frame( EntityID = eid, 
                    LocationID = "UKNWN", 
                    ObservationTimeString = timeGrid, 
                    Variable = vMean, 
                    Value = values, 
                    ObservationTime = floor(as.numeric(timeGrid)), 
                    stringsAsFactors = FALSE )
        
      })
    })

  fakeEventRecords  
} 


##===========================================================
## ERTMonSimpleTestData
##===========================================================

#' Simple test data generation
#' @description Generates test data with specified parameters and writes the 
#' corresponding CSV files in a specified directory.
#' @param numberOfEntities The number of entities.
#' @param numberOfVariables The number of variables
#' @param timeInterval The time interval corresponding to one time cell.
#' @param numberOfTimeCells The number of time cells.
#' @param randomStartTimesQ Should the starting times for different entities be different.
#' @param variableFunction A function or a function specification string, 
#' one of "Constant", "Linear", or "Random".
#' @param exportDirectoryName The name of the directory where the CSV files should be written.
#' If \code{NULL} the files are not exported
#' @return A list with named elements. 
#' @details The element names corrspond to the exported files.
#' The element names of the result are "EntityAttributes", "EventRecords", "ComputationSpecification".
#' The function specified with \code{variableFunction} is applied to 
#' the points of a generated regular time grid.
#' @family Non-monadic functions
#' @export
ERTMonSimpleTestData <- function( numberOfEntities = 10,
                                  numberOfVariables = 3,
                                  timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = FALSE,
                                  variableFunction = "Linear", 
                                  exportDirectoryName = NULL ) {
  
  fakeEntityAttributes <- GenereateSimpleTestDataEntityAttributes( numberOfEntities = numberOfEntities )
  
  fakeEventRecords <- GenereateSimpleTestDataEventRecords( entityAttributes = fakeEntityAttributes, 
                                                           numberOfVariables = numberOfVariables, 
                                                           timeInterval = timeInterval, 
                                                           numberOfTimeCells = numberOfTimeCells,
                                                           randomStartTimesQ = randomStartTimesQ,
                                                           variableFunction = variableFunction) 
  
  fakeCompSpec <- ERTMonEmptyComputationSpecification( nrow = as.integer(3 * numberOfVariables) )
  fakeCompSpec$Variable <- sort( rep_len( unique(fakeEventRecords$Variable), length.out = nrow(fakeCompSpec) ) )
  fakeCompSpec$Aggregation.interval.length <- rep_len( timeInterval * c(1,3,3), length.out = nrow(fakeCompSpec) )
  fakeCompSpec$Aggregation.function <- rep_len( c("Mean", "Range", "Max"), length.out = nrow(fakeCompSpec) )  
  fakeCompSpec$Max.history.length <- rep_len( numberOfTimeCells * timeInterval, length.out = nrow(fakeCompSpec) )  
  fakeCompSpec$Normalization.scope <- rep_len( c("Entity", "Variable", "None"), length.out = nrow(fakeCompSpec) )  
  fakeCompSpec$Normalization.function <- rep_len( c("Mean", "Max", "None"), length.out = nrow(fakeCompSpec) )  
  
  if( !is.null(exportDirectoryName) ) {
    if( !( is.character(exportDirectoryName) && file.exists(exportDirectoryName) ) ) {
      warning( "The argument exportDirectoryName is expected to be an existing directory.", call. = TRUE )
    }

    write.csv( x = fakeEntityAttributes, file = file.path( exportDirectoryName, "entityAttributes.csv" ) )    
    write.csv( x = fakeEventRecords, file = file.path( exportDirectoryName, "eventRecords.csv" ) )
    write.csv( x = fakeCompSpec, file = file.path( exportDirectoryName, "computationSpecification.csv" ) )
  } 
    
  list( EntityAttributes = fakeEntityAttributes, 
        EventRecords = fakeEventRecords, 
        ComputationSpecification = fakeCompSpec )
  
}

##===========================================================
## Experiments
##===========================================================
if( FALSE ) {
  
  fakeEntityAttributes <- GenereateSimpleTestDataEntityAttributes( numberOfEntities = 1 )
  
  fakeEventRecords <- GenereateSimpleTestDataEventRecords( entityAttributes = fakeEntityAttributes, 
                                                           numberOfVariables = 10, 
                                                           variableFunction = function(x) {runif(n=1)}, 
                                                           timeInterval = 900, 
                                                           numberOfTimeCells = 12,
                                                           randomStartTimesQ = TRUE)

  print( summary( as.data.frame( unclass( fakeEntityAttributes ) ) ) )
  
  print( summary( as.data.frame( unclass( fakeEventRecords ) ) ) )
  
}


if( FALSE ) {

  set.seed(seed = 2132)
  
  ERTMonSimpleTestData( numberOfEntities = 3, numberOfVariables = 12, 
                        timeInterval = 900, numberOfTimeCells = 12, randomStartTimesQ = TRUE, 
                        variableFunction = "Constant", 
                        exportDirectoryName = file.path( ".", "data", "ConstantTestData") )
  
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = file.path( ".", "data", "LinearTestData") )

}

