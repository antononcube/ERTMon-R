##===========================================================
## Event records transformations OOP computation specification class
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
# Title: ComputationSpecification class
# Author: Anton Antonov
# Start date: 2018-10-13
#---

##===========================================================
## Dependencies load
##===========================================================

if( !exists("ertMonDirName") ) {
  ertMonDirName <- "~/ERTMon-R"
}

source( file.path( ertMonDirName, "R/DataIngestionFrameworkFunctions.R" ) )

##===========================================================
## ComputationSpecification class / singleton 
##===========================================================

setClass( "ComputationSpecification",
          
          slots = list( ID = "character", 
                        originalParameters = "data.frame",
                        parameters = "data.frame",
                        variableToAggregationIntervalLength = "vector",
                        variableToMaxHistoryLength = "vector",
                        aggrFuncSpecToFunc = "vector",
                        normalizationFuncSpecToFunc = "vector",
                        imposeRegularTimeGridsQ = "logical",
                        labels = "character",
                        diedLabel ="character",
                        survivedLabel = "character"),
          
          prototype = list( ID = NA_character_,
                            originalParameters = NULL,
                            parameters = NULL,
                            variableToAggregationIntervalLength = NULL,
                            variableToMaxHistoryLength = NULL,
                            aggrFuncSpecToFunc = NULL,
                            normalizationFuncSpecToFunc = NULL,
                            imposeRegularTimeGridsQ = FALSE,
                            labels = NULL,
                            diedLabel = "Died",
                            survivedLabel = "Survived")
)


##-----------------------------------------------------------
## Method signatures
setGeneric("readSpec", function (object, fileName) standardGeneric("readSpec") )
setGeneric("ingestSpec", function (object) standardGeneric("ingestSpec") )

##===========================================================
## Method implementations
##===========================================================

setMethod("readSpec",
          signature = c(object = "ComputationSpecification", fileName = "character" ), 
          def = function(object, fileName) {
            cat("\n\tRead specifications\n")
            
            if( file.exists(fileName) ) {
              object@originalParameters <- read.csv( file = fileName, stringsAsFactors = FALSE )
            } else {
              stop( "File does not exist.", call. = TRUE )
            }
            
            cat("\n\t\t...DONE\n")
            
            object
          }
)

setMethod("ingestSpec",
          signature = c(object = "ComputationSpecification" ), 
          def = function(object) {
            cat("\n\tProcess data specifications\n")

            object@parameters <- ProcessDataSpecification( dataSpec = object@originalParameters )
            # dataSpecDF$Aggregation.interval.length = 5*60;
            
            ## View(object@parameters)
            
            ## Rules of variable to time related values
            object@variableToAggregationIntervalLength<- setNames( object@parameters$Aggregation.interval.length, object@parameters$Variable )
            object@variableToMaxHistoryLength <- setNames( object@parameters$Max.history.length, object@parameters$Variable )
            
            ## Rules for known aggregation functions
            object@aggrFuncSpecToFunc <- 
              c( 
                Identity = function(x) x,
                None = function(x) x,
                Mean = function(x) mean(x,na.rm=T), 
                Median = function(x) median(x,na.rm=T),
                Max = function(x) max(x,na.rm=T), 
                Sum = function(x) sum(x,na.rm=T), 
                Range = function(x) max(x) - min(x),
                SD = function(x) sd(x,na.rm=T), 
                IRQ = function(x) IRQ(x,na.rm=T),
                Count = function(x) length(x),
                First = function(x) x[[1]], 
                Length = n,
                Tally = tally )
       
            ## Rules for known normalization functions     
            object@normalizationFuncSpecToFunc <- object@aggrFuncSpecToFunc
              
            ## Labels
            object@survivedLabel <- object@parameters[ object@parameters$Variable == "Label","Critical.label"]
            object@diedLabel <- paste0("Non.", object@survivedLabel)
            
            object@labels <- c(object@survivedLabel, object@diedLabel)
              
            cat("\n\t\t...DONE\n")
            
            object 
          }
)
