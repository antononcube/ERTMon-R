##===========================================================
## Event records transformations OOP computation specification class
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
##===========================================================

#---
# Title: ComputationSpecification class
# Author: Anton Antonov
# Start date: 2018-10-13
#---

##===========================================================
## Dependencies load
##===========================================================

#' @include DataIngestionFrameworkFunctions.R
NULL

# if( !exists("ertMonDirName") ) {
#   ertMonDirName <- "~/ERTMon-R"
# }
# 
# source( file.path( ertMonDirName, "R/DataIngestionFrameworkFunctions.R" ) )

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
setGeneric("readSpec", function (object, fileName, ...) standardGeneric("readSpec") )
setGeneric("setSpec", function (object, compSpec) standardGeneric("setSpec") )
setGeneric("ingestSpec", function (object, ...) standardGeneric("ingestSpec") )

##===========================================================
## Method implementations
##===========================================================

setMethod("readSpec",
          signature = c(object = "ComputationSpecification", fileName = "character" ), 
          def = function(object, fileName, ...) {
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tRead computation specification...\n") }
            
            if( file.exists(fileName) ) {
              object@originalParameters <- read.csv( file = fileName, stringsAsFactors = FALSE )
            } else {
              stop( "File does not exist.", call. = TRUE )
            }
            
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            object
          }
)

setMethod("setSpec",
          signature = c(object = "ComputationSpecification", compSpec = "data.frame" ), 
          def = function(object, compSpec) {
            object@originalParameters <- compSpec            
            object
          }
)

setMethod("ingestSpec",
          signature = c(object = "ComputationSpecification" ), 
          def = function(object, ...) {
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tProcess computation specification...\n") }

            object@parameters <- ProcessDataSpecification( dataSpec = object@originalParameters, addLabelRowQ = FALSE )
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
                MaxAbs = function(x) max(abs(x),na.rm=T),
                Min = function(x) min(x,na.rm=T), 
                MinAbs = function(x) min(abs(x),na.rm=T), 
                Sum = function(x) sum(x,na.rm=T), 
                SumAbs = function(x) sum(abs(x),na.rm=T), 
                Range = function(x) max(x,na.rm=T) - min(x,na.rm=T),
                AbsRange = function(x) abs(max(x,na.rm=T) - min(x,na.rm=T)),
                SD = function(x) sd(x,na.rm=T), 
                IRQ = function(x) IRQ(x,na.rm=T),
                Count = function(x) length(x),
                First = function(x) x[[1]], 
                Length = dplyr::n,
                Tally = dplyr::tally )
       
            ## Rules for known normalization functions     
            object@normalizationFuncSpecToFunc <- object@aggrFuncSpecToFunc
            
            ## Labels
            object@survivedLabel <- object@parameters[ object@parameters$Variable == "Label","Critical.label"]
            object@diedLabel <- paste0("Non.", object@survivedLabel)
            
            object@labels <- c(object@survivedLabel, object@diedLabel)
              
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            object 
          }
)
