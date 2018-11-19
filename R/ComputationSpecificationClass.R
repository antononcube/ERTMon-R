##===========================================================
## Event records transformations OOP computation specification class
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
setGeneric("setSpec", function (object, compSpec) standardGeneric("setSpec") )
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

setMethod("setSpec",
          signature = c(object = "ComputationSpecification", compSpec = "data.frame" ), 
          def = function(object, compSpec) {
            object@originalParameters <- compSpec            
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
