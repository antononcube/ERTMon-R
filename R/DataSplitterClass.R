##===========================================================
## Event records transformations OOP data splitter class
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

#---
# Title: Data Splitter class
# Author: Anton Antonov
# Start date: 2018-10-13
#---
library(Matrix)

##===========================================================
## Data splitter class
##===========================================================
## Application of the Composite design pattern.
## That will be also used as Decorator.

setClass( "DataSplitter",
          
          slots = list( id = "character", 
                        trainInds = "integer",
                        testInds = "integer"),
          
          prototype = list( ID = NA_character_, 
                            trainInds = NA_integer_,
                            testInds = NA_integer_)
)


##-----------------------------------------------------------
## Method signatures
setGeneric("splitIndices", function (object, data, trainFraction, splitMethod, randomSeed, labelColName) standardGeneric("splitIndices") )

## It is interesting to consider default implementations of function methods.


##-----------------------------------------------------------
## Splitting indices for a sparse matrix
setMethod("splitIndices",
          signature = c(object = "DataSplitter", data = "sparseMatrix", trainFraction = "numeric", splitMethod = "character", randomSeed = "ANY", labelColName = "character" ), 
          def = function(object, data, trainFraction = 0.7, splitMethod = "LabelProportional", randomSeed = NULL, labelColName = "Label" ) {
            cat("\n\tDataSplitter::splitIndices for a sparse matrix ...\n")
            
            if( is.integer(randomSeed) ) {
              set.seed(randomSeed) 
              cat("\n\t\tSet random seed to: ", randomSeed, " .\n")
            }
            
            ## Considering performance there is a question how big memory-wise the matrix dataMat is.
            ## Since it is very sparse probably not that big.
            dataMatOne <- data
            dataMatOne@x[ dataMatOne@x > 0 ] <- 1
            
            ## Currently NOT working.
            ## These code has to generalized to arbitrary number of labels. 
            if ( splitMethod == "LabelAndSizeProportional" ) {
              ## Not a faithful approach. To make this the sampling points should be of low descrepancy.
              ## E.g. sort the indices by history length, partition them by 3, pick randomly 2 for training, the rest for testing.
              ordInds <- order( rowSums(dataMatOne) )
              
              ## Equal fraction of CB and Non.CB wrt to history size
              trainInds <- c( sample( which( dataMatOne[ordInds, paste0("Label.",survivedLabel)] == 1 ), round( trainFraction*sum(dataMatOne[, paste0("Label.",survivedLabel)]) ) ),
                              sample( which( dataMatOne[ordInds, paste0("Label.",diedLabel)] == 1 ), round( trainFraction*sum(dataMatOne[, paste0("Label.",diedLabel)]) ) ) )
              trainInds <- ordInds[trainInds]
              testInds <- setdiff( 1:nrow(dataMatOne), trainInds )
              
              
            } else if ( splitMethod == "LabelProportional" ) {
              ## Equal fraction of CB and Non.CB
              trainInds <- c( sample( which( dataMatOne[, paste0("Label.",survivedLabel)] == 1 ), round( trainFraction*sum(dataMatOne[, paste0("Label.",survivedLabel)]) ) ),
                              sample( which( dataMatOne[, paste0("Label.",diedLabel)] == 1 ), round( trainFraction*sum(dataMatOne[, paste0("Label.",diedLabel)]) ) ) )
              testInds <- setdiff( 1:nrow(dataMatOne), trainInds )
              
            } else {
              ## Direct, naive
              trainInds <- sample( 1:nrow(dataMatOne), round(0.7*nrow(dataMatOne)))
              testInds <- setdiff( 1:nrow(dataMatOne), trainInds )
              
            }
            
            object@trainInds <- trainInds
            object@testInds <- testInds
            
            cat("\n\t\t...DONE\n")
            
            object 
          }
)

##-----------------------------------------------------------
## Splitting indices for a data frame
setMethod("splitIndices",
          signature = c(object = "DataSplitter", data = "data.frame", trainFraction = "numeric", splitMethod = "character", randomSeed = "ANY", labelColName = "character" ), 
          def = function(object, data, trainFraction = 0.7, splitMethod = "LabelProportional", randomSeed = NULL, labelColName = "character" ) {
            cat("\n\tDataSplitter::splitIndices for a data frame ...\n")
            
            if( is.integer(randomSeed) ) {
              set.seed(randomSeed) 
              cat("\n\t\tSet random seed to: ", randomSeed, " .\n")
            }
            
            ## Direct, naive
            trainInds <- sample( 1:nrow(data), round(trainFraction*nrow(data)))
            testInds <- setdiff( 1:nrow(data), trainInds )
            
            object@trainInds <- trainInds
            object@testInds <- testInds
            
            cat("\n\t\t...DONE\n")
            
            object
          }
)
