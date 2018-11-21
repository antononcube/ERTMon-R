##===========================================================
## Event records transformations OOP data transformer class for categorical matrices
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
# Title: Data transformer for categorical sparse amtices class
# Author: Anton Antonov
# Start date: 2017-11-03
#---

# @include DataTransformerClass.R
#NULL

if( !exists("ertMonDirName") ) {
  ertMonDirName <- "~/ERTMon-R"
}

if( !isClass("DataTransformerClass") ) { 
  source( file.path( ertMonDirName, "R/DataTransformerClass.R" ) )
}

##===========================================================
## DataTransformerCatMatrices class
##===========================================================
setClass( "DataTransformerCatMatrices",
          contains = "DataTransformer"
)

##---------------------------------------------------------
# register conversion function
setAs("DataTransformer", "DataTransformerCatMatrices", 
      function(from, to ){
        new( to, 
             ID = from@ID,
             dataRepresentation = from@dataRepresentation,
             compSpec = from@compSpec,
             entityAttributes = from@entityAttributes,
             eventRecordsForCategoricalMatrices = from@eventRecords,
             transformedData = from@transformedData,
             sparseMatrices = from@sparseMatrices,
             dataMat = from@dataMat,
             groupAggregatedValues = from@groupAggregatedValues,
             progressObject = from@progressObject
        )
      })

##---------------------------------------------------------
## Convert to a list of sparse matrices, impose row IDs, and concatenate to a sparse matrix.
setMethod("makeSparseMatrices",
          signature = c(object = "DataTransformerCatMatrices" ), 
          function(object, breaks = seq(0,1,0.05) ) {
            
            cat("\n\tMake categorical sparse matrices...\n")
            
            object <- callNextMethod( object )
              
            labelMatInd <- grep( "Label", names(object@sparseMatrices) )
            labelMatName <- names(object@sparseMatrices)[[labelMatInd]]
            
            ## Exclude the labels matrix.
            catSMats <- 
              llply( names(object@sparseMatrices)[-labelMatInd], function(nm) { 
                m <- object@sparseMatrices[[nm]]
                qs <- quantile( m@x, breaks )
                m@x <- as.numeric( findInterval( x = m@x, vec = qs, all.inside = T ) )
                m
              } )
            names(catSMats) <- names(object@sparseMatrices)[-labelMatInd]
            
            catSMats <- llply( catSMats, function(x) { ToColumnValueIncidenceMatrix( mat = x, rowNames = TRUE, colNames = TRUE) } )
            
            object@sparseMatrices <- c( catSMats, object@sparseMatrices[[labelMatInd]] )
            names(object@sparseMatrices) <- c( names(catSMats)[-(length(catSMats)+1)], labelMatName )

            object@dataMat <- do.call( cbind, object@sparseMatrices )
            
            cat("\n\t\t...DONE\n")
            
            object
            
          })

