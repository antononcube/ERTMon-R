##===========================================================
## Event records transformations OOP data transformer class for categorical matrices
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
# Title: Data transformer for categorical sparse amtices class
# Author: Anton Antonov
# Start date: 2017-11-03
#---

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
             patientData = from@patientData,
             medicalRecordsDataForCategoricalMatrices = from@medicalRecordsDataForCategoricalMatrices,
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

