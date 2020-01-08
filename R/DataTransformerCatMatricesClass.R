##===========================================================
## Event records transformations OOP data transformer class for categorical matrices
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
##===========================================================

#---
# Title: Data transformer for categorical sparse amtices class
# Author: Anton Antonov
# Start date: 2017-11-03
#---

#' @include DataTransformerClass.R
NULL

# if( !exists("ertMonDirName") ) {
#   ertMonDirName <- "~/ERTMon-R"
# }
# 
# if( !isClass("DataTransformerClass") ) { 
#   source( file.path( ertMonDirName, "R/DataTransformerClass.R" ) )
# }

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
            
            ## Find is the calculation of a label feature matrix specified?
            findLabelMatQ <- HasLabelRowQ( object@compSpec@parameters )
            
            if ( findLabelMatQ ) {
              
              labelMatInd <- grep( "Label", names(object@sparseMatrices) )
              labelMatName <- names(object@sparseMatrices)[[labelMatInd]]
              
              ## Exclude the labels matrix.
              catSMats <- 
                purrr::map( 
                  names(object@sparseMatrices)[-labelMatInd], 
                  function(nm) { 
                    m <- object@sparseMatrices[[nm]]
                    qs <- quantile( m@x, breaks )
                    m@x <- as.numeric( findInterval( x = m@x, vec = qs, all.inside = T ) )
                    m
                  } )
              names(catSMats) <- names(object@sparseMatrices)[-labelMatInd]
              
              catSMats <- purrr::map( catSMats, function(x) { ToColumnValueIncidenceMatrix( mat = x, rowNames = TRUE, colNames = TRUE) } )
              
              object@sparseMatrices <- c( catSMats, object@sparseMatrices[[labelMatInd]] )
              names(object@sparseMatrices) <- c( names(catSMats)[-(length(catSMats)+1)], labelMatName )
              
            } else {
              
              ## Just duplication of the code above without label columns.
              ## I think it is easier to read the code that way.
              
              catSMats <- 
                purrr::map( 
                  names(object@sparseMatrices), 
                  function(nm) { 
                    m <- object@sparseMatrices[[nm]]
                    qs <- quantile( m@x, breaks )
                    m@x <- as.numeric( findInterval( x = m@x, vec = qs, all.inside = T ) )
                    m
                  } )
              names(catSMats) <- names(object@sparseMatrices)
              
              catSMats <- purrr::map( catSMats, function(x) { ToColumnValueIncidenceMatrix( mat = x, rowNames = TRUE, colNames = TRUE) } )
              
              object@sparseMatrices <- c( catSMats, object@sparseMatrices )
              names(object@sparseMatrices) <- names(catSMats)
            }
            
            object@dataMat <- do.call( cbind, object@sparseMatrices )
            
            cat("\n\t\t...DONE\n")
            
            object
            
          })

