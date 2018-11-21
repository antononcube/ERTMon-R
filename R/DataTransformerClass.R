##===========================================================
## Event records transformations OOP transformation class
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
# Title: Data transformer class
# Author: Anton Antonov
# Start date: 2018-10-07
#---

##===========================================================
## DataTransformer class hierarchy
##===========================================================

## General class behaving as Template Method.
## The sub-classes might use different representations of the data.
## (But perform the same operations.)
##
##
## Since S4 is used the objects are not handled by reference (when copied and worked on.)
## Having a dataObject can be / might be too slow. It might be a good idea to move to S6.
## 
## Note that 
##   1. the entity data is not transformed, and
##   2. the event records data is transformed.
##
## For performance and explicitness of pipeline construction the event records data
## is an argument to the transformation opeartion functions.

# A data transformer class.
# @slot ID ID of the object.
setClass( "DataTransformer",
          slots = list( ID = "character",
                        dataRepresentation = "character",
                        eventRecords = "data.frame",
                        entityAttributes = "data.frame",
                        compSpec = "ComputationSpecification",
                        # dataObject = "DataWrapper",
                        eventRecordsForCategoricalMatrices = "data.frame",
                        transformedData = "data.frame",
                        sparseMatrices = "vector",
                        dataMat = "sparseMatrix",
                        groupAggregatedValues = "ANY",
                        outlierBoundaries = "ANY",
                        progressObject = "ANY" ),
          
          prototype = list( ID = NA_character_, 
                            dataRepresentation = NA_character_,
                            compSpec = NULL,
                            # dataObject = NULL,
                            entityAttributes = NULL,
                            eventRecordsForCategoricalMatrices = NULL,
                            transformedData = NULL,
                            sparseMatrices = NULL,
                            dataMat = NULL,
                            groupAggregatedValues = NULL,
                            outlierBoundaries = NULL,
                            progressObject = NULL )
) 

##-----------------------------------------------------------
## Method signatures

## Template Method Abstract algorithm
setGeneric("transformData", function (object, compSpec, eventRecordsData, entityAttributes, ...) standardGeneric("transformData") )

## Operation steps
setGeneric("restrictToSpecifiedVariables", function (eventRecords, object) standardGeneric("restrictToSpecifiedVariables") )
setGeneric("addTimeGrid", function (eventRecords, object) standardGeneric("addTimeGrid") )
setGeneric("aggregateOverTimeGrid", function (eventRecords, object) standardGeneric("aggregateOverTimeGrid") )
setGeneric("aggregateAndAccumulateOverGroups", function (object, aggrMRData) standardGeneric("aggregateAndAccumulateOverGroups") )
setGeneric("normalize", function (aggrMRData, object) standardGeneric("normalize") )
setGeneric("makeSparseMatrices", function (object, ...) standardGeneric("makeSparseMatrices") )
setGeneric("sparseMatricesToDataFrame", function (object, simpleConversion, numericLabelColumn) standardGeneric("sparseMatricesToDataFrame") )

## Internal / protected
setGeneric("normalizeGroupsBySpec", function (specRow, object, matLongFormData, entityAttributes, normalizationFuncSpecToFunc) standardGeneric("normalizeGroupsBySpec") )

##-----------------------------------------------------------
## Template Method operation definition
setMethod("transformData",
          signature = c(object = "DataTransformer", compSpec = "ComputationSpecification", eventRecordsData = "data.frame", entityAttributes = "data.frame"), 
          function(object, compSpec, eventRecordsData, entityAttributes, ...) {
            
            additionalArgs <- list(...)
            testDataRun <- FALSE
            outlierIdentifierParametersFunc <- QuartileIdentifierParameters
            
            if( "testDataRun" %in% names(additionalArgs) ) { 
              testDataRun <- additionalArgs[["testDataRun"]] 
            }
            
            if( "outlierIdentifierParameters" %in% names(additionalArgs) ) { 
              outlierIdentifierParametersFunc <- additionalArgs[["outlierIdentifierParameters"]] 
            } 
            
            ## Set data fields
            object@compSpec <- compSpec
            object@entityAttributes <- entityAttributes
            
            ## Find outliers
            outCompSpec <- compSpec@parameters[ compSpec@parameters$Aggregation.function %in% c(  "OutCnt", "OutFrc" ),  ]
            if( !testDataRun && nrow(outCompSpec) > 0 ) {
              object@outlierBoundaries <-
                ddply( outCompSpec, "Variable", function(x) {
                  obs <-
                    eventRecordsData %>% 
                    dplyr::filter( Variable == x$Variable[[1]] )
                  obs <- outlierIdentifierParametersFunc( obs$Value )
                  data.frame( Variable = x$Variable[[1]], Lower = obs[[1]], Upper = obs[[2]], stringsAsFactors = F )
                })
            }
            
            ## Main transformation computation
            eventRecordsData <- 
              eventRecordsData %>%
              restrictToSpecifiedVariables(object) %>%
              addTimeGrid(object) %>%
              aggregateOverTimeGrid(object)
            
            ## This is useful for making categorical matrices
            object@eventRecordsForCategoricalMatrices <- eventRecordsData
            
            ## Find aggregate values over groups of records 
            if( !testDataRun ) {
              object <- aggregateAndAccumulateOverGroups(object, eventRecordsData )
            }
            
            ## Normalize
            object@transformedData <- eventRecordsData %>% normalize(object)
            
            ## Create a sparse matrices
            ## Note that this breaks the style of the transformation pipeline.
            object <- makeSparseMatrices( object )
            
            object
          }
)


##---------------------------------------------------------
## Restrict event records
setMethod("restrictToSpecifiedVariables",
          signature = c(eventRecords = "data.frame", object = "DataTransformer"), 
          function(eventRecords, object) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Restrict event records." ) }
            
            cat("\n\tRestrict event records to specificed variables...\n")
            
            eventRecords <- dplyr::filter( eventRecords, Variable %in% object@compSpec@parameters$Variable )
            
            cat("\n\t\t...DONE\n")
            
            eventRecords
          }
)

##---------------------------------------------------------
setMethod("addTimeGrid",
          signature = c(eventRecords = "data.frame", object = "DataTransformer"), 
          function(eventRecords, object) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Find most recent observation date for each entity." ) }
            
            cat("\n\tFind most recent observation date for each entity...\n")
            
            eventRecords <- 
              dplyr::left_join(
                dplyr::filter( dplyr::summarise( dplyr::group_by( dplyr::select( eventRecords, EntityID, ObservationTime ), EntityID ), 
                                                 MostRecentTimeEpoch = max(ObservationTime, na.rm = T) ), 
                               !is.na(MostRecentTimeEpoch) ),
                eventRecords, by = "EntityID")
            
            cat("\n\t\t...DONE\n")
            
            cat("\n\tCompute differences with max time...\n")
            
            eventRecords <- dplyr::mutate( eventRecords, 
                                           DiffToMaxObsTime = MostRecentTimeEpoch - ObservationTime )
            
            cat("\n\t\t...DONE\n")
            
            cat("\n\tRestrict event records to specfied maximal history length...\n")
            
            eventRecords <- dplyr::mutate( eventRecords, DiffToMaxObsTime = as.numeric( DiffToMaxObsTime ) )
            eventRecords <- dplyr::filter( eventRecords, DiffToMaxObsTime <= object@compSpec@variableToMaxHistoryLength[Variable] )
            ## In order to make this work with Spark innter join has to be used (dplyr or SQL).
            
            cat("\n\t\t...DONE\n")
            
            cat("\n\tFor each of the specified variables find time grid intervals...\n")
            
            eventRecords <- dplyr::mutate( eventRecords, TimeGridCell = floor( DiffToMaxObsTime / object@compSpec@variableToAggregationIntervalLength[Variable] ) )
            eventRecords <- dplyr::mutate( eventRecords, TimeGridCell = TimeGridCell + 1 )
            
            cat("\n\t\t...DONE\n")
            
            eventRecords
          }
)


##---------------------------------------------------------
setMethod("aggregateOverTimeGrid",
          signature = c(eventRecords = "data.frame", object = "DataTransformer"), 
          function(eventRecords, object) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Find aggregated values." ) }
            
            ##---------------------------------------------------------
            ## For each of the specified variables, 
            ##   for each entity, 
            ##     for each time grid cell 
            ##       find aggregate function values
            cat("\n\tFind aggregated values...\n")
            
            aggrMRData <-
              object@compSpec@parameters %>%
              rowwise() %>%
              do( AggregateEventRecordsBySpec( .,
                                               eventRecords,
                                               object@entityAttributes,
                                               object@compSpec@aggrFuncSpecToFunc,
                                               object@outlierBoundaries ) ) %>%
              ungroup()
            
            ## If we do not ungroup we will get
            ##   Warning message:
            ##   Grouping rowwise data frame strips rowwise nature 
            
            ## This was considered because 
            ## AggregateEventRecordsBySpec might modify the specification rows of object@compSpec@parameters
            ## for outlier handling.
            # aggrMRData <-
            #   ldply( 1:length(object@compSpec@parameters), function(i) {
            #     res <- 
            #       AggregateEventRecordsBySpec( object@compSpec@parameters[i,], eventRecords, object@entityAttributes, object@compSpec@aggrFuncSpecToFunc ) 
            #     object@compSpec@parameters[i,] <- res$SpecRow
            #     res$Result
            #   })
            
            cat("\n\t\t...DONE\n")
            
            aggrMRData
          }
)


##---------------------------------------------------------
setMethod("normalize",
          signature = c(aggrMRData = "data.frame", object = "DataTransformer"), 
          function(aggrMRData, object) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Normalization functions application." ) }
            
            cat("\n\tNormalization functions application...\n")
            
            ## So basically we do the same thing we did for the aggreation: rowwise() %>% do( ... ) .
            
            aggrMRData <- 
              object@compSpec@parameters %>% 
              rowwise() %>% 
              do( normalizeGroupsBySpec( ., 
                                         object,
                                         aggrMRData, 
                                         object@entityAttributes, 
                                         object@compSpec@normalizationFuncSpecToFunc ) ) %>% 
              ungroup()
            
            cat("\n\t\t...DONE\n")
            
            aggrMRData
          }
)

##---------------------------------------------------------
## Convert to a list of sparse matrices, impose row IDs, and concatenate to a sparse matrix.
setMethod("makeSparseMatrices",
          signature = c(object = "DataTransformer"), 
          function(object, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Sparse matrix." ) }
            
            cat("\n\tMake sparse matrices...\n")
            
            labelMat <- 
              xtabs( ~ EntityID + Value, 
                     object@entityAttributes %>% dplyr::filter( Attribute == "Label" ), 
                     sparse = T)
            colnames(labelMat) <- paste( "Label", colnames(labelMat), sep="." )
            labelMat@x[labelMat@x > 1 ] <- 1
            
            smats <- 
              dlply( object@transformedData, c("MatrixName"), function(x) { 
                
                ## I am not sure this here is the best way to handle this.
                if( sum( is.nan(x$AValue) ) ) {
                  warning( paste0( "NaN values for the matrix", x$MatrixName[[1]], ". Attempting to continue by replacing each NaN with 0." ), call. = T )
                  x$AValue[ is.nan(x$AValue) ] <- 0
                }
                
                res <- xtabs( AValue ~ EntityID + VarID, x, sparse = T) 
                colnames(res) <- paste( gsub(".","-",x$MatrixName[[1]],fixed = T), colnames(res), sep="-")
                res
              }) 
            
            allRowIDs <- unique( unlist( llply( smats, rownames) ) )
            
            smats <- llply( smats, function(x) ImposeRowIDs( rowIDs = allRowIDs, smat = x ) )
            smats <- c( smats, Label = ImposeRowIDs( rowIDs = allRowIDs, smat = labelMat ) )
            
            ## Sort the columns of the sparse matrices
            smatNames <- names(smats)
            smats <- 
              llply( smats, function(sm) {
                if( sum( grepl( "\\.", colnames(sm)[[1]] ) ) == 0 ) { sm } 
                else {
                  ## Suppress warning messages while converting column names to corresponding values
                  oldw <- getOption("warn"); options(warn = -1)
                  colVals <- as.numeric( laply( strsplit( colnames(sm), "\\." ), function(x) x[[length(x)]] ) )
                  options(warn = oldw)
                  
                  if ( sum( is.na( colVals ) ) > 0 || sort(colVals) != (1:ncol(sm)) ) { sm }
                  else {
                    colOrder <- order( colVals  )
                    sm[, colOrder, drop = F]
                  }
                }
              })
            names(smats) <- smatNames
            
            ## Final result
            object@sparseMatrices <- smats
            object@dataMat <- do.call( cbind, smats )
            
            cat("\n\t\t...DONE\n")
            
            object
          }
)

##---------------------------------------------------------
## Adapter: a data frame.      
setMethod("sparseMatricesToDataFrame",
          signature = c(object = "DataTransformer", simpleConversion = "logical", numericLabelColumn = "logical" ), 
          function(object, simpleConversion, numericLabelColumn) {
            
            ## Shortcuts
            diedLabel <- object@compSpec@diedLabel
            survivedLabel <- object@compSpec@survivedLabel
            dataMat <- object@dataMat 
            
            if ( simpleConversion ) { 
              ## Too simple, no NA handling.
              
              vsDF <- as.data.frame( as.matrix( dataMat[, -grep( "Label", colnames(dataMat) ) ] ), stringsAsFactors = T )
              if( !numericLabelColumn ) {
                vsDF <- cbind( vsDF, Label = ifelse( dataMat[,paste0("Label.",diedLabel)], diedLabel, survivedLabel ), stringsAsFactors = T )  
              } else { 
                vsDF <- cbind( vsDF, Label = dataMat[,paste0("Label.",diedLabel)], stringsAsFactors = T ) 
              }
              
            } else {  
              
              ## Proper handling of NA's.
              
              vsMat <- matrix(NA, nrow = nrow(dataMat), ncol = ncol(dataMat) )
              dataMatDF <- as.data.frame( summary(dataMat) )
              vsMat[ as.matrix(dataMatDF[,1:2]) ] <- dataMatDF[,3]
              rownames(vsMat) <- rownames(dataMat); colnames(vsMat) <- colnames(dataMat)
              vsDF <- as.data.frame( vsMat[, -grep( "Label", colnames(vsMat) ) ] )
              lblCol <- ifelse( vsMat[,paste0("Label.",diedLabel)], diedLabel, survivedLabel )
              lblCol[is.na(lblCol)] <- survivedLabel
              vsDF <- cbind( vsDF, Label = lblCol  )
            }
            
            ## This code is here is convenience.
            ## The same code (transformation) is also in EntityClassifierH2OGLM.
            ## Note the assumption that the last column is the `Label` column. (See above.)
            if ( numericLabelColumn ) {
              
              vsDF <- cbind( vsDF[,-ncol(vsDF)], Label = ifelse( dataMat[ , paste0("Label.", diedLabel) ], 1, 0 ) )
              
            }
            
            vsDF          
          }
)

##---------------------------------------------------------
## Related to the normalization step for the method "normalize". This is an intermediate step.
#' @description Applies a normalization function to long form contingency matrix data and stores the results.
#' @param object the current object
#' @param aggrMRData aggregated event records in long form
setMethod("aggregateAndAccumulateOverGroups",
          signature = c(object = "DataTransformer", aggrMRData = "data.frame" ), 
          function(object, aggrMRData) {
            
            cat("\n\tAggregation over groups functions application (and accumulation)...\n")
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Aggregate and accumulate over groups of records." ) }
            
            allEntityAttributes <- unique(object@entityAttributes$Attribute)
            
            object@groupAggregatedValues <-
              ddply( object@compSpec@parameters, "MatrixName", function(specRow) { 
                
                func <- object@compSpec@aggrFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]
                
                if ( specRow$Normalization.scope[[1]] == "Variable" ) {
                  
                  dfNormalizationValues <- 
                    aggrMRData %>% 
                    dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                    dplyr::summarise( NormalizationValue = func(AValue) ) %>% 
                    dplyr::mutate( Scope = "Variable", Attribute = NA ) %>% 
                    dplyr::ungroup()
                  
                  if( nrow(dfNormalizationValues) == 0 ) { 
                    NULL 
                  } else { 
                    #print( colnames(object@groupAggregatedValues) )
                    #print( cbind( MatrixName = specRow$MatrixName, dfNormalizationValues, stringsAsFactors = F ) )
                    rbind( object@groupAggregatedValues, 
                           cbind( MatrixName = specRow$MatrixName, dfNormalizationValues, stringsAsFactors = F ) ) 
                  }
                  
                } else if ( specRow$Normalization.scope[[1]] %in% allEntityAttributes ) {
                  
                  dfNormalizationValues <- 
                    aggrMRData %>% 
                    dplyr::filter( MatrixName == specRow$MatrixName ) %>%
                    dplyr::inner_join( object@entityAttributes[, c("EntityID", "Attribute")] %>% 
                                         dplyr::filter( Attribute == specRow$Normalization.scope[[1]] ),
                                       by = "EntityID" ) %>% 
                    dplyr::group_by( Attribute ) %>% 
                    dplyr::summarise( NormalizationValue = func(AValue) ) %>% 
                    dplyr::ungroup()
                  
                  if( nrow(dfNormalizationValues) == 0 ) { NULL }
                  else { rbind( object@groupAggregatedValues, 
                                cbind( MatrixName = specRow$MatrixName, 
                                       Scope = specRow$Normalization.scope[[1]], 
                                       dfNormalizationValues, 
                                       stringsAsFactors = F ) ) }
                  
                } else {
                  NULL
                }
                
              })
            
            cat("\n\t\t...DONE\n")
            
            object
            
          })

##---------------------------------------------------------
## Normalization step for the method "normalize".
#' @description Applies a normalization function to long form contingency matrix data.
#' @param specRow a speficication that has columns "MatrixName" and "Normalization"
#' @param object the current object; it is the second argument in order to use %>%
#' @param matLongFormData aggregated event records in long form
#' @param entityAttributes entity specifica data
#' @param normalizationFuncSpecToFunc a named elements list of normalization functions
setMethod("normalizeGroupsBySpec",
          signature = c(specRow = "list", object = "DataTransformer",  matLongFormData = "data.frame", entityAttributes = "data.frame", normalizationFuncSpecToFunc = "list"), 
          function(specRow, object, matLongFormData, entityAttributes, normalizationFuncSpecToFunc ) {
            
            if ( !( specRow$Normalization.scope[[1]] %in% c( "None", "NULL", "Entity" ) ) ) {
              
              if( is.null(object@groupAggregatedValues) ) {
                stop("Missing accumulated averages data.", call. = TRUE )  
              }
              
              if ( ! ( specRow$Normalization.function[[1]] %in% c( names(normalizationFuncSpecToFunc), c( "Null", "NULL", "None") ) ) ) {
                stop("Uknown normalization function.", call. = TRUE )
              }
              
              dfNormalizationValues <- 
                object@groupAggregatedValues %>%
                dplyr::filter( MatrixName == specRow$MatrixName )
              
              if( is.null(dfNormalizationValues) || nrow(dfNormalizationValues) == 0 ) {
                
                warning( paste( "Missing accumulated group normalization values data (after filtering.) for MatrixName=", specRow$MatrixName, "." ), call. = TRUE )
                
                matLongFormData %>% dplyr::filter( MatrixName == specRow$MatrixName )
                
              } else if ( specRow$Normalization.scope[[1]] == "Variable" ) {
                
                matLongFormData %>% 
                  dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                  dplyr::inner_join( dfNormalizationValues, by = "MatrixName" ) %>%
                  dplyr::group_by( EntityID ) %>% 
                  dplyr::mutate( AValue = AValue / NormalizationValue ) %>% 
                  dplyr::select( EntityID, TimeGridCell, VarID, AValue, MatrixName )
                
              } else {
                
                matLongFormData %>% 
                  dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                  dplyr::inner_join( dfNormalizationValues, by = "MatrixName" ) %>%
                  dplyr::group_by( EntityID ) %>% 
                  dplyr::mutate( AValue = AValue / NormalizationValue ) %>% 
                  dplyr::select( EntityID, TimeGridCell, VarID, AValue, MatrixName )
              }
              
            } else if ( specRow$Normalization.scope[[1]] == "Entity" &&
                        specRow$Normalization.function[[1]] %in% names(normalizationFuncSpecToFunc) ) {
              
              # cat( "in:", specRow$Normalization.function[[1]], "\n" )
              func <- normalizationFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]
              matLongFormData %>% 
                dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                group_by( EntityID ) %>% 
                dplyr::mutate( AValue = AValue / func(AValue) ) %>% 
                ungroup()
              
            } else {
              
              matLongFormData %>% 
                dplyr::filter( MatrixName == specRow$MatrixName )
              
            }
          }
)
