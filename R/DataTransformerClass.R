##===========================================================
## Event records transformations OOP transformation class
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
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
                        timeCellsInterpretation = "data.frame",
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
                            timeCellsInterpretation = NULL,
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
setGeneric("restrictToSpecifiedVariables", function (eventRecords, object, ...) standardGeneric("restrictToSpecifiedVariables") )
setGeneric("addTimeGrid", function (eventRecords, object) standardGeneric("addTimeGrid") )
setGeneric("aggregateOverTimeGrid", function (eventRecords, object, ...) standardGeneric("aggregateOverTimeGrid") )
setGeneric("aggregateAndAccumulateOverGroups", function (object, aggrERData, ...) standardGeneric("aggregateAndAccumulateOverGroups") )
setGeneric("normalize", function (aggrERData, object, ...) standardGeneric("normalize") )
setGeneric("makeSparseMatrices", function (object, ...) standardGeneric("makeSparseMatrices") )
setGeneric("sparseMatricesToDataFrame", function (object, simpleConversion, numericLabelColumn) standardGeneric("sparseMatricesToDataFrame") )

## Internal / protected
setGeneric("normalizeGroupsBySpec", function (specRow, object, matLongFormData, entityAttributes, normalizationFuncSpecToFunc) standardGeneric("normalizeGroupsBySpec") )

##-----------------------------------------------------------
## Template Method operation definition
setMethod("transformData",
          signature = c(object = "DataTransformer", compSpec = "ComputationSpecification", eventRecordsData = "data.frame", entityAttributes = "data.frame"), 
          function(object, compSpec, eventRecordsData, entityAttributes, ...) {
            
            ## Processing additional arguments
            additionalArgs <- list(...)
            testDataRun <- FALSE
            outlierIdentifierParametersFunc <- QuartileIdentifierParameters
            alignmentSpec <- "MaxTime"
            echoStepsQ <- FALSE
            
            if( "testDataRun" %in% names(additionalArgs) ) { 
              testDataRun <- additionalArgs[["testDataRun"]] 
            }
            
            if( "outlierIdentifierParameters" %in% names(additionalArgs) ) { 
              outlierIdentifierParametersFunc <- additionalArgs[["outlierIdentifierParameters"]] 
            } 
            
            if( "alignmentSpec" %in% names(additionalArgs) ) { 
              alignmentSpec <- additionalArgs[["alignmentSpec"]] 
            }
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            
            ## Set data fields
            object@compSpec <- compSpec
            object@entityAttributes <- entityAttributes
            
            ## Find outliers
            outCompSpec <- compSpec@parameters[ compSpec@parameters$Aggregation.function %in% c(  "OutCnt", "OutFrc" ),  ]
            if( !testDataRun && nrow(outCompSpec) > 0 ) {
              object@outlierBoundaries <-
                purrr::map_dfr( split(outCompSpec, outCompSpec$Variable), function(x) {
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
              restrictToSpecifiedVariables( object, echoStepsQ = echoStepsQ ) %>%
              aggregateOverTimeGrid( object, alignmentSpec = alignmentSpec, echoStepsQ = echoStepsQ )

            ## Time cells interpretations
            if( tolower(alignmentSpec) %in% c("maxtime", "max") ) { tsSign = -1 } else { tsSign = 1 }
            
            object@timeCellsInterpretation <-
              eventRecordsData %>% 
              dplyr::select( MatrixName, TimeGridCell ) %>% 
              dplyr::distinct() %>% 
              dplyr::inner_join( compSpec@parameters, by = "MatrixName" )
            
            if( tsSign > 0 ) {
              
              object@timeCellsInterpretation <-
                object@timeCellsInterpretation %>% 
                dplyr::mutate( StartTime = tsSign * TimeGridCell * Aggregation.interval.length ) %>% 
                dplyr::mutate( EndTime = tsSign * (TimeGridCell + 1) * Aggregation.interval.length )
              
            } else {
              
              object@timeCellsInterpretation <-
                object@timeCellsInterpretation %>% 
                dplyr::mutate( StartTime = tsSign * (TimeGridCell + 1) * Aggregation.interval.length ) %>% 
                dplyr::mutate( EndTime = tsSign * TimeGridCell * Aggregation.interval.length )
              
            }
            
            object@timeCellsInterpretation <-
              object@timeCellsInterpretation %>% 
              dplyr::select( MatrixName, TimeGridCell, StartTime, EndTime )
            
            ## Note that we are not saving the transformed event records in the slot "eventRecords".
            ## That slot is reserved for the initial event records data.
            ## We use the slots transformedData and eventRecordsForCategoricalMatrices.
            ## I.e. do not do this:
            ## object@eventRecords <- eventRecordsData
            
            ## This is useful for making categorical matrices
            object@eventRecordsForCategoricalMatrices <- eventRecordsData
            
            ## Find aggregate values over groups of records 
            if( !testDataRun ) {
              object <- aggregateAndAccumulateOverGroups(object, eventRecordsData, echoStepsQ = echoStepsQ )
            }

            ## Normalize
            object@transformedData <- eventRecordsData %>% normalize(object, echoStepsQ = echoStepsQ)
            
            ## Create a sparse matrices
            ## Note that this breaks the style of the transformation pipeline.
            object <- makeSparseMatrices( object, echoStepsQ = echoStepsQ )
            
            object
          }
)


##---------------------------------------------------------
## Restrict event records
setMethod("restrictToSpecifiedVariables",
          signature = c(eventRecords = "data.frame", object = "DataTransformer"), 
          function(eventRecords, object, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Restrict event records." ) }
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tRestrict event records to specificed variables...\n") }

            eventRecords <- dplyr::filter( eventRecords, Variable %in% object@compSpec@parameters$Variable )
          
            if( is.null(eventRecords) || nrow(eventRecords) == 0 ) {
              
              if( sum( unique(eventRecords$Variable) %in% object@compSpec@parameters$Variable ) == 0 ) {
                stop("None of the computation specification variables are found in the event records.", call. = TRUE )
              }
                
              stop("Obtained empty event records after variable restriction.", call. = TRUE )
            }
              
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            eventRecords
          }
)

##---------------------------------------------------------
# Should not be used since it is based on Variable-only granularity.
# I.e. it does not do proper processing of the computation specifications.
setMethod("addTimeGrid",
          signature = c(eventRecords = "data.frame", object = "DataTransformer"), 
          function(eventRecords, object) {
            
            ## NOTE THIS !
            assertthat::assert_that(FALSE)
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Find most recent observation date for each entity." ) }
            
            cat("\n\tFind most recent observation date for each entity...\n")
            
            eventRecords <- 
              dplyr::left_join(
                dplyr::filter( dplyr::summarise( dplyr::group_by( dplyr::select( eventRecords, EntityID, ObservationTime ), EntityID ), 
                                                 MostRecentTimeEpoch = max(ObservationTime, na.rm = T), .groups = "drop" ), 
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
          function(eventRecords, object, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Find aggregated values." ) }
            
            additionalArgs <- list(...)
            alignmentSpec <- "MaxTime"
            echoStepsQ <- TRUE
            
            if( "alignmentSpec" %in% names(additionalArgs) ) { 
              alignmentSpec <- additionalArgs[["alignmentSpec"]] 
            }
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            ##---------------------------------------------------------
            ## For each of the specified variables, 
            ##   for each entity, 
            ##     for each time grid cell 
            ##       find aggregate function values
            if( echoStepsQ ) { cat("\n\tFind aggregated values...\n") }

            aggrERData <-
              object@compSpec@parameters %>%
              rowwise() %>%
              do( AggregateEventRecordsBySpec( .,
                                               eventRecords,
                                               object@entityAttributes,
                                               object@compSpec@aggrFuncSpecToFunc,
                                               object@outlierBoundaries, 
                                               alignmentSpec = alignmentSpec,
                                               echoStepsQ = echoStepsQ ) ) %>%
              ungroup()
            
            ## If we do not ungroup we will get
            ##   Warning message:
            ##   Grouping rowwise data frame strips rowwise nature 
            
            ## This was considered because 
            ## AggregateEventRecordsBySpec might modify the specification rows of object@compSpec@parameters
            ## for outlier handling.
            # aggrERData <-
            #   ldply( 1:length(object@compSpec@parameters), function(i) {
            #     res <- 
            #       AggregateEventRecordsBySpec( object@compSpec@parameters[i,], eventRecords, object@entityAttributes, object@compSpec@aggrFuncSpecToFunc ) 
            #     object@compSpec@parameters[i,] <- res$SpecRow
            #     res$Result
            #   })
            
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            aggrERData
          }
)


##---------------------------------------------------------
setMethod("normalize",
          signature = c(aggrERData = "data.frame", object = "DataTransformer"), 
          function(aggrERData, object, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Normalization functions application." ) }
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tNormalization functions application...\n") }
            
            ## So basically we do the same thing we did for the aggreation: rowwise() %>% do( ... ) .
            
            aggrERData <- 
              object@compSpec@parameters %>% 
              rowwise() %>% 
              do( normalizeGroupsBySpec( ., 
                                         object,
                                         aggrERData, 
                                         object@entityAttributes, 
                                         object@compSpec@normalizationFuncSpecToFunc ) ) %>% 
              ungroup()
            
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            aggrERData
          }
)

##---------------------------------------------------------
## Convert to a list of sparse matrices, impose row IDs, and concatenate to a sparse matrix.
setMethod("makeSparseMatrices",
          signature = c(object = "DataTransformer"), 
          function(object, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Sparse matrix." ) }
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tMake sparse matrices...\n") }
            
            ## Find is the calculation of a label feature matrix specified?
            findLabelMatQ <- HasLabelRowQ( compSpec = object@compSpec@parameters, labelVariable = "Label" )

            if( findLabelMatQ ) {
              labelMat <- 
                xtabs( ~ EntityID + Value, 
                       object@entityAttributes %>% dplyr::filter( Attribute == "Label" ), 
                       sparse = T)
              colnames(labelMat) <- paste( "Label", colnames(labelMat), sep="." )
              labelMat@x[labelMat@x > 1 ] <- 1
            }
            
            if( is.null(object@transformedData) || nrow(object@transformedData) == 0 ) {
              stop("The object@transformedData is empty.", call. = TRUE)
            }
            
            smats <- 
              purrr::map( 
                split( object@transformedData, object@transformedData$MatrixName ), 
                function(x) { 
                  
                  if( findLabelMatQ && x$MatrixName[[1]] == "Label" ) { 
                    NA
                  } else {
                    ## I am not sure this here is the best way to handle this.
                    if( sum( is.nan(x$AValue) ) ) {
                      warning( paste0( "NaN values for the matrix", x$MatrixName[[1]], ". Attempting to continue by replacing each NaN with 0." ), call. = T )
                      x$AValue[ is.nan(x$AValue) ] <- 0
                    }
                    
                    res <- xtabs( AValue ~ EntityID + VarID, x, sparse = T) 
                    colnames(res) <- paste( x$MatrixName[[1]], colnames(res), sep="-")
                    res
                  }
                  
                }) 
            
            smats <- smats[ !is.na(smats) ]
            
            allRowIDs <- unique( unlist( purrr::map( smats, rownames) ) )
            
            smats <- purrr::map( smats, function(x) ImposeRowIDs( rowIDs = allRowIDs, smat = x ) )
            
            if( findLabelMatQ ) {
              smats <- c( smats, Label = ImposeRowIDs( rowIDs = allRowIDs, smat = labelMat ) )
            }
            
            ## Sort the columns of the sparse matrices
            smatNames <- names(smats)
            smats <- 
              purrr::map( 
                smats, 
                function(sm) {
                  if( sum( grepl( "\\.", colnames(sm)[[1]] ) ) == 0 ) { sm } 
                  else {
                    ## Suppress warning messages while converting column names to corresponding values
                    oldw <- getOption("warn"); options(warn = -1)
                    colVals <- as.numeric( purrr::map( strsplit( colnames(sm), "\\." ), function(x) x[[length(x)]] ) )
                    options(warn = oldw)
                    
                    if ( sum( is.na( colVals ) ) > 0 || mean(sort(colVals) != (1:ncol(sm))) > 0 ) { sm }
                    else {
                      colOrder <- order( colVals  )
                      sm[, colOrder, drop = F]
                    }
                  }
                })
            names(smats) <- smatNames

            ## Most likely redundant at this point,
            ## because of the earlier check with object@transformedData.
            if( length(smats) == 0 ) {
              warning("No sparse matrices were derived.", call. = T)
              return(object)
            }
              
            ## Final result
            object@sparseMatrices <- smats
            object@dataMat <- do.call( cbind, smats )
            
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
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
            
            ## Find is the calculation of a label feature matrix specified?
            findLabelMatQ <- HasLabelRowQ( object@compSpec@parameters )
            
            if ( !findLabelMatQ && simpleConversion ) {             
              
              vsDF <- as.data.frame( as.matrix(dataMat), stringsAsFactors = T )
              
            } else if ( findLabelMatQ && simpleConversion ) { 
              ## Too simple, no NA handling.
              
              vsDF <- as.data.frame( as.matrix( dataMat[, -grep( "Label", colnames(dataMat) ) ] ), stringsAsFactors = T )
              if( !numericLabelColumn ) {
                vsDF <- cbind( vsDF, Label = ifelse( dataMat[,paste0("Label.",diedLabel)], diedLabel, survivedLabel ), stringsAsFactors = T )  
              } else { 
                vsDF <- cbind( vsDF, Label = dataMat[,paste0("Label.",diedLabel)], stringsAsFactors = T ) 
              }
              
            } else if ( !findLabelMatQ && !simpleConversion ) {  
              
              ## Proper handling of NA's.
              
              vsMat <- matrix(NA, nrow = nrow(dataMat), ncol = ncol(dataMat) )
              dataMatDF <- as.data.frame( summary(dataMat) )
              vsMat[ as.matrix(dataMatDF[,1:2]) ] <- dataMatDF[,3]
              rownames(vsMat) <- rownames(dataMat); colnames(vsMat) <- colnames(dataMat)
              vsDF <- as.data.frame( vsMat )
              
            } else if ( findLabelMatQ && !simpleConversion ) {  
              
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
            ## Note the assumption that the last column is the `Label` column. (See above.)
            if ( findLabelMatQ && numericLabelColumn ) {
              
              vsDF <- cbind( vsDF[,-ncol(vsDF)], Label = ifelse( dataMat[ , paste0("Label.", diedLabel) ], 1, 0 ) )
              
            }
            
            vsDF          
          }
)

##---------------------------------------------------------
## Related to the normalization step for the method "normalize". This is an intermediate step.
#' @description Applies a normalization function to long form contingency matrix data and stores the results.
#' @param object the current object
#' @param aggrERData aggregated event records in long form
setMethod("aggregateAndAccumulateOverGroups",
          signature = c(object = "DataTransformer", aggrERData = "data.frame" ), 
          function(object, aggrERData, ...) {
            
            if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/6, detail = "Aggregate and accumulate over groups of records." ) }
            
            additionalArgs <- list(...)
            echoStepsQ <- TRUE
            
            if( "echoStepsQ" %in% names(additionalArgs) ) { 
              echoStepsQ <- additionalArgs[["echoStepsQ"]] 
            }
            
            if( echoStepsQ ) { cat("\n\tAggregation over groups functions application (and accumulation)...\n") }
            
            allEntityAttributes <- unique(object@entityAttributes$Attribute)

            object@groupAggregatedValues <-
              purrr::map_dfr( split( object@compSpec@parameters, object@compSpec@parameters$MatrixName), function(specRow) { 
                
                if( specRow$Normalization.function[[1]] != "None" ) {
                  
                  if( !( specRow$Normalization.function[[1]] %in% names(object@compSpec@normalizationFuncSpecToFunc) ) ) {
                    stop( paste0( "Unknown normalization function \"", specRow$Normalization.function[[1]], "\" ", 
                                  "in the computation specification."), 
                          call. = TRUE )
                  }
            
                  func <- object@compSpec@normalizationFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]
                  
                  if ( specRow$Normalization.scope[[1]] == "Variable" ) {
                    
                    dfNormalizationValues <- 
                      aggrERData %>% 
                      dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                      dplyr::summarise( NormalizationValue = func(AValue), .groups = "drop" ) %>% 
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
                      aggrERData %>% 
                      dplyr::filter( MatrixName == specRow$MatrixName ) %>%
                      dplyr::inner_join( object@entityAttributes[, c("EntityID", "Attribute")] %>% 
                                           dplyr::filter( Attribute == specRow$Normalization.scope[[1]] ),
                                         by = "EntityID" ) %>% 
                      dplyr::group_by( Attribute ) %>% 
                      dplyr::summarise( NormalizationValue = func(AValue), .groups = "drop" ) %>% 
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
                  
                } else {
                  NULL
                }
                
              })
            
            if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
            
            object
            
          })

##---------------------------------------------------------
## Normalization step for the method "normalize".
#' @description Applies a normalization function to long form contingency matrix data.
#' @param specRow a specification that has columns "MatrixName" and "Normalization"
#' @param object the current object; it is the second argument in order to use %>%
#' @param matLongFormData aggregated event records in long form
#' @param entityAttributes entity specific data
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
              

              if( length(object@groupAggregatedValues) > 0 ) {
                dfNormalizationValues <- 
                  object@groupAggregatedValues %>%
                  dplyr::filter( MatrixName == specRow$MatrixName )
              } else {
                dfNormalizationValues <- NULL
              }
              
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
              if( specRow$Normalization.function[[1]] != "None" ) {
                
                func <- normalizationFuncSpecToFunc[ specRow$Normalization.function[[1]] ][[1]]
                matLongFormData %>% 
                  dplyr::filter( MatrixName == specRow$MatrixName ) %>% 
                  group_by( EntityID ) %>% 
                  dplyr::mutate( AValue = AValue / func(AValue) ) %>% 
                  ungroup()
                
              }
              
            } else {
              
              matLongFormData %>% 
                dplyr::filter( MatrixName == specRow$MatrixName )
              
            }
          }
)
