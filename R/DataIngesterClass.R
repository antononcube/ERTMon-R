##===========================================================
## Event records transformations OOP data ingester class
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
##===========================================================


#---
# Title: Data Ingester class
# Author: Anton Antonov
# Start date: 2018-10-13
#---

##===========================================================
## Dependencies load
##===========================================================

#' @include DataWrapperClass.R
#' @include DataIngestionFrameworkFunctions.R
NULL

# if( !exists("ertMonDirName") ) {
#   ertMonDirName <- "~/ERTMon-R"
# }
# 
# source( file.path( ertMonDirName, "R/DataWrapperClass.R" ) )
# source( file.path( ertMonDirName, "R/DataIngestionFrameworkFunctions.R" ) )

##===========================================================
## DataIngester hierarchy
##===========================================================

## Strategy.
## Different data ingesters for different sources of data.
## All data ingesters can produce R data frames.

setClass( "DataIngester",
          slots = list( ID = "character", 
                        fileNameEventRecords = "character", 
                        fileNameEntityAttributes = "character", 
                        dataObj = "DataWrapper",
                        progressObject = "ANY" ),
          
          prototype = list( ID = NA_character_, 
                            fileNameEventRecords = NA_character_, 
                            fileNameEntityAttributes = NA_character_, 
                            dataObj = NULL,
                            progressObject = NULL )
)

##-----------------------------------------------------------
## Methods
setGeneric( name="readDataFromDirectory", function( object, directoryName, ... ) standardGeneric("readDataFromDirectory") )
setGeneric( name="readData", function( object, fileNameRecords, fileNameAttributes, ... ) standardGeneric("readData") )
setGeneric( name="setEventRecords", function( object, eventRecords ) standardGeneric("setEventRecords") )
setGeneric( name="setEntityAttributes", function( object, entityAttributes ) standardGeneric("setEntityAttributes") )
setGeneric( name="ingestData", function( object, labelAttributeName ) standardGeneric("ingestData") )


##===========================================================
## Method implementations
##===========================================================

##-----------------------------------------------------------
setMethod( "readDataFromDirectory",
           signature = c( "DataIngester", "character" ),
           def = function(object, directoryName, ...)
           {
             if( !VerifyDataDirectory(directoryName) ) {
               stop( paste0("Not a valid data directory,", 
                            directoryName, 
                            ". A valid directory has files with names 'eventRecords.csv' and 'entityAttributes.csv'."), 
                     call. = TRUE )
             }
             
             readData( object, 
                       file.path( directoryName, "eventRecords.csv" ), 
                       file.path( directoryName, "entityAttributes.csv" ),
                       ...)
           }
)


setMethod( "readData",
           signature = c( "DataIngester", "character", "character" ),
           def = function(object, fileNameRecords, fileNameAttributes, ...)
           {
             additionalArgs <- list(...)
             echoStepsQ <- TRUE
             
             if( "echoStepsQ" %in% names(additionalArgs) ) { 
               echoStepsQ <- additionalArgs[["echoStepsQ"]] 
             }
             
             if( echoStepsQ ) { cat("\n\tReading event records data and entity attributes data.\n") }
          
             object@dataObj <- new( "DataWrapper" )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read event records data." ) }
             
             ## mrData
             object@dataObj@eventRecords <- read.csv( fileNameRecords, stringsAsFactors = FALSE )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read entity attributes data." ) }
             
             ## pData
             object@dataObj@entityAttributes <- read.csv( fileNameAttributes, stringsAsFactors = FALSE )
             
             object@fileNameEventRecords <- fileNameRecords
             object@fileNameEntityAttributes <- fileNameAttributes
             
             if( echoStepsQ ) { cat("\n\t\t...DONE\n") }
             
             object
           }
)

setMethod("setEventRecords",
          signature = c(object = "DataIngester", eventRecords = "data.frame" ), 
          def = function(object, eventRecords) {
            
            if( is.null(object@dataObj) ) {
              object@dataObj <- new( "DataWrapper" )
            }
            
            object@dataObj@eventRecords <- eventRecords    
            object@fileNameEventRecords <- NA_character_
            object
          }
)

setMethod("setEntityAttributes",
          signature = c(object = "DataIngester", entityAttributes = "data.frame" ), 
          def = function(object, entityAttributes) {
            
            if( is.null(object@dataObj) ) {
              object@dataObj <- new( "DataWrapper" )
            }
            
            object@dataObj@entityAttributes <- entityAttributes
            object@fileNameEntityAttributes <- NA_character_
            object
          }
)

setMethod( "ingestData",
           signature = c( "DataIngester", "character" ),
           def = function(object, labelAttributeName)
           {
             if ( is.null( object@dataObj ) ) {
               stop( "Read data first.", call. = TRUE )
             }
     
             # assertthat::assert_that( labelAttributeName %in% unique(object@dataObj@entityAttributes$Attribute)  )
                                      
             if( !(labelAttributeName %in% unique(object@dataObj@entityAttributes$Attribute)) ) {
               object@dataObj@entityAttributes <- AddMissingLabelAttributes( object@dataObj@entityAttributes, labelValue = "None" )
             }
             
             qDF <- 
               object@dataObj@entityAttributes %>% 
               dplyr::filter( Attribute == labelAttributeName )
             
             
             object@dataObj@labels <- unique( qDF$Value )

             object
           }  
)