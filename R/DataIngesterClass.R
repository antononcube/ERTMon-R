##===========================================================
## Event records transformations OOP data ingester class
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
# Title: Data Ingester class
# Author: Anton Antonov
# Start date: 2018-10-13
#---

##===========================================================
## Dependencies load
##===========================================================

if( !exists("ertMonDirName") ) {
  ertMonDirName <- "~/ERTMon-R"
}

source( file.path( ertMonDirName, "R/DataWrapperClass.R" ) )
source( file.path( ertMonDirName, "R/DataIngestionFrameworkFunctions.R" ) )

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
setGeneric( name="readDataFromDirectory", function( object, directoryName ) standardGeneric("readDataFromDirectory") )
setGeneric( name="readData", function( object, fileNameRecords, fileNameAttributes ) standardGeneric("readData") )
setGeneric( name="setEventRecords", function( object, eventRecords ) standardGeneric("setEventRecords") )
setGeneric( name="setEntityAttributes", function( object, entityAttributes ) standardGeneric("setEntityAttributes") )
setGeneric( name="ingestData", function( object, labelAttributeName ) standardGeneric("ingestData") )


##===========================================================
## Method implementations
##===========================================================

##-----------------------------------------------------------
setMethod( "readDataFromDirectory",
           signature = c( "DataIngester", "character" ),
           def = function(object, directoryName)
           {
             if( !VerifyDataDirectory(directoryName) ) {
               stop( paste0("Not a valid data directory,", 
                            directoryName, 
                            ". A valid directory has files with names 'eventRecords.csv' and 'entityAttributes.csv'."), 
                     call. = TRUE )
             }
             
             readData( object, 
                       file.path( directoryName, "eventRecords.csv" ), 
                       file.path( directoryName, "entityAttributes.csv" ) )
           }
)


setMethod( "readData",
           signature = c( "DataIngester", "character", "character" ),
           def = function(object, fileNameRecords, fileNameAttributes)
           {
             cat("\n\tReading event records data and entity attributes data.\n")
          
             object@dataObj <- new( "DataWrapper" )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read event records data." ) }
             
             ## mrData
             object@dataObj@eventRecords <- read.csv( fileNameRecords, stringsAsFactors = FALSE )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read entity attributes data." ) }
             
             ## pData
             object@dataObj@entityAttributes <- read.csv( fileNameAttributes, stringsAsFactors = FALSE )
             
             object@fileNameEventRecords <- fileNameRecords
             object@fileNameEntityAttributes <- fileNameAttributes
             
             cat("\n\t\t...DONE\n")
             
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
           def = function(object, labelAttributeName )
           {
             if ( is.null( object@dataObj ) ) {
               stop( "Read data first.", call. = TRUE )
             }
     
             # assertthat::assert_that( labelAttributeName %in% colnames(object@dataObj@entityAttributes) )
             assertthat::assert_that( labelAttributeName %in% object@dataObj@entityAttributes$Attribute  )
                                      
             qDF <- 
               object@dataObj@entityAttributes %>% 
               dplyr::filter( Attribute == labelAttributeName )
             
             
             object@dataObj@labels <- unique( qDF$Value )

             object
           }  
)