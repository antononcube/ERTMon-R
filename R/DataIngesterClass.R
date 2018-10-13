##===========================================================
## Event records transformations OOP data ingester class
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
                        fileNameRecords = "character", 
                        fileNamePatients = "character", 
                        dataObj = "DataWrapper",
                        progressObject = "ANY" ),
          
          prototype = list( ID = NA_character_, 
                            fileNameRecords = NA_character_, 
                            fileNamePatients = NA_character_, 
                            dataObj = NULL,
                            progressObject = NULL )
)

##-----------------------------------------------------------
## Methods
setGeneric( name="readDataFromDirectory", function( object, directoryName ) standardGeneric("readDataFromDirectory") )
setGeneric( name="readData", function( object, fileNameRecords, fileNamePatients ) standardGeneric("readData") )
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
           def = function(object, fileNameRecords, fileNamePatients)
           {
             cat("\n\tReading event records data and entity attributes data.\n")
          
             object@dataObj <- new( "DataWrapper" )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read event records data." ) }
             
             ## mrData
             object@dataObj@eventRecords <- read.csv( fileNameRecords, stringsAsFactors = FALSE )
             
             if( !is.null(object@progressObject) ) { object@progressObject$inc( 1/2, detail = "Read entity attributes data." ) }
             
             ## pData
             object@dataObj@entityAttributes <- read.csv( fileNamePatients, stringsAsFactors = FALSE )
             
             object@fileNameRecords = fileNameRecords
             object@fileNamePatients = fileNamePatients
             
             cat("\n\t\t...DONE\n")
             
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