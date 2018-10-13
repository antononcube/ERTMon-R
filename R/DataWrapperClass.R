#---
# Title: Data Wrapper class
# Author: Anton Antonov
# Start date: 2017-07-15
#---


##===========================================================
## DataWrapper hierarchy
##===========================================================
## May be random IDs can be genereated with stringi::stri_rand_strings(1,6) .

setClass( "DataWrapper",
          slots = list( ID = "character", 
                        eventRecords = "data.frame",
                        entityAttributes = "data.frame",
                        labels = "character",
                        diedLabel = "character", 
                        survivedLabel = "character" ),
          
          prototype = list( ID = NA_character_, 
                            eventRecords = NULL,
                            entityAttributes = NULL,
                            labels = NA_character_,
                            diedLabel = NA_character_,
                            survivedLabel = NA_character_ )
)

setClass( "DataWrapperSpark",
          contains = "DataWrapper"
)


##===========================================================
## DataWrapper function methods
##===========================================================

