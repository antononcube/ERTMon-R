##===========================================================
## Event records transformations OOP data wrapper class
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
# Title: Data Wrapper class
# Author: Anton Antonov
# Start date: 2018-10-13
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

