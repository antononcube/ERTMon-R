##===========================================================
## Event records transformations OOP class loading script
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
##===========================================================

#---
# Title: Load classes
# Author: Anton Antonov
# Start date: 2018-10-13
#---

## The "loading of classes" should not be needed. 
## Keeping this file for awhile in order to address possible problems.

library(Matrix)

library(dplyr)
library(purrr)

library(stringi)
library(RcppRoll)

library(devtools)

# fnames = list.files( path = ".", pattern = ".*Class.R" )
# purrr::map( fnames, source ) 
# 
# if( !exists("ertMonDirName") ) {
#   ertMonDirName <- "~/ERTMon-R"
# }
# 
# source( file.path( ertMonDirName, "R", "DataConversionFunctions.R") )
# source( file.path( ertMonDirName, "R", "OutlierIdentifiers.R") )
# source( file.path( ertMonDirName, "R", "DataIngestionFrameworkFunctions.R") )
# source( file.path( ertMonDirName, "R", "DataWrapperClass.R") )
# source( file.path( ertMonDirName, "R", "DataIngesterClass.R") )
# source( file.path( ertMonDirName, "R", "DataSplitterClass.R") )
# source( file.path( ertMonDirName, "R", "ComputationSpecificationClass.R") )
# source( file.path( ertMonDirName, "R", "DataTransformerClass.R") )
# source( file.path( ertMonDirName, "R", "DataTransformerCatMatricesClass.R") )
# source( file.path( ertMonDirName, "R", "ERTMon.R") )
# source( file.path( ertMonDirName, "R", "SimpleTestDataCreation.R") )

