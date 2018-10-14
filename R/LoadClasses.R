##===========================================================
## Event records transformations OOP class loading script
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
# Title: Load classes
# Author: Anton Antonov
# Start date: 2018-10-13
#---


library(plyr)
library(tidyr)
library(dplyr)

library(stringi)
library(Matrix)
library(ggplot2)
library(RcppRoll)

#devtools::install_github("jrowen/rhandsontable")
library(rhandsontable)

#devtools::install_github("rstudio/sparklyr")
#library(sparklyr)

#spark_install(version = "1.6.2")
#spark_install(version = "1.6.0")

library(h2o)

library(devtools)

## 
source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/DataConversionFunctions.R")
source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/OutlierIdentifiers.R")
#source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/VariableImportanceByClassifiers.R")

# source("~/MathematicaForPrediction/R/DataConversionFunctions.R")
# source("~/MathematicaForPrediction/R/OutlierIdentifiers.R")
# source("~/MathematicaForPrediction/R/VariableImportanceByClassifiers.R")

# fnames = list.files( path = ".", pattern = ".*Class.R" )
# llply( fnames, source ) 

if( !exists("ertMonDirName") ) {
  ertMonDirName <- "~/ERTMon-R"
}

source( file.path( ertMonDirName, "R", "DataIngestionFrameworkFunctions.R") )
source( file.path( ertMonDirName, "R", "DataWrapperClass.R") )
source( file.path( ertMonDirName, "R", "DataIngesterClass.R") )
source( file.path( ertMonDirName, "R", "DataSplitterClass.R") )
source( file.path( ertMonDirName, "R", "ComputationSpecificationClass.R") )
source( file.path( ertMonDirName, "R", "DataTransformerClass.R") )
source( file.path( ertMonDirName, "R", "DataTransformerCatMatricesClass.R") )


