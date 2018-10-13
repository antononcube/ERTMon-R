#---
# Title: Load classes
# Author: Anton Antonov
# Start date: 2017-07-15
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
## source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/DataConversionFunctions.R")
source("~/MathematicaForPrediction/R/DataConversionFunctions.R")
source("~/MathematicaForPrediction/R/OutlierIdentifiers.R")
source("~/MathematicaForPrediction/R/VariableImportanceByClassifiers.R")

# fnames = list.files( path = ".", pattern = ".*Class.R" )
# llply( fnames, source ) 

if( !exists("ertMonDirName") ) {
  ertMonDirName <- "~/ERTMon-R"
}

source( file.path( ertMonDirName, "R/DataIngestionFrameworkFunctions.R") )
source( file.path( ertMonDirName, "R/DataWrapperClass.R") )
source( file.path( ertMonDirName, "R/DataIngesterClass.R") )
source( file.path( ertMonDirName, "R/DataSplitterClass.R") )
source( file.path( ertMonDirName, "R/ComputationSpecificationClass.R") )
source( file.path( ertMonDirName, "R/DataTransformerClass.R") )
source( file.path( ertMonDirName, "R/DataTransformerCatMatricesClass.R") )


