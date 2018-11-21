##===========================================================
## Event records transformations OOP class loading script
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
# Title: Load classes
# Author: Anton Antonov
# Start date: 2018-10-13
#---

library(plyr)
library(dplyr)

library(stringi)
library(Matrix)
library(RcppRoll)

library(devtools)

## 
#source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/DataConversionFunctions.R")
source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/OutlierIdentifiers.R")

# source("~/MathematicaForPrediction/R/DataConversionFunctions.R")
# source("~/MathematicaForPrediction/R/OutlierIdentifiers.R")

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
source( file.path( ertMonDirName, "R", "ERTMon.R") )

