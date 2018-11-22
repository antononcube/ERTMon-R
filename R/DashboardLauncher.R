##===========================================================
## ERTMon-R dashboard launcher
## Copyright (C) 2018  Anton Antonov
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
# Title: Dashboard launcher
# Author: Anton Antonov
# Start date: 2017-08-02
#---


##===========================================================
## Create objects at play
##===========================================================
if ( FALSE ) {

  source("./R/LoadClasses.R")
  
  library(shinydashboard)
  #library(rhandsontable)
  library(shiny)
  library(DT)
  library(d3heatmap)
  
  ## Here we create objects with default values that are going
  ## to be used in the dashboard.
  
  ## Default data directory name.
  ## The directory name has all the CSV files needed to for dashboard.
  ## Of course the data ingestion can happen from other sources.
  ## Both data and computation specifications are held in that directory.
  directoryName <- "~/ERTMon-R/FakeData/"
  testDataDirectoryName <- "~/ERTMon-R/TestFakeData/"
  specFileName <- file.path( directoryName, "computationSpecification.csv" )
  
  ## Data ingester object for reading medical and patient data.
  ## [ ] Ideally the type of this object can be replaced in the dashboard.
  diObj <- new( "DataIngester")  
  diTestObj <- NULL 
  
  ## Computational specifications object for reading and ingesting specifications.
  ## I do not see why we would have different types for this object. (But of course we can.)
  compSpecObj <- new( "ComputationSpecification" )
  
  ## ERTMon object.
  

  runApp( "./R/Dashboard" )  
}
