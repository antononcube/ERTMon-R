##===========================================================
## ERTMon-R dashboard launcher
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
# Title: Dashboard launcher
# Author: Anton Antonov
# Start date: 2017-08-02
#---

source("./R/LoadClasses.R")

library(shinydashboard)
#library(rhandsontable)
library(shiny)
library(DT)


## Note that this brings and external dependency.
library(devtools)
##source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/VariableImportanceByClassifiers.R")


##===========================================================
## Create objects at play
##===========================================================
if ( TRUE ) {

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
  

  
}

runApp( "./R/Dashboard" )