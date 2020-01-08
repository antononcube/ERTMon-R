##===========================================================
## ERTMon-R dashboard launcher
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
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

  library(ERTMon)
  library(shinydashboard)
  #library(rhandsontable)
  library(shiny)
  library(DT)
  library(d3heatmap)
  library(ggplot2)
  
  ## Here we create objects with default values that are going
  ## to be used in the dashboard.
  
  ## Default data directory name.
  ## The directory name has all the CSV files needed to for dashboard.
  ## Of course the data ingestion can happen from other sources.
  ## Both data and computation specifications are held in that directory.
  directoryName <- file.path( "..", "..", "data", "FakeData")
  testDataDirectoryName <- file.path( "..", "..", "data", "TestFakeData")
  specFileName <- file.path( directoryName, "computationSpecification.csv" )
  exportModelID <- as.character( Sys.time() )
  exportFilePrefix <- gsub( pattern = " ", replacement = "_", paste0( exportModelID, "_" ), fixed = TRUE)
  
  # runApp( file.path( ".", "R", "Dashboard") )  
  runApp( ERTMonCreateInterface(  dataDirectoryName = directoryName, testDataDirectoryName = testDataDirectoryName) )
} else {
  cat("\n\t\tDashboardLauncher.R: Evaluation is prevented; check the value of the if-statement clause.\n\n")
}
