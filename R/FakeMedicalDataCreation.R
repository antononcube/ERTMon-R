##===========================================================
## Event records transformations monad fake medical data creation
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
##
##===========================================================


#---
# Title: Fake medical data creation
# Author: Anton Antonov
# Start date: 2018-10-13
#---


## Mission
##--------------------------
##
## Create fake medical data in order to provide test data for ERTMon.
## At least 500 patients, and at least 3 vital signs per patient.
##
##
## Approach
##--------------------------
##
## The simplest way to produce this data is to have time series that: 
##  1. are more-or-less constant for the patients of each type;
##  2. have stiff upward and/or downward gradients at their ends wrt patient condition;
##  3. have the same density for all patients;
##  4. have the same unit for all patients.
##
## Further elaborations and improvements can be done on this base algorithm.
##
##
## Alternatives to consider
##--------------------------
##
## 1. Similar type of data can be taken from financial stock markets or weather data.
##
## 2. We can use data derived from nutritional and activity databases.
##    Like, MyFitnessPal and FitBit.
##
## 3. Instead of using fake data we can use public data for other types of 
##    critical condtion. For example:
##      "Epileptic Seizure Recognition Data Set"
##       http://archive.ics.uci.edu/ml/datasets/Epileptic+Seizure+Recognition .


##===========================================================
## Parameters
##===========================================================

nEntities <- 500
outputDirName <- file.path( ".", "data", "FakeData")
writeFilesQ <- FALSE
evaluateAllQ <- FALSE
evaluateQ <- FALSE

##===========================================================
## Load libraries
##===========================================================

library(purrr)
library(ggplot2)
library(reshape2)
library(lubridate)

##===========================================================
## Generate patients
##===========================================================
if ( evaluateQ && ( evaluateAllQ || !exists("fakeEntityData") ) ) {
  
  cat("\n\tCreate fake patients data...\n")
  
  ## The patients ages are normaly distributed and restricted to in the range [0,100]
 
  ages <- round(rnorm( n = 2*nEntities, mean = 50, sd = 20))
  ages <- ages[ 0 <= ages & ages <= 100 ]
  ages <- sample( ages, nEntities )
  ## hist(ages)
  
  codes <- runif( n = length(ages), min = 0, max = 1 )
  codes <- ifelse( codes > 0.7, "CB", "Non.CB" )
  
  fakeEntityData <- data.frame( EntityID = 1:length(ages), Age = ages, Label = codes, stringsAsFactors = FALSE )

  fakeEntityData <- reshape2::melt( fakeEntityData, id.vars = "EntityID" )
  fakeEntityData <- setNames( fakeEntityData, c("EntityID", "Attribute", "Value" ) )
  fakeEntityData <- fakeEntityData[ order(fakeEntityData$EntityID), ]
  
  print( summary( as.data.frame( unclass( fakeEntityData ) ) ) )
  
  if( writeFilesQ ) {
    cat("\n\t\tWrite fake medical records to:", file.path( outputDirName, "entityAttributes.csv" ), "\n")
    
    write.csv( x = fakeEntityData, file = file.path( outputDirName, "entityAttributes.csv" ) )
  }
  
  cat("\n\t\t...DONE\n")
}



##===========================================================
## Generate patients vitals
##===========================================================
if ( evaluateQ && ( evaluateAllQ || !exists("fakeMedicalRecords") ) ) {
  
  cat("\n\tCreate fake medical records...\n")
  
  ## For each patient generation HR, RR, and NBP wrt to his/her final condition.  
  ## Time grid
  
  cbVitalSignsMeans <- c( HR = 110, RR = 50, NBP.M = 140 )
  nonCBVitalSignsMeans <- c( HR = 70, RR = 30, NBP.M = 110 )
  
  nIntervals <- 16
  timeInterval <- 900 
  timeGridIndexes <- 0:(nIntervals-1)
  biasIndexes <- timeGridIndexes[(nIntervals-4):nIntervals]

  fakeMedicalRecords <- 
    purrr::map_dfr( names(cbVitalSignsMeans), function(vSign) {
      
      purrr::pmap_dfr( as.list(fakeEntityData[ fakeEntityData$Attribute == "Label", ]), function(...) {
        
        args <- list(...)
        rowEntityID <- args$EntityID; rowValue <- args$Value;

        timeGrid <- Sys.time() + timeInterval * timeGridIndexes
        values <- round(runif(n = length(timeGrid), min = 0.8, max = 1.2 ))
        
        if( rowValue == "CB" ) {
          values <- cbVitalSignsMeans[[vSign]] * values
          if( runif(1) < 0.5 ) { values[biasIndexes] <- 1.4*values[biasIndexes] }
          else { values[biasIndexes] <- 0.4*values[biasIndexes] }
          
        } else {
          values <- nonCBVitalSignsMeans[[vSign]] * values
        }
        
        ## "EntityID","LocationID","ObservationTime","Variable","Value","ObservationTime"
        data.frame( EntityID = rowEntityID, 
                    LocationID = "UKNWN", 
                    ObservationTimeString = timeGrid, 
                    Variable = vSign, 
                    Value = values, 
                    ObservationTime = floor(as.numeric(timeGrid)), 
                    stringsAsFactors = FALSE )
        
      })
      
    })
  
  
  print( summary( as.data.frame( unclass( fakeMedicalRecords ) ) ) )
  
  
  if( writeFilesQ ) {
    cat("\n\t\tWrite fake medical records to:", file.path( outputDirName, "eventRecords.csv" ), "\n")
    
    write.csv( x = fakeMedicalRecords, file = file.path( outputDirName, "eventRecords.csv" ) )
  }
  
  cat("\n\t\t...DONE\n")
}
