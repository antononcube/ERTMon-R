
if(FALSE) {
  
  rm(list=ls())
  library(ERTMon)
  # source("./R/LoadClasses.R")
  
  dirName <- if( grepl("vignettes", getwd()) ) { file.path( getwd(), "..", "data", "FakeData") } else { file.path( getwd(), "data", "FakeData")}
  print(dirName)
  
  source( file.path( dirName, "..", "..", "R", "LoadClasses.R") )
  
  fakeEventRecords <- read.csv( file = file.path( dirName, "eventRecords.csv"), stringsAsFactors = FALSE)
  print(head(fakeEventRecords))
  
  fakeEntityAttributes <- read.csv( file = file.path( dirName, "entityAttributes.csv"), stringsAsFactors = FALSE)
  print(head(fakeEntityAttributes))
  
  fakeCompSpec <- read.csv( file = file.path( dirName, "computationSpecification.csv"), stringsAsFactors = FALSE)
  print(fakeCompSpec)
  
  cat("\nSetting data...\n")
  
  ertmon1 <-
    ERTMonUnit() %>%
    ERTMonSetEventRecords( fakeEventRecords ) %>%
    ERTMonSetEntityAttributes( fakeEntityAttributes ) %>%
    ERTMonSetComputationSpecification( fakeCompSpec )
  
  cat("...DONE\n")
  
  cat("\nMain processing function...\n")
  
  ertmon1 <-
    ertmon1 %>%
    ERTMonProcessEventRecords( alignmentSpec = "MaxTime" )
  
  cat("...DONE\n")
  
  View(ertmon1 %>% ERTMonTakeTimeCellsInterpretation)
  
  cat("\nContingency matrices:\n")
  smats <- ertmon1 %>% ERTMonTakeContingencyMatrices()
  cat(names(smats), "\n")
  
}