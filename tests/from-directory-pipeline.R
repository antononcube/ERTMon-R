
if(FALSE) {
  
  rm(list=ls())
  library(ERTMon)
  #source("./R/LoadClasses.R")
  
  
  dirName <- if( grepl("vignettes", getwd()) ) { file.path( getwd(), "..", "data", "FakeData") } else { file.path( getwd(), "data", "FakeData")}
  print(dirName)
  
  source( file.path( dirName, "..", "..", "R", "LoadClasses.R") )
  
  cat("\nReading data from \"", dirName, "\" ...\n")
  
  if(FALSE ) {
    ertmon1 <-
      ERTMonUnit() %>%
      ERTMonReadDataFromDirectory( dirName )
  } else { 
    ertmon1 <-
      ERTMonUnit() %>%
      ERTMonReadComputationSpecification( file.path( dirName, "computationSpecification.csv" ) ) %>% 
      ERTMonReadDataFromDirectory( dirName, readCompSpecQ = FALSE )
  }
  
  cat("...DONE\n")
  
  cat("\nMain processing function...\n")
  
  ertmon1 <-
    ertmon1 %>%
    ERTMonProcessEventRecords()
  
  cat("...DONE\n")
  
  View(ertmon1 %>% ERTMonTakeTimeCellsInterpretation)
  
  cat("\nContingency matrices:\n")
  smats <- ertmon1 %>% ERTMonTakeContingencyMatrices()
  cat(names(smats), "\n")
  
}