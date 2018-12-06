
if(FALSE) {
  
  rm(list=ls())
  # library(ERTMon)
  source("./R/LoadClasses.R")
 
  testData <-
    ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                          timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                          dataType = "Linear", 
                          exportDirectoryName = NULL )
  
  # testData$ComputationSpecification$Normalization.function <- "Mean"
  
  ertmon1 <-
    ERTMonUnit() %>%
    ERTMonSetEventRecords( testData$EventRecords ) %>%
    ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
    ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
    ERTMonProcessEventRecords( alignmentSpec = "MinTime" )
  
  View(ertmon1 %>% ERTMonTakeTimeCellsInterpretation)
  
}