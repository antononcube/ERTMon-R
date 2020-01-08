
if(FALSE) {
  
  rm(list=ls())
  library(ERTMon)
  #source("./R/LoadClasses.R")
 
  testData <-
    ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                          timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                          variableFunction = "Linear", 
                          exportDirectoryName = NULL )
  
  # testData$ComputationSpecification$Normalization.function <- "Count"
  testData$ComputationSpecification$Aggregation.function <- "Count"
  testData$ComputationSpecification <- unique(testData$ComputationSpecification)
  
  testData$ComputationSpecification <-
    testData$ComputationSpecification %>% 
    dplyr::group_by( Variable ) %>% 
    dplyr::filter( dplyr::row_number() == n() ) %>% 
    ungroup()
  
  ertmon1 <-
    ERTMonUnit() %>%
    ERTMonSetEventRecords( testData$EventRecords ) %>%
    ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
    ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
    ERTMonProcessEventRecords( alignmentSpec = "MinTime" )
  
  View(ertmon1 %>% ERTMonTakeTimeCellsInterpretation)
  
}