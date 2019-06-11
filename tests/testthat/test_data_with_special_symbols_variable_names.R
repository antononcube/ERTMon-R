context("Data with special symbols variable names")
library(ERTMon)

## This test is known to fail.
## It is included to test the fix / handling of these kind of cases.

testData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 42, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

testData$EventRecords <-
  testData$EventRecords %>% 
  dplyr::mutate( Variable = paste0( 'a[/.', Variable, '/]') )

testData$ComputationSpecification <-
  testData$ComputationSpecification %>% 
  dplyr::mutate( Variable = paste0( 'a[/.', Variable, '/]') )

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttributes ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )


test_that("Expected monad object", {
  expect_is( ertmon0, "ERTMon" )
})