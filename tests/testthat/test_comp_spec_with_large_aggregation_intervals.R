context("Computation specification with large intervals")
library(ERTMon)

## This test is known to fail.
## It is included to test the fix / handling of these kind of cases.

numberOfTimeCells <- 4
timeInterval <- 900

testData <-
  ERTMonSimpleTestData( numberOfEntities = 2, numberOfVariables = 3, 
                        timeInterval = timeInterval, numberOfTimeCells = numberOfTimeCells, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

# testData$ComputationSpecification <-
#   testData$ComputationSpecification %>% 
#   dplyr::mutate( Aggregation.interval.length = 2*numberOfTimeCells*timeInterval,
#                  Max.history.length = 6*numberOfTimeCells*timeInterval )

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttributes ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )


test_that("Expected monad object", {
  expect_is( ertmon0, "ERTMon" )
})