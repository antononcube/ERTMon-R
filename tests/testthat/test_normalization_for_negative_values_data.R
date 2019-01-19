context("Normalization for negative values data")
library(ERTMon)

numberOfEntities <- 10
nIntervals <- 5

testData <-
  ERTMonSimpleTestData( numberOfEntities = numberOfEntities, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 40, randomStartTimesQ = TRUE, 
                        variableFunction = "Random", 
                        exportDirectoryName = NULL )

testData$ComputationSpecification$Aggregation.interval.length <- 3*900
testData$ComputationSpecification$Max.history.length <- nIntervals * testData$ComputationSpecification$Aggregation.interval.length
testData$ComputationSpecification$Normalization.scope <- "Variable"
testData$ComputationSpecification$Normalization.function <- "MaxAbs"

testData$EventRecords$Value <- (-10) * testData$EventRecords$Value

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

qDF <- ertmon0 %>% ERTMonTakeTrasformedData()

test_that("Values are all negative ", {
  expect_equal( mean( testData$EventRecords$Value <= 0 ), 1 )
}) 

test_that("Max under 1", {
  expect_equal( mean( qDF$AValue >= -1 ), 1 )
  expect_equal( mean( abs(qDF$AValue) <= 1 ), 1 )
}) 
