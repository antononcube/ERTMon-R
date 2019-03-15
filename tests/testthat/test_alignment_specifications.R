context("Alignment specification tests")
library(ERTMon)
library(magrittr)
library(Matrix)

numberOfEntities <- 10
nIntervals <- 5

set.seed(78834)
testData <-
  ERTMonSimpleTestData( numberOfEntities = numberOfEntities, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 40, randomStartTimesQ = TRUE, 
                        variableFunction = "Random", 
                        exportDirectoryName = NULL )

# testData$ComputationSpecification <-
#   testData$ComputationSpecification %>% 
#   dplyr::filter( Aggregation.function != "Range" )

testData$ComputationSpecification$Aggregation.interval.length <- 3*900
testData$ComputationSpecification$Max.history.length <- nIntervals * testData$ComputationSpecification$Aggregation.interval.length
testData$ComputationSpecification$Normalization.scope <- "Variable"
testData$ComputationSpecification$Normalization.function <- "Max"

meanTime <- mean(testData$EventRecords$ObservationTime)

## Make several monad objects with different alignments.

ertmon0 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

ertmon1 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonProcessEventRecords( alignmentSpec = "MaxTime" )

ertmon2 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonProcessEventRecords( alignmentSpec = meanTime )


## Time cells interpretation

dfTCI0 <- ertmon0 %>% ERTMonTakeTimeCellsInterpretation 

dfTCI1 <- ertmon1 %>% ERTMonTakeTimeCellsInterpretation 

dfTCI2 <- ertmon2 %>% ERTMonTakeTimeCellsInterpretation 

test_that("Expected time cell interpreation values for 'MinTime'", {
  expect_equal( mean( dfTCI0$StartTime >= 0 ), 1 )
  expect_equal( mean( dfTCI0$EndTime > 0 ), 1 )
  expect_equal( mean( (dfTCI0$EndTime - dfTCI0$StartTime) > 0 ), 1 )
}) 

test_that("Expected time cell interpreation values for 'MaxTime'", {
  expect_equal( mean( dfTCI1$StartTime < 0 ), 1 )
  expect_equal( mean( dfTCI1$EndTime <= 0 ), 1 )
  expect_equal( mean( (dfTCI1$EndTime - dfTCI1$StartTime) > 0 ), 1 )
}) 

test_that("Expected time cell interpreation values for mean time", {
  expect_equal( mean( dfTCI2$StartTime >= 0 ), 1 )
  expect_equal( mean( dfTCI2$EndTime > 0 ), 1 )
  expect_equal( mean( (dfTCI2$EndTime - dfTCI2$StartTime) > 0 ), 1 )
  ## We expect with alignSpec = meanTime to have less columns.
  ## If did not generate the data randomStartTimesQ = TRUE, then at ~ half less columns.
  expect_true( ncol(ertmon2 %>% ERTMonTakeFeatureMatrix) < 0.95 * ncol(ertmon0 %>% ERTMonTakeFeatureMatrix) )
}) 
