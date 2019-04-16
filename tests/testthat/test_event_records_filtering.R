context("Event records filtering tests")
library(ERTMon)
library(magrittr)
library(Matrix)

numberOfEntities <- 10
numberOfVariables <- 5
nIntervals <- 5

set.seed(78834)
testData <-
  ERTMonSimpleTestData( numberOfEntities = numberOfEntities, numberOfVariables = numberOfVariables, 
                        timeInterval = 900, numberOfTimeCells = nIntervals, randomStartTimesQ = TRUE, 
                        variableFunction = "Random", 
                        exportDirectoryName = NULL )

minTime <- min(testData$EventRecords$ObservationTime)

ertmon0 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonFilterEventRecords( minObservationTime = "MinTime", maxObservationTime = "MaxTime" )

ertmon1 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonFilterEventRecords( minObservationTime = "MaxMinTime", maxObservationTime = "MinMaxTime" )

ertmon2 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonFilterEventRecords( variables = c("Var.1", "Var.2"), entityIDs = c( 1, 2, 3 ) )

ertmon3 <-
  ERTMonUnit( eventRecords = testData$EventRecords, 
              entityAttributes = testData$EntityAttributes, 
              compSpec = testData$ComputationSpecification ) %>%
  ERTMonFilterEventRecords( minObservationTime = minTime, maxObservationTime = "MinMaxTime" )


## More test with ertmon1 has to be added.

test_that("Expected new member", {
  expect_is( ertmon0$SummarisedObservationTimes, "data.frame"  )
  expect_is( ertmon1$SummarisedObservationTimes, "data.frame"  )
  expect_is( ertmon2$SummarisedObservationTimes, "data.frame"  )
  expect_is( ertmon3$SummarisedObservationTimes, "data.frame"  )
}) 

test_that("Expected (no) changes", {
  expect_equal( nrow(ertmon0$EventRecords), nrow(testData$EventRecords) )
  expect_true( nrow(ertmon1$EventRecords) < nrow(testData$EventRecords) )
  expect_true( nrow(ertmon2$EventRecords) < nrow(testData$EventRecords) )
  expect_true( nrow(ertmon3$EventRecords) < nrow(testData$EventRecords) )
}) 

test_that("Expected variables", {
  expect_equal( unique((ertmon0 %>% ERTMonTakeEventRecords)$Variable), paste0( "Var.", 1:numberOfVariables ) )
  expect_equal( unique((ertmon2 %>% ERTMonTakeEventRecords)$Variable), c("Var.1", "Var.2") )
}) 

test_that("Expected entity ID's", {
  expect_equal( unique((ertmon0 %>% ERTMonTakeEventRecords)$EntityID), 1:numberOfEntities  )
  expect_equal( unique((ertmon2 %>% ERTMonTakeEventRecords)$EntityID), c( 1, 2, 3)  )
}) 

