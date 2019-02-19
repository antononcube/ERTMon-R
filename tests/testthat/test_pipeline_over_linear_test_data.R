context("Pipeline over linear test data")
library(ERTMon)

testData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )


testData$ComputationSpecification$Aggregation.function <- "Count"
testData$ComputationSpecification$Aggregation.interval.length <- 3*900
testData$ComputationSpecification <- unique(testData$ComputationSpecification)

testData$ComputationSpecification <-
  testData$ComputationSpecification %>% 
  dplyr::group_by( Variable ) %>% 
  dplyr::filter( dplyr::row_number() == n() ) %>% 
  ungroup()

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

fmat0 <- as.matrix( ertmon0 %>% ERTMonTakeFeatureMatrix )

dfTCI0 <- ertmon0 %>% ERTMonTakeTimeCellsInterpretation
dfTCI0 <- dfTCI0[ grep("^Var", dfTCI0$MatrixName), ]

## Positive time are expected because of alignmentSpec = "MinTime".
test_that("Positive time grid cells interpretation times", {
  expect_equal( mean( dfTCI0$StartTime >= 0 ), 1 )
  expect_equal( mean( dfTCI0$EndTime >= 0 ), 1 )
  expect_equal( mean( dfTCI0$EndTime - dfTCI0$StartTime > 0 ), 1 )
})

## The aggregation function is "Count" and the aggregation time interval is 3 times
## larger than the time interval used in the simple test data creation.
## Hence we expect all 3's for the variable features. (With prefixex "Var".)
test_that("Counts of three for each time cell", {
  expect_equal( mean( as.numeric(fmat0[, grep( "^Var", colnames(fmat0), value = T)]) == 3 ), 1 )
})
