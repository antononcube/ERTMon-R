context("Pipeline over non-ovelapping test data")
library(ERTMon)

numberOfTimeCells1 <- 5
timeInterval1 <- 900

testData1 <-
  ERTMonSimpleTestData( numberOfEntities = 4, numberOfVariables = 3, 
                        timeInterval = timeInterval1, numberOfTimeCells = numberOfTimeCells1, randomStartTimesQ = FALSE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

testData2 <-
  ERTMonSimpleTestData( numberOfEntities = 3, numberOfVariables = 2, 
                        timeInterval = 300, numberOfTimeCells = 7, randomStartTimesQ = FALSE, 
                        variableFunction = "Random", 
                        exportDirectoryName = NULL )

## Change the variables of testData2 to be start with "Offset".
## Change the entity ID's to be greater than 100.
testData2$ComputationSpecification <-
  testData2$ComputationSpecification %>% 
  dplyr::mutate( Variable = paste0( "Offset", Variable ) )

testData2$EntityAttributes <-
  testData2$EntityAttributes %>% 
  dplyr::mutate( EntityID = 100 + EntityID )

testData2$EventRecords <-
  testData2$EventRecords %>% 
  dplyr::mutate( Variable = paste0( "Offset", Variable ) ) %>% 
  dplyr::mutate( EntityID = 100 + EntityID ) %>% 
  dplyr::mutate( ObservationTime = ObservationTime + numberOfTimeCells1 + (numberOfTimeCells1 + 3) * timeInterval1 ) 

## Combine testData1 and testData2.
testData <- 
  list( EventRecords = 
          rbind( testData1$EventRecords, 
                 testData2$EventRecords ),
        EntityAttributes = 
          rbind( testData1$EntityAttributes, 
                 testData2$EntityAttributes ),
        ComputationSpecification = 
          rbind( testData1$ComputationSpecification, 
                 testData2$ComputationSpecification)
  )

## Modify the computation spefication.
testData$ComputationSpecification$Aggregation.function <- "Mean"
testData$ComputationSpecification$Aggregation.interval.length <- 2*900
testData$ComputationSpecification <- unique(testData$ComputationSpecification)

testData$ComputationSpecification <-
  testData$ComputationSpecification %>% 
  dplyr::group_by( Variable ) %>% 
  dplyr::filter( dplyr::row_number() == n() ) %>% 
  ungroup()

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttributes ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = min(testData2$EventRecords$ObservationTime) - timeInterval1/2 )

fmat0 <- as.matrix( ertmon0 %>% ERTMonTakeFeatureMatrix )

## The way the alignmentSpec was chosen is such that all features should "Offset" in them.
test_that("No variables from testData1", {
  expect_equal( length( grep( "Offset", colnames(fmat0), invert = T) ), 0 )
  expect_equal( mean( as.integer( rownames(fmat0) ) > 100 ), 1 )
})
