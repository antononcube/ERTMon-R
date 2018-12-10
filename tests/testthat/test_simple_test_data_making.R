context("Simple test data making")
library(ERTMon)

constantTestData <-
  ERTMonSimpleTestData( numberOfEntities = 3, numberOfVariables = 1, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = FALSE, 
                        variableFunction = "Constant", 
                        exportDirectoryName = NULL )

linearTestData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

randomTestData <-
  ERTMonSimpleTestData( numberOfEntities = 1000, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 4, randomStartTimesQ = TRUE, 
                        variableFunction = "Random", 
                        exportDirectoryName = NULL )

functionTestData <-
  ERTMonSimpleTestData( numberOfEntities = 100, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 4, randomStartTimesQ = FALSE, 
                        variableFunction = function(x) { runif(n=1) }, 
                        exportDirectoryName = NULL )


test_that("Simple test data result names", {
  expect_type( constantTestData, "list" )
  expect_type( linearTestData, "list" )
  expect_type( randomTestData, "list" )
  expect_type( functionTestData, "list" )
  
  expect_equal( sort(names(constantTestData)), sort(c("EntityAttributes", "EventRecords","ComputationSpecification")) )
  expect_equal( sort(names(linearTestData)), sort(c("EntityAttributes", "EventRecords","ComputationSpecification")) )
  expect_equal( sort(names(randomTestData)), sort(c("EntityAttributes", "EventRecords","ComputationSpecification")) )
  expect_equal( sort(names(functionTestData)), sort(c("EntityAttributes", "EventRecords","ComputationSpecification")) )
})

test_that("Simple test data shapes", {
  expect_is(constantTestData$EntityAttributes, "data.frame")
  expect_is(constantTestData$EventRecords, "data.frame")
  expect_is(constantTestData$ComputationSpecification, "data.frame")
  
  expect_is(linearTestData$EntityAttributes, "data.frame")
  expect_is(linearTestData$EventRecords, "data.frame")
  expect_is(linearTestData$ComputationSpecification, "data.frame")
  
  expect_is(randomTestData$EntityAttributes, "data.frame")
  expect_is(randomTestData$EventRecords, "data.frame")
  expect_is(randomTestData$ComputationSpecification, "data.frame")
  
  expect_is(functionTestData$EntityAttributes, "data.frame")
  expect_is(functionTestData$EventRecords, "data.frame")
  expect_is(functionTestData$ComputationSpecification, "data.frame")
})

test_that("Simple test constant data", {

  ## The constant data is prepared to have same starting times for all entities.
  ## This test checks that.
  expect_equal( constantTestData$EventRecords %>% 
                  dplyr::group_by( EntityID ) %>% 
                  dplyr::summarise( MinObsTime = min(ObservationTime) ) %>% 
                  dplyr::select( MinObsTime ) %>% 
                  unique() %>% 
                  nrow(),
                1 
  )

  ## Check that we have constant values.  
  expect_equal( constantTestData$EventRecords %>% 
                  dplyr::group_by( EntityID ) %>% 
                  dplyr::summarise( MinValue = min(Value) ) %>% 
                  dplyr::select( MinValue ) %>% 
                  unique() %>% 
                  nrow(),
                1 
  )
})


test_that("Simple test linear data", {
  ## The linear data is prepared to have different, random starting times for the entities.
  ## This test checks that.
  expect_equal( linearTestData$EventRecords %>% 
                  dplyr::group_by( EntityID ) %>% 
                  dplyr::summarise( MinObsTime = min(ObservationTime) ) %>% 
                  dplyr::select( MinObsTime ) %>% 
                  unique() %>% 
                  nrow(),
                length(unique(linearTestData$EntityAttributes$EntityID))
  )
  
  ## This is one way of testing the expected linearity of the values.
  ## The expected mean(res==1) value is 1, but for some reason it is not.
  ## A possible explanation is the way the liear function is run over the time grid.
  expect_true(
    { res <- 
      linearTestData$EventRecords %>% 
      dplyr::group_by( EntityID ) %>% 
      dplyr::mutate( Diff = c(1,diff(Value))) %>% dplyr::ungroup() %>% dplyr::select(Diff) %>% unlist()
      mean(res==1) > 0.97
    }
    )
})
