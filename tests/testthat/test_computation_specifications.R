context("Computation Specification")
library(ERTMon)

testData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 3, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

compSpecWideForm <- testData$ComputationSpecification
compSpecLongForm <- ComputationSpecificationToLongForm(testData$ComputationSpecification)

test_that("Computation specification type", {
  expect_equal( ComputationSpecificationType(compSpecLongForm), "LongForm" )
  expect_equal( ComputationSpecificationType(compSpecWideForm), "WideForm" )
  expect_equal( ComputationSpecificationType(c(2312)), "Unknown" )
})

test_that("Computation specification acceptance.", {
  expect_type( ERTMonUnit() %>% ERTMonSetComputationSpecification( compSpecLongForm ), "list" )
  expect_is( ERTMonUnit() %>% ERTMonSetComputationSpecification( compSpecLongForm ), "ERTMon" )
  
  expect_type( ERTMonUnit() %>% ERTMonSetComputationSpecification( compSpecWideForm ), "list" )
  expect_is( ERTMonUnit() %>% ERTMonSetComputationSpecification( compSpecWideForm ), "ERTMon" )
  
  expect_warning( ERTMonUnit() %>% ERTMonSetComputationSpecification( runif(12) ) )
  expect_equal( ERTMonUnit() %>% ERTMonSetComputationSpecification( 3432 ), ERTMonFailureSymbol )
})
  
