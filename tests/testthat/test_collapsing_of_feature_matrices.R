context("Collapsing of feature matrices")
library(ERTMon)

numberOfEntities <- 10
nIntervals <- 5

testData <-
  ERTMonSimpleTestData( numberOfEntities = numberOfEntities, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 40, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )


ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

feMats <- ertmon0 %>% ERTMonTakeContingencyMatrices

focusMatNames <- grep( "Mean$", ertmon0 %>% ERTMonTakeFeatureNamePrefixes, value = T )
  
ertmon0 <- 
  ertmon0 %>% 
  ERTMonCollapseFeatureMatrices( matrixNames = focusMatNames )

resMats1 <- ertmon0 %>% ERTMonTakeValue

ertmon0 <- 
  ertmon0 %>% 
  ERTMonCollapseFeatureMatrices( matrixNames = focusMatNames, collapseFunction = rowSums )

resMats2 <- ertmon0 %>% ERTMonTakeValue

test_that("Collapse feature sub-matrices with colSums (default).", {
  expect_is( ertmon0, "ERTMon" )
  expect_type( resMats1, "list" )
  
  expect_true( length(resMats1) == length(focusMatNames) )
  expect_equal( mean( purrr::map_chr( resMats1, class ) == "numeric" ), 1 )
  
  expect_equal( mean( purrr::map2_dbl( resMats1, purrr::map( feMats[focusMatNames], colSums), function(x,y) mean(x==y) ) ), 1 )
})

test_that("Collapse feature sub-matrices with rowSums.", {
  expect_is( ertmon0, "ERTMon" )
  expect_type( resMats2, "list" )
  
  expect_true( length(resMats2) == length(focusMatNames) )
  expect_equal( mean( purrr::map_chr( resMats2, class ) == "numeric" ), 1 )
  
  expect_equal( mean( purrr::map2_dbl( resMats2, purrr::map( feMats[focusMatNames], rowSums), function(x,y) mean(x==y) ) ), 1 )
})