context("Collapsing of feature matrices")
library(ERTMon)

##===========================================================
## The tests implemented are
## 1) [X] sanity check tests,
## 2) [X] correctness test for colSums,
## 3) [X] correctness test for rowSums.

## More tests are needed: 
## 1) [ ] with that other collapse functions, 
## 2) [ ] with specified entity ID's,
## 3) [ ] appropriate warnings tests.
##===========================================================

## The tests are supposed to pass with both TRUE and FALSE
completeColumnRangeQ <- TRUE

## Generate data.
testData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 10, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

## Transform the generated event records.
ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

## Extract feature sub-matrices.
feMats <- ertmon0 %>% ERTMonTakeContingencyMatrices( columnPrefixesQ = F, completeColumnRangeQ = completeColumnRangeQ )

## Select feature sub-matrices names to focus on.
focusMatNames <- grep( "Mean$", ertmon0 %>% ERTMonTakeFeatureNamePrefixes, value = T )
  
## Collapse feature specified sub-matrices with the default collapse function, colSums.
ertmon0 <- 
  ertmon0 %>% 
  ERTMonCollapseFeatureMatrices( matrixNames = focusMatNames, completeColumnRangeQ = completeColumnRangeQ )

## Get the result. 
resMats1 <- ertmon0 %>% ERTMonTakeValue

## Collapse feature specified sub-matrices with rowSums.
ertmon0 <- 
  ertmon0 %>% 
  ERTMonCollapseFeatureMatrices( matrixNames = focusMatNames, collapseFunction = rowSums, completeColumnRangeQ = completeColumnRangeQ )

## Get the result.
resMats2 <- ertmon0 %>% ERTMonTakeValue


test_that("Collapse feature sub-matrices with colSums (default).", {
  expect_is( ertmon0, "ERTMon" )
  expect_type( resMats1, "list" )
  
  expect_true( length(resMats1) == length(focusMatNames) )
  expect_equal( mean( purrr::map_chr( resMats1, class ) == "numeric" ), 1 )
  
  ## This the "what is the function supposed to do" test.
  expect_equal( mean( purrr::map2_dbl( resMats1, purrr::map( feMats[focusMatNames], colSums), function(x,y) mean(x==y) ) ), 1 )
})


test_that("Collapse feature sub-matrices with rowSums.", {
  expect_is( ertmon0, "ERTMon" )
  expect_type( resMats2, "list" )
  
  expect_true( length(resMats2) == length(focusMatNames) )
  expect_equal( mean( purrr::map_chr( resMats2, class ) == "numeric" ), 1 )
  
  ## This the "what is the function supposed to do" test.
  expect_equal( mean( purrr::map2_dbl( resMats2, purrr::map( feMats[focusMatNames], rowSums), function(x,y) mean(x==y) ) ), 1 )
})

