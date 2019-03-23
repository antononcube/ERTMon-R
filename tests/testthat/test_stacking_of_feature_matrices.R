context("Stacking of feature matrices")
library(ERTMon)

##===========================================================
## The tests implemented are
## 1) [X] sanity check tests,
## 2) [X] correctness test for stacking with specified variables.
## 3) [X] and specified entity ID's,

## More tests are needed: 
## 1) [ ] with specifications that produce feature sub-matrices with different numbers of columns,
## 2) [ ] appropriate warnings tests.
##===========================================================

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
feMats <- ertmon0 %>% ERTMonTakeContingencyMatrices

## Select feature sub-matrices names to focus on.
focusMatNames <- grep( "Mean$", ertmon0 %>% ERTMonTakeFeatureNamePrefixes, value = T )

## Select entity ID's.
focusEntityIDs <- c( "1", "2", "6" )

## Stack feature specified sub-matrices.
ertmon0 <- 
  ertmon0 %>% 
  ERTMonStackFeatureMatrices( matrixNames = focusMatNames, entityIDs = focusEntityIDs, sep = "<@>" )

## Get the result. 
res <- ertmon0 %>% ERTMonTakeValue

## The "check" matrix.
checkMat <- do.call( rbind, purrr::map( feMats[focusMatNames], function(x) x[ focusEntityIDs, , drop=F] ) )
rownames(checkMat) <- NULL

res1 <- res
rownames(res1) <- NULL

test_that("Stack feature sub-matrices.", {
  expect_is( ertmon0, "ERTMon" )
  expect_true( "dgCMatrix" %in% class(res)  )
  
  expect_true( nrow(res) == length(focusMatNames) * length(focusEntityIDs) )

  ## This the "what is the function supposed to do" test.
  expect_equal( max(res1 - checkMat), 0. )
})
