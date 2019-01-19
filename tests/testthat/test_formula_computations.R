context("Formula computations")
library(ERTMon)

numberOfEntities <- 10
nIntervals <- 5

testData <-
  ERTMonSimpleTestData( numberOfEntities = numberOfEntities, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 40, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )

testData$ComputationSpecification$Aggregation.interval.length <- 3*900
testData$ComputationSpecification$Max.history.length <- nIntervals * testData$ComputationSpecification$Aggregation.interval.length

formulaSpecDF <- 
  data.frame( 
    "FormulaID" = c(1),
    "TermID" = c(1,1,1,2,2), 
    "FeatureName" = c("Var.1.Mean", "Var.2.Mean", "Var.3.Mean", "Var.4.Mean", "Var.5.Mean"), 
    "ReduceFunction" = c("+"),
    "Coefficient" = c(1, 2, 3, 1, 1), 
    "Exponent" = c(1.5, 1, 2, 1, 1),
    "RatioPart" = c( "Numerator", "Denominator", "Denominator", "Numerator", "Denominator"),
    stringsAsFactors = FALSE)

formulaSpecMultDF <- formulaSpecDF
formulaSpecMultDF$ReduceFunction <- "*"

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

cMats <- ertmon0 %>% ERTMonTakeContingencyMatrices

## Direct formula computation.
cMatRes <- cMats$Var.1.Mean^1.5 / ( 2 * cMats$Var.2.Mean + 3 * cMats$Var.3.Mean^2 ) + cMats$Var.4.Mean / cMats$Var.5.Mean

cMatMultRes <- cMats$Var.1.Mean^1.5 / ( 2 * cMats$Var.2.Mean * 3 * cMats$Var.3.Mean^2 ) + cMats$Var.4.Mean / cMats$Var.5.Mean

ertmon0 <- ertmon0 %>% ERTMonComputeFormula( formulaSpec = formulaSpecDF )

formulaMat <- as.matrix( ertmon0 %>% ERTMonTakeValue )

ertmon0 <- ertmon0 %>% ERTMonComputeFormula( formulaSpec = formulaSpecMultDF )

formulaMultMat <- as.matrix( ertmon0 %>% ERTMonTakeValue )

## We get a matrix.
test_that("Formula matrix with plus", {
  expect_equal( mean( formulaSpecDF$FeatureName %in% names(cMats) ), 1 )
  expect_is( formulaMat, "matrix" )
})

test_that("Formula matrix with times", {
  expect_equal( mean( formulaSpecMultDF$FeatureName %in% names(cMats) ), 1 )
  expect_is( formulaMultMat, "matrix" )
})

## Getting a matrix with expected dimensions.
test_that("Expected dimensions of the feature matrix", {
  expect_true( nrow(formulaMat) == numberOfEntities )
  expect_true( ncol(formulaMat) <= nIntervals + 1 )
  expect_true( nrow(formulaMat) == nrow(cMatRes) )
  expect_true( ncol(formulaMat) == ncol(cMatRes) )
})

## Compare with alternative computations.
test_that("Comparison with a direct formula computation, plus", {
  expect_true( max(abs(cMatRes - formulaMat)) < 1.0E-8 )
})

test_that("Comparison with a direct formula computation, times", {
  expect_true( max(abs(cMatMultRes - formulaMultMat)) < 1.0E-8 )
})

