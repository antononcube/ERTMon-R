context("Formula computations")
library(ERTMon)


testData <-
  ERTMonSimpleTestData( numberOfEntities = 10, numberOfVariables = 5, 
                        timeInterval = 900, numberOfTimeCells = 36, randomStartTimesQ = TRUE, 
                        variableFunction = "Linear", 
                        exportDirectoryName = NULL )


testData$ComputationSpecification$Aggregation.interval.length <- 3*900

formulaSpecDF <- 
  data.frame( 
    "FormulaID" = c(1),
    "TermID" = c(1,1,1,2,2), 
    "FeatureName" = c("Var.1.Mean", "Var.2.Mean", "Var.3.Mean", "Var.4.Mean", "Var.5.Mean"),  
    "Coefficient" = c(1, 2, 3, 1, 1), 
    "Exponent" = c(1.5, 1, 2, 1, 1),
    "RatioPart" = c( "Numerator", "Denominator", "Denominator", "Numerator", "Denominator"),
    stringsAsFactors = FALSE)
formulaSpecDF

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonSetEventRecords( testData$EventRecords ) %>%
  ERTMonSetEntityAttributes( testData$EntityAttr ) %>%
  ERTMonSetComputationSpecification( testData$ComputationSpecification ) %>% 
  ERTMonProcessEventRecords( alignmentSpec = "MinTime" )

cMats <- ertmon0 %>% ERTMonTakeContingencyMatrices

## Direct formula computation.
cMatRes <- cMats$Var.1.Mean^1.5 / ( 2 * cMats$Var.2.Mean + 3 * cMats$Var.3.Mean^2 ) + cMats$Var.4.Mean / cMats$Var.5.Mean
  
ertmon0 <- ertmon0 %>% ERTMonComputeFormula( formulaSpec = formulaSpecDF, reduceFunc = "+" )

formulaMat <- as.matrix( ertmon0 %>% ERTMonTakeValue )

## We get a matrix.
test_that("Formula matrix", {
  expect_equal( mean( formulaSpecDF$FeatureName %in% names(cMats) ), 1 )
  expect_type( formulaMat, "matrix" )
})

## Compare with alternative computations.
test_that("Comparison with a direct formula computation", {
  expect_true( max(abs(cMatRes - formulaMat)) < 1.0E-8 )
})
