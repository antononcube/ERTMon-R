context("Random pipelines")
library(ERTMon)

set.seed(342)

numberOfPipelines <- 30

testData <- ERTMonSimpleTestData( numberOfEntities = 4, numberOfVariables = 5, 
                                  timeInterval = 900, numberOfTimeCells = 12, randomStartTimesQ = T, 
                                  variableFunction = "random", exportDirectoryName = NULL)

testData2 <- ERTMonSimpleTestData( numberOfEntities = 2, numberOfVariables = 5, 
                                   timeInterval = 900, numberOfTimeCells = 12, randomStartTimesQ = T, 
                                   variableFunction = "random", exportDirectoryName = NULL)

formulaSpecDF <- 
  data.frame( 
    "FormulaID" = c(1),
    "TermID" = c(1,1,1,2,2), 
    "TermCoefficient" = c( 0.5, 0.5, 0.5, 1.2, 1.2),
    "FeatureName" = c("Var.1.Mean", "Var.2.Mean", "Var.3.Mean", "Var.4.Mean", "Var.5.Mean"), 
    "ReduceFunction" = c("+"),
    "Coefficient" = c(1, 2, 3, 1, 1), 
    "Exponent" = c(1.5, 1, 2, 1, 1),
    "RatioPart" = c( "Numerator", "Denominator", "Denominator", "Numerator", "Denominator"),
    stringsAsFactors = FALSE)


pipelineLevels <-
  list( level1 = c( "ERTMonUnit()", 
                    "ERTMonUnit( testData$EventRecords, testData$EntityAttributes, testData$ComputationSpecification )"),
        level2 = c( "ERTMonSetEventRecords(testData$EventRecords)",
                    "ERTMonSetEntityAttributes(testData$EntityAttributes)",
                    "ERTMonSetComputationSpecification(testData$ComputationSpecification)" ),
        level3 = c( "ERTMonProcessEventRecords()", 
                    "ERTMonProcessEventRecords( alignmentSpec = 'MinTime' )", 
                    "ERTMonProcessEventRecords( alignmentSpec = min(testData$EventRecords$ObservationTime) )" ),
        level4 = c( "ERTMonExtractFeatures( testData2$EventRecords, testData2$EntityAttributes, testData2$ComputationSpecification )", 
                    "ERTMonComputeFormula(formulaSpecDF)" ) 
  )

randomPipelines <- 
  purrr::map( 1:numberOfPipelines, 
              function(x) { 
                rp <- Reduce( function(a,x) { c( a, sample(x,1) )}, init = c(), x = pipelineLevels ) 
                parse( text = paste( rp, collapse = " %>% " ))
              })

ertRes <- purrr::map( randomPipelines, purrr::safely(eval))

checkRes <- purrr::map_lgl( ertRes, function(x) is.na(x$result) || is.list(x$result) && class(x$result) == "ERTMon" )

test_that( "Random pipelines produce NA's or ERTMon S3 objects.", {
  expect_true( sum(is.na(checkRes)) == 0 && mean(checkRes) == 1 )
})
