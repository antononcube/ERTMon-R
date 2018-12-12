context("Pipeline from directory")
library(ERTMon)

dirName <- if( grepl("testthat", getwd()) ) { 
  file.path( getwd(), "..", "..", "data", "FakeData") 
} else { 
  file.path( getwd(), "..", "data", "FakeData")
}

ertmon0 <-
  ERTMonUnit() %>%
  ERTMonReadDataFromDirectory( dirName )

ertmon0 <- ertmon0 %>% ERTMonProcessEventRecords()

ertmon1 <-
  ERTMonUnit() %>%
  ERTMonReadComputationSpecification( file.path( dirName, "computationSpecification.csv" ) ) %>% 
  ERTMonReadDataFromDirectory( dirName, readCompSpecQ = FALSE )

ertmon1 <- ertmon1 %>% ERTMonProcessEventRecords()
  
smat0 <- ertmon0 %>% ERTMonTakeFeatureMatrix()
smat1 <- ertmon1 %>% ERTMonTakeFeatureMatrix()

test_that("Same feature matrices", {
  expect_equal( as.matrix(smat0), as.matrix(smat1) )
}) 