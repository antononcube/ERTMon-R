##===========================================================
## ERTMon-R dashboard server 
##
## BSD 3-Clause License
## 
## Copyright (c) 2018, Anton Antonov
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
## 
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
## 
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
## 
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
##          SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

library(shinydashboard)
library(DT)
#library(shiny)

function(input, output, session)  {
  ##-------------------------------------------------------
  ## Reactive evalutions
  ##-------------------------------------------------------
  
  values <- reactiveValues()
  
  ## Data directory name
  dataDirName <- reactive({
    if ( !VerifyDataDirectory(input$dataDirName) ) { NA }
    else { input$dataDirName } 
  })
  
  
  ## Editable table
  # observe({
  #   if (!is.null(input$hotCompSpec)) {
  #     compSpecDF = hot_to_r(input$hotCompSpec)
  #   } else {
  #     if (is.null(values[["compSpecDF"]]))
  #       compSpecDF <- compSpecObj@parameters
  #     else
  #       compSpecDF <- values[["compSpecDF"]]
  #   }
  #   values[["compSpecDF"]] <- compSpecDF
  # })
  
  ##-------------------------------------------------------
  ## Output rendering
  ##-------------------------------------------------------
  output$inSpecFileName <- renderText({ input$inSpecFileName })
  
  output$outSpecFileName <- renderText({ input$outSpecFileName })
  
  output$dataDirName <- renderText({ input$dataDirName })
  
  output$testDataDirName <- renderText({ input$testDataDirName })
  
  # output$hotCompSpec <- renderRHandsontable({
  #   compSpecDF <- values[["compSpecDF"]]
  #   if (!is.null(compSpecDF))
  #     rhandsontable(compSpecDF, useTypes = as.logical(input$useType), stretchH = "all")
  # })
  
  ##-------------------------------------------------------
  ## Computations specification
  ##-------------------------------------------------------
  
  ## Read specification table 
  observeEvent( input$readSpecAction, {
    
    if( file.exists( input$inSpecFileName ) ) {
      
      compSpecObj <- compSpecObj %>% readSpec( input$inSpecFileName ) %>% ingestSpec()
      
      values$compSpecObj <- compSpecObj
      
      output$compSpecDF <- DT::renderDataTable({ datatable({
        compSpecObj@parameters
      }, rownames = FALSE, filter = 'top', options = list(pageLength = 20, autoWidth = FALSE) ) })
      
    }
    
  })
  
  ##-------------------------------------------------------
  ## Training data
  ##-------------------------------------------------------
  
  ## Read event records data 
  observeEvent( input$readDataAction, {
    
    if( file.exists( input$dataDirName ) ) {
      
      ## This has to be refactored.
      
      progress <- shiny::Progress$new()
      progress$set(message = "Read from directory", value = 0)
      on.exit(progress$close())
      
      diObj@progressObject <- progress
      
      diObj <- diObj %>% readDataFromDirectory( input$dataDirName ) %>% ingestData( "Label" )
      
      ## Assignment of the labels according to specification
      ccLabel <- values$compSpecObj@parameters[ values$compSpecObj@parameters$Variable == "Label", "Critical.label"]
      
      values$compSpecObj@diedLabel <- ccLabel
      values$compSpecObj@survivedLabel <- setdiff( diObj@dataObj@labels, ccLabel )
      values$compSpecObj@labels <- c( DiedLabel = values$compSpecObj@diedLabel, SurvivedLabel = values$compSpecObj@survivedLabel)
      
      assertthat::assert_that( ccLabel %in% diObj@dataObj@labels )
      
      diObj@dataObj@diedLabel <- values$compSpecObj@diedLabel
      diObj@dataObj@survivedLabel <- values$compSpecObj@survivedLabel
      diObj@dataObj@labels <- values$compSpecObj@labels
      
      assertthat::assert_that( length(diObj@dataObj@survivedLabel) == 1 )
      
      assertthat::assert_that( mean( c(diObj@dataObj@diedLabel, diObj@dataObj@survivedLabel) %in% diObj@dataObj@labels ) == 1 )
      
      values$diObj <- diObj
      
      ## It is potentially slow to calculate summaries
      # output$summaryData <- DT::renderDataTable({ datatable({
      #     summary( diObj@dataObj@eventRecordsData )
      # }, rownames = FALSE, filter = 'top', options = list(pageLength = 20, autoWidth = FALSE) ) })
      # 
      
      output$eventRecordsDataSummary <- renderPrint({ summary( as.data.frame( unclass( diObj@dataObj@eventRecords ) ) ) })
      
      output$entityAttributesDataSummary <- renderPrint({ summary( as.data.frame( unclass( diObj@dataObj@entityAttributes ) ) ) })
      
    } else {
      
      warning( paste0( "The directory input$dataDirName does not exist: input$dataDirName = \"" , input$dataDirName, "\" ." ) )  
      
    }
    
  })
  
  ## Transform event records data 
  observeEvent( input$transformDataAction, {
    
    if ( !is.null( values$compSpecObj ) && !is.null( values$compSpecObj@parameters ) && 
         !is.null( values$diObj ) && !is.null( values$diObj@dataObj ) ) {
      
      progress <- shiny::Progress$new()
      progress$set(message = "Transform data", value = 0)
      on.exit(progress$close())
      
      dtObj <- new( "DataTransformer" )
      
      dtObj@compSpec <- values[["compSpecObj"]]
      
      dtObj@progressObject <- progress
      
      dtObj <- transformData( dtObj, values$compSpecObj, values$diObj@dataObj@eventRecords, values$diObj@dataObj@entityAttributes )
      
      # output$summaryData <- DT::renderDataTable({ datatable({
      #   summary( dtObj@transformedData )
      # }, rownames = FALSE, filter = 'top', options = list(pageLength = 20, autoWidth = FALSE) ) })
      # 
      
      values[["dtObj"]] <- dtObj
  
      values[["dtCatObj"]] <- as( dtObj, "DataTransformerCatMatrices")
      
      output$transformedDataSummary <- renderPrint({ summary( as.data.frame( unclass( dtObj@transformedData ) ) ) })
      
      progress
    } else {
      
      warning( "Missing data." )
    }
    
  })
  
  ##-------------------------------------------------------
  ## Testing data
  ##-------------------------------------------------------
  
  ## Read event records data 
  observeEvent( input$readTestDataAction, {
    
    if( is.null(values$diObj) || is.null(values$diObj@dataObj) ) {
      
      warning( paste0( "Read training data first." ) )  
      
    } else if( !file.exists( input$testDataDirName ) ) {
      
      warning( paste0( "The directory input$testDataDirName does not exist: input$testDataDirName = \"" , input$testDataDirName, "\" ." ) )  
      
    } else {
      
      progress <- shiny::Progress$new()
      progress$set(message = "Read test data from directory", value = 0)
      on.exit(progress$close())
      
      if( is.null(diTestObj) ) {
        diTestObj <- new( "DataIngester" ) 
      }
      
      diTestObj@progressObject <- progress
      
      diTestObj <- diTestObj %>% readDataFromDirectory( input$dataDirName ) %>% ingestData( "Label" )
      
      values$diTestObj <- diTestObj
      
      assertthat::assert_that( diTestObj@dataObj@labels[[1]] == values$diObj@dataObj@labels[[1]] )
      assertthat::assert_that( diTestObj@dataObj@labels[[2]] == values$diObj@dataObj@labels[[2]] )
      
      
      output$eventRecordsTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( diTestObj@dataObj@eventRecordsData ) ) ) })
      
      output$entityAttributesTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( diTestObj@dataObj@entityAttributesData ) ) ) })
      
    } 
    
  })
  
  ## Transform event records data 
  observeEvent( input$transformTestDataAction, {
    
    if( is.null(values$dtObj) ) {
      
      warning( paste0( "Transform the training data first." ) )  
      
    } else if ( !is.null( values$compSpecObj ) && !is.null( values$compSpecObj@parameters ) && 
                !is.null( values$diTestObj ) && !is.null( values$diTestObj@dataObj ) ) {
      
      progress <- shiny::Progress$new()
      progress$set(message = "Transform test data", value = 0)
      on.exit(progress$close())
      
      dtObj <- values[["dtObj"]]
      
      ## The following object should be already assigned, and it is not needed.
      #dtObj@compSpec <- values[["compSpecObj"]]
      
      dtObj@progressObject <- progress
      
      dtObj <- transformData( dtObj, values$compSpecObj, values$diObj@dataObj@eventRecordsData, values$diObj@dataObj@entityAttributesData, testDataRun = TRUE )
      
      # output$summaryData <- DT::renderDataTable({ datatable({
      #   summary( dtObj@transformedData )
      # }, rownames = FALSE, filter = 'top', options = list(pageLength = 20, autoWidth = FALSE) ) })
      # 
      
      ## Note that this object is probably a shallow copy of values[["dtObj"]] :
      values[["dtTestObj"]] <- dtObj
      
      output$transformedTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( dtObj@transformedData ) ) ) })
      
      progress
    } else {
      
      warning( "Missing testing data." )
    }
    
  })
  
  ## Remove test data 
  observeEvent( input$removeTestDataAction, {
    
    values[["diTestObj"]] <- NULL
    values[["dtTestObj"]] <- NULL
    
    output$eventRecordsTestDataSummary <- renderPrint({ NULL })
    output$entityAttributesTestDataSummary <- renderPrint({ NULL })
    output$transformedTestDataSummary <- renderPrint({ NULL })
    
  })
  
  ##-------------------------------------------------------
  ## Plots of the feature matrix
  ##-------------------------------------------------------
  
  ## Sparse matrix image for the feature matrix
  observeEvent( input$plotFeatureMatrixImageAction, {
    
    if ( !is.null( values[["dtObj"]] ) ) {
      
      output$plotFeatureMatrixImage <- renderPlot({ 
        image(values[["dtObj"]]@dataMat)
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })
  
  ## Heatmap for the feature matrix
  observeEvent( input$plotFeatureMatrixImageAction, {
    
    if ( !is.null( values[["dtObj"]] ) ) {
      
      output$plotFeatureMatrixHeatmap <- renderPlot({ 
        renderD3heatmap(d3heatmap(values[["dtObj"]]@dataMat))
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })
}


