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
      
      ertMonCS <- ERTMonUnit() %>% ERTMonReadComputationSpecification( input$inSpecFileName )

      values$compSpec <- ertMonCS %>% ERTMonTakeComputationSpecification
      
      output$compSpecDF <- DT::renderDataTable({ datatable({
        values$compSpec
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
      
      ertObj <- 
        ERTMonUnit() %>% 
        ERTMonReadDataFromDirectory( input$dataDirName )
      
      values$ertObj <- ertObj
      
      output$eventRecordsDataSummary <- renderPrint({ summary( as.data.frame( unclass( ertObj %>% ERTMonTakeEventRecords ) ) ) })
      
      output$entityAttributesDataSummary <- renderPrint({ summary( as.data.frame( unclass( ertObj %>% ERTMonTakeEntityAttributes ) ) ) })
      
    } else {
      
      warning( paste0( "The directory input$dataDirName does not exist: input$dataDirName = \"" , input$dataDirName, "\" ." ) )  
      
    }
    
  })
  
  ## Transform event records data 
  observeEvent( input$transformDataAction, {
    
    if ( !is.null( values$ertObj ) && !is.null( values$compSpec ) ) {
      
      progress <- shiny::Progress$new()
      progress$set(message = "Transform data", value = 0)
      on.exit(progress$close())
      
      values$ertObj <-
        values$ertObj %>% 
        ERTMonProcessEventRecords(echoStepsQ = TRUE, progressObject = progress )
      
      transformedData <-  ERTMonTakeTrasformedData(values$ertObj)
        
      output$transformedDataSummary <- renderPrint({ summary( as.data.frame( unclass( transformedData ) ) ) })

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
      
      ertTestObj <- 
        ERTMonUnit() %>% 
        ERTMonReadDataFromDirectory( input$dataDirName )
      
      values$ertTestObj <- ertTestObj
      
      output$eventRecordsTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( ertTestObj %>% ERTMonTakeEventRecords ) ) ) })
      
      output$entityAttributesTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( ertTestObj %>% ERTMonTakeEntityAttributes ) ) ) })
      
      progress
    } 
    
  })
  
  ## Transform test event records data 
  observeEvent( input$transformTestDataAction, {
    
    if( is.null(values$dtObj) ) {
      
      warning( paste0( "Transform the training data first." ) )  
      
    } else if ( !is.null( values$ertObj ) && 
                ERTMonDataTransformerCheck(values$ertObj, functionName = "server.R::transformTestDataAction", logicalResult = T) ) {
      
      progress <- shiny::Progress$new()
      progress$set(message = "Transform test data", value = 0)
      on.exit(progress$close())
      
      values$ertTestObj <- values$ertObj
      
      values$ertTestObj <-
        values$ertTestObj %>% 
        ERTMonExtractFeatures(echoStepsQ = TRUE, progressObject = progress )
      
      transformedData <- ERTMonTakeTrasformedData(values$ertTestObj)
      
      output$transformedTestDataSummary <- renderPrint({ summary( as.data.frame( unclass( transformedData ) ) ) })
      
      progress
    } else {
      
      warning( "Missing testing data." )
    }
    
  })
  
  ## Remove test data 
  observeEvent( input$removeTestDataAction, {
    
    values[["ertTestObj"]] <- NULL
    
    output$eventRecordsTestDataSummary <- renderPrint({ NULL })
    output$entityAttributesTestDataSummary <- renderPrint({ NULL })
    output$transformedTestDataSummary <- renderPrint({ NULL })
    
  })
  
  ##-------------------------------------------------------
  ## Plots distributions
  ##-------------------------------------------------------
  
  ## Plot variables distributions
  observeEvent( input$plotVariablesDistributionsAction, {
    
    if ( !is.null( values[["ertObj"]] ) ) {
      
      output$plotVariablesDistributions <- renderPlot({ 
        ggplot( values$ertObj %>% ERTMonTakeEventRecords ) + geom_histogram( aes(x=Value) ) + facet_wrap( ~Variable, scales = "free" )
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })
  
  ## Plot matrix names aggregated values distrubutions
  observeEvent( input$plotMatrixNameDistributionsAction, {
    
    if ( !is.null( values[["ertObj"]] ) ) {
      
      output$plotMatrixNameDistributions <- renderPlot({ 
        ggplot( values$ertObj %>% ERTMonTakeTrasformedData ) + geom_histogram( aes(x=AValue) ) + facet_wrap( ~MatrixName, scales = "free" )
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })
  
  ##-------------------------------------------------------
  ## Plots of the feature matrix
  ##-------------------------------------------------------
  
  ## Sparse matrix image for the feature matrix
  observeEvent( input$plotFeatureMatrixImageAction, {
    
    if ( !is.null( values[["ertObj"]] ) ) {
      
      output$plotFeatureMatrixImage <- renderPlot({ 
        image( values[["ertObj"]] %>% ERTMonTakeFeatureMatrix )
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })
  
  ## Heatmap for the feature matrix
  observeEvent( input$plotFeatureMatrixImageAction, {
    
    if ( !is.null( values[["ertObj"]] ) ) {
      
      output$plotFeatureMatrixHeatmap <- renderPlot({ 
        renderD3heatmap(d3heatmap( values[["ertObj"]] %>% ERTMonTakeFeatureMatrix ))
      })
      
    } else {
      
      warning( "Missing data transformation object for testing data." )
    }
    
  })

}


