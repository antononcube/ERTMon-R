##===========================================================
## ERTMon-R dashboard server 
##
## LGPL-3.0 License, see https://www.gnu.org/licenses/lgpl-3.0.txt
## 
## Copyright (c) 2019, Anton Antonov
## All rights reserved.
##
##===========================================================

#' @import shinydashboard
#' @import DT
#' @import shiny
NULL


##===========================================================
## UI function
##===========================================================

#' Event records transformation dashboard user interface
#' @description Creates the Shiny user interface function for an 
#' event records transformations interface. 
#' @param dataDirectoryName A directory name with data.
#' @param testDataDirectoryName A directory name with test data.
#' @param compSpecFileName A file name of computation specification; 
#' (a CSV file).
#' @param exportModelID A string with a model ID to be used for export.
#' @param exportFilePrefix A string with the prefix to be used for 
#' the export model files.
#' @return Shiny UI object.
#' @family Interface functions
#' @export
ERTMonMakeUI <- function( dataDirectoryName = "./", 
                          testDataDirectoryName = "./", 
                          compSpecFileName = file.path( dataDirectoryName, "computationSpecification.csv" ),
                          exportModelID = as.character( Sys.time() ),
                          exportFilePrefix = gsub( pattern = " ", replacement = "_", paste0( exportModelID, "_" ), fixed = TRUE)) {
  
  dashboardPage(
    dashboardHeader(title = "Event records transformation dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Computation specifications", tabName = "CompSpec"),
        menuItem("Data", tabName = "Data"),
        menuItem("Plots distributions", tabName = "PlotDistributions"),
        menuItem("Plot feature matrix", tabName = "PlotFeatureMatrix"),
        menuItem("Export feature matrix", tabName = "ExportFeatureMatrix")
      )
    ),
    dashboardBody(
      
      tabItems(
        tabItem( tabName = "CompSpec",
                 
                 h2("Computation specifications"),
                 
                 fluidPage(
                   
                   column(12,
                          
                          textInput( "inSpecFileName", 
                                     label = "File name to read:", 
                                     value = compSpecFileName, 
                                     placeholder = "Name of a CSV file with specifications to read"),
                          
                          verbatimTextOutput("inSpecFileName"),
                          
                          # textInput( "outSpecFileName", label = "File name to save:", value = "", placeholder = "Name of a CSV file with specifications to write"),
                          # 
                          # verbatimTextOutput("outSpecFileName"),
                          
                          fluidRow(
                            # h3("Table options"),
                            column(3, actionButton( "readSpecAction", "Read specification") ),
                            column(3, actionButton( "saveSpecAction", "Save table") ),
                            column(3, radioButtons( "saveType", label = "Save file type", choices = c("CSV" = "csv", "RDS" = "rds"), selected = "csv") ),
                            column(3, radioButtons( "useType", label = "Use data types", choices = c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE") )
                          ),
                          
                          helpText("After the data is read and ingested",
                                   "the data is transformed using data specifications."),
                          
                          DT::dataTableOutput("compSpecDF")
                   ))
        ),
        
        tabItem( tabName = "Data",
                 
                 h2("Data reading, ingestion, and transformation"),
                 
                 fluidPage(
                   tags$head(
                     tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                   ),
                   
                   tabsetPanel(
                     
                     tabPanel("Training data",
                              
                              column(12,
                                     h3("Training data"),
                                     
                                     textInput( "dataDirName", label = "Data directory name:", 
                                                value = dataDirectoryName, placeholder = "directory name with data files"),
                                     
                                     verbatimTextOutput("dataDirName"),
                                     
                                     fluidRow(
                                       column(3, actionButton( "readDataAction", "Read data")),
                                       column(3, actionButton( "transformDataAction", "Transform data"))
                                     ),
                                     
                                     helpText("Press the buttons for the different stages."),
                                     hr(),
                                     
                                     # DT::dataTableOutput("summaryData")
                                     h4( "Event records summary" ),
                                     verbatimTextOutput("eventRecordsDataSummary"),
                                     hr(),
                                     
                                     h4( "Entity attributes summary" ),
                                     verbatimTextOutput("entityAttributesDataSummary"),
                                     hr(),
                                     
                                     h4( "Transformed data summary" ),
                                     verbatimTextOutput("transformedDataSummary")
                                     
                              )
                     ),
                     
                     tabPanel("Testing data",
                              
                              column(12,
                                     h3("Test data"),
                                     
                                     textInput( "testDataDirName", label = "Testing data directory name:", 
                                                value = testDataDirectoryName, placeholder = "directory name with test data files"),
                                     
                                     verbatimTextOutput("testDataDirName"),
                                     
                                     fluidRow(
                                       column(3, actionButton( "readTestDataAction", "Read test data")),
                                       column(3, actionButton( "transformTestDataAction", "Transform test data")),
                                       column(3, actionButton( "removeTestDataAction", "Remove test data"))
                                     ),
                                     
                                     helpText("Press the buttons for the different stages."),
                                     hr(),
                                     
                                     # DT::dataTableOutput("summaryData")
                                     h4( "Event records summary" ),
                                     verbatimTextOutput("eventRecordsTestDataSummary"),
                                     hr(),
                                     
                                     h4( "Entity attributes summary" ),
                                     verbatimTextOutput("entityAttributesTestDataSummary"),
                                     hr(),
                                     
                                     h4( "Transformed data summary" ),
                                     verbatimTextOutput("transformedTestDataSummary")
                                     
                              )
                     )
                     
                   )
                 )
        ),
        
        tabItem( tabName = "PlotDistributions",
                 
                 h2("Plot distributions"),
                 
                 hr(), 
                 
                 tabsetPanel( 
                   tabPanel( "Variables distributions",
                             h3("Variables distributions"),
                             
                             fluidRow(
                               column(3, actionButton( "plotVariablesDistributionsAction", "Plot variables distributions"))
                             ),
                             
                             plotOutput( "plotVariablesDistributions" )
                   ),
                   
                   tabPanel( "Feature types distributions",          
                             h3("Feature types distributions"),
                             
                             fluidRow(
                               column(3, actionButton( "plotMatrixNameDistributionsAction", "Plot feature types distributions"))
                             ),
                             
                             plotOutput( "plotMatrixNameDistributions" )
                   )
                 )      
        ),
        
        tabItem( tabName = "PlotFeatureMatrix",
                 
                 h2("Plot the feature matrix"),
                 
                 textInput( "featureNamesPattern", "Feature names pattern:",  value = ".*"),
                 
                 hr(), 
                 
                 tabsetPanel( 
                   tabPanel( "Sparse matrix image",
                             h3("Image of the feature matrix"),
                             
                             fluidRow(
                               column(3, actionButton( "plotFeatureMatrixImageAction", "Plot feature matrix image"))
                             ),
                             
                             plotOutput( "plotFeatureMatrixImage" )
                   ),
                   
                   tabPanel( "Heatmap",          
                             h3("Heatmap plot of the feature matrix"),
                             
                             fluidRow(
                               column(3, actionButton( "plotFeatureMatrixHeatmapAction", "Plot heatmap"))
                             ),
                             
                             d3heatmapOutput( "plotFeatureMatrixHeatmap", width = "100%", height = "600px" )
                   )
                 )      
        ),
        
        tabItem( tabName = "ExportFeatureMatrix",
                 
                 column(12,
                        
                        h2("Export the feature matrix"),
                        
                        textInput( "exportDirName", label = "Export directory name:", 
                                   value = dataDirectoryName, placeholder = "directory name for the CSV export files"),
                        
                        textInput( "exportModelID", label = "A string to be used as a model ID:", 
                                   value = exportModelID, placeholder = "export model ID"),
                        
                        textInput( "exportPrefix", label = "Export-files name prefix:", 
                                   value = exportFilePrefix, placeholder = "prefix"),
                        
                        verbatimTextOutput("exportDirName"),
                        
                        hr(), 
                        
                        fluidRow(
                          column(3, actionButton( "exportDataAction", "Export data"))
                        ),
                        
                        helpText("Press the button for export."),
                        hr(),
                        
                        h4( "Computation specification summary" ),
                        verbatimTextOutput("exportedCompSpecSummary"),
                        hr(),
                        
                        h4( "Feature matrix summary" ),
                        verbatimTextOutput("exportedFeatureMatrixSummary"),
                        hr(),
                        
                        h4( "Time grid cells interpretation summary" ),
                        verbatimTextOutput("exportedTimeGridCellsInterpretationSummary")
                        
                 )
        )
        
      )
    )
  )
}


##===========================================================
## Server function
##===========================================================

#' Event records transformation server function
#' @description Creates the Shiny server function for an 
#' event records transformations interface
#' @return Shiny server function
#' @family Interface functions
#' @export
ERTMonMakeServerFunction <- function() {
  
  
  shinyServer( function(input, output, session)  {
    
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
    
    ##-------------------------------------------------------
    ## Export of the feature matrix
    ##-------------------------------------------------------
    
    observeEvent( input$exportDataAction, {
      
      if( file.exists( input$exportDirName ) ) {
        
        values$ertObj <- 
          values$ertObj %>% 
          ERTMonExport( directoryName = input$exportDirName, modelID = input$exportModelID, fileNamePrefix = input$exportPrefix  )
        
        exportedData <- values$ertObj %>% ERTMonTakeValue
        
        compSpecDF <- exportedData$ComputationSpecification
        featureMatrixDF <- exportedData$FeatureMatrix
        tciDF <- exportedData$TimeCellsInterpretation
        
        output$exportedCompSpecSummary <- renderPrint({ summary( as.data.frame( unclass( compSpecDF ) ) ) })
        
        output$exportedFeatureMatrixSummary <- renderPrint({ summary( as.data.frame( unclass( featureMatrixDF ) ) ) })
        
        output$exportedTimeGridCellsInterpretationSummary <- renderPrint({ summary( as.data.frame( unclass( tciDF ) ) ) })
        
      } else {
        
        warning( paste0( "The directory input$exportDirName does not exist: input$exportDirName = \"" , input$exportDirName, "\" ." ) )  
        
      }
      
    })
    
  })
}


##===========================================================
## Make shiny app
##===========================================================

#' Event records transformation dashboard
#' @description Creates the Shiny application with an 
#' event records transformations interface. 
#' @param dataDirectoryName A directory name with data.
#' @param testDataDirectoryName A directory name with test data.
#' @param compSpecFileName A file name of computation specification; 
#' (a CSV file).
#' @param exportModelID A string with a model ID to be used for export.
#' @param exportFilePrefix A string with the prefix to be used for 
#' the export model files.
#' @return Shiny app.
#' @family Interface functions
#' @export
ERTMonCreateInterface <- function( dataDirectoryName = "./", 
                                   testDataDirectoryName = "./", 
                                   compSpecFileName = file.path( dataDirectoryName, "computationSpecification.csv" ),
                                   exportModelID = as.character( Sys.time() ),
                                   exportFilePrefix = gsub( pattern = " ", replacement = "_", paste0( exportModelID, "_" ), fixed = TRUE)) {
  

  shiny::shinyApp( ui = ERTMonMakeUI( dataDirectoryName = dataDirectoryName, 
                                      testDataDirectoryName = testDataDirectoryName, 
                                      compSpecFileName = compSpecFileName, 
                                      exportModelID = exportModelID, 
                                      exportFilePrefix = exportFilePrefix),
                   server = ERTMonMakeServerFunction()
  )
  
}


