##===========================================================
## ERTMon-R dashboard ui 
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

dashboardPage(
  dashboardHeader(title = "Event records transformation dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Computation specifications", tabName = "CompSpec"),
      menuItem("Data", tabName = "Data"),
      menuItem("Plot feature matrix", tabName = "PlotFeatureMatrix")
      #menuItem("Export feature matrix", tabName = "ExportFeatureMatrix")
    )
  ),
  dashboardBody(

    tabItems(
      tabItem( tabName = "CompSpec",
               
               h2("Computation specifications"),
               
               fluidPage(
                 
                 column(12,
                        
                        textInput( "inSpecFileName", label = "File name to read:", value = specFileName, placeholder = "Name of a CSV file with specifications to read"),
                        
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
                                              value = directoryName, placeholder = "directory name with data files"),
                                   
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
      )
      
    )
  )
)

