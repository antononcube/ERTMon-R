##===========================================================
## ERTMon-R dashboard ui 
## Copyright (C) 2018  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
      menuItem("Data", tabName = "Data")
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
      )
      
    )
  )
)

