########################################################################
# Script Karajaulas UI
#install.packages("devtools")
#devtools::install_github("rstudio/shiny-incubator")
# css bootstrap: http://bootswatch.com/flatly/
########################################################################
require(Biobase)
library(shiny)
library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)

shinyUI
(     fluidPage( 
  
      theme = "bootstrap.css",
      includeCSS("www/omics.css"),
      includeScript("www/waiting.js"),

      navbarPage(   
                    title="Omics-in-Cloud",
                    
                    windowTitle="Omics-in-Cloud",
                    
                    collapsible=TRUE,
                    
                    # Home
                    tabPanel("Home", id = "home",
                             icon = icon("fa fa-cloud"),
                             mainPanel(
                               h4("Página principal")
                             )
                             ),
                    
                    # Catalog
                    tabPanel("Data Catalog", icon = icon("fa fa-th"),
                             navlistPanel(
                               "Data Catalog", widths = c(2, 10),
                               tabPanel("ICO data", id = "icodata"),
                               
                               tabPanel("Other data", id = "otherdata",
                                        sidebarLayout(
                                          sidebarPanel(
                                            # select dataset
                                            uiOutput("choose_dataset"),
                                            # select platform
                                            uiOutput("choose_platform")
                                          ),
                                          mainPanel (

                                          )
                                        )
                               )
                             )
                    ),
                    
                    # Load Data
                    tabPanel("Load Data", icon = icon("fa fa-database"),
                              navlistPanel(
                                "Load Data", widths = c(2, 10),
                                tabPanel("NCBI", icon = icon("fa fa-cloud-download"), id = "ncbi",
                                        sidebarLayout(
                                          sidebarPanel(textInput("experimentupload", label = h3("Experiment name"), value = "Enter text..."),
                                                       submitButton("Download experiment")),
                                          mainPanel(
                                            tags$div(class = "waiting", p("Progress.."), img(src="img/loader.gif")),
                                            dataTableOutput("experiment")
                                          )
                                        )
                                ),
                                tabPanel("ICO", icon = icon("fa fa-cloud-upload"), id = "ico",
                                        sidebarLayout(
                                          sidebarPanel(fileInput('file1', h3('Choose file to upload'))),
                                          mainPanel(
                                            tags$div(class = "waiting", p("Progress.."), img(src="img/loader.gif")),
                                            dataTableOutput('contents')
                                          )
                                        )
                                    )
                              )
                            ),
                              
                    # Análisis
                    tabPanel("Analysis", icon = icon("fa fa-bar-chart"),
                              navlistPanel(
                                "Analysis", widths = c(2, 10),
                                tabPanel("Anal1", id = "anal1"),
                                tabPanel("Anal2", id = "anal2")
                              )
                    ),
                   
                    # Ayuda
                    tabPanel("Help", 
                             icon = icon("fa fa-question"), id = "help"
                    )
                    
      )
  ) # final fluidPage
)

