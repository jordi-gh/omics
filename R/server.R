require(Biobase)
library(shiny)
library(shinyjs)
library(shiny)
library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)

# el usuario texto normal, el password esta en md5 p.ej. la b -> 92eb5ffee6ae2fec3ad71c777531578f
credentials <- list("rpena" = "92eb5ffee6ae2fec3ad71c777531578f","dmendez" = "92eb5ffee6ae2fec3ad71c777531578f","jtorresz" = "92eb5ffee6ae2fec3ad71c777531578f")

## Carregar variables d'entorn local
source('ConfigLocal.R')
source(file.path(gb_Rdir, 'IncRel.R'))
options(shiny.maxRequestSize=500*1024^2)

shinyServer(function(input, output) {
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
        wellPanel(id = "login",
          textInput(".username", "Username:"),
          passwordInput(".password", "Password:"),
          div(actionButton(".login", "Log in"), style="text-align: center;")
        ),
        textOutput("message")
      ))
    } else {
      
      # ---------------------------------------------------------------------  
      # TODA LA INTERFICIE AQUÍ
      # ---------------------------------------------------------------------  
      fluidPage( 
        
        theme = "bootstrap.css",
        includeCSS("www/omics.css"),
        includeScript("www/waiting.js"),
        
        navbarPage(
          
          div(icon("fa fa-user"), input$.username),
          
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
                                  textInput("searchexperiment", label = h3("Search"), value = "Enter text..."),
                                  # select dataset
                                  uiOutput("choose_dataset"),
                                  # select platform
                                  uiOutput("choose_platform")
                                ),
                                
                                mainPanel(
                                  textOutput("salidaprueba")
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
                                             actionButton("submitexperiment", "Download experiment")),
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
      #----------------------------------------------
      # FIN INTERFICIE
      #----------------------------------------------
    }

  )
  
  #---------------------------------------------------------------------
  #                                                   ------------------
  # FUCIONALIDADES APLICACIÓN                         ------------------
  #                                                   ------------------
  #---------------------------------------------------------------------
  
  # ---------------------------------------------------------------------  
  # Upload ICO
  # ---------------------------------------------------------------------  
  output$contents <- renderDataTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    objGEO <- getGEO(filename=inFile$datapath)
    
    guardaFitxer(objGEO,inFile$name,inFile$datapath)
    
    if (length(Table(objGEO))==0){
      return(NULL)
    }
    Table(objGEO)[,1:2]
    
  })
  
  # ---------------------------------------------------------------------
  # Upload NCBI
  # ---------------------------------------------------------------------
  output$experiment <- renderDataTable({ 
    
    if (input$experimentupload!="Enter text..." && input$experimentupload!=""){
      destdir = file.path(gb_Rdir, 'BD')
      
      ExperimentNCBI <- getGEO(input$experimentupload, destdir = destdir)
      Table(ExperimentNCBI)[,1:2]
      
    }
    
  })
  
  # ---------------------------------------------------------------------
  # Data Catalog NCBI
  # ---------------------------------------------------------------------
  # Select inicial de DataSets
  output$choose_dataset <- renderUI({
    
    if (is.null(input$searchexperiment) || input$searchexperiment == "Enter text..."){
      return()
    }
    else{
      if(!file.exists(gb_geoSQLFile)) { getSQLiteFile() }
      con <- dbConnect(SQLite(),gb_geoSQLFile)
      sql <- paste("SELECT DISTINCT gse.gse",
                   "FROM",
                   " gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
                   " JOIN gse ON gse_gsm.gse=gse.gse",
                   " JOIN gse_gpl ON gse_gpl.gse=gse.gse",
                   " JOIN gpl ON gse_gpl.gpl=gpl.gpl",
                   "WHERE",
                   " gse.title LIKE '%",input$searchexperiment,"%'", sep=" ")
      rs <- dbGetQuery(con,sql)
      data_sets <- as.matrix(rs)
      dbDisconnect(con)
      selectInput("dataset", "GSE", c('Select DataSet',as.list(data_sets)))
    }
    
  })
  
  # Select inicial de Platform (una vez seleccionado DataSets)
  output$choose_platform <- renderUI({
    
    if (is.null(input$dataset) || input$dataset == "Select DataSet"){
      return()
    }
    else{
      if(!file.exists(gb_geoSQLFile)) { getSQLiteFile() }
      con <- dbConnect(SQLite(),gb_geoSQLFile)
      sql <- paste("SELECT gds FROM gds WHERE gse = '",input$dataset,"'", sep="")
      rs <- dbGetQuery(con,sql)
      data_sets <- as.matrix(rs)
      dbDisconnect(con)
      selectInput("platform", "GDS", c('Select Platform',data_sets))  
    }
    
  })
  
  output$salidaprueba <- renderText({
    
    paste("selected: ",input$dataset," - ",input$platform)
    
  })
  
})
