########################################################################
# Script Karajaulas Server
########################################################################
require(Biobase)
library(shiny)
library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)
source('D:\\Master\\TFM\\R\\ShinyApp\\IncRel.R')

options(shiny.maxRequestSize=500*1024^2)

shinyServer(function(input, output, session) {

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
        
        destdir = 'D:\\Master\\TFM\\R\\ShinyApp\\BD'
        ExperimentNCBI <- getGEO(input$experimentupload, destdir = destdir)
        Table(ExperimentNCBI)[,1:2]
        
      }
      
    })
    
    # ---------------------------------------------------------------------
    # Data Catalog NCBI
    # ---------------------------------------------------------------------
    # Select inicial de DataSets
    output$choose_dataset <- renderUI({
      
      if(!file.exists('dbNCBI\\GEOmetadb.sqlite')) { getSQLiteFile() }
      con <- dbConnect(SQLite(),'dbNCBI\\GEOmetadb.sqlite')
      rs <- dbGetQuery(con,'select gds from gds')
      data_sets <- rs[-1,]
      dbDisconnect(con)
      selectInput("dataset", "DataSet GDS", as.list(c('Select DataSet',data_sets)))
      
    })
    
    # Select inicial de Platform (una vez seleccionado DataSets)
    output$choose_platform <- renderUI({
      
      if (is.null(input$dataset) || input$dataset == "Select DataSet"){
        return()
      }
      else{
        if(!file.exists('dbNCBI\\GEOmetadb.sqlite')) { getSQLiteFile() }
        con <- dbConnect(SQLite(),'dbNCBI\\GEOmetadb.sqlite')
        rs <- dbGetQuery(con,'select gds from gds')
        data_sets <- rs[-1,]
        dbDisconnect(con)
        updateSelectInput("platform", "GPL", as.list(c('Select Platform',data_sets)))  
      }
      
    })
    
})

