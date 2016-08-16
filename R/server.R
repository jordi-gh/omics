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

dir = '/home/daniel/Documentos/màster del universo/ICO/omics/R'
source(paste(dir, '/IncRel.R', sep=""))

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
        ###### DIRECTORIO!
        #destdir = 'D:\\Master\\TFM\\R\\ShinyApp\\BD'
        destdir = '/home/daniel/Documentos/màster del universo/ICO/omics/R/BD'
        ######
        
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
        if(!file.exists('dbNCBI/GEOmetadb.sqlite')) { getSQLiteFile() }
        #con <- dbConnect(SQLite(),'dbNCBI/GEOmetadb.sqlite')
        con <- dbConnect(SQLite(),paste(dir, '/dbNCBI/GEOmetadb.sqlite', sep=""))
        #sql <- "SELECT DISTINCT gds FROM gds"
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
        if(!file.exists('dbNCBI/GEOmetadb.sqlite')) { getSQLiteFile() }
        #con <- dbConnect(SQLite(),'dbNCBI/GEOmetadb.sqlite')
        con <- dbConnect(SQLite(),paste(dir, '/dbNCBI/GEOmetadb.sqlite', sep=""))
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

