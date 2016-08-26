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
library(gplots)

## Carregar variables d'entorn local
source('ConfigLocal.R')
##Carregar includes
source(file.path(gb_Rdir, 'IncRel.R'))
source(file.path(gb_Rdir, 'IncGEO.R'))

options(shiny.maxRequestSize=500*1024^2)

# Leer lista de usuarios i pwd 
db <- getMetadataDB()
sql<-"SELECT * FROM usuaris"
res<-getQuery(db,sql)
#Convertim de dataframe a 'named list' amb credencials (nom1=pwd1,nom2=pwd2...)
credentials<-list()
i <- 1
for (nom in res$username){
  credentials[[nom]] <- res$pwd[i]
  i <- i+1
}


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
          
          div(actionLink("logout", "Logout"),icon("fa fa-user"), input$.username),
          
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
                                  textInput("searchexperiment", label = h3("Search GPL"), value = "Enter text..."),
                                  # select dataset
                                  uiOutput("choose_dataset"),
                                  # select sample
                                  uiOutput("choose_sample")
                                ),
                                
                                mainPanel(
                                  textOutput("salidaprueba"),
                                  htmlOutput("descripcionplatform"),
                                  htmlOutput("descripcionserie"),
                                  htmlOutput("descripcionsample")
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
                     tabPanel("Heatmap GSE", id = "heatmapgse",
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput("gseplot", "GSE", c('Select GSE','GSE976'))
                                ),
                                mainPanel(
                                  tags$div(class = "waiting", p("Progress.."), img(src="img/loader.gif")),
                                  plotOutput('heatmapgse')
                                )
                              )
                     ),
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
  # Analisis1: Heatmap GSE
  # --------------------------------------------------------------------- 
  output$heatmapgse <- renderPlot({
    
    if (input$gseplot!="Select GSE" && input$gseplot!=""){
      destdir = file.path(gb_Rdir, 'BD')
      gse = getGEO(input$gseplot, destdir = destdir)[[1]]
      sdN = 3
      sds = apply(log2(exprs(gse)+0.0001),1,sd)
      heatmap.2(log2(exprs(gse)+0.0001)[sds>sdN,],trace='none',scale='row')
    }
    
  })
  

  # ---------------------------------------------------------------------  
  # Upload ICO
  # ---------------------------------------------------------------------  
  output$contents <- renderDataTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    objGEO <- getGEO(filename=inFile$datapath)
    
    guardaFitxer(objGEO,inFile$name,inFile$datapath)
    if (toupper(class(objGEO))=='EXPRESSIONSET'){
      pData(objGEO)[,1:3]
    } else if (toupper(class(objGEO))=='GSE'){
      #Agafem info dels GSMs del GSE en format dataframe
      getGSMsFromGSE(objGEO)
    }
      else {
        if (length(Table(objGEO))==0) {
          return(NULL)
        }
      Table(objGEO)[,1:2]
    }
  })
  
  # ---------------------------------------------------------------------
  # Upload NCBI
  # ---------------------------------------------------------------------
  output$experiment <- renderDataTable({ 
    submitExperiment() #Esperamos al submit
  })
  
  submitExperiment <- eventReactive(input$submitexperiment,{ 
    
    if (input$experimentupload!="Enter text..." && input$experimentupload!=""){
      destdir = file.path(gb_Rdir, 'BD')
      
      ExperimentNCBI <- getGEO(input$experimentupload, destdir = destdir)
      type <- substr(input$experimentupload, 0, 3)
      if (type == 'GPL') cols <- c("ID", "Gene Symbol", "ENTREZ_GENE_ID")
      else if (type == 'GSM') cols <- c("ID_REF",	"VALUE")
      else if (type == 'GDS') cols <- c("ID_REF",	"IDENTIFIER")
      
      if (type == 'GSE') exprs(ExperimentNCBI[[1]])
      else Table(ExperimentNCBI)[,cols]
      
    }
    
  })
  
  # ---------------------------------------------------------------------
  # Data Catalog NCBI
  # ---------------------------------------------------------------------
  # Select inicial de DataSets
  output$choose_dataset <- renderUI({
    
    if (is.null(input$searchexperiment) || input$searchexperiment == "Enter text..."){
      selectizeInput("dataset", "GSE", c('Select DataSet Serie',""))
    }
    else{
      if(!file.exists(gb_geoSQLFile)) { getSQLiteFile() }
      con <- dbConnect(SQLite(),gb_geoSQLFile)
      sql <- paste("SELECT DISTINCT gse.gse",
                   " FROM",
                   " gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
                   " JOIN gse ON gse_gsm.gse=gse.gse",
                   " JOIN gse_gpl ON gse_gpl.gse=gse.gse",
                   " JOIN gpl ON gse_gpl.gpl=gpl.gpl",
                   " WHERE",
                   " gpl.gpl LIKE '%",input$searchexperiment,"%'", sep="")
      rs <- dbGetQuery(con,sql)
      data_sets <- as.matrix(rs)
      dbDisconnect(con)
      selectizeInput("dataset", "GSE", c('Select DataSet Serie',as.list(data_sets)),options = list(maxOptions = 10))
    }
    
  })
  
  # Select inicial de Sample (una vez seleccionado DataSets)
  output$choose_sample <- renderUI({
    
    if (is.null(input$dataset) || input$dataset == "Select DataSet Serie"){
      selectInput("sample", "GSM", c('Select DataSet Sample',""))
    }
    else{
      if(!file.exists(gb_geoSQLFile)) { getSQLiteFile() }
      con <- dbConnect(SQLite(),gb_geoSQLFile)
      sql <- paste("SELECT DISTINCT gsm.gsm",
                   " FROM gsm",
                   " JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
                   " WHERE gse_gsm.gse = '",input$dataset,"'", sep="")
      rs <- dbGetQuery(con,sql)
      data_sets <- as.matrix(rs)
      dbDisconnect(con)
      selectizeInput("sample", "GSM", c('Select DataSet Sample',as.list(data_sets)),options = list(maxOptions = 10))
    }
    
  })
  
  output$salidaprueba <- renderText({
    
    paste("selected: ",input$searchexperiment," - ",input$dataset," - ",input$sample)
    
  })
  
  
  output$descripcionplatform <- renderText({
    if (is.null(input$searchexperiment) || input$searchexperiment == "Enter text..."){
      return()
    }
    con <- dbConnect(SQLite(),gb_geoSQLFile)
    sql <- paste("SELECT *",
                 " FROM gpl",
                 " WHERE gpl.gpl = '",input$searchexperiment,"'", sep="")
    rs <- dbGetQuery(con,sql)
    
    str0 <-"<br/>"
    str1 <-paste("GPL: ",rs$gpl)
    str2 <-paste("ID: ",rs$ID)
    str3 <-paste("Title: ",rs$title)
    str4 <-paste("Status: ",rs$status)
    str5 <-paste("Date: ",rs$submission_date)
    
    HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  
  output$descripcionserie <- renderText({
    if (is.null(input$dataset) || input$dataset == "Select DataSet Serie"){
      return()
    }
    con <- dbConnect(SQLite(),gb_geoSQLFile)
    sql <- paste("SELECT *",
                 " FROM gse",
                 " WHERE gse.gse = '",input$dataset,"'", sep="")
    rs <- dbGetQuery(con,sql)
    
    str0 <-"<br/>"
    str1 <-paste("GSE: ",rs$gse)
    str2 <-paste("ID: ",rs$ID)
    str3 <-paste("Title: ",rs$title)
    str4 <-paste("Status: ",rs$status)
    str5 <-paste("Date: ",rs$submission_date)
    
    HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  
  output$descripcionsample <- renderText({
    if (is.null(input$sample) || input$sample == "Select DataSet Sample"){
      return()
    }
    con <- dbConnect(SQLite(),gb_geoSQLFile)
    sql <- paste("SELECT *",
                 " FROM gsm",
                 " WHERE gsm.gsm = '",input$sample,"'", sep="")
    rs <- dbGetQuery(con,sql)
    
    str0 <-"<br/>"
    str1 <-paste("GSM: ",rs$gsm)
    str2 <-paste("ID: ",rs$ID)
    str3 <-paste("Title: ",rs$title)
    str4 <-paste("Status: ",rs$status)
    str5 <-paste("Date: ",rs$submission_date)
    
    HTML(paste(str0, str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  # ---------------------------------------------------------------------
  # FIN Data Catalog NCBI
  # ---------------------------------------------------------------------
    
    # ---------------------------------------------------------------------
    # Logout
    # ---------------------------------------------------------------------
    observeEvent(input$logout , {
      USER$Logged <- FALSE
      USER$pass <- ""
    })
  
})
    
    