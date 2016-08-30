require(Biobase)
library(shiny)
library(shinyjs)
library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)
library(gplots)

## Carregar variables d'entorn local
source('ConfigLocal.R')
##Carregar variables globals
source('ConfigGlobal.R')
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

#campos obligatorios para el formulario de usuario nuevo y todos los campos del form
fieldsMandatory <- c("newusername", "newpassword","newname","newlastname1","newmail","newrole")
fieldsAll <- c("newusername", "newpassword","newname","newlastname1","newlastname2","newmail","newrole","newgroup","userrolid")
res_roles <- getRoles(db,'no_admin')


shinyServer(function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE)
  USERPROFILE <- reactiveValues(Profile = FALSE)
  #res_icofilesgrup <- getIcofilesGroup(db,USERPROFILE$Profile$grupuser)
  
  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
      USERPROFILE$Profile <- getUserProfile(db,input$.username)
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
      
      #para el formulario de cambio de permisos
      res_grups <- getGrups(db,USERPROFILE$Profile$grupuser)
      
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
                   titlePanel("Welcome to Omics-in-Cloud"), br(),
                   sidebarLayout(
                     sidebarPanel(
                       h3(img(src="img/profile.png"),paste0("Profile User ")),
                       br(),
                       h4(paste0("User    : ", USERPROFILE$Profile$nom, " ", USERPROFILE$Profile$cognom1, " ",USERPROFILE$Profile$cognom2)),
                       h4(paste0("Username: ", USERPROFILE$Profile$username)),
                       h4(paste0("Group   : ", USERPROFILE$Profile$nomgrup)),
                       h4(paste0("Role    : ", USERPROFILE$Profile$nomrol))
                     ),
                   mainPanel(
                     h2("Your data"),
                     br(),
                     tags$ul(h4("ICO files"),
                             tags$li("Number of owner files: "),
                             tags$li("Number of shared files with me: ")
                     ),br(),
                     tags$ul(h4("NCBI files"),
                             tags$li("Number of GPL files: "),
                             tags$li("Number of GSM files: "),
                             tags$li("Number of GSE files: "),
                             tags$li("Number of GDS files: ")
                             )
                   )
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
                     tabPanel("Hierarchical Clustering", id = "heatmapgse",
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput("gseplot", "GSE", c('Select GSE','GSE976')),
                                  downloadButton('downloadheatmap', 'Download Plot')
                                ),
                                mainPanel(
                                  tags$div(class = "waiting", p("Progress.."), img(src="img/loader.gif")),
                                  plotOutput('heatmapgse')
                                )
                              )
                     ),
                     tabPanel("GSM bins", id = "gsebins",
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput("bins",
                                              "Number of bins:",
                                              min = 1,
                                              max = 20,
                                              value = 10)
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                  plotOutput("distPlot")
                                )
                              )
                      )
                   )
          ),
          
          # Manager Options
          tabPanel("Manager Options", icon = icon("fa fa-cog"),
                   navlistPanel(widths = c(2, 10),
                     tabPanel("New User", icon = icon("fa fa-plus-circle"), id = "newuser",
                              sidebarLayout(
                                sidebarPanel(width = 10, id='newuserform',
                                             h4(paste0("New user ", USERPROFILE$Profile$nomgrup)),br(),
                                             shinyjs::hidden(
                                               div(
                                                 id = "saved_msg",
                                                 h3("User saved !"),
                                                 br()
                                               )
                                             ),
                                             shinyjs::hidden(
                                               div(
                                                 id = "error_msg",
                                                 h3("Not save. Permission denied. Please, contact with your manager"),
                                                 br()
                                               )
                                             ),
                                             fluidRow( column(4,textInput("newusername", labelMandatory("Username"),""),
                                             passwordInput("newpassword", labelMandatory("Password"),""),
                                             selectInput("newrole", labelMandatory("User role"), choices = split(res_roles$id,res_roles$nomrol)),
                                             actionButton("submit", "New User", class = "btn-primary")
                                             ),
                                             column(7, textInput("newname", labelMandatory("Name"),""),
                                             textInput("newlastname1", labelMandatory("Lastname 1"),""),
                                             textInput("newlastname2", "Lastname 2",""),
                                             textInput("newmail", labelMandatory("Mail"),"")),
                                             hidden(textInput("newgroup","Group",USERPROFILE$Profile$grupuser)),
                                             hidden(textInput("userrolid","Rol",USERPROFILE$Profile$rolid)))
                                ),
                                mainPanel()
                              )
                      ),
                     tabPanel("Share File", icon = icon("fa fa-exchange"), id = "sharefile",
                               sidebarLayout(
                                 sidebarPanel(width = 10, id='sharefileform',
                                   fluidRow(column(2,checkboxGroupInput("idgrups", "Select groups:", choices = split(res_grups$id,res_grups$nomgrup)),
                                   br(),br(),actionButton("submitaccess", "Share file", class = "btn-primary")),
                                   column(8,
                                          shinyjs::hidden(
                                            div(
                                              id = "shared_msg",
                                              h3("File shared !"),
                                              br()
                                            )
                                          ),
                                   h4("Select file to share (row table):"),br(),
                                   dataTableOutput('tblfilesico'))
                                   )
                                ),
                                mainPanel()
                               )
                            )
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
  # Analisis1: Hierarchical Clustering Heatmap GSE
  # --------------------------------------------------------------------- 
  plotInputHeatmap <- function(){
        if (input$gseplot!="Select GSE" && input$gseplot!=""){
          destdir = file.path(gb_Rdir, 'BD')
          gse = getGEO(input$gseplot, destdir = destdir)[[1]]
          sdN = 3
          sds = apply(log2(exprs(gse)+0.0001),1,sd)
          heatmap.2(log2(exprs(gse)+0.0001)[sds>sdN,],trace='none',scale='row')
        }
  }
  
  output$heatmapgse <- renderPlot({
    print(plotInputHeatmap())
  })
  
  output$downloadheatmap <- downloadHandler(
    filename = 'heatmapGSE.jpg',
    content = function(file) {
      jpeg(file, width=1024, height=768, units="px",quality=100)
      print(plotInputHeatmap())
      dev.off()
    })
  
  # ---------------------------------------------------------------------  
  # Analisis2: GSM Bins
  # --------------------------------------------------------------------- 
  output$distPlot <- renderPlot({
    ExperimentNCBI <- getGEO("GSM320590", destdir = destdir)
    cols <- c("VALUE")
    
    Value <- Table(ExperimentNCBI)[,cols]
    bins <- seq(min(Value), max(Value), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(Value, breaks = bins, col = 'red', border = 'white',
         main = 'GSM320590 values', labels = TRUE)
  })
  
  # ---------------------------------------------------------------------  
  # Load ICO
  # ---------------------------------------------------------------------  
  output$contents <- renderDataTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    objGEO <- getGEO(filename=inFile$datapath)
    #Guardem a persistència ICO
    guardaGEO(objGEO,inFile$name,inFile$datapath)
    #Retornem info de visualitzacio de l'objecte GEO en format dataframe 
    dfView(objGEO)
  },options = list(
    scrollX = TRUE
  ))
  
  # ---------------------------------------------------------------------
  # Load NCBI
  # ---------------------------------------------------------------------
  output$experiment <- renderDataTable({ 
    submitExperiment() #Esperamos al submit
  },options = list(
    scrollX = TRUE
  ))
  
  submitExperiment <- eventReactive(input$submitexperiment,{ 
    
    if (input$experimentupload!="Enter text..." && input$experimentupload!=""){
      destdir = file.path(gb_Rdir, 'BD')
      
      ExperimentNCBI <- getGEO(input$experimentupload, destdir = destdir)
      
      #Guardem a persistència ICO
      #Compte: Enviem un 'named param' a la funció
      #http://blog.moertel.com/posts/2006-01-20-wondrous-oddities-rs-function-call-semantics.html
      guardaGEO(ExperimentNCBI,accession=input$experimentupload)
      # Retorna dataframe de visualització segons tipus d'objecte GEO
      dfView(ExperimentNCBI)
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
      USERPROFILE$Profile <- FALSE
      USER$pass <- ""
    })
  
  # ---------------------------------------------------------------------
  # New User Form
  # ---------------------------------------------------------------------
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data
  })
  
  saveData <- function(data) {
    db <- getMetadataDB()
    sql = paste0("INSERT INTO usuaris (username,pwd,nom,cognom1,cognom2,mail,lastlog,rolid) values ('",data[1],"','",data[2],"','",data[3],"','",data[4],"','",data[5],"','",data[6],"','',",data[7],")")
    result = sendQuery(db,sql)
    sql = paste0("SELECT id from usuaris where username='",data[1],"'")
    result = dbGetQuery(db,sql)
    sql2 = paste0("INSERT INTO usuari_grup (userid,grupid) VALUES (",result$id,",'",data[8],"')");
    result2 = sendQuery(db,sql2)
  }
  
  observeEvent(input$submit, {
    data <- formData()
    # Solo permite guardar si el rol es 1 admin o 2 coordinador
    if (data[9]==1 || data[9]==2){
      saveData(data)
      shinyjs::reset("newuserform")
      shinyjs::show("saved_msg")
    }
    else{
      shinyjs::reset("newuserform")
      shinyjs::show("error_msg")
    }
  })
  
  # ---------------------------------------------------------------------
  # Share ICOfiles
  # ---------------------------------------------------------------------
  filesData <- reactive({
    res_icofilesgrup <- getIcofilesGroup(db,USERPROFILE$Profile$grupuser)
    res_icofilesgrup
  })

  output$tblfilesico = DT::renderDataTable({
        DT::datatable(filesData(),
                     options = list(orderClasses = TRUE,
                                    aLengthMenu = c(5, 10, 25),
                                     pageLength = 5)
                     , selection = 'single')
  })
  
  observeEvent(input$submitaccess, {
    #grupos seleccionados (1 o mas de uno)
    idgrupsselected <- input$idgrups
    #fichero seleccionado
    uidselected <- filesData()[input$tblfilesico_rows_selected,]$uid
    
    db <- getMetadataDB()
    # borramos todos los derechos que habia sobre el fichero compartido
    sql = paste0("DELETE FROM accessfiles WHERE uidfile = '",uidselected,"'")
    result = dbGetQuery(db,sql)
    
    # compartimos el fichero con los grupos seleccionados
     for (i in 1:length(idgrupsselected)){
       sql = paste0("INSERT INTO accessfiles (uidfile,idgroup,dateaccess) VALUES ('",uidselected,"',",idgrupsselected[i],",date('now'))")
       result = dbGetQuery(db,sql)
     }
    
    #mensaje y reset de form
    shinyjs::reset("sharefileform")
    shinyjs::show("shared_msg")
  })
  
  # ---------------------------------------------------------------------
  # FIN SESION SHINY: Codigo de limpieza aquí
  # ---------------------------------------------------------------------
  session$onSessionEnded(function() {
    closeMetaDB();
    message('SessionEnd OK')
  })
  
  
})

    

    