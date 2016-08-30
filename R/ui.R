library(shinyjs)

shinyUI(fluidPage(
  
  theme = "bootstrap.css",
  includeCSS("www/omics.css"),
  includeCSS("www/style.css"),
  includeScript("www/md5.js"),
  includeScript("www/passwdInputBinding.js"),
  
  shinyjs::useShinyjs(),
  
  fluidRow(align="center", h1("Omics-in-Cloud  ",img(src = "img/logoICO.png"))),

  uiOutput("app")
  
))
