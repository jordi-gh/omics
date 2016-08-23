require(Biobase)
#library(shiny)
#library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)
library(utils)

#SQLITE
db <- dbConnect(SQLite(), 
                'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')
#dbDisconnect(db)
dbSendQuery(conn = db,
            "CREATE TABLE gsm
            (uid TEXT,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT
             )
            ")
# dbSendQuery(conn = db,"DROP TABLE gsm")
dbGetQuery(db,'Select * from gsm');

sql = paste('SELECT * FROM gpl WHERE name = "','GPL571"',sep='')
res = dbGetQuery(db, sql)
if (nrow(res)>0){
  message('EUREKA! el tenim')
}

#objGEO <- Table(getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\GPL11154.soft",GSEMatrix=false))
objGEO <- Table(getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false))
class(objGEO)
#Meta(objGEO)
## Carregar objecte i segons classe posem a taula del model relacional que correspongui
objGEO <-getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false)
if (class(objGEO)=='GSM'){
   sql = 'INSERT INTO gsm (name,down,path,filename) VALUES(:name,:down,:path,:filename)'
   #sql = 'INSERT INTO GSM (name,down,path,filename) VALUES("test",1,"PATH","filenameee")'
   valors<-data.frame(name='GSM1897275',
                      down=1,
                      path='D:\\Master\\TFM\\Dades\\ICO\\samples',
                      filename='GSM1897275.soft')
   dbSendPreparedQuery(db, sql, bind.data = valors)
}


objGEO <- getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false)
objGEO <- getGEO(filename="D:\\Master\\TFM\\Dades\\Mostres\\GSM320590.soft",GSEMatrix=false)

taula=Table(objGEO)
# Insert a CouchDB
getURL(paste("http://localhost:5984/geodb/","12345",sep=""),
       customrequest="PUT",
       httpheader=c('Content-Type'='application/json'),
       postfields='{\"couchdb\":\"Welcome\",\"version\":\"1.0.1\"}')




source('ConfigLocal.R')
source('C:\\Jordi\\Master\\TFM\\R\\ConfigLocal.R')
source(file.path('C:','Jordi','Master','TFM','R','ConfigLocal.R'))

a<-source('ConfigLocal.R')
a

source(paste(R_dir, 'IncRel.R', sep=path_sep))
source(file.path(R_dir,'IncRel.R'))

source(file.path(dirname(sys.frame(1)$ofile),'ConfigLocal.R'))
source(file.path('.','ConfigLocal.R'))
source('ConfigLocal.R')

wd <- setwd(".")
setwd(wd)

getwd()
source('./ConfigLocal.R',local = TRUE)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
frame_files



LocationOfThisScript = function() # Function LocationOfThisScript returns the location of this .R script (may be needed to source other files in same dir)
{
  this.file = NULL
  # This file may be 'sourced'
  for (i in -(1:sys.nframe())) {
    if (identical(sys.function(i), base::source)) this.file = (normalizePath(sys.frame(i)$ofile))
  }
  
  if (!is.null(this.file)) return(dirname(this.file))
  
  # But it may also be called from the command line
  cmd.args = commandArgs(trailingOnly = FALSE)
  cmd.args.trailing = commandArgs(trailingOnly = TRUE)
  cmd.args = cmd.args[seq.int(from=1, length.out=length(cmd.args) - length(cmd.args.trailing))]
  res = gsub("^(?:--file=(.*)|.*)$", "\\1", cmd.args)
  
  # If multiple --file arguments are given, R uses the last one
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case. Maybe we are in an R GUI?
  return(NULL)
}
current.dir = LocationOfThisScript()

dir <- dirname(parent.frame(2)$ofile)
setwd(dir)


utils::getSrcDirectory()
dir1<-getSrcDirectory()[1]


source(file.path(gb_Rdir, 'IncRel.R'))
db <- getMetadataDB()
sql<-"SELECT * FROM usuaris"
res<-exeQuery(db,sql)
credentials1<-list()
i <- 1
for (nom in res$username){
  credentials1[[nom]] <- res$pwd[i]
  i <- i+1
}

credentials <- res[,c("username","pwd")]
list1 <- as.list(credentials)

list2 <- list("rpena" = "92eb5ffee6ae2fec3ad71c777531578f","dmendez" = "92eb5ffee6ae2fec3ad71c777531578f","jtorresz" = "92eb5ffee6ae2fec3ad71c777531578f")


objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GPL226.soft",GSEMatrix=false)
colnames(Table(objGEO))


