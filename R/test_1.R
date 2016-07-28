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
objGEO2 <- getGEO(filename="D:\\Master\\TFM\\Dades\\Mostres\\GSM320590.soft",GSEMatrix=false)

taula=Table(objGEO)
# Insert a CouchDB
getURL(paste("http://localhost:5984/geodb/","12345",sep=""),
       customrequest="PUT",
       httpheader=c('Content-Type'='application/json'),
       postfields='{\"couchdb\":\"Welcome\",\"version\":\"1.0.1\"}')

source('D:\\Master\\TFM\\dev\\R\\IncCouch.R')

res=buscaAMetadades('gsm',list(nom='GSM1897275'))

if (!identical(res, FALSE)){
  message(res$uid)
}

#Test llegir document 

cdb$id <- '5797b71009ba7e5c96a2e8dcd400394a'
cdb$id <- '5797b71009ba7e5c96a2e8dcd4004915'
cdb <- cdbGetDoc(cdb)
reg=cdb$res


#Test Esborrar document
cdb <- iniGeoDB()
cdb$id <- '_design/example'
cdb <- cdbDeleteDoc(cdb)
