#library(RJSONIO)
#library(rjson)
library(jsonlite)
library(RCurl)
#install.packages("R4CouchDB")
library(R4CouchDB)

#########################################################################
#Cal tenir instal·lat i actiu couchdb i una bd creada amb el nom 'geodb'
#########################################################################

#Funció demo per veure com podem accedir a couchDB sense utilitzar R4couchDB,
#només amb crides url directes
# GeoACouch_curl <- function(objGEO,nom) {
#   #Demanem un UUID a CouchDB. Això ho podriem fer nosaltres amb llibraria R uuid
#   #Caldrà veure si és una millor opció
#   response <- fromJSON(getURL("http://localhost:5984/_uuids"))
#   uid=response$uuid
#   #Convertim taula de dades a JSON, si en té
#   taula<-toJSON(Table(objGEO))
#   #Convertim metadades a JSON
#   metadades<-toJSON(Meta(objGEO))
#   #Preparem registre per inserir a BD
#   newreg<-toJSON(list(nom=nom,metadades=metadades,taula=taula))
#   getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
#          customrequest="PUT",
#          httpheader=c('Content-Type'='application/json'),
#          postfields=newreg)
#   #Retornem uid per guardar-lo a model relacional
#   return (uid)
# }

#Inicialitzar objecte CouchDB utilitzat per R4CouchDB amb la nostra bd: geodb
iniGeoDB<- function(){
  cdb <- cdbIni()
  cdb$serverName <- "localhost"
  cdb$port <- 5984
  cdb$DBName="geodb"
  return(cdb)
}

#Utilitzem funcions R4CouchDB per accedir BD
#Passem un objecte GEO i el posem a Couch, separant nom,taula i metadades
GeoACouch <- function(objGEO,nom){
  #Demanem un UUID a CouchDB. Això ho podriem fer nosaltres amb llibraria R uuid
  #Caldrà veure si és una millor opció
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  cdb <- iniGeoDB()
  tipusObj<-class(objGEO)
  #NOTA: Utilitzem serialize/unserialize de jsonlite perque és el que manté la consistència
  #de l'objecte. tojson/fromjson de rjson converteixen els objectes GEO en list al recuperar-los
  #Per saber si un objecte s'ha modificat després de fer serialize/unserialize podem utilitzar
  # all.equal(objGEO_orig,objGEO_recuperat) i hauria de donar TRUE
  #Convertim metadades a JSON per poder interrogar valors sense recuperar objecte
  metadades<-jsonlite::serializeJSON(Meta(objGEO))  
  #Convertim objecte sencer a JSON per recuperar-lo igual
  jsonobj<-jsonlite::serializeJSON(objGEO)
   #Si és un GSE no té taula associada i té dues llistes (GPL i GSM)
  if (tipusObj=='GSE'){
    gpllist <- jsonlite::serializeJSON(GPLList(objGEO))
    gsmlist <- jsonlite::serializeJSON(GSMList(objGEO))
    newreg.data <- list(
      jsonobj=jsonobj,
      gpllist=gpllist,
      gsmlist=gsmlist,
      metadades=metadades,
      tipus=tipusObj,
      nom=nom)
  } else {
    newreg.data <- list(
      jsonobj=jsonobj,
      metadades=metadades,
      tipus=tipusObj,
      nom=nom)
  }
  cdb$dataList <- newreg.data
  cdb$id <- uid  ## optional, otherwise an ID is generated
  cdb <- cdbAddDoc(cdb)
  #Retornem uid per guardar-lo a model relacional
  return (uid)
}

#Donat un uid recuperem objecte GEO de CouchDB
CouchAGeo <- function(uid){
  cdb <- iniGeoDB()
  cdb$id <- uid
  cdb <- cdbGetDoc(cdb)
  info<-cdb$res
  tipusObj<-info$tipus

  objGeo <- jsonlite::unserializeJSON(info$jsonobj)
  return(objGEO)

}

