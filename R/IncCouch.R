#library(RJSONIO)
library(rjson)
library(RCurl)
#install.packages("R4CouchDB")
library(R4CouchDB)

#########################################################################
#Cal tenir instal·lat i actiu couchdb i una bd creada amb el nom 'geodb'
#########################################################################

GeoACouch <- function(objGEO,nom) {
  #Demanem un UUID a CouchDB. Això ho podriem fer nosaltres amb llibraria R uuid
  #Caldrà veure si és una millor opció
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  #Convertim taula de dades a JSON, si en té
  taula<-toJSON(Table(objGEO))
  #Convertim metadades a JSON
  metadades<-toJSON(Meta(objGEO))
  #Preparem registre per inserir a BD
  newreg<-toJSON(list(nom=nom,metadades=metadades,taula=taula))
  getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
         customrequest="PUT",
         httpheader=c('Content-Type'='application/json'),
         postfields=newreg)
  #Retornem uid per guardar-lo a model relacional
  return (uid)
}

