#library(RJSONIO)
library(rjson)
library(RCurl)
#install.packages("R4CouchDB")
library(R4CouchDB)

#########################################################################
#Cal tenir instal?lat i actiu couchdb i una bd creada amb el nom 'geodb'
#########################################################################

GeoACouch <- function(objGEO,nom) {
  #Demanem un UUID a CouchDB. Ho podriem fer nosaltres amb llibraria R uuid
  #Caldra veure si es una millor opcio
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  #Convertim taula de dades a JSON, si en te
  if (toupper(class(objGEO))=='EXPRESSIONSET'){
    eset=toJSON(objGEO)
    newreg<-toJSON(list(nom=nom,expression_set=eset))
  } else if (toupper(class(objGEO))=='GSE'){
    #Convertim metadades a JSON
    metadades<-toJSON(Meta(objGEO))
    #Convertim llista de GPLs a JSON
    gpls<-toJSON(objGEO@gpls)
    #Convertim llista de GSMs a JSON
    gsms<-toJSON(objGEO@gsms)
    #Preparem registre per inserir a BD
    newreg<-toJSON(list(nom=nom,metadades=metadades,gpls=gpls,gsms=gsms))
  }
  else {
    taula<-toJSON(Table(objGEO))
    #Convertim metadades a JSON
    metadades<-toJSON(Meta(objGEO))
    #Preparem registre per inserir a BD
    newreg<-toJSON(list(nom=nom,metadades=metadades,taula=taula))
  }
  getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
         customrequest="PUT",
         httpheader=c('Content-Type'='application/json'),
         postfields=newreg)
  #Retornem uid per guardar-lo a model relacional
  return (uid)
}


NoGeoACouch <- function(strJSON,nomfitxer) {
  #Demanem un UUID a CouchDB. Ho podriem fer nosaltres amb llibraria R uuid
  #Caldra veure si es una millor opcio
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  message(paste('DBG1: nou id: ',uid,sep=''))
  #Preparem registre per inserir a BD
  newreg<-toJSON(list(nom=nomfitxer,data=strJSON))
  message('DBG2')
  getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
         customrequest="PUT",
         httpheader=c('Content-Type'='application/json'),
         postfields=newreg)
  #Retornem uid per guardar-lo a model relacional
  message('DBG3')
  return (uid)
}
