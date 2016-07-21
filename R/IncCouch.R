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

#Mirem si tenim un fitxer a CouchDB
#Taula indica la taula (gpl,gds..) on hem de buscar
#uidnom es un named list on podem tenir qualsevol dels dos valors(nom,uid). 
buscaACouch <- function(taula,uidnom) {
  #Connectar a BD SQLite
  db <- dbConnect(SQLite(), 'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')
  uid = uidnom[['uid']]
  nom = uidnom[['nom']]
  if ((is.null(uid))&&(is.null(nom))){ 
    return(false)
  }
  sql = paste('SELECT * FROM',taula,sep=' ')
  if (!is.null(uid)) {
    sql = paste(paste(paste(sql,'WHERE uid="',sep=' '),uid,sep=''),'"',sep='')
  } else {
    sql = paste(paste(paste(sql,'WHERE name="',sep=' '),nom,sep=''),'"',sep='')
  }
  res = dbGetQuery(db, sql)
  if (nrow(res)>0){
    return(true)
  } else {
    return(false)
  }
}
