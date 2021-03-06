#library(RJSONIO)
library(rjson)
#library(jsonlite)
library(RCurl)
#install.packages("R4CouchDB")
library(R4CouchDB)

source(file.path(gb_Rdir, 'IncGEO.R'))

#########################################################################
#Cal tenir instal?lat i actiu couchdb i una bd creada amb el nom 'geodb'
#########################################################################

# GeoACouch_old <- function(objGEO,nom) {
#   #Demanem un UUID a CouchDB. Ho podriem fer nosaltres amb llibraria R uuid
#   #Caldra veure si es una millor opcio
#   response <- fromJSON(getURL("http://localhost:5984/_uuids"))
#   uid=response$uuid
#   #Convertim taula de dades a JSON, si en te
#   tipusGEO <- toupper(class(objGEO))
#   if (tipusGEO =='EXPRESSIONSET'){
#     eset=toJSON(objGEO)
#     newreg<-toJSON(list(nom=nom,tipus=tipusGEO,expression_set=eset))
#   } else if (tipusGEO=='GSE'){
#     #Convertim metadades a JSON
#     metadades<-toJSON(Meta(objGEO))
#     #Convertim llista de GPLs a JSON
#     gpls<-toJSON(objGEO@gpls)
#     #Convertim llista de GSMs a JSON
#     gsms<-toJSON(objGEO@gsms)
#     #Preparem registre per inserir a BD
#     newreg<-toJSON(list(nom=nom,tipus=tipusGEO,metadades=metadades,gpls=gpls,gsms=gsms))
#   }
#   else {
#     taula<-toJSON(Table(objGEO))
#     #Convertim metadades a JSON
#     metadades<-toJSON(Meta(objGEO))
#     #Preparem registre per inserir a BD
#     newreg<-toJSON(list(nom=nom,tipus=tipusGEO,metadades=metadades,taula=taula))
#   }
#   response<-fromJSON(getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
#          customrequest="PUT",
#          httpheader=c('Content-Type'='application/json'),
#          postfields=newreg))
#   #Retornem dades registre guardar-les a model relacional
#   return (response)
# }

GeoACouch <- function(objGEO,nom) {
  #Demanem un UUID a CouchDB. Ho podriem fer nosaltres amb llibraria R uuid
  #Caldra veure si es una millor opcio
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  #Convertim objecte a JSON
  classeGEO <- class(objGEO)
 # jsonGEO=jsonlite::toJSON(objGEO)
  # jsonGEO=toJSON(raw2hex(serialize(objGEO,NULL),sep=''))
  jsonGEO <- objecteAJSON(objGEO)
  newreg<-toJSON(list(nom=nom,classe=classeGEO,objecte=jsonGEO))
  response<-fromJSON(getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
         customrequest="PUT",
         httpheader=c('Content-Type'='application/json'),
         postfields=newreg))
  #Retornem resposta sencera per guardar info a model relacional
  return (response)
}


ICOACouch <- function(strJSON,nomfitxer) {
  #Demanem un UUID a CouchDB. Ho podriem fer nosaltres amb llibraria R uuid
  #Caldra veure si es una millor opcio
  response <- fromJSON(getURL("http://localhost:5984/_uuids"))
  uid=response$uuid
  #Preparem registre per inserir a BD
  newreg<-toJSON(list(nom=nomfitxer,rawdata=strJSON))
  response <- fromJSON(getURL(paste("http://localhost:5984/geodb/",uid,sep=""),
         customrequest="PUT",
         httpheader=c('Content-Type'='application/json'),
         postfields=newreg))
  #Retornem dades registre guardar-les a model relacional
  return (response)
}

#Llegir un fitxer ICO de CouchDB 
CouchAICO <- function(uid){
  response <- getURL(paste("http://localhost:5984/geodb/",uid,sep=""))
  #Retornem el rawdata del fitxer en format JSON
  llistareg <- fromJSON(response)
  if (is.null(llistareg)){
      return(FALSE)
  }
  strJSON <- llistareg['rawdata']
  return(strJSON)
}


#Llegir un fitxer GEO de CouchDB
CouchAGEO <- function(uid){
  response <- getURL(paste("http://localhost:5984/geodb/",uid,sep=""))
  #Retornem el document en format JSON
  llistareg <- fromJSON(response)
  #Si no l'hem trobat sortim
  if (is.null(llistareg)){
    return(FALSE)
  }
  #Si no és GEO sortim
  if (is.null(llistareg['classe'])){
    return (FALSE)
  }
  classeGEO<-llistareg['classe']

  #Control classe vàlida
  if(!is.element(classeGEO,c('ExpressionSet','GPL','GSM','GDS','GSE'))){
    return(FALSE) #No és tipus GEO reconegut
  }
  objGEO=new(classeGEO)
  #objGEO<-jsonlite::fromJSON(llistareg['objecte'])
  # obj_hex <- fromJSON(llistareg['objecte']) 
  # obj_raw <- wkb::hex2raw(obj_hex)
  # objGEO <- unserialize(obj_raw)
  objGEO<-JSONAObjecte(llistareg['objecte'])
  return(objGEO)
}
# CouchAGEO <- function(uid){
#   objGEO<-getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE976_series_matrix.txt.gz",GSEMatrix=false)
#   return(objGEO)
# }

#Eliminar document de couch per uid
eliminaCouch <- function(uid=NULL,rev=NULL){
  #Si no tenim uid definit no fem res: PODRIEM ELIMINAR DB SENCERA
  if(is.null(uid)){
    return(FALSE)
  }
  url<-paste0("http://localhost:5984/geodb/",uid)
  #Per eliminar hem d'afegir la revisió del document
  url<-paste(url,rev,sep='?rev=')
  response <- getURL(url,
                     customrequest="DELETE",
                     httpheader=c('Content-Type'='application/json'))
  return(fromJSON(response))
}


