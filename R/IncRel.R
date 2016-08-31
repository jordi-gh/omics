library(GEOquery)
library(GEOmetadb)
library(RSQLite)
library(wkb) 

source(file.path(gb_Rdir, 'IncCouch.R'))


getMetadataDB <- function(){
  #Mantenim una única connexió amb variable global db_meta
  #Si ja la tenim instanciada i vàlida no cal reconnectar
  if ((!is.null(db_meta)) && (dbIsValid(db_meta))){
      return(db_meta)
  }
  SQLFile = file.path(gb_Rdir, 'metadata.sqlite')
  db_meta <- dbConnect(SQLite(), SQLFile)
  return(db_meta)
}

#Tancar connexió SQLite
closeMetaDB <- function(){
  if ((!is.null(db_meta)) && (dbIsValid(db_meta))){
    dbDisconnect(db_meta)
  }
}

#Guardem objecte GEO a persistència SQLite i Couch 
guardaGEO <- function(objecte,filename='',path='',accession=NULL) {
  #Carreguem la db
  db <- getMetadataDB()
  objGEO = objecte
  classe <- toupper(class(objGEO)) 
  message(paste('guardaGEO, Classe GEO:',classe,sep=' '))
  #Si tenim un list, utilitzem primer element de la llista (cas dels GSE baixats de NCBI)
  if ((classe=='LIST') && (length(objecte)>0)){
    objGEO = objecte[[1]]
    classe=toupper(class(objGEO)) 
  }
  #Si no ens informen directament el nom GEO, el busquem a l'objecte/filename
  if (is.null(accession)){
    name <- nomGEO(objGEO,filename,db)
  } else {
    name = accession
  }  
  uid<-existeixGEO(objGEO,name,db)  
  # Si l'hem trobat no cal afegir-lo
  if (!is.null(uid)){
    return(uid)
  }
  ## Carregar objecte i segons classe posem info general del fitxer a taula del model relacional que correspongui
  if (toupper(class(objGEO))=='GSM'){
     uid<-GeoACouch(objGEO,name) 
     sql = 'INSERT INTO gsm (uid,name,down,path,filename,downdate) VALUES(:uid,:name,:down,:path,:filename,:downdate)'
     valors<-data.frame(uid=uid,
                        name=name,
                        down=1,
                        path=path,
                        filename=filename,
                        downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                        )
     dbSendPreparedQuery(db, sql, bind.data = valors)
  }
  if ((toupper(class(objGEO))=='GSE') || (toupper(class(objGEO))=='EXPRESSIONSET')){
    uid<-GeoACouch(objGEO,name)
    sql = 'INSERT INTO gse (uid,name,down,path,filename,downdate) VALUES(:uid,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       name=name,
                       down=1,
                       path=path,
                       filename=filename,
                       downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                       )
    dbSendPreparedQuery(db, sql, bind.data = valors)
  }
  if (toupper(class(objGEO))=='GPL'){
    uid<-GeoACouch(objGEO,name) 
    sql = 'INSERT INTO gpl (uid,name,down,path,filename,downdate) VALUES(:uid,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       name=name,
                       down=1,
                       path=path,
                       filename=filename,
                       downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                       )
    dbSendPreparedQuery(db, sql, bind.data = valors)
  }    
  if (toupper(class(objGEO))=='GDS'){
    uid<-GeoACouch(objGEO,name)
    sql = 'INSERT INTO gds (uid,name,down,path,filename,downdate) VALUES(:uid,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       name=name,
                       down=1,
                       path=path,
                       filename=filename,
                       downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                       )
    dbSendPreparedQuery(db, sql, bind.data = valors)
  }
  return(uid)
}


#Guardem fitxer format lliure ICO a Couch
guardaNoGEO <- function(dataJSON,filename='',filenamepath='',userid='') {
  #Carreguem la db
  db <- getMetadataDB()
  info<-existeixNoGEO(filename,db)  
  # Si l'hem trobat no cal afegir-lo
  if (!is.null(info)){
    uid=info$uid
    message(paste('Found uid: ',uid,sep=''))
    return(uid)
  }
  ## Guardar fitxer a taula fitxers ICO
  uid<-NoGeoACouch(dataJSON,filename) 
  sql = 'INSERT INTO icofiles (uid,name,path,filename,loaddate,typefile,userowner) VALUES(:uid,:name,:path,:filename,:loaddate,:typefile,:userowner)'
  valors<-data.frame(uid=uid,
                     name=filename,
                     path=filenamepath,
                     filename=filename,
                     loaddate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S'),
                     typefile='NA',
                     userowner=userid
  )
  dbSendPreparedQuery(db, sql, bind.data = valors)
  return(uid)
}  

# Busquem Filename a metadades i si existeix elrecuperem fitxer sencer de Couch
recuperaNoGEO <- function(filename,userid){
  #Carreguem la db
  db <- getMetadataDB()
  info<-existeixNoGEO(filename)  
  # Si no l'hem trobat sortim
  if (is.null(info)){
    message(paste('Not found filename: ',filename,sep=''))
    return(list(FALSE, 'Not found'))
  }
  #Comprovem si usuari hi té accés
  bAcces=FALSE
  #Si és owner, no cal mirar res més
  if(info$userowner==userid){
    bAcces=TRUE
  } else {
    #TODO: utilitzar funcions d'acces per grups
    bAcces=TRUE
  }
  if(isTRUE(bAcces)){
    dataJSON <- CouchANoGEO(info$uid)
    if (dataJSON==FALSE){
       return(list(FALSE,'Unknown error'))  
    }
      
  } else {
    return(list(FALSE,'Unauthorized'))
  }
  return(list(TRUE, dataJSON))
}

#Si nom fitxer el tenim a COuch, descarregar i guardar-lo al filepath especificat
downloadNoGEO <- function(downpath,name,userid){
  #Retorna una llista de 2 elements: 1=True/False, 2=registre Couch/Missatge Error
  res <- recuperaNoGEO(name,userid)
  if(!is.null(res) && is.list(res)){
    if (isTRUE(res[[1]])){
      fitxer_json <- res[[2]]
      res_hex <- fromJSON(fitxer_json) ## CASCA AQUI: Error in file(con, "r") : cannot open the connection
      res_raw <- wkb::hex2raw(res_hex)
      writeBin(res_raw,downpath)
      message(paste('Downloaded file: ',downpath,sep=''))
      return(TRUE)
    } else {
      message(paste('Could not download file: ',res[[2]],sep=''))
      return(FALSE)
    }
  } else {
    return (FALSE)
  }
}



nomGEO <- function(objGEO,filename, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  switch(toupper(class(objGEO)),
         'GSM'={name=objGEO@header$geo_accession},
         'GPL'={name=objGEO@header$geo_accession},
         'GSE'={name=objGEO@header$geo_accession},
         'GDS'={
           #En GDS no hay geo_accession para identificar el nombre del fichero 
           #El nombre lo saco de la serie (GSE) asociada al primer sample, quizá no sea lo mejor
           #Entiendo que dentro de un fichero GSE todos los samples deberían estar asociados a su serie...
           name=objGEO@header$dataset[1]
         },
         'EXPRESSIONSET'={
           #Si es un GSE format Matrix agafem el nom del filename, sembla que no es pot accedir al geo_accession des de l'objecte
           #També afegim sufixe '_matrix' per diferenciar-lo del GSE complet
           pos <- regexpr('_series_matrix.txt',filename)
           if (pos>0){
             name<-substr(filename,1,pos-1)
           } else {
             stop('Invalid GEO file')
           }
           name<-paste(name,'_matrix',sep='')

         },
         stop('Invalid GEO Object')
  )
  return(name)
}

existeixGEO <- function(objGEO,nom, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  #Mirem si tenim l'objecte GEO a la BD JSON amb una query a la BD relacional de metadades 
  nomreg=nom
  switch(toupper(class(objGEO)),
         'GSM'={tablename='gsm'},
         'GPL'={tablename='gpl'},
         'GSE'={tablename='gse'},
         'GDS'={tablename='gds'},
         'EXPRESSIONSET'={
           tablename='gse'
           paste(nomreg,'_matrix',sep='')
         },
         stop('Invalid GEO Object')
  )       
  sql = paste('SELECT * FROM ',tablename,sep='')
  sql = paste(paste(paste(sql,' WHERE name = "',sep=''),nomreg,sep=''),'"',sep='')
  res = dbGetQuery(db, sql)
  #Si ja el tenim retornem uid 
  if (nrow(res)>0){
    uid=res$uid
    return(uid)
  } else {
    return(NULL)
  }
}

existeixNoGEO <- function(nom, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  #Mirem si tenim el fitxer a la BD JSON amb una query a la BD relacional de metadades 
  nomreg=nom
  sql = 'SELECT * FROM icofiles'
  sql = paste0(paste0(paste0(sql,' WHERE name = "'),nom),'"')
  res = dbGetQuery(db, sql)
  #Si el tenim retornem registre sencer
  if (nrow(res)>0){
    return(res)  
  } else {
    return(NULL)
  }
}


#----------------------------------------------------------
# Retorna los datos de un usuario a partir del username
#----------------------------------------------------------
getUserProfile <- function (db,username){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  
  sql = paste(paste("select *, ug.grupid as grupuser
        from usuaris u
        ,		usuari_grup ug
        ,		rols r
        ,		grups g
        where u.username = '",username,sep=''),
        "' and u.id = ug.userid
        and u.rolid = r.id
        and ug.grupid=g.id",sep='')
  
  res = dbGetQuery(db, sql)
  
  if (nrow(res)>0){
    return(res)
  } else {
    return(NULL)
  }
}

#----------------------------------------------------------
# Retorna la lista de roles de la aplicación
# type = no_admin -> todos los roles
# en caso contrario todos
#----------------------------------------------------------
getRoles <- function (db, type){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  if (type=='no_admin'){
    sql="SELECT * FROM rols WHERE id<>1"
  }
  else{
    sql="SELECT * FROM rols"
  }
  
  res = dbGetQuery(db, sql)
  
  if (nrow(res)>0){
    return(res)
  } else {
    return(NULL)
  }
  
}

#----------------------------------------------------------
# Retorna la lista de ficheros propiedad del grupo
#----------------------------------------------------------
getIcofilesGroup <- function (db, idgrupo){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  sql=paste0("SELECT i.uid,i.name,i.path,i.typefile FROM icofiles i, usuari_grup u  WHERE i.userowner = u.userid and u.grupid = '",idgrupo,"'")

  res = dbGetQuery(db, sql)
  
  if (nrow(res)>0){
    return(res)
  } else {
    return(NULL)
  }
  
}

#----------------------------------------------------------
# Retorna la lista de grupos de la aplicación
# param idgroup <> '' -> todos los grupos menos el pasado
# en caso contrario todos
#----------------------------------------------------------
getGrups <- function (db, idgroup=''){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  if (idgroup!=''){
    sql=paste0("SELECT * FROM grups WHERE id<>",idgroup)
  }
  else{
    sql="SELECT * FROM grups"
  }
  
  res = dbGetQuery(db, sql)
  
  if (nrow(res)>0){
    return(res)
  } else {
    return(NULL)
  }
  
}

sendQuery <- function(db, comanda){
  ## Converteixo a UTF-8 les comandes amb enc2utf8. 
  ## No he trobat com fer-ho per defecte tot i tenir RStudio a UTF-8
  comanda_utf8 <- enc2utf8(comanda)
  result <- dbSendQuery(conn = db, comanda_utf8)
  return(result)
}


getQuery <- function(db, comanda){
  ## Converteixo a UTF-8 les comandes amb enc2utf8. 
  ## No he trobat com fer-ho per defecte tot i tenir RStudio a UTF-8
  comanda_utf8 <- enc2utf8(comanda)
  result <- dbGetQuery(conn = db, comanda_utf8)
  return(result)
}

