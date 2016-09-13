library(GEOquery)
library(GEOmetadb)
library(RSQLite)
library(wkb) 

source(file.path(gb_Rdir, 'IncCouch.R'))

#----------------------------------------------------------
# Retorna instància singleton de DB Metadata
#----------------------------------------------------------
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

#----------------------------------------------------------
#Guardem objecte GEO a persistència SQLite i Couch 
#----------------------------------------------------------
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
  reg<-existeixGEO(objGEO,name,db)  
  uid<-reg$uid
  # Si l'hem trobat no cal afegir-lo
  if (!is.null(uid)){
    return(uid)
  }
  ## Carregar objecte i segons classe posem info general del fitxer a taula del model relacional que correspongui
  if (toupper(class(objGEO))=='GSM'){
     resp<-GeoACouch(objGEO,name) 
     uid<-resp$id
     lastrev<-resp$rev
     
     sql = 'INSERT INTO gsm (uid,lastrev,name,down,path,filename,downdate) VALUES(:uid,:lastrev,:name,:down,:path,:filename,:downdate)'
     valors<-data.frame(uid=uid,
                        lastrev=lastrev,
                        name=name,
                        down=1,
                        path=path,
                        filename=filename,
                        downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                        )
     dbSendPreparedQuery(db, sql, bind.data = valors)
  }
  if ((toupper(class(objGEO))=='GSE') || (toupper(class(objGEO))=='EXPRESSIONSET')){
    resp<-GeoACouch(objGEO,name) 
    uid<-resp$id
    lastrev<-resp$rev
    sql = 'INSERT INTO gse (uid,lastrev,name,down,path,filename,downdate) VALUES(:uid,:lastrev,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       lastrev=lastrev,
                       name=name,
                       down=1,
                       path=path,
                       filename=filename,
                       downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
                       )
    dbSendPreparedQuery(db, sql, bind.data = valors)
  }
  if (toupper(class(objGEO))=='GPL'){
    resp<-GeoACouch(objGEO,name) 
    uid<-resp$id
    lastrev<-resp$rev
    sql = 'INSERT INTO gpl (uid,lastrev,name,down,path,filename,downdate) VALUES(:uid,:lastrev,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       lastrev=lastrev,
                       name=name,
                       down=1,
                       path=path,
                       filename=filename,
                       downdate=format(Sys.time(),format='%Y/%m/%d %H:%M:%S')
    )
    dbSendPreparedQuery(db, sql, bind.data = valors)
  }    
  if (toupper(class(objGEO))=='GDS'){
    resp<-GeoACouch(objGEO,name) 
    uid<-resp$id
    lastrev<-resp$rev
    sql = 'INSERT INTO gds (uid,lastrev,name,down,path,filename,downdate) VALUES(:uid,:lastrev,:name,:down,:path,:filename,:downdate)'
    valors<-data.frame(uid=uid,
                       lastrev=lastrev,
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

#----------------------------------------------------------
#Guardem fitxer format lliure ICO a Couch i retornem uid + vista en format dataframe
#----------------------------------------------------------
guardaICO <- function(inFile,userid) {
  #Carreguem la db
  db <- getMetadataDB()
  #Mirem si es tracta d'un Format GEO per fer visualització específica
  res <- tryCatch({
    filename<-inFile$datapath
    #message(paste0('DBG1: ',inFile$datapath))
    objGEO <- getGEO(filename=filename)
    #Retornem info de visualitzacio de l'objecte GEO en format dataframe 
    vista <- dfView(objGEO)
    tipus<-toupper(class(objGEO))
  }, error = function(err){
    #Retornem info de visualització: Fitxer format desconegut
    vista <- data.frame('File'=filename,'Content'='Unknown format')
    tipus<-'NA'
  })  
  
  #Guardem a persistència ICO 
  #Recopilem informació del fitxer
  info <- file.info(inFile$datapath)
  #Tamany en bytes
  tamany <- info$size  
  #Llegim en format raw el fitxer sencer
  fitxer_raw <- readBin(con=inFile$datapath,"raw",tamany)
  #Convertim a string de bytes hexadecimal i passem a JSON
  dataJSON <- toJSON(raw2hex(fitxer_raw,sep=''))
  filename <- inFile$name
  filenamepath <- inFile$datapath

  info<-existeixICO(filename,db)  
  # Si l'hem trobat no cal afegir-lo
  if (!is.null(info)){
    uid=info$uid
    message(paste('Found uid: ',uid,sep=''))
    return(list(TRUE,uid,vista))
    
  }
  ## Guardar fitxer a taula fitxers ICO
  resp<-ICOACouch(dataJSON,filename) 
  uid<-resp$id
  lastrev<-resp$rev
  message(paste0('UID assignat: ',uid))
  
  sql = 'INSERT INTO icofiles (uid,lastrev,name,path,filename,loaddate,typefile,userowner) VALUES(:uid,:lastrev,:name,:path,:filename,:loaddate,:typefile,:userowner)'
  valors<-data.frame(uid=uid,
                     lastrev=lastrev,
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

#--------------------------------------------------------------------------------------------
# Busquem Filename a metadades i si existeix elrecuperem fitxer sencer de Couch
#--------------------------------------------------------------------------------------------
recuperaICO <- function(filename,userid){
  #Comprovem si existeix i usuari hi té accés
  #accessibleICO retorna una llista de 2 elements: 1=True/False, 2=registre metadades/Missatge Error
  res=accessibleICO(filename,userid)
  # Si no l'hem trobat sortim
  if ((res[[1]])==FALSE){
    message(paste(res[[2]],filename,sep=': '))
    return(list(FALSE, res[[2]]))
  }
  bAcces = res[[1]]
  if(isTRUE(bAcces)){
    dataJSON <- CouchAICO(info$uid)
    if (dataJSON==FALSE){
       return(list(FALSE,'Unknown error'))  
    }
      
  } else {
    return(list(FALSE,'Unauthorized'))
  }
  return(list(TRUE, dataJSON))
}

#----------------------------------------------------------------------------------
#Busquem ICO a metadades i comprovem que existeix i que és accessible per usuari
#----------------------------------------------------------------------------------
accessibleICO <- function(nom,userid){
  #Carreguem la db
  db <- getMetadataDB()
  info<-existeixICO(nom)  
  #Si no existeix registre, hem acabat
  if (is.null(info)){
    return(list(FALSE,'File not found'))
  }
  #Si és owner, no cal mirar res més
  if(info$userowner==userid){
    return(list(TRUE,info))
  } else {
    uid=info$uid
    #Mirem si té accés per grup
    grups_fitx <- grupsFitxer(uid,db)
    grups_user <- grupsUsuari(userid,db)
    #Si els dos conjunts tenen algun grup comú: usuari té accés
    if(length(intersect(grups_fitx,grups_user))>0){
      return(list(TRUE,info))
    } else {
      return(list(FALSE,'Insufficient permissions'))
    }
  }
  
}

#--------------------------------------------------------------------------------------------
#Retorna els grups que tenen accés a un fitxer ICO
#--------------------------------------------------------------------------------------------
grupsFitxer <- function(uid,db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  
  sql = paste(paste('SELECT * FROM accessfiles where uidfile=',uid,sep="'"),"'",sep="")
  message(sql)
  res = getQuery(db, sql)
  #Retornem llista de grups que tenen acces
  if (nrow(res)>0){
    #TODO: Cal revisar dateaccess, datedenied
    message()
    df <- res['idgroup'] 
    #Convertim a list
    return(df[,'idgroup'])
  } else {
    return(list())
  }  
}

#--------------------------------------------------------------------------------------------
#Retorna els grups als que pertany un usuari
#--------------------------------------------------------------------------------------------
grupsUsuari <- function(userid,db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  
  sql = paste0('SELECT * FROM usuari_grup where userid=',userid)
  res = getQuery(db, sql)
  #Retornem llista de grups on tenim l'usuari
  if (nrow(res)>0){
    df <- res['grupid']
    #Convertim a list
    return(df[,'grupid'])
  } else {
    return(list())
  }  
}

#--------------------------------------------------------------------------------------------
#Descarregar a disc fitxer ICO (format lliure) guardat a Couch
#--------------------------------------------------------------------------------------------
downloadICO <- function(downpath,name,userid){
  #recuperaICO retorna una llista de 2 elements: 1=True/False, 2=registre Couch/Missatge Error
  res <- recuperaICO(name,userid)
  if(!is.null(res) && is.list(res)){
    if (isTRUE(res[[1]])){
      fitxer_json <- res[[2]]
      res_hex <- fromJSON(fitxer_json) 
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


#--------------------------------------------------------------------------------------------
#Retorna el nom GEO d'un objecte GEO.
#Si l'objecte no té propietat accession (expressionset), utilitzem el filename 
#--------------------------------------------------------------------------------------------
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

#--------------------------------------------------------------------------------------------
#Mirem si tenim descarregat objecte GEO utilitzant metadades. 
#Si el trobem, retornem registre de metadades
#--------------------------------------------------------------------------------------------
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
  #Si ja el tenim retornem registre 
  if (nrow(res)>0){
    return(res)
  } else {
    return(NULL)
  }
}

#--------------------------------------------------------------------------------------------
#Mirem si tenim descarregat fitxer ICO utilitzant metadades. 
#Si el trobem, retornem registre amb tota la info
#--------------------------------------------------------------------------------------------
existeixICO <- function(nom, db){
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

#--------------------------------------------------------------------------------------------
#Mirem si tenim GEO descarregat a partir del nom i tipus, mirant metadades
#Retorna True/False
#--------------------------------------------------------------------------------------------
inDataCatalog <- function(nom, type, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  #Mirem si tenim el fitxer a la BD JSON amb una query a la BD relacional de metadades
  nomreg=nom
  switch(toupper(type),
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
  #Retornem bool TRUE si és al catàleg ICO, FALSE si no hi és.
  if (nrow(res)>0){
    return(TRUE)  
  } else {
    return(FALSE)
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

#----------------------------------------------------------
# Retorna numero de ficheros para mostrarlo en la Home
#----------------------------------------------------------
getDataHome <- function (db, username){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  aDataHome<-array()
  
  #Datos de ficheros NCBI
  sql="select count(1) as gdscount from gds"
  res = dbGetQuery(db, sql)
  aDataHome['gds'] <- res$gdscount
  sql="select count(1) as gplcount from gpl"
  res = dbGetQuery(db, sql)
  aDataHome['gpl'] <- res$gplcount
  sql="select count(1) as gsmcount from gsm"
  res = dbGetQuery(db, sql)
  aDataHome['gsm'] <- res$gsmcount
  sql="select count(1) as gsecount from gse"
  res = dbGetQuery(db, sql)
  aDataHome['gse'] <- res$gsecount
  
  #Datos de ficheros ICO
  #Numero de ficheros de mi grupo de trabajo
  #La subquery saca todos los usuarios del grupo
  sql=paste0("select count(1) as icoownercount
              from icofiles
              where userowner in (
                select userid from usuari_grup where grupid in (select distinct ug.grupid from 	usuari_grup ug, usuaris u where ug.userid = u.id and u.username = '",username,"')
              )")
  res = dbGetQuery(db, sql)
  aDataHome['icoowner'] <- res$icoownercount
  
  #Numero de ficheros que han compartido al grupo del usuario pasado como parametro
  #Recordar que en accessfiles no está el grupo del owner del fichero
  sql=paste0("select count(1) as icosharecount from accessfiles where idgroup in (
              select distinct ug.grupid from 	usuari_grup ug, usuaris u where ug.userid = u.id and u.username = '",username,"')")
  res = dbGetQuery(db, sql)
  aDataHome['icoshare'] <- res$icosharecount
  
  return(aDataHome)
}

#----------------------------------------------------------
# Retorna todos los ficheros de tipo typefile a los que tiene el acceso 
# el usuario (tanto ficheros typefile NCBI como ICO tipo typefile como
# ICO tipo typefile compartidos con el grupo del usuario)
#----------------------------------------------------------
myXfiles <- function (db, username,typefile){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  sqlncbi=paste0("select filename, uid from ", typefile)
  
  sqlmy=paste0("select filename, uid
                from icofiles
                where userowner in (
                  select userid 
                  from usuari_grup 
                  where grupid in ( select distinct ug.grupid 
                                    from 	usuari_grup ug, usuaris u 
                                    where ug.userid = u.id and u.username = '",username,"')
                )
               and typefile='",typefile,"'")
 
  sqlshare=paste0("select filename, uid
                   from icofiles
                   where uid in ( select uidfile as icosharecount 
                                  from accessfiles 
                                  where idgroup in (select distinct ug.grupid 
                                                    from usuari_grup ug, usuaris u 
                                                    where ug.userid = u.id and u.username = '",username,"'))
                  and typefile='",typefile,"'")
  
  
  sql = paste0(sqlncbi ," UNION ", sqlmy ," UNION ", sqlshare)
  res = dbGetQuery(db, sql)

  return(res)
   
}

#----------------------------------------------------------
# Elimina un fitxer ICO de les metadades i el seu document de Couch
#----------------------------------------------------------
eliminaICO <- function(nom, userid, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  #Mirem si tenim el fitxer a la BD JSON i si tenim permisos
  #accessibleICO retorna una llista de 2 elements: 1=True/False, 2=registre metadades/Missatge Error
  res=accessibleICO(nom,userid)
  # Si no l'hem trobat sortim
  if ((res[[1]])==FALSE){
    error<-paste(res[[2]],nom,sep=': ')
    message(error)
    return(list(FALSE, error))
  }
  registre <- res[[2]]
  uid=registre$uid
  rev=registre$lastrev
  #Eiminem registre de Couch
  eliminaCouch(uid,rev)
  #Eliminem registre de la BD relacional (metadades)
  sql=paste0(paste0(paste0('DELETE FROM icofiles WHERE name = "'),nom),'"')
  res = dbSendQuery(db, sql)
  return(res)
}  



#----------------------------------------------------------
# Elimina un fitxer GEO de les metadades i el seu document de Couch
#----------------------------------------------------------
eliminaGEO <- function(objGEO, nom, db){
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  } 
  #Mirem si tenim el fitxer a la BD JSON 
  #No cal mirar permisos, els fitxers GEO són públics
  reg=existeixGEO(objGEO, nom, db)
  # Si no l'hem trobat sortim
  if (is.null(reg)){
    error<-paste('File not found',nom,sep=': ')
    message(error)
    return(list(FALSE, error))
  }
  uid=reg$uid
  rev=reg$lastrev
  #Eiminem registre de Couch
  eliminaCouch(uid,rev)
  #Eliminem registre de la BD relacional (metadades)
  nomreg<-nom
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
  sql=paste0(paste0(paste0(paste0('DELETE FROM ',tablename),' WHERE name = "'),nomreg),'"')
  res = dbSendQuery(db, sql)
  return(res)
}  

#----------------------------------------------------------
# Retorna los ficheros propios y compartidos para el catalogo ICO
#----------------------------------------------------------
getCatalogoICO <- function (db, username, type){
  
  if(missing(db)) {
    #Carreguem la db
    db <- getMetadataDB()
  }
  
  if (type=='owner'){
    #Ficheros de mi grupo de trabajo
    #La subquery saca todos los usuarios del grupo
    sql=paste0("select i.filename, i.loaddate, i.typefile , u.username as owner
              from icofiles i
              ,    usuaris u
              where i.userowner in (
                select userid from usuari_grup where grupid in (select distinct ug.grupid from 	usuari_grup ug, usuaris u where ug.userid = u.id and u.username = '",username,"')
              ) 
             and i.userowner = u.id")
    res = dbGetQuery(db, sql)
  }
  
  if (type=='shared'){
    #Ficheros que han compartido al grupo del usuario pasado como parametro
    #Recordar que en accessfiles no está el grupo del owner del fichero
    sql=paste0("select i.filename, i.loaddate, i.typefile , u.username as owner
              from icofiles i
              ,    usuaris u
              where i.uid in (select uidfile
              from accessfiles where idgroup in (
              select distinct ug.grupid from 	usuari_grup ug, usuaris u where ug.userid = u.id and u.username = '",username,"')) 
              and i.userowner = u.id")
    res = dbGetQuery(db, sql)
  }
  
  return(res)
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

