library(GEOquery)
library(GEOmetadb)
library(RSQLite)


source(file.path(gb_Rdir, 'IncCouch.R'))


getMetadataDB <- function(){
  SQLFile = file.path(gb_Rdir, 'metadata.sqlite')
  db <- dbConnect(SQLite(), SQLFile)
  return(db)
}

guardaFitxer <- function(objGEO,filename,path,accession=NULL) {
  #Carreguem la db
  db <- getMetadataDB()
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

sendQuery <- function(db, comanda){
  ## Converteixo a UTF-8 les comandes amb enc2utf8. 
  ## No he trobat com fer-ho per defecte tot i tenir RStudio a UTF-8
  comanda_utf8 <- enc2utf8(comanda)
  result <- dbGetQuery(conn = db, comanda_utf8)
  return(result)
}


getQuery <- function(db, comanda){
  ## Converteixo a UTF-8 les comandes amb enc2utf8. 
  ## No he trobat com fer-ho per defecte tot i tenir RStudio a UTF-8
  comanda_utf8 <- enc2utf8(comanda)
  result <- dbGetQuery(conn = db, comanda_utf8)
  return(result)
}

