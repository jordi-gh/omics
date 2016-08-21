library(GEOquery)
library(GEOmetadb)
library(RSQLite)


source(file.path(gb_Rdir, 'IncCouch.R'))


getMetadataDB <- function(){
  SQLFile = file.path(gb_Rdir, 'metadata.sqlite')
  db <- dbConnect(SQLite(), SQLFile)
  return(db)
}

guardaFitxer <- function(objGEO,filename,path) {
  #Mirem si tenim el fitxer a la BD JSON amb una query a la BD relacional de metadades 
  name=objGEO@header$geo_accession
  switch(toupper(class(objGEO)),
       'GSM'={tablename='gsm'},
       'GPL'={tablename='gpl'},
       'GSE'={tablename='gse'},
       'GDS'={tablename='gds'},
       stop('Invalid GEO Object')
  )       
  sql = paste('SELECT * FROM ',tablename,sep='')
  sql = paste(paste(paste(sql,' WHERE name = "',sep=''),name,sep=''),'"',sep='')
  res = dbGetQuery(db, sql)
  #Si ja el tenim retornem uid 
  if (nrow(res)>0){
    uid=res$uid
    #message('TROBAT')
    return(uid)
  }
  ## Carregar objecte i segons classe posem info general del fitxer a taula del model relacional que correspongui
  if (class(objGEO)=='GSM'){
     #name=objGEO@header$geo_accession
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
  if (class(objGEO)=='GSE'){
    #name=objGEO@header$geo_accession
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
  if (class(objGEO)=='GPL'){
    #name=objGEO@header$geo_accession
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
  if (class(objGEO)=='GDS'){
    #name=objGEO@header$geo_accession
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
