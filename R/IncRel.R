library(GEOquery)
library(GEOmetadb)
library(RSQLite)
source('D:\\Master\\TFM\\dev\\R\\IncCouch.R')

db <- dbConnect(SQLite(), 'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')

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
#   sql = paste('SELECT * FROM ',tablename,sep='')
#   sql = paste(paste(paste(sql,' WHERE name = "',sep=''),name,sep=''),'"',sep='')
#   res = dbGetQuery(db, sql)
#   #Si ja el tenim retornem uid 
#   if (nrow(res)>0){
#     uid=res$uid
#     #message('TROBAT')
#     return(uid)
#   }
  res=buscaAMetadades(tablename,list(nom=name))
  if (typeof(res)=='list'){
      return(res$uid)
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

#Mirem si tenim un fitxer descarregat buscant a metadades SQLite
#Taula indica la taula (gpl,gds..) on hem de buscar
#uidnom es un named list on podem tenir qualsevol dels dos valors(nom,uid). 
buscaAMetadades <- function(taula,uidnom) {
  #Connectar a BD SQLite
  db <- dbConnect(SQLite(), 'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')
  uid = uidnom[['uid']]
  nom = uidnom[['nom']]
  if ((is.null(uid))&&(is.null(nom))){ 
    return(FALSE)
  }
  sql = paste('SELECT * FROM',taula,sep=' ')
  if (!is.null(uid)) {
    sql = paste(paste(paste(sql,'WHERE uid="',sep=' '),uid,sep=''),'"',sep='')
  } else {
    sql = paste(paste(paste(sql,'WHERE name="',sep=' '),nom,sep=''),'"',sep='')
  }
  res = dbGetQuery(db, sql)
  if (nrow(res)>0){
    uid=res$uid[[1]]
    nom=res$name[[1]]
    return(list(uid=uid,nom=nom))
  } else {  
    return(FALSE)
  }
  
}
