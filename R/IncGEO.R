#Funcions de conversió i tractament dels objectes GEO

#Retornar dataframe de l'objecte per visualitzacions
dfView <- function(objecte){
   objGEO = objecte
   classe <- toupper(class(objGEO)) 
   message(paste('dfView, Classe GEO:',classe,sep=' '))
   #Si tenim un list, utilitzem primer element de la llista (cas dels GSE baixats de NCBI)
   if ((classe=='LIST') && (length(objecte)>0)){
      objGEO = objecte[[1]]
      classe=toupper(class(objGEO)) 
   }
   switch(classe,
         'GSM'={df<-dfViewGSM(objGEO)},
         'GPL'={df<-dfViewGPL(objGEO)},
         'GSE'={df<-dfViewGSE(objGEO)},
         'GDS'={df<-dfViewGDS(objGEO)},
         'EXPRESSIONSET'={df<-dfViewExprSet(objGEO)},
         stop('Invalid GEO Object')
   )   
   return(df)
}  

#veure GSE en format dataframe
dfViewGSE <- function(GSE) {
  if (toupper(class(GSE))!='GSE'){
    return(FALSE)
  }
  #Intentem obtenir info GSMs amb funcions GEO, si falla ho fem a ma
  res <- tryCatch({
    return(pData(phenoData(GSE)))
  }, error = function(err){
       message(paste('#No podem obtenir pData del GSE: ',err,sep=''))
       message('Obtenim llista de samples manualment')
  })  
  
  #Convertir info dels gsms a dataframe
  vect_acc<-c()
  vect_tit<-c()
  vect_org<-c()
  vect_mol<-c()
  i<-1
  for(sample in GSE@gsms){
    gsmaux<-GSE@gsms[[i]]
    i<-i+1
    vect_acc<-c(vect_acc,c(gsmaux@header$geo_accession))
    vect_tit<-c(vect_tit,c(gsmaux@header$title))
    vect_org<-c(vect_org,c(gsmaux@header$organism_ch1))
    vect_mol<-c(vect_mol,c(gsmaux@header$molecule_ch1))
  }
  #Creem Data Frame
  df_gsms <- data.frame(vect_acc,vect_tit,vect_org,vect_mol)
  #Afegim noms de columnes
  names(df_gsms)<-c("Sample","Title","Organism","Molecule")
  return(df_gsms)
}

dfViewGPL <- function(GPL){
    #Mostra GPL a GEO:
    #http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL341
    if (toupper(class(GPL))!='GPL'){
      return(FALSE)
    }
    #Columnes a mostrar primer per defecte
    colsdef <- c("ID","Gene Symbol","ENTREZ_GENE_ID","Gene name","Description")
    colstaula <- colnames(Table(GPL))
    colsview <- validaColumnes(colsdef,colstaula)
    df <- Table(GPL)[,colsview]
    return(df)
}

dfViewGDS <- function(GDS){
  if (toupper(class(GDS))!='GDS'){
    return(FALSE)
  }
  #Columnes a mostrar primer per defecte
  colsdef <- c("ID_REF","IDENTIFIER")
  colstaula <- colnames(Table(GDS))
  colsview <- validaColumnes(colsdef,colstaula)
  df <- Table(GDS)[,colsview]
  return(df)
}

dfViewGSM <- function(GSM){
  #Mostra GSM a GEO
  #http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM81022
  if (toupper(class(GSM))!='GSM'){
    return(FALSE)
  }
  colsdef <- c("ID_REF","VALUE")
  colstaula <- colnames(Table(GSM))
  colsview <- validaColumnes(colsdef,colstaula)
  df <- Table(GSM)[,colsview]
  return(df)
}

dfViewExprSet <- function(ExprSet){
  if (toupper(class(ExprSet))!='EXPRESSIONSET'){
    return(FALSE)
  }
  colsdef <- c("geo_accession","title","molecule_ch1","organism_ch1","data_row_count")
  colstaula <- colnames(pData(ExprSet))
  colsview <- validaColumnes(colsdef,colstaula)
  df <- pData(ExprSet)[,colsview]
  return(df)
}

#Validem les columnes a mostrar amb les de l'objecte GEO i si no hi són les eliminem
#Després afegim la resta de columnes disponibles a l'objecte
validaColumnes <- function(colsval, colsdisp) {
  #Eliminem de les columnes a validar les que no hi siguin al fitxer
  colsval <- intersect(colsval,colsdisp)
  #Afegim resta de columnes disponibles al fitxer
  colsview <- c(colsval,setdiff(colsdisp,colsval))
  return(colsview)
}


# type <- substr(input$experimentupload, 0, 3)
# if (type == 'GPL') cols <- c("ID", "Gene name") #c("ID", "Gene Symbol", "ENTREZ_GENE_ID")
# else if (type == 'GSM') cols <- c("ID_REF",	"VALUE")
# else if (type == 'GDS') cols <- c("ID_REF",	"IDENTIFIER")
# 
# if (type == 'GSE') exprs(ExperimentNCBI[[1]])
# else Table(ExperimentNCBI)[,cols]

