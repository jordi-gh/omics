#Funcions de conversió i tractament dels objectes GEO

#Retornar info dels GSMs dins del GSE en format dataframe
getGSMsFromGSE <- function(GSE){
  if (toupper(class(GSE))!='GSE'){
    return(FALSE)
  }
  #Convertir info dels gsms a dataframe
  vect_acc<-c()
  vect_tit<-c()
  vect_org<-c()
  vect_mol<-c()
  i<-1
  for(sample in GSE@gsms){
    gsmaux<-objGEO@gsms[[i]]
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