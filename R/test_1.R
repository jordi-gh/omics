require(Biobase)
#library(shiny)
#library(shinyBS)
library(GEOquery)
library(GEOmetadb)
library(couchDB)
library(DT)
library(RSQLite)
library(utils)
library(wkb) 

#SQLITE
db <- dbConnect(SQLite(), 
                'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')
#dbDisconnect(db)
dbSendQuery(conn = db,
            "CREATE TABLE gsm
            (uid TEXT,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT
             )
            ")
# dbSendQuery(conn = db,"DROP TABLE gsm")
dbGetQuery(db,'Select * from gsm');

sql = paste('SELECT * FROM gpl WHERE name = "','GPL571"',sep='')
res = dbGetQuery(db, sql)
if (nrow(res)>0){
  message('EUREKA! el tenim')
}

#objGEO <- Table(getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\GPL11154.soft",GSEMatrix=false))
objGEO <- Table(getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false))
class(objGEO)
#Meta(objGEO)
## Carregar objecte i segons classe posem a taula del model relacional que correspongui
objGEO <-getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false)
if (class(objGEO)=='GSM'){
   sql = 'INSERT INTO gsm (name,down,path,filename) VALUES(:name,:down,:path,:filename)'
   #sql = 'INSERT INTO GSM (name,down,path,filename) VALUES("test",1,"PATH","filenameee")'
   valors<-data.frame(name='GSM1897275',
                      down=1,
                      path='D:\\Master\\TFM\\Dades\\ICO\\samples',
                      filename='GSM1897275.soft')
   dbSendPreparedQuery(db, sql, bind.data = valors)
}


objGEO <- getGEO(filename="D:\\Master\\TFM\\Dades\\ICO\\samples\\GSM1897275.soft",GSEMatrix=false)
objGEO <- getGEO(filename="D:\\Master\\TFM\\Dades\\Mostres\\GSM320590.soft",GSEMatrix=false)

taula=Table(objGEO)
# Insert a CouchDB
getURL(paste("http://localhost:5984/geodb/","12345",sep=""),
       customrequest="PUT",
       httpheader=c('Content-Type'='application/json'),
       postfields='{\"couchdb\":\"Welcome\",\"version\":\"1.0.1\"}')




source('ConfigLocal.R')
source('C:\\Jordi\\Master\\TFM\\R\\ConfigLocal.R')
source(file.path('C:','Jordi','Master','TFM','R','ConfigLocal.R'))

a<-source('ConfigLocal.R')
a

source(paste(R_dir, 'IncRel.R', sep=path_sep))
source(file.path(R_dir,'IncRel.R'))

source(file.path(dirname(sys.frame(1)$ofile),'ConfigLocal.R'))
source(file.path('.','ConfigLocal.R'))
source('ConfigLocal.R')

wd <- setwd(".")
setwd(wd)

getwd()
source('./ConfigLocal.R',local = TRUE)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
frame_files



LocationOfThisScript = function() # Function LocationOfThisScript returns the location of this .R script (may be needed to source other files in same dir)
{
  this.file = NULL
  # This file may be 'sourced'
  for (i in -(1:sys.nframe())) {
    if (identical(sys.function(i), base::source)) this.file = (normalizePath(sys.frame(i)$ofile))
  }
  
  if (!is.null(this.file)) return(dirname(this.file))
  
  # But it may also be called from the command line
  cmd.args = commandArgs(trailingOnly = FALSE)
  cmd.args.trailing = commandArgs(trailingOnly = TRUE)
  cmd.args = cmd.args[seq.int(from=1, length.out=length(cmd.args) - length(cmd.args.trailing))]
  res = gsub("^(?:--file=(.*)|.*)$", "\\1", cmd.args)
  
  # If multiple --file arguments are given, R uses the last one
  res = tail(res[res != ""], 1)
  if (0 < length(res)) return(dirname(res))
  
  # Both are not the case. Maybe we are in an R GUI?
  return(NULL)
}
current.dir = LocationOfThisScript()

dir <- dirname(parent.frame(2)$ofile)
setwd(dir)


utils::getSrcDirectory()
dir1<-getSrcDirectory()[1]


source(file.path(gb_Rdir, 'IncRel.R'))
db <- getMetadataDB()
sql<-"SELECT * FROM usuaris"
res<-exeQuery(db,sql)
credentials1<-list()
i <- 1
for (nom in res$username){
  credentials1[[nom]] <- res$pwd[i]
  i <- i+1
}

credentials <- res[,c("username","pwd")]
list1 <- as.list(credentials)

list2 <- list("rpena" = "92eb5ffee6ae2fec3ad71c777531578f","dmendez" = "92eb5ffee6ae2fec3ad71c777531578f","jtorresz" = "92eb5ffee6ae2fec3ad71c777531578f")


objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GPL226.soft",GSEMatrix=false)
colnames(Table(objGEO))

#Descarreguem GSE indicant Matrix = False: Ens baixa fitxer associat GPL8300 de 8 Mb
objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE220_series_matrix.txt.gz",GSEMatrix=false)
colnames(Table(objGEO))
Meta(objGEO)
name=objGEO@header$geo_accession

#Descarreguem GSE deixant valor per defecte Matrix=True: Idèntic resultat
objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE220_series_matrix.txt",GSEMatrix=TRUE)
GSMprueba <- GSMList(objGEO)[[1]]
tableGSMprueba <- Table(GSMprueba)[]

#Fitxer GSE SOFT complet
objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE220.soft.gz",GSEMatrix=FALSE)
#Convertir info dels gsms a dataframe
i<-1
vect_acc<-c()
vect_tit<-c()
vect_org<-c()
vect_mol<-c()
for(mostra in objGEO@gsms){
   gsmaux<-objGEO@gsms[[i]]
   i<-i+1
   message(gsmaux@header$geo_accession)
   vect_acc<-c(vect_acc,c(gsmaux@header$geo_accession))
   vect_tit<-c(vect_tit,c(gsmaux@header$title))
   vect_org<-c(vect_org,c(gsmaux@header$organism_ch1))
   vect_mol<-c(vect_mol,c(gsmaux@header$molecule_ch1))
}
#Creem Data Frame
df_gsms <- data.frame(vect_acc,vect_tit,vect_org,vect_mol)
names(df_gsms)<-c("GEO Accession","Title","Organism","Molecule")

objGEO@gsms

gsm1<-objGEO@gsms[[1]]
gsm1@header

#Descarregar GSE, en forma de expressionset (matrix = true) i en forma normal (matrix=false)
gse2553 <- getGEO('GSE2553',GSEMatrix=TRUE)  #8 MB

gse2553 <- getGEO('GSE2553',GSEMatrix=FALSE) #374 MB

objGEO <- getGEO("GSE220",GSEMatrix = FALSE)
objGEO <- getGEO("GSE220",GSEMatrix = TRUE)

  
GSM1 <-getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSM101.soft",GSEMatrix=false)

GDS1 <-getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GDS100.soft.gz",GSEMatrix=false)
class(GDS1)
name=GDS1@header$geo_accession
name=GDS1@header$dataset[1]

GDS2 <-getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GDS4352.soft.gz",GSEMatrix=false)
class(GDS2)
name=GDS2@header$geo_accession
name=GDS2@header$dataset[1]

GSE1 <-getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE781-GPL97_series_matrix.txt")
class(GSE1)

#Grafiques
gse = getGEO('GSE976')[[1]]
sdN = 3
sds = apply(log2(exprs(gse)+0.0001),1,sd)
library(gplots)
heatmap.2(log2(exprs(gse)+0.0001)[sds>sdN,],trace='none',scale='row')

# Visualització pollo Dani dels GSE
objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE220.soft.gz",GSEMatrix=FALSE)
pdExperimentNCBI <- pData(phenoData(objGEO[[1]]))  ##Peta: Error in objGEO[[1]] : this S4 class is not subsettable
pdExperimentNCBI <- pData(phenoData(objGEO)) ## PEta: unable to find an inherited method for function ‘phenoData’ for signature ‘"GSE"’ 

objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GSE220.soft.gz",GSEMatrix=TRUE)
pdExperimentNCBI <- pData(phenoData(objGEO[[1]]))  ##Peta: Error in objGEO[[1]] : this S4 class is not subsettable

#Carregar file no GEO
nomfitxer <- "C:\\Jordi\\Master\\TFM\\DEV\\R\\dadesICO\\Gene expression_ log2_test.xlsx"
nomfitxer <- "C:\\Jordi\\Master\\TFM\\DEV\\README.md"
nomfitxer2 <- "C:\\Jordi\\Master\\TFM\\DEV\\R\\dadesICO\\Gene expression_ log2_test_copia.xlsx"
nomfitxer2 <- "C:\\Jordi\\Master\\TFM\\DEV\\README_copia.md"
info <- file.info(nomfitxer)
#mirem tamany
info$size
#Llegim en format raw el fitxer sencer
fitxer_raw <- readBin(con=nomfitxer,"raw",info$size)
#fitxer Hex
fitxer_hex <- raw2hex(fitxer_raw,sep='')
#Convertim a string de bytes hexadecimal i passem a JSON
fitxer_json <- toJSON(fitxer_hex)
#Conversió inversa
#h <- sapply(seq(1, nchar(fitxer_hex), by=2), function(x) strtoi(substr(fitxer_hex, x, x+1),16))
res_hex <- fromJSON(fitxer_json)
res_raw <- wkb::hex2raw(res_hex)
writeBin(res_raw,nomfitxer2)

# User profiles
db <- getMetadataDB()
perfil <- getUserProfile(db,'jtorresz')

#Problema colnames no existents
objGEO <- getGEO(filename="C:\\Jordi\\Master\\TFM\\DEV\\R\\BD\\GPL1111.soft",GSEMatrix=false)
colnames(Table(objGEO))

db <- getMetadataDB()
res_roles <- getRoles(db,'no_admin')


#Download fitxer de couch
res <- downloadICO("C:\\Jordi\\Master\\TFM\\DEV\\R\\dadesICO\\Gene expression_ log2_test_COPIA.xlsx","Gene expression_ log2_test.xlsx",3)
