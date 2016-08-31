library(RSQLite)
## Carregar variables d'entorn local
source('ConfigLocal.R')
source(file.path(gb_Rdir, 'IncRel.R'))

db <- getMetadataDB()

dbSendQuery(conn = db,
            "CREATE TABLE gsm
            (uid TEXT PRIMARY KEY,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT,
             downdate TEXT
             )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gse
            (uid TEXT PRIMARY KEY,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT,
             downdate TEXT
             )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gpl
            (uid TEXT PRIMARY KEY,
            name TEXT,
            down INT,
            path TEXT,
            filename TEXT,
            downdate TEXT
            )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gds
            (uid TEXT PRIMARY KEY,
            name TEXT,
            down INT,
            path TEXT,
            filename TEXT,
            downdate TEXT
            )
            ")

## Crear taules d'usuaris, grups i rols
dbSendQuery(conn = db,
            "CREATE TABLE rols
            (id INT PRIMARY KEY AUTOINCREMENT,
            nomrol TEXT,
            descripcio TEXT
            )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE usuari_grup
            (userid INT,
            grupid TEXT,
            CONSTRAINT usergrup_unique UNIQUE (userid,grupid)
            )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE usuaris
            (id INT PRIMARY KEY AUTOINCREMENT,
            username TEXT,
            pwd TEXT,
            nom TEXT,
            cognom1 TEXT,
            cognom2 TEXT,
            mail TEXT,
            lastlog TEXT,
            rolid INT,
            CONSTRAINT username_unique UNIQUE (username)
            FOREIGN KEY (rolid) REFERENCES rols(id)
            )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE grups
            (id INT PRIMARY KEY AUTOINCREMENT,
            nomgrup TEXT,
            descripcio TEXT
            )
            ")

dbSendQuery(conn = db,
            "CREATE TABLE icofiles
            (uid TEXT PRIMARY KEY,
             name TEXT,
             path TEXT,
             filename TEXT,
             loaddate TEXT,
             typefile TEXT, 
             userowner INT,
             FOREIGN KEY (userowner) REFERENCES usuaris(id)
             )
")

dbSendQuery(conn = db,
            "CREATE TABLE accessfiles
            (id INT PRIMARY KEY AUTOINCREMENT,
             uidfile TEXT,
             idgroup INT,  
             dateaccess TEXT,
             datedenied TEXT,
             FOREIGN KEY (uidfile) REFERENCES icofiles(uid)
             FOREIGN KEY (idgroup) REFERENCES grups(id)
             )
")
dbDisconnect(db)

