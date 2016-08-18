library(RSQLite)
## Carregar variables d'entorn local
source('ConfigLocal.R')

SQLFile = file.path(gb_Rdir, 'metadata.sqlite')
db <- dbConnect(SQLite(), SQLFile)

dbSendQuery(conn = db,
            "CREATE TABLE gsm
            (uid TEXT,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT,
             downdate TEXT
             )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gse
            (uid TEXT,
             name TEXT,
             down INT,
             path TEXT,
             filename TEXT,
             downdate TEXT
             )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gpl
            (uid TEXT,
            name TEXT,
            down INT,
            path TEXT,
            filename TEXT,
            downdate TEXT
            )
            ")
dbSendQuery(conn = db,
            "CREATE TABLE gds
            (uid TEXT,
            name TEXT,
            down INT,
            path TEXT,
            filename TEXT,
            downdate TEXT
            )
            ")

dbDisconnect(db)

