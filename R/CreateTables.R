library(RSQLite)

dir = '/home/daniel/Documentos/màster del universo/ICO/omics/R'
db <- dbConnect(SQLite(), paste(dir, '/Test.sqlite', sep=""))

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
