library(RSQLite)

db <- dbConnect(SQLite(), 
                'D:\\Master\\TFM\\R\\ShinyApp\\Test.sqlite')

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
