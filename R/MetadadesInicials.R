library(RSQLite)
## Carregar variables d'entorn local
source('ConfigLocal.R')
## Carregar funcions SQLite
source(file.path(gb_Rdir, 'IncRel.R'))

db <- getMetadataDB()

## Omplir usuaris, i rols per defecte
sql<-"INSERT INTO rols VALUES(1,'admin','Administrador')"
sendQuery(db,sql)

sql<-"INSERT INTO rols VALUES(2,'coord','Coordinador')"
sendQuery(db,sql)

sql<-"INSERT INTO rols VALUES(3,'user','Usuari')"
sendQuery(db,sql)

sql<-"INSERT INTO usuaris VALUES(1,'rpena','58df81bda18a035861c8f70ea25f7eed','Raúl','Peña','Cabeza','rpena@uic.es',NULL,1)"
sendQuery(db,sql)

sql<-"INSERT INTO usuaris VALUES(2,'dmendez','6db0aeb9bff420a5169026940f2c5a54','Daniel','Méndez','Martí','dmendez@uic.es',NULL,1)"
sendQuery(db,sql)

sql<-"INSERT INTO usuaris VALUES(3,'jtorresz','e5472d7e14c150dab0c8deda3aa9032e','Jordi','Torres','Zapata','jtorresz@uic.es',NULL,1)"
sendQuery(db,sql)

sql<-"INSERT INTO grups (id,nomgrup,descripcio) VALUES (1,'Grup1','Grup Recerca1')"
sendQuery(db,sql)

sql<-"INSERT INTO grups (id,nomgrup,descripcio) VALUES (2,'Grup2','Grup Recerca2')"
sendQuery(db,sql)

sql<-"INSERT INTO grups (id,nomgrup,descripcio) VALUES (3,'Grup3','Grup Recerca3')"
sendQuery(db,sql)

sql<-"INSERT INTO usuari_grup (userid,grupid) VALUES (1,'1')"
sendQuery(db,sql)

sql<-"INSERT INTO usuari_grup (userid,grupid) VALUES (2,'2')"
sendQuery(db,sql)

sql<-"INSERT INTO usuari_grup (userid,grupid) VALUES (3,'1')"
sendQuery(db,sql)

dbDisconnect(db)

