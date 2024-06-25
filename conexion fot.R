library(tidyverse)
library("RMySQL")

# Conectar a la bd ------
conFot <- dbConnect(MySQL(), user = "fotuser", password = "USer89h#23T+2aeF07", 
                       dbname = "fot", host = "144.217.19.237")
# listar las tablas en la base de datos -----
dbListTables(conFot)

#cargar los datos en data frame local ----
db_data_excluded<-dbGetQuery(conFot, "select *  from data_excluded")

db_data_no_excluded<-dbGetQuery(conFot, "select *  from data_no_excluded")

#ver estructura de tablas
str(db_data_excluded)
str(db_data_no_excluded)


dbDisconnect(conFot)
