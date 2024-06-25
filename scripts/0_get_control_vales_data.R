#------------------------------------------------------------------------------*
# datos del control de vales (grupo control Hapin)
#-----------------------------------------------------------------------------
library(package = "tidyverse")


#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos entrega de vales 1
entrega_vales1 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 198"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales 2
entrega_vales2 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 196"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales 3
entrega_vales3 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 199"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales 4
entrega_vales4 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 200"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales de saldo 1
entrega_vales_saldo1 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 246"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales de saldo 2
entrega_vales_saldo2 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 247"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos entrega de vales de saldo 3
entrega_vales_saldo3 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 248"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos canje de vales 1
canje_vales1 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 205"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos canje de vales 2
canje_vales2 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 206"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos canje de vales 3
canje_vales3 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 207"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos canje de vales 4
canje_vales4 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 208"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()


#entrega de productos 1
entrega_productos1 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 240"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#entrega de productos 2
entrega_productos2 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 241"
  )
) %>%
  select(record, field_name, value) %>% distinct() %>% 
  spread(field_name, value) %>% 
  as_tibble()

#entrega de productos 3
entrega_productos3 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 242"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>% 
  as_tibble()

#entrega de productos 4
entrega_productos4 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 52",
    # get specifically for s0
    "and event_id = 243"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>% 
  as_tibble()
#metadata de las fotos subidas
#---------------------------
edocs_metadata <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select * ",
    "from redcap_edocs_metadata"
    
  )
) %>%
  select(doc_id, stored_name, file_extension, project_id, stored_date, delete_date, date_deleted_server) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)





