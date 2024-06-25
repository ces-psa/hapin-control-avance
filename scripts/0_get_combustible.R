#------------------------------------------------------------------------------*
# __   __   _    __        _                  ___         
# \ \ / /__| |_ /_/__ _  _| |___ ___  _  _   / __|__ _ ___
#  \ V / -_) ' \| / _| || | / _ (_-< | || | | (_ / _` (_-<
#   \_/\___|_||_|_\__|\_,_|_\___/__/  \_, |  \___\__,_/__/
##------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#entrega de vales de combustible
entrega_vales_gasolina <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 57",
    # get specifically for s0
    "and event_id = 233"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()

#canje de vales de combustible
canje_vales_gasolina <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 57",
    # get specifically for s0
    "and event_id = 236"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()

#bitacora de vehiculos
bitacora_vehiculos <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 57",
    # get specifically for s0
    "and event_id = 237"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>%
  as_tibble()

#bitacora de vehiculos
recargas_gas <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 57",
    # get specifically for s0
    "and event_id = 245"
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

