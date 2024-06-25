library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos de tamizaje plasticos
cambio_cilindro <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 81",
    # get specifically for s0
    "and event_id = 300"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
