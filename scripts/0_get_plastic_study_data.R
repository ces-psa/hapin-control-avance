#------------------------------------------------------------------------------*
# datos de estudio de plasticos
#-----------------------------------------------------------------------------
library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos de tamizaje plasticos
plastic_tamizaje <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 64",
    # get specifically for s0
    "and event_id = 290"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

#Datos de visita orina plasticos
pastic_orina <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 64",
    # get specifically for s0
    "and event_id = 263"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()

# Disconnect from database
DBI::dbDisconnect(conn = data_base)





