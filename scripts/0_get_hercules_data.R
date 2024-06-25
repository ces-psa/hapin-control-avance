#------------------------------------------------------------------------------*
# datos del proyecto de HERCULES
#-----------------------------------------------------------------------------
library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos del consentimiento de hercules
hercules_consentimiento <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 58",
    # get specifically for s0
    "and event_id = 249"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()

#Datos del CRF de hercules
hercules_data <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 58",
    # get specifically for s0
    "and event_id = 252"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()

#Datos del CRF de hercules
hercules_b2 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 58",
    # get specifically for s0
    "and event_id = 279"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)





