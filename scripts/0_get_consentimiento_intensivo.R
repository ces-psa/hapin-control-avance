#------------------------------------------------------------------------------*
# datos del proyecto de Ancillary consentimientos
#-----------------------------------------------------------------------------
library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos del consentimiento de hercules
intensivo_consentimiento <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 48",
    # get specifically for s0
    "and event_id = 250"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
