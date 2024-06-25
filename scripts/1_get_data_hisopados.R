#------------------------------------------------------------------------------*
# datos del proyecto de HISOPADOS RSV
#-----------------------------------------------------------------------------
library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos de CRF de hisopados C34h
c34h_toma_muestra_data <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 53",
    # get specifically for s0
    "and event_id = 307"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()



# Disconnect from database
DBI::dbDisconnect(conn = data_base)


