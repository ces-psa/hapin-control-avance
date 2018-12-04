#------------------------------------------------------------------------------*
# Datos de contacto de participantes ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")


s0 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 39",
    # get specifically for s0
    "and event_id = 193"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
