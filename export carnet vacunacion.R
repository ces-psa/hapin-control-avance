library(package = "tidyverse")

#conexion a la bd
data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

#Datos de tamizaje plasticos
datos_documentos <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 162",
    # get specifically for s0
    "and event_id = 446"
  )
) %>%
  select(record, field_name, value) %>% 
  spread(field_name, value) %>% 
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)


datos_documentos %>% filter(is.na(z42_foto1) & is.na(z42_foto2)) 
%>% writexl::write_xlsx("output/revision_carnetvacunacion_sin_foto.xlsx")
