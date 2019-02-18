#------------------------------------------------------------------------------*
# Datos de contacto de participantes ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

z10_vars <- c(
  "record", "record_id", "id_estudio",  "municipio", "z10_village", "com_jalapa", 
  "com_jalapa_new", "z10_alti_gps", "z10_lati_gps", "z10_long_gps", "z10_arm",
  "type_goelocation"
)

private_vars <- c(
  "z10_p_name", "z10_p_lastname_2",
  "z10_aow_name", "z10_aow_lastname",
  "z10_hh_name", "z10_hh_lastname_2",
  "z10_dpi", "z10_dob", "fecha_parto", "fecha_nacimiento_bb"
)

gt_z10 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 39",
    # "in \"(select project_id from redcap_projects where project_name = 'main_study_gt'\") A",
    "and field_name in",
    paste0(
      "('",
      paste(
        c(z10_vars, private_vars),
        collapse = "', '"
      ),
      "')"
    ),
    # get specifically for z10
    "and event_id = 161"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  select(!!!z10_vars, !!!private_vars) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)




#------------------------------------------------------------------------------*
# Generate participant lookup table ----
#------------------------------------------------------------------------------*

gt_z10 %>%
  mutate_all(
    funs(iconv(., from = "Latin1", to = "UTF-8"))
  ) %>%
  mutate_at(
    vars(
      z10_p_name, z10_p_lastname_2,
      z10_aow_name, z10_aow_lastname,
      z10_hh_name, z10_hh_lastname_2
    ),
    funs(
      if_else(
        condition = is.na(.),
        true = "",
        false = .
      )
    )
  ) %>%
  mutate(
    embarazada = paste(z10_p_name, z10_p_lastname_2),
    adulta = paste(z10_aow_name, z10_aow_lastname),
    esposo = paste(z10_hh_name, z10_hh_lastname_2)
  ) %>%
  select(
    "ID tamizaje" = record_id,
    "ID estudio" = id_estudio,
    "Nombre embarazada" = embarazada,
    "DPI embarazada" = z10_dpi,
    "Fecha nacimiento embarazada" = z10_dob,
    "Nombre esposo" = esposo,
    "Fecha esperada de parto" = fecha_parto,
    "Fecha del parto" = fecha_nacimiento_bb,
    "Nombre otra adulta" = adulta
  ) %>%
  DT::datatable() %>%
  htmlwidgets::saveWidget(file = "participants.html")

file.rename(
  from = "participants.html",
  to = "output/reference/participants.html"
)


# Drop identifiable information
gt_z10 <- gt_z10 %>%
  select(!!! setdiff(z10_vars, private_vars))




#------------------------------------------------------------------------------*
# Clean up z10 data ----
#------------------------------------------------------------------------------*

# Hard limits on geo-data
gt_participants <- gt_z10 %>%
  # GPS data as numbers
  mutate_at(
    vars(matches("alti|lati|long")),
    funs(
      gsub("[^-0-9.]", "", .) %>%
        as.numeric()
    )
  ) %>%
  # Select GPS data given source type
  mutate(
    long = case_when(
      # type_goelocation == "1" ~ z10_long_tablet,
      type_goelocation == "2" ~ z10_long_gps,
      TRUE ~ NA_real_
    ),
    lat = case_when(
      # type_goelocation == "1" ~ z10_lati_tablet,
      type_goelocation == "2" ~ z10_lati_gps,
      TRUE ~ NA_real_
    ),
    elevation = case_when(
      # type_goelocation == "1" ~ z10_alti_tablet,
      type_goelocation == "2" ~ z10_alti_gps,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-matches("(long|lati|alti)_(gps|tablet)")) %>%
  # Common fixes for GPS data
  mutate(
    # Longitude should be roughly in -90.266 to -89.61
    # and latitude roughly in 14.42 to 14.87
    # If these appear to be reversed, switch
    long_lat_switched = (
      # latitude is negative and < -80
      lat < -80 |
        # longitude is positive and < 80
        between(long, 0, 80)
    ),
    gps_d_temp = if_else(long_lat_switched, long, lat),
    long = if_else(long_lat_switched, lat, long),
    lat = if_else(long_lat_switched, gps_d_temp, lat),
    # Longitude in Jalapa has to be negative
    long = case_when(long > 0 ~ long * -1, TRUE ~ long),
    # Latitude in Jalapa has to be positive,
    lat = case_when(lat < 0 ~ lat * -1, TRUE ~ lat),
    # more drastic fixes
    long = case_when(
      abs(long) > 100 ~ long %>%
        sub("(-[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ long
    ),
    lat = case_when(
      abs(lat) > 100 ~ lat %>%
        sub("(-[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ lat
    ),
    # Elevation can not be below 100 meters
    elevation = case_when(
      elevation < 100 ~ elevation * 100,
      TRUE ~ elevation
    )
  )
