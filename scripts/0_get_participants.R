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
        z10_vars,
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
  select(!!!z10_vars) %>%
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)


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
    # Elevation can not be below 100 meters
    elevation = case_when(
      elevation < 100 ~ elevation * 100,
      TRUE ~ elevation
    )
  )
