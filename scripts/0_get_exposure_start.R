#------------------------------------------------------------------------------*
# Datos de inicio de actividades de exposure ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")


gt_exposure_events <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select event_id, descrip",
    "from redcap_events_metadata",
    "where arm_id in",
    paste(
      "(select arm_id",
      "from redcap_events_arms",
      "where project_id in",
      "(select project_id from redcap_projects",
      " where project_name = 'hapin_guatemala_exposure'))"
    )
  )
)


gt_exposure_start <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id in (select project_id from redcap_projects where project_name = 'hapin_guatemala_exposure')"
  )
) %>%
  left_join(gt_exposure_events) %>%
  mutate(
    visit = recode_factor(
      paste0(descrip, "_arm_2"),
      linea_de_base_arm_2 = "baseline",
      p1_arm_2 = "p1",
      p2_arm_2 = "p2",
      b1_y_crecimiento_m_arm_2 = "b1",
      b2_y_crecimiento_m_arm_2 = "b2",
      b3_y_crecimiento_m_arm_2 = "b3",
      b4_arm_2 = "b4",
      .default = descrip,
      .ordered = TRUE
    )
  ) %>%
  select(visit, record, field_name, value) %>%
  spread(field_name, value) %>%
  as_tibble() %>%
  mutate_at(
    vars(matches("long|lat|elev")),
    funs(as.double)
  ) %>%
  mutate(
    gth4x_long = case_when(gth4x_long > 0 ~ gth4x_long * -1, TRUE ~ gth4x_long),
    # gthx_latitude in Jalapa has to be positive,
    gthx_lat = case_when(gthx_lat < 0 ~ gthx_lat * -1, TRUE ~ gthx_lat),
    # more drastic fixes
    gth4x_long = case_when(
      abs(gth4x_long) > 100 ~ gth4x_long %>%
        sub("(-?[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ gth4x_long
    ),
    gthx_lat = case_when(
      abs(gthx_lat) > 100 ~ gthx_lat %>%
        sub("(-?[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ gthx_lat
    )
  )


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
