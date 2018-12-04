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
  as_tibble()


# Disconnect from database
DBI::dbDisconnect(conn = data_base)
