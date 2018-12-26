#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
gt_emory_file <- list.files(
  path = "data/exports", pattern = "MainSt.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

gt_emory_data <- gt_emory_file %>%
  pull(file) %>%
  read_csv() %>%
  # Manual removes
  filter(
    !(redcap_event_name == "elegibilidad_arm_1" & id == "33010"),
    !(redcap_event_name == "elegibilidad_arm_1" & id == "G004"),
    !(id == "99999")
  ) %>%
  mutate(
    # Default change for all "monthly" visits
    visit = gsub(
      pattern = "m([0-9]+).+.",
      replacement = "mensual\\1",
      redcap_event_name
    ),
    # Assign routine names to the event-visit combinations
    visit = recode_factor(
      redcap_event_name,
      elegibilidad_arm_1 = "tamizaje",
      linea_de_base_arm_2 = "baseline",
      p1_arm_2 = "p1",
      p2_arm_2 = "p2",
      birth_arm_2 = "parto",
      b1_y_crecimiento_m_arm_2 = "b1",
      b2_y_crecimiento_m_arm_2 = "b2",
      b3_y_crecimiento_m_arm_2 = "b3",
      b4_arm_2 = "b4",
      salida_del_estudio_arm_2 = "salida",
      segun_sea_necesari_arm_2 = "libres1",
      segun_sea_necesari_arm_2b = "libres2",
      segun_sea_necesari_arm_2c = "libres3",
      .default = visit,
      .ordered = TRUE
    )
  ) %>%
  select(redcap_event_name, visit, id, everything())
