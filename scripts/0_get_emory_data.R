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
  read_csv(col_types = cols(.default = col_character())) %>%
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
      mes1_arm_2 = "m1",
      mes_2_arm_2 = "m2",
      b1_arm_2 = "b1",
      mes_4_arm_2 = "m4",
      mes_5_arm_2 = "m5",
      b2_arm_2 = "b2",
      mes_7_arm_2 = "m7",
      mes_8_arm_2 = "m8",
      b3_arm_2 = "b3",
      mes_10_arm_2 = "m10",
      mes_11_arm_2 = "m11",
      b4_arm_2 = "b4",
      salida_del_estudio_arm_2 = "salida",
      segun_sea_necesari_arm_2 = "libres1",
      segun_sea_necesari_arm_2b = "libres2",
      segun_sea_necesari_arm_2c = "libres3",
      .default = visit,
      .ordered = TRUE
    )
  ) %>%
  select(redcap_event_name, visit, id, everything()) %>%
  mutate_at(
    vars(matches("date"), m17_ga),
    funs(as.Date)
  )
  
