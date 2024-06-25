#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

# Get screening data from Emory export
ue_intensive_file <- list.files(
  path = "data/exports", pattern = "HAPINGuatemalaExpos.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


if(
  lubridate::date(max(gt_emory_file$export_time)) >
  lubridate::date(ue_intensive_file$export_time)
){
  stop(
    paste(
      "El archivo exportado del estudio intensivo de exposición debe ser tan",
      "reciente como el archivo del estudio principal."
    )
  )
}


ue_intensive_data <- ue_intensive_file %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate(
    # Assign routine names to the event-visit combinations
    visit = recode_factor(
      redcap_event_name,
      elegibilidad_arm_1 = "tamizaje",
      linea_de_base_arm_2 = "baseline",
      blp1_arm_1 = "e_b-p1",
      p1_arm_2 = "p1",
      p1p2_arm_1 = "e_p1-p2",
      p2_arm_2 = "p2",
      birth_arm_2 = "parto",
      p2b1_arm_1 = "e_parto-b1",
      mes1_arm_2 = "m1",
      mes_2_arm_2 = "m2",
      b1_arm_2 = "b1",
      b1b2_arm_1 = "e_b1-b2",
      mes_4_arm_2 = "m4",
      mes_5_arm_2 = "m5",
      b2_arm_2 = "b2",
      b2b3_arm_1 = "e_b2-b3",
      mes_7_arm_2 = "m7",
      mes_8_arm_2 = "m8",
      b3_arm_2 = "b3",
      b3b4_arm_1 = "e_b3-b4",
      mes_10_arm_2 = "m10",
      mes_11_arm_2 = "m11",
      b4_arm_2 = "b4",
      .ordered = TRUE
    )
  ) %>%
  select(redcap_event_name, visit, id = record_id, everything()) %>%
  # fix types
  mutate_at(
    vars(matches("date")),
    list(~ as.Date(.))
  )


# End of script
