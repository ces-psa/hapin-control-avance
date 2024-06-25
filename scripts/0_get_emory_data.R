#------------------------------------------------------------------------------*
# Get data from Emory RedCap export ----
#------------------------------------------------------------------------------*


library(tidyverse)
library(dplyr)
library(readr)


#-----------------------------------------------------------------
#EMORY ARM 1
# Get screening data from Emory export
gt_emory_file1 <- list.files(
  path = "data/exports", pattern = "MainSt-Arm1.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))


gt_emory_data_arm1 <- gt_emory_file1 %>% 
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
      blp1_arm_1 = "blp1",
      p1_arm_2 = "p1",
      p1p2_arm_1 = "p1p2",
      p2_arm_2 = "p2",
      birth_arm_2 = "parto",
      p2b1_arm_1 = "p2b1",
      mes1_arm_2 = "m1",
      mes_2_arm_2 = "m2",
      b1_arm_2 = "b1",
      b1b2_arm_1 = "b1b2",
      mes_4_arm_2 = "m4",
      mes_5_arm_2 = "m5",
      b2_arm_2 = "b2",
      b2b3_arm_1 = "b2b3",
      mes_7_arm_2 = "m7",
      mes_8_arm_2 = "m8",
      b3_arm_2 = "b3",
      b3b4_arm_1 = "b3b4",
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
  # fix types
  mutate_at(
    vars(matches("date"), m17_ga),
    list(~ as.Date(.))
  ) %>%
  mutate_at(
    vars(m17_hr_bpm),
    list(~ as.numeric(.))
  )

#archivo de emory ARM 2
# Get screening data from Emory export
gt_emory_file2 <- list.files(
  path = "data/exports", pattern = "MainSt-Arm2.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

gt_emory_data_arm2 <- gt_emory_file2 %>%
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
      blp1_arm_1 = "blp1",
      p1_arm_2 = "p1",
      p1p2_arm_1 = "p1p2",
      p2_arm_2 = "p2",
      birth_arm_2 = "parto",
      p2b1_arm_1 = "p2b1",
      mes1_arm_2 = "m1",
      mes_2_arm_2 = "m2",
      b1_arm_2 = "b1",
      b1b2_arm_1 = "b1b2",
      mes_4_arm_2 = "m4",
      mes_5_arm_2 = "m5",
      b2_arm_2 = "b2",
      b2b3_arm_1 = "b2b3",
      mes_7_arm_2 = "m7",
      mes_8_arm_2 = "m8",
      b3_arm_2 = "b3",
      b3b4_arm_1 = "b3b4",
      mes_10_arm_2 = "m10",
      mes_11_arm_2 = "m11",
      b4_arm_2 = "b4",
      salida_del_estudio_arm_2 = "salida",
      segun_sea_necesari_arm_2 = "libres1",
      segun_sea_necesari_arm_2b = "libres2",
      segun_sea_necesari_arm_2c = "libres3",
      segun_sea_necesari_arm_2d = "libres4",
      segun_sea_necesari_arm_2e = "libres5",
      .default = visit,
      .ordered = TRUE
    )
  ) %>%
  select(redcap_event_name, visit, id, everything()) %>%
  # fix types
  mutate_at(
    vars(matches("date")),
    list(~ as.Date(.))
  )

#datos Emory ARM 3
#archivo de emory ARM 2
# Get screening data from Emory export
gt_emory_file3 <- list.files(
  path = "data/exports", pattern = "MainSt-Arm3.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

gt_emory_data_arm3 <- gt_emory_file3 %>%
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
      blp1_arm_1 = "blp1",
      p1_arm_2 = "p1",
      p1p2_arm_1 = "p1p2",
      p2_arm_2 = "p2",
      birth_arm_2 = "parto",
      p2b1_arm_1 = "p2b1",
      mes1_arm_2 = "m1",
      mes_2_arm_2 = "m2",
      b1_arm_2 = "b1",
      b1b2_arm_1 = "b1b2",
      mes_4_arm_2 = "m4",
      mes_5_arm_2 = "m5",
      b2_arm_2 = "b2",
      b2b3_arm_1 = "b2b3",
      mes_7_arm_2 = "m7",
      mes_8_arm_2 = "m8",
      b3_arm_2 = "b3",
      b3b4_arm_1 = "b3b4",
      mes_10_arm_2 = "m10",
      mes_11_arm_2 = "m11",
      b4_arm_2 = "b4",
      salida_del_estudio_arm_2 = "salida",
      segun_sea_necesari_arm_2 = "libres1",
      segun_sea_necesari_arm_2b = "libres2",
      segun_sea_necesari_arm_2c = "libres3",
      segun_sea_necesari_arm_2d = "libres4",
      segun_sea_necesari_arm_2e = "libres5",
      .default = visit,
      .ordered = TRUE
    )
  ) %>%
  select(redcap_event_name, visit, id, everything()) %>%
  # fix types
  mutate_at(
    vars(matches("date")),
    list(~ as.Date(.))
  ) %>%
  mutate_at(
    vars(m17_hr_bpm),
    list(~ as.numeric(.))
  )

