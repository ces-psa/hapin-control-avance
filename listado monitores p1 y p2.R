

#HAPINGuatemalaMainSt-LBP1P2_DATA_2020-08-07_1652.csv


#archivo de emory ARM 2
# Get screening data from Emory export
gt_emory_file2 <- list.files(
  path = "data/exports", pattern = "MainSt-LBP1P2.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

gt_emory_lb_to_p2 <- gt_emory_file2 %>%
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

gt_emory_lb_to_p2 %>% filter(visit=="p1" | visit=="p2") %>% select(id, visit,matches("h41_")) %>% select(id, h41_date, h41_date_oaw, matches("_fid")) %>% gather(
  key=variable, value = value, -id, -h41_date, -h41_date_oaw
) %>% filter(!is.na(value)) %>% filter(value!='888') %>% select(id, fecha=h41_date, id_filtro=value, visit) %>% writexl::write_xlsx("output/listado_filtros_p1_p2.xlsx")

gt_emory_lb_to_p2 %>% filter(visit=="p1" | visit=="p2") %>% select(id, matches("h41b_")) %>% select(id, h41b_date, matches("_filter")) %>% gather(
  key = variable, value = value, -id, -h41b_date
) %>% filter(!is.na(value)) %>% filter(value!="888") %>% filter(variable=="h41b_filter1" | variable=="h41b_filter2") %>% select(
  id, fecha=h41b_date, id_filtro=value
) %>% writexl::write_xlsx("output/lista_filtros_h41b.xlsx")






