gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by, h41_kap1_log_id)



gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_envir___3,h41_person___3
                                                         ,h41_c_beacon_id1,
                                                         h41_c_beacon_id2,
                                                         h41_c_lascar_id,
                                                         h41_c_ecm_id) %>% mutate(
    Beacon_1=if_else(
      h41_c_beacon_id1=="888" | is.na(h41_c_beacon_id1),
      "No", "Si"
    ),
    Beacon_2=if_else(
      h41_c_beacon_id2=="888" | is.na(h41_c_beacon_id2),
      "No", "Si"
    ),
    ECM_nino=if_else(
      h41_c_ecm_id=="888" | is.na(h41_c_ecm_id),
      "No", "Si"
    ),
    Lascar_nino=if_else(
      h41_c_lascar_id=="888" | is.na(h41_c_lascar_id),
      "No", "Si"
    )
 ) %>%  mutate(
   Tipo_medida=case_when(
     Beacon_1=="Si" & ECM_nino=="Si" ~ "Ambas medidas",
     Beacon_1=="Si" & ECM_nino=="No" ~ "Medida Indirecta",
     Beacon_1=="No" & ECM_nino=="Si" ~ "Medida directa"
   )
 ) %>% select(id, h41_date, Tipo_medida) %>% 
  #group_by(Beacon_1, Beacon_2, ECM_nino) %>% count() %>% 
  writexl::write_xlsx("output/tipo_medidas_ninos_b5.xlsx")
