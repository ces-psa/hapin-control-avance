

#sacar los 800 ids participantes en hpain 1
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, 
                                                               id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% 
#   mutate(
#   ventana=as.Date(c30_dob) + lubridate::days(350),
#   un_anio=as.Date(c30_dob) + lubridate::days(365)
# ) %>% 
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_c)) %>% select(
    id,salida_nino=e3_date_exit_c, e3_reason_c
  )
) %>% mutate(
  razon_retiro_nino=recode(e3_reason_c, "1"="Finalizacion del estudio",
                   "3"="Retiro voluntario de laparticipante",
                   "5"="Se mudo del area de estudio",
                   "6"="Fallecio"
                  )
) %>% 
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id,
                                                                 e3_madre=e3_date_exit,
                                                                 e3_reason)
) %>% mutate(
  razon_retiro_madre=recode(e3_reason, "1"="Finalizacion del estudio",
                           "3"="Retiro voluntario de laparticipante",
                           "4"="Retirada por el equipo del estudio",
                           "5"="Se mudo del area de estudio",
                           "6"="Fallecio",
                           "8"="Madre: Aborto/ Aborto espontaneo, mortinato ,muerte del nino"
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_type, fecha_muerte_e2=e2_death_date) %>% 
    filter(e2_type=="1" | e2_type=="2")
) %>% mutate(
  tipo_evento_e2=recode(
    e2_type, "1"="Muerte",
    "2"="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)"
  )
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_title) %>% filter(
      e1_title=="2"
    ) %>% mutate(
      tipo_evento_e1=recode(e1_title,"2"="Aborto espontaneo (< 20 semanas de gestacion)")
    )
  )  %>% 
  left_join(
            gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% filter(visit=="b1") %>% select(
              id, b1_c32=c32_date
            )  
          ) %>%
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% filter(visit=="b2") %>% select(
      id, b2_c32=c32_date
    )  
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% filter(visit=="b3") %>% select(
      id, b3_c32=c32_date
    )  
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% filter(visit=="b4") %>% select(
      id, b4_c32=c32_date
    )  
  ) %>% select(id, c30_dob, fec_probable_parto=m17_ga, fecha_muerte_e2, salida_madre=e3_madre,
               razon_retiro_madre, salida_nino, razon_retiro_nino, 
                      tipo_evento_e2, tipo_evento_e1, b1_c32, b2_c32, b3_c32, b4_c32) %>% 
  writexl::write_xlsx("output/revision_datos_kelya.xlsx")


