gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(mes=lubridate::month(fpp),
             anio=lubridate::year(fpp)) %>% group_by(mes, anio) %>% summarize(n=n())

#C36 EN PILOTO VIGILANCIA UVG
data_vigilancia_uvg<-read_csv("D:/Descargas/PilotoVigilanciaNeum_DATA_2020-01-16_1340.csv")

#C36 Y C36a EMORY MAIN STUDY
c36_main_study<-gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by, c36_time)
c36a_main_study<-gt_emory_data_arm2 %>% filter(!is.na(c36a_date)) %>% select(id, c36a_date, c36a_by, c36a_time)

#c36a Repeticiones Emory
# repeated crfs ----
repeats_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaRepeat_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaRepeat_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

data_repeats <- read_csv(
  file = repeats_file$file,
  col_types = cols(.default = col_character())
)

c36a_repeticiones<-data_repeats %>% filter(!is.na(c36a_date)) %>% select(id=c36a_hhid, c36a_date, c36a_by, c36a_time)

c36a_enviados<-c36a_main_study %>% mutate(fecha=as.character(c36a_date)) %>% select(-c36a_date) %>%   bind_rows(
  c36a_repeticiones %>% mutate(fecha=as.character(c36a_date)) %>% select(-c36a_date)
) %>% mutate(crf="c36a", iniciales=c36a_by, hora=c36a_time) %>% select(-c36a_by)

c36_enviados<-c36_main_study %>% transmute(id, fecha=as.character(c36_date), iniciales=c36_by, c36_time, crf="c36", hora=c36_time)

c36_integrados<-c36a_enviados %>% bind_rows(c36_enviados)

c36_integrados %>% writexl::write_xlsx("output/c36_data.xlsx")


#CUANTOS P1 Y P2
#P1 pendientes de clinica
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) & (visit=="p1" | visit=="p2")) %>% select(
    id_estudio=id
  )
) %>% filter(fp2<=Sys.Date()) %>% 
select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
  mesp1=lubridate::month(fp1),
  aniop1=lubridate::year(fp1),
  mesp2=lubridate::month(fp2),
  aniop2=lubridate::year(fp2)
) %>% mutate(
  type = if_else(
    condition = grepl("^35[0-9]{3}", id_estudio),
    true = "oaw",
    false = "pw"
  ) 
  ) %>% group_by(aniop1,mesp1,type) %>% summarize(n=n()) %>% arrange(aniop1, mesp1, type) %>% 
  writexl::write_xlsx("output/p1_pendientes_clinica.xlsx")

#P2 pendientes de clinica
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) & (visit=="p2")) %>% select(
    id_estudio=id
  )
) %>% 
  select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
    mesp1=lubridate::month(fp1),
    aniop1=lubridate::year(fp1),
    mesp2=lubridate::month(fp2),
    aniop2=lubridate::year(fp2)
  ) %>% mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id_estudio),
      true = "oaw",
      false = "pw"
    ) 
  ) %>% group_by(aniop2,mesp2,type) %>% summarize(n=n()) %>% arrange(aniop2, mesp2, type) %>% 
  writexl::write_xlsx("output/p2_pendientes_clinica.xlsx")

#P1 para exposicion
  #CUANTOS P1 Y P2
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% mutate(
    fcc=as.Date(fpp-lubridate::days(280)),
    dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
    edad_semanas=as.numeric(dias)%/% 7,
    edad_dias=as.numeric(dias)%%7,
    edad_gestacional=paste0(edad_semanas,".",edad_dias),
    fp1=fcc + lubridate::days(168),
    fp1_final= fcc + lubridate::days(223),
    fp2=fcc + lubridate::days(224),
    fp2_final= fcc + lubridate::days(252)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) & (visit=="p1" | visit=="p2")) %>% select(
      id_estudio=id
    )
  ) %>% filter(fp2<=Sys.Date()) %>% 
    select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
      mesp1=lubridate::month(fp1),
      aniop1=lubridate::year(fp1),
      mesp2=lubridate::month(fp2),
      aniop2=lubridate::year(fp2)
    ) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>% group_by(aniop1,mesp1,type) %>% summarize(n=n()) %>% arrange(aniop1, mesp1, type) %>% 
    writexl::write_xlsx("output/p1_pendientes_exposicion.xlsx")
  
  #PENDIENTES B1 Clinica
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(
      !is.na(c31_date) & (redcap_event_name=="b1_arm_2" | redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" |
                            redcap_event_name=="b4_arm_2")
    ) %>% select(id_estudio=id, redcap_event_name)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(id=="35013")
  ) %>% 
    mutate_all(as.character) %>% 
    mutate(fecha_nacimiento=if_else(
      is.na(c30_dob), fpp, c30_dob
    )
    ) %>% mutate(fecha_b1=as.Date(fecha_nacimiento) + lubridate::days(77)) %>% 
    mutate(   anio=lubridate::year(fecha_b1),
              mes=lubridate::month(fecha_b1)
    ) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>% 
    group_by(
      anio, mes, type
    ) %>% summarize(n=n()) %>% writexl::write_xlsx("output/b1_pendientes_clinica.xlsx")
  
  
  #PENDIENTES B2 Clinica
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(
      !is.na(c31_date) & (redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2" )
    ) %>% select(id_estudio=id, redcap_event_name)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(id=="35013" )
  ) %>% 
    mutate_all(as.character) %>% 
    mutate(fecha_nacimiento=if_else(
      is.na(c30_dob), fpp, c30_dob
    )
    ) %>% mutate(fecha_b2=as.Date(fecha_nacimiento) + lubridate::days(167)) %>% 
    mutate(   anio=lubridate::year(fecha_b2),
              mes=lubridate::month(fecha_b2)
    ) %>%mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>%  
    group_by(
      anio, mes, type
    ) %>% summarize(n=n()) %>% writexl::write_xlsx("output/b2_pendientes_clinica.xlsx")
  
  #PENDIENTES B3 Clinica
  
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(
      !is.na(c31_date) & (redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2")
    ) %>% select(id_estudio=id, redcap_event_name)
  ) %>%  anti_join(
    gt_emory_data_arm2 %>% filter(id=="35013" )
  ) %>% 
    mutate_all(as.character) %>% 
    mutate(fecha_nacimiento=if_else(
      is.na(c30_dob), fpp, c30_dob
    )
    ) %>% mutate(fecha_b3=as.Date(fecha_nacimiento) + lubridate::days(257)) %>% 
    mutate(   anio=lubridate::year(fecha_b3),
              mes=lubridate::month(fecha_b3)
    ) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>%  
    group_by(
      anio, mes, type
    ) %>% summarize(n=n()) %>% writexl::write_xlsx("output/b3_pendientes_clinica.xlsx")
  
  #PENDIENTES B4 clinica
  
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(
      !is.na(c31_date) & redcap_event_name=="b4_arm_2"
    ) %>% select(id_estudio=id, redcap_event_name)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(id=="35013" )
  ) %>% 
    mutate_all(as.character) %>% 
    mutate(fecha_nacimiento=if_else(
      is.na(c30_dob), fpp, c30_dob
    )
    ) %>% mutate(fecha_b4=as.Date(fecha_nacimiento) + lubridate::days(365)) %>% 
    mutate(   anio=lubridate::year(fecha_b4),
              mes=lubridate::month(fecha_b4)
    ) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>% 
    group_by(
      anio, mes, type
    ) %>% 
    summarize(n=n()) %>% writexl::write_xlsx("output/b4_pendientes_clinica.xlsx")
  
  #P2 para exposicion
  #CUANTOS P1 Y P2
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
  ) %>% anti_join(
    salidas %>% select(id_estudio=id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
  ) %>% mutate(
    fcc=as.Date(fpp-lubridate::days(280)),
    dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
    edad_semanas=as.numeric(dias)%/% 7,
    edad_dias=as.numeric(dias)%%7,
    edad_gestacional=paste0(edad_semanas,".",edad_dias),
    fp1=fcc + lubridate::days(168),
    fp1_final= fcc + lubridate::days(223),
    fp2=fcc + lubridate::days(224),
    fp2_final= fcc + lubridate::days(252)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) & (visit=="p2")) %>% select(
      id_estudio=id
    )
  ) %>%
    select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
      mesp1=lubridate::month(fp1),
      aniop1=lubridate::year(fp1),
      mesp2=lubridate::month(fp2),
      aniop2=lubridate::year(fp2)
    ) %>% mutate(
      type = if_else(
        condition = grepl("^35[0-9]{3}", id_estudio),
        true = "oaw",
        false = "pw"
      ) 
    ) %>% group_by(aniop2,mesp2,type) %>% summarize(n=n()) %>% arrange(aniop2, mesp2, type) %>% 
    writexl::write_xlsx("output/p2_pendientes_exposicion.xlsx")
  
  
#%>% filter(aniop1>2019 | aniop2>2019) %>% group_by(aniop1,mesp1) %>% summarize(n=n())


#pendients b4 exposicion

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(h41_date) & redcap_event_name=="b4_arm_2"
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b4=as.Date(fecha_nacimiento) + lubridate::days(338)) %>% 
  mutate(   anio=lubridate::year(fecha_b4),
            mes=lubridate::month(fecha_b4)
  ) %>% mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id_estudio),
      true = "oaw",
      false = "pw"
    ) 
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% 
  summarize(n=n()) %>% writexl::write_xlsx("output/b4_pendientes_exposicion.xlsx")




#b1 pendientes exposicion
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(h41_date) ) %>%  filter(redcap_event_name=="b1_arm_2" | 
                                     redcap_event_name=="b2_arm_2" | redcap_event_name=="b4_arm_2") %>% 
    select(id_estudio=id, redcap_event_name)
  )%>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b1=as.Date(fecha_nacimiento) + lubridate::days(63)) %>% 
  mutate(   anio=lubridate::year(fecha_b1),
            mes=lubridate::month(fecha_b1)
  ) %>% mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id_estudio),
      true = "oaw",
      false = "pw"
    ) 
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% summarize(n=n()) %>% writexl::write_xlsx("output/b1_pendientes_exposicion.xlsx")


#pendientes de b2 exposicion
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(h41_date) & (redcap_event_name=="b2_arm_2" | redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b2=as.Date(fecha_nacimiento) + lubridate::days(153)) %>% 
  mutate(   anio=lubridate::year(fecha_b2),
            mes=lubridate::month(fecha_b2)
  ) %>%mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id_estudio),
      true = "oaw",
      false = "pw"
    ) 
  ) %>%  
  group_by(
    anio, mes, type
  ) %>% summarize(n=n()) %>% writexl::write_xlsx("output/b2_pendientes_exposicion.xlsx")



