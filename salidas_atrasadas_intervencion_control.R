gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% mutate(
  type=if_else(grepl("^35[0-9]{3}", id), "owa", "pwg")
) %>% filter(type=="owa") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h42_date) & visit=="b4") %>% select(id, h42_date)
)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% mutate(
  type=if_else(grepl("^35[0-9]{3}", id), "owa", "pwg")
) %>% filter(type=="owa") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(a26_date) & visit=="b4") %>% select(id, a26_date)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(start_b4=as.Date(fecha_nacimiento) + lubridate::days(351)) %>% 
  mutate(   anio=lubridate::year(start_b4),
            mes=lubridate::month(start_b4)
  ) %>% #transmute(id_tamizaje, id, brazo=recode(brazo,"1"="Intervencion","2"="Control"),fecha_nacimiento, start_b4, anio, mes) %>% write_csv("output/cmit_pendientes.csv")
  group_by(
    anio, mes
  ) %>% 
  summarize(n=n()) %>% ungroup() %>% writexl::write_xlsx("output/cmit_pendientes_por_mes.xlsx")



#PENDIENTES B4 clinica

b4_pendientes_clinica<- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
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
) %>% 
  #anti_join(
  #   gt_emory_data_arm2 %>% filter(id=="35013" )
  # ) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b4=as.Date(fecha_nacimiento) + lubridate::days(365)) %>% 
  mutate(   anio=lubridate::year(fecha_b4),
            mes=lubridate::month(fecha_b4)
  ) %>% 
  group_by(
    anio, mes
  ) %>% 
  summarize(n=n()) %>% ungroup()


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% mutate(
  type=if_else(grepl("^35[0-9]{3}", id), "owa", "pwg")
) %>% anti_join(
 salidas %>% select(id)
) %>%  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(start_b4=as.Date(fecha_nacimiento) + lubridate::days(351)) %>% 
  mutate(   anio=lubridate::year(start_b4),
            mes=lubridate::month(start_b4)
  )%>% #filter(brazo=="0") %>%  write_csv("output/salidas_atrasadas_control")
  group_by(
    anio, mes, brazo
  ) %>% 
  summarize(n=n()) %>% ungroup() %>% write_csv("output/casas_por_brazo_mes.csv")

#h56_realizados
salidas_uvg <- read_csv(
  file = "data/exports/FinalizacinHAPIN_DATA_2020-03-02_0927.csv",
  col_types = cols(.default = col_character())
) %>%
  print()

salidas_emory<-gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(id, h56_date)

salidas_integradas <-salidas_uvg %>% select(id=record_id, h56_date=h56g_date) %>% filter(!is.na(h56_date)) %>% bind_rows(
  salidas_emory %>% mutate_all(as.character)
)

salidas_atrasadas <-readxl::read_excel(path = "D:/Descargas/Intervencion_salida_atrasada.xlsx", sheet="hogares_con_salida_atrasada_ult")
salidas_atrasadas<-salidas_atrasadas %>% mutate_all(as.character)
salidas_atrasadas %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c33_date)) %>% select(id, fecha_hizo_clinica_b4=c33_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h41_date)) %>% select(id, fecha_hizo_exposicion_b4=h41_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c31_date)) %>% select(id, fecha_hizo_c31=c31_date)
) %>% 
  left_join(
    salidas_integradas %>% select(id, fecha_h56=h56_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)
  ) %>% mutate(
    # edad actual
    current_age_days = as.numeric(
      Sys.Date() - as.Date(fecha_nacimiento), unit = "days"
    ),
    current_age_months = current_age_days / (365.25 / 12)
    ) %>%  select(
      screening_id, id, fecha_nacimiento, edad_meses=current_age_months, fecha_salida,
      anio, mes, brazo, Comunidad_1, Comunidad_2, fecha_ultima_entrega_gas,
      fecha_hizo_clinica_b4, fecha_hizo_exposicion_b4, fecha_hizo_c31, fecha_h56
    ) %>% left_join(gt_emory_data_arm2 %>% filter(visit=="b4") %>% 
        filter(!is.na(c33_date)) %>%
        select(id, c33_date, c33_wt1_time) %>% filter(!is.na(c33_wt1_time))
    ) %>%   write_csv("output/Intervencion_salidas_atrasadas_revision.csv")


#SALIDAS ATRASADAS GRUPO CONTROL
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% mutate(
  type=if_else(grepl("^35[0-9]{3}", id), "owa", "pwg")
) %>% anti_join(
  salidas %>% select(id)
) %>%  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(start_b4=as.Date(fecha_nacimiento) + lubridate::days(351)) %>% 
  mutate(   anio=lubridate::year(start_b4),
            mes=lubridate::month(start_b4)
  ) %>% filter(brazo=="0") %>% filter(anio=="2020" & mes<7) %>% mutate_all(as.character) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c33_date)) %>% select(id, fecha_hizo_clinica_b4=c33_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h41_date)) %>% select(id, fecha_hizo_exposicion_b4=h41_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c31_date)) %>% select(id, fecha_hizo_c31=c31_date)
) %>% 
  left_join(
    salidas_integradas %>% select(id, fecha_h56=h56_date)
  ) %>%  mutate(
    # edad actual
    current_age_days = as.numeric(
      Sys.Date() - as.Date(fecha_nacimiento), unit = "days"
    ),
    current_age_months = current_age_days / (365.25 / 12)
  ) %>% left_join(
    comunidades %>% select(id=id_estudio, Comunidad_1=community, Comunidad_2=community_new)
  ) %>%  select(
    id_tamizaje, id, fecha_nacimiento, edad_meses=current_age_months, fecha_salida=start_b4,
    anio, mes, brazo, Comunidad_1, Comunidad_2,
    fecha_hizo_clinica_b4, fecha_hizo_exposicion_b4, fecha_hizo_c31, fecha_h56
  ) %>% left_join(
    lista_vales_entregados %>% filter(grepl("VALE-",id_vale)) %>% group_by(id) %>% count() %>% select(id, cantidad_vales_entregados=n)
  ) %>% 
  left_join(
    lista_vales_canjeados %>% filter(grepl("VALE-[0-9]",id_vale)) %>%  group_by(id) %>% summarize(
      monto_canjeado=sum(monto), cantidad_vales_canjeados=n()
    )
  )  %>% left_join(gt_emory_data_arm2 %>% filter(visit=="b4") %>% 
                     filter(!is.na(c33_date)) %>%
                     select(id, c33_date, c33_wt1_time) %>% filter(!is.na(c33_wt1_time))
  ) %>%  write_csv("output/control_salidas_atrasadas.csv")


