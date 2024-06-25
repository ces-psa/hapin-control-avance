library(tidyverse)
# Datos expor Emory ---------------------------
# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
source(file = "scripts/0_get_hapinII.R", encoding = "UTF-8")

# Hogares con Estufa grupo Control ----------------------------------------
cat_hogares_estufas<-read_csv("data/dictionaries/cat_hogares_estufas.csv")
cat_hogares_estufas<-cat_hogares_estufas %>% mutate_all(as.character)

cat_hogares_estufas %>% print()


# Candidatos Niños para B5 ----
#Niños
candidatos_ninos_b5_b6<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(330),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c) 
  
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_reason) %>% filter(
    e3_reason=="8"
  )
) %>%  
  mutate(
    completo_b4=case_when(
      e3_reason_c=="1" ~ "Si",
      TRUE ~ "No"
    )
  ) %>% 
  select(id, fecha_nacimiento=c30_dob, completo_b4) %>% mutate(
    `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
    `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
    `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
    `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
    `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
  )






## Hogares que pasaron a 24meses y tenían estufa en 12 meses grupo control ------
hogares_24m_con_estufa_12meses<-gt_hapin_II_data %>% filter(visit=="b5" & !is.na(s4_date)) %>% filter(s4_consent=="1" | !is.na(s4_ocon_date)) %>% select(
  id, s4_date, s4_consent, s4_consent_c, s4_ocon_date
) %>% left_join(
  cat_hogares_estufas
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(s6_arm=recode(
    s6_arm, "1"="Intervención", "0"="Control"
  )
  )
) %>% select(id, s4_date, s4_consent_c, s4_ocon_date, estufa=Clasificacion, s6_arm) %>% 
  filter(!is.na(estufa))

hogares_24m_con_estufa_12meses %>% left_join(
  candidatos_ninos_b5_b6 %>% select(id, fecha_nacimiento, completo_b4)
) #%>% writexl::write_xlsx("output/revision_hogares_con_Estufa.xlsx")


# 103 Hogares grupo control con estufa pasaron a 24 meses  de 111 que habia con estufa en 12 meses
# un 93% pasó

# h56 12 meses hapin 1 ----
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id,
                                                         s6_arm=recode(s6_arm, "1"="Intervencion",
                                                                       "0"="Control")) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(
      id, 
      h56_date,
      Fogon_fuego_abierto=as.numeric(h56_stove_able___1) ,
      Estufa_biomasa=as.numeric(h56_stove_able___2),
      Rondereza=as.numeric(h56_stove_able___3),
      Estufa_portatil_lenia=as.numeric(h56_stove_able___4),
      Querosene=as.numeric(h56_stove_able___5),
      Estufa_gas=as.numeric(h56_stove_able___6),
      Estufa_electrica=as.numeric(h56_stove_able___7),
      Comal=as.numeric(h56_stove_able___8),
      Otra=as.numeric(h56_stove_able___555),
      h56_lpg,
      la_comida_no_sabe_bien = as.numeric(h56_lpg_reason___1),
      el_gas_es_peligroso= as.numeric(h56_lpg_reason___2),
      es_complicado= as.numeric(h56_lpg_reason___3),
      es_caro_comprar_recargas= as.numeric(h56_lpg_reason___4),
      es_caro_arreglar_estufa= as.numeric(h56_lpg_reason___5),
      suministro_gas_no_estable= as.numeric(h56_lpg_reason___6),
      distribuidor_gas_lejos= as.numeric(h56_lpg_reason___7),
      el_gas_no_deseable_en_comunidad= as.numeric(h56_lpg_reason___8),
      no_puedo_cocinar_todos_tipos_alimentos= as.numeric(h56_lpg_reason___9),
      dificil_encontrar_repuestos_estufa= as.numeric(h56_lpg_reason___10),
      lenia_es_gratis= as.numeric(h56_lpg_reason___11),
      lenia_mas_barata_que_gas= as.numeric(h56_lpg_reason___12),
      mas_facil_conseguir_lenia_que_gas= as.numeric(h56_lpg_reason___13),
      otra= as.numeric(h56_lpg_reason___555),
      h56_lpg_reason_other,
      h56_cylinder,
      h56_days,
      h56_lpg_cost,
      h56_refill,
      h65_program,
      h56_program_yn,
    )
    
  ) %>% 
#%>% mutate(
  # h56_stove_able_1=recode(h56_stove_able___1,"1"="Fogon Fuego abierto", "0"=NA_character_),
  # h56_stove_able_2=recode(h56_stove_able___2,"1"="Estufa de biomasa", "0"=NA_character_),
  # h56_stove_able_3=recode(h56_stove_able___3,"1"="Rondereza", "0"=NA_character_),
  # h56_stove_able_4=recode(h56_stove_able___4,"1"="Estufa portatil de lenia", "0"=NA_character_),
  # h56_stove_able_5=recode(h56_stove_able___5,"1"="Querosene", "0"=NA_character_),
  # h56_stove_able_6=recode(h56_stove_able___6,"1"="Estufa a gas", "0"=NA_character_),
  # h56_stove_able_7=recode(h56_stove_able___7,"1"="Estufa Electrica", "0"=NA_character_),
  # h56_stove_able_8=recode(h56_stove_able___8,"1"="Comal", "0"=NA_character_),
  # h56_stove_able_555=recode(h56_stove_able___555,"1"="Otra", "0"=NA_character_),
  # h56_stove_able=paste0(
  #   h56_stove_able_1,"; ",h56_stove_able_2,"; ",h56_stove_able_3, "; ",h56_stove_able_4, "; ",h56_stove_able_5,"; ",
  #   h56_stove_able_6,"; ",h56_stove_able_8, "; ", h56_stove_able_555
  # )
  # ) 
  #%>% 
  left_join(
    gt_hapin_II_data %>% filter(visit=="b5" & !is.na(s4_date)) %>% select(id, s4_date, s4_consent_c, s4_ocon_date) %>%
      filter(
      s4_consent_c=="1" | is.na(s4_ocon_date)
    )
  ) %>% left_join(
    #aqui podemos identificar los candidatos para b5 y b6
    candidatos_ninos_b5_b6 %>% select(id, completo_b4)
  ) %>% writexl::write_xlsx("output/revision_gas_h56.xlsx")

gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(h41_date_v2)) %>% select(id, fecha_h41=h41_date_v2, h41_by_v2) %>% 
  anti_join(
    gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(c33_date_2)) %>% select(id)
  )
#33145
#35056


# listado de 36 meses para visita -----
gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(330),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit, e3_reason)
) %>% mutate(
  descartar=case_when(
    # is.na(e3_date_exit_c) ~ "No",
    # as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    e3_reason=="8"  ~  "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% group_by(
    Anio=lubridate::year(`36_meses`),
    Mes=lubridate::month(`36_meses`)
  ) %>% count() %>% writexl::write_xlsx("output/visitas_36m_por_mes.xlsx")
  print()

  
  # h57 24 meses hapin 1 ----
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id,
                                                              s6_arm=recode(s6_arm, "1"="Intervencion",
                                                                            "0"="Control")) %>% left_join(
                                                                              cat_hogares_estufas %>% select(id, Tipo_estufa=Clasificacion)
                                                                            ) %>% left_join(
gt_hapin_II_data %>% filter(visit=="b5" & !is.na(h57_date)) %>% select(
  id, s4_consent_c, s4_ocon_date
) ) %>% 
    left_join(
gt_hapin_II_data %>% filter(visit=="b5" & !is.na(h57_date)) %>% select(
  id, h57_date, h57_stove 
)
    ) %>% filter(!is.na(Tipo_estufa)) %>% writexl::write_xlsx("output/reviewed_h57_stove.xlsx")

  
## Variables solicitadas h57 24 meses -----
  gt_hapin_II_data %>% filter(visit=="b5" & !is.na(h57_date)) %>% select(
    id, s4_consent_c, s4_ocon_date
  )  %>% 
  left_join(
    gt_hapin_II_data %>% filter(visit=="b5" & !is.na(h57_date)) %>% select(
      id, h57_date, h57_stove, 	h57_hapin_stove, h57_stove_no,  	h57_stove_other,
      h57_disposed, 	h57_hapin_cylin, h57_cylin_no,  	h57_cylin_why,
      h57_gas_cooking, h57_buy_gas, h57_gas_adv, h57_new_stove, h57_fuel_compare
    )
  ) %>% writexl::write_xlsx("output/revison_gas_h57_hapin_II,xlsx")


  gt_emory_data_arm2 %>% filter(!is.na(h42_date)) %>% select(id, h42_stove1___5)
  
  
  
  gt_hapin_II_data %>% filter(visit=="b5" & !is.na(s4_date)) %>% filter(s4_consent=="1" | !is.na(s4_ocon_date)) %>% select(
    id, s4_date, s4_consent, s4_consent_c, s4_ocon_date
  ) %>% left_join(
    cat_hogares_estufas
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(s6_arm=recode(
      s6_arm, "1"="Intervención", "0"="Control"
    )
    )
  ) %>% select(id, s4_date, s4_consent_c, s4_ocon_date, estufa=Clasificacion, s6_arm) %>% mutate(
    consintio=case_when(
      s4_consent_c=='1' ~ "Si",
      !is.na(s4_ocon_date) ~ "Si",
      TRUE ~ "No"
    )
  ) %>% group_by(id) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, c35_date)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_date,h57_stove, h57_hapin_stove,h57_stove_no,	h57_disposed,
                                  h57_gas_cooking)
  ) %>% writexl::write_xlsx("output/24m_h57_review2.xlsx")
  
  arm2 %>% filter(!is.na(c37_date)) %>% group_by(
    Anio=lubridate::year(c37_date),
    Month=lubridate::month(c37_date)
  ) %>% count() %>% arrange(Anio, Month)
  
  
  repeats %>% filter(!is.na(c37_date)) %>% group_by(
    Anio=lubridate::year(c37_date),
    Month=lubridate::month(c37_date)
  ) %>% count() %>% arrange(Anio, Month)
  
  
  # Hogares H56 estufas revisión ----
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id,
     s6_arm=recode(s6_arm, "1"="Intervencion","0"="Control")) %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(
        id, 
        h56_date,
        as.numeric(h56_stove_able___1), 
        as.numeric(h56_stove_able___2), 
        as.numeric(h56_stove_able___3), 
        as.numeric(h56_stove_able___4), 
        as.numeric(h56_stove_able___5), 
        as.numeric(h56_stove_able___6), 
        as.numeric(h56_stove_able___7), 
        as.numeric(h56_stove_able___8),
        as.numeric(h56_stove_able___555)
)
) %>% writexl::write_xlsx("output/revision h56_depaso.xlsx")
  
  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id,
                                                              s6_arm=recode(s6_arm, "1"="Intervencion","0"="Control")) %>% left_join(
                                                                gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(
                                                                  id, 
                                                                  h56_date,
                                                                  as.numeric(h56_lpg_reason___1), 
                                                                  as.numeric(h56_lpg_reason___2), 
                                                                  as.numeric(h56_lpg_reason___3), 
                                                                  as.numeric(h56_lpg_reason___4), 
                                                                  as.numeric(h56_lpg_reason___5), 
                                                                  as.numeric(h56_lpg_reason___6), 
                                                                  as.numeric(h56_lpg_reason___7), 
                                                                  as.numeric(h56_lpg_reason___8),
                                                                  as.numeric(h56_lpg_reason___9),
                                                                  as.numeric(h56_lpg_reason___10),
                                                                  as.numeric(h56_lpg_reason___11),
                                                                  as.numeric(h56_lpg_reason___12),
                                                                  as.numeric(h56_lpg_reason___13),
                                                                  as.numeric(h56_lpg_reason___555)
                                                                )
                                                              ) %>% writexl::write_xlsx(
            "output/revision _h56_razones.xlsx"
                                                              )

  
  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id,
                                                              s6_arm=recode(s6_arm, "1"="Intervencion","0"="Control")) %>% left_join(
                                                                gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(
                                                                  id, 
                                                                  h56_date,
                                                                  h56_lpg
                                                                )
                                                              ) %>% 
    writexl::write_xlsx("output/h56_lpg.xlsx")
  

  
  # FREQ DE C31 ----
  gt_hapin_II_data %>% filter(!is.na(c31_date)) %>% select(c31_cough) %>% table() %>% kable(caption = "Freq TOS c31 24m")

  
  
  # FREQ DE C31 ----
  gt_hapin_II_data %>% filter(!is.na(c31_date)) %>% transmute(c31_cough=recode(
    c31_cough, "1"="Si", "0"="No"
  ),
  c31_breath=recode(
    c31_breath, "1"="Si", "0"="No"
  )
  ) %>% table()
  
  retiros_revision<-read_csv("c:/temp/retiros.csv")
  retiros_revision<-retiros_revision %>% mutate_all(as.character)

  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% left_join(
    retiros_revision %>% mutate(retiro="si")
  ) %>% filter(!is.na(retiro))
  
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob, c30_sex) 
  
  #conteos de visitas 36 meses por mes
  candidatos_ninos_b5_b6 %>% group_by(
    anio=lubridate::year(`36_meses`),
    mes=lubridate::month(`36_meses`),
  ) %>% count() %>% writexl::write_xlsx("output/candidatos_36m.xlsx")

  candidatos_ninos_b5_b6 %>% select(id) %>% left_join(
    gt_hapin_II_data %>% filter(visit=="b6") %>%  select(id, visit,s4_date)
  ) %>% group_by(
    anio=lubridate::year(`s4_date`),
    mes=lubridate::month(`s4_date`),
  ) %>% count() %>% writexl::write_xlsx("output/realizados_s4.xlsx")

  
  candidatos_ninos_b5_b6 %>% select(id) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2)
  )   %>% group_by(
    anio=lubridate::year(`h41_date_v2`),
    mes=lubridate::month(`h41_date_v2`),
  ) %>% count() %>% writexl::write_xlsx("output/realizados_h41.xlsx")
  
  candidatos_ninos_b5_b6 %>% select(id) %>% left_join(
    gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
      filter(!is.na(m11_date)) %>% select(id, m11_date)
  ) %>% group_by(
    anio=lubridate::year(`m11_date`),
    mes=lubridate::month(`m11_date`),
  ) %>% count() %>% writexl::write_xlsx("output/realizados_m11.xlsx")
  
  
  # CONTEOS PRODUCTIVIDAD POR PERSONA HAPIN 36 MESES ----
  fecha_conteos="2022-02-18"
  #s4
  s4_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by) %>% 
    filter(as.Date(s4_date)>=fecha_conteos) %>%  mutate(
      dia=format(s4_date, "%A"),
      mes=format(s4_date, "%B"),
      iniciales=s4_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="S4")
  
  #m10
  m10_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by) %>% 
      filter(as.Date(m10_date)>=fecha_conteos) %>%  mutate(
        dia=format(m10_date, "%A"),
        mes=format(m10_date, "%B"),
        iniciales=m10_by
      ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="M10")
  
  #m11
  m11_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by) %>% 
    filter(as.Date(m11_date)>=fecha_conteos) %>%  mutate(
      dia=format(m11_date, "%A"),
      mes=format(m11_date, "%B"),
      iniciales=m11_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="M11")
  
  #m14a
  m14a_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by) %>% 
    filter(as.Date(m14a_date)>=fecha_conteos) %>%  mutate(
      dia=format(m14a_date, "%A"),
      mes=format(m14a_date, "%B"),
      iniciales=m14a_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="M14a")
  
  #m14b
  m14b_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by) %>% 
    filter(as.Date(m14b_date)>=fecha_conteos) %>%  mutate(
      dia=format(m14b_date, "%A"),
      mes=format(m14b_date, "%B"),
      iniciales=m14b_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="M14b")
  
  #m19
  m19_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by) %>% 
    filter(as.Date(m19_date)>=fecha_conteos) %>%  mutate(
      dia=format(m19_date, "%A"),
      mes=format(m19_date, "%B"),
      iniciales=m19_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="M19")
  
  #c31
  c31_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c31_date_2)) %>% select(id, c31_date_2, c31_by_2) %>% 
    filter(as.Date(c31_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(c31_date_2, "%A"),
      mes=format(c31_date_2, "%B"),
      iniciales=c31_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C31")
  
  #c32
  c32_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c32_date_2)) %>% select(id, c32_date_2, c32_by_2) %>% 
    filter(as.Date(c32_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(c32_date_2, "%A"),
      mes=format(c32_date_2, "%B"),
      iniciales=c32_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C32")
  
  #c33
  c33_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(id, c33_date_2, c33_by_2) %>% 
    filter(as.Date(c33_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(c33_date_2, "%A"),
      mes=format(c33_date_2, "%B"),
      iniciales=c33_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C33")
  
  #c35
  c35_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id, c35_date_2, c35_by_2) %>% 
    filter(as.Date(c35_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(c35_date_2, "%A"),
      mes=format(c35_date_2, "%B"),
      iniciales=c35_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C35")
  
  #c85
  c85_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(h85_date)) %>% select(id, h85_date, c85_by) %>% 
    filter(as.Date(h85_date)>=fecha_conteos) %>%  mutate(
      dia=format(h85_date, "%A"),
      mes=format(h85_date, "%B"),
      iniciales=c85_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C85")
  
  #c42
  c42_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  filter(!is.na(c42_date)) %>% select(id, c42_date, c42_by) %>% 
    filter(as.Date(c42_date)>=fecha_conteos) %>%  mutate(
      dia=format(c42_date, "%A"),
      mes=format(c42_date, "%B"),
      iniciales=c42_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C42")
  
  #c86
  c86_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(c86_date)) %>% select(id, c86_date, c86_by) %>% 
    filter(as.Date(c86_date)>=fecha_conteos) %>%  mutate(
      dia=format(c86_date, "%A"),
      mes=format(c86_date, "%B"),
      iniciales=c86_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="C86")

  #H41
  h41_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2, h41_by_v2) %>% 
    filter(as.Date(h41_date_v2)>=fecha_conteos) %>%  mutate(
      dia=format(h41_date_v2, "%A"),
      mes=format(h41_date_v2, "%B"),
      iniciales=h41_by_v2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="H41")
  
  #H42
  h42_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(h42_date_2)) %>% select(id, h42_date_2, h42_by_2) %>% 
    filter(as.Date(h42_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(h42_date_2, "%A"),
      mes=format(h42_date_2, "%B"),
      iniciales=h42_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="H42")
  
  #H43
  h43_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(h43_date_2)) %>% select(id, h43_date_2, h43_by_2) %>% 
    filter(as.Date(h43_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(h43_date_2, "%A"),
      mes=format(h43_date_2, "%B"),
      iniciales=h43_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="H43")
  
  
  #H57
  h57_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(h57_date_2)) %>% select(id, h57_date_2, h57_by_2) %>% 
    filter(as.Date(h57_date_2)>=fecha_conteos) %>%  mutate(
      dia=format(h57_date_2, "%A"),
      mes=format(h57_date_2, "%B"),
      iniciales=h57_by_2
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="H57")
  
  #E3
  e3_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(e3_date)) %>% select(id, e3_date, e3_by) %>% 
    filter(as.Date(e3_date)>=fecha_conteos) %>%  mutate(
      dia=format(e3_date, "%A"),
      mes=format(e3_date, "%B"),
      iniciales=e3_by
    ) %>% group_by(
      mes, dia, iniciales
    ) %>% count() %>% mutate(crf="E3")  

  all_crf_sent<-s4_36m_realizados %>% bind_rows(
    list(
      m10_36m_realizados,
      m11_36m_realizados,
      m14a_36m_realizados,
      m14b_36m_realizados,
      m19_36m_realizados,
      c31_36m_realizados,
      c32_36m_realizados,
      c33_36m_realizados,
      c35_36m_realizados,
      c85_36m_realizados,
      c42_36m_realizados,
      c86_36m_realizados,
      h41_36m_realizados,
      h42_36m_realizados,
      h43_36m_realizados,
      h57_36m_realizados,
      e3_36m_realizados
    )
  )
  
  #generar excel para crear tabla dinámica
  all_crf_sent %>% writexl::write_xlsx(paste0("output/crf_sent_", 
                                              fecha_conteos,"_al_", Sys.Date(),".xlsx"))
  
  # Revision de datos ----
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
    filter(!is.na(c86_date)) %>% select(id, c86_date, c86_by) %>% 
    anti_join(
      gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
        filter(!is.na(h42_date_2)) %>% transmute(id, c86_date=h42_date_2)
    ) %>% arrange(desc(c86_date)) %>% View()

  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
    !is.na(h41_date_v2) 
  ) %>% select(id, h41_date_v2) %>% left_join(
    gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
      !is.na(h42_date_2) 
    ) %>% select(id, h42_date_2)
  ) %>% mutate(
    flag=case_when(
      h42_date_2==h41_date_v2 ~ "h42 igual h41",
      h42_date_2<h41_date_v2 ~ "h42 menor que h41",
      h42_date_2>h41_date_v2 ~ "ok"
    ),
    diferencia_h42=as.Date(h42_date_2)- as.Date(h41_date_v2)
  ) %>% filter(flag=="ok") %>% filter(diferencia_h42>'1')
    filter(flag!="h42 igual h41")

    
    gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(
      h42_date_2
    )
    ) %>% select(id, h42_date_2) %>% filter(h42_date_2>="2022-06-01" & h42_date_2<="2022-06-30") %>% 
      mutate(
      dia=format(h42_date_2, "%A")
    ) %>% filter(dia=="sábado") %>% arrange(h42_date_2) %>% 
      bind_rows(
    gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(
      c86_date
    )
    ) %>% select(id, c86_date) %>% filter(c86_date>="2022-06-01" & c86_date<="2022-06-30") %>% mutate(
      dia=format(c86_date, "%A")
    ) %>% filter(dia=="sábado") %>% arrange(c86_date) %>% select(id_c86=id, c86_date, dia_c86=dia )
    ) %>% writexl::write_xlsx("output/revision_sabados_junio.xlsx")
 
#revision muertes, abortos y slaidos de niños hapin 1       
dt_abortos_muertes_salidas_nino<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% left_join(gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_title) %>% filter(e2_title=="2") %>% transmute(id, muerte_fetal="Si")
)  %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_participant, e2_title) %>% filter(
    e2_participant=="3"
  ) %>% filter(e2_title=="7") %>% transmute(id, muerte_nino="Si")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_title) %>% filter(
    e1_title=="2"
  ) %>% transmute(id, aborto="Si")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_reason_c) %>% mutate(
    e3_salida=recode(
      e3_reason_c, "1"="Finalizacion_estudio", "2"="No elegible", "3"="Retiro voluntario", "4"="Retirado por equipo",
      "5"="Se mudo del area de estudio", "6"="Fallecio", "7"="Perdido en seguimiento", "555"="Otro"
    )
  ) %>% select(id, e3_salida_nino=e3_salida)
)

dt_abortos_muertes_salidas_nino<-dt_abortos_muertes_salidas_nino %>% mutate(
  Evento_nino=case_when(
    aborto=="Si" ~ "aborto",
    muerte_fetal=="Si" ~ "muerte_fetal",
    muerte_nino=="Si" ~ "muerte_nino",
    TRUE ~ e3_salida_nino
    
  )
)
  
dt_abortos_muertes_salidas_nino %>%  writexl::write_xlsx("output/revision_muertes_bortos.xlsx")


#revision owas que participaron solas
dt_abortos_muertes_salidas_nino %>% filter(type=="owa") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_reason_o, e3_last_visit_o) %>% mutate(
    e3_salida_owa=recode(
      e3_reason_o, "1"="Finalizacion_estudio", "2"="No elegible", "3"="Retiro voluntario", "4"="Retirado por equipo",
      "5"="Se mudo del area de estudio", "6"="Fallecio", "7"="Perdido en seguimiento", "555"="Otro"
    ),
    ultima_visita_owa=recode(
      e3_last_visit_o, "0"="Elegibilidad", "1"="BL", "2"="P1", "3"="P2", "5"="B1", "6"="B2",
                         "7"="B3","8"="B4"
  )
) %>% select(id, e3_salida_owa, ultima_visita_owa)
) %>% writexl::write_xlsx("output/revision_owas.xlsx")



# consentimientos 36m ----
dt_consentimientos_36m<-gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
  visit=="b6"
) %>% transmute(
  id, fecha_s4=s4_date, iniciales=s4_by, consintio=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
) %>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) 

revision_keyla<-read_csv("c:/temp/revision_keyla.csv")
revision_keyla<-revision_keyla %>% mutate_all(as.character)

dt_consentimientos_36m<-dt_consentimientos_36m %>% left_join(
  revision_keyla %>% select(`ID tamizaje`,id, color )
) 

dt_consentimientos_36m %>% writexl::write_xlsx("output/listado_consentimientos_36m.xlsx")



# revisión de no consentidos sin E3
gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
  visit=="b6"
) %>% transmute(
  id, fecha_s4=s4_date, iniciales=s4_by, consintio=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
) %>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) %>% filter(
    consintio=="Si"
  ) %>% left_join(
    gt_hapin_II_data %>% filter(
      !is.na(e3_date)
    ) %>% filter(visit=="b6") %>% transmute(id,tienen_e3="Si")
  ) %>% filter(!is.na(tienen_e3)) %>% writexl::write_xlsx(
    "output/e3_pendientes_36m.xlsx"
  )

gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(is.na(m10_room_no)) %>% select(id, m10_date, m10_by, visit) %>% writexl::write_xlsx(
  "output/completar_m10.xlsx"
)


#pendientes de exposición al 23 de agosto 2022
listado_bebes_36m %>% select(id, fecha_nacimiento, fecha_36m=`36_meses`) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id)
) %>% anti_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(s4_consent_c=="0") %>% select(id)
) %>% anti_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(e3_date)) %>% select(id)
) %>% writexl::write_xlsx("output/pendientes_exposicion.xlsx")


#revisión cuando se inicio fot solo en dia 2
gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id, c86_date, c86_fot_to_do_2, c86_reason_cocomp_other_d2) %>% 
  filter(is.na(c86_fot_to_do_2)) %>% arrange(c86_date) %>% View()

#casas que no se les hizo fot
gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2, h41_by_v2) %>% filter(
  as.Date(h41_date_v2)>="2022-09-05"
) %>% filter(h41_by_v2=="APL") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% 
    transmute(id, grupo=recode(s6_arm,"1"="Intervencion", "0"="Control")) 
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,
                                 `ID tamizaje`,`Nombre embarazada`,
                                 comunidad=`Comunidad embarazada (original z10)`,
                                 `Comunidad embarazada (nueva)`,`Celular embarazada`,
                                 `Celular esposo`, Celular_otro_miembro,
                                 HAPIN_II_madre, HAPIN_II_nino,
                                 HAPIN_II_adulta
  ) %>% group_by(id) %>% slice(1) 
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                               fecha_nacimiento=c30_dob)
  ) %>% writexl::write_xlsx("output/listado_h41_sin_fot_APL.xlsx")

dt_anomalias_hapin %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, ultima_visita_madre=e3_last_visit)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, ultima_visita_nino=e3_last_visit_c)
) %>% mutate(
  ultima_visita_madre=recode(ultima_visita_madre,"3"="P2","4"="Birth","5"="B1","6"="B2","7"="B3","8"="B4"),
  ultima_visita_nino=recode(ultima_visita_nino,"4"="Birth","5"="B1","6"="B2","7"="B3","8"="B4"),
) %>% writexl::write_xlsx("output/tabla_anomalias_ultima_visita_hapin1.xlsx")

#anomalias congenitas
anomalias_e1<-gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_category___5, redcap_event_name) %>% 
  filter(e1_category___5=="1")

anomalias_e2<-gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_type, redcap_event_name) %>% 
  filter(e2_type=="6") 

anomalias_c30<-gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>%  select(id, c30_date, c30_problem_type___13) %>% 
  filter(c30_problem_type___13=="1")

anomalias_e2 %>% filter(!(id %in% anomalias_c30$id))

dt_anomalias_hapin<-anomalias_e1 %>% transmute(id, origen="E1", fecha=e1_date) %>% bind_rows(
  anomalias_e2 %>% transmute(id, origen="E2", fecha=e2_date)
) %>% bind_rows(
  anomalias_c30 %>% transmute(id, origen="C30", fecha=c30_date)
) %>% arrange(id)
  writexl::write_xlsx("output/anomalias_hapin.xlsx")

  anomalias<-dt_anomalias_hapin %>% distinct(id) %>% left_join(
    gt_hapin_II_data %>% filter(visit=="b5") %>%  filter(!is.na(s4_date)) %>% transmute(id, s4_date, consintio=recode(
      s4_consent_c,"1"="Si", "0"="No"
    )
    )
    
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, fecha_c35_24m=c35_date)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(s4_date)) %>% transmute(
      id, s4_36m=s4_date, consintio_36m=recode(s4_consent_c,"1"="Si", "0"="No")
    )
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id, fecha_c35_36m=c35_date_2)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, ultima_visita_madre=e3_last_visit)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, ultima_visita_nino=e3_last_visit_c)
  ) %>% mutate(
    ultima_visita_madre=recode(ultima_visita_madre,"3"="P2","4"="Birth","5"="B1","6"="B2","7"="B3","8"="B4"),
    ultima_visita_nino=recode(ultima_visita_nino,"4"="Birth","5"="B1","6"="B2","7"="B3","8"="B4"),
  )
  
  dt_c35 %>% filter(!is.na(c35_date)) %>% mutate(
    visita=case_when(
      redcap_event_name=="b1_arm_2" ~ "b1",
      redcap_event_name=="b2_arm_2" ~ "b2",
      redcap_event_name=="b3_arm_2" ~ "b3",
      redcap_event_name=="b4_arm_2" ~ "b1",
      redcap_event_name=="birth_arm_2" ~ "birth"
    )
  )
  #%>%   writexl::write_xlsx("output/seguimiento_ninos_anomalias.xlsx")
  

  #revision fechas de inicio 36meses
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b6") %>% select(id, s4_date, s4_by, visit
                                                                                  ) %>% mutate(
                inicio=min(as.Date(s4_date)),
                ultimo=max(as.Date(s4_date))
                                                                                  )
                       

  #revision fechas de inicio exposición en 36meses
  gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2, h41_by_v2, visit
  ) %>% mutate(
    inicio=min(as.Date(h41_date_v2)),
    ultimo=max(as.Date(h41_date_v2))
  )
  
  
  
  #revision pesos de madre en 24 y 36 meses
  gt_hapin_II_data %>% filter(!is.na(m14a_date)) %>% select(id, visit, m14a_date, m14a_by,
                                                            matches("^m14_"), m14a_note) %>% writexl::write_xlsx(
    "output/m14a_24_36meses.xlsx"
  )
  
  #revision pesos de madre en 24 y 36 meses
  gt_hapin_II_data %>% filter(!is.na(m14a_date)) %>% select(id, visit, m14a_date, m14a_by,
                                                            m14_wt1, m14_wt2,m14_wt3,
                                                            m14_wt_comp, m14_wt_incomp,
                                                            m14_wt_incomp_other ) %>% 
    writexl::write_xlsx("output/pesos_b5_b6.xlsx")
                                                            
mdat<-readxl::read_xlsx(path = "data/MDAT.xlsx")



# mdat %>% group_by(Mes, month, c85_complete) %>% count() %>% ungroup() %>% cumsum(4)
#  group_by(
#   Anio) %>% count() %>% ggplot(
#     aes(x=Anio, y=n)
#   ) + geom_bar(stat="identity", position="stack", fill="#a04000"
               
               
  # Conteos Credi ----
  gt_emory_data_arm2 %>% filter(!is.na(c35_date)) %>% select(id, visit, matches("c35_") ) %>% writexl::write_xlsx("output/datos_Credi_hapin1.xlsx")
  

#conteo c86 por mes
gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id, c86_date) %>% mutate(
  year=lubridate::year(c86_date),
  month=lubridate::month(c86_date)
) %>% writexl::write_xlsx("output/revision_c86_libny.xlsx")



gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, fecha=h41_date_v2) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(m14b_date))
)

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% filter(id=="33521") %>% select(id, c30_dob) %>% mutate(
  fecha_b6=as.Date(c30_dob)+ lubridate::days(1095 ),
  fin_ventana= as.Date(fecha_b6) + lubridate::days(90)
)

gt_hapin_II_data %>% filter(!is.na(c31_date_2)) %>% filter(c31_date_2>="2023-01-03") %>% select(id, c31_date_2) %>% 
  anti_join(
    gt_hapin_II_data %>% filter(!is.na(c42_date)) %>% filter(c42_date>="2023-01-03") %>% select(id)
  )


gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(h41_date_v2>="2022-12-01") %>% select(id, h41_date_v2, h41_by_v2) %>% 
  anti_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% filter(c33_date_2>="2022-12-01") %>% select(id)
  )


gt_emory_data_arm2 %>% filter(!is.na(c42_collect)) %>% group_by(visit, c42_collect) %>% count() %>% ungroup() %>% 
  mutate(
    c42_collect=recode(
  c42_collect, "1"="tarjeta vacunacion", "2"="Madre del bebe", "4"="registro centro salud"
)
)

gt_emory_data_arm2 %>% filter(!is.na(c42_collect)) %>% group_by(visit, c42_collect) %>% filter(c42_collect=="2") %>%
  select(id, c42_by,visit)
#b4 627 de 738 

gt_emory_data_arm2 %>% filter(!is.na(c42_date) & !is.na(c42_collect)) %>% group_by(visit) %>% count()
gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% group_by(visit) %>% count()


gt_emory_data_arm2 %>% filter(!is.na(c42_date) & is.na(c42_collect)) %>% select(visit, id, c42_date,
                                                                                c42_by, c42_note) %>% group_by(
                                                                                  visit,
                                                                                  lubridate::year(c42_date),
                                                                                  lubridate::month(c42_date)
                                                                                ) 

gt_emory_data_arm2 %>% filter(!is.na(c42_date) & is.na(c42_collect)) %>% select(visit, id, c42_date,
c42_by, c42_note) %>% filter(visit=="b3") %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c42_date) & !is.na(c42_collect)) %>% select(id,visit) %>% filter(visit=="b4")
                                                                                )

gt_emory_data_arm2 %>% filter(!is.na(c42_date) & is.na(c42_collect)) %>% select(visit, id, c42_date,
                                                                                c42_by, c42_note) %>% filter(visit=="b4") %>% anti_join(
                                                                                  gt_emory_data_arm2 %>% filter(!is.na(c42_date) & !is.na(c42_collect)) %>% select(id,visit) %>% filter(visit=="b3")
                                                                                )


gt_hapin_II_data %>% filter(!is.na(c42_collect)) %>% mutate(
  c42_collect=recode(
    c42_collect, "1"="tarjeta vacunacion", "2"="Madre del bebe", "4"="registro centro salud","3"="Otro miembro hogar"
  )) %>% group_by(c42_collect) %>% count()

  

gt_hapin_II_data %>% filter(!is.na(b10_tc_spots)) %>% select(id, b10_date, b10_tc_spots) %>% group_by(b10_tc_spots) %>% count()


gt_emory_data_arm2 %>% filter(!is.na(e2_type)) %>% select(id, e2_date,visit, e2_type, e2_resolve, e2_actions) %>% group_by(e2_type) %>% filter(e2_type=="6") %>% View()


#datos Varinia estudiante Anaite
dt_c35<-read_csv("data/exports/HAPINGuatemalaMainSt-C35_DATA_2023-02-23_1624.csv")
dt_c35<-dt_c35 %>% mutate(id=as.character(id))
dt_c35 %>% group_by(redcap_event_name) %>% count()
dt_c35 %>% filter(!is.na(c35_date)) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_date) %>% count()
dt_c35 %>% filter(!is.na(c35_cry)) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_cry) %>% count()


dt_c35 %>% filter(!is.na(c35_date) & is.na(c35_cry) ) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_date) %>% group_by(
  lubridate::year(c35_date), lubridate::month(c35_date)
) %>% count()


datos_varinia<-dt_c35 %>% filter(redcap_event_name=="b4_arm_2") %>%  select(
  id, fecha=c35_date,
  c35_book,
  c35_home_toys,
  c35_shop_toys,
  c35_objects,
  c35_alone,
  c35_child,
  c35_read,
  c35_read_who,
  c35_story,
  c35_story_who,
  c35_song,
  c35_song_who,
  c35_outside,
  c35_outside_who,
  c35_play,
  c35_play_who,
  c35_draw,
  c35_draw_who,
  c35_unexpect,
  c35_no_control,
  c35_nervous,
  c35_confident,
  c35_your_way,
  c35_cope,
  c35_control_irritation,
  c35_top,
  c35_anger,
  c35_difficulty,
  c35_grasp,
  c35_hands,
  c35_recognize,
  c35_fist,
  c35_interest,
  c35_roll,
  c35_mouth,
  c35_affection,
  c35_pickup,
  c35_object,
  c35_feet,
  c35_sound,
  c35_hold,
  c35_tap,
  c35_sitting,
  c35_change,
  c35_point,
  c35_name,
  c35_respond,
  c35_crawl,
  c35_eat,
  c35_transfer,
  c35_clap
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(
    id, c30_sex, c30_dob
  )
) %>% filter(!is.na(c30_dob)) %>% filter(!is.na(c35_alone))

#integrar segundo pedido de variables de Varinia
datos_varinia
gt_emory_data_arm2 %>% filter(!is.na(h56_))

datos_varinia %>% write_csv("output/datos_varinia_b4.csv")
datos_varinia %>% writexl::write_xlsx("output/datos_varinia_b4.xlsx")
write_rds(datos_varinia, file = "output/datos_varinia_b4.RDS")


gt_hapin_II_data %>% filter(!is.na(c31_date_2)) %>% select(id, c31_date_2) %>% filter(c31_date_2>="2023-02-20") %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id, c35_date_2) %>% filter(c35_date_2>="2023-02-20")  
) %>% filter(is.na(c35_date_2))

gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>% select(id, h42_date_2)
) %>% filter(is.na(h42_date_2))

gt_hapin_II_data %>% filter(c86_date=="2023-02-24") %>% select(id, c86_date, c86_by)

gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(id, c33_date_2, c33_by_2) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id)
) %>% select(id)

gt_hapin_II_data %>% filter(h41_date_2_v2=="2023-03-02") %>% select(id, h41_date_2_v2, h41_by_2_v2)


gt_emory_data_arm2 %>% filter(id %in% c("35011", "33079", "33346", "33488", "35003")) %>% select(
 id, c30_sex
) %>% filter(!is.na(c30_sex)) %>% mutate(
  c30_sex=recode(c30_sex, "1"="Masculino","2"="Femenino")
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, nombre_nino=Nombre_bb)
) %>% mutate(valor_campo=case_when(
  id=="33079" ~ "Femenino",
  id=="33346" ~ "Femenino",
  id=="33488" ~ "Femenino",
  id=="35003" ~ "Masculino",
  id=="35011" ~ "Masculino",
)) %>% writexl::write_xlsx("output/niños_error_c30.xlsx")

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b6") %>% select(
  id, s4_consent_c, s4_mcon_version
) %>% mutate(
  consintio=recode(s4_consent_c, "1"="Si","0"="No"),
  cambiar_version=if_else(
    grepl("*ENERO",s4_mcon_version) & consintio=="Si","Si", "No"
  )
) %>% select(id,s4_mcon_version, consintio,cambiar_version) %>%  writexl::write_xlsx("output/revision_version_consentimiento.xlsx")

H41 %>% select(id) %>% anti_join(
  m14b_acumulado_b6 %>% select(id)
)

listado_48m<-listado_bebes_36m %>% select(id, fecha_nacimiento, fecha_48=`48_meses`)

listado_48m %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% select(id, s4_date)
) %>% mutate(
  fecha_48_2=as.Date(fecha_nacimiento) + lubridate::days(1460),
  dias_pasados=case_when(
    is.na(s4_date) ~ Sys.Date()- as.Date(fecha_48)
  ),
  dias_pasados2=case_when(
    is.na(s4_date) ~ Sys.Date()- as.Date(fecha_48_2)
  )
) %>% arrange(desc(dias_pasados)) %>% write.xlsx("output/revision_vencidos_48_20-05-2023.xlsx")

gt_hapin_II_data %>% filter(!is.na(s4_reason)) %>% select(id, s4_reason, visit) %>% writexl::write_xlsx("output/revisioni_comentarios_s4.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% filter(
  id=="33584"
)

gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id, c86_date,c86_by,
peso=c86_ave_antrho_wt1, talla=c86_ave_antrho_ht1, temp=c86_temperature) %>% 
  left_join(
  gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(
    id, c33_date_2, c33_by_2, peso_c33=c33_ave_wt_2,talla_c33=c33_ave_ht_2
  )
  )%>% filter(
    id=="33570"
  ) %>% mutate(
    dif=as.Date(c33_date_2) - as.Date(c86_date)
  )

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% filter(c30_dob=="2020-03-29") %>% select(id, c30_dob)

gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id, c86_date,c86_by,
                                                         peso=c86_ave_antrho_wt1, talla=c86_ave_antrho_ht1, temp=c86_temperature) %>% 
  left_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(
      id, c33_date_2, c33_by_2, peso_c33=c33_ave_wt_2,talla_c33=c33_ave_ht_2
    )
  )%>% filter(
   c86_date=="2023-04-26"
  ) %>% mutate(
    dif=as.Date(c33_date_2) - as.Date(c86_date)
  )


dt_intensivo<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2023-06-06_1019.csv")

dt_intensivo<-dt_intensivo %>% mutate(id=as.character(record_id))

conteo_intensivos<- dt_intensivo %>% group_by(id) %>% count() %>% transmute(id, cantidad_intensivos=n) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>% filter(s4_consent_c=="1") %>% 
    transmute(id, consintio_b5="Si")
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(s4_consent_c=="1") %>% 
    transmute(id, consintio_b6="Si")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% transmute(
    id, fecha_nacimiento=c30_dob, 
    visit_48=as.Date(fecha_nacimiento) + lubridate::days(1452),
    visita_54=as.Date(fecha_nacimiento) + lubridate::days(1633)
  )
) %>% group_by(cantidad_intensivos,consintio_b5, consintio_b6) %>% count() 

listado<-dt_intensivo %>% group_by(id) %>% count() %>% transmute(id, cantidad_intensivos=n) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>% filter(s4_consent_c=="1") %>% 
    transmute(id, consintio_b5="Si")
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(s4_consent_c=="1") %>% 
    transmute(id, consintio_b6="Si")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% transmute(
    id, fecha_nacimiento=c30_dob, 
    visit_48=as.Date(fecha_nacimiento) + lubridate::days(1452),
    visita_54=as.Date(fecha_nacimiento) + lubridate::days(1620)
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b7") %>% filter(!is.na(s4_date)) %>% transmute(id, consintio_48m=recode(s4_consent_c,"1"="Si","0"="No"))
)

listado

list(
  "listado"=listado,
  "conteo"=conteo_intensivos
) %>% writexl::write_xlsx("output/intensivos.xlsx")

#revision intensivos
dt_intensivo %>% group_by(id) %>% count() %>% transmute(id, cantidad_intensivos=n) %>% arrange(cantidad_intensivos)

gt_hapin_II_data %>% filter(
  !is.na(c33_date_2)
  
) %>% select( peso=c33_ave_wt_2, talla=c33_ave_ht_2) %>% write_csv("output/36m_peso_talla.csv")


data_36m<-gt_hapin_II_data %>% filter(
  !is.na(c33_date_2)
  
) %>% filter(visit=="b6") %>% transmute( peso=as.numeric(c33_ave_wt_2), talla=as.numeric(c33_ave_ht_2))


data_36m$z_score_peso <- (as.numeric(data_36m$peso) - mean(as.numeric(data_36m$peso), na.rm = TRUE)) / sd(as.numeric(data_36m$peso), na.rm = TRUE)
data_36m$z_score_talla <- (as.numeric(data_36m$talla) - mean(as.numeric(data_36m$talla), na.rm = TRUE)) / sd(as.numeric(data_36m$talla), na.rm = TRUE)

data_36m<-data_36m %>% filter(!is.na(z_score_peso) & !is.na(z_score_talla))

library(ggplot2)


# Graficar los Z-scores utilizando un scatter plot
ggplot(data_36m, aes(x = z_score_peso, y = z_score_talla)) +
  geom_point() +
  xlab("Z-score Peso") +
  ylab("Z-score Talla") +
  ggtitle("Diagrama de Dispersión de Z-scores")

#DOB
data_dob<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, dob=c30_dob)

#set weigth and high
#b1
dt_peso_talla<-gt_emory_data_arm2 %>% filter(!is.na(c33_date) & visit=="b1") %>% select(
  id, fecha_b1=c33_date, peso=c33_ave_wt, talla=c33_ave_ht
) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
  data_dob
) %>% mutate(
  age= round((as.Date(fecha_b1) - as.Date(dob) ) / 30, 1)
) %>% select(id, peso, talla, age) %>%  bind_rows(
  #b2
  gt_emory_data_arm2 %>% filter(!is.na(c33_date) & visit=="b2") %>% select(
    id, fecha_b2=c33_date, peso=c33_ave_wt, talla=c33_ave_ht
  ) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
    data_dob
  ) %>% mutate(
    age= round((as.Date(fecha_b2) - as.Date(dob) ) / 30, 1)
  ) 
)%>% select(id, peso, talla, age) %>%  bind_rows(
  #b3
  gt_emory_data_arm2 %>% filter(!is.na(c33_date) & visit=="b3") %>% select(
    id, fecha_b3=c33_date, peso=c33_ave_wt, talla=c33_ave_ht
  ) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
    data_dob
  ) %>% mutate(
    age= round((as.Date(fecha_b3) - as.Date(dob) ) / 30, 1)
  ) 
)%>% select(id, peso, talla, age) %>%  bind_rows(
  #b4
  gt_emory_data_arm2 %>% filter(!is.na(c33_date) & visit=="b4") %>% select(
    id, fecha_b4=c33_date, peso=c33_ave_wt, talla=c33_ave_ht
  ) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
    data_dob
  ) %>% mutate(
    age= round((as.Date(fecha_b4) - as.Date(dob) ) / 30, 1)
  ) 
)%>% select(id, peso, talla, age) %>% bind_rows(
  #24m
  gt_hapin_II_data %>% filter(!is.na(c33_date) & visit=="b5") %>% transmute(
    id, fecha_b5=c33_date, peso=c33_ave_ht, talla=c33_ave_ht
  ) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
    data_dob
  ) %>% mutate(
    age=round((as.Date(fecha_b5)- as.Date(dob))/30,1)
  ) %>% select(id, peso, talla, age)
) %>% bind_rows(
  #36m
  gt_hapin_II_data %>% filter(!is.na(c33_date_2) & visit=="b6") %>% transmute(
    id, fecha_b6=c33_date_2, peso=c33_ave_ht_2, talla=c33_ave_ht_2
  ) %>% filter(!is.na(peso) & !is.na(talla)) %>% left_join(
    data_dob
  ) %>% mutate(
    age=round((as.Date(fecha_b6)- as.Date(dob))/30,1)
  ) %>% select(id, peso, talla, age)
) 
  
#calcular valores z
dt_peso_talla$z_score_peso <- (as.numeric(dt_peso_talla$peso) - mean(as.numeric(dt_peso_talla$peso), na.rm = TRUE)) / sd(as.numeric(dt_peso_talla$peso), na.rm = TRUE)
dt_peso_talla$z_score_talla <- (as.numeric(dt_peso_talla$talla) - mean(as.numeric(dt_peso_talla$talla), na.rm = TRUE)) / sd(as.numeric(dt_peso_talla$talla), na.rm = TRUE)
dt_peso_talla$z_score_age <- (as.numeric(dt_peso_talla$age) - mean(as.numeric(dt_peso_talla$age), na.rm = TRUE)) / sd(as.numeric(dt_peso_talla$age), na.rm = TRUE)
  
dt_peso_talla

ggplot(dt_peso_talla, aes(x = age, y = z_score_peso)) +
  geom_point()+
  xlab("Age (months)") +
  ylab("Z-score weigth") +
  ggtitle("Dispersion of Z-scores")





ggplot(dt_peso_talla, aes(x = age, y = z_score_peso)) +
  geom_line()


gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% select(id, e3_date, e3_reason, visit) %>% filter(visit=="b7") %>% group_by(e3_reason) %>% count()

gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% select(id, e3_date, e3_reason, visit) %>% filter(visit=="b7") %>% 
  filter(e3_reason=="5")


gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b7") %>% select(id, h41_by_v2, h41_date_v2) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>% filter(visit=="b7") %>% select(id)
)


consentimientos_48m %>% filter(!is.na(rechazado)) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b7") %>%  select(id, s4_reason)
) %>% View()

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(s4_date=="2023-03-03") %>% select(id, s4_by, s4_date, visit)


gt_hapin_II_data %>%  filter(visit=="b6") %>% filter(!is.na(h41_date_v2)) %>% select(id) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id)
) %>% anti_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(e3_date)) %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  age=(Sys.Date() - as.Date(c30_dob)) / 30
)


gt_hapin_II_data %>% filter(visit=="b7") %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent_c) %>% left_join(
  cat_intensivo %>% transmute(id, Intensivo="Si")
) %>% filter(!is.na(Intensivo))


#revision rapida de IDS
gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2) %>% arrange(h41_date_v2)

gt_hapin_II_data %>% group_by(redcap_event_name) %>% count()


#revision filtros
filtros_usados_hapin %>% filter(id_filtro=="3V51531")

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b6") %>% select(id, consintio=s4_consent_c) %>% group_by(
  consintio
) %>% count()

gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2) %>% count()

data_ranni<-read_csv("C:/Users/aramirez/ownCloud/Exposure_group/Export_Redcap/h41_h41b_h42_h43/HAPINIIGuatemala-H41H42H43H41b_DATA_2023-07-10_1852.csv")

data_ranni %>% group_by(redcap_event_name) %>% count()

data_ranni %>% filter(record_id!="99999") %>% group_by(redcap_event_name) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% filter(id=="33502")


gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>%  filter(visit=="b7") %>% select(id, 
    fecha=h42_date_2, iniciales=h42_by_2,ayer_en_escuela=h42_c_spend___2,
    mayor_tiempo_en=h42_c_spend_most,tiempo_en_escuela= h42_school_today,
    uso_chaleco_en_escuela=h42_school_device_2) %>% filter(ayer_en_escuela=="1") %>% mutate(
      ayer_en_escuela=recode(ayer_en_escuela,"1"="Si"),
      mayor_tiempo_en=recode(
        mayor_tiempo_en, "1"="Nuestra casa",
        "2"="Escuela"
      ),
      uso_chaleco_en_escuela=recode(
        uso_chaleco_en_escuela, "1"="Si",
        "0"="No"
      )
    ) %>% writexl::write_xlsx(
      "output/ninos_escuela_b7.xlsx"
    )


gt_hapin_II_data %>% group_by(visit) %>% count()

gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>%  filter(visit=="b7") %>% 
  select(id, h42_date_2, h42_by_2, h42_c_spend___2,h42_c_spend_most, h42_school_time) %>% 
  filter( h42_school_time>0
          ) %>% filter(h42_c_spend___2=="0") %>% writexl::write_xlsx("output/corregir_listado_escuelas.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% arrange(
  desc(as.Date(c30_dob))
)


gt_hapin_II_data %>% filter(m14a_date=="2021-04-21") %>% select(id, m14a_date, redcap_event_name)

dt_c31<-read_csv("data/HAPINIIGuatemala_DATA_2023-09-04_1156.csv")

dt_c31 %>% group_by(redcap_event_name) %>% count()
dt_c31<-dt_c31 %>% mutate(visit=if_else(redcap_event_name=="24_month_arm_1", "b5","b6"))

# Obtener los nombres de todas las columnas
nombres_columnas <- names(dt_c31)

# Filtrar las columnas que contienen "_2" en sus nombres
columnas_con_2 <- nombres_columnas[grep("_2", nombres_columnas)]

# Filtrar las columnas que no contienen "_2" en sus nombres
columnas_sin_2 <- nombres_columnas[!grepl("_2", nombres_columnas)]

# Crear un nuevo conjunto de datos solo con las columnas que contienen "_2" y 'record_id'
columnas_con_2_con_record_id <- c(columnas_con_2, "record_id")
datos_con_2 <- dt_c31[columnas_con_2_con_record_id]

# Crear un nuevo conjunto de datos solo con las columnas que no contienen "_2" y 'record_id'
columnas_sin_2_con_record_id <- c(columnas_sin_2, "record_id")
datos_sin_2 <- dt_c31[columnas_sin_2_con_record_id]


datos_sin_2<- datos_sin_2 %>% select(record_id, everything())
datos_con_2<- datos_con_2 %>% select(record_id, everything())

data_set_c31 <- cbind( datos_sin_2, datos_con_2)


# Obtener los nombres de las columnas de ambos dataframes
nombres_data_set_c31 <- names(data_set_c31)
nombres_dt_c31 <- names(dt_c31)

# Verificar si los conjuntos de nombres de columnas son iguales
son_iguales <- setequal(nombres_data_set_c31, nombres_dt_c31)

if (son_iguales) {
  cat("El conjunto de datos 'data_set_c31' tiene todas las columnas de 'dt_c31'.\n")
} else {
  cat("El conjunto de datos 'data_set_c31' NO tiene todas las columnas de 'dt_c31'.\n")
}


data_set_c31 %>% write_csv("output/data_set_c31.csv")
data_set_c31 %>% saveRDS("output/data_set_c31.rds")

revison_c31<-read_csv("output/data_set_c31.csv")
revison_c31 %>% transmute(id=`record_id...1`, c31_illness___1, c31_illness___3, 
                          c31_illness_2___1, c31_illness_2___3) %>% group_by(
                            c31_illness___1, c31_illness___3, 
                            c31_illness_2___1, c31_illness_2___3
                          ) %>% count()

# consentimientos 36m ----
listado_bebes %>% select(id) %>% left_join(

  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
    visit=="b6"
  ) %>% transmute(
    id, fecha_s4=s4_date, iniciales=s4_by, consintio=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
  )
  )%>%  left_join(
    datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`,Nombre_nino=Nombre_bb,  nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                   Celular_madre_nino=`Celular embarazada`,
                                   Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                     id
                                   ) %>% slice(1)
  ) %>% left_join(
    rutas
  ) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                                 contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
    ) %>% select(`ID tamizaje`, everything()) %>% writexl::write_xlsx("output/listado_consentimientos_36m.xlsx")
  
# consentimientos 24m ----
listado_bebes %>% select(id) %>% left_join(
  
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
    visit=="b5"
  ) %>% transmute(
    id, fecha_s4=s4_date, iniciales=s4_by, 
    consintio_nino=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
  )
)%>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=Nombre_bb, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) %>% writexl::write_xlsx("output/listado_consentimientos_24m.xlsx")



gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
  visit=="b5"
) %>% transmute(
  id, fecha_s4=s4_date, iniciales=s4_by, 
  consintio_madre=recode(s4_consent,"1"="Si", "0"="No"), version=s4_mcon_version,
  consintio_owa=if_else(!is.na(s4_ocon_date),"Si", "No")
) %>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, 
                                 nombre_madre_madre=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) %>% writexl::write_xlsx("output/listado_consentimientos_24m_madre_y_owa.xlsx")


# consentimientos 48m ----
listado_bebes %>% select(id) %>% left_join(
  
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
    visit=="b7"
  ) %>% transmute(
    id, fecha_s4=s4_date, iniciales=s4_by, consintio=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
  )
)%>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`,Nombre_nino=Nombre_bb,  nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) %>% writexl::write_xlsx("output/listado_consentimientos_48m.xlsx")





cat_fot_pendientes %>% mutate(cat_pendiente_fot="si") %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b7") %>% filter(!is.na(h41_date_v2)) %>% select(
    id,h41_date_v2
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b7") %>% filter(!is.na(c86_date)) %>% select(
    id, c86_date
  )
) %>% filter(!is.na(h41_date_v2) & is.na(c86_date)) %>% arrange(h41_date_v2) %>% writexl::write_xlsx(
  "output/listado_conh41_pendiente_fot.xlsx"
)

listado_60<-listado_bebes %>% transmute(id, fecha=as.Date(`60_meses`)) %>% filter(fecha>=Sys.Date())
listado_60<-listado_bebes %>% transmute(id, fecha=as.Date(fecha_nacimiento)+lubridate::days(1825)) %>% filter(fecha>=Sys.Date())
  
pendientes_fot_4<-cat_fot_pendientes %>% left_join(
  listado_bebes %>% transmute(id, fecha=as.Date(`48_meses`))
) %>% filter(fecha>=Sys.Date()) %>% mutate(grupo="48_meses") %>% bind_rows(
  listado_60 %>% mutate(grupo="60_meses")
) %>% group_by(anio=lubridate::year(as.Date(fecha)),
               mes=lubridate::month(as.Date(fecha)),
               grupo) %>% count()%>% ungroup() %>% transmute(
                 anio, mes, grupo, pendientes=n
               ) 

library(ggplot2)
pendientes_fot_integrado<-pendientes_fot_4 %>% mutate(
  fecha=paste0(anio,"-",mes)
)

# Asegúrate de que la columna fecha esté en formato de fecha (ejemplo: "YYYY-MM")
pendientes_fot_integrado<-pendientes_fot_integrado %>%
  mutate(fecha=as.Date(paste0(anio,"-",mes), format = "%Y-%m"))


# Crea el gráfico de barras
ggplot(pendientes_fot_integrado, aes(x = as.Date(paste(anio, mes, "01", sep = "-")), y = pendientes, fill = grupo)) +
  geom_bar(stat = "identity") +
  labs(x = "Año-Mes", y = "Caga Fot", fill = "Grupo") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pendientes_fot_integrado, aes(x = fecha, y = pendientes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Pendientes por Fecha",
       x = "Fecha",
       y = "Pendientes")



# Define una paleta de colores para los grupos
colores_grupos <- c("48_meses" = "#FC8B50" , "60_meses" = "#FFDDBF" )

# Crea el gráfico de barras con colores personalizados
ggplot(pendientes_fot_integrado, aes(x = as.Date(paste(anio, mes, "01", sep = "-")), y = pendientes, fill = grupo)) +
  geom_bar(stat = "identity") +
  labs(x = "Año-Mes", y = "Pendientes", fill = "Grupo") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_fill_manual(values = colores_grupos) +  # Asigna colores personalizados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

etiquetas_personalizadas <- c("Niños 48m pendientes de fot", "Niños 60m", "Etiqueta3")

# Crea el gráfico de barras con colores personalizados y etiquetas
ggplot(pendientes_fot_integrado, aes(x = as.Date(paste(anio, mes, "01", sep = "-")), y = pendientes, fill = grupo)) +
  geom_bar(stat = "identity", position = "dodge") +  # Configura la posición de las barras
  geom_text(aes(label = pendientes), vjust = -0.5, color = "black", size = 3) +  # Agrega etiquetas
  labs(x = "Año-Mes", y = "Niños", fill = "Grupo") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_fill_manual(values = colores_grupos) +  # Asigna colores personalizados
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Crea el gráfico de barras con colores personalizados y etiquetas personalizadas
ggplot(pendientes_fot_integrado, aes(x = as.Date(paste(anio, mes, "01", sep = "-")), y = pendientes, fill = grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = pendientes), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Año-Mes", y = "Visitas esperadas", fill = "Grupo") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_fill_manual(values = colores_grupos, labels = etiquetas_personalizadas) +  # Asigna etiquetas personalizadas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#grafica para ver producción por equipo semanalmente
dt_revision_produccion_b7<-gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b7") %>% transmute(
  id, iniciales=h41_by_v2, fecha=h41_date_v2, equipo="exposicion"
) %>% bind_rows(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b7") %>% transmute(
    id, iniciales=s4_by, fecha=s4_date, equipo="reclutamiento"
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c85_by, fecha=c85_date, equipo="mdat"
  )
) %>% filter(as.Date(fecha)>="2023-03-06") #%>%  writexl::write_xlsx("output/produccion.xlsx")

dt_revision_produccion_b7 %>% mutate(
  semana=lubridate::week(fecha)
) %>% arrange(semana) %>%  writexl::write_xlsx("output/produccion.xlsx")




#grafica para ver producción por equipo semanalmente
dt_revision_produccion_b6<-gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% transmute(
  id, iniciales=h41_by_v2, fecha=h41_date_v2, equipo="exposicion"
) %>% bind_rows(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b6") %>% transmute(
    id, iniciales=s4_by, fecha=s4_date, equipo="reclutamiento"
  )
  )%>% filter(as.Date(fecha)>="2022-10-01" & as.Date(fecha)<="2023-03-31") 
# ) %>% bind_rows(
#   gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% filter(visit=="b6") %>% transmute(
#     id, iniciales=c85_by, fecha=c85_date, equipo="mdat"
#   )
# ) 



dt_revision_produccion_b6 %>% mutate(
  anio=lubridate::year(fecha),
  semana=lubridate::week(fecha)
  
) %>% arrange(semana) %>%  writexl::write_xlsx("output/produccion_b6.xlsx")




#grafica
# Asegurarse de que 'fecha' sea de tipo Date
dt_revision_produccion_b7$fecha <- as.Date(dt_revision_produccion_b7$fecha)

# Agregar una columna para la semana
dt_revision_produccion_b7 <- dt_revision_produccion_b7 %>%
  mutate(semana = lubridate::week(fecha))

# Crear la gráfica de producción por semana y equipo
ggplot(dt_revision_produccion_b7, aes(x = semana, fill = equipo)) +
  geom_bar() +
  labs(
    title = "Producción por Equipo por Semana",
    x = "Semana",
    y = "Cantidad de Producción",
    fill = "Equipo"
  ) +
  theme_minimal() +
  scale_fill_discrete() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calcular el recuento de observaciones por semana y equipo
resumen <- dt_revision_produccion_b7 %>%
  group_by(semana, equipo) %>%
  summarize(cantidad = n())

# Crear la gráfica de líneas de producción por semana y equipo
ggplot(resumen, aes(x = semana, y = cantidad, color = equipo, group = equipo)) +
  geom_line() +
  labs(
    title = "Producción por Equipo por Semana",
    x = "Semana",
    y = "Cantidad de Producción",
    color = "Equipo"
  ) +
  theme_minimal() +
  scale_color_discrete() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




b7_integrado<- gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=c31_by_2,fecha=c31_date_2,
                                                    crf="c31", equipo="exposicion"
                                                    ) %>% filter(!is.na(fecha)) %>% bind_rows(
gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h42_by_2,fecha=h42_date_2,
                                                crf="h42", equipo="exposicion"
                                                      ) %>% filter(!is.na(fecha))
                                                    ) %>% bind_rows(
 gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h43_by_2,fecha=h43_date_2,
                                      crf="h43", equipo="exposicion"
                                                      ) %>% filter(!is.na(fecha))
                                                    )  %>% bind_rows(
gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h41_by_v2,fecha=h41_date_v2,
                                                     crf="h41", equipo="exposicion"
                                                      ) %>% filter(!is.na(fecha))
                                                    ) %>% bind_rows(
gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
  id, iniciales=c85_by, fecha=c85_date, crf="c85", equipo="mdat"
) %>% filter(!is.na(fecha))
                                                    ) %>% bind_rows(
gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
  id, iniciales=m10_by, fecha=m10_date, crf="m10", equipo="reclutamiento"
) %>% filter(
                                                      !is.na(fecha)
                                                    )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m14b_by, fecha=m14b_date, crf="m14b", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m14a_by, fecha=m14a_date, crf="m14a", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c33_by_2, fecha=c33_date_2, crf="c33", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=s4_by, fecha=s4_date, crf="s4", equipo="reclutamiento"
  )%>% filter(
    !is.na(fecha)
  )
)%>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
                                                    ) %>% mutate(
produccion_exposicion=case_when(
  crf=="h41" ~ 1,
  crf=="m14b" ~ 1,
  crf=="c31" ~ 1,
  crf=="h42" ~ 1,
  crf=="h43" ~ 1,
  crf=="c33" ~ 1,
  crf=="m14a" ~ 1,
  TRUE ~ 0
) 
) 

b7_integrado %>% group_by(iniciales, id, semana, equipo, produccion_exposicion) %>% count() %>% arrange(id) %>% filter(
  produccion_exposicion==1
) %>% 
  mutate(
  exposicion=1
)%>%  writexl::write_xlsx( "output/revision_produccion_b7_exposicion.xlsx" )
b7_integrado %>% write.xlsx("output/revision_b7_integrado.xlsx")

gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
  id, iniciales=m14b_by, fecha=m14b_date, crf="m14b", equipo="exposicion"
)%>% filter(
  !is.na(fecha)
) %>% filter(as.Date(fecha)>="2023-05-01" & as.Date(fecha)<="2023-05-06") %>% group_by(
  iniciales
) %>% count()

b7_integrado %>% filter(crf=="c85") %>% group_by(iniciales, id, semana, equipo) %>% count() %>% arrange(id) %>% filter(
  equipo=="mdat"
) %>% 
  mutate(
    mdat=1
  )%>%  writexl::write_xlsx( "output/revision_produccion_b7_mdat.xlsx" )

b7_integrado %>% mutate(produccion_reclutamiento=case_when(
  crf=="s4" ~ 1,
  crf=="m10" ~ 1,
  TRUE ~ 0
)
)%>% filter(produccion_reclutamiento==1) %>% group_by(iniciales, id, semana, 
                     produccion_reclutamiento) %>% count() %>% arrange(id)  %>% 
  mutate(
    reclutamiento=1
  )%>% filter(semana=="50")
  
  writexl::write_xlsx( "output/revision_produccion_b7_reclutamiento.xlsx" )


b7_integrado %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m11_by, fecha=m11_date, crf="m11", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m19_by, fecha=m19_date, crf="m19", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c32_by_2, fecha=c32_date_2, crf="c32", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c35_by_2, fecha=c35_date_2, crf="c35", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c42_by, fecha=c42_date, crf="c42", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
) %>% mutate(produccion_reclutamiento=case_when(
  crf=="s4" ~ 1,
  crf=="m10" ~ 1,
  crf=="m11" ~ 1,
  crf=="m19" ~ 1,
  crf=="c32" ~ 1,
  crf=="c35" ~ 1,
  crf=="c42" ~ 1,
  TRUE ~ 0
)
) 
#%>% filter(produccion_reclutamiento==1) %>% filter(id=="33368")
%>% group_by(iniciales, id, semana, 
                                                      produccion_reclutamiento) %>% count() %>% arrange(id)  %>% 
  mutate(
    reclutamiento=1
  ) %>% write.xlsx("output/b7_reclutamiento_crfs.xlsx")
  

## B6 integrado
b6_integrado<-gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(id,iniciales=c31_by_2,fecha=c31_date_2,
                                                       crf="c31", equipo="reclutamiento"
) %>% filter(!is.na(fecha)) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(id,iniciales=h42_by_2,fecha=h42_date_2,
                                                         crf="h42", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(id,iniciales=h43_by_2,fecha=h43_date_2,
                                                         crf="h43", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(id,iniciales=h41_by_v2,fecha=h41_date_v2,
                                                         crf="h41", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=c85_by, fecha=c85_date, crf="c85", equipo="mdat"
  ) %>% filter(!is.na(fecha))
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=m10_by, fecha=m10_date, crf="m10", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=m14b_by, fecha=m14b_date, crf="m14b", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=m14a_by, fecha=m14a_date, crf="m14a", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=c33_by_2, fecha=c33_date_2, crf="c33", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=s4_by, fecha=s4_date, crf="s4", equipo="reclutamiento"
  )%>% filter(
    !is.na(fecha)
  )
)%>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
) %>% mutate(
  produccion_exposicion=case_when(
    crf=="h41" ~ 1,
    crf=="m14b" ~ 1,
    crf=="h42" ~ 1,
    crf=="h43" ~ 1,
    crf=="c33" ~ 1,
    crf=="m14a" ~ 1,
    TRUE ~ 0
  ) 
) 


b6_integrado<-b6_integrado %>%  bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=m11_by, fecha=m11_date, crf="m11", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=m19_by, fecha=m19_date, crf="m19", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=c32_by_2, fecha=c32_date_2, crf="c32", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=c35_by_2, fecha=c35_date_2, crf="c35", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b6") %>% transmute(
    id, iniciales=c42_by, fecha=c42_date, crf="c42", equipo="reclutamiento"
  ) %>% filter(
    !is.na(fecha)
  )
) %>%  mutate(  anio=lubridate::year(fecha),   semana=lubridate::week(fecha)
) %>% mutate(produccion_reclutamiento=case_when(
  crf=="s4" ~ 1,
  crf=="m10" ~ 1,
  crf=="m11" ~ 1,
  crf=="m19" ~ 1,
  crf=="c32" ~ 1,
  crf=="c35" ~ 1,
  crf=="c42" ~ 1,
  crf=="c31" ~ 1,
  TRUE ~ 0
)
)
b6_integrado<-b6_integrado %>% filter(fecha>="2022-10-01" & fecha<="2023-03-31")
b6_integrado %>% mutate(
  iniciales=case_when(
    iniciales=="LNA" ~ "LMA",
    iniciales=="LMAA" ~ "LMA",
    iniciales=="LMA11" ~ "LMA",
    iniciales=="RCM." ~ "RCM",
    iniciales=="KNC" ~ "KCA",
    iniciales=="CL" ~ "CLL",
    iniciales=="ALM" ~ "LMA",
    TRUE ~ iniciales
  )
)%>%   filter(produccion_reclutamiento=="1") %>% group_by(
  anio=lubridate::year(as.Date(fecha)), mes=lubridate::month(as.Date(fecha)), semana, id, iniciales,
) %>% count() %>% mutate(
  reclutamiento=1
) %>%  writexl::write_xlsx("output/b6_reclutamiento_iniciales.xlsx")
# %>% filter(produccion_exposicion=="1") %>% group_by(
#   anio=lubridate::year(as.Date(fecha)), mes=lubridate::month(as.Date(fecha)), semana, id, iniciales,
# ) %>% count() %>% mutate(
#   exposicion=1
# ) %>%  writexl::write_xlsx("output/b6_exposicion_iniciales.xlsx")

b6_integrado %>% filter(produccion_exposicion=="1") %>% group_by(
  anio=lubridate::year(as.Date(fecha)), mes=lubridate::month(as.Date(fecha)), semana
) %>%  writexl::write_xlsx("output/b6_exposicion_iniciales.xlsx")
  
gt_hapin_II_data %>% filter(!is.na(e2_date)) %>% select(id, e2_date,e2_participant,e2_title,e2_type, visit)
gt_hapin_II_data %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_participant, e1_title, visit)  
  

gt_hapin_II_data %>% filter(visit=="b7") %>% select(
  id, m14
)




#crfs de exposición de una fecha establecida
gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=c31_by_2,fecha=c31_date_2,
                                                       crf="c31", equipo="exposicion"
) %>% filter(!is.na(fecha)) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h42_by_2,fecha=h42_date_2,
                                                         crf="h42", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h43_by_2,fecha=h43_date_2,
                                                         crf="h43", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(id,iniciales=h41_by_v2,fecha=h41_date_v2,
                                                         crf="h41", equipo="exposicion"
  ) %>% filter(!is.na(fecha))
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m14b_by, fecha=m14b_date, crf="m14b", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
)  %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=m14a_by, fecha=m14a_date, crf="m14a", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c33_by_2, fecha=c33_date_2, crf="c33", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
) %>% bind_rows(
  gt_hapin_II_data %>% filter(visit=="b7") %>% transmute(
    id, iniciales=c31_by_2, fecha=c31_date_2, crf="c31", equipo="exposicion"
  )%>% filter(
    !is.na(fecha)
  )
) %>% writexl::write_xlsx("output/revision_exposicion_crfs_b7.xlsx")


#consentimientos 24 meses
listado_bebes %>% select(id, Nombre_Madre=nombre_madre_nino, comunidad) %>% left_join(
  gt_hapin_II_data %>%filter(visit=="b5") %>%   filter(!is.na(s4_date)) %>% transmute(
    id, fecha_consentimiento=s4_date, consintio_madre=recode(s4_consent, "1"="Si","0"="No"),
    consintio_ninio=recode(s4_consent_c,"1"="Si","0"="No"),
    consintio_adulta=if_else(is.na(s4_ocon_date),"No","Si")
    
  )
) %>% writexl::write_xlsx("output/listado_consentimiento_24m.xlsx")


#intensivos

#extraer datos de osm
source("scripts/0_get_osm_uvg.R")



cat_intensivos %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b7") %>% select(id, s4_consent_c)
) %>% left_join(gt_osm_uvg_data %>% select(id, hapi_date)) %>% 
  filter(s4_consent_c=="0")

gt_hapin_II_data %>% filter(visit=="b7") %>%  filter(!is.na(c33_ht1_2)) %>% transmute(altura=as.numeric(c33_ht1_2)) %>% 
  summarize(min=min(altura), max=max(altura))


gt_hapin_II_data %>% filter(visit=="b7") %>%  filter(!is.na(c33_ht1_2)) %>% transmute(id,altura=as.numeric(c33_ht1_2)) %>% 
  arrange(desc(altura))

summary(as.numeric(gt_hapin_II_data$c33_ht1_2))

#revision b10 adultas
gt_emory_data_arm2 %>% filter(!is.na(b10_v_code1)) %>% select(id, b10_v_code1)


cat_revision<-read_csv("c:/temp/cat_id_revision.csv")
cat_revision<-cat_revision %>% mutate_all(as.character)

cat_revision %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% 
    filter(redcap_event_name=="linea_de_base_arm_2") %>% select(id, s6_arm)
) %>% View()

gt_emory_data_arm2 %>% filter(is.na(s6_arm)) %>% 
  filter(redcap_event_name=="linea_de_base_arm_2") %>% select(id, s6_date) 


data_lab<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2024-02-29_0942.csv")

#cargar los datos
data_lab<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2024-02-29_1446.csv", cols(.default = "c"),show_col_types = FALSE)
#renombrer columnas
names(data_lab) <- data_lab[1,]
data_lab<-data_lab[-1,]

cat_revision %>% left_join(
data_lab %>% filter(redcap_event_name=="registro_arm_2") %>% select(
  correlativo, matches("aliquot_id_")
) %>% gather(
  key = "variable", value = "value", -correlativo
) %>% filter(!is.na(value)) %>% mutate(
  id_alicuota=value
) %>% select(-value, -variable) %>% 
  mutate(tipo=if_else(substr(id_alicuota,1,2)=="35","owa","pw")) %>% filter(tipo=="owa") %>% mutate(
    id=substr(id_alicuota,1,5)
  )
) 


sangre_venosa<-gt_emory_data_arm2 %>% select(id) %>% distinct() %>%
  mutate(tipo=if_else(substr(id,1,2)=="35","owa","pw")) %>% 
  filter(tipo=="owa") %>% arrange(desc(id)) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_v_code1)) %>% 
      select(id, b10_date,redcap_event_name, b10_v_code1) %>% filter(
        redcap_event_name=="linea_de_base_arm_2"
      ) %>% transmute(id, fecha_lb=b10_date, sangre_venosa_lb=b10_v_code1)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_v_code1)) %>% 
      select(id, b10_date,redcap_event_name, b10_v_code1) %>% filter(
        redcap_event_name=="b2_arm_2"
      ) %>% transmute(id, fecha_b2=b10_date, sangre_venosa_b2=b10_v_code1)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_v_code1)) %>% 
      select(id, b10_date,redcap_event_name, b10_v_code1) %>% filter(
        redcap_event_name=="b4_arm_2"
      ) %>% transmute(id, fecha_b4=b10_date, sangre_venosa_b4=b10_v_code1)
  )


celula_bucal<-gt_emory_data_arm2 %>% select(id) %>% distinct() %>%
  mutate(tipo=if_else(substr(id,1,2)=="35","owa","pw")) %>% 
  filter(tipo=="owa") %>% arrange(desc(id)) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_buccal)) %>% 
      select(id, b10_date,redcap_event_name, b10_buccal) %>% filter(
        redcap_event_name=="linea_de_base_arm_2"
      ) %>% transmute(id, fecha_lb=b10_date, celua_bucal_lb=b10_buccal)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_buccal)) %>% 
      select(id, b10_date,redcap_event_name, b10_buccal) %>% filter(
        redcap_event_name=="b2_arm_2"
      ) %>% transmute(id, fecha_b2=b10_date, celula_bucal_b2=b10_buccal)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_buccal)) %>% 
      select(id, b10_date,redcap_event_name, b10_buccal) %>% filter(
        redcap_event_name=="b4_arm_2"
      ) %>% transmute(id, fecha_b4=b10_date, celula_bucal_b4=b10_buccal)
  ) 


enjuague_bucal<-gt_emory_data_arm2 %>% select(id) %>% distinct() %>%
  mutate(tipo=if_else(substr(id,1,2)=="35","owa","pw")) %>% 
  filter(tipo=="owa") %>% arrange(desc(id)) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_oral)) %>% 
      select(id, b10_date,redcap_event_name, b10_oral) %>% filter(
        redcap_event_name=="linea_de_base_arm_2"
      ) %>% transmute(id, fecha_lb=b10_date, enjuague_lb=b10_oral)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_oral)) %>% 
      select(id, b10_date,redcap_event_name, b10_oral) %>% filter(
        redcap_event_name=="b2_arm_2"
      ) %>% transmute(id, fecha_b2=b10_date, enjuague_b2=b10_oral)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_oral)) %>% 
      select(id, b10_date,redcap_event_name, b10_oral) %>% filter(
        redcap_event_name=="b4_arm_2"
      ) %>% transmute(id, fecha_b4=b10_date, enjuague_b4=b10_oral)
  ) 


raspado_nasal<-gt_emory_data_arm2 %>% select(id) %>% distinct() %>%
  mutate(tipo=if_else(substr(id,1,2)=="35","owa","pw")) %>% 
  filter(tipo=="owa") %>% arrange(desc(id)) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_nasal)) %>% 
      select(id, b10_date,redcap_event_name, b10_nasal) %>% filter(
        redcap_event_name=="linea_de_base_arm_2"
      ) %>% transmute(id, fecha_lb=b10_date, enjuague_lb=b10_nasal)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_nasal)) %>% 
      select(id, b10_date,redcap_event_name, b10_nasal) %>% filter(
        redcap_event_name=="b2_arm_2"
      ) %>% transmute(id, fecha_b2=b10_date, enjuague_b2=b10_nasal)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(!is.na(b10_nasal)) %>% 
      select(id, b10_date,redcap_event_name, b10_nasal) %>% filter(
        redcap_event_name=="b4_arm_2"
      ) %>% transmute(id, fecha_b4=b10_date, enjuague_b4=b10_nasal)
  ) 

sangre_venosa
celula_bucal
enjuague_bucal
raspado_nasal


data_alicuotas<-data_lab %>% select(correlativo, date=alicuot_storage_date,
                                    matches("aliquot_id_")) %>% gather(
  key = "variable", value = "value", -correlativo, -date
) %>% filter(!is.na(value)) %>% select(correlativo, date, id_alicuota=value)

data_alicuotas$id_alicuota <- gsub("--", "-", data_alicuotas$id_alicuota)

data_alicuotas %>% mutate(
  tipo=substr(id_alicuota,10,12)
) %>% group_by(tipo) %>% count() %>% View()

sangre_venosa %>% mutate(id_tubo_lb=substr(sangre_venosa_lb,1,8)) %>% left_join(
  data_alicuotas %>% mutate(
    tipo=substr(id_alicuota,10,12),
    id_tubo_lb=substr(id_alicuota,1,8)
  ) %>% filter(id_tubo_lb=="35147-O1") %>% filter(tipo=="P11" | tipo=="P12"
                                    | tipo=="P13" | tipo=="P14") %>% transmute(fecha_lab_lb=date,id_tubo_lb, id_alicuota)
) %>% filter(id=="35147") %>% View()
 


#consentimientos 48 meses
consentimiento_48m<-gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
  visit=="b7"
) %>% transmute(
  id, fecha_s4=s4_date, iniciales=s4_by, consintio=recode(s4_consent_c,"1"="Si","0"="No"), version=s4_mcon_version
) %>%  left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% select(`ID tamizaje`, everything()) 



consentimiento_48m %>% writexl::write_xlsx("output/listado_consentimientos_48m.xlsx")


