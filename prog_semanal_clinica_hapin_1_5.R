library("tidyverse")
#library("qrcode")
library(knitr)
library(openxlsx)

salidas_adultas<-dt_e3 %>% select(id, motivo_salida_adulta) %>% filter(!is.na(motivo_salida_adulta))

#catalogo de productos
cat_hogares_estufas<-read_csv("data/dictionaries/cat_hogares_estufas.csv", show_col_types = FALSE)
hogares_con_estufa<-cat_hogares_estufas %>% mutate_all(as.character) %>% select(id,tipo_estufa_control=Clasificacion)
#catalogo intensivo cohorte
cat_intensivo<-read_csv("data/dictionaries/cat_intensivo_cohorte.csv",show_col_types = FALSE)
cat_intensivo<-cat_intensivo %>% mutate_all(as.character)

#fuera de ventana
cat_fuera_ventana<-read_csv("data/dictionaries/cat_fuera_ventana.csv",show_col_types = FALSE)
cat_fuera_ventana<-cat_fuera_ventana %>% mutate_all(as.character)



#lista de bebes candidatos
listado_bebes<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(330),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit, e3_reason)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    #e3_reason=="8"  ~  "Si",
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
  ) %>% 
  print()

#listado base para 36 meses
listado_bebes_36m<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
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
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.30*48)),
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
  ) %>% 
  print()

#agregar visitas de 36 meses
visitas_36_meses<-listado_bebes_36m%>% filter(as.Date(`36_meses`)<=(Sys.Date()+
       lubridate::days(27))) %>%  
  select(id, fecha_nacimiento,`36_meses`) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                   `Nombre otra adulta`, Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                   comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                   `Celular_otro_miembro`, HAPIN_II_madre,
                                   HAPIN_II_nino,
                                   HAPIN_II_adulta)
  ) %>% left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_madre=e3_reason)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_owa=e3_reason_o)
  ) %>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25,
    type=if_else(grepl("^35",id),"owa","pwg"),
    
    participan=case_when(
      type=="pwg" ~ "madre",
      type=="owa" & e3_owa!="1" ~ "madre",
      type=="owa" & e3_madre!="1" ~ "owa",
      type=="owa" & e3_madre=="1" & e3_owa=="1" ~ "madre y owa"
    ),
    inicio_ventana=as.Date(`36_meses`)-lubridate::days(28)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_36_meses=`36_meses`, edad_actual_meses ,
    inicio_ventana,participan, brazo, comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
    HAPIN_II_nino,
    HAPIN_II_adulta
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% 
  # anti_join(
  #   gt_hapin_II_data %>% filter(!is.na(c33_date)| !is.na(a24a_date)) %>% select(id)
  # ) %>% anti_join(
  #   gt_hapin_II_data %>% filter(!is.na(s4_date) &  s4_consent=="0" & s4_consent=="0") %>% select(id)
  # ) %>% 
  left_join(
    hogares_con_estufa
  ) %>% print()
  # left_join(
  #   salidas_adultas
  # ) %>% left_join(
  #   gt_hapin_II_data %>% select(id, s4_date) %>% filter(!is.na(s4_date)) %>%
  #     transmute(id,s4_realizado="Si")
  # )


#Hogares que salieron en 24 meses
salidas_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
  select(id, e3_date)

#Hogares que salieron en 36 meses
salidas_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

#Hogares que salieron en 36 meses
salidas_48m<-gt_hapin_II_data %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

#quitar los hogares que ya se hicieron
visitas_36_meses<-visitas_36_meses %>% anti_join(
  gt_hapin_II_data %>% filter(redcap_event_name=='year_3_q1_36m_arm_1') %>% filter(
    s4_consent_c=='0'
  )
) %>% anti_join(
  gt_hapin_II_data %>% filter(redcap_event_name=='year_3_q1_36m_arm_1') %>% filter(
    !is.na(c31_date_2)
  ) %>% select(id)
)



progra_reclutamiento<-listado_bebes %>% filter(as.Date(`24_meses`)<=(Sys.Date()+
                                                 lubridate::days(27))) %>%  select(id, fecha_nacimiento,`24_meses`) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                 `Nombre otra adulta`, Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                 comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                 `Celular_otro_miembro`, HAPIN_II_madre,
                                 HAPIN_II_nino,
                                 HAPIN_II_adulta)
) %>% left_join(
  rutas %>% select(comunidad_1=comunidad, ruta)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_madre=e3_reason)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_owa=e3_reason_o)
) %>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25,
    type=if_else(grepl("^35",id),"owa","pwg"),
    
    participan=case_when(
      type=="pwg" ~ "madre",
      type=="owa" & e3_owa!="1" ~ "madre",
      type=="owa" & e3_madre!="1" ~ "owa",
      type=="owa" & e3_madre=="1" & e3_owa=="1" ~ "madre y owa"
    ),
    inicio_ventana=as.Date(`24_meses`)-lubridate::days(28)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_24_meses=`24_meses`, edad_actual_meses ,
    inicio_ventana,participan, brazo, comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
    HAPIN_II_nino,
    HAPIN_II_adulta
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% anti_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date)| !is.na(a24a_date)) %>% select(id)
) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date) &  s4_consent=="0" & s4_consent=="0") %>% select(id)
) %>% left_join(
  hogares_con_estufa
) %>% left_join(
  salidas_adultas
) %>% left_join(
  gt_hapin_II_data %>% select(id, s4_date) %>% filter(!is.na(s4_date)) %>%
    transmute(id,s4_realizado="Si")
) %>% anti_join(
  salidas_24m %>% select(id)
) %>% 
  bind_rows(
  visitas_36_meses
) %>% select(1:4,27,5:26) %>% anti_join(
    salidas_36m %>% select(id)
) 

#programación reclutamiento  
dt_fuera_ventana<-gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b7") %>% select(id, s4_consent_c, s4_reason_c) %>% 
  filter(grepl("FUERA DE VENTANA", s4_reason_c)) %>% transmute(id) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)
  ) %>% mutate(
    fecha_48= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  ) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                   `Nombre otra adulta`, Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                   comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                   `Celular_otro_miembro`, HAPIN_II_madre,
                                   HAPIN_II_nino,
                                   HAPIN_II_adulta)
  ) %>% left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_48, edad_actual_meses , brazo, comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
    HAPIN_II_nino,
    HAPIN_II_adulta
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% anti_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date_2)| !is.na(m10_date)) %>% filter(visit=="b7") %>% select(id)
  )

#agregar niños de 4 años
listado_bebes_48m<-listado_bebes_36m
#consultar si usar 30.2*12= 1452 dias, o usar 365*4 = 1460 dias despues de la fecha de nacimiento
progra_48m<- listado_bebes_48m %>% select(id, fecha_nacimiento, fecha_48=`48_meses`) %>% 
  filter(as.Date(fecha_48)<=(Sys.Date()+lubridate::days(40))) %>%  
  select(id, fecha_nacimiento,fecha_48)  %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                   `Nombre otra adulta`, Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                   comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                   `Celular_otro_miembro`, HAPIN_II_madre,
                                   HAPIN_II_nino,
                                   HAPIN_II_adulta)
  ) %>% left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_48, edad_actual_meses , brazo, comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
    HAPIN_II_nino,
    HAPIN_II_adulta
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% anti_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date_2)| !is.na(m10_date)) %>% filter(visit=="b7") %>% select(id)
  ) %>%
  anti_join(
    gt_hapin_II_data %>% filter(!is.na(s4_date) &  s4_consent=="0" & s4_consent=="0") %>% 
      filter(visit=="b7") %>% select(id )
    )%>% anti_join(
      salidas_48m %>% select(id)
    )  %>% 
  anti_join(
    gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% filter(visit=="b7") %>% select(id)
  ) %>% bind_rows(
    dt_fuera_ventana
  ) %>% 
  left_join(
    hogares_con_estufa
  ) %>% mutate(
    fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
    dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_48) ),
    categoria=case_when(
      dif_dias <= 30 ~ "<=30",
      dif_dias > 30  & dif_dias <=60 ~ "31-60",
      dif_dias > 60  & dif_dias <=90 ~ "61-90",
      dif_dias > 90 ~ ">90"
    ),
    # inicio_ventana=as.Date(fecha_48) - lubridate::days(31),
    # fin_ventana=as.Date(fecha_48) + lubridate::days(90),
    #se modificó para reclutar mas gente
    inicio_ventana=as.Date(fecha_48) - lubridate::days(27),
    fin_ventana=as.Date(fecha_48) + lubridate::days(87),
    dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_48)
  )

#programación 4.5 años

# programacion_54<-listado_bebes %>% select(id, fecha_nacimiento)%>% mutate(
#   fecha_54=as.Date(fecha_nacimiento) + lubridate::days(1633)) %>% 
#   filter(as.Date(fecha_54)<=(Sys.Date()+lubridate::days(27))) %>%  
#   select(id, fecha_nacimiento,fecha_54) %>% left_join(
#     datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
#                                    `Nombre otra adulta`, Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
#                                    comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
#                                    `Celular_otro_miembro`, HAPIN_II_madre,
#                                    HAPIN_II_nino,
#                                    HAPIN_II_adulta)
#   ) %>% left_join(
#     rutas %>% select(comunidad_1=comunidad, ruta)
#   ) %>% 
#   mutate(
#     edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
#   ) %>% left_join(
#     gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
#   ) %>% 
#   select(
#     id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_54, edad_actual_meses , brazo, comunidad_1, comunidad_2, ruta, 
#     `Celular embarazada`, `Celular esposo`,
#     `Celular_otro_miembro`,`Nombre embarazada`,
#     `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
#     HAPIN_II_nino,
#     HAPIN_II_adulta
#   ) %>% 
#   left_join(
#     gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
#                                                                contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
#   ) %>% anti_join(
#     gt_hapin_II_data %>% filter(!is.na(c33_date_2)| !is.na(m10_date)) %>% filter(visit=="b7") %>% select(id)
#   ) %>%
#   anti_join(
#     gt_hapin_II_data %>% filter(!is.na(s4_date) &  s4_consent=="0" & s4_consent=="0") %>% 
#       filter(visit=="b8") %>% select(id )
#   )%>%
#   left_join(
#     hogares_con_estufa
#   ) %>% 
#   anti_join(
#     gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% filter(visit=="b8") %>% select(id)
#   ) %>% mutate(
#    # fecha_54=as.Date(fecha_nacimiento) + lubridate::days(1095),
#     dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_54) ),
#     categoria=case_when(
#       dif_dias <= 30 ~ "<=30",
#       dif_dias > 30  & dif_dias <=60 ~ "31-60",
#       dif_dias > 60  & dif_dias <=90 ~ "61-90",
#       dif_dias > 90 ~ ">90"
#     ),
#     inicio_ventana=as.Date(fecha_54) - lubridate::days(27),
#     fin_ventana=as.Date(fecha_54) + lubridate::days(87),
#     dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_54)
#   )
  


progra_36m<-progra_reclutamiento %>%  mutate(
  fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
  dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b6) ),
  categoria=case_when(
    dif_dias <= 30 ~ "<=30",
    dif_dias > 30  & dif_dias <=60 ~ "31-60",
    dif_dias > 60  & dif_dias <=90 ~ "61-90",
    dif_dias > 90 ~ ">90"
  )
) %>%   select(30, 1:28) %>%   select(-fecha_24_meses, -edad_actual_meses,
                                   -s4_realizado, -motivo_salida_adulta,
                                   -HAPIN_II_nino,
                                   -HAPIN_II_madre,
                                   -HAPIN_II_adulta, -`Nombre otra adulta`,
                                   -participan
                                   ) %>% mutate(
                                     fin_ventana=fecha_36_meses + lubridate::days(87),
                                     dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_b6)
                                   ) %>%
  select(
                                     1:4,22,6, 21, 7:19
                                   ) %>% 
    arrange(desc(dias_pasados_fecha_visita))
  
programacion_48_meses<-  progra_36m %>% left_join(
    datos_participantes %>% select(id=`ID estudio` , sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
                                   direccion_new, telefono_new)
  ) %>%  mutate(tipo_visita="b6") %>%  bind_rows(
  progra_48m %>% select(categoria,id_tamizaje, id, fecha_nacimiento,
                        dias_pasados_fecha_visita, inicio_ventana, fin_ventana, 
                        brazo, comunidad_1, comunidad_2, ruta, `Celular embarazada`,
                        `Celular esposo`, Celular_otro_miembro, `Nombre embarazada`, Nombre_bebe,
                        contacto1_h56, contacto2_h56, contacto3_h56, tipo_estufa_control) %>% mutate(
                          tipo_visita="b7"
                        ) %>% left_join(
                          datos_participantes %>% select(id=`ID estudio` , sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
                                                         direccion_new, telefono_new)
                        )
  ) %>% left_join(
    cat_intensivo %>% transmute(id, Prioridad="Si")
  ) %>% 
    # bind_rows(
  #   programacion_54 %>% select(categoria,id_tamizaje, id, fecha_nacimiento,
  #                              dias_pasados_fecha_visita, inicio_ventana, fin_ventana, 
  #                              brazo, comunidad_1, comunidad_2, ruta, `Celular embarazada`,
  #                              `Celular esposo`, Celular_otro_miembro, `Nombre embarazada`, Nombre_bebe,
  #                              contacto1_h56, contacto2_h56, contacto3_h56, tipo_estufa_control) %>% mutate(
  #                                tipo_visita="54m"
  #                              ) %>% left_join(
  #                                datos_participantes %>% select(id=`ID estudio` , sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
  #                                                               direccion_new, telefono_new)
  #                              )
  # ) %>% 
    select(
    categoria, tipo_visita, id, id_tamizaje, fecha_nacimiento, dias_pasados_fecha_visita, inicio_ventana, fin_ventana,
    brazo, comunidad_1, comunidad_2, ruta, `Nombre embarazada`, Nombre_bebe, 
    sector_old, direccion_old, referencia_old, sector_new, 
    direccion_new, referencia_new, telefono_new, `Celular embarazada`, `Celular esposo`, 
    Celular_otro_miembro, contacto1_h56, contacto2_h56, contacto3_h56, tipo_estufa_control, Prioridad ) 
# %>% left_join(
#       gt_hapin_II_data %>% filter(!is.na(s4_mcon_version)) %>% filter(visit=="b6") %>% select(id, version=s4_mcon_version) %>% mutate(
#         cambiar_consentimiento=if_else(grepl("*ENERO",version),"Si","No")
#       ) %>% select(id, cambiar_consentimiento)
#     ) 
  
programacion_60_meses<-listado_bebes %>% select(
  id, fecha_nacimiento, `60_meses`, id_tamizaje=`ID tamizaje`
) %>%  mutate(
  fecha_b8=as.Date(fecha_nacimiento) + lubridate::days(1815),
  dif_dias= as.Date(Sys.Date()) - as.Date(fecha_b8) ,
  categoria=case_when(
    dif_dias <= 30 ~ "<=30",
    dif_dias > 30  & dif_dias <=60 ~ "31-60",
    dif_dias > 60  & dif_dias <=90 ~ "61-90",
    dif_dias > 90 ~ ">90"
  ),
  inicio_ventana=as.Date(fecha_b8) - lubridate::days(27),
  fin_ventana=as.Date(fecha_b8) + lubridate::days(27),
 ) %>%  filter(as.Date(fecha_b8)<=(Sys.Date()+lubridate::days(27)))  %>% transmute(
  categoria, tipo_visita="b8", id, id_tamizaje, fecha_nacimiento,
  dias_pasados_fecha_visita=dif_dias, inicio_ventana, fin_ventana
)  %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
) %>% left_join(
  datos_participantes %>% select(id_estudio=`ID estudio`, id_tamizaje=`ID tamizaje`,
                                 comunidad_1=`Comunidad embarazada (original z10)`,
                                 comunidad_2=`Comunidad embarazada (nueva)`)
) %>% left_join(
  rutas %>% select(comunidad_1=comunidad, ruta)
) %>% left_join(
  datos_participantes %>% select(id_estudio=`ID estudio`, id_tamizaje=`ID tamizaje`,`Nombre embarazada`,
                                 Nombre_bebe=Nombre_bb, sector_old, direccion_old, referencia_old, sector_new,
                                 direccion_new, referencia_new, telefono_new, `Celular embarazada`, `Celular esposo`,
                                 Celular_otro_miembro
                                 )
)  %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                                 contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
    ) %>% left_join(
      hogares_con_estufa %>% select(id_estudio=id, tipo_estufa_control)
    ) %>% left_join(
      cat_intensivo %>% transmute(id, Prioridad="Si")
    ) %>% left_join(
      gt_hapin_II_data %>% filter(visit=="b7") %>% select(id_estudio=id, s4_consent_c)
    ) %>% mutate(
      recordar_act_48m=case_when(
        s4_consent_c=='0' ~ "Si",
        is.na(s4_consent_c) ~ "Si",
        TRUE ~ ""
      )
    ) %>% select(-s4_consent_c) %>% select(-id_estudio) %>% anti_join(
      gt_hapin_II_data %>% filter(visit=="b8") %>% filter(!is.na(s4_consent_c)) %>% select(id)
    ) 
# %>% mutate(
#       rename(id=id_estudio)
#     ) 




  programacion_48_meses %>%mutate(
    recordar_act_48m=""
  ) %>% bind_rows(
    programacion_60_meses
  ) %>%  mutate(tipo_visita=recode(tipo_visita,"b7"="48m","b8"="60m") ) %>%  writexl::write_xlsx(paste0("output/visitas/prog_reclutamiento_48_60_meses_",Sys.Date(),".xlsx"))

  
  
#-----------------------------
# listado_bebes %>% select(id, fecha_nacimiento, fecha_48=`48_meses`) %>%  mutate(
#   fecha_b7=as.Date(fecha_nacimiento) + lubridate::days(1460),
#   dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b7) ),
#   categoria=case_when(
#     dif_dias <= 30 ~ "<=30",
#     dif_dias > 30  & dif_dias <=60 ~ "31-60",
#     dif_dias > 60  & dif_dias <=90 ~ "61-90",
#     dif_dias > 90 ~ ">90"
#   )
# ) %>% writexl::write_xlsx("output/revision_visitas_b7.xlsx")

#programación MDAT temporal
#---------------------    
# leer registro de hogares donde ya se realizó mdat -----
# mdat_realizado<-read_csv("c:/temp/registro_mdat.csv", show_col_types = FALSE)
# dat_realizado<-mdat_realizado %>% mutate_all(as.character)
# 
# listado_bebes_36m %>% select(id, fecha_nacimiento, `36_meses`) %>% left_join(
#   gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
#     filter(s4_consent_c=="1") %>% filter(visit=="b6") %>% select(
#       id, consintieron=s4_consent_c, fecha_consentimiento=s4_date
#     )
# ) %>% filter(!is.na(fecha_consentimiento)) %>% select(-consintieron) %>%  mutate(
#   inicio_ventana= as.Date(`36_meses`) - lubridate::days(27),
#   fin_ventana=as.Date(`36_meses`) + lubridate::days(57)
# ) %>% filter(
#   fin_ventana>Sys.Date() & fin_ventana<=Sys.Date()+ lubridate::days(57)
# ) %>% arrange(fin_ventana) %>% left_join(
#   gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% select(id, c85_date)
# ) %>% filter(is.na(c85_date)) %>% select(-c85_date) %>%  left_join(
#   datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
#                                  Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
#                                  comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
#                                  `Celular_otro_miembro`, HAPIN_II_madre,
#                                  HAPIN_II_nino,
#                                  HAPIN_II_adulta) %>% group_by(id) %>% slice(1)
# ) %>% left_join(
#   rutas %>% select(comunidad_1=comunidad, ruta)
# )%>% 
#   mutate(
#     edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
#   ) %>% 
#   select(
#     id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, edad_actual_meses ,
#     comunidad_1, comunidad_2, ruta, 
#     `Celular embarazada`, `Celular esposo`,
#     `Celular_otro_miembro`,`Nombre embarazada`,
#     Nombre_bebe, HAPIN_II_madre,
#     HAPIN_II_nino,
#     HAPIN_II_adulta
#   ) %>% 
#   left_join(
#     gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
#                                                                contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
#   )%>% select(1:4,5:18) %>% 
# left_join(
#     mdat_realizado %>% transmute(id=as.character(id), Mdat_realizado_por=encuestador, notas_realizado=notas)
#   ) %>% mutate(
#     fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
#     dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b6) ),
#     categoria=case_when(
#       dif_dias <= 30 ~ "<=30",
#       dif_dias > 30  & dif_dias <=60 ~ "31-60",
#       dif_dias > 60  & dif_dias <=90 ~ "61-90",
#       dif_dias > 90 ~ ">90"
#     ),
#     fin_ventana=fecha_b6 + lubridate::days(87),
#     dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_b6)
#     
#   ) %>% select(
#     23, 1:3,24:25, 5:12, 16:18
#   ) %>% arrange(desc(dias_pasados_fecha_visita)) %>%  writexl::write_xlsx(
#     paste0("output/visitas/prog_MDAT_36m_",Sys.Date(),".xlsx")
#  )



#programación para dias MDAT ----
mdat_b7<-gt_hapin_II_data %>% filter(s4_consent_c=='1') %>% filter(visit=="b6") %>% select(id) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(visit=="b6") %>% select(id, m10_date)
) %>%   left_join(
#   gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2)
# ) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% filter(visit=="b6") %>% select(id, c85_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% filter(visit=="b6") %>% select(id, e3_date)
)%>% mutate(
  #sacar candidatos
  candidato=case_when(
    !is.na(m10_date) & is.na(e3_date) & is.na(c85_date) ~ "1"
  )
) %>% filter(candidato=="1") %>%  transmute(id, visita="b6")%>% left_join(
  #periodods de ventana
  listado_bebes_36m %>% select(id, fecha_nacimiento, `36_meses`)
) %>% mutate(
  inicio_ventana= as.Date(`36_meses`) - lubridate::days(27),
  fin_ventana=as.Date(`36_meses`) + lubridate::days(80)
) %>% filter(
  inicio_ventana<=Sys.Date() & fin_ventana>=Sys.Date() + lubridate::days(9)
) %>% arrange(inicio_ventana) %>% bind_rows(
  #programación para dias MDAT B7----
  gt_hapin_II_data %>% filter(s4_consent_c=='1') %>% filter(visit=="b7") %>% select(id) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(visit=="b7") %>% select(id, m10_date)
  ) %>%   left_join(
    #   gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2)
    # ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% filter(visit=="b7") %>% select(id, c85_date)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% filter(visit=="b7") %>% select(id, e3_date)
  )%>% mutate(
    #sacar candidatos
    candidato=case_when(
      !is.na(m10_date) & is.na(e3_date) & is.na(c85_date) ~ "1"
    )
  ) %>% filter(candidato=="1") %>%  transmute(id, visita="b7")%>% left_join(
    #periodods de ventana
    listado_bebes_36m %>% select(id, fecha_nacimiento, `48_meses`)
  ) %>% mutate(
    inicio_ventana= as.Date(`48_meses`) - lubridate::days(27),
    fin_ventana=as.Date(`48_meses`) + lubridate::days(80)
  ) #%>% filter(
  #   inicio_ventana<=Sys.Date() #& fin_ventana>=Sys.Date() + lubridate::days(9)
  # ) 
  # %>% arrange(inicio_ventana)
  
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                 Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                 comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                 `Celular_otro_miembro`, HAPIN_II_madre,
                                 HAPIN_II_nino,
                                 sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
                          direccion_new, telefono_new,
                                 HAPIN_II_adulta) %>% group_by(id) %>% slice(1)
) %>% left_join(
  rutas %>% select(comunidad_1=comunidad, ruta)
)%>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, visita, fecha_nacimiento, edad_actual_meses ,
    comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    Nombre_bebe, sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
    direccion_new, telefono_new,
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% mutate(
    fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
    fecha_b7=as.Date(fecha_nacimiento) + lubridate::days(1460),
    dif_dias=if_else(visita=="b6",as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b6)),as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b7) )),
    categoriab6=case_when(
      dif_dias <= 30 ~ "<=30",
      dif_dias > 30  & dif_dias <=60 ~ "31-60",
      dif_dias > 60  & dif_dias <=90 ~ "61-90",
      dif_dias > 90 ~ ">90"
    ),
    fin_ventana=if_else(visita=="b6",fecha_b6 + lubridate::days(87),fecha_b7 + lubridate::days(87)),
    dias_pasados_fecha_visita=if_else(visita=="b6", Sys.Date() - as.Date(fecha_b6), Sys.Date() - as.Date(fecha_b7))
    
    
  ) %>% select(-fecha_b6, - fecha_b7, -dif_dias  ) %>%
    select(
     categoria=categoriab6, id_tamizaje, id,edad_actual_meses, dias_pasados_fecha_visita, visita,
       fecha_nacimiento,  comunidad_1, comunidad_2, ruta, `Celular embarazada`,
     `Celular esposo`, Celular_otro_miembro, `Nombre embarazada`, Nombre_bebe,  sector_old, 
     direccion_old, referencia_old, sector_new, referencia_new, 
     direccion_new, telefono_new, contacto1_h56, contacto2_h56, contacto3_h56
    #17,1:2,19,3:16
  ) %>% filter(visita!="b6") %>% 
    arrange(desc(dias_pasados_fecha_visita)) %>% anti_join(
      cat_fuera_ventana
    ) 
  
  cat_cambio_consentimientos<-read_csv("data/dictionaries/cat_cambio_consentimiento_b8.csv",show_col_types = FALSE)
  cat_cambio_consentimientos<-cat_cambio_consentimientos %>% mutate_all(as.character)
  
  
  #mdat B8
 mdat_b8<- gt_hapin_II_data %>% filter(s4_consent_c=='1') %>% filter(visit=="b8") %>% select(id, s4_date, s4_by) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% filter(visit=="b7") %>% select(id, m10_date)
  ) %>%   left_join(
    #   gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b6") %>% select(id, h41_date_v2)
    # ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c85_date)) %>% filter(visit=="b8") %>% select(id, c85_date)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(e3_date)) %>% filter(visit=="b8") %>% select(id, e3_date)
  )%>% mutate(
    #sacar candidatos
    candidato=case_when(
      !is.na(m10_date) & is.na(e3_date) & is.na(c85_date) ~ "1"
    )
  ) %>% filter(candidato=="1") %>%  transmute(id, visita="b8")%>% left_join(
    #periodods de ventana
    listado_bebes_36m %>% select(id, fecha_nacimiento, `60_meses`)
  ) %>% mutate(
    inicio_ventana= as.Date(`60_meses`) - lubridate::days(27),
    fin_ventana=as.Date(`60_meses`) + lubridate::days(80)
  )  %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`,
                                   Nombre_bebe=`Nombre_bb`, comunidad_2=`Comunidad embarazada (nueva)`,
                                   comunidad_1=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`,
                                   `Celular_otro_miembro`, HAPIN_II_madre,
                                   HAPIN_II_nino,
                                   sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
                                   direccion_new, telefono_new,
                                   HAPIN_II_adulta) %>% group_by(id) %>% slice(1)
  ) %>% left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  )%>% 
  mutate(
    edad_actual_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
  ) %>% 
  select(
    id_tamizaje=`ID tamizaje`, id, visita, fecha_nacimiento, edad_actual_meses ,
    comunidad_1, comunidad_2, ruta, 
    `Celular embarazada`, `Celular esposo`,
    `Celular_otro_miembro`,`Nombre embarazada`,
    Nombre_bebe, sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
    direccion_new, telefono_new,
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% mutate(
    #fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
    fecha_b7=as.Date(fecha_nacimiento) + lubridate::days(1460),
    fecha_b8=as.Date(fecha_nacimiento) + lubridate::days(1815),
    dif_dias=if_else(visita=="b7",as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b7)),as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b8) )),
    categoriab8=case_when(
      dif_dias <= 30 ~ "<=30",
      dif_dias > 30  & dif_dias <=60 ~ "31-60",
      dif_dias > 60  & dif_dias <=90 ~ "61-90",
      dif_dias > 90 ~ ">90"
    ),
    fin_ventana=if_else(visita=="b7",fecha_b7 + lubridate::days(87),fecha_b8 + lubridate::days(87)),
    dias_pasados_fecha_visita=if_else(visita=="b7", Sys.Date() - as.Date(fecha_b7), Sys.Date() - as.Date(fecha_b8))
    
    
  ) %>% select(-fecha_b7, - fecha_b8, -dif_dias  ) %>%
  select(
    categoria=categoriab8, id_tamizaje, id,edad_actual_meses, dias_pasados_fecha_visita, visita,
    fecha_nacimiento,  comunidad_1, comunidad_2, ruta, `Celular embarazada`,
    `Celular esposo`, Celular_otro_miembro, `Nombre embarazada`, Nombre_bebe,  sector_old, 
    direccion_old, referencia_old, sector_new, referencia_new, 
    direccion_new, telefono_new, contacto1_h56, contacto2_h56, contacto3_h56
    #17,1:2,19,3:16
  ) %>% filter(visita!="b7") %>% 
  arrange(desc(dias_pasados_fecha_visita)) %>% left_join(
    cat_cambio_consentimientos %>% transmute(id, cambiar_consentimiento="Si")
  ) 
  
  mdat_b8 %>% bind_rows(
    mdat_b7
  ) %>% mutate(
    visita=recode(visita,"b7"="48m","b8"="60m"),
    edad_actual_meses=round(as.numeric(edad_actual_meses),1)
  ) %>% arrange(desc(dias_pasados_fecha_visita)) %>%  writexl::write_xlsx(
      paste0("output/visitas/prog_MDAT_48m",Sys.Date(),".xlsx")
    )
  
