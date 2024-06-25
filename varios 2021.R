gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(comunidades%>% select(id=id_estudio, community, community_new)
) %>% group_by(community) %>% filter(community=="OTRA")
  count() %>% writexl::write_xlsx("output/cantidad_hogares_comunidad.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% filter(a24b_date=="2019-10-31") %>% select(id, a24b_date, a24b_by, visit)


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_date=="2019-10-31") %>% select(id, b10_date, b10_by, visit)

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% 
select(id, c30_date, c30_dob) %>% filter(id=="35021")


gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, visit, c31_date, c31_by) %>% filter(c31_date=="2019-05-02")

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_by, fec_nac_c30=c30_dob,c30_where,c30_where_other) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, fp_z10=`Fecha del parto`) 
  
) %>% mutate(
  flag=if_else(as.Date(fec_nac_c30)>as.Date(fp_z10) | as.Date(fec_nac_c30)<as.Date(fp_z10),"1","0")
) %>% filter(flag=="1") %>% writexl::write_xlsx("output/inconsistencias_fec_nacimiento_z10_26052020_.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_by, c31_date, visit ) %>% filter(
  c31_date=="2020-03-23"
)

gt_hapin_II_data %>% filter(!is.na(c42_date)) %>% select(id, c42_date) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id) %>% filter(id=="33131")
)


  #CASAS CONTROL
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% filter(s6_arm=="0") %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  all_entregas
) %>% write_csv("output/revison_control.csv")


gt_emory_data_arm2 %>% filter(!is.na(c40_date)) %>% group_by(c40_cyanosis) %>% count()




b10_tc_spots_num
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% filter(s6_arm=="1") %>% select(id) %>% anti_join(
  salidas %>% select(id)
)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% anti_join(
  salidas %>% select(id)
)




gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nac=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, fec_nac) %>% mutate(
  fec_salida= as.Date(fec_nac) + lubridate::days(round(30.25*12))
) %>% anti_join(
  salidas %>% select(id)
) %>% group_by(lubridate::year(fec_salida), lubridate::month(fec_salida)) %>% 
count() %>% writexl::write_xlsx("output/proyec_salidas_intervencion.xlsx")


#ENTERICAS ETIQUETAS

data_entericas<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), m17_ga, as.Date(c30_dob))
) %>% select(id, fecha_nacimiento) %>% mutate(
  b2_date=fecha_nacimiento + lubridate::days(round(30.25*6)),
  b3_date=fecha_nacimiento + lubridate::days(round(30.25*9)),
  b4_date=fecha_nacimiento + lubridate::days(round(30.25*9))
) %>% print()

b2_entericas_s8<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_consent) %>% filter(s8_consent=="1") %>% select(id) %>% mutate(
  label_f=paste0(id,"-C1-F0"),
  label_m1=paste0(id,"-C1-M1"),
  label_m2=paste0(id,"-C1-M2")
  
  #entericas_file
  
  gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id)
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value)

b3_entericas_s8<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_consent) %>% filter(s8_consent=="1") %>% select(id) %>% mutate(
  label_f=paste0(id,"-C2-F0"),
  label_m1=paste0(id,"-C2-M1"),
  label_m2=paste0(id,"-C2-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value)

b4_entericas_s8<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_consent) %>% filter(s8_consent=="1") %>% select(id) %>% mutate(
  label_f=paste0(id,"-C3-F0"),
  label_m1=paste0(id,"-C3-M1"),
  label_m2=paste0(id,"-C3-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value)


#sacar las b2 y b3 que se les generará etiqueta
b2_entericas<- data_entericas %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) | is.na(h55_date)) %>% select(id, visit) %>% filter(visit=="b2")
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) | is.na(h55_date)) %>% select(id, visit) %>% filter(visit=="b4")
)

#B2 pendientes
etiquetas_b2<- b2_entericas %>% mutate(
  label_f=paste0(id,"-C1-F0"),
  label_m1=paste0(id,"-C1-M1"),
  label_m2=paste0(id,"-C1-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value) %>% bind_rows(
  b2_entericas_s8
) %>%    print()


#b3 pendientes
etiquetas_b3<- b2_entericas %>% mutate(
  label_f=paste0(id,"-C2-F0"),
  label_m1=paste0(id,"-C2-M1"),
  label_m2=paste0(id,"-C2-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value) %>%   print()
b3_entericas_s8 %>% anti_join(
  
)


#b4 que vienen desde b2
etiquetas_b4<- b2_entericas %>% mutate(
  label_f=paste0(id,"-C3-F0"),
  label_m1=paste0(id,"-C3-M1"),
  label_m2=paste0(id,"-C3-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value) %>%   print()



#sacar las B4 que se les generará etiqueta
b4_entericas<- data_entericas %>%  anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) | is.na(h55_date)) %>% select(id, visit) %>% filter(visit=="b4")
) %>% print()


#b4 pendientes de realizar
etiquetas_b4_pend<- b4_entericas %>% mutate(
  label_f=paste0(id,"-C3-F0"),
  label_m1=paste0(id,"-C3-M1"),
  label_m2=paste0(id,"-C3-M2")
) %>% select(id, label_f,label_m1, label_m2) %>%gather(key = "variable", value="value", -id) %>% arrange(id) %>% select(value) %>%   print()

#B4 pendientes descontando las que vienen de B2
b4_pendientes<-etiquetas_b4_pend %>% anti_join(
 bind_rows(list( etiquetas_b4,
       b4_entericas_s8
 )
 )
)

etiquetas_b2 %>% bind_rows(
  list(
  b2_entericas_s8,
  etiquetas_b3,
  b3_entericas_s8,
  etiquetas_b4,
  b4_entericas_s8,
  b4_pendientes
)
) %>% writexl::write_xlsx("output/etiquetas_entericas_total.xlsx")
  
  
  

##REVISION DE CAMBIO DE CILINDROS INTERVENCION

data_change_lgp<- read_csv("C:/temp/HAPINRegistrocambios_DATA_2020-10-01_1047.csv")
cambio_cilindro<-data_change_lgp %>% mutate_all(as.character)

salidas_intervencion<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="1") %>% left_join(
  salidas %>% select(id, sale)
) %>% filter(!is.na(sale))


salidas_intervencion %>% anti_join(
  cambio_cilindro %>% select(id=record_id)
) %>% write_csv("output/pendientes_registrar_cambio_lgp.csv")

#H56 pendientes
salidas_emory
salidas_uvg %>% select(id=record_id, contact_date, h56g_date) %>% anti_join(
  salidas_emory %>% select(id)
) %>% print(n=Inf)


gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_consent) %>% group_by(s8_consent) %>% count()


gt_emory_data_arm3 %>% filter(id=="33422") %>% filter(!is.na(h40_date)) %>% select(id, h40_date, h40_by) %>% 
  filter(id=="33422") %>% 
  arrange(
  desc(h40_date)  
  )


#revision IDs h51
data_revision<-read_csv("data/revision_h51.csv")
data_revision %>% mutate_all(as.character) %>% select(id=H51_hhid, record_id) %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date)
) %>% print(n=Inf)

#REVISION DE CONSENTIMIENTOS ------
gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, s8_consent, visit) %>% filter(s8_consent=="1") %>% 
left_join( 
  data_entericas %>% transmute(id=as.character(c39_id),colecto_muestra=c39_col_diaper, visit=str_sub(redcap_event_name, 1,2), 
                               fecha_muestra=c39_date, Comentarios=c39_comments) %>% mutate(
    colecto_muestra=recode(colecto_muestra, "1"="Si", "2"="No")
  )
  ) %>% writexl::write_xlsx(paste0("output/revision_consentimientos_entericas_",Sys.Date(),".xlsx"))

consentidas_enterias<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, s8_consent, visit) %>% filter(s8_consent=="1") %>% 
  select(id, consentimiento="Si")
consentimientos_revision<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, s8_consent, visit) %>% filter(s8_consent=="1") %>% 
  left_join( 
    data_entericas %>% transmute(id=as.character(c39_id),colecto_muestra=c39_col_diaper, visit=str_sub(redcap_event_name, 1,2), 
                                 fecha_muestra=c39_date, Comentarios=c39_comments) %>% mutate(
                                   colecto_muestra=recode(colecto_muestra, "1"="Si", "2"="No")
                                 )
  )

escaneados<-read_csv("c:/temp/escaneados.csv")
escaneados<-escaneados %>% mutate_all(as.character)
consentimientos_revision %>% left_join(
  escaneados
) %>% writexl::write_xlsx("output/revision_entericas_escaneados.xlsx")

data_entericas %>% transmute(id=c39_id, c39_col_diaper) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date)
) %>% filter(is.na(s8_date))

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id,  c30_dob, c30_wt_record, c30_where,c30_wt_by,c30_wt_by_other) %>% 
  filter(c30_wt_by=="1")
  group_by(c30_wt_by) %>% count()

# CONSENTIMIENTOS 24 MESES
  
  
  
# CONSENTIMIENTOS 36 MESES -----
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(visit=="b6") %>% select(id, s4_date, s4_by, s4_consent_c) %>% 
    transmute(
      id, fecha_consentimiento=s4_date, iniciales=s4_by,
      consintio=recode(s4_consent_c, "1"="Si", "0"="No")
    ) %>% arrange(fecha_consentimiento) %>% writexl::write_xlsx(
      paste0("output/listado_consentimientos_36m_",Sys.Date(),".xlsx")
    )
  
  

gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by, visit) %>% 
  filter(visit=="baseline") %>% filter(m10_date=="2019-04-11")



gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_by, c30_ave_wt) %>% filter(c30_date > "2020-03-20") %>% 
  arrange(c30_date) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, c30a_date, c30a_by, c30a_ave_wt)
  ) %>% 
  print(n=Inf)


data_entrega<- read_csv("c:/temp/lista_entrega_productos.csv")
data_entrega<-data_entrega %>% mutate_all(as.character)

data_entrega %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `Comunidad embarazada (original z10)`,
                                 `Comunidad embarazada (nueva)`, `Celular embarazada`, `Celular esposo`, `Nombre embarazada`)
) %>% 
  left_join(
  rutas %>% select(`Comunidad embarazada (original z10)`=comunidad, ruta)
) %>% transmute(
  Orden=orden,
  id,
  Nombre_de_la_participante=`Nombre embarazada`,
  Comunidad=`Comunidad embarazada (original z10)`,
  Fecha_solicitud,
  `Codigo articulo solicitado`,
  `Descripcion del Producto`,
  Precio, notas1, `notas 2`,
  ruta,
  `Celular embarazada`, `Celular esposo`, `Nombre embarazada`
  
) %>% write_csv("output/lista_productos_entrega_rutas.csv")



llamadas_covid<-read_csv("c:/temp/HAPINGuatemalaRepeti_DATA_2020-10-23_1648.csv")
llamadas_covid %>% group_by(redcap_event_name) %>% count()
llamadas_covid %>% filter(redcap_event_name=="covid_llamadas_arm_5") %>% select(id, cov_date) %>% mutate(
  fecha=substr(id,7,16)
) %>% arrange(desc(fecha))

gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% select(id, c32_date, visit) %>% filter(c32_date>="2020-03-25" &
                                                                                      c32_date<="2020-04-06") %>% group_by(
                                                                                        visit
                                                                                      ) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, visit) %>% filter(c30_date>="2020-03-25" &
                                                                                             c30_date<="2020-04-06") %>% group_by(
                                                                                               visit
                                                                                             ) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, visit) %>% filter(c31_date>="2020-03-25" &
                                                                                             c31_date<="2020-04-06") %>% group_by(
                                                                                               visit
                                                                                             ) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id, h56_date, visit) %>% filter(h56_date>="2020-03-25" &
                                                                                             c31_date<="2020-04-06") %>% group_by(
                                                                                               visit
                                                                                             ) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id, h56_date, visit) %>% filter(h56_date>="2020-03-25" &
                                                                                             c31_date<="2020-04-06") %>% group_by(
                                                                                               visit
                                                                                             ) %>% count()


#PENDIENTES DE CRF COVID QUE AUN ESTAN ACTIVAS #316 participantes salieron antes de que iniciara el crf covid
#conteo crf covid
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  # anti_join(
  #   salidas %>% select(id)
  # ) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id, v1_date)
  ) %>% mutate(
    s6_arm=recode(s6_arm, "0"="Control", "1"="Intervencion")
  ) %>% arrange(v1_date) %>% group_by(v1_date) %>% count() %>% write_csv("output/conteo_crf_covid.csv")

#lista de activas pendientes de crf covid
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  anti_join(
    salidas %>% select(id)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% mutate(
    fecha_nacimiento=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob))
  ) %>% select(id, fecha_nacimiento, s6_arm) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id, v1_date)
  ) %>% mutate(
    s6_arm=recode(s6_arm, "0"="Control", "1"="Intervencion")
  ) %>% arrange(v1_date) %>% filter(is.na(v1_date)) %>% select(id, s6_arm, fecha_nacimiento) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`,
      `Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`, `Comunidad embarazada (nueva)`,
      `Celular embarazada`, `Celular esposo`, `Celular_otro_miembro`)
  ) %>% left_join(
    rutas %>% select(comunidad, ruta)
  ) %>% 
  write_csv("output/lista_pendientes_crf_covid.csv")


gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id, v1_date) %>% arrange( v1_date)

gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit) %>% filter(e3_date_exit<"2020-07-21") %>% 
  anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o) %>% filter(e3_date_exit_o<"2020-07-21")    
  )


#24 meses
conte_24<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% group_by(
  anio=lubridate::year(`24_meses`),
  mes=lubridate::month(`24_meses`)
) %>% count() %>% print()

conte_36<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% group_by(
  anio=lubridate::year(`36_meses`),
  mes=lubridate::month(`36_meses`)
) %>% count() %>% print()


conte_48<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% group_by(
  anio=lubridate::year(`48_meses`),
  mes=lubridate::month(`48_meses`)
) %>% count() %>% print()

conte_48<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% group_by(
  anio=lubridate::year(`60_meses`),
  mes=lubridate::month(`60_meses`)
) %>% count() %>% print()

salidas

gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% 
  select(id, h40_date, h40_dot1, h40_dot1_v2, redcap_event_name) %>% filter(h40_dot1_v2=="1543" | h40_dot1=="1543")



resumen_misiones<- read_csv("c:/temp/MissionSummary_2020-11-3_1042.csv")
estado_mision<-read_csv("C:/temp/LastKnownStatusofDots_2020-11-3_1002.csv")
estado_mision <- estado_mision %>% mutate_all(as.character)

resumen_misiones<-resumen_misiones %>% mutate_all(as.character)

resumen_misiones %>% anti_join(
  salidas %>% select(hhid=id)
) %>% filter(!is.na(dot_name)) %>%
  #write_csv("output/lista_misiones_sin_id_dot.csv")
  left_join(
  estado_mision %>% select(dot_name, last_update, mission_status, latest_longitude, latest_campaign )
) %>% filter(mission_status=="Running") %>%  
  # write_csv("output/hogares_activos_misiones.csv")
  group_by(hhid) %>% count() %>% print(n=Inf) %>%
  write_csv("output/resumen_hogares_dots_activos.csv")

gt_emory_data_arm2 %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by, a23_disease___555, a23_disease_other, redcap_event_name) %>% 
  filter(a23_disease___555=="1") %>% write_csv("output/a23_B6_other.csv")


m13<-read_csv("c:/temp/HAPINGuatemalaMainSt-M13_DATA_2020-11-02_1036.csv")

m13 %>% filter(!is.na(m13_date)) %>% select(id, m13_date, m13_by, m13_disease___555, m13_disease_other) %>% filter(
  m13_disease___555=="1"
) %>% write_csv("output/enfermedades_m13_B6_otro.csv")


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="0") %>% 
  anti_join(
    salidas %>% select(id)
    #entrega_vales1 %>% select(id=record_id, id_vale) 
  ) %>% left_join(
    entrega_vales1 %>% transmute(id=record_id, id_vale, tipo_entrega="1") %>% bind_rows(
      list(
        entrega_vales2 %>% transmute(id=record_id, id_vale, tipo_entrega="2"),
        entrega_vales3 %>% transmute(id=record_id, id_vale, tipo_entrega="3"),
        entrega_vales4 %>% transmute(id=record_id, id_vale, tipo_entrega="4")
      )
    ) %>% group_by(id) %>% count() %>% arrange(n)
    
  ) %>% write_csv("output/vales_pendientes.csv")

entrega_vales1 %>% transmute(id=record_id, id_vale, tipo_entrega="1") %>% bind_rows(
  list(
    entrega_vales2 %>% transmute(id=record_id, id_vale, tipo_entrega="2"),
    entrega_vales3 %>% transmute(id=record_id, id_vale, tipo_entrega="3"),
    entrega_vales4 %>% transmute(id=record_id, id_vale, tipo_entrega="4")
  )
) %>% group_by(id) %>% count() %>% arrange(n)



#revision grupo intervesión con CArla
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% filter(s6_arm=="1") %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
)


#revisión de hisopados
data_hisopados<-read_csv("c:/temp/HAPINGuatemalaRepeti_DATA_2021-01-22_1546.csv")

data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4")

data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
  select(id, c34h_date, c34h_visit_id,c34h_tipo_muestra) %>% arrange(c34h_date) %>% filter(!is.na(c34h_visit_id)) 

data_hisopados %>% group_by(c34h_consentimiento) %>% count()

#fechas hisopados
data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
  select(id, c34h_date, c34h_visit_id,c34h_tipo_muestra) %>% arrange(c34h_date) %>% filter(!is.na(c34h_visit_id)) %>% 
  filter(!is.na(c34h_tipo_muestra)) %>% group_by(
    lubridate::year(c34h_date),
    lubridate::month(c34h_date),
    lubridate::day(c34h_date)
  ) %>% count() %>% write_csv("output/conteo_hisopados_por_mes_dia.csv")

c36a_emory %>% select(id=record_id, c36a_hhid, c36a_date, c36a_visit,c36a_by) %>%  left_join(
  data_hisopados %>% select(hhid=id, c34h_date, c36a_visit=c34h_visit_id, c34h_by)
) %>% filter(c36a_date>="2020-02-19") %>%
  left_join(
  data_piloto %>% filter(!is.na(c34_date)) %>% select(c34id=record_id, redcap_event_name, c34_date, c34_by, c36a_visit=c34_id_visita)
) %>% select(
  id=c36a_hhid, id_hiso=hhid, id_lus=c34id,
  c36a_by,
  id_visit=c36a_visit,
  fecha_c36=c36a_date,
  fecha_hisopado=c34h_date,
  hisopado_iniciales=c34h_by,
  fecha_lus=c34_date
) %>% write_csv("output/revision_hisopados.csv")
  
  
  data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
    select(id, c34h_date, c34h_visit_id,c34h_tipo_muestra, c34h_by) %>% arrange(c34h_date) %>% 
    filter(!is.na(c34h_visit_id))   %>% group_by( c34h_by) %>% count()

#puntos de GPS
dt_exposure<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2020-11-16_1037.csv")
  
coordenadas_visitas <- dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="linea_de_base_arm_2") %>%
  transmute(
  id, 
  fecha= as.Date(gth4x_date),
  longitud=gth4x_long,
  latitud=gthx_lat,
  elevacion=gthx_elevation,
  visita="baseline"
) %>% bind_rows(
  dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="p1_arm_2") %>%
    transmute(
      id, 
      fecha= as.Date(gth4x_date),
      longitud=gth4x_long,
      latitud=gthx_lat,
      elevacion=gthx_elevation,
      visita="p1"
    )
) %>% bind_rows(
  dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="p2_arm_2") %>%
    transmute(
      id, 
      fecha= as.Date(gth4x_date),
      longitud=gth4x_long,
      latitud=gthx_lat,
      elevacion=gthx_elevation,
      visita="p2"
    )
) %>% bind_rows(
  dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="p2_arm_2") %>%
    transmute(
      id, 
      fecha= as.Date(gth4x_date),
      longitud=gth4x_long,
      latitud=gthx_lat,
      elevacion=gthx_elevation,
      visita="b1"
    )
) %>% bind_rows(
  dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="b2_arm_2") %>%
    transmute(
      id, 
      fecha= as.Date(gth4x_date),
      longitud=gth4x_long,
      latitud=gthx_lat,
      elevacion=gthx_elevation,
      visita="b2"
    )
) %>% bind_rows(
  dt_exposure %>% mutate(id=as.character(record_id)) %>% filter(redcap_event_name=="b4_arm_2") %>%
    transmute(
      id, 
      fecha= as.Date(gth4x_date),
      longitud=gth4x_long,
      latitud=gthx_lat,
      elevacion=gthx_elevation,
      visita="b4"
    )
)

coordenadas_visitas %>% group_by(visita) %>% count()

coordenadas_visitas %>% filter(visita=="p2") %>% transmute(id, p2_date=fecha, p1_long=longitud, p1_lat=latitud, p1_z=elevacion) %>% 
  left_join(
    coordenadas_visitas %>% filter(visita=="baseline") %>% 
      transmute(id, bl_date=fecha, bl_long=longitud, bl_lat=latitud, bl_z=elevacion)
  ) %>% left_join(
    coordenadas_visitas %>% filter(visita=="p1") %>% 
      transmute(id, p1_date=fecha, p1_long=longitud, p1_lat=latitud, p1_z=elevacion)
  ) %>% left_join(
    coordenadas_visitas %>% filter(visita=="b1") %>% 
      transmute(id, b1_date=fecha, b1_long=longitud, b1_lat=latitud, b1_z=elevacion)
  ) %>% left_join(
    coordenadas_visitas %>% filter(visita=="b2") %>% 
      transmute(id, b2_date=fecha, b2_long=longitud, b2_lat=latitud, b2_z=elevacion)
  ) %>% print(n=Inf)


dt_m19<-read_csv("c:/temp/HAPINGuatemalaMainSt_DATA_2020-11-19_1043.csv")
dt_m19<-dt_m19 %>% mutate_all(as.character)

dt_m19 %>% filter(!is.na(m19_date)) %>% select(id, m19_fuel___4, m19_fuel_cost, redcap_event_name) %>% 
  filter(m19_fuel___4=="1") %>% filter(m19_fuel_cost>0) %>% group_by(m19_fuel_cost) %>% count() %>% print(n=Inf)
                  

dt_m19 %>% filter(!is.na(m19_date)) %>% select(id, m19_fuel___4, m19_fuel_cost, redcap_event_name) %>% 
  filter(m19_fuel___4=="1") %>% filter(m19_fuel_cost=="2000") 

gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id, m10_sleep) %>% filter(!is.na(m10_sleep)) %>%  writexl::write_xlsx("output/m10_sleep.xlsx")

dt_m19 %>% filter(!is.na(m19_date)) %>% select(id, m19_fuel___4, m19_fuel_cost, redcap_event_name) %>% 
  filter(m19_fuel___4=="1") %>% filter(m19_fuel_cost!=888) %>%
  filter(redcap_event_name=="linea_de_base_arm_2") %>% filter(m19_fuel_cost>"0") %>% group_by(m19_fuel_cost) %>% count() %>%  %>% arrange(m19_fuel_cost)



dt_m19 %>% filter(!is.na(m19_date)) %>% 
  select(id,m19_fuel___1,  m19_fuel___2, m19_fuel___3, m19_fuel___4, m19_fuel_cost, 
         redcap_event_name) %>% filter( redcap_event_name=="linea_de_base_arm_2"
                                               ) %>% filter(
                                                 m19_fuel___1=="1" | m19_fuel___2=="1" | m19_fuel___3=="1" | m19_fuel___4=="1" 
                                               ) %>% 
  filter(m19_fuel_cost!="888") %>% filter(m19_fuel_cost!="0") %>%  write_csv("output/combustibles.csv")

  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(salidas %>% select(id)) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_date_exit_o)
  ) %>% filter(type=="owa" & is.na(e3_date_exit_o))

  
  salidas_uvg %>%  select(id=record_id, h56g_date) %>% anti_join(
    salidas_emory %>% select(id)
  ) %>%  print(n=Inf)
  
  #sacar la cantidad de hogares que se enrolaron
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
    type=if_else(grepl("^35", id), "owa", "pwg")
  ) %>% 
    #quitar los hogares que tienen salida
    anti_join(
    salidas %>% select(id)
  ) %>% 
    #agrupar por tipo
    group_by(type) %>% count()

  #total de hogares que han salido
  salidas %>% select(id) %>% 
    #identificar los hogares 35
    mutate(
      type=if_else(grepl("^35", id), "owa", "pwg")
    ) %>% 
    #Contar por tipo de hogar
    group_by(type) %>% count()
    
    
  
  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by, visit) %>% filter(
    c31_date>="2020-08-07" & c31_date<="2020-08-11"
  ) %>% print(n=Inf)
  
  
  gt_emory_data_arm2 %>% filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by, visit) %>%
    filter(id=="33476")

  
  
  #leer archivos de una carpeta
  matriz_archivos <- list.files(
    path = "c:/temp", pattern = "HAPIN.+csv", full.names = TRUE
  ) %>%
    tibble(
      file = .,
      export_time = file %>%
        gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
        lubridate::ymd_hm(tz = "America/New_York")
    ) %>%
    slice(which.max(export_time))
  
  


h41_data %>% filter(!is.na(h41_date)) %>%  select(id, redcap_event_name) %>% group_by(redcap_event_name) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% select(id)
) %>% 
  group_by(s6_arm) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>%   
  left_join(
    #agregamos comunidades
    comunidades %>% select(id=house_id, comunidad_1= community, comunidad_2=community_new)
  ) %>% left_join(
    #agregamos rutas
    rutas %>% select(comunidad_1=comunidad, ruta)
  )%>% 
  left_join(
    #agregamos contactos y datos de la participante
    datos_participantes %>% transmute(id=`ID estudio`, 
                                      tel_embarazada=
                                        `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                      tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
  ) %>% write_csv("output/participantes_acitvas_06-01-2021.csv")


intensivo<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2020-12-11_1740.csv")
intensivo<-intensivo %>% mutate_all(as.character)

intensivo %>% filter(redcap_event_name=="blp1_arm_1") %>% select(record_id, h41_date, h41_by) %>% anti_join(
  intensivo %>% filter(redcap_event_name=="b3b4_arm_1") %>% select(record_id)
) %>% anti_join(
  salidas %>% select(record_id=id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(record_id=id, c30_dob)
  ) %>% mutate(
    edad_dias=Sys.Date() - as.Date(c30_dob),
    edad_meses= edad_dias/30.25
    
  ) %>% select(id=record_id, edad_meses)

#revision de conteos con Erick
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
dt_m19 %>% filter(!is.na(m19_date)) %>% 
  select(id,m19_fuel___1,  m19_fuel___2, m19_fuel___3, m19_fuel___4, m19_fuel_cost, 
         redcap_event_name) %>% filter( redcap_event_name=="linea_de_base_arm_2"
         ) %>% filter(
           m19_fuel___1=="1" | m19_fuel___2=="1" | m19_fuel___3=="1" | m19_fuel___4=="1" 
         )
) %>% #filter(m19_fuel_cost!="888" | m19_fuel_cost!="0") %>% 
  group_by(m19_fuel_cost) %>% count()


gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id)

pendientes_b2_b4<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>%  select(id, visit)
) %>% filter(is.na(visit))


pendientes_b4<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>%  select(id, visit)
) %>% filter(!is.na(visit))
  
s8_pendiente_b2_b4<-gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id, visit) %>% filter(visit=="b2")
s8_pendiente_b4<-gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id, visit) %>% filter(visit=="b4") %>% anti_join(
  gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% select(id, visit) %>% filter(visit=="b2") %>% select(id)
)


pendientes_b2_b4 %>% mutate(
  b2_f0=paste0(id,"-C1-F0"),
  b2_m1=paste0(id, "-C1-M1"),
  b2_m2=paste0(id, "-C1-M2"),
  b3_f0=paste0(id,"-C2-F0"),
  b3_m1=paste0(id, "-C2-M1"),
  b3_m2=paste0(id, "-C2-M2"),
  b4_f0=paste0(id,"-C3-F0"),
  b4_m1=paste0(id, "-C3-M1"),
  b4_m2=paste0(id, "-C3-M2")
) %>% bind_rows(
  pendientes_b4 %>% mutate(
    b4_f0=paste0(id,"-C3-F0"),
    b4_m1=paste0(id, "-C3-M1"),
    b4_m2=paste0(id, "-C3-M2")
  )
) %>% bind_rows(
  s8_pendiente_b2_b4 %>% mutate(
    b2_f0=paste0(id,"-C1-F0"),
    b2_m1=paste0(id, "-C1-M1"),
    b2_m2=paste0(id, "-C1-M2"),
    b3_f0=paste0(id,"-C2-F0"),
    b3_m1=paste0(id, "-C2-M1"),
    b3_m2=paste0(id, "-C2-M2"),
    b4_f0=paste0(id,"-C3-F0"),
    b4_m1=paste0(id, "-C3-M1"),
    b4_m2=paste0(id, "-C3-M2")
  )
) %>% bind_rows(
  s8_pendiente_b4 %>% mutate(
    b3_f0=paste0(id,"-C2-F0"),
    b3_m1=paste0(id, "-C2-M1"),
    b3_m2=paste0(id, "-C2-M2"),
    b4_f0=paste0(id,"-C3-F0"),
    b4_m1=paste0(id, "-C3-M1"),
    b4_m2=paste0(id, "-C3-M2")
  )
) %>%  select(-visit) %>% gather(
  ., key="variable", value="value", -id
) %>% filter(!is.na(value)) %>% select(id, eitiqueta=value) %>% writexl::write_xlsx("output/etiquetas_entericas.xlsx")







#revision de pesos con Adly
lista_redcap<-read_csv("C:/Users/aramirez/Downloads/lista_pesos_madre_total_12-11-2020.csv")
lista_redcap <- lista_redcap %>% mutate_all(as.character)

lista_fabiola<-read_csv("C:/Users/aramirez/Downloads/pesos madre carnet.csv")
lista_fabiola<-lista_fabiola %>% mutate_all(as.character)


lista_redcap
pesos_carnet<-lista_fabiola %>% transmute(
  id, peso_1=peso1, fecha1=fecha,
  peso2=peso2_1, fecha2=fecha_1,
  peso3, fecha3=fecha_2,
  peso4=`peso 4`, fecha4=fecha_3,
  peso5=`peso 5`, fecha5=fecha_4,
  peso6=`peso 6`, fecha6=fecha_5,
  peso7=`peso 7`, fecha7=fecha_6,
  peso8=`peso 8`, fecha8=fecha_7
)


lista_productos<-read_csv("C:/Users/aramirez/Downloads/entrega_productos_diciembre.csv")
lista_productos<-lista_productos %>% mutate_all(as.character)

lista_productos %>% mutate(comunidad=Comunidad) %>%  left_join(
    datos_participantes %>% select(ID=`ID estudio`, `Celular embarazada`, `Celular esposo`, Celular_otro_miembro)
) %>% left_join(
  rutas
) %>% 
  write_csv("output/datos_entrega_productoa_diciembre.csv")

salidas_uvg %>% select(id=record_id) %>% anti_join(
  salidas_emory %>% select(id)
)


gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id,c31_date,  c31_h_facility, c31_h_facility_2, visit) %>% 
  filter(c31_h_facility=="1" | c31_h_facility_2=="1") %>% left_join(
    
  )


salidas %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id, h56_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_reason)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_reason_o)
) %>% mutate(
  type=if_else(grepl("^35", id), "owa", "pwg")
) %>% filter(e3_reason=="1" | e3_reason_o=="1") %>% filter(is.na(h56_date))


data_filtros<-read_csv("c:/temp/HAPINGuatemalaFiltro_DATA_2020-12-18_1610.csv")

data_filtros %>% select()


gt_emory_data_arm3 %>% filter(!is.na(h46_date)) %>% select(id, redcap_event_name, matches("h46_"))

data_h46<-read_csv("data/h46/HAPINGuatemalaRepeat-H46gt_DATA_2021-01-07_1730.csv")

data_h46 %>% filter(h46b_filter1=="0") %>% group_by(h46b_hhid,lubridate::year(h46b_date), lubridate::month(h46b_date)) %>% count() %>% 
  print(n=Inf) %>% write_csv("output/conteo_h46_sinnfiltro.csv")
data_h46 %>% filter(h46b_filter1=="1") %>% group_by(h46b_hhid,lubridate::year(h46b_date), lubridate::month(h46b_date)) %>% count() %>% 
  print(n=Inf) %>% write_csv("output/conteo_h46_confiltro.csv")

data_h46 %>% filter(is.na(h46b_filter1)) %>% group_by(h46b_hhid,lubridate::year(h46b_date), lubridate::month(h46b_date)) %>% count() %>% 
  print(n=Inf) %>% write_csv("output/conteo_h46_na_filtro.csv")

data_h46 %>%  group_by(h46b_hhid,lubridate::year(h46b_date), lubridate::month(h46b_date), h46b_filter1) %>% count() %>% 
  print(n=Inf)

data_h46 %>% filter(is.na(h46b_filter1)) %>% select(record_id)

data_h46 %>% filter(is.na(h46b_hhid)) %>% select(record_id, h46b_by, h46b_date, h46b_hhid)

data_h46 %>% filter(is.na(h46b_filter1) ) %>% select(record_id, h46b_hhid) %>% print(n=Inf)

data_h46 %>% filter(h46b_hhid=="35026")%>% select(
  record_id, h46b_hhid, h46b_date, h46b_by, h46b_instrument___1,h46b_instrument___2,
  h46b_instrument___4, h46b_instrument___555, h46b_instrument_other,h46b_filter4_id
) %>% mutate(
  h46b_instrument___1=recode(h46b_instrument___1,"0"="","1"="E-Sampler"),
  h46b_instrument___2=recode(h46b_instrument___2,"0"="","1"="pDR"),
  h46b_instrument___4=recode(h46b_instrument___4,"0"="","1"="Bomba"),
  h46b_instrument___555=recode(h46b_instrument___555,"0"="","1"="Otro (PATS)")
) %>% write_csv("output/listado_35026_casella_revision.csv")

data_h46 %>% select(record_id, h46b_hhid,h46b_date,h46b_by,h46b_filter1) %>% 
  write_csv("output/listado_h46_Esampler.csv")


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% mutate(
  edad_meses=case_when(
    !is.na(fecha_nacimiento) ~  ((Sys.Date() - as.Date(fecha_nacimiento))/30.25),
    TRUE ~ NA_real_
  )
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, Nombre_bb, `Nombre embarazada`, `Comunidad embarazada (original z10)`, )
) %>% write_csv("output/listado_bebes_activos.csv")



data_h41b<-read_csv("c:/temp/HAPINGuatemalaMainSt-H41b_DATA_2021-01-08_1556.csv")
data_h41b %>% filter(!is.na(h41b_date)) %>% select(id, redcap_event_name, h41b_device, 	
                                                   h41b_envir1, 	h41b_type1, h41b_type2 ) %>% 
  filter(	h41b_type1=="1") %>% group_by(redcap_event_name) %>% count()



dt_h40_full<-read_csv("c:/temp/HAPINGuatemalaMainSt-H40h40b_DATA_2021-01-11_1309.csv")

dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, h40_dl_date1, h40_dl_time1,
                                                     h40_dl_date2, h40_dl_time2,
                                                     h40_dl_date3, h40_dl_time3,
                                                     h40_dl_date4, h40_dl_time4,
                                                     h40_dl_date1_v2, h40_dl_time1_v2,
                                                     h40_dl_date2_v2, h40_dl_time2_v2,
                                                     h40_dl_date3_v2, h40_dl_time3_v2,
                                                     h40_dl_date4_v2, h40_dl_time4_v2) %>% 
  mutate_all(as.character) %>%
  gather(
    key=variable, value = value, -id, -h40_date, -redcap_event_name
  ) %>%  mutate(
    fecha=case_when(
      variable=="h40_dl_date1" ~ value,
      variable=="h40_dl_date2" ~ value,
      variable=="h40_dl_date3" ~ value,
      variable=="h40_dl_date4" ~ value,
      variable=="h40_dl_date1_v2" ~ value,
      variable=="h40_dl_date2_v2" ~ value,
      variable=="h40_dl_date3_v2" ~ value,
      variable=="h40_dl_date4_v2" ~ value,
    )
    
  ) %>% transmute(id, h40_date, redcap_event_name, variable, fecha ) %>% filter(as.Date(fecha)=="2020-09-24") %>% 
  arrange(id)







dt_h40_full %>% filter(!is.na(h40_date)) %>%  transmute(id, h40_date, redcap_event_name, fecha=h40_dl_date1, hora=h40_dl_time1) %>% 
  mutate_all(as.character) %>% filter(!is.na(fecha)) %>% bind_rows(
    list(
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date2, hora=h40_dl_time2) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date3, hora=h40_dl_time3) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date4, hora=h40_dl_time4) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date1_v2, hora=h40_dl_time1_v2) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date2_v2, hora=h40_dl_time2_v2) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date3_v2, hora=h40_dl_time3_v2) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha)),
      dt_h40_full %>% filter(!is.na(h40_date)) %>%  select(id, h40_date, redcap_event_name, fecha=h40_dl_date4_v2, hora=h40_dl_time4_v2) %>% 
        mutate_all(as.character) %>% filter(!is.na(fecha))
    )
  )  %>% filter(h40_date=="2019-06-17") %>% arrange(fecha)
  filter(fecha=="2019-05-07") %>% arrange(id)



#REVISION DE DATOS DE HISOPADOS Y MUESTRAS PROCESADAS
  data_repeticiones_hisopados<-read_csv("c:/temp/HAPINGuatemalaRepeti_DATA_2021-01-14_1149.csv")


data_repeticiones_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>% select(
  id, c34h_id, matches("c34")
) %>% filter(!is.na(c34h_id))


data_repeticiones_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>% select(
  id, c34h_id, matches("c34")
) %>% filter(!is.na(c34h_id_procesa))

c34h<-data_repeticiones_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>% select(
  id, c34h_id, matches("c34")
) %>% select(
  id, c34h_id,
  c34h_date,
  c34h_visit_id,
  c34h_consentimiento
) %>% filter(!is.na(c34h_id)) %>% mutate_all(as.character) %>% mutate(id_visita=c34h_visit_id) %>% 
  left_join(
    data_repeticiones_hisopados %>% filter(!is.na(c34h_id_procesa)) %>% select(c34h_date_procesa, id_visita= c34h_id_procesa)
  ) 

dt_repeticiones<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2021-01-15_1110.csv")

dt_repeticiones %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("c34")) %>% 
  filter(!is.na(c34a_date)) %>% mutate(id=as.character(c34a_hhid)) %>% filter(
    c34a_date>="2020-02-20"
  ) %>% transmute(record_id, id, c34a_date, id_visita=c34a_visit) %>% left_join(
    c34h %>% transmute(c34h_id, c34h_date, id_visita, c34h_date_procesa )
  ) %>% filter(!is.na(c34h_id)) %>% write_csv("output/c34a_con_hisopado.csv")

c34h %>% transmute(c34h_id, c34h_date, id_visita, c34h_date_procesa ) %>% left_join(
  dt_repeticiones %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("c34")) %>% 
    filter(!is.na(c34a_date)) %>% mutate(id=as.character(c34a_hhid)) %>% filter(
      c34a_date>="2020-02-20"
    ) %>% transmute(record_id, id, c34a_date, id_visita=c34a_visit)
) %>% filter(is.na(c34a_date)) %>% write_csv("output/hisopados_sin_c34a.csv")



c34<-dt_repeticiones %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("c34")) %>% 
  filter(!is.na(c34a_date)) %>% mutate(id=as.character(c34a_hhid))

c36a<-dt_repeticiones %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("c36")) %>% 
  filter(!is.na(c36a_date)) %>% mutate(id=as.character(c36a_hhid))

c34h %>% transmute(c34h_id, c34h_date, id_visita, c34h_date_procesa ) %>% left_join(
  c36a %>% transmute(record_id, id, c36a_date, c36a_oxy_60, c36a_oxy_90, c36a_oxy_120, id_visita=c36a_visit ) %>% mutate(
    hypoxemia = (c36a_oxy_60 < 92 | c36a_oxy_90 < 92 | c36a_oxy_120 < 92)
  ) %>% filter(
    c36a_date>="2020-02-18"
  ) 
) %>%  left_join(
  dt_repeticiones %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("c34")) %>% 
    filter(!is.na(c34a_date)) %>% mutate(id=as.character(c34a_hhid)) %>% filter(
      c34a_date>="2020-02-15"
    ) %>% transmute(c34a_date, id_visita=c34a_visit)
) %>% filter(is.na(c34a_date))

  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% filter(id=="33660") %>% select(id, c30_dob)

  #revisar los que deberían tener ultrasonido
    c36a %>% mutate(
    hypoxemia = (c36a_oxy_60 < 92 | c36a_oxy_90 < 92 | c36a_oxy_120 < 92)
  ) %>% filter(
    c36a_date>="2020-02-18"
  ) %>% mutate(
    ultrasonido = (
     ( c36a_stridor == "1" | c36a_grunt == "1" | c36a_s_indraw == "1" | c36a_drink == "1" | c36a_vomit == "1" 
       | c36a_convulsion == "1" | c36a_unconscious == "1" | c36a_feed == "1" | c36a_move == "1" | 
        c36a_malnutrition == "1" | c36a_oxy_60 <= 92 | c36a_oxy_90 <= 92 | c36a_oxy_120 <= 92 | c36a_temp > 38 | 
       c36a_temp < 35.5 | ( c36a_age < 2 & (c36a_rr1 >= 60 | c36a_rr2 >=60 )) )
    )
  ) %>% mutate(
    id_visita=c36a_visit
  ) %>% mutate(
    oms=c36a_indraw=="1" | (c36a_age<2 & (c36a_rr1>=60 | c36a_rr2>=60)) | 
      (c36a_age > 2 & (c36a_rr1>=50 | c36a_rr2>=50)) 
  ) %>% group_by(ultrasonido, oms) %>% count()
  
  
    c34h %>% select(c34h_date, id_visita) %>% left_join(
      
      c36a %>% mutate(
        hypoxemia = (c36a_oxy_60 < 92 | c36a_oxy_90 < 92 | c36a_oxy_120 < 92)
      ) %>% filter(
        c36a_date>="2020-02-18"
      ) %>% mutate(
        ultrasonido = (
          ( c36a_stridor == "1" | c36a_grunt == "1" | c36a_s_indraw == "1" | c36a_drink == "1" | c36a_vomit == "1" 
            | c36a_convulsion == "1" | c36a_unconscious == "1" | c36a_feed == "1" | c36a_move == "1" | 
              c36a_malnutrition == "1" | c36a_oxy_60 <= 92 | c36a_oxy_90 <= 92 | c36a_oxy_120 <= 92 | c36a_temp > 38 | 
              c36a_temp < 35.5 | ( c36a_age < 2 & (c36a_rr1 >= 60 | c36a_rr2 >=60 )) )
        )
      ) %>% mutate(
        id_visita=c36a_visit
      )
    ) %>% select(c34h_date, id_visita, ultrasonido, c36a_hhid)
    
    c36a %>% mutate(
      hypoxemia = (c36a_oxy_60 < 92 | c36a_oxy_90 < 92 | c36a_oxy_120 < 92)
    ) %>% filter(
      c36a_date>="2020-02-18"
    ) %>% mutate(
      ultrasonido = (
        ( c36a_stridor == "1" | c36a_grunt == "1" | c36a_s_indraw == "1" | c36a_drink == "1" | c36a_vomit == "1" 
          | c36a_convulsion == "1" | c36a_unconscious == "1" | c36a_feed == "1" | c36a_move == "1" | 
            c36a_malnutrition == "1" | c36a_oxy_60 <= 92 | c36a_oxy_90 <= 92 | c36a_oxy_120 <= 92 | c36a_temp > 38 | 
            c36a_temp < 35.5 | ( c36a_age < 2 & (c36a_rr1 >= 60 | c36a_rr2 >=60 )) )
      )
    ) %>% mutate(
      id_visita=c36a_visit
    ) %>% select(record_id, id_visita, c36a_date, ultrasonido) %>% filter(ultrasonido==TRUE) %>% anti_join(
      c34h %>% select(id_visita)
    )
    
    
    c36a %>% mutate(
      hypoxemia = (c36a_oxy_60 < 92 | c36a_oxy_90 < 92 | c36a_oxy_120 < 92)
    ) %>% filter(
      c36a_date>="2020-02-18"
    ) %>% mutate(
      ultrasonido = (
        ( c36a_stridor == "1" | c36a_grunt == "1" | c36a_s_indraw == "1" | c36a_drink == "1" | c36a_vomit == "1" 
          | c36a_convulsion == "1" | c36a_unconscious == "1" | c36a_feed == "1" | c36a_move == "1" | 
            c36a_malnutrition == "1" | c36a_oxy_60 <= 92 | c36a_oxy_90 <= 92 | c36a_oxy_120 <= 92 | c36a_temp > 38 | 
            c36a_temp < 35.5 | ( c36a_age < 2 & (c36a_rr1 >= 60 | c36a_rr2 >=60 )) )
      )
    ) %>% mutate(
      oms=c36a_indraw=="1" | (c36a_age<2 & (c36a_rr1>=60 | c36a_rr2>=60)) | 
        (c36a_age > 2 & (c36a_rr1>=50 | c36a_rr2>=50)) 
    ) %>% mutate(
      id_visita=c36a_visit
    ) %>% select(record_id, id_visita, c36a_date, ultrasonido, oms) %>% filter(oms==TRUE) %>% anti_join(
      c34 %>% select(id_visita=c34a_visit) 
    ) %>% write_csv("output/verificar_c34a_pendiente.csv")
    
    
    #revision consentimientos y muestras de entericas
    gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, 
                                                              s8_consent, visit) %>% filter(
                                 s8_consent=="1") %>%  left_join( 
        data_entericas %>% transmute(
          id=as.character(c39_id),colecto_muestra=c39_col_diaper, 
          visit=str_sub(redcap_event_name, 1,2), 
                                     fecha_muestra=c39_date, 
          Comentarios=c39_comments) %>% mutate(
                                       colecto_muestra=recode(
                                         colecto_muestra, "1"="Si", "2"="No")
                                     )
      ) %>% filter(is.na(fecha_muestra))
    

    
    
 data_h40<-read_csv("c:/temp/data_dots/HAPINGuatemalaMainSt-H40h40b_DATA_2021-01-19_1301.csv")
  data_h40 %>% group_by(redcap_event_name) %>% count()
  
  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(!is.na(m10_house_eco)) %>% select(id, m10_house_eco)
    )
  
  gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, visit, s8_date, s8_consent) %>% filter(s8_consent=="1") %>% 
    filter(visit=="b2") %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
    ) %>% mutate(
      edad_dias= (Sys.Date() - as.Date(c30_dob))/30.25,
    ) %>% write_csv("output/consentidas_b2.csv")

  #hogares 35 que b10 BL indica que no había adulta participando
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>%mutate(
    type=if_else(grepl("^35",id), "owa","pwg")
  ) %>% filter(type=="owa") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="baseline") %>% 
      select(id, b10_date, orina=b10_to_u_code,sangre_seca=b10_to_b_code, b10_v_code1,b10_other_w,b10_notes)
  ) %>% filter(b10_other_w=="0")

  #muestras recolectadas join con listado de Priya
listado_priya<-read_csv("c:/temp/sample_list_bl.csv")
listado_priya<-listado_priya %>% mutate_all(as.character)

 dt_muestras_bl<- gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>%mutate(
    type=if_else(grepl("^35",id), "owa","pwg")
  ) %>% filter(type=="owa") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(visit=="baseline") %>% 
      select(id, b10_date, orina=b10_to_u_code,sangre_seca=b10_to_b_code, 
             sangre_tubo=b10_v_code1, celula_bucal=b10_buccal, 
             enjuague_oral=b10_oral, raspado_nasal=b10_nasal,b10_other_w,b10_notes)
  ) %>% left_join(
    listado_priya
  ) #%>% writexl::write_xlsx("output/seguimiento_muestras_adultas_bl.xlsx")

 #leer datos proyecto de Lab redcap local
 datos_lab<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2021-01-27_1132.csv")
 #tubos y contenedores
 tubos_lab<-datos_lab %>% filter(!is.na(aliquot_container_id)) %>% select(aliquot_container_id,
                                                              alicuot_storage_date,
                                                              alicuot_storage_by,
                                                              matches("aliquot_id")) %>% gather(
                                                                key="variable", value="value",
                                                                -aliquot_container_id, -alicuot_storage_date, -alicuot_storage_by
                                                              ) %>% filter(!is.na(value)) %>% mutate(
                                                                id_sample=value
                                                              ) %>% select(
                                                                contenedor=aliquot_container_id, fecha=alicuot_storage_date,
                                                                iniciales=alicuot_storage_by, id_sample
                                                              )

 dt_envios<-datos_lab %>% filter(!is.na(shipment_id)) %>% select(shipment_id,
                                                      shipment_date,shipment_from,
                                                      shipment_to,
                                                      matches("container_id"),
                                                     ) %>% gather(
                                                      key = "variable", 
                                                      value = "value",
                                                      -shipment_id, -shipment_date,
                                                      -shipment_from, -shipment_to
                                                      ) %>% filter(!is.na(value)) %>% mutate(
                                                        id_contenedor=value
                                                      ) %>% select(shipment_id,
                                                                   fecha_envio=shipment_date,
                                                                   desde=shipment_from,
                                                                   hacia=shipment_to,
                                                                   contenedor=value)
 
dt_muestras_envios<- tubos_lab %>% left_join(
   dt_envios %>% select(contenedor, fecha_envio, hacia)
 ) %>% mutate(id=substr(id_sample, 1, 5)) %>% mutate(
   type=if_else(
     grepl("^35", id), "owa", "pwg"
   )
 ) %>% filter(type=="owa") %>%arrange(id_sample) %>% mutate(
   flag1=substr(id_sample, 10, 11),
   flag2=substr(id_sample, 7, 8)
 ) %>% filter(flag1=="U1") %>% filter(flag2=="O1") %>% mutate(
   orina=paste0(substr(id_sample, 1,9),"U0")
 ) %>% distinct(contenedor,fecha, fecha_envio, hacia, orina)
 

dt_muestras_bl %>% select(id, b10_date, orina, b10_other_w, b10_notes, lista_priya=sample_id) %>% 
  left_join(
  dt_muestras_envios 
  ) %>% writexl::write_xlsx("output/muestras_orina_contenedores.xlsx")
 

tubos_lab %>% filter(substr(id_sample,1,10)=="35005-O6-P")
tubos_lab %>% filter(contenedor=="Box-0402") %>% print(n=800)

cilindros100<-read_csv("c:/temp/cilindros_100.csv")
clindros_cien <- cilindros100 %>% mutate_all(as.character)

clindros_cien %>%   left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% mutate_all(as.character) %>%  select(id, 
                                                            id_cilindro_inicial=h50_tank_id1, peso_cilindro1=h50_tank_wt1,
                                                            id_cilindro_inicial2= h50_tank_id2, 
                                                            peso_cilindro2=h50_tank_wt2)
) %>% left_join(
  all_gas_actions %>% filter(action=="install") %>% 
    select(id=house_id, source, cyl_id, cyl_weight, date)
    
) %>% write_csv("output/cilindros_cien_historico.csv")


all_gas_actions %>% filter(house_id=="33014") %>% print(n=Inf)
all_gas_actions %>% filter(cyl_id=="UVG-0002") %>% print(n=Inf)

all_gas_actions %>% write_csv("output/all_gas_action.csv")


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fec_nacimiento=c30_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, salida_madre=e3_date_exit)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, salida_adulta=e3_date_exit_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, fec_install_estufa=h50_date)
  ) %>% mutate(
    dias_participo=as.Date(salida_adulta) - as.Date(fec_install_estufa)
  ) %>%  filter(id=="35001" | id=="35109" | id=="35113") %>% writexl::write_xlsx(
    "output/revisión_estufas.xlsx"
  )

dt_h41<-read_csv("c:/temp/HAPINGuatemalaMainSt-H41completo_DATA_2021-02-03_1236.csv")
dt_h41 <- dt_h41 %>% filter(!is.na(h41_date)) %>%  mutate_all(as.character)
dt_h41 %>% filter(!is.na(h41_date)) %>% group_by(h41_visit) %>% count()
dt_h41 %>% select(id,redcap_event_name, h41_visit, h41_by,h41_pri_roof,h41_pri_wall,
                  h41_k_length, h41_k_width,h41_k_diameter) %>% 
  filter(h41_k_diameter!="888")
  group_by(h41_k_diameter) %>% count() %>% print(n=Inf)


h41_pri_roof
h41_pri_wall

datos_lab %>% group_by(redcap_event_name) %>% count()

datos_lab %>% filter(redcap_event_name=="registro_arm_2") %>% select(aliquot_container_id, matches("aliquot_id_"))


gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by, m14_bp_num) %>%
  filter(m14b_date=="2018-08-28")

gt_emory_repeat_data %>% filter(!is.na(h54_date)) %>% select(id, h54_date, h54_by) %>% filter(h54_date=="2019-06-11")


gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_by, m14b_date, m14_bp_num, visit) %>% filter(
  m14b_date=="2019-12-19"
) %>% print(n=Inf)


gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_by, m14b_date, m14_bp_num, visit,
                                                            m14_pregnant) %>% filter(
  visit=="b4"
) %>% filter(
  id=="33504" |
    id=="33506" |
    id=="33528" |
    id=="33634"
)

gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_by, m14b_date, m14_bp_num, visit,
                                                            m14_pregnant) %>% filter(
                                                              visit=="b4"
                                                            ) %>% filter(
                                                              id=="33634" 
                                                            )


gt_emory_repeat_data %>% filter(!is.na(c36a_date)) %>% select(record_id, id=c36a_hhid, c36a_dob) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% filter(
  id=="33018"    |
    id=="33156"  |
    id=="33162"  |
    id=="33219"  |
    id=="33272"  |
    id=="33291"  |
    id=="33327" |
    id=="33357"  |
    id=="33453"  |
    id=="33588"  |
    id=="33660"  |
    id=="35097"  
) %>% filter(as.Date(c36a_dob) != as.Date(c30_dob))


gt_emory_repeat_data %>% filter(!is.na(c36a_date)) %>% select(record_id, id=c36a_hhid, c36a_dob) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% filter(
  
    id=="33588"  
) 

gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% filter(
  id=="33327" | id=="33453"
)

gt_emory_repeat_data %>% filter(!is.na(c40_date)) %>% select(record_id, id=c40_hhid, c40_dob) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% filter(
    id=="33336"  |
      id=="33409"  |
      id=="33414"  |
      id=="33498"  |
      id=="33527"  |
      id=="33555"  |
      id=="33592"  |
      id=="33595"  
  ) %>% filter(as.Date(c40_dob)!= as.Date(c30_dob))


gt_emory_data_arm2 %>% filter(!is.na(h53_date)) %>% select(id, h53_date, h53_by, visita=as.character(visit)) %>% bind_rows(
  gt_emory_data_arm3 %>% filter(!is.na(h53_date)) %>% select(id, h53_date, h53_by, visit)
)

all_gas_actions %>% filter(cyl_id=="UVG-0187")


#hapin 1-5
dt_llamadas<-read_csv("data/hapin1_5/Llamadashapin15_DATA_2021-03-05_1629.csv")
contactadas_hapin_1_5<-dt_llamadas %>% filter(!is.na(call_id_hapin)) %>% mutate(call_id_hapin=as.character(call_id_hapin)) %>% 
  filter(call_successfull=="1" | call_successfull2=="1" | call_successfull3=="1") %>% mutate(
    telefono=case_when(
      call_successfull=="1" ~ paste0("tel 1: ",call_tel1),
      call_successfull2=="1" ~ paste0("tel 2: ",call_tel2),
      call_successfull3=="1"~ paste0("tel 3: ",call_tel3)
    ),
    vive_mismo_lugar=call_update_contact1
  ) %>% select(id=call_id_hapin, iniciales=call_by, fecha=call_date, telefono, vive_mismo_lugar) %>% print() 


dt_todas_llamadas<-dt_llamadas %>% filter(!is.na(call_id_hapin)) %>% mutate(call_id_hapin=as.character(call_id_hapin)) %>% 
  filter(call_successfull!="1" | call_successfull2!="1" | call_successfull3!="1") %>% mutate(
    telefono=case_when(
      call_successfull=="1" ~ paste0("tel 1: ",call_tel1),
      call_successfull2=="1" ~ paste0("tel 2: ",call_tel2),
      call_successfull3=="1"~ paste0("tel 3: ",call_tel3)
    ),
    vive_mismo_lugar=call_update_contact1
  ) %>% select(id=call_id_hapin, iniciales=call_by, fecha=call_date, telefono, vive_mismo_lugar) %>% print() 

nocontactadas<-dt_todas_llamadas %>% anti_join(
  contactadas_hapin_1_5 %>% select(id)
) 

candidatos<-read_csv("data/hapin1_5/candidatos_dic-marzo.csv")
candidatos<-candidatos %>% mutate_all(as.character)

sin_registro_intento<-candidatos %>% anti_join(
  contactadas_hapin_1_5 %>%  bind_rows(nocontactadas) %>% select(id)
) %>% select(id, fecha_nacimiento)


list(
  contactadas = contactadas_hapin_1_5,
  no_contactadas = nocontactadas,
  sin_intento = sin_registro_intento
) %>% writexl::write_xlsx(paste0("output/reporte_llamadas_hapin_", Sys.Date(),".xlsx"))



dt_bolivar<-read_csv("c:/temp/BOLIVAR/BOLIVAR.csv")
dt_bolivar %>% group_by(ESTADO_CONTRIBUYENTE) %>% count()




gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% 
  filter(id=="35045" ) %>% 
  select(id, a24b_date, a24b_by, visit, a24_pregnant, a24_bp_refer,
         a24_dbp1, a24_dbp2,a24_dbp3)

gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, 
                                                            m14b_by, m14_bp_num) %>% 
  filter(m14b_date=="2020-08-18") %>% print(n=Inf)

ls_enero<-read_csv("c:/temp/listado_enero.csv")

ls_enero %>%mutate_all(as.character) %>%  left_join(
  datos_participantes %>% mutate_all(as.character) %>%  select(ID=`ID estudio`,
                                 `Celular embarazada`,
                                 `Celular esposo`,
                                 `Celular_otro_miembro`)
) %>% write_csv("c:/temp/productos_enero2021_telefono.csv")



#REvision de CIMT
cimt_baseline<-gt_emory_data_arm2 %>%  filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, visit) %>% 
  filter(visit=="baseline") %>% mutate(baseline="Si") %>% select(id, fecha=a26_date, iniciales_bl=a26_by,visit) %>% print()
cimt_b4<-gt_emory_data_arm2 %>%  filter(!is.na(a26_date)) %>% filter(visit=="b4") %>% select(id, fecha=a26_date, iniciales=a26_by, visit) %>% print()

cimt_baseline %>% bind_rows(
  cimt_b4
) %>% print(n=Inf)


solo_b4<-cimt_b4 %>% anti_join(
  cimt_baseline %>% select(id)
)

solo_bl<-cimt_baseline %>% anti_join(
  cimt_b4 %>% select(id)
)
)

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% filter(type=="owa") %>% left_join(
cimt_baseline %>% select(id, fecha_bl=fecha, iniciales_bl) %>%  left_join(
  cimt_b4 %>% select(id, fecha_b4=fecha, iniciales_b4=iniciales)
) %>% bind_rows(
  solo_b4 %>% select(id, fecha_b4=fecha, iniciales_b4=iniciales)
)
)%>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, fecha_salida_adulta=e3_date_exit_o, e3_reason_o) %>% mutate(
    motivo_salida=case_when(
      e3_reason_o=="1" ~ "Finalizacion del estudio",
      e3_reason_o=="2" ~ "No elegible",
      e3_reason_o=="3" ~ "Retiro voluntario de laparticipante",
      e3_reason_o=="4" ~ "Retirada por el equipo del estudio",
      e3_reason_o=="5" ~ "Se mudo del area de estudio",
      e3_reason_o=="6" ~ "Fallecio",
      e3_reason_o=="7" ~ "Perdido durante el seguimiento",
      e3_reason_o=="555" ~ "Otro"
    )
  )
) %>% select(-e3_reason_o) %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>%  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)),
  cumple_anio= as.Date(fecha_nacimiento)+lubridate::years(1)
) %>% select(id_tamizaje, id, date_cimt_basal=fecha_bl,
             iniciales_cimt_basal=iniciales_bl,
             date_cimt_b4=fecha_b4, iniciales_cimt_b4=iniciales_b4,
             fecha_salida_adulta, motivo_salida,
             fecha_cumple_b4=cumple_anio) %>% 
  mutate(temporada_covid=
           if_else(
             as.Date(fecha_salida_adulta)>"2020-03-24" & 
               as.Date(fecha_salida_adulta)<"2020-07-03","Si","No"
           )) %>% writexl::write_xlsx("output/revision_cimt_02-03-2021.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% filter(
  as.Date(c33_date)>"2020-01-28" & as.Date(c33_date)<"2020-08-01"
) %>% select(id, c33_date,c33_wt1, visit) %>% arrange(c33_date) %>%  print(n=Inf)

#revision de participantes actival al 02-03-2021

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>%  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)),
  cumple_anio= as.Date(fecha_nacimiento)+lubridate::days(365),
  edad_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
) %>% select(-c30_dob, -m17_ga) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`,
                                 comunidad=`Comunidad embarazada (original z10)`,
                                 `Comunidad embarazada (nueva)`,
                                 `Celular embarazada`,
                                 `Celular esposo`,
                                 `Celular_otro_miembro`)
) %>% left_join(
  rutas
) %>% write_csv("output/revision_participantes_activas.csv")


#pendientes de consentir en entericas
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>%  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)),
  cumple_anio= as.Date(fecha_nacimiento)+lubridate::days(365),
  edad_meses=(Sys.Date()-as.Date(fecha_nacimiento))/30.25
) %>% select(-c30_dob, -m17_ga) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`,
                                 comunidad=`Comunidad embarazada (original z10)`,
                                 `Comunidad embarazada (nueva)`,
                                 `Celular embarazada`,
                                 `Celular esposo`,
                                 `Celular_otro_miembro`)
) %>% left_join(
  rutas
) %>% anti_join(
  consentidas_enterias<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, s8_consent, visit) %>% filter(s8_consent=="1") %>% 
    select(id)
  
) %>% write_csv("output/pendientes_consentir_entericas.csv")

consentidas_enterias<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_by, s8_consent, visit) %>% filter(s8_consent=="1") %>% 
  select(id, consentimiento="Si")

#leer y graficar datos de purple air
files <- list.files(path="c:/temp/PA_01 HAPIN/")
tmp <- lapply(files, read.csv, header = FALSE)
matriz <- do.call(rbind, tmp)
header <- scan(files[1], nlines = 1, sep = ",")
names(matriz) <- header




gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% group_by(type) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c33_date) & !is.na(c33_wt1)) %>% select(id, c33_date, c33_wt1) %>% 
  filter(as.Date(c33_date)>"2020-03-01" & as.Date(c33_date)<"2020-09-01") %>% 
  arrange(c33_date) %>%  print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(h41_date) & !is.na(h41_m_st)) %>% select(id, h41_date, h41_m_st) %>% 
  filter(as.Date(h41_date)>"2020-03-01" & as.Date(h41_date)<"2020-08-01") %>% 
  arrange(h41_date) %>%  print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, matches("h41_")) %>%  filter(as.Date(h41_date)>"2020-03-01" & as.Date(h41_date)<"2020-08-01") %>% 
  arrange(h41_date) %>%  print(n=Inf)

#resumen de E3 por tipo de salida


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_o_col_time) %>%  
  filter(as.Date(b10_date)>"2020-03-01" & as.Date(b10_date)<"2020-08-01") %>% 
  arrange(b10_date) %>%  print(n=Inf)

#conteos hapin 1.5
gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by) %>% filter(
  as.Date(s4_date)<"2021-03-19")

gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% select(id, m11_date,m11_by) %>% filter(
  as.Date(m11_date)>="2021-03-22"
)

#resumen de E3 clasificación 
library(ggplot2)
dt_e3<-gt_emory_data_arm2 %>%  filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  type=if_else(
    grepl("^35",id),
    "owa","pwg"
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% transmute(id, fecha_salida_madre=e3_date_exit, 
                                                               motivo_salida_madre=e3_reason, 
                                                               ultima_visita_madre=e3_last_visit) %>% 
    mutate(
      motivo_salida_madre=recode(motivo_salida_madre,"1"="Finalizacion estudio",
                                 "2"="No elegible",
                                 "3"="Retiro voluntario",
                                 "4"="Retirada por el equipo",
                                 "5"="Se mudo del area de estudio",
                                 "6"="Fallecio",
                                 "7"="Perdido durante el seguimiento",
                                 "8"="Madre: Aborto/ Aborto espontaneo, mortinato ,muerte del nino",
                                 "555"="Otro"),
      ultima_visita_madre=recode(
        ultima_visita_madre, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth", "5"="B1",
                              "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% transmute(id, fecha_salida_adulta=e3_date_exit_o, 
                                                               motivo_salida_adulta=e3_reason_o, 
                                                               ultima_visita_adulta=e3_last_visit_o) %>% 
    mutate(
      motivo_salida_adulta=recode(motivo_salida_adulta,"1"="Finalizacion estudio",
                                 "2"="No elegible",
                                 "3"="Retiro voluntario",
                                 "4"="Retirada por el equipo",
                                 "5"="Se mudo del area de estudio",
                                 "6"="Fallecio",
                                 "7"="Perdido durante el seguimiento",
                                                                  "555"="Otro"),
      ultima_visita_adulta=recode(
        ultima_visita_adulta, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth","5"="B1",
        "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% transmute(id, fecha_salida_nino=e3_date_exit_c, 
                                                                 motivo_salida_nino=e3_reason_c, 
                                                                 ultima_visita_nino=e3_last_visit_c) %>% 
    mutate(
      motivo_salida_nino=recode(motivo_salida_nino,"1"="Finalizacion estudio",
                                  "2"="No elegible",
                                  "3"="Retiro voluntario",
                                  "4"="Retirada por el equipo",
                                  "5"="Se mudo del area de estudio",
                                  "6"="Fallecio",
                                  "7"="Perdido durante el seguimiento",
                                  "555"="Otro"),
      ultima_visita_nino=recode(
        ultima_visita_nino, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth", "5"="B1",
        "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% mutate(
  s6_arm=recode(s6_arm,"1"="Intervencion", "0"="Control")
) %>% print()

dt_e3 %>% writexl::write_xlsx(paste0("output/revision_Salidas_",Sys.Date(),".xlsx"))

#salidas madre
dt_salidas_madre<-dt_e3 %>% transmute(id,Brazo=s6_arm, motivo_salida_madre) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_mother=ggplot(
      (dt_salidas_madre %>% mutate(`Motivo Salida`=if_else(is.na(motivo_salida_madre),
                                                               "Continua Participando", 
                                                               motivo_salida_madre))
       ), aes(`Motivo Salida`, fill=Brazo)) + geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Salidas Madre") +
      theme (plot.title = element_text(family="Comic Sans MS",
                                       size=rel(3), #Tamaño relativo de la letra del título
                                       vjust=1, #Justificación vertical, para separarlo del gráfico
                                       hjust=0.5,
                                       face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                       color="red", #Color del texto
                                       lineheight=1.5)) +
      labs(x="Motivo Salida segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1.5))) 


#salidas OWA
dt_salidas_adulta<-dt_e3 %>% transmute(id, Brazo=s6_arm, type, motivo_salida_adulta) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_adulta=ggplot(
  (dt_salidas_adulta %>% filter(type=="owa") %>% mutate(`Motivo Salida`=if_else(is.na(motivo_salida_adulta),
                                                       "Continua Participando", 
                                                       motivo_salida_adulta))
  ), aes(`Motivo Salida`, fill=Brazo)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Salidas OWA") +
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(3), #Tamaño relativo de la letra del título
                                   vjust=1, #Justificación vertical, para separarlo del gráfico
                                   hjust=0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) +
  labs(x="Motivo Salida segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="red", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="red", size=rel(1.5))) 



#salidas ONino
dt_salidas_nino<-dt_e3 %>% transmute(id, Brazo=s6_arm, type, c30_dob, motivo_salida_nino) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_nino=ggplot(
  (dt_salidas_nino %>% filter(!is.na(c30_dob)) %>% mutate(`Motivo Salida`=if_else(is.na(motivo_salida_nino),
                                                                                "Continua Participando", 
                                                                                motivo_salida_nino))
  ), aes(`Motivo Salida`, fill=Brazo)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Salidas Nino") +
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(3), #Tamaño relativo de la letra del título
                                   vjust=1, #Justificación vertical, para separarlo del gráfico
                                   hjust=0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) +
  labs(x="Motivo Salida segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="red", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="red", size=rel(1.5))) 




png(filename="output/graficas_e3.png",   # Nombre del archivo y extensión
     height = 11,
     width = 18,
     res= 200,       # Resolución
     units = "cm")  # Unidades.
grafic_mother
grafic_adulta
grafic_nino# grafic_adulta grafic_nino               # Gráfico
dev.off() 

ggsave("salidas_madre.png", plot = grafic_mother + grafic_adulta, path = "output/")



#GRAFICA ULTIMA VISITA
#Ultima visita madre
dt_visitas_madre<-dt_e3 %>% transmute(id,Brazo=s6_arm, ultima_visita_madre) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_visit_mother=ggplot(
  (dt_visitas_madre %>% mutate(`Ultima Visita`=case_when(
    ultima_visita_madre=="BL" ~ "    BL",
    ultima_visita_madre=="P1" ~ "   P1",
    ultima_visita_madre=="P2" ~ "  P2",
    ultima_visita_madre=="Birth" ~ " Birth",
    ultima_visita_madre=="B1" ~ "B1",
    ultima_visita_madre=="B2" ~ "B2",
    ultima_visita_madre=="B3" ~ "B3",
    ultima_visita_madre=="B4" ~ "B4",
   TRUE ~ "      Continua Participando"
    
  # )
  #                                if_else(is.na(ultima_visita_madre),
  #                                                      " Continua Participando", 
  #                                                      ultima_visita_madre)
    )
  )
  ), aes(`Ultima Visita`, fill=Brazo)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Ultima Visita Madre") +
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(3), #Tamaño relativo de la letra del título
                                   vjust=1, #Justificación vertical, para separarlo del gráfico
                                   hjust=0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) +
  labs(x="Ultima Visita segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1.5))) 


#salidas OWA
dt_visitas_adulta<-dt_e3 %>% transmute(id, Brazo=s6_arm, type, ultima_visita_adulta) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_visit_adulta=ggplot(
  (dt_visitas_adulta %>% filter(type=="owa") %>% mutate(
    `Ultima Visita`=case_when(
      ultima_visita_adulta=="BL" ~ "    BL",
      ultima_visita_adulta=="P1" ~ "   P1",
      ultima_visita_adulta=="P2" ~ "  P2",
      ultima_visita_adulta=="Birth" ~ " Birth",
      ultima_visita_adulta=="B1" ~ "B1",
      ultima_visita_adulta=="B2" ~ "B2",
      ultima_visita_adulta=="B3" ~ "B3",
      ultima_visita_adulta=="B4" ~ "B4",
      TRUE ~ "      Continua Participando"
    )                                                   )
  ), aes(`Ultima Visita`, fill=Brazo)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Ultima visita OWA") +
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(3), #Tamaño relativo de la letra del título
                                   vjust=1, #Justificación vertical, para separarlo del gráfico
                                   hjust=0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) +
  labs(x="Ultima visita segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="red", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="red", size=rel(1.5))) 



#salidas ONino
dt_salidas_nino<-dt_e3 %>% transmute(id, Brazo=s6_arm, type, c30_dob, motivo_salida_nino) 
#dt_salidas_madre<-dt_salidas_madre %>% group_by(motivo_salida_madre) %>% count() 

grafic_nino=ggplot(
  (dt_salidas_nino %>% filter(!is.na(c30_dob)) %>% mutate(`Motivo Salida`=if_else(is.na(motivo_salida_nino),
                                                                                  "Continua Participando", 
                                                                                  motivo_salida_nino))
  ), aes(`Motivo Salida`, fill=Brazo)) + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9)) + ggtitle("Salidas Nino") +
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(3), #Tamaño relativo de la letra del título
                                   vjust=1, #Justificación vertical, para separarlo del gráfico
                                   hjust=0.5,
                                   face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                   color="red", #Color del texto
                                   lineheight=1.5)) +
  labs(x="Motivo Salida segun E3", y="Cantidad participantes") +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="red", size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="red", size=rel(1.5))) 




png(filename="output/graficas_e3.png",   # Nombre del archivo y extensión
    height = 11,
    width = 18,
    res= 200,       # Resolución
    units = "cm")  # Unidades.
grafic_mother
grafic_adulta
grafic_nino# grafic_adulta grafic_nino               # Gráfico
dev.off() 


png(filename="output/salida_Adultas_e3.png",   # Nombre del archivo y extensión
    height = 11,
    width = 18,
    res= 200,       # Resolución
    units = "cm")  # Unidades.
grafic_adulta
#grafic_nino# grafic_adulta grafic_nino               # Gráfico
dev.off() 

ggsave("salidas_madre.png", plot = grafic_mother + grafic_adulta, path = "output/")

gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, c33_date, c33_by, c33_stadio,
                                                           c33_tape) %>% filter(
  c33_by=="LMA"
) %>% filter(c33_date=="2020-10-29")


dt_revision<-read_csv("c:/temp/id_revision.csv")
dt_revision<-dt_revision %>% mutate_all(as.character)
dt_revision %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, date=c30a_date, iniciales=c30a_by,c30a_stadiometer)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, date=c33_date, iniciales=c33_by, c33_tape)
) %>% arrange(date) %>% print(n=Inf) %>% writexl::write_xlsx("output/QC_c30a_periodo_covid.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, date=c33_date, 
                                                           iniciales=c33_by, c33_tape) %>% filter(
  date>="2020-03-10" & date<="2020-07-15"
) %>% arrange(date) %>% print(n=Inf) 


dt_revision %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, date=c30_date, iniciales=c30_by)
) %>% arrange(date) %>% writexl::write_xlsx("output/revision_c30.xlsx")


#REVISIÓN DE MUESTRAS SANGRE SECA EXPOSICION Y WALDEMAR
var1="2021-03-12"
listado_bebes %>% mutate(
  edad_calculada=(as.Date(var1) - as.Date(fecha_nacimiento))/30
) %>% select(id, fecha_nacimiento, edad_calculada) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_by) %>% filter(visit=="b4")
  #filter(as.Date(h41_date)<"2021-03-12") 
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(b10_date))  %>% select(id,b10_date, b10_by, b10_tm_spots, visit) %>% filter(visit=="b4") %>% 
    select(-visit)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_consent)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, fecha_Salida=e3_date_exit)
) %>% filter(edad_calculada<="15") %>% writexl::write_xlsx("output/reision_seroprevalencia.xlsx")


#consentimientos S4
gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>%  filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_consent, s4_consent_c,
                                                        s4_owa,s4_ocon_date) %>% mutate(
                    consintio_madre=recode(s4_consent, "1"="Si", "0"="No"),
                    consintio_nino=recode(s4_consent_c, "1"="Si", "0"="No"),
                    adulta_participando=recode(s4_owa, "1"="Si", "0"="No"),
                                                        ) %>% transmute(
            id_hogar=id, fecha_consentimiento=s4_date,
            iniciales=s4_by, consintio_madre, consintio_nino,adulta_participando,
            fecha_adulta_firmo=s4_ocon_date, type=if_else(grepl("^35", id_hogar),"hogar_35", "hogar_33")
                                                        )  %>% writexl::write_xlsx(
                                                          paste0("output/listado_consentimientos_hapin_1_5_al",
                                                                 Sys.Date(),".xlsx")
                                                        )

gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id,h57_date, h57_by, h57_stove) %>% group_by(h57_stove, h57_by) %>% count()

gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id,h57_date, h57_by, h57_stove) %>% filter(is.na(h57_stove))


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_by, s4_date,s4_owa,s4_ocon_version) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% 
  filter(type=="owa") %>% arrange(s4_ocon_version) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_date_exit_o, e3_reason_o,e3_last_visit_o)
  ) %>%  print(n=Inf) %>% writexl::write_xlsx("output/s4_adultas_con_e3.xlsx")


gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_date, h57_by,
                                                         h57_stove) %>% arrange(h57_stove) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm)
  ) %>% filter(is.na(h57_stove)) %>% print(n=Inf)

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_by, s4_date,s4_owa,s4_ocon_version) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% 
  filter(type=="owa") %>% left_join(
    dt_e3 %>% select(id, motivo_salida_adulta)
  ) %>% mutate(
    participa_adulta=recode(s4_owa,"0"="No", "1"="Si")
  ) %>% select(id, fecha_s4=s4_date, iniciales=s4_by, participa_adulta, version_protocolo_adulta=s4_ocon_version,
               tipo_hogar=type, motivo_salida_hapin1=motivo_salida_adulta) %>% writexl::write_xlsx(
                 paste0("output/lista_consentimientos_adultas_hapinII_al_",Sys.Date(),".xlsx")
               )






gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_c_urine, b10_tc_spots) %>% mutate(
  b10_c_urine=recode(b10_c_urine, "1"="Si", "0"="No"),
  b10_tc_spots=recode(b10_tc_spots, "1"="Si", "0"="No"),
) %>% 
  writexl::write_xlsx(paste0("output/conteo_muestras_b5.xlsx"))


gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_envir___3,h41_person___3) %>% 
 group_by(
   h41_envir___3,
   h41_person___3
 ) %>% count()

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_envir___3,h41_person___3
                                                         ,h41_c_beacon_id1,
                                                         h41_c_beacon_id2,
                                                         h41_c_lascar_id,
                                                         h41_c_ecm_id,) %>% 
  writexl::write_xlsx("output/revision_ecm_beacons_04022022.xlsx")


#revision ecm
gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, redcap_event_name,  h41_date, h41_by,
                                                                                          matches("^h41_kap1_ecm_id"),
                                                                                          matches("^h41_kap1_ecm_f"),
                                                                                          matches("^h41_kap2_ecm_id"),
                                                                                          matches("^h41_kap2_ecm_f"),
                                                                                          matches("^h41_sap_ecm_id"),
                                                                                          matches("^h41_sap_ecm_f"),
                                                                                          matches("^h41_m_ecm_id"),
                                                                                          matches("^h41_m_ecm_f"),
                                                                                          matches("^h41_c_ecm_id"),
                                                                                          matches("^h41_c_ecm_f"),
                                                                                          matches("^h41_b_ecm_id"),
                                                                                          matches("^h41_b_ecm_f"),
                                                                                          matches("^h41_o_ecm_id"),
                                                                                          matches("^h41_o_ecm_f")
                                                                                          
) %>% filter(
  h41_date=='2021-04-28'
) %>% writexl::write_xlsx("output/revision_ecm_2021-04-28.xlsx")
                                                         
                                                         

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_kap1_log_id )

gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,e2_type  ) %>% filter(
  e2_type =="6"
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, madre=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`)
) %>% 
  writexl::write_xlsx(paste0("output/lista_e2_anomalia_congenita.xlsx"))


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date)
) %>% filter(!is.na(c33_date)) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date)
) %>% left_join(
  listado_bebes %>% select(id, fecha_nacimiento, fecha_ideal_visita=`24_meses`)
) %>% mutate(
  periodo_clinica=as.Date(c33_date) - as.Date(fecha_ideal_visita),
  periodo_exposicion=as.Date(h41_date) - as.Date(fecha_ideal_visita)
) %>% select(
  id, fecha_consentimiento=s4_date, fecha_ideal_visita, 
  fecha_clinica=c33_date, fecha_exposicion=h41_date,
  periodo_clinica, periodo_exposicion
) %>% mutate(
  rango_clinica=case_when(
    
  )
)



# gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, visit, 
#                                                              b10_tc_spots,b10_tc_spots_num)
# ) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, al_nacer=b10a_tc_spots_num)
# ) %>% muate(
#   flag1=case_when(
#     al_nacer>="2" ~ "1",
# al_nacer<- gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(   
#  gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, al_nacer=b10a_tc_spots_num)
# ) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, visit, 
#                                                              b10_tc_spots,b10_tc_spots_num) %>% 
#     filter(visit=="b4")
# ) %>% select(id, al_nacer, b4=b10_tc_spots_num) %>% group_by(al_nacer) %>% count()
# 
# b4<- gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(   
#   gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, al_nacer=b10a_tc_spots_num)
# ) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, visit, 
#                                                              b10_tc_spots,b10_tc_spots_num) %>% 
#     filter(visit=="b4")
# ) %>% select(id, al_nacer, b4=b10_tc_spots_num) %>% group_by(b4) %>% count()
# 
# list(al_nacer, b4) %>% writexl::write_xlsx("output/test.xlsx")

  #filter(!is.na(	b10_tc_spots)) %>% writexl::write_xlsx("output/gotas_validas.xlsx")


#Lista de muestras estratificado por participante y visita, posterior al 20 de marzo 2020 incluye hapin 1.5
#madre linea basal
lb_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg"))left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="baseline") %>%  select(id, b10_date, 
                               bl_madre=b10_tm_spots_num)
) 
#adulta linea basal
lb_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
  gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
    filter(visit=="baseline") %>%  select(id, b10_date, 
                                          bl_owa=b10_to_spots_num)
) %>% print()  

#madre p1
p1_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p1") %>%  select(id, b10_date, 
                                            bl_madre=b10_tm_spots_num)
  ) %>% print()

#adulta P1
p1_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p1") %>%  select(id, b10_date, 
                                            bl_owa=b10_to_spots_num)
  ) %>% print() 


#madre p2
p2_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p2") %>%  select(id, b10_date, 
                                      bl_madre=b10_tm_spots_num)
  ) %>% print()


gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date,b10_tc_urine,
                                                         b10_tc_spots) %>% group_by(
                                                           b10_tc_spots
                                                         ) %>% count()
#145 dbs
gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date,b10_tc_urine,
                                                         b10_tc_spots) %>% group_by(
                                                           b10_tc_urine
                                                         ) %>% count()



gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(
  id, matches("s4_")
) %>% mutate(
  type=if_else(
    grepl(
      "^35", id
    ), "owa", "pwg"
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, fecha_antro=c33_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(a24a_date)) %>% select(id, antro_adulta=a24a_date)
) %>% writexl::write_xlsx("output/revision_s4.xlsx")


gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id=s4_main_id) %>% mutate(
  type=if_else(
    grepl("^35",id),"owa","pwg"
  )
  )%>% filter(type=="owa")

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id,h41_c_beacon_id1,
                                                               h41_c_ecm_id)
gt_hapin_II_data %>%  filter(!is.na(h41_c_beacon_id1) | 
                                     !is.na(h41_c_beacon_id2)
                            | !is.na(h41_c_ecm_id)) %>% select(id,h41_c_beacon_id1,
                                                               h41_c_ecm_id) %>% writexl::write_xlsx(
                                                                 "output/revision_medidas_directas,xlsx"
                                                               )
#ecm medida directa
#BEacon medida indirecta
                            
data_piloto<-read_csv("c:/temp/PilotoVigilanciaNeum_DATA_2021-06-18_1643.csv")

data_piloto %>% select(record_id, today) %>% arrange(desc(today))

gt_emory_data_arm2 %>% filter(!is.na(c30_wt_where)) %>% 
  select(id, c30_date, c30_by, c30_wt_where, c30_wt_where_other) %>% filter(!is.na(c30_wt_where_other)) %>% 
  writexl::write_xlsx("output/revision_c30_igss.xlsx")

#Migración datos hapin II uvg hacia hapin II emory m14b
dt_hapinII_gt<-read_csv("c:/temp/HAPINIIGT_DATA_2021-07-05_1137.csv")
dt_hapinII_gt<-dt_hapinII_gt %>% mutate(id=as.character(record_id)) %>% select(id, everything())

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, m14b_date) %>% filter(
  is.na(m14b_date)
) %>% write_csv("output/migración_m14b_24-06-2021.csv")

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, m14b_date, m14_bp_num,m14b_by) %>% filter(
  is.na(m14b_date)
)

gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m14a_date)

dt_hapinII_gt %>% filter(!is.na(m14a_date)) %>% select(id, m14a_by) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m14a_date)
) %>% filter(is.na(m14a_date)) %>% writexl::write_xlsx("output/migracion_m14a_24-06-2021.xlsx")

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% mutate(
  type=grepl("^35",id),"owa","pwg"
) %>% writexl::write_xlsx("output/revision_ids.xlsx")

dt_hapinII_gt %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by)

gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date)
gt_emory_data_arm2 %>% filter(!is.na(m11_date)) %>% select(id, m11_date, visit) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date)
) %>% arrange( v1_date, m11_date) %>% filter(!is.na(v1_date))

gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date,v1_hh_num) %>% 
left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date) & visit=="baseline") %>% select(
    id, m10_sleep
  )
) %>% select(id, v1_date, personas_v1=v1_hh_num, personas_m10=m10_sleep) %>%  mutate(
  diferencia=case_when(
    as.numeric(personas_v1)== as.numeric(personas_m10) ~ "The same",
    as.numeric(personas_v1)> as.numeric(personas_m10) ~ "More",
    as.numeric(personas_v1) < as.numeric(personas_m10) ~ "Fewer",
    TRUE ~ NA_character_
  )
)  %>% writexl::write_xlsx(
  "output/revision_v1_personas.xlsx"
)

gt_emory_data_arm2 %>% filter(!is.na(m11_date)) %>% select(id, m11_date,visit) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date)
  ) %>% filter(!is.na(v1_date)) %>% 
  group_by(id) %>% mutate(
    diferencia= case_when(
      as.Date(m11_date)<as.Date(v1_date) ~ as.Date(v1_date) - as.Date(m11_date),
      as.Date(m11_date) > as.Date(v1_date) ~ as.Date(m11_date) - as.Date(v1_date),
      as.Date(m11_date) == as.Date(v1_date) ~ as.Date(m11_date) - as.Date(v1_date)
    )
  ) %>% group_by(id) %>% summarize(minimo=min(diferencia)) %>% ungroup() %>%  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(m11_date)) %>% select(id, m11_date,visit) %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date)
    ) %>% filter(!is.na(v1_date)) %>% 
      group_by(id) %>% mutate(
        diferencia= case_when(
          as.Date(m11_date)<as.Date(v1_date) ~ as.Date(v1_date) - as.Date(m11_date),
          as.Date(m11_date) > as.Date(v1_date) ~ as.Date(m11_date) - as.Date(v1_date),
          as.Date(m11_date) == as.Date(v1_date) ~ as.Date(m11_date) - as.Date(v1_date)
        )
      ) %>% select(id, visit, minimo=diferencia)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(m11_date)) %>% select(id, m11_date,visit)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id,v1_date)
  ) %>% arrange(id) %>% writexl::write_xlsx("output/diferencia_m11_v1_cercano.xlsx")

dt_hapinII_gt<-read_csv("c:/temp/HAPINIIGT_DATA_2021-07-12_1317.csv")

dt_hapinII_gt %>% filter(!is.na(m14a_date)) %>% 
  transmute(id=as.character(record_id), m14a_date) %>% anti_join(
    gt_hapin_II_data %>% filter(!is.na(m14a_date)) %>% select(id)
  ) %>% writexl::write_xlsx("output/m14a_migracion_pendiente.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, c33_date, 
                                                           c33_wt1,c33_wt2, 
                                                           c33_wt3, c33_ave_wt, visit) %>% 
  filter(id=="33302") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_ave_wt )
  )

gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, c33_by, c33_date, visit,
                                     c33_ht1, c33_ht2, c33_ht3, c33_ave_ht) %>% 
  filter(id=="33590") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_ave_ht )
  )

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_by, c30_wt_record,
                                                           c30_wt_unit) %>% filter(
    id=="33006" | id=="33346" | id=="35046" | id=="33402" )



m11_v1<-read_csv("c:/temp/m11_v11.csv")
m11_v1 %>% mutate(id=as.character(id)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% 
    select(id, fecha_salida_madre=e3_date_exit)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, fecha_salida_nino=e3_date_c)
) %>% writexl::write_xlsx("output/Diferencia_m11_v1_cercano.xlsx")


#revision de pesos
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% transmute(
  id, peso_registrado=c30_wt_record, peso_hapin=c30_wt1,
  unidad=recode(c30_wt_unit, "1"="Kilogramos.gramos","2"="Libras.onzas")
) %>% arrange(unidad) %>% mutate(
  enteros=floor(as.numeric(peso_registrado)),
  decimales= as.numeric(peso_registrado) - floor(enteros)
) %>%  writexl::write_xlsx("output/revision_pesos_decimales.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, fecha_entrega_estufa=h50_date) %>% 
  left_join(
    datos_participantes %>% select(`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`)
  ) %>% writexl::write_xlsx("output/listado_participantes_intervencion.xlsx")

gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_wt1_time) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm)
) %>% group_by(s6_arm) %>% count()

listado_bebes %>% filter(as.Date(`24_meses`)>"2021-08-30") %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date)
) %>% filter(is.na(b10_date)) %>% select(id) %>% 
  mutate(
    DBS=paste0(id,"-C7-X0"),
    U0=paste0(id,"-C7-U0"),
    U1=paste0(id,"-C7-U1"),
    U2=paste0(id,"-C7-U2"),
    U3=paste0(id,"-C7-U3"),
    U4=paste0(id,"-C7-U4")
  ) %>% gather(key="variable", value="value", -id) %>% arrange(id) %>% select(
    HHI=id, type=variable, id_label=value
  ) %>% writexl::write_xlsx("output/label_b5_gt.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, fecha_randomizacion=s6_date) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`)
) %>% writexl::write_xlsx("output/listado_randomizadas.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                           c30_date, visit, c30_head1, c30_head2,
                                                           c30_ave_ht
                                                           ) %>% 
  filter(id=="35118") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, c33_head1, c33_head2,
                                                               c33_ave_ht, visita=visit)
  )


dt_supervision<-read_csv("c:/temp/HAPINGuatemalaSuperv_DATA_2021-08-16_1117.csv")
dt_supervision %>% mutate(id=as.character(record_id)) %>% 
  filter(redcap_event_name=="complemento_de_con_arm_1") %>% select(id, gt_s4_consent_date,gt_s4_consent_signed) %>% 
arrange(desc(gt_s4_consent_date)) %>% group_by(gt_s4_consent_signed) %>% count()


data_repeated<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2021-08-16_1307.csv")
data_repeated %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(id_h40=record_id, c40_date, id_visita=c40_visit) %>% 
  filter(
  !is.na(c40_date)
) %>% filter(!is.na(id_visita)) %>% left_join(
  data_repeated %>% filter(redcap_event_name=="surveillance_arm_4") %>% 
    select(id_c36a=record_id, c36a_hhid, c36a_date, id_visita=c36a_visit) %>% filter(
      !is.na(c36a_hhid)
    )
) 


c40<-read_csv("c:/temp/neumonia/c40.csv")
c40<-c40 %>% filter(!is.na(c40_visit))

c37<-read_csv("c:/temp/neumonia/c37.csv")
c37<-c37 %>% filter(!is.na(c37_visit))

c36<-read_csv("c:/temp/neumonia/c36.csv")
c36<-c36 %>% filter(!is.na(c36a_visit))

c34<-read_csv("c:/temp/neumonia/c34.csv")
c34<-c34 %>% filter(!is.na(c34a_visit))

c41<-read_csv("c:/temp/neumonia/c41.csv")
c41<-c41 %>% filter(!is.na(c41_visit))

c40 %>% select(id_c40=record_id, c40_hhid, c40_date, id_visita=c40_visit) %>% 
  left_join(
    c36 %>% select(id_c36=record_id, c36a_hhid, c36a_date, id_visita=c36a_visit)
  ) %>% 
  left_join(
    c37 %>% select(id_c37=record_id, c37a_hhid, c37_date, id_visita=c37_visit)
  ) %>% left_join(
    c34 %>% select(id_c34=record_id, c34a_hhid, c34a_date, id_visita=c34a_visit)
  ) %>% left_join(
    c41 %>% select(id_c41=record_id, c41_hhid, c41_date, id_visita=c41_visit)
  ) %>% writexl::write_xlsx("output/revision_c40_neumonia.xlsx")



c40 %>% transmute(id_c40=record_id, id=as.character(c40_hhid), c40_date, id_visita=c40_visit) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% 
    select(id, c31_date, c31_by, visit) 
) %>% arrange(id, c40_date, c31_date) %>% mutate(
  diferencia= as.Date(c40_date) - as.Date(c31_date)
) %>% arrange(desc(diferencia)) %>% filter(diferencia=="0") %>% writexl::write_xlsx(
  "output/revision_c40_c31.xlsx"
)

gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, acepto_en=visit, s8_consent) %>% 
  filter(s8_consent=="1") %>% select(id, fecha_consentimiento_entericas=s8_date, acepto_en) %>% 
  left_join(
    listado_bebes %>% select(id, `24_meses`,  `36_meses`)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, fecha_visita_24m=c33_date)
  ) %>% mutate(visita_24m_realizada=if_else(is.na(fecha_visita_24m),"No","Si")) %>% transmute(
    id, fecha_consentimiento_entericas, acepto_en, fecha_24_meses=`24_meses`,
    fecha_36_meses=`36_meses`, visita_24m_realizada
  ) %>% writexl::write_xlsx("output/listado_entericas.xlsx")


dt_hapin_exposure<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2021-08-19_0951.csv")

dt_hapin_exposure %>% mutate(id=as.character(record_id)) %>% select(id, everything()) %>% mutate(
  visit=substr(redcap_event_name,1,2)
) %>% select(id, visit, everything()) %>% filter(visit=="p1" | visit=="p2" | visit=="b1" | visit=="b2" 
                                                 | visit=="b4") %>% mutate(
                                                  crf= gth4x_crf
                                                 ) %>% 
  select(id, visit, crf, latitud=gthx_lat, longitud= gth4x_long, altura=gthx_elevation) %>% mutate(
    longitud=paste0("-",as.character(longitud))
    ) %>% arrange(id) %>% writexl::write_xlsx("output/tabla_coordenadas.xlsx")


dt_hercules<-read_csv("c:/temp/Hercules_DATA_2021-08-19_1508.csv")

dt_hercules<-dt_hercules %>% mutate(id=as.character(record_id)) %>% select(id, everything()) %>% mutate(brazo=case_when(
  redcap_event_name=="6_meses_arm_3" ~ "6_meses",
  redcap_event_name=="b12_biomuestras_arm_2"~ "12_meses",
  redcap_event_name=="consentimiento_arm_1" ~ "consentimientos"
)) %>% select(id, brazo, everything())

dt_hercules %>% filter(!is.na(h_date)) %>% select(id, brazo, matches("^h_")) %>% mutate(
  h_consent_accept=recode(h_consent_accept,"1"="Si","0"="No" ),
  h_consent_accept_desc= recode(h_consent_accept_desc,"1"="No está interesada",
                                "2"="El esposo no aceptó", "3"="No quieren más muestras",
                               "4"="Otro"
  )
) %>% left_join(
  dt_hercules %>% filter(brazo=="6_meses") %>% select(
    brazo_seguimiento=brazo, h_id_hercules=record_id_hercules, date, iniciales_6=iniciales,
    visita_6=visita, amamantando_6=amamantando, mastitis_6=mastitis,b2_antibiotico_7dias_6=b2_antibiotico_7dias,
    b2_antibiotico_3meses_6=b2_antibiotico_3meses,b2_alimentos_24horas_6=b2_alimentos_24horas,
    b2_medicamento_6=b2_medicamento,b2_hospitalizado_6=b2_hospitalizado,b2_hospitalizado_cuando_6=b2_hospitalizado_cuando,
    b2_hospitalizado_motivo_6=b2_hospitalizado_motivo,amamanto_time_6=amamanto_time,alimentos_asados_6=alimentos_asados,
    fuente_agua_tomar___1_6=fuente_agua_tomar___1,fuente_agua_tomar___2_6=fuente_agua_tomar___2,
    fuente_agua_tomar___3_6=fuente_agua_tomar___3,fuente_agua_tomar___4_6=fuente_agua_tomar___4,
    fuente_agua_tomar___5_6=fuente_agua_tomar___5,fuente_agua_tomar___6_6=fuente_agua_tomar___6,
    fuente_agua_tomar___7_6=fuente_agua_tomar___7,fuente_agua_tomar___8_6=fuente_agua_tomar___8,
    fuente_agua_tomar___9_6=fuente_agua_tomar___9,fuente_agua_tomar___10_6=fuente_agua_tomar___10,
    fuente_agua_tomar___11_6=fuente_agua_tomar___11,fuente_agua_tomar___12_6=fuente_agua_tomar___12,
    fuente_agua_tomar___13_6=fuente_agua_tomar___13,fuente_agua_tomar___14_6=fuente_agua_tomar___14,
    fuente_agua_tomar___15_6=fuente_agua_tomar___15,fuente_agua_tomar___16_6=fuente_agua_tomar___16,
    fuente_agua_tomar___555_6=fuente_agua_tomar___555,
    fuente_agua_cocinar___1_6=fuente_agua_cocinar___1,fuente_agua_cocinar___2_6=fuente_agua_cocinar___2,
    fuente_agua_cocinar___3_6=fuente_agua_cocinar___3,fuente_agua_cocinar___4_6=fuente_agua_cocinar___4,
    fuente_agua_cocinar___5_6=fuente_agua_cocinar___5,fuente_agua_cocinar___6_6=fuente_agua_cocinar___6,
    fuente_agua_cocinar___7_6=fuente_agua_cocinar___7,fuente_agua_cocinar___8_6=fuente_agua_cocinar___8,
    fuente_agua_cocinar___9_6=fuente_agua_cocinar___9,fuente_agua_cocinar___10_6=fuente_agua_cocinar___10,
    fuente_agua_cocinar___11_6=fuente_agua_cocinar___11,fuente_agua_cocinar___12_6=fuente_agua_cocinar___12,
    fuente_agua_cocinar___13_6=fuente_agua_cocinar___13,fuente_agua_cocinar___14_6=fuente_agua_cocinar___14,
    fuente_agua_cocinar___15_6=fuente_agua_cocinar___15,fuente_agua_cocinar___16_6=fuente_agua_cocinar___16,
    fuente_agua_cocinar___555_6=fuente_agua_cocinar___555,cantidad_tazas_agua_6=cantidad_tazas_agua,
    cantidad_bebidas_enlatadas_6=cantidad_bebidas_enlatadas,uso_pesticidas_6=uso_pesticidas,
    pesticidas_time_6=pesticidas_time,uso_cosmeticos_6=uso_cosmeticos,uso_cosmeticos_frecuencia_6=uso_cosmeticos_frecuencia,
    uso_productos_cabello_6=uso_productos_cabello, uso_product_cabello_freq_6=uso_product_cabello_freq,
    uso_anticonceptivo_6=uso_anticonceptivo,toma_medicamento_6=toma_medicamento,
    lista_medicmaentos___1_6=lista_medicmaentos___1,lista_medicmaentos___2_6=lista_medicmaentos___2,
    lista_medicmaentos___3_6=lista_medicmaentos___3,lista_medicmaentos___4_6=lista_medicmaentos___4,
    lista_medicmaentos___5_6=lista_medicmaentos___5,lista_medicmaentos___555_6=lista_medicmaentos___555,
    toma_medicamentos_freq_6=toma_medicamentos_freq,uso_repelente_6=uso_repelente,uso_repelente_freq_6=uso_repelente_freq,
    uso_unguento_6=uso_unguento,uso_unguento_freq_6=uso_unguento_freq,uso_protector_6=uso_protector,
    uso_protector_freq_6=uso_protector_freq,uso_desodorante_6=uso_desodorante,uso_desodorante_freq_6=uso_desodorante_freq,
    uso_polvos_6=uso_polvos,uso_polvos_freq_6=uso_polvos_freq,muestra_lechematerna_6=muestra_lechematerna,
    time_muestra_leche_6=time_muestra_leche,id_leche_materna_6=id_leche_materna,no_muestra_leche_6=no_muestra_leche,
    no_razon_otro_6=no_razon_otro,muestra_oral_m_6=muestra_oral_m,time_muestra_oral_6=time_muestra_oral,
    id_muestra_oral_m_6=id_muestra_oral_m,muestra_nasal_m_6=muestra_nasal_m,time_muestra_nasal_m_6=time_muestra_nasal_m,
    id_muestra_nasal_m_6=id_muestra_nasal_m, muestra_oral_n_6=muestra_oral_n,time_muestra_oral_n_6=time_muestra_oral_n,
    id_muestra_oral_n_6=id_muestra_oral_n, id_muestra_oral_n_6=id_muestra_oral_n, muestra_nasa_n_6=muestra_nasa_n,
    time_muestra_nasal_n_6=time_muestra_nasal_n,id_muestra_nasal_n_6=id_muestra_nasal_n,m_control_6=m_control,
    no_m_control_desc_6=no_m_control_desc,m_dup_nasal_6=m_dup_nasal,time_dup_nasal_6=time_dup_nasal,
    id_dup_nasal_6=id_dup_nasal
  )
) %>% left_join(
  dt_hercules %>% filter(brazo=="12_meses") %>% select(
    brazo_seguimiento=brazo, h_id_hercules=record_id_hercules, date_12=date, iniciales_b12=iniciales,
    visita_b12=visita, hijo_enfermo_b12=hijo_enfermo, hijo_antibioticos_b12=hijo_antibioticos,consentimiento_b12=consentimiento,
    parto_cesarea_b12=parto_cesarea,antibiotico_7dias_b12=antibiotico_7dias, nombre_antibiotic_7dias_b12=nombre_antibiotic_7dias,
    antibiotico_3meses_b12=antibiotico_3meses, nombre_antibiotic_3meses_b12=nombre_antibiotic_3meses,
    muestra_oral_m_b12=muestra_oral_m, time_muestra_oral_b12=time_muestra_oral, id_muestra_oral_m_b12=id_muestra_oral_m,
    muestra_nasal_m_b12=muestra_nasal_m, time_muestra_nasal_m_b12=time_muestra_nasal_m, id_muestra_nasal_m_b12=id_muestra_nasal_m,
    muestra_oral_n_b12=muestra_oral_n, time_muestra_oral_n_b12=time_muestra_oral_n, id_muestra_oral_n_b12=id_muestra_oral_n,
    muestra_nasa_n_b12=muestra_nasa_n, time_muestra_oral_n_b12=time_muestra_oral_n, id_muestra_nasal_n_b12=id_muestra_nasal_n,
    m_control_b12=m_control, no_m_control_desc_b12=no_m_control_desc, m_dup_nasal_b12=m_dup_nasal, time_dup_nasal_b12=time_dup_nasal,
    id_dup_nasal_b12=id_dup_nasal, no_dup_nasal_desc_b12=no_dup_nasal_desc, dup_nasal_coment_b12=dup_nasal_coment
  )  
  
) %>% write_csv("output/dt_hercules_gt.csv")

datos_envios_lab<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2021-09-08_1059.csv")

datos_envios_lab %>% filter(!is.na(shipment_id)) %>% select(
  shipment_id, shipment_date, shipment_by, shipment_quantity
) %>% arrange(desc(shipment_id)) %>% group_by(shipment_id) %>% count() %>% filter(n>1)
  writexl::write_xlsx("output/envios_lab_revision.xlsx")

  gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_date
                                                          ) %>% anti_join(
                                                            gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id)                                                            
                                                          )
  

  
#revision datos de piloto neumonía
  piloto_data<-read_csv("c:/temp/piloto_data.csv")
  esfuerzo_data<-read_csv("c:/temp/ESFUERZOHAPIN_DATA_2021-09-13_1133.csv")
  
  piloto_data %>% filter(!is.na(today)) %>% select(record_id, today) %>% arrange(desc(today))
  
  esfuerzo_data %>% select(record_id, time_start, time_end) %>% 
    arrange(time_start) %>% mutate(
      hora_inicio=( paste0(lubridate::hour(time_start),":",lubridate::minute(time_start) )),
      fecha_inicio=lubridate::date(time_start)
      ) %>% select(record_id, fecha_inicio, hora_inicio, hora_fin=time_end) %>% writexl::write_xlsx("output/revision_esfuerzo.xlsx")
  
  
  vigilancia_neumonia<-read_csv("c:/temp/HAPINVigilanciaGuate_DATA_2021-09-13_1203.csv")
  
  
  #revision de c31 missing
  
  gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, visit,c33_date) %>% left_join(
  gt_emory_data_arm2 %>% filter(is.na(c31_date) ) %>% transmute(id, visit, c31_pendiente="Si") %>% filter(
    visit=="b1" | visit=="b2" | visit=="b3" | visit=="b4" ) 
  ) %>% filter(c31_pendiente=="Si") %>% select(id, fecha_missing=c33_date, visita_perdida=visit) %>%  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, visit)
  ) %>% group_by(id) %>% mutate(
    diferencia = as.Date(fecha_missing) - as.Date(c31_date)
  ) %>% mutate(
    diferencia= as.character(diferencia),
    diff_positivos= if_else(substr(diferencia,1,1)=="-",substr(diferencia,2,length(diferencia)), diferencia),
    distancia_visitas=as.numeric(diff_positivos)
  ) %>% group_by(id) %>% summarize(minimo=min(distancia_visitas)) %>% select(id, distancia_visitas=minimo) %>%  left_join(
    
    gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, visit,c33_date) %>% left_join(
      gt_emory_data_arm2 %>% filter(is.na(c31_date) ) %>% transmute(id, visit, c31_pendiente="Si") %>% filter(
        visit=="b1" | visit=="b2" | visit=="b3" | visit=="b4" ) 
    ) %>% filter(c31_pendiente=="Si") %>% select(id, fecha_missing=c33_date, visita_perdida=visit) %>%  left_join(
      gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, visit)
    ) %>% group_by(id) %>% mutate(
      diferencia = as.Date(fecha_missing) - as.Date(c31_date)
    ) %>% mutate(
      diferencia= as.character(diferencia),
      diff_positivos= if_else(substr(diferencia,1,1)=="-",substr(diferencia,2,length(diferencia)), diferencia),
      distancia_visitas=as.numeric(diff_positivos)
    )
    
  ) %>%  writexl::write_xlsx("output/revision_c31_missing_mas_cercanos.xlsx")

  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, visit) %>% left_join(
    
    gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, visit,c33_date) %>% left_join(
      gt_emory_data_arm2 %>% filter(is.na(c31_date) ) %>% transmute(id, visit, c31_pendiente="Si") %>% filter(
        visit=="b1" | visit=="b2" | visit=="b3" | visit=="b4" ) 
    ) %>% filter(c31_pendiente=="Si") %>% select(id, fecha_missing=c33_date)
    
  ) %>% filter(id!="33001") %>% group_by(id) %>% mutate(
    diferencia= case_when(
      as.Date(fecha_missing)<as.Date(c33_date) ~ as.Date(fecha_missing) - as.Date(c33_date),
      as.Date(fecha_missing) > as.Date(c33_date) ~ as.Date(fecha_missing) - as.Date(c33_date),
      as.Date(fecha_missing) == as.Date(c33_date) ~ as.Date(fecha_missing) - as.Date(c33_date),
      TRUE ~ NA_integer_
    )
  ) %>% group_by(id) %>% summarize(minimo=min(diferencia))
  
  
  
 #datos solicitados por Anaite para Keyla
  #datos exportados Main Study
  data<-read_csv("c:/temp/HAPINGuatemalaMainSt_DATA_2021-09-16_1148.csv")
  dt_data<-data %>% mutate(HHID=as.character(id),
                  visit=substr(redcap_event_name,1, 2)) %>% select(HHID,visit, -id, everything()) 
  
  #Datos de 24 meses
  datos_24<- gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(HHID=id,matches("^c33_")) %>% mutate(
    visita="24_meses"
  ) %>% select(HHID,visita, everything())
  
  datos_24<-datos_24 %>%
    setNames(c(names(.)[1], paste0("b5_",names(.)[-1]))) 

  dt_data %>% filter(!is.na(c32_date)) %>% select(HHID, visit, matches("^c32_")) %>% filter(visit=="b2") %>% left_join(
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m10_date)) %>%  select(HHID=id, m10_date, matches("^m10_"))
  ) %>% left_join(
    dt_data %>% filter(
      !is.na(c31_date)
    ) %>% filter(visit=="b2") %>%  select(HHID,matches("^c31_"))
    
  ) %>% left_join(
   datos_24
  ) %>% write_csv("output/dt_c33b2_m10_c31b2_c33b5.csv")
  
dt_repeated<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2021-09-23_1603.csv")
  
dt_repeated %>% group_by(redcap_event_name) %>% count()
dt_repeated %>% filter(redcap_event_name=="surveillance_arm_4") %>%  select(record_id, c40_date, 
                                                    c40_hhid, c40_visit, c40_location, c40_location_other) %>% 
  filter(c40_location=="555") %>% print(n=Inf)

#Revisión entericas
gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>% select(id, matches("^e7_"))

dt_entericas<-read_csv("c:/temp/HAPINentericas_DATA_2021-09-29_1513.csv")
dt_lab<- read_csv("c:/temp/HAPINGuatemalaLab_DATA_2021-09-28_1415.csv")

dt_entericas<-dt_entericas %>% mutate(id=as.character(c39_id)) %>% select(id, everything())

muestras_entericas<-dt_entericas %>% select(id, redcap_event_name, c39_date, c39_id_sample) %>% 
  filter(!is.na(c39_id_sample)) %>% mutate(id_sample=gsub("--", "-", c39_id_sample),
                                           visita=substr(redcap_event_name, 1, 2)) %>% 
  select(id, visita, id_sample, fecha_enterica=c39_date) %>% writexl::write_xlsx("output/listado_muestras_entericas.xlsx")

#revisión de E7
gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>% select(id, e7_date, e7_pv_date, e7_start_date, 
                                                          e7_stop_date,e7_type___7, e7_describ,e7_correct,
                                                          e7_prevent) %>%
filter(e7_type___7=="1") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(id, h55_date, visit, h55_move___1, h55_move___2,
                                                             h55_where, h55_area, h55_type, h55_o_type,
                                                             h55_o_area,h55_move_both, h55_o_move_date,
                                                             h55_o_return_date) %>% 
    filter(h55_move___1=="1" | h55_move___2=="1")
) %>%
  writexl::write_xlsx("output/revision_e7_h55.xlsx")

  gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(id, h55_date, visit, h55_move___1, h55_move___2,
                                                             h55_where, h55_area, h55_type, h55_o_type,
                                                             h55_o_area,h55_move_both, h55_o_move_date,
                                                             h55_o_return_date) %>% 
    filter(h55_move___1=="1" | h55_move___2=="1")

  
gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, matches("^h57_")) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm)
) %>% mutate(
  uso_estufa=case_when(
    h57_stove=="0" ~ "No",
    h57_stove=="1" ~ "Si, Estufa biomasa",
    h57_stove=="2" ~ "Si, estufa electrica",
    h57_stove=="3" ~ "Si estufa de gas y cilindro de gas",
  ),
  estufa_hapin=case_when(
    h57_hapin_stove=="0" ~ "No",
    h57_hapin_stove=="1" ~ "Si, todavía en uso",
    h57_hapin_stove=="2" ~ "Si, no está en uso (guardada)",
    h57_hapin_stove=="3" ~ "Si, segun autoreporte (no fue posible observar la estufa)",
    h57_hapin_stove=="88" ~ "N/A",
  ),
  brazo=if_else(s6_arm=="0", "control","intervencion")
) %>% select(id, brazo, h57_gas_cooking, uso_estufa, estufa_hapin) %>% filter(brazo=="intervencion") %>% 
  filter(uso_estufa=="No")

#plasticos integrado datos de adolescentes
dt_plastic<-read_csv("c:/temp/plasticos/PlasticsSubstudy_DATA_2021-10-13_0825.csv")

dt_plastic %>% group_by(redcap_event_name) %>% count()

dt_plastic %>% filter(!is.na(b12_date)) %>% select(id=record_id, matches("^b12_")) %>% left_join(
  

dt_plastic %>% filter(!is.na(el_dia)) %>% select(id=record_id,
  v2_orina_quien_completo_formula=quien_completo_formula,v2_orina_plasticos_identificacion=plasticos_identificacion,
  v2_orina_el_dia=el_dia,v2_orina_platos_plastico=platos_plastico,v2_orina_vasos_plasticos=vasos_plasticos,
  v2_orina_utensilios_plasticos=utensilios_plasticos,v2_orina_agua_6oz=agua_6oz,
  v2_orina_bolsas_8oz=bolsas_8oz,v2_orina_bebidas_enlatadas=bebidas_enlatadas,
  v2_orina_bebidas_embotelladas=bebidas_embotelladas,v2_orina_embotelladas_vidrio=embotelladas_vidrio,
  v2_orina_fuente_principal=fuente_principal,v2_orina_pesticidas=pesticidas,
  v2_orina_afirmativo_pesticidas=afirmativo_pesticidas,v2_orina_cosmeticos=cosmeticos,
  v2_orina_otro_cosmeticos=otro_cosmeticos,
  v2_orina_perfumes_48=perfumes_48,v2_orina_cremas_48=cremas_48,v2_orina_cabello_48=cabello_48,
  v2_orina_repelente=repelente,v2_orina_solar_48=solar_48,v2_orina_desodorante_48=desodorante_48,
  v2_orina_polvo_48=polvo_48,v2_orina_asado=asado,v2_orina_tortillas_quemadas=tortillas_quemadas,
  v2_orina_fuego_abierto=fuego_abierto,v2_orina_alrededor_fuego=alrededor_fuego,
  v2_orina_pastico_fuego_abierto=pastico_fuego_abierto,
  v2_orina_pastico_encender_fuego=pastico_encender_fuego,v2_orina_quemo_basura=quemo_basura,
  v2_orina_afirmativo_quemando=afirmativo_quemando,v2_orina_pastico_basura=pastico_basura,
  v2_orina_recolecto_orina=recolecto_orina,
  v2_orina_hora_recoleto_orina=hora_recoleto_orina,v2_orina_custodia_orina=custodia_orina,
  v2_orina_escanee_o_escriba_la_etiqu=escanee_o_escriba_la_etiqu,v2_orina_especifique=especifique,
  v2_orina_otro_orina=otro_orina,v2_orina_comentarios=comentarios
)
) %>% writexl::write_xlsx(
  "output/plastic_b12_v2_piloto.xlsx"
)


#REVISION MUESTRAS ENTERICAS UVG Y JALAPA
entericas_visitas<- readxl::read_xlsx("C:/HAPIN/Main Study/entericas/listado_entericas_jalapa.xlsx")
entericas_mr<- readxl::read_xlsx("C:/HAPIN/Main Study/entericas/listado_entericas_uvg.xlsx", skip = 1 )


entericas_lab<- readxl::read_xlsx("C:/HAPIN/Main Study/entericas/entericas_lab.xlsx")

entericas_mr %>% mutate(id=substr(`No. De Muestra`,1,5),
                        visita=case_when(
                          substr(`No. De Muestra`,7,8)=="C1" ~ "b2",
                          substr(`No. De Muestra`,7,8)=="C3" ~ "b4",
                          TRUE ~ "Revisar"
                        )) %>% distinct(id,visita) %>% left_join(
  entericas_visitas
) %>% group_by(ubicación) %>% count()

entericas_lab %>% mutate(visita=case_when(
  substr(id_muestra,7,8)=="C1" ~ "b2",
  substr(id_muestra,7,8)=="C3" ~ "b4",
  TRUE ~ "Revisar"),
  id=substr(id_muestra,1,5)
) %>% left_join(
  entericas_visitas
) %>% group_by(ubicación) %>% count()

entericas_visitas %>% left_join(
  
  entericas_lab %>% mutate(visita=case_when(
    substr(id_muestra,7,8)=="C1" ~ "b2",
    substr(id_muestra,7,8)=="C3" ~ "b4",
    TRUE ~ "Revisar"),
    id=substr(id_muestra,1,5)
  )
  
) %>% filter(is.na(id_muestra))


#comunidades y participantes hapin
comunidade_coordenadas<-gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id) %>% left_join(
  comunidades %>% select(id_tamizaje, id=house_id, community, lat, long)
) %>% filter(!is.na(lat)) %>% group_by(community) %>% summarize(lat=median(lat), long=median(long)) %>% ungroup() %>% print()

comunidade_conteos<-gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id) %>% left_join(
  comunidades %>% select(id_tamizaje, id=house_id, community, lat, long)
) %>%  group_by(community) %>% count() %>% left_join(
  rutas %>% select(community=comunidad, ruta)
) %>% print()

comunidade_conteos %>% left_join(
  comunidade_coordenadas
) %>% write_csv("output/comunidades_hapin.csv")


#revision datos solicitados por Lisa
comunidade_conteos %>% left_join(
  comunidade_coordenadas
) %>% mutate(
  flag=if_else(ruta=="12",1,0)
) %>% filter(!is.na(flag) & flag=="0")





dt_hapinII_uvg<-read_csv("c:/temp/HAPINIIGT_DATA_2021-11-18_1009.csv")
dt_hapinII_uvg<-dt_hapinII_uvg %>% mutate(id=as.character(record_id)) %>% select(id, everything())

dt_hapinII_uvg %>% filter(!is.na(h41b_date)) %>% select(id, matches("h41b_")) %>% 
  anti_join(
    gt_hapin_II_data %>% filter(!is.na(h41b_date)) %>% select(id)
  ) %>% select(
    id, fecha=h41b_date, iniciales=h41b_by
  ) %>% writexl::write_xlsx("output/migracion_h41b_uvg.xlsx")




#Revisión A26 QC

adultas_gt<-read_csv("c:/temp/HAPINIIGT_DATA_2021-12-17_0942.csv")

gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, a26_image) %>% left_join(
  adultas_gt %>% select(record_id, matches("^a26")) %>% filter(!is.na(a26c_date)) %>% transmute(
    id=as.character(record_id), imagen_rc_uvg='Si'
  )
) %>% select(id, fecha_rc_emory=a26_date, iniciales_emory=a26_by, imagen_rc_uvg) %>% writexl::write_xlsx("output/QC_a26_imagenes.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(e2_date))

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_owa, s4_by, s4_ocon_version,s4_ocon_date,s4_ocon_version,s4_note) %>% mutate(
  type=if_else(grepl("^35",id), "owa","pwg")
)  %>% filter(type=="owa") %>% writexl::write_xlsx("output/s4_owas.xlsx")

#traducción E1 y E2

gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id,visit, matches("^e1_")) %>% 
  select(id, visit, e1_title_other, e1_resolve, e1_problem, e1_factor, e1_cause_other, e1_stove_other, 
         ) %>% writexl::write_xlsx("output/lista_e1_translate.xlsx")


gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_by, h41_date, 
                h41_kap1_ecm_id,h41_kap1_ecm_fid) %>% filter(
                  id=='33135'
)

listado_bebes %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent, s4_consent_c,
                                                          s4_owa)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, c35_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date)
) %>% writexl::write_xlsx("output/revision_m14b.xlsx")

#productividad por dia
 dt_m10_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(m10_date)
 ) %>% select(id, m10_date, m10_by) %>% transmute(
   id, fecha=m10_date, iniciales=m10_by, crf="M10"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01") 
 
 dt_m11_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(m11_date)
 ) %>% select(id, m11_date, m11_by) %>% transmute(
   id, fecha=m11_date, iniciales=m11_by, crf="M11"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01") 

 dt_c33_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(c33_date_2)
 ) %>% select(id, c33_date_2, c33_by_2) %>% transmute(
   id, fecha=c33_date_2, iniciales=c33_by_2, crf="C33"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_c86_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(c86_date)
 ) %>% select(id, c86_date, c86_by) %>% transmute(
   id, fecha=c86_date, iniciales=c86_by, crf="C86"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_h41_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(h41_date_v2)
 ) %>% select(id, h41_date_v2, h41_by_v2) %>% transmute(
   id, fecha=h41_date_v2, iniciales=h41_by_v2, crf="H41"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_h42_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(
   !is.na(h42_date_2)
 ) %>% select(id, h42_date_2, h42_by_2) %>% transmute(
   id, fecha=h42_date_2, iniciales=h42_by_2, crf="H42"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 #24 meses
 dt_m10_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(m10_date)
 ) %>% select(id, m10_date, m10_by) %>% transmute(
   id, fecha=m10_date, iniciales=m10_by, crf="M10"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01") 
 
 dt_m11_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(m11_date)
 ) %>% select(id, m11_date, m11_by) %>% transmute(
   id, fecha=m11_date, iniciales=m11_by, crf="M11"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01") 
 
 dt_c33_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(c33_date)
 ) %>% select(id, c33_date, c33_by) %>% transmute(
   id, fecha=c33_date, iniciales=c33_by, crf="C33"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_b10_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(b10_date)
 ) %>% select(id, b10_date, b10_by) %>% transmute(
   id, fecha=b10_date, iniciales=b10_by, crf="B10"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_h41_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(h41_date)
 ) %>% select(id, h41_date, h41_by) %>% transmute(
   id, fecha=h41_date, iniciales=h41_by, crf="H41"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 dt_h42_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(
   !is.na(h42_date)
 ) %>% select(id, h42_date, h42_by) %>% transmute(
   id, fecha=h42_date, iniciales=h42_by, crf="H42"
 ) %>% mutate(
   dia_semana=lubridate::day(fecha),
   dia_semana=format(fecha,"%A")
 ) %>% filter(fecha>="2022-03-01")
 
 #bind rows 24 meses
 prod_36m<-dt_m10_36m %>% bind_rows(
   list(
     dt_m11_36m,
     dt_c33_36m,
     dt_c86_36m,
     dt_h41_36m,
     dt_h42_36m
   )
 ) %>% mutate(
   visita="36m"
 )
 
prod_24m<- dt_m10_24m %>% bind_rows(
   list(
     dt_m11_24m,
     dt_c33_24m,
     dt_b10_24m,
     dt_h41_24m,
     dt_h42_24m
   )
 ) %>% mutate(
   visita="24m"
 )

prod_24m %>% bind_rows(
  prod_36m
) %>% group_by(visita,dia_semana, crf, iniciales) %>% count() %>% writexl::write_xlsx("output/productividad_dias_semana.xlsx")


prod_24m %>% bind_rows(
  prod_36m
) %>% filter(dia_semana=="sábado") %>%  group_by(visita,dia_semana, crf, iniciales) %>% 
  count() %>% writexl::write_xlsx("output/productividad_sabados.xlsx")


prod_24m %>% bind_rows(
  prod_36m
) %>% writexl::write_xlsx("output/productividad_data.xlsx")
  
  
gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(s4_consent_c=="1") %>% select(
  id, fecha_s4_36m=s4_date , s4_by
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(c33_date_2)) %>% select(
    id, fecha_antro=c33_date_2
  
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(h41_date_v2)) %>% select(
    id, fecha_h41=h41_date_v2
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(h42_date_2)) %>% select(
    id, fecha_h42=h42_date_2
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(c86_date)) %>% select(
    id, fecha_fot=c86_date
  )
) %>% writexl::write_xlsx("output/revision_datos_c36_exposicion_fot.xlsx")



gt_emory_data_arm2 %>% select(redcap_event_name) %>% table()
gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_date, h57_gas_cooking,
                                                         h57_buy_gas,h57_gas_adv) %>% filter(
                                                           h57_gas_cooking>"0"
                                                         ) %>% filter(is.na(h57_gas_adv))


gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, fecha_h41=h41_date_2_v2) %>% left_join(
  datos_participantes %>%   select(id=`ID estudio`,  `Nombre embarazada`,
                                   `Comunidad embarazada (original z10)`) %>% group_by(id) %>% 
    slice(1)
) %>% arrange(`Comunidad embarazada (original z10)`) %>% writexl::write_xlsx("output/36m_realizadas_exposicion.xlsx")


datos_participantes %>% group_by(`ID tamizaje`) %>% slice(1)

listado_bebes_36m %>% group_by(
  lubridate::year(`36_meses`),
  lubridate::month(`36_meses`)
) %>% count() %>% writexl::write_xlsx("output/visitas_36m_por_mes.xlsx")

gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% filter(redcap_event_name=="year_3_q1_36m_arm_1")



revisar<-read_csv("c:/temp/revisar_fechas_c86.csv")
revisar<-revisar %>% mutate_all(as.character)
gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% select(id,
                                                         matches("^c86")) %>% 
  left_join(
  revisar %>% transmute(id=Id, revisar="si")
) %>% filter(revisar=="si") %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% 
    select(id, c30_dob)
) %>% mutate(
  flag=if_else(c30_dob!=c86_dob, "modificar_c86", NA_character_)
) %>% filter(!is.na(flag)) %>% select(
  id, c86_dob, c30_dob, flag
) %>% writexl::write_xlsx("output/actualizar_dob_c86.xlsx")






#revision de pesos c30
dt_c30<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, matches("^c30_"))

revision_c36<-readxl::read_xlsx(
  path = "c:/temp/C36_check.xlsx", sheet = "c36"
)

revision_c36a<-readxl::read_xlsx(
  path = "c:/temp/C36_check.xlsx", sheet = "c36a"
)

revision_c36a<-revision_c36a %>% select(
  IRC, HHID, timepoint, c36_date=c36a_date, 
  c36_cloth=c36a_cloth, c36_wt=c36a_wt
)


dt_revision_c36_integrado<-revision_c36 %>% bind_rows(
  revision_c36a
) %>% mutate(
  id=as.character(HHID)
) %>% select(id, everything())

dt_c33<-gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(
  id, visit, matches("^c33_")
)

dt_revision_c36_integrado %>% left_join(
  dt_c30 %>% transmute(id, unidad_peso_c30=recode(c30_wt_unit,"1"="Kilos","2"="Libras"),
                                  tomo_medida_c30=recode(c30_cloth,"1"="Desnudo", "2"="Con ropa"
                                               ),prome_desnudo_c30=c30_ave_wt, 
                       prome_con_ropa_c30=c30_ave_wt2)
) %>% left_join(
  dt_c33 %>% filter(visit=="b1") %>% transmute(
    id, b1_c33_date=c33_date,  b1_tomo_medida_c33=recode(c33_cloth,"1"="Desnudo", "2"="Con ropa"
    ), b1_prome_desnudo_c33=c33_ave_wt, b1_prome_con_ropa_c33=c33_ave_wt2
  )
) %>% left_join(
  dt_c33 %>% filter(visit=="b2") %>% transmute(
    id, b1_c33_date=c33_date,  b2_tomo_medida_c33=recode(c33_cloth,"1"="Desnudo", "2"="Con ropa"
    ), b2_prome_desnudo_c33=c33_ave_wt, b2_prome_con_ropa_c33=c33_ave_wt2
  )
)%>% left_join(
  dt_c33 %>% filter(visit=="b3") %>% transmute(
    id, b3_c33_date=c33_date,  b3_tomo_medida_c33=recode(c33_cloth,"1"="Desnudo", "2"="Con ropa"
    ), b3_prome_desnudo_c33=c33_ave_wt, b3_prome_con_ropa_c33=c33_ave_wt2
  )
)%>% left_join(
  dt_c33 %>% filter(visit=="b4") %>% transmute(
    id, b4_c33_date=c33_date,  b4_tomo_medida_c33=recode(c33_cloth,"1"="Desnudo", "2"="Con ropa"
    ), b4_prome_desnudo_c33=c33_ave_wt, b4_prome_con_ropa_c33=c33_ave_wt2
  )
) %>% writexl::write_xlsx("output/revision_pesos.xlsx")
  
  
  
  
#falta medidas exposición
gt_hapin_II_data  %>% filter( redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h41_date)) %>% select(
    id, h41_date
  ) %>% anti_join(
    medida_mixta %>% bind_rows(
      medida_directa,
      medida_indirecta
    )
  )

gt_hapin_II_data  %>% filter( redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h41_date)) %>% select(id,h41_date, h41_by,  matches("*ecm_id")) %>% filter(
    h41_m_ecm_id=="235" | h41_c_ecm_id=="235" | h41_sap_ecm_id=="235"
  ) %>% arrange(h41_date) %>% writexl::write_xlsx("output/emc_235.xlsx")

gt_hapin_II_data  %>% filter( redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(h41_date_v2)) %>% select(id,h41_date_v2, h41_by_v2, h41_c_device) %>%
  filter(
    h41_c_device=="240" 
  ) %>% arrange(h41_date_v2) %>% writexl::write_xlsx("output/emc_235_36m.xlsx")

gt_hapin_II_data  %>% filter( redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h41_date)) %>% select(id,h41_date, h41_by,  matches("*ecm_id")) %>% filter(
    h41_m_ecm_id=="240" | h41_c_ecm_id=="240" | h41_sap_ecm_id=="240"
  ) %>% arrange(h41_date) %>% writexl::write_xlsx("output/emc_240.xlsx")

gt_repeat_data %>% group_by(redcap_event_name) %>% count()



gt_emory_repeat_data %>% group_by(redcap_event_name) %>% count()

gt_emory_repeat_data %>% filter(!is.na(c36a_date)) %>% filter(c36a_hhid=="35144") %>% 
  select(record_id, c36a_date, c36a_hhid)


gt_hapin_II_data %>% filter( redcap_event_name=="24_month_arm_1") %>%  filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by) %>% anti_join(
  gt_hapin_II_data %>% filter( redcap_event_name=="24_month_arm_1") %>% filter(!is.na(c31_date)) %>% 
    select(id)
)

z10_export<-read_csv("c:/temp/z10.csv")

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_date)) %>% select(
    id_tamizaje=id, id=s4_main_id
  )
) %>% left_join(
  z10_export %>% mutate(
    id_tamizaje=if_else(grepl("^3",record_id), id_estudio, record_id)
  ) %>% select(id_tamizaje, z10_alti_gps)
) %>% filter(is.na(z10_alti_gps))

gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% group_by(redcap_event_name) %>% count()



gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(id, c33_date_2,c33_wt1_2, c33_wt2_2,
                                                           c33_ht1_2, c33_ht2_2) %>% mutate(
  min(as.numeric(c33_ht1_2))
)

gt_hapin_II_data %>% filter(!is.na(c31_dob_2)) %>% select(id, c31_by_2,c31_date_2, c31_dob_2) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  diferencia=if_else(as.Date(c31_dob_2)!= as.Date(c30_dob),"1", NA_character_)
) %>% filter(!is.na(diferencia)) %>% select(
  id, iniciales=c31_by_2, 
  fecha_c31= c31_date_2,
  fecha_nac_c31=c31_dob_2,
  fecha_nac_c30=c30_dob
)


gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id,  h41_by_v2, 
                                                            c41_child_v2, h41_date_v2, 
                                                            h41_date_2_v2,h41_c_device) %>% filter(
as.Date(h41_date_v2)>as.Date(h41_date_2_v2)
)


gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>% select(
  id, redcap_event_name, h42_by_2, h42_child_2, matches("^H42_")
) %>% filter(id=="33343")


gt_hapin_II_data %>% filter(!is.na(h42_date_2)) %>% select(id, h42_by_2, h42_date_2, h43_pcook_2 ) %>% filter(
  is.na(h43_pcook_2)
)


gt_hapin_II_data %>% filter(!is.na(h41_date_2)) %>% select(id, h41_date_2, h41_by_2) %>% 
  filter(as.Date(h41_date_2)>="2022-05-15")

h41_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2, h41_by_v2) %>% 
  filter(as.Date(h41_date_v2)>="2022-05-15") %>% mutate(
    dia=format(h41_date_v2, "%A"),
    mes=format(h41_date_v2,"%B"),
    iniciales=h41_by_v2
  ) %>% group_by(
    mes,dia,iniciales
  ) %>% count() %>% mutate(crf="H41")

h41_36m_realizados

c33_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(id, c33_date_2, c33_by_2) %>% 
  filter(as.Date(c33_date_2)>="2022-05-15") %>%  mutate(
    dia=format(c33_date_2, "%A"),
    mes=format(c33_date_2, "%B"),
    iniciales=c33_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C33")

s4_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(
  id, s4_date, s4_by
) %>% filter(
  as.Date(s4_date)>="2022-05-15"
) %>% mutate(
  dia=format(s4_date, "%A"),
  mes=format(s4_date, "%B"),
  iniciales=s4_by
) %>% group_by(
  mes, dia, iniciales
) %>% count() %>% mutate(crf="S4")




h41_36m_realizados %>% bind_rows(
  list(
    c33_36m_realizados,
    s4_36m_realizados
  )
) %>% writexl::write_xlsx("output/productividad_dia_expo_clinica.xlsx")

gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% group_by(redcap_event_name) %>% count()



#cuantos H55 de hapin 1 tiene mudanza por visita
gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(
  id, h55_date, visit,
  No_mudanza=h55_move___0, 
  Mudanza_madre=h55_move___1, 
  Mudanza_owa=h55_move___2, 
  Mudanza_nino=h55_move___3,	
  dentro_area_estudio=h55_area
) %>% group_by(
  #id,
  visit,
  #No_mudanza,
  Mudanza_madre
  #Mudanza_owa,
  #Mudanza_nino
) %>% count()
#%>% writexl::write_xlsx("output/mudanzas.xlsx")
#cantidad de h55 por visita
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=recode(s6_arm, "1"="Intervencion", "0"="Control"))
gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% group_by(redcap_event_name) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(
  id, h55_date, visit,
  No_mudanza=h55_move___0, 
  Mudanza_madre=h55_move___1, 
  Mudanza_owa=h55_move___2, 
  Mudanza_nino=h55_move___3,	
  dentro_area_estudio=h55_area
) %>% filter(Mudanza_madre=='1') %>% group_by(id) %>% count() %>% filter(n=="1")
