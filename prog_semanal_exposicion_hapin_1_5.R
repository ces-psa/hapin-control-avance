library("tidyverse")
#install.packages("xml2")
library(xml2)
library(XML)
library(purrr)
#catalogo de productos
cat_hogares_estufas<-read_csv("data/dictionaries/cat_hogares_estufas.csv", show_col_types = FALSE)
hogares_con_estufa<-cat_hogares_estufas %>% mutate_all(as.character) %>% select(id,tipo_estufa_control=Clasificacion)
cat_fot_pendientes<-read_csv("data/dictionaries/cat_fot_pendientes.csv",show_col_types = FALSE)
cat_fot_pendientes<-cat_fot_pendientes %>% mutate_all(as.character)

cat_blancos_48m<-read_csv("data/dictionaries/cat_blancos_48m.csv")
cat_blancos_48m<-cat_blancos_48m %>% mutate_all(as.character)

cat_blancos_60m<-read_csv("data/dictionaries/cat_blancos_60m.csv")
cat_blancos_60m<-cat_blancos_60m %>% mutate_all(as.character)
#listado de bebes
#lista de bebes candidatos
listado_bebes<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c)
) %>% mutate(
  descartar=case_when(
    e3_reason_c=="6" ~ "Si",
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
                                 Celular_madre_nino=`Celular embarazada`, Celular_papa_nino=`Celular esposo`, 
                                 Celular_otro_miembro) 
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% group_by(id) %>% slice(1) %>% 
  print()

#PROGRAMACION EXPOSICION
#36 m3w3w
data_36_meses<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1" & !is.na(c31_date_2)) %>% 
  select(id, c31_date=c31_date_2) %>%
        # peso=c33_ave_wt_2, talla=c33_ave_ht_2) 
     anti_join (
           gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% select(id) 
         ) %>% left_join(
           datos_participantes %>% select(id=`ID estudio`,
                                          `ID tamizaje`,`Nombre embarazada`, Nombre_bb,
                                          comunidad=`Comunidad embarazada (original z10)`,
                                          `Comunidad embarazada (nueva)`,`Celular embarazada`,
                                          `Celular esposo`, Celular_otro_miembro,
                                          HAPIN_II_madre, HAPIN_II_nino,
                                          HAPIN_II_adulta
           ) %>% group_by(id) %>% slice(1) 
         ) %>% left_join(
           rutas
         ) %>% left_join(
           gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                                      fecha_nacimiento=c30_dob)
         ) %>% left_join(
           listado_bebes %>% select(id,  fecha_b6=`36_meses`) %>% group_by(id) %>% slice(1) 
         ) %>% left_join(
           hogares_con_estufa
         ) %>% left_join(
           gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(
             redcap_event_name=="year_3_q1_36m_arm_1"
           ) %>% transmute(id,
                                                                      consent_nino=recode(s4_consent_c,"1"="Si","0"="No"),
                                                                      consent_madre=recode(s4_consent,"1"="Si","0"="No"),
                                                                      adulta=if_else(is.na(s4_ocon_date),"No","Si")
           )
         )
#Hogares que salieron en 24 meses
salidas_24m<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
  select(id, e3_date) 

#Hogares que salieron en 36 meses
salidas_36m<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

#24 y 36 meses
progra_expo<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% filter(!is.na(c33_date) | 
                                                                              !is.na(a23_date)) %>% 
  select(id, c33_date) %>% 
  anti_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>%  filter(!is.na(h41_date)) %>% select(id) 
) %>% 
  left_join(
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
) %>% 
  left_join(
  listado_bebes %>% select(id,  fecha_b5=`24_meses`) 
) %>% 
  filter(as.Date(fecha_b5)>"2021-01-15") %>% 
  left_join(
  hogares_con_estufa
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% transmute(id,
              consent_nino=recode(s4_consent_c,"1"="Si","0"="No"),
              consent_madre=recode(s4_consent,"1"="Si","0"="No"),
              adulta=if_else(is.na(s4_ocon_date),"No","Si")
)
) %>% 
  anti_join(
    bind_rows(
      salidas_24m %>% select(id)
      # ,
      # salidas_36m %>% select(id)
    )
  ) %>%  #datos 36m
  bind_rows(
  data_36_meses %>% anti_join( salidas_36m %>% select(id))
) %>% anti_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(h41_date_v2)) %>% select(id)
)  

# #lista 36 meses
# listado_bebes %>% select(id, fecha_nacimiento) %>% mutate(
#   fecha_48=as.Date(fecha_nacimiento) + lubridate::days(30.25*48)
# ) %>% anti_join(
#   gt_hapin_II_data %>% group_by(redcap_event_name) %>% count()
# )
 

#PROGRAMACION EXPOSICION 48 meses
#48 meses
data_48_meses<-gt_hapin_II_data %>% filter(visit=="b7" & !is.na(m10_date)) %>% 
  select(id, m10_date) %>%
  # peso=c33_ave_wt_2, talla=c33_ave_ht_2) 
  anti_join (
    gt_hapin_II_data %>%filter(visit=="b7") %>% filter(!is.na(h41_date_v2)) %>% select(id) 
  ) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,
                                   `ID tamizaje`,`Nombre embarazada`, Nombre_bb,
                                   comunidad=`Comunidad embarazada (original z10)`,
                                   `Comunidad embarazada (nueva)`,`Celular embarazada`,
                                   `Celular esposo`, Celular_otro_miembro,
                                   HAPIN_II_madre, HAPIN_II_nino,
                                   HAPIN_II_adulta
    ) %>% group_by(id) %>% slice(1) 
  ) %>% left_join(
    rutas
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                               fecha_nacimiento=c30_dob)
  ) %>% left_join(
    listado_bebes %>% select(id,  fecha_b7=`48_meses`) %>% group_by(id) %>% slice(1) 
  ) %>% left_join(
    hogares_con_estufa
  ) %>% left_join(
    gt_hapin_II_data %>%  filter(!is.na(s4_date)) %>% filter(
      visit=="b7"
    ) %>% transmute(id,
                    consent_nino=recode(s4_consent_c,"1"="Si","0"="No"),
                    consent_madre=recode(s4_consent,"1"="Si","0"="No"),
                    adulta=if_else(is.na(s4_ocon_date),"No","Si")
    )
  )

#Hogares que salieron en 36 meses
salidas_48m<-gt_hapin_II_data %>% filter(visit=="b7") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

data_48_meses<-data_48_meses %>% anti_join(
  salidas_48m
)

dt_programacion_exposicion<-progra_expo %>% select(-c33_date, -fecha_b5, -consent_nino, -consent_madre, -adulta, -c31_date, -fecha_b6) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c31_date_2)) %>% select(id, fecha_reclutamiento=c31_date_2)
) %>%  mutate(
  visita="b6"
)  %>% 
  mutate(
  fecha_b6=as.Date(fecha_nacimiento) + lubridate::days(1095),
  dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b6) ),
  categoria=case_when(
    dif_dias <= 30 ~ "<=30",
    dif_dias > 30  & dif_dias <=60 ~ "31-60",
    dif_dias > 60  & dif_dias <=90 ~ "61-90",
    dif_dias > 90 ~ ">90"
  ),
  fin_ventana=fecha_b6 + lubridate::days(87),
  dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_b6),
  edad_actual= as.numeric((Sys.Date() - as.Date(fecha_nacimiento)) / 30.25)
)  %>% bind_rows(
  data_48_meses %>% mutate(visita="b7",
                           fecha_b7=as.Date(fecha_nacimiento) + lubridate::days(1460),
                           dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b7) ),
                           categoria=case_when(
                             dif_dias <= 30 ~ "<=30",
                             dif_dias > 30  & dif_dias <=60 ~ "31-60",
                             dif_dias > 60  & dif_dias <=90 ~ "61-90",
                             dif_dias > 90 ~ ">90"
                           ),
                           fin_ventana=fecha_b7 + lubridate::days(87),
                           dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_b7),
                           edad_actual= as.numeric((Sys.Date() - as.Date(fecha_nacimiento)) / 30.25)
                           ) 
) %>%   select(20,17,1:2,13,19,21,22,3,15,12,4:8,14) %>% 
arrange(desc(dias_pasados_fecha_visita)) %>% left_join(
  datos_participantes %>% select(id=`ID estudio` , sector_old, direccion_old, referencia_old, sector_new, referencia_new, 
                                 direccion_new, telefono_new)
) %>% select(
  categoria, visita, id,`ID tamizaje`, fecha_nacimiento, fin_ventana, dias_pasados_fecha_visita, `Nombre embarazada`,Nombre_bebe=Nombre_bb,
  ruta, comunidad_1= comunidad, comunidad_2= `Comunidad embarazada (nueva)`, sector_old, 
  direccion_old, referencia_old, sector_new, direccion_new, referencia_new, telefono_new, `Celular embarazada`,
  `Celular esposo`, Celular_otro_miembro, tipo_estufa_control
) %>%   left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                                 contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
) %>% left_join(
  cat_fot_pendientes %>% transmute(id, hacer_FOT="Si")
) 

dt_fot_blncos_48m<-dt_programacion_exposicion %>% left_join(
  cat_blancos_48m %>% transmute(id, colocar_blanco="Si")
) %>% select(id, hacer_FOT, colocar_blanco)

dt_fot_blncos_48m<-dt_fot_blncos_48m %>% mutate_all(., ~replace(.,is.na(.),"No"))

programacion_integrada_exposicion<-dt_programacion_exposicion %>% select(-hacer_FOT) %>%  left_join(
  dt_fot_blncos_48m
) %>% select(
  categoria, visita, id, `ID tamizaje`, hacer_FOT, colocar_blanco, everything()
) %>% transmute(
  categoria, visita, `ID tamizaje`, id, FOT=hacer_FOT, Blanco=colocar_blanco, comunidad_1, comunidad_2, 
  fecha_nacimiento=as.Date(fecha_nacimiento), fin_ventana=as.character(fin_ventana), dias_pasados_fecha_visita=as.character(dias_pasados_fecha_visita), `Nombre embarazada`, Nombre_bebe,
  ruta, sector_old, direccion_old, referencia_old, sector_new, direccion_new, referencia_new, telefono_new, 
  `Celular embarazada`, `Celular esposo`, Celular_otro_miembro, tipo_estufa_control, contacto1_h56, contacto2_h56,
  contacto3_h56
) %>% bind_rows(
  progra_intensivos_update
) 

programacion_integrada_exposicion<-programacion_integrada_exposicion %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b7") %>% filter(s4_consent_c=='0') %>% transmute(
    id, consintio_48m="No"
  )
) 

#programación 60 meses
#60 meses
data_60_meses<-gt_hapin_II_data %>% filter(visit=="b8" & !is.na(m10_date)) %>% 
  select(id, m10_date) %>%
  # peso=c33_ave_wt_2, talla=c33_ave_ht_2) 
  anti_join (
    gt_hapin_II_data %>%filter(visit=="b8") %>% filter(!is.na(h41_date_v2)) %>% select(id) 
  ) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,
                                   `ID tamizaje`,`Nombre embarazada`, Nombre_bb,
                                   comunidad=`Comunidad embarazada (original z10)`,
                                   `Comunidad embarazada (nueva)`,`Celular embarazada`,
                                   `Celular esposo`, Celular_otro_miembro,
                                   HAPIN_II_madre, HAPIN_II_nino,
                                   HAPIN_II_adulta, sector_old, direccion_old, referencia_old,
                                   sector_new, direccion_new, referencia_new, telefono_new
    ) %>% group_by(id) %>% slice(1) 
  ) %>% left_join(
    rutas
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                               fecha_nacimiento=c30_dob)
  ) %>% left_join(
    listado_bebes %>% select(id,  fecha_b7=`48_meses`) %>% group_by(id) %>% slice(1) 
  ) %>% left_join(
    hogares_con_estufa
  ) %>% left_join(
    gt_hapin_II_data %>%  filter(!is.na(s4_date)) %>% filter(
      visit=="b8"
    ) %>% transmute(id,
                    consent_nino=recode(s4_consent_c,"1"="Si","0"="No"),
                    consent_madre=recode(s4_consent,"1"="Si","0"="No"),
                    adulta=if_else(is.na(s4_ocon_date),"No","Si")
    )
  )

#Hogares que salieron en 60 meses
salidas_60m<-gt_hapin_II_data %>% filter(visit=="b8") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

data_60_meses<-data_60_meses %>% anti_join(
  salidas_60m
)

#armar programación 60 meses
programacion_60_meses_integrada<-data_60_meses %>% mutate(visita="b8",
                         fecha_b8=as.Date(fecha_nacimiento) + lubridate::days(1815),
                         dif_dias=as.numeric( as.Date(Sys.Date()) - as.Date(fecha_b8) ),
                         categoria=case_when(
                           dif_dias <= 30 ~ "<=30",
                           dif_dias > 30  & dif_dias <=60 ~ "31-60",
                           dif_dias > 60  & dif_dias <=90 ~ "61-90",
                           dif_dias > 90 ~ ">90"
                         ),
                         fin_ventana=fecha_b8 + lubridate::days(87),
                         dias_pasados_fecha_visita= Sys.Date() - as.Date(fecha_b8),
                         edad_actual= round(as.numeric((Sys.Date() - as.Date(fecha_nacimiento)) / 30.25),1)
) %>% transmute(
  #como se manejará el tema de blancos en 60 meses?
  categoria, visita, `ID tamizaje`, id, FOT="", Blanco="", comunidad_1=comunidad, 
  comunidad_2=`Comunidad embarazada (nueva)`, fecha_nacimiento, fin_ventana, 
  dias_pasados_fecha_visita, `Nombre embarazada`, Nombre_bebe=Nombre_bb, ruta,sector_old, direccion_old,
  referencia_old, sector_new, direccion_new, referencia_new, telefono_new, `Celular embarazada`,
  `Celular esposo`, Celular_otro_miembro, tipo_estufa_control
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                             contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
) %>% mutate(
  #como se manejará?
  Duplicado=""
)


#generar excel con la programación integrada
programacion_integrada_exposicion<-programacion_integrada_exposicion %>% bind_rows(
  programacion_60_meses_integrada %>% mutate(fecha_nacimiento=as.Date(fecha_nacimiento),
                                             fin_ventana=as.character(fin_ventana),
                                             dias_pasados_fecha_visita=as.character(dias_pasados_fecha_visita))
) 

programacion_integrada_exposicion %>% mutate(
  visita=recode(
    visita, "b7"="48m",
    "b8"="60m",
    "54m"="54m"
  )
) %>% left_join(
  cat_blancos_60m %>% transmute(id=id_blancos, blanco_60m="Si")
) %>% 
  mutate(
  blanco_60m=if_else(is.na(blanco_60m), "No", blanco_60m),
  Blanco=if_else(Blanco=="No" | Blanco=="Si", Blanco, blanco_60m)
) %>% select(-blanco_60m) %>% arrange(desc(as.numeric(dias_pasados_fecha_visita))) %>%  writexl::write_xlsx(paste0("output/visitas/exposicion/programacion_exposicion_48_54_60_meses_",Sys.Date(),".xlsx"))

#crear archivo GPX de coordenadas actualizadas
df_gpx<- programacion_integrada_exposicion %>% select(id_estudio=id) %>% 
#   bind_rows(
#   progra_intensivos_update %>% select(id)
# ) %>% 
  left_join(
  z10_coordinates_new
) %>% filter(!is.na(z10_gps_lat_new)) %>% transmute(
  ident=id_estudio, 
  Latitude=z10_gps_lat_new,
  Longitude=paste0("-",as.numeric(z10_gps_long_2))
)

df_gpx %>% write_csv(., paste0("output/visitas/exposicion/programacion_",Sys.Date(),".txt"))



programacion_coordenadas<-dt_programacion_exposicion %>% select(id_estudio=id) %>% left_join(
  z10_coordinates_new
) %>% filter(!is.na(z10_gps_lat_new)) %>% transmute(ident=id_estudio, latitude=z10_gps_lat_new, 
                                                 longitude=z10_gps_long_2, altitude=z10_gps_alt_2)




gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date) %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id)
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,
                                 `ID tamizaje`,`Nombre embarazada`,
                                 comunidad=`Comunidad embarazada (original z10)`,
                                 `Comunidad embarazada (nueva)`,`Celular embarazada`,
                                 `Celular esposo`, Celular_otro_miembro,
                                 HAPIN_II_madre, HAPIN_II_nino,
                                 HAPIN_II_adulta
  )
) %>% left_join(
  rutas
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, 
                                                             fecha_nacimiento=c30_dob)
) %>% left_join(
  listado_bebes %>% select(id,  fecha_b5=`24_meses`) 
) %>% filter(as.Date(fecha_b5)<"2021-01-15") %>% left_join(
  hogares_con_estufa
) 



