




#CONTEO CUMPLE AÑOS DE BEBES HAPIN

#lista de bebes candidatos
listado_bebes<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
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
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`, Celular_papa_nino=`Celular esposo`, Celular_otro_miembro)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% 
  print() #write_csv("output/listado_Seguimiento_bebes_postHapin.csv")

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

conte_60<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
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




####------------------------------
# ARMAR LA TABLA PARA EXPORTAR EXCEL CON LOS CONTEOS
#---------------------------------
Anio<-c("2020")
Mes<-c("12")
anio_2020<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()


Anio<-c("2021")
Mes<-c("1","2","3","4","5","6","7","8","9","10","11","12")
anio_2021<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()

Anio<-c("2022")
Mes<-c("1","2","3","4","5","6","7","8","9","10","11","12")
anio_2022<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()

Anio<-c("2023")
Mes<-c("1","2","3","4","5","6","7","8","9","10","11","12")
anio_2023<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()

Anio<-c("2024")
Mes<-c("1","2","3","4","5","6","7","8","9","10","11","12")
anio_2024<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()

Anio<-c("2025")
Mes<-c("1","2","3","4","5","6","7","8","9","10","11","12")
anio_2025<-data.frame(Anio,Mes) %>% arrange(Anio) %>% print()

matris_periodos<-anio_2020 %>% mutate_all(as.character) %>%  bind_rows(
  list(
    anio_2021 %>% mutate_all(as.character),
    anio_2022 %>% mutate_all(as.character),
    anio_2023 %>% mutate_all(as.character),
    anio_2024 %>% mutate_all(as.character),
    anio_2025 %>% mutate_all(as.character)
  )
) %>% mutate(
  codigo=paste0(Anio,Mes)
) %>% 
  select(codigo, Anio, Mes) %>% print()


conteo_bebes<-matris_periodos%>% mutate_all(as.character) %>% 
  left_join(
  conte_24 %>% mutate(
    codigo=paste0(anio, mes)
  ) %>% transmute(codigo=as.character(codigo), meses_24=n) 
) %>% select(codigo, Anio, Mes, meses_24) %>% 
  left_join(
      conte_36 %>% mutate(
        codigo=paste0(anio, mes)
      ) %>% transmute(codigo=as.character(codigo), meses_36=n)
    ) %>% select(codigo, Anio, Mes, meses_24, meses_36) %>%  
    left_join(
      conte_48 %>% mutate(
        codigo=as.character(paste0(anio, mes))
      ) %>% transmute(codigo=as.character(codigo), meses_48=n)
    ) %>% select(codigo, Anio, Mes,meses_24, meses_36, meses_48) %>% 
    left_join(
  conte_60 %>% mutate(
    codigo=as.character(paste0(anio, mes))
  ) %>% transmute(codigo=as.character(codigo), meses_60=n)
)  %>%   mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%mutate(
  Total_por_Mes = select(., contains("meses")) %>% rowSums(.,na.rm = TRUE)
)%>% select(
  Anio, Mes, `24_meses`=meses_24, `36_meses`=meses_36, `48_meses`=meses_48, `60_meses`=meses_60, Total_por_Mes
) %>% 
  print()

list(
 "conteo_candidatos"= conteo_bebes,
 "lista_candidatos"= listado_bebes
) %>%  writexl::write_xlsx("output/candidatos_hapin_1_5.xlsx")


listado_bebes %>% filter(as.Date(`24_meses`)<Sys.Date()) %>%  select(id, fecha_nacimiento,`24_meses`) %>% left_join(
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
    )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=if_else(s6_arm=="1", "Intervencion","Control"))
) %>% 
  select(
  id_tamizaje=`ID tamizaje`, id, fecha_nacimiento, fecha_24_meses=`24_meses`, edad_actual_meses ,
  participan, brazo, comunidad_1, comunidad_2, ruta, 
  `Celular embarazada`, `Celular esposo`,
  `Celular_otro_miembro`,`Nombre embarazada`,
  `Nombre otra adulta`, Nombre_bebe, HAPIN_II_madre,
  HAPIN_II_nino,
  HAPIN_II_adulta
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% writexl::write_xlsx(paste0("output/visitas/programacion_clinica_24meses_",Sys.Date(),".xlsx"))

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id, matches("s4_")) %>% group_by(
  s4_mcon_version
) %>% count()




