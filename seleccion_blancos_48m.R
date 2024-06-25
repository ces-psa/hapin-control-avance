library(tidyverse)

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

#Hogares que salieron en 36 meses
salidas_48m<-gt_hapin_II_data %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% filter(!is.na(e3_date)) %>% 
  select(id, e3_date)

candidatos_48m<-listado_bebes_36m %>% anti_join(
  salidas_48m
)

# dt_candidatos_blancos<-candidatos_48m %>% anti_join(
#   gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b7") %>% select(id, visit)
# ) %>% transmute(id, rownum=row_number())

dt_candidatos_blancos %>% write_csv("data/dictionaries/candidatos_blancos.csv")

#seed to establish the random
set.seed(11082023)
muestra_blancos<-sample(dt_candidatos_blancos$id, size=45, replace=FALSE)

blancos<-muestra_blancos %>% as.data.frame()
blancos %>% write_csv("data/dictionaries/cat_blancos_48m.csv")



