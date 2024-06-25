# ReportBack realizados por Maya ----
library("tidyverse")
cat_rb_old<-read_csv("data/dictionaries/cat_reportback_completed.csv")
cat_rb_old<-cat_rb_old %>% mutate_all(as.character)





gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
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
) %>% filter(descartar=="No")   %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`,
                                 Celular_papa_nino=`Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
)%>%
  left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% left_join(
    cat_rb_old
  ) %>% filter(is.na(ReportBack)) %>% transmute(
    id, `ID tamizaje`, fecha_nacimiento=c30_dob, nombre_madre_nino,
    comunidad, Celular_madre_nino, Celular_papa_nino, Celular_otro_miembro, ruta,
    contacto1_h56, contacto2_h56, contacto3_h56
  ) %>% writexl::write_xlsx(paste0(
  "output/listado_visitas_ReportBack_pendientes_",Sys.Date(),".xlsx"  
  )
  )

