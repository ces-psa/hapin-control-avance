#"conteos c31
programacion_anterior <- readxl::read_xlsx(path = "output/visitas/programacion_clinica_al_2020-07-24.xlsx", sheet = "c31")

conteo_14_dias<-programacion_anterior %>% select(id, visit=visita_donde_debe_registrarse, fin_ventana, ultima_fecha_vencimiento) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, visit, c31_date)
  ) %>% mutate(
    flag_realizados=if_else(
      !is.na(c31_date), "realizado", "pendiente"
    )
  ) %>% group_by(flag_realizados) %>% summarize(cantidad=n())



#"conteos c31
programacion_anterior <- readxl::read_xlsx(path = "output/visitas/programacion_clinica_al_2020-07-31.xlsx", sheet = "c31")

conteo_7_dias<-programacion_anterior %>% select(id, visit=visita_donde_debe_registrarse, fin_ventana, ultima_fecha_vencimiento) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, visit, c31_date)
  ) %>% mutate(
    flag_realizados=if_else(
      !is.na(c31_date), "realizado", "pendiente"
    )
  ) %>% group_by(flag_realizados) %>% summarize(cantidad=n())

list(
  conteo_7_dias= conteo_7_dias,
  conteo_14_dias= conteo_14_dias
  
) %>% writexl::write_xlsx(paste0("output/conteos_c31_realizados_",Sys.Date(),".xlsx"))




#revision de variables C31

gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, matches("c31_"))

gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date,  
                                                           c31_rr1,c31_rr_method) %>% 
  filter(is.na(c31_rr1)) %>% filter(is.na(c31_rr_method)) %>% mutate(
    anio= lubridate::year(c31_date),
    mes= lubridate::month(c31_date)
  ) %>% group_by(anio, mes) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c31_colect)) %>% select(id, c31_colect)
