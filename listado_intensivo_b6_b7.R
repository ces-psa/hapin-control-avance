library("tidyverse")

#listado enviado por Shirin
listado_shirin<-read_csv("c:/temp/Randomized_36M_list.csv")
listado_shirin_gt<-listado_shirin %>% filter(IRC=="Guatemala") %>% mutate(id=as.character(HHID))

#listado de intensivos hapin 1
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
    visita_54=as.Date(fecha_nacimiento) + lubridate::days(1633)
  )
)

list(
  "listado"=listado,
  "conteo"=conteo_intensivos
) %>% writexl::write_xlsx("output/intensivos.xlsx")


listado_intensivo_guate<-listado %>% filter(consintio_b5=="Si" | consintio_b6=="Si") %>% left_join(
listado_shirin_gt
) %>% mutate(
  listado_shirin=if_else(is.na(IRC),"No","Si")
) %>% select(id, cantidad_intensivos, fecha_nacimiento, visita_48=visit_48, visita_54, listado_shirin) %>% 
  arrange(cantidad_intensivos) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=recode(s6_arm,"1"="intervencion", "0"="Control"))
  ) 
  writexl::write_xlsx("output/listado_intensivo_guatemala.xlsx")

  listado_shirin<-listado_shirin_gt %>% left_join(
    listado
  ) %>% select(
    id,IRC, brazo=s6_arm, cantidad_intensivos
  ) %>% left_join(
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
  ) %>% select(id, cantidad_intensivos, brazo, fecha_nacimiento, consintio_b5, consintio_b6, visita_48=visit_48, visita_54)
  #%>% writexl::write_xlsx("output/listado_intensivo_shirin.xlsx")

  list(
    listado_gutemala=listado_intensivo_guate,
    listado_shirin=listado_shirin
  ) %>% writexl::write_xlsx("output/listados_guate_shirin.xlsx")
  
