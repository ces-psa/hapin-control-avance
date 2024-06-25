library(package = "tidyverse")

# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

#salidas
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

##VISITAS VENCIDAS GRUPO DE CLINICA
#------------------------------------

matriz_eventos_arm2<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm) & visit=="baseline") %>% 
    transmute(id, grupo=if_else(s6_arm=="1", "Intervención","Control"))
) %>% 
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  nacio=if_else(
    !is.na(c30_dob), "Si", "No"
  ),
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(fpp)
  )
) %>% mutate(
  type = if_else(
    condition = grepl("^35[0-9]{3}", id),
    true = "oaw",
    false = "pw"
  ) 
) %>%  
  mutate(
  fcc=as.Date(fpp) - lubridate::days(280),
  edad_dias=if_else(nacio=="No", Sys.Date()-as.Date(fcc), as.Date(fecha_nacimiento) - as.Date(fcc)),
  edad_semanas= as.numeric(edad_dias) %/% 7,
  edad_residuo = as.numeric(edad_dias) %% 7,
  edad_estacional=paste0(edad_semanas,".",edad_residuo)
) %>% select(-fcc, -edad_dias, -edad_semanas, -edad_residuo) %>% 
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date) & visit=="baseline") %>% select(id, fec_lb=m10_date) 
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) & visit=="p1") %>% select(id, fec_p1=m11_date) 
) %>% left_join(
  gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a23_date)) & visit=="p2") %>% 
    transmute(id, fec_p2=if_else(!is.na(m11_date), m11_date, a23_date)) 
) %>% left_join(
 gt_emory_data_arm2 %>% filter(!is.na(c30_date) & visit=="parto" ) %>% select(id, fec_birth=c30_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c31_date) & visit=="m1") %>% select(id, fec_m1=c31_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a23_date)) & visit=="b1") %>% transmute(id, fec_b1=if_else(!is.na(m11_date),m11_date, a23_date))
)%>% left_join(
  gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a23_date)) & visit=="b2") %>% transmute(id, fec_b2=if_else(!is.na(m11_date),m11_date, a23_date))
)%>% left_join(
  gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a23_date)) & visit=="b3") %>% transmute(id, fec_b3=if_else(!is.na(m11_date),m11_date, a23_date))
)%>% left_join(
  gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a23_date)) & visit=="b4") %>% transmute(id, fec_b4=if_else(!is.na(m11_date),m11_date, a23_date))
) %>% left_join(
  salidas %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date)
  ) %>% select(id, fec_salida=e3_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id, fec_aborto=e1_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id, fec_muerte=e2_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, fec_salida_owa=e3_date_o)
)

#matriz de crf realizados
matriz_eventos_arm2 %>% writexl::write_xlsx("output/matriz_eventos.xlsx")

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm) & visit=="baseline") %>% 
    transmute(id, grupo=if_else(s6_arm=="1", "Intervención","Control"))
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
  ) %>% mutate(
    nacio=if_else(
      !is.na(c30_dob), "Si", "No"
    ),
    fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(fpp)
    )
  ) %>% mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    ) 
  ) %>% left_join(
    salidas %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date)
    ) %>% select(id, fec_salida=e3_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id, fec_aborto=e1_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id, fec_muerte=e2_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, fec_salida_owa=e3_date_o)
  ) %>%  
  mutate(
    fcc=as.Date(fpp) - lubridate::days(280),
    edad_dias=if_else(nacio=="No", Sys.Date()-as.Date(fcc), as.Date(fecha_nacimiento) - as.Date(fcc)),
    edad_semanas= as.numeric(edad_dias) %/% 7,
    edad_residuo = as.numeric(edad_dias) %% 7,
    edad_estacional=paste0(edad_semanas,".",edad_residuo)
  ) %>% select(-edad_dias, -edad_semanas, -edad_residuo) %>% group_by(id) %>% 
  mutate(
    limite_p1=as.Date(fcc) + lubridate::days(223),
    inicia_p1=as.Date(fcc) + lubridate::days(168),
    #sacar la cantidad de p1 esperadas
p1_esperadas = case_when(
    !is.na(fec_salida) & (as.Date(fcc) + lubridate::days(168) > as.Date(fec_salida)) ~ "FALSE",
    !is.na(fec_aborto) & (as.Date(fcc) + lubridate::days(168) > as.Date(fec_aborto)) ~ "FALSE",
    is.na(grupo) ~ "FALSE",
    as.Date(fcc) + lubridate::days(168) <= Sys.Date() ~ "TRUE"
    
  )
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(visit=="p1" & (!is.na(m11_date) | !is.na(a23_date)) ) %>% select(id, m11_date, a23_date, visit)
  ) %>% 
  group_by(id) %>% 
  mutate(
    p1_realizadas=case_when(
    is.na(m11_date) & is.na(a23_date) ~ "FALSE",
    type=="pw" & is.na(m11_date) ~ "FALSE",
    type=="oaw" & is.na(a23_date) & is.na(m11_date) ~ "FALSE",
    TRUE ~ "TRUE"
  )
  )%>% 
  select(-fpp, -c30_dob, -id_tamizaje) %>% mutate(
    p1_vencidas=case_when(
      p1_esperadas=="FALSE" ~ "FALSE",
      p1_realizadas=="TRUE" ~ "FALSE",
      p1_realizadas=="FALSE" & limite_p1>=Sys.Date() ~ "FALSE",
      p1_realizadas=="FALSE" & as.Date(fcc) + lubridate::days(224) <= Sys.Date() ~ "TRUE"
      )
  ) %>% group_by(p1_esperadas, p1_realizadas, p1_vencidas) %>% count()
  write_csv("output/vencidas.csv")

  
  # %>%  
