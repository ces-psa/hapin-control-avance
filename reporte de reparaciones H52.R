#PROPORCION DE ESTUFAS REPARADAS 
gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, fecha_instalacion_estufa=h50_date)
) %>% mutate(type = if_else(
  condition = grepl("^35[0-9]{3}", id),
  true = "oaw",
  false = "pw"
) 
) %>% 
  left_join(

gt_emory_data_arm3 %>% filter(!is.na(h52_date)) %>% select(id, h52_date, h52_by, visit, h52_repair___1,h52_repair___2,h52_repair___3,h52_repair___4,
                                                           h52_repair___555, h52_repair_other) %>% group_by(id) %>% 
                      summarize(cantidad_reparaciones=n(), fec_primera_instalacion=min(as.Date(h52_date)), 
                                                  fec_ultima_reparacion=max(as.Date(h52_date))) 
)%>% left_join(
salidas %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% mutate(type = if_else(
    condition = grepl("^35[0-9]{3}", id),
    true = "oaw",
    false = "pw"
  ) 
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% select(id, e3_date, e3_date_o)
) %>% mutate(fecha_salida=case_when(
  type=="pw" ~ as.Date(e3_date),
  type=="oaw" & as.Date(e3_date_o) > as.Date(e3_date) ~ as.Date(e3_date_o),
  type=="oaw" & as.Date(e3_date_o) < as.Date(e3_date) ~ as.Date(e3_date)
)
) %>% select(-type)
) %>% select(-e3_date, -e3_date_o, -sale) %>% 
  writexl::write_xlsx(paste0("output/cantidad_reparaciones_al_",Sys.Date(),".xlsx" ))


  
  # 
  # mutate(
  #   cantidad_reparaciones = as.numeric(h52_repair___1) +
  #     as.numeric(h52_repair___2) +
  #     as.numeric(h52_repair___3) +
  #     as.numeric(h52_repair___4) +
  #     as.numeric(h52_repair___555)
  # ) %>% filter(cantidad_reparaciones==1)


#HOGRAES CON DOTS
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  brazo=recode(s6_arm,"1"="Intervencion", "0"="Control")
)

all_dots %>% left_join(
  gt_emory_data_arm3
)


all_dots %>% group_by(id_dot)
