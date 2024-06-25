# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

gt_emory_data %>% select(id, h50_date) %>% filter(!is.na(h50_date)) %>% arrange(h50_date) %>% left_join(
  gt_emory_data %>% select(id_tamizaje=id, id=s4_main_id, s4_date) %>% filter(!is.na(id))
) %>% left_join(
  gt_emory_data %>% select(id,c30_date) %>% filter(!is.na(c30_date)) 
) %>% filter(!is.na(c30_date)) %>% 
  left_join(
    gt_emory_data %>% select(id, e3_date) %>% filter(!is.na(e3_date))
  ) %>% filter(is.na(e3_date)) %>%  mutate(
    fecha_prob_salida = c30_date + lubridate::days(365)
  ) %>% select(id_tamizaje,"house_id"=id, "fecha_instalacion_estufa"=h50_date, "Fecha_consentimiento"=s4_date, "Fecha nacimiento"=c30_date, "Fecha_estimada_salida"=fecha_prob_salida) %>% 
  writexl::write_xlsx("output/fecha_finaliza_intervencion.xlsx"
  )
    
