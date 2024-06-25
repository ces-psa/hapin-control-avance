#leer reporte de don roberto integrado
data_update<-readxl::read_excel(path = "data/actualizacion_polletones.xlsx", sheet = "update_datos")
datos_roberto<-data_update %>% mutate_all(as.character) %>% select(id=`I D`, notas_roberto=NOTAS, codigo_observacion)

#366 descontando las salidas
#305 casas actualizadas al momento
gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date, h50_traditional) %>%
  left_join(
  datos_roberto
) %>% left_join(
  salidas %>% transmute(id, tiene_salida="Si")
) %>% mutate(flag=if_else(h50_traditional!=codigo_observacion, "revisar_redcap",NA_character_)) %>% 
  write_csv(paste0("output/actualizar_h50_polletones_al_",Sys.Date(),".csv"))


gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date, h50_traditional)

  
 # gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_date, c30_by, c30_dob) %>% left_join(
 #   datos_participantes %>% select(id=`ID estudio`, id_tamizaje=`ID tamizaje`, fecha_parto_z10=`Fecha del parto`)
 # ) %>% mutate_all(as.character) %>% left_join(
 #   salidas %>% transmute(id, tiene_salida="Si")
 # ) %>% mutate(
 #   `diferencia_c30-z10`=as.Date(c30_dob)-as.Date(fecha_parto_z10)
 # ) %>% writexl::write_xlsx("output/fechas_parto_c30_z10.xlsx")

#hogares que falta actualizar la información
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date) %>% anti_join(
   datos_roberto %>% select(id)
 )
 

 
