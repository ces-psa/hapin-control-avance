library("tidyverse")
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id"=s4_main_id) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(
      "id_tamizaje"=id, 
      m17_date,
      m17_by,
      "latido_fetal"=m17_hr,#1=Si,0=No
      "saco_presente"=m17_sac, #1=Si, 0=No
      "un_feto"=m17_one_fetus, #1=si, 0=No
      "anomalia"=m17_abnorm,#1=Si, 0=No
      "sabe_fur"=m17_lmp, #1=Si, 0=No
      "fur_1"=m17_lmp_date,
      "semanas_fur"=m17_lmp_ga,
      "fpp_ultra"=m17_ultra_edd,
      "semanas_ultra"=m17_ultra_ga,
      "metodo"=m17_ga_method, #1=Fur, 2=Ultra
      "fpp"=m17_ga
    )
  ) %>% mutate(
    fecha_alex="2018-12-24",
    fcc=as.Date(fpp)- lubridate::days(280),
    dias= as.Date(fecha_alex) - as.Date(fcc),
    ga_semanas= as.numeric(dias)%/%7,
    ga_dias=as.numeric(dias)%%7,
    edad_m17_calculada=paste0(ga_semanas,".",ga_dias)
  ) %>% select(id_tamizaje,id,fpp,fcc,edad_m17_calculada) %>% 
  mutate(
      errores=case_when(
      metodo=="2" & fpp!=fpp_ultra ~ "fpp diferente a la del metodo elegido",
      is.na(metodo) ~  "sin metodo elegido",
     TRUE ~ NA_character_
    ),
    error_fecha=case_when(
      as.character(m17_date) > as.character(Sys.Date()) ~ "Error en fecha de CRF",
      # as.character(m17_date)>as.character(fpp) ~ "Error en fecha de CRF",
      # as.character(m17_date)>as.character(fpp) ~ "Error en fecha de CRF",
      TRUE ~ NA_character_
  )
  )%>% select(id_tamizaje,"iniciales"=m17_by,fpp_ultra,metodo, fpp, errores, error_fecha) %>% filter(!is.na(errores) | !is.na(error_fecha)) %>% print(n=Inf)
  
  writexl::write_xlsx("output/errores_m17_fpp_metodo.xlsx")
