library("tidyverse")
#leer datos de emory
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

datos_m17<-gt_emory_data %>% select(id,m17_date, m17_by, m17_hr, m17_sac, 
                                    m17_one_fetus, m17_lmp_date,m17_lmp_ga,
                         m17_ultra_edd,m17_ultra_ga,
                         m17_ga_method, m17_ga) %>% filter(!is.na(m17_date)) %>% 
  mutate(metodo=case_when(
    m17_ga_method=="1" ~ "FUR", 
    m17_ga_method=="2" ~ "Ultra",
    TRUE ~ NA_character_)) %>% select(-m17_ga_method, -m17_lmp_ga, -m17_ultra_edd, -m17_ultra_ga)

#-----------------------------
#INSCRITAS QUE NO TIENEN M17 O S2
#-----------------------------
gt_emory_data %>%filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id) %>% anti_join(
  bind_rows(
  datos_m17 %>% transmute("id_tamizaje"=id, m17_date, m17_ga, metodo),
  gt_emory_data %>% filter(!is.na(s2_date)) %>% select("id_tamizaje"=id, s2_date)
  )
)


#----------------------
#REVISAR LOS M17 QUE LES FALTA METODO O QUE NO TIENEN FECHA PROBABLE DE PARTO INGRESADA
datos_m17 %>% filter(is.na(metodo)) %>% left_join(
  gt_emory_data %>% filter(!is.na(s4_main_id)) %>% select(id, "id_estudio"=s4_main_id)
) %>% filter(!is.na(id_estudio))

datos_m17 %>% filter(is.na(m17_ga)) %>% left_join(
  gt_emory_data %>% filter(!is.na(s4_main_id)) %>% select(id, "id_estudio"=s4_main_id)
) %>% filter(!is.na(id_estudio))


datos_m17 %>% filter(m17_hr=="1" & m17_sac=="1" & m17_one_fetus=="1" ) %>%  filter(!is.na(semanas_ga)) %>% left_join(
  gt_emory_data %>% select(id, "id_estudio"=s4_main_id, s4_date) %>% filter(!is.na(s4_date))
) %>% select(id,id_estudio,"fecha_s4"=s4_date, "fecha_m17"=m17_date, "inciales_m17"=m17_by, "edad_gestacional"=semanas_ga, "fpp"=m17_ga ) %>% 
  mutate(
    fembarazo=as.Date(fpp)- lubridate::days(280),
    dias=as.Date(fecha_m17)- as.Date(fembarazo),
    ga_calculada=paste0(as.numeric(dias)%/%7,".", as.numeric(dias)%%7)
  ) %>% arrange(ga_calculada) %>% filter(edad_gestacional>="9") %>% filter(is.na(fecha_s4)) %>% mutate(
    dias_actual= Sys.Date()- as.Date(fembarazo),
    ga_actual=paste0(as.numeric(dias_actual)%/%7,".",as.numeric(dias_actual)%%7)
  ) %>% select(id,fecha_s4,fecha_m17, inciales_m17, ga_m17=edad_gestacional, fpp, ga_actual) %>% arrange(ga_actual) %>% 
  print(n=Inf)


#-------------------------------
#inscritas sin M17 o S2
#--------------------------------
#s4 sin M17 o S2
  s4_m17<-gt_emory_data %>% select("id_tamizaje"=id,s4_date, s4_by, s4_consent, "id_estudio"=s4_main_id) %>% 
    filter(!is.na(id_estudio)) %>% left_join(
  gt_emory_data %>% select("id_tamizaje"=id, m17_date, 
                           m17_by,
                           m17_ga ) %>% filter(!is.na(m17_date))
) %>% left_join(
  gt_emory_data %>% select("id_tamizaje"=id, s2_date, s2_by, 
                           s2_fetus,s2_gest,s2_onefetus,s2_participate,s2_owa )
) %>% filter(is.na(s2_owa))
  filter(is.na(m17_ga))
#G1514       2019-05-16 WNS   1          33433      NA         NA     NA

#errore de elegibilidad en respuestas condicionantes
  gt_emory_data %>% select("id_tamizaje"=id,s4_date, s4_by, s4_consent, "id_estudio"=s4_main_id) %>% 
    filter(!is.na(id_estudio)) %>% left_join(
      gt_emory_data %>% select("id_tamizaje"=id, s2_date, s2_by, 
                               s2_fetus,s2_gest,s2_onefetus,s2_participate,s2_owa )
    ) %>% mutate(
      elegible=as.integer(s2_fetus)+as.integer(s2_gest)+as.integer(s2_onefetus)+as.integer(s2_participate)
    ) %>% filter(elegible<4)
  
#pendientes de S3
  gt_emory_data %>% select("id_tamizaje"=id,s4_date, s4_by, s4_consent, "id_estudio"=s4_main_id, s4_owa) %>% 
    filter(!is.na(id_estudio)) %>% left_join(
      gt_emory_data %>% select("id_tamizaje"=id, s2_date, s2_by, 
                               s2_participate,s2_owa )
    ) %>% filter(!is.na(s2_owa)) %>% left_join(
      gt_emory_data %>% select("id_tamizaje"=id, s3_date, s3_by, s3_participate ) %>% filter(!is.na(s3_date))
    ) %>% filter(!is.na(s3_date)) %>% write_csv("output/s4_sin_s3.csv")
  
 
  
  
  
  
  #--------------------------------------
  #LISTA DE HOGARES DE LA FUENTE SOLICITADO POR MAYA Y ERICK
  #--------------------------------------
  
  gt_emory_data %>%filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id) %>% 
    anti_join(
      bind_rows(
      salidas %>% select("id_estudio"=id),
      gt_emory_data %>%
        filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
        select("id_estudio"=id)
        )
      ) %>% left_join(gt_participants %>% select(record_id,id_estudio, com_jalapa) %>%
    mutate(house_id=if_else(condition = grepl("^G[0-9]{4}",id_estudio),
                            true = record_id,
                            false = id_estudio
              )) %>% mutate(
                id_tamizaje=if_else(
                  condition = grepl("^G[0-9]{4}",record_id),
                  true = record_id,
                  false = id_estudio
                )) %>% filter(com_jalapa=="2101052" | com_jalapa=="2101125") %>%  mutate(
                  community = recode(
                    com_jalapa,
                    "2101052" = "LA FUENTE DE LA MONTAA",
                    "2101125" = "SAN JOSE LA FUENTE",
                  )
                ) %>% select("id_estudio"=house_id, "comunidad"=community)
            ) %>% filter(!is.na(comunidad)) %>% 
    writexl::write_xlsx(paste0("output/hogares_la_fuente_al_", Sys.Date(),".xlsx"))
  
