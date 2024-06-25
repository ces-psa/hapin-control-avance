#REPORTE HERCUES
#---------------------

library("tidyverse")
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Datos de Hercules
source(file = "scripts/0_get_hercules_data.R", encoding = "UTF-8")


#reporte de consentimientos y B12 Hercules
data_hercules_gt<-gt_emory_data_arm1 %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id) %>% filter(!is.na(id_estudio)) %>% left_join(
  gt_emory_data_arm1 %>% select("id_tamizaje"=id, s1_date, s1_community_name) %>% filter(!is.na(s1_date))
) %>% left_join(
  gt_emory_data_arm2 %>% select("id_estudio"=id, s6_arm, s6_date) %>% filter(!is.na(s6_date)) %>% mutate(brazo_s6=if_else(s6_arm==1, "Intervencion", "Control"))
) %>% left_join(
hercules_consentimiento %>% select("id_estudio"=record, h_consent_accept, h_consent_accept_desc, h_arms, h_date, h_id_hercules, h_iniciales ) %>% mutate(
  acepta_consentimiento = if_else(
    h_consent_accept==1, "Si acepta",
    "No acepta"
  )
)
   %>%  mutate(
     no_accept_desc = case_when(
       h_consent_accept_desc==1 ~ "No esta interesado",
       h_consent_accept_desc==2 ~ "El esposo no acepta",
       h_consent_accept_desc==3 ~ "No quieren más muestras",
       h_consent_accept_desc==4 ~ "Otro",
       TRUE ~ NA_character_
     )
   )
) %>% filter(!is.na(h_consent_accept)) %>% select(-h_consent_accept_desc, -h_consent_accept, -s6_arm, -h_arms) %>% 
  left_join(
    hercules_data %>% mutate(
      fecha_segunda_visita = as.Date(date) + lubridate::days(180)
    ) %>% 
      select(
      "id_estudio"=record, "Fecha B12 biomuestras"= date, "Iniciales B12 biomuestras"=iniciales, visita, fecha_segunda_visita
    ) %>% mutate(
      Visita=case_when(
       visita==1 ~ "Nacimiento",
       visita==2 ~ "B12",
        TRUE ~ NA_character_
      )
    )
  ) %>% left_join(
    comunidades %>% select(id_estudio,community)
  ) %>% select(id_tamizaje, id_estudio, "comunidad"=community, "grupo"=brazo_s6, "fecha_hercules"=h_date, "id_hercules"=h_id_hercules, "iniciales_crf_hercules"=h_iniciales,
               acepta_consentimiento, "porque no acepta"=no_accept_desc, `Fecha B12 biomuestras`, `Iniciales B12 biomuestras`, Visita, fecha_segunda_visita)

  data_hercules_gt %>%  writexl::write_xlsx(paste0("output/reporte_hercules_",Sys.Date(),".xlsx"))

gt_emory_data_arm1 %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id) %>% filter(!is.na(id_estudio)) %>% left_join(
  gt_emory_data_arm2 %>% select("id_estudio"=id, s6_date, s6_arm) %>% filter(!is.na(s6_date)) %>% mutate(
    brazo=recode(s6_arm,
      "1"="Intervencion",
      "0"="Control"
    )
  )
) %>% left_join(
hercules_data %>% select("id_estudio"=record, date, iniciales,
                         muestra_oral_m,
                         id_muestra_oral_m,
                         muestra_nasal_m,
                         id_muestra_nasal_m,
                         muestra_oral_n,
                         id_muestra_oral_n,
                         muestra_nasa_n,
                         id_muestra_nasal_n,
                         m_control,
                         id_muestra_control
                         
            ) 
) %>% filter(!is.na(date)) %>% write_csv("output/muestras_hercules.csv")
  #filter(is.na(brazo))
  group_by(brazo) %>% summarize(
  contador=n()
)

hercules_consentimiento %>% select("id"=record_id, h_id_hercules) %>% left_join(
  hercules_data %>% select(date,record_id, "h_id_hercules"=record_id_hercules) %>% filter(h_id_hercules=="HER-009")
) %>% filter(is.na(date))


data_hercules_gt %>% filter(acepta_consentimiento=="Si aceptó")

segunda_visita<-hercules_b2 %>% transmute("id_estudio"=record,
                                       segunda_visita_realizada="Si",
v2_muestra_oral_m=muestra_oral_m,
v2_id_muestra_oral_m=id_muestra_oral_m,
v2_muestra_nasal_m=muestra_nasal_m,
v2_id_muestra_nasal_m=id_muestra_nasal_m,
v2_muestra_oral_n=muestra_oral_n,
v2_id_muestra_oral_n=id_muestra_oral_n,
v2_muestra_nasa_n=muestra_nasa_n,
v2_id_muestra_nasal_n=id_muestra_nasal_n,
v2_m_control=m_control,
v2_id_muestra_control=id_muestra_control,
v2_muestra_lechematerna=muestra_lechematerna,
v2_id_leche_materna=id_leche_materna
) 

primer_visita<-hercules_data %>% select("id_estudio"=record,
                           muestra_oral_m,
                           id_muestra_oral_m,
                           muestra_nasal_m,
                           id_muestra_nasal_m,
                           muestra_oral_n,
                           id_muestra_oral_n,
                           muestra_nasa_n,
                           id_muestra_nasal_n,
                           m_control,
                           id_muestra_control
                           
  )  %>% filter(!is.na(date)) 

segunda_visita %>% left_join(primer_visita) %>%  write_csv("output/segunda_visita_hercules.csv")

data_hercules_gt %>% left_join(
  primer_visita
) %>% left_join(
  segunda_visita
) %>% write_csv("output/reporte_hercules_muestras.csv")

data_hercules_gt %>% select(id_estudio, id_hercules, iniciales_crf_hercules , fecha_hercules) %>% write_csv("output/id_hercules.csv")

