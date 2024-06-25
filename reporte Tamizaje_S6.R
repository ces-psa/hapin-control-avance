# load use packages
library(package = "tidyverse")
library(package = "crosstalk")


###CARGA DE DATOS EMORY Y UVG
# Pre-screening information (s0)
source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Load helper functions
source(file = "scripts/zz_output.R")
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

#sacar comunidades
cat_comunidades<-read_csv("data/dictionaries/cat_comunidades.csv")
comunidad_coord <- read_csv("data/exports/comunidad_coord.csv")
comunidades<-gt_participants %>% select(record_id,id_estudio, codigo=com_jalapa) %>%
  mutate(house_id=if_else(condition = grepl("^G[0-9]{4}",id_estudio),
                          true = record_id,
                          false = id_estudio
  )) %>%mutate(id_tamizaje=if_else(condition = grepl("^G[0-9]{4}",record_id),
                                   true = record_id,
                                   false = id_estudio
  )) %>% 
  left_join(cat_comunidades %>% mutate(codigo=as.character(codigo))) %>% select(id=house_id, id_tamizaje, comunidad, codigo)


#---------------------------------------------------------------------------------------
#reporte de S2 que tiene pendiente S4 con edad gestacional menor a 19 semanas y 6 dias
#---------------------------------------------------------------------------------------
gt_emory_data_arm1 %>% select(id_tamizaje=id, s2_date, s2_participate) %>% filter(!is.na(s2_date),s2_participate=="1") %>% 
  left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, s4_date, id_estudio=s4_main_id)
  ) %>% filter(is.na(s4_date)) %>% 
  left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, m17_date, m17_ga) %>% filter(!is.na(m17_date)) %>% mutate(
      conception = m17_ga - lubridate::days(280),
      ga_semanas_m17 = trunc(as.numeric(m17_date - conception, unit = "weeks")),
      ga_dias_m17= as.numeric(m17_date - conception, unit = "days")%%7,
      ga_semanas_today = trunc(as.numeric(Sys.Date() - conception, unit="weeks")),
      ga_dias_today= as.numeric(m17_date - conception, unit = "days")%%7
    )
  ) %>% mutate(
   edad_gestacional= paste(ga_semanas_today," semanas y ", ga_dias_today, "dias")
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, s1_date, s1_community_name) %>% filter(!is.na(s1_date))
  ) %>% #writexl::write_xlsx("output/s2_hasta_s4_sinfiltro.xlsx")
  filter(ga_semanas_today<=19 & ga_dias_today<=6) %>%  select(id_tamizaje, "Comunidad"=s1_community_name, "Fecha del S2"=s2_date, "Fecha del M17"=m17_date,"Edad gestacional:"= edad_gestacional, "Semanas:"= ga_semanas_today, "dias:"=ga_dias_today) %>% arrange(`Semanas:`) %>%  print(n=Inf) %>% 
  writexl::write_xlsx(paste("output/reporte_s2_hata_s4_pendiente",Sys.Date(),".xlsx"))


#--------------------------------------------------------------------------------------
#Reporte de S4 que tienen pendiente Linea Basal (M11)
#--------------------------------------------------------------------------------------

#identificar las salidas
salidas<-gt_emory_data_arm2 %>% select(id,e3_date) %>% filter(!is.na(e3_date)) %>%
  left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_o) %>% filter(!is.na(e3_date_o))
  ) %>%left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_c) %>% filter(!is.na(e3_date_c))
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
  ) %>%mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    )
  ) %>% mutate(
    sale = case_when(
      type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
      type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
      type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%filter(sale=="1")

#generar reporte de las que tienen S4 y tienen pendiente Linea Basal Clinica
gt_emory_data_arm1 %>%select(id_tamizaje=id, s4_date, id=s4_main_id) %>% filter(!is.na(id)) %>% anti_join(
  bind_rows(
    list(
      gt_emory_data_arm2 %>% select(id,m11_date, redcap_event_name) %>% filter(!is.na(m11_date) & redcap_event_name=="linea_de_base_arm_2") %>% select(id),
      gt_emory_data_arm2 %>% select(id, a21_date, redcap_event_name) %>% filter(!is.na(a21_date) & redcap_event_name=="linea_de_base_arm_2") %>% select(id),
      salidas %>% select(id)
    )
  )
)

gt_emory_data_arm1 %>% select(id_tamizaje=id, s4_date, id=s4_main_id) %>% filter(!is.na(id)) %>% 
left_join(
  gt_emory_data_arm2 %>% select(id,m11_date, redcap_event_name) %>% filter(!is.na(m11_date) & redcap_event_name=="linea_de_base_arm_2")
) %>% left_join(
  gt_emory_data_arm2 %>% select(id, a21_date, redcap_event_name) %>% filter(!is.na(a21_date) & redcap_event_name=="linea_de_base_arm_2")
) %>% 
  # quitar las salidas 
  left_join(
    salidas %>% select(id, sale)
  ) %>% filter(is.na(sale)) %>% 
  mutate(
        lb=if_else(
                  grepl("^33", id) & is.na(m11_date), "pendiente",""
                  ),
        lb=if_else(
                 grepl("^35", id) & is.na(a21_date), "pendiente", lb
                  )
            ) %>% filter(lb=="pendiente") %>% 
    select(id_tamizaje, id, "Fecha de S4"=s4_date, "Fecha de M11"=m11_date, "Fecha de A21"=a21_date, "Linea Basal"=lb) %>% 
      writexl::write_xlsx(paste("output/s4_pendientes_lb_",Sys.Date(),".xlsx"))
  

#generar reporte de las que tienen S4 y tienen pendiente Linea Exposición
gt_emory_data_arm1 %>% select(id_tamizaje=id, s4_date, id=s4_main_id) %>% filter(!is.na(id)) %>% 
  left_join(
    gt_emory_data_arm2 %>% select(id,m11_date, redcap_event_name) %>% filter(!is.na(m11_date) & redcap_event_name=="linea_de_base_arm_2")
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, a21_date, redcap_event_name) %>% filter(!is.na(a21_date) & redcap_event_name=="linea_de_base_arm_2")
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>% select(id,h42_date, redcap_event_name) %>% filter(!is.na(h42_date) & redcap_event_name=="linea_de_base_arm_2")
  ) %>%
  # quitar las salidas y abortos
anti_join(
  bind_rows(
    salidas %>% select(id),
    gt_emory_data_arm2 %>%
      filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
      select(id),
    gt_emory_data_arm2 %>% filter(!is.na(h42_date) & redcap_event_name=="linea_de_base_arm_2") %>% select(id)
  )
  ) %>% 
  mutate(
      lbe=if_else(
        is.na(h42_date),"pendiente", NA_character_
      )
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, m17_date, m17_ga) %>% filter(!is.na(m17_date)) %>% mutate(
      conception = m17_ga - lubridate::days(280),
      ga_semanas_m17 = trunc(as.numeric(m17_date - conception, unit = "weeks")),
      ga_dias_m17= as.numeric(m17_date - conception, unit = "days")%%7,
      ga_semanas_today = trunc(as.numeric(Sys.Date() - conception, unit="weeks")),
      ga_dias_today= as.numeric(m17_date - conception, unit = "days")%%7
    )
  ) %>% mutate(
    edad_gestacional= paste(ga_semanas_today," semanas y ", ga_dias_today, "dias")
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, s1_date, s1_community_name) %>% filter(!is.na(s1_date))
  ) %>% filter(lbe=="pendiente") %>% mutate(
    dias_s4=Sys.Date() - s4_date
  ) %>% arrange(desc(dias_s4)) %>% 
  select(id_tamizaje, id, "Fecha de S4"=s4_date, "Fecha de M11"=m11_date, "Fecha de A21"=a21_date, "Edad gestacional"= edad_gestacional, "Comunidad"=s1_community_name, "Linea Basal Exposicion"=lbe, "Dias desde S4"=dias_s4) %>% 
  writexl::write_xlsx(paste0("output/s4_pendientes_lb_exposicion",Sys.Date(),".xlsx"))









