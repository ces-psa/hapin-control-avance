library("tidyverse")

# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")


complemento<-read_csv("D:/Descargas/MainStudyGT_DATA_2019-11-04_0952.csv")

#Conteo de salidas del estudio
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

# salida_after_s6 <- s6 %>% left_join(salidas) %>%
#   mutate(
#     dias_s6_e3=e3_date-s6_date
#   ) %>% filter(sale==1) %>% count()


gt_emory_data_arm1 %>% select(id, s4_date, record_id=s4_main_id) %>% filter(!is.na(record_id)) %>%  left_join(
  complemento %>% filter(redcap_event_name=="complemento_consen_arm_3") %>% 
        filter(!is.na(cc_date)) %>% select(record_id, fecha_complemento=cc_date, cc_iniciales, cc_accept)
) %>% left_join(
  salidas %>% select(record_id=id, salida=sale) 
) %>% mutate(
  Complemento_consentimiento= if_else(
    is.na(fecha_complemento), "pendiente", NA_character_
  )
) %>% filter(is.na(salida)) %>% filter(s4_date<'2019-02-18') %>% filter(is.na(fecha_complemento)) %>% select(
  "Id tamizaje"=id, "Id Estudio"=record_id, "Fecha de consentimiento (S4)"= s4_date, Complemento_consentimiento
) %>% writexl::write_xlsx(paste0("output/pendientes_complemento_consent_",Sys.Date(),".xlsx"))
  

complemento %>% select(record_id, s0_date) %>% filter(!is.na(s0_date)) %>% 
  filter(s0_date>='2019-09-15') %>% write_csv("output/ids_s0.csv")

gt_emory_data_arm1 %>% select(id,s1_date) %>% filter(!is.na(s1_date)) %>% 
  filter(s1_date>='2019-09-18') %>% write_csv("output/lista_s1.csv")



