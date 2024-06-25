# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
# Emory RedCap export data
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

library("tidyverse")

gt_emory_data_arm2 %>% select(id, h42_date, redcap_event_name) %>% filter(!is.na(h42_date), redcap_event_name=="linea_de_base_arm_2") %>% anti_join(
  salidas %>% select(id)
) %>% 
  left_join(
    gt_emory_data_arm2 %>% select(id, b10_date, redcap_event_name) %>% filter(!is.na(b10_date), redcap_event_name=="linea_de_base_arm_2")
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, id=s4_main_id) %>% filter(!is.na(id))
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id,s1_date, s1_community_name) %>% filter(!is.na(s1_date))
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, s6_date) %>% filter(!is.na(s6_date))
  ) %>% filter(is.na(s6_date)) %>% mutate(
    dias_h42_hoy=Sys.Date()- h42_date,
    limite_s6= h42_date + lubridate::days(7)
  )%>% mutate(
    revisar=if_else(condition = dias_h42_hoy>7,"Revisar","")
  ) %>% 
  select(id_tamizaje, "House id"=id, Comunidad=s1_community_name, 
         "Fecha H42"=h42_date, "Fecha B10"=b10_date,
         "Fecha Limite para S6"=limite_s6, "Dias transcurridos desde H42"=dias_h42_hoy, "Tiempo Vencido"=revisar ) %>% writexl::write_xlsx(paste("output/pendientes_s6_",Sys.Date(),".xlsx"))


gt_emory_data_arm1 %>%
  filter(!is.na(s4_main_id)) %>%
  select(id=s4_main_id, s4_date) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(m10_date) | !is.na(h41_date) | !is.na(h42_date) | !is.na(s6_date), visit == "baseline") %>%
      select(id, m10_date, h41_date, h42_date, s6_date)
  ) %>%
  filter(is.na(s6_date)) %>%
  mutate(
    days_no_rand = case_when(
      !is.na(s6_date) ~ NA_real_,
      !is.na(h41_date) ~ as.numeric(Sys.Date() - h41_date, unit = "days"),
      !is.na(m10_date) ~ as.numeric(Sys.Date() - m10_date, unit = "days"),
      !is.na(s4_date) ~ as.numeric(Sys.Date() - s4_date, unit = "days"),
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(desc(days_no_rand)) %>%
  anti_join(
    salidas %>% select(id)
  ) %>% 
  print(n = Inf)

