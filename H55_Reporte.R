# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

library("tidyverse")

gt_emory_data_arm2 %>% select(id, redcap_event_name, h55_date,h55_by, h55_type, h55_move_date, 
                         h55_return_date, h55_intend_return, h55_move_both, h55_o_type,
                         h55_o_move_date, h55_o_return_date, h55_o_intend_return) %>% 
  filter(!is.na(h55_date)) %>% mutate(
                                evento=case_when(
                                      redcap_event_name=="b1_arm_2" ~ "b1",
                                      redcap_event_name=="birth_arm_2" ~ "birth",
                                      redcap_event_name=="p1_arm_2" ~ "p1",
                                      redcap_event_name=="p2_arm_2" ~ "p2",
                                      TRUE ~ NA_character_
                                      )
  ) %>% filter(!is.na(h55_type)) %>% filter(h55_type=="1") %>% filter(is.na(h55_return_date)) %>% mutate(mudanza_temporal=if_else(
    h55_type=="1","Si","No"
  )
    
  ) %>% mutate(
    mudanza_temporal_adulta=if_else(
      h55_o_type=="1","Si","No"
    )
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, id=s4_main_id, s4_date) %>% filter(!is.na(id))
  ) %>% left_join(
    gt_emory_data_arm1 %>% select(id_tamizaje=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
  ) %>% mutate(
    fc= m17_ga - lubridate::days(280),
    ga_dias=Sys.Date() - fc,
    ga_semanas = as.numeric(ga_dias)%/%7,
    ga_d = as.numeric(ga_dias)%%7
    
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
  ) %>% mutate(
    edad_child = if_else(!is.na(c30_date),Sys.Date()-c30_date, NA_real_),
    semanas_child=if_else(as.numeric(edad_child)>7, as.numeric(edad_child)%/%7, 0),
    dias_child=if_else(as.numeric(edad_child)>7, as.numeric(edad_child)%%7, as.numeric(edad_child))
  ) %>% 
  #select(id,h55_date, fc, m17_ga, ga_semanas,ga_d, c30_date, edad_child, semanas_child,dias_child)
  select(id, evento, h55_date, mudanza_temporal,"fecha mudanza"=h55_move_date, "Fecha de retorno"=h55_return_date, 
           "fecha programada para retornar"=h55_intend_return, "Mudanza temporal Adulta"=mudanza_temporal_adulta,
           "Fecha mudanza Adulta"=h55_o_move_date, "Fecha de retorno Adulta"=h55_o_return_date, "Fecha programada retorno Adulta"=h55_o_intend_return
        ) %>% writexl::write_xlsx(paste("output/h55_mudanzas_sin_fecha_retorno_", Sys.Date(),".xlsx"))





