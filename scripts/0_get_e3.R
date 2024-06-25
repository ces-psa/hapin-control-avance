library("tidyverse")

dt_e3<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  type=if_else(
    grepl("^35",id),
    "owa","pwg"
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% transmute(id, fecha_salida_madre=e3_date_exit, 
                                                               motivo_salida_madre=e3_reason, 
                                                               ultima_visita_madre=e3_last_visit) %>% 
    mutate(
      motivo_salida_madre=recode(motivo_salida_madre,"1"="Finalizacion estudio",
                                 "2"="No elegible",
                                 "3"="Retiro voluntario",
                                 "4"="Retirada por el equipo",
                                 "5"="Se mudo del area de estudio",
                                 "6"="Fallecio",
                                 "7"="Perdido durante el seguimiento",
                                 "8"="Madre: Aborto/ Aborto espontaneo, mortinato ,muerte del nino",
                                 "555"="Otro"),
      ultima_visita_madre=recode(
        ultima_visita_madre, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth", "5"="B1",
        "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% transmute(id, fecha_salida_adulta=e3_date_exit_o, 
                                                                 motivo_salida_adulta=e3_reason_o, 
                                                                 ultima_visita_adulta=e3_last_visit_o) %>% 
    mutate(
      motivo_salida_adulta=recode(motivo_salida_adulta,"1"="Finalizacion estudio",
                                  "2"="No elegible",
                                  "3"="Retiro voluntario",
                                  "4"="Retirada por el equipo",
                                  "5"="Se mudo del area de estudio",
                                  "6"="Fallecio",
                                  "7"="Perdido durante el seguimiento",
                                  "555"="Otro"),
      ultima_visita_adulta=recode(
        ultima_visita_adulta, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth","5"="B1",
        "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% transmute(id, fecha_salida_nino=e3_date_exit_c, 
                                                                 motivo_salida_nino=e3_reason_c, 
                                                                 ultima_visita_nino=e3_last_visit_c) %>% 
    mutate(
      motivo_salida_nino=recode(motivo_salida_nino,"1"="Finalizacion estudio",
                                "2"="No elegible",
                                "3"="Retiro voluntario",
                                "4"="Retirada por el equipo",
                                "5"="Se mudo del area de estudio",
                                "6"="Fallecio",
                                "7"="Perdido durante el seguimiento",
                                "555"="Otro"),
      ultima_visita_nino=recode(
        ultima_visita_nino, "0"="Elegibilidad", "1"="BL","2"="P1","3"="P2","4"="Birth", "5"="B1",
        "6"="B2", "7"="B3","8"="B4"
      )
    )
) %>% mutate(
  s6_arm=recode(s6_arm,"1"="Intervencion", "0"="Control")
)
