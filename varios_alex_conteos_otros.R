library("tidyverse")
complemento<-read_csv("D:/Descargas/MainStudyGT_DATA_2019-08-30_0932.csv")
intensivo<- read_csv("D:/Descargas/HAPINGuatemalaExposu_DATA_2019-08-30_1002.csv")

consentimientos<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("it_tamizaje"=id, "id"=s4_main_id, s4_date, s4_by)

complemento<-complemento %>% filter(!is.na(cc_date)) %>% select("id"=record_id, cc_date, cc_iniciales)

intensivo<-intensivo %>% filter(!is.na(ie_date)) %>% mutate(id=as.character(record_id)) %>%  select(id, ie_date, ie_initials)

consentimientos %>% left_join(
  complemento
) %>% left_join(
  intensivo
) %>% select(
  "ID"=id,
  "Fecha_consentimiento"= s4_date,
  "inciales consentimiento"=s4_by,
  "Fecha_complemento_consentimiento"=cc_date,
  "iniciales_complemento_consentimiento"=cc_iniciales,
  "Fecha_consentimiento_intensivo"=ie_date,
  "iniciales_consentimiento_intensivo"=ie_initials
) %>% writexl::write_xlsx("output/lista_consentimientos.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date,h50_traditional) %>% filter(h50_traditional=="5")

gt_emory_data_arm2 %>% filter(!is.na(e5_date))

con_dot<-gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% select(id, h40_date) %>% group_by(id) %>% summarize(n=n())
gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date,h50_traditional) %>% filter(h50_traditional=="5") %>% left_join(
  con_dot
) %>% filter(!is.na(n))

241 hogares con estufa destruida
120 no tienen dots
131 tienen dots 

gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_m_ecm_other) %>% filter(!is.na(h41_m_ecm_other))


gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, visit) %>% anti_join(
  salidas %>% select(id)
) 

d_visitas<-gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, visit) %>% anti_join(
  salidas %>% select(id)
) %>% select(id, visitas=visit)


  total_visitas<-gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, visit) %>% anti_join(
    salidas %>% select(id)
  ) %>% filter(visit=="baseline") %>%  select(id, visit, visitas=visit) %>% complete(nesting(id),
    visitas=c("baseline","p1","p2","b1","b2","b3","b4")
  ) %>% arrange(id)

  total_visitas %>% left_join(
    d_visitas %>% transmute(id,visitas, realizada="Si")
  ) %>% mutate(orden=case_when(
    visitas=="baseline" ~ "1",
    visitas=="p1" ~ "2",
    visitas=="p2" ~ "3",
    visitas=="b1" ~ "4",
    visitas=="b2" ~ "5",
    visitas=="b3" ~ "6",
    visitas=="b4" ~ "7",
    TRUE ~ ""
  )) %>% write_csv("output/temp/visitas.csv")
  
  
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id=s4_main_id) %>% anti_join(
    salidas %>% select(id)
  ) %>% mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    )
  ) %>% filter(type=="oaw") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% transmute(id,fecha_baseline=a24a_date, visit) %>% filter(visit=="baseline") %>% anti_join(
      salidas %>% select(id)
    )
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% transmute(id,fecha_p1=a24a_date, visit) %>% filter(visit=="p1") %>% anti_join(
      salidas %>% select(id)
    )
  ) 
  
  gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(a24a_date)) %>% select(id)
  
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% 
    left_join(
      gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id,fpp=m17_ga)
    ) %>% anti_join(
      salidas
    ) %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id,c30_dob)
    ) %>% mutate(
      ya_nacio=if_else(is.na(c30_dob),"No","Si")
    ) %>% mutate(
      fecha_nacimiento=if_else(is.na(c30_dob),as.character(fpp), as.character(c30_dob))
    ) %>% filter(fecha_nacimiento>="2019-09-15") %>% select(id_tamizaje, id, ya_nacio, fecha_nacimiento) %>% write_csv("output/nacimientos.csv")
                 
  gt_emory_data_arm2 %>% filter(!is.na(m10_date) & visit=="baseline") %>% select(id, m10_date) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(a27_date)) %>% select(id, a27_date)
  ) %>% mutate(
    type=if_else(
      grepl("^35[0-9]{3}",id),"oaw","pw"
    )
  ) %>% filter(type=="oaw") %>% mutate(
    diferente=if_else(m10_date!=a27_date,"diferente", NA_character_)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id, s4_date)
  ) %>% transmute(id_tamizaje, id, consentimiento=s4_date, m10_date, a27_date, 
    diferencia_dias=a27_date-s4_date
  ) %>% write_csv("output/s4_a27_diferencia.csv")
