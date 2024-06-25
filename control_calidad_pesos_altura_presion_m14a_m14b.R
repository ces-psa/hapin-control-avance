

#REVISION DE PESOS, ALTURA Y DATOS DE PRESION DIASTOLICA EN M14a Y M14b
peso_mother<-gt_emory_data_arm2 %>% filter(!is.na(m14a_date)) %>%  select(id, m14a_date, m14a_by,m14_ht1, m14_ht2, m14_ht3,
                                                                          m14_wt1,m14_wt2,m14_wt3,
                                                                          redcap_event_name) %>%  
  filter(as.numeric(m14_ht1)<80 | as.numeric(m14_ht2)<80 | as.numeric(m14_ht3)<80  ) %>% 
  bind_rows(
    gt_emory_data_arm2 %>% filter(!is.na(m14a_date)) %>%  select(id, m14a_date, m14a_by,m14_ht1, m14_ht2, m14_ht3,
                                                                 m14_wt1,m14_wt2,m14_wt3,
                                                                 redcap_event_name) %>%  
      filter(as.numeric(m14_wt1)>100 | as.numeric(m14_wt2)>100 | as.numeric(m14_wt3)>100)
  )

peso_mother %>% full_join(
  gt_emory_data_arm2 %>% filter(!is.na(m14a_date)) %>%  select(id, m14a_date, m14a_by, m14_ht1, m14_ht2, m14_ht3,
                                                               m14_wt1,m14_wt2,m14_wt3,
                                                               redcap_event_name) %>% 
    filter(id=="35041" | id=="33450" | id=="33476" | id=="33529" | id=="35041")
) %>% arrange(id, redcap_event_name) %>% mutate(
  altura1=as.numeric(m14_ht1),
  altura2=as.numeric(m14_ht2),
  altura3=as.numeric(m14_ht3),
  peso1=as.numeric(m14_wt1),
  peso2=as.numeric(m14_wt2),
  peso3=as.numeric(m14_wt3),
  evento=recode(redcap_event_name, "p1_arm_2"="p1",
                "linea_de_base_arm_2"="linea_basal",
                "p2_arm_2"="p2")
) %>% select(-m14_ht1, -m14_ht2, -m14_ht3, -m14_wt1, -m14_wt2, -m14_wt3, -redcap_event_name) %>% writexl::write_xlsx("output/revision_m14a_2020-02-03.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,redcap_event_name,m14_dbp1,m14_dbp2,m14_dbp3) %>% 
  filter(as.numeric(m14_dbp1)<30 | as.numeric(m14_dbp2)<30|as.numeric(m14_dbp3)<30) %>% full_join(
    gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% 
      select(id, m14b_date,m14b_by,redcap_event_name,m14_dbp1,m14_dbp2,m14_dbp3) %>% 
      filter(id=="33067") 
  ) %>% transmute(id, m14b_date,m14b_by,diastolica1=m14_dbp1, diastolica2=m14_dbp2, diastolica3=m14_dbp3, evento=redcap_event_name  ) %>% 
  writexl::write_xlsx("output/revision_m14b_2020-02-03.xlsx")
