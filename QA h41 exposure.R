h41_data<-read_csv("c:/temp/HAPINGuatemalaMainSt-H41completo_DATA_2020-12-11_0953.csv")
h41_data<-h41_data %>% mutate_all(as.character)


h41_data %>% select(id, redcap_event_name, h41_kap1_ecm_id, h41_kap1_ecm_fid, h41_kap2_ecm_id, h41_rap1_ecm_id) %>% mutate(
  visit=substr(redcap_event_name, 1,2)
) %>% mutate(visit=if_else(visit=="li", "baseline", visit))


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% group_by(type) %>% count()


gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="baseline") %>% select(id) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% group_by(type) %>% count()


h41_data<-h41_data %>% select(id, redcap_event_name, matches("h41_")  ) %>% mutate(
  visit=substr(redcap_event_name, 1,2),
  visit=if_else(visit=="li", "baseline", visit)
)

#sacar conteos de ECM madre
 gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% left_join(
  h41_data %>%  select(id, h41_m_ecm_id, h41_m_ecm_fid, visit) %>% 
    filter(visit=="b1") %>% filter(!is.na(h41_m_ecm_id))
) %>% filter(!is.na(h41_m_ecm_fid))
 
 

 h41_data %>%  select(id, h41_m_ecm_id, h41_m_ecm_fid, visit) %>% 
   filter(visit=="baseline") 

 
 
 c36_repeated<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2020-12-16_1658.csv")
 
 c36_repeated <- c36_repeated %>% filter(!is.na(c36a_date)) %>% select(id=record_id, redcap_event_name, matches("c36"))
 
 c36_repeated<-c36_repeated %>% mutate_all(as.character)
 
 c36_repeated %>% select(id, c36a_hhid, c36a_date, c36a_time, c36a_by) %>% print(n=Inf)

 c36_repeated %>% select(c36a_age ) %>% group_by(c36a_age) %>% count(c36a_age) %>% print(n=Inf)
 c36_repeated %>% filter(as.numeric(c36a_age)>=12) %>% select(id, c36a_hhid)
 
 
 c36_repeated %>% filter(!is.na(c36a_age)) %>% select(identificador=id, id=c36a_hhid, c36a_dob, c36a_age ) %>% left_join(
   gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
 ) %>% mutate(
   flag= if_else(
     as.Date(c36a_dob)==as.Date(c30_dob),"1", "0")
   ) %>% filter(flag=="0")
 
 c36_repeated %>% filter(!is.na(c36a_date)) %>% group_by(as.Date(c36a_date)) %>% count() %>% 
   print(n=Inf)
 
 c36_repeated
 
