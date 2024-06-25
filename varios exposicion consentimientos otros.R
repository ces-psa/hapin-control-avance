library("tidyverse")

intensivo<-read_csv("D:/Descargas/emory.csv")
consentimientos<-read_csv("D:/Descargas/uvg.csv")

intensivo %>% filter(!is.na(h41_date)) %>% mutate(record_id=as.character(record_id)) %>% group_by(record_id) %>% summarize(n=n()) %>% write_csv("output/hogares_intesivo.csv")



hogares<-intensivo %>% filter(!is.na(h41_date)) %>% mutate(record_id=as.character(record_id)) %>% group_by(record_id) %>% summarize(n=n())

hogares %>% left_join(
  consentimientos %>% filter(!is.na(ie_date)) %>% transmute(record_id=as.character(record_id), ie_date)
) %>% write_csv("output/lista_hogares_intensivo_exposicion.csv")

consentimientos %>% filter(!is.na(ie_date)) %>% transmute(record_id=as.character(record_id), ie_date) %>% left_join(
  hogares %>% select(record_id, n)
) %>% filter(is.na(n))


hercules_consentimiento %>% filter(h_consent_accept=="1") %>% select(id=record, fecha=h_date, iniciales=h_iniciales) %>% write_csv(
  "output/consentimientos_hercules.csv"
)

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(
  id=s4_main_id, fecha=s4_date, iniciales=s4_by
) %>% 
mutate(
  type = if_else(
    condition = grepl("^35[0-9]{3}", id),
    true = "oaw",
    false = "pw"
  ) 
  )%>% filter(type=="pw") %>% arrange(id) %>% writexl::write_xlsx("output/consentimientos_embarazada.xlsx")

gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(
  id=s4_main_id, fecha=s4_date, iniciales=s4_by
) %>% 
  mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    ) 
  )%>% filter(type=="oaw") %>% arrange(id) %>% writexl::write_xlsx("output/consentimientos_adulta.xlsx")
  



gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(
  
  id, c30_dob
  
) %>% filter(id=="35023")


ancilary<- read_csv("data/exports/HAPINGuatemalaExposu_DATA_2019-12-09_1537.csv")
#h41b_date, h42_date,

ancilary %>% select(id=record_id, h41_date,  redcap_event_name) %>%
mutate_all(as.character) %>% mutate(
  type = if_else(
    condition = grepl("^35[0-9]{3}", id),
    true = "oaw",
    false = "pw"
  ) 
)%>% mutate(fecha_h41=as.Date(h41_date), visita=substring(redcap_event_name, 1,4)) %>% group_by(redcap_event_name, type) %>% summarize(n=n(), ultima_fecha=max(fecha_h41))
