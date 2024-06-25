#leer datos de todos los filtros export H41_H41OAW_H41b_completos
library(package = "tidyverse")

# Get screening data from Emory export
h41_all_files <- list.files(
  path = "data/exports", pattern = "HAPINGuatemalaMainSt-H41H41OAWH41bcomplet_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


h41_all_data <- h41_all_files %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    #id=record_id
  ) %>% #select(-record_id) %>% 
  select( redcap_event_name, 
          id, everything()) %>% filter(id!="99999") %>% print()


##datos del Intensivo

# Get screening data from Emory export
h41_all_intensivo <- list.files(
  path = "data/exports", pattern = "HAPINGuatemalaExposu_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()


h41_intensivo <- h41_all_intensivo %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    id=record_id
  ) %>% select(-record_id) %>% 
  select( redcap_event_name, 
          id, everything()) %>% filter(id!="99999") %>% print()

h41_b5_files<- list.files(
  path = "data/exports", pattern = "HAPINIIGuatemala_DATA_.+csv", full.names = TRUE
) %>% 
  tibble(
    file=.,
    export_time = file %>% 
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>% 
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>% 
  slice(which.max(export_time)) %>% 
  print()

h41_b5 <- h41_b5_files %>% 
  pull(file) %>% 
  read_csv(col_types = cols(.default = col_character())) %>% 
  mutate_at(
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    id=record_id
  ) %>% select(-record_id) %>% 
  select(redcap_event_name, 
         id, everything()) %>% filter(id!="99999") %>% print()

#DATOS FILTROS EN H41
dt_filtros_h41<-h41_all_data %>% filter(!is.na(h41_date)) %>% select(id, redcap_event_name,  h41_date, h41_by,
                                                                     matches("^h41_kap1_ecm_id"),
                                                                     matches("^h41_kap1_ecm_f"),
                                                                     matches("^h41_kap2_ecm_id"),
                                                                     matches("^h41_kap2_ecm_f"),
                                                                     matches("^h41_sap_ecm_id"),
                                                                     matches("^h41_sap_ecm_f"),
                                                                     matches("^h41_m_ecm_id"),
                                                                     matches("^h41_m_ecm_f"),
                                                                     matches("^h41_c_ecm_id"),
                                                                     matches("^h41_c_ecm_f"),
                                                                     matches("^h41_b_ecm_id"),
                                                                     matches("^h41_b_ecm_f"),
                                                                     matches("^h41_o_ecm_id"),
                                                                     matches("^h41_o_ecm_f")
                                                                     
) %>% print()



#DATOS FILTROS EN H41 INTENSIVO
dt_filtros_h41_intensivo<-h41_intensivo %>% filter(!is.na(h41_date)) %>% select(id, redcap_event_name,  h41_date, h41_by,
                                                                     matches("^h41_kap1_ecm_id"),
                                                                     matches("^h41_kap1_ecm_f"),
                                                                     matches("^h41_kap2_ecm_id"),
                                                                     matches("^h41_kap2_ecm_f"),
                                                                     matches("^h41_sap_ecm_id"),
                                                                     matches("^h41_sap_ecm_f"),
                                                                     matches("^h41_m_ecm_id"),
                                                                     matches("^h41_m_ecm_f"),
                                                                     matches("^h41_c_ecm_id"),
                                                                     matches("^h41_c_ecm_f"),
                                                                     matches("^h41_b_ecm_id"),
                                                                     matches("^h41_b_ecm_f"),
                                                                     matches("^h41_o_ecm_id"),
                                                                     matches("^h41_o_ecm_f")
                                                                     
) %>% print()

#DATOS FILTROS EN H41 B5
dt_filtros_h41_b5<-h41_b5 %>% filter(!is.na(h41_date)) %>% select(id, redcap_event_name,  h41_date, h41_by,
                                               matches("^h41_kap1_ecm_id"),
                                               matches("^h41_kap1_ecm_f"),
                                               matches("^h41_kap2_ecm_id"),
                                               matches("^h41_kap2_ecm_f"),
                                               matches("^h41_sap_ecm_id"),
                                               matches("^h41_sap_ecm_f"),
                                               matches("^h41_m_ecm_id"),
                                               matches("^h41_m_ecm_f"),
                                               matches("^h41_c_ecm_id"),
                                               matches("^h41_c_ecm_f"),
                                               matches("^h41_b_ecm_id"),
                                               matches("^h41_b_ecm_f"),
                                               matches("^h41_o_ecm_id"),
                                               matches("^h41_o_ecm_f")
                                               
) %>% print()

#Armar tabla de H41 Main Study
h41_main_study<-dt_filtros_h41 %>% filter(!is.na(h41_kap1_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                             id_ecm=h41_kap1_ecm_id, id_filtro=h41_kap1_ecm_fid, tipo="cocina_principal") %>% 
  bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_kap1_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_kap1_ecm_id_2, id_filtro=h41_kap1_ecm_fid_2, tipo="owa_cocina_principal")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_kap2_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_kap2_ecm_id, id_filtro=h41_kap2_ecm_fid, tipo="cocina_secundaria")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_kap2_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_kap2_ecm_id_2, id_filtro=h41_kap2_ecm_fid_2, tipo="owa_cocina_secundaria")
  )%>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_sap_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_sap_ecm_id, id_filtro=h41_sap_ecm_fid, tipo="habitaciones")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_sap_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_sap_ecm_id_2, id_filtro=h41_sap_ecm_fid_2, tipo="owa_habitaciones")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_m_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                        id_ecm=h41_m_ecm_id, id_filtro=h41_m_ecm_fid, tipo="madre")
  )%>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_m_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_m_ecm_id_2, id_filtro=h41_m_ecm_fid_2, tipo="owa_madre")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_c_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_c_ecm_id, id_filtro=h41_c_ecm_fid, tipo="nino")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_c_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_c_ecm_id_2, id_filtro=h41_c_ecm_fid_2, tipo="owa_nino")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_b_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_b_ecm_id, id_filtro=h41_b_ecm_fid, tipo="blancos")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_b_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_b_ecm_id_2, id_filtro=h41_b_ecm_fid_2, tipo="owa_blancos")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_o_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_o_ecm_id, id_filtro=h41_o_ecm_fid, tipo="adulta")
  ) %>% bind_rows(
    dt_filtros_h41 %>% filter(!is.na(h41_o_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_o_ecm_id_2, id_filtro=h41_o_ecm_fid_2, tipo="owa_adulta")
  ) %>% mutate(proyecto="MainStudy")
  #writexl::write_xlsx("output/ecm_filtros_integrado.xlsx")

#Armar tabla H41 Intensivo


h41_intensivo<-dt_filtros_h41_intensivo %>% filter(!is.na(h41_kap1_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                   id_ecm=h41_kap1_ecm_id, id_filtro=h41_kap1_ecm_fid, tipo="cocina_principal") %>% 
 bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_kap2_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_kap2_ecm_id, id_filtro=h41_kap2_ecm_fid, tipo="cocina_secundaria")
  ) %>% bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_sap_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_sap_ecm_id, id_filtro=h41_sap_ecm_fid, tipo="habitaciones")
  )  %>% bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_m_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_m_ecm_id, id_filtro=h41_m_ecm_fid, tipo="madre")
  )%>%  bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_c_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_c_ecm_id, id_filtro=h41_c_ecm_fid, tipo="nino")
  ) %>% bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_b_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_b_ecm_id, id_filtro=h41_b_ecm_fid, tipo="blancos")
  ) %>% bind_rows(
    dt_filtros_h41_intensivo %>% filter(!is.na(h41_o_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_o_ecm_id, id_filtro=h41_o_ecm_fid, tipo="adulta")
  )  %>% mutate(proyecto="Intensivo")



hapinII_h41_b5<- dt_filtros_h41_b5 %>%  filter(!is.na(h41_kap1_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                id_ecm=h41_kap1_ecm_id, id_filtro=h41_kap1_ecm_fid, tipo="cocina_principal") %>% 
  bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_kap1_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                         id_ecm=h41_kap1_ecm_id_2, id_filtro=h41_kap1_ecm_fid_2, tipo="owa_cocina_principal")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_kap2_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                       id_ecm=h41_kap2_ecm_id, id_filtro=h41_kap2_ecm_fid, tipo="cocina_secundaria")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_kap2_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                         id_ecm=h41_kap2_ecm_id_2, id_filtro=h41_kap2_ecm_fid_2, tipo="owa_cocina_secundaria")
  )%>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_sap_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_sap_ecm_id, id_filtro=h41_sap_ecm_fid, tipo="habitaciones")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_sap_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                        id_ecm=h41_sap_ecm_id_2, id_filtro=h41_sap_ecm_fid_2, tipo="owa_habitaciones")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_m_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_m_ecm_id, id_filtro=h41_m_ecm_fid, tipo="madre")
  )%>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_m_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_m_ecm_id_2, id_filtro=h41_m_ecm_fid_2, tipo="owa_madre")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_c_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_c_ecm_id, id_filtro=h41_c_ecm_fid, tipo="nino")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_c_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_c_ecm_id_2, id_filtro=h41_c_ecm_fid_2, tipo="owa_nino")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_b_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_b_ecm_id, id_filtro=h41_b_ecm_fid, tipo="blancos")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_b_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_b_ecm_id_2, id_filtro=h41_b_ecm_fid_2, tipo="owa_blancos")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_o_ecm_fid)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                    id_ecm=h41_o_ecm_id, id_filtro=h41_o_ecm_fid, tipo="adulta")
  ) %>% bind_rows(
    dt_filtros_h41_b5 %>% filter(!is.na(h41_o_ecm_fid_2)) %>%  transmute(id, redcap_event_name, h41_date, h41_by, 
                                                                      id_ecm=h41_o_ecm_id_2, id_filtro=h41_o_ecm_fid_2, tipo="owa_adulta")
  ) %>% mutate(proyecto="HapinII")


#ecm y filtros H41 intensivo y estudio principal
h41_main_study %>% bind_rows(
 list(
   h41_intensivo,
   hapinII_h41_b5
 ) 
) %>% writexl::write_xlsx(paste0("output/h41_ecm_filtros_integrado_",Sys.Date(),".xlsx"))

#DATOS FILTROS EN H41B
dt_filtros_h41b<-h41_all_data %>% filter(!is.na(h41b_date)) %>% select(id, redcap_event_name, h41b_date, h41b_by,
                                                                       matches("^h41b_filter")
                                                                       
) 

#tabla de filtros H41 hapin 1
dt_filtros_h41 %>% 
  select(id, fecha=h41_date, id_filtro=h41_kap1_ecm_fid) %>% filter(!is.na(id_filtro)) %>% 
  filter(id_filtro=="3M52232")




#revision filtros
dt_filtros_h41 %>% filter(redcap_event_name=="b4_arm_2") %>% 
  transmute(id, iniciales=h41_by, fecha=h41_date,
            id_filtro=h41_kap1_ecm_fid, ubicacion="cocina") %>% 
  bind_rows(
    dt_filtros_h41 %>% filter(redcap_event_name=="b4_arm_2") %>% 
      transmute(
        id, iniciales=h41_by, fecha=h41_date
      )
  ) %>% print()
