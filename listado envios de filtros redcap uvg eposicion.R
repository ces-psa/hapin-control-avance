data_filtros<-read_csv("data/filtros/HAPINGuatemalaFiltro_DATA_2021-08-11_1140.csv")
data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% select(tracked_filter_date, matches("tracked_filter_id")) %>% gather(
  
  key=variable, value="value", -tracked_filter_date
) %>% filter(!is.na(value)) %>% select(fecha=tracked_filter_date, id_filtro=value) %>% 
  filter(as.Date(fecha)>="2021-05-20") %>% writexl::write_xlsx(
  "output/listado_envios_filtro_2021-05-20 al 2021-05-21.xlsx"
)

#listado filtros Alejandro
data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% select(tracked_filter_date, matches("tracked_filter_id")) %>% 
  gather(
  key=variable, value="value", -tracked_filter_date
) %>% filter(!is.na(value)) %>% select(fecha=tracked_filter_date, id_filtro=value) %>% 
arrange(desc(fecha)) %>% filter(fecha=="2021-07-02") %>% writexl::write_xlsx(
  paste0("output/listado_envios_filtros_2021-07-02.xlsx")
)

#revisar listado por ID para buscar errores
data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% filter(
  tracked_filter_date=="2021-03-10"
) %>% select( record_id, tracked_filter_date, matches("tracked_filter_id")) %>% print()
  #write_csv("output/revision_alejandro.csv")

#REVISION DE FILTROS B5 Y B4
#Leer datos de hapin II UVG
source(file = "scripts/0_get_hapinII_uvg.R", encoding = "UTF-8")

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




H41b_gt_files<- list.files(
  path = "data/exports", pattern = "HAPINIIGT_DATA_.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time)) %>%
  print()

h41b_gt_data<- H41b_gt_files %>%
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character())) %>%
  mutate_at(
    
    vars(matches("date")),
    list(as.Date)
  ) %>% mutate(
    #id=record_id
  ) %>% #select(-record_id) %>% 
  select( redcap_event_name, 
          id=record_id, everything()) %>% filter(redcap_event_name=="event_1_arm_1") %>%  filter(id!="99999") %>% print()



#datos de H41 B5
dt_filtros_h41_b5<-gt_hapin_II_data %>% filter(!is.na(h41_date)) %>%  select(id, redcap_event_name,  h41_date, h41_by,
                                                                             matches("^h41_kap1_ecm_f"),
                                                                             matches("^h41_kap2_ecm_f"),
                                                                             matches("^h41_sap_ecm_f"),
                                                                             matches("^h41_m_ecm_f"),
                                                                             matches("^h41_c_ecm_f"),
                                                                             matches("^h41_b_ecm_f"),
                                                                             matches("^h41_o_ecm_f")
)


#DATOS FILTROS EN H41
dt_filtros_h41<-h41_all_data %>% filter(!is.na(h41_date)) %>% select(id, redcap_event_name,  h41_date, h41_by,
                                                     matches("^h41_kap1_ecm_f"),
                                                     matches("^h41_kap2_ecm_f"),
                                                     matches("^h41_sap_ecm_f"),
                                                     matches("^h41_m_ecm_f"),
                                                     matches("^h41_c_ecm_f"),
                                                     matches("^h41_b_ecm_f"),
                                                     matches("^h41_o_ecm_f")
                                                     )
#filtros H41b de B5
dt_filtros_h41b_b5<-h41b_gt_data %>%  filter(!is.na(h41b_date)) %>% select(id, redcap_event_name, h41b_date, h41b_by,
                                                             matches("^h41b_filter")
                                                             
)

#DATOS FILTROS EN H41B
dt_filtros_h41b<-h41_all_data %>% filter(!is.na(h41b_date)) %>% select(id, redcap_event_name, h41b_date, h41b_by,
                                                      matches("^h41b_filter")
                                                      
                                                      )

#tabla de filtros H41 hapin 1
dt_filtros_h41 %>% filter(as.Date(h41_date)>="2021-03-01") %>% 
  select(id, fecha=h41_date, id_filtro=h41_kap1_ecm_fid) %>% filter(!is.na(id_filtro))




#revision filtros
dt_filtros_h41 %>% filter(redcap_event_name=="b4_arm_2") %>% 
  transmute(id, iniciales=h41_by, fecha=h41_date,
          id_filtro=h41_kap1_ecm_fid, ubicacion="cocina") %>% 
bind_rows(
  dt_filtros_h41 %>% filter(redcap_event_name=="b4_arm_2") %>% 
    transmute(
      id, iniciales=h41_by, fecha=h41_date
    )
)
  
#Inventario de Filtros
inventario_filtros<-data_filtros %>% filter(redcap_event_name=="registro_nuevos_fi_arm_1") %>% 
  select(new_filter_source, new_filter_size, matches("^new_filter_id_")) %>% 
  gather(key="variable", value = "value", -new_filter_size, -new_filter_source) %>% 
  filter(!is.na(value)) %>% select(lab_pesaje=new_filter_source,
                                   tipo_filtro=new_filter_size,
                                   id_filtro=value) %>% group_by(lab_pesaje, tipo_filtro) %>% count()

almacenamiento<-data_filtros %>% filter(redcap_event_name=="almacenamiento_arm_2") %>%  
  select(
    enviara=tracked_filter_location_new, matches("^tracked_filter_id_")
  ) %>% gather(
    key = "variable", value="value", -enviara
  ) %>% filter(!is.na(value)) %>% mutate(
    tipo=case_when(
      substr(value,1,3)=="3M3" ~ "37mm",
      substr(value, 1, 3)=="3M4" ~ "47mm",
      substr(value, 1, 3)=="3M5" ~ "15mm",
      substr(value, 1, 3)=="3P0" ~ "piloto",
      substr(value, 1, 3)=="3P4" ~ "piloto",
      substr(value, 1, 3)=="3P5" ~ "piloto",
      substr(value, 1, 3)=="3V5" ~ "15mm_uvg",
      substr(value, 1, 3)=="PLA" ~ "37mm",
      substr(value, 1, 3)=="UVG" ~ "15mm_uvg",
      TRUE ~ NA_character_
    )
  ) %>% filter(tipo!="piloto") %>% filter(tipo!="15mm_uvg") %>% select(
    destino=enviara, id_filtro=value, tipo
  ) %>% group_by(tipo, destino) %>% count()
    
envios<-data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% 
select(matches("tracked_filter_id_")) %>% gather(
  key="variable", value="value"
) %>% filter(!is.na(value)) %>% select(id_filtro=value) %>% mutate(
  tipo=case_when(
    substr(id_filtro,1,3)=="3M3" ~ "37mm",
    substr(id_filtro,1,3)=="3M4" ~ "47mm",
    substr(id_filtro,1,3)=="3M5" ~ "15mm",
    substr(id_filtro,1,3)=="3P0" ~ "piloto",
    substr(id_filtro,1,3)=="3P4" ~ "piloto",
    substr(id_filtro,1,3)=="3P5" ~ "piloto",
    substr(id_filtro,1,3)=="PLA" ~ "37mm",
    TRUE ~ NA_character_
  )
) %>%  group_by(tipo) %>% count()

#resumen conteo de filtros
list(
  "inventario"=inventario_filtros,
  "almacenados"=almacenamiento,
 "envios"= envios
) %>% writexl::write_xlsx("output/resumen_filtros.xlsx")

#conteo filtros faltantes
#envios
data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% 
  select(matches("tracked_filter_id_")) %>% gather(
    key="variable", value="value"
  ) %>% filter(!is.na(value)) %>% select(id_filtro=value) %>% mutate(
    tipo=case_when(
      substr(id_filtro,1,3)=="3M3" ~ "37mm",
      substr(id_filtro,1,3)=="3M4" ~ "47mm",
      substr(id_filtro,1,3)=="3M5" ~ "15mm",
      substr(id_filtro,1,3)=="3P0" ~ "piloto",
      substr(id_filtro,1,3)=="3P4" ~ "piloto",
      substr(id_filtro,1,3)=="3P5" ~ "piloto",
      substr(id_filtro,1,3)=="PLA" ~ "37mm",
      TRUE ~ NA_character_
    )
  ) %>% anti_join(
    
#almacenamiento
data_filtros %>% filter(redcap_event_name=="almacenamiento_arm_2") %>%  
  select(
    enviara=tracked_filter_location_new, matches("^tracked_filter_id_")
  ) %>% gather(
    key = "variable", value="value", -enviara
  ) %>% filter(!is.na(value)) %>% mutate(
    tipo=case_when(
      substr(value,1,3)=="3M3" ~ "37mm",
      substr(value, 1, 3)=="3M4" ~ "47mm",
      substr(value, 1, 3)=="3M5" ~ "15mm",
      substr(value, 1, 3)=="3P0" ~ "piloto",
      substr(value, 1, 3)=="3P4" ~ "piloto",
      substr(value, 1, 3)=="3P5" ~ "piloto",
      substr(value, 1, 3)=="3V5" ~ "15mm_uvg",
      substr(value, 1, 3)=="PLA" ~ "37mm",
      substr(value, 1, 3)=="UVG" ~ "15mm_uvg",
      TRUE ~ NA_character_
    )
  ) %>% filter(tipo!="piloto") %>% filter(tipo!="15mm_uvg") %>% select(
    destino=enviara, id_filtro=value, tipo
  ) %>% select(id_filtro)
) %>% group_by(tipo) %>% count()

#envios
data_filtros %>% filter(redcap_event_name=="envio_arm_2") %>% 
  select(matches("tracked_filter_id_")) %>% gather(
    key="variable", value="value"
  ) %>% filter(!is.na(value)) %>% select(id_filtro=value) %>% mutate(
    tipo=case_when(
      substr(id_filtro,1,3)=="3M3" ~ "37mm",
      substr(id_filtro,1,3)=="3M4" ~ "47mm",
      substr(id_filtro,1,3)=="3M5" ~ "15mm",
      substr(id_filtro,1,3)=="3P0" ~ "piloto",
      substr(id_filtro,1,3)=="3P4" ~ "piloto",
      substr(id_filtro,1,3)=="3P5" ~ "piloto",
      substr(id_filtro,1,3)=="PLA" ~ "37mm",
      TRUE ~ NA_character_
    )
  ) %>% anti_join(
    #inventario
    data_filtros %>% filter(redcap_event_name=="almacenamiento_arm_2") %>%  
      select(
        enviara=tracked_filter_location_new, matches("^tracked_filter_id_")
      ) %>% gather(
        key = "variable", value="value", -enviara
      ) %>% filter(!is.na(value)) %>% mutate(
        tipo=case_when(
          substr(value,1,3)=="3M3" ~ "37mm",
          substr(value, 1, 3)=="3M4" ~ "47mm",
          substr(value, 1, 3)=="3M5" ~ "15mm",
          substr(value, 1, 3)=="3P0" ~ "piloto",
          substr(value, 1, 3)=="3P4" ~ "piloto",
          substr(value, 1, 3)=="3P5" ~ "piloto",
          substr(value, 1, 3)=="3V5" ~ "15mm_uvg",
          substr(value, 1, 3)=="PLA" ~ "37mm",
          substr(value, 1, 3)=="UVG" ~ "15mm_uvg",
          TRUE ~ NA_character_
        )
      ) %>% filter(tipo!="piloto") %>% filter(tipo!="15mm_uvg") %>% select(
        destino=enviara, id_filtro=value, tipo
      )
  ) %>% group_by(tipo) %>% count()

#conteo uso de filtros en cocina
dt_filtros<-gt_hapin_II_data %>% filter(!is.na(h41_date)) %>%  select(id, h41_date, h41_by,
                                                         id_filtro=h41_kap1_ecm_fid,
                                                        ) %>% print()
dt_filtros_owa<-gt_hapin_II_data %>% filter(!is.na(h41_date_oaw)) %>% select(id, h41_date=h41_date_oaw,
                                                                             h41_by=h41_by_2_oaw,
                                                                            id_filtro=h41_kap1_ecm_fid_2) %>% 
  print()

dt_filtros %>% bind_rows(
  dt_filtros_owa
) %>% writexl::write_xlsx(paste0("output/reision_filtros_cocina_",Sys.Date(),".xlsx"))
dt_filtros
dt_filtros_h41b %>% group_by(h41b_filter3) %>% count() %>% filter(n>1)
