#libreria
library("tidyverse")

# buscar el archivo mas reciente de repeated crfs
repeats_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaRepeat_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaRepeat_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

repeats <- read_csv(
  file = repeats_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()

#sacar los datos de c36 que hay en Emory repeated CRF
c36a_emory<- repeats %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, matches("^c36a")) %>% filter(!is.na(c36a_hhid))
c36a_emory<-c36a_emory %>% mutate(hhid=c36a_hhid, id=c36a_visit) %>% select(id,hhid,1:98)

#leer los datos de hisopados que estan en redcap uvg
# buscar el archivo mas reciente de repeated crfs
repeats_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaRepeti_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaRepeti_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

data_hisopados <- read_csv(
  file = repeats_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()
#datos del brazo de hisopados
data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
  select(id, redcap_event_name, matches("^c34h_")) %>% mutate_all(as.character)

#Datos del crf toma de muestra
toma_muestra<-data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
                      select(id, redcap_event_name, matches("^c34h_")) %>% mutate_all(as.character) %>% 
  filter(!is.na(c34h_id)) %>% select(-matches("_procesa"), -matches("alicuota")) %>% 
  select(id_crf=id, id_visita=c34h_visit_id,hhid=c34h_id, fec_toma_muestra=c34h_date, iniciales_toma_muestra=c34h_by,
         elegible_toma_muestra=c34h_elegible, consentimiento_toma_muestra=c34h_consentimiento,
         hisopado_toma_muestra=c34h_hisopado, tipo_toma_muestra=c34h_tipo_muestra,
         fecha_toma_muestra=c34h_date_muestra, hora_toma_muestra=c34h_time_muestra,
         fecha_muestra_refrigerador=c34h_date_muestra_refri, hora_muestra_refrigerador=c34h_time_muestra_refri,
         fecha_muestra_lab=c34h_date_muestra_lab, hora_muestra_lab=c34h_time_muestra_lab) %>% print()
    
                    

#Datos del crf toma procesamiento de muestra
procesamiento_muestra<-data_hisopados %>% filter(redcap_event_name=="toma_muestra_arm_4") %>%
  select(id, redcap_event_name, matches("^c34h_")) %>% select(id_crf_procesa=id, 
                                                              id_visita=c34h_id_procesa,
                                                              fecha_procesa_muestra=c34h_date_procesa,
                                                              hora_procesa_muestra=c34h_time_procesa,
                                                              iniciales_procesa_muestra=c34h_by_procesa,
                                                              fecha_procesa_muestra_lab=c34h_date_muestra_procesa,
                                                              hora_procesa_muestra_lab=c34h_time_lab_procesa,
                                                              c34h_alicuota1,
                                                              c34h_alicuota2,
                                                              c34h_alicuota3,
                                                              fecha_almacenamiento_muestra_lab=c34h_date_storage_procesa,
                                                              hora_almacenamiento_muestra_lab=c34h_time_storage_procesa,
                                                              Resultado_fluA=c34h_result_alic1_flua,
                                                              Resultado_flua_ct=c34h_result_alic1_flua_ct,
                                                              Subtipo_fluA=c34h_result_alic1_subtipo,
                                                              ResultadoFluB=c34h_result_alic1_flub,
                                                              Resultado_RSV=c34h_result_alic1_rsv,
                                                              Resultado_hMPV=c34h_result_alic1_hmpv
                                                              ) %>% filter(
    !is.na(id_visita)
  ) %>% filter(is.na(Resultado_fluA))
  print()

#unificación de datos
# toma y procesamiento
toma_muestra %>% write_csv("output/toma_muestras.csv")
  procesamiento_muestra %>% write_csv("output/procesamiento.csv")
dt_toma_resultados<-toma_muestra %>% left_join(
  procesamiento_muestra
)

c36a_emory %>% select(id_visita=id, id_hogar=hhid, -c36a_hhid, 1:99) %>%   left_join(
  dt_toma_resultados 
) %>% filter(!is.na(fec_toma_muestra)) %>% writexl::write_xlsx("output/datos_rsv.xlsx")

data_piloto<-read_csv("C:/Users/aramirez/Downloads/PilotoVigilanciaNeum_DATA_2021-03-02_1747.csv")
data_piloto<-data_piloto %>% mutate_all(as.character)
data_piloto %>% filter(!is.na(c34_date)) %>% select(id=record_id, redcap_event_name, c34_date, c34_by, c34_id_visita)
data_piloto %>% filter(!is.na(today)) %>% filter(grepl("^33", record_id) | grepl("^35", record_id)) %>% select(id=record_id, today) %>% arrange(desc(id))

repeats %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, id=c36a_hhid, c36a_date) %>% filter(!is.na(c36a_date)) %>% 
  left_join(
    
  )
gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% select(id, c36_date)

repeats %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id, id=c34a_hhid, c34a_date) %>% filter(!is.na(c34a_date))




#revision de datos c34 piloto vrs muestras

data_piloto %>% filter(!is.na(c34_date)) %>%  select(id=record_id, c34_date, c34_by, c34_start) %>% left_join(
  data_hisopados %>% select(id, redcap_event_name)
) %>% filter(!is.na(redcap_event_name))
