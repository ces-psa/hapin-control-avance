#libreria tydiverse
library(package="tidyverse")
#estraer la información de hisopados
source(file ="scripts/1_get_data_hisopados.R", encoding = "UTF-8")

#revisar los datos de cada CRF para unificar por el código de visita
#datos del CRF de toma de muestra
toma_hisopado<-c34h_toma_muestra_data %>% filter(!is.na(c34h_date)) %>% select(hhid=c34h_id,id_visita=c34h_visit_id,c34h_by, 
                                                                c34h_date,c34h_time,c34h_elegible,c34h_consentimiento,
                                                                c34h_hisopado,c34h_tipo_muestra,c34h_date_muestra,c34h_time_muestra,
                                                                c34h_date_muestra_refri,c34h_time_muestra_refri,c34h_date_muestra_lab,c34h_time_muestra_lab) %>% print()

#datos del CRF de procesamiento de muestra
procesamiento_muestra<-c34h_toma_muestra_data %>% filter(!is.na(c34h_date_procesa)) %>% mutate(record_id=substring(record,1,4)) %>% select(record_id,
                                                                c34h_date_procesa,c34h_time_procesa, c34h_by_procesa, id_visita=c34h_id_procesa,
                                                                c34h_date_muestra_procesa,c34h_time_lab_procesa,c34h_alicuota1,c34h_alicuota2,
                                                                c34h_alicuota3,c34h_date_storage_procesa,c34h_time_storage_procesa) %>% print()

#Registros que no tienen procesamiento de muestra, por ID de visita
toma_hisopado %>% anti_join(
  procesamiento_muestra %>% select(id_visita)
) %>% print()

#registros que solo tienen procesmiento de muestra y no tienen tomade muestra, por ID de visita
procesamiento_muestra %>% anti_join(
  toma_hisopado %>% select(id_visita)
)

#Revisar que no hayan ID visitas repeditos en ambos CRFs
toma_hisopado %>% group_by(id_visita) %>% count() %>% filter(n>1)
procesamiento_muestra %>% group_by(id_visita) %>% count() %>% filter(n>1)

hisopados_toma_muestra_y_procesamiento<-toma_hisopado %>% left_join(
  procesamiento_muestra
) 
# %>% writexl::write_xlsx("output/hisopados.xlsx")


#REVISION DE LUS
c34_data<-read_csv("D:/Descargas/PilotoVigilanciaNeum_DATA_2020-03-10_1125.csv")

c34_lus<-c34_data %>% filter(!is.na(c34_date)) %>% select(id=record_id, redcap_event_name,
                                                 c34_date, c34_by, id_visita=c34_id_visita ) %>% filter(!is.na(id_visita))

hisopados_toma_muestra_y_procesamiento %>% left_join(
  c34_lus %>% mutate(hhid_lus=id)
) %>% mutate(toma_muestra=if_else(!is.na(c34h_date),"Si","No"),
             procesamiento_muestra=if_else(!is.na(c34h_date_muestra_procesa), "Si","No"),
             LUS=if_else(is.na(hhid_lus),"No","Si")
             ) %>% select(hhid,id_visita,toma_muestra, procesamiento_muestra, LUS) %>% group_by(toma_muestra, procesamiento_muestra, LUS) %>% count() %>% 
  writexl::write_xlsx("output/hisopados_muestras_lus_freq.xlsx")

hisopados_toma_muestra_y_procesamiento %>% left_join(
  c34_lus %>% mutate(hhid_lus=id)
) %>% filter(is.na(hhid_lus)) %>% select(hhid)
