library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
#leer rds b6
rds_b6<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/ecm_b6.rds")
#rds_b6<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/emc_b6.rds")
# 
# rds_b6_clean<-rds_b6 %>% mutate(
#   id=str_extract(ORIGEN, "\\d{5}")
# ) 

#leer archivos h41
#buscar el mas reciente
file_h41<-list.files(
  path = "C:/Users/aramirez/ownCloud/Exposure_group/Export_Redcap/h41_h41b_h42_h43/", pattern = "HAPINIIGuatemala-H41H42H43H41b_DATA_.+csv", full.names = TRUE
) %>%  
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))

#cargar los datos
dt_h41<-read_csv(file_h41$file, cols(.default = "c"),show_col_types = FALSE)
#renombrer columnas
names(dt_h41) <- dt_h41[1,]
dt_h41<-dt_h41[-1,]

#crear hhid para rds ecm
rds_b6_clean<-rds_b6 %>% mutate(
  id=str_extract(ORIGEN, "\\d{5}")
)


#sacar lista de ids segun archivos ECM
rds_b6_clean<-rds_b6_clean %>% select(
  id, id_filtro=`header_Filter ID`, id_ecm=`header_Device Serial`
) %>% distinct()


dt_h41_b6<-dt_h41 %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% transmute(id=as.character(record_id), h41_date_v2,
                                                                                    c41_child_v2, 
                                                                                    id_ecm_h41=case_when(
                                                                                      str_length(h41_c_device)==3 ~  paste0("ECM00",h41_c_device),
                                                                                      str_length(h41_c_device)==2 ~  paste0("ECM000",h41_c_device),
                                                                                    )
                                                                                    ,id_filtro_h41=h41_c_filter) %>% 
  filter(!is.na(h41_date_v2)) %>% mutate(
    
  ) %>% filter(id!="99999")
#falta archivo ecm
h41_sin_ecm<-dt_h41_b6 %>% anti_join(
  rds_b6_clean %>% select(id)
)
#falta H41
ecm_sin_h41<-rds_b6_clean %>% anti_join(
  dt_h41_b6 %>% select(id)
)

revisar_ecm_filtro<-dt_h41_b6 %>% left_join(
  rds_b6_clean %>% select(id, id_filtro_ecm=id_filtro, id_ecm)
) %>% mutate(
  revisar_filtro=if_else(id_filtro_h41!=id_filtro_ecm,"Si",NA_character_),
  revisar_ecm=if_else(id_ecm!=id_ecm_h41,"Si",NA_character_)
) %>% filter(!is.na(revisar_filtro) | !is.na(revisar_ecm))

QC_completitud_ECM<-h41_sin_ecm %>% transmute(
  `Participant_ID`=id, fecha_h41=h41_date_v2, c41_child_v2, id_ecm=id_ecm_h41, id_filtro=id_filtro_h41, visit="b6", 
  Problem="Participant ID in REDCap with H41 data (device/filter ID) but without ECM-b6 file", 
  Solution=" "
) %>% bind_rows(
  ecm_sin_h41 %>% transmute(
    `Participant_ID`=id, id_ecm, id_filtro,visit="b6",
    Problem="Participant ID  with ECM files, but  without H41", 
    Solution=" "
  )
) %>% bind_rows(
  revisar_ecm_filtro %>% filter(!is.na(revisar_filtro)) %>% transmute(
    `Participant_ID`=id,fecha_h41=h41_date_v2, c41_child_v2,id_ecm=id_ecm_h41, id_filtro=id_filtro_h41, visit="b6",
    Problem=paste0("Filter id in redcap is: ", id_filtro, " but filter id in ecm files is: ", id_filtro_ecm ), 
    Solution=" "
  )
) %>% bind_rows(
  revisar_ecm_filtro %>% filter(!is.na(revisar_ecm)) %>% transmute(
    `Participant_ID`=id,fecha_h41=h41_date_v2, c41_child_v2,id_ecm, id_filtro=id_filtro_h41, visit="b6",
    Problem=paste0("ECM id in redcap is: ", id_ecm_h41, " but ECM id in ecm files is: ", id_ecm ), 
    Solution=" "
  )
)

#sacar ECM con duración menor a 21 horas y mayor a 27 horas
#set de datos para calcular duracion
rds_b6_duracion<-rds_b6 %>% mutate(
  id=str_extract(ORIGEN, "\\d{5}")
) %>% select(id, filter_id= `header_Filter ID`, 
             ecm_id= `header_Device Serial`, fecha=Date, hora= Time) %>% 
  group_by(
    id, ecm_id, filter_id, fecha ) %>% 
  transmute(
    min_hora=min(hora), max_hora=max(hora)
  ) %>% 
  group_by(id, ecm_id, filter_id, fecha, min_hora, max_hora) %>% count() %>% select(-n)

#convertir a formato fecha para calculos posteriores
rds_b6_duracion$fecha <-as.Date(rds_b6_duracion$fecha, format = "%d/%m/%Y")

#calcular la duración para cada ECM
rds_b6_duracion_calculada<-rds_b6_duracion %>% mutate(
  inicio=as.POSIXct(paste(fecha, min_hora), format = "%Y-%m-%d %H:%M:%S"),
  fin=as.POSIXct(paste(fecha, max_hora), format= "%Y-%m-%d %H:%M:%S")
)  %>%  mutate(
  duracion_dia=difftime(fin, inicio, units = "secs"),
) %>% group_by(id, ecm_id, filter_id) %>% summarize(
  inicio=min(inicio), fin=max(fin), duracion=sum(duracion_dia)
) %>% 
  mutate(
    duracion_horas = as.double(duracion) %/% 3600,
    # minutos = as.character((as.double(duracion) %% 3600) %/% 60),
    # segundos = as.character(as.double(duracion) %% 60)
  ) %>% ungroup() %>% select(-duracion)

#filtrar los que tuvieron duración mayor a 27 horas y menor a 21 horas
QC_duracion_ECM<-rds_b6_duracion_calculada %>% select(id,ecm_id, filter_id, inicio, fin, duracion_horas ) %>% filter(
  duracion_horas>27 | duracion_horas<21
) %>%  left_join(
  dt_h41_b6
)

# Sacar nota del tipo de apagado que registró el ECM
dt_tipo_apagado<-rds_b6 %>% mutate(id=str_extract(ORIGEN,"\\d{5}")) %>%
  select(id, ecm_id=`header_Device Serial`, filter_id=`header_Filter ID`,
         ShutDownReason)  %>%
  filter(!is.na(ShutDownReason)) %>% group_by(id, ecm_id, filter_id, ShutDownReason) %>% count() %>%  ungroup() %>% 
  group_by(id, ecm_id, filter_id) %>% summarize(tipo_apagado=paste(ShutDownReason, collapse = "; "))

#agregar tipo de apagado
QC_duracion_ECM<-QC_duracion_ECM %>% left_join(
  dt_tipo_apagado %>% ungroup() %>% select(id, ShutDownReason=tipo_apagado)
)

#armar el formato para exportar a Excel
QC_duracion_ECM<-QC_duracion_ECM %>% transmute(
  Participant_ID=id, fecha_H41=h41_date_v2 ,
  inicio, fin,
  duracion_horas, id_ecm=ecm_id, id_filtro=filter_id, 
  visit="b6", ShutDownReason, Problem="Duration major to 27 hours or less to 21 hours", Solution=""
) 



#mantener formato de fehca y hora
QC_duracion_ECM$inicio <- format(QC_duracion_ECM$inicio, format = "%Y-%m-%d %H:%M:%S")
QC_duracion_ECM$fin <- format(QC_duracion_ECM$fin, format = "%Y-%m-%d %H:%M:%S")


list(
  "QC_completitud_ECM"=QC_completitud_ECM,
  "QC_duracion_ECM"=QC_duracion_ECM
)%>% writexl::write_xlsx(paste0("output/QC_ecm_h41_b6_",Sys.Date(),".xlsx"))

  
