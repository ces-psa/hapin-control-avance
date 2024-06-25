library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
library(lubridate)
library(dplyr)
#leer rds b7
rds_b7<-readRDS("C:/GitHub_repo/qc_exposure_hapin/rds/rds_hapin_upas.rds")

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
rds_b7_clean<-rds_b7 %>% mutate(
  id=str_extract(ORIGEN, "\\d{5}")
)


#sacar lista de ids segun archivos ECM
rds_b7_clean<-rds_b7_clean %>% select(
  id, id_filtro=`header_Filter ID`, id_ecm=`header_Device Serial`
) %>% distinct()


dt_h41_b7<-dt_h41 %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% transmute(id=as.character(record_id), h41_date_v2,
                                                                                    c41_child_v2, 
                                                                                    id_ecm_h41=case_when(
                                                                                      str_length(h41_c_device)==3 ~  paste0("ECM00",h41_c_device),
                                                                                      str_length(h41_c_device)==2 ~  paste0("ECM000",h41_c_device),
                                                                                    )
                                                                                    ,id_filtro_h41=h41_c_filter) %>% 
  filter(!is.na(h41_date_v2)) %>% mutate(
    
  ) %>% filter(id!="99999")
#falta archivo ecm
h41_sin_ecm<-dt_h41_b7 %>% anti_join(
  rds_b7_clean %>% select(id)
)
#falta H41
ecm_sin_h41<-rds_b7_clean %>% anti_join(
  dt_h41_b7 %>% select(id)
)
revisar_ecm_filtro<-dt_h41_b7 %>% left_join(
  rds_b7_clean %>% select(id, id_filtro_ecm=id_filtro, id_ecm)
) %>% mutate(
  revisar_filtro=if_else(id_filtro_h41!=id_filtro_ecm,"Si",NA_character_),
  revisar_ecm=if_else(id_ecm!=id_ecm_h41,"Si",NA_character_)
) %>% filter(!is.na(revisar_filtro) | !is.na(revisar_ecm))

QC_completitud_ECM<-h41_sin_ecm %>% transmute(
  `Participant_ID`=id, fecha_h41=h41_date_v2, c41_child_v2, id_ecm=id_ecm_h41, id_filtro=id_filtro_h41, visit="B7", 
  Problem="Participant ID in REDCap with H41 data (device/filter ID) but without ECM-B7 file"
) %>% bind_rows(
  ecm_sin_h41 %>% transmute(
    `Participant_ID`=id, id_ecm, id_filtro,visit="B7",
    Problem="Participant ID  with ECM files, but  without H41", 
    Solution=" "
  )
) %>% bind_rows(
  revisar_ecm_filtro %>% filter(!is.na(revisar_filtro)) %>% transmute(
    `Participant_ID`=id,fecha_h41=h41_date_v2, c41_child_v2,id_ecm=id_ecm_h41, id_filtro=id_filtro_h41, visit="B7",
    Problem=paste0("Filter id in redcap is: ", id_filtro, " but filter id in ecm files is: ", id_filtro_ecm ), 
    Solution=" "
  )
) %>% bind_rows(
  revisar_ecm_filtro %>% filter(!is.na(revisar_ecm)) %>% transmute(
    `Participant_ID`=id,fecha_h41=h41_date_v2, c41_child_v2,id_ecm, id_filtro=id_filtro_h41, visit="B7",
    Problem=paste0("ECM id in redcap is: ", id_ecm_h41, " but ECM id in ecm files is: ", id_ecm ), 
    Solution=" "
  )
)

#sacar ECM con duración menor a 21 horas y mayor a 27 horas
#set de datos para calcular duracion
rds_b7_duracion<-rds_b7 %>% mutate(
  id=str_extract(ORIGEN, "\\d{5}")
) %>% select(id, filter_id=`header_Filter ID`, 
             ecm_id=`header_Device Serial`, fecha=Date, hora= Time) #%>% 


# Unimos las columnas Date y Time en una sola columna "DateTime"
rds_b7_duracion$DateTime <- as.POSIXct(paste(rds_b7_duracion$fecha, rds_b7_duracion$hora), format = "%d/%m/%Y %H:%M:%S")
rds_b7_duracion<-rds_b7_duracion %>% arrange(id, filter_id, DateTime)

# Calculamos la diferencia de tiempo entre cada medida y la siguiente
rds_b7_duracion$TimeDiff <- c(NA, diff(rds_b7_duracion$DateTime))

filtered_data <- rds_b7_duracion %>%
  filter(is.na(TimeDiff) | (TimeDiff <= 900 &  TimeDiff >0))

filtered_data_inicio_fin<-filtered_data %>% group_by(id,ecm_id, filter_id) %>% summarize(
  inicio=min(DateTime),
  fin=max(DateTime)
) %>% select(id, ecm_id, filter_id, inicio, fin)

filtered_data_inicio_fin$inicio <- as.POSIXct(filtered_data_inicio_fin$inicio, format = "%Y-%m-%d %H:%M:%S")
filtered_data_inicio_fin$final <- as.POSIXct(filtered_data_inicio_fin$final, format = "%Y-%m-%d %H:%M:%S")

# Calculamos la duración por día
Duration_b7 <- filtered_data %>% filter(!is.na(TimeDiff)) %>% 
  group_by(id, ecm_id, filter_id) %>%
  summarize(
    Duration = (sum(as.numeric(TimeDiff)))/3600)

Duration_b7<-Duration_b7 %>% left_join(
  filtered_data_inicio_fin
) %>% select(id, ecm_id, filter_id, inicio, fin, Duracion=Duration)


# Creamos el objeto de estilo para mantener el formato de fecha y hora
date_style <- createStyle(numFmt = "yyyy-mm-dd HH:mm:ss")

# Escribimos los datos en un archivo de Excel manteniendo el formato de fecha y hora
#write.xlsx(Duration_b7, "output/datos_con_fecha.xlsx", row.names = FALSE, styles = list("datetime" = date_style))




# #convertir a formato fecha para calculos posteriores
# rds_b7_duracion$fecha <-as.Date(rds_b7_duracion$fecha, format = "%d/%m/%Y")
# 
# #calcular la duración para cada ECM
# rds_b7_duracion_calculada<-rds_b7_duracion %>% mutate(
# inicio=as.POSIXct(paste(fecha, min_hora), format = "%Y-%m-%d %H:%M:%S"),
# fin=as.POSIXct(paste(fecha, max_hora), format= "%Y-%m-%d %H:%M:%S")
# )  %>%  mutate(
#   duracion_dia=difftime(fin, inicio, units = "secs"),
# ) %>% group_by(id, ecm_id, filter_id) %>% summarize(
#   inicio=min(inicio), fin=max(fin), duracion=sum(duracion_dia)
#                                           ) %>% 
#   mutate(
#   duracion_horas = as.double(duracion) %/% 3600,
#   # minutos = as.character((as.double(duracion) %% 3600) %/% 60),
#   # segundos = as.character(as.double(duracion) %% 60)
# ) %>% ungroup() %>% select(-duracion)

#filtrar los que tuvieron duración mayor a 27 horas y menor a 21 horas
QC_duracion_ECM<-Duration_b7 %>% select(id,ecm_id, filter_id, inicio, fin, Duracion ) %>% filter(
  Duracion>27 | Duracion<21
) %>%  left_join(
  dt_h41_b7
)

# Sacar nota del tipo de apagado que registró el ECM
dt_tipo_apagado<-rds_b7 %>% mutate(id=str_extract(ORIGEN,"\\d{5}")) %>%
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
  duracion_horas=Duracion, id_ecm=ecm_id, id_filtro=filter_id, 
  visit="B7", ShutDownReason, Problem="Duration major to 27 hours or less to 21 hours", Solution=""
) 


#mantener formato de fehca y hora
QC_duracion_ECM$inicio <- format(QC_duracion_ECM$inicio, format = "%Y-%m-%d %H:%M:%S")
QC_duracion_ECM$fin <- format(QC_duracion_ECM$fin, format = "%Y-%m-%d %H:%M:%S")

#agregar variables de H41

motivos_apagado<-dt_h41 %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% transmute(
  Participant_id=as.character(record_id), h41_date_v2,
  id_ecm_h41=case_when(
    str_length(h41_c_device)==3 ~  paste0("ECM00",h41_c_device),
    str_length(h41_c_device)==2 ~  paste0("ECM000",h41_c_device),
  ),
  id_filtro_h41=h41_c_filter , 
  estaba_apagado=recode(
    h41_c_stop_v2, "0"="No", "1"="Si"
  ), hora_apagado=h41_c_end_v2,
  h41_prob_v2___0=recode(h41_prob_v2___0,"1"= "Ninguno","0"=""),
  h41_prob_v2___1=recode(h41_prob_v2___1,"1"= "Filtro caído en área limpia","0"=""),
  h41_prob_v2___2=recode(h41_prob_v2___2,"1"= "	Filtro caído en área no limpia","0"=""),
  h41_prob_v2___3=recode(h41_prob_v2___3,"1"= "	filtro roto","0"=""),
  h41_prob_v2___4=recode(h41_prob_v2___4,"1"= "Filtro sobrecargado de tal manera que parte de la muestra se ha desprendido o se puede despegar","0"=""),
  h41_prob_v2___5=recode(h41_prob_v2___5,"1"= "Filtro contaminado de otro modo ","0"=""),
  h41_prob_v2___6=recode(h41_prob_v2___6,"1"= "	Muestreador dañado","0"=""),
  h41_prob_v2___7=recode(h41_prob_v2___7,"1"= "	Filtro perdido","0"=""),
  h41_prob_other_v2
) %>%  filter(!is.na(h41_date_v2)) %>%  filter(Participant_id!="99999")  %>% transmute(
  Participant_id, h41_date_v2, 
  id_ecm_h41,id_filtro_h41, estaba_apagado, hora_apagado,
  problema_manipulacion=paste0(
    h41_prob_v2___0,"  ", h41_prob_v2___1,"  ",h41_prob_v2___2,"  ",
    h41_prob_v2___3,"  ",
    h41_prob_v2___4,"  ",
    h41_prob_v2___5,"  ",
    h41_prob_v2___6,"  ",
    h41_prob_v2___7,"  ",
    h41_prob_other_v2
    
  )
)

QC_duracion_ECM<-QC_duracion_ECM %>% left_join(
  motivos_apagado %>% transmute(Participant_ID=as.character(Participant_id), 
                                id_ecm=id_ecm_h41, id_filtro=id_filtro_h41, estaba_apagado, 
                                hora_apagado, problema_manipulacion, Comentarios_personal_Jalapa=""
  )
) 


#QC validación id_ECM en nombre de archivo
QC_ecm_filtro_nombre_archivo<-rds_b7 %>% mutate(id=str_extract(ORIGEN,"\\d{5}")) %>%
  select(id, ecm_id_inside=`header_Device Serial`, filter_id_inside=`header_Filter ID`,
         ORIGEN)  %>% distinct() %>%   mutate(
           filter_id_outside=str_extract(ORIGEN, "\\d[Vv]\\d+"),
           ecm_id_outside=str_extract(ORIGEN, "[A-Za-z]{3}\\d{5}")
         ) %>% select(-ORIGEN) %>% mutate(
           Problem=case_when(
             ecm_id_inside != ecm_id_outside ~ "El id_ecm registrado dentro dle archivo no coincide con el id_ecm registrado en el nombre del archivo",
             filter_id_inside != filter_id_outside  ~ "El id_filtro registrado dentro del archivo no coincide con el id_filtro registrado en el nombre del archivo"
           )
         ) %>% filter(!is.na(Problem)) %>% left_join(
           dt_h41_b7 %>% select(-c41_child_v2)
         ) %>% transmute(
           id, fecha_h41=h41_date_v2, id_ecm_h41, id_filtro_h41,
           id_ecm_nombre_archivo=ecm_id_outside, id_filter_nombre_archivo=filter_id_outside,
           id_ecm_dentro_archivo=ecm_id_inside, id_filter_dentro_archivo=filter_id_inside,
           Problem, Solution=""
         )



list(
  "QC_completitud_ECM"=QC_completitud_ECM,
  # "Revision_duracion_fuera_Rango"=QC_duracion_ECM,
  "QC_nombre_archivo"=QC_ecm_filtro_nombre_archivo
)%>% writexl::write_xlsx(paste0("output/QC_ecm_h41_b7_",Sys.Date(),".xlsx")) 
#write.xlsx(., "output/datos_con_fecha.xlsx", row.names = FALSE, styles = list("datetime" = date_style))


