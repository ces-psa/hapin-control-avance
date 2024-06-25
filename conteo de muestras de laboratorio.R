library("tidyverse")
data_muestras<-read_csv("d:/descargas/HAPINGuatemalaLab_DATA_2020-04-27_1132.csv")
data_muestras<-read_csv("data/exports/HAPINGuatemalaLab_DATA_2022-02-10_1318.csv")
muestras_enviadas<-read_csv("d:/descargas/muestras_enviadas.csv")


muestras_redcap<-data_muestras %>% select(alicuot_storage_date,starts_with("aliquot_id")) %>% 
  gather(key=variable, value=value, -alicuot_storage_date) %>% filter(!is.na(value)) %>% transmute(fecha=alicuot_storage_date, id_muestra=value,type="tubos") %>% 
    bind_rows(
      data_muestras %>% select(card_storage_date,starts_with("card_id")) %>% 
        gather(key=variable, value=value, -card_storage_date) %>% filter(!is.na(value)) %>% transmute(fecha=card_storage_date, id_muestra=value,type="tarjetas")
            )

#muestras enviadas que no estan registradas en redcap
muestras_enviadas %>% anti_join(
  muestras_redcap %>% select("send_muestra"=id_muestra)
) %>% write_csv("output/enviadas_no_redcap.csv")

#sacar lista de muestras que registradas en redcap
muestras_gt<-muestras_redcap %>% anti_join(
  muestras_enviadas %>% select("id_muestra"=send_muestra)
) %>% filter(!grepl("{9}-U[5-7]",id_muestra))

muestras_redcap %>% mutate(id=substr(id_muestra,1,5 )) %>% filter(id=="33074")


#Sacar lista de contenedores donde se guardaron las muestras
contenedores<-data_muestras %>% select(aliquot_container_id,starts_with("aliquot_id")) %>% 
  gather(key=variable, value=value, -aliquot_container_id) %>% 
    filter(!is.na(value)) %>% transmute(contenedor=aliquot_container_id, id_muestra=value,type="tubos")%>% 
  bind_rows(
    data_muestras %>% select(card_container_id,starts_with("card_id")) %>% 
      gather(key=variable, value=value, -card_container_id) %>% filter(!is.na(value)) %>% transmute(contenedor=card_container_id, id_muestra=value,type="tarjetas")
  )
#Ubicar muestras sin identificador de Bag, para Luri
contenedores %>% filter(id_muestra=="35024-C4-X0" | id_muestra=="33055-C5-X0" | id_muestra=="33133-C4-X0" 
                          | id_muestra=="33074-C4-X0" | id_muestra=="35046-C4-X0" | id_muestra=="33009-C5-X0" | id_muestra=="33027-C5-X0" ) %>% writexl::write_xlsx("output/ubicar_muestras_luri.xlsx")

#quitar lista de muestras que fueron enviadas a Guate, contenedores depurados
contenedores_jalapa<-contenedores %>% anti_join(
  muestras_enviadas %>% select("id_muestra"=send_muestra)
) %>% filter(!grepl("{9}-U[5-7]",id_muestra))

#averiguar contenedor para una muestra en redcap
contenedores %>% filter(id_muestra=="33407-M2-X0")

muestras_gt %>% bind_cols(contenedores) %>% write_csv("output/muestras_contenedores.csv")
#lista de muestras y contenedores redcap en Jalapa y UVG
muestras_contenedor<-muestras_gt %>% bind_cols(contenedores) %>% select(fecha, contenedor, id_muestra, type)

muestras_contenedor %>% filter(contenedor=="Bag-0146") %>% group_by(fecha) %>% summarize(contador=n())

#data_muestras %>% filter(card_container_id=="33189-M3-X0") %>% select(correlativo, card_container_id) %>% write_csv("output/containers5.csv")
cont_sangre<-data_muestras %>%group_by(card_container_id) %>% summarize(contador=n())
cont_tubos<-data_muestras %>%group_by(aliquot_container_id) %>% summarize(contador=n())

id_contenedores<-cont_sangre %>% select("id_contenedor"=card_container_id ) %>% bind_rows(
  cont_tubos %>% select("id_contenedor"=aliquot_container_id)
)

enviados<-data_muestras %>% select(shipment_date,starts_with("container_id_"))%>% 
  gather(key=variable, value=value, -shipment_date) %>% 
  filter(!is.na(value)) %>% transmute(fecha=shipment_date, id_containers=value)

enviados %>% anti_join(
  id_contenedores %>% select("id_containers"=id_contenedor)
)

#lista de contenedores y bolsas que estan en Jalapa
contenedores_jalapa<-id_contenedores %>% select("id_containers"=id_contenedor) %>% anti_join(
  enviados
) %>% filter(id_containers!="33189-M3-X0" & id_containers!="33184-M3-X0")

#lista de contenedores y bolsas enviados a UVG
contenedores_uvg<-enviados

muestras_uvg<-muestras_contenedor %>% anti_join(
  contenedores_jalapa %>% select("contenedor"=id_containers)
) %>% mutate(ubicacion="UVG")

muestras_contenedor %>% left_join(
  muestras_uvg %>% select(id_muestra, ubicacion)
) %>% mutate(ubicacion=if_else(
  is.na(ubicacion),"Jalapa", ubicacion
)) %>% group_by(contenedor, ubicacion, type) %>% summarise(contador=n(), muestra_antigua=min(fecha) ) %>% write_csv("output/ubicacion_muestras.csv")

muestras_redcap %>% filter(id_muestra=="35135-M1-U3")

contenedores %>% filter(id_muestra=="35135-M1-U3")

#REVISION DE ETIQUETAS LAB JALAPA
revisar<-read_csv("D:/Descargas/revision_etiquetas_lab.csv")
revisar<-revisar %>% mutate_all(as.character)

muestras_redcap %>% mutate(id=substr(id_muestra,1,5 )) %>% filter(id=="35032") %>% arrange(fecha) %>%  print(n=Inf)

revisar %>% select(id_estudio=`HH ID`, visita=Visit, Participant, tipo_muestra=`Sample type`, QR=Etiquetas, id_muestra=buscar) %>% left_join(
  muestras_redcap
) %>% write_csv("output/revision_muestras_lab.csv")


#REVISION DE MUESTRAS CONTROL DE CALIDAD B10 ERICK
muestras_redcap<-data_muestras %>% select(alicuot_storage_date,starts_with("aliquot_id")) %>% 
  gather(key=variable, value=value, -alicuot_storage_date) %>% filter(!is.na(value)) %>% transmute(fecha=alicuot_storage_date, id_muestra=value,type="tubos") %>% 
  bind_rows(
    data_muestras %>% select(card_storage_date,starts_with("card_id")) %>% 
      gather(key=variable, value=value, -card_storage_date) %>% filter(!is.na(value)) %>% transmute(fecha=card_storage_date, id_muestra=value,type="tarjetas")
  )

data_muestras %>% select(card_storage_date,starts_with("card_id"),starts_with("blood_spots_valid"), starts_with("blood_spots_invalid") ) %>% 
      gather(key=variable, value=value, -card_storage_date) %>% filter(!is.na(value)) %>% 
  transmute(fecha=card_storage_date, 
id_muestra=case_when(
  substr(variable,1,7)=="card_id" ~ value
  ),
gotas_validas=case_when(
  substr(variable,1,13)=="blood_spots_v" ~ value
) ,
gotas_no_validas=case_when(
  substr(variable,1,14)=="blood_spots_in" ~ value
) 
) %>% spread(., key=fecha)
# starts_with("blood_spots_valid"), starts_with("blood_spots_invalid")
# 
# data_muestras %>% write_csv("output/datos_muestras.csv")
# 
# ,
# id_muestra=case_when(
#   substr(variable,1,7)=="card_id" ~ value
# ),
# gotas_validas=case_when(
#   substr(variable,1,13)=="blood_spots_v" ~ value
# ) ,
# gotas_no_validas=case_when(
#   substr(variable,1,14)=="blood_spots_in" ~ value
# ) 

#detectar id_muestra de sangre que se encuentran ingresadas dos veces
data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
  select(starts_with("card_id")) %>% gather(key = variable, value = value) %>% filter(!is.na(value)) %>% group_by(value) %>% count() %>% filter(n>1) %>% left_join(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_storage_date, card_container_id, starts_with("card_id")) %>% gather(key=variable, value=value, -card_storage_date, -card_container_id) %>% 
      filter(!is.na(value)) 
  ) %>% writexl::write_xlsx("output/revision-muestras-ingreso_doble.xlsx")
         

#integracion manual de IDs y cantidad de gotas validas e invalidas
#INTEGRAR MUESTRAS DE SANGRE
#-------------------------------------------------------------------------
muestras_sangre_integradas<-data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
  select(card_container_id,card_id_001, blood_spots_valid_001, blood_spots_invalid_001 ) %>% filter(!is.na(card_id_001)) %>% transmute(
    contenedor=card_container_id, id_muestra=card_id_001, circulos_validos=blood_spots_valid_001, circulos_no_validos=blood_spots_invalid_001, posicion=001) %>% 
  bind_rows(
data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
  select(card_container_id,card_id_002, blood_spots_valid_002, blood_spots_invalid_002 ) %>% filter(!is.na(card_id_002)) %>% transmute(
    contenedor=card_container_id, id_muestra=card_id_002, circulos_validos=blood_spots_valid_002, circulos_no_validos=blood_spots_invalid_002, posicion=002)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_003, blood_spots_valid_003, blood_spots_invalid_003 ) %>% filter(!is.na(card_id_003)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_003, circulos_validos=blood_spots_valid_003, circulos_no_validos=blood_spots_invalid_003, posicion=003)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_004, blood_spots_valid_004, blood_spots_invalid_004 ) %>% filter(!is.na(card_id_004)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_004, circulos_validos=blood_spots_valid_004, circulos_no_validos=blood_spots_invalid_004, posicion=004)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_005, blood_spots_valid_005, blood_spots_invalid_005 ) %>% filter(!is.na(card_id_005)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_005, circulos_validos=blood_spots_valid_005, circulos_no_validos=blood_spots_invalid_005, posicion=005)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_006, blood_spots_valid_006, blood_spots_invalid_006 ) %>% filter(!is.na(card_id_006)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_006, circulos_validos=blood_spots_valid_006, circulos_no_validos=blood_spots_invalid_006, posicion=006)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_007, blood_spots_valid_007, blood_spots_invalid_007 ) %>% filter(!is.na(card_id_007)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_007, circulos_validos=blood_spots_valid_007, circulos_no_validos=blood_spots_invalid_007, posicion=007)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_008, blood_spots_valid_008, blood_spots_invalid_008 ) %>% filter(!is.na(card_id_008)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_008, circulos_validos=blood_spots_valid_008, circulos_no_validos=blood_spots_invalid_008, posicion=008)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_009, blood_spots_valid_009, blood_spots_invalid_009 ) %>% filter(!is.na(card_id_009)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_009, circulos_validos=blood_spots_valid_009, circulos_no_validos=blood_spots_invalid_009, posicion=009)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_010, blood_spots_valid_010, blood_spots_invalid_010 ) %>% filter(!is.na(card_id_010)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_010, circulos_validos=blood_spots_valid_010, circulos_no_validos=blood_spots_invalid_010, posicion=010)
  ) %>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_011, blood_spots_valid_011, blood_spots_invalid_011 ) %>% filter(!is.na(card_id_011)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_011, circulos_validos=blood_spots_valid_011, circulos_no_validos=blood_spots_invalid_011, posicion=011)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_012, blood_spots_valid_012, blood_spots_invalid_012 ) %>% filter(!is.na(card_id_012)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_012, circulos_validos=blood_spots_valid_012, circulos_no_validos=blood_spots_invalid_012, posicion=012)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_013, blood_spots_valid_013, blood_spots_invalid_013 ) %>% filter(!is.na(card_id_013)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_013, circulos_validos=blood_spots_valid_013, circulos_no_validos=blood_spots_invalid_013, posicion=013)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_014, blood_spots_valid_014, blood_spots_invalid_014 ) %>% filter(!is.na(card_id_014)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_014, circulos_validos=blood_spots_valid_014, circulos_no_validos=blood_spots_invalid_014, posicion=014)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_015, blood_spots_valid_015, blood_spots_invalid_015 ) %>% filter(!is.na(card_id_015)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_015, circulos_validos=blood_spots_valid_015, circulos_no_validos=blood_spots_invalid_015, posicion=015)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_016, blood_spots_valid_016, blood_spots_invalid_016 ) %>% filter(!is.na(card_id_016)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_016, circulos_validos=blood_spots_valid_016, circulos_no_validos=blood_spots_invalid_016, posicion=016)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_017, blood_spots_valid_017, blood_spots_invalid_017 ) %>% filter(!is.na(card_id_017)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_017, circulos_validos=blood_spots_valid_017, circulos_no_validos=blood_spots_invalid_017, posicion=017)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_018, blood_spots_valid_018, blood_spots_invalid_018 ) %>% filter(!is.na(card_id_018)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_018, circulos_validos=blood_spots_valid_018, circulos_no_validos=blood_spots_invalid_018, posicion=018)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_019, blood_spots_valid_019, blood_spots_invalid_019 ) %>% filter(!is.na(card_id_019)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_019, circulos_validos=blood_spots_valid_019, circulos_no_validos=blood_spots_invalid_019, posicion=019)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_020, blood_spots_valid_020, blood_spots_invalid_020 ) %>% filter(!is.na(card_id_020)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_020, circulos_validos=blood_spots_valid_020, circulos_no_validos=blood_spots_invalid_020, posicion=020)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_021, blood_spots_valid_021, blood_spots_invalid_021 ) %>% filter(!is.na(card_id_021)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_021, circulos_validos=blood_spots_valid_021, circulos_no_validos=blood_spots_invalid_021, posicion=021)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_022, blood_spots_valid_022, blood_spots_invalid_022 ) %>% filter(!is.na(card_id_022)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_022, circulos_validos=blood_spots_valid_022, circulos_no_validos=blood_spots_invalid_022, posicion=022)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_023, blood_spots_valid_023, blood_spots_invalid_023 ) %>% filter(!is.na(card_id_023)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_023, circulos_validos=blood_spots_valid_023, circulos_no_validos=blood_spots_invalid_023, posicion=023)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_024, blood_spots_valid_024, blood_spots_invalid_024 ) %>% filter(!is.na(card_id_024)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_024, circulos_validos=blood_spots_valid_024, circulos_no_validos=blood_spots_invalid_024, posicion=024)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_025, blood_spots_valid_025, blood_spots_invalid_025 ) %>% filter(!is.na(card_id_025)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_025, circulos_validos=blood_spots_valid_025, circulos_no_validos=blood_spots_invalid_025, posicion=025)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_026, blood_spots_valid_026, blood_spots_invalid_026 ) %>% filter(!is.na(card_id_026)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_026, circulos_validos=blood_spots_valid_026, circulos_no_validos=blood_spots_invalid_026, posicion=026)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_027, blood_spots_valid_027, blood_spots_invalid_027 ) %>% filter(!is.na(card_id_027)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_027, circulos_validos=blood_spots_valid_027, circulos_no_validos=blood_spots_invalid_027, posicion=027)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_028, blood_spots_valid_028, blood_spots_invalid_028 ) %>% filter(!is.na(card_id_028)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_028, circulos_validos=blood_spots_valid_028, circulos_no_validos=blood_spots_invalid_028, posicion=028)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_029, blood_spots_valid_029, blood_spots_invalid_029 ) %>% filter(!is.na(card_id_029)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_029, circulos_validos=blood_spots_valid_029, circulos_no_validos=blood_spots_invalid_029, posicion=029)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_030, blood_spots_valid_030, blood_spots_invalid_030 ) %>% filter(!is.na(card_id_030)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_030, circulos_validos=blood_spots_valid_030, circulos_no_validos=blood_spots_invalid_030, posicion=030)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_031, blood_spots_valid_031, blood_spots_invalid_031 ) %>% filter(!is.na(card_id_031)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_031, circulos_validos=blood_spots_valid_031, circulos_no_validos=blood_spots_invalid_031, posicion=031)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_032, blood_spots_valid_032, blood_spots_invalid_032 ) %>% filter(!is.na(card_id_032)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_032, circulos_validos=blood_spots_valid_032, circulos_no_validos=blood_spots_invalid_032, posicion=032)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_033, blood_spots_valid_033, blood_spots_invalid_033 ) %>% filter(!is.na(card_id_033)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_033, circulos_validos=blood_spots_valid_033, circulos_no_validos=blood_spots_invalid_033, posicion=033)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_034, blood_spots_valid_034, blood_spots_invalid_034 ) %>% filter(!is.na(card_id_034)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_034, circulos_validos=blood_spots_valid_034, circulos_no_validos=blood_spots_invalid_034, posicion=034)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_035, blood_spots_valid_035, blood_spots_invalid_035 ) %>% filter(!is.na(card_id_035)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_035, circulos_validos=blood_spots_valid_035, circulos_no_validos=blood_spots_invalid_035, posicion=035)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_036, blood_spots_valid_036, blood_spots_invalid_036 ) %>% filter(!is.na(card_id_036)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_036, circulos_validos=blood_spots_valid_036, circulos_no_validos=blood_spots_invalid_036, posicion=036)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_037, blood_spots_valid_037, blood_spots_invalid_037 ) %>% filter(!is.na(card_id_037)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_037, circulos_validos=blood_spots_valid_037, circulos_no_validos=blood_spots_invalid_037, posicion=037)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_038, blood_spots_valid_038, blood_spots_invalid_038 ) %>% filter(!is.na(card_id_038)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_038, circulos_validos=blood_spots_valid_038, circulos_no_validos=blood_spots_invalid_038, posicion=038)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_039, blood_spots_valid_039, blood_spots_invalid_039 ) %>% filter(!is.na(card_id_039)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_039, circulos_validos=blood_spots_valid_039, circulos_no_validos=blood_spots_invalid_039, posicion=039)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_040, blood_spots_valid_040, blood_spots_invalid_040 ) %>% filter(!is.na(card_id_040)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_040, circulos_validos=blood_spots_valid_040, circulos_no_validos=blood_spots_invalid_040, posicion=040)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_041, blood_spots_valid_041, blood_spots_invalid_041 ) %>% filter(!is.na(card_id_041)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_041, circulos_validos=blood_spots_valid_041, circulos_no_validos=blood_spots_invalid_041, posicion=041)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_042, blood_spots_valid_042, blood_spots_invalid_042 ) %>% filter(!is.na(card_id_042)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_042, circulos_validos=blood_spots_valid_042, circulos_no_validos=blood_spots_invalid_042, posicion=042)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_043, blood_spots_valid_043, blood_spots_invalid_043 ) %>% filter(!is.na(card_id_043)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_043, circulos_validos=blood_spots_valid_043, circulos_no_validos=blood_spots_invalid_043, posicion=043)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_044, blood_spots_valid_044, blood_spots_invalid_044 ) %>% filter(!is.na(card_id_044)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_044, circulos_validos=blood_spots_valid_044, circulos_no_validos=blood_spots_invalid_044, posicion=044)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_045, blood_spots_valid_045, blood_spots_invalid_045 ) %>% filter(!is.na(card_id_045)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_045, circulos_validos=blood_spots_valid_045, circulos_no_validos=blood_spots_invalid_045, posicion=045)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_046, blood_spots_valid_046, blood_spots_invalid_046 ) %>% filter(!is.na(card_id_046)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_046, circulos_validos=blood_spots_valid_046, circulos_no_validos=blood_spots_invalid_046, posicion=046)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_047, blood_spots_valid_047, blood_spots_invalid_047 ) %>% filter(!is.na(card_id_047)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_047, circulos_validos=blood_spots_valid_047, circulos_no_validos=blood_spots_invalid_047, posicion=047)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_048, blood_spots_valid_048, blood_spots_invalid_048 ) %>% filter(!is.na(card_id_048)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_048, circulos_validos=blood_spots_valid_048, circulos_no_validos=blood_spots_invalid_048, posicion=048)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_049, blood_spots_valid_049, blood_spots_invalid_049 ) %>% filter(!is.na(card_id_049)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_049, circulos_validos=blood_spots_valid_049, circulos_no_validos=blood_spots_invalid_049, posicion=049)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_050, blood_spots_valid_050, blood_spots_invalid_050 ) %>% filter(!is.na(card_id_050)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_050, circulos_validos=blood_spots_valid_050, circulos_no_validos=blood_spots_invalid_050, posicion=050)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_051, blood_spots_valid_051, blood_spots_invalid_051 ) %>% filter(!is.na(card_id_051)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_051, circulos_validos=blood_spots_valid_051, circulos_no_validos=blood_spots_invalid_051, posicion=051)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_052, blood_spots_valid_052, blood_spots_invalid_052 ) %>% filter(!is.na(card_id_052)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_052, circulos_validos=blood_spots_valid_052, circulos_no_validos=blood_spots_invalid_052, posicion=052)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_053, blood_spots_valid_053, blood_spots_invalid_053 ) %>% filter(!is.na(card_id_053)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_053, circulos_validos=blood_spots_valid_053, circulos_no_validos=blood_spots_invalid_053, posicion=053)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_054, blood_spots_valid_054, blood_spots_invalid_054 ) %>% filter(!is.na(card_id_054)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_054, circulos_validos=blood_spots_valid_054, circulos_no_validos=blood_spots_invalid_054, posicion=054)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_055, blood_spots_valid_055, blood_spots_invalid_055 ) %>% filter(!is.na(card_id_055)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_055, circulos_validos=blood_spots_valid_055, circulos_no_validos=blood_spots_invalid_055, posicion=055)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_056, blood_spots_valid_056, blood_spots_invalid_056 ) %>% filter(!is.na(card_id_056)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_056, circulos_validos=blood_spots_valid_056, circulos_no_validos=blood_spots_invalid_056, posicion=056)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_057, blood_spots_valid_057, blood_spots_invalid_057 ) %>% filter(!is.na(card_id_057)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_057, circulos_validos=blood_spots_valid_057, circulos_no_validos=blood_spots_invalid_057, posicion=057)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_058, blood_spots_valid_058, blood_spots_invalid_058 ) %>% filter(!is.na(card_id_058)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_058, circulos_validos=blood_spots_valid_058, circulos_no_validos=blood_spots_invalid_058, posicion=058)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_059, blood_spots_valid_059, blood_spots_invalid_059 ) %>% filter(!is.na(card_id_059)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_059, circulos_validos=blood_spots_valid_059, circulos_no_validos=blood_spots_invalid_059, posicion=059)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_060, blood_spots_valid_060, blood_spots_invalid_060 ) %>% filter(!is.na(card_id_060)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_060, circulos_validos=blood_spots_valid_060, circulos_no_validos=blood_spots_invalid_060, posicion=060)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_061, blood_spots_valid_061, blood_spots_invalid_061 ) %>% filter(!is.na(card_id_061)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_061, circulos_validos=blood_spots_valid_061, circulos_no_validos=blood_spots_invalid_061, posicion=061)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_062, blood_spots_valid_062, blood_spots_invalid_062 ) %>% filter(!is.na(card_id_062)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_062, circulos_validos=blood_spots_valid_062, circulos_no_validos=blood_spots_invalid_062, posicion=062)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_063, blood_spots_valid_063, blood_spots_invalid_063 ) %>% filter(!is.na(card_id_063)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_063, circulos_validos=blood_spots_valid_063, circulos_no_validos=blood_spots_invalid_063, posicion=063)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_064, blood_spots_valid_064, blood_spots_invalid_064 ) %>% filter(!is.na(card_id_064)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_064, circulos_validos=blood_spots_valid_064, circulos_no_validos=blood_spots_invalid_064, posicion=064)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_065, blood_spots_valid_065, blood_spots_invalid_065 ) %>% filter(!is.na(card_id_065)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_065, circulos_validos=blood_spots_valid_065, circulos_no_validos=blood_spots_invalid_065, posicion=065)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_066, blood_spots_valid_066, blood_spots_invalid_066 ) %>% filter(!is.na(card_id_066)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_066, circulos_validos=blood_spots_valid_066, circulos_no_validos=blood_spots_invalid_066, posicion=066)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_067, blood_spots_valid_067, blood_spots_invalid_067 ) %>% filter(!is.na(card_id_067)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_067, circulos_validos=blood_spots_valid_067, circulos_no_validos=blood_spots_invalid_067, posicion=067)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_068, blood_spots_valid_068, blood_spots_invalid_068 ) %>% filter(!is.na(card_id_068)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_068, circulos_validos=blood_spots_valid_068, circulos_no_validos=blood_spots_invalid_068, posicion=068)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_069, blood_spots_valid_069, blood_spots_invalid_069 ) %>% filter(!is.na(card_id_069)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_069, circulos_validos=blood_spots_valid_069, circulos_no_validos=blood_spots_invalid_069, posicion=069)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_070, blood_spots_valid_070, blood_spots_invalid_070 ) %>% filter(!is.na(card_id_070)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_070, circulos_validos=blood_spots_valid_070, circulos_no_validos=blood_spots_invalid_070, posicion=070)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_071, blood_spots_valid_071, blood_spots_invalid_071 ) %>% filter(!is.na(card_id_071)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_071, circulos_validos=blood_spots_valid_071, circulos_no_validos=blood_spots_invalid_071, posicion=071)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_072, blood_spots_valid_072, blood_spots_invalid_072 ) %>% filter(!is.na(card_id_072)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_072, circulos_validos=blood_spots_valid_072, circulos_no_validos=blood_spots_invalid_072, posicion=072)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_073, blood_spots_valid_073, blood_spots_invalid_073 ) %>% filter(!is.na(card_id_073)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_073, circulos_validos=blood_spots_valid_073, circulos_no_validos=blood_spots_invalid_073, posicion=073)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_074, blood_spots_valid_074, blood_spots_invalid_074 ) %>% filter(!is.na(card_id_074)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_074, circulos_validos=blood_spots_valid_074, circulos_no_validos=blood_spots_invalid_074, posicion=074)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_075, blood_spots_valid_075, blood_spots_invalid_075 ) %>% filter(!is.na(card_id_075)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_075, circulos_validos=blood_spots_valid_075, circulos_no_validos=blood_spots_invalid_075, posicion=075)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_076, blood_spots_valid_076, blood_spots_invalid_076 ) %>% filter(!is.na(card_id_076)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_076, circulos_validos=blood_spots_valid_076, circulos_no_validos=blood_spots_invalid_076, posicion=076)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_077, blood_spots_valid_077, blood_spots_invalid_077 ) %>% filter(!is.na(card_id_077)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_077, circulos_validos=blood_spots_valid_077, circulos_no_validos=blood_spots_invalid_077, posicion=077)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_078, blood_spots_valid_078, blood_spots_invalid_078 ) %>% filter(!is.na(card_id_078)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_078, circulos_validos=blood_spots_valid_078, circulos_no_validos=blood_spots_invalid_078, posicion=078)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_079, blood_spots_valid_079, blood_spots_invalid_079 ) %>% filter(!is.na(card_id_079)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_079, circulos_validos=blood_spots_valid_079, circulos_no_validos=blood_spots_invalid_079, posicion=079)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_080, blood_spots_valid_080, blood_spots_invalid_080 ) %>% filter(!is.na(card_id_080)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_080, circulos_validos=blood_spots_valid_080, circulos_no_validos=blood_spots_invalid_080, posicion=080)
  )%>% 
  bind_rows(
    data_muestras %>% filter(redcap_event_name=="registro_arm_2") %>% mutate_all(as.character) %>% 
      select(card_container_id,card_id_081, blood_spots_valid_081, blood_spots_invalid_081 ) %>% filter(!is.na(card_id_081)) %>% transmute(
        contenedor=card_container_id, id_muestra=card_id_081, circulos_validos=blood_spots_valid_081, circulos_no_validos=blood_spots_invalid_081, posicion=081)
  )

#OPERAR MUESTRAS DE SANGRE INTEGRADAS
#---------------------------------
muestras_sangre_integradas %>% writexl::write_xlsx("output/muestras_samgre_2020-04-27.xlsx")

#leer datos para revisar
datos_erick<-read_csv("D:/HAPIN/revision_muestras_eric.csv")
datos_erick<-datos_erick %>% mutate_all(as.character)

#verificar informacion de muestras de sangre
datos_erick %>% mutate(id_muestra=Etiqueta) %>% left_join(
  muestras_sangre_integradas
) %>% writexl::write_xlsx("output/datos_revisados_muestras_erick.xlsx")

#Comprobar existencia de muestras
#verificar informacion
datos_erick %>% mutate(id_muestra=Etiqueta) %>% left_join(
  muestras_redcap
)%>% writexl::write_xlsx("output/datos_revisados_existencia_muestras.xlsx")

dt_muestras_lab<-read_csv("data/exports/HAPINGuatemalaLab_DATA_2022-02-10_1318.csv")


dt_muestras_lab %>% filter(!is.na(card_container_id)) %>% 
  select(card_container_id, card_storage_date, matches("card_id_")) %>% 
  gather(key="variable", value="value", -card_container_id, -card_storage_date) %>% 
  select(card_container_id, fecha=card_storage_date, id_muestra=value) %>% filter(!is.na(id_muestra)) %>% mutate(
    id=substr(id_muestra, 1, 5)
  ) %>% filter(id=="33599")


dt_muestras_lab %>% filter(!is.na(urine_record_date)) %>% select(date=urine_record_date,
                                                                 matches("urine_id_")) %>% 
  gather(key="variable", value = "value", -date) %>% select(date, id_muestra=value) %>% 
  filter(!is.na(id_muestra)) %>% mutate(id=substr(id_muestra, 1, 5)) %>% 
  filter(id=="33314")

dt_muestras_lab %>% filter(!is.na(aliquot_container_id)) %>% select(aliquot_container_id,date=alicuot_storage_date,
                                                                    matches("aliquot_id_")) %>% 
  gather(key="variable", value="value", -aliquot_container_id, -date) %>% filter(!is.na(value)) %>% 
  select(aliquot_container_id, date, id_muestra=value) %>% 
  mutate(id=substr(id_muestra,1,5)) %>% filter(
    id=="33458"
  ) %>% arrange(date) %>% print(n=100)

