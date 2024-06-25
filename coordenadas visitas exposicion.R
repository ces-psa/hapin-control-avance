# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

library("tidyverse")

#______________________________________________________________________
#SACAR LISTA DE COMUNIDADES CON BASE EN CATALOGO DE COMUNIDADES JALAPA
#_____________________________________________________________________
#---------------------------------------------------------------------
comunidades<-gt_participants %>% select(record_id,id_estudio, com_jalapa, com_jalapa_new, lat, long) %>%
  mutate(house_id=if_else(condition = grepl("^G[0-9]{4}",id_estudio),
                          true = record_id,
                          false = id_estudio
  )) %>% mutate(
    id_tamizaje=if_else(
      condition = grepl("^G[0-9]{4}",record_id),
      true = record_id,
      false = id_estudio
    )) %>%  mutate(
      community = recode(
        com_jalapa,
        "2101308" = "SANTA CRUZ",
        "2101283" = "EL MILAGRO",
        "2101127" = "LAS JOYITAS",
        "2101045" = "LAS GALERAS",
        "2101207" = "LAS PEAS",
        "2101006" = "ANSHIGUA",
        "2101181" = "CARRIZALITO",
        "2101220" = "SHICAL",
        "2101200" = "LA PASTORIA",
        "2101115" = "POTRERO CARRILLO",
        "2101210" = "LOS PINOS",
        "2101131" = "SAN LORENZO",
        "2101076" = "LA PUENTE",
        "2101202" = "LA LAGUNA",
        "2101072" = "LOS LIMARES",
        "2101035" = "EL MOJON",
        "2101351" = "LAGUNA VERDE",
        "2101173" = "SAN LUIS GUISHORO",
        "2101138" = "SANTA ELENA",
        "2101216" = "RIO FRIO",
        "2101132" = "SAN ANTONIO LA NORIA",
        "2101182" = "EL CASCABILLAL Y EL COPALITO",
        "2101080" = "LOS CIEGOS",
        "2101137" = "SAN IGNACIO",
        "2101151" = "LA VENTURA",
        "2101104" = "LLANO DE LA PUERTA",
        "2101175" = "LOS MEZCALES",
        "2101020" = "CHAGUITE",
        "2101116" = "POTRERO DEL BURRO",
        "2101014" = "EL CARRIZAL",
        "2101129" = "SAN JOSE",
        "2101030" = "EL TALQUEZAL",
        "2101032" = "EL RODEO",
        "2101145" = "TIERRA BLANCA",
        "2101081" = "LAGUNETA LOS ACHIOTES O QUEBRADA HONDA",
        "2101002" = "ACHIOTES JUMAY",
        "2101238" = "EL GUAJE",
        "2101070" = "LOS TALPETATES",
        "2101036" = "EL TABLON",
        "2101083" = "LA TEJERA",
        "2101074" = "LOS IZOTES",
        "2101123" = "SANSAYO",
        "2101113" = "PATA GALANA",
        "2101114" = "PALO VERDE",
        "2101039" = "EL LIMON",
        "2101241" = "EL PINALITO",
        "2101251" = "LA LAGUNILLA",
        "2101134" = "SANSURUTATE",
        "2101037" = "EL TERRERO I",
        "2101157" = "YERBABUENA",
        "2101203" = "LAGUNETA EL SAPO",
        "2101195" = "ITZACOBA",
        "2101227" = "VOLCAN PAZ",
        "2101071" = "LA PAZ",
        "2101124" = "SASHICO",
        "2101022" = "EL DURAZNAL",
        "2101133" = "SUQUINAY",
        "2101021" = "DIVISADERO",
        "2101128" = "SANYUYO",
        "2101150" = "VOLCAN SANYUYO",
        "2101253" = "LAS MARIAS",
        "2101027" = "EL PITO",
        "2101208" = "LAS PIEDRAS",
        "2101015" = "CERRO DE ALCOBA",
        "2101060" = "LAZARETO",
        "2101172" = "LOS GONZALEZ",
        "2101052" = "LA FUENTE DE LA MONTAA",
        "2101029" = "EL DURAZNO",
        "2101117" = "PINO GORDO",
        "2101031" = "EL ROBLAR",
        "2101028" = "EL ARENAL",
        "2101062" = "LA AURORA",
        "2101068" = "LA LAGUNETA",
        "2101166" = "AGUA ZARCA",
        "2101026" = "EL DURAZNITO",
        "2101265" = "LAS CRUCES",
        "2101125" = "SAN JOSE LA FUENTE",
        "2101023" = "EL PARAISO",
        "2101147" = "URLANTA",
        "2101010" = "EL MIRADOR",
        "2101188" = "EL COYOTE",
        "2101224" = "TRUJILLO",
        "2101065" = "LOS LLANITOS",
        "2101007" = "ARLOROMA",
        "2101148" = "URAYANSAPO",
        "2101179" = "ARAISAPO",
        "2101144" = "TATASIRIRE",
        "2101066" = "LAS AZUCENAS",
        "2101193" = "GOLFILLO",
        "2101061" = "LOS TABLONES",
        "2101012" = "BUENA VISTA",
        "2101064" = "LOMA DE ENMEDIO",
        "2101105" = "MIRAFLORES",
        "2101053" = "LAS GUACAMAYAS",
        "2101205" = "LAS JOYAS",
        "2101004" = "ALTUPE",
        "2101009" = "EL AGUACATE",
        "2101025" = "EL BOSQUE",
        "2101016" = "CORONA",
        "2101213" = "LA PIEDRONA",
        "2101099" = "LOS CEDROS",
        "2101222" = "SUQUINAY",
        "2101109" = "MOJON DEL MUERTO",
        "2101106" = "MIRAMUNDO",
        "2101108" = "MAL PASO",
        "2101232" = "SAUSAL",
        "2101187" = "EL CONFITERO",
        "2101102" = "LAS MORITAS",
        "2101110" = "ORCHOJ",
        "2101011" = "AGUIJOTES",
        "2101120" = "QUEBRADITAS",
        "2101196" = "JOYA GRANDE",
        "2101250" = "LOS LOPEZ",
        "2101249" = "EL ASTILLERO",
        "2101247" = "SHICAL",
        "2101159" = "CARTAGO",
        "2101121" = "RIO BLANCO",
        "2101047" = "EL RODEO",
        "2101059" = "JICALTEPEQUE",
        "2101003" = "ASTILLERO",
        "2101215" = "RIO BLANCO ARRIBA",
        "2101201" = "LA VICENTINA",
        "2101158" = "LAS TAPIAS",
        "2101255" = "LAGUNA VERDE",
        "2101252" = "GARCIA",
        "2101063" = "LAS DELICIAS",
        "2101161" = "VAREJONES",
        "2101051" = "EL ZAPOTE",
        "2101044" = "EL RETIRO",
        "2101997" = "POBLACION DISPERSA",
        "2101310" = "SAN LORENZO",
        "2101041" = "EL TIGRE",
        "2101311" = "SAN FRANSISICO BELLA VISTA",
        "2101312" = "LA ESMERALDA",
        "2101352" = "LINDA VISTA",
        "2101314" = "INGENIO DE LOS FIERROS",
        "2101315" = "CARTAGO",
        "2101180" = "BETHANIA",
        "2101362" = "VILLA ADRIANA",
        "2101355" = "SALFATE",
        "2101316" = "LAS LOMITAS",
        "2101257" = "CARRIZALITO",
        "2101318" = "PARINAQUE",
        "2101354" = "EL CAFETAL",
        "2101103" = "LLANO GRANDE",
        "2101319" = "MOSQUITO",
        "2101320" = "HATO VIEJO",
        "2101321" = "INGENIO DE AYARZA",
        "2101361" = "VERDUGO",
        "2101322" = "CUESTA GRANDE",
        "2101323" = "EL JUTE",
        "2101324" = "GRACIAS A DIOS",
        "2101325" = "EL SAUZAL",
        "2101326" = "LA TOMA",
        "2101327" = "EL VOLCAN",
        "2101328" = "LA LAGUNA",
        "2101329" = "LA CUCHILLA",
        "2101330" = "TALQUETZAL",
        "2101126" = "SANSIRISAY",
        "2101331" = "LA TEJERA",
        "2101332" = "SAN FRANCISCO",
        "2101333" = "SANCASTI",
        "2101334" = "AGUA CALIENTE",
        "2101335" = "EL SITIO",
        "2101336" = "CEBOLLIN",
        "2101337" = "EL AGUACATE",
        "2101338" = "LOS LLANITOS",
        "2101339" = "RASTROJO LIMPIO",
        "2101359" = "EL CEDRO",
        "2101340" = "EL INCIENSO",
        "2101341" = "AGUA BLANCA",
        "2101342" = "LAGUNA",
        "2101343" = "LAS MARIAS",
        "2101344" = "LA CARBONERA",
        "2101346" = "LA VENTANA",
        "2101350" = "LAS CRUCESITAS",
        "2101270" = "SAN FERNANDO O CAMELOT",
        "2101090" = "LAGUNA VERDE",
        "2101254" = "JOSE LA CARBONERA",
        "2101347" = "LAGUNA VERDE",
        "2101235" = "AGUA BLANCA",
        "2101017" = "DOBLE R",
        "2101034" = "HACIENDA LA PONDEROSA",
        "2101139" = "SAN MIGUEL",
        "2101256" = "LA CIENEGA",
        "2101018" = "SAN ISIDRO",
        "2101240" = "EL CARMEN",
        "2101309" = "EL CARMEN",
        "2101268" = "EL CHAGUITE",
        "2101239" = "LAS UVAS",
        "2101033" = "EL MANGUITO",
        "2101348" = "LOS MEZCALES",
        "2101358" = "EL AGUACATE",
        "2101111" = "OJO DE AGUA",
        "2101194" = "INGENIO FIERRO",
        "2101357" = "SABANETAS",
        "2101360" = "COCALES",
        "2101048" = "EL JUTILLO",
        "2101293" = "ESTACON",
        "2101353" = "LA BOTIJA",
        "2101349" = "SAN FRANSISCO POZA VERDE",
        "2101001" = "JALAPA",
        "999" = "OTRA"
      ),
      community_new = recode(
        com_jalapa_new,
        "2101308" = "SANTA CRUZ",
        "2101283" = "EL MILAGRO",
        "2101127" = "LAS JOYITAS",
        "2101045" = "LAS GALERAS",
        "2101207" = "LAS PEAS",
        "2101006" = "ANSHIGUA",
        "2101181" = "CARRIZALITO",
        "2101220" = "SHICAL",
        "2101200" = "LA PASTORIA",
        "2101115" = "POTRERO CARRILLO",
        "2101210" = "LOS PINOS",
        "2101131" = "SAN LORENZO",
        "2101076" = "LA PUENTE",
        "2101202" = "LA LAGUNA",
        "2101072" = "LOS LIMARES",
        "2101035" = "EL MOJON",
        "2101351" = "LAGUNA VERDE",
        "2101173" = "SAN LUIS GUISHORO",
        "2101138" = "SANTA ELENA",
        "2101216" = "RIO FRIO",
        "2101132" = "SAN ANTONIO LA NORIA",
        "2101182" = "EL CASCABILLAL Y EL COPALITO",
        "2101080" = "LOS CIEGOS",
        "2101137" = "SAN IGNACIO",
        "2101151" = "LA VENTURA",
        "2101104" = "LLANO DE LA PUERTA",
        "2101175" = "LOS MEZCALES",
        "2101020" = "CHAGUITE",
        "2101116" = "POTRERO DEL BURRO",
        "2101014" = "EL CARRIZAL",
        "2101129" = "SAN JOSE",
        "2101030" = "EL TALQUEZAL",
        "2101032" = "EL RODEO",
        "2101145" = "TIERRA BLANCA",
        "2101081" = "LAGUNETA LOS ACHIOTES O QUEBRADA HONDA",
        "2101002" = "ACHIOTES JUMAY",
        "2101238" = "EL GUAJE",
        "2101070" = "LOS TALPETATES",
        "2101036" = "EL TABLON",
        "2101083" = "LA TEJERA",
        "2101074" = "LOS IZOTES",
        "2101123" = "SANSAYO",
        "2101113" = "PATA GALANA",
        "2101114" = "PALO VERDE",
        "2101039" = "EL LIMON",
        "2101241" = "EL PINALITO",
        "2101251" = "LA LAGUNILLA",
        "2101134" = "SANSURUTATE",
        "2101037" = "EL TERRERO I",
        "2101157" = "YERBABUENA",
        "2101203" = "LAGUNETA EL SAPO",
        "2101195" = "ITZACOBA",
        "2101227" = "VOLCAN PAZ",
        "2101071" = "LA PAZ",
        "2101124" = "SASHICO",
        "2101022" = "EL DURAZNAL",
        "2101133" = "SUQUINAY",
        "2101021" = "DIVISADERO",
        "2101128" = "SANYUYO",
        "2101150" = "VOLCAN SANYUYO",
        "2101253" = "LAS MARIAS",
        "2101027" = "EL PITO",
        "2101208" = "LAS PIEDRAS",
        "2101015" = "CERRO DE ALCOBA",
        "2101060" = "LAZARETO",
        "2101172" = "LOS GONZALEZ",
        "2101052" = "LA FUENTE DE LA MONTAA",
        "2101029" = "EL DURAZNO",
        "2101117" = "PINO GORDO",
        "2101031" = "EL ROBLAR",
        "2101028" = "EL ARENAL",
        "2101062" = "LA AURORA",
        "2101068" = "LA LAGUNETA",
        "2101166" = "AGUA ZARCA",
        "2101026" = "EL DURAZNITO",
        "2101265" = "LAS CRUCES",
        "2101125" = "SAN JOSE LA FUENTE",
        "2101023" = "EL PARAISO",
        "2101147" = "URLANTA",
        "2101010" = "EL MIRADOR",
        "2101188" = "EL COYOTE",
        "2101224" = "TRUJILLO",
        "2101065" = "LOS LLANITOS",
        "2101007" = "ARLOROMA",
        "2101148" = "URAYANSAPO",
        "2101179" = "ARAISAPO",
        "2101144" = "TATASIRIRE",
        "2101066" = "LAS AZUCENAS",
        "2101193" = "GOLFILLO",
        "2101061" = "LOS TABLONES",
        "2101012" = "BUENA VISTA",
        "2101064" = "LOMA DE ENMEDIO",
        "2101105" = "MIRAFLORES",
        "2101053" = "LAS GUACAMAYAS",
        "2101205" = "LAS JOYAS",
        "2101004" = "ALTUPE",
        "2101009" = "EL AGUACATE",
        "2101025" = "EL BOSQUE",
        "2101016" = "CORONA",
        "2101213" = "LA PIEDRONA",
        "2101099" = "LOS CEDROS",
        "2101222" = "SUQUINAY",
        "2101109" = "MOJON DEL MUERTO",
        "2101106" = "MIRAMUNDO",
        "2101108" = "MAL PASO",
        "2101232" = "SAUSAL",
        "2101187" = "EL CONFITERO",
        "2101102" = "LAS MORITAS",
        "2101110" = "ORCHOJ",
        "2101011" = "AGUIJOTES",
        "2101120" = "QUEBRADITAS",
        "2101196" = "JOYA GRANDE",
        "2101250" = "LOS LOPEZ",
        "2101249" = "EL ASTILLERO",
        "2101247" = "SHICAL",
        "2101159" = "CARTAGO",
        "2101121" = "RIO BLANCO",
        "2101047" = "EL RODEO",
        "2101059" = "JICALTEPEQUE",
        "2101003" = "ASTILLERO",
        "2101215" = "RIO BLANCO ARRIBA",
        "2101201" = "LA VICENTINA",
        "2101158" = "LAS TAPIAS",
        "2101255" = "LAGUNA VERDE",
        "2101252" = "GARCIA",
        "2101063" = "LAS DELICIAS",
        "2101161" = "VAREJONES",
        "2101051" = "EL ZAPOTE",
        "2101044" = "EL RETIRO",
        "2101997" = "POBLACION DISPERSA",
        "2101310" = "SAN LORENZO",
        "2101041" = "EL TIGRE",
        "2101311" = "SAN FRANSISICO BELLA VISTA",
        "2101312" = "LA ESMERALDA",
        "2101352" = "LINDA VISTA",
        "2101314" = "INGENIO DE LOS FIERROS",
        "2101315" = "CARTAGO",
        "2101180" = "BETHANIA",
        "2101362" = "VILLA ADRIANA",
        "2101355" = "SALFATE",
        "2101316" = "LAS LOMITAS",
        "2101257" = "CARRIZALITO",
        "2101318" = "PARINAQUE",
        "2101354" = "EL CAFETAL",
        "2101103" = "LLANO GRANDE",
        "2101319" = "MOSQUITO",
        "2101320" = "HATO VIEJO",
        "2101321" = "INGENIO DE AYARZA",
        "2101361" = "VERDUGO",
        "2101322" = "CUESTA GRANDE",
        "2101323" = "EL JUTE",
        "2101324" = "GRACIAS A DIOS",
        "2101325" = "EL SAUZAL",
        "2101326" = "LA TOMA",
        "2101327" = "EL VOLCAN",
        "2101328" = "LA LAGUNA",
        "2101329" = "LA CUCHILLA",
        "2101330" = "TALQUETZAL",
        "2101126" = "SANSIRISAY",
        "2101331" = "LA TEJERA",
        "2101332" = "SAN FRANCISCO",
        "2101333" = "SANCASTI",
        "2101334" = "AGUA CALIENTE",
        "2101335" = "EL SITIO",
        "2101336" = "CEBOLLIN",
        "2101337" = "EL AGUACATE",
        "2101338" = "LOS LLANITOS",
        "2101339" = "RASTROJO LIMPIO",
        "2101359" = "EL CEDRO",
        "2101340" = "EL INCIENSO",
        "2101341" = "AGUA BLANCA",
        "2101342" = "LAGUNA",
        "2101343" = "LAS MARIAS",
        "2101344" = "LA CARBONERA",
        "2101346" = "LA VENTANA",
        "2101350" = "LAS CRUCESITAS",
        "2101270" = "SAN FERNANDO O CAMELOT",
        "2101090" = "LAGUNA VERDE",
        "2101254" = "JOSE LA CARBONERA",
        "2101347" = "LAGUNA VERDE",
        "2101235" = "AGUA BLANCA",
        "2101017" = "DOBLE R",
        "2101034" = "HACIENDA LA PONDEROSA",
        "2101139" = "SAN MIGUEL",
        "2101256" = "LA CIENEGA",
        "2101018" = "SAN ISIDRO",
        "2101240" = "EL CARMEN",
        "2101309" = "EL CARMEN",
        "2101268" = "EL CHAGUITE",
        "2101239" = "LAS UVAS",
        "2101033" = "EL MANGUITO",
        "2101348" = "LOS MEZCALES",
        "2101358" = "EL AGUACATE",
        "2101111" = "OJO DE AGUA",
        "2101194" = "INGENIO FIERRO",
        "2101357" = "SABANETAS",
        "2101360" = "COCALES",
        "2101048" = "EL JUTILLO",
        "2101293" = "ESTACON",
        "2101353" = "LA BOTIJA",
        "2101349" = "SAN FRANSISCO POZA VERDE",
        "2101001" = "JALAPA",
        "999" = "OTRA"
      )
    )


#------------------------------------------------
#VISITAS C31
#-----------------
#catalogo de rutas
rutas<-read_csv("data/cat_rutas_visitas.csv")
#-----------------



###cargar las coordenadas mas recientes

# repeated crfs ----
coordenadas_file <- list.files(
  path = "c:/temp/",
  pattern = "HAPINGuatemalaExposu_DATA_",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaExposu_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

data_coordenadas <- read_csv(
  file = coordenadas_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()

data_coordenadas %>% group_by(redcap_event_name) %>% count()

#sacar tabla de coordenadas para cada visita
dt_coordenadas_visitas<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  left_join(
  data_coordenadas %>% filter(redcap_event_name=="linea_de_base_arm_2") %>% 
    transmute(id=record_id, visita="lb",
               fecha_lb=gth4x_date, lb_long=gth4x_long, lb_lat=gthx_lat, lb_alt=gthx_elevation)
) %>%  left_join(
  data_coordenadas %>% filter(redcap_event_name=="p1_arm_2") %>% 
    transmute(id=record_id, p1_visita="p1",
              fecha_p1=gth4x_date, p1_long=gth4x_long, p1_lat=gthx_lat, p1_alt=gthx_elevation)
) %>% left_join(
  data_coordenadas %>% filter(redcap_event_name=="p2_arm_2") %>% 
    transmute(id=record_id, p2_visita="p2",
              fecha_p2=gth4x_date, p2_long=gth4x_long, p2_lat=gthx_lat, p2_alt=gthx_elevation)
) %>% left_join(
  data_coordenadas %>% filter(redcap_event_name=="b1_y_crecimiento_m_arm_2") %>% 
    transmute(id=record_id, b1_visita="b1",
              fecha_b1=gth4x_date, b1_long=gth4x_long, b1_lat=gthx_lat, b1_alt=gthx_elevation)
) %>% left_join(
  data_coordenadas %>% filter(redcap_event_name=="b2_y_crecimiento_m_arm_2") %>% 
    transmute(id=record_id, b2_visita="b2",
              fecha_b2=gth4x_date, b2_long=gth4x_long, b2_lat=gthx_lat, b2_alt=gthx_elevation)
) %>%  left_join(
  data_coordenadas %>% filter(redcap_event_name=="b4_arm_2") %>% 
    transmute(id=record_id, b4_visita="b4",
              fecha_b4=gth4x_date, b4_long=gth4x_long, b4_lat=gthx_lat, b4_alt=gthx_elevation)
) %>%  print(n=Inf)

main_study_gt<-read_csv("c:/temp/MainStudyGT_DATA_2022-09-09_1127.csv")

#coordenadas de z10
coordenadas_z10<-main_study_gt %>% filter(!is.na(z10_date)) %>% mutate(
  id=if_else(grepl("^G",id_estudio),as.character(record_id), as.character(id_estudio) )
  ) %>% select(id, z10_lat=z10_lati_gps, z10_long=z10_long_gps, z10_alt=z10_alti_gps) %>% filter(!is.na(id)) 

coordenadas_z10<-coordenadas_z10 %>% mutate_all(as.character)
#coordenadas de exposición
dt_coord_exposicion<-dt_coordenadas_visitas %>% mutate(
  lat=case_when(
    !is.na(b4_lat) ~ b4_lat,
    !is.na(b2_lat) ~ b2_lat,
    !is.na(b1_lat) ~ b1_lat,
    !is.na(p2_lat) ~ p2_lat,
    !is.na(p1_lat) ~ p1_lat,
    !is.na(lb_lat) ~ lb_lat
  ),
  long=case_when(
    !is.na(b4_long) ~ b4_long,
    !is.na(b2_long) ~ b2_long,
    !is.na(b1_long) ~ b1_long,
    !is.na(p2_long) ~ p2_long,
    !is.na(p1_long) ~ p1_long,
    !is.na(lb_long) ~ lb_long
  ),
  alt=case_when(
    !is.na(b4_alt) ~ b4_alt,
    !is.na(b2_alt) ~ b2_alt,
    !is.na(b1_alt) ~ b1_alt,
    !is.na(p2_alt) ~ p2_alt,
    !is.na(p1_alt) ~ p1_alt,
    !is.na(lb_alt) ~ lb_alt
  )
) %>% select(
  id, lat, long, alt
)

dt_coord_exposicion<-dt_coord_exposicion %>% mutate_all(as.character)

tabla_coordenadas_hapin1<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, grupo=recode(s6_arm,"1"="Intervencion", "0"="Control")) %>% 
  left_join(
    dt_coord_exposicion
  ) %>% left_join(
    coordenadas_z10
  ) %>% mutate(
    lat=if_else( is.na(lat), z10_lat, lat ),
    long=if_else( is.na(long), z10_long, long ),
    alt=if_else( is.na(alt), z10_alt, alt)
  ) %>% transmute(
    id, grupo,
    lat,
    long,
    alt
  ) %>% mutate(
    lat=if_else(lat=="1465779", "14.65779", lat)
    )
tabla_coordenadas_hapin1$long<- gsub(".*^","-",tabla_coordenadas_hapin1$long)

tabla_coordenadas_hapin1 %>% write_csv("output/coordenadas_hapin1.csv")

coor_out<-c("33654", 
            "35095",
            "35126",  
            "33052",
            "33357",
            "33574",
            "33070",
            "33632", 
            "33100",
            "33168",  
            "33451", 
            "35060")
dt_coordenadas_visitas %>% filter(id %in% coor_out) %>% View()
tabla_coordenadas_hapin1 %>% filter(id %in% coor_out)
coordenadas_z10 %>% filter(id=="33052")

#marcar los registros que tuvieron migración o medidas diferentes
dt_coordenadas_visitas %>% filter(!is.na(lb_lat)) %>%  mutate(
  lb_p1=if_else((lb_lat == p1_lat),"0", "1")
) %>% filter(lb_p1=="1") %>% print(n=Inf) %>% write_csv("output/coordenadas.csv")

