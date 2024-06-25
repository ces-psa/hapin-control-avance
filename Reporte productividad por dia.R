# Emory RedCap export data ----
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10) ----
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10) ----
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")
# Participant information (z10) -----
source(file = "scripts/0_get_e3.R", encoding = "UTF-8")



library("tidyverse")

#______________________________________________________________________
#SACAR LISTA DE COMUNIDADES CON BASE EN CATALOGO DE COMUNIDADES JALAPA ----
#_____________________________________________________________________

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




#Rutas ----


rutas<-read_csv("data/cat_rutas_visitas.csv")






# CONTEOS PRODUCTIVIDAD POR PERSONA HAPIN 36 MESES ----
fecha_conteos="2022-10-01"
#s4
s4_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by) %>% 
  filter(as.Date(s4_date)>=fecha_conteos) %>%  mutate(
    dia=format(s4_date, "%A"),
    mes=format(s4_date, "%B"),
    iniciales=s4_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="S4")

#m10
m10_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by) %>% 
  filter(as.Date(m10_date)>=fecha_conteos) %>%  mutate(
    dia=format(m10_date, "%A"),
    mes=format(m10_date, "%B"),
    iniciales=m10_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M10")

#m11
m11_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by) %>% 
  filter(as.Date(m11_date)>=fecha_conteos) %>%  mutate(
    dia=format(m11_date, "%A"),
    mes=format(m11_date, "%B"),
    iniciales=m11_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M11")

#m14a
m14a_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by) %>% 
  filter(as.Date(m14a_date)>=fecha_conteos) %>%  mutate(
    dia=format(m14a_date, "%A"),
    mes=format(m14a_date, "%B"),
    iniciales=m14a_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M14a")

#m14b
m14b_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by) %>% 
  filter(as.Date(m14b_date)>=fecha_conteos) %>%  mutate(
    dia=format(m14b_date, "%A"),
    mes=format(m14b_date, "%B"),
    iniciales=m14b_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M14b")

#m19
m19_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by) %>% 
  filter(as.Date(m19_date)>=fecha_conteos) %>%  mutate(
    dia=format(m19_date, "%A"),
    mes=format(m19_date, "%B"),
    iniciales=m19_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M19")

#c31
c31_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c31_date_2)) %>% select(id, c31_date_2, c31_by_2) %>% 
  filter(as.Date(c31_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(c31_date_2, "%A"),
    mes=format(c31_date_2, "%B"),
    iniciales=c31_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C31")

#c32
c32_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c32_date_2)) %>% select(id, c32_date_2, c32_by_2) %>% 
  filter(as.Date(c32_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(c32_date_2, "%A"),
    mes=format(c32_date_2, "%B"),
    iniciales=c32_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C32")

#c33
c33_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% select(id, c33_date_2, c33_by_2) %>% 
  filter(as.Date(c33_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(c33_date_2, "%A"),
    mes=format(c33_date_2, "%B"),
    iniciales=c33_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C33")

#c35
c35_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id, c35_date_2, c35_by_2) %>% 
  filter(as.Date(c35_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(c35_date_2, "%A"),
    mes=format(c35_date_2, "%B"),
    iniciales=c35_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C35")

#c85
c85_36m_realizados<-gt_hapin_II_data %>% filter(!is.na(h85_date)) %>% select(id, h85_date, c85_by) %>% 
  filter(as.Date(h85_date)>=fecha_conteos) %>%  mutate(
    dia=format(h85_date, "%A"),
    mes=format(h85_date, "%B"),
    iniciales=c85_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C85")

#c42
c42_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  filter(!is.na(c42_date)) %>% select(id, c42_date, c42_by) %>% 
  filter(as.Date(c42_date)>=fecha_conteos) %>%  mutate(
    dia=format(c42_date, "%A"),
    mes=format(c42_date, "%B"),
    iniciales=c42_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C42")

#c86
c86_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(c86_date)) %>% select(id, c86_date, c86_by) %>% 
  filter(as.Date(c86_date)>=fecha_conteos) %>%  mutate(
    dia=format(c86_date, "%A"),
    mes=format(c86_date, "%B"),
    iniciales=c86_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C86")

#H41
h41_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(h41_date_v2)) %>% select(id, h41_date_v2, h41_by_v2) %>% 
  filter(as.Date(h41_date_v2)>=fecha_conteos) %>%  mutate(
    dia=format(h41_date_v2, "%A"),
    mes=format(h41_date_v2, "%B"),
    iniciales=h41_by_v2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H41")

#H42
h42_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(h42_date_2)) %>% select(id, h42_date_2, h42_by_2) %>% 
  filter(as.Date(h42_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(h42_date_2, "%A"),
    mes=format(h42_date_2, "%B"),
    iniciales=h42_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H42")

#H43
h43_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(h43_date_2)) %>% select(id, h43_date_2, h43_by_2) %>% 
  filter(as.Date(h43_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(h43_date_2, "%A"),
    mes=format(h43_date_2, "%B"),
    iniciales=h43_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H43")


#H57
h57_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(h57_date_2)) %>% select(id, h57_date_2, h57_by_2) %>% 
  filter(as.Date(h57_date_2)>=fecha_conteos) %>%  mutate(
    dia=format(h57_date_2, "%A"),
    mes=format(h57_date_2, "%B"),
    iniciales=h57_by_2
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H57")

#E3
e3_36m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  
  filter(!is.na(e3_date)) %>% select(id, e3_date, e3_by) %>% 
  filter(as.Date(e3_date)>=fecha_conteos) %>%  mutate(
    dia=format(e3_date, "%A"),
    mes=format(e3_date, "%B"),
    iniciales=e3_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="E3")  

all_crf_sent_36<-s4_36m_realizados %>% bind_rows(
  list(
    m10_36m_realizados,
    m11_36m_realizados,
    m14a_36m_realizados,
    m14b_36m_realizados,
    m19_36m_realizados,
    c31_36m_realizados,
    c32_36m_realizados,
    c33_36m_realizados,
    c35_36m_realizados,
   # c85_36m_realizados,
    c42_36m_realizados,
    c86_36m_realizados,
    h41_36m_realizados,
    h42_36m_realizados,
    h43_36m_realizados,
    h57_36m_realizados,
    e3_36m_realizados
  )
)

#grafica Keyla
all_crf_sent_36 %>% ggplot(
  aes(x=iniciales, y=n, group=dia)
) + geom_line()

#generar excel para crear tabla dinámica
all_crf_sent_36 %>% writexl::write_xlsx(paste0("output/crf_sent_", 
                                            fecha_conteos,"_al_", Sys.Date(),".xlsx"))

#conteo historico de 24 meses ----
#s4
s4_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by) %>% 
  filter(as.Date(s4_date)>=fecha_conteos) %>%  mutate(
    dia=format(s4_date, "%A"),
    mes=format(s4_date, "%B"),
    iniciales=s4_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="S4")

#m10
# m10_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
#   filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by) %>% 
#   filter(as.Date(m10_date)>=fecha_conteos) %>%  mutate(
#     dia=format(m10_date, "%A"),
#     mes=format(m10_date, "%B"),
#     iniciales=m10_by
#   ) %>% group_by(
#     mes, dia, iniciales
#   ) %>% count() %>% mutate(crf="M10")

#m11
m11_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by) %>% 
  filter(as.Date(m11_date)>=fecha_conteos) %>%  mutate(
    dia=format(m11_date, "%A"),
    mes=format(m11_date, "%B"),
    iniciales=m11_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M11")

#m14a
m14a_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by) %>% 
  filter(as.Date(m14a_date)>=fecha_conteos) %>%  mutate(
    dia=format(m14a_date, "%A"),
    mes=format(m14a_date, "%B"),
    iniciales=m14a_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M14a")

#m14b
m14b_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by) %>% 
  filter(as.Date(m14b_date)>=fecha_conteos) %>%  mutate(
    dia=format(m14b_date, "%A"),
    mes=format(m14b_date, "%B"),
    iniciales=m14b_by
  ) %>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="M14b")

#m19
# m19_24m_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
#   filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by) %>% 
#   filter(as.Date(m19_date)>=fecha_conteos) %>%  mutate(
#     dia=format(m19_date, "%A"),
#     mes=format(m19_date, "%B"),
#     iniciales=m19_by
#   ) %>% group_by(
#     mes, dia, iniciales
#   ) %>% count() %>% mutate(crf="M19")

#A23a 
a23_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by) %>% 
  filter(as.Date(a23_date)>=fecha_conteos) %>% mutate(
    dia=format(a23_date, "%A"),
    mes=format(a23_date, "%B"),
    iniciales=a23_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="A23")

#A24a 
a24a_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(a24a_date)) %>% select(id, a24a_date, a24a_by) %>% 
  filter(as.Date(a24a_date)>=fecha_conteos) %>% mutate(
    dia=format(a24a_date, "%A"),
    mes=format(a24a_date, "%B"),
    iniciales=a24a_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="A24a")

#A24b 
a24b_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(a24b_date)) %>% select(id, a24b_date, a24b_by) %>% 
  filter(as.Date(a24b_date)>=fecha_conteos) %>% mutate(
    dia=format(a24b_date, "%A"),
    mes=format(a24b_date, "%B"),
    iniciales=a24b_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="A24b")

#A26a 
a26a_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by) %>% 
  filter(as.Date(a26_date)>=fecha_conteos) %>% mutate(
    dia=format(a26_date, "%A"),
    mes=format(a26_date, "%B"),
    iniciales=a26_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="A26a")

#A30 
a30_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(a30_date)) %>% select(id, a30_date, a30_by) %>% 
  filter(as.Date(a30_date)>=fecha_conteos) %>% mutate(
    dia=format(a30_date, "%A"),
    mes=format(a30_date, "%B"),
    iniciales=a30_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="A30")

#C31
c31_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by) %>% 
  filter(as.Date(c31_date)>=fecha_conteos) %>% mutate(
    dia=format(c31_date, "%A"),
    mes=format(c31_date, "%B"),
    iniciales=c31_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C31")

#C32
c32_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by) %>% 
  filter(as.Date(c32_date)>=fecha_conteos) %>% mutate(
    dia=format(c32_date, "%A"),
    mes=format(c32_date, "%B"),
    iniciales=c32_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C32")

#C33
c33_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by) %>% 
  filter(as.Date(c33_date)>=fecha_conteos) %>% mutate(
    dia=format(c33_date, "%A"),
    mes=format(c33_date, "%B"),
    iniciales=c33_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C33")


#C35
c35_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by) %>% 
  filter(as.Date(c35_date)>=fecha_conteos) %>% mutate(
    dia=format(c35_date, "%A"),
    mes=format(c35_date, "%B"),
    iniciales=c35_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="C35")

#B10
b10_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by) %>% 
  filter(as.Date(b10_date)>=fecha_conteos) %>% mutate(
    dia=format(b10_date, "%A"),
    mes=format(b10_date, "%B"),
    iniciales=b10_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="B10")

#B10
b10o_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(b10_date_2)) %>% select(id, b10_date_2, b10_by_2) %>% 
  filter(as.Date(b10_date_2)>=fecha_conteos) %>% mutate(
    dia=format(b10_date_2, "%A"),
    mes=format(b10_date_2, "%B"),
    iniciales=b10_by_2
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="B10OWA")

#H41
h41_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by) %>% 
  filter(as.Date(h41_date)>=fecha_conteos) %>% mutate(
    dia=format(h41_date, "%A"),
    mes=format(h41_date, "%B"),
    iniciales=h41_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H41")
#H42
h42_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by) %>% 
  filter(as.Date(h42_date)>=fecha_conteos) %>% mutate(
    dia=format(h42_date, "%A"),
    mes=format(h42_date, "%B"),
    iniciales=h42_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H42")

#H43
h43_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h43_date)) %>% select(id, h43_date, h43_by) %>% 
  filter(as.Date(h43_date)>=fecha_conteos) %>% mutate(
    dia=format(h43_date, "%A"),
    mes=format(h43_date, "%B"),
    iniciales=h43_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H43")
#H41b
h41b_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by) %>% 
  filter(as.Date(h41b_date)>=fecha_conteos) %>% mutate(
    dia=format(h41b_date, "%A"),
    mes=format(h41b_date, "%B"),
    iniciales=h41b_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H41b")

#H57
h57_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(h57_date)) %>% select(id, h57_date, h57_by) %>% 
  filter(as.Date(h57_date)>=fecha_conteos) %>% mutate(
    dia=format(h57_date, "%A"),
    mes=format(h57_date, "%B"),
    iniciales=h57_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="H57")

#E1
e1_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by) %>% 
  filter(as.Date(e1_date)>=fecha_conteos) %>% mutate(
    dia=format(e1_date, "%A"),
    mes=format(e1_date, "%B"),
    iniciales=e1_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="E1")

#E2
e2_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by) %>% 
  filter(as.Date(e2_date)>=fecha_conteos) %>% mutate(
    dia=format(e2_date, "%A"),
    mes=format(e2_date, "%B"),
    iniciales=e2_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="E2")

#E3
e3_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(e3_date)) %>% select(id, e3_date, e3_by) %>% 
  filter(as.Date(e3_date)>=fecha_conteos) %>% mutate(
    dia=format(e3_date, "%A"),
    mes=format(e3_date, "%B"),
    iniciales=e3_by
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="E3")

#E3 owa
e3o_24m_realizados<- gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>% 
  filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_by_o) %>% 
  filter(as.Date(e3_date_o)>=fecha_conteos) %>% mutate(
    dia=format(e3_date_o, "%A"),
    mes=format(e3_date_o, "%B"),
    iniciales=e3_by_o
  )%>% group_by(
    mes, dia, iniciales
  ) %>% count() %>% mutate(crf="E3OWA")

all_crf_sent_24<-s4_24m_realizados %>% bind_rows(
  list(
    m11_24m_realizados,
    m14a_24m_realizados,
    m14b_24m_realizados,
    a23_24m_realizados,
    a24a_24m_realizados,
    a24b_24m_realizados,
    a26a_24m_realizados,
    a30_24m_realizados,
    c31_24m_realizados,
    c32_24m_realizados,
    c33_24m_realizados,
    c35_24m_realizados,
    b10_24m_realizados,
    h41_24m_realizados,
    h42_24m_realizados,
    h43_24m_realizados,
    h57_24m_realizados,
    e1_24m_realizados,
    e2_24m_realizados,
    e3_24m_realizados,
    b10o_24m_realizados,
    h41b_24m_realizados,
    e3o_24m_realizados
  )
)

all_crf_sent_24 %>% writexl::write_xlsx(paste0("output/crf_sent_24_", 
                                               fecha_conteos,"_al_", Sys.Date(),".xlsx"))
