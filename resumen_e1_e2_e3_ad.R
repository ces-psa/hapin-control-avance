# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

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

#agregar fecha a las Salidas
salidas_fecha<-salidas %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>%  transmute(id, fecha_salida=e3_date)
) 
#sacar todos los E2
eventos_e2<-gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date,e2_participant,e2_title, e2_type,e2_outcome ) %>% mutate(
  participante=recode(e2_participant,
                        "1"="Madre",
                      "2"="Mujer Mayor Adulta",
                      "3"="Nino"),
  titulo_evento=recode(
    e2_title,
    "1"="Quemadura (responda a PARTE B)",
    "2"="Muerte fetal (> 20 semanas de gestacion) (incluye muerte intrauterina/fallecimiento fetal)",
    "3"="Hemorragia post-parto",
    "4"="Septicemia",
    "5"="Intoxicacion por monoxido de carbono",
    "6"="Lesion [incluyendo no-violenta (por ejemplo, accidente automovilistico) y de violencia fisica]",
    "7"="Muerte",
    "555"="Otro"
  ),
  tipo=recode(
    e2_type,
    "1"="Muerte (no incluyendo muerte del feto)",
    "2"="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)",
    "3"="Situacion que pone en peligro la vida",
    "4"="Hospitalizacion nueva o prolongada",
    "5"="Discapacidad/incapacidad persistente o significativa",
    "6"="Anomalia congenita/defecto de nacimiento",
    "555"="Otro evento/experiencia medica importante"
  ),
  resuelto=recode(
    e2_outcome,
    "1"="Resuelto, sin secuelas",
    "2"="Resuelto con secuelas",
    "3"="Enveto continua",
    "4"="Muerte"
  )
) %>% transmute(id, tipo_evento="E2", fecha_evento=e2_date, participante, titulo_evento, tipo, resuelto) %>% 
  left_join(
     salidas_fecha %>% select(id, fecha_salida)
     ) %>% 
  mutate(
      fecha_salida=case_when(
        id=="35001" ~ "No aplica",
        id=="35021" ~ "No aplica",
        TRUE ~ as.character(fecha_salida)
      )
    )

eventos_e1<-gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_participant, e1_title, e1_category___1, e1_category___2, e1_category___3, e1_category___4, e1_category___5,
                                                          e1_category___6, e1_category___7,e1_outcome) %>% mutate(
                                                            e1_category___1=recode(e1_category___1, "1"= " Muerte, incluidos natimuerto (muerte fetal intrauterina) ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___2=recode(e1_category___2, "1"= " Situacion que pone en peligro la vida ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___3=recode(e1_category___3, "1"= " Hospitalizacion nueva o prolongada ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___4=recode(e1_category___4, "1"= " Discapacidad/incapacidad persistente o significativa ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___5=recode(e1_category___5, "1"= " Anomalia congenita/defecto de nacimiento ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___6=recode(e1_category___6, "1"= " Otro evento/experiencia medica importante ",
                                                                                   "0"=""
                                                            ),
                                                            e1_category___7=recode(e1_category___7, "1"= " Ninguno de los anteriores ",
                                                                                   "0"=""
                                                            ),
                                                           tipo=paste0(e1_category___1,e1_category___2,e1_category___3,
                                                                         e1_category___4,
                                                                         e1_category___5,e1_category___6,
                                                                         e1_category___7
                                                                         ),
                                                          participante=recode(e1_participant,
                                                           "1"="Madre",
                                                           "2"="Mujer Mayor Adulta",
                                                           "3"="Nino"
                                                          ),
                                                          titulo_evento=recode(e1_title, 
                                                           "1"= "Quemadura (responda a PARTE B)",
                                                           "2"="Aborto espontaneo (< 20 semanas de gestacion)",
                                                           "3"="Hipertension (presion arterial sistolica >=140 o presion arterial diastolica >=90 mmHg)",
                                                           "4"="Anemia severa (Hb < 7.0 g / dL)",
                                                           "5"="Desnutricion aguda grave",
                                                           "555"="Otro"
                                                          ),
                                                          resuelto=recode(
                                                            e1_outcome,
                                                            "1"= "Resuelto, sin secuelas",
                                                            "2"="Resuelto, con secuelas",
                                                            "3"="Evento Continua",
                                                            "4"="Muerte (Complete el formulario E2-EAG)"
                                                          )
) %>% transmute(id, tipo_evento="E1", fecha_evento=e1_date, participante,titulo_evento, tipo,resuelto) %>% 
  left_join(
    salidas_fecha %>% select(id, fecha_salida)
  ) %>% 
  mutate(
    fecha_salida=case_when(
      id=="35001" ~ "No aplica",
      id=="35021" ~ "No aplica",
      TRUE ~ as.character(fecha_salida)
    )
  )

eventos_e1 %>% bind_rows(
  eventos_e2
) %>% writexl::write_xlsx(
  paste0("output/Lista_eventos_salidas_al_",Sys.Date(),".xlsx")
)

#lista de eventos
eventos<-eventos_e1 %>% bind_rows(
  eventos_e2
) 

#filtrar solo las muertes y abortos
eventos %>% filter(grepl("^Muerte",tipo) | grepl("^Muerte",resuelto) | grepl("^Aborto", titulo_evento)) %>% transmute(id, tipo_crf=tipo_evento, fecha_evento, tipo,titulo_evento, fecha_salida) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% transmute(id, brazo=recode(s6_arm, "1"="Intervencion", "0"="Control"))
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,ID_tamizaje=`ID tamizaje`, Nombre=`Nombre embarazada`, Comunidad=`Comunidad embarazada (original z10)`, `Celular embarazada`, `Celular esposo`, Celular_otro_miembro )
) %>% select(
  ID_estudio=id, ID_tamizaje, Nombre, tipo_crf, fecha_evento, tipo, titulo_evento, fecha_salida, brazo, Comunidad,`Celular embarazada`, `Celular esposo`, Celular_otro_miembro
) %>% writexl::write_xlsx(paste0("output/reference/lista_muertes_al_",Sys.Date(),".xlsx"))


##SACAR TABLA PARA MORTINATOS Y MUERTES DE E2
#sacar todos los E2
e2_muertes<-gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date,e2_participant, e2_type, e2_death_date, e2_death_verify___1,
                                                                      e2_death_verify___2, e2_death_verify___3, e2_death_verify___4,
                                                                      e2_death_verify___5,e2_sae_nature ) %>% mutate(
                                                                        vm1=if_else(e2_death_verify___1==1, "Auto-informado por la familia", NA_character_),
                                                                        vm2=if_else(e2_death_verify___2==1, "Registro medico clinico", NA_character_),
                                                                        vm3=if_else(e2_death_verify___3==1, "Registro medico del hospital", NA_character_),
                                                                        vm4=if_else(e2_death_verify___4==1, "Informado por partera (para partos en domicilio)", NA_character_),
                                                                        vm5=if_else(e2_death_verify___5==1, "Certificado de muerte", NA_character_),
                                                                        verifico_muerte=paste0(vm1,",",vm2,",", vm3, ",", vm4,",",vm5 ) %>% na.omit()
                                                                      ) %>% select(id, e2_date, e2_participant, e2_type, e2_death_date, verifico_muerte,e2_sae_nature) %>%
  filter(e2_type==1 | e2_type==2) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, Fecha_nacimiento=c30_dob)
  ) %>% 
  mutate(
  participante=recode(e2_participant,
                      "1"="Madre",
                      "2"="Mujer Mayor Adulta",
                      "3"="Nino"),
  tipo=recode(
    e2_type,
    "1"="Muerte (no incluyendo muerte del feto)",
    "2"="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)",
    "3"="Situacion que pone en peligro la vida",
    "4"="Hospitalizacion nueva o prolongada",
    "5"="Discapacidad/incapacidad persistente o significativa",
    "6"="Anomalia congenita/defecto de nacimiento",
    "555"="Otro evento/experiencia medica importante"
  )
) %>% mutate_all(as.character) %>% mutate(
Edad=as.Date(e2_death_date) - as.Date(Fecha_nacimiento),
Edad_meses=as.numeric(Edad) %/% 30,
Edad_dias= as.numeric(Edad)%% 30
) %>% transmute(ID_HH=id, Fecha_Reporte_Evento=e2_date, Participante=participante, Tipo_EA=tipo,Fecha_nacimiento, Fecha_Muerte=e2_death_date, Edad_meses,Edad_dias, Como_se_identifico_el_EAS=verifico_muerte,Descripcion=e2_sae_nature)

e1_abortos<-gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_participant, e1_title ) %>% filter(e1_title==2) %>% 
  mutate( participante=recode(e1_participant,  
                              "1"="Madre"
                              ),
                                titulo_evento=recode(e1_title,
                                 "2"="Aborto espontaneo (< 20 semanas de gestacion)"
                                  )
                               ) %>% transmute(ID_HH=id, Fecha_Reporte_Evento=e1_date,Participante=participante, Tipo_EA=titulo_evento)

#generar tabla por rangos de fecha
fecha_inicio="2020-07-11"
fecha_fin="2020-10-14"
e2_muertes %>% mutate_all(as.character) %>% bind_rows(
  e1_abortos %>% mutate_all(as.character)
) %>%filter( Fecha_Reporte_Evento>=fecha_inicio &
  Fecha_Reporte_Evento<=fecha_fin
) %>% arrange(Fecha_Reporte_Evento) %>% 
  writexl::write_xlsx(paste0("output/eas/EAS_muertes_al_",fecha_fin,".xlsx"))

#esta parte sirvió para definir por grupo las muertes que han sucedido
# muertes<-e2_muertes %>% mutate_all(as.character) %>% bind_rows(
#   e1_abortos %>% mutate_all(as.character)
# ) %>%filter( Fecha_Reporte_Evento>=fecha_inicio &
#                Fecha_Reporte_Evento<=fecha_fin
# ) %>% arrange(Fecha_Reporte_Evento)
# 
# muertes %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(ID_HH=id, s6_arm) %>% mutate(Grupo=recode(
#     s6_arm,"1"="Intervencion", "0"="Control"
#   )) %>% select(ID_HH, Grupo)
# ) %>% writexl::write_xlsx(paste0("output/eas/EAS_muertes_al_",fecha_fin,".xlsx"))

#salidas de intervención que no fueron por muerte
# salidas %>% anti_join(
#   muertes %>% select(id=ID_HH)
# ) %>% anti_join(bind_rows(
#   gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id, h56_date),
#  salidas_uvg %>% filter(!is.na(h56g_date)) %>%  select(id=record_id, h56g_date)
# )
# ) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(ID_HH=id, s6_arm) %>% mutate(Grupo=recode(
#          s6_arm,"1"="Intervencion", "0"="Control"
#      )) %>% select(id=ID_HH, Grupo)
# ) %>% writexl::write_xlsx(paste0("output/eas/EAS_salidas_no-muerte_al_",fecha_fin,".xlsx"))

#e2_muertes %>% mutate(test=grepl("*NA*",Como_se_identifico_el_EAS)  ) %>% select(Como_se_identifico_el_EAS, test)

##TABLA MENSUAL PARA EAS
fecha_inicio="2020-07-11"
fecha_fin="2020-10-14"
  e2_otros<-gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date,e2_participant, e2_type, e2_start_date, e2_sae_nature ) %>% 
    filter(e2_type>2) %>% 
    mutate(
      participante=recode(e2_participant,
                          "1"="Madre",
                          "2"="Mujer Mayor Adulta",
                          "3"="Nino"),
      tipo=recode(
        e2_type,
        "1"="Muerte (no incluyendo muerte del feto)",
        "2"="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)",
        "3"="Situacion que pone en peligro la vida",
        "4"="Hospitalizacion nueva o prolongada",
        "5"="Discapacidad/incapacidad persistente o significativa",
        "6"="Anomalia congenita/defecto de nacimiento",
        "555"="Otro evento/experiencia medica importante"
      )
    ) %>%  left_join(
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, Fecha_nacimiento_nino=c30_dob)
    )  %>% 
    filter(e2_date>=fecha_inicio &
      e2_date<=fecha_fin
    ) %>%  transmute(
      ID_HH=id, Fecha_Evento=e2_date, Participante=participante, Tipo_EA=tipo, Fecha_inicio_evento=e2_start_date, Fecha_nacimiento_nino,
      Descripcion=e2_sae_nature
    )
e2_otros %>% writexl::write_xlsx(paste0("output/eas/otros_EAS_",fecha_fin,".xlsx"))  



#REVISAR AUTOPSIAS VERBALES
#Muertes en Utero
e2_muertes %>% filter(Tipo_EA=="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c80_date)) %>% select(ID_HH=id, c80_date, c80_by)
) %>% writexl::write_xlsx("output/eas/mortinatos_c80.xlsx")

#Muertes de 0 a 28 dias
e2_muertes %>% filter(Tipo_EA=="Muerte (no incluyendo muerte del feto)") %>% 
  filter(Edad_meses==0 & Edad_dias<=28) %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c81_date)) %>% select(ID_HH=id, c81_date, c81_by)
) %>% writexl::write_xlsx("output/eas/muertes_0-28dias_c81.xlsx")


#Muertes de>28 dias
e2_muertes %>% filter(Tipo_EA=="Muerte (no incluyendo muerte del feto)") %>% 
  filter(Edad_dias>=29 | Edad_meses>=1) %>%  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c82_date)) %>% select(ID_HH=id, c82_date, c82_by)
  ) %>% writexl::write_xlsx("output/eas/muertes_mayor_29dias_c82.xlsx")

e1_abortos %>% filter(Tipo_EA=="Aborto espontaneo (< 20 semanas de gestacion)") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c81_date)) %>% select(ID_HH=id, c81_date, c81_by)
) %>% writexl::write_xlsx("output/eas/abortos_c81.xlsx")


matriz_avances %>% select(id, fec_nacimiento, muerte_bebe, aborto, salida_anticipada) %>% 
  filter(
  !is.na(muerte_bebe) | !is.na(aborto) | !is.na(salida_anticipada)
) %>% left_join(
  e2_muertes %>% select(id=ID_HH,Fecha_Muerte)
)


gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(
  id, e3_reason
) %>% group_by(e3_reason) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(
  id, e3_reason_c
) %>% group_by(e3_reason_c) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(c30_dob))

