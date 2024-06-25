# Emory RedCap dictionary
#source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

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
#constante para calcular visita mensual
fecha1="2020-06-26"
fecha2="2020-07-02"
#ml <- 365.25 / 12
ml <- 30

#todas las visitas
all_c31_mensuales <- gt_emory_data_arm2 %>%
  filter(!is.na(c30_date)) %>%
  transmute(id, dob = as.Date(c30_dob), visit="m1") %>%
  complete(
    nesting(id, dob),
    visit = c("m1","m2", "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10","m11", "b4")
  ) %>% 
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(c31_date)) %>%
      filter(visit %in% c("m1","m2", "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10","m11", "b4")) %>%
      select(id, visit, c31 = c31_date)
  )%>%
  mutate(
    visit = factor(visit, levels = c("m1","m2", "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10","m11", "b4")),
    #definir ventana inicial
    visit_start = dob -
      #para cuadrar periodos de ventana en los b1 en adelante
      7 +
      recode(
        visit,
        m1 = ml,
        m2=  2 * ml,
        b1 = 3 * ml,
        m4 = 4 * ml,
        m5 = 5 * ml,
        b2 = 6 * ml,
        m7 = 7 * ml,
        m8 = 8 * ml,
        b3 = 9 * ml,
        m10 = 10 * ml,
        m11 = 11 * ml,
        b4 = (12 * ml) 
      ),
    #definir fin de vetana
    visit_end = dob +
      
      recode(
        visit,
        m1 = ml,
        m2=  2 * ml,
        b1 = 3 * ml,
        m4 = 4 * ml,
        m5 = 5 * ml,
        b2 = 6 * ml,
        m7 = 7 * ml,
        m8 = 8 * ml,
        b3 = 9 * ml,
        m10 = 10 * ml,
        m11 = 11 * ml,
        b4 = (12 * ml)
      ),
    #calcular visitas de la siguiente semana y anteriores
    # report_date = Sys.Date() %>%
    report_date = as.Date(fecha1) %>%
      lubridate::ceiling_date(unit = "weeks") %>%
      magrittr::add(0)
    # ,
    # entered_window = visit_start < fecha1
    #calcular visitas de la semana 2
    # report_date_2 = Sys.Date() %>%
    #     lubridate::ceiling_date(unit = "weeks") %>%
    #     magrittr::add(14),
    # entered_window_2 = visit_start < report_date_2 & visit_start >= report_date
  ) %>%
  arrange(id, visit) %>% 
  group_by(id) %>%
  mutate(
    #reglas para quedarse con la ultima visita pendiente que aun es valida
    expected = is.na(c31) &
#      entered_window &
      case_when(
        visit == "m1" ~ is.na(c31[visit=="m2"]) & is.na(c31[visit=="b1"]) &
          is.na(c31[visit=="m4"]) &
          is.na(c31[visit=="m5"]) &
          is.na(c31[visit=="b2"]) &
          is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m2" ~ is.na(c31[visit=="b1"]) &
          is.na(c31[visit=="m4"]) &
          is.na(c31[visit=="m5"]) &
          is.na(c31[visit=="b2"]) &
          is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "b1" ~ is.na(c31[visit=="m4"]) &
          is.na(c31[visit=="m5"]) &
          is.na(c31[visit=="b2"]) &
          is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m4" ~ is.na(c31[visit=="m5"]) &
          is.na(c31[visit=="b2"]) &
          is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m5" ~ is.na(c31[visit=="b2"]) &
          is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "b2" ~ is.na(c31[visit=="m7"]) &
          is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m7" ~ is.na(c31[visit=="m8"]) &
          is.na(c31[visit=="b3"]) &
          is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m8" ~ is.na(c31[visit=="b3"]) &
         is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "b3" ~ is.na(c31[visit=="m10"]) &
          is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m10" ~ is.na(c31[visit=="m11"]) &
          is.na(c31[visit=="b4"]),
        visit == "m11" ~ is.na(c31[visit=="b4"]),
        visit == "b4" ~ TRUE
      )
  ) %>% 
  ungroup() %>%
  print()

#todas las visitas que aun no han pasado y que no se han hecho
all_c31_mensuales%>% filter(expected) %>% 
  transmute(id, fecha_nacimiento=dob, 
            #limites mas menos 7 dias para visualización del encuestador
            fecha_visita=visit_start+7, tipo_visita=visit, limite_inferior=visit_start, 
                                                    limite_superior=visit_end+22, dias_restantes=as.integer(limite_superior - as.Date(fecha2))-1) %>% 
 #quitar las salidas y donde falleció el bebe
   anti_join(
    bind_rows(
      list(
        salidas %>% select(id),
        gt_emory_data_arm2 %>%  
          filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
          select(id)
      )
    )
  ) %>% 
  #delimitar las menos 7 dias antes de la fecha2 y mayores a 22 días antes de la fecha2 (-7 a +22 dias)
  filter(fecha_visita<fecha2) %>% filter(as.Date(fecha_visita)>(as.Date(fecha2)-lubridate::days(22))) %>% 
  left_join(
    comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
  )%>% arrange((fecha_visita)) %>%  left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>%  write_csv("output/programacion_visitas_c31_mensuales_air.csv")

