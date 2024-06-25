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
fecha1="2020-11-13"
fecha2="2020-11-19"
ml <- 365.25 / 12
#ml <- 308
#todas las visitas
all_c31 <- gt_emory_data_arm2 %>%
    left_join(
        gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
    ) %>% 
    left_join(
        gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
            gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
        ) %>% select(id, m17_ga)
    ) %>% mutate(
        dob=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob))
    ) %>% 
    transmute(id, dob, visit="m1") %>%
    complete(
        nesting(id, dob),
        visit = c("m1", "b1", "b2", "b3", "b4")
    ) %>% 
    left_join(
        gt_emory_data_arm2 %>%
            filter(!is.na(c31_date)) %>%
            filter(visit %in% c("m1", "b1", "b2", "b3", "b4")) %>%
            select(id, visit, c31 = c31_date)
        )%>%
    mutate(
        visit = factor(visit, levels = c("m1", "b1", "b2", "b3", "b4")),
       #definir ventana inicial
         visit_start = dob -
           #para cuadrar periodos de ventana en los b1 en adelante
            13 +
            recode(
                visit,
                m1 = ml,
                b1 = 3 * ml,
                b2 = 6 * ml,
                b3 = 9 * ml,
                b4 = (12 * ml) + 13
            ),
       #definir fin de vetana
       visit_end = dob +
            14 +
            recode(
                visit,
                m1 = ml,
                b1 = 3 * ml,
                b2 = 6 * ml,
                b3 = 9 * ml,
                b4 = 12 * ml
            ),
       #calcular visitas de la siguiente semana y anteriores
       # report_date = Sys.Date() %>%
       report_date = as.Date(fecha1) %>%
            lubridate::ceiling_date(unit = "weeks") %>%
            magrittr::add(0),
        entered_window = visit_start < fecha2
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
            entered_window &
            case_when(
                visit == "m1" ~ is.na(c31[visit=="b1"]) &
                    is.na(c31[visit=="b2"]) &
                    is.na(c31[visit=="b3"]) &
                    is.na(c31[visit=="b4"]),
                visit == "b1" ~ is.na(c31[visit=="b2"]) &
                    is.na(c31[visit=="b3"]) &
                    is.na(c31[visit=="b4"]),
                visit == "b2" ~ is.na(c31[visit=="b3"]) &
                    is.na(c31[visit=="b4"]),
                visit == "b3" ~ is.na(c31[visit=="b4"]),
                visit == "b4" ~ TRUE
            )
    ) %>% 
    ungroup() %>%
    print()

#todos los A23 para sacar listado de adultas
all_a23_solo_adulta <- gt_emory_data_arm2 %>%mutate(type = if_else(
    condition = grepl("^35", id),
    true = "oaw",
    false = "pw"
        ) 
    ) %>% filter(type=="oaw") %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
    ) %>% left_join(
        gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>%  select(id_tamizaje=id, id=s4_main_id) %>% left_join(
            gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
        ) %>% select(id, m17_ga)
    ) %>% mutate(
        dob=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob))
    ) %>% 
    transmute(id, dob, visit="b1") %>% left_join(
        gt_emory_data_arm2 %>% mutate(type = if_else(
            condition = grepl("^35", id),
            true = "oaw",
            false = "pw"
        ) ) %>% filter(type=="oaw") %>% 
            filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
            select(id, e3_date, e3_date_c)
        
    ) %>% left_join(
        gt_emory_data_arm2 %>% mutate(type = if_else(
            condition = grepl("^35", id),
            true = "oaw",
            false = "pw"
        ) ) %>% filter(type=="pw") %>% 
            filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
            select(id, e2_title, e1_title)
    ) %>% filter(!is.na(e3_date) | !is.na(e3_date_c)) %>% select(id, dob, visit) %>% anti_join(
        salidas %>% select(id)
    ) %>%
    complete(
        nesting(id, dob),
        visit = c( "b1", "b2", "b3", "b4")
    ) %>% 
    left_join(
        gt_emory_data_arm2 %>%
            filter(!is.na(a23_date)) %>%
            filter(visit %in% c("b1", "b2", "b3", "b4")) %>%
            select(id, visit, a23 = a23_date)
    )%>%
    mutate(
        visit = factor(visit, levels = c("b1", "b2", "b3", "b4")),
        #definir ventana inicial
        visit_start = dob -
            #para cuadrar periodos de ventana en los b1 en adelante
            13 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,
                b3 = 9 * ml,
                b4 = (12 * ml) + 13
            ),
        #definir fin de vetana
        visit_end = dob +
            14 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,
                b3 = 9 * ml,
                b4 = 12 * ml
            ),
        #calcular visitas de la siguiente semana y anteriores
        # report_date = Sys.Date() %>%
        report_date = as.Date(fecha1) %>%
            lubridate::ceiling_date(unit = "weeks") %>%
            magrittr::add(0),
        entered_window = visit_start < fecha2

    ) %>%
    arrange(id, visit) %>% group_by(id) %>% 
    mutate(
        #reglas para quedarse con la ultima visita pendiente que aun es valida
        expected = is.na(a23) &
            entered_window &
            case_when(
                
                visit == "b1" ~ is.na(a23[visit=="b2"]) &
                    is.na(a23[visit=="b3"]) &
                    is.na(a23[visit=="b4"]),
                visit == "b2" ~ is.na(a23[visit=="b3"]) &
                    is.na(a23[visit=="b4"]),
                visit == "b3" ~ is.na(a23[visit=="b4"]),
                visit == "b4" ~ TRUE
            )
    ) %>% 
    ungroup() %>%
    print()

#definir las visitas de la siguiente semana y vencidas que aun son validas, descartando las salidas, los abortos y agregando comunidades
c31_7_dias<-all_c31 %>% filter(expected) %>% transmute(house_id=id, fecha_nacimiento=dob, fecha_visita=visit_start+13, tipo_visita=visit, limite_inferior=visit_start, 
                      limite_superior=visit_end, dias_restantes=as.integer(limite_superior - as.Date(fecha2))) %>% 
                          anti_join(
                              bind_rows(
                                  list(
                                      salidas %>% select(house_id=id),
                                  gt_emory_data_arm2 %>% mutate(type = if_else(
                                      condition = grepl("^35", id),
                                      true = "oaw",
                                      false = "pw"
                                  ) ) %>% 
                                      filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
                                      select(house_id=id),
                                  gt_emory_data_arm2 %>% mutate(type = if_else(
                                      condition = grepl("^35", id),
                                      true = "oaw",
                                      false = "pw"
                                  ) ) %>% 
                                      filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
                                      select(house_id=id)
                              )
                          )
                          )%>% bind_rows(
                            all_a23_solo_adulta  %>% transmute(house_id=id, fecha_nacimiento=dob, fecha_visita=visit_start+13, tipo_visita=visit, limite_inferior=visit_start, 
                                            limite_superior=visit_end, dias_restantes=as.integer(limite_superior - as.Date(fecha2)))
                          ) %>% arrange(desc(house_id)) %>% 
                            left_join(
                          comunidades %>% select(house_id, id_tamizaje, comunidad_1= community, comunidad_2=community_new)
                      )%>% arrange(limite_inferior) %>% mutate(
                          fecha_limite=case_when(
                              tipo_visita=="m1" ~ as.Date(fecha_nacimiento)+ lubridate::days(89),
                              tipo_visita=="b1" ~ as.Date(fecha_nacimiento)+ lubridate::days(180),
                              tipo_visita=="b2" ~ as.Date(fecha_nacimiento)+ lubridate::days(272),
                              tipo_visita=="b3" ~ as.Date(fecha_nacimiento)+ lubridate::days(360),
                              
                              TRUE ~ as.Date(fecha_visita)+ lubridate::days(378)
                          )
                      ) %>% filter(as.Date(fecha1) - lubridate::days(1)<=as.Date(fecha_limite))
 ####REVISAR LA ADULTAS QUE YA SE HICIERON, VOY POR AQUI   

a23_7_dias<-all_a23_solo_adulta %>% filter(expected) %>% transmute(house_id=id, fecha_nacimiento=dob, fecha_visita=visit_start+13, tipo_visita=visit, limite_inferior=visit_start, 
                                           limite_superior=visit_end, dias_restantes=as.integer(limite_superior - as.Date(fecha2))) %>% 
    anti_join(
        bind_rows(
            list(
                salidas %>% mutate(type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ) ) %>% filter(type=="oaw") %>%  select(house_id=id),
                gt_emory_data_arm2 %>% mutate(type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ) ) %>% filter(type=="oaw") %>% 
                    filter(!is.na(e3_date_o)) %>%
                    select(house_id=id)
            )
        )
    )%>%  
    left_join(
        comunidades %>% select(house_id, id_tamizaje, comunidad_1= community, comunidad_2=community_new)
    )%>% arrange(limite_inferior) %>% mutate(
        fecha_limite=case_when(
           # tipo_visita=="m1" ~ as.Date(fecha_nacimiento)+ lubridate::days(89),
            tipo_visita=="b1" ~ as.Date(fecha_nacimiento)+ lubridate::days(180),
            tipo_visita=="b2" ~ as.Date(fecha_nacimiento)+ lubridate::days(272),
            tipo_visita=="b3" ~ as.Date(fecha_nacimiento)+ lubridate::days(360),
            
            TRUE ~ as.Date(fecha_visita)+ lubridate::days(378)
        )
    ) %>% filter(as.Date(fecha1) - lubridate::days(1)<=as.Date(fecha_limite))


#nuevo C31 solicitado por personal de Jalapa
c31_semanal_total<-c31_7_dias %>% select(house_id, id_tamizaje, fecha_nacimiento, 
                                         tipo_visita, inicio_ventana=limite_inferior, fin_ventana=limite_superior, 
                                         comunidad_1, comunidad_2, dias_restantes) %>% left_join(
                                             rutas %>% select(comunidad_1=comunidad, ruta)
                                         ) %>% arrange(ruta) %>% filter(inicio_ventana<=fecha1)
c31_semanal_vencidas<-c31_semanal_total %>% filter(dias_restantes < "0")%>% transmute(house_id, id_tamizaje, fecha_nacimiento, 
                                                                                      visita=tipo_visita, inicio_ventana, fin_ventana, comunidad_1, comunidad_2,
                                                                                      dias_fuera_ventana=dias_restantes * -1, ruta) %>% 
                                                                            left_join(
                                                                                datos_participantes %>% select(house_id=`ID estudio`, 
                                                                                                               tel_embarazada=`Celular embarazada`,
                                                                                                               tel_esposo=`Celular esposo`, 
                                                                                                               tel_otro_miembro=Celular_otro_miembro, 
                                                                                                               `Nombre embarazada`)
                                                                            )

c31_semanal_vigentes <- c31_semanal_total %>% filter(dias_restantes>="0") %>% transmute(house_id, id_tamizaje, fecha_nacimiento, 
                                                                                        visita=tipo_visita, inicio_ventana, fin_ventana, 
                                                                                        comunidad_1, comunidad_2,
                                                                                        ruta) %>% 
                                                            left_join(
                                                            datos_participantes %>% transmute(house_id=`ID estudio`, 
                                                                                           tel_embarazada=
                                                                                               `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                                                                           tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
                                                        )

list(
    "lista_semanal_vigentes"=c31_semanal_vigentes,
    "lista_semanal_vencidas"=c31_semanal_vencidas 
) %>% writexl::write_xlsx(paste0("output/visitas/programacion_visitas_clinca_del_", 
                            fecha1,"_al_",fecha2,".xlsx"))

#----------------------------------------------------------
#VISITAS MENSUALES DE ERICK
#----------------------------------------------------------
# Datos de Hercules
source(file = "scripts/0_get_hercules_data.R", encoding = "UTF-8")
#source(file = "scripts/plastics-locations.R", encoding = "UTF-8")
source(file = "scripts/0_get_consentimiento_intensivo.R", encoding = "UTF-8")
#source(file = "scripts/0_get_plastic_study_data.R", encoding = "UTF-8")
#reporte de consentimientos y B12 Hercules
data_hercules_gt<-gt_emory_data_arm1 %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id) %>% filter(!is.na(id_estudio)) %>% left_join(
    gt_emory_data_arm1 %>% select("id_tamizaje"=id, s1_date, s1_community_name) %>% filter(!is.na(s1_date))
) %>% left_join(
    gt_emory_data_arm2 %>% select("id_estudio"=id, s6_arm, s6_date) %>% filter(!is.na(s6_date)) %>% mutate(brazo_s6=if_else(s6_arm==1, "Intervencion", "Control"))
) %>% left_join(
    hercules_consentimiento %>% select("id_estudio"=record, h_consent_accept, h_consent_accept_desc, h_arms, h_date, h_id_hercules, h_iniciales ) %>% 
        mutate(
        acepta_consentimiento = if_else(
            h_consent_accept==1, "Si aceptó",
            "No aceptó"
        )
    )
    %>%  mutate(
            no_accept_desc = case_when(
            h_consent_accept_desc==1 ~ "No está interesado",
            h_consent_accept_desc==2 ~ "El esposo no aceptó",
            h_consent_accept_desc==3 ~ "No quieren más muestras",
            h_consent_accept_desc==4 ~ "Otro",
            TRUE ~ NA_character_
        )
    )
) %>% filter(!is.na(h_consent_accept)) %>% select(-h_consent_accept_desc, -h_consent_accept, -s6_arm, -h_arms)  %>% 
    left_join(
        hercules_data %>% mutate(
            fecha_segunda_visita = as.Date(date) + lubridate::days(180)
        ) %>% 
            select(
                "id_estudio"=record, "Fecha B12 biomuestras"= date, "Iniciales B12 biomuestras"=iniciales, visita, fecha_segunda_visita
            ) %>% mutate(
                Visita=case_when(
                    visita==1 ~ "Nacimiento",
                    visita==2 ~ "B12",
                    TRUE ~ NA_character_
                )
            )
    ) %>% left_join(
        hercules_b2 %>% filter(!is.na(date)) %>% select(h_id_hercules=record_id_hercules, f_visita=date ) %>% mutate(Hercules_B2_realizada=if_else(
            is.na(f_visita),"No","Si"
        ))
    ) %>% 
    left_join(
        comunidades %>% select(id_estudio,community)
    ) %>% select(id_tamizaje, id_estudio, "comunidad"=community, "grupo"=brazo_s6, "fecha_hercules"=h_date, "id_hercules"=h_id_hercules, "iniciales_crf_hercules"=h_iniciales,
                 acepta_consentimiento, "porque no acepta"=no_accept_desc, `Fecha B12 biomuestras`, `Iniciales B12 biomuestras`, Visita, fecha_segunda_visita, Hercules_B2_realizada)


#------------------------------
#VISITAS B1 A B4 DE EXPOSICION
#-----------------------------

#constante para calcular visita mensual
ml <- 365.25 / 12

#todas las visitas h41 de b1 a b4 de exposición
all_b1_b4 <- gt_emory_data_arm2 %>%
  left_join(
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% left_join(
      gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
          gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
      ) %>% select(id, m17_ga)
  ) %>% mutate(
      dob=if_else(is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob))
  ) %>% 
    transmute(id, dob, visit="b1") %>%
    complete(
        nesting(id, dob),
        visit = c("b1", "b2",  "b4")
    ) %>% 
    left_join(
        gt_emory_data_arm2 %>%
            filter(!is.na(h41_date)) %>%
            filter(visit %in% c("b1", "b2", "b4")) %>%
            select(id, visit, h41 = h41_date)
           
    ) %>%
    mutate(
        visit = factor(visit, levels = c("b1", "b2", "b4")),
        #definir ventana inicial
        visit_start = dob -
            28 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,

                b4 = 12 * ml
            ),
        #definir fin de vetana
        visit_end = dob +
            28 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,

                b4 = 12 * ml
            ),
        #calcular visitas de la siguiente semana y anteriores
        report_date = Sys.Date() %>%
            lubridate::ceiling_date(unit = "weeks") %>%
            magrittr::add(7),
        entered_window = visit_start < report_date
    ) %>%
    arrange(id, visit) %>%
    group_by(id) %>%
    mutate(
        #reglas para quedarse con la ultima visita pendiente que aun es valida
        expected = is.na(h41) &
            entered_window &
            case_when(
                visit == "b1" ~ is.na(h41[visit=="b2"]) &
                    is.na(h41[visit=="b4"]),
                visit == "b2" ~ is.na(h41[visit=="b4"]),
                visit == "b4" ~ TRUE
            )
    ) %>%
    ungroup() %>%
    print()

#LISTADOS DE VISITAS DURANTE CUARENTENA
#todas las visitas h41 de b1 a b4 de exposición
all_H55_b1_b4 <- gt_emory_data_arm2 %>%
    filter(!is.na(c30_date)) %>%
    transmute(id, dob = as.Date(c30_dob), visit="b1") %>%
    complete(
        nesting(id, dob),
        visit = c("b1", "b2",  "b4")
    ) %>% 
    left_join(
        gt_emory_data_arm2 %>%
            filter(!is.na(h55_date)) %>%
            filter(visit %in% c("b1", "b2", "b4")) %>%
            select(id, visit, h41 = h55_date)
        
    ) %>%
    mutate(
        visit = factor(visit, levels = c("b1", "b2", "b4")),
        #definir ventana inicial
        visit_start = dob -
            28 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,
                
                b4 = 12 * ml
            ),
        #definir fin de vetana
        visit_end = dob +
            28 +
            recode(
                visit,
                b1 = 3 * ml,
                b2 = 6 * ml,
                
                b4 = 12 * ml
            ),
        #calcular visitas de la siguiente semana y anteriores
        report_date = Sys.Date() %>%
            lubridate::ceiling_date(unit = "weeks") %>%
            magrittr::add(7),
        entered_window = visit_start < report_date
    ) %>%
    arrange(id, visit) %>%
    group_by(id) %>%
    mutate(
        #reglas para quedarse con la ultima visita pendiente que aun es valida
        expected = is.na(h41) &
            entered_window &
            case_when(
                visit == "b1" ~ is.na(h41[visit=="b2"]) &
                    is.na(h41[visit=="b4"]),
                visit == "b2" ~ is.na(h41[visit=="b4"]),
                visit == "b4" ~ TRUE
            )
    ) %>%
    ungroup() %>%
    print()

#las visitas de b1 a b4 de exposición que se peuden hacer y las que han quedado vencidas pero aun estan vigentes
all_b1_b4 %>% filter(expected) %>% 
    #agregar id de tamizaje
    left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% 
    #agregar a que grupo pertenecen
    left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, brazo=s6_arm)
) %>% 
    #agregar comunidades
    left_join(
    comunidades %>% select(id=id_estudio,comunidad_1=community, comunidad_2=community_new)
) %>% 
    #agregar información de hercules
    left_join(
    data_hercules_gt %>% transmute(id=id_estudio, Hercules="Si", Hercules_B2_realizada)
 ) %>% anti_join(
    bind_rows(
    salidas %>% select(id),
    gt_emory_data_arm2 %>%
        filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
        select(id)) ) %>% 
    #exportar listado b1 a b4 para exposicion
    select(
    id_tamizaje, id, fecha_nacimiento=dob, visit_start, visit_end, tipo_visita=visit, brazo, comunidad_1, comunidad_2, Hercules, Hercules_B2_realizada
) %>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(id=record_id, participa_intensivo="Si")
) %>%  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
        select(id, e3_date, e3_date_o) %>% 
        anti_join(
            salidas %>% select(id)
            ) %>% 
        mutate(
            type = if_else(
                condition = grepl("^35", id),
                true = "oaw",
                false = "pw"
            ),
            participantes= case_when
            (
                type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
            ) ,
    ) %>% select(-e3_date, -e3_date_o, -type) ) %>%   writexl::write_xlsx(paste0("output/visitas/exposicion/visitas_exposicion_al_",
                                           lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
                                             "_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx")
)

#DURANTE CUARENTENA
#las visitas de b1 a b4 de exposición que se peuden hacer y las que han quedado vencidas pero aun estan vigentes USANDO H55  como referencia
all_H55_b1_b4 %>% filter(expected) %>% 
    #agregar id de tamizaje
    left_join(
        gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
    ) %>% 
    #agregar a que grupo pertenecen
    left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, brazo=s6_arm)
    ) %>% 
    #agregar comunidades
    left_join(
        comunidades %>% select(id=id_estudio,comunidad_1=community, comunidad_2=community_new)
    ) %>% 
    #agregar información de hercules
    left_join(
        data_hercules_gt %>% transmute(id=id_estudio, Hercules="Si", Hercules_B2_realizada)
    )  %>% anti_join(
        bind_rows(
            salidas %>% select(id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select(id)
        )
    ) %>% 
    #exportar listado b1 a b4 para exposicion
    transmute(
        id_tamizaje, id, fecha_nacimiento=dob, visit_start, visit_end, tipo_visita=visit, brazo, comunidad_1, comunidad_2, Hercules, Hercules_B2_realizada
        
    ) %>% left_join(
        intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(id=record_id, participa_intensivo="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type)
    ) %>% writexl::write_xlsx(paste0("output/visitas/exposicion/visitas_exposicion_h55_al_",
                                     lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
                                     "_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx")
    )

#                                                                     lubridate::floor_date(Sys.Date(), unit = "month", week_start = 1),".xlsx")) 
#-----------------------------------------------
#VISITAS P1 Y P2 EXPOSICION
###--------------------------------------
##VISITAS QUE LES TOCA P1 EXPOSICION
###--------------------------------------

#LISTA DE PENDIENTES P1 DE EXPOSICION
#H41 ACTUAL SEMANA MAS  7 DIAS
#--------------------------------------------
# extraer las inscritas
expo_p1_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id) 
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1)%>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p1", 
              "Edad en Semanas:"=edad_semanas, "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P1"=fecha_P1, 
              "Fecha limite P1"=fecha_limite_P1, Comunidad_1, Comunidad_2, brazo, tamizada_en_plasticos, participantes ) 
# %>%
#     writexl::write_xlsx(paste0("output/visitas/exposicion/p1/Visitas_P1_Exposicion_del_",
#                                lubridate::floor_date(Sys.Date()+7, unit = "weeks",
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))

#P1 SEMANA MAS  14 DIAS
#--------------------------------------------
# extraer las inscritas
expo_p1_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p1", "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P1"=fecha_P1, "Fecha limite P1"=fecha_limite_P1, Comunidad_1, Comunidad_2, brazo, tamizada_en_plasticos, participantes) 
# %>%
#     writexl::write_xlsx(paste0("output/visitas/exposicion/p1/Visitas_P1_Exposicion_del_",
#                                lubridate::floor_date(Sys.Date()+14, unit = "weeks",
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx"))

#P1 SEMANA MAS  21 DIAS
#--------------------------------------------
# extraer las inscritas
expo_p1_21_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1)%>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p1", "Edad en Semanas:"=edad_semanas, 
                                         "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P1"=fecha_P1, "Fecha limite P1"=fecha_limite_P1, Comunidad_1, Comunidad_2, brazo, tamizada_en_plasticos, participantes) 

#descontar los de la lista de 7 dias para dejar solo los de 14 dias
expo_p1_7_dias<-expo_p1_7_dias %>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)

expo_p1_14<-expo_p1_14_dias %>% anti_join(
    bind_rows(
        expo_p1_7_dias %>% select(HH_ID)
       
    )
 )
expo_p1_14<-expo_p1_14%>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)

expo_p1_21<-expo_p1_21_dias %>% anti_join(
    bind_rows(
        expo_p1_7_dias %>% select(HH_ID),
        expo_p1_14_dias %>% select(HH_ID),
        
    )
    
)

expo_p1_21<-expo_p1_21%>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)


#generar el archivo de excel
list(
    "P1_7_Dias"=expo_p1_7_dias ,
    "P1_14_Dias"=expo_p1_14,
    "P1_21_Dias"=expo_p1_21
) %>% writexl::write_xlsx(
    paste0("output/visitas/exposicion/p1/Visitas_P1_Exposicion_del_",
           lubridate::floor_date(Sys.Date()+14, unit = "weeks",
                 week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),"_y_",
                                         lubridate::floor_date(Sys.Date()+21, unit = "weeks",
                                                week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+28, unit = "weeks", week_start = 7),".xlsx")
)


#P2 exposición
#------------------------------
#semana actual mas 7 dias
#.----------------------------
# extraer las inscritas
expo_p2_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new )
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1)  %>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p2","Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P2"=fecha_P2, "Fecha limite P2"=fecha_limite_P2, Comunidad_1, Comunidad_2, brazo,tamizada_en_plasticos, participantes) 
    
#------------------------------
#semana actual mas 14 dias
#.----------------------------
# extraer las inscritas
expo_p2_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new )
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p2","Edad en Semanas:"=edad_semanas, 
                    "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P2"=fecha_P2, "Fecha limite P2"=fecha_limite_P2, Comunidad_1, Comunidad_2, brazo,tamizada_en_plasticos, participantes) 

#agregar candidatas de plasticos
expo_p2_7_dias<-expo_p2_7_dias %>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)

expo_p2_14<-expo_p2_14_dias %>% anti_join(
    expo_p2_7_dias %>% select(HH_ID)
)

expo_p2_14<-expo_p2_14%>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)

#------------------------------
#semana actual mas 21 dias
#.----------------------------
# extraer las inscritas
expo_p2_21_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select("id_estudio"=id, s6_arm) %>% mutate(
            brazo=recode(
                s6_arm, "1"= "Intervencion", "0"="Control"
            )
        )
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(h41_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new )
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1)  %>% left_join(
        plastic_tamizaje %>% transmute(id_estudio=record, tamizada_en_plasticos="Si")
    ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_o)) %>% 
            select(id, e3_date, e3_date_o) %>% 
            anti_join(
                salidas %>% select(id)
            ) %>% 
            mutate(
                type = if_else(
                    condition = grepl("^35", id),
                    true = "oaw",
                    false = "pw"
                ),
                participantes= case_when
                (
                    type=="oaw" & is.na(e3_date) & !is.na(e3_date_o) ~ "Solo Embarazada",
                    type=="oaw" & !is.na(e3_date) & is.na(e3_date_o) ~ "Solo Adulta",
                    type=="oaw" & is.na(e3_date) & is.na(e3_date_o) ~ "Embarazada y Adulta"
                ) 
            ) %>% select(-e3_date, -e3_date_o, -type) %>% select(id_estudio=id, participantes)
    ) %>% transmute("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, visit="p2","Edad en Semanas:"=edad_semanas, 
                    "Dias:"=edad_dias, "GA"= edad_concat,"Fecha minima P2"=fecha_P2, "Fecha limite P2"=fecha_limite_P2, Comunidad_1, Comunidad_2, brazo,tamizada_en_plasticos, participantes) 

#agregar candidatas de plasticos
expo_p2_21_dias<-expo_p2_21_dias %>% left_join(
    visitas %>% filter(is.na(tiene_salida)) %>% select(HH_ID=id,visit=visita_pendiente, Candidatas_plasticos=cantidad_candidatas_en_hogar )
)%>% left_join(
    intensivo_consentimiento %>% filter(ie_enroll_accept_m=="1") %>%  transmute(HH_ID=record_id, participa_intensivo="Si")
)

expo_p2_21<-expo_p2_21_dias %>% anti_join(
    bind_rows(
        expo_p2_7_dias %>% select(HH_ID),
        expo_p2_14_dias %>% select(HH_ID)
    )
   
)

list(
    "p2_7_Dias"=expo_p2_7_dias,
    "p2_14_Dias"=expo_p2_14,
    "p2_21_Dias"=expo_p2_21
) %>% writexl::write_xlsx(
    paste0("output/visitas/exposicion/p2/Visitas_P2_Exposicion_del_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
                                            "_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),"_y_",
           lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1),
           "_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx")
)


###--------------------------------------
##VISITAS QUE LES TOCA P1 CLINICA
###--------------------------------------

#-------------------------------------
#VISITAS P1 CLINICA CON NUEVO CODIGO
#-----------------------------------

#LISTA DE PENDIENTES P1 DE CLINICA
#SEMANA MAS  7 DIAS
#--------------------------------------------
# extraer las inscritas
clinica_p1_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
    gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m11_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
            ) %>%   left_join(
                #Agregar la comunidad
                comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
            ) %>% #ordenar por edad gestacional
                    arrange(desc(edad_semanas)) %>% 
                            filter(flag_visita==1) %>% 
                            select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
                                   "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P1, "Fin_ventana"=fecha_limite_P1, Comunidad_1, Comunidad_2) %>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    ) %>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    )%>% arrange(Ruta)
# %>%
#                                             writexl::write_xlsx(paste0("output/visitas/p1/Visitas_P1_Clinica_del_",
#                                                                        lubridate::floor_date(Sys.Date()+7, unit = "weeks",
#                                                                             week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))


#SEMANA MAS  14 DIAS
#--------------------------------------------
# extraer las inscritas
clinica_p1_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m11_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32, "1","0"),
        edad_concat=paste0(edad_semanas," semanas con ", edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P1, "Fin_ventana"=fecha_limite_P1, Comunidad_1, Comunidad_2)%>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta)
# %>%
#     writexl::write_xlsx(paste0("output/visitas/p1/Visitas_P1_Clinica_del_",
#                                lubridate::floor_date(Sys.Date()+14, unit = "weeks",
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx"))

clinica_p1_14 <- clinica_p1_14_dias %>% anti_join(
    clinica_p1_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1) %>%  select(HH_ID)
)
 

list(
    "P1_7_dias"=clinica_p1_7_dias%>% filter(as.Date(Inicio_ventana)<=fecha1),
    "P1_14_dias"= clinica_p1_14
) %>% writexl::write_xlsx(
    paste0("output/visitas/p1/Visitas_P1_Clinica2_del_", lubridate::floor_date(Sys.Date()+7, unit = "weeks",
                                                week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7), "_y_",
                 lubridate::floor_date(Sys.Date()+14, unit = "weeks",
                     week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),
                        ".xlsx")
)

# clinica_p1_7_dias %>% transmute(id_tamizaje=`ID tamizaje`, HH_ID, `Edad en Semanas:`, `Dias:`,GA, tipo_visita="p1", limite_inferior=`Fecha minima P1`, limite_superior=`Fecha limite P1`, Comunidad_1, Comunidad_2) %>% 
#     bind_rows(
# c31_semanal_total %>% select(id_tamizaje, HH_ID=house_id,  fecha_nacimiento, tipo_visita, limite_inferior, limite_superior, dias_restantes, Comunidad_1=comunidad_1, Comunidad_2=comunidad_2, fecha_limite)
# )
#PENDIENTES P1 DE ULTRASONIDO
#SEMANA ACTUAL MAS 7 DIAS
#--------------------------------------------------------
# extraer las inscritas
ultra_p1_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m18_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P1, "Fin_ventana"=fecha_limite_P1, Comunidad_1, Comunidad_2 )%>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta) 
# %>%
#         writexl::write_xlsx(paste0("output/visitas/p1/Visitas_P1_Ultra_del_",
#                                lubridate::floor_date(Sys.Date()+7, unit = "weeks",
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))


#SEMANA ACTUAL MAS 14 DIAS
#--------------------------------------------------------
# extraer las inscritas
ultra_p1_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m18_date) & redcap_event_name=="p1_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P1=fga + lubridate::days(168),
        fecha_limite_P1=fga+lubridate::days(196),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=24 & edad_semanas<32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad"=community)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P1, "Fin_ventana"=fecha_limite_P1, Comunidad)%>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta) 
# %>%
#     writexl::write_xlsx(paste0("output/visitas/p1/Visitas_P1_Ultra_del_",
#                                lubridate::floor_date(Sys.Date()+14, unit = "weeks",
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx"))

ultra_p1_14<-ultra_p1_14_dias %>% anti_join(
    ultra_p1_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1) %>%  select(HH_ID)
)

list(
    "P1_7_dias"= ultra_p1_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1),
    "P1_14_dias"=ultra_p1_14
) %>% writexl::write_xlsx(
    paste0("output/visitas/p1/Visitas_P1_Ultra_del_",
           lubridate::floor_date(Sys.Date()+7, unit = "weeks",
                 week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),"_y_",
           lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),
           ".xlsx")
)



##----------------------------------------
##PENDIENTES DE P2
##---------------------------------------
#LISTA DE P2 SEMANAL DE CLINICA
#P1 SEMANA ACTUAL MAS 7 DIAS
#--------------------------------------------------------
# extraer las inscritas
clinica_p2_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m11_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new )
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P2, "Fin_ventana"=fecha_limite_P2, Comunidad_1, Comunidad_2) %>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta) 
#%>% 
 #   writexl::write_xlsx(paste0("output/visitas/p2/Visitas_P2_Clinica_del_",
  #                             lubridate::floor_date(Sys.Date()+7, unit = "weeks", 
   #                                                  week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))
#---------------------------------------
#P1 CLINICA SEMANA ACTUAL MAS 14 DIAS
#--------------------------------------
# extraer las inscritas
clinica_p2_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m11_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new )
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P2, "Fin_ventana"=fecha_limite_P2, Comunidad_1, Comunidad_2) %>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta) 
#%>% 
 #   writexl::write_xlsx(paste0("output/visitas/p2/Visitas_P2_Clinica_del_",
  #                             lubridate::floor_date(Sys.Date()+14, unit = "weeks", 
   #                                                  week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx"))

clinica_p2_14<-clinica_p2_14_dias %>% anti_join(
    clinica_p2_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1) %>%  select(HH_ID)
)

list(
    "P2_7_dias"=clinica_p2_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1),
    "P2_14_dias"=clinica_p2_14
) %>% writexl::write_xlsx(
    paste0("output/visitas/p2/Visitas_P2_Clinica_del_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", 
                 week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),"_y_",
           lubridate::floor_date(Sys.Date()+14, unit = "weeks", 
                     week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),
           ".xlsx")
)

#archivo integrado para planificacion de P1 hasta B4
# visitas_semana_actual<-clinica_p1_7_dias %>% transmute(id_tamizaje=`ID tamizaje`, HH_ID, `Edad en Semanas:`, `Dias:`,GA, fecha_nacimiento="",tipo_visita="p1", limite_inferior=`Fecha minima P1`, 
#                                 limite_superior=`Fecha limite P1`, diferencia=as.Date(limite_superior)-as.Date(fecha2), dias_restantes=as.numeric(diferencia),Comunidad_1, Comunidad_2) %>% 
#     bind_rows(
#         list(
#             clinica_p2_14 %>% transmute(id_tamizaje=`ID tamizaje`, HH_ID, `Edad en Semanas:`, `Dias:`,GA,fecha_nacimiento="",tipo_visita="p2", limite_inferior=`Fecha minima P2`, 
#                                         limite_superior=`Fecha limite P2`, diferencia=as.Date(limite_superior)-as.Date(fecha2), dias_restantes=as.numeric(diferencia), Comunidad_1, Comunidad_2),
#             c31_semanal_vigentes %>% transmute(id_tamizaje, HH_ID=house_id,  fecha_nacimiento=as.character(fecha_nacimiento), tipo_visita, limite_inferior, limite_superior, 
#                                                dias_restantes=as.numeric(dias_restantes), Comunidad_1=comunidad_1, Comunidad_2=comunidad_2)
#              )
#     ) %>% select(-diferencia) %>% filter(as.Date(limite_inferior)<=as.Date(fecha2))
#generar excel integrado
# list(
#     p1_b4_vigentes=visitas_semana_actual,
#     m1_b4_vencidas=c31_semanal_vencidas
# ) %>% writexl::write_xlsx(paste0("output/visitas/integradas/lista_visitas_p1_b4_del_",fecha1,"_al_",fecha2,".xlsx"))
 

#PENDIENTES P2 DE ULTRASONIDO
# P2 ULTRASONIDO SEMANA ACTUAL MAS 7 DIAS
# extraer las inscritas
ultra_p2_7_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m18_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P2, "Fin_ventana"=fecha_limite_P2, Comunidad_1, Comunidad_2) %>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta)
# %>% 
#     writexl::write_xlsx(paste0("output/visitas/p2/Visitas_P2_Ultra_del_",
#                                lubridate::floor_date(Sys.Date()+7, unit = "weeks", 
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))

# P2 ULTRASONIDO SEMANA ACTUAL MAS 14 DIAS
# extraer las inscritas
ultra_p2_14_dias<-gt_emory_data_arm1 %>% select(
    "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
) %>% filter(!is.na(id_estudio)) %>% 
    #para revisar edad gestacional
    left_join(
        gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% 
    #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
    anti_join(
        bind_rows(
            salidas %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
                select("id_estudio"=id),
            gt_emory_data_arm2 %>% filter(!is.na(m18_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
        )
        
    )  %>% 
    #revisar edad gestacional en semanas
    mutate(
        fga= as.Date(m17_ga) - lubridate::days(280),
        dias =  lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 1) - fga ,
        edad_semanas= as.numeric(dias) %/% 7,
        edad_dias = as.numeric(dias)%% 7,
        fecha_P2=fga + lubridate::days(224),
        fecha_limite_P2=fga+lubridate::days(252),
        #flag_visita1= lubridate::floor_date(fecha_P1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #flag_visita2= lubridate::floor_date(fecha_limite_p1, unit="weeks", week_start = 1) == lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
        #Identificar las que les corresponde visita en la semana
        flag_visita=if_else(edad_semanas>=32,"1","0"),
        edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
    ) %>%   left_join(
        #Agregar la comunidad
        comunidades %>% transmute("id_estudio"=house_id, "Comunidad_1"=community, "Comunidad_2"=community_new)
    ) %>% #ordenar por edad gestacional
    arrange(desc(edad_semanas)) %>% 
    filter(flag_visita==1) %>% 
    select("ID tamizaje"=id_tamizaje, "HH_ID"=id_estudio, "Edad en Semanas:"=edad_semanas, 
           "Dias:"=edad_dias, "GA"= edad_concat,"Inicio_ventana"=fecha_P2, "Fin_ventana"=fecha_limite_P2, Comunidad_1, Comunidad_2) %>% 
    left_join(
        datos_participantes %>% transmute(HH_ID=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    )%>% left_join(
        rutas %>% transmute(Comunidad_1=comunidad, Ruta=ruta)
    ) %>% arrange(Ruta) 
# %>% 
#     writexl::write_xlsx(paste0("output/visitas/p2/Visitas_P2_Ultra_del_",
#                                lubridate::floor_date(Sys.Date()+14, unit = "weeks", 
#                                                      week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),".xlsx"))

ultra_p2_14<-ultra_p2_14_dias %>% anti_join(
    ultra_p2_7_dias %>% filter(as.Date(Inicio_ventana)<=fecha1) %>%  select(HH_ID)
)

list(
    "P2_7_dias"=ultra_p2_7_dias%>% filter(as.Date(Inicio_ventana)<=fecha1),
    "P2_14_dias"=ultra_p2_14
) %>% writexl::write_xlsx(
    paste0("output/visitas/p2/Visitas_P2_Ultra_del_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", 
                            week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),"_y_",
           lubridate::floor_date(Sys.Date()+14, unit = "weeks", 
                week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+21, unit = "weeks", week_start = 7),       
           ".xlsx")
)

###--------------------------------------
##VISITAS QUE LES TOCA B1 , B2 , B3 y B4
###--------------------------------------
#NUEVO CODIGO PARA B1 A B4

#constante para calcular visita mensual
# ml <- 365.25 / 12
# 
# #todas las visitas
# all_c32_b1_b4 <- gt_emory_data_arm2 %>%
#     filter(!is.na(c30_date)) %>%
#     transmute(id, dob = as.Date(c30_dob), visit="b1") %>%
#     complete(
#         nesting(id, dob),
#         visit = c("b1", "b2", "b3", "b4")
#     ) %>% 
#     left_join(
#         gt_emory_data_arm2 %>%
#             filter(!is.na(c32_date)) %>%
#             filter(visit %in% c("b1", "b2", "b3", "b4")) %>%
#             select(id, visit, c32 = c32_date)
#             
#     ) %>%
#     left_join(
#         gt_emory_data_arm2 %>%
#             filter(!is.na(a24a_date)) %>%
#             filter(visit %in% c("b1", "b2", "b3", "b4")) %>%
#             select(id, visit, a24a_date)
#     ) %>% mutate(
#         c32=case_when(
#             !is.na(c32) ~ as.character(c32),
#             is.na(c32) & !is.na(a24a_date) ~ as.character(a24a_date),
#             TRUE ~ NA_character_
#         )
#     ) %>% 
#     mutate(
#         visit = factor(visit, levels = c( "b1", "b2", "b3", "b4")),
#         #definir ventana inicial
#         visit_start = dob -
#             13 +
#             recode(
#                 visit,
#                 m1 = ml,
#                 b1 = 3 * ml,
#                 b2 = 6 * ml,
#                 b3 = 9 * ml,
#                 b4 = 12 * ml
#             ),
#         #definir fin de vetana
#         visit_end = dob +
#             14 +
#             recode(
#                 visit,
#                 m1 = ml,
#                 b1 = 3 * ml,
#                 b2 = 6 * ml,
#                 b3 = 9 * ml,
#                 b4 = 12 * ml
#             ),
#         #calcular visitas de la siguiente semana y anteriores
#         report_date = Sys.Date() %>%
#             lubridate::ceiling_date(unit = "weeks") %>%
#             magrittr::add(0),
#         entered_window = visit_start < report_date,
#         #calcular visitas de la semana 2
#         report_date_2 = Sys.Date() %>%
#             lubridate::ceiling_date(unit = "weeks") %>%
#             magrittr::add(7),
#         entered_window_2 = visit_start < report_date_2 & visit_start >= report_date
#     ) %>%
#     arrange(id, visit) %>%
#     group_by(id) %>%
#     mutate(
#         #reglas para quedarse con la ultima visita pendiente que aun es valida
#         expected = is.na(c32) &
#             entered_window &
#             case_when(
#                 visit == "m1" ~ is.na(c32[visit=="b1"]) &
#                     is.na(c32[visit=="b2"]) &
#                     is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b1" ~ is.na(c32[visit=="b2"]) &
#                     is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b2" ~ is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b3" ~ is.na(c32[visit=="b4"]),
#                 visit == "b4" ~ TRUE
#             ),
#         expected_2 = is.na(c32) &
#             entered_window_2 &
#             case_when(
#                 visit == "m1" ~ is.na(c32[visit=="b1"]) &
#                     is.na(c32[visit=="b2"]) &
#                     is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b1" ~ is.na(c32[visit=="b2"]) &
#                     is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b2" ~ is.na(c32[visit=="b3"]) &
#                     is.na(c32[visit=="b4"]),
#                 visit == "b3" ~ is.na(c32[visit=="b4"]),
#                 visit == "b4" ~ TRUE
#             )
#     ) %>%
#     ungroup() %>%
#     print()
# #definir las visitas de la siguiente semana y vencidas que aun son validas, descartando las salidas, los abortos y agregando comunidades
# c32_7_dias<-all_c32_b1_b4 %>% filter(expected) %>% transmute(house_id=id, fecha_nacimiento=dob, fecha_visita=visit_start+4, tipo_visita=visit, limite_inferior=visit_start, 
#                                                        limite_superior=visit_end, dias_restantes=as.integer(limite_superior - Sys.Date())) %>% 
#     anti_join(
#         bind_rows(
#             gt_emory_data_arm2 %>%
#                 filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
#                 select(house_id=id),
#             gt_emory_data_arm2 %>%
#                 filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
#                 select(house_id=id)
#         )
#     ) %>% left_join(
#         comunidades %>% select(house_id, id_tamizaje, comunidad_1= community, comunidad_2=community_new)
#     )%>% arrange(limite_inferior)
# 
# #definir las visitas de la  semana 2 y vencidas que aun son validas, descartando las salidas, los abortos y agregando comunidades
# c32_14_dias<-all_c32_b1_b4 %>% filter(expected_2) %>% transmute(house_id=id, fecha_nacimiento=dob, fecha_visita=visit_start+4, tipo_visita=visit, 
#                                                           limite_inferior=visit_start, limite_superior=visit_end, dias_restantes=as.integer(limite_superior - Sys.Date())) %>% 
#     anti_join(
#         bind_rows( 
#             gt_emory_data_arm2 %>%
#                 filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
#                 select(house_id=id),
#             gt_emory_data_arm2 %>%
#                 filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
#                 select(house_id=id)
#         )
#     ) %>% left_join(
#         comunidades %>% select(house_id, id_tamizaje, comunidad_1= community, comunidad_2=community_new)
#     ) %>% arrange(limite_inferior)
# 
# 
# list("c32_7_dias"=c32_7_dias,
#      "c32_14_dias"=c32_14_dias 
# ) %>% writexl::write_xlsx(paste0("output/visitas/b1_b4/visitas_B1-B4_semana_del_",lubridate::floor_date(Sys.Date()+0, unit = "weeks", week_start = 1),
#                                  "_al_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 7),".xlsx"))
# 






#---------------------------------------------------------------
###Visitas B1 y B2 en un mismo archivo
# los datos como en control avance
#----------------------------------------------------------------------------------------------------------

# gt_emory_data_arm1 %>%select("id_tamizaje"=id, "id"=s4_main_id) %>% filter(!is.na(id)) %>% left_join(
#     gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
# ) %>% left_join(
#     gt_emory_data_arm2 %>% select(id, c30a_date) %>% filter(!is.na(c30a_date))
# ) %>% mutate(c30_date=if_else(!is.na(c30_date),c30_date, c30a_date)) %>% 
#     # los nacimientos
#     filter(!is.na(c30_date)) %>%
#     select(id_tamizaje, id, c30_date) %>%
#     anti_join(
#         bind_rows(
#             salidas %>% select(id),
#             gt_emory_data_arm2 %>%
#                 filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
#                 select(id)
#         )
#     )  %>%
#     # agregar variable del mes
#     mutate(mes = NA) %>%
#     # agregar fecha de cada mes de edad
#     complete(
#         # usando solo las combinaciones existentes de id y c30_date
#         nesting(id, id_tamizaje, c30_date),
#         # agregar todos los meses del estudio
#         mes = 1:12
#     ) %>%
#     # calcular fecha de visitas mensuales
#     mutate(
#         fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
#         visita = case_when(
#             # visitas b1, b2, b3, b4
#             mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
#             # resto de visitas mensuales
#             TRUE ~ paste0("m", mes)
#         ),
#         # ¿se debe de visitar esta semana?
#         flag_visita = lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) == lubridate::floor_date(fecha_visita, unit = "weeks", week_start = 1)
#     )  %>% filter(flag_visita=="TRUE") %>% 
#     #eliminar los c31 que ya se hicieron
#     left_join(
#         gt_emory_data_arm2 %>% select(id,m11_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(m11_date))
#     ) %>% mutate(
#         visit=if_else(
#             condition = is.na(visit), 
#             true = "pendiente", 
#             false = as.character(visit)
#         )
#     ) %>% 
#     #quitamos los B3 que ya se hicieron
#     filter(visita!=visit) %>% anti_join(
#         gt_emory_data_arm2 %>% filter(visit=="b3" & !is.na(c32_date)) %>% select(id)
#     ) %>% 
#     #agregar id de tamizaje y comunidad
#     left_join(
#         comunidades %>% mutate(id=house_id)
#     ) %>% 
#     #agregar fechas limite para visita
#     mutate(
#         limite_inferior= fecha_visita - lubridate::days(3),
#         limite_superior= fecha_visita + lubridate::days(3),
#         dias=as.numeric(limite_superior - Sys.Date())
#     ) %>%
#     #generar listado
#     select(id_tamizaje,id, fecha_nacimiento=c30_date, mes, fecha_visita, 
#            tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias,
#            id_tamizaje, "Comunidad"=community)%>% filter( mes=="3" | mes=="6" | mes=="9" | mes=="12") %>% arrange(`dias restantes`) #%>% 
#     writexl::write_xlsx(paste0("output/visitas/b1_b4/visitas_B1-B4_semana_del_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),
#                                "_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))
#-----------------------------------------
#Visistas B2
#---------------------------------------
# los datos como en control avance
#----------------------------------------
# gt_emory_data_arm2 %>%
#     # los nacimientos
#     filter(!is.na(c30_date)) %>%
#     select(id, c30_date) %>%
#     # quitar las salidas
#     left_join(
#         gt_emory_data_arm2 %>%
#             filter(!is.na(e3_date_c)) %>%
#             select(id, e3_date_c)
#     ) %>% {
#         # si hay salidas, mostrarlas
#         if(any(!is.na(.$e3_date_c))) filter(., !is.na(e3_date_c)) %>% print()
#         # seguir con todos los datos
#         .
#     } %>%
#     filter(is.na(e3_date_c)) %>% 
#     select(-e3_date_c) %>%
#     # agregar variable del mes
#     mutate(mes = NA) %>%
#     # agregar fecha de cada mes de edad
#     complete(
#         # usando solo las combinaciones existentes de id y c30_date
#         nesting(id, c30_date),
#         # agregar todos los meses del estudio
#         mes = 1:12
#     ) %>%
#     # calcular fecha de visitas mensuales
#     mutate(
#         fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
#         visita = case_when(
#             # visitas b1, b2, b3, b4
#             mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
#             # resto de visitas mensuales
#             TRUE ~ paste0("m", mes)
#         ),
#         # ¿se debe de visitar esta semana?
#         flag_visita = lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) == lubridate::floor_date(fecha_visita, unit = "weeks", week_start = 1)
#     ) %>%  filter(flag_visita=="TRUE") %>% 
#     #eliminar los c31 que ya se hicieron
#     left_join(
#         gt_emory_data_arm2 %>% select(id,m11_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(m11_date))
#     ) %>% mutate(
#         visit=if_else(
#             condition = is.na(visit), 
#             true = "pendiente", 
#             false = as.character(visit)
#         )
#     ) %>% filter(visita!=visit) %>% 
#     #agregar id de tamizaje y comunidad
#     left_join(
#         comunidades %>% mutate(id=house_id)
#     ) %>% 
#     #agregar fechas limite para visita
#     mutate(
#         limite_inferior= fecha_visita - lubridate::days(3),
#         limite_superior= fecha_visita + lubridate::days(3),
#         dias=as.numeric(limite_superior - Sys.Date())
#     ) %>%
#     #generar listado
#     select(house_id=id, fecha_nacimiento=c30_date, mes, fecha_visita, 
#            tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias,
#            id_tamizaje, "Comunidad"=community)%>% filter( mes=="6") %>% arrange(`dias restantes`) #%>% 
    #writexl::write_xlsx(paste0("output/visitas_B2_semana_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))


#---------------------------------------------
#Visistas B3
#____________________________________
# los datos como en control avance
#----------------------------------------
# gt_emory_data_arm2 %>%
#     # los nacimientos
#     filter(!is.na(c30_date)) %>%
#     select(id, c30_date) %>%
#     # quitar las salidas
#     left_join(
#         gt_emory_data_arm2 %>%
#             filter(!is.na(e3_date_c)) %>%
#             select(id, e3_date_c)
#     ) %>% {
#         # si hay salidas, mostrarlas
#         if(any(!is.na(.$e3_date_c))) filter(., !is.na(e3_date_c)) %>% print()
#         # seguir con todos los datos
#         .
#     } %>%
#     filter(is.na(e3_date_c)) %>% 
#     select(-e3_date_c) %>%
#     # agregar variable del mes
#     mutate(mes = NA) %>%
#     # agregar fecha de cada mes de edad
#     complete(
#         # usando solo las combinaciones existentes de id y c30_date
#         nesting(id, c30_date),
#         # agregar todos los meses del estudio
#         mes = 1:12
#     ) %>%
#     # calcular fecha de visitas mensuales
#     mutate(
#         fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
#         visita = case_when(
#             # visitas b1, b2, b3, b4
#             mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
#             # resto de visitas mensuales
#             TRUE ~ paste0("m", mes)
#         ),
#         # ¿se debe de visitar esta semana?
#         flag_visita = lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) == lubridate::floor_date(fecha_visita, unit = "weeks", week_start = 1)
#     ) %>%  filter(flag_visita=="TRUE") %>% 
#     #eliminar los c31 que ya se hicieron
#     left_join(
#         gt_emory_data_arm2 %>% select(id,m11_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(m11_date))
#     ) %>% mutate(
#         visit=if_else(
#             condition = is.na(visit), 
#             true = "pendiente", 
#             false = as.character(visit)
#         )
#     ) %>% filter(visita!=visit) %>% 
#     #agregar id de tamizaje y comunidad
#     left_join(
#         comunidades %>% mutate(id=house_id)
#     ) %>% 
#     #agregar fechas limite para visita
#     mutate(
#         limite_inferior= fecha_visita - lubridate::days(3),
#         limite_superior= fecha_visita + lubridate::days(3),
#         dias=as.numeric(limite_superior - Sys.Date())
#     ) %>%
#     #generar listado
#     select(house_id=id, fecha_nacimiento=c30_date, mes, fecha_visita, 
#            tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias,
#            id_tamizaje, "Comunidad"=community)%>% filter( mes=="9") %>% arrange(`dias restantes`) #%>% 
    #writexl::write_xlsx(paste0("output/visitas_B3_semana_",lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1),"_al_",lubridate::floor_date(Sys.Date()+14, unit = "weeks", week_start = 7),".xlsx"))



#_-------------------------------------------
#
#VISITAS ATRASADAS, PENDIENTES DE PONER AL DIA
#
#___________________________________________

## -------------------
##LISTA DE P1 Y P2 PENDIENTES DE PONER AL DIA
##-------------------------------------------
# gt_emory_data_arm1 %>% select(
#     "id_tamizaje"=id, "id_estudio"=s4_main_id, s4_date
# ) %>% filter(!is.na(id_estudio)) %>% 
#     #para revisar edad gestacional
#     left_join(
#         gt_emory_data_arm1 %>% select("id_tamizaje"=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
#     ) %>% 
#     #Quitar las salidas, los abortos y los que ya nacieron, y descartar todas las que ya tienen P1
#     anti_join(
#         bind_rows(
#             salidas %>% select("id_estudio"=id),
#             gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select("id_estudio"=id),
#             gt_emory_data_arm2 %>%
#                 filter(e2_title %in% c("2" , "7") | e1_title == "2") %>%
#                 select("id_estudio"=id),
#             gt_emory_data_arm2 %>% filter(!is.na(m11_date) & redcap_event_name=="p2_arm_2") %>% select("id_estudio"=id)
#         )
#         
#     ) %>% 
#      mutate(
#         fga= as.Date(m17_ga) - lubridate::days(280),
#         dias =  lubridate::floor_date(Sys.Date(), unit = "weeks", week_start = 1) - fga ,
#         edad_semanas= as.numeric(dias) %/% 7,
#         edad_dias = as.numeric(dias)%% 7,
#         fecha_P1=fga + lubridate::days(168),
#         fecha_limite_P1=fga+lubridate::days(196),
#         fecha_P2=fga + lubridate::days(224),
#         fecha_limite_P2=fga+lubridate::days(238),
#         flag_visita_p2= lubridate::floor_date(fecha_limite_P2, unit="weeks", week_start = 1) < lubridate::floor_date(Sys.Date(), unit = "weeks", week_start = 1),
#         #Identificar las que les corresponde visita en la semana
#         #flag_visita=if_else(edad_semanas>=26 & edad_semanas<=36,"1","0"),
#         edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
#     ) %>%   left_join(
#         #Agregar la comunidad
#         comunidades %>% transmute("id_estudio"=house_id, "Comunidad"=community)
#     ) %>% #ordenar por edad gestacional
#     
#     arrange(desc(edad_semanas))




#----------------------------------------
# los datos como en control avance
#------------------------------------------------------
# # gt_emory_data_arm1 %>%select("id_tamizaje"=id, "id"=s4_main_id) %>% filter(!is.na(id)) %>% left_join(
# #     gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
# # ) %>% left_join(
# #     gt_emory_data_arm2 %>% select(id, c30a_date) %>% filter(!is.na(c30a_date))
# # ) %>% mutate(c30_date=if_else(!is.na(c30_date),c30_date, c30a_date)) %>% 
# #     # los nacimientos
# #     filter(!is.na(c30_date)) %>%
# #     select(id_tamizaje, id, c30_date) %>%
# #     anti_join(
# #         bind_rows(
# #             gt_emory_data_arm2 %>%
# #                 filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
# #                 select(id),
# #             gt_emory_data_arm2 %>%
# #                 filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
# #                 select(id)
# #         )
# #     )  %>%
# #     # agregar variable del mes
# #     mutate(mes = NA) %>%
# #     # agregar fecha de cada mes de edad
# #     complete(
# #         # usando solo las combinaciones existentes de id y c30_date
# #         nesting(id, id_tamizaje, c30_date),
# #         # agregar todos los meses del estudio
# #         mes = 1:12
# #     ) %>%
# #     # calcular fecha de visitas mensuales
# #     mutate(
# #         fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
# #         visita = case_when(
# #             # visitas b1, b2, b3, b4
# #             mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
# #             # resto de visitas mensuales
# #             TRUE ~ paste0("m", mes)
# #         ),
# #         # ¿se debe de visitar esta semana?
# #         flag_visita = lubridate::floor_date(fecha_visita, unit = "weeks", week_start = 1) < lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) 
# #     ) %>%  filter(flag_visita=="TRUE") %>% 
# #     #eliminar los c31 que ya se hicieron
# #     left_join(
# #         gt_emory_data_arm2 %>% select(id,c31_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(c31_date))
# #     ) %>% mutate(
# #         visit=if_else(
# #             condition = is.na(visit), 
# #             true = "pendiente", 
# #             false = as.character(visit)
# #         )
# #     ) %>% filter(visita!=visit) %>% 
# #     #agregar id de tamizaje y comunidad
# #     left_join(
# #         comunidades 
# #     ) %>% 
# #     #agregar fechas limite para visita
# #     mutate(
# #         limite_inferior= fecha_visita - lubridate::days(3),
# #         limite_superior= fecha_visita + lubridate::days(3),
# #         dias=as.numeric(limite_superior - Sys.Date())
# #     ) %>%
# #     #generar listado
# #     select(house_id=id, fecha_nacimiento=c30_date, mes, fecha_visita, 
# #            tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias,
# #            id_tamizaje, "Comunidad"=codigo) %>%  arrange(`dias restantes`) %>% 
# #     writexl::write_xlsx("output/visitas_c31_pendientes.xlsx")
# 
# 
# 
# 
# # los datos como en control avance
# gt_emory_data_arm2 %>%
#     # los nacimientos
#     filter(!is.na(c30_date)) %>%
#     select(id, c30_date) %>%
#     # quitar las salidas
#     left_join(
#         gt_emory_data_arm2 %>%
#             filter(!is.na(e3_date_c)) %>%
#             select(id, e3_date_c)
#     ) %>% {
#         # si hay salidas, mostrarlas
#         if(any(!is.na(.$e3_date_c))) filter(., !is.na(e3_date_c)) %>% print()
#         # seguir con todos los datos
#         .
#     } %>%
#     filter(is.na(e3_date_c)) %>% 
#     select(-e3_date_c) %>%
#     # agregar variable del mes
#     mutate(mes = NA) %>%
#     # agregar fecha de cada mes de edad
#     complete(
#         # usando solo las combinaciones existentes de id y c30_date
#         nesting(id, c30_date),
#         # agregar todos los meses del estudio
#         mes = 1:12
#     ) %>%
#     # calcular fecha de visitas mensuales
#     mutate(
#         fecha_visita = c30_date + lubridate::days(round(365.25/12 * mes)),
#         visita = case_when(
#             # visitas b1, b2, b3, b4
#             mes %in% c(3, 6, 9, 12) ~ paste0("b", map(mes, ~ which(c(3, 6, 9, 12) == .x) %>% ifelse(length(.), ., 0L))),
#             # resto de visitas mensuales
#             TRUE ~ paste0("m", mes)
#         ),
#         # ¿se debe de visitar esta semana?
#         flag_visita = lubridate::floor_date(fecha_visita, unit = "weeks", week_start = 1) < lubridate::floor_date(Sys.Date()+7, unit = "weeks", week_start = 1) 
#     ) %>%  filter(flag_visita=="TRUE") %>% 
#     #eliminar los c31 que ya se hicieron
#     left_join(
#         gt_emory_data_arm2 %>% select(id,m11_date, visit) %>% mutate(visita=as.character(visit)) %>%  filter(!is.na(m11_date))
#     ) %>% mutate(
#         visit=if_else(
#             condition = is.na(visit), 
#             true = "pendiente", 
#             false = as.character(visit)
#         )
#     ) %>% filter(visita!=visit) %>% 
#     #agregar id de tamizaje y comunidad
#     left_join(
#         comunidades %>% mutate("id"=id_estudio)
#     ) %>% 
#     #agregar fechas limite para visita
#     mutate(
#         limite_inferior= fecha_visita - lubridate::days(3),
#         limite_superior= fecha_visita + lubridate::days(3),
#         dias=as.numeric(limite_superior - Sys.Date())
#     ) %>%
#     #generar listado
#     select(house_id=id, fecha_nacimiento=c30_date, mes, fecha_visita, 
#            tipo_visita=visita, limite_inferior, limite_superior, "dias restantes"=dias,
#            id_tamizaje, "Comunidad_1"=community, "Comunidad_2"=community_new) %>%  arrange(`dias restantes`) %>% 
#     writexl::write_xlsx("output/visitas_m11_pendientes_vencidos.xlsx")



#----------------------------------------------------------------------------------------------
# REPORTE DE PARTOS ESPERADOS PARA REALIZAR LAS LLAMADAS (LO SOLICITA WENDY)
#--------------------------------------------------------------------------------
# salidas<-gt_emory_data_arm2 %>% select(id,e3_date, e3_by) %>% filter(!is.na(e3_date)) %>%
#     left_join(
#         gt_emory_data_arm2 %>% select(id,e3_date_o, e3_by_o) %>% filter(!is.na(e3_date_o))
#     ) %>%left_join(
#         gt_emory_data_arm2 %>% select(id,e3_date_c, e3_by_c) %>% filter(!is.na(e3_date_c))
#     ) %>% left_join(
#         gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
#     ) %>%mutate(
#         type = if_else(
#             condition = grepl("^35", id),
#             true = "oaw",
#             false = "pw"
#         )
#     ) %>% mutate(
#         sale = case_when(
#             type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
#             type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
#             type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#             type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#             TRUE ~ NA_character_
#         )
#     ) %>%filter(sale=="1") %>% select(id,sale)
#---------------------------------------------------------------
#PARTOS ESPERADOS
#--------------------------------------------------------------
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id"=s4_main_id) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id,s6_arm)
) %>% filter(!is.na(s6_arm)) %>% 
    #sacamos las que tienen E3 salida, aborto o muerte fetal
    anti_join(
        bind_rows(
            salidas,
            gt_emory_data_arm2 %>%
                filter(e2_title %in% c("2", "7") | e1_title == "2") %>%
            select(id)
    )
    
) %>% #sacamos las que ya nacio el bb o tienen c30a
    anti_join(
        bind_rows(
            gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id),
            gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id)
            )
    ) %>% 
    left_join(
        gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select("id_tamizaje"=id, m17_ga)
    ) %>% mutate(
        #fpp_calculate=lubridate::floor_date(m17_ga+7, unit = "weeks"),
        ega=as.Date(m17_ga)- lubridate::days(280),
        dias= Sys.Date() - as.Date(ega),
        ga_semanas= as.numeric(dias)%/%7,
        ga_dias=as.numeric(dias)%%7
    ) %>% left_join(
        comunidades %>% transmute("id"=house_id, "Comunidad"=community)
    ) %>% arrange(desc(ga_semanas)) %>% 
    transmute(
        id_tamizaje, id, brazo=if_else(s6_arm=="1", "Intervencion", "Control"),
        "Fecha probable de Parto"=m17_ga,
        "Edad gestacional"= paste0(ga_semanas," con ",ga_dias),
        Comunidad
    ) %>%
    left_join(
        datos_participantes %>% transmute(id=`ID estudio`, 
                                          tel_embarazada=
                                              `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                          tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
    ) %>% 
    #generar listado para que realicen las llamadas
    writexl::write_xlsx(paste0("output/partos_esperados_al_",Sys.Date(),".xlsx"))
 

#--------------------------------------------------------------------------------------   
#CODIGO PARA SACAR CANTIDAD DE HOGARES POR COMUNIDAD
#------------------------------------------------------------------------------------
#lista de inscritas por comunidad
# comunidades %>% filter(!is.na(lat)) %>% filter(lat>=13) %>% anti_join(
#     salidas %>% select("id_estudio"=id)
# ) %>%   group_by(com_jalapa) %>% summarise(contador=n(), lat= mean(lat), long = mean(long) ) %>% write_csv("output/reference/comunidades_inscritas_coords.csv")
# 
# #lista tamizadas por comunidad
# gt_emory_data_arm1 %>% filter(!is.na(s1_date)) %>% select(
#     id, s1_community_name,s1_pregnant,s1_age_s,s1_area_s
# ) %>% group_by(
#     s1_community_name
# ) %>% summarize(
#     contador=n()
# ) %>%  write_csv("output/reference/comunidades_tamizadas_coords.csv")
# 
# 
# gt_emory_data_arm1 %>% select(id, s1_community_name,s1_pregnant,s1_age_s,s1_area_s,s1_area_s,s1_biomass, s1_smoke, s1_move, s1_lpg, s1_physical_biomass, s1_physical_lpg, s2_fetus, s2_gest, s2_onefetus) %>% filter( 
#         s1_pregnant=="1" &
#         s1_age_s=="1" &
#         s1_area_s=="1" &
#         s1_biomass=="1" &
#         s1_smoke=="0" &
#         s1_move=="0" &
#         (s1_lpg=="0" | s1_lpg=="888") &
#         (s1_physical_lpg=="0" | s1_physical_lpg=="888") &
#          s2_fetus=="1" &
#         s2_gest=="1" &
#         s2_onefetus=="1"
#         ) %>%
#     group_by(
#     s1_community_name
# ) %>% summarize(
#     contador=n()
# ) %>% write_csv("output/reference/comunidades_elegibles_coords.csv")
# 
# 




