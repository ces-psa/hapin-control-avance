# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")
# Participant information (z10)
source(file = "scripts/0_get_e3.R", encoding = "UTF-8")



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

rutas<-read_csv("data/cat_rutas_visitas.csv", show_col_types = FALSE)

# Programacion hapin 1-5 clinica
source(file = "prog_semanal_clinica_hapin_1_5.R", encoding = "UTF-8")
# Programacion Intensivo
source(file = "programacion_intensivos_54m.R", encoding = "UTF-8")
  # Programacion hapin 1-5 exposición
source(file = "prog_semanal_exposicion_hapin_1_5.R", encoding = "UTF-8")




#nombre y sexo de los niños
gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% transmute(id, dob=c30_dob, sexo=
                                                               recode(c30_sex,"1"="Masculino","2"="Femenino") ) %>% left_join(
                                                                 datos_participantes %>% select(
                                                                   `ID tamizaje`, id=`ID estudio`, Nombre_bb
                                                                 ) %>% distinct()
                                                               ) %>% writexl::write_xlsx("output/listado_bebes_hapin.xlsx")


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                #-----------------

#--------------------------------------------------
#
# LOGICA PARA SACAR LAS VISITAS PRIORIZADAS DE C31
#--------------------------------------------------
arm2<-gt_emory_data_arm2
upcoming_c31_visits <- arm2 %>%
  # comenzar solo con los que ya nacieron
  filter(!is.na(c30_date)) %>%
  select(id, dob = c30_dob) %>%
  # quitar todos los que ya salieron
  anti_join(
    arm2 %>%
      filter(
        # las que ya no tienen niño
        !is.na(e3_date_c)
      )
  ) %>%
  # agregar el c31 más reciente para contexto
  left_join(
    arm2 %>%
      filter(!is.na(c31_date)) %>%
      select(id, last_visit = visit, last_c31 = c31_date) %>%
      group_by(id) %>%
      slice(which.max(last_c31)) %>%
      ungroup()
  ) %>%
  # y quitar las que hicieron c31 en b4, aunque aun no tengan salida
  filter(last_visit != "b4") %>%
  mutate(
    # edad actual
    current_age_days = as.numeric(
      Sys.Date() - as.Date(dob), unit = "days"
    ),
    current_age_months = current_age_days / (365.25 / 12),
    
    # dias desde la visita anterior esperada (la última vez que cumplió meses)
    days_since_last_expected = current_age_days - floor( floor(current_age_months) * (365.25 / 12) ),
    
    # dias hasta la siguiente visita (la próxima vez que cumple meses)
    days_to_next_expected = floor( ceiling(current_age_months) * (365.25 / 12) ) - current_age_days,
    
    # con base en eso tomar la siguiente visita según la regla de -7 a +22
    # la variable quiere decir a cuál de las edades corresponderia el c31
    # si sale NA, es porque aun no le tocaría, pero ya casi, entonces mejor poner como toca la siguiente (pasa segun el numero de dias del mes actual)
    next_c31_month = case_when(
      
      # si no han pasado más de 22 días, entonces la visita es del mes anterior
      days_since_last_expected <= 22 ~ floor(current_age_months),
      
      # aunque hayan pasado más de 22 días, pero si la edad ya son 12 meses, igual debe hacerla (porque es una b4 atrasada)
      current_age_months >= 12 ~ floor(current_age_months),
      
      days_to_next_expected <= 7 ~ ceiling(current_age_months),
      TRUE ~ ceiling(current_age_months)
    ),
    
    # segun la edad del siguiente esperado, calcular la fecha en que le habría tocado
    expected_c31_date = as.Date(dob) + floor( next_c31_month * (365.25 / 12) ),
    
    # fecha en que le toca ahora. Si está pasada, fecha de mañana
    next_c31_date = if_else(
      condition = expected_c31_date < Sys.Date(),
      true = Sys.Date() + 1,
      false = expected_c31_date
    ),
    
    # y ver en qué semana se les dice (pensando que esa semana es el lunes, pero esto se les da el jueves)
    next_c31_week = lubridate::floor_date(next_c31_date, unit = "weeks"),
    
    delay = floor(as.numeric(Sys.Date() - expected_c31_date, unit = "days")),
    delay_groups = cut_width(delay, width = 7, boundary = 0)
  ) %>%
  # ordenarlas según cuándo les toca
  arrange(next_c31_date) %>%
  print()

upcoming_c31_visits %>% 
  # todas las que se deben visitar esta semana o antes son las atrasadas, y deben sacarlas ahora
  filter(next_c31_week <= as.Date("2021-07-07")) %>% write_csv("output/listado_visitas_c31_semana_2021-07-07.csv")

lista_c31<-upcoming_c31_visits %>%
  # todas las que se deben visitar esta semana o antes son las atrasadas, y deben sacarlas ahora
  filter(next_c31_week <= as.Date("2021-07-07"))

upcoming_c31_visits %>%
  count(next_c31_week, delay_groups) %>%
  spread(next_c31_week, n)

listado_c31<-lista_c31 %>% select(id,fecha_nacimiento=dob, ultima_visita_realizada=last_visit, fecha_ultimo_c31=last_c31, 
                     edad_actual_dias=current_age_days, edad_actual_meses=current_age_months, 
                     c31_que_corresponde=next_c31_month, fecha_proximo_c31=next_c31_date, delay_groups, days_to_next_expected) %>% mutate(
                     prioridad=case_when(
                       delay_groups=="(21,28]" ~ "1",
                       delay_groups=="(14,21]" ~ "2",
                       delay_groups=="(7,14]" ~ "3",
                       delay_groups=="(0,7]" ~ "4",
                       delay_groups=="(-7,0]" ~ "5"
                     ),
                       dias_desde_ultimo_c31= Sys.Date() - fecha_ultimo_c31,
                     meses_desde_ultimo_c31= dias_desde_ultimo_c31 / 30.25,
                     fecha_real_proximo_c31= Sys.Date() + lubridate::days(days_to_next_expected)
                     )  %>% mutate(
                       visita_corresponde=case_when(
                         c31_que_corresponde=="3" | c31_que_corresponde=="6" | c31_que_corresponde=="9" | c31_que_corresponde=="12" ~ paste0("b", as.integer(c31_que_corresponde)/3),
                         c31_que_corresponde=="1" | c31_que_corresponde=="2" | c31_que_corresponde=="4" | c31_que_corresponde=="5" | c31_que_corresponde=="7" | c31_que_corresponde=="8" | c31_que_corresponde=="10" | c31_que_corresponde=="11" ~ paste0("m", as.integer(c31_que_corresponde)),
                         c31_que_corresponde>="12" ~ "b4" 
                         #TRUE ~ paste0("m",c31_que_corresponde)
                       )
                     ) %>% 
  arrange(prioridad) %>% 
  #Agregar comunidades y rutas
  left_join(
    comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
  )%>%  left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>%
  select( id, id_tamizaje, fecha_nacimiento, ultima_visita_realizada, fecha_ultimo_c31, c31_que_corresponde,
                                  edad_actual_dias, edad_actual_meses, en_dias_desde_ultimo_c31=dias_desde_ultimo_c31, 
                                  en_meses_desde_ultimo_c31=meses_desde_ultimo_c31,  fecha_real_proximo_c31, 
                                  visita_donde_debe_registrarse=visita_corresponde, prioridad, comunidad_1, comunidad_2, ruta
    
   ) %>%  filter(ultima_visita_realizada!=visita_donde_debe_registrarse) %>% #select(id, fecha_nacimiento, ultima_visita_realizada, fecha_ultimo_c31,
  #                                                                                  edad_actual_meses,en_dias_desde_ultimo_c31,c31_que_corresponde,
  #                                                                                  visita_donde_debe_registrarse,fecha_real_proximo_c31) %>% group_by(visita_donde_debe_registrarse, c31_que_corresponde) %>% count()
#%>% 
  mutate(
    fecha_ideal_cumple_mes=case_when(
      visita_donde_debe_registrarse=="m1" ~ as.Date(fecha_nacimiento) + lubridate::days(30),
      visita_donde_debe_registrarse=="m2" ~ as.Date(fecha_nacimiento) + lubridate::days(60),
      visita_donde_debe_registrarse=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(90),
      visita_donde_debe_registrarse=="m4" ~ as.Date(fecha_nacimiento) + lubridate::days(121),
      visita_donde_debe_registrarse=="m5" ~ as.Date(fecha_nacimiento) + lubridate::days(151),
      visita_donde_debe_registrarse=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(181),
      visita_donde_debe_registrarse=="m7" ~ as.Date(fecha_nacimiento) + lubridate::days(211),
      visita_donde_debe_registrarse=="m8" ~ as.Date(fecha_nacimiento) + lubridate::days(242),
      visita_donde_debe_registrarse=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(272),
      visita_donde_debe_registrarse=="m10" ~ as.Date(fecha_nacimiento) + lubridate::days(302),
      visita_donde_debe_registrarse=="m11" ~ as.Date(fecha_nacimiento) + lubridate::days(332),
      visita_donde_debe_registrarse=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(363)
    )
  ) %>% mutate(
    inicio_ventana= as.Date(fecha_ideal_cumple_mes) - lubridate::days(7),
    fin_ventana=as.Date(fecha_ideal_cumple_mes) + lubridate::days(7),
    ultima_fecha_vencimiento= as.Date(fecha_ideal_cumple_mes) + lubridate::days(22),
    dias_desde_ultimo_cumple_mes= Sys.Date() - as.Date(fecha_ideal_cumple_mes)
    #limitar listado a los que estan en su ultima fecha de vencimiento menor al proximo jueves
  ) %>% filter(visita_donde_debe_registrarse=="b4" | (visita_donde_debe_registrarse!="b4" & ultima_fecha_vencimiento>="2021-07-07")) %>% #filter(dias_desde_cumple_mes<="22") %>% 
  select(id, id_tamizaje, fecha_nacimiento, ultima_visita_realizada, fecha_ultimo_c31, 
         edad_actual_meses,   fecha_ideal_cumple_mes, inicio_ventana,fin_ventana,ultima_fecha_vencimiento,dias_desde_ultimo_cumple_mes,
         visita_donde_debe_registrarse,  comunidad_1, comunidad_2, ruta) %>% 
  arrange(desc(dias_desde_ultimo_cumple_mes)) #%>% write_csv(paste0("output/c31_priorizados_revision_al_02_07_2020.csv"))



#-----------------------------------------------------------------
#
#VISITAS B1 A B4 CLINICA
#-----------------------------------------------------------------
fecha2="2021-07-07"

#sacar todas las participantes aleatorizadas
all_b1_b4_clinica<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=s6_arm) %>% 
  #sacamos fechas de nacimiento para participantes madres
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% 
  #sacamos fechas probable de parto como fecha de nacimiento para participantes adultas
  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
) %>% select(-c30_dob, -m17_ga) %>% 
  #quitar las que ya salieron
  anti_join(salidas %>% select(id)) %>% mutate(
  type=if_else(grepl("^35", id), "owa", "pwg")
) %>% filter(id!="33248") %>% 
  #identificar los hogares que tienen participando owa y pwg
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_madre=e3_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_owa=e3_date_o)
) %>% 
  mutate(
  participan=case_when(
    type=="pwg" ~ "madre",
    type=="owa" & is.na(e3_madre) & is.na(e3_owa) ~ "madre y owa",
    type=="owa" & !is.na(e3_madre) ~ "owa",
    type=="owa" & !is.na(e3_owa) ~ "madre"
    
  )
)  %>% select(-e3_madre, -e3_owa) %>%
  #completar matriz de visitas
  mutate(
    visita="b1"
  ) %>% complete(
    nesting(id, fecha_nacimiento,brazo, id_tamizaje, type, participan),
    visita = c( "b1", "b2", "b3", "b4")
  ) %>% mutate(
    start_window=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(76),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(167),
      visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(258),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(349)
    ),
    end_window=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(104),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(195),
      visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(286),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(377)
    ),
    last_day_end_visit=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(153),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(244),
      visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(335),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(450)
    )
  ) %>% 
  #identificar la ultima visita realizada a pwg
  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, visita=visit, fecha_visita_madre=c33_date) 
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(a23_date)) %>% select(id, visita=visit, fecha_visita_owa=a23_date) %>% filter(visita=="b1" | visita=="b2" | visita=="b4")
) %>% mutate(
  date_visit=case_when(
    participan=="madre" ~ fecha_visita_madre,
    participan=="madre y owa" & !is.na(fecha_visita_madre) ~ fecha_visita_madre,
    participan=="madre y owa" & !is.na(fecha_visita_owa) ~ fecha_visita_owa,
    participan=="owa" ~ fecha_visita_owa
  )
) %>% mutate(
  entered_window = start_window < fecha2 
  ) %>% arrange(id, visita) %>% group_by(id) %>%  
  #filter(entered_window==TRUE) #%>% 
  mutate(
    esperada = is.na(date_visit) &
      entered_window &
      case_when(
        visita == "b1" ~ is.na(date_visit[visita=="b2"]) &
          is.na(date_visit[visita=="b3"]) &
          is.na(date_visit[visita=="b4"]),
        visita == "b2" ~ is.na(date_visit[visita=="b3"]) &
          is.na(date_visit[visita=="b4"]),
        visita == "b3" ~ is.na(date_visit[visita=="b4"]),
        visita == "b4" ~ is.na(date_visit[visita=="b4"])
      )
  ) %>% ungroup() %>% filter(esperada==TRUE) %>% 
  # filter(visita =="b4" | (visita =="b1" & last_day_end_visit<=fecha2) | (visita =="b2" & last_day_end_visit<=fecha2) |
  #          (visita =="b3" & last_day_end_visit<=fecha2)) %>% 
  mutate(
    current_age_days = as.numeric(
      Sys.Date() - as.Date(fecha_nacimiento), unit = "days"
    ),
    current_age_months = current_age_days / (365.25 / 12)
  ) %>% 
  #seleccionamos columnas para el listado
  transmute(id_tamizaje, id, fecha_nacimiento, edad_actual_meses=current_age_months, participan, visita, 
            inicio_ventana=start_window, fin_ventana=end_window, fin_ventana_vencida=last_day_end_visit,
            brazo)%>%  
  left_join(
    #agregamos comunidades
    comunidades %>% select(id=house_id, comunidad_1= community, comunidad_2=community_new)
  ) %>% left_join(
    #agregamos rutas
    rutas %>% select(comunidad_1=comunidad, ruta)
  )%>% 
  left_join(
    #agregamos contactos y datos de la participante
    datos_participantes %>% transmute(id=`ID estudio`, 
                                      tel_embarazada=
                                        `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                      tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
  )

#todas las vigentes
all_b1_b4_clinica_vigentes<- all_b1_b4_clinica %>% filter(fin_ventana>=fecha2) %>% 
  select(-fin_ventana_vencida) %>% mutate(brazo=recode(brazo,"1"="Intervencion","0"="Control")) %>% arrange(fin_ventana) %>% 
  print()

#todas las vencidas
all_b1_b4_clinica_vencidas<- all_b1_b4_clinica %>% filter(fin_ventana<=fecha2) %>% 
  mutate(inicio_ventana_vencida=as.Date(inicio_ventana) + lubridate::days(1)) %>% select(-inicio_ventana, -fin_ventana) %>% 
  mutate(brazo=recode(brazo,"1"="Intervencion","0"="Control")) %>%  select(1:6,16,7:15) %>% filter(visita=="b4" | 
                                                                                                     (visita=="b1" & fin_ventana_vencida>fecha2) |
                                                                                                     (visita=="b2" & fin_ventana_vencida>fecha2) |
                                                                                                     (visita=="b3" & fin_ventana_vencida>fecha2)
                                                                                                      ) %>% arrange(fin_ventana_vencida) %>%   print()
#Marcar los c31 que deben hacerce en B vigentes
all_b1_b4_clinica_vigentes<-all_b1_b4_clinica_vigentes %>% left_join(
  listado_c31 %>% select(id, visita=visita_donde_debe_registrarse) %>% mutate(
    Nota="Hacer C31"
  )
) 


all_b1_b4_clinica_vigentes<-all_b1_b4_clinica_vigentes %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% transmute(id, visita=visit, c31_realizado="Si")
) %>% mutate(
  Nota=case_when(
    Nota=="Hacer C31" ~ "Hacer C31",
    is.na(Nota) & is.na(c31_realizado) ~ "Hacer C31",
    TRUE ~ NA_character_
  )
) %>% select(-c31_realizado)

#Marcar los c31 que deben hacerce en B vencidas
all_b1_b4_clinica_vencidas<-all_b1_b4_clinica_vencidas %>% left_join(
  listado_c31 %>% select(id, visita=visita_donde_debe_registrarse) %>% mutate(
    Nota="Hacer C31"
  )
) 


all_b1_b4_clinica_vencidas<-all_b1_b4_clinica_vencidas%>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% transmute(id, visita=visit, c31_realizado="Si")
) %>% mutate(
  Nota=case_when(
    Nota=="Hacer C31" ~ "Hacer C31",
    is.na(Nota) & is.na(c31_realizado) ~ "Hacer C31",
    TRUE ~ NA_character_
  )
) %>% select(-c31_realizado)


#identificar los c31 que se harán en B para no duplicar trabajo
listado_c31<-listado_c31 %>% left_join(
    all_b1_b4_clinica_vigentes %>% select(id, visita_donde_debe_registrarse=visita ) %>% bind_rows(
      all_b1_b4_clinica_vencidas %>% select(id, visita_donde_debe_registrarse=visita)
    ) %>% mutate(
      Nota="Se hara en B"
    )
) %>% left_join(
  
  gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% filter(visit=="b3") %>% select(id, c33_date, visit) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(id, h55_date, visit) %>% filter(visit=="b3")
  ) %>% filter(is.na(h55_date)) %>% anti_join(
    salidas %>% select(id)
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% filter(visit=="b4") %>%  select(id, h55_b4=h55_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% mutate(
    edad_dias=(Sys.Date() - as.Date(c30_dob)),
    edad_meses= edad_dias / 30.25
  ) %>% select(id, fecha_b3=c33_date, fec_nacimiento=c30_dob, edad_actual_meses=edad_meses) %>% left_join(
    comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
  )%>%  left_join(
    rutas %>% select(comunidad_1=comunidad, ruta)
  ) %>% arrange(desc(edad_actual_meses)) %>% transmute(id, Nota2="Hacer H55 en B3")
)


list(
  "b1_b4_vigentes" = all_b1_b4_clinica_vigentes,
  "b1_b4_vencidas" = all_b1_b4_clinica_vencidas,
  "c31" = listado_c31
) %>% writexl::write_xlsx(paste0("output/visitas/programacion_clinica_al_",fecha2,".xlsx"))




##_--------------------------------------------------------------
# EXPOSICION
#----------------------------------------------------------------
#-----------------------------------------------------------------

fecha2="2021-07-07"

#sacar todas las participantes aleatorizadas
all_b1_b4_exposicion<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, brazo=s6_arm) %>% 
  #sacamos fechas de nacimiento para participantes madres
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% 
  #sacamos fechas probable de parto como fecha de nacimiento para participantes adultas
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% mutate(
    fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
  ) %>% select(-c30_dob, -m17_ga) %>% 
  #quitar las que ya salieron
  anti_join(salidas %>% select(id)) %>% mutate(
    type=if_else(grepl("^35", id), "owa", "pwg")
  ) %>% 
  #identificar los hogares que tienen participando owa y pwg
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_madre=e3_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_owa=e3_date_o)
  ) %>% 
  mutate(
    participan=case_when(
      type=="pwg" ~ "madre",
      type=="owa" & is.na(e3_madre) & is.na(e3_owa) ~ "madre y owa",
      type=="owa" & !is.na(e3_madre) ~ "owa",
      type=="owa" & !is.na(e3_owa) ~ "madre"
      
    )
  )  %>% select(-e3_madre, -e3_owa) %>%
  #completar matriz de visitas
  mutate(
    visita="b1"
  ) %>% complete(
    nesting(id, fecha_nacimiento,brazo, id_tamizaje, type, participan),
    visita = c( "b1", "b2", "b4")
  ) %>% mutate(
    start_window=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*3)-28),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*6)-28),
      #visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(258),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)-28)
    ),
    end_window=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*3)+8),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*6)+28),
      #visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(286),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)+28)
    ),
    last_day_end_visit=case_when(
      visita=="b1" ~ as.Date(fecha_nacimiento) + lubridate::days(151),
      visita=="b2" ~ as.Date(fecha_nacimiento) + lubridate::days(333),
      #visita=="b3" ~ as.Date(fecha_nacimiento) + lubridate::days(335),
      visita=="b4" ~ as.Date(fecha_nacimiento) + lubridate::days(480)
    )
  ) %>% 
  #identificar la ultima visita realizada a pwg
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, visita=visit, fecha_visita_madre=h41_date) 
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% select(id, visita=visit, fecha_visita_owa=a24b_date) %>% filter(visita=="b1" | visita=="b2" | visita=="b4")
  ) %>% mutate(
    date_visit=case_when(
      participan=="madre" ~ fecha_visita_madre,
      participan=="madre y owa" & !is.na(fecha_visita_madre) ~ fecha_visita_madre,
      participan=="madre y owa" & !is.na(fecha_visita_owa) ~ fecha_visita_owa,
      participan=="owa" ~ fecha_visita_owa
    )
  ) %>% mutate(
    entered_window = start_window < fecha2 
  ) %>% arrange(id, visita) %>% group_by(id) %>%  
  #filter(entered_window==TRUE) #%>% 
  mutate(
    esperada = is.na(date_visit) &
      entered_window &
      case_when(
        visita == "b1" ~ is.na(date_visit[visita=="b2"]) &
          #is.na(date_visit[visita=="b3"]) &
          is.na(date_visit[visita=="b4"]),
        visita == "b2" ~ #is.na(date_visit[visita=="b3"]) &
          is.na(date_visit[visita=="b4"]),
       # visita == "b3" ~ is.na(date_visit[visita=="b4"]),
        visita == "b4" ~ is.na(date_visit[visita=="b4"])
      )
  ) %>% ungroup() %>% filter(esperada==TRUE) %>% 
  # filter(visita =="b4" | (visita =="b1" & last_day_end_visit<=fecha2) | (visita =="b2" & last_day_end_visit<=fecha2) |
  #          (visita =="b3" & last_day_end_visit<=fecha2)) %>% 
  mutate(
    current_age_days = as.numeric(
      Sys.Date() - as.Date(fecha_nacimiento), unit = "days"
    ),
    current_age_months = current_age_days / (365.25 / 12)
  ) %>% 
  #seleccionamos columnas para el listado
  transmute(id_tamizaje, id, fecha_nacimiento, edad_actual_meses=current_age_months, participan, visita, 
            inicio_ventana=start_window, fin_ventana=end_window, fin_ventana_vencida=last_day_end_visit,
            brazo)%>%  
  left_join(
    #agregamos comunidades
    comunidades %>% select(id=house_id, comunidad_1= community, comunidad_2=community_new)
  ) %>% left_join(
    #agregamos rutas
    rutas %>% select(comunidad_1=comunidad, ruta)
  )%>% 
  left_join(
    #agregamos contactos y datos de la participante
    datos_participantes %>% transmute(id=`ID estudio`, 
                                      tel_embarazada=
                                        `Celular embarazada`, tel_esposo=`Celular esposo`, 
                                      tel_otro_miembro=Celular_otro_miembro, `Nombre embarazada`)
  )

#todas las vigentes
all_b1_b4_expo_vigentes<- all_b1_b4_exposicion %>% filter(fin_ventana>=fecha2) %>% 
  select(-fin_ventana_vencida) %>% mutate(brazo=recode(brazo,"1"="Intervencion","0"="Control")) %>% arrange(fin_ventana) %>% 
  print()

#todas las vencidas
all_b1_b4_expo_vencidas<- all_b1_b4_exposicion %>% filter(fin_ventana<=fecha2) %>% 
  mutate(inicio_ventana_vencida=as.Date(inicio_ventana) + lubridate::days(1)) %>% select(-inicio_ventana, -fin_ventana) %>% 
  mutate(brazo=recode(brazo,"1"="Intervencion","0"="Control")) %>%  select(1:6,16,7:15) %>% filter(visita=="b4" | 
                                                                                                     (visita=="b1" & fin_ventana_vencida>fecha2) |
                                                                                                     (visita=="b2" & fin_ventana_vencida>fecha2) 
                                                                                                    # (visita=="b3" & fin_ventana_vencida>fecha2)
                                                                                                   ) %>% arrange(fin_ventana_vencida) %>%   print()
#leer datos de intensivo
data_intensivo<- read_csv("data/exports/HAPINGuatemalaExposu_DATA_2020-10-28_1538.csv")
intensivo<-data_intensivo %>% mutate_all(as.character) %>%  select(id=record_id, redcap_event_name, h41_date) %>%  mutate(
  visit= substr(redcap_event_name,1,4)
) 

#set de datos participantes en intensivo
dt_intensivo<-intensivo %>% filter(visit=="blp1") %>% transmute(id, Participa_Intensivo="Si")

dt_entericas<-gt_emory_data_arm2 %>% filter(s8_consent=="1") %>% transmute(id, Participa_Entericas="Si")

#identificasr participantes de intensivo y de Entericas
b1_b4_exposicion_vigentes<- all_b1_b4_expo_vigentes %>% left_join(
    dt_intensivo
) %>% left_join(
  dt_entericas
)

b1_b4_exposicion_vencidas <- all_b1_b4_expo_vencidas %>% left_join(
  dt_intensivo
) %>% left_join(
  dt_entericas
)

list(
  "b1_b4_vigentes" = b1_b4_exposicion_vigentes,
  "b1_b4_vencidas" = b1_b4_exposicion_vencidas
 # "c31" = listado_c31
) %>% writexl::write_xlsx(paste0("output/visitas/exposicion/programacion_visitas_expo_al_",fecha2,".xlsx"))



#Marcar los c31 que deben hacerce en B vigentes
# all_b1_b4_clinica_vigentes<-all_b1_b4_clinica_vigentes %>% left_join(
#   listado_c31 %>% select(id, visita=visita_donde_debe_registrarse) %>% mutate(
#     Nota="Hacer C31"
#   )
# ) 

# 
# all_b1_b4_expo_vigentes<-all_b1_b4_expo_vigentes %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% transmute(id, visita=visit, c31_realizado="Si")
# ) %>% mutate(
#   Nota=case_when(
#     Nota=="Hacer C31" ~ "Hacer C31",
#     is.na(Nota) & is.na(c31_realizado) ~ "Hacer C31",
#     TRUE ~ NA_character_
#   )
# ) %>% select(-c31_realizado)

#Marcar los c31 que deben hacerce en B vencidas
# all_b1_b4_clinica_vencidas<-all_b1_b4_clinica_vencidas %>% left_join(
#   listado_c31 %>% select(id, visita=visita_donde_debe_registrarse) %>% mutate(
#     Nota="Hacer C31"
#   )
# ) 
# 
# 
# all_b1_b4_clinica_vencidas<-all_b1_b4_clinica_vencidas%>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% transmute(id, visita=visit, c31_realizado="Si")
# ) %>% mutate(
#   Nota=case_when(
#     Nota=="Hacer C31" ~ "Hacer C31",
#     is.na(Nota) & is.na(c31_realizado) ~ "Hacer C31",
#     TRUE ~ NA_character_
#   )
# ) %>% select(-c31_realizado)


#identificar los c31 que se harán en B para no duplicar trabajo
# listado_c31<-listado_c31 %>% left_join(
#   all_b1_b4_clinica_vigentes %>% select(id, visita_donde_debe_registrarse=visita ) %>% bind_rows(
#     all_b1_b4_clinica_vencidas %>% select(id, visita_donde_debe_registrarse=visita)
#   ) %>% mutate(
#     Nota="Se hara en B"
#   )
# ) %>% left_join(
#   
#   gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% filter(visit=="b3") %>% select(id, c33_date, visit) %>% left_join(
#     gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(id, h55_date, visit) %>% filter(visit=="b3")
#   ) %>% filter(is.na(h55_date)) %>% anti_join(
#     salidas %>% select(id)
#   ) %>% anti_join(
#     gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% filter(visit=="b4") %>%  select(id, h55_b4=h55_date)
#   ) %>% left_join(
#     gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
#   ) %>% mutate(
#     edad_dias=(Sys.Date() - as.Date(c30_dob)),
#     edad_meses= edad_dias / 30.25
#   ) %>% select(id, fecha_b3=c33_date, fec_nacimiento=c30_dob, edad_actual_meses=edad_meses) %>% left_join(
#     comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
#   )%>%  left_join(
#     rutas %>% select(comunidad_1=comunidad, ruta)
#   ) %>% arrange(desc(edad_actual_meses)) %>% transmute(id, Nota2="Hacer H55 en B3")
# )


