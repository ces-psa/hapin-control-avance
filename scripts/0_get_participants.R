#------------------------------------------------------------------------------*
# Datos de contacto de participantes ----
#------------------------------------------------------------------------------*

library(package = "tidyverse")

data_base <- DBI::dbConnect(odbc::odbc(), "hapin-gt", dbname = "hapindb")

z10_vars <- c(
  "record", "record_id", "id_estudio",  "municipio", "z10_village", "com_jalapa", 
  "com_jalapa_new", "z10_alti_gps", "z10_lati_gps", "z10_long_gps", "z10_arm",
  "type_goelocation"
)

private_vars <- c(
  "z10_p_name", "z10_p_lastname_2",
  "z10_aow_name", "z10_aow_lastname",
  "z10_hh_name", "z10_hh_lastname_2",
  "z10_dpi", "z10_dob", "fecha_parto", "fecha_nacimiento_bb",
  "z10_cell_participant", "z10_cell_hh", "z10_cell_other"
)

gt_z10 <- DBI::dbGetQuery(
  conn = data_base,
  statement = paste(
    "select *",
    "from redcap_data",
    "where project_id = 39",
    # "in \"(select project_id from redcap_projects where project_name = 'main_study_gt'\") A",
    "and field_name in",
    paste0(
      "('",
      paste(
        c(z10_vars, private_vars),
        collapse = "', '"
      ),
      "')"
    ),
    # get specifically for z10
    "and event_id = 161"
  )
) %>%
  select(record, field_name, value) %>%
  spread(field_name, value) %>%
  select(!!!z10_vars, !!!private_vars) %>%
  as_tibble() %>%
  mutate(
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
    )
  )


# Disconnect from database
DBI::dbDisconnect(conn = data_base)




#------------------------------------------------------------------------------*
# Generate participant lookup table ----
#------------------------------------------------------------------------------*

gt_z10 %>%
  mutate_all(
    list(~ iconv(., from = "Latin1", to = "UTF-8"))
  ) %>%
  mutate_at(
    vars(
      z10_p_name, z10_p_lastname_2,
      z10_aow_name, z10_aow_lastname,
      z10_hh_name, z10_hh_lastname_2
    ),
    list(
      ~ if_else(
        condition = is.na(.),
        true = "",
        false = .
      )
    )
  ) %>%
  mutate(
    embarazada = paste(z10_p_name, z10_p_lastname_2),
    adulta = paste(z10_aow_name, z10_aow_lastname),
    esposo = paste(z10_hh_name, z10_hh_lastname_2)
  ) %>%
  select(
    "ID tamizaje" = record_id,
    "ID estudio" = id_estudio,
    "Nombre embarazada" = embarazada,
    "Comunidad embarazada" = community,
    "Celular embarazada" = z10_cell_participant,
    "Celular esposo" = z10_cell_hh,
    "DPI embarazada" = z10_dpi,
    "Fecha nacimiento embarazada" = z10_dob,
    "Nombre esposo" = esposo,
    "Fecha esperada de parto" = fecha_parto,
    "Fecha del parto" = fecha_nacimiento_bb,
    "Nombre otra adulta" = adulta
  ) %>%
  DT::datatable() %>%
  htmlwidgets::saveWidget(file = "participants.html")

file.rename(
  from = "participants.html",
  to = "output/reference/participants.html"
)


# Drop identifiable information
gt_z10 <- gt_z10 %>%
  select(!!! setdiff(z10_vars, private_vars))




#------------------------------------------------------------------------------*
# Clean up z10 data ----
#------------------------------------------------------------------------------*

# Hard limits on geo-data
gt_participants <- gt_z10 %>%
  # GPS data as numbers
  mutate_at(
    vars(matches("alti|lati|long")),
    list(
      ~ gsub("[^-0-9.]", "", .) %>%
        as.numeric()
    )
  ) %>%
  # Select GPS data given source type
  mutate(
    long = case_when(
      # type_goelocation == "1" ~ z10_long_tablet,
      type_goelocation == "2" ~ z10_long_gps,
      TRUE ~ NA_real_
    ),
    lat = case_when(
      # type_goelocation == "1" ~ z10_lati_tablet,
      type_goelocation == "2" ~ z10_lati_gps,
      TRUE ~ NA_real_
    ),
    elevation = case_when(
      # type_goelocation == "1" ~ z10_alti_tablet,
      type_goelocation == "2" ~ z10_alti_gps,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-matches("(long|lati|alti)_(gps|tablet)")) %>%
  # Common fixes for GPS data
  mutate(
    # Longitude should be roughly in -90.266 to -89.61
    # and latitude roughly in 14.42 to 14.87
    # If these appear to be reversed, switch
    long_lat_switched = (
      # latitude is negative and < -80
      lat < -80 |
        # longitude is positive and < 80
        between(long, 0, 80)
    ),
    gps_d_temp = if_else(long_lat_switched, long, lat),
    long = if_else(long_lat_switched, lat, long),
    lat = if_else(long_lat_switched, gps_d_temp, lat),
    # Longitude in Jalapa has to be negative
    long = case_when(long > 0 ~ long * -1, TRUE ~ long),
    # Latitude in Jalapa has to be positive,
    lat = case_when(lat < 0 ~ lat * -1, TRUE ~ lat),
    # more drastic fixes
    long = case_when(
      abs(long) > 100 ~ long %>%
        sub("(-?[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ long
    ),
    lat = case_when(
      abs(lat) > 100 ~ lat %>%
        sub("(-?[0-9]{2})(.+)", "\\1.\\2", .) %>%
        as.double(),
      TRUE ~ lat
    ),
    # Elevation can not be below 100 meters
    elevation = case_when(
      elevation < 100 ~ elevation * 100,
      TRUE ~ elevation
    )
  )
