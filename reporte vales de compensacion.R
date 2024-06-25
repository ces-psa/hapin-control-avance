library("tidyverse")
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# control de vales
source(file = "scripts/0_get_control_vales_data.R", encoding = "UTF-8")

#catalogo de productos
cat_productos<-read_csv("data/dictionaries/cat_productos_canje.csv")

#library("tidyverse")

#SACAR LISTA DE COMUNIDADES CON BASE EN CATALOGO DE COMUNIDADES JALAPA
#_____________________________________________________________________

#--------------
#---COMUNIDADES
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



#--------------------------------------------------------
#vales entregados
vales_entregados<-entrega_vales1 %>% mutate(entrega="1") %>% select("id"=record_id, id_vale, entrega) %>% 
  bind_rows(
  entrega_vales2 %>% mutate(entrega="2") %>% select("id"=record_id, id_vale, entrega),
  entrega_vales3 %>% mutate(entrega="3") %>% select("id"=record_id, id_vale, entrega),
  entrega_vales4 %>% mutate(entrega="4")%>% select("id"=record_id, id_vale, entrega)
)

vales_entregados %>% group_by(entrega) %>% summarize(
  n=n()
)
vales_entregados %>% arrange(id,entrega) %>% mutate(entrega1=if_else(entrega=="1","Si","Pendiente"),
                                                    entrega2=if_else(entrega=="2","Si", "Pendiente"),
                                                    entrega3=if_else(entrega=="3","Si", "Pendiente"),
                                                    entrega4=if_else(entrega=="4","Si", "Pendiente"))

vales_entregados %>% filter(entrega=="4")
#vales canjeados
vales_canjeados<-canje_vales1 %>% select("id"=record_id, id_vale1, id_vale2, id_vale3, id_vale4) %>% gather(
  key=variable, value = value, -id
) %>% arrange(id) %>% filter(!is.na(value)) %>% mutate(canje=1) %>% transmute(id, "vale"=substr(variable,4,8), id_vale=value) %>% 
  bind_rows(
    canje_vales2 %>% select("id"=record_id, id_vale1, id_vale_saldo_1) %>% gather(
      key=variable, value = value, -id
    ) %>% arrange(id) %>% filter(!is.na(value)) %>% mutate(canje=1) %>% filter(nchar(variable)<9) %>%  transmute(id, "vale"=if_else(nchar(variable)>8, substr(variable,9,15),substr(variable,4,8)), id_vale=value)
    
  )
vales_entregados %>%  group_by(id) %>% summarize(contador=n(), monto=contador*500) %>% write_csv("output/vales_compensacion_entregados.csv")
entrega_vales_saldo1 %>% filter(record!="99999") %>% transmute("id"=record_id, id_vale=id_vale_saldo, monto_vale=as.numeric(monto_vale_saldo), 
                                                           tipo_vale="saldo") %>% group_by(id) %>% 
  summarize(contador=n(), monto=sum(monto_vale)) %>% writexl::write_xlsx(paste0("output/vales_saldo_entregados_al_", Sys.Date(),".xlsx"))


#vales de saldo entregados de compensacion y de saldo
total_vales_entregados<-vales_entregados %>% mutate(tipo_vale="compensacion", monto_vale=500) %>%  bind_rows(
      entrega_vales_saldo1 %>% filter(record_id!="99999") %>% transmute("id"=record_id, id_vale=id_vale_saldo, monto_vale=as.numeric(monto_vale_saldo), tipo_vale="saldo")
) #%>% writexl::write_xlsx(paste0("output/total_vales_entregados_al_", Sys.Date(),".xlsx"))

vales_canjeados %>% filter(id_vale=="VALE-0016" | id_vale=="VALE-0075")

total_vales_canjeados<-vales_canjeados %>% mutate(tipo_vale="compensacion", monto_vale=500) %>% group_by(id) %>% summarize(
  contador=n(),
  monto=sum(monto_vale),
  tipo_vale="compensacion"
) #%>% write_csv("output/vales_canjeados_compensacion.csv")

#lista en excel de entregas y canjes
list(
  "entregados"=total_vales_entregados, 
  "canjeados"=total_vales_canjeados
) %>% writexl::write_xlsx(paste0("output/reporte_vales_compensacion_al_",Sys.Date(),".xlsx"))

#productos entregados

#------------------------------
#REPORTE PARA ENTREGA DE PRODUCTOS EN JALAPA SOLICITÓ GABY

#------------------------------
#lista de hogares control con comunidad
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id"=s4_main_id) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date))
)


#hogares_con_vales<-
  entrega_vales1 %>% mutate(entrega="1") %>% select("id"=record_id, fecha_entrega, id_vale, entrega) %>% 
  bind_rows(
    entrega_vales2 %>% mutate(entrega="2") %>% select("id"=record_id, fecha_entrega, id_vale, entrega),
    entrega_vales3 %>% mutate(entrega="3") %>% select("id"=record_id, fecha_entrega, id_vale, entrega),
    entrega_vales4 %>% mutate(entrega="4")%>% select("id"=record_id, fecha_entrega, id_vale, entrega)
  ) %>% arrange(id) %>% select(-fecha_entrega) %>% spread(entrega, id_vale) %>% print(n=Inf) %>% writexl::write_xlsx("output/entregas_vales_consolidado.xlsx")
  
  canje_vales1 %>% select(record_id, fecha_canje, id_producto1, id_producto2, id_producto3, id_producto4) %>% bind_rows(
    canje_vales2 %>% select(record_id, fecha_canje, id_producto1, id_producto2, id_producto3, id_producto4, id_producto5),
    canje_vales3 %>% select(record_id, fecha_canje, id_producto1, id_producto2),
  ) %>% left_join(
    cat_productos %>% select("id_producto1"=Id_Producto, "prducto1"=Descripcion, "precio1"= Precio)
  ) %>% left_join(
    cat_productos %>% select("id_producto2"=Id_Producto, "prodcuto2"=Descripcion, "precio2"=Precio)
  ) %>% print(n=Inf)
  

  
  
  
canje_vales1 %>% select("id"=record_id, fecha_canje, id_vale1, id_vale2, id_vale3, id_vale4, id_producto1, id_producto2) %>% 
  arrange(id) %>% write_csv("output/lista_canjes_vales.csv")

  # group_by(id) %>% summarize(
  #   fechas=paste0(fecha_entrega, collapse = " "))

  
#-----------------------
#ENTREGA DE PRODUCTOS
  entrega_productos1 %>% mutate(entrega_producto=1) %>% select(id=record_id, fecha_entrega_producto, foto_entrega) %>% bind_rows(
    list(
    entrega_productos2 %>% mutate(entrega_producto=2) %>% select(id=record_id, fecha_entrega_producto, foto_entrega),
    entrega_productos3 %>% mutate(entrega_producto=2) %>% select(id=record_id, fecha_entrega_producto, foto_entrega),
    entrega_productos4 %>% mutate(entrega_producto=2) %>% select(id=record_id, fecha_entrega_producto, foto_entrega)
    
    )
  ) %>% mutate(doc_id=as.integer(foto_entrega)) %>%  left_join(
    edocs_metadata
    ) %>% 
    mutate(
      imagen_guatemala=if_else(is.na(stored_name), NA_character_,
            paste0('=HIPERVINCULO(','"','\\', '\\','172.30.1.57','\\','Data','\\','Salud Ambiental','\\','HAPIN_JALAPA','\\','Reportes','\\','Control','\\','edocs','\\',
                   stored_name,'"',')')
      ),
      imagen_jalapa=if_else(is.na(stored_name), NA_character_,
                            paste0('=HIPERVINCULO(','"','z:\\', 'HAPIN_JALAPA','\\','Reportes','\\','Vehiculos','\\','img','\\',stored_name,'"',')')
      )
    ) %>% select(-foto_entrega, -doc_id, -stored_name, -file_extension, -project_id, -stored_date, -delete_date, -date_deleted_server) %>% 
    write_csv(paste0("output/reporte_entrega_productos_control_",Sys.Date(),".csv"))
  
#revision p2 grupo control
 #con_36_semanas<-
   gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id,fecha_tamizaje=s6_date) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga) %>% mutate(
      fcc=fpp-280,
      edad_dias=Sys.Date()- as.Date(fcc),
      edad_semanas=as.numeric(edad_dias)%/%7,
      edad_dias_=as.numeric(edad_dias)%%7,
      edad_gestacional=paste0(edad_semanas,".",edad_dias_)
    ) %>% select(id_tamizaje, edad_gestacional)
  ) %>% filter(edad_gestacional>=36) %>%
    left_join(
   gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="p2") %>% select(id,fecha_p2=h41_date)
  ) %>% filter(!is.na(fecha_p2)) %>% filter(edad_gestacional>36) %>% left_join(
    salidas
  ) %>% filter(is.na(sale)) %>% left_join(
    nacidos %>% select(id, fecha_nacimiento)
  )
  
   #13 que se les hizo visita cuando tenian de 34 a 35 semanas
   gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id,fecha_tamizaje=s6_date) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="p2") %>% select(id,fecha_p2=h41_date)
  ) %>% filter(!is.na(fecha_p2)) %>% anti_join(
    con_36_semanas
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>%  
     left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga) %>% mutate(
      fcc=fpp-280,
      edad_dias=Sys.Date()- as.Date(fcc),
      edad_semanas=as.numeric(edad_dias)%/%7,
      edad_dias_=as.numeric(edad_dias)%%7,
      edad_gestacional=paste0(edad_semanas,".",edad_dias_)
    ) %>% select(id_tamizaje, edad_gestacional)
  )
  
#revision nacimientos grupo control
  nacidos<-gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id,fecha_tamizaje=s6_date) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
  ) %>% filter(!is.na(fecha_nacimiento))
  
  nacidos %>% mutate(
    limite_inferior_b2= as.Date(fecha_nacimiento) + 154,
    limite_superior_b2= as.Date(fecha_nacimiento) + 272,
    edad=Sys.Date()-as.Date(fecha_nacimiento)
  ) %>% mutate(
    flag_b2=if_else(edad>=154 & edad<=272, "1","0"
    )
  ) %>% filter(flag_b2=="1") %>% anti_join(
    salidas
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>% select(id,fecha_b2=h41_date)
  ) %>% filter(!is.na(fecha_b2))
    

  nacidos %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>% select(id,fecha_b2=h41_date)
  ) %>% filter(is.na(fecha_b2))
  
  
  #verificar nacimientos
  gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id,fecha_tamizaje=s6_date) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga) %>% mutate(
      fcc=fpp-280,
      edad_dias=Sys.Date()- as.Date(fcc),
      edad_semanas=as.numeric(edad_dias)%/%7,
      edad_dias_=as.numeric(edad_dias)%%7,
      edad_gestacional=paste0(edad_semanas,".",edad_dias_)
    ) %>% select(id_tamizaje, edad_gestacional)
  ) %>% filter(edad_gestacional>=40) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
  ) %>% anti_join(
    salidas
  )
  
  
  e3 %>% mutate(ultima_visita=
                  paste(e3_last_visit,e3_last_visit_o,e3_last_visit_c)
  ) %>% print(n=Inf)
  
 salida_control<- gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id,fecha_tamizaje=s6_date) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    salidas
    ) %>% filter(sale=="1") %>% left_join(
      salidas_detalle
    ) %>%  mutate(
      motivo_salida=case_when(
        e3_reason=="8" ~ "muerte",
        e3_reason<8 ~ "salida_anticipada",
        TRUE ~ NA_character_
      )
    ) %>% arrange(motivo_salida)

 salida_control %>% group_by(motivo_salida) %>% summarize(
   n=n()
 )
 
 #salidas por muerte y vales entregados / canjeados
 salida_control %>% filter(motivo_salida=="muerte") %>% left_join(
   vales_entregados %>% group_by(id) %>% summarize(n=n())
 ) %>% left_join(
   canjeados
 )
 
 #salidas voluntarias
 #33038 P2
 #33046 LB
 #33062 P1
 #33106 P1
 #33108 P1
 #33279 P2
 #33492 LB
 #35002 P1
 salida_control %>% filter(motivo_salida=="salida_anticipada") %>% left_join(
   vales_entregados %>% group_by(id) %>% summarize(n=n())
 ) %>% left_join(
   canjeados
 )
 
 canjeados<-vales_canjeados %>% group_by(id) %>% summarize(canjeados=n())
 #-------------------------------------
# 
#  
 
 # canjes_totales<-vales_canjeados %>% left_join(
 #   cat_productos %>% transmute(
 #     value=Id_Producto, Descripcion
 #   )
 # ) %>% select(id,codigo_producto=value, canje, Descripcion)
 
#revision de canjes anterior-----------------------------------
# 
# productos_entregados<-entrega_productos1 %>% mutate(entrega_producto=1) %>% select(id=record_id, fecha_entrega_producto, foto_entrega) %>% bind_rows(
#   entrega_productos2 %>% mutate(entrega_producto=2) %>% select(id=record_id, fecha_entrega_producto, foto_entrega)
# ) %>% mutate(doc_id=as.integer(foto_entrega)) %>%  left_join(
#   edocs_metadata
# ) %>% 
#   mutate(
#     imagen_guatemala=if_else(is.na(stored_name), NA_character_,
#                              paste0('=HYPERLINK(','"','\\', '\\','172.30.1.57','\\','Data','\\','Salud Ambiental','\\','HAPIN_JALAPA','\\','Reportes','\\','Control','\\','edocs','\\',stored_name,'"',')')
#     )
#     # imagen_jalapa=if_else(is.na(stored_name), NA_character_,
#     #                       paste0('=HYPERLINK(','"','z:\\', 'HAPIN_JALAPA','\\','Reportes','\\','Vehiculos','\\','img','\\',stored_name,'"',')')
#     # )
#   )

# canjes_octubre<- canje_vales1 %>% transmute(id=record, fecha_canje, canje="1") %>% bind_rows(
#   canje_vales2 %>% transmute(id=record, fecha_canje, canje="2")
# ) %>% filter(fecha_canje>="2019-12-01" & fecha_canje<="2019-12-31")
# 
# canjes_octubre<-canjes_octubre %>% mutate_all(as.character) %>% left_join(
#   canjes_totales %>% mutate_all(as.character)
# ) %>% anti_join(
#   productos_entregados %>% transmute(id=as.character(id), fecha_entrega_producto )
#   ) 
# 
# canjes_octubre %>% left_join(
#   datos_participantes %>% transmute(id=`ID estudio`,`Nombre embarazada`, comunidad_1=`Comunidad embarazada (nueva)`, comunidad_2=`Comunidad embarazada (original z10)` )
# ) %>% write_csv("output/canjes_diciembre_2019.csv")

#revision de entregas pendientes
# canjes_general<- canje_vales1 %>% transmute(id=record, fecha_canje, canje="1") %>% bind_rows(
#   canje_vales2 %>% transmute(id=record, fecha_canje, canje="2")
# ) %>% filter(fecha_canje<="2019-12-31")
# 
# canjes_general<-canjes_general %>% mutate_all(as.character) %>% left_join(
#   canjes_totales %>% mutate_all(as.character)
# ) %>% filter(canje=="1") %>% anti_join(
#   productos_entregados %>% transmute(id=as.character(id), fecha_entrega_producto )
# ) 
# 
# canjes_general %>% anti_join(
#   productos_entregados %>% select(id)
# ) %>% arrange(fecha_canje)
# 
# canjes_general %>% write_csv("output/entregas_pendientes.csv")
# 
# canje_vales1 %>% select(id=record, fecha_canje, id_producto1, id_producto2) %>% bind_rows(
#   canje_vales2 %>% select(id=record, fecha_canje, id_producto1, id_producto2)
# ) %>% gather(key = variable, value=value, -id, -fecha_canje) %>% mutate(
#    Id_Producto= case_when(
#     variable=="id_producto1" ~ value,
#     variable=="id_producto2" ~ value,
#     TRUE ~ NA_character_
#         ) ) %>% select(id, fecha_canje, Id_Producto) %>% left_join(
#           cat_productos %>% select(Id_Producto, Descripcion, Precio)
#         ) %>% write_csv("output/revision_canjes_pedidos.csv")


#lista de pedidos mensuales
 #--------------------------------------

 canje_vales1 %>% filter(is.na(fecha_canje))
 
 gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(id=="33211")

pedidos_hechos<-canje_vales1 %>% select(id=record, fecha_canje, id_producto1, id_producto2, id_producto3, id_producto4, 
                                        id_producto5, id_producto6) %>% bind_rows(
  canje_vales2 %>% select(
  id=record, fecha_canje, id_producto1, id_producto2, id_producto3, id_producto4, id_producto5, id_producto6),
  canje_vales3 %>% select(id=record, fecha_canje, id_producto1, id_producto2, id_producto3, id_producto4, id_producto5 ),
  canje_vales4 %>% select(id=record, fecha_canje, id_producto1 ),
) %>% gather(key = variable, value=value, -id, -fecha_canje) %>% mutate(
  Id_Producto= case_when(
    variable=="id_producto1" ~ value,
    variable=="id_producto2" ~ value,
    variable=="id_producto3" ~ value,
    variable=="id_producto4" ~ value,
    variable=="id_producto5" ~ value,
    variable=="id_producto6" ~ value,
    TRUE ~ NA_character_
  )
  ) %>% select(id, fecha_canje, Id_Producto) %>% left_join(
    cat_productos %>% select(Id_Producto, Descripcion, Precio)
  )

 
#pedidos_hechos %>% write_csv("output/productos_canjeados.csv")
#REPORTE PARA ENTREGAS DE PRODUCTOS INTEGRANDO PEDIDOS POR FECHA Y COMUNIDADES PARA ELABORAR MAPA
lista_entrega<-pedidos_hechos %>% left_join(
  #listado integrado del 1 de diciembre al 28 de enero
datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`, comunidad_1=`Comunidad embarazada (original z10)`,
                               comunidad_2=`Comunidad embarazada (nueva)`)
) %>% filter(fecha_canje>="2018-01-01" & fecha_canje<="2021-04-27") %>% filter(!is.na(Id_Producto)) %>%  select(
  ID=id,
  `Nombre de Participante`= `Nombre embarazada`,
  Comunidad_1=comunidad_1,
  Comunidad_2=comunidad_2,
  Fecha_solicitud=fecha_canje,
  `Codigo articulo solicitado`= Id_Producto,
  `Descripcion del Producto`= Descripcion,
  Precio
) 

lista_entrega %>% write_csv("output/lista_productos_2018-01-01_al_2021-04-27.csv")

#listado general Maya
lista_entrega %>% left_join(
  datos_participantes %>% select(`ID tamizaje`, ID=`ID estudio`,
                                 `Celular embarazada`, `Celular esposo`,
                                 `Celular_otro_miembro`
                                 )
) %>% write_csv("output/lista_productos_2018-01-01_al_2021-03-08.csv")


lista_entrega_general<-pedidos_hechos %>% left_join(
  #listado integrado del 1 de diciembre al 28 de enero
  datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`, comunidad_1=`Comunidad embarazada (original z10)`,
                                 comunidad_2=`Comunidad embarazada (nueva)`)
) %>%  filter(!is.na(Id_Producto)) %>%  select(
  ID=id,
  `Nombre de Participante`= `Nombre embarazada`,
  Comunidad_1=comunidad_1,
  Comunidad_2=comunidad_2,
  Fecha_solicitud=fecha_canje,
  `Codigo articulo solicitado`= Id_Producto,
  `Descripcion del Producto`= Descripcion,
  Precio
) 

cat_estufas<-read_csv("data/dictionaries/cat_estufas.csv")

lista_entrega_general %>% arrange(Fecha_solicitud) %>% left_join(
  cat_estufas %>% select(`Codigo articulo solicitado`=Id_Producto, tipo)
) %>% filter(!is.na(tipo)) %>% left_join(
  salidas %>% select("ID"=id, sale)
) %>% 
  group_by( `Descripcion del Producto`, tipo, sale) %>% count() %>% arrange(tipo) %>%
  write_csv("output/conteo_estufas.csv")
  
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="0") %>% left_join(
  salidas
) %>% filter(!is.na(sale))
# 238 slaidas hay 77 que pidieron estufa
# de esas 77 
# 1 biomasa       4
# 2 electrica    26
# 3 gas          47
  
cat_productos_mas_1100<-read_csv("data/dictionaries/cat_mas_1100.csv")
cat_tipos_productos<-read_csv("c:/temp/clasificacion_productos.csv")

lista_entrega_general %>% arrange(Fecha_solicitud) %>% left_join(
  cat_productos_mas_1100 %>% transmute(`Codigo articulo solicitado`=Id_Producto, tipo="mayor1100")
) %>% filter(!is.na(tipo)) %>% left_join(
  salidas %>% select("ID"=id, sale)
) %>% filter(!is.na(tipo)) %>% print(n=Inf)
  group_by(`Descripcion del Producto`, sale) %>% count() %>% arrange(sale) %>% write_csv(
  "output/productos_mayores_1100.csv"
)

#producto mas solicitado
  lista_entrega_general %>% left_join(
    cat_tipos_productos %>% select(`Codigo articulo solicitado`=cod_producto,
                                   Clasificacion)
  ) %>%  group_by(
    #`Descripcion del Producto`
   #ID, 
   Clasificacion
                 ) %>% count() %>% write_csv("output/conteo_productos_entregados.csv")
    #write_csv("output/conteo_productos_solicitados.csv")
  
#cat_productos %>% select(Id_Producto, Descripcion, Precio) 

#NOTA esta parte es para integrar los pedidos de canje 3 de diciembre al 5 de febrero que estaban pendientes que estaban pendientes
#pendientes de canje 3 y 4
# canjes_3_pendientes<-canje_vales3 %>% select(id=record, fecha_canje,id_producto1, id_producto2) %>%  gather(key = variable, value=value, -id, -fecha_canje) %>% 
#   mutate(
#     Id_Producto= case_when(
#       variable=="id_producto1" ~ value,
#       variable=="id_producto2" ~ value,
#       TRUE ~ NA_character_
#     ) ) %>% select(id, fecha_canje, Id_Producto) %>% filter(!is.na(Id_Producto)) %>%   left_join(
#       cat_productos %>% select(Id_Producto, Descripcion, Precio)
#     ) %>% left_join(
#       #listado integrado del 1 de diciembre al 28 de enero
#       datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`, comunidad_1=`Comunidad embarazada (original z10)`,
#                                      comunidad_2=`Comunidad embarazada (nueva)`)
#     ) %>% filter(fecha_canje>="2019-12-01" & fecha_canje<="2020-02-05") %>% filter(!is.na(Id_Producto)) %>%  select(
#       ID=id,
#       `Nombre de Participante`= `Nombre embarazada`,
#       Comunidad_1=comunidad_1,
#       Comunidad_2=comunidad_2,
#       Fecha_solicitud=fecha_canje,
#       `Codigo articulo solicitado`= Id_Producto,
#       `Descripcion del Producto`= Descripcion,
#       Precio
#     ) 
# 
# 
# lista_entrega %>% bind_rows(
#   canjes_3_pendientes
# ) %>% write_csv("output/lista_productos_del_06_feb_al_11_feb.csv")



#MAPA RUTA DE ENTREGA
# comunidad_coordenada<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id, 
#                                                              "fecha_consentimiento"=s4_date) %>% left_join(
#   comunidades %>% select(id_estudio, "comunidad_1"=community, "comunidad_2"=community_new, lat, long)
# ) %>% left_join(
#   gt_emory_data_arm1 %>% filter(!is.na(s1_date)) %>% select("id_tamizaje"=id, s1_community_name )
# ) %>% mutate(
#   comunidad=case_when(
#     !is.na(comunidad_1) ~ comunidad_1,
#     is.na(comunidad_1) & !is.na(comunidad_2) ~ comunidad_2,
#     is.na(comunidad_1) & is.na(comunidad_2) ~ s1_community_name
#   ) 
# ) %>% left_join(
#   lista_entrega %>% transmute(id_estudio=ID) %>% group_by(id_estudio) %>% summarize(cantidad_productos=n())
# ) %>% filter(!is.na(cantidad_productos)) %>% 
#   select(id_tamizaje, id_estudio, comunidad, lat, long) %>% filter(!is.na(lat)) %>%  group_by(comunidad) %>% 
#   summarize(cantidad_hogares=n(), latitud=mean(lat), longitud=mean(long)) %>% write_csv("output/comunidades_pedidos_coordenadas.csv")


##REVISION DE PRODUCTOS SOLICITADOS PENDIENTES DE ENTREGAR
pedidos_hechos %>% arrange(desc(fecha_canje)) %>% filter(!is.na(Id_Producto)) %>% left_join(

#productos entregados
entrega_productos1 %>% select(id=record, cantidad_productos_entrega, fecha=fecha_entrega_producto) %>% bind_rows(
  entrega_productos2 %>% select(id=record, cantidad_productos_entrega, fecha=fecha_entrega_producto),
  entrega_productos3 %>% select(id=record, cantidad_productos_entrega, fecha=fecha_entrega_producto)
)
) %>% mutate(
  producto_entregado=case_when(
    is.na(fecha) ~ "pendiente",
    fecha_canje > fecha ~ "pendiente",
    TRUE ~ "Si"
                      ) 
) %>% 
  write_csv("output/pedidos_entregas.csv")


#vales entregados
#vales entregados lo utilizaremos para descartar contra los vales recogidos al momento de entregar el producto
vales_entregados<-entrega_vales1 %>% mutate(entrega="1") %>% select("id"=record_id, id_vale, entrega, fecha_entrega) %>% 
  bind_rows(
    entrega_vales2 %>% mutate(entrega="2") %>% select("id"=record_id, id_vale, entrega, fecha_entrega),
    entrega_vales3 %>% mutate(entrega="3") %>% select("id"=record_id, id_vale, entrega, fecha_entrega),
    entrega_vales4 %>% mutate(entrega="4")%>% select("id"=record_id, id_vale, entrega, fecha_entrega)
  ) %>% filter(!is.na(id_vale)) %>% write_csv("output/vales_entregados.csv")

#vales recogidos en entrega de productos
entrega_productos1 %>% mutate(entrega_producto="1") %>% select(id=record_id, id_vale=id_vale1_v2, fecha_entrega_producto) %>% 
  arrange(id)
  bind_rows(
    entrega_productos2 %>% mutate(entrega_producto="2") %>% 
      select(id=record_id, id_vale=id_vale1_v2, fecha_entrega_producto)
  ) %>% 
    write_csv("output/vales_recogidos.csv")

  #cantidad de vales canjeados
  canje_vales1 %>% select(id=record, id_vale=id_vale1) %>% bind_rows(
    list(
      canje_vales1 %>% select(id=record, id_vale=id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale3) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale= id_vale4) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale1) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale3) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale4) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale1) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale2) %>% filter(!is.na(id_vale))
          )
  ) %>% write_csv("output/vales_canjeados.csv")
  
  canje_vales1 %>% select(id=record, id_vale=id_vale1, fecha=fecha_canje) %>% bind_rows(
    list(
      canje_vales1 %>% select(id=record, id_vale=id_vale2, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale3, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale= id_vale4, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale2, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale1, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale2, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale3, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale4, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale1, fecha=fecha_canje) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale2, fecha=fecha_canje) %>% filter(!is.na(id_vale))
    )
  ) %>% left_join(
    entrega_productos1 %>% transmute(id=record,fecha_entrega_producto)
  ) %>% write_csv("output/canjes y entregas.csv")
  
  
  #revision entrega de efectivo
  lista_productos_entregar<-read_csv("D:/Descargas/entregas_abril.csv")
  vales_saldo<-read_csv("D:/Descargas/vales_saldo.csv")
  lista_productos_entregar<-lista_productos_entregar %>% mutate_all(as.character)
  vales_saldo<-vales_saldo %>% mutate_all(as.character)
  
  lista_productos_entregar %>% left_join(
      datos_participantes %>% select(ID=`ID estudio`, `Celular embarazada`, `Celular esposo`, `Celular_otro_miembro` )
  ) %>% arrange(ID, Comunidad_1) %>% write_csv("output/lista_productos_datos_participantes.csv")
  
  vales_saldo %>% left_join(
  salidas_uvg %>% filter(!is.na(h56g_date)) %>% select(id=record_id) %>% bind_rows(
    salidas_emory %>% select(id)
  ) %>% group_by(id) %>% count() %>% mutate(h56="Si")
  ) %>% select(-n) %>% left_join(
    salidas %>% transmute(id, tiene_salida="Si")
  ) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, `Celular embarazada`, `Celular esposo`, `Celular_otro_miembro`, `Comunidad embarazada (nueva)`, 
                                   `Comunidad embarazada (original z10)` )
  ) %>% 
    write_csv("output/vales_saldo_revision.csv")
  
  
  
  
  #matriz para revisión de compensación
  
  #vales canjeados
  vales_canjeados<-canje_vales1 %>% select(id=record, id_vale=id_vale1) %>% bind_rows(
    list(
      canje_vales1 %>% select(id=record, id_vale=id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale3) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale= id_vale4) %>% filter(!is.na(id_vale)),
      canje_vales1 %>% select(id=record, id_vale=id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale1) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale2) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale3) %>% filter(!is.na(id_vale)),
      canje_vales2 %>% select(id=record, id_vale= id_vale4) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale1) %>% filter(!is.na(id_vale)),
      canje_vales3 %>% select(id=record, id_vale= id_vale2) %>% filter(!is.na(id_vale))
    )
  )
  vales_canjeados %>% select(id_vale, id) %>%  unique() %>% left_join(
    vales_entregados %>% select(id_vale, fecha_entrega) %>%  unique()
  ) %>% filter(is.na(fecha_entrega)) %>% write_csv("output/revisar_vales_canjeados_no_entregados.csv")
 
 
  gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
  ) %>% left_join(
    salidas %>% transmute(id, salida="Si")
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit, e3_reason)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_exit_o, e3_reason_o)
      ) %>% left_join(
        gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c)
      ) %>% mutate(type=if_else(grepl("^35",id), "owa", "pwg"))  %>% 
    mutate(
        fecha_salida=case_when(
          !is.na(e3_date_exit_c) & e3_reason_c=="6" ~ as.Date(e3_date_exit_c),
          !is.na(e3_date_exit) & type=="pwg" ~ as.Date(e3_date_exit),
          !is.na(e3_date_exit) & is.na(e3_date_exit_o) & type=="owa" ~ e3_date_exit_o,
          !is.na(e3_date_exit) & !is.na(e3_date_exit_o) & type=="owa" ~ if_else(as.Date(e3_date_exit)>as.Date(e3_date_exit_o), as.Date(e3_date_exit), as.Date(e3_date_exit_o))
        )
      ) %>% 
    mutate(
        fecha_nacimiento=if_else(
          is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)
        ), 
        nacio=if_else(is.na(c30_dob), "Si", "No")
        # edad_bebe_salida= fecha_salida - fecha_nacimiento,
        # edad_bebe_no_salida= Sys.Date() - as.Date(fecha_nacimiento)
      ) %>% mutate(
        e3_reason=recode(e3_reason, "1"="Finalizacion del estudio",
                         "2"="No elegible",
                         "3"="Retiro voluntario de laparticipante",
                         "4"="Retirada por el equipo del estudio",
                         "5"="Se mudo del area de estudio",
                         "6"="Fallecio",
                         "7"="Perdido durante el seguimiento",
                         "8"="Madre: Aborto/ Aborto espontaneo, mortinato ,muerte del nino",
                         "555"="Otro"),
        e3_reason_o=recode(e3_reason_o, "1"="Finalizacion del estudio",
                         "2"="No elegible",
                         "3"="Retiro voluntario de laparticipante",
                         "4"="Retirada por el equipo del estudio",
                         "5"="Se mudo del area de estudio",
                         "6"="Fallecio",
                         "7"="Perdido durante el seguimiento",
                         "555"="Otro"),
        e3_reason_c=recode(e3_reason_c, "1"="Finalizacion del estudio",
                           "2"="No elegible",
                           "3"="Retiro voluntario de laparticipante",
                           "4"="Retirada por el equipo del estudio",
                           "5"="Se mudo del area de estudio",
                           "6"="Fallecio",
                           "7"="Perdido durante el seguimiento",
                           "555"="Otro"),
        
      ) %>% 
    mutate(
        motivo_salida=case_when(
          !is.na(e3_date_exit_c) & e3_reason_c=="6" ~ e3_reason_c,
          !is.na(e3_date_exit) & type=="pwg" ~ e3_reason,
          !is.na(e3_date_exit) & is.na(e3_date_exit_o) & type=="owa" ~ e3_reason_o,
          !is.na(e3_date_exit) & !is.na(e3_date_exit_o) & type=="owa" ~ if_else(as.Date(e3_date_exit)>as.Date(e3_date_exit_o), 
                                                                                e3_reason, e3_reason_o)
        ),
        fcc= as.Date(m17_ga) - lubridate::days(280)
      ) %>% 
     mutate(
        edad_bebe=case_when(
          salida=="Si" & is.na(c30_dob) ~ paste0((as.Date(fecha_salida) - as.Date(fcc))/ 7, " semanas"),
          salida=="Si" & !is.na(c30_dob) ~ paste0(round((as.Date(fecha_salida)- as.Date(fecha_nacimiento))/30), " meses"),
          is.na(salida) & !is.na(c30_dob) ~ paste0(round((Sys.Date()- as.Date(fecha_nacimiento))/30), " meses")
        )
      ) %>% left_join(
        vales_entregados %>% group_by(id) %>% count()
      ) %>% left_join(
        vales_canjeados %>% select(id_vale, id) %>%  unique() %>% group_by(id) %>% count() %>% select(id, vales_canjeados=n)
      ) %>% transmute(
        id_tamizaje,
        id,
        type,
        salida=if_else(is.na(salida), "No", salida),
        vales_entregados=n,
        vales_canjeados,
        fecha_salida,
        motivo_salida,
        edad_bebe
      ) %>% write_csv("output/revision_datos_compensacion.csv")

  
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
    type=if_else(grepl("^35[0-9]", id), "owa","pwg")
  )  %>% group_by(type, s6_arm) %>% count()

  
 
