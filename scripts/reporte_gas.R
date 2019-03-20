#---------------------------------------
#librerias para reportes de gas hapin Jalapa
#---------------------------------------

library(package = "tidyverse")

# Most recent export for Repeated CRFs project
gt_emory_repeat_file <- list.files(
  path = "data/exports", pattern = "Repeat.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))


# Get the data from the most recent export
gt_emory_repeat_data <- gt_emory_repeat_file %>%
  pull(file) %>%
  read_csv() %>%
  mutate_at(
    vars(matches("hhid")),
    funs(as.character)
  ) %>%
  mutate(
    # Default change for all "monthly" visits
    visit = "mensual",
    arm = case_when(
      grepl("^h46", redcap_event_name) ~ "ambient",
      grepl("^h51", redcap_event_name) ~ "gas"
    ),
    cor_crf = case_when(
      !is.na(h51_hhid) & is.na(h46b_hhid) ~ "h51",
      !is.na(h46b_hhid) & is.na(h51_hhid) ~ "h46",
      TRUE ~ NA_character_
    ),
    id = case_when(
      !is.na(h51_hhid) & is.na(h46b_hhid) ~ h51_hhid,
      !is.na(h46b_hhid) & is.na(h51_hhid) ~ h46b_hhid,
      TRUE ~ NA_character_
    ),
    manual_id = gsub("^ *([0-9]*).+", "\\1", record_id),
    is_id = grepl("^ *[0-9]{5} +", record_id)
  ) %>%
  select(redcap_event_name, visit, id, manual_id, is_id, everything())


current_emory_gas_data <- gt_emory_repeat_data %>%
  # usar solo los de entrega de gas
  filter(redcap_event_name == "h51_gas_arm_1") %>%
  # como caracter para preservar fechas y horas
  mutate_all(as.character) %>%
  # Algunos IDs no los escanearon. Hay que revisar, pero mientras los reemplazamos
  # por el manual
  mutate(
    id = if_else(
      condition = is.na(id),
      true = manual_id,
      false = id
    )
  ) %>%
  # conservando variables de id y las que te interesan del h51
  select(
    house_id = id, request_date = h51_refill_date, 
    matches("h51_(by|date|refill|install|remove|leak|repair)")
  ) %>%
  mutate(
    source = "emory_current"
  )

#current emory con fugas
#current_emory_gas_fugas<- current_emory_gas_data %>% 
        #    select(house_id, h51_date, h51_by, h51_leak___1, h51_repair) %>% filter(!is.na(h51_leak___1)) %>% filter(h51_leak___1=="0") %>% filter(h51_repair=="0") %>%  print()

#------------------------------------------------------------------------------*
# Previous source - UVG RedCap repeated instruments project ----
#------------------------------------------------------------------------------*


# Most recent export for Repeated CRFs project
gt_repeat_file <- list.files(
  path = "data/exports", pattern = "Repeti.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))


# Get the data from the most recent export
gt_repeat_data <- gt_repeat_file %>%
  pull(file) %>%
  read_csv() %>%
  mutate(
    # Default change for all "monthly" visits
    visit = "mensual",
    arm = case_when(
      grepl("entgrega", redcap_event_name) ~ "gas",
      grepl("uso_de", redcap_event_name) ~ "stove_use",
      grepl("llamadas", redcap_event_name) ~ "preg_calls"
    ),
    cor_crf = case_when(
      !is.na(h51_date) & is.na(h54_date) ~ "h51",
      !is.na(h54_date) & is.na(h51_date) ~ "h54",
      arm == "preg_calls" ~ "h54",
      TRUE ~ NA_character_
    ),
    manual_id = gsub("^ *([0-9]*).+", "\\1", id),
    id = case_when(
      cor_crf == "h51" ~ as.character(record_id_gas),
      cor_crf == "h54" ~ as.character(record_id),
      cor_crf == "calls" ~ as.character(id_hogar),
      TRUE ~ NA_character_
    ),
    is_id = grepl("^ *[0-9]{5} +", manual_id)
  ) %>%
  select(redcap_event_name, visit, id, manual_id, is_id, everything(), matches("h51_(leak|repair)"))


gt_gas_data <- gt_repeat_data %>%
  filter(cor_crf == "h51", !is.na(h51_date)) %>%
  # como caracter para preservar fechas y horas
  mutate_all(as.character) %>%
  # conservando variables de id y las que te interesan del h51
  mutate(
    h51_install_id1 = if_else(
      condition = is.na(h51_install_id1),
      true = if_else(
        condition = is.na(h51_install_scan_id1),
        true = h51_install_id_manual1,
        false = h51_install_scan_id1
      ),
      false = h51_install_id1
    ),
    h51_install_id2 = if_else(
      condition = is.na(h51_install_id2),
      true = if_else(
        condition = is.na(h51_install_scan_id2),
        true = h51_install_id_manual2,
        false = h51_install_scan_id2
      ),
      false = h51_install_id2
    ),
    h51_remove_id1 = if_else(
      condition = is.na(h51_remove_id1),
      true = if_else(
        condition = is.na(h51_remove_scan_id1),
        true = h51_remove_id_manual1,
        false = h51_remove_scan_id1
      ),
      false = h51_remove_id1
    ),
    h51_remove_id2 = if_else(
      condition = is.na(h51_remove_id2),
      true = if_else(
        condition = is.na(h51_remove_scan_id2),
        true = h51_remove_id_manual2,
        false = h51_remove_scan_id2
      ),
      false = h51_remove_id2
    )
  ) %>%
  select(
    house_id = id, request_date = h51_refill_date,
    matches("h51_(by|date|refill|install|remove)"),
    -matches("scan|manual")
  ) %>%
  mutate(
    source = "uvg"
  ) #%>%
  #print()

#gt_repeat_data_fugas<- gt_repeat_data %>% select(id,h51_date,h51_leak___1,h51_leak___2,h51_leak___3,h51_leak___4,h51_leak___5,h51_leak___6,h51_leak___555) %>% filter(!is.na(h51_leak___1)) %>%  print()


#------------------------------------------------------------------------------*
# Original use - Emory RedCap montly events in main study project ----
#------------------------------------------------------------------------------*

source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")




stove_install_data <- gt_emory_data %>%
  select(house_id = id, visit, matches("h50")) %>%
  filter(
    !is.na(h50_date)
  ) %>%
  mutate_all(as.character) %>%
  mutate(
    source = "emory-stove-install"
  ) %>%
  select(
    house_id,
    h51_by = h50_by,
    h51_date = h50_date,
    h51_install_id1 = h50_tank_id1,
    h51_install_id2 = h50_tank_id2,
    h51_install_wt1 = h50_tank_wt1,
    h51_install_wt2 = h50_tank_wt2,
    source
  ) #%>%
  #print()




original_emory_gas_data <- gt_emory_data %>%
  # solo los datos que te interesan
  filter(!is.na(h51_date)) %>%
  # conservar solo las variables que te interesan
  select(
    house_id = id, redcap_event_name, request_date = h51_refill_date,
    matches("h51_(by|date|refill|install|remove)")
  ) %>%
  # como caracter para preservar fechas y horas
  mutate_all(as.character) %>%
  # juntar todas las copias en un mismo bloque
  gather(column, value, matches("h51_"), na.rm = TRUE) %>%
  mutate(
    column = if_else(
      condition = !grepl("_v[0-9]+", column),
      true = paste0(column, "_v1"),
      false = column
    )
  ) %>%
  extract(
    column, into = c("variable", "version"),
    regex = "(.+)_v([0-9]+)"
  ) %>%
  spread(variable, value) %>%
  select(-redcap_event_name, -version) %>%
  mutate(
    source = "emory_original"
  ) %>% print()


#gt_emory_data %>% select(id,h51_date,h51_leak___1) %>% filter(!is.na(h51_date)) %>% filter(!is.na(h51_leak___1)) %>% filter(h51_leak___1=="0") %>% print()

#------------------------------------------------------------------------------*
# Join all data sources ----
#------------------------------------------------------------------------------*



# structure gas delivery data for analysis
all_gas_actions <- list(
  stove_install_data,
  original_emory_gas_data,
  gt_gas_data,
  current_emory_gas_data
) %>%
  bind_rows() %>%
  # darle id unico a cada fila
  rownames_to_column() %>%
  # bajar todas las variables que se repiten varias veces
  gather(column, value, matches("h51_[ir]")) %>%
  # separar la variable de la repetición
  extract(
    column, into = c("action", "variable", "rep"),
    regex = "h51_([^_]+)_([^0-9]+)([0-9]+)"
  ) %>%filter(!is.na(variable)) %>% 
  # subir las variables (id y peso) a columnas
  # slice(c(6449, 14509, 16121))
  spread(variable, value, convert = TRUE) %>%
  select(
    source, house_id, by = h51_by, request_date,
    cyl_id = id, cyl_weight = wt, date = h51_date, action
  ) %>%
  filter(
    !is.na(cyl_id),
    ! (grepl("^8+$", cyl_id) & is.na(cyl_weight))
  ) %>%
  mutate(
    action = factor(action, levels = c("remove", "install"))
  ) %>%
  arrange(house_id, date, action) %>% left_join(
                                gt_emory_data %>% select(house_id=id, s6_date, s6_arm) %>% filter(!is.na(s6_date))
                                ) #%>% 
  #print()
#-----------------
#imprimir resumen de H51 integrado
#-----------------
all_gas_actions %>% write_csv(paste("output/resumen_h51_",Sys.Date(),".csv"))


cantidad_cilindros<-all_gas_actions %>%
  #marcar hogares con cilindros de cien libras
    mutate(
            cyl_cien=case_when(house_id=="33040" ~ "Si",
                     house_id=="33110" ~ "Si",
                     house_id=="35021" ~ "Si",
                     house_id=="35033" ~ "Si",
                     TRUE ~ "No")
  ) %>% filter(cyl_cien!="Si" & action=="install") %>% select(
  house_id, action, s6_date) %>% left_join(gt_emory_data %>% select(house_id=id, h50_date) %>% filter(!is.na(h50_date))
                                        ) %>% filter(!is.na(h50_date)) %>%
  group_by(house_id) %>% summarize(contador=n())

cantidad_cilindros %>% left_join(
  gt_emory_data %>% select(house_id=id, h50_date) %>% filter(!is.na(h50_date))
) %>% left_join(gt_emory_data %>%
                  select(house_id=id, m10_date, m10_sleep) %>%
                  filter(!is.na(m10_date)) %>% print()
)



cantidad_cilindros_fechas<-all_gas_actions %>%
  #marcar hogares con cilindros de cien libras
  mutate(
    cyl_cien=case_when(house_id=="33040" ~ "Si",
                       house_id=="33110" ~ "Si",
                       house_id=="35021" ~ "Si",
                       house_id=="35033" ~ "Si",
                       TRUE ~ "No")
  ) %>% filter(cyl_cien!="Si" & action=="install" & source!="emory-stove-install") %>% mutate(
    fecha_install=date
  ) %>%
  select(
    house_id, action, fecha_install, request_date, s6_date) %>% left_join(gt_emory_data %>% select(house_id=id, h50_date) %>% filter(!is.na(h50_date))
    ) %>% filter(!is.na(h50_date)) %>%
  group_by(house_id) %>%
    summarize_at(vars(fecha_install), funs(firts_date=min(fecha_install), last_date=max(fecha_install), pedidos=n())) %>% mutate(
      libras=pedidos*25
    ) %>% left_join(gt_emory_data %>%
                      select(house_id=id, m10_date, m10_sleep) %>%
                      filter(!is.na(m10_date))
    ) %>% left_join(
        gt_emory_data %>% select(house_id=id,h50_date) %>% filter(!is.na(h50_date))
      ) %>% mutate(
        cilindros_utilizados= 2+pedidos,
        fecha_install_estufa=h50_date
      ) %>% mutate(
        dias_uso = as.Date(last_date) - as.Date(fecha_install_estufa)
      ) %>% select(house_id, date_stove=fecha_install_estufa,
                   primer_pedido=firts_date,
                   ultimo_pedido=last_date,
                   cilindros_utilizados,
                   libras_utilizadas=libras,
                   dias_uso,
                   cantidad_personas=m10_sleep
                   )




tiempos_entrega<-all_gas_actions %>%
  #marcar hogares con cilindros de cien libras
  mutate(
    cyl_cien=case_when(house_id=="33040" ~ "Si",
                       house_id=="33110" ~ "Si",
                       house_id=="35021" ~ "Si",
                       house_id=="35033" ~ "Si",
                       TRUE ~ "No")
  ) %>% filter(cyl_cien!="Si" & action=="install" & source!="emory-stove-install") %>% mutate(
    fecha_install=date
  ) %>%
  select(
    house_id, action, fecha_install, request_date, s6_date) %>%
      mutate_at(
            vars(fecha_install, request_date),
            funs(as.Date)
          ) %>%
  mutate(
      demora_instalacion = as.numeric(fecha_install - request_date, units = "days")
    ) %>%  group_by(house_id) %>%
  summarize(
    entregas = n(),
    promedio_demora = mean(demora_instalacion, na.rm = TRUE),
    max_demora = max(demora_instalacion, na.rm = TRUE)
  )

#agregar comunidades
cat_comunidades<-read_csv("data/dictionaries/cat_comunidadez_z10.csv")
comunidades<-gt_emory_data %>% select(house_id=s4_main_id, id_tamizaje=id, s4_date) %>% filter(!is.na(house_id)) %>% left_join(
 gt_participants %>% select(record_id,id_estudio, codigo=com_jalapa) %>% 
      mutate(house_id=if_else(condition = grepl("^G[0-9]{4}",id_estudio),
                                           true = record_id,
                                           false = id_estudio
 )) %>% left_join(cat_comunidades %>% mutate(codigo=as.character(codigo)))
)

cantidad_cilindros_fechas %>% left_join(
  tiempos_entrega
) %>% left_join(comunidades %>% select(house_id, comunidad)) %>%mutate(
  meses_uso_gas=dias_uso / 30,
  promedio_cilindro_mes=as.numeric(cilindros_utilizados) / as.numeric(meses_uso_gas),
  libras_por_dia=as.numeric(libras_utilizadas)/as.numeric(dias_uso),
  libras_por_dia_persona=libras_por_dia/as.numeric(cantidad_personas),
  promedio_demora
) %>% mutate(
  meses_uso_gas=round(meses_uso_gas, digits = 2),
  promedio_cilindro_mes=round(promedio_cilindro_mes, digits = 2),
  libras_por_dia=round(libras_por_dia, digits = 2),
  libras_por_dia_persona=round(libras_por_dia_persona, digits = 2),
  promedio_demora=round(promedio_demora, digits = 2),
  
) %>%  select(house_id, date_stove, primer_pedido, 
             ultimo_pedido, "Cantidad de cilindros utilizados"=cilindros_utilizados, 
             "Cantidad de meses de uso de gas"=meses_uso_gas, "Promedio de cilindros por mes"=promedio_cilindro_mes, 
             "Cantidad de libras de gas utilizadas"=libras_utilizadas, "Cantidad de dias de uso de gas"=dias_uso, "Cantidad de personas en el hogar"=cantidad_personas, 
             "Promedio de libras consumidas por dia"=libras_por_dia, "Promedio de libras por persona"=libras_por_dia_persona, "Cantidad de entregas de gas registradas"=entregas, 
             "Promedio demora en entrega"=promedio_demora, "Maximo demora en entrega"=max_demora,"Comunidad"=comunidad) %>% 
  writexl::write_xlsx(paste0("output/reporte_promedios_uso_gas_",Sys.Date(),".xlsx"))
# 
# 
# 
# #----------------------------------------- %>% 
# #PENDIENTES DE H50
# #-----------------------------------------
# 


H50_faltante <- gt_emory_data %>%
          select(id,s6_date, s6_arm) %>%
              filter(!is.na(s6_date) & s6_arm=="1") %>%
          left_join(
                    gt_emory_data %>% select(id_tamizaje=id, id=s4_main_id, s4_date,s1_community_name) %>%
                          filter(!is.na(s4_date))
            ) %>%#agregamos la cantidad de personas que duermen en el hogar
        left_join(
            gt_emory_data %>% select(id,m10_date,m10_sleep) %>% filter(!is.na(m10_date))
        ) %>%
    left_join(
        gt_emory_data %>% select(id_tamizaje=id, m17_date, m17_ga) %>% filter(!is.na(m17_date))
    ) %>% left_join(
      gt_emory_data %>% select(id,h50_date) %>% filter(!is.na(h50_date))
    ) %>% #filtramos los faltantes de h50
      filter(is.na(h50_date)) %>% #descartamos las salidas registradas en e3
      left_join(
        gt_emory_data %>% select(id, e3_date) %>% filter(!is.na(e3_date))
          ) %>% filter(is.na(e3_date)) %>% #agregamos edad gestacional
              mutate(
                  fecha_concepcion = as.Date(m17_ga) - lubridate::days(280),
                  edad_semanas = as.numeric(Sys.Date() - fecha_concepcion) %/%7,
                  dias= as.numeric(Sys.Date()- fecha_concepcion) %%7,
                  dias_ramdom= as.numeric(Sys.Date() - as.Date(s6_date)),
                  edad_gestacional = paste(edad_semanas,"s", dias, "d")
                ) %>% arrange(desc(dias_ramdom)) %>% #cambiamos etiquetas para generar el export
                    select("Id Hogar (S6)"=id, "Id Tamizaje (z10)"=id_tamizaje,
                         "comunidad (s1)"=s1_community_name, "Consentimiento (S4)"=s4_date,
                         "USG (m17)"=m17_date, "Personas por Hogar (m10)"=m10_sleep,
                         "Edad gestacional (M17)"= edad_gestacional,
                         "Fecha Randomizacion (6)" = s6_date,
                         "Dias desde Randomizacion Fecha Actual" = dias_ramdom
                          ) %>% #%>% #exportamos el reporte con fecha en formato excel
                    writexl::write_xlsx(paste0("output/Pendientes_instalacion_estufas_",Sys.Date(),".xlsx"))
# #------------------------------------
# #pendientes de S6
# #------------------------------------
# salidas<-gt_emory_data %>% select(id,e3_date) %>% filter(!is.na(e3_date)) %>%
#   left_join(
#     gt_emory_data %>% select(id,e3_date_o) %>% filter(!is.na(e3_date_o))
#   ) %>%left_join(
#     gt_emory_data %>% select(id,e3_date_c) %>% filter(!is.na(e3_date_c))
#   ) %>% left_join(
#     gt_emory_data %>% select(id, c30_date) %>% filter(!is.na(c30_date))
#   ) %>%mutate(
#     type = if_else(
#       condition = grepl("^35[0-9]{3}", id),
#       true = "oaw",
#       false = "pw"
#     )
#   ) %>% mutate(
#     sale = case_when(
#       type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
#       type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
#       type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       TRUE ~ NA_character_
#     )
#   ) %>%filter(sale=="1")
# 
# salida_after_s6 <- s6 %>% left_join(salidas) %>%
#   mutate(
#     dias_s6_e3=e3_date-s6_date
#   ) %>% filter(sale==1)
# 
# salidas_antes_s6 <- salidas %>% left_join(s6) %>% filter(is.na(s6_date))
# 
# 
# 
# total_h42 <- gt_emory_data %>% select(id, h42_date, redcap_event_name) %>% filter(!is.na(h42_date)) %>%
#     filter(redcap_event_name=="linea_de_base_arm_2") %>%  
#   left_join(
#     gt_emory_data %>% select(id, b10_date, redcap_event_name) %>% filter(!is.na(b10_date)) %>% 
#       filter(redcap_event_name=="linea_de_base_arm_2")) %>% left_join(
#     salidas_antes_s6
#   ) %>% filter(is.na(sale))
#   
# s6 <- gt_emory_data %>% select(id,s6_date,s6_arm) %>% filter(!is.na(s6_date))
# 
# s1 <- gt_emory_data %>% select(
#          id_tamizaje=id,s1_date, s1_community_name
#               ) %>%  filter(!is.na(s1_date)) %>% 
#                 left_join(
#                   gt_emory_data %>% select(id_tamizaje=id, s4_date, id=s4_main_id) %>% 
#                     filter(!is.na(s4_date))) %>% 
#                       filter(!is.na(id)) %>% print()
# 
# total_h42 %>% select(id, h42_date, b10_date) %>%  
#     left_join(s6) %>% 
#       filter(is.na(s6_date)) %>% 
#         mutate(
#           fecha_limite_s6=as.Date(h42_date)+lubridate::days(7)
#           ) %>%  
#   select(id, h42_date,b10_date,s6_date,fecha_limite_s6) %>% 
#       mutate(
#         revisar=case_when( 
#           fecha_limite_s6<=Sys.Date() ~ "Revisar",
#           TRUE ~ ""
#           )
#         ) %>% mutate(
#           dias_h42_hoy=Sys.Date()-as.Date(h42_date)
#         ) %>%  left_join(
#           s1
#         ) %>% select(
#           id_tamizaje, id, "comunidad"=s1_community_name, h42_date, b10_date, fecha_limite_s6, dias_h42_hoy, revisar 
#         ) %>% print() %>% write_csv(paste("output/h42_sin_s6_", Sys.Date(),".csv"))
# 






























