#------------------------------------------------------------------------------*
# Summarize recorded lpg deliveies ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")



#------------------------------------------------------------------------------*
# Current data source - Emory RedCap repeated instruments project ----
#------------------------------------------------------------------------------*


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
    house_id = id,
    matches("h51_(by|date|refill|install|remove)")
  ) %>%
  mutate(
    source = "emory_current"
  )



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
      grepl("engrega", redcap_event_name) ~ "gas",
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
  select(redcap_event_name, visit, id, manual_id, is_id, everything())


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
    house_id = id,
    matches("h51_(by|date|refill|install|remove)"),
    -matches("scan|manual")
  ) %>%
  mutate(
    source = "uvg"
  ) %>%
    print()




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
  ) %>%
  print()




original_emory_gas_data <- gt_emory_data %>%
  # solo los datos que te interesan
  filter(!is.na(h51_date)) %>%
  # conservar solo las variables que te interesan
  select(
    house_id = id, redcap_event_name,
    matches("h51_(by|date|refill|install|remove)")
  ) %>%
  # como caracter para preservar fechas y horas
  mutate_all(as.character) %>%
  # juntar todas las copias en un mismo bloque
  gather(column, value, matches("h51_")) %>%
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
  ) %>%
  print()




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
  ) %>%
  # subir las variables (id y peso) a columnas
  spread(variable, value, convert = TRUE) %>%
  select(
    source, house_id, by = h51_by,
    cyl_id = id, cyl_weight = wt, date = h51_date, action
  ) %>%
  filter(
    !is.na(cyl_id),
    ! (grepl("^8+$", cyl_id) & is.na(cyl_weight))
  ) %>%
  mutate(
    action = factor(action, levels = c("remove", "install"))
  ) %>%
  arrange(house_id, date, action) %>%
  print()



# promedios de uso de gas



# unir instalacion de cilindros con retiro de cilindros
all_gas_use <- all_gas_actions %>%
  filter(action == "install") %>%
  select(-action) %>%
  # con un full join por si hay IDs que no aparecen en los dos
  full_join(
    all_gas_actions %>%
      filter(action == "remove") %>%
      select(-action),
    # decir por cuáles unir
    by = c(house_id = "house_id", cyl_id = "cyl_id")
    # El resto de variables van a decir .x al final del nombre para las
    # instalaciones y .y para los retiros
  ) %>%
  # hacer explicitos los nombres de las variables repetidas
  set_names(
    names(.) %>%
      gsub(
        pattern = "[.]x",
        replacement = "_install",
        x = .
      ) %>%
      gsub(
        pattern = "[.]y",
        replacement = "_remove",
        x = .
      )
  ) %>%
  # Calcular el uso
  mutate(
    # dias de uso
    use_days = as.numeric(
      as.Date(date_remove) - as.Date(date_install),
      units = "days"
    ),
    # peso al instalar - peso al quitar
    lpg_use = (cyl_weight_install - cyl_weight_remove) * 2.20462
  ) %>%
  # quitar las que tienen la misma fecha
  filter(date_install != date_remove, use_days > 0) %>%
  # agregar la comunidad
  left_join(
    gt_emory_data %>%
      filter(!is.na(s1_date) | !is.na(s4_main_id)) %>%
      select(
        house_id = s4_main_id, community = s1_community_name
      )
  ) %>%
  select(source_install, community, everything()) %>%
  writexl::write_xlsx(
    path = paste0("output/all_use_", Sys.Date(), ".xlsx")
  )
  
  print(n = Inf)




consumo_casa <- all_gas_use %>%
  group_by(house_id) %>%
  summarize(
    entregas = n(),
    n_cilindros_rapidos = sum(use_days < 10, na.rm = TRUE),
    total_consumo_libras = sum(lpg_use, na.rm = TRUE),
    total_consumo_dias = sum(use_days, na.rm = TRUE),
    consumo_libras_diario = total_consumo_libras / total_consumo_dias,
    promedio_duracion_cilindro = total_consumo_dias / entregas
  ) %>%
  mutate_if(
    is.double,
    funs(round(., 1))
  ) %>%
  print()



entregas_dias <- all_gas_actions %>%
  filter(action == "install", !is.na(date), !is.na(cyl_weight)) %>%
  group_by(fecha_entrega = date) %>%
  summarize(
    cilindros_25 = sum(cyl_weight < 50),
    cilindros_100 = sum(cyl_weight >= 50),
    entregas_gas = n()
  ) %>%
  print()





consumo_casa %>%
  mutate(
    grupo = case_when(
      n_cilindros_rapidos > 0 | promedio_duracion_cilindro < 15 ~ "Uso elevado",
      TRUE ~ "Uso esperado"
    )
  ) %>%
  arrange(promedio_duracion_cilindro, n_cilindros_rapidos) %>%
  split(.$grupo) %>%
  map(
    ~ .x %>%
      select(
        Hogar = house_id,
        "Total de entregas de gas" = entregas,
        "Total de consumo de gas (libras)" = total_consumo_libras,
        "Consumo diario promedio (libras)" = consumo_libras_diario,
        "Promedio de duración de cilindro" = promedio_duracion_cilindro,
        "Cilindros consumidos en menos de 10 días" = n_cilindros_rapidos
      )
  ) %>%
  c(
    "Entregas diarias" = list(entregas_dias)
  ) %>%
  writexl::write_xlsx(
    path = paste0("output/uso-gas_", Sys.Date(), ".xlsx")
  )




todas_entregas <- all_gas_actions %>%
  filter(action == "install", !is.na(date), !is.na(cyl_weight)) %>%
  arrange(date) %>%
  mutate(
    tipo = recode(
      source,
      "emory-stove-install" = "instalación-estufa",
      .default = "refill-gas"
    )
  ) %>%
  select(
    tipo, fecha = date, id = house_id, iniciales = by,
    cilindro = cyl_id, peso_instalado = cyl_weight
  ) %>%
  print()



todas_entregas %>%
  list(
    todas = .,
    mensuales = mutate(
      .,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    ) %>%
    group_by("Año" = year, "Mes" = month) %>%
    summarize(
      total_cilindros = n(),
      cilindros_25 = sum(peso_instalado < 50),
      cilindros_100 = sum(peso_instalado >= 50)
    )
  ) %>%
  writexl::write_xlsx(
    path = paste0("output/entregas-gas_", Sys.Date(), ".xlsx")
  )


#codigo resumido Oscar
all_gas_actions %>%
  filter(as.Date(date) >= as.Date("2018-11-06")) %>%
  group_by(house_id, action) %>%
  summarize(
    n = n(),
    first_date = min(as.Date(date), na.rm = FALSE),
    last_date = max(as.Date(date), na.rm = FALSE),
    weight = sum(cyl_weight, na.rm = TRUE)
  ) %>%
  mutate(
    n = paste(action, n) %>% paste0(collapse = ", "),
    first_date = min(first_date, na.rm = TRUE),
    last_date = max(last_date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(action) %>%
  spread(action, weight) %>%
  mutate(
    days = as.numeric(last_date - first_date, unit = "days"),
    gas_used = (install - remove) * 2.20462
  ) %>%
  # agregar la comunidad
  left_join(
    gt_emory_data %>%
      filter(!is.na(s1_date) | !is.na(s4_main_id)) %>%
      select(
        house_id = s4_main_id, community = s1_community_name
      )
  ) %>%
  # agregar fecha de estufa
  left_join(
    gt_emory_data %>%
      filter(!is.na(h50_date) | !is.na(s6_date)) %>%
      select(
        house_id = id, fecha_estufa = h50_date, grupo = s6_arm
      )
  ) %>%
  select(community, house_id, grupo, fecha_estufa, everything()) %>%
  print(n = Inf)%>%
  writexl::write_xlsx(
    path = paste0("output/entregas_comunidad_", Sys.Date(), ".xlsx")
  )

#grafica codigo revisado con oscar

all_gas_actions %>%
  filter(!is.na(request_date), action == "install") %>%
  mutate_at(
    vars(date, request_date),
    funs(as.Date)
  ) %>%
  mutate(
    demora_instalacion = as.numeric(date - request_date, units = "days")
  ) %>% {
    filter(., demora_instalacion > 5) %>% print()
    
    writexl::write_xlsx(., "output/cada_instalacion.xlsx")
    
    plot(ggplot(.) + geom_histogram(aes(x = demora_instalacion), binwidth = 1))
    
    ggsave(filename = "output/demora_instalaciones.pdf", last_plot())
    
    .
  } %>%
  group_by(house_id) %>%
  summarize(
    entregas = n(),
    promedio_demora = mean(demora_instalacion, na.rm = TRUE),
    max_demora = max(demora_instalacion, na.rm = TRUE)
  )




