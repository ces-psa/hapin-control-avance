library(package = "tidyverse")
# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")
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
  read_csv(show_col_types = FALSE) %>%
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
  read_csv(show_col_types = FALSE) %>%
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

#source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")




stove_install_data <- gt_emory_data_arm2 %>%
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




original_emory_gas_data <- gt_emory_data_arm3 %>%
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
    gt_emory_data_arm2 %>% select(house_id=id, s6_date, s6_arm) %>% filter(!is.na(s6_date))
  )

