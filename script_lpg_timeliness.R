#------------------------------------------------------------------------------*
# Get data from Emory RedCap exports ----
#------------------------------------------------------------------------------*

# Emory RedCap export data
source(file = "scripts/lpg/0a_read_emory_data_main.R", encoding = "UTF-8")

# Intensive exposure data
source(file = "scripts/lpg/0b_get_intensive_data.R", encoding = "UTF-8")

# Repeat crfs data
source(file = "scripts/lpg/0c_get_emory_repeated.R", encoding = "UTF-8")

#------------------------------------------------------------------------------*
# Get data from Guatemala RedCap exports ----
#------------------------------------------------------------------------------*

# Repeat crfs data
source(file = "scripts/lpg/0e_get_guatemala_repeated.R", encoding = "UTF-8")




#------------------------------------------------------------------------------*
# Prepare exit dates ----
#------------------------------------------------------------------------------*


# List study exits
source(file = "scripts/lpg/0f_list_exits.R", encoding = "UTF-8")


exits <- arm2 %>%
  filter(!is.na(e3_date) | !is.na(e3_date_o) | !is.na(e3_date_c)) %>%
  select(
    id,
    # pregnant/mother woman exit
    ep = e3_date_exit,
    # other adult woman exit
    eo = e3_date_exit_o,
    # child exit date
    ec = e3_date_exit_c
  ) %>%
  print()




# Allow breaks in table cells
prepare_cells <- function(df){
  if(!knitr::is_latex_output()){
    df %>%
      set_names(
        names(.) %>%
          gsub("<br>", " ", .) %>%
          gsub("\\\\vspace[^}]+[}]", "", .)
      ) %>%
      mutate_all(
        list(
          ~ gsub("<br>", " ", .) %>%
            gsub("\\\\vspace[^}]+[}]", "", .)
        )
      )
  } else {
    df %>%
      set_names(
        names(.) %>%
          gsub("<br>", "\n", .) %>%
          gsub("%", "\\%", ., fixed = TRUE) %>%
          kableExtra::linebreak()
      ) %>%
      mutate_all(
        list(
          ~ gsub("<br>", "\n", .) %>%
            gsub("%", "\\%", ., fixed = TRUE) %>%
            kableExtra::linebreak()
        )
      )
  }
}


kable <- if(interactive()){
  function(df, ...) { print(df, n = Inf)}
} else if(!knitr::is_latex_output()) {
  knitr::kable
} else {
  function(df, caption = NULL, ...){
    df %>%  
      knitr::kable("latex", caption = caption, booktabs = T, escape = F) %>%
      kableExtra::kable_styling(latex_options = c("hold_position"), ...)
  }
}



#LPG code
all_gas_sources <- list(
  
  # New repeated CRF in Emory RedCap server
  emory_repeat = gt_emory_repeat_data %>%
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
      matches("h51_(by|date|refill|install|remove)")
    ) %>%
    mutate(
      source = "emory_current"
    ),
  
  # prototype repeated CRF in Guatemala RedCap server
  gt_repeat = gt_repeat_data %>%
    filter(!is.na(h51_date)) %>%
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
    ),
  
  
  # original gas cylinders installed
  gas_stove_install = arm2 %>%
    select(house_id = id, visit, matches("h50")) %>%
    filter(
      !is.na(h50_date)
    ) %>%
    mutate_all(as.character) %>%
    mutate(
      source = "emory-stove-install"
    ) %>%
    transmute(
      house_id,
      request_date = h50_date,
      h51_date = h50_date,
      h51_by = h50_by,
      h51_install_id1 = h50_tank_id1,
      h51_install_id2 = h50_tank_id2,
      h51_install_wt1 = h50_tank_wt1,
      h51_install_wt2 = h50_tank_wt2,
      source
    ),
  
  
  # original gas delivery from Emory data
  arm3 %>%
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
    )
) %>%
  bind_rows() %>%
  print()


all_gas_actions <- all_gas_sources %>%
  # darle id unico a cada fila
  rownames_to_column() %>%
  # bajar todas las variables que se repiten varias veces
  gather(column, value, matches("h51_(install|remove)")) %>%
  # separar la variable de la repetición
  extract(
    column, into = c("action", "variable", "rep"),
    regex = "h51_([^_]+)_([^0-9]+)([0-9]+)"
  ) %>%
  filter(!is.na(variable)) %>% 
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
  arrange(house_id, date, action) %>%
  left_join(
    arm2 %>%
      select(house_id=id, s6_date, s6_arm) %>% filter(!is.na(s6_date))
  ) %>%
  print()




# checks for wrong house ids
all_gas_actions %>%
  filter(!grepl("3[35][0-9]{3}", house_id)) %>%
  select(oid = house_id, cyl_id, ref_action = action, ref_date = date) %>%
  left_join(
    all_gas_actions %>%
      select(house_id, cyl_id, date, action)
  ) %>%
  mutate(
    date = as.Date(date),
    ref_date = as.Date(ref_date),
    diff = as.numeric(abs(date - ref_date), unit = "days")
  ) %>%
  arrange(oid, diff) %>%
  group_by(oid, cyl_id) %>%
  filter(
    (ref_action == "install" & date > ref_date) |
      (ref_action == "remove" & date < ref_date)
  ) %>%
  slice(1:2) %>%
  print()



entregas_gas <- all_gas_actions %>%
  filter(action == "install") %>%
  transmute(
    id = house_id,
    by,
    refill = as.Date(date),
    request = as.Date(request_date)
  ) %>%
  arrange(id, request) %>%
  mutate(
    delay = as.numeric(refill - request, unit = "days")
  ) %>%
  # get average time between requests
  group_by(id) %>%
  mutate(
    duration = c(NA, diff.Date(request)),
    avg_duration = median(duration, na.rm = TRUE),
    roll_duration = zoo::rollapply(
      if_else(duration == 0, avg_duration, duration),
      width = 10,
      FUN = median,
      na.pa = TRUE,  na.rm = TRUE, partial = TRUE
    )
  ) %>%
  ungroup() %>%
  # filter(!is.na(duration)) %>%
  mutate(
    rel_delay = (delay / avg_duration) * 100,
    rol_delay = (delay / roll_duration) * 100,
    group = case_when(
      rol_delay < 50 ~ "En tiempo",
      rol_delay < 90 ~ "Arriesgado",
      rol_delay >= 90 ~ "Demasiado tarde"
    ) %>%
      factor(
        levels = c("En tiempo", "Arriesgado", "Demasiado tarde")
      )
  ) %>%
  filter(request <= refill | is.na(duration)) %>%
  print()



entregas_gas %>%
  mutate(
    Periodo = "Todo el proyecto"
  ) %>%
  bind_rows(
    # last quarter 2019
    filter(
      .,
      request >= as.Date("2019-10-01"),
      request < as.Date("2020-01-01")
    ) %>%
      mutate(
        Periodo = "Último Trimestre<br>2019"
      ),
    
    # first quarter 2020
    filter(
      .,
      request >= as.Date("2020-01-01"),
      request < as.Date("2020-04-01")
    ) %>%
      mutate(
        Periodo = "Primer Trimestre<br>2020"
      ),
    
    # Last month
    filter(
      .,
      request >= Sys.Date() - lubridate::days(30),
      request < Sys.Date()
    ) %>%
      mutate(
        Periodo = paste0(
          "Último mes,<br>",
          " del ", Sys.Date() - lubridate::days(30),
          " al ", Sys.Date() - lubridate::days(1)
        )
      )
  ) %>%
  # Default order for periods as defined above
  mutate(
    Periodo = Periodo %>%
      sub("proyecto", "proyecto\\vspace{1em}", ., fixed = TRUE) %>%
      sub("$", "<br>n (%)", .) %>%
      factor(levels = unique(.)),
    tanques = round(rel_delay / 100, 1)
  ) %>%
  group_by(Periodo) %>%
  summarize(
    Entregas = n(),
    
    # 5.5 dias de entrega por semana
    "Entregas por dia\\vspace{0.5em}" = request %>%
      range(na.rm = TRUE) %>%
      diff.Date() %>%
      as.numeric(unit = "days") %>%
      magrittr::multiply_by(5.5/7) %>%
      magrittr::divide_by(Entregas, .) %>%
      round(),
    
    "Entregas a tiempo<br>(< 50% del segundo cilindro gastado)\\vspace{0.5em}" =
      sum(group == "En tiempo", na.rm = TRUE) %>%
      paste0(" (", round(./Entregas * 100, 1), ")"),
    
    "Entregas arriesgadas<br>(50% a < 90% del segundo cilindro gastado)\\vspace{0.5em}" =
      sum(group == "Arriesgado", na.rm = TRUE) %>%
      paste0(" (", round(./Entregas * 100, 1), ")"),
    
    "Entregas tarde<br>(>= 90% del segundo cilindro gastado)\\vspace{0.5em}" =
      sum(group == "Demasiado tarde", na.rm = TRUE) %>%
      paste0(" (", round(./Entregas * 100, 1), ")"),
    
    "Dias entre solicitud y entrega,<br>promedio (min-max)" = paste0(
      round(mean(delay[delay >= 0], na.rm = TRUE)), " (",
      paste(range(delay[delay >= 0], na.rm = TRUE), collapse = " - "),
      ")"
    )
  ) %>%
  rename("Entregas\\vspace{0.5em}" = Entregas) %>%
  gather(Parámetro, value, -Periodo, factor_key = TRUE) %>%
  spread(Periodo, value) %>%
  prepare_cells() %>%
  kable(caption = "Resumen de desempeño en entregas de gas")



#graphic lpg timeliness
entregas_gas %>%
  filter(!is.na(duration), duration < 31, rel_delay < 200) %>%
  mutate(
    year = lubridate::year(request)
  ) %>%
  ggplot() +
  geom_jitter(
    aes(x = request, y = rol_delay, color = group, size = group)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4), reverse = TRUE),
    size = "none"
  ) +
  labs(
    title = "Tiempos desde solicitud hasta entrega de gas",
    subtitle = paste(
      "Los tiempos de entrega son calculados para cada casa",
      "en relación a cuánto dura en promedio\nun cilindro de gas en esa casa,",
      "y cuánto se ha usado del segundo cilindro."
    ),
    color = "Temporalidad",
    x = "Fecha de solicitud",
    y = "Estimado de gas usado\ndel segundo cilindro hasta la entrega"
  ) +
  expand_limits(y = 200) +
  facet_grid(. ~ year, scales = "free_x", space = "free") +
  scale_color_manual(
    values = c("chartreuse3", "darkgoldenrod1", "red")
  ) +
  scale_size_manual(
    values = c(0.2, 0.7, 2)
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b", expand = c(0, 0)) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(50, 100, 150, 200),
    labels = c(
      "Medio\ncilindro", "1\ncilindro", "1.5\ncilindros", "2\ncilindros"
    )
  ) +
  theme(
    legend.position = c(0.23, 0.99),
    legend.justification = c(0, 1),
    panel.spacing.x = unit(0, "lines")
  )
