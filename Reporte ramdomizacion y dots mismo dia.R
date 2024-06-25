#Conteo de salidas del estudio
salidas<-gt_emory_data %>% select(id,e3_date) %>% filter(!is.na(e3_date)) %>%
  left_join(
    gt_emory_data %>% select(id,e3_date_o) %>% filter(!is.na(e3_date_o))
  ) %>%left_join(
    gt_emory_data %>% select(id,e3_date_c) %>% filter(!is.na(e3_date_c))
  ) %>% left_join(
    gt_emory_data %>% select(id, c30_date) %>% filter(!is.na(c30_date))
  ) %>%mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    )
  ) %>% mutate(
    sale = case_when(
      type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
      type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
      type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%filter(sale=="1")

gt_emory_data %>%
  filter(!is.na(s6_date)) %>%
  select(id, s6_date, s6_arm, stove_install = h50_date, s6_by) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(h40_date)) %>%
      group_by(id) %>%
      summarize(
        first_dot = min(h40_date),
        dot_by = h40_by[which.min(h40_date)] %>% unique()
      )
  ) %>% left_join(
    salidas %>% select(id,sale) %>% mutate(tiene_salida=if_else(
      sale=="1","Si","No"
    ))
  ) %>% 
  mutate(
    coincide=if_else(as.Date(s6_date)==as.Date(first_dot) & s6_by==dot_by,"Si","No")
  ) %>% 
  arrange(desc(s6_arm), desc(s6_date)) %>% mutate(Brazo=recode(
    s6_arm, "1"="Intervencion", "0"="Control"
  )) %>% 
  select("ID"=id,"Fecha_ramdomizacion"=s6_date,Brazo,"Iniciales S6"=s6_by, "Fecha_instalacion_Dot"=first_dot, "Ramdomizacion y Dots mismo dia"=coincide, "Fecha_Instalacion_Estufa"=stove_install) %>% 
  print() %>% write_csv("output/reporte_dots.csv")
