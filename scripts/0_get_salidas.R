#salidas que cumplen algortimo para dar de baja en el estudio
salidas<-gt_emory_data_arm2 %>% select(id,e3_date, e3_by) %>% filter(!is.na(e3_date)) %>% 
  #unir con los e3 de owa
  left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_o, e3_by_o) %>% filter(!is.na(e3_date_o))
  ) %>% #unir con los e3 de niños
  left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_c, e3_by_c) %>% filter(!is.na(e3_date_c))
  ) %>% #verificar nacimientos
  left_join(
    gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
  ) %>% #clasificar hogar con embarazada y hogar con embarazada + Adulta
  mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    )
  ) %>% #regla para validar la salida
  mutate(
    sale = case_when(
      #es hogar con adulta y ya salio la embarazada y no ha nacido el niño
      type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
      #es hogar de embarazada tiene salida y no ha nacido el niño
      type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
      #es hogar con adulta y ya nacio el niño, tiene e3 para embarazda, adulta y niño
      type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      #es hogar con embarazada ya nació el niño, tiene e3 para embarazada y niño
      type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>% filter(sale=="1") %>% select(id,sale)


salidas_detalle<-gt_emory_data_arm2 %>% select(id,e3_date, e3_by,e3_reason) %>% filter(!is.na(e3_date)) %>% 
  left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_o, e3_by_o,e3_reason_o) %>% filter(!is.na(e3_date_o))
  ) %>%left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_c, e3_by_c, e3_reason_c) %>% filter(!is.na(e3_date_c))
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
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
  ) %>% filter(sale=="1") %>% select(id,e3_date,e3_date_o,e3_date_c,sale,e3_reason, e3_reason_o, e3_reason_c)



