



dt1<-gt_emory_data_arm1 %>% filter(!is.na(s4_date)) %>% select("ID_tamizaje"=id, s4_date, s4_by, s4_consent, s4_reason) %>% filter(s4_consent=="0") %>% 
  select(ID_tamizaje, "fecha_s4"=s4_date,"Iniciales"=s4_by, "Razon_rechazo"=s4_reason) %>% writexl::write_xlsx("output/lista_motivo_rechazos_s4.xlsx")


gt_emory_data_arm1%>%
  filter(!is.na(s4_date), s4_consent=="0") %>% 
         #!is.na(s4_reason)) %>%
  #count(reason = s4_reason) %>%
  mutate(reason = s4_reason,
    class = case_when(
      grepl("requisi|criter|condici|horno|otro", reason, ignore.case = TRUE) ~ "No cumple",
      grepl("no.+(qui|disp|consent|acepta|acepto)", reason, ignore.case = TRUE) ~ "No quiere",
      grepl("esposo", reason, ignore.case = TRUE) ~ "No la deja el esposo",
      grepl("interesa", reason, ignore.case = TRUE) ~ "No interesa",
      grepl("no.+dispuesta|dejar", reason, ignore.case = TRUE) ~ "No cambia estufa de leña",
      grepl("estufa.+(gas|cilindr|electric)", reason, ignore.case = TRUE) ~ "Tiene estufa gas",
      grepl("otra.+proyecto", reason, ignore.case = TRUE) ~ "Casa en hapin",
      grepl("migra|muda|fuera", reason, ignore.case = TRUE) ~ "Por migracion",
      grepl("no.+permiten|apoya", reason, ignore.case = TRUE) ~ "No le permiten",
      grepl("tiempo.+pensarlo", reason, ignore.case = TRUE) ~ "No dio respuesta",
      grepl("trabaja.+fuera|gas|tiempo|sabado", reason, ignore.case = TRUE) ~ "Por el trabajo"
    )
  ) %>%
  arrange(class) %>%group_by(class) %>% summarize(contador=n()) %>% select("Razon"=class, "Cantidad_rechazos"=contador) %>% 
  print(n = Inf) %>% writexl::write_xlsx("output/rechazos_agrupados.xlsx")

