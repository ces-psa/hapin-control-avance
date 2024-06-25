#lista de candidatos hapin
data_hapin_1_5<-listado_bebes %>% select(id, fecha_nacimiento, fec_visita_b5=`24_meses`) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent, s4_consent_c, s4_ocon_date)
) %>% mutate(
  type=if_else(grepl("^35",id), "hogar_35", "hogar_33")
) %>%  left_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date) | !is.na(a23_date)) %>% select(id, c33_date, a23_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date)
) %>% mutate(
  inicio_ventana= fec_visita_b5 - lubridate::days(28),
  fin_ventana=fec_visita_b5 + lubridate::days(28)
) %>% mutate( 
  dias_desde_fec_b5_pendientes=Sys.Date() - fec_visita_b5,
  dias_desde_fec_b5_pendientes=if_else((is.na(c33_date) | is.na(h41_date) | is.na(a23_date)) & (s4_consent!="0" | is.na(s4_date)), 
                                       as.character(dias_desde_fec_b5_pendientes), "")
) %>% mutate(
  fec_clinica=if_else(is.na(c33_date), a23_date, c33_date),
  periodo_clinica= as.Date(fec_clinica) - as.Date(fec_visita_b5),
  periodo_exposicion=case_when(
    fec_visita_b5>="2021-01-15" ~ as.Date(h41_date) - as.Date(fec_visita_b5)
  ),
  periodo_exposicion=if_else(
    fec_visita_b5<"2021-01-15", "No aplica" ,as.character(periodo_exposicion)
  )
) %>% mutate(
  clasificacion_clinica=case_when(
    periodo_clinica<=28 ~ "<=28",
    periodo_clinica > 28 & periodo_clinica<=60 ~ "29 a 60",
    periodo_clinica > 60 & periodo_clinica <=90 ~ "61 a 90",
    periodo_clinica > 90 ~ "> 90",
    s4_consent=="0" & s4_consent_c=="0" & is.na(s4_ocon_date) ~ "No aceptó",
    s4_consent=="1" & is.na(fec_clinica) ~ "consentida pendiente clinica"
  ),
  clasificacion_exposicion = case_when(
    periodo_exposicion!="No aplica" & as.numeric(periodo_exposicion) <=28 ~  "<=28",
    periodo_exposicion!="No aplica" & as.numeric(periodo_exposicion) >28 & as.numeric(periodo_exposicion)<=60 ~  "29 a 60",
    periodo_exposicion!="No aplica" & as.numeric(periodo_exposicion) >60 & as.numeric(periodo_exposicion)<=90 ~  "61 a 90",
    periodo_exposicion!="No aplica" & as.numeric(periodo_exposicion) >90  ~  "> 90",
    s4_consent=="0" & s4_consent_c=="0" & is.na(s4_ocon_date) ~ "No aceptó",
    is.na(s4_date) ~ "pendiente_consentir y pendiente exposición",
    periodo_exposicion=="No aplica" ~ "No aplicaba",
    TRUE ~ "Pendiente de Exposición"
  )
) 

data_hapin_1_5 %>% writexl::write_xlsx("output/revision.xlsx")
#filtrar las visitas para los que cumplen 2 años en septiembre 2021
dt_visitas_1_5<-data_hapin_1_5 %>% filter(as.Date(fec_visita_b5)<="2021-09-30")
dt_visitas_1_5 %>% writexl::write_xlsx("output/revision_visitas_1_5.xlsx")
#carga de visitas pendientes clinica hapin 1.5
pendientes_clinica_1_5<-dt_visitas_1_5 %>% filter(
  is.na(fec_clinica) & s4_consent!="0" & s4_consent_c!="0" | (is.na(s4_consent) & is.na(s4_ocon_date) )
) %>% mutate(
  anio=format(fec_visita_b5, "%Y"),
  mes=format(fec_visita_b5, "%B"),
  m=format(fec_visita_b5, "%m")
) %>% group_by(
  anio,
  mes,
  m) %>% count() %>% arrange(m) %>%  ungroup() %>%  select(anio, mes, visitas_pendientes=n)


#carga de visitas pednientes_exposicion hapin 1.5
pendientes_exposicion_1_5<-dt_visitas_1_5 %>% filter(
  (is.na(h41_date) & s4_consent!="0" & s4_consent_c!="0") | (is.na(h41_date)& is.na(s4_consent)) |
    (is.na(h41_date)) & !is.na(s4_ocon_date)
) %>% filter(fec_visita_b5>="2021-01-15") %>% mutate(
  anio=format(fec_visita_b5, "%Y"),
  mes=format(fec_visita_b5, "%B"),
  m=format(fec_visita_b5, "%m")
) %>%  group_by(
  anio,
  mes,
  m) %>% count() %>% arrange(m) %>%  ungroup() %>%  select(anio, mes, visitas_pendientes=n)

realizadas_clinica<-dt_visitas_1_5 %>% group_by(
  clasificacion_clinica
) %>% count()


realizadas_exposicion<-dt_visitas_1_5 %>% group_by(
  clasificacion_exposicion
) %>% count()

list(
  visitas_pendientes_clinica=pendientes_clinica_1_5,
  visitas_pendientes_exposicion=pendientes_exposicion_1_5,
  realizadas_clinica=realizadas_clinica,
  realizadas_exposicion=realizadas_exposicion
) %>% writexl::write_xlsx(paste0("output/Hapin_visits_24m_Dec_Sep_al_", Sys.Date(),".xlsx"))

