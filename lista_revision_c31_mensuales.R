# salidas simplificadas
exits <- gt_emory_data_arm2 %>%
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

# con los que ya nacieron
lista_c31<-gt_emory_data_arm2 %>%
  filter(!is.na(c30_dob)) %>%
  transmute(
    id,
    dob = c30_dob,
    # edad al dia de hoy
    current_age = as.numeric(Sys.Date() - as.Date(dob), unit = "days") / (365.25/12)
  ) %>%
  # agregar el registro c31 más reciente
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(c31_date)) %>%
      select(id, visit, last_c31 = c31_date) %>%
      group_by(id) %>%
      slice(which.max(last_c31))
  ) %>%
  mutate(
    # la fecha de la "siguiente visita esperada" asumiendo que son mensuales
    next_c31_visit = current_age %>%
      # con esto conseguimos la siguiente vez que cumplirá mes exacto
      ceiling() %>%
      if_else(
        # si ya se pasaron de 12, calculamos como si tuviera 12 para que le toque b4
        condition = . > 12,
        true = 12,
        false = .
      ) %>%
      factor(
        levels = 1:12,
        # según el mes lo etiquetamos con la visita correspondiente
        labels = c(
          "m1", "m2", "b1", "m4", "m5",
          "b2", "m7", "m8", "b3", "m10",
          "m11", "b4"
        )
      ),
    # fecha de la siguiente vez que cumple mes exacto
    next_c31_date = as.Date(dob) + (floor(ceiling(current_age) * (365.25/12)))
  ) %>%
  arrange(next_c31_date) %>%
  left_join(exits) %>%
  mutate(
    # si es positivo, dias que se ha pasado de la visita
    # si es negativo, dias que le faltan para la visita
    days_since_expected = as.numeric(Sys.Date() - as.Date(next_c31_date), unit = "days"),
    
    # etiquetar si ya no toca hacerlo (porque ya completó el estudio o salió)
    next_c31_date = case_when(
      # mark completed evaluations
      visit == "b4" ~ "ya completó evaluación",
      !is.na(ec) & ec < next_c31_date ~ "salió del estudio antes de visita",
      !is.na(ec) ~ "se perdió la visita, salida registrada después",
      TRUE ~ as.character(next_c31_date)
    ),
    days_since_expected = if_else(
      !grepl("[0-9]", next_c31_date),
      "done",
      as.character(days_since_expected)
    ),
    
    # meses desde la visita c31 anterior
    months_since_last = case_when(
      !grepl("[0-9]", next_c31_date) ~ "--",
      is.na(last_c31) ~ (Sys.Date() - as.Date(dob)) %>%
        as.numeric(unit = "days") %>%
        magrittr::divide_by(365.25/12) %>%
        round(1) %>%
        as.character(),
      !is.na(last_c31) ~ (Sys.Date() - as.Date(last_c31)) %>%
        as.numeric(unit = "days") %>%
        magrittr::divide_by(365.25/12) %>%
        round(1) %>%
        as.character()
    )
  ) %>%
  arrange(next_c31_date) %>%
  select(-ep, -eo, -ec) %>%
  print(n = Inf) 

#Listado para Jhon
cat_rutas<-read_csv("data/cat_rutas_visitas.csv")
lista_c31 %>% left_join(
  comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
) %>% left_join(
  
)
  writexl::write_xlsx("output/lista_c31_mes.xlsx")

lista_c31 %>% filter(months_since_last>"3.5") %>% filter(as.character(visit)!= as.character(next_c31_visit)) %>% 
  transmute(
    id, fecha_nacimiento=dob, edad_actual=current_age,
    ultima_visita_realizada=visit,
    fecha_ultima_visita=last_c31,
    meses_desde_ultima_visita=months_since_last
  ) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,`ID tamizaje`, `Nombre embarazada`, `Comunidad embarazada (nueva)`, `Comunidad embarazada (original z10)`,
                                   `Celular embarazada`, `Celular esposo`, Celular_otro_miembro)
  ) %>% writexl::write_xlsx("output/revision_c31_mayor_3_5_meses.xlsx")

lista_c31 %>% mutate(next_date_c31=last_c31 + lubridate::days(30)) %>% arrange(next_date_c31) %>% filter(meses_desde_ultimo_c31=nex_date_c31)
