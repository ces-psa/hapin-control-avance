#source(file = "scripts/scraps/new_emory_data.R", encoding = "UTF-8")
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")


ga_weeks <- function(ga){
  w <- as.numeric(sub("[^0-9]+([0-9]+)?", "", ga))
  d <- as.numeric(sub("[0-9]+([^0-9]+)?", "", ga)) %>%
    if_else(is.na(.), 0, .)
  w + (d / 7)
}


datos_m17 <- gt_emory_data_arm1 %>%
  filter(!is.na(m17_date), !is.na(s4_date)) %>%
  mutate(screening_id=id) %>% 
  # select(
  #   screening_id, id, m17_date, m17_hr_bpm,
  #   m17_lmp, m17_lmp_date,
  #   m17_lmp_ga, m17_ultra_ga,
  #   m17_ga_method, m17_ga
  # ) %>%
  mutate_at(
    vars(matches("_crl[12]")),
    list(~gsub("^8+$", "", .))
  ) %>%
  mutate_at(
    vars(matches("_(crl|hc|fl)[12]")),
    list(as.numeric)
  ) %>%
  mutate(
    # manual fixes
    m17_fl2 = case_when(
      screening_id == "G1383" ~ 2.89,
      TRUE ~ m17_fl2
    )
  ) %>%
  transmute(
    # variables básicas para información de edad gestacional
    screening_id, id, m17_by, m17_date,
    m17_hr, m17_hr_bpm,
    m17_lmp, m17_lmp_date,
    m17_lmp_ga, m17_ultra_ga,
    m17_ga_method, m17_ga,
    # edad gestacional según el método elegido.
    # Esto supuestamente está en formato semanas.dias
    ga = recode(
      m17_ga_method,
      `1` = m17_lmp_ga,
      `2` = m17_ultra_ga
    ),
    m17_ga_method = recode(
      m17_ga_method,
      `1` = "fur",
      `2` = "ultrasonido"
    ),
    # convertir semanas.dias en dias
    gad = ga %>%
      if_else(
        condition = grepl("[^0-9]", .),
        true = .,
        false = NA_character_
      ) %>%
      gsub("[0-9]+[^0-9]([0-9])+", "\\1", .) %>%
      as.numeric() %>%
      if_else(
        condition = is.na(.),
        true = 0,
        false = .
      ),
    gaw = ga %>%
      gsub("(^[0-9]+)([^0-9][0-9]+)?", "\\1", .) %>%
      as.numeric() %>%
      magrittr::add(gad/7),
    gaw_ultra = ga_weeks(m17_ultra_ga),
    fpp_calc = m17_date + (280 - (gaw*7)),
    diferencia_escritas = as.numeric(
      fpp_calc - m17_ga,
      units = "days"
    ),
    
    #--------------------------------------------------------------------------*
    # calculadas según se define en el proyecto RedCap UVG
    # Test gestational age calculations". Ese a su vez lo diseñé según las
    # instrucciones de M17 y los cálculos de la hoja de excel de intergrowth
    #--------------------------------------------------------------------------*
    
    # medias de las medidas del feto
    crl = map2_dbl(m17_crl1, m17_crl2, ~ mean(c(.x, .y), na.rm = TRUE)),
    hc = map2_dbl(m17_hc1, m17_hc2, ~ mean(c(.x, .y), na.rm = TRUE)),
    fl = map2_dbl(m17_fl1, m17_fl2, ~ mean(c(.x, .y), na.rm = TRUE)),
    
    ga_lmp = as.numeric(m17_date - m17_lmp_date, unit = "days") / 7,
    ga_obus = case_when(
      !is.na(crl) & crl < 8.5 ~ (
        40.9041 + ( 3.21585 * sqrt(crl * 10) + (0.348956 * crl * 10))
      ) / 7,
      !is.na(fl) & !is.na(hc) ~ (
        (exp(1)) ^ (
          (0.03243 * ((log( hc * 10) ) ^ (2) )) +
            ((0.001644 * (fl * 10) * log( hc * 10 )) + 3.813))
      ) / 7
    ),
    
    # fechas probables de parto calculadas
    fpp_lmp = m17_date - round(ga_lmp * 7) + 280,
    fpp_obus = m17_date - round(ga_obus * 7) + 280,
    
    # método esperado según SOP de CIC
    ga_method = case_when(
      m17_lmp == 0 ~ "ultrasonido",
      crl < 8.5 | ga_obus < 16 ~ if_else(
        condition = ( abs(ga_obus - ga_lmp ) * 7 ) < 7,
        true = "fur",
        false = "ultrasonido"
      ),
      ga_obus >= 16 ~ if_else(
        condition = ( abs(ga_obus - ga_lmp ) * 7 ) < 10,
        true = "fur",
        false = "ultrasonido"
      )
    ),
    
    ga_esperada = case_when(
      m17_lmp == 0 ~ ga_obus,
      crl < 8.5 | ga_obus < 16 ~ if_else(
        condition = ( abs(ga_obus - ga_lmp) * 7 ) < 7,
        true = ga_lmp,
        false = ga_obus
      ),
      ga_obus >= 16 ~ if_else(
        condition = ( abs(ga_obus - ga_lmp) * 7 ) < 10,
        true = ga_lmp,
        false = ga_obus
      )
    ),
    fpp_esperada = m17_date - round(ga_esperada * 7) + 280,
    diferencia_real = abs(as.numeric(m17_ga - fpp_esperada, unit = "days"))
  )


datos_m17 %>%
  arrange(!is.na(id), !is.na(crl), !is.na(m17_lmp_date)) %>%
  select(
    screening_id, id, crl, hc, fl,
    m17_ga_method, ga_method, ga_obus, m17_ultra_ga, ga_lmp, m17_lmp_ga
  ) %>%
  print(n = Inf)



repeated <- datos_m17 %>%
  filter(
    is.na(crl) | crl < 100,
    is.na(fl) | fl < 5,
    is.na(hc) | hc < 100
  ) %>%
  select(screening_id) %>%
  left_join(
    gt_emory_data_arm1 %>% select(screening_id=id, matches("_(crl|fl|hc)[12]"))
  ) %>%
  mutate_at(
    vars(matches("_crl[12]")),
    list(~gsub("^8+$", "", .))
  ) %>%
  mutate_at(
    vars(matches("_(crl|hc|fl)[12]")),
    list(as.numeric)
  ) %>%
  mutate(
    # manual fixes
    m17_fl2 = case_when(
      screening_id == "G1383" ~ 2.89,
      TRUE ~ m17_fl2
    )
  ) %>%
  gather(var, value, matches("m17"), na.rm = TRUE) %>%
  extract(var, into = c("var", "cor"), regex = "m17_(crl|fl|hc)([12])") %>%
  mutate(
    cor = recode(cor, "1" = "first", "2" = "second")
  ) %>%
  spread(cor, value) %>%
  mutate(
    d = abs(first - second),
    mag = d / ((first + second) / 2)
  ) %>%
  arrange(screening_id, var) %>%
  print()



repeated %>%
  filter(!is.na(mag)) %>%
  ggplot() +
  geom_point(
    aes(x = first, y = second, color = mag > 0.2, size = mag > 0.2)
  ) +
  facet_wrap(~ var, scales = "free") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_size_manual(values = c(0.6, 1))







errores_m17 <- datos_m17 %>%
  left_join(
    repeated %>%
      group_by(screening_id) %>%
      filter( mag > 0.2) %>%
      summarize(check = paste(var, collapse = ","))
  ) %>%
  mutate(
    # list problems
    problemas = data_frame(
      a = if_else(m17_ga != fpp_esperada, "fpp equivocada", NA_character_),
      b = if_else(m17_date < m17_lmp_date, "fecha de m17 < fur", NA_character_),
      c = if_else(m17_lmp == 1 & is.na(m17_lmp_date), "falta fur", NA_character_),
      d = if_else(abs(gaw_ultra - ga_obus) * 7 > 3, "fpp por ultrasonido equivocada", NA_character_),
      e = if_else(diferencia_real > 1, "m17_ga diferente a la esperada", NA_character_),
      f = if_else(m17_ga_method != ga_method, "método seleccionado != método esperado", NA_character_),
      g = if_else(as.numeric(m17_lmp_ga) > 40, "edad fur no válida", NA_character_),
      h = if_else(as.numeric(m17_ultra_ga) > 40, "edad ultra no válida", NA_character_),
      i = if_else(!is.na(check), paste("diferencia muy grande entre primera y segunda medida de", check), NA_character_),
      z = if_else(m17_hr == 1 & !between(m17_hr_bpm, 100, 300), "revisar frecuencia cardiaca", NA_character_)
    ) %>%
      apply(1, function(x) na.omit(x) %>% paste(., collapse = ";\n")),
    
    # prioridades
    prioridad = data_frame(
      a = if_else(m17_ga != fpp_esperada, 1, 0),
      b = if_else(m17_date < m17_lmp_date, 5, 0),
      c = if_else(m17_lmp == 1 & is.na(m17_lmp_date), 5, 0),
      d = if_else(abs(gaw_ultra - ga_obus) * 7 > 3, 1, 0),
      e = if_else(diferencia_real > 1, 2, 0),
      f = if_else(m17_ga_method != ga_method, 2, 0),
      g = if_else(as.numeric(m17_lmp_ga) > 40, 5, 0),
      h = if_else(as.numeric(m17_ultra_ga) > 40, 5, 0),
      i = if_else(!is.na(check), 1, 0),
      z = if_else(m17_hr == 1 & !between(m17_hr_bpm, 100, 300), 5, 0)
    ) %>%
      apply(1, sum, na.rm = TRUE)
  ) %>%
  arrange(
    desc(prioridad),
    diferencia_escritas > 0 | diferencia_escritas < 5,
    desc(abs(diferencia_real)),
    diferencia_real > 0 | diferencia_real < 5,
    desc(abs(diferencia_escritas))
  )


errores_m17 %>%
  filter(prioridad > 0) %>%
  writexl::write_xlsx(path = paste0("output/errors/m17_", Sys.Date(), ".xlsx"))
