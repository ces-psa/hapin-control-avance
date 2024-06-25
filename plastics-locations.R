#source(file = "scripts/scraps/new_emory_data.R", encoding = "UTF-8")
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")


routes <- readxl::read_excel(path = "data/logs/2019-05-16_communities.xlsx")


# hh demographics
all_women <- gt_emory_data_arm2 %>% 
  filter(!is.na(m10_date)) %>%
  #, grepl("^G", screening_id)
  select(
    id, visit, dem_date = m10_date, hh_size = m10_sleep,
    matches("m10_(relation|gender|age)")
  ) %>%
  gather(variable, value, matches("m10_"), na.rm = TRUE) %>%
  mutate(
    variable = if_else(
      condition = !grepl("_[0-9]+$", variable),
      true = paste0(variable, "_1"),
      false = variable
    )
  ) %>%
  separate(variable, into = c(NA, "variable", "copy")) %>%
  spread(variable, value, convert = TRUE) %>%
  filter(gender == 2) %>%
  # adjust age since demographics collected at baseline
  mutate(
    months_since = as.numeric(Sys.Date() - dem_date) / (365.25 / 12),
    pos_age = ceiling(age + months_since / 12),
    relation = recode(
      relation,
      "1" = "pareja",
      "2" = "hija",
      "3" = "nuera",
      "4" = "nieta",
      "5" = "madre",
      "6" = "suegra",
      "7" = "hermana",
      "8" = "otra pariente",
      "9" = "hija adoptiva / hijastra",
      "10" = "no esta relacionada"
    )
  ) %>%
  select(-copy) %>%
  # add communities
  left_join(
    gt_emory_data_arm1 %>%
      filter(!is.na(s4_main_id)) %>%
      select(screening_id = id, id = s4_main_id, community = s1_community_name)
  ) %>%
  # add route
  left_join(
    routes %>%
      separate_rows(ids) %>%
      select(id = ids, correct_name, route_group)
  ) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(s6_date)) %>%
      select(id, arm = s6_arm)
  ) %>%
  arrange(route_group) %>%
  print()


all_women %>%
  filter(
    !is.na(arm),
    between(pos_age, 13, 18)
  ) %>%
  {list(
    resumen = group_by(., route_group) %>%
      summarize(
        n = n(),
        control = sum(arm == 0),
        intervention = sum(arm == 1),
        communities = community %>% unique() %>% paste(collapse = ",")
      ),
    detalle = .
  )} %>%
  writexl::write_xlsx(path = "output/temp/2019-09-04_plastics.xlsx")

resumen_detalle<-all_women %>%
  filter(
    !is.na(arm),
    between(pos_age, 13, 18)
  ) %>%
  {list(
    resumen = group_by(., route_group) %>%
      summarize(
        n = n(),
        control = sum(arm == 0),
        intervention = sum(arm == 1),
        communities = community %>% unique() %>% paste(collapse = ",")
      ),
    detalle = .
  )} 

resumen_detalle %>% writexl::write_xlsx("output/temp/resumen_detalle.xlsx")
  
tbl_plastic<- all_women %>% filter(between(pos_age, 13, 18)) %>%  group_by(id, screening_id) %>% summarize(cantidad_adolecentes=n()) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select("screening_id"=id, "fpp"=m17_ga)
) %>%  mutate(
  fga= as.Date(fpp) - lubridate::days(280),
  dias =  lubridate::floor_date(Sys.Date(), unit = "weeks", week_start = 1) - fga ,
  edad_semanas= as.numeric(dias) %/% 7,
  edad_dias = as.numeric(dias)%% 7,
  fecha_P1=fga + lubridate::days(168),
  fecha_limite_P1=fga+lubridate::days(196),
  fecha_P2=fga + lubridate::days(224),
  fecha_limite_P2=fga+lubridate::days(252)
  #flag_visita_P1=if_else(edad_semanas>=24 & edad_semanas<=28,"P1",NA_character_),
  #flag_visita_P2=if_else(edad_semanas>=22 & edad_semanas<=36,"P2",NA_character_),
  #edad_concat=paste0(edad_semanas," semanas con ",edad_dias," dias" )
) %>% left_join(
  salidas
) %>% mutate(tiene_salida=if_else(sale=="1", "Si", NA_character_)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% 
  mutate(
  nacio=if_else(!is.na(c30_dob), "Si", "No"),
  fecha_nacimiento=if_else(!is.na(c30_dob), as.character(c30_dob), as.character(fpp)),
  fecha_M1=as.Date(fecha_nacimiento) + lubridate::days(30),
  fecha_B1=as.Date(fecha_nacimiento) + lubridate::days(90),
  fecha_B2=as.Date(fecha_nacimiento) + lubridate::days(180),
  fecha_B3=as.Date(fecha_nacimiento) + lubridate::days(270),
  fecha_B4=as.Date(fecha_nacimiento) + lubridate::days(360),
) %>%left_join(
  gt_emory_data_arm2 %>% filter(visit=="p1" & !is.na(h41_date)) %>% select(id, h41_date)
) %>% mutate(
  p1=if_else(
    !is.na(h41_date), "realizado", "pendiente"
  )
) %>% select(-h41_date) %>%  left_join(
  gt_emory_data_arm2 %>% filter(visit=="p2" & !is.na(h41_date)) %>% select(id,h41_date)
) %>% mutate(
  p2=if_else(
    !is.na(h41_date), "realizado", "pendiente"
  )
) %>% select(-h41_date) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="m1") %>% select(id,c31_date)
) %>% mutate(
  m1=if_else(
    !is.na(c31_date), "realizado", "pendiente"
    )
  ) %>% select(-c31_date) %>% left_join(
      gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h41_date)) %>%  select(id, h41_date)
  ) %>% mutate(
    b1=if_else(
      !is.na(h41_date),"realizado", "pendiente"
    )
  ) %>% select(-h41_date) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h41_date)) %>% select(id, h41_date)
    ) %>% mutate(
      b2=if_else(
        !is.na(h41_date),"realizado", "pendiente"
      )
    ) %>% select(-h41_date) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(c31_date)) %>% select(id, c31_date)
    ) %>% mutate(
      b3=if_else(
        !is.na(c31_date),"realizado", "pendiente"
      )
    ) %>% select(-c31_date) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h41_date)) %>% select(id, h41_date)
    ) %>% mutate(
      b4=if_else(
        !is.na(h41_date),"realizado", "pendiente"
      )
    ) %>% select(-h41_date) %>% 
    select(-dias, -edad_semanas, -edad_dias, screening_id, -sale, -c30_dob)#%>% writexl::write_xlsx("output/fechas_lisa.xlsx")

visitas<-tbl_plastic %>% mutate(fecha_P1=as.character(fecha_P1),
                       fecha_P2=as.character(fecha_P2),
                       fecha_M1=as.character(fecha_M1),
                       fecha_B1=as.character(fecha_B1),
                       fecha_B2=as.character(fecha_B2),
                       fecha_B3=as.character(fecha_B3),
                       fecha_B4=as.character(fecha_B4),
                       fecha_nacimiento=as.character(fecha_B4)
                       ) %>% 
  gather(key=variable, value = value, -id, -screening_id, -cantidad_adolecentes, -nacio, -fecha_nacimiento, -tiene_salida, -p1, -p2, -m1, -b1, -b2,-b3, -b4) %>% mutate(
  fecha_visita=case_when(
    variable=="fecha_P1" ~ value,
    variable=="fecha_P2" ~ value,
    variable=="fecha_M1" ~ value,
    variable=="fecha_B1" ~ value,
    variable=="fecha_B2" ~ value,
    variable=="fecha_B3" ~ value,
    variable=="fecha_B4" ~ value,
    TRUE ~ NA_character_
  ),
  tipo_visita= case_when(
    variable=="fecha_P1" ~ "p1",
    variable=="fecha_P2" ~ "p2",
    variable=="fecha_M1" ~ "m1",
    variable=="fecha_B1" ~ "b1",
    variable=="fecha_B2" ~ "b2",
    variable=="fecha_B3" ~ "b3",
    variable=="fecha_B4" ~ "b4",
  )
) %>% filter(!is.na(fecha_visita)) %>% select(screening_id, id, tiene_salida, "mujeres_candidatas"=cantidad_adolecentes, "Nacido"=nacio, fecha_nacimiento, fecha_visita, tipo_visita,p1, p2,m1,b2,b3,b4) %>% gather(
  key = variable, value = value, -screening_id, -id, -tiene_salida, -mujeres_candidatas, -Nacido, -fecha_nacimiento, -fecha_visita, -tipo_visita
) %>% mutate(
  se_hizo_visita=case_when(
    tipo_visita=="p1" & variable=="p1" ~ value,
    tipo_visita=="p2" & variable=="p2" ~ value,
    tipo_visita=="m1" & variable=="m1" ~ value,
    tipo_visita=="b1" & variable=="b1" ~ value,
    tipo_visita=="b2" & variable=="b2" ~ value,
    tipo_visita=="b3" & variable=="b3" ~ value,
    tipo_visita=="b4" & variable=="b4" ~ value,
    TRUE ~ NA_character_
  )
) %>% 
  filter(!is.na(se_hizo_visita)) %>% filter(se_hizo_visita=="pendiente") %>% left_join(
    comunidades %>% select("id"=id_estudio, "comunidad_1"=community, "comunidad_2"= community_new)
  ) %>% 
  select(screening_id,  
         id, tiene_salida,    "cantidad_candidatas_en_hogar"=mujeres_candidatas,  "Bebe nacido"=Nacido,
           "Fecha de nacimiento"=fecha_nacimiento, fecha_visita,"visita_pendiente"=tipo_visita, comunidad_1, comunidad_2) #%>% writexl::write_xlsx("output/listado_lisa.xlsx")

visitas %>% writexl::write_xlsx("output/visitas.xlsx")
