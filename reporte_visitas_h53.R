library("tidyverse")
#---------------------------------------
#VISITAS PARA H53
#---------------------------------------

gt_emory_data_arm1 %>%
  filter(!is.na(s4_main_id)) %>%
  transmute(
    screening_id = id, id = s4_main_id, edd = as.Date(m17_ga)
  ) %>%
  anti_join(
    bind_rows(
      gt_emory_data_arm2 %>%
        filter(!is.na(e3_date) | !is.na(e3_date_c)) %>%
        select(id),
      gt_emory_data_arm2 %>%
        filter(e2_title %in% c("2", "7"), e1_title == "2") %>%
        select(id)
    )
  ) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(s6_date)) %>%
      select(id, s6_arm)
  ) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(c30_date) | !is.na(c30a_date)) %>%
      transmute(
        id,
        dob = if_else(
          !is.na(c30_date),
          as.Date(c30_dob),
          c30a_date
        )
      )
  ) %>%
  mutate(
    type = if_else(
      is.na(dob),
      "según fpp",
      "ya nació"
    ),
    dob = if_else(
      !is.na(dob),
      dob,
      edd
    ),
    p2 = edd - lubridate::days(280) + lubridate::days(32 * 7),
    b2 = dob + lubridate::days(floor(6 * 365 / 12)),
    b4 = dob + lubridate::days(floor(337))
  ) %>%
  gather(visit, date, p2, b2, b4, na.rm = TRUE, factor_key = TRUE) %>%
  mutate(
    type = if_else(
      visit == "p2",
      "no ha nacido",
      type
    ),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE)
  ) %>%
  # que salgan en orden de mes
  arrange(date, desc(s6_arm)) %>%
  mutate(
    mes = paste(year, month) %>% factor(levels = unique(.))
  ) %>% 
  arrange(desc(s6_arm), visit, date) %>% filter(
    #date>='2021-05-01' & date<='2021-05-31'
  ) %>% left_join(
    comunidades %>% select("id"=id_estudio, community, community_new)
  ) %>% mutate(
    brazo=recode(s6_arm, "1"= "Intervencion", "0"="Control")
  ) %>% filter(visit=="b4") %>% anti_join(
    salidas %>% select(id)
  ) %>% 
  select(screening_id, id, type, "fecha_visita"=date, mes, "Visita_corresponde"=visit, brazo, "Comunidad_1"=community, "Comunidad_2"=community_new) %>% 
  writexl::write_xlsx(paste0("output/visitas/visitas_b4_h53_Mayo2021.xlsx"))


  # split(.$mes) %>%
  # writexl::write_xlsx("output/visitas/visitas_p2-b1_h53.xlsx")

#-------------------
#dots
#-------------------
gt_emory_data_arm2 %>%
  filter(!is.na(s6_date)) %>%
  select(id, s6_date, s6_arm, stove_install = h50_date, s6_by) %>%
  left_join(
    gt_emory_data_ %>%
      filter(!is.na(h40_date)) %>%
      group_by(id) %>%
      summarize(
        first_dot = min(h40_date),
        dot_by = h40_by[which.min(h40_date)] %>% unique()
      )
  ) %>%
  arrange(desc(s6_arm), desc(s6_date)) %>%
  print(n = Inf)

  #------------------------------------------------
  #SACAR LISTA DE REPORTES DE USO DE FUEGO DEL H54
  #------------------------------------------------
  gt_emory_repeat_data %>% filter(!is.na(h54_date)) %>% 
    select(h54_hhid,h54_date, h54_by,h54_stove_use,h54_stove_num,
           h54_stove1_loc,h54_stove1_evid,h54_stove2_loc,h54_stove2_evid) %>% filter(h54_stove_use=="1") %>% mutate(
             evidencia_estufa=recode(h54_stove_use,"0"="No", "1"="Si"),
             cantidad_estufas=h54_stove_num,
             ubicacion_estufa_1=recode(h54_stove1_loc, "1"="Interior", "2"="Aire libre"),
             evidencia_estufa_1=recode(h54_stove1_evid, "1"="La estufa esta siendo usada durante la visita", 
                                       "2"="Se observa calor, humo, brasas u otras senales de uso corriente"),
             ubicacion_estufa_2=recode(h54_stove2_loc, "1"="Interior", "2"="Aire libre"),
             evidencia_estufa_2=recode(h54_stove2_evid, "1"="La estufa esta siendo usada durante la visita", 
                                       "2"="Se observa calor, humo, brasas u otras senales de uso corriente"),
             fecha_h54=as.Date(h54_date),
             ID=h54_hhid, 
             Iniciales=h54_by
             
           ) %>% filter(fecha_h54>="2021-05-03" & fecha_h54<="2021-05-25") %>% 
    arrange(desc(ID,fecha_h54)) %>% select(
      ID, fecha_h54, Iniciales, evidencia_estufa, cantidad_estufas, ubicacion_estufa_1, evidencia_estufa_1,
      ubicacion_estufa_2, evidencia_estufa_2
    ) %>%left_join(
      gt_emory_data_arm2 %>% filter (!is.na(h50_date)) %>% select(ID=id, fecha_instalacion_estufa=h50_date)
    ) %>%  anti_join(
      salidas %>% select(ID=id)
    ) %>% 
    left_join(
      datos_participantes %>% select(ID=`ID estudio`, Nombre=`Nombre embarazada`,Comunidad_1=`Comunidad embarazada (original z10)`,
                                     Comunidad_2=`Comunidad embarazada (nueva)`,
                                     `Celular embarazada`, `Celular esposo`, `Celular_otro_miembro`)
    ) %>% left_join(
      gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(ID=id, s6_arm)
    ) %>% filter(s6_arm=="1") %>% select(-s6_arm) %>% 
    writexl::write_xlsx(paste0("output/reporte_uso_fuego_al_", Sys.Date(), ".xlsx"))
  
