library("tidyverse")
#---------------------------------------
#VISITAS PARA H53
#---------------------------------------


df_entregas_gas<-all_gas_actions %>%
  #marcar hogares con cilindros de cien libras
  mutate(
    cyl_cien=case_when(house_id=="33040" ~ "Si",
                       house_id=="33110" ~ "Si",
                       house_id=="35021" ~ "Si",
                       house_id=="35033" ~ "Si",
                       TRUE ~ "No")
  ) %>% filter(date>="2019-01-01") %>%
  select(id=house_id,cyl_id,action, date, cyl_weight) %>% arrange(house_id,date)

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
    b4 = dob + lubridate::days(floor(346))
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
  arrange(desc(s6_arm), visit, date) %>% left_join(
    comunidades %>% select("id"=id_estudio, community, community_new)
  ) %>% mutate(
    brazo=recode(s6_arm, "1"= "Intervencion", "0"="Control")
  ) %>% filter(visit=="b4") %>% anti_join(
    salidas %>% select(id)
  ) %>% 
  select(screening_id, id, type, "fecha_visita"=date, mes, "Visita_corresponde"=visit, brazo, "Comunidad_1"=community, "Comunidad_2"=community_new)  %>%
  filter(brazo=="Intervencion") %>% group_by(mes) %>% 
  count() %>% write_csv("output/salidas_intervencion.csv")

salidas_atrasadas<-read_csv("output/projeccion_salidas_intervencion.csv")
salidas_atrasadas<-salidas_atrasadas %>% mutate_all(as.character)
salidas_atrasadas %>% left_join(
  df_entregas_gas %>% filter(action=="install") %>% group_by(id) %>% mutate(fecha=last(date)) %>% filter(date==fecha) %>% 
    select(id, fecha_ultima_instalacion_gas=date)
) %>% write_csv("output/hogares_con_salida_atrasada_ultima_entrega_gas.csv")
