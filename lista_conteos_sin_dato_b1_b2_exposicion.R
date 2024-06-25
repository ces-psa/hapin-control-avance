#-----------------------------------------
#
#REVISION DE DATOS FALTANTES EXPOSICIÓN
#
#------------------------------------------
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(type=if_else(grepl("^35[0-9]", id), "owa", "madre")) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)),
  edad_dias= Sys.Date() - fecha_nacimiento,
  edad_meses= edad_dias / 30.25
) %>% select(id_tamizaje, id, type, fecha_nacimiento, edad_meses, e3_date, e3_date_o) %>% mutate(
  participando=case_when(
    type=="owa" & !is.na(e3_date_o) ~ "solo madre",
    type=="owa" & !is.na(e3_date) ~ "solo owa",
    type=="owa" & is.na(e3_date) & is.na(e3_date_o) ~ "madre y owa",
    type=="madre" ~ "solo madre"
  )
) %>% select(-e3_date, -e3_date_o) %>% filter(as.numeric(edad_meses)>=6) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h41_date)) %>% select(id, visit, fecha_h41=h41_date)
) %>% filter(is.na(visit)) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h41_date)) %>% select(id, visita=visit, fecha_h41_b2=h41_date)
) %>% filter(is.na(visita)) %>% write_csv("output/mayores_6_meses_sin_B1_y_B2_exposicion.csv")

