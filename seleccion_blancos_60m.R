candidatos_blancos_60m<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(330),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c, e3_reason_c)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit, e3_reason)
) %>% mutate(
  descartar=case_when(
    e3_reason=="8"  ~  "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% select(id, fecha_60m=`60_meses`) %>% anti_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>%  filter(!is.na(e2_date)) %>% select(id, e2_date, e2_participant, e2_title, redcap_event_name) %>% filter(!is.na(e2_date)) %>% filter(e2_participant=="3") %>% filter(e2_title=="7") %>% select(id)
) %>% anti_join(
  gt_hapin_II_data %>% filter(visit=="b8") %>% filter(!is.na(h41_date_v2)) %>% select(id)
)

# Crear el data frame de ejemplo
set.seed(21062024)  # Para reproducibilidad
blancos_60m <- sample(candidatos_blancos_60m$id, size = 35, replace = FALSE)
blancos_60m<-data.frame("id_blancos"=blancos_60m)

blancos_60m %>% write_csv("data/dictionaries/cat_blancos_60m.csv")

