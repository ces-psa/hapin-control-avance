library(tidyverse)
library(dplyr)

#datos generales hapin 1
# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")
# Participant information (z10)
source(file = "scripts/0_get_e3.R", encoding = "UTF-8")

dt_fetp<-read_csv("data/fetp/HAPINGuatemalaMainSt-Revisionvariables_DATA_2023-03-21_1249.csv")
dt_z10_completo<-read_csv("data/exports/z10_edad_madre.csv")

dt_c35<-read_csv("data/exports/HAPINGuatemalaMainSt-C35_DATA_2023-02-23_1624.csv")
dt_c35<-dt_c35 %>% mutate(id=as.character(id))
dt_c35 %>% group_by(redcap_event_name) %>% count()
dt_c35 %>% filter(!is.na(c35_date)) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_date) %>% count()
dt_c35 %>% filter(!is.na(c35_cry)) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_cry) %>% count()


dt_c35 %>% filter(!is.na(c35_date) & is.na(c35_cry) ) %>% filter(redcap_event_name=="b4_arm_2") %>%  select(id, c35_date) %>% group_by(
  lubridate::year(c35_date), lubridate::month(c35_date)
) %>% count()


datos_varinia<-dt_c35 %>% filter(redcap_event_name=="b4_arm_2") %>%  select(
  id, fecha=c35_date,
  c35_book,
  c35_home_toys,
  c35_shop_toys,
  c35_objects,
  c35_alone,
  c35_child,
  c35_read,
  c35_read_who,
  c35_story,
  c35_story_who,
  c35_song,
  c35_song_who,
  c35_outside,
  c35_outside_who,
  c35_play,
  c35_play_who,
  c35_draw,
  c35_draw_who,
  c35_unexpect,
  c35_no_control,
  c35_nervous,
  c35_confident,
  c35_your_way,
  c35_cope,
  c35_control_irritation,
  c35_top,
  c35_anger,
  c35_difficulty,
  c35_grasp,
  c35_hands,
  c35_recognize,
  c35_fist,
  c35_interest,
  c35_roll,
  c35_mouth,
  c35_affection,
  c35_pickup,
  c35_object,
  c35_feet,
  c35_sound,
  c35_hold,
  c35_tap,
  c35_sitting,
  c35_change,
  c35_point,
  c35_name,
  c35_respond,
  c35_crawl,
  c35_eat,
  c35_transfer,
  c35_clap
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(
    id, c30_sex, c30_dob
  )
) %>% filter(!is.na(c30_dob)) %>% filter(!is.na(c35_alone))

#integrar segundo pedido de variables de Varinia
datos_varinia2<-datos_varinia %>% left_join(
  dt_fetp %>% filter(!is.na(m10_date))%>% filter(redcap_event_name=="linea_de_base_arm_2")  %>% select(id,
  fecha_m10=m10_date,  m10_educ, m10_educ_year,  m10_educ_dad, m10_dad_year, m10_sleep, 	m10_floor___1,
  m10_floor___2,	m10_floor___3, 	m10_floor___4,	m10_floor___5,	m10_floor___6,	m10_floor___7,	m10_floor___8,
  m10_floor___9,	m10_floor___10,	m10_floor___11,	m10_floor___12,	m10_floor___13,	m10_floor___14,	m10_floor___555,
  m10_floor___888, m10_elect
  
  )
) %>% left_join(
  dt_fetp %>% filter(!is.na(h56_date)) %>% select(id, h56_occup___1, h56_occup___2, h56_occup___3,
                                                  h56_occup___4, h56_occup___5, h56_occup___6, h56_occup___7,
                                                  h56_occup___8, h56_occup___9, h56_occup___10, h56_occup___11, h56_occup___12,
                                                  h56_occup___13, h56_occup___14, h56_occup___15, h56_occup___16, h56_occup___555,
                                                  h56_ocupp_other)
)

datos_varinia2 %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_date)) %>% select(id_tamizaje=id, id=s4_main_id) %>% mutate_all(as.character)
) %>% left_join(
 #dt_z10_completo %>% select(id_tamizaje=record_id, fecha_dob_madre=z10_dob)
  dt_z10_completo %>% mutate(
   id_tamizaje=as.character(record_id),
   anio_nac_madre=case_when(
     substr(z10_dob,2,2)=="-" ~ substr(z10_dob,6,10),
     substr(z10_dob,5,5)=="-" ~ substr(z10_dob,1,4), 
     substr(z10_dob,3,3)=="-" ~ substr(z10_dob,7,11)
   )
 ) %>% select(id_tamizaje, anio_nac_madre)

 
) %>% 
  mutate(
  anio_m10=lubridate::year(fecha_m10),
  edad_nac_m10=as.numeric(anio_m10) - as.numeric(anio_nac_madre)
  #edad_madre= lubridate::year(as.Date(fecha_m10)) - anio_nac_madre
    #round(( as.Date(fecha_m10)- as.Date(fecha_dob_madre) ) /365,0)
) %>% 
  select(
  id, fecha_m10, 
  m10_educ, m10_educ_year,  m10_educ_dad, m10_dad_year, m10_sleep, 	m10_floor___1,
  m10_floor___2,	m10_floor___3, 	m10_floor___4,	m10_floor___5,	m10_floor___6,	m10_floor___7,	m10_floor___8,
  m10_floor___9,	m10_floor___10,	m10_floor___11,	m10_floor___12,	m10_floor___13,	m10_floor___14,	m10_floor___555,
  m10_floor___888, m10_elect,
  h56_occup___1, h56_occup___2, h56_occup___3,
  h56_occup___4, h56_occup___5, h56_occup___6, h56_occup___7,
  h56_occup___8, h56_occup___9, h56_occup___10, h56_occup___11, h56_occup___12,
  h56_occup___13, h56_occup___14, h56_occup___15, h56_occup___16, h56_occup___555,
  h56_ocupp_other, anio_nac_madre, edad_nac_m10
) %>% write_csv("output/datos_varinia_b4_update.csv")

datos_varinia %>% write_csv("output/datos_varinia_b4.csv")
datos_varinia %>% writexl::write_xlsx("output/datos_varinia_b4.xlsx")
write_rds(datos_varinia, file = "output/datos_varinia_b4.RDS")
