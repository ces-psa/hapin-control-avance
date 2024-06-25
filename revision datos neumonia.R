# FIRST LOAD DATA

library(package = "tidyverse")


# find most recent files
#amr1 file
arm1_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm1_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm1_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

#arm2 file
main_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm2_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm2_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

# find most recent files
arm3_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaMainSt-Arm3_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaMainSt-Arm3_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

#repeat_file
repeats_file <- list.files(
  path = "data/exports",
  pattern = "HAPINGuatemalaRepeat_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINGuatemalaRepeat_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()

#piloto_file
piloto_file <- list.files(
  path = "data/exports",
  pattern = "PilotoVigilanciaNeum_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+PilotoVigilanciaNeum_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()


# Manually select data export
arm1 <- read_csv(
  file = arm1_file$file,
  col_types = cols(.default = col_character())
) %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$"), m17_ga, c30_dob),
    list(as.Date)
  ) %>%
  filter(id != "99999") %>%
  mutate(
    visit = redcap_event_name %>%
      sub("_arm.*", "", .) %>%
      recode(
        .,
        linea_de_base = "baseline",
        .default = .
      ) %>%
      factor(
        levels = c("baseline", "p1", "p2",  "b1", "b2", "b3", "b4")
      ),
    # manual fix for overwriten s4
    s4_main_id = if_else(
      condition = id == "G1488",
      true = "33488",
      false = s4_main_id
    )
  ) %>%
  print()

arm2 <- read_csv(
  file = main_file$file,
  col_types = cols(.default = col_character())
) %>%
  filter(id != "99999") %>%
  # fix date types
  mutate_at(
    vars(matches("^[^_]+_date$"), c30_dob),
    list(as.Date)
  ) %>%
  mutate(
    visit = redcap_event_name %>%
      sub("_arm.*", "", .) %>%
      recode(
        .,
        linea_de_base = "baseline",
        .default = .
      ) %>%
      factor(
        levels = c("baseline", "p1", "p2","m1", "m2",  "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10","m11", "b4")
      )
  ) %>%
  print()


# repeated crfs ----


repeats <- read_csv(
  file = repeats_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()


# arm3 monthly visits ----



# Manually select data export
arm3 <- read_csv(
  file = arm3_file$file,
  col_types = cols(.default = col_character())
) %>%
  filter(id != "99999") %>%
  print()

#datos del piloto
piloto <- read_csv(
  file = piloto_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()


# repeats %>% filter(!is.na(c36a_hhid)) %>% select(c36a_hhid, c36a_date, c36a_time, llegada_servicio=c36a_h_date,
#                                                  hora_servicio=c36a_h_time, id_visita=c36a_visit) %>% left_join(
#       repeats %>% filter(!is.na(c34a_date)) %>% select(c34a_hhid, c34a_date, id_visita=c34a_visit)
#                                                  )

#revision de c34a que tienen id de visita repetido
repeats %>% filter(!is.na(c34a_date)) %>% select(c34a_hhid, c34a_date, id_visita=c34a_visit) %>% 
  group_by(id_visita) %>% count() %>% 
  filter(n>1)
#QC se corrigieron c34 con id visita repetidos
repeats %>% filter(!is.na(c34a_date)) %>% select(record_id,c34a_hhid, c34a_date,c34a_by, id_visita=c34a_visit) %>% filter(
  id_visita=="V00035" 
  | id_visita=="V00120" | id_visita=="V00123" | id_visita=="V00201" | id_visita=="V00199"
) #%>% write_csv("output/revisar_c34_id_visita_repetido.csv")
integrado_sin_piloto %>% filter( id_visita=="V00035" #se agregó c41 no tenia codigo de visita, el id esta mal
                                  | id_visita=="V00123" | id_visita=="V00201" | id_visita=="V00199") %>% 
  arrange(id)

#arm2 %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by, c36_age, c36_oxy_60) %>% filter(!is.na(c36_oxy_60))

#revision de los c36a de arm2 que deberían tener c34 en piloto
#c36a_main_study
c36a_main_study<-arm2 %>% filter(!is.na(c36a_date)) %>% select(id, c36a_date, c36a_by, redcap_event_name, 
                                              c36a_age,c36a_stridor,c36a_grunt,c36a_s_indraw,
                                              c36a_drink,c36a_vomit, c36a_convulsion, c36a_unconscious,
                                              c36a_feed, c36a_move, c36a_malnutrition, c36a_oxy_60, c36a_oxy_90,
                                              c36a_oxy_120, c36a_temp,c36a_rr1, c36a_rr2
                                              ) %>% 
  mutate(
                    ultrasonido = (
                ( c36a_stridor == "1" | c36a_grunt == "1" | c36a_s_indraw == "1" | c36a_drink == "1" | c36a_vomit == "1" 
                  | c36a_convulsion == "1" | c36a_unconscious == "1" | c36a_feed == "1" | c36a_move == "1" | 
                   c36a_malnutrition == "1" | c36a_oxy_60 <= 92 | c36a_oxy_90 <= 92 | c36a_oxy_120 <= 92 | c36a_temp > 38 | 
         c36a_temp < 35.5 | ( c36a_age < 2 & (c36a_rr1 >= 60 | c36a_rr2 >=60 )) )
                                           )
                                         ) %>% print()

  
piloto %>% filter(!is.na(record_id)) %>% select(record_id, c34_date, c34_by) %>% print()

#c36a_repeated se extrajeron los crfs de las diferentes fuentes de datos
c36a_repeated<-repeats %>% filter(!is.na(c36a_hhid)) %>% transmute(id=c36a_hhid, fecha_crf=as.Date(c36a_date),
                                                                c36a_by, c36a_time, redcap_event_name,
                                                                fecha_servicio=as.Date(c36a_hospitalized),
                                                 hora_servicio=c36a_h_time, id_visita=c36a_visit) %>% 
                                              select(
                                                   id, fecha_crf, iniciales=c36a_by, fecha_servicio, 
                                                   id_visita, evento=redcap_event_name
                                                 ) %>% print()
#c36a main study
c36a_arm2<-arm2 %>% filter(!is.na(c36a_date)) %>% select(id, fecha_crf=c36a_date, c36a_by, 
                                                         fecha_servicio=c36a_hospitalized, redcap_event_name) %>%
                                                        mutate(
                                                           evento=case_when(
                                                             redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                             redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                             redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                             redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                             redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                           ),
                                                           id_visita=NA_character_
                                                         ) %>% select(id, fecha_crf, iniciales=c36a_by, fecha_servicio, 
                                                                      id_visita,evento) %>% print()

#c36 main study
c36_main_study<-arm2 %>% filter(!is.na(c36_date)) %>% select(id, fecha_crf=c36_date, c36_by, 
                                                             fecha_servicio=c36_hospitalized, 
                                                             redcap_event_name) %>%  mutate(
                                                               evento=case_when(
                                                                 redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                                 redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                                 redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                                 redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                                 redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                               ),
                                                               id_visita=NA_character_
                                                             ) %>% select(
                                                               id, fecha_crf, iniciales=c36_by, fecha_servicio, evento, id_visita
                                                             ) %>% print()

#c37 main study
c37_arm2<-arm2 %>% filter(!is.na(c37_date)) %>% select(id, fecha_crf=c37_date, 
                                                       iniciales=c37_by, fecha_servicio=c37_date_cxr, 
                                                       redcap_event_name) %>%  mutate(
                                                         evento=case_when(
                                                           redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                           redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                           redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                           redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                           redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                         ),
                                                         id_visita=NA_character_
                                                       ) %>% select(id, fecha_crf, iniciales, fecha_servicio,
                                                                    evento, id_visita) %>%  print()

#c37 repeated
c37_repeated<-repeats %>% filter(!is.na(c37_date)) %>% transmute(id=c37a_hhid, fecha_crf=c37_date, c37_visit,
                                                              iniciales=c37_by, 
                                                              fecha_servicio=c37_date_cxr,
                                                              redcap_event_name) %>% transmute(
                                                                id, fecha_crf, iniciales, fecha_servicio,
                                                                evento=redcap_event_name, id_visita=c37_visit
                                                                
                                                              ) %>%  print()

#c40 arm2
c40_arm2<-arm2 %>% filter(!is.na(c40_date)) %>% transmute(id, fecha_crf=c40_date, iniciales=c40_by, 
                                                       fecha_servicio=c40_date_arrive, 
                                                       redcap_event_name
                                                      ) %>% mutate(
                                                         evento=case_when(
                                                           redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                           redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                           redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                           redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                           redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                         ),
                                                         id_visita=NA_character_
                                                       ) %>% print()

#c40 Repeated
c40_repeated<-repeats %>% filter(!is.na(c40_date)) %>% select(id=c40_hhid, fecha_crf=c40_date, 
                                                                iniciales=c40_by, fecha_servicio=c40_date_arrive,
                                                              evento=redcap_event_name, id_visita=c40_visit) %>% 
                                                                  print()

#C41 arm2
c41_arm2<-arm2 %>% filter(!is.na(c41_date)) %>% select(id, fecha_crf=c41_date, iniciales=c41_by, 
                                                       fecha_servicio=c41_date_admit, redcap_event_name) %>% 
                                                                      mutate(
                                                                        evento=case_when(
                                                                          redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                                          redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                                          redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                                          redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                                          redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                                        ),
                                                                        id_visita=NA_character_
                                                                      ) %>% print()

#c41 Repeated
c41_repeated<-repeats %>% filter(!is.na(c41_date)) %>% select(id=c41_hhid, fecha_crf=c41_date, 
                                                              iniciales=c41_by, fecha_servicio=c41_date_admit,
                                                              evento=redcap_event_name, id_visita=c41_visit) %>% 
                                                                print()

#c34 arm2
c34_arm2<-arm2 %>% filter(!is.na(c34_date)) %>% select(id, fecha_crf=c34_date, iniciales=c34_by, 
                                                       fecha_servicio=c34_date, redcap_event_name
                                                       ) %>% 
                                                        mutate(
                                                          evento=case_when(
                                                            redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
                                                            redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
                                                            redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
                                                            redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
                                                            redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
                                                          ),
                                                          id_visita=NA_character_
                                                        ) %>% print()

#c34 Repeated
c34_repeated<-repeats %>% filter(!is.na(c34a_date)) %>% select(id=c34a_hhid, fecha_crf=c34a_date, 
                                                               iniciales=c34a_by, fecha_servicio=c34a_exam_date,
                                                               evento=redcap_event_name, id_visita=c34a_visit) %>% 
                                                        print()

#c34 piloto
c34_piloto<-piloto %>% filter(!is.na(c34_date)) %>% select(id=record_id, fecha_crf=c34_date, iniciales=c34_by,
                                                           fecha_servicio=c34_date, evento=redcap_event_name,
                                                           id_visita=c34_id_visita) %>% 
                                                      print()
#tabla integrada para conteo y revisión de crfs
integrado<-c36a_arm2 %>% transmute(id, crf="c36a_main_study", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita) %>% bind_rows(
  list(
  c36a_repeated %>% transmute(id, crf="c36a_repeated", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c36_main_study %>% transmute(id, crf="c36_main_study",fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c34_repeated %>% transmute(id, crf="c34a_repeated", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita ),
  c34_piloto %>% transmute(id, crf="c34_piloto", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c37_arm2 %>% transmute(id, crf="c37_main_study", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c37_repeated %>% transmute(id, crf="c37_repeated", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c40_repeated %>% transmute(id, crf="c40_repeated", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c40_arm2 %>% transmute(id, crf="c40_main_study", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c41_repeated %>% transmute(id, crf="c41_repeated", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita),
  c41_arm2 %>% transmute(id, crf="c41_main_study", fecha_crf=as.Date(fecha_crf), iniciales, fecha_servicio=as.Date(fecha_servicio), evento, id_visita)
  )
) %>% print()

#crear campo piloto para indicar si el crf pertenece al proyecto piloto vigilancia
integrado<-integrado %>%mutate(piloto=if_else(crf=="c34_piloto","Si","No")) 
# %>% 
#     filter(piloto=="0") %>% select(-piloto, everything()) #%>% 
    #write_csv("output/seguimiento_crf_neumonia_sin_piloto.csv")

  integrado %>% filter(piloto=="Si") %>% writexl::write_xlsx("output/crfs_piloto.xlsx")
  
  #revisión de coincidencias en integrado de crfs
  
  revision_id_visita<-integrado %>% group_by(id_visita, id) %>% count() %>% ungroup() %>% select(id_visita, id)
#validar codigos de visita utilizados en mas de un paciente
  revision_id_visita %>% group_by(id_visita) %>% count() %>% filter(n>1)
  
  #revisar cantidad de registros sin id_visita
  integrado %>% filter(is.na(id_visita)) #626 sin id visita
  integrado %>% filter(piloto=="No") %>% filter(is.na(id_visita)) #319 excluyendo piloto
  integrado %>% filter(piloto=="Si") %>% filter(is.na(id_visita)) #307 son del piloto
  integrado %>% filter(is.na(id_visita)) %>% group_by(crf, evento) %>% count() %>% writexl::write_xlsx(
    "output/conteo_crfs_sin_id_visita.xlsx"
  )
  
  #revision_id_visita %>% filter(piloto=="No") %>% group_by(id_visita) %>% count() %>% filter(n>1)
#integrar a la tabla la validación de codigos de visita utilizados en mas de un paciente
 integrado_1<- integrado %>% filter(!is.na(id_visita)) %>% mutate(
    rev_usada_mas_de_un_paciente= if_else(
      id_visita=="V00080" | id_visita=="V00183" | id_visita=="V00201", "Si", NA_character_ 
    )
  )
 
 
#crear fecha de referencia o fecha de servicio, como apoyo para la identificación de casos
 integrado_1<-integrado_1 %>% select(-rev_usada_mas_de_un_paciente) %>%  mutate(
  fecha_referencia_revision=if_else(
    is.na(fecha_servicio), as.Date(fecha_crf), as.Date(fecha_servicio)
  )
)
 



 
#revisiones
#quitar los C34 pilotos
  integrado_sin_piloto<-integrado

#todos los c36 que tienen hospitalización
  #Revisión c40 con c36 que tienen fecha de hospitalización
  dt_diferente_evento<-integrado_sin_piloto %>% filter(grepl("^c40",crf)) %>% left_join(
    
    integrado_sin_piloto %>% filter(grepl("^c36",crf)) %>% 
      select(id, fecha_servicio, evento_c36=evento) %>% filter(
    !is.na(fecha_servicio)) %>% transmute(id, fecha_servicio, evento_c36, c36="tiene_ingreso_hospitalizacion")
  ) %>% mutate(
    diferente_evento=if_else(evento!=evento_c36, "Revisar", "")
  ) %>% filter(diferente_evento=="Revisar") %>% select(id,crf, fecha_crf,iniciales, evento,diferente_evento)
    #writexl::write_xlsx("output/c40_sin_c36.xlsx")
    #marcar revisión de eventos
  integrado_sin_piloto<- integrado_sin_piloto %>% left_join(
      dt_diferente_evento
    ) 
  
  #revisar los c36 con fecha hospitalización que tengan un c40, para determinar si es necesario llenar un c40 retrospectivo con base en notas del personal
  
  integrado_sin_piloto %>% filter(grepl("^c36",crf)) %>% 
    select(id, crf,fecha_c36=fecha_crf,fecha_servicio, evento_c36=evento, id_visita) %>% 
    filter(
    !is.na(fecha_servicio)
  ) %>% filter(!is.na(id_visita)) %>% left_join(
    integrado_sin_piloto %>% filter(grepl("^c40",crf)) %>%  select(id_c40=id, fecha_c40=fecha_crf, evento_c40=evento, id_visita) %>% 
      filter(!is.na(id_visita)) 
  ) %>% left_join(
    integrado_sin_piloto %>% filter(grepl("^c41",crf)) %>%  select(id_c41=id, fecha_c41=fecha_crf, evento_c41=evento, id_visita) %>% 
      filter(!is.na(id_visita)) 
  ) %>% 
    writexl::write_xlsx("output/revision_c36_hosp_c40.xlsx")
  
  #bitacora de revisiones 
  Issues<-integrado_sin_piloto %>% filter(id=="33366" | id=="33423" | id=="33357") %>% 
    filter(crf=="c40_repeated"
                      | crf=="c41_repeated") %>% arrange(id) %>% filter(
                        id_visita=="V00140" | id_visita=="V00027" | id_visita=="V00131"
                      ) %>%  mutate(
                        issue="Se completó crf faltante retrospectivamente"
                      ) %>% bind_rows(
                        integrado_sin_piloto %>% filter(id=="33014" & crf=="c40_main_study" & 
                                                          fecha_servicio=="2019-03-22") %>% mutate(
                                                            issue="Se completó crf faltante retrospectivamente"
                                                          )
                      ) %>% bind_rows(
      list(
                        integrado_sin_piloto %>% 
      filter(id=="33018" & (crf=="c36_main_study" | crf=="c37_main_study") & fecha_crf=="2019-05-20") %>% 
        mutate(
          issue="Se completó crf c40 y c41 en ss2 retrospectivamente"
        ),
      integrado_sin_piloto %>% filter(id=="33018" & (crf=="c40_main_study" | crf=="c41_main_study") 
                                      & fecha_servicio=="2019-06-12" & evento=="ss4"
                      ) %>%  mutate(
                        issue="Esta sobre escrito con valores de ss3, necesita regresarse a los valores anteriores que corresponden a ss4"
                      ),
      integrado_sin_piloto %>% filter(id=="33018" & (crf=="c40_main_study" | crf=="c41_main_study") 
                                      & fecha_servicio=="2019-06-12" & evento=="ss1"
      ) %>%  mutate(
        issue="DMC debe eliminar del ss1, el correcto se encuentra en ss3 (crf repetido)"
      )
      ) 
 ) %>% bind_rows(
   integrado_sin_piloto %>% filter(id=="33140" & crf=="c36_main_study" & evento=="ss1" & fecha_servicio=="2019-03-08") %>% 
     mutate(
       issue="Se creo c40 y c41 retrospectivamente con base en registros de campo"
     )
 ) %>% bind_rows(
   integrado_sin_piloto %>% filter(id=="33398" & crf=="c36_main_study" & evento=="ss4") %>% 
     mutate(
       issue="DMC debe cambiar la fecha de servicio del c36, la correcta es: 2019-08-07 (Y-M-D), c40 y c41 ya tienen la fecha correcta, con base en registros del personal de campo"
     )
 ) %>% bind_rows(
   integrado_sin_piloto %>% filter(id=="35032" & crf=="c36_main_study" & evento=="ss2") %>%
     mutate(
       issue="DMC debe cambiar la fecha de servicio del c36 de ss2  a: 2019-05-02 (Y-M-D), DMC debe eliminar el c37 de ss1, se movio c37 ss1 hacia ss2 y se crearon los c40 y c41 en ss2 retrospectivamente"
     )
 ) %>% bind_rows(
   integrado_sin_piloto %>% filter(id=="33006" & crf=="c36_main_study" & evento=="ss1") %>% 
     mutate(
       issue="DMC debe eliminar c40, c41 y c37 de ss1, 
       ya fueron creados en ss2. Este niño no tiene c36 en ss2 porque no se tamizó ya que ingresó por convulsiones, y el c36 de ss1 no tiene hospitalización"
 )
 )

    
 
  
  #Revisar los c36 hospitalizados contra los c40 donde no hay id_visita para buscar coincidencias
  integrado_sin_piloto %>% filter(grepl("^c36",crf)) %>% 
    select(id, crf,fecha_c36=fecha_crf,fecha_servicio, evento_c36=evento, id_visita) %>% 
    filter(
      is.na(id_visita)
    )  %>% filter(!is.na(fecha_servicio)) %>% left_join(
      integrado_sin_piloto %>% filter(grepl("^c40",crf)) %>%  select(id, fecha_c40=fecha_crf, crf_c40=crf,evento_c40=evento, fecha_servicio,id_visita) %>% 
        filter(is.na(id_visita)) %>% select(-id_visita)
    ) %>%   writexl::write_xlsx("output/revision_c36_hosp_c40_sinID_Visita.xlsx")
  
    # integrado_sin_piloto %>% filter(grepl("^c36",crf)) %>% 
    #   select(id, crf,fecha_c36=fecha_crf,fecha_servicio, evento_c36=evento, id_visita) %>% 
    #   filter(
    #     is.na(id_visita)
    #   )  %>% filter(!is.na(fecha_servicio)) %>% left_join(
    #     integrado_sin_piloto %>% filter(grepl("^c40",crf)) %>%  select(id, fecha_c40=fecha_crf, evento_c40=evento, fecha_servicio,id_visita) %>% 
    #       filter(is.na(id_visita)) %>% select(-id_visita)
    #   ) %>% filter(!is.na(fecha_c40)) %>% select(id, crf, fecha_servicio, evento=evento_c36)
    # 
  
 
  #integrado_sin_piloto %>% filter(id=="33006") %>% writexl::write_xlsx("output/33006.xlsx")
  
  # revision de codigos temporales y únicos
revision_codigos_unicos<-integrado_sin_piloto %>% filter(is.na(id_visita)) %>% filter(crf=="c36_main_study" | 
                                                               crf=="c36a_main_study") %>% mutate(
                                                                 tipo_codigo=if_else(
                                                                   is.na(fecha_servicio),"codigo unico",
                                                                   "codigo temporal"
                                                                 )
                                                               )
#se creo un campo para los c36 y c36a de main study sin hospitalización
integrado_sin_piloto %>% left_join(
  revision_codigos_unicos
) %>% arrange(id,evento) %>% writexl::write_xlsx("output/revision_tipos_codigos.xlsx")

Issues<-Issues %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33011" & crf=="c36_main_study" & fecha_crf=="2019-04-17" & evento=="ss2") %>% 
    mutate(
      issue="DMC debe eliminar del ss2, el mismo caso esta ingresado correctamente en ss1"
    )
) %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33095" & crf=="c36_main_study" & fecha_servicio=="2019-04-20" &
                                    evento=="ss1") %>%
    mutate(
      issue="Se completó c41 retrospectivamente en ss1"
    )
) %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33253" & crf=="c36_main_study" & evento=="ss1") %>% 
    mutate(
      issue="Se traslado el C36a de ss1 hacia ss3, para no tener dos c36 en el mismo evento ya que son de fechas diferentes"
    )
) %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33420" & crf=="c36_main_study" & evento=="ss1") %>% 
    mutate(
      issue="CRF repetido c36 y c36a en el mismo evento y con los mismos datos"
    )
) %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33431" & crf=="c37_main_study" & evento=="ss1") %>% 
    mutate(
      issue="CRF tiene fecha de servicio fuera de rango, sin embargo es menor a la fecha de egreso de hospitalización, pertenece al mismo caso"
    )
) %>% bind_rows(
  integrado_sin_piloto %>% filter(id=="33438" & crf=="c41_main_study" & evento=="ss1") %>% 
    mutate(
      issue="DMC debe eliminar este c41, esta ingresado en repeated con id de visita"
    )
)
  
Issues %>% writexl::write_xlsx("output/bitacora_revision_crfs.xlsx")


#hacer match por fecha de servicio con c40 y c36 hospitalizados
  # asignar codigo de visita temporal a c36 hospitalizados
  c36_vt<-read_csv("C:/temp/codigo_temporal_c36_hospitalizados.csv")
  c36_vt<-c36_vt %>% mutate(id=as.character(id), id_temporal=id_visita_generado)
  
  integrado_sin_piloto<-integrado_sin_piloto %>% mutate(
    id_visita_generado=id_visita
  ) 

  #asignar codigo temporal al set de datos, el códito temporal se generó con una revisión manual detallada en conjunto con Adly
  integrado_sin_piloto<-integrado_sin_piloto %>% left_join(
  c36_vt %>% select(-id_visita_generado)
) %>% mutate(
  id_visita_generado=if_else(!is.na(id_visita),id_visita,id_temporal)
) %>% select(-id_temporal)

  #asignacion de codigo temporal para C40, despues de hacer una revisión manual de los casos revisando incluso notas del staff de campo  
c40_vt<-read_csv("c:/temp/codigo_temporal_c40_hospitalizados.csv")  
c40_vt<-c40_vt %>% mutate(id=as.character(id),id_temporal=id_visita_generado)

#asignar codigo temporal a c40 en el set de datos completo
integrado_sin_piloto<-integrado_sin_piloto %>% left_join(
  c40_vt %>% select(-id_visita_generado)
) %>% mutate(
  id_visita_generado=if_else(!is.na(id_temporal),id_temporal,id_visita_generado)
) %>% select(-id_temporal)

#revisión de casos c41
integrado_sin_piloto %>% filter(!is.na(id_visita_generado)) %>% filter(is.na(id_visita)) %>% 
  filter(grepl("^c36",crf)) %>% left_join(
    integrado_sin_piloto %>% filter(grepl("^c41", crf)) %>% select(
      id, crf_c41=crf, fecha_c41=fecha_crf, fecha_servicio, evento_c41=evento, iniciales_c41=iniciales
    )
  ) %>% writexl::write_xlsx("output/revision_c36_c41_hospitalizados.xlsx")

# asignar codigo de visita temporal a c36 hospitalizados a c41
c41_vt<-read_csv("C:/temp/codigo_temporal_c41_hospitalizados.csv")
c41_vt<-c41_vt %>% mutate(id=as.character(id), fecha_crf=as.Date(fecha_crf))

#asignar codigo temporal a los c41 con base en revisión manual y notas del staff

integrado_sin_piloto<-integrado_sin_piloto %>% left_join(
  c41_vt 
) %>% mutate(
  id_visita_generado=if_else(!is.na(id_tmp),id_tmp,id_visita_generado)
) %>% select(-id_tmp)

#revisión de casos c37 para asiganación de códigos temporales

integrado_sin_piloto %>% filter(!is.na(id_visita_generado)) %>% filter(is.na(id_visita)) %>% 
  filter(grepl("^c36",crf)) %>% left_join(
    integrado_sin_piloto %>% filter(grepl("^c37", crf)) %>% select(
      id, crf_c37=crf, fecha_c37=fecha_crf, fecha_servicio, evento_c37=evento, iniciales_c37=iniciales
    )
  ) %>% writexl::write_xlsx("output/revision_c36_c37_hospitalizados.xlsx")


#asiganción de codigo temporal para c37
c37_vt<-read_csv("c:/temp/codigo_temporal_c37_hospitalizados.csv")  
c37_vt<-c37_vt %>% mutate(id=as.character(id),id_temporal=id_visita_generado)

integrado_sin_piloto<-integrado_sin_piloto %>% left_join(
  c37_vt %>% select(-id_visita_generado)
) %>% mutate(
  id_visita_generado=if_else(!is.na(id_temporal),id_temporal,id_visita_generado)
) %>% select(-id_temporal)

#revision de casos pendientes, revisión manual a detalle de cada caso con Adly
integrado_sin_piloto %>% writexl::write_xlsx("output/revision_crf_neumonia_id_temporal.xlsx")

#ver c31 del caso 33014, revisión caso especial
gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by, visit) %>% filter(id=="33014")


#revisiond el caso 35032
gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% filter(id=="35032") %>%
  select(id, redcap_event_name, visit, matches("^c36")) %>%  writexl::write_xlsx(
  "output/revision_id_c36.xlsx"
)

#leer codigos asignados manualmente
neumonia_ids_asignados<- readxl::read_xlsx("c:/temp/revision_neumonia_final.xlsx")
neumonia_ids_asignados<-neumonia_ids_asignados %>% 
  mutate(id=as.character(id), fecha_crf=as.Date(fecha_crf))

datos_revisados<-integrado_sin_piloto %>% select(
  id, crf, fecha_crf, iniciales, fecha_servicio, evento, flag1, id_visita_generado
) %>% 
  left_join(
  neumonia_ids_asignados %>% select(
    id, crf, fecha_crf, iniciales, evento, id_visita_final=id_visita_generado, 
    comentarios
  )
) %>% print()

#generar archivo para revisión en linea
neumonia_ids_asignados %>%  writexl::write_xlsx("output/revision_crf_neumonia_ids_generados.xlsx")


#armar set de datos de los diferentes origenes, con el ID que los identifica





#----------------------------------------------


#revision linea de tiempo
piloto %>% select(record_id, today) %>% arrange(today)
piloto %>% select(record_id, today) %>% arrange(desc(today))
gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% select(id, c36_date) %>% 
  arrange(c36_date)
gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% select(id, c36_date) %>% 
  arrange(desc(c36_date))

gt_emory_data_arm2 %>% filter(!is.na(c36a_date)) %>% select(id, c36a_date) %>% 
  arrange(c36a_date)
gt_emory_data_arm2 %>% filter(!is.na(c36a_date)) %>% select(id, c36a_date) %>% 
  arrange(desc(c36a_date))

repeats %>% filter(!is.na(c36a_date)) %>% select(record_id, c36a_date) %>% 
  arrange(desc(c36a_date))

#---------------------------------

dt_h41b<-read_csv("c:/temp/HAPINGuatemalaMainSt-H41H41OAWH41bcomplet_DATA_2021-10-21_1658.csv")

dt_intensivo<-read_csv("C:/temp/HAPINGuatemalaExposu_DATA_2021-10-21_1714.csv")

dt_intensivo %>% filter(!is.na(h41_date))

dt_intensivo %>% filter(!is.na(h41b_date)) %>% select(record_id, matches("^h41b_")) %>% select(
  record_id, h41b_date, matches("^h41b_pum"), matches("^h41b_ins_note"), matches("^h41b_data_")
) %>% arrange(h41b_date) %>% writexl::write_xlsx("output/revisión_h41b_intensivo_pump.xlsx")


#----------------------------------------------------------------------  
integrado_1 %>% group_by(id, fecha_servicio) %>% group_by(id_visita) %>% count()

c36_main_study %>% filter(id=="33006")

arm2 %>% filter(!is.na(c36_date)) %>% select(id, fecha_crf=c36_date, c36_by, 
                                             fecha_servicio=c36_hospitalized, 
                                             redcap_event_name, matches("^c36_")) %>% writexl::write_xlsx(
                                               "output/c36_revision_ms.xlsx"
                                             )




#revision de c36 piloto
c36_piloto<-piloto %>% filter(!is.na(today)) %>% mutate(id=as.character(record_id), today, time, dateofbirth, age, sexo, interviewer)

gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% filter(id=="33006") %>% writexl::write_xlsx(
  "output/c36.xlsx"
)

