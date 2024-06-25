#lista de E8 para Flor
gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, c31_date, visit) %>% filter(c31_date>="2020-03-25") %>% anti_join(
  
  gt_emory_data_arm2 %>% filter(!is.na(e8_date)) %>% filter(e8_initials1=="ADA" | e8_initials2=="ADA" | e8_initials3=="ADA" | e8_initials4=="ADA" |
                                                              e8_initials1=="AIR" | e8_initials2=="AIR" | e8_initials3=="AIR" | e8_initials4=="AIR" |
                                                              e8_initials1=="FEJ" | e8_initials2=="FEJ" | e8_initials3=="FEJ" | e8_initials4=="FEJ" ) %>% 
    select(id)
) %>% writexl::write_xlsx("output/lista_E8_pendientes_al_2020-05-21.xlsx")





##REVISAR DENOMINADORES DE VISITAS PROGRAMADAS VRS VISITAS REALIZADAS
#--------------------------------------------------------------------------

# find most recent files
path_file <- list.files(
  path = "D:/Descargas/listados",
  pattern = "programacion_visitas_clinca_del_",
  full.names = TRUE
) %>% 
  print()

programadas_vigentes1<- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-03-29_al_2020-04-05.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes2 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-06_al_2020-04-12.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes3 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-13_al_2020-04-19.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes4 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-20_al_2020-04-26.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes5 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-27_al_2020-05-03.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes6 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-05-04_al_2020-05-10.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()
programadas_vigentes7 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-05-11_al_2020-05-17.xlsx", sheet = "lista_semanal_vigentes", col_names = TRUE
) %>%
  print()

#todas las programadas vigentes:
programadas_vigentes<-programadas_vigentes1 %>% bind_rows(
  list(
    programadas_vigentes2,
    programadas_vigentes3,
    programadas_vigentes4,
    programadas_vigentes5,
    programadas_vigentes6,
    programadas_vigentes7
  )
) %>% print()

#programadas_vencidas
#----------------------

programadas_vencidas1<- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-03-29_al_2020-04-05.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas2 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-06_al_2020-04-12.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas3 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-13_al_2020-04-19.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas4 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-20_al_2020-04-26.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas5 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-04-27_al_2020-05-03.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas6 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-05-04_al_2020-05-10.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()
programadas_vencidas7 <- readxl::read_excel(
  "D:/Descargas/listados/programacion_visitas_clinca_del_2020-05-11_al_2020-05-17.xlsx", sheet = "lista_semanal_vencidas", col_names = TRUE
) %>%
  print()

programadas_vencidas<- programadas_vencidas1 %>% bind_rows(
  list(
    programadas_vencidas2,
    programadas_vencidas3,
    programadas_vencidas4,
    programadas_vencidas5,
    programadas_vencidas6,
    programadas_vencidas7
  )
)

#visitas programadas vigentes por tipo visita
programadas_vigentes %>% group_by(visita) %>% count()

#visitasprogramadas vencidas
programadas_vencidas %>% group_by(visita) %>% count()


visitas_programadas_nocompletadas <-programadas_vigentes %>% select(id=house_id, visita) %>% anti_join(
gt_emory_data_arm2 %>% filter(!is.na(m11_date) | !is.na(c31_date) | !is.na(a23_date)) %>% filter(visit=="b1" | 
                                                                                                   visit=="b2" | visit=="b3" |
                                                                                                   visit=="b4" | visit=="m1") %>%  
  select(id, visita=visit)
)

#visitas pendientes de realizar
visitas_programadas_nocompletadas %>% group_by(visita) %>% count()
visitas_vencidas_nocompletadas %>% group_by(visita) %>% count()

#Visitas programadas vencidas
visitas_vencidas_nocompletadas <-programadas_vencidas %>% select(id=house_id, visita) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) | !is.na(c31_date) | !is.na(a23_date)) %>% filter(visit=="b1" | 
                                                                                                     visit=="b2" | visit=="b3" |
                                                                                                     visit=="b4" | visit=="m1") %>%  
    select(id, visita=visit)
)

#REVISION DE PROGRAMACION DE HOGARES VRS HOGARES REALIZADOS
total_visitgas_integradas <-programadas_vigentes %>% select(id=house_id, visita) %>%  bind_rows(
  programadas_vencidas %>% select(id=house_id, visita)
)

total_participantes_programadas<- total_visitgas_integradas[!duplicated(total_visitgas_integradas),]

total_pendientes_integradas<-visitas_programadas_nocompletadas %>% bind_rows(
  visitas_vencidas_nocompletadas
)

total_participantes_pendientes<-total_pendientes_integradas[!duplicated(total_pendientes_integradas),]

