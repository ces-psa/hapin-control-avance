

#Lista de muestras estratificado por participante y visita, posterior al 20 de marzo 2020 incluye hapin 1.5
#madre linea basal
lb_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="baseline") %>%  transmute(id, fecha=b10_date, 
                                            validas=b10_tm_spots_num,tipo="madre",
                                            visita="baseline")
  ) %>% print()

#adulta linea basal
lb_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="baseline") %>%  transmute(id, fecha=b10_date, 
                                            validas=b10_to_spots_num,
                                            tipo="owa",
                                            visita="baseline")
  ) %>% print()  

#madre p1
p1_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p1") %>%  transmute(id, fecha=b10_date, 
                                      validas=b10_tm_spots_num, tipo="madre",
                                      visita="p1")
  ) %>% print()

#adulta P1
p1_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p1") %>%  transmute(id, fecha=b10_date, 
                                      validas=b10_to_spots_num,
                                      tipo="owa", visita="p1")
  ) %>% print() 


#madre p2
p2_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p2") %>%  transmute(id, fecha=b10_date, 
                                      validas=b10_tm_spots_num,
                                      tipo="madre",
                                      visita="p2")
  ) %>% print()

#adulta P2
p2_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="p2") %>%  transmute(id, fecha=b10_date, validas=b10_to_spots_num,
                                         tipo="owa",
                                         visita="p2")
  ) %>% print()

                    
nacimiento<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10a_date))  %>%  transmute(id, fecha=b10a_date, 
                                      validas=b10a_tc_spots_num, tipo="bebe",
                                      visita="nacimiento")
  ) %>% print()



#madre b1
b1_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b1") %>%  transmute(id, fecha=b10_date, 
                                         validas=b10_tm_spots_num,
                                         tipo="madre",
                                         visita="b1")
  ) %>% print()

#adulta b1
b1_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b1") %>%  transmute(id, fecha=b10_date, validas=b10_to_spots_num,
                                         tipo="owa",
                                         visita="b1")
  ) %>% print()

#niño b1
b1_nino<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join( 
    gt_emory_data_arm2 %>% filter(!is.na(b10_date))  %>%  filter(visit=="b1") %>% 
      transmute(id, fecha=b10_date, 
               validas=b10_tc_spots_num, tipo="bebe",
                   visita="b1")
  ) %>% print()

#madre b2
b2_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b2") %>%  transmute(id, fecha=b10_date, 
                                         validas=b10_tm_spots_num,
                                         tipo="madre",
                                         visita="b2")
  ) %>% print()

#adulta b2
b2_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b2") %>%  transmute(id, fecha=b10_date, validas=b10_to_spots_num,
                                         tipo="owa",
                                         visita="b2")
  ) %>% print()

#niño b2
b2_nino<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join( 
    gt_emory_data_arm2 %>% filter(!is.na(b10_date))  %>%  filter(visit=="b2") %>% 
      transmute(id, fecha=b10_date, 
                validas=b10_tc_spots_num, tipo="bebe",
                visita="b2")
  ) %>% print()


#madre b4
b4_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b4") %>%  transmute(id, fecha=b10_date, 
                                         validas=b10_tm_spots_num,
                                         tipo="madre",
                                         visita="b4")
  ) %>% print()

#adulta b4
b4_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>% left_join(   
    gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% 
      filter(visit=="b4") %>%  transmute(id, fecha=b10_date, validas=b10_to_spots_num,
                                         tipo="owa",
                                         visita="b4")
  ) %>% print()

#niño b4
b4_nino<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join( 
    gt_emory_data_arm2 %>% filter(!is.na(b10_date))  %>%  filter(visit=="b4") %>% 
      transmute(id, fecha=b10_date, 
                validas=b10_tc_spots_num, tipo="bebe",
                visita="b4")
  ) %>% print()

#madre b5
b5_madre<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% transmute(
      id, fecha=b10_date, validas=b10_tm_spots_num, tipo="madre", visita="b5"
    ) 
  ) %>% print()

#adulta b5
b5_owa<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>%
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% filter(type=="owa") %>%  left_join(
    gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% transmute(
      id, fecha=b10_date, validas=b10_to_spots_num, tipo="owa", visita="b5"
    ) 
  ) %>% print()

#niño b5
b5_nino<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% 
  mutate(type=if_else(grepl("^35",id),"owa","pwg")) %>% left_join( 
    gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% 
      transmute(id, fecha=b10_date, 
                validas=b10_tc_spots_num, tipo="bebe",
                visita="b5")
  ) %>% print()

conteo_filtro_fecha<-lb_madre %>% filter(!is.na(validas)) %>% bind_rows(
  lb_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  p1_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  p1_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  p2_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  p2_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  nacimiento %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b5_nino %>% filter(!is.na(validas))
) %>% filter(as.Date(fecha)>="2020-03-01") %>% group_by(
  tipo, visita, validas
) %>% count() %>% print()

#conteo sin filtro de fecha
conteo_sin_filtro<-lb_madre %>% filter(!is.na(validas)) %>% bind_rows(
  lb_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  p1_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  p1_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  p2_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  p2_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  nacimiento %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b1_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b2_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_madre %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_owa %>% filter(!is.na(validas))
) %>% bind_rows(
  b4_nino %>% filter(!is.na(validas))
) %>% bind_rows(
  b5_nino %>% filter(!is.na(validas))
) %>%  group_by(
  tipo, visita, validas
) %>% count() %>% print()


list(
  conteo_filtro_fecha = "Conteo_mayor_marzo_2020",
  conteo_sin_filtro = "conteo_general"
) %>% writexl::write_xlsx("output/conteos_dbs.xlsx")


conteo_filtro_fecha %>% write_csv("output/conteo_con_fecha.ccsv")
conteo_sin_filtro %>% write_csv("output/conteo_general.csv")
