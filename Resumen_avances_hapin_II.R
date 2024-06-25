
# install.package(ggplot2) # para instalar ggplot2
library(scales)
library(ggplot2)


#sacar los 800 ids participantes en hpain 1
#----
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, 
                                                               id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(350),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
)


#conteo avances en niños
#----
#lista de bebes candidatos
listado_bebes<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
  ventana=as.Date(c30_dob) + lubridate::days(330),
  un_anio=as.Date(c30_dob) + lubridate::days(365)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, e3_date_exit_c)
) %>% mutate(
  descartar=case_when(
    is.na(e3_date_exit_c) ~ "No",
    as.Date(e3_date_exit_c)< as.Date(ventana) ~ "Si",
    TRUE ~ "No"
  )
) %>% filter(descartar=="No") %>% select(id, fecha_nacimiento=c30_dob) %>% mutate(
  `12_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*12)),
  `24_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*24)),
  `36_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*36)),
  `48_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*48)),
  `60_meses`= as.Date(fecha_nacimiento) + lubridate::days(round(30.25*60))
) %>% mutate(
    fec_visita_24m= `24_meses` 
) %>% 
  print()

total_participantes<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, 
                                                               id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fec_nacimiento=if_else(is.na(c30_dob),as.Date(m17_ga), as.Date(c30_dob)),
  ) %>% select(id, fec_nacimiento) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_reason)) %>% select(id, e3_reason)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_reason_c)) %>% select(id, e3_reason_c)
  )

matriz_avances<-total_participantes %>% left_join(
listado_bebes %>% mutate(
  Anio=lubridate::year(fec_visita_24m),
  mes=lubridate::month(fec_visita_24m)
) %>% arrange(Anio,mes)
) %>% mutate(
  candidato=case_when(
     !is.na(fec_visita_24m) ~ "1",
    TRUE ~ NA_character_
  ),
  muerte_bebe=case_when(
    e3_reason_c=="6" ~ "1",
    TRUE ~ NA_character_
  ),
  aborto=case_when(
    is.na(e3_reason_c) & e3_reason=="8" ~ "1",
    TRUE ~ NA_character_
  ),
  salida_anticipada=case_when(
    e3_reason=="3" | e3_reason=="4" | e3_reason=="5" ~ "1",
    TRUE ~ NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_consent,
                                           s4_consent_c,
                                       s4_ocon_date, visit)
) %>% left_join(
  gt_hapin_II_data %>%  filter(visit=="b5" & !is.na(s4_date)) %>% filter(!is.na(c35_date)) %>% select(id, c35_date)
) %>%   mutate(
  s4_realizado=case_when(
    !is.na(s4_consent_c) & !is.na(fec_visita_24m) ~ "1",
    TRUE ~ NA_character_
  ),
  rechazados=case_when(
    s4_consent_c=="0" & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    s4_consent_c=="0" ~ "1",
  
    TRUE ~ NA_character_
  ),
  realizadas=case_when(
    !is.na(c35_date) & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    !is.na(c35_date) & !is.na(fec_visita_24m) ~ "1",
    TRUE ~ NA_character_
  ),
  `muertes/abortos/mortinatos`= case_when(
    aborto=="1" ~ "1",
    muerte_bebe=="1" ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes_consentir=case_when(
    !is.na(fec_visita_24m) & is.na(s4_consent_c) & is.na(`muertes/abortos/mortinatos`) ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes_visitar=case_when(
    s4_consent_c=="1" & is.na(c35_date) & !is.na(fec_visita_24m) ~ "1",
    
    TRUE ~ NA_character_
  ),
#agregar avances hapin 36m
)   
#%>% 
  left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_consent_c_36m=s4_consent_c)
) %>% left_join(
  gt_hapin_II_data %>%  filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(c35_date_2)) %>% 
    select(id, c35_date_2)
) %>%  mutate(
  s4_realizado_36m=case_when(
    !is.na(s4_consent_c_36m) & !is.na(fec_visita_36m) ~ "1",
    TRUE ~ NA_character_
  ),
  rechazados_36m=case_when(
    s4_consent_c_36m=="0" & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    s4_consent_c_36m=="0" ~ "1",
    
    TRUE ~ NA_character_
  ),
  realizadas_36m=case_when(
    !is.na(c35_date_2) & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    !is.na(c35_date_2) & !is.na(fec_visita_36m) ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes_consentir_36m=case_when(
    !is.na(fec_visita_36m) & is.na(s4_consent_c_36m) & is.na(`muertes/abortos/mortinatos`) ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes_visita_36mr=case_when(
    s4_consent_c_36m=="1" & is.na(c35_date_2) & !is.na(fec_visita_36m) ~ "1",
    
    TRUE ~ NA_character_
  )
  )%>%   print()


#Categorizar los motivos de rechazo
motivo_rechazo_24m<- listado_bebes %>% select(id) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="24_month_arm_1") %>%  filter(s4_consent_c=="0") %>% 
  select(id, s4_consent_c, s4_reason_c)  %>% anti_join(
   matriz_avances %>% filter(`muertes/abortos/mortinatos`=="1" | salida_anticipada=="1") %>% select(id)
 ) %>% left_join(
   matriz_avances %>% select(id, rechazados)
 ) %>% mutate(
   flag1=case_when(
     grepl("MIGRA", s4_reason_c ) ~ "MIGRACION",
     grepl("MIGRO ", s4_reason_c ) ~ "MIGRACION",
     grepl("MIGRÓ", s4_reason_c ) ~ "MIGRACION",
     grepl("fuera", s4_reason_c) ~ "MIGRACION",
     grepl("FUERA ", s4_reason_c) ~ "MIGRACION",
     grepl("MUDO", s4_reason_c) ~ "MIGRACION",
     grepl("MUDARON", s4_reason_c) ~ "MIGRACION",
     grepl("VIAJE", s4_reason_c) ~ "MIGRACION",
     grepl("VIAJÓ", s4_reason_c) ~ "MIGRACION",
     grepl("Migraron", s4_reason_c) ~ "MIGRACION",
     grepl("PERMISO", s4_reason_c) ~ "NO LE DAN PERMISO",
     grepl("TIEMPO ", s4_reason_c) ~ "NO TIENE TIEMPO",
    # grepl("VALOR ", s4_reason_c) ~ "NO VE VALOR EN LA COMPENSACION",
     grepl("NO QUIERE ", s4_reason_c) ~ "NO QUIERE CONTINUAR PARTICIPANDO",
     grepl("NO QUIERE,", s4_reason_c) ~ "NO QUIERE CONTINUAR PARTICIPANDO",
    grepl("NO ESTA DEACUERDO", s4_reason_c) ~ "NO QUIERE CONTINUAR PARTICIPANDO",
    
   )
 ) %>% mutate(
   Motivo_migracion=if_else(
     flag1=="MIGRACION", "1", NA_character_
   ),
   Motivo_no_permiso=if_else(
     flag1=="NO LE DAN PERMISO", "1", NA_character_
   ),
   Motivo_no_quiere=if_else(
     flag1=="NO QUIERE CONTINUAR PARTICIPANDO", "1", NA_character_
   ),
   Motivo_no_tiempo=if_else(
     flag1=="NO TIENE TIEMPO", "1", NA_character_
   ),
   Motivo_no_valora=if_else(
     flag1=="NO VE VALOR EN LA COMPENSACION", "1", NA_character_
   ),
 )
 ) %>% filter(!is.na(s4_consent_c)) %>%  print()
  #writexl::write_xlsx("output/rechazos_s4_24m.xlsx")

#revision de matriz de avance
matriz_avances %>% left_join(
  motivo_rechazo_24m %>% select(id, Motivo_migracion, Motivo_no_permiso, Motivo_no_quiere, Motivo_no_tiempo,
                                Motivo_no_valora)
) %>% writexl::write_xlsx("output/revision_avances_bebes_hapin2.xlsx") 
matriz_avances %>% group_by(e3_reason, e3_reason_c) %>% count()


#proyeccion de bebes para exposición
matriz_avances_exposicion<-total_participantes %>% left_join(
  listado_bebes %>% mutate(
    Anio=lubridate::year(fec_visita_24m),
    mes=lubridate::month(fec_visita_24m)
  ) %>% arrange(Anio,mes)
) %>% mutate(
  candidato=case_when(
    !is.na(fec_visita_24m) ~ "1",
    TRUE ~ NA_character_
  ),
  muerte_bebe=case_when(
    e3_reason_c=="6" ~ "1",
    TRUE ~ NA_character_
  ),
  aborto=case_when(
    is.na(e3_reason_c) & e3_reason=="8" ~ "1",
    TRUE ~ NA_character_
  ),
  salida_anticipada=case_when(
    e3_reason=="3" | e3_reason=="4" | e3_reason=="5" ~ "1",
    TRUE ~ NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_consent,
                                       s4_consent_c,
                                       s4_ocon_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  filter(!is.na(h41_date)) %>% select(id, h41_date)
) %>% mutate(
  s4_realizado=case_when(
    !is.na(s4_consent_c) & !is.na(fec_visita_36m) ~ "1",
    TRUE ~ NA_character_
  ),
  rechazados=case_when(
    s4_consent_c=="0" & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    s4_consent_c=="0" ~ "1",
    
    TRUE ~ NA_character_
  ),
  
  realizadas=case_when(
    !is.na(h41_date) & (muerte_bebe=="1" | aborto=="1" | salida_anticipada=="1") ~ NA_character_,
    !is.na(h41_date) & !is.na(fec_visita_36m) ~ "1",
    TRUE ~ NA_character_
  ),
  `muertes/abortos/mortinatos`= case_when(
    aborto=="1" ~ "1",
    muerte_bebe=="1" ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes_visitar=case_when(
    s4_consent_c=="1" & is.na(h41_date) & !is.na(fec_visita_36m) ~ "1",
    
    TRUE ~ NA_character_
  ),
  pendientes_consentir=case_when(
    !is.na(fec_visita_36m) & is.na(s4_consent_c) & is.na(`muertes/abortos/mortinatos`) ~ "1",
    TRUE ~ NA_character_
  ),
) %>% print()

#excel para exposicion
matriz_avances_exposicion %>% left_join(
  motivo_rechazo_24m %>% select(id, Motivo_migracion, Motivo_no_permiso, Motivo_no_quiere, Motivo_no_tiempo,
                                Motivo_no_valora)
) %>%  writexl::write_xlsx("output/revision_avances_bebes_exposicion.xlsx") 

#sacar tabla para adultas
td_adultas<-total_participantes %>% left_join(
  listado_bebes %>% mutate(
    Anio=lubridate::year(fec_visita),
    mes=lubridate::month(fec_visita)
  ) %>% arrange(Anio,mes)
) %>% left_join(
  gt_hapin_II_data %>%  filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(a23_date)) %>% select(id, a23_date)
) %>% mutate(
  type=if_else(
    grepl("^35",id),
    "hh_35",
    "hh_33"
  )
) %>% filter(type=="hh_35") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_reason_o)) %>% select(id, e3_reason_o)
) %>% mutate(
  candidato=case_when(
    !is.na(fec_visita) ~ "1",
    TRUE ~ NA_character_
  ),
  muerte_bebe=case_when(
    e3_reason_c=="6" ~ "1",
    TRUE ~ NA_character_
  ),
  aborto_bebe=case_when(
    is.na(e3_reason_c) & e3_reason=="8" ~ "1",
    TRUE ~ NA_character_
  ),
  salida_anticipada_adulta=case_when(
    e3_reason_o=="3" | e3_reason_o=="5" ~ "1",
    TRUE ~ NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_consent,
                                       s4_consent_c,s4_owa,s4_o_reason,
                                       s4_ocon_date)
) %>% mutate(
  s4_realizado=case_when(
    !is.na(s4_owa) ~ "1",
    TRUE ~ NA_character_
  ),
  rechazados=case_when(
    !is.na(s4_owa) & !is.na(s4_o_reason) ~ "1",
    TRUE ~ NA_character_
  ),
  realizadas=case_when(
    !is.na(a23_date) ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes=case_when(
  s4_owa=="1" &  is.na(a23_date) ~ "1",
    !is.na(fec_visita) & is.na(s4_owa) ~ "1",
    TRUE ~ NA_character_
  )
) %>% print()

td_adultas %>% writexl::write_xlsx("output/avances_adultas_hapin_1_5.xlsx")  



#Adultas exposición
td_adultas_exposicion<-total_participantes %>% left_join(
  listado_bebes %>% mutate(
    Anio=lubridate::year(fec_visita),
    mes=lubridate::month(fec_visita)
  ) %>% arrange(Anio,mes)
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(h41_date)) %>% select(id, h41_date)
) %>% mutate(
  type=if_else(
    grepl("^35",id),
    "hh_35",
    "hh_33"
  )
) %>% filter(type=="hh_35") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_reason_o)) %>% select(id, e3_reason_o)
) %>% mutate(
  candidato=case_when(
    !is.na(fec_visita) ~ "1",
    TRUE ~ NA_character_
  ),
  muerte_bebe=case_when(
    e3_reason_c=="6" ~ "1",
    TRUE ~ NA_character_
  ),
  aborto_bebe=case_when(
    is.na(e3_reason_c) & e3_reason=="8" ~ "1",
    TRUE ~ NA_character_
  ),
  salida_anticipada_adulta=case_when(
    e3_reason_o=="3" | e3_reason_o=="5" ~ "1",
    TRUE ~ NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
    filter(!is.na(s4_date)) %>% select(id, s4_consent,
                                       s4_consent_c,s4_owa,s4_o_reason,
                                       s4_ocon_date)
) %>% mutate(
  s4_realizado=case_when(
    !is.na(s4_owa) ~ "1",
    TRUE ~ NA_character_
  ),
  rechazados=case_when(
    !is.na(s4_owa) & !is.na(s4_o_reason) ~ "1",
    TRUE ~ NA_character_
  ),
  realizadas=case_when(
    !is.na(h41_date) ~ "1",
    TRUE ~ NA_character_
  ),
  pendientes=case_when(
    s4_owa=="1" &  is.na(h41_date) ~ "1",
    !is.na(fec_visita) & is.na(s4_owa) ~ "1",
    TRUE ~ NA_character_
  )
) %>%print()

td_adultas_exposicion %>% writexl::write_xlsx("output/avances_adultas_exposicion_1_5.xlsx")  

#graficas realizados pendientes
td_ninos<-matriz_avances %>% mutate(pendientes=if_else(is.na(pendientes_consentir), pendientes_visitar, pendientes_consentir))


dt_grafica_ninos<-td_ninos %>% transmute(id, fec_visita=`24_meses`, s4_realizado, 
                                      rechazados, realizadas, pendientes) %>% 
  filter(!is.na(fec_visita)) %>% mutate(
    Group=case_when(
      is.na(s4_realizado) ~ "24m_Pending",
      !is.na(s4_realizado) & !is.na(realizadas) ~ "   Visit_conducted",
      !is.na(rechazados) ~ " Rejected",
      
      !is.na(s4_realizado) ~ "  Consented",
      
      
      TRUE ~ NA_character_
    ),
    semana_date=lubridate::floor_date(fec_visita, unit = "weeks", week_start = 1),
  ) %>% select(id, semana_date,Group)


##ELABORAR GRAFICA
ggplot(dt_grafica_ninos,aes(x=semana_date, group=Group, fill=Group))+ 
  scale_fill_manual(values=c("#1e8449","#2874a6","#a93226","#717d7e" ))+
  stat_bin( binwidth=2, alpha=0.5,
            position="stack") + theme_bw()+
  xlab("Weeks")+
  ylab("Visits")+
  scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d %m %Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(breaks=seq(1,30,2))
  


#grafica adultas
dt_grafica_adultas<-td_adultas %>% select(id, fec_visita, s4_realizado, rechazados, realizadas, pendientes) %>% 
  filter(!is.na(fec_visita)) %>%  mutate(
    Group=case_when(
      is.na(s4_realizado) ~ " Pendiente_consentir",
      !is.na(s4_realizado) & !is.na(realizadas) ~ "   Visita_realizada",
      !is.na(rechazados) ~ "Rechazados",
      
      !is.na(s4_realizado) ~ "  Consentimiento_realizado",
      
      
      TRUE ~ NA_character_
    ),
    semana_date=lubridate::floor_date(fec_visita, unit = "weeks", week_start = 1),
  ) %>% select(id, semana_date,Group)


##ELABORAR GRAFICA
ggplot(dt_grafica_adultas,aes(x=semana_date, group=Group, fill=Group))+ 
  scale_fill_manual(values=c("#1e8449","#2874a6","#717d7e", "#a93226"))+
  stat_bin( binwidth=2, alpha=0.5,
            position="stack") + theme_bw()+
  xlab("Weeks")+
  ylab("Visits")+
  scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d %m %Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(breaks=seq(1,30,2))

#grafica de lineas
td_visitas<-td_ninos %>% transmute(id, fec_visita=`24_meses`, candidato) %>% filter(!is.na(fec_visita)) %>% mutate(
  semana_date=lubridate::floor_date(fec_visita, unit = "weeks", week_start = 1)
) %>% print()

#Armar tabla para graficas de niños
td_candidatos<-td_visitas %>% group_by(semana_date,candidato) %>% count()

grafica_visitas<-ggplot(td_candidatos,aes(x=semana_date, y=n))+geom_line(color="blue")+
  xlab("Weeks")+
  ylab("Visits")+
  scale_x_date(breaks=date_breaks("1 week"),
               labels=date_format("%d %m %Y"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks=seq(1,30,2))
grafica_c33<- ggplot(c33_realizados,aes(x=semana_date, y=n))+geom_line(color="green")+
      xlab("Weeks")+
      ylab("Visits")+
      scale_x_date(breaks=date_breaks("1 week"),
                   labels=date_format("%d %m %Y"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks=seq(1,30,2))


c33_realizados<-gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(c33_date)) %>% select(c33_date) %>% mutate(
  semana_date=lubridate::floor_date(c33_date, unit="weeks", week_start=1),
  visita_realizada=1
) %>% select(semana_date, visita_realizada) %>% group_by(semana_date,visita_realizada) %>% count()

#matriz de fechas para agrupar las visitas
dt_fechas<-as.data.frame(seq(as.Date("2020-11-30"), as.Date("2022-12-31"), "days"))
matriz_fechas<-dt_fechas %>% select(fec=`seq(as.Date("2020-11-30"), as.Date("2022-12-31"), "days")`)

#realizados clinica
realizados_clinica<-matriz_fechas %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>% filter(!is.na(c33_date)) %>%transmute(
    fec=c33_date, c33_realizado=1,
    semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
  )
) %>% group_by(semana_date, c33_realizado) %>% count() %>% filter(!is.na(c33_realizado)) %>% 
  ungroup() %>% select(semana_date, realizados_clinica=n)

#realizados clinica integrado
realizados_clinica<-matriz_fechas %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>% filter(!is.na(c33_date)) %>%transmute(
    fec=c33_date, c33_realizado=1,
    semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
  )
) %>% group_by(semana_date, c33_realizado) %>% count() %>% filter(!is.na(c33_realizado)) %>% 
  ungroup() %>% select(semana_date, realizados_clinica=n)

realizados_clinica_b6<-matriz_fechas %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(c33_date_2)) %>%transmute(
    fec=c33_date_2, c33_realizado=1,
    semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
  )
) %>% group_by(semana_date, c33_realizado) %>% count() %>% 
  filter(!is.na(c33_realizado)) %>%  ungroup() %>% select(semana_date, realizados_clinica_b6=n)

realizados_clinica<-realizados_clinica %>% left_join(
  realizados_clinica_b6 ) %>% mutate(
  realizados_clinica= if_else(!is.na(realizados_clinica_b6),
                              as.numeric(realizados_clinica) + as.numeric(realizados_clinica_b6),
                              as.numeric(realizados_clinica)
                              )
  ) %>% select(-realizados_clinica_b6)
#realizados exposicion
realizados_exposicion<-matriz_fechas %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>%  filter(!is.na(h41_date)) %>%transmute(
    fec=h41_date, h41_realizado=1,
    semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
  )
) %>% group_by(semana_date, h41_realizado) %>% count() %>% filter(
  !is.na(h41_realizado)) %>% 
  ungroup() %>% select(semana_date, realizados_exposicion=n)

realizadoas_exposicion_b6<-matriz_fechas %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>%  filter(!is.na(h41_date_v2)) %>%transmute(
    fec=h41_date_v2, h41_realizado=1,
    semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
  )
) %>% group_by(semana_date, h41_realizado) %>% count() %>% filter(
  !is.na(h41_realizado)) %>% 
  ungroup() %>% select(semana_date, realizados_exposicion_b6=n)

realizados_exposicion %>% left_join(
  realizadoas_exposicion_b6
  ) %>% mutate(
    realizados_exposicion= if_else(!is.na(realizados_exposicion_b6),
                                as.numeric(realizados_exposicion) + as.numeric(realizados_exposicion_b6),
                                as.numeric(realizados_exposicion)
    )
  ) %>% select(-realizados_exposicion_b6)


#candidatos
td_visitas_36<-td_ninos %>% transmute(id, fec_visita=`36_meses`, candidato_36=candidato) %>% filter(!is.na(fec_visita)) %>% mutate(
  semana_date=lubridate::floor_date(fec_visita, unit = "weeks", week_start = 1)
) %>% print()

#Armar tabla para graficas de niños
td_candidatos_36<-td_visitas_36 %>% group_by(semana_date,candidato_36) %>% count()

visitas<-td_candidatos %>%ungroup() %>%  select(semana_date, candidato=n) %>% bind_rows(
  td_candidatos_36 %>% ungroup() %>%  select(semana_date, candidato_36=n)
) %>% mutate(candidato=if_else(is.na(candidato),candidato_36, candidato)
             ) %>% select(-candidato_36) %>% group_by(semana_date, candidato) %>% count()

visitas<-visitas %>% ungroup() %>% select(semana_date, candidato)
#semanas del eje X
matriz_semanas<-matriz_fechas %>% mutate(
  semana_date=lubridate::floor_date(fec, unit = "weeks", week_start = 1)
) %>% group_by(semana_date) %>% count() %>% select(semana_date)
#generar la tabla matriz para las graficas
tabla_integrada_graficas<-matriz_semanas %>% left_join(
  visitas
) %>% 
  left_join(
  realizados_clinica
) %>% left_join(
  realizados_exposicion
) %>%print()
#enviar la tabla a excel para revisión
  tabla_integrada_graficas %>%  writexl::write_xlsx("output/carga_laboral.xlsx")
  
  
  #grafica integrada
  ##--------
  par(mfrow=c(1,2))
  ggplot(tabla_integrada_graficas,aes(x=semana_date, y=candidatos))+geom_line(color="blue")+
    geom_point(color="blue")+
      xlab("Weeks")+
      ylab("Visits")+
      scale_x_date(breaks=date_breaks("1 week"),
                   labels=date_format("%d %m %Y"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_y_continuous(breaks=seq(1,30,2))

            ggplot(tabla_integrada_graficas,aes(x=semana_date, y=realizados_clinica))+
              geom_line(color="red")+ geom_point(color="red")+
            xlab("Weeks")+
            ylab("Visits")+
            scale_x_date(breaks=date_breaks("1 week"),
                         labels=date_format("%d %m %Y"))+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_y_continuous(breaks=seq(1,30,2))

            ggplot(tabla_integrada_graficas,aes(x=semana_date, y=realizados_clinica))+
              geom_line(color="red")+ geom_point(color="red")+
              xlab("Weeks")+
              ylab("Visits")+
              scale_x_date(breaks=date_breaks("1 week"),
                           labels=date_format("%d %m %Y"))+
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
              scale_y_continuous(breaks=seq(1,30,2))
   #------ 
            #graficas integradas
  

           grafica_integrada<- tabla_integrada_graficas%>% mutate(semana_date=as.character(semana_date)) %>% transmute(
             `  cadidates` =candidatos, ` antrho_visits`=realizados_clinica, `exposure_visits`=realizados_exposicion
           ) %>%   gather(key="variable", value="value", -semana_date)
           grafica_integrada<-grafica_integrada %>% mutate(semana_date=as.Date(semana_date), Group=variable)
           
           ggplot(grafica_integrada %>% mutate(semana_date=as.Date(semana_date), Group=variable),
                  aes(x=semana_date, y=value, color=Group))+geom_line()+ 
             geom_point()+
             xlab("Weeks")+
             ylab("Visits")+
             scale_x_date(breaks=date_breaks("1 week"),
                          labels=date_format("%d %m %Y"))+
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
             scale_y_continuous(breaks=seq(1,30,2)) 
           
#revision pendientes de 3 años
           cumple_visita<-listado_bebes %>% mutate(
             fec_visita_3_Anios=as.Date(fecha_nacimiento)+lubridate::years(3),
             incia_ventana_3=as.Date(fec_visita_3_Anios) - lubridate::days(27)
           ) %>% anti_join(
             matriz_avances %>% filter(rechazados=="1") %>% select(id)
           ) %>% group_by(
             Anio=lubridate::year(fec_visita_3_Anios),
             Mes=lubridate::month(fec_visita_3_Anios)
           ) %>% count()
           
           inicia_ventana<-listado_bebes %>% mutate(
             fec_visita_3_Anios=as.Date(fecha_nacimiento)+lubridate::days(1095),
             incia_ventana_3=as.Date(fec_visita_3_Anios) - lubridate::days(27)
           ) %>% anti_join(
             matriz_avances %>% filter(rechazados=="1") %>% select(id)
           ) %>% group_by(
             Anio=lubridate::year(incia_ventana_3),
             Mes=lubridate::month(incia_ventana_3)
           ) %>% count()
list(
  cumple_visita=cumple_visita,
  inicia_ventana=inicia_ventana
) %>% writexl::write_xlsx("output/conteo_3_anios.xlsx")    

#integrar histograma con 24 y 36 meses
dt_histograma_ninos_integrado<-dt_grafica_ninos %>% bind_rows(
  listado_bebes %>% mutate(
    fec_visita_3_Anios=as.Date(fecha_nacimiento)+lubridate::days(1095),
    incia_ventana_3=as.Date(fec_visita_3_Anios) - lubridate::days(27)
   ) %>% left_join(
     gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(s4_date)) %>% select(
       s4_date, s4_consent_c
     )
   )
    #anti_join(
  #   matriz_avances %>% filter(rechazados=="1") %>% select(id)
  # ) %>% 
  mutate(
    semana_date=lubridate::floor_date(fec_visita_3_Anios, unit = "weeks", week_start = 1),
    #Group="36_Pending"
  ) %>% select(id, semana_date, Group) %>% filter(semana_date<="2022-12-31")
) 

dt_histograma_ninos_integrado %>% mutate(
  mes=paste0(lubridate::year(semana_date),"-",lubridate::month(semana_date))
)


gt_hapin_II_data %>% 

##ELABORAR HISTOGRAMA 24 Y 36 MESES
ggplot(dt_histograma_ninos_integrado,aes(x=semana_date, group=Group, fill=Group))+ 
  scale_fill_manual(values=c("#1e8449","#2874a6","#a93226","#717d7e","#0a0d0d" ))+
  stat_bin( binwidth=2, alpha=0.5,
            position="stack") + theme_bw()+
  xlab("Weeks")+
  ylab("Visits")+
  scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d %m %Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(breaks=seq(1,30,2))

#excel con datos origen de las gráficas
dt_histograma_ninos_integrado %>% writexl::write_xlsx("output/origen_histograma_avances.xlsx")
grafica_integrada %>% writexl::write_xlsx("output/origen_grafica_grupos.xlsx")


#CONTEO DE MUESTRAS
listado_bebes %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent_c)
) %>%  mutate(
  flag_expo=if_else(
    fecha_nacimiento<"2019-01-15","1", NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>%  filter(!is.na(b10_date)) %>% select(id, b10_date, b10_c_urine, b10_tc_urine, 
                                                           b10_tc_u_code, b10_tc_spots,
                                                           b10_tc_b_code)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>%  select(
  id, flag_expo,b10_date,s4_consent_c, b10_c_urine, b10_tc_urine,b10_tc_spots) %>% 
  group_by(s4_consent_c,b10_tc_urine,b10_tc_spots,flag_expo) %>% count() %>% 
  writexl::write_xlsx(paste0("output/conteos_muestras_",Sys.Date(),".xlsx"))

listado_bebes %>% left_join(
  gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(s4_date)) %>% select(id, s4_date,s4_consent, s4_consent_c, s4_owa, 
                                                          s4_ocon_date, s4_o_reason, s4_note)
) %>% mutate(
  type=if_else(grepl("^35",id), "owa","pwg")
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by)
) %>%   writexl::write_xlsx("output/revision_datos_24m.xlsx")

gt_hapin_II_data %>% select(redcap_event_name) %>% table()


## grafica para reunion -----
dt_general<-total_participantes %>% left_join(
  listado_bebes %>% mutate(
    Anio=lubridate::year(fec_visita_24m),
    mes=lubridate::month(fec_visita_24m)
  ) %>% arrange(Anio,mes)
) %>% mutate(
  candidato=case_when(
    !is.na(fec_visita_24m) ~ "1",
    TRUE ~ NA_character_
  )
) %>% left_join(
  listado_bebes %>% mutate(
    Anio=lubridate::year( `36_meses`),
    mes=lubridate::month(`36_meses`)
  ) %>% arrange(Anio,mes)
) %>% mutate(
  fec_visita_36=`36_meses`,
  candidato_36=case_when(
    !is.na(fec_visita_36) ~ "1",
    TRUE ~ NA_character_
  )
) %>% select(id, fec_visita_24m,fec_visita_36, candidato, candidato_36) %>% left_join(
  gt_hapin_II_data %>% filter(visit=='b5') %>% filter(!is.na(s4_date)) %>% filter(!is.na(c35_date)) %>% 
    select(id, fecha_hizo_b5=c35_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=='b6') %>% filter(!is.na(s4_date)) %>% filter(!is.na(c35_date_2)) %>% 
    select(id, fecha_hizo_b6=c35_date_2)
)

dt_esperadas_24<-dt_general %>% select(fec_visita_24m) %>% mutate(
  semana_date=lubridate::floor_date(fec_visita_24m, unit = "weeks", week_start = 1)
) %>% group_by(semana_date) %>% count() %>% select(semana_date, Esperadas_24=n)

dt_esperadas_36<-dt_general %>% select(fec_visita_36) %>% mutate(
  semana_date=lubridate::floor_date(fec_visita_36, unit = "weeks", week_start = 1)
) %>% group_by(semana_date) %>% count() %>% select(semana_date, Esperadas_36=n)

dt_realizadas_24<-dt_general %>% select(fecha_hizo_b5) %>% mutate(
  semana_date=lubridate::floor_date(fecha_hizo_b5, unit = "weeks", week_start = 1)
) %>% group_by(semana_date) %>% count() %>% select(semana_date, Realizadas_24=n)

dt_realizadas_36<-dt_general %>% select(fecha_hizo_b6) %>% mutate(
  semana_date=lubridate::floor_date(fecha_hizo_b6, unit = "weeks", week_start = 1)
) %>% group_by(semana_date) %>% count() %>% select(semana_date, Realizadas_36=n)

dt_merge_visitas<-dt_esperadas_36 %>% merge(
  dt_esperadas_24, by="semana_date", all = TRUE
) %>% merge(
  dt_realizadas_24, by="semana_date", all = TRUE
) %>% merge(
  dt_realizadas_36, by="semana_date", all = TRUE
)
#afc8a4
#ffa000
#panel.grid.major = element_line(color="grey"),
ggplot(dt_merge_visitas) +
  geom_area(aes(x=semana_date, y=Esperadas_24, fill="Expected 24m"),  color= "#ffa000", alpha=0.7, size=0.8) +
  geom_area(aes(x=semana_date, y=Realizadas_24, fill="Completed 24m"), alpha=0.6, size=1)+
  geom_area(aes(x=semana_date, y=Esperadas_36, fill="Expected 36m"),   color="#415c77", alpha=0.7, size=0.8) +
  geom_area(aes(x=semana_date, y=Realizadas_36, fill="Completed 36m"), alpha=0.5, size=1) +
  scale_fill_manual(values = c("#ffd999", "#ffa000","#b7c2cc","#415c77"), limits=c(
    "Expected 24m",
    "Completed 24m",
    "Expected 36m",
    "Completed 36m"
    )) +
  theme_minimal(
  ) +
  scale_x_date(breaks=date_breaks("2 week"), labels=date_format("%d %m %Y")) +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_y_continuous(breaks=seq(1,30,2))+
  labs(fill="", x="Weeks", y="Visits")
  
#desde hace 2 meses para 24 y 36  
gt_hapin_II_data %>% filter(!is.na(h41_date_2)) %>% 
  select(id, redcap_event_name,h41_date_2, h41_by_2) %>% mutate(
    dia=format(lubridate::day(h41_date_2), %d)
  )



