#REVISION DE DATOS

#set de datos con fecha de nacimiento
gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
  #fecha de nacimiento
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% 
    select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(
    is.na(as.Date(c30_dob)), as.Date(m17_ga), as.Date(c30_dob)
  )
) 

#todos los refiles de gas
all_gas_actions %>% filter(action=="install") %>% 
  filter(source!="emory-stove-install") %>% left_join(
    #set de datos con fechas de nacimiento
    gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
      #fecha de nacimiento
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
    ) %>% left_join(
      gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% 
        select(id_tamizaje=id, id=s4_main_id)
    ) %>% left_join(
      gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
    ) %>% mutate(
      fecha_nacimiento=if_else(
        is.na(as.Date(c30_dob)), as.Date(m17_ga), as.Date(c30_dob)
      )
    ) %>% select(house_id=id, fecha_nacimiento)
    
  ) %>% mutate(
    #ver los hogares que tienen al menos un refil antes de la fecha de nacimiento
    flag=case_when(
      as.Date(date) < as.Date(fecha_nacimiento) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>% filter(flag=="1") %>% group_by(house_id) %>% 
  count() %>% writexl::write_xlsx("output/conteo_entregas_gas_gestacion.xlsx")


all_gas_actions %>% filter(action=="install") %>% 
  filter(source!="emory-stove-install") %>% left_join(
    #set de datos con fechas de nacimiento
    gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
      #fecha de nacimiento
      gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
    ) %>% left_join(
      gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% 
        select(id_tamizaje=id, id=s4_main_id)
    ) %>% left_join(
      gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
    ) %>% mutate(
      fecha_nacimiento=if_else(
        is.na(as.Date(c30_dob)), as.Date(m17_ga), as.Date(c30_dob)
      )
    ) %>% select(house_id=id, fecha_nacimiento)
    
  ) %>% mutate(
    #ver los hogares que tienen al menos un refil antes de la fecha de nacimiento
    flag=case_when(
      as.Date(date) < as.Date(fecha_nacimiento) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>% filter(flag=="1") %>% mutate(
    flag_tiempo= as.Date(date) - as.Date(request_date)
  ) %>% writexl::write_xlsx("output/entregas_antes_nacimiento.xlsx")



gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, matches("h50_")) %>% select(
  id, h50_traditional
)



#h40_ins_date1 fecha lanzamiento
#h40_stop_date1 fecha finalización
#h40_dot1 id del dot 
#h40_date la fecha
#Revisar fecha de instalacion y fecha de finalización por cada DOT

#sacar fecha inicio y fecha fin de los dots

h40_entrada_Casa1<-gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% select(
  id, visit, h40_date,h40_stove1,h40_area1, h40_dot1, h40_visit1, h40_ins_date1, 
  h40_countinue_dot1, h40_stop_date1
)  %>% transmute(
  id, visit, fecha_h40=h40_date, id_dot=h40_dot1,
  tipo_estufa=case_when(
    h40_stove1=="1" ~ "Abierto/Fuego",
    h40_stove1=="2" ~ "Estufa de biomasa/Plancha",
    h40_stove1=="3" ~ "Rondereza",
    h40_stove1=="4" ~ "Estufa portatil (de lena)",
    h40_stove1=="5" ~ "Queroseno",
    h40_stove1=="6" ~ "Estufa de gas",
    h40_stove1=="7" ~ "Electrica",
    h40_stove1=="8" ~ "Comal",
    h40_stove1=="555" ~ "Otra",
    TRUE ~ NA_character_
    
  ),
  ambiente=case_when(
    h40_area1=="1" ~ "KAP1",
    h40_area1=="2" ~ "KAP2",
    h40_area1=="3" ~ "HOP",
    h40_area1=="555" ~ "Otro",
    TRUE ~ NA_character_
  ),
  tipo_visita=case_when(
    h40_visit1=="1" ~ "Instalacion inicial",
    h40_visit1=="2" ~ "Seguimiento",
    TRUE ~ NA_character_
  ),
  fecha_lanzamiento=h40_ins_date1,
  continua=case_when(
    h40_countinue_dot1=="1" ~ "Si",
    h40_countinue_dot1=="2" ~ "No (Finaliza mision)",
    h40_countinue_dot1=="3" ~ "No (Problema dispositivo)",
    h40_countinue_dot1=="4" ~ "No (Estufa removida)"
  ),
  fecha_finaliza= h40_stop_date1
) %>% filter(!is.na(id_dot)) %>% bind_rows(
  
  #sacar fecha inicio y fecha fin de los 2dos dot
    gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% select(
    id, visit, h40_date,h40_stove2,h40_area2, h40_dot2, h40_visit2, h40_ins_date2, 
    h40_countinue_dot2, h40_stop_date2
  )  %>% transmute(
    id, visit, fecha_h40=h40_date, id_dot=h40_dot2,
    tipo_estufa=case_when(
      h40_stove2=="1" ~ "Abierto/Fuego",
      h40_stove2=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove2=="3" ~ "Rondereza",
      h40_stove2=="4" ~ "Estufa portatil (de lena)",
      h40_stove2=="5" ~ "Queroseno",
      h40_stove2=="6" ~ "Estufa de gas",
      h40_stove2=="7" ~ "Electrica",
      h40_stove2=="8" ~ "Comal",
      h40_stove2=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area2=="1" ~ "KAP1",
      h40_area2=="2" ~ "KAP2",
      h40_area2=="3" ~ "HOP",
      h40_area2=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit2=="1" ~ "Instalacion inicial",
      h40_visit2=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date2,
    continua=case_when(
      h40_countinue_dot2=="1" ~ "Si",
      h40_countinue_dot2=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot2=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot2=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date2
  ) %>% filter(!is.na(id_dot))
)  %>% bind_rows(
  #sacar fecha inicio y fecha fin de los 3ros dot
  gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% select(
    id, visit, h40_date,h40_stove3,h40_area3, h40_dot3, h40_visit3, h40_ins_date3, 
    h40_countinue_dot3, h40_stop_date3
  )  %>% transmute(
    id, visit, fecha_h40=h40_date, id_dot=h40_dot3,
    tipo_estufa=case_when(
      h40_stove3=="1" ~ "Abierto/Fuego",
      h40_stove3=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove3=="3" ~ "Rondereza",
      h40_stove3=="4" ~ "Estufa portatil (de lena)",
      h40_stove3=="5" ~ "Queroseno",
      h40_stove3=="6" ~ "Estufa de gas",
      h40_stove3=="7" ~ "Electrica",
      h40_stove3=="8" ~ "Comal",
      h40_stove3=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area3=="1" ~ "KAP1",
      h40_area3=="2" ~ "KAP2",
      h40_area3=="3" ~ "HOP",
      h40_area3=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit3=="1" ~ "Instalacion inicial",
      h40_visit3=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date3,
    continua=case_when(
      h40_countinue_dot3=="1" ~ "Si",
      h40_countinue_dot3=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot3=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot3=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date3
  ) %>% filter(!is.na(id_dot))
)  %>% bind_rows(
  #sacar fecha inicio y fecha fin de los 4tos dot
  gt_emory_data_arm3 %>% filter(!is.na(h40_date)) %>% transmute(
    id, visit, h40_date,h40_stove4,h40_area4, h40_dot4, h40_visit4, h40_ins_date4, 
    h40_countinue_dot4=NA_character_ , h40_stop_date4
  )  %>% transmute(
    id, visit, fecha_h40=h40_date, id_dot=h40_dot4,
    tipo_estufa=case_when(
      h40_stove4=="1" ~ "Abierto/Fuego",
      h40_stove4=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove4=="3" ~ "Rondereza",
      h40_stove4=="4" ~ "Estufa portatil (de lena)",
      h40_stove4=="5" ~ "Queroseno",
      h40_stove4=="6" ~ "Estufa de gas",
      h40_stove4=="7" ~ "Electrica",
      h40_stove4=="8" ~ "Comal",
      h40_stove4=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area4=="1" ~ "KAP1",
      h40_area4=="2" ~ "KAP2",
      h40_area4=="3" ~ "HOP",
      h40_area4=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit4=="1" ~ "Instalacion inicial",
      h40_visit4=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date4,
    continua=case_when(
      h40_countinue_dot4=="1" ~ "Si",
      h40_countinue_dot4=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot4=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot4=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date4
  ) %>% filter(!is.na(id_dot))
)


#H40 entrada a casa 2
h40_entrada_Casa2<-gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2)) %>% select(
  id, visit, h40_date_v2,h40_stove1_v2,h40_area1_v2, h40_dot1_v2, h40_visit1_v2, h40_ins_date1_v2, 
  h40_countinue_dot1_v2, h40_stop_date1_v2
)  %>% transmute(
  id, visit, fecha_h40=h40_date_v2, id_dot=h40_dot1_v2,
  tipo_estufa=case_when(
    h40_stove1_v2=="1" ~ "Abierto/Fuego",
    h40_stove1_v2=="2" ~ "Estufa de biomasa/Plancha",
    h40_stove1_v2=="3" ~ "Rondereza",
    h40_stove1_v2=="4" ~ "Estufa portatil (de lena)",
    h40_stove1_v2=="5" ~ "Queroseno",
    h40_stove1_v2=="6" ~ "Estufa de gas",
    h40_stove1_v2=="7" ~ "Electrica",
    h40_stove1_v2=="8" ~ "Comal",
    h40_stove1_v2=="555" ~ "Otra",
    TRUE ~ NA_character_
    
  ),
  ambiente=case_when(
    h40_area1_v2=="1" ~ "KAP1",
    h40_area1_v2=="2" ~ "KAP2",
    h40_area1_v2=="3" ~ "HOP",
    h40_area1_v2=="555" ~ "Otro",
    TRUE ~ NA_character_
  ),
  tipo_visita=case_when(
    h40_visit1_v2=="1" ~ "Instalacion inicial",
    h40_visit1_v2=="2" ~ "Seguimiento",
    TRUE ~ NA_character_
  ),
  fecha_lanzamiento=h40_ins_date1_v2,
  continua=case_when(
    h40_countinue_dot1_v2=="1" ~ "Si",
    h40_countinue_dot1_v2=="2" ~ "No (Finaliza mision)",
    h40_countinue_dot1_v2=="3" ~ "No (Problema dispositivo)",
    h40_countinue_dot1_v2=="4" ~ "No (Estufa removida)"
  ),
  fecha_finaliza= h40_stop_date1_v2
) %>% filter(!is.na(id_dot)) %>% bind_rows(
  
  #sacar fecha inicio y fecha fin de los 2dos dot
  gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2)) %>% select(
    id, visit, h40_date_v2,h40_stove2_v2,h40_area2_v2, h40_dot2_v2, h40_visit2_v2, h40_ins_date2_v2, 
    h40_countinue_dot2_v2, h40_stop_date2_v2
  )  %>% transmute(
    id, visit, fecha_h40=h40_date_v2, id_dot=h40_dot2_v2,
    tipo_estufa=case_when(
      h40_stove2_v2=="1" ~ "Abierto/Fuego",
      h40_stove2_v2=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove2_v2=="3" ~ "Rondereza",
      h40_stove2_v2=="4" ~ "Estufa portatil (de lena)",
      h40_stove2_v2=="5" ~ "Queroseno",
      h40_stove2_v2=="6" ~ "Estufa de gas",
      h40_stove2_v2=="7" ~ "Electrica",
      h40_stove2_v2=="8" ~ "Comal",
      h40_stove2_v2=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area2_v2=="1" ~ "KAP1",
      h40_area2_v2=="2" ~ "KAP2",
      h40_area2_v2=="3" ~ "HOP",
      h40_area2_v2=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit2_v2=="1" ~ "Instalacion inicial",
      h40_visit2_v2=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date2_v2,
    continua=case_when(
      h40_countinue_dot2_v2=="1" ~ "Si",
      h40_countinue_dot2_v2=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot2_v2=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot2_v2=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date2_v2
  ) %>% filter(!is.na(id_dot))
)  %>% bind_rows(
  #sacar fecha inicio y fecha fin de los 3ros dot
  gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2)) %>% select(
    id, visit, h40_date_v2, h40_stove3_v2, h40_area3_v2, h40_dot3_v2, h40_visit3_v2, h40_ins_date3_v2, 
    h40_countinue_dot3_v2, h40_stop_date3_v2
  )  %>% transmute(
    id, visit, fecha_h40=h40_date_v2, id_dot=h40_dot3_v2,
    tipo_estufa=case_when(
      h40_stove3_v2=="1" ~ "Abierto/Fuego",
      h40_stove3_v2=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove3_v2=="3" ~ "Rondereza",
      h40_stove3_v2=="4" ~ "Estufa portatil (de lena)",
      h40_stove3_v2=="5" ~ "Queroseno",
      h40_stove3_v2=="6" ~ "Estufa de gas",
      h40_stove3_v2=="7" ~ "Electrica",
      h40_stove3_v2=="8" ~ "Comal",
      h40_stove3_v2=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area3_v2=="1" ~ "KAP1",
      h40_area3_v2=="2" ~ "KAP2",
      h40_area3_v2=="3" ~ "HOP",
      h40_area3_v2=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit3_v2=="1" ~ "Instalacion inicial",
      h40_visit3_v2=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date3_v2,
    continua=case_when(
      h40_countinue_dot3_v2=="1" ~ "Si",
      h40_countinue_dot3_v2=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot3_v2=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot3_v2=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date3_v2
  ) %>% filter(!is.na(id_dot))
)  %>% bind_rows(
  #sacar fecha inicio y fecha fin de los 4tos dot
  gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2)) %>% transmute(
    id, visit, h40_date_v2,h40_stove4_v2,h40_area4_v2, h40_dot4_v2, h40_visit4_v2, h40_ins_date4_v2, 
    h40_countinue_dot4_v2=NA_character_ , h40_stop_date4_v2
  )  %>% transmute(
    id, visit, fecha_h40=h40_date_v2, id_dot=h40_dot4_v2,
    tipo_estufa=case_when(
      h40_stove4_v2=="1" ~ "Abierto/Fuego",
      h40_stove4_v2=="2" ~ "Estufa de biomasa/Plancha",
      h40_stove4_v2=="3" ~ "Rondereza",
      h40_stove4_v2=="4" ~ "Estufa portatil (de lena)",
      h40_stove4_v2=="5" ~ "Queroseno",
      h40_stove4_v2=="6" ~ "Estufa de gas",
      h40_stove4_v2=="7" ~ "Electrica",
      h40_stove4_v2=="8" ~ "Comal",
      h40_stove4_v2=="555" ~ "Otra",
      TRUE ~ NA_character_
      
    ),
    ambiente=case_when(
      h40_area4_v2=="1" ~ "KAP1",
      h40_area4_v2=="2" ~ "KAP2",
      h40_area4_v2=="3" ~ "HOP",
      h40_area4_v2=="555" ~ "Otro",
      TRUE ~ NA_character_
    ),
    tipo_visita=case_when(
      h40_visit4_v2=="1" ~ "Instalacion inicial",
      h40_visit4_v2=="2" ~ "Seguimiento",
      TRUE ~ NA_character_
    ),
    fecha_lanzamiento=h40_ins_date4_v2,
    continua=case_when(
      h40_countinue_dot4_v2=="1" ~ "Si",
      h40_countinue_dot4_v2=="2" ~ "No (Finaliza mision)",
      h40_countinue_dot4_v2=="3" ~ "No (Problema dispositivo)",
      h40_countinue_dot4_v2=="4" ~ "No (Estufa removida)"
    ),
    fecha_finaliza= h40_stop_date4_v2
  ) %>% filter(!is.na(id_dot))
)

all_dots_revision<- h40_entrada_Casa1 %>% bind_rows(
  h40_entrada_Casa2
)
#sacar instalaciones y fin de mision
instalaciones_dots<-all_dots_revision %>% filter(tipo_visita=="Instalacion inicial")
fin_mision_dots<-all_dots_revision %>% filter(!is.na(continua)) %>% filter(continua!="Si")

#unificar fecha instalacion y fecha fin de misión
instalacion_fin_mision_dots<-instalaciones_dots %>% select(-fecha_finaliza) %>% distinct() %>% left_join(
  fin_mision_dots %>% select(id, id_dot, fecha_finaliza) %>% distinct()
) 

#sacar tabla con fecha de instalacion de estufa y fechas de dots
gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% 
  select(id, 
          h50_date,
          h50_traditional) %>% mutate(
          ubicacion_estufa=case_when(
             h50_traditional=="1" ~ "La misma cocina",
             h50_traditional=="2" ~ "Cuarto diferente",
             h50_traditional=="3" ~ "Afuera",
             h50_traditional=="4" ~ "Almacenado",
             h50_traditional=="5" ~ "Destruido",
                           )
                                                           ) %>% left_join(
            instalacion_fin_mision_dots
                                                           ) %>% 
  arrange(id, fecha_h40) %>% mutate(
    dias_dot=as.Date(fecha_finaliza) - as.Date(fecha_lanzamiento)
  ) %>% 
  writexl::write_xlsx("output/revision_estufas_dots.xlsx")
