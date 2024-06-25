library("tidyverse")
#leer datos de emory
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
# listado en z10
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")
# listado en z10
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

#---------------------------------------------------------------------------------------
# #Conteo de salidas del estudio
# salidas<-gt_emory_data %>% select(id,e3_date, e3_by) %>% filter(!is.na(e3_date)) %>%
#   left_join(
#     gt_emory_data %>% select(id,e3_date_o, e3_by_o) %>% filter(!is.na(e3_date_o))
#   ) %>%left_join(
#     gt_emory_data %>% select(id,e3_date_c, e3_by_c) %>% filter(!is.na(e3_date_c))
#   ) %>% left_join(
#     gt_emory_data %>% select(id, c30_date) %>% filter(!is.na(c30_date))
#   ) %>%mutate(
#     type = if_else(
#       condition = grepl("^35[0-9]{3}", id),
#       true = "oaw",
#       false = "pw"
#     )
#   ) %>% mutate(
#     sale = case_when(
#       type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
#       type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
#       type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       TRUE ~ NA_character_
#     )
#   ) %>%filter(sale=="1") %>% select(id,sale)

#-----------------------------------------------------------------------------------------
# gt_emory_data_arm1 %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id, "fecha_enrolamiento"=s4_date) %>% filter(!is.na(id_estudio)) %>% 
#   left_join(
#     gt_emory_data_arm2 %>% select("id_estudio"=id, "fecha_tamizaje"=s6_date, s6_arm) %>% filter(!is.na(fecha_tamizaje))
#   ) %>%left_join(
#     salidas %>% select("id_estudio"=id, sale)
#   ) %>% filter(is.na(sale))

# gt_emory_data_arm1 %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id, "fecha_enrolamiento"=s4_date) %>% filter(!is.na(id_estudio)) %>% 
#   left_join(
#     gt_emory_data_arm2 %>% select("id_estudio"=id, "fecha_tamizaje"=s6_date, s6_arm) %>% filter(!is.na(fecha_tamizaje))
#   ) %>%left_join(
#     salidas %>% select("id_estudio"=id, sale)
#   ) %>% mutate(salio_estudio=if_else(is.na(sale),"No","Si"))


#sacar listado de E2 que se han llenado, considerando las salidas del estudio
gt_emory_data_arm2 %>% select(id, e2_date, e2_by, e2_participant, e2_title,e2_title_other, e2_type,e2_type_other, e2_death_date, e2_outcome,) %>% 
    filter(!is.na(e2_date)) %>%  mutate(E2_participante=case_when(
  e2_participant=="1" ~ "Madre",
  e2_participant=="2" ~ "Mujer Adulta",
  e2_participant=="3" ~ "Nino",
  TRUE ~ NA_character_
)) %>% full_join(
  gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id,e1_date, e1_by, e1_participant, e1_title) %>% filter(e1_title=="2")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, s6_arm) %>% mutate(brazo=recode(s6_arm, "1"="Intervencion", "0"="Control"))
)  %>% mutate(
            E2_titulo_evento=recode(e2_title,
                       "1"= "Quemadura (responda a PARTE B)",
                       "2"="Muerte fetal (> 20 semanas de gestacion) (incluye muerte intrauterina/fallecimiento fetal)",
                       "3"="Hemorragia post-parto",
                       "4"="Septicemia",
                       "5"="Intoxicacion por monoxido de carbono",
                       "6"="Lesion [incluyendo no-violenta (por ejemplo, accidente automovilistico) y de violencia fisica]",
                       "7"="Muerte",
                       "555"="Otro"),
  E2_tipo_EAG=recode(e2_type,
      "1"= "Muerte (no incluyendo muerte del feto)",
      "2"= "Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)",
      "3"= "Situacion que pone en peligro la vida",
      "4"= "Hospitalizacion nueva o prolongada",
      "5"="Discapacidad/incapacidad persistente o significativa",
      "6"= "Anomalia congenita/defecto de nacimiento",
      "555"= "Otro evento/experiencia medica importante"
  ),
  E2_resuelto_AAG=recode(
    e2_outcome,
    "1"= "Resuelto, sin secuelas",
    "2"= "Resuelto con secuelas",
    "3"= "Enveto continua",
    "4"="Muerte"
  ),
  E1_participante=case_when(
    e1_participant=="1" ~ "Madre",
    e1_participant=="2" ~ "Mujer Adulta",
    e1_participant=="3" ~ "Nino",
    TRUE ~ NA_character_
  ),
  E1_titulo_evento=recode(e1_title,
                          "2"= "Aborto espontaneo (< 20 semanas de gestacion)"
                          )
)  %>% left_join(
  salidas %>% select(id, sale)
) %>% 
  mutate(
    tiene_salida_estudio=case_when(
      (e2_title==2 | e2_title==7 | e2_type==1 | e2_type==2 | e2_outcome==4 | e1_title==2) & is.na(sale) ~ "No",
      (e2_title==2 | e2_title==7 | e2_type==1 | e2_type==2 | e2_outcome==4 | e1_title==2) & !is.na(sale) ~ "Si",
      TRUE ~ NA_character_
    )
  ) %>% select(-e2_participant,-e2_title, -e2_type, e2_outcome) %>% mutate(tiene_salida_estudio=if_else(
    id=="35021" | id=="35001", "No Aplica", tiene_salida_estudio
  )) %>% 
  select ("HH_ID"=id,
         "Fecha de E2"=e2_date,
          "Fecha de E1"=e1_date,
          "Iniciales E2"=e2_by,
          "Iniciales E1"=e1_by,
          "Participante en E2"=E2_participante,
          E2_titulo_evento,
          "Otro titulo evento"=e2_title_other,
          E2_tipo_EAG,
          "Otro tipo EAG"= e2_type_other,
          E2_resuelto_AAG,
          "Participante en E1"=E1_participante,
          E1_titulo_evento,
          "Salida Estudio (E3)"=tiene_salida_estudio,
          brazo
  ) %>% writexl::write_xlsx(paste0("output/lista_e2_e1_al_",Sys.Date(),".xlsx"))

#%>% 
  
  
  
  group_by(id) %>% mutate(
    correlativo=1:n(),
    e2_date= as.character(e2_date)) %>% arrange(id, e2_date) %>%  
  select ("Correlativo"=correlativo,
          "HH_ID"=id,
          "Fecha de E2"=e2_date,
          "Iniciales E2"=e2_by,
          "Participante en E2"=participante,
          "Tiene Salida Estudio (E3)"=tiene_salida_estudio
  ) %>% print() 

#%>% write_csv("output/lista_e2_adly.csv")

# #Listar todos los E3 realizados
# e3<-gt_emory_data_arm2 %>% select(id,e3_date, e3_by,e3_last_visit) %>% filter(!is.na(e3_date)) %>%
#   left_join(
#     gt_emory_data_arm2 %>% select(id,e3_date_o, e3_by_o,e3_last_visit_o) %>% filter(!is.na(e3_date_o))
#   ) %>%left_join(
#     gt_emory_data_arm2 %>% select(id,e3_date_c, e3_by_c,e3_last_visit_c) %>% filter(!is.na(e3_date_c))
#   ) %>% left_join(
#     gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
#   ) %>%mutate(
#     type = if_else(
#       condition = grepl("^35[0-9]{3}", id),
#       true = "oaw",
#       false = "pw"
#     )
#   ) %>% mutate(
#     sale = case_when(
#       type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
#       type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
#       type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
#       TRUE ~ NA_character_
#     )
#   )
  
  #exportar reporte con todos los e3
e3 %>% mutate(Estado_en_estudio=if_else(
                          !is.na(sale),"Salio de Estudio", "Continua en Estudio"
                              )
      ) %>% select("HH_ID"=id, "Fecha E3_madre"=e3_date, "Fecha E3_adulta"=e3_date_o,
                   "Fecha E3_nino"=e3_date_c, Estado_en_estudio) %>% writexl::write_xlsx("output/Lista_E3.xlsx")



#--------------------------------------------------
#REPORTE DE SALIDAS E3
#-------------------------------------------------
# Get pre-screening information (s0)
source(file = "reporte vales de compensacion.R", encoding = "UTF-8")

gt_emory_data_arm2 %>% select(id,e3_date, e3_by) %>% filter(!is.na(e3_date)) %>%
  left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_o, e3_by_o) %>% filter(!is.na(e3_date_o))
  ) %>%left_join(
    gt_emory_data_arm2 %>% select(id,e3_date_c, e3_by_c) %>% filter(!is.na(e3_date_c))
  ) %>% left_join(
    gt_emory_data_arm2 %>% select(id, c30_date) %>% filter(!is.na(c30_date))
  ) %>%mutate(
    type = if_else(
      condition = grepl("^35[0-9]{3}", id),
      true = "oaw",
      false = "pw"
    )
  ) %>% mutate(
    sale = case_when(
      type=="oaw" & !is.na(e3_date)&!is.na(e3_date_o)&is.na(c30_date) ~ "1",
      type=="pw" & !is.na(e3_date)&is.na(c30_date) ~ "1",
      type=="oaw" & !is.na(e3_date) & !is.na(e3_date_o) & !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      type=="pw" & !is.na(e3_date) &  !is.na(c30_date) & !is.na(e3_date_c) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%filter(sale=="1") %>% select(id,sale)

e3_brazo<- salidas %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id"=s4_main_id, "id_tamizaje"=id,s4_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, s6_date, s6_arm) %>% mutate(Grupo=recode(s6_arm, "1"="Intervencion","0"="Control"))
) %>% filter(!is.na(s6_arm)) %>% select(id_tamizaje, id, "fecha_salida"=e3_date, fecha_enrrolamiento=s4_date, "fecha_randomizacion"=s6_date, Grupo) %>% 
  left_join(
  total_vales_entregados %>%  filter(tipo_vale=="compensacion") %>% group_by(id) %>% summarize(monto=sum(monto_vale))
  ) %>% 
    left_join(
    total_vales_entregados %>%  filter(tipo_vale=="saldo") %>% group_by(id) %>% summarize(monto_saldo=sum(monto_vale))
    )%>% 
    left_join(
        total_vales_canjeados %>% transmute(id, "monto_vale_canje"=monto)
    ) %>% mutate(
      monto_canjeado=monto_vale_canje-as.numeric(monto_saldo)
      ) %>%
  select(id_tamizaje, id, fecha_salida, fecha_enrrolamiento, fecha_randomizacion, Grupo, "Monto entregado en vales"=monto, "Monto canjeado"=monto_canjeado, "Saldo pendiente de canje"=monto_saldo) %>% 
  writexl::write_xlsx("output/lista_e3_ramdomizados.xlsx")


e2_brazo<-gt_emory_data_arm2 %>% select(id, e2_date, e2_by, e2_participant, e2_title,e2_title_other, e2_type,e2_type_other, e2_death_date, e2_outcome,) %>% filter(!is.na(e2_date)) %>% left_join(
  salidas %>% select(id, sale)
) %>% mutate(participante=case_when(
  e2_participant=="1" ~ "Madre",
  e2_participant=="2" ~ "Mujer Adulta",
  e2_participant=="3" ~ "Nino",
  TRUE ~ NA_character_
)) %>% mutate(
  tiene_salida_estudio=if_else(is.na(sale), "No", "Si"),
  titulo_evento=recode(e2_title,
                       "1"= "Quemadura (responda a PARTE B)",
                       "2"="Muerte fetal (> 20 semanas de gestacion) (incluye muerte intrauterina/fallecimiento fetal)",
                       "3"="Hemorragia post-parto",
                       "4"="Septicemia",
                       "5"="Intoxicacion por monoxido de carbono",
                       "6"="Lesion [incluyendo no-violenta (por ejemplo, accidente automovilistico) y de violencia fisica]",
                       "7"="Muerte",
                       "555"="Otro"),
  tipo_EAG=recode(e2_type,
                  "1"= "Muerte (no incluyendo muerte del feto)",
                  "2"= "Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)",
                  "3"= "Situacion que pone en peligro la vida",
                  "4"= "Hospitalizacion nueva o prolongada",
                  "5"="Discapacidad/incapacidad persistente o significativa",
                  "6"= "Anomalia congenita/defecto de nacimiento",
                  "555"= "Otro evento/experiencia medica importante"
  ),
  resuelto_AAG=recode(
    e2_outcome,
    "1"= "Resuelto, sin secuelas",
    "2"= "Resuelto con secuelas",
    "3"= "Enveto continua",
    "4"="Muerte"
  )
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id, s6_arm) %>% mutate(Grupo=recode(s6_arm, "1"="intervencion", "0"="Control"))
) %>% select(-e2_participant,-e2_title, -e2_type, -s6_arm, e2_outcome) %>% 
  select ("HH_ID"=id,
          "Fecha de E2"=e2_date,
          "Iniciales E2"=e2_by,
          "Participante en E2"=participante,
          titulo_evento,
          "Otro titulo evento"=e2_title_other,
          tipo_EAG,
          "Otro tipo EAG"= e2_type_other,
          resuelto_AAG,
          "Salida Estudio (E3)"=tiene_salida_estudio,
          Grupo
  )

# total_vales_entregados %>% group_by(id) %>% summarize(cantidad_vales_entregados=n()) %>% left_join(
#   total_vales_canjeados %>% select(id, cantidad_vales_canjeados=contador)
# )

#Listado de E3 en Redcap
revision_e3<-gt_emory_data_arm2 %>% filter(!is.na(e3_date) | !is.na(e3_date_c) | !is.na(e3_date_o)) %>% 
  select(id, e3_date, e3_reason, e3_date_c, e3_reason_c, e3_date_o, e3_reason_o) %>% 
  mutate(e3_reason=recode(e3_reason,"1"="Finalizacion del estudio", 
         "2"="No elegible",
        "3"="Retiro voluntario de la participante",
       "4"="Retirada por el equipo del estudio",
 "5"="Se mudo del area de estudio",
 "6"="Fallecio",
"7"="Perdido durante el seguimiento",
"8"="Madre: Aborto/Aborto espontaneo, mortinato, muerte del nino",
"9"="Otro"),
e3_reason_c=recode(e3_reason_c,"1"="Finalizacion del estudio", 
                 "2"="No elegible",
                 "3"="Retiro voluntario de la participante",
                 "4"="Retirada por el equipo del estudio",
                 "5"="Se mudo del area de estudio",
                 "6"="Fallecio",
                 "7"="Perdido durante el seguimiento",
                 "8"="Madre: Aborto/Aborto espontaneo, mortinato, muerte del nino",
                 "9"="Otro"),
e3_reason_o=recode(e3_reason_o,"1"="Finalizacion del estudio", 
                   "2"="No elegible",
                   "3"="Retiro voluntario de la participante",
                   "4"="Retirada por el equipo del estudio",
                   "5"="Se mudo del area de estudio",
                   "6"="Fallecio",
                   "7"="Perdido durante el seguimiento",
                   "8"="Madre: Aborto/Aborto espontaneo, mortinato, muerte del nino",
                   "9"="Otro")
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% print()
  write_csv("output/lista_e3.csv")
  revision_e3 %>% group_by(e3_reason) %>% count()
 
  revision_e3 %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id)
  )%>% group_by(e3_reason) %>% count()
  
  revision_e3 %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, participo_hapin="si")
  ) %>% filter(!is.na(participo_hapin))%>% filter(e3_reason=="Retiro voluntario de la participante") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% 
      select(id,e3_volunt_other )
  )
    group_by(e3_reason) %>% count()
  
  revision_e3 %>% filter(e3_reason=="Retiro voluntario de la participante")
  
  e3_volunt_other_c
  e3_voluntary_c

  
  
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_reason) %>% 
    filter(!is.na(s4_reason)) %>% writexl::write_xlsx("output/revision_s4.xlsx")
    group_by(
    s4_reason
  ) %>% count() %>% writexl::write_xlsx("output/revision_rechazos.xlsx")

    e2_brazo %>% group_by(tipo_EAG) %>% count()
    e2_brazo %>% 
      filter(
        tipo_EAG=="Muerte fetal (incluye muerte intrauterina/fallecimiento fetal)") %>% 
      anti_join(
        gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(HH_ID=id)
      )
    
    gt_emory_data_arm2 %>% filter(!is.na(e3_date)) 
