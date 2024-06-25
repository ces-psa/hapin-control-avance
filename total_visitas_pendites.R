# load use packages
library(package = "tidyverse")
library(package = "crosstalk")
library(package= "kableExtra")


###CARGA DE DATOS EMORY Y UVG
# Pre-screening information (s0)
#source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Load helper functions
#source(file = "scripts/zz_output.R")

# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

#salidas
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

#datos de entregas de gas y cambios de cilindro
#source(file = "scripts/1_all_gas_action.R", encoding = "UTF-8")
#source(file = "scripts/0_get_cambio_cilindro.R", encoding = "UTF-8")


#CUANTOS P1 Y P2
#P1 pendientes de clinica
p1_clinica_pendientes<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% 
  anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) & (visit=="p1" | visit=="p2")) %>% select(
    id_estudio=id
  )
) %>% 
  filter(fp1_final>=Sys.Date()) %>% 
  select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
    mesp1=lubridate::month(fp1),
    aniop1=lubridate::year(fp1),
    mesp2=lubridate::month(fp2),
    aniop2=lubridate::year(fp2)
  ) %>% mutate(
    type=if_else(
      grepl("^35",id_estudio),"owa","pwg"
    )
  ) %>% 
  group_by(aniop1,mesp1,type) %>% summarize(n=n()) %>% arrange(aniop1, mesp1, type) %>% ungroup()

#P2 pendientes de clinica
p2_clinica_pendientes<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(m11_date) & (visit=="p2" | visit=="parto")) %>% select(
    id_estudio=id
  )
) %>% 
  select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
    mesp1=lubridate::month(fp1),
    aniop1=lubridate::year(fp1),
    mesp2=lubridate::month(fp2),
    aniop2=lubridate::year(fp2)
  )  %>% mutate(
    type=if_else(
      grepl("^35",id_estudio),"owa","pwg"
    )
  ) %>%  group_by(aniop2,mesp2,type) %>% summarize(n=n()) %>% arrange(aniop2, mesp2,type) %>% ungroup()



#PENDIENTES B1 Clinica
b1_pendientes_clinica<- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c31_date) & (redcap_event_name=="b1_arm_2" | redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" |
                          redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>%  anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c33_date) & (redcap_event_name=="b1_arm_2" | redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" |
                          redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
#   anti_join(
#   gt_emory_data_arm2 %>% filter(id=="35013")
# ) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b1=as.Date(fecha_nacimiento) + lubridate::days(77)) %>% 
  mutate(   anio=lubridate::year(fecha_b1),
            mes=lubridate::month(fecha_b1)
  ) %>% mutate(
    type=if_else( grepl("^35", id_estudio), "owa","pwg"
      
    )
  ) %>% 
  group_by(
   anio, mes, type
  ) %>% summarize(n=n()) %>% ungroup() %>% print()


#PENDIENTES B2 Clinica
b2_pendientes_clinica<- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c31_date) & (redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2" )
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c33_date) & (redcap_event_name=="b2_arm_2" | redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2" )
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
#   anti_join(
#   gt_emory_data_arm2 %>% filter(id=="35013" )
# ) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b2=as.Date(fecha_nacimiento) + lubridate::days(167)) %>% 
  mutate(   anio=lubridate::year(fecha_b2),
            mes=lubridate::month(fecha_b2)
  ) %>% mutate(
    type=if_else(grepl("^35",id_estudio),"owa","pwg")
  ) %>% 
  group_by(
   anio, mes,type
  ) %>% summarize(n=n()) %>% ungroup() %>% print()

#PENDIENTES B3 Clinica

b3_pendientes_clinica<- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c31_date) & (redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>%  anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c33_date) & (redcap_event_name=="b3_arm_2" | redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
#   anti_join(
#   gt_emory_data_arm2 %>% filter(id=="35013" )
# ) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b3=as.Date(fecha_nacimiento) + lubridate::days(257)) %>% 
  mutate(   anio=lubridate::year(fecha_b3),
            mes=lubridate::month(fecha_b3)
  ) %>% mutate(
    type=if_else(
      grepl("^35",id_estudio),"owa","pwg"
    )
  ) %>% 
  group_by(
    anio, mes,type
  ) %>% summarize(n=n()) %>% ungroup() %>% print()

#PENDIENTES B4 clinica

b4_pendientes_clinica<- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(c31_date) & redcap_event_name=="b4_arm_2"
  ) %>% select(id_estudio=id, redcap_event_name)
 ) %>% anti_join(
   gt_emory_data_arm2 %>% filter(
     !is.na(c33_date) & redcap_event_name=="b4_arm_2"
   ) %>% select(id_estudio=id, redcap_event_name)
 ) %>% 
#anti_join(
#   gt_emory_data_arm2 %>% filter(id=="35013" )
# ) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b4=as.Date(fecha_nacimiento) + lubridate::days(365)) %>% 
  mutate(   anio=lubridate::year(fecha_b4),
            mes=lubridate::month(fecha_b4)
  ) %>% mutate(
    type=if_else(
      grepl("^35",id_estudio), "owa","pwg"
    )
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% 
  summarize(n=n()) %>% ungroup() %>% print()

####------------------------------
# ARMAR LA TABLA PARA EXPORTAR EXCEL CON LOS PENDIENTES DE CLINICA INTEGRADOS
#---------------------------------
Anio<-c("2019")
Mes<-c("10","10","11","11","12","12")
Type<-c("owa","pwg")
anio_2019<-data.frame(Anio,Mes,Type) %>% arrange(Anio)


Anio<-c("2020")
Mes<-c("1","1","2","2","3","3","4","4","5","5","6","6","7","7","8","8","9","9","10","10","11","11","12","12")
Type<-c("owa","pwg")
anio_2020<-data.frame(Anio,Mes,Type) %>% arrange(Anio)

Anio<-c("2021")
Mes<-c("1","1","2","2","3","3","4","4","5","5","6","6")
Type<-c("owa","pwg")
anio_2021<-data.frame(Anio,Mes,Type) %>% arrange(Anio) %>% print()

matris_visitas_pendiente_clinica<-anio_2019 %>% mutate_all(as.character) %>%  bind_rows(
  list(
  anio_2020 %>% mutate_all(as.character),
  anio_2021 %>% mutate_all(as.character)
  )
) %>% mutate(
  codigo=paste0(Anio,Mes,Type)
) %>% 
  select(codigo, Anio, Mes,Type) %>% print()

Visitas_pendientes_clinica<-matris_visitas_pendiente_clinica %>% left_join(
  p1_clinica_pendientes %>% mutate(
    codigo=as.character(paste0(aniop1, mesp1, type))
  ) %>% select(codigo, pend_P1=n)
) %>% left_join(
  p2_clinica_pendientes %>%  mutate(
    codigo=as.character(paste0(aniop2, mesp2, type))
  ) %>% select(codigo, pend_P2=n)
) %>% left_join(
  b1_pendientes_clinica %>%  mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B1=n)
) %>% left_join(
  b2_pendientes_clinica %>% mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B2=n)
) %>% left_join(
  b3_pendientes_clinica %>%  mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B3=n)
) %>% left_join(
  b4_pendientes_clinica %>% mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B4=n)
) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% mutate(
  Total_por_mes= select(., contains("pend")) %>%  rowSums(.,na.rm = TRUE)
) %>% select(-codigo, Anio,Mes, P1=pend_P1, P2=pend_P2, B1=pend_B1, B2=pend_B2, B3=pend_B3, B4=pend_B4)


Visitas_pendientes_clinica %>% writexl::write_xlsx(
  paste0("output/pendientes/Clinica_visitas_pendientes_al_", Sys.Date(),".xlsx")
)







#----------------------------------------------------
#EXPOSICION
#-----------------------------------------------------
#P1 para exposicion
#CUANTOS P1 Y P2
p1_pendientes_exposicion<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & (visit=="p1" | visit=="p2")) %>% select(
    id_estudio=id
  )
) %>% filter(fp2<=Sys.Date()) %>% 
  select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
    mesp1=lubridate::month(fp1),
    aniop1=lubridate::year(fp1),
    mesp2=lubridate::month(fp2),
    aniop2=lubridate::year(fp2),
    type=if_else(
      grepl(
        "^35",id_estudio), "owa", "pwg"
    )
  ) %>% 
group_by(aniop1,mesp1, type) %>% summarize(n=n()) %>% arrange(aniop1, mesp1,type) %>% ungroup() %>% print()


#P2 para exposicion
#CUANTOS P1 Y P2
p2_pendientes_exposicion<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id_estudio=id)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2" | e2_title=="7") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% mutate(
  fcc=as.Date(fpp-lubridate::days(280)),
  dias= (Sys.Date() - as.Date(fpp-lubridate::days(280))),
  edad_semanas=as.numeric(dias)%/% 7,
  edad_dias=as.numeric(dias)%%7,
  edad_gestacional=paste0(edad_semanas,".",edad_dias),
  fp1=fcc + lubridate::days(168),
  fp1_final= fcc + lubridate::days(223),
  fp2=fcc + lubridate::days(224),
  fp2_final= fcc + lubridate::days(252)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & (visit=="p2")) %>% select(
    id_estudio=id
  )
) %>%
  select(id_tamizaje, id_estudio, fpp, fp1, fp1_final, fp2, fp2_final) %>% mutate(
    mesp1=lubridate::month(fp1),
    aniop1=lubridate::year(fp1),
    mesp2=lubridate::month(fp2),
    aniop2=lubridate::year(fp2),
    type=if_else(
      grepl(
        "^35",id_estudio), "owa", "pwg"
    )
  ) %>%  group_by(aniop2,mesp2, type) %>% summarize(n=n()) %>% arrange(aniop2, mesp2, type) %>% ungroup() %>% print()


#%>% filter(aniop1>2019 | aniop2>2019) %>% group_by(aniop1,mesp1) %>% summarize(n=n())


#b1 pendientes exposicion
b1_pendientes_exposicion<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
 ) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
    !is.na(h41_date) ) %>%  filter(redcap_event_name=="b1_arm_2" | 
                                     redcap_event_name=="b2_arm_2" | redcap_event_name=="b4_arm_2") %>% 
    select(id_estudio=id, redcap_event_name)
) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b1=as.Date(fecha_nacimiento) + lubridate::days(63)) %>% 
  mutate(   anio=lubridate::year(fecha_b1),
            mes=lubridate::month(fecha_b1),
            type=if_else(
              grepl(
                "^35",id_estudio), "owa", "pwg"
            )
  ) %>% anti_join(
    gt_emory_data_arm2 %>% filter(!is.na(h42_date)) %>% filter(
      redcap_event_name=="b1_arm_2" | 
        redcap_event_name=="b2_arm_2" | redcap_event_name=="b4_arm_2"
      
    )  %>% transmute(id_estudio=id)
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% summarize(n=n()) %>% ungroup() %>% print()


#pendientes de b2 exposicion
b2_pendientes_exposicion<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
   (!is.na(h41_date) | !is.na(h55_date)) & (redcap_event_name=="b2_arm_2" | redcap_event_name=="b4_arm_2")
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b2=as.Date(fecha_nacimiento) + lubridate::days(153)) %>% 
  mutate(   anio=lubridate::year(fecha_b2),
            mes=lubridate::month(fecha_b2),
            type=if_else(
              grepl(
                "^35",id_estudio), "owa", "pwg"
            )
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% summarize(n=n()) %>% ungroup() %>% print()


#pendients b4 exposicion

b4_pendientes_exposicion<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id_estudio= s4_main_id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id_estudio=id, c30_dob)
) %>% anti_join(
  salidas %>% select(id_estudio=id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e2_title=="2") %>% select(id_estudio=id, e2_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(e1_title=="2") %>% select(id_estudio=id, e1_title)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(
   (!is.na(h41_date) | !is.na(h55_date)) & redcap_event_name=="b4_arm_2"
  ) %>% select(id_estudio=id, redcap_event_name)
) %>% 
  mutate_all(as.character) %>% 
  mutate(fecha_nacimiento=if_else(
    is.na(c30_dob), fpp, c30_dob
  )
  ) %>% mutate(fecha_b4=as.Date(fecha_nacimiento) + lubridate::days(338)) %>% 
  mutate(   anio=lubridate::year(fecha_b4),
            mes=lubridate::month(fecha_b4),
            type=if_else(
              grepl(
                "^35",id_estudio), "owa", "pwg"
            )
  ) %>% 
  group_by(
    anio, mes, type
  ) %>% 
  summarize(n=n()) %>% ungroup() %>% print()








####------------------------------
# ARMAR LA TABLA PARA EXPORTAR EXCEL CON LOS PENDIENTES DE EXPOSICION
#---------------------------------
Anio<-c("2019")
Mes<-c("10","10","11","11","12","12")
Type<-c("owa","pwg")
anio_2019<-data.frame(Anio,Mes,Type) %>% arrange(Anio) %>% print()


Anio<-c("2020")
Mes<-c("1","1","2","2","3","3","4","4","5","5","6","6","7","7","8","8","9","9","10","10","11","11","12","12")
Type<-c("owa","pwg")
anio_2020<-data.frame(Anio,Mes,Type) %>% arrange(Anio) %>% print()

Anio<-c("2021")
Mes<-c("1","1","2","2","3","3","4","4","5","5","6","6")
Type<-c("owa","pwg")
anio_2021<-data.frame(Anio,Mes,Type) %>% arrange(Anio) %>% print()

matris_visitas_pendiente_clinica<-anio_2019 %>% mutate_all(as.character) %>%  bind_rows(
  list(
    anio_2020 %>% mutate_all(as.character),
    anio_2021 %>% mutate_all(as.character)
  )
) %>% mutate(
  codigo=paste0(Anio,Mes,Type)
) %>% 
  select(codigo, Anio, Mes,Type) %>% print()

matris_visitas_pendiente_expo<-anio_2019 %>% mutate_all(as.character) %>%  bind_rows(
  list(
    anio_2020 %>% mutate_all(as.character),
    anio_2021 %>% mutate_all(as.character)
  )
) %>% mutate(
  codigo=paste0(Anio,Mes,Type)
) %>% 
  select(codigo, Anio, Mes, Type)

Visitas_pendientes_exposicion<-matris_visitas_pendiente_expo%>% left_join(
  p1_pendientes_exposicion %>% mutate(
    codigo=as.character(paste0(aniop1, mesp1, type))
  ) %>% select(codigo, pend_P1=n)
) %>% left_join(
  p2_pendientes_exposicion %>%  mutate(
    codigo=as.character(paste0(aniop2, mesp2, type))
  ) %>% select(codigo, pend_P2=n)
) %>% left_join(
  b1_pendientes_exposicion %>%  mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B1=n)
) %>% left_join(
  b2_pendientes_exposicion %>% mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B2=n)
) %>% left_join(
  b4_pendientes_exposicion %>% mutate(
    codigo=as.character(paste0(anio, mes, type))
  ) %>% select(codigo, pend_B4=n)
) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% mutate(
    Total_por_mes= select(., contains("pend")) %>%  rowSums(.,na.rm = TRUE)
  ) %>% select(-codigo, Anio,Mes, P1=pend_P1, P2=pend_P2, B1=pend_B1, B2=pend_B2,  B4=pend_B4)


Visitas_pendientes_exposicion %>% writexl::write_xlsx(
  paste0("output/pendientes/Exposicion_visitas_pendientes_al_", Sys.Date(),".xlsx")
)

##--------------------------------------------------------
#PROYECCION SALIDAS POR MES INTERVENCIÓN
#---------------------------------------------------------

gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nac=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, fec_nac) %>% mutate(
  fec_salida= as.Date(fec_nac) + lubridate::days(round(30.25*12))
) %>% anti_join(
  salidas %>% select(id)
) %>% group_by(lubridate::year(fec_salida), lubridate::month(fec_salida)) %>% count() %>% writexl::write_xlsx("output/proyec_salidas_intervencion.xlsx")




gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nac=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, fec_nac) %>% mutate(
  fec_salida= as.Date(fec_nac) + lubridate::days(round(30.25*12))
) %>%  group_by(lubridate::year(fec_salida), lubridate::month(fec_salida)) %>% count() %>% writexl::write_xlsx("output/salidas_intervencion.xlsx")



##--------------------------------------------------------
#PROYECCION SALIDAS POR MES control
#---------------------------------------------------------

gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nac=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, fec_nac) %>% mutate(
  fec_salida= as.Date(fec_nac) + lubridate::days(round(30.25*12))
) %>% anti_join(
  salidas %>% select(id)
) %>% group_by(lubridate::year(fec_salida), lubridate::month(fec_salida)) %>% 
  count() %>% writexl::write_xlsx("output/proyec_salidas_control.xlsx")




gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nac=if_else(
    is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
  )
) %>% select(id, id_tamizaje, fec_nac) %>% mutate(
  fec_salida= as.Date(fec_nac) + lubridate::days(round(30.25*12))
) %>%  group_by(lubridate::year(fec_salida), lubridate::month(fec_salida)) %>% count() %>% writexl::write_xlsx("output/fechas_salidas_control.xlsx")


# gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>%  left_join(
#   gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
# ) %>% left_join(
#   gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
# ) %>% left_join(
#   gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
# ) %>% mutate(
#   fec_nac=if_else(
#     is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
#   )
# ) %>% transmute(
#   id, fec_nac, visit="m1") %>%
#   complete(nesting(id, fec_nac),
#     visit = c("m1", "m2", "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10", "m11", "b4")
#   ) %>%   left_join(
#     gt_emory_data_arm2 %>%
#       filter(!is.na(c31_date)) %>%
#       filter(visit %in% c("m1", "m2", "b1", "m4", "m5", "b2", "m7", "m8", "b3", "m10", "m11", "b4")) %>%
#       select(id, visit, c31 = c31_date)
#   ) %>% filter(is.na(c31)) %>% anti_join(
#     salidas %>% select(id)
#   )

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>%  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  ) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
  ) %>% mutate(
    fec_nac=if_else(
      is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)
    )
  ) %>% transmute(
    id, fec_nac) %>% anti_join(
      salidas %>% select(id)
    ) %>% filter(lubridate::year(fec_nac)=="2019" & lubridate::month(fec_nac)=="7") group_by(
      lubridate::year(fec_nac),
      lubridate::month(fec_nac)
    ) %>% count() %>% write_csv("output/revision_nacimientos.csv")


gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by) %>% group_by(
  lubridate::year(c33_date),
  lubridate::month(c33_date),
  c33_by
) %>% count() %>% write_csv("output/revision_productividad.csv")



gt_emory_data_arm2 %>% filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by) %>% group_by(
  lubridate::year(c32_date),
  lubridate::month(c32_date),
  c32_by
) %>% count() %>% write_csv("output/revision_productividad_c32.csv")

#salidas Exposición intensivo
data_intensivo<-read_csv("data/exports/HAPINGuatemalaExposu_DATA_2020-10-21_1819.csv")
hogares_intensivo<-data_intensivo %>% mutate_all(as.character) %>%  filter(!is.na(h41_date)) %>% select(id=record_id, redcap_event_name, h41_date) %>% 
  group_by(id) %>% count() %>% select(id)

pend_salidas_intensivo<-hogares_intensivo %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
    gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
  )
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nacimiento=if_else(is.na(c30_dob),as.Date(m17_ga), as.Date(c30_dob)
                           )
  ) %>% select(id, s6_arm, fec_nacimiento) %>% anti_join(
    salidas %>% select(id)
  ) %>% mutate(
    fec_salida=fec_nacimiento + lubridate::days(365),
    s6_arm=recode(s6_arm,"1"="Intervencion","0"="Control")
  ) %>% transmute(id, s6_arm, fec_salida, intensivo="si")
  # group_by(lubridate::year(fec_salida),
  #                lubridate::month(fec_salida),
  #                s6_arm) %>% count() %>% writexl::write_xlsx(paste0("output/projeccion_Salidas_intensivo_,",Sys.Date(),".xlsx"))



gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
)%>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  fec_nacimiento=if_else(is.na(c30_dob),as.Date(m17_ga), as.Date(c30_dob)
  )
) %>% select(id, s6_arm, fec_nacimiento) %>% anti_join(
  salidas %>% select(id)
) %>% filter(s6_arm=="1")
  mutate(
  fec_salida=as.Date(fec_nacimiento) + lubridate::days(365),
  s6_arm=recode(s6_arm,"1"="Intervencion","0"="Control")
) %>% select(id, s6_arm, fec_salida) %>% left_join(
  pend_salidas_intensivo %>% select(id, intensivo)
) %>% group_by(
  lubridate::year(fec_salida),
  lubridate::month(fec_salida),
  s6_arm, intensivo
) %>% count() %>% writexl::write_xlsx("output/pendientes_salidas.xlsx")


