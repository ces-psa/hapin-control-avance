# load use packages
library(package = "tidyverse")
library(package = "crosstalk")

###CARGA DE DATOS EMORY Y UVG
# Pre-screening information (s0)
source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Load helper functions
source(file = "scripts/zz_output.R")

# Emory RedCap dictionary

source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Stops knitting if lpg delivery data is not up to date
source(file = "scripts/uso-estufas.R", encoding = "UTF-8")

#fecha de la semana
fecha1<-'2019-01-28'
fecha2<-'2019-02-03'


###conteo del H50
h50<-gt_emory_data_arm2 %>% select(id, h50_date,h50_traditional ) %>% filter(!is.na(h50_date)) %>% filter(h50_traditional<="3") %>%  print()
##37
#datos H40, tienen al menos un dots
h40<-gt_emory_data_arm3 %>% 
  select(id,h40_date,h40_visit1, h40_dot1, h40_dot2, h40_dot3, h40_dot4,
         h40_visit2, h40_visit3, h40_visit4, 
         h40_countinue_dot1,h40_countinue_dot2, h40_countinue_dot3,
         h40_continue_dot4_v2,h40_countinue_dot2_v2, h40_countinue_dot3_v2) %>% 
  filter(!is.na(h40_date) & h40_visit1=="1") %>% print()

h40$seguimiento <- ifelse((h40$h40_continue_dot4_v2=="2" & !is.na(h40$h40_continue_dot4_v2)) | (h40$h40_countinue_dot2_v2=="2" & !is.na(h40$h40_countinue_dot2_v2)) | (h40$h40_countinue_dot3_v2=="2" & !is.na(h40$h40_countinue_dot3_v2)) , "No continua", "continua")

table(h40$seguimiento)

h40 %>% mutate(is.na(h40_countinue_dot1)="3")
#View(h40)

all_visit<-h40 %>% gather(
   h40_visit2,h40_visit3,h40_visit4,
  h40_continue_dot4_v2,h40_countinue_dot2_v2,h40_countinue_dot3_v2,
  -id, -h40_date,-h40_visit1 , 
  factor_key = TRUE
) %>% arrange(id,h40_date) %>%  print()

#View(all_visit)
h40_seguimiento <- gt_emory_data_arm3 %>% 
  select(id,h40_date,h40_visit1, h40_visit2, h40_visit3, h40_visit4, h40_countinue_dot1,h40_countinue_dot2, h40_countinue_dot3,h40_continue_dot4_v2,h40_countinue_dot2_v2, h40_countinue_dot3_v2) %>% 
  filter(!is.na(h40_date) & h40_visit1=="2")  %>% print()
  #View(h40_seguimiento)
  
  
  h40 %>% select(id, h40_date, h40_visit1, h40_dot1, h40_dot2, h40_dot3, h40_dot4) %>%  gather(
    key = variable, value=value, -id, -h40_date
  ) %>% mutate(
    id_dot=case_when(
      variable=="h40_dot1"  ~ value,
      variable=="h40_dot2"  ~ value,
      variable=="h40_dot3"  ~ value,
      variable=="h40_dot4"  ~ value,
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(id_dot))%>% select(id, h40_date, id_dot) %>% 
    write_csv("output/lista_dots_ids.csv")
    
  
  
  
  #& h40_visit1=="2"
#ids de Intervención 
s6<-gt_emory_data %>% select(id, s6_date, s6_arm) %>% filter(!is.na(s6_date) & s6_arm=="1") %>% print()

h40_arm<-h40 %>%  left_join(s6) %>% print()

h40_intervencion <- h40_arm %>% filter(s6_arm=="1") %>% print()

h40_control <- h40_arm %>% filter(is.na(s6_arm)) %>% print()

h40_control %>% writexl::write_xlsx(., "output/instalaciones_h40_control.xlsx")
h40_intervencion %>% writexl::write_xlsx(.,"output/h40_intervencion.xlsx")

#ids de Intervención que les falta el H50
m10<- gt_emory_data %>% select(id,m10_date,redcap_event_name, m10_sleep) %>% filter(!is.na(m10_date)) %>% print()

m17<-gt_emory_data %>% select(id_tamizaje=id,m17_date, m17_ga) %>% filter(!is.na(m17_date)) %>% print()

intervencion<- gt_emory_data %>% select(id,s6_date, s6_arm) %>% filter(!is.na(s6_date) & s6_arm=="1") %>% print()

tamizaje <- gt_emory_data %>% select(id_tamizaje=id, id=s4_main_id, s4_date,s1_community_name) %>% filter(!is.na(s4_date)) %>% print()

tamizaje_ga<- tamizaje %>% left_join(m17) %>% print()

inter_s4<-intervencion %>% left_join(tamizaje_ga) %>% print()

h50<-gt_emory_data %>% select(id,h50_date) %>% filter(!is.na(h50_date)) %>%  print()

inter_s4_h50<- inter_s4 %>% left_join(h50) %>% filter(is.na(h50_date)) %>% 
  print() 
#%>% write_csv("output/intervencion_sin_h50.csv")
e3<-gt_emory_data %>% select(id,e3_date) %>% filter(!is.na(e3_date)) %>% print()
h50_faltante<-inter_s4_h50 %>% left_join(e3) %>% left_join(m10 %>% select(id,m10_date,m10_sleep)
                                             ) %>% print() 

#agregar la edad gestacional
h50_export<- h50_faltante %>% mutate(
  fecha_concepcion = as.Date(m17_ga) - lubridate::days(280),
  edad_semanas = as.numeric(Sys.Date() - fecha_concepcion) %/%7,
  dias= as.numeric(Sys.Date()- fecha_concepcion) %%7,
  dias_ramdom= as.numeric(Sys.Date() - as.Date(s6_date))
)

h50_export %>% mutate(
              edad_gestacional = paste(edad_semanas,"s", dias, "d"),
              as.Date(m17_date),
              as.Date(s4_date)
              ) %>% 
              select("Id Hogar (S6)"=id, "Id Tamizaje (z10)"=id_tamizaje,
                                  "comunidad (s1)"=s1_community_name, "Consentimiento (S4)"=s4_date,
                                  "USG (m17)"=m17_date, "Personas por Hogar (m10)"=m10_sleep,
                                  "Edad gestacional (M17)"= edad_gestacional,
                                  "Fecha Randomizacion (6)" = s6_date,
                                  "Dias desde Randomizacion Fecha Actual" = dias_ramdom
              ) %>% print() %>% writexl::write_xlsx(paste0("output/Pendientes_instalacion_estufas_",Sys.Date(),".xlsx"))
  
  
 # write_csv(paste0("output/Pendientes_instalacion_estufas_", Sys.Date(), ".csv"))


View(h50_export)
s0 %>% select(record,s0_date) %>% arrange(desc(s0_date))%>% print()
#report_date = last_report_day,
#conception_date = fpp - lubridate::days(280),




#-------------------------------------------------------------------
#revisar casas con instalación de dots
#-------------------------------------------------------------------
#H40
dot_instalados1<-gt_emory_data_arm3 %>% filter(!is.na(h40_date) ) %>% select(
  id, visit, h40_date,  h40_dot1, h40_visit1, h40_dot2, h40_visit2, h40_dot3, 
    h40_visit3, h40_dot4, h40_visit4
) %>% gather(key = variable, value=value, -id, -h40_date, -visit) %>%  mutate(
  id_dot= case_when(
    variable=="h40_dot1" & !is.na(value) ~ value,
    variable=="h40_dot2" & !is.na(value) ~ value,
    variable=="h40_dot3" & !is.na(value) ~ value,
    variable=="h40_dot4" & !is.na(value) ~ value,
    TRUE ~ NA_character_
  )
) %>% filter(!is.na(id_dot)) %>%  select(id, h40_date, visit, id_dot) %>% left_join(

gt_emory_data_arm3 %>% filter(!is.na(h40_date) ) %>% select(
  id, visit, h40_date,  h40_dot1, h40_visit1, h40_dot2, h40_visit2, h40_dot3, 
  h40_visit3, h40_dot4, h40_visit4
) %>% gather(key = variable, value=value, -id, -h40_date, -visit) %>% 
 mutate(
  tipo_visita=case_when(
    variable=="h40_visit1" ~ value,
    variable=="h40_visit2" ~ value,
    variable=="h40_visit3" ~ value,
    variable=="h40_visit4" ~ value,
    TRUE ~ NA_character_
  )
) %>% filter(!is.na(tipo_visita)) %>% select(id, h40_date, visit, tipo_visita)
) %>% mutate(accion=recode(
  tipo_visita,"1"="Instalacion", "2"="Seguimiento" 
),
crf="H40"
) #%>% filter(id=="33173") %>% print()

#H40b
dot_instalados2<-gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2) ) %>% select(
  id, visit, h40_date_v2,  h40_dot1_v2, h40_visit1_v2, h40_dot2_v2, h40_visit2_v2, h40_dot3_v2, 
  h40_visit3_v2, h40_dot4_v2, h40_visit4_v2
) %>% gather(key = variable, value=value, -id, -h40_date_v2, -visit) %>%  mutate(
  id_dot= case_when(
    variable=="h40_dot1_v2" & !is.na(value) ~ value,
    variable=="h40_dot2_v2" & !is.na(value) ~ value,
    variable=="h40_dot3_v2" & !is.na(value) ~ value,
    variable=="h40_dot4_v2" & !is.na(value) ~ value,
    TRUE ~ NA_character_
  )
) %>% filter(!is.na(id_dot)) %>%  select(id, h40_date_v2, visit, id_dot) %>% left_join(
  
  gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2) ) %>% select(
    id, visit, h40_date_v2,  h40_dot1_v2, h40_visit1_v2, h40_dot2_v2, h40_visit2_v2, h40_dot3_v2, 
    h40_visit3_v2, h40_dot4_v2, h40_visit4_v2
  ) %>% gather(key = variable, value=value, -id, -h40_date_v2, -visit) %>% 
    mutate(
      tipo_visita=case_when(
        variable=="h40_visit1_v2" ~ value,
        variable=="h40_visit2_v2" ~ value,
        variable=="h40_visit3_v2" ~ value,
        variable=="h40_visit4_v2" ~ value,
        TRUE ~ NA_character_
      )
    ) %>% filter(!is.na(tipo_visita)) %>% select(id, h40_date_v2, visit, tipo_visita)
) %>% mutate(accion=recode(
  tipo_visita,"1"="Instalacion", "2"="Seguimiento" 
),
crf="H40_v2"
) %>% select(id, h40_date=h40_date_v2, visit, id_dot, tipo_visita, accion, crf) %>%  print()

#listar todos los Dots registrados con sus acciones(instalación o seguimiento)
all_dots<- dot_instalados1 %>% bind_rows(
  dot_instalados2
) %>% print()



gt_emory_data_arm3 %>% filter(!is.na(h40_date) ) %>% select(
  id, visit, h40_date,  h40_dot1, h40_visit1, h40_countinue_dot1, h40_stop_date1,
  h40_dot2, h40_visit2, h40_dot3, 
  h40_visit3, h40_dot4, h40_visit4
) %>%  gather(key = variable, value=value, -id, -h40_date, -visit) %>%  mutate(
  id_dot= case_when(
    variable=="h40_dot1" & !is.na(value) ~ value,
    variable=="h40_dot2" & !is.na(value) ~ value,
    variable=="h40_dot3" & !is.na(value) ~ value,
    variable=="h40_dot4" & !is.na(value) ~ value,
    TRUE ~ NA_character_
  )
) %>% filter(!is.na(id_dot)) %>%  select(id, h40_date, visit, id_dot) %>% left_join(
  
  gt_emory_data_arm3 %>% filter(!is.na(h40_date) ) %>% select(
    id, visit, h40_date,  h40_dot1, h40_visit1, h40_dot2, h40_visit2, h40_dot3, 
    h40_visit3, h40_dot4, h40_visit4
  ) %>% gather(key = variable, value=value, -id, -h40_date, -visit) %>% 
    mutate(
      tipo_visita=case_when(
        variable=="h40_visit1" ~ value,
        variable=="h40_visit2" ~ value,
        variable=="h40_visit3" ~ value,
        variable=="h40_visit4" ~ value,
        TRUE ~ NA_character_
      )
    ) %>% filter(!is.na(tipo_visita)) %>% select(id, h40_date, visit, tipo_visita)
) %>% mutate(accion=recode(
  tipo_visita,"1"="Instalacion", "2"="Seguimiento" 
),
crf="H40"
)


#Sacar lista de Dots que solo tienen instalación pero queno tienen seguimiento
all_dots %>% filter(tipo_visita=="1") %>% anti_join(
  all_dots %>% filter(tipo_visita=="2") %>% select(id,id_dot)
) %>% arrange(h40_date) %>% writexl::write_xlsx(paste0("output/dots_sin_seguimientos_al_",Sys.Date(),".xlsx"))
 


#Sacar lista de Dots que solo tienen seguimiento pero que no tienen instalación
all_dots %>% filter(tipo_visita=="2") %>% anti_join(
  all_dots %>% filter(tipo_visita=="1") %>% select(id,id_dot)
) %>% arrange(h40_date) %>% writexl::write_xlsx(paste0("output/dots_sin_instalacion_al_",Sys.Date(),".xlsx"))

all_dots %>% filter(id_dot=="1543")
all_dots %>% filter(id_dot=="1638")
all_dots %>% writexl::write_xlsx(paste0("output/all_instalations_dots_2",Sys.Date(),".xlsx"))

#revisar todos los registros de H40 en Redcap comparar contra datos de geocene periscope
data_geocene<-read_csv("data/geocene/MissionSummary_2021-1-7_1417.csv")

all_dots %>% left_join(
  data_geocene %>%  mutate(
    id_dot=if_else(
      substr(dot_name,1,4)=="Dot-" , substr(dot_name, 5, length(dot_name)), dot_name
    )
  ) 
) %>% write_csv("output/all_dots_and_geocene.csv")

#REVISAR CONTRA DATOS DEL DASHBOARD
instalacion_dots<-all_dots %>% filter(tipo_visita=="1") %>% anti_join(
  all_dots %>% filter(tipo_visita=="2") %>% select(id,id_dot)
)

datos_geocene<-read_csv("D:/Descargas/LastKnownStatusofDots_2020-3-12_0209.csv")
datos_geocene %>% select(id_dot=dot_name, latest_campaign, last_update) %>% left_join(
  instalacion_dots
) %>% filter(!is.na(h40_date))


#REPORTE DE DOTS ADIAZ, REVISIOIN DE COLOCACIÓN DE DOTS
#Leer datos del Estudio intensivo
intensivo_data<-read_csv("data/HAPINGuatemalaExposu_DATA_2020-06-16_0153.csv")
intensivo_data<-intensivo_data %>% mutate_all(as.character)

#armar dataset para analizar
#hogares aleatorizados
data_control_intervencion_intensivo<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% left_join(
  #revision de instalación de estufa
  # 1=La misma cocina donde se encuentra la estufa de gas
  # 2=Cuarto diferente de la estufa de gas
  # 3=Afuera
  # 4=Almacenado
  # 5=Destruido
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id,h50_traditional )
) %>% left_join(
  #Identificar a los hogares que participan en intensivo
  intensivo_data %>% group_by(record_id) %>% count() %>% ungroup() %>% transmute(id=record_id,intensivo="Si" )
)

#dataframe dots
#h40_visit: 1=Instalacion inicial
#           2=Seguimiento de un instrumento instalado
#H40_stove:
# 1=Abierto/Fuego 3 piedras,Chula de Metal/Barro, poyo o plancha, fogon
# 2=Estufa de biomasa/Plancha con chimenea, Imbabura, Cocina mejorada
# 3=Rondereza
# 4=Estufa portatil (de lena)
# 5=Queroseno
# 6=Estufa de gas
# 7=Electrica
# 8=Comal
# 555=Otra

#h40_primer_dot
dt_dots<-
gt_emory_data_arm3 %>%  filter(!is.na(h40_date)) %>% select(id, h40_date, h40_stove1, h40_dot1, h40_visit1
) %>% mutate( id_dot=h40_dot1, tipo_visita=h40_visit1, tipo_estufa=h40_stove1) %>% select(id, h40_date, id_dot, tipo_visita, tipo_estufa) %>% bind_rows(
 #h40 segundo dot
   gt_emory_data_arm3 %>%  filter(!is.na(h40_date)) %>% select(id, h40_date, h40_stove2, h40_dot2, h40_visit2,
  ) %>% mutate( id_dot=h40_dot2, tipo_visita=h40_visit2, tipo_estufa=h40_stove2) %>% select(id, h40_date, id_dot, tipo_visita, tipo_estufa)
) %>% bind_rows(
  #h40 tercer dot
  gt_emory_data_arm3 %>%  filter(!is.na(h40_date)) %>% select(id, h40_date, h40_stove3, h40_dot3, h40_visit3
  ) %>% mutate( id_dot=h40_dot3, tipo_visita=h40_visit3, tipo_estufa=h40_stove3) %>% select(id, h40_date, id_dot, tipo_visita, tipo_estufa)
) %>% bind_rows(
  #h40 cuarto dot
  gt_emory_data_arm3 %>%  filter(!is.na(h40_date)) %>% select(id, h40_date, h40_stove4, h40_dot4, h40_visit4
  ) %>% mutate( id_dot=h40_dot4, tipo_visita=h40_visit4, tipo_estufa=h40_stove4) %>% select(id, h40_date, id_dot, tipo_visita, tipo_estufa)
) %>% #unificar visita 1 y Visita 2
  bind_rows(

#H40_visita2
gt_emory_data_arm3 %>% filter(!is.na(h40_date_v2)) %>% select(id, h40_date_v2, h40_stove1_v2, h40_dot1_v2, h40_visit1_v2) %>%
  mutate( id_dot=h40_dot1_v2, tipo_visita=h40_visit1_v2, tipo_estufa=h40_stove1_v2) %>% select(id, h40_date=h40_date_v2, id_dot, tipo_visita, tipo_estufa) %>% 
  bind_rows(
  #h40_vista2 segundo dot
  gt_emory_data_arm3 %>%  filter(!is.na(h40_date_v2)) %>% select(id, h40_date_v2, h40_stove2_v2, h40_dot2_v2, h40_visit2_v2,
  ) %>% mutate( id_dot=h40_dot2_v2, tipo_visita=h40_visit2_v2, tipo_estufa=h40_stove2_v2) %>% select(id, h40_date=h40_date_v2, id_dot, tipo_visita, tipo_estufa)
) %>% bind_rows(
  #h40_vista2 tercer dot
  gt_emory_data_arm3 %>%  filter(!is.na(h40_date_v2)) %>% select(id, h40_date_v2, h40_stove3_v2, h40_dot3_v2, h40_visit3_v2,
  ) %>% mutate( id_dot=h40_dot3_v2, tipo_visita=h40_visit3_v2, tipo_estufa=h40_stove3_v2) %>% select(id, h40_date=h40_date_v2, id_dot, tipo_visita, tipo_estufa)
) %>% bind_rows(
  #h40_vista2 cuarto dot
  gt_emory_data_arm3 %>%  filter(!is.na(h40_date_v2)) %>% select(id, h40_date_v2, h40_stove4_v2, h40_dot4_v2, h40_visit4_v2,
  ) %>% mutate( id_dot=h40_dot4_v2, tipo_visita=h40_visit4_v2, tipo_estufa=h40_stove4_v2) %>% select(id, h40_date=h40_date_v2, id_dot, tipo_visita, tipo_estufa)
)

) %>% print()

#REPORTES
#Hogares Intervención que tienen polletón y que no tienen Instalación de Dot
data_control_intervencion_intensivo %>% filter(brazo=="1") %>% left_join(
  dt_dots %>% filter(tipo_visita=="1") %>% group_by(id) %>% count() %>% ungroup() %>% select(id, instalaciones_dots=n)
) %>% 
  mutate(
  h50_traditional=recode(h50_traditional,
                         "1"="La misma cocina donde se encuentra la estufa de gas",
                         "2"="Cuarto diferente de la estufa de gas",
                         "3"="Afuera",
                         "4"="Almacenado",
                         "5"="Destruido"   )
) %>% filter(h50_traditional!="Destruido") %>% filter(h50_traditional!="Almacenado") %>% arrange(instalaciones_dots) %>% print()

#hogares intervención e intensivo
data_control_intervencion_intensivo %>% filter(brazo=="1") %>% filter(!is.na(intensivo)) %>% 
  left_join(
    dt_dots %>% filter(tipo_visita=="1")
  ) %>% filter(is.na(id_dot))


#Hogares control
data_control_intervencion_intensivo %>% filter(brazo=="0") %>% left_join(
  dt_dots %>%  mutate(tipo_estufa=recode(tipo_estufa,
                                                                              "1"="Abierto/Fuego 3 piedras,Chula de Metal/Barro, poyo o plancha, fogon",
                                                                              "2"="Estufa de biomasa/Plancha con chimenea, Imbabura, Cocina mejorada",
                                                                              "3"="Rondereza",
                                                                              "4"="Estufa portatil (de lena)",
                                                                              "5"="Queroseno",
                                                                              "6"="Estufa de gas",
                                                                              "7"="Electrica",
                                                                              "8"="Comal",
                                                                              "555"="Otra"  
                                                                              )
                                                           ) %>% 
    group_by(id, id_dot, tipo_estufa) %>% count()
) %>% filter(intensivo=="Si") %>% filter(!is.na(tipo_estufa)) %>% group_by(id, tipo_estufa) %>% summarize() %>% write_csv("output/control_intensivo.csv")

#cantidad de Dots por brazo
dt_dots %>% left_join(
  data_control_intervencion_intensivo %>% select(id, brazo)
) %>% mutate(tipo_estufa=recode(tipo_estufa,
                                "1"="Abierto/Fuego 3 piedras,Chula de Metal/Barro, poyo o plancha, fogon",
                                "2"="Estufa de biomasa/Plancha con chimenea, Imbabura, Cocina mejorada",
                                "3"="Rondereza",
                                "4"="Estufa portatil (de lena)",
                                "5"="Queroseno",
                                "6"="Estufa de gas",
                                "7"="Electrica",
                                "8"="Comal",
                                "555"="Otra"  
)
) %>% mutate(
  brazo=recode(brazo, "1"="Intervencion", "0"="Control")
) %>% write_csv("output/dot_brazo.csv")

#DOTs sin seguimiento
dt_dots %>% filter(tipo_visita=="1") %>% select(id_dot) %>% anti_join(
  dt_dots %>% filter(tipo_visita=="2") %>% select(id_dot)
) %>% left_join(dt_dots %>% filter(tipo_visita=="1")
) %>% write_csv("output/Revision_DOTs_Sin_Seguimiento.csv")

#DOTs sin instalacion
dt_dots %>% filter(tipo_visita=="2") %>% select(id_dot) %>% anti_join(
  dt_dots %>% filter(tipo_visita=="1") %>% select(id_dot)
) %>% left_join(dt_dots %>% filter(tipo_visita=="2")) %>% group_by(id_dot, id, h40_date) %>% count() %>% arrange(id, h40_date) %>% 
  select(id_dot, id, h40_date) %>% write_csv("output/Revision_DOTs_Sin_Instalacion.csv")

dt_dots %>% group_by(tipo_estufa) %>% count()
dt_dots %>% filter(!is.na(tipo_estufa))%>% filter(tipo_estufa=="3")

#REVISION DE DOTS PARA 2021 CON ALEJANDRO

dt_periscope<-read_csv("c:/temp/data_dots/MissionSummary_2021-1-13_1019.csv")
dt_inventario_dots<-read_csv("c:/temp/data_dots/inventario_dot_unique.csv")

lista_dot_periscope<-dt_periscope %>% select(hhid, mission_name, dot_name, latest_download) %>% mutate(
  id_dot=case_when(
    substr(dot_name, 1,3)=="Dot" | substr(dot_name, 1,3)=="dot" ~  substr(dot_name, 5, length(dot_name)), 
    substr(dot_name,1,3)=="fil" ~  substr(mission_name, 5,8),
    TRUE ~ dot_name
  )
  ) %>% filter(!is.na(id_dot))

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% transmute(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
) %>% select(id_tamizaje, id, brazo=s6_arm, fecha_nacimiento) %>% mutate(
  inicio_ventana_b4= fecha_nacimiento + lubridate::days(337),
  cumple_anio= fecha_nacimiento + lubridate::days(365),
  fin_ventana_b4= fecha_nacimiento + lubridate::days(393),
) %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% filter(visit=="b4") %>% select(
    id, h41_date
  )
) %>% filter(is.na(h41_date)) %>% #filter(as.Date(cumple_anio)>="2021-02-20") %>% 
  select(
  id_tamizaje, id, brazo, fecha_nacimiento
) %>% left_join(
  lista_dot_periscope %>% transmute(id=hhid, id_dot, latest_download)
) %>% filter(!is.na(id_dot))  %>% write_csv("output/dots_activos_13-01-2021.csv")

lista_dot_periscope %>% mutate_all(as.character) %>%  select(hhid, latest_download, id_dot) %>% left_join(
  dt_inventario_dots %>% mutate_all(as.character) %>%  select(id_dot, estado)
) %>% write_csv("output/periscope_inventario.csv")

dt_inventario_dots %>% left_join(
  lista_dot_periscope %>% select(hhid, latest_download, id_dot) %>% distinct(id_dot)
) %>% write_csv("output/revision_inventario.csv")


  



dt_inventario_dots %>%filter(!is.na(id_dot)) %>%  mutate(
  id_dot=as.numeric(id_dot)
) %>% anti_join(
  lista_dot_periscope %>% mutate(id_dot=as.numeric(id_dot)) %>% distinct(id_dot)
) %>% write_csv("output/inventario_no_periscope.csv")


periscope_no_inventario<-lista_dot_periscope %>% filter(!is.na(id_dot)) %>% mutate(id_dot=as.numeric(id_dot)) %>%
  distinct(id_dot) %>% anti_join(
  dt_inventario_dots %>%filter(!is.na(id_dot)) %>%  mutate(
    id_dot=as.numeric(id_dot)
  )
)

periscope_no_inventario %>% left_join(
  lista_dot_periscope %>% mutate(id_dot=as.numeric(id_dot))
) %>% write_csv("output/misiones_no_inventario.csv")


#datos de hapin testing
hapin_testing<-read_csv("c:/temp/data_dots/hapin_testing.csv")
hapin_testing %>% mutate_all(as.character) %>%  mutate(
  id_dot=case_when(
    substr(dot_name, 1,3)=="Dot" | substr(dot_name, 1,3)=="dot" ~  substr(dot_name, 5, length(dot_name)), 
    substr(dot_name,1,3)=="fil" ~  substr(mission_name, 5,8),
    TRUE ~ dot_name
  )
) %>% transmute(hhid, mission_name, dot_name, latest_download, id_dot, origen="hapin_testing"  ) %>% filter(!is.na(id_dot))

#integramos todas las misiones de hapin_guatemala y hapin_testing
all_mission<-lista_dot_periscope %>% mutate(origen="hapin_guatemala") %>% mutate_all(as.character) %>% bind_rows(
  hapin_testing %>% mutate_all(as.character) %>%  mutate(
    id_dot=case_when(
      substr(dot_name, 1,3)=="Dot" | substr(dot_name, 1,3)=="dot" ~  substr(dot_name, 5, length(dot_name)), 
      substr(dot_name,1,3)=="fil" ~  substr(mission_name, 5,8),
      TRUE ~ dot_name
    )
  ) %>% transmute(hhid, mission_name, dot_name, latest_download, id_dot, origen="hapin_testing"  ) %>% 
    filter(!is.na(id_dot)) %>% mutate_all(as.character)
) %>% mutate(id_dot=as.numeric(id_dot))

#sacar los dots que estan en inventario pero no aparecen en misiones
dt_inventario_dots %>% mutate(id_dot=as.numeric(id_dot)) %>% anti_join(
  all_mission %>% distinct(id_dot)
) %>% write_csv("output/inventario_no_misiones.csv")

all_mission_no_inventario<-all_mission %>% distinct(id_dot) %>% anti_join(
  dt_inventario_dots %>% mutate(id_dot=as.numeric(id_dot))
)

#sacar los dots que estan en misiones pero no aparecen en inventario
all_mission_no_inventario %>% left_join(
  all_mission
) %>% write_csv("output/all_mission_no_inventario.csv")


#sacar listado actualizado de dots que deben quedar despues de marzo
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% transmute(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fecha_nacimiento=if_else(!is.na(c30_dob), as.Date(c30_dob), as.Date(m17_ga))
) %>% select(id_tamizaje, id, brazo=s6_arm, fecha_nacimiento) %>% mutate(
  inicio_ventana_b4= fecha_nacimiento + lubridate::days(337),
  cumple_anio= fecha_nacimiento + lubridate::days(365),
  fin_ventana_b4= fecha_nacimiento + lubridate::days(393),
) %>%  left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% filter(visit=="b4") %>% select(
    id, h41_date
  )
) %>%   #filter(is.na(h41_date)) %>% 
  filter(as.Date(cumple_anio)>="2021-02-20") %>% 
  select(
    id_tamizaje, id, brazo, fecha_nacimiento
  ) %>% left_join(
    all_mission %>% transmute(id=hhid, id_dot, latest_download) %>% 
      left_join(
        dt_status_mission %>% mutate(id_dot=as.numeric(id_dot))
      ) %>% filter(mission_status=="Running" | is.na(mission_status)) 
  ) %>% filter(!is.na(id_dot)) %>%  write_csv("output/dots_activos_13-01-2021_revision.csv")



estado_misiones<-read_csv("c:/temp/data_dots/LastKnownStatusofDots_2021-1-13_0432.csv")

dt_status_mission<-estado_misiones %>% mutate(
  id_dot=case_when(
    substr(dot_name, 1,3)=="Dot" | substr(dot_name, 1,3)=="dot" ~  substr(dot_name, 5, length(dot_name)), 
    substr(dot_name,1,3)=="fil" | substr(dot_name,1,3)=="Fil"  ~  "999999",
    TRUE ~ dot_name
  )
) %>% select(id_dot, dot_name, last_update, mission_status, 
             latest_campaign, latest_latitude, latest_longitude) %>% mutate(
               id_dot=as.numeric(id_dot)
             )


