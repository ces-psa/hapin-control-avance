#librerias
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
dt_z10_completo<-read_csv("data/exports/z10_edad_madre.csv")

dt_fetp<-read_csv("data/fetp/HAPINGuatemalaMainSt-Revisionvariables_DATA_2023-03-15_0105.csv")

#datos c35 completo
dt_c35<-read_csv("data/exports/HAPINGuatemalaMainSt-C35_DATA_2023-02-23_1624.csv")
dt_c35<-dt_c35 %>% mutate(id=as.character(id))

#agergar visita a c31
dt_c35<-dt_c35 %>% filter(!is.na(c35_date)) %>% mutate(
  visita=case_when(
    redcap_event_name=="b1_arm_2" ~ "b1",
    redcap_event_name=="b3_arm_2" ~ "b3",
    redcap_event_name=="b2_arm_2" ~ "b2",
    redcap_event_name=="b4_arm_2" ~ "b4",
    redcap_event_name=="birth_arm_2" ~ "birth"
  )
)


#anomalias congenitas
anomalias_e1<-gt_emory_data_arm2 %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_category___5, redcap_event_name) %>% 
  filter(e1_category___5=="1")

anomalias_e2<-gt_emory_data_arm2 %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_type, redcap_event_name) %>% 
  filter(e2_type=="6") 

anomalias_c30<-gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>%  select(id, c30_date, c30_problem_type___13) %>% 
  filter(c30_problem_type___13=="1")

anomalias_e2 %>% filter(!(id %in% anomalias_c30$id))

dt_anomalias_hapin<-anomalias_e1 %>% transmute(id, origen="E1", fecha=e1_date) %>% bind_rows(
  anomalias_e2 %>% transmute(id, origen="E2", fecha=e2_date)
) %>% bind_rows(
   anomalias_c30 %>% transmute(id, origen="C30", fecha=c30_date)
) %>% arrange(id)
#writexl::write_xlsx("output/anomalias_hapin.xlsx")

anomalias<-dt_anomalias_hapin %>% distinct(id) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b5") %>%  filter(!is.na(s4_date)) %>% transmute(id, s4_date, consintio=recode(
    s4_consent_c,"1"="Si", "0"="No"
  )
  )
  
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, fecha_c35_24m=c35_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(visit=="b6") %>% filter(!is.na(s4_date)) %>% transmute(
    id, s4_36m=s4_date, consintio_36m=recode(s4_consent_c,"1"="Si", "0"="No")
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c35_date_2)) %>% select(id, fecha_c35_36m=c35_date_2)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, ultima_visita_madre=e3_last_visit)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_c)) %>% select(id, ultima_visita_nino=e3_last_visit_c)
) %>% mutate(
  ultima_visita_madre=recode(ultima_visita_madre,"3"="P2","4"="Birth","5"="B1","6"="b3","7"="B3","8"="B4"),
  ultima_visita_nino=recode(ultima_visita_nino,"4"="Birth","5"="B1","6"="b3","7"="B3","8"="B4"),
)

#sacar set de variables por visita ----
## b1----
c35_b1<-dt_c35 %>% filter(!is.na(c35_date)) %>% filter(visita=="b1") %>% select(
  id, visita, c35_move, c35_bring, c35_grasp, c35_hands, c35_fist, c35_interest,
  c35_laugh, c35_smile, c35_suck, c35_recognize
)
#agregar prefijo de visita
colnames(c35_b1) <- paste("b1",colnames(c35_b1),sep="_")
#renombrar el ID
c35_b1<-c35_b1 %>% rename(.,id=b1_id)

## b3----
c35_b3<-dt_c35 %>% filter(!is.na(c35_date)) %>% filter(visita=="b3") %>% select(
  id, visita, c35_move, c35_bring, c35_grasp, c35_hands, c35_fist, c35_interest,
  c35_laugh, c35_smile, c35_suck, c35_recognize
)
#agregar prefijo de visita
colnames(c35_b3) <- paste("b3",colnames(c35_b3),sep="_")
#renombrar el ID
c35_b3<-c35_b3 %>% rename(.,id=b3_id)

## b3----
c35_b3<-dt_c35 %>% filter(!is.na(c35_date)) %>% filter(visita=="b3") %>% select(
  id, visita, c35_move, c35_bring, c35_grasp, c35_hands, c35_fist, c35_interest,
  c35_laugh, c35_smile, c35_suck, c35_recognize
)
#agregar prefijo de visita
colnames(c35_b3) <- paste("b3",colnames(c35_b3),sep="_")
#renombrar el ID
c35_b3<-c35_b3 %>% rename(.,id=b3_id)

## b4----
c35_b4<-dt_c35 %>% filter(!is.na(c35_date)) %>% filter(visita=="b4") %>% select(
  id, visita, c35_move, c35_bring, c35_grasp, c35_hands, c35_fist, c35_interest,
  c35_laugh, c35_smile, c35_suck, c35_recognize
)
#agregar prefijo de visita
colnames(c35_b4) <- paste("b4",colnames(c35_b4),sep="_")
#renombrar el ID
c35_b4<-c35_b4 %>% rename(.,id=b4_id)

#integar variables
c35_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
   c35_b1 
) %>% left_join(
  c35_b3
) %>% left_join(
  c35_b3
) %>% left_join(
  c35_b4
) 


#Sacar datos de E2 ----
## sacar las variables de E2 ----
dt_e2<-dt_fetp %>% filter(!is.na(e2_date)) %>%  select(id,redcap_event_name,
      e2_title, e2_type, e2_start_date, e2_continuing, e2_sae_nature, e2_outcome)
### agregar visita ----
dt_e2<-dt_e2 %>% mutate(visita=case_when(
  redcap_event_name=="segun_sea_necesari_arm_2" ~ "ss1",
  redcap_event_name=="segun_sea_necesari_arm_2b" ~ "ss2",
  redcap_event_name=="segun_sea_necesari_arm_2c" ~ "ss3",
  redcap_event_name=="segun_sea_necesari_arm_2d" ~ "ss4",
  redcap_event_name=="segun_sea_necesari_arm_2e" ~ "ss5",
  
)) %>% select(-redcap_event_name)

e2_ss1<- dt_e2 %>% filter(visita=="ss1")

##agregar prefijo de visita----
colnames(e2_ss1) <- paste("ss1",colnames(e2_ss1),sep="_")
##renombrar el ID ----
e2_ss1<-e2_ss1 %>% rename(.,id=ss1_id)


## Agregar identificador de visita ----
e2_ss2<- dt_e2 %>% filter(visita=="ss2")

##agregar prefijo de visita----
colnames(e2_ss2) <- paste("ss2",colnames(e2_ss2),sep="_")
##renombrar el ID ----
e2_ss2<-e2_ss2 %>% rename(.,id=ss2_id)

## Agregar identificador de visita ----
e2_ss3<- dt_e2 %>% filter(visita=="ss3")

##agregar prefijo de visita----
colnames(e2_ss3) <- paste("ss3",colnames(e2_ss3),sep="_")
##renombrar el ID ----
e2_ss3<-e2_ss3 %>% rename(.,id=ss3_id)


## Agregar identificador de visita ----
e2_ss4<- dt_e2 %>% filter(visita=="ss4")

##agregar prefijo de visita----
colnames(e2_ss4) <- paste("ss4",colnames(e2_ss4),sep="_")
##renombrar el ID ----
e2_ss4<-e2_ss4 %>% rename(.,id=ss4_id)

## Agregar identificador de visita ----
e2_ss5<- dt_e2 %>% filter(visita=="ss5")

##agregar prefijo de visita----
colnames(e2_ss5) <- paste("ss5",colnames(e2_ss5),sep="_")
##renombrar el ID ----
e2_ss5<-e2_ss5 %>% rename(.,id=ss5_id)

e2_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  e2_ss1
) %>% left_join(
  e2_ss2
) %>% left_join(
  e2_ss3
) %>% left_join(
  e2_ss4
) %>% left_join(
  e2_ss5
)

# Sacar varialbes de E3 solicitadas -----
dt_e3<-dt_fetp %>% filter(!is.na(e3_date)) %>% select(id, redcap_event_name, 
                  e3_date_exit, e3_date_final, e3_reason, e3_voluntary___1, e3_voluntary___2,
                  e3_voluntary___3, e3_voluntary___4, e3_voluntary___5, #e3_voluntary_o___555,
                  e3_team, e3_randomiz, e3_last_visit
                                               )
##agregar visita----
dt_e3<-dt_e3 %>% select(-redcap_event_name) %>% mutate(visita="salida")

##agregar prefijo a variables e3----
e3_salida<- dt_e3 

##agregar prefijo de visita----
colnames(e3_salida) <- paste("salida",colnames(e3_salida),sep="_")
##renombrar el ID ----
e3_salida<-e3_salida %>% rename(.,id=salida_id)

#variables E3 ----
e3_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  e3_salida 
)

#varibles C33 -----
dt_c33<-dt_fetp %>% filter(!is.na(c33_date)) %>% select(id, redcap_event_name,
  c33_wt1_time, c33_wt1, c33_wt2_time, c33_wt2, c33_wt3_time, c33_wt3, c33_cloth,
  c33_cloth_wt, c33_ave_wt, c33_ave_wt2, c33_ht1_time, c33_ht1, c33_ht2_time, 
  c33_ht2, c33_ht3_time, c33_ht3, c33_ave_ht, c33_head1_time, c33_head1, c33_head2_time,
  c33_head2, c33_head3_time, c33_head3, c33_malnutrition, c33_heart
)

#separar c33 en visitas ----
dt_c33<-dt_c33 %>% mutate(
  visita=case_when(
    redcap_event_name=="b1_arm_2" ~ "b1",
    redcap_event_name=="b2_arm_2" ~ "b2",
    redcap_event_name=="b3_arm_2" ~ "b3",
    redcap_event_name=="b4_arm_2" ~ "b4",
    redcap_event_name=="birth_arm_2" ~ "birth"
  )
) %>% select(-redcap_event_name) 

##filtrar visitas b1
c33_b1<-dt_c33 %>% filter(visita=="b1")
### agregar prefijo
##agregar prefijo de visita----
colnames(c33_b1) <- paste("b1",colnames(c33_b1),sep="_")
##renombrar el ID ----
c33_b1<-c33_b1 %>% rename(.,id=b1_id)


##filtrar visitas b2
c33_b2<-dt_c33 %>% filter(visita=="b2")
### agregar prefijo
##agregar prefijo de visita----
colnames(c33_b2) <- paste("b2",colnames(c33_b2),sep="_")
##renombrar el ID ----
c33_b2<-c33_b2 %>% rename(.,id=b2_id)

##filtrar visitas b3
c33_b3<-dt_c33 %>% filter(visita=="b3")
### agregar prefijo
##agregar prefijo de visita----
colnames(c33_b3) <- paste("b3",colnames(c33_b3),sep="_")
##renombrar el ID ----
c33_b3<-c33_b3 %>% rename(.,id=b3_id)

##filtrar visitas b4
c33_b4<-dt_c33 %>% filter(visita=="b4")
### agregar prefijo
##agregar prefijo de visita----
colnames(c33_b4) <- paste("b4",colnames(c33_b4),sep="_")
##renombrar el ID ----
c33_b4<-c33_b4 %>% rename(.,id=b4_id)


c33_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  c33_b1
) %>% left_join(
  c33_b2
) %>% left_join(
  c33_b3
) %>% left_join(
  c33_b4
)

#varibales m17 ----
dt_m17<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% left_join(
  dt_fetp %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id,m17_abnorm)
) %>% mutate(visita="elegibilidad")

##agregar prefijo de visita----
colnames(dt_m17) <- paste("tamizaje",colnames(dt_m17),sep="_")
##renombrar el ID ----
dt_m17<-dt_m17 %>% rename(.,id=tamizaje_id)

## integrar variables m17 y visita ----
m17_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  dt_m17
)

#integrar variables de C30 ---
# variables de c30-----
dt_c30<-dt_fetp %>% filter(!is.na(c30_date)) %>% select(id, c30_sex, c30_age, c30_delivery, c30_cry,
                                                c30_icu,c30_problem_type___1,
                            c30_problem_type___2, c30_problem_type___3, c30_problem_type___4, c30_problem_type___5,
                            c30_problem_type___6, c30_problem_type___7, c30_problem_type___8, c30_problem_type___9,
                            c30_problem_type___10, c30_problem_type___11, c30_problem_type___12, c30_problem_type___13,
                            c30_problem_type___14, c30_problem_type___555, c30_wt_record) %>% mutate(visita="birth") 


##agregar prefijo de visita----
colnames(dt_c30) <- paste("birth",colnames(dt_c30),sep="_")
##renombrar el ID ----
dt_c30<-dt_c30 %>% rename(.,id=birth_id)

## integrar variables m17 y visita ----
c30_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  dt_c30
)


#integrar variables m13
dt_m13<-dt_fetp %>% filter(!is.na(m13_date)) %>% select(id, redcap_event_name, m13_pregnancy1, m13_preg_num, m13_abortion,
                                                m13_reffer, m13_unit_2___1, m13_unit_2___555
                                                ) %>% mutate(
                    visita=case_when( 
                      redcap_event_name=="linea_de_base_arm_2" ~ "lb",
                      redcap_event_name=="p1_arm_2" ~ "p1",
                      redcap_event_name=="p2_arm_2" ~ "p2"
                    )
                                                ) %>% select(-redcap_event_name)
## sacar las visitas m13
m13_lb<- dt_m13 %>% filter(visita=="lb")

##agregar prefijo de visita----
colnames(m13_lb) <- paste("lb",colnames(m13_lb),sep="_")
##renombrar el ID ----
m13_lb<-m13_lb %>% rename(.,id=lb_id)

## sacar las visitas m13
m13_p1<- dt_m13 %>% filter(visita=="p1")

##agregar prefijo de visita----
colnames(m13_p1) <- paste("p1",colnames(m13_p1),sep="_")
##renombrar el ID ----
m13_p1<-m13_p1 %>% rename(.,id=p1_id)

## sacar las visitas m13
m13_p2<- dt_m13 %>% filter(visita=="p2")

##agregar prefijo de visita----
colnames(m13_p2) <- paste("p2",colnames(m13_p2),sep="_")
##renombrar el ID ----
m13_p2<-m13_p2 %>% rename(.,id=p2_id)

## integrar variables m17 y visita ----
m13_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  m13_lb
) %>% left_join(
  m13_p1
) %>% left_join(
  m13_p2
)

#Sacar variables de m14a ----
dt_m14a<-dt_fetp %>% filter(!is.na(m14a_date)) %>% select(id, redcap_event_name, m14_wt1_time,
                        m14_wt1, m14_wt2_time, m14_wt2, m14_wt3, m14_wt_comp, m14_wt_incomp,
                        m14_ht1_time, m14_ht1, m14_ht2_time, m14_ht2, m14_ht3, m14_ht_comp, m14_ht_incomp
                        ) %>% mutate(  visita=case_when( redcap_event_name=="linea_de_base_arm_2" ~ "lb",
                                                     redcap_event_name=="p1_arm_2" ~ "p1",
                                                     redcap_event_name=="p2_arm_2" ~ "p2"  )
                        ) %>% select(-redcap_event_name)

## separar visitas ----
m14a_lb<-dt_m14a %>% filter(visita=="lb")# %>% select(-redcap_event_name)

##agregar prefijo de visita----
colnames(m14a_lb) <- paste("lb",colnames(m14a_lb),sep="_")
##renombrar el ID ----
m14a_lb<-m14a_lb %>% rename(.,id=lb_id)

## separar visitas ----
m14a_p1<-dt_m14a %>% filter(visita=="p1") #%>% select(-redcap_event_name)

##agregar prefijo de visita----
colnames(m14a_p1) <- paste("p1",colnames(m14a_p1),sep="_")
##renombrar el ID ----
m14a_p1<-m14a_p1 %>% rename(.,id=p1_id)


## separar visitas ----
m14a_p2<-dt_m14a %>% filter(visita=="p2")# %>% select(-redcap_event_name)

##agregar prefijo de visita----
colnames(m14a_p2) <- paste("p2",colnames(m14a_p2),sep="_")
##renombrar el ID ----
m14a_p2<-m14a_p2 %>% rename(.,id=p2_id)

## integrar variables m14a y visita ----
m14a_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  m14a_lb
) %>% left_join(
  m14a_p1
) %>% left_join(
  m14a_p2
)

# variables m14b ----
dt_m14b<-dt_fetp %>% filter(!is.na(m14b_date)) %>% select(id, redcap_event_name, 
                    m10_after_birth, m14_sbp1, m14_dbp1, m14_sbp2, m14_dbp2
                    ) %>% mutate(
                      visita=case_when(
                        redcap_event_name=="linea_de_base_arm_2" ~ "lb",
                        redcap_event_name=="p1_arm_2" ~ "p1",
                        redcap_event_name=="p2_arm_2" ~ "p2" ,
                        redcap_event_name=="b1_arm_2" ~ "b1",
                        redcap_event_name=="b2_arm_2" ~ "b2",
                        redcap_event_name=="b3_arm_2" ~ "b3",
                        redcap_event_name=="b4_arm_2" ~ "b4",
                        redcap_event_name=="birth_arm_2" ~ "birth",
                    )
                    ) %>% select(-redcap_event_name)

# separar en visitas ----
### linea basal ----
m14b_lb<-dt_m14b %>% filter(visita=="lb") 
###agregar prefijo de visita----
colnames(m14b_lb) <- paste("lb",colnames(m14b_lb),sep="_")
###renombrar el ID ----
m14b_lb<-m14b_lb %>% rename(.,id=lb_id)

# separar en visitas ----
### linea basal ----
m14b_p1<-dt_m14b %>% filter(visita=="p1") 
###agregar prefijo de visita----
colnames(m14b_p1) <- paste("p1",colnames(m14b_p1),sep="_")
###renombrar el ID ----
m14b_p1<-m14b_p1 %>% rename(.,id=p1_id)

# separar en visitas ----
### linea basal ----
m14b_p2<-dt_m14b %>% filter(visita=="p2") 
###agregar prefijo de visita----
colnames(m14b_p2) <- paste("p2",colnames(m14b_p2),sep="_")
###renombrar el ID ----
m14b_p2<-m14b_p2 %>% rename(.,id=p2_id)

# separar en visitas ----
### linea basal ----
m14b_b1<-dt_m14b %>% filter(visita=="b1") 
###agregar prefijo de visita----
colnames(m14b_b1) <- paste("b1",colnames(m14b_b1),sep="_")
###renombrar el ID ----
m14b_b1<-m14b_b1 %>% rename(.,id=b1_id)

# separar en visitas ----
### linea basal ----
m14b_b2<-dt_m14b %>% filter(visita=="b2") 
###agregar prefijo de visita----
colnames(m14b_b2) <- paste("b2",colnames(m14b_b2),sep="_")
###renombrar el ID ----
m14b_b2<-m14b_b2 %>% rename(.,id=b2_id)

# separar en visitas ----
### linea basal ----
m14b_b4<-dt_m14b %>% filter(visita=="b4") 
###agregar prefijo de visita----
colnames(m14b_b4) <- paste("b4",colnames(m14b_b4),sep="_")
###renombrar el ID ----
m14b_b4<-m14b_b4 %>% rename(.,id=b4_id)

## integrar variables m14b y visita ----
m14b_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  m14b_lb
) %>% left_join(
  m14b_p1
) %>% left_join(
  m14b_p2
) %>% left_join(
  m14b_b1
) %>% left_join(
  m14b_b2
) %>% left_join(
  m14b_b4
)

#sacar varialbes de M19 ----
dt_m19<-dt_fetp %>% filter(!is.na(m19_date)) %>% select(id, redcap_event_name, m19_c_med_cost, m19_occup___1,
                            m19_occup___2, m19_occup___3, m19_occup___4, m19_occup___5, m19_occup___6, m19_occup___7,
                            m19_occup___8, m19_occup___9, m19_occup___10, m19_occup___11, m19_occup___12,
                            m19_occup___13, m19_occup___14, m19_occup___15, m19_occup___555, m19_payment_month) %>% mutate(
                             visita=case_when(
                               redcap_event_name=="linea_de_base_arm_2" ~ "lb",
                               redcap_event_name=="b4_arm_2" ~ "b4"
                             ) 
                            ) %>% select(-redcap_event_name)

## separar en visitas ----
### linea basal ----
m19_lb<-dt_m19 %>% filter(visita=="lb") 
###agregar prefijo de visita----
colnames(m19_lb) <- paste("lb",colnames(m19_lb),sep="_")
###renombrar el ID ----
m19_lb<-m19_lb %>% rename(.,id=lb_id)

### b4 ----
m19_b4<-dt_m19 %>% filter(visita=="b4") 
###agregar prefijo de visita----
colnames(m19_b4) <- paste("b4",colnames(m19_b4),sep="_")
###renombrar el ID ----
m19_b4<-m19_b4 %>% rename(.,id=b4_id)

## integrar variables m17 y visita ----
m19_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  m19_lb
) %>% left_join(
  m19_b4
) 


#sacar variables de m10 -----
dt_m10<-dt_fetp %>% filter(!is.na(m10_date)) %>% select(id, redcap_event_name, m10_sleep, m10_educ, m10_house) %>% mutate(
  visita=case_when(
    redcap_event_name=="linea_de_base_arm_2" ~ "lb",
    redcap_event_name=="b4_arm_2" ~ "b4"
  ) 
) %>% select(-redcap_event_name)
## separar en visitas----
m10_lb<-dt_m10 %>% filter(visita=="lb")
###agregar prefijo de visita----
colnames(m10_lb) <- paste("lb",colnames(m10_lb),sep="_")
###renombrar el ID ----
m10_lb<-m10_lb %>% rename(.,id=lb_id)
## separar en visitas----
m10_b4<-dt_m10 %>% filter(visita=="b4")
###agregar prefijo de visita----
colnames(m10_b4) <- paste("b4",colnames(m10_b4),sep="_")
###renombrar el ID ----
m10_b4<-m10_b4 %>% rename(.,id=b4_id)

## integrar variables m17 y visita ----
m10_variables<-anomalias %>% select(id,ultima_visita_madre, ultima_visita_nino) %>% left_join(
  m10_lb
) %>% left_join(
  m10_b4
) %>% left_join(
dt_fetp %>% filter(!is.na(m10_date)) %>% select(id, matches("m10_relation"), matches("m10_age")
) %>% mutate(
  edad_padre=case_when(
    m10_relation=="1" ~ m10_age,
    m10_relation_2=="1" ~ m10_age_2,
    m10_relation_3=="1" ~ m10_age_3,
    m10_relation_4=="1" ~ m10_age_4,
    m10_relation_5=="1" ~ m10_age_5,
    m10_relation_6=="1" ~ m10_age_6,
    m10_relation_7=="1" ~ m10_age_7,
    m10_relation_8=="1" ~ m10_age_8,
    m10_relation_9=="1" ~ m10_age_9,
    m10_relation_10=="1" ~ m10_age_10,
    m10_relation_11=="1" ~ m10_age_11,
    m10_relation_12=="1" ~ m10_age_12,
    m10_relation_13=="1" ~ m10_age_13,
    m10_relation_14=="1" ~ m10_age_14,
#    m10_relation_15=="1" ~ m10_age_15,
#    m10_relation_16=="1" ~ m10_age_16,
  #  m10_relation_17=="1" ~ m10_age_17,
    #m10_relation_18=="1" ~ m10_age_18,
   # m10_relation_19=="1" ~ m10_age_19,
  )
) %>% select(id, edad_padre) %>% filter(!is.na(edad_padre))
) 

#m13 agregadas
 
m13_new<-dt_fetp %>% filter(!is.na(m13_date)) %>% select(
 id,redcap_event_name, matches("m13_disease_") ,matches("m13_vit_")
) %>% filter(redcap_event_name=="linea_de_base_arm_2") %>% select(-redcap_event_name) %>% mutate(visit="lb")


###agregar prefijo de visita----
colnames(m13_new) <- paste("lb",colnames(m13_new),sep="_")
m13_new<-m13_new %>% rename(.,id=lb_id)
m13_variables<-m13_variables %>% left_join(
  m13_new
)

edad_gestacional<-anomalias %>% select(id) %>% left_join(
  gt_emory_data_arm1 %>% transmute(id_tamizaje=id, id=as.character(s4_main_id))
) %>% left_join(
  dt_z10_completo %>% select(id_tamizaje=record_id, fec_nac_madre=z10_dob) 
) %>% left_join(
  gt_emory_data_arm1 %>% select(id_tamizaje=id, fecha_m17=m17_date, semanas_fur=m17_lmp_ga,
                                semanas_ultra=m17_ultra_ga, metodo_edad_ga=m17_ga_method
  )
) %>% mutate(
  edad_madre_al_reclutamiento=as.Date(fecha_m17)-as.Date(fec_nac_madre),
  metodo_edad_ga=recode(
    metodo_edad_ga, "1"="FUR","2"="Ultrasonido"),
  edad_ga=if_else(metodo_edad_ga=="FUR", semanas_fur, semanas_ultra)
) %>% select(
  id, edad_ga, metodo_edad_ga
)



s1<-gt_emory_data_arm1 %>% filter(!is.na(s1_date)) %>% select(tamizaje_id_tamizaje=id, matches("s1_")) %>% left_join(
  m17_variables %>% select(tamizaje_id_tamizaje, id)
) %>%   select(id, id_tamizaje=tamizaje_id_tamizaje, 2:24 )

s1_m17<-anomalias %>% select(id) %>% left_join(
  s1
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, matches("m17_"))
) 
  
m13_solicitadas<-dt_fetp %>% filter(!is.na(m13_date)) %>%  select(id, redcap_event_name,m13_date, m13_vit, m13_vit_list___1,m13_vit_list___2, m13_vit_list___3, m13_vit_list___4,
                     m13_vit_list___555, m13_vit_other) %>% mutate(
                       visita=case_when(
                         redcap_event_name=="linea_de_base_arm_2" ~ "lb",
                         redcap_event_name=="p1_arm_2" ~ "p1",
                         redcap_event_name=="p2_arm_2" ~ "p2"
                       )
                     ) %>% select(id, visita,fecha_m13=m13_date, 4:11) %>%left_join(
                       gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
                     ) %>%   left_join(
                       dt_z10_completo %>% mutate(
                        id_tamizaje=as.character(record_id),
                         anio_nac_madre=case_when(
                           substr(z10_dob,2,2)=="-" ~ substr(z10_dob,6,10),
                           substr(z10_dob,5,5)=="-" ~ substr(z10_dob,1,4), 
                           substr(z10_dob,3,3)=="-" ~ substr(z10_dob,7,11)
                       )
                       ) 
                     )

m13_solicitadas<-anomalias %>% select(id) %>% left_join(
  m13_solicitadas
) 

dt_m11<-read_csv("data/fetp/HAPINGuatemalaMainSt-M11_DATA_2023-05-16_1914.csv")
dt_m11_new<-dt_m11 %>% filter(!is.na(m11_date)) %>% transmute(id=as.character(id), redcap_event_name,m11_alc, m11_alc_binge) %>% mutate(
  visita=case_when(
    redcap_event_name=="linea_de_base_arm_2" ~ "lb",
    redcap_event_name=="p1_arm_2" ~ "p1",
    redcap_event_name=="p2_arm_2" ~ "p2" ,
    redcap_event_name=="b1_arm_2" ~ "b1",
    redcap_event_name=="b2_arm_2" ~ "b2",
    redcap_event_name=="b3_arm_2" ~ "b3",
    redcap_event_name=="b4_arm_2" ~ "b4",
    redcap_event_name=="birth_arm_2" ~ "birth"
  ) 
) %>% select(-redcap_event_name)



## sacar las visitas m11 lb
dt_m11_new_lb<- dt_m11_new %>% filter(visita=="lb")

##agregar prefijo de visita----
colnames(dt_m11_new_lb) <- paste("lb",colnames(dt_m11_new_lb),sep="_")
##renombrar el ID ----
dt_m11_new_lb<-dt_m11_new_lb %>% rename(.,id=lb_id)

## sacar las visitas m11 p1
dt_m11_new_p1<- dt_m11_new %>% filter(visita=="p1")

##agregar prefijo de visita----
colnames(dt_m11_new_p1) <- paste("p1",colnames(dt_m11_new_p1),sep="_")
##renombrar el ID ----
dt_m11_new_p1<-dt_m11_new_p1 %>% rename(.,id=p1_id)

## sacar las visitas m11 p2
dt_m11_new_p2<- dt_m11_new %>% filter(visita=="p2")

##agregar prefijo de visita----
colnames(dt_m11_new_p2) <- paste("p2",colnames(dt_m11_new_p2),sep="_")
##renombrar el ID ----
dt_m11_new_p2<-dt_m11_new_p2 %>% rename(.,id=p2_id)

## sacar las visitas m11 b1
dt_m11_new_b1<- dt_m11_new %>% filter(visita=="b1")

##agregar prefijo de visita----
colnames(dt_m11_new_b1) <- paste("b1",colnames(dt_m11_new_b1),sep="_")
##renombrar el ID ----
dt_m11_new_b1<-dt_m11_new_b1 %>% rename(.,id=b1_id)

## sacar las visitas m11 b2
dt_m11_new_b2<- dt_m11_new %>% filter(visita=="b2")

##agregar prefijo de visita----
colnames(dt_m11_new_b2) <- paste("b2",colnames(dt_m11_new_b2),sep="_")
##renombrar el ID ----
dt_m11_new_b2<-dt_m11_new_b2 %>% rename(.,id=b2_id)

## sacar las visitas m11 b4
dt_m11_new_b4<- dt_m11_new %>% filter(visita=="b4")

##agregar prefijo de visita----
colnames(dt_m11_new_b4) <- paste("b4",colnames(dt_m11_new_b4),sep="_")
##renombrar el ID ----
dt_m11_new_b4<-dt_m11_new_b4 %>% rename(.,id=b4_id)

m11_variables<-anomalias %>% select(id) %>% left_join(
  dt_m11_new_lb
) %>% left_join(
  dt_m11_new_p1
) %>% left_join(
  dt_m11_new_p2
) %>% left_join(
  dt_m11_new_b1
) %>% left_join(
  dt_m11_new_b2
) %>% left_join(
  dt_m11_new_b4
)

list(
  c35=c35_variables,
  e2=e2_variables,
  e3=e3_variables,
  m17=m17_variables,
  c30=c30_variables,
  m13=m13_variables,
  m14a=m14a_variables,
  m14b=m14b_variables,
  m19=m19_variables,
  m10=m10_variables,
  m11=m11_variables,
  edad_gestacional_m17=edad_gestacional,
  m13_solicitadas=m13_solicitadas,
  s1_m17=s1_m17
  
) %>%
  writexl::write_xlsx("output/variables_c35_e2_e3_m17_c30_m13_m14a_m14b_m19_m10_s1_m17.xlsx")

