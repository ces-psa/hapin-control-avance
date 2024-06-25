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

#fechas
fecha1="2020-02-16"
fecha2= "2020-02-26"
  #Sys.Date()

sabado1="2020-02-08"
sabado1="2020-02-15"
sabado1="2020-02-22"

fecha_sabados="2020-02-08"
#cantidad de M17 producidos
m17_agrupado<-gt_emory_data %>% filter(!is.na(m17_date)) %>%  select("id_tamizaje"=id, m17_date, m17_by) %>% filter(m17_date>=fecha1) %>% group_by(m17_by)
#tabla agrupada por iniciales
table(m17_agrupado$m17_by)

#cantidad de M18 producidos
m18_agrupados <-gt_emory_data_arm2 %>% filter(!is.na(m18_date)) %>% select(id, m18_date, m18_by, visit) %>% filter(m18_date>=fecha1) %>% group_by(m18_by, visit)
table(paste0(m18_agrupados$m18_by,m18_agrupados$visit))

#cantidad de a26 producidos
a26_agrupados <-gt_emory_data %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, visit) %>% filter(a26_date>=fecha1) %>% group_by(a26_by, visit)
table(paste0(a26_agrupados$a26_by,a26_agrupados$visit))

#cantidad de a26 producidos
c36_agrupados <-gt_emory_data %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by, visit) %>% filter(c36_date>=fecha1) %>% group_by(c36_by, visit)
table(paste0(c36_agrupados$c36_by,c36_agrupados$visit))

#conteo de C36 en redcap UVG
data_c36<-read_csv("D:/Descargas/PilotoVigilanciaNeum_DATA_2020-02-27_1039.csv")
data_c34<-read_csv("D:/Descargas/PilotoVigilanciaNeum_DATA_2020-02-27_1039.csv")
c36_uvg<-data_c36 %>% filter(!is.na(today)) %>% select("id"=record_id, today, interviewer, redcap_event_name) %>% filter(today>=fecha1) %>% group_by(interviewer, redcap_event_name)
table(paste0(c36_uvg$interviewer,c36_uvg$redcap_event_name))

#revision de C31 y c36 vigilancia
c36_uvg %>% left_join(
gt_emory_data %>% filter(!is.na(c31_date)) %>% select(id, "today"=c31_date, c31_by)
) %>% print(n=Inf)


#cantidad de E2 producidos
e2_agrupados <-gt_emory_data %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by, visit) %>% filter(e2_date>=fecha1) %>% group_by(e2_by, visit)
table(paste0(e2_agrupados$e2_by,e2_agrupados$visit))

#cantidad de C34 producidos
c34_agrupado<-data_c34 %>% filter(!is.na(c34_date)) %>% select(record_id, c34_date, c34_by, redcap_event_name) %>% filter(c34_date>=fecha1) %>% group_by(c34_by, redcap_event_name)
table(paste0(c34_agrupado$c34_by, c34_agrupado$redcap_event_name))

#CONTEO DE PESOS AL NACER
gt_emory_data %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_by,c30_wt, c30_where, #donde fue el parto
                                                      c30_wt_where, #donde fue pesado
                                                      c30_ave_wt #peso promedio
                                                      ) %>% mutate(
                                                        lugar_peso=case_when(
                                                          c30_wt_where=="1" ~ "Hospital publico",
                                                          c30_wt_where=="2" ~ "Hospital privado",
                                                          c30_wt_where=="3" ~ "Casa propia",
                                                          c30_wt_where=="4" ~ "Casa del trabajador de salud comunitario",
                                                          c30_wt_where=="5" ~ "Casa de otro miembro de la familia",
                                                          c30_wt_where=="6" ~ "Centro / puesto salud publica",
                                                          c30_wt_where=="7" ~ "Clinica privada",
                                                          c30_wt_where=="555" ~ "Otro",
                                                        ),
                                                        c30_wt=if_else(c30_wt=="1", "Si", "No")
                                                        ) %>% 
                                                      filter(
                                                        c30_date>=fecha1
                                                      ) %>% select(id, "fecha_c30"=c30_date, "iniciales"=c30_by, "se peso al nacer"=c30_wt, lugar_peso, "peso_promedio"=c30_ave_wt ) %>% 
  write_csv("output/pesos_al_nacer.csv")

#REVISION DE PRODUCTIVIDAD POR CRF EN LINEA BASAL
crf_bl<-gt_emory_data_arm2 %>% filter(visit=="baseline")  %>% filter(!is.na(s6_date)) %>% select(id, s6_date, s6_by, visit) %>% 
  mutate(crf="s6", fecha=s6_date, iniciales=s6_by )%>% filter(s6_date>=fecha1) %>% select(crf, fecha, iniciales, visit) %>%  bind_rows(
    #m10
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by, visit) %>% 
      mutate(crf="m10", fecha=m10_date, iniciales=m10_by )%>% filter(m10_date>=fecha1) %>% select(crf, fecha, iniciales, visit),
    #m11
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
      mutate(crf="m11", fecha=m11_date, iniciales=m11_by )%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m13
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m13_date)) %>% select(id, m13_date, m13_by,visit) %>% 
      mutate(crf="m13", fecha=m13_date, iniciales=m13_by )%>% filter(m13_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14a
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by,visit) %>% 
      mutate(crf="m14a", fecha=m14a_date, iniciales=m14a_by )%>% filter(m14a_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by )%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m19
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by,visit) %>% 
      mutate(crf="m19", fecha=m19_date, iniciales=m19_by )%>% filter(m19_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a29
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a21_date)) %>% select(id, a21_date, a21_by,visit) %>% 
      mutate(crf="a21", fecha=a21_date, iniciales=a21_by )%>% filter(a21_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a23
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by,visit) %>% 
      mutate(crf="a23", fecha=a23_date, iniciales=a23_by )%>% filter(a23_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a24a
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, a24a_by) %>% 
      mutate(crf="a24a", fecha=a24a_date, iniciales=a24a_by )%>% filter(a24a_date>=fecha1) %>% select(crf, fecha, iniciales),
    #a24b
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a24b_date)) %>% select(id, a24b_date, a24b_by,visit) %>% 
      mutate(crf="a24b", fecha=a24b_date, iniciales=a24b_by )%>% filter(a24b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a26
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by,visit) %>% 
      mutate(crf="a26", fecha=a26_date, iniciales=a26_by )%>% filter(a26_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a27
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a27_date)) %>% select(id, a27_date, a27_by,visit) %>% 
      mutate(crf="a27", fecha=a27_date, iniciales=a27_by)%>% filter(a27_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a29
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a29_date)) %>% select(id, a29_date, a29_by,visit) %>% 
      mutate(crf="a29", fecha=a29_date, iniciales=a29_by )%>% filter(a29_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a30
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(a30_date)) %>% select(id, a30_date, a30_by,visit) %>% 
      mutate(crf="a30", fecha=a30_date, iniciales=a30_by )%>% filter(a30_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by )%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by )%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by )%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h43
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h43_date)) %>% select(id, h43_date, h43_by,visit) %>% 
      mutate(crf="h43", fecha=h43_date, iniciales=h43_by)%>% filter(h43_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h50
    gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h50_date)) %>% select(id, h50_date, h50_by,visit) %>% 
      mutate(crf="h50", fecha=h50_date, iniciales=h50_by)%>% filter(h50_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
crf_bl<-crf_bl %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 1_", visit))

#PRODUCTIVIDAD_P1
crf_p1<-#m11
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
      mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m13
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m13_date)) %>% select(id, m13_date, m13_by,visit) %>% 
      mutate(crf="m13", fecha=m13_date, iniciales=m13_by)%>% filter(m13_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14a
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by,visit) %>% 
      mutate(crf="m14a", fecha=m14a_date, iniciales=m14a_by)%>% filter(m14a_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m19
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by,visit) %>% 
      mutate(crf="m19", fecha=m19_date, iniciales=m19_by)%>% filter(m19_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m18
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m18_date)) %>% select(id, m18_date, m18_by,visit) %>% 
      mutate(crf="m18", fecha=m18_date, iniciales=m18_by)%>% filter(m18_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a21
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(a21_date)) %>% select(id, a21_date, a21_by,visit) %>% 
      mutate(crf="a21", fecha=a21_date, iniciales=a21_by)%>% filter(a21_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a23
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by,visit) %>% 
      mutate(crf="a23", fecha=a23_date, iniciales=a23_by)%>% filter(a23_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a24a
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, a24a_by) %>% 
      mutate(crf="a24a", fecha=a24a_date, iniciales=a24a_by)%>% filter(a24a_date>=fecha1) %>% select(crf, fecha, iniciales),
    #a24b
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(a24b_date)) %>% select(id, a24b_date, a24b_by,visit) %>% 
      mutate(crf="a24b", fecha=a24b_date, iniciales=a24b_by)%>% filter(a24b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #h55
    gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
crf_p1<-crf_p1 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 2_", visit))

#PRODUCTIVIDAD_P2
crf_p2<-#m11
  gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
  mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m13
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m13_date)) %>% select(id, m13_date, m13_by,visit) %>% 
      mutate(crf="m13", fecha=m13_date, iniciales=m13_by)%>% filter(m13_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14a
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m14a_date)) %>% select(id, m14a_date, m14a_by,visit) %>% 
      mutate(crf="m14a", fecha=m14a_date, iniciales=m14a_by)%>% filter(m14a_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m19
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m19_date)) %>% select(id, m19_date, m19_by,visit) %>% 
      mutate(crf="m19", fecha=m19_date, iniciales=m19_by)%>% filter(m19_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #m18
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(m18_date)) %>% select(id, m18_date, m18_by,visit) %>% 
      mutate(crf="m18", fecha=m18_date, iniciales=m18_by)%>% filter(m18_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a21
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(a21_date)) %>% select(id, a21_date, a21_by,visit) %>% 
      mutate(crf="a21", fecha=a21_date, iniciales=a21_by)%>% filter(a21_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a23
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by,visit) %>% 
      mutate(crf="a23", fecha=a23_date, iniciales=a23_by)%>% filter(a23_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a24a
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, a24a_by) %>% 
      mutate(crf="a24a", fecha=a24a_date, iniciales=a24a_by)%>% filter(a24a_date>=fecha1) %>% select(crf, fecha, iniciales),
    #a24b
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(a24b_date)) %>% select(id, a24b_date, a24b_by,visit) %>% 
      mutate(crf="a24b", fecha=a24b_date, iniciales=a24b_by)%>% filter(a24b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
  
    #h55
    gt_emory_data_arm2 %>% filter(visit=="p2") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
crf_p2<-crf_p2 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 3_", visit))

#evento BIRTH
crf_birth<-#b10a
  gt_emory_data_arm2 %>% filter(visit=="parto") %>% filter(!is.na(b10a_date)) %>% select(id, b10a_date, b10a_by,visit) %>% 
  mutate(crf="b10a", fecha=b10a_date, iniciales=b10a_by)%>% filter(b10a_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #c30
    gt_emory_data_arm2 %>% filter(visit=="parto") %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_by,visit) %>% 
      mutate(crf="c30", fecha=c30_date, iniciales=c30_by)%>% filter(c30_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c30a
    gt_emory_data_arm2 %>% filter(visit=="parto") %>% filter(!is.na(c30a_date)) %>% select(id, c30a_date, c30a_by,visit) %>% 
      mutate(crf="c30a", fecha=c30a_date, iniciales=c30a_by)%>% filter(c30a_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h55
    gt_emory_data_arm2 %>% filter(visit=="parto") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
crf_birth<-crf_birth %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 4_", visit))

#visita mes 1
crf_m1<-#b10a
  gt_emory_data_arm2 %>% filter(visit=="m1") %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by,visit) %>% 
  mutate(crf="c31", fecha=c31_date, iniciales=c31_by)%>% filter(c31_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
crf_m1<-crf_m1 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 5_", visit))

#PRODUCTIVIDAD_B1
crf_b1<-#m11
  gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
  mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c31
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by,visit) %>% 
      mutate(crf="c31", fecha=c31_date, iniciales=c31_by)%>% filter(c31_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c32
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by,visit) %>% 
      mutate(crf="c32", fecha=c32_date, iniciales=c32_by)%>% filter(c32_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c33
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by,visit) %>% 
      mutate(crf="c33", fecha=c33_date, iniciales=c33_by)%>% filter(c33_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c35
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by,visit) %>% 
      mutate(crf="c35", fecha=c35_date, iniciales=c35_by)%>% filter(c35_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #h55
    gt_emory_data_arm2 %>% filter(visit=="b1") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
crf_b1<-crf_b1 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 6_", visit))

#PRODUCTIVIDAD_B2
crf_b2<-#m11
  gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
  mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c31
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by,visit) %>% 
      mutate(crf="c31", fecha=c31_date, iniciales=c31_by)%>% filter(c31_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c32
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by,visit) %>% 
      mutate(crf="c32", fecha=c32_date, iniciales=c32_by)%>% filter(c32_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c33
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by,visit) %>% 
      mutate(crf="c33", fecha=c33_date, iniciales=c33_by)%>% filter(c33_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c35
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by,visit) %>% 
      mutate(crf="c35", fecha=c35_date, iniciales=c35_by)%>% filter(c35_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #h55
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )

crf_b2<-crf_b2 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 7_", visit))


#PRODUCTIVIDAD_B3
crf_b2<-#m11
  gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
  mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c31
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by,visit) %>% 
      mutate(crf="c31", fecha=c31_date, iniciales=c31_by)%>% filter(c31_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c32
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by,visit) %>% 
      mutate(crf="c32", fecha=c32_date, iniciales=c32_by)%>% filter(c32_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c33
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by,visit) %>% 
      mutate(crf="c33", fecha=c33_date, iniciales=c33_by)%>% filter(c33_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c35
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by,visit) %>% 
      mutate(crf="c35", fecha=c35_date, iniciales=c35_by)%>% filter(c35_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="b3") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #h55
    gt_emory_data_arm2 %>% filter(visit=="b2") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )

crf_b3<-crf_b2 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 8_", visit))


#PRODUCTIVIDAD_B4
crf_b2<-#m11
  gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by,visit) %>% 
  mutate(crf="m11", fecha=m11_date, iniciales=m11_by)%>% filter(m11_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #m14b
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by,visit) %>% 
      mutate(crf="m14b", fecha=m14b_date, iniciales=m14b_by)%>% filter(m14b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #b10
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,visit) %>% 
      mutate(crf="b10", fecha=b10_date, iniciales=b10_by)%>% filter(b10_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c31
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c31_date)) %>% select(id, c31_date, c31_by,visit) %>% 
      mutate(crf="c31", fecha=c31_date, iniciales=c31_by)%>% filter(c31_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c32
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c32_date)) %>% select(id, c32_date, c32_by,visit) %>% 
      mutate(crf="c32", fecha=c32_date, iniciales=c32_by)%>% filter(c32_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c33
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by,visit) %>% 
      mutate(crf="c33", fecha=c33_date, iniciales=c33_by)%>% filter(c33_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c35
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(c35_date)) %>% select(id, c35_date, c35_by,visit) %>% 
      mutate(crf="c35", fecha=c35_date, iniciales=c35_by)%>% filter(c35_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by,visit) %>% 
      mutate(crf="h41", fecha=h41_date, iniciales=h41_by)%>% filter(h41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h41b
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h41b_date)) %>% select(id, h41b_date, h41b_by,visit) %>% 
      mutate(crf="h41b", fecha=h41b_date, iniciales=h41b_by)%>% filter(h41b_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h42
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h42_date)) %>% select(id, h42_date, h42_by,visit) %>% 
      mutate(crf="h42", fecha=h42_date, iniciales=h42_by)%>% filter(h42_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #h55
    gt_emory_data_arm2 %>% filter(visit=="b4") %>% filter(!is.na(h55_date)) %>% select(id, h55_date, h55_by,visit) %>% 
      mutate(crf="h55", fecha=h55_date, iniciales=h55_by)%>% filter(h55_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )

crf_b4<-crf_b2 %>% filter(!is.na(visit)) %>% mutate(visit=paste0(" 9_", visit))

#PRODUCTIVIDAD_SS1
crf_ss1<-#m10
  gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(m10_date)) %>% select(id, m10_date, m10_by, visit) %>% 
  mutate(crf="m10", fecha=m10_date, iniciales=m10_by) %>% filter(m10_date>=fecha1) %>% select(crf, fecha, iniciales, visit) %>% bind_rows(
    #m60
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(m60_date)) %>% select(id, m60_date, m60_by,visit) %>% 
      mutate(crf="m60", fecha=m60_date, iniciales=m60_by)%>% filter(m60_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #a70
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(a70_date)) %>% select(id, a70_date, a70_by,visit) %>% 
      mutate(crf="a70", fecha=a70_date, iniciales=a70_by)%>% filter(a70_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c36
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by,visit) %>% 
      mutate(crf="c36", fecha=c36_date, iniciales=c36_by)%>% filter(c36_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c37
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c37_date)) %>% select(id, c37_date, c37_by,visit) %>% 
      mutate(crf="c37", fecha=c37_date, iniciales=c37_by)%>% filter(c37_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c40
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c40_date)) %>% select(id, c40_date, c40_by,visit) %>% 
      mutate(crf="c40", fecha=c40_date, iniciales=c40_by)%>% filter(c40_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c41
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c41_date)) %>% select(id, c41_date, c41_by,visit) %>% 
      mutate(crf="c41", fecha=c41_date, iniciales=c41_by)%>% filter(c41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c80
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c80_date)) %>% select(id, c80_date, c80_by,visit) %>% 
      mutate(crf="c80", fecha=c80_date, iniciales=c80_by)%>% filter(c80_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c81
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c81_date)) %>% select(id, c81_date, c81_by,visit) %>% 
      mutate(crf="c81", fecha=c81_date, iniciales=c81_by)%>% filter(c81_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c81
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c81_date)) %>% select(id, c81_date, c81_by,visit) %>% 
      mutate(crf="c81", fecha=c81_date, iniciales=c81_by)%>% filter(c81_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c82
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(c82_date)) %>% select(id, c82_date, c82_by,visit) %>% 
      mutate(crf="c82", fecha=c82_date, iniciales=c82_by)%>% filter(c82_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #h43
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(h43_date)) %>% select(id, h43_date, h43_by,visit) %>% 
      mutate(crf="h43", fecha=h43_date, iniciales=h43_by)%>% filter(h43_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e1
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by,visit) %>% 
      mutate(crf="e1", fecha=e1_date, iniciales=e1_by)%>% filter(e1_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e2
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,visit) %>% 
      mutate(crf="e2", fecha=e2_date, iniciales=e2_by)%>% filter(e2_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e5
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e5_date)) %>% select(id, e5_date, e5_by,visit) %>% 
      mutate(crf="e5", fecha=e5_date, iniciales=e5_by)%>% filter(e5_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e6
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e6_date)) %>% select(id, e6_date, e6_by,visit) %>% 
      mutate(crf="e6", fecha=e6_date, iniciales=e6_by)%>% filter(e6_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e7
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e7_date)) %>% select(id, e7_date, e7_by,visit) %>% 
      mutate(crf="e7", fecha=e7_date, iniciales=e7_by)%>% filter(e7_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e8
    gt_emory_data_arm2 %>% filter(visit=="libres1") %>% filter(!is.na(e8_date)) %>% select(id, e8_date, e8_initials1,visit) %>% 
      mutate(crf="e8", fecha=e8_date, iniciales=e8_initials1)%>% filter(e8_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )
  
crf_ss1<-crf_ss1 %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 0_","segun_necesario1"))

#PRODUCTIVIDAD_SS2
crf_ss2<-
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by,visit) %>% 
      mutate(crf="c36", fecha=c36_date, iniciales=c36_by)%>% filter(c36_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #c37
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(c37_date)) %>% select(id, c37_date, c37_by,visit) %>% 
      mutate(crf="c37", fecha=c37_date, iniciales=c37_by)%>% filter(c37_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c40
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(c40_date)) %>% select(id, c40_date, c40_by,visit) %>% 
      mutate(crf="c40", fecha=c40_date, iniciales=c40_by)%>% filter(c40_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c41
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(c41_date)) %>% select(id, c41_date, c41_by,visit) %>% 
      mutate(crf="c41", fecha=c41_date, iniciales=c41_by)%>% filter(c41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #e1
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by,visit) %>% 
      mutate(crf="e1", fecha=e1_date, iniciales=e1_by)%>% filter(e1_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e2
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,visit) %>% 
      mutate(crf="e2", fecha=e2_date, iniciales=e2_by)%>% filter(e2_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e5
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(e5_date)) %>% select(id, e5_date, e5_by,visit) %>% 
      mutate(crf="e5", fecha=e5_date, iniciales=e5_by)%>% filter(e5_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e6
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(e6_date)) %>% select(id, e6_date, e6_by,visit) %>% 
      mutate(crf="e6", fecha=e6_date, iniciales=e6_by)%>% filter(e6_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e7
    gt_emory_data_arm2 %>% filter(visit=="libres2") %>% filter(!is.na(e7_date)) %>% select(id, e7_date, e7_by,visit) %>% 
      mutate(crf="e7", fecha=e7_date, iniciales=e7_by)%>% filter(e7_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
   
  )

crf_ss2<-crf_ss2 %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 1_","segun_necesario2"))

#PRODUCTIVIDAD_SS3
crf_ss3<-
  gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by,visit) %>% 
  mutate(crf="c36", fecha=c36_date, iniciales=c36_by)%>% filter(c36_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #c37
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(c37_date)) %>% select(id, c37_date, c37_by,visit) %>% 
      mutate(crf="c37", fecha=c37_date, iniciales=c37_by)%>% filter(c37_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c40
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(c40_date)) %>% select(id, c40_date, c40_by,visit) %>% 
      mutate(crf="c40", fecha=c40_date, iniciales=c40_by)%>% filter(c40_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c41
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(c41_date)) %>% select(id, c41_date, c41_by,visit) %>% 
      mutate(crf="c41", fecha=c41_date, iniciales=c41_by)%>% filter(c41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #e1
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by,visit) %>% 
      mutate(crf="e1", fecha=e1_date, iniciales=e1_by)%>% filter(e1_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e2
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,visit) %>% 
      mutate(crf="e2", fecha=e2_date, iniciales=e2_by)%>% filter(e2_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e5
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(e5_date)) %>% select(id, e5_date, e5_by,visit) %>% 
      mutate(crf="e5", fecha=e5_date, iniciales=e5_by)%>% filter(e5_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e6
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(e6_date)) %>% select(id, e6_date, e6_by,visit) %>% 
      mutate(crf="e6", fecha=e6_date, iniciales=e6_by)%>% filter(e6_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e7
    gt_emory_data_arm2 %>% filter(visit=="libres3") %>% filter(!is.na(e7_date)) %>% select(id, e7_date, e7_by,visit) %>% 
      mutate(crf="e7", fecha=e7_date, iniciales=e7_by)%>% filter(e7_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
    
  )

crf_ss3<-crf_ss3 %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 2_","segun_necesario3"))

#PRODUCTIVIDAD_SS4
crf_ss4<-
  gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by,visit) %>% 
  mutate(crf="c36", fecha=c36_date, iniciales=c36_by)%>% filter(c36_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #c37
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(c37_date)) %>% select(id, c37_date, c37_by,visit) %>% 
      mutate(crf="c37", fecha=c37_date, iniciales=c37_by)%>% filter(c37_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c40
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(c40_date)) %>% select(id, c40_date, c40_by,visit) %>% 
      mutate(crf="c40", fecha=c40_date, iniciales=c40_by)%>% filter(c40_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c41
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(c41_date)) %>% select(id, c41_date, c41_by,visit) %>% 
      mutate(crf="c41", fecha=c41_date, iniciales=c41_by)%>% filter(c41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #e1
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by,visit) %>% 
      mutate(crf="e1", fecha=e1_date, iniciales=e1_by)%>% filter(e1_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e2
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2d") %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,visit) %>% 
      mutate(crf="e2", fecha=e2_date, iniciales=e2_by)%>% filter(e2_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
    
  )

crf_ss4<-crf_ss4 %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 3_","segun_necesario4"))

#PRODUCTIVIDAD_SS5
crf_ss5<-
  gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(c36_date)) %>% select(id, c36_date, c36_by,visit) %>% 
  mutate(crf="c36", fecha=c36_date, iniciales=c36_by)%>% filter(c36_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #c37
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(c37_date)) %>% select(id, c37_date, c37_by,visit) %>% 
      mutate(crf="c37", fecha=c37_date, iniciales=c37_by)%>% filter(c37_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c40
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(c40_date)) %>% select(id, c40_date, c40_by,visit) %>% 
      mutate(crf="c40", fecha=c40_date, iniciales=c40_by)%>% filter(c40_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #c41
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(c41_date)) %>% select(id, c41_date, c41_by,visit) %>% 
      mutate(crf="c41", fecha=c41_date, iniciales=c41_by)%>% filter(c41_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    
    #e1
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(e1_date)) %>% select(id, e1_date, e1_by,visit) %>% 
      mutate(crf="e1", fecha=e1_date, iniciales=e1_by)%>% filter(e1_date>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e2
    gt_emory_data_arm2 %>% filter(visit=="segun_sea_necesari_arm_2e") %>% filter(!is.na(e2_date)) %>% select(id, e2_date, e2_by,visit) %>% 
      mutate(crf="e2", fecha=e2_date, iniciales=e2_by)%>% filter(e2_date>=fecha1) %>% select(crf, fecha, iniciales,visit)
    
  )

crf_ss5<-crf_ss5 %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 4_","segun_necesario5"))


#SALIDAS_ESTUDIO
crf_salida<- #e3 madre
  gt_emory_data_arm2 %>% filter(visit=="salida") %>%filter(!is.na(e3_date)) %>% select(id, e3_date, e3_by, visit) %>% 
  mutate(crf="e3_madre", fecha=e3_date, iniciales=e3_by)%>% filter(e3_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #e3 owa
    gt_emory_data_arm2 %>% filter(visit=="salida") %>%filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_by_o, visit) %>% 
      mutate(crf="e3_owa", fecha=e3_date_o, iniciales=e3_by_o)%>% filter(e3_date_o>=fecha1) %>% select(crf, fecha, iniciales,visit),
    #e3 child
    gt_emory_data_arm2 %>% filter(visit=="salida") %>%filter(!is.na(e3_date_c)) %>% select(id, e3_date_c, e3_by_c, visit) %>% 
      mutate(crf="e3_child", fecha=e3_date_c, iniciales=e3_by_c)%>% filter(e3_date_c>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )


crf_salida<-crf_salida %>% filter(!is.na(visit)) %>% mutate(visit=paste0("1 5_",visit))
#exportar set de datos para crear tabla dinamica
crf_bl %>% bind_rows(crf_p1, crf_p2,crf_birth, crf_m1,crf_b1,crf_b2,crf_b3,crf_b4, crf_ss1,crf_ss2,crf_ss3, crf_ss4, crf_ss5, crf_salida) %>% writexl::write_xlsx("output/productividad.xlsx") 


gt_emory_data_arm2 %>% filter(!is.na(a24b_date) & is.na(a24b_by)) %>%  filter(a24b_date =="2019-04-11") %>%  select(id,visit)

#PRODUCTIVIDAD DE INTERVENCION
crf_h51<-gt_emory_repeat_data %>% filter(!is.na(h51_date)) %>% select(id, h51_date, h51_by) %>% mutate(crf="h51", fecha=h51_date, iniciales=h51_by) %>% 
    filter(h51_date>=fecha1) %>% select(crf, fecha, iniciales)
crf_h50<-gt_emory_data_arm2 %>% filter(visit=="baseline") %>% filter(!is.na(h50_date)) %>% select(id, h50_date, h50_by) %>% 
  mutate(crf="h50", fecha=h50_date, iniciales=h50_by)%>% filter(h50_date>=fecha1) %>% select(crf, fecha, iniciales)

crf_h52<-gt_emory_data_arm3 %>% filter(!is.na(h52_date)) %>% select(id,h52_date,h52_by) %>% mutate(crf="h52", fecha=h52_date, iniciales=h52_by) %>% 
  filter(h52_date >=fecha1) %>% select(crf,fecha, iniciales)
#CREAR DATOS PARA EXPORTAR
crf_h50 %>% bind_rows(
  crf_h51,crf_h52
) %>%mutate(
  dia_semana=lubridate::wday(fecha, label = TRUE)
) %>% 
  writexl::write_xlsx("output/productividad_intervencion_dias.xlsx")


#PRODUCTIVIDAD ELEGIBILIDAD
crf_salida<- #e3 madre
  gt_emory_data_arm2 %>% filter(visit=="salida") %>%filter(!is.na(e3_date)) %>% select(id, e3_date, e3_by, visit) %>% 
  mutate(crf="e3_madre", fecha=e3_date, iniciales=e3_by)%>% filter(e3_date>=fecha1) %>% select(crf, fecha, iniciales,visit) %>% bind_rows(
    #e3 owa
    gt_emory_data_arm2 %>% filter(visit=="salida") %>%filter(!is.na(e3_date_o)) %>% select(id, e3_date_o, e3_by_o, visit) %>% 
      mutate(crf="e3_owa", fecha=e3_date_o, iniciales=e3_by_o)%>% filter(e3_date_o>=fecha1) %>% select(crf, fecha, iniciales,visit)
  )

#S1
tamizaje<- #s1
  gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(s1_date)) %>%  select(id,s1_date,s1_by, visit) %>% 
  mutate(crf="s1", fecha=s1_date, iniciales=s1_by) %>%  filter(s1_date>=fecha1) %>%  select(crf, fecha, iniciales, visit) %>%  bind_rows(
    #m17
    gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(m17_date)) %>%  select(id,m17_date,m17_by, visit) %>% 
      mutate(crf="m17", fecha=m17_date, iniciales=m17_by) %>%  filter(m17_date>=fecha1) %>%  select(crf, fecha, iniciales, visit),
    #s2
    gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(s2_date)) %>%  select(id,s2_date,s2_by, visit) %>% 
      mutate(crf="s2", fecha=s2_date, iniciales=s2_by) %>%  filter(s2_date>=fecha1) %>%  select(crf, fecha, iniciales, visit),
    #s3
    gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(s3_date)) %>%  select(id,s3_date,s3_by, visit) %>% 
      mutate(crf="s3", fecha=s3_date, iniciales=s3_by) %>%  filter(s3_date>=fecha1) %>%  select(crf, fecha, iniciales, visit),
    #s4
    gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(s4_date)) %>%  select(id,s4_date,s4_by, visit) %>% 
      mutate(crf="s4", fecha=s4_date, iniciales=s4_by) %>%  filter(s4_date>=fecha1) %>%  select(crf, fecha, iniciales, visit),
    #e7
    gt_emory_data_arm1 %>% filter(visit=="tamizaje") %>% filter(!is.na(e7_date)) %>%  select(id,e7_date,e7_by, visit) %>% 
      mutate(crf="e7", fecha=e7_date, iniciales=e7_by) %>%  filter(e7_date>=fecha1) %>%  select(crf, fecha, iniciales, visit)
  )

tamizaje %>% writexl::write_xlsx("output/productividad_tamizaje.xlsx")

tamizaje %>% group_by(visit,crf,iniciales) %>% summarize(
  n=n()
) %>% spread(visit,n) %>% spread(crf, tamizaje)

visitas<-tamizaje %>%  group_by(iniciales,visit) %>% summarize(
  n=n()
) %>% spread(visit, n)

crfs<-tamizaje %>% mutate(
  crf=paste(crf,visit)
) %>% count(iniciales,crf) %>% spread(crf, n)


tablita<-crfs %>% full_join(visitas)


intervencion<-crf_h50 %>% bind_rows(
  crf_h51,crf_h52
) %>%mutate(
  dia_semana=lubridate::wday(fecha, label = TRUE)
)

intervencion %>% group_by(iniciales, crf) %>% summarize(
  n=n()
) %>% spread(crf,n)

##productividad antropometria
#Sacar los C33
gt_emory_data_arm2 %>%filter(!is.na(c33_date)) %>%  select(id, visit, c33_date, c33_by) %>% group_by(visit) %>%  
  mutate(crf="c33", fecha=c33_date, iniciales=c33_by) %>%  
  filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit) %>% 
  #sacar los C35
  bind_rows(
    gt_emory_data_arm2 %>%filter(!is.na(c35_date)) %>%  select(id, visit, c35_date, c35_by) %>% group_by(visit) %>%  
      mutate(crf="c35", fecha=c35_date, iniciales=c35_by) %>%  
      filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit)
  ) %>% 
  #sacar los C31 M1
  bind_rows(
    gt_emory_data_arm2 %>%filter(!is.na(c31_date) & visit=="m1") %>%  select(id, visit, c31_date, c31_by) %>% group_by(visit) %>%  
      mutate(crf="c31", fecha=c31_date, iniciales=c31_by) %>%  
      filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit)
  ) %>% 
  #agregar los M11 de linea Basal
  bind_rows(
    gt_emory_data_arm2 %>%filter(!is.na(m11_date) & visit=="baseline") %>%  select(id, visit, m11_date, m11_by) %>% group_by(visit) %>%  
      mutate(crf="m11", fecha=m11_date, iniciales=m11_by) %>%  
      filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit)
  ) %>% 
  #agregar los M18 de p1 y p2
  bind_rows(
    gt_emory_data_arm2 %>%filter(!is.na(m18_date)) %>%  select(id, visit, m18_date, m18_by) %>% group_by(visit) %>%  
      mutate(crf="m18", fecha=m18_date, iniciales=m18_by) %>%  
      filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit)
  ) %>% 
#agregar los M17
 bind_rows(
   gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% transmute(id_tamizaje=as.character(id),id=as.character(s4_main_id)) %>% left_join(
     gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% transmute(id_tamizaje=as.character(id), m17_date, m17_by, visit) 
   ) %>% group_by(visit) %>% 
    mutate(crf="m17", fecha=m17_date, iniciales=m17_by) %>%
    filter(fecha>="2020-02-16" & fecha<="2020-02-26") %>%  select(crf, fecha, iniciales, visit)
) %>% writexl::write_xlsx("output/productividad_febrero_16-26.xlsx")
