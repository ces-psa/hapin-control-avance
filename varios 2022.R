library(package = "tidyverse")
gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% select(id, visit, a24a_date, a24a_by) %>% filter(
grepl("33[0-9]",id)
)



gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, h50_date)

estufas_con_falla<-readxl::read_excel(path = "C:/HAPIN/estufas_falla.xlsx", sheet = "Hoja1")

estufas_con_falla %>% mutate_all(as.character) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h50_date)) %>% select(id, fecha_instalacion_estufa=h50_date)
) %>% left_join(
  gt_emory_data_arm3 %>% filter(!is.na(h52_date)) %>% select(id, redcap_event_name, 
                                                            h52_date, h52_repair___1,h52_repair___2,
                                                            h52_repair___3,h52_repair___4,
                                                            h52_repair___555,
                                                            h52_repair_other,h52_stove___1,h52_stove___2,
                                                            h52_stove___3,h52_stove___4,h52_stove___5,h52_stove___6,
                                                            h52_stove___7,h52_stove___555)
) 



gt_emory_data_arm2 %>% filter(is.na(a24b_date)) %>% select(id, visit
                                                          )


#-----------------
#catalogo de rutas
rutas<-read_csv("data/cat_rutas_visitas.csv")
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  type=if_else(
    grepl("^35",id), "owa", "pwg"
  ),
  brazo=recode(brazo,
   "0"="Control","1"="Intervencion"
  )
) %>% left_join(
 comunidades %>% select(id=id_estudio, community, community_new  ) %>% mutate(comunidad=if_else(is.na(community_new), community, community_new))
) %>% left_join(
  rutas
) %>% select(id, brazo, type, comunidad, ruta) %>%   writexl::write_xlsx("output/activas_en_estudio.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id, v1_date, v1_by) %>%  filter(v1_by=="33264")
%>% writexl::write_xlsx("output/conteos_covid_fabiola.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% filter(c33_date=="2020-06-19") %>% select(id, c33_date, c33_by)


gt_emory_repeat_data %>% filter(redcap_event_name=="surveillance_arm_4") %>% select(record_id) %>% write_csv("output/reision rr_id_repeated.csv")



gt_emory_repeat_data %>% filter(!is.na(h51_date)) %>% select(h51_hhid, h51_date) %>% filter(
  h51_hhid=="35097"
) %>% arrange(desc(h51_date))


gt_emory_data_arm2 %>% filter(!is.na(m11_date)) %>% select(id, m11_date, m11_by) %>% filter(m11_date=="2020-06-10") %>% arrange(m11_by)


gt_emory_data_arm2 %>% filter(!is.na(a26_date))  %>% select(id, visit, a26_date) %>% filter()           


##---------------------------
# DATOS DE H55 PENDIENTES EN B3

gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% filter(visit=="b3") %>% select(id, c33_date, visit) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% select(id, h55_date, visit) %>% filter(visit=="b3")
) %>% filter(is.na(h55_date)) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h55_date)) %>% filter(visit=="b4") %>%  select(id, h55_b4=h55_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  edad_dias=(Sys.Date() - as.Date(c30_dob)),
  edad_meses= edad_dias / 30.25
) %>% select(id, fecha_b3=c33_date, fec_nacimiento=c30_dob, edad_actual_meses=edad_meses) %>% left_join(
  comunidades %>% select(id=id_estudio, id_tamizaje=record_id, comunidad_1= com_jalapa, comunidad_2=com_jalapa_new)
)%>%  left_join(
  rutas %>% select(comunidad_1=comunidad, ruta)
) %>% arrange(desc(edad_actual_meses)) %>% writexl::write_xlsx(paste0("output/pendientes_h55_en_b3_al", Sys.Date(),".xlsx"))



data_covid19<-read_csv("data/HAPINGuatemalaMainSt-Covid19_DATA_2020-09-03_1719.csv")


gt_emory_data_arm2 %>% filter(
  !is.na(s6_arm)
) %>% select(id) %>% anti_join(
  salidas %>%  select(id)
) %>% left_join(
  data_covid19 %>% filter(!is.na(v1_date)) %>%  select(id,v1_date, v1_by ) %>% mutate_all(as.character)
) %>% filter(is.na(v1_date))



#activas:  398
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% count()

#salidas  (353 h56
salidas_uvg <- read_csv(
  file = "data/exports/FinalizacinHAPIN_DATA_2020-03-02_0927.csv",
  col_types = cols(.default = col_character())
) %>%
  print()

salidas_emory<-gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(id, h56_date)
 
salidas_red_uvg<-  salidas_uvg %>% select(id=record_id, h56_date=h56g_date) %>% anti_join(
    salidas_emory
  )

salidas_emory %>% bind_rows(
  salidas_red_uvg
)

gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit, e3_reason) %>% mutate(
  e3_reason=recode(
    e3_reason, "1"="Finalizacion estudio",
    "2"= "No elegible",
    "3"= "Retiro Voluntario",
    "4"= "Retirada por equipo",
    "5"="Se mudo area estudio",
    "6"="Fallecio",
    "7"= "Perdido en seguimiento",
    "8"="Madre aborto, mortinato, muerte nino",
    "555"="Otro"
  )
) %>% group_by(e3_reason) %>% count()
# 1 Finalizacion estudio                   351
# 2 Madre aborto, mortinato, muerte nino    38
# 3 No elegible                              1
# 4 Retirada por equipo                      1
# 5 Retiro Voluntario                       13
# 6 Se mudo area estudio                    11

salidas_emory %>% bind_rows(
  salidas_red_uvg
) %>% anti_join(
gt_emory_data_arm2 %>% filter(!is.na(e3_date)) %>% select(id, e3_date_exit, e3_reason) %>% mutate(
  e3_reason=recode(
    e3_reason, "1"="Finalizacion estudio",
    "2"= "No elegible",
    "3"= "Retiro Voluntario",
    "4"= "Retirada por equipo",
    "5"="Se mudo area estudio",
    "6"="Fallecio",
    "7"= "Perdido en seguimiento",
    "8"="Madre aborto, mortinato, muerte nino",
    "555"="Otro"
  )
) %>% filter(e3_reason=="Finalizacion estudio")
)

  salidas_red_uvg %>% print(n=Inf)
  
  
  #revision pesos redcap
  data_hospital<-read_csv("C:/HAPIN/data_pesos_hospital.csv")
  data_hospital<-data_hospital %>% mutate_all(as.character)
  
  data_hospital %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% 
      transmute(id, c30_by, c30_date, peso_hosp_rc=c30_wt_record,
                unidad_hosp_rc=recode(c30_wt_unit,"1"="kilogramos.gramos","2"="libras.onzas"), 
                c30_head1,c30_head2,c30_head3
      ) %>% 
      mutate(
        circunf_antro_rc=case_when(
          is.na(c30_head3) & is.na(c30_head2) ~ as.numeric(c30_head1),
          is.na(c30_head3) & !is.na(c30_head2) ~ (as.numeric(c30_head2) + as.numeric(c30_head1))/2,
          TRUE ~ NA_real_
        )
      ) 
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na())
  )
    write_csv("output/revision_pesos.csv")
  
  
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% 
    transmute(id, c30_by, peso_hosp_rc=c30_wt_record,
       unidad_hosp_rc=recode(c30_wt_unit,"1"="kilogramos.gramos","2"="libras.onzas"), 
       c30_head1,c30_head2,c30_head3
    ) %>% 
    mutate(
      circunf_antro_rc=case_when(
        is.na(c30_head3) & is.na(c30_head2) ~ as.numeric(c30_head1),
        is.na(c30_head3) & !is.na(c30_head2) ~ (as.numeric(c30_head2) + as.numeric(c30_head1))/2,
        TRUE ~ NA_real_
      )
    ) 
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
type=if_else(grepl("^35[0-9]", id), "owa", "pwg")
) %>% filter(type=="owa")

#test graficas bland-altman
A <- c(-0.358, 0.788, 1.23, -0.338, -0.789, -0.255, 0.645, 0.506, 
       0.774, -0.511, -0.517, -0.391, 0.681, -2.037, 2.019, -0.447, 
       0.122, -0.412, 1.273, -2.165)
B <- c(0.121, 1.322, 1.929, -0.339, -0.515, -0.029, 1.322, 0.951, 
       0.799, -0.306, -0.158, 0.144, 1.132, -0.675, 2.534, -0.398, 0.537, 
       0.173, 1.508, -1.955)

plot(A, B, main="Scatter plot")
abline(0,1)

plot((A+B)/2, A-B, main="Mean-Difference-Plot")

library(BlandAltmanLeh)
bland.altman.plot(A, B, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")

#ejemplo 2
# Generates two random measurements
measurement1 <- rnorm(100)
measurement2 <- rnorm(100)
library(blandr)

# Generates a plot, with no optional arguments
blandr.draw( measurement1 , measurement2 )

# Generates a plot, using the in-built R graphics
blandr.draw( measurement1 , measurement2 , plotter = 'rplot' )

# Generates a plot, with title changed
blandr.draw( measurement1 , measurement2 , plotTitle = 'Bland-Altman example plot' )

# Generates a plot, with title changed, and confidence intervals off
blandr.draw(measurement2, measurement1 ,  plotTitle = 'Bland-Altman example plot' ,
             ciDisplay = TRUE , ciShading = TRUE )

blandr.draw(measurement1, measurement2, method1name = "Method 1",
            method2name = "Method 2",
            plotTitle = "Bland-Altman plot for comparison of 2 methods",
            sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
            ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
            lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
            overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
            y.plot.mode = "difference", plotProportionalBias = FALSE,
            plotProportionalBias.se = TRUE, assume.differences.are.normal = TRUE)


#revision de datos Mtellez
data_lab_jalapa<-read_csv("c:/temp/HAPINGuatemalaLab_DATA_2020-11-05_1201.csv")

data_lab_jalapa %>% group_by(redcap_event_name) %>% count()
data_lab_jalapa %>% filter(!is.na(card_container_id)) %>% select(card_container_id, matches("card_id_")) %>% gather(
  key=variable, value = value, -card_container_id
) %>% filter(grepl("35141-O6",value)) 
# %>% 
#   write_csv("output/lab_erase.csv")


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  s6_arm=recode(s6_arm,"0"="Control", "1"="Intervencion")
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id= s4_main_id)
) %>% select(id_tamizaje, id, grupo=s6_arm) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(m14a_date) & visit=="baseline") %>% select(
      id, fec_lb=m14a_date,lb_peso1=m14_wt1, lb_peso2=m14_wt2
    ) %>% mutate(
      lb_promedio=(as.numeric(lb_peso1)+as.numeric(lb_peso2))/2
    ) 
  ) %>% 
    left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(m14a_date) & visit=="p1") %>% select(id,fec_p1=m14a_date, p1_peso1=m14_wt1,
                                                                              p1_peso2=m14_wt1) %>% 
      mutate(
        p1_promedio=(as.numeric(p1_peso1)+as.numeric(p1_peso2))/2
      )
  ) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, lb_ga=m17_ultra_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(m14a_date) & visit=="p2") %>% select(id,fec_p2=m14a_date, p2_peso1=m14_wt1,
                                                                              p2_peso2=m14_wt1) %>% 
      mutate(
        p2_promedio=(as.numeric(p2_peso1)+as.numeric(p2_peso2))/2
      )
  ) %>% mutate(
    fcc= as.Date(m17_ga) -  lubridate::days(280),
    lb_sem=as.numeric((as.Date(fec_lb) - as.Date(fcc))) %/% 7,
    lb_dias=as.numeric((as.Date(fec_lb) - as.Date(fcc))) %% 7,
    lb_ga=paste0(lb_sem,".",lb_dias),
    
    p1_sem=as.numeric((as.Date(fec_p1) - as.Date(fcc))) %/% 7,
    p1_dias=as.numeric((as.Date(fec_p1) - as.Date(fcc))) %% 7,
    p1_ga=paste0(p1_sem,".",p1_dias),
    
    p2_sem=as.numeric((as.Date(fec_p2) - as.Date(fcc))) %/% 7,
    p2_dias=as.numeric((as.Date(fec_p2) - as.Date(fcc))) %% 7,
    p2_ga=paste0(p2_sem,".",p2_dias),
    
    lb_hasta_p1=as.numeric(p1_promedio)-as.numeric(lb_promedio),
    lb_hasta_p2=as.numeric(p2_promedio)-as.numeric(lb_promedio),
    p1_hasta_p2=as.numeric(p2_promedio)-as.numeric(p1_promedio),
  ) %>% select(
    id_tamizaje, id, grupo, lb_peso1, lb_peso2, lb_promedio, lb_ga, fec_lb,
    p1_peso1, p1_peso2, p1_promedio, p1_ga, fec_p1,
    p2_peso1, p2_peso2, p2_promedio, p2_ga, fec_p2,
    lb_hasta_p1, lb_hasta_p2, p1_hasta_p2
  )  %>% mutate(
    confirmar=case_when(
      lb_hasta_p1<0 ~ "peso LB y en P1",
      lb_hasta_p2<0 ~ "peso LB y en P2",
      p1_hasta_p2<0 ~ "peso en P1 y peso en P2"
    ),
    Observaciones= if_else(
      is.na(fec_p2) & !is.na(p2_ga),
      "Se hizo en tiempo de Covid",
      NA_character_
    )
  ) %>% 
  arrange(desc(lb_hasta_p1, lb_hasta_p2, p1_hasta_p2)) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`, `Nombre embarazada`, `Comunidad embarazada (original z10)`,
                                   `Comunidad embarazada (nueva)`, `Celular embarazada`, `Celular esposo`,
                                   `Celular_otro_miembro`)
  ) %>% 
  writexl::write_xlsx("output/lista_pesos_madre_total_12-11-2020.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  s6_arm=recode(s6_arm,"0"="Control", "1"="Intervencion")
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id= s4_main_id)
) %>% select(-s6_arm) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, m17_ga)
) %>% mutate(
  fec_nacimiento=if_else(
    is.na(c30_dob), as.Date(m17_ga), as.Date(c30_dob)
  )
) %>% select(id_tamizaje, id, fec_nacimiento) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% select(id)
) %>% mutate(b4_pendiente="Si") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b2") %>% transmute(id, b2_realizado="Si")
) %>% mutate(
  b2_pendiente=if_else(
    is.na(b2_realizado), "Si", NA_character_
  )
) %>% mutate(
  edad_mes=as.numeric((Sys.Date() -  as.Date(fec_nacimiento))) %/% 30.25,
  edad_dias=round(as.numeric((Sys.Date() -  as.Date(fec_nacimiento))) %% 30.25),
  edad_bb=paste0(edad_mes," meses con ",edad_dias, " dias"),
  visitas_pendientes= case_when(
    is.na(b2_realizado) & b4_pendiente=="Si" ~ "B2 y B4",
    !is.na(b2_realizado) & b4_pendiente=="Si" ~ "B4",
    TRUE ~ NA_character_
  )
) %>% transmute(
  id_tamizaje, id, fec_nacimiento, edad_meses=as.numeric(edad_mes), "con (dias):"=edad_dias, edad_bb, visitas_pendientes
) %>% arrange(desc(edad_meses)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c33_date) & visit=="b3") %>% transmute(id, b3_clinica_realizado="Si" ) 
) %>% left_join(
  data_intensivo %>% filter(redcap_event_name=="blp1_arm_1") %>% transmute(
    id=as.character(record_id), Participa_Intensivo="Si"
  )
) %>% writexl::write_xlsx("output/listado_Pendientes_B2_B4_exposición.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(a26_date) & visit=="b4") %>% select(id, a26_date) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_o)) %>% select(id, e3_date_o)
) %>% mutate(
  flag=if_else(
    as.Date(a26_date) > as.Date(e3_date_o),
    "1",
    "0"

) %>% filter(flag=="1")


#revision datos credi Lisa
dt_credi<-read_csv("c:/temp/HAPIN_CREDI_ITT_nf_20210219.csv")

dt_credi %>% group_by(irc, timepoint) %>% count()

dt_credi %>% filter(timepoint!="Birth") %>% filter(!is.na(c35_age))


dt_credi %>%  filter(timepoint!="Birth") %>% filter(!is.na(c35_age)) %>%  mutate(
  edad_adecuada=case_when(
     as.numeric(c35_age) < 6 & !is.na(c35_move)  ~ "1",
     as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 11 & !is.na(c35_grasp)& is.na(c35_suck)  ~ "1",
     as.numeric(c35_age) >=12  & !is.na(c35_roll) & is.na(c35_interest) ~ "1",
     is.na(c35_age) ~ NA_character_,
     TRUE ~ "0"
  )
) %>% select(irc,visit, timepoint, hhid, c35_age, c35_move, c35_suck,
             c35_grasp, c35_roll, c35_interest, edad_adecuada) %>% group_by(irc,edad_adecuada) %>% count()
  
  # filter(edad_adecuada=="0") %>% filter(
  #             (c35_age< 6 & is.na(c35_move)) |
  #             (c35_age>=6 & c35_age<=11 & is.na(c35_grasp) ) |
  #               (c35_age>=12 & is.na(c35_roll))
  #            ) %>% group_by(irc, edad_adecuada) %>% count()


dt_credi %>%  filter(timepoint!="Birth") %>% filter(!is.na(c35_age)) %>%  mutate(
  edad_adecuada=case_when(
    as.numeric(c35_age) < 6 & !is.na(c35_move)  ~ "1",
    as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 11 & !is.na(c35_grasp)& is.na(c35_suck)  ~ "1",
    as.numeric(c35_age) >=12  & !is.na(c35_roll) & is.na(c35_interest) ~ "1",
    is.na(c35_age) ~ NA_character_,
    TRUE ~ "0"
  )
) %>% select(irc,visit, timepoint, hhid, c35_age, c35_move, c35_suck,
             c35_grasp, c35_roll, c35_interest, edad_adecuada) %>% filter(edad_adecuada=="0") %>% 
  mutate(
    temprano=case_when(
    as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 11  & !is.na(c35_suck) ~ "1",
    as.numeric(c35_age) >=12  & !is.na(c35_interest) ~ "1"
      )
    ) %>% mutate(
  tarde=case_when(
    as.numeric(c35_age) < 6 & is.na(c35_move) & !is.na(c35_grasp) ~ "1",
    as.numeric(c35_age) >=6 & as.numeric(c35_age) <= 11 & is.na(c35_grasp) ~ "1",
    as.numeric(c35_age) >=12 & is.na(c35_roll) ~ "1"
      )
  ) %>%   filter(is.na(temprano) & is.na(tarde))
    group_by(irc, temprano, tarde) %>% count()

    
    
    
    # Create a 30 x 30 matrix (of 30 rows and 30 columns)
    mymat <- matrix(nrow=30, ncol=60)
    
    # For each row and for each column, assign values based on position: product of two indexes
    for(i in 1:dim(mymat)[1]) {
      for(j in 1:dim(mymat)[2]) {
        mymat[i,j] = i*j
      }
    }

    
    # Just show the upper left 10x10 chunk
    mymat[1:10, 1:10]
    dt_credi<- dt_credi %>% mutate(ultima_columna=0)
    
    is.na(dt_credi[1,247])
    i=1
      #dim(dt_credi)[1] #file
    j=(dim(dt_credi)[2])-2 #columnas
    h=247
    for(i  in 1:dim(dt_credi)[1] ) {
    for (h in h:1) {
      
      tiene_datos=is.na(dt_credi[i,h])
      
      if_else(
        tiene_datos==FALSE,
        {
          dt_credi[i,h]=i,h
          h=1
        },
        h=h-1
      )
    }
    }   
 
    dt_credi[1,248]
    
   variable_nombre<-dt_credi %>%  select(246) %>% variable.names()
dt_credi %>% select(246) %>% mutate(
  columna_final=if_else(is.na(variable_nombre), NA_character_ , 246)
)   
     !is.na(dt_credi[1,247])
variable.names(dt_credi[1,247])      




#REVISION ESTADISTICAS DE GAS LPG
gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="1") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% mutate(
  before_birth=if_else(
    is.na(fecha_nacimiento), "1","0"
  )
) %>% group_by(before_birth) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="1") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% mutate(
  before_birth=if_else(
    is.na(fecha_nacimiento), "1","0"
  )
) %>% anti_join(
all_gas_actions %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="1") %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
  ) %>% mutate(
    before_birth=if_else(
      is.na(fecha_nacimiento), "1","0"
    )
  ) %>% select(house_id=id, fecha_nacimiento, saliio_antes_nacer=before_birth)
) %>% mutate(
  recarga_durante_gestacion=case_when(
    as.Date(date) < as.Date(fecha_nacimiento) & action=="install" ~ "1",
    TRUE ~ NA_character_
  )
) %>% filter(action=="install") %>% filter(recarga_durante_gestacion=="1") %>% distinct(house_id) %>% select(id=house_id)
)


all_gas_actions %>% filter(action=="install") %>% filter(
  source!="emory-stove-install"
) 
#fechas nacimiento y parto

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% filter(s6_arm=="1") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% mutate(
  before_birth=if_else(
    is.na(fecha_nacimiento), "1","0"
  )
) 
)



#revision h54 todos los hogares
gt_emory_repeat_data %>% filter(!is.na(h54_date)) %>% 
  select(h54_hhid,h54_date, h54_by,h54_stove_use,h54_stove_num,
         h54_stove1_loc,h54_stove1_evid,h54_stove2_loc,h54_stove2_evid) %>% filter(h54_stove_use=="1") %>% mutate(
           evidencia_estufa=recode(h54_stove_use,"0"="No", "1"="Si"),
           cantidad_estufas=h54_stove_num,
           ubicacion_estufa_1=recode(h54_stove1_loc, "1"="Interior", "2"="Aire libre"),
           evidencia_estufa_1=recode(h54_stove1_evid, "1"="La estufa esta siendo usada durante la visita", 
                                     "2"="Se observa calor, humo, brasas u otras senales de uso corriente"),
           ubicacion_estufa_2=recode(h54_stove2_loc, "1"="Interior", "2"="Aire libre"),
           evidencia_estufa_2=recode(h54_stove2_evid, "1"="La estufa esta siendo usada durante la visita", 
                                     "2"="Se observa calor, humo, brasas u otras senales de uso corriente"),
           fecha_h54=as.Date(h54_date),
           ID=h54_hhid, 
           Iniciales=h54_by
           
         ) %>% writexl::write_xlsx("output/revision_h54.xlsx")


etiquetas<-read_csv("c:/temp/etiquetas_hapin_1_5.csv")
etiquetas %>% mutate_all(as.character) %>% mutate(tipo="X0") %>% 
  complete(
    nesting(id),
    tipo = c("X0", "U0", "U1", "U2", "U3", "U4")
) %>% mutate(
  etiqueta=paste0(id,"-C7-",tipo)
) %>% writexl::write_xlsx("output/etiquetas_hapin_1_5_child.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(b10a_date)) %>% select(id, visit, b10a_tc_spots) %>% group_by(b10a_tc_spots) %>% count()


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_consent, s4_consent_c,
                                                        s4_owa) %>% filter(id!="99999") %>% 
  mutate(type=if_else(
                                                          grepl("^35",id),
                                                          "owa",
                                                          "pwg"
                                                        )) %>% group_by(
                                                          type,s4_consent,
  s4_consent_c,
  s4_owa
) %>% count()

gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% select(id, m11_date) %>%  mutate(type=if_else(
  grepl("^35",id),
  "owa",
  "pwg"
)) %>% group_by(type) %>% count()

dt_24meses<-gt_hapin_II_data %>% filter(!is.na(c35_date)) %>% select(id, c35_date) %>% mutate(type=if_else(
  grepl("^35",id),
  "owa",
  "pwg"
)) %>% group_by(type) %>% count()


#conteso de sangre en mujer madre en b4
dt_sangre_seca<-gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(visit=="b4") %>% select(
  id, 
  b10_date,
  b10_by,
  b10_tm_spots
) %>% arrange(desc(b10_date)) %>% filter(b10_tm_spots=="1")

dt_sangre_seca %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% group_by(type) %>% count()

dt_sangre_seca %>% anti_join(
  dt_24meses %>% select(id)
) 



gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_consent, s4_consent_c,
                                                        s4_owa) %>% filter(id!="99999") %>% 
  mutate(type=if_else(
    grepl("^35",id),
    "owa",
    "pwg"
  )) %>% anti_join(

    dt_24meses %>% select(id)
    
    ) %>% anti_join(
      dt_sangre_seca %>% select(id)
    )




all_gas_actions %>% filter(house_id=="35035")


v1_covid<-read_csv("c:/temp/HAPINGuatemalaMainSt-V1covid19gt_DATA_2021-03-25_1958.csv")
v1_covid %>% filter(!is.na(v1_date)) %>% write_csv("c:/temp/cv1_covid19_export.csv")


rutas

gt_hapin_II_data %>% filter(!is.na(c31_date))  %>% count()


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% filter(visit=="b4") %>% select(id)
) %>% mutate(type=if_else(grepl("^35",id), "owa", "pwg")) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(inicio_ventana=as.Date(c30_dob)+lubridate::days(365-28)) %>% 
  arrange(inicio_ventana) %>% left_join(
    datos_participantes %>% select(id=`ID estudio`,
                                   `ID tamizaje`, `Nombre embarazada`, 
                                   comunidad=`Comunidad embarazada (original z10)`,
                                   `Celular embarazada`, `Celular esposo`, `Celular_otro_miembro`
                                   )
    )%>% left_join(  rutas
                                   ) %>% mutate(
                                     brazo=if_else(s6_arm=="1","Intervencion", "Control")
                                     ) %>% select(-s6_arm) %>% writexl::write_xlsx("output/pendientes_exposición_al_05-04-2021.xlsx")
  


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(visit=="b2") %>% select(id, b10_date, b10_to_spots,
                                                                                   b10_to_b_code,b10_venous,
                                                                                   b10_v_code1,b10_buccal,b10_buccal_2) %>%
  mutate(
    type=if_else(grepl("^35",id), "owa", "pwg")
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_date_exit_o)
  ) %>% mutate(
    flag1=if_else(as.Date(e3_date_exit_o) < as.Date(b10_date),"1","")
  ) %>% 
  writexl::write_xlsx("output/revision_b10.xlsx")
#sangre seca en adulta b10_to_spots
#QR sangre seca adulta b10_to_b_code
#sangre venosa adulta b10_venous

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(a23_date)) %>% select(id, a23_date)
) %>% mutate(
  type=if_else(grepl("^35",id), "owa", "pwg")
) %>% filter(type=="owa") %>% arrange(a23_date) %>%  print(n=Inf)


gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% mutate(
  type=if_else(grepl("^35",id),"owa","pwg")
) %>% print(n=40) %>% group_by(brazo, type) %>% count()


#revision de productos Carla
lista_entrega
pedidos_carla<-read_csv("c:/temp/consolidado_carla.csv")
pedidos_carla %>% transmute(ID,Fecha_solicitud, `Codigo articulo solicitado`, Listado_carla="Si") %>% filter(ID=="33435")

lista_entrega %>% mutate(Fecha_solicitud=as.Date(Fecha_solicitud)) %>%  left_join(
  pedidos_carla %>% transmute(ID, Fecha_solicitud=as.Date(Fecha_solicitud), `Codigo articulo solicitado`, Listado_carla="Si")
) %>% filter(is.na(Listado_carla)) %>% write_csv("output/revision_carla_productos.csv")


#REVISIÓN DE MUESTRAS SANGRE SECA EXPOSICION Y WALDEMAR
var1="2021-03-12"
listado_bebes %>% mutate(
  edad_calculada=(as.Date(var1) - as.Date(fecha_nacimiento))/30
) %>% select(id, fecha_nacimiento, edad_calculada) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, h41_date, visit, h41_by,b10_tm_spots) %>% filter(visit=="b4")
    #filter(as.Date(h41_date)<"2021-03-12") 
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_consent)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, fecha_Salida=e3_date_exit)
) %>% filter(edad_calculada<="15") %>% writexl::write_xlsx("output/reision_seroprevalencia.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by,b10_tm_spots, visit ) %>%
  filter(visit=="b4") %>% filter(b10_tm_spots=="1") %>% arrange(b10_date)

gt_emory_data_arm2 %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, visit) %>% bind_rows(
  gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, visit=redcap_event_name)
) %>% group_by(visit) %>% count()

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_consent, s4_consent_c, s4_owa, s4_ocon_date) %>% 
  mutate(type=if_else(grepl("^35",id), "owa", "pwg")) %>% filter(type=="owa") %>% filter(!is.na(s4_ocon_date)) %>% 
  left_join(
    gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id, a26_date, a26_by, visit=redcap_event_name)
  ) %>% filter(!is.na(a26_date))
#43 s4 en hogares 35


gt_emory_data_arm2 %>% filter(!is.na(a24b_date)) %>% select(id, a24b_date, a24b_by, a24_sbp1, visit) %>% group_by(visit) %>% count()



consentimientos_madre %>% select(id, s4_date,s4_consent, s4_ocon_date, type) %>% filter(
  type=="owa"
)  %>% left_join(
 gt_hapin_II_data %>% filter(!is.na(a23_date)) %>% select(id, a23_date) 
) %>% print(n=Inf) %>% filter(is.na(s4_ocon_date))

hogares_rechazaron %>% left_join(
  listado_bebes %>% select(id, fecha_nacimiento)
) %>% mutate(
  edad=(Sys.Date() - as.Date(fecha_nacimiento))/12
  ) %>% writexl::write_xlsx("output/revision.xlsx")

hogares33_rechazaron %>% bind_rows(
  hogares35_rechazaron
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, matches("s4_"))
) %>% left_join(
  listado_bebes %>% select(id, fecha_nacimiento) 
) %>%  mutate(
  edad=(as.Date(s4_date) - as.Date(fecha_nacimiento))/12
) %>%   writexl::write_xlsx("output/rechazos_s4_fechas.xlsx")


gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_by, h57_stove) %>% filter(is.na(
  h57_stove
)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm)
) %>% filter(s6_arm=="1") %>% writexl::write_xlsx("output/intervencion_h57.xlsx")

gt_hapin_II_data %>% filter(!is.na(h57_date)) %>% select(id, h57_by, h57_date, h57_stove) %>% filter(is.na(
  h57_stove
)) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm)
) %>% filter(s6_arm=="0") %>% left_join(
  hogares_con_estufa
) %>% writexl::write_xlsx("output/grupoControl__h57.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(b10a_tc_b_code)) %>% select(id, b10a_tc_b_code)

gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_by, s4_ocon_date) %>% 
  filter(!is.na(s4_ocon_date)) %>% anti_join(
    gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id)
  )

gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by) %>% mutate(
  type=if_else(grepl("35",id),"owa","pwg")
) %>% filter(type=="owa") %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id,s4_ocon_date)
)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, salida_madre=e3_date_exit)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, salida_adulta=e3_date_exit_o)
) %>% mutate(brazo=recode(s6_arm,"1"="Intervencion", "0"="Control")) %>% select(-s6_arm) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% mutate(
  inicio_ventana_b4= as.Date(fecha_nacimiento) + lubridate::days(338),
  fin_ventana_b4= as.Date(fecha_nacimiento) + lubridate::days(393)
) %>% arrange(fin_ventana_b4) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date) & visit=="b4") %>% select(id, fecha_paso_clinica=m10_date)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(h41_date) & visit=="b4") %>% select(id, fecha_paso_exposicion=h41_date)
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`,
          `Nombre embarazada`, 
          comunidad=`Comunidad embarazada (original z10)`, `Celular embarazada`,
          `Celular esposo`, Celular_otro_miembro)
) %>% left_join(
  rutas
) %>% writexl::write_xlsx(paste0("output/participantes_activas_al_",Sys.Date(),".xlsx"))


#revisión a24a faltante
gt_emory_data_arm2 %>% filter(!is.na(a24a_date)) %>% select(id, a24a_date, a24a_by, redcap_event_name) %>% filter(
  as.Date(a24a_date)>="2019-04-21" & as.Date(a24a_date)<="2019-04-23"
)

gt_emory_data_arm2 %>% filter(!is.na(a24_visit)) %>% select(id, visit, a24_visit, a24a_date, a24a_by) %>% 
#arrange(visit) %>% print(n=Inf)
  filter(a24_visit=="1") %>% group_by(visit) %>% count()

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  type=if_else(
    grepl("^35",id),"hogar_adulta","hogar_madre"
    )
  ) %>% group_by(type) %>% count()


data_lab<-read_csv("data/exports/HAPINGuatemalaLab_DATA_2021-05-19_1045.csv")

data_lab %>% filter(redcap_event_name=="registro_arm_2") %>% select(
  card_container_id, matches("card_id_"),
  matches("blood_spots_valid_"),
  matches("blood_spots_invalid_")
) %>% filter(!is.na(card_container_id)) %>% 
  gather(
  key="variable", value="value", -card_container_id
) %>% filter(!is.na(value)) %>% mutate(
  Sample_id=if_else(
    grepl("^card_id_", variable), value, NA_character_
  ),
  valid_drops=if_else(
    grepl("^blood_spots_valid_", variable), value, NA_character_
  )
) %>% group_by(variable) %>% count() %>% print(n=500)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% anti_join(
  salidas %>% select(id)
)

gt_emory_data_arm2 %>% filter(!is.na(m11_date) & visit=="p1") %>% select(id, m11_date) %>% 
  filter(m11_date=="2019-02-20")

gt_emory_data_arm2 %>% filter(!is.na(m14a_date)) %>% select(id, m14a_date,
                                                            m14a_by, m14_scale_num) %>% 
  filter(m14a_by=="ACG") %>% arrange(m14a_date) %>% print(n=300)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, s6_arm) %>% mutate(
  type=if_else(
    grepl("^35",id),"owa","pwg"
  )
) %>% filter(type=="owa") %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(
    id, s4_date,s4_ocon_date
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date)
) %>% filter(is.na(c33_date)) %>% filter(is.na(s4_ocon_date)) %>% arrange(s4_date) %>% print(n=80)



gt_emory_data_arm2 %>% filter(!is.na(e6_date)) %>% select(id, e6_date)

gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, fecha_exposicion=b10_date) %>% arrange(
  desc(fecha_exposicion)
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                 Celular_madre_nino=`Celular embarazada`, Celular_papa_nino=`Celular esposo`, Celular_otro_miembro)
) %>% left_join(
  rutas
) %>% 
  left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                               contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
  ) %>% writexl::write_xlsx("output/candidatos_timetracker.xlsx")
  print()

  data_rb<-read_csv("c:/temp/ReportBack_DATA_2021-07-07_1627.csv")
  data_rb %>% select(record_id, eh_date, eh_iniciales) %>% mutate(
    id=substr(record_id, 1,5)
  )  %>% left_join(
    datos_participantes %>% select(id=`ID estudio`, `ID tamizaje`, nombre_madre_nino=`Nombre embarazada`, comunidad=`Comunidad embarazada (original z10)`,
                                   Celular_madre_nino=`Celular embarazada`, Celular_papa_nino=`Celular esposo`, Celular_otro_miembro)
  ) %>% left_join(
    rutas
  ) %>% 
    left_join(
      gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% select(id,contacto1_h56=h56_phone1,
                                                                 contacto2_h56=h56_phone2,contacto3_h56=h56_phone3)
    ) %>% writexl::write_xlsx("output/casas_report_back.xlsx")
  
  gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% select(id, h41_date, h41_by) %>% filter(
    as.Date(h41_date)>"2021-06-01"
  ) %>% arrange(desc(h41_date)) %>% print(n=30)
  
  gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_by) %>% filter(
    as.Date(b10_date)>"2021-06-01"
  ) %>% arrange(desc(b10_date)) %>% print(n=30)
  
  gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% select(id, c33_date, c33_by) %>% filter(
    as.Date(c33_date)>"2021-06-01"
  ) %>% arrange(desc(c33_date)) %>% print(n=30)
  
  gt_hapin_II_data %>% filter(!is.na(a23_date)) %>% select(id, a23_date, a23_by) %>% filter(
    as.Date(a23_date)>"2021-06-01"
  ) %>% arrange(desc(a23_date)) %>% print(n=30)

  gt_emory_data_arm2 %>% filter(!is.na(m14a_date))  %>% select(id, m14a_date, m14a_by,m14_scale_num) %>% filter(
    m14a_date=="2019-05-07"
  )
  
  gt_hapin_II_data %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date, m14b_by) %>% 
    filter(is.na(m14b_by))
  
  data_renap<-read_csv("c:/temp/data_renap.csv")
  
  
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% 
                                  select(id, h41_date, h41_by) %>% filter(h41_date=="2020-07-24")
  
                                
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_by, c30_dob, c30_age) %>% 
    arrange(desc(c30_dob))
  
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit) %>% arrange(
    desc(e3_date_exit)
  )
  gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_date_exit_o) %>% arrange(
    desc(e3_date_exit_o)
  )
  
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% 
    select(id, h41_date, h41_by, visit) %>% filter(h41_date=="2020-06-04")
  dt_intensivo<-read_csv("c:/temp/HAPINGuatemalaExposu_DATA_2021-10-27_2305.csv")
  
  intensivo<-dt_intensivo %>% mutate(id=as.character(record_id)) %>% 
    select(id, everything(), -record_id,) %>% filter(!is.na(h41_date)) %>% 
    select(id,h41_date, redcap_event_name) %>%
    mutate(visit=substr(redcap_event_name,1,4)) %>% select(id, h41_date, visit)
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% 
    select(id, h41_date, h41_by, visit) %>% filter(
      id=="33457" | 
        id=="33472" |
        id=="35007" |
        id=="35147") %>% writexl::write_xlsx("output/revision_fechas_h41.xlsx")
  
  gt_emory_data_arm2 %>% filter(!is.na(m14b_date)) %>% select(id, m14b_date) %>% 
    filter(m14b_date=="2020-06-05")
    
  
  data_entericas %>% filter(!is.na(c39_date)) %>%  group_by(visit) %>% count()

  gt_emory_data_arm2 %>% filter(!is.na(s8_date))  %>% group_by(id,visit) %>% count()
  
  gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% select(id, visit, h41_date) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% select(id, e3_date_exit)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit_o)) %>% select(id, e3_date_exit_o)
  ) %>% bind_rows(
    intensivo
  ) %>% arrange(id, h41_date) %>%  writexl::write_xlsx("output/listado_visitas_h41.xlsx")
  
  lab_entericas<-read_csv("c:/temp/lab_entericas_alex.csv")
  
  
  lab_entericas %>% filter(!is.na(aliquot_container_id_ent)) %>% select(
    aliquot_container_id_ent, matches("aliquot_id_")
  ) %>% gather(key="variable", value="value", -aliquot_container_id_ent) %>% filter(
    !is.na(value)
  ) %>% mutate(
    id=substr(value,1,5),
      visit= case_when(
      substr(value,7,8)=="C1" ~ "b2",
      substr(value,7,8)=="C3" ~ "b4",
      TRUE ~ "revisar"
    )
    ) %>% group_by(id,visit) %>% count() %>% arrange(id) %>% writexl::write_xlsx("output/revision_tubos_visitas.xlsx")
  
  lab_entericas %>% filter(!is.na(aliquot_container_id_ent)) %>% select(
    aliquot_container_id_ent, matches("aliquot_id_")
  ) %>% gather(key="variable", value="value", -aliquot_container_id_ent) %>% filter(
    !is.na(value)
  ) %>% writexl::write_xlsx("output/entericas_lab.xlsx")

  
  
  lm1 = df_train_2 %>% lm( formula = y~x)
  lm1 %>% summary()   
  
  lm()
  
  
  
  #revisión variables c31
  gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(
    id, visit, matches("^c31_")
  ) %>% writexl::write_xlsx("output/datos_c31.xlsx")

  
  gt_emory_data_arm2 %>% filter(!is.na(c41_date)) %>% select(
    id, visit, matches("^c41_")
  ) %>% writexl::write_xlsx("output/datos_c41.xlsx")

  gt_emory_data_arm2 %>% filter(!is.na(c41_date)) %>% select(
    id, visit, matches("^c41_")
  ) %>% transmute(id, visit, c41_diagnosis,
                  c41_diagnosis_translate="", 
                  c41_diagnosis_2, c41_diagnosis_2_translate="",
               c41_comment,
               c41_comment_translate="") %>% writexl::write_xlsx(
                 "output/c41_main_study_translate_list.xlsx"
               )

  gt_emory_data_arm2 %>% filter(!is.na(c42_date)) %>% select(
    id, visit, matches("^c42_")
  ) %>% select(id, visit, c42_note) %>% filter(!is.na(c42_note)) %>% 
    writexl::write_xlsx("output/c42_main_Study_translate.xlsx")

  
  data_repeated<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2021-12-06_1649.csv")  
  
  data_repeated %>% filter(!is.na(c41_date)) %>% 
    select(
      record_id, matches("^c41_")
    ) %>% transmute(
      
      record_id, c41_hhid, c41_date,
      c41_diagnosis, c41_diagnosis_translate="",
      c41_diagnosis_2, c41_diagnosis_2_translate="",
      c41_comment, c41_comment_translate=""
    ) %>% writexl::write_xlsx("output/c41_repeated_translate.xlsx")
  
  
  #revision adultas libny
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
  ) %>% mutate(type=if_else(
    grepl("^35",id), "owa", "pwg"
  )
  ) %>% filter(type=="owa") %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(
      id, s4_date, s4_consent, s4_owa, s4_ocon_date
    )
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(a26_date)) %>% select(id, a26_date)
  ) %>% writexl::write_xlsx("output/listado_completo_adultas.xlsx")
  
  
  #leer datos h51 repeated
  h51_repeated<-read_csv("c:/temp/HAPINGuatemalaRepeat_DATA_2021-12-08_1605.csv")
  
h51_repeated %>% filter(h51_hhid=="35004") %>% filter(h51_date=="2019-10-07")
  

#conteos para presentación autoridades Jalapa
gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id, m17_date, m17_by) %>% 
  group_by(
    lubridate::year(m17_date),
    lubridate::month(m17_date)
  ) %>% count() %>% writexl::write_xlsx("output/m17_realizados.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, visit, b10_date, b10_tm_hg, b10_to_hba1c) %>% 
  filter(!is.na(b10_to_hba1c))

#conteo de c35
gt_emory_data_arm2 %>% filter(!is.na(c33_wt1)) %>% select(id, c33_wt1, visit) %>% 
  group_by(
    visit
  ) %>% count() %>% writexl::write_xlsx("output/conteos_c33_por_visita.xlsx")

m14b<-gt_emory_data_arm2 %>% filter(!is.na(m14_sbp1)) %>% select(id, visit, m14_sbp1) %>% group_by(visit) %>% count() %>% 
  select(visita=visit, Cantidad_medidas_Presion_m14b=n)

a24b<-gt_emory_data_arm2 %>% filter(!is.na(a24_sbp1)) %>% select(id, visit, a24_sbp1) %>% group_by(visit) %>% count() %>% 
  select(visita=visit, Cantidad_medidas_Presion_a24b=n)

list(
  Presion_m14b=m14b,
  Presion_a24b=a24b
) %>% writexl::write_xlsx("output/conteo_presiones.xlsx")


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent_c) %>% group_by(
  s4_consent_c
) %>% filter(is.na(s4_consent_c))


listado_bebes %>% left_join(
gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent_c)
) %>%  mutate(
  flag_expo=if_else(
    fecha_nacimiento<"2019-01-15","1", NA_character_
  )
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_c_urine, b10_tc_urine, 
                                                           b10_tc_u_code, b10_tc_spots,
                                                           b10_tc_b_code)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
  ) %>%  select(
    id, flag_expo,b10_date,s4_consent_c, b10_c_urine, b10_tc_urine,b10_tc_spots) %>% 
  group_by(s4_consent_c,b10_tc_urine,b10_tc_spots,flag_expo) %>% count() %>% writexl::write_xlsx("output/conteos_muestras.xlsx")
  
# filter(s4_consent_c=='1') %>% filter(is.na(b10_tc_urine) & is.na(b10_tc_spots)) %>% 
#   arrange(s4_date)
  #filter(s4_consent_c=='0') %>% filter(b10_tc_urine=='1') 



listado_bebes %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% select(id, s4_date, s4_consent_c)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(b10_date)) %>% select(id, b10_date, b10_tc_urine, 
                                                           b10_tc_u_code, b10_tc_spots,
                                                           b10_tc_b_code)
)%>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% filter(s4_consent_c=='1') %>% filter(is.na(b10_tc_urine)) %>% mutate(
  flag_expo=if_else(
    `24_meses`<"2021-01-15","1", NA_character_
  )
) %>% group_by(flag_expo) %>% count()


listado_bebes %>% anti_join(
  gt_hapin_II_data %>% select(id, s4_consent_c) %>% filter(s4_consent_c=='0')
) %>% group_by(
  lubridate::year(`36_meses`),
  lubridate::month(`36_meses`)
) %>% count() %>% writexl::write_xlsx("output/conteos_36_meses.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id_estudio=id) %>% left_join(
gt_participants
) %>% filter(com_jalapa=="ACHIOTES JUMAY") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id_estudio=id, visit, m10_sleep) %>% 
    filter(visit=="baseline")
) %>% select(com_jalapa, m10_sleep)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id_estudio=id) %>% left_join(
  gt_participants
) %>% filter(com_jalapa=="CORONA") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id_estudio=id, visit, m10_sleep) %>% 
    filter(visit=="baseline")
) %>% select(com_jalapa, m10_sleep)

gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id_estudio=id) %>% left_join(
  gt_participants
) %>% filter(com_jalapa=="ARAISAPO") %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(m10_date)) %>% select(id_estudio=id, visit, m10_sleep) %>% 
    filter(visit=="baseline")
) %>% select(com_jalapa, m10_sleep) %>% arrange(m10_sleep)


gt_emory_data_arm2 %>% filter(!is.na(e3_date))

candidatos_hapin_36_meses<-gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob) %>% mutate(
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
) %>% filter(descartar=="No")

listado_bebes %>% group_by(
  lubridate::year(`36_meses`),
  lubridate::month(`36_meses`)
  
) %>% count() %>% writexl::write_xlsx("output/ninos_36_meses_por_mes.xlsx")


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
  select(
    id, s4_date, s4_consent, s4_consent_c
) %>% filter(s4_date>="2022-03-07") %>% anti_join(
  gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(s4_date>="2022-03-07") %>% 
    select(id) 
  
)

gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(s4_date)) %>% 
  select(
    id, s4_date, s4_by, s4_consent, s4_consent_c
  ) %>% left_join(
    gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(h41_date_v2)) %>% 
      select(id, h41_date_v2)
  ) %>% arrange(s4_date, h41_date_v2) %>% writexl::write_xlsx("output/consentimientos_36m.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% group_by(v1_phone, v1_by) %>% count()
gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id, v1_date, v1_phone) %>% arrange(desc(v1_date))


gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% filter(e3_date_exit>"2020-07-20") %>%  group_by(
  lubridate::year(e3_date_exit),
  lubridate::month(e3_date_exit)
) %>% count() %>% writexl::write_xlsx("output/salidas.xlsx")

dt_llamadas_covid<-read_csv("c:/temp/HAPINGuatemalaRepeti_DATA_2022-03-23_1117.csv")

dt_llamadas_covid %>% filter(cov_by=="JGC") %>% group_by(id_hapin,cov_by) %>% count() %>% arrange(desc(n)) %>% writexl::write_xlsx("output/revision_covid.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(e3_date_exit)) %>% filter(e3_date_exit>"2020-07-20") %>% select(
  id, e3_date_exit
) %>% anti_join(
  gt_emory_data_arm2 %>% filter(!is.na(v1_date)) %>% select(id)
) %>% anti_join(
  dt_llamadas_covid %>% filter(cov_by=="JGC") %>% group_by(id_hapin) %>% select(id=id_hapin)
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, `Celular embarazada`,
                                 `Celular esposo`, Celular_otro_miembro) %>% group_by(
                                   id
                                 ) %>% slice(1)
)
dt_llamadas_covid %>% filter(cov_by=="JGC") %>% select(id=id_hapin, cov_by)


gt_hapin_II_data %>% filter(!is.na(s4_date)) %>% filter(redcap_event_name=="24_month_arm_1") %>%  select(
  id, s4_date, s4_by, s4_consent, s4_consent_c, s4_owa,s4_ocon_date, s4_o_reason, s4_note
) %>%left_join(
  gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% filter(redcap_event_name=="24_month_arm_1") %>% 
    select(id, fecha_m11=m11_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(c33_date)) %>% filter(
    redcap_event_name=="24_month_arm_1"
  ) %>% select(id, fecha_c33=c33_date)
) %>% left_join(
  gt_hapin_II_data %>% filter(!is.na(a23_date)) %>% filter(redcap_event_name=="24_month_arm_1") %>% 
    select(id, fecha_a23=a23_date)
) %>% mutate(
  consintió_madre=if_else(s4_consent=="1", "Si","N0"),
  consintió_nino=if_else(s4_consent_c=="1", "Si","N0"),
  consintió_adulta=if_else(!is.na(s4_ocon_date), "Si","N0")
) %>% select(
  id, fecha_s4=s4_date, iniciales_S4=s4_by, consintió_madre, consintió_nino, consintió_adulta,
  fecha_m11, fecha_c33, fecha_a23
) %>% writexl::write_xlsx("output/revision_consentimientos_24m.xlsx")

gt_hapin_II_data %>% filter(!is.na(m10_date)) %>% select(
  id, m10_date, m10_by, m10_land
) %>% filter(is.na(m10_land))

gt_hapin_II_data %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% filter(!is.na(s4_date)) %>% 
  select(
    id, s4_date, s4_by, s4_consent, s4_consent_c, s4_owa,s4_ocon_date, s4_o_reason, s4_note
  ) %>%left_join(
    gt_hapin_II_data %>% filter(!is.na(m11_date)) %>% filter(redcap_event_name=="year_3_q1_36m_arm_1") %>% 
      select(id, fecha_m11=m11_date)
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(c33_date_2)) %>% filter(
      redcap_event_name=="year_3_q1_36m_arm_1"
    ) %>% select(id, fecha_c33=c33_date_2)
  ) %>%  mutate(
    consintió_madre=if_else(s4_consent=="1", "Si","N0"),
    consintió_nino=if_else(s4_consent_c=="1", "Si","N0")
  ) %>% left_join(
    gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(
      redcap_event_name=="year_3_q1_36m_arm_1"
    ) %>% select(id, fecha_ch41=h41_date_v2)
  ) %>% select(
    id, fecha_s4=s4_date, iniciales_s4=s4_by, consintió_madre, consintió_nino,
    fecha_m11, fecha_c33, fecha_ch41
  ) %>% writexl::write_xlsx("output/revision_consentimientos_36m.xlsx")

f1_data<-read_csv("c:/temp/Ecolectivos_DATA_2022-05-05_1731.csv")
z1_data<-read_csv("c:/temp/Ecolectivos_DATA_2022-05-05_1813.csv")
z1_data<-z1_data %>% filter(!is.na(z1_date)) %>% select(id_censo, z1_date, z1_name_contac1, z1_contact_cel_1, 
                                              z1_name_contact2, z1_contact_cel_2)
dt_f1_hc<-f1_data %>% select(id_censo, f1_date, f1_by, f1_notes, matches("^f1_fuel_other")) 

dt_f1_hc %>% mutate(
  hc7= f1_fuel_other___0 + f1_fuel_other___1 + f1_fuel_other___2
  + f1_fuel_other___3 + f1_fuel_other___4 + f1_fuel_other___5 +
    f1_fuel_other___6 + f1_fuel_other___7 + f1_fuel_other___9 +
    f1_fuel_other___10 + f1_fuel_other___11 + f1_fuel_other___12 +
    f1_fuel_other___13 + f1_fuel_other___14 + f1_fuel_other___15 +
    f1_fuel_other___555
) %>% select(id_censo, f1_date, f1_by, hc7, f1_notes) %>% filter(!is.na(f1_date) & hc7=="0") %>%  left_join(
  z1_data
) %>% 
  writexl::write_xlsx(
  "output/revision_hc7_contacto.xlsx"
)

0, No usa otros combustibles
1, Alcohol/etanol
2, Gasolina/diesel (no en generador)
3, Kerosene
4, Carbón sin procesar
5, Carbón briquetas/pelotillas
6, GLP
7, Leña
9, Residuos de la agricultura o los cultivos/pasto/paja/arbustos/mazorcas de maíz
10, Residuos animales/estiércol
11, Pelotillas de biomasa procesada/briquetas
12, Astillas de madera o Chiriviscos
13, Basura (no plástico)
14, Aserrín
15, Plástico
555, Otro
