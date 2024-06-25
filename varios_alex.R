# load use packages
library(package = "tidyverse")
library(package = "crosstalk")

# Load helper functions
source(file = "scripts/zz_output.R")

# Emory RedCap dictionary

source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
#datos para reporte de nacimientos
m17<-gt_emory_data %>% select(id,m17_date, m17_ga) %>% filter(!is.na(m17_date)) %>%print()
inscritas<-gt_emory_data %>% select(id,s4_main_id,s4_date) %>% filter(!is.na(s4_main_id)) %>%
  left_join(m17) %>% print() %>% write_csv("inscritas_m17.csv")

gt_emory_data %>% select(id,e3_date) %>% filter(!is.na(e3_date)) %>% write_csv("salidas_estudio.csv")

c30<-gt_emory_data %>% select(id,c30_date,c30_age,c30_delivery) %>% filter(!is.na(c30_date)) %>% print()

e1<-gt_emory_data %>% select(id,e1_date,e1_title) %>%filter(!is.na(e1_date)) %>%  print()
e2<-gt_emory_data %>% select(id,e2_date, e2_title) %>% filter(!is.na(e2_date)) %>% print()

inscritas %>% select(id_tamizaje=id, id=s4_main_id, s4_date, m17_date, m17_ga) %>% left_join(c30) %>% 
  left_join(e1) %>% left_join(e2) %>% write_csv("inscritas_c30_e1_e2.csv")
gt_emory_data %>% select(id,e1_date,e1_title) %>% filter(e1_title=="2") %>%print()

#Datos para reporte de uso de estufas
#casas de intervencion
intervencion<-gt_emory_data %>% select(id,s6_date, s6_arm) %>% filter(!is.na(s6_date) & s6_arm=="1") %>% print()
estufas<-gt_emory_data %>% select(id,h50_date, h50_traditional) %>% filter(!is.na(h50_date)) %>%   print()
intervencion %>% left_join(estufas) %>% print()
gt_emory_data %>% 
  select(id,h40_date, h40_visit1, h40_visit1_v2, h40_visit2,h40_visit2_v2) %>% 
    print()

#listado de e3_embarazada
e3<-gt_emory_data %>% select(id,e3_date,e3_by,e3_date_exit,e3_date_final,e3_reason, e3_voluntary___1,e3_voluntary___2,e3_voluntary___3,e3_voluntary___4,e3_voluntary___5,e3_voluntary___555,
                             e3_volunt_other) %>% filter(!is.na(e3_date)) 

e3$e3_reason<-recode_factor(e3$e3_reason, `1`="finalizacion del estudio",
                     `2`="No elegible", 
                     `3`="Retiro voluntario de laparticipante",
                     `4`="Retirada por el equipo del estudio",
                     `5`="Se mudo del area de estudio",
                     `6`="Fallecio",
                     `7`="Perdido durante el seguimiento",
                     `555`="Otro") %>% print()
e3$e3_voluntary___1<-recode_factor(e3$e3_voluntary___1, `1`="Procedimientos demasiado invasivos",
                                  `0`="" 
                                  ) 
e3$e3_voluntary___2<-recode_factor(e3$e3_voluntary___2, `1`="Los procedimientos consumen demasiado tiempo",
                                   `0`="" 
                                    )
e3$e3_voluntary___3<-recode_factor(e3$e3_voluntary___3, `1`="No ve valor en el estudio",
                                   `0`=""  
                                    )
e3$e3_voluntary___4<-recode_factor(e3$e3_voluntary___4, `1`="La familia no quiere que participe",
                                   `0`=""  
                                  )
e3$e3_voluntary___5<-recode_factor(e3$e3_voluntary___5, `1`="No quiere estar en el grupo asignado",
                                   `0`=""  
                                    )
e3$e3_voluntary___555<-recode_factor(e3$e3_voluntary___555, `1`="Otro, especifique",
                                     `0`=""  
                                  )
e3_embarazada<-unite(e3, retiro_voluntario, c(7:12), sep="  ", remove = TRUE)
e3_embarazada$retiro_voluntario<-str_trim  (e3_embarazada$retiro_voluntario)

e3_embarazada %>% write_csv("e3_embarazada.csv")


#e3_OWA
#listado 
e3_o<-gt_emory_data %>% select(id,e3_date_o,e3_by_o,e3_date_exit_o,e3_date_final_o,e3_reason_o, e3_voluntary_o___1,e3_voluntary_o___2,e3_voluntary_o___3,e3_voluntary_o___4,e3_voluntary_o___5,e3_voluntary_o___555,
                             e3_volunt_other_o) %>% filter(!is.na(e3_date_o)) 

e3_o$e3_reason_o<-recode_factor(e3_o$e3_reason_o, `1`="finalizacion del estudio",
                            `2`="No elegible", 
                            `3`="Retiro voluntario de laparticipante",
                            `4`="Retirada por el equipo del estudio",
                            `5`="Se mudo del area de estudio",
                            `6`="Fallecio",
                            `7`="Perdido durante el seguimiento",
                            `555`="Otro") %>% print()
e3_o$e3_voluntary_o___1<-recode_factor(e3_o$e3_voluntary_o___1, `1`="Procedimientos demasiado invasivos",
                                   `0`="" 
) 
e3_o$e3_voluntary_o___2<-recode_factor(e3_o$e3_voluntary_o___2, `1`="Los procedimientos consumen demasiado tiempo",
                                   `0`="" 
)
e3_o$e3_voluntary_o___3<-recode_factor(e3_o$e3_voluntary_o___3, `1`="No ve valor en el estudio",
                                   `0`=""  
)
e3_o$e3_voluntary_o___4<-recode_factor(e3_o$e3_voluntary_o___4, `1`="La familia no quiere que participe",
                                   `0`=""  
)
e3_o$e3_voluntary_o___5<-recode_factor(e3_o$e3_voluntary_o___5, `1`="No quiere estar en el grupo asignado",
                                   `0`=""  
)
e3_o$e3_voluntary_o___555<-recode_factor(e3_o$e3_voluntary_o___555, `1`="Otro, especifique",
                                     `0`=""  
)
e3_owa<-unite(e3_o, retiro_voluntario, c(7:12), sep="  ", remove = TRUE)
e3_owa$retiro_voluntario<-str_trim  (e3_owa$retiro_voluntario)

e3_owa %>%print() %>%  write_csv("e3_owa.csv") 


#e3_child
#listado 
e3_c<-gt_emory_data %>% select(id,e3_date_c,e3_by_c,e3_date_exit_c,e3_date_final_c,e3_reason_c, e3_voluntary_c___1,e3_voluntary_c___2,e3_voluntary_c___3,e3_voluntary_c___4,e3_voluntary_c___5,e3_voluntary_c___555,
                               e3_volunt_other_c) %>% filter(!is.na(e3_date_c)) 

e3_c$e3_reason_c<-recode_factor(e3_c$e3_reason_c, `1`="finalizacion del estudio",
                                `2`="No elegible", 
                                `3`="Retiro voluntario de laparticipante",
                                `4`="Retirada por el equipo del estudio",
                                `5`="Se mudo del area de estudio",
                                `6`="Fallecio",
                                `7`="Perdido durante el seguimiento",
                                `555`="Otro") %>% print()
e3_c$e3_voluntary_c___1<-recode_factor(e3_c$e3_voluntary_c___1, `1`="Procedimientos demasiado invasivos",
                                       `0`="" 
) 
e3_c$e3_voluntary_c___2<-recode_factor(e3_c$e3_voluntary_c___2, `1`="Los procedimientos consumen demasiado tiempo",
                                       `0`="" 
)
e3_c$e3_voluntary_c___3<-recode_factor(e3_c$e3_voluntary_c___3, `1`="No ve valor en el estudio",
                                       `0`=""  
)
e3_c$e3_voluntary_c___4<-recode_factor(e3_c$e3_voluntary_c___4, `1`="La familia no quiere que participe",
                                       `0`=""  
)
e3_c$e3_voluntary_c___5<-recode_factor(e3_c$e3_voluntary_c___5, `1`="No quiere estar en el grupo asignado",
                                       `0`=""  
)
e3_c$e3_voluntary_c___555<-recode_factor(e3_c$e3_voluntary_c___555, `1`="Otro, especifique",
                                         `0`=""  
)
e3_child<-unite(e3_c, retiro_voluntario, c(7:12), sep="  ", remove = TRUE)
e3_child$retiro_voluntario<-str_trim  (e3_child$retiro_voluntario)

e3_child %>%print() %>%  write_csv("e3_child.csv") 






e3 %>% print()
gt_emory_data %>% select(id,e3_date_o) %>% filter(!is.na(e3_date_o)) %>% print()
gt_emory_data %>% select(id,e3_date_c) %>% filter(!is.na(e3_date_c)) %>% print()
                                                        
