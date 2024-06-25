
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

library("tidyverse")


#E6. ¿El nino ha presentado diarrea definido como 3 o mas deposiciones sueltas (no solidas/puede tomar la forma del contenedor) 
#con un periodo de 24 horas?
#c36_diarrhea:
###1 Si, en el ultimo dia (ej. En las ultimas 24 horas), 
#2 Si, en los ultimos 2-7 dias (desde este mismo dia de la semana pasada), 
#0 No

c36_diarrea<-gt_emory_data_arm2 %>% filter(!is.na(c36_date)) %>% select(id,c36_diarrhea ) %>% mutate(
  Q_E6=recode(c36_diarrhea,"1"="Si, en el ultimo dia", "2"="Si, en los ultimos 2-7 dias", "0"="No")
) %>% bind_rows(
  gt_emory_data_arm2 %>% filter(!is.na(c36a_date)) %>% select(id,c36a_diarrhea ) %>% mutate(
    Q_E6=recode(c36a_diarrhea,"1"="Si, en el ultimo dia", "2"="Si, en los ultimos 2-7 dias", "0"="No")
  ) 
) %>% group_by(Q_E6) %>% summarize(n=n()) %>% writexl::write_xlsx("output/c36_diarrea.xlsx")

#frecuencias C36 y C36a
table(c36_diarrea$Q_E6)


#2.	C31 A5=4: 
#A5. Desde este mismo dia de la semana pasada, el nino ha tenido
#(LEER COMPLETAMENTE CADA RESPUESTA, SELECCIONE TODAS LAS QUE APLIQUEN)
#Nota: Incluye sintomas actuales. Congestion / secrecion nasal por si sola no se incluye. 

#1141 registros
gt_emory_data_arm2 %>% filter(!is.na(c31_date)) %>% select(id, redcap_event_name, c31_illness___1,c31_illness___2,c31_illness___3,
                                                           c31_illness___4,c31_illness___0) %>% 
  mutate(
  A5_diarrea=if_else(c31_illness___4=="1", "Diarrea definida como las asientos de 3 o mas de heces sueltas en 24 horas.", NA_character_)
) %>% select(id, A5_diarrea, redcap_event_name) %>% group_by(redcap_event_name, A5_diarrea) %>% summarize(n=n()) %>% writexl::write_xlsx("output/frecuencias_diarrea.xlsx")

#frecuencias c31
table(c31_diarrea$A5_diarrea)

#3.	C41 12 y 13
#“What was the child’s primary diagnosis for this illness?”
#Q12= c41_diagnosis
#Q13 = c41_diagnosis_2
gt_emory_data_arm2 %>% filter(!is.na(c41_date)) %>% select(id, c41_diagnosis, c41_diagnosis_2) %>% filter(
  grepl("DIAR", c41_diagnosis) | grepl("DIAR", c41_diagnosis_2)
) %>% writexl::write_xlsx("output/c41_diarrea.xlsx")


#otras diarreas, en C81 y C82
#C81
gt_emory_data_arm2 %>% filter(!is.na(c81_date)) %>% select(id, c81_date, c81_diarrhea)
#hay tres registros pero no se indica diarrea

#C82
gt_emory_data_arm2 %>% filter(!is.na(c82_date)) %>% select(id, c82_date, c82_diarrhea, c82_repeat_type___2)
#no hay registros de C82
