# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

library("tidyverse")

gt_emory_data_arm2 %>%
  filter(grepl("^35[0-9]{3}", id), !is.na(b10_date) | !is.na(b10_date_2)) %>%
  select(
    id, visit, matches("^b10_date"), matches("hba1c")
  ) %>%
  #select(-matches("date")) %>%
  left_join(
    gt_emory_data_arm1 %>%
      filter(!is.na(s4_main_id)) %>%
      select(id = s4_main_id, inscrita = s4_date)
  ) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(c30_date) | !is.na(c30a_date)) %>%
      transmute(
        id,
        parto = if_else(!is.na(c30_dob), as.Date(c30_dob), c30a_date),
        ya_nacio = !is.na(parto)
      )
  ) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(
        (!is.na(e2_date) & e2_title %in% c(2, 7)) |
          (!is.na(e1_date) & e1_title == 2)
      ) %>%
      transmute(
        id,
        evento_adverso = case_when(
          e1_title == 2 ~ "Aborto (<20 semanas)",
          e2_title == 2 ~ "Muerte fetal (>= semanas)",
          e2_title == 7 & e2_participant == 1 ~ "Muerte de la madre",
          e2_title == 7 & e2_participant == 3 ~ "Muerte del infante",
          e2_title == 7 & e2_participant == 2 ~ "Muerte de OAW",
          TRUE ~ "revisar!"
        )
      )
  ) %>%
  gather(variable, value, matches("hba1c"), na.rm = TRUE) %>% 
  # revisar si hay duplicadas
  #filter(duplicated(paste(id, visit)))
  select(-variable) %>%
  spread(visit, value) %>%
  print(n = Inf)



#------------------------------------------------------------------------------
#script 2
#-----------------------------------------------------------------------------
gt_emory_data %>%
  filter(grepl("^35[0-9]{3}", id), !is.na(b10_date) | !is.na(b10_date_2)) %>%
  select(
    id, visit, matches("^b10_date"), matches("hba1c")
  ) %>%
  #select(-matches("date")) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(s4_main_id)) %>%
      select(id = s4_main_id, inscrita = s4_date)
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(!is.na(c30_date) | !is.na(c30a_date)) %>%
      transmute(
        id,
        parto = if_else(!is.na(c30_dob), as.Date(c30_dob), c30a_date),
        ya_nacio = !is.na(parto)
      )
  ) %>%
  left_join(
    gt_emory_data %>%
      filter(
        (!is.na(e2_date) & e2_title %in% c(2, 7)) |
          (!is.na(e1_date) & e1_title == 2)
      ) %>%
      transmute(
        id,
        evento_adverso = case_when(
          e1_title == 2 ~ paste0("Aborto (<20 semanas, ", e2_start_date, ")"),
          e2_title == 2 ~ paste0("Muerte fetal (>= semanas, ", e2_start_date, ")"),
          e2_title == 7 & e2_participant == 1 ~ paste0("Muerte de la madre (", e2_death_date, ")"),
          e2_title == 7 & e2_participant == 2 ~ paste0("Muerte de OAW (", e2_death_date, ")"),
          e2_title == 7 & e2_participant == 3 ~ paste0("Muerte del infante (", e2_death_date, ")"),
          TRUE ~ "revisar!"
        )
      )
  ) %>%
  # conservar fecha junto con el valor medido
  mutate_at(vars(matches("date")), list(as.character)) %>% # conservar con formato de fecha
  gather(variable, value, matches("b10"), na.rm = TRUE) %>%
  # quitar las que tienen dos fechas de b10 pero solo un valor de hba1c
  slice(-95) %>%
  extract(variable, into = "variable", regex = ".+(date|hba1c).*") %>%
  # poner cada fecha a la par de su valor de hba1c
  spread(variable, value) %>%
  # juntarlas
  mutate(
    value = paste0(hba1c, " (", date, ")")
  ) %>%
  select(-date, -hba1c) %>%
  spread(visit, value) %>%
  print(n = Inf) %>% writexl::write_xlsx("output/reporte_owa_b10.xlsx")


gt_emory_data_arm2 %>% filter(!is.na(m14b_date) & visit=="baseline") %>% select(id, m14b_date,m14_spb_ave,m14_dpb_ave) %>% mutate(
  presion=case_when(
    m14_spb_ave<120 | m14_dpb_ave<80 ~ "normal",
    (m14_spb_ave>=120 & m14_spb_ave<=139) | (m14_dpb_ave>=80 & m14_dpb_ave<=89) ~ "prehipertensa",
    m14_spb_ave>=140 | m14_dpb_ave>=90 ~ "hipertensa",
    TRUE ~ NA_character_
  )
) %>% group_by(presion) %>% summarize(cantidad=n()
                                      )


  gt_emory_data_arm2 %>% filter(!is.na(a24b_date ) & visit=="baseline") %>% select(id, a24b_date, a24_spb_ave, a24_dpb_ave) %>%  mutate(
    presion=case_when(
      a24_spb_ave<120 | a24_dpb_ave<80 ~ "normal",
      (a24_spb_ave>=120 & a24_spb_ave<=139) | (a24_dpb_ave>=80 & a24_dpb_ave<=89) ~ "prehipertensa",
      a24_spb_ave>=140 | a24_dpb_ave>=90 ~ "hipertensa",
      TRUE ~ NA_character_
    )
  ) %>% filter(is.na(presion))
    group_by(presion) %>% summarize(cantidad=n())



#120-139 sistolico y/o 80 y 89 de diastolica es prehipertension
#140 o mas de sistolica y/o 90 o mas de diastolica son presion alta

gt_emory_data_arm2 %>% filter(!is.na(m14b_date) & visit=="baseline") %>% count() #711
gt_emory_data_arm2 %>% filter(!is.na(a24b_date) & visit=="baseline") %>% count() #129
table(gt_emory_data_arm2$visit)
