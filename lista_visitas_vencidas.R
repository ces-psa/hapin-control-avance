#libreria
library(package = "tidyverse")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Participant information (z10)
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

matriz_visitas_clinica<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% mutate(
    ya_nacio=if_else(is.na(c30_dob), "No", "Si")
  ) %>% mutate(
    fcc=m17_ga - lubridate::days(280),
    fecha_nacimiento=if_else(ya_nacio=="Si", as.Date(c30_dob), as.Date(m17_ga)),
    p1_start= fcc + lubridate::days(168),
    p1_end= fcc + lubridate::days(196),
    p2_start= fcc + lubridate::days(224),
    p2_end= fcc + lubridate::days(252),
    m1_start= fcc+ lubridate::days(16),
    m1_end= fcc+ lubridate::days(44),
    b1_start= as.Date(fecha_nacimiento) + lubridate::days(76),
    b1_end= as.Date(fecha_nacimiento) + lubridate::days(104),
    b2_start= as.Date(fecha_nacimiento) + lubridate::days(166),
    b2_end= as.Date(fecha_nacimiento) + lubridate::days(194),
    b3_start= as.Date(fecha_nacimiento) + lubridate::days(256),
    b3_end= as.Date(fecha_nacimiento) + lubridate::days(284),
    b4_start= as.Date(fecha_nacimiento) + lubridate::days(346),
    b4_end= as.Date(fecha_nacimiento) + lubridate::days(374),
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a21_date)) & visit=="p1") %>% select(id, fec_p1_m=m11_date, fec_p1_o=a21_date) 
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a21_date))  & visit=="p2") %>% select(id, fec_p2_m=m11_date, fec_p2_o=a21_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob) & visit=="parto") %>% select(id, fec_birth=c30_date, c30_dob)
  )  %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c31_date)  & visit=="m1") %>% select(id, fec_m1_m=c31_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a21_date))  & visit=="b1") %>% select(id, fec_b1_m=m11_date, fec_b1_o=a21_date)
  )  %>% left_join(
    gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a21_date))  & visit=="b2") %>% select(id, fec_b2_m=m11_date, fec_b2_o=a21_date)
  )  %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c31_date) & visit=="b3") %>% select(id, fec_b3_m=c31_date)
  )%>% left_join(
    gt_emory_data_arm2 %>% filter((!is.na(m11_date) | !is.na(a21_date)) & visit=="b4") %>% select(id, fec_b4_m=m11_date, fec_b4_o=a21_date)
  )  %>% 
  mutate(
  P1_m_fuera_ventana=if_else(
    fec_p1_m<p1_start | fec_p1_m >p1_end, "Si", "No"
  ),
  P1_o_fuera_ventana=if_else(
    fec_p1_o < p1_start | fec_p1_o > p1_end, "Si", "No"
  ),
  P2_m_fuera_ventana=if_else(
    fec_p2_m<p2_start | fec_p2_m>p2_end, "Si", "No"
  ),
  P2_o_fuera_ventana=if_else(
    fec_p2_o<p2_start | fec_p2_o>p2_end, "Si", "No"
  ),
  c30_fuera_ventana= if_else(
    as.Date(fec_birth) > as.Date(c30_dob) + lubridate::days(1), "Si", "No"
    ),
  M1_m_fuera_ventana=if_else( fec_m1_m < m1_start | fec_m1_m > m1_end, "Si", "No"),
  B1_m_fuera_ventana=if_else( fec_b1_m < b1_start | fec_b1_m > b1_end, "Si", "No"),
  B1_o_fuera_ventana=if_else( fec_b1_o < b1_start | fec_b1_o > b1_end, "Si", "No"),
  B2_m_fuera_ventana=if_else( fec_b2_m < b2_start | fec_b2_m > b2_end, "Si", "No"),
  B2_o_fuera_ventana=if_else( fec_b2_o < b2_start | fec_b2_o > b2_end, "Si", "No"),
  B3_m_fuera_ventana=if_else( fec_b3_m < b3_start | fec_b3_m > b3_end, "Si", "No"),
  B4_m_fuera_ventana=if_else( fec_b4_m < b4_start | fec_b4_m > b4_end, "Si", "No"),
  B4_o_fuera_ventana=if_else( fec_b4_o < b4_start | fec_b4_o > b4_end, "Si", "No")
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>%  group_by(visit=="libres1") %>% select(id,E7_ss1=e7_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>%  group_by(visit=="libres2") %>% select(id,E7_ss2=e7_date)
  ) %>% print()


#GENERAR EXCEL PARA CLINICA
list(
 P1 = matriz_visitas_clinica %>% filter(P1_m_fuera_ventana=="Si" | P1_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, p1_start, p1_end, 
                                                                                                fec_p1_m, fec_p1_o, P1_m_fuera_ventana, P1_o_fuera_ventana,
                                                                                                E7_ss1, E7_ss2, E7_ss3=""
                                                               ) %>% filter(p1_start>"2019-12-21"),
 P2 = matriz_visitas_clinica %>% filter(P2_m_fuera_ventana=="Si" | P2_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, p2_start, p2_end, fec_p2_m,fec_p2_o, 
                                                                                                P2_m_fuera_ventana, P2_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
 ) %>% filter(p2_start>="2019-12-21"),
 
 Nacimientos = matriz_visitas_clinica %>% filter(c30_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, fecha_nacimiento, c30_date=fec_birth, c30_fuera_ventana,
                                                                                     E7_ss1, E7_ss2, E7_ss3=""
 ) %>% filter(fecha_nacimiento>="2019-12-21"),

 M1 = matriz_visitas_clinica %>% filter(M1_m_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, m1_start, m1_end, fec_m1_m,
                                                                                                        M1_m_fuera_ventana,E7_ss1, E7_ss2, E7_ss3="" ) %>% filter(m1_start>="2019-12-21"),
 
 B1 = matriz_visitas_clinica %>% filter(B1_m_fuera_ventana=="Si" | B1_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b1_start, b1_end, fec_b1_m, fec_b1_o,
                                                                                                B1_m_fuera_ventana, B1_o_fuera_ventana,
                                                                                                E7_ss1, E7_ss2, E7_ss3="") %>% filter(b1_start>="2019-12-21"),
 B2 = matriz_visitas_clinica %>% filter(B2_m_fuera_ventana=="Si" | B2_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b2_start, b2_end, fec_b2_m,fec_b2_o, 
                                                                                                B2_m_fuera_ventana, B2_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
 ) %>% filter(b2_start>="2019-12-21"),
 B3 = matriz_visitas_clinica %>% filter(B3_m_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b3_start, b3_end, fec_b3_m, B3_m_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
 ) %>% filter(b3_start>="2019-12-21"),
 B4 = matriz_visitas_clinica %>% filter(B4_m_fuera_ventana=="Si" | B4_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b4_start, b4_end, fec_b4_m, fec_b4_o, 
                                                                                                B4_m_fuera_ventana, B4_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
 ) %>% filter(b4_start>="2019-12-21")
)  %>% writexl::write_xlsx(paste0("output/visitas_fuera_ventana_al_diciembre_","2020-06-15_",".xlsx"))


#LISTADO DE EXPOSICION
fecha_inicio="2019-12-21"
ml=30.25
matriz_visitas_exposicion <- gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id) %>% 
  left_join(
    gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
  ) %>% mutate(
    ya_nacio=if_else(is.na(c30_dob), "No", "Si")
  ) %>% mutate(
    fcc=m17_ga - lubridate::days(280),
    fecha_nacimiento=if_else(ya_nacio=="Si", as.Date(c30_dob), as.Date(m17_ga)),
    p1_start= fcc + lubridate::days(168),
    p1_end= fcc + lubridate::days(196),
    p2_start= fcc + lubridate::days(224),
    p2_end= fcc + lubridate::days(252),
    m1_start= fecha_nacimiento + lubridate::days(2),
    m1_end= fecha_nacimiento + lubridate::days(58),
    b1_start= as.Date(fecha_nacimiento) + lubridate::days(62),
    b1_end= as.Date(fecha_nacimiento) + lubridate::days(119),
    b2_start= as.Date(fecha_nacimiento) + lubridate::days(152),
    b2_end= as.Date(fecha_nacimiento) + lubridate::days(210),
    b3_start = as.Date(fecha_nacimiento) + lubridate::days(242),
    b3_end = as.Date(fecha_nacimiento) + lubridate::days(300),
    b4_start= as.Date(fecha_nacimiento) + lubridate::days(332),
    b4_end= as.Date(fecha_nacimiento) +  lubridate::days(391)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="p1") %>% mutate(
        fec_p1_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_p1_o=a24
      ) %>%   select(id, fec_p1_m, fec_p1_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="p2") %>% mutate(
        fec_p2_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_p2_o=a24
      ) %>%  select(id, fec_p2_m, fec_p2_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="m1") %>% mutate(
        fec_m1_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_m1_o=a24
      ) %>%  select(id, fec_m1_m, fec_m1_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="b1") %>% mutate(
        fec_b1_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_b1_o=a24
      ) %>%   select(id, fec_b1_m, fec_b1_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="b2") %>% mutate(
        fec_b2_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_b2_o=a24
      ) %>%  select(id, fec_b2_m, fec_b2_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="b3") %>% mutate(
        fec_b3_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_b3_o=a24
      ) %>% select(id, fec_b3_m, fec_b3_o)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(h41_date) | !is.na(h44_date) | !is.na(h55_date)) %>% select(id, a24=a24b_date, h41=h41_date, h44=h44_date, h55=h55_date, visit) %>% 
      filter(visit=="b4") %>% mutate(
        fec_b4_m=case_when(
          !is.na(h41) ~ h41,
          !is.na(h44) ~ h44,
          !is.na(h55) ~ h55
        ),
        fec_b4_o=a24
      ) %>%  select(id, fec_b4_m, fec_b4_o)
  ) %>% 
  mutate(
    P1_m_fuera_ventana=if_else(
      fec_p1_m<p1_start | fec_p1_m >p1_end, "Si", "No"
    ),
    P1_o_fuera_ventana=if_else(
      fec_p1_o < p1_start | fec_p1_o > p1_end, "Si", "No"
    ),
    P2_m_fuera_ventana=if_else(
      fec_p2_m<p2_start | fec_p2_m>p2_end, "Si", "No"
    ),
    P2_o_fuera_ventana=if_else(
      fec_p2_o<p2_start | fec_p2_o>p2_end, "Si", "No"
    ),
    M1_m_fuera_ventana=if_else( fec_m1_m < m1_start | fec_m1_m > m1_end, "Si", "No"),
    B1_m_fuera_ventana=if_else( fec_b1_m < b1_start | fec_b1_m > b1_end, "Si", "No"),
    B1_o_fuera_ventana=if_else( fec_b1_o < b1_start | fec_b1_o > b1_end, "Si", "No"),
    B2_m_fuera_ventana=if_else( fec_b2_m < b2_start | fec_b2_m > b2_end, "Si", "No"),
    B2_o_fuera_ventana=if_else( fec_b2_o < b2_start | fec_b2_o > b2_end, "Si", "No"),
    B3_m_fuera_ventana=if_else( fec_b3_m < b3_start | fec_b3_m > b3_end, "Si", "No"),
    B4_m_fuera_ventana=if_else( fec_b4_m < b4_start | fec_b4_m > b4_end, "Si", "No"),
    B4_o_fuera_ventana=if_else( fec_b4_o < b4_start | fec_b4_o > b4_end, "Si", "No")
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>% filter(visit=="libres1") %>% select(id,E7_ss1=e7_date)
  ) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>%  filter(visit=="libres2") %>% select(id,E7_ss2=e7_date)
  )  %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(e7_date)) %>%  filter(visit=="libres3") %>% select(id,E7_ss3=e7_date)
  )  %>% print()


#GENERAR EXCEL PARA EXPOSICION
list(
  P1 = matriz_visitas_exposicion %>% filter(P1_m_fuera_ventana=="Si" | P1_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, p1_start, p1_end, 
                                                                                                            fec_p1_m, fec_p1_o, P1_m_fuera_ventana, P1_o_fuera_ventana,
                                                                                                            E7_ss1, E7_ss2, E7_ss3=""
  ) %>% filter(p1_start> fecha_inicio),

    P2 = matriz_visitas_exposicion %>% filter(P2_m_fuera_ventana=="Si" | P2_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, p2_start, p2_end, fec_p2_m,fec_p2_o, 
                                                                                                            P2_m_fuera_ventana, P2_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
  ) %>% filter(p2_start>=fecha_inicio),

  
  M1 = matriz_visitas_exposicion %>% filter(M1_m_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, m1_start, m1_end, fec_m1_m,
                                                                                 M1_m_fuera_ventana,E7_ss1, E7_ss2, E7_ss3="" ) %>% filter(m1_start>=fecha_inicio),
  
  B1 = matriz_visitas_exposicion %>% filter(B1_m_fuera_ventana=="Si" | B1_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b1_start, b1_end, fec_b1_m, fec_b1_o,
                                                                                                            B1_m_fuera_ventana, B1_o_fuera_ventana,
                                                                                                            E7_ss1, E7_ss2, E7_ss3="") %>% filter(b1_start>=fecha_inicio),
  B2 = matriz_visitas_exposicion %>% filter(B2_m_fuera_ventana=="Si" | B2_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b2_start, b2_end, fec_b2_m,fec_b2_o, 
                                                                                                            B2_m_fuera_ventana, B2_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
  ) %>% filter(b2_start>=fecha_inicio),
  
  B3 = matriz_visitas_exposicion %>% filter(B3_m_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b3_start, b3_end, fec_b3_m, B3_m_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
  ) %>% filter(b3_start>=fecha_inicio),
  
  B4 = matriz_visitas_exposicion %>% filter(B4_m_fuera_ventana=="Si" | B4_o_fuera_ventana=="Si") %>% transmute(id_tamizaje, id, b4_start, b4_end, fec_b4_m, fec_b4_o, 
                                                                                                            B4_m_fuera_ventana, B4_o_fuera_ventana,E7_ss1, E7_ss2, E7_ss3=""
  ) %>% filter(b4_start>=fecha_inicio)
)  %>% writexl::write_xlsx(paste0("output/visitas_fuera_ventana_exposicion_Diciembre_al_","2020-06-15_",".xlsx"))

