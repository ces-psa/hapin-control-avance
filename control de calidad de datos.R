


#REVISION DE DATOS FECHA DE NACIMIENTO EN C31 Y C35, COMPARADOS CONTRA C30
gt_emory_data_arm2 %>% select(id, visit, matches("c31")) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, fecha_nacimiento=c30_dob)
) %>% select(id, visit, c31_dob, fecha_nacimiento) %>% filter(!is.na(c31_dob)) %>% mutate(
 flag1=if_else(as.Date(c31_dob)==as.Date(fecha_nacimiento), "0","1") 
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c35_date)) %>% select(id, visit, c35_date, c35_by, c35_dob)
) %>% mutate(
  flag2=if_else(as.Date(c35_dob)==as.Date(fecha_nacimiento),"0","1")
) %>% filter(flag1=="1" | flag2=="1") %>% select(
  id, visit, c31_dob, c35_dob, fecha_nacimiento
) %>% writexl::write_xlsx(paste0("output/inconsistencia_fechas_nacimiento_al_",Sys.Date(),".xlsx"))



gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob) %>% anti_join(
  salidas %>% select(id)
)

#revisión de fechas probable de parto M17 Vrs C30
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_dob,c30_edd,c30_age) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_date, m17_by, m17_ga)
) %>% mutate(
  flag1=if_else(as.Date(c30_edd)==as.Date(m17_ga),"0","1")
) %>% filter(flag1=="1") %>% select(
  id_tamizaje,
  id,
  fecha_c30=c30_date,
  fecha_nacimiento=c30_dob,
  age_c30=c30_age,
  fecha_m17=m17_date,
  iniciales_m17=m17_by,
  FPP_c30=c30_edd,
  FPP_m17=m17_ga
) %>% mutate(
  `c30 - m17`= as.Date(FPP_c30) - as.Date(FPP_m17)
) %>% left_join(
  datos_participantes %>% select(id=`ID estudio`, fpp_z10=`Fecha esperada de parto`)
) %>% 
  
  writexl::write_xlsx(
  "output/inconsistencias_FPP_c30_m17_20200928.xlsx"
)

#control de calidad variables C30
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, matches("c30_")) %>% 
  select(id, c30_date, c30_by, c30_wt_record, c30_wt1_time,c30_wt1,c30_wt2, c30_wt3,c30_cloth)  %>% filter(!is.na(c30_wt1_time)) %>% left_join(
    gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, c30a_wt1)
  ) %>% arrange(c30_wt2) %>% select(id, c30_by, c30_wt1,c30_cloth) %>% arrange(c30_cloth) %>%    print(n=Inf)
group_by(
  c30_wt1
) %>% count() %>% arrange(c30_wt1) %>% print(n=Inf)


#c30 peso de ropa
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, matches("c30_")) %>% 
  select(id, c30_date, c30_by, c30_cloth, c30_cloth_wt ) %>% 
  filter(c30_cloth=="1") %>% print(n=Inf)

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, matches("c30_")) %>% 
  select(id, c30_date, c30_by, c30_cloth, c30_cloth_wt ) %>% 
  filter(c30_cloth=="1") %>% print(n=Inf)

c30_head1

#c30 longitudes
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_by,c30_ht1_time,c30_head1,c30_head2,c30_head3) %>% 
  mutate(
    dif_2_1=as.numeric(c30_head2) - as.numeric(c30_head1),
    dif_3_2=as.numeric(c30_head3) - as.numeric(c30_head2)
  )%>% arrange(dif_2_1) %>% 
  print(n=Inf)

#Control de calidad entéricas, tamizadas vrs muestras colectadas
# Emory RedCap export data entericas
source(file = "scripts/0_get_entericas_data.R", encoding = "UTF-8")

dt_tamizadas_entericas<-gt_emory_data_arm2 %>% filter(!is.na(s8_date)) %>% select(id, s8_date, s8_consent) %>% mutate(consentimiento=recode(
                                                                  s8_consent,"1"="Si", "0"="No"
)) %>% select(id, fecha_tamizaje=s8_date, consentimiento)

muestras_colectadas<-data_entericas %>% select(id=c39_id, c39_date, c39_col_diaper, redcap_event_name) %>% mutate(
  visit= str_sub(redcap_event_name, 1,2)
) %>% transmute(id, fecha_muestra=c39_date, colecto_panal=recode(c39_col_diaper, "1"="Si", "2"="No"), visit) 

dt_tamizadas_entericas %>% left_join(
  muestras_colectadas
) %>% writexl::write_xlsx(paste0("output/datos_entericas_tamizaje_muestras_",Sys.Date(),".xlsx"))


gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob) %>% filter(c30_dob=="2019-09-14")
gt_emory_data_arm2 %>% filter(!is.na(c33_date)) %>% select(id, c33_date, visit) %>% filter(id=="33353")

data_llamadas<-read_csv("c:/temp/HAPINGuatemalaRepeti_DATA_2020-10-07_1250.csv")
data_llamadas %>%  select(id_hapin, cov_date) %>% arrange(cov_date) %>% filter(id_hapin=="33252")


#promedios de peso
gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob, c30_ave_wt, c30_ave_ht) %>% left_join(
  gt_emory_data_arm2 %>% filter(visit=="b1" & !is.na(c33_date)) %>% select(id, c33_ave_wt,c33_ave_ht)
) %>% arrange(c33_ave_ht) %>%  print(n=Inf)
                                                                               
