library(package = "tidyverse")
# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

#salidas
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_date, c30_dob, c30_sex, c30_ave_wt, c30_ave_ht, c30_wt, 
                                                           c30_wt_where,c30_wt_record, c30_wt_unit,
                                                           c30_head1,c30_head2,c30_head3,c30_complete_no,c30_complete_no_ot) %>% mutate(
                                                             c30_wt=recode(c30_wt,"1"="Si", "0"="No"),
                                                             c30_wt_where=recode(c30_wt_where,
                                                                          "1"= "Hospital publico",
                                                                          "2"="Hospital privado",
                                                                          "3"="Casa propia",
                                                                          "4"="Casa del trabajador de salud comunitario",
                                                                          "5"="Casa de otro miembro de la familia",
                                                                          "6"="Centro/puesto de salud publica",
                                                                          "7"="Clinica privada",
                                                                          "555"="Otro"),
                                                             c30_wt_unit=recode(
                                                               c30_wt_unit, "1"="Kilogramos.gramos","2"="Libras.onzas"
                                                             )
                                                           ) %>% 
  mutate(
    #convertir a gramos el peso en hospital
    c30_wt_gramos=case_when(c30_wt_unit=="Libras.onzas" ~ (as.double(c30_wt_record) * 454), 
                            c30_wt_unit=="Kilogramos.gramos" ~ (as.double(c30_wt_record) * 1000)
    )
   ) %>% mutate(
     promedio_cabeza=case_when(
      !is.na(c30_head3) ~  sum(as.double(c30_head1), as.double(c30_head2), as.double(c30_head3), na.rm = TRUE ) / 3,
      is.na(c30_head3) ~  sum(as.double(c30_head1), as.double(c30_head2), as.double(c30_head3), na.rm = TRUE ) / 2,
     )
   ) %>% mutate(
     c30_sex=recode(c30_sex, "1"="Masculino", "2"="Femenino", "3"="No se puede determinar")
   ) %>% left_join(
     gt_emory_data_arm2 %>% filter(!is.na(c30a_date)) %>% select(id, c30a_ave_wt, c30a_ave_ht,c30a_head1,c30a_head2 ) %>% mutate(
       promedio_cabeza_c30a=sum(as.double(c30a_head1), as.double(c30a_head2))/2
     )
   ) %>% mutate(
     longitud_hapin=if_else(is.na(c30_ave_ht),c30a_ave_ht,c30_ave_ht ),
     promedio_cabeza_hapin=if_else(is.na(promedio_cabeza),promedio_cabeza_c30a, promedio_cabeza ),
     prom_peso_grms_hapin=if_else(is.na(c30_ave_wt),c30a_ave_wt, c30_ave_wt)
   ) %>% mutate(
     c30_complete_no=recode(
       c30_complete_no, "1"= "Nino medicamente fragil",
       "2"="Nino medicamente fragil",
       "555"="Otro"
     )
   ) %>% 
select(id, c30_date, fecha_nacimiento=c30_dob, sexo=c30_sex,longitud_hapin, 
                promedio_cabeza_hapin, prom_peso_grms_hapin,
                 peso_grms_nohapin=c30_wt_gramos, peso_nohapin_tomado_en=c30_wt_where,
       c30_complete_no, c30_complete_no_ot) %>% writexl::write_xlsx("output/antro/pesos_tallas.xlsx")
