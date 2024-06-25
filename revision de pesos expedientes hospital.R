


#revision pesos redcap
data_hospital<-read_csv("C:/HAPIN/data_pesos_hospital.csv")
data_hospital<-data_hospital %>% mutate_all(as.character)

data_hospital %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% 
    transmute(id, c30_by, c30_date, peso_hosp_rc=c30_wt_record,
              unidad_hosp_rc=recode(c30_wt_unit,"1"="kilogramos.gramos","2"="libras.onzas"), 
              c30_head1,c30_head2,c30_head3
    ) 
  %>% 
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


