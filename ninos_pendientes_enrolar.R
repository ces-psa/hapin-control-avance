
#contabilizar la cantidad de niños pendientes de enrolar
gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id"=s4_main_id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select("id_tamizaje"=id,m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id,c30_dob)
) %>% mutate(
  nacio=if_else(
    is.na(c30_dob),"No", "Si"
  )
) %>% filter(nacio=="Si")
