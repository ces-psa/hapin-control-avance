data_h43_estufas<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% transmute(id, 
       grupo=recode(s6_arm, "1"="Intervencion","0"="Control")) %>% left_join(
gt_hapin_II_data %>% filter(!is.na(h43_date_2)) %>% filter(visit=="b7" ) %>% select(
  id, fecha_h43=h43_date_2, h43_stove1_2, h43_stove2_2
) %>% filter(h43_stove1_2=="6" | h43_stove2_2=="6")
)

cat_estufas<-read_csv("data/dictionaries/cat_hogares_estufas.csv")

cat_estufas<-cat_estufas %>% mutate_all(as.character) %>% select(id, tipo_estufa=Clasificacion)

data_h43_estufas %>% left_join(
  cat_estufas %>% select(id, estufa_canjeada_control=tipo_estufa)
) %>% writexl::write_xlsx("output/revision_estufas_gas_h43_48m.xlsx")
