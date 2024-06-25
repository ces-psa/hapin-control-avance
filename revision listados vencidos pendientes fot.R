library(tidyverse)

fot_pendientes<-read_csv("data/dictionaries/cat_fot_pendientes.csv")
fot_pendientes<-fot_pendientes %>% mutate_all(as.character)

pendientes_laura<-read_csv("c:/temp/pendientes_fot_laura.csv")
pendientes_laura<-pendientes_laura %>% select(id) %>% mutate_all(as.character)

pendientes_laura %>% anti_join(
  fot_pendientes
)

fot_pendientes %>% anti_join(
  pendientes_laura
)

gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% filter(visit=="b7") %>% select(
  id, fecha_exposicion=h41_date_v2
) %>% left_join(
  fot_pendientes %>% transmute(id, hacer_FOT="Si")
) %>% filter(!is.na(hacer_FOT)) %>% left_join(

gt_hapin_II_data %>% filter(!is.na(c86_date)) %>% filter(redcap_event_name=="year4_q1_48m_arm_1") %>% select(
  id, c86_date
)
) %>% filter(is.na(c86_date)) %>% View()
  writexl::write_xlsx("output/pendientes_fot_vencidos.xlsx")


