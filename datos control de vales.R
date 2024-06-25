library(tidyverse)

#catalogo de productos
cat_productos<-read_csv("data/dictionaries/cat_productos_canje.csv")

dt_hapin_1<-read_csv("C:/users/aramirez/Downloads/HAPINGuatemalaMainSt_DATA_2024-04-03_1144.csv")

#sacar listado de grupo control
dt_g_control<-dt_hapin_1 %>% filter(!is.na(s6_arm)) %>% transmute(id, 
                          grupo=recode(s6_arm,"1"="intervencion","0"="Control")) %>% filter(grupo=="Control")


dt_g_control<-dt_g_control %>% left_join(
  dt_hapin_1 %>% filter(!is.na(e3_date)) %>% select(id, date_exit=e3_date)
)

dt_entrega_vales<-read_csv("C:/users/aramirez/Downloads/entrega_vales.csv")
dt_canje_vales<-read_csv("C:/users/aramirez/Downloads/Canjes.csv")
dt_entrega_productos<-read_csv("C:/users/aramirez/Downloads/entrega_productos.csv")

dt_entrega_vales<-dt_entrega_vales %>% mutate(
  id=as.character(record_id)) %>% select(id, everything(),-record_id)

dt_canje_vales<-dt_canje_vales %>% mutate(
  id=as.character(record_id)) %>% select(id, everything(),-record_id)


dt_entrega_productos<-dt_entrega_productos %>% mutate(
  id=as.character(record_id)) %>% select(id, everything(),-record_id)

cat_productos %>% write_csv("output/cat_products.csv")
dt_entrega_vales %>% write_csv("output/dt_vales_entregados.csv")
dt_canje_vales %>% write_csv("output/dt_canje_vales.csv")
dt_entrega_productos %>% write_csv("output/dt_entrega_productos.csv")

