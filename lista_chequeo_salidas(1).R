
library(package = "tidyverse")

# Datos del H56
#source(file = "scripts/0_get_datos_h56.R", encoding = "UTF-8")
# leer los datos de arm2

 salidas_uvg <- read_csv(
   file = "data/exports/FinalizacinHAPIN_DATA_2020-03-02_0927.csv",
   col_types = cols(.default = col_character())
 ) %>%
   print()
 
 salidas_emory<-gt_emory_data_arm2 %>% filter(!is.na(h56_date)) %>% transmute(id, h56_date)
 

 
 # Datos de vales
source(file = "scripts/0_get_control_vales_data.R", encoding = "UTF-8")
#canjes de vales
all_canjes <- canje_vales1 %>% 
  select(
    id = record, fecha_canje, matches("id_vale[0-9]$")
  ) %>% bind_rows( 
    list(
      canje_vales2 %>% 
      select(
        id = record, fecha_canje, matches("id_vale[0-9]$")
      )
    ,
    canje_vales3 %>% 
      select(
        id = record, fecha_canje, matches("id_vale[0-9]$")
      )
  )
  ) %>% 
  gather(key, vale, -id, -fecha_canje, na.rm = TRUE) %>%
  print()


#suma de vales canjeados por hogar
canjes <- all_canjes %>%
  group_by(id) %>%
  summarize(
    vales_canjeados = vale %>% unique() %>% length()
  ) %>%
  print()


lista_chequeo <- gt_emory_data_arm2 %>%
  filter(grepl("b4_", redcap_event_name)) %>%
  left_join(
    gt_emory_data_arm2 %>%
      filter(!is.na(s6_date)) %>%
      transmute(
        id,
        brazo = recode(
          s6_arm,
          "1" = "intervención",
          "0" = "control"
        )
      )
  ) %>%
  left_join(canjes) %>%
  left_join(
    gt_emory_data_arm1 %>%
      filter(!is.na(s4_main_id)) %>%
      select(id = s4_main_id, id_tamizaje = id)
  ) %>%
  select(
    id, id_tamizaje, brazo, vales_canjeados,
    matches("(m10|m11|m14b?|m19|a21|a26a|b10|c31|c353|c42|h41|h55)_(date|by)")
    # -matches("oaw|a21|h41.+_2_2|b10.+_2")
  ) %>%
  arrange(substr(id, 1, 2), is.na(m10_date), brazo) %>%
  anti_join(
    bind_rows (
    salidas %>% select(id),
    salidas_uvg %>% select(id=record_id),
    salidas_emory %>% select(id)
  ) 
  )%>%
  # filter(!is.na(m10_date), grepl("^35", id)) %>%
  print()


lista_chequeo %>% left_join(
  comunidades %>% select(id=id_estudio, comunidad_1=community, comunidad_2=community_new)
) %>%
  writexl::write_xlsx(
    "output/lista-chequeo-salidas.xlsx"
  )

# #REPORTE DE SALIDAS
# salidas_uvg<-salidas_uvg %>% select(id=record_id, fecha=h56g_date) %>% filter(!is.na(fecha))
# salidas_uvg<-salidas_uvg  %>% anti_join(
#   salidas_emory
# )
# list(
# #TOTAL h56 REGISTRADOS
# total_salidas<-salidas_emory %>% bind_rows(
#   salidas_uvg %>% transmute(id, h56_date=as.Date(fecha))
# ) ,
# #SALIDAS DE LA SEMANA
# salidas_semanales<-salidas_emory %>% bind_rows(
#   salidas_uvg %>% transmute(id, h56_date=as.Date(fecha))
# ) %>% filter(h56_date>="2020-06-01" & h56_date<="2020-06-07")
# ) %>% writexl::write_xlsx("output/conteo_salidas_h56.xlsx")
# 
# 
# #revision entregas de efectivo
# entregas<-read_csv("D:/Descargas/entregas.csv")
# entregas %>% mutate_all(as.character) %>% left_join(
#   salidas_semanales %>% transmute(ID=id, h56_date )
# ) %>% write_csv("output/entregas_salidas.csv")
