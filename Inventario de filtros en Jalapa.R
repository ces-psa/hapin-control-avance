#conteo de filtros en Jalapa
library(tidyverse)

dt_inventario<-read_csv("data/filtros/HAPINGuatemalaFiltro_DATA_2022-08-11_1047.csv")
dt_main_study<-read_csv("data/filtros/HAPINGuatemalaMainSt-H41H41OAWH41bcomplet_DATA_2022-08-11_1306.csv")

inventario_15mm<-dt_inventario %>% filter(new_filter_source=="UVG") %>% arrange(
  new_filter_date
) %>% filter(new_filter_size=="15mm") %>% select(
  record_id, new_filter_date, matches("^new_filter_id_")
) %>% gather(
  key = "variable", value = "value", -record_id, -new_filter_date
) %>% filter(!is.na(value)) %>% transmute(
  record_id, fecha_recepcion=new_filter_date, id_filtro=value
)

#revisar lotes
inventario_15mm %>% writexl::write_xlsx("output/revision_lotes_filtros_15mm.xlsx")


#sacar listado de filtros de 15 usados en b4 hapin 1
h41_ms<-dt_main_study %>% filter(!is.na(h41_date)) %>% filter(
  as.Date(h41_date)>="2021-03-09"
) %>% select(id, matches("^h41_"))

h41b_ms<-dt_main_study %>% filter(!is.na(h41b_date)) %>% filter(
  as.Date(h41_date)>="2021-03-09"
) %>% select(id, matches("^h41b_"))

#sacar listado de filtros usados en 24 meses

h41_24m<-gt_hapin_II_data %>% filter(!is.na(h41_date)) %>%
filter(
    as.Date(h41_date)>="2021-03-09"
  ) %>% select(id, matches("^h41_"))

gt_hapin_II_data %>% filter(!is.na(h41_date_2)) %>% group_by(visit) %>% count()
gt_hapin_II_data %>% filter(!is.na(h41_date)) %>% group_by(visit) %>% count()
gt_hapin_II_data %>% filter(!is.na(h41_date_v2)) %>% group_by(visit) %>% count()

#sacar listado de filtros usados en 36 meses

