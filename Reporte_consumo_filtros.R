# load use packages
library(package = "tidyverse")
#leer csv de filtor redcap
filtros<-read_csv("data/HAPINGuatemalaFiltro_DATA_2019-04-09_1550.csv")

#filtros de 47mm
#filtros_47mm<-
filtros %>% filter(redcap_event_name=="almacenamiento_arm_2") %>% gather(key=variable, value=value, -tracked_filter_date) %>% 
  mutate(
    filtro=case_when(
     grepl("tracked_filter_id_", variable) ~ value,
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(filtro)) %>% mutate(
    medida=case_when(
      grepl("3P4",filtro) ~ "47",
      grepl("3M3",filtro) ~ "37",
      grepl("3P0",filtro) ~ "37",
      grepl("3M5",filtro) ~ "15",
      grepl("3P5",filtro) ~ "15",
      TRUE ~ NA_character_
    )
  ) %>% mutate(
    year=lubridate::year(tracked_filter_date),
    mes=lubridate::month(tracked_filter_date)
  ) %>% group_by(year,mes,medida) %>% summarize(
    contador=n()
  ) %>% arrange(year,mes,medida) %>%  select("Año"=year, "Mes"=mes, "Tamaño de Filtro"=medida, "Cantidad consumida"=contador) %>% 
  writexl::write_xlsx(paste("output/consumo_filtros_",Sys.Date(),".xlsx"))
