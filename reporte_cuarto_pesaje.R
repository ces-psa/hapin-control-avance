# load use packages
library(package = "tidyverse")
library(package = "crosstalk")
library(package= "kableExtra")

data_pesaje <- read_csv("data/exports/HAPINGuatemalaFiltro_DATA_2022-07-25_1019.csv")

data_pesaje %>% group_by(redcap_event_name) %>% count()

#datos registradosen existencias
existencias_new<-data_pesaje %>% filter(redcap_event_name=="existencias_arm_3") %>% select(
  record_id, matches("^weighing"), matches("^weighning_")
) %>% filter(!is.na(weighing_new_box_date)) %>% group_by(weighning_size_filter) %>% summarize(
  cantidad=sum(weighning_amount_filter )
) %>% select(
  tipo_filtro=weighning_size_filter ,
  cantidad
) %>% ungroup()


existencias_new


#datos registrados en pre pesaje
filtros_pesados<-data_pesaje %>% filter(redcap_event_name=="pesaje_arm_3") %>% 
  select(record_id, matches("^tracked")) %>% filter(
    !is.na(tracked_filter_quantity_pesaje)
  ) %>% group_by(
    tracked_filter_operation_pesaje, 
    tracked_type_filter_pesaje
  ) %>% summarize(
    cantidad=sum(tracked_filter_quantity_pesaje)
  )

#existencias inventario nuevos
existencias_new %>% left_join(
  filtros_pesados %>% transmute(
    trabajo_realizado=tracked_filter_operation_pesaje,
    tipo_filtro=tracked_type_filter_pesaje,
    cantidad_trabajado=cantidad
  )
) %>% mutate(
  existencias=as.numeric(cantidad) - as.numeric(cantidad_trabajado)
  ) %>% filter(trabajo_realizado=="preweigh") %>% select(
  tipo_filtro, cantidad_nueva=cantidad, cantidad_prepesada=cantidad_trabajado, 
  existencias
)

#filtros 
filtros_pesados %>% filter(tracked_filter_operation_pesaje=="preweigh") %>% left_join(
  filtros_pesados %>% filter(tracked_filter_operation_pesaje=="postweigh") %>% 
    select(trabajo_realizado=tracked_filter_operation_pesaje, tracked_type_filter_pesaje, cantidad_post=cantidad)
) %>% group_by(tracked_type_filter_pesaje) %>% summarize(
  pendientes_postpeso= sum(cantidad) - sum(cantidad_post)
)

#tabla eventualidades
data_pesaje %>%  filter(!is.na(evnet_date)) %>% select(
  record_id, evnet_date, matches("^event")
) %>% arrange(desc(evnet_date))


# Revision de datos con Jorge ----
filtros_prepesados<-data_pesaje %>% filter(redcap_event_name=="pesaje_arm_3") %>% select(record_id, tracked_filter_date_pesaje,
                                                                     tracked_filter_operation_pesaje,
                                                                     tracked_type_filter_pesaje,
                                                                     matches("^tracked_filter_id_")) %>% 
  filter(tracked_filter_operation_pesaje=="preweigh") 

filtros_prepesados %>%  select(-tracked_filter_operation_pesaje, -tracked_type_filter_pesaje) %>% gather(
   key = "variable", value = "value",  -tracked_filter_date_pesaje, -record_id) %>% 
  filter(!is.na(value)) %>% transmute(record_id, fecha_pesaje=tracked_filter_date_pesaje, id_filtro=value) %>% 
  group_by(
    id_filtro
  ) %>% mutate(
    tipo_filtro=substr(id_filtro,1,3)
  ) %>% group_by(tipo_filtro) %>% count()

