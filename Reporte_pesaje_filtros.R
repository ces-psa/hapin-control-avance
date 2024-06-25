
library(package = "tidyverse")


#-----------------------------------------------------------------
#all data filter
# Get all data filters from last  export
filtros_file <- list.files(
  path = "data/exports", pattern = "HAPINGuatemalaFiltro_DATA.+csv", full.names = TRUE
) %>%
  tibble(
    file = .,
    export_time = file %>%
      gsub(".+?_([-0-9_]+).csv", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(export_time))


dt_filtros <- filtros_file %>% 
  pull(file) %>%
  read_csv(col_types = cols(.default = col_character()))
 



#dt_filtros<-read_csv("C:/temp/HAPINGuatemalaFiltro_DATA_2022-01-21_1132.csv")

dt_filtros %>% group_by(redcap_event_name) %>% count()

#filtros pesados en UVG
dt_filtros %>% filter(redcap_event_name=="registro_nuevos_fi_arm_1") %>% select(
record_id, matches("^new")
) %>% filter(new_filter_source=="UVG")

#datos arm3 pesaje
#registrados en existencia
existencias_inventarios<-dt_filtros %>% filter(redcap_event_name=="existencias_arm_3") %>% select(
  record_id, matches("^weighning")
) %>% group_by(tipo=weighning_size_filter) %>% summarize(cantidad=sum(as.numeric(weighning_amount_filter)))

#pesaje de filtros
pesaje_filtros<-dt_filtros %>% filter(redcap_event_name=="pesaje_arm_3") %>% select(
  record_id, tracked_filter_by_pesaje,
  tracked_filter_quantity_pesaje, tracked_filter_date_pesaje,
  tracked_filter_operation_pesaje, discarded_filter_reason_pesaje,
  discarded_filter_comment_pesaje, tracked_type_filter_pesaje,
  tracked_filter_ambient_file, matches("^tracked_filter_id_"),
  matches("^tracked_filter_pic_")
) %>% select(record_id,matches("_pesaje"))

pesaje_filtros %>% gather(key = "variable", value="value",

  - tracked_filter_by_pesaje,
  -tracked_filter_date_pesaje,
  -tracked_filter_operation_pesaje
) %>% filter(!is.na(value)) %>% 
  transmute(
   
    tracked_filter_by_pesaje,
    tracked_filter_date_pesaje,
    tracked_filter_operation_pesaje,
    id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
  ) %>% filter(!is.na(id_filtro)) %>% group_by(tracked_filter_operation_pesaje) %>% 
  count()

#conteo de filtros trabajados

pesaje_filtros %>% group_by(tracked_filter_by_pesaje,
                            tracked_filter_operation_pesaje,
                            tracked_type_filter_pesaje) %>% summarize(
cantidad_filtros=sum(as.numeric(tracked_filter_quantity_pesaje))  
) %>% mutate(
  usuario=recode(tracked_filter_by_pesaje, "10"="Yunuen", "11"="Jorge")
) %>% select(usuario,  2,3,4)

#eventualidades
# dt_eventualidades<-read_csv("c:/temp/HAPINGuatemalaFiltro_DATA_2022-01-21_1206.csv")
# dt_eventualidades %>% writexl::write_xlsx("output/eventualidades_pesaje.xlsx")

#lista de filtros enviados

dt_filtros %>% group_by(redcap_event_name) %>% count()
dt_filtros %>% filter(redcap_event_name=="descarte_arm_2") %>% filter(
  !is.na(tracked_filter_date)
) %>% select(record_id, matches("^tracked"), matches("^discarded")) %>% group_by(
  discarded_filter_reason, 
) %>% count()

#revisar filtros con postpeso que no tienen prepesaje

pesaje_filtros %>% gather(key = "variable", value="value",
                          
                          - tracked_filter_by_pesaje,
                          -tracked_filter_date_pesaje,
                          -tracked_filter_operation_pesaje
) %>% filter(!is.na(value)) %>% 
  transmute(
    
    tracked_filter_by_pesaje,
    tracked_filter_date_pesaje,
    tracked_filter_operation_pesaje,
    id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
  ) %>% filter(!is.na(id_filtro)) %>% filter(
    tracked_filter_operation_pesaje=="postweigh"
  ) %>% anti_join(
    
    pesaje_filtros %>% gather(key = "variable", value="value",
                              
                              - tracked_filter_by_pesaje,
                              -tracked_filter_date_pesaje,
                              -tracked_filter_operation_pesaje
    ) %>% filter(!is.na(value)) %>% 
      transmute(
        
        tracked_filter_by_pesaje,
        tracked_filter_date_pesaje,
        tracked_filter_operation_pesaje,
        id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
      ) %>% filter(!is.na(id_filtro)) %>% filter(
        tracked_filter_operation_pesaje=="preweigh"
      ) %>% select(id_filtro)
  ) %>% left_join(
    pesaje_filtros %>% select(record_id,  tracked_filter_by_pesaje,
                              tracked_filter_date_pesaje,
                              tracked_filter_operation_pesaje)
  ) %>% select(record_id, everything())
  writexl::write_xlsx("output/postpeso_sin_prepesaje.xlsx")






pesaje_filtros %>% gather(key = "variable", value="value",
                          
                          - tracked_filter_by_pesaje,
                          -tracked_filter_date_pesaje,
                          -tracked_filter_operation_pesaje
) %>% filter(!is.na(value)) %>% 
  transmute(
    
    tracked_filter_by_pesaje,
    tracked_filter_date_pesaje,
    tracked_filter_operation_pesaje,
    id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
  ) %>% filter(!is.na(id_filtro)) %>% filter(
    tracked_filter_operation_pesaje=="preweigh" 
  ) %>% anti_join(
    
    pesaje_filtros %>% gather(key = "variable", value="value",
                              
                              - tracked_filter_by_pesaje,
                              -tracked_filter_date_pesaje,
                              -tracked_filter_operation_pesaje
    ) %>% filter(!is.na(value)) %>% 
      transmute(
        
        tracked_filter_by_pesaje,
        tracked_filter_date_pesaje,
        tracked_filter_operation_pesaje,
        id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
      ) %>% filter(!is.na(id_filtro)) %>% filter(
        tracked_filter_operation_pesaje=="postweigh"
      ) %>% select(id_filtro)
  ) %>% writexl::write_xlsx("output/prepesaje_sin_postpeso.xlsx")


pesaje_filtros %>% gather(key = "variable", value="value",
                          
                          - tracked_filter_by_pesaje,
                          -tracked_filter_date_pesaje,
                          -tracked_filter_operation_pesaje
) %>% filter(!is.na(value)) %>% 
  transmute(
    
    tracked_filter_by_pesaje,
    tracked_filter_date_pesaje,
    tracked_filter_operation_pesaje,
    id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
  ) %>% filter(!is.na(id_filtro)) %>% filter(
    tracked_filter_operation_pesaje=="postweigh"
  ) %>% writexl::write_xlsx("output/postpesos_registrados.xlsx")


pesaje_filtros %>% gather(key = "variable", value="value",
                          
                          - tracked_filter_by_pesaje,
                          -tracked_filter_date_pesaje,
                          -tracked_filter_operation_pesaje
) %>% filter(!is.na(value)) %>% 
  transmute(
    
    tracked_filter_by_pesaje,
    tracked_filter_date_pesaje,
    tracked_filter_operation_pesaje,
    id_filtro=if_else(grepl("tracked_filter_id",variable),value, NA_character_)
  ) %>% filter(!is.na(id_filtro)) %>% filter(
    tracked_filter_operation_pesaje=="preweigh" 
  ) %>% writexl::write_xlsx("output/prepesos_registrados.xlsx")
