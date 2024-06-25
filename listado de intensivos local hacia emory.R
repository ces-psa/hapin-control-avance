library(tidyverse)
#extraer datos de osm
source("scripts/0_get_osm_uvg.R")
# Emory HAPIN II  
source(file = "scripts/0_get_hapinII.R", encoding = "UTF-8")

gt_osm_uvg_data %>% filter(!is.na(hapi_date)) %>% select(id, fecha=hapi_date, iniciales=hapi_by, 
                                                         notas=hapi_e_notes, notas2=hapi_f_notes) %>% anti_join(
  gt_hapin_II_data %>% filter(visit=="b7b8") %>% filter(!is.na(h41_date_v2)) %>% select(id)
) %>% writexl::write_xlsx("output/intensivos_pendientes_traslado_emory.xlsx")
