library(tidyverse)
source("scripts/0_get_osm_uvg.R")

intensivos_local<-gt_osm_uvg_data %>% filter(!is.na(hapi_date)) %>% select(id, fecha= hapi_date,iniciales= hapi_by, notas=hapi_e_notes) %>% bind_rows(
  gt_osm_uvg_data %>% filter(!is.na(s4_date)) %>% select(id, fecha_s4=s4_date, iniciales=s4_by, razon=s4_reason)
) 

intensivos_local %>% anti_join(
gt_hapin_II_data %>% filter(redcap_event_name=="year4q3_54m_arm_1") %>% filter(!is.na(h41_date_v2)) %>% select(id) 
) %>% writexl::write_xlsx(paste0("output/listado_intensivos_pendientes_en_emory_", Sys.Date(), ".xlsx"))
