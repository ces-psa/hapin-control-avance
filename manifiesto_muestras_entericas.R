library(tidyverse)


dt_entericas<-read_csv("data/entericas/HAPINentericas_DATA_2023-01-27_1020.csv")

dt_entericas_lab<-read_csv("data/entericas/HAPINGuatemalaLab_DATA_2023-01-27_1033.csv")
dt_entericas_lab<-dt_entericas_lab %>% filter(!is.na(aliquot_container_id_ent))

dt_entericas_muestras_redcap<-dt_entericas %>% transmute(id=as.character(c39_id),
                      visita=substr(redcap_event_name,1,2),
                      id_muestra=c39_id_sample) %>% filter(!is.na(id_muestra)) %>% mutate(
                        id_hh=substr(id_muestra,1,5)
                      ) 

dt_alicuotas<-dt_entericas_lab %>% select(
  correlativo, aliquot_container_id_ent, alicuot_storage_date_v2, matches("aliquot_id_")
) %>% gather(
  key = "variable" , value = "value",  -correlativo, 
  -aliquot_container_id_ent, -alicuot_storage_date_v2 ) %>% select(
    correlativo, caja=aliquot_container_id_ent, fecha=alicuot_storage_date_v2, id_alicuota=value
  ) %>% filter(!is.na(id_alicuota)) %>%  mutate(
    id_alicuota=str_replace(id_alicuota, "--", "-")
  ) %>% group_by(id_alicuota) %>% mutate(id_hapin=substr(id_alicuota,1,5)) 

#revision datos faltantes
dt_alicuotas %>% anti_join(
  dt_entericas_muestras_redcap %>% select(id_hapin=id_hh)
)

#revision datos sin alicuotas
dt_entericas_muestras_redcap %>% anti_join(
  dt_alicuotas %>% select(id_hh=id_hapin)
)


dt_alicuotas %>% select(Sample_ID=id_alicuota, Box=caja, id_hapin) %>% arrange(Sample_ID) %>% group_by(
  id_hapin
) %>% mutate(
  cantidad=row_number()
) %>% 
  writexl::write_xlsx(
  "output/alicuotas_entericas.xlsx"
)
