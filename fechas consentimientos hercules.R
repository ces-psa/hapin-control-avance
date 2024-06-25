library(tidyverse)

dt_consentimientos_hercules<-read_csv("c:/temp/Hercules_DATA_2023-08-03_1433.csv")

dt_consentimientos_hercules %>% select(id=record_id, fecha_consentimiento=h_date, 
                                    acepp=h_consent_accept, id_hercules=h_id_hercules   ) %>% write_csv("output/consent_date_hercules.csv")

