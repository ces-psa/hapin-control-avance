library(package = "tidyverse")
library(package = "crosstalk")
#library(package = "sf")

# Load helper functions
source(file = "scripts/zz_output.R")

# Participant information (z10)
source(file = "scripts/0_get_participants.R", encoding = "UTF-8")

# Pre-screening information (s0)
source(file = "scripts/0_get_prescreening.R", encoding = "UTF-8")

# Exposure supporting data (h41 start)
source(file = "scripts/0_get_exposure_start.R", encoding = "UTF-8")
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
# gt_emory_data %>%
#   select(id, s1_ultra, matches("m17.+(fl|crl|hc)"), s4_main_id) %>%
#   filter(s1_ultra == 1) %>%
#   print(n = Inf)

# Emory RedCap dictionary
source(file = "scripts/reporte_gas.R", encoding = "UTF-8")

source(file = "scripts/0_classify_data.R", encoding = "UTF-8")
#catalogo comunidades Jalapa
cat_comunidades<-read_csv("data/exports/cat_comunidades.csv")
serie_estufas<-read_csv("D:/Descargas/serie_estufas.csv")

gt_emory_data_arm3 %>% 
  #select(id,h52_date, h52_repair___1, h52_repair___3, h52_repair___4, h52_repair___555, h52_repair_other, h52_repair___2 ) %>% 
  select(id,h52_date, h52_repair___1, h52_repair___555, 
         h52_repair___2, h52_repair___3, h52_repair___4,
         h52_repair_other, h52_stove___1, h52_stove___2, 
         h52_stove___3 , h52_stove___4,h52_stove___5,h52_stove___6,
         h52_stove___7,h52_stove___555, h52_stove_other ) %>% 
  filter(!is.na(h52_date)) %>% mutate(
    h52_repair___1=if_else(
      condition = h52_repair___1=="1",
      true = "Reparacion Estufa:",
      false = ""
    )) %>% mutate(
      h52_repair___2=if_else(
        condition = h52_repair___2=="1",
        true = "Cilindro de gas / regulador:",
        false = ""
      )) %>% mutate(
        h52_repair___3=if_else(
          condition = h52_repair___3=="1",
          true = "Tubos de hule:",
          false = ""
        )) %>% mutate(
          h52_repair___4=if_else(
            condition = h52_repair___4=="1",
            true = "Valvula de intercambio:",
            false = ""
          )) %>% mutate(h52_repair___555=if_else(
            condition = h52_repair___555=="1",
            true = h52_repair_other            ,
            false = ""
          )) %>%  mutate(h52_stove___1=if_else(
            condition = h52_stove___1=="1",
            true = "Ninguno, se inspecciono la estufa y se encontro que funcionaba bien",
            false = ""
          )) %>% mutate(h52_stove___2=if_else(
            condition = h52_stove___2=="1",
            true = "Perilla de la estufa reemplazado o arreglado",
            false = ""
          )) %>% mutate(h52_stove___3=if_else(
            condition = h52_stove___3=="1",
            true = "Quemador de estufa reemplazado o reparado",
            false = ""
          )) %>% mutate(h52_stove___4=if_else(
            condition = h52_stove___4=="1",
            true = "Valvula de estufa engrasada",
            false = ""
          )) %>% mutate(h52_stove___5=if_else(
            condition = h52_stove___5=="1",
            true = "Valvula de la estufa reemplazada o reparada",
            false = ""
          )) %>% mutate(h52_stove___6=if_else(
            condition = h52_stove___6=="1",
            true = "Comal reemplazado",
            false = ""
          )) %>% mutate(h52_stove___7=if_else(
            condition = h52_stove___7=="1",
            true = "Estufa reemplazado",
            false = ""
          )) %>% mutate(h52_stove___555=if_else(
            condition = h52_stove___555=="1",
            true = h52_stove_other,
            false = ""
          )) %>%  select(id,h52_date, h52_repair___1, h52_repair___2, 
                         h52_repair___3, h52_repair___4, h52_repair___555, 
                         h52_stove___1, h52_stove___2, h52_stove___3, 
                         h52_stove___4, h52_stove___5,h52_stove___6,
                         h52_stove___7,h52_stove___555) %>% arrange(h52_date) %>% 
  group_by(id) %>% mutate(
    cor=1:n(),
    h52_date= as.character(h52_date)) %>% 
  gather(
    key=columna, value = reparaciones, -id, -cor
  ) %>% 
  filter(reparaciones!="", !is.na(reparaciones)) %>% 
  group_by(id, cor) %>% summarize(
    reparaciones= paste(reparaciones, collapse=", ")) %>% 
  spread(key = cor, value = reparaciones, fill = "") %>% left_join(
    serie_estufas %>% mutate(
      id=as.character(house_id)
    ) %>%  select(id, serie)
  ) %>% mutate(
        valvula_engrasada= if_else(grepl("engrasada",`1`)=="TRUE",1L,0L),
        valvula_engrasada= if_else(grepl("engrasada",`2`)=="TRUE", 
                                  as.numeric(valvula_engrasada)+1, as.numeric(valvula_engrasada))
            ) %>% mutate(
              `1`=gsub(":,",": ",`1`),
              `2`=gsub(":,",": ",`2`)
            ) %>% 
      select(id, "Primera Reparación"=`1`, "Segunda Reparacion"=`2`, valvula_engrasada,  "Número de Serie"=serie) %>% print() %>% 
          writexl::write_xlsx(paste("output/Reparaciones_estufas_",Sys.Date(),".xlsx"))

