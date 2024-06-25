library(package = "tidyverse")
# Emory RedCap dictionary
source(file = "scripts/0_get_emory_dictionary.R", encoding = "UTF-8")

# Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")

#salidas
source(file = "scripts/0_get_salidas.R", encoding = "UTF-8")

#casas con cilindros de 100 libras
cilindros_100<-read_csv("data/cilindros100/cilindros100.csv")
cilindros_100<-cilindros_100 %>% mutate_all(as.character)

#sacar dias pendientes de participación en el hogar para cada casa de intervención
gt_emory_data_arm2 %>% filter(s6_arm=="1") %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% 
  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_date)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% 
  left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_date)) %>% select(id_tamizaje=id, m17_ga)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_dob)) %>% select(id, c30_dob)
) %>% mutate(
  fecha_nacimiento=if_else(
    is.na(c30_dob), as.character(m17_ga),as.character(c30_dob)
  ),
  ya_nacio=if_else(
    is.na(c30_dob), "No", "Si"
  )
) %>%select(-m17_ga, -c30_dob) %>% 
  mutate(
    fecha_finaliza=as.Date(fecha_nacimiento) +  lubridate::days(393)
  ) %>% mutate(
    dias_por_participar= fecha_finaliza - Sys.Date()
  ) %>% arrange(dias_por_participar) %>% filter(dias_por_participar>=0) %>% left_join(
    #esto sale del script de consumos promedios de gas, aqui se integran los cilindros de cien libras
    data_uso_gas_total %>% mutate(
      cyl_cien=case_when(house_id=="33040" ~ "Si",
                         house_id=="33498" ~ "Si",
                         house_id=="35107" ~ "Si",
                         house_id=="33324" ~ "Si",
                         house_id=="35089" ~ "Si",
                         house_id=="35033" ~ "Si",
                         house_id=="33487" ~ "Si",
                         house_id=="35081" ~ "Si",
                         house_id=="33624" ~ "Si",
                         house_id=="35130" ~ "Si",
                         house_id=="33394" ~ "Si",
                         house_id=="35135" ~ "Si",
                         TRUE ~ "No")
    ) %>%  select(id=house_id,libras_por_dia, cyl_cien )
  ) %>% mutate(
    refilles_25_por_comprar=(as.numeric(dias_por_participar) * as.numeric(libras_por_dia))/25
  ) %>% writexl::write_xlsx("output/proyeccion_compra_refill_gas_2020-11-08.xlsx")
 
 
  
