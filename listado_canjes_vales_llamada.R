library(package="tidyverse")
# buscar el archivo mas reciente
vales_file <- list.files(
  path = "data/vales",
  pattern = "HAPINcontrolvales_DATA",
  full.names = TRUE
) %>%
  tibble(
    file = .,
    date = file %>%
      sub(".+HAPINcontrolvales_DATA_([0-9_-]{15}).+", "\\1", .) %>%
      lubridate::ymd_hm(tz = "America/New_York")
  ) %>%
  slice(which.max(date)) %>%
  print()


# cargar los datos del export mas reciente
vales_data <- read_csv(
  file = vales_file$file,
  col_types = cols(.default = col_character())
) %>%
  print()


#catalogo de productos
cat_productos<-read_csv("data/dictionaries/cat_productos_canje.csv")

vales_data %>% filter(!is.na(id_vale)) %>% select(id=record_id, redcap_event_name, id_vale) %>% filter(grepl("^VALE", id_vale)) %>% 
  mutate(
    entrega=substr(redcap_event_name,12,17)
  ) %>% select(-redcap_event_name) %>% mutate(
    visit=case_when(
      entrega=="vale_1" ~ "p2",
      entrega=="vale_2" ~ "parto",
      entrega=="vale_3" ~ "b3",
      entrega=="vale_4" ~ "b4"
    )
  )

#LISTADO PARA REGISTRAR TELEFONICAMENTE LAS ENTREGAS DE VALES PENDIENTES
gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(visit)) %>% select(id, visit) %>% filter(visit=="p2" | visit=="parto" | visit=="b2" | visit=="b4")
) %>% left_join(
  #hacer join con todas las entregas de vales
  vales_data %>% filter(!is.na(id_vale)) %>% select(id=record_id, redcap_event_name, id_vale) %>% filter(grepl("^VALE", id_vale)) %>% 
    mutate(
      entrega=substr(redcap_event_name,12,17)
    ) %>% select(-redcap_event_name) %>% mutate(
      visit=case_when(
        entrega=="vale_1" ~ "p2",
        entrega=="vale_2" ~ "parto",
        entrega=="vale_3" ~ "b2",
        entrega=="vale_4" ~ "b4"
      )
    )
  
) %>%  left_join(
  datos_participantes %>% select(id_tamizaje=`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`, `Nombre otra adulta`, 
                                  `Celular embarazada`, `Celular esposo`,`Celular_otro_miembro`)
) %>% filter(is.na(id_vale)) %>% 
  writexl::write_xlsx(paste0("output/entregas_vale_pendientes_",Sys.Date(),".xlsx"))

#LISTADO DE TODOS LOS VALES ENTREGADOS EN LOS HOGARES
gt_emory_data_arm2 %>% filter(s6_arm=="0") %>% select(id) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(visit)) %>% select(id, visit) %>% filter(visit=="p2" | visit=="parto" | visit=="b2" | visit=="b4")
) %>% left_join(
  #hacer join con todas las entregas de vales
  vales_data %>% filter(!is.na(id_vale)) %>% select(id=record_id, redcap_event_name, id_vale) %>% filter(grepl("^VALE", id_vale)) %>% 
    mutate(
      entrega=substr(redcap_event_name,12,17)
    ) %>% select(-redcap_event_name) %>% mutate(
      visit=case_when(
        entrega=="vale_1" ~ "p2",
        entrega=="vale_2" ~ "parto",
        entrega=="vale_3" ~ "b2",
        entrega=="vale_4" ~ "b4"
      )
    )
  
) %>%  left_join(
  datos_participantes %>% select(id_tamizaje=`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`, `Nombre otra adulta`, 
                                 `Celular embarazada`, `Celular esposo`,`Celular_otro_miembro`)
) %>% filter(!is.na(id_vale)) %>% 
  writexl::write_xlsx(paste0("output/entregas_vale_realizadas_",Sys.Date(),".xlsx"))



#LISTADO PARA ENTREGAS DE VALES WAGNER
#LISTA DE ENTREGAS
lista_vales_entregados<-vales_data %>% filter(!is.na(id_vale)) %>% select(id=record_id, redcap_event_name, id_vale) %>% filter(grepl("^VALE", id_vale)) %>% 
  mutate(
    entrega=substr(redcap_event_name,12,17)
  ) %>% select(-redcap_event_name) %>% mutate(
    visit=case_when(
      entrega=="vale_1" ~ "p2",
      entrega=="vale_2" ~ "parto",
      entrega=="vale_3" ~ "b2",
      entrega=="vale_4" ~ "b4"
    ),
    monto=500
  ) %>% bind_rows(
    #agregamos los montos entregados en vales de saldo
    vales_data %>% filter(!is.na(id_vale_saldo)) %>% select(id=record_id, redcap_event_name, id_vale_saldo,monto_vale_saldo) %>% mutate(
      entrega=substr(redcap_event_name,9,15),
      visit=NA_character_,
      monto= as.numeric(monto_vale_saldo)
    ) %>% select(id, id_vale=id_vale_saldo, visit, entrega, monto)
  )  %>% print()



#LISTA DE CANJES
lista_vales_canjeados<-vales_data %>% filter(!is.na(id_vale1)) %>% select(record_id, id_vale=id_vale1, redcap_event_name) %>% bind_rows(
  vales_data %>% filter(!is.na(id_vale2 )) %>% select(record_id, id_vale=id_vale2, redcap_event_name)
)%>% bind_rows(
  vales_data %>% filter(!is.na(id_vale3 )) %>% select(record_id, id_vale=id_vale3, redcap_event_name)
) %>% bind_rows(
  vales_data %>% filter(!is.na(id_vale4 )) %>% select(record_id, id_vale=id_vale4, redcap_event_name)
) %>%   mutate(
  entrega=substr(redcap_event_name,1,7),
  visit=NA_character_,
  monto=500
) %>% select(id=record_id, id_vale, entrega, monto) %>% bind_rows(
vales_data %>% filter(!is.na(id_vale_saldo_1)) %>% select(record_id, redcap_event_name, id_vale=id_vale_saldo_1,monto=monto_vale_saldo_1) %>%
  bind_rows(
    vales_data %>% filter(!is.na(id_vale_saldo_2)) %>% select(record_id, redcap_event_name, id_vale=id_vale_saldo_2, monto=monto_vale_saldo_2)
  ) %>% 
  bind_rows(
    vales_data %>% filter(!is.na(id_vale_saldo_3)) %>% select(record_id, redcap_event_name, id_vale= id_vale_saldo_3, monto=monto_vale_saldo_3)
  ) %>%  mutate(
    entrega=substr(redcap_event_name,1,7),
    visit=NA_character_,
    monto=as.numeric(monto)
  )%>% select(id=record_id, id_vale, entrega, monto) 
) %>%   print()

#GENERAR LISTA DE VALES DISPONIBLES PARA CADA HOGAR INCLUYENDO VALES DE SALDO
lista_vales_entregados %>% anti_join(
  #DESCONTAMOS LOS VALES CANJEADOS
  lista_vales_canjeados %>% select(id_vale, monto)
) %>% arrange(id) %>% writexl::write_xlsx(paste0("output/lista_vales_activos_",Sys.Date(),".xlsx"))

# gt_emory_data_arm2 %>% filter(visit=="p1") %>% filter(!is.na(m18_date)) %>% select(id, m18_date, m18_by) %>% 
#   filter(m18_date=="2019-04-15")


#lista de hogares que estan en periodo de B4 y la cantidad de vales entregados
participantes_b4<-gt_emory_data_arm2 %>% filter(!is.na(s6_arm)) %>% select(id, brazo=s6_arm) %>% filter(brazo=="0") %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select(id_tamizaje=id, id=s4_main_id)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(m17_ga)) %>% select(id_tamizaje=id, fpp=m17_ga)
) %>% anti_join(
  salidas %>% select(id)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>% select(id, c30_dob)
) %>% mutate(
  fecha_nacimiento=if_else(is.na(c30_dob), as.Date(fpp), as.Date(c30_dob)),
  edad_dias=(Sys.Date()- as.Date(fecha_nacimiento)),
  edad_meses=(Sys.Date()- as.Date(fecha_nacimiento)) / 30.25
) %>% filter(edad_dias>=351) %>% select(id_tamizaje, id, fecha_nacimiento, edad_meses) %>% anti_join(
  salidas %>% select(id)
)

#cantidad de vales entregados

# Datos de vales
source(file = "scripts/0_get_control_vales_data.R", encoding = "UTF-8")
#canjes de vales
all_entregas <- entrega_vales1 %>% select(id=record_id, id_vale) %>% bind_rows(
  list(
  entrega_vales2 %>% select(id=record_id, id_vale),
  entrega_vales3 %>% select(id=record_id, id_vale),
  entrega_vales4 %>% select(id=record_id, id_vale)
  )
) %>% group_by(id) %>% count() %>% print()

participantes_b4 %>% left_join(
  all_entregas %>% select(id, cantidad_vales_entregados=n)
) %>% filter(cantidad_vales_entregados<4) %>%  left_join(
  datos_participantes %>% select(id_tamizaje=`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`, `Nombre otra adulta`, 
                                 `Celular embarazada`, `Celular esposo`,`Celular_otro_miembro`)
) %>% writexl::write_xlsx(paste0("output/b4_pendientes_vales_al_",Sys.Date(),".xlsx"))


entrega_productos1 %>% select(record_id, fecha_entrega=fecha_entrega_producto, id_vale1=id_vale1_v2) %>% bind_rows(
  entrega_productos1 %>% select(record_id, fecha_entrega=fecha_entrega_producto, id_vale2=id_vale2_v2)
) %>% bind_rows(
  entrega_productos1 %>% select(record_id, fecha_entrega=fecha_entrega_producto, id_vale3=id_vale3_v2)
) %>% print(n=Inf)

#canjes por cada visita, reporte para Lisa

vales_data %>% group_by(redcap_event_name) %>% count
canjes<-vales_data %>% filter(redcap_event_name=="canje_1_arm_2" | redcap_event_name=="canje_2_arm_2" |
                        redcap_event_name=="canje_3_arm_2" |
                        redcap_event_name=="canje_4_arm_2") %>% 
  select(id=record_id, fecha_canje, id_vale1, id_vale2,
           id_vale3, id_vale4, redcap_event_name) %>% mutate(
             visita=case_when(
               !is.na(id_vale4) ~ "b4",
               !is.na(id_vale3) & is.na(id_vale4) ~ "b2",
               !is.na(id_vale2) & is.na(id_vale3) ~ "birth",
               !is.na(id_vale1) & is.na(id_vale2) ~ "p2"
             )
           ) %>%filter(!is.na(visita)) 

dt_entregas_vales<-vales_data %>%  filter(redcap_event_name=="entrega_de_vale_1_arm_1" | redcap_event_name=="entrega_de_vale_2_arm_1" |
                         redcap_event_name=="entrega_de_vale_3_arm_1" |
                         redcap_event_name=="entrega_de_vale_4_arm_1") %>% 
  select(record_id, redcap_event_name, fecha_entrega, id_vale) %>% mutate(
    visita=case_when(
      redcap_event_name=="entrega_de_vale_1_arm_1" ~ "p2",
      redcap_event_name=="entrega_de_vale_2_arm_1" ~ "birth",
      redcap_event_name=="entrega_de_vale_3_arm_1" ~ "b2",
      redcap_event_name=="entrega_de_vale_4_arm_1" ~ "b4",
    )
  )

canjes %>% select(id, fecha_canje, id_vale=id_vale1) %>% bind_rows(
  list(
  canjes %>% select(id, fecha_canje, id_vale=id_vale2) %>% filter(!is.na(id_vale)),
  canjes %>% select(id, fecha_canje, id_vale=id_vale3) %>% filter(!is.na(id_vale)),
  canjes %>% select(id, fecha_canje, id_vale=id_vale4) %>% filter(!is.na(id_vale))
  )
)

dt_cantes<-read_csv("output/test_vales_canje.csv")
dt_canjes

dt_entregas_vales %>% select(-redcap_event_name) %>% left_join(
  dt_canjes %>% select(id_vale, fecha_canje)
)
