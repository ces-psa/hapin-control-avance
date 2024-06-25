#control de recargas
# Pre-screening information (s0)
#source(file = "scripts/0_get_combustible.R", encoding = "UTF-8")
library(package = "tidyverse")
recargas_gas_data<-read_csv("data/exports/Hapincontrolvehiculo_DATA_2020-05-11_1447.csv") 
recargas_gas_data %>%  mutate(
  provider=case_when(
    proveedor=="1" ~ "Tropigas",
    proveedor=="2" ~ "Dagas",
    proveedor=="3" ~ "Otro",
    TRUE ~ NA_character_
  )
) %>%  arrange(fecha_recarga) %>% select(
  "Id_registro"=record_id,
  "Persona que recibe la recarga"=iniciales_recibe,
  "Persona que entrega recarga"=nombre_quien_entrega,
  "Placa vehiculo proveedor"=placa_proveedor,
  "Numero de boleta"=boleta,
  "Fecha de recarga"= fecha_recarga,
  "Hora de la recarga"=time,
  "Proveedor"=provider,
  "Cantidad de cilindros de 25"=cilindros_25,
  "Cantidad de cilindros de 100"=cilindros_100,
  "Total de recargas recibidas"=cantidad_recargas,
 # "Guatemala: Foto boleta"=imagen_guatemala,
  #"Jalapa: Foto boleta"=imagen_jalapa,
  "Obervaciones"=observaciones
  
) %>% write_csv("output/reporte_recargas_gas.csv")

#recargas_gas_data <- read_csv("D:/Descargas/Hapincontrolvehiculo_DATA_2020-03-16_1751.csv")
#recargas_gas
recargas_gas_data %>% select(fecha_recarga,foto_boleta)

#armar la tabla con el hipervinculo hacia las imagenes
recargas<-recargas_gas %>% mutate(doc_id=as.integer(foto_boleta)) %>%  left_join(
  edocs_metadata) %>% 
    mutate(
      imagen_guatemala=if_else(is.na(stored_name), NA_character_,
        paste0('=HYPERLINK(','"','\\', '\\','172.30.1.57','\\','Data','\\','Salud Ambiental','\\','HAPIN_JALAPA','\\','Reportes','\\','Vehiculos','\\','img','\\',stored_name,'"',')')
      ),
       imagen_jalapa=if_else(is.na(stored_name), NA_character_,
         paste0('=HYPERLINK(','"','z:\\', 'HAPIN_JALAPA','\\','Reportes','\\','Vehiculos','\\','img','\\',stored_name,'"',')')
       )
    ) %>% mutate(
      provider=case_when(
        proveedor=="1" ~ "Tropigas",
        proveedor=="2" ~ "Dagas",
        proveedor=="3" ~ "Otro",
        TRUE ~ NA_character_
      )
    ) %>%  arrange(fecha_recarga) %>% select(
      "Id_registro"=record,
      "Persona que recibe la recarga"=iniciales_recibe,
      "Persona que entrega recarga"=nombre_quien_entrega,
      "Placa vehiculo proveedor"=placa_proveedor,
      "Numero de boleta"=boleta,
      "Fecha de recarga"= fecha_recarga,
      "Hora de la recarga"=time,
      "Proveedor"=provider,
      "Cantidad de cilindros de 25"=cilindros_25,
      "Cantidad de cilindros de 100"=cilindros_100,
      "Total de recargas recibidas"=cantidad_recargas,
      "Guatemala: Foto boleta"=imagen_guatemala,
      "Jalapa: Foto boleta"=imagen_jalapa,
      "Obervaciones"=observaciones
      
    ) 

#exportar el set de datos a CSV
recargas %>% write_csv(paste0("output/recargas_gas_", Sys.Date(),".csv"))

#reporte de cantidad de recargas de gas por semana
recarga_semanal<-recargas %>% mutate(
                semana=lubridate::floor_date(as.Date(`Fecha de recarga`), unit = "weeks")
                                     )
#exportar reporte
recarga_semanal %>% group_by(semana) %>% summarize(
  contador=n(), fecha_semana=min(`Fecha de recarga`), total_recargas=sum(as.numeric(`Total de recargas recibidas`))
) %>% select("Semana del:"=fecha_semana,"Número de entregas:"=contador, "cantidad de Recargas recibidas:"=total_recargas) %>% write_csv(paste("output/recargas_gas_semanal_al",Sys.Date(),".csv")
)


