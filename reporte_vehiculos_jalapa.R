-----------------------
#reporte de vehiculos
#---------------------
#library(package = "tidyverse")
#library(shiny)

# datos de vehiculos de redcap uvg
source(file = "scripts/0_get_combustible.R", encoding = "UTF-8")
#fechas inicio y fin del reporte quincenal
fecha1<-"2020-09-01"
fecha2<-"2020-09-30"



#bitacora_vehiculos
#delimitamos el mes para la bitacora
bitacora_quincena<-bitacora_vehiculos %>% filter(as.Date(date_start)>=fecha1 & as.Date(date_start)<=fecha2)
#bitacora_quincena<-bitacora_quincena %>% select(record,"fecha"=date, "fecha_salida"=date_start, "destino"=destino_bitacora, "hora_fin"=hora_final_bitacora)
#bitacora_mes %>% write_csv("output/bitacora_revision.csv")
#bitacora_vehiculos %>% filter(is.na(kilometraje_end)) %>% write_csv("output/pendientes_kilometraje_final_febrero.csv")
resumen_bitacora<-bitacora_quincena %>% select(record, "Fecha_registro"=date, "Fecha salida"=date_start, "Origen"=origen_bitacora, 
                                              "Destino"=destino_bitacora, "Kilometraje_inicio"=kilometraje_start,  "kilometraje_final"=kilometraje_end,
                                          "Hora_inicio"=time_start,"Hora final"= hora_final_bitacora, "Piloto"= piloto_bitacora, "Placa"=placa_bitacora, "Proyecto"=proyecto)
resumen_bitacora %>% arrange(Placa, Kilometraje_inicio) %>% write_csv(paste0("output/bitacora_del_2",fecha1,"_al_",fecha2,".csv"))


#sacamos el resumen de kilometros recorridos por vehículo por mes (fechas del reporte)
kilometraje<-bitacora_quincena %>% filter(!is.na(kilometraje_end)) %>% 
  filter(!is.na(kilometraje_start)) %>% filter(!is.na(time_start)) %>% filter(!is.na(hora_final_bitacora)) %>% 
  select(placa=placa_bitacora, date_start, time_start, hora_final_bitacora, kilometraje_start, kilometraje_end, piloto_bitacora) %>%  
  mutate(inicio=paste(date_start,"_", time_start) %>% lubridate::ymd_hm(),
           fin=paste(date_start, hora_final_bitacora)%>% lubridate::ymd_hm()) %>% 
    group_by(placa) %>% summarize(
      fecha_inicial=min(inicio),
      fecha_final=max(fin)
   
          ) %>% left_join(
             bitacora_quincena %>% 
                              mutate(fecha_inicial=paste(date_start,"_", time_start) %>% lubridate::ymd_hm(),
                                     final=paste(date_start, hora_final_bitacora)%>% lubridate::ymd_hm()) %>% 
                    select(placa=placa_bitacora, fecha_inicial, kilo_inicial=kilometraje_start) 
          ) %>% left_join(
             bitacora_quincena %>% 
              mutate(inicio=paste(date_start,"_", time_start) %>% lubridate::ymd_hm(),
                     fecha_final=paste(date_start, hora_final_bitacora)%>% lubridate::ymd_hm()) %>% 
              select(placa=placa_bitacora, fecha_final, kilo_final=kilometraje_end)
          ) %>%  mutate(
            kilometros_recorridos=as.numeric(kilo_final) -  as.numeric(kilo_inicial)
          )
#revision de placas
table(bitacora_vehiculos$placa_bitacora)
kilometraje %>% write_csv(paste0("output/kilometraje_vehiculos_del_",fecha1,"_al_",fecha2,".csv"))
#kilometraje %>% write_csv(paste0("output/kilometraje_vehiculos_al_", Sys.Date(),".csv"))
#--------------------------------------------
#list( "bitacora"=bitacora_mes,"kilometraje"=kilometraje ) %>% writexl::write_xlsx(paste("output/achivo_test.xlsx"))

#bitacora_mes %>% write_csv(paste0("output/bitacora_",fecha2,".csv"))
#bitacora_mes %>% write_csv(paste0("output/bitacora_",Sys.Date(),".csv"))
  
#ejemplo de Oscar
# si en df tenes los datos que nos mandaste, entonces df %>%
# haces una lista, con un elemento por pestaña
# list(
#   # resumen por persona/semana en una
#   resumen = mutate(
#     ., # le indicas que use todos los datos
#     semana = lubridate::floor_date(fecha_entrega, unit = "week")
#   ) %>%
#     group_by(responsable_recibe, semana) %>%
#     summarize(
#       # todos los recibidos
#       recibido = sum(monto, na.rm = TRUE),
#       # total que tiene fecha de canje
#       reportado_canje = sum(monto[!is.na(date_canje)], na.rm = TRUE),
#       # si hay un numero negativo aqui, es porque canjearon más de lo esperado
#       pendiente = recibido - reportado_canje
#     ) %>%
#     # como puede ser que el vale lo reciban una semana y lo cambien la siguiente
#     # se les puede poner un acumulado para ver si emparejan con el tiempo
#     # Si le dan los vales a otra persona alli si ya no sale el asunto y hay que revisar el detalle
#     mutate(
#       # cuando les sobra pendiente es positivo, cuando gastan de mas pendiente es negativo
#       # y si con el tiempo gastan todo deberia balancear a cero.
#       # TODO: como se hace con los vueltos?
#       acumulado = cumsum(pendiente)
#     ),
#   # y el detalle en otra
#   detalle = .
# ) %>%
#   # y al darle la lista al writexl, escribe el archivo con una pestaña por cada elemento de la lista,
#   # y si los elementos tienen nombre le pone eso a las pestañas
#   writexl::write_xlsx("archivo")




#--------------------
#vales canjeados por vehículo
#--------------------
combustible_vehiculo<-canje_vales_gasolina %>% filter(as.Date(date_canje)>=fecha1 & as.Date(date_canje)<=fecha2) %>% 
    select(record,date_canje, placa_canje, responsable_canje, monto_comprobante_canje, galones_canje) %>% 
    mutate(
      galones= gsub("[^0-9.]","", galones_canje) %>% as.numeric(),
      monto = gsub("[^0-9.]","", monto_comprobante_canje) %>% as.numeric()
    ) %>% 
  group_by(placa_canje) %>% 
  summarize(
    galones=sum(galones),
    valor=sum(monto),
    numero_canjes=n(),
    responsable_canje=paste(responsable_canje, collapse ="," )
  )

table(canje_vales_gasolina$placa_canje)
#canje_vales_gasolina %>% filter(placa_canje=="PLANTA ELECTRICA HAPIN")
reporte <-kilometraje %>% left_join(
  combustible_vehiculo %>% select(placa=placa_canje, monto=valor, responsable_canje, galones, numero_canjes)  
) %>% mutate(
  monto=gsub("[^0-9.]","", monto) %>% as.numeric(),
  galones=if_else(is.na(galones),0, galones)
) %>% mutate(
  numero_canjes=if_else(is.na(numero_canjes),0L,numero_canjes),
  kilometros_galon=kilometros_recorridos/galones
  
) %>%mutate(
  as.Date(fecha_inicial),
  as.Date(fecha_final)
) %>%  select(placa, "Fecha inicio"=fecha_inicial, "Kilometro inicial"=kilo_inicial, 
              "Fecha fin"=fecha_final,  "Kilometro final"=kilo_final, 
              "Kilometros recorridos"=kilometros_recorridos, "Cantidad de Canjes"=numero_canjes, 
              "Monto consumido en Q"=monto, "Galones consumidos"=galones, 
              "Promedio Kilometros/Galon"=kilometros_galon, "Pilotos"=responsable_canje
              ) %>% print(n=Inf)
 reporte %>% write_csv(paste0("output/consumo_vehiculos_del_",fecha1,"_al_", fecha2,".csv"))
   #writexl::write_xlsx("output/consumo_vehiculos.xlsx")
   
   
   #--------------------------------------------------
   #reporte vales entregados y vales canjeados
   #---------------------------------------------------
   #vales entregados y vales canjeados

   vales_gasolina_mes<-entrega_vales_gasolina %>% 
      filter(fecha_entrega>=fecha1 & fecha_entrega<= fecha2) %>% 
          gather(key=variable, value=value, -responsable, -fecha_entrega, -tipo_vale) %>% 
     mutate(vale=case_when(
                    grepl("qr_vale", variable) ~ value,
                    TRUE ~ NA_character_
                            )
            ) %>% mutate(
              monto=case_when(tipo_vale=="1" ~ 100L,
                              tipo_vale=="2" ~ 50L,
                              tipo_vale=="3" ~ 20L,
                              TRUE ~ NA_integer_
                              )
            ) %>% 
     filter(
              !is.na(vale)
            ) %>% select(vale, fecha_entrega, "responsable_recibe"=responsable, monto) %>% 
     left_join(
       canje_vales_gasolina %>% 
         filter(
          # date_canje>="2019-05-01" & date_canje<="2019-05-31"
           date_canje>=fecha1 & date_canje<=fecha2
         ) %>% gather(key=variable, value=value, -placa_canje, -responsable_canje, -date_canje) %>% 
         mutate(
           vale=case_when(
             grepl("canje_vale", variable) ~ value,
             TRUE ~ NA_character_
           )
         ) %>% filter(
           !is.na(vale) & vale!=0
         ) %>% select(
           vale, date_canje, placa_canje, responsable_canje
         )
     ) 
 # %>% 
 #     #{ filter(is.na(date_canje)) %>% write_csv("output/vales_pendientes_canje.csv")} %>%  
 #     write_csv(paste("output/reporte_vales_gasolina_entregados_", fecha1, "_al_", fecha2,".csv"))
 #    
 
 #------------------
 #VALES DE GASOLINA SEMANAL
 #-------------------
 vales_gasolina_semana<-entrega_vales_gasolina %>% 
   #filter(fecha_entrega>="2019-05-01" & fecha_entrega<= "2019-05-31") %>% 
   filter(fecha_entrega>=fecha1 & fecha_entrega<= fecha2) %>% 
   gather(key=variable, value=value, -responsable, -fecha_entrega, -tipo_vale) %>% 
   mutate(vale=case_when(
     grepl("qr_vale", variable) ~ value,
     TRUE ~ NA_character_
   )
   ) %>% mutate(
     monto=case_when(tipo_vale=="1" ~ 100L,
                     tipo_vale=="2" ~ 50L,
                     tipo_vale=="3" ~ 20L,
                     TRUE ~ NA_integer_
     )
   ) %>% 
   filter(
     !is.na(vale)
   ) %>% select(vale, fecha_entrega, "responsable_recibe"=responsable, monto) %>% 
   left_join(
     canje_vales_gasolina %>% 
       filter(
         date_canje>=fecha1 & date_canje<=fecha2
       ) %>% gather(key=variable, value=value, -placa_canje, -responsable_canje, -date_canje) %>% 
       mutate(
         vale=case_when(
           grepl("canje_vale", variable) ~ value,
           TRUE ~ NA_character_
         )
       ) %>% filter(
         !is.na(vale) & vale!=0
       ) %>% select(
         vale, date_canje, placa_canje, responsable_canje
       )
   )
   # ) %>% 
   # #{ filter(is.na(date_canje)) %>% write_csv("output/vales_pendientes_canje.csv")} %>%  
   # write_csv(paste("output/reporte_vales_gasolina_semana_", fecha3, "_al_", fecha4,".csv"))
#list(vales_gasolina_mes,vales_gasolina_semana) %>% writexl::write_xlsx("output/reporte_vales_gasolina.xlsx")

#reporte semanal de entrega de vales
vales_gasolina_semana %>% write_csv(paste0("output/listado_vales_del_",fecha1,"_al_",fecha2,".csv"))

