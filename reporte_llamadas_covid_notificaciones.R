library("tidyverse")
datos_llamadas<-read_csv("data/llamadas/Llamadascovid_DATA_2020-05-18_1059.csv")
llamadas_asignadas<-read_csv("data/llamadas/data_llamadas_asignadas.csv")
respuesta_llamadas<-read_csv("data/llamadas/respuesta_llamadas.csv")
respuesta_llamadas<-respuesta_llamadas %>% mutate_all(as.character)
#lista asignada de llamadas
llamadas_asignadas<-llamadas_asignadas %>% mutate_all(as.character)
#datos de llamadas realizadas
datos_llamadas<-datos_llamadas %>% mutate_all(as.character)

datos_llamadas %>% filter(id_hapin=="3357")
datos_llamadas %>% select(record_id,id_hapin,cov_by,cov_date,cov_tipo_llamada,
                          cov_telefono,
                          cov_contacto_exitoso,
                          cov_nocontacto_respuesta___1,
                          cov_nocontacto_respuesta___2,
                          cov_nocontacto_respuesta___3,
                          cov_nocontacto_respuesta___4,
                          cov_nocontacto_respuesta___5,
                          cov_nocontacto_respuesta___6,
                          cov_otro_telefono1,
                          cov_telefono_2,
                          cov_contacto_exitoso_2,
                          cov_nocontacto_respuesta_2___1,
                          cov_nocontacto_respuesta_2___2,
                          cov_nocontacto_respuesta_2___3,
                          cov_nocontacto_respuesta_2___4,
                          cov_nocontacto_respuesta_2___5,
                          cov_otro_telefono2,
                          cov_telefono_3,
                          cov_contacto_exitoso_3) %>% filter(id_hapin=="45039")
#armar matriz de llamadas realizadas
intentos_llamadas<-datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(!is.na(cov_contacto_exitoso)) %>%  select(
  id_hapin, cov_by,cov_date,
  telefono=cov_telefono,
  respondio=cov_contacto_exitoso
) %>% mutate(respondio=recode(respondio,"1"="Si", "2"="No")) %>% bind_rows(
  datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(!is.na(cov_contacto_exitoso_2)) %>%  select(
    id_hapin, cov_by,cov_date,
    telefono=cov_telefono_2,
    respondio=cov_contacto_exitoso_2
  ) %>% mutate(respondio=recode(respondio,"1"="Si", "2"="No"))
  
) %>% bind_rows(
  datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(!is.na(cov_contacto_exitoso_3)) %>%  select(
    id_hapin, cov_by,cov_date,
    telefono=cov_telefono_3,
    respondio=cov_contacto_exitoso_3
  ) %>% mutate(respondio=recode(respondio,"1"="Si", "2"="No"))
  
) %>% filter(id_hapin!="99999") %>% group_by(id_hapin,cov_by,respondio) %>% count() 

intentos_llamadas%>% writexl::write_xlsx("output/llamadas/conteos_intentos_llamadas.xlsx")

#cantidad de hogares que han llamado
datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(id_hapin!="99999") %>% group_by(id_hapin,cov_by) %>% count() %>% writexl::write_xlsx("output/llamadas/hogares.xlsx")

#clasificacion de intentos y tipos de respuestas:
llamadas_sin_respuesta<-datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(cov_contacto_exitoso=="2") %>% select(id_hapin, cov_by, cov_date, 
                                                                                                     cov_nocontacto_respuesta___1,
                                                                                                     cov_nocontacto_respuesta___2,
                                                                                                     cov_nocontacto_respuesta___3,
                                                                                                     cov_nocontacto_respuesta___4,
                                                                                                     cov_nocontacto_respuesta___5,
                                                                                                     cov_nocontacto_respuesta___6) %>% mutate(
        cov_nocontacto_respuesta___1=recode(cov_nocontacto_respuesta___1, "1"="envio directamente a buzón","0"=" - "),
        cov_nocontacto_respuesta___2=recode(cov_nocontacto_respuesta___2, "1"="timbró, pero no respondió","0"=" - "),
        cov_nocontacto_respuesta___3=recode(cov_nocontacto_respuesta___3, "1"="número telefónico ya no le pertenece","0"=" - "),
        cov_nocontacto_respuesta___4=recode(cov_nocontacto_respuesta___4, "1"="Respondió familiar que participante ya no viven en el mismo hogar y no tiene el nuevo número de teléfono","0"=" - "),
        cov_nocontacto_respuesta___5=recode(cov_nocontacto_respuesta___5, "1"="Respondió familiar que participante ya no viven en el mismo hogar dio el nuevo numero","0"=" - "),
        cov_nocontacto_respuesta___6=recode(cov_nocontacto_respuesta___6, "1"="Otro","0"=" - ")
                                                                                                     ) %>% 
 mutate(
          respuesta=paste(
            cov_nocontacto_respuesta___1, cov_nocontacto_respuesta___2, cov_nocontacto_respuesta___3,cov_nocontacto_respuesta___4, cov_nocontacto_respuesta___5,
            cov_nocontacto_respuesta___6
          )
        ) %>% select(id_hapin, cov_by, cov_date, respuesta) %>% 
  #agregamos resultados para el segundo numero en el crf
  bind_rows(
    datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(cov_contacto_exitoso_2=="2") %>% select(id_hapin, cov_by, cov_date, 
                                                                                                         cov_nocontacto_respuesta_2___1,
                                                                                                         cov_nocontacto_respuesta_2___2,
                                                                                                         cov_nocontacto_respuesta_2___3,
                                                                                                         cov_nocontacto_respuesta_2___4,
                                                                                                         cov_nocontacto_respuesta_2___5,
                                                                                                         cov_nocontacto_respuesta_2___6) %>% mutate(
                                                                                                           cov_nocontacto_respuesta_2___1=recode(cov_nocontacto_respuesta_2___1, "1"="envio directamente a buzón","0"=" - "),
                                                                                                           cov_nocontacto_respuesta_2___2=recode(cov_nocontacto_respuesta_2___2, "1"="timbró, pero no respondió","0"=" - "),
                                                                                                           cov_nocontacto_respuesta_2___3=recode(cov_nocontacto_respuesta_2___3, "1"="número telefónico ya no le pertenece","0"=" - "),
                                                                                                           cov_nocontacto_respuesta_2___4=recode(cov_nocontacto_respuesta_2___4, "1"="Respondió familiar que participante ya no viven en el mismo hogar y no tiene el nuevo número de teléfono","0"=" - "),
                                                                                                           cov_nocontacto_respuesta_2___5=recode(cov_nocontacto_respuesta_2___5, "1"="Respondió familiar que participante ya no viven en el mismo hogar dio el nuevo numero","0"=" - "),
                                                                                                           cov_nocontacto_respuesta_2___6=recode(cov_nocontacto_respuesta_2___6, "1"="Otro","0"=" - ")
                                                                                                        ) %>% 
      mutate(
        respuesta=paste(
          cov_nocontacto_respuesta_2___1, cov_nocontacto_respuesta_2___2, cov_nocontacto_respuesta_2___3,cov_nocontacto_respuesta_2___4, 
          cov_nocontacto_respuesta_2___5,cov_nocontacto_respuesta_2___6
        )
      ) %>% select(id_hapin, cov_by, cov_date, respuesta)   
  
        ) %>% 
  #agregamos resultados para el tercer numero en el crf
  bind_rows(
    datos_llamadas %>% filter(cov_tipo_llamada=="7") %>% filter(cov_contacto_exitoso_3=="2") %>% select(id_hapin, cov_by, cov_date, 
                                                                                                           cov_nocontacto_respuesta_3___1,
                                                                                                           cov_nocontacto_respuesta_3___2,
                                                                                                           cov_nocontacto_respuesta_3___3,
                                                                                                           cov_nocontacto_respuesta_3___4,
                                                                                                           cov_nocontacto_respuesta_3___5,
                                                                                                           cov_nocontacto_respuesta_3___6) %>% mutate(
                                                                                                             cov_nocontacto_respuesta_3___1=recode(cov_nocontacto_respuesta_3___1, "1"="envio directamente a buzón","0"=" - "),
                                                                                                             cov_nocontacto_respuesta_3___2=recode(cov_nocontacto_respuesta_3___2, "1"="timbró, pero no respondió","0"=" - "),
                                                                                                             cov_nocontacto_respuesta_3___3=recode(cov_nocontacto_respuesta_3___3, "1"="número telefónico ya no le pertenece","0"=" - "),
                                                                                                             cov_nocontacto_respuesta_3___4=recode(cov_nocontacto_respuesta_3___4, "1"="Respondió familiar que participante ya no viven en el mismo hogar y no tiene el nuevo número de teléfono","0"=" - "),
                                                                                                             cov_nocontacto_respuesta_3___5=recode(cov_nocontacto_respuesta_3___5, "1"="Respondió familiar que participante ya no viven en el mismo hogar dio el nuevo numero","0"=" - "),
                                                                                                             cov_nocontacto_respuesta_3___6=recode(cov_nocontacto_respuesta_3___6, "1"="Otro","0"=" - ")
                                                                                                           ) %>% 
      mutate(
        respuesta=paste(
          cov_nocontacto_respuesta_3___1, cov_nocontacto_respuesta_3___2, cov_nocontacto_respuesta_3___3,cov_nocontacto_respuesta_3___4, 
          cov_nocontacto_respuesta_3___5,cov_nocontacto_respuesta_3___6
        )
      ) %>% select(id_hapin, cov_by, cov_date, respuesta)   
    
  )                                                                                                                                            
llamadas_sin_respuesta%>% writexl::write_xlsx("output/revision_respuestas_llamadas.xlsx")  

hogares_no_contactados<-datos_llamadas %>% filter(cov_tipo_llamada=="7") %>%  select(id=id_hapin, cov_by, cov_contacto_exitoso, cov_contacto_exitoso_2,
                                                             cov_contacto_exitoso_3 ) %>%  
  mutate(contacto_exitoso=case_when(
    cov_contacto_exitoso=="1" ~ "Realizado",
    cov_contacto_exitoso_2=="1" ~ "Realizado",
    cov_contacto_exitoso_3=="1" ~ "Realizado",
   TRUE  ~ "No_contactado")
) %>% filter(contacto_exitoso=="No_contactado" & id!="99999") %>% group_by(id) %>% count() %>%  anti_join(
  datos_llamadas %>% filter(cov_tipo_llamada=="7") %>%  select(id=id_hapin, cov_by, cov_contacto_exitoso, cov_contacto_exitoso_2,
                                                               cov_contacto_exitoso_3 ) %>%  
    mutate(contacto_exitoso=case_when(
      cov_contacto_exitoso=="1" ~ "Realizado",
      cov_contacto_exitoso_2=="1" ~ "Realizado",
      cov_contacto_exitoso_3=="1" ~ "Realizado",
      TRUE  ~ "No_contactado")
    ) %>% filter(contacto_exitoso=="Realizado") %>% group_by(id) %>% count() %>% filter(id!="99999") %>% select(id)
)

hogares_contactados<-datos_llamadas %>% filter(cov_tipo_llamada=="7") %>%  select(id=id_hapin, cov_by, cov_contacto_exitoso, cov_contacto_exitoso_2,
                                                             cov_contacto_exitoso_3 ) %>%  
  mutate(contacto_exitoso=case_when(
    cov_contacto_exitoso=="1" ~ "Realizado",
    cov_contacto_exitoso_2=="1" ~ "Realizado",
    cov_contacto_exitoso_3=="1" ~ "Realizado",
    TRUE  ~ "No_contactado")
  ) %>% filter(contacto_exitoso=="Realizado") %>% group_by(id) %>% count() %>% filter(id!="99999") %>% select(id)

no_exitosas<-llamadas_asignadas %>% left_join(
  hogares_no_contactados %>% left_join(
  respuesta_llamadas
) %>% select(id, respuesta) %>% filter(id!="3357" | id!="99999") %>%  mutate(exitosas="No") 
)%>%  filter(!is.na(exitosas))
 # write_csv("output/hogares_no_contactados.csv")

  #llamadas exitosas
exitosas<-llamadas_asignadas  %>% left_join(
  hogares_contactados %>% mutate(exitosas="Si") %>% filter(id!="99999")
) %>% filter(exitosas=="Si")
#%>% writexl::write_xlsx("output/hogares_contactados.xlsx")

#unificar los datos
pendientes<-llamadas_asignadas %>% anti_join(
  no_exitosas %>% select(id)
) %>% anti_join(
  exitosas %>% select(id)
)

pendientes %>% bind_rows(
  exitosas
) %>% bind_rows(
  no_exitosas
) %>% left_join(
  datos_participantes %>% select(id_tamizaje=`ID tamizaje`, id=`ID estudio`, `Nombre embarazada`, `Nombre otra adulta`, 
                                 `Comunidad embarazada (nueva)`, `Comunidad embarazada (original z10)`, `Celular embarazada`,
                                 `Celular esposo`,`Celular_otro_miembro`)
) %>%  write_csv("output/llamadas/lista_informacion_llamadas_23-04-2020.csv")
  
  write_csv("output/llamadas/reporte_llamadas_covid.csv")

llamadas_sin_respuesta %>% 
  anti_join(
    hogares_contactados %>% select(id_hapin=id)
  ) %>%  group_by(id_hapin) %>% select(id=id_hapin, respuesta ) %>% writexl::write_xlsx("output/datos_respuesta2.xlsx")



#-------------------------------------------------
#
#REVISION DE NUMEROS CON RESPUESTA POSITIVA
#
#-------------------------------------------------
 datos_llamadas %>% select(record_id, id_hapin, cov_by, telefono= cov_telefono, exitoso=cov_contacto_exitoso  ) %>%  bind_rows(
   datos_llamadas %>% select(record_id, id_hapin, cov_by, telefono= cov_telefono_2, exitoso=cov_contacto_exitoso_2  )
 ) %>% bind_rows(
   datos_llamadas %>% select(record_id, id_hapin, cov_by, telefono= cov_telefono_3, exitoso=cov_contacto_exitoso_3  )
 ) %>% filter(exitoso=="1") %>% select(-record_id) %>% arrange(id_hapin) %>% writexl::write_xlsx("output/lista_numeros_exitosos.xlsx")
