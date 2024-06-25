#B10 Recoleccion de Biomuestras OAW

#E11. ¿Ud. recolecto muestra de sangre venosa de la otra mujer adulta? 

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% select(id, b10_date,visit, b10_venous) %>% filter(!is.na(b10_venous)) %>% mutate(type = if_else(
  condition = grepl("^35[0-9]{3}", id),
  true = "oaw",
  false = "pw"
)
) %>% left_join(
  gt_emory_data_arm2 %>% filter(!is.na(s6_date)) %>% select(id,s6_arm) %>% mutate(grupo=recode(
    s6_arm,"1"="Intervencion","0"="Control"
  ))
) %>% filter(b10_venous==1 & !is.na(b10_venous)) %>% 
  group_by(visit,grupo,id) %>% summarize(cantidad=n()) %>% filter(visit=="b2")


gt_emory_data_arm2 %>% filter(!is.na(h41_date)) %>% mutate(
  type=substr(id, 1, 2)
) %>% 
  select(type, id, visit, matches("^b10_")) %>% group_by(
    type, visit
  ) %>% summarize(
    visitas= n(),
    sangre = sum(b10_venous==1 & !is.na(b10_venous))
  )


m17<-gt_emory_data_arm1 %>% filter(!is.na(m17_date))

table(m17$m17_by)

gt_emory_data_arm2 %>% filter(!is.na(b10_date)) %>% filter(b10_tm_u_code=="33100-C4-FBS1" | b10_tm_b_code=="33100-C4-FBS1")
b10_tm_u_code
b10_tm_b_code

#lista de comunidades
data_tamizajes<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id, "fecha_consentimiento"=s4_date) %>% left_join(
  comunidades %>% select(id_estudio, "comunidad_1"=community, "comunidad_2"=community_new)
) 



#quitar las que ya tienen lugar de reclutamiento
conreclutamiento_1<-read_csv("data/tamizaje_comunidades.csv")
#generar listado para validar con Jalapa donde fueron reclutadas
data_tamizajes %>% anti_join(
  conreclutamiento_1 %>% transmute(id_estudio=as.character(id_estudio))
) %>%  writexl::write_xlsx("output/temp/lista_id_comunidades_pend_reclutamiento.xlsx")

#data_tamizajes%>% writexl::write_xlsx("output/temp/lista_id_comunidades.xlsx")


conteo_hogares<-gt_emory_data_arm1 %>% filter(!is.na(s4_main_id)) %>% select("id_tamizaje"=id, "id_estudio"=s4_main_id, "fecha_consentimiento"=s4_date) %>% left_join(
  comunidades %>% select(id_estudio, "comunidad_1"=community, "comunidad_2"=community_new, lat, long)
) %>% left_join(
  gt_emory_data_arm1 %>% filter(!is.na(s1_date)) %>% select("id_tamizaje"=id, s1_community_name )
) %>% mutate(
  comunidad=case_when(
    !is.na(comunidad_1) ~ comunidad_1,
    is.na(comunidad_1) & !is.na(comunidad_2) ~ comunidad_2,
    is.na(comunidad_1) & is.na(comunidad_2) ~ s1_community_name
  ) 
) %>% select(id_tamizaje, id_estudio, comunidad, lat, long) %>% filter(!is.na(lat)) %>%  group_by(comunidad) %>% summarize(cantidad_hogares=n(), latitud=mean(lat), longitud=mean(long)) 
# %>% 
#   write_csv("output/temp/conteo_hogares.csv")


tamizaje_comunidades<-read_csv("data/tamizaje_comunidades.csv")
#comunidades reclutadas completado
tamizaje_comunidades_sep<-read_csv("output/temp/tamizaje_comunidades_reclutamiento.csv")
tamizaje_comunidades_sep<-tamizaje_comunidades_sep %>% mutate(com_reclu=`COMUNIDAD DE RECLUTAMIENTO`)
tamizaje_comunidades_sep %>% mutate(recode(
  com_reclu, "LA FUENTE DE LA MONTAA" = "LA FUENTE", "EL DIVISADERO"="DIVISADERO"
) 
)
tamizaje_comunidades_sep<-tamizaje_comunidades_sep %>% mutate_all(as.character)

#estandarizar la comunidad de reclutamiento
reclutamientos<-tamizaje_comunidades_sep %>% mutate("com_reclu"=`COMUNIDAD DE RECLUTAMIENTO`) %>% mutate(
  distrito=case_when(
    com_reclu=="LA FUENTE" ~ "JALAPA",
    com_reclu=="LA FUENTE DE LA MONTAA" ~ "JALAPA",
    com_reclu=="EL DURAZNAL" ~ "JALAPA",
    com_reclu=="DIVISADERO" ~ "JALAPA",
    com_reclu=="EL DIVISADERO" ~ "JALAPA",
    com_reclu=="LA TOMA" ~ "JALAPA",
    com_reclu=="MIRAMUNDO" ~ "JALAPA",
    com_reclu=="EL PARAISO" ~ "JALAPA",
    com_reclu=="LAS GUACAMAYAS" ~ "JALAPA",
    com_reclu=="LAS AZUCENAS" ~ "JALAPA",
    com_reclu=="LOS IZOTES" ~ "SANYUYO",
    com_reclu=="LAGUNA DEL PITO" ~ "SANYUYO",
    com_reclu=="EL PITO" ~ "SANYUYO",
    com_reclu=="LAGUNA EL PITO" ~ "SANYUYO",
    com_reclu=="LA PAZ" ~ "SANYUYO",
    com_reclu=="PALO VERDE" ~ "SANYUYO",
    com_reclu=="SANSIRISAY" ~ "SANYUYO",
    com_reclu=="EL DURAZNO" ~ "SANYUYO",
    com_reclu=="LA LAGUNETA" ~ "SANYUYO",
    com_reclu=="SANYUYO" ~ "SANYUYO",
    com_reclu=="LA LAGUNILLA" ~ "SANYUYO",
    com_reclu=="EL CARRIZAL" ~ "SANYUYO",
    com_reclu=="EL RODEO" ~ "SANYUYO",
    com_reclu=="SANSURUTATE" ~ "SANYUYO",
    com_reclu=="ARAISAPO" ~ "SANYUYO",
    com_reclu=="ACHIOTES JUMAY" ~ "JALAPA",
    com_reclu=="LAGUNETA" ~ "SANYUYO"
  )
) %>% select(id_tamizaje=it_tamizaje, id_estudio, com_reclu, distrito)

#Estandarizar nombres
reclutamientos<-reclutamientos %>% mutate(comunidad_reclutamiento=recode(
  com_reclu, "LA FUENTE DE LA MONTAA" = "LA FUENTE", "EL DIVISADERO"="DIVISADERO", "LAGUNA EL PITO"="EL PITO","LA LAGUNETA"="LAGUNETA", "LAS AZUCENAS"="AZUCENAS",
  "LAS GUACAMAYAS"="GUACAMAYAS", "ACHIOTES JUMAY"="DIVISADERO", "EL DURAZNAL"="DURAZNAL", "EL CARRIZAL"="CARRIZAL","LA LAGUNILLA"="LAGUNILLA", "LOS IZOTES"="IZOTES"
) 
)

table(reclutamientos$comunidad_reclutamiento)
#Hogares reclutados en el distrito de Jalapa
jalapa<-reclutamientos %>% 
  group_by(comunidad_reclutamiento,distrito) %>% summarize(cantidad=n()) %>% arrange(desc(cantidad)) %>% filter(distrito=="JALAPA")

#Hogares reclutados en el distrito de Sanyuyo
sanyuyo<-reclutamientos %>% 
  group_by(comunidad_reclutamiento,distrito) %>% summarize(cantidad=n()) %>% arrange(desc(cantidad)) %>% filter(distrito=="SANYUYO")

ggplot(jalapa, mapping = aes(jalapa$distrito,jalapa$cantidad) )



jalapa %>% select(com_reclu, cantidad) %>%  arrange(desc(cantidad)) %>%  ggplot(aes(x = com_reclu,  y = cantidad)) + 
  geom_bar(stat = "identity", position=position_dodge()) + xlab("COMUNIDAD") + ylab("RECLUTADOS")  +  
  ggtitle("RECLUTAMIENTOS EN DISTRITO JALAPA")  +  scale_fill_grey(start = 0.5, end = 0.6, na.value = "red")  + 
  theme_bw() + 
  theme(plot.title=element_text(family="Times", face="bold", size=18) , axis.text.x =element_text(family="Times", face="bold", size=10, angle = 90) , 
        axis.text.y =element_text(family="Times", face="bold", size=10))

sanyuyo %>% arrange(desc(cantidad)) %>%  ggplot(aes(x = com_reclu,  y = cantidad)) + 
  geom_bar(stat = "identity") + xlab("COMUNIDAD") + ylab("RECLUTADOS")  +  
  ggtitle("RECLUTAMIENTOS EN DISTRITO SANYUYO")  + scale_fill_gradient(low="blue", high="red")  + 
  theme_bw() + 
  theme(plot.title=element_text(family="Times", face="bold", size=18) , axis.text.x =element_text(family="Times", face="bold", size=10, angle = 90) , 
        axis.text.y =element_text(family="Times", face="bold", size=10))

#exportar hogares reclutados por distritos
list(
  "distrito_jalapa"=jalapa,
  "distrito_sanyuyo"=sanyuyo,
  "reclutamientos"=reclutamientos
) %>% writexl::write_xlsx("output/temp/conteos_reclutamientos.xlsx")

gt_emory_data_arm2 %>% filter(!is.na(c30_date)) %>%  select(id, c30_date, c30_ave_wt ) %>% write_csv("output/pesos_promedio.csv")


gt_emory_data_arm1 %>%  select(id, s4_main_id) %>% filter(!is.na(s4_main_id))
